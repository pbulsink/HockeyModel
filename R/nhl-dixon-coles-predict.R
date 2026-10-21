# Dixon-Coles predictions: probability matrices, expected goals, and result sampling

#' Predict home / draw / away win probability for a single game
#'
#' @description The main Dixon-Coles prediction entry point.  It builds the
#'   full score-probability matrix for the pairing via [.dcProbMatrix()] (which
#'   composes the `lambda`/`mu` expected goals from `m`, the Dixon-Coles
#'   low-score adjustment `rho` via [tau()], and the Weibull tie-enhancement
#'   `beta`/`eta`/`k` from [getWeibullParams()]), then sums the lower triangle
#'   (home win), the diagonal (draw), and the upper triangle (away win) and
#'   normalises the result.
#'
#'   When `draws = FALSE` (e.g. for a best-of series) the draw mass is
#'   distributed proportionally onto the home and away outcomes using the
#'   original pre-redistribution values, so neither outcome is biased by the
#'   other's update.
#'
#' @param home (`character(1)`) Home team name.
#' @param away (`character(1)`) Away team name.
#' @param params (`list` or `NULL`) The named list containing `m`, `rho`,
#'   `beta`, `eta`, and `k`.  If `NULL`, the package defaults are used (see
#'   [updateModel()] for how to obtain a fresh params list).
#' @param maxgoal (`integer(1)`) Maximum number of goals per team to include in
#'   the score matrix.  Defaults to `10`.
#' @param scores (`data.frame`) Historical scores.  Used only to compute `m`
#'   and `rho` when they are not already in `params`; normally not needed.
#'   Defaults to [HockeyModel::scores].
#' @param expected_mean (`double` or `NULL`) The league-wide mean of
#'   `lambda`/`mu`.  When supplied together with `season_percent`, expected
#'   goals are regressed toward this value (used for remainder-of-season
#'   projections).
#' @param season_percent (`double` or `NULL`) Fraction of the season complete,
#'   used only together with `expected_mean` for the regression blend.
#' @param draws (`logical(1)`) Whether draws are an allowed outcome.  Defaults
#'   to `TRUE`.  When `FALSE`, the draw probability is redistributed to home
#'   and away and a two-element result is returned.
#' @param home_b2b (`logical(1)`) Whether the home team is on a back-to-back
#'   (played its previous game exactly one day earlier).  When `TRUE`, the home
#'   expected goals are scaled by `exp(-B2B_HOME_NHL)` and the away expected
#'   goals against it by `exp(+B2B_HOME_NHL)`.  Defaults to `FALSE` (no
#'   adjustment), so existing calls are unchanged.
#' @param away_b2b (`logical(1)`) Whether the away team is on a back-to-back.
#'   When `TRUE`, the away expected goals are scaled by `exp(-B2B_AWAY_NHL)` and
#'   the home expected goals against it by `exp(+B2B_AWAY_NHL)`.  Defaults to
#'   `FALSE` (no adjustment).
#' @param eps_home (`double` or `NULL`) Home b2b magnitude; defaults to
#'   [B2B_HOME_NHL].  Overridable for tuning.
#' @param eps_away (`double` or `NULL`) Away b2b magnitude; defaults to
#'   [B2B_AWAY_NHL].  Overridable for tuning.
#'
#' @returns (`numeric`) A named probability vector — `HomeWin`, `Draw`, and
#'   `AwayWin` when `draws = TRUE`, or `HomeWin` and `AwayWin` when
#'   `draws = FALSE` — summing to `1`.
#' @export
#'
#' @examples \dontrun{
#'   # Regulation win/draw/loss probabilities for one matchup
#'   DCPredict(home = "Toronto Maple Leafs", away = "Montreal Canadiens")
#'
#'   # Best-of series (no draws)
#'   DCPredict("Boston Bruins", "Tampa Bay Lightning", draws = FALSE)
#' }
DCPredict <- function(
  home,
  away,
  params = NULL,
  maxgoal = 10,
  scores = HockeyModel::scores,
  expected_mean = NULL,
  season_percent = NULL,
  draws = TRUE,
  home_b2b = FALSE,
  away_b2b = FALSE,
  eps_home = NULL,
  eps_away = NULL
) {
  if (is.null(params)) {
    params <- .parse_dc_params(params)
  }
  probability_matrix <- .dcProbMatrix(
    home = home,
    away = away,
    params = params,
    maxgoal = maxgoal,
    expected_mean = expected_mean,
    season_percent = season_percent,
    home_b2b = home_b2b,
    away_b2b = away_b2b,
    eps_home = eps_home,
    eps_away = eps_away
  )

  HomeWinProbability <- sum(probability_matrix[lower.tri(probability_matrix)])
  DrawProbability <- sum(diag(probability_matrix))
  AwayWinProbability <- sum(probability_matrix[upper.tri(probability_matrix)])

  # Simple Adjust for under-predicting odds
  odds <- normalizeOdds(c(
    HomeWinProbability,
    DrawProbability,
    AwayWinProbability
  ))

  if (!draws) {
    # Distribute the draw probability proportionally to home/away, using the
    # original (pre-redistribution) values for both so neither outcome is
    # biased by the other's update.
    orig_hw <- HomeWinProbability
    orig_aw <- AwayWinProbability
    pair <- orig_hw + orig_aw
    HomeWinProbability <- orig_hw + orig_hw / pair * DrawProbability
    AwayWinProbability <- orig_aw + orig_aw / pair * DrawProbability
    odds <- normalizeOdds(c(HomeWinProbability, AwayWinProbability))
  }
  return(odds)
}

#' Vectorized Dixon-Coles probability array
#'
#' @description Builds the Dixon-Coles probability matrix for each of many
#'   (lambda, mu) pairs at once, returning a 3D array of dimension
#'   `(maxgoal+1) x (maxgoal+1) x n`. Each slice `pm[,,j]` is exactly what
#'   [.prob_matrix] returns for the j-th game, so the per-game output is
#'   statistically identical to the scalar path.
#'
#' @param lambda (`double(n)`) Home expected goals, one value per game.
#' @param mu (`double(n)`) Away expected goals, one value per game.
#' @param params Pre-parsed Dixon-Coles parameter list (m, rho, beta, eta, k).
#'   Must be the output of [.parse_dc_params()]; callers in the public entry
#'   chain are responsible for parsing once (#47). `NULL` triggers a fallback
#'   parse (for direct test calls).
#' @param maxgoal (`integer(1)`) Maximum goals per team.
#' @returns (`array`) Probability array of dim `(maxgoal+1) x (maxgoal+1) x n`.
#' @keywords internal
.dcProbArray <- function(lambda, mu, params = NULL, maxgoal) {
  if (is.null(params)) {
    params <- .parse_dc_params(params)
  }
  n <- length(lambda)
  G <- maxgoal + 1
  idx0 <- 0:maxgoal

  # Weibull tie-enhancement on the diagonal (shared across games).
  weib <- stats::dweibull(seq_len(G), shape = params$beta, scale = params$eta) *
    params$k
  up <- upper.tri(matrix(1, G, G))
  lo <- lower.tri(matrix(1, G, G))

  .build_one <- function(lam, mu) {
    pm <- stats::dpois(idx0, lam) %*% t(stats::dpois(idx0, mu))
    # Dixon-Coles tau scaling on the four low-score cells, then clamp negatives.
    # Must match .prob_matrix exactly: it uses matrix(c(c00,c01,c10,c11), nrow=2)
    # (column-major), so [2,1] gets c01 (mu) and [1,2] gets c10 (lambda).
    pm[1, 1] <- pm[1, 1] * (1 - (lam * mu * params$rho))
    pm[2, 1] <- pm[2, 1] * (1 + (mu * params$rho))
    pm[1, 2] <- pm[1, 2] * (1 + (lam * params$rho))
    pm[2, 2] <- pm[2, 2] * (1 - params$rho)
    pm[1:2, 1:2][pm[1:2, 1:2] < 0] <- 0
    # Weibull tie-enhancement on the diagonal.
    diag(pm) <- diag(pm) * weib
    # Cap diagonal if it reaches/exceeds 1.
    ds <- sum(diag(pm))
    if (ds >= 1) {
      diag(pm) <- diag(pm) * (1 - 1e-6) / ds
      ds <- sum(diag(pm))
    }
    # Renormalize the off-diagonal to fill the remaining mass.
    offsum <- sum(pm[up]) + sum(pm[lo])
    nf <- offsum / (1 - ds)
    pm[up] <- pm[up] / nf
    pm[lo] <- pm[lo] / nf
    return(pm)
  }

  out <- array(0, dim = c(G, G, n))
  for (j in seq_len(n)) {
    out[,, j] <- .build_one(lambda[j], mu[j])
  }
  return(out)
}

#' Vectorized Dixon-Coles odds
#'
#' @description Computes home/draw/away win probabilities for many games at
#'   once. The output is statistically identical to calling [DCPredict]
#'   per game, but avoids the per-game function-call overhead that makes
#'   `.todayDC()` / `remainderSeasonDC()` O(F) in function calls.
#'
#' @param home (`character(n)`) Home team name per game.
#' @param away (`character(n)`) Away team name per game.
#' @param params Pre-parsed Dixon-Coles parameter list (m, rho, beta, eta, k).
#'   Must be the output of [.parse_dc_params()]; callers in the public entry
#'   chain are responsible for parsing once (#47). `NULL` triggers a fallback
#'   parse (for direct test calls).
#' @param maxgoal (`integer(1)`) Maximum goals per team.
#' @param expected_mean (`double` or `NULL`) Mean lambda/mu for regression.
#' @param season_percent (`double` or `NULL`) Season completion fraction for
#'   regression.
#' @param draws (`logical(1)`) Whether to return a 3-column (home, draw, away)
#'   matrix; if `FALSE`, returns a 2-column (home, away) matrix with the draw
#'   probability distributed proportionally.
#' @param home_b2b (`logical(n)`) Per-game flag for whether the home team is on
#'   a back-to-back; scales that game's expected goals by exp(+/- `B2B_HOME_NHL`).
#'   Defaults to all `FALSE`.
#' @param away_b2b (`logical(n)`) Per-game flag for whether the away team is on
#'   a back-to-back; scales that game's expected goals by exp(+/- `B2B_AWAY_NHL`).
#'   Defaults to all `FALSE`.
#' @returns (`matrix(n x 3)` or `matrix(n x 2)`) Probability columns.
#' @keywords internal
.dcPredictVectorized <- function(
  home,
  away,
  params = NULL,
  maxgoal = 10,
  expected_mean = NULL,
  season_percent = NULL,
  draws = TRUE,
  home_b2b = FALSE,
  away_b2b = FALSE
) {
  if (is.null(params)) {
    params <- .parse_dc_params(params)
  }
  n <- length(home)

  # Expected goals home / away per game, with the same error recovery path as
  # [.dcLambda] so new or unseen teams still produce a numeric estimate.
  lam <- vapply(
    seq_len(n),
    function(i) {
      x <- try(
        as.numeric(stats::predict(
          params$m,
          data.frame(Home = 1, Team = home[i], Opponent = away[i]),
          type = "response"
        )),
        silent = TRUE
      )
      if (is.numeric(x)) {
        x
      } else {
        .DCPredictErrorRecover(
          team = home[i],
          opponent = away[i],
          homeiceadv = TRUE
        )
      }
    },
    numeric(1)
  )
  mu <- vapply(
    seq_len(n),
    function(i) {
      x <- try(
        as.numeric(stats::predict(
          params$m,
          data.frame(Home = 0, Team = away[i], Opponent = home[i]),
          type = "response"
        )),
        silent = TRUE
      )
      if (is.numeric(x)) {
        x
      } else {
        .DCPredictErrorRecover(
          team = away[i],
          opponent = home[i],
          homeiceadv = FALSE
        )
      }
    },
    numeric(1)
  )

  if (!is.null(expected_mean) && !is.null(season_percent)) {
    lam <- lam *
      (1 - 1 / 3 * season_percent) +
      expected_mean * (1 / 3 * season_percent)
    mu <- mu *
      (1 - 1 / 3 * season_percent) +
      expected_mean * (1 / 3 * season_percent)
  }

  # Back-to-back adjustment (#64), applied after the regression blend so this
  # fixed situational effect is not shrunk by `season_percent`. Mirrors the
  # per-game scaling in [.dcLambda()]: a b2b home team scores less and concedes
  # more; a b2b away team does the opposite. Both can apply to the same game.
  if (any(home_b2b)) {
    lam[home_b2b] <- lam[home_b2b] * exp(-B2B_HOME_NHL)
    mu[home_b2b] <- mu[home_b2b] * exp(B2B_HOME_NHL)
  }
  if (any(away_b2b)) {
    mu[away_b2b] <- mu[away_b2b] * exp(-B2B_AWAY_NHL)
    lam[away_b2b] <- lam[away_b2b] * exp(B2B_AWAY_NHL)
  }

  pm <- .dcProbArray(lambda = lam, mu = mu, params = params, maxgoal = maxgoal)

  # Per-game raw (home, draw, away) regulation probabilities from the matrix.
  hw <- apply(pm, 3, function(x) sum(x[lower.tri(x)]))
  dw <- apply(pm, 3, function(x) sum(diag(x)))
  aw <- apply(pm, 3, function(x) sum(x[upper.tri(x)]))

  if (!draws) {
    # Distribute the draw probability proportionally to home/away (both from
    # the original values), then normalize each game's pair to sum to 1.
    pair <- hw + aw
    h2 <- hw + hw / pair * dw
    a2 <- aw + aw / pair * dw
    p2 <- h2 + a2
    hw <- h2 / p2
    aw <- a2 / p2
    return(matrix(
      c(hw, aw),
      nrow = n,
      ncol = 2,
      byrow = FALSE,
      dimnames = list(NULL, c("HomeWin", "AwayWin"))
    ))
  }

  # Normalize each game's triple to sum to 1 (mirrors DCPredict's normalizeOdds).
  tot <- hw + dw + aw
  hw <- hw / tot
  dw <- dw / tot
  aw <- aw / tot
  return(matrix(
    c(hw, dw, aw),
    nrow = n,
    ncol = 3,
    byrow = FALSE,
    dimnames = list(NULL, c("HomeWin", "Draw", "AwayWin"))
  ))
}

#' Compute expected goals for a home/away pairing
#'
#' @description Returns the Poisson expected-goals `lambda` (home) and `mu`
#'   (away) for a pairing by reading the home and away coefficients out of the
#'   fitted model `m` (via [stats::predict()] at `Home = 1` / `Home = 0`).
#'   These are the rate parameters that [.prob_matrix()] uses to build the
#'   score matrix.  If a team or opponent is not in the model (e.g. a new
#'   expansion team), a fallback estimate is supplied by
#'   [.DCPredictErrorRecover()].
#'
#' @param home (`character(1)`) Home team name.
#' @param away (`character(1)`) Away team name.
#' @param params (`list` or `NULL`) Pre-parsed Dixon-Coles parameter list
#'   (`m`, `rho`, `beta`, `eta`, `k`).  Must be the output of
#'   [.parse_dc_params()]; callers in the public entry chain are responsible
#'   for parsing once (#47).  `NULL` triggers a fallback parse (for direct
#'   test calls).
#' @returns (`list`) A named list with elements `home` and `away`, each a
#'   single expected-goals (`lambda`/`mu`) value.
.dcLambda <- function(home, away, params = NULL) {
  if (is.null(params)) {
    params <- .parse_dc_params(params)
  }
  xg <- list("home" = NA, "away" = NA)

  # Expected goals home
  xg$home <- try(
    stats::predict(
      params$m,
      data.frame(Home = 1, Team = home, Opponent = away),
      type = "response"
    )[1],
    TRUE
  )

  # Expected goals away
  xg$away <- try(
    stats::predict(
      params$m,
      data.frame(Home = 0, Team = away, Opponent = home),
      type = "response"
    )[1],
    TRUE
  )

  if (!is.numeric(xg$home)) {
    xg$home <- .DCPredictErrorRecover(
      team = home,
      opponent = away,
      homeiceadv = TRUE
    )
  }
  if (!is.numeric(xg$away)) {
    xg$away <- .DCPredictErrorRecover(
      team = away,
      opponent = home,
      homeiceadv = FALSE
    )
  }

  return(xg)
}

#' Apply the back-to-back (b2b) adjustment to expected goals (#64)
#'
#' @description Scales a home/away expected-goals pair for a game in which one or
#'   both teams are on a back-to-back.  On the log link this is exactly equivalent
#'   to subtracting from the b2b team's attack and defensive-strength coefficients:
#'   a b2b home team scores less (`lambda` down) and concedes more (`mu` up); a b2b
#'   away team does the opposite.  Both teams can be on a b2b in the same game, so
#'   both adjustments compose.  This is applied *after* any regression-to-mean
#'   blend (see [.dcProbMatrix()] and [.dcPredictVectorized()]) because it is a fixed
#'   situational adjustment that should not be shrunk by `season_percent`.
#'
#' @param lambda (`double`) Home expected goals.
#' @param mu (`double`) Away expected goals.
#' @param home_b2b (`logical(1)`) Whether the home team is on a back-to-back.
#'   Defaults to `FALSE`.
#' @param away_b2b (`logical(1)`) Whether the away team is on a back-to-back.
#'   Defaults to `FALSE`.
#' @param eps_home (`double` or `NULL`) Home b2b magnitude; defaults to
#'   [B2B_HOME_NHL].  Overridable so [.tune_b2b()] can evaluate candidate values.
#' @param eps_away (`double` or `NULL`) Away b2b magnitude; defaults to
#'   [B2B_AWAY_NHL].  Overridable for tuning.
#'
#' @returns (`list`) A named list with elements `lambda` and `mu`, each adjusted.
#' @keywords internal
.apply_b2b <- function(
  lambda,
  mu,
  home_b2b = FALSE,
  away_b2b = FALSE,
  eps_home = NULL,
  eps_away = NULL
) {
  if (is.null(eps_home)) {
    eps_home <- B2B_HOME_NHL
  }
  if (is.null(eps_away)) {
    eps_away <- B2B_AWAY_NHL
  }
  if (isTRUE(home_b2b)) {
    lambda <- lambda * exp(-eps_home)
    mu <- mu * exp(eps_home)
  }
  if (isTRUE(away_b2b)) {
    mu <- mu * exp(-eps_away)
    lambda <- lambda * exp(eps_away)
  }
  return(list(lambda = lambda, mu = mu))
}

#' Compute expected goals from the Dixon-Coles probability matrix
#'
#' @description Returns the model's expected-goals `xg` for a pairing by taking
#'   the weighted mean of each side's goals under the full Dixon-Coles score
#'   matrix ([.dcProbMatrix()]).  Because the matrix includes the Dixon-Coles
#'   low-score and Weibull diagonal adjustments, the expected goals here differ
#'   slightly from the raw Poisson `lambda`/`mu` in [.dcLambda()].
#'
#' @param home (`character(1)`) Home team name.
#' @param away (`character(1)`) Away team name.
#' @param params (`list` or `NULL`) Pre-parsed Dixon-Coles parameter list
#'   (`m`, `rho`, `beta`, `eta`, `k`).  Must be the output of
#'   [.parse_dc_params()]; callers in the public entry chain are responsible
#'   for parsing once (#47).  `NULL` triggers a fallback parse (for direct
#'   test calls).
#' @param maxgoal (`integer(1)`) Maximum goals included per team.
#' @param home_b2b (`logical(1)`) Whether the home team is on a back-to-back;
#'   passed to [.dcProbMatrix()] so the displayed xG matches the adjusted odds.
#'   Defaults to `FALSE`.
#' @param away_b2b (`logical(1)`) Whether the away team is on a back-to-back;
#'   passed to [.dcProbMatrix()].  Defaults to `FALSE`.
#'
#' @returns (`list`) A named list with elements `home` and `away`, each the
#'   expected-goals value for that side.
#' @keywords internal
.dcxG <- function(
  home,
  away,
  params = NULL,
  maxgoal = 10,
  home_b2b = FALSE,
  away_b2b = FALSE
) {
  pm <- .dcProbMatrix(
    home = home,
    away = away,
    params = params,
    maxgoal = maxgoal,
    home_b2b = home_b2b,
    away_b2b = away_b2b
  )

  away_xg <- stats::weighted.mean(0:maxgoal, colSums(pm))
  home_xg <- stats::weighted.mean(0:maxgoal, rowSums(pm))

  return(list("home" = home_xg, "away" = away_xg))
}

#' Build the Dixon-Coles score-probability matrix for a pairing
#'
#' @description Team-aware wrapper around [.prob_matrix()].  Resolves the
#'   expected-goals `lambda` (home) and `mu` (away) for the given pairing via
#'   [.dcLambda()], optionally applies the regression-to-the-mean blend (see
#'   `expected_mean` / `season_percent`), and delegates to [.prob_matrix()] to
#'   produce the adjusted score matrix.  Use this when you have *teams*; use
#'   [.prob_matrix()] directly when you already have `lambda` and `mu`.
#'
#' @param home (`character(1)`) Home team name.
#' @param away (`character(1)`) Away team name.
#' @param params (`list` or `NULL`) Pre-parsed Dixon-Coles parameter list
#'   (`m`, `rho`, `beta`, `eta`, `k`).  Must be the output of
#'   [.parse_dc_params()]; callers in the public entry chain are responsible
#'   for parsing once (#47).  `NULL` triggers a fallback parse (for direct
#'   test calls).
#' @param maxgoal (`integer(1)`) Maximum number of goals per team to include.
#' @param scores (`data.frame`) Historical scores, used only to compute `m`
#'   and `rho` if they are missing from `params`.  Defaults to
#'   [HockeyModel::scores].
#' @param expected_mean (`double` or `NULL`) League mean of `lambda`/`mu`,
#'   used only together with `season_percent` for the regression blend.
#' @param season_percent (`double` or `NULL`) Fraction of the season complete
#'   for the regression blend.
#'
#' @param home_b2b (`logical(1)`) Whether the home team is on a back-to-back.
#'   When `TRUE`, [.apply_b2b()] scales the expected goals by exp(+/-
#'   `B2B_HOME_NHL`).  Defaults to `FALSE`.
#' @param away_b2b (`logical(1)`) Whether the away team is on a back-to-back.
#'   When `TRUE`, [.apply_b2b()] scales the expected goals by exp(+/-
#'   `B2B_AWAY_NHL`).  Defaults to `FALSE`.
#' @param eps_home (`double` or `NULL`) Home b2b magnitude; defaults to
#'   [B2B_HOME_NHL].  Overridable for tuning.
#' @param eps_away (`double` or `NULL`) Away b2b magnitude; defaults to
#'   [B2B_AWAY_NHL].  Overridable for tuning.
#'
#' @returns (`matrix`) A square `(maxgoal + 1) x (maxgoal + 1)` probability
#'   matrix with home goals on rows and away goals on columns, summing to `1`.
.dcProbMatrix <- function(
  home,
  away,
  params = NULL,
  maxgoal = 10,
  scores = HockeyModel::scores,
  expected_mean = NULL,
  season_percent = NULL,
  home_b2b = FALSE,
  away_b2b = FALSE,
  eps_home = NULL,
  eps_away = NULL
) {
  if (is.null(params)) {
    params <- .parse_dc_params(params)
  }
  xg <- .dcLambda(home = home, away = away, params = params)
  # Expected goals home
  lambda <- as.numeric(xg$home)

  # Expected goals away
  mu <- as.numeric(xg$away)

  if (!is.null(expected_mean) && !is.null(season_percent)) {
    lambda <- lambda *
      (1 - 1 / 3 * season_percent) +
      expected_mean * (1 / 3 * season_percent)
    mu <- mu *
      (1 - 1 / 3 * season_percent) +
      expected_mean * (1 / 3 * season_percent)
  }

  # Back-to-back adjustment (#64), applied after the regression blend so this
  # fixed situational effect is not shrunk by `season_percent`.
  b2b <- .apply_b2b(
    lambda,
    mu,
    home_b2b = home_b2b,
    away_b2b = away_b2b,
    eps_home = eps_home,
    eps_away = eps_away
  )
  lambda <- b2b$lambda
  mu <- b2b$mu

  probability_matrix <- .prob_matrix(
    lambda = lambda,
    mu = mu,
    params = params,
    maxgoal = maxgoal
  )

  return(probability_matrix)
}

#' Build the Dixon-Coles score-probability matrix from expected goals
#'
#' @description The central score-matrix builder, operating directly on the
#'   expected-goals `lambda` and `mu`.  It differs from [.dcProbMatrix()] only
#'   in that no team lookup or regression-to-the-mean happens here — callers
#'   supply `lambda` and `mu` directly (e.g. from [.dcLambda()]).
#'
#'   Starting from independent Poisson counts with rates `lambda` (home) and
#'   `mu` (away) over scores `0:maxgoal`, it applies three adjustments and
#'   renormalises:
#'   1. **Dixon-Coles low-score correction** — multiplies the four low-score
#'      cells by [tau()] (via `rho`, the dependence between the teams' counts
#'      in tight games, see [getRho()]).  Cells pushed slightly negative near
#'      the edges of `rho`'s range are clipped to `0`.
#'   2. **Weibull diagonal enhancement** — when `enhance = TRUE`, multiplies
#'      each tied cell \eqn{(x,x)} by \eqn{\text{Weibull}(x;\beta,\eta)\cdot k}
#'      so the diagonal mass approaches the empirical tie rate (see
#'      [getWeibullParams()]).
#'   3. **Renormalisation** — the off-diagonal (win/loss) mass is scaled by
#'      `1 - sum(diag) / sum(all)` so the matrix always sums to `1` after the
#'      diagonal inflation.  Because the off-diagonal is scaled relative to the
#'      inflated diagonal, the result depends on the Weibull *shape*, not the
#'      absolute scale of `k`.
#'
#' @param lambda (`double(1)`) Home expected goals (Poisson `lambda`, from
#'   [getM()] `$m` or [.dcLambda()]).
#' @param mu (`double(1)`) Away expected goals (Poisson `mu`, from [getM()]
#'   `$m` or [.dcLambda()]).
#' @param params (`list` or `NULL`) Pre-parsed Dixon-Coles parameter list
#'   (`m`, `rho`, `beta`, `eta`, `k`).  Must be the output of
#'   [.parse_dc_params()]; callers in the public entry chain are responsible
#'   for parsing once (#47).  `NULL` triggers a fallback parse (for direct
#'   test calls).
#' @param maxgoal (`integer(1)`) Maximum number of goals per team to include.
#' @param enhance (`logical(1)`) Apply the Weibull diagonal tie-enhancement
#'   (`TRUE`, default) and renormalize the off-diagonal against it. When
#'   `FALSE` the unmodified standard Dixon-Coles diagonal is kept: the Weibull
#'   diagonal multiplier is skipped (equivalent to `k = 0`) and the off-diagonal
#'   is *not* renormalized against a zeroed diagonal, so the returned matrix is
#'   the tau-adjusted bivariate-Poisson score distribution normalized to sum to
#'   1.
#'
#' @returns (`matrix`) A square `(maxgoal + 1) x (maxgoal + 1)` probability
#'   matrix with home goals on rows and away goals on columns, all entries in
#'   `[0, 1]` and summing to `1` (validated by [.validateProbMatrix()]).
#' @keywords internal
.prob_matrix <- function(lambda, mu, params = NULL, maxgoal, enhance = TRUE) {
  if (is.null(params)) {
    params <- .parse_dc_params(params)
  }
  probability_matrix <- stats::dpois(0:maxgoal, lambda) %*%
    t(stats::dpois(0:maxgoal, mu))

  # scaling_matrix <- matrix(tau(c(0, 1, 0, 1), c(0, 0, 1, 1), lambda, mu, params$rho), nrow = 2)
  # mvoed tau into vector - lookup and vectorized was too slow.
  scaling_matrix <- matrix(
    c(
      1 - (lambda * mu * params$rho),
      1 + (mu * params$rho),
      1 + (lambda * params$rho),
      1 - params$rho
    ),
    nrow = 2
  )
  probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix
  # The tau adjustment can push the (0,0)/(0,1)/(1,0)/(1,1) cells slightly
  # negative near the edges of rho's valid range (a known Dixon-Coles
  # boundary artifact -- see Dixon & Coles 1997). Clamp here, before the
  # diagonal/off-diagonal renormalization below, so a negative low-goal cell
  # can't leak into the final matrix or corrupt the renormalization sums.
  probability_matrix[1:2, 1:2][probability_matrix[1:2, 1:2] < 0] <- 0

  # enhance = FALSE (or a non-positive k) returns the unmodified standard
  # Dixon-Coles score distribution: the Weibull diagonal multiplier is skipped
  # and, crucially, the off-diagonal is NOT renormalized against a zeroed
  # diagonal (that renormalization is what makes the enhanced pipeline's
  # output independent of k). The matrix is simply the tau-adjusted
  # bivariate-Poisson mass normalized to sum to 1, so the diagonal is the
  # genuine theoretical tie probability.
  if (!enhance || params$k <= 0) {
    probability_matrix <- probability_matrix / sum(probability_matrix)
    .validateProbMatrix(probability_matrix, context = ".prob_matrix")
    return(probability_matrix)
  }

  diag(probability_matrix) <- diag(probability_matrix) *
    stats::dweibull(
      c(1:(maxgoal + 1)),
      shape = params$beta,
      scale = params$eta
    ) *
    params$k
  # for(d in 1:(maxgoal+1)){
  # probability_matrix[d,d]<-probability_matrix[d,d] * (stats::dweibull(d, shape = params$beta, scale = params$eta) * params$k)
  # }

  # probability_matrix <- probability_matrix/sum(probability_matrix)  # turn into a sum=1 matrix
  #  #  Normalizing the whole matrix reduces the effect of the tie enhancement.

  diag_sum <- sum(diag(probability_matrix))
  if (diag_sum >= 1) {
    # The Weibull tie-enhancement (params$k/beta/eta) can inflate the diagonal
    # past 1 for extreme inputs (e.g. very low/high expected goals combined
    # with a large k). Renormalizing the off-diagonal by (1 - diag_sum) would
    # then divide by a negative number and flip legitimate probabilities
    # negative. Cap the diagonal so it leaves a small positive remainder for
    # the off-diagonal (win/loss) outcomes instead.
    warning(
      ".prob_matrix: tie-enhanced diagonal probability (",
      signif(diag_sum, 4),
      ") reached or exceeded 1; capping to leave room for win/loss outcomes."
    )
    diag(probability_matrix) <- diag(probability_matrix) *
      (1 - 1e-6) /
      diag_sum
    diag_sum <- sum(diag(probability_matrix))
  }

  renorm <- 1 - diag_sum
  normfact <- sum(
    probability_matrix[upper.tri(probability_matrix)],
    probability_matrix[lower.tri(probability_matrix)]
  ) /
    renorm

  probability_matrix[upper.tri(
    probability_matrix
  )] <- probability_matrix[upper.tri(probability_matrix)] / normfact
  probability_matrix[lower.tri(
    probability_matrix
  )] <- probability_matrix[lower.tri(probability_matrix)] / normfact

  .validateProbMatrix(probability_matrix, context = ".prob_matrix")
}

#' Sample a single random game result from the Dixon-Coles model
#'
#' @description Draws one random game outcome by sampling a final score from
#'   the Dixon-Coles score-probability matrix ([.dcProbMatrix()]).  If the
#'   sampled score is a tie, an overtime/shootout winner is drawn (OT vs SO at
#'   the historical `0.686`/`0.314` split, with the side chosen by
#'   [.extraTimeSolver()]) and an extra goal is added to the winner — mirroring
#'   how a real tie is resolved in extra time.
#'
#'   Each call is independent, so repeated calls return a new random result.
#'
#' @param home (`character(1)`) Home team name.
#' @param away (`character(1)`) Away team name.
#' @param params (`list` or `NULL`) The named list containing `m`, `rho`,
#'   `beta`, `eta`, and `k`.  If `NULL`, the package defaults are used (see
#'   [updateModel()]).
#' @param maxgoal (`integer(1)`) Maximum number of goals per team to include.
#'   Defaults to `10`.
#' @param scores (`data.frame`) Historical scores, used only to compute `m` and
#'   `rho` when they are not in `params`.  Defaults to [HockeyModel::scores].
#' @param expected_mean (`double` or `NULL`) League mean of `lambda`/`mu` for
#'   regression-to-the-mean; used only with `season_percent`.
#' @param season_percent (`double` or `NULL`) Fraction of the season complete
#'   for the regression blend.
#' @param as_result (`logical(1)`) If `TRUE` (default), return a single
#'   `scores$Result`-style code (`1`, `0.75`, `0.6`, `0.4`, `0.25`, or `0`).
#'   If `FALSE`, return a list with the raw `HomeGoals`, `AwayGoals`, and
#'   `OTStatus`.
#'
#' @returns A single `Result` code when `as_result = TRUE`, or a list with
#'   `HomeGoals`, `AwayGoals`, and `OTStatus` when `as_result = FALSE`.
#' @export
#'
#' @examples \dontrun{
#'   # A random regulation/OT/SO result (one of: 1, 0.75, 0.6, 0.4, 0.25, 0)
#'   dcSample("Toronto Maple Leafs", "Montreal Canadiens")
#'
#'   # Or the raw final score and extra-time status
#'   dcSample(
#'     "Toronto Maple Leafs",
#'     "Montreal Canadiens",
#'     as_result = FALSE
#'   )
#' }
dcSample <- function(
  home,
  away,
  params = NULL,
  maxgoal = 10,
  scores = HockeyModel::scores,
  expected_mean = NULL,
  season_percent = NULL,
  as_result = TRUE
) {
  if (is.null(params)) {
    params <- .parse_dc_params(params)
  }
  pm <- .dcProbMatrix(
    home = home,
    away = away,
    params = params,
    maxgoal = maxgoal
  )

  # .prob_matrix() already guarantees valid probabilities (see .validateProbMatrix);
  # re-validate here in case pm was constructed/modified by a caller.
  pm <- .validateProbMatrix(pm, context = "dcSample probability matrix")

  goals <- as.vector(arrayInd(
    sample(seq_along(pm), size = 1, prob = pm),
    .dim = dim(pm)
  )) -
    1

  if (goals[1] == goals[2]) {
    # TODO Verify OT/SO ratio and also verify if wniner is coin flip or stronger team has better chance?
    otstatus <- sample(c("OT", "SO"), size = 1, prob = c(0.6858606, 0.3141394))
    otwinner <- sample(
      c("Home", "Away"),
      size = 1,
      prob = .extraTimeSolver(
        sum(pm[lower.tri(pm)]),
        sum(pm[upper.tri(pm)]),
        sum(diag(pm))
      )[2:3]
    )
    if (otwinner == "Home") {
      goals[1] <- goals[1] + 1
    } else {
      goals[2] <- goals[2] + 1
    }
  } else {
    otstatus <- ""
  }
  if (as_result) {
    return(dplyr::case_when(
      goals[1] > goals[2] & otstatus == "" ~ 1,
      goals[1] < goals[2] & otstatus == "" ~ 0,
      goals[1] > goals[2] & otstatus == "OT" ~ 0.75,
      goals[1] > goals[2] & otstatus == "SO" ~ 0.6,
      goals[1] < goals[2] & otstatus == "OT" ~ 0.25,
      goals[1] < goals[2] & otstatus == "SO" ~ 0.4
    ))
  } else {
    return(list(
      "HomeGoals" = goals[1],
      "AwayGoals" = goals[2],
      "OTStatus" = otstatus
    ))
  }
}

#' Sample game result(s) from the Dixon-Coles score matrix
#'
#' @description Builds the score matrix via [.prob_matrix()], decomposes the
#'   draw mass into overtime/shootout outcomes with [.extraTimeSolver()], then
#'   draws `nsim` result code(s) from the six-outcome distribution
#'   (home win, home OT, home SO, away SO, away OT, away win) at the historical
#'   `0.686`/`0.314` OT/SO split.  A convenient sampling counterpart to
#'   [.dcExpandedOdds()] (which returns the probabilities without sampling).
#'
#' @param lambda (`double` or `numeric`) Home expected goal(s), one per game.
#' @param mu (`double` or `numeric`) Away expected goal(s), one per game.
#' @param params (`list` or `NULL`) Pre-parsed Dixon-Coles parameter list
#'   (`m`, `rho`, `beta`, `eta`, `k`).  Must be the output of
#'   [.parse_dc_params()]; callers in the public entry chain are responsible
#'   for parsing once (#47).  `NULL` triggers a fallback parse (for direct
#'   test calls).
#' @param maxgoal (`integer(1)`) Maximum goals per team to include.  Defaults
#'   to `8`.
#' @param nsim (`integer(1)`) Number of simulations to draw per game.
#'
#' @returns (`numeric`) One or more result codes from `c(1, 0.75, 0.6, 0.4,
#'   0.25, 0)`, matching the `scores$Result` encoding (see [scores]).
#' @keywords internal
.dcResult <- function(lambda, mu, params = NULL, maxgoal = 8, nsim = 1) {
  if (is.null(params)) {
    params <- .parse_dc_params(params)
  }
  .dcr <- function(lambda, mu, params, maxgoal, nsim) {
    if (is.na(lambda)) {
      return(NA)
    }
    pm <- .prob_matrix(
      lambda = lambda,
      mu = mu,
      params = params,
      maxgoal = maxgoal
    )

    homewinprob <- sum(pm[lower.tri(pm)])
    awaywinprob <- sum(pm[upper.tri(pm)])
    otwinnerprob <- .extraTimeSolver(homewinprob, awaywinprob, sum(diag(pm)))[
      2:3
    ]
    resultprob <- c(
      homewinprob,
      otwinnerprob[1] * 0.6858606,
      otwinnerprob[1] * 0.3141394,
      otwinnerprob[2] * 0.3141394,
      otwinnerprob[2] * 0.6858606,
      awaywinprob
    )
    resultprob <- .validateProbMatrix(
      resultprob,
      context = ".dcResult resultprob"
    )
    results <- sample(
      c(1, 0.75, 0.6, 0.4, 0.25, 0),
      size = nsim,
      replace = TRUE,
      prob = resultprob
    )
    return(results)
  }

  v_dcr <- Vectorize(.dcr, c("lambda", "mu"))

  if (length(lambda) == 1) {
    return(.dcr(lambda, mu, params, maxgoal, nsim))
  } else {
    return(as.vector(v_dcr(lambda, mu, params, maxgoal, nsim)))
  }
}

#' Sample one or more result labels from expanded win-state probabilities
#'
#' @param hw (`double`) Regulation home-win probability.
#' @param hot (`double`) Home overtime-win probability.
#' @param hso (`double`) Home shootout-win probability.
#' @param aso (`double`) Away shootout-win probability.
#' @param aot (`double`) Away overtime-win probability.
#' @param aw (`double`) Regulation away-win probability.
#' @param size (`integer(1)`) Number of samples to draw.
#' @returns (`numeric`) Result values encoded like `scores$Result`.
#' @keywords internal
.sampleResult <- function(hw, hot, hso, aso, aot, aw, size = 1) {
  .sr <- function(hw, hot, hso, aso, aot, aw, size) {
    return(sample(
      c(1, 0.75, 0.6, 0.4, 0.25, 0),
      size = size,
      replace = TRUE,
      prob = c(hw, hot, hso, aso, aot, aw)
    ))
  }
  v_sr <- Vectorize(.sr, c("hw", "hot", "hso", "aso", "aot", "aw"))

  if (size == 1) {
    return(.sr(hw, hot, hso, aso, aot, aw, size))
  } else {
    return(as.vector(v_sr(hw, hot, hso, aso, aot, aw, size)))
  }
}

#' Expand DC odds into regulation/OT/SO result components
#'
#' @param lambda (`double`) Home expected goals.
#' @param mu (`double`) Away expected goals.
#' @param params Pre-parsed Dixon-Coles parameter list (m, rho, beta, eta, k).
#'   Must be the output of [.parse_dc_params()]; callers in the public entry
#'   chain are responsible for parsing once (#47). `NULL` triggers a fallback
#'   parse (for direct test calls).
#' @param maxgoal (`integer(1)`) Maximum goals included per team.
#' @returns (`numeric`) Probabilities for home win, home OT, home SO, away SO,
#'   away OT, and away win.
#' @keywords internal
.dcExpandedOdds <- function(lambda, mu, params = NULL, maxgoal = 8) {
  if (is.null(params)) {
    params <- .parse_dc_params(params)
  }
  .dceo <- function(lambda, mu, params, maxgoal, nsim) {
    if (is.na(lambda)) {
      return(NA)
    }
    pm <- .prob_matrix(
      lambda = lambda,
      mu = mu,
      params = params,
      maxgoal = maxgoal
    )

    homewinprob <- sum(pm[lower.tri(pm)])
    awaywinprob <- sum(pm[upper.tri(pm)])
    otwinnerprob <- .extraTimeSolver(homewinprob, awaywinprob, sum(diag(pm)))[
      2:3
    ]
    resultprob <- c(
      homewinprob,
      otwinnerprob[1] * 0.6858606,
      otwinnerprob[1] * 0.3141394,
      otwinnerprob[2] * 0.3141394,
      otwinnerprob[2] * 0.6858606,
      awaywinprob
    )
    return(.validateProbMatrix(
      resultprob,
      context = ".dcExpandedOdds resultprob"
    ))
  }

  v_dceo <- Vectorize(.dceo, c("lambda", "mu"))

  if (length(lambda) == 1) {
    return(.dceo(lambda, mu, params, maxgoal))
  } else {
    return(v_dceo(lambda, mu, params, maxgoal))
  }
}

#' Recover fallback expected goals when a team is missing from the model
#'
#' @description Used by [.dcLambda()] and [.dcPredictVectorized()] when a team
#'   or opponent is not in the fitted model (e.g. a newly-expanded team).  It
#'   estimates expected goals by pairing the known team's coefficient with a
#'   conservative opponent coefficient (the lowest attack or highest defence in
#'   the model), or falls back to the historical league-average goal rate
#'   (home `3.32`, away `2.83`) when both are unknown.  This keeps the model
#'   from erroring on unseen teams.
#'
#' @param team (`character(1)`) Team being predicted.
#' @param opponent (`character(1)`) Opponent team.
#' @param homeiceadv (`logical(1)`) Whether to include the home-ice
#'   coefficient.  Defaults to `FALSE`.
#' @param m (`glm`) Fitted Dixon-Coles model.  Defaults to
#'   [HockeyModel::m].
#'
#' @returns (`double(1)`) Fallback expected-goals estimate.
#' @keywords internal
.DCPredictErrorRecover <- function(
  team,
  opponent,
  homeiceadv = FALSE,
  m = HockeyModel::m
) {
  teamlist <- unique(m$data$Team)
  opponentlist <- unique(m$data$Opponent)

  if (homeiceadv) {
    homeice <- m$coefficients["Home"]
  } else {
    homeice <- 0
  }

  if (!(team %in% teamlist) && !(opponent %in% opponentlist)) {
    lambda <- NA
  } else if (!(team %in% teamlist)) {
    teamp <- min(m$coefficients[grep("Team", names(m$coefficients))]) # lowest goals scored for new team
    opponentp <- m$coefficients[paste0("Opponent", opponent)]

    lambda <- exp(teamp + opponentp + homeice)
  } else if (!(opponent %in% opponentlist)) {
    teamp <- m$coefficients[paste0("Team", team)]
    opponentp <- max(m$coefficients[grep("Opponent", names(m$coefficients))]) # most goals allowed for new team

    lambda <- exp(teamp + opponentp + homeice)
  } else {
    lambda <- NA
  }

  if (is.na(lambda)) {
    if (homeiceadv) {
      lambda <- 3.319624 # Historical home goals
    } else {
      lambda <- 2.827417 # Historical away goals
    }
  }

  return(unname(lambda))
}
