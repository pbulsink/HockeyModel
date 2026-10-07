# Dixon-Coles predictions: probability matrices, expected goals, and result sampling

#' DC Predict home/draw/away win
#'
#' @description Using Dixon-Coles' technique, predict odds each of home win, draw, and away win.
#'
#' @param home home team
#' @param away away team
#' @param params The named list containing m, rho, beta, eta, and k. See [updateModel()] for information on the params list
#' @param maxgoal max number of goals per team
#' @param scores optional, if not supplying m & rho, scores used to calculate them.
#' @param expected_mean the mean lambda & mu, used only for regression
#' @param season_percent the percent complete of the season, used for regression
#' @param draws Whether draws are allowed. Default True
#'
#' @return a vector of home win, draw, and away win probability, or if draws=False, a vector of home and away win probability
#' @export
DCPredict <- function(
  home,
  away,
  params = NULL,
  maxgoal = 10,
  scores = HockeyModel::scores,
  expected_mean = NULL,
  season_percent = NULL,
  draws = TRUE
) {
  params <- .parse_dc_params(params = params)
  probability_matrix <- .dcProbMatrix(
    home = home,
    away = away,
    params = params,
    maxgoal = maxgoal,
    expected_mean = expected_mean,
    season_percent = season_percent
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
#' @param params (`list`) Dixon-Coles parameter list (m, rho, beta, eta, k).
#' @param maxgoal (`integer(1)`) Maximum goals per team.
#' @returns (`array`) Probability array of dim `(maxgoal+1) x (maxgoal+1) x n`.
#' @keywords internal
.dcProbArray <- function(lambda, mu, params, maxgoal) {
  params <- .parse_dc_params(params)
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
    out[, , j] <- .build_one(lambda[j], mu[j])
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
#' @param params (`list`) Dixon-Coles parameter list (m, rho, beta, eta, k).
#' @param maxgoal (`integer(1)`) Maximum goals per team.
#' @param expected_mean (`double` or `NULL`) Mean lambda/mu for regression.
#' @param season_percent (`double` or `NULL`) Season completion fraction for
#'   regression.
#' @param draws (`logical(1)`) Whether to return a 3-column (home, draw, away)
#'   matrix; if `FALSE`, returns a 2-column (home, away) matrix with the draw
#'   probability distributed proportionally.
#' @returns (`matrix(n x 3)` or `matrix(n x 2)`) Probability columns.
#' @keywords internal
.dcPredictVectorized <- function(
  home,
  away,
  params = NULL,
  maxgoal = 10,
  expected_mean = NULL,
  season_percent = NULL,
  draws = TRUE
) {
  params <- .parse_dc_params(params)
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

#' DC Expected Goals
#'
#' @description Given a home and away team, provide lambda values
#'
#' @param home The home team name
#' @param away The away team name
#' @param params The named list containing m, rho, beta, eta, and k. See [updateModel()] for information on the params list
#'
#' @return a list of $home and $away Poisson Lambda values -
.dcLambda <- function(home, away, params = NULL) {
  params <- .parse_dc_params(params = params)
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

#' Compute expected goals from a Dixon-Coles probability matrix
#'
#' @param home (`character(1)`) Home team name.
#' @param away (`character(1)`) Away team name.
#' @param params (`list` or `NULL`) Dixon-Coles parameter list.
#' @param maxgoal (`integer(1)`) Maximum goals included per team.
#' @returns (`list`) Home and away expected goals.
#' @keywords internal
.dcxG <- function(home, away, params = NULL, maxgoal = 10) {
  pm <- .dcProbMatrix(
    home = home,
    away = away,
    params = params,
    maxgoal = maxgoal
  )

  away_xg <- stats::weighted.mean(0:maxgoal, colSums(pm))
  home_xg <- stats::weighted.mean(0:maxgoal, rowSums(pm))

  return(list("home" = home_xg, "away" = away_xg))
}

#' Generate the Dixon-Coles' Probability Matrix
#'
#' @param home home team
#' @param away away team
#' @param params The named list containing m, rho, beta, eta, and k. See [updateModel()] for information on the params list
#' @param maxgoal max number of goals per team
#' @param scores optional, if not supplying m & rho, scores used to calculate them.
#' @param expected_mean the mean lambda & mu, used only for regression
#' @param season_percent the percent complete of the season, used for regression
#'
#' @return a square matrix of dims 0:maxgoal with odds at each count of  home goals on 'rows' and away goals  on 'columns'
.dcProbMatrix <- function(
  home,
  away,
  params = NULL,
  maxgoal = 10,
  scores = HockeyModel::scores,
  expected_mean = NULL,
  season_percent = NULL
) {
  params <- .parse_dc_params(params = params)

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

  probability_matrix <- .prob_matrix(
    lambda = lambda,
    mu = mu,
    params = params,
    maxgoal = maxgoal
  )

  return(probability_matrix)
}

#' Probability Matrix
#'
#' @description Given a mu, lambda, rho, and theta, generate a probability matrix. Differs from .dcProbMatrix in that no regresson or solving for supplied teams happens
#'
#' @param lambda home lambda
#' @param mu away mu
#' @param params The named list containing m, rho, beta, eta, and k. See [updateModel()] for information on the params list
#' @param maxgoal max goals per game
#'
#' @return a square matrix of maxgoal:maxgoal, with all entries in `[0, 1]` and
#'   summing to 1 (see [.validateProbMatrix])
.prob_matrix <- function(lambda, mu, params, maxgoal) {
  params <- .parse_dc_params(params)
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

#' DC Sample
#'
#' @description Get a random single game result using DC method. repeated running should give a new value each time
#'
#' @param home home team
#' @param away away team
#' @param params The named list containing m, rho, beta, eta, and k. See [updateModel()] for information on the params list
#' @param maxgoal max number of goals per team
#' @param scores optional, if not supplying m & rho, scores used to calculate them.
#' @param expected_mean the mean lambda & mu, used only for regression
#' @param season_percent the percent complete of the season, used for regression
#' @param as_result Whether to give a score or just flatten it to result type (see \link{scores} for score result)
#'
#' @return a random Home & away goals & OT/SO status if needed
#' @export
#'
#' @examples dcSample("Toronto Maple Leafs", "Montreal Canadiens")
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
  params <- .parse_dc_params(params)
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

#' DC Result Sample
#'
#' @param lambda home team lambda
#' @param mu away team mu
#' @param params The named list containing m, rho, beta, eta, and k. See [updateModel()] for information on the params list
#' @param maxgoal max goals predicable per game, default 10
#' @param nsim the number of simulations in each result
#'
#' @return a result from 0 to 1 corresponding to \link{scores} results
.dcResult <- function(lambda, mu, params = NULL, maxgoal = 8, nsim = 1) {
  params <- .parse_dc_params(params)

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
#' @param params (`list` or `NULL`) Dixon-Coles parameter list.
#' @param maxgoal (`integer(1)`) Maximum goals included per team.
#' @returns (`numeric`) Probabilities for home win, home OT, home SO, away SO,
#'   away OT, and away win.
#' @keywords internal
.dcExpandedOdds <- function(lambda, mu, params = NULL, maxgoal = 8) {
  params <- .parse_dc_params(params)

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

#' Recover fallback expected goals when model prediction fails
#'
#' @param team (`character(1)`) Team being predicted.
#' @param opponent (`character(1)`) Opponent team.
#' @param homeiceadv (`logical(1)`) Whether to include home-ice coefficient.
#' @param m (`glm`) Fitted Dixon-Coles model.
#' @returns (`double(1)`) Fallback expected goals estimate.
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
