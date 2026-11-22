#' Evaluate a Dixon-Coles weighting configuration
#'
#' @description Fits the Dixon-Coles model repeatedly over a held-out test
#'   period, varying `xi`, `upsilon`, and `nu`, and returns the log-loss on
#'   the test period.  Because the return value is a scalar (lower is better),
#'   the function can be passed directly to [stats::optim()] or a similar
#'   optimiser to find the best weighting parameters.
#'
#'   **Usage with `optim()`** (minimize log-loss):
#'   ```r
#'   # NHL
#'   optim(
#'     par = c(DC_XI_NHL, DC_UPSILON_NHL, DC_NU_NHL),
#'     fn  = function(p) .tune_dc_weight(xi = p[1], upsilon = p[2], nu = p[3]),
#'     method = "L-BFGS-B",
#'     lower  = c(1e-4, 30,  0),
#'     upper  = c(0.05, 730, 5)
#'   )
#'
#'   # PWHL
#'   optim(
#'     par = c(DC_XI_PWHL, DC_UPSILON_PWHL, DC_NU_PWHL),
#'     fn  = function(p) .tune_dc_weight(p[1], p[2], p[3], league = "PWHL"),
#'     method = "L-BFGS-B",
#'     lower  = c(1e-4, 30,  0),
#'     upper  = c(0.05, 730, 5),
#'     control = list(parscale = c(0.01, 10000, 10))
#'   )
#'   ```
#'
#' @param xi (`double(1)`) Time-decay slope for within-season weighting.
#'   Defaults to the league-appropriate constant ([DC_XI_NHL] / [DC_XI_PWHL]).
#' @param upsilon (`double(1)`) Midpoint (days) for the within-season logistic
#'   curve.  Defaults to the league-appropriate constant.
#' @param nu (`double(1)`) Cross-season discounting exponent.  `0` disables
#'   cross-season discounting.  Defaults to the league-appropriate constant
#'   ([DC_NU_NHL] / [DC_NU_PWHL]).
#' @param league (`character(1)`) `"NHL"` (default) or `"PWHL"`.  Determines
#'   which scores dataset and default parameter values to use.
#'
#' @returns (`double(1)`) Log-loss on the held-out test period (lower is
#'   better).
#' @keywords internal
.tune_dc_weight <- function(
  xi = NULL,
  upsilon = NULL,
  nu = NULL,
  league = "NHL"
) {
  league <- match.arg(league, c("NHL", "PWHL"))

  # League-specific defaults and data
  if (league == "PWHL") {
    xi <- if (is.null(xi)) DC_XI_PWHL else xi
    upsilon <- if (is.null(upsilon)) DC_UPSILON_PWHL else upsilon
    nu <- if (is.null(nu)) DC_NU_PWHL else nu
    all_scores <- HockeyModel::pwhlScores
    # Validate: need at least two PWHL seasons to have a meaningful hold-out
    if (nrow(all_scores) == 0) {
      cli::cli_abort(
        "No PWHL scores available. Run {.fn updatePWHLScoresAPI} first."
      )
    }
    all_scores <- .pwhl_add_result(all_scores)
    # Use the most recent PWHL season as the test period
    season_starts <- .derive_season_starts(all_scores$Date)
    if (length(season_starts) < 2) {
      cli::cli_abort(
        "At least two PWHL seasons of data are required to tune weights."
      )
    }
    test_start <- season_starts[length(season_starts)]
    # Fixed rho/beta/eta/k — only m changes during the sweep
    fixed_rho <- if (is.null(HockeyModel::pwhl_rho)) {
      -0.25
    } else {
      HockeyModel::pwhl_rho
    }
    fixed_beta <- if (is.null(HockeyModel::pwhl_beta)) {
      2
    } else {
      HockeyModel::pwhl_beta
    }
    fixed_eta <- if (is.null(HockeyModel::pwhl_eta)) {
      3
    } else {
      HockeyModel::pwhl_eta
    }
    fixed_k <- if (is.null(HockeyModel::pwhl_k)) 5 else HockeyModel::pwhl_k
  } else {
    xi <- if (is.null(xi)) DC_XI_NHL else xi
    upsilon <- if (is.null(upsilon)) DC_UPSILON_NHL else upsilon
    nu <- if (is.null(nu)) DC_NU_NHL else nu
    all_scores <- HockeyModel::scores
    all_scores <- unique(all_scores[all_scores$Date > as.Date("2010-08-01"), ])
    test_start <- as.Date("2022-10-01")
    # Fixed rho/beta/eta/k — only m changes during the sweep
    fixed_rho <- HockeyModel::rho
    fixed_beta <- HockeyModel::beta
    fixed_eta <- HockeyModel::eta
    fixed_k <- HockeyModel::k
  }

  cli::cli_alert_info(
    "Evaluating {league} weights: xi={xi}, upsilon={upsilon}, nu={nu}"
  )

  truth <- all_scores[all_scores$Date >= test_start, ]
  test_dates <- sort(unique(truth$Date))

  # Inner function: produce HomeWin probability for all games on date d
  .get_game_odds <- function(d, all_scores, xi, upsilon, nu) {
    current_m <- getM(
      scores = all_scores[all_scores$Date < d, ],
      currentDate = d,
      xi = xi,
      upsilon = upsilon,
      nu = nu
    )
    params <- list(
      m = current_m,
      rho = fixed_rho,
      beta = fixed_beta,
      eta = fixed_eta,
      k = fixed_k
    )
    sch <- truth[truth$Date == d, ]
    sch$HomeWin <- sch$AwayWin <- NA_real_
    for (g in sch$GameID) {
      odds <- DCPredict(
        sch[sch$GameID == g, ]$HomeTeam,
        sch[sch$GameID == g, ]$AwayTeam,
        params = params,
        draws = FALSE
      )
      sch[sch$GameID == g, ]$HomeWin <- odds[[1]]
      sch[sch$GameID == g, ]$AwayWin <- odds[[2]]
    }
    gc()
    return(sch[, c("GameID", "HomeWin", "AwayWin")])
  }

  cl <- parallel::makeCluster(.parseCores(4))
  on.exit(parallel::stopCluster(cl), add = TRUE)
  doSNOW::registerDoSNOW(cl)
  `%dopar%` <- foreach::`%dopar%` # This hack passes R CMD CHK
  `%do%` <- foreach::`%do%` # This hack passes R CMD CHK
  i <- 0 # This hack passes R CMD CHK
  r <- foreach::foreach(
    i = seq_along(test_dates),
    .combine = "rbind"
  ) %dopar%
    (.get_game_odds(test_dates[i], all_scores, xi, upsilon, nu))

  schedule <- dplyr::left_join(
    truth[, c("GameID", "Date", "HomeTeam", "AwayTeam", "Result")],
    r,
    by = "GameID"
  )
  ll <- logLoss(schedule$HomeWin, schedule$Result > 0.5)
  acc <- accuracy(schedule$HomeWin > 0.5, actual = schedule$Result > 0.5)
  cli::cli_alert_info(
    "Accuracy = {round(acc, 4)}, LogLoss = {round(ll, 4)}."
  )
  return(ll)
}

# Determining performance with xi = 0.00426
# Accuracy = 0.6007, LogLoss = 0.6669.

#' One refit walk over the held-out b2b test period (#64)
#'
#' @description Refits `m` for every date in the held-out NHL test period (same
#'   scheme as [.tune_dc_weight()]: data from 2010-08-01, test start 2022-10-01),
#'   and stores each game's *base* expected goals (`lambda`/`mu`, before any b2b
#'   scaling) together with its home/away b2b flags and `Result`.  The expensive
#'   refits happen once here; the cheap per-game b2b scaling in [.b2b_eval()] can
#'   then be applied for many candidate magnitudes without re-refitting.
#'
#' @returns (`data.frame`) One row per test-period game with columns `base_lambda`,
#'   `base_mu`, `home_b2b`, `away_b2b`, and `Result`.
#' @keywords internal
.b2b_base_walk <- function() {
  all_scores <- HockeyModel::scores
  all_scores <- unique(all_scores[all_scores$Date > as.Date("2010-08-01"), ])
  test_start <- as.Date("2022-10-01")

  truth <- all_scores[all_scores$Date >= test_start, ]
  test_dates <- sort(unique(truth$Date))

  # Compute b2b flags serially (cheap) so the parallel workers only need to call
  # exported functions (getM / predict); internal helpers aren't visible there.
  flag_table <- lapply(
    test_dates,
    function(d) {
      sch <- truth[truth$Date == d, ]
      prior <- all_scores[
        all_scores$Date < d,
        c("Date", "HomeTeam", "AwayTeam")
      ]
      fl <- .b2b_flags(games = sch, history = prior)
      c(fl$home_b2b, fl$away_b2b)
    }
  )
  names(flag_table) <- format(test_dates)

  # Base lambda/mu for every game on date d (no b2b scaling yet). Only exported
  # functions are called here so it runs inside parallel workers; unseen teams
  # fall back to the historical league-average goal rate.
  .get_base <- function(d, flags) {
    current_m <- getM(
      scores = all_scores[all_scores$Date < d, ],
      currentDate = d
    )
    sch <- truth[truth$Date == d, ]

    lam <- vapply(
      seq_len(nrow(sch)),
      function(i) {
        x <- tryCatch(
          as.numeric(stats::predict(
            current_m,
            data.frame(
              Home = 1,
              Team = sch$HomeTeam[i],
              Opponent = sch$AwayTeam[i]
            ),
            type = "response"
          )),
          error = function(e) NA_real_
        )
        if (is.na(x)) {
          3.32 # historical home goals fallback for unseen teams
        } else {
          x
        }
      },
      numeric(1)
    )
    mu <- vapply(
      seq_len(nrow(sch)),
      function(i) {
        x <- tryCatch(
          as.numeric(stats::predict(
            current_m,
            data.frame(
              Home = 0,
              Team = sch$AwayTeam[i],
              Opponent = sch$HomeTeam[i]
            ),
            type = "response"
          )),
          error = function(e) NA_real_
        )
        if (is.na(x)) {
          2.83 # historical away goals fallback for unseen teams
        } else {
          x
        }
      },
      numeric(1)
    )

    data.frame(
      base_lambda = lam,
      base_mu = mu,
      home_b2b = flags[seq_len(nrow(sch))],
      away_b2b = flags[(nrow(sch) + 1):(2 * nrow(sch))],
      Result = sch$Result
    )
  }

  cl <- parallel::makeCluster(.parseCores(4))
  on.exit(parallel::stopCluster(cl), add = TRUE)
  doSNOW::registerDoSNOW(cl)
  `%dopar%` <- foreach::`%dopar%` # This hack passes R CMD CHK
  i <- 0 # This hack passes R CMD CHK
  base <- foreach::foreach(
    i = seq_along(test_dates),
    .combine = "rbind"
  ) %dopar%
    (.get_base(test_dates[i], flag_table[[format(test_dates[i])]]))

  return(base)
}

#' Evaluate b2b log-loss for a magnitude pair from a precomputed base frame (#64)
#'
#' @description Applies the exp(+/- eps) scaling to each game's stored base
#'   `lambda`/`mu`, rebuilds the Dixon-Coles score matrix, and returns the
#'   home-win log-loss on the test period.  This is cheap (no refits), so it can
#'   be called for many candidate magnitudes against a single [.b2b_base_walk()]
#'   result.  The `draws = FALSE` redistribution matches [DCPredict()] exactly.
#'
#' @param base (`data.frame`) Output of [.b2b_base_walk()].
#' @param eps_home (`double(1)`) Home b2b magnitude.
#' @param eps_away (`double(1)`) Away b2b magnitude.
#'
#' @returns (`double(1)`) Log-loss on the held-out test period (lower is better).
#' @keywords internal
.b2b_eval <- function(base, eps_home = B2B_HOME_NHL, eps_away = B2B_AWAY_NHL) {
  fixed_params <- list(
    rho = HockeyModel::rho,
    beta = HockeyModel::beta,
    eta = HockeyModel::eta,
    k = HockeyModel::k
  )

  hw <- vapply(
    seq_len(nrow(base)),
    function(i) {
      b2b <- .apply_b2b(
        base$base_lambda[i],
        base$base_mu[i],
        home_b2b = base$home_b2b[i],
        away_b2b = base$away_b2b[i],
        eps_home = eps_home,
        eps_away = eps_away
      )
      pm <- .prob_matrix(
        lambda = b2b$lambda,
        mu = b2b$mu,
        params = fixed_params,
        maxgoal = 10
      )
      hw_raw <- sum(pm[lower.tri(pm)])
      dw_raw <- sum(diag(pm))
      aw_raw <- sum(pm[upper.tri(pm)])
      # draws = FALSE: redistribute the draw mass proportionally (matches DCPredict).
      pair <- hw_raw + aw_raw
      hw2 <- hw_raw + hw_raw / pair * dw_raw
      aw2 <- aw_raw + aw_raw / pair * dw_raw
      normalizeOdds(c(hw2, aw2))[1]
    },
    numeric(1)
  )

  ll <- logLoss(hw, base$Result > 0.5)
  acc <- accuracy(hw > 0.5, actual = base$Result > 0.5)
  cli::cli_alert_info(
    "eps_home={round(eps_home, 4)}, eps_away={round(eps_away, 4)}: Accuracy = {round(acc, 4)}, LogLoss = {round(ll, 4)}."
  )
  return(ll)
}

#' Tune the back-to-back (b2b) adjustment magnitudes (#64)
#'
#' @description Runs one refit walk over the held-out NHL test period and returns
#'   the log-loss for a given pair of b2b magnitudes.  Because it returns a scalar
#'   (lower is better) it can be passed to an optimiser; [.tune_b2b_grid()] runs a
#'   coarse grid followed by L-BFGS-B refinement and records the optimum into
#'   [B2B_HOME_NHL] / [B2B_AWAY_NHL].
#'
#' @param eps_home (`double(1)`) Log-scale b2b magnitude for a team playing at
#'   home.  Defaults to [B2B_HOME_NHL].
#' @param eps_away (`double(1)`) Log-scale b2b magnitude for a team on the road.
#'   Defaults to [B2B_AWAY_NHL].
#'
#' @returns (`double(1)`) Log-loss on the held-out test period (lower is better).
#' @keywords internal
.tune_b2b <- function(eps_home = B2B_HOME_NHL, eps_away = B2B_AWAY_NHL) {
  base <- .b2b_base_walk()
  return(.b2b_eval(base, eps_home = eps_home, eps_away = eps_away))
}

#' Coarse grid + L-BFGS-B search for the b2b magnitudes (#64)
#'
#' @description Runs a single refit walk ([.b2b_base_walk()]), then sweeps a coarse
#'   grid over `(eps_home, eps_away)` in `[0, 0.4]` (step `~0.1`) and refines around
#'   the best cell with L-BFGS-B — all cheap evaluations against that one walk.
#'   Returns the optimal magnitudes as a named list; record them into
#'   [B2B_HOME_NHL] / [B2B_AWAY_NHL].
#'
#' @returns (`list`) A named list with `eps_home` and `eps_away`.
#' @keywords internal
.tune_b2b_grid <- function() {
  base <- .b2b_base_walk() # one expensive walk, reused for every evaluation

  grid <- seq(0, 0.4, by = 0.1)
  results <- expand.grid(eps_home = grid, eps_away = grid)

  ll_vec <- vapply(
    seq_len(nrow(results)),
    function(i) {
      cli::cli_alert_info(
        "Grid point {i}/{nrow(results)}: eps_home={results$eps_home[i]}, eps_away={results$eps_away[i]}"
      )
      .b2b_eval(
        base,
        eps_home = results$eps_home[i],
        eps_away = results$eps_away[i]
      )
    },
    numeric(1)
  )

  best_i <- which.min(ll_vec)
  best_h <- results$eps_home[best_i]
  best_a <- results$eps_away[best_i]
  cli::cli_alert_info(
    "Best grid cell: eps_home={best_h}, eps_away={best_a} (LogLoss = {min(ll_vec)})"
  )

  opt <- stats::optim(
    par = c(best_h, best_a),
    fn = function(p) .b2b_eval(base, eps_home = p[1], eps_away = p[2]),
    method = "L-BFGS-B",
    lower = c(0, 0),
    upper = c(0.4, 0.4)
  )

  cli::cli_alert_info(
    "Optimized: eps_home={round(opt$par[1], 4)}, eps_away={round(opt$par[2], 4)} (LogLoss = {round(opt$value, 4)})"
  )
  return(list(eps_home = opt$par[1], eps_away = opt$par[2]))
}
