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
#'     fn  = function(p) tune_dc_weight(xi = p[1], upsilon = p[2], nu = p[3]),
#'     method = "L-BFGS-B",
#'     lower  = c(1e-4, 30,  0),
#'     upper  = c(0.05, 730, 5)
#'   )
#'
#'   # PWHL
#'   optim(
#'     par = c(DC_XI_PWHL, DC_UPSILON_PWHL, DC_NU_PWHL),
#'     fn  = function(p) tune_dc_weight(p[1], p[2], p[3], league = "PWHL"),
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
tune_dc_weight <- function(
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
    all_scores <- pwhl_add_result(all_scores)
    # Use the most recent PWHL season as the test period
    season_starts <- derive_season_starts(all_scores$Date)
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
  get_game_odds <- function(d, all_scores, xi, upsilon, nu) {
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

  cl <- parallel::makeCluster(parseCores(4))
  on.exit(parallel::stopCluster(cl), add = TRUE)
  doSNOW::registerDoSNOW(cl)
  `%dopar%` <- foreach::`%dopar%` # This hack passes R CMD CHK
  `%do%` <- foreach::`%do%` # This hack passes R CMD CHK
  i <- 0 # This hack passes R CMD CHK
  r <- foreach::foreach(
    i = seq_along(test_dates),
    .combine = "rbind"
  ) %dopar%
    (get_game_odds(test_dates[i], all_scores, xi, upsilon, nu))

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
