# Dixon-Coles workflow: parameter updating, daily/playoff predictions, and season simulation orchestration

#' Update NHL Dixon-Coles parameters
#'
#' @description Fits the Dixon-Coles model to NHL scores data and returns (and
#'   optionally saves) the five model parameters.
#'
#' @param scores (`data.frame`) NHL game scores. Defaults to
#'   [HockeyModel::scores].
#' @param currentDate (`Date`) Reference date for time-weighting. Defaults to
#'   today.
#' @param xi (`double(1)`) Logistic slope for within-season time-decay
#'   weighting.  Defaults to [DC_XI_NHL].
#' @param upsilon (`double(1)`) Logistic midpoint (days) for within-season
#'   time-decay weighting.  Defaults to [DC_UPSILON_NHL].
#' @param nu (`double(1)`) Cross-season discounting exponent.  `0` (default
#'   [DC_NU_NHL]) disables cross-season discounting.  See [.DCweights()] for
#'   details.
#' @param save_data (`logical(1)`) If `TRUE` and `usethis` is installed, writes
#'   the parameters as package data objects.
#'
#' @returns A named list with elements `m`, `rho`, `beta`, `eta`, and `k`.
#' @keywords internal
.update_dc_nhl <- function(
  scores = HockeyModel::scores,
  currentDate = Sys.Date(),
  xi = DC_XI_NHL,
  upsilon = DC_UPSILON_NHL,
  nu = DC_NU_NHL,
  save_data = TRUE
) {
  message("Calculating new model parameters...")
  if (!is.Date(currentDate)) {
    cli::cli_abort("{.arg currentDate} must be a Date or date-like value.")
  }
  if (currentDate != Sys.Date()) {
    currentDate <- as.Date(currentDate)
    scores <- scores[scores$Date < currentDate, ]
    save_data <- FALSE
  }
  m <- getM(
    scores = scores,
    currentDate = currentDate,
    xi = xi,
    upsilon = upsilon,
    nu = nu
  )
  message("Solving for low scoring games...")
  rho <- getRho(m = m, scores = scores)
  message("Enhancing Tie Games")
  params <- getWeibullParams(m = m, rho = rho, scores = scores)
  beta <- params$beta
  eta <- params$eta
  k <- params$k
  if (save_data && requireNamespace("usethis", quietly = TRUE)) {
    suppressMessages(usethis::use_data(m, rho, beta, eta, k, overwrite = TRUE))
  }
  return(list("m" = m, "rho" = rho, "beta" = beta, "eta" = eta, "k" = k))
}

#' Dixon-Coles predictions for a single day
#'
#' @description Predicts home/away/draw win probabilities for every game in
#'   the [schedule] played on the given date using [DCPredict()], which builds
#'   the bivariate Poisson score matrix with the Dixon-Coles low-score
#'   correction (`rho`) and the Weibull tie enhancement (`beta`, `eta`, `k`).
#'   Returns `NULL` when there are no games on the given day.
#'
#' @param today (`Date`) The date to predict.  Defaults to today.
#' @param params (`list` or `NULL`) Named list with `m`, `rho`, `beta`, `eta`,
#'   and `k` (as returned by [updateModel()] / [.update_dc_nhl()]).  Defaults
#'   to the frozen package data.
#' @param schedule (`data.frame`) The schedule to source games from.  Defaults
#'   to [HockeyModel::schedule].
#' @param expected_mean (`double(1)` or `NULL`) League-average goals used only
#'   for the regression-to-mean shrinkage.  Defaults to `NULL` (no regression).
#' @param season_percent (`double(1)` or `NULL`) Fraction of the season
#'   complete, used only for the regression-to-mean shrinkage.  Defaults to
#'   `NULL`.
#' @param include_xG (`logical(1)`) If `TRUE`, also returns `Home_xG` and
#'   `Away_xG` (expected goals) and drops `GameID`.  Defaults to `FALSE`.
#' @param draws (`logical(1)`) If `TRUE` (default), return `HomeWin`,
#'   `AwayWin`, and `Draw`; if `FALSE`, renormalise to a two-way `HomeWin` /
#'   `AwayWin`.
#'
#' @returns (`data.frame`) One row per game on the given date with `HomeTeam`,
#'   `AwayTeam`, `HomeWin`, `AwayWin`, `Draw`, and `GameID`; or `NULL` if no
#'   games are scheduled that day.
#' @keywords internal
.todayDC <- function(
  params = NULL,
  today = Sys.Date(),
  schedule = HockeyModel::schedule,
  expected_mean = NULL,
  season_percent = NULL,
  include_xG = FALSE,
  draws = TRUE
) {
  if (!is.Date(today)) {
    cli::cli_abort("{.arg today} must be a Date or date-like value.")
  }
  params <- .parse_dc_params(params)
  # games <- games_today(date = today)
  games <- schedule[schedule$Date == today, ]
  if (nrow(games) == 0) {
    return(NULL)
  }

  preds <- data.frame(
    HomeTeam = games$HomeTeam,
    AwayTeam = games$AwayTeam,
    HomeWin = 0,
    AwayWin = 0,
    Draw = 0,
    GameID = games$GameID,
    stringsAsFactors = FALSE
  )
  if (include_xG) {
    preds$Away_xG <- preds$Home_xG <- 0
  }

  # Compute all games' odds at once via the vectorized path.
  odds <- .dcPredictVectorized(
    home = preds$HomeTeam,
    away = preds$AwayTeam,
    params = params,
    expected_mean = expected_mean,
    season_percent = season_percent,
    draws = draws
  )
  if (draws) {
    preds$HomeWin <- odds[, "HomeWin"]
    preds$AwayWin <- odds[, "AwayWin"]
    preds$Draw <- odds[, "Draw"]
  } else {
    preds$HomeWin <- odds[, "HomeWin"]
    preds$AwayWin <- odds[, "AwayWin"]
  }

  if (include_xG) {
    for (i in seq_len(nrow(preds))) {
      xg <- .dcxG(
        home = preds$HomeTeam[[i]],
        away = preds$AwayTeam[[i]],
        params = params
      )
      preds$Home_xG[[i]] <- xg$home
      preds$Away_xG[[i]] <- xg$away
    }
  }

  if (include_xG) {
    preds$GameID <- NULL
  }

  return(preds)
}

#' Dixon-Coles odds to win a best-of-seven playoff series
#'
#' @description Combines the single-game Dixon-Coles win probability of the
#'   home team (with home-ice advantage) and the away team into a best-of-seven
#'   series win probability for the home team, given the current series score
#'   (`home_wins`, `away_wins`).  Single-game probabilities come from
#'   [DCPredict()] and are normalised to exclude the draw; the series
#'   probability is computed with [playoffSeriesOdds()].
#'
#' @param home (`character`) The team name with home-ice advantage for the
#'   series.
#' @param away (`character`) The opponent team name.
#' @param params (`list` or `NULL`) Named list with `m`, `rho`, `beta`, `eta`,
#'   and `k` (as returned by [updateModel()]).  Defaults to the frozen package
#'   data.
#' @param home_wins (`integer`) Games already won by the home team in the
#'   series.  Defaults to 0.
#' @param away_wins (`integer`) Games already won by the away team in the
#'   series.  Defaults to 0.
#'
#' @returns (`double(1)`) The probability that the home team wins the series
#'   from the current state.
#' @keywords internal
playoffDC <- function(home, away, params = NULL, home_wins = 0, away_wins = 0) {
  params <- .parse_dc_params(params)
  # Odds of home ice advantage team win at home
  homeodds <- DCPredict(home = home, away = away, params = params)
  homeodds <- normalizeOdds(c(homeodds[1], homeodds[3]))[1]
  # Odds of home ice advantage team win away
  awayodds <- DCPredict(home = away, away = home, params = params)
  awayodds <- normalizeOdds(c(awayodds[3], awayodds[1]))[1]

  homewin <- playoffSeriesOdds(homeodds, awayodds, home_wins, away_wins)
  return(homewin)
}

#' Dixon-Coles remainder-of-season projections
#'
#' @description Projects the rest of the NHL season using the Dixon-Coles
#'   model.  By default this runs a full season simulation
#'   ([simulateSeasonParallel()]) and returns each team's playoff and Cup
#'   odds.  Alternatively, it can return the per-game win/draw odds for every
#'   remaining scheduled game (`odds = TRUE`), or each team's expected goals
#'   (`mu_lambda = TRUE`; mutually exclusive with `odds`).
#'
#' @param nsims (`integer`) Number of season simulations to run.  Defaults to
#'   `1e4`.
#' @param cores (`integer` or `NULL`) Number of cores for parallel
#'   simulation; `1` for single-core, or `NULL` to use all cores (falling back
#'   to 1 if the `parallel` package is unavailable).
#' @param params (`list` or `NULL`) Named list with `m`, `rho`, `beta`, `eta`,
#'   and `k` (as returned by [updateModel()] / [.update_dc_nhl()]).  Defaults
#'   to the frozen package data.
#' @param scores (`data.frame`) Historical scores.  Defaults to
#'   [HockeyModel::scores].
#' @param schedule (`data.frame`) Unplayed future games.  Defaults to
#'   [HockeyModel::schedule].
#' @param odds (`logical(1)`) If `TRUE`, return the per-game odds table
#'   instead of simulating.  Defaults to `FALSE`.
#' @param regress (`logical(1)`) If `TRUE` (default), apply
#'   regression-to-mean shrinkage of team-strength predictions as the season
#'   progresses.  Automatically disabled once the regular season has ended.
#' @param mu_lambda (`logical(1)`) If `TRUE`, return each team's expected
#'   goals (mu/lambda) instead of simulating.  Cannot be used with
#'   `odds = TRUE`.  Defaults to `FALSE`.
#'
#' @returns Depending on the flags: the season-simulation summary (playoff and
#'   Cup odds per team), a per-game odds data frame (`odds = TRUE`), or a
#'   data frame of team expected goals (`mu_lambda = TRUE`).
#' @keywords internal
remainderSeasonDC <- function(
  nsims = 1e4,
  cores = NULL,
  params = NULL,
  scores = HockeyModel::scores,
  schedule = HockeyModel::schedule,
  odds = FALSE,
  regress = TRUE,
  mu_lambda = FALSE
) {
  pred_list <- list()

  cores <- .parseCores(cores)

  params <- .parse_dc_params(params = params)

  last_game_date <- as.Date(max(scores$Date))
  schedule <- .add_postponed_to_schedule_end(schedule)
  schedule <- schedule[schedule$Date > last_game_date, ]
  schedule <- schedule |>
    dplyr::arrange(.data$Date, .data$GameID)

  # cant regress through playoffs, turn off if not regular season anymore
  if (regress) {
    if (
      nrow(schedule[
        schedule$Date >= Sys.Date() & schedule$GameType == "R",
      ]) ==
        0
    ) {
      regress <- FALSE
    }
  }

  if (regress) {
    season_end_date <- as.Date(max(schedule[schedule$GameType == "R", ]$Date))
    season_start_date <- as.Date(min(c(
      scores[scores$Date > as.Date(getSeasonStartDate()), "Date"],
      schedule[schedule$Date > as.Date(getSeasonStartDate()), "Date"]
    )))
    season_length <- as.integer(season_end_date) - as.integer(season_start_date)
    remaining_length <- as.integer(season_end_date) - as.integer(last_game_date)
    expected_mean <- 2.835184
  } else {
    expected_mean <- NULL
    season_percent <- NULL
  }

  for (day in unique(schedule$Date)) {
    d <- as.Date(day, origin = "1970-01-01")
    if (regress) {
      # Adjust regress to mean
      season_percent <- (remaining_length -
        as.integer(season_end_date - as.Date(d))) /
        season_length
    }

    preds <- .todayDC(
      today = d,
      schedule = schedule,
      season_percent = season_percent,
      expected_mean = expected_mean,
      params = params
    )
    preds$Date <- d
    pred_list <- c(pred_list, list(preds))
  }

  # odds_table$Date <- schedule$Date
  if (length(pred_list) == 0) {
    odds_table <- data.frame(
      HomeTeam = character(),
      AwayTeam = character(),
      HomeWin = numeric(),
      AwayWin = numeric(),
      Draw = numeric(),
      GameID = numeric(),
      Date = as.Date(character()),
      stringsAsFactors = FALSE
    )
  } else {
    odds_table <- dplyr::bind_rows(pred_list)
  }
  odds_table$GameID <- as.numeric(odds_table$GameID)

  if (odds) {
    return(odds_table)
  }

  if (mu_lambda) {
    odds_table$mu <- NA
    odds_table$lambda <- NA

    for (g in seq_len(nrow(odds_table))) {
      d <- as.Date(odds_table[g, "Date"], origin = "1970-01-01")
      # Expected goals home
      lambda <- try(
        stats::predict(
          HockeyModel::m,
          data.frame(
            Home = 1,
            Team = odds_table$HomeTeam[g],
            Opponent = odds_table$AwayTeam[g]
          ),
          type = "response"
        ),
        TRUE
      )

      # Expected goals away
      mu <- try(
        stats::predict(
          HockeyModel::m,
          data.frame(
            Home = 0,
            Team = odds_table$AwayTeam[g],
            Opponent = odds_table$HomeTeam[g]
          ),
          type = "response"
        ),
        TRUE
      )

      if (!is.numeric(lambda)) {
        lambda <- .DCPredictErrorRecover(
          team = odds_table$HomeTeam[g],
          opponent = odds_table$AwayTeam[g],
          homeiceadv = TRUE
        )
      }
      if (!is.numeric(mu)) {
        mu <- .DCPredictErrorRecover(
          team = odds_table$AwayTeam[g],
          opponent = odds_table$HomeTeam[g],
          homeiceadv = FALSE
        )
      }

      if (regress) {
        # Adjust regress to mean
        season_percent <- (remaining_length -
          as.integer(season_end_date - as.Date(d))) /
          season_length

        lambda <- lambda *
          (1 - 1 / 3 * season_percent) +
          expected_mean * (1 / 3 * season_percent)
        mu <- mu *
          (1 - 1 / 3 * season_percent) +
          expected_mean * (1 / 3 * season_percent)
      }
      odds_table[g, "lambda"] <- lambda
      odds_table[g, "mu"] <- mu
    }
    odds_table$HomeWin <- odds_table$AwayWin <- odds_table$Draw <- NULL
    return(odds_table)
  }

  summary_results <- simulateSeasonParallel(
    nsims = nsims,
    cores = cores,
    scores = scores,
    schedule = schedule
  )

  return(summary_results)
}

#' Dixon-Coles predictions for a range of days (catch-up)
#'
#' @description Backfills Dixon-Coles season projections for every date in
#'   `start` through `end`.  For each date it refits the full Dixon-Coles
#'   parameter set on all scores up to that day ([.update_dc_nhl()]) and then
#'   simulates the season ([loopless_sim()]), saving each day's results to
#'   the prediction directory.  Intended for catching up after a gap in daily
#'   predictions.
#'
#' @param start (`Date`) First day to predict.  Defaults to the start of the
#'   season.
#' @param end (`Date`) Last day to predict.  Defaults to today.
#' @param scores (`data.frame`) Historical scores.  Defaults to
#'   [HockeyModel::scores].
#' @param schedule (`data.frame`) The schedule.  Defaults to
#'   [HockeyModel::schedule].
#' @param filedir (`character`) Directory where the daily prediction files are
#'   saved.  Defaults to the `HockeyModel.prediction.path` option.
#' @param nsims (`integer`) Number of season simulations per day.  Defaults to
#'   `1e5`.
#' @param cores (`integer` or `NULL`) Number of cores for parallel
#'   simulation; `1` for single-core, or `NULL` to use all cores.
#' @param likelihood_graphic (`logical(1)`) If `TRUE` (default), also produce
#'   the point-likelihood graphic for today's predictions.
#'
#' @returns (`logical(1)`) `TRUE` on completion (individual day failures are
#'   reported via `message()`).
#' @keywords internal
dcPredictMultipleDays <- function(
  start = as.Date(getSeasonStartDate()),
  end = Sys.Date(),
  scores = HockeyModel::scores,
  schedule = HockeyModel::schedule,
  filedir = getOption("HockeyModel.prediction.path"),
  nsims = 1e5,
  cores = NULL,
  likelihood_graphic = TRUE
) {
  if (!dir.exists(filedir)) {
    dir.create(filedir, recursive = TRUE)
  }
  cores <- .parseCores(cores)

  if (!is.Date(start)) {
    cli::cli_abort("{.arg start} must be a Date or date-like value.")
  }
  if (!is.Date(end)) {
    cli::cli_abort("{.arg end} must be a Date or date-like value.")
  }
  predict_dates <- seq(from = as.Date(end), to = as.Date(start), by = -1) # do it backwards to get the most recent date done first

  schedule$Date <- as.Date(schedule$Date)
  schedule <- .add_postponed_to_schedule_end(schedule)

  message("Running predictions for ", length(predict_dates), " day(s).")
  for (day in predict_dates) {
    d <- as.Date(day, origin = "1970-01-01")
    message("Predictions as of: ", d)
    score <- scores[scores$Date < day, ]
    score <- score[score$Date > as.Date("2008-08-01"), ]
    sched <- schedule[schedule$Date >= day, ]
    params <- .update_dc_nhl(scores = score, currentDate = d)
    preds <- NULL

    # preds <- loopless_sim(nsims = nsims, cores = cores, scores = score, schedule = sched, params = params, likelihood_graphic=likelihood_graphic)
    preds <- tryCatch(
      expr = {
        message("Predicting with Loopless Sim")
        loopless_sim(
          nsims = nsims,
          cores = cores,
          scores = score,
          schedule = sched,
          params = params,
          likelihood_graphic = likelihood_graphic
        )
      },
      error = function(error) {
        message("An error occurred:")
        message(error)
        return(NULL)
      }
    )

    if (!is.null(preds) && "summary_results" %in% names(preds)) {
      message("Saving Prediction file...")
      saveRDS(
        preds$summary_results,
        file = file.path(filedir, paste0(d, "-predictions.RDS"))
      )
      if (d == Sys.Date()) {
        plot_point_likelihood(preds = preds$raw_results)
      }
    } else {
      message("An error occurred, retrying ", d, ".")
      preds <- NULL
      preds <- tryCatch(
        expr = {
          message("Predicting with Old Version Sim")
          remainderSeasonDC(
            nsims = nsims,
            scores = score,
            schedule = sched,
            regress = TRUE
          )
        },
        error = function(error) {
          message("An error occurred:")
          message(error)
          return(NULL)
        }
      )
      if (!is.null(preds) && "summary_results" %in% names(preds)) {
        message("Saving Prediction file...")
        saveRDS(
          preds$summary_results,
          file = file.path(filedir, paste0(d, "-predictions.RDS"))
        )
      } else {
        message("An Error Occurred. Continuing to next day...")
      }
    }
    gc(verbose = FALSE)
  }

  return(TRUE)
}

#' Get Dixon-Coles season metrics (log loss and accuracy)
#'
#' @description Computes the season-long log loss and top-pick accuracy of the
#'   Dixon-Coles model.  For each day, it re-estimates the full parameter set
#'   (`m`, `rho`, `beta`, `eta`, `k`) using only games played before that day
#'   (via [predictMultipleDaysResultsDC()]), produces home/away/draw odds, and
#'   compares them against the actual results.  See [logLoss()] and
#'   [accuracy()] for the metric definitions.
#'
#' @param schedule (`data.frame`) The schedule used for the calculations.
#'   Defaults to [HockeyModel::schedule].
#' @param scores (`data.frame`) Historical scores used to compare predictions
#'   against.  Defaults to [HockeyModel::scores].
#'
#' @returns (`list`) A named list with `LogLoss` and `Accuracy` for the season.
#' @export
getSeasonMetricsDC <- function(
  schedule = HockeyModel::schedule,
  scores = HockeyModel::scores
) {
  sched <- schedule
  sched$Home.WLD <- sched$Away.WLD <- sched$Draw.WLD <- sched$Home.WL <- sched$Away.WL <- sched$Result <- NA
  season_sofar <- scores[
    scores$Date > as.Date(getSeasonStartDate()),
    c("GameID", "Result")
  ]

  sched <- predictMultipleDaysResultsDC(
    startDate = getSeasonStartDate(),
    endDate = Sys.Date()
  )
  sched <- dplyr::left_join(sched, season_sofar, by = "GameID", )

  sched$Home.WL <- (sched$HomeWin / (sched$HomeWin + sched$AwayWin)) *
    sched$Draw +
    sched$HomeWin
  sched$Away.WL <- (sched$AwayWin / (sched$HomeWin + sched$AwayWin)) *
    sched$Draw +
    sched$AwayWin

  sched <- sched[stats::complete.cases(sched), ]

  logloss <- logLoss(predicted = sched$Home.WL, actual = sched$Result)
  accuracy <- accuracy(
    predicted = sched$Home.WL > 0.5,
    actual = sched$Result > 0.5
  )

  return(list("LogLoss" = logloss, "Accuracy" = accuracy))
}

#' Predict Dixon-Coles W/L/D results for a range of days
#'
#' @description Recalculates the full Dixon-Coles parameter set ([m], [rho],
#'   [beta], [eta], [k]) for each day using only games played before that day,
#'   and returns the home/away/draw odds it would have produced.  This is a
#'   backtest-style "what the model predicted at the time" view.  It is
#'   relatively slow: a full season can take an hour or more depending on
#'   hardware.
#'
#' @param startDate (`Date`) First day of predicted results.
#' @param endDate (`Date`) Last day of predicted results.
#' @param schedule (`data.frame`) The schedule.  Defaults to
#'   [HockeyModel::schedule].
#' @param scores (`data.frame`) Historical scores.  Defaults to
#'   [HockeyModel::scores].
#'
#' @returns (`data.frame`) A data frame like [HockeyModel::schedule] with
#'   `HomeWin`, `AwayWin`, and `Draw` odds added for the date range.
#' @export
predictMultipleDaysResultsDC <- function(
  startDate,
  endDate,
  schedule = HockeyModel::schedule,
  scores = HockeyModel::scores
) {
  if (!is.Date(startDate)) {
    cli::cli_abort("{.arg startDate} must be a Date or date-like value.")
  }
  if (!is.Date(endDate)) {
    cli::cli_abort("{.arg endDate} must be a Date or date-like value.")
  }

  sched <- schedule[
    schedule$Date >= as.Date(startDate) & schedule$Date <= as.Date(endDate),
  ]

  for (day in unique(sched$Date)) {
    d <- as.Date(day, origin = "1970-01-01")
    message("Results as of: ", d)
    score <- scores[scores$Date < day, ]
    score <- score[score$Date > (as.Date(startDate) - 4000), ] # only feed in ~ 11 years data to calculate m & rho
    params <- list()
    params$m <- getM(scores = score, currentDate = d)
    params$rho <- getRho(m = params$m, scores = score)
    w.day <- getWeibullParams(m = params$m, rho = params$rho, scores = score)
    params$beta <- w.day$beta
    params$eta <- w.day$eta
    params$k <- w.day$k

    p <- .todayDC(today = d, params = params)

    sched[sched$Date == d, "HomeWin"] <- p$HomeWin
    sched[sched$Date == d, "AwayWin"] <- p$AwayWin
    sched[sched$Date == d, "Draw"] <- p$Draw
  }

  return(sched)
}
