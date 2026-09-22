# Dixon-Coles workflow: parameter updating, daily/playoff predictions, and season simulation orchestration

#' Update Dixon Coles parameters
#'
#' @description This function updates the model parameters to best fit the provided data. The parameters for this model are as follows:
#' * [m] is the result of the main model fit and contains team attack and defense strengths, plus home ice advantage terms
#' * [rho] is the Dixon-Coles low scores adjustment term.
#' * [beta] is the Weibull distribution's 'shape' parameter. This is used with [eta] to produce a curve multiplied by the diagonal score possibility matrix to enhance the odds of tie games
#' * [eta] is the Weibull distribution's 'scale' parameter. See above for its importance
#' * [k] is the multiplication factor used with the Weibull distribution to enhance ties
#'
#' @param scores scores, if not then HockeyModel::scores is used
#' @param currentDate Current Date, usually today but useful to set a different date if back calculating results
#' @param xi (`double(1)`) Logistic slope for within-season time-decay
#'   weighting.  Defaults to [DC_XI_NHL].
#' @param upsilon (`double(1)`) Logistic midpoint (days) for within-season
#'   time-decay weighting.  Defaults to [DC_UPSILON_NHL].
#' @param nu (`double(1)`) Cross-season discounting exponent.  `0` (default
#'   [DC_NU_NHL]) disables cross-season discounting.  See [DCweights()] for
#'   details.
#' @param save_data Whether to save parameters to the package.
#'
#' @return a named list containing m, rho, beta, eta and k values for the model.
#'
#' @seealso [m], [rho], [beta], [eta], [k], [DC_XI_NHL], [DC_UPSILON_NHL],
#'   [DC_NU_NHL]
#'
#' @export
updateDC <- function(
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

#' DC Predictions Today
#'
#' @param today Generate predictions for this date. Defaults to today
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param schedule schedule to use, if not the built-in
#' @param expected_mean the mean lambda & mu, used only for regression
#' @param season_percent the percent complete of the season, used for regression
#' @param include_xG Whether to include team expected goals. default FALSE
#' @param draws whether to report draws in odds (AwayWin/HomeWin/Draw) or not (AwayWin/HomeWin). Default True
#'
#' @return a data frame of HomeTeam, AwayTeam, HomeWin, AwayWin, Draw, GameID; or NULL if no games today
todayDC <- function(
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
  params <- parse_dc_params(params)
  #games <- games_today(date = today)
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
  for (i in seq_len(nrow(preds))) {
    p <- DCPredict(
      preds$HomeTeam[[i]],
      preds$AwayTeam[[i]],
      params = params,
      expected_mean = expected_mean,
      season_percent = season_percent,
      draws = draws
    )
    if (draws) {
      preds$HomeWin[[i]] <- p[[1]]
      preds$AwayWin[[i]] <- p[[3]]
      preds$Draw[[i]] <- p[[2]]
    } else {
      preds$HomeWin[[i]] <- p[[1]]
      preds$AwayWin[[i]] <- p[[2]]
    }

    if (include_xG) {
      xg <- dcxG(
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

#' Playoff Odds DC
#'
#' @param home Series Home Ice Advantage Team Name
#' @param away Away (Opponent) Team Name
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param home_wins Number of wins for home ice advantage team thus far in series
#' @param away_wins Number of wins for away team thus far in series
#'
#' @return home ice advantage team odds to win series
#' @export
playoffDC <- function(home, away, params = NULL, home_wins = 0, away_wins = 0) {
  params <- parse_dc_params(params)
  # Odds of home ice advantage team win at home
  homeodds <- DCPredict(home = home, away = away, params = params)
  homeodds <- normalizeOdds(c(homeodds[1], homeodds[3]))[1]
  # Odds of home ice advantage team win away
  awayodds <- DCPredict(home = away, away = home, params = params)
  awayodds <- normalizeOdds(c(awayodds[3], awayodds[1]))[1]

  homewin <- playoffSeriesOdds(homeodds, awayodds, home_wins, away_wins)
  return(homewin)
}

#' DC remainder of season
#' @description Odds for each team to get to playoffs.
#'
#' @param nsims Number of simulations
#' @param cores The number of cores to use if using parallel processing, or 1 for single-core, NULL defaults to all cores or 1 if `parallel` package not installed.
#' @param scores the historical scores
#' @param schedule un-played future games
#' @param odds whether to return odds table or simulate season
#' @param regress whether to apply a regression to the mean for team strength on future predictions
#' @param mu_lambda whether to return team xG values. Can't be set true if odds is true
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return data frame of Team, playoff odds.
#' @export
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
  odds_table <- data.frame(
    HomeTeam = character(),
    AwayTeam = character(),
    HomeWin = numeric(),
    AwayWin = numeric(),
    Draw = numeric(),
    GameID = numeric(),
    stringsAsFactors = FALSE
  )

  cores <- parseCores(cores)

  params <- parse_dc_params(params = params)

  last_game_date <- as.Date(max(scores$Date))
  schedule <- add_postponed_to_schedule_end(schedule)
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

    preds <- todayDC(
      today = d,
      schedule = schedule,
      season_percent = season_percent,
      expected_mean = expected_mean,
      params = params
    )
    preds$Date <- d
    odds_table <- rbind(odds_table, preds)
  }

  #odds_table$Date <- schedule$Date
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
        lambda <- DCPredictErrorRecover(
          team = odds_table$HomeTeam[g],
          opponent = odds_table$AwayTeam[g],
          homeiceadv = TRUE
        )
      }
      if (!is.numeric(mu)) {
        mu <- DCPredictErrorRecover(
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

#' DC Predict Multiple Days
#'
#' @description For catching up on daily predictions
#'
#' @param start First day to predict. Default start of season
#' @param end Last day to predict. Default today
#' @param scores HockeyModel::scores
#' @param schedule HockeyModel::schedule
#' @param filedir Where to save prediction files.
#' @param nsims number of simulations to run
#' @param cores number of cores in parallel to simulate on
#' @param likelihood_graphic Whether to call for creation of likelihood graphic. Default True
#'
#' @return true, if successful
#' @export
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
  cores <- parseCores(cores)

  if (!is.Date(start)) {
    cli::cli_abort("{.arg start} must be a Date or date-like value.")
  }
  if (!is.Date(end)) {
    cli::cli_abort("{.arg end} must be a Date or date-like value.")
  }
  predict_dates <- seq(from = as.Date(end), to = as.Date(start), by = -1) # do it backwards to get the most recent date done first

  schedule$Date <- as.Date(schedule$Date)
  schedule <- add_postponed_to_schedule_end(schedule)

  message("Running predictions for ", length(predict_dates), " day(s).")
  for (day in predict_dates) {
    d <- as.Date(day, origin = "1970-01-01")
    message("Predictions as of: ", d)
    score <- scores[scores$Date < day, ]
    score <- score[score$Date > as.Date("2008-08-01"), ]
    sched <- schedule[schedule$Date >= day, ]
    params <- updateDC(scores = score, currentDate = d)
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

#' Get Season Metrics
#'
#' @description Calculates the Log Loss and Accuracy of the model by re-estimating m and rho daily and creating game odds
#'
#' @param schedule HockeyModel::schedule used to help with calculations
#' @param scores HockeyModel::scores used to compare to predictions
#'
#' @return a list of log loss and accuracy for the season
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

#' Predict Multiple Days's W/L/D results
#'
#' @description Calculate what each days' game predictions were by recalculating rho and m using games only up to the previous day. This is kinda slow, so a full season might take an hour or more depending on your hardware. Split out of the SeasonMetrics code to help with requests by @JB4991 on twitter.
#'
#' @param startDate First day of predicted results
#' @param endDate Last day of predicted results
#' @param schedule HockeyModel::schedule
#' @param scores HockeyModel::scores
#'
#' @return a data frame like HockeyModel::schedule with HomeWin, AwayWin and Draw odds
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

    p <- todayDC(today = d, params = params)

    sched[sched$Date == d, "HomeWin"] <- p$HomeWin
    sched[sched$Date == d, "AwayWin"] <- p$AwayWin
    sched[sched$Date == d, "Draw"] <- p$Draw
  }

  return(sched)
}
