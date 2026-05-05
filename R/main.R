# Hosts main functions
# Calls to data and prediction calculations, produce figures & tables

#' Resolve requested front-end leagues
#'
#' @param league (`character(1)` or `NULL`) Requested league selector. `NULL`,
#'   `NA`, and `"both"` expand to both leagues.
#' @returns (`character`) Normalised league names.
#' @keywords internal
.resolve_frontend_leagues <- function(league = NULL) {
  if (is.null(league) || (length(league) == 1 && is.na(league))) {
    return(c("NHL", "PWHL"))
  }

  if (!is.character(league) || length(league) != 1) {
    cli::cli_abort(
      "{.arg league} must be NULL, NA, {.val both}, {.val nhl}, or {.val pwhl}."
    )
  }

  league <- toupper(league)
  if (league == "BOTH") {
    return(c("NHL", "PWHL"))
  }

  if (league %in% c("NHL", "PWHL")) {
    return(league)
  }

  cli::cli_abort(
    "{.arg league} must be NULL, NA, {.val both}, {.val nhl}, or {.val pwhl}."
  )
}


#' Simplify multi-league front-end results
#'
#' @param result (`list`) Per-league result list.
#' @param leagues (`character`) Normalised league names from
#'   `.resolve_frontend_leagues()`.
#' @returns Either a single-league result or the original named list.
#' @keywords internal
.simplify_frontend_result <- function(result, leagues) {
  if (length(leagues) == 1) {
    return(result[[tolower(leagues)]])
  }

  result
}


#' Select a league-specific front-end value
#'
#' @param x (`any`) Candidate input value.
#' @param league (`character(1)`) Normalised league name.
#' @param use_default (`logical(1)`) Whether the caller omitted the argument and
#'   should therefore use the league-specific default.
#' @param default (`any`) Fallback value used when `x` is `NULL`.
#' @returns A league-specific value.
#' @keywords internal
.frontend_value_for_league <- function(
  x,
  league,
  use_default = FALSE,
  default = NULL
) {
  if (isTRUE(use_default) || is.null(x)) {
    return(default)
  }

  if (is.list(x) && !is.data.frame(x) && !is.null(names(x))) {
    league_name <- tolower(league)
    if (league_name %in% names(x)) {
      return(x[[league_name]])
    }

    upper_names <- toupper(names(x))
    if (league %in% upper_names) {
      return(x[[which(upper_names == league)[1]]])
    }
  }

  x
}


#' Compute front-end prediction directories
#'
#' @param data_dir (`character(1)`) Root prediction directory.
#' @returns (`list`) Named NHL and PWHL prediction directories.
#' @keywords internal
.frontend_prediction_dirs <- function(
  data_dir = getOption("HockeyModel.prediction.path", "./prediction_results")
) {
  list(
    nhl = data_dir,
    pwhl = file.path(data_dir, "pwhl")
  )
}


#' Default NHL graphics directory
#'
#' @returns (`character(1)`) NHL graphics directory derived from package
#'   options.
#' @keywords internal
.default_nhl_graphics_dir <- function() {
  getOption("HockeyModel.graphics.path", "./prediction_results/graphics")
}


#' Default PWHL graphics directory
#'
#' @returns (`character(1)`) PWHL graphics directory derived from package
#'   options.
#' @keywords internal
.default_pwhl_graphics_dir <- function() {
  file.path(
    getOption("HockeyModel.prediction.path", "./prediction_results"),
    "pwhl_graphics"
  )
}


#' Convert PWHL simulations to saved prediction snapshots
#'
#' @param sim_results (`list`) Output from [pwhl_loopless_sim()].
#' @returns A [tibble::tibble()] with `Team`, `Playoffs`, `meanPoints`, and
#'   `Presidents` columns.
#' @keywords internal
.pwhl_prediction_summary <- function(sim_results) {
  presidents <- sim_results$raw_results |>
    dplyr::group_by(.data$Team) |>
    dplyr::summarise(
      Presidents = mean(.data$Rank == 1, na.rm = TRUE),
      .groups = "drop"
    )

  sim_results$summary_results |>
    dplyr::select(
      "Team",
      Playoffs = "Make_Playoffs",
      "meanPoints"
    ) |>
    dplyr::left_join(presidents, by = "Team") |>
    tibble::as_tibble()
}


#' Update the NHL front-end model payload
#'
#' @param save_data (`logical(1)`) Whether to persist refreshed package data.
#' @returns (`list`) NHL `scores`, `schedule`, and `params`.
#' @keywords internal
.update_model_nhl <- function(save_data = TRUE) {
  cli::cli_inform("Updating Schedule")
  schedule <- updateScheduleAPI(save_data = save_data)
  cli::cli_inform("Updating Scores")
  scores <- updateScoresAPI(schedule = schedule, save_data = save_data)
  cli::cli_inform("Refitting Model Parameters")
  params <- updateDC(scores = scores, save_data = save_data)
  list(
    scores = scores,
    schedule = schedule,
    params = params
  )
}

#' Update Model
#' @description Updates the requested league model data. With `league = NULL`,
#'   `NA`, or `"both"`, both NHL and PWHL models are updated in one call.
#'
#' @param save_data whether to save data to the package file
#' @param league which league front-end to run: `NULL`, `NA`, or `"both"` runs
#'   both leagues; `"nhl"` and `"pwhl"` run one league only
#'
#' @return For a single league, a list of scores, schedule, and params. For
#'   both leagues, a named list with `nhl` and `pwhl` entries of that same form.
#'
#' @export
updateModel <- function(save_data = TRUE, league = NULL) {
  leagues <- .resolve_frontend_leagues(league)
  result <- list()

  if ("NHL" %in% leagues) {
    result$nhl <- .update_model_nhl(save_data = save_data)
  }
  if ("PWHL" %in% leagues) {
    result$pwhl <- updatePWHLModel(save_data = save_data)
  }

  .simplify_frontend_result(result, leagues)
}


#' Update saved NHL prediction snapshots
#'
#' @param data_dir (`character(1)`) Directory of saved NHL prediction files.
#' @param scores (`data.frame`) NHL scores.
#' @param schedule (`data.frame`) NHL schedule.
#' @param params (`list` or `NULL`) NHL model parameters.
#' @returns `NULL` (invisibly).
#' @keywords internal
.update_predictions_nhl <- function(
  data_dir = getOption("HockeyModel.prediction.path", "./prediction_results"),
  scores = HockeyModel::scores,
  schedule = HockeyModel::schedule,
  params = NULL
) {
  params <- parse_dc_params(params)

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  if (scores$Date[nrow(scores)] < (Sys.Date())) {
    updateScoresAPI(save_data = TRUE)
  }
  filelist <- list.files(path = data_dir)
  pdates <- substr(filelist, 1, 10) # gets the dates list of prediction
  pdates <- pdates[!is.na(as.Date(pdates))]
  if (length(pdates) == 0) {
    dcPredictMultipleDays(
      start = Sys.Date(),
      scores = scores,
      schedule = schedule,
      filedir = data_dir
    )
    return(invisible(NULL))
  }
  lastp <- as.Date(max(pdates))
  if (lastp != Sys.Date()) {
    dcPredictMultipleDays(
      start = as.Date(lastp) + 1,
      scores = scores,
      schedule = schedule,
      filedir = data_dir
    )
  }

  invisible(NULL)
}


#' Update saved PWHL prediction snapshots
#'
#' @param data_dir (`character(1)`) Directory of saved PWHL prediction files.
#' @param scores (`data.frame`) PWHL scores.
#' @param schedule (`data.frame`) PWHL schedule.
#' @param params (`list` or `NULL`) PWHL model parameters.
#' @returns `NULL` (invisibly).
#' @keywords internal
.update_predictions_pwhl <- function(
  data_dir = file.path(
    getOption("HockeyModel.prediction.path", "./prediction_results"),
    "pwhl"
  ),
  scores = HockeyModel::pwhlScores,
  schedule = HockeyModel::pwhlSchedule,
  params = NULL
) {
  params <- parse_pwhl_dc_params(params)

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  if (nrow(schedule) == 0) {
    return(invisible(NULL))
  }

  prediction_file <- file.path(data_dir, paste0(Sys.Date(), "-predictions.RDS"))
  if (file.exists(prediction_file)) {
    return(invisible(NULL))
  }

  sim_results <- pwhl_loopless_sim(
    scores = scores,
    schedule = schedule,
    params = params
  )
  saveRDS(.pwhl_prediction_summary(sim_results), prediction_file)

  invisible(NULL)
}

#' Update predictions
#'
#' @param data_dir directory of predictions
#' @param scores HockeyModel::scores or a custom value
#' @param schedule HockeyModel::schedule or a custom value
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param league which league front-end to run: `NULL`, `NA`, or `"both"` runs
#'   both leagues; `"nhl"` and `"pwhl"` run one league only
#'
#' @return NULL
#'
#' @export
updatePredictions <- function(
  data_dir = getOption("HockeyModel.prediction.path"),
  scores = HockeyModel::scores,
  schedule = HockeyModel::schedule,
  params = NULL,
  league = NULL
) {
  leagues <- .resolve_frontend_leagues(league)
  scores_missing <- missing(scores)
  schedule_missing <- missing(schedule)
  data_dirs <- .frontend_prediction_dirs(data_dir)
  result <- list()

  if ("NHL" %in% leagues) {
    nhl_data_dir <- if (is.list(data_dir) && !is.data.frame(data_dir)) {
      .frontend_value_for_league(data_dir, "NHL", data_dirs$nhl)
    } else {
      data_dirs$nhl
    }
    result$nhl <- .update_predictions_nhl(
      data_dir = nhl_data_dir,
      scores = .frontend_value_for_league(
        scores,
        "NHL",
        use_default = scores_missing,
        default = HockeyModel::scores
      ),
      schedule = .frontend_value_for_league(
        schedule,
        "NHL",
        use_default = schedule_missing,
        default = HockeyModel::schedule
      ),
      params = .frontend_value_for_league(params, "NHL", NULL)
    )
  }
  if ("PWHL" %in% leagues) {
    pwhl_data_dir <- if (is.list(data_dir) && !is.data.frame(data_dir)) {
      .frontend_value_for_league(data_dir, "PWHL", data_dirs$pwhl)
    } else {
      data_dirs$pwhl
    }
    result$pwhl <- .update_predictions_pwhl(
      data_dir = pwhl_data_dir,
      scores = .frontend_value_for_league(
        scores,
        "PWHL",
        use_default = scores_missing,
        default = HockeyModel::pwhlScores
      ),
      schedule = .frontend_value_for_league(
        schedule,
        "PWHL",
        use_default = schedule_missing,
        default = HockeyModel::pwhlSchedule
      ),
      params = .frontend_value_for_league(params, "PWHL", NULL)
    )
  }

  invisible(.simplify_frontend_result(result, leagues))
}


#' Build the NHL today-odds plot
#'
#' @param date (`Date`) Target date.
#' @param params (`list` or `NULL`) NHL model parameters.
#' @param schedule (`data.frame`) NHL schedule.
#' @param scores (`data.frame`) NHL scores.
#' @returns A [ggplot2::ggplot()] object or `NULL`.
#' @keywords internal
.today_odds_plot_nhl <- function(
  date = Sys.Date(),
  params = NULL,
  schedule = HockeyModel::schedule,
  scores = HockeyModel::scores
) {
  params <- parse_dc_params(params)

  if (scores$Date[nrow(scores)] < (date - 7)) {
    cli::cli_alert_info(
      "Scores may be out of date. This can affect predictions. Please update if midseason."
    )
  }
  games <- games_today(schedule = schedule, date = date)
  if (is.null(games) || nrow(games) == 0) {
    cli::cli_alert_info("No games today.")
    return(NULL)
  }
  plot_odds_today(
    today = date,
    params = params,
    schedule = schedule,
    league = "NHL"
  )
}


#' Build the PWHL today-odds plot
#'
#' @param date (`Date`) Target date.
#' @param params (`list` or `NULL`) PWHL model parameters.
#' @param schedule (`data.frame`) PWHL schedule.
#' @param scores (`data.frame`) PWHL scores.
#' @returns A [ggplot2::ggplot()] object or `NULL`.
#' @keywords internal
.today_odds_plot_pwhl <- function(
  date = Sys.Date(),
  params = NULL,
  schedule = HockeyModel::pwhlSchedule,
  scores = HockeyModel::pwhlScores
) {
  params <- parse_pwhl_dc_params(params)

  if (nrow(scores) > 0 && scores$Date[nrow(scores)] < (date - 7)) {
    cli::cli_alert_info(
      "PWHL scores may be out of date. This can affect predictions."
    )
  }
  games <- pwhl_games_today(schedule = schedule, date = date)
  if (is.null(games) || nrow(games) == 0) {
    cli::cli_alert_info("No PWHL games today.")
    return(NULL)
  }
  plot_odds_today(
    today = date,
    params = params,
    schedule = schedule,
    league = "PWHL"
  )
}

#' Today's game odds graphic
#'
#' @param date date to predict odds. Default today
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param schedule HockeyModel::schedule or a custom value
#' @param scores HockeyModel::scores or a custom value
#' @param league which league front-end to run: `NULL`, `NA`, or `"both"` runs
#'   both leagues; `"nhl"` and `"pwhl"` run one league only
#'
#' @return Today's odds ggplot object for a single league, or a named list of
#'   league plots when both leagues are requested.
#' @export
todayOddsPlot <- function(
  date = Sys.Date(),
  params = NULL,
  schedule = HockeyModel::schedule,
  scores = HockeyModel::scores,
  league = NULL
) {
  leagues <- .resolve_frontend_leagues(league)
  schedule_missing <- missing(schedule)
  scores_missing <- missing(scores)
  result <- list()

  if ("NHL" %in% leagues) {
    result$nhl <- .today_odds_plot_nhl(
      date = date,
      params = .frontend_value_for_league(params, "NHL", NULL),
      schedule = .frontend_value_for_league(
        schedule,
        "NHL",
        use_default = schedule_missing,
        default = HockeyModel::schedule
      ),
      scores = .frontend_value_for_league(
        scores,
        "NHL",
        use_default = scores_missing,
        default = HockeyModel::scores
      )
    )
  }
  if ("PWHL" %in% leagues) {
    result$pwhl <- .today_odds_plot_pwhl(
      date = date,
      params = .frontend_value_for_league(params, "PWHL", NULL),
      schedule = .frontend_value_for_league(
        schedule,
        "PWHL",
        use_default = schedule_missing,
        default = HockeyModel::pwhlSchedule
      ),
      scores = .frontend_value_for_league(
        scores,
        "PWHL",
        use_default = scores_missing,
        default = HockeyModel::pwhlScores
      )
    )
  }

  .simplify_frontend_result(result, leagues)
}


#' Load saved league predictions
#'
#' @param league (`character(1)`) Either `"NHL"` or `"PWHL"`.
#' @param data_dir (`character(1)`) Root prediction directory.
#' @returns A prediction history data frame.
#' @keywords internal
.league_predictions <- function(
  league = "NHL",
  data_dir = getOption("HockeyModel.prediction.path", "./prediction_results")
) {
  dirs <- .frontend_prediction_dirs(data_dir)
  compile_predictions(
    dir = if (league == "PWHL") dirs$pwhl else dirs$nhl
  )
}


#' Build the NHL playoff-odds plot
#'
#' @param data_dir (`character(1)`) Root prediction directory.
#' @returns A [ggplot2::ggplot()] object.
#' @keywords internal
.playoff_odds_nhl <- function(
  data_dir = getOption("HockeyModel.prediction.path", "./prediction_results")
) {
  plot_prediction_playoffs_by_team(
    all_predictions = .league_predictions("NHL", data_dir = data_dir)
  )
}


#' Build the PWHL playoff-odds plot
#'
#' @param data_dir (`character(1)`) Root prediction directory.
#' @returns A [ggplot2::ggplot()] object.
#' @keywords internal
.playoff_odds_pwhl <- function(
  data_dir = getOption("HockeyModel.prediction.path", "./prediction_results")
) {
  plot_prediction_playoffs_by_team(
    all_predictions = .league_predictions("PWHL", data_dir = data_dir),
    teamColours = HockeyModel::pwhlTeamColours
  )
}

#' Predict playoff odds graphic
#'
#' Convenience wrapper around [plot_prediction_playoffs_by_team()].
#'
#' @param data_dir (`character(1)`) Directory of saved prediction snapshots.
#' @param league (`character(1)` or `NULL`) Which league front-end to run.
#'   `NULL`, `NA`, and `"both"` run both leagues; `"nhl"` and `"pwhl"` run one
#'   league only.
#'
#' @returns If `league` is `"nhl"` or `"pwhl"`, a playoff-odds
#'   [ggplot2::ggplot()] object. If `league` is `NULL`, `NA`, or `"both"`, a
#'   named list with `nhl` and `pwhl` playoff-odds plots.
#' @export
playoffOdds <- function(
  data_dir = getOption("HockeyModel.prediction.path", "./prediction_results"),
  league = NULL
) {
  leagues <- .resolve_frontend_leagues(league)
  result <- list()

  if ("NHL" %in% leagues) {
    result$nhl <- .playoff_odds_nhl(data_dir = data_dir)
  }
  if ("PWHL" %in% leagues) {
    result$pwhl <- .playoff_odds_pwhl(data_dir = data_dir)
  }

  .simplify_frontend_result(result, leagues)
}


#' Build the NHL first-place plot
#'
#' @param data_dir (`character(1)`) Root prediction directory.
#' @returns A [ggplot2::ggplot()] object.
#' @keywords internal
.president_odds_nhl <- function(
  data_dir = getOption("HockeyModel.prediction.path", "./prediction_results")
) {
  plot_prediction_presidents_by_team(
    all_predictions = .league_predictions("NHL", data_dir = data_dir)
  )
}


#' Build the PWHL first-place plot
#'
#' @param data_dir (`character(1)`) Root prediction directory.
#' @returns A [ggplot2::ggplot()] object.
#' @keywords internal
.president_odds_pwhl <- function(
  data_dir = getOption("HockeyModel.prediction.path", "./prediction_results")
) {
  plot_prediction_presidents_by_team(
    all_predictions = .league_predictions("PWHL", data_dir = data_dir),
    teamColours = HockeyModel::pwhlTeamColours
  )
}

#' Predict President's Odds graphic
#'
#' Convenience wrapper around [plot_prediction_presidents_by_team()].
#'
#' @param data_dir (`character(1)`) Directory of saved prediction snapshots.
#' @param league (`character(1)` or `NULL`) Which league front-end to run.
#'   `NULL`, `NA`, and `"both"` run both leagues; `"nhl"` and `"pwhl"` run one
#'   league only.
#'
#' @returns If `league` is `"nhl"` or `"pwhl"`, a President's Trophy odds
#'   [ggplot2::ggplot()] object. If `league` is `NULL`, `NA`, or `"both"`, a
#'   named list with `nhl` and `pwhl` President's Trophy odds plots.
#' @export
presidentOdds <- function(
  data_dir = getOption("HockeyModel.prediction.path", "./prediction_results"),
  league = NULL
) {
  leagues <- .resolve_frontend_leagues(league)
  result <- list()

  if ("NHL" %in% leagues) {
    result$nhl <- .president_odds_nhl(data_dir = data_dir)
  }
  if ("PWHL" %in% leagues) {
    result$pwhl <- .president_odds_pwhl(data_dir = data_dir)
  }

  .simplify_frontend_result(result, leagues)
}


#' Build the NHL points projection plot
#'
#' @param data_dir (`character(1)`) Root prediction directory.
#' @returns A [ggplot2::ggplot()] object.
#' @keywords internal
.point_predict_nhl <- function(
  data_dir = getOption("HockeyModel.prediction.path", "./prediction_results")
) {
  plot_prediction_points_by_team(
    all_predictions = .league_predictions("NHL", data_dir = data_dir)
  )
}


#' Build the PWHL points projection plot
#'
#' @param data_dir (`character(1)`) Root prediction directory.
#' @returns A [ggplot2::ggplot()] object.
#' @keywords internal
.point_predict_pwhl <- function(
  data_dir = getOption("HockeyModel.prediction.path", "./prediction_results")
) {
  plot_prediction_points_by_team(
    all_predictions = .league_predictions("PWHL", data_dir = data_dir),
    teamColours = HockeyModel::pwhlTeamColours
  )
}

#' Predict Points graphic
#'
#' Convenience wrapper around [plot_prediction_points_by_team()].
#'
#' @param data_dir (`character(1)`) Directory of saved prediction snapshots.
#' @param league (`character(1)` or `NULL`) Which league front-end to run.
#'   `NULL`, `NA`, and `"both"` run both leagues; `"nhl"` and `"pwhl"` run one
#'   league only.
#'
#' @returns If `league` is `"nhl"` or `"pwhl"`, a point-projection
#'   [ggplot2::ggplot()] object. If `league` is `NULL`, `NA`, or `"both"`, a
#'   named list with `nhl` and `pwhl` point-projection plots.
#' @export
pointPredict <- function(
  data_dir = getOption("HockeyModel.prediction.path", "./prediction_results"),
  league = NULL
) {
  leagues <- .resolve_frontend_leagues(league)
  result <- list()

  if ("NHL" %in% leagues) {
    result$nhl <- .point_predict_nhl(data_dir = data_dir)
  }
  if ("PWHL" %in% leagues) {
    result$pwhl <- .point_predict_pwhl(data_dir = data_dir)
  }

  .simplify_frontend_result(result, leagues)
}


#' Build the NHL ratings plot
#'
#' @param m (`any`) NHL model `m`.
#' @returns A [ggplot2::ggplot()] object.
#' @keywords internal
.ratings_nhl <- function(m = HockeyModel::m) {
  plot_team_rating(m = m, league = "NHL")
}


#' Build the PWHL ratings plot
#'
#' @param m (`any`) PWHL model `m`.
#' @returns A [ggplot2::ggplot()] object.
#' @keywords internal
.ratings_pwhl <- function(m = HockeyModel::pwhl_m) {
  plot_team_rating(m = m, league = "PWHL")
}

#' Current ratings
#'
#' @param m HockeyModel::m or a custom value
#' @param league which league front-end to run: `NULL`, `NA`, or `"both"` runs
#'   both leagues; `"nhl"` and `"pwhl"` run one league only
#'
#' @return Today's ratings ggplot object for a single league, or a named list of
#'   league plots when both leagues are requested.
#' @export
ratings <- function(m = HockeyModel::m, league = NULL) {
  leagues <- .resolve_frontend_leagues(league)
  m_missing <- missing(m)
  result <- list()

  if ("NHL" %in% leagues) {
    result$nhl <- .ratings_nhl(
      m = .frontend_value_for_league(
        m,
        "NHL",
        use_default = m_missing,
        default = HockeyModel::m
      )
    )
  }
  if ("PWHL" %in% leagues) {
    result$pwhl <- .ratings_pwhl(
      m = .frontend_value_for_league(
        m,
        "PWHL",
        use_default = m_missing,
        default = HockeyModel::pwhl_m
      )
    )
  }

  .simplify_frontend_result(result, leagues)
}

#' Post daily model graphics to social media
#'
#' @param games (`data.frame`) Games to reference for game-specific posts.
#' @param graphic_dir (`character(1)`) Directory containing generated images.
#' @param delay (`double(1)`) Delay in seconds between posts.
#' @param schedule (`data.frame`) Schedule used to determine daily context.
#' @returns `NULL` (invisibly).
#' @keywords internal
tweet <- function(
  games,
  graphic_dir = .default_nhl_graphics_dir(),
  delay = stats::runif(1, min = 2, max = 6) * 60,
  schedule = HockeyModel::schedule
) {
  if (inRegularSeason()) {
    # Only runs if schedule has regular season games remaining

    # rtoot::post_toot(
    #   status = paste0("Predicted points for #NHL teams (before games on ", Sys.Date(), ")."),
    #   media = file.path(graphic_dir, "point_predict.png"), # token = token,
    #   alt_text = paste0("Points predicted history for the last 14 days, as of ", Sys.Date(), "")
    # )
    try(
      atrrr::post(
        text = paste0(
          "Predicted points for #NHL teams (before games on ",
          Sys.Date(),
          ")."
        ),
        image = file.path(graphic_dir, "point_predict.png"), # token = token,
        image_alt = paste0(
          "Points predicted history for the last 14 days, as of ",
          Sys.Date(),
          ""
        )
      )
    )

    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)

    # rtoot::post_toot(
    #   status = paste0("Playoff odds for #NHL teams (before games on ", Sys.Date(), "). #HockeyTwitter"),
    #   media = file.path(graphic_dir, "playoff_odds.png"),
    #   alt_text = paste0("Playoff Odds for each NHL team history and today's value as of ", Sys.Date(), "")
    # )
    try(
      atrrr::post(
        text = paste0(
          "Playoff odds for #NHL teams (before games on ",
          Sys.Date(),
          "). #HockeyTwitter"
        ),
        image = file.path(graphic_dir, "playoff_odds.png"),
        image_alt = paste0(
          "Playoff Odds for each NHL team history and today's value as of ",
          Sys.Date(),
          ""
        )
      )
    )

    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)

    # rtoot::post_toot(
    #   status = paste0("President's trophy odds for #NHL teams (before games on ", Sys.Date(), "). #HockeyTwitter"),
    #   media = file.path(graphic_dir, "president_odds.png"),
    #   alt_text = paste0("President's Trophy Odds for each NHL team history and today's value as of ", Sys.Date(), "")
    # )
    try(
      atrrr::post(
        text = paste0(
          "President's trophy odds for #NHL teams (before games on ",
          Sys.Date(),
          "). #HockeyTwitter"
        ),
        image = file.path(graphic_dir, "president_odds.png"),
        image_alt = paste0(
          "President's Trophy Odds for each NHL team history and today's value as of ",
          Sys.Date(),
          ""
        )
      )
    )
  }

  return(invisible(NULL))
}

#' Daily functions, rolled into one call
#'
#' @param graphic_dir Directory for graphic files
#' @param subdir subdirectory to `graphic_dir` for pace plots
#' @param delay delay between tweet posts
#' @returns `NULL` (invisibly).
#' @keywords internal
.daily_summary_nhl <- function(
  graphic_dir = .default_nhl_graphics_dir(),
  subdir = "pace",
  delay = stats::runif(1, min = 2, max = 6) * 60
) {
  if (inOffSeason()) {
    if (
      getSeasonStartDate() - Sys.Date() > 7 ||
        getSeasonStartDate() - Sys.Date() < 0
    ) {
      stop("Offseason")
    }
  }
  modelparams <- updateModel()
  sc <- modelparams$schedule
  params <- parse_dc_params(params = modelparams)

  if (Sys.Date() > max(sc$Date)) {
    stop("No future games planned")
  }

  if (!dir.exists(graphic_dir)) {
    dir.create(graphic_dir, recursive = TRUE)
  }

  message("Creating graphics...")

  # generate plots
  if (!is.null(games_today())) {
    today <- todayOddsPlot(
      params = params,
      schedule = modelparams$schedule,
      scores = modelparams$scores
    )
    # save to files.
    grDevices::png(
      filename = file.path(graphic_dir, "today_odds.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(today)
    Sys.sleep(5)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    today_table <- daily_odds_table(
      params = params,
      schedule = modelparams$schedule
    )
    save_gt_as_png(
      today_table,
      filename = file.path(graphic_dir, "today_odds_table.png")
    )

    # rtoot::post_toot(
    #   status = "Predicted odds table for today's #NHL games.",
    #   media = file.path(graphic_dir, "today_odds_table.png"),
    #   alt_text = paste0("Odds table for Today's NHL games, for date ", Sys.Date(), ".")
    # )
    try(
      atrrr::post(
        text = "Predicted odds table for today's #NHL games.",
        image = file.path(graphic_dir, "today_odds_table.png"),
        image_alt = paste0(
          "Odds table for Today's NHL games, for date ",
          Sys.Date(),
          "."
        )
      )
    )

    # rtoot::post_toot(
    #   status = "Predicted odds for today's #NHL games.",
    #   media = file.path(graphic_dir, "today_odds.png"),
    #   alt_text = paste0("Odds graphic for Today's NHL games, for date ", Sys.Date(), ".")
    # )
    try(
      atrrr::post(
        text = "Predicted odds for today's #NHL games.",
        image = file.path(graphic_dir, "today_odds.png"),
        image_alt = paste0(
          "Odds graphic for Today's NHL games, for date ",
          Sys.Date(),
          "."
        )
      )
    )

    rating <- ratings(params$m)
    # save to files.
    grDevices::png(
      filename = file.path(graphic_dir, "current_rating.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(rating)
    Sys.sleep(5)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    # rtoot::post_toot(
    #   status = paste0("Current team ratings (as of ", Sys.Date(), ")."),
    #   media = file.path(graphic_dir, "current_rating.png"),
    #   alt_text = paste0("Current team rating graphic for ", Sys.Date(), ".")
    # )
    try(
      atrrr::post(
        text = paste0("Current team ratings (as of ", Sys.Date(), ")."),
        image = file.path(graphic_dir, "current_rating.png"),
        image_alt = paste0("Current team rating graphic for ", Sys.Date(), ".")
      )
    )
  }

  if (inRegularSeason()) {
    updatePredictions(
      scores = modelparams$scores,
      schedule = modelparams$schedule,
      params = params
    )
    playoff <- playoffOdds()
    president <- presidentOdds()
    point <- pointPredict()
    rating <- ratings(m = params$m)

    Sys.sleep(15)

    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    grDevices::png(
      filename = file.path(graphic_dir, "playoff_odds.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(playoff)
    Sys.sleep(5)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    grDevices::png(
      filename = file.path(graphic_dir, "president_odds.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(president)
    Sys.sleep(5)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    grDevices::png(
      filename = file.path(graphic_dir, "point_predict.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(point)
    Sys.sleep(5)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    grDevices::png(
      filename = file.path(graphic_dir, "current_rating.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(rating)
    Sys.sleep(5)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    # Make Pace Plots
    plot_pace_by_team(
      graphic_dir = graphic_dir,
      subdir = subdir,
      scores = modelparams$scores
    )
    plot_pace_by_division(
      graphic_dir = graphic_dir,
      subdir = subdir,
      scores = modelparams$scores
    )
    plot_point_likelihood(graphic_dir = graphic_dir, subdir = subdir)
  }

  message("Posting Tweets...")
  tweet(graphic_dir, delay = delay, graphic_dir = graphic_dir) # , games_today = Sys.Date() %in% sc[sc$GameStatus != "Postponed", ]$Date)

  message("Delaying ", delay, " seconds to space tweets...")
  Sys.sleep(delay)

  if (inRegularSeason()) {
    tweetPlayoffOdds(graphic_dir = graphic_dir, params = params)

    message("Delaying ", delay / 2, " seconds to space tweets...")
    Sys.sleep(delay / 2)
  } else if (inPlayoffs()) {
    message("Calculating Playoff Odds")
    tweetPlayoffOdds(graphic_dir = graphic_dir, trimcup = TRUE)
  }

  if (as.numeric(format(Sys.Date(), "%w")) == 1 && inRegularSeason()) {
    # On monday post pace plots
    tweetPace(delay = delay, graphic_dir = graphic_dir)
  }

  if (as.numeric(format(Sys.Date(), "%w")) == 0 && inRegularSeason()) {
    message("Tweeting Metrics")
    # On Sunday post metrics
    tweetMetrics()
  }

  if (as.numeric(format(Sys.Date(), "%w")) == 2 && inRegularSeason()) {
    message("Tweeting Likelihoods")
    # On Tuesday post expected points (likelihood)
    tweetLikelihoods(delay = delay, graphic_dir = graphic_dir)
  }

  series <- getAPISeries()
  if (
    !is.na(series) &&
      length(series) > 1 &&
      nrow(series[series$Status == "Ongoing", ]) > 0
  ) {
    # TODO: Watch next spring to see if this goes ok
    message("Tweeting Series")
    tweetSeries(graphic_dir = graphic_dir, params = params)
    Sys.sleep(delay)
  }
}


#' Daily front-end summary
#'
#' Runs the daily front-end workflow for NHL, PWHL, or both leagues.
#'
#' @param graphic_dir (`character(1)`) Directory for graphic files. When
#'   omitted, NHL uses [getOption()] `HockeyModel.graphics.path` and PWHL uses
#'   `file.path(getOption("HockeyModel.prediction.path"), "pwhl_graphics")`.
#' @param subdir (`character(1)`) Subdirectory to `graphic_dir` for pace plots.
#' @param delay (`double(1)`) Delay between social-media posts in seconds.
#' @param league (`character(1)` or `NULL`) Which league front-end to run. Can
#'   be one of:
#'   * `NULL`, `NA`, or `"both"`: Run both leagues.
#'   * `"nhl"`: Run the NHL workflow only.
#'   * `"pwhl"`: Run the PWHL workflow only.
#'
#' @returns For a single league, `NULL` (invisibly). For both leagues, a named
#'   list with `nhl` and `pwhl` entries.
#' @export
dailySummary <- function(
  graphic_dir = .default_nhl_graphics_dir(),
  subdir = "pace",
  delay = stats::runif(1, min = 2, max = 6) * 60,
  league = NULL
) {
  graphic_dir_missing <- missing(graphic_dir)
  leagues <- .resolve_frontend_leagues(league)
  result <- list()

  if ("NHL" %in% leagues) {
    result$nhl <- .daily_summary_nhl(
      graphic_dir = .frontend_value_for_league(
        graphic_dir,
        "NHL",
        .default_nhl_graphics_dir()
      ),
      subdir = subdir,
      delay = delay
    )
  }
  if ("PWHL" %in% leagues) {
    pwhl_graphic_dir <- if (graphic_dir_missing) {
      .default_pwhl_graphics_dir()
    } else {
      .frontend_value_for_league(
        graphic_dir,
        "PWHL",
        .default_pwhl_graphics_dir()
      )
    }
    result$pwhl <- dailyPWHLSummary(
      graphic_dir = pwhl_graphic_dir,
      delay = delay
    )
  }

  .simplify_frontend_result(result, leagues)
}

#' Tweet Pace Plots
#'
#' @param delay Delay between posted tweets
#' @param graphic_dir The graphics directory
#' @param subdir The pace subdirectory in graphics
#' @param prediction_dir The predictions directory
#' @param scores HockeyModel::scores or a custom value
#'
#' @export
tweetPace <- function(
  delay = stats::runif(1, min = 1, max = 3) * 60,
  graphic_dir = getOption("HockeyModel.graphics.path"),
  subdir = "pace",
  prediction_dir = getOption("HockeyModel.prediction.path"),
  scores = HockeyModel::scores
) {
  # make sure we're working with the most up-to-date info.
  scores <- updateScoresAPI(save_data = TRUE)

  # Make Pace Plots
  plot_pace_by_team(
    graphic_dir = graphic_dir,
    subdir = subdir,
    prediction_dir = prediction_dir,
    scores = scores
  )

  filelist <- list.files(path = prediction_dir)
  pdates <- substr(filelist, 1, 10) # gets the dates list of prediction
  pdates <- pdates[!is.na(as.Date(pdates))]
  lastp <- as.Date(max(pdates))
  current_preds <- readRDS(file.path(
    prediction_dir,
    paste0(lastp, "-predictions.RDS")
  ))
  preds <- readRDS(file.path(
    prediction_dir,
    paste0(getSeasonStartDate(), "-predictions.RDS")
  ))
  scores <- scores[scores$Date > as.Date(getSeasonStartDate()), ]

  teamlist <- unique(preds$Team)

  teamColours <- HockeyModel::teamColours

  reply_id <- NULL
  for (team in teamlist) {
    ngames <- sum(sum(scores$HomeTeam == team), sum(scores$AwayTeam == team))
    status <- paste0(
      team,
      " pace after ",
      ngames,
      " games. The model initially predicted ",
      format(
        round(as.numeric(preds[preds$Team == team, "meanPoints"]), digits = 1),
        nsmall = 1
      ),
      " points, now expecting ",
      format(
        round(
          as.numeric(current_preds[current_preds$Team == team, "meanPoints"]),
          digits = 1
        ),
        nsmall = 1
      ),
      ". #HockeyTwitter ",
      teamColours[teamColours$Team == team, "Hashtag"]
    )

    try(
      atrrr::post(
        text = status,
        image = file.path(
          graphic_dir,
          subdir,
          paste0(tolower(gsub(" ", "_", team)), ".png")
        ),
        image_alt = paste0(
          team,
          "'s Performance against predicted pace as of ",
          Sys.Date(),
          ""
        )
      )
    )

    # rtoot::post_toot(
    #   status = status,
    #   media = file.path(graphic_dir, subdir, paste0(tolower(gsub(" ", "_", team)), ".png")),
    #   alt_text = paste0(team, "'s Performance against predicted pace as of ", Sys.Date(), "")
    # )

    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(stats::runif(1, min = 1, max = 3) * 60)
  }
  pacediff <- data.frame(
    "Team" = current_preds$Team,
    "Initial" = preds$meanPoints,
    "Current" = current_preds$meanPoints,
    stringsAsFactors = FALSE
  )
  pacediff$Diff <- pacediff$Current - pacediff$Initial

  maxteam <- pacediff[which.max(pacediff$Diff), "Team"]
  minteam <- pacediff[which.min(pacediff$Diff), "Team"]

  recapstatus <- paste0(
    "To recap - ",
    "\nFurthest above expectation: ",
    maxteam,
    " ",
    teamColours[teamColours$Team == maxteam, "Hashtag"],
    "\nFurthest below expectation: ",
    minteam,
    " ",
    teamColours[teamColours$Team == minteam, "Hashtag"]
  )
  atrrr::post(text = recapstatus)
  # rtoot::post_toot(status = recapstatus)

  Sys.sleep(stats::runif(1, min = 2, max = 6) * 60)

  # Make Division Plots
  plot_pace_by_division(
    graphic_dir = graphic_dir,
    subdir = subdir,
    prediction_dir = prediction_dir,
    scores = scores
  )

  for (division in getDivisions()) {
    status <- paste(
      "Current Points compared to predicted (at season start) for #NHL teams in the",
      division,
      "division.\nPositive values are exceeding expectation, negative are performing below predicted."
    )
    # rtoot::post_toot(
    #   status = status,
    #   media = file.path(graphic_dir, subdir, paste0(division, "_pace.png")),
    #   alt_text = paste0(division, " teams pace above/below expected as of ", Sys.Date(), ".")
    # )
    try(
      atrrr::post(
        text = status,
        image = file.path(graphic_dir, subdir, paste0(division, "_pace.png")),
        image_alt = paste0(
          division,
          " teams pace above/below expected as of ",
          Sys.Date(),
          "."
        )
      )
    )

    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)
  }
}

#' Tweet Likelihood plots (ggridges)
#'
#' @param delay time to delay. Default 5 min
#' @param graphic_dir graphics directory
#' @param subdir subdirectory - usually 'preds'
#' @param scores updated scores
#'
#' @returns `NULL` (invisibly).
#' @export
tweetLikelihoods <- function(
  delay = stats::runif(1, min = 3, max = 6) * 60,
  graphic_dir = getOption("HockeyModel.graphics.path"),
  subdir = "pace",
  scores = HockeyModel::scores
) {
  # make likelihood plots
  plot_point_likelihood(graphic_dir = graphic_dir, subdir = subdir)

  for (conf in getConferences()) {
    if (
      file.exists(file.path(
        graphic_dir,
        subdir,
        paste0(tolower(conf), "likelihood.png")
      )) &&
        as.Date(file.mtime(file.path(
          graphic_dir,
          subdir,
          paste0(tolower(conf), "likelihood.png")
        ))) ==
          Sys.Date()
    ) {
      # Tweet them out

      # rtoot::post_toot(
      #   status = paste0("#NHL ", conf, " Conference Team final point likelihoods:"),
      #   media = file.path(graphic_dir, subdir, paste0(tolower(conf), "likelihood.png")),
      #   alt_text = paste0("Point likelihoods for teams in the ", conf, " conference.")
      # )
      try(
        atrrr::post(
          text = paste0(
            "#NHL ",
            conf,
            " Conference Team final point likelihoods:"
          ),
          image = file.path(
            graphic_dir,
            subdir,
            paste0(tolower(conf), "likelihood.png")
          ),
          image_alt = paste0(
            "Point likelihoods for teams in the ",
            conf,
            " conference."
          )
        )
      )

      # delay
      message("Delaying ", delay / 2, " seconds to space tweets...")
      Sys.sleep(delay / 2)
    }
  }

  return(invisible(NULL))
}

#' Tweet Game Plots
#'
#' @param games Games to tweet graphics from
#' @param delay Delay between tweets
#' @param graphic_dir the graphics directory
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @export
tweetGames <- function(
  games = games_today(),
  delay = stats::runif(1, min = 4, max = 8) * 60,
  graphic_dir = getOption("HockeyModel.graphics.path"),
  params = NULL
) {
  params <- parse_dc_params(params)
  # Tweet each game
  if (is.null(games)) {
    message("No games to tweet")
    return()
  }

  if (nrow(games) == 0) {
    message("No games to tweet")
    return()
  }

  if (!dir.exists(graphic_dir)) {
    dir.create(graphic_dir, recursive = TRUE)
  }

  teamColours <- HockeyModel::teamColours

  for (g in seq_len(nrow(games))) {
    home <- as.character(games[g, "HomeTeam"])
    away <- as.character(games[g, "AwayTeam"])
    plt <- plot_game(home = home, away = away, params = params)
    grDevices::png(
      filename = file.path(graphic_dir, "predicted_goals.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(plt)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }
    status <- paste0(
      teamColours[teamColours$Team == away, "Hashtag"],
      " at ",
      teamColours[teamColours$Team == home, "Hashtag"],
      " predicted goals. #",
      getShortTeam(away),
      "vs",
      getShortTeam(home),
      " #HockeyTwitter"
    )

    # rtoot::post_toot(
    #   status = status,
    #   media = file.path(graphic_dir, "predicted_goals.png"),
    #   alt_text = paste0("Odds of each goal for both ", away, " and ", home, " in their game.")
    # )
    try(
      atrrr::post(
        text = status,
        image = file.path(graphic_dir, "predicted_goals.png"),
        image_alt = paste0(
          "Odds of each goal for both ",
          away,
          " and ",
          home,
          " in their game."
        )
      )
    )

    file.remove(file.path(graphic_dir, "predicted_goals.png"))

    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)
  }
}

#' Tweet Metrics
#' @description Tweet the metrics (Log Loss and Accuracy)
#'
#' @returns `NULL` (invisibly).
#' @export
tweetMetrics <- function() {
  metrics <- getSeasonMetricsDC()

  status <- paste0(
    "Metrics as of ",
    Sys.Date(),
    "\nLog Loss: ",
    round(metrics$LogLoss, 4),
    "\nAccuracy: ",
    round(metrics$Accuracy * 100, 2),
    " %"
  )
  message(status)

  # rtoot::post_toot(status = status)
  try(atrrr::post(text = status))
}

#' Tweet Series
#' @description Tweet the series odds graphics
#'
#' @param graphic_dir directory to save the image
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param delay Delay in seconds between posts. Default is a random value between 1 and 3 minutes.
#'
#' @return NULL
#' @export
tweetSeries <- function(
  params = NULL,
  graphic_dir = getOption("HockeyModel.graphics.path"),
  delay = stats::runif(1, min = 1, max = 3) * 60
) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg gt} is required to save the series odds table image.",
        "i" = "Install it with {.code install.packages('gt')}."
      )
    )
  }

  params <- parse_dc_params(params)
  while (grDevices::dev.cur() != 1) {
    grDevices::dev.off()
  }
  series <- getAPISeries()
  series <- series[
    series$Status == "Ongoing",
    c("HomeTeam", "AwayTeam", "HomeWins", "AwayWins")
  ]
  if (nrow(series) == 0) {
    message("No Series to Tweet")
    return()
  }
  plt <- plot_playoff_series_odds(series = series, params = params)
  grDevices::png(
    filename = file.path(graphic_dir, "series_odds.png"),
    width = 11,
    height = 8.5,
    units = "in",
    res = 300
  )
  print(plt)
  while (grDevices::dev.cur() != 1) {
    grDevices::dev.off()
  }

  status <- paste0(
    "#NHL #StanleyCup Playoff Series Odds before games on ",
    Sys.Date()
  )
  # rtoot::post_toot(
  #   status = status,
  #   media = file.path(graphic_dir, "series_odds.png"),
  #   alt_text = "A graphic showing odds for each series' winner"
  # )
  try(
    atrrr::post(
      text = status,
      image = file.path(graphic_dir, "series_odds.png"),
      image_alt = "A graphic showing odds for each series' winner"
    )
  )

  message("Delaying ", delay, " seconds to space tweets...")
  Sys.sleep(delay)

  tbl <- series_odds_table(series = series, params = params)
  save_gt_as_png(
    tbl,
    filename = file.path(graphic_dir, "series_odds_table.png")
  )

  try(
    atrrr::post(
      text = paste0(
        "#NHL #StanleyCup Playoff Series Odds table before games on ",
        Sys.Date()
      ),
      image = file.path(graphic_dir, "series_odds_table.png"),
      image_alt = "A table showing odds for each series' winner"
    )
  )
}


#' Tweet Playoff Odds
#'
#' @description Tweet a graphic of the playoff odds
#'
#' @param summary_results the summary results file, otherwise the most recent will be loaded
#' @param graphic_dir graphic dir
#' @param trimcup trim to just cup winners
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return NULL
#' @export
tweetPlayoffOdds <- function(
  summary_results = NULL,
  params = NULL,
  graphic_dir = getOption("HockeyModel.graphics.path"),
  trimcup = FALSE
) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg gt} is required. Install it with {.code install.packages('gt')}."
    )
  }

  params <- parse_dc_params(params)
  playoffodds <- simulatePlayoffs(
    summary_results = summary_results,
    params = params
  )

  playoffodds$Conference <- getTeamConferences(playoffodds$Team)
  if (trimcup) {
    plt <- format_playoff_odds(
      playoff_odds = playoffodds,
      caption_text = "NHL Playoffs",
      trim = FALSE,
      trimcup = trimcup
    )
    save_gt_as_png(
      plt,
      filename = file.path(graphic_dir, "playoff_odds.png")
    )

    status <- paste0(
      "#NHL Eastern and Western Conference Playoff and #StanleyCup Odds before games on ",
      Sys.Date(),
      ". #HockeyTwitter"
    )

    # Posting Tweet
    # rtoot::post_toot(
    #   status = paste0("#NHL Playoff and #StanleyCup Odds before games on ", Sys.Date(), "."),
    #   media = file.path(graphic_dir, "playoff_odds.png"),
    #   alt_text = "Playoff Odds"
    # )
    try(
      atrrr::post(
        text = paste0(
          "#NHL Playoff and #StanleyCup Odds before games on ",
          Sys.Date(),
          "."
        ),
        image = file.path(graphic_dir, "playoff_odds.png"),
        image_alt = "Playoff Odds"
      )
    )
  } else {
    for (conf in unique(playoffodds$Conference)) {
      plt <- format_playoff_odds(
        playoff_odds = playoffodds[
          playoffodds$Conference == conf,
          which(names(playoffodds) != "Conference")
        ],
        caption_text = paste(conf, "Conference"),
        trim = FALSE,
        trimcup = trimcup
      )
      save_gt_as_png(
        plt,
        filename = file.path(
          graphic_dir,
          paste0(tolower(conf), "_playoff_odds.png")
        )
      )
    }
    status <- paste0(
      "#NHL Eastern and Western Conference Playoff and #StanleyCup Odds before games on ",
      Sys.Date(),
      ". #HockeyTwitter"
    )

    # Posting Tweet
    # rtoot::post_toot(
    #   status = paste0("#NHL Eastern Conference Playoff and #StanleyCup Odds before games on ", Sys.Date(), "."),
    #   media = file.path(graphic_dir, "eastern_playoff_odds.png"),
    #   alt_text = "Eastern Playoff Odds"
    # )
    # rtoot::post_toot(
    #   status = paste0("#NHL Western Conference Playoff and #StanleyCup Odds before games on ", Sys.Date(), "."),
    #   media = file.path(graphic_dir, "western_playoff_odds.png"),
    #   alt_text = "Western Playoff Odds"
    # )
    try(
      atrrr::post(
        text = paste0(
          "#NHL Eastern Conference Playoff and #StanleyCup Odds before games on ",
          Sys.Date(),
          "."
        ),
        image = file.path(graphic_dir, "eastern_playoff_odds.png"),
        image_alt = "Eastern Playoff Odds"
      )
    )

    try(
      atrrr::post(
        text = paste0(
          "#NHL Western Conference Playoff and #StanleyCup Odds before games on ",
          Sys.Date(),
          "."
        ),
        image = file.path(graphic_dir, "western_playoff_odds.png"),
        image_alt = "Western Playoff Odds"
      )
    )
  }
}
