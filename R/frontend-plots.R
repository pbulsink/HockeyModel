# Front-end plots: odds, playoff, president's, points, and ratings graphics

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
