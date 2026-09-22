# Front-end core: model/prediction update orchestration for NHL and PWHL

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
  pdates <- get_prediction_dates(data_dir)
  lastp <- if (length(pdates) == 0L) {
    as.Date(getSeasonStartDate()) - 1L
  } else {
    max(pdates)
  }
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
