# NHL prediction logging and backfilling

#' Record Today's Predictions
#'
#' @description Record today's predictions to file (for easy later retrieval). Run \code{cleanupPredictionsFile} periodically to tidy
#'
#' @param today Day's predictions to record. Defaults to today, but can set any other day
#' @param filepath csv file location to store predictions. Will append to file.
#' @param schedule HockeyModel::schedule or supplied. \code{today} date must be in schedule
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param include_xG Whether to record daily XG values
#' @param draws whether to record draw odds (True) or not (False). Default is True
#'
#' @return NULL
#' @export
recordTodaysPredictions <- function(
  today = Sys.Date(),
  filepath = file.path(getOption("HockeyModel.data.path"), "dailyodds.csv"),
  schedule = HockeyModel::schedule,
  params = NULL,
  include_xG = FALSE,
  draws = TRUE
) {
  params <- parse_dc_params(params)
  if (!is.Date(today)) {
    cli::cli_abort("{.arg today} must be a Date or date-like value.")
  }
  today <- as.Date(today)
  today_sched <- schedule[schedule$Date == today, ]
  if (nrow(today_sched) == 0) {
    stop("No games on date:", today)
  }
  today_preds <- todayDC(
    today = today,
    params = params,
    schedule = schedule,
    include_xG = include_xG,
    draws = draws
  )
  preds <- dplyr::full_join(
    today_sched,
    today_preds,
    suffix = c("", ""),
    by = c("HomeTeam", "AwayTeam")
  )
  if (!include_xG) {
    if (!draws) {
      preds <- preds[, c(
        "Date",
        "GameID",
        "HomeTeam",
        "AwayTeam",
        "HomeWin",
        "AwayWin"
      )]
    } else {
      preds <- preds[, c(
        "Date",
        "GameID",
        "HomeTeam",
        "AwayTeam",
        "HomeWin",
        "AwayWin",
        "Draw"
      )]
    }
  } else {
    if (!draws) {
      preds <- preds[, c(
        "Date",
        "HomeTeam",
        "AwayTeam",
        "HomeWin",
        "AwayWin",
        "Home_xG",
        "Away_xG"
      )]
    } else {
      preds <- preds[, c(
        "Date",
        "HomeTeam",
        "AwayTeam",
        "HomeWin",
        "AwayWin",
        "Draw",
        "Home_xG",
        "Away_xG"
      )]
    }
  }

  if (file.exists(filepath)) {
    utils::write.table(
      preds,
      file = filepath,
      append = TRUE,
      col.names = FALSE,
      row.names = FALSE,
      sep = ",",
      dec = "."
    )
  } else {
    utils::write.table(
      preds,
      file = filepath,
      append = FALSE,
      col.names = TRUE,
      row.names = FALSE,
      sep = ",",
      dec = "."
    )
  }
}

#' Cleanup Predictions File
#'
#' @description Sometimes the predictions file may end up with game duplicates (last minute postponements, etc,) This deduplicates, taking only the latest instance of a prediction (Games are unique by GameID)
#'
#' @param filepath file path to cleanup
#'
#' @return NULL
#' @export
cleanupPredictionsFile <- function(
  filepath = file.path(getOption("HockeyModel.data.path"), "dailyodds.csv")
) {
  if (!file.exists(filepath)) {
    return(FALSE)
  }
  dailyodds <- utils::read.csv(filepath)
  dailyodds <- dailyodds |>
    dplyr::mutate("Date" = as.Date(.data$Date)) |>
    dplyr::arrange(dplyr::desc(.data$Date)) |>
    dplyr::distinct(.data$GameID, .keep_all = TRUE) |>
    dplyr::arrange(.data$Date, .data$GameID) |>
    utils::write.table(
      file = filepath,
      append = FALSE,
      col.names = TRUE,
      row.names = FALSE,
      sep = ",",
      dec = "."
    )

  return(TRUE)
}

#' Backfill historical daily prediction records
#'
#' @param startDate (`Date`) First date to backfill.
#' @param endDate (`Date`) Last date to backfill.
#' @param scores scores, if not then HockeyModel::scores is used.
#' @param schedule schedule, if not then HockeyModel::schedule is used.
#' @param filepath (`character(1)`) CSV path for recorded predictions.
#' @param include_xG (`logical(1)`) Whether to include xG columns.
#' @param draws (`logical(1)`) Whether to store draw probabilities.
#' @returns (`logical(1)`) `TRUE` when processing completes.
#' @keywords internal
build_past_predictions <- function(
  startDate,
  endDate,
  scores = HockeyModel::scores,
  schedule = HockeyModel::schedule,
  filepath = file.path(getOption("HockeyModel.data.path"), "dailyodds.csv"),
  include_xG = FALSE,
  draws = TRUE
) {
  if (!is.Date(startDate)) {
    cli::cli_abort("{.arg startDate} must be a Date or date-like value.")
  }
  if (!is.Date(endDate)) {
    cli::cli_abort("{.arg endDate} must be a Date or date-like value.")
  }
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)

  for (day in seq.Date(startDate, endDate, by = 1)) {
    d <- as.Date(day, origin = "1970-01-01")
    if (nrow(schedule[schedule$Date == d, ]) == 0) {
      next # no games that day, just skip it.
    }
    message("Results as of: ", d)
    score <- scores[scores$Date < day, ]
    score <- score[score$Date > (as.Date(startDate) - 4000), ] # only feed in ~ 11 years data to calculate m & rho
    sched <- schedule[schedule$Date == d, ]
    params <- list()
    params$m <- getM(scores = score, currentDate = d)
    params$rho <- getRho(m = params$m, scores = score)
    w.day <- getWeibullParams(m = params$m, rho = params$rho, scores = score)
    params$beta <- w.day$beta
    params$eta <- w.day$eta
    params$k <- w.day$k

    recordTodaysPredictions(
      today = d,
      filepath = filepath,
      schedule = sched,
      params = params,
      include_xG = include_xG,
      draws = draws
    )
  }
  cleanupPredictionsFile(filepath = filepath)
  return(TRUE)
}
