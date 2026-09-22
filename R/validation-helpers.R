# Input validation and small data-shaping helpers used across the package

#' Clean a model
#'
#' @description reduce the size of a model for better long-term storage.
#'
#' @param cm model to clean
#'
#' @return a smaller model, ready for saving
cleanModel <- function(cm) {
  # from http://www.win-vector.com/blog/2014/05/trimming-the-fat-from-glm-models-in-r/
  cm$y <- c()
  cm$model <- c()

  cm$residuals <- c()
  cm$effects <- c()
  cm$qr$qr <- c()
  cm$linear.predictors <- c()
  cm$weights <- c()
  cm$prior.weights <- c()

  cm
}


#' Normalize Odds
#'
#' @param odds a vector of odds to normalize
#'
#' @return odds summing to 1
#' @export
normalizeOdds <- function(odds) {
  odds <- unlist(odds)
  odds[odds > 1] <- 1 - 1e-10
  odds[odds < 0] <- 1e-10
  odds <- odds / sum(odds)
  return(odds)
}


#' Get Historical Points for all teams listed in a scores frame
#'
#' @param sc scores frame
#'
#' @returns A tibble with season point totals by team.
historicalPoints <- function(sc) {
  sc <- sc |>
    dplyr::rowwise() |>
    dplyr::mutate(Season = getSeason(.data$Date))

  sc <- droplevels(sc)

  seasons_to_process <- setdiff(unique(sc$Season), "20122013")

  points <- purrr::map(seasons_to_process, function(i) {
    s <- sc[sc$Season == i & sc$GameType == "R", ]
    b <- buildStats(s)
    b$Season <- i
    b[, colnames(b) %in% c("Team", "Season", "Points")]
  }) |>
    dplyr::bind_rows()

  return(points)
}


#' Validate GameID numbers
#'
#' @param gameIDs a single game ID or vector of game IDs
#'
#' @return TRUE if gameIDs is valid, or FALSE if gameIDs is not (or a vector of length of input of TRUE/FALSE
#' @export
gameIDValidator <- function(gameIDs) {
  return(grepl("(19|20)\\d{2}0[1-4][0-1]\\d{3}", gameIDs))
}

#' Is this a Date?
#'
#' @description Is this a date?
#'
#' @param date is this a date?
#'
#' @return is it a date?
#' @export
#'
#' @examples is.Date("2020-12-13")
#' is.Date("bob")
is.Date <- function(date) {
  tryCatch(!is.na(as.Date(date)), error = function(err) {
    FALSE
  })
}

#' Validate an NHL season identifier
#'
#' @param season (`character(1)`) Candidate season ID.
#' @returns (`logical(1)`) `TRUE` when `season` matches `YYYYYYYY` format.
#' @keywords internal
seasonValidator <- function(season) {
  # TODO: Currently 19272099 would pass - make sure the two years are sequential
  if (!is.character(season)) {
    return(FALSE)
  }
  return(grepl("^(19|20)\\d{2}(19|20)\\d{2}$", season))
}


#' Split draw probability into overtime/shootout outcomes
#'
#' @param home_win (`double`) Regulation home-win probabilities.
#' @param away_win (`double`) Regulation away-win probabilities.
#' @param draw (`double`) Draw probabilities before overtime resolution.
#' @returns (`matrix` or `numeric`) Home regulation, home OT/SO, away OT/SO,
#'   and away regulation probabilities.
#' @keywords internal
extraTimeSolver <- function(home_win, away_win, draw) {
  ets <- function(home_win, away_win, draw) {
    homenorm <- normalizeOdds(c(home_win, away_win))[1]
    home_ot <- 0.345 * homenorm + 0.315

    home_draw <- draw * home_ot
    away_draw <- draw * (1 - home_ot)

    return(c(home_win, home_draw, away_draw, away_win))
  }

  v_ets <- Vectorize(ets, )
  if (length(home_win) == 1) {
    return(ets(home_win = home_win, away_win = away_win, draw = draw))
  } else {
    return(t(v_ets(home_win, away_win, draw)))
  }
}


#' Add Postponed Games to Schedule End
#'
#' @description Sometimes games are postponed without a makeup date initially announced. The model just drops those games if this function is not employed to move the games to the end of the season
#' Note that the games are all dumped on one day so it doesn't account for back to back or travel days or anything.
#'
#' @param schedule the schedule to reconfigure
#'
#' @return a schedule with postponed games moved to the end of the schedule - helps to not drop games that are otherwise in the past but weren't played.
add_postponed_to_schedule_end <- function(schedule = HockeyModel::schedule) {
  if (!any(schedule$GameStatus == "Postponed")) {
    # no postponed games
    return(schedule)
  } else if (
    all(schedule[schedule$GameStatus == "Postponed", "Date"] > Sys.Date())
  ) {
    # all game postponements are in future games - just play them.
    return(schedule)
  }

  for (g in schedule[
    schedule$GameStatus == "Postponed" & schedule$Date < Sys.Date(),
  ]$GameID) {
    # The model doesn't (currently) account for what games are back to back or anything - so they can all be played on the same (last) date of the schedule
    # Using the last date of the regular season to not interfere with playoffs
    schedule[schedule$GameID == g, ]$Date <- max(
      max(schedule[schedule$GameType == "R", ]$Date),
      Sys.Date()
    )
  }
  schedule <- schedule |>
    dplyr::arrange(.data$Date, .data$GameID)
  return(schedule)
}
