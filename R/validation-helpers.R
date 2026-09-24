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


#' Validate a Dixon-Coles probability matrix or result-probability vector
#'
#' @description Checks that all entries are finite, within tolerance of
#'   `[0, 1]`, and sum to 1 (within tolerance). Small negative/over-1 values
#'   are clamped silently -- these arise both from floating point error and
#'   from the Dixon-Coles tau adjustment itself, which can push a boundary
#'   cell (e.g. 0-0/1-0 goals) slightly negative for some `rho`/lambda/mu
#'   combinations (see Dixon & Coles 1997). Larger violations raise an error
#'   since they indicate a modeling bug (e.g. a mis-normalized matrix) rather
#'   than an expected small artifact.
#'
#' @param x (`numeric`) A probability matrix or vector, expected to sum to 1.
#' @param tol (`double(1)`) Tolerance for the sum-to-1 check.
#' @param clamp_tol (`double(1)`) Magnitude of out-of-range values that are
#'   silently clamped into `[0, 1]` rather than raising an error.
#' @param context (`character(1)`) Label used in error messages to identify
#'   the caller.
#' @returns `x`, with any within-tolerance out-of-range values clamped into
#'   `[0, 1]`.
#' @keywords internal
validateProbMatrix <- function(
  x,
  tol = 1e-6,
  clamp_tol = 1e-3,
  context = "probability"
) {
  if (any(!is.finite(x))) {
    stop(context, ": contains non-finite values (NA/NaN/Inf).")
  }

  out_of_range <- x < -clamp_tol | x > 1 + clamp_tol
  if (any(out_of_range)) {
    stop(
      context,
      ": contains invalid probabilities outside [0, 1] (min=",
      signif(min(x), 4),
      ", max=",
      signif(max(x), 4),
      ")."
    )
  }
  # Clamp small negative/over-1 values (floating point noise, or the known
  # small negative artifacts from the tau adjustment) into range.
  x[x < 0] <- 0
  x[x > 1] <- 1

  total <- sum(x)
  if (abs(total - 1) > max(tol, clamp_tol)) {
    stop(
      context,
      ": probabilities sum to ",
      signif(total, 8),
      ", expected 1."
    )
  }
  # Renormalize after clamping so the returned probabilities sum to exactly 1;
  # clamping alone (without this) can leave the sum off by up to `clamp_tol`.
  x / total
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
