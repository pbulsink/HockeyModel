# Input validation and small data-shaping helpers used across the package

#' Clean a model
#'
#' @description reduce the size of a model for better long-term storage.
#'
#' @param cm model to clean
#'
#' @return a smaller model, ready for saving
.cleanModel <- function(cm) {
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
#' @keywords internal
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
.validateProbMatrix <- function(
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
.historicalPoints <- function(sc) {
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
#' @keywords internal
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
#' @keywords internal
#'
#' @examples \dontrun{
#' is.Date("2020-12-13")
#' is.Date("bob")
#' }
is.Date <- function(date) {
  # A bare numeric (e.g. 12345) is not a valid date-like value, even though
  # as.Date() would silently interpret it as a days-since-epoch offset.
  if (is.numeric(date) && !inherits(date, c("Date", "POSIXt"))) {
    return(FALSE)
  }
  tryCatch(!is.na(as.Date(date)), error = function(err) {
    FALSE
  })
}

#' Validate an NHL season identifier
#'
#' @param season (`character(1)`) Candidate season ID.
#' @returns (`logical(1)`) `TRUE` when `season` matches `YYYYYYYY` format.
#' @keywords internal
.seasonValidator <- function(season) {
  # TODO: Currently 19272099 would pass - make sure the two years are sequential
  if (!is.character(season)) {
    return(FALSE)
  }
  return(grepl("^(19|20)\\d{2}(19|20)\\d{2}$", season))
}


#' Coerce a date-like value to `Date`
#'
#' @description Accepts a single `Date`, `POSIXt`, or character string
#'   parseable by [as.Date()], and returns it as `Date`. Anything else
#'   (including bare numerics, which would otherwise be silently read as days
#'   since the epoch) raises an error. Shared by the league season-state
#'   internals so date validation lives in one place.
#'
#' @param date (`Date` or `character(1)`) Date to check. Defaults to [Sys.Date()].
#' @returns (`Date`) The coerced date.
#' @keywords internal
.coerce_date <- function(date = Sys.Date()) {
  # Check length first so vector input errors cleanly instead of inside is.Date()
  if (length(date) != 1L || !is.Date(date)) {
    cli::cli_abort("{.arg date} must be a Date or date-like value.")
  }
  suppressWarnings(as.Date(date))
}


#' Validate a single logical flag argument
#'
#' @description Shared by the league season-state internals so `boolean`
#'   validation lives in one place.
#'
#' @param boolean (`logical(1)`) Value to validate.
#' @returns (`logical(1)`) The validated value, invisibly.
#' @keywords internal
.validate_boolean <- function(boolean) {
  if (!is.logical(boolean) || length(boolean) != 1L || is.na(boolean)) {
    cli::cli_abort("{.arg boolean} must be a single TRUE/FALSE value.")
  }
  invisible(boolean)
}


#' Split draw probability into overtime/shootout outcomes
#'
#' @param home_win (`double`) Regulation home-win probabilities.
#' @param away_win (`double`) Regulation away-win probabilities.
#' @param draw (`double`) Draw probabilities before overtime resolution.
#' @returns (`matrix` or `numeric`) Home regulation, home OT/SO, away OT/SO,
#'   and away regulation probabilities.
#' @keywords internal
.extraTimeSolver <- function(home_win, away_win, draw) {
  .ets <- function(home_win, away_win, draw) {
    homenorm <- normalizeOdds(c(home_win, away_win))[1]
    home_ot <- 0.345 * homenorm + 0.315

    home_draw <- draw * home_ot
    away_draw <- draw * (1 - home_ot)

    return(c(home_win, home_draw, away_draw, away_win))
  }

  v_ets <- Vectorize(.ets, )
  if (length(home_win) == 1) {
    return(.ets(home_win = home_win, away_win = away_win, draw = draw))
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
.add_postponed_to_schedule_end <- function(schedule = HockeyModel::schedule) {
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

#' Determine back-to-back (b2b) status for a set of games
#'
#' @description A team is on a b2b for a game on date `d` if it played any game
#'   exactly one day earlier (`d - 1`).  Given today's games and a frame of prior
#'   games, returns per-game logical flags for whether the home or away team is
#'   on a back-to-back.  Only rows dated exactly one day before each game are
#'   used; future-dated or older rows are ignored.
#'
#' @param games (`data.frame`) Today's games with `Date`, `HomeTeam`, and
#'   `AwayTeam` columns (one row per game).
#' @param history (`data.frame`) Prior games with `Date`, `HomeTeam`, and
#'   `AwayTeam` columns.  May be empty; only rows dated exactly one day before a
#'   game's date are used to flag that game.
#'
#' @returns (`list`) A named list with elements `home_b2b` and `away_b2b`, each a
#'   logical vector of length `nrow(games)` that is `TRUE` when the team played
#'   exactly one day before its game in `games`.
#' @keywords internal
.b2b_flags <- function(games, history) {
  n <- nrow(games)
  if (n == 0) {
    return(list(home_b2b = logical(0), away_b2b = logical(0)))
  }

  # Union of teams playing on each date in `history`, keyed by that date, so a
  # "who played yesterday?" lookup is O(1) per game instead of rescanning.
  hist_dates <- unique(history$Date)
  teams_by_date <- lapply(
    hist_dates,
    function(dt) {
      rows <- history[history$Date == dt, ]
      c(rows$HomeTeam, rows$AwayTeam)
    }
  )
  names(teams_by_date) <- format(hist_dates)

  .yesterday_teams <- function(dt) {
    key <- format(as.Date(dt) - 1)
    if (is.null(key) || !(key %in% names(teams_by_date))) {
      return(logical(0))
    }
    teams_by_date[[key]]
  }

  home_b2b <- vapply(
    seq_len(n),
    function(i) games$HomeTeam[i] %in% .yesterday_teams(games$Date[i]),
    logical(1)
  )
  away_b2b <- vapply(
    seq_len(n),
    function(i) games$AwayTeam[i] %in% .yesterday_teams(games$Date[i]),
    logical(1)
  )

  return(list(home_b2b = home_b2b, away_b2b = away_b2b))
}
