# Season date/type lookup and team name/conference/division helper functions

# Session-level cache for NHL season metadata (#87): repeated lookups within a
# session reuse one API fetch instead of each function re-fetching on its own.
# Successful fetches only are cached, so a transient failure does not pin the
# offline state for the rest of the session.
nhl_seasons_cache <- new.env()

# The cache is disabled under testthat: VCR cassettes and mocked bindings each
# expect their own response, and a shared in-session value would leak across
# tests (and some fixtures disagree about which season is "latest").
.nhl_seasons_caching_disabled <- function() {
  nzchar(Sys.getenv("TESTTHAT"))
}

#' getSeasonStartDate
#'
#' @param season Season (8 character code) or NULL for current/most recent season
#' @param schedule NHL schedule used to derive dates when the API is
#'   unavailable. Defaults to [HockeyModel::schedule].
#'
#' @return Season start date as date
#' @export
getSeasonStartDate <- function(
  season = NULL,
  schedule = HockeyModel::schedule
) {
  seasons <- .fetch_nhl_seasons()

  if (!is.null(seasons) && nrow(seasons) > 0) {
    if (is.null(season)) {
      return(as.Date(utils::tail(seasons$startDate, 1)))
    }
    if (season %in% seasons$id) {
      return(as.Date(seasons[seasons$id == season, ]$startDate))
    }
    # The API knows no such season; fall through to the local schedule below.
  }

  dates <- .nhl_season_dates_from_schedule(season, schedule)
  if (is.null(dates)) {
    cli::cli_abort(
      "Season start date for {.val {season}} unavailable: no network access and no games for that season were found in the local schedule."
    )
  }

  dates$start
}


#' Get the latest NHL season ID
#'
#' @param schedule NHL schedule used to derive the current season when the API
#'   is unavailable. Defaults to [HockeyModel::schedule].
#'
#' @returns (`character(1)`) Season ID in eight-digit format such as
#'   `"20172018"`.
#' @keywords internal
getCurrentSeason8 <- function(schedule = HockeyModel::schedule) {
  seasons <- .fetch_nhl_seasons()

  if (!is.null(seasons) && nrow(seasons) > 0) {
    return(as.character(utils::tail(seasons$id, 1)))
  }

  # Offline fallback: the latest season present in the local schedule.
  dates <- .nhl_season_dates_from_schedule(NULL, schedule)
  if (is.null(dates)) {
    cli::cli_abort(
      "Cannot determine the current season: no network access and no games found in the local schedule."
    )
  }

  paste0(dates$year, dates$year + 1L)
}


#' GetCurrentSeasonEndDate
#'
#' @param season Season (8 character code) or NULL for current/most recent season
#' @param schedule NHL schedule used to derive dates when the API is
#'   unavailable. Defaults to [HockeyModel::schedule].
#'
#' @return Season end date (as date)
#' @export
getSeasonEndDate <- function(season = NULL, schedule = HockeyModel::schedule) {
  seasons <- .fetch_nhl_seasons()

  if (!is.null(seasons) && nrow(seasons) > 0) {
    if (is.null(season)) {
      return(as.Date(utils::tail(seasons$endDate, 1)))
    }
    if (season %in% seasons$id) {
      return(as.Date(seasons[seasons$id == season, ]$endDate))
    }
    # The API knows no such season; fall through to the local schedule below.
  }

  dates <- .nhl_season_dates_from_schedule(season, schedule)
  if (is.null(dates)) {
    cli::cli_abort(
      "Season end date for {.val {season}} unavailable: no network access and no games for that season were found in the local schedule."
    )
  }

  dates$end
}

#' Fetch NHL season metadata from the NHL statistics API
#'
#' @description Retrieves all NHL seasons from
#'   `https://api.nhle.com/stats/rest/en/season`. Results are cached for the
#'   session (#87) so repeated lookups do not re-fetch independently. Returns
#'   `NULL` when the request fails so callers can fall back to schedule-based
#'   checks; failures are not cached, so a recovered connection is picked up on
#'   the next call.
#'
#' @returns A data frame of season metadata, or `NULL` on failure.
#' @keywords internal
.fetch_nhl_seasons <- function() {
  # Caching is off under testthat so each call (VCR cassette or mocked HTTP
  # seam) resolves independently.
  if (!.nhl_seasons_caching_disabled() && !is.null(nhl_seasons_cache$data)) {
    return(nhl_seasons_cache$data)
  }

  seasons <- .fetch_nhl_seasons_http()

  # Only successful fetches are cached, so a transient failure does not pin
  # the offline state for the rest of the session.
  if (!is.null(seasons)) {
    nhl_seasons_cache$data <- seasons
  }

  seasons
}

#' Fetch NHL season metadata directly from the API (no caching)
#'
#' @description The uncached HTTP fetch behind [.fetch_nhl_seasons()]. Split
#'   out so the session-level cache can be tested against a counting stub.
#'
#' @returns A data frame of season metadata, or `NULL` on failure.
#' @keywords internal
.fetch_nhl_seasons_http <- function() {
  tryCatch(
    {
      url <- "https://api.nhle.com/stats/rest/en/season"
      resp <- httr2::request(url) |>
        httr2::req_cache(tempdir()) |>
        httr2::req_retry(max_seconds = 120) |>
        httr2::req_perform() |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON()
      resp$data
    },
    error = function(e) NULL
  )
}

#' Derive a season's dates from the local schedule (offline fallback, #87)
#'
#' @description Used when the NHL statistics API is unavailable: derives the
#'   requested season's start and end dates from `schedule` game dates, keyed
#'   by the year prefix of each `GameID`. When `season` is `NULL`, the latest
#'   season present in the schedule is used.
#'
#' @param season (`character(1)` or `NULL`) Season ID in eight-digit format.
#' @param schedule (`data.frame`) Schedule with `Date` and `GameID` columns.
#' @returns A list with elements `start`, `end` (each a `Date`), and `year`
#'   (`integer(1)`), or `NULL` when the schedule holds no games for that season.
#' @keywords internal
.nhl_season_dates_from_schedule <- function(season, schedule) {
  if (!is.data.frame(schedule) || nrow(schedule) == 0) {
    return(NULL)
  }

  years <- as.integer(substr(as.character(schedule$GameID), 1, 4))
  valid <- !is.na(years) & !is.na(schedule$Date)
  if (!any(valid)) {
    return(NULL)
  }

  if (is.null(season)) {
    years_valid <- max(years[valid])
  } else if (.seasonValidator(season)) {
    years_valid <- as.integer(substr(season, 1, 4))
  } else {
    cli::cli_abort(
      "{.arg season} must be a valid NHL season ID (e.g. {.val 20202021})."
    )
  }

  in_season <- valid & years == years_valid
  if (!any(in_season)) {
    return(NULL)
  }

  dates <- schedule$Date[in_season]
  list(start = min(dates), end = max(dates), year = years_valid)
}

#' Derive an NHL season ID from a calendar date
#'
#' @description Approximates the NHL season ID for `date` using the fact that
#'   NHL regular seasons run from October through June: dates in October or
#'   later belong to the season starting that year, earlier dates to the
#'   previous year. Used only as a fallback when the NHL API is unavailable.
#'
#' @param date (`Date`) Date within an NHL season window.
#' @returns (`character(1)`) Season ID in eight-digit format such as `"20182019"`.
#' @keywords internal
.nhl_season_id_from_date <- function(date) {
  year <- as.integer(format(date, "%Y"))
  start_year <- if (as.integer(format(date, "%m")) >= 10L) year else year - 1L
  paste0(start_year, start_year + 1L)
}

#' Check if a date falls in an NHL regular season
#'
#' @description Internal league-specific implementation of [inRegularSeason()]
#'   for the NHL. Queries the NHL statistics API and falls back to inspecting
#'   `schedule` when the API is unavailable. When the fallback is used with
#'   `boolean = FALSE`, the season ID is approximated from the calendar date.
#'
#' @param date (`Date` or `character(1)`) Date to check. Defaults to [Sys.Date()].
#' @param boolean (`logical(1)`) When `TRUE`, returns a logical indicator; when
#'   `FALSE`, returns the season ID string if in a regular season, else `FALSE`.
#' @param schedule (`data.frame`) NHL schedule used for the fallback check.
#'   Defaults to [HockeyModel::schedule].
#'
#' @returns `logical(1)`, or a season ID string / `FALSE` when `boolean = FALSE`.
#' @keywords internal
.in_regular_season_nhl <- function(
  date = Sys.Date(),
  boolean = TRUE,
  schedule = HockeyModel::schedule
) {
  date <- .coerce_date(date)
  .validate_boolean(boolean)

  seasons_data <- .fetch_nhl_seasons()
  if (!is.null(seasons_data) && nrow(seasons_data) > 0) {
    match <- seasons_data[
      as.Date(seasons_data$startDate) <= date &
        as.Date(seasons_data$regularSeasonEndDate) >= date,
    ]
    if (nrow(match) > 0) {
      return(if (boolean) TRUE else as.character(match$id))
    }
    return(FALSE)
  }

  reg_games <- schedule[!is.na(schedule$Date) & schedule$GameType == "R", ]
  if (nrow(reg_games) == 0) {
    return(FALSE)
  }
  min_date <- min(reg_games$Date)
  max_date <- max(reg_games$Date)
  if (date < min_date || date > max_date) {
    return(FALSE)
  }
  if (boolean) TRUE else .nhl_season_id_from_date(date)
}

.inRegularSeasonNHL <- .in_regular_season_nhl

#' Check if a date falls in the NHL off-season
#'
#' @description Internal league-specific implementation of [inOffSeason()] for
#'   the NHL. Queries the NHL statistics API and falls back to inspecting
#'   `schedule` when the API is unavailable.
#'
#' @param date (`Date` or `character(1)`) Date to check. Defaults to [Sys.Date()].
#' @param schedule (`data.frame`) NHL schedule used for the fallback check.
#'   Defaults to [HockeyModel::schedule].
#'
#' @returns `logical(1)` indicating whether the date is in the off-season.
#' @keywords internal
.in_off_season_nhl <- function(
  date = Sys.Date(),
  schedule = HockeyModel::schedule
) {
  date <- .coerce_date(date)

  seasons_data <- .fetch_nhl_seasons()
  if (!is.null(seasons_data) && nrow(seasons_data) > 0) {
    in_season <- seasons_data[
      as.Date(seasons_data$startDate) <= date &
        as.Date(seasons_data$endDate) >= date,
    ]
    return(nrow(in_season) == 0)
  }

  if (nrow(schedule) == 0 || all(is.na(schedule$Date))) {
    return(TRUE)
  }
  min_date <- min(schedule$Date, na.rm = TRUE)
  max_date <- max(schedule$Date, na.rm = TRUE)
  date < min_date || date > max_date
}

.inOffSeasonNHL <- .in_off_season_nhl

#' Check if a date falls in the NHL playoffs
#'
#' @description Internal league-specific implementation of [inPlayoffs()] for
#'   the NHL. Playoffs are considered to run from the day after the regular
#'   season ends until the season end date. Queries the NHL statistics API and
#'   falls back to inspecting `schedule` when the API is unavailable. When the
#'   fallback is used with `boolean = FALSE`, the season ID is approximated
#'   from the calendar date.
#'
#' @param date (`Date` or `character(1)`) Date to check. Defaults to [Sys.Date()].
#' @param boolean (`logical(1)`) When `TRUE`, returns a logical indicator; when
#'   `FALSE`, returns the season ID string if in the playoffs, else `FALSE`.
#' @param schedule (`data.frame`) NHL schedule used for the fallback check.
#'   Defaults to [HockeyModel::schedule].
#'
#' @returns `logical(1)`, or a season ID string / `FALSE` when `boolean = FALSE`.
#' @keywords internal
.in_playoffs_nhl <- function(
  date = Sys.Date(),
  boolean = TRUE,
  schedule = HockeyModel::schedule
) {
  date <- .coerce_date(date)
  .validate_boolean(boolean)

  seasons_data <- .fetch_nhl_seasons()
  if (!is.null(seasons_data) && nrow(seasons_data) > 0) {
    match <- seasons_data[
      as.Date(seasons_data$regularSeasonEndDate) <= date &
        as.Date(seasons_data$endDate) >= date,
    ]
    if (nrow(match) > 0) {
      return(if (boolean) TRUE else as.character(match$id))
    }
    return(FALSE)
  }

  playoff_games <- schedule[!is.na(schedule$Date) & schedule$GameType == "P", ]
  if (nrow(playoff_games) == 0) {
    return(FALSE)
  }
  min_date <- min(playoff_games$Date)
  max_date <- max(playoff_games$Date)
  if (date < min_date || date > max_date) {
    return(FALSE)
  }
  if (boolean) TRUE else .nhl_season_id_from_date(date)
}

.inPlayoffsNHL <- .in_playoffs_nhl

#' Get Season from Game Date
#'
#' @param gamedate The date of the game to check for season
#' @param schedule NHL schedule used to derive the season when the API is
#'   unavailable. Defaults to [HockeyModel::schedule].
#'
#' @return a character season id (e.g. 20172018)
#' @export
getSeason <- function(gamedate = Sys.Date(), schedule = HockeyModel::schedule) {
  if (!is.Date(gamedate)) {
    cli::cli_abort("{.arg gamedate} must be a Date or date-like value.")
  }
  seasons <- .fetch_nhl_seasons()

  # Offline fallback: match the date against every season present in the local
  # schedule (one season per year prefix of GameID, spanning start to end).
  if (is.null(seasons) || nrow(seasons) == 0) {
    years <- sort(unique(as.integer(substr(
      as.character(schedule$GameID),
      1,
      4
    ))))
    seasons <- NULL
    for (y in years[!is.na(years)]) {
      dates <- .nhl_season_dates_from_schedule(
        paste0(y, y + 1L),
        schedule
      )
      if (!is.null(dates)) {
        seasons <- dplyr::bind_rows(
          seasons,
          data.frame(
            id = paste0(y, y + 1L),
            startDate = dates$start,
            endDate = dates$end
          )
        )
      }
    }
    if (nrow(seasons) == 0) {
      return(NULL)
    }
  }

  .gs <- function(gd, seasons) {
    gd <- as.Date(gd)
    season_list <- seasons[seasons$startDate <= gd & seasons$endDate >= gd, ]
    if (nrow(season_list) == 1) {
      return(as.character(season_list$id))
    } else {
      return(NULL)
    }
  }
  vgs <- Vectorize(FUN = .gs, vectorize.args = c("gd"))

  if (length(gamedate) == 1) {
    return(.gs(gd = gamedate, seasons = seasons))
  } else if (length(gamedate) > 1) {
    return(unname(vgs(gd = gamedate, seasons = seasons)))
  }
}


#' Get conference names from the team lookup table
#'
#' @param teamColours (`data.frame`) Team metadata table with a `Conference`
#'   column.
#' @returns (`character`) Unique conference names.
#' @keywords internal
.getConferences <- function(teamColours = HockeyModel::teamColours) {
  return(unique(teamColours$Conference))
}

#' Get division names from the team lookup table
#'
#' @param teamColours (`data.frame`) Team metadata table with a `Division`
#'   column.
#' @returns (`character`) Unique division names.
#' @keywords internal
.getDivisions <- function(teamColours = HockeyModel::teamColours) {
  return(unique(teamColours$Division))
}

#' Get conferences for one or more teams
#'
#' @param teams (`character`) Team names.
#' @param teamColours (`data.frame`) Team metadata table.
#' @returns (`character`) Conference name for each input team.
#' @keywords internal
.getTeamConferences <- function(teams, teamColours = HockeyModel::teamColours) {
  .getteamconf <- function(t, teamColours = HockeyModel::teamColours) {
    return(teamColours[teamColours$Team == t, ]$Conference)
  }

  v_getteamconf <- Vectorize(.getteamconf, "t")
  teams <- .clean_names(teams)
  if (length(teams) == 1) {
    return(.getteamconf(t = teams, teamColours = teamColours))
  } else {
    return(unname(v_getteamconf(t = teams, teamColours = teamColours)))
  }
}

#' Get divisions for one or more teams
#'
#' @param teams (`character`) Team names.
#' @param teamColours (`data.frame`) Team metadata table.
#' @returns (`character`) Division name for each input team.
#' @keywords internal
.getTeamDivisions <- function(teams, teamColours = HockeyModel::teamColours) {
  .getteamdiv <- function(t, teamColours = HockeyModel::teamColours) {
    return(teamColours[teamColours$Team == t, ]$Division)
  }

  v_getteamdiv <- Vectorize(.getteamdiv, "t")
  teams <- .clean_names(teams)
  if (length(teams) == 1) {
    return(.getteamdiv(t = teams, teamColours = teamColours))
  } else {
    return(unname(v_getteamdiv(t = teams, teamColours = teamColours)))
  }
}

#' Convert long team names to short codes
#'
#' @param teams (`character`) Long-form team names.
#' @param teamColours (`data.frame`) Team metadata table.
#' @returns (`character`) Team short code for each input team.
#' @keywords internal
.getShortTeam <- function(teams, teamColours = HockeyModel::teamColours) {
  .getteamshort <- function(t) {
    if (t %in% teamColours$Team) {
      return(teamColours[teamColours$Team == t, ]$ShortCode)
    } else {
      return(NA_character_)
    }
  }

  v_getteamshort <- Vectorize(.getteamshort, "t")
  teams <- .clean_names(teams)
  if (length(teams) == 1) {
    return(.getteamshort(t = teams))
  } else {
    return(unname(v_getteamshort(t = teams)))
  }
}

#' Convert short team codes to long names
#'
#' @param teams (`character`) Team short codes.
#' @param teamColours (`data.frame`) Team metadata table.
#' @returns (`character`) Long-form team name for each input code.
#' @keywords internal
.getLongTeam <- function(teams, teamColours = HockeyModel::teamColours) {
  .getteamlong <- function(t) {
    if (t %in% teamColours$ShortCode) {
      return(teamColours[teamColours$ShortCode == t, ]$Team)
    } else {
      return(NA_character_)
    }
  }

  v_getteamlong <- Vectorize(.getteamlong, "t")
  teams <- .clean_names(teams)
  if (length(teams) == 1) {
    return(.getteamlong(t = teams))
  } else {
    return(unname(v_getteamlong(t = teams)))
  }
}

#' Get the regular-season game count for a season
#'
#' @param season (`character(1)`) Season ID in eight-digit format.
#' @returns (`numeric(1)`) Number of regular-season games per team.
#' @keywords internal
.getNumGames <- function(season = getCurrentSeason8()) {
  if (!is.null(season)) {
    if (!.seasonValidator(season)) {
      cli::cli_abort(
        "{.arg season} must be a valid NHL season ID (e.g. {.val 20202021})."
      )
    }
  } else {
    stop("Season must be supplied")
  }

  # Cached session fetch (#87): no independent re-fetch per call.
  seasons <- .fetch_nhl_seasons()
  if (is.null(seasons) || nrow(seasons) == 0) {
    cli::cli_abort(
      "Cannot determine the game count for {.val {season}}: no network access and season metadata is unavailable."
    )
  }

  return(seasons[seasons$id == season, ]$numberOfGames)
}
