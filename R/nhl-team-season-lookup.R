# Season date/type lookup and team name/conference/division helper functions

#' getSeasonStartDate
#'
#' @param season Season (8 character code) or NULL for current/most recent season
#'
#' @return Season start date as date
#' @export
getSeasonStartDate <- function(season = NULL) {
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) |>
    httr2::req_cache(tempdir()) |>
    httr2::req_retry(max_seconds = 120) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
  seasons <- seasons$data

  if (!is.null(season)) {
    if (season %in% seasons$id) {
      return(as.Date(seasons[seasons$id == season, ]$startDate))
    } else {
      stop("Season not found: ", season)
    }
  } else {
    return(as.Date(utils::tail(seasons$startDate, 1)))
  }
}


#' Get the latest NHL season ID
#'
#' @returns (`character(1)`) Season ID in eight-digit format such as
#'   `"20172018"`.
#' @export
getCurrentSeason8 <- function() {
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) |>
    httr2::req_cache(tempdir()) |>
    httr2::req_retry(max_seconds = 120) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
  seasons <- seasons$data

  return(as.character(utils::tail(seasons$id, 1)))
}


#' GetCurrentSeasonEndDate
#'
#' @param season Season (8 character code) or NULL for current/most recent season
#'
#' @return Season end date (as date)
#' @export
getSeasonEndDate <- function(season = NULL) {
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) |>
    httr2::req_cache(tempdir()) |>
    httr2::req_retry(max_seconds = 120) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
  seasons <- seasons$data

  if (!is.null(season)) {
    if (season %in% seasons$id) {
      return(as.Date(seasons[seasons$id == season, ]$endDate))
    } else {
      stop("Season not found: ", season)
    }
  } else {
    return(as.Date(utils::tail(seasons$endDate, 1)))
  }
}

#' In Regular Season
#'
#' @param date Date to check if it's in a season
#' @param boolean Whether to return a result as TRUE/FALSE (True) or to return the seasonID if the date is in a season
#'
#' @return Either TRUE/FALSE or a seasonID/FALSE
#' @export
inRegularSeason <- function(date = Sys.Date(), boolean = TRUE) {
  if (!is.Date(date)) {
    cli::cli_abort("{.arg date} must be a Date or date-like value.")
  }
  if (!is.logical(boolean) || length(boolean) != 1 || is.na(boolean)) {
    cli::cli_abort("{.arg boolean} must be a single TRUE/FALSE value.")
  }
  date <- as.Date(date)
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) |>
    httr2::req_cache(tempdir()) |>
    httr2::req_retry(max_seconds = 120) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
  seasons <- seasons$data
  seasons_list <- seasons[
    seasons$startDate <= date & seasons$regularSeasonEndDate >= date,
  ]
  if (boolean) {
    return(ifelse(nrow(seasons_list) > 0, TRUE, FALSE))
  } else {
    if (nrow(seasons_list) > 0) {
      return(as.character(seasons_list$id))
    } else {
      return(FALSE)
    }
  }
}

#' In Off Season
#'
#' @description Determine if a provided date is in the off-season i.e. not in regular season or playoffs
#'
#' @param date Date to check if it's in any off season. Default today
#'
#' @return TRUE if we're in off-season, else FALSE
#' @export
inOffSeason <- function(date = Sys.Date()) {
  if (!is.Date(date)) {
    cli::cli_abort("{.arg date} must be a Date or date-like value.")
  }
  date <- as.Date(date)
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) |>
    httr2::req_cache(tempdir()) |>
    httr2::req_retry(max_seconds = 120) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
  seasons <- seasons$data

  seasons_list <- seasons[seasons$endDate > date & seasons$startDate < date, ]
  return(ifelse(nrow(seasons_list) > 0, FALSE, TRUE))
}

#' In Playoffs
#'
#' @description check if the date is in a playoff period. Note: playoffs are considered from the day after the regular
#' season ends.
#'
#' @param date Date to check if it's in a playoffs period
#' @param boolean Whether to return a result as TRUE/FALSE (True) or to return the seasonID if the date is in a playoffs
#'
#' @return Either TRUE/FALSE or a seasonID/FALSE
#' @export
inPlayoffs <- function(date = Sys.Date(), boolean = TRUE) {
  if (!is.Date(date)) {
    cli::cli_abort("{.arg date} must be a Date or date-like value.")
  }
  date <- as.Date(date)
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) |>
    httr2::req_cache(tempdir()) |>
    httr2::req_retry(max_seconds = 120) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
  seasons <- seasons$data
  seasons_list <- seasons[
    as.Date(seasons$regularSeasonEndDate) <= date &
      as.Date(seasons$endDate) >= date,
  ]
  if (boolean) {
    return(ifelse(nrow(seasons_list) > 0, TRUE, FALSE))
  } else {
    if (nrow(seasons_list) > 0) {
      return(as.character(seasons_list$id))
    } else {
      return(FALSE)
    }
  }
}

#' Get Season from Game Date
#'
#' @param gamedate The date of the game to check for season
#'
#' @return a character season id (e.g. 20172018)
#' @export
getSeason <- function(gamedate = Sys.Date()) {
  if (!is.Date(gamedate)) {
    cli::cli_abort("{.arg gamedate} must be a Date or date-like value.")
  }
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) |>
    httr2::req_cache(tempdir()) |>
    httr2::req_retry(max_seconds = 120) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
  seasons <- seasons$data
  gs <- function(gd, seasons) {
    gd <- as.Date(gd)
    season_list <- seasons[seasons$startDate <= gd & seasons$endDate >= gd, ]
    if (nrow(season_list) == 1) {
      return(as.character(season_list$id))
    } else {
      return(NULL)
    }
  }
  vgs <- Vectorize(FUN = gs, vectorize.args = c("gd"))

  if (length(gamedate) == 1) {
    return(gs(gd = gamedate, seasons = seasons))
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
getConferences <- function(teamColours = HockeyModel::teamColours) {
  return(unique(teamColours$Conference))
}

#' Get division names from the team lookup table
#'
#' @param teamColours (`data.frame`) Team metadata table with a `Division`
#'   column.
#' @returns (`character`) Unique division names.
#' @keywords internal
getDivisions <- function(teamColours = HockeyModel::teamColours) {
  return(unique(teamColours$Division))
}

#' Get conferences for one or more teams
#'
#' @param teams (`character`) Team names.
#' @param teamColours (`data.frame`) Team metadata table.
#' @returns (`character`) Conference name for each input team.
#' @keywords internal
getTeamConferences <- function(teams, teamColours = HockeyModel::teamColours) {
  getteamconf <- function(t, teamColours = HockeyModel::teamColours) {
    return(teamColours[teamColours$Team == t, ]$Conference)
  }

  v_getteamconf <- Vectorize(getteamconf, "t")
  teams <- clean_names(teams)
  if (length(teams) == 1) {
    return(getteamconf(t = teams, teamColours = teamColours))
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
getTeamDivisions <- function(teams, teamColours = HockeyModel::teamColours) {
  getteamdiv <- function(t, teamColours = HockeyModel::teamColours) {
    return(teamColours[teamColours$Team == t, ]$Division)
  }

  v_getteamdiv <- Vectorize(getteamdiv, "t")
  teams <- clean_names(teams)
  if (length(teams) == 1) {
    return(getteamdiv(t = teams, teamColours = teamColours))
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
getShortTeam <- function(teams, teamColours = HockeyModel::teamColours) {
  getteamshort <- function(t) {
    if (t %in% teamColours$Team) {
      return(teamColours[teamColours$Team == t, ]$ShortCode)
    } else {
      return(NA_character_)
    }
  }

  v_getteamshort <- Vectorize(getteamshort, "t")
  teams <- clean_names(teams)
  if (length(teams) == 1) {
    return(getteamshort(t = teams))
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
getLongTeam <- function(teams, teamColours = HockeyModel::teamColours) {
  getteamlong <- function(t) {
    if (t %in% teamColours$ShortCode) {
      return(teamColours[teamColours$ShortCode == t, ]$Team)
    } else {
      return(NA_character_)
    }
  }

  v_getteamlong <- Vectorize(getteamlong, "t")
  teams <- clean_names(teams)
  if (length(teams) == 1) {
    return(getteamlong(t = teams))
  } else {
    return(unname(v_getteamlong(t = teams)))
  }
}

#' Get the regular-season game count for a season
#'
#' @param season (`character(1)`) Season ID in eight-digit format.
#' @returns (`numeric(1)`) Number of regular-season games per team.
#' @keywords internal
getNumGames <- function(season = getCurrentSeason8()) {
  if (!is.null(season)) {
    if (!seasonValidator(season)) {
      cli::cli_abort(
        "{.arg season} must be a valid NHL season ID (e.g. {.val 20202021})."
      )
    }
  } else {
    stop("Season must be supplied")
  }

  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) |>
    httr2::req_cache(tempdir()) |>
    httr2::req_retry(max_seconds = 120) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
  seasons <- seasons$data

  return(seasons[seasons$id == season, ]$numberOfGames)
}
