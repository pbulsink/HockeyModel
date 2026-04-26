# PWHL API Interface
# Functions to fetch schedule and scores from the PWHL HockeyTech API

PWHL_API_BASE <- "https://lscluster.hockeytech.com/feed/index.php"
PWHL_API_KEY <- "446521baf8c38984"
PWHL_API_CLIENT <- "pwhl"

#' Build a PWHL HockeyTech API request
#'
#' @param params Named list of query parameters to append.
#' @returns A prepared [`httr2::request`] object.
#' @keywords internal
pwhl_api_request <- function(params) {
  all_params <- c(
    params,
    list(key = PWHL_API_KEY, client_code = PWHL_API_CLIENT)
  )
  req <- httr2::request(PWHL_API_BASE)
  req <- do.call(httr2::req_url_query, c(list(req), all_params))
  req |>
    httr2::req_cache(tempdir()) |>
    httr2::req_retry(max_seconds = 120)
}


#' Get all PWHL seasons
#'
#' @description Retrieves all PWHL seasons from the HockeyTech API, including
#'   regular seasons, preseason, and playoffs.
#'
#' @returns A data frame with columns `id`, `name`, `shortname`, `career`,
#'   `playoff`, `start_date`, and `end_date`.
#' @export
getPWHLSeasons <- function() {
  resp <- tryCatch(
    pwhl_api_request(list(feed = "modulekit", view = "seasons")) |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON(),
    error = function(e) {
      cli::cli_abort("Failed to retrieve PWHL seasons: {conditionMessage(e)}")
    }
  )

  seasons <- resp$SiteKit$Seasons
  if (is.null(seasons) || nrow(seasons) == 0) {
    cli::cli_abort("No PWHL season data returned from API.")
  }

  data.frame(
    id = as.integer(seasons$season_id),
    name = seasons$season_name,
    shortname = seasons$shortname,
    career = as.integer(seasons$career),
    playoff = as.integer(seasons$playoff),
    start_date = as.Date(seasons$start_date),
    end_date = as.Date(seasons$end_date),
    stringsAsFactors = FALSE
  )
}


#' Get the current PWHL season ID
#'
#' @description Returns the ID of the most recent regular PWHL season. If today
#'   falls within an active season, that season's ID is returned; otherwise the
#'   most recent completed regular season is returned.
#'
#' @returns (`integer(1)`) PWHL season ID.
#' @export
getCurrentPWHLSeason <- function() {
  seasons <- tryCatch(
    getPWHLSeasons(),
    error = function(e) NULL
  )
  if (is.null(seasons)) {
    return(NULL)
  }

  # Use only regular (non-playoff, career-counting) seasons
  reg <- seasons[seasons$career == 1 & seasons$playoff == 0, ]
  if (nrow(reg) == 0) {
    return(NULL)
  }

  today <- Sys.Date()
  active <- reg[reg$start_date <= today & reg$end_date >= today, ]
  if (nrow(active) > 0) {
    return(max(active$id))
  }

  return(max(reg$id))
}


#' Get PWHL Schedule
#'
#' @description Gets the PWHL game schedule for the season requested. Returns
#'   a data frame formatted for use with the rest of the HockeyModel package.
#'
#' @param season (`integer(1)`) PWHL season ID. Defaults to the current season
#'   via `getCurrentPWHLSeason()`.
#' @param pwhlTeamColours (`data.frame`) The built-in PWHL team colours dataset
#'   if not otherwise provided.
#'
#' @returns A data frame with columns `Date`, `HomeTeam`, `AwayTeam`, `GameID`,
#'   `GameType`, and `GameStatus`.
#' @export
getPWHLSchedule <- function(
  season = getCurrentPWHLSeason(),
  pwhlTeamColours = HockeyModel::pwhlTeamColours
) {
  if (is.null(season) || !is.numeric(season) || length(season) != 1) {
    cli::cli_abort("{.arg season} must be a single numeric PWHL season ID.")
  }
  season <- as.integer(season)

  resp <- tryCatch(
    pwhl_api_request(
      list(feed = "modulekit", view = "schedule", season_id = season)
    ) |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON(),
    error = function(e) {
      cli::cli_abort(
        "Failed to retrieve PWHL schedule for season {season}: {conditionMessage(e)}"
      )
    }
  )

  games <- resp$SiteKit$Schedule
  if (is.null(games) || length(games) == 0 || nrow(games) == 0) {
    cli::cli_alert_info("No games found for PWHL season {season}.")
    return(data.frame(
      Date = as.Date(character()),
      HomeTeam = character(),
      AwayTeam = character(),
      GameID = integer(),
      GameType = character(),
      GameStatus = character(),
      stringsAsFactors = FALSE
    ))
  }

  is_playoff <- tryCatch(
    {
      all_seasons <- getPWHLSeasons()
      row <- all_seasons[all_seasons$id == season, ]
      if (nrow(row) == 0) 0L else as.integer(row$playoff)
    },
    error = function(e) 0L
  )

  game_type <- if (isTRUE(is_playoff == 1L)) "P" else "R"

  sched <- data.frame(
    Date = as.Date(games$date_played),
    HomeTeam = pwhl_get_long_team(games$home_team$code, pwhlTeamColours),
    AwayTeam = pwhl_get_long_team(games$visiting_team$code, pwhlTeamColours),
    GameID = as.integer(games$id),
    GameType = game_type,
    GameStatus = ifelse(
      games$game_status == "4",
      "Final",
      ifelse(games$game_status == "1", "Scheduled", games$game_status)
    ),
    stringsAsFactors = FALSE
  )

  sched <- sched[!is.na(sched$HomeTeam) & !is.na(sched$AwayTeam), ]
  sched <- sched[order(sched$Date, sched$GameID), ]
  sched
}


#' Get PWHL Scores
#'
#' @description Gets scores for one or more PWHL games by their game IDs.
#'
#' @param gameIDs (`integer` or `character`) PWHL game ID(s) to retrieve.
#' @param progress (`logical(1)`) Whether to show a progress bar. Requires the
#'   `progress` package.
#'
#' @returns A data frame with columns `Date`, `HomeTeam`, `AwayTeam`, `GameID`,
#'   `HomeGoals`, `AwayGoals`, `OTStatus`, `GameType`, and `GameStatus`.
#' @export
getPWHLScores <- function(
  gameIDs = NULL,
  progress = TRUE
) {
  if (is.null(gameIDs) || length(gameIDs) == 0) {
    cli::cli_abort(
      "No valid {.arg gameIDs} provided to {.fn getPWHLScores}."
    )
  }
  gameIDs <- as.integer(gameIDs)
  gameIDs <- unique(gameIDs[!is.na(gameIDs)])
  if (length(gameIDs) == 0) {
    cli::cli_abort(
      "No valid {.arg gameIDs} provided to {.fn getPWHLScores}."
    )
  }

  if (progress && !requireNamespace("progress", quietly = TRUE)) {
    progress <- FALSE
  }
  if (progress) {
    pb <- progress::progress_bar$new(
      format = "  getting PWHL scores [:bar] :percent eta: :eta",
      total = length(gameIDs),
      show_after = 5
    )
  }

  scores <- NULL
  for (gid in gameIDs) {
    sc <- tryCatch(
      pwhl_game_summary(gid),
      error = function(e) {
        message("Error retrieving PWHL game ", gid, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(sc)) {
      scores <- dplyr::bind_rows(scores, sc)
    }

    if (progress) {
      pb$tick()
    }
  }

  scores
}


#' Resolve a PWHL team name from API city and nickname fields
#'
#' @description Constructs a canonical team name from the city and nickname
#'   fields returned by the HockeyTech game-summary API. First attempts an
#'   ASCII-normalised lookup against `pwhlTeamColours`; falls back to
#'   `"<city> <nickname>"` if no match is found.
#'
#' @param city (`character(1)`) City field from the API (e.g. `"Montr\u00e9al"`).
#' @param nickname (`character(1)`) Nickname field from the API
#'   (e.g. `"Victoire"`).
#' @param pwhlTeamColours (`data.frame`) PWHL team metadata table.
#' @returns (`character(1)`) Canonical team name.
#' @keywords internal
pwhl_resolve_team_name <- function(
  city,
  nickname,
  pwhlTeamColours = HockeyModel::pwhlTeamColours
) {
  candidate <- paste(city, nickname)
  # Normalise accents before lookup (e.g. "Montr\u00e9al" -> "Montreal")
  candidate_ascii <- stringi::stri_trans_general(candidate, "latin-ascii")
  teams_ascii <- stringi::stri_trans_general(
    pwhlTeamColours$Team,
    "latin-ascii"
  )
  idx <- match(candidate_ascii, teams_ascii)
  if (!is.na(idx)) pwhlTeamColours$Team[idx] else candidate
}


#' Fetch a single PWHL game summary
#'
#' @param gid (`integer(1)`) PWHL game ID.
#' @returns A one-row data frame or `NULL` if the game is not final.
#' @keywords internal
pwhl_game_summary <- function(gid) {
  resp <- tryCatch(
    pwhl_api_request(
      list(feed = "gc", tab = "gamesummary", game_id = gid, lang = "en")
    ) |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON(),
    error = function(e) {
      message("Failed to fetch PWHL game ", gid, ": ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(resp)) {
    return(NULL)
  }

  gs <- resp$GC$Gamesummary
  if (is.null(gs)) {
    return(NULL)
  }

  meta <- gs$meta
  # Only process finished games
  if (is.null(meta) || is.null(meta$status) || meta$status != "4") {
    return(NULL)
  }

  scoring <- gs$scoring
  home_goals <- if (!is.null(scoring)) {
    sum(as.integer(scoring$home[!is.na(scoring$home)]))
  } else {
    as.integer(meta$homeGoals)
  }
  away_goals <- if (!is.null(scoring)) {
    sum(as.integer(scoring$visitor[!is.na(scoring$visitor)]))
  } else {
    as.integer(meta$visitingGoals)
  }

  n_periods <- if (!is.null(scoring)) nrow(scoring) else 3L
  ot_status <- if (n_periods == 3L) {
    ""
  } else if (n_periods == 4L) {
    # Check for shootout
    shootout <- gs$shootout
    if (!is.null(shootout) && length(shootout) > 0) "SO" else "OT"
  } else {
    "OT"
  }

  home_team <- pwhl_resolve_team_name(
    meta$HomeCity,
    meta$HomeNickname,
    HockeyModel::pwhlTeamColours
  )
  away_team <- pwhl_resolve_team_name(
    meta$VisitorCity,
    meta$VisitorNickname,
    HockeyModel::pwhlTeamColours
  )

  game_type <- ifelse(
    !is.null(meta$playoff) && meta$playoff == "1",
    "P",
    "R"
  )

  data.frame(
    Date = as.Date(meta$date_played),
    HomeTeam = home_team,
    AwayTeam = away_team,
    GameID = as.integer(gid),
    HomeGoals = as.integer(home_goals),
    AwayGoals = as.integer(away_goals),
    OTStatus = ot_status,
    GameType = game_type,
    GameStatus = "Final",
    stringsAsFactors = FALSE
  )
}


#' Get today's PWHL games
#'
#' @description Returns PWHL games scheduled for a given date, or `NULL` if
#'   there are none.
#'
#' @param schedule (`data.frame`) PWHL schedule, defaults to
#'   [HockeyModel::pwhlSchedule].
#' @param date (`Date`) The date to look up. Defaults to today.
#'
#' @returns A data frame of scheduled PWHL games for that date, or `NULL`.
#' @export
pwhl_games_today <- function(
  schedule = HockeyModel::pwhlSchedule,
  date = Sys.Date()
) {
  if (!inherits(date, "Date")) {
    cli::cli_abort("{.arg date} must be a Date or date-like value.")
  }
  date <- as.Date(date)
  games <- schedule[
    !is.na(schedule$Date) &
      schedule$Date == date &
      schedule$GameStatus %in% c("Scheduled", "1"),
  ]
  if (nrow(games) == 0) {
    return(NULL)
  }
  games
}


#' Update PWHL schedule from the API
#'
#' @description Fetches the current PWHL season schedule and optionally saves
#'   it as package data.
#'
#' @param save_data (`logical(1)`) Whether to write the result to package data
#'   via `usethis`.
#'
#' @returns A data frame of the PWHL schedule.
#' @export
updatePWHLScheduleAPI <- function(save_data = FALSE) {
  season <- getCurrentPWHLSeason()
  if (is.null(season)) {
    cli::cli_abort("Could not determine the current PWHL season.")
  }
  pwhlSchedule <- getPWHLSchedule(season)
  if (is.null(pwhlSchedule) || nrow(pwhlSchedule) == 0) {
    cli::cli_abort("Failed to retrieve PWHL schedule from API.")
  }

  if (save_data && requireNamespace("usethis", quietly = TRUE)) {
    suppressMessages(usethis::use_data(pwhlSchedule, overwrite = TRUE))
  }
  return(pwhlSchedule)
}


#' Update PWHL scores from the API
#'
#' @description Retrieves scores for PWHL games that are in the schedule but
#'   not yet in the scores data frame, then optionally saves the result.
#'
#' @param pwhlScores (`data.frame`) Existing PWHL scores.
#' @param schedule (`data.frame`) PWHL schedule used to determine which games
#'   need scores.
#' @param full_season (`logical(1)`) If `TRUE`, re-fetches all games in the
#'   schedule; otherwise only fetches new games.
#' @param save_data (`logical(1)`) Whether to write updated scores to package
#'   data.
#'
#' @returns Updated PWHL scores data frame.
#' @export
updatePWHLScoresAPI <- function(
  pwhlScores = HockeyModel::pwhlScores,
  schedule = HockeyModel::pwhlSchedule,
  full_season = FALSE,
  save_data = FALSE
) {
  if (nrow(schedule) == 0) {
    cli::cli_alert_info("PWHL schedule is empty \u2014 nothing to update.")
    return(unique(pwhlScores))
  }

  if (full_season) {
    needed <- schedule[schedule$Date >= min(schedule$Date), ]$GameID
  } else {
    needed <- schedule[schedule$Date < Sys.Date(), ]$GameID
    needed <- needed[
      !needed %in%
        pwhlScores[
          pwhlScores$GameStatus == "Final",
        ]$GameID
    ]
  }

  if (length(needed) > 0) {
    updated <- getPWHLScores(needed)
    if (!is.null(updated) && nrow(updated) > 0) {
      pwhlScores <- pwhlScores |>
        dplyr::filter(!(.data$GameID %in% needed)) |>
        dplyr::bind_rows(updated) |>
        dplyr::mutate(
          Date = as.Date(.data$Date),
          GameID = as.integer(.data$GameID)
        ) |>
        dplyr::arrange(.data$Date, .data$GameStatus, .data$GameID)

      if (save_data && requireNamespace("usethis", quietly = TRUE)) {
        suppressMessages(usethis::use_data(pwhlScores, overwrite = TRUE))
      }
    }
  } else {
    cli::cli_alert_info("PWHL scores are already up to date.")
  }

  unique(pwhlScores)
}


#' Convert PWHL short team codes to full team names
#'
#' @param codes (`character`) PWHL team short code(s).
#' @param pwhlTeamColours (`data.frame`) PWHL team metadata table.
#' @returns (`character`) Full team name(s).
#' @keywords internal
pwhl_get_long_team <- function(
  codes,
  pwhlTeamColours = HockeyModel::pwhlTeamColours
) {
  get_one <- function(code) {
    row <- pwhlTeamColours[pwhlTeamColours$ShortCode == code, ]
    if (nrow(row) == 0) {
      return(NA_character_)
    }
    row$Team
  }
  vapply(codes, get_one, character(1L), USE.NAMES = FALSE)
}


#' Convert PWHL full team names to short codes
#'
#' @param teams (`character`) Full PWHL team name(s).
#' @param pwhlTeamColours (`data.frame`) PWHL team metadata table.
#' @returns (`character`) Short team code(s).
#' @keywords internal
pwhl_get_short_team <- function(
  teams,
  pwhlTeamColours = HockeyModel::pwhlTeamColours
) {
  get_one <- function(team) {
    row <- pwhlTeamColours[pwhlTeamColours$Team == team, ]
    if (nrow(row) == 0) {
      return(NA_character_)
    }
    row$ShortCode
  }
  vapply(teams, get_one, character(1L), USE.NAMES = FALSE)
}


#' Rebuild `pwhlTeamColours` package data from source CSV
#'
#' @returns `NULL` (invisibly). Writes updated data when `usethis` is
#'   available.
#' @keywords internal
buildPWHLTeamColours <- function() {
  pwhlTeamColours <- utils::read.csv(
    "./data-raw/logos/pwhl_team_colours.csv",
    stringsAsFactors = FALSE
  )
  if (requireNamespace("usethis", quietly = TRUE)) {
    usethis::use_data(pwhlTeamColours, overwrite = TRUE)
  } else {
    warning(
      "Can't write pwhlTeamColours to file, usethis package must be installed."
    )
  }
  invisible(NULL)
}
