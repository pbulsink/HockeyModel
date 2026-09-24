# NHL API functions for fetching schedules, scores, xG data, and boxscores

#' Get NHL Schedule
#'
#' @description Gets the NHL schedule from the NHL API for the season(s) requested. Returns data formatted for further use. This can be slow if requesting many seasons due to the API rate limit.
#'
#' @param season Season(s) for which the schedule is requested, as YYYY format. Defaults to current season. Accepts single years, ranges e.g.c(2010:2015) or complex ranges e.g. c(2010:2015, 2018).
#' @param teamColours the built-in teamColours data set if not otherwise provided.
#'
#' @return a data frame of all scheduled games for the season(s) requested, with Date, HomeTeam, AwayTeam, GameID, and GameType.
#' @export
getNHLSchedule <- function(
  season = getCurrentSeason8(),
  teamColours = HockeyModel::teamColours
) {
  if (!seasonValidator(season)) {
    cli::cli_abort(
      "{.arg season} must be a valid NHL season ID (e.g. {.val 20202021})."
    )
  }

  # This is imilar to how Dan Morse did it in hockeyR
  sched <- purrr::map(unique(teamColours$ShortCode), function(i) {
    url <- paste0(
      "https://api-web.nhle.com/v1/club-schedule-season/",
      i,
      "/",
      season
    )

    site <- tryCatch(
      httr2::request(url) |>
        httr2::req_cache(tempdir()) |>
        httr2::req_retry(max_seconds = 120) |>
        httr2::req_perform() |>
        httr2::resp_body_string() |>
        jsonlite::fromJSON(),
      error = function(cond) {
        message(paste0("There was a problem fetching ", i, "'s games: ", cond))
        return(NULL)
      }
    )

    if (!is.null(site) && !is.null(site$games) && length(site$games) > 0) {
      sg <- site$games |>
        dplyr::filter(.data$gameType > 1)

      data.frame(
        Date = sg$gameDate,
        HomeTeam = getLongTeam(sg$homeTeam$abbrev),
        AwayTeam = getLongTeam(sg$awayTeam$abbrev),
        GameID = sg$id,
        GameType = ifelse(
          sg$gameType == 2,
          "R",
          ifelse(sg$gameType == 3, "P", "NA")
        ),
        GameStatus = sg$gameState
      ) |>
        dplyr::filter(.data$GameType %in% c("R", "P"))
    } else {
      NULL
    }
  }) |>
    dplyr::bind_rows()

  sched <- sched |>
    unique() |>
    dplyr::arrange(.data$Date, .data$GameID)

  if (
    nrow(sched[
      sched$GameStatus %in% c("FUT", "PPD") & sched$Date < Sys.Date(),
    ]) >
      0
  ) {
    sched[
      sched$GameStatus %in% c("FUT", "PPD") & sched$Date < Sys.Date(),
    ]$Date <- max(sched$Date)
  }

  return(sched)
}


#' Games Today
#'
#' @description given a schedule, it returns todays (or another date's) scheduled games (excluding postponements),
#' or NULL if there are none
#'
#' @param schedule the schedule within which to look for games
#' @param date the date to look for games, as a date
#' @param all_games whether to return all games scheduled for a date (True) or exclude postponed, rescheduled,
#' in-progress, or completed games (False, default)
#'
#' @return Scheduled games (in the format of the schedule) for the requested date, or NULL if none
#' @export
games_today <- function(
  schedule = HockeyModel::schedule,
  date = Sys.Date(),
  all_games = FALSE
) {
  if (!is.Date(date)) {
    cli::cli_abort("{.arg date} must be a Date or date-like value.")
  }
  url <- paste0(
    "https://api-web.nhle.com/v1/schedule/",
    as.Date(date, "%Y-%m-%d")
  )

  sched <- httr2::request(url) |>
    httr2::req_cache(tempdir()) |>
    httr2::req_retry(max_seconds = 120) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()
  gameWeek <- sched$gameWeek

  if (
    gameWeek[gameWeek$date == as.Date(date, "%Y-%m-%d", ), ]$numberOfGames == 0
  ) {
    return(NULL)
  }

  gids <- gameWeek[gameWeek$date == as.Date(date, "%Y-%m-%d"), ]$games[[1]]$id
  todaygames <- schedule[schedule$GameID %in% gids, ]
  if (nrow(todaygames) == 0) {
    message(
      "Games on today aren't present in Schedule. Be sure schedule is updated!!"
    )
    return(NULL)
  }
  return(todaygames)
}


#' Update schedule using the NHL API
#'
#' @param save_data whether to write to package data
#'
#' @return data frame of schedule, after optionally writing to package data
#' @export
updateScheduleAPI <- function(save_data = FALSE) {
  currentSeason <- getSeason()
  if (is.null(currentSeason)) {
    currentSeason <- getCurrentSeason8()
  }
  schedule <- getNHLSchedule(currentSeason)
  if (is.null(schedule)) {
    cli::cli_abort("Failed to retrieve schedule from NHL API.")
  }

  if (save_data && requireNamespace("usethis", quietly = TRUE)) {
    suppressMessages(usethis::use_data(schedule, overwrite = TRUE))
  }
  return(schedule)
}


#' Get NHL Scores
#'
#' @description Get the NHL game scores from the NHL API for any game(s) with a final score. Returns data formatted
#' for future use. Requires gameID(s) be provided.This can be slow if requesting many games due to the API rate limit.
#'
#' @param gameIDs Game IDs (10 digit number).
#' @param schedule optional, provide a schedule if not using the HockeyModel::schedule
#' @param progress whether to show a progress bar. Requires the 'progress' package installed
#'
#' @return a data frame with Date, HomeTeam, AwayTeam, GameID, HomeGoals, AwayGoals, OTStatus and GameType
#' @export
getNHLScores <- function(
  gameIDs = NULL,
  schedule = HockeyModel::schedule,
  progress = TRUE
) {
  scores <- NULL
  if (is.null(gameIDs)) {
    gameIDs <- schedule[schedule$Date < Sys.Date(), "GameID"]
  }

  gameIDs <- gameIDs[gameIDValidator(gameIDs)]
  if (length(gameIDs) == 0) {
    cli::cli_abort(
      "Error in HockeyModel::getNHLScores. No valid {.param gameIDs} provided."
    )
  }

  if (progress) {
    if (!requireNamespace("progress", quietly = TRUE)) {
      progress <- FALSE
    }
  }
  if (progress) {
    pb <- progress::progress_bar$new(
      format = "  getting scores [:bar] :percent eta: :eta",
      total = length(gameIDs),
      show_after = 5
    )
  }

  for (g in gameIDs) {
    sc <- NA
    tryCatch(
      sc <- nhl_boxscore(g),
      error = function(e) message("Error in GameID", g, ": ", e)
    )

    if (all(is.na(sc)) || "nhl_get_data_error" %in% class(sc[[1]])) {
      next
    }
    if (sc$gameState == "OFF") {
      dfs <- data.frame(
        "Date" = as.Date(sc$gameDate),
        "HomeTeam" = paste(
          sc$homeTeam$placeName[[1]],
          sc$homeTeam$commonName[[1]]
        ),
        "AwayTeam" = paste(
          sc$awayTeam$placeName[[1]],
          sc$awayTeam$commonName[[1]]
        ),
        "GameID" = sc$id,
        "HomeGoals" = sc$homeTeam$score,
        "AwayGoals" = sc$awayTeam$score,
        "OTStatus" = ifelse(
          sc$periodDescriptor$number == 3,
          "",
          sc$periodDescriptor$number
        ),
        "GameType" = ifelse(substr(g, 6, 6) == "2", "R", "P"),
        "GameStatus" = "Final"
      )
      scores <- rbind(scores, dfs)
    } else {
      warning(
        "Game ",
        g,
        " not in final state, instead showing ",
        sc$gameState,
        "\nGame schedule state is ",
        sc$gameScheduleState
      )
      next
    }
    if (progress) {
      pb$tick()
    }
  }

  if (!is.null(scores)) {
    scores <- clean_names(scores)
    if (nrow(scores[scores$OTStatus %in% "3rd", ]) > 0) {
      scores[scores$OTStatus %in% "3rd", ]$OTStatus <- ""
    }
    scores <- scores |>
      dplyr::mutate(
        # SO is checked first: it is a special case of "> 3" and would
        # otherwise always be shadowed by the OT branch below.
        OTStatus = dplyr::case_when(
          .data$OTStatus == 5 & .data$GameType == "R" ~ "SO",
          .data$OTStatus > 3 ~ "OT",
          .data$OTStatus <= 3 ~ "",
          .default = NA_character_
        )
      )

    if (anyNA(scores$OTStatus)) {
      cli::cli_abort(
        "{.fn getNHLScores} encountered unrecognized {.field OTStatus}/{.field GameType} combinations for {.val {scores[is.na(scores$OTStatus), 'GameID']}}."
      )
    }

    # Modern NHL games are always decided (no regulation ties), so a tied
    # score should only ever appear with an empty OTStatus if the boxscore
    # is incomplete/erroneous, and never alongside an OT/SO decision.
    invalid_tie <- scores$HomeGoals == scores$AwayGoals
    if (any(invalid_tie)) {
      cli::cli_abort(
        "{.fn getNHLScores} found tied final scores (games are always decided in regulation, OT, or SO) for {.val {scores[invalid_tie, 'GameID']}}."
      )
    }

    scores <- scores |>
      dplyr::mutate(
        Result = dplyr::case_when(
          (.data$HomeGoals > .data$AwayGoals) & .data$OTStatus == "" ~ 1,
          (.data$HomeGoals < .data$AwayGoals) & .data$OTStatus == "" ~ 0,
          (.data$HomeGoals > .data$AwayGoals) & .data$OTStatus == "OT" ~ 0.75,
          (.data$HomeGoals > .data$AwayGoals) & .data$OTStatus == "SO" ~ 0.6,
          (.data$HomeGoals < .data$AwayGoals) & .data$OTStatus == "SO" ~ 0.4,
          (.data$HomeGoals < .data$AwayGoals) & .data$OTStatus == "OT" ~ 0.25,
          .default = NA_real_
        )
      ) |>
      dplyr::arrange(.data$Date, .data$GameStatus, .data$GameID)

    if (anyNA(scores$Result)) {
      cli::cli_abort(
        "{.fn getNHLScores} produced {.val NA} {.field Result} values for {.val {scores[is.na(scores$Result), 'GameID']}}; check {.field HomeGoals}/{.field AwayGoals}/{.field OTStatus}."
      )
    }
  }

  if (is.null(scores) || nrow(scores) == 0) {
    message("No final scores were retrieved; skipping xG lookup.")
    return(scores)
  }

  message("Now getting natural stat trick xG results")
  scores_xg <- get_xg(gameIds = scores$GameID)
  scores <- dplyr::left_join(scores, scores_xg, by = "GameID")
  return(scores)
}


#' Load or download a Natural Stat Trick game report
#'
#' @param gid (`character(1)` or `numeric(1)`) NHL game ID in ten-digit format.
#' @param cache_path (`character(1)`) Path to the local Natural Stat Trick
#'   report cache CSV. Defaults to the `HockeyModel.nst.cache.path` option,
#'   falling back to `~/Documents/natstattrick.csv` if unset. Exposed as a
#'   parameter so tests can point it at a temporary file.
#' @returns (`data.frame`) Natural Stat Trick report rows for `gid`.
#' @keywords internal
load_or_get_nst <- function(
  gid,
  cache_path = getOption(
    "HockeyModel.nst.cache.path",
    "~/Documents/natstattrick.csv"
  )
) {
  season <- as.numeric(substr(gid, 1, 4))
  game_id <- as.numeric(substr(gid, 5, 10))

  season <- paste0(season, season + 1)
  season <- as.numeric(season)

  if (
    file.exists(cache_path) &&
      system2(
        "grep",
        paste0('-l "', gid, '" ', cache_path),
        stdout = FALSE
      ) ==
        0
  ) {
    nstall <- utils::read.csv(cache_path)
    nstdf <- nstall |>
      dplyr::filter(.data$game_id == gid)
  } else {
    nstdf <- naturalstattrick::nst_report_df(
      season = season,
      game_id = game_id
    )
    nstdf <- nstdf |>
      dplyr::mutate("game_id" = gid)
    # write.table() warns whenever append=TRUE and col.names=TRUE, even if
    # the file doesn't exist yet, so the header row is written separately
    # on first use rather than via append.
    is_new_cache <- !file.exists(cache_path) || file.size(cache_path) == 0
    if (is_new_cache) {
      utils::write.table(
        nstdf,
        file = cache_path,
        append = FALSE,
        row.names = FALSE,
        col.names = TRUE,
        sep = ","
      )
    } else {
      utils::write.table(
        nstdf,
        file = cache_path,
        append = TRUE,
        row.names = FALSE,
        col.names = FALSE,
        sep = ","
      )
    }
  }

  return(nstdf)
}

#' Get xG for one or many gameIds
#'
#' @param gameIds one or many game Ids
#'
#' @return a list with Game ID, HomexG and AwayxG, (plus home and away G, CF, pk G, pk CF, pk xG, pp G, pp CF, pp xG,)
#' if one game ID supplied, or a data frame with those columns
#' @export
get_xg <- function(gameIds) {
  gxg <- function(gid) {
    season <- as.numeric(substr(gid, 1, 4))

    if (season < 2007) {
      return(list("GameID" = gid, "HomexG" = NA, "AwayxG" = NA))
    }

    nst_report <- load_or_get_nst(gid)

    return(list(
      "GameID" = as.integer(gid),
      "HomexG" = as.numeric(nst_report[nst_report$h_a == "home", ]$xgf_all),
      "AwayxG" = as.numeric(nst_report[nst_report$h_a == "away", ]$xgf_all),
      "HomeG" = as.numeric(nst_report[nst_report$h_a == "home", ]$gf_all),
      "AwayG" = as.numeric(nst_report[nst_report$h_a == "away", ]$gf_all),
      "HomeCF" = as.numeric(nst_report[nst_report$h_a == "home", ]$cf_all),
      "AwayCF" = as.numeric(nst_report[nst_report$h_a == "away", ]$cf_all),
      "HomexGpk" = as.numeric(nst_report[nst_report$h_a == "home", ]$xgf_pk),
      "AwayxGpk" = as.numeric(nst_report[nst_report$h_a == "away", ]$xgf_pk),
      "HomeGpk" = as.numeric(nst_report[nst_report$h_a == "home", ]$gf_pk),
      "AwayGpk" = as.numeric(nst_report[nst_report$h_a == "away", ]$gf_pk),
      "HomeCFpk" = as.numeric(nst_report[nst_report$h_a == "home", ]$cf_pk),
      "AwayCFpk" = as.numeric(nst_report[nst_report$h_a == "away", ]$cf_pk),
      "HomexGpp" = as.numeric(nst_report[nst_report$h_a == "home", ]$xgf_pp),
      "AwayxGpp" = as.numeric(nst_report[nst_report$h_a == "away", ]$xgf_pp),
      "HomeGpp" = as.numeric(nst_report[nst_report$h_a == "home", ]$gf_pp),
      "AwayGpp" = as.numeric(nst_report[nst_report$h_a == "away", ]$gf_pp),
      "HomeCFpp" = as.numeric(nst_report[nst_report$h_a == "home", ]$cf_pp),
      "AwayCFpp" = as.numeric(nst_report[nst_report$h_a == "away", ]$cf_pp)
    ))
  }

  if (length(gameIds) == 0) {
    return(NA)
  } else if (length(gameIds) == 1) {
    return(as.data.frame(gxg(gameIds)))
  } else {
    gxgs <- data.frame()
    for (i in seq_along(gameIds)) {
      gxgs <- dplyr::bind_rows(gxgs, gxg(gameIds[i]))
    }

    return(gxgs)
  }
}


#' Update past scores using the NHL API
#'
#' @param scores old scores
#' @param schedule current schedule
#' @param full_season whether to re-scrape the full season
#' @param save_data whether to write the data to package
#'
#' @return data frame of scores, after optionally writing to package data
#' @export
updateScoresAPI <- function(
  scores = HockeyModel::scores,
  schedule = HockeyModel::schedule,
  full_season = FALSE,
  save_data = FALSE
) {
  if (full_season) {
    neededGames <- schedule[schedule$Date >= getSeasonStartDate(), ]$GameID
  } else {
    neededGames <- schedule[schedule$Date < Sys.Date(), ]$GameID
    neededGames <- neededGames[
      !neededGames %in% scores[scores$GameStatus == "Final", ]$GameID
    ]
  }
  if (length(neededGames) > 0) {
    updatedSc <- getNHLScores(neededGames)
    if (!is.null(updatedSc)) {
      scores <- scores |>
        dplyr::filter(!(.data$GameID %in% neededGames)) |>
        dplyr::bind_rows(updatedSc) |>
        dplyr::mutate(
          Date = as.Date(.data$Date),
          GameID = as.numeric(.data$GameID)
        ) |>
        dplyr::arrange(.data$Date, .data$GameStatus, .data$GameID)
      if (save_data && requireNamespace("usethis", quietly = TRUE)) {
        suppressMessages(usethis::use_data(scores, overwrite = TRUE))
      }
    }
  } else {
    message("Scores are updated to today's date already.")
  }
  return(unique(scores))
}


#' Normalize team name fields
#'
#' @param sc (`vector` or `data.frame`) Values containing team names, or a data
#'   frame with team-name columns.
#' @returns (`vector` or `data.frame`) `sc` with normalized historical and
#'   accented team names.
#' @keywords internal
clean_names <- function(sc) {
  if (is.vector(sc)) {
    sc <- stringi::stri_trans_general(str = sc, "latin-ascii")
    sc <- replace(sc, sc == "Utah Utah Hockey Club", "Utah Hockey Club")
    sc <- replace(sc, sc == "Phoenix Coyotes", "Arizona Coyotes")
    sc <- replace(sc, sc == "Arizona Coyotes", "Utah Hockey Club")
    sc <- replace(sc, sc == "Utah Hockey Club", "Utah Mammoth")
    sc <- replace(sc, sc == "Atlanta Thrashers", "Winnipeg Jets")
    sc <- replace(sc, sc == "Minnesota North Stars", "Dallas Stars")
    sc <- replace(sc, sc == "Quebec Nordiques", "Colorado Avalanche")
  } else if (is.data.frame(sc)) {
    if ("HomeTeam" %in% names(sc)) {
      sc <- sc |>
        dplyr::mutate(
          "HomeTeam" = stringi::stri_trans_general(
            str = .data$HomeTeam,
            "latin-ascii"
          )
        ) |>
        dplyr::mutate(
          "HomeTeam" = replace(
            .data$HomeTeam,
            .data$HomeTeam == "Utah Utah Hockey Club",
            "Utah Hockey Club"
          ),
          "HomeTeam" = replace(
            .data$HomeTeam,
            .data$HomeTeam == "Phoenix Coyotes",
            "Arizona Coyotes"
          ),
          "HomeTeam" = replace(
            .data$HomeTeam,
            .data$HomeTeam == "Arizona Coyotes",
            "Utah Hockey Club"
          ),
          "HomeTeam" = replace(
            .data$HomeTeam,
            .data$HomeTeam == "Utah Hockey Club",
            "Utah Mammoth"
          ),
          "HomeTeam" = replace(
            .data$HomeTeam,
            .data$HomeTeam == "Atlanta Thrashers",
            "Winnipeg Jets"
          ),
          "HomeTeam" = replace(
            .data$HomeTeam,
            .data$HomeTeam == "Minnesota North Stars",
            "Dallas Stars"
          ),
          "HomeTeam" = replace(
            .data$HomeTeam,
            .data$HomeTeam == "Quebec Nordiques",
            "Colorado Avalanche"
          ),
        )
    }
    if ("AwayTeam" %in% names(sc)) {
      sc <- sc |>
        dplyr::mutate(
          "AwayTeam" = stringi::stri_trans_general(
            str = .data$AwayTeam,
            "latin-ascii"
          )
        ) |>
        dplyr::mutate(
          "AwayTeam" = replace(
            .data$AwayTeam,
            .data$AwayTeam == "Utah Utah Hockey Club",
            "Utah Hockey Club"
          ),
          "AwayTeam" = replace(
            .data$AwayTeam,
            .data$AwayTeam == "Phoenix Coyotes",
            "Arizona Coyotes"
          ),
          "AwayTeam" = replace(
            .data$AwayTeam,
            .data$AwayTeam == "Arizona Coyotes",
            "Utah Hockey Club"
          ),
          "AwayTeam" = replace(
            .data$AwayTeam,
            .data$AwayTeam == "Utah Hockey Club",
            "Utah Mammoth"
          ),
          "AwayTeam" = replace(
            .data$AwayTeam,
            .data$AwayTeam == "Atlanta Thrashers",
            "Winnipeg Jets"
          ),
          "AwayTeam" = replace(
            .data$AwayTeam,
            .data$AwayTeam == "Minnesota North Stars",
            "Dallas Stars"
          ),
          "AwayTeam" = replace(
            .data$AwayTeam,
            .data$AwayTeam == "Quebec Nordiques",
            "Colorado Avalanche"
          ),
        )
    }
    if ("Team" %in% names(sc)) {
      sc <- sc |>
        dplyr::mutate(
          "Team" = stringi::stri_trans_general(str = .data$Team, "latin-ascii")
        ) |>
        dplyr::mutate(
          "Team" = replace(
            .data$Team,
            .data$Team == "Utah Utah Hockey Club",
            "Utah Hockey Club"
          ),
          "Team" = replace(
            .data$Team,
            .data$Team == "Phoenix Coyotes",
            "Arizona Coyotes"
          ),
          "Team" = replace(
            .data$Team,
            .data$Team == "Arizona Coyotes",
            "Utah Hockey Club"
          ),
          "Team" = replace(
            .data$Team,
            .data$Team == "Utah Hockey Club",
            "Utah Mammoth"
          ),
          "Team" = replace(
            .data$Team,
            .data$Team == "Atlanta Thrashers",
            "Winnipeg Jets"
          ),
          "Team" = replace(
            .data$Team,
            .data$Team == "Minnesota North Stars",
            "Dallas Stars"
          ),
          "Team" = replace(
            .data$Team,
            .data$Team == "Quebec Nordiques",
            "Colorado Avalanche"
          ),
        )
    }
    if ("name" %in% names(sc)) {
      sc <- sc |>
        dplyr::mutate(
          "name" = stringi::stri_trans_general(str = .data$name, "latin-ascii")
        ) |>
        dplyr::mutate(
          "name" = replace(
            .data$name,
            .data$name == "Utah Utah Hockey Club",
            "Utah Hockey Club"
          ),
          "name" = replace(
            .data$name,
            .data$name == "Phoenix Coyotes",
            "Arizona Coyotes"
          ),
          "name" = replace(
            .data$name,
            .data$name == "Arizona Coyotes",
            "Utah Hockey Club"
          ),
          "name" = replace(
            .data$name,
            .data$name == "Utah Hockey Club",
            "Utah Mammoth"
          ),
          "name" = replace(
            .data$name,
            .data$name == "Atlanta Thrashers",
            "Winnipeg Jets"
          ),
          "name" = replace(
            .data$name,
            .data$name == "Minnesota North Stars",
            "Dallas Stars"
          ),
          "name" = replace(
            .data$name,
            .data$name == "Quebec Nordiques",
            "Colorado Avalanche"
          ),
        )
    }
    if ("Date" %in% names(sc)) {
      sc <- sc |>
        dplyr::mutate("Date" = as.Date(.data$Date))
    }
    if ("OTStatus" %in% names(sc)) {
      if (
        any(sc$OTStatus %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "7OT", "8OT"))
      ) {
        sc[
          sc$OTStatus %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "7OT", "8OT"),
        ]$OTStatus <- "OT"
      }
    }
  }
  return(sc)
}

#' Get Playoff Series using the NHL API
#' @description Gets the current season (or previous seasons') playoff series information using the NHL API
#'
#' @param season Optional, the season's playoff series to retrieve
#' @param wins_required Number of wins needed to clinch a series. The NHL
#'   playoff-bracket API does not report the series format, so this defaults
#'   to `4` (best-of-seven, the NHL's current format) but can be overridden if
#'   the format ever changes.
#'
#' @return a data frame with Round, Series, Home and Away Teams, number of wins each, playoff ranking/seed and
#' whether the series is complete
#' @export
getAPISeries <- function(season = getCurrentSeason8(), wins_required = 4) {
  if (!seasonValidator(season)) {
    cli::cli_abort(
      "{.arg season} must be an 8-digit season ID string like {.val 20182019}."
    )
  }
  url <- paste0(
    "https://api-web.nhle.com/v1/playoff-bracket/",
    substr(season, 5, 8)
  )

  series <- httr2::request(url) |>
    httr2::req_cache(tempdir()) |>
    httr2::req_retry(max_seconds = 120) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON(flatten = TRUE)

  series <- series$series

  if (length(series) == 0) {
    return(data.frame())
  }

  playoffSeries <- series |>
    dplyr::select(
      "Round" = "playoffRound",
      "Series" = "seriesLetter",
      "HomeTeam" = "topSeedTeam.name.default",
      "AwayTeam" = "bottomSeedTeam.name.default",
      "HomeWins" = "topSeedWins",
      "AwayWins" = "bottomSeedWins",
      "HomeSeed" = "topSeedRank",
      "AwaySeed" = "bottomSeedRank"
    )

  if (nrow(playoffSeries) == 0) {
    return(data.frame())
  }

  playoffSeries <- clean_names(playoffSeries)

  playoffSeries$Status <- ifelse(
    playoffSeries$HomeWins >= wins_required |
      playoffSeries$AwayWins >= wins_required,
    "Complete",
    "Ongoing"
  )
  playoffSeries <- playoffSeries |>
    dplyr::mutate(
      "Round" = as.integer(.data$Round),
      # Series identifiers are treated as opaque strings (not assumed to be
      # A-Z letters), since the API's series-letter scheme could change.
      "Series" = as.character(.data$Series),
      "HomeWins" = as.integer(.data$HomeWins),
      "AwayWins" = as.integer(.data$AwayWins),
      "HomeSeed" = as.integer(.data$HomeSeed),
      "AwaySeed" = as.integer(.data$AwaySeed)
    ) |>
    dplyr::filter(
      .data$HomeTeam != "TBD",
      .data$AwayTeam != "TBD"
    )

  return(playoffSeries[stats::complete.cases(playoffSeries), ])
}


#' Fetch an NHL boxscore payload
#'
#' @param gid (`character(1)` or `numeric(1)`) NHL game ID.
#' @returns (`list`) Parsed boxscore response from the NHL API.
#' @keywords internal
nhl_boxscore <- function(gid) {
  url <- paste0("https://api-web.nhle.com/v1/gamecenter/", gid, "/boxscore")
  req <- httr2::request(url) |>
    httr2::req_cache(tempdir()) |>
    httr2::req_retry(max_seconds = 120) |>
    httr2::req_cache(tempdir()) |>
    httr2::req_perform()
  req <- jsonlite::fromJSON(httr2::resp_body_string(req))
  return(req)
}
