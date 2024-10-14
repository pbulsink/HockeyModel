#' Get NHL Schedule
#'
#' @description Gets the NHL schedule from the NHL API for the season(s) requested. Returns data formatted for further use. This can be slow if requesting many seasons due to the API rate limit.
#'
#' @param season Season(s) for which the schedule is requested, as YYYY format. Defaults to current season. Accepts single years, ranges e.g.c(2010:2015) or complex ranges e.g. c(2010:2015, 2018).
#'
#' @return a data frame of all scheduled games for the season(s) requested, with Date, HomeTeam, AwayTeam, GameID, and GameType.
#' @export
getNHLSchedule <- function(season = getCurrentSeason8()) {
  stopifnot(seasonValidator(season))

  # This is imilar to how Dan Morse did it in hockeyR
  sched <- NULL
  for (i in unique(teamColours$ShortCode)) {
    url <- paste0("https://api-web.nhle.com/v1/club-schedule-season/", i, "/", season)

    site <- tryCatch(
      httr2::request(url) %>%
        httr2::req_retry(max_seconds = 120) %>%
        httr2::req_perform() %>%
        httr2::resp_body_string() %>%
        jsonlite::fromJSON(),
      error = function(cond) {
        message(paste0("There was a problem fetching ",i,"'s games: ", cond))
        return(NULL)
      }
    )

    if (!is.null(site) & !is.null(site$games) & length(site$games) > 0) {
      sg <- site$games %>%
        dplyr::filter(.data$gameType > 1)

      games <- data.frame(Date = sg$gameDate,
                             HomeTeam = getLongTeam(sg$homeTeam$abbrev),
                             AwayTeam = getLongTeam(sg$awayTeam$abbrev),
                             GameID = sg$id,
                             GameType = ifelse(sg$gameType == 2, "R", ifelse(sg$gameType == 3, "P", "NA")),
                             GameStatus = sg$gameState) %>%
        dplyr::filter(.data$GameType %in% c("R", "P"))

      sched <- dplyr::bind_rows(sched, games)
    } else {
      next()
    }
  }

  sched <- sched %>%
    unique() %>%
    dplyr::arrange(.data$Date, .data$GameID)

  if(nrow(sched[sched$GameStatus %in% c("FUT", "PPD") & sched$Date < Sys.Date(),]) > 0){
    sched[sched$GameStatus %in% c("FUT", "PPD") & sched$Date < Sys.Date(),]$Date <- max(sched$Date)
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
games_today <- function(schedule = HockeyModel::schedule, date = Sys.Date(), all_games = FALSE) {
  stopifnot(is.Date(date))
  url <- paste0("https://api-web.nhle.com/v1/schedule/", as.Date(date, "%Y-%m-%d"))

  sched <- httr2::request(url) %>%
    httr2::req_retry(max_seconds = 120) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    jsonlite::fromJSON()
  gameWeek <- sched$gameWeek

  if(gameWeek[gameWeek$date == as.Date(date, "%Y-%m-%d", ),]$numberOfGames == 0){
    return(NULL)
  }

  gids <- gameWeek[gameWeek$date == as.Date(date, "%Y-%m-%d"), ]$games[[1]]$id
  todaygames <- schedule[schedule$GameID %in% gids, ]
  if (nrow(todaygames) == 0){
    message("Games on today aren't present in Schedule. Be sure schedule is updated!!")
    return(NULL)
  }
  return(todaygames)
}


#' Update schedule using the NHL API
#'
#' @param schedule current schedule data
#' @param save_data whether to write to package data
#'
#' @return data frame of schedule, after optionally writing to package data
#' @export
updateScheduleAPI <- function(schedule = HockeyModel::schedule, save_data = FALSE) {
  currentSeason <- getSeason()
  if (is.null(currentSeason)) {
    currentSeason <- getCurrentSeason8()
  }
  sched <- getNHLSchedule(currentSeason)
  stopifnot(!is.null(sched))

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
getNHLScores <- function(gameIDs = NULL, schedule = HockeyModel::schedule, progress = TRUE) {
  scores <- NULL
  if (is.null(gameIDs)) {
    gameIDs <- gameIDs[gameIDs %in% schedule[schedule$Date < Sys.Date(), "GameID"]]
  }
  if (progress) {
    if (!requireNamespace("progress", quietly = TRUE)) {
      progress <- FALSE
    }
  }
  if (progress) {
    pb <- progress::progress_bar$new(
      format = "  getting scores [:bar] :percent eta: :eta",
      total = length(gameIDs), show_after = 5
    )
  }
  dropped_gid <- c()

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
        "HomeTeam" = paste(sc$homeTeam$placeName[[1]], sc$homeTeam$name[[1]]),
        "AwayTeam" = paste(sc$awayTeam$placeName[[1]], sc$awayTeam$name[[1]]),
        "GameID" = sc$id,
        "HomeGoals" = sc$homeTeam$score,
        "AwayGoals" = sc$awayTeam$score,
        "OTStatus" = ifelse(sc$periodDescriptor$number == 3, "", sc$periodDescriptor$number),
        "GameType" = ifelse(substr(g, 6, 6) == 2, "R", "P"),
        "GameStatus" = "Final"
      )
      scores <- rbind(scores, dfs)
    } else {
      warning("Game ", g, " not in final state, instead showing ", sc$gameState, "\nGame schedule state is ", sc$gameScheduleState)
      dropped_gid <- c(dropped_gid, g)
      next
    }
    if (progress) {
      pb$tick()
    }
  }

  gameIDs <- gameIDs[!(gameIDs %in% dropped_gid)]

  scores_xg <- get_xg(gameIds = gameIDs)
  if (!is.null(scores)) {
    scores <- clean_names(scores)
    if (nrow(scores[scores$OTStatus == "3rd", ]) > 0) {
      scores[scores$OTStatus == "3rd", ]$OTStatus <- ""
    }
    scores <- scores %>%
      dplyr::mutate(OTStatus = dplyr::case_when(
        .data$OTStatus <= 3 ~ "",
        .data$OTStatus > 3 ~ "OT",
        .data$OTStatus == 5 & .data$GameType == "R" ~ "SO",
      )) %>%
      dplyr::mutate(Result = dplyr::case_when(
        (.data$HomeGoals > .data$AwayGoals) & .data$OTStatus == "" ~ 1,
        (.data$HomeGoals < .data$AwayGoals) & .data$OTStatus == "" ~ 0,
        (.data$HomeGoals == .data$AwayGoals) ~ 0.5,
        (.data$HomeGoals > .data$AwayGoals) & .data$OTStatus == "OT" ~ 0.75,
        (.data$HomeGoals > .data$AwayGoals) & .data$OTStatus == "SO" ~ 0.6,
        (.data$HomeGoals < .data$AwayGoals) & .data$OTStatus == "SO" ~ 0.4,
        (.data$HomeGoals < .data$AwayGoals) & .data$OTStatus == "OT" ~ 0.25,
      )) %>%
      dplyr::arrange(.data$Date, .data$GameStatus, .data$GameID)
  }
  scores <- dplyr::left_join(scores, scores_xg, by = "GameID")
  return(scores)
}


load_or_get_nst <- function(gid) {
  season <- as.numeric(substr(gid, 1, 4))
  game_id <- as.numeric(substr(gid, 5, 10))

  season <- paste0(season, season + 1)

  if (system2("grep", paste0('-l "', gid, '" ', "~/Documents/natstattrick.csv"), stdout = FALSE) == 0) {
    nstall <- utils::read.csv("~/Documents/natstattrick.csv")
    nstdf <- nstall %>%
      dplyr::filter(.data$game_id == gid)
  } else {
    nstdf <- naturalstattrick::nst_report_df(
      season = season,
      game_id = game_id
    )
    utils::write.table(nstdf,
      file = "~/Documents/natstattrick.csv", append = TRUE,
      row.names = FALSE, col.names = FALSE, sep = ","
    )
  }
  closeAllConnections()

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

    # xG<-BulsinkBxG::get_game_xg(gid)
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

  # v_gxg <- Vectorize(gxg, SIMPLIFY = FALSE)

  if (length(gameIds) == 0) {
    return(NA)
  } else if (length(gameIds) == 1) {
    return(gxg(gameIds))
  } else {
    gxgs <- data.frame()
    for (i in seq_along(gameIds)) {
      gxgs <- dplyr::bind_rows(gxgs, gxg(gameIds[i]))
    }
    # gxgs <- v_gxg(gid = gameIds)
    # gxgs <- dplyr::bind_rows(gxgs)

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
updateScoresAPI <- function(scores = HockeyModel::scores, schedule = HockeyModel::schedule,
                            full_season = FALSE, save_data = FALSE) {
  if (full_season) {
    neededGames <- schedule[schedule$Date >= getSeasonStartDate(), ]$GameID
  } else {
    neededGames <- schedule[schedule$Date < Sys.Date(), ]$GameID
    neededGames <- neededGames[!neededGames %in% scores[scores$GameStatus == "Final", ]$GameID]
  }
  if (length(neededGames) > 0) {
    updatedSc <- getNHLScores(neededGames)
    if (!is.null(updatedSc)) {
      scores <- scores %>%
        dplyr::filter(!(.data$GameID %in% neededGames)) %>%
        dplyr::bind_rows(updatedSc) %>%
        dplyr::mutate(Date = as.Date(.data$Date),
                      GameID = as.numeric(.data$GameID)) %>%
        dplyr::arrange(.data$Date, .data$GameStatus, .data$GameID)
      if (save_data && requireNamespace("usethis", quietly = TRUE)) {
        suppressMessages(usethis::use_data(scores, overwrite = TRUE))
      }
    }
  } else {
    message("Scores are updated to today's date already.")
  }
  return(scores)
}

updateScoresAPI_byGameID <- function(gameids, save_data = FALSE) {
  updatedSc <- getNHLScores(gameids, schedule = dplyr::bind_rows(HockeyModel::scores, HockeyModel::schedule))
  if (!is.null(updatedSc)) {
    scores <- scores %>%
      dplyr::filter(!(.data$GameID %in% gameids)) %>%
      dplyr::bind_rows(updatedSc) %>%
      dplyr::arrange(.data$Date, .data$GameStatus, .data$GameID)
    if (save_data && requireNamespace("usethis", quietly = TRUE)) {
      suppressMessages(usethis::use_data(scores, overwrite = TRUE))
    }
  }
  invisible(scores)
}

clean_names <- function(sc) {
  if (is.vector(sc)) {
    sc <- stringi::stri_trans_general(str = sc, "latin-ascii")
    sc <- replace(sc, sc == "Utah Utah Hockey Club", "Utah Hockey Club")
    sc <- replace(sc, sc == "Phoenix Coyotes", "Arizona Coyotes")
    sc <- replace(sc, sc == "Arizona Coyotes", "Utah Hockey Club")
    sc <- replace(sc, sc == "Atlanta Thrashers", "Winnipeg Jets")
    sc <- replace(sc, sc == "Minnesota North Stars", "Dallas Stars")
    sc <- replace(sc, sc == "Quebec Nordiques", "Colorado Avalanche")
  } else if (is.data.frame(sc)) {
    if ("HomeTeam" %in% names(sc)) {
      sc <- sc %>%
        dplyr::mutate("HomeTeam" = stringi::stri_trans_general(str = .data$HomeTeam, "latin-ascii")) %>%
        dplyr::mutate(
          "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Utah Utah Hockey Club", "Utah Hockey Club"),
          "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Phoenix Coyotes", "Arizona Coyotes"),
          "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Arizona Coyotes", "Utah Hockey Club"),
          "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Atlanta Thrashers", "Winnipeg Jets"),
          "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Minnesota North Stars", "Dallas Stars"),
          "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Quebec Nordiques", "Colorado Avalanche"),
        )
    }
    if ("AwayTeam" %in% names(sc)) {
      sc <- sc %>%
        dplyr::mutate("AwayTeam" = stringi::stri_trans_general(str = .data$AwayTeam, "latin-ascii")) %>%
        dplyr::mutate(
          "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Utah Utah Hockey Club", "Utah Hockey Club"),
          "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Phoenix Coyotes", "Arizona Coyotes"),
          "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Arizona Coyotes", "Utah Hockey Club"),
          "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Atlanta Thrashers", "Winnipeg Jets"),
          "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Minnesota North Stars", "Dallas Stars"),
          "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Quebec Nordiques", "Colorado Avalanche"),
        )
    }
    if ("Team" %in% names(sc)) {
      sc <- sc %>%
        dplyr::mutate("Team" = stringi::stri_trans_general(str = .data$Team, "latin-ascii")) %>%
        dplyr::mutate(
          "Team" = replace(.data$Team, .data$Team == "Utah Utah Hockey Club", "Utah Hockey Club"),
          "Team" = replace(.data$Team, .data$Team == "Phoenix Coyotes", "Arizona Coyotes"),
          "Team" = replace(.data$Team, .data$Team == "Arizona Coyotes", "Utah Hockey Club"),
          "Team" = replace(.data$Team, .data$Team == "Atlanta Thrashers", "Winnipeg Jets"),
          "Team" = replace(.data$Team, .data$Team == "Minnesota North Stars", "Dallas Stars"),
          "Team" = replace(.data$Team, .data$Team == "Quebec Nordiques", "Colorado Avalanche"),
        )
    }
    if ("name" %in% names(sc)) {
      sc <- sc %>%
        dplyr::mutate("name" = stringi::stri_trans_general(str = .data$name, "latin-ascii")) %>%
        dplyr::mutate(
          "name" = replace(.data$name, .data$name == "Utah Utah Hockey Club", "Utah Hockey Club"),
          "name" = replace(.data$name, .data$name == "Phoenix Coyotes", "Arizona Coyotes"),
          "name" = replace(.data$name, .data$name == "Arizona Coyotes", "Utah Hockey Club"),
          "name" = replace(.data$name, .data$name == "Atlanta Thrashers", "Winnipeg Jets"),
          "name" = replace(.data$name, .data$name == "Minnesota North Stars", "Dallas Stars"),
          "name" = replace(.data$name, .data$name == "Quebec Nordiques", "Colorado Avalanche"),
        )
    }
    if ("Date" %in% names(sc)) {
      sc <- sc %>%
        dplyr::mutate("Date" = as.Date(.data$Date))
    }
    if ("OTStatus" %in% names(sc)) {
      if (any(sc$OTStatus %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "7OT", "8OT"))) {
        sc[sc$OTStatus %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "7OT", "8OT"), ]$OTStatus <- "OT"
      }
    }
  }
  return(sc)
}


#' Get Playoff Series using the NHL API
#' @description Gets the current season (or previous seasons') playoff series information using the NHL API
#'
#' @param season Optional, the season's playoff series to retrieve
#'
#' @return a data frame with Round, Series, Home and Away Teams, number of wins each, playoff ranking/seed and
#' whether the series is complete
#' @export
getAPISeries <- function(season = getCurrentSeason8()) {

  url <- paste0("https://api-web.nhle.com/v1/playoff-bracket/", substr(getCurrentSeason8(), 5, 8))

  series <- httr2::request(url) %>%
    httr2::req_retry(max_seconds = 120) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    jsonlite::fromJSON(flatten = TRUE)

  series <- series$series

  if (length(series) == 0) {
    return(data.frame())
    # stop('No Series Data Available')
  }

  playoffSeries <- series %>%
    dplyr::select("Round" = "playoffRound",
           "Series" = "seriesLetter",
           "HomeTeam" = "topSeedTeam",
           "AwayTeam" = "bottomSeedTeam.name.default",
           "HomeWins" = "topSeedWins",
           "AwayWins" = "bottomSeedWins",
           "HomeSeed" = "topSeedRank",
           "AwaySeed" = "bottomSeedRank",
           "requiredWins" = 4)

  if (nrow(playoffSeries) == 0) {
    return(data.frame())
    # stop('No Series Data Available')
  }

  playoffSeries <- clean_names(playoffSeries)

  playoffSeries$Status <- ifelse(playoffSeries$HomeWins == playoffSeries$requiredWins | playoffSeries$AwayWins == playoffSeries$requiredWins, "Complete", "Ongoing")
  playoffSeries$requiredWins <- NULL
  playoffSeries <- playoffSeries %>%
    dplyr::mutate(
      "Round" = as.integer(.data$Round),
      "Series" = sapply(.data$Series, function(x) which(LETTERS == x, useNames = FALSE), USE.NAMES = FALSE),
      "HomeWins" = as.integer(.data$HomeWins),
      "AwayWins" = as.integer(.data$AwayWins),
      "HomeSeed" = as.integer(.data$HomeSeed),
      "AwaySeed" = as.integer(.data$AwaySeed)
    )
  return(playoffSeries)
}


validateWins <- function(playoffSeries, seriesStatusShort) {
  hometeam <- playoffSeries$HomeTeam
  awayteam <- playoffSeries$AwayTeam

  homeshort <- getShortTeam(hometeam)
  awayshort <- getShortTeam(awayteam)

  statusteam <- ifelse(grepl(homeshort, seriesStatusShort), homeshort,
    ifelse(grepl(awayshort, seriesStatusShort), awayshort, NA)
  )
  wins <- unlist(strsplit(seriesStatusShort, "-"))
  wins[1] <- unlist(strsplit(wins[1], " "))[length(unlist(strsplit(wins[1], " ")))]
  if (is.na(statusteam)) {
    # Tied
    return(as.numeric(c(wins[1], wins[2])))
  } else if (statusteam == awayshort) {
    # Away leading, invert (as response is homewins, awaywins)
    return(as.numeric(c(wins[2], wins[1])))
  } else {
    return(as.numeric(c(wins[1], wins[2])))
  }
}


#' getSeasonStartDate
#'
#' @param season Season (8 character code) or NULL for current/most recent season
#'
#' @return Season start date as date
#' @export
getSeasonStartDate <- function(season = NULL) {
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) %>%
    httr2::req_cache(tempdir()) %>%
    httr2::req_retry(max_seconds = 120) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
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


#' Get Current Season
#'
#' @return current season (as 20172018 format) based on today's date.
#' @export
getCurrentSeason8 <- function() {
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) %>%
    httr2::req_cache(tempdir()) %>%
    httr2::req_retry(max_seconds = 120) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    jsonlite::fromJSON()
  seasons <- seasons$data

  return(utils::tail(seasons$id, 1))
}


#' GetCurrentSeasonEndDate
#'
#' @param season Season (8 character code) or NULL for current/most recent season
#'
#' @return Season end date (as date)
#' @export
getSeasonEndDate <- function(season = NULL) {
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) %>%
    httr2::req_cache(tempdir()) %>%
    httr2::req_retry(max_seconds = 120) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
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
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) %>%
    httr2::req_cache(tempdir()) %>%
    httr2::req_retry(max_seconds = 120) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    jsonlite::fromJSON()
  seasons <- seasons$data
  seasons_list <- seasons[seasons$startDate <= date & seasons$regularSeasonEndDate >= date, ]
  if (boolean) {
    return(ifelse(nrow(seasons_list) > 0, TRUE, FALSE))
  } else {
    if (nrow(seasons_list) > 0) {
      return(seasons_list$id)
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
  stopifnot(is.Date(date))
  date <- as.Date(date)
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) %>%
    httr2::req_cache(tempdir()) %>%
    httr2::req_retry(max_seconds = 120) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
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
  stopifnot(is.Date(date))
  date <- as.Date(date)

  return(all(date > getSeasonEndDate(), date < as.Date("2025-07-05")))
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) %>%
    httr2::req_cache(tempdir()) %>%
    httr2::req_retry(max_seconds = 120) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    jsonlite::fromJSON()
  seasons <- seasons$data
  seasons_list <- seasons[as.Date(seasons$regularSeasonEndDate) <= date & as.Date(seasons$endDate) >= date, ]
  if (boolean) {
    return(ifelse(nrow(seasons_list) > 0, TRUE, FALSE))
  } else {
    if (nrow(seasons_list) > 0) {
      return(seasons_list$seasonId)
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
  stopifnot(is.Date(gamedate))
  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) %>%
    httr2::req_cache(tempdir()) %>%
    httr2::req_retry(max_seconds = 120) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    jsonlite::fromJSON()
  seasons <- seasons$data
  gs <- function(gd, seasons) {
    gd <- as.Date(gd)
    season_list <- seasons[seasons$startDate <= gd & seasons$endDate >= gd, ]
    if (nrow(season_list) == 1) {
      return(season_list$id)
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


getConferences <- function() {
  return(unique(teamColours$Conference))
}

getDivisions <- function() {
  return(unique(teamColours$Division))
}

getTeamConferences <- function(teams) {
  getteamconf <- function(t) {
    return(teamColours[teamColours$Team == t, ]$Conference)
  }

  v_getteamconf <- Vectorize(getteamconf, "t")
  teams <- clean_names(teams)
  if (length(teams) == 1) {
    return(getteamconf(t = teams))
  } else {
    return(unname(v_getteamconf(t = teams)))
  }
}

getTeamDivisions <- function(teams) {
  getteamdiv <- function(t) {
    return(teamColours[teamColours$Team == t, ]$Division)
  }

  v_getteamdiv <- Vectorize(getteamdiv, "t")
  teams <- clean_names(teams)
  if (length(teams) == 1) {
    return(getteamdiv(t = teams))
  } else {
    return(unname(v_getteamdiv(t = teams)))
  }
}

getShortTeam <- function(teams) {
  getteamshort <- function(t) {
    return(teamColours[teamColours$Team == t, ]$ShortCode)
  }

  v_getteamshort <- Vectorize(getteamshort, "t")
  teams <- clean_names(teams)
  if (length(teams) == 1) {
    return(getteamshort(t = teams))
  } else {
    return(unname(v_getteamshort(t = teams)))
  }
}

getLongTeam <- function(teams) {
  getteamlong <- function(t) {
    return(teamColours[teamColours$ShortCode == t, ]$Team)
  }

  v_getteamshort <- Vectorize(getteamlong, "t")
  teams <- clean_names(teams)
  if (length(teams) == 1) {
    return(getteamlong(t = teams))
  } else {
    return(unname(v_getteamshort(t = teams)))
  }
}

getNumGames <- function(season = getCurrentSeason8()) {
  if (!is.null(season)) {
    stopifnot(seasonValidator(season))
  } else {
    stop("Season must be supplied")
  }

  url <- "https://api.nhle.com/stats/rest/en/season"
  seasons <- httr2::request(url) %>%
    httr2::req_cache(tempdir()) %>%
    httr2::req_retry(max_seconds = 120) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    jsonlite::fromJSON()
  seasons <- seasons$data

  return(seasons[seasons$id == season, ]$numberOfGames)
}

nhl_boxscore <- function(gid) {
  url <- paste0("https://api-web.nhle.com/v1/gamecenter/", gid, "/boxscore")
  req <- httr2::request(url) %>%
    httr2::req_retry(max_seconds = 120) %>%
    httr2::req_cache(path = "./cache/") %>%
    httr2::req_perform()
  req <- jsonlite::fromJSON(httr2::resp_body_string(req))
  return(req)
}
