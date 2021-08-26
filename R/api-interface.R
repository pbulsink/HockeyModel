#' Get NHL Schedule
#'
#' @description Gets the NHL schedule from the NHL API for the season(s) requested. Returns data formatted for further use. This can be slow if requesting many seasons due to the API rate limit.
#'
#' @param season Season(s) for which the schedule is requested, as YYYY format. Defaults to current season. Accepts single years, ranges e.g.c(2010:2015) or complex ranges e.g. c(2010:2015, 2018).
#'
#' @return a data frame of all scheduled games for the season(s) requested, with Date, HomeTeam, AwayTeam, GameID, and GameType.
#' @export
#'
#' @seealso See [nhlapi::nhl_schedule_seasons()] for alternative schedule requests, and [nhlapi::nhl_games()] for more information on GameID
getNHLSchedule<-function(season=getSeason()){
  if(is.null(season)){
    season<-utils::tail(nhlapi::nhl_seasons()$seasonId, 1)
  }
  sched<-nhlapi::nhl_schedule_seasons(season)
  if(length(sched) == 0){
    # there was an error
    warning("There was an error getting the ", season, " schedule from the NHL", call. = FALSE)
    return(NULL)
  } else {
    return(processNHLSchedule(sched = sched))
  }
}

processNHLSchedule<-function(sched, progress = TRUE){
  schedule<-data.frame("Date" = character(), "HomeTeam" = character(), "AwayTeam" = character(), "GameID" = integer(), "GameType" = character(), "GameState" = character())
  if(progress){
    if(!requireNamespace('progress', quietly = TRUE)){ progress<-FALSE }
  }
  if(progress){
    pb<-progress::progress_bar$new(
      format = "  getting schedule [:bar] :percent eta: :eta",
      total = length(sched), show_after = 5)
  }
  for (y in 1:length(sched)){
    if(!(length(sched[[y]]$dates)>0)){
      next
    }
    for(d in 1:nrow(sched[[y]]$dates)){
      df<-data.frame("Date" = character(), "HomeTeam" = character(), "AwayTeam" = character(), "GameID" = integer(), "GameType" = character(), "GameState" = character())
      for (g in 1:sched[[y]]$dates$totalGames[[d]]){
        dfg<-data.frame("Date" = sched[[y]]$dates$date[[d]],
                        "HomeTeam" = sched[[y]]$dates$games[[d]]$teams.home.team.name[[g]],
                        "AwayTeam" = sched[[y]]$dates$games[[d]]$teams.away.team.name[[g]],
                        "GameID" = sched[[y]]$dates$games[[d]]$gamePk[[g]],
                        "GameType" = sched[[y]]$dates$games[[d]]$gameType[[g]],
                        "GameState" = sched[[y]]$dates$games[[d]]$status.detailedState[[g]])
        df<-rbind(df, dfg)
      }
      schedule<-rbind(schedule, df)
    }
    if(progress){
      pb$tick()
    }
  }
  schedule <- clean_names(schedule)
  schedule <- schedule %>%
    dplyr::filter(.data$GameType %in% c("P", "R")) %>%
    dplyr::arrange(.data$Date, dplyr::desc(.data$GameState), .data$GameID)

  schedule$GameState[is.na(schedule$GameState)] <- "Final"

  return(schedule)
}


#' Games Today
#'
#' @description given a schedule, it returns todays (or another date's) scheduled games (excluding postponements), or NULL if there are none
#'
#' @param schedule the schedule within which to look for games
#' @param date the date to look for games, as a date
#' @param all_games whether to return all games scheduled for a date (True) or exclude postponed, rescheduled, in-progress, or completed games (False, default)
#'
#' @return Scheduled games (in the format of the schedule) for the requested date, or NULL if none
#' @export
games_today<-function(schedule=HockeyModel::schedule, date=Sys.Date(), all_games = FALSE){
  stopifnot(methods::is(date, "Date"))
  todaygames<-processNHLSchedule(nhlapi::nhl_schedule_date_range(date, date))
  if(!all_games){
    todaygames<-todaygames["Scheduled" %in% todaygames$GameState, ]
  }
  if (nrow(todaygames) > 0){
    return(todaygames)
  } else {
    return(NULL)
  }
}


#' Update schedule using the NHL API
#'
#' @param schedule current schedule data
#' @param save_data whether to write to package data
#'
#' @return data frame of schedule, after optionally writing to package data
#' @export
updateScheduleAPI<-function(schedule = HockeyModel::schedule, save_data = FALSE){
  currentSeason<-getSeason()
  if(is.null(currentSeason)){
    currentSeason<-utils::tail(nhlapi::nhl_seasons()$seasonId, 1)
  }
  sched<-getNHLSchedule(currentSeason)
  stopifnot(!is.null(sched))
  gameIDs<-sched$GameID
  schedule<-schedule %>%
    dplyr::filter(!(.data$GameID %in% gameIDs)) %>%
    dplyr::bind_rows(sched) %>%
    dplyr::arrange(.data$Date, dplyr::desc(.data$GameState), .data$GameID)

  schedule<-removeUnscheduledGames(schedule = schedule)

  if(save_data){
    suppressMessages(usethis::use_data(schedule, overwrite=TRUE))
  }
  return(schedule)
}


#' Get NHL Scores
#'
#' @description Get the NHL game scores from the NHL API for any game(s) with a final score. Returns data formatted for future use. Requires gameID(s) be provided.This can be slow if requesting many games due to the API rate limit.
#'
#' @param gameIDs Game IDs (10 digit number). See [nhlapi::nhl_games()] for more information
#' @param schedule optional, provide a schedule if not using the HockeyModel::schedule
#' @param progress whether to show a progress bar. Requires the 'progress' package installed
#'
#' @return a data frame with Date, HomeTeam, AwayTeam, GameID, HomeGoals, AwayGoals, OTStatus and GameType
#' @export
#'
#' @seealso See [nhlapi::nhl_games()] for more information on gameIDs
getNHLScores<-function(gameIDs, schedule = HockeyModel::schedule, progress = TRUE){
  scores<-NULL
  gameIDs <- gameIDs[gameIDs %in% schedule[schedule$Date <= Sys.Date(), "GameID"]]
  if(progress){
    if(!requireNamespace('progress', quietly = TRUE)){ progress<-FALSE }
  }
  if(progress){
    pb<-progress::progress_bar$new(
      format = "  getting scores [:bar] :percent eta: :eta",
      total = length(gameIDs), show_after = 5)
  }
  for(g in gameIDs){
    sc<-nhlapi::nhl_games_linescore(g)
    if('nhl_get_data_error' %in% class(sc[[1]])){
      next
    }
    if(sc[[1]]$currentPeriod > 0){
      if(sc[[1]]$currentPeriodTimeRemaining == "Final"){
        dfs<-data.frame("Date" = schedule[schedule$GameID == g, ]$Date,
                    "HomeTeam" = sc[[1]]$teams$home$team$name,
                    "AwayTeam" = sc[[1]]$teams$away$team$name,
                    "GameID" = g,
                    "HomeGoals" = sc[[1]]$teams$home$goals,
                    "AwayGoals" = sc[[1]]$teams$away$goals,
                    "OTStatus" = sc[[1]]$currentPeriodOrdinal,
                    "GameType" = schedule[schedule$GameID == g, ]$GameType,
                    "GameStatus" = sc[[1]]$currentPeriodTimeRemaining)
        scores<-rbind(scores, dfs)
      } else if (sc[[1]]$currentPeriodTimeRemaining == "20:00" & sc[[1]]$currentPeriod == 1){
        #MAYBE the game is cancelled?
        #get the schedule's date for this game:
        d<-schedule[schedule$GameID == g, ]$Date
        #Check the NHL API for this game on that date
        newsched<-processNHLSchedule(nhlapi::nhl_schedule_date_range(startDate = d, endDate = d), progress = FALSE)
        if(!(g %in% newsched$GameID)){
          #This game isn't in the schedule anymore. Schedule is out of date.
          warning("Game ", g, " no longer scheduled.")
          next
        }
      } else {
        warning("Game ", g, " not in final state, instead showing ", sc[[1]]$currentPeriodTimeRemaining)
        next
      }
    }
    if(progress){
      pb$tick()
    }
  }
  scores<-clean_names(scores)
  scores[scores$OTStatus == "3rd", ]$OTStatus<-""
  scores<-scores %>%
    dplyr::mutate(Result = dplyr::case_when(
                    (.data$HomeGoals >  .data$AwayGoals) & .data$OTStatus == "" ~ 1,
                    (.data$HomeGoals <  .data$AwayGoals) & .data$OTStatus == "" ~ 0,
                    (.data$HomeGoals == .data$AwayGoals) ~ 0.5,
                    (.data$HomeGoals >  .data$AwayGoals) & .data$OTStatus == "OT" ~ 0.75,
                    (.data$HomeGoals >  .data$AwayGoals) & .data$OTStatus == "SO" ~ 0.6,
                    (.data$HomeGoals <  .data$AwayGoals) & .data$OTStatus == "SO" ~ 0.4,
                    (.data$HomeGoals <  .data$AwayGoals) & .data$OTStatus == "OT" ~ 0.25,
    )) %>%
    dplyr::arrange(.data$Date, .data$GameStatus, .data$GameID)

  return(scores)
}


#' Remove Unscheduled Games
#'
#' @description Sometimes games are scheduled then not played (e.g. unneeded games in playoff series, etc.)
#'
#' @param schedule the schedule to check for unscheduled games
#' @param save_data whether to save the cleaned schedule to package or not. Default False
#'
#' @return a schedule with unscheduled games removed
#' @export
removeUnscheduledGames<-function(schedule=HockeyModel::schedule, save_data=FALSE){
  unsched<-schedule[schedule$GameState != "Final" & schedule$Date < Sys.Date(),]
  removedGames<-c()
  for(g in unsched$GameID){
    d<-unsched[unsched$GameID == g, ]$Date

    sched<-nhlapi::nhl_schedule_date_range(startDate = d, endDate = d)

    checksched<-processNHLSchedule(sched)
    if(!(g %in% checksched$GameID)){
      #Game is removed
      removedGames <- c(removedGames, g)
    }
  }

  schedule<-schedule[!(schedule$GameID %in% removedGames), ]

  if(save_data){
    usethis::use_data(schedule, overwrite = TRUE)
  }

  return(schedule)
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
updateScoresAPI<-function(scores=HockeyModel::scores, schedule=HockeyModel::schedule, full_season = FALSE, save_data=FALSE){
  if(full_season){
    neededGames<-schedule[schedule$Date > getSeasonStartDate(), ]$GameID
  } else {
    neededGames<-schedule[schedule$Date < Sys.Date(), ]$GameID
    neededGames<-neededGames[!neededGames %in% scores[scores$GameStatus == 'Final', ]$GameID]
  }
  if(length(neededGames)>0){
    updatedSc<-getNHLScores(neededGames)
    scores<-scores %>%
      dplyr::filter(!(.data$GameID %in% neededGames)) %>%
      dplyr::bind_rows(updatedSc) %>%
      dplyr::arrange(.data$Date, .data$GameStatus, .data$GameID)
    if (save_data){
      suppressMessages(usethis::use_data(scores, overwrite=TRUE))
    }
  } else {
    message("Scores are updated to today's date already.")
  }
  return(scores)
}


clean_names<-function(sc){
  if(is.vector(sc)){
    sc<-stringi::stri_trans_general(str=sc, 'latin-ascii')
    sc<-replace(sc, sc == "Phoenix Coyotes", "Arizona Coyotes")
    sc<-replace(sc, sc == "Atlanta Thrashers", "Winnipeg Jets")
    sc<-replace(sc, sc == "Minnesota North Stars", "Dallas Stars")
    sc<-replace(sc, sc == "Quebec Nordiques", "Colorado Avalanche")
    sc<-replace(sc, sc == "Chicago Blackhawks", "Chicago")
  } else if (is.data.frame(sc)){
    if('HomeTeam' %in% names(sc)){
      sc <- sc %>%
        dplyr::mutate("HomeTeam" = stringi::stri_trans_general(str=.data$HomeTeam, 'latin-ascii')) %>%
        dplyr::mutate("HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Phoenix Coyotes", "Arizona Coyotes"),
                      "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Atlanta Thrashers", "Winnipeg Jets"),
                      "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Minnesota North Stars", "Dallas Stars"),
                      "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Quebec Nordiques", "Colorado Avalanche"),
                      "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Chicago Blackhawks", "Chicago")
                      )
    }
    if('AwayTeam' %in% names(sc)){
      sc <- sc %>%
        dplyr::mutate("AwayTeam" = stringi::stri_trans_general(str=.data$AwayTeam, 'latin-ascii')) %>%
        dplyr::mutate("AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Phoenix Coyotes", "Arizona Coyotes"),
                      "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Atlanta Thrashers", "Winnipeg Jets"),
                      "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Minnesota North Stars", "Dallas Stars"),
                      "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Quebec Nordiques", "Colorado Avalanche"),
                      "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Chicago Blackhawks", "Chicago")
        )
    }
    if('Team' %in% names(sc)) {
      sc <- sc %>%
        dplyr::mutate("Team" = stringi::stri_trans_general(str=.data$Team, 'latin-ascii')) %>%
        dplyr::mutate("Team" = replace(.data$Team, .data$Team == "Phoenix Coyotes", "Arizona Coyotes"),
                      "Team" = replace(.data$Team, .data$Team == "Atlanta Thrashers", "Winnipeg Jets"),
                      "Team" = replace(.data$Team, .data$Team == "Minnesota North Stars", "Dallas Stars"),
                      "Team" = replace(.data$Team, .data$Team == "Quebec Nordiques", "Colorado Avalanche"),
                      "Team" = replace(.data$Team, .data$Team == "Chicago Blackhawks", "Chicago")
        )
    }
    if('name' %in% names(sc)) {
      sc <- sc %>%
        dplyr::mutate("name" = stringi::stri_trans_general(str=.data$name, 'latin-ascii')) %>%
        dplyr::mutate("name" = replace(.data$name, .data$name == "Phoenix Coyotes", "Arizona Coyotes"),
                      "name" = replace(.data$name, .data$name == "Atlanta Thrashers", "Winnipeg Jets"),
                      "name" = replace(.data$name, .data$name == "Minnesota North Stars", "Dallas Stars"),
                      "name" = replace(.data$name, .data$name == "Quebec Nordiques", "Colorado Avalanche"),
                      "name" = replace(.data$name, .data$name == "Chicago Blackhawks", "Chicago")
        )
    }
    if('Date' %in% names(sc)){
      sc <- sc %>%
        dplyr::mutate("Date" = as.Date(.data$Date))
    }
    if('OTStatus' %in% names(sc)){
      if(any(sc$OTStatus %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "7OT", "8OT"))){
        sc[sc$OTStatus %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "7OT", "8OT"),]$OTStatus <- "OT"
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
#' @return a data frame with Round, Series, Home and Away Teams, number of wins each, playoff ranking/seed and whether the series is complete
#' @export
getAPISeries <- function(season=getCurrentSeason8()){
  series<-nhlapi::nhl_tournaments_playoffs(expand = 'round.series', seasons = as.character(season))
  playoffSeries<-data.frame("Round"=integer(), "Series"=integer(), "HomeTeam"=character(), "AwayTeam"=character(),
                            "HomeWins"=integer(), "AwayWins"=integer(), "HomeSeed"=integer(), "AwaySeed"=integer(), "requiredWins"=integer())
  for(rnd in 1:length(series[[1]]$rounds$series)){
    if('matchupTeams' %in% names(series[[1]]$rounds$series[[rnd]])){
      for(srs in 1:length(series[[1]]$rounds$series[[rnd]]$matchupTeams)){
        if(!is.null(series[[1]]$rounds$series[[rnd]]$matchupTeams[[srs]])){
          playoffSeries[nrow(playoffSeries)+1, ]<-c(rnd,srs,series[[1]]$rounds$series[[rnd]]$matchupTeams[[srs]]$team.name,
                                                    series[[1]]$rounds$series[[rnd]]$matchupTeams[[srs]]$seriesRecord.wins,
                                                    series[[1]]$rounds$series[[rnd]]$matchupTeams[[srs]]$seed.rank,
                                                    series[[1]]$rounds$format.numberOfWins[[rnd]])
        }
      }
    }
  }
  if(nrow(playoffSeries) == 0){
    return(NA)
    #stop('No Series Data Available')
  }

  playoffSeries<-clean_names(playoffSeries)

  playoffSeries$Status <- ifelse(playoffSeries$HomeWins == playoffSeries$requiredWins | playoffSeries$AwayWins == playoffSeries$requiredWins, 'Complete', 'Ongoing')
  playoffSeries$requiredWins <- NULL
  playoffSeries <- playoffSeries %>%
    dplyr::mutate("Round" = as.integer(.data$Round),
                  "Series" = as.integer(.data$Series),
                  "HomeWins" = as.integer(.data$HomeWins),
                  "AwayWins" = as.integer(.data$AwayWins),
                  "HomeSeed" = as.integer(.data$HomeSeed),
                  "AwaySeed" = as.integer(.data$AwaySeed)) %>%
    dplyr::mutate("SeriesID" = dplyr::case_when(
      .data$Round == 1 ~ .data$Series + 0, ## + 0 recasts to numeric, avoids issue, see https://github.com/tidyverse/dplyr/issues/5876
      .data$Round == 2 ~ .data$Series + 8,
      .data$Round == 3 ~ .data$Series + 12,
      .data$Round == 4 ~ .data$Series + 14
    ))
  return(playoffSeries)
}


#' getSeasonStartDate
#'
#' @param season Season (8 character code) or NULL for current/most recent season
#'
#' @return Season start date as date
#' @export
getSeasonStartDate<-function(season=NULL){
  seasons<-nhlapi::nhl_seasons()
  if(!is.null(season)){
    if(season %in% seasons$seasonId){
      return(as.Date(seasons[seasons$seasonId == season]$regularSeasonStartDate))
    } else {
      stop("Season not found: ", season)
    }
  } else {
    return(as.Date(utils::tail(seasons$regularSeasonStartDate, 1)))
  }
}


#' Get Current Season
#'
#' @return current season (as 20172018 format) based on today's date.
#' @export
getCurrentSeason8 <- function(){
  seasons<-nhlapi::nhl_seasons()
  return(utils::tail(seasons$seasonId, 1))
}


#' GetCurrentSeasonEndDate
#'
#' @param season Season (8 character code) or NULL for current/most recent season
#'
#' @return Season end date (as date)
#' @export
getSeasonEndDate<-function(season=NULL){
  seasons<-nhlapi::nhl_seasons()
  if(!is.null(season)){
    if(season %in% seasons$seasonId){
      return(as.Date(seasons[seasons$seasonId == season]$seasonEndDate))
    } else {
      stop("Season not found: ", season)
    }
  } else {
    return(as.Date(utils::tail(seasons$seasonEndDate, 1)))
  }
}

#' In Regular Season
#'
#' @param date Date to check if it's in a season
#' @param boolean Whether to return a result as TRUE/FALSE (True) or to return the seasonID if the date is in a season
#'
#' @return Either TRUE/FALSE or a seasonID/FALSE
#' @export
inRegularSeason <- function(date = Sys.Date(), boolean = TRUE){
  stopifnot(is.Date(date))
  date<-as.Date(date)
  seasons<-nhlapi::nhl_seasons()
  seasons_list<-seasons[seasons$regularSeasonStartDate <= date & seasons$regularSeasonEndDate >= date, ]
  if(boolean){
    return(ifelse(nrow(seasons_list)>0, TRUE, FALSE))
  } else {
    if(nrow(seasons_list)>0){
      return(seasons_list$seasonId)
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
inOffSeason <- function(date=Sys.Date()){
  stopifnot(is.Date(date))
  date<-as.Date(date)
  seasons<-nhlapi::nhl_seasons()
  seasons_list<-seasons[seasons$seasonEndDate > date & seasons$regularSeasonStartDate < date, ]
  return(ifelse(nrow(seasons_list)>0, FALSE, TRUE))
}

#' In Playoffs
#'
#' @description check if the date is in a playoff period. Note: playoffs are considered from the day after the regular season ends.
#'
#' @param date Date to check if it's in a playoffs period
#' @param boolean Whether to return a result as TRUE/FALSE (True) or to return the seasonID if the date is in a playoffs
#'
#' @return Either TRUE/FALSE or a seasonID/FALSE
#' @export
inPlayoffs <- function(date = Sys.Date(), boolean = TRUE){
  stopifnot(is.Date(date))
  date<-as.Date(date)
  seasons<-nhlapi::nhl_seasons()
  seasons_list<-seasons[as.Date(seasons$regularSeasonEndDate) <= date & as.Date(seasons$seasonEndDate) >= date, ]
  if(boolean){
    return(ifelse(nrow(seasons_list)>0, TRUE, FALSE))
  } else {
    if(nrow(seasons_list)>0){
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
getSeason <- function(gamedate=Sys.Date()){
  stopifnot(is.Date(gamedate))
  seasons<-nhlapi::nhl_seasons()
  gs<-function(gd, seasons){
    gd<-as.Date(gd)
    season_list<-seasons[seasons$regularSeasonStartDate<=gd & seasons$seasonEndDate >= gd, ]
    if(nrow(season_list) == 1){
      return(season_list$seasonId)
    } else {
      return(NULL)
    }
  }
  vgs<-Vectorize(FUN = gs, vectorize.args = c('gd'))


  if(length(gamedate) == 1){
    return(gs(gd = gamedate, seasons = seasons))
  } else if (length(gamedate) > 1) {
    return(unname(vgs(gd = gamedate, seasons = seasons)))
  }
}


getConferences <- function(apiteams=nhlapi::nhl_teams()){
  return(unique(apiteams$conference.name))
}

getDivisions <- function(apiteams=nhlapi::nhl_teams()){
  return(unique(apiteams$division.name))
}

getTeamConferences <- function(teams, apiteams=nhlapi::nhl_teams()){
  apiteams<-clean_names(apiteams)
  getteamconf<-function(t, apiteams){
    if(t %in% apiteams$name){
      return(apiteams[apiteams$name == t, ]$conference.name)
    } else {
      return(NA)
    }
  }

  v_getteamconf<-Vectorize(getteamconf, "t")
  teams<-clean_names(teams)
  if(length(teams) == 1){
    return(getteamconf(t = teams, apiteams = apiteams))
  } else {
    return(unname(v_getteamconf(t=teams, apiteams = apiteams)))
  }
}

getTeamDivisions <- function(teams, apiteams=nhlapi::nhl_teams()){
  apiteams<-clean_names(apiteams)
  getteamdiv<-function(t, apiteams){
    if(t %in% apiteams$name){
      return(apiteams[apiteams$name == t, ]$division.name)
    } else {
      return(NA)
    }
  }

  v_getteamdiv<-Vectorize(getteamdiv, "t")
  teams<-clean_names(teams)
  if(length(teams) == 1){
    return(getteamdiv(t = teams, apiteams = apiteams))
  } else {
    return(unname(v_getteamdiv(t=teams, apiteams = apiteams)))
  }
}

getShortTeam<-function(teams, apiteams=nhlapi::nhl_teams()){
  apiteams<-clean_names(apiteams)
  getteamshort<-function(t, apiteams){
    if(t %in% apiteams$name){
      return(apiteams[apiteams$name == t, ]$abbreviation)
    } else {
      return(NA)
    }
  }

  v_getteamshort<-Vectorize(getteamshort, "t")
  teams<-clean_names(teams)
  if(length(teams) == 1){
    return(getteamshort(t = teams, apiteams = apiteams))
  } else {
    return(unname(v_getteamshort(t=teams, apiteams = apiteams)))
  }
}
