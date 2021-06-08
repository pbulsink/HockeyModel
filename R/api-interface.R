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
  sched<-getNHLSchedule(getSeason())
  stopifnot(!is.null(sched))
  gameIDs<-sched$GameID
  schedule<-schedule %>%
    dplyr::filter(!(.data$GameID %in% gameIDs)) %>%
    dplyr::bind_rows(sched) %>%
    dplyr::arrange(.data$Date, dplyr::desc(.data$GameState), .data$GameID)
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
    ))

  return(scores)
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
    neededGames<-schedule[schedule$Date > getCurrentSeasonStartDate(), ]$GameID
  } else {
    neededGames<-schedule[schedule$Date < Sys.Date(), ]$GameID
    neededGames<-neededGames[!neededGames %in% scores[scores$GameStatus == 'Final', ]$GameID]
  }
  if(length(neededGames)>0){
    updatedSc<-getNHLScores(neededGames)
    scores<-scores %>%
      dplyr::filter(!(.data$GameID %in% neededGames)) %>%
      dplyr::bind_rows(updatedSc) %>%
      dplyr::arrange(.data$GameID)
    if (save_data){
      suppressMessages(usethis::use_data(scores, overwrite=TRUE))
    }
  } else {
    message("Scores are updated to today's date already.")
  }
  return(scores)
}


clean_names<-function(sc){
  sc <- sc %>%
    dplyr::mutate("HomeTeam" = stringi::stri_trans_general(str=.data$HomeTeam, 'latin-ascii'),
                  "AwayTeam" = stringi::stri_trans_general(str=.data$AwayTeam, 'latin-ascii')) %>%
    dplyr::mutate("HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Phoenix Coyotes", "Arizona Coyotes"),
                  "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Atlanta Thrashers", "Winnipeg Jets"),
                  "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Minnesota North Stars", "Dallas Stars"),
                  "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Quebec Nordiques", "Colorado Avalanche"),
                  "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Phoenix Coyotes", "Arizona Coyotes"),
                  "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Atlanta Thrashers", "Winnipeg Jets"),
                  "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Minnesota North Stars", "Dallas Stars"),
                  "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Quebec Nordiques", "Colorado Avalanche"),
                  "HomeTeam" = replace(.data$HomeTeam, .data$HomeTeam == "Chicago Blackhawks", "Chicago"),
                  "AwayTeam" = replace(.data$AwayTeam, .data$AwayTeam == "Chicago Blackhawks", "Chicago")
                  )
  if('Date' %in% names(sc)){
    sc <- sc %>%
      dplyr::mutate("Date" = as.Date(.data$Date))
  }
  if('OTStatus' %in% names(sc)){
    sc[sc$OTStatus %in% c("2OT", "3OT", "4OT", "5OT", "6OT", "7OT", "8OT"),]$OTStatus <- "OT"
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
                            "HomeWins"=integer(), "AwayWins"=integer(), "HomeSeed"=integer(), "AwaySeed"=integer(), requiredWins=integer())
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
    stop('No Series Data Available')
  }

  playoffSeries<-clean_names(playoffSeries)

  playoffSeries$Status <- ifelse(playoffSeries$HomeWins == playoffSeries$requiredWins | playoffSeries$AwayWins == playoffSeries$requiredWins, 'Complete', 'Ongoing')
  playoffSeries$requiredWins <- NULL
  return(playoffSeries)
}
