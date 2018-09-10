#' PythagoreanPuck remainder of season
#' @description Odds for each team to get to playoffs.
#'
#' @param nsims Number of simulations
#' @param scores the historical scores
#' @param schedule uplayed future games
#' @param odds whether to return odds table or predictions
#' @param ... arguements to pass to elo predictor & simulateSeason
#'
#' @return data frame of Team, playoff odds.
#' @export
remainderSeasonPP <- function(nsims=10000, scores = HockeyModel::scores, schedule = HockeyModel::schedule, odds = FALSE, ...){
  odds_table<-data.frame(HomeTeam = character(), AwayTeam=character(),
                         HomeWin=numeric(), AwayWin=numeric(), Draw=numeric(),
                         stringsAsFactors = FALSE)

  for(d in unique(schedule$Date)){
    preds<-todayELO(today=d, schedule = schedule, ...)
    preds$Date <- d
    odds_table<-rbind(odds_table, preds)
  }
  odds_table$Date<-schedule$Date

  if(odds){
    return(odds_table)
  }

  summary_results <- simulateSeasonParallel(odds_table = odds_table, nsims = nsims, scores = scores, schedule = schedule, ...)

  return(summary_results)
}

#' PythagoreanPuck Predictions Today
#'
#' @param today Generate predictions for this date. Defaults to today
#' @param schedule shcedule to use, if not the builtin
#'
#' @return a data frame of HomeTeam, AwayTeam, HomeWin, AwayWin, Draw, or NULL if no games today
#' @export
todayPP <- function(today = Sys.Date(), schedule = HockeyModel::schedule){
  games<-schedule[schedule$Date == today, ]
  if(nrow(games) == 0){
    return(NULL)
  }

  preds<-data.frame(HomeTeam=games$HomeTeam, AwayTeam=games$AwayTeam,
                    HomeWin=0, AwayWin = 0, Draw = 0,
                    stringsAsFactors = FALSE)

  for(i in 1:nrow(preds)){
    p<-predictPPResult()
    preds$HomeWin[[i]]<-p$HomeWin
    preds$AwayWin[[i]]<-p$AwayWin
    preds$Draw[[i]]<-p$Draw
  }

  return(preds)
}

predictPPResult<-function(){
  return(list(HomeWin = 0.4, AwayWin = 0.4, Draw = 0.2))
}

PPModel<-function(scores=HockeyModel::scores){
  scores$Season <- vGetSeason(scores$Date)
  history<-data.frame(Season = unique(scores$Season))
  history[c(make.names(paste0(unique(scores$HomeTeam), '.win.p')))] <- NA
  history[c(make.names(paste0(unique(scores$HomeTeam), '.exp.p')))] <- NA
  for(t in unique(scores$HomeTeam)){
    for(season in unique(scores$Season)){
      s<-scores[scores$Season == season, ]
      history[history$Season == season, make.names(paste0(t, '.win.p'))]<-winPercent(team = t, scores = scores, season = season)
      history[history$Season == season, make.names(paste0(t, '.exp.p'))]<-ePr(team = t, scores = s, season = season)
    }
  }
  return(history)
}

winPercent<-function(team, scores = HockeyModel::scores, season="20172018"){
  if(!is.null(season)){
    if(!'Season' %in% colnames(scores)){
      scores$Season <- vGetSeason(scores$Date)
    }
    scores<-scores[scores$Season == season, ]
  }
  sc<-scores
  homewins <- nrow(sc[(sc$HomeTeam == team & sc$Result > 0.5), ])
  awaywins <- nrow(sc[(sc$AwayTeam == team & sc$Result < 0.5), ])
  draws <- nrow(sc[(sc$HomeTeam == team & sc$Result == 0.5),])/2

  games <- nrow(sc[(sc$HomeTeam == team | sc$AwayTeam == team),])

  return((homewins + awaywins + draws)/games)
}

ePr<-function(team, scores = HockeyModel::scores, season="20172018"){
  if(!is.null(season)){
    if(!'Season' %in% colnames(scores)){
      scores$Season <- vGetSeason(scores$Date)
    }
    scores<-scores[scores$Season == season, ]
  }
  sc<-scores
  GF<-sum(c(sc[sc$HomeTeam == team, 'HomeGoals'], sc[sc$AwayTeam == team, 'AwayGoals']))
  ng<-nrow(sc[(sc$HomeTeam == team | sc$AwayTeam == team), ])
  GA<-sum(c(sc[sc$HomeTeam == team, 'AwayGoals'], sc[sc$AwayTeam == team, 'HomeGoals']))
  GFg<-GF/ng
  GAg<-GA/ng

  E<-(GFg+GAg)^0.458
  return((GF^E)/((GF^E)+(GA^E)))
}


