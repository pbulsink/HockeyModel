# BradleyTerry2 models W/Ties (GenDavidson)
# https://www.slideshare.net/htstatistics/turner-use-r2013talk
# https://en.wikipedia.org/wiki/List_of_international_games_played_by_NHL_teams#1997_Mighty_Ducks_of_Anaheim.E2.80.93Vancouver_Canucks_Japanese_games
# Use Shifted & Scaled Davidson (see ?GenDavidson)

#' Update BT
#'
#' @export
updateBT <- function(){
  dataBT<-prepareDataForBT()
  fittedBT<-fitBT(btdata = dataBT)
  devtools::use_data(dataBT, overwrite = TRUE)
  devtools::use_data(fittedBT, overwrite = TRUE)
}

plotBT <- function(){NULL}

#' Predict Today's results using BT
#'
#' @param today Date to predict, optional
#' @param fittedBT The bt model (Generalized Davidson or simple BT) as HockeyModel::fittedBT or HockeyModel::fittedBTSimple
#' @param schedule the future's schedule from HockeyModel::schedule
#'
#' @return a data frame of home win, away win, and draw odds.
#' @export
todayBT <- function(today = Sys.Date(), fittedBT=HockeyModel::fittedBT, schedule = HockeyModel::schedule){
  games<-schedule[schedule$Date == today, ]
  if(nrow(games) == 0){
    return(NULL)
  }

  preds<-data.frame(HomeTeam=games$HomeTeam, AwayTeam=games$AwayTeam,
                    HomeWin=0, AwayWin = 0, Draw = 0,
                    stringsAsFactors = FALSE)

  if('gnm' %in% class(fittedBT)){ #BradleyTerry Generalized Davidson
    for(i in 1:nrow(games)){
      newdata<-data.frame(Date = today,
                          HomeTeam = as.character(games[i, 'HomeTeam']),
                          AwayTeam = as.character(games[i, 'AwayTeam']),
                          Result = c(1,0,-1))
      newdata$AtHome <- TRUE
      p<-stats::predict(object = fittedBT, newdata = newdata, type = 'response')
      p<-normalizeOdds(p)
      preds$HomeWin[[i]]<-p[[1]]
      preds$AwayWin[[i]]<-p[[3]]
      preds$Draw[[i]]<-p[[2]]
    }
  } else if ('BTm' %in% class(fittedBT)){ #BradleyTerry Simple Model
    for(i in 1:nrow(games)){
      p<-simpleBTPredict(btModel = fittedBT,
                         home.team = as.character(games[i, 'HomeTeam']),
                         away.team = as.character(games[i, 'AwayTeam']))
      preds$HomeWin[[i]]<-p - 0.20*p
      preds$AwayWin[[i]]<-(1-p) - (0.20*(1-p))
      preds$Draw[[i]]<-0.20
    }
  }

  return(preds)
}

#' BT remainder of season
#' @description Odds for each team to get to playoffs.
#'
#' @param nsims Number of simulations
#' @param scores the historical scores
#' @param schedule uplayed future games
#' @param odds Whether to return raw odds (TRUE) or go to simulation.
#' @param ... additional parameters to pass to season simulation (in parallel)
#'
#' @return data frame of Team, playoff odds.
#' @export
remainderSeasonBT <- function(nsims=10000, scores = HockeyModel::scores, schedule = HockeyModel::schedule, fittedBT = HockeyModel::fittedBT, odds = FALSE, ...){
  season_sofar<-scores[scores$Date > as.Date("2018-08-01"),]

  season_sofar <- season_sofar[c('Date','HomeTeam','AwayTeam','Result'),]
  odds_table<-data.frame(HomeTeam = character(), AwayTeam=character(),
                         HomeWin=numeric(), AwayWin=numeric(), Draw=numeric(),
                         stringsAsFactors = FALSE)

  for(d in unique(schedule$Date)){
    preds<-todayBT(today=d, fittedBT = fittedBT, schedule = schedule)
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

#' Prepare Data for BT fitting
#'
#' @param data The scores data to convert. Defaults to all HockeyModel::scores.
#'
#' @return a long data frame
#' @export
prepareDataForBT <- function(data=HockeyModel::scores){
  btscores<-data[,c('Date','HomeTeam','AwayTeam','Result')]
  btscores$Result<-round((btscores$Result*2)-1)
  btscores$Season<-as.factor(vGetSeason(btscores$Date))
  btscores<-gnm::expandCategorical(btscores, 'Result', idvar='game')
  btscores$AtHome<-!logical(nrow(btscores))
  return(btscores)
}

#' Fit BradleyTerry Generalized Davidson Model
#'
#' @param btdata data from prepareDataForBT, or defaults to all data prepared.
#' @param subset_from date from which (forward) to run BT model. Full nhl history is at least > 45 GB ram requirement (true value unknown). Runing since 1980-08-01 requires minimum 16GB RAM system RAM, process time = hours. Minimum few years required for estimate of team strength, home advantage, and tie percents
#'
#' @importFrom BradleyTerry2 GenDavidson
#' @return a gnm fit model
#' @export
fitBTGD <- function(btdata=prepareDataForBT(), subset_from = '2016-08-01'){
  if(as.Date(subset_from)>Sys.Date()){
    stop('subset_from date must be in past')
  }
  btdata<-btdata[btdata$Date > as.Date(subset_from), ]
  btdata<-droplevels(btdata)
  fittedBT<-gnm::gnm(count ~
                       GenDavidson(Result == 1, Result == 0, Result == -1,
                                                  HomeTeam, AwayTeam,
                                                  home.adv = ~1,
                                                  tie.max = ~1,
                                                  tie.scale = ~1,
                                                  tie.mode = ~1,
                                                  at.home1 = AtHome,
                                                  at.home2 = !AtHome
                       ) - 1,
                     #eliminate = quote(game),
                     family = stats::poisson,
                     data = btdata,
                     subset = quote(Date) > as.Date(subset_from))
  gc(verbose = FALSE)
  return(fittedBT)
}

#' Simple BT Predict Game
#'
#' @param btSimple simple Bradley Terry Model
#' @param home.team text home team
#' @param away.team text away team
#'
#' @return odd for a home team win from 0-1
#' @export
simpleBTPredict<-function(btModel, home.team, away.team){
  bthome<-btModel$coefficients[['at.home']]
  bta<-BradleyTerry2::BTabilities(btModel)[away.team,][[1]]
  bth<-BradleyTerry2::BTabilities(btModel)[home.team,][[1]]

  lambda<-bth-bta + bthome

  return(exp(lambda)/(1+exp(lambda)))
}

#' Fit Simple BradleyTerry Model
#'
#' @param btdata data from prepareDataForBT, or defaults to all data prepared.
#' @param subset_from date from which (forward) to run BT model.
#'
#' @return a gnm fit model
#' @export
fitBTSimple <- function(scores=HockeyModel::scores, subset_from = as.Date('2016-08-01')){
  if(!is.null(subset_from)){
    if(as.Date(subset_from)>Sys.Date()){
      stop('subset_from date must be in past')
    }
    sc<-scores[scores$Date > as.Date(subset_from), ]
  } else {
    sc<-scores
  }

  #simple BT cannot handle ties
  sc<-sc[sc$Result != 0.5,]
  sc$Result <- round(sc$Result)

  sc$Season<-as.factor(HockeyModel:::vGetSeason(sc$Date))

  sc<-droplevels(sc)

  sc<-sc %>%
    dplyr::mutate(home.team = HomeTeam,
                  away.team = AwayTeam) %>%
    dplyr::group_by(home.team, away.team, Season) %>%
    dplyr::summarise(home.wins = sum(Result>0.5), away.wins = sum(Result<0.5)) %>%
    as.data.frame()

  sc$home.team<-data.frame(team = sc$home.team, at.home = 1)
  sc$away.team<-data.frame(team = sc$away.team, at.home = 0)

  fittedBTSimple<-BradleyTerry2::BTm(outcome = cbind(home.wins, away.wins),
                               player1 = home.team,
                               player2 = away.team,
                               data = sc,
                               formula = ~team + at.home,
                               id = "team")

  return(fittedBTSimple)
}

#' Plot the BT GD model Fit
#'
#' @param fittedBT the HockeyModel::fittedBT Model
#' @param dataBT the HockeyModel::dataBT data
#'
#' @return a baseplot from BradleyTerry2
#' @export
plotBTGDModelFit <- function(fittedBT=HockeyModel::fittedBT, dataBT=HockeyModel::dataBT){
  coef <- stats::coef(fittedBT)
  alpha <- names(coef[-(1:4)])
  BradleyTerry2::plotProportions(quote(Result) == 1, quote(Result) == 0, quote(Result) == -1,
                                 quote(HomeTeam:Season), quote(AwayTeam:Season),
                                 abilities = coef[alpha], home.adv = coef["home.adv"],
                                 tie.max = coef["tie.max"], tie.scale = coef["tie.scale"],
                                 tie.mode = coef["tie.mode"],
                                 at.home1 = quote(AtHome), at.home2 = !quote(AtHome),
                                 data = dataBT, subset = quote(count) == 1)
}
