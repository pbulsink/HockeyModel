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
#' @param btdata The original btdata frame from HockeyModel::dataBT
#' @param fittedBT The bt model as HockeyModel::fittedBT
#' @param schedule the future's schedule from HockeyModel::schedule
#'
#' @export
todayBT <- function(today = Sys.Date(), btdata=HockeyModel::dataBT, fittedBT=HockeyModel::fittedBT, schedule = HockeyModel::schedule){
  games<-schedule[schedule$Date == today, ]
  if(nrow(games) == 0){
    return(NULL)
  }

  preds<-data.frame(HomeTeam=games$HomeTeam, AwayTeam=games$AwayTeam,
                    HomeWin=0, AwayWin = 0, Draw = 0,
                    stringsAsFactors = FALSE)

  for(i in 1:nrow(games)){
    newdata<-data.frame(Date = today,
                        HomeTeam = factor(games[i, 'HomeTeam'], levels = levels(btdata$HomeTeam)),
                        AwayTeam = factor(games[i, 'AwayTeam'], levels = levels(btdata$AwayTeam)),
                        Season = factor("20172018", levels = levels(btdata$Season)),
                        Result = factor(c(1,0,-1), levels = levels(btdata$Result)),
                        game = as.factor("63071")) # picky on this game numberbeing in btdata, but doesn't actually impact result. weird.
    newdata$AtHome <- TRUE
    p<-stats::predict(object = fittedBT, newdata = newdata, type = 'response')
    p<-normalizeOdds(p)
    preds$HomeWin[[i]]<-p[[1]]
    preds$AwayWin[[i]]<-p[[3]]
    preds$Draw[[i]]<-p[[2]]
  }

  return(preds)
}

#' BT remainder of season
#' @description Odds for each team to get to playoffs.
#'
#' @param nsims Number of simulations
#' @param scores the historical scores
#' @param schedule uplayed future games
#'
#' @return data frame of Team, playoff odds.
#' @export
remainderSeasonBT <- function(nsims=10000, scores = HockeyModel::scores, schedule = HockeyModel::schedule){
  season_sofar<-scores[scores$Date > as.Date("2018-08-01"),]

  season_sofar <- season_sofar[c('Date','HomeTeam','AwayTeam','Result'),]
  odds_table<-data.frame(HomeTeam = character(), AwayTeam=character(),
                         HomeWin=numeric(), AwayWin=numeric(), Draw=numeric(),
                         stringsAsFactors = FALSE)

  for(d in unique(schedule$Date)){
    preds<-todayBT(today=d)
    preds$Date <- d
    odds_table<-rbind(odds_table, preds)
  }
  odds_table$Date<-schedule$Date

  summary_results <- simulateSeason(odds_table, nsims, scores, schedule)

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
fitBT <- function(btdata=prepareDataForBT(), subset_from = '2008-08-01'){
  if(as.Date(subset_from)>Sys.Date()){
    stop('subset_from date must be in past')
  }
  fittedBT<-gnm::gnm(count ~
                       GenDavidson(Result == 1, Result == 0, Result == -1,
                                                  HomeTeam:Season, AwayTeam:Season,
                                                  home.adv = ~1,
                                                  tie.max = ~1,
                                                  tie.scale = ~1,
                                                  tie.mode = ~1,
                                                  at.home1 = AtHome,
                                                  at.home2 = !AtHome
                       ) - 1,
                     eliminate = quote(game),
                     family = stats::poisson,
                     data = btdata,
                     subset = quote(Date) > as.Date(subset_from))
  gc(verbose = FALSE)
  return(fittedBT)
}

#' Plot the BT model Fit
#'
#' @param fittedBT the HockeyModel::fittedBT Model
#' @param dataBT the HockeyModel::dataBT data
#'
#' @return a baseplot from BradleyTerry2
#' @export
plotBTModelFit <- function(fittedBT=HockeyModel::fittedBT, dataBT=HockeyModel::dataBT){
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
