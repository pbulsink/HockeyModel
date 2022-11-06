## Iterative (instead of time-weighted) dixon-coles style (poisson based) model.
# Based on work by Jan Lasek and Marek Gagolewski. See dx.doi.org/10.1016/j.ijforecast.2020.11.008
# See also https://github.com/janekl/iterative-rating-systems

# default ranking for unknown teams is attack, defence = 0
# Regular: mix=1 = fully xG, mix=0 = fully goals


updateIterativeRankings <- function(home_goals, away_goals, home_xg=NULL, away_xg=NULL,
                            home_attack = 0, home_defence = 0, away_attack = 0, away_defence = 0,
                            home_adv = 0.075073, intercept = 1.71418, gamma = 0.005142, lambda = 0.01973,
                            rho=1, attack_mix=1, defend_mix=0) {

  # Calculate poisson expected goals
  mu_h <- exp(intercept + home_attack - away_defence + home_adv)
  mu_a <- exp(intercept + away_attack - home_defence)

  #Update factors
  if(is.null(home_xg) | is.null(away_xg)){
    update <- c(home_goals-mu_h, -(away_goals-mu_a), away_goals-mu_a, -(home_goals-mu_h))
  } else {
    home_a <- stats::weighted.mean(c(home_goals, home_xg), c(1-attack_mix, attack_mix))
    away_a <- stats::weighted.mean(c(away_goals, away_xg), c(1-attack_mix, attack_mix))
    home_d <- stats::weighted.mean(c(away_goals, away_xg), c(1-defend_mix, defend_mix))
    away_d <- stats::weighted.mean(c(home_goals, home_xg), c(1-defend_mix, defend_mix))

    update <- c( home_a - mu_h, -(home_d-mu_a), away_a-mu_a, -(away_d-mu_h))
  }

  va_h <- gamma*(update[1] - lambda * (home_attack - rho*home_defence))
  vd_h <- gamma*(update[2] - lambda * (home_defence - rho*home_attack))
  va_a <- gamma*(update[3] - lambda * (away_attack - rho*away_defence))
  vd_a <- gamma*(update[4] - lambda * (away_defence - rho*away_attack))

  return(c(home_attack+va_h, home_defence+vd_h, away_attack+va_a, away_defence+vd_a))
}


#' Get Team Iterative Rankings
#'
#' @description Gets a team's attack and defense ratings for iterative dixon-coles techniques. If a team hasn't been sampled before in this
#' given rankings data set, attack and defense values of 0 are returned, and the team added to the rankings data frame. The rankings
#' data frame is always returned as well, it's good tocapture that in case the team is new.
#'
#' @param team Team name
#' @param rankings rankings data frame
#'
#' @return attack & defence ratings, plust the rankings data frame again (in case a team is added with attack & defense of 0)
getTeamRankings <- function(team, rankings){
  if(team %in% rankings$Team){
    attack <- rankings[rankings$Team == team, ]$Attack
    defence <- rankings[rankings$Team == team, ]$Defence
  } else {
    attack <- defence <- 0
    rankings<-dplyr::bind_rows(rankings, data.frame("Team" = team, 'Attack' = 0, 'Defence' = 0))
  }

  return(list('attack'=attack, 'defence' = defence, 'rankings' = rankings))
}


iterateGame <- function(game, rankings, make_predictions=FALSE, intercept=0.02, gamma = 0.0036, lambda = .000099, rho=0, home_adv = 0.0707, attack_mix=0, defend_mix=1){
  home_team <- game$HomeTeam
  away_team <- game$AwayTeam

  r <- getTeamRankings(home_team, rankings)
  home_attack <- r$attack
  home_defence <- r$defence
  rankings <- r$rankings

  r <- getTeamRankings(away_team, rankings)
  away_attack <- r$attack
  away_defence <- r$defence
  rankings <- r$rankings

  if (make_predictions){
    iter<-iterativeGamePredict(home = home_team, away = away_team,
                               rankings = rankings, intercept = intercept,
                               home_adv = home_adv, probs_style = 'h')
    probs<-iter$Probs[1]
    home_xg <- iter$HomexGPred
    away_xg <- iter$AwayxGPred
  } else {
    probs<-home_xg<-away_xg<-NA
  }

  new_ranks <- updateIterativeRankings(home_goals = game$HomeGoals, away_goals = game$AwayGoals,
                               home_xg = game$HomexG, away_xg = game$AwayxG,
                               home_attack = home_attack, home_defence = home_defence, away_attack = away_attack,
                               away_defence = away_defence, home_adv = home_adv,intercept = intercept, gamma = gamma,
                               lambda = lambda, rho=rho, attack_mix = attack_mix, defend_mix = defend_mix)

  rankings[rankings$Team == home_team, ]$Attack <- new_ranks[1]
  rankings[rankings$Team == home_team, ]$Defence <- new_ranks[2]
  rankings[rankings$Team == away_team, ]$Attack <- new_ranks[3]
  rankings[rankings$Team == away_team, ]$Defence <- new_ranks[4]

  #TODO RETURN Rankings
  return(list('Rankings' = rankings, 'HomeWin' = probs, 'HomexGPred' = home_xg, 'AwayxGPred' = away_xg))
}

iterateSeason <- function(intercept=1.71418, gamma = 0.005142, lambda = 0.01973, rho=0, home_adv = 0.075073, attack_mix=0, defend_mix=1, scores = HockeyModel::scores, verbose = TRUE){

  scores <- scores[,c("Date", "HomeTeam" ,"AwayTeam", "GameID", "HomeGoals", "AwayGoals", "HomexG", "AwayxG", "OTStatus", "Result")]

  if(!requireNamespace('tictoc')) {
    verbose <- FALSE
  }
  scores$TotalGoals<-scores$HomeGoals + scores$AwayGoals

  #Split scores to warm-up phase and predictive phase.
  warm_ups<-scores[scores$Date <= as.Date(Sys.Date()-365*3) & scores$Date > as.Date("2011-08-01"), ]
  train <-scores[scores$Date < as.Date(Sys.Date()-365) & scores$Date > as.Date(Sys.Date()-365*3), ]
  test <- scores[scores$Date >= as.Date(Sys.Date()-365),]

  train$HomeWin <- train$HomexGPred <- train$AwayxGPred <- train$TotalxGPred <- NA_real_
  test$HomeWin <- test$HomexGPred <- test$AwayxGPred <- test$TotalxGPred <- NA_real_

  #Typical use -> warm up the rankings from first seasons
  rankings <- data.frame("Team" = character(), "Attack"=numeric(), "Defence"=numeric())
  if(verbose) tictoc::tic("Warmup")
  for (g in 1:nrow(warm_ups)){
    iters<-iterateGame(game = warm_ups[g,], rankings = rankings, make_predictions = FALSE, intercept = intercept, gamma = gamma,
                       lambda = lambda, rho = rho, home_adv = home_adv, attack_mix = attack_mix, defend_mix = defend_mix)

    rankings<-iters$Rankings
  }
  if(verbose) tictoc::toc()

  #Then record predictions and update simultaneously

  if(verbose) tictoc::tic("Train")
  for (g in 1:nrow(train)){
    iters<-iterateGame(game = train[g,], rankings = rankings, make_predictions = TRUE, intercept = intercept, gamma = gamma,
                       lambda = lambda, rho = rho, home_adv = home_adv, attack_mix = attack_mix, defend_mix = defend_mix)

    rankings<-iters$Rankings
    train[g,]$HomeWin <- iters$HomeWin
    train[g,]$HomexGPred <- iters$HomexGPred
    train[g,]$AwayxGPred <- iters$AwayxGPred
    train[g,]$TotalxGPred <- iters$HomexGPred + iters$AwayxGPred
  }
  if(verbose) tictoc::toc()

  # Test against a hold out season
  if(verbose) tictoc::tic("Test")
  for (g in 1:nrow(test)){
    iters<-iterateGame(game = test[g,], rankings = rankings, make_predictions = TRUE, intercept = intercept, gamma = gamma,
                       lambda = lambda, rho = rho, home_adv = home_adv, attack_mix = attack_mix, defend_mix = defend_mix)

    rankings<-iters$Rankings
    test[g,]$HomeWin <- iters$HomeWin
    test[g,]$HomexGPred <- iters$HomexGPred
    test[g,]$AwayxGPred <- iters$AwayxGPred
    test[g,]$TotalxGPred <- iters$HomexGPred + iters$AwayxGPred
  }
  if(verbose) tictoc::toc()

  if(verbose){
    message("Train LL: ", logLoss(train$HomeWin, as.integer(train$Result>0.5)))
    message("Train Accuracy: ", accuracy(train$HomeWin, train$Result))
    message("Train AUC: ", auc(train$HomeWin, train$Result))
    message("Train xG R2: ", rsquare(train$TotalxGPred, train$TotalGoals))
    message("Train xG RMSE: ", rmse(train$TotalxGPred, train$TotalGoals))
    message("---")
    message("Test LL: ", logLoss(test$HomeWin, as.integer(test$Result>0.5)))
    message("Test Accuracy: ", accuracy(test$HomeWin, test$Result))
    message("Test AUC: ", auc(test$HomeWin, test$Result))
    message("Test xG R2: ", rsquare(test$TotalxGPred, test$TotalGoals))
    message("Test xG RMSE: ", rmse(test$TotalxGPred, test$TotalGoals))
  }

  return(list(rankings=rankings, test=test, train=train))
}


#' Optimize Iterative Dixon-Coles
#'
#' @description Call this to optimize the iterative dixon-coles method.
#'
#' @param target_model Default 'wl' for targeting optimization of the win/loss model. Also accepts 'xG'
#' @param params The starting point for Parameter optimization
#'
#' @return the results of `optim` including parameters
optimizeIterative<-function(target_model = "wl", params=HockeyModel::iterativeParameters){
  if(!(target_model %in% c('wl', 'WL', 'Wl', 'wL', 'xg', 'xG', 'XG', 'Xg'))){
    stop("HockeyModel:::optimizeIterative: Target model must be either 'wl' or 'xG'")
  }
  if(target_model %in% c('wl','Wl', 'wL','WL')){
    params <- params$params_wl
    optfunc<-optimizeIterative_WL_Internal
    fnscale<-1
  } else {
    params <- params$params_xg
    optfunc<-optimizeIterative_XG_Internal
    fnscale<-1
  }

  optimized<-stats::optim(c(params$intercept,params$gamma,params$lambda,params$rho,params$home_adv,params$attack_mix,params$defend_mix), optfunc, control=list('fnscale'=fnscale))

  return(optimized)
}


optimizeIterative_WL_Internal<-function(params){

  intercept<-params[1]
  gamma<-params[2]
  lambda<-params[3]
  rho<-params[4]
  home_adv<-params[5]
  attack_mix<-params[6]
  defend_mix<-params[7]
  if(intercept < 0){
    return(0)
  }
  if(rho < -1 | rho > 1){
    return(1e5)
  }
  if (attack_mix < 0 | attack_mix > 1){
    return(1e5)
  }
  if (defend_mix < 0 | defend_mix > 1){
    return(1e5)
  }
  if (gamma < 0 | gamma > 0.1){
    return(1e5)
  }
  if (home_adv < -1 | home_adv > 1){
    return(1e5)
  }
  if(lambda < 0){
    return(1e5)
  }

  out<-iterateSeason(intercept=intercept, gamma = gamma, lambda = lambda, rho=rho, home_adv = home_adv, attack_mix=attack_mix, defend_mix=defend_mix)
  pre<-out$train
  ll<-logLoss(pre$HomeWin, pre$Result)
  acc<-accuracy(pre$HomeWin, pre$Result)
  return(ll/acc)
}

optimizeIterative_XG_Internal<-function(params){
  intercept<-params[1]
  gamma<-params[2]
  lambda<-params[3]
  rho<-params[4]
  home_adv<-params[5]
  attack_mix<-params[6]
  defend_mix<-params[7]
  if(intercept < 0){
    return(0)
  }
  if(rho < -1 | rho > 1){
    return(1e5)
  }
  if (attack_mix < 0 | attack_mix > 1){
    return(1e5)
  }
  if (defend_mix < 0 | defend_mix > 1){
    return(1e5)
  }
  if (gamma < 0){
    return(1e5)
  }
  if (home_adv < -1 | home_adv > 1){
    return(1e5)
  }
  if(lambda < 0){
    return(1e5)
  }

  out<-iterateSeason(intercept=intercept, gamma = gamma, lambda = lambda, rho=rho, home_adv = home_adv, attack_mix=attack_mix, defend_mix=defend_mix)
  pre<-out$train
  rmse<-rmse(pre$TotalxGPred, pre$TotalGoals)
  return(rmse)
}

#' Predict the outcome of one game using iterative Dixon Coles model
#'
#' @param home The Home Team
#' @param away The Away Team
#' @param rankings A rankings dataframe
#' @param intercept Modelled 'intercept' value
#' @param home_adv Modelled 'home_adv' value
#' @param probs_style either `'h'` (default) or one of `'ha'`, `'hda`, or `'hdda'`
#'
#' @return a named list with Probs (a vector), HomexGPred and AwayxGPred (values)
#' @export
iterativeGamePredict <- function(home, away, rankings, intercept = 1.71418, home_adv = 0.075073, probs_style='h'){

  stopifnot(probs_style %in% c('h', 'ha', 'hda', 'hdda'))

  r <- getTeamRankings(home, rankings)
  home_attack <- r$attack
  home_defence <- r$defence
  rankings <- r$rankings

  r <- getTeamRankings(away, rankings)
  away_attack <- r$attack
  away_defence <- r$defence
  rankings <- r$rankings

  # Calculate poisson expected goals
  mu_h <- exp(intercept + home_attack - away_defence + home_adv)
  mu_a <- exp(intercept + away_attack - home_defence)
  pm <- prob_matrix(mu_h, mu_a, params=NULL, maxgoal=10)

  # This is home, draw, away win
  h<-sum(pm[lower.tri(pm)])
  a<-sum(pm[upper.tri(pm)])
  d<-1-h-a
  if(probs_style == 'h'){
    probs<-c(sum(extraTimeSolver(h,a,d)[c(1,2)]))
  } else if (probs_style == 'ha'){
    et<-extraTimeSolver(h,a,d)
    probs<-c(sum(et[c(1,2)]), sum(et[c(3,4)]))
  } else if (probs_style == 'hda'){
    et<-extraTimeSolver(h,a,d)
    probs<-c(et[1], sum(et[c(2,3)]), et[4])
  } else if (probs_style == 'hdda'){
    probs<-extraTimeSolver(h,a,d)
  }

  away_xg<-stats::weighted.mean(0:10, colSums(pm))
  home_xg<-stats::weighted.mean(0:10, rowSums(pm))
  return(list('Probs' = probs, 'HomexGPred' = home_xg, 'AwayxGPred' = away_xg))

}


saveIterativePredictions<-function(rankings, params, filepath="./prediction_results/iterative_ranking"){
  if(!file.exists(filepath)){
    dir.create(filepath)
  }
  rankparams<-list("Rankings" = rankings, "Params" = params)
  saveRDS(rankparams, file=file.path(filepath, paste0(Sys.Date(), "-iterative-rankings.RDS")))
}


readIterativePredictions<-function(filename, filepath = "./prediction_results/iterative_ranking"){
  if(file.exists(file.path(filepath, filename))){
    rankparams<-readRDS(file.path(filepath, filename))
    rankings<-rankparams$Rankings
    params<-rankparams$Params
  } else {
    stop("HockeyModel:::read_iterative_rankings: File ", filename, " doesn't exist at location ", filepath, ".")
  }
  return(list("Rankings" = rankings, "Params" = params))
}


getIterativePredictions<-function(rankings, params, date = Sys.Date(), schedule=HockeyModel::schedule){
  stopifnot(is.Date(date))
  games<-schedule[schedule$Date == date, ]
  if(nrow(games) == 0){
    return(NULL)
  }
  results<-data.frame("GameID" = integer(), "HomeTeam" = character(), "AwayTeam" = character(),
                      "HomeWin" = numeric(), "AwayWin" = numeric(), "TotalxG" = numeric())
  for(g in 1:nrow(games)){
    i<-iterateGame(game = games[g,], rankings = rankings, make_predictions = TRUE, intercept = params$intercept,
                   gamma = params$gamma, lambda = params$lambda, rho = params$rho, home_adv = params$home_adv)
    rankings<-i$Rankings
    results[nrow(results)+1,] <- c(games[g,]$GameID, games[g,]$HomeTeam, games[g,]$AwayTeam, i$HomeWin, 1-i$HomeWin, i$HomexGPred + i$AwayxGPred)
  }

  results$HomeWin <- as.numeric(results$HomeWin)
  results$AwayWin <- as.numeric(results$AwayWin)
  results$TotalxG <- as.numeric(results$TotalxG)

  return(list("rankings"=rankings, "results"=results))
}


updateRankingsToToday <- function(rankings, params, scores=HockeyModel::scores, rankings_date = getSeasonStartDate()){
  scores<-scores[scores$Date >= rankings_date, ]

  if (nrow(scores) > 0){
    for (g in 1:nrow(scores)){
      iters<-iterateGame(game = scores[g,], rankings = rankings, make_predictions = FALSE, intercept = params$intercept, gamma = params$gamma,
                         lambda = params$lambda, rho = params$rho, home_adv = params$home_adv, attack_mix = params$attack_mix, defend_mix = params$defend_mix)

      rankings<-iters$Rankings
    }
  }
  return(rankings)
}


#' Get Iterative Predictions
#'
#' @description Get predictions of all games on a date. Return win/loss and total xG from the Dixon-Coles iterative model
#'
#' @param date Game date to get results for
#' @param schedule The HockeyModel::schedule object
#' @param scores The HockeyModel::scores object
#' @param rankings The HockeyModel::iterativeRankings object
#' @param params The HockeyModel:::iterativeParameters object
#'
#' @return a data.frame with colums for GameID, Home & Away Teams, Home & Away win percentage, and TotalxG predicted
#' @export
getIterativeTable<-function(date, schedule=HockeyModel::schedule,  scores=HockeyModel::scores, rankings=HockeyModel::iterativeRankings, params=HockeyModel::iterativeParameters){
  params_xg <- params$params_xg
  params_wl <- params$params_wl

  rankings_xg <- updateRankingsToToday(rankings=rankings$rankings_xg, params=params_xg, scores=scores, rankings_date=rankings$date)
  rankings_wl <- updateRankingsToToday(rankings=rankings$rankings_wl, params=params_wl, scores=scores, rankings_date=rankings$date)

  results_xg<-getIterativePredictions(rankings = rankings_xg, params = params_xg, date = date, schedule=schedule)$results
  results_wl<-getIterativePredictions(rankings = rankings_wl, params = params_wl, date = date, schedule=schedule)$results

  results_xg<-results_xg[,c("GameID", "TotalxG")]
  results_wl$TotalxG <- NULL

  results<-dplyr::left_join(results_wl, results_xg, by="GameID")
  return(results)

}


iterateiveDailyUpdate<-function(){
  scores<-updateScoresAPI()
  schedule<-updateScheduleAPI()
  results<-getIterativeTable(date=Sys.Date(), schedule=schedule, scores=scores)
  return(results)
}


iterativeOddsTable<-function(schedule=HockeyModel::schedule, rankings=HockeyModel::iterativeRankings, params=HockeyModel::iterativeParameters){
  odds_table<-data.frame("GameID" = integer(), "HomeTeam" = character(), "AwayTeam" = character(),
                         "HomeWin" = numeric(), "AwayWin" = numeric(), "Draw" = numeric())

  params<-params$params_wl
  rankings<-rankings$rankings_wl

  schedule<-schedule[schedule$Date >= getSeasonStartDate(),]

  for(g in 1:nrow(schedule)){
    i<-iterativeGamePredict(home = schedule[g,]$HomeTeam, away = schedule[g,]$AwayTeam, rankings = rankings, intercept = params$intercept, home_adv = params$home_adv, probs_style = 'hda')
    odds_table[nrow(odds_table)+1,] <- c(schedule[g,]$GameID, schedule[g,]$HomeTeam, schedule[g,]$AwayTeam,
                                         as.numeric(i$Probs[1]), as.numeric(i$Probs[3]), as.numeric(i$Probs[2]))
  }
  odds_table$Date<-schedule$Date
  odds_table$HomeWin<-as.numeric(odds_table$HomeWin)
  odds_table$AwayWin<-as.numeric(odds_table$AwayWin)
  odds_table$Draw <- as.numeric(odds_table$Draw)
  return(odds_table)
}

getNewIterativeRankings<-function(target_model = "wl", params = HockeyModel::iterativeParameters){
  stopifnot(target_model %in% c('wl', 'WL', 'xG', 'xg', 'XG'))

  if(target_model %in% c('wl', 'WL')){
    params<-params$params_wl
  } else {
    params<-params$params_xg
  }

  return(iterateSeason(intercept = params$intercept, gamma = params$gamma, lambda = params$lambda, rho = params$rho, home_adv = params$home_adv, attack_mix = params$attack_mix, defend_mix = params$defend_mix)$rankings)
}

#' Get (and optionally save to package) iterative rankings
#'
#' @param params input parameters to use for retraining
#' @param save_data whether to save the new parameters to package
#'
#' @return invisibly, a new set of rankings
#' @export
getReplacementRankings<-function(params = HockeyModel::iterativeParameters, save_data=FALSE){
  rankings_wl<-getNewIterativeRankings('wl', params = params)
  rankings_xg<-getNewIterativeRankings('xg', params = params)
  iterativeRankings <- list('rankings_wl' = rankings_wl, 'rankings_xg' = rankings_xg, 'rankings_date' = Sys.Date())
  if(save_data & requireNamespace('usethis', quietly = TRUE)){
    usethis::use_data(iterativeRankings, overwrite = T)
  }
  invisible(iterativeRankings)
}

#' Get replacement/new iterative parameters
#'
#' @param params parameters to start with.
#' @param save_data whether to save data to the package. Default false
#'
#' @return invisibly, new parameters
#' @export
getReplacementIterativeParameters <- function(params = HockeyModel::iterativeParameters, save_data=FALSE){
  optim_wl<-optimizeIterative('wl', params = params)
  optim_xg<-optimizeIterative('xg', params = params)
  params_wl <- list(intercept = optim_wl$params[1], gamma = optim_wl$par[2], lambda = optim_wl$par[3], rho = optim_wl$par[4], home_adv = optim_wl$par[5], attack_mix = optim_wl$par[6], defend_mix = optim_wl$par[7])
  params_xg <- list(intercept = optim_xg$params[1], gamma = optim_xg$par[2], lambda = optim_xg$par[3], rho = optim_xg$par[4], home_adv = optim_xg$par[5], attack_mix = optim_xg$par[6], defend_mix = optim_xg$par[7])
  iterativeParameters <- list('params_wl' = params_wl, 'params_xg' = params_xg)
  if(save_data & requireNamespace('usethis', quietly = TRUE)){
    usethis::use_data(iterativeParameters, overwrite = T)
  }
  invisible(iterativeParameters)
}
