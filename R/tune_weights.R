tune_dc_weight <- function(xi = 0.003, upsilon = 150) {
  message("Determining performance with xi = ", xi, " and upsilon = ", upsilon)
  scores <- HockeyModel::scores
  scores <- unique(scores[scores$Date > as.Date("2010-08-01"), ])
  truth <- scores[scores$Date > as.Date("2022-10-01"), ]
  schedule <- truth[, c("Date", "HomeTeam", "AwayTeam", "GameID", "GameType", "GameStatus")]
  schedule$GameStatus <- "Scheduled"
  #schedule$HomeWin <- schedule$AwayWin <- NA

  get_game_odds <- function(d, schedule, scores, xi, upsilon) {
    current_m <- getM(scores = scores[scores$Date < d,], currentDate = d, xi = xi, upsilon = upsilon)
    current_rho <- HockeyModel::rho #getRho(m = current_m, scores = scores[scores$Date< d, ])
    #params <- getWeibullParams(m = current_m, rho = current_rho, scores = scores[scores$Date < d,])
    beta <- HockeyModel::beta #params$beta
    eta <- HockeyModel::eta #params$eta
    k <- HockeyModel::k #params$k
    params <- list("m" = current_m, "rho"=current_rho, "beta" = beta, "eta" = eta, "k" = k)
    sch<-schedule[schedule$Date == d,]
    sch$HomeWin <- sch$AwayWin <- NA
    for(g in sch$GameID){
      odds<-DCPredict(sch[sch$GameID == g, ]$HomeTeam, sch[sch$GameID == g, ]$AwayTeam, params = params, draws = F)
      sch[sch$GameID == g,]$HomeWin <- odds[1]
      sch[sch$GameID == g,]$AwayWin <- odds[2]
    }
    gc()
    return(sch[,c("GameID", "HomeWin", "AwayWin")])
  }

  cl <- parallel::makeCluster(4)
  doSNOW::registerDoSNOW(cl)
  `%dopar%` <- foreach::`%dopar%`  # This hack passes R CMD CHK
  `%do%` <- foreach::`%do%`
  r <- foreach::foreach(i = 1:length(unique(schedule$Date)), .combine = 'rbind', .packages = c('HockeyModel')) %dopar% (
    get_game_odds(unique(schedule$Date)[i], schedule, scores, xi, upsilon)
  )
  parallel::stopCluster(cl)
  schedule<-dplyr::left_join(schedule, r, by = "GameID")
  acc <- accuracy(schedule$HomeWin > 0.5, actual = truth$Result>.5)
  ll <- logLoss(schedule$HomeWin, truth$Result > 0.5)
  message("Accuracy = ", round(acc, 4), ", LogLoss = ", round(ll, 4), ".")
  return(schedule)
}

#Determining performance with xi = 0.00426
#Accuracy = 0.6007, LogLoss = 0.6669.

#optim(par = c(1,1), fn = tune_dc_weight, control = list(fnscale = -1),
#      method = "L-BFGS-B", lower = c(0.01, 0.1), upper = c(99, 4))
