tune_dc_weight <- function(xi = 0.01, upsilon = 365, scores=HockeyModel::scores) {
  scores <- scores[scores$Date > as.Date("2010-08-01"), ]
  truth <- scores[scores$Date > as.Date("2022-10-01"), ]
  schedule <- truth[, c("Date", "HomeTeam", "AwayTeam", "GameID", "GameType", "GameStatus")]
  schedule$GameStatus <- "Scheduled"
  schedule$HomeWin <- schedule$AwayWin <- NA

  for(d in unique(schedule$Date)){
    current_m <- getM(scores = scores[scores$Date < d,], currentDate = d, xi = xi, upsilon = upsilon)
    current_rho <- getRho(m = current_m, scores = scores[scores$Date< d, ])
    params <- getWeibullParams(m = current_m, rho = current_rho, scores = scores[scores$Date < d,])
    beta <- params$beta
    eta <- params$eta
    k <- params$k
    params <- list("m" = current_m, "rho"=current_rho, "beta" = beta, "eta" = eta, "k" = k)

    for(g in schedule[schedule$Date == d,]$GameID){
      odds<-DCPredict(schedule[schedule$GameID == g, ]$HomeTeam, schedule[schedule$GameID == g, ]$AwayTeam, params = params, draws = F)
      schedule[schedule$GameID == g,]$HomeWin <- odds[1]
      schedule[schedule$GameID == g,]$AwayWin <- odds[2]
    }
  }

  acc <- accuracy(schedule$HomeWin > 0, actual = truth$Result>.5)
  ll <- logLoss(schedule$HomeWin, truth$Result > 0.5)
  message("Accuracy = ", round(acc, 4), ", LogLoss = ", round(ll, 4), ".")
  return(acc)
}


#optim(c(xi = 0.01, beta = 365), fn = tune_dc_weight)
