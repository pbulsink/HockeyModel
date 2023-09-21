tune_dc_weight <- function(xi = 0.01, beta = 365) {
  scores <- HockeyModel::scores
  truth <- scores[scores$Date > as.Date("2022-10-01"), ]
  schedule <- truth[, c("Date", "HomeTeam", "AwayTeam", "GameID", "GameType", "GameStatus")]
  schedule$GameStatus <- "Scheduled"
  schedule$HomeWin <- schedule$AwayWin <- NA

  schedule

  for(d in unique(schedule$Date)){
    current_m <- getM(scores = scores[scores$Date < d,], currentDate = d, xi = xi, beta = beta)
    current_rho <- getRho(m = current_m, scores = scores)
    params <- getWeibullParams(m = current_m, rho = current_rho, scores = scores[scores$Date < d,])
    beta <- params$beta
    eta <- params$eta
    k <- params$k
    params <- list("m" = current_m, "rho"=current_rho, "beta" = beta, "eta" = eta, "k" = k)

    for(g in schedule[schedule$Date == d,]){
      odds<-DCPredict(g$HomeTeam, g$AwayTeam, draws = F)
      schedule[schedule$GameID == g$GameID,]$HomeWin <- odds$HomeWinProbability
      schedule[schedule$GameID == g$GameID,]$AwayWin <- odds$AwayWinProbability
    }
  }

  acc <- accuracy(schedule$HomeWin > 0, actual = truth$Result>.5)
  ll <- logLoss(schedule$HomeWin, truth$Result > 0.5)
  message("Accuracy = ", round(acc, 4), ", LogLoss = ", round(ll, 4), ".")
  return(list(accuracy = acc, logloss = ll))
}
