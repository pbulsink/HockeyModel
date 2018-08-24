#metrics
#How models would have done last year.

#Each model is trained on ???? to 20162017. Then tested on 20172018.

#Reported Values:
# - LogLoss for game by game results
# - Recovery (max 31): for each team a predicted points (with sd) is generated. Sum the density at actual points for each team for a gaussian curve with sd=sd, centre= points predicted
# - accuracy - playoffs


sc16<-scores
sc16<-sc16[sc16$Date < "2017-08-01",]
sc17<-scores
sc17<-sc17[sc17$Date > "2017-08-01",]
sc17<-sc17[sc17$Date < "2018-08-01",]

adv16 <- prepareAdvancedData()
adv16 <- adv16[adv16$Date < "2017-08-01",]



elo.16<-NULL
dc.16<-NULL
dc5.16<-NULL
dc1.16<-NULL
bt.16<-NULL
ml.16<-NULL

dcLogLoss<-MLmetrics::logLoss()
dc5LogLoss<-MLmetrics::logLoss()
dc1LogLoss<-MLmetrics::logLoss()
btLogLoss<-MLmetrics::logLoss()
eloLogLoss<-MLmetrics::logLoss()
mlLogLoss<-MLmetrics::logLoss()

dcRecovery<-recovery()
dc5Recovery<-recovery()
dc1Recovery<-recovery()
btRecovery<-recovery()
eloRecovery<-recovery()
mlRecovery<-recovery()

dcAccuracy <- sum((dc.16$Playoffs>0.5) == sc17$Playoffs)/nrow(sc17)
dc5Accuracy <- sum((dc5.16$Playoffs>0.5) == sc17$Playoffs)/nrow(sc17)
dc1Accuracy <- sum((dc1.16$Playoffs>0.5) == sc17$Playoffs)/nrow(sc17)
eloAccuracy <- sum((elo.16$Playoffs>0.5) == sc17$Playoffs)/nrow(sc17)
btAccuracy <- sum((bt.16$Playoffs>0.5) == sc17$Playoffs)/nrow(sc17)
mlAccuracy <- sum((ml.16$Playoffs>0.5) == sc17$Playoffs)/nrow(sc17)



#' Recovery Calculating
#'
#' @param predicted data.frame of Team, Predicted Points, SD
#' @param actual data.frame of Team, Actual Points
#'
#' @return single value 0-31
#' @export
recovery<-function(predicted, actual){
  return(dnorm(actual$Points, mean = predicted$Points, sd = predicted$SD))
}
