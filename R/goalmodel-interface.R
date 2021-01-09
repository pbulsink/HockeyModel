#Dixon-Coles using the goalmodel package
#set:
# xi = 0.00426
# include: days since last game (<=7)
# XXX include: games in last week (>=0)
# include: playing time offset (OT = 70/60, SO = 65/60, 2OT = 80/60, 3OT = 100/60, 4OT = 120/60, %OT = 140/60, 6OT = 160/60)
# rs = TrUE
# dc = TRUE
# hfa = TRUE


prepScoresForGM<-function(gmScores = HockeyModel::scores, xi = 0.00426){
  gmScores$weight <- goalmodel::weights_dc(gmScores$Date, xi = xi)
  gmScores<-gmScores[gmScores$weight > 1e-7,] # this trims to about 10 years of data with xi at 0.00426
  daysSince<-daysSinceGame(gmScores, 7)
  gmScores$HomeDaysSinceGame <- log(daysSince$HomeDays)
  gmScores$AwayDaysSinceGame <- log(daysSince$AwayDays)
  gmScores$PlayTimeOffset <- dplyr::case_when(gmScores$OTStatus == "OT" ~ 70/60, #give it half of a 20 min OT period
                                              gmScores$OTStatus == "SO" ~ 65/60,
                                              gmScores$OTStatus == "2OT" ~ 90/60,
                                              gmScores$OTStatus == "3OT" ~ 110/60,
                                              gmScores$OTStatus == "" ~ 60/60,
                                              TRUE ~ 130/60) #Capture & cap any longer OT periods as 130/60, they're so infrequent anyway
  gmScores$PlayTimeOffset <- log(gmScores$PlayTimeOffset)
  return(gmScores)
}

daysSinceGame <- function(sched, maxdays = 7){
  sched<-sched[,c('Date', 'HomeTeam', 'AwayTeam')]
  hdays<-c()
  adays<-c()
  pb<-progress::progress_bar$new(
    format = "  processing data [:bar] :percent eta: :eta",
    total = nrow(sched))
  for (i in 1:nrow(sched)){
    ht<-sched[i, 'HomeTeam']
    at<-sched[i, 'AwayTeam']
    dt<-sched[i,]$Date
    if(nrow(sched[sched$Date > (dt - maxdays) & sched$Date < dt, ]) > 0){
      #subset data frame to only dates within past maxdays and with home/away team matching t, then pull the game dates, subtract from dt, and take the min
      hds<-as.integer(min(dt-sched[(sched$Date > (dt - maxdays) & sched$Date < dt) & (sched$HomeTeam == ht | sched$AwayTeam == ht), ]$Date))
      ads<-as.integer(min(dt-sched[(sched$Date > (dt - maxdays) & sched$Date < dt) & (sched$HomeTeam == at | sched$AwayTeam == at), ]$Date))
    } else {
      hds<-maxdays
      ads<-maxdays
    }
    hdays<-c(hdays, hds)
    adays<-c(adays, ads)
    pb$tick()
  }
  adays[is.na(adays)]<-maxdays
  hdays[is.na(hdays)]<-maxdays
  return(list(HomeDays = hdays, AwayDays = adays))
}

trainGMModel<-function(gmScores=NULL){
  if(is.null(gmScores)){
    gmScores<-prepScoresForGM()
  }
  xx1<-matrix(0, nrow=nrow(gmScores), ncol=2)
  xx2<-matrix(0, nrow=nrow(gmScores), ncol=2)

  colnames(xx1) <- colnames(xx2) <- c('DaysSinceGame', "PlayTimeOffset")

  xx1[,1]<-gmScores$HomeDaysSinceGame
  xx2[,1]<-gmScores$AwayDaysSinceGame
  xx1[,2]<-xx2[,2]<-gmScores$PlayTimeOffset
  gm_mod<-goalmodel::goalmodel(goals1 = gmScores$HomeGoals,
                               goals2 = gmScores$AwayGoals,
                               team1 = gmScores$HomeTeam,
                               team2 = gmScores$AwayTeam,
                               weights = gmScores$weight,
                               x1 = xx1, x2 = xx2,
                               dc = TRUE, rs = TRUE,
                               hfa = TRUE)

  return(gm_mod)
}


