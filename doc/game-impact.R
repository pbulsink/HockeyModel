## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(HockeyModel)

## ----make_score_schedule-------------------------------------------------
scores<-HockeyModel::scores
scores<-scores[scores$Date > as.Date("2008-08-01"),]
scores<-scores[scores$Date < as.Date("2018-12-07"),]
max_score_date<-max(HockeyModel::scores$Date)
schedule<-rbind(HockeyModel::scores[HockeyModel::scores$Date > as.Date("2018-12-07"), c('Date', 'HomeTeam', 'AwayTeam')], 
                HockeyModel::schedule[HockeyModel::schedule$Date > max_score_date, ])

## ----get_rho_m, message = FALSE, error = FALSE, warning=FALSE------------
m<-getM(scores = scores, currentDate = as.Date("2018-12-07"))
rho<-getRho(scores = scores, m = m)

## ----season_predict, include=FALSE---------------------------------------
predictions<-loopless_sim(cores = 1, nsims = 1e3, schedule = schedule, rho=rho, m=m)
ari<-as.numeric(predictions$summary_results[predictions$summary_results$Team == 'Arizona Coyotes', 'Playoffs'])
sjs<-as.numeric(predictions$summary_results[predictions$summary_results$Team == 'San Jose Sharks', 'Playoffs'])

## ----season_predict_fake, eval=FALSE-------------------------------------
#  predictions<-remainderSeasonDC(schedule = schedule, scores = scores, rho=rho, m=m, regress = TRUE)

## ----Arizona_win---------------------------------------------------------
result<-data.frame(Date = as.Date("2018-11-23"), 
                   AwayTeam = "San Jose Sharks", 
                   AwayGoals = 2,
                   HomeTeam = "Arizona Coyotes", 
                   HomeGoals = 3,
                   OTStatus = "",
                   League = "NHL",
                   Tie = FALSE,
                   Winner = "Arizona Coyotes",
                   Loser = "San Jose Sharks",
                   Result = 1
                   )
#add the results to scores
newscores<-rbind(scores, result)
#remove the game from schedule
newschedule<-schedule[2:nrow(schedule),]

new_predict<-loopless_sim(cores = 1, nsims = 1e3, schedule = newschedule, scores = newscores, rho=rho, m=m)

## ----chances-------------------------------------------------------------
ari_previous_chance<-as.numeric(predictions$summary_results[predictions$summary_results$Team == "Arizona Coyotes", "Playoffs"])
ari_new_chance<-as.numeric(new_predict$summary_results[new_predict$summary_results$Team == "Arizona Coyotes", "Playoffs"])
print(paste0("Was: ", round(ari_previous_chance*100, 2), "%, now: ", round(ari_new_chance*100, 2), "%, change of ", round((ari_new_chance-ari_previous_chance)*100, 2), "%."))

## ----include = FALSE-----------------------------------------------------
sjs_previous_chance<-as.numeric(predictions$summary_results[predictions$summary_results$Team == "San Jose Sharks", "Playoffs"])
sjs_new_chance<-as.numeric(new_predict$summary_results[new_predict$summary_results$Team == "San Jose Sharks", "Playoffs"])
edm_previous_chance<-as.numeric(predictions$summary_results[predictions$summary_results$Team == "Edmonton Oilers", "Playoffs"])
edm_new_chance<-as.numeric(new_predict$summary_results[new_predict$summary_results$Team == "Edmonton Oilers", "Playoffs"])

## ----results, include = FALSE--------------------------------------------
home_ot<-result
home_ot$OTStatus <- "OT"
home_ot$Tie <- TRUE
home_ot$Result <- 0.75

home_so<-home_ot
home_so$OTStatus <- "SO"
home_so$Result <- 0.6

away_win<-result
away_win$HomeGoals <-2
away_win$AwayGoals <- 3
away_win$Winner <- 'San Jose Sharks'
away_win$Loser <- 'Arizona Coyotes'
away_win$Result <- 0

away_ot <- away_win
away_ot$OTStatus = "OT"
away_ot$Tie <- TRUE
away_ot$Result <- 0.25

away_so<-away_ot
away_so$OTStatus <-"SO"
away_so$Result <- 0.4

home_ot_score<-rbind(scores, home_ot)
home_so_score<-rbind(scores, home_so)
away_win_score<-rbind(scores, away_win)
away_ot_score<-rbind(scores, away_ot)
away_so_score<-rbind(scores, away_so)

predict_home_ot<-loopless_sim(cores = 1, nsims = 1e3, schedule = newschedule, scores = home_ot_score, rho=rho, m=m)
predict_home_so<-loopless_sim(cores = 1, nsims = 1e3, schedule = newschedule, scores = home_so_score, rho=rho, m=m)
predict_away_win<-loopless_sim(cores = 1, nsims = 1e3, schedule = newschedule, scores = away_win_score, rho=rho, m=m)
predict_away_ot<-loopless_sim(cores = 1, nsims = 1e3, schedule = newschedule, scores = away_ot_score, rho=rho, m=m)
predict_away_so<-loopless_sim(cores = 1, nsims = 1e3, schedule = newschedule, scores = away_so_score, rho=rho, m=m)

ari_home_ot_chance<-as.numeric(predict_home_ot$summary_results[predict_home_ot$summary_results$Team == "Arizona Coyotes", "Playoffs"])
ari_home_so_chance<-as.numeric(predict_home_so$summary_results[predict_home_so$summary_results$Team == "Arizona Coyotes", "Playoffs"])
ari_away_chance<-as.numeric(predict_away_win$summary_results[predict_away_win$summary_results$Team == "Arizona Coyotes", "Playoffs"])
ari_away_ot_chance<-as.numeric(predict_away_ot$summary_results[predict_away_ot$summary_results$Team == "Arizona Coyotes", "Playoffs"])
ari_away_so_chance<-as.numeric(predict_away_so$summary_results[predict_away_so$summary_results$Team == "Arizona Coyotes", "Playoffs"])

sjs_home_ot_chance<-as.numeric(predict_home_ot$summary_results[predict_home_ot$summary_results$Team == "San Jose Sharks", "Playoffs"])
sjs_home_so_chance<-as.numeric(predict_home_so$summary_results[predict_home_so$summary_results$Team == "San Jose Sharks", "Playoffs"])
sjs_away_chance<-as.numeric(predict_away_win$summary_results[predict_away_win$summary_results$Team == "San Jose Sharks", "Playoffs"])
sjs_away_ot_chance<-as.numeric(predict_away_ot$summary_results[predict_away_ot$summary_results$Team == "San Jose Sharks", "Playoffs"])
sjs_away_so_chance<-as.numeric(predict_away_so$summary_results[predict_away_so$summary_results$Team == "San Jose Sharks", "Playoffs"]) 

impact<-data.frame(Scenario = c("Home Win", "Home OT Win", "Home SO Win", "Away SO Win", "Away OT Win", "Away Win"), 
                   "Home Team (Arizona Coyotes)" = c(ari_new_chance - ari_previous_chance,
                                                   ari_home_ot_chance - ari_previous_chance,
                                                   ari_home_so_chance - ari_previous_chance,
                                                   ari_away_so_chance - ari_previous_chance,
                                                   ari_away_ot_chance - ari_previous_chance,
                                                   ari_away_chance - ari_previous_chance
                   ),
                   "Away Team (San Jose Sharks)" = c(sjs_new_chance - sjs_previous_chance,
                                                       sjs_home_ot_chance - sjs_previous_chance,
                                                       sjs_home_so_chance - sjs_previous_chance,
                                                       sjs_away_so_chance - sjs_previous_chance,
                                                       sjs_away_ot_chance - sjs_previous_chance,
                                                       sjs_away_chance - sjs_previous_chance)
                   )
impact$Home.Team..Arizona.Coyotes.<-round(impact$Home.Team..Arizona.Coyotes.*100, 2)
impact$Away.Team..San.Jose.Sharks.<-round(impact$Away.Team..San.Jose.Sharks.*100, 2)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(impact)

