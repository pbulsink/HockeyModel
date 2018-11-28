odds_table<-remainderSeasonDC(odds = TRUE)
odds_table$Result <- NA

season_sofar<-scores[scores$Date > as.Date("2018-08-01"),]
last_scores_date<-season_sofar[nrow(season_sofar), 'Date']
odds_table<-odds_table[odds_table$Date > last_scores_date, ]

season_sofar<-season_sofar[, c('Date', 'HomeTeam','AwayTeam','Result')]

season_sofar$HomeWin <- season_sofar$AwayWin <- season_sofar$Draw <- NA


all_results<-rbind(season_sofar, odds_table)

season_length<-nrow(all_results)

nsims<-1e2

all_results$r1<-all_results$r2<-all_results$r3<-NA

multi_season<-dplyr::bind_rows(replicate(nsims, all_results, simplify = FALSE))

multi_season$r1<-runif(n=season_length*nsims)
multi_season$r2<-runif(n=season_length*nsims)
multi_season$r3<-runif(n=season_length*nsims)

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% dplyr::mutate(...)
  .data
}

ms2<-multi_season %>%
  mutate_cond(is.na(Result), Result = 1*(as.numeric(r1<HomeWin)) +
                     0.75 * (as.numeric(r1 > HomeWin & r1 < (HomeWin + Draw)) * (as.numeric(r2 > 0.5) * as.numeric(r3 < 0.75))) +
                     0.6 * (as.numeric(r1 > HomeWin & r1 < (HomeWin + Draw)) * (as.numeric(r2 > 0.5) * as.numeric (r3 > 0.75))) +
                     0.4 * (as.numeric(r1 > HomeWin & r1 < (HomeWin + Draw)) * (as.numeric(r2 < 0.5) * as.numeric (r3 > 0.75))) +
                     0.25 * (as.numeric(r1 > HomeWin & r1 < (HomeWin + Draw)) * (as.numeric(r2 < 0.5) * as.numeric (r3 < 0.75))) +
                     0)

ms2$r1<-ms2$r2<-ms2$r3<-ms2$HomeWin <- ms2$AwayWin <- ms2$Draw <- NULL
ms2$sim<-rep(1:nsims, each = season_length)

teams<-unique(ms2$HomeTeam)
teams<-as.character(teams[order(teams)])
n<-length(teams)

all_results <- data.frame(Team = rep(teams, nsims),
                          SimNo = rep(1:nsims, each = n),
                          Points = rep(NA, n * nsims),
                          W = rep(NA, n*nsims),
                          L = rep(NA, n * nsims),
                          OTW = rep(NA, n * nsims),
                          SOW = rep(NA, n * nsims),
                          OTL = rep(NA, n * nsims),
                          SOL = rep(NA, n * nsims),
                          Rank = rep(NA, n * nsims),
                          ConfRank = rep(NA, n * nsims),
                          DivRank = rep(NA, n * nsims),
                          Playoffs = rep(NA, n * nsims),
                          stringsAsFactors = FALSE)

lapply_league<-function(ms2, nsims, all_results){
  teamresults<-function(team, ms2, nsims){
    return(applyteam(ms2, team, nsims))
  }

  results<-lapply(X = teams, teamresults, nsims = nsims, ms2=ms2)
  names(results)<-teams
  for(t in teams){
    all_results[all_results$Team == t, 'W']<-results[[t]]$w
    all_results[all_results$Team == t, 'L']<-results[[t]]$l
    all_results[all_results$Team == t, 'OTW']<-results[[t]]$otw
    all_results[all_results$Team == t, 'OTL']<-results[[t]]$otl
    all_results[all_results$Team == t, 'SOW']<-results[[t]]$sow
    all_results[all_results$Team == t, 'SOL']<-results[[t]]$sol
  }
  all_results$Points<-all_results$W*2 + all_results$OTW*2 + all_results$SOW*2 + all_results$OTL + all_results$SOL
  return(all_results)
}

for_league<-function(ms2, nsims, all_results){
  for(t in teams){
    results<-applyteam(ms2, t, nsims)
    all_results[all_results$Team == t, 'W']<-results$w
    all_results[all_results$Team == t, 'L']<-results$l
    all_results[all_results$Team == t, 'OTW']<-results$otw
    all_results[all_results$Team == t, 'OTL']<-results$otl
    all_results[all_results$Team == t, 'SOW']<-results$sow
    all_results[all_results$Team == t, 'SOL']<-results$sol
  }

  all_results$Points<-all_results$W*2 + all_results$OTW*2 + all_results$SOW*2 + all_results$OTL + all_results$SOL
  return(all_results)
}

mb<-microbenchmark::microbenchmark('lapply' = lapply_league(ms2=ms2,nsims=nsims,all_results=all_results),
                               'for' = for_league(ms2=ms2,nsims = nsims, all_results = all_results))
ggplot2::autoplot(mb)
mb

applyteam<-function(ms2, team, nsims){
  th<-ms2[ms2$HomeTeam == team,]
  ta<-ms2[ms2$AwayTeam == team,]
  w<-sapply(1:nsims, function(s) nrow(th[th$Result == 1 & th$sim == s,]) + nrow(ta[ta$Result == 0 & ta$sim == s,]))
  l<-sapply(1:nsims, function(s) nrow(th[th$Result == 0 & th$sim == s,]) + nrow(ta[ta$Result == 1 & ta$sim == s,]))
  otw<-sapply(1:nsims, function(s) nrow(th[th$Result == 0.75 & th$sim == s,]) + nrow(ta[ta$Result == 0.25 & ta$sim == s,]))
  otl<-sapply(1:nsims, function(s) nrow(th[th$Result == 0.25 & th$sim == s,]) + nrow(ta[ta$Result == 0.75 & ta$sim == s,]))
  sow<-sapply(1:nsims, function(s) nrow(th[th$Result == 0.6 & th$sim == s,]) + nrow(ta[ta$Result == 0.4 & ta$sim == s,]))
  sol<-sapply(1:nsims, function(s) nrow(th[th$Result == 0.4 & th$sim == s,]) + nrow(ta[ta$Result == 0.6 & ta$sim == s,]))
  rm(ta, th)
  return(list(w=w,l=l,otw=otw,otl=otl,sow=sow,sol=sol))
}

all_results[(n*(i-1) + 1):(n*i),]$Rank <- table$Rank
all_results[(n*(i-1) + 1):(n*i),]$ConfRank <- table$ConfRank
all_results[(n*(i-1) + 1):(n*i),]$DivRank <- table$DivRank
all_results[(n*(i-1) + 1):(n*i),]$Playoffs <- table$Playoffs

microbenchmark::microbenchmark('seasontable' = buildStats(scores = HockeyModel::scores[HockeyModel::scores$Date > as.Date("2018-10-01")]))
