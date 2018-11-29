odds_table<-remainderSeasonDC(odds = TRUE)
odds_table$Result <- NA

season_sofar<-scores[scores$Date > as.Date("2018-08-01"),]
last_scores_date<-season_sofar[nrow(season_sofar), 'Date']
odds_table<-odds_table[odds_table$Date > last_scores_date, ]

season_sofar<-season_sofar[, c('Date', 'HomeTeam','AwayTeam','Result')]

season_sofar$HomeWin <- season_sofar$AwayWin <- season_sofar$Draw <- NA


all_season<-rbind(season_sofar, odds_table)

season_length<-nrow(all_season)

nsims<-1e3

all_season$r1<-all_results$r2<-all_results$r3<-NA

multi_season<-dplyr::bind_rows(replicate(nsims, all_season, simplify = FALSE))

multi_season$r1<-runif(n=season_length*nsims)
multi_season$r2<-runif(n=season_length*nsims)
multi_season$r3<-runif(n=season_length*nsims)

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% dplyr::mutate(...)
  .data
}

multi_season<-multi_season %>%
  mutate_cond(is.na(Result), Result = 1*(as.numeric(r1<HomeWin)) +
                     0.75 * (as.numeric(r1 > HomeWin & r1 < (HomeWin + Draw)) * (as.numeric(r2 > 0.5) * as.numeric(r3 < 0.75))) +
                     0.6 * (as.numeric(r1 > HomeWin & r1 < (HomeWin + Draw)) * (as.numeric(r2 > 0.5) * as.numeric (r3 > 0.75))) +
                     0.4 * (as.numeric(r1 > HomeWin & r1 < (HomeWin + Draw)) * (as.numeric(r2 < 0.5) * as.numeric (r3 > 0.75))) +
                     0.25 * (as.numeric(r1 > HomeWin & r1 < (HomeWin + Draw)) * (as.numeric(r2 < 0.5) * as.numeric (r3 < 0.75))) +
                     0)

multi_season$r1<-multi_season$r2<-multi_season$r3<-multi_season$HomeWin <- multi_season$AwayWin <- multi_season$Draw <- NULL
multi_season$sim<-rep(1:nsims, each = season_length)

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

applyteam<-function(multi_season, team, nsims){
  th<-multi_season[multi_season$HomeTeam == team,]
  ta<-multi_season[multi_season$AwayTeam == team,]
  w<-sapply(1:nsims, function(s) nrow(th[th$Result == 1 & th$sim == s,]) + nrow(ta[ta$Result == 0 & ta$sim == s,]))
  l<-sapply(1:nsims, function(s) nrow(th[th$Result == 0 & th$sim == s,]) + nrow(ta[ta$Result == 1 & ta$sim == s,]))
  otw<-sapply(1:nsims, function(s) nrow(th[th$Result == 0.75 & th$sim == s,]) + nrow(ta[ta$Result == 0.25 & ta$sim == s,]))
  otl<-sapply(1:nsims, function(s) nrow(th[th$Result == 0.25 & th$sim == s,]) + nrow(ta[ta$Result == 0.75 & ta$sim == s,]))
  sow<-sapply(1:nsims, function(s) nrow(th[th$Result == 0.6 & th$sim == s,]) + nrow(ta[ta$Result == 0.4 & ta$sim == s,]))
  sol<-sapply(1:nsims, function(s) nrow(th[th$Result == 0.4 & th$sim == s,]) + nrow(ta[ta$Result == 0.6 & ta$sim == s,]))
  rm(ta, th)
  return(list(w=w,l=l,otw=otw,otl=otl,sow=sow,sol=sol))
}

lapply_league<-function(multi_season, nsims, all_results){
  teamresults<-function(team, multi_season, nsims){
    return(applyteam(multi_season, team, nsims))
  }

  results<-lapply(X = teams, teamresults, nsims = nsims, multi_season=multi_season)
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

all_results<-lapply_league(multi_season = multi_season, nsims = nsims, all_results = all_results)

playoffs<-function(all_results){
  for(i in 1:nsims){
    sresult<-all_results[all_results$SimNo == i,]
    sresult$Rank <- rank(-sresult$Points, ties.method = 'random')
    #division spots
    for(division in nhl_divisions) {
      sresult[sresult$Team %in% division, "DivRank"] <- rank(sresult[sresult$Team %in% division, "Rank"])
      sresult[sresult$Team %in% division, "Playoffs"] <- as.numeric(sresult[sresult$Team %in% division, "DivRank"] < 4)
    }
    #wildcard
    for(conference in nhl_conferences) {
      conf<-unname(conference)
      sresult[sresult$Team %in% conf, "ConfRank"] <- rank(sresult[sresult$Team %in% conf, "Rank"])
      sresult[sresult$Team %in% conf & sresult$Playoffs == 0, "Playoffs"] <- as.numeric(sresult[sresult$Team %in% conf & sresult$Playoffs == 0, "ConfRank"]<3)
    }

    all_results[all_results$SimNo == i, 'Rank']<-sresult$Rank
    all_results[all_results$SimNo == i, "Playoffs"]<-sresult$Playoffs
    all_results[all_results$SimNo == i, 'ConfRank']<-sresult$ConfRank
    all_results[all_results$SimNo == i, "DivRank"]<-sresult$DivRank
  }
  return(all_results)
}

all_results<-playoffs(all_results)

microbenchmark::microbenchmark('seasontable' = buildStats(scores = HockeyModel::scores[HockeyModel::scores$Date > as.Date("2018-10-01")]))
