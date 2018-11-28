odds_table$Result <- NA
season_sofar$HomeWin <- season_sofar$AwayWin <- season_sofar$Draw <- NA

all_results<-rbind(season_sofar, odds_table)

to_simulate<-which(!complete.cases(all_results$Result))

season_length<-nrow(all_results)

nsims<-1e4

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
#ms2$sim<-rep(1:nsims, each = season_length)

teams<-unique(ms2$HomeTeam)
teams<-teams[order(teams)]

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

all_results[(n*(i-1) + 1):(n*i),]$Points <- table$Points
all_results[(n*(i-1) + 1):(n*i),]$W <- table$W
all_results[(n*(i-1) + 1):(n*i),]$L <- table$L
all_results[(n*(i-1) + 1):(n*i),]$OTW <- table$OTW
all_results[(n*(i-1) + 1):(n*i),]$SOW <- table$SOW
all_results[(n*(i-1) + 1):(n*i),]$OTL <- table$OTL
all_results[(n*(i-1) + 1):(n*i),]$SOL <- table$SOL
all_results[(n*(i-1) + 1):(n*i),]$Rank <- table$Rank
all_results[(n*(i-1) + 1):(n*i),]$ConfRank <- table$ConfRank
all_results[(n*(i-1) + 1):(n*i),]$DivRank <- table$DivRank
all_results[(n*(i-1) + 1):(n*i),]$Playoffs <- table$Playoffs


