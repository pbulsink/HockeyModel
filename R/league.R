#league stuff: tables, standings, stats, etc.

#' Bulid Stats Table
#'
#' @param scores scores to use to build stats table
#'
#' @return a stats table (as tibble)
#' @export
buildStats<-function(scores){
  scores<-droplevels(scores)
  teamlist <- sort(unique(c(as.character(scores$HomeTeam), as.character(scores$AwayTeam))))
  tmp1<-scores %>%
    dplyr::group_by(!!dplyr::sym('HomeTeam')) %>%
    dplyr::summarise(
      GP = length(!!dplyr::sym('Result')),
      W = sum(!!dplyr::sym('Result') == 1),
      OTW = sum(!!dplyr::sym('Result') == 0.75),
      SOW = sum(!!dplyr::sym('Result') == 0.6),
      OTL = sum(!!dplyr::sym('Result') == 0.25),
      SOL = sum(!!dplyr::sym('Result') == 0.4),
      L = sum(!!dplyr::sym('Result') == 0),
      P = as.numeric(!!dplyr::sym('W')*2 + !!dplyr::sym('OTW')*2 + !!dplyr::sym('SOW')*2 + !!dplyr::sym('OTL') + !!dplyr::sym('SOL'))
    ) %>%
    dplyr::ungroup()
  tmp2<-scores %>%
    dplyr::group_by(!!dplyr::sym('AwayTeam')) %>%
    dplyr::summarise(
      GP = length(!!dplyr::sym('Result')),
      W = sum(!!dplyr::sym('Result') == 0),
      OTW = sum(!!dplyr::sym('Result') == 0.25),
      SOW = sum(!!dplyr::sym('Result') == 0.4),
      OTL = sum(!!dplyr::sym('Result') == 0.75),
      SOL = sum(!!dplyr::sym('Result') == 0.6),
      L = sum(!!dplyr::sym('Result') == 1),
      P = as.numeric(!!dplyr::sym('W')*2 + !!dplyr::sym('OTW')*2 + !!dplyr::sym('SOW')*2 + !!dplyr::sym('OTL') + !!dplyr::sym('SOL'))
    ) %>%
    dplyr::ungroup()

  team_stats<-data.frame(
    Team=teamlist,
    GP = tmp1$GP + tmp2$GP,
    Points = tmp1$P + tmp2$P,
    W = tmp1$W + tmp2$W,
    L = tmp1$L + tmp2$L,
    OTL = tmp1$OTL + tmp2$OTL,
    OTW = tmp1$OTW + tmp2$OTW,
    SOL = tmp1$SOL + tmp2$SOL,
    SOW = tmp1$SOW + tmp2$SOW,
    stringsAsFactors = FALSE
  )

  nhl_conferences <- HockeyModel::nhl_conferences
  nhl_divisions <- HockeyModel::nhl_divisions

  team_stats$Rank <- rank(-team_stats$Points, ties.method = 'random')

  team_stats$ConfRank <- 0
  team_stats$ConfRank[team_stats$Team %in% nhl_conferences$East] <- rank(-team_stats$Points[team_stats$Team %in% nhl_conferences$East], ties.method = 'random')
  team_stats$ConfRank[team_stats$Team %in% nhl_conferences$West] <- rank(-team_stats$Points[team_stats$Team %in% nhl_conferences$West], ties.method = 'random')

  team_stats$DivRank <- 0
  team_stats$DivRank[team_stats$Team %in% nhl_divisions$Atlantic] <- rank(-team_stats$Points[team_stats$Team %in% nhl_divisions$Atlantic], ties.method = 'random')
  team_stats$DivRank[team_stats$Team %in% nhl_divisions$Central] <- rank(-team_stats$Points[team_stats$Team %in% nhl_divisions$Central], ties.method = 'random')
  team_stats$DivRank[team_stats$Team %in% nhl_divisions$Metropolitan] <- rank(-team_stats$Points[team_stats$Team %in% nhl_divisions$Metropolitan], ties.method = 'random')
  team_stats$DivRank[team_stats$Team %in% nhl_divisions$Pacific] <- rank(-team_stats$Points[team_stats$Team %in% nhl_divisions$Pacific], ties.method = 'random')

  team_stats$Playoffs <- 0
  #Top three from each division
  team_stats$Playoffs[team_stats$DivRank <= 3] <- 1

  #Two more teams from each conference, organized by conference rank, not currently in the playoffs
  team_stats[team_stats$Playoffs == 0 & team_stats$Team %in% nhl_conferences$East, 'Playoffs'][order(team_stats[team_stats$Playoffs == 0 & team_stats$Team %in% nhl_conferences$East, 'ConfRank'])][1:2] <- 1

  team_stats[team_stats$Playoffs == 0 & team_stats$Team %in% nhl_conferences$West, 'Playoffs'][order(team_stats[team_stats$Playoffs == 0 & team_stats$Team %in% nhl_conferences$West, 'ConfRank'])][1:2] <- 1

  return(tibble::as_tibble(team_stats))
}

#' Simulate the remainder of the season
#'
#' @param odds_table A dataframe with HomeTeam, AwayTeam, HomeWin, AwayWin, Draw, and Date
#' @param scores Past (historical) season scores. Defaults to HockeyModel::Scores
#' @param schedule Future unplayed games. Defaults to HockeyModel::schedule
#' @param nsims number of simulations to run
#' @param progress whether to show a progress bar.
#'
#' @return a data frame of results
#' @export
simulateSeason <- function(odds_table, scores=HockeyModel::scores, nsims=10000, schedule=HockeyModel::schedule, progress = FALSE){
  teamlist<-c()
  if(!is.null(scores)){
    season_sofar<-scores[scores$Date > as.Date(getCurrentSeasonStartDate()),]

    season_sofar <- season_sofar[,c('Date','HomeTeam','AwayTeam','Result')]

    teamlist<-c(teamlist, sort(unique(c(as.character(season_sofar$HomeTeam), as.character(season_sofar$AwayTeam)))))
  } else {
    season_sofar<-NULL
  }

  teamlist<-c(teamlist, sort(unique(c(as.character(schedule$Home), as.character(schedule$Away)))))

  n<-length(teamlist)

  all_results <- data.frame(Team = rep(teamlist, nsims),
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
  if(progress){
    pb<-dplyr::progress_estimated(nsims)
  }
  for(i in 1:nsims){
    #Generate Games results once
    tmp<-odds_table
    tmp$res1<-stats::runif(n = nrow(tmp))
    tmp$res2<-stats::runif(n = nrow(tmp))
    tmp$res3<-stats::runif(n = nrow(tmp))
    tmp$Result <- 1*(as.numeric(tmp$res1<tmp$HomeWin)) +
      0.75 * (as.numeric(tmp$res1 > tmp$HomeWin & tmp$res1 < (tmp$HomeWin + tmp$Draw)) * (as.numeric(tmp$res2 > 0.5) * as.numeric (tmp$res3 < 0.75))) +
      0.6 * (as.numeric(tmp$res1 > tmp$HomeWin & tmp$res1 < (tmp$HomeWin + tmp$Draw)) * (as.numeric(tmp$res2 > 0.5) * as.numeric (tmp$res3 > 0.75))) +
      0.4 * (as.numeric(tmp$res1 > tmp$HomeWin & tmp$res1 < (tmp$HomeWin + tmp$Draw)) * (as.numeric(tmp$res2 < 0.5) * as.numeric (tmp$res3 > 0.75))) +
      0.25 * (as.numeric(tmp$res1 > tmp$HomeWin & tmp$res1 < (tmp$HomeWin + tmp$Draw)) * (as.numeric(tmp$res2 < 0.5) * as.numeric (tmp$res3 < 0.75))) +
      0

    tmp$HomeWin <- NULL
    tmp$AwayWin <- NULL
    tmp$Draw <- NULL
    tmp$res1<-NULL
    tmp$res2<-NULL
    tmp$res3<-NULL

    tmp<-rbind(season_sofar, tmp)
    #Make the season table
    table<-buildStats(tmp)

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

    if(progress){
      pb$tick()$print()
    }
  }

  summary_results<-all_results %>%
    dplyr::group_by(!!dplyr::sym('Team')) %>%
    dplyr::summarise(
      Playoffs = mean(!!dplyr::sym('Playoffs')),
      meanPoints = mean(!!dplyr::sym('Points'), na.rm = TRUE),
      maxPoints = max(!!dplyr::sym('Points'), na.rm = TRUE),
      minPoints = min(!!dplyr::sym('Points'), na.rm = TRUE),
      meanWins = mean(!!dplyr::sym('W'), na.rm = TRUE),
      maxWins = max(!!dplyr::sym('W'), na.rm = TRUE),
      Presidents = sum(!!dplyr::sym('Rank') == 1)/dplyr::n(),
      meanRank = mean(!!dplyr::sym('Rank'), na.rm = TRUE),
      bestRank = min(!!dplyr::sym('Rank'), na.rm = TRUE),
      meanConfRank = mean(!!dplyr::sym('ConfRank'), na.rm = TRUE),
      bestConfRank = min(!!dplyr::sym('ConfRank'), na.rm = TRUE),
      meanDivRank = mean(!!dplyr::sym('DivRank'), na.rm = TRUE),
      bestDivRank = min(!!dplyr::sym('DivRank'), na.rm = TRUE),
      sdPoints = stats::sd(!!dplyr::sym('Points'), na.rm = TRUE),
      sdWins = stats::sd(!!dplyr::sym('W'), na.rm = TRUE),
      sdRank = stats::sd(!!dplyr::sym('Rank'), na.rm = TRUE),
      sdConfRank = stats::sd(!!dplyr::sym('ConfRank'), na.rm = TRUE),
      sdDivRank = stats::sd(!!dplyr::sym('DivRank'), na.rm = TRUE)
    )


  return(list(summary_results = summary_results, raw_results = all_results))
}


#' Simulate the remainder of the season
#'
#' @param odds_table A dataframe with HomeTeam, AwayTeam, HomeWin, AwayWin, Draw, and Date
#' @param scores Past (historical) season scores. Defaults to HockeyModel::Scores
#' @param schedule Future unplayed games. Defaults to HockeyModel::schedule
#' @param nsims number of simulations to run
#' @param cores number of cores to use in parallel.
#' @param progress whether to show a progress bar.
#'
#' @return a data frame of results
#' @export
simulateSeasonParallel <- function(odds_table, scores=HockeyModel::scores, nsims=10000, schedule=HockeyModel::schedule, cores = parallel::detectCores() - 1, progress = FALSE){
  teamlist<-c()
  if(!is.null(scores)){
    season_sofar<-scores[scores$Date > as.Date(getCurrentSeasonStartDate()),]
    season_sofar <- season_sofar[,c('Date','HomeTeam','AwayTeam','Result')]
  } else {
    season_sofar<-NULL
  }

  teamlist<-c(teamlist, sort(unique(c(as.character(schedule$Home), as.character(schedule$Away)))))

  if(!is.finite(cores)) {
    cores <- 2
  }

  `%dopar%` <- foreach::`%dopar%`  # This hack passes R CMD CHK
  cl<-parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)
  if(progress){
    pb<-utils::txtProgressBar(max = nsims, style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
  } else {
    opts <- list()
  }
  all_results <- foreach::foreach(i=1:nsims, .combine='rbind', .options.snow = opts) %dopar% {
    #Generate Games results once
    tmp<-odds_table
    tmp$res1<-stats::runif(n = nrow(tmp))
    tmp$res2<-stats::runif(n = nrow(tmp))
    tmp$res3<-stats::runif(n = nrow(tmp))
    tmp$Result <- 1*(as.numeric(tmp$res1<tmp$HomeWin)) +
      0.75 * (as.numeric(tmp$res1 > tmp$HomeWin & tmp$res1 < (tmp$HomeWin + tmp$Draw)) * (as.numeric(tmp$res2 > 0.5) * as.numeric (tmp$res3 < 0.75))) +
      0.6 * (as.numeric(tmp$res1 > tmp$HomeWin & tmp$res1 < (tmp$HomeWin + tmp$Draw)) * (as.numeric(tmp$res2 > 0.5) * as.numeric (tmp$res3 > 0.75))) +
      0.4 * (as.numeric(tmp$res1 > tmp$HomeWin & tmp$res1 < (tmp$HomeWin + tmp$Draw)) * (as.numeric(tmp$res2 < 0.5) * as.numeric (tmp$res3 > 0.75))) +
      0.25 * (as.numeric(tmp$res1 > tmp$HomeWin & tmp$res1 < (tmp$HomeWin + tmp$Draw)) * (as.numeric(tmp$res2 < 0.5) * as.numeric (tmp$res3 < 0.75))) +
      0

    tmp$HomeWin <- NULL
    tmp$AwayWin <- NULL
    tmp$Draw <- NULL
    tmp$res1<-NULL
    tmp$res2<-NULL
    tmp$res3<-NULL

    tmp<-rbind(season_sofar, tmp)
    #Make the season table
    table<-buildStats(tmp)

    table
  }
  if(progress){
    close(pb)
  }
  parallel::stopCluster(cl)
  gc(verbose = FALSE)

  summary_results<-all_results %>%
    dplyr::group_by(!!dplyr::sym('Team')) %>%
    dplyr::summarise(
      Playoffs = mean(!!dplyr::sym('Playoffs')),
      meanPoints = mean(!!dplyr::sym('Points'), na.rm = TRUE),
      maxPoints = max(!!dplyr::sym('Points'), na.rm = TRUE),
      minPoints = min(!!dplyr::sym('Points'), na.rm = TRUE),
      meanWins = mean(!!dplyr::sym('W'), na.rm = TRUE),
      maxWins = max(!!dplyr::sym('W'), na.rm = TRUE),
      Presidents = sum(!!dplyr::sym('Rank') == 1)/dplyr::n(),
      meanRank = mean(!!dplyr::sym('Rank'), na.rm = TRUE),
      bestRank = min(!!dplyr::sym('Rank'), na.rm = TRUE),
      meanConfRank = mean(!!dplyr::sym('ConfRank'), na.rm = TRUE),
      bestConfRank = min(!!dplyr::sym('ConfRank'), na.rm = TRUE),
      meanDivRank = mean(!!dplyr::sym('DivRank'), na.rm = TRUE),
      bestDivRank = min(!!dplyr::sym('DivRank'), na.rm = TRUE),
      sdPoints = stats::sd(!!dplyr::sym('Points'), na.rm = TRUE),
      sdWins = stats::sd(!!dplyr::sym('W'), na.rm = TRUE),
      sdRank = stats::sd(!!dplyr::sym('Rank'), na.rm = TRUE),
      sdConfRank = stats::sd(!!dplyr::sym('ConfRank'), na.rm = TRUE),
      sdDivRank = stats::sd(!!dplyr::sym('DivRank'), na.rm = TRUE)
    )


  return(list(summary_results = summary_results, raw_results = all_results))
}

#' Compile predictions to one object
#'
#' @description compiles predictions from a group of .RDS files to one data.frame
#' @param dir Directory holding the prediction .RDS files.
#'
#' @return a data frame.
#' @export
compile_predictions<-function(dir="./prediction_results"){
  #Find the files
  filelist<-list.files(path = dir)
  pdates<-substr(filelist, 1, 10)  # gets the dates list of prediction
  pdates<-pdates[pdates != 'graphics']
  all_predictions<-lapply(pdates, function(f) readRDS(file.path(dir, (paste0(f, "-predictions.RDS"))))) #Read all the files
  names(all_predictions)<-pdates
  all_predictions<-dplyr::bind_rows(all_predictions, .id="predictionDate")
  return(all_predictions)
}


#' 'Loopless' simulation
#'
#' @param nsims number of simulations to run (approximate)
#' @param cores number of cores in parallel to process
#' @param schedule games to play
#' @param scores Season to this point
#' @param rho rho factor to pass. default HockeyModel::rho
#' @param m m model to pass. default HockeyModel::m
#' @param odds_table odds from remainderSeasonDC(Odds = TRUE), or null
#'
#' @return a two member list, of all results and summary results
#' @export
loopless_sim<-function(nsims=1e5, cores = parallel::detectCores() - 1, schedule = HockeyModel::schedule, scores=HockeyModel::scores, rho = HockeyModel::rho, m = HockeyModel::m, odds_table = NULL){

  nsims <- floor(nsims/cores)

  if(is.null(odds_table)){
    odds_table<-remainderSeasonDC(scores = scores, schedule = schedule, odds = TRUE, rho = rho, m = m)
  }
  odds_table$Result <- NA

  season_sofar<-scores[scores$Date > as.Date(getCurrentSeasonStartDate()),]

  if(nrow(season_sofar) > 0){

    last_scores_date<-season_sofar[nrow(season_sofar), 'Date']
    odds_table<-odds_table[odds_table$Date > last_scores_date, ]

    season_sofar<-season_sofar[, c('Date', 'HomeTeam','AwayTeam','Result')]
    season_sofar$HomeWin <- season_sofar$AwayWin <- season_sofar$Draw <- NA

    all_season<-rbind(season_sofar, odds_table)
  } else {
    all_season <- odds_table
  }

  `%dopar%` <- foreach::`%dopar%`
  #`%do%` <- foreach::`%do%`
  cl<-parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)

  #Ram management issues. Send smaller chunks more often, hopefully this helps.

  all_results <- foreach::foreach(i=1:(cores*5), .combine='rbind', .packages = "HockeyModel") %dopar% {
    all_results<-sim_engine(all_season = all_season, nsims = floor(nsims/5))
    return(all_results)
  }

  parallel::stopCluster(cl)
  gc(verbose = FALSE)

  summary_results<-all_results %>%
    dplyr::group_by(!!dplyr::sym('Team')) %>%
    dplyr::summarise(
      Playoffs = mean(!!dplyr::sym('Playoffs')),
      meanPoints = mean(!!dplyr::sym('Points'), na.rm = TRUE),
      maxPoints = max(!!dplyr::sym('Points'), na.rm = TRUE),
      minPoints = min(!!dplyr::sym('Points'), na.rm = TRUE),
      meanWins = mean(!!dplyr::sym('W'), na.rm = TRUE),
      maxWins = max(!!dplyr::sym('W'), na.rm = TRUE),
      Presidents = sum(!!dplyr::sym('Rank') == 1)/dplyr::n(),
      meanRank = mean(!!dplyr::sym('Rank'), na.rm = TRUE),
      bestRank = min(!!dplyr::sym('Rank'), na.rm = TRUE),
      meanConfRank = mean(!!dplyr::sym('ConfRank'), na.rm = TRUE),
      bestConfRank = min(!!dplyr::sym('ConfRank'), na.rm = TRUE),
      meanDivRank = mean(!!dplyr::sym('DivRank'), na.rm = TRUE),
      bestDivRank = min(!!dplyr::sym('DivRank'), na.rm = TRUE),
      sdPoints = stats::sd(!!dplyr::sym('Points'), na.rm = TRUE),
      sdWins = stats::sd(!!dplyr::sym('W'), na.rm = TRUE),
      sdRank = stats::sd(!!dplyr::sym('Rank'), na.rm = TRUE),
      sdConfRank = stats::sd(!!dplyr::sym('ConfRank'), na.rm = TRUE),
      sdDivRank = stats::sd(!!dplyr::sym('DivRank'), na.rm = TRUE),
      p_rank1 = sum(!!dplyr::sym('ConfRank') == 1 & !!dplyr::sym('DivRank') == 1)/dplyr::n(),
      p_rank2 = sum(!!dplyr::sym('ConfRank') != 1 & !!dplyr::sym('DivRank') == 1)/dplyr::n(),
      # Solving 3 & 4 & 5 & 6 doesn't *really* matter, because 3/4 play the 5/6 within their own division.
      # In 2nd round, 1/8 or 2/7 play the 3/6 or 4/5 from their own division. No re-seeding occurs.
      # See: https://en.wikipedia.org/wiki/Stanley_Cup_playoffs#Current_format
      p_rank_34 = sum(!!dplyr::sym('DivRank') == 2)/dplyr::n(),
      p_rank_56 = sum(!!dplyr::sym('DivRank') == 3)/dplyr::n(),
      p_rank7 = sum(!!dplyr::sym('Wildcard') == 1)/dplyr::n(),
      p_rank8 = sum(!!dplyr::sym('Wildcard') == 2)/dplyr::n()
    )

  return(list(summary_results = summary_results, raw_results = all_results))
}

#' Simulation engine to be parallelized or used in single core
#'
#' @param all_season One seasons' scores & odds schedule
#' @param nsims Number of simulations to run
#'
#' @return results of `nsims` season simulations, as one long data frame score table.
#' @export
sim_engine<-function(all_season, nsims){

  season_length<-nrow(all_season)
  if(season_length != 1271){
    warning('Season length not as expected: ', season_length)
  }

  multi_season<-dplyr::bind_rows(replicate(nsims, all_season, simplify = FALSE))
  multi_season$sim<-rep(1:nsims, each = season_length)
  multi_season$r1<-stats::runif(n=nrow(multi_season))
  multi_season$r2<-stats::runif(n=nrow(multi_season))
  multi_season$r3<-stats::runif(n=nrow(multi_season))

  Result <- dplyr::sym('Result')  # is.na(!!dplyr::sym('Result')) got really mad. offload to before call calmed it.

  multi_season<-multi_season %>%
    mutate_cond(is.na(!!Result), Result = 1*(as.numeric(!!dplyr::sym('r1')<!!dplyr::sym('HomeWin'))) +
                  0.75 * (as.numeric(!!dplyr::sym('r1') > !!dplyr::sym('HomeWin') &
                                       !!dplyr::sym('r1') < (!!dplyr::sym('HomeWin') + !!dplyr::sym('Draw'))) *
                            (as.numeric(!!dplyr::sym('r2') > 0.5) * as.numeric(!!dplyr::sym('r3') < 0.75))) +
                  0.6 * (as.numeric(!!dplyr::sym('r1') > !!dplyr::sym('HomeWin') &
                                      !!dplyr::sym('r1') < (!!dplyr::sym('HomeWin') + !!dplyr::sym('Draw'))) *
                           (as.numeric(!!dplyr::sym('r2') > 0.5) * as.numeric (!!dplyr::sym('r3') > 0.75))) +
                  0.4 * (as.numeric(!!dplyr::sym('r1') > !!dplyr::sym('HomeWin') &
                                      !!dplyr::sym('r1') < (!!dplyr::sym('HomeWin') + !!dplyr::sym('Draw'))) *
                           (as.numeric(!!dplyr::sym('r2') < 0.5) * as.numeric (!!dplyr::sym('r3') > 0.75))) +
                  0.25 * (as.numeric(!!dplyr::sym('r1') > !!dplyr::sym('HomeWin') &
                                       !!dplyr::sym('r1') < (!!dplyr::sym('HomeWin') + !!dplyr::sym('Draw'))) *
                            (as.numeric(!!dplyr::sym('r2') < 0.5) * as.numeric (!!dplyr::sym('r3') < 0.75))) +
                  0)

  multi_season$r1<-multi_season$r2<-multi_season$r3<-multi_season$HomeWin <- multi_season$AwayWin <- multi_season$Draw <- NULL

  long_season<-data.frame(Team = c(as.character(multi_season$HomeTeam), as.character(multi_season$AwayTeam)), stringsAsFactors = FALSE)
  long_season$Result<-c(multi_season$Result, 1-multi_season$Result)
  long_season$SimNo<-c(multi_season$sim, multi_season$sim)

  rm(multi_season)

  all_results<-long_season %>%
    dplyr::group_by(!!dplyr::sym('SimNo'), !!dplyr::sym('Team')) %>%
    dplyr::summarise(W = sum(!!dplyr::sym('Result') == 1),
              OTW = sum(!!dplyr::sym('Result') == 0.75),
              SOW = sum(!!dplyr::sym('Result') == 0.6),
              SOL = sum(!!dplyr::sym('Result') == 0.4),
              OTL = sum(!!dplyr::sym('Result') == 0.25),
              L = sum(!!dplyr::sym('Result') == 0)) %>%
    as.data.frame()

  rm(long_season)

  all_results$Points<-all_results$W*2 + all_results$OTW*2 + all_results$SOW*2 + all_results$OTL + all_results$SOL

  all_results$Conference <- getConference(all_results$Team)
  all_results$Division <- getDivision(all_results$Team)
  all_results$Wildcard <- NA

  all_results <- all_results %>%
    dplyr::group_by(!!dplyr::sym('SimNo')) %>%
    dplyr::mutate(Rank = rank(-!!dplyr::sym('Points'), ties.method = 'random')) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!dplyr::sym('SimNo'), !!dplyr::sym('Conference')) %>%
    dplyr::mutate(ConfRank = rank(!!dplyr::sym('Rank'))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!dplyr::sym('SimNo'), !!dplyr::sym('Division')) %>%
    dplyr::mutate(DivRank = rank(!!dplyr::sym('ConfRank'))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Playoffs = ifelse(!!dplyr::sym('DivRank') <=3, 1, 0)) %>%
    dplyr::group_by(!!dplyr::sym('SimNo'), !!dplyr::sym('Conference')) %>%
    dplyr::arrange(!!dplyr::sym('Playoffs'), !!dplyr::sym('ConfRank')) %>%
    dplyr::mutate(Wildcard = ifelse(!!dplyr::sym('Playoffs') == 0, dplyr::row_number(), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(!!dplyr::sym('SimNo'), !!dplyr::sym('Team')) %>%
    #mutate_cond(!!dplyr::sym('Wildcard') <= 2, Playoffs = 1) %>% #TODO might not work at scale???
    dplyr::select(!!dplyr::sym('SimNo'), !!dplyr::sym('Team'), !!dplyr::sym('W'), !!dplyr::sym('OTW'),
                  !!dplyr::sym('SOW'), !!dplyr::sym('SOL'), !!dplyr::sym('OTL'), !!dplyr::sym('Points'),
                  !!dplyr::sym('Wildcard'), !!dplyr::sym('Rank'), !!dplyr::sym('ConfRank'),
                  !!dplyr::sym('DivRank'), !!dplyr::sym('Playoffs'))

  #Do this by hand as it doesn't seem to want to work in dplyr pipe
  all_results[!is.na(all_results$Wildcard) & all_results$Wildcard <= 2,]$Playoffs<-1
  all_results$Wildcard[is.na(all_results$Wildcard)]<-0

  #all_results$Wildcard<-NULL

  return(all_results)
}

#' Playoff Win Calculator
#'
#' @description wraps PlayoffSeriesOdds with odds generator for a given home and away team
#' @param home_team Home Ice Advantage Team
#' @param away_team Opponent Team
#' @param home_wins Home Ice Advantage Team Wins in Series
#' @param away_wins Opponent Team Wins in Series
#'
#' @return Odds from 0-1
#' @export
playoffWin<-function(home_team, away_team, home_wins = 0, away_wins = 0){
  home_odds<-DCPredict(home = home_team, away = away_team)
  away_odds<-1-DCPredict(home = away_team, away = home_team)
  return(playoffSeriesOdds(home_odds = home_odds, away_odds = away_odds, home_win = home_wins, away_win = away_wins))
}

#' Statistical Playoff Series Odds Solver
#'
#' @description Given home and away win odds, produce the odds of the 'home advantage' team winning the series. From \url{http://www.stat.umn.edu/geyer/playoff.html}, modified to function with odds determination.
#' @references \url{http://www.stat.umn.edu/geyer/playoff.html}
#'
#' @param home_odds Team odds with home-ice advantage at home
#' @param away_odds Team odds with home-ice advantage at away (on the road)
#' @param home_win Number of home ice advantage team wins thus far in the series. Default to 0 (prediction before series start)
#' @param away_win Number of away team wins thus far in the series
#'
#' @return numeric odds of home team win series (1-odds for away odds)
#' @export
playoffSeriesOdds<-function(home_odds, away_odds, home_win=0, away_win=0){
  ngames <- 7
  game_home <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  game_to <- 4

  p1_home<-home_odds
  p1_road<-away_odds

  if (p1_home < 0 || p1_home > 1 || p1_road < 0 || p1_road > 1){
    stop("impossible odds")
  }

  home_win <- as.integer(home_win)
  away_win <- as.integer(away_win)

  if (home_win < 0 | away_win < 0){
    stop("negative number of wins impossible")
  }
  if (home_win >= game_to) {
    message("series already won")
    return(1)
  }
  if (away_win >= game_to) {
    message("series already won")
    return(0)
  }

  games_played <- home_win + away_win

  if (games_played > ngames){
    stop("total wins greater than games impossible")
  }
  if (games_played == ngames){
    stop("nothing to do, series over")
  }

  x.g <- games_played
  x.w1 <- home_win
  x.w2 <- away_win
  x.p <- 1.0
  finished_series <- NULL
  for (i in (games_played + 1):ngames) {
    p1now <- ifelse(game_home[i], p1_home, p1_road)
    l <- length(x.g)
    y.w1 <- c(x.w1 + 1, x.w1[l])
    y.w2 <- c(x.w2[1], x.w2 + 1)
    y.g <- c(x.g + 1, x.g[1] + 1)
    y.p <- c(x.p * p1now, 0)
    y.p <- y.p + c(0, x.p * (1 - p1now))
    unfinished_series <- y.w1 < game_to & y.w2 < game_to
    if (any(! unfinished_series)) {
      series <- cbind(y.g, y.w1, y.w2, y.p)
      finished_series <- rbind(finished_series, series[! unfinished_series, ])
    }
    x.g <- y.g[unfinished_series]
    x.w1 <- y.w1[unfinished_series]
    x.w2 <- y.w2[unfinished_series]
    x.p <- y.p[unfinished_series]
  }

  p1total <- sum(finished_series[finished_series[,2] == game_to, 4])

  return(p1total)
}

#' Playoff solver: chances of making each round & winning cup
#'
#' @param all_results if available, the results from simulations. Else loaded.
#' @param pretty_format Whether to return a pretty table or raw data.
#'
#' @return a tibble of playoff odds
#' @export
playoffSolver<-function(all_results = NULL, pretty_format = TRUE){
  if(is.null(all_results)){
    filelist<-list.files(path = "./prediction_results")
    pdates<-substr(filelist, 1, 10)  # gets the dates list of prediction
    pdates<-pdates[pdates != 'graphics']
    lastp<-as.Date(max(pdates))
    all_results<-readRDS(file.path("./prediction_results", paste0(lastp,"-predictions.RDS")))
    summary_results<-all_results
  }

  if('summary_results' %in% names(all_results)){
    summary_results <- all_results$summary_results
  } else {
    summary_results<-all_results
  }

  #Convention: p# is odds of making it out of the round #. p0 is making playoffs, p1 is making out of 1st round, etc. so p4 is win cup.

  #make 1st round win odds matrix. p34 and p56 are combined odds for two positions, so /2 for individual position odds
  p0<-summary_results %>%
    dplyr::mutate(p_rank3 = !!dplyr::sym('p_rank_34')/2,
                  p_rank4 = !!dplyr::sym('p_rank_34')/2,
                  p_rank5 = !!dplyr::sym('p_rank_56')/2,
                  p_rank6 = !!dplyr::sym('p_rank_56')/2) %>%
    dplyr::select(!!dplyr::sym('Team'), !!dplyr::sym('p_rank1'), !!dplyr::sym('p_rank2'), !!dplyr::sym('p_rank3'),
                  !!dplyr::sym('p_rank4'), !!dplyr::sym('p_rank5'), !!dplyr::sym('p_rank6'), !!dplyr::sym('p_rank7'),
                  !!dplyr::sym('p_rank8'), !!dplyr::sym('meanRank'))

  p0<-p0[sum(p0$p_rank1, p0$p_rank2, p0$p_rank3, p0$p_rank5, p0$p_rank7, p0$p_rank8)>0,]
  p0$Division<-getDivision(p0$Team)
  p0$Conference<-getConference(p0$Team)
  p1<-p0
  p1$p_rank1 <- p1$p_rank2 <- p1$p_rank3 <- p1$p_rank4 <- p1$p_rank5 <- p1$p_rank6 <- p1$p_rank7 <- p1$p_rank8 <- 0
  p2<-p3<-p4<-p1


  ranks1<-paste('p_rank', c(1:8), sep = '')

  for (team in p1$Team){
    #pass to a function solving a teams' odds of progressing (sorting opponent, etc.)
    p1[p1$Team == team, ranks1]<-team_progression_odds(round = 1, team = team, odds = p0)
  }

  p1$p_rank18<-p1$p_rank1 + p1$p_rank8
  p1$p_rank27<-p1$p_rank2 + p1$p_rank7
  p1$p_rank36<-p1$p_rank3 + p1$p_rank6
  p1$p_rank45<-p1$p_rank4 + p1$p_rank5
  p1[,ranks1]<-p2[,ranks1]<-p3[,ranks1]<-p4[,ranks1]<-NULL

  #Fix normalization fixes.
  ranks2<-c('p_rank18', 'p_rank27', 'p_rank36', 'p_rank45')
  p1[, ranks2]<-p1[, ranks2]*(8/sum(p1[,ranks2]))

  #make 2nd round win odds matrix
  p2$cfodds<-0
  for (team in p2$Team){
    p2[p2$Team == team, ]$cfodds<-team_progression_odds(round = 2, team = team, odds = p1)
  }
  p2$cfodds<-p2$cfodds*(4/sum(p2$cfodds))

  #make 3rd round win odds matrix+
  p3$fodds<-0
  for(team in p3$Team){
    p3[p3$Team == team, ]$fodds<-team_progression_odds(round = 3, team = team, odds = p2)
  }

  p3$fodds<-p3$fodds*(2/sum(p3$fodds))

  #make cup win odds matrix
  p4$cupodds<-0
  for(team in p4$Team){
    p4[p4$Team == team, ]$cupodds<-team_progression_odds(round = 4, team = team, odds = p3)
  }
  p4$cupodds<-p4$cupodds * (1/sum(p4$cupodds))

  #Summarize Data
  playoff_odds<-tibble::tibble(Team = summary_results$Team,
                       Make_Playoffs = 0,
                       Win_First_Round = 0,
                       Win_Second_Round = 0,
                       Win_Conference = 0,
                       Win_Cup = 0
                       )
  playoff_odds$Make_Playoffs = p0$p_rank1 + p0$p_rank2 + p0$p_rank3 + p0$p_rank4 + p0$p_rank5 + p0$p_rank6 + p0$p_rank7 + p0$p_rank8
  playoff_odds$Win_First_Round = p1$p_rank18 + p1$p_rank27 + p1$p_rank36 + p1$p_rank45
  playoff_odds$Win_Second_Round = p2$cfodds
  playoff_odds$Win_Conference = p3$fodds
  playoff_odds$Win_Cup = p4$cupodds

  if(pretty_format){

    format_playoff_odds<-function(playoff_odds, caption_text){
      playoff_odds$Make_Playoffs = round(playoff_odds$Make_Playoffs*100, 2)
      playoff_odds$Win_First_Round = round(playoff_odds$Win_First_Round*100, 2)
      playoff_odds$Win_Second_Round = round(playoff_odds$Win_Second_Round*100, 2)
      playoff_odds$Win_Conference = round(playoff_odds$Win_Conference*100, 2)
      playoff_odds$Win_Cup = round(playoff_odds$Win_Cup*100, 2)

      playoff_odds<-formattable::formattable(playoff_odds,
                                             col.names = c("Team", "Make Playoffs", "Win 1st Round", "Win 2nd Round", "Win Conference", "Win Cup"),
                                             caption = paste0(caption_text, " Playoff Odds as of ", Sys.Date(), " | P. Bulsink (@BulsinkB)"),
                                             align = c('l', rep("r", 5)),
                                             list(
                                               `Team` = formattable::formatter("span", style = ~formattable::style(font.weight = "bold")),
                                               formattable::area(col = 2:6) ~ function(x) formattable::percent(x/100, digits = 2),
                                               formattable::area(col = 2:6) ~ formattable::color_tile("#fefffe", "#3ccc3c")
                                               ))
    }

    playoff_odds<-dplyr::arrange(playoff_odds, desc(Win_Cup))
    east_odds<-format_playoff_odds(playoff_odds[playoff_odds$Team %in% HockeyModel::nhl_conferences$East,], caption_text = "Eastern Conference")
    west_odds<-format_playoff_odds(playoff_odds[playoff_odds$Team %in% HockeyModel::nhl_conferences$West,], caption_text = "Western Conference")

    playoff_odds<-list('east' = east_odds, 'west' = west_odds)
  }
  return(playoff_odds)
}

#' Team Progression Odds
#'
#' @param round Playoff round (1-4)
#' @param team Team to calculate odds for
#' @param odds input odds of each opponent team making each position of the round
#'
#' @return a vector of odds of progressing in each position
#' @export
team_progression_odds<-function(round, team, odds){
  #Very many errors, in all likelihood. Also slow.

  c_opponents<-odds[odds$Conference == getConference(team),]$Team
  c_opponents<-c_opponents[c_opponents != team]
  d_opponents<-odds[odds$Division == getDivision(team),]$Team
  d_opponents<-d_opponents[d_opponents != team]
  #home_opponents are cup opponents against whom 'team' would be the home team, away_opponents are against better teams, who 'team' would be the away team.
  cf_home_opponents<-odds[(odds$Conference == getConference(team) & odds$meanRank > odds[odds$Team == team,]$meanRank),]$Team
  cf_away_opponents<-odds[(odds$Conference == getConference(team) & odds$meanRank < odds[odds$Team == team,]$meanRank),]$Team
  f_home_opponents<-odds[(odds$Conference != getConference(team) & odds$meanRank > odds[odds$Team == team,]$meanRank),]$Team
  f_away_opponents<-odds[(odds$Conference != getConference(team) & odds$meanRank < odds[odds$Team == team,]$meanRank),]$Team

  if(round == 1){
    ranks<-paste('p_rank', c(1:8), sep = '')

    team_odds<-as.numeric(odds[odds$Team == team, names(odds) %in% ranks])
    o1<-sum(sapply(c_opponents, function(x) (odds[odds$Team == x, ]$p_rank8/sum(odds[odds$Team %in% c_opponents,]$p_rank8)) *
                     playoffDC(home = team, away = x)),
            na.rm = TRUE)*team_odds[1]
    o2<-sum(sapply(c_opponents, function(x) (odds[odds$Team == x, ]$p_rank7/sum(odds[odds$Team %in% c_opponents,]$p_rank7)) *
                     playoffDC(home = team, away = x)),
            na.rm = TRUE)*team_odds[2]
    o3<-sum(sapply(d_opponents, function(x) (odds[odds$Team == x, ]$p_rank6/sum(odds[odds$Team %in% d_opponents,]$p_rank6)) *
                     playoffDC(home = team, away = x)),
            na.rm = TRUE)*team_odds[3]
    o4<-o3
    o6<-sum(sapply(d_opponents, function(x) (odds[odds$Team == x, ]$p_rank3/sum(odds[odds$Team %in% d_opponents,]$p_rank3)) *
                     (1-playoffDC(home = x, away = team))),
            na.rm = TRUE)*team_odds[6]
    o5<-o6
    o7<-sum(sapply(c_opponents, function(x) (odds[odds$Team == x, ]$p_rank2/sum(odds[odds$Team %in% c_opponents,]$p_rank2)) *
                     (1-playoffDC(home = x, away = team))),
            na.rm = TRUE)*team_odds[7]
    o8<-sum(sapply(c_opponents, function(x) (odds[odds$Team == x, ]$p_rank1/sum(odds[odds$Team %in% c_opponents,]$p_rank1)) *
                     (1-playoffDC(home = x, away = team))),
            na.rm = TRUE)*team_odds[8]

    return(c(o1,o2,o3,o4,o5,o6,o7,o8))

  } else if (round == 2){

    team_odds<-as.numeric(odds[odds$Team == team, c('p_rank18', 'p_rank27', 'p_rank36', 'p_rank45')])
    #1/8 winner plays 3/4 or 5/6 winner from division. noramlization are *2 because we're summing together 2 avenues using the same team_odds.
    o1<-sum(sapply(d_opponents, function(x) (odds[odds$Team == x, ]$p_rank36/(2*sum(odds[odds$Team %in% d_opponents,]$p_rank36))) *
                     playoffDC(home = team, away = x)),
            na.rm = TRUE)*team_odds[1] +
      sum(sapply(d_opponents, function(x) (odds[odds$Team == x, ]$p_rank45/(2*sum(odds[odds$Team %in% d_opponents,]$p_rank45))) *
                   playoffDC(home = team, away = x)),
          na.rm = TRUE)*team_odds[1]

    o2<-sum(sapply(d_opponents, function(x) (odds[odds$Team == x, ]$p_rank36/(2*sum(odds[odds$Team %in% d_opponents,]$p_rank36))) *
                     playoffDC(home = team, away = x)),
            na.rm = TRUE)*team_odds[2] +
      sum(sapply(d_opponents, function(x) (odds[odds$Team == x, ]$p_rank45/(2*sum(odds[odds$Team %in% d_opponents,]$p_rank45))) *
                   playoffDC(home = team, away = x)),
          na.rm = TRUE)*team_odds[2]

    o3<-sum(sapply(d_opponents, function(x) (odds[odds$Team == x, ]$p_rank27/(2*sum(odds[odds$Team %in% d_opponents,]$p_rank27))) *
                     playoffDC(home = team, away = x)),
            na.rm = TRUE)*team_odds[3] +
      sum(sapply(d_opponents, function(x) (odds[odds$Team == x, ]$p_rank27/(2*sum(odds[odds$Team %in% d_opponents,]$p_rank27))) *
                   playoffDC(home = team, away = x)),
          na.rm = TRUE)*team_odds[3]

    o4<-sum(sapply(d_opponents, function(x) (odds[odds$Team == x, ]$p_rank18/(2*sum(odds[odds$Team %in% d_opponents,]$p_rank18))) *
                     playoffDC(home = team, away = x)),
            na.rm = TRUE)*team_odds[4] +
      sum(sapply(d_opponents, function(x) (odds[odds$Team == x, ]$p_rank18/(2*sum(odds[odds$Team %in% d_opponents,]$p_rank18))) *
                   playoffDC(home = team, away = x)),
          na.rm = TRUE)*team_odds[4]

    return(o1+o2+o3+o4)

  } else if (round == 3){
    if(length(cf_home_opponents)>0){
      o1<-sum(sapply(cf_home_opponents, function(x) (odds[odds$Team == x, ]$cfodds/sum(odds[odds$Team %in% c_opponents,]$cfodds)) * playoffDC(home = team, away = x)),
              na.rm = TRUE) * odds[odds$Team == team, ]$cfodds
    } else {
      o1<-0
    }
    if(length(cf_away_opponents)>0){
      o2<-sum(sapply(cf_away_opponents, function(x) (odds[odds$Team == x, ]$cfodds/sum(odds[odds$Team %in% c_opponents,]$cfodds)) * playoffDC(home = x, away = team)),
              na.rm = TRUE) * odds[odds$Team == team, ]$cfodds
    } else {
      o2 <- 0
    }

    return(o1+o2)

  } else if (round == 4){
    if(length(f_home_opponents) > 0) {
      o1 <- sum(sapply(f_home_opponents, function(x) (odds[odds$Team == x, ]$fodds/sum(odds[odds$Conference != odds[odds$Team == team,]$Conference,]$fodds)) * playoffDC(home = team, away = x)),
                na.rm = TRUE) * odds[odds$Team == team, ]$fodds
    } else {
      o1 <- 0
    }
    if(length(f_away_opponents) > 0){
      o2 <- sum(sapply(f_away_opponents, function(x) (odds[odds$Team == x, ]$fodds/sum(odds[odds$Conference != odds[odds$Team == team,]$Conference,]$fodds)) * playoffDC(home = x, away = team)),
                na.rm = TRUE) * odds[odds$Team == team, ]$fodds
    } else {
      o2 <- 0
    }

    return(o1+o2)
  } else {
    stop("Round must be in [1..4]")
  }
}

