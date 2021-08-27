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

  #remainderSeasonDC(nsims=10, cores=1, scores=scor, schedule = sched, regress = TRUE) testing passes results instead of home & Away goals
  if('HomeGoals' %in% colnames(scores)){
    tmp1<-scores %>%
      dplyr::group_by(.data$HomeTeam) %>%
      dplyr::summarise(
        GP = dplyr::n(),
        W = sum(.data$HomeGoals > .data$AwayGoals & .data$OTStatus == ''),
        OTW = sum(.data$HomeGoals > .data$AwayGoals & .data$OTStatus == 'OT'),
        SOW = sum(.data$HomeGoals > .data$AwayGoals & .data$OTStatus == 'SO'),
        OTL = sum(.data$HomeGoals < .data$AwayGoals & .data$OTStatus == 'OT'),
        SOL = sum(.data$HomeGoals < .data$AwayGoals & .data$OTStatus == 'SO'),
        L = sum(.data$HomeGoals < .data$AwayGoals & .data$OTStatus == ''),
        P = as.numeric(.data$W*2 + .data$OTW*2 + .data$SOW*2 + .data$OTL + .data$SOL)
      ) %>%
      dplyr::ungroup()
    tmp2<-scores %>%
      dplyr::group_by(.data$AwayTeam) %>%
      dplyr::summarise(
        GP = dplyr::n(),
        W = sum(.data$AwayGoals > .data$HomeGoals & .data$OTStatus == ''),
        OTW = sum(.data$AwayGoals > .data$HomeGoals & .data$OTStatus == 'OT'),
        SOW = sum(.data$AwayGoals > .data$HomeGoals & .data$OTStatus == 'SO'),
        OTL = sum(.data$AwayGoals < .data$HomeGoals & .data$OTStatus == 'OT'),
        SOL = sum(.data$AwayGoals < .data$HomeGoals & .data$OTStatus == 'SO'),
        L = sum(.data$AwayGoals < .data$HomeGoals & .data$OTStatus == ''),
        P = as.numeric(.data$W*2 + .data$OTW*2 + .data$SOW*2 + .data$OTL + .data$SOL)
      ) %>%
      dplyr::ungroup()
  } else if ('Result' %in% colnames(scores)) {
    tmp1<-scores %>%
      dplyr::group_by(.data$HomeTeam) %>%
      dplyr::summarise(
        GP = dplyr::n(),
        W = sum(.data$Result == 1),
        OTW = sum(.data$Result == 0.75),
        SOW = sum(.data$Result == 0.60),
        OTL = sum(.data$Result == 0.40),
        SOL = sum(.data$Result == 0.25),
        L = sum(.data$Result == 0),
        P = as.numeric(.data$W*2 + .data$OTW*2 + .data$SOW*2 + .data$OTL + .data$SOL)
      ) %>%
      dplyr::ungroup()
    tmp2<-scores %>%
      dplyr::group_by(.data$AwayTeam) %>%
      dplyr::summarise(
        GP = dplyr::n(),
        W = sum(.data$Result == 0),
        OTW = sum(.data$Result == 0.25),
        SOW = sum(.data$Result == 0.40),
        OTL = sum(.data$Result == 0.60),
        SOL = sum(.data$Result == 0.75),
        L = sum(.data$Result == 1),
        P = as.numeric(.data$W*2 + .data$OTW*2 + .data$SOW*2 + .data$OTL + .data$SOL)
      ) %>%
      dplyr::ungroup()
  } else {
    stop("Scores must contain home & away goal info or result info.")
  }

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

  team_stats<-team_stats %>%
    dplyr::mutate(Rank = rank(dplyr::desc(.data$Points), ties.method = 'random'),#TODO sort properly using point%, reg. win, ROW, W, head to head
                  Conf = getTeamConferences(.data$Team), #convenience data, dropped later
                  Div = getTeamDivisions(.data$Team)) %>%
    dplyr::group_by(.data$Conf) %>%
    dplyr::mutate(ConfRank = rank(dplyr::desc(.data$Points), ties.method = 'random')) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$Div) %>%
    dplyr::mutate(DivRank = rank(dplyr::desc(.data$Points), ties.method = 'random')) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Playoffs = ifelse(.data$DivRank <= 3, 1, 0)) %>%
    dplyr::group_by(.data$Conf, .data$Playoffs) %>%
    dplyr::mutate(Playoffs = ifelse(.data$Rank %in% utils::tail(sort(.data$Rank), 2), 1, .data$Playoffs)) %>% ## Renaming top two playoff teams as 'in' doesn't matter, because they're in already
    dplyr::ungroup() %>%
    dplyr::select(-c("Conf", "Div"))

  return(tibble::as_tibble(team_stats))
}


#' Today's Odds
#'
#' @description Determine today's games' odds (if today has games), or a specified date's odds
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param today The date for which you want game odds
#' @param schedule The schedule, default to internal schedule
#' @param expected_mean the mean lambda & mu, used only for regression
#' @param season_percent the percent complete of the season, used for regression
#'
#' @return a data frame of HomeTeam, AwayTeam, HomeWin, AwayWin, Draw, or NULL if no games today
#' @export
#'
#' @examples todayOdds(today=as.Date("2019-11-01"))
todayOdds <- function(params=NULL, today=Sys.Date(), schedule=HockeyModel::schedule, expected_mean=NULL, season_percent=NULL){
  return(todayDC(params=params, today=today, schedule=schedule, expected_mean=expected_mean, season_percent=season_percent))
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
    season_sofar<-scores[scores$Date > as.Date(getSeasonStartDate()),]

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
                            #ConfRank = rep(NA, n * nsims),
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
    #all_results[(n*(i-1) + 1):(n*i),]$ConfRank <- table$ConfRank
    all_results[(n*(i-1) + 1):(n*i),]$DivRank <- table$DivRank
    all_results[(n*(i-1) + 1):(n*i),]$Playoffs <- table$Playoffs

    if(progress){
      pb$tick()$print()
    }
  }

  summary_results<-all_results %>%
    dplyr::group_by(.data$Team) %>%
    dplyr::summarise(
      Playoffs =   mean(.data$Playoffs),
      meanPoints = mean(.data$Points, na.rm = TRUE),
      maxPoints = max(.data$Points, na.rm = TRUE),
      minPoints = min(.data$Points, na.rm = TRUE),
      meanWins = mean(.data$W, na.rm = TRUE),
      maxWins = max(.data$W, na.rm = TRUE),
      Presidents = sum(.data$Rank == 1)/dplyr::n(),
      meanRank = mean(.data$Rank, na.rm = TRUE),
      bestRank = min(.data$Rank, na.rm = TRUE),
      #meanConfRank = mean(.data$ConfRank, na.rm = TRUE),
      #bestConfRank = min(.data$ConfRank, na.rm = TRUE),
      meanDivRank = mean(.data$DivRank, na.rm = TRUE),
      bestDivRank = min(.data$DivRank, na.rm = TRUE),
      sdPoints = stats::sd(.data$Points, na.rm = TRUE),
      sdWins = stats::sd(.data$W, na.rm = TRUE),
      sdRank = stats::sd(.data$Rank, na.rm = TRUE),
      #sdConfRank = stats::sd(.data$ConfRank, na.rm = TRUE),
      sdDivRank = stats::sd(.data$DivRank, na.rm = TRUE)
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
    season_sofar<-scores[scores$Date > as.Date(getSeasonStartDate()),]
    season_sofar <- season_sofar[,c('Date','HomeTeam','AwayTeam','Result')]
  } else {
    season_sofar<-NULL
  }

  teamlist<-c(teamlist, sort(unique(c(as.character(schedule$Home), as.character(schedule$Away)))))

  if(!is.finite(cores)) {
    cores <- 2
  }

  if(cores > 1){
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
      table$SimNo<-i

      table
    }
    if(progress){
      close(pb)
    }
    parallel::stopCluster(cl)
    gc(verbose = FALSE)

  } else {
    all_results<-list()
    for(i in 1:nsims){
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
      table$SimNo<-i
      all_results[[i]]<-table
    }
    all_results<-dplyr::bind_rows(all_results)
  }

  summary_results<-all_results %>%
    dplyr::group_by(.data$Team) %>%
    dplyr::summarise(
      Playoffs = mean(.data$Playoffs),
      meanPoints = mean(.data$Points, na.rm = TRUE),
      maxPoints = max(.data$Points, na.rm = TRUE),
      minPoints = min(.data$Points, na.rm = TRUE),
      meanWins = mean(.data$W, na.rm = TRUE),
      maxWins = max(.data$W, na.rm = TRUE),
      Presidents = sum(.data$Rank == 1)/dplyr::n(),
      meanRank = mean(.data$Rank, na.rm = TRUE),
      bestRank = min(.data$Rank, na.rm = TRUE),
      #meanConfRank = mean(.data$ConfRank, na.rm = TRUE),
      #bestConfRank = min(.data$ConfRank, na.rm = TRUE),
      meanDivRank = mean(.data$DivRank, na.rm = TRUE),
      bestDivRank = min(.data$DivRank, na.rm = TRUE),
      sdPoints = stats::sd(.data$Points, na.rm = TRUE),
      sdWins = stats::sd(.data$W, na.rm = TRUE),
      sdRank = stats::sd(.data$Rank, na.rm = TRUE),
      #sdConfRank = stats::sd(.data$ConfRank, na.rm = TRUE),
      sdDivRank = stats::sd(.data$DivRank, na.rm = TRUE)
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
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param odds_table odds from remainderSeasonDC(Odds = TRUE), or null
#' @param season_sofar The results of the season to date
#'
#' @return a two member list, of all results and summary results
#' @export
loopless_sim<-function(nsims=1e5, cores = parallel::detectCores() - 1, schedule = HockeyModel::schedule, scores=HockeyModel::scores, params=NULL, odds_table = NULL, season_sofar=NULL){

  params<-parse_dc_params(params)

  nsims <- floor(nsims/cores)

  if(is.null(odds_table)){
    odds_table<-remainderSeasonDC(scores = scores, schedule = schedule, params=params, nsims = nsims, mu_lambda = TRUE)
  }
  #odds_table$Result <- NA

  if(is.null(season_sofar)){
    season_sofar<-scores[scores$Date > as.Date(getSeasonStartDate()),]
  }

  #if(is.na(season_sofar)) {
  #  season_sofar<-scores[NULL,]
  #}

  if(nrow(season_sofar) > 0){

    last_scores_date<-season_sofar[nrow(season_sofar), 'Date']
    odds_table<-odds_table[odds_table$Date > last_scores_date, ]

    season_sofar<-season_sofar[, c('Date', 'HomeTeam','AwayTeam','Result')]
    #season_sofar$HomeWin <- season_sofar$AwayWin <- season_sofar$Draw <- NA

    all_season<-dplyr::bind_rows(season_sofar, odds_table)
  } else {
    all_season <- odds_table
  }

  if(cores == 1){
    #for testing only, really.
    all_results<-sim_engine(all_season = all_season, nsims = nsims, params = params)
  } else {
    #this fixes CRAN checks
    `%dopar%` <- foreach::`%dopar%`
    #`%do%` <- foreach::`%do%`

    cl<-parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)



    #Ram management issues. Send smaller chunks more often, hopefully this helps.
    all_results <- foreach::foreach(i=1:(cores*5), .combine='rbind', .packages = "HockeyModel") %dopar% {
      all_results<-sim_engine(all_season = all_season, nsims = floor(nsims/5), params=params)
      return(all_results)
    }

    parallel::stopCluster(cl)
    gc(verbose = FALSE)
  }

  summary_results<-all_results %>%
    dplyr::group_by(.data$Team) %>%
    dplyr::summarise(
      Playoffs = mean(.data$Playoffs),
      meanPoints = mean(.data$Points, na.rm = TRUE),
      maxPoints = max(.data$Points, na.rm = TRUE),
      minPoints = min(.data$Points, na.rm = TRUE),
      meanWins = mean(.data$W, na.rm = TRUE),
      maxWins = max(.data$W, na.rm = TRUE),
      Presidents = sum(.data$Rank == 1)/dplyr::n(),
      meanRank = mean(.data$Rank, na.rm = TRUE),
      bestRank = min(.data$Rank, na.rm = TRUE),
      meanConfRank = mean(.data$ConfRank, na.rm = TRUE),
      bestConfRank = min(.data$ConfRank, na.rm = TRUE),
      meanDivRank = mean(.data$DivRank, na.rm = TRUE),
      bestDivRank = min(.data$DivRank, na.rm = TRUE),
      sdPoints = stats::sd(.data$Points, na.rm = TRUE),
      sdWins = stats::sd(.data$W, na.rm = TRUE),
      sdRank = stats::sd(.data$Rank, na.rm = TRUE),
      sdConfRank = stats::sd(.data$ConfRank, na.rm = TRUE),
      sdDivRank = stats::sd(.data$DivRank, na.rm = TRUE),
      p_rank1 = sum(.data$ConfRank == 1 & .data$DivRank == 1)/dplyr::n(),
      p_rank2 = sum(.data$ConfRank != 1 & .data$DivRank == 1)/dplyr::n(),
      # Solving 3 & 4 & 5 & 6 doesn't *really* matter, because 3/4 play the 5/6 within their own division.
      # In 2nd round, 1/8 or 2/7 play the 3/6 or 4/5 from their own division. No re-seeding occurs.
      # See: https://en.wikipedia.org/wiki/Stanley_Cup_playoffs#Current_format
      p_rank_34 = sum(.data$DivRank == 2)/dplyr::n(),
      p_rank_56 = sum(.data$DivRank == 3)/dplyr::n(),
      p_rank7 = sum(.data$Wildcard == 1)/dplyr::n(),
      p_rank8 = sum(.data$Wildcard == 2)/dplyr::n()
    )

  return(list(summary_results = summary_results, raw_results = all_results))
}

#' Simulation engine to be parallelized or used in single core
#'
#' @param all_season One seasons' scores & odds schedule
#' @param nsims Number of simulations to run
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return results of `nsims` season simulations, as one long data frame score table.
#' @export
sim_engine<-function(all_season, nsims, params=NULL){

  params<-parse_dc_params(params)

  season_length<-nrow(all_season)

  multi_season<-dplyr::bind_rows(replicate(nsims, all_season, simplify = FALSE))
  multi_season$sim<-rep(1:nsims, each = season_length)

  #Result <- dplyr::sym('Result')  # is.na(!!dplyr::sym('Result')) got really mad. offload to before call calmed it.

  if('lambda' %in% names(multi_season)){
    multi_season$Result<-dcResult(lambda = multi_season$lambda, mu = multi_season$mu, params = params)
  }

  if(!('Result' %in% names(multi_season)) | sum(is.na(multi_season$Result) > 0)){
    multi_season$r1<-stats::runif(n=nrow(multi_season))
    multi_season$r2<-stats::runif(n=nrow(multi_season))
    multi_season$r3<-stats::runif(n=nrow(multi_season))

    multi_season<-multi_season %>%
      mutate_cond(is.na(.data$Result), Result = 1*(as.numeric(.data$r1<.data$HomeWin)) +
                    0.75 * (as.numeric(.data$r1 > .data$HomeWin &
                                         .data$r1 < (.data$HomeWin + .data$Draw)) *
                              (as.numeric(.data$r2 > 0.5) * as.numeric(.data$r3 < 0.75))) +
                    0.6 * (as.numeric(.data$r1 > .data$HomeWin &
                                        .data$r1 < (.data$HomeWin + .data$Draw)) *
                             (as.numeric(.data$r2 > 0.5) * as.numeric (.data$r3 > 0.75))) +
                    0.4 * (as.numeric(.data$r1 > .data$HomeWin &
                                        .data$r1 < (.data$HomeWin + .data$Draw)) *
                             (as.numeric(.data$r2 < 0.5) * as.numeric (.data$r3 > 0.75))) +
                    0.25 * (as.numeric(.data$r1 > .data$HomeWin &
                                         .data$r1 < (.data$HomeWin + .data$Draw)) *
                              (as.numeric(.data$r2 < 0.5) * as.numeric (.data$r3 < 0.75))) +
                    0)

  }
  long_season<-data.frame(Team = c(as.character(multi_season$HomeTeam), as.character(multi_season$AwayTeam)), stringsAsFactors = FALSE)
  long_season$Result<-c(multi_season$Result, 1-multi_season$Result)
  long_season$SimNo<-c(multi_season$sim, multi_season$sim)

  rm(multi_season)

  all_results<-long_season %>%
    dplyr::group_by(.data$SimNo, .data$Team) %>%
    dplyr::summarise(W = sum(.data$Result == 1),
              OTW = sum(.data$Result == 0.75),
              SOW = sum(.data$Result == 0.6),
              SOL = sum(.data$Result == 0.4),
              OTL = sum(.data$Result == 0.25),
              L = sum(.data$Result == 0)) %>%
    as.data.frame()

  rm(long_season)


  all_results$Points<-all_results$W*2 + all_results$OTW*2 + all_results$SOW*2 + all_results$OTL + all_results$SOL

  all_results$Conference <- getTeamConferences(all_results$Team)
  all_results$Division <- getTeamDivisions(all_results$Team)
  all_results$Wildcard <- NA

  all_results <- all_results %>%
    dplyr::group_by(.data$SimNo) %>%
    dplyr::mutate(Rank = rank(-.data$Points, ties.method = 'random')) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$SimNo, .data$Conference) %>%
    dplyr::mutate(ConfRank = rank(.data$Rank)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$SimNo, .data$Division) %>%
    dplyr::mutate(DivRank = rank(.data$ConfRank)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Playoffs = ifelse(.data$DivRank <=3, 1, 0)) %>%
    dplyr::group_by(.data$SimNo, .data$Conference) %>%
    dplyr::arrange(.data$Playoffs, .data$ConfRank) %>%
    dplyr::mutate(Wildcard = ifelse(.data$Playoffs == 0, dplyr::row_number(), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Playoffs = ifelse(.data$DivRank <= 3, 1, 0)) %>% #Top 3 in each division
    dplyr::arrange(.data$SimNo, .data$Team) %>%
    #mutate_cond(.data$Wildcard <= 2, Playoffs = 1) %>% # Wildcard 2 teams/conference
    dplyr::select(.data$SimNo, .data$Team, .data$W, .data$OTW,
                  .data$SOW, .data$SOL, .data$OTL, .data$Points,
                  .data$Wildcard, .data$Rank, .data$ConfRank,
                  .data$DivRank, .data$Playoffs)

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
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return Odds from 0-1 of home team winning. Away odds are 1 - return value
#' @export
playoffWin<-function(home_team, away_team, home_wins = 0, away_wins = 0, params=NULL){
  params<-parse_dc_params(params)
  home_odds<-DCPredict(home = home_team, away = away_team, draws=FALSE, params=params)[1]
  away_odds<-1-DCPredict(home = away_team, away = home_team, draws=FALSE,  params=params)[1]
  return(playoffSeriesOdds(home_odds = home_odds, away_odds = away_odds, home_win = home_wins, away_win = away_wins))
}

#' Random Series Winner
#'
#' @description generate a random series winner given a home and away team
#'
#' @param home_team Home Team name (required)
#' @param away_team Away Team name (Required)
#' @param home_wins Number of home wins (default 0)
#' @param away_wins Number of away team wins (default 0)
#' @param homeAwayOdds pre-calculated home & away team parings odds of a home win. Overrides playoffwin calculation
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return TRUE if the home team wins, else FALSE
#' @export
randomSeriesWinner<-function(home_team, away_team, home_wins=0, away_wins=0, homeAwayOdds = NULL, params=NULL){
  if(is.null(homeAwayOdds)){
    params<-parse_dc_params(params)
    return(ifelse(stats::runif(1)<playoffWin(home_team=home_team, away_team=away_team, home_wins=home_wins, away_wins=away_wins, params=params),
           home_team, away_team))
  } else {
    hao<-homeAwayOdds[homeAwayOdds$HomeTeam == home_team & homeAwayOdds$AwayTeam == away_team, ]
    if(nrow(hao) == 1){
      return(ifelse(stats::runif(1)<hao$HomeOdds, home_team, away_team))
    } else {
      #Calculated odds aren't in there, get it manually
      params<-parse_dc_params(params)
      return(ifelse(stats::runif(1)<playoffWin(home_team=home_team, away_team=away_team, home_wins=home_wins, away_wins=away_wins, params = params),
                    home_team, away_team))
    }
  }
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
#' @param ngames Number of games in the series, defaults to 7
#' @param game_home vector of T/F for 'home team' home games. Defaults for NHL best of 7 series: \code{c(T,T,F,F,T,F,T)}
#' @param predict_games_to_win (Defualt False) If TRUE, returns the table of ways the series could finish.
#'
#' @return numeric odds of home team win series (1-odds for away odds)
#' @export
playoffSeriesOdds<-function(home_odds, away_odds, home_win=0, away_win=0, ngames=NULL, game_home=NULL, predict_games_to_win = FALSE){
  if(is.null(ngames)){
    ngames <- 7
  }
  if(is.null(game_home) & ngames == 7){
    game_home <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  } else {
    game_home <- rep(c(TRUE, FALSE), as.integer(ngames+1/2))[1:ngames]
  }

  game_to <- ceiling(ngames/2)

  if(length(home_odds)>1 | length(away_odds) > 1){
    stop("handle only one series at a time")
  }
  p1_home<-home_odds
  p1_road<-away_odds

  if (p1_home < 0 | p1_home > 1 | p1_road < 0 | p1_road > 1){
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

  if(predict_games_to_win == FALSE){
    p1total <- sum(finished_series[finished_series[,2] == game_to, 4])

    return(p1total)
  } else {
    return(unfinished_series)
  }
}


#' simulate Playoffs
#'
#' @description Solves playoff odds by MC simulation.
#'
#' @param summary_results summary results
#' @param nsims Number of playoff sims to run. Too many takes a long time.
#' @param cores Number of processor cores to use
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return a data frame of each teams' odds of winning each round (First Round, Second Round, Conference Finals and Stanley Cup)
#' @export
simulatePlayoffs<-function(summary_results=NULL, nsims=1e5, cores = parallel::detectCores() - 1, params=NULL){
  params <- parse_dc_params(params)
  if(is.null(summary_results)){
    filelist<-list.files(path = "./prediction_results")
    pdates<-substr(filelist, 1, 10)  # gets the dates list of prediction
    pdates<-pdates[pdates != 'graphics']
    lastp<-as.Date(max(pdates))
    summary_results<-readRDS(file.path("./prediction_results", paste0(lastp,"-predictions.RDS")))
  }

  summary_results<-summary_results %>%
    dplyr::mutate("Conf" = getTeamConferences(.data$Team),
                  "Div" = getTeamDivisions(.data$Team))
  if('p_rank3' %in% names(summary_results)){
    #Shortcut for having prank3, 4, 5, 6 instead of prank34, and prank56. add them
    summary_results <- summary_results %>%
      dplyr::mutate("p_rank_34" = .data$p_rank3 + .data$p_rank4,
                    "p_rank_56" = .data$p_rank5 + .data$p_rank6)
  }
  east_results<-summary_results %>% dplyr::filter(.data$Conf == "Eastern")
  west_results<-summary_results %>% dplyr::filter(.data$Conf == "Western")

  homeAwayOdds<-getAllHomeAwayOdds(summary_results$Team, params=params)

  simresults<-data.frame("SimNo" = integer(),
                         "l1" = character(),
                         "l2" = character(),
                         "l3" = character(),
                         "l4" = character(),
                         "l5" = character(),
                         "l6" = character(),
                         "l7" = character(),
                         "l8" = character(),
                         "series1" = character(),
                         "series2" = character(),
                         "series3" = character(),
                         "series4" = character(),
                         "series5" = character(),
                         "series6" = character(),
                         "series7" = character(),
                         "series8" = character(),
                         "series9" = character(),
                         "series10" = character(),
                         "series11" = character(),
                         "series12" = character(),
                         "series13" = character(),
                         "series14" = character(),
                         "series15" = character())

  currentSeries<-getAPISeries()

  if(is.na(currentSeries)){
    message("too early to mix in real-life series")
    completedSeries<-data.frame("Series" = character(), "Winner" = character(), "Loser" = character())
    currentSeries<-data.frame("Round" = integer(), "Series" = integer(), "HomeTeam" = character(), "AwayTeam" = character(),
                              "HomeWins" = integer(), "AwayWins" = integer(), "HomeSeed" = integer(), "AwaySeed" = integer(),
                              "Statsu" = character(), "SeriesID" = integer())
  } else {
    completedSeries<-getCompletedSeries(currentSeries)
    for(s in currentSeries[currentSeries$Status == "Ongoing", ]$SeriesID){
      homeAwayOdds[homeAwayOdds$HomeTeam == currentSeries[currentSeries$SeriesID == s, ]$HomeTeam &
                     homeAwayOdds$AwayTeam == currentSeries[currentSeries$SeriesID == s, ]$AwayTeam, ]$HomeOdds <-
        playoffWin(home_team = currentSeries[currentSeries$SeriesID == s, ]$HomeTeam,
                   away_team = currentSeries[currentSeries$SeriesID == s, ]$AwayTeam,
                   home_wins = currentSeries[currentSeries$SeriesID == s, ]$HomeWins,
                   away_wins = currentSeries[currentSeries$SeriesID == s, ]$AwayWins,
                   params = params)
    }
  }

  if(cores > 1){
    cl<-parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)


    `%dopar%` <- foreach::`%dopar%`

    simresults <- foreach::foreach(i=1:(cores*5), .combine='rbind', .packages = "HockeyModel") %dopar% {
      simresults<-playoffSolverEngine(nsims = ceiling(nsims/(cores*5)), completedSeries = completedSeries, east_results = east_results, west_results = west_results, currentSeries=currentSeries, summary_results = summary_results, homeAwayOdds=homeAwayOdds)
      return(simresults)
    }

    parallel::stopCluster(cl)
    gc(verbose = FALSE)

  } else{
    #Single cores is easier for testing
    simresults<-playoffSolverEngine(nsims = nsims, completedSeries = completedSeries, east_results = east_results, west_results = west_results, currentSeries=currentSeries, summary_results = summary_results, homeAwayOdds=homeAwayOdds)

  }

  simodds<-data.frame("Team" = summary_results$Team)

  simodds<-simodds %>%
    dplyr::rowwise() %>%
    dplyr::mutate("Make_Playoffs" =(nrow(simresults[simresults$series1 == .data$Team, ]) +
                    nrow(simresults[simresults$series2 == .data$Team, ]) +
                    nrow(simresults[simresults$series3 == .data$Team, ]) +
                    nrow(simresults[simresults$series4 == .data$Team, ]) +
                    nrow(simresults[simresults$series5 == .data$Team, ]) +
                    nrow(simresults[simresults$series6 == .data$Team, ]) +
                    nrow(simresults[simresults$series7 == .data$Team, ]) +
                    nrow(simresults[simresults$series8 == .data$Team, ]) +
                    nrow(simresults[simresults$l1 == .data$Team, ]) +
                    nrow(simresults[simresults$l2 == .data$Team, ]) +
                    nrow(simresults[simresults$l3 == .data$Team, ]) +
                    nrow(simresults[simresults$l4 == .data$Team, ]) +
                    nrow(simresults[simresults$l5 == .data$Team, ]) +
                    nrow(simresults[simresults$l6 == .data$Team, ]) +
                    nrow(simresults[simresults$l7 == .data$Team, ]) +
                    nrow(simresults[simresults$l8 == .data$Team, ]))/nrow(simresults),
                  "Win_First_Round" = (nrow(simresults[simresults$series1 == .data$Team, ]) +
                    nrow(simresults[simresults$series2 == .data$Team, ]) +
                    nrow(simresults[simresults$series3 == .data$Team, ]) +
                    nrow(simresults[simresults$series4 == .data$Team, ]) +
                    nrow(simresults[simresults$series5 == .data$Team, ]) +
                    nrow(simresults[simresults$series6 == .data$Team, ]) +
                    nrow(simresults[simresults$series7 == .data$Team, ]) +
                    nrow(simresults[simresults$series8 == .data$Team, ]))/nrow(simresults),
                  "Win_Second_Round" = (nrow(simresults[simresults$series9 == .data$Team, ]) +
                    nrow(simresults[simresults$series10 == .data$Team, ]) +
                    nrow(simresults[simresults$series11 == .data$Team, ]) +
                    nrow(simresults[simresults$series12 == .data$Team, ]))/nrow(simresults),
                  "Win_Conference" = (nrow(simresults[simresults$series13 == .data$Team, ]) +
                    nrow(simresults[simresults$series14 == .data$Team, ]))/nrow(simresults),
                  "Win_Cup" = nrow(simresults[simresults$series15 == .data$Team, ])/nrow(simresults)) %>%
    dplyr::arrange(dplyr::desc(.data$Win_Cup), dplyr::desc(.data$Win_Conference), dplyr::desc(.data$Win_Second_Round), dplyr::desc(.data$Win_First_Round), dplyr::desc(.data$Make_Playoffs), .data$Team) %>%
    as.data.frame()

  return(simodds)
}

reseedTwoTeams<-function(team1, team2, summary_results, p1=NULL){
  t1p<-summary_results[summary_results$Team == team1, ]$meanPoints
  t2p<-summary_results[summary_results$Team == team2, ]$meanPoints

  if(!is.null(p1) && !is.null(nrow(p1)) && nrow(p1) > 0){
    if(team1 == p1){
      return(c(team1, team2))
    } else if (team2 == p1) {
      return(c(team2, team1))
    }
  }
  if(t1p > t2p){
    return(c(team1, team2))
  } else if (t2p > t1p){
    return(c(team2, team1))
  } else {
    if(stats::runif(1)<0.5){
      return(c(team1, team2))
    } else {
      return(c(team2, team1))
    }
  }
}

#' Given current seriess and a series number and home and away teams, either return true series winner, random series winner (with home/away wins considered) or random series winner
#'
#' @param series_number Series number from 1:15
#' @param currentSeries the full list of series returned from getAPISeries
#' @param homeTeam Home Team extracted from summary_results
#' @param awayTeam away Team extracted from summary_results
#' @param homeAwayOdds if calculated, the odds of a home or away team win
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return a series winner (team name)
single_series_solver<-function(series_number, currentSeries, homeTeam, awayTeam, homeAwayOdds = NULL, params=NULL){
  params<-parse_dc_params(params)
  if(is.na(currentSeries) || nrow(currentSeries) == 0){
    return(randomSeriesWinner(homeTeam, awayTeam, homeAwayOdds = homeAwayOdds, params=params))
  }
  series<-currentSeries[currentSeries$SeriesID == series_number,]
  if(nrow(series[series$Status == "Complete",]) == 1){
    if(series$HomeTeam != homeTeam | series$AwayTeam != awayTeam){
      warning("Team Mismatch series ", series_number, ". Home Team expected ", series$HomeTeam, " got ", homeTeam, ". Away Team expected ", series$AwayTeam, " got ", awayTeam,". Using API series information.")
    }
    return(ifelse(series$HomeWins > series$AwayWins, series$HomeTeam, series$AwayTeam))
  } else if (nrow(currentSeries[currentSeries == series_number & currentSeries$Status == "Ongoing",]) == 1) {
    if(series$HomeTeam != homeTeam | series$AwayTeam != awayTeam){
      warning("Team Mismatch series ", series_number, ". Home Team expected ", series$HomeTeam, " got ", homeTeam, ". Away Team expected ", series$AwayTeam, " got ", awayTeam,". Using API series information.")
    }
    return(randomSeriesWinner(series$HomeTeam, series$AwayTeam, home_wins = series$HomeWins, away_wins = series$AwayWins, homeAwayOdds = homeAwayOdds, params=params))
  } else {
    return(randomSeriesWinner(homeTeam, awayTeam, homeAwayOdds = homeAwayOdds, params=params))
  }
}

getCompletedSeries<-function(currentSeries){
  completedSeries<-currentSeries %>%
    dplyr::filter(.data$Status == 'Complete') %>%
    dplyr::mutate("Winner" = dplyr::case_when(
      .data$HomeWins > .data$AwayWins ~ .data$HomeTeam,
      .data$HomeWins < .data$AwayWins ~ .data$AwayTeam),
                  "Loser" = dplyr::case_when(
      .data$HomeWins > .data$AwayWins ~ .data$AwayTeam,
      .data$HomeWins < .data$AwayWins ~ .data$HomeTeam),
                  "Series" = paste0('series', .data$SeriesID)) %>%
    dplyr::select(c('Series', 'Winner', 'Loser'))
  return(completedSeries)
}

#' Playoff Solver Engine
#'
#' @description Does the actual simulating. A function so it's parallelizable. Not to be called directly
#' @param nsims number of sims (in each core)
#' @param completedSeries completed series
#' @param east_results east_results
#' @param west_results west_results
#' @param currentSeries currentSeries
#' @param summary_results summary_results
#' @param homeAwayOdds precalculated home & away pairs of odds - if available.
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @export
playoffSolverEngine<-function(nsims,completedSeries,east_results, west_results, currentSeries, summary_results, homeAwayOdds, params=NULL){
  params<-parse_dc_params(params)
  simresults<-data.frame("SimNo" = integer(),
                         "l1" = character(),
                         "l2" = character(),
                         "l3" = character(),
                         "l4" = character(),
                         "l5" = character(),
                         "l6" = character(),
                         "l7" = character(),
                         "l8" = character(),
                         "series1" = character(),
                         "series2" = character(),
                         "series3" = character(),
                         "series4" = character(),
                         "series5" = character(),
                         "series6" = character(),
                         "series7" = character(),
                         "series8" = character(),
                         "series9" = character(),
                         "series10" = character(),
                         "series11" = character(),
                         "series12" = character(),
                         "series13" = character(),
                         "series14" = character(),
                         "series15" = character())
  srvec<-c()
  apiteams<-nhlapi::nhl_teams()

  east_results<-as.data.frame(east_results)
  west_results<-as.data.frame(west_results)

  for (sim in 1:nsims){
    if(all(paste0('series', 1:8) %in% completedSeries$Series)){
      series1<-completedSeries[completedSeries$Series == 'series1', ]$Winner
      series2<-completedSeries[completedSeries$Series == 'series2', ]$Winner
      series3<-completedSeries[completedSeries$Series == 'series3', ]$Winner
      series4<-completedSeries[completedSeries$Series == 'series4', ]$Winner
      series5<-completedSeries[completedSeries$Series == 'series5', ]$Winner
      series6<-completedSeries[completedSeries$Series == 'series6', ]$Winner
      series7<-completedSeries[completedSeries$Series == 'series7', ]$Winner
      series8<-completedSeries[completedSeries$Series == 'series8', ]$Winner
      l1<-completedSeries[completedSeries$Series == 'series1', ]$Loser
      l2<-completedSeries[completedSeries$Series == 'series2', ]$Loser
      l3<-completedSeries[completedSeries$Series == 'series3', ]$Loser
      l4<-completedSeries[completedSeries$Series == 'series4', ]$Loser
      l5<-completedSeries[completedSeries$Series == 'series5', ]$Loser
      l6<-completedSeries[completedSeries$Series == 'series6', ]$Loser
      l7<-completedSeries[completedSeries$Series == 'series7', ]$Loser
      l8<-completedSeries[completedSeries$Series == 'series8', ]$Loser
    } else {
      #solve east conference
      er<-east_results
      if('series1' %in% completedSeries$Series){
        series1 <- completedSeries[completedSeries$Series == 'series1', ]$Winner
        l1 <- completedSeries[completedSeries$Series == 'series1', ]$Loser
        er<-er[er$Team != series1,]
        er<-er[er$Team != l1,]
      } else if (1 %in% currentSeries$SeriesID){
        e1.1 <- currentSeries[currentSeries$SeriesID == 1, ]$HomeTeam
        er<-er[er$Team != e1.1,]
        ewc2 <- currentSeries[currentSeries$SeriesID == 1, ]$AwayTeam
        er<-er[er$Team != ewc2,]
        odds<-playoffWin(e1.1, ewc2, currentSeries[currentSeries$SeriesID == 1, ]$HomeWins,
                         currentSeries[currentSeries$SeriesID == 1, ]$AwayWins, params = params)
        series1<-sample(c(e1.1, ewc2), size = 1, prob = c(odds, 1-odds))
        l1 <- ifelse(series4 == e1.1, ewc2, e1.1) #If winner = a, then b, else a
      } else {
        e1.1<-er[sample(1:nrow(er), size = 1, prob = er$p_rank1),]$Team
        er<-er[er$Team != e1.1,]
        ewc2<-er[sample(1:nrow(er), size = 1, prob = er$p_rank8),]$Team
        er<-er[er$Team != ewc2,]
        series1 <- single_series_solver(series_number = 1, currentSeries = currentSeries, homeTeam = e1.1, awayTeam = ewc2, homeAwayOdds = homeAwayOdds)
        l1 <- ifelse(series1 == e1.1, ewc2, e1.1) #If winner = a, then b, else a
      }
      p1div<-getTeamDivisions(e1.1, apiteams=apiteams)

      if('series2' %in% completedSeries$Series){
        series2 <- completedSeries[completedSeries$Series == 'series2', ]$Winner
        l2 <- completedSeries[completedSeries$Series == 'series2', ]$Loser
        er<-er[er$Team != series2,]
        er<-er[er$Team != l2,]
      } else if (2 %in% currentSeries$SeriesID){
        e1.2 <- currentSeries[currentSeries$SeriesID == 2, ]$HomeTeam
        er<-er[er$Team != e1.1,]
        e1.3 <- currentSeries[currentSeries$SeriesID == 2, ]$AwayTeam
        er<-er[er$Team != e1.3,]
        odds<-playoffWin(e1.2, e1.3, currentSeries[currentSeries$SeriesID == 2, ]$HomeWins,
                         currentSeries[currentSeries$SeriesID == 2, ]$AwayWins, params = params)
        series2<-sample(c(e1.2, e1.3), size = 1, prob = c(odds, 1-odds))
        l2 <- ifelse(series2 == e1.2, e1.3, e1.2) #If winner = a, then b, else a
      } else {
        e1.2<-er[er$Div == p1div,]$Team[sample(1:nrow(er[er$Div == p1div, ]), size = 1, prob = er[er$Div == p1div, ]$p_rank_34)]
        er<-er[er$Team != e1.2,]
        e1.3<-er[er$Div == p1div,]$Team[sample(1:nrow(er[er$Div == p1div, ]), size = 1, prob = er[er$Div == p1div, ]$p_rank_56)]
        er<-er[er$Team != e1.3,]
        series2 <- single_series_solver(series_number = 2, currentSeries = currentSeries, homeTeam = e1.2, awayTeam = e1.3, homeAwayOdds = homeAwayOdds)
        l2 <- ifelse(series2 == e1.2, e1.3, e1.2) #If winner = a, then b, else a
      }

      if('series3' %in% completedSeries$Series){
        series3 <- completedSeries[completedSeries$Series == 'series3', ]$Winner
        l3 <- completedSeries[completedSeries$Series == 'series3', ]$Loser
        er<-er[er$Team != series3,]
        er<-er[er$Team != l3,]
      } else if (3 %in% currentSeries$SeriesID){
        e2.1 <- currentSeries[currentSeries$SeriesID == 3, ]$HomeTeam
        er<-er[er$Team != e2.1,]
        ewc1 <- currentSeries[currentSeries$SeriesID == 3, ]$AwayTeam
        er<-er[er$Team != ewc1,]
        odds<-playoffWin(e2.1, ewc1, currentSeries[currentSeries$SeriesID == 3, ]$HomeWins,
                         currentSeries[currentSeries$SeriesID == 3, ]$AwayWins, params = params)
        series3<-sample(c(e2.1, ewc1), size = 1, prob = c(odds, 1-odds))
        l3 <- ifelse(series3 == e2.2, e2.3, e2.2) #If winner = a, then b, else a
      } else {
        e2.1<-er[sample(1:nrow(er), size = 1, prob = er$p_rank2),]$Team
        er<-er[er$Team != e2.1,]
        ewc1<-er[sample(1:nrow(er), size = 1, prob = er$p_rank7),]$Team
        er<-er[er$Team != ewc1,]
        series3 <- single_series_solver(series_number = 3, currentSeries = currentSeries, homeTeam = e2.1, awayTeam = ewc1, homeAwayOdds = homeAwayOdds)
        l3 <- ifelse(series3 == e2.1, ewc1, e2.1) #If winner = a, then b, else a
      }

      if('series4' %in% completedSeries$Series){
        series4 <- completedSeries[completedSeries$Series == 'series4', ]$Winner
        l4 <- completedSeries[completedSeries$Series == 'series4', ]$Loser
        er<-er[er$Team != series4,]
        er<-er[er$Team != l4,]
      } else if (4 %in% currentSeries$SeriesID){
        e2.2 <- currentSeries[currentSeries$SeriesID == 4, ]$HomeTeam
        er<-er[er$Team != e2.2,]
        e2.3 <- currentSeries[currentSeries$SeriesID == 4, ]$AwayTeam
        er<-er[er$Team != e2.3,]
        odds<-playoffWin(e2.2, e2.3, currentSeries[currentSeries$SeriesID == 4, ]$HomeWins,
                         currentSeries[currentSeries$SeriesID == 4, ]$AwayWins, params = params)
        series4<-sample(c(e2.2, e2.3), size = 1, prob = c(odds, 1-odds))
        l4 <- ifelse(series4 == e2.2, e2.3, e2.2) #If winner = a, then b, else a
      } else {
        e2.2<-er[er$Div != p1div,]$Team[sample(1:nrow(er[er$Div != p1div, ]), size = 1, prob = er[er$Div != p1div, ]$p_rank_34)]
        er<-er[er$Team != e2.2,]
        e2.3<-er[er$Div != p1div,]$Team[sample(1:nrow(er[er$Div != p1div, ]), size = 1, prob = er[er$Div != p1div, ]$p_rank_56)]
        er<-er[er$Team != e2.3,]
        series4 <- single_series_solver(series_number = 4, currentSeries = currentSeries, homeTeam = e2.2, awayTeam = e2.3, homeAwayOdds = homeAwayOdds)
        l4 <- ifelse(series4 == e2.2, e2.3, e2.2) #If winner = a, then b, else a
      }

      wr<-west_results

      if('series5' %in% completedSeries$Series){
        series5 <- completedSeries[completedSeries$Series == 'series5', ]$Winner
        l5 <- completedSeries[completedSeries$Series == 'series5', ]$Loser
        wr<-wr[wr$Team != series5,]
        wr<-wr[wr$Team != l5,]
      } else if (5 %in% currentSeries$SeriesID){
        w1.1 <- currentSeries[currentSeries$SeriesID == 5, ]$HomeTeam
        wr<-wr[wr$Team != w1.1,]
        wwc2 <- currentSeries[currentSeries$SeriesID == 5, ]$AwayTeam
        wr<-wr[wr$Team != wwc2,]
        odds<-playoffWin(w1.1, wwc2, currentSeries[currentSeries$SeriesID == 5, ]$HomeWins,
                         currentSeries[currentSeries$SeriesID == 5, ]$AwayWins, params = params)
        series5<-sample(c(w1.1, wwc2), size = 1, prob = c(odds, 1-odds))
        l5 <- ifelse(series5 == w1.1, wwc2, w1.1) #If winner = a, then b, else a
      } else {
        w1.1<-wr[sample(1:nrow(wr), size = 1, prob = wr$p_rank1),]$Team
        wr<-wr[wr$Team != w1.1,]
        wwc2<-wr[sample(1:nrow(wr), size = 1, prob = wr$p_rank8),]$Team
        wr<-wr[wr$Team != wwc2,]
        series5 <- single_series_solver(series_number = 5, currentSeries = currentSeries, homeTeam = w1.1, awayTeam = wwc2, homeAwayOdds = homeAwayOdds)
        l5 <- ifelse(series5 == w1.1, wwc2, w1.1) #If winner = a, then b, else a
      }
      p1div<-getTeamDivisions(w1.1, apiteams=apiteams)

      if('series6' %in% completedSeries$Series){
        series6 <- completedSeries[completedSeries$Series == 'series6', ]$Winner
        l6 <- completedSeries[completedSeries$Series == 'series6', ]$Loser
        wr<-wr[wr$Team != series6,]
        wr<-wr[wr$Team != l6,]
      } else if (6 %in% currentSeries$SeriesID) {
        w1.2 <- currentSeries[currentSeries$SeriesID == 6, ]$HomeTeam
        wr<-wr[wr$Team != w1.1,]
        w1.3 <- currentSeries[currentSeries$SeriesID == 6, ]$AwayTeam
        wr<-wr[wr$Team != w1.3,]
        odds<-playoffWin(w1.2, w1.3, currentSeries[currentSeries$SeriesID == 6, ]$HomeWins,
                         currentSeries[currentSeries$SeriesID == 6, ]$AwayWins, params = params)
        series6<-sample(c(w1.2, w1.3), size = 1, prob = c(odds, 1-odds))
        l6 <- ifelse(series6 == w1.2, w1.3, w1.2) #If winner = a, then b, else a
      } else {
        w1.2<-wr[wr$Div == p1div,]$Team[sample(1:nrow(wr[wr$Div == p1div, ]), size = 1, prob = wr[wr$Div == p1div, ]$p_rank_34)]
        wr<-wr[wr$Team != w1.2,]
        w1.3<-wr[wr$Div == p1div,]$Team[sample(1:nrow(wr[wr$Div == p1div, ]), size = 1, prob = wr[wr$Div == p1div, ]$p_rank_56)]
        wr<-wr[wr$Team != w1.3,]
        series6 <- single_series_solver(series_number = 6, currentSeries = currentSeries, homeTeam = w1.2, awayTeam = w1.3, homeAwayOdds = homeAwayOdds)
        l6 <- ifelse(series6 == w1.2, w1.3, w1.2) #If winner = a, then b, else a
      }

      if('series7' %in% completedSeries$Series){
        series7 <- completedSeries[completedSeries$Series == 'series7', ]$Winner
        l7 <- completedSeries[completedSeries$Series == 'series7', ]$Loser
        wr<-wr[wr$Team != series7,]
        wr<-wr[wr$Team != l7,]
      } else if (7 %in% currentSeries$SeriesID){
        w2.1 <- currentSeries[currentSeries$SeriesID == 7, ]$HomeTeam
        wr<-wr[wr$Team != w1.1,]
        wwc1 <- currentSeries[currentSeries$SeriesID == 7, ]$AwayTeam
        wr<-wr[wr$Team != wwc1,]
        odds<-playoffWin(w2.1, wwc1, currentSeries[currentSeries$SeriesID == 7, ]$HomeWins,
                         currentSeries[currentSeries$SeriesID == 7, ]$AwayWins, params = params)
        series7<-sample(c(w2.1, wwc1), size = 1, prob = c(odds, 1-odds))
        l7 <- ifelse(series7 == w2.1, wwc1, w2.1) #If winner = a, then b, else a
      } else {
        w2.1<-wr[sample(1:nrow(wr), size = 1, prob = wr$p_rank2),]$Team
        wr<-wr[wr$Team != w2.1,]
        wwc1<-wr[sample(1:nrow(wr), size = 1, prob = wr$p_rank7),]$Team
        wr<-wr[wr$Team != wwc1,]
        series7 <- single_series_solver(series_number = 7, currentSeries = currentSeries, homeTeam = w2.1, awayTeam = wwc1, homeAwayOdds = homeAwayOdds)
        l7 <- ifelse(series7 == w2.1, wwc1, w2.1) #If winner = a, then b, else a
      }

      if('series8' %in% completedSeries$Series){
        series8 <- completedSeries[completedSeries$Series == 'series8', ]$Winner
        l8 <- completedSeries[completedSeries$Series == 'series8', ]$Loser
        wr<-wr[wr$Team != series8,]
        wr<-wr[wr$Team != l8,]
      } else if (8 %in% currentSeries$SeriesID){
        w2.2 <- currentSeries[currentSeries$SeriesID == 8, ]$HomeTeam
        wr<-wr[wr$Team != w2.2,]
        w2.3 <- currentSeries[currentSeries$SeriesID == 8, ]$AwayTeam
        wr<-wr[wr$Team != w2.3,]
        odds<-playoffWin(w2.2, w2.3, currentSeries[currentSeries$SeriesID == 8, ]$HomeWins,
                         currentSeries[currentSeries$SeriesID == 8, ]$AwayWins, params = params)
        series8<-sample(c(w2.2, w2.3), size = 1, prob = c(odds, 1-odds))
        l8 <- ifelse(series8 == w2.2, w2.3, w2.2) #If winner = a, then b, else a
      } else {
        w2.2<-wr[wr$Div != p1div,]$Team[sample(1:nrow(wr[wr$Div != p1div, ]), size = 1, prob = wr[wr$Div != p1div, ]$p_rank_34)]
        wr<-wr[wr$Team != w2.2,]
        w2.3<-wr[wr$Div != p1div,]$Team[sample(1:nrow(wr[wr$Div != p1div, ]), size = 1, prob = wr[wr$Div != p1div, ]$p_rank_56)]
        wr<-wr[wr$Team != w2.3,]
        series8 <- single_series_solver(series_number = 4, currentSeries = currentSeries, homeTeam = w2.2, awayTeam = w2.3, homeAwayOdds = homeAwayOdds)
        l8 <- ifelse(series8 == w2.2, w2.3, w2.2) #If winner = a, then b, else a
      }
    }

    #No reseeding for round 2 (but in reality yeah there is, wildCard doesn't have home advantage)
    if('series9' %in% completedSeries$Series){
      series9 <- completedSeries[completedSeries$Series == 'series9', ]$Winner
    } else {
      rs<-reseedTwoTeams(series1, series2, summary_results, currentSeries[currentSeries$SeriesID == 1,]$HomeTeam)
      series9 <- single_series_solver(series_number = 9, currentSeries = currentSeries, homeTeam = rs[1], awayTeam = rs[2], homeAwayOdds = homeAwayOdds)
    }

    if('series10' %in% completedSeries$Series){
      series10 <- completedSeries[completedSeries$Series == 'series10', ]$Winner
    } else {
      rs<-reseedTwoTeams(series3, series4, summary_results, currentSeries[currentSeries$SeriesID == 3, ]$HomeTeam)
      series10 <- single_series_solver(series_number = 10, currentSeries = currentSeries, homeTeam = rs[1], awayTeam = rs[2], homeAwayOdds = homeAwayOdds)
    }

    if('series11' %in% completedSeries$Series){
      series11 <- completedSeries[completedSeries$Series == 'series11', ]$Winner
    } else {
      rs<-reseedTwoTeams(series5, series6, summary_results, currentSeries[currentSeries$SeriesID == 5, ]$HomeTeam)
      series11 <- single_series_solver(series_number = 11, currentSeries = currentSeries, homeTeam = rs[1], awayTeam = rs[2], homeAwayOdds = homeAwayOdds)
    }

    if('series12' %in% completedSeries$Series){
      series12 <- completedSeries[completedSeries$Series == 'series12', ]$Winner
    } else {
      rs<-reseedTwoTeams(series7, series8, summary_results, currentSeries[currentSeries$SeriesID == 7,]$HomeTeam)
      series12 <- single_series_solver(series_number = 12, currentSeries = currentSeries, homeTeam = rs[1], awayTeam = rs[2], homeAwayOdds = homeAwayOdds)
    }

    #Reseed for conference finals & stanley cup finals
    if('series13' %in% completedSeries$Series){
      series13 <- completedSeries[completedSeries$Series == 'series13', ]$Winner
    } else {
      rs<-reseedTwoTeams(series9, series10, summary_results)
      series13 <- single_series_solver(series_number = 13, currentSeries = currentSeries, homeTeam = rs[1], awayTeam = rs[2], homeAwayOdds = homeAwayOdds)
    }

    if('series14' %in% completedSeries$Series){
      series14 <- completedSeries[completedSeries$Series == 'series14', ]$Winner
    } else {
      rs<-reseedTwoTeams(series11, series12, summary_results)
      series14 <- single_series_solver(series_number = 14, currentSeries = currentSeries, homeTeam = rs[1], awayTeam = rs[2], homeAwayOdds = homeAwayOdds)
    }

    #Stanley Cup Final
    if('series15' %in% completedSeries$Series){
      series15 <- completedSeries[completedSeries$Series == 'series15', ]$Winner
    } else {
      rs<-reseedTwoTeams(series13, series14, summary_results)
      series15 <- single_series_solver(series_number = 15, currentSeries = currentSeries, homeTeam = rs[1], awayTeam = rs[2], homeAwayOdds = homeAwayOdds)
    }
    srvec<-c(srvec, sim, l1, l2, l3, l4, l5, l6, l7, l8, series1, series2, series3, series4, series5, series6, series7, series8, series9, series10, series11, series12, series13, series14, series15)
  }
  srdf<-as.data.frame(matrix(srvec, ncol=24, byrow=TRUE))
  names(srdf)<-names(simresults)
  simresults<-dplyr::as_tibble(srdf)
  return(simresults)
}

getAllHomeAwayOdds<-function(teamlist, params=NULL){
  params<-parse_dc_params(params)
  homeAwayOdds<-expand.grid("HomeTeam" = teamlist, "AwayTeam" = teamlist, stringsAsFactors = FALSE)
  homeAwayOdds<-homeAwayOdds[homeAwayOdds$HomeTeam != homeAwayOdds$AwayTeam,]
  homeAwayOdds$HomeOdds <- apply(homeAwayOdds, 1, function(x) playoffWin(x[1], x[2], params=params))
  return(homeAwayOdds)
}


#' Record Today's Predictions
#'
#' @description Record today's predictions to file (for easy later retrieval). Run \code{cleanupPredictionsFile} periodically to tidy
#'
#' @param today Day's predictions to record. Defaults to today, but can set any other day
#' @param file csv file location to store predictions. Will append to file.
#' @param schedule HockeyModel::schedule or supplied. \code{today} date must be in schedule
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return NULL
#' @export
recordTodaysPredictions<-function(today=Sys.Date(), file="./data-raw/dailyodds.csv", schedule=HockeyModel::schedule, params=NULL){
  params<-parse_dc_params(params)
  stopifnot(is.Date(today))
  today<-as.Date(today)
  today_sched<-schedule[schedule$Date == today,]
  if(nrow(today_sched) == 0){
    stop("No games on date:", today)
  }
  today_preds<-todayDC(today = today, params=params, schedule = schedule)
  preds<-dplyr::full_join(today_sched, today_preds, suffix=c("",""), by = c("HomeTeam", "AwayTeam"))
  preds<-preds[,c("Date", "GameID", "HomeTeam", "AwayTeam", "HomeWin", "AwayWin", "Draw")]

  utils::write.table(preds, file="./data-raw/dailyodds.csv", append = TRUE, col.names = FALSE, row.names = FALSE, sep=",", dec=".")
}

#' Cleanup Predictions File
#'
#' @description Sometimes the predictions file may end up with game duplicates (last minute postponements, etc,) This deduplicates, taking only the latest instance of a prediction (Games are unique by GameID)
#'
#' @param file file path to cleanup
#'
#' @return NULL
#' @export
cleanupPredictionsFile<-function(file="./data-raw/dailyodds.csv"){
  dailyodds<-utils::read.csv(file)
  dailyodds<-dailyodds %>%
    dplyr::mutate("Date" = as.Date(.data$Date)) %>%
    dplyr::arrange(dplyr::desc(.data$Date)) %>%
    dplyr::distinct(.data$GameID, .keep_all=TRUE) %>%
    dplyr::arrange(.data$Date, .data$GameID)
  utils::write.table(dailyodds, file=file, append = FALSE, col.names = TRUE, row.names = FALSE, sep=",", dec=".")
}

build_past_predictions<-function(startDate, endDate, file="./data-raw/dailyodds.csv"){
  stopifnot(is.Date(startDate))
  stopifnot(is.Date(endDate))
  startDate<-as.Date(startDate)
  endDate<-as.Date(endDate)
  scores<-HockeyModel::scores
  schedule<-HockeyModel::schedule

  for(day in seq.Date(startDate, endDate, by=1)){
    d<-as.Date(day, origin="1970-01-01")
    if(nrow(schedule[schedule$Date == d,]) == 0){
      next  # no games that day, just skip it.
    }
    message('Results as of: ', d)
    score<-scores[scores$Date < day,]
    score<-score[score$Date > (as.Date(startDate) - 4000),]  # only feed in ~ 11 years data to calculate m & rho
    sched<-schedule[schedule$Date == d,]
    params<-list()
    params$m<-getM(scores = score, currentDate = d)
    params$rho<-getRho(m = params$m, scores = score)
    w.day<-getWeibullParams(m = params$m, rho=params$rho, scores = score)
    params$beta<-w.day$beta
    params$eta<-w.day$eta
    params$k<-w.day$k

    recordTodaysPredictions(today=d, file=file, schedule = sched, params = params)
  }
  return(TRUE)
}
