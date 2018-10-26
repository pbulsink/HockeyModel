#league stuff: tables, standings, stats, etc.

#' Bulid Stats Table
#'
#' @param scores scores to use to build stats table
#'
#' @return a stats table (as tibble)
#' @export
buildStats<-function(scores = HockeyModel::scores){
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
      P = !!dplyr::sym('W')*2 + !!dplyr::sym('OTW')*2 + !!dplyr::sym('SOW')*2 + !!dplyr::sym('OTL') + !!dplyr::sym('SOL')
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
      P = !!dplyr::sym('W')*2 + !!dplyr::sym('OTW')*2 + !!dplyr::sym('SOW')*2 + !!dplyr::sym('OTL') + !!dplyr::sym('SOL')
      ) %>%
    dplyr::ungroup()

  team_stats<-tibble::tibble(
    Team=teamlist,
    GP = tmp1$GP + tmp2$GP,
    Points = tmp1$P + tmp2$P,
    W = tmp1$W + tmp2$W,
    L = tmp1$L + tmp2$L,
    OTL = tmp1$OTL + tmp2$OTL,
    OTW = tmp1$OTW + tmp2$OTW,
    SOL = tmp1$SOL + tmp2$SOL,
    SOW = tmp1$SOW + tmp2$SOW
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
  for(i in 1:16){
    if(sum(team_stats[team_stats$Team %in% nhl_conferences$East, 'Playoffs'])<8){
      team_stats[(team_stats$Team %in% nhl_conferences$East & team_stats$ConfRank == i), 'Playoffs'] <- 1
    } else {
      #Enough playoff teams
      break
    }
  }
  for(i in 1:16){
    if(sum(team_stats[team_stats$Team %in% nhl_conferences$West, 'Playoffs'])<8){
      team_stats[(team_stats$Team %in% nhl_conferences$West & team_stats$ConfRank == i), 'Playoffs'] <- 1
    } else {
      #Enough playoff teams
      break
    }
  }

  return(team_stats)
}


#' Simulate the remainder of the season
#'
#' @param odds_table A dataframe with HomeTeam, AwayTeam, HomeWin, AwayWin, Draw, and Date
#' @param scores Past (historical) season scores. Defaults to HockeyModel::Scores
#' @param schedule Future unplayed games. Defaults to HockeyModel::schedule
#' @param nsims number of simulations to run
#'
#' @return a data frame of results
#' @export
simulateSeason <- function(odds_table, scores=HockeyModel::scores, nsims=10000, schedule=HockeyModel::schedule){
  teamlist<-c()
  if(!is.null(scores)){
    season_sofar<-scores[scores$Date > as.Date("2018-08-01"),]

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

  pb<-dplyr::progress_estimated(nsims)
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

    pb$tick()$print()
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
#'
#' @return a data frame of results
#' @export
simulateSeasonParallel <- function(odds_table, scores=HockeyModel::scores, nsims=10000, schedule=HockeyModel::schedule, cores = parallel::detectCores() - 1){
  teamlist<-c()
  if(!is.null(scores)){
    season_sofar<-scores[scores$Date > as.Date("2018-08-01"),]

    season_sofar <- season_sofar[,c('Date','HomeTeam','AwayTeam','Result')]

    teamlist<-c(teamlist, sort(unique(c(as.character(season_sofar$HomeTeam), as.character(season_sofar$AwayTeam)))))
  } else {
    season_sofar<-NULL
  }

  teamlist<-c(teamlist, sort(unique(c(as.character(schedule$Home), as.character(schedule$Away)))))

  if(!is.finite(cores)) cores <- 2

  n<-length(teamlist)

  # all_results <- data.frame(Team = rep(teamlist, nsims),
  #                           SimNo = rep(1:nsims, each = n),
  #                           Points = rep(NA, n * nsims),
  #                           W = rep(NA, n*nsims),
  #                           L = rep(NA, n * nsims),
  #                           OTW = rep(NA, n * nsims),
  #                           SOW = rep(NA, n * nsims),
  #                           OTL = rep(NA, n * nsims),
  #                           SOL = rep(NA, n * nsims),
  #                           Rank = rep(NA, n * nsims),
  #                           ConfRank = rep(NA, n * nsims),
  #                           DivRank = rep(NA, n * nsims),
  #                           Playoffs = rep(NA, n * nsims),
  #                           stringsAsFactors = FALSE)

  `%dopar%` <- foreach::`%dopar%`
  cl<-parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)
  pb<-utils::txtProgressBar(max = nsims, style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
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
  close(pb)
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
  all_predictions<-lapply(pdates, function(f) readRDS(file.path(dir, (paste0(f, "-predictions.RDS"))))) #Read all the files
  names(all_predictions)<-pdates
  all_predictions<-dplyr::bind_rows(all_predictions, .id="predictionDate")
  return(all_predictions)
}

#' Plot Predicted Points
#'
#' @param all_predictions the compiled predictions
#' @param past_days number of past days to include on the plot. Default: a fortnight
#'
#' @return a ggplot object
#' @export
plot_prediction_points_by_team<-function(all_predictions = compile_predictions(), past_days = 14){
  #Trim predictions to fit plot
  all_predictions$predictionDate<-as.Date(all_predictions$predictionDate)
  lastdate <- max(all_predictions$predictionDate)
  firstdate <- lastdate - past_days
  all_predictions<-all_predictions[all_predictions$predictionDate >= firstdate,]

  #extract constants
  teams<-unique(all_predictions$Team)
  dates<-as.Date(unique(all_predictions$predictionDate))
  #Get division
  all_predictions$Division<-getDivision(all_predictions$Team)
  #Set divisions to logical order
  all_predictions$facet <- factor(x = all_predictions$Division, levels = c("Pacific", "Central", "Metropolitan", "Atlantic"))
  #make team label appear properly later with ggrepel
  all_predictions$label <- ifelse(all_predictions$predictionDate == max(all_predictions$predictionDate),
                                   as.character(paste0(getShortTeam(all_predictions$Team), '\n', round(all_predictions$meanPoints, digits = 0))),
                                   NA_character_)

  #Build and trim team colours for plot
  teamColoursList<-as.vector(teamColours$Hex[teamColours$Code == "Primary"])
  names(teamColoursList)<-teamColours$Team[teamColours$Code == "Primary"]
  teamColoursList<-teamColoursList[names(teamColoursList) %in% teams]

  #make plot
  p<-ggplot2::ggplot(data=all_predictions, ggplot2::aes(x=predictionDate, y=meanPoints, colour = Team)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap( ~ facet, ncol = length(unique(all_predictions$Division))) +
    ggplot2::scale_x_date(expand = ggplot2::expand_scale(mult = c(0,.33))) +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Points") +
    ggplot2::ggtitle(paste0("Predicted Points Over the Past ", past_days, " Days")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggrepel::geom_label_repel(ggplot2::aes(label = label),direction = 'y', na.rm = TRUE, segment.alpha = 0, hjust = 0.5, xlim = c(lastdate, NA))

  return(p)
}

#' Plot Playoff Odds
#'
#' @param all_predictions the compiled predictions
#' @param past_days number of past days to include on the plot. Default: a fortnight
#'
#' @return a ggplot object
#' @export
plot_prediction_playoffs_by_team <- function(all_predictions = compile_predictions(), past_days = 14){
  #Trim predictions to fit plot
  all_predictions$predictionDate<-as.Date(all_predictions$predictionDate)
  lastdate <- max(all_predictions$predictionDate)
  firstdate <- lastdate - past_days
  all_predictions<-all_predictions[all_predictions$predictionDate >= firstdate,]

  #extract constants
  teams<-unique(all_predictions$Team)
  dates<-as.Date(unique(all_predictions$predictionDate))
  #Get division
  all_predictions$Division<-getDivision(all_predictions$Team)
  #Set divisions to logical order
  all_predictions$facet <- factor(x = all_predictions$Division, levels = c("Pacific", "Central", "Metropolitan", "Atlantic"))
  #make team label appear properly later with ggrepel
  all_predictions$label <- ifelse(all_predictions$predictionDate == max(all_predictions$predictionDate),
                                  as.character(paste0(getShortTeam(all_predictions$Team), '\n', signif(all_predictions$Playoffs*100, digits = 2), '%')),
                                  NA_character_)

  #Build and trim team colours for plot
  teamColoursList<-as.vector(teamColours$Hex[teamColours$Code == "Primary"])
  names(teamColoursList)<-teamColours$Team[teamColours$Code == "Primary"]
  teamColoursList<-teamColoursList[names(teamColoursList) %in% teams]

  #make plot
  p<-ggplot2::ggplot(data=all_predictions, ggplot2::aes(x=predictionDate, y=Playoffs, colour = Team)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap( ~ facet, ncol = length(unique(all_predictions$Division))) +
    ggplot2::scale_x_date(expand = ggplot2::expand_scale(mult = c(0,.33))) +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Points") +
    ggplot2::ggtitle(paste0("Playoff Odds Over the Past ", past_days, " Days")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggrepel::geom_label_repel(ggplot2::aes(label = label),direction = 'y', na.rm = TRUE, segment.alpha = 0, hjust = 0.5, xlim = c(lastdate, NA))

  return(p)
}

#' Plot President's Trophey Odds
#'
#' @param all_predictions the compiled predictions
#' @param past_days number of past days to include on the plot. Default: a fortnight
#'
#' @return a ggplot object
#' @export
plot_prediction_presidents_by_team <- function(all_predictions = compile_predictions(), past_days = 14){
  #Trim predictions to fit plot
  all_predictions$predictionDate<-as.Date(all_predictions$predictionDate)
  lastdate <- max(all_predictions$predictionDate)
  firstdate <- lastdate - past_days
  all_predictions<-all_predictions[all_predictions$predictionDate >= firstdate,]

  #extract constants
  teams<-unique(all_predictions$Team)
  dates<-as.Date(unique(all_predictions$predictionDate))
  #Get division
  all_predictions$Division<-getDivision(all_predictions$Team)
  #Set divisions to logical order
  all_predictions$facet <- factor(x = all_predictions$Division, levels = c("Pacific", "Central", "Metropolitan", "Atlantic"))
  #make team label appear properly later with ggrepel
  all_predictions$label <- ifelse(all_predictions$predictionDate == max(all_predictions$predictionDate),
                                  as.character(paste0(getShortTeam(all_predictions$Team), '\n', signif(all_predictions$Presidents*100, digits = 2), '%')),
                                  NA_character_)

  #Build and trim team colours for plot
  teamColoursList<-as.vector(teamColours$Hex[teamColours$Code == "Primary"])
  names(teamColoursList)<-teamColours$Team[teamColours$Code == "Primary"]
  teamColoursList<-teamColoursList[names(teamColoursList) %in% teams]

  #make plot
  p<-ggplot2::ggplot(data=all_predictions, ggplot2::aes(x=predictionDate, y=Presidents, colour = Team)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap( ~ facet, ncol = length(unique(all_predictions$Division))) +
    ggplot2::scale_x_date(expand = ggplot2::expand_scale(mult = c(0,.33))) +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Points") +
    ggplot2::ggtitle(paste0("President's Trophy Odds Over the Past ", past_days, " Days")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggrepel::geom_label_repel(ggplot2::aes(label = label),direction = 'y', na.rm = TRUE, segment.alpha = 0, hjust = 0.5, xlim = c(lastdate, NA))


  return(p)
}


#' Plot Pace by team
#'
#' @param team Specific team(s) to plot. Default = all
#' @param scores the historical scores.
#' @param season choose a specific season. Default NULL is most recent (current)
#'
#' @return a ggplot object
#' @export
plot_pace_by_team <- function(team = 'all', scores = HockeyModel::scores, season = NULL) {
  ##TODO: pace charts
}

#' Plot Today's Odds
#'
#' @param today The day's odds to plot
#' @param ... additional parameters to pass
#'
#' @return a ggplot image of odds
#'
#' @export
plot_odds_today <- function(today = Sys.Date(), ...) {
  todayodds<-todayDC(today = today, ...)

  #add odds for each team in OT/SO
  todayodds$HomeWinOT<-(todayodds$HomeWin / (todayodds$HomeWin + todayodds$AwayWin)) * todayodds$Draw
  todayodds$AwayWinOT<-todayodds$Draw-todayodds$HomeWinOT

  #Melt data to work with ggplot
  m<-reshape2::melt(todayodds, id.vars = c('HomeTeam', 'AwayTeam'))
  m$variable<-factor(x = m$variable, levels = c("AwayWin", "AwayWinOT", "Draw", "HomeWinOT", "HomeWin"), ordered = TRUE)

  #Make colour and alpha lists for plot
  plotcolors<-c()
  plotalpha<-c()
  for(i in 1:nrow(todayodds)){
    plotcolors<-c(plotcolors,
                  teamColours[(teamColours$Team == todayodds[i, 'HomeTeam'] & teamColours$Code == 'Primary'), 'Hex'],
                  teamColours[(teamColours$Team == todayodds[i, 'HomeTeam'] & teamColours$Code == 'Primary'), 'Hex'],
                  teamColours[(teamColours$Team == todayodds[i, 'AwayTeam'] & teamColours$Code == 'Primary'), 'Hex'],
                  teamColours[(teamColours$Team == todayodds[i, 'AwayTeam'] & teamColours$Code == 'Primary'), 'Hex'])
    plotalpha <- c(plotalpha, 0.9, 0.7, 0.6, 0.9)
  }

  #build plot
  p<-ggplot2::ggplot(m[m$variable %in% c('HomeWin','HomeWinOT', 'AwayWinOT', 'AwayWin'),], ggplot2::aes(y = value, x = HomeTeam, group = variable)) +
    ggplot2::geom_bar(stat = "identity", position='fill', fill = plotcolors, alpha = plotalpha, colour = 'white') +
   #ggplot2::scale_y_continuous(fill = plotcolors, alpha = plotalpha) +
    ggplot2::xlab("") +
    ggplot2::ylab("Result Odds") +
    ggplot2::ggtitle(paste0("  Predictions for Games ", Sys.Date(), "\n")) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.border = ggplot2::element_blank(),
                   panel.grid= ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(2,1,1,1), "lines"))+
    ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(add = 0.3),
                                breaks = c(0,0.5,1)) +
    ggplot2::annotate("text", x = todayodds$HomeTeam, y = -.01, hjust = 1, label = todayodds$HomeTeam) +
    ggplot2::annotate("text", x = todayodds$HomeTeam, y = 1.01, hjust = 0, label = todayodds$AwayTeam) +
    ggplot2::annotate("label", x = todayodds$HomeTeam, y = 0.01, hjust = 0, label = round(todayodds$HomeWin, 3)) +
    ggplot2::annotate("label", x = todayodds$HomeTeam, y= .99, hjust = 1, label = round(todayodds$AwayWin, 3)) +
    ggplot2::annotate("label", x = todayodds$HomeTeam, y=todayodds$HomeWin + todayodds$HomeWinOT - 0.01, hjust = 1, label = round(todayodds$HomeWinOT, 3)) +
    ggplot2::annotate("label", x = todayodds$HomeTeam, y=todayodds$HomeWin + todayodds$HomeWinOT + 0.02, hjust = 0, label = round(todayodds$AwayWinOT, 3)) +
    ggplot2::coord_flip()

  #Add instructions to read
  text_home <- grid::textGrob("Home Win", gp = grid::gpar(fontsize = 10), hjust = 0)
  text_away <- grid::textGrob("Away Win", gp = grid::gpar(fontsize = 10), hjust = 1)
  otlabel.y <- todayodds[nrow(todayodds), "HomeWin"] + todayodds[nrow(todayodds), "Draw"]/2
  text_ot <- grid::textGrob("OT/SO Decision", gp = grid::gpar(fontsize = 10), hjust = 0.5)
  p<-p +
    ggplot2::annotation_custom(text_home, xmin=nrow(todayodds) + 0.55, ymin = 0.01, ymax = .01) +
    ggplot2::annotation_custom(text_away, xmin=nrow(todayodds) + 0.55, ymin = 0.99, ymax = .99)  +
    ggplot2::annotation_custom(text_ot, xmin=nrow(todayodds) + 0.55, ymin = otlabel.y, ymax = otlabel.y)

  #Turn off clipping so the instructions can show
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid::grid.draw(gt)

  return(gt)

}
