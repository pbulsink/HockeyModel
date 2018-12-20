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

  return(tibble::as.tibble(team_stats))
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
    season_sofar<-scores[scores$Date > as.Date("2018-08-01"),]
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

#' Plot Predicted Points
#'
#' @param all_predictions the compiled predictions
#' @param past_days number of past days to include on the plot. Default: a fortnight
#' @param teamColours HockeyModel::teamColours or a custom value
#'
#' @return a ggplot object
#' @export
plot_prediction_points_by_team<-function(all_predictions = compile_predictions(), past_days = 14, teamColours = HockeyModel::teamColours){
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
  teamColoursList<-as.vector(teamColours$Hex)
  names(teamColoursList)<-teamColours$Team
  teamColoursList<-teamColoursList[names(teamColoursList) %in% teams]

  #make plot
  p<-ggplot2::ggplot(data=all_predictions, ggplot2::aes_(x=quote(predictionDate), y=quote(meanPoints), colour = quote(Team))) +
    ggalt::geom_xspline(spline_shape = 0.5) +
    ggplot2::facet_wrap( ~ facet, ncol = length(unique(all_predictions$Division))) +
    ggplot2::scale_x_date(expand = ggplot2::expand_scale(mult = c(0,.33))) +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Points") +
    ggplot2::ggtitle(paste0("Predicted Points Over the Past ", past_days, " Days"), subtitle = "Chart by @BulsinkB") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggrepel::geom_label_repel(ggplot2::aes_(label = quote(label)),direction = 'y', na.rm = TRUE, segment.alpha = 0, hjust = 0.5, xlim = c(lastdate, NA))

  return(p)
}

#' Plot Playoff Odds
#'
#' @param all_predictions the compiled predictions
#' @param past_days number of past days to include on the plot. Default: a fortnight
#' @param teamColours HockeyModel::teamColours or a custom value
#'
#' @return a ggplot object
#' @export
plot_prediction_playoffs_by_team <- function(all_predictions = compile_predictions(), past_days = 14, teamColours = HockeyModel::teamColours){
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
  teamColoursList<-as.vector(teamColours$Hex)
  names(teamColoursList)<-teamColours$Team
  teamColoursList<-teamColoursList[names(teamColoursList) %in% teams]

  #make plot
  p<-ggplot2::ggplot(data=all_predictions, ggplot2::aes_(x=quote(predictionDate), y=quote(Playoffs), colour = quote(Team))) +
    ggalt::geom_xspline(spline_shape = 0.5) +
    #ggplot2::geom_line() +
    ggplot2::facet_wrap( ~ facet, ncol = length(unique(all_predictions$Division))) +
    ggplot2::scale_x_date(expand = ggplot2::expand_scale(mult = c(0,.33))) +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("Plaoff Odds") +
    ggplot2::ggtitle(paste0("Playoff Odds Over the Past ", past_days, " Days"), subtitle = "Chart by @BulsinkB") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggrepel::geom_label_repel(ggplot2::aes_(label = quote(label)), direction = 'y', na.rm = TRUE, segment.alpha = 0, hjust = 0.5, xlim = c(lastdate, NA))

  return(p)
}

#' Plot President's Trophey Odds
#'
#' @param all_predictions the compiled predictions
#' @param past_days number of past days to include on the plot. Default: a fortnight
#' @param minimum Minimum chance at pres trophy to plot. Cleans up significantly.
#' @param teamColours HockeyModel::teamColours or a custom value
#'
#' @return a ggplot object
#' @export
plot_prediction_presidents_by_team <- function(all_predictions = compile_predictions(), past_days = 14, minimum = 0.01, teamColours = HockeyModel::teamColours){
  #Trim predictions to fit plot
  all_predictions$predictionDate<-as.Date(all_predictions$predictionDate)
  lastdate <- max(all_predictions$predictionDate)
  firstdate <- lastdate - past_days
  all_predictions<-all_predictions[all_predictions$predictionDate >= firstdate,]

  rankedTeams<-unname(unlist(all_predictions[(all_predictions$predictionDate == lastdate & all_predictions$Presidents > minimum), "Team"]))

  all_predictions<-all_predictions[all_predictions$Team %in% rankedTeams, ]

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
  teamColoursList<-as.vector(teamColours$Hex)
  names(teamColoursList)<-teamColours$Team
  teamColoursList<-teamColoursList[names(teamColoursList) %in% teams]

  #make plot
  p<-ggplot2::ggplot(data=all_predictions, ggplot2::aes_(x=quote(predictionDate), y=quote(Presidents), colour = quote(Team))) +
    ggalt::geom_xspline(spline_shape = 0.5) +
    ggplot2::facet_wrap( ~ facet, ncol = length(unique(all_predictions$Division))) +
    ggplot2::scale_x_date(expand = ggplot2::expand_scale(mult = c(0,.33))) +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::xlab("Date") +
    ggplot2::ylab("President's Trophy Odds") +
    ggplot2::ggtitle(paste0("President's Trophy Odds Over the Past ", past_days, " Days"), subtitle = "Chart by @BulsinkB") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggrepel::geom_label_repel(ggplot2::aes_(label = quote(label)), direction = 'y', na.rm = TRUE, segment.alpha = 0, hjust = 0.5, xlim = c(lastdate, NA))


  return(p)
}


#' Plot Pace by team
#'
#' @param graphic_dir The graphics directory
#' @param subdir The pace subdirectory in graphics
#' @param prediction_dir The predictions directory
#' @param teamColours HockeyModel::teamColours or a custom value
#' @param scores The HockeyModel::scores object, or custom scores in the same format.
#'
#' @export
plot_pace_by_team<-function(graphic_dir = './prediction_results/graphics', subdir = 'pace', prediction_dir = "./prediction_results", scores=HockeyModel::scores, teamColours = HockeyModel::teamColours){
  sc<-scores[scores$Date > as.Date("2018-10-01"),]

  teamlist<-unique(c(as.character(sc$HomeTeam), as.character(sc$AwayTeam)))

  #Get old and most recent predictions
  p<-readRDS(file.path(prediction_dir, "2018-10-03-predictions.RDS"))

  filelist<-list.files(path = prediction_dir)
  pdates<-substr(filelist, 1, 10)  # gets the dates list of prediction
  pdates<-pdates[pdates != 'graphics']
  lastp<-as.Date(max(pdates))
  q<-readRDS(file.path(prediction_dir, paste0(lastp,"-predictions.RDS")))

  if(!dir.exists(file.path(graphic_dir, subdir))){
    dir.create(file.path(graphic_dir, subdir), recursive = TRUE)
  }

  for (team in teamlist){
    colour = teamColours[teamColours$Team == team, 'Hex']
    teamscores<-sc[sc$HomeTeam == team | sc$AwayTeam == team, c('AwayTeam','HomeTeam','Result')]
    teamscores[teamscores$AwayTeam == team, 'Result'] <- 1-teamscores[teamscores$AwayTeam == team, 'Result']
    teamscores$Venue<-'Home'
    teamscores[teamscores$AwayTeam == team, 'Venue'] <- 'Away'
    teamscores$Points<-ceiling(2*teamscores$Result)
    teamscores$cPoints<-cumsum(teamscores$Points)
    teamscores$GameNum<-1:nrow(teamscores)
    ngames<-nrow(teamscores)
    cp<-utils::tail(teamscores$cPoints, 1)

    pteam<-p[p$Team == team, ]
    ppoints<-pteam$meanPoints
    maxp<-pteam$meanPoints + 2*(pteam$sdPoints)
    minp<-pteam$meanPoints - 2*(pteam$sdPoints)
    qteam<-q[q$Team == team, ]
    qpoints<-qteam$meanPoints
    maxq<-qteam$meanPoints + 2*(qteam$sdPoints)
    minq<-qteam$meanPoints + 2*(qteam$sdPoints)

    plt <- ggplot2::ggplot(teamscores, ggplot2::aes_(x = quote(GameNum), y = quote(cPoints), colour = quote(Venue))) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(limits = c(0, 82)) +
      ggplot2::scale_y_continuous(limits = c(0, 164)) +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle('Points Pace', subtitle = paste0(team, ' Expected Points: ', format(round(qpoints, 1), nsmall = 1), "\nChart by @BulsinkB"))+
      ggplot2::ylab('Points') +
      ggplot2::xlab('Game Number') +
      ggplot2::geom_segment(x = 0, y = 0, xend = 82, yend = ppoints, alpha = 0.05, colour = 'grey')+
      ggplot2::geom_segment(x = 0, y = 0, xend = 82, yend = maxp, alpha = 0.05, colour = 'grey')+
      ggplot2::geom_segment(x = 0, y = 0, xend = 82, yend = minp, alpha = 0.05, colour = 'grey')+
      ggplot2::geom_segment(x = ngames, y = cp, xend = 82, yend = qpoints, alpha = 0.2, colour = colour)+
      ggplot2::geom_segment(x = ngames, y = cp, xend = 82, yend = maxq, alpha = 0.2, colour = colour)+
      ggplot2::geom_segment(x = ngames, y = cp, xend = 82, yend = minq, alpha = 0.2, colour = colour)

    grDevices::png(filename = file.path(graphic_dir, subdir, paste0(tolower(gsub(" ", "_", team)), '.png')), width = 11, height = 8.5, units = 'in', res = 300)
    print(plt)
    while(grDevices::dev.cur()!=1){
      grDevices::dev.off()
    }
  }
}


#' Plot Today's Odds
#'
#' @param today The day's odds to plot. Default today.
#' @param rho HockeyModel::rho or a custom value
#' @param m HockeyModel::m or a custom value
#' @param schedule HockeyModel::schedule or a custom value
#' @param teamColours HockeyModel::teamColours or a custom value
#' @param ... additional parameters to pass
#'
#' @return a ggplot image of odds
#'
#' @export
plot_odds_today <- function(today = Sys.Date(), rho=HockeyModel::rho, m = HockeyModel::m, schedule = HockeyModel::schedule, teamColours=HockeyModel::teamColours, ...) {
  todayodds<-todayDC(today = today, rho = rho, m = m, schedule = schedule)

  #add odds for each team in OT/SO
  todayodds$HomeWinOT<-(todayodds$HomeWin / (todayodds$HomeWin + todayodds$AwayWin)) * todayodds$Draw
  todayodds$AwayWinOT<-todayodds$Draw-todayodds$HomeWinOT

  #Melt data to work with ggplot
  melted<-reshape2::melt(todayodds, id.vars = c('HomeTeam', 'AwayTeam'))
  melted$variable<-factor(x = melted$variable, levels = c("AwayWin", "AwayWinOT", "Draw", "HomeWinOT", "HomeWin"), ordered = TRUE)

  #Make colour and alpha lists for plot
  plotcolors<-c()
  plotalpha<-c()
  for(i in 1:nrow(todayodds)){
    plotcolors<-c(plotcolors,
                  teamColours[teamColours$Team == todayodds[i, 'HomeTeam'], 'Hex'],
                  teamColours[teamColours$Team == todayodds[i, 'HomeTeam'], 'Hex'],
                  teamColours[teamColours$Team == todayodds[i, 'AwayTeam'], 'Hex'],
                  teamColours[teamColours$Team == todayodds[i, 'AwayTeam'], 'Hex'])
    plotalpha <- c(plotalpha, 1, 0.7, 0.7, 1)
  }

  #Prepare instructions to read
  text_home <- grid::textGrob("Home Win", gp = grid::gpar(fontsize = 10), hjust = 0)
  text_away <- grid::textGrob("Away Win", gp = grid::gpar(fontsize = 10), hjust = 1)
  otlabel.y <- todayodds[nrow(todayodds), "HomeWin"] + todayodds[nrow(todayodds), "Draw"]/2
  text_ot <- grid::textGrob("OT/SO Decision", gp = grid::gpar(fontsize = 10), hjust = 0.5)

  #build plot
  p<-ggplot2::ggplot(melted[melted$variable %in% c('HomeWin','HomeWinOT', 'AwayWinOT', 'AwayWin'),],
                     ggplot2::aes_(y = quote(value), x = quote(HomeTeam), group = quote(variable))) +
    ggplot2::geom_bar(stat = "identity", position='fill', fill = plotcolors, alpha = plotalpha, colour = 'white') +
   #ggplot2::scale_y_continuous(fill = plotcolors, alpha = plotalpha) +
    ggplot2::xlab("") +
    ggplot2::ylab("Result Odds") +
    ggplot2::ggtitle(paste0("Predictions for Games ", Sys.Date()), subtitle = "Chart by @BulsinkB") +
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
    ggplot2::annotate("label", x = todayodds$HomeTeam, y = 0.01, hjust = 0, label = format(round(todayodds$HomeWin, 3), nsmall = 3)) +
    ggplot2::annotate("label", x = todayodds$HomeTeam, y= .99, hjust = 1, label = format(round(todayodds$AwayWin, 3), nsmall = 3)) +
    ggplot2::annotate("label", x = todayodds$HomeTeam, y=todayodds$HomeWin + todayodds$HomeWinOT - 0.01, hjust = 1, label = format(round(todayodds$HomeWinOT, 3), nsmall = 3)) +
    ggplot2::annotate("label", x = todayodds$HomeTeam, y=todayodds$HomeWin + todayodds$HomeWinOT + 0.02, hjust = 0, label = format(round(todayodds$AwayWinOT, 3), nsmall = 3)) +
    ggplot2::coord_flip()

  #Turn off clipping so the instructions can show
  #gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  #gt$layout$clip[gt$layout$name == "panel"] <- "off"
  #grid::grid.draw(gt)

  return(p)

}


#' Plot single game expected goals
#'
#' @param home The Home Team
#' @param away The AWay Team
#' @param m the DC m model
#' @param rho the DC rho value
#' @param maxgoal the max number of goals to predict. Plot a few less.
#' @param teamColours HockeyModel::teamColours or a custom value
#'
#' @return a ggplot object
#' @export
plot_game<-function(home, away, m=HockeyModel::m, rho = HockeyModel::rho, maxgoal = 10, teamColours = HockeyModel::teamColours){
  # Expected goals home
  lambda <- try(stats::predict(m, data.frame(Home = 1, Team = home, Opponent = away), type = "response"), TRUE)

  # Expected goals away
  mu<-try(stats::predict(m, data.frame(Home = 0, Team = away, Opponent = home), type = "response"), TRUE)

  #fix errors
  if(!is.numeric(lambda)){
    lambda<-DCPredictErrorRecover(team = home, opponent = away, homeiceadv = TRUE)
  }
  if(!is.numeric(mu)){
    mu<-DCPredictErrorRecover(team = away, opponent = home, homeiceadv = FALSE)
  }

  #goal prediction method by poisson & d/c handling of low scores:
  probability_matrix <- stats::dpois(0:maxgoal, lambda) %*% t(stats::dpois(0:maxgoal, mu))

  scaling_matrix <- matrix(tau(c(0, 1, 0, 1), c(0, 0, 1, 1), lambda, mu, rho), nrow = 2)
  probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix

  goals<-data.frame(Goals = c(0:maxgoal), Home = 0, Away = 0)

  goals$Away<-colSums(probability_matrix)*1/sum(colSums(probability_matrix))
  goals$Home<-rowSums(probability_matrix)*1/sum(rowSums(probability_matrix))

  goals<-reshape2::melt(goals, id = "Goals", variable.name = "Team", value.name = "Density")

  plotcolors<-c(teamColours[teamColours$Team == home, "Hex"],
                teamColours[teamColours$Team == away, "Hex"])

  home_hjust<-1-(mu>lambda)

  odds<-DCPredict(home = home, away = away)

  p <- ggplot2::ggplot(data = goals, ggplot2::aes_(x = quote(Goals), y = quote(Density), fill = quote(Team))) +
    #ggplot2::geom_point() +
    ggplot2::geom_area(position = "identity", alpha = 0.6) +
    ggplot2::theme_minimal() +
    ggplot2::geom_vline(xintercept = mu,linetype="dashed") +
    ggplot2::geom_vline(xintercept = lambda,linetype="dashed") +
    ggplot2::scale_x_continuous(limits = c(0, 8)) +
    ggplot2::scale_fill_manual(labels = c(home, away), values = plotcolors)+
    ggplot2::scale_color_manual(labels = c(home, away), values = plotcolors)+
    ggplot2::annotate(geom = 'label', x = mu, y = 0.0, label = paste0(away, "\nPredicted Goals:", format(round(mu, 2), nsmall = 2)), hjust = home_hjust, vjust = 0) +
    ggplot2::annotate(geom = 'label', x = lambda, y = 0.0, label = paste0(home, "\nPredicted Goals:",format(round(lambda, 2), nsmall = 2)), hjust = 1-home_hjust, vjust = 0) +
    ggplot2::xlab('Predicted Team Goals') +
    ggplot2::ylab('Odds') +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle("Predicted Goals", subtitle = paste0(away, " at ", home, " on ", Sys.Date(),"\nWin Odds - Away: ", format(round(odds[[3]], 3), nsmall = 3), " - Home: ", format(round(odds[[1]], 3), nsmall = 3), " - OT/SO: ", format(round(odds[[2]], 3), nsmall = 3)))

  return(p)
}

#' 'Loopless' simulation
#'
#' @param nsims number of simulations to run (approximate)
#' @param cores number of cores in parallel to process
#' @param schedule games to play
#' @param scores Season to this point
#' @param rho rho factor to pass. default HockeyModel::rho
#' @param m m model to pass. default HockeyModel::m
#'
#' @return a two member list, of all results and summary results
#' @export
loopless_sim<-function(nsims=1e5, cores = parallel::detectCores() - 1, schedule = HockeyModel::schedule, scores=HockeyModel::scores, rho = HockeyModel::rho, m = HockeyModel::m){

  nsims <- floor(nsims/cores)

  odds_table<-remainderSeasonDC(scores = scores, schedule = schedule, odds = TRUE, rho = rho, m = m)
  odds_table$Result <- NA

  season_sofar<-scores[scores$Date > as.Date("2018-08-01"),]

  last_scores_date<-season_sofar[nrow(season_sofar), 'Date']
  odds_table<-odds_table[odds_table$Date > last_scores_date, ]

  season_sofar<-season_sofar[, c('Date', 'HomeTeam','AwayTeam','Result')]
  season_sofar$HomeWin <- season_sofar$AwayWin <- season_sofar$Draw <- NA

  all_season<-rbind(season_sofar, odds_table)

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
      sdDivRank = stats::sd(!!dplyr::sym('DivRank'), na.rm = TRUE)
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
    mutate_cond(!!dplyr::sym('Playoffs') == 0, Wildcard = rank(!!dplyr::sym('ConfRank'))) %>%
    dplyr::ungroup() %>%
    mutate_cond(!!dplyr::sym('Playoffs') == 0 & !!dplyr::sym('Wildcard') <= 2, Playoffs = 1) %>%
    dplyr::select(!!dplyr::sym('SimNo'), !!dplyr::sym('Team'), !!dplyr::sym('W'), !!dplyr::sym('OTW'),
                  !!dplyr::sym('SOW'), !!dplyr::sym('SOL'), !!dplyr::sym('OTL'), !!dplyr::sym('Points'),
                  !!dplyr::sym('Rank'), !!dplyr::sym('ConfRank'), !!dplyr::sym('DivRank'), !!dplyr::sym('Playoffs'))


  return(all_results)
}


