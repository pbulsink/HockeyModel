#Graphics

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
  all_predictions$Division<-getTeamDivisions(all_predictions$Team)
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
    ggplot2::scale_x_date(expand = ggplot2::expansion(mult = c(0,.33))) +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::labs(x = "Date",
                  y = "Points",
                  title = paste0("Predicted Points Over the Past ", past_days, " Days"),
                  caption = paste0("P. Bulsink (@BulsinkBot) | ", Sys.Date()))+
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  if(requireNamespace('ggrepel', quietly = TRUE)){
    p <- p +
      ggrepel::geom_label_repel(ggplot2::aes_(label = quote(label)),direction = 'y', na.rm = TRUE, segment.alpha = 0, hjust = 0.5, xlim = c(lastdate, NA))
  }

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
  #Get division
  all_predictions$Division<-getTeamDivisions(all_predictions$Team)
  #Set divisions to logical order
  all_predictions$facet <- factor(x = all_predictions$Division, levels = c("Pacific", "Central", "Metropolitan", "Atlantic"))
  #make team label appear properly later with ggrepel
  playoff_odds<-all_predictions[all_predictions$predictionDate == lastdate,]$Playoffs
  label<-format(round(playoff_odds*100, digits = 0), nsmall = 0, trim = TRUE)
  label[label == '100' & playoff_odds > 0.999]<-'~100'
  label[label == '100' & playoff_odds != 1]<-'>99.5'
  label[playoff_odds == 1]<-'100'
  label[label == '0' & playoff_odds < 0.001]<-'~0'
  label[label == '0' & playoff_odds != 0]<-'<0.5'
  label[playoff_odds == 0]<-'0'

  label<-paste0(getShortTeam(all_predictions[all_predictions$predictionDate == lastdate, ]$Team), '\n', label, '%', sep = '')

  all_predictions$label <- NA_character_
  all_predictions[all_predictions$predictionDate == lastdate,]$label <- label

  #Build and trim team colours for plot
  teamColoursList<-as.vector(teamColours$Hex)
  names(teamColoursList)<-teamColours$Team
  teamColoursList<-teamColoursList[names(teamColoursList) %in% teams]

  #make plot
  p<-ggplot2::ggplot(data=all_predictions, ggplot2::aes_(x=quote(predictionDate), y=quote(Playoffs), colour = quote(Team))) +
    ggalt::geom_xspline(spline_shape = 0.5) +
    #ggplot2::geom_line() +
    ggplot2::facet_wrap( ~ facet, ncol = length(unique(all_predictions$Division))) +
    ggplot2::scale_x_date(expand = ggplot2::expansion(mult = c(0,.33))) +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::labs(x = "Date",
                  y = "Playoff Odds",
                  title = paste0("Playoff Odds Over the Past ", past_days, " Days"),
                  caption = paste0("P. Bulsink (@BulsinkBot) | ", Sys.Date()))+
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  if(requireNamespace('ggrepel')){
    p <- p +
      ggrepel::geom_label_repel(ggplot2::aes_(label = quote(label)), direction = 'y', na.rm = TRUE, segment.alpha = 0, hjust = 0.5, xlim = c(lastdate, NA))
  }

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
  all_predictions$Division<-getTeamDivisions(all_predictions$Team)
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
    ggplot2::scale_x_date(expand = ggplot2::expansion(mult = c(0,.33))) +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::labs(x = "Date",
                  y = "President's Trophy Odds",
                  title = paste0("President's Trophy Odds Over the Past ", past_days, " Days"),
                  caption = paste0("P. Bulsink (@BulsinkBot) | ", Sys.Date()),
                  subtitle = paste0("Teams with < ", round(minimum*100, 2),"% odds hidden for simplicity"))+
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  if(requireNamespace("ggrepel", quietly = TRUE)){
    p <- p +
      ggrepel::geom_label_repel(ggplot2::aes_(label = quote(label)), direction = 'y', na.rm = TRUE, segment.alpha = 0, hjust = 0.5, xlim = c(lastdate, NA))
  }

  return(p)
}


#' Plot Pace By Division
#'
#' @param graphic_dir Graphics Directory
#' @param subdir Subdirectory for pace graphics
#' @param prediction_dir Directory for predictions
#' @param scores HockeyModel::scores
#'
#' @return ggplot graphic of team pace vs. predicted
#' @export
plot_pace_by_division<-function(graphic_dir = file.path(devtools::package_file(), "prediction_results","graphics"), subdir = 'pace', prediction_dir = file.path(devtools::package_file(), "prediction_results"), scores=HockeyModel::scores){
  sc<-scores[scores$Date >= as.Date(getSeasonStartDate()),]
  sc<-sc[sc$GameType == "R",]

  #Get old predictions
  p<-readRDS(file.path(prediction_dir, paste0(getSeasonStartDate(), "-predictions.RDS")))

  if(!dir.exists(file.path(graphic_dir, subdir))){
    dir.create(file.path(graphic_dir, subdir), recursive = TRUE)
  }

  ngames<-getNumGames()

  teamlist<-unique(c(as.character(sc$HomeTeam), as.character(sc$AwayTeam)))

  teampoints<-as.list(rep(NA, length(teamlist)))
  names(teampoints)<-teamlist


  teamPerformance<-data.frame("GameNum" = 0:ngames)

  for (team in teamlist){
    teamscores<-sc[sc$HomeTeam == team | sc$AwayTeam == team, c('AwayTeam','HomeTeam','Result', 'GameID')]
    teamscores[teamscores$AwayTeam == team, 'Result'] <- 1-teamscores[teamscores$AwayTeam == team, 'Result']
    teamscores$Venue<-'Home'
    teamscores[teamscores$AwayTeam == team, 'Venue'] <- 'Away'
    teamscores$Points<-ceiling(2*teamscores$Result)
    teamscores$cPoints<-cumsum(teamscores$Points)
    teamscores$GameNum<-1:nrow(teamscores)
    teamscores$xPoints<-teamscores$GameNum*(p[p$Team == team, ]$meanPoints/ngames)
    teamscores$xDiff<-teamscores$cPoints-teamscores$xPoints
    teampoints[[team]]<-max(teamscores$cPoints)
    teamscores<-teamscores[,c("GameNum", "xDiff")]
    teamscores<-tibble::add_row(teamscores, "GameNum" = 0, "xDiff" = 0)
    names(teamscores) <- c("GameNum", team)
    teamPerformance<-dplyr::left_join(teamPerformance, teamscores, by="GameNum")
  }

  games_played <- max(which(rowSums(teamPerformance[,2:ncol(teamPerformance)], na.rm = T) != 0))

  teamPerformance<-teamPerformance[teamPerformance$GameNum <= (games_played + 1),]

  teamPerformance<-tidyr::pivot_longer(teamPerformance,
                                       !.data$GameNum,
                                       names_to = "Team",
                                       values_to = "PointDiff"
                                       )

  teamColours<-HockeyModel::teamColours
  #Build and trim team colours for plot
  teamColoursList<-as.vector(teamColours$Hex)
  names(teamColoursList)<-teamColours$Team
  teamColoursList<-teamColoursList[names(teamColoursList) %in% teamlist]

  teamPerformance$label <- NA_character_


  for(team in teamlist){
    pointdiff<-teamPerformance[teamPerformance$Team == team & teamPerformance$GameNum == max(teamPerformance[teamPerformance$Team == team & !is.na(teamPerformance$PointDiff), "GameNum"]), "PointDiff"]
    lab<-paste0(getShortTeam(team), " - ", teampoints[team], " pts. (", ifelse(pointdiff > 0, "+", ""), round(pointdiff, 1), ")")
    teamPerformance[teamPerformance$Team == team & teamPerformance$GameNum == max(teamPerformance[teamPerformance$Team == team & !is.na(teamPerformance$PointDiff), "GameNum"]), "label"]<-lab
  }

  teamPerformance$Div <- getTeamDivisions(teamPerformance$Team)

  for(division in unique(teamPerformance$Div)){
    #tl<-teamlist[teamlist %in% unlist(HockeyModel::nhl_divisions[division])]
    tp<-teamPerformance[teamPerformance$Div == division, ]

    plt<-ggplot2::ggplot(tp, ggplot2::aes_string(x = "GameNum", y = "PointDiff", colour = "Team"))+
      #ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::geom_smooth(span=0.2, na.rm=TRUE, se = FALSE) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::coord_cartesian(xlim=c(0,max(teamPerformance$GameNum, 12)), clip="off")+
      ggplot2::labs(title = "Points vs. Predicted at Season Start",
                    subtitle = paste(division, "Division Teams"),
                    x = "Game Number",
                    y = "Points Above/Below Predicted",
                    caption = paste0("P. Bulsink (@BulsinkBot) | ", Sys.Date())) +
      ggplot2::scale_colour_manual(values = teamColoursList) +
      ggplot2::scale_x_continuous(breaks = seq(from=0, to=max(teamPerformance$GameNum), by = 5))+#, expand = ggplot2::expansion(mult = c(0, .1)))+
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none",
                     plot.margin = ggplot2::unit(c(1,7,1,1), "lines"))
    if(requireNamespace("ggrepel", quietly = TRUE)){
      plt<-plt +
        ggrepel::geom_label_repel(ggplot2::aes_string(label = "label"), direction = 'y', na.rm = TRUE, segment.alpha = 0, hjust = 0.5, xlim = c(max(teamPerformance$GameNum,12), max(teamPerformance$GameNum,12)+max(teamPerformance$GameNum,12)*.18))
    }

    grDevices::png(filename = file.path(graphic_dir, subdir, paste0(tolower(gsub(" ", "_", division)), '_pace.png')), width = 11, height = 8.5, units = 'in', res = 300)
    print(plt)
    while(grDevices::dev.cur()!=1){
      grDevices::dev.off()
    }
  }
  return(plt)
}


#' Plot Pace by team
#'
#' @param graphic_dir The graphics directory
#' @param subdir The pace subdirectory in graphics
#' @param prediction_dir The predictions directory
#' @param scores The HockeyModel::scores object, or custom scores in the same format.
#'
#' @export
plot_pace_by_team<-function(graphic_dir = file.path(devtools::package_file(), "prediction_results", "graphics"), subdir = 'pace', prediction_dir = file.path(devtools::package_file(), "prediction_results"), scores=HockeyModel::scores){
  sc<-scores[scores$Date > as.Date(getSeasonStartDate()),]

  teamlist<-unique(c(as.character(sc$HomeTeam), as.character(sc$AwayTeam)))

  #Get old and most recent predictions
  p<-readRDS(file.path(prediction_dir, paste0(getSeasonStartDate(), "-predictions.RDS")))

  filelist<-list.files(path = prediction_dir)
  pdates<-substr(filelist, 1, 10)  # gets the dates list of prediction
  pdates<-pdates[pdates != 'graphics']
  lastp<-as.Date(max(pdates))
  q<-readRDS(file.path(prediction_dir, paste0(lastp,"-predictions.RDS")))

  if(!dir.exists(file.path(graphic_dir, subdir))){
    dir.create(file.path(graphic_dir, subdir), recursive = TRUE)
  }
  numgames<-getNumGames()

  teamColours <- HockeyModel::teamColours

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
    minq<-qteam$meanPoints - 2*(qteam$sdPoints)

    plt <- ggplot2::ggplot(teamscores, ggplot2::aes_(x = quote(GameNum), y = quote(cPoints), colour = quote(Venue))) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(limits = c(0, numgames)) +
      ggplot2::scale_y_continuous(limits = c(0, numgames*2)) +
      ggplot2::labs(x = "Game Number",
                    y = "Points",
                    title = "Points Pace",
                    subtitle = paste0(team, ' Expected Points: ', format(round(qpoints, 1), nsmall = 1)),
                    caption = paste0("P. Bulsink (@BulsinkBot) | ", Sys.Date()))+
      ggplot2::theme_minimal() +
      ggplot2::geom_segment(x = 0, y = 0, xend = numgames, yend = ppoints, alpha = 0.05, colour = 'grey')+
      ggplot2::geom_segment(x = 0, y = 0, xend = numgames, yend = maxp, alpha = 0.05, colour = 'grey')+
      ggplot2::geom_segment(x = 0, y = 0, xend = numgames, yend = minp, alpha = 0.05, colour = 'grey')+
      ggplot2::geom_segment(x = ngames, y = cp, xend = numgames, yend = qpoints, alpha = 0.2, colour = colour)+
      ggplot2::geom_segment(x = ngames, y = cp, xend = numgames, yend = maxq, alpha = 0.2, colour = colour)+
      ggplot2::geom_segment(x = ngames, y = cp, xend = numgames, yend = minq, alpha = 0.2, colour = colour)

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
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param schedule HockeyModel::schedule or a custom value
#' @param teamColours HockeyModel::teamColours or a custom value
#'
#' @return a ggplot image of odds
#'
#' @export
plot_odds_today <- function(today = Sys.Date(), params=NULL, schedule = HockeyModel::schedule, teamColours=HockeyModel::teamColours) {
  params<-parse_dc_params(params)
  todayodds<-todayDC(today = today, params, schedule = schedule)
  todayodds$GameID<-NULL
  todayodds$HomeWinOT<-todayodds$AwayWinOT<-0

  #add odds for each team in OT/SO
  for(g in 1:nrow(todayodds)){
    todayodds$HomeWinOT[g] <- extraTimeSolver(home_win = todayodds$HomeWin[g], away_win = todayodds$AwayWin[g], draw = todayodds$Draw[g])[2]
    todayodds$AwayWinOT[g] <- extraTimeSolver(home_win = todayodds$HomeWin[g], away_win = todayodds$AwayWin[g], draw = todayodds$Draw[g])[3]

  }

  #Melt data to work with ggplot
  #melted<-reshape2::melt(todayodds, id.vars = c('HomeTeam', 'AwayTeam'))
  melted<-tidyr::pivot_longer(todayodds, cols = c("HomeWin", "AwayWin", "HomeWinOT", "AwayWinOT", "Draw"),
                              names_to = 'variable', values_to = 'value')
  melted$variable<-factor(x = melted$variable, levels = c("AwayWin", "AwayWinOT", "Draw", "HomeWinOT", "HomeWin"), ordered = TRUE)
  melted<-melted[melted$variable != "Draw",]

  melted$alpha<-1
  melted$colour<-""
  for(i in 1:nrow(melted)){
    melted[i,]$alpha<-ifelse(melted[i,]$variable %in% c("HomeWin", "AwayWin"), yes=1, no=0.7)
    tc <- getTeamColours(home = melted[i, ]$HomeTeam, away=melted[i, ]$AwayTeam)
    melted[i,]$colour<-ifelse(melted[i,]$variable %in% c("HomeWin", "HomeWinOT"),
                              yes=tc$home,
                              no =tc$away)
  }

  #Prepare instructions to read
  text_home <- grid::textGrob("Home Win", gp = grid::gpar(fontsize = 10), hjust = 0)
  text_away <- grid::textGrob("Away Win", gp = grid::gpar(fontsize = 10), hjust = 1)
  otlabel.y <- todayodds[nrow(todayodds), "HomeWin"] + todayodds[nrow(todayodds), "Draw"]/2
  text_ot <- grid::textGrob("OT/SO Decision", gp = grid::gpar(fontsize = 10), hjust = 0.5)

  #build plot
  #p<-ggplot2::ggplot(melted[melted$variable %in% c('HomeWin','HomeWinOT', 'AwayWinOT', 'AwayWin'),],
  p<-ggplot2::ggplot(melted,
                     ggplot2::aes_(y = quote(value), x = quote(HomeTeam), group = quote(variable))) +
    ggplot2::geom_bar(stat = "identity", position='fill', fill = melted$colour, alpha = melted$alpha, colour = 'white') +
    ggplot2::labs(x = "",
                  y = "Result Odds",
                  title = "Predictions for Today's Games",
                  subtitle = paste0("Games played on ", Sys.Date()),
                  caption = paste0("P. Bulsink (@BulsinkBot) | ", Sys.Date()))+
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.border = ggplot2::element_blank(),
                   panel.grid= ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(2,1,1,1), "lines"))+
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.3),
                                breaks = c(0,0.5,1)) +
    ggplot2::annotate("text", x = todayodds$HomeTeam, y = -.01, hjust = 1, label = todayodds$HomeTeam) +
    ggplot2::annotate("text", x = todayodds$HomeTeam, y = 1.01, hjust = 0, label = todayodds$AwayTeam) +
    ggplot2::annotate("label", x = todayodds$HomeTeam, y = 0.01, hjust = 0, label = format(round(todayodds$HomeWin, 3), nsmall = 3)) +
    ggplot2::annotate("label", x = todayodds$HomeTeam, y= .99, hjust = 1, label = format(round(todayodds$AwayWin, 3), nsmall = 3)) +
    ggplot2::annotate("label", x = todayodds$HomeTeam, y=todayodds$HomeWin + todayodds$HomeWinOT - 0.01, hjust = 1, label = format(round(todayodds$HomeWinOT, 3), nsmall = 3)) +
    ggplot2::annotate("label", x = todayodds$HomeTeam, y=todayodds$HomeWin + todayodds$HomeWinOT + 0.01, hjust = 0, label = format(round(todayodds$AwayWinOT, 3), nsmall = 3)) +
    ggplot2::coord_flip()

  return(p)

}


#' Plot Today's Playoff Series Odds
#'
#' @param series A data frame of home team, away team, home wins, away wins
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param teamColours HockeyModel::teamColours or a custom value
#'
#' @return a ggplot image of odds
#'
#' @export
plot_playoff_series_odds <- function(series = getAPISeries(), params=NULL, teamColours=HockeyModel::teamColours) {
  params<-parse_dc_params(params)
  series<-series[,c("HomeTeam", "AwayTeam", "HomeWins", "AwayWins")]
  series$HomeOdds<-apply(series, MARGIN = 1, FUN = function(x) playoffWin(x[1], x[2], x[3], x[4], params=params))
  series$AwayOdds<-1-series$HomeOdds
  series2<-series
  #For now, drop won games:
  series2$HomeWins <- series2$AwayWins <- NULL


  #Melt data to work with ggplot
  #melted<-reshape2::melt(series2, id.vars = c('HomeTeam', 'AwayTeam'))
  melted<-tidyr::pivot_longer(series2, cols = c("HomeOdds", "AwayOdds"), names_to = 'variable', values_to = 'value')
  melted$variable<-factor(x = melted$variable, levels = c("AwayOdds", "HomeOdds"), ordered = TRUE)
  #melted$HomeTeam <- factor(x = melted$HomeTeam, levels = melted$HomeTeam[1:(length(melted$HomeTeam)/2)], ordered = TRUE)
  melted$colour = ""

  for(i in 1:nrow(melted)){
    tc <- getTeamColours(home = melted[i, ]$HomeTeam, away= melted[i,]$AwayTeam)
    melted[i,]$colour<-ifelse(melted[i,]$variable == "HomeOdds",
                              yes=tc$home,
                              no=tc$away)
  }

  #Prepare instructions to read
  #text_home <- grid::textGrob("Home Win", gp = grid::gpar(fontsize = 10), hjust = 0)
  #text_away <- grid::textGrob("Away Win", gp = grid::gpar(fontsize = 10), hjust = 1)

  #build plot
  p<-ggplot2::ggplot(melted, ggplot2::aes_(y = quote(value), x = quote(HomeTeam), group = quote(variable))) +
    ggplot2::geom_bar(stat = "identity", position='fill', fill = melted$colour, colour = 'white') +
    ggplot2::labs(x = "",
                  y = "Series Odds",
                  title = "Predictions for Playoff Series",
                  subtitle = paste0("Before Games on ", Sys.Date(), ". Number of wins in brackets."),
                  caption = paste0("P. Bulsink (@BulsinkBot) | ", Sys.Date()))+
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.border = ggplot2::element_blank(),
                   panel.grid= ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(2,1,1,1), "lines"))+
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.3),
                                breaks = c(0,0.5,1)) +
    ggplot2::annotate("text", x = series$HomeTeam, y = -.01, hjust = 1, label = series$HomeTeam) +
    ggplot2::annotate("text", x = series$HomeTeam, y = 1.01, hjust = 0, label = series$AwayTeam) +
    ggplot2::annotate("label", x = series$HomeTeam, y = 0.01, hjust = 0, label = paste0(format(round(series$HomeOdds, 3), nsmall = 3), ' (', series$HomeWins, ')')) +
    ggplot2::annotate("label", x = series$HomeTeam, y= .99, hjust = 1, label = paste0(format(round(series$AwayOdds, 3), nsmall = 3), ' (', series$AwayWins, ')')) +
    ggplot2::coord_flip()

  return(p)

}


#' Plot single game expected goals
#'
#' @param home The Home Team
#' @param away The Away Team
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param maxgoal the max number of goals to predict. Plot a few less.
#'
#' @return a ggplot object
#' @export
plot_game<-function(home, away, params=NULL, maxgoal = 10){
  params<-parse_dc_params(params)
  # Expected goals home
  lambda <- try(stats::predict(params$m, data.frame(Home = 1, Team = home, Opponent = away), type = "response"), TRUE)

  # Expected goals away
  mu<-try(stats::predict(params$m, data.frame(Home = 0, Team = away, Opponent = home), type = "response"), TRUE)

  #fix errors
  if(!is.numeric(lambda)){
    lambda<-DCPredictErrorRecover(team = home, opponent = away, homeiceadv = TRUE)
  }
  if(!is.numeric(mu)){
    mu<-DCPredictErrorRecover(team = away, opponent = home, homeiceadv = FALSE)
  }

  probability_matrix<-dcProbMatrix(home=home, away=away,params=params,maxgoal=maxgoal)


  goals<-data.frame(Goals = c(0:maxgoal), Home = 0, Away = 0)

  goals$Away<-colSums(probability_matrix)*1/sum(colSums(probability_matrix))
  goals$Home<-rowSums(probability_matrix)*1/sum(rowSums(probability_matrix))

  #goals<-reshape2::melt(goals, id = "Goals", variable.name = "Team", value.name = "Density")
  goals<-tidyr::pivot_longer(goals, cols = c("Home", "Away"), names_to = "Team", values_to="Density")
  tc<-getTeamColours(home = home, away = away)
  plotcolors<-c(tc$home,tc$away)

  home_hjust<-1-(mu>lambda)

  odds<-DCPredict(home = home, away = away)

  p <- ggplot2::ggplot(data = goals, ggplot2::aes_(x = quote(Goals), y = quote(Density), fill = quote(Team))) +
    ggplot2::geom_area(position = "identity", alpha = 0.6) +
    ggplot2::geom_vline(xintercept = mu,linetype="dashed") +
    ggplot2::geom_vline(xintercept = lambda,linetype="dashed") +
    ggplot2::scale_x_continuous(limits = c(0, 8)) +
    ggplot2::scale_fill_manual(labels = c(home, away), values = plotcolors)+
    ggplot2::scale_color_manual(labels = c(home, away), values = plotcolors)+
    ggplot2::annotate(geom = 'label', x = mu, y = 0.0, label = paste0(away, "\nPredicted Goals:", format(round(mu, 2), nsmall = 2)), hjust = home_hjust, vjust = 0) +
    ggplot2::annotate(geom = 'label', x = lambda, y = 0.0, label = paste0(home, "\nPredicted Goals:",format(round(lambda, 2), nsmall = 2)), hjust = 1-home_hjust, vjust = 0) +
    ggplot2::labs(x = "Predicted Team Goals",
                  y = "Odds",
                  title = "Predicted Goals",
                  subtitle =  paste0(away, " at ", home, " on ", Sys.Date(),"\nWin Odds - Away: ", format(round(odds[[3]], 3), nsmall = 3), " - Home: ", format(round(odds[[1]], 3), nsmall = 3), " - OT/SO: ", format(round(odds[[2]], 3), nsmall = 3)),
                  caption = paste0("P. Bulsink (@BulsinkBot) | ", Sys.Date()))+
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.background = ggplot2::element_rect(fill = 'white', colour = 'white'),
                   legend.position = c(0.85, 0.85))

  ## ALT:
  ## data <- tibble(x=c(0:9,0:9), team=c(rep("Home", 10), rep("Away", 10)))
  ## data$y <- NA
  ## data$y[1:10]<-dpois(0:9, 3.8)
  ## data$y[11:20]<-dpois(0:9, 2.7)
  ## ggplot()+
  ##   geom_col(data = data[1:10,], mapping = aes(x = x, y = y, fill=team), alpha = .3) +
  ##   geom_col(data = data[11:20,], mapping = aes(x = x, y = y, fill=team), alpha = .3) +
  ##   geom_vline(xintercept = 2.7) + geom_vline(xintercept = 3.8) +
  ##   annotate(geom="label", x = 2.7, y=0, label="Away\nxG=2.7") + annotate(geom = "label", x = 3.8, y = 0, label="Home\nxG=3.8") +
  ##   xlab("Goals") + ylab("Odds") + ggtitle("Expected Goals", subtitle = "Fake Game 2019-12-19") + scale_x_continuous(breaks = c(0:9))
  return(p)
}

#' Team Point Predict Plot
#'
#' @param preds Raw predictions to generate ggridges point likelyhood plot.
#' @param savefiles Whether to save files to disk
#' @param graphic_dir Directory to save plot images
#' @param subdir Subdirectory to save plot images
#'
#' @return plot(s) in a list, named for conference(s) in use at the time.
#' @export
plot_point_likelihood <- function(preds=NULL, graphic_dir = file.path(devtools::package_file(), "prediction_results", "graphics"), subdir = 'pace', savefiles = TRUE) {
  if(is.null(preds)){
    #Try this:
    preds<-loopless_sim(nsims = 1e4)$raw_results
  }

  conferences<-getConferences()

  preds$Conf<-getTeamConferences(preds$Team)

  teamColours <- HockeyModel::teamColours
  teamColoursList<-as.vector(teamColours$Hex)
  names(teamColoursList)<-teamColours$Team

  p<-list()

  #sort the likelihood plots by points
  teamsorted<-preds %>%
    dplyr::group_by(.data$Team) %>%
    dplyr::summarise(mean.points = mean(.data$Points)) %>%
    dplyr::arrange(.data$mean.points) %>%
    dplyr::pull(.data$Team)

  preds$Team <- factor(preds$Team, levels = teamsorted)

  for(conf in conferences){
    conf_preds<-preds[preds$Conf == conf, ]

    conf_colourslist<-teamColoursList[names(teamColoursList) %in% conf_preds$Team]

    plot<-ggplot2::ggplot(conf_preds, ggplot2::aes_(x = quote(Points), y = quote(Team), fill=quote(Team))) +
      ggridges::geom_density_ridges(rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2, alpha=.6, from=40, to=130)+
      ggplot2::scale_fill_manual(values = conf_colourslist) +
      ggplot2::labs(x = 'Predicted Point Likelyhood',
                    y = '',
                    title = paste0("Point Likelyhoods for ", conf, " Conference - ", getCurrentSeason8()),
                    caption = paste0("P. Bulsink (@BulsinkBot) | ", Sys.Date()))+
      ggridges::theme_ridges(grid = TRUE) +
      ggplot2::theme(legend.position = "none",
                     panel.grid.major.y = ggplot2::element_line(size=.1, color="grey"))

    p[[conf]]<-plot

    if(savefiles){
      grDevices::png(filename = file.path(graphic_dir, subdir, paste0(tolower(conf), 'likelihood.png')), width = 11, height = 8.5, units = 'in', res = 300)
      print(plot)
      while(grDevices::dev.cur()!=1){
        grDevices::dev.off()
      }
    }
  }
  return(p)
}

#' Plot Team Rating
#'
#' @description Produces a plot of offensive and defensive ratings of teams, 0 centred (not scaled).
#'
#' @param m HockeyModel::m
#' @param teamlist select a subset of teams, if desired
#'
#' @return a ggplot2 plot
#' @export
plot_team_rating<-function(m = HockeyModel::m, teamlist = NULL){
  if(is.null(teamlist)){
    teamlist<-as.character(unique(m$data$Team))
  }
  #Note: invert defence because positive is better defence makes more sense
  team_params <- data.frame(Attack = as.numeric(m$coefficients[1:length(teamlist)]),
                            Defence = c(0, -m$coefficients[(length(teamlist)+1):(length(teamlist)*2-1)]),
                            Team = sort(teamlist))

  #Standardize data
  team_params$Attack <- (team_params$Attack - mean(team_params$Attack))/stats::sd(team_params$Attack)
  team_params$Defence <- (team_params$Defence - mean(team_params$Defence))/stats::sd(team_params$Defence)

  #Build and trim team colours for plot
  teamColoursList<-as.vector(HockeyModel::teamColours$Hex)
  names(teamColoursList)<-HockeyModel::teamColours$Team
  teamColoursList<-teamColoursList[names(teamColoursList) %in% teamlist]

  p<-ggplot2::ggplot(team_params,
                     ggplot2::aes_(x=quote(Attack),
                                   y=quote(Defence),
                                   color=quote(Team),
                                   label=quote(Team)
                     )
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = 'grey', size = 1)+
    ggplot2::geom_vline(xintercept = 0, colour = 'grey', size = 1)+
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::labs(x = "Offence",
                  y = "Defence",
                  title = "Current Team Offence & Defence Ratings",
                  subtitle = paste0("As of ", Sys.Date()),
                  caption = paste0("P. Bulsink (@BulsinkBot) | ", Sys.Date()))+
    ggplot2::theme_minimal() +
    ggplot2::coord_cartesian(xlim = c(-max(abs(team_params$Attack))+0.1,
                                      max(abs(team_params$Attack))+0.1),
                             ylim = c(-max(abs(team_params$Defence))+0.1,
                                      max(abs(team_params$Defence))+0.1))+
    ggplot2::annotate("label", x = -max(abs(team_params$Attack)), y = -max(abs(team_params$Defence)), hjust = 0, vjust = 0, label = "Bad") +
    ggplot2::annotate("label", x = max(abs(team_params$Attack)), y = max(abs(team_params$Defence)), hjust = 1, vjust = 1, label = "Good") +
    ggplot2::annotate("label", x = -max(abs(team_params$Attack)), y = max(abs(team_params$Defence)), hjust = 0, vjust = 1, label = "Calm") +
    ggplot2::annotate("label", x = max(abs(team_params$Attack)), y = -max(abs(team_params$Defence)), hjust = 1, vjust = 0, label = "Frantic") +
    ggplot2::theme(legend.position="none")

  if(requireNamespace("ggrepel", quietly = TRUE)){
    p <- p +
      ggrepel::geom_text_repel(force=2, max.iter=5000)
  }

  return(p)
}


#' Get Team Colours
#'
#' @description Given a home and away team, return a set of two colours that correspond one to each team, yet are not too similar to eachother. Instead of returning Blue and Blue for Buffalo and Tampa, change Buffalo to their Gold
#'
#' @param home Home Team colours to get
#' @param away Away Team's colours to get
#' @param delta Colour delta required. Default 0.15. See [colourDelta]. Must be between 0 and 1
#'
#' @return a list with two items: home & away, each containing the appropriate hex colour value
#' @export
#'
#' @examples
#' getTeamColours("Buffalo Sabres", "Tampa Bay Lightning")
getTeamColours<-function(home, away, delta = 0.15){
  teamColours <- HockeyModel::teamColours
  stopifnot(home %in% teamColours$Team)
  stopifnot(away %in% teamColours$Team)
  stopifnot(is.numeric(delta))
  stopifnot(delta<1)
  stopifnot(delta>=0)

  #Get primary & alternate colour for home and away
  hprimary<-teamColours[teamColours$Team == home, 'Hex']
  aprimary<-teamColours[teamColours$Team == away, 'Hex']
  halt<-teamColours[teamColours$Team == home, 'AltHex']
  aalt<-teamColours[teamColours$Team == away, 'AltHex']

  #record deltas to send most different colour incase no best option appears
  ppdelta <- colourDelta(hprimary, aprimary)
  padelta <- colourDelta(hprimary, aalt)
  apdelta <- colourDelta(halt, aprimary)
  aadelta <- colourDelta(halt, aalt)

  if(ppdelta < delta){
    #Colours too similar
    #Try away team alternate colour
    if(padelta < delta){
      #Still too similar, try home team alternate
      if(apdelta < delta){
        #Still too similar, try both alternates
        if(aadelta < delta){
          #all too similar, just use best delta
          message('No Great Colour Separation')
          bestdelta<-max(c(ppdelta, padelta, apdelta, aadelta))
          if(ppdelta == bestdelta){
            h<-hprimary
            a<-aprimary
          } else if(padelta == bestdelta){
            h<-hprimary
            a<-aalt
          } else if(apdelta == bestdelta){
            h<-halt
            a<-aprimary
          } else {
            h <- halt
            a <- aalt
          }
        } else {
          h <- halt
          a <- aalt
        }
      } else {
        h <- halt
        a<-aprimary
      }
    } else {
      h<-hprimary
      a<-aalt
    }
  } else {
    h<-hprimary
    a<-aprimary
  }

  return(list('home' = h, 'away' = a))
}


#' Format Playoff Odds
#'
#' @description Takes a playoff odds table and returns a gt table
#'
#' @param playoff_odds a playoff odds data frame with columns Team, Make_Playoffs, Win_First_Round, Win_Second_Round, Win_Conference, Win_Cup
#' @param caption_text Additional text to prepend to " Playoff Odds" in table title. E.g. 'Eastern Conference' if only eastteams sent in.
#'
#' @param trim Whether to drop teams that have 0 chance of making playoffs. Default true
#' @param trimcup Whether to drorp teams that have 0 chance of winning cup. Default false
#'
#' @return a gt table
#' @export
format_playoff_odds<-function(playoff_odds, caption_text = "", trim=TRUE, trimcup = FALSE){
  stopifnot(requireNamespace('gt', quietly = TRUE))
  teamColours <- HockeyModel::teamColours
  playoff_odds<-playoff_odds %>%
    dplyr::arrange(dplyr::desc(.data$Win_Cup), dplyr::desc(.data$Win_Conference), dplyr::desc(.data$Win_Second_Round), dplyr::desc(.data$Win_First_Round), dplyr::desc(.data$Make_Playoffs), .data$Team)

  if(trim){
    playoff_odds<-playoff_odds %>%
      dplyr::filter(.data$Make_Playoffs > 0)
  }
  if(trimcup){
    playoff_odds<-playoff_odds %>%
      dplyr::filter(.data$Win_Cup > 0)
  }

  playoff_odds_gt <- playoff_odds %>%
    tibble::add_column("block" = "  ", .before = 1) %>%
    tibble::add_column("image" = "", .after = 1) %>%
    dplyr::mutate("image" = .data$Team) %>%
    gt::gt() %>%
    gt::tab_header(title = paste(caption_text, "Playoff Odds"), subtitle = paste0("Generated ", Sys.Date(), " | P. Bulsink (@BulsinkBot)")) %>%
    gt::cols_label("block" = " ",
                   "image" = " ",
                   "Make_Playoffs" = "Make Playoffs",
                   "Win_First_Round" = "Win First Round",
                   "Win_Second_Round" = "Win Second Round",
                   "Win_Conference" = "Win Conference",
                   "Win_Cup" = "Win Cup") %>%
    gt::data_color(columns = 4:8, color = scales::col_numeric(c("#fefffe","#3ccc3c"), domain=c(0,1)))%>%
    gt::fmt_percent(columns = 4:8, drop_trailing_zeros = FALSE) %>%
    gt::tab_options(heading.align = 'left')

  for(i in 1:nrow(playoff_odds)) {
    playoff_odds_gt <- playoff_odds_gt %>%
      gt::tab_style(style = gt::cell_fill(color = teamColours[teamColours$Team == playoff_odds$Team[i], "Hex"]),
                    locations = gt::cells_body(columns = "block", rows = i)) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = "image", rows = i),
        fn = function(x) {
          gt::local_image(
            filename = ifelse(file.exists(file.path(devtools::package_file(), "data-raw", "logos", paste0(tolower(gsub(" ", "_", x)), ".gif"))),
                              file.path(devtools::package_file(), "data-raw", "logos", paste0(tolower(gsub(" ", "_", x)), ".gif")),
                              file.path(devtools::package_file(), "data-raw", "logos", "nhl.gif")),
            height = "30px")
        }
      )
  }

  return(playoff_odds_gt)
}


#' Today Odds Table
#'
#' @description Returns a gt table of odds for today's games (or games for a supplied date)
#'
#' @param today A date for games to create a table. Defaults to today.
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param schedule Schedule, or HockeyModel Schedule
#' @param include_logo whether to include dailyfaceoff logo
#'
#' @return a gt table
#' @export
daily_odds_table <- function(today = Sys.Date(), params=NULL, schedule = HockeyModel::schedule, include_logo=FALSE){
  stopifnot(requireNamespace('gt', quietly = TRUE))
  params<-parse_dc_params(params)
  todayodds<-todayDC(today = as.Date(today), params = params, schedule = schedule)
  todayodds$HomexG<-NA
  todayodds$AwayxG<-NA

  for(g in 1:nrow(todayodds)){
    # Expected goals home
    lambda <- try(stats::predict(params$m, data.frame(Home = 1, Team = todayodds$HomeTeam[g], Opponent = todayodds$AwayTeam[g]), type = "response"), TRUE)

    # Expected goals away
    mu<-try(stats::predict(params$m, data.frame(Home = 0, Team = todayodds$AwayTeam[g], Opponent = todayodds$HomeTeam[g]), type = "response"), TRUE)

    #fix errors
    if(!is.numeric(lambda)){
      lambda<-DCPredictErrorRecover(team = todayodds$HomeTeam[g], opponent = todayodds$AwayTeam[g], homeiceadv = TRUE)
    }
    if(!is.numeric(mu)){
      mu<-DCPredictErrorRecover(team = todayodds$AwayTeam[g], opponent = todayodds$HomeTeam[g], homeiceadv = FALSE)
    }

    todayodds$HomexG[g] <- lambda
    todayodds$AwayxG[g] <- mu
    todayodds[g, c('HomeWin', 'AwayWin')]<-normalizeOdds(todayodds[g, c('HomeWin', 'AwayWin')])
  }

  teamColours<-HockeyModel::teamColours

  todayodds_gt<- todayodds %>%
    dplyr::select(.data$HomeTeam, .data$HomexG, .data$HomeWin, .data$AwayWin, .data$AwayxG, .data$AwayTeam) %>%
    tibble::add_column("homeimage" = "", .before = 1) %>%
    tibble::add_column("homeblock" = "  ", .before = 1) %>%
    tibble::add_column("awayimage" = "") %>%
    tibble::add_column("awayblock" = "  ") %>%
    dplyr::mutate("homeimage" = .data$HomeTeam,
                  "awayimage" = .data$AwayTeam) %>%
    gt::gt() %>%
    gt::tab_header(title = paste0("Game Odds"), subtitle = paste0("For games ", today, " | P. Bulsink (@BulsinkBot)")) %>%
    gt::tab_spanner(label = "Home", columns = c('HomeTeam', 'HomexG', 'HomeWin')) %>%
    gt::tab_spanner(label = "Away", columns = c('AwayWin', 'AwayxG', 'AwayTeam')) %>%
    gt::cols_label("homeblock" = " ",
                   "homeimage" = " ",
                   "awayblock" = " ",
                   "awayimage" = " ",
                   "HomexG" = "xG",
                   "HomeWin" = "Win",
                   "HomeTeam" = "Team",
                   #"Draw" = "OT/SO?",
                   "AwayxG" = "xG",
                   "AwayWin" = "Win",
                   "AwayTeam" = "Team") %>%
    gt::data_color(columns = c(5,6), color = scales::col_numeric(palette = c("#cc3c3c", "#ffffff", "#3c3ccc"), domain=c(0,1)))%>%
    #gt::data_color(columns = 6, color = scales::col_numeric(palette = c("#fefeff", "#3c3ccc"), domain=c(0,1)))%>%
    #gt::data_color(columns = c(5,7), color = scales::col_bin(palette = c("#fefffe", "#ffffff", "#3ccc3c"), bins = c(0,0.5, 1)))%>%
    gt::fmt_percent(columns = 5:6, decimals = 1) %>%
    gt::fmt_number(columns = c(4, 7), drop_trailing_zeros = FALSE, decimals = 2) %>%
    gt::tab_options(heading.align = 'left',
                    table.border.bottom.color = "white",
                    table.border.top.color = "white")

  for(i in 1:nrow(todayodds)) {
    todayodds_gt <- todayodds_gt %>%
      gt::tab_style(style = gt::cell_fill(color = teamColours[teamColours$Team == todayodds$HomeTeam[i], "Hex"]),
                    locations = gt::cells_body(columns = "homeblock", rows = i)) %>%
      gt::tab_style(style = gt::cell_fill(color = teamColours[teamColours$Team == todayodds$AwayTeam[i], "Hex"]),
                    locations = gt::cells_body(columns = "awayblock", rows = i)) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = "homeimage", rows = i),
        fn = function(x) {
          gt::local_image(
            filename = ifelse(file.exists(file.path(devtools::package_file(), "data-raw", "logos", paste0(tolower(gsub(" ", "_", x)), ".gif"))),
                              file.path(devtools::package_file(), "data-raw", "logos", paste0(tolower(gsub(" ", "_", x)), ".gif")),
                              file.path(devtools::package_file(), "data-raw", "logos", "nhl.gif")),
            height = "30px")
        }
      ) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = "awayimage", rows = i),
      fn = function(x) {
        gt::local_image(
          filename = ifelse(file.exists(file.path(devtools::package_file(), "data-raw", "logos", paste0(tolower(gsub(" ", "_", x)), ".gif"))),
                            file.path(devtools::package_file(), "data-raw", "logos", paste0(tolower(gsub(" ", "_", x)), ".gif")),
                            file.path(devtools::package_file(), "data-raw", "logos", "nhl.gif")),
          height = "30px")
      }
    )
  }

  if(include_logo){
  todayodds_gt <- todayodds_gt %>%
    gt::tab_source_note(gt::md("<img src='https://www.dailyfaceoff.com/wp-content/uploads/2021/06/DFO-Logo-Mobile-Large.png' style='height:35px;'>"))
  }

  return(todayodds_gt)
}


series_odds_table <- function(){
  NULL  # TODO: DO IT
}

