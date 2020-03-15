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
    ggplot2::labs(x = "Date",
                  y = "Points",
                  title = paste0("Predicted Points Over the Past ", past_days, " Days"),
                  caption = paste0("P. Bulsink (@BulsinkB) | ", Sys.Date()))+
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
    ggplot2::scale_x_date(expand = ggplot2::expand_scale(mult = c(0,.33))) +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::labs(x = "Date",
                  y = "Playoff Odds",
                  title = paste0("Playoff Odds Over the Past ", past_days, " Days"),
                  caption = paste0("P. Bulsink (@BulsinkB) | ", Sys.Date()))+
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
    ggplot2::labs(x = "Date",
                  y = "President's Trophy Odds",
                  title = paste0("President's Trophy Odds Over the Past ", past_days, " Days"),
                  caption = paste0("P. Bulsink (@BulsinkB) | ", Sys.Date()),
                  subtitle = "Teams with < 1% odds hidden for simplicity")+
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
  sc<-scores[scores$Date > as.Date(getCurrentSeasonStartDate()),]

  teamlist<-unique(c(as.character(sc$HomeTeam), as.character(sc$AwayTeam)))

  #Get old and most recent predictions
  p<-readRDS(file.path(prediction_dir, paste0(getCurrentSeasonStartDate(), "-predictions.RDS")))

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
    minq<-qteam$meanPoints - 2*(qteam$sdPoints)

    plt <- ggplot2::ggplot(teamscores, ggplot2::aes_(x = quote(GameNum), y = quote(cPoints), colour = quote(Venue))) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(limits = c(0, 82)) +
      ggplot2::scale_y_continuous(limits = c(0, 164)) +
      ggplot2::labs(x = "Game Number",
                    y = "Points",
                    title = "Points Pace",
                    subtitle = paste0(team, ' Expected Points: ', format(round(qpoints, 1), nsmall = 1)),
                    caption = paste0("P. Bulsink (@BulsinkB) | ", Sys.Date()))+
      ggplot2::theme_minimal() +
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
    tc <- getTeamColours(home = todayodds[i, 'HomeTeam'], away= todayodds[i, 'AwayTeam'])
    plotcolors<-c(plotcolors, tc$home, tc$home, tc$away, tc$away)
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
    ggplot2::labs(x = "",
                  y = "Result Odds",
                  title = "Predictions for Today's Games",
                  subtitle = paste0("Games played on ", Sys.Date()),
                  caption = paste0("P. Bulsink (@BulsinkB) | ", Sys.Date()))+
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
    ggplot2::annotate("label", x = todayodds$HomeTeam, y=todayodds$HomeWin + todayodds$HomeWinOT + 0.01, hjust = 0, label = format(round(todayodds$AwayWinOT, 3), nsmall = 3)) +
    ggplot2::coord_flip()

  return(p)

}


#' Plot Today's Playoff Series Odds
#'
#' @param series A data frame of home team, away team, home wins, away wins
#' @param rho HockeyModel::rho or a custom value
#' @param m HockeyModel::m or a custom value
#' @param schedule HockeyModel::schedule or a custom value
#' @param teamColours HockeyModel::teamColours or a custom value
#' @param ... additional parameters to pass
#'
#' @return a ggplot image of odds
#'
#' @export
plot_playoff_series_odds <- function(series = HockeyModel::series, rho=HockeyModel::rho, m = HockeyModel::m, schedule = HockeyModel::schedule, teamColours=HockeyModel::teamColours, ...) {

  series$HomeOdds<-apply(series, MARGIN = 1, FUN = function(x) playoffWin(x[1], x[2], x[3], x[4]))
  series$AwayOdds<-1-series$HomeOdds

  #For now, drop won games:
  series$HomeWins <- series$AwayWins <- NULL


  #Melt data to work with ggplot
  melted<-reshape2::melt(series, id.vars = c('HomeTeam', 'AwayTeam'))
  melted$variable<-factor(x = melted$variable, levels = c("AwayOdds", "HomeOdds"), ordered = TRUE)
  melted$HomeTeam <- factor(x = melted$HomeTeam, levels = melted$HomeTeam[1:(length(melted$HomeTeam)/2)], ordered = TRUE)

  #Make colour list for plot
  plotcolors<-c()
  for(i in 1:nrow(series)){
    tc <- getTeamColours(home = series[i, 'HomeTeam'], away= series[i, 'AwayTeam'])
    plotcolors<-c(plotcolors, tc$home, tc$away)
  }

  #Prepare instructions to read
  text_home <- grid::textGrob("Home Win", gp = grid::gpar(fontsize = 10), hjust = 0)
  text_away <- grid::textGrob("Away Win", gp = grid::gpar(fontsize = 10), hjust = 1)

  #build plot
  p<-ggplot2::ggplot(melted[melted$variable %in% c('HomeOdds','AwayOdds'),],
                     ggplot2::aes_(y = quote(value), x = quote(HomeTeam), group = quote(variable))) +
    ggplot2::geom_bar(stat = "identity", position='fill', fill = plotcolors, colour = 'white') +
    ggplot2::labs(x = "",
                  y = "Series Odds",
                  title = "Predictions for Playoff Series",
                  subtitle = paste0("Before Games on ", Sys.Date()),
                  caption = paste0("P. Bulsink (@BulsinkB) | ", Sys.Date()))+
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.border = ggplot2::element_blank(),
                   panel.grid= ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(2,1,1,1), "lines"))+
    ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(add = 0.3),
                                breaks = c(0,0.5,1)) +
    ggplot2::annotate("text", x = series$HomeTeam, y = -.01, hjust = 1, label = series$HomeTeam) +
    ggplot2::annotate("text", x = series$HomeTeam, y = 1.01, hjust = 0, label = series$AwayTeam) +
    ggplot2::annotate("label", x = series$HomeTeam, y = 0.01, hjust = 0, label = format(round(series$HomeOdds, 3), nsmall = 3)) +
    ggplot2::annotate("label", x = series$HomeTeam, y= .99, hjust = 1, label = format(round(series$AwayOdds, 3), nsmall = 3)) +
    ggplot2::coord_flip()

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
                  caption = paste0("P. Bulsink (@BulsinkB) | ", Sys.Date()))+
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
#' @param preds Raw predictions to generate ggridges point likelyhood plot. Otherwise will generate with current m, rho, scores and schedule in HockeyModel.
#' @param graphic_dir Directory to save plot images
#' @param subdir Subdirectory to save plot images
#' @param ... Additional parameters to pass to loopless sim
#'
#' @return two plots in list, as $eastplot and $westplot
#' @export
plot_point_likelihood <- function(preds=NULL, graphic_dir = './prediction_results/graphics', subdir = 'pace', ...) {

  if(is.null(preds)){
    preds<-loopless_sim(...)
  }

  east_preds<-preds$raw_results
  east_preds<-east_preds[east_preds$Team %in% HockeyModel::nhl_conferences$East,]
  west_preds<-preds$raw_results
  west_preds<-west_preds[west_preds$Team %in% HockeyModel::nhl_conferences$West,]

  teamColours <- HockeyModel::teamColours
  teamColoursList<-as.vector(teamColours$Hex)
  names(teamColoursList)<-teamColours$Team
  east_colour <- teamColoursList[names(teamColoursList) %in% east_preds$Team]
  west_colour <- teamColoursList[names(teamColoursList) %in% west_preds$Team]


  eastplot<-ggplot2::ggplot(east_preds, ggplot2::aes_(x = quote(Points), y = quote(Team), fill=quote(Team))) +
    ggridges::geom_density_ridges(rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2, alpha=.6, from=40, to=130)+
    ggplot2::scale_fill_manual(values = east_colour) +
    ggplot2::labs(x = 'Predicted Point Likelyhood',
                  y = '',
                  title = paste0("Point Likelyhoods for Eastern Conference - ", getCurrentSeason8()),
                  caption = paste0("P. Bulsink (@BulsinkB) | ", Sys.Date()))+
    ggridges::theme_ridges(grid = FALSE) +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.y = ggplot2::element_line(size=.1, color="grey"))

  westplot<-ggplot2::ggplot(west_preds, ggplot2::aes_(x = quote(Points), y = quote(Team), fill=quote(Team))) +
    ggridges::geom_density_ridges(rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2, alpha=.6, from = 40, to = 130)+
    ggplot2::scale_fill_manual(values = west_colour) +
    ggplot2::labs(x = 'Predicted Point Likelyhood',
                  y = '',
                  title = paste0("Point Likelyhoods for Western Conference - ", getCurrentSeason8()),
                  caption = paste0("P. Bulsink (@BulsinkB) | ", Sys.Date()))+
    ggridges::theme_ridges(grid = FALSE) +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.y = ggplot2::element_line(size=.1, color="grey"))


  grDevices::png(filename = file.path(graphic_dir, subdir, 'eastlikelihood.png'), width = 11, height = 8.5, units = 'in', res = 300)
  print(eastplot)
  while(grDevices::dev.cur()!=1){
    grDevices::dev.off()
  }

  grDevices::png(filename = file.path(graphic_dir, subdir, 'westlikelihood.png'), width = 11, height = 8.5, units = 'in', res = 300)
  print(westplot)
  while(grDevices::dev.cur()!=1){
    grDevices::dev.off()
  }
}

#' Plot Team Rating
#'
#' @description Produces a plot of offensive and defensive ratings of teams, 0 centred (not scaled).
#'
#' @param m HockeyModel::m
#' @param teamlist select a subset of teams, if desired
#'
#' @return a ggplot2 plot
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
                  caption = paste0("P. Bulsink (@BulsinkB) | ", Sys.Date()))+
    ggplot2::theme_minimal() +
    ggplot2::coord_cartesian(xlim = c(-max(abs(team_params$Attack))+0.1,
                                      max(abs(team_params$Attack))+0.1),
                             ylim = c(-max(abs(team_params$Defence))+0.1,
                                      max(abs(team_params$Defence))+0.1))+
    ggrepel::geom_text_repel(force=2, max.iter=5000) +
    ggplot2::annotate("label", x = -max(abs(team_params$Attack)), y = -max(abs(team_params$Defence)), hjust = 0, vjust = 0, label = "Bad") +
    ggplot2::annotate("label", x = max(abs(team_params$Attack)), y = max(abs(team_params$Defence)), hjust = 1, vjust = 1, label = "Good") +
    ggplot2::annotate("label", x = -max(abs(team_params$Attack)), y = max(abs(team_params$Defence)), hjust = 0, vjust = 1, label = "Calm") +
    ggplot2::annotate("label", x = max(abs(team_params$Attack)), y = -max(abs(team_params$Defence)), hjust = 1, vjust = 0, label = "Frantic") +
    ggplot2::theme(legend.position="none")
  return(p)
}


#' Get Team Colours
#'
#' @description Given a home and away team, return a set of two colours that correspond one to each team, yet are not too similar to eachother. Instead of returning Blue and Blue for Buffalo and Tampa, change Buffalo to their Gold
#'
#' @param home Home Team colours to get
#' @param away Away Team's colours to get
#' @param delta Colour delta required. Default 0.15. See [colourDelta]. Must be between 0 and 1
#' @param teamColours HockeyModel::TeamColours, or other provided (optional)
#'
#' @return a list with two items: home & away, each containing the appropriate hex colour value
#' @export
#'
#' @examples
#' getTeamColours("Buffalo Sabres", "Tampa Bay Lightning")
getTeamColours<-function(home, away, delta = 0.15, teamColours = HockeyModel::teamColours){
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


#' Export a Formattable as PNG, PDF, or JPEG
#'
#' @description From https://github.com/renkun-ken/formattable/issues/26.
#'
#' @param f A formattable.
#' @param file Export path with extension .png, .pdf, or .jpeg.
#' @param width Width specification of the html widget being exported.
#' @param height Height specification of the html widget being exported.
#' @param background Background color specification.
#' @param delay Time to wait before taking webshot, in seconds.
export_formattable <- function(f, file, width = "100%", height = NULL,
                               background = "white", delay = 0.2)
{
  if(!webshot::is_phantomjs_installed()){
    webshot::install_phantomjs()
  }
  w <- formattable::as.htmlwidget(f, width = width, height = height)
  path <- htmltools::html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot::webshot(url,
                   file = file,
                   selector = ".formattable_widget",
                   delay = delay)
}
