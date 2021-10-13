#Hosts main functions
#Calls to data and prediction calculations, produce figures & tables

#' Update Model
#' @description updates the schedule, scores, and model parameters (m, rho, theta, gamma). Returns them in a list of named values
#'
#' @param save_data whether to save data to the package file
#'
#' @return a list of scores, schedule, and param named list
#'
#' @export
updateModel <- function(save_data=TRUE){
  schedule<-updateScheduleAPI(save_data = save_data)
  scores<-updateScoresAPI(schedule = schedule, save_data = save_data)
  params<-updateDC(scores = scores, save_data = save_data)
  return(list("scores" = scores, "schedule" = schedule, "params" = params))
}

#' Update predictions
#'
#' @param data_dir directory of predictions
#' @param scores HockeyModel::scores or a custom value
#' @param schedule HockeyModel::schedule or a custom value
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return NULL
#'
#' @export
updatePredictions<- function(data_dir = file.path(devtools::package_file(), "prediction_results"), scores = HockeyModel::scores, schedule = HockeyModel::schedule, params=NULL){
  params<-parse_dc_params(params)

  if(scores$Date[nrow(scores)] < (Sys.Date() - 7)){
    updateScoresAPI(save_data = TRUE)
  }
  filelist<-list.files(path = data_dir)
  pdates<-substr(filelist, 1, 10)  # gets the dates list of prediction
  pdates<-pdates[pdates != 'graphics']
  lastp<-as.Date(max(pdates))
  if(lastp != Sys.Date()){
    dcPredictMultipleDays(start = as.Date(lastp)+1, scores = scores, schedule = schedule, filedir = data_dir)
  }
}

#' Today's game odds graphic
#'
#' @param date date to predict odds. Default today
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param schedule HockeyModel::schedule or a custom value
#' @param scores HockeyModel::scores or a custom value
#'
#' @return today's odds ggplot object
#' @export
todayOddsPlot <- function(date = Sys.Date(), params=NULL, schedule = HockeyModel::schedule, scores = HockeyModel::scores){
  params<-parse_dc_params(params)

  if(scores$Date[nrow(scores)] < (date - 7)){
    message('Scores may be out of date. This can affect predictions. Please update if midseason.')
  }
  if(nrow(schedule[schedule$Date == date && schedule$GameState != "Postponed", ]) == 0){
    message("No games today.")
    return()
  }
  return(plot_odds_today(today = date, params=params, schedule = schedule))
}

#' Predict playoff odds graphic
#'
#' @return playoff odds ggplot object
#' @export
playoffOdds <- function(){
  return(plot_prediction_playoffs_by_team())
}

#' Predict President's Odds graphic
#'
#' @return president's odds ggplot object
#' @export
presidentOdds <- function(){
  return(plot_prediction_presidents_by_team())
}

#' Predict Points graphic
#'
#' @return point prediction ggplot object
#' @export
pointPredict <- function(){
  return(plot_prediction_points_by_team())
}

#' Current ratings
#'
#' @param m HockeyModel::m or a custom value
#'
#' @return today's ratings ggplot object
#' @export
ratings <- function(m = HockeyModel::m) {
  return(plot_team_rating(m=m))
}

tweet <- function(games, graphic_dir = './prediction_results/graphics/', token = rtweet::get_token(), delay =stats::runif(1, min=2,max=6)*60, schedule=HockeyModel::schedule){

  # if(!is.null(games_today)){
  #   #don't try tweet todays' games if none exist
  #   rtweet::post_tweet(status = "Predicted odds table for today's #NHL games. #HockeyTwitter",
  #                     media = file.path(graphic_dir, "today_odds_table.png"), token = token)
  #   my_timeline<-rtweet::get_timeline(user = 'BulsinkB', token = token)
  #   reply_id<-my_timeline$status_id[1]
  #   rtweet::post_tweet(status = "Predicted odds for today's #NHL games. #HockeyTwitter",
  #                      media = file.path(graphic_dir, "today_odds.png"), token = token)
  #   my_timeline<-rtweet::get_timeline(user = 'BulsinkB', token = token)
  #   reply_id<-my_timeline$status_id[1]
  #   rtweet::post_tweet(status = paste0("Current team ratings (as of ", Sys.Date(), "). #HockeyTwitter"),
  #                     media = file.path(graphic_dir, "current_rating.png"),
  #                     in_reply_to_status_id = reply_id, token = token)
  # } else {
  rtweet::post_tweet(status = paste0("Current team ratings (as of ", Sys.Date(), "). #HockeyTwitter"),
                       media = file.path(graphic_dir, "current_rating.png"))
  #}
  #until Rtweet has scheduler
  message("Delaying ", delay, " seconds to space tweets...")
  Sys.sleep(delay)

  if(nrow(schedule[schedule$Date >= Sys.Date() & schedule$GameType == "R",]) > 0){
    # Only runs if schedule has regular season games remaining

    rtweet::post_tweet(status = paste0("Predicted points for #NHL teams (before games on ", Sys.Date(), "). #HockeyTwitter"),
                       media = file.path(graphic_dir, "point_predict.png"), token = token)

    my_timeline<-rtweet::get_timeline(user = 'BulsinkB', token = token)
    reply_id<-my_timeline$status_id[1]

    #until Rtweet has scheduler
    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)

    rtweet::post_tweet(status = paste0("Playoff odds for #NHL teams (before games on ", Sys.Date(), "). #HockeyTwitter"),
                       media = file.path(graphic_dir, "playoff_odds.png"),
                       in_reply_to_status_id = reply_id, token = token)

    my_timeline<-rtweet::get_timeline(user = 'BulsinkB', token = token)
    reply_id<-my_timeline$status_id[1]

    #until Rtweet has scheduler
    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)

    rtweet::post_tweet(status = paste0("President's trophy odds for #NHL teams (before games on ", Sys.Date(), "). #HockeyTwitter"),
                       media = file.path(graphic_dir, "president_odds.png"),
                       in_reply_to_status_id = reply_id, token = token)
  }
}

#' Daily functions, rolled into one call
#'
#' @param graphic_dir Directory for graphic files
#' @param subdir subdirectory to `graphic_dir` for pace plots
#' @param token token to pass to rtweet calls
#' @param delay delay between tweet posts
#'
#' @export
dailySummary <- function(graphic_dir = './prediction_results/graphics/', subdir = "pace", token = rtweet::get_token(), delay =stats::runif(1, min=2,max=6)*60){

  if(inOffSeason()){
    if(getSeasonStartDate()-Sys.Date() > 7 | getSeasonStartDate() - Sys.Date() < 0){
      stop("Offseason")
    }
  }
  modelparams<-updateModel()
  sc<-modelparams$schedule
  params<-parse_dc_params(params=modelparams$params)

  if(Sys.Date() > max(sc$Date)){
    stop('No future games planned')
  }

  if(!dir.exists(graphic_dir)){
    dir.create(graphic_dir, recursive = TRUE)
  }

  message("Creating graphics...")

  #generate plots
  # if(Sys.Date() %in% sc$Date){
  #   today <- todayOddsPlot(params=params, schedule = modelparams$schedule, scores = modelparams$scores)
  #   #save to files.
  #   grDevices::png(filename = file.path(graphic_dir, 'today_odds.png'), width = 11, height = 8.5, units = 'in', res = 300)
  #   print(today)
  #   Sys.sleep(5)
  #   while(grDevices::dev.cur()!=1){
  #     grDevices::dev.off()
  #   }
  #
  #   today_table <- daily_odds_table(params=params, schedule = modelparams$schedule)
  #   gt::gtsave(today_table, filename = file.path(graphic_dir, 'today_odds_table.png'))
  # }

  if(inRegularSeason()){
    updatePredictions(scores = modelparams$scores, schedule = modelparams$schedule, params=params)
    playoff <- playoffOdds()
    president <- presidentOdds()
    point <- pointPredict()
    rating <- ratings(m = params$m)

    Sys.sleep(15)

    while(grDevices::dev.cur()!=1){
      grDevices::dev.off()
    }

    grDevices::png(filename = file.path(graphic_dir, 'playoff_odds.png'), width = 11, height = 8.5, units = 'in', res = 300)
    print(playoff)
    Sys.sleep(5)
    while(grDevices::dev.cur()!=1){
      grDevices::dev.off()
    }

    grDevices::png(filename = file.path(graphic_dir, 'president_odds.png'), width = 11, height = 8.5, units = 'in', res = 300)
    print(president)
    Sys.sleep(5)
    while(grDevices::dev.cur()!=1){
      grDevices::dev.off()
    }

    grDevices::png(filename = file.path(graphic_dir, 'point_predict.png'), width = 11, height = 8.5, units = 'in', res = 300)
    print(point)
    Sys.sleep(5)
    while(grDevices::dev.cur()!=1){
      grDevices::dev.off()
    }

    grDevices::png(filename = file.path(graphic_dir, 'current_rating.png'), width = 11, height = 8.5, units = 'in', res = 300)
    print(rating)
    Sys.sleep(5)
    while(grDevices::dev.cur()!=1){
      grDevices::dev.off()
    }

    #Make Pace Plots
    plot_pace_by_team(graphic_dir = graphic_dir, subdir = subdir, scores = modelparams$scores)
    plot_pace_by_division(graphic_dir = graphic_dir, subdir = subdir, scores=modelparams$scores)
    plot_point_likelihood(graphic_dir = graphic_dir, subdir = subdir)
  }

  message("Posting Tweets...")
  tweet(graphic_dir, token = token, delay = delay, graphic_dir = graphic_dir)#, games_today = Sys.Date() %in% sc[sc$GameState != "Postponed", ]$Date)
  #until Rtweet has scheduler
  message("Delaying ", delay, " seconds to space tweets...")
  Sys.sleep(delay)

  # tweetGames(games = sc[sc$Date == Sys.Date() && sc$GameState != 'Posponed', ], params=params, graphic_dir = graphic_dir, token = token, delay=delay)

  if(inRegularSeason()){
    tweetPlayoffOdds(token = token, graphic_dir = graphic_dir, params=params)

    #until Rtweet has scheduler
    message("Delaying ", delay/2, " seconds to space tweets...")
    Sys.sleep(delay/2)
  } else if (inPlayoffs()){
    tweetPlayoffOdds(token=token, graphic_dir = graphic_dir, trimcup = TRUE)
  }

  # if(as.numeric(format(Sys.Date(), "%w")) == 1 & inRegularSeason()){
  #   #On monday post pace plots
  #   tweetPace(token = token, delay = delay, graphic_dir = graphic_dir)
  # }

  if(as.numeric(format(Sys.Date(), "%w")) == 0 & inRegularSeason()) {
    #On Sunday post metrics
    tweetMetrics(token = token)
  }

  if(as.numeric(format(Sys.Date(), "%w")) == 2 & inRegularSeason()) {
    #On Tuesday post expected points (likelihood)
    tweetLikelihoods(delay = delay, graphic_dir = graphic_dir, token = token)
  }

  series<-getAPISeries()
  if(nrow(series[series$Status == "Ongoing", ]) > 0){  # TODO: Watch next spring to see if this goes ok
    tweetSeries(graphic_dir = graphic_dir, token=token, params=params)
    Sys.sleep(delay)
  }
}

#' Tweet Pace Plots
#'
#' @param delay Delay between posted tweets
#' @param graphic_dir The graphics directory
#' @param subdir The pace subdirectory in graphics
#' @param prediction_dir The predictions directory
#' @param token rtweet token
#' @param scores HockeyModel::scores or a custom value
#'
#' @export
tweetPace<-function(delay = stats::runif(1,min=1,max=3)*60, graphic_dir = file.path(devtools::package_file(), "prediction_results", "graphics"), subdir = "pace", prediction_dir = file.path(devtools::package_file(), "prediction_results"), token = rtweet::get_token(), scores = HockeyModel::scores){
  #make sure we're working with the most up-to-date info.
  scores<-updateScoresAPI(save_data = T)

  #Make Pace Plots
  plot_pace_by_team(graphic_dir = graphic_dir, subdir = subdir, prediction_dir = prediction_dir, scores = scores)

  filelist<-list.files(path = prediction_dir)
  pdates<-substr(filelist, 1, 10)  # gets the dates list of prediction
  pdates<-pdates[pdates != 'graphics']
  lastp<-as.Date(max(pdates))
  current_preds<-readRDS(file.path(prediction_dir, paste0(lastp,"-predictions.RDS")))
  preds<-readRDS(file.path(prediction_dir, paste0(getSeasonStartDate(), "-predictions.RDS")))
  scores<-scores[scores$Date > as.Date(getSeasonStartDate()), ]

  teamlist<-unique(preds$Team)

  teamColours <- HockeyModel::teamColours

  reply_id <- NULL
  for(team in teamlist){
    ngames <- sum(sum(scores$HomeTeam == team), sum(scores$AwayTeam == team))
    status<-paste0(team,
                   " pace after ",
                   ngames,
                   " games. The model initially predicted ",
                   format(round(as.numeric(preds[preds$Team == team, 'meanPoints']), digits = 1), nsmall = 1),
                   " points, now expecting ",
                   format(round(as.numeric(current_preds[current_preds$Team == team, 'meanPoints']), digits = 1), nsmall = 1),
                   ". #HockeyTwitter ",
                   teamColours[teamColours$Team == team, "Hashtag"])

    rtweet::post_tweet(status = status,
                       media = file.path(graphic_dir,
                                         subdir,
                                         paste0(tolower(gsub(" ", "_", team)), '.png')),
                       in_reply_to_status_id = reply_id,
                       token = token)

    #until Rtweet has scheduler
    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(stats::runif(1,min=1,max=3)*60)
    my_timeline<-rtweet::get_timeline(user = 'BulsinkB', token = token)
    reply_id<-my_timeline$status_id[1]
  }
  pacediff<-data.frame("Team" = current_preds$Team, "Initial" = preds$meanPoints, "Current" = current_preds$meanPoints, stringsAsFactors = FALSE)
  pacediff$Diff <- pacediff$Current - pacediff$Initial

  maxteam<-pacediff[which.max(pacediff$Diff), 'Team']
  minteam<-pacediff[which.min(pacediff$Diff), 'Team']

  recapstatus <- paste0("To recap - ",
                        "\nFurthest above expectation: ", maxteam, " ", teamColours[teamColours$Team == maxteam, "Hashtag"],
                        "\nFurthest below expectation: ", minteam, " ", teamColours[teamColours$Team == minteam, "Hashtag"])
  rtweet::post_tweet(status = recapstatus,
                     in_reply_to_status_id = reply_id,
                     token = token)

  Sys.sleep(stats::runif(1,min=2,max=6)*60)

  #Make Division Plots
  plot_pace_by_division(graphic_dir = graphic_dir, subdir = subdir, prediction_dir = prediction_dir, scores=scores)

  reply_id<-NULL

  for (division in getDivisions()){
    status <- paste("Current Points compared to predicted (at season start) for #NHL teams in the", division, "division.\nPositive values are exceeding expectation, negative are performing below predicted. #HockeyTwitter")
    rtweet::post_tweet(status = status,
                       media = file.path(graphic_dir,
                                         subdir,
                                         paste0(division, '_pace.png')),
                       in_reply_to_status_id = reply_id,
                       token = token)
    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)
    my_timeline<-rtweet::get_timeline(user = 'BulsinkB', token = token)
    reply_id<-my_timeline$status_id[1]
  }
}

#' Tweet Likelihood plots (ggridges)
#'
#' @param delay time to delay. Default 5 min
#' @param graphic_dir graphics directory
#' @param subdir subdirectory - usually 'preds'
#' @param token rtweeet token
#' @param scores updated scores
#
#' @export
tweetLikelihoods <- function(delay =stats::runif(1,min=3,max=6)*60, graphic_dir = file.path(devtools::package_file(), "prediction_results", "graphics"), subdir = "pace", token = rtweet::get_token(), scores = HockeyModel::scores) {
  #make likelihood plots
  plot_point_likelihood(graphic_dir = graphic_dir, subdir = subdir)

  for (conf in getConferences()){
    #Tweet them out
    rtweet::post_tweet(status = paste0("#NHL ", conf, " Conference Team final point likelihoods:"),
                       media = file.path(graphic_dir,
                                         subdir,
                                         paste0(tolower(conf), 'likelihood.png')),
                       token = token)
    #until Rtweet has scheduler
    message("Delaying ", delay/2, " seconds to space tweets...")
    Sys.sleep(delay/2)
  }
}

#' Tweet Game Plots
#'
#' @param games Games to tweet graphics from
#' @param delay Delay between tweets
#' @param graphic_dir the graphics directory
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param token the token for rtweet
#'
#' @export
tweetGames<-function(games = games_today(), delay =stats::runif(1,min=4,max=8)*60, graphic_dir = file.path(devtools::package_file(), "prediction_results", "graphics"), params=NULL, token = rtweet::get_token()){
  params<-parse_dc_params(params)
  #Tweet each game
  if(is.null(games)){
    message("No games to tweet")
    return()
  }

  if(nrow(games) == 0){
    message("No games to tweet")
    return()
  }

  if(!dir.exists(graphic_dir)){
    dir.create(graphic_dir, recursive = TRUE)
  }

  teamColours <- HockeyModel::teamColours

  for(g in 1:nrow(games)){
    home<-as.character(games[g,"HomeTeam"])
    away<-as.character(games[g,"AwayTeam"])
    plt<-plot_game(home = home, away = away, params=params)
    grDevices::png(filename = file.path(graphic_dir, 'predicted_goals.png'), width = 11, height = 8.5, units = 'in', res = 300)
    print(plt)
    while(grDevices::dev.cur()!=1){
      grDevices::dev.off()
    }
    status<-paste0(teamColours[teamColours$Team == away, "Hashtag"], " at ", teamColours[teamColours$Team == home, "Hashtag"], " predicted goals. #", getShortTeam(away),"vs",getShortTeam(home)," #HockeyTwitter")
    rtweet::post_tweet(status = status,
                       media = file.path(graphic_dir, 'predicted_goals.png'),
                       token = token)

    file.remove(file.path(graphic_dir, 'predicted_goals.png'))

    #until Rtweet has scheduler
    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)
  }
}

#' Tweet Metrics
#' @description Tweet the metrics (Log Loss and Accuracy)
#' @param token rtweet token
#'
#' @return NULL
#' @export
tweetMetrics<-function(token = rtweet::get_token()){
  metrics<-getSeasonMetricsDC()

  status <- paste0("Metrics as of ", Sys.Date(),
                   "\nLog Loss: ", round(metrics$LogLoss, 4),
                   "\nAccuracy: ", round(metrics$Accuracy * 100, 2), " %\n#HockeyTwitter")
  message(status)

  rtweet::post_tweet(status = status, token = token)
}

#' Tweet Series
#' @description Tweet the series odds graphics
#'
#' @param graphic_dir directory to save the image
#' @param token rtweet token
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return NULL
#' @export
tweetSeries<-function(token = rtweet::get_token(), params=NULL, graphic_dir = file.path(devtools::package_file(), "prediction_results", "graphics")){
  params<-parse_dc_params(params)
  while(grDevices::dev.cur()!=1){
    grDevices::dev.off()
  }
  series<-getAPISeries()
  series<-series[series$Status == "Ongoing", c("HomeTeam", "AwayTeam", "HomeWins", "AwayWins")]
  if(nrow(series) == 0){
    message('No Series to Tweet')
    return()
  }
  plt<-plot_playoff_series_odds(series = series, params=params)
  grDevices::png(filename = file.path(graphic_dir, 'series_odds.png'), width = 11, height = 8.5, units = 'in', res = 300)
  print(plt)
  while(grDevices::dev.cur()!=1){
    grDevices::dev.off()
  }

  status <- paste0("#NHL #StanleyCup Playoff Series Odds before games on ", Sys.Date(), " #HockeyTwitter")

  rtweet::post_tweet(status = status,
                     media = file.path(graphic_dir, 'series_odds.png'),
                     token = token)
}


#' Tweet Playoff Odds
#'
#' @description Tweet a graphic of the playoff odds
#'
#' @param summary_results the summary results file, otherwise the msot recent will be loaded
#' @param token token for twitter
#' @param graphic_dir graphic dir
#' @param trimcup trim to just cup winners
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return NULL
#' @export
tweetPlayoffOdds<-function(summary_results=NULL, params=NULL, token = rtweet::get_token(), graphic_dir = file.path(devtools::package_file(), "prediction_results", "graphics"), trimcup = FALSE){
  params<-parse_dc_params(params)
  playoffodds <- simulatePlayoffs(summary_results = summary_results, params=params)

  playoffodds$Conference <- getTeamConferences(playoffodd$Team)

  for(conf in unique(playoffodds$Conference)){
    plt<-format_playoff_odds(playoff_odds = playoffodds[playoffodds$Conference == conf, which(names(playoffodds) != "Conference")], caption_text = paste(conf, "Conference"), trim=FALSE, trimcup=trimcup)
    gt::gtsave(plt, filename = file.path(graphic_dir, paste0(tolower(conf), "_playoff_odds.png")))
  }
  status<- paste0("#NHL Eastern and Western Conference Playoff and #StanleyCup Odds before games on ", Sys.Date(), ". #HockeyTwitter")

  #Posting Tweet
  rtweet::post_tweet(status = status,
                     media = c(file.path(graphic_dir, "eastern_playoff_odds.png"), file.path(graphic_dir, "western_playoff_odds.png")),
                     token = token)

}
