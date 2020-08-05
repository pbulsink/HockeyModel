#Hosts main functions
#Calls to data and preciction calculations, produce figures & tables

#' Update Model
#'
#' @param ... Items to pass to update functions.
#'
#' @export
updateModel <- function(...){
  scores<-updateScores(last_playoffs = Sys.Date() > as.Date("2020-07-29"), ...)
  schedule<-updateSchedule(...)
  dcparams<-updateDC(scores = scores)
  #devtools::install(local = FALSE)
  #.rs.restartR()
  #require(HockeyModel)
  return(list(scores = scores, schedule = schedule, m = dcparams$m, rho = dcparams$rho))
}

#' Update predictions
#'
#' @param data_dir directory of predictions
#' @param scores HockeyModel::scores or a custom value
#' @param schedule HockeyModel::schedule or a custom value
#' @param ... Items to pass to update functions.
#'
#' @export
updatePredictions<- function(data_dir = "./prediction_results/", scores = HockeyModel::scores, schedule = HockeyModel::schedule, ...){
  if(scores$Date[nrow(scores)] < (Sys.Date() - 7)){
    message('Scores may be out of date. This can affect predictions. Please update if midseason.')
  }
  filelist<-list.files(path = data_dir)
  pdates<-substr(filelist, 1, 10)  # gets the dates list of prediction
  pdates<-pdates[pdates != 'graphics']
  lastp<-as.Date(max(pdates))
  if(lastp != Sys.Date()){
    dcPredictMultipleDays(start = as.Date(lastp)+1, scores = scores, schedule = schedule, ...)
  }
}

#' Today's game odds graphic
#'
#' @param date date to predict odds. Default today
#' @param rho HockeyModel::rho or a custom value
#' @param m HockeyModel::m or a custom value
#' @param schedule HockeyModel::schedule or a custom value
#' @param scores HockeyModel::scores or a custom value
#' @param ... Items to pass to update functions.
#'
#' @return today's odds ggplot object
#' @export
todayOdds <- function(date = Sys.Date(), rho = HockeyModel::rho, m = HockeyModel::m, schedule = HockeyModel::schedule, scores = HockeyModel::scores, ...){
  if(scores$Date[nrow(scores)] < (date - 7)){
    message('Scores may be out of date. This can affect predictions. Please update if midseason.')
  }
  if(nrow(schedule[schedule$Date == date, ]) == 0){
    stop("No games today.")
  }
  return(plot_odds_today(date, rho = rho, m = m, schedule = schedule, ...))
}

#' Predict playoff odds graphic
#'
#' @param ... Items to pass to update and plot functions.
#'
#' @return playoff odds ggplot object
#' @export
playoffOdds <- function(...){
  # if(scores$Date[nrow(scores)] < (Sys.Date() - 7)){
  #   message('Scores may be out of date. This can affect predictions. Please update if midseason.')
  # }
  # updatePredictions(...)
  return(plot_prediction_playoffs_by_team(...))
}

#' Predict President's Odds graphic
#'
#' @param ... Items to pass to update and plot functions.
#'
#' @return president's odds ggplot object
#' @export
presidentOdds <- function(...){
  # if(scores$Date[nrow(scores)] < (Sys.Date() - 7)){
  #   message('Scores may be out of date. This can affect predictions. Please update if midseason.')
  # }
  # updatePredictions(...)
  return(plot_prediction_presidents_by_team())
}

#' Predict Points graphic
#'
#' @param ... Items to pass to update and plot functions.
#'
#' @return point prediction ggplot object
#' @export
pointPredict <- function(...){
  # if(scores$Date[nrow(scores)] < (Sys.Date() - 7)){
  #   message('Scores may be out of date. This can affect predictions. Please update if midseason.')
  # }
  # updatePredictions(...)
  return(plot_prediction_points_by_team())
}

#' Current ratings
#'
#' @param m HockeyModel::m or a custom value
#' @param ... Items to pass to plot functions.
#'
#' @return today's ratings ggplot object
#' @export
ratings <- function(m = HockeyModel::m, ...) {
  return(plot_team_rating(m=m, ...))
}

tweet <- function(games, graphic_dir = './prediction_results/graphics/', token = rtweet::get_token(), delay = 60*10, games_today = FALSE, ...){

  if(games_today){
    #don't try tweet todays' games if none exist
    rtweet::post_tweet(status = "Predicted odds for today's #NHL games. #HockeyTwitter",
                      media = file.path(graphic_dir, "today_odds.png"), token = token)
    my_timeline<-rtweet::get_timeline(user = 'BulsinkB', token = token)
    reply_id<-my_timeline$status_id[1]
    rtweet::post_tweet(status = paste0("Current team ratings (as of ", Sys.Date(), "). #HockeyTwitter"),
                      media = file.path(graphic_dir, "current_rating.png"),
                      in_reply_to_status_id = reply_id, token = token)
  } else {
    rtweet::post_tweet(status = paste0("Current team ratings (as of ", Sys.Date(), "). #HockeyTwitter"),
                       media = file.path(graphic_dir, "current_rating.png"))
  }
  #until Rtweet has scheduler
  message("Delaying ", delay, " seconds to space tweets...")
  Sys.sleep(delay)

  if(Sys.Date() <= as.Date('2020-07-29')){
    #TODO Doesn't yet programattically know that reg. season is done. Fix this summer.

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
#' @param token token to pass to rtweet calls
#' @param delay delay between tweet posts
#' @param ... Items to pass to update and plot functions.
#'
#' @export
dailySummary <- function(graphic_dir = './prediction_results/graphics/', token = rtweet::get_token(), delay = 60*10, ...){
  #message("Reminder, run updateModel() first.")
  #Sys.sleep(5)

  if(lubridate::year(Sys.Date()) == 2020 & lubridate::month(Sys.Date()) %in% c(7:10)){
    #COVID RULES
    modelparams<-updateModel(...)
    sc<-modelparams$schedule

    if(Sys.Date() %in% sc$Date){
      today <- todayOdds(rho = modelparams$rho, m = modelparams$m, schedule = modelparams$schedule, scores = modelparams$scores, ...)
      #save to files.
      grDevices::png(filename = file.path(graphic_dir, 'today_odds.png'), width = 11, height = 8.5, units = 'in', res = 300)
      print(today)
      Sys.sleep(5)
      while(grDevices::dev.cur()!=1){
        grDevices::dev.off()
      }

      rtweet::post_tweet(status = "Predicted odds for today's #NHL games. #HockeyTwitter",
                         media = file.path(graphic_dir, "today_odds.png"), token = token)

      tweetGames(games = sc[sc$Date == Sys.Date(), ], delay = delay, graphic_dir = graphic_dir, m = modelparams$m, rho = modelparams$rho, token = token)
    }
    return(NULL)
  }
  if(lubridate::month(Sys.Date()) %in% c(7:9) & lubridate::year((Sys.Date()) != 2020)){
    stop('No off-season predictions')
  }

  modelparams<-updateModel(...)
  in_reg_season<-FALSE
  if(Sys.Date()<= as.Date("2020-7-29")){
    in_reg_season<-TRUE
    if(Sys.Date()<=as.Date("2020-04-10")){
      updatePredictions(scores = modelparams$scores, schedule = modelparams$schedule)
    }
  }

  if(!dir.exists(graphic_dir)){
    dir.create(graphic_dir, recursive = TRUE)
  }
  sc<-modelparams$schedule

  message("Creating graphics...")

    #generate plots
  if(Sys.Date() %in% sc$Date){
    today <- todayOdds(rho = modelparams$rho, m = modelparams$m, schedule = modelparams$schedule, scores = modelparams$scores, ...)
    #save to files.
    grDevices::png(filename = file.path(graphic_dir, 'today_odds.png'), width = 11, height = 8.5, units = 'in', res = 300)
    print(today)
    Sys.sleep(5)
    while(grDevices::dev.cur()!=1){
      grDevices::dev.off()
    }
  }
  if(in_reg_season){
    playoff <- playoffOdds()
    president <- presidentOdds()
    point <- pointPredict()
    rating <- ratings(m = modelparams$m)

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
  }

  message("Posting Tweets...")
  tweet(graphic_dir, token = token, delay = delay, graphic_dir = graphic_dir, games_today = Sys.Date() %in% sc$Date, ...)
  #until Rtweet has scheduler
  message("Delaying ", delay, " seconds to space tweets...")
  Sys.sleep(delay)

  tweetGames(games = sc[sc$Date == Sys.Date(), ], m = modelparams$m, rho = modelparams$rho, graphic_dir = graphic_dir, token = token, delay=delay)

  if(lubridate::month(Sys.Date()) %in% c(3,4) & in_reg_season){
    playoff_odds<-playoffSolver()
    #save to files.
    export_formattable(playoff_odds$east, file.path(graphic_dir, 'east_playoff_odds.png'))
    export_formattable(playoff_odds$west, file.path(graphic_dir, 'west_playoff_odds.png'))

    rtweet::post_tweet(status = "#NHL Eastern and Western Conference Playoff & #StanleyCup Odds #HockeyTwitter",
                       media = c(file.path(graphic_dir, 'east_playoff_odds.png'),
                                 file.path(graphic_dir, 'west_playoff_odds.png')),
                       token = token)
    #until Rtweet has scheduler
    message("Delaying ", delay/2, " seconds to space tweets...")
    Sys.sleep(delay/2)

  }

  # if(lubridate::day(Sys.Date()) == 1 & in_reg_season){
  #   tweetPace(token = token, delay = delay, graphic_dir = graphic_dir)
  # }
  #
  # if(lubridate::wday(lubridate::now()) == 1 & in_reg_season) {
  #   #On Sunday post metrics
  #   tweetMetrics(token = token)
  # }
  #
  # if(lubridate::wday(lubridate::now()) == 3 & in_reg_season) {
  #   #On Tuesday post expected points (likelyhood)
  #   tweetLikelihoods(delay = delay, graphic_dir = graphic_dir, token = token)
  # }
}

#' Tweet Pace Plots
#'
#' @param delay Delay between posted tweets
#' @param graphic_dir The graphics directory
#' @param subdir The pace subdirectory in graphics
#' @param prediction_dir The predictions directory
#' @param token rtweet token
#' @param scores HockeyModel::scores or a custom value
#' @param teamColours HockeyModel::teamColours or a custom value
#'
#' @export
tweetPace<-function(delay = 60*5, graphic_dir = "./prediction_results/graphics/", subdir = "pace", prediction_dir = "./prediction_results/", token = rtweet::get_token(), scores = HockeyModel::scores, teamColours = HockeyModel::teamColours){
  #make sure we're working with the most up-to-date info.
  scores<-updateScores(last_playoffs = Sys.Date() > as.Date("2020-04-04"))

  #Make Pace Plots
  plot_pace_by_team(graphic_dir = graphic_dir, subdir = subdir, prediction_dir = prediction_dir, scores = scores)

  filelist<-list.files(path = prediction_dir)
  pdates<-substr(filelist, 1, 10)  # gets the dates list of prediction
  pdates<-pdates[pdates != 'graphics']
  lastp<-as.Date(max(pdates))
  current_preds<-readRDS(file.path(prediction_dir, paste0(lastp,"-predictions.RDS")))
  preds<-readRDS(file.path(prediction_dir, paste0(getCurrentSeasonStartDate(), "-predictions.RDS")))
  scores<-scores[scores$Date > as.Date(getCurrentSeasonStartDate()), ]

  teamlist<-unique(preds$Team)

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
                       token = token)

    #until Rtweet has scheduler
    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)
  }
  pacediff<-data.frame("Team" = current_preds$Team, "Initial" = preds$meanPoints, "Current" = current_preds$meanPoints, stringsAsFactors = FALSE)
  pacediff$Diff <- pacediff$Current - pacediff$Initial
  maxdiff<-max(pacediff$Diff)
  mindiff<-min(pacediff$Diff)
  maxteam<-pacediff[which.max(pacediff$Diff), 'Team']
  minteam<-pacediff[which.min(pacediff$Diff), 'Team']

  recapstatus <- paste0("To recap - ",
                        "\nFurthest above expectation: ", maxteam, " ", teamColours[teamColours$Team == maxteam, "Hashtag"],
                        "\nFurthest below expectation: ", minteam, " ", teamColours[teamColours$Team == minteam, "Hashtag"])
  rtweet::post_tweet(status = recapstatus,
                     in_reply_to_status_id = reply_id,
                     token = token)
  my_timeline<-rtweet::get_timeline(user = 'BulsinkB', token = token)
  reply_id<-my_timeline$status_id[1]
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
tweetLikelihoods <- function(delay = 60*5, graphic_dir = "./prediction_results/graphics/", subdir = "pace", token = rtweet::get_token(), scores = HockeyModel::scores) {
  #make likelihood plots
  plot_point_likelihood(graphic_dir = graphic_dir, subdir = subdir, scores = scores)

  #Tweet them out
  rtweet::post_tweet(status = "#NHL Eastern Conference Team final point likelihoods:",
                     media = file.path(graphic_dir,
                                       subdir,
                                       'eastlikelihood.png'),
                     token = token)
  #until Rtweet has scheduler
  message("Delaying ", delay/2, " seconds to space tweets...")
  Sys.sleep(delay/2)

  rtweet::post_tweet(status = "#NHL Western Conference Team final point likelihoods:",
                     media = file.path(graphic_dir,subdir,'westlikelihood.png'),
                     token = token)
}

#' Tweet Game Plots
#'
#' @param games Games to tweet graphics from
#' @param delay Delay between tweets
#' @param graphic_dir the graphics directory
#' @param m HockeyModel::m or a custom value
#' @param rho HockeyModel::rho or a custom value
#' @param token the token for rtweet
#' @param teamColours HockeyModel::teamColours or a custom value
#'
#' @export
tweetGames<-function(games = HockeyModel::schedule[HockeyModel::schedule$Date == Sys.Date(), ], delay = 60*10, graphic_dir = "./prediction_results/graphics/", m = HockeyModel::m, rho = HockeyModel::rho, token = rtweet::get_token(), teamColours = HockeyModel::teamColours){
  #Tweet each game
  if(!dir.exists(graphic_dir)){
    dir.create(graphic_dir, recursive = TRUE)
  }

  if(nrow(games) == 0){
    stop("No games to tweet")
  }

  teamColours <- HockeyModel::teamColours

  for(g in 1:nrow(games)){
    home<-as.character(games[g,"HomeTeam"])
    away<-as.character(games[g,"AwayTeam"])
    plt<-plot_game(home = home, away = away, m=m, rho=rho)
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
#' @param series series data
#' @param graphic_dir directory to save the image
#' @param token rtweet token
#'
#' @return NULL
#' @export
tweetSeries<-function(series = HockeyModel::series, token = rtweet::get_token(), graphic_dir = "./prediction_results/graphics/"){
  while(grDevices::dev.cur()!=1){
    grDevices::dev.off()
  }
  plt<-plot_playoff_series_odds(series = series)
  grDevices::png(filename = file.path(graphic_dir, 'series_odds.png'), width = 11, height = 8.5, units = 'in', res = 300)
  print(plt)
  while(grDevices::dev.cur()!=1){
    grDevices::dev.off()
  }

  status <- paste0("#NHL Playoff Series Odds before games on ", Sys.Date(), " #HockeyTwitter #StanleyCup")

  rtweet::post_tweet(status = status,
                     media = file.path(graphic_dir, 'series_odds.png'),
                     token = token)
}

#' Tweet Playoff Odds
#'
#' @description Tweet the formattable graphics with each team's odds of each round/cup win
#'
#' @param playoffOdds During COVID, pass covid_play_in_solver(), afterwards change to all_results = all_results
#'
#' @param token rtweet token
#' @param graphic_dir directory to save images
#'
#' @return NULL
#' @export
tweetPlayoffOdds<-function(playoffOdds=covid_play_in_solver(), token = rtweet::get_token(), graphic_dir = "./prediction_results/graphics/"){
  plts<-playoffSolver(p0=playoffOdds)

  #after COVID use this instead:
  #plts <- playoffSolver(all_results = playoffOdds)

  export_formattable(f=plts$east, file = file.path(graphic_dir, "east_playoff_odds.png"))
  export_formattable(f=plts$west, file = file.path(graphic_dir, "west_playoff_odds.png"))

  status<- paste0("#NHL #StanleyCup Odds before games on ", Sys.Date(), ". #HockeyTwitter")

  #Posting Tweet
  rtweet::post_tweet(status = status,
                     media = c(file.path(graphic_dir, "east_playoff_odds.png"), file.path(graphic_dir, "west_playoff_odds.png")),
                     token = token)

}
