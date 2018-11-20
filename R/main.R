#Hosts main functions
#Calls to data and preciction calculations, produce figures & tables

#' Update Model
#'
#' @param ... Items to pass to update functions.
#'
#' @export
updateModel <- function(...){
  scores<-updateScores(...)
  schedule<-updateSchedule(...)
  dcparams<-updateDC(scores = scores, ...)
  #devtools::install(local = FALSE)
  #.rs.restartR()
  #require(HockeyModel)
  return(list(scores = scores, schedule = schedule, m = dcparams$m, rho = dcparams$rho))
}

#' Update predictions
#'
#' @param data_dir directory of predictions
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
#' @param ... Items to pass to plot functions.
#'
#' @return today's ratings ggplot object
#' @export
ratings <- function(m = HockeyModel::m, ...) {
  return(plotDC(m=m, ...))
}

tweet <- function(games, graphic_dir = './prediction_results/graphics/', token = rtweet::get_token(), delay = 60*15, ...){
  rtweet::post_tweet(status = "Predicted odds for today's #NHL games",
                     media = file.path(graphic_dir, "today_odds.png"), token = token)
  my_timeline<-rtweet::get_timeline(rtweet:::home_user(), token = token)
  reply_id<-my_timeline$status_id[1]
  rtweet::post_tweet(status = paste0("Current team ratings (as of ", Sys.Date(), ")."),
                     media = file.path(graphic_dir, "current_rating.png"),
                     in_reply_to_status_id = reply_id, token = token)

  #until Rtweet has scheduler
  message("Delaying ", delay, " seconds to space tweets...")
  Sys.sleep(delay)

  rtweet::post_tweet(status = paste0("Predicted points for #NHL teams (before games on ", Sys.Date(), ")."),
                     media = file.path(graphic_dir, "point_predict.png"), token = token)

  my_timeline<-rtweet::get_timeline(rtweet:::home_user(), token = token)
  reply_id<-my_timeline$status_id[1]

  #until Rtweet has scheduler
  message("Delaying ", delay, " seconds to space tweets...")
  Sys.sleep(delay)

  rtweet::post_tweet(status = paste0("Playoff odds for #NHL teams (before games on ", Sys.Date(), ")."),
                     media = file.path(graphic_dir, "playoff_odds.png"),
                     in_reply_to_status_id = reply_id, token = token)

  my_timeline<-rtweet::get_timeline(rtweet:::home_user(), token = token)
  reply_id<-my_timeline$status_id[1]

  #until Rtweet has scheduler
  message("Delaying ", delay, " seconds to space tweets...")
  Sys.sleep(delay)

  rtweet::post_tweet(status = paste0("President's trophy odds for #NHL teams (before games on ", Sys.Date(), ")."),
                     media = file.path(graphic_dir, "president_odds.png"),
                     in_reply_to_status_id = reply_id, token = token)

}

#' Daily functions, rolled into one call
#'
#' @param ... Items to pass to update and plot functions.
#'
#' @export
dailySummary <- function(graphic_dir = './prediction_results/graphics/', token = rtweet::get_token(), ...){
  #message("Reminder, run updateModel() first.")
  #Sys.sleep(5)
  modelparams<-updateModel(...)
  updatePredictions(scores = modelparams$scores, schedule = modelparams$schedule)

  if(!dir.exists(graphic_dir)){
    dir.create(graphic_dir, recursive = TRUE)
  }
  sc<-modelparams$schedule
  if(Sys.Date() %in% sc$Date){
    message("Creating graphics...")
    today <- todayOdds(rho = modelparams$rho, m = modelparams$m, schedule = modelparams$schedule, scores = modelparams$scores, ...)
    ggplot2::ggsave(file.path(graphic_dir, 'today_odds.png'), plot = today, width = 11, height = 8.5, units = "in")

    playoff <- playoffOdds(...)
    ggplot2::ggsave(file.path(graphic_dir, 'playoff_odds.png'), plot = playoff, width = 11, height = 8.5, units = "in")

    president <- presidentOdds(...)
    ggplot2::ggsave(file.path(graphic_dir, 'president_odds.png'), plot = president, width = 11, height = 8.5, units = "in")

    point <- pointPredict(...)
    ggplot2::ggsave(file.path(graphic_dir, 'point_predict.png'), plot = point, width = 11, height = 8.5, units = "in")

    rating <- ratings(m = modelparams$m)
    ggplot2::ggsave(file.path(graphic_dir, 'current_rating.png'), plot = rating, width = 11, height = 8.5, units = "in")

    message("Posting Tweets...")
    tweet(graphic_dir, token = token, ...)
    #until Rtweet has scheduler
    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)

    tweetGames(games = sc[sc$Date == Sys.Date(), ], m = modelparams$m, rho = modelparams$rho, graphic_dir = graphic_dir, token = token)

  }
}

#' Tweet Pace Plots
#'
#' @param delay Delay between posted tweets
#' @param graphic_dir The graphics directory
#' @param subdir The pace subdirectory in graphics
#' @param prediction_dir The predictions directory
#'
#' @export
tweetPace<-function(delay = 60*5, graphic_dir = "./prediction_results/graphics/", subdir = "pace", prediction_dir = "./prediction_results/", token = rtweet::get_token(), scores = HockeyModel::scores){
  #make sure we're working with the most up-to-date info.
  scores<-updateScores()

  #Make Pace Plots
  plot_pace_by_team(graphic_dir = graphic_dir, subdir = subdir, prediction_dir = prediction_dir, scores = scores)

  filelist<-list.files(path = prediction_dir)
  pdates<-substr(filelist, 1, 10)  # gets the dates list of prediction
  pdates<-pdates[pdates != 'graphics']
  lastp<-as.Date(max(pdates))
  current_preds<-readRDS(file.path(prediction_dir, paste0(lastp,"-predictions.RDS")))
  preds<-readRDS(file.path(prediction_dir, "2018-10-03-predictions.RDS"))
  scores<-scores[scores$Date > as.Date("2018-10-03"), ]

  teamlist<-unique(preds$Team)

  rtweet::post_tweet(paste0("Here are team pace plots as of ", Sys.Date(), ". Plots show original predicted points range and current predicted range."))

  my_timeline<-rtweet::get_timeline(rtweet:::home_user(), token = token)
  reply_id<-my_timeline$status_id[1]

  for(team in teamlist){
    ngames <- sum(sum(scores$HomeTeam == team), sum(scores$AwayTeam == team))
    status<-paste0(team,
                   " pace after ",
                   ngames,
                   " games. Originally predicted ",
                   format(round(as.numeric(preds[preds$Team == team, 'meanPoints'], digits = 1)), nsmall = 1),
                   " points, now on pace for ",
                   format(round(as.numeric(current_preds[current_preds$Team == team, 'meanPoints'], 1)), nsmall = 1),
                   ". ",
                   teamColours[teamColours$Team == team, "Hashtag"])

    rtweet::post_tweet(status = status,
                       media = file.path(graphic_dir,
                                         subdir,
                                         paste0(tolower(gsub(" ", "_", team)), '.png')),
                       in_reply_to_status_id = reply_id,
                       token = token)
    my_timeline<-rtweet::get_timeline(rtweet:::home_user(), token = token)
    reply_id<-my_timeline$status_id[1]

    #until Rtweet has scheduler
    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)
  }
}

#' Tweet Game Plots
#'
#' @param games Games to tweet graphics from
#' @param delay Delay between tweets
#' @param graphic_dir the graphics directory
#'
#' @export
tweetGames<-function(games = HockeyModel::schedule[HockeyModel::schedule$Date == Sys.Date(), ], delay = 60*15, graphic_dir = "./prediction_results/graphics/", m = HockeyModel::m, rho = HockeyModel::rho, token = rtweet::get_token()){
  #Tweet each game
  if(!dir.exists(graphic_dir)){
    dir.create(graphic_dir, recursive = TRUE)
  }

  for(g in 1:nrow(games)){
    home<-games[g,"HomeTeam"]
    away<-games[g,"AwayTeam"]
    plt<-plot_game(home = home, away = away, m=m, rho=rho)
    ggplot2::ggsave(file.path(graphic_dir, 'predicted_goals.png'), plot = plt, width = 11, height = 8.5, units = "in")
    status<-paste0(teamColours[teamColours$Team == home, "Hashtag"], " at ", teamColours[teamColours$Team == away, "Hashtag"], " predicted goals.")
    rtweet::post_tweet(status = status,
                       media = file.path(graphic_dir, 'predicted_goals.png'),
                       token = token)

    file.remove(file.path(graphic_dir, 'predicted_goals.png'))

    #until Rtweet has scheduler
    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)
  }
}
