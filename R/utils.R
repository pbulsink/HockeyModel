#' Clean a model
#'
#' @description reduce the size of a model for better long-term storage.
#'
#' @param cm model to clean
#'
#' @return a smaller model, ready for saving
cleanModel <- function(cm) {
  #from http://www.win-vector.com/blog/2014/05/trimming-the-fat-from-glm-models-in-r/
  cm$y <- c()
  cm$model <- c()

  cm$residuals <- c()
  cm$effects <- c()
  cm$qr$qr <- c()
  cm$linear.predictors <- c()
  cm$weights <- c()
  cm$prior.weights <- c()

  cm
}


#' Normalize Odds
#'
#' @param odds a vector of odds to normalize
#'
#' @return odds summing to 1
#' @export
normalizeOdds<-function(odds){
  odds<-unlist(odds)
  odds[odds>1]<-1-1e-10
  odds[odds<0]<-1e-10
  odds<-odds/sum(odds)
  return(odds)
}


#' Get Historical Points for all teams listed in a scores frame
#'
#' @param sc scores frame
#'
#' @return a tibble of season points for each team that season(s)
historicalPoints<-function(sc){
  points<-tibble::tibble(Team = character(), Season = character(), Points = integer())

  sc$Season<-getSeason(sc$Date)

  sc<-droplevels(sc)

  for (i in unique(sc$Season)){
    if(i == "20122013"){
      next
    }

    s<-sc[sc$Season == i & sc$GameType == "R",]
    b<-buildStats(s)
    b$Season <- i

    points<-dplyr::bind_rows(points, b[,colnames(b) %in% c('Team', 'Season', 'Points')])

  }

  return(points)
}


#' Conditional Mutate
#' @description Mutate at condition. useful in dplyr pipes. From StackOverflow https://stackoverflow.com/a/34096575/3933405
#'
#' @param .data Data passed in
#' @param condition Condition whether to peform mutate
#' @param ... mutate to happen
#' @param envir environment to cary through.
#' @export
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% dplyr::mutate(...)
  .data
}


#' Log Loss Calculator
#'
#' @param predicted Predicted odds of an event occuring
#' @param actual If the event occured (0 or 1), or model results in 0, 0.25, 0.4, 0.6, 0.75, 1.0
#'
#' @return a log loss value for the event(s)
#' @export
logLoss<-function(predicted, actual){
  stopifnot(length(predicted) == length(actual))
  predicted[predicted == 0]<-1e-15
  predicted[predicted == 1]<-1-1e-15

  actual <- as.numeric(actual > 0.5)

  ll<-(actual * log(predicted)) + ((1-actual) * log(1-predicted))

  return(-(sum(ll)/length(predicted)))
}


#' Accuracy Calculator
#'
#' @param predicted Predicted odds of an event occuring. needen't be of set {0,1}
#' @param actual If the event occured (0 or 1), or model results in 0, 0.25, 0.4, 0.6, 0.75, 1.0
#'
#' @return a percentage of correct predictions
#' @export
accuracy<-function(predicted, actual){
  stopifnot(length(predicted) == length(actual))

  predicted <- as.numeric(predicted > 0.5)
  actual <- as.numeric(actual > 0.5)

  accuracy <- sum(as.numeric(predicted == actual))/length(predicted)

  return(accuracy)
}


#'AUC
#'@description calculate the AUC metrics. From MLMetrics
#'
#' @param predicted Predicted odds of an event occuring. needen't be of set {0,1}
#' @param actual If the event occured (0 or 1), or model results in 0, 0.25, 0.4, 0.6, 0.75, 1.0
#'
#' @return a single value for auc
#' @export
auc<-function(predicted, actual){
  stopifnot(length(predicted) == length(actual))

  actual <- as.numeric(actual > 0.5)

  rank<- rank(predicted)
  n_positive<- sum(actual > 0.5)
  n_negative<- sum(actual < 0.5)

  auc <- (sum(rank[actual > 0.5]) - n_positive * (n_positive + 1)/2)/(n_positive * n_negative)

  return(auc)
}


#'RMSE
#'@description calculate the RMSE metrics. From MLMetrics
#'
#' @param predicted Predicted numeric value
#' @param actual Actual numeric value
#'
#' @return a single value for RMSE
#' @export
rmse<-function(predicted, actual){
  stopifnot(length(predicted) == length(actual))

  return(sqrt(mean((actual-predicted)^2)))
}


#'R Square
#'@description calculate the R^2 metrics. From MLMetrics
#'
#' @param predicted Predicted numeric value
#' @param actual Actual numeric value
#'
#' @return a single value for R^2
#' @export
rsquare<-function(predicted, actual){
  stopifnot(length(predicted) == length(actual))

  return(stats::cor(predicted, actual) ^ 2)
}

#'MSE
#'@description calculate the MSE metrics. From MLMetrics
#'
#' @param predicted Predicted numeric value
#' @param actual Actual numeric value
#'
#' @return a single value for MSE
#' @export
mse <- function(predicted, actual){
  stopifnot(length(predicted) == length(actual))

  return(mean((actual-predicted)^2))
}

#' Check Colour Delta
#'
#' @description Check the similarity of two colours by hex code. 0 = identical, 1 = opposite (black & white)
#'
#' @param hex1 colour one, as hex code #XXXXXX
#' @param hex2 colour two, as hex code #XXXXXX
#'
#' @return a value from 0 (identical) to 1 (completely opposite, black & white)
#' @export
#' @examples
#' #colour similarity between FLA and TBL primary colours
#' colourDelta("#041E42","#002868")
colourDelta<-function(hex1, hex2){
  c1 <- hexToRGB(hex1)/255
  c2 <- hexToRGB(hex2)/255
  delta <- abs(c1-c2)
  deltaM <- mean(delta)
  deltaM
}


#' Hex to RGB
#'
#' @description convert Hex colours to RGB colours
#'
#' @param hex colour as hex code #XXXXXX
#'
#' @return vector of three numbers, R, G, B from 0 to 255
#' @export
#' @examples
#' hexToRGB("#FFFFFF")
hexToRGB <- function(hex){
  r<-strtoi(paste0("0x",substr(hex, 2,3)))
  g<-strtoi(paste0("0x",substr(hex, 4,5)))
  b<-strtoi(paste0("0x",substr(hex, 6,7)))
  return(c(r,g,b))
}

formatPredsForHockeyVisContest<-function(predictions, candyType = 'Fuzzy Peaches', handle='@bot.bulsink.ca'){
  #IneffectiveMath's (Micah's) contest
  #format = {'handle':'???','preferredSourCandyType':'???', 'Predictions':{ 'ANA':(m,u), 'ARI':(m,u), 'BOS':(m,u), 'BUF':(m,u), 'CAR':(m,u), 'CBJ':(m,u), 'CGY':(m,u), 'CHI':(m,u), 'COL':(m,u), 'DAL':(m,u), 'DET':(m,u), 'EDM':(m,u), 'FLA':(m,u), 'L.A':(m,u), 'MIN':(m,u), 'MTL':(m,u), 'N.J':(m,u), 'NSH':(m,u), 'NYI':(m,u), 'NYR':(m,u), 'OTT':(m,u), 'PHI':(m,u), 'PIT':(m,u), 'S.J':(m,u), 'STL':(m,u), 'T.B':(m,u), 'TOR':(m,u), 'VAN':(m,u), 'VGK':(m,u), 'WPG':(m,u), 'WSH':(m,u), } }
  output = paste0("{'handle':'", handle,
                  "', 'preferredSourCandyType':'", candyType,
                  "', 'Predictions':{ ",
                  "'ANA':(", round(predictions[predictions$Team == "Anaheim Ducks",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Anaheim Ducks",]$sdPoints, 2), "), ",
                  "'BOS':(", round(predictions[predictions$Team == "Boston Bruins",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Boston Bruins",]$sdPoints,  2), "), ",
                  "'BUF':(", round(predictions[predictions$Team == "Buffalo Sabres",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Buffalo Sabres",]$sdPoints, 2), "), ",
                  "'CAR':(", round(predictions[predictions$Team == "Carolina Hurricanes",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Carolina Hurricanes",]$sdPoints, 2), "), ",
                  "'CBJ':(", round(predictions[predictions$Team == "Columbus Blue Jackets",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Columbus Blue Jackets",]$sdPoints, 2), "), ",
                  "'CGY':(", round(predictions[predictions$Team == "Calgary Flames",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Calgary Flames",]$sdPoints, 2), "), ",
                  "'CHI':(", round(predictions[predictions$Team == "Chicago Blackhawks",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Chicago Blackhawks",]$sdPoints, 2), "), ",
                  "'COL':(", round(predictions[predictions$Team == "Colorado Avalanche",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Colorado Avalanche",]$sdPoints, 2), "), ",
                  "'DAL':(", round(predictions[predictions$Team == "Dallas Stars",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Dallas Stars",]$sdPoints, 2), "), ",
                  "'DET':(", round(predictions[predictions$Team == "Detroit Red Wings",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Detroit Red Wings",]$sdPoints, 2), "), ",
                  "'EDM':(", round(predictions[predictions$Team == "Edmonton Oilers",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Edmonton Oilers",]$sdPoints, 2), "), ",
                  "'FLA':(", round(predictions[predictions$Team == "Florida Panthers",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Florida Panthers",]$sdPoints, 2), "), ",
                  "'L.A':(", round(predictions[predictions$Team == "Los Angeles Kings",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Los Angeles Kings",]$sdPoints, 2), "), ",
                  "'MIN':(", round(predictions[predictions$Team == "Minnesota Wild",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Minnesota Wild",]$sdPoints, 2), "), ",
                  "'MTL':(", round(predictions[predictions$Team == "Montreal Canadiens",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Montreal Canadiens",]$sdPoints, 2), "), ",
                  "'N.J':(", round(predictions[predictions$Team == "New Jersey Devils",]$meanPoints, 1), ",", round(predictions[predictions$Team == "New Jersey Devils",]$sdPoints, 2), "), ",
                  "'NSH':(", round(predictions[predictions$Team == "Nashville Predators",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Nashville Predators",]$sdPoints, 2), "), ",
                  "'NYI':(", round(predictions[predictions$Team == "New York Islanders",]$meanPoints, 1), ",", round(predictions[predictions$Team == "New York Islanders",]$sdPoints, 2), "), ",
                  "'NYR':(", round(predictions[predictions$Team == "New York Rangers",]$meanPoints, 1), ",", round(predictions[predictions$Team == "New York Rangers",]$sdPoints, 2), "), ",
                  "'OTT':(", round(predictions[predictions$Team == "Ottawa Senators",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Ottawa Senators",]$sdPoints, 2), "), ",
                  "'PHI':(", round(predictions[predictions$Team == "Philadelphia Flyers",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Philadelphia Flyers",]$sdPoints, 2), "), ",
                  "'PIT':(", round(predictions[predictions$Team == "Pittsburgh Penguins",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Pittsburgh Penguins",]$sdPoints, 2), "), ",
                  "'S.J':(", round(predictions[predictions$Team == "San Jose Sharks",]$meanPoints, 1), ",", round(predictions[predictions$Team == "San Jose Sharks",]$sdPoints, 2), "), ",
                  "'SEA':(", round(predictions[predictions$Team == "Seattle Kraken",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Seattle Kraken",]$sdPoints, 2), "), ",
                  "'STL':(", round(predictions[predictions$Team == "St. Louis Blues",]$meanPoints, 1), ",", round(predictions[predictions$Team == "St. Louis Blues",]$sdPoints, 2), "), ",
                  "'T.B':(", round(predictions[predictions$Team == "Tampa Bay Lightning",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Tampa Bay Lightning",]$sdPoints, 2), "), ",
                  "'TOR':(", round(predictions[predictions$Team == "Toronto Maple Leafs",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Toronto Maple Leafs",]$sdPoints, 2), "), ",
                  "'UTA':(", round(predictions[predictions$Team == "Utah Hockey Club",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Utah Hockey Club",]$sdPoints,  2), "), ",
                  "'VAN':(", round(predictions[predictions$Team == "Vancouver Canucks",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Vancouver Canucks",]$sdPoints, 2), "), ",
                  "'VGK':(", round(predictions[predictions$Team == "Vegas Golden Knights",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Vegas Golden Knights",]$sdPoints, 2), "), ",
                  "'WPG':(", round(predictions[predictions$Team == "Winnipeg Jets",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Winnipeg Jets",]$sdPoints, 2), "), ",
                  "'WSH':(", round(predictions[predictions$Team == "Washington Capitals",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Washington Capitals",]$sdPoints, 2), "), ",
                  "} }"
  )
  return(output)
}

#' Validate GameID numbers
#'
#' @param gameIDs a single game ID or vector of game IDs
#'
#' @return TRUE if gameIDs is valid, or FALSE if gameIDs is not (or a vector of length of input of TRUE/FALSE
#' @export
gameIDValidator<-function(gameIDs){
  return(grepl("(19|20)\\d{2}0[1-4][0-1]\\d{3}", gameIDs))
}

#' Is this a Date?
#'
#' @description Is this a date?
#'
#' @param date is this a date?
#'
#' @return is it a date?
#' @export
#'
#' @examples is.Date("2020-12-13"); is.Date("bob")
is.Date<-function(date){
  tryCatch(!is.na(as.Date(date)),error=function(err){FALSE})
}

seasonValidator<-function(season){
  #TODO: Currently 19272099 would pass - make sure the two years are sequential
  return(grepl("(19|20)\\d{2}(19|20)\\d{2}", as.character(season)))
}


extraTimeSolver<-function(home_win, away_win, draw){

  ets<-function(home_win, away_win, draw){
    homenorm<-normalizeOdds(c(home_win, away_win))[1]
    home_ot<-0.345*homenorm+0.315

    home_draw<-draw*home_ot
    away_draw<-draw*(1-home_ot)

    return(c(home_win, home_draw, away_draw, away_win))
  }

  v_ets<-Vectorize(ets, )
  if(length(home_win) == 1){
    return(ets(home_win = home_win, away_win = away_win, draw = draw))
  } else {
    return(t(v_ets(home_win, away_win, draw)))
  }
}



#' Add Postponed Games to Schedule End
#'
#' @description Sometimes games are postponed without a makeup date initially announced. The model just drops those games if this function is not employed to move the games to the end of the season
#' Note that the games are all dumped on one day so it doesn't account for back to back or travel days or anything.
#'
#' @param schedule the schedule to reconfigure
#'
#' @return a schedule with postponed games moved to the end of the schedule - helps to not drop games that are otherwise in the past but weren't played.
add_postponed_to_schedule_end<-function(schedule = HockeyModel::schedule){
  if(!any(schedule$GameStatus == "Postponed")){
    #no postponed games
    return(schedule)
  } else if(all(schedule[schedule$GameStatus == "Postponed", "Date"] > Sys.Date())){
    #all game postponements are in future games - just play them.
    return(schedule)
  }

  for(g in schedule[schedule$GameStatus == "Postponed" & schedule$Date < Sys.Date(), ]$GameID){
    #The model doesn't (currently) account for what games are back to back or anything - so they can all be played on the same (last) date of the schedule
    #Using the last date of the regualr sseason to not interfere with playoffs
    schedule[schedule$GameID == g, ]$Date <- max(schedule[schedule$GameType == "R",]$Date, Sys.Date())
  }
  schedule<-schedule %>%
    dplyr::arrange(.data$Date, .data$GameID)
  return(schedule)

}


gId<-function(gameId){
  if(!is.numeric(gameId)){
    return(FALSE)
  } else if (!nchar(gameId) == 10) {
    return(FALSE)
  } else if (!grepl(pattern = "20[1,2][0-9]0[2,3][0-9]{4}", gameId)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
is_valid_gameId<-Vectorize(gId)

parseCores<-function(cores){
  cores <- cores
  if(is.null(cores)){
    if (!requireNamespace('parallel', quietly = TRUE)){
      message("Parallel package must be installed to use multi-core processing.")
      cores<-1
    } else {
      cores<-parallel::detectCores()
    }
  } else {
    if(!requireNamespace('parallel', quietly = TRUE)){
      message("Parallel package must be installed to use multi-core processing.")
      cores <- 1
    } else if (!is.numeric(cores) | cores%%1 != 0 | cores <= 0) {
      message("Cores must be a integer number")
      cores <- 1
    } else if (cores > parallel::detectCores()){
      cores <- parallel::detectCores()
    }
  }
  return(cores)
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.HockeyModel <- NULL
  if(requireNamespace('devtools', quietly = TRUE)){
    packagefile<-NULL
    packagefile<-tryCatch(devtools::package_file(), error = function(e) return(NULL))
    if(is.null(packagefile)){
      op.HockeyModel <- NULL
    } else {
      op.HockeyModel <- list(HockeyModel.prediction.path = file.path(devtools::package_file(), "prediction_results"),
                             HockeyModel.graphics.path = file.path(devtools::package_file(), "prediction_results", "graphics"),
                             HockeyModel.data.path = file.path(devtools::package_file(), 'data-raw'))
    }
  }
  if(is.null(op.HockeyModel)){
    op.HockeyModel <- list(HockeyModel.prediction.path = file.path(path.expand("~"), "HockeyModel","prediction_results"),
                           HockeyModel.graphics.path = file.path(path.expand("~"), "HockeyModel","prediction_results","graphics"),
                           HockeyModel.data.path = file.path(path.expand("~"), "HockeyModel", "data-raw"))
  }

  toset <- !(names(op.HockeyModel) %in% names(op))
  if(any(toset)) options(op.HockeyModel[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  msgtext <- paste0('HockeyModel package loaded.\nUsing ', getOption("HockeyModel.prediction.path"),
                    ' as prediction path.\nTo change path, set option("HockeyModel.prediction.path" = [new path]).\n',
                    'This can be done interactively or using .RProfile to save your preference.')
  packageStartupMessage(msgtext)
}
