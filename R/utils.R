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


#' Get Current Season
#'
#' @return current season (as 20172018 format) based on today's date
#' @export
getCurrentSeason8 <- function(){
  getSeason(Sys.Date())
}


#' Get Current Season Start Date
#'
#' @return current season's nominal start date (Oct 01) as character, e.g. "2019-10-01"
#' @export
getCurrentSeasonStartDate <- function(){
  return(paste0(strtrim(getCurrentSeason8(), 4), "-10-01"))
}


#' Get Season from Game Date
#'
#' @param gamedate The date of the game to check for season
#'
#' @return a character season id (e.g. 20172018)
#' @export
getSeason <- function(gamedate=Sys.Date()){
  gs<-function(gd){
    year<-as.integer(strftime(gd, '%Y'))
    month<-as.integer(strftime(gd, '%m'))
    if(month >= 9){
      return(paste0(year,year+1))
    } else {
      return(paste0(year-1,year))
    }
  }
  vgs<-Vectorize(gs)


  if(length(gamedate) == 1){
    return(gs(gamedate))
  } else if (length(gamedate) > 1) {
    return(unname(vgs(gamedate)))
  }
}


#' Get the division of a team or vector of teams
#'
#' @param team a single team or vector of teams
#'
#' @return the division (or vector of divisions) for the team(s)
getDivision<-function(team){
  gd <- function(t){
    nhl_divisions <- HockeyModel::nhl_divisions
    d<-grep(t, nhl_divisions)
    if(length(d) != 1) {
      return (NA)
    } else {
      return(names(nhl_divisions)[d])
    }
  }

  vgd<-Vectorize(gd)

  if(length(team) == 1) {
    return(gd(team))
  } else {
    return(as.vector(vgd(team)))
  }
}


#' Get the conference of a team or vector of teams
#'
#' @param team a single team or vector of teams
#'
#' @return the conference (or vector of conferences) for the team(s)
getConference<-function(team){
  gconf <- function(t){
    nhl_conferences <- HockeyModel::nhl_conferences
    d<-grep(t, nhl_conferences)
    if(length(d) != 1) {
      return (NA)
    } else {
      return(names(nhl_conferences)[d])
    }
  }

  vgconf<-Vectorize(gconf)

  if(length(team) == 1) {
    return(gconf(team))
  } else {
    return(as.vector(vgconf(team)))
  }
}


#' Get a Team's short code
#'
#' @param team a single team or vector of teams
#'
#' @return the team's (or teams') short code(s)
getShortTeam<-function(team){
  team_short<-list(
    "Anaheim Ducks" = "ANA", "Arizona Coyotes" = "ARI", "Boston Bruins" = "BOS", "Buffalo Sabres" ="BUF",
    "Calgary Flames" = "CGY", "Carolina Hurricanes" = "CAR", "Chicago Blackhawks" = "CHI",
    "Colorado Avalanche" = "COL", "Columbus Blue Jackets" = "CBJ", "Dallas Stars" = "DAL",
    "Detroit Red Wings" = "DET", "Edmonton Oilers" = "EDM", "Florida Panthers" = "FLA",
    "Los Angeles Kings" = "LAK","Minnesota Wild" = "MIN", "Montreal Canadiens" = "MTL",
    "Nashville Predators" = "NSH", "New Jersey Devils" = "NJD", "New York Islanders" = "NYI",
    "New York Rangers" = "NYR", "Ottawa Senators" = "OTT", "Philadelphia Flyers" = "PHI",
    "Pittsburgh Penguins" = "PIT", "San Jose Sharks" = "SJS", "St. Louis Blues" = "STL",
    "Tampa Bay Lightning" = "TBL", "Toronto Maple Leafs" = "TOR", "Vancouver Canucks" = "VAN",
    "Vegas Golden Knights" = "VGK","Washington Capitals" = "WSH", "Winnipeg Jets" = "WPG"
  )
  ts<-function(t){
    return(team_short[[t]])
  }

  vts<-Vectorize(ts)

  if(length(team) == 1){
    return(ts(team))
  } else {
    return(as.vector(vts(team)))
  }
}


#' Normalize Odds
#'
#' @param odds a vector of odds to normalize
#'
#' @return odds summing to 1
#' @export
normalizeOdds<-function(odds){
  odds<-unlist(odds)
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

formatPredsForHockeyVisContest<-function(predictions, candyType = 'Fuzzy Peaches', handle='pbulsink'){
  #IneffectiveMath's (Micah's) contest
  #format = {'handle':'???','preferredSourCandyType':'???', 'Predictions':{ 'ANA':(m,u), 'ARI':(m,u), 'BOS':(m,u), 'BUF':(m,u), 'CAR':(m,u), 'CBJ':(m,u), 'CGY':(m,u), 'CHI':(m,u), 'COL':(m,u), 'DAL':(m,u), 'DET':(m,u), 'EDM':(m,u), 'FLA':(m,u), 'L.A':(m,u), 'MIN':(m,u), 'MTL':(m,u), 'N.J':(m,u), 'NSH':(m,u), 'NYI':(m,u), 'NYR':(m,u), 'OTT':(m,u), 'PHI':(m,u), 'PIT':(m,u), 'S.J':(m,u), 'STL':(m,u), 'T.B':(m,u), 'TOR':(m,u), 'VAN':(m,u), 'VGK':(m,u), 'WPG':(m,u), 'WSH':(m,u), } }
  output = paste0("{'handle':'", handle,
                  "', 'preferredSourCandyType':'", candyType,
                  "', 'Predictions':{ ",
                  "'ANA':(", round(predictions[predictions$Team == "Anaheim Ducks",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Anaheim Ducks",]$sdPoints, 2), "), ",
                  "'ARI':(", round(predictions[predictions$Team == "Arizona Coyotes",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Arizona Coyotes",]$sdPoints,  2), "), ",
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
                  "'SEA':(", round(predictions[predictions$Team == "Seattle Kracken",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Seattle Kracken",]$sdPoints, 2),
                  "'STL':(", round(predictions[predictions$Team == "St. Louis Blues",]$meanPoints, 1), ",", round(predictions[predictions$Team == "St. Louis Blues",]$sdPoints, 2), "), ",
                  "'T.B':(", round(predictions[predictions$Team == "Tampa Bay Lightning",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Tampa Bay Lightning",]$sdPoints, 2), "), ",
                  "'TOR':(", round(predictions[predictions$Team == "Toronto Maple Leafs",]$meanPoints, 1), ",", round(predictions[predictions$Team == "Toronto Maple Leafs",]$sdPoints, 2), "), ",
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
