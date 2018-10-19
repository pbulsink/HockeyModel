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

#' Get Season from Game Date
#'
#' @param gamedate The date of the game to check for season
#'
#' @return a character season id (e.g. 20172018)
#' @export
getSeason <- function(gamedate){
  gs<-function(gd){
    year<-as.integer(strftime(gd, '%Y'))
    month<-as.integer(strftime(gd, '%m'))
    if(month < 8){
      return(paste0(year-1,year))
    } else {
      return(paste0(year,year+1))
    }
  }
  vgs<-Vectorize(gs)


  if(length(gamedate) == 1){
    return(gs(gamedate))
  } else if (length(gamedate) > 1) {
    return(vgs(gamedate))
  }
}

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

historicalPoints<-function(sc){
  points<-tibble::tibble(Team = character(), Season = character(), Points = integer())

  for (i in unique(sc$Season)){
    if(i == "20122013"){
      next
    }
    if(i == "20172018"){
      ngames<-1271
    } else {
      ngames<-1230
    }

    s<-sc[sc$Season == i,]
    s<-sc[1:ngames, ]
    b<-buildStats(s)
    b$Season <- i


    points<-dplyr::bind_rows(points, b[,colnames(b) %in% c('Team', 'Season', 'Points')])

  }

  return(points)
}
