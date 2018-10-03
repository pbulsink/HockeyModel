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
    year<-as.integer(strftime(gamedate, '%Y'))
    month<-as.integer(strftime(gamedate, '%m'))
    if(month < 8){
      return(paste0(year-1,year))
    } else {
      return(paste0(year,year+1))
    }
  }
  vgs<-Vectorize(gs)


  if(length(gamedate) == 1)
    return(gs(gd))
  else if (length(gamedate) > 1) {
    return(vgs(gd))
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
