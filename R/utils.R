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
  year<-as.integer(strftime(gamedate, '%Y'))
  month<-as.integer(strftime(gamedate, '%m'))
  if(month < 8){
    return(paste0(year-1,year))
  } else {
    return(paste0(year,year+1))
  }
}

vGetSeason<-Vectorize(getSeason)

normalizeOdds<-function(odds){
  odds<-unlist(odds)
  odds<-odds/sum(odds)
  return(odds)
}
