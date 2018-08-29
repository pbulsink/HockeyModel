#' Recovery Calculating
#'
#' @param pPoints vector of predcited points
#' @param aPoints vector of actual points
#' @param sdPoints vector of predicted points standard deviations
#'
#' @return single value 0-31
#' @export
recovery<-function(pPoints, sdPoints, aPoints){
  return(sum(stats::dnorm(aPoints, mean = pPoints, sd = sdPoints)))
}
