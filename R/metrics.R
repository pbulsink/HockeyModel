#' Recovery Calculating
#'
#' @param predicted data.frame of Team, Predicted Points, SD
#' @param actual data.frame of Team, Actual Points
#'
#' @return single value 0-31
#' @export
recovery<-function(pPoints, sdPoints, aPoints){
  return(sum(dnorm(aPoints, mean = pPoints, sd = sdPoints)))
}
