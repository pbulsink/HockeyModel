#Hosts dixon-coles specific functions. Many copied from my work from pbulsink.github.io
# see http://opisthokonta.net/?p=913

updateDC <- function(){NULL}

plotDC <- function(){NULL}

todayDC <- function(){NULL}

playoffDC <- function(){NULL}

tau_singular <- function(xx, yy, lambda, mu, rho) {
  if (xx == 0 & yy == 0) {
    return(1 - (lambda * mu * rho))
  } else if (xx == 0 & yy == 1) {
    return(1 + (lambda * rho))
  } else if (xx == 1 & yy == 0) {
    return(1 + (mu * rho))
  } else if (xx == 1 & yy == 1) {
    return(1 - rho)
  } else {
    return(1)
  }
}

tau <- Vectorize(tau_singular, c('xx', 'yy', 'lambda', 'mu'))



#' Fast Dixon-Coles model fitting 'm'.
#'
#' @description Produces a
#'
#' @param scores the historical scores to evaluate
#'
#' @return a model 'm' of dixon coles type parameters.
#' @export
doFastFit <- function(scores=scores) {
  df.indep <- data.frame(
    Date = c(scores$Date, scores$Date),
    Weight = c(DCweights(dates = scores$Date, currentDate = Sys.Date(), xi = 0.002), DCweights(dates = scores$Date, currentDate = Sys.Date(), xi = 0.002)),
    Team = as.factor(c(as.character(scores$HomeTeam), as.character(scores$AwayTeam))),
    Opponent = as.factor(c(as.character(scores$AwayTeam), as.character(scores$HomeTeam))),
    Goals = c(scores$HomeGoals, scores$AwayGoals),
    Home = c(rep(1, nrow(scores)), rep(0, nrow(scores)))
  )
  #df.indep[df.indep$Weight < 1e-15, ]$Weight <- 0
  m <- stats::glm(Goals ~ Team + Opponent + Home,
                  data = df.indep,
                  weights = df.indep$Weight,
                  family = stats::poisson(link = log),
                  start = rep(0.01, length(unique(df.indep$Team))*2)
  )
  return(m)
}

#' Fast DC model
#'
#' @param m result from doFastFit
#' @param scores the historical scores to evaluate
#'
#' @return a res object
#' @export
doFastDC <- function(m = NULL, scores=scores) {
  if (is.null(m)){
    doFastFit(scores)
  }
  expected <- stats::fitted(m)
  home.expected <- as.vector(expected[1:nrow(scores)])
  away.expected <- as.vector(expected[(nrow(scores) + 1):(nrow(scores) * 2)])
  weights<-m$data$Weight[1:nrow(scores)]

  DCoptimRhoFn.fast <- function(par) {
    rho <- par[1]
    DClogLik(y1 = scores$HomeGoals, y2 = scores$AwayGoals, mu = home.expected, lambda = away.expected, rho = rho, weights = weights)
  }

  res <- stats::optim(par = c(0.1),
                      fn = DCoptimRhoFn.fast,
                      #control = list(fnscale = -1),
                      method = "BFGS")
  return(res)
}

#' DC Predict home/draw/away win
#'
#' @description Using Dixon Coles technique, predict odds each of home win, draw, and away win.
#'
#' @param m result from doFastFit
#' @param res result from doFastDC
#' @param home home team
#' @param away away team
#' @param maxgoal max number of goals per team
#'
#' @return a list of home win, draw, and away win probability
#' @export
fastDCPredict <- function(home, away, m = NULL, res = NULL, maxgoal = 8, scores = scores) {
  if(is.null(m)){
    m <- doFastFit(scores = scores)
  }
  if(is.null(res)){
    res <- doFastDC(m=m, scores=scores)
  }
  # Expected goals home
  lambda <- stats::predict(m, data.frame(Home = 1, Team = home, Opponent = away), type = "response")

  # Expected goals away
  mu <- stats::predict(m, data.frame(Home = 0, Team = away, Opponent = home), type = "response")

  probability_matrix <- stats::dpois(0:maxgoal, lambda) %*% t(stats::dpois(0:maxgoal, mu))

  scaling_matrix <- matrix(tau(c(0, 1, 0, 1), c(0, 0, 1, 1), lambda, mu, res$par), nrow = 2)
  probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix

  HomeWinProbability <- sum(probability_matrix[lower.tri(probability_matrix)])
  DrawProbability <- sum(diag(probability_matrix))
  AwayWinProbability <- sum(probability_matrix[upper.tri(probability_matrix)])

  return(c(HomeWinProbability, DrawProbability, AwayWinProbability))
}

#' dc weight
#'
#' @param dates list of dates to calculate weights.
#' @param currentDate date from which to calculate weight
#' @param xi tuning factor
#'
#' @return list of weights coresponding to dates
#' @keywords internal
DCweights <- function(dates, currentDate = Sys.Date(), xi = 0) {
  datediffs <- dates - as.Date(currentDate)
  datediffs <- as.numeric(datediffs * -1)
  w <- exp(-1 * xi * datediffs)
  w[datediffs <= 0] <- 0  #Future dates should have zero weights
  return(w)
}

DClogLik <- function(y1, y2, lambda, mu, rho = 0, weights = NULL) {
  # rho=0, independence y1 home goals y2 away goals mu:expected Home, lambda: expected Away
  t <- tau(y1, y2, lambda, mu, rho)
  loglik <- log(t) + log(stats::dpois(y1, lambda)) + log(stats::dpois(y2, mu))
  if (is.null(weights)) {
    return(sum(loglik, na.rm = TRUE))
  } else {
    return(sum(loglik * weights, na.rm = TRUE))
  }
}
