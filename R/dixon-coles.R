#Hosts dixon-coles specific functions. Many copied from my work from pbulsink.github.io
# see http://opisthokonta.net/?p=913

updateDC <- function(){NULL}

plotDC <- function(){NULL}

todayDC <- function(){NULL}

playoffDC <- function(){NULL}

tau <- Vectorize(function(xx, yy, lambda, mu, rho) {
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
})

doFastFit <- function(scores) {
  df.indep <- data.frame(
    Date = c(scores$Date, scores$Date),
    Weight = c(DCweights(dates = scores$Date, currentDate = Sys.Date(), xi = 0.002), DCweights(dates = scores$Date, currentDate = Sys.Date(), xi = 0.002)),
    Team = as.factor(c(as.character(scores$HomeTeam), as.character(scores$AwayTeam))),
    Opponent = as.factor(c(as.character(scores$AwayTeam), as.character(scores$HomeTeam))),
    Goals = c(scores$HomeGoals, scores$AwayGoals),
    Home = c(rep(1, nrow(scores)), rep(0, nrow(scores)))
  )
  m <- stats::glm(Goals ~ Home + Team + Opponent, data = df.indep, weights = df.indep$Weight, family = stats::poisson())
  return(m)
}

doFastDC <- function(m, scores) {
  expected <- stats::fitted(m)
  home.expected <- expected[1:nrow(scores)]
  away.expected <- expected[(nrow(scores) + 1):(nrow(scores) * 2)]

  DCoptimRhoFn.fast <- function(par) {
    rho <- par[1]
    DClogLik(scores$HomeGoals, scores$AwayGoals, home.expected, away.expected, rho)
  }

  res <- stats::optim(par = c(0.1), fn = DCoptimRhoFn.fast, control = list(fnscale = -1), method = "BFGS")
  return(res)
}

fastDCPredict <- function(m, res, home, away, maxgoal = 8) {
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

DCweights <- function(dates, currentDate = Sys.Date(), xi = 0) {
  datediffs <- dates - as.Date(currentDate)
  datediffs <- as.numeric(datediffs * -1)
  w <- exp(-1 * xi * datediffs)
  w[datediffs <= 0] <- 0  #Future dates should have zero weights
  return(w)
}

DClogLik <- function(y1, y2, lambda, mu, rho = 0, weights = NULL) {
  # rho=0, independence y1 home goals y2 away goals
  loglik <- log(tau(y1, y2, lambda, mu, rho)) + log(stats::dpois(y1, lambda)) + log(stats::dpois(y2, mu))
  if (is.null(weights)) {
    return(sum(loglik))
  } else {
    return(sum(loglik * weights))
  }
}
