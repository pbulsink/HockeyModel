#Hosts dixon-coles specific functions. Many copied from my work from pbulsink.github.io
# see http://opisthokonta.net/?p=913

#' Update Dixon Coles parameters
#'
#' @param scores scores, if not then data(scores)
#'
#' @export
updateDC <- function(scores){
  scoresdc<-scores[scores$Date > as.Date("2008-08-01"),]
  m <- getM(scores=scoresdc)
  rho <- getRho(m = m, scores = scoresdc)
  devtools::use_data(m, rho, overwrite = TRUE)
}

#' Produce a plot of each team's offence and defence scores
#'
#' @param m m from data(m)
#' @param teamlist Teams to plot.
#'
#' @return a ggplot object
#' @export
plotDC <- function(m, teamlist = NULL){
  if(is.null(teamlist)){
    teamlist<-as.character(unique(m$data$Home))
  }
  team_params <- data.frame(Attack = as.numeric(m$coefficients[1:length(teamlist)]),
                            Defence = c(0, m$coefficients[(length(teamlist)+1):(length(teamlist)*2-1)]),
                            Team = sort(teamlist))

  p<-ggplot2::ggplot(team_params, ggplot2::aes_(x=quote(Attack), y=quote(Defence), color=quote(Team), label=quote(Team))) +
    ggplot2::ggtitle("Attack and Defence Parameters") +
    ggplot2::xlab("Attack") +
    ggplot2::ylab("Defence (lower = better)") +
    ggplot2::geom_point() +
    ggrepel::geom_text_repel(force=2, max.iter=5000) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="none")
  return(p)
}

#' DC Predictions Today
#'
#' @param today Generate predictions for this date. Defaults to today
#'
#' @return a data frame of HomeTeam, AwayTeam, HomeWin, AwayWin, Draw, or NULL if no games today
#' @export
todayDC <- function(today = Sys.Date()){
  games<-schedule[schedule$Date == today, ]
  if(nrow(games) == 0){
    return(NULL)
  }

  preds<-data.frame(HomeTeam=games$Home, AwayTeam=games$Visitor,
                    HomeWin=0, AwayWin = 0, Draw = 0,
                    stringsAsFactors = FALSE)
  for(i in 1:nrow(preds)){
    p<-DCPredict(preds$HomeTeam[[i]], preds$AwayTeam[[i]], m=m, rho=rho)
    preds$HomeWin[[i]]<-p[[1]]
    preds$AwayWin[[i]]<-p[[3]]
    preds$Draw[[i]]<-p[[2]]
  }

  return(preds)
}

#' DC remainder of season
#' @description Odds for each team to get to playoffs.
#'
#' @param nsims Number of simulations
#'
#' @return data frame of Team, playoff odds.
#' @export
remainderSeasonDC <- function(nsims=10000){
  season_sofar<-scores[scores$Date > as.Date("2018-08-01"),]
  teamlist<-sort(unique(c(season_sofar$HomeTeam, season_sofar$AwayTeam,schedule$Home, schedule$Away)))
  games<-data.frame(HomeTeam = character(), AwayTeam=character(),
                    HomeWin=numeric(), AwayWin=numeric(), Draw=numeric(),
                    stringsAsFactors = FALSE)
  for(d in unique(schedule$Date)){
    preds<-todayDC(today=d)
    preds$Date <- d
    games<-rbind(games, preds)
  }
  games$Date<-schedule$Date

  n<-length(teamlist)

  all_results <- data.frame(Team = rep(sTeams, nsims),
                       SimNo = rep(1:iSim, each = n),
                       Pts = rep(NA, n * nsims),
                       W = rep(NA, n*nsims),
                       OTW = rep(NA, n * nsims),
                       SOW = rep(NA, n * nsims),
                       Rank = rep(NA, n * nsims))
  for(i in 1:nsims){
    tmp<-games
    tmp$res1<-runif(n = nrow(games))
    tmp$res2<-runif(n = nrow(games))
    tmp$D<-(tmp$res1>tmp$HomeWin && tmp$res1<(tmp$HomeWin + tmp$Draw))
    tmp$H<-2*as.numeric((tmp$res1<tmp$HomeWin)) +
      1*as.numeric((tmp$D)) +
      1*as.numeric((tmp$D)*tmp$res2<0.5) + 0
    tmp$A<-2*(tmp$res1>(tmp$HomeWin+tmp$Draw)) + 1*(tmp$D) + 1*(tmp$D)*tmp$res2>0.5+0
  }
  return(games)
}

#' DC Season Points/Ranking
#'
#' @return data frame of 'league table', including predicted points
#' @export
seasonRankDC <- function(){
  NULL
}

playoffOpponentDC <- function(){
  NULL
}

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
#' @keywords internal
getM <- function(scores=scores) {
  df.indep <- data.frame(
    Date = c(scores$Date, scores$Date),
    Weight = c(DCweights(dates = scores$Date, currentDate = Sys.Date(), xi = 0.002), DCweights(dates = scores$Date, currentDate = Sys.Date(), xi = 0.002)),
    Team = as.factor(c(as.character(scores$HomeTeam), as.character(scores$AwayTeam))),
    Opponent = as.factor(c(as.character(scores$AwayTeam), as.character(scores$HomeTeam))),
    Goals = c(scores$HomeGoals, scores$AwayGoals),
    Home = c(rep(1, nrow(scores)), rep(0, nrow(scores)))
  )
  #Using a (+0) to remove intercept and give a value for each team instead of assuming 'Anaheim Ducks' = 0 (reference)
  m <- stats::glm(Goals ~ Team + Opponent + Home + 0,
                  data = df.indep,
                  weights = df.indep$Weight,
                  family = stats::poisson(link = log),
                  start = rep(0.01, length(unique(df.indep$Team))*2),
                  model = FALSE
  )
  m<-cleanModel(m)  #reduce M size
  return(m)
}

#' Generate a 'rho' factor for low scoring games
#'
#' @param m result from getM
#' @param scores the historical scores to evaluate
#'
#' @return a numeric value (typically -0.5 to 0)
#' @keywords internal
getRho <- function(m = NULL, scores=scores) {
  if (is.null(m)){
    getM(scores)
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
  return(res$par)

  #of course, res$par is rho. Ranges from -0.2779 for last decade, -0.175 for 20152016 or 0.09 fo the whole league's history
}

#' DC Predict home/draw/away win
#'
#' @description Using Dixon Coles technique, predict odds each of home win, draw, and away win.
#'
#' @param m result from getM
#' @param rho rho from getRho
#' @param home home team
#' @param away away team
#' @param maxgoal max number of goals per team
#' @param scores optional, if not supplying m & rho, scores used to calculate them.
#'
#' @return a list of home win, draw, and away win probability
#' @keywords internal
DCPredict <- function(home, away, m = NULL, rho = NULL, maxgoal = 8, scores = scores) {
  if(is.null(m)){
    m <- getM(scores = scores)
  }
  if(is.null(rho)){
    rho <- getRho(m=m, scores=scores)
  }
  # Expected goals home
  lambda <- stats::predict(m, data.frame(Home = 1, Team = home, Opponent = away), type = "response")

  # Expected goals away
  mu <- stats::predict(m, data.frame(Home = 0, Team = away, Opponent = home), type = "response")

  probability_matrix <- stats::dpois(0:maxgoal, lambda) %*% t(stats::dpois(0:maxgoal, mu))

  scaling_matrix <- matrix(tau(c(0, 1, 0, 1), c(0, 0, 1, 1), lambda, mu, rho), nrow = 2)
  probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix

  HomeWinProbability <- sum(probability_matrix[lower.tri(probability_matrix)])
  DrawProbability <- sum(diag(probability_matrix))
  AwayWinProbability <- sum(probability_matrix[upper.tri(probability_matrix)])

  return(c(HomeWinProbability, DrawProbability, AwayWinProbability))
}

#' DC Weight
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
