#Hosts dixon-coles specific functions. Many copied from my work from pbulsink.github.io
# see http://opisthokonta.net/?p=913
# see http://rstudio-pubs-static.s3.amazonaws.com/149923_584734fddffe40799cee564c938948d7.html

#' Update Dixon Coles parameters
#'
#' @description This function updates the model parameters to best fit the provided data. The parameters for this model are as follows:
#' * [m] is the result of the main model fit and contains team attack and defense strengths, plus home ice advantage terms
#' * [rho] is the Dixon-Coles low scores adjustment term.
#' * [beta] is the Weibull distribution's 'shape' parameter. This is used with [eta] to produce a curve multiplied by the diagonal score possibility matrix to enhance the odds of tie games
#' * [eta] is the Weibull distribution's 'scale' parameter. See above for its importance
#' * [k] is the multiplication factor used with the Weibull distribution to enhance ties
#'
#' @param scores scores, if not then HockeyModel::scores is used
#' @param currentDate Current Date, usually today but useful to set a different date if back calculating results
#' @param save_data Whether to save parameters to the package.
#'
#' @return a named list containing m, rho, beta, eta and k values for the model.
#'
#' @seealso [m], [rho], [beta], [eta], [k]
#'
#' @export
updateDC <- function(scores = HockeyModel::scores, currentDate = Sys.Date(), save_data = TRUE){
  message('Calculating new model parameters...')
  if (currentDate != Sys.Date()){
    currentDate <- as.Date(currentDate)
    scores<-scores[scores$Date < currentDate, ]
    save_data <- FALSE
  }
  m <- getM(scores=scores, currentDate = currentDate)
  message('Solving for low scoring games...')
  rho <- getRho(m = m, scores = scores)
  message('Enhancing Tie Games')
  params <- getWeibullParams(m = m, rho = rho, scores = scores)
  beta <- params$beta
  eta <- params$eta
  k <- params$k
  if(save_data){
    suppressMessages(usethis::use_data(m, rho, beta, eta, k, overwrite = TRUE))
  }
  return(list("m" = m, "rho"=rho, "beta" = beta, "eta" = eta, "k" = k))
}

#' Produce a plot of each team's offence and defense scores
#'
#' @param m m from data(m)
#' @param teamlist Teams to plot.
#'
#' @return a ggplot object
#' @export
plotDC <- function(m = HockeyModel::m, teamlist = NULL){
  if(is.null(teamlist)){
    teamlist<-as.character(unique(m$data$Team))
  }
  team_params <- data.frame(Attack = as.numeric(m$coefficients[1:length(teamlist)]),
                            Defence = c(0, -m$coefficients[(length(teamlist)+1):(length(teamlist)*2-1)]),
                            Team = sort(teamlist))

  #Build and trim team colours for plot
  teamColoursList<-as.vector(HockeyModel::teamColours$Hex)
  names(teamColoursList)<-HockeyModel::teamColours$Team
  teamColoursList<-teamColoursList[names(teamColoursList) %in% teamlist]

  p<-ggplot2::ggplot(team_params,
                     ggplot2::aes_(x=quote(Attack),
                                   y=quote(Defence),
                                   color=quote(Team),
                                   label=quote(Team)
                                   )
                     ) +
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggrepel::geom_text_repel(force=2, max.iter=5000) +
    ggplot2::labs(x = "Attack",
                  y = "Defence",
                  title = "Current Team Attack & Defense Ratings",
                  subtitle = paste0("As of ", Sys.Date()),
                  caption = paste0("P. Bulsink (@BulsinkB) | ", Sys.Date()))+
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="none")
  return(p)
}

#' DC Predictions Today
#'
#' @param today Generate predictions for this date. Defaults to today
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param schedule schedule to use, if not the built-in
#' @param expected_mean the mean lambda & mu, used only for regression
#' @param season_percent the percent complete of the season, used for regression
#'
#' @return a data frame of HomeTeam, AwayTeam, HomeWin, AwayWin, Draw, or NULL if no games today
todayDC <- function(params=NULL, today = Sys.Date(), schedule = HockeyModel::schedule, expected_mean = NULL, season_percent = NULL){
  params<-parse_dc_params(params)
  games<-schedule[schedule$Date == today, ]
  if(nrow(games) == 0){
    return(NULL)
  }

  preds<-data.frame(HomeTeam=games$HomeTeam, AwayTeam=games$AwayTeam,
                    HomeWin=0, AwayWin = 0, Draw = 0,
                    stringsAsFactors = FALSE)
  for(i in 1:nrow(preds)){
    p<-DCPredict(preds$HomeTeam[[i]], preds$AwayTeam[[i]], params=params, expected_mean=expected_mean, season_percent=season_percent)
    preds$HomeWin[[i]]<-p[[1]]
    preds$AwayWin[[i]]<-p[[3]]
    preds$Draw[[i]]<-p[[2]]
  }

  return(preds)
}


#' Playoff Odds DC
#'
#' @param home Series Home Ice Advantage Team Name
#' @param away Away (Opponent) Team Name
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param home_wins Number of wins for home ice advantage team thus far in series
#' @param away_wins Number of wins for away team thus far in series
#'
#' @return home ice advantage team odds to win series
#' @export
playoffDC <- function(home, away, params, home_wins = 0, away_wins = 0){
  params<-parse_dc_params(params)
  #Odds of home ice advantage team win at home
  homeodds<-DCPredict(home=home, away=away, params=params)
  homeodds<-normalizeOdds(c(homeodds[1], homeodds[3]))[1]
  #Odds of home ice advantage team win away
  awayodds<-DCPredict(home=away, away=home, params=params)
  awayodds<-normalizeOdds(c(awayodds[3], awayodds[1]))[1]

  homewin<-playoffSeriesOdds(homeodds, awayodds, home_wins, away_wins)
  return(homewin)
}


#' DC Simulate season with reevaluation every n days
#'
#' @param nsims number of simulations
#' @param scores historical scores
#' @param schedule schedule this season
#' @param cores number of cores to process
#' @param ndays number of days of games to play before reevaluating m
#' @param regress Whether to regress toward mean
#' @param progress Whether to show a progress bar.
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return data frame of Team point ranges, playoff odds, etc.
#' @export
dcRealSeasonPredict<-function(nsims=1e5, params=NULL, scores = HockeyModel::scores, schedule = HockeyModel::schedule, cores = parallel::detectCores()-1, ndays=7, regress=FALSE, progress = FALSE){

  schedule$Date<-as.Date(schedule$Date)
  schedule<-schedule[schedule$Date > max(scores$Date), ]
  dcscores<-scores[scores$Date > as.Date("2005-08-01"),]
  dcscores<-droplevels(dcscores)

  #Get a tie performance for each team.
  dcscores.ot<-dcscores[dcscores$OTStatus != '', ]
  dcscores.ot$minscore<-pmin(dcscores.ot$HomeGoals, dcscores.ot$AwayGoals)
  dcscores.ot$HomeGoals<-dcscores.ot$HomeGoals-dcscores.ot$minscore
  dcscores.ot$AwayGoals<-dcscores.ot$AwayGoals-dcscores.ot$minscore

  dcscores$OTStatus <- NULL
  dcscores.ot$minscore <- NULL

  #Generate m
  dc.m.ot <- getM(scores=dcscores.ot, currentDate = schedule$Date[[1]])
  dc.m.original <- getM(scores = dcscores, currentDate = schedule$Date[[1]])
  params<-parse_dc_params(params=params)

  if(regress){
    last_game_date<-as.Date(max(scores$Date))
    season_end_date <- as.Date(max(schedule$Date))
    season_start_date <- as.Date(min(c(scores[scores$Date > as.Date(getSeasonStartDate()), 'Date'], schedule[schedule$Date > as.Date(getSeasonStartDate()), 'Date'])))
    season_length <- as.integer(season_end_date) - as.integer(season_start_date)
    remaining_length <- as.integer(season_end_date) - as.integer(last_game_date)
    expected_mean<-2.835184
  }

  maxgoal<-10

  `%dopar%` <- foreach::`%dopar%`
  cl<-parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)
  if(progress){
    pb<-utils::txtProgressBar(max = nsims, style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
  } else {
    opts <- list()
  }
  #all_results <- data.frame(Team = character(), GP = integer(), Points = integer(), W = integer(), L = integer(), OTL = integer(), SOL = integer(), SOW = integer(), Rank = integer(), ConfRank = integer(), DivRank = integer(), Playoffs = integer())

  all_results <- foreach::foreach(i=1:nsims, .combine='rbind', .options.snow = opts, .packages=c("HockeyModel")) %dopar%{ #, DCPredictErrorRecover = DCPredictErrorRecover) %dopar% {
  #for(i in 1:nsims){
    dc.m <- dc.m.original
    results<-schedule
    results$HomeGoals<-NA
    results$AwayGoals<-NA
    results$Result<-NA
    results<-dplyr::bind_rows(dcscores[dcscores$Date > as.Date(getSeasonStartDate()),], results)

    lastdate<-as.Date(schedule$Date[[1]])

    for(g in which(is.na(results$Result))){
      if(results$Date[[g]] > (lastdate + ndays)){
        #New dc.m
        newscores<-rbind(dcscores, results[results$Date<results$Date[[g]], ])
        dc.m<-getM(scores = newscores, currentDate = results$Date[[g]])
        lastdate<-results$Date[[g]]
      }

      home<-results$HomeTeam[[g]]
      away<-results$AwayTeam[[g]]
      # Expected goals home
      lambda <- try(stats::predict(dc.m, data.frame(Home = 1, Team = home, Opponent = away), type = "response"), TRUE)

      # Expected goals away
      mu<-try(stats::predict(dc.m, data.frame(Home = 0, Team = away, Opponent = home), type = "response"), TRUE)

      #Fix something wrong with expected away or home goals
      if(!is.numeric(lambda)){
        message("unknown lambda")
        lambda<-DCPredictErrorRecover(team = home, opponent = away, homeiceadv = TRUE, m = dc.m)
      }
      if(!is.numeric(mu)){
        message("unknown mu")
        mu<-DCPredictErrorRecover(team = away, opponent = home, homeiceadv = FALSE, m = dc.m)
      }

      if(regress){
        #Adjust regress to mean
        season_percent <- (remaining_length - as.integer(season_end_date - as.Date(results$Date[[g]])))/season_length
        lambda <- lambda * (1-1/3 * season_percent) + expected_mean * (1/3 * season_percent)
        mu <- mu * (1-1/3 * season_percent) + expected_mean * (1/3 * season_percent)
      }

      #TODO Just call random result from the two teams...
      probability_matrix <- prob_matrix(lambda = lambda, mu = mu, params=params, maxgoal=maxgoal)

      goals<-data.frame(Goals = c(0:maxgoal), Home = 0, Away = 0)

      goals$Away<-colSums(probability_matrix)*1/sum(colSums(probability_matrix))
      goals$Home<-rowSums(probability_matrix)*1/sum(rowSums(probability_matrix))

      #Sum of densities (1 indexed... row 1 == 0 goals) for randomly getting a reasonable goal total

      hg<-cumsum(goals$Home)
      ag<-cumsum(goals$Away)

      #Use runif to get a goals total, then adjust for 0/1 off-index. max includes... ,1 for error prevention (e.g. no hg < runif(1))
      homegoals<-max(which(hg < stats::runif(1)), 1)-1
      awaygoals<-max(which(ag < stats::runif(1)), 1)-1

      #Deal with ties
      if(homegoals == awaygoals){
        params$m<-dc.m.ot
        p<-DCPredict(home = home, away = away, params=params, maxgoal=1, scores=NULL)
        pHome<-normalizeOdds(c(p[[1]], 0, p[[3]]))[[1]]
        if(stats::runif(1) < pHome){
          homegoals <- homegoals + 1
          if(stats::runif(1) > p[[2]]){
            results$Result[[g]] <- 0.75
          } else {
            results$Result[[g]] <- 0.60
          }
        } else {
          awaygoals <- awaygoals + 1
          if(stats::runif(1) > p[[2]]){
            results$Result[[g]] <- 0.25
          } else {
            results$Result[[g]] <- 0.40
          }
        }
      } else {
        results$Result[[g]]<-ifelse(homegoals>awaygoals, 1, 0)
      }

      results$HomeGoals[[g]] <- homegoals
      results$AwayGoals[[g]] <- awaygoals
    }
    table<-buildStats(results)
  }

  if(progress){
    close(pb)
  }
  parallel::stopCluster(cl)
  gc(verbose = FALSE)

  summary_results<-all_results %>%
    dplyr::group_by(.data$Team) %>%
    dplyr::summarise(
      Playoffs = mean(.data$Playoffs),
      meanPoints = mean(.data$Points, na.rm = TRUE),
      maxPoints = max(.data$Points, na.rm = TRUE),
      minPoints = min(.data$Points, na.rm = TRUE),
      meanWins = mean(.data$W, na.rm = TRUE),
      maxWins = max(.data$W, na.rm = TRUE),
      Presidents = sum(.data$Rank == 1)/dplyr::n(),
      meanRank = mean(.data$Rank, na.rm = TRUE),
      bestRank = min(.data$Rank, na.rm = TRUE),
      meanConfRank = mean(.data$ConfRank, na.rm = TRUE),
      bestConfRank = min(.data$ConfRank, na.rm = TRUE),
      meanDivRank = mean(.data$DivRank, na.rm = TRUE),
      bestDivRank = min(.data$DivRank, na.rm = TRUE),
      sdPoints = stats::sd(.data$Points, na.rm = TRUE),
      sdWins = stats::sd(.data$W, na.rm = TRUE),
      sdRank = stats::sd(.data$Rank, na.rm = TRUE),
      sdConfRank = stats::sd(.data$ConfRank, na.rm = TRUE),
      sdDivRank = stats::sd(.data$DivRank, na.rm = TRUE)
    )


  return(list(summary_results = summary_results, raw_results = all_results))
}


#' DC remainder of season
#' @description Odds for each team to get to playoffs.
#'
#' @param nsims Number of simulations
#' @param cores The number of cores to use if using parallel processing
#' @param scores the historical scores
#' @param schedule un-played future games
#' @param odds whether to return odds table or simulate season
#' @param regress whether to apply a regression to the mean for team strength on future predictions
#' @param mu_lambda whether to return team xG values. Can't be set true if odds is true
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return data frame of Team, playoff odds.
#' @export
remainderSeasonDC <- function(nsims=1e4, cores = parallel::detectCores()-1, params=NULL, scores = HockeyModel::scores, schedule = HockeyModel::schedule, odds = FALSE, regress = TRUE, mu_lambda = FALSE){

  odds_table<-data.frame(HomeTeam = character(), AwayTeam=character(),
                    HomeWin=numeric(), AwayWin=numeric(), Draw=numeric(),
                    stringsAsFactors = FALSE)

  params<-parse_dc_params(params=params)

  last_game_date<-as.Date(max(scores$Date))
  schedule <- schedule[schedule$Date > last_game_date, ]

  #cant regress through playoffs, turn off if not regular season anymore
  if(regress){
    if(nrow(schedule[schedule$Date >= Sys.Date() & schedule$GameType == "R", ]) == 0){
      regress <- FALSE
    }
  }

  if(regress){
    season_end_date <- as.Date(max(schedule[schedule$GameType == 'R', ]$Date))
    season_start_date <- as.Date(min(c(scores[scores$Date > as.Date(getSeasonStartDate()), 'Date'], schedule[schedule$Date > as.Date(getSeasonStartDate()), 'Date'])))
    season_length <- as.integer(season_end_date) - as.integer(season_start_date)
    remaining_length <- as.integer(season_end_date) - as.integer(last_game_date)
    expected_mean<-2.835184
  } else {
    expected_mean<-NULL
    season_percent <- NULL
  }

  for(day in unique(schedule$Date)){
    d<-as.Date(day, origin="1970-01-01")
    if(regress){
      #Adjust regress to mean
      season_percent <- (remaining_length - as.integer(season_end_date - as.Date(d)))/season_length
    }

    preds<-todayDC(today=d, schedule = schedule, season_percent = season_percent, expected_mean = expected_mean, params = params)
    preds$Date <- d
    odds_table<-rbind(odds_table, preds)
  }
  odds_table$Date<-schedule$Date

  if(odds){
    return(odds_table)
  }

  if(mu_lambda){
    odds_table$mu <- NA
    odds_table$lambda <- NA

    for(g in 1:nrow(odds_table)){
      d<-as.Date(odds_table[g,"Date"], origin="1970-01-01")
      # Expected goals home
      lambda <- try(stats::predict(HockeyModel::m, data.frame(Home = 1, Team = odds_table$HomeTeam[g], Opponent = odds_table$AwayTeam[g]), type = "response"), TRUE)

      # Expected goals away
      mu<-try(stats::predict(HockeyModel::m, data.frame(Home = 0, Team = odds_table$AwayTeam[g], Opponent = odds_table$HomeTeam[g]), type = "response"), TRUE)

      if(!is.numeric(lambda)){
        lambda<-DCPredictErrorRecover(team = odds_table$HomeTeam[g], opponent = odds_table$AwayTeam[g], homeiceadv = TRUE)
      }
      if(!is.numeric(mu)){
        mu<-DCPredictErrorRecover(team = odds_table$AwayTeam[g], opponent = odds_table$HomeTeam[g], homeiceadv = FALSE)
      }

      if(regress){
        #Adjust regress to mean
        season_percent <- (remaining_length - as.integer(season_end_date - as.Date(d)))/season_length

        lambda <- lambda * (1-1/3 * season_percent) + expected_mean * (1/3 * season_percent)
        mu <- mu * (1-1/3 * season_percent) + expected_mean * (1/3 * season_percent)
      }
      odds_table[g, "lambda"]<-lambda
      odds_table[g, "mu"]<-mu
    }
    odds_table$HomeWin<-odds_table$AwayWin<-odds_table$Draw <- NULL
    return(odds_table)
  }

  summary_results <- simulateSeasonParallel(odds_table = odds_table, nsims = nsims, cores = cores, scores = scores, schedule = schedule)

  return(summary_results)
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

#' Tau function
#' @description Used in Dixon-Coles's to adjust low scores
#'
#' @param xx Homeoal value for adjustment factor calculation
#' @param yy Away Goal value for adjustment factor calculation
#' @param lambda Home goal expected for adjustment factor calculation
#' @param mu Away Goal expected for adjustment factor calculation
#' @param rho the factor for adjustment calculations
#'
#' @export
tau <- Vectorize(tau_singular, c('xx', 'yy', 'lambda', 'mu'))


#' Fast Dixon-Coles model fitting 'm'.
#'
#' @description Produces a DC model
#'
#' @param scores the historical scores to evaluate
#' @param currentDate (for date weight adjustment)
#' @param xi aggressiveness of date weighting
#'
#' @export
#' @return a model 'm' of Dixon-Coles' type parameters.
getM <- function(scores=HockeyModel::scores, currentDate = Sys.Date(), xi=0.00426) {
  stopifnot(is.Date(currentDate))
  currentDate<-as.Date(currentDate)

  scores<-scores[scores$Date>=(currentDate-4000),] #auto-trim to ~11 years of data, past then the model doesn't get better, just bigger
  df.indep <- data.frame(
    Date = c(scores$Date, scores$Date),
    GameID = c(scores$GameID, scores$GameID),
    Weight = c(DCweights(dates = scores$Date, currentDate = currentDate, xi = xi), DCweights(dates = scores$Date, currentDate = currentDate, xi = xi)),
    Team = as.factor(c(as.character(scores$HomeTeam), as.character(scores$AwayTeam))),
    Opponent = as.factor(c(as.character(scores$AwayTeam), as.character(scores$HomeTeam))),
    Goals = c(scores$HomeGoals, scores$AwayGoals),
    Home = c(rep(1, nrow(scores)), rep(0, nrow(scores)))
  )
  #df.indep <- df.indep[df.indep$Weight > 1e-8,]
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
#' @export
getRho <- function(m = HockeyModel::m, scores=HockeyModel::scores) {
  if (is.null(m)){
    m<-getM(scores)
  }
  scores<-scores[scores$GameID %in% unique(m$data$GameID),]
  expected <- stats::fitted(m)
  home.expected <- as.vector(expected[1:nrow(scores)])
  away.expected <- as.vector(expected[(nrow(scores) + 1):(nrow(scores) * 2)])
  weights<-m$data$Weight[1:nrow(scores)]

  DCoptimRhoFn.fast <- function(par) {
    rho <- par[1]
    DCRhoLogLik(y1 = scores$HomeGoals, y2 = scores$AwayGoals, mu = home.expected, lambda = away.expected, rho = rho, weights = weights)
  }

  res <- stats::optim(par = c(-0.1),
                      fn = DCoptimRhoFn.fast,
                      #control = list(fnscale = -1),
                      method = "BFGS")
  return(res$par)

  #of course, res$par is rho. Ranges from -0.2779 for last decade, -0.175 for 20152016 or 0.09 fo the whole league's history
}


#' Get Weibull Params
#'
#' @description Weibull Params determine is the diagonal enhancement for goals, helping to more accurately predict tie games.
#'
#' @param m HockeyModel::m
#' @param rho HockeyMoel::rho
#' @param scores HockeyModel::scores
#'
#' @return a [beta] and [eta] and [k] value as a list
#' @export
getWeibullParams <- function(m=HockeyModel::m, rho=HockeyModel::rho, scores=HockeyModel::scores){
  if(is.null(m)){
    m<-getM(scores)
  }
  if(is.null(rho)){
    rho<-getRho(m = m, scores=scores)
  }

  #Have to grab the 'low score' of each tie game, then add one for modelling purposes (Weibull @ x=0 is 0)
  scores<-scores%>%
    dplyr::filter(.data$GameID %in% unique(m$data$GameID)) %>%
    dplyr::mutate('weight' = m$data$Weight[1:dplyr::n()],
                  'mu' = stats::fitted(m)[1:dplyr::n()],
                  'lambda' = stats::fitted(m)[(dplyr::n()+1):(dplyr::n()*2)])%>%
    dplyr::mutate('Goals' = dplyr::case_when(.data$Result == 0.25 ~ .data$HomeGoals+1,
                                             .data$Result == 0.4  ~ .data$HomeGoals+1,
                                             .data$Result == 0.5  ~ .data$HomeGoals+1,
                                             .data$Result == 0.6  ~ .data$AwayGoals+1,
                                             .data$Result == 0.75 ~ .data$AwayGoals+1,
                                             TRUE ~ NA_real_))
  DCoptimTheta.fast <- function(par) {
    beta<-par[1]
    eta <- par[2]
    k<-par[3]
    #This gets multiplied by two because each tie game has 2 teams getting x goals, whereas non-tie games where either team get x goals get pulled in.
    goalratios<-sapply(1:max(scores$Goals, na.rm = TRUE), FUN = function(x) (nrow(scores[scores$Goals == x & !is.na(scores$Goals),])/nrow(scores[!is.na(scores$Goals),]))/(nrow(scores[(scores$HomeGoals == x | scores$AwayGoals == x) & is.na(scores$Goals),])/nrow(scores[is.na(scores$Goals),])) ) * 2
    weibulldist<-stats::dweibull(1:max(scores$Goals, na.rm = TRUE), shape = beta, scale=eta)*k
    return(sum((goalratios-weibulldist)^2))
  }

  res<-stats::optim(par = c(3, 1, 6),
                    fn = DCoptimTheta.fast,
                    lower = c(-Inf, 0, -Inf),
                    #control = list(fnscale=-1),
                    method="L-BFGS-B")

  return(list('beta' = res$par[1], 'eta' = res$par[2], 'k'=res$par[3]))
}


#' DC Predict home/draw/away win
#'
#' @description Using Dixon-Coles' technique, predict odds each of home win, draw, and away win.
#'
#' @param home home team
#' @param away away team
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param maxgoal max number of goals per team
#' @param scores optional, if not supplying m & rho, scores used to calculate them.
#' @param expected_mean the mean lambda & mu, used only for regression
#' @param season_percent the percent complete of the season, used for regression
#' @param draws Whether draws are allowed. Default True
#'
#' @return a vector of home win, draw, and away win probability, or if draws=False, a vector of home and away win probability
#' @export
DCPredict <- function(home, away, params=NULL, maxgoal = 8, scores = HockeyModel::scores, expected_mean=NULL, season_percent=NULL, draws=TRUE) {
  params<-parse_dc_params(params=params)
  probability_matrix <- dcProbMatrix(home = home, away = away, params=params, maxgoal = maxgoal)

  HomeWinProbability <- sum(probability_matrix[lower.tri(probability_matrix)])
  DrawProbability <- sum(diag(probability_matrix))
  AwayWinProbability <- sum(probability_matrix[upper.tri(probability_matrix)])

  #Simple Adjust for under-predicting odds
  #TODO Fix it: add draw parameter to model
  #HomeWinProbability <- HomeWinProbability * (0.43469786 / 0.4558628)
  #AwayWinProbability <- AwayWinProbability * (0.33333333 / 0.3597192)
  #DrawProbability <- DrawProbability * (0.2319688 / 0.1755118)
  odds <- normalizeOdds(c(HomeWinProbability, DrawProbability, AwayWinProbability))

  if(!draws){
    HomeWinProbability<-HomeWinProbability+normalizeOdds(c(HomeWinProbability, AwayWinProbability))[1]*DrawProbability
    AwayWinProbability<-AwayWinProbability+normalizeOdds(c(HomeWinProbability, AwayWinProbability))[2]*DrawProbability
    odds<-normalizeOdds(c(HomeWinProbability, AwayWinProbability))
  }
  return(odds)
}

#' Generate the Dixon-Coles' Probability Matrix
#'
#' @param home home team
#' @param away away team
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param maxgoal max number of goals per team
#' @param scores optional, if not supplying m & rho, scores used to calculate them.
#' @param expected_mean the mean lambda & mu, used only for regression
#' @param season_percent the percent complete of the season, used for regression
#'
#' @return a square matrix of dims 0:maxgoal with odds at each count of  home goals on 'rows' and away goals  on 'columns'
dcProbMatrix<-function(home, away, params=NULL, maxgoal = 8, scores = HockeyModel::scores, expected_mean=NULL, season_percent=NULL){
  params<-parse_dc_params(params=params)

  # Expected goals home
  lambda <- try(stats::predict(params$m, data.frame(Home = 1, Team = home, Opponent = away), type = "response"), TRUE)

  # Expected goals away
  mu<-try(stats::predict(params$m, data.frame(Home = 0, Team = away, Opponent = home), type = "response"), TRUE)

  if(!is.numeric(lambda)){
    lambda<-DCPredictErrorRecover(team = home, opponent = away, homeiceadv = TRUE)
  }
  if(!is.numeric(mu)){
    mu<-DCPredictErrorRecover(team = away, opponent = home, homeiceadv = FALSE)
  }

  if(!is.null(expected_mean) & ! is.null(season_percent)) {
    lambda <- lambda * (1-1/3 * season_percent) + expected_mean * (1/3 * season_percent)
    mu <- mu * (1-1/3 * season_percent) + expected_mean * (1/3 * season_percent)
  }

  probability_matrix<-prob_matrix(lambda=lambda, mu=mu, params=params, maxgoal=maxgoal)

  return(probability_matrix)
}

#' Probability Matrix
#'
#' @description Given a mu, lambda, rho, and theta, generate a probability matrix. Differs from dcProbMatrix in that no regresson or solving for supplied teams happens
#'
#' @param lambda home lambda
#' @param mu away mu
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param maxgoal max goals per game
#'
#' @return a square matrix of maxgoal:maxgoal
prob_matrix<-function(lambda, mu, params, maxgoal){
  params<-parse_dc_params(params)
  probability_matrix <- stats::dpois(0:maxgoal, lambda) %*% t(stats::dpois(0:maxgoal, mu))

  #scaling_matrix <- matrix(tau(c(0, 1, 0, 1), c(0, 0, 1, 1), lambda, mu, params$rho), nrow = 2)
  #mvoed tau into vector - lookup and vectorized was too slow.
  scaling_matrix <- matrix(c(1 - (lambda * mu * params$rho), 1 + (mu * params$rho), 1 + (lambda * params$rho), 1 - params$rho), nrow = 2)
  probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix

  diag(probability_matrix)<-diag(probability_matrix)*stats::dweibull(c(1:(maxgoal+1)), shape=params$beta, scale = params$eta)*params$k
  #for(d in 1:(maxgoal+1)){
    #probability_matrix[d,d]<-probability_matrix[d,d] * (stats::dweibull(d, shape = params$beta, scale = params$eta) * params$k)
  #}

  probability_matrix <- probability_matrix/sum(probability_matrix)  # turn into a sum=1 matrix

  return(probability_matrix)
}


#' DC Sample
#'
#' @description Get a random single game result using DC method. repeated running should give a new value each time
#'
#' @param home home team
#' @param away away team
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param maxgoal max number of goals per team
#' @param scores optional, if not supplying m & rho, scores used to calculate them.
#' @param expected_mean the mean lambda & mu, used only for regression
#' @param season_percent the percent complete of the season, used for regression
#' @param as_result Whether to give a score or just flatten it to result type (see \link{scores} for score result)
#'
#' @return a random Home & away goals & OT/SO status if needed
#' @export
#'
#' @examples dcSample("Toronto Maple Leafs", "Montreal Canadiens")
dcSample<-function(home, away, params=NULL, maxgoal = 8, scores = HockeyModel::scores, expected_mean=NULL, season_percent=NULL, as_result=TRUE){
  params<-parse_dc_params(params)
  pm <- dcProbMatrix(home = home, away = away, params=params, maxgoal = maxgoal)

  #sometimes there's negative probabilities. This handles that with fakign a very low value instead
  pm2<-pm
  pm2[pm2<0]<-1e-8

  goals<-as.vector(arrayInd(sample(1:length(pm2), size = 1, prob = pm2), .dim = dim(pm2)))-1

  if (goals[1] == goals[2]){
    #TODO Verify OT/SO ratio and also verify if wniner is coin flip or stronger team has better chance?
    otstatus = sample(c("OT", "SO"), size = 1, prob = c(0.6858606, 0.3141394))
    otwinner = sample(c("Home", "Away"), size = 1, prob = c(sum(pm[lower.tri(pm)]), sum(pm[upper.tri(pm)])))
    if(otwinner == "Home"){
      goals[1] <- goals[1] + 1
    } else {
      goals[2] <- goals[2] + 1
    }
  } else {
    otstatus = ""
  }
  if(as_result){
    return(dplyr::case_when(
      goals[1]>goals[2] & otstatus == "" ~ 1,
      goals[1]<goals[2] & otstatus == "" ~ 0,
      goals[1]>goals[2] & otstatus == "OT" ~ 0.75,
      goals[1]>goals[2] & otstatus == "SO" ~ 0.6,
      goals[1]<goals[2] & otstatus == "OT" ~ 0.25,
      goals[1]<goals[2] & otstatus == "SO" ~ 0.4
      ))
  } else {
    return(list("HomeGoals" = goals[1], "AwayGoals" = goals[2], "OTStatus" = otstatus))
  }
}


#' DC Result Sample
#'
#' @param lambda home team lambda
#' @param mu away team mu
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param maxgoal max goals predicable per game, default 10
#'
#' @return a result from 0 to 1 corresponding to \link{scores} results
dcResult<-function(lambda, mu, params=NULL, maxgoal=8){
  params<-parse_dc_params(params)

  dcr<-function(lambda, mu, params, maxgoal){
    pm <- prob_matrix(lambda=lambda, mu=mu, params=params, maxgoal=maxgoal)

    #sometimes there's negative probabilities. This handles that with fakign a very low value instead
    pm2<-pm
    pm2[pm2<0]<-1e-8

    goals<-as.vector(arrayInd(sample(1:length(pm2), size = 1, prob = pm2), .dim = dim(pm2)))-1

    if (goals[1] > goals[2]){
      return(1)
    } else if(goals[1]<goals[2]){
      return(0)
    } else{
      #TODO Verify OT/SO ratio and also verify if wniner is coin flip or stronger team has better chance?
      otstatus = sample(c(0.25, 0.1), size = 1, prob = c(0.6858606, 0.3141394))
      otwinner = sample(c(1, -1), size = 1, prob = c(sum(pm[lower.tri(pm)]), sum(pm[upper.tri(pm)])))
      return(0.5+(otstatus*otwinner))  # this will yield 0.75 for home OT, 0.25 for away OT, 0.6 for home SO, 0.4 for away SO win.
    }
  }

  v_dcr<-Vectorize(dcr, c('lambda', 'mu'))

  if(length(lambda) == 1){
    return(dcr(lambda, mu, params, maxgoal))
  } else {
    return(as.vector(v_dcr(lambda, mu, params, maxgoal)))
  }
}


#' DC Weight
#'
#' @param dates list of dates to calculate weights.
#' @param currentDate date from which to calculate weight
#' @param xi tuning factor
#'
#' @return list of weights coresponding to dates
#' @keywords internal
DCweights <- function(dates, currentDate = Sys.Date(), xi = 0.00426) {
  datediffs <- dates - as.Date(currentDate)
  datediffs <- as.numeric(datediffs * -1)
  w <- exp(-1 * xi * datediffs)
  w[datediffs <= 0] <- 0  #Future dates should have zero weights
  return(w)
}


DCRhoLogLik <- function(y1, y2, lambda, mu, rho = 0, weights = NULL) {
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

  return(cm)
}

DCPredictErrorRecover<-function(team, opponent, homeiceadv = FALSE, m = HockeyModel::m){
  teamlist<-unique(m$data$Team)
  opponentlist<-unique(m$data$Opponent)

  if(homeiceadv){
    homeice<-m$coefficients['Home']
  } else {
    homeice<-0
  }

  if(!(team %in% teamlist) & !(opponent %in% opponentlist)){
    lambda <- NA
  } else if(!(team %in% teamlist)){
    teamp<-min(m$coefficients[grep('Team', names(m$coefficients))])  # lowest goals scored for new team
    opponentp<-m$coefficients[paste0('Opponent', opponent)]

    lambda<-exp(teamp+opponentp+homeice)
  } else if (!(opponent %in% opponentlist)){
    teamp<-m$coefficients[paste0('Team', team)]
    opponentp<-max(m$coefficients[grep('Opponent', names(m$coefficients))])  # most goals allowed for new team

    lambda<-exp(teamp+opponentp+homeice)
  } else {
    lambda <- NA
  }

  if(is.na(lambda)){
    if(homeiceadv){
      lambda <- 3.319624 #Historical home goals
    } else {
      lambda <- 2.827417 #Historical away goals
    }
  }

  return(unname(lambda))
}

#' DC Predict Multiple Days
#'
#' @description For catching up on daily predictions
#'
#' @param start First day to predict. Default start of season
#' @param end Last day to predict. Default today
#' @param scores HockeyModel::scores
#' @param schedule HockeyModel::schedule
#' @param filedir Where to save prediction files.
#' @param nsims number of simulations to run
#' @param cores number of cores in parallel to simulate on
#'
#' @return true, if successful
#' @export
dcPredictMultipleDays<-function(start=as.Date(getSeasonStartDate()), end=Sys.Date(), scores=HockeyModel::scores, schedule=HockeyModel::schedule, filedir = "./prediction_results", nsims = 1e5, cores = parallel::detectCores() - 1){

  if(!dir.exists(filedir)){
    dir.create(filedir, recursive = TRUE)
  }

  predict_dates<-seq(from = end, to = start, by = -1)

  schedule$Date<-as.Date(schedule$Date)

  message("Running predictions for ", length(predict_dates), " day(s).")
  for(day in predict_dates){
    d<-as.Date(day, origin="1970-01-01")
    message('Predictions as of: ', d)
    score<-scores[scores$Date < day,]
    score<-score[score$Date > as.Date("2008-08-01"),]
    sched<-schedule[schedule$Date >= day,]
    params<-list()
    params$m<-getM(scores = score, currentDate = d)
    params$rho<-getRho(m = params$m, scores = score)
    #TODO This should change to updateDC(date=d)?
    w.day <- getWeibullParams(m=params$m, rho=params$rho, scores=score)
    params$beta <- w.day$beta
    params$eta <- w.day$eta
    params$k <- w.day$k
    preds<-NULL
    preds<-tryCatch(expr = {
      message("Predicting with Loopless Sim")
      #remainderSeasonDC(nsims=1e5, scores=score, schedule = sched, regress = TRUE)
      loopless_sim(nsims = nsims, cores = cores, scores = score, schedule = sched, params = params)
    },
    error = function(error) {
      message('An error occurred:')
      message(error)
      return(NULL)
    })
    if(!is.null(preds) & 'summary_results' %in% names(preds)){
      message('Saving Prediction file...')
      saveRDS(preds$summary_results, file = file.path(filedir, paste0(d, '-predictions.RDS')))
    } else {
      message('An error occurred, retrying ', d, '.')
      preds<-NULL
      preds<-tryCatch(expr = {
          message("Predicting with Old Version Sim")
          remainderSeasonDC(nsims=1e5, scores=score, schedule = sched, regress = TRUE)
        }, error = function(error) {
          message('An error occurred:')
          message(error)
          return(NULL)
        })
      if(!is.null(preds) & 'summary_results' %in% names(preds)){
        message('Saving Prediction file...')
        saveRDS(preds$summary_results, file = file.path(filedir, paste0(d, '-predictions.RDS')))
      } else {
        message('An Error Occurred. Continuing to next day...')
      }
    }
    gc(verbose = FALSE)
  }

  return(TRUE)
}


#' Get Season Metrics
#'
#' @description Calculates the Log Loss and Accuracy of the model by re-estimating m and rho daily and creating game odds
#'
#' @param schedule HockeyModel::schedule used to help with calculations
#' @param scores HockeyModel::scores used to compare to predictions
#'
#' @return a list of log loss and accuracy for the season
#' @export
getSeasonMetricsDC<-function(schedule = HockeyModel::schedule, scores = HockeyModel::scores){
  sched<-schedule
  sched$Home.WLD<-sched$Away.WLD<-sched$Draw.WLD<-sched$Home.WL<-sched$Away.WL<-sched$Result<-NA
  season_sofar<-scores[scores$Date > as.Date(getSeasonStartDate()), c("GameID", "Result")]

  sched<-predictMultipleDaysResultsDC(startDate = getSeasonStartDate(), endDate = Sys.Date)
  sched<-dplyr::left_join(sched, season_sofar, by="GameID", )

  sched$Home.WL<-(sched$HomeWin/(sched$HomeWin + sched$AwayWin))*sched$Draw + sched$HomeWin
  sched$Away.WL<-(sched$AwayWin/(sched$HomeWin + sched$AwayWin))*sched$Draw + sched$AwayWin

  sched<-sched[stats::complete.cases(sched), ]

  logloss<-logLoss(predicted = sched$Home.WL, actual = sched$Result)
  accuracy <- accuracy(predicted = sched$Home.WL>0.5, actual = sched$Result > 0.5)

  return(list("LogLoss" = logloss, "Accuracy" = accuracy))
}

#' Predict Multiple Days's W/L/D results
#'
#' @description Calculate what each days' game predictions were by recalculating rho and m using games only up to the previous day. This is kinda slow, so a full season might take an hour or more depending on your hardware. Split out of the SeasonMetrics code to help with requests by @JB4991 on twitter.
#'
#' @param startDate First day of predicted results
#' @param endDate Last day of predicted results
#' @param schedule HockeyModel::schedule
#' @param scores HockeyModel::scores
#'
#' @return a data frame like HockeyModel::schedule with HomeWin, AwayWin and Draw odds
#' @export
predictMultipleDaysResultsDC <- function(startDate, endDate, schedule = HockeyModel::schedule, scores = HockeyModel::scores){
  stopifnot(is.Date(startDate))
  stopifnot(is.Date(endDate))

  sched<-schedule[schedule$Date >= as.Date(startDate) & schedule$Date <= as.Date(endDate), ]

  for (day in unique(sched$Date)) {
    d<-as.Date(day, origin="1970-01-01")
    message('Results as of: ', d)
    score<-scores[scores$Date < day,]
    score<-score[score$Date > (as.Date(startDate) - 4000),]  # only feed in ~ 11 years data to calculate m & rho
    params<-list()
    params$m<-getM(scores = score, currentDate = d)
    params$rho<-getRho(m = params$m, scores = score)
    w.day <- getWeibullParams(m = params$m, rho=params$rho, scores = score)
    params$beta<-w.day$beta
    params$eta <- w.day$eta
    params$k <- w.day$k

    p<-todayDC(today = d, params=params)

    sched[sched$Date == d, "HomeWin"]<-p$HomeWin
    sched[sched$Date == d, "AwayWin"]<-p$AwayWin
    sched[sched$Date == d, "Draw"]<-p$Draw
  }

  return(sched)
}

parse_dc_params<-function(params=NULL){
  returnparams<-list()

  if("m" %in% names(params)){
    returnparams$m<-params$m
  } else {
    returnparams$m<-HockeyModel::m
  }
  if("rho" %in% names(params)){
    returnparams$rho<-params$rho
  } else {
    returnparams$rho<-HockeyModel::rho
  }

  if("beta" %in% names(params)){
    returnparams$beta<-params$beta
  } else {
    returnparams$beta<-HockeyModel::beta
  }

  if("eta" %in% names(params)){
    returnparams$eta<-params$eta
  } else {
    returnparams$eta<-HockeyModel::eta
  }

  if("k" %in% names(params)){
    returnparams$k<-params$k
  } else {
    returnparams$k<-HockeyModel::k
  }

  return(returnparams)
}
