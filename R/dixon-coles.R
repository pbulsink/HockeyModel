# Hosts dixon-coles specific functions. Many copied from my work from pbulsink.github.io
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
#' @param min_games_for_xg_weight Minimum number of complete games required to fit `xg_weight`. Defaults to 30.
#'
#' @return a named list containing m, rho, beta, eta, k and xg_weight values for the model.
#'
#' @seealso [m], [rho], [beta], [eta], [k]
#'
#' @export
updateDC <- function(
  scores = HockeyModel::scores,
  currentDate = Sys.Date(),
  save_data = TRUE,
  min_games_for_xg_weight = 30
) {
  message("Calculating new model parameters...")
  stopifnot(is.Date(currentDate))
  if (currentDate != Sys.Date()) {
    currentDate <- as.Date(currentDate)
    scores <- scores[scores$Date < currentDate, ]
    save_data <- FALSE
  }
  m <- getM(scores = scores, currentDate = currentDate)
  message("Solving for low scoring games...")
  rho <- getRho(m = m, scores = scores)
  message("Enhancing Tie Games")
  params_w <- getWeibullParams(m = m, rho = rho, scores = scores)
  beta <- params_w$beta
  eta <- params_w$eta
  k <- params_w$k

  # Fit xG blending weight using historical data (may return 0 if insufficient data)
  xg_weight <- tryCatch(
    fit_xg_weight(params = list(m = m, rho = rho, beta = beta, eta = eta, k = k), scores = scores, min_games = min_games_for_xg_weight),
    error = function(e) {
      message("Warning: fit_xg_weight failed: ", e$message)
      return(0)
    }
  )

  if (save_data && requireNamespace("usethis", quietly = TRUE)) {
    suppressMessages(usethis::use_data(m, rho, beta, eta, k, xg_weight, overwrite = TRUE))
  }
  return(list("m" = m, "rho" = rho, "beta" = beta, "eta" = eta, "k" = k, "xg_weight" = xg_weight))
}


#' DC Predictions Today
#'
#' @param today Generate predictions for this date. Defaults to today
#' @param params The named list containing m, rho, beta, eta, k, and optional xg_weight. See [updateDC] for information on the params list
#' @param schedule schedule to use, if not the built-in
#' @param expected_mean the mean lambda & mu, used only for regression
#' @param season_percent the percent complete of the season, used for regression
#' @param include_xG Whether to include team expected goals. default FALSE
#' @param use_xg Whether to use xG-adjusted lambdas when producing probabilities. Default FALSE
#' @param draws whether to report draws in odds (AwayWin/HomeWin/Draw) or not (AwayWin/HomeWin). Default True
#'
#' @return a data frame of HomeTeam, AwayTeam, HomeWin, AwayWin, Draw, GameID; or NULL if no games today
todayDC <- function(
  params = NULL,
  today = Sys.Date(),
  schedule = HockeyModel::schedule,
  expected_mean = NULL,
  season_percent = NULL,
  include_xG = FALSE,
  use_xg = FALSE,
  draws = TRUE
) {
  stopifnot(is.Date(today))
  params <- parse_dc_params(params)
  #games <- games_today(date = today)
  games <- schedule[schedule$Date == today, ]
  if (nrow(games) == 0) {
    return(NULL)
  }

  preds <- data.frame(
    HomeTeam = games$HomeTeam,
    AwayTeam = games$AwayTeam,
    HomeWin = 0,
    AwayWin = 0,
    Draw = 0,
    GameID = games$GameID,
    stringsAsFactors = FALSE
  )
  if (include_xG) {
    preds$Away_xG <- preds$Home_xG <- 0
  }
  for (i in seq_len(nrow(preds))) {
    p <- DCPredict(
      preds$HomeTeam[[i]],
      preds$AwayTeam[[i]],
      params = params,
      expected_mean = expected_mean,
      season_percent = season_percent,
      draws = draws,
      use_xg = use_xg
    )
    if (draws) {
      preds$HomeWin[[i]] <- p[[1]]
      preds$AwayWin[[i]] <- p[[3]]
      preds$Draw[[i]] <- p[[2]]
    } else {
      preds$HomeWin[[i]] <- p[[1]]
      preds$AwayWin[[i]] <- p[[2]]
    }

    if (include_xG) {
      xg <- dcxG(
        home = preds$HomeTeam[[i]],
        away = preds$AwayTeam[[i]],
        params = params,
        use_xg = use_xg
      )
      preds$Home_xG[[i]] <- xg$home
      preds$Away_xG[[i]] <- xg$away
    }
  }

  if (include_xG) {
    preds$GameID <- NULL
  }

  return(preds)
}


#' Playoff Odds DC
#'
#' @param home Series Home Ice Advantage Team Name
#' @param away Away (Opponent) Team Name
#' @param params The named list containing m, rho, beta, eta, k, and optional xg_weight. See [updateDC] for information on the params list
#' @param home_wins Number of wins for home ice advantage team thus far in series
#' @param away_wins Number of wins for away team thus far in series
#'
#' @return home ice advantage team odds to win series
#' @export
playoffDC <- function(home, away, params = NULL, home_wins = 0, away_wins = 0, use_xg = FALSE) {
  params <- parse_dc_params(params)
  # Odds of home ice advantage team win at home
  homeodds <- DCPredict(home = home, away = away, params = params, use_xg = use_xg)
  homeodds <- normalizeOdds(c(homeodds[1], homeodds[3]))[1]
  # Odds of home ice advantage team win away
  awayodds <- DCPredict(home = away, away = home, params = params, use_xg = use_xg)
  awayodds <- normalizeOdds(c(awayodds[3], awayodds[1]))[1]

  homewin <- playoffSeriesOdds(homeodds, awayodds, home_wins, away_wins)
  return(homewin)
}


#' DC remainder of season
#' @description Odds for each team to get to playoffs.
#'
#' @param nsims Number of simulations
#' @param cores The number of cores to use if using parallel processing, or 1 for single-core, NULL defaults to all cores or 1 if `parallel` package not installed.
#' @param scores the historical scores
#' @param schedule un-played future games
#' @param odds whether to return odds table or simulate season
#' @param regress whether to apply a regression to the mean for team strength on future predictions
#' @param mu_lambda whether to return team xG values. Can't be set true if odds is true
#' @param params The named list containing m, rho, beta, eta, k, and optional xg_weight. See [updateDC] for information on the params list
#'
#' @return data frame of Team, playoff odds.
#' @export
remainderSeasonDC <- function(
  nsims = 1e4,
  cores = NULL,
  params = NULL,
  scores = HockeyModel::scores,
  schedule = HockeyModel::schedule,
  odds = FALSE,
  regress = TRUE,
  mu_lambda = FALSE,
  use_xg = FALSE
) {
  odds_table <- data.frame(
    HomeTeam = character(),
    AwayTeam = character(),
    HomeWin = numeric(),
    AwayWin = numeric(),
    Draw = numeric(),
    GameID = numeric(),
    stringsAsFactors = FALSE
  )

  cores <- parseCores(cores)

  params <- parse_dc_params(params = params)

  last_game_date <- as.Date(max(scores$Date))
  schedule <- add_postponed_to_schedule_end(schedule)
  schedule <- schedule[schedule$Date > last_game_date, ]
  schedule <- schedule |>
    dplyr::arrange(.data$Date, .data$GameID)

  # cant regress through playoffs, turn off if not regular season anymore
  if (regress) {
    if (
      nrow(schedule[
        schedule$Date >= Sys.Date() & schedule$GameType == "R",
      ]) ==
        0
    ) {
      regress <- FALSE
    }
  }

  if (regress) {
    season_end_date <- as.Date(max(schedule[schedule$GameType == "R", ]$Date))
    season_start_date <- as.Date(min(c(
      scores[scores$Date > as.Date(getSeasonStartDate()), "Date"],
      schedule[schedule$Date > as.Date(getSeasonStartDate()), "Date"]
    )))
    season_length <- as.integer(season_end_date) - as.integer(season_start_date)
    remaining_length <- as.integer(season_end_date) - as.integer(last_game_date)
    expected_mean <- 2.835184
  } else {
    expected_mean <- NULL
    season_percent <- NULL
  }

  for (day in unique(schedule$Date)) {
    d <- as.Date(day, origin = "1970-01-01")
    if (regress) {
      # Adjust regress to mean
      season_percent <- (remaining_length -
        as.integer(season_end_date - as.Date(d))) /
        season_length
    }

    preds <- todayDC(
      today = d,
      schedule = schedule,
      season_percent = season_percent,
      expected_mean = expected_mean,
      params = params,
      use_xg = use_xg
    )
    preds$Date <- d
    odds_table <- rbind(odds_table, preds)
  }

  #odds_table$Date <- schedule$Date
  odds_table$GameID <- as.numeric(odds_table$GameID)

  if (odds) {
    return(odds_table)
  }

  if (mu_lambda) {
    odds_table$mu <- NA
    odds_table$lambda <- NA

    for (g in seq_len(nrow(odds_table))) {
      d <- as.Date(odds_table[g, "Date"], origin = "1970-01-01")
      # Expected goals home
      lambda <- try(
        stats::predict(
          HockeyModel::m,
          data.frame(
            Home = 1,
            Team = odds_table$HomeTeam[g],
            Opponent = odds_table$AwayTeam[g]
          ),
          type = "response"
        ),
        TRUE
      )

      # Expected goals away
      mu <- try(
        stats::predict(
          HockeyModel::m,
          data.frame(
            Home = 0,
            Team = odds_table$AwayTeam[g],
            Opponent = odds_table$HomeTeam[g]
          ),
          type = "response"
        ),
        TRUE
      )

      if (!is.numeric(lambda)) {
        lambda <- DCPredictErrorRecover(
          team = odds_table$HomeTeam[g],
          opponent = odds_table$AwayTeam[g],
          homeiceadv = TRUE
        )
      }
      if (!is.numeric(mu)) {
        mu <- DCPredictErrorRecover(
          team = odds_table$AwayTeam[g],
          opponent = odds_table$HomeTeam[g],
          homeiceadv = FALSE
        )
      }

      if (regress) {
        # Adjust regress to mean
        season_percent <- (remaining_length -
          as.integer(season_end_date - as.Date(d))) /
          season_length

        lambda <- lambda *
          (1 - 1 / 3 * season_percent) +
          expected_mean * (1 / 3 * season_percent)
        mu <- mu *
          (1 - 1 / 3 * season_percent) +
          expected_mean * (1 / 3 * season_percent)
      }
      odds_table[g, "lambda"] <- lambda
      odds_table[g, "mu"] <- mu
    }
    odds_table$HomeWin <- odds_table$AwayWin <- odds_table$Draw <- NULL
    return(odds_table)
  }

  summary_results <- simulateSeasonParallel(
    nsims = nsims,
    cores = cores,
    scores = scores,
    schedule = schedule
  )

  return(summary_results)
}

#' Compute one Dixon-Coles low-score adjustment factor
#'
#' @param xx (`integer(1)`) Home goals.
#' @param yy (`integer(1)`) Away goals.
#' @param lambda (`double(1)`) Home expected goals.
#' @param mu (`double(1)`) Away expected goals.
#' @param rho (`double(1)`) Low-score dependence parameter.
#' @returns (`double(1)`) Tau scaling factor for one score pair.
#' @keywords internal
tau_singular <- function(xx, yy, lambda, mu, rho) {
  if (xx == 0 && yy == 0) {
    return(1 - (lambda * mu * rho))
  } else if (xx == 0 && yy == 1) {
    return(1 + (lambda * rho))
  } else if (xx == 1 && yy == 0) {
    return(1 + (mu * rho))
  } else if (xx == 1 && yy == 1) {
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
tau <- Vectorize(tau_singular, c("xx", "yy", "lambda", "mu"))


#' Fast Dixon-Coles model fitting 'm'.
#'
#' @description Produces a DC model
#'
#' @param scores the historical scores to evaluate
#' @param currentDate (for date weight adjustment)
#' @param xi aggressiveness of date weighting
#' @param upsilon date weight cliff
#'
#' @export
#' @return a model 'm' of Dixon-Coles' type parameters.
getM <- function(
  scores = HockeyModel::scores,
  currentDate = Sys.Date(),
  xi = 0.00426,
  upsilon = 365
) {
  # stopifnot(is.Date(currentDate))
  currentDate <- as.Date(currentDate)

  scores <- scores[scores$Date >= (currentDate - 4000), ] # auto-trim to ~11 years of data, past then the model doesn't get better, just bigger
  df.indep <- data.frame(
    Date = c(scores$Date, scores$Date),
    GameID = c(scores$GameID, scores$GameID),
    Weight = c(
      DCweights(
        dates = scores$Date,
        currentDate = currentDate,
        xi = xi,
        upsilon = upsilon
      ),
      DCweights(
        dates = scores$Date,
        currentDate = currentDate,
        xi = xi,
        upsilon = upsilon
      )
    ),
    Team = as.factor(c(
      as.character(scores$HomeTeam),
      as.character(scores$AwayTeam)
    )),
    Opponent = as.factor(c(
      as.character(scores$AwayTeam),
      as.character(scores$HomeTeam)
    )),
    Goals = c(scores$HomeGoals, scores$AwayGoals),
    Home = c(rep(1, nrow(scores)), rep(0, nrow(scores)))
  )
  # df.indep <- df.indep[df.indep$Weight > 1e-8,]
  # Using a (+0) to remove intercept and give a value for each team instead of assuming 'Anaheim Ducks' = 0 (reference)
  m <- stats::glm(
    Goals ~ Team + Opponent + Home + 0,
    data = df.indep,
    weights = df.indep$Weight,
    family = stats::poisson(link = log),
    start = rep(0.01, length(unique(df.indep$Team)) * 2),
    model = FALSE
  )
  m <- cleanModel(m) # reduce M size
  return(m)
}

#' Generate a 'rho' factor for low scoring games
#'
#' @param m result from getM
#' @param scores the historical scores to evaluate
#'
#' @return a numeric value (typically -0.5 to 0)
#' @export
getRho <- function(m = HockeyModel::m, scores = HockeyModel::scores) {
  if (is.null(m)) {
    m <- getM(scores)
  }
  scores <- scores[scores$GameID %in% unique(m$data$GameID), ]
  expected <- stats::fitted(m)
  home.expected <- as.vector(expected[seq_len(nrow(scores))])
  away.expected <- as.vector(expected[(nrow(scores) + 1):(nrow(scores) * 2)])
  weights <- m$data$Weight[seq_len(nrow(scores))]

  DCoptimRhoFn.fast <- function(par) {
    rho <- par[1]
    DCRhoLogLik(
      y1 = scores$HomeGoals,
      y2 = scores$AwayGoals,
      mu = home.expected,
      lambda = away.expected,
      rho = rho,
      weights = weights
    )
  }

  res <- stats::optim(
    par = c(-0.1),
    fn = DCoptimRhoFn.fast,
    # control = list(fnscale = -1),
    method = "BFGS"
  )
  return(res$par)

  # of course, res$par is rho. Ranges from -0.2779 for last decade, -0.175 for 20152016 or 0.09 fo the whole league's history
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
getWeibullParams <- function(
  m = HockeyModel::m,
  rho = HockeyModel::rho,
  scores = HockeyModel::scores
) {
  if (is.null(m)) {
    m <- getM(scores)
  }
  if (is.null(rho)) {
    rho <- getRho(m = m, scores = scores)
  }

  # Have to grab the 'low score' of each tie game, then add one for modelling purposes (Weibull @ x=0 is 0)
  scores <- scores |>
    dplyr::filter(.data$GameID %in% unique(m$data$GameID)) |>
    dplyr::mutate(
      "weight" = m$data$Weight[seq_len(dplyr::n())],
      "mu" = stats::fitted(m)[seq_len(dplyr::n())],
      "lambda" = stats::fitted(m)[(dplyr::n() + 1):(dplyr::n() * 2)]
    ) |>
    dplyr::mutate(
      "Goals" = dplyr::case_when(
        .data$Result == 0.25 ~ .data$HomeGoals + 1,
        .data$Result == 0.4 ~ .data$HomeGoals + 1,
        .data$Result == 0.5 ~ .data$HomeGoals + 1,
        .data$Result == 0.6 ~ .data$AwayGoals + 1,
        .data$Result == 0.75 ~ .data$AwayGoals + 1,
        TRUE ~ NA_real_
      )
    )
  DCoptimTheta.fast <- function(par) {
    beta <- par[1]
    eta <- par[2]
    k <- par[3]
    # This gets multiplied by two because each tie game has 2 teams getting x goals, whereas non-tie games where either team get x goals get pulled in.
    goalratios <- sapply(1:max(scores$Goals, na.rm = TRUE), FUN = function(x) {
      (nrow(scores[scores$Goals == x & !is.na(scores$Goals), ]) /
        nrow(scores[!is.na(scores$Goals), ])) /
        (nrow(scores[
          (scores$HomeGoals == x | scores$AwayGoals == x) & is.na(scores$Goals),
        ]) /
          nrow(scores[is.na(scores$Goals), ]))
    }) *
      2
    weibulldist <- stats::dweibull(
      1:max(scores$Goals, na.rm = TRUE),
      shape = beta,
      scale = eta
    ) *
      k
    return(sum((goalratios - weibulldist)^2))
  }

  res <- stats::optim(
    par = c(3, 1, 6),
    fn = DCoptimTheta.fast,
    lower = c(-Inf, 0, -Inf),
    # control = list(fnscale=-1),
    method = "L-BFGS-B"
  )

  return(list("beta" = res$par[1], "eta" = res$par[2], "k" = res$par[3]))
}


#' DC Predict home/draw/away win
#'
#' @description Using Dixon-Coles' technique, predict odds each of home win, draw, and away win.
#'
#' @param home home team
#' @param away away team
#' @param params The named list containing m, rho, beta, eta, k, and optional xg_weight. See [updateDC] for information on the params list
#' @param maxgoal max number of goals per team
#' @param scores optional, if not supplying m & rho, scores used to calculate them.
#' @param expected_mean the mean lambda & mu, used only for regression
#' @param season_percent the percent complete of the season, used for regression
#' @param draws Whether draws are allowed. Default True
#' @param use_xg Whether to use xG-adjusted lambdas when producing the probability matrix (logical)
#' @param xg_weight Optional blending weight (0..1). If provided, blends xG-based and goal-based probabilities: combined = xg_weight * p_xg + (1-xg_weight) * p_goal
#'
#' @return a vector of home win, draw, and away win probability, or if draws=False, a vector of home and away win probability
#' @export
DCPredict <- function(
  home,
  away,
  params = NULL,
  maxgoal = 10,
  scores = HockeyModel::scores,
  expected_mean = NULL,
  season_percent = NULL,
  draws = TRUE,
  use_xg = FALSE,
  xg_weight = NULL
) {
  params <- parse_dc_params(params = params)

  # Use xg_weight from params if not explicitly supplied
  if (is.null(xg_weight)) {
    if ("xg_weight" %in% names(params)) {
      xg_weight <- params$xg_weight
    } else {
      xg_weight <- NULL
    }
  }

  # If xg_weight is provided and strictly between 0 and 1, blend the two distributions
  if (!is.null(xg_weight) && is.numeric(xg_weight) && xg_weight > 0 && xg_weight < 1) {
    w <- as.numeric(xg_weight)
    w <- min(max(w, 0), 1)

    pm_goal <- dcProbMatrix(
      home = home,
      away = away,
      params = params,
      maxgoal = maxgoal,
      scores = scores,
      expected_mean = expected_mean,
      season_percent = season_percent,
      use_xg = FALSE
    )

    pm_xg <- dcProbMatrix(
      home = home,
      away = away,
      params = params,
      maxgoal = maxgoal,
      scores = scores,
      expected_mean = expected_mean,
      season_percent = season_percent,
      use_xg = TRUE
    )

    p_goal <- c(sum(pm_goal[lower.tri(pm_goal)]), sum(diag(pm_goal)), sum(pm_goal[upper.tri(pm_goal)]))
    p_xg <- c(sum(pm_xg[lower.tri(pm_xg)]), sum(diag(pm_xg)), sum(pm_xg[upper.tri(pm_xg)]))

    p_combined <- w * p_xg + (1 - w) * p_goal
    odds <- normalizeOdds(p_combined)

    if (!draws) {
      HomeWinProbability <- p_combined[1] + normalizeOdds(c(p_combined[1], p_combined[3]))[1] * p_combined[2]
      AwayWinProbability <- p_combined[3] + normalizeOdds(c(p_combined[1], p_combined[3]))[2] * p_combined[2]
      odds <- normalizeOdds(c(HomeWinProbability, AwayWinProbability))
    }

    return(odds)
  }

  # If xg_weight == 1, use xG-based lambdas; if xg_weight == 0 or NULL, fall back to use_xg flag
  if (!is.null(xg_weight) && is.numeric(xg_weight) && xg_weight == 1) {
    probability_matrix <- dcProbMatrix(
      home = home,
      away = away,
      params = params,
      maxgoal = maxgoal,
      scores = scores,
      expected_mean = expected_mean,
      season_percent = season_percent,
      use_xg = TRUE
    )
  } else {
    probability_matrix <- dcProbMatrix(
      home = home,
      away = away,
      params = params,
      maxgoal = maxgoal,
      scores = scores,
      expected_mean = expected_mean,
      season_percent = season_percent,
      use_xg = use_xg
    )
  }

  HomeWinProbability <- sum(probability_matrix[lower.tri(probability_matrix)])
  DrawProbability <- sum(diag(probability_matrix))
  AwayWinProbability <- sum(probability_matrix[upper.tri(probability_matrix)])

  # Simple Adjust for under-predicting odds
  odds <- normalizeOdds(c(HomeWinProbability, DrawProbability, AwayWinProbability))

  if (!draws) {
    HomeWinProbability <- HomeWinProbability + normalizeOdds(c(HomeWinProbability, AwayWinProbability))[1] * DrawProbability
    AwayWinProbability <- AwayWinProbability + normalizeOdds(c(HomeWinProbability, AwayWinProbability))[2] * DrawProbability
    odds <- normalizeOdds(c(HomeWinProbability, AwayWinProbability))
  }
  return(odds)
}


# Fit an optimal blending weight between xG-based and goal-based predictions.
# The function minimizes log loss on historical games by finding w in [0,1]
# that minimizes logLoss( w * HomeWL_xg + (1-w) * HomeWL_goal , actualResult ).
fit_xg_weight <- function(params = NULL, scores = HockeyModel::scores, min_games = 30) {
  params <- parse_dc_params(params)
  if (is.null(scores) || nrow(scores) == 0) return(0)
  if (!all(c("HomeTeam", "AwayTeam", "Result") %in% names(scores))) return(0)

  n <- nrow(scores)
  p_goal_mat <- matrix(NA_real_, nrow = n, ncol = 3)
  p_xg_mat <- matrix(NA_real_, nrow = n, ncol = 3)
  results <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    row <- scores[i, ]
    if (is.na(row$HomeTeam) || is.na(row$AwayTeam) || is.na(row$Result)) next

    pm_goal <- try(
      dcProbMatrix(
        home = row$HomeTeam,
        away = row$AwayTeam,
        params = params,
        maxgoal = 10,
        scores = scores,
        use_xg = FALSE
      ),
      TRUE
    )
    pm_xg <- try(
      dcProbMatrix(
        home = row$HomeTeam,
        away = row$AwayTeam,
        params = params,
        maxgoal = 10,
        scores = scores,
        use_xg = TRUE
      ),
      TRUE
    )

    if (inherits(pm_goal, "try-error") || inherits(pm_xg, "try-error")) next

    p_goal <- c(sum(pm_goal[lower.tri(pm_goal)]), sum(diag(pm_goal)), sum(pm_goal[upper.tri(pm_goal)]))
    p_xg <- c(sum(pm_xg[lower.tri(pm_xg)]), sum(diag(pm_xg)), sum(pm_xg[upper.tri(pm_xg)]))

    if (!all(is.finite(p_goal)) || !all(is.finite(p_xg))) next

    p_goal_mat[i, ] <- p_goal
    p_xg_mat[i, ] <- p_xg
    results[i] <- as.numeric(row$Result)
  }

  valid <- which(!is.na(results) & rowSums(is.na(p_goal_mat)) == 0 & rowSums(is.na(p_xg_mat)) == 0)
  if (length(valid) < min_games) return(0)

  p_goal_mat <- p_goal_mat[valid, , drop = FALSE]
  p_xg_mat <- p_xg_mat[valid, , drop = FALSE]
  results_vec <- results[valid]

  # Objective: for a given w, compute combined Home.WL and return logLoss
  objective <- function(w) {
    p_comb <- w * p_xg_mat + (1 - w) * p_goal_mat
    homeWL <- numeric(nrow(p_comb))
    for (j in seq_len(nrow(p_comb))) {
      ph <- p_comb[j, 1]
      pd <- p_comb[j, 2]
      pa <- p_comb[j, 3]
      denom <- ph + pa
      if (!is.finite(denom) || denom <= 0) {
        homeWL[j] <- ph
      } else {
        homeWL[j] <- (ph / denom) * pd + ph
      }
    }
    return(logLoss(homeWL, results_vec))
  }

  res <- stats::optimize(f = objective, interval = c(0, 1))
  return(as.numeric(res$minimum))
}
#' DC Expected Goals
#'
#' @description Given a home and away team, provide lambda values
#'
#' @param home The home team name
#' @param away The away team name
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return a list of $home and $away Poisson Lambda values -
get_team_xg_stats <- function(scores = HockeyModel::scores) {
  if (is.null(scores)) return(NULL)
  if (!all(c("HomexG", "AwayxG", "HomeTeam", "AwayTeam") %in% names(scores))) {
    return(NULL)
  }

  df_home <- data.frame(
    Team = scores$HomeTeam,
    xg_for = as.numeric(scores$HomexG),
    xg_against = as.numeric(scores$AwayxG),
    stringsAsFactors = FALSE
  )
  df_away <- data.frame(
    Team = scores$AwayTeam,
    xg_for = as.numeric(scores$AwayxG),
    xg_against = as.numeric(scores$HomexG),
    stringsAsFactors = FALSE
  )
  long_df <- dplyr::bind_rows(df_home, df_away)
  long_df <- long_df[stats::complete.cases(long_df), ]
  if (nrow(long_df) == 0) return(NULL)

  team_stats <- long_df |>
    dplyr::group_by(.data$Team) |>
    dplyr::summarize(
      xg_for = mean(.data$xg_for, na.rm = TRUE),
      xg_against = mean(.data$xg_against, na.rm = TRUE),
      .groups = "drop"
    )
  team_xg_for <- setNames(team_stats$xg_for, team_stats$Team)
  team_xg_against <- setNames(team_stats$xg_against, team_stats$Team)
  league_avg <- mean(long_df$xg_for, na.rm = TRUE)
  return(list(team_xg_for = team_xg_for, team_xg_against = team_xg_against, league_avg = league_avg))
}


# Fit a global xG scaling ratio by regressing observed per-team-game xG on predicted
# lambdas from the current model. Returns a multiplicative ratio to convert
# predicted lambdas to xG-scaled lambdas (defaults to 1 if insufficient data).
fit_xg_ratio <- function(params = NULL, scores = HockeyModel::scores, min_games = 10, clamp = c(0.1, 10)) {
  params <- parse_dc_params(params)
  if (is.null(scores)) return(1)
  if (!all(c("HomexG", "AwayxG", "HomeTeam", "AwayTeam") %in% names(scores))) return(1)
  n <- nrow(scores)
  if (n == 0) return(1)

  pred_h <- numeric(n)
  pred_a <- numeric(n)

  for (i in seq_len(n)) {
    ph <- try(
      stats::predict(params$m, data.frame(Home = 1, Team = scores$HomeTeam[i], Opponent = scores$AwayTeam[i]), type = "response")[1],
      TRUE
    )
    if (!is.numeric(ph) || is.na(ph)) {
      ph <- tryCatch(
        DCPredictErrorRecover(team = scores$HomeTeam[i], opponent = scores$AwayTeam[i], homeiceadv = TRUE, m = params$m),
        error = function(e) NA
      )
    }

    pa <- try(
      stats::predict(params$m, data.frame(Home = 0, Team = scores$AwayTeam[i], Opponent = scores$HomeTeam[i]), type = "response")[1],
      TRUE
    )
    if (!is.numeric(pa) || is.na(pa)) {
      pa <- tryCatch(
        DCPredictErrorRecover(team = scores$AwayTeam[i], opponent = scores$HomeTeam[i], homeiceadv = FALSE, m = params$m),
        error = function(e) NA
      )
    }

    pred_h[i] <- as.numeric(ph)
    pred_a[i] <- as.numeric(pa)
  }

  obs_h <- as.numeric(scores$HomexG)
  obs_a <- as.numeric(scores$AwayxG)

  keep_h <- !is.na(pred_h) & !is.na(obs_h) & pred_h > 0 & is.finite(pred_h) & is.finite(obs_h)
  keep_a <- !is.na(pred_a) & !is.na(obs_a) & pred_a > 0 & is.finite(pred_a) & is.finite(obs_a)

  count <- sum(keep_h) + sum(keep_a)
  if (count < min_games) return(1)

  pred_all <- c(pred_h[keep_h], pred_a[keep_a])
  obs_all <- c(obs_h[keep_h], obs_a[keep_a])

  denom <- sum(pred_all^2, na.rm = TRUE)
  if (denom <= 0) return(1)

  ratio <- sum(obs_all * pred_all, na.rm = TRUE) / denom
  if (!is.finite(ratio) || ratio <= 0) ratio <- 1

  # Clamp to avoid extreme scaling
  ratio <- min(max(ratio, clamp[1]), clamp[2])
  return(as.numeric(ratio))
}


dcLambda <- function(home, away, params = NULL, scores = HockeyModel::scores, use_xg = FALSE) {
  params <- parse_dc_params(params = params)
  xg <- list("home" = NA, "away" = NA)

  # Expected goals home
  xg$home <- try(
    stats::predict(
      params$m,
      data.frame(Home = 1, Team = home, Opponent = away),
      type = "response"
    )[1],
    TRUE
  )

  # Expected goals away
  xg$away <- try(
    stats::predict(
      params$m,
      data.frame(Home = 0, Team = away, Opponent = home),
      type = "response"
    )[1],
    TRUE
  )

  if (!is.numeric(xg$home)) {
    xg$home <- DCPredictErrorRecover(
      team = home,
      opponent = away,
      homeiceadv = TRUE
    )
  }
  if (!is.numeric(xg$away)) {
    xg$away <- DCPredictErrorRecover(
      team = away,
      opponent = home,
      homeiceadv = FALSE
    )
  }

  # Optionally adjust predicted goals using fitted xG scaling ratio based on historical xG vs predicted lambdas.
  if (isTRUE(use_xg)) {
    ratio <- tryCatch(fit_xg_ratio(params = params, scores = scores), error = function(e) 1)
    if (!is.null(ratio) && is.numeric(ratio) && is.finite(ratio) && ratio > 0) {
      xg$home <- as.numeric(xg$home) * ratio
      xg$away <- as.numeric(xg$away) * ratio
    }
  }

  return(xg)
}

#' Compute expected goals from a Dixon-Coles probability matrix
#'
#' @param home (`character(1)`) Home team name.
#' @param away (`character(1)`) Away team name.
#' @param params (`list` or `NULL`) Dixon-Coles parameter list.
#' @param maxgoal (`integer(1)`) Maximum goals included per team.
#' @param scores (`data.frame`) Historical scores used for xG-adjusted lambdas.
#' @param use_xg (`logical(1)`) Whether to use xG-adjusted lambdas.
#' @returns (`list`) Home and away expected goals.
#' @keywords internal
dcxG <- function(home, away, params = NULL, maxgoal = 10, scores = HockeyModel::scores, use_xg = FALSE) {
  pm <- dcProbMatrix(
    home = home,
    away = away,
    params = params,
    maxgoal = maxgoal,
    scores = scores,
    use_xg = use_xg
  )

  away_xg <- stats::weighted.mean(0:maxgoal, colSums(pm))
  home_xg <- stats::weighted.mean(0:maxgoal, rowSums(pm))

  return(list("home" = home_xg, "away" = away_xg))
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
dcProbMatrix <- function(
  home,
  away,
  params = NULL,
  maxgoal = 10,
  scores = HockeyModel::scores,
  expected_mean = NULL,
  season_percent = NULL,
  use_xg = FALSE
) {
  params <- parse_dc_params(params = params)

  xg <- dcLambda(home = home, away = away, params = params, scores = scores, use_xg = use_xg)
  # Expected goals home
  lambda <- as.numeric(xg$home)

  # Expected goals away
  mu <- as.numeric(xg$away)

  if (!is.null(expected_mean) && !is.null(season_percent)) {
    lambda <- lambda *
      (1 - 1 / 3 * season_percent) +
      expected_mean * (1 / 3 * season_percent)
    mu <- mu *
      (1 - 1 / 3 * season_percent) +
      expected_mean * (1 / 3 * season_percent)
  }

  probability_matrix <- prob_matrix(
    lambda = lambda,
    mu = mu,
    params = params,
    maxgoal = maxgoal
  )

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
prob_matrix <- function(lambda, mu, params, maxgoal) {
  params <- parse_dc_params(params)
  probability_matrix <- stats::dpois(0:maxgoal, lambda) %*%
    t(stats::dpois(0:maxgoal, mu))

  # scaling_matrix <- matrix(tau(c(0, 1, 0, 1), c(0, 0, 1, 1), lambda, mu, params$rho), nrow = 2)
  # mvoed tau into vector - lookup and vectorized was too slow.
  scaling_matrix <- matrix(
    c(
      1 - (lambda * mu * params$rho),
      1 + (mu * params$rho),
      1 + (lambda * params$rho),
      1 - params$rho
    ),
    nrow = 2
  )
  probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix

  diag(probability_matrix) <- diag(probability_matrix) *
    stats::dweibull(
      c(1:(maxgoal + 1)),
      shape = params$beta,
      scale = params$eta
    ) *
    params$k
  # for(d in 1:(maxgoal+1)){
  # probability_matrix[d,d]<-probability_matrix[d,d] * (stats::dweibull(d, shape = params$beta, scale = params$eta) * params$k)
  # }

  # probability_matrix <- probability_matrix/sum(probability_matrix)  # turn into a sum=1 matrix
  #  #  Normalizing the whole matrix reduces the effect of the tie enhancement.

  renorm <- 1 - sum(diag(probability_matrix))
  normfact <- sum(
    probability_matrix[upper.tri(probability_matrix)],
    probability_matrix[lower.tri(probability_matrix)]
  ) /
    renorm

  probability_matrix[upper.tri(
    probability_matrix
  )] <- probability_matrix[upper.tri(probability_matrix)] / normfact
  probability_matrix[lower.tri(
    probability_matrix
  )] <- probability_matrix[lower.tri(probability_matrix)] / normfact

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
dcSample <- function(
  home,
  away,
  params = NULL,
  maxgoal = 10,
  scores = HockeyModel::scores,
  expected_mean = NULL,
  season_percent = NULL,
  as_result = TRUE,
  use_xg = FALSE
) {
  params <- parse_dc_params(params)
  pm <- dcProbMatrix(
    home = home,
    away = away,
    params = params,
    maxgoal = maxgoal,
    scores = scores,
    expected_mean = expected_mean,
    season_percent = season_percent,
    use_xg = use_xg
  )

  # sometimes there's negative probabilities. This handles that with fakign a very low value instead
  pm2 <- pm
  pm2[pm2 < 0] <- 1e-8

  goals <- as.vector(arrayInd(
    sample(seq_along(pm2), size = 1, prob = pm2),
    .dim = dim(pm2)
  )) -
    1

  if (goals[1] == goals[2]) {
    # TODO Verify OT/SO ratio and also verify if wniner is coin flip or stronger team has better chance?
    otstatus <- sample(c("OT", "SO"), size = 1, prob = c(0.6858606, 0.3141394))
    otwinner <- sample(
      c("Home", "Away"),
      size = 1,
      prob = extraTimeSolver(
        sum(pm[lower.tri(pm)]),
        sum(pm[upper.tri(pm)]),
        sum(diag(pm))
      )[2:3]
    )
    if (otwinner == "Home") {
      goals[1] <- goals[1] + 1
    } else {
      goals[2] <- goals[2] + 1
    }
  } else {
    otstatus <- ""
  }
  if (as_result) {
    return(dplyr::case_when(
      goals[1] > goals[2] & otstatus == "" ~ 1,
      goals[1] < goals[2] & otstatus == "" ~ 0,
      goals[1] > goals[2] & otstatus == "OT" ~ 0.75,
      goals[1] > goals[2] & otstatus == "SO" ~ 0.6,
      goals[1] < goals[2] & otstatus == "OT" ~ 0.25,
      goals[1] < goals[2] & otstatus == "SO" ~ 0.4
    ))
  } else {
    return(list(
      "HomeGoals" = goals[1],
      "AwayGoals" = goals[2],
      "OTStatus" = otstatus
    ))
  }
}


#' DC Result Sample
#'
#' @param lambda home team lambda
#' @param mu away team mu
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param maxgoal max goals predicable per game, default 10
#' @param nsim the number of simulations in each result
#'
#' @return a result from 0 to 1 corresponding to \link{scores} results
dcResult <- function(lambda, mu, params = NULL, maxgoal = 8, nsim = 1) {
  params <- parse_dc_params(params)

  dcr <- function(lambda, mu, params, maxgoal, nsim) {
    if (is.na(lambda)) {
      return(NA)
    }
    pm <- prob_matrix(
      lambda = lambda,
      mu = mu,
      params = params,
      maxgoal = maxgoal
    )

    homewinprob <- sum(pm[lower.tri(pm)])
    awaywinprob <- sum(pm[upper.tri(pm)])
    otwinnerprob <- extraTimeSolver(homewinprob, awaywinprob, sum(diag(pm)))[
      2:3
    ]
    resultprob <- c(
      homewinprob,
      otwinnerprob[1] * 0.6858606,
      otwinnerprob[1] * 0.3141394,
      otwinnerprob[2] * 0.3141394,
      otwinnerprob[1] * 0.6858606,
      awaywinprob
    )
    results <- sample(
      c(1, 0.75, 0.6, 0.4, 0.25, 0),
      size = nsim,
      replace = TRUE,
      prob = resultprob
    )
    return(results)
  }

  v_dcr <- Vectorize(dcr, c("lambda", "mu"))

  if (length(lambda) == 1) {
    return(dcr(lambda, mu, params, maxgoal, nsim))
  } else {
    return(as.vector(v_dcr(lambda, mu, params, maxgoal, nsim)))
  }
}


#' Sample one or more result labels from expanded win-state probabilities
#'
#' @param hw (`double`) Regulation home-win probability.
#' @param hot (`double`) Home overtime-win probability.
#' @param hso (`double`) Home shootout-win probability.
#' @param aso (`double`) Away shootout-win probability.
#' @param aot (`double`) Away overtime-win probability.
#' @param aw (`double`) Regulation away-win probability.
#' @param size (`integer(1)`) Number of samples to draw.
#' @returns (`numeric`) Result values encoded like `scores$Result`.
#' @keywords internal
sampleResult <- function(hw, hot, hso, aso, aot, aw, size = 1) {
  sr <- function(hw, hot, hso, aso, aot, aw, size) {
    return(sample(
      c(1, 0.75, 0.6, 0.4, 0.25, 0),
      size = size,
      replace = TRUE,
      prob = c(hw, hot, hso, aso, aot, aw)
    ))
  }
  v_sr <- Vectorize(sr, c("hw", "hot", "hso", "aso", "aot", "aw"))

  if (size == 1) {
    return(sr(hw, hot, hso, aso, aot, aw, size))
  } else {
    return(as.vector(v_sr(hw, hot, hso, aso, aot, aw, size)))
  }
}


#' Expand DC odds into regulation/OT/SO result components
#'
#' @param lambda (`double`) Home expected goals.
#' @param mu (`double`) Away expected goals.
#' @param params (`list` or `NULL`) Dixon-Coles parameter list.
#' @param maxgoal (`integer(1)`) Maximum goals included per team.
#' @returns (`numeric`) Probabilities for home win, home OT, home SO, away SO,
#'   away OT, and away win.
#' @keywords internal
dcExpandedOdds <- function(lambda, mu, params = NULL, maxgoal = 8) {
  params <- parse_dc_params(params)

  dceo <- function(lambda, mu, params, maxgoal, nsim) {
    if (is.na(lambda)) {
      return(NA)
    }
    pm <- prob_matrix(
      lambda = lambda,
      mu = mu,
      params = params,
      maxgoal = maxgoal
    )

    homewinprob <- sum(pm[lower.tri(pm)])
    awaywinprob <- sum(pm[upper.tri(pm)])
    otwinnerprob <- extraTimeSolver(homewinprob, awaywinprob, sum(diag(pm)))[
      2:3
    ]
    resultprob <- c(
      homewinprob,
      otwinnerprob[1] * 0.6858606,
      otwinnerprob[1] * 0.3141394,
      otwinnerprob[2] * 0.3141394,
      otwinnerprob[1] * 0.6858606,
      awaywinprob
    )
    return(resultprob)
  }

  v_dceo <- Vectorize(dceo, c("lambda", "mu"))

  if (length(lambda) == 1) {
    return(dceo(lambda, mu, params, maxgoal))
  } else {
    return(v_dceo(lambda, mu, params, maxgoal))
  }
}

#' DC Weight
#'
#' @param dates list of dates to calculate weights.
#' @param currentDate date from which to calculate weight
#' @param xi tuning factor
#'
#' @return list of weights corresponding to dates
#' @keywords internal
DCweights_old <- function(dates, currentDate = Sys.Date(), xi = 0.00426) {
  datediffs <- dates - as.Date(currentDate)
  datediffs <- as.numeric(datediffs * -1)
  w <- exp(-1 * xi * datediffs)
  w[datediffs <= 0] <- 0 # Future dates should have zero weights
  return(w)
}

#' Compute sigmoid time-decay weights for historical matches
#'
#' @param dates (`Date`) Match dates.
#' @param currentDate (`Date`) Reference date for weighting.
#' @param xi (`double(1)`) Logistic slope.
#' @param upsilon (`double(1)`) Logistic midpoint.
#' @returns (`double`) Weight per input date.
#' @keywords internal
DCweights <- function(
  dates,
  currentDate = Sys.Date(),
  xi = .01,
  upsilon = 365.25
) {
  datediffs <- dates - as.Date(currentDate)
  datediffs <- as.numeric(datediffs * -1)
  w <- 1 - 1 / (1 + exp(-xi * (datediffs - upsilon)))
  w[datediffs <= 0] <- 0 # Future dates should have zero weights
  return(w)
}

#' Evaluate Dixon-Coles log-likelihood for a rho value
#'
#' @param y1 (`integer`) Home goals.
#' @param y2 (`integer`) Away goals.
#' @param lambda (`double`) Home expected goals.
#' @param mu (`double`) Away expected goals.
#' @param rho (`double(1)`) Dependence parameter.
#' @param weights (`double` or `NULL`) Optional per-match weights.
#' @returns (`double(1)`) Summed (optionally weighted) log-likelihood.
#' @keywords internal
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


#' Recover fallback expected goals when model prediction fails
#'
#' @param team (`character(1)`) Team being predicted.
#' @param opponent (`character(1)`) Opponent team.
#' @param homeiceadv (`logical(1)`) Whether to include home-ice coefficient.
#' @param m (`glm`) Fitted Dixon-Coles model.
#' @returns (`double(1)`) Fallback expected goals estimate.
#' @keywords internal
DCPredictErrorRecover <- function(
  team,
  opponent,
  homeiceadv = FALSE,
  m = HockeyModel::m
) {
  teamlist <- unique(m$data$Team)
  opponentlist <- unique(m$data$Opponent)

  if (homeiceadv) {
    homeice <- m$coefficients["Home"]
  } else {
    homeice <- 0
  }

  if (!(team %in% teamlist) && !(opponent %in% opponentlist)) {
    lambda <- NA
  } else if (!(team %in% teamlist)) {
    teamp <- min(m$coefficients[grep("Team", names(m$coefficients))]) # lowest goals scored for new team
    opponentp <- m$coefficients[paste0("Opponent", opponent)]

    lambda <- exp(teamp + opponentp + homeice)
  } else if (!(opponent %in% opponentlist)) {
    teamp <- m$coefficients[paste0("Team", team)]
    opponentp <- max(m$coefficients[grep("Opponent", names(m$coefficients))]) # most goals allowed for new team

    lambda <- exp(teamp + opponentp + homeice)
  } else {
    lambda <- NA
  }

  if (is.na(lambda)) {
    if (homeiceadv) {
      lambda <- 3.319624 # Historical home goals
    } else {
      lambda <- 2.827417 # Historical away goals
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
#' @param likelihood_graphic Whether to call for creation of likelihood graphic. Default True
#'
#' @return true, if successful
#' @export
dcPredictMultipleDays <- function(
  start = as.Date(getSeasonStartDate()),
  end = Sys.Date(),
  scores = HockeyModel::scores,
  schedule = HockeyModel::schedule,
  filedir = getOption("HockeyModel.prediction.path"),
  nsims = 1e5,
  cores = NULL,
  likelihood_graphic = TRUE
) {
  if (!dir.exists(filedir)) {
    dir.create(filedir, recursive = TRUE)
  }
  cores <- parseCores(cores)

  stopifnot(is.Date(start))
  stopifnot(is.Date(end))
  predict_dates <- seq(from = as.Date(end), to = as.Date(start), by = -1) # do it backwards to get the most recent date done first

  schedule$Date <- as.Date(schedule$Date)
  schedule <- add_postponed_to_schedule_end(schedule)

  message("Running predictions for ", length(predict_dates), " day(s).")
  for (day in predict_dates) {
    d <- as.Date(day, origin = "1970-01-01")
    message("Predictions as of: ", d)
    score <- scores[scores$Date < day, ]
    score <- score[score$Date > as.Date("2008-08-01"), ]
    sched <- schedule[schedule$Date >= day, ]
    params <- updateDC(scores = score, currentDate = d)
    preds <- NULL

    # preds <- loopless_sim(nsims = nsims, cores = cores, scores = score, schedule = sched, params = params, likelihood_graphic=likelihood_graphic)
    preds <- tryCatch(
      expr = {
        message("Predicting with Loopless Sim")
        loopless_sim(
          nsims = nsims,
          cores = cores,
          scores = score,
          schedule = sched,
          params = params,
          likelihood_graphic = likelihood_graphic
        )
      },
      error = function(error) {
        message("An error occurred:")
        message(error)
        return(NULL)
      }
    )

    if (!is.null(preds) && "summary_results" %in% names(preds)) {
      message("Saving Prediction file...")
      saveRDS(
        preds$summary_results,
        file = file.path(filedir, paste0(d, "-predictions.RDS"))
      )
      if (d == Sys.Date()) {
        plot_point_likelihood(preds = preds$raw_results)
      }
    } else {
      message("An error occurred, retrying ", d, ".")
      preds <- NULL
      preds <- tryCatch(
        expr = {
          message("Predicting with Old Version Sim")
          remainderSeasonDC(
            nsims = nsims,
            scores = score,
            schedule = sched,
            regress = TRUE
          )
        },
        error = function(error) {
          message("An error occurred:")
          message(error)
          return(NULL)
        }
      )
      if (!is.null(preds) && "summary_results" %in% names(preds)) {
        message("Saving Prediction file...")
        saveRDS(
          preds$summary_results,
          file = file.path(filedir, paste0(d, "-predictions.RDS"))
        )
      } else {
        message("An Error Occurred. Continuing to next day...")
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
getSeasonMetricsDC <- function(
  schedule = HockeyModel::schedule,
  scores = HockeyModel::scores
) {
  sched <- schedule
  sched$Home.WLD <- sched$Away.WLD <- sched$Draw.WLD <- sched$Home.WL <- sched$Away.WL <- sched$Result <- NA
  season_sofar <- scores[
    scores$Date > as.Date(getSeasonStartDate()),
    c("GameID", "Result")
  ]

  sched <- predictMultipleDaysResultsDC(
    startDate = getSeasonStartDate(),
    endDate = Sys.Date()
  )
  sched <- dplyr::left_join(sched, season_sofar, by = "GameID", )

  sched$Home.WL <- (sched$HomeWin / (sched$HomeWin + sched$AwayWin)) *
    sched$Draw +
    sched$HomeWin
  sched$Away.WL <- (sched$AwayWin / (sched$HomeWin + sched$AwayWin)) *
    sched$Draw +
    sched$AwayWin

  sched <- sched[stats::complete.cases(sched), ]

  logloss <- logLoss(predicted = sched$Home.WL, actual = sched$Result)
  accuracy <- accuracy(
    predicted = sched$Home.WL > 0.5,
    actual = sched$Result > 0.5
  )

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
predictMultipleDaysResultsDC <- function(
  startDate,
  endDate,
  schedule = HockeyModel::schedule,
  scores = HockeyModel::scores
) {
  stopifnot(is.Date(startDate))
  stopifnot(is.Date(endDate))

  sched <- schedule[
    schedule$Date >= as.Date(startDate) & schedule$Date <= as.Date(endDate),
  ]

  for (day in unique(sched$Date)) {
    d <- as.Date(day, origin = "1970-01-01")
    message("Results as of: ", d)
    score <- scores[scores$Date < day, ]
    score <- score[score$Date > (as.Date(startDate) - 4000), ] # only feed in ~ 11 years data to calculate m & rho
    params <- list()
    params$m <- getM(scores = score, currentDate = d)
    params$rho <- getRho(m = params$m, scores = score)
    w.day <- getWeibullParams(m = params$m, rho = params$rho, scores = score)
    params$beta <- w.day$beta
    params$eta <- w.day$eta
    params$k <- w.day$k

    p <- todayDC(today = d, params = params)

    sched[sched$Date == d, "HomeWin"] <- p$HomeWin
    sched[sched$Date == d, "AwayWin"] <- p$AwayWin
    sched[sched$Date == d, "Draw"] <- p$Draw
  }

  return(sched)
}

#' Normalize nested Dixon-Coles parameter lists
#'
#' @param params (`list` or `NULL`) Candidate parameters, optionally nested
#'   under a `params` element.
#' @returns (`list`) Named list containing `m`, `rho`, `beta`, `eta`, and `k`.
#' @keywords internal
parse_dc_params <- function(params = NULL) {
  returnparams <- list()
  while ("params" %in% names(params)) {
    params <- params$params
  }

  if ("m" %in% names(params)) {
    returnparams$m <- params$m
  } else {
    returnparams$m <- HockeyModel::m
  }
  if ("rho" %in% names(params)) {
    returnparams$rho <- params$rho
  } else {
    returnparams$rho <- HockeyModel::rho
  }

  if ("beta" %in% names(params)) {
    returnparams$beta <- params$beta
  } else {
    returnparams$beta <- HockeyModel::beta
  }

  if ("eta" %in% names(params)) {
    returnparams$eta <- params$eta
  } else {
    returnparams$eta <- HockeyModel::eta
  }

  if ("k" %in% names(params)) {
    returnparams$k <- params$k
  } else {
    returnparams$k <- HockeyModel::k
  }

  # xg_weight: blended weight for xG vs goals. Prefer explicit param, then package-saved value, else default 0
  if ("xg_weight" %in% names(params)) {
    returnparams$xg_weight <- params$xg_weight
  } else {
    pkg_xw <- tryCatch(HockeyModel::xg_weight, error = function(e) NULL)
    if (!is.null(pkg_xw)) {
      returnparams$xg_weight <- pkg_xw
    } else {
      returnparams$xg_weight <- 0
    }
  }

  return(returnparams)
}
