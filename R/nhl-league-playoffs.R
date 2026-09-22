# NHL playoff series odds and simulation

#' Playoff Win Calculator
#'
#' @description wraps PlayoffSeriesOdds with odds generator for a given home and away team
#' @param home_team Home Ice Advantage Team
#' @param away_team Opponent Team
#' @param home_wins Home Ice Advantage Team Wins in Series
#' @param away_wins Opponent Team Wins in Series
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return Odds from 0-1 of home team winning. Away odds are 1 - return value
#' @export
playoffWin <- function(
  home_team,
  away_team,
  home_wins = 0,
  away_wins = 0,
  params = NULL
) {
  params <- parse_dc_params(params)
  home_odds <- DCPredict(
    home = home_team,
    away = away_team,
    draws = FALSE,
    params = params
  )[1]
  away_odds <- 1 -
    DCPredict(
      home = away_team,
      away = home_team,
      draws = FALSE,
      params = params
    )[1]
  return(playoffSeriesOdds(
    home_odds = home_odds,
    away_odds = away_odds,
    home_win = home_wins,
    away_win = away_wins
  ))
}


#' Random Series Winner
#'
#' @description generate a random series winner given a home and away team
#'
#' @param home_team Home Team name (required)
#' @param away_team Away Team name (Required)
#' @param home_wins Number of home wins (default 0)
#' @param away_wins Number of away team wins (default 0)
#' @param homeAwayOdds pre-calculated home & away team parings odds of a home win. Overrides playoffwin calculation
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return TRUE if the home team wins, else FALSE
#' @export
randomSeriesWinner <- function(
  home_team,
  away_team,
  home_wins = 0,
  away_wins = 0,
  homeAwayOdds = NULL,
  params = NULL
) {
  if (is.null(homeAwayOdds)) {
    params <- parse_dc_params(params)
    return(ifelse(
      stats::runif(1) <
        playoffWin(
          home_team = home_team,
          away_team = away_team,
          home_wins = home_wins,
          away_wins = away_wins,
          params = params
        ),
      home_team,
      away_team
    ))
  } else {
    hao <- homeAwayOdds[
      homeAwayOdds$HomeTeam == home_team & homeAwayOdds$AwayTeam == away_team,
    ]
    if (nrow(hao) == 1) {
      return(ifelse(stats::runif(1) < hao$HomeOdds, home_team, away_team))
    } else {
      # Calculated odds aren't in there, get it manually
      params <- parse_dc_params(params)
      return(ifelse(
        stats::runif(1) <
          playoffWin(
            home_team = home_team,
            away_team = away_team,
            home_wins = home_wins,
            away_wins = away_wins,
            params = params
          ),
        home_team,
        away_team
      ))
    }
  }
}


#' Statistical Playoff Series Odds Solver
#'
#' @description Given home and away win odds, produce the odds of the 'home advantage' team winning the series. From \url{http://www.stat.umn.edu/geyer/playoff.html}, modified to function with odds determination.
#' @references \url{http://www.stat.umn.edu/geyer/playoff.html}
#'
#' @param home_odds Team odds with home-ice advantage at home
#' @param away_odds Team odds with home-ice advantage at away (on the road)
#' @param home_win Number of home ice advantage team wins thus far in the series. Default to 0 (prediction before series start)
#' @param away_win Number of away team wins thus far in the series
#' @param ngames Number of games in the series, defaults to 7
#' @param game_home vector of T/F for 'home team' home games. Defaults for NHL best of 7 series: \code{c(T,T,F,F,T,F,T)}
#' @param predict_games_to_win (Defualt False) If TRUE, returns the table of ways the series could finish.
#'
#' @return numeric odds of home team win series (1-odds for away odds)
#' @export
playoffSeriesOdds <- function(
  home_odds,
  away_odds,
  home_win = 0,
  away_win = 0,
  ngames = NULL,
  game_home = NULL,
  predict_games_to_win = FALSE
) {
  if (is.null(ngames)) {
    ngames <- 7
  }
  if (is.null(game_home) && ngames == 7) {
    game_home <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  } else {
    game_home <- rep(c(TRUE, FALSE), as.integer(ngames + 1 / 2))[1:ngames]
  }

  game_to <- ceiling(ngames / 2)

  if (length(home_odds) > 1 || length(away_odds) > 1) {
    cli::cli_abort(c(
      "Error in HockeyModel::playoffSeriesOdds()",
      "x" = "Expected single values for {.arg home_odds} and {.arg away_odds}, got vectors instead.",
      "i" = "Please retry with one home odds value and one away odds value."
    ))
  }
  p1_home <- home_odds
  p1_road <- away_odds

  if (p1_home < 0 || p1_home > 1 || p1_road < 0 || p1_road > 1) {
    cli::cli_abort(c(
      "Error in HockeyModel::playoffSeriesOdds()",
      "x" = "Expected probabilities between 0 and 1; impossible odds were provided.",
      "i" = "Please retry with {.arg home_odds} and {.arg away_odds} values in the range [0, 1]."
    ))
  }

  home_win_num <- suppressWarnings(as.numeric(home_win))
  away_win_num <- suppressWarnings(as.numeric(away_win))

  if (
    length(home_win) != 1 ||
      length(away_win) != 1 ||
      is.na(home_win_num) ||
      is.na(away_win_num) ||
      !is.finite(home_win_num) ||
      !is.finite(away_win_num) ||
      home_win_num != floor(home_win_num) ||
      away_win_num != floor(away_win_num)
  ) {
    cli::cli_abort(c(
      "Error in HockeyModel::playoffSeriesOdds()",
      "x" = "Expected single whole-number values for {.arg home_win} and {.arg away_win}.",
      "i" = "Please retry with one non-negative integer win count for each team."
    ))
  }

  home_win <- as.integer(home_win_num)
  away_win <- as.integer(away_win_num)
  if (home_win < 0 || away_win < 0) {
    cli::cli_abort(c(
      "Error in HockeyModel::playoffSeriesOdds()",
      "x" = "Expected non-negative win counts, got {.arg home_win} = {home_win} and {.arg away_win} = {away_win}.",
      "i" = "Please retry with zero or positive integers for wins already recorded."
    ))
  }
  if (home_win >= game_to) {
    cli::cli_alert_info(
      "Series already won; returning 1 for the home team win probability."
    )
    return(1)
  }
  if (away_win >= game_to) {
    cli::cli_alert_info(
      "Series already won; returning 0 for the home team win probability."
    )
    return(0)
  }

  games_played <- home_win + away_win

  if (games_played > ngames) {
    cli::cli_abort(c(
      "Error in HockeyModel::playoffSeriesOdds()",
      "x" = "Expected total recorded wins to be less than or equal to {.arg ngames}, got {games_played} wins over {ngames} games.",
      "i" = "Please retry with win counts that do not exceed the total number of games in the series."
    ))
  }
  if (games_played == ngames) {
    cli::cli_abort(c(
      "Error in HockeyModel::playoffSeriesOdds()",
      "x" = "Expected an unfinished series, but all {ngames} games are already recorded.",
      "i" = "Please retry with an in-progress series or use the known series outcome directly."
    ))
  }

  x.g <- games_played
  x.w1 <- home_win
  x.w2 <- away_win
  x.p <- 1.0
  finished_series <- NULL
  for (i in (games_played + 1):ngames) {
    p1now <- ifelse(game_home[i], p1_home, p1_road)
    l <- length(x.g)
    y.w1 <- c(x.w1 + 1, x.w1[l])
    y.w2 <- c(x.w2[1], x.w2 + 1)
    y.g <- c(x.g + 1, x.g[1] + 1)
    y.p <- c(x.p * p1now, 0)
    y.p <- y.p + c(0, x.p * (1 - p1now))
    unfinished_series <- y.w1 < game_to & y.w2 < game_to
    if (any(!unfinished_series)) {
      series <- cbind(y.g, y.w1, y.w2, y.p)
      finished_series <- rbind(finished_series, series[!unfinished_series, ])
    }
    x.g <- y.g[unfinished_series]
    x.w1 <- y.w1[unfinished_series]
    x.w2 <- y.w2[unfinished_series]
    x.p <- y.p[unfinished_series]
  }

  if (predict_games_to_win == FALSE) {
    p1total <- sum(finished_series[finished_series[, 2] == game_to, 4])

    return(p1total)
  } else {
    return(unfinished_series)
  }
}


#' simulate Playoffs
#'
#' @description Solves playoff odds by MC simulation.
#'
#' @param summary_results summary results
#' @param nsims Number of playoff sims to run. Too many takes a long time.
#' @param cores Number of processor cores to use
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return a data frame of each teams' odds of winning each round (First Round, Second Round, Conference Finals and Stanley Cup)
#' @export
simulatePlayoffs <- function(
  summary_results = NULL,
  nsims = 1e5,
  cores = NULL,
  params = NULL
) {
  params <- parse_dc_params(params)
  cores <- parseCores(cores)
  # TODO use compile_predictions for this?
  if (is.null(summary_results)) {
    pdates <- get_prediction_dates(getOption("HockeyModel.prediction.path"))
    if (length(pdates) == 0L) {
      cli::cli_alert_info(
        "No prediction files found; skipping playoff simulation."
      )
      return(invisible(NULL))
    }
    lastp <- max(pdates)
    if (lastp < Sys.Date() - 7) {
      cli::cli_alert_info(
        "Most recent prediction ({.val {lastp}}) is more than 7 days old; skipping playoff simulation."
      )
      return(invisible(NULL))
    }
    summary_results <- readRDS(file.path(
      getOption("HockeyModel.prediction.path"),
      paste0(lastp, "-predictions.RDS")
    ))
  }

  summary_results <- summary_results |>
    dplyr::mutate(
      "Conf" = getTeamConferences(.data$Team),
      "Div" = getTeamDivisions(.data$Team)
    )
  if ("p_rank3" %in% names(summary_results)) {
    # Shortcut for having prank3, 4, 5, 6 instead of prank34, and prank56. add them
    summary_results <- summary_results |>
      dplyr::mutate(
        "p_rank_34" = .data$p_rank3 + .data$p_rank4,
        "p_rank_56" = .data$p_rank5 + .data$p_rank6
      )
  }
  east_results <- summary_results |> dplyr::filter(.data$Conf == "Eastern")
  west_results <- summary_results |> dplyr::filter(.data$Conf == "Western")

  homeAwayOdds <- getAllHomeAwayOdds(summary_results$Team, params = params)

  simresults <- data.frame(
    "SimNo" = integer(),
    "l1" = character(),
    "l2" = character(),
    "l3" = character(),
    "l4" = character(),
    "l5" = character(),
    "l6" = character(),
    "l7" = character(),
    "l8" = character(),
    "series1" = character(),
    "series2" = character(),
    "series3" = character(),
    "series4" = character(),
    "series5" = character(),
    "series6" = character(),
    "series7" = character(),
    "series8" = character(),
    "series9" = character(),
    "series10" = character(),
    "series11" = character(),
    "series12" = character(),
    "series13" = character(),
    "series14" = character(),
    "series15" = character()
  )

  # currentSeries<-getAPISeries()

  currentSeries <- data.frame(
    "Round" = integer(),
    "Series" = integer(),
    "HomeTeam" = character(),
    "AwayTeam" = character(),
    "HomeWins" = integer(),
    "AwayWins" = integer(),
    "HomeSeed" = integer(),
    "AwaySeed" = integer(),
    "Status" = character(),
    "SeriesID" = integer()
  )

  if (nrow(currentSeries) == 0) {
    message("too early to mix in real-life series")
    completedSeries <- data.frame(
      "Series" = character(),
      "Winner" = character(),
      "Loser" = character()
    )
    currentSeries <- data.frame(
      "Round" = integer(),
      "Series" = integer(),
      "HomeTeam" = character(),
      "AwayTeam" = character(),
      "HomeWins" = integer(),
      "AwayWins" = integer(),
      "HomeSeed" = integer(),
      "AwaySeed" = integer(),
      "Status" = character(),
      "SeriesID" = integer()
    )
  } else {
    completedSeries <- getCompletedSeries(currentSeries)
    for (s in currentSeries[currentSeries$Status == "Ongoing", ]$SeriesID) {
      homeAwayOdds[
        homeAwayOdds$HomeTeam ==
          currentSeries[currentSeries$SeriesID == s, ]$HomeTeam &
          homeAwayOdds$AwayTeam ==
            currentSeries[currentSeries$SeriesID == s, ]$AwayTeam,
      ]$HomeOdds <-
        playoffWin(
          home_team = currentSeries[currentSeries$SeriesID == s, ]$HomeTeam,
          away_team = currentSeries[currentSeries$SeriesID == s, ]$AwayTeam,
          home_wins = currentSeries[currentSeries$SeriesID == s, ]$HomeWins,
          away_wins = currentSeries[currentSeries$SeriesID == s, ]$AwayWins,
          params = params
        )
    }
  }

  if (cores > 1 & !requireNamespace('doSNOW', quietly = TRUE)) {
    cli::cli_warn(
      "Warning: package {.pkg doSNOW} not installed. Reverting to single-core processing."
    )
    cores <- 1
  }
  if (cores > 1) {
    cl <- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)

    `%dopar%` <- foreach::`%dopar%`

    simresults <- foreach::foreach(
      i = 1:(cores * 100),
      .combine = "rbind"
    ) %dopar%
      {
        simresults <- playoffSolverEngine(
          nsims = ceiling(nsims / (cores * 100)),
          completedSeries = completedSeries,
          east_results = east_results,
          west_results = west_results,
          currentSeries = currentSeries,
          summary_results = summary_results,
          homeAwayOdds = homeAwayOdds
        )
        return(simresults)
      }

    parallel::stopCluster(cl)
    gc(verbose = FALSE)
  } else {
    # Single cores is easier for testing
    simresults <- playoffSolverEngine(
      nsims = nsims,
      completedSeries = completedSeries,
      east_results = east_results,
      west_results = west_results,
      currentSeries = currentSeries,
      summary_results = summary_results,
      homeAwayOdds = homeAwayOdds
    )
  }

  simodds <- data.frame("Team" = summary_results$Team)

  simodds <- simodds |>
    dplyr::rowwise() |>
    dplyr::mutate(
      "Make_Playoffs" = summary_results[
        summary_results$Team == .data$Team,
      ]$Playoffs,
      "Win_First_Round" = (nrow(simresults[
        simresults$series1 == .data$Team,
      ]) +
        nrow(simresults[simresults$series2 == .data$Team, ]) +
        nrow(simresults[simresults$series3 == .data$Team, ]) +
        nrow(simresults[simresults$series4 == .data$Team, ]) +
        nrow(simresults[simresults$series5 == .data$Team, ]) +
        nrow(simresults[simresults$series6 == .data$Team, ]) +
        nrow(simresults[simresults$series7 == .data$Team, ]) +
        nrow(simresults[simresults$series8 == .data$Team, ])) /
        nrow(simresults),
      "Win_Second_Round" = (nrow(simresults[
        simresults$series9 == .data$Team,
      ]) +
        nrow(simresults[simresults$series10 == .data$Team, ]) +
        nrow(simresults[simresults$series11 == .data$Team, ]) +
        nrow(simresults[simresults$series12 == .data$Team, ])) /
        nrow(simresults),
      "Win_Conference" = (nrow(simresults[
        simresults$series13 == .data$Team,
      ]) +
        nrow(simresults[simresults$series14 == .data$Team, ])) /
        nrow(simresults),
      "Win_Cup" = nrow(simresults[simresults$series15 == .data$Team, ]) /
        nrow(simresults)
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$Win_Cup),
      dplyr::desc(.data$Win_Conference),
      dplyr::desc(.data$Win_Second_Round),
      dplyr::desc(.data$Win_First_Round),
      dplyr::desc(.data$Make_Playoffs),
      .data$Team
    ) |>
    as.data.frame()

  return(simodds)
}

#' Order two teams by predicted seeding priority
#'
#' @param team1 (`character(1)`) First team.
#' @param team2 (`character(1)`) Second team.
#' @param summary_results (`data.frame`) Simulation summary with `meanPoints`.
#' @param p1 (`character(1)` or `NULL`) Optional forced first seed.
#' @returns (`character`) Length-two vector in seeding order.
#' @keywords internal
reseedTwoTeams <- function(team1, team2, summary_results, p1 = NULL) {
  t1p <- summary_results[summary_results$Team == team1, ]$meanPoints
  t2p <- summary_results[summary_results$Team == team2, ]$meanPoints

  if (!is.null(p1) && !is.null(nrow(p1)) && nrow(p1) > 0) {
    if (team1 == p1) {
      return(c(team1, team2))
    } else if (team2 == p1) {
      return(c(team2, team1))
    }
  }
  if (t1p > t2p) {
    return(c(team1, team2))
  } else if (t2p > t1p) {
    return(c(team2, team1))
  } else {
    if (stats::runif(1) < 0.5) {
      return(c(team1, team2))
    } else {
      return(c(team2, team1))
    }
  }
}

#' Given current seriess and a series number and home and away teams, either return true series winner, random series winner (with home/away wins considered) or random series winner
#'
#' @param series_number Series number from 1:15
#' @param currentSeries the full list of series returned from getAPISeries
#' @param homeTeam Home Team extracted from summary_results
#' @param awayTeam away Team extracted from summary_results
#' @param homeAwayOdds if calculated, the odds of a home or away team win
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return a series winner (team name)
single_series_solver <- function(
  series_number,
  currentSeries,
  homeTeam,
  awayTeam,
  homeAwayOdds = NULL,
  params = NULL
) {
  params <- parse_dc_params(params)
  if (is.na(currentSeries) || nrow(currentSeries) == 0) {
    return(randomSeriesWinner(
      homeTeam,
      awayTeam,
      homeAwayOdds = homeAwayOdds,
      params = params
    ))
  }
  series <- currentSeries[currentSeries$SeriesID == series_number, ]
  if (nrow(series[series$Status == "Complete", ]) == 1) {
    if (series$HomeTeam != homeTeam || series$AwayTeam != awayTeam) {
      warning(
        "Team Mismatch series ",
        series_number,
        ". Home Team expected ",
        series$HomeTeam,
        " got ",
        homeTeam,
        ". Away Team expected ",
        series$AwayTeam,
        " got ",
        awayTeam,
        ". Using API series information."
      )
    }
    return(ifelse(
      series$HomeWins > series$AwayWins,
      series$HomeTeam,
      series$AwayTeam
    ))
  } else if (
    nrow(currentSeries[
      currentSeries == series_number & currentSeries$Status == "Ongoing",
    ]) ==
      1
  ) {
    if (series$HomeTeam != homeTeam || series$AwayTeam != awayTeam) {
      warning(
        "Team Mismatch series ",
        series_number,
        ". Home Team expected ",
        series$HomeTeam,
        " got ",
        homeTeam,
        ". Away Team expected ",
        series$AwayTeam,
        " got ",
        awayTeam,
        ". Using API series information."
      )
    }
    return(randomSeriesWinner(
      series$HomeTeam,
      series$AwayTeam,
      home_wins = series$HomeWins,
      away_wins = series$AwayWins,
      homeAwayOdds = homeAwayOdds,
      params = params
    ))
  } else {
    return(randomSeriesWinner(
      homeTeam,
      awayTeam,
      homeAwayOdds = homeAwayOdds,
      params = params
    ))
  }
}

#' Build winner/loser summary for completed playoff series
#'
#' @param currentSeries (`data.frame`) Series table from [getAPISeries()].
#' @returns (`data.frame`) Series identifier with winner and loser teams.
#' @keywords internal
getCompletedSeries <- function(currentSeries) {
  completedSeries <- currentSeries |>
    dplyr::filter(.data$Status == "Complete") |>
    dplyr::mutate(
      "Winner" = dplyr::case_when(
        .data$HomeWins > .data$AwayWins ~ .data$HomeTeam,
        .data$HomeWins < .data$AwayWins ~ .data$AwayTeam
      ),
      "Loser" = dplyr::case_when(
        .data$HomeWins > .data$AwayWins ~ .data$AwayTeam,
        .data$HomeWins < .data$AwayWins ~ .data$HomeTeam
      ),
      "Series" = paste0("series", .data$SeriesID)
    ) |>
    dplyr::select(c("Series", "Winner", "Loser"))
  return(completedSeries)
}

#' Playoff Solver Engine
#'
#' @description Does the actual simulating. A function so it's parallelizable. Not to be called directly. Exported for parallel's use
#' @param nsims number of sims (in each core)
#' @param completedSeries completed series
#' @param east_results east_results
#' @param west_results west_results
#' @param currentSeries currentSeries
#' @param summary_results summary_results
#' @param homeAwayOdds precalculated home & away pairs of odds - if available.
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @export
playoffSolverEngine <- function(
  nsims,
  completedSeries,
  east_results,
  west_results,
  currentSeries,
  summary_results,
  homeAwayOdds,
  params = NULL
) {
  params <- parse_dc_params(params)
  simresults <- data.frame(
    "SimNo" = integer(),
    "l1" = character(),
    "l2" = character(),
    "l3" = character(),
    "l4" = character(),
    "l5" = character(),
    "l6" = character(),
    "l7" = character(),
    "l8" = character(),
    "series1" = character(),
    "series2" = character(),
    "series3" = character(),
    "series4" = character(),
    "series5" = character(),
    "series6" = character(),
    "series7" = character(),
    "series8" = character(),
    "series9" = character(),
    "series10" = character(),
    "series11" = character(),
    "series12" = character(),
    "series13" = character(),
    "series14" = character(),
    "series15" = character()
  )
  srvec <- c()

  east_results <- as.data.frame(east_results)
  west_results <- as.data.frame(west_results)

  for (sim in 1:nsims) {
    if (all(paste0("series", 1:8) %in% completedSeries$Series)) {
      series1 <- completedSeries[completedSeries$Series == "series1", ]$Winner
      series2 <- completedSeries[completedSeries$Series == "series2", ]$Winner
      series3 <- completedSeries[completedSeries$Series == "series3", ]$Winner
      series4 <- completedSeries[completedSeries$Series == "series4", ]$Winner
      series5 <- completedSeries[completedSeries$Series == "series5", ]$Winner
      series6 <- completedSeries[completedSeries$Series == "series6", ]$Winner
      series7 <- completedSeries[completedSeries$Series == "series7", ]$Winner
      series8 <- completedSeries[completedSeries$Series == "series8", ]$Winner
      l1 <- completedSeries[completedSeries$Series == "series1", ]$Loser
      l2 <- completedSeries[completedSeries$Series == "series2", ]$Loser
      l3 <- completedSeries[completedSeries$Series == "series3", ]$Loser
      l4 <- completedSeries[completedSeries$Series == "series4", ]$Loser
      l5 <- completedSeries[completedSeries$Series == "series5", ]$Loser
      l6 <- completedSeries[completedSeries$Series == "series6", ]$Loser
      l7 <- completedSeries[completedSeries$Series == "series7", ]$Loser
      l8 <- completedSeries[completedSeries$Series == "series8", ]$Loser
    } else {
      # solve east conference
      er <- east_results
      eastseries <- list() # p1 to p8 (conference winner, div #2, 2nd place, second div#2, second div#3, wc1, div #3, wc2)
      serieslist <- list()
      for (s in 1:4) {
        if (paste0("series", s) %in% completedSeries$series) {
          eastseries[paste0("p", s)] <- completedSeries[
            completedSeries$Series == paste0("series", s),
          ]$Winner
          eastseries[paste0("p", 9 - s)] <- completedSeries[
            completedSeries$Series == paste0("series", s),
          ]$Loser
          serieslist[paste0("series", s)] <- completedSeries[
            completedSeries$Series == paste0("series", s),
          ]$Winner
          serieslist[paste0("l", s)] <- completedSeries[
            completedSeries$Series == paste0("series", s),
          ]$Loser
          er <- er[
            er$Team !=
              completedSeries[
                completedSeries$Series == paste0("series", s),
              ]$Winner,
          ]
          er <- er[
            er$Team !=
              completedSeries[
                completedSeries$Series == paste0("series", s),
              ]$Loser,
          ]
        } else if (s %in% currentSeries$SeriesID) {
          eastseries[paste0("p", s)] <- currentSeries[
            currentSeries$SeriesID == s,
          ]$HomeTeam
          eastseries[paste0("p", 9 - s)] <- currentSeries[
            currentSeries$SeriesID == s,
          ]$AwayTeam
          er <- er[
            er$Team != currentSeries[currentSeries$SeriesID == s, ]$HomeTeam,
          ]
          er <- er[
            er$Team != currentSeries[currentSeries$SeriesID == s, ]$AwayTeam,
          ]
        }
      }
      if (!("p1" %in% names(eastseries))) {
        eastseries["p1"] <- er[
          sample(seq_len(nrow(er)), size = 1, prob = er$p_rank1),
        ]$Team
        er <- er[er$Team != eastseries["p1"], ]
        p1div <- getTeamDivisions(eastseries["p1"])
      } else {
        p1div <- getTeamDivisions(eastseries["p1"])
      }
      if (!("p2" %in% names(eastseries))) {
        eastseries["p2"] <- er[er$Div == p1div, ]$Team[sample(
          seq_len(nrow(er[er$Div == p1div, ])),
          size = 1,
          prob = er[er$Div == p1div, ]$p_rank_34
        )]
        er <- er[er$Team != eastseries["p2"], ]
        eastseries["p7"] <- er[er$Div == p1div, ]$Team[sample(
          seq_len(nrow(er[er$Div == p1div, ])),
          size = 1,
          prob = er[er$Div == p1div, ]$p_rank_56
        )]
        er <- er[er$Team != eastseries["p7"], ]
      }
      if (!("p3" %in% names(eastseries))) {
        eastseries["p3"] <- er[er$Div != p1div, ]$Team[sample(
          seq_len(nrow(er[er$Div != p1div, ])),
          size = 1,
          prob = er[er$Div != p1div, ]$p_rank2
        )]
        er <- er[er$Team != eastseries["p3"], ]
      }
      if (!("p4" %in% names(eastseries))) {
        eastseries["p4"] <- er[er$Div != p1div, ]$Team[sample(
          seq_len(nrow(er[er$Div != p1div, ])),
          size = 1,
          prob = er[er$Div != p1div, ]$p_rank_34
        )]
        er <- er[er$Team != eastseries["p4"], ]
        eastseries["p5"] <- er[er$Div != p1div, ]$Team[sample(
          seq_len(nrow(er[er$Div != p1div, ])),
          size = 1,
          prob = er[er$Div != p1div, ]$p_rank_56
        )]
        er <- er[er$Team != eastseries["p5"], ]
      }

      if (!("p6" %in% names(eastseries))) {
        eastseries["p6"] <- er[
          sample(seq_len(nrow(er)), size = 1, prob = er$p_rank7),
        ]$Team
        er <- er[er$Team != eastseries["p6"], ]
      }

      if (!("p8" %in% names(eastseries))) {
        eastseries["p8"] <- er[
          sample(seq_len(nrow(er)), size = 1, prob = er$p_rank8),
        ]$Team
        # er<-er[er$Team != eastseries['p8'],]
      }

      if (!("series1" %in% serieslist)) {
        series1 <- single_series_solver(
          series_number = 1,
          currentSeries = currentSeries,
          homeTeam = eastseries[["p1"]],
          awayTeam = eastseries[["p8"]],
          homeAwayOdds = homeAwayOdds
        )
        l1 <- ifelse(
          series1 == eastseries[["p1"]],
          eastseries[["p8"]],
          eastseries[["p1"]]
        )
      } else {
        series1 <- serieslist[["series1"]]
        l1 <- serieslist[["l1"]]
      }
      if (!("series2" %in% serieslist)) {
        series2 <- single_series_solver(
          series_number = 2,
          currentSeries = currentSeries,
          homeTeam = eastseries[["p2"]],
          awayTeam = eastseries[["p7"]],
          homeAwayOdds = homeAwayOdds
        )
        l2 <- ifelse(
          series2 == eastseries[["p2"]],
          eastseries[["p7"]],
          eastseries[["p2"]]
        )
      } else {
        series2 <- serieslist[["series2"]]
        l2 <- serieslist[["l2"]]
      }
      if (!("series3" %in% serieslist)) {
        series3 <- single_series_solver(
          series_number = 3,
          currentSeries = currentSeries,
          homeTeam = eastseries[["p3"]],
          awayTeam = eastseries[["p6"]],
          homeAwayOdds = homeAwayOdds
        )
        l3 <- ifelse(
          series3 == eastseries[["p3"]],
          eastseries[["p6"]],
          eastseries[["p3"]]
        )
      } else {
        series3 <- serieslist[["series3"]]
        l3 <- serieslist[["l3"]]
      }
      if (!("series4" %in% serieslist)) {
        series4 <- single_series_solver(
          series_number = 4,
          currentSeries = currentSeries,
          homeTeam = eastseries[["p4"]],
          awayTeam = eastseries[["p5"]],
          homeAwayOdds = homeAwayOdds
        )
        l4 <- ifelse(
          series4 == eastseries[["p4"]],
          eastseries[["p5"]],
          eastseries[["p4"]]
        )
      } else {
        series4 <- serieslist[["series4"]]
        l4 <- serieslist[["l4"]]
      }
      rm(er, serieslist, eastseries)

      wr <- west_results
      westseries <- list() # p1 to p8 (conference winner, div #2, 2nd place, second div#2, second div#3, wc1, div #3, wc2)
      serieslist <- list()
      for (s in 1:4) {
        # of course, west series first rounds are # 5-8, so s+4 for all references
        if (paste0("series", s) %in% completedSeries$series) {
          westseries[paste0("p", s)] <- completedSeries[
            completedSeries$Series == paste0("series", s + 4),
          ]$Winner
          westseries[paste0("p", 9 - s)] <- completedSeries[
            completedSeries$Series == paste0("series", s + 4),
          ]$Loser
          serieslist[paste0("series", s)] <- completedSeries[
            completedSeries$Series == paste0("series", s + 4),
          ]$Winner
          serieslist[paste0("l", s)] <- completedSeries[
            completedSeries$Series == paste0("series", s + 4),
          ]$Loser
          wr <- wr[
            wr$Team !=
              completedSeries[
                completedSeries$Series == paste0("series", s + 4),
              ]$Winner,
          ]
          wr <- wr[
            wr$Team !=
              completedSeries[
                completedSeries$Series == paste0("series", s + 4),
              ]$Loser,
          ]
        } else if (s %in% currentSeries$SeriesID) {
          westseries[paste0("p", s)] <- currentSeries[
            currentSeries$SeriesID == s + 4,
          ]$HomeTeam
          westseries[paste0("p", 9 - s)] <- currentSeries[
            currentSeries$SeriesID == s + 4,
          ]$AwayTeam
          wr <- wr[
            wr$Team !=
              currentSeries[currentSeries$SeriesID == s + 4, ]$HomeTeam,
          ]
          wr <- wr[
            wr$Team !=
              currentSeries[currentSeries$SeriesID == s + 4, ]$AwayTeam,
          ]
        }
      }
      if (!("p1" %in% names(westseries))) {
        westseries["p1"] <- wr[
          sample(seq_len(nrow(wr)), size = 1, prob = wr$p_rank1),
        ]$Team
        wr <- wr[wr$Team != westseries["p1"], ]
        p1div <- getTeamDivisions(westseries["p1"])
      } else {
        p1div <- getTeamDivisions(westseries["p1"])
      }
      if (!("p2" %in% names(westseries))) {
        westseries["p2"] <- wr[wr$Div == p1div, ]$Team[sample(
          seq_len(nrow(wr[wr$Div == p1div, ])),
          size = 1,
          prob = wr[wr$Div == p1div, ]$p_rank_34
        )]
        wr <- wr[wr$Team != westseries["p2"], ]
        westseries["p7"] <- wr[wr$Div == p1div, ]$Team[sample(
          seq_len(nrow(wr[wr$Div == p1div, ])),
          size = 1,
          prob = wr[wr$Div == p1div, ]$p_rank_56
        )]
        wr <- wr[wr$Team != westseries["p7"], ]
      }
      if (!("p3" %in% names(westseries))) {
        westseries["p3"] <- wr[wr$Div != p1div, ]$Team[sample(
          seq_len(nrow(wr[wr$Div != p1div, ])),
          size = 1,
          prob = wr[wr$Div != p1div, ]$p_rank2
        )]
        wr <- wr[wr$Team != westseries["p3"], ]
      }
      if (!("p4" %in% names(westseries))) {
        westseries["p4"] <- wr[wr$Div != p1div, ]$Team[sample(
          seq_len(nrow(wr[wr$Div != p1div, ])),
          size = 1,
          prob = wr[wr$Div != p1div, ]$p_rank_34
        )]
        wr <- wr[wr$Team != westseries["p4"], ]
        westseries["p5"] <- wr[wr$Div != p1div, ]$Team[sample(
          seq_len(nrow(wr[wr$Div != p1div, ])),
          size = 1,
          prob = wr[wr$Div != p1div, ]$p_rank_56
        )]
        wr <- wr[wr$Team != westseries["p5"], ]
      }

      if (!("p6" %in% names(westseries))) {
        westseries["p6"] <- wr[
          sample(seq_len(nrow(wr)), size = 1, prob = wr$p_rank7),
        ]$Team
        wr <- wr[wr$Team != westseries["p6"], ]
      }

      if (!("p8" %in% names(westseries))) {
        westseries["p8"] <- wr[
          sample(seq_len(nrow(wr)), size = 1, prob = wr$p_rank8),
        ]$Team
        # wr<-wr[wr$Team != westseries['p8'],]
      }

      if (!("series1" %in% serieslist)) {
        series5 <- single_series_solver(
          series_number = 5,
          currentSeries = currentSeries,
          homeTeam = westseries[["p1"]],
          awayTeam = westseries[["p8"]],
          homeAwayOdds = homeAwayOdds
        )
        l5 <- ifelse(
          series5 == westseries[["p1"]],
          westseries[["p8"]],
          westseries[["p1"]]
        )
      } else {
        series5 <- serieslist[["series1"]]
        l5 <- serieslist[["l1"]]
      }
      if (!("series2" %in% serieslist)) {
        series6 <- single_series_solver(
          series_number = 6,
          currentSeries = currentSeries,
          homeTeam = westseries[["p2"]],
          awayTeam = westseries[["p7"]],
          homeAwayOdds = homeAwayOdds
        )
        l6 <- ifelse(
          series6 == westseries[["p2"]],
          westseries[["p7"]],
          westseries[["p2"]]
        )
      } else {
        series6 <- serieslist[["series2"]]
        l6 <- serieslist[["l2"]]
      }
      if (!("series3" %in% serieslist)) {
        series7 <- single_series_solver(
          series_number = 7,
          currentSeries = currentSeries,
          homeTeam = westseries[["p3"]],
          awayTeam = westseries[["p6"]],
          homeAwayOdds = homeAwayOdds
        )
        l7 <- ifelse(
          series7 == westseries[["p3"]],
          westseries[["p6"]],
          westseries[["p3"]]
        )
      } else {
        series7 <- serieslist[["series3"]]
        l7 <- serieslist[["l3"]]
      }
      if (!("series4" %in% serieslist)) {
        series8 <- single_series_solver(
          series_number = 8,
          currentSeries = currentSeries,
          homeTeam = westseries[["p4"]],
          awayTeam = westseries[["p5"]],
          homeAwayOdds = homeAwayOdds
        )
        l8 <- ifelse(
          series8 == westseries[["p4"]],
          westseries[["p5"]],
          westseries[["p4"]]
        )
      } else {
        series8 <- serieslist[["series4"]]
        l8 <- serieslist[["l4"]]
      }
      rm(wr, serieslist, westseries)
    }

    # No reseeding for round 2 (but in reality yeah there is, wildCard doesn't have home advantage)
    if ("series9" %in% completedSeries$Series) {
      series9 <- completedSeries[completedSeries$Series == "series9", ]$Winner
    } else {
      rs <- reseedTwoTeams(
        series1,
        series2,
        summary_results,
        currentSeries[currentSeries$SeriesID == 1, ]$HomeTeam
      )
      series9 <- single_series_solver(
        series_number = 9,
        currentSeries = currentSeries,
        homeTeam = rs[1],
        awayTeam = rs[2],
        homeAwayOdds = homeAwayOdds
      )
    }

    if ("series10" %in% completedSeries$Series) {
      series10 <- completedSeries[completedSeries$Series == "series10", ]$Winner
    } else {
      rs <- reseedTwoTeams(
        series3,
        series4,
        summary_results,
        currentSeries[currentSeries$SeriesID == 3, ]$HomeTeam
      )
      series10 <- single_series_solver(
        series_number = 10,
        currentSeries = currentSeries,
        homeTeam = rs[1],
        awayTeam = rs[2],
        homeAwayOdds = homeAwayOdds
      )
    }

    if ("series11" %in% completedSeries$Series) {
      series11 <- completedSeries[completedSeries$Series == "series11", ]$Winner
    } else {
      rs <- reseedTwoTeams(
        series5,
        series6,
        summary_results,
        currentSeries[currentSeries$SeriesID == 5, ]$HomeTeam
      )
      series11 <- single_series_solver(
        series_number = 11,
        currentSeries = currentSeries,
        homeTeam = rs[1],
        awayTeam = rs[2],
        homeAwayOdds = homeAwayOdds
      )
    }

    if ("series12" %in% completedSeries$Series) {
      series12 <- completedSeries[completedSeries$Series == "series12", ]$Winner
    } else {
      rs <- reseedTwoTeams(
        series7,
        series8,
        summary_results,
        currentSeries[currentSeries$SeriesID == 7, ]$HomeTeam
      )
      series12 <- single_series_solver(
        series_number = 12,
        currentSeries = currentSeries,
        homeTeam = rs[1],
        awayTeam = rs[2],
        homeAwayOdds = homeAwayOdds
      )
    }

    # Reseed for conference finals & stanley cup finals
    if ("series13" %in% completedSeries$Series) {
      series13 <- completedSeries[completedSeries$Series == "series13", ]$Winner
    } else {
      rs <- reseedTwoTeams(series9, series10, summary_results)
      series13 <- single_series_solver(
        series_number = 13,
        currentSeries = currentSeries,
        homeTeam = rs[1],
        awayTeam = rs[2],
        homeAwayOdds = homeAwayOdds
      )
    }

    if ("series14" %in% completedSeries$Series) {
      series14 <- completedSeries[completedSeries$Series == "series14", ]$Winner
    } else {
      rs <- reseedTwoTeams(series11, series12, summary_results)
      series14 <- single_series_solver(
        series_number = 14,
        currentSeries = currentSeries,
        homeTeam = rs[1],
        awayTeam = rs[2],
        homeAwayOdds = homeAwayOdds
      )
    }

    # Stanley Cup Final
    if ("series15" %in% completedSeries$Series) {
      series15 <- completedSeries[completedSeries$Series == "series15", ]$Winner
    } else {
      rs <- reseedTwoTeams(series13, series14, summary_results)
      series15 <- single_series_solver(
        series_number = 15,
        currentSeries = currentSeries,
        homeTeam = rs[1],
        awayTeam = rs[2],
        homeAwayOdds = homeAwayOdds
      )
    }
    srvec <- c(
      srvec,
      sim,
      l1,
      l2,
      l3,
      l4,
      l5,
      l6,
      l7,
      l8,
      series1,
      series2,
      series3,
      series4,
      series5,
      series6,
      series7,
      series8,
      series9,
      series10,
      series11,
      series12,
      series13,
      series14,
      series15
    )
    rm(
      l1,
      l2,
      l3,
      l4,
      l5,
      l6,
      l7,
      l8,
      series1,
      series2,
      series3,
      series4,
      series5,
      series6,
      series7,
      series8,
      series9,
      series10,
      series11,
      series12,
      series13,
      series14,
      series15
    )
  }
  srdf <- as.data.frame(matrix(srvec, ncol = 24, byrow = TRUE))
  names(srdf) <- names(simresults)
  simresults <- dplyr::as_tibble(srdf)
  return(simresults)
}

#' Get Series Odds
#'
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return NULL if no series are currently set but not complete, else a data frame.
#' @export
getSeriesOdds <- function(params = NULL) {
  series <- getAPISeries()

  if (is.na(series)) {
    return(NULL)
  }
  if (nrow(series) == 0) {
    return(NULL)
  }

  series <- series[series$Status != "Complete", ]

  if (nrow(series) == 0) {
    return(NULL)
  }

  params <- parse_dc_params(params)

  series$HomeSeed <- NULL
  series$AwaySeed <- NULL
  series$SeriesID <- NULL
  series$HomeOdds <- 0
  for (i in seq_len(nrow(series))) {
    series[i, ]$HomeOdds <- playoffWin(
      series[i, ]$HomeTeam,
      series[i, ]$AwayTeam,
      series[i, ]$HomeWins,
      series[i, ]$AwayWins,
      params = params
    )
  }
  series$AwayOdds <- 1 - series$HomeOdds

  return(series)
}
