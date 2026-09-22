# NHL season simulation engine and odds calculation

#' Simulate the remainder of the season
#'
#' @param scores Past (historical) season scores. Defaults to HockeyModel::Scores
#' @param schedule Future unplayed games. Defaults to HockeyModel::schedule
#' @param nsims number of simulations to run
#' @param cores number of cores to use in parallel.
#' @param progress whether to show a progress bar.
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return a data frame of results
#' @export
simulateSeasonParallel <- function(
  scores = HockeyModel::scores,
  nsims = 10000,
  schedule = HockeyModel::schedule,
  cores = NULL,
  progress = FALSE,
  params = NULL
) {
  teamlist <- c()
  if (!is.null(scores)) {
    season_sofar <- scores[scores$Date > as.Date(getSeasonStartDate()), ]
    season_sofar <- season_sofar[, c("Date", "HomeTeam", "AwayTeam", "Result")]
  } else {
    season_sofar <- NULL
  }

  teamlist <- c(
    teamlist,
    sort(unique(c(as.character(schedule$Home), as.character(schedule$Away))))
  )

  cores <- parseCores(cores)

  odds_table <- remainderSeasonDC(
    scores = scores,
    schedule = schedule,
    params = params,
    odds = TRUE
  )

  odds_table$HOT <- extraTimeSolver(
    odds_table$HomeWin,
    odds_table$AwayWin,
    odds_table$Draw
  )[, 2]
  odds_table$AOT <- extraTimeSolver(
    odds_table$HomeWin,
    odds_table$AwayWin,
    odds_table$Draw
  )[, 3]

  if (cores > 1 && requireNamespace("parallel", quietly = TRUE)) {
    `%dopar%` <- foreach::`%dopar%` # This hack passes R CMD CHK
    cl <- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)
    if (progress) {
      pb <- utils::txtProgressBar(max = nsims, style = 3)
      progress <- function(n) utils::setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
    } else {
      opts <- list()
    }
    all_results <- foreach::foreach(
      i = 1:nsims,
      .combine = "rbind",
      .options.snow = opts,
      .packages = c("HockeyModel")
    ) %dopar%
      {
        # Generate Games results once
        tmp <- odds_table
        tmp$res1 <- stats::runif(n = nrow(tmp))
        tmp$res2 <- stats::runif(n = nrow(tmp))
        tmp$Result <- 1 *
          (as.numeric(tmp$res1 < tmp$HomeWin)) +
          0.75 *
            (as.numeric(
              tmp$res1 > tmp$HomeWin & tmp$res1 < (tmp$HomeWin + tmp$HOT)
            ) *
              as.numeric(tmp$res2 < 0.6858606)) +
          0.6 *
            (as.numeric(
              tmp$res1 > tmp$HomeWin & tmp$res1 < (tmp$HomeWin + tmp$HOT)
            ) *
              as.numeric(tmp$res2 > 0.6858606)) +
          0.4 *
            (as.numeric(
              tmp$res1 > tmp$HomeWin &
                tmp$res1 < (tmp$HomeWin + tmp$HOT + tmp$AOT)
            ) *
              as.numeric(tmp$res2 > 0.6858606)) +
          0.25 *
            (as.numeric(
              tmp$res1 > tmp$HomeWin &
                tmp$res1 < (tmp$HomeWin + tmp$HOT + tmp$AOT)
            ) *
              as.numeric(tmp$res2 < 0.6858606)) +
          0

        tmp$HomeWin <- NULL
        tmp$AwayWin <- NULL
        tmp$HOT <- NULL
        tmp$AOT <- NULL
        tmp$Draw <- NULL
        tmp$res1 <- NULL
        tmp$res2 <- NULL

        tmp <- rbind(season_sofar, tmp)
        # Make the season table
        table <- buildStats(tmp)
        table$SimNo <- i

        table
      }
    if (progress) {
      close(pb)
    }
    parallel::stopCluster(cl)
    gc(verbose = FALSE)
  } else {
    if (cores > 1 && !requireNamespace("parallel", quietly = TRUE)) {
      message(
        "Parallel processing is only available if the parallels package is installed."
      )
    }
    all_results <- list()
    for (i in 1:nsims) {
      tmp <- odds_table
      tmp$HOT <- extraTimeSolver(tmp$HomeWin, tmp$AwayWin, tmp$Draw)[, 2]
      tmp$AOT <- extraTimeSolver(tmp$HomeWin, tmp$AwayWin, tmp$Draw)[, 3]
      tmp$res1 <- stats::runif(n = nrow(tmp))
      tmp$res2 <- stats::runif(n = nrow(tmp))
      tmp$Result <- 1 *
        (as.numeric(tmp$res1 < tmp$HomeWin)) +
        0.75 *
          (as.numeric(
            tmp$res1 > tmp$HomeWin & tmp$res1 < (tmp$HomeWin + tmp$HOT)
          ) *
            as.numeric(tmp$res2 < 0.6858606)) +
        0.6 *
          (as.numeric(
            tmp$res1 > tmp$HomeWin & tmp$res1 < (tmp$HomeWin + tmp$HOT)
          ) *
            as.numeric(tmp$res2 > 0.6858606)) +
        0.4 *
          (as.numeric(
            tmp$res1 > tmp$HomeWin &
              tmp$res1 < (tmp$HomeWin + tmp$HOT + tmp$AOT)
          ) *
            as.numeric(tmp$res2 > 0.6858606)) +
        0.25 *
          (as.numeric(
            tmp$res1 > tmp$HomeWin &
              tmp$res1 < (tmp$HomeWin + tmp$HOT + tmp$AOT)
          ) *
            as.numeric(tmp$res2 < 0.6858606)) +
        0

      tmp$HomeWin <- NULL
      tmp$AwayWin <- NULL
      tmp$HOT <- NULL
      tmp$AOT <- NULL
      tmp$Draw <- NULL
      tmp$res1 <- NULL
      tmp$res2 <- NULL

      tmp <- rbind(season_sofar, tmp)
      # Make the season table
      table <- buildStats(tmp)
      table$SimNo <- i
      all_results[[i]] <- table
    }
    all_results <- dplyr::bind_rows(all_results)
  }

  summary_results <- all_results |>
    dtplyr::lazy_dt() |>
    dplyr::group_by(.data$Team) |>
    dplyr::summarise(
      Playoffs = mean(.data$Playoffs),
      meanPoints = mean(.data$Points, na.rm = TRUE),
      maxPoints = max(.data$Points, na.rm = TRUE),
      minPoints = min(.data$Points, na.rm = TRUE),
      meanWins = mean(.data$W, na.rm = TRUE),
      maxWins = max(.data$W, na.rm = TRUE),
      Presidents = sum(.data$Rank == 1) / dplyr::n(),
      meanRank = mean(.data$Rank, na.rm = TRUE),
      bestRank = min(.data$Rank, na.rm = TRUE),
      # meanConfRank = mean(.data$ConfRank, na.rm = TRUE),
      # bestConfRank = min(.data$ConfRank, na.rm = TRUE),
      meanDivRank = mean(.data$DivRank, na.rm = TRUE),
      bestDivRank = min(.data$DivRank, na.rm = TRUE),
      sdPoints = stats::sd(.data$Points, na.rm = TRUE),
      sdWins = stats::sd(.data$W, na.rm = TRUE),
      sdRank = stats::sd(.data$Rank, na.rm = TRUE),
      # sdConfRank = stats::sd(.data$ConfRank, na.rm = TRUE),
      sdDivRank = stats::sd(.data$DivRank, na.rm = TRUE)
    ) |>
    tibble::as_tibble()

  return(list(summary_results = summary_results, raw_results = all_results))
}

#' Compile predictions to one object
#'
#' @description compiles predictions from a group of .RDS files to one data.frame
#' @param dir Directory holding the prediction .RDS files.
#'
#' @return a data frame.
#' @export
compile_predictions <- function(
  dir = getOption("HockeyModel.prediction.path")
) {
  pdates <- get_prediction_dates(dir)
  if (length(pdates) == 0L) {
    cli::cli_abort("No prediction files found in {.path {dir}}.")
  }
  all_predictions <- purrr::map(as.character(pdates), function(f) {
    readRDS(file.path(dir, paste0(f, "-predictions.RDS")))
  })
  names(all_predictions) <- as.character(pdates)
  all_predictions <- dplyr::bind_rows(all_predictions, .id = "predictionDate")
  return(all_predictions)
}


#' 'Loopless' simulation
#'
#' @param nsims number of simulations to run (approximate)
#' @param cores number of cores in parallel to process
#' @param schedule games to play
#' @param scores Season to this point
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param season_sofar The results of the season to date
#' @param likelihood_graphic whether to create a likelihood graphic
#' @param odds_table a table of odds for all games in schedule. Null, unless provided. Should be similar to the output of `remainderSeasonDC(odds=TRUE)`,
#' which is a data.frame of HomeTeam, AwayTeam, HomeWin, AwayWin, Draw, GameID and Date
#'
#' @return a two member list, of all results and summary results
#' @export
loopless_sim <- function(
  nsims = 1e5,
  cores = NULL,
  schedule = HockeyModel::schedule,
  scores = HockeyModel::scores,
  params = NULL,
  season_sofar = NULL,
  likelihood_graphic = TRUE,
  odds_table = NULL
) {
  params <- parse_dc_params(params)

  cores <- parseCores(cores)

  nsims <- floor(nsims / cores)

  schedule <- schedule[!(schedule$GameID %in% scores$GameID), ]

  schedule <- add_postponed_to_schedule_end(schedule)

  if (
    is.null(odds_table) ||
      !(all(
        c(
          "HomeTeam",
          "AwayTeam",
          "HomeWin",
          "AwayWin",
          "Draw",
          "GameID",
          "Date"
        ) %in%
          colnames(odds_table)
      ))
  ) {
    odds_table <- remainderSeasonDC(
      scores = scores,
      schedule = schedule,
      params = params,
      nsims = nsims,
      odds = TRUE
    )
  }

  season_sofar <- scores[
    scores$Date >=
      as.Date(getSeasonStartDate(getSeason(schedule[, "Date"][1]))),
  ]

  if (nrow(season_sofar) > 0) {
    season_sofar <- season_sofar[, c(
      "Date",
      "HomeTeam",
      "AwayTeam",
      "Result",
      "GameID"
    )]
    odds_table <- odds_table[!(odds_table$GameID %in% season_sofar$GameID), ]
    all_season <- dplyr::bind_rows(season_sofar, odds_table)
  } else {
    all_season <- odds_table
    all_season$Result <- NA
  }

  oddsseason <- extraTimeSolver(
    all_season$HomeWin,
    all_season$AwayWin,
    1 - (all_season$HomeWin + all_season$AwayWin)
  )
  all_season$HomeOT <- oddsseason[, 2] * 0.6858606
  all_season$HomeSO <- oddsseason[, 2] * 0.3141394
  all_season$AwaySO <- oddsseason[, 3] * 0.3141394
  all_season$AwayOT <- oddsseason[, 3] * 0.6858606

  rm(oddsseason, season_sofar, odds_table)

  if (cores == 1 || !requireNamespace("parallel", quietly = TRUE)) {
    if (cores > 1) {
      message("Multi-core processing requires the parallel package.")
    }
    # for testing only, really.
    all_results <- sim_engine(
      all_season = all_season,
      nsims = nsims,
      params = params
    )
  } else {
    # this fixes CRAN checks
    `%dopar%` <- foreach::`%dopar%`
    # `%do%` <- foreach::`%do%`

    cl <- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)

    # Ram management issues. Send smaller chunks more often, hopefully this helps.
    all_results <- foreach::foreach(
      i = seq_along(1:(cores * 100)),
      .combine = "rbind",
      .packages = "HockeyModel"
    ) %dopar%
      {
        all_results <- sim_engine(
          all_season = all_season,
          nsims = ceiling(nsims / 100),
          params = params
        )
        return(all_results)
      }

    parallel::stopCluster(cl)
    gc(verbose = FALSE)
  }

  summary_results <- all_results |>
    # dtplyr::lazy_dt() |>
    dplyr::group_by(.data$Team) |>
    dplyr::summarise(
      Playoffs = mean(.data$Playoffs),
      meanPoints = mean(.data$Points, na.rm = TRUE),
      maxPoints = max(.data$Points, na.rm = TRUE),
      minPoints = min(.data$Points, na.rm = TRUE),
      meanWins = mean(.data$W, na.rm = TRUE),
      maxWins = max(.data$W, na.rm = TRUE),
      Presidents = sum(.data$Rank == 1) / dplyr::n(),
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
      sdDivRank = stats::sd(.data$DivRank, na.rm = TRUE),
      p_rank1 = sum(.data$ConfRank == 1 & .data$DivRank == 1) / dplyr::n(),
      p_rank2 = sum(.data$ConfRank != 1 & .data$DivRank == 1) / dplyr::n(),
      # Solving 3 & 4 & 5 & 6 doesn't *really* matter, because 3/4 play the 5/6 within their own division.
      # In 2nd round, 1/8 or 2/7 play the 3/6 or 4/5 from their own division. No re-seeding occurs.
      # See: https://en.wikipedia.org/wiki/Stanley_Cup_playoffs#Current_format
      p_rank_34 = sum(.data$DivRank == 2) / dplyr::n(),
      p_rank_56 = sum(.data$DivRank == 3) / dplyr::n(),
      p_rank7 = sum(.data$Wildcard == 1) / dplyr::n(),
      p_rank8 = sum(.data$Wildcard == 2) / dplyr::n()
    ) |>
    tibble::as_tibble()

  if (likelihood_graphic) {
    plot_point_likelihood(preds = all_results)
  }

  return(list(summary_results = summary_results, raw_results = all_results))
}

#' Simulation engine to be parallelized or used in single core
#'
#' @param all_season One seasons' scores & odds schedule
#' @param nsims Number of simulations to run
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return results of `nsims` season simulations, as one long data frame score table.
#' @export
sim_engine <- function(all_season, nsims, params = NULL) {
  params <- parse_dc_params(params)

  season_length <- nrow(all_season)

  # multi_season<-dplyr::bind_rows(replicate(nsims, all_season[,c('HomeTeam', 'AwayTeam', 'Result', 'GameID')], simplify = FALSE))
  # multi_season$sim<-rep(1:nsims, each = season_length)

  resultslist <- list()

  # TODO: This can be vectorized or delooped by doing Result prediction on long_season?
  for (g in all_season$GameID) {
    if (is.na(all_season[all_season$GameID == g, ]$Result)) {
      odds <- as.vector(all_season[
        all_season$GameID == g,
        c("HomeWin", "HomeOT", "HomeSO", "AwaySO", "AwayOT", "AwayWin")
      ])
      # multi_season[multi_season$GameID == g,]$Result <- sampleResult(odds[[1]], odds[[2]], odds[[3]], odds[[4]], odds[[5]], odds[[6]], size=nsims)
      resultslist[[as.character(g)]] <- sampleResult(
        odds[[1]],
        odds[[2]],
        odds[[3]],
        odds[[4]],
        odds[[5]],
        odds[[6]],
        size = nsims
      )
    } else {
      resultslist[[as.character(g)]] <- rep(
        all_season[all_season$GameID == g, ]$Result,
        nsims
      )
    }
  }

  long_season <- data.frame(
    Team = c(
      rep(all_season$HomeTeam, each = nsims),
      rep(all_season$AwayTeam, each = nsims)
    ),
    SimNo = c(rep(1:nsims, season_length), rep(1:nsims, season_length)),
    Result = c(unlist(resultslist), 1 - unlist(resultslist))
  )

  rm(resultslist)

  all_results <- long_season |>
    dtplyr::lazy_dt() |>
    dplyr::group_by(.data$SimNo, .data$Team) |>
    dplyr::summarise(
      W = sum(.data$Result == 1),
      OTW = sum(.data$Result == 0.75),
      SOW = sum(.data$Result == 0.6),
      SOL = sum(.data$Result == 0.4),
      OTL = sum(.data$Result == 0.25),
      L = sum(.data$Result == 0)
    ) |>
    as.data.frame()

  rm(long_season)

  all_results$Points <- all_results$W *
    2 +
    all_results$OTW * 2 +
    all_results$SOW * 2 +
    all_results$OTL +
    all_results$SOL

  all_results$Conference <- unlist(getTeamConferences(all_results$Team))
  all_results$Division <- getTeamDivisions(all_results$Team)
  all_results$Wildcard <- 100

  all_results <- all_results |>
    dtplyr::lazy_dt() |>
    dplyr::group_by(.data$SimNo) |>
    dplyr::mutate(Rank = rank(-.data$Points, ties.method = "random")) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$SimNo, .data$Conference) |>
    dplyr::mutate(ConfRank = rank(.data$Rank)) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$SimNo, .data$Division) |>
    dplyr::mutate(DivRank = rank(.data$ConfRank)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Playoffs = ifelse(.data$DivRank <= 3, 1, 0)) |>
    dplyr::group_by(.data$SimNo, .data$Conference) |>
    dplyr::arrange(.data$Playoffs, .data$ConfRank) |>
    dplyr::mutate(
      Wildcard = ifelse(.data$Playoffs == 0, dplyr::row_number(), 100)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$SimNo, .data$Team) |>
    dplyr::select(
      .data$SimNo,
      .data$Team,
      .data$W,
      .data$OTW,
      .data$SOW,
      .data$SOL,
      .data$OTL,
      .data$Points,
      .data$Wildcard,
      .data$Rank,
      .data$ConfRank,
      .data$DivRank,
      .data$Playoffs
    ) |>
    tibble::as_tibble()

  all_results[
    !is.na(all_results$Wildcard) & all_results$Wildcard <= 2,
  ]$Playoffs <- 1
  all_results$Wildcard[is.na(all_results$Wildcard)] <- 0
  # all_results$Wildcard<-NULL

  return(all_results)
}


#' Today's Odds
#'
#' @description Determine today's games' odds (if today has games), or a specified date's odds
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param today The date for which you want game odds
#' @param schedule The schedule, default to internal schedule
#' @param expected_mean the mean lambda & mu, used only for regression
#' @param season_percent the percent complete of the season, used for regression
#' @param include_xG Whether to include xG values in reported odds
#'
#' @return a data frame of HomeTeam, AwayTeam, HomeWin, AwayWin, Draw, or NULL if no games today
#' @export
#'
#' @examples todayOdds(today = as.Date("2019-11-01"))
todayOdds <- function(
  params = NULL,
  today = Sys.Date(),
  schedule = HockeyModel::schedule,
  expected_mean = NULL,
  season_percent = NULL,
  include_xG = FALSE
) {
  return(todayDC(
    params = params,
    today = today,
    schedule = schedule,
    expected_mean = expected_mean,
    season_percent = season_percent,
    include_xG = include_xG
  ))
}


#' Precompute playoff win odds for all home-away team pairings
#'
#' @param teamlist (`character`) Team names to pair.
#' @param params (`list` or `NULL`) Dixon-Coles parameter list.
#' @returns (`data.frame`) Pairwise table with `HomeOdds`.
#' @keywords internal
getAllHomeAwayOdds <- function(teamlist, params = NULL) {
  params <- parse_dc_params(params)
  homeAwayOdds <- expand.grid(
    "HomeTeam" = teamlist,
    "AwayTeam" = teamlist,
    stringsAsFactors = FALSE
  )
  homeAwayOdds <- homeAwayOdds[homeAwayOdds$HomeTeam != homeAwayOdds$AwayTeam, ]
  homeAwayOdds$HomeOdds <- purrr::pmap_dbl(
    homeAwayOdds,
    function(HomeTeam, AwayTeam, ...) {
      playoffWin(HomeTeam, AwayTeam, params = params)
    }
  )
  return(homeAwayOdds)
}
