# NHL season simulation engine and odds calculation

#' Sample simulated results for odds-table games
#'
#' @description Draws the regulation/OT/SO outcome for every game in `odds_table`
#'   (`nrow(odds_table)` times, i.e. one outcome per simulation) using the
#'   two-draw `res1`/`res2` scheme. Only the sampled columns are built here; the
#'   invariant odds columns are not copied, so callers can reuse the same
#'   `odds_table` across simulations without re-copying it each iteration.
#'
#' @param odds_table (`data.frame`) Odds table with `HomeWin`, `HOT`, `AOT`
#'   (and any other columns). Used for its row count and the per-game odds.
#' @return `list(res1 = <double>, res2 = <double>, Result = <double>)` each of
#'   length `nrow(odds_table)`.
#' @keywords internal
sim_odds_results <- function(odds_table) {
  res1 <- stats::runif(n = nrow(odds_table))
  res2 <- stats::runif(n = nrow(odds_table))
  home_win <- odds_table$HomeWin
  hot <- odds_table$HOT
  aot <- odds_table$AOT
  in_ot <- as.numeric(res1 > home_win & res1 < (home_win + hot))
  in_so <- as.numeric(
    res1 > home_win &
      res1 < (home_win + hot + aot)
  )
  away_sos <- as.numeric(res2 > 0.6858606)
  away_ot <- as.numeric(res2 < 0.6858606)
  Result <-
    1 * (res1 < home_win) +
    0.75 * in_ot * away_ot +
    0.6 * in_ot * away_sos +
    0.4 * in_so * away_sos +
    0.25 * in_so * away_ot
  return(list(res1 = res1, res2 = res2, Result = Result))
}

#' Split a simulation count into worker chunks
#'
#' @description Distributes exactly `nsims` simulations across `n` chunks so the
#'   remainder is spread over the first `nsims %% n` chunks (no simulations are
#'   dropped or duplicated).
#'
#' @param nsims (`integer(1)`) Total number of simulations to distribute.
#' @param n (`integer(1)`) Number of chunks (worker tasks).
#' @return (`integer`) Vector of length `n` of chunk sizes whose sum equals
#'   `nsims`.
#' @keywords internal
simulation_chunks <- function(nsims, n) {
  base <- nsims %/% n
  rem <- nsims %% n
  sizes <- rep(base, n)
  if (rem > 0) {
    sizes[seq_len(rem)] <- base + 1
  }
  return(sizes)
}

#' Build the per-team stats table for a batch of simulations
#'
#' @description Runs `length(sim_ids)` simulations (one per element of
#'   `sim_ids`) against `odds_table`, building the per-team standings for each
#'   and tagging them with their simulation number. Lives at package level (not
#'   a closure) so it is visible to `parallel`/`doSNOW` workers, which only see
#'   the package's attached environment.
#'
#' @param sim_ids (`integer`) Simulation numbers to assign to each built table.
#' @param odds_table (`data.frame`) Odds table with `HomeTeam`, `AwayTeam`,
#'   `Date`, `GameID`, `HomeWin`, `HOT`, `AOT`.
#' @param season_sofar (`data.frame` or `NULL`) Past (played) season scores with
#'   `Date`, `HomeTeam`, `AwayTeam`, `Result`; prepended to every simulation.
#' @return (`tibble`) Per-team stats with a `SimNo` column, one block per
#'   simulation in `sim_ids`.
#' @keywords internal
sim_batch <- function(sim_ids, odds_table, season_sofar = NULL) {
  out <- lapply(sim_ids, function(i) {
    res <- sim_odds_results(odds_table)
    table <- buildStats(
      dplyr::bind_cols(
        season_sofar,
        odds_table[, c("HomeTeam", "AwayTeam", "Date", "GameID")],
        tibble::tibble(Result = res$Result)
      )
    )
    table$SimNo <- i
    table
  })
  dplyr::bind_rows(out)
}

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
    # Worker body. doSNOW workers only see the *attached* package namespace,
    # so we call the exported buildStats() directly (its own closure resolves
    # its internal helpers correctly). The result-sampling body is inlined
    # here rather than calling the internal sim_odds_results(), because
    # non-exported functions are not reliably callable from a worker. It uses
    # only base R + stats::runif, so it runs identically in a worker.
    run_sim_batch <- function(sim_ids, odds_table, season_sofar) {
      home_win <- odds_table$HomeWin
      hot <- odds_table$HOT
      aot <- odds_table$AOT
      base_cols <- odds_table[, c("HomeTeam", "AwayTeam", "Date", "GameID")]
      out <- lapply(sim_ids, function(i) {
        res1 <- stats::runif(n = nrow(odds_table))
        res2 <- stats::runif(n = nrow(odds_table))
        in_ot <- as.numeric(res1 > home_win & res1 < (home_win + hot))
        in_so <- as.numeric(res1 > home_win & res1 < (home_win + hot + aot))
        away_sos <- as.numeric(res2 > 0.6858606)
        away_ot <- as.numeric(res2 < 0.6858606)
        Result <-
          1 * (res1 < home_win) +
          0.75 * in_ot * away_ot +
          0.6 * in_ot * away_sos +
          0.4 * in_so * away_sos +
          0.25 * in_so * away_ot
        score_table <- dplyr::bind_cols(
          season_sofar,
          base_cols,
          tibble::tibble(Result = Result)
        )
        table <- buildStats(score_table)
        table$SimNo <- i
        table
      })
      dplyr::bind_rows(out)
    }

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
    # Run `nsims` simulations across `cores` batched tasks rather than one task
    # per simulation, so the invariant odds_table is serialised to each worker
    # only `cores` times instead of `nsims` times (#49).
    chunk_sizes <- simulation_chunks(nsims, cores)
    # Assign each simulation number (1..nsims) to a worker: worker i runs the
    # simulations whose index falls in its chunk.
    sim_to_worker <- rep(1:cores, times = chunk_sizes)
    all_results <- foreach::foreach(
      i = seq_len(cores),
      .combine = "rbind",
      .options.snow = opts,
      .packages = c("HockeyModel")
    ) %dopar%
      {
        ids <- which(sim_to_worker == i)
        run_sim_batch(ids, odds_table, season_sofar)
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
    all_results <- sim_batch(1:nsims, odds_table, season_sofar)
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
  params <- .parse_dc_params(params)

  cores <- parseCores(cores)

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

    # Distribute exactly `nsims` simulations across `cores` chunks so the
    # remainder is not dropped and the total matches the requested count
    # (previously `floor(nsims / cores)` lost the remainder, and `cores * 100`
    # tasks each ran `ceiling(nsims / 100)` simulations, so the total rarely
    # matched `nsims`). Each task also runs many simulations before returning,
    # reducing serialisation round-trips (#51, #49).
    chunk_sizes <- simulation_chunks(nsims, cores)
    all_results <- foreach::foreach(
      i = seq_len(cores),
      .combine = "rbind",
      .packages = "HockeyModel"
    ) %dopar%
      {
        sim_engine(
          all_season = all_season,
          nsims = chunk_sizes[i],
          params = params
        )
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
  params <- .parse_dc_params(params)

  season_length <- nrow(all_season)

  # multi_season<-dplyr::bind_rows(replicate(nsims, all_season[,c('HomeTeam', 'AwayTeam', 'Result', 'GameID')], simplify = FALSE))
  # multi_season$sim<-rep(1:nsims, each = season_length)

  # Pre-extract the per-game columns once so the loop below indexes by row
  # (O(G)) rather than re-scanning `all_season` by GameID every iteration
  # (O(G^2)). RNG draw order and per-game logic are unchanged.
  is_unplayed <- is.na(all_season$Result)
  hw <- all_season$HomeWin
  hot <- all_season$HomeOT
  hso <- all_season$HomeSO
  aso <- all_season$AwaySO
  aot <- all_season$AwayOT
  aw <- all_season$AwayWin
  played_result <- all_season$Result

  # Per-game result vectors (length nsims each) are collected in `home_res`
  # and `away_res` lists, then aggregated into per-team totals. This avoids
  # the 2*G*S row `long_season` intermediate the previous implementation
  # built before `group_by(SimNo, Team)` summarisation.
  teamlist <- sort(unique(c(
    all_season$HomeTeam,
    all_season$AwayTeam
  )))
  home_idx <- match(all_season$HomeTeam, teamlist)
  away_idx <- match(all_season$AwayTeam, teamlist)

  home_res <- vector("list", season_length)
  away_res <- vector("list", season_length)

  for (i in seq_len(season_length)) {
    if (is_unplayed[i]) {
      r <- sampleResult(
        hw[i],
        hot[i],
        hso[i],
        aso[i],
        aot[i],
        aw[i],
        size = nsims
      )
    } else {
      r <- rep(played_result[i], nsims)
    }
    home_res[[i]] <- r
    away_res[[i]] <- 1 - r
  }

  # Map each team to the indices of games where it is home / away
  home_games <- stats::setNames(
    lapply(seq_along(teamlist), function(t) which(home_idx == t)),
    teamlist
  )
  away_games <- stats::setNames(
    lapply(seq_along(teamlist), function(t) which(away_idx == t)),
    teamlist
  )

  # Sum a boolean mask over the games a team plays (home or away)
  sum_mask <- function(games_idx, res_list, value) {
    if (length(games_idx) == 0L) {
      return(rep(0, nsims))
    }
    out <- rep(0, nsims)
    for (g in games_idx) {
      out <- out + (res_list[[g]] == value)
    }
    out
  }

  all_results <- data.frame(
    SimNo = rep(1:nsims, length(teamlist)),
    Team = rep(teamlist, each = nsims)
  )
  # Each team's W/OTW/SOW counts both home and away wins; L/OTL/SOL counts
  # both home and away losses. The away result is `1 - home_result`, so e.g.
  # an away win (away_res == 1) corresponds to home_res == 0.
  all_results$W <- unlist(mapply(
    function(t) {
      sum_mask(home_games[[t]], home_res, 1) +
        sum_mask(away_games[[t]], away_res, 1)
    },
    seq_along(teamlist),
    SIMPLIFY = FALSE
  ))
  all_results$OTW <- unlist(mapply(
    function(t) {
      sum_mask(home_games[[t]], home_res, 0.75) +
        sum_mask(away_games[[t]], away_res, 0.75)
    },
    seq_along(teamlist),
    SIMPLIFY = FALSE
  ))
  all_results$SOW <- unlist(mapply(
    function(t) {
      sum_mask(home_games[[t]], home_res, 0.6) +
        sum_mask(away_games[[t]], away_res, 0.6)
    },
    seq_along(teamlist),
    SIMPLIFY = FALSE
  ))
  all_results$L <- unlist(mapply(
    function(t) {
      sum_mask(home_games[[t]], home_res, 0) +
        sum_mask(away_games[[t]], away_res, 0)
    },
    seq_along(teamlist),
    SIMPLIFY = FALSE
  ))
  all_results$OTL <- unlist(mapply(
    function(t) {
      sum_mask(home_games[[t]], home_res, 0.25) +
        sum_mask(away_games[[t]], away_res, 0.25)
    },
    seq_along(teamlist),
    SIMPLIFY = FALSE
  ))
  all_results$SOL <- unlist(mapply(
    function(t) {
      sum_mask(home_games[[t]], home_res, 0.4) +
        sum_mask(away_games[[t]], away_res, 0.4)
    },
    seq_along(teamlist),
    SIMPLIFY = FALSE
  ))

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
      "SimNo",
      "Team",
      "W",
      "OTW",
      "SOW",
      "SOL",
      "OTL",
      "Points",
      "Wildcard",
      "Rank",
      "ConfRank",
      "DivRank",
      "Playoffs"
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
  params <- .parse_dc_params(params)
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
