# PWHL Model
# Dixon-Coles model fitting, prediction, graphics, and summary functions for PWHL

#' Add `Result` column to PWHL scores
#'
#' @description Converts the `HomeGoals`, `AwayGoals`, and `OTStatus` columns
#'   of a PWHL scores data frame into the numeric `Result` encoding used by the
#'   Dixon-Coles model (same scale as [HockeyModel::scores]):
#'   1 = home regulation win, 0.75 = home OT win, 0.6 = home SO win,
#'   0.4 = away SO win, 0.25 = away OT win, 0 = away regulation win.
#'
#' @param scores (`data.frame`) PWHL scores as returned by [getPWHLScores()].
#'
#' @returns The same data frame with an added `Result` column.
#' @keywords internal
pwhl_add_result <- function(scores) {
  scores$Result <- dplyr::case_when(
    scores$HomeGoals > scores$AwayGoals & scores$OTStatus == "" ~ 1,
    scores$HomeGoals > scores$AwayGoals & scores$OTStatus == "OT" ~ 0.75,
    scores$HomeGoals > scores$AwayGoals & scores$OTStatus == "SO" ~ 0.6,
    scores$HomeGoals < scores$AwayGoals & scores$OTStatus == "SO" ~ 0.4,
    scores$HomeGoals < scores$AwayGoals & scores$OTStatus == "OT" ~ 0.25,
    scores$HomeGoals < scores$AwayGoals & scores$OTStatus == "" ~ 0,
    TRUE ~ NA_real_
  )
  scores
}


#' Update PWHL Dixon-Coles parameters
#'
#' @description Fits the Dixon-Coles model to PWHL scores data and returns (and
#'   optionally saves) the five model parameters: `pwhl_m`, `pwhl_rho`,
#'   `pwhl_beta`, `pwhl_eta`, and `pwhl_k`.
#'
#' @param scores (`data.frame`) PWHL game scores. Defaults to
#'   [HockeyModel::pwhlScores].
#' @param currentDate (`Date`) Reference date for time-weighting. Defaults to
#'   today.
#' @param save_data (`logical(1)`) If `TRUE` and `usethis` is installed, writes
#'   the parameters as package data objects.
#'
#' @returns A named list with elements `m`, `rho`, `beta`, `eta`, and `k`.
#' @export
updatePWHLDC <- function(
  scores = HockeyModel::pwhlScores,
  currentDate = Sys.Date(),
  save_data = TRUE
) {
  if (!is.Date(currentDate)) {
    cli::cli_abort("{.arg currentDate} must be a Date or date-like value.")
  }
  if (nrow(scores) == 0) {
    cli::cli_abort(
      "No PWHL scores to fit. Run {.fn updatePWHLScoresAPI} first."
    )
  }

  scores <- scores[scores$GameStatus == "Final", ]
  if (nrow(scores) == 0) {
    cli::cli_abort("No finalised PWHL games found in {.arg scores}.")
  }

  scores <- pwhl_add_result(scores)

  if (currentDate != Sys.Date()) {
    currentDate <- as.Date(currentDate)
    scores <- scores[scores$Date < currentDate, ]
    save_data <- FALSE
  }

  cli::cli_inform("Fitting PWHL Dixon-Coles model (m)...")
  pwhl_m <- getM(scores = scores, currentDate = currentDate)

  cli::cli_inform("Solving for PWHL low-scoring games (rho)...")
  pwhl_rho <- getRho(m = pwhl_m, scores = scores)

  cli::cli_inform("Fitting PWHL tie-game enhancement (beta, eta, k)...")
  pwhl_params <- getWeibullParams(m = pwhl_m, rho = pwhl_rho, scores = scores)
  pwhl_beta <- pwhl_params$beta
  pwhl_eta <- pwhl_params$eta
  pwhl_k <- pwhl_params$k

  if (save_data && requireNamespace("usethis", quietly = TRUE)) {
    suppressMessages(usethis::use_data(
      pwhl_m,
      pwhl_rho,
      pwhl_beta,
      pwhl_eta,
      pwhl_k,
      overwrite = TRUE
    ))
  }

  list(
    m = pwhl_m,
    rho = pwhl_rho,
    beta = pwhl_beta,
    eta = pwhl_eta,
    k = pwhl_k
  )
}


#' Parse PWHL Dixon-Coles parameter list
#'
#' @description Like [parse_dc_params()] but falls back to the PWHL-specific
#'   package data objects (`pwhl_m`, `pwhl_rho`, `pwhl_beta`, `pwhl_eta`,
#'   `pwhl_k`) instead of the NHL ones.
#'
#' @param params (`list` or `NULL`) Candidate parameters, optionally nested
#'   under a `params` key.
#'
#' @returns A named list with elements `m`, `rho`, `beta`, `eta`, and `k`.
#' @keywords internal
parse_pwhl_dc_params <- function(params = NULL) {
  while ("params" %in% names(params)) {
    params <- params$params
  }

  list(
    m = if ("m" %in% names(params)) params$m else HockeyModel::pwhl_m,
    rho = if ("rho" %in% names(params)) params$rho else HockeyModel::pwhl_rho,
    beta = if ("beta" %in% names(params)) {
      params$beta
    } else {
      HockeyModel::pwhl_beta
    },
    eta = if ("eta" %in% names(params)) params$eta else HockeyModel::pwhl_eta,
    k = if ("k" %in% names(params)) params$k else HockeyModel::pwhl_k
  )
}


#' Update the PWHL model
#'
#' @description Fetches the latest PWHL schedule and scores then re-fits the
#'   Dixon-Coles model parameters. Returns a list with `scores`, `schedule`,
#'   and `params` — the same shape as [updateModel()].
#'
#' @param save_data (`logical(1)`) Whether to persist the updated data and
#'   model parameters as package data.
#'
#' @returns A named list with elements `scores`, `schedule`, and `params`.
#' @export
updatePWHLModel <- function(save_data = TRUE) {
  cli::cli_inform("Updating PWHL Schedule")
  schedule <- updatePWHLScheduleAPI(save_data = save_data)

  cli::cli_inform("Updating PWHL Scores")
  scores <- updatePWHLScoresAPI(
    schedule = schedule,
    save_data = save_data
  )

  cli::cli_inform("Refitting PWHL Model Parameters")
  params <- updatePWHLDC(scores = scores, save_data = save_data)

  list(
    scores = scores,
    schedule = schedule,
    params = params
  )
}


# ── Season-state helpers ──────────────────────────────────────────────────────

#' Is there an active PWHL season?
#'
#' @description Returns `TRUE` if `date` falls within the first and last game
#'   dates recorded in `schedule`, i.e. the PWHL season has started but not
#'   yet finished.
#'
#' @param date (`Date`) Date to test. Defaults to today.
#' @param schedule (`data.frame`) PWHL schedule. Defaults to
#'   [HockeyModel::pwhlSchedule].
#'
#' @returns `logical(1)`.
#' @export
pwhl_in_season <- function(
  date = Sys.Date(),
  schedule = HockeyModel::pwhlSchedule
) {
  if (!inherits(date, "Date")) {
    cli::cli_abort("{.arg date} must be a Date object, not a {class(date)}.")
  }
  if (nrow(schedule) == 0) {
    return(FALSE)
  }
  date >= min(schedule$Date) && date <= max(schedule$Date)
}


#' PWHL season start date
#'
#' @param schedule (`data.frame`) PWHL schedule. Defaults to
#'   [HockeyModel::pwhlSchedule].
#'
#' @returns (`Date`) First game date in the schedule, or `NA` if empty.
#' @keywords internal
pwhl_season_start_date <- function(schedule = HockeyModel::pwhlSchedule) {
  if (nrow(schedule) == 0) {
    return(as.Date(NA_character_))
  }
  min(schedule[schedule$GameType == "R", ]$Date, na.rm = TRUE)
}


# ── PWHL Season Simulation ────────────────────────────────────────────────────

#' Simulate the remainder of the PWHL season (loopless)
#'
#' @description Runs Monte Carlo simulations of the remaining PWHL regular
#'   season and returns per-team season summary statistics.
#'
#' @param nsims (`integer(1)`) Number of simulations to run.
#' @param scores (`data.frame`) PWHL scores. Defaults to
#'   [HockeyModel::pwhlScores].
#' @param schedule (`data.frame`) PWHL schedule. Defaults to
#'   [HockeyModel::pwhlSchedule].
#' @param params (`list` or `NULL`) PWHL DC parameter list.
#' @param odds_table (`data.frame` or `NULL`) Pre-computed odds table. If
#'   `NULL`, computed via [remainderSeasonDC()].
#'
#' @returns A named list with:
#'   * `summary_results` – per-team season statistics (means, SDs, playoff
#'     odds)
#'   * `raw_results` – one row per team per simulation
#' @export
pwhl_loopless_sim <- function(
  nsims = 1e4,
  scores = HockeyModel::pwhlScores,
  schedule = HockeyModel::pwhlSchedule,
  params = NULL,
  odds_table = NULL
) {
  params <- parse_pwhl_dc_params(params)

  scores_rs <- scores[scores$GameType == "R", ]
  season_start <- pwhl_season_start_date(schedule)

  if (is.null(odds_table)) {
    # PWHL schedule has no postponed games, so regression is not needed here.
    odds_table <- remainderSeasonDC(
      scores = scores_rs,
      schedule = schedule[schedule$GameType == "R", ],
      params = params,
      odds = TRUE,
      regress = FALSE
    )
  }

  season_sofar <- scores_rs[scores_rs$Date >= season_start, ]
  season_sofar <- pwhl_add_result(season_sofar)
  # Drop any rows with missing Result (should not occur in clean data)
  season_sofar <- season_sofar[!is.na(season_sofar$Result), ]

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
    all_season$Result <- NA_real_
  }

  # Compute OT probabilities only for unscheduled games (Result is NA)
  future_mask <- is.na(all_season$Result)
  all_season$HomeOT <- NA_real_
  all_season$HomeSO <- NA_real_
  all_season$AwaySO <- NA_real_
  all_season$AwayOT <- NA_real_

  if (any(future_mask)) {
    ot_probs <- extraTimeSolver(
      all_season$HomeWin[future_mask],
      all_season$AwayWin[future_mask],
      1 - (all_season$HomeWin[future_mask] + all_season$AwayWin[future_mask])
    )
    all_season$HomeOT[future_mask] <- ot_probs[, 2] * 0.6858606
    all_season$HomeSO[future_mask] <- ot_probs[, 2] * 0.3141394
    all_season$AwaySO[future_mask] <- ot_probs[, 3] * 0.3141394
    all_season$AwayOT[future_mask] <- ot_probs[, 3] * 0.6858606
  }

  teamlist <- sort(unique(c(schedule$HomeTeam, schedule$AwayTeam)))

  # Run simulations
  all_results <- purrr::map_dfr(seq_len(nsims), function(sim) {
    sim_data <- all_season
    unknown <- is.na(sim_data$Result)
    if (any(unknown)) {
      sim_data$Result[unknown] <- purrr::pmap_dbl(
        sim_data[
          unknown,
          c("HomeWin", "HomeOT", "HomeSO", "AwaySO", "AwayOT", "AwayWin")
        ],
        function(HomeWin, HomeOT, HomeSO, AwaySO, AwayOT, AwayWin, ...) {
          sampleResult(
            HomeWin,
            HomeOT,
            HomeSO,
            AwaySO,
            AwayOT,
            AwayWin,
            size = 1
          )
        }
      )
    }

    # Tally results for each team
    purrr::map_dfr(teamlist, function(team) {
      home_games <- sim_data[sim_data$HomeTeam == team, ]
      away_games <- sim_data[sim_data$AwayTeam == team, ]

      pts <- sum(
        (home_games$Result == 1) * 2,
        (home_games$Result == 0.75) * 2,
        (home_games$Result == 0.6) * 2,
        (home_games$Result == 0.25),
        (home_games$Result == 0.4),
        (away_games$Result == 0) * 2,
        (away_games$Result == 0.25) * 2,
        (away_games$Result == 0.4) * 2,
        (away_games$Result == 0.75),
        (away_games$Result == 0.6),
        na.rm = TRUE
      )
      wins <- sum(home_games$Result == 1, away_games$Result == 0, na.rm = TRUE)

      data.frame(
        Team = team,
        SimNo = sim,
        Points = pts,
        W = wins,
        stringsAsFactors = FALSE
      )
    })
  })

  # Rank teams within each simulation by points
  all_results <- all_results |>
    dplyr::group_by(.data$SimNo) |>
    dplyr::mutate(
      Rank = rank(-.data$Points, ties.method = "random")
    ) |>
    dplyr::ungroup()

  # PWHL top 4 make playoffs
  all_results$Playoffs <- as.integer(all_results$Rank <= 4)

  summary_results <- all_results |>
    dplyr::group_by(.data$Team) |>
    dplyr::summarise(
      Make_Playoffs = mean(.data$Playoffs),
      meanPoints = mean(.data$Points, na.rm = TRUE),
      maxPoints = max(.data$Points, na.rm = TRUE),
      minPoints = min(.data$Points, na.rm = TRUE),
      meanWins = mean(.data$W, na.rm = TRUE),
      sdPoints = stats::sd(.data$Points, na.rm = TRUE),
      sdWins = stats::sd(.data$W, na.rm = TRUE),
      meanRank = mean(.data$Rank, na.rm = TRUE),
      bestRank = min(.data$Rank, na.rm = TRUE)
    ) |>
    tibble::as_tibble()

  list(summary_results = summary_results, raw_results = all_results)
}


# ── dailyPWHLSummary ──────────────────────────────────────────────────────────

#' Daily PWHL summary — update, predict, and post
#'
#' @description The PWHL equivalent of [dailySummary()]. Fetches the latest
#'   schedule and scores, re-fits the Dixon-Coles model, generates graphics for
#'   today's games and season-wide predictions, and optionally posts them to
#'   social media via [atrrr::post()].
#'
#' Running `dailyPWHLSummary()` with no arguments performs the full daily
#' workflow using the package's stored PWHL datasets.
#'
#' @param graphic_dir (`character(1)`) Directory to save generated PNG files.
#' @param delay (`double(1)`) Seconds to wait between social-media posts.
#'
#' @returns `NULL` (invisibly).
#' @export
dailyPWHLSummary <- function(
  graphic_dir = file.path(
    getOption("HockeyModel.prediction.path", "./prediction_results"),
    "pwhl_graphics"
  ),
  delay = stats::runif(1, min = 2, max = 6) * 60
) {
  model_data <- updatePWHLModel()
  schedule <- model_data$schedule
  scores <- model_data$scores
  params <- parse_pwhl_dc_params(model_data$params)

  if (nrow(schedule) == 0 || Sys.Date() > max(schedule$Date)) {
    cli::cli_alert_info("No PWHL games scheduled; nothing to do.")
    return(invisible(NULL))
  }

  if (!dir.exists(graphic_dir)) {
    dir.create(graphic_dir, recursive = TRUE)
  }

  cli::cli_inform("Creating PWHL graphics...")

  # ── Today's games ───────────────────────────────────────────────────────────
  today_games <- pwhl_games_today(schedule = schedule)
  if (!is.null(today_games) && nrow(today_games) > 0) {
    today_plot <- plot_odds_today(
      params = params,
      schedule = schedule,
      league = "PWHL"
    )
    if (!is.null(today_plot)) {
      grDevices::png(
        filename = file.path(graphic_dir, "pwhl_today_odds.png"),
        width = 11,
        height = 8.5,
        units = "in",
        res = 300
      )
      print(today_plot)
      Sys.sleep(5)
      while (grDevices::dev.cur() != 1) {
        grDevices::dev.off()
      }

      try(
        atrrr::post(
          text = paste0(
            "Predicted odds for today's #PWHL games on ",
            Sys.Date(),
            "."
          ),
          image = file.path(graphic_dir, "pwhl_today_odds.png"),
          image_alt = paste0(
            "Odds graphic for today's PWHL games on ",
            Sys.Date(),
            "."
          )
        )
      )
    }

    today_table <- daily_odds_table(
      params = params,
      schedule = schedule,
      league = "PWHL"
    )
    if (!is.null(today_table)) {
      save_gt_as_png_ragg(
        today_table,
        filename = file.path(graphic_dir, "pwhl_today_odds_table.png")
      )

      Sys.sleep(delay)

      try(
        atrrr::post(
          text = paste0(
            "Predicted odds table for today's #PWHL games on ",
            Sys.Date(),
            "."
          ),
          image = file.path(graphic_dir, "pwhl_today_odds_table.png"),
          image_alt = paste0(
            "Odds table for today's PWHL games on ",
            Sys.Date(),
            "."
          )
        )
      )
    }

    # Team rating plot
    if (!is.null(params$m)) {
      rating_plot <- tryCatch(
        plot_team_rating(m = params$m, league = "PWHL"),
        error = function(e) NULL
      )
      if (!is.null(rating_plot)) {
        grDevices::png(
          filename = file.path(graphic_dir, "pwhl_current_rating.png"),
          width = 11,
          height = 8.5,
          units = "in",
          res = 300
        )
        print(rating_plot)
        Sys.sleep(5)
        while (grDevices::dev.cur() != 1) {
          grDevices::dev.off()
        }

        Sys.sleep(delay)

        try(
          atrrr::post(
            text = paste0(
              "Current #PWHL team ratings (as of ",
              Sys.Date(),
              ")."
            ),
            image = file.path(graphic_dir, "pwhl_current_rating.png"),
            image_alt = paste0(
              "PWHL team offence/defence rating scatter plot as of ",
              Sys.Date(),
              "."
            )
          )
        )
      }
    }
  }

  # ── Playoff series ──────────────────────────────────────────────────────────
  series <- tryCatch(
    getPWHLPlayoffSeries(scores = scores, schedule = schedule),
    error = function(e) NULL
  )
  if (!is.null(series) && nrow(series) > 0) {
    series_plot <- tryCatch(
      plot_playoff_series_odds(
        series = series,
        params = params,
        teamColours = HockeyModel::pwhlTeamColours,
        league = "PWHL"
      ),
      error = function(e) NULL
    )
    if (!is.null(series_plot)) {
      grDevices::png(
        filename = file.path(graphic_dir, "pwhl_series_odds.png"),
        width = 11,
        height = 8.5,
        units = "in",
        res = 300
      )
      print(series_plot)
      Sys.sleep(5)
      while (grDevices::dev.cur() != 1) {
        grDevices::dev.off()
      }

      Sys.sleep(delay)

      try(
        atrrr::post(
          text = paste0(
            "#PWHL playoff series odds as of ",
            Sys.Date(),
            "."
          ),
          image = file.path(graphic_dir, "pwhl_series_odds.png"),
          image_alt = paste0(
            "PWHL playoff series odds as of ",
            Sys.Date(),
            "."
          )
        )
      )
    }

    series_tbl <- tryCatch(
      series_odds_table(series = series, params = params, league = "PWHL"),
      error = function(e) NULL
    )
    if (!is.null(series_tbl)) {
      save_gt_as_png_ragg(
        series_tbl,
        filename = file.path(graphic_dir, "pwhl_series_odds_table.png")
      )

      Sys.sleep(delay)

      try(
        atrrr::post(
          text = paste0(
            "#PWHL playoff series odds table as of ",
            Sys.Date(),
            "."
          ),
          image = file.path(graphic_dir, "pwhl_series_odds_table.png"),
          image_alt = paste0(
            "PWHL playoff series odds table as of ",
            Sys.Date(),
            "."
          )
        )
      )
    }
  }

  # ── Season-wide predictions (regular season only) ───────────────────────────
  if (pwhl_in_season(schedule = schedule)) {
    remaining_rs <- schedule[
      schedule$GameType == "R" & schedule$Date > Sys.Date(),
    ]

    if (nrow(remaining_rs) > 0) {
      cli::cli_inform("Running PWHL season simulations...")
      sim_results <- tryCatch(
        pwhl_loopless_sim(
          nsims = 1e4,
          scores = scores,
          schedule = schedule,
          params = params
        ),
        error = function(e) {
          cli::cli_alert_info("PWHL simulation failed: {conditionMessage(e)}")
          NULL
        }
      )

      if (!is.null(sim_results)) {
        playoff_tbl <- tryCatch(
          format_playoff_odds(
            playoff_odds = sim_results$summary_results,
            caption_text = "PWHL",
            league = "PWHL"
          ),
          error = function(e) NULL
        )
        if (!is.null(playoff_tbl)) {
          save_gt_as_png_ragg(
            playoff_tbl,
            filename = file.path(graphic_dir, "pwhl_playoff_odds.png")
          )

          Sys.sleep(delay)

          try(
            atrrr::post(
              text = paste0(
                "#PWHL playoff qualification odds before games on ",
                Sys.Date(),
                "."
              ),
              image = file.path(graphic_dir, "pwhl_playoff_odds.png"),
              image_alt = paste0(
                "PWHL team playoff odds table as of ",
                Sys.Date(),
                "."
              )
            )
          )
        }
      }
    }
  }

  return(invisible(NULL))
}
