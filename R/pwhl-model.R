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


# ── PWHL-flavoured DC helpers ─────────────────────────────────────────────────

#' PWHL today's odds (internal)
#'
#' @description Like [todayDC()] but uses the PWHL schedule and parameters.
#'
#' @param params (`list` or `NULL`) DC parameter list (PWHL).
#' @param today (`Date`) Date to predict. Defaults to today.
#' @param schedule (`data.frame`) PWHL schedule.
#'
#' @returns Data frame with `HomeTeam`, `AwayTeam`, `HomeWin`, `AwayWin`,
#'   `Draw`, and `GameID`, or `NULL` if no games on `today`.
#' @keywords internal
pwhl_today_dc <- function(
  params = NULL,
  today = Sys.Date(),
  schedule = HockeyModel::pwhlSchedule
) {
  if (!is.Date(today)) {
    cli::cli_abort("{.arg today} must be a Date or date-like value.")
  }
  params <- parse_pwhl_dc_params(params)
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

  for (i in seq_len(nrow(preds))) {
    p <- DCPredict(
      preds$HomeTeam[[i]],
      preds$AwayTeam[[i]],
      params = params,
      draws = TRUE
    )
    preds$HomeWin[[i]] <- p[[1]]
    preds$Draw[[i]] <- p[[2]]
    preds$AwayWin[[i]] <- p[[3]]
  }

  preds
}


#' Get PWHL team colour pair for a matchup
#'
#' @description PWHL-specific wrapper around [getTeamColours()] that uses
#'   [HockeyModel::pwhlTeamColours].
#'
#' @param home (`character(1)`) Home team name.
#' @param away (`character(1)`) Away team name.
#' @param delta (`numeric(1)`) Minimum colour distance; see [colourDelta()].
#'
#' @returns A list with `home` and `away` hex colour strings.
#' @keywords internal
pwhl_get_team_colours <- function(home, away, delta = 0.15) {
  tc <- HockeyModel::pwhlTeamColours
  if (!home %in% tc$Team) {
    cli::cli_abort("{.arg home} ({.val {home}}) is not a recognised PWHL team.")
  }
  if (!away %in% tc$Team) {
    cli::cli_abort("{.arg away} ({.val {away}}) is not a recognised PWHL team.")
  }

  hprimary <- tc[tc$Team == home, "Hex"]
  aprimary <- tc[tc$Team == away, "Hex"]
  halt <- tc[tc$Team == home, "AltHex"]
  aalt <- tc[tc$Team == away, "AltHex"]

  ppdelta <- colourDelta(hprimary, aprimary)
  padelta <- colourDelta(hprimary, aalt)
  apdelta <- colourDelta(halt, aprimary)
  aadelta <- colourDelta(halt, aalt)

  if (ppdelta >= delta) {
    return(list(home = hprimary, away = aprimary))
  }
  if (padelta >= delta) {
    return(list(home = hprimary, away = aalt))
  }
  if (apdelta >= delta) {
    return(list(home = halt, away = aprimary))
  }
  if (aadelta >= delta) {
    return(list(home = halt, away = aalt))
  }

  # All combos too similar; pick the best available
  bestdelta <- max(c(ppdelta, padelta, apdelta, aadelta))
  if (ppdelta == bestdelta) {
    return(list(home = hprimary, away = aprimary))
  } else if (padelta == bestdelta) {
    return(list(home = hprimary, away = aalt))
  } else if (apdelta == bestdelta) {
    return(list(home = halt, away = aprimary))
  }
  list(home = halt, away = aalt)
}


# ── PWHL Graphics ─────────────────────────────────────────────────────────────

#' Plot today's PWHL game odds
#'
#' @description Produces a stacked bar chart of home/away/OT win odds for each
#'   PWHL game scheduled on `today`. Returns `NULL` if there are no games.
#'
#' @param today (`Date`) Date to predict. Defaults to today.
#' @param params (`list` or `NULL`) PWHL DC parameter list.
#' @param schedule (`data.frame`) PWHL schedule.
#'
#' @returns A [ggplot2::ggplot()] object, or `NULL`.
#' @export
pwhl_plot_odds_today <- function(
  today = Sys.Date(),
  params = NULL,
  schedule = HockeyModel::pwhlSchedule
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required. Install it with {.code install.packages('ggplot2')}."
    )
  }
  params <- parse_pwhl_dc_params(params)
  todayodds <- pwhl_today_dc(
    params = params,
    today = today,
    schedule = schedule
  )
  if (is.null(todayodds) || nrow(todayodds) == 0) {
    return(NULL)
  }

  todayodds$HomeWinOT <- todayodds$AwayWinOT <- 0
  for (g in seq_len(nrow(todayodds))) {
    ot <- extraTimeSolver(
      home_win = todayodds$HomeWin[g],
      away_win = todayodds$AwayWin[g],
      draw = todayodds$Draw[g]
    )
    todayodds$HomeWinOT[g] <- ot[2]
    todayodds$AwayWinOT[g] <- ot[3]
  }

  todayodds$GameID <- NULL

  melted <- tidyr::pivot_longer(
    todayodds,
    cols = c("HomeWin", "AwayWin", "HomeWinOT", "AwayWinOT", "Draw"),
    names_to = "variable",
    values_to = "value"
  )
  melted$variable <- factor(
    x = melted$variable,
    levels = c("AwayWin", "AwayWinOT", "Draw", "HomeWinOT", "HomeWin"),
    ordered = TRUE
  )
  melted <- melted[melted$variable != "Draw", ]

  melted$alpha <- ifelse(melted$variable %in% c("HomeWin", "AwayWin"), 1, 0.7)
  melted$colour <- ""

  for (i in seq_len(nrow(melted))) {
    tc <- pwhl_get_team_colours(
      home = melted[i, ]$HomeTeam,
      away = melted[i, ]$AwayTeam
    )
    melted[i, ]$colour <- ifelse(
      melted[i, ]$variable %in% c("HomeWin", "HomeWinOT"),
      tc$home,
      tc$away
    )
  }

  ggplot2::ggplot(
    melted,
    ggplot2::aes(
      y = .data$value,
      x = .data$HomeTeam,
      group = .data$variable
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "fill",
      fill = melted$colour,
      alpha = melted$alpha,
      colour = "white"
    ) +
    ggplot2::labs(
      x = "",
      y = "Result Odds",
      title = "Predictions for Today's PWHL Games",
      subtitle = paste0("Games played on ", today),
      caption = paste0("P. Bulsink (@bot.bulsink.ca) | ", Sys.Date())
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(2, 1, 1, 1), "lines")
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(add = 0.3),
      breaks = c(0, 0.5, 1)
    ) +
    ggplot2::annotate(
      "text",
      x = todayodds$HomeTeam,
      y = -.01,
      hjust = 1,
      label = todayodds$HomeTeam
    ) +
    ggplot2::annotate(
      "text",
      x = todayodds$HomeTeam,
      y = 1.01,
      hjust = 0,
      label = todayodds$AwayTeam
    ) +
    ggplot2::annotate(
      "label",
      x = todayodds$HomeTeam,
      y = 0.01,
      hjust = 0,
      label = format(round(todayodds$HomeWin, 3), nsmall = 3)
    ) +
    ggplot2::annotate(
      "label",
      x = todayodds$HomeTeam,
      y = 0.99,
      hjust = 1,
      label = format(round(todayodds$AwayWin, 3), nsmall = 3)
    ) +
    ggplot2::coord_flip()
}


#' PWHL daily odds table
#'
#' @description Returns a `gt` table of win odds and expected goals for today's
#'   (or a specified date's) PWHL games. Returns `NULL` when there are no games.
#'
#' @param today (`Date`) Date for games. Defaults to today.
#' @param params (`list` or `NULL`) PWHL DC parameter list.
#' @param schedule (`data.frame`) PWHL schedule.
#'
#' @returns A `gt` table object, or `NULL`.
#' @export
pwhl_daily_odds_table <- function(
  today = Sys.Date(),
  params = NULL,
  schedule = HockeyModel::pwhlSchedule
) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg gt} is required. Install it with {.code install.packages('gt')}."
    )
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg scales} is required. Install it with {.code install.packages('scales')}."
    )
  }

  params <- parse_pwhl_dc_params(params)
  todayodds <- pwhl_today_dc(
    params = params,
    today = today,
    schedule = schedule
  )
  if (is.null(todayodds) || nrow(todayodds) == 0) {
    return(NULL)
  }

  todayodds$HomexG <- NA_real_
  todayodds$AwayxG <- NA_real_

  for (g in seq_len(nrow(todayodds))) {
    xg <- dcxG(
      home = todayodds$HomeTeam[g],
      away = todayodds$AwayTeam[g],
      params = params
    )
    todayodds$HomexG[g] <- xg$home
    todayodds$AwayxG[g] <- xg$away
    todayodds[g, c("HomeWin", "AwayWin")] <- normalizeOdds(todayodds[
      g,
      c("HomeWin", "AwayWin")
    ])
  }

  teamColours <- HockeyModel::pwhlTeamColours

  todayodds_gt <- todayodds |>
    dplyr::select(
      .data$HomeTeam,
      .data$HomexG,
      .data$HomeWin,
      .data$AwayWin,
      .data$AwayxG,
      .data$AwayTeam
    ) |>
    tibble::add_column("homeblock" = "  ", .before = 1) |>
    tibble::add_column("awayblock" = "  ") |>
    gt::gt() |>
    gt::tab_header(
      title = "PWHL Game Odds",
      subtitle = paste0("For games ", today, " | P. Bulsink (@bot.bulsink.ca)")
    ) |>
    gt::tab_spanner(
      label = "Home",
      columns = c("HomeTeam", "HomexG", "HomeWin")
    ) |>
    gt::tab_spanner(
      label = "Away",
      columns = c("AwayWin", "AwayxG", "AwayTeam")
    ) |>
    gt::cols_label(
      "homeblock" = " ",
      "awayblock" = " ",
      "HomexG" = "xG",
      "HomeWin" = "Win",
      "HomeTeam" = "Team",
      "AwayxG" = "xG",
      "AwayWin" = "Win",
      "AwayTeam" = "Team"
    ) |>
    gt::data_color(
      columns = c(4, 5),
      fn = scales::col_numeric(
        palette = c("#cc3c3c", "#ffffff", "#3c3ccc"),
        domain = c(0, 1)
      )
    ) |>
    gt::fmt_percent(columns = 4:5, decimals = 1) |>
    gt::fmt_number(
      columns = c(3, 6),
      drop_trailing_zeros = FALSE,
      decimals = 2
    ) |>
    gt::tab_options(
      heading.align = "left",
      table.border.bottom.color = "white",
      table.border.top.color = "white"
    )

  for (i in seq_len(nrow(todayodds))) {
    todayodds_gt <- todayodds_gt |>
      gt::tab_style(
        style = gt::cell_fill(
          color = teamColours[teamColours$Team == todayodds$HomeTeam[i], "Hex"]
        ),
        locations = gt::cells_body(columns = "homeblock", rows = i)
      ) |>
      gt::tab_style(
        style = gt::cell_fill(
          color = teamColours[teamColours$Team == todayodds$AwayTeam[i], "Hex"]
        ),
        locations = gt::cells_body(columns = "awayblock", rows = i)
      )
  }

  todayodds_gt
}


#' PWHL team ratings plot
#'
#' @description Produces an offence-vs-defence scatter plot for PWHL teams
#'   using the fitted PWHL model `m`.
#'
#' @param m (`glm`) PWHL DC model. Defaults to [HockeyModel::pwhl_m].
#' @param teamlist (`character`) Subset of teams. `NULL` uses all teams in `m`.
#'
#' @returns A [ggplot2::ggplot()] object.
#' @export
pwhl_plot_team_rating <- function(m = HockeyModel::pwhl_m, teamlist = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ggplot2} is required. Install it with {.code install.packages('ggplot2')}."
    )
  }
  if (is.null(m)) {
    cli::cli_abort(
      "PWHL model is not fitted yet. Run {.fn updatePWHLDC} first."
    )
  }
  if (is.null(teamlist)) {
    teamlist <- as.character(unique(m$data$Team))
  }

  team_params <- data.frame(
    Attack = as.numeric(m$coefficients[seq_along(teamlist)]),
    Defence = c(
      0,
      -m$coefficients[(length(teamlist) + 1):(length(teamlist) * 2 - 1)]
    ),
    Team = sort(teamlist)
  )
  team_params$Attack <- (team_params$Attack - mean(team_params$Attack)) /
    stats::sd(team_params$Attack)
  team_params$Defence <- (team_params$Defence - mean(team_params$Defence)) /
    stats::sd(team_params$Defence)

  tc <- HockeyModel::pwhlTeamColours
  teamColoursList <- as.vector(tc$Hex)
  names(teamColoursList) <- tc$Team
  teamColoursList <- teamColoursList[names(teamColoursList) %in% teamlist]

  p <- ggplot2::ggplot(
    team_params,
    ggplot2::aes(
      x = .data$Attack,
      y = .data$Defence,
      color = .data$Team,
      label = .data$Team
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey", linewidth = 1) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey", linewidth = 1) +
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = teamColoursList) +
    ggplot2::labs(
      x = "Offence",
      y = "Defence",
      title = "Current PWHL Team Offence & Defence Ratings",
      subtitle = paste0("As of ", Sys.Date()),
      caption = paste0("P. Bulsink (@bot.bulsink.ca) | ", Sys.Date())
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_cartesian(
      xlim = c(
        -max(abs(team_params$Attack)) + 0.1,
        max(abs(team_params$Attack)) + 0.1
      ),
      ylim = c(
        -max(abs(team_params$Defence)) + 0.1,
        max(abs(team_params$Defence)) + 0.1
      )
    ) +
    ggplot2::annotate(
      "label",
      x = -max(abs(team_params$Attack)),
      y = -max(abs(team_params$Defence)),
      hjust = 0,
      vjust = 0,
      label = "Bad"
    ) +
    ggplot2::annotate(
      "label",
      x = max(abs(team_params$Attack)),
      y = max(abs(team_params$Defence)),
      hjust = 1,
      vjust = 1,
      label = "Good"
    ) +
    ggplot2::annotate(
      "label",
      x = -max(abs(team_params$Attack)),
      y = max(abs(team_params$Defence)),
      hjust = 0,
      vjust = 1,
      label = "Calm"
    ) +
    ggplot2::annotate(
      "label",
      x = max(abs(team_params$Attack)),
      y = -max(abs(team_params$Defence)),
      hjust = 1,
      vjust = 0,
      label = "Frantic"
    ) +
    ggplot2::theme(legend.position = "none")

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p + ggrepel::geom_text_repel(force = 2, max.iter = 5000)
  }

  p
}


# ── PWHL Season Simulation ────────────────────────────────────────────────────

#' Simulate the remainder of the PWHL regular season
#'
#' @description Produces a data frame of win odds for all remaining PWHL
#'   regular-season games, used as input for [pwhl_loopless_sim()].
#'
#' @param scores (`data.frame`) PWHL scores with a `Result` column.
#' @param schedule (`data.frame`) PWHL full-season schedule.
#' @param params (`list` or `NULL`) PWHL DC parameter list.
#'
#' @returns A data frame of `HomeTeam`, `AwayTeam`, `HomeWin`, `AwayWin`,
#'   `Draw`, `GameID`, and `Date` for un-played games.
#' @keywords internal
pwhl_remainder_season_dc <- function(
  scores,
  schedule,
  params = NULL
) {
  params <- parse_pwhl_dc_params(params)

  last_game_date <- as.Date(max(scores$Date))
  remaining <- schedule[
    schedule$Date > last_game_date & schedule$GameType == "R",
  ]
  remaining <- remaining[order(remaining$Date, remaining$GameID), ]

  if (nrow(remaining) == 0) {
    return(data.frame(
      HomeTeam = character(),
      AwayTeam = character(),
      HomeWin = numeric(),
      AwayWin = numeric(),
      Draw = numeric(),
      GameID = integer(),
      Date = as.Date(character()),
      stringsAsFactors = FALSE
    ))
  }

  odds_table <- data.frame(
    HomeTeam = remaining$HomeTeam,
    AwayTeam = remaining$AwayTeam,
    HomeWin = 0,
    AwayWin = 0,
    Draw = 0,
    GameID = remaining$GameID,
    Date = remaining$Date,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(odds_table))) {
    p <- DCPredict(
      odds_table$HomeTeam[[i]],
      odds_table$AwayTeam[[i]],
      params = params,
      draws = TRUE
    )
    odds_table$HomeWin[[i]] <- p[[1]]
    odds_table$Draw[[i]] <- p[[2]]
    odds_table$AwayWin[[i]] <- p[[3]]
  }

  odds_table
}


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
#'   `NULL`, computed via [pwhl_remainder_season_dc()].
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
    odds_table <- pwhl_remainder_season_dc(
      scores = scores_rs,
      schedule = schedule,
      params = params
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


#' Format PWHL playoff odds table
#'
#' @description Produces a `gt` table of PWHL team playoff qualification odds,
#'   suitable for saving as an image.
#'
#' @param playoff_odds (`data.frame`) Per-team odds with at least `Team` and
#'   `Make_Playoffs` columns, as returned by [pwhl_loopless_sim()].
#' @param caption_text (`character(1)`) Extra caption text.
#'
#' @returns A `gt` table.
#' @export
pwhl_format_playoff_odds <- function(playoff_odds, caption_text = "PWHL") {
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg gt} is required. Install it with {.code install.packages('gt')}."
    )
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg scales} is required. Install it with {.code install.packages('scales')}."
    )
  }

  tc <- HockeyModel::pwhlTeamColours
  playoff_odds <- playoff_odds |>
    dplyr::arrange(dplyr::desc(.data$Make_Playoffs), .data$Team)

  playoff_odds_gt <- playoff_odds |>
    tibble::add_column("block" = "  ", .before = 1) |>
    gt::gt() |>
    gt::tab_header(
      title = paste(caption_text, "Playoff Odds"),
      subtitle = paste0(
        "Generated ",
        Sys.Date(),
        " | P. Bulsink (@bot.bulsink.ca)"
      )
    ) |>
    gt::cols_label(
      "block" = " ",
      "Make_Playoffs" = "Make Playoffs",
      "meanPoints" = "Mean Points",
      "meanRank" = "Mean Rank"
    ) |>
    gt::data_color(
      columns = "Make_Playoffs",
      fn = scales::col_numeric(c("#fefffe", "#3ccc3c"), domain = c(0, 1))
    ) |>
    gt::fmt_percent(columns = "Make_Playoffs", drop_trailing_zeros = FALSE) |>
    gt::fmt_number(
      columns = c("meanPoints", "meanRank"),
      decimals = 1
    ) |>
    gt::tab_options(heading.align = "left")

  for (i in seq_len(nrow(playoff_odds))) {
    playoff_odds_gt <- playoff_odds_gt |>
      gt::tab_style(
        style = gt::cell_fill(
          color = tc[tc$Team == playoff_odds$Team[i], "Hex"]
        ),
        locations = gt::cells_body(columns = "block", rows = i)
      )
  }

  playoff_odds_gt
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
    today_plot <- pwhl_plot_odds_today(
      params = params,
      schedule = schedule
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

    today_table <- pwhl_daily_odds_table(
      params = params,
      schedule = schedule
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
        pwhl_plot_team_rating(m = params$m),
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
          pwhl_format_playoff_odds(
            playoff_odds = sim_results$summary_results,
            caption_text = "PWHL"
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
