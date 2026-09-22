# PWHL daily summary: fetch/update model, generate graphics, and post daily social media summary

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
      save_gt_as_png(
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
    getPWHLPlayoffSeries(),
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
      save_gt_as_png(
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
          save_gt_as_png(
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
