# Front-end daily summary: combined daily update/graphics/social workflow

#' Daily functions, rolled into one call
#'
#' @param graphic_dir Directory for graphic files
#' @param subdir subdirectory to `graphic_dir` for pace plots
#' @param delay delay between tweet posts
#' @returns `NULL` (invisibly).
#' @keywords internal
.daily_summary_nhl <- function(
  graphic_dir = .default_nhl_graphics_dir(),
  subdir = "pace",
  delay = stats::runif(1, min = 2, max = 6) * 60
) {
  if (inOffSeason()) {
    if (
      getSeasonStartDate() - Sys.Date() > 7 ||
        getSeasonStartDate() - Sys.Date() < 0
    ) {
      stop("Offseason")
    }
  }
  modelparams <- updateModel()
  sc <- modelparams$schedule
  params <- parse_dc_params(params = modelparams)

  if (Sys.Date() > max(sc$Date)) {
    stop("No future games planned")
  }

  if (!dir.exists(graphic_dir)) {
    dir.create(graphic_dir, recursive = TRUE)
  }

  message("Creating graphics...")

  # generate plots
  if (!is.null(games_today())) {
    today <- todayOddsPlot(
      params = params,
      schedule = modelparams$schedule,
      scores = modelparams$scores
    )
    # save to files.
    grDevices::png(
      filename = file.path(graphic_dir, "today_odds.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(today)
    Sys.sleep(5)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    today_table <- daily_odds_table(
      params = params,
      schedule = modelparams$schedule
    )
    save_gt_as_png(
      today_table,
      filename = file.path(graphic_dir, "today_odds_table.png")
    )

    try(
      atrrr::post(
        text = "Predicted odds table for today's #NHL games.",
        image = file.path(graphic_dir, "today_odds_table.png"),
        image_alt = paste0(
          "Odds table for Today's NHL games, for date ",
          Sys.Date(),
          "."
        )
      )
    )

    try(
      atrrr::post(
        text = "Predicted odds for today's #NHL games.",
        image = file.path(graphic_dir, "today_odds.png"),
        image_alt = paste0(
          "Odds graphic for Today's NHL games, for date ",
          Sys.Date(),
          "."
        )
      )
    )

    rating <- ratings(params$m)
    # save to files.
    grDevices::png(
      filename = file.path(graphic_dir, "current_rating.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(rating)
    Sys.sleep(5)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    try(
      atrrr::post(
        text = paste0("Current team ratings (as of ", Sys.Date(), ")."),
        image = file.path(graphic_dir, "current_rating.png"),
        image_alt = paste0("Current team rating graphic for ", Sys.Date(), ".")
      )
    )
  }

  if (inRegularSeason()) {
    updatePredictions(
      scores = modelparams$scores,
      schedule = modelparams$schedule,
      params = params
    )
    playoff <- playoffOdds()
    president <- presidentOdds()
    point <- pointPredict()
    rating <- ratings(m = params$m)

    Sys.sleep(15)

    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    grDevices::png(
      filename = file.path(graphic_dir, "playoff_odds.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(playoff)
    Sys.sleep(5)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    grDevices::png(
      filename = file.path(graphic_dir, "president_odds.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(president)
    Sys.sleep(5)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    grDevices::png(
      filename = file.path(graphic_dir, "point_predict.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(point)
    Sys.sleep(5)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    grDevices::png(
      filename = file.path(graphic_dir, "current_rating.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(rating)
    Sys.sleep(5)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }

    # Make Pace Plots
    plot_pace_by_team(
      graphic_dir = graphic_dir,
      subdir = subdir,
      scores = modelparams$scores
    )
    plot_pace_by_division(
      graphic_dir = graphic_dir,
      subdir = subdir,
      scores = modelparams$scores
    )
    plot_point_likelihood(graphic_dir = graphic_dir, subdir = subdir)
  }

  message("Posting Tweets...")
  tweet(graphic_dir, delay = delay, graphic_dir = graphic_dir) # , games_today = Sys.Date() %in% sc[sc$GameStatus != "Postponed", ]$Date)

  message("Delaying ", delay, " seconds to space tweets...")
  Sys.sleep(delay)

  if (inRegularSeason()) {
    tweetPlayoffOdds(graphic_dir = graphic_dir, params = params)

    message("Delaying ", delay / 2, " seconds to space tweets...")
    Sys.sleep(delay / 2)
  } else if (inPlayoffs()) {
    message("Calculating Playoff Odds")
    tweetPlayoffOdds(graphic_dir = graphic_dir, trimcup = TRUE)
  }

  if (as.numeric(format(Sys.Date(), "%w")) == 1 && inRegularSeason()) {
    # On monday post pace plots
    tweetPace(delay = delay, graphic_dir = graphic_dir)
  }

  if (as.numeric(format(Sys.Date(), "%w")) == 0 && inRegularSeason()) {
    message("Tweeting Metrics")
    # On Sunday post metrics
    tweetMetrics()
  }

  if (as.numeric(format(Sys.Date(), "%w")) == 2 && inRegularSeason()) {
    message("Tweeting Likelihoods")
    # On Tuesday post expected points (likelihood)
    tweetLikelihoods(delay = delay, graphic_dir = graphic_dir)
  }

  series <- getAPISeries()
  if (
    !is.na(series) &&
      length(series) > 1 &&
      nrow(series[series$Status == "Ongoing", ]) > 0
  ) {
    # TODO: Watch next spring to see if this goes ok
    message("Tweeting Series")
    tweetSeries(graphic_dir = graphic_dir, params = params)
    Sys.sleep(delay)
  }
}


#' Daily front-end summary
#'
#' Runs the daily front-end workflow for NHL, PWHL, or both leagues.
#'
#' @param graphic_dir (`character(1)`) Directory for graphic files. When
#'   omitted, NHL uses [getOption()] `HockeyModel.graphics.path` and PWHL uses
#'   `file.path(getOption("HockeyModel.prediction.path"), "pwhl_graphics")`.
#' @param subdir (`character(1)`) Subdirectory to `graphic_dir` for pace plots.
#' @param delay (`double(1)`) Delay between social-media posts in seconds.
#' @param league (`character(1)` or `NULL`) Which league front-end to run. Can
#'   be one of:
#'   * `NULL`, `NA`, or `"both"`: Run both leagues.
#'   * `"nhl"`: Run the NHL workflow only.
#'   * `"pwhl"`: Run the PWHL workflow only.
#'
#' @returns For a single league, `NULL` (invisibly). For both leagues, a named
#'   list with `nhl` and `pwhl` entries.
#' @export
dailySummary <- function(
  graphic_dir = .default_nhl_graphics_dir(),
  subdir = "pace",
  delay = stats::runif(1, min = 2, max = 6) * 60,
  league = NULL
) {
  graphic_dir_missing <- missing(graphic_dir)
  leagues <- .resolve_frontend_leagues(league)
  result <- list()

  if ("NHL" %in% leagues) {
    result$nhl <- .daily_summary_nhl(
      graphic_dir = .frontend_value_for_league(
        graphic_dir,
        "NHL",
        .default_nhl_graphics_dir()
      ),
      subdir = subdir,
      delay = delay
    )
  }
  if ("PWHL" %in% leagues) {
    pwhl_graphic_dir <- if (graphic_dir_missing) {
      .default_pwhl_graphics_dir()
    } else {
      .frontend_value_for_league(
        graphic_dir,
        "PWHL",
        .default_pwhl_graphics_dir()
      )
    }
    result$pwhl <- dailyPWHLSummary(
      graphic_dir = pwhl_graphic_dir,
      delay = delay
    )
  }

  .simplify_frontend_result(result, leagues)
}
