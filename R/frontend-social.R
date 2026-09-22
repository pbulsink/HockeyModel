# Front-end social: social media posting helpers

#' Post daily model graphics to social media
#'
#' @param games (`data.frame`) Games to reference for game-specific posts.
#' @param graphic_dir (`character(1)`) Directory containing generated images.
#' @param delay (`double(1)`) Delay in seconds between posts.
#' @param schedule (`data.frame`) Schedule used to determine daily context.
#' @returns `NULL` (invisibly).
#' @keywords internal
tweet <- function(
  games,
  graphic_dir = .default_nhl_graphics_dir(),
  delay = stats::runif(1, min = 2, max = 6) * 60,
  schedule = HockeyModel::schedule
) {
  if (inRegularSeason()) {
    # Only runs if schedule has regular season games remaining
    try(
      atrrr::post(
        text = paste0(
          "Predicted points for #NHL teams (before games on ",
          Sys.Date(),
          ")."
        ),
        image = file.path(graphic_dir, "point_predict.png"), # token = token,
        image_alt = paste0(
          "Points predicted history for the last 14 days, as of ",
          Sys.Date(),
          ""
        )
      )
    )

    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)

    try(
      atrrr::post(
        text = paste0(
          "Playoff odds for #NHL teams (before games on ",
          Sys.Date(),
          "). #HockeyTwitter"
        ),
        image = file.path(graphic_dir, "playoff_odds.png"),
        image_alt = paste0(
          "Playoff Odds for each NHL team history and today's value as of ",
          Sys.Date(),
          ""
        )
      )
    )

    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)

    try(
      atrrr::post(
        text = paste0(
          "President's trophy odds for #NHL teams (before games on ",
          Sys.Date(),
          "). #HockeyTwitter"
        ),
        image = file.path(graphic_dir, "president_odds.png"),
        image_alt = paste0(
          "President's Trophy Odds for each NHL team history and today's value as of ",
          Sys.Date(),
          ""
        )
      )
    )
  }

  return(invisible(NULL))
}
#' Tweet Pace Plots
#'
#' @param delay Delay between posted tweets
#' @param graphic_dir The graphics directory
#' @param subdir The pace subdirectory in graphics
#' @param prediction_dir The predictions directory
#' @param scores HockeyModel::scores or a custom value
#'
#' @export
tweetPace <- function(
  delay = stats::runif(1, min = 1, max = 3) * 60,
  graphic_dir = getOption("HockeyModel.graphics.path"),
  subdir = "pace",
  prediction_dir = getOption("HockeyModel.prediction.path"),
  scores = HockeyModel::scores
) {
  # make sure we're working with the most up-to-date info.
  scores <- updateScoresAPI(save_data = TRUE)

  # Make Pace Plots
  plot_pace_by_team(
    graphic_dir = graphic_dir,
    subdir = subdir,
    prediction_dir = prediction_dir,
    scores = scores
  )

  pdates <- get_prediction_dates(prediction_dir)
  if (length(pdates) == 0L) {
    cli::cli_abort("No prediction files found in {.path {prediction_dir}}.")
  }
  lastp <- max(pdates)
  current_preds <- readRDS(file.path(
    prediction_dir,
    paste0(lastp, "-predictions.RDS")
  ))
  preds <- readRDS(file.path(
    prediction_dir,
    paste0(getSeasonStartDate(), "-predictions.RDS")
  ))
  scores <- scores[scores$Date > as.Date(getSeasonStartDate()), ]

  teamlist <- unique(preds$Team)

  teamColours <- HockeyModel::teamColours

  reply_id <- NULL
  for (team in teamlist) {
    ngames <- sum(sum(scores$HomeTeam == team), sum(scores$AwayTeam == team))
    status <- paste0(
      team,
      " pace after ",
      ngames,
      " games. The model initially predicted ",
      format(
        round(as.numeric(preds[preds$Team == team, "meanPoints"]), digits = 1),
        nsmall = 1
      ),
      " points, now expecting ",
      format(
        round(
          as.numeric(current_preds[current_preds$Team == team, "meanPoints"]),
          digits = 1
        ),
        nsmall = 1
      ),
      ". #HockeyTwitter ",
      teamColours[teamColours$Team == team, "Hashtag"]
    )

    try(
      atrrr::post(
        text = status,
        image = file.path(
          graphic_dir,
          subdir,
          paste0(tolower(gsub(" ", "_", team)), ".png")
        ),
        image_alt = paste0(
          team,
          "'s Performance against predicted pace as of ",
          Sys.Date(),
          ""
        )
      )
    )

    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(stats::runif(1, min = 1, max = 3) * 60)
  }
  pacediff <- data.frame(
    "Team" = current_preds$Team,
    "Initial" = preds$meanPoints,
    "Current" = current_preds$meanPoints,
    stringsAsFactors = FALSE
  )
  pacediff$Diff <- pacediff$Current - pacediff$Initial

  maxteam <- pacediff[which.max(pacediff$Diff), "Team"]
  minteam <- pacediff[which.min(pacediff$Diff), "Team"]

  recapstatus <- paste0(
    "To recap - ",
    "\nFurthest above expectation: ",
    maxteam,
    " ",
    teamColours[teamColours$Team == maxteam, "Hashtag"],
    "\nFurthest below expectation: ",
    minteam,
    " ",
    teamColours[teamColours$Team == minteam, "Hashtag"]
  )
  atrrr::post(text = recapstatus)

  Sys.sleep(stats::runif(1, min = 2, max = 6) * 60)

  # Make Division Plots
  plot_pace_by_division(
    graphic_dir = graphic_dir,
    subdir = subdir,
    prediction_dir = prediction_dir,
    scores = scores
  )

  for (division in getDivisions()) {
    status <- paste(
      "Current Points compared to predicted (at season start) for #NHL teams in the",
      division,
      "division.\nPositive values are exceeding expectation, negative are performing below predicted."
    )

    try(
      atrrr::post(
        text = status,
        image = file.path(graphic_dir, subdir, paste0(division, "_pace.png")),
        image_alt = paste0(
          division,
          " teams pace above/below expected as of ",
          Sys.Date(),
          "."
        )
      )
    )

    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)
  }
}

#' Tweet Likelihood plots (ggridges)
#'
#' @param delay time to delay. Default 5 min
#' @param graphic_dir graphics directory
#' @param subdir subdirectory - usually 'preds'
#' @param scores updated scores
#'
#' @returns `NULL` (invisibly).
#' @export
tweetLikelihoods <- function(
  delay = stats::runif(1, min = 3, max = 6) * 60,
  graphic_dir = getOption("HockeyModel.graphics.path"),
  subdir = "pace",
  scores = HockeyModel::scores
) {
  # make likelihood plots
  plot_point_likelihood(graphic_dir = graphic_dir, subdir = subdir)

  for (conf in getConferences()) {
    if (
      file.exists(file.path(
        graphic_dir,
        subdir,
        paste0(tolower(conf), "likelihood.png")
      )) &&
        as.Date(file.mtime(file.path(
          graphic_dir,
          subdir,
          paste0(tolower(conf), "likelihood.png")
        ))) ==
          Sys.Date()
    ) {
      # Tweet them out

      try(
        atrrr::post(
          text = paste0(
            "#NHL ",
            conf,
            " Conference Team final point likelihoods:"
          ),
          image = file.path(
            graphic_dir,
            subdir,
            paste0(tolower(conf), "likelihood.png")
          ),
          image_alt = paste0(
            "Point likelihoods for teams in the ",
            conf,
            " conference."
          )
        )
      )

      # delay
      message("Delaying ", delay / 2, " seconds to space tweets...")
      Sys.sleep(delay / 2)
    }
  }

  return(invisible(NULL))
}

#' Tweet Game Plots
#'
#' @param games Games to tweet graphics from
#' @param delay Delay between tweets
#' @param graphic_dir the graphics directory
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @export
tweetGames <- function(
  games = games_today(),
  delay = stats::runif(1, min = 4, max = 8) * 60,
  graphic_dir = getOption("HockeyModel.graphics.path"),
  params = NULL
) {
  params <- parse_dc_params(params)
  # Tweet each game
  if (is.null(games)) {
    message("No games to tweet")
    return()
  }

  if (nrow(games) == 0) {
    message("No games to tweet")
    return()
  }

  if (!dir.exists(graphic_dir)) {
    dir.create(graphic_dir, recursive = TRUE)
  }

  teamColours <- HockeyModel::teamColours

  for (g in seq_len(nrow(games))) {
    home <- as.character(games[g, "HomeTeam"])
    away <- as.character(games[g, "AwayTeam"])
    plt <- plot_game(home = home, away = away, params = params)
    grDevices::png(
      filename = file.path(graphic_dir, "predicted_goals.png"),
      width = 11,
      height = 8.5,
      units = "in",
      res = 300
    )
    print(plt)
    while (grDevices::dev.cur() != 1) {
      grDevices::dev.off()
    }
    status <- paste0(
      teamColours[teamColours$Team == away, "Hashtag"],
      " at ",
      teamColours[teamColours$Team == home, "Hashtag"],
      " predicted goals. #",
      getShortTeam(away),
      "vs",
      getShortTeam(home),
      " #HockeyTwitter"
    )

    try(
      atrrr::post(
        text = status,
        image = file.path(graphic_dir, "predicted_goals.png"),
        image_alt = paste0(
          "Odds of each goal for both ",
          away,
          " and ",
          home,
          " in their game."
        )
      )
    )

    file.remove(file.path(graphic_dir, "predicted_goals.png"))

    message("Delaying ", delay, " seconds to space tweets...")
    Sys.sleep(delay)
  }
}

#' Tweet Metrics
#' @description Tweet the metrics (Log Loss and Accuracy)
#'
#' @returns `NULL` (invisibly).
#' @export
tweetMetrics <- function() {
  metrics <- getSeasonMetricsDC()

  status <- paste0(
    "Metrics as of ",
    Sys.Date(),
    "\nLog Loss: ",
    round(metrics$LogLoss, 4),
    "\nAccuracy: ",
    round(metrics$Accuracy * 100, 2),
    " %"
  )
  message(status)

  try(atrrr::post(text = status))
}

#' Tweet Series
#' @description Tweet the series odds graphics
#'
#' @param graphic_dir directory to save the image
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#' @param delay Delay in seconds between posts. Default is a random value between 1 and 3 minutes.
#'
#' @return NULL
#' @export
tweetSeries <- function(
  params = NULL,
  graphic_dir = getOption("HockeyModel.graphics.path"),
  delay = stats::runif(1, min = 1, max = 3) * 60
) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg gt} is required to save the series odds table image.",
        "i" = "Install it with {.code install.packages('gt')}."
      )
    )
  }

  params <- parse_dc_params(params)
  while (grDevices::dev.cur() != 1) {
    grDevices::dev.off()
  }
  series <- getAPISeries()
  series <- series[
    series$Status == "Ongoing",
    c("HomeTeam", "AwayTeam", "HomeWins", "AwayWins")
  ]
  if (nrow(series) == 0) {
    message("No Series to Tweet")
    return()
  }
  plt <- plot_playoff_series_odds(series = series, params = params)
  grDevices::png(
    filename = file.path(graphic_dir, "series_odds.png"),
    width = 11,
    height = 8.5,
    units = "in",
    res = 300
  )
  print(plt)
  while (grDevices::dev.cur() != 1) {
    grDevices::dev.off()
  }

  status <- paste0(
    "#NHL #StanleyCup Playoff Series Odds before games on ",
    Sys.Date()
  )
  try(
    atrrr::post(
      text = status,
      image = file.path(graphic_dir, "series_odds.png"),
      image_alt = "A graphic showing odds for each series' winner"
    )
  )

  message("Delaying ", delay, " seconds to space tweets...")
  Sys.sleep(delay)

  tbl <- series_odds_table(series = series, params = params)
  save_gt_as_png(
    tbl,
    filename = file.path(graphic_dir, "series_odds_table.png")
  )

  try(
    atrrr::post(
      text = paste0(
        "#NHL #StanleyCup Playoff Series Odds table before games on ",
        Sys.Date()
      ),
      image = file.path(graphic_dir, "series_odds_table.png"),
      image_alt = "A table showing odds for each series' winner"
    )
  )
}


#' Tweet Playoff Odds
#'
#' @description Tweet a graphic of the playoff odds
#'
#' @param summary_results the summary results file, otherwise the most recent will be loaded
#' @param graphic_dir graphic dir
#' @param trimcup trim to just cup winners
#' @param params The named list containing m, rho, beta, eta, and k. See [updateDC] for information on the params list
#'
#' @return NULL
#' @export
tweetPlayoffOdds <- function(
  summary_results = NULL,
  params = NULL,
  graphic_dir = getOption("HockeyModel.graphics.path"),
  trimcup = FALSE
) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg gt} is required. Install it with {.code install.packages('gt')}."
    )
  }

  params <- parse_dc_params(params)
  playoffodds <- simulatePlayoffs(
    summary_results = summary_results,
    params = params
  )

  if (is.null(playoffodds)) {
    return(invisible(NULL))
  }

  playoffodds$Conference <- getTeamConferences(playoffodds$Team)
  if (trimcup) {
    plt <- format_playoff_odds(
      playoff_odds = playoffodds,
      caption_text = "NHL Playoffs",
      trim = FALSE,
      trimcup = trimcup
    )
    save_gt_as_png(
      plt,
      filename = file.path(graphic_dir, "playoff_odds.png")
    )

    status <- paste0(
      "#NHL Eastern and Western Conference Playoff and #StanleyCup Odds before games on ",
      Sys.Date(),
      ". #HockeyTwitter"
    )

    try(
      atrrr::post(
        text = paste0(
          "#NHL Playoff and #StanleyCup Odds before games on ",
          Sys.Date(),
          "."
        ),
        image = file.path(graphic_dir, "playoff_odds.png"),
        image_alt = "Playoff Odds"
      )
    )
  } else {
    for (conf in unique(playoffodds$Conference)) {
      plt <- format_playoff_odds(
        playoff_odds = playoffodds[
          playoffodds$Conference == conf,
          which(names(playoffodds) != "Conference")
        ],
        caption_text = paste(conf, "Conference"),
        trim = FALSE,
        trimcup = trimcup
      )
      save_gt_as_png(
        plt,
        filename = file.path(
          graphic_dir,
          paste0(tolower(conf), "_playoff_odds.png")
        )
      )
    }
    status <- paste0(
      "#NHL Eastern and Western Conference Playoff and #StanleyCup Odds before games on ",
      Sys.Date(),
      ". #HockeyTwitter"
    )

    try(
      atrrr::post(
        text = paste0(
          "#NHL Eastern Conference Playoff and #StanleyCup Odds before games on ",
          Sys.Date(),
          "."
        ),
        image = file.path(graphic_dir, "eastern_playoff_odds.png"),
        image_alt = "Eastern Playoff Odds"
      )
    )

    try(
      atrrr::post(
        text = paste0(
          "#NHL Western Conference Playoff and #StanleyCup Odds before games on ",
          Sys.Date(),
          "."
        ),
        image = file.path(graphic_dir, "western_playoff_odds.png"),
        image_alt = "Western Playoff Odds"
      )
    )
  }
}
