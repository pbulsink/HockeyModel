test_that(".resolve_frontend_leagues normalizes supported inputs (#39)", {
  expect_equal(.resolve_frontend_leagues(NULL), c("NHL", "PWHL"))
  expect_equal(.resolve_frontend_leagues(NA_character_), c("NHL", "PWHL"))
  expect_equal(.resolve_frontend_leagues("both"), c("NHL", "PWHL"))
  expect_equal(.resolve_frontend_leagues("nhl"), "NHL")
  expect_equal(.resolve_frontend_leagues("PWHL"), "PWHL")
  expect_error(.resolve_frontend_leagues("ahl"), "league")
})

test_that("updateModel dispatches both leagues by default (#39)", {
  local_mocked_bindings(
    .update_model_nhl = function(save_data = TRUE) {
      list(source = "nhl", save_data = save_data)
    },
    updatePWHLModel = function(save_data = TRUE) {
      list(source = "pwhl", save_data = save_data)
    },
    .package = "HockeyModel"
  )

  result <- updateModel(save_data = FALSE)

  expect_named(result, c("nhl", "pwhl"))
  expect_identical(result$nhl$source, "nhl")
  expect_identical(result$pwhl$source, "pwhl")
  expect_false(result$nhl$save_data)
  expect_false(result$pwhl$save_data)
})

test_that("updateModel returns a single-league payload when requested (#39)", {
  local_mocked_bindings(
    .update_model_nhl = function(save_data = TRUE) {
      list(source = "nhl", save_data = save_data)
    },
    updatePWHLModel = function(save_data = TRUE) {
      list(source = "pwhl", save_data = save_data)
    },
    .package = "HockeyModel"
  )

  expect_identical(
    updateModel(save_data = FALSE, league = "nhl"),
    list(source = "nhl", save_data = FALSE)
  )
  expect_identical(
    updateModel(save_data = FALSE, league = "pwhl"),
    list(source = "pwhl", save_data = FALSE)
  )
})

test_that("updatePredictions writes PWHL prediction snapshots (#39)", {
  prediction_dir <- withr::local_tempdir()
  schedule <- data.frame(
    Date = as.Date("2025-01-15"),
    HomeTeam = "Boston Fleet",
    AwayTeam = "Minnesota Frost",
    GameID = 1L,
    GameType = "R",
    GameStatus = "Scheduled",
    stringsAsFactors = FALSE
  )
  scores <- data.frame(
    Date = as.Date(character()),
    HomeTeam = character(),
    AwayTeam = character(),
    GameID = integer(),
    GameType = character(),
    GameStatus = character(),
    stringsAsFactors = FALSE
  )
  summary_results <- tibble::tibble(
    Team = c("Boston Fleet", "Minnesota Frost"),
    Make_Playoffs = c(0.75, 0.60),
    meanPoints = c(72, 69)
  )
  raw_results <- tibble::tibble(
    Team = c(
      "Boston Fleet",
      "Boston Fleet",
      "Minnesota Frost",
      "Minnesota Frost"
    ),
    Rank = c(1, 2, 2, 1)
  )

  local_mocked_bindings(
    pwhl_loopless_sim = function(...) {
      list(summary_results = summary_results, raw_results = raw_results)
    },
    .package = "HockeyModel"
  )

  updatePredictions(
    data_dir = prediction_dir,
    scores = scores,
    schedule = schedule,
    params = list(),
    league = "pwhl"
  )

  result <- readRDS(file.path(
    prediction_dir,
    "pwhl",
    paste0(Sys.Date(), "-predictions.RDS")
  ))

  expect_named(result, c("Team", "Playoffs", "meanPoints", "Presidents"))
  expect_equal(result$Playoffs, c(0.75, 0.60))
  expect_equal(result$Presidents, c(0.5, 0.5))
})

test_that("todayOddsPlot and dailySummary fan out by league (#39)", {
  local_mocked_bindings(
    .today_odds_plot_nhl = function(...) "nhl-today",
    .today_odds_plot_pwhl = function(...) "pwhl-today",
    .daily_summary_nhl = function(...) "nhl-summary",
    dailyPWHLSummary = function(...) "pwhl-summary",
    .package = "HockeyModel"
  )

  expect_identical(
    todayOddsPlot(date = as.Date("2025-01-15"), league = "nhl"),
    "nhl-today"
  )
  expect_identical(
    todayOddsPlot(date = as.Date("2025-01-15"), league = "pwhl"),
    "pwhl-today"
  )
  expect_identical(
    todayOddsPlot(date = as.Date("2025-01-15")),
    list(nhl = "nhl-today", pwhl = "pwhl-today")
  )

  expect_identical(dailySummary(league = "nhl"), "nhl-summary")
  expect_identical(dailySummary(league = "pwhl"), "pwhl-summary")
  expect_identical(
    dailySummary(),
    list(nhl = "nhl-summary", pwhl = "pwhl-summary")
  )
})

test_that("PWHL front ends use league defaults for omitted inputs", {
  observed <- new.env(parent = emptyenv())

  local_mocked_bindings(
    .update_predictions_pwhl = function(data_dir, scores, schedule, params) {
      observed$prediction_dir <- data_dir
      observed$prediction_scores <- scores
      observed$prediction_schedule <- schedule
      observed$prediction_params <- params
      invisible(NULL)
    },
    .today_odds_plot_pwhl = function(date, params, schedule, scores) {
      observed$today_date <- date
      observed$today_params <- params
      observed$today_schedule <- schedule
      observed$today_scores <- scores
      "pwhl-today"
    },
    .ratings_pwhl = function(m) {
      observed$ratings_m <- m
      "pwhl-ratings"
    },
    .package = "HockeyModel"
  )

  prediction_dir <- withr::local_tempdir()

  expect_identical(
    updatePredictions(data_dir = prediction_dir, league = "pwhl"),
    invisible(NULL)
  )
  expect_identical(
    todayOddsPlot(date = as.Date("2025-01-15"), league = "pwhl"),
    "pwhl-today"
  )
  expect_identical(ratings(league = "pwhl"), "pwhl-ratings")

  expect_identical(observed$prediction_dir, file.path(prediction_dir, "pwhl"))
  expect_identical(observed$prediction_scores, HockeyModel::pwhlScores)
  expect_identical(observed$prediction_schedule, HockeyModel::pwhlSchedule)
  expect_null(observed$prediction_params)
  expect_identical(observed$today_date, as.Date("2025-01-15"))
  expect_identical(observed$today_schedule, HockeyModel::pwhlSchedule)
  expect_identical(observed$today_scores, HockeyModel::pwhlScores)
  expect_null(observed$today_params)
  expect_identical(observed$ratings_m, HockeyModel::pwhl_m)
})

test_that("dailySummary uses option-based graphic defaults for both leagues (#39)", {
  withr::local_options(
    list(
      HockeyModel.graphics.path = "/tmp/nhl-graphics",
      HockeyModel.prediction.path = "/tmp/prediction-root"
    )
  )

  observed <- new.env(parent = emptyenv())

  local_mocked_bindings(
    .daily_summary_nhl = function(graphic_dir, ...) {
      observed$nhl <- graphic_dir
      "nhl-summary"
    },
    dailyPWHLSummary = function(graphic_dir, ...) {
      observed$pwhl <- graphic_dir
      "pwhl-summary"
    },
    .package = "HockeyModel"
  )

  expect_identical(
    dailySummary(),
    list(nhl = "nhl-summary", pwhl = "pwhl-summary")
  )
  expect_identical(observed$nhl, "/tmp/nhl-graphics")
  expect_identical(
    observed$pwhl,
    file.path("/tmp/prediction-root", "pwhl_graphics")
  )
})

test_that("plot wrappers route PWHL prediction data through shared graphics (#39)", {
  all_predictions <- tibble::tibble(
    predictionDate = as.Date(c("2025-01-14", "2025-01-15")),
    Team = c("Boston Fleet", "Boston Fleet"),
    meanPoints = c(70, 71),
    Playoffs = c(0.7, 0.8),
    Presidents = c(0.4, 0.45)
  )

  local_mocked_bindings(
    compile_predictions = function(dir) {
      expect_match(dir, "pwhl$")
      all_predictions
    },
    plot_prediction_playoffs_by_team = function(
      all_predictions,
      teamColours,
      ...
    ) {
      list(kind = "playoffs", teams = teamColours$Team, data = all_predictions)
    },
    plot_prediction_presidents_by_team = function(
      all_predictions,
      teamColours,
      ...
    ) {
      list(
        kind = "presidents",
        teams = teamColours$Team,
        data = all_predictions
      )
    },
    plot_prediction_points_by_team = function(
      all_predictions,
      teamColours,
      ...
    ) {
      list(kind = "points", teams = teamColours$Team, data = all_predictions)
    },
    .package = "HockeyModel"
  )

  playoff_plot <- playoffOdds(data_dir = tempdir(), league = "pwhl")
  president_plot <- presidentOdds(data_dir = tempdir(), league = "pwhl")
  point_plot <- pointPredict(data_dir = tempdir(), league = "pwhl")

  expect_identical(playoff_plot$kind, "playoffs")
  expect_identical(president_plot$kind, "presidents")
  expect_identical(point_plot$kind, "points")
  expect_true("Boston Fleet" %in% playoff_plot$teams)
  expect_s3_class(playoff_plot$data$predictionDate, "Date")
})
