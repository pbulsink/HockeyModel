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
