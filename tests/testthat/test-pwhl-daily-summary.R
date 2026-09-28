test_that("dailyPWHLSummary() reports (not silently swallows) failed posts (#noissue)", {
  withr::local_options(list(HockeyModel.prediction.path = withr::local_tempdir()))
  local_mocked_bindings(
    updatePWHLModel = function(...) list(
      schedule = data.frame(
        Date = Sys.Date() + 1,
        GameType = "R",
        HomeTeam = "Team A",
        AwayTeam = "Team B"
      ),
      scores = data.frame(),
      params = NULL
    ),
    parse_pwhl_dc_params = function(params = NULL) list(m = NULL),
    pwhl_games_today = function(schedule, date = Sys.Date()) NULL,
    getPWHLPlayoffSeries = function() data.frame(),
    pwhl_in_season = function(schedule) FALSE,
    .package = "HockeyModel"
  )
  local_mocked_bindings(
    post = function(...) stop("network is down"),
    .package = "atrrr"
  )

  result <- suppressWarnings(dailyPWHLSummary(delay = 0))
  expect_true(is.data.frame(result))
  expect_named(result, c("description", "success", "error"))
})

test_that("dailyPWHLSummary() attributes failed posts to their description (#noissue)", {
  withr::local_options(list(HockeyModel.prediction.path = withr::local_tempdir()))
  local_mocked_bindings(
    updatePWHLModel = function(...) list(
      schedule = data.frame(
        Date = Sys.Date() + 1,
        GameType = "R",
        HomeTeam = "Team A",
        AwayTeam = "Team B"
      ),
      scores = data.frame(),
      params = NULL
    ),
    parse_pwhl_dc_params = function(params = NULL) list(m = NULL),
    pwhl_games_today = function(schedule, date = Sys.Date()) {
      data.frame(Date = Sys.Date() + 1, HomeTeam = "Team A", AwayTeam = "Team B")
    },
    plot_odds_today = function(params, schedule, league = "NHL") NULL,
    daily_odds_table = function(params, schedule, league = "NHL") "a-table",
    save_gt_as_png = function(tbl, filename) invisible(NULL),
    plot_team_rating = function(m, league = "NHL", ...) "a-plot",
    getPWHLPlayoffSeries = function() data.frame(),
    pwhl_in_season = function(schedule) FALSE,
    .package = "HockeyModel"
  )
  local_mocked_bindings(
    post = function(...) stop("rate limited"),
    .package = "atrrr"
  )

  result <- suppressWarnings(
    suppressMessages(
      dailyPWHLSummary(graphic_dir = withr::local_tempdir(), delay = 0)
    )
  )
  expect_equal(nrow(result), 2)
  expect_true(all(!result$success))
  expect_equal(
    result$description,
    c("today's odds table", "current ratings")
  )
  expect_true(all(result$error == "rate limited"))
})
