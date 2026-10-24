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
    .active_frontend_leagues = function() c("NHL", "PWHL"),
    .update_model_nhl = function(save_data = TRUE) {
      list(source = "nhl", save_data = save_data)
    },
    .update_model_pwhl = function(save_data = TRUE) {
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
    .update_model_pwhl = function(save_data = TRUE) {
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

# Front-end active-league dispatch tests for issue #44

test_that(".active_frontend_leagues returns only leagues with games on (#44)", {
  local_mocked_bindings(
    .in_off_season_nhl = function(...) FALSE,
    .in_off_season_pwhl = function(...) TRUE,
    .package = "HockeyModel"
  )

  expect_identical(.active_frontend_leagues(), "NHL")

  local_mocked_bindings(
    .in_off_season_nhl = function(...) TRUE,
    .in_off_season_pwhl = function(...) FALSE,
    .package = "HockeyModel"
  )

  expect_identical(.active_frontend_leagues(), "PWHL")

  local_mocked_bindings(
    .in_off_season_nhl = function(...) TRUE,
    .in_off_season_pwhl = function(...) TRUE,
    .package = "HockeyModel"
  )

  expect_error(.active_frontend_leagues(), "Neither NHL nor PWHL are active")
})

test_that("updateModel() updates both leagues by default (#44)", {
  local_mocked_bindings(
    .update_model_nhl = function(save_data = TRUE) list(source = "nhl"),
    .update_model_pwhl = function(save_data = TRUE) list(source = "pwhl"),
    .package = "HockeyModel"
  )

  result <- updateModel()

  expect_named(result, c("nhl", "pwhl"))
})

test_that("league = 'both' forces both leagues regardless of activity (#44)", {
  local_mocked_bindings(
    .update_model_nhl = function(save_data = TRUE) list(source = "nhl"),
    .update_model_pwhl = function(save_data = TRUE) list(source = "pwhl"),
    .package = "HockeyModel"
  )

  result <- updateModel(league = "both")

  expect_named(result, c("nhl", "pwhl"))
})

test_that("updatePredictions() only updates active leagues by default (#44)", {
  local_mocked_bindings(
    .active_frontend_leagues = function() "NHL",
    .update_predictions_nhl = function(...) invisible(NULL),
    .update_predictions_pwhl = function(...) stop("should not run"),
    .package = "HockeyModel"
  )

  expect_null(updatePredictions())
})

test_that("dailySummary() only runs active league workflows by default (#44)", {
  local_mocked_bindings(
    .active_frontend_leagues = function() "PWHL",
    .daily_summary_nhl = function(...) stop("should not run"),
    .daily_summary_pwhl = function(...) "pwhl-summary",
    .package = "HockeyModel"
  )

  expect_identical(dailySummary(), "pwhl-summary")
})

test_that("plot wrappers only plot active leagues by default (#44)", {
  local_mocked_bindings(
    .active_frontend_leagues = function() "NHL",
    .today_odds_plot_nhl = function(...) "nhl-today",
    .today_odds_plot_pwhl = function(...) stop("should not run"),
    .playoff_odds_nhl = function(data_dir) list(kind = "playoffs"),
    .playoff_odds_pwhl = function(data_dir) stop("should not run"),
    .president_odds_nhl = function(data_dir) list(kind = "presidents"),
    .president_odds_pwhl = function(data_dir) stop("should not run"),
    .point_predict_nhl = function(data_dir) list(kind = "points"),
    .point_predict_pwhl = function(data_dir) stop("should not run"),
    .ratings_nhl = function(m) "nhl-ratings",
    .ratings_pwhl = function(m) stop("should not run"),
    .package = "HockeyModel"
  )

  expect_identical(todayOddsPlot(), "nhl-today")
  expect_identical(playoffOdds(data_dir = tempdir())$kind, "playoffs")
  expect_identical(presidentOdds(data_dir = tempdir())$kind, "presidents")
  expect_identical(pointPredict(data_dir = tempdir())$kind, "points")
  expect_identical(ratings(), "nhl-ratings")
})

test_that("season-state queries still evaluate both leagues by default (#44)", {
  local_mocked_bindings(
    .in_regular_season_nhl = function(...) TRUE,
    .in_playoffs_nhl = function(...) FALSE,
    .in_off_season_nhl = function(...) FALSE,
    .in_regular_season_pwhl = function(...) FALSE,
    .in_playoffs_pwhl = function(...) FALSE,
    .in_off_season_pwhl = function(...) TRUE,
    .package = "HockeyModel"
  )

  result <- inRegularSeason()

  expect_named(result, c("nhl", "pwhl"))
  expect_true(result$nhl)
  expect_false(result$pwhl)
})
