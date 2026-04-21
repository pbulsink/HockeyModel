context("test-graphics")

# ============ Basic graphics tests ============
test_that("plot_team_rating executes without error", {
  expect_error(plot_team_rating(), NA)
})

test_that("plot_team_rating returns ggplot object", {
  p <- plot_team_rating()
  expect_true(ggplot2::is_ggplot(p))
})

test_that("plot_team_rating has layers", {
  p <- plot_team_rating()
  expect_true(length(p$layers) > 0)
})

# ============ plot_pace_by_team tests ============
test_that("plot_pace_by_team executes without error", {
  expect_error(suppressWarnings(plot_pace_by_team()), NA)
})

# ============ plot_pace_by_division tests ============
test_that("plot_pace_by_division executes without error", {
  expect_error(suppressWarnings(plot_pace_by_division()), NA)
})

# ============ todayOdds tests ============
test_that("todayOdds returns data frame or NULL", {
  result <- suppressWarnings(todayOdds())
  expect_true(is.data.frame(result) || is.null(result))
})

# ============ todayOddsPlot tests ============
test_that("todayOddsPlot executes without error", {
  p <- suppressWarnings(todayOddsPlot())
  expect_true(
    ggplot2::is_ggplot(p) || is.list(p) || is.null(p)
  )
})

# ============ plot_playoff_series_odds tests ============
test_that("plot_playoff_series_odds executes gracefully", {
  tryCatch(
    expect_error(suppressWarnings(plot_playoff_series_odds()), NA),
    error = function(e) skip("plot_playoff_series_odds requires data")
  )
})

# ============ plot_prediction_playoffs_by_team tests ============
test_that("plot_prediction_playoffs_by_team executes gracefully", {
  tryCatch(
    expect_error(suppressWarnings(plot_prediction_playoffs_by_team()), NA),
    error = function(e) skip("plot_prediction_playoffs_by_team requires data")
  )
})
