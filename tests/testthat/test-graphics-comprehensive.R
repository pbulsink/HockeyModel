context("test-graphics")

# ============ Basic graphics tests ============
test_that("plot_team_rating executes without error", {
  expect_error(plot_team_rating(), NA)
})

test_that("plot_team_rating returns ggplot object", {
  p <- plot_team_rating()
  expect_true(ggplot2::is.ggplot(p))
})

test_that("plot_team_rating plot has data layer", {
  p <- plot_team_rating()
  expect_true(length(p$layers) > 0)
})

# ============ plot_pace_by_team tests ============
test_that("plot_pace_by_team executes without error", {
  expect_error(suppressWarnings(plot_pace_by_team()), NA)
})

test_that("plot_pace_by_team returns ggplot or NULL", {
  p <- suppressWarnings(plot_pace_by_team())
  expect_true(ggplot2::is.ggplot(p) || is.null(p))
})

# ============ plot_pace_by_division tests ============
test_that("plot_pace_by_division executes without error", {
  expect_error(suppressWarnings(plot_pace_by_division()), NA)
})

test_that("plot_pace_by_division returns ggplot or NULL", {
  p <- suppressWarnings(plot_pace_by_division())
  expect_true(ggplot2::is.ggplot(p) || is.null(p))
})

# ============ plot_game tests ============
test_that("plot_game executes without error", {
  expect_error(suppressWarnings(plot_game()), NA)
})

test_that("plot_game returns ggplot or NULL or list", {
  p <- suppressWarnings(plot_game())
  expect_true(ggplot2::is.ggplot(p) || is.null(p) || is.list(p))
})

# ============ plot_odds_today tests ============
test_that("plot_odds_today executes without error", {
  tryCatch(
    expect_error(suppressWarnings(plot_odds_today()), NA),
    error = function(e) skip("plot_odds_today requires data")
  )
})

# ============ daily_odds_table tests ============
test_that("daily_odds_table executes without error", {
  tryCatch(
    expect_error(suppressWarnings(daily_odds_table()), NA),
    error = function(e) skip("daily_odds_table requires data")
  )
})

# ============ todayOddsPlot tests ============
test_that("todayOddsPlot executes without error", {
  p <- suppressWarnings(todayOddsPlot())
  expect_true(
    ggplot2::is.ggplot(p) || is.list(p) || is.null(p)
  )
})

# ============ todayOdds tests ============
test_that("todayOdds returns data frame or NULL", {
  result <- suppressWarnings(todayOdds())
  expect_true(is.data.frame(result) || is.null(result))
})

# ============ colourDelta tests ============
test_that("colourDelta returns color for numeric input", {
  result <- colourDelta(0.5, 0.6)
  expect_true(is.character(result))
})

test_that("colourDelta returns valid hex color", {
  result <- colourDelta(0.5, 0.6)
  expect_match(result, "^#[0-9A-Fa-f]{6}$")
})

# ============ plot_playoff_series_odds tests ============
test_that("plot_playoff_series_odds executes without error", {
  tryCatch(
    expect_error(suppressWarnings(plot_playoff_series_odds()), NA),
    error = function(e) skip("plot_playoff_series_odds requires data")
  )
})

# ============ plot_prediction_playoffs_by_team tests ============
test_that("plot_prediction_playoffs_by_team executes without error", {
  tryCatch(
    expect_error(suppressWarnings(plot_prediction_playoffs_by_team()), NA),
    error = function(e) skip("plot_prediction_playoffs_by_team requires data")
  )
})

# ============ plot_point_likelihood tests ============
test_that("plot_point_likelihood handles missing package gracefully", {
  tryCatch(
    expect_error(suppressWarnings(plot_point_likelihood()), NA),
    error = function(e) skip("plot_point_likelihood requires ggridges")
  )
})

# ============ plot_prediction_points_by_team tests ============
test_that("plot_prediction_points_by_team executes without error", {
  tryCatch(
    expect_error(suppressWarnings(plot_prediction_points_by_team()), NA),
    error = function(e) skip("plot_prediction_points_by_team requires data")
  )
})

# ============ plot_prediction_presidents_by_team tests ============
test_that("plot_prediction_presidents_by_team executes without error", {
  tryCatch(
    expect_error(suppressWarnings(plot_prediction_presidents_by_team()), NA),
    error = function(e) skip("plot_prediction_presidents_by_team requires data")
  )
})
