test_that("Points Predictions by Team Graphics Produce", {
  skip_if_not_installed("ggforce")
  if (!requireNamespace('ggforce')) {
    expect_error(
      suppressWarnings(plot_prediction_points_by_team(
        all_predictions = HockeyModel::example_predictions,
        past_days = Sys.Date() - as.Date("2021-01-12")
      )),
      "Package ggforce is required"
    )
    skip("No `ggforce` package.")
  }

  # Using the example predictions file, past 'n' days is today - 2021-01-12 (the first day of predictions)
  p <- suppressWarnings(plot_prediction_points_by_team(
    all_predictions = HockeyModel::example_predictions,
    past_days = Sys.Date() - as.Date("2021-01-12")
  ))
  expect_true(ggplot2::is_ggplot(p))
  expect_identical(
    p$labels$title,
    paste0(
      "Predicted Points Over the Past ",
      Sys.Date() - as.Date("2021-01-12"),
      " Days"
    )
  )
  expect_identical(p$labels$y, "Points")
  expect_identical(p$labels$x, "Date")
})

test_that("Playoffs Predictions by Team Graphics Produce", {
  skip_if_not_installed("ggforce")
  # Using the example predictions file, past 'n' days is today - 2021-01-12 (the first day of predictions)
  p <- suppressWarnings(plot_prediction_playoffs_by_team(
    all_predictions = HockeyModel::example_predictions,
    past_days = Sys.Date() - as.Date("2021-01-12")
  ))
  expect_true(ggplot2::is_ggplot(p))
  expect_identical(
    p$labels$title,
    paste0(
      "Playoff Odds Over the Past ",
      Sys.Date() - as.Date("2021-01-12"),
      " Days"
    )
  )
  expect_identical(p$labels$y, "Playoff Odds")
  expect_identical(p$labels$x, "Date")
})

test_that("Presidents Predictions by Team Graphics Produce", {
  if (!requireNamespace('ggforce')) {
    expect_error(
      suppressWarnings(plot_prediction_presidents_by_team(
        all_predictions = HockeyModel::example_predictions,
        past_days = Sys.Date() - as.Date("2021-01-12"),
        minimum = 0.01
      )),
      "Package ggforce is required"
    )
    skip("No `ggforce` package.")
  }

  # Using the example predictions file, past 'n' days is today - 2021-01-12 (the first day of predictions)
  p <- suppressWarnings(plot_prediction_presidents_by_team(
    all_predictions = HockeyModel::example_predictions,
    past_days = Sys.Date() - as.Date("2021-01-12"),
    minimum = 0.01
  ))
  expect_true(ggplot2::is_ggplot(p))
  expect_identical(
    p$labels$title,
    paste0(
      "President's Trophy Odds Over the Past ",
      Sys.Date() - as.Date("2021-01-12"),
      " Days"
    )
  )
  expect_identical(p$labels$y, "President's Trophy Odds")
  expect_identical(p$labels$x, "Date")
})

test_that("Presidents predictions keep a PWHL facet", {
  all_predictions <- tibble::tibble(
    predictionDate = as.Date(c("2025-01-14", "2025-01-15")),
    Team = c("Boston Fleet", "Boston Fleet"),
    Presidents = c(0.15, 0.2)
  )

  if (!requireNamespace('ggforce')) {
    expect_error(
      suppressWarnings(plot_prediction_presidents_by_team(
        all_predictions = all_predictions,
        past_days = 14,
        minimum = 0.01,
        teamColours = HockeyModel::pwhlTeamColours
      )),
      "Package ggforce is required"
    )
    skip("No `ggridges` package.")
  }

  p <- plot_prediction_presidents_by_team(
    all_predictions = all_predictions,
    past_days = 14,
    minimum = 0.01,
    teamColours = HockeyModel::pwhlTeamColours
  )

  expect_false(any(is.na(p$data$facet)))
  expect_identical(as.character(unique(p$data$facet)), "PWHL")
})

# ============ plot_prediction_playoffs_by_team tests ============
test_that("plot_prediction_playoffs_by_team executes gracefully", {
  if (!requireNamespace('ggforce')) {
    expect_error(
      plot_prediction_playoffs_by_team(all_predictions = example_predictions),
      "Package ggforce is required"
    )
    skip("No `ggforce` package.")
  }
  p <- plot_prediction_playoffs_by_team(all_predictions = example_predictions)
  expect_true(ggplot2::is_ggplot(p))
})
