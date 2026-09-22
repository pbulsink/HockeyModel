test_that("Team Strength Plot Graphics Produce", {
  p <- plot_team_rating()

  expect_true(ggplot2::is_ggplot(p))
  expect_identical(p$labels$title, "Current NHL Team Offence & Defence Ratings")
  expect_identical(p$labels$y, "Defence")
  expect_identical(p$labels$x, "Offence")
})

test_that("Single Game xG plot OK", {
  p <- suppressWarnings(plot_game("Vancouver Canucks", "Edmonton Oilers"))
  expect_true(ggplot2::is_ggplot(p))
  expect_identical(p$labels$title, "Predicted Goals")
  expect_identical(p$labels$y, "Odds")
  expect_identical(p$labels$x, "Predicted Team Goals")
})

test_that("Predicted Points plot OK", {
  preds <- HockeyModel::example_raw_predictions

  if (!requireNamespace('ggridges')) {
    expect_error(
      suppressWarnings(plot_point_likelihood(preds = preds, savefiles = FALSE)),
      "Package ggridges is required"
    )
    skip("No `ggridges` package.")
  }
  p <- suppressWarnings(plot_point_likelihood(preds = preds, savefiles = FALSE))
  for (i in seq_along(length(p))) {
    expect_true(ggplot2::is_ggplot(p[[i]]))
    expect_identical(p[[i]]$labels$y, "")
    expect_identical(p[[i]]$labels$x, "Predicted Point Likelyhood")
  }
})

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
