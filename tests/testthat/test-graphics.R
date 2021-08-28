test_that("Team Strength Plot Graphics Produce",{
  p<-plot_team_rating()

  expect_true(ggplot2::is.ggplot(p))
  expect_identical(p$labels$title, "Current Team Offence & Defence Ratings")
  expect_identical(p$labels$y, "Defence")
  expect_identical(p$labels$x, "Offence")
})

test_that("Points Predictions by Team Graphics Produce", {
  #Have to use a large number of past days because mid-summer tests with past 14 days would fail
  p<-suppressWarnings(plot_prediction_points_by_team(past_days=365))
  expect_true(ggplot2::is.ggplot(p))
  expect_identical(p$labels$title, "Predicted Points Over the Past 365 Days")
  expect_identical(p$labels$y, "Points")
  expect_identical(p$labels$x, "Date")
})

test_that("Playoffs Predictions by Team Graphics Produce", {
  p<-suppressWarnings(plot_prediction_playoffs_by_team(past_days=365))
  expect_true(ggplot2::is.ggplot(p))
  expect_identical(p$labels$title, "Playoff Odds Over the Past 365 Days")
  expect_identical(p$labels$y, "Playoff Odds")
  expect_identical(p$labels$x, "Date")
})

test_that("Presidents Predictions by Team Graphics Produce", {
  p<-suppressWarnings(plot_prediction_presidents_by_team(past_days=365))
  sleep(30) # presidents' very slow for some reason currently
  expect_true(ggplot2::is.ggplot(p))
  expect_identical(p$labels$title, "President's Trophy Odds Over the Past 365 Days")
  expect_identical(p$labels$y, "President's Trophy Odds")
  expect_identical(p$labels$x, "Date")
})

test_that("Today Odds plot OK", {
  p<-suppressWarnings(plot_odds_today(today=as.Date("2019-11-01")))
  expect_true(ggplot2::is.ggplot(p))
  expect_identical(p$labels$title, "Predictions for Today's Games")
  expect_identical(p$labels$y, "Result Odds")
  expect_identical(p$labels$x, "")

  p<-suppressWarnings(daily_odds_table(today = as.Date("2019-11-01")))
  expect_true('gt_tbl' %in% class(p))
  expect_identical(p$`_heading`$title, "Game Odds")
})
