test_that("Team Strength Plot Graphics Produce",{
  p<-plot_team_rating()

  expect_true(ggplot2::is.ggplot(p))
  expect_identical(p$labels$title, "Current Team Offence & Defence Ratings")
  expect_identical(p$labels$y, "Defence")
  expect_identical(p$labels$x, "Offence")
})

test_that("Points Predictions by Team Graphics Produce", {
  #Using the example predictions file, past 'n' days is today - 2021-01-12 (the first day of predictions)
  p<-suppressWarnings(plot_prediction_points_by_team(all_predictions = HockeyModel::example_predictions, past_days=Sys.Date()-as.Date("2021-01-12")))
  expect_true(ggplot2::is.ggplot(p))
  expect_identical(p$labels$title, paste0("Predicted Points Over the Past ", Sys.Date()-as.Date("2021-01-12")," Days"))
  expect_identical(p$labels$y, "Points")
  expect_identical(p$labels$x, "Date")
})

test_that("Playoffs Predictions by Team Graphics Produce", {
  #Using the example predictions file, past 'n' days is today - 2021-01-12 (the first day of predictions)
  p<-suppressWarnings(plot_prediction_playoffs_by_team(all_predictions = HockeyModel::example_predictions, past_days=Sys.Date()-as.Date("2021-01-12")))
  expect_true(ggplot2::is.ggplot(p))
  expect_identical(p$labels$title, paste0("Playoff Odds Over the Past ", Sys.Date()-as.Date("2021-01-12")," Days"))
  expect_identical(p$labels$y, "Playoff Odds")
  expect_identical(p$labels$x, "Date")
})

test_that("Presidents Predictions by Team Graphics Produce", {
  #Using the example predictions file, past 'n' days is today - 2021-01-12 (the first day of predictions)
  p<-suppressWarnings(plot_prediction_presidents_by_team(all_predictions = HockeyModel::example_predictions, past_days=Sys.Date()-as.Date("2021-01-12"), minimum = 0.01))
  expect_true(ggplot2::is.ggplot(p))
  expect_identical(p$labels$title, paste0("President's Trophy Odds Over the Past ", Sys.Date()-as.Date("2021-01-12")," Days"))
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

test_that("Single Game xG plot OK", {
  p<-suppressWarnings(plot_game("Vancouver Canucks", "Edmonton Oilers"))
  expect_true(ggplot2::is.ggplot(p))
  expect_identical(p$labels$title, "Predicted Goals")
  expect_identical(p$labels$y, "Odds")
  expect_identical(p$labels$x, "Predicted Team Goals")
})

test_that("Predicted Points plot OK", {
  preds<-HockeyModel::example_predictions
  preds<-preds[preds$predictionDate == "2021-01-30", ]
  p<-suppressWarnings(plot_point_likelihood(preds = preds, savefiles = F))
  for(i in 1:length(p)){
    expect_true(ggplot2::is.ggplot(p[[i]]))
    expect_identical(p[[i]]$labels$y, "")
    expect_identical(p[[i]]$labels$x, "Predicted Point Likelyhood")
  }
})

test_that("Series Graphics are OK", {
  series<-getAPISeries("20182019")
  series<-series[1:3, c("HomeTeam", "AwayTeam", "HomeWins", "AwayWins")]
  series$HomeWins<-c(0, 3, 3)
  series$AwayWins<-c(4, 3, 2)

  p<-suppressWarnings(plot_playoff_series_odds(series=series))
  expect_true(ggplot2::is.ggplot(p))
  expect_identical(p$labels$title, "Predictions for Playoff Series")
  expect_identical(p$labels$y, "Series Odds")
  expect_identical(p$labels$x, "")
})
