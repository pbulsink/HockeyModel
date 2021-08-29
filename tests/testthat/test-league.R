context("test-league")

test_that("Playoff series odds ok", {
  expect_equal(playoffSeriesOdds(0.5, 0.5), 0.5)
  expect_equal(playoffSeriesOdds(0.7, 0.3, 3,3), 0.7)
  expect_equal(playoffSeriesOdds(0.5,0.5,0,3), 1/2^4)
  expect_error(playoffSeriesOdds(1.2,0.5), regexp = 'impossible odds')
  expect_message(playoffSeriesOdds(0.5,0.5,4,2), regexp = 'series already won')
})

test_that("Playoff Sim finishes OK", {
  summary_results<-HockeyModel::summary_results_testing
  playoffResults<-simulatePlayoffs(summary_results = summary_results, nsims=2, cores=1)
  expect_true(is.data.frame(playoffResults))
  expect_true(all(playoffResults$Make_Playoffs <= 1))
  expect_equal(sum(playoffResults$Make_Playoffs), 16)
  expect_equal(sum(playoffResults$Win_First_Round), 8)
  expect_equal(sum(playoffResults$Win_Second_Round), 4)
  expect_equal(sum(playoffResults$Win_Conference), 2)
  expect_equal(sum(playoffResults$Win_Cup), 1)

  playoffResults<-simulatePlayoffs(summary_results = summary_results, nsims=4, cores=2)
  expect_true(is.data.frame(playoffResults))
  expect_true(all(playoffResults$Make_Playoffs <= 1))
  expect_equal(sum(playoffResults$Make_Playoffs), 16)
  expect_equal(sum(playoffResults$Win_First_Round), 8)
  expect_equal(sum(playoffResults$Win_Second_Round), 4)
  expect_equal(sum(playoffResults$Win_Conference), 2)
  expect_equal(sum(playoffResults$Win_Cup), 1)
})

test_that("Convenience Functions are OK", {
  expect_equal(nrow(todayOdds(today=as.Date("2019-11-01"))), 8)
  expect_equal(ncol(todayOdds(today=as.Date("2019-11-01"))), 5)
})

test_that("Predictions File saves", {
  expect_true(suppressWarnings(build_past_predictions(startDate = "2021-01-30", endDate = "2021-01-30", filepath = "./odds.csv")))
  expect_true(file.exists("./odds.csv"))
  preds<-read.csv("./odds.csv")
  expect_equal(nrow(preds), 12)
  expect_equal(ncol(preds), 7)
  expect_equal(names(preds), c("Date", "GameID", "HomeTeam", "AwayTeam", "HomeWin", "AwayWin", "Draw"))

  expect_true(cleanupPredictionsFile("./odds.csv"))

  file.remove("./odds.csv")
  expect_false(file.exists("./odds.csv"))
})
