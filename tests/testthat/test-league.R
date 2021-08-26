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

})
