context("test-league")

test_that("Playoff series odds ok", {
  expect_equal(playoffSeriesOdds(0.5, 0.5), 0.5)
  expect_equal(playoffSeriesOdds(0.7, 0.3, 3,3), 0.7)
  expect_equal(playoffSeriesOdds(0.5,0.5,0,3), 1/2^4)
  expect_error(playoffSeriesOdds(1.2,0.5), regexp = 'impossible odds')
  expect_message(playoffSeriesOdds(0.5,0.5,4,2), regexp = 'series already won')
})
