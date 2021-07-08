context("test-utils")

test_that("odds normalization works", {
  expect_equal(sum(normalizeOdds(runif(3))), 1)
  expect_equal(sum(normalizeOdds(runif(2))), 1)
})

test_that("Season from Game Date works", {
  expect_equal(getSeason("2018-10-05"), "20182019")
  expect_equal(getSeason("2019-02-15"), "20182019")

  expect_equal(getSeason(c("2018-10-05", "2019-02-15")), c("20182019", "20182019"))
})

test_that("Past points are calculated correctly", {
  sc<-scores[scores$Date > as.Date("2015-08-01"), ]
  p<-historicalPoints(sc = sc)
  expect_equal(as.numeric(p[p$Team == "Anaheim Ducks" & p$Season == "20152016", "Points"]), 103)
  expect_equal(as.numeric(p[p$Team == "Ottawa Senators" & p$Season == "20162017", "Points"]), 98)
  expect_equal(as.numeric(p[p$Team == "Toronto Maple Leafs" & p$Season == "20172018", "Points"]), 105)
  expect_true(is.na(as.numeric(p[p$Team == "Vegas Golden Knights" & p$Season == "20152016", "Points"])))
})

test_that("Metrics are correctly calculated", {
  expect_equal(logLoss(0,0), 0)
  expect_equal(logLoss(1,1), 0)
  expect_equal(logLoss(0.5, 1), -log(0.5))
  expect_equal(logLoss(0.5, 1), logLoss(0.5, 0))
  expect_equal(logLoss(c(0,1), c(0,1)), 0)
  expect_equal(logLoss(c(0.5,0), c(0, 0)), mean(c(0, -log(0.5))))

  expect_equal(accuracy(0.4, 0), 1)
  expect_equal(accuracy(0.4, 1), 0)
  expect_equal(accuracy(c(0.4, 0.6), c(1,1)), 0.5)
})

test_that("Colours are correctly compared", {
  expect_equal(hexToRGB("#000000"), c(0,0,0))
  expect_equal(hexToRGB("#FFFFFF"), c(255, 255, 255))
  expect_equal(hexToRGB("#101010"), c(16,16,16))

  expect_equal(colourDelta("#000000", "#000000"), 0)
  expect_equal(colourDelta("#000000", "#FFFFFF"), 1)

  expect_equal(colourDelta("#0000FF", "#000000"), 1/3)
})

test_that("Date Checks are OK", {
  expect_true(is.Date("2020-12-13"))
  expect_false(is.Date("bob"))
  expect_false(is.Date(8))
  expect_false(is.Date("2020-02-30"))
})
