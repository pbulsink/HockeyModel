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
  sc<-sc[sc$Date < as.Date("2018-09-01"), ]
  p<-historicalPoints(sc = sc)
  expect_equal(as.numeric(p[p$Team == "Anaheim Ducks" & p$Season == "20152016", "Points"]), 103)
  expect_equal(as.numeric(p[p$Team == "Ottawa Senators" & p$Season == "20162017", "Points"]), 98)
  expect_equal(as.numeric(p[p$Team == "Toronto Maple Leafs" & p$Season == "20172018", "Points"]), 105)
  expect_true(is.na(as.numeric(p[p$Team == "Vegas Golden Knights" & p$Season == "20152016", "Points"])))
})

test_that("Metrics are correctly calculated", {
  expect_equal(rmse(c(0.1, 0.2), c(0.15, 0.25)), 0.05)

  expect_equal(auc(c(0,0,1,1), c(0.1, 0.2, 0.6, 0.7)),1)
  expect_equal(auc(c(0,0,1,1), c(0.1, 0.6, 0.4, 0.7)), 0.5)

  expect_equal(logLoss(0,0), 0)
  expect_equal(logLoss(1,1), 0)
  expect_equal(logLoss(0.5, 1), -log(0.5))
  expect_equal(logLoss(0.5, 1), logLoss(0.5, 0))
  expect_equal(logLoss(c(0,1), c(0,1)), 0)
  expect_equal(logLoss(c(0.5,0), c(0, 0)), mean(c(0, -log(0.5))))

  expect_equal(accuracy(0.4, 0), 1)
  expect_equal(accuracy(0.4, 1), 0)
  expect_equal(accuracy(c(0.4, 0.6), c(1,1)), 0.5)

  expect_error(rmse(c(1,2,3), c(1,2)))
  expect_error(auc(c(1,2,3), c(1,2)))
  expect_error(logLoss(c(1,2,3), c(1,2)))
  expect_error(accuracy(c(1,2,3), c(1,2)))
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

test_that("GameIDs are validated", {
  expect_true(gameIDValidator("2021021001"))
  expect_true(gameIDValidator(2021021001))
  expect_false(gameIDValidator("2021091001"))
  expect_false(gameIDValidator("bob"))
  expect_false(gameIDValidator(TRUE))
})

test_that("IneffectiveMath HockeyVis Contest output is a string", {
  preds<-HockeyModel::example_predictions
  preds<-preds[preds$predictionDate == max(preds$predictionDate), ]
  im<-formatPredsForHockeyVisContest(predictions=preds)
  expect_true(is.character(im))
  expect_true(grepl("pbulsink", x=im))
})

test_that("Season Validates", {
  expect_true(seasonValidator("20202021"))
  expect_false(seasonValidator(20202021))
  expect_false(seasonValidator("Bob"))
  expect_false(seasonValidator(TRUE))
})

test_that("Draws Normalize", {
  expect_equal(extraTimeSolver(0.45, 0.35, 0.2), c(0.45, 0.1018125, 0.0981875, 0.35))
})
