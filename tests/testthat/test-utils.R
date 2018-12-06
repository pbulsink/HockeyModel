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
