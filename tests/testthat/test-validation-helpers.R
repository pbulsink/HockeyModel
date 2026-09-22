context("test-utils")

# ============ normalizeOdds tests ============
test_that("normalizeOdds works", {
  expect_equal(sum(normalizeOdds(runif(3))), 1)
  expect_equal(sum(normalizeOdds(runif(2))), 1)
  expect_equal(sum(normalizeOdds(c(0.3, 0.4, 0.5))), 1)
})

test_that("normalizeOdds clamps values", {
  result <- normalizeOdds(c(-0.5, 0.5, 2))
  expect_equal(sum(result), 1)
  expect_true(all(result > 0))
  expect_true(all(result < 1))
})

test_that("normalizeOdds handles edge cases", {
  expect_equal(sum(normalizeOdds(c(0, 0, 1))), 1)
  expect_equal(sum(normalizeOdds(c(1, 1, 1))), 1)
})

test_that("normalizeOdds handles single values", {
  result <- normalizeOdds(c(0.5))
  expect_equal(result, 1)
})

test_that("normalizeOdds works with unlisted vectors", {
  result <- normalizeOdds(list(0.2, 0.3, 0.5))
  expect_equal(sum(result), 1)
})

test_that("Past points function works", {
  # Just test that the function runs without error
  sc <- scores[
    scores$Date > as.Date("2017-11-01") & scores$Date < as.Date("2017-12-01"),
  ]

  p <- HockeyModel:::historicalPoints(sc = sc)
  expect_true(is.data.frame(p))
  expect_equal(nrow(p), 31)
  expect_equal(colnames(p), c("Team", "Points", "Season"))
})

test_that("Date Checks are OK", {
  expect_true(is.Date("2020-12-13"))
  expect_false(is.Date("bob"))
  expect_false(is.Date("2020-02-30"))
})

test_that("is.Date handles valid dates", {
  expect_true(is.Date("2020-12-13"))
  dates <- c("2020-12-13", "2021-01-01", "2022-06-15")
  results <- sapply(dates, is.Date)
  expect_true(all(results))
})

test_that("is.Date rejects invalid date strings", {
  expect_false(is.Date("not-a-date"))
  expect_false(is.Date("bob"))
})

test_that("GameIDs are validated", {
  expect_true(gameIDValidator("2021021001"))
  expect_true(gameIDValidator(2021021001))
  expect_false(gameIDValidator("2021091001"))
  expect_false(gameIDValidator("bob"))
  expect_false(gameIDValidator(TRUE))
})

test_that("gameIDValidator handles vectors", {
  valid_ids <- c("2021021001", "2020020500", "2019030100")
  expect_true(all(gameIDValidator(valid_ids)))
})

test_that("gameIDValidator rejects invalid game types and bad input", {
  expect_false(gameIDValidator("2021051001"))
  expect_false(gameIDValidator("bob"))
  expect_false(gameIDValidator(TRUE))
})

test_that("gameIDValidator handles edge cases", {
  expect_true(gameIDValidator("2001010001"))
  expect_true(gameIDValidator("2099041999"))
})

test_that("Season Validates", {
  expect_true(seasonValidator("20202021"))
  expect_false(seasonValidator("Bob"))
  expect_false(seasonValidator(TRUE))
})

test_that("Draws Normalize", {
  expect_equal(
    extraTimeSolver(0.45, 0.35, 0.2),
    c(0.45, 0.1018125, 0.0981875, 0.35)
  )
})
