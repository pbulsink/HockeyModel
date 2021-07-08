context("test-api-interface")

test_that("Season Dates & binaries work", {
  expect_visible(inRegularSeason())
  expect_visible(inPlayoffs())
  expect_visible(inOffSeason())
  expect_false(any(inRegularSeason(), inPlayoffs()) == inOffSeason())
  expect_equal(inRegularSeason("2018-12-02", boolean = FALSE), "20182019")
  expect_false(inPlayoffs("2018-12-02", boolean = FALSE))
  expect_true(inOffSeason("2018-08-01"))
})

test_that("SeasonID gets seasons ok", {
  expect_match(getCurrentSeason8(), regexp = "\\d{8}")
  expect_equal(getSeason("2018-12-02"), "20182019")
  expect_null(getSeason("2018-09-01"))
})
