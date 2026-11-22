# Front-end season-state dispatch tests for issue #43

test_that("inRegularSeason() evaluates both leagues by default (#43)", {
  observed <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .in_regular_season_nhl = function(date, boolean, schedule) {
      observed$nhl_reg <- list(date = date, boolean = boolean)
      "20182019"
    },
    .in_playoffs_nhl = function(...) FALSE,
    .in_off_season_nhl = function(...) TRUE,
    .in_regular_season_pwhl = function(date, boolean, schedule) {
      observed$pwhl_reg <- list(date = date, boolean = boolean)
      "5"
    },
    .in_playoffs_pwhl = function(...) FALSE,
    .in_off_season_pwhl = function(...) TRUE,
    .package = "HockeyModel"
  )

  check_date <- as.Date("2018-12-02")

  result <- inRegularSeason(date = check_date)
  expect_named(result, c("nhl", "pwhl"))
  expect_equal(result$nhl, "20182019")
  expect_equal(result$pwhl, "5")
  expect_identical(observed$nhl_reg$date, check_date)
  expect_true(observed$nhl_reg$boolean)

  result <- inRegularSeason(date = check_date, boolean = FALSE, league = "both")
  expect_named(result, c("nhl", "pwhl"))
  expect_false(observed$pwhl_reg$boolean)

  expect_equal(
    inRegularSeason(date = check_date, league = NA),
    list(nhl = "20182019", pwhl = "5")
  )
})

test_that("inRegularSeason() returns a single result for one league (#43)", {
  local_mocked_bindings(
    .in_regular_season_nhl = function(...) "20182019",
    .in_playoffs_nhl = function(...) FALSE,
    .in_off_season_nhl = function(...) TRUE,
    .in_regular_season_pwhl = function(...) "5",
    .in_playoffs_pwhl = function(...) FALSE,
    .in_off_season_pwhl = function(...) TRUE,
    .package = "HockeyModel"
  )

  check_date <- as.Date("2018-12-02")

  expect_equal(inRegularSeason(date = check_date, league = "nhl"), "20182019")
  expect_equal(inRegularSeason(date = check_date, league = "NHL"), "20182019")
  expect_equal(inRegularSeason(date = check_date, league = "pwhl"), "5")
  expect_equal(inRegularSeason(date = check_date, league = "PWHL"), "5")
})

test_that("inPlayoffs() dispatches by league (#43)", {
  observed <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .in_regular_season_nhl = function(...) FALSE,
    .in_playoffs_nhl = function(date, boolean, schedule) {
      observed$nhl_po <- list(date = date, boolean = boolean)
      "20182019"
    },
    .in_off_season_nhl = function(...) TRUE,
    .in_regular_season_pwhl = function(...) FALSE,
    .in_playoffs_pwhl = function(date, boolean, schedule) {
      observed$pwhl_po <- list(date = date, boolean = boolean)
      "3"
    },
    .in_off_season_pwhl = function(...) TRUE,
    .package = "HockeyModel"
  )

  check_date <- as.Date("2019-05-01")

  result <- inPlayoffs(date = check_date)
  expect_named(result, c("nhl", "pwhl"))
  expect_equal(result$nhl, "20182019")
  expect_equal(result$pwhl, "3")
  expect_identical(observed$nhl_po$date, check_date)

  expect_equal(inPlayoffs(date = check_date, league = "nhl"), "20182019")
  expect_equal(
    inPlayoffs(date = check_date, boolean = FALSE, league = "pwhl"),
    "3"
  )
})

test_that("inOffSeason() dispatches by league (#43)", {
  observed <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .in_regular_season_nhl = function(...) FALSE,
    .in_playoffs_nhl = function(...) FALSE,
    .in_off_season_nhl = function(date, schedule) {
      observed$nhl_off <- date
      TRUE
    },
    .in_regular_season_pwhl = function(...) FALSE,
    .in_playoffs_pwhl = function(...) FALSE,
    .in_off_season_pwhl = function(date, schedule) {
      observed$pwhl_off <- date
      FALSE
    },
    .package = "HockeyModel"
  )

  check_date <- as.Date("2019-08-01")

  result <- inOffSeason(date = check_date)
  expect_named(result, c("nhl", "pwhl"))
  expect_true(result$nhl)
  expect_false(result$pwhl)
  expect_identical(observed$nhl_off, check_date)
  expect_identical(observed$pwhl_off, check_date)

  expect_true(inOffSeason(date = check_date, league = "NHL"))
  expect_false(inOffSeason(date = check_date, league = "pwhl"))
})

test_that("front-end season functions reject invalid leagues (#43)", {
  expect_error(inRegularSeason(league = "KHL"), "must be")
  expect_error(inPlayoffs(league = c("nhl", "pwhl")), "must be")
  expect_error(inOffSeason(league = 1), "must be")
})
