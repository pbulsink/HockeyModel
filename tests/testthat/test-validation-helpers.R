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
  # getSeason is called once per row; stub it so the test is deterministic
  testthat::local_mocked_bindings(
    getSeason = function(gamedate = Sys.Date()) "20172018"
  )
  # Just test that the function runs without error
  sc <- scores[
    scores$Date > as.Date("2017-11-01") & scores$Date < as.Date("2017-12-01"),
  ]

  p <- HockeyModel:::.historicalPoints(sc = sc)
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
  expect_true(.seasonValidator("20202021"))
  expect_false(.seasonValidator("Bob"))
  expect_false(.seasonValidator(TRUE))
})

test_that(".coerce_date() accepts Dates and date-like strings (#43)", {
  d <- as.Date("2024-05-15")
  expect_identical(.coerce_date(d), d)
  expect_equal(.coerce_date("2024-05-15"), d)
})

test_that(".coerce_date() rejects non-date input (#43)", {
  expect_error(.coerce_date("not-a-date"), "date")
  # Bare numerics would be silently read as days since the epoch by as.Date()
  expect_error(.coerce_date(12345), "date")
  expect_error(.coerce_date(TRUE), "date")
  expect_error(.coerce_date(c("2024-05-15", "2024-06-01")), "date")
})

test_that(".validate_boolean() accepts single logicals (#43)", {
  expect_true(isTRUE(.validate_boolean(TRUE)))
  expect_false(isTRUE(.validate_boolean(FALSE)))
})

test_that(".validate_boolean() rejects invalid flags (#43)", {
  expect_error(.validate_boolean(c(TRUE, FALSE)), "boolean")
  expect_error(.validate_boolean(NA), "boolean")
  expect_error(.validate_boolean("yes"), "boolean")
  expect_error(.validate_boolean(1L), "boolean")
})

test_that("Draws Normalize", {
  expect_equal(
    .extraTimeSolver(0.45, 0.35, 0.2),
    c(0.45, 0.1018125, 0.0981875, 0.35)
  )
})

# #80: postponed-game status handling must use a single source of truth. The
# API returns "PPD" (postponed) and occasionally "RESCHEDULED"; it never
# returns "Postponed", so .add_postponed_to_schedule_end() must match on the
# real statuses or it silently does nothing.
test_that(".add_postponed_to_schedule_end moves past PPD/RESCHEDULED games to season end (#80)", {
  today <- Sys.Date()
  sched <- data.frame(
    Date = c(
      as.Date("2025-11-03"), # played regular-season game (past)
      today - 10, # postponed game with a past date
      today + 5, # future game
      today + 9 # future game
    ),
    HomeTeam = c("A", "B", "C", "D"),
    AwayTeam = c("B", "A", "D", "C"),
    GameID = c(1L, 2L, 3L, 4L),
    GameType = c("R", "R", "P", "P"),
    GameStatus = c("Final", "PPD", "FUT", "FUT"),
    stringsAsFactors = FALSE
  )

  out <- .add_postponed_to_schedule_end(sched)

  # The postponed game is moved to the end of the schedule: the later of the
  # last regular-season date and today.
  expect_equal(out[out$GameID == 2, ]$Date, max(as.Date("2025-11-03"), today))
  # Non-postponed rows are untouched.
  expect_equal(
    out[out$GameID != 2, c("Date", "HomeTeam")],
    sched[sched$GameID != 2, c("Date", "HomeTeam")]
  )
})

test_that(".add_postponed_to_schedule_end also handles RESCHEDULED (#80)", {
  today <- Sys.Date()
  sched <- data.frame(
    Date = c(as.Date("2025-11-03"), today - 4),
    HomeTeam = c("A", "B"),
    AwayTeam = c("B", "A"),
    GameID = c(1L, 2L),
    GameType = c("R", "R"),
    GameStatus = c("Final", "RESCHEDULED"),
    stringsAsFactors = FALSE
  )

  out <- .add_postponed_to_schedule_end(sched)
  expect_equal(out[out$GameID == 2, ]$Date, max(as.Date("2025-11-03"), today))
})

test_that(".add_postponed_to_schedule_end leaves schedules with no postponed games unchanged (#80)", {
  today <- Sys.Date()
  sched <- data.frame(
    Date = c(as.Date("2025-11-03"), today + 5),
    HomeTeam = c("A", "B"),
    AwayTeam = c("B", "A"),
    GameID = c(1L, 2L),
    GameType = c("R", "P"),
    GameStatus = c("Final", "FUT"),
    stringsAsFactors = FALSE
  )

  out <- .add_postponed_to_schedule_end(sched)
  expect_equal(out$Date, sched$Date)
})

test_that(".postponed_statuses is the single source of truth for postponed statuses (#80)", {
  expect_setequal(.postponed_statuses(), c("PPD", "RESCHEDULED"))
  # The status string the API never returns must not be in the set.
  expect_false("Postponed" %in% .postponed_statuses())
})
