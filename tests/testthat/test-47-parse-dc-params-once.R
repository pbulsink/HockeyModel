# Regression test for Issue #47: .parse_dc_params() should be called only once
# per public call boundary, not again inside the internal prediction chain.
#
# DCPredict() and dcSample() are called once per game, per day, per simulation,
# and previously every internal step (.dcProbMatrix, .dcLambda, .prob_matrix,
# .dcProbArray, .dcPredictVectorized) re-parsed the same params list.
#
# The test below counts .parse_dc_params() invocations by swapping in a
# counting shim in the package namespace, running the public function, and
# asserting the count. This locks in the invariant without changing any
# public behavior.

context("Issue 47: parse_dc_params called once at the boundary")

.track_parse_count <- function(code) {
  ns <- asNamespace("HockeyModel")
  orig_parse <- get0(".parse_dc_params", envir = ns)

  counter <- new.env(parent = ns)
  counter$n <- 0L

  counting_parse <- function(params = NULL, defaults = NULL) {
    counter$n <- counter$n + 1L
    orig_parse(params = params, defaults = defaults)
  }

  assignInNamespace(
    ".parse_dc_params",
    counting_parse,
    ns = "HockeyModel"
  )
  on.exit(
    assignInNamespace(
      ".parse_dc_params",
      orig_parse,
      ns = "HockeyModel"
    ),
    add = TRUE
  )

  counter$n <- 0L
  out <- code
  list(result = out, calls = counter$n)
}

test_that("DCPredict parses params exactly once per call", {
  set.seed(1)
  tracked <- .track_parse_count({
    DCPredict(home = "Toronto Maple Leafs", away = "Ottawa Senators")
  })

  expect_length(tracked$result, 3)
  expect_equal(tracked$calls, 1L)
})

test_that("dcSample parses params exactly once per call", {
  set.seed(1)
  tracked <- .track_parse_count({
    dcSample(home = "Toronto Maple Leafs", away = "Ottawa Senators")
  })

  expect_true(tracked$result %in% c(0, 0.25, 0.4, 0.6, 0.75, 1))
  expect_equal(tracked$calls, 1L)
})

test_that(".dcPredictVectorized parses params exactly once per call", {
  home <- c("Toronto Maple Leafs", "Ottawa Senators", "New Jersey Devils")
  away <- c("Ottawa Senators", "Toronto Maple Leafs", "Philadelphia Flyers")
  tracked <- .track_parse_count({
    .dcPredictVectorized(home = home, away = away, draws = TRUE)
  })

  expect_equal(nrow(tracked$result), 3)
  expect_equal(tracked$calls, 1L)
})

test_that("playoffWin parses params exactly once per call", {
  tracked <- .track_parse_count({
    playoffWin(
      home_team = "Toronto Maple Leafs",
      away_team = "Ottawa Senators"
    )
  })

  expect_true(tracked$result > 0 && tracked$result < 1)
  expect_equal(tracked$calls, 1L)
})

test_that("randomSeriesWinner parses params exactly once per call", {
  set.seed(42)
  tracked <- .track_parse_count({
    randomSeriesWinner(
      home_team = "Toronto Maple Leafs",
      away_team = "Ottawa Senators"
    )
  })

  expect_true(
    tracked$result %in% c("Toronto Maple Leafs", "Ottawa Senators")
  )
  expect_equal(tracked$calls, 1L)
})
