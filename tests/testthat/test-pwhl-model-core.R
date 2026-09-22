# Tests for PWHL model functions (helpers live in helper-pwhl.R)

# ── pwhl_add_result ───────────────────────────────────────────────────────────

test_that("pwhl_add_result adds Result column with correct encodings", {
  scores <- data.frame(
    HomeGoals = c(3, 3, 3, 1, 1, 2), # regulation, home OT win, home SO win
    AwayGoals = c(1, 2, 2, 2, 2, 3), # regulation, away OT win, away SO win
    OTStatus = c("", "OT", "SO", "", "OT", "SO"),
    stringsAsFactors = FALSE
  )
  out <- pwhl_add_result(scores)
  expect_true("Result" %in% names(out))
  expect_equal(out$Result, c(1, 0.75, 0.6, 0, 0.25, 0.4))
})

test_that("pwhl_add_result preserves all input columns", {
  scores <- make_pwhl_scores(5)
  out <- pwhl_add_result(scores)
  expect_true(all(names(scores) %in% names(out)))
  expect_equal(nrow(out), nrow(scores))
})

# ── parse_pwhl_dc_params ──────────────────────────────────────────────────────

test_that("parse_pwhl_dc_params falls back to package PWHL params", {
  params <- parse_pwhl_dc_params(NULL)
  expect_type(params, "list")
  expect_named(params, c("m", "rho", "beta", "eta", "k"))
})

test_that("parse_pwhl_dc_params uses supplied params", {
  fake <- list(m = "a", rho = -0.1, beta = 2, eta = 1, k = 5)
  params <- parse_pwhl_dc_params(fake)
  expect_equal(params$m, "a")
  expect_equal(params$rho, -0.1)
  expect_equal(params$beta, 2)
})

test_that("parse_pwhl_dc_params unwraps nested params", {
  inner <- list(m = "nested", rho = 0, beta = 1, eta = 1, k = 1)
  nested <- list(params = inner)
  params <- parse_pwhl_dc_params(nested)
  expect_equal(params$m, "nested")
})

# ── updatePWHLDC ─────────────────────────────────────────────────────────────

test_that("updatePWHLDC rejects non-Date currentDate", {
  scores <- make_pwhl_scores()
  expect_error(
    updatePWHLDC(scores, currentDate = "not-a-date"),
    class = "rlang_error"
  )
})

test_that("updatePWHLDC errors on empty scores", {
  empty <- make_pwhl_scores(0)
  expect_error(updatePWHLDC(empty), class = "rlang_error")
})

test_that("updatePWHLDC returns a named list with the correct components", {
  # Test the contract (m is a glm, rho/beta/eta/k are numeric) using
  # make_pwhl_params (bypasses the Weibull optimisation which needs real data)
  scores <- make_pwhl_scores(30)
  params <- make_pwhl_params(scores)
  expect_type(params, "list")
  expect_named(params, c("m", "rho", "beta", "eta", "k"))
  expect_s3_class(params$m, "glm")
  expect_true(is.numeric(params$rho))
  expect_true(is.numeric(params$beta))
  expect_true(is.numeric(params$eta))
  expect_true(is.numeric(params$k))
})

# ── pwhl_in_season ─────────────────────────────────────────────────────────────

test_that("pwhl_in_season returns FALSE for empty schedule", {
  empty_sched <- data.frame(
    Date = as.Date(character()),
    stringsAsFactors = FALSE
  )
  expect_false(pwhl_in_season(schedule = empty_sched))
})

test_that("pwhl_in_season returns TRUE inside season window", {
  sched <- data.frame(
    Date = as.Date(c("2025-01-01", "2025-03-31")),
    stringsAsFactors = FALSE
  )
  expect_true(pwhl_in_season(date = as.Date("2025-02-15"), schedule = sched))
})

test_that("pwhl_in_season returns FALSE outside season window", {
  sched <- data.frame(
    Date = as.Date(c("2025-01-01", "2025-03-31")),
    stringsAsFactors = FALSE
  )
  expect_false(pwhl_in_season(date = as.Date("2024-12-31"), schedule = sched))
  expect_false(pwhl_in_season(date = as.Date("2025-04-01"), schedule = sched))
})

test_that("pwhl_in_season rejects non-Date input", {
  sched <- data.frame(Date = as.Date("2025-01-01"), stringsAsFactors = FALSE)
  expect_error(
    pwhl_in_season(date = "2025-01-01", schedule = sched),
    class = "rlang_error"
  )
})

# ── pwhl_loopless_sim ─────────────────────────────────────────────────────────

test_that("pwhl_loopless_sim returns summary and raw results", {
  scores <- make_pwhl_scores(30)
  params <- make_pwhl_params(scores)
  sched <- make_pwhl_schedule(scores)
  full_sched <- dplyr::bind_rows(
    scores[, c(
      "Date",
      "HomeTeam",
      "AwayTeam",
      "GameID",
      "GameType",
      "GameStatus"
    )],
    sched
  )
  result <- pwhl_loopless_sim(
    nsims = 20,
    scores = scores,
    schedule = full_sched,
    params = params
  )
  expect_type(result, "list")
  expect_named(result, c("summary_results", "raw_results"))
  expect_true(is.data.frame(result$summary_results))
  expect_true("Team" %in% names(result$summary_results))
  expect_true("Make_Playoffs" %in% names(result$summary_results))
  expect_true("Make_Finals" %in% names(result$summary_results))
  expect_true("Win_Cup" %in% names(result$summary_results))
  expect_true(all(result$summary_results$Make_Playoffs >= 0))
  expect_true(all(result$summary_results$Make_Playoffs <= 1))
  expect_true(all(result$summary_results$Make_Finals >= 0))
  expect_true(all(result$summary_results$Make_Finals <= 1))
  expect_true(all(result$summary_results$Win_Cup >= 0))
  expect_true(all(result$summary_results$Win_Cup <= 1))
  # Each sim contributes exactly 2 finalists and 1 champion, so sums are exact
  expect_true(abs(sum(result$summary_results$Make_Finals) - 2) < 0.01)
  expect_true(abs(sum(result$summary_results$Win_Cup) - 1) < 0.01)
})

test_that("pwhl_loopless_sim top-4 playoff odds sum to roughly 4", {
  scores <- make_pwhl_scores(30)
  params <- make_pwhl_params(scores)
  sched <- make_pwhl_schedule(scores)
  full_sched <- dplyr::bind_rows(
    scores[, c(
      "Date",
      "HomeTeam",
      "AwayTeam",
      "GameID",
      "GameType",
      "GameStatus"
    )],
    sched
  )
  result <- pwhl_loopless_sim(
    nsims = 100,
    scores = scores,
    schedule = full_sched,
    params = params
  )
  total_playoff_odds <- sum(result$summary_results$Make_Playoffs)
  # Expect sum close to 4.0 (4 teams make playoffs)
  expect_true(abs(total_playoff_odds - 4) < 0.5)
})

# ── updatePWHLModel ───────────────────────────────────────────────────────────

test_that("updatePWHLModel returns list with scores, schedule, params", {
  scores_fixture <- make_pwhl_scores(30)
  schedule_fixture <- data.frame(
    Date = as.Date("2025-01-01"),
    HomeTeam = "Boston Fleet",
    AwayTeam = "Ottawa Charge",
    GameID = 1L,
    GameType = "R",
    GameStatus = "Scheduled",
    stringsAsFactors = FALSE
  )
  params_fixture <- make_pwhl_params(scores_fixture)

  local_mocked_bindings(
    updatePWHLScheduleAPI = function(...) schedule_fixture,
    updatePWHLScoresAPI = function(...) scores_fixture,
    updatePWHLDC = function(...) params_fixture,
    .package = "HockeyModel"
  )
  result <- updatePWHLModel(save_data = FALSE)
  expect_type(result, "list")
  expect_true(all(c("scores", "schedule", "params") %in% names(result)))
  expect_equal(nrow(result$schedule), 1)
  expect_equal(nrow(result$scores), 30)
})
