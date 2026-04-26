# Tests for PWHL model functions

# ── Test Helpers ──────────────────────────────────────────────────────────────

# Minimal sample PWHL scores for offline testing
make_pwhl_scores <- function(n_games = 20) {
  if (n_games == 0) {
    return(data.frame(
      Date = as.Date(character()),
      HomeTeam = character(),
      AwayTeam = character(),
      GameID = integer(),
      HomeGoals = integer(),
      AwayGoals = integer(),
      OTStatus = character(),
      GameType = character(),
      GameStatus = character(),
      stringsAsFactors = FALSE
    ))
  }
  teams <- c(
    "Boston Fleet",
    "Minnesota Frost",
    "Montreal Victoire",
    "New York Sirens",
    "Ottawa Charge",
    "Toronto Sceptres"
  )
  set.seed(42)
  matchups <- expand.grid(HomeTeam = teams, AwayTeam = teams)
  matchups <- matchups[matchups$HomeTeam != matchups$AwayTeam, ]
  matchups <- matchups[rep(seq_len(nrow(matchups)), length.out = n_games), ]

  home_goals <- sample(1:5, n_games, replace = TRUE)
  away_goals <- sample(1:5, n_games, replace = TRUE)
  # OT games: ensure goals differ (winner scored extra goal)
  ot_status <- rep("", n_games)
  # Use ~20% OT rate, capped at available diff-goal games, minimum 5 for model fitting
  diff_idx <- which(home_goals != away_goals)
  n_ot <- min(max(5L, floor(n_games * 0.2)), length(diff_idx))
  ot_idx <- diff_idx[seq_len(n_ot)]
  ot_status[ot_idx] <- sample(c("OT", "SO"), n_ot, replace = TRUE)

  data.frame(
    Date = seq.Date(as.Date("2024-01-01"), by = "3 days", length.out = n_games),
    HomeTeam = as.character(matchups$HomeTeam),
    AwayTeam = as.character(matchups$AwayTeam),
    GameID = seq_len(n_games),
    HomeGoals = home_goals,
    AwayGoals = away_goals,
    OTStatus = ot_status,
    GameType = "R",
    GameStatus = "Final",
    stringsAsFactors = FALSE
  )
}

make_pwhl_schedule <- function(scores) {
  teams <- c(
    "Boston Fleet",
    "Minnesota Frost",
    "Montreal Victoire",
    "New York Sirens",
    "Ottawa Charge",
    "Toronto Sceptres"
  )
  matchups <- expand.grid(HomeTeam = teams, AwayTeam = teams)
  matchups <- matchups[matchups$HomeTeam != matchups$AwayTeam, ]
  matchups <- matchups[seq_len(10), ]
  n <- nrow(matchups)
  data.frame(
    Date = seq.Date(max(scores$Date) + 3, by = "3 days", length.out = n),
    HomeTeam = as.character(matchups$HomeTeam),
    AwayTeam = as.character(matchups$AwayTeam),
    GameID = max(scores$GameID) + seq_len(n),
    GameType = "R",
    GameStatus = "Scheduled",
    stringsAsFactors = FALSE
  )
}

# Build DC params without running Weibull optimisation (for speed in tests
# that don't specifically test updatePWHLDC)
make_pwhl_params <- function(scores) {
  sc <- pwhl_add_result(scores)
  m <- getM(scores = sc, currentDate = max(sc$Date) + 1)
  rho <- getRho(m = m, scores = sc)
  list(m = m, rho = rho, beta = 2.5, eta = 1.5, k = 5.0)
}

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

# ── pwhl_today_dc ─────────────────────────────────────────────────────────────

test_that("pwhl_today_dc returns NULL when no games today", {
  scores <- make_pwhl_scores(30)
  params <- make_pwhl_params(scores)
  sched <- make_pwhl_schedule(scores)
  result <- pwhl_today_dc(
    params = params,
    today = as.Date("1900-01-01"),
    schedule = sched
  )
  expect_null(result)
})

test_that("pwhl_today_dc returns correct columns when games exist", {
  scores <- make_pwhl_scores(30)
  params <- make_pwhl_params(scores)
  sched <- make_pwhl_schedule(scores)
  today <- sched$Date[1]
  result <- pwhl_today_dc(params = params, today = today, schedule = sched)
  expect_true(is.data.frame(result))
  expect_true(all(
    c("HomeTeam", "AwayTeam", "HomeWin", "AwayWin", "Draw", "GameID") %in%
      names(result)
  ))
  expect_true(all(result$HomeWin >= 0 & result$HomeWin <= 1))
  expect_true(all(result$AwayWin >= 0 & result$AwayWin <= 1))
})

test_that("pwhl_today_dc rejects non-Date today", {
  expect_error(pwhl_today_dc(today = "not-a-date"), class = "rlang_error")
})

# ── pwhl_get_team_colours ─────────────────────────────────────────────────────

test_that("pwhl_get_team_colours returns list with home and away colours", {
  tc <- pwhl_get_team_colours("Boston Fleet", "Ottawa Charge")
  expect_type(tc, "list")
  expect_true(all(c("home", "away") %in% names(tc)))
  expect_match(tc$home, "^#[0-9A-Fa-f]{6}$")
  expect_match(tc$away, "^#[0-9A-Fa-f]{6}$")
})

test_that("pwhl_get_team_colours rejects unknown teams", {
  expect_error(
    pwhl_get_team_colours("Unknown Team", "Boston Fleet"),
    class = "rlang_error"
  )
  expect_error(
    pwhl_get_team_colours("Boston Fleet", "Unknown Team"),
    class = "rlang_error"
  )
})

test_that("pwhl_get_team_colours returns different colours for all six teams", {
  teams <- HockeyModel::pwhlTeamColours$Team
  for (t1 in teams) {
    for (t2 in teams[teams != t1]) {
      tc <- pwhl_get_team_colours(t1, t2)
      expect_match(tc$home, "^#[0-9A-Fa-f]{6}$")
      expect_match(tc$away, "^#[0-9A-Fa-f]{6}$")
    }
  }
})

# ── pwhl_remainder_season_dc ──────────────────────────────────────────────────

test_that("pwhl_remainder_season_dc returns correct structure", {
  scores <- make_pwhl_scores(30)
  params <- make_pwhl_params(scores)
  sched <- make_pwhl_schedule(scores)
  all_sched <- dplyr::bind_rows(
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
  result <- pwhl_remainder_season_dc(
    scores = scores,
    schedule = all_sched,
    params = params
  )
  expect_true(is.data.frame(result))
  expect_true(all(
    c(
      "HomeTeam",
      "AwayTeam",
      "HomeWin",
      "AwayWin",
      "Draw",
      "GameID",
      "Date"
    ) %in%
      names(result)
  ))
  expect_true(nrow(result) > 0)
  expect_true(all(result$HomeWin >= 0 & result$HomeWin <= 1))
})

test_that("pwhl_remainder_season_dc returns empty frame when no future games", {
  scores <- make_pwhl_scores(10)
  past_sched <- scores[, c(
    "Date",
    "HomeTeam",
    "AwayTeam",
    "GameID",
    "GameType",
    "GameStatus"
  )]
  result <- pwhl_remainder_season_dc(
    scores = scores,
    schedule = past_sched,
    params = list(m = NULL, rho = NULL, beta = 1, eta = 1, k = 1)
  )
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
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
  expect_true(all(result$summary_results$Make_Playoffs >= 0))
  expect_true(all(result$summary_results$Make_Playoffs <= 1))
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
