context("test-nhl-dixon-coles-workflow")

test_that("Model params generate OK", {
  params <- suppressWarnings(updateDC(save_data = FALSE))
  expect_true(is.list(params))
  expect_true(all(c("m", "rho", "beta", "eta", "k") %in% names(params)))

  # rho is in [-0.5, 0.5] per Dixon-Coles bounds (goalmodel convention)
  expect_gte(params$rho, -0.5)
  expect_lte(params$rho, 0.5)

  expect_lt(params$beta, 10)
  expect_gt(params$beta, 1)
  expect_lt(params$eta, 10)
  expect_gt(params$eta, 1)
  expect_lt(params$k, 10)
  expect_gt(params$k, 1)
})

test_that("updateDC with historical date works", {
  params <- suppressWarnings(updateDC(
    currentDate = as.Date("2019-01-01"),
    save_data = FALSE
  ))
  expect_true(is.list(params))
  expect_true(all(c("m", "rho", "beta", "eta", "k") %in% names(params)))
})

test_that("remainderSeasonDC returns odds table directly", {
  sched <- HockeyModel::schedule[
    HockeyModel::schedule$Date >= as.Date("2025-10-07") &
      HockeyModel::schedule$Date <= as.Date("2025-10-10"),
  ]
  scor <- HockeyModel::scores[
    HockeyModel::scores$Date < as.Date("2025-10-07"),
  ]

  local_mocked_bindings(
    todayDC = function(today, schedule, ...) {
      day_games <- schedule[schedule$Date == as.Date(today), ]
      data.frame(
        HomeTeam = day_games$HomeTeam,
        AwayTeam = day_games$AwayTeam,
        HomeWin = 0.5,
        AwayWin = 0.3,
        Draw = 0.2,
        GameID = day_games$GameID
      )
    },
    .package = "HockeyModel"
  )

  result <- remainderSeasonDC(
    nsims = 3,
    cores = 1,
    scores = scor,
    schedule = sched,
    odds = TRUE,
    regress = FALSE
  )
  expect_s3_class(result, "data.frame")
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
})

test_that("loopless_sim returns summary and raw results", {
  sched <- HockeyModel::schedule[
    HockeyModel::schedule$Date >= as.Date("2025-10-07") &
      HockeyModel::schedule$Date <= as.Date("2025-10-10"),
  ]
  scor <- HockeyModel::scores[
    HockeyModel::scores$Date < as.Date("2025-10-07"),
  ]
  odds_table <- sched[, c("Date", "HomeTeam", "AwayTeam", "GameID")]
  odds_table$HomeWin <- 0.5
  odds_table$AwayWin <- 0.3
  odds_table$Draw <- 0.2
  odds_table <- odds_table[, c(
    "HomeTeam",
    "AwayTeam",
    "HomeWin",
    "AwayWin",
    "Draw",
    "GameID",
    "Date"
  )]

  local_mocked_bindings(
    getSeason = function(gamedate = Sys.Date()) "20192020",
    getSeasonStartDate = function(season = NULL) as.Date("2025-10-07"),
    sim_engine = function(all_season, nsims, params = NULL) {
      teams <- sort(unique(c(all_season$HomeTeam, all_season$AwayTeam)))
      data.frame(
        SimNo = rep(1, length(teams)),
        Team = teams,
        W = rep(1, length(teams)),
        OTW = rep(0, length(teams)),
        SOW = rep(0, length(teams)),
        SOL = rep(0, length(teams)),
        OTL = rep(0, length(teams)),
        Points = rep(2, length(teams)),
        Wildcard = rep(0, length(teams)),
        Rank = seq_along(teams),
        ConfRank = seq_along(teams),
        DivRank = rep(1, length(teams)),
        Playoffs = rep(1, length(teams))
      )
    },
    .package = "HockeyModel"
  )

  result <- loopless_sim(
    nsims = 4,
    cores = 1,
    schedule = sched,
    scores = scor,
    odds_table = odds_table,
    likelihood_graphic = FALSE
  )

  expect_true(is.list(result))
  expect_true(all(c("summary_results", "raw_results") %in% names(result)))
  expect_s3_class(result$summary_results, "tbl_df")
  expect_s3_class(result$raw_results, "data.frame")
})

# ============ DC Playoffs tests ============
test_that("DC Playoffs functions", {
  po_odds <- playoffDC("Toronto Maple Leafs", "Carolina Hurricanes")
  expect_lt(po_odds, 1)
  expect_gt(po_odds, 0)
})

test_that("playoffDC returns numeric probability", {
  result <- playoffDC("Toronto Maple Leafs", "Carolina Hurricanes")
  expect_true(is.numeric(result))
  expect_equal(length(result), 1)
})

test_that("playoffDC same team near 0.5", {
  result <- playoffDC("Toronto Maple Leafs", "Toronto Maple Leafs")
  expect_true(abs(result - 0.5) < 0.1)
})

# ============ todayDC tests ============
test_that("DC Today returns data or NULL", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)

  today_odds <- todayDC(today = as.Date("2019-11-01"))
  expect_true(is.null(today_odds) || is.data.frame(today_odds))

  if (!is.null(today_odds)) {
    expect_true("HomeTeam" %in% colnames(today_odds))
    expect_true("AwayTeam" %in% colnames(today_odds))
  }
})

test_that("todayDC returns NULL for no games", {
  result <- todayDC(today = as.Date("2020-07-15"))
  expect_null(result)
})

test_that("todayDC odds sum to 1 when available", {
  sched <- HockeyModel::scores
  sched <- sched[sched$Date > as.Date("2019-10-01"), ]
  sched <- sched[sched$Date < as.Date("2019-12-31"), ]
  today_odds <- todayDC(today = as.Date("2019-11-01"), schedule = sched)
  if (!is.null(today_odds) && nrow(today_odds) > 0) {
    for (i in seq_len(nrow(today_odds))) {
      expect_equal(
        today_odds$HomeWin[i] + today_odds$AwayWin[i] + today_odds$Draw[i],
        1,
        tolerance = 1e-10
      )
    }
  }
})

# ── todayDC (PWHL) ────────────────────────────────────────────────────────────

test_that("todayDC returns NULL when no PWHL games today", {
  scores <- make_pwhl_scores(30)
  params <- make_pwhl_params(scores)
  sched <- make_pwhl_schedule(scores)
  result <- todayDC(
    params = params,
    today = as.Date("1900-01-01"),
    schedule = sched,
    draws = TRUE
  )
  expect_null(result)
})

test_that("todayDC returns correct columns when PWHL games exist", {
  scores <- make_pwhl_scores(30)
  params <- make_pwhl_params(scores)
  sched <- make_pwhl_schedule(scores)
  today <- sched$Date[1]
  result <- todayDC(
    params = params,
    today = today,
    schedule = sched,
    draws = TRUE
  )
  expect_true(is.data.frame(result))
  expect_true(all(
    c("HomeTeam", "AwayTeam", "HomeWin", "AwayWin", "Draw", "GameID") %in%
      names(result)
  ))
  expect_true(all(result$HomeWin >= 0 & result$HomeWin <= 1))
  expect_true(all(result$AwayWin >= 0 & result$AwayWin <= 1))
})

test_that("todayDC rejects non-Date today", {
  expect_error(todayDC(today = "not-a-date"), class = "rlang_error")
})
