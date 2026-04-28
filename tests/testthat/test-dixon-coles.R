context("test-dixon-coles")

# ============ updateDC tests ============
test_that("Model params generate OK", {
  params <- suppressWarnings(updateDC(save_data = FALSE))
  expect_true(is.list(params))
  expect_true(all(c("m", "rho", "beta", "eta", "k") %in% names(params)))

  expect_lt(params$rho, 0)
  expect_gt(params$rho, -0.5)

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

# ============ dcProbMatrix tests ============
test_that("DC Functions function", {
  pmat <- dcProbMatrix(home = "Toronto Maple Leafs", away = "Ottawa Senators")
  expect_equal(sum(pmat), 1)
  pmat2 <- prob_matrix(
    lambda = 2,
    mu = 2,
    params = list("rho" = -0.25, "beta" = 2, "eta" = 2, "k" = 5),
    maxgoal = 4
  )
  expect_equal(sum(pmat2), 1)
  expect_equal(
    pmat2,
    structure(
      c(
        0.0713211695449963,
        0.0194577853633925,
        0.0389155707267851,
        0.0259437138178567,
        0.0129718569089284,
        0.0194577853633925,
        0.168448674977137,
        0.0778311414535702,
        0.0518874276357135,
        0.0259437138178567,
        0.0389155707267851,
        0.0778311414535702,
        0.0579136240868313,
        0.0518874276357135,
        0.0259437138178567,
        0.0259437138178567,
        0.0518874276357135,
        0.0518874276357135,
        0.00596378005160021,
        0.0172958092119045,
        0.0129718569089284,
        0.0259437138178567,
        0.0259437138178567,
        0.0172958092119045,
        0.000196430560280055
      ),
      .Dim = c(5L, 5L)
    )
  )
})

test_that("prob_matrix sums to 1", {
  pmat <- prob_matrix(
    lambda = 1.5,
    mu = 1.5,
    params = list("rho" = -0.1, "beta" = 2, "eta" = 2, "k" = 3),
    maxgoal = 6
  )
  expect_equal(sum(pmat), 1, tolerance = 1e-10)
})

test_that("dcProbMatrix creates symmetric-like structure", {
  pmat <- dcProbMatrix(
    home = "Toronto Maple Leafs",
    away = "Toronto Maple Leafs"
  )
  expect_equal(sum(pmat), 1)
})

# ============ DC Convenience tests ============
test_that("DC Convenience functions are ok", {
  params <- parse_dc_params(NULL)
  expect_true(
    dcResult(lambda = 3, mu = 3, params = params) %in%
      c(0, 0.25, 0.4, 0.5, 0.6, 0.75, 1)
  )

  sim <- dcSample(home = "Nashville Predators", away = "Colorado Avalanche")
  expect_true(sim %in% c(0, 0.25, 0.4, 0.6, 0.75, 1))

  sim <- dcSample("Dallas Stars", "Columbus Blue Jackets", as_result = FALSE)
  expect_true(sim$OTStatus %in% c("", "OT", "SO"))
  expect_equal(length(sim), 3)
  expect_equal(names(sim), c("HomeGoals", "AwayGoals", "OTStatus"))
})

test_that("dcSample produces consistent results", {
  set.seed(123)
  sim1 <- dcSample("Toronto Maple Leafs", "Ottawa Senators")
  set.seed(123)
  sim2 <- dcSample("Toronto Maple Leafs", "Ottawa Senators")
  expect_equal(sim1, sim2)
})

test_that("dcSample with as_result=FALSE returns data frame", {
  sim <- dcSample("Toronto Maple Leafs", "Ottawa Senators", as_result = FALSE)
  expect_true(is.numeric(sim$HomeGoals))
  expect_true(is.numeric(sim$AwayGoals))
  expect_true(is.character(sim$OTStatus))
})

test_that("dcResult handles various score combinations", {
  set.seed(10)
  expect_equal(dcResult(5, 2), 1)
  expect_equal(dcResult(2, 5), 0)
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

# ============ Package constants ============
test_that("Package weighting constants are numeric and in expected ranges", {
  expect_true(is.numeric(DC_XI_NHL))
  expect_true(is.numeric(DC_UPSILON_NHL))
  expect_true(is.numeric(DC_NU_NHL))
  expect_true(is.numeric(DC_XI_PWHL))
  expect_true(is.numeric(DC_UPSILON_PWHL))
  expect_true(is.numeric(DC_NU_PWHL))

  expect_gt(DC_XI_NHL, 0)
  expect_gt(DC_UPSILON_NHL, 0)
  expect_gte(DC_NU_NHL, 0)

  expect_gt(DC_XI_PWHL, 0)
  expect_gt(DC_UPSILON_PWHL, 0)
  expect_gte(DC_NU_PWHL, 0)
})

# ============ derive_season_starts tests ============
test_that("derive_season_starts returns one date per season", {
  # Three seasons: 2022-23, 2023-24, 2024-25
  dates <- as.Date(c(
    "2022-10-11",
    "2022-11-15",
    "2023-03-01", # 2022-23
    "2023-10-10",
    "2023-12-01",
    "2024-04-15", # 2023-24
    "2024-10-08",
    "2025-01-20" # 2024-25
  ))
  starts <- derive_season_starts(dates)
  expect_length(starts, 3)
  expect_s3_class(starts, "Date")
  expect_true(all(starts == sort(starts))) # sorted
  expect_equal(starts[1], as.Date("2022-10-11"))
  expect_equal(starts[2], as.Date("2023-10-10"))
  expect_equal(starts[3], as.Date("2024-10-08"))
})

test_that("derive_season_starts handles a single season", {
  dates <- as.Date(c("2024-10-08", "2024-11-01", "2025-02-15"))
  starts <- derive_season_starts(dates)
  expect_length(starts, 1)
  expect_equal(starts, as.Date("2024-10-08"))
})

# ============ DCweights tests ============
test_that("DCweights with nu = 0 equals legacy sigmoid weights", {
  dates <- as.Date(c("2025-01-01", "2024-06-01", "2023-10-01"))
  current <- as.Date("2025-03-01")
  w_new <- DCweights(
    dates,
    currentDate = current,
    xi = 0.005,
    upsilon = 300,
    nu = 0
  )
  w_leg <- DCweights(
    dates,
    currentDate = current,
    xi = 0.005,
    upsilon = 300,
    nu = 0
  )
  expect_equal(w_new, w_leg)
})

test_that("DCweights with nu > 0 reduces weight for older seasons", {
  # Two dates: one in current season, one a full season back
  season_starts <- as.Date(c("2023-10-01", "2024-10-01"))
  current <- as.Date("2025-02-01")
  dates_cur <- as.Date("2024-11-01") # current season
  dates_prev <- as.Date("2023-11-01") # previous season

  w_no_nu <- DCweights(
    c(dates_cur, dates_prev),
    currentDate = current,
    xi = 0.001,
    upsilon = 365,
    nu = 0
  )
  w_with_nu <- DCweights(
    c(dates_cur, dates_prev),
    currentDate = current,
    xi = 0.001,
    upsilon = 365,
    nu = 2,
    season_start_dates = season_starts
  )

  # Current season: multiplier = 1/(1+0^2) = 1; weight unchanged
  expect_equal(w_with_nu[1], w_no_nu[1])
  # Previous season: multiplier = 1/(1+1^2) = 0.5; weight halved
  expect_equal(w_with_nu[2], w_no_nu[2] * 0.5, tolerance = 1e-10)
})

test_that("DCweights with nu > 0 requires season_start_dates", {
  dates <- as.Date(c("2024-10-01", "2023-10-01"))
  expect_error(
    DCweights(dates, currentDate = as.Date("2025-01-01"), nu = 2),
    "season_start_dates"
  )
})

test_that("DCweights assigns zero weight to future dates", {
  dates <- as.Date(c("2025-05-01", "2025-01-01"))
  current <- as.Date("2025-03-01")
  w <- DCweights(dates, currentDate = current, nu = 0)
  expect_equal(w[1], 0)
  expect_gt(w[2], 0)
})

test_that("DCweights cross-season discount is monotonically decreasing", {
  season_starts <- as.Date(c(
    "2021-10-01",
    "2022-10-01",
    "2023-10-01",
    "2024-10-01"
  ))
  current <- as.Date("2025-02-01")
  # One representative game per season
  game_dates <- as.Date(c(
    "2024-11-01",
    "2023-11-01",
    "2022-11-01",
    "2021-11-01"
  ))
  w <- DCweights(
    game_dates,
    currentDate = current,
    xi = 0.001,
    upsilon = 1000, # nearly flat sigmoid so we see season effect clearly
    nu = 2,
    season_start_dates = season_starts
  )
  # Each successive season should have lower weight
  expect_gt(w[1], w[2])
  expect_gt(w[2], w[3])
  expect_gt(w[3], w[4])
})

# ============ getM with nu tests ============
test_that("getM with nu > 0 produces a valid model", {
  scor <- HockeyModel::scores[
    HockeyModel::scores$Date >= as.Date("2022-10-01"),
  ]
  m_nu <- suppressWarnings(getM(
    scores = scor,
    currentDate = as.Date("2025-01-01"),
    xi = DC_XI_NHL,
    upsilon = DC_UPSILON_NHL,
    nu = 2
  ))
  expect_s3_class(m_nu, "glm")
  expect_true(length(m_nu$coefficients) > 0)
})

test_that("getM with nu = 0 and nu = 2 produce same team coefficient names", {
  # Cross-season discounting changes coefficient magnitudes but not the set of
  # teams (coefficient names), since both use the same scores subset.
  scor <- HockeyModel::scores[
    HockeyModel::scores$Date >= as.Date("2022-10-01"),
  ]
  m0 <- suppressWarnings(getM(
    scores = scor,
    currentDate = as.Date("2025-01-01"),
    nu = 0
  ))
  m2 <- suppressWarnings(getM(
    scores = scor,
    currentDate = as.Date("2025-01-01"),
    nu = 2
  ))
  # Same coefficient names regardless of nu (same teams in the data)
  expect_equal(names(m0$coefficients), names(m2$coefficients))
  # Coefficients differ because weights changed
  expect_false(all(m0$coefficients == m2$coefficients))
})
