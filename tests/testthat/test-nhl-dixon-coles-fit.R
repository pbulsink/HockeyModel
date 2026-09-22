context("test-nhl-dixon-coles-fit")

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
  xi <- 0.005
  upsilon <- 300L

  # Compute expected weights using the sigmoid formula directly, without calling
  # DCweights().  This guards against regressions in the implementation.
  # Formula: w = 1 - 1 / (1 + exp(-xi * (datediffs - upsilon)))
  # where datediffs = (currentDate - date) in days (positive = past game).
  datediffs <- as.numeric(as.Date(current) - dates)
  w_expected <- 1 - 1 / (1 + exp(-xi * (datediffs - upsilon)))
  w_expected[datediffs <= 0] <- 0 # Future dates get weight 0

  w_new <- DCweights(
    dates,
    currentDate = current,
    xi = xi,
    upsilon = upsilon,
    nu = 0
  )
  expect_equal(w_new, w_expected)
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
