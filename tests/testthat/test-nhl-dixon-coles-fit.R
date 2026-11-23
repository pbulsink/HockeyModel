context("test-nhl-dixon-coles-fit")

# ============ #103: model-collapse regression tests ============
test_that("getWeibullParams reaches the interior least-squares minimum (#103)", {
  # The 1e-6 lower floor + upper cap added in f558a41d flattened the gradient
  # and pinned beta/eta to the corner (SSE far above the attainable minimum).
  # With the restored open bounds, L-BFGS-B must land off its lower bounds and
  # near the historically-observed hockey values (beta ~ 3, eta ~ 4, k ~ 6).
  wp <- suppressWarnings(getWeibullParams(
    m = HockeyModel::m,
    rho = HockeyModel::rho
  ))
  expect_gt(wp$beta, 0.5)
  expect_gt(wp$eta, 0.5)
  expect_lt(wp$beta, 20)
  expect_lt(wp$eta, 20)
})

test_that("getWeibullParams warns when the fit collapses to the boundary", {
  # A fit pinned to the Weibull domain boundary (beta/eta ~ 0) zeroes the
  # score-matrix diagonal and silently drains wins/points from simulations
  # downstream (#103), so it must warn. Mock optim to return such a fit.
  testthat::local_mocked_bindings(
    optim = function(...) list(par = c(1e-6, 1e-6, -2)),
    .package = "stats"
  )
  expect_warning(
    getWeibullParams(m = HockeyModel::m, rho = HockeyModel::rho),
    "collapsed to the parameter boundary"
  )
})

test_that("getWeibullParams does not warn for a healthy fit", {
  testthat::local_mocked_bindings(
    optim = function(...) list(par = c(3.04, 4.29, 6.68)),
    .package = "stats"
  )
  expect_no_warning(
    getWeibullParams(m = HockeyModel::m, rho = HockeyModel::rho)
  )
})

test_that("getRho returns a negative, off-boundary estimate (#103)", {
  # On regulation-time scores the Dixon-Coles low-score dependence is negative
  # for hockey (observed ~ -0.15 to -0.28). It must not pin to the +0.5 bound.
  rho <- suppressWarnings(getRho(m = HockeyModel::m))
  expect_lt(rho, 0)
  expect_gt(rho, -0.5)
})

test_that(".regulation_scores reconstructs ties from OT/SO final scores", {
  scor <- data.frame(
    Date = as.Date(c("2024-10-10", "2024-10-11", "2024-10-12", "2024-10-13")),
    GameID = c("G1", "G2", "G3", "G4"),
    HomeTeam = c("A", "B", "C", "D"),
    AwayTeam = c("E", "F", "G", "H"),
    HomeGoals = c(3, 2, 4, 1),
    AwayGoals = c(1, 3, 3, 2),
    Result = c(1, 0.25, 0.75, 0) # reg home win, away OT win, home OT win, reg away win
  )
  reg <- .regulation_scores(scor)
  # Regulation win unchanged
  expect_equal(reg$HomeGoals[1], 3)
  expect_equal(reg$AwayGoals[1], 1)
  # Away OT win (0.25): away scored the OT winner -> regulation 2-2 tie
  expect_equal(reg$HomeGoals[2], 2)
  expect_equal(reg$AwayGoals[2], 2)
  # Home OT win (0.75): home scored the OT winner -> regulation 3-3 tie
  expect_equal(reg$HomeGoals[3], 3)
  expect_equal(reg$AwayGoals[3], 3)
  # Ties now exist where OT/SO games were decided
  expect_true(any(reg$HomeGoals == reg$AwayGoals))
})

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

# ============ .derive_season_starts tests ============
test_that(".derive_season_starts returns one date per season", {
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
  starts <- .derive_season_starts(dates)
  expect_length(starts, 3)
  expect_s3_class(starts, "Date")
  expect_true(all(starts == sort(starts))) # sorted
  expect_equal(starts[1], as.Date("2022-10-11"))
  expect_equal(starts[2], as.Date("2023-10-10"))
  expect_equal(starts[3], as.Date("2024-10-08"))
})

test_that(".derive_season_starts handles a single season", {
  dates <- as.Date(c("2024-10-08", "2024-11-01", "2025-02-15"))
  starts <- .derive_season_starts(dates)
  expect_length(starts, 1)
  expect_equal(starts, as.Date("2024-10-08"))
})

# ============ .DCweights tests ============
test_that(".DCweights with nu = 0 equals legacy sigmoid weights", {
  dates <- as.Date(c("2025-01-01", "2024-06-01", "2023-10-01"))
  current <- as.Date("2025-03-01")
  xi <- 0.005
  upsilon <- 300L

  # Compute expected weights using the sigmoid formula directly, without calling
  # .DCweights().  This guards against regressions in the implementation.
  # Formula: w = 1 - 1 / (1 + exp(-xi * (datediffs - upsilon)))
  # where datediffs = (currentDate - date) in days (positive = past game).
  datediffs <- as.numeric(as.Date(current) - dates)
  w_expected <- 1 - 1 / (1 + exp(-xi * (datediffs - upsilon)))
  w_expected[datediffs <= 0] <- 0 # Future dates get weight 0

  w_new <- .DCweights(
    dates,
    currentDate = current,
    xi = xi,
    upsilon = upsilon,
    nu = 0
  )
  expect_equal(w_new, w_expected)
})

test_that(".DCweights with nu > 0 reduces weight for older seasons", {
  # Two dates: one in current season, one a full season back
  season_starts <- as.Date(c("2023-10-01", "2024-10-01"))
  current <- as.Date("2025-02-01")
  dates_cur <- as.Date("2024-11-01") # current season
  dates_prev <- as.Date("2023-11-01") # previous season

  w_no_nu <- .DCweights(
    c(dates_cur, dates_prev),
    currentDate = current,
    xi = 0.001,
    upsilon = 365,
    nu = 0
  )
  w_with_nu <- .DCweights(
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

test_that(".DCweights with nu > 0 requires season_start_dates", {
  dates <- as.Date(c("2024-10-01", "2023-10-01"))
  expect_error(
    .DCweights(dates, currentDate = as.Date("2025-01-01"), nu = 2),
    "season_start_dates"
  )
})

test_that(".DCweights assigns zero weight to future dates", {
  dates <- as.Date(c("2025-05-01", "2025-01-01"))
  current <- as.Date("2025-03-01")
  w <- .DCweights(dates, currentDate = current, nu = 0)
  expect_equal(w[1], 0)
  expect_gt(w[2], 0)
})

test_that(".DCweights cross-season discount is monotonically decreasing", {
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
  w <- .DCweights(
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

# ============ getRho expectation alignment tests ============
test_that("getRho scores home goals against the home expected-goals rate", {
  # Regression test for #69: .DCRhoLogLik() contracts `lambda` as the HOME
  # expected-goals rate (it evaluates dpois(y1 = HomeGoals, lambda)), so
  # getRho() must pass lambda = home expectation and mu = away expectation.
  set.seed(17)
  teams <- c("Alpha", "Beta", "Gamma")
  n <- 300L
  home <- sample(teams, n, replace = TRUE)
  away <- vapply(
    seq_len(n),
    function(i) {
      setdiff(teams, home[i])[sample.int(length(setdiff(teams, home[i])), 1)]
    },
    character(1)
  )
  # Asymmetric scores so the two alignments give measurably different rho.
  scor <- data.frame(
    Date = as.Date("2024-10-01") + sample(60:200, n, replace = TRUE),
    GameID = paste0("G", seq_len(n)),
    HomeTeam = home,
    AwayTeam = away,
    HomeGoals = rpois(n, 3.2),
    AwayGoals = rpois(n, 2.5)
  )

  m <- suppressWarnings(
    getM(scores = scor, currentDate = as.Date("2025-03-15"), nu = 0)
  )
  n_games <- nrow(scor)
  expected <- stats::fitted(m)
  home_exp <- as.vector(expected[seq_len(n_games)])
  away_exp <- as.vector(expected[(n_games + 1):(n_games * 2)])
  weights <- m$data$Weight[seq_len(n_games)]

  fit_rho <- function(lambda, mu) {
    stats::optim(
      0,
      function(rho) {
        -sum(.DCRhoLogLik(
          y1 = scor$HomeGoals,
          y2 = scor$AwayGoals,
          lambda = lambda,
          mu = mu,
          rho = rho,
          weights = weights
        ))
      },
      method = "Brent",
      lower = -0.5,
      upper = 0.5
    )$par
  }

  rho_correct <- fit_rho(home_exp, away_exp) # contract: lambda is home
  rho_swapped <- fit_rho(away_exp, home_exp) # the bug in #69

  expect_equal(getRho(m = m, scores = scor), rho_correct, tolerance = 1e-4)
  # The swapped alignment must be distinguishable from the correct one.
  expect_gt(abs(rho_correct - rho_swapped), 1e-3)
})

test_that("getM starting values match model rank", {
  # Team and Opponent are constructed from the same home/away union, so their
  # factor levels are always identical and the design matrix for
  # `Team + Opponent + Home + 0` is always full rank with exactly
  # `2 * n_teams` columns, matching the length of the `start` vector, even
  # when a team appears only as a home team or only as an away team.
  build_scores_with_lopsided_team <- function(
    mode = c("home_only", "away_only")
  ) {
    mode <- match.arg(mode)
    set.seed(909)
    teams <- LETTERS[1:6]
    n <- 40
    home <- sample(teams, n, replace = TRUE)
    away <- vapply(
      home,
      function(h) sample(setdiff(teams, h), 1),
      character(1)
    )
    if (mode == "home_only") {
      home[1] <- "F"
      away[1] <- "A"
    } else {
      home[1] <- "A"
      away[1] <- "F"
    }
    dates <- Sys.Date() - rev(seq_len(n))
    data.frame(
      Date = dates,
      GameID = seq_len(n),
      HomeTeam = home,
      AwayTeam = away,
      HomeGoals = rpois(n, 3),
      AwayGoals = rpois(n, 3)
    )
  }

  for (mode in c("home_only", "away_only")) {
    scor <- build_scores_with_lopsided_team(mode)
    m <- suppressWarnings(getM(
      scores = scor,
      currentDate = max(scor$Date) + 1,
      nu = 0
    ))

    df.indep <- data.frame(
      Team = as.factor(c(
        as.character(scor$HomeTeam),
        as.character(scor$AwayTeam)
      )),
      Opponent = as.factor(c(
        as.character(scor$AwayTeam),
        as.character(scor$HomeTeam)
      ))
    )
    expected_length <- length(unique(df.indep$Team)) * 2

    expect_length(m$coefficients, expected_length)
    expect_false(anyNA(m$coefficients), info = paste("mode:", mode))
  }
})
