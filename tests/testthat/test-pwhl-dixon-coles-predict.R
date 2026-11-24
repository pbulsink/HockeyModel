context("test-pwhl-dixon-coles-predict")

# PWHL prediction-path tests mirror test-nhl-dixon-coles-predict.R, exercising
# the shared DC functions against PWHL teams and bundled PWHL params
# (pwhl_m/pwhl_rho/pwhl_beta/pwhl_eta/pwhl_k) so both leagues' flow paths are
# covered.  The pure-math helpers (.prob_matrix with explicit lambda/mu,
# .dcResult/.dcExpandedOdds with explicit params, and the synthetic-team tests)
# are league-agnostic and already covered by the NHL test file.

.pwhl_params <- function() {
  list(
    m = HockeyModel::pwhl_m,
    rho = HockeyModel::pwhl_rho,
    beta = HockeyModel::pwhl_beta,
    eta = HockeyModel::pwhl_eta,
    k = HockeyModel::pwhl_k
  )
}

# Teams from the bundled PWHL scores so every prediction uses known teams.
.pwhl_teams <- function() {
  unique(c(HockeyModel::pwhlScores$HomeTeam, HockeyModel::pwhlScores$AwayTeam))
}

# ============ .dcProbMatrix with PWHL params ============
test_that(".dcProbMatrix returns a valid probability matrix for PWHL teams", {
  pmat <- .dcProbMatrix(
    home = "Boston Fleet",
    away = "Ottawa Charge",
    params = .pwhl_params()
  )
  expect_equal(sum(pmat), 1, tolerance = 1e-10)
  expect_true(all(pmat >= 0))
})

test_that(".dcProbMatrix same team returns a valid matrix (PWHL)", {
  pmat <- .dcProbMatrix(
    home = "Boston Fleet",
    away = "Boston Fleet",
    params = .pwhl_params()
  )
  expect_equal(sum(pmat), 1, tolerance = 1e-10)
})

# ============ DCPredict with PWHL teams and params ============
test_that("DCPredict(draws = TRUE/FALSE) produces valid, consistent results (PWHL)", {
  home <- .pwhl_teams()[1]
  away <- .pwhl_teams()[2]

  odds_with_draws <- DCPredict(
    home = home,
    away = away,
    params = .pwhl_params(),
    draws = TRUE
  )
  odds_no_draws <- DCPredict(
    home = home,
    away = away,
    params = .pwhl_params(),
    draws = FALSE
  )

  expect_length(odds_with_draws, 3)
  expect_equal(sum(odds_with_draws), 1, tolerance = 1e-10)

  expect_length(odds_no_draws, 2)
  expect_equal(sum(odds_no_draws), 1, tolerance = 1e-10)

  expect_true(all(odds_with_draws > 0))
  expect_true(all(odds_no_draws > 0))

  # Without draws the draw mass is redistributed onto both sides.
  expect_gt(odds_no_draws[1], odds_with_draws[1])
  expect_gt(odds_no_draws[2], odds_with_draws[3])
})

test_that("dcSample produces valid results for PWHL teams", {
  home <- .pwhl_teams()[1]
  away <- .pwhl_teams()[2]

  set.seed(7)
  sim <- dcSample(home = home, away = away, params = .pwhl_params())
  expect_true(sim %in% c(0, 0.25, 0.4, 0.6, 0.75, 1))

  set.seed(7)
  sim2 <- dcSample(home = home, away = away, params = .pwhl_params())
  expect_equal(sim, sim2) # deterministic under a fixed seed
})

test_that("dcSample with as_result=FALSE returns data frame (PWHL)", {
  sim <- dcSample(
    .pwhl_teams()[1],
    .pwhl_teams()[3],
    params = .pwhl_params(),
    as_result = FALSE
  )
  expect_true(is.numeric(sim$HomeGoals))
  expect_true(is.numeric(sim$AwayGoals))
  expect_true(is.character(sim$OTStatus))
})

# ============ .dcPredictVectorized matches DCPredict per-game (PWHL) ============
test_that(".dcPredictVectorized matches DCPredict per-game, draws = TRUE (PWHL)", {
  teams <- .pwhl_teams()
  home <- head(teams, 4)
  away <- c(tail(teams, -1)[1:3], teams[1])

  vec <- .dcPredictVectorized(
    home = home,
    away = away,
    params = .pwhl_params(),
    draws = TRUE
  )
  ref <- t(vapply(
    seq_along(home),
    function(i) {
      p <- DCPredict(
        home = home[i],
        away = away[i],
        params = .pwhl_params(),
        draws = TRUE
      )
      c(p[1], p[2], p[3])
    },
    numeric(3)
  ))
  expect_equal(unname(vec), unname(ref), tolerance = 1e-10)
  expect_equal(rowSums(vec), rep(1, length(home)))
})

test_that(".dcPredictVectorized matches DCPredict per-game, draws = FALSE (PWHL)", {
  teams <- .pwhl_teams()
  home <- head(teams, 3)
  away <- c(tail(teams, -1)[1:2], teams[1])

  vec <- .dcPredictVectorized(
    home = home,
    away = away,
    params = .pwhl_params(),
    draws = FALSE
  )
  ref <- t(vapply(
    seq_along(home),
    function(i) {
      p <- DCPredict(
        home = home[i],
        away = away[i],
        params = .pwhl_params(),
        draws = FALSE
      )
      c(p[1], p[2])
    },
    numeric(2)
  ))
  expect_equal(unname(vec), unname(ref), tolerance = 1e-10)
  expect_equal(rowSums(vec), rep(1, length(home)))
})

# ============ playoffWin / randomSeriesWinner with PWHL teams ============
test_that("playoffWin returns a probability in (0, 1) for PWHL teams", {
  result <- playoffWin(
    .pwhl_teams()[1],
    .pwhl_teams()[2],
    params = .pwhl_params()
  )
  expect_true(result > 0 && result < 1)
})

test_that("randomSeriesWinner returns one of the two PWHL teams", {
  set.seed(42)
  a <- .pwhl_teams()[1]
  b <- .pwhl_teams()[2]
  result <- randomSeriesWinner(a, b, params = .pwhl_params())
  expect_true(result %in% c(a, b))
})

# ============ .dcPredictErrorRecover / .dcLambda with PWHL model (#81) ============
test_that(".dcLambda recovers unseen-team expected goals from a supplied PWHL model", {
  # A synthetic model disjoint from the bundled PWHL fit: an unseen home team
  # must take the recovery path using the *supplied* model, not pwhl_m.
  fake_m <- list(
    coefficients = c(
      `TeamAlpha` = 1,
      `TeamBeta` = 2,
      `OpponentAlpha` = -0.5,
      `OpponentBeta` = -0.6,
      Home = 0.3
    ),
    data = data.frame(
      Team = factor(c("Alpha", "Beta")),
      Opponent = factor(c("Alpha", "Beta"))
    )
  )

  xg <- .dcLambda(
    home = "Unseen City",
    away = "Alpha",
    params = list(m = fake_m)
  )
  expect_equal(xg$home, exp(min(c(1, 2)) - 0.5 + 0.3))
})
