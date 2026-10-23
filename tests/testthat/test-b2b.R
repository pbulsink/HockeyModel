# Tests for the back-to-back (b2b) adjustment (#64).

# ============ .b2b_flags() ============
test_that(".b2b_flags flags teams that played exactly one day prior", {
  games <- data.frame(
    Date = as.Date("2024-01-15"),
    HomeTeam = c("Toronto Maple Leafs", "Boston Bruins", "San Jose Sharks"),
    AwayTeam = c("Ottawa Senators", "Montreal Canadiens", "Vancouver Canucks"),
    stringsAsFactors = FALSE
  )
  # Toronto and San Jose played on 2024-01-14 (yesterday); Boston did not.
  history <- data.frame(
    Date = as.Date("2024-01-14"),
    HomeTeam = c(
      "Toronto Maple Leafs",
      "New Jersey Devils",
      "Seattle Kraken",
      "Nashville Predators"
    ),
    AwayTeam = c(
      "Washington Capitals",
      "Montreal Canadiens",
      "San Jose Sharks",
      "Detroit Red Wings"
    ),
    stringsAsFactors = FALSE
  )

  flags <- .b2b_flags(games, history)
  # Toronto (home) and San Jose (away) played yesterday; Boston did not.
  expect_equal(flags$home_b2b, c(TRUE, FALSE, TRUE))
  # Ottawa and Vancouver did not play yesterday; Montreal (away) did.
  expect_equal(flags$away_b2b, c(FALSE, TRUE, FALSE))
})

test_that(".b2b_flags handles both teams on a b2b", {
  games <- data.frame(
    Date = as.Date("2024-01-15"),
    HomeTeam = "Toronto Maple Leafs",
    AwayTeam = "Ottawa Senators",
    stringsAsFactors = FALSE
  )
  history <- data.frame(
    Date = as.Date("2024-01-14"),
    HomeTeam = "Toronto Maple Leafs",
    AwayTeam = "Ottawa Senators",
    stringsAsFactors = FALSE
  )

  flags <- .b2b_flags(games, history)
  expect_true(flags$home_b2b[1])
  expect_true(flags$away_b2b[1])
})

test_that(".b2b_flags returns all FALSE for empty history", {
  games <- data.frame(
    Date = as.Date("2024-01-15"),
    HomeTeam = "Toronto Maple Leafs",
    AwayTeam = "Ottawa Senators",
    stringsAsFactors = FALSE
  )
  flags <- .b2b_flags(
    games,
    history = data.frame(
      Date = as.Date(character()),
      HomeTeam = character(),
      AwayTeam = character()
    )
  )
  expect_false(any(flags$home_b2b))
  expect_false(any(flags$away_b2b))
})

test_that(".b2b_flags ignores games not exactly one day prior", {
  # A game two days earlier (or later) must NOT count as a b2b.
  games <- data.frame(
    Date = as.Date("2024-01-15"),
    HomeTeam = "Toronto Maple Leafs",
    AwayTeam = "Ottawa Senators",
    stringsAsFactors = FALSE
  )
  history <- data.frame(
    Date = as.Date(c("2024-01-13", "2024-01-16")), # two days before, and in the future
    HomeTeam = c("Toronto Maple Leafs", "Ottawa Senators"),
    AwayTeam = c("New Jersey Devils", "Philadelphia Flyers"),
    stringsAsFactors = FALSE
  )

  flags <- .b2b_flags(games, history)
  expect_false(flags$home_b2b[1])
  expect_false(flags$away_b2b[1])
})

test_that(".b2b_flags returns empty logicals for no games", {
  games <- data.frame(
    Date = as.Date(character()),
    HomeTeam = character(),
    AwayTeam = character()
  )
  history <- data.frame(
    Date = as.Date("2024-01-14"),
    HomeTeam = "Toronto Maple Leafs",
    AwayTeam = "Ottawa Senators"
  )
  flags <- .b2b_flags(games, history)
  expect_length(flags$home_b2b, 0)
  expect_length(flags$away_b2b, 0)
})

# ============ b2b scaling of lambda/mu (exact exp(+/- eps)) ============
test_that(".dcProbMatrix scales lambda/mu by exactly exp(+/- eps)", {
  params <- .parse_dc_params(NULL)
  home <- "Toronto Maple Leafs"
  away <- "Ottawa Senators"
  base <- .dcLambda(home, away, params) # list(home = lam, away = mu)

  eps_h <- B2B_HOME_NHL
  eps_a <- B2B_AWAY_NHL

  # Home on b2b: home attack down (lam * exp(-eps_h)), home defence weaker
  # so away scores more (mu * exp(+eps_h)).
  pm_home <- .dcProbMatrix(home, away, params, home_b2b = TRUE)
  ref_home <- .prob_matrix(
    lambda = base$home * exp(-eps_h),
    mu = base$away * exp(eps_h),
    params = params,
    maxgoal = 10
  )
  expect_equal(pm_home, ref_home, tolerance = 1e-10)

  # Away on b2b: away attack down (mu * exp(-eps_a)), away defence weaker so
  # home scores more (lam * exp(+eps_a)).
  pm_away <- .dcProbMatrix(home, away, params, away_b2b = TRUE)
  ref_away <- .prob_matrix(
    lambda = base$home * exp(eps_a),
    mu = base$away * exp(-eps_a),
    params = params,
    maxgoal = 10
  )
  expect_equal(pm_away, ref_away, tolerance = 1e-10)

  # Both on b2b: both adjustments compose.
  pm_both <- .dcProbMatrix(home, away, params, home_b2b = TRUE, away_b2b = TRUE)
  ref_both <- .prob_matrix(
    lambda = base$home * exp(-eps_h + eps_a),
    mu = base$away * exp(eps_h - eps_a),
    params = params,
    maxgoal = 10
  )
  expect_equal(pm_both, ref_both, tolerance = 1e-10)

  # Neither on b2b: unchanged (backward compatible).
  pm_none <- .dcProbMatrix(home, away, params)
  ref_none <- .prob_matrix(
    lambda = base$home,
    mu = base$away,
    params = params,
    maxgoal = 10
  )
  expect_equal(pm_none, ref_none, tolerance = 1e-10)
})

# ============ Vectorized-vs-scalar equivalence with b2b flags ============
test_that(".dcPredictVectorized matches DCPredict per-game with b2b flags", {
  params <- .parse_dc_params(NULL)
  home <- c(
    "Toronto Maple Leafs",
    "Ottawa Senators",
    "New Jersey Devils",
    "Philadelphia Flyers"
  )
  away <- c(
    "Ottawa Senators",
    "Toronto Maple Leafs",
    "Philadelphia Flyers",
    "New Jersey Devils"
  )
  home_b2b <- c(TRUE, FALSE, TRUE, FALSE)
  away_b2b <- c(FALSE, TRUE, TRUE, FALSE)

  vec <- .dcPredictVectorized(
    home = home,
    away = away,
    params = params,
    draws = TRUE,
    home_b2b = home_b2b,
    away_b2b = away_b2b
  )
  ref <- t(vapply(
    seq_along(home),
    function(i) {
      p <- DCPredict(
        home = home[i],
        away = away[i],
        params = params,
        draws = TRUE,
        home_b2b = home_b2b[i],
        away_b2b = away_b2b[i]
      )
      c(p[1], p[2], p[3])
    },
    numeric(3)
  ))
  expect_equal(unname(vec), unname(ref), tolerance = 1e-10)
})

test_that(".dcPredictVectorized matches DCPredict with b2b flags and regression", {
  # The b2b adjustment is a fixed situational effect: it must be applied AFTER
  # the regression-to-mean blend in both paths so they stay identical.
  params <- .parse_dc_params(NULL)
  home <- c("Toronto Maple Leafs", "New Jersey Devils", "Washington Capitals")
  away <- c("Ottawa Senators", "Philadelphia Flyers", "Buffalo Sabres")
  home_b2b <- c(TRUE, FALSE, TRUE)
  away_b2b <- c(FALSE, TRUE, FALSE)

  vec <- .dcPredictVectorized(
    home = home,
    away = away,
    params = params,
    draws = TRUE,
    expected_mean = 2.835184,
    season_percent = 0.5,
    home_b2b = home_b2b,
    away_b2b = away_b2b
  )
  ref <- t(vapply(
    seq_along(home),
    function(i) {
      p <- DCPredict(
        home = home[i],
        away = away[i],
        params = params,
        draws = TRUE,
        expected_mean = 2.835184,
        season_percent = 0.5,
        home_b2b = home_b2b[i],
        away_b2b = away_b2b[i]
      )
      c(p[1], p[2], p[3])
    },
    numeric(3)
  ))
  expect_equal(unname(vec), unname(ref), tolerance = 1e-10)
})

# ============ .todayDC() integration ============
test_that(".todayDC applies b2b to a team playing on consecutive days", {
  params <- .parse_dc_params(NULL)
  d <- as.Date("2024-01-15")
  # Toronto plays Ottawa on day 1 and Montreal on day 2 -> Toronto is on a b2b.
  sched <- data.frame(
    Date = c(d, d + 1),
    GameID = 1:2,
    HomeTeam = c("Toronto Maple Leafs", "Toronto Maple Leafs"),
    AwayTeam = c("Ottawa Senators", "Montreal Canadiens"),
    stringsAsFactors = FALSE
  )

  p_b2b <- .todayDC(today = d + 1, params = params, schedule = sched)
  # Control: only the day-2 game in the schedule -> no prior game, no b2b.
  sched_ctrl <- sched[sched$Date == d + 1, ]
  p_ctrl <- .todayDC(today = d + 1, params = params, schedule = sched_ctrl)

  # The back-to-back home team's odds must differ from the non-b2b baseline.
  expect_false(isTRUE(all.equal(p_b2b$HomeWin, p_ctrl$HomeWin)))
})

test_that(".todayDC leaves non-b2b games unchanged", {
  params <- .parse_dc_params(NULL)
  d <- as.Date("2024-01-15")
  # Boston did NOT play on day 1, so its day-2 game must be unaffected by b2b.
  sched <- data.frame(
    Date = c(d, d + 1),
    GameID = 1:2,
    HomeTeam = c("Toronto Maple Leafs", "Boston Bruins"),
    AwayTeam = c("Ottawa Senators", "Montreal Canadiens"),
    stringsAsFactors = FALSE
  )

  # Only the day-2 game is returned (Boston), so it's row 1 of each result.
  p_full <- .todayDC(today = d + 1, params = params, schedule = sched)
  # Control with only the Boston game (no prior games at all).
  sched_ctrl <- sched[sched$Date == d + 1 & sched$HomeTeam == "Boston Bruins", ]
  p_ctrl <- .todayDC(today = d + 1, params = params, schedule = sched_ctrl)

  expect_equal(p_full$HomeWin[1], p_ctrl$HomeWin[1])
})
