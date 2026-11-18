context("test-nhl-league-playoffs")

test_that("Playoff series odds ok", {
  expect_equal(playoffSeriesOdds(0.5, 0.5), 0.5)
  expect_equal(playoffSeriesOdds(0.7, 0.3, 3, 3), 0.7)
  expect_equal(playoffSeriesOdds(0.5, 0.5, 0, 3), 1 / 2^4)
  expect_error(
    playoffSeriesOdds(1.2, 0.5),
    regexp = "Error in HockeyModel::playoffSeriesOdds\\(\\)"
  )
  expect_message(
    playoffSeriesOdds(0.5, 0.5, 4, 2),
    regexp = "Series already won; returning 1 for the home team win probability"
  )
  expect_message(
    playoffSeriesOdds(0.5, 0.5, 2, 4),
    regexp = "Series already won; returning 0 for the home team win probability"
  )
})

test_that("Playoff series honours a user-supplied game_home", {
  # Home always wins at home (p=1) and never wins on the road (p=0).
  # All-road pattern: the home team loses every game, so it can only win
  # with probability 0. The buggy code overwrote this with an alternating
  # 4-home-game pattern and returned 1.
  expect_equal(playoffSeriesOdds(1, 0, game_home = rep(FALSE, 7)), 0)

  # A single home game up front: the home team wins only game 1 (p=1), then
  # loses all six road games (p_road=0), so it cannot reach four wins.
  expect_equal(
    playoffSeriesOdds(
      1,
      0,
      game_home = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
    ),
    0
  )

  # A custom pattern equal to the NHL default must match the omitted-argument
  # result exactly.
  nhl_default <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  expect_equal(
    playoffSeriesOdds(0.6, 0.4, game_home = nhl_default),
    playoffSeriesOdds(0.6, 0.4)
  )

  # The default pattern only applies when game_home is omitted: with p_home=1
  # and p_road=0 the NHL default (four home games) lets the home team win,
  # whereas an explicit all-road pattern cannot. This distinguishes the
  # omission path from a forced custom pattern.
  expect_lt(
    playoffSeriesOdds(1, 0, game_home = rep(FALSE, 7)),
    playoffSeriesOdds(1, 0)
  )
})

# #76: simulatePlayoffs() must forward `params` to playoffSolverEngine().
# Mock the engine so we can capture what was passed without running a full sim.
test_that("simulatePlayoffs forwards params to playoffSolverEngine (#76)", {
  summary_results <- HockeyModel::summary_results_testing

  captured_params <- NULL
  local_mocked_bindings(
    playoffSolverEngine = function(
      nsims,
      completedSeries,
      east_results,
      west_results,
      currentSeries,
      summary_results,
      homeAwayOdds,
      params = NULL
    ) {
      captured_params <<- params
      data.frame(matrix(nrow = nsims, ncol = 30)) |>
        setNames(c("SimNo", paste0("l", 1:8), paste0("series", 1:15)))
    },
    .package = "HockeyModel"
  )

  simulatePlayoffs(summary_results = summary_results, nsims = 2, cores = 1)

  # A NULL params argument is resolved to a parsed list (with m/rho/beta/eta/k),
  # not passed through as-is. So we expect a named list with all five keys.
  expect_true(is.list(captured_params))
  expect_true(all(
    c("m", "rho", "beta", "eta", "k") %in% names(captured_params)
  ))
})

# #77: .reseedTwoTeams(p1 = ) must honour the forced home-ice team. An earlier
# version gated this on `!is.null(nrow(p1))`, which is always FALSE for a
# character vector, so p1 was never honoured and seeding fell back to meanPoints
# (plus a coin flip on ties) regardless. Now: supplied p1 wins; empty/NULL falls
# back to meanPoints / coin-flip.
test_that(".reseedTwoTeams honours the forced home-ice team p1 (#77)", {
  sr <- data.frame(
    Team = c("A", "B"),
    meanPoints = c(80, 90),
    stringsAsFactors = FALSE
  )

  # No override: higher-meanPoints team is seeded first.
  expect_equal(.reseedTwoTeams("A", "B", sr), c("B", "A"))
  expect_equal(.reseedTwoTeams("B", "A", sr), c("B", "A"))

  # p1 forces the lower-points team to home ice, overriding meanPoints.
  expect_equal(.reseedTwoTeams("A", "B", sr, p1 = "A"), c("A", "B"))
  expect_equal(.reseedTwoTeams("B", "A", sr, p1 = "B"), c("B", "A"))

  # Empty-string and NA p1 are both treated as no override. A zero-length p1 is
  # what an empty currentSeries actually passes for a TBD slot in the real sim,
  # so that must also fall back (and not error) rather than honouring anything.
  expect_equal(.reseedTwoTeams("A", "B", sr, p1 = ""), c("B", "A"))
  expect_equal(.reseedTwoTeams("A", "B", sr, p1 = NA), c("B", "A"))
  expect_equal(.reseedTwoTeams("A", "B", sr, p1 = character(0)), c("B", "A"))
})

# #78/#79: .single_series_solver() must (a) detect an empty/NULL currentSeries
# without is.na()-on-a-data-frame, and (b) match the requested series by its
# SeriesID column rather than comparing the whole frame element-wise.
test_that(".single_series_solver handles empty/NULL currentSeries (#78)", {
  empty <- data.frame(
    Round = integer(),
    Series = integer(),
    HomeTeam = character(),
    AwayTeam = character(),
    HomeWins = integer(),
    AwayWins = integer(),
    HomeSeed = integer(),
    AwaySeed = integer(),
    Status = character(),
    SeriesID = integer()
  )

  home <- "Toronto Maple Leafs"
  away <- "Ottawa Senators"

  # Empty series: randomSeriesWinner path, returns one of the two teams.
  expect_true(.single_series_solver(1, empty, home, away) %in% c(home, away))

  # A NULL series (a defensive caller could pass this) takes the same branch
  # and must not crash.
  expect_true(.single_series_solver(1, NULL, home, away) %in% c(home, away))
})

test_that(".single_series_solver selects the ongoing series by SeriesID (#79)", {
  # A non-empty frame used to trip is.na() on a data frame (coercion error).
  cs <- data.frame(
    Round = 1L,
    Series = 1L,
    HomeTeam = "Toronto Maple Leafs",
    AwayTeam = "Ottawa Senators",
    HomeWins = 2L,
    AwayWins = 3L,
    HomeSeed = 1L,
    AwaySeed = 8L,
    Status = "Ongoing",
    SeriesID = 1L
  )

  # The ongoing branch uses the API teams (not the passed-in home/away) and
  # returns one of them. Before the fix this errored in is.na(); now it works.
  expect_true(
    .single_series_solver(1, cs, "Some Home", "Some Away") %in%
      c("Toronto Maple Leafs", "Ottawa Senators")
  )
})

test_that(".single_series_solver returns the decided winner for a complete series", {
  cs <- data.frame(
    Round = 1L,
    Series = 1L,
    HomeTeam = "Toronto Maple Leafs",
    AwayTeam = "Ottawa Senators",
    HomeWins = 4L,
    AwayWins = 2L,
    HomeSeed = 1L,
    AwaySeed = 8L,
    Status = "Complete",
    SeriesID = 1L
  )

  # Complete: deterministic by win count (home leads 4-2), no randomness.
  expect_equal(
    .single_series_solver(1, cs, "Toronto Maple Leafs", "Ottawa Senators"),
    "Toronto Maple Leafs"
  )
})

test_that(".single_series_solver warns on team mismatch for a complete series", {
  cs <- data.frame(
    Round = 1L,
    Series = 1L,
    HomeTeam = "Toronto Maple Leafs",
    AwayTeam = "Ottawa Senators",
    HomeWins = 4L,
    AwayWins = 2L,
    HomeSeed = 1L,
    AwaySeed = 8L,
    Status = "Complete",
    SeriesID = 1L
  )

  expect_warning(
    .single_series_solver(1, cs, "Wrong Home", "Wrong Away"),
    regexp = "Team Mismatch"
  )
})

test_that("Playoff Sim finishes OK", {
  summary_results <- HockeyModel::summary_results_testing
  playoffResults <- simulatePlayoffs(
    summary_results = summary_results,
    nsims = 2,
    cores = 1
  )
  expect_true(is.data.frame(playoffResults))
  expect_true(all(playoffResults$Make_Playoffs <= 1))
  expect_equal(sum(playoffResults$Make_Playoffs), 16)
  expect_equal(sum(playoffResults$Win_First_Round), 8)
  expect_equal(sum(playoffResults$Win_Second_Round), 4)
  expect_equal(sum(playoffResults$Win_Conference), 2)
  expect_equal(sum(playoffResults$Win_Cup), 1)

  if (!requireNamespace("doSNOW")) {
    expect_warning(
      simulatePlayoffs(
        summary_results = summary_results,
        nsims = 4,
        cores = 2
      ),
      "Reverting to single-core processing"
    )
  }
  playoffResults <- simulatePlayoffs(
    summary_results = summary_results,
    nsims = 4,
    cores = 2
  )
  expect_true(is.data.frame(playoffResults))
  expect_true(all(playoffResults$Make_Playoffs <= 1))
  expect_equal(sum(playoffResults$Make_Playoffs), 16)
  expect_equal(sum(playoffResults$Win_First_Round), 8)
  expect_equal(sum(playoffResults$Win_Second_Round), 4)
  expect_equal(sum(playoffResults$Win_Conference), 2)
  expect_equal(sum(playoffResults$Win_Cup), 1)
})
