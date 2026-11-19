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

  # The ongoing branch is reached by matching on SeriesID (before the fix this
  # errored in is.na()). Intentionally passing mismatched teams exercises both
  # the selection and the team-mismatch warning.
  expect_warning(
    .single_series_solver(1, cs, "Some Home", "Some Away") %in%
      c("Toronto Maple Leafs", "Ottawa Senators"),
    regexp = "Team Mismatch"
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

# #74: simulatePlayoffs() must fetch live series from getAPISeries() and mix
# them into the bracket. Previously the call was commented out, so currentSeries
# was always empty and a team already through round 1 could still lose it in
# simulation.
test_that("simulatePlayoffs fetches and maps live API series without erroring (#74)", {
  summary_results <- HockeyModel::summary_results_testing
  teams <- c(
    "Toronto Maple Leafs",
    "Ottawa Senators",
    "Boston Bruins",
    "Detroit Red Wings",
    "New Jersey Devils",
    "Philadelphia Flyers",
    "Tampa Bay Lightning",
    "Florida Panthers",
    "Los Angeles Kings",
    "Vegas Golden Knights",
    "Dallas Stars",
    "Colorado Avalanche",
    "St. Louis Blues",
    "Minnesota Wild",
    "Edmonton Oilers",
    "Winnipeg Jets"
  )

  # The API reports first-round series as letters A-O; simulatePlayoffs maps
  # them to numeric SeriesIDs that the engine keys off — any unmapped letter
  # would crash the solver rather than short-circuit. All eight slots complete,
  # so the bracket is resolvable through the mapping alone.
  api_series <- data.frame(
    Round = rep(1L, 8),
    Series = LETTERS[1:8],
    HomeTeam = teams[c(1, 3, 5, 7, 9, 11, 13, 15)],
    AwayTeam = teams[c(2, 4, 6, 8, 10, 12, 14, 16)],
    HomeWins = rep(4L, 8),
    AwayWins = rep(2L, 8),
    HomeSeed = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
    AwaySeed = c(8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L),
    Status = rep("Complete", 8)
  )

  local_mocked_bindings(
    getAPISeries = function(...) api_series,
    .package = "HockeyModel"
  )

  # No crash means every letter mapped to a valid SeriesID.
  res <- simulatePlayoffs(
    summary_results = summary_results,
    nsims = 2,
    cores = 1
  )
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), nrow(summary_results))
})

test_that("simulatePlayoffs short-circuits completed API series (#74)", {
  summary_results <- HockeyModel::summary_results_testing

  teams <- c(
    "Toronto Maple Leafs",
    "Ottawa Senators",
    "Boston Bruins",
    "Detroit Red Wings",
    "New Jersey Devils",
    "Philadelphia Flyers",
    "Tampa Bay Lightning",
    "Florida Panthers",
    "Los Angeles Kings",
    "Vegas Golden Knights",
    "Dallas Stars",
    "Colorado Avalanche",
    "St. Louis Blues",
    "Minnesota Wild",
    "Edmonton Oilers",
    "Winnipeg Jets"
  )

  api_series <- data.frame(
    Round = rep(1L, 8),
    Series = LETTERS[1:8],
    HomeTeam = teams[c(1, 3, 5, 7, 9, 11, 13, 15)],
    AwayTeam = teams[c(2, 4, 6, 8, 10, 12, 14, 16)],
    HomeWins = rep(4L, 8),
    AwayWins = rep(2L, 8),
    HomeSeed = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
    AwaySeed = c(8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L),
    Status = rep("Complete", 8)
  )

  local_mocked_bindings(
    getAPISeries = function(...) api_series,
    .package = "HockeyModel"
  )

  res <- simulatePlayoffs(
    summary_results = summary_results,
    nsims = 2,
    cores = 1
  )

  expect_true(is.data.frame(res))
  # Home teams won every mocked series: each clinches round 1 in all sims.
  expect_equal(
    sum(res$Win_First_Round[res$Team %in% api_series$HomeTeam]),
    length(api_series$HomeTeam)
  )
  expect_equal(sum(res$Win_First_Round[res$Team %in% api_series$AwayTeam]), 0)
})

test_that("simulatePlayoffs mixes in ongoing API series (#74)", {
  summary_results <- HockeyModel::summary_results_testing

  # One live (ongoing) first-round series per conference; the other six slots
  # are completed so the bracket is fully resolvable.
  api_series <- data.frame(
    Round = rep(1L, 8),
    Series = LETTERS[1:8],
    HomeTeam = c(
      "Toronto Maple Leafs",
      "Boston Bruins",
      "New Jersey Devils",
      "Tampa Bay Lightning",
      "Los Angeles Kings",
      "Dallas Stars",
      "St. Louis Blues",
      "Edmonton Oilers"
    ),
    AwayTeam = c(
      "Ottawa Senators",
      "Detroit Red Wings",
      "Philadelphia Flyers",
      "Florida Panthers",
      "Vegas Golden Knights",
      "Colorado Avalanche",
      "Minnesota Wild",
      "Winnipeg Jets"
    ),
    HomeWins = c(2L, 4L, 4L, 4L, 1L, 4L, 4L, 4L),
    AwayWins = c(1L, 3L, 3L, 2L, 0L, 2L, 1L, 2L),
    HomeSeed = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
    AwaySeed = c(8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L),
    Status = c(
      "Ongoing",
      "Complete",
      "Complete",
      "Complete",
      "Ongoing",
      "Complete",
      "Complete",
      "Complete"
    )
  )

  local_mocked_bindings(
    getAPISeries = function(...) api_series,
    .package = "HockeyModel"
  )

  # Simulated seeding generally differs from the real bracket, so "Team
  # Mismatch" warnings here are expected and informational.
  res <- suppressWarnings(
    simulatePlayoffs(summary_results = summary_results, nsims = 2, cores = 1)
  )

  expect_true(is.data.frame(res))
  # Each ongoing series resolves through .single_series_solver() (the old
  # is.na()-on-a-data-frame bug would have errored here): its two teams together
  # win that round-1 slot in every sim, so their first-round proportions sum to 1.
  expect_equal(
    sum(res$Win_First_Round[
      res$Team %in% c("Toronto Maple Leafs", "Ottawa Senators")
    ]),
    1
  )
  expect_equal(
    sum(res$Win_First_Round[
      res$Team %in% c("Los Angeles Kings", "Vegas Golden Knights")
    ]),
    1
  )
  # The six completed series are short-circuited to their winners.
  expect_equal(
    sum(res$Win_First_Round[res$Team %in% api_series$HomeTeam[-c(1, 5)]]),
    length(api_series$HomeTeam) - 2L
  )
})

# #75: the parallel path must run exactly `nsims` simulations, not a rounded-up
# multiple of cores*100 tasks. The chunking is what both engines share, so it
# is tested directly (doSNOW workers load the installed package and can't be
# namespace-mocked).
test_that(".simulation_chunks splits nsims exactly", {
  expect_equal(.simulation_chunks(10, 2), c(5L, 5L))
  expect_equal(sum(.simulation_chunks(7, 3)), 7)
  expect_equal(.simulation_chunks(7, 3), c(3L, 2L, 2L))
  # nsims < n: some workers get zero sims, total is still exact.
  expect_equal(sum(.simulation_chunks(1, 4)), 1)
})

test_that("simulatePlayoffs parallel path runs with chunked nsims (#75)", {
  skip_if_not_installed("doSNOW")

  summary_results <- HockeyModel::summary_results_testing
  res <- simulatePlayoffs(
    summary_results = summary_results,
    nsims = 4,
    cores = 2
  )

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), nrow(summary_results))
  expect_true(all(res$Make_Playoffs <= 1))
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
