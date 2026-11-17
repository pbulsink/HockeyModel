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
