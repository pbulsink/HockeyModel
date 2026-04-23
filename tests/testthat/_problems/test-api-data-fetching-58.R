# Extracted from test-api-data-fetching.R:58

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-api-data-fetching")

# test -------------------------------------------------------------------------
vcr::use_cassette("get-nhl-scores-single-game-goals", {
    scores <- getNHLScores(2020020001, progress = FALSE)
    expect_true(all(is.numeric(scores$HomeGoals)))
    expect_true(all(is.numeric(scores$AwayGoals)))
    expect_true(all(scores$HomeGoals >= 0))
    expect_true(all(scores$AwayGoals >= 0))
  })
