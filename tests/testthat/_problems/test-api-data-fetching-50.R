# Extracted from test-api-data-fetching.R:50

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-api-data-fetching")

# test -------------------------------------------------------------------------
vcr::use_cassette("get-nhl-scores-single-game-columns", {
    scores <- getNHLScores(2020020001, progress = FALSE)
    required_cols <- c("Date", "HomeTeam", "AwayTeam", "HomeGoals", "AwayGoals", "GameID", "GameType")
    expect_true(all(required_cols %in% colnames(scores)))
  })
