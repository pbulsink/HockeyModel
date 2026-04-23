# Extracted from test-api-data-fetching.R:125

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-api-data-fetching")

# test -------------------------------------------------------------------------
vcr::use_cassette("update-scores-api-structure", {
    scores <- updateScoresAPI(save_data = FALSE)
    required_cols <- c("Date", "HomeTeam", "AwayTeam", "HomeGoals", "AwayGoals")
    expect_true(all(required_cols %in% colnames(scores)))
  })
