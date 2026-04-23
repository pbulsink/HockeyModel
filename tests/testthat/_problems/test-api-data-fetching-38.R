# Extracted from test-api-data-fetching.R:38

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-api-data-fetching")

# test -------------------------------------------------------------------------
vcr::use_cassette("get-nhl-scores-single-game", {
    scores <- getNHLScores(2020020001, progress = FALSE)
    expect_true(is.data.frame(scores))
  })
