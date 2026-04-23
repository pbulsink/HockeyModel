# Extracted from test-api-data-fetching.R:118

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-api-data-fetching")

# test -------------------------------------------------------------------------
vcr::use_cassette("update-scores-api", {
    scores <- updateScoresAPI(save_data = FALSE)
    expect_true(is.data.frame(scores))
  })
