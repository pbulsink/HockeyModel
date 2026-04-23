# Extracted from test-api-data-fetching.R:163

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-api-data-fetching")

# test -------------------------------------------------------------------------
season <- getCurrentSeason8()
