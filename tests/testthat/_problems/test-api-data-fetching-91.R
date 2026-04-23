# Extracted from test-api-data-fetching.R:91

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-api-data-fetching")

# test -------------------------------------------------------------------------
series <- getAPISeries(season = "20182019")
