# Extracted from test-utils.R:286

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-utils")

# test -------------------------------------------------------------------------
result <- HockeyModel::getSeason("2020-12-25")
