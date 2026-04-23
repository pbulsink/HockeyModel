# Extracted from test-utils.R:281

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-utils")

# test -------------------------------------------------------------------------
expect_equal(HockeyModel::getSeason("2018-10-15"), 20182019L)
