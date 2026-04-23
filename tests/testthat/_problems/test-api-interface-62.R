# Extracted from test-api-interface.R:62

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-api-interface")

# test -------------------------------------------------------------------------
tmpdir <- withr::local_tempdir()
withr::local_options("HockeyModel.prediction.path" = tmpdir)
expect_visible(inRegularSeason())
