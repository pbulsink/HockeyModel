# Extracted from test-graphics-comprehensive.R:20

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-graphics")

# test -------------------------------------------------------------------------
expect_error(suppressWarnings(plot_pace_by_team()), NA)
