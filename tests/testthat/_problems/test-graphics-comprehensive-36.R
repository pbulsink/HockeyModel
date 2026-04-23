# Extracted from test-graphics-comprehensive.R:36

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-graphics")

# test -------------------------------------------------------------------------
p <- suppressWarnings(todayOddsPlot())
