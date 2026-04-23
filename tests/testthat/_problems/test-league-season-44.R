# Extracted from test-league-season.R:44

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-league-season")

# test -------------------------------------------------------------------------
result <- inOffSeason(as.Date("2018-07-15"))
