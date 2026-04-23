# Extracted from test-league.R:44

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-league")

# test -------------------------------------------------------------------------
tmpfile <- withr::local_tempfile(pattern = "odds-", fileext = ".csv")
expect_true(suppressWarnings(build_past_predictions(startDate = "2021-01-30", endDate = "2021-01-30", filepath = tmpfile)))
expect_true(file.exists(tmpfile))
preds <- read.csv(tmpfile)
