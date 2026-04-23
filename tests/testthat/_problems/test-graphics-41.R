# Extracted from test-graphics.R:41

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
p <- suppressWarnings(plot_odds_today(today = as.Date("2019-11-01")))
