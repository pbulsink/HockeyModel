# Extracted from test-graphics.R:74

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
series <- getAPISeries("20182019")
