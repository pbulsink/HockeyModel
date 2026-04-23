# Extracted from test-graphics.R:65

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
preds <- HockeyModel::example_raw_predictions
p <- suppressWarnings(plot_point_likelihood(preds = preds, savefiles = FALSE))
