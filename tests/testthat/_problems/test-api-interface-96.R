# Extracted from test-api-interface.R:96

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-api-interface")

# test -------------------------------------------------------------------------
tmpdir <- withr::local_tempdir()
withr::local_options("HockeyModel.prediction.path" = tmpdir)
expect_equal(clean_names(c("Chicago Blackhawks", "Toronto Maple Leafs")), c("Chicago Blackhawks", "Toronto Maple Leafs"))
expect_equal(getTeamConferences("Chicago Blackhawks"), "Western")
expect_equal(getTeamConferences("Toronto Maple Leafs"), "Eastern")
expect_equal(getTeamDivisions("Toronto Maple Leafs"), "Atlantic")
expect_equal(getShortTeam("Toronto Maple Leafs"), "TOR")
expect_equal(getSeasonEndDate(season = "20182019"), as.Date("2019-06-12"))
