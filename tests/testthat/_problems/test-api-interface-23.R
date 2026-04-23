# Extracted from test-api-interface.R:23

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "HockeyModel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("test-api-interface")

# test -------------------------------------------------------------------------
tmpdir <- withr::local_tempdir()
withr::local_options("HockeyModel.prediction.path" = tmpdir)
withr::local_file(file.path(tmpdir, "xG.csv"))
write.table(data.frame("GameId" = 2020020001, "home_xg" = 4.3, "away_xg" = 3.1),
    file = file.path(tmpdir, "xG.csv"),
    row.names = FALSE, col.names = TRUE, sep = ","
  )
score <- getNHLScores(2020020001, progress = F)
