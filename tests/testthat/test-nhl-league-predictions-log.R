context("test-nhl-league-predictions-log")

test_that("Predictions File saves", {
  skip_if_hockey_apis_unavailable()
  tmpfile <- withr::local_tempfile(pattern = "odds-", fileext = ".csv")
  sched <- HockeyModel::scores
  sched <- sched[sched$Date > as.Date("2021-01-01"), ]
  sched <- sched[sched$Date < as.Date("2021-01-31"), ]
  expect_true(suppressWarnings(build_past_predictions(
    startDate = "2021-01-29",
    endDate = "2021-01-30",
    filepath = tmpfile,
    schedule = sched
  )))
  expect_true(file.exists(tmpfile))
  preds <- read.csv(tmpfile)
  expect_equal(nrow(preds), 13)
  expect_equal(ncol(preds), 7)
  expect_equal(
    names(preds),
    c("Date", "GameID", "HomeTeam", "AwayTeam", "HomeWin", "AwayWin", "Draw")
  )

  expect_true(cleanupPredictionsFile(tmpfile))

  file.remove(tmpfile)
  expect_false(file.exists(tmpfile))
})
