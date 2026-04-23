context("test-api-interface")
skip_if_hockey_apis_unavailable()

test_that("Schedules are ok", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)

  sched <- getNHLSchedule()
  expect_true(is.data.frame(sched))
  expect_equal(ncol(sched), 6)
  expect_equal(colnames(sched), c("Date", "HomeTeam", "AwayTeam", "GameID", "GameType", "GameStatus"))
  expect_true(all(sched$GameType %in% c("R", "P")))
})

test_that("Scores are OK", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)
  withr::local_file(file.path(tmpdir, "xG.csv"))
  write.table(data.frame("GameId" = 2020020001, "home_xg" = 4.3, "away_xg" = 3.1),
    file = file.path(tmpdir, "xG.csv"),
    row.names = FALSE, col.names = TRUE, sep = ","
  )

  score <- getNHLScores(2020020001, progress = F)
  expect_true(is.data.frame(score))
  expect_equal(nrow(score), 1)
  required_cols <- c(
    "Date", "HomeTeam", "AwayTeam", "GameID", "HomeGoals", "AwayGoals",
    "OTStatus", "GameType", "GameStatus", "Result", "HomexG", "AwayxG"
  )
  expect_true(all(required_cols %in% colnames(score)))
  expect_equal(score$GameID[[1]], 2020020001)
  expect_equal(score$GameStatus[[1]], "Final")
  expect_true(is.numeric(score$HomexG[[1]]) || is.na(score$HomexG[[1]]))
  expect_true(is.numeric(score$AwayxG[[1]]) || is.na(score$AwayxG[[1]]))

  expect_message(games_today(date = as.Date("2019-11-01")), "Games on today aren't present in Schedule")
  expect_true(is.null(games_today(date = as.Date("2019-11-01")))) # Why null? because games_today only returns 'scheduled' games from a date. NULL return is equivalent to finishing the code anyway (i.e. not an error)
})

test_that("Series is ok", {
  # tough to test as it's a moving target
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)

  series <- getAPISeries()
  if (inPlayoffs()) {
    # now there should be a series
    expect_gt(nrow(series), 0)
    expect_true(is.data.frame(series))
  }
  series <- getAPISeries("20182019")
  expect_true(is.data.frame(series))
  expect_equal(nrow(series), 15)
  expect_equal(ncol(series), 10)
  expect_true(all(series$Status == "Complete"))
})

test_that("Season Dates & Binaries work", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)

  expect_visible(inRegularSeason())
  expect_visible(inPlayoffs())
  expect_visible(inOffSeason())
  expect_false(inOffSeason("2018-12-02"))
  expect_equal(inRegularSeason("2018-12-02", boolean = FALSE), "20182019")
  expect_false(inPlayoffs("2018-12-02", boolean = FALSE))
  expect_true(inOffSeason("2018-08-01"))
})

test_that("SeasonID gets seasons ok", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)

  expect_match(getCurrentSeason8(), regexp = "\\d{8}")
  expect_equal(getSeason("2018-12-02"), "20182019")
  expect_null(getSeason("2018-09-01"))
})

test_that("Get Team Info is OK", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)

  expect_equal(getTeamDivisions("bob"), character(0))
})

test_that("Other Utility Functions are OK", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)

  expect_equal(clean_names(c("Chicago Blackhawks", "Toronto Maple Leafs")), c("Chicago Blackhawks", "Toronto Maple Leafs"))
  expect_equal(getTeamConferences("Chicago Blackhawks"), "Western")
  expect_equal(getTeamConferences("Toronto Maple Leafs"), "Eastern")
  expect_equal(getTeamDivisions("Toronto Maple Leafs"), "Atlantic")
  expect_equal(getShortTeam("Toronto Maple Leafs"), "TOR")
  expect_equal(getSeasonEndDate(season = "20182019"), as.Date("2019-06-12"))
  expect_equal(getNumGames("20202021"), 56)
})
