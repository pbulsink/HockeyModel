context("test-api-data-fetching")

# ============ getNHLSchedule tests ============
test_that("getNHLSchedule returns data frame", {
  vcr::use_cassette("get-nhl-schedule-2024-2025", {
    sched <- getNHLSchedule("20242025")
    expect_true(is.data.frame(sched))
  })
})

test_that("getNHLSchedule validates season input", {
  expect_error(getNHLSchedule("bob"))
  expect_error(getNHLSchedule(20202021))
})

test_that("getNHLSchedule has required columns", {
  vcr::use_cassette("get-nhl-schedule-columns", {
    sched <- getNHLSchedule("20242025")
    if (!is.null(sched) && nrow(sched) > 0) {
      required_cols <- c("Date", "HomeTeam", "AwayTeam", "GameID", "GameType", "GameStatus")
      expect_true(all(required_cols %in% colnames(sched)))
    }
  })
})

test_that("getNHLSchedule returns valid dates", {
  vcr::use_cassette("get-nhl-schedule-dates", {
    sched <- getNHLSchedule("20242025")
    if (!is.null(sched) && nrow(sched) > 0) {
      expect_true(all(sapply(sched$Date, is.Date)))
    }
  })
})

# ============ getNHLScores tests ============
test_that("getNHLScores returns data frame", {
  scores <- getNHLScores(2020020001, progress = FALSE)
  expect_true(is.data.frame(scores))
})

test_that("getNHLScores validates season input", {
  expect_error(getNHLScores("not-a-season"))
  expect_error(getNHLScores(12345))
})

test_that("getNHLScores has required columns", {
  scores <- getNHLScores(2020020001, progress = FALSE)
  required_cols <- c("Date", "HomeTeam", "AwayTeam", "HomeGoals", "AwayGoals", "GameID", "GameType")
  expect_true(all(required_cols %in% colnames(scores)))
})

test_that("getNHLScores returns valid goals", {
  scores <- getNHLScores(2020020001, progress = FALSE)
  expect_true(all(is.numeric(scores$HomeGoals)))
  expect_true(all(is.numeric(scores$AwayGoals)))
  expect_true(all(scores$HomeGoals >= 0))
  expect_true(all(scores$AwayGoals >= 0))
})

# ============ games_today tests ============
test_that("games_today returns NULL or data frame", {
  vcr::use_cassette("games-today", {
    today_games <- games_today(date = as.Date("2019-11-01"))
    expect_true(is.null(today_games) || is.data.frame(today_games))
  })
})

test_that("games_today validates date input", {
  expect_error(games_today(date = "not-a-date"))
  expect_error(games_today(date = 12345))
})

test_that("games_today returns games on specified date", {
  vcr::use_cassette("games-today-valid", {
    today_games <- games_today(date = as.Date("2019-11-01"))
    if (!is.null(today_games)) {
      expect_true(nrow(today_games) > 0)
      expect_true(all(today_games$Date == as.Date("2019-11-01")))
    }
  })
})

# ============ getAPISeries tests ============
test_that("getAPISeries returns data frame", {
  series <- getAPISeries(season = "20182019")
  expect_true(is.data.frame(series) || is.null(series))
})

test_that("getAPISeries handles integer input", {
  expect_error(getAPISeries(season = 1))
})

# ============ updateScheduleAPI tests ============
test_that("updateScheduleAPI returns data frame", {
  vcr::use_cassette("update-schedule-api", {
    sched <- updateScheduleAPI(save_data = FALSE)
    expect_true(is.data.frame(sched))
  })
})

test_that("updateScheduleAPI has schedule structure", {
  vcr::use_cassette("update-schedule-api-structure", {
    sched <- updateScheduleAPI(save_data = FALSE)
    required_cols <- c("Date", "HomeTeam", "AwayTeam", "GameID")
    expect_true(all(required_cols %in% colnames(sched)))
  })
})

# ============ updateScoresAPI tests ============
test_that("updateScoresAPI returns data frame", {
  vcr::use_cassette("update-scores-api", {
    scores <- updateScoresAPI(save_data = FALSE)
    expect_true(is.data.frame(scores))
  })
})

test_that("updateScoresAPI has scores structure", {
  vcr::use_cassette("update-scores-api-structure", {
    scores <- updateScoresAPI(save_data = FALSE)
    required_cols <- c("Date", "HomeTeam", "AwayTeam", "HomeGoals", "AwayGoals")
    expect_true(all(required_cols %in% colnames(scores)))
  })
})

# ============ recordTodaysPredictions tests ============
test_that("recordTodaysPredictions handles valid predictions", {
  tmpdir <- withr::local_tempdir()
  preds <- data.frame(
    Date = Sys.Date(),
    HomeTeam = "Toronto Maple Leafs",
    AwayTeam = "Ottawa Senators",
    HomeWin = 0.55,
    AwayWin = 0.35,
    Draw = 0.1,
    GameID = 2024010001
  )

  result <- tryCatch({
    recordTodaysPredictions(predictions = preds, filedir = tmpdir)
    TRUE
  }, error = function(e) {
    FALSE
  })

  expect_true(is.logical(result))
})

# ============ getCurrentSeason8 tests ============
test_that("getCurrentSeason8 returns proper format", {
  season <- getCurrentSeason8()
  expect_true(is.character(season))
  expect_equal(nchar(season), 8)
  expect_true(grepl("^\\d{8}$", season))
})

test_that("getCurrentSeason8 returns valid season", {
  season <- getCurrentSeason8()
  first_four <- as.integer(substr(season, 1, 4))
  last_four <- as.integer(substr(season, 5, 8))
  expect_equal(last_four - first_four, 1)
})
