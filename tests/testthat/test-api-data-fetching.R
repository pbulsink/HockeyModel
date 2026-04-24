context("test-api-data-fetching")
skip_if_hockey_apis_unavailable()

# ============ getNHLSchedule tests ============

test_that("getNHLSchedule validates season input", {
  expect_error(getNHLSchedule("bob"))
  expect_error(getNHLSchedule(20202021))
})

test_that("getNHLSchedule has required structure", {
  vcr::use_cassette("nhl-schedule", {
    sched <- getNHLSchedule("20242025")
    expect_true(is.data.frame(sched))
    expect_gt(nrow(sched), 0)

    required_cols <- c("Date", "HomeTeam", "AwayTeam", "GameID", "GameType", "GameStatus")
    expect_true(all(required_cols %in% colnames(sched)))

    expect_true(all(sapply(sched$Date, is.Date)))
  })
})

# ============ getNHLScores tests ============

test_that("getNHLScores validates season input", {
  expect_error(getNHLScores("not-a-season"))
  expect_error(getNHLScores(12345))
})

test_that("getNHLScores has required structure", {
  vcr::use_cassette("get-nhl-scores-single-game-columns", {
    scores <- getNHLScores(2020020001, progress = FALSE)

    expect_true(is.data.frame(scores))
    required_cols <- c("Date", "HomeTeam", "AwayTeam", "HomeGoals", "AwayGoals", "GameID", "GameType")
    expect_true(all(required_cols %in% colnames(scores)))

    expect_true(all(is.numeric(scores$HomeGoals)))
    expect_true(all(is.numeric(scores$AwayGoals)))
    expect_true(all(scores$HomeGoals >= 0))
    expect_true(all(scores$AwayGoals >= 0))
  })
})

# ============ games_today tests ============
test_that("games_today returns NULL or data frame", {
  sched <- HockeyModel::scores
  sched <- sched[sched$Date > as.Date("2019-10-01"), ]
  sched <- sched[sched$Date < as.Date("2019-12-31"), ]
  vcr::use_cassette("games-today", {
    today_games_not_scheduled <- games_today(date = as.Date("2019-11-01"))
    expect_true(is.null(today_games_not_scheduled))

    today_games <- games_today(date = as.Date("2019-11-01"), schedule = sched)
    expect_true(is.data.frame(today_games))
    expect_true(nrow(today_games) > 0)
    expect_true(all(today_games$Date == as.Date("2019-11-01")))
  })
})

test_that("games_today validates date input", {
  expect_error(games_today(date = "not-a-date"))
  expect_error(games_today(date = 12345))
})

# ============ getAPISeries tests ============
test_that("getAPISeries returns data frame", {
  vcr::use_cassette("get-series", {
    series <- getAPISeries(season = "20182019")
    expect_true(is.data.frame(series))
    expect_equal(nrow(series), 15)
    expect_true(all(series$status == "Complete"))
  })
})

test_that("getAPISeries handles integer input", {
  expect_error(getAPISeries(season = 1))
})

# ============ getCurrentSeason8 tests ============
test_that("getCurrentSeason8 returns proper format", {
  vcr::use_cassette("current-season", {
    season <- getCurrentSeason8()
    expect_true(is.character(season))
    expect_equal(nchar(season), 8)
    expect_true(grepl("^\\d{8}$", season))

    first_four <- as.integer(substr(season, 1, 4))
    last_four <- as.integer(substr(season, 5, 8))
    expect_equal(last_four - first_four, 1)
  })
})
