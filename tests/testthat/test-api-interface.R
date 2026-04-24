context("test-api-interface")

test_that("getNHLSchedule validates season input", {
  expect_error(getNHLSchedule("bob"))
  expect_error(getNHLSchedule(20202021))
})

test_that("Schedules are ok", {
  vcr::use_cassette("nhl-schedule", {
    sched <- getNHLSchedule("20242025")
    expect_true(is.data.frame(sched))
    expect_equal(ncol(sched), 6)
    expect_equal(
      colnames(sched),
      c("Date", "HomeTeam", "AwayTeam", "GameID", "GameType", "GameStatus")
    )
    expect_true(all(sched$GameType %in% c("R", "P")))
  })
})

test_that("getNHLScores validates season input", {
  expect_error(getNHLScores("not-a-season"))
  expect_error(getNHLScores(12345))
})

test_that("Scores are OK", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)

  local_mocked_bindings(
    get_xg = function(gameIds) {
      data.frame(
        GameID = as.integer(gameIds),
        HomexG = 4.3,
        AwayxG = 3.1
      )
    },
    .package = "HockeyModel"
  )

  vcr::use_cassette("get-nhl-scores-single-game-columns", {
    score <- getNHLScores(2020020001, progress = FALSE)
    expect_true(is.data.frame(score))
    expect_equal(nrow(score), 1)
    required_cols <- c(
      "Date",
      "HomeTeam",
      "AwayTeam",
      "GameID",
      "HomeGoals",
      "AwayGoals",
      "OTStatus",
      "GameType",
      "GameStatus",
      "Result",
      "HomexG",
      "AwayxG"
    )
    expect_true(all(required_cols %in% colnames(score)))
    expect_equal(score$GameID[[1]], 2020020001)
    expect_equal(score$GameStatus[[1]], "Final")
    expect_true(is.numeric(score$HomexG[[1]]) || is.na(score$HomexG[[1]]))
    expect_true(is.numeric(score$AwayxG[[1]]) || is.na(score$AwayxG[[1]]))
  })
})

test_that("get_xg() uses component parser results", {
  local_mocked_bindings(
    load_or_get_nst = function(gid) {
      data.frame(
        h_a = c("home", "away"),
        xgf_all = c(2.5, 1.8),
        gf_all = c(3, 2),
        cf_all = c(50, 45),
        xgf_pk = c(0.2, 0.1),
        gf_pk = c(0, 0),
        cf_pk = c(4, 3),
        xgf_pp = c(0.8, 0.6),
        gf_pp = c(1, 1),
        cf_pp = c(10, 8)
      )
    },
    .package = "HockeyModel"
  )

  xg <- get_xg(2020020001)
  expect_s3_class(xg, "data.frame")
  expect_equal(xg$GameID[[1]], 2020020001L)
  expect_equal(xg$HomexG[[1]], 2.5)
  expect_equal(xg$AwayxG[[1]], 1.8)
})

test_that("games_today returns NULL or data frame", {
  sched <- HockeyModel::scores
  sched <- sched[sched$Date > as.Date("2019-10-01"), ]
  sched <- sched[sched$Date < as.Date("2019-12-31"), ]

  expect_message(
    games_today(date = as.Date("2019-11-01")),
    "Games on today aren't present in Schedule"
  )
  expect_null(games_today(date = as.Date("2019-11-01")))

  today_games <- games_today(date = as.Date("2019-11-01"), schedule = sched)
  expect_true(is.data.frame(today_games))
  expect_true(nrow(today_games) > 0)
  expect_true(all(today_games$Date == as.Date("2019-11-01")))
})

test_that("games_today validates date input", {
  expect_error(games_today(date = "not-a-date"))
  expect_error(games_today(date = 12345))
})

test_that("Series is ok", {
  vcr::use_cassette("get-series", {
    series <- getAPISeries("20182019")
    expect_true(is.data.frame(series))
    expect_equal(nrow(series), 15)
    expect_true(all(
      c(
        "Round",
        "Series",
        "HomeTeam",
        "AwayTeam",
        "HomeWins",
        "AwayWins",
        "HomeSeed",
        "AwaySeed",
        "Status"
      ) %in%
        colnames(series)
    ))
    expect_true(all(series$Status == "Complete"))
  })
})

test_that("getAPISeries handles integer input", {
  expect_error(getAPISeries(season = 1))
})

test_that("Season Dates & Binaries work", {
  vcr::use_cassette("utils", {
    expect_visible(inRegularSeason())
    expect_visible(inPlayoffs())
    expect_visible(inOffSeason())
    expect_false(inOffSeason("2018-12-02"))
    expect_equal(inRegularSeason("2018-12-02", boolean = FALSE), "20182019")
    expect_false(inPlayoffs("2018-12-02", boolean = FALSE))
    expect_true(inOffSeason("2018-08-01"))
    expect_equal(getSeasonEndDate(season = "20182019"), as.Date("2019-06-12"))
    expect_equal(HockeyModel::getSeason("2018-10-05"), "20182019")
    expect_equal(HockeyModel::getSeason("2019-02-15"), "20182019")
  })
})

test_that("SeasonID gets seasons ok", {
  vcr::use_cassette("current-season", {
    season <- getCurrentSeason8()
    expect_true(is.null(season) || is.character(season))
    if (!is.null(season)) {
      expect_equal(nchar(season), 8)
      expect_true(grepl("^\\d{8}$", season))
      first_four <- as.integer(substr(season, 1, 4))
      last_four <- as.integer(substr(season, 5, 8))
      expect_equal(last_four - first_four, 1)
      expect_equal(getSeason("2018-12-02"), "20182019")
      expect_null(getSeason("2018-09-01"))
    }
  })
})
