context("test-nhl-api-fetch")

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

test_that("load_or_get_nst() fetches and caches a Natural Stat Trick report", {
  cache_file <- withr::local_tempfile(fileext = ".csv")

  vcr::use_cassette("nst-report", {
    nst <- load_or_get_nst(2020020001, cache_path = cache_file)
  })

  expect_s3_class(nst, "data.frame")
  expect_setequal(nst$h_a, c("home", "away"))
  expect_true(all(c("xgf_all", "gf_all", "cf_all") %in% colnames(nst)))
  # Result should now be served from the cache file, not the network.
  expect_true(file.exists(cache_file))

  nst_cached <- load_or_get_nst(2020020001, cache_path = cache_file)
  expect_equal(nst_cached$xgf_all, nst$xgf_all)
})

test_that("games_today returns NULL or data frame", {
  sched <- HockeyModel::scores
  sched <- sched[sched$Date > as.Date("2019-10-01"), ]
  sched <- sched[sched$Date < as.Date("2019-12-31"), ]

  vcr::use_cassette("games-today", {
    today_missing <- NULL
    expect_message(
      today_missing <- games_today(date = as.Date("2019-11-01")),
      "Games on today aren't present in Schedule"
    )
    expect_null(today_missing)
  })

  vcr::use_cassette("games-today", {
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

test_that("getNHLScores derives gameIDs from schedule when gameIDs = NULL (#3.4)", {
  schedule <- data.frame(
    Date = as.Date(c("2020-01-01", "2020-01-02")),
    GameID = c(2020020001, 2020020002)
  )

  local_mocked_bindings(
    gameIDValidator = function(x) rep(TRUE, length(x)),
    nhl_boxscore = function(gid) {
      list(
        gameState = "OFF",
        gameDate = "2020-01-01",
        id = as.numeric(gid),
        homeTeam = list(
          placeName = list("Home"),
          commonName = list("Team"),
          score = 3
        ),
        awayTeam = list(
          placeName = list("Away"),
          commonName = list("Team"),
          score = 1
        ),
        periodDescriptor = list(number = 3)
      )
    },
    get_xg = function(gameIds) {
      data.frame(GameID = as.integer(gameIds), HomexG = 1, AwayxG = 1)
    },
    .package = "HockeyModel"
  )

  score <- getNHLScores(gameIDs = NULL, schedule = schedule, progress = FALSE)
  expect_true(is.data.frame(score))
  expect_equal(sort(score$GameID), sort(schedule$GameID))
})

test_that("getNHLScores detects shootout games (#3.7)", {
  local_mocked_bindings(
    gameIDValidator = function(x) rep(TRUE, length(x)),
    nhl_boxscore = function(gid) {
      list(
        gameState = "OFF",
        gameDate = "2020-01-01",
        id = as.numeric(gid),
        homeTeam = list(
          placeName = list("Home"),
          commonName = list("Team"),
          score = 3
        ),
        awayTeam = list(
          placeName = list("Away"),
          commonName = list("Team"),
          score = 2
        ),
        periodDescriptor = list(number = 5)
      )
    },
    get_xg = function(gameIds) {
      data.frame(GameID = as.integer(gameIds), HomexG = 1, AwayxG = 1)
    },
    .package = "HockeyModel"
  )

  score <- getNHLScores(gameIDs = 2020020001, progress = FALSE)
  expect_equal(score$OTStatus, "SO")
  expect_equal(score$Result, 0.6)
})

test_that("getNHLScores errors on unrecognized OTStatus values (#3.9)", {
  local_mocked_bindings(
    gameIDValidator = function(x) rep(TRUE, length(x)),
    nhl_boxscore = function(gid) {
      list(
        gameState = "OFF",
        gameDate = "2020-01-01",
        id = as.numeric(gid),
        homeTeam = list(
          placeName = list("Home"),
          commonName = list("Team"),
          score = 3
        ),
        awayTeam = list(
          placeName = list("Away"),
          commonName = list("Team"),
          score = 2
        ),
        # A missing/unparseable period number falls through every case_when
        # branch and must error rather than silently produce NA.
        periodDescriptor = list(number = NA_real_)
      )
    },
    get_xg = function(gameIds) {
      data.frame(GameID = as.integer(gameIds), HomexG = 1, AwayxG = 1)
    },
    .package = "HockeyModel"
  )

  expect_error(
    getNHLScores(gameIDs = 2020020001, progress = FALSE),
    "unrecognized"
  )
})

test_that("getNHLScores rejects tied final scores (#3.10)", {
  local_mocked_bindings(
    gameIDValidator = function(x) rep(TRUE, length(x)),
    nhl_boxscore = function(gid) {
      list(
        gameState = "OFF",
        gameDate = "2020-01-01",
        id = as.numeric(gid),
        homeTeam = list(
          placeName = list("Home"),
          commonName = list("Team"),
          score = 2
        ),
        awayTeam = list(
          placeName = list("Away"),
          commonName = list("Team"),
          score = 2
        ),
        periodDescriptor = list(number = 3)
      )
    },
    get_xg = function(gameIds) {
      data.frame(GameID = as.integer(gameIds), HomexG = 1, AwayxG = 1)
    },
    .package = "HockeyModel"
  )

  expect_error(
    getNHLScores(gameIDs = 2020020001, progress = FALSE),
    "tied final scores"
  )
})

test_that("getNHLScores skips xG lookup when no final scores are retrieved (#3.11)", {
  called <- FALSE
  local_mocked_bindings(
    gameIDValidator = function(x) rep(TRUE, length(x)),
    nhl_boxscore = function(gid) {
      list(gameState = "FUT", gameScheduleState = "OK")
    },
    get_xg = function(gameIds) {
      called <<- TRUE
      data.frame(GameID = integer(0), HomexG = numeric(0), AwayxG = numeric(0))
    },
    .package = "HockeyModel"
  )

  score <- suppressWarnings(getNHLScores(
    gameIDs = 2020020001,
    progress = FALSE
  ))
  expect_null(score)
  expect_false(called)
})
