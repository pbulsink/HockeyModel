test_that("getPWHLSeasons returns correct structure", {
  vcr::use_cassette("pwhl-seasons", {
    seasons <- getPWHLSeasons()
    expect_true(is.data.frame(seasons))
    expect_true(nrow(seasons) >= 1)
    expect_true(all(
      c(
        "id",
        "name",
        "shortname",
        "career",
        "playoff",
        "start_date",
        "end_date"
      ) %in%
        colnames(seasons)
    ))
    expect_true(is.integer(seasons$id))
    expect_true(inherits(seasons$start_date, "Date"))
    expect_true(inherits(seasons$end_date, "Date"))
    # Verify known season data from cassette
    expect_true(5L %in% seasons$id)
    expect_true("2024-25 Regular Season" %in% seasons$name)
    reg_season <- seasons[seasons$id == 5L, ]
    expect_equal(reg_season$career, 1L)
    expect_equal(reg_season$playoff, 0L)
  })
})

test_that("getCurrentPWHLSeason returns valid season", {
  vcr::use_cassette("pwhl-seasons", {
    season <- getCurrentPWHLSeason()
    expect_false(is.null(season))
    expect_true(is.numeric(season) || is.integer(season))
    expect_true(season > 0)
  })
})

test_that("getPWHLSchedule validates season input", {
  expect_error(getPWHLSchedule("not-a-number"))
  expect_error(getPWHLSchedule(NULL))
  expect_error(getPWHLSchedule(c(1L, 2L)))
})

test_that("getPWHLSchedule returns correct structure", {
  vcr::use_cassette("pwhl-schedule", {
    sched <- getPWHLSchedule(season = 5L)
    expect_true(is.data.frame(sched))
    expect_true(nrow(sched) >= 1)
    expect_equal(
      colnames(sched),
      c("Date", "HomeTeam", "AwayTeam", "GameID", "GameType", "GameStatus")
    )
    expect_true(all(sched$GameType %in% c("R", "P")))
    expect_true(inherits(sched$Date, "Date"))
    expect_true(is.integer(sched$GameID))
    # Check teams match pwhlTeamColours
    valid_teams <- HockeyModel::pwhlTeamColours$Team
    known_home <- sched$HomeTeam[!is.na(sched$HomeTeam)]
    expect_true(all(known_home %in% valid_teams))
  })
})

test_that("pwhl_games_today validates date input", {
  expect_error(pwhl_games_today(date = "not-a-date"))
  expect_error(pwhl_games_today(date = 20240101))
})

test_that("pwhl_games_today returns games from schedule", {
  test_schedule <- data.frame(
    Date = as.Date(c("2024-11-29", "2024-11-29", "2024-12-01")),
    HomeTeam = c("Toronto Sceptres", "Ottawa Charge", "Boston Fleet"),
    AwayTeam = c("Boston Fleet", "Minnesota Frost", "Toronto Sceptres"),
    GameID = c(137L, 138L, 139L),
    GameType = c("R", "R", "R"),
    GameStatus = c("Scheduled", "Scheduled", "Scheduled"),
    stringsAsFactors = FALSE
  )
  games <- pwhl_games_today(
    schedule = test_schedule,
    date = as.Date("2024-11-29")
  )
  expect_true(is.data.frame(games))
  expect_equal(nrow(games), 2L)
  expect_true(all(games$Date == as.Date("2024-11-29")))
})

test_that("getPWHLScores validates empty gameIDs", {
  expect_error(getPWHLScores(NULL))
  expect_error(getPWHLScores(integer(0)))
  expect_error(getPWHLScores(c(NA_integer_, NA_integer_)))
})

test_that("getPWHLScores returns correct structure for finished game", {
  vcr::use_cassette("pwhl-game-summary", {
    scores <- getPWHLScores(137, progress = FALSE)
    expect_true(is.data.frame(scores))
    expect_equal(nrow(scores), 1L)
    expect_true(all(
      c(
        "Date",
        "HomeTeam",
        "AwayTeam",
        "GameID",
        "HomeGoals",
        "AwayGoals",
        "OTStatus",
        "GameType",
        "GameStatus"
      ) %in%
        colnames(scores)
    ))
    expect_equal(scores$GameID, 137)
    expect_equal(scores$GameStatus, "Final")
    expect_equal(scores$HomeGoals, 4)
    expect_equal(scores$AwayGoals, 1)
    expect_equal(scores$OTStatus, "")
    expect_equal(scores$GameType, "")
  })
})

test_that("pwhlTeamColours has correct structure", {
  tc <- HockeyModel::pwhlTeamColours
  expect_true(is.data.frame(tc))
  expect_equal(nrow(tc), 8L)
  expect_true(all(
    c(
      "Team",
      "Hex",
      "AltHex",
      "Hashtag",
      "ShortCode",
      "Division",
      "Conference",
      "PWHLID"
    ) %in%
      colnames(tc)
  ))
  expect_true(all(tc$Division == "PWHL"))
  expect_true(all(tc$Conference == "PWHL"))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", tc$Hex)))
})

test_that("pwhlSchedule has correct empty structure", {
  sched <- HockeyModel::pwhlSchedule
  expect_true(is.data.frame(sched))
  expect_equal(
    colnames(sched),
    c("Date", "HomeTeam", "AwayTeam", "GameID", "GameType", "GameStatus")
  )
})

test_that("getLongTeam and pwhl_get_short_team works for PWHL", {
  tc <- HockeyModel::pwhlTeamColours
  codes <- tc$ShortCode
  teams <- tc$Team

  long <- getLongTeam(codes, tc)
  expect_equal(long, teams)

  short <- getShortTeam(teams, tc)
  expect_equal(short, codes)
})

test_that("pwhl_get_long_team returns NA for unknown code", {
  result <- getLongTeam("XYZ", HockeyModel::pwhlTeamColours)
  expect_true(is.na(result))
})

# ── getPWHLPlayoffSeries tests ────────────────────────────────────────────────

test_that("getPWHLPlayoffSeries returns NULL when API returns no brackets", {
  vcr::use_cassette("pwhl-playoff-empty", {
    result <- getPWHLPlayoffSeries(season_id = 9999L)
    expect_null(result)
  })
})

test_that("getPWHLPlayoffSeries returns correct structure from API", {
  vcr::use_cassette("pwhl-playoff-brackets", {
    result <- getPWHLPlayoffSeries(season_id = 3L)
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 2L)
    expect_equal(
      colnames(result),
      c("HomeTeam", "AwayTeam", "HomeWins", "AwayWins", "Status")
    )
    expect_true(all(result$Status %in% c("Ongoing", "Complete")))
    expect_true(is.integer(result$HomeWins))
    expect_true(is.integer(result$AwayWins))
  })
})

test_that("getPWHLPlayoffSeries correctly marks ongoing and complete series", {
  vcr::use_cassette("pwhl-playoff-brackets", {
    result <- getPWHLPlayoffSeries(season_id = 3L)
    ongoing <- result[result$Status == "Ongoing", ]
    complete <- result[result$Status == "Complete", ]
    expect_equal(nrow(ongoing), 0)
    expect_equal(nrow(complete), 2)
    expect_equal(complete$HomeTeam[1], "Toronto Sceptres")
    expect_equal(complete$AwayTeam[2], "Boston Fleet")
    expect_equal(complete$HomeWins, c(2, 0))
    expect_equal(complete$AwayWins, c(3, 3))
  })
})
