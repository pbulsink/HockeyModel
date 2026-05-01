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
  vcr::use_cassette("pwhl-seasons", {
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
})

test_that("pwhl_games_today returns NULL on no-game date", {
  empty_schedule <- HockeyModel::pwhlSchedule
  result <- pwhl_games_today(
    schedule = empty_schedule,
    date = as.Date("2024-01-01")
  )
  expect_null(result)
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
    scores <- getPWHLScores(137L, progress = FALSE)
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
    expect_equal(scores$GameID, 137L)
    expect_equal(scores$GameStatus, "Final")
    expect_equal(scores$HomeGoals, 2L)
    expect_equal(scores$AwayGoals, 1L)
    expect_equal(scores$OTStatus, "")
    expect_equal(scores$GameType, "R")
  })
})

test_that("pwhlTeamColours has correct structure", {
  tc <- HockeyModel::pwhlTeamColours
  expect_true(is.data.frame(tc))
  expect_equal(nrow(tc), 6L)
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

test_that("pwhlScores has correct empty structure", {
  sc <- HockeyModel::pwhlScores
  expect_true(is.data.frame(sc))
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
      colnames(sc)
  ))
})

test_that("pwhl_get_long_team and pwhl_get_short_team round-trip", {
  tc <- HockeyModel::pwhlTeamColours
  codes <- tc$ShortCode
  teams <- tc$Team

  long <- pwhl_get_long_team(codes)
  expect_equal(long, teams)

  short <- pwhl_get_short_team(teams)
  expect_equal(short, codes)
})

test_that("pwhl_get_long_team returns NA for unknown code", {
  result <- pwhl_get_long_team("XYZ")
  expect_true(is.na(result))
})

# ── getPWHLPlayoffSeries tests ────────────────────────────────────────────────

test_that("getPWHLPlayoffSeries returns NULL when no playoff games", {
  sched_no_playoffs <- data.frame(
    Date = as.Date("2024-01-01"),
    HomeTeam = "Toronto Sceptres",
    AwayTeam = "Boston Fleet",
    GameID = 1L,
    GameType = "R",
    GameStatus = "Final",
    stringsAsFactors = FALSE
  )
  scores_empty <- data.frame(
    Date = as.Date(character(0)),
    HomeTeam = character(0),
    AwayTeam = character(0),
    GameID = integer(0),
    HomeGoals = integer(0),
    AwayGoals = integer(0),
    OTStatus = character(0),
    GameType = character(0),
    GameStatus = character(0),
    stringsAsFactors = FALSE
  )
  result <- getPWHLPlayoffSeries(
    scores = scores_empty,
    schedule = sched_no_playoffs
  )
  expect_null(result)
})

test_that("getPWHLPlayoffSeries home team is team with first home game", {
  # Game 1: Boston at Toronto (Toronto is home first)
  # Game 2: Boston at Toronto again
  # Game 3: Toronto at Boston (away for Toronto)
  playoff_schedule <- data.frame(
    Date = as.Date(c("2024-04-01", "2024-04-03", "2024-04-05")),
    HomeTeam = c("Toronto Sceptres", "Toronto Sceptres", "Boston Fleet"),
    AwayTeam = c("Boston Fleet", "Boston Fleet", "Toronto Sceptres"),
    GameID = c(200L, 201L, 202L),
    GameType = c("P", "P", "P"),
    GameStatus = c("Final", "Final", "Scheduled"),
    stringsAsFactors = FALSE
  )
  playoff_scores <- data.frame(
    Date = as.Date(c("2024-04-01", "2024-04-03")),
    HomeTeam = c("Toronto Sceptres", "Toronto Sceptres"),
    AwayTeam = c("Boston Fleet", "Boston Fleet"),
    GameID = c(200L, 201L),
    HomeGoals = c(3L, 2L),
    AwayGoals = c(1L, 4L),
    OTStatus = c("", ""),
    GameType = c("P", "P"),
    GameStatus = c("Final", "Final"),
    stringsAsFactors = FALSE
  )
  result <- getPWHLPlayoffSeries(
    scores = playoff_scores,
    schedule = playoff_schedule
  )
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1L)
  # Toronto had the first home game, so Toronto is the series home team
  expect_equal(result$HomeTeam, "Toronto Sceptres")
  expect_equal(result$AwayTeam, "Boston Fleet")
  expect_equal(result$HomeWins, 1L)
  expect_equal(result$AwayWins, 1L)
  expect_equal(result$Status, "Ongoing")
})

test_that("getPWHLPlayoffSeries marks series complete at 3 wins (best-of-5)", {
  # Toronto wins 3 games to sweep the best-of-5 series
  # Games 1-2 at Toronto (home), game 3 at Boston (away for Toronto)
  playoff_schedule <- data.frame(
    Date = as.Date(c("2024-04-01", "2024-04-03", "2024-04-05")),
    HomeTeam = c("Toronto Sceptres", "Toronto Sceptres", "Boston Fleet"),
    AwayTeam = c("Boston Fleet", "Boston Fleet", "Toronto Sceptres"),
    GameID = c(200L, 201L, 202L),
    GameType = c("P", "P", "P"),
    GameStatus = c("Final", "Final", "Final"),
    stringsAsFactors = FALSE
  )
  # Toronto wins all 3: games 1 and 2 at home, game 3 on the road
  playoff_scores <- data.frame(
    Date = as.Date(c("2024-04-01", "2024-04-03", "2024-04-05")),
    HomeTeam = c("Toronto Sceptres", "Toronto Sceptres", "Boston Fleet"),
    AwayTeam = c("Boston Fleet", "Boston Fleet", "Toronto Sceptres"),
    GameID = c(200L, 201L, 202L),
    HomeGoals = c(3L, 2L, 1L),
    AwayGoals = c(1L, 1L, 4L),
    OTStatus = c("", "", ""),
    GameType = c("P", "P", "P"),
    GameStatus = c("Final", "Final", "Final"),
    stringsAsFactors = FALSE
  )
  result <- getPWHLPlayoffSeries(
    scores = playoff_scores,
    schedule = playoff_schedule
  )
  expect_equal(result$HomeWins, 3L) # Toronto wins all 3 (2 home + 1 away)
  expect_equal(result$AwayWins, 0L) # Boston wins 0
  expect_equal(result$Status, "Complete")
})
