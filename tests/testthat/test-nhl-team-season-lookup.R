# #87: season lookups must work from the local schedule when the API is down,
# and repeated lookups within a session must not re-fetch independently.
offline_nhl_schedule <- data.frame(
  Date = c(as.Date("2019-04-06"), as.Date("2019-05-30")),
  HomeTeam = c("Team A", "Team B"),
  AwayTeam = c("Team B", "Team A"),
  GameID = c(2018012345, 2018999999), # both in the 2018-19 season (YYYY prefix)
  GameType = c("R", "P"),
  GameStatus = c("Final", "FUT"),
  stringsAsFactors = FALSE
)

test_that("season lookups derive dates from the local schedule when offline (#87)", {
  local_mocked_bindings(
    .fetch_nhl_seasons = function() NULL,
    .package = "HockeyModel"
  )

  expect_equal(
    getSeasonStartDate(season = "20182019", schedule = offline_nhl_schedule),
    as.Date("2019-04-06")
  )
  expect_equal(
    getSeasonEndDate(season = "20182019", schedule = offline_nhl_schedule),
    as.Date("2019-05-30")
  )
  # The current season is the latest season present in the local schedule.
  expect_equal(
    getCurrentSeason8(schedule = offline_nhl_schedule),
    "20182019"
  )
  # A date inside a scheduled season maps to that season.
  expect_equal(
    getSeason(as.Date("2019-04-20"), schedule = offline_nhl_schedule),
    "20182019"
  )
  # A date between seasons has no local match.
  expect_null(getSeason(as.Date("2019-06-15"), schedule = offline_nhl_schedule))
})

test_that("season lookups error informatively when offline with an empty schedule (#87)", {
  local_mocked_bindings(
    .fetch_nhl_seasons = function() NULL,
    .package = "HockeyModel"
  )

  expect_error(getSeasonStartDate(season = "20182019"), "no network access")
  expect_error(getSeasonEndDate(season = "20182019"), "no network access")
})

test_that("season lookups cache the API response within a session (#87)", {
  fetches <- new.env()
  fetches$n <- 0L
  mock_seasons <- data.frame(
    id = c(20182019, 20192020),
    startDate = as.Date(c("2018-10-03", "2019-10-02")),
    endDate = as.Date(c("2019-06-12", "2020-08-30"))
  )

  # Caching is disabled under testthat, so re-enable it for this test only.
  withr::local_envvar(TESTTHAT = "")

  # Mock the uncached HTTP seam so the session cache is what gets exercised.
  local_mocked_bindings(
    .fetch_nhl_seasons_http = function() {
      fetches$n <- fetches$n + 1L
      mock_seasons
    },
    .package = "HockeyModel"
  )

  clear_nhl_seasons_cache()

  expect_equal(getSeasonStartDate(season = "20182019"), as.Date("2018-10-03"))
  expect_equal(getSeasonEndDate(season = "20182019"), as.Date("2019-06-12"))
  expect_equal(getCurrentSeason8(), "20192020")
  # All three lookups hit the API exactly once, not once each.
  expect_equal(fetches$n, 1L)

  clear_nhl_seasons_cache()
  getCurrentSeason8()
  expect_equal(fetches$n, 2L)
})

mock_nhl_seasons <- data.frame(
  id = c(20182019, 20192020),
  startDate = as.Date(c("2018-10-03", "2019-10-02")),
  regularSeasonEndDate = as.Date(c("2019-04-07", "2020-04-05")),
  endDate = as.Date(c("2019-06-12", "2020-08-30"))
)

mock_nhl_schedule <- data.frame(
  Date = c(as.Date("2018-10-03"), as.Date("2019-04-07")),
  HomeTeam = c("Team A", "Team B"),
  AwayTeam = c("Team B", "Team A"),
  GameID = c(1L, 2L),
  GameType = c("R", "R"),
  GameStatus = c("Final", "Scheduled"),
  stringsAsFactors = FALSE
)

mock_nhl_schedule_playoffs <- rbind(
  mock_nhl_schedule,
  data.frame(
    Date = c(as.Date("2019-04-13"), as.Date("2019-06-12")),
    HomeTeam = c("Team A", "Team B"),
    AwayTeam = c("Team B", "Team A"),
    GameID = c(3L, 4L),
    GameType = c("P", "P"),
    GameStatus = c("Final", "Scheduled"),
    stringsAsFactors = FALSE
  )
)

empty_nhl_schedule <- data.frame(
  Date = as.Date(character()),
  HomeTeam = character(),
  AwayTeam = character(),
  GameID = integer(),
  GameType = character(),
  GameStatus = character()
)

test_that("Season Dates & Binaries work (#43)", {
  vcr::use_cassette("utils", {
    expect_visible(inRegularSeason(league = "NHL"))
    expect_visible(inPlayoffs(league = "NHL"))
    expect_visible(inOffSeason(league = "NHL"))
    expect_false(inOffSeason("2018-12-02", league = "NHL"))
    expect_equal(
      inRegularSeason("2018-12-02", boolean = FALSE, league = "NHL"),
      "20182019"
    )
    expect_false(inPlayoffs("2018-12-02", boolean = FALSE, league = "NHL"))
    expect_true(inOffSeason("2018-08-01", league = "NHL"))
    expect_equal(getSeasonEndDate(season = "20182019"), as.Date("2019-06-12"))
    expect_equal(HockeyModel::getSeason("2018-10-05"), "20182019")
    expect_equal(HockeyModel::getSeason("2019-02-15"), "20182019")
  })
})

test_that(".in_regular_season_nhl() uses API season data when available (#43)", {
  local_mocked_bindings(
    .fetch_nhl_seasons = function() mock_nhl_seasons,
    .package = "HockeyModel"
  )

  expect_true(.in_regular_season_nhl(date = as.Date("2018-12-02")))
  expect_equal(
    .in_regular_season_nhl(date = as.Date("2018-12-02"), boolean = FALSE),
    "20182019"
  )
  # Between regular season end and season end is playoffs, not regular season
  expect_false(.in_regular_season_nhl(date = as.Date("2019-05-01")))
  expect_false(
    .in_regular_season_nhl(date = as.Date("2019-05-01"), boolean = FALSE)
  )
})

test_that(".in_playoffs_nhl() uses API season data when available (#43)", {
  local_mocked_bindings(
    .fetch_nhl_seasons = function() mock_nhl_seasons,
    .package = "HockeyModel"
  )

  expect_true(.in_playoffs_nhl(date = as.Date("2019-05-01")))
  expect_equal(
    .in_playoffs_nhl(date = as.Date("2019-05-01"), boolean = FALSE),
    "20182019"
  )
  expect_false(.in_playoffs_nhl(date = as.Date("2018-12-02")))
})

test_that(".in_off_season_nhl() uses API season data when available (#43)", {
  local_mocked_bindings(
    .fetch_nhl_seasons = function() mock_nhl_seasons,
    .package = "HockeyModel"
  )

  expect_true(.in_off_season_nhl(date = as.Date("2019-08-01")))
  expect_false(.in_off_season_nhl(date = as.Date("2018-12-02")))
  expect_false(.in_off_season_nhl(date = as.Date("2019-05-01")))
})

test_that(".in_regular_season_nhl() falls back to the schedule when the API fails (#43)", {
  local_mocked_bindings(
    .fetch_nhl_seasons = function() NULL,
    .package = "HockeyModel"
  )

  expect_true(
    .in_regular_season_nhl(
      date = as.Date("2018-12-02"),
      schedule = mock_nhl_schedule
    )
  )
  expect_equal(
    .in_regular_season_nhl(
      date = as.Date("2018-12-02"),
      boolean = FALSE,
      schedule = mock_nhl_schedule
    ),
    "20182019"
  )
  # Before the first scheduled regular season game
  expect_false(
    .in_regular_season_nhl(
      date = as.Date("2018-09-01"),
      schedule = mock_nhl_schedule
    )
  )
  # Past the last scheduled regular season game
  expect_false(
    .in_regular_season_nhl(
      date = as.Date("2019-05-01"),
      schedule = mock_nhl_schedule
    )
  )
})

test_that(".in_playoffs_nhl() falls back to the schedule when the API fails (#43)", {
  local_mocked_bindings(
    .fetch_nhl_seasons = function() NULL,
    .package = "HockeyModel"
  )

  expect_true(
    .in_playoffs_nhl(
      date = as.Date("2019-05-01"),
      schedule = mock_nhl_schedule_playoffs
    )
  )
  expect_equal(
    .in_playoffs_nhl(
      date = as.Date("2019-05-01"),
      boolean = FALSE,
      schedule = mock_nhl_schedule_playoffs
    ),
    "20182019"
  )
  expect_false(
    .in_playoffs_nhl(
      date = as.Date("2018-12-02"),
      schedule = mock_nhl_schedule_playoffs
    )
  )
  # No playoff games in the schedule at all
  expect_false(
    .in_playoffs_nhl(date = as.Date("2019-05-01"), schedule = mock_nhl_schedule)
  )
})

test_that(".in_off_season_nhl() falls back to the schedule when the API fails (#43)", {
  local_mocked_bindings(
    .fetch_nhl_seasons = function() NULL,
    .package = "HockeyModel"
  )

  expect_true(
    .in_off_season_nhl(
      date = as.Date("2018-09-01"),
      schedule = mock_nhl_schedule
    )
  )
  expect_true(
    .in_off_season_nhl(
      date = as.Date("2019-07-01"),
      schedule = mock_nhl_schedule_playoffs
    )
  )
  expect_false(
    .in_off_season_nhl(
      date = as.Date("2018-12-02"),
      schedule = mock_nhl_schedule
    )
  )
  expect_true(.in_off_season_nhl(
    date = Sys.Date(),
    schedule = empty_nhl_schedule
  ))
})

test_that("NHL season internals validate their inputs (#43)", {
  expect_error(.in_regular_season_nhl(date = "not-a-date"), "date")
  expect_error(.in_playoffs_nhl(date = 12345), "date")
  expect_error(.in_off_season_nhl(date = TRUE), "date")
  expect_error(.in_regular_season_nhl(boolean = c(TRUE, FALSE)), "boolean")
  expect_error(.in_playoffs_nhl(boolean = NA), "boolean")
})

test_that("camelCase aliases mirror the snake_case NHL internals (#43)", {
  expect_identical(.inRegularSeasonNHL, .in_regular_season_nhl)
  expect_identical(.inPlayoffsNHL, .in_playoffs_nhl)
  expect_identical(.inOffSeasonNHL, .in_off_season_nhl)
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

test_that("getSeasonStartDate returns dates", {
  vcr::use_cassette("season-start-date", {
    start <- getSeasonStartDate(season = "20182019")
    expect_s3_class(start, "Date")
    expect_equal(start, as.Date("2018-10-03"))

    latest <- getSeasonStartDate()
    expect_s3_class(latest, "Date")
  })

  vcr::use_cassette("season-start-date", {
    expect_error(getSeasonStartDate(season = "not-a-season"))
  })
})

test_that(".getNumGames returns a game count", {
  vcr::use_cassette("num-games", {
    n <- .getNumGames(season = "20182019")
    expect_true(is.numeric(n))
    expect_length(n, 1)
    expect_true(n > 0)
  })

  expect_error(.getNumGames(season = "not-a-season"))
})
