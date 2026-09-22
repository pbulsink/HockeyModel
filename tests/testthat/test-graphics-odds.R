test_that("Today Odds plot OK", {
  local_mocked_bindings(
    todayDC = function(...) {
      data.frame(
        Date = as.Date("2019-11-01"),
        GameID = 2019020196,
        HomeTeam = "New Jersey Devils",
        AwayTeam = "Philadelphia Flyers",
        HomeWin = 0.45,
        AwayWin = 0.35,
        Draw = 0.20
      )
    },
    getCurrentSeason8 = function() "20192020",
    .package = "HockeyModel"
  )

  p <- suppressWarnings(plot_odds_today(today = as.Date("2019-11-01")))
  expect_true(is.null(p) || ggplot2::is_ggplot(p))
  if (is.null(p)) {
    skip("No games available for the requested date")
  }
  expect_identical(p$labels$title, "Predictions for Today's NHL Games")
  expect_identical(p$labels$y, "Result Odds")
  expect_identical(p$labels$x, "")

  if (!requireNamespace('gt')) {
    expect_error(
      suppressWarnings(daily_odds_table(today = as.Date("2019-11-01"))),
      "Package gt is required"
    )
    skip("No `gt` package.")
  }
  p <- suppressWarnings(daily_odds_table(today = as.Date("2019-11-01")))
  expect_true("gt_tbl" %in% class(p))
  expect_identical(p$`_heading`$title, "NHL Game Odds")
})

test_that("Series Graphics are OK", {
  series <- data.frame(
    HomeTeam = c(
      "Tampa Bay Lightning",
      "Boston Bruins",
      "Washington Capitals"
    ),
    AwayTeam = c(
      "Columbus Blue Jackets",
      "Toronto Maple Leafs",
      "Carolina Hurricanes"
    ),
    HomeWins = c(0, 3, 3),
    AwayWins = c(4, 3, 2)
  )
  p <- suppressWarnings(plot_playoff_series_odds(series = series))
  expect_true(ggplot2::is_ggplot(p))
  expect_identical(p$labels$title, "Predictions for NHL Playoff Series")
  expect_identical(p$labels$y, "Series Odds")
  expect_identical(p$labels$x, "")
})

# ============ todayOddsPlot tests ============
test_that("todayOddsPlot executes without error", {
  sched <- HockeyModel::scores
  sched <- sched[sched$Date > as.Date("2019-10-01"), ]
  sched <- sched[sched$Date < as.Date("2019-12-31"), ]
  vcr::use_cassette("todayOdds", {
    p <- suppressWarnings(todayOddsPlot(
      date = as.Date("2019-11-01"),
      schedule = sched,
      league = "nhl"
    ))
    expect_true(ggplot2::is_ggplot(p))
  })
})

# ============ todayOddsPlot tests ============
test_that("todayOddsPlot executes without error", {
  mock_schedule <- HockeyModel::schedule[
    HockeyModel::schedule$GameID %in% 2019020196,
  ]
  mock_scores <- HockeyModel::scores[
    HockeyModel::scores$Date <= as.Date("2019-11-01"),
  ]
  local_mocked_bindings(
    games_today = function(schedule, date, all_games = FALSE) {
      schedule[schedule$GameID %in% 2019020196, ]
    },
    todayDC = function(...) {
      data.frame(
        Date = as.Date("2019-11-01"),
        GameID = 2019020196,
        HomeTeam = "New Jersey Devils",
        AwayTeam = "Philadelphia Flyers",
        HomeWin = 0.45,
        AwayWin = 0.35,
        Draw = 0.20
      )
    },
    getCurrentSeason8 = function() "20192020",
    .package = "HockeyModel"
  )
  p <- todayOddsPlot(
    date = as.Date("2019-11-01"),
    schedule = mock_schedule,
    scores = mock_scores,
    league = "nhl"
  )
  expect_true(ggplot2::is_ggplot(p) || is.null(p))
})

# ============ plot_playoff_series_odds tests ============
test_that("plot_playoff_series_odds executes gracefully", {
  series <- structure(
    list(
      Round = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
      Series = 1:8,
      HomeTeam = c(
        "Buffalo Sabres",
        "Tampa Bay Lightning",
        "Carolina Hurricanes",
        "Pittsburgh Penguins",
        "Colorado Avalanche",
        "Dallas Stars",
        "Vegas Golden Knights",
        "Edmonton Oilers"
      ),
      AwayTeam = c(
        "Boston Bruins",
        "Montreal Canadiens",
        "Ottawa Senators",
        "Philadelphia Flyers",
        "Los Angeles Kings",
        "Minnesota Wild",
        "Utah Mammoth",
        "Anaheim Ducks"
      ),
      HomeWins = c(3L, 2L, 4L, 1L, 4L, 2L, 1L, 1L),
      AwayWins = c(1L, 2L, 0L, 3L, 0L, 2L, 2L, 3L),
      HomeSeed = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
      AwaySeed = c(4L, 3L, 4L, 3L, 4L, 3L, 4L, 3L),
      Status = c(
        "Ongoing",
        "Ongoing",
        "Complete",
        "Ongoing",
        "Complete",
        "Ongoing",
        "Ongoing",
        "Ongoing"
      )
    ),
    row.names = c(NA, 8L),
    class = "data.frame"
  )
  p <- plot_playoff_series_odds(series = series)
  expect_true(ggplot2::is_ggplot(p))
})
