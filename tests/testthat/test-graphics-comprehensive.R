context("test-graphics")

# ============ Basic graphics tests ============
test_that("plot_team_rating executes without error", {
  expect_error(plot_team_rating(), NA)
})

test_that("plot_team_rating returns ggplot object", {
  p <- plot_team_rating()
  expect_true(ggplot2::is_ggplot(p))
})

test_that("plot_team_rating has layers", {
  p <- plot_team_rating()
  expect_true(length(p$layers) > 0)
})

# ============ plot_pace_by_team tests ============
test_that("plot_pace_by_team executes without error", {
  tmpdir <- withr::local_tempdir()
  sc <- data.frame(
    Date = as.Date(c("2019-10-02", "2019-10-03")),
    HomeTeam = c("Toronto Maple Leafs", "Edmonton Oilers"),
    AwayTeam = c("Montreal Canadiens", "Vancouver Canucks"),
    Result = c(1, 0),
    GameType = c("R", "R"),
    GameID = c(1, 2)
  )
  preds <- data.frame(
    Team = unique(c(sc$HomeTeam, sc$AwayTeam)),
    meanPoints = c(100, 95, 90, 85),
    sdPoints = c(8, 8, 8, 8)
  )
  saveRDS(preds, file.path(tmpdir, "2019-10-01-predictions.RDS"))
  saveRDS(preds, file.path(tmpdir, "2019-10-03-predictions.RDS"))

  local_mocked_bindings(
    getSeasonStartDate = function(season = NULL) as.Date("2019-10-01"),
    getNumGames = function(season = NULL) 82,
    .package = "HockeyModel"
  )

  expect_error(
    suppressWarnings(plot_pace_by_team(
      graphic_dir = tmpdir,
      prediction_dir = tmpdir,
      scores = sc
    )),
    NA
  )
})

# ============ plot_pace_by_division tests ============
test_that("plot_pace_by_division executes without error", {
  tmpdir <- withr::local_tempdir()
  sc <- data.frame(
    Date = as.Date(c("2019-10-02", "2019-10-03")),
    HomeTeam = c("Toronto Maple Leafs", "Edmonton Oilers"),
    AwayTeam = c("Montreal Canadiens", "Vancouver Canucks"),
    Result = c(1, 0),
    GameType = c("R", "R"),
    GameID = c(1, 2)
  )
  preds <- data.frame(
    Team = unique(c(sc$HomeTeam, sc$AwayTeam)),
    meanPoints = c(100, 95, 90, 85),
    sdPoints = c(8, 8, 8, 8)
  )
  saveRDS(preds, file.path(tmpdir, "2019-10-01-predictions.RDS"))

  local_mocked_bindings(
    getSeasonStartDate = function(season = NULL) as.Date("2019-10-01"),
    getNumGames = function(season = NULL) 82,
    .package = "HockeyModel"
  )

  expect_error(
    suppressWarnings(plot_pace_by_division(
      graphic_dir = tmpdir,
      prediction_dir = tmpdir,
      scores = sc
    )),
    NA
  )
})

# ============ todayOdds tests ============
test_that("todayOdds returns data frame or NULL", {
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
    .package = "HockeyModel"
  )
  result <- suppressWarnings(todayOdds(today = as.Date("2019-11-01")))
  expect_true(is.data.frame(result))
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

# ============ plot_prediction_playoffs_by_team tests ============
test_that("plot_prediction_playoffs_by_team executes gracefully", {
  p <- plot_prediction_playoffs_by_team(all_predictions = example_predictions)
  expect_true(ggplot2::is_ggplot(p))
})
