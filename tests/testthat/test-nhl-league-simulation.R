context("test-nhl-league-simulation")

test_that("Convenience Functions are OK", {
  skip_if_hockey_apis_unavailable()
  odds <- todayOdds(today = as.Date("2019-11-01"))
  expect_true(is.null(odds) || is.data.frame(odds))
  if (!is.null(odds)) {
    expect_true(all(
      c("HomeTeam", "AwayTeam", "HomeWin", "AwayWin") %in% names(odds)
    ))
  }
})

# ============ todayOdds tests ============
test_that("todayOdds returns data frame or NULL", {
  sched <- HockeyModel::scores
  sched <- sched[sched$Date > as.Date("2019-10-01"), ]
  sched <- sched[sched$Date < as.Date("2019-12-31"), ]
  result <- suppressWarnings(todayOdds(
    today = as.Date("2019-11-01"),
    schedule = sched
  ))
  expect_true(is.data.frame(result))
})

# ============ todayOdds tests (from test-graphics-comprehensive.R) ============
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
