test_that("iterative functions works", {

  vcr::use_cassette("iterative", {
  it <- iterativeOddsTable()
  expect_equal(
    colnames(it),
    c(
      "GameID",
      "HomeTeam",
      "AwayTeam",
      "HomeWin",
      "AwayWin",
      "Draw",
      "Date"
    )
  )

  tr <- getTeamRankings(
    "Toronto Maple Leafs",
    HockeyModel::iterativeRankings$rankings_wl
  )
  expect_equal(length(tr), 3)
  expect_equal(names(tr), c("attack", "defence", "rankings"))

  expect_equal(
    getTeamRankings("Bob", HockeyModel::iterativeRankings$rankings_wl)$attack,
    0
  )

  sched <- HockeyModel::scores
  sched <- sched[sched$Date > as.Date("2018-10-01"), ]
  sched <- sched[sched$Date < as.Date("2018-12-31"), ]
  it2 <- getIterativeTable(date = as.Date("2018-11-04"), schedule = sched)
  expect_equal(nrow(it2), 3)
  expect_equal(
    colnames(it2),
    c(
      "GameID",
      "HomeTeam",
      "AwayTeam",
      "HomeWin",
      "AwayWin",
      "TotalxG"
    )
  )

  rr <- suppressMessages(getReplacementRankings(save_data = FALSE))
  expect_equal(Sys.Date(), rr$rankings_date)
  expect_equal(names(rr), names(HockeyModel::iterativeRankings))
  })
})
