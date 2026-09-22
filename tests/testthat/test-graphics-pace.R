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
