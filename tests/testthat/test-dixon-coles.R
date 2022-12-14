context("test-dixon-coles")

test_that("Model Creates OK", {
  tmpdir<-withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)
  withr::local_options("BulsinkBxG.data.path" = tmpdir)

  params<-suppressWarnings(updateDC(save_data = FALSE))

  expect_lt(params$rho, 0)
  expect_gt(params$rho, -0.5)

  expect_lt(params$beta, 10)
  expect_gt(params$beta, 1)
  expect_lt(params$eta, 10)
  expect_gt(params$eta, 1)
  expect_lt(params$k, 10)
  expect_gt(params$k, 1)
})

test_that("DC Functions function", {
  pmat<-dcProbMatrix(home = "Toronto Maple Leafs", away="Ottawa Senators")
  expect_equal(sum(pmat), 1)
  pmat2<-prob_matrix(lambda=2, mu=2, params=list('rho'=-0.25, 'beta'= 2, 'eta'=2, 'k'=5), maxgoal = 4)
  expect_equal(sum(pmat2), 1)
  expect_equal(pmat2,
               structure(c(0.0713211695449963, 0.0194577853633925, 0.0389155707267851,
                           0.0259437138178567, 0.0129718569089284, 0.0194577853633925, 0.168448674977137,
                           0.0778311414535702, 0.0518874276357135, 0.0259437138178567, 0.0389155707267851,
                           0.0778311414535702, 0.0579136240868313, 0.0518874276357135, 0.0259437138178567,
                           0.0259437138178567, 0.0518874276357135, 0.0518874276357135, 0.00596378005160021,
                           0.0172958092119045, 0.0129718569089284, 0.0259437138178567, 0.0259437138178567,
                           0.0172958092119045, 0.000196430560280055), .Dim = c(5L, 5L)))
})

test_that("DC Convenience functions are ok", {
  params<-parse_dc_params(NULL)
  expect_true(dcResult(lambda = 3, mu = 3, params=params) %in% c(0, 0.25, 0.4, 0.5, 0.6, 0.75, 1))

  sim<-dcSample(home = "Nashville Predators", away = "Colorado Avalanche")
  expect_true(sim %in% c(0, 0.25, 0.4, 0.6, 0.75, 1))

  sim<-dcSample("Dallas Stars", "Columbus Blue Jackets", as_result = FALSE)
  expect_true(sim$OTStatus %in% c("", "OT", "SO"))
  expect_equal(length(sim), 3)
  expect_equal(names(sim), c("HomeGoals", "AwayGoals", "OTStatus"))
})

test_that("Predictions Run", {
  tmpdir<-withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)
  withr::local_options("BulsinkBxG.data.path" = tmpdir)

  sched<-schedule[schedule$Date > as.Date("2018-09-01") & schedule$Date <= as.Date("2019-04-06"),]
  scor<-scores[scores$Date < as.Date("2018-09-01"),]

  #First shot, loopedSim
  expect_true(suppressWarnings(dcPredictMultipleDays(start=as.Date("2018-08-01"), end = as.Date("2018-08-01"), schedule = sched, scores = scor, nsims=10, cores=1, filedir = "./", likelihood_graphic=FALSE)))
  expect_true(file.exists("./2018-08-01-predictions.RDS"))
  file.remove("./2018-08-01-predictions.RDS")
  expect_false(file.exists("./2018-08-01-predictions.RDS"))

  #Try again parallel
  expect_true(suppressWarnings(dcPredictMultipleDays(start=as.Date("2018-08-01"), end = as.Date("2018-08-01"), schedule = sched, scores = scor, nsims=10, cores=2, filedir = "./", likelihood_graphic=FALSE)))
  expect_true(file.exists("./2018-08-01-predictions.RDS"))
  file.remove("./2018-08-01-predictions.RDS")
  expect_false(file.exists("./2018-08-01-predictions.RDS"))

  #first shot, remainderSeason
  remainderseason<-remainderSeasonDC(nsims=10, cores=1, scores=scor, schedule = sched, regress = TRUE)
  expect_equal(names(remainderseason), c("summary_results", "raw_results"))
  expect_equal(nrow(remainderseason$summary_results)*10, nrow(remainderseason$raw_results))

  #try again parallel
  remainderseason<-remainderSeasonDC(nsims=10, cores=2, scores=scor, schedule = sched, regress = TRUE)
  expect_equal(names(remainderseason), c("summary_results", "raw_results"))
  expect_equal(nrow(remainderseason$summary_results)*10, nrow(remainderseason$raw_results))
})

test_that("DC Playoffs functions", {
  po_odds<-playoffDC("Toronto Maple Leafs", "Carolina Hurricanes")
  expect_lt(po_odds, 1)
  expect_gt(po_odds, 0)
})

test_that("DC Today is good", {
  tmpdir<-withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)
  withr::local_options("BulsinkBxG.data.path" = tmpdir)

  today_odds<-todayDC(today=as.Date("2019-11-01"))
  expect_equal(nrow(today_odds), 8)
  expect_equal(ncol(today_odds), 6)
  expect_equal(colnames(today_odds), c("HomeTeam", "AwayTeam", "HomeWin", "AwayWin", "Draw", "GameID"))

  today_odds_xg<-todayDC(today=as.Date("2019-11-01"), include_xG = TRUE)
  expect_equal(nrow(today_odds_xg), 8)
  expect_equal(ncol(today_odds_xg), 7)
  expect_equal(colnames(today_odds_xg), c("HomeTeam", "AwayTeam", "HomeWin", "AwayWin", "Draw", "Home_xG", "Away_xG"))

  today_odds_xg$Home_xG<-today_odds_xg$Away_xG<-today_odds$GameID<-NULL

  expect_identical(today_odds, today_odds_xg)

})

test_that("DC Result works", {
  expect_true(dcResult(4,2) %in% c(1, 0.75, 0.6, 0.4, 0.25, 0))
  expect_true(all(dcResult(c(4,2), c(2,4)) %in% c(1, 0.75, 0.6, 0.4, 0.25, 0)))
})
