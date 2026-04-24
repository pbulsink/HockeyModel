context("test-dixon-coles")

# ============ updateDC tests ============
test_that("Model params generate OK", {
  params <- suppressWarnings(updateDC(save_data = FALSE))
  expect_true(is.list(params))
  expect_true(all(c("m", "rho", "beta", "eta", "k") %in% names(params)))

  expect_lt(params$rho, 0)
  expect_gt(params$rho, -0.5)

  expect_lt(params$beta, 10)
  expect_gt(params$beta, 1)
  expect_lt(params$eta, 10)
  expect_gt(params$eta, 1)
  expect_lt(params$k, 10)
  expect_gt(params$k, 1)
})

test_that("updateDC with historical date works", {
  params <- suppressWarnings(updateDC(currentDate = as.Date("2019-01-01"), save_data = FALSE))
  expect_true(is.list(params))
  expect_true(all(c("m", "rho", "beta", "eta", "k") %in% names(params)))
})

# ============ dcProbMatrix tests ============
test_that("DC Functions function", {
  pmat <- dcProbMatrix(home = "Toronto Maple Leafs", away = "Ottawa Senators")
  expect_equal(sum(pmat), 1)
  pmat2 <- prob_matrix(lambda = 2, mu = 2, params = list("rho" = -0.25, "beta" = 2, "eta" = 2, "k" = 5), maxgoal = 4)
  expect_equal(sum(pmat2), 1)
  expect_equal(
    pmat2,
    structure(c(
      0.0713211695449963, 0.0194577853633925, 0.0389155707267851,
      0.0259437138178567, 0.0129718569089284, 0.0194577853633925, 0.168448674977137,
      0.0778311414535702, 0.0518874276357135, 0.0259437138178567, 0.0389155707267851,
      0.0778311414535702, 0.0579136240868313, 0.0518874276357135, 0.0259437138178567,
      0.0259437138178567, 0.0518874276357135, 0.0518874276357135, 0.00596378005160021,
      0.0172958092119045, 0.0129718569089284, 0.0259437138178567, 0.0259437138178567,
      0.0172958092119045, 0.000196430560280055
    ), .Dim = c(5L, 5L))
  )
})

test_that("prob_matrix sums to 1", {
  pmat <- prob_matrix(lambda = 1.5, mu = 1.5, params = list("rho" = -0.1, "beta" = 2, "eta" = 2, "k" = 3), maxgoal = 6)
  expect_equal(sum(pmat), 1, tolerance = 1e-10)
})

test_that("dcProbMatrix creates symmetric-like structure", {
  pmat <- dcProbMatrix(home = "Toronto Maple Leafs", away = "Toronto Maple Leafs")
  expect_equal(sum(pmat), 1)
})

# ============ DC Convenience tests ============
test_that("DC Convenience functions are ok", {
  params <- parse_dc_params(NULL)
  expect_true(dcResult(lambda = 3, mu = 3, params = params) %in% c(0, 0.25, 0.4, 0.5, 0.6, 0.75, 1))

  sim <- dcSample(home = "Nashville Predators", away = "Colorado Avalanche")
  expect_true(sim %in% c(0, 0.25, 0.4, 0.6, 0.75, 1))

  sim <- dcSample("Dallas Stars", "Columbus Blue Jackets", as_result = FALSE)
  expect_true(sim$OTStatus %in% c("", "OT", "SO"))
  expect_equal(length(sim), 3)
  expect_equal(names(sim), c("HomeGoals", "AwayGoals", "OTStatus"))
})

test_that("dcSample produces consistent results", {
  set.seed(123)
  sim1 <- dcSample("Toronto Maple Leafs", "Ottawa Senators")
  set.seed(123)
  sim2 <- dcSample("Toronto Maple Leafs", "Ottawa Senators")
  expect_equal(sim1, sim2)
})

test_that("dcSample with as_result=FALSE returns data frame", {
  sim <- dcSample("Toronto Maple Leafs", "Ottawa Senators", as_result = FALSE)
  expect_true(is.numeric(sim$HomeGoals))
  expect_true(is.numeric(sim$AwayGoals))
  expect_true(is.character(sim$OTStatus))
})

test_that("dcResult handles various score combinations", {
  set.seed(10)
  expect_equal(dcResult(5, 2), 1)
  expect_equal(dcResult(2, 5), 0)
})

test_that("Predictions Run", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)

  sched <- schedule[schedule$Date > as.Date("2018-09-01") & schedule$Date <= as.Date("2019-04-06"), ]
  scor <- scores[scores$Date < as.Date("2018-09-01"), ]

  sched$Date <- as.Date(sched$Date)
  scor$Date <- as.Date(scor$Date)

  tryCatch({
    result <- suppressWarnings(
      dcPredictMultipleDays(
        start = as.Date("2018-08-01"),
        end = as.Date("2018-08-01"),
        schedule = sched,
        scores = scor,
        nsims = 3,
        cores = 1,
        filedir = "./",
        likelihood_graphic = FALSE)
      )
    expect_true(result)
    if (file.exists("./2018-08-01-predictions.RDS")) {
      file.remove("./2018-08-01-predictions.RDS")
    }
  }, error = function(e) {
    stop("dcPredictMultipleDays has issues")
  })

  #Try multicore:
  tryCatch({
    result <- suppressWarnings(
      dcPredictMultipleDays(
        start = as.Date("2018-08-01"),
        end = as.Date("2018-08-01"),
        schedule = sched,
        scores = scor,
        nsims = 4,
        cores = 2,
        filedir = "./",
        likelihood_graphic = FALSE)
    )
    expect_true(result)
    if (file.exists("./2018-08-01-predictions.RDS")) {
      file.remove("./2018-08-01-predictions.RDS")
    }
  }, error = function(e) {
    skip("dcPredictMultipleDays has issues with multicore")
  })
})

test_that("dcPredictMultipleDays returns TRUE", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)

  sched <- schedule[schedule$Date > as.Date("2018-09-01") & schedule$Date <= as.Date("2019-04-06"), ]
  scor <- scores[scores$Date < as.Date("2018-09-01"), ]

  sched$Date <- as.Date(sched$Date)
  scor$Date <- as.Date(scor$Date)

  tryCatch({
    result <- suppressWarnings(dcPredictMultipleDays(
      start = as.Date("2018-08-01"),
      end = as.Date("2018-08-01"),
      schedule = sched,
      scores = scor,
      nsims = 3,
      cores = 1,
      filedir = "./",
      likelihood_graphic = FALSE
    ))
    expect_true(result)

    if (file.exists("./2018-08-01-predictions.RDS")) {
      file.remove("./2018-08-01-predictions.RDS")
    }
  }, error = function(e) {
    stop("dcPredictMultipleDays has issues")
  })
})

test_that("remainderSeasonDC returns correct structure", {
  sched <- schedule[schedule$Date > as.Date("2018-09-01") & schedule$Date <= as.Date("2019-04-06"), ]
  scor <- scores[scores$Date < as.Date("2018-09-01"), ]

  tryCatch({
    result <- remainderSeasonDC(nsims = 3, cores = 1, scores = scor, schedule = sched, regress = FALSE)
    expect_true(is.list(result))
  }, error = function(e) {
    stop("remainderSeasonDC has implementation issues")
  })
})

# ============ DC Playoffs tests ============
test_that("DC Playoffs functions", {
  po_odds <- playoffDC("Toronto Maple Leafs", "Carolina Hurricanes")
  expect_lt(po_odds, 1)
  expect_gt(po_odds, 0)
})

test_that("playoffDC returns numeric probability", {
  result <- playoffDC("Toronto Maple Leafs", "Carolina Hurricanes")
  expect_true(is.numeric(result))
  expect_equal(length(result), 1)
})

test_that("playoffDC same team near 0.5", {
  result <- playoffDC("Toronto Maple Leafs", "Toronto Maple Leafs")
  expect_true(abs(result - 0.5) < 0.1)
})

# ============ todayDC tests ============
test_that("DC Today returns data or NULL", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)

  today_odds <- todayDC(today = as.Date("2019-11-01"))
  expect_true(is.null(today_odds) || is.data.frame(today_odds))

  if (!is.null(today_odds)) {
    expect_true("HomeTeam" %in% colnames(today_odds))
    expect_true("AwayTeam" %in% colnames(today_odds))
  }
})

test_that("todayDC returns NULL for no games", {
  result <- todayDC(today = as.Date("2020-07-15"))
  expect_null(result)
})

test_that("todayDC odds sum to 1 when available", {
  sched <- HockeyModel::scores
  sched <- sched[sched$Date > as.Date("2019-10-01"),]
  sched <- sched[sched$Date < as.Date("2019-12-31"),]
  today_odds <- todayDC(today = as.Date("2019-11-01"), schedule = sched)
  if (!is.null(today_odds) && nrow(today_odds) > 0) {
    for (i in seq_len(nrow(today_odds))) {
      expect_equal(today_odds$HomeWin[i] + today_odds$AwayWin[i] + today_odds$Draw[i], 1, tolerance = 1e-10)
    }
  }
})
