context("test-pwhl-dixon-coles-workflow")

# PWHL workflow tests mirror test-nhl-dixon-coles-workflow.R, exercising the
# shared DC workflow functions against bundled PWHL data so both leagues'
# flow paths are covered.  The synthetic Team A–F remainderSeasonDC tests and
# dcPredictMultipleDays fallback test in the NHL file are generic / NHL-specific
# respectively and need no PWHL counterpart here.

.pwhl_params <- function() {
  list(
    m = HockeyModel::pwhl_m,
    rho = HockeyModel::pwhl_rho,
    beta = HockeyModel::pwhl_beta,
    eta = HockeyModel::pwhl_eta,
    k = HockeyModel::pwhl_k
  )
}

# ============ .update_dc_pwhl end-to-end ============
test_that(".update_dc_pwhl generates valid params", {
  params <- suppressWarnings(.update_dc_pwhl(save_data = FALSE))
  expect_true(is.list(params))
  expect_true(all(c("m", "rho", "beta", "eta", "k") %in% names(params)))

  # rho is in [-0.5, 0.5] per Dixon-Coles bounds
  expect_gte(params$rho, -0.5)
  expect_lte(params$rho, 0.5)

  # Weibull params are positive and finite for a valid fit
  expect_true(all(is.finite(params$beta)) && params$beta > 0)
  expect_true(all(is.finite(params$eta)) && params$eta > 0)
  expect_true(all(is.finite(params$k)))
})

test_that(".update_dc_pwhl with historical date works", {
  # The bundled PWHL scores span 2023-12 to 2026-04, so a mid-season date
  # (Oct 2025) has enough prior data for all three stages.
  params <- suppressWarnings(.update_dc_pwhl(
    currentDate = as.Date("2025-10-01"),
    save_data = FALSE
  ))
  expect_true(is.list(params))
  expect_true(all(c("m", "rho", "beta", "eta", "k") %in% names(params)))
})

# ============ remainderSeasonDC with PWHL data ============
test_that("remainderSeasonDC returns odds table directly (PWHL)", {
  pwhlScores <- HockeyModel::pwhlScores
  pwhlSchedule <- HockeyModel::pwhlSchedule

  # Use the actual scheduled games as future schedule
  sched <- pwhlSchedule[pwhlSchedule$GameStatus == "Scheduled", ]
  scor <- pwhlScores

  local_mocked_bindings(
    .todayDC = function(today, schedule, ...) {
      day_games <- schedule[schedule$Date == as.Date(today), ]
      data.frame(
        HomeTeam = day_games$HomeTeam,
        AwayTeam = day_games$AwayTeam,
        HomeWin = 0.5,
        AwayWin = 0.3,
        Draw = 0.2,
        GameID = day_games$GameID
      )
    },
    .package = "HockeyModel"
  )

  result <- remainderSeasonDC(
    nsims = 3,
    cores = 1,
    scores = scor,
    schedule = sched,
    params = .pwhl_params(),
    odds = TRUE,
    regress = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(
    c(
      "HomeTeam",
      "AwayTeam",
      "HomeWin",
      "AwayWin",
      "Draw",
      "GameID",
      "Date"
    ) %in%
      names(result)
  ))
  expect_gt(nrow(result), 0)
})

# ============ .todayDC with PWHL teams and params ============
test_that(".todayDC keeps GameID alongside xG columns for PWHL (#82)", {
  first <- HockeyModel::pwhlScores[1, ]

  result <- .todayDC(
    params = .pwhl_params(),
    today = max(HockeyModel::pwhlSchedule$Date) + 5,
    schedule = data.frame(
      Date = as.Date(max(HockeyModel::pwhlSchedule$Date) + 5),
      HomeTeam = first$HomeTeam,
      AwayTeam = first$AwayTeam,
      GameID = "G1",
      stringsAsFactors = FALSE
    ),
    include_xG = TRUE
  )

  expect_true("GameID" %in% names(result))
  expect_equal(result$GameID, "G1")
  # xG columns are present on the same rows as GameID (not a separate table).
  expect_true(all(c("Home_xG", "Away_xG") %in% names(result)))
})

test_that(".todayDC returns valid odds for PWHL games", {
  pwhlSchedule <- HockeyModel::pwhlSchedule
  # Use the first scheduled date to get actual games
  today <- min(pwhlSchedule$Date)

  result <- .todayDC(
    params = .pwhl_params(),
    today = today,
    schedule = pwhlSchedule
  )

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  # Each row's odds sum to 1
  for (i in seq_len(nrow(result))) {
    expect_equal(
      result$HomeWin[i] + result$AwayWin[i] + result$Draw[i],
      1,
      tolerance = 1e-10
    )
  }
})

# ============ playoffDC with PWHL params (workflow-level) ============
test_that("playoffDC returns a valid probability for PWHL teams", {
  teams <- unique(c(
    HockeyModel::pwhlScores$HomeTeam,
    HockeyModel::pwhlScores$AwayTeam
  ))
  result <- playoffDC(teams[1], teams[2], params = .pwhl_params())
  expect_true(is.numeric(result))
  expect_equal(length(result), 1)
  expect_gt(result, 0)
  expect_lt(result, 1)
})
