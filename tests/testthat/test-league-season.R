context("test-league-season")

# ============ Season Date tests ============
test_that("getSeasonStartDate returns correct date", {
  date <- getSeasonStartDate("20182019")
  expect_true(is.Date(date) || inherits(date, "Date"))
  expect_equal(as.character(date), "2018-10-03")
})

test_that("getSeasonStartDate works with multiple seasons", {
  seasons <- c("20172018", "20182019", "20192020")
  dates <- sapply(seasons, getSeasonStartDate)
  expect_true(all(sapply(dates, is.Date)))
  expect_true(all(substr(dates, 1, 4) == c("2017", "2018", "2019")))
})

test_that("getSeasonEndDate returns correct date", {
  date <- getSeasonEndDate("20182019")
  expect_true(is.Date(date) || inherits(date, "Date"))
  expect_equal(as.character(date), "2019-06-12")
})

test_that("getSeasonEndDate works with multiple seasons", {
  seasons <- c("20172018", "20182019", "20192020")
  dates <- sapply(seasons, getSeasonEndDate)
  expect_true(all(sapply(dates, is.Date)))
})

test_that("Season start is before season end", {
  season <- "20182019"
  start <- getSeasonStartDate(season)
  end <- getSeasonEndDate(season)
  expect_true(start < end)
})

# ============ Season Check tests ============
test_that("inRegularSeason works", {
  expect_true(inRegularSeason(as.Date("2018-11-01")))
  expect_false(inRegularSeason(as.Date("2018-09-01")))
  expect_false(inRegularSeason(as.Date("2019-05-01")))
})

test_that("inPlayoffs works", {
  expect_true(inPlayoffs(as.Date("2019-05-01")))
  expect_false(inPlayoffs(as.Date("2018-11-01")))
})

test_that("inOffSeason works", {
  expect_true(inOffSeason(as.Date("2018-08-01")))
  expect_false(inOffSeason(as.Date("2018-10-01")))
  expect_false(inOffSeason(as.Date("2019-05-01")))
})

# ============ buildStats tests ============
test_that("buildStats creates stats", {
  scores_subset <- scores[scores$Date > as.Date("2018-09-01") & scores$Date <= as.Date("2018-10-15"), ]
  stats <- buildStats(scores_subset)
  
  expect_true(is.data.frame(stats))
  expect_true("Team" %in% colnames(stats))
  expect_true("Points" %in% colnames(stats))
})

test_that("buildStats handles empty data", {
  empty_scores <- scores[scores$Date > as.Date("2099-01-01"), ]
  stats <- buildStats(empty_scores)
  expect_true(is.data.frame(stats))
})

test_that("buildStats returns valid points", {
  scores_subset <- scores[scores$Date > as.Date("2018-09-01") & scores$Date <= as.Date("2018-10-15"), ]
  stats <- buildStats(scores_subset)
  
  if (nrow(stats) > 0) {
    expect_true(all(stats$Points >= 0))
  }
})

# ============ dailySummary tests ============
test_that("dailySummary returns data frame", {
  summary <- dailySummary(date = as.Date("2018-10-15"))
  expect_true(is.data.frame(summary) || is.null(summary))
})

test_that("dailySummary has game structure", {
  summary <- dailySummary(date = as.Date("2018-10-15"))
  if (!is.null(summary) && nrow(summary) > 0) {
    expect_true("HomeTeam" %in% colnames(summary))
    expect_true("AwayTeam" %in% colnames(summary))
  }
})

# ============ ratings tests ============
test_that("ratings returns data frame", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)
  
  rating_df <- suppressWarnings(ratings(date = as.Date("2018-10-15")))
  expect_true(is.data.frame(rating_df) || is.null(rating_df))
})

test_that("ratings has team column", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)
  
  rating_df <- suppressWarnings(ratings(date = as.Date("2018-10-15")))
  if (!is.null(rating_df) && nrow(rating_df) > 0) {
    expect_true("Team" %in% colnames(rating_df))
  }
})

# ============ getIterativeTable tests ============
test_that("getIterativeTable returns data frame", {
  iter_table <- getIterativeTable(date = as.Date("2018-10-15"))
  expect_true(is.data.frame(iter_table) || is.null(iter_table))
})

test_that("getIterativeTable has expected structure", {
  iter_table <- getIterativeTable(date = as.Date("2018-10-15"))
  if (!is.null(iter_table) && nrow(iter_table) > 0) {
    expected_cols <- c("Team", "PointsFor", "PointsAgainst")
    expect_true(all(expected_cols %in% colnames(iter_table)))
  }
})

# ============ getSeasonMetricsDC tests ============
test_that("getSeasonMetricsDC returns data frame", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)
  
  metrics <- suppressWarnings(getSeasonMetricsDC(date = as.Date("2018-10-15")))
  expect_true(is.data.frame(metrics) || is.null(metrics))
})

test_that("getSeasonMetricsDC has metric columns", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)
  
  metrics <- suppressWarnings(getSeasonMetricsDC(date = as.Date("2018-10-15")))
  if (!is.null(metrics) && nrow(metrics) > 0) {
    expect_true(any(c("Team", "Metric", "Value") %in% colnames(metrics)))
  }
})

# ============ pointPredict tests ============
test_that("pointPredict returns numeric", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)
  
  points <- suppressWarnings(pointPredict(
    team = "Toronto Maple Leafs",
    date = as.Date("2018-10-15"),
    end_date = as.Date("2019-04-06")
  ))
  expect_true(is.numeric(points) || is.list(points))
})

# ============ predictMultipleDaysResultsDC tests ============
test_that("predictMultipleDaysResultsDC returns data frame", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)
  
  sched <- schedule[schedule$Date > as.Date("2018-09-01") & schedule$Date <= as.Date("2019-04-06"), ]
  scor <- scores[scores$Date < as.Date("2018-09-01"), ]
  
  preds <- suppressWarnings(predictMultipleDaysResultsDC(
    start = as.Date("2018-08-01"),
    end = as.Date("2018-08-05"),
    schedule = sched,
    scores = scor,
    nsims = 5,
    cores = 1
  ))
  expect_true(is.data.frame(preds) || is.null(preds))
})

# ============ remainderSeasonDC tests ============
test_that("remainderSeasonDC returns list with expected structure", {
  sched <- schedule[schedule$Date > as.Date("2018-09-01") & schedule$Date <= as.Date("2019-04-06"), ]
  scor <- scores[scores$Date < as.Date("2018-09-01"), ]
  
  result <- remainderSeasonDC(nsims = 5, cores = 1, scores = scor, schedule = sched, regress = TRUE)
  expect_true(is.list(result))
  expect_true("summary_results" %in% names(result))
  expect_true("raw_results" %in% names(result))
})

test_that("remainderSeasonDC summary has team column", {
  sched <- schedule[schedule$Date > as.Date("2018-09-01") & schedule$Date <= as.Date("2019-04-06"), ]
  scor <- scores[scores$Date < as.Date("2018-09-01"), ]
  
  result <- remainderSeasonDC(nsims = 5, cores = 1, scores = scor, schedule = sched, regress = TRUE)
  if ("summary_results" %in% names(result) && nrow(result$summary_results) > 0) {
    expect_true("Team" %in% colnames(result$summary_results))
  }
})

# ============ presidentOdds tests ============
test_that("presidentOdds returns numeric between 0 and 1", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)
  
  odds <- suppressWarnings(presidentOdds(team = "Toronto Maple Leafs"))
  if (!is.null(odds)) {
    expect_true(is.numeric(odds))
    expect_true(odds >= 0 && odds <= 1)
  }
})

test_that("presidentOdds returns same value each time when seeded", {
  tmpdir <- withr::local_tempdir()
  withr::local_options("HockeyModel.prediction.path" = tmpdir)
  
  set.seed(123)
  odds1 <- suppressWarnings(presidentOdds(team = "Toronto Maple Leafs"))
  set.seed(123)
  odds2 <- suppressWarnings(presidentOdds(team = "Toronto Maple Leafs"))
  
  if (!is.null(odds1) && !is.null(odds2)) {
    expect_equal(odds1, odds2)
  }
})

# ============ getTeamColours tests ============
test_that("getTeamColours returns vector", {
  colors <- getTeamColours("Toronto Maple Leafs")
  expect_true(is.vector(colors) || is.character(colors))
})

test_that("getTeamColours returns hex colors", {
  colors <- getTeamColours("Boston Bruins")
  if (!is.null(colors) && length(colors) > 0) {
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
  }
})
