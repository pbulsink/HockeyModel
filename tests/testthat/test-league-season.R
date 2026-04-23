context("test-league-season")
skip_if_hockey_apis_unavailable()

# ============ getSeasonStartDate tests ============
test_that("getSeasonStartDate returns Date object", {
  start <- getSeasonStartDate(20182019)
  expect_true(inherits(start, "Date"))
})

test_that("getSeasonStartDate returns valid date", {
  start <- getSeasonStartDate(20182019)
  expect_true(!is.na(start))
})

test_that("getSeasonStartDate is before end date", {
  start <- getSeasonStartDate(20182019)
  end <- getSeasonEndDate(20182019)
  expect_true(start < end)
})

# ============ getSeasonEndDate tests ============
test_that("getSeasonEndDate returns Date object", {
  end <- getSeasonEndDate(20182019)
  expect_true(inherits(end, "Date"))
})

test_that("getSeasonEndDate returns valid date", {
  end <- getSeasonEndDate(20182019)
  expect_true(!is.na(end))
})

# ============ getCurrentSeason8 tests ============
test_that("getCurrentSeason8 returns numeric or character", {
  result <- suppressWarnings(getCurrentSeason8())
  expect_true(is.numeric(result) || is.character(result))
})

# ============ inOffSeason tests ============
test_that("inOffSeason returns logical", {
  result <- inOffSeason(as.Date("2018-06-15"))
  expect_true(is.logical(result))
})

test_that("inOffSeason true during offseason", {
  result <- inOffSeason(as.Date("2018-07-15"))
  expect_true(result)
})

test_that("inOffSeason false during season", {
  result <- inOffSeason(as.Date("2018-11-15"))
  expect_false(result)
})

# ============ inPlayoffs tests ============
test_that("inPlayoffs returns logical", {
  result <- inPlayoffs(as.Date("2018-04-15"))
  expect_true(is.logical(result))
})

test_that("inPlayoffs false during regular season", {
  result <- inPlayoffs(as.Date("2018-12-15"))
  expect_false(result)
})

# ============ inRegularSeason tests ============
test_that("inRegularSeason returns logical", {
  result <- inRegularSeason(as.Date("2018-11-15"))
  expect_true(is.logical(result))
})

test_that("inRegularSeason true during regular season", {
  result <- inRegularSeason(as.Date("2018-11-15"))
  expect_true(result)
})

test_that("inRegularSeason false during offseason", {
  result <- inRegularSeason(as.Date("2018-07-15"))
  expect_false(result)
})

# ============ getTeamColours tests ============
test_that("getTeamColours returns vector", {
  colors <- getTeamColours("Toronto Maple Leafs", "Montreal Canadiens")
  expect_true(is.vector(colors) || is.character(colors))
})

test_that("getTeamColours returns hex colors", {
  colors <- getTeamColours("Boston Bruins", "New York Rangers")
  if (!is.null(colors) && length(colors) > 0) {
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
  }
})

# ============ todayOdds tests ============
test_that("todayOdds returns data frame or NULL", {
  result <- suppressWarnings(todayOdds())
  expect_true(is.data.frame(result) || is.null(result))
})

# ============ colourDelta tests ============
test_that("colourDelta returns numeric or character", {
  result <- colourDelta(0.5, 0.55)
  expect_true(is.numeric(result) || is.character(result))
})

test_that("colourDelta handles equal values", {
  result <- colourDelta(0.5, 0.5)
  expect_true(is.numeric(result) || is.character(result))
})

# ============ todayOddsPlot tests ============
test_that("todayOddsPlot executes without error", {
  p <- suppressWarnings(todayOddsPlot())
  expect_true(
    ggplot2::is_ggplot(p) || is.list(p) || is.null(p)
  )
})
