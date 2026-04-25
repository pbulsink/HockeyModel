context("test-league-season")

# ============ getSeasonStartDate tests ============
test_that("getSeasonStartDate returns Date object", {
  vcr::use_cassette("current-season", {
    start <- getSeasonStartDate(20242025)
    expect_true(inherits(start, "Date"))
  })
})

test_that("getSeasonStartDate returns valid date", {
  vcr::use_cassette("current-season", {
    start <- getSeasonStartDate(20242025)
    expect_true(!is.na(start))
  })
})

test_that("getSeasonStartDate is before end date", {
  vals <- new.env(parent = emptyenv())
  vcr::use_cassette("current-season", {
    vals$start <- getSeasonStartDate(20242025)
  })
  vcr::use_cassette("current-season", {
    vals$end <- getSeasonEndDate(20242025)
    expect_true(vals$start < vals$end)
  })
})

# ============ getSeasonEndDate tests ============
test_that("getSeasonEndDate returns Date object", {
  vcr::use_cassette("current-season", {
    end <- getSeasonEndDate(20242025)
    expect_true(inherits(end, "Date"))
  })
})

test_that("getSeasonEndDate returns valid date", {
  vcr::use_cassette("current-season", {
    end <- getSeasonEndDate(20242025)
    expect_true(!is.na(end))
  })
})

# ============ getCurrentSeason8 tests ============
test_that("getCurrentSeason8 returns numeric or character", {
  vcr::use_cassette("current-season", {
    result <- suppressWarnings(getCurrentSeason8())
    expect_true(is.null(result) || is.numeric(result) || is.character(result))
  })
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
  sched <- HockeyModel::scores
  sched <- sched[sched$Date > as.Date("2019-10-01"), ]
  sched <- sched[sched$Date < as.Date("2019-12-31"), ]
  result <- suppressWarnings(todayOdds(today = as.Date("2019-11-01"), schedule = sched))
  expect_true(is.data.frame(result))
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
