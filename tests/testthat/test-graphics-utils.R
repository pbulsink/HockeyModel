context("test-graphics-utils")

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

# ============ colourDelta tests ============
test_that("colourDelta returns numeric or character", {
  result <- colourDelta(0.5, 0.55)
  expect_true(is.numeric(result) || is.character(result))
})

test_that("colourDelta handles equal values", {
  result <- colourDelta(0.5, 0.5)
  expect_true(is.numeric(result) || is.character(result))
})

test_that("Colours are correctly compared", {
  expect_equal(hexToRGB("#000000"), c(0, 0, 0))
  expect_equal(hexToRGB("#FFFFFF"), c(255, 255, 255))
  expect_equal(hexToRGB("#101010"), c(16, 16, 16))

  expect_equal(colourDelta("#000000", "#000000"), 0)
  expect_equal(colourDelta("#000000", "#FFFFFF"), 1)

  expect_equal(colourDelta("#0000FF", "#000000"), 1 / 3)
})

# ============ hexToRGB tests ============
test_that("hexToRGB converts correctly", {
  expect_equal(hexToRGB("#000000"), c(0, 0, 0))
  expect_equal(hexToRGB("#FFFFFF"), c(255, 255, 255))
  expect_equal(hexToRGB("#101010"), c(16, 16, 16))
})

test_that("hexToRGB handles lowercase", {
  expect_equal(hexToRGB("#ffffff"), c(255, 255, 255))
  expect_equal(hexToRGB("#aabbcc"), hexToRGB("#AABBCC"))
})

test_that("hexToRGB primary colors", {
  expect_equal(hexToRGB("#FF0000"), c(255, 0, 0))
  expect_equal(hexToRGB("#00FF00"), c(0, 255, 0))
  expect_equal(hexToRGB("#0000FF"), c(0, 0, 255))
})

# ============ colourDelta tests ============
test_that("colourDelta calculates correctly", {
  expect_equal(colourDelta("#000000", "#000000"), 0)
  expect_equal(colourDelta("#000000", "#FFFFFF"), 1)
  expect_equal(colourDelta("#0000FF", "#000000"), 1 / 3)
})

test_that("colourDelta is symmetric", {
  expect_equal(
    colourDelta("#FF0000", "#00FF00"),
    colourDelta("#00FF00", "#FF0000")
  )
})

# ── getTeamColours (PWHL) ─────────────────────────────────────────────────────

test_that("getTeamColours returns list with home and away colours for PWHL", {
  tc <- getTeamColours(
    "Boston Fleet",
    "Ottawa Charge",
    teamColours = HockeyModel::pwhlTeamColours
  )
  expect_type(tc, "list")
  expect_true(all(c("home", "away") %in% names(tc)))
  expect_match(tc$home, "^#[0-9A-Fa-f]{6}$")
  expect_match(tc$away, "^#[0-9A-Fa-f]{6}$")
})

test_that("getTeamColours rejects unknown PWHL teams", {
  expect_error(
    getTeamColours(
      "Unknown Team",
      "Boston Fleet",
      teamColours = HockeyModel::pwhlTeamColours
    ),
    class = "rlang_error"
  )
  expect_error(
    getTeamColours(
      "Boston Fleet",
      "Unknown Team",
      teamColours = HockeyModel::pwhlTeamColours
    ),
    class = "rlang_error"
  )
})

test_that("getTeamColours returns valid colours for all six PWHL teams", {
  teams <- HockeyModel::pwhlTeamColours$Team
  for (t1 in teams) {
    for (t2 in teams[teams != t1]) {
      tc <- getTeamColours(t1, t2, teamColours = HockeyModel::pwhlTeamColours)
      expect_match(tc$home, "^#[0-9A-Fa-f]{6}$")
      expect_match(tc$away, "^#[0-9A-Fa-f]{6}$")
    }
  }
})
