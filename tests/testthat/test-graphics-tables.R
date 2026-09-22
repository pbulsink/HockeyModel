test_that("Series Odds Table is ok", {
  series <- data.frame(
    HomeTeam = c(
      "Tampa Bay Lightning",
      "Boston Bruins",
      "Washington Capitals"
    ),
    AwayTeam = c(
      "Columbus Blue Jackets",
      "Toronto Maple Leafs",
      "Carolina Hurricanes"
    ),
    HomeWins = c(0, 3, 3),
    AwayWins = c(4, 3, 2)
  )
  if (!requireNamespace('gt')) {
    expect_error(series_odds_table(series = series), "Package gt is required")
    skip("No `gt` package.")
  }
  t <- suppressWarnings(series_odds_table(series = series))
  expect_true("gt_tbl" %in% class(t))
  expect_equal(t$`_heading`$title, "NHL Playoff Series Odds")
  expect_equal(t$`_data`$HomeTeam[1], "Tampa Bay Lightning")
  expect_equal(t$`_data`$AwayTeam[1], "Columbus Blue Jackets")
  expect_true(is.numeric(t$`_data`$HomeOdds))
  expect_true(is.numeric(t$`_data`$AwayOdds))
  expect_equal(t$`_data`$HomeOdds[1] + t$`_data`$AwayOdds[1], 1)
})

test_that("Playoff Table is ok", {
  po <- structure(
    list(
      Team = c(
        "Tampa Bay Lightning",
        "Colorado Avalanche",
        "Carolina Hurricanes",
        "Vegas Golden Knights",
        "Pittsburgh Penguins",
        "Dallas Stars",
        "Florida Panthers",
        "Toronto Maple Leafs",
        "New York Islanders",
        "Edmonton Oilers",
        "Boston Bruins",
        "Winnipeg Jets",
        "Washington Capitals",
        "New York Rangers",
        "Calgary Flames",
        "Minnesota Wild",
        "Nashville Predators",
        "St. Louis Blues",
        "Columbus Blue Jackets",
        "Chicago Blackhawks",
        "Montreal Canadiens",
        "Vancouver Canucks",
        "Los Angeles Kings",
        "Philadelphia Flyers",
        "San Jose Sharks",
        "Anaheim Ducks",
        "Arizona Coyotes",
        "Ottawa Senators",
        "Detroit Red Wings",
        "New Jersey Devils",
        "Buffalo Sabres",
        "Seattle Kraken"
      ),
      Make_Playoffs = c(
        0.7904,
        0.8,
        0.7619,
        0.7714,
        0.7333,
        0.7238,
        0.8095,
        0.7428,
        0.8095,
        0.8476,
        0.8285,
        0.7142,
        0.7714,
        0.6190,
        0.8190,
        0.63808,
        0.6857,
        0.5142,
        0.2476,
        0.4190,
        0.5238,
        0.4190,
        0.2476,
        0.1714,
        0.1142,
        0.04766,
        0.2190,
        0.0571,
        0.0571,
        0.0571,
        0.0190,
        0.0190
      ),
      Win_First_Round = c(
        0.6095,
        0.6761,
        0.5238,
        0.6476,
        0.4190,
        0.3714,
        0.4095,
        0.3619,
        0.43809,
        0.5619,
        0.4285,
        0.3142,
        0.3809,
        0.2571,
        0.3714,
        0.2571,
        0.2952,
        0.1714,
        0.028571,
        0.1428,
        0.1142,
        0.0952,
        0.04761,
        0.0190,
        0.0190,
        0.0190,
        0.0095,
        0.0095,
        0,
        0,
        0,
        0
      ),
      Win_Second_Round = c(
        0.5238,
        0.5714,
        0.2952,
        0.5809,
        0.1809,
        0.1714,
        0.1523,
        0.1619,
        0.21904,
        0.1523,
        0.1619,
        0.1142,
        0.1428,
        0.1142,
        0.1047,
        0.1333,
        0.0857,
        0.0285,
        0.028571,
        0.0190,
        0.0190,
        0.0190,
        0.01904,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      Win_Conference = c(
        0.3238,
        0.3904,
        0.2095,
        0.2952,
        0.0857,
        0.0761,
        0.0761,
        0.0476,
        0.09523,
        0.0666,
        0.0666,
        0.0571,
        0.0380,
        0.0571,
        0.0190,
        0.0380,
        0.0380,
        0.0190,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ),
      Win_Cup = c(
        0.2380,
        0.2095,
        0.1333,
        0.1142,
        0.0571,
        0.0380,
        0.0380,
        0.0380,
        0.02857,
        0.0285,
        0.0190,
        0.0190,
        0.0190,
        0.0095,
        0.0095,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      )
    ),
    groups = structure(
      list(
        .rows = structure(
          list(
            1L,
            2L,
            3L,
            4L,
            5L,
            6L,
            7L,
            8L,
            9L,
            10L,
            11L,
            12L,
            13L,
            14L,
            15L,
            16L,
            17L,
            18L,
            19L,
            20L,
            21L,
            22L,
            23L,
            24L,
            25L,
            26L,
            27L,
            28L,
            29L,
            30L,
            31L,
            32L
          ),
          ptype = integer(0),
          class = c("vctrs_list_of", "vctrs_vctr", "list")
        )
      ),
      row.names = c(NA, -32L),
      class = c("tbl_df", "tbl", "data.frame")
    ),
    row.names = c(NA, -32L),
    class = "data.frame"
  )

  if (!requireNamespace('gt')) {
    expect_error(format_playoff_odds(po), "Package gt is required")
    skip("No `gt` package.")
  }
  p <- format_playoff_odds(po)
  expect_true("gt_tbl" %in% class(p))
  expect_equal(p$`_heading`$title, " NHL Playoff Odds")
  expect_equal(p$`_data`$Team[1], "Tampa Bay Lightning")
})
