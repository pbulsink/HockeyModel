context("test-nhl-team-season-lookup")

test_that("Season Dates & Binaries work", {
  vcr::use_cassette("utils", {
    expect_visible(inRegularSeason())
    expect_visible(inPlayoffs())
    expect_visible(inOffSeason())
    expect_false(inOffSeason("2018-12-02"))
    expect_equal(inRegularSeason("2018-12-02", boolean = FALSE), "20182019")
    expect_false(inPlayoffs("2018-12-02", boolean = FALSE))
    expect_true(inOffSeason("2018-08-01"))
    expect_equal(getSeasonEndDate(season = "20182019"), as.Date("2019-06-12"))
    expect_equal(HockeyModel::getSeason("2018-10-05"), "20182019")
    expect_equal(HockeyModel::getSeason("2019-02-15"), "20182019")
  })
})

test_that("SeasonID gets seasons ok", {
  vcr::use_cassette("current-season", {
    season <- getCurrentSeason8()
    expect_true(is.null(season) || is.character(season))
    if (!is.null(season)) {
      expect_equal(nchar(season), 8)
      expect_true(grepl("^\\d{8}$", season))
      first_four <- as.integer(substr(season, 1, 4))
      last_four <- as.integer(substr(season, 5, 8))
      expect_equal(last_four - first_four, 1)
      expect_equal(getSeason("2018-12-02"), "20182019")
      expect_null(getSeason("2018-09-01"))
    }
  })
})
