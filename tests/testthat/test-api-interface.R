context("test-api-interface")

test_that("Season Dates & Binaries work", {
  expect_visible(inRegularSeason())
  expect_visible(inPlayoffs())
  expect_visible(inOffSeason())
  expect_false(any(inRegularSeason(), inPlayoffs()) == inOffSeason())
  expect_equal(inRegularSeason("2018-12-02", boolean = FALSE), "20182019")
  expect_false(inPlayoffs("2018-12-02", boolean = FALSE))
  expect_true(inOffSeason("2018-08-01"))
})

test_that("SeasonID gets seasons ok", {
  expect_match(getCurrentSeason8(), regexp = "\\d{8}")
  expect_equal(getSeason("2018-12-02"), "20182019")
  expect_null(getSeason("2018-09-01"))
})

test_that("Get Team Info is OK", {
  apiteams<-nhlapi::nhl_teams()
  expect_equal(getShortTeam("Toronto Maple Leafs"), apiteams[apiteams$name == "Toronto Maple Leafs", ]$abbreviation)
  expect_equal(getShortTeam(c("Toronto Maple Leafs", "Ottawa Senators")),
               c(apiteams[apiteams$name == "Toronto Maple Leafs", ]$abbreviation, apiteams[apiteams$name == "Ottawa Senators", ]$abbreviation))
  expect_true(is.na(getShortTeam("bob")))

  expect_equal(getTeamConferences("Toronto Maple Leafs"), apiteams[apiteams$name == "Toronto Maple Leafs", ]$conference.name)
  expect_equal(getTeamConferences(c("Toronto Maple Leafs", "Ottawa Senators")),
               c(apiteams[apiteams$name == "Toronto Maple Leafs", ]$conference.name, apiteams[apiteams$name == "Ottawa Senators", ]$conference.name))
  expect_true(is.na(getTeamConferences("bob")))

  expect_equal(getTeamDivisions("Toronto Maple Leafs"),apiteams[apiteams$name == "Toronto Maple Leafs", ]$division.name)
  expect_equal(getTeamDivisions(c("Toronto Maple Leafs", "Ottawa Senators")),
               c(apiteams[apiteams$name == "Toronto Maple Leafs", ]$division.name, apiteams[apiteams$name == "Ottawa Senators", ]$division.name))
  expect_true(is.na(getTeamDivisions("bob")))

  expect_equal(getConferences(), unique(apiteams$conference.name))
  expect_equal(getDivisions(), unique(apiteams$division.name))
})

