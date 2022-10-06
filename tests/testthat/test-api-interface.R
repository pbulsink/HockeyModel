context("test-api-interface")

test_that("Schedules are ok", {
  sched<-getNHLSchedule()
  expect_true(is.data.frame(sched))
  expect_equal(ncol(sched), 6)
  expect_equal(colnames(sched), c("Date", "HomeTeam", "AwayTeam", "GameID", "GameType", "GameState"))
  expect_true(all(sched$GameType %in% c("R", "P")))

  sched2<-updateScheduleAPI(schedule=sched)
  expect_true(is.data.frame(sched2))
  expect_equal(ncol(sched), 6)
  expect_equal(colnames(sched), c("Date", "HomeTeam", "AwayTeam", "GameID", "GameType", "GameState"))
  expect_true(all(sched$GameType %in% c("R", "P")))
  expect_equal(sched, sched2)

  #add a dummy game to sched2
  sched2<-rbind(sched2, data.frame("Date" = as.Date("2019-10-31"), "HomeTeam" = "New Jersey Devils", "AwayTeam" = "Philadelphia Flyers",
                                   "GameID" = 2019020196, "GameType" = "R", "GameState" = "Scheduled"))
  sched2 <- removeUnscheduledGames(schedule = sched2)
  expect_equal(sched, sched2)
})

test_that("Scores are OK", {
  score<-getNHLScores(2020020001, progress = F)
  expect_true(is.data.frame(score))
  expect_equal(ncol(score), 12)
  expect_equal(nrow(score), 1)
  #goodscore<-structure(list(Date = structure(18640, class = "Date"), HomeTeam = "Philadelphia Flyers",
  #                          AwayTeam = "Pittsburgh Penguins", GameID = 2020020001, HomeGoals = 6L,
  #                          AwayGoals = 3L, OTStatus = "", GameType = "R", GameStatus = "Final",
  #                          Result = 1), row.names = c(NA, -1L), class = "data.frame")
  #expect_identical(score, goodscore)

  today<-games_today(date=as.Date("2019-11-01"))
  expect_true(is.null(today)) #Why null? because games_today only returns 'scheduled' games from a date. NULL return is equivalent to finishing the code anyway (i.e. not an error)
})

test_that("Series is ok", {
  #tough to test as it's a moving target
  series<-getAPISeries()
  if(inPlayoffs()){
    #now there should be a series
    expect_gt(nrow(series), 0)
    expect_true(is.data.frame(series))
  }
  series<-getAPISeries("20182019")
  expect_true(is.data.frame(series))
  expect_equal(nrow(series), 15)
  expect_equal(ncol(series), 10)
  expect_true(all(series$Status == "Complete"))
})

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
  expect_equal(getShortTeam("bob"), character(0))

  expect_equal(getTeamConferences("Toronto Maple Leafs"), apiteams[apiteams$name == "Toronto Maple Leafs", ]$conference.name)
  expect_equal(getTeamConferences(c("Toronto Maple Leafs", "Ottawa Senators")),
               c(apiteams[apiteams$name == "Toronto Maple Leafs", ]$conference.name, apiteams[apiteams$name == "Ottawa Senators", ]$conference.name))
  expect_equal(getTeamConferences("bob"), character(0))

  expect_equal(getTeamDivisions("Toronto Maple Leafs"),apiteams[apiteams$name == "Toronto Maple Leafs", ]$division.name)
  expect_equal(getTeamDivisions(c("Toronto Maple Leafs", "Ottawa Senators")),
               c(apiteams[apiteams$name == "Toronto Maple Leafs", ]$division.name, apiteams[apiteams$name == "Ottawa Senators", ]$division.name))
  expect_equal(getTeamDivisions("bob"), character(0))

  expect_equal(getConferences(), unique(apiteams$conference.name))
  expect_equal(getDivisions(), unique(apiteams$division.name))
})

test_that("Other Utility Functions are OK", {
  expect_equal(clean_names(c("Chicago Blackhawks", "Toronto Maple Leafs")), c("Chicago Blackhawks", "Toronto Maple Leafs"))
  expect_equal(getTeamConferences("Chicago Blackhawks"), "Western")
  expect_equal(getTeamConferences("Toronto Maple Leafs"), "Eastern")
  expect_equal(getTeamDivisions("Toronto Maple Leafs"), "Atlantic")
  expect_equal(getShortTeam("Toronto Maple Leafs"), "TOR")
  expect_equal(getSeasonEndDate(season="20182019"), as.Date("2019-06-12"))
  expect_equal(getNumGames("20202021"), 56)
})
