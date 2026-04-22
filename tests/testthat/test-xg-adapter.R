testthat::test_that("get_xg reads from HockeyModel.xg.path", {
  tmp <- tempfile(fileext = ".csv")
  gid <- "2024010001"

  df <- data.frame(
    game_id = c(gid, gid),
    h_a = c("home", "away"),
    xgf_all = c(1.23, 0.98),
    gf_all = c(2, 1),
    cf_all = c(10, 8),
    xgf_pk = c(0.0, 0.0),
    gf_pk = c(0, 0),
    cf_pk = c(0, 0),
    xgf_pp = c(0.0, 0.0),
    gf_pp = c(0, 0),
    cf_pp = c(0, 0),
    stringsAsFactors = FALSE
  )
  utils::write.csv(df, tmp, row.names = FALSE)

  old_opt <- getOption("HockeyModel.xg.path")
  options(HockeyModel.xg.path = tmp)
  on.exit(options(HockeyModel.xg.path = old_opt), add = TRUE)

  res <- get_xg(c(gid))
  testthat::expect_true("HomexG" %in% names(res))
  testthat::expect_equal(res$HomexG[1], 1.23)
  testthat::expect_equal(res$AwayxG[1], 0.98)
})

testthat::test_that("get_xg uses grep prefilter before CSV parse", {
  tmp <- tempfile(fileext = ".csv")
  gid <- "2024010002"

  writeLines(
    c(
      "game_id,h_a,xgf_all,gf_all,cf_all,xgf_pk,gf_pk,cf_pk,xgf_pp,gf_pp,cf_pp",
      "2024010001,home,9.99,1,1,0,0,0,0,0,0,bad_extra_field",
      paste(gid, "home", "2.10", "2", "11", "0", "0", "0", "0", "0", "0", sep = ","),
      paste(gid, "away", "1.40", "1", "9", "0", "0", "0", "0", "0", "0", sep = ",")
    ),
    con = tmp
  )

  old_opt <- getOption("HockeyModel.xg.path")
  options(HockeyModel.xg.path = tmp)
  on.exit(options(HockeyModel.xg.path = old_opt), add = TRUE)

  res <- get_xg(c(gid))
  testthat::expect_equal(res$HomexG[1], 2.10)
  testthat::expect_equal(res$AwayxG[1], 1.40)
})
