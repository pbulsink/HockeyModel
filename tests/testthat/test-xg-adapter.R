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
