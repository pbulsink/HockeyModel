context("xG scaling and integration tests")

testthat::test_that("fit_xg_ratio recovers a known multiplicative factor", {
  params <- tryCatch(parse_dc_params(NULL), error = function(e) skip("No model available"))

  # Use a subset of historical scores present in package data
  sc <- scores[1:40, ]
  sc$Date <- as.Date(sc$Date)

  # Compute predicted lambdas for subset (fallback to recovery if needed)
  ph <- sapply(seq_len(nrow(sc)), function(i) {
    p <- try(stats::predict(params$m, data.frame(Home = 1, Team = sc$HomeTeam[i], Opponent = sc$AwayTeam[i]), type = "response")[1], TRUE)
    if (!is.numeric(p) || is.na(p)) p <- DCPredictErrorRecover(team = sc$HomeTeam[i], opponent = sc$AwayTeam[i], homeiceadv = TRUE, m = params$m)
    as.numeric(p)
  })
  pa <- sapply(seq_len(nrow(sc)), function(i) {
    p <- try(stats::predict(params$m, data.frame(Home = 0, Team = sc$AwayTeam[i], Opponent = sc$HomeTeam[i]), type = "response")[1], TRUE)
    if (!is.numeric(p) || is.na(p)) p <- DCPredictErrorRecover(team = sc$AwayTeam[i], opponent = sc$HomeTeam[i], homeiceadv = FALSE, m = params$m)
    as.numeric(p)
  })

  keep <- !is.na(ph) & !is.na(pa) & ph > 0 & pa > 0
  if (sum(keep) < 5) {
    skip("Not enough valid predictions in sample to test xG fitting")
  }

  sc <- sc[keep, ]
  ph <- ph[keep]
  pa <- pa[keep]

  true_ratio <- 1.7
  sc$HomexG <- ph * true_ratio
  sc$AwayxG <- pa * true_ratio

  ratio <- fit_xg_ratio(params = params, scores = sc, min_games = 1)
  testthat::expect_equal(ratio, true_ratio, tolerance = 1e-6)
})


testthat::test_that("dcLambda applies fitted scaling when use_xg=TRUE", {
  params <- tryCatch(parse_dc_params(NULL), error = function(e) skip("No model available"))

  sc <- scores[1:40, ]
  sc$Date <- as.Date(sc$Date)

  ph <- sapply(seq_len(nrow(sc)), function(i) {
    p <- try(stats::predict(params$m, data.frame(Home = 1, Team = sc$HomeTeam[i], Opponent = sc$AwayTeam[i]), type = "response")[1], TRUE)
    if (!is.numeric(p) || is.na(p)) p <- DCPredictErrorRecover(team = sc$HomeTeam[i], opponent = sc$AwayTeam[i], homeiceadv = TRUE, m = params$m)
    as.numeric(p)
  })
  pa <- sapply(seq_len(nrow(sc)), function(i) {
    p <- try(stats::predict(params$m, data.frame(Home = 0, Team = sc$AwayTeam[i], Opponent = sc$HomeTeam[i]), type = "response")[1], TRUE)
    if (!is.numeric(p) || is.na(p)) p <- DCPredictErrorRecover(team = sc$AwayTeam[i], opponent = sc$HomeTeam[i], homeiceadv = FALSE, m = params$m)
    as.numeric(p)
  })

  keep <- !is.na(ph) & !is.na(pa) & ph > 0 & pa > 0
  if (sum(keep) < 5) {
    skip("Not enough valid predictions in sample to test xG scaling")
  }

  sc <- sc[keep, ]
  ph <- ph[keep]
  pa <- pa[keep]

  true_ratio <- 2.0
  sc$HomexG <- ph * true_ratio
  sc$AwayxG <- pa * true_ratio

  home <- sc$HomeTeam[1]
  away <- sc$AwayTeam[1]

  ratio <- fit_xg_ratio(params = params, scores = sc, min_games = 1)
  no_xg <- dcLambda(home, away, params = params, scores = sc, use_xg = FALSE)$home
  with_xg <- dcLambda(home, away, params = params, scores = sc, use_xg = TRUE)$home

  testthat::expect_equal(with_xg, no_xg * ratio, tolerance = 1e-6)
})
