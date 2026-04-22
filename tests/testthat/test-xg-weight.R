context("xG weight blending")

# Ensure blending in DCPredict is a convex combination of the two distributions
test_that("DCPredict blending equals convex combination", {
  params <- tryCatch(parse_dc_params(NULL), error = function(e) skip("No model available"))

  # pick two common teams present in sample data
  home <- "Toronto Maple Leafs"
  away <- "Ottawa Senators"

  p_goal <- DCPredict(home, away, params = params, use_xg = FALSE)
  p_xg <- DCPredict(home, away, params = params, use_xg = TRUE)
  p_blend <- DCPredict(home, away, params = params, xg_weight = 0.3)

  # numeric tolerance a bit loose to avoid numerical differences
  expect_equal(p_blend, 0.3 * p_xg + 0.7 * p_goal, tolerance = 1e-8)
})


test_that("fit_xg_weight returns value in [0,1] or 0 with insufficient data", {
  params <- tryCatch(parse_dc_params(NULL), error = function(e) skip("No model available"))

  sc <- scores
  sc$Date <- as.Date(sc$Date)

  w <- fit_xg_weight(params = params, scores = sc, min_games = 5)
  expect_true(is.numeric(w))
  expect_true(w >= 0 && w <= 1)

  # insufficient data should return 0
  w2 <- fit_xg_weight(params = params, scores = sc[1:2, ], min_games = 10)
  expect_equal(w2, 0)
})