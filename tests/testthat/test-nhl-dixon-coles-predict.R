context("test-nhl-dixon-coles-predict")

# ============ dcProbMatrix tests ============
test_that("DC Functions function", {
  pmat <- dcProbMatrix(home = "Toronto Maple Leafs", away = "Ottawa Senators")
  expect_equal(sum(pmat), 1)
  pmat2 <- prob_matrix(
    lambda = 2,
    mu = 2,
    params = list("rho" = -0.25, "beta" = 2, "eta" = 2, "k" = 5),
    maxgoal = 4
  )
  expect_equal(sum(pmat2), 1)
  expect_equal(
    pmat2,
    structure(
      c(
        0.0713211695449963,
        0.0194577853633925,
        0.0389155707267851,
        0.0259437138178567,
        0.0129718569089284,
        0.0194577853633925,
        0.168448674977137,
        0.0778311414535702,
        0.0518874276357135,
        0.0259437138178567,
        0.0389155707267851,
        0.0778311414535702,
        0.0579136240868313,
        0.0518874276357135,
        0.0259437138178567,
        0.0259437138178567,
        0.0518874276357135,
        0.0518874276357135,
        0.00596378005160021,
        0.0172958092119045,
        0.0129718569089284,
        0.0259437138178567,
        0.0259437138178567,
        0.0172958092119045,
        0.000196430560280055
      ),
      .Dim = c(5L, 5L)
    )
  )
})

test_that("prob_matrix sums to 1", {
  pmat <- prob_matrix(
    lambda = 1.5,
    mu = 1.5,
    params = list("rho" = -0.1, "beta" = 2, "eta" = 2, "k" = 3),
    maxgoal = 6
  )
  expect_equal(sum(pmat), 1, tolerance = 1e-10)
})

test_that("dcProbMatrix creates symmetric-like structure", {
  pmat <- dcProbMatrix(
    home = "Toronto Maple Leafs",
    away = "Toronto Maple Leafs"
  )
  expect_equal(sum(pmat), 1)
})

# ============ DC Convenience tests ============
test_that("DC Convenience functions are ok", {
  params <- parse_dc_params(NULL)
  expect_true(
    dcResult(lambda = 3, mu = 3, params = params) %in%
      c(0, 0.25, 0.4, 0.5, 0.6, 0.75, 1)
  )

  sim <- dcSample(home = "Nashville Predators", away = "Colorado Avalanche")
  expect_true(sim %in% c(0, 0.25, 0.4, 0.6, 0.75, 1))

  sim <- dcSample("Dallas Stars", "Columbus Blue Jackets", as_result = FALSE)
  expect_true(sim$OTStatus %in% c("", "OT", "SO"))
  expect_equal(length(sim), 3)
  expect_equal(names(sim), c("HomeGoals", "AwayGoals", "OTStatus"))
})

test_that("dcSample produces consistent results", {
  set.seed(123)
  sim1 <- dcSample("Toronto Maple Leafs", "Ottawa Senators")
  set.seed(123)
  sim2 <- dcSample("Toronto Maple Leafs", "Ottawa Senators")
  expect_equal(sim1, sim2)
})

test_that("dcSample with as_result=FALSE returns data frame", {
  sim <- dcSample("Toronto Maple Leafs", "Ottawa Senators", as_result = FALSE)
  expect_true(is.numeric(sim$HomeGoals))
  expect_true(is.numeric(sim$AwayGoals))
  expect_true(is.character(sim$OTStatus))
})

test_that("dcResult handles various score combinations", {
  set.seed(10)
  expect_equal(dcResult(5, 2), 1)
  expect_equal(dcResult(2, 5), 0)
})
