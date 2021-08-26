context("test-dixon-coles")

test_that("Model Creates OK", {
  params<-suppressWarnings(updateDC(save_data = FALSE))

  expect_lt(params$rho, 0)
  expect_gt(params$rho, -0.5)

  expect_lt(params$beta, 10)
  expect_gt(params$beta, 1)
  expect_lt(params$eta, 10)
  expect_gt(params$eta, 1)
  expect_lt(params$k, 10)
  expect_gt(params$k, 1)
})

test_that("DC Functions function", {
  pmat<-dcProbMatrix(home = "Toronto Maple Leafs", away="Ottawa Senators")
  expect_equal(sum(pmat), 1)
  pmat2<-prob_matrix(lambda=2, mu=2, params=list('rho'=-0.25, 'beta'= 2, 'eta'=2, 'k'=5), maxgoal = 4)
  expect_equal(sum(pmat2), 1)
  expect_equal(pmat2,
                   structure(c(0.0743597671195273, 0.0190959661920909, 0.0381919323841818,
                               0.0254612882561212, 0.0127306441280606, 0.0190959661920909, 0.175625334284379,
                               0.0763838647683636, 0.0509225765122424, 0.0254612882561212, 0.0381919323841818,
                               0.0763838647683636, 0.0603810008671787, 0.0509225765122424, 0.0254612882561212,
                               0.0254612882561212, 0.0509225765122424, 0.0509225765122424, 0.00621786348454779,
                               0.0169741921707475, 0.0127306441280606, 0.0254612882561212, 0.0254612882561212,
                               0.0169741921707475, 0.000204799371782146), .Dim = c(5L, 5L)))
})

test_that("DC Convenience functions are ok", {
  params<-parse_dc_params(NULL)
  expect_true(dcResult(lambda = 3, mu = 3, params=params) %in% c(0, 0.25, 0.4, 0.5, 0.6, 0.75, 1))
})
