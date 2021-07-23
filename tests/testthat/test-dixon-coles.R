context("test-dixon-coles")

test_that("Model Creates OK", {
  m<-suppressWarnings(getM())
  rho<-suppressWarnings(getRho(m=m))
  expect_lt(rho, 0)
  expect_gt(rho, -0.5)

  t<-getTheta(m=m,rho=rho)
  expect_lt(t$theta, 10)
  expect_gt(t$theta, 1)
  expect_lt(t$gamma, 10)
  expect_gt(t$gamma, 1)
})

test_that("DC Functions function", {
  pmat<-dcProbMatrix(home = "Toronto Maple Leafs", away="Ottawa Senators")
  expect_equal(sum(pmat), 1)

})
