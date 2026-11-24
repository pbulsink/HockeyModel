context("test-pwhl-dixon-coles-fit")

# PWHL fit tests mirror test-nhl-dixon-coles-fit.R against bundled PWHL data.
# The key difference: pwhlScores has no Result column, so .pwhl_add_result()
# must be applied before passing scores to getM/getRho/getWeibullParams —
# exactly what .update_dc_pwhl() does internally.

.pwhl_sc <- function() {
  HockeyModel:::.pwhl_add_result(HockeyModel::pwhlScores)
}

# ============ getWeibullParams with PWHL data ============
test_that("getWeibullParams reaches a valid interior fit for PWHL", {
  # With the open bounds restored in #103, L-BFGS-B must land off its lower
  # bounds and produce finite positive shape/scale parameters.
  wp <- suppressWarnings(getWeibullParams(
    m = HockeyModel::pwhl_m,
    rho = HockeyModel::pwhl_rho,
    scores = .pwhl_sc()
  ))
  expect_true(is.finite(wp$beta) && wp$beta > 0.1)
  expect_true(is.finite(wp$eta) && wp$eta > 0.1)
  expect_true(is.finite(wp$k))
})

# ============ getRho with PWHL data ============
test_that("getRho returns a bounded, off-boundary estimate for PWHL (#103)", {
  # On regulation-time scores the Dixon-Coles low-score dependence should be
  # estimable (not pinned to the +0.5 boundary) for PWHL as well.
  rho <- suppressWarnings(getRho(m = HockeyModel::pwhl_m, scores = .pwhl_sc()))
  expect_true(is.finite(rho))
  expect_gte(rho, -0.5)
  expect_lte(rho, 0.5)
  # Should not be pinned to either boundary
  expect_gt(rho, -0.49)
  expect_lt(rho, 0.49)
})

# ============ getM with PWHL scores and cross-season discounting ============
test_that("getM with nu > 0 produces a valid model for PWHL", {
  sc <- .pwhl_sc()
  m_nu <- suppressWarnings(getM(
    scores = sc,
    currentDate = max(sc$Date) + 30,
    xi = DC_XI_PWHL,
    upsilon = DC_UPSILON_PWHL,
    nu = DC_NU_PWHL
  ))
  expect_s3_class(m_nu, "glm")
  expect_true(length(coef(m_nu)) > 0)
})

test_that("getM with nu = 0 and nu = DC_NU_PWHL produce same team coefficient names", {
  # Cross-season discounting changes coefficient magnitudes but not the set of
  # teams, since both use the same scores subset.
  sc <- .pwhl_sc()
  m0 <- suppressWarnings(getM(
    scores = sc,
    currentDate = max(sc$Date) + 30,
    xi = DC_XI_PWHL,
    upsilon = DC_UPSILON_PWHL,
    nu = 0
  ))
  mpw <- suppressWarnings(getM(
    scores = sc,
    currentDate = max(sc$Date) + 30,
    xi = DC_XI_PWHL,
    upsilon = DC_UPSILON_PWHL,
    nu = DC_NU_PWHL
  ))
  expect_equal(names(coef(m0)), names(coef(mpw)))
})
