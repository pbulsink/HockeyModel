context("test-utils")

test_that("Metrics are correctly calculated", {
  expect_equal(rmse(c(0.1, 0.2), c(0.15, 0.25)), 0.05)

  expect_equal(auc(c(0, 0, 1, 1), c(0.1, 0.2, 0.6, 0.7)), 1)
  expect_equal(auc(c(0, 0, 1, 1), c(0.1, 0.6, 0.4, 0.7)), 0.5)

  expect_equal(logLoss(0, 0), 0)
  expect_equal(logLoss(1, 1), 0)
  expect_equal(logLoss(0.5, 1), -log(0.5))
  expect_equal(logLoss(0.5, 1), logLoss(0.5, 0))
  expect_equal(logLoss(c(0, 1), c(0, 1)), 0)
  expect_equal(logLoss(c(0.5, 0), c(0, 0)), mean(c(0, -log(0.5))))

  expect_equal(accuracy(0.4, 0), 1)
  expect_equal(accuracy(0.4, 1), 0)
  expect_equal(accuracy(c(0.4, 0.6), c(1, 1)), 0.5)

  expect_error(
    rmse(c(1, 2, 3), c(1, 2)),
    regexp = "Error in HockeyModel::rmse\\(\\)"
  )
  expect_error(
    auc(c(1, 2, 3), c(1, 2)),
    regexp = "Error in HockeyModel::auc\\(\\)"
  )
  expect_error(
    logLoss(c(1, 2, 3), c(1, 2)),
    regexp = "Error in HockeyModel::logLoss\\(\\)"
  )
  expect_error(
    accuracy(c(1, 2, 3), c(1, 2)),
    regexp = "Error in HockeyModel::accuracy\\(\\)"
  )
})

# ============ RMSE tests ============
test_that("rmse calculation is correct", {
  expect_equal(rmse(c(1, 2, 3), c(1, 2, 3)), 0)
  expect_equal(rmse(c(0, 0, 0), c(1, 1, 1)), 1)
  expect_equal(rmse(c(0.5), c(0.5)), 0)
})

test_that("rmse handles mismatched lengths", {
  expect_error(
    rmse(c(1, 2), c(1, 2, 3)),
    regexp = "Error in HockeyModel::rmse\\(\\)"
  )
})

# ============ MSE tests ============
test_that("mse calculation is correct", {
  expect_equal(mse(c(1, 2, 3), c(1, 2, 3)), 0)
  expect_equal(mse(c(0, 0, 0), c(1, 1, 1)), 1)
  expect_equal(mse(c(1, 2), c(2, 3)), 1)
})

test_that("mse handles mismatched lengths", {
  expect_error(
    mse(c(1, 2), c(1, 2, 3)),
    regexp = "Error in HockeyModel::mse\\(\\)"
  )
})

# ============ R-Square tests ============
test_that("rsquare calculation is correct", {
  expect_equal(rsquare(c(1, 2, 3), c(1, 2, 3)), 1)
  expect_equal(rsquare(c(1, 2, 3), c(3, 2, 1)), 1)
})

test_that("rsquare handles mismatched lengths", {
  expect_error(
    rsquare(c(1, 2), c(1, 2, 3)),
    regexp = "Error in HockeyModel::rsquare\\(\\)"
  )
})

# ============ Log Loss tests ============
test_that("logLoss handles edge probabilities", {
  result <- logLoss(0.5, 0.5)
  expect_true(is.numeric(result))
  expect_true(result > 0)
})

test_that("logLoss with mixed inputs", {
  preds <- c(0.1, 0.5, 0.9)
  actual <- c(0, 0.5, 1)
  result <- logLoss(preds, actual)
  expect_true(is.numeric(result))
})

# ============ Accuracy tests ============
test_that("accuracy with all correct predictions", {
  expect_equal(accuracy(c(0.1, 0.2, 0.9), c(0, 0, 1)), 1)
})

test_that("accuracy with all incorrect predictions", {
  expect_equal(accuracy(c(0.9, 0.8, 0.1), c(0, 0, 1)), 0)
})

# ============ AUC tests ============
test_that("auc with perfect separation", {
  expect_equal(auc(c(0.1, 0.2, 0.3, 0.9, 0.95, 0.99), c(0, 0, 0, 1, 1, 1)), 1)
})

test_that("auc with random predictions", {
  # random predictions should be near 0.5
  set.seed(42)
  preds <- runif(100)
  actual <- sample(c(0, 1), 100, replace = TRUE)
  result <- auc(preds, actual)
  expect_true(result > 0 && result < 1)
})
