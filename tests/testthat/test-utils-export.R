context("test-utils")

test_that("IneffectiveMath HockeyVis Contest output is a string", {
  preds <- HockeyModel::example_predictions
  if (!is.null(preds) && nrow(preds) > 0) {
    preds <- preds[preds$predictionDate == max(preds$predictionDate), ]
    im <- formatPredsForHockeyVisContest(predictions = preds)
    expect_true(is.character(im))
  }
})
