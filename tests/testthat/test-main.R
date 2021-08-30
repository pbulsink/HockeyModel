test_that("model Updates OK", {
  model<-supressWarnings(updateModel())
  expect_true(names(model) = c("scores", "schedule", "params"))
})

#Most of the rest of main just calls the other package functions - testing is of limited benefit
