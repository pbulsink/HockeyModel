test_that("dailySummary uses option-based graphic defaults for both leagues (#39)", {
  withr::local_options(
    list(
      HockeyModel.graphics.path = "/tmp/nhl-graphics",
      HockeyModel.prediction.path = "/tmp/prediction-root"
    )
  )

  observed <- new.env(parent = emptyenv())

  local_mocked_bindings(
    .daily_summary_nhl = function(graphic_dir, ...) {
      observed$nhl <- graphic_dir
      "nhl-summary"
    },
    dailyPWHLSummary = function(graphic_dir, ...) {
      observed$pwhl <- graphic_dir
      "pwhl-summary"
    },
    .package = "HockeyModel"
  )

  expect_identical(
    dailySummary(),
    list(nhl = "nhl-summary", pwhl = "pwhl-summary")
  )
  expect_identical(observed$nhl, "/tmp/nhl-graphics")
  expect_identical(
    observed$pwhl,
    file.path("/tmp/prediction-root", "pwhl_graphics")
  )
})
