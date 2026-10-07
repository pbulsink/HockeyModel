library(testthat)
pkgload::load_all()
testthat::test_file(file.path(
  "tests",
  "testthat",
  "test-nhl-dixon-coles-predict.R"
))
