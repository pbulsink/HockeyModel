library(testthat)
pkgload::load_all()
testthat::test_file(file.path(
  "tests",
  "testthat",
  "test-47-parse-dc-params-once.R"
))
