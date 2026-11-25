# ============ .parseCores ============

test_that(".parseCores defaults to detectCores() with no option set (#90)", {
  expect_equal(
    HockeyModel:::.parseCores(NULL),
    as.integer(parallel::detectCores())
  )
})

test_that(".parseCores honours the HockeyModel.cores option (#90)", {
  # Operators on memory-constrained machines cap their default worker count.
  withr::local_options("HockeyModel.cores" = 1)
  expect_equal(HockeyModel:::.parseCores(NULL), 1L)
})

test_that(".parseCores stays silent when the option is applied (#107)", {
  # A full daily run resolves cores at several call sites; messaging each one
  # was chatty, so the cap now applies without notice.
  withr::local_options("HockeyModel.cores" = 1)
  expect_silent(HockeyModel:::.parseCores(NULL))
})

test_that(".parseCores caps the option at detectCores() (#90)", {
  withr::local_options(
    "HockeyModel.cores" = parallel::detectCores() + 5
  )
  expect_equal(
    HockeyModel:::.parseCores(NULL),
    as.integer(parallel::detectCores())
  )
})

test_that(".parseCores ignores invalid option values (#90)", {
  for (bad in c(0, -1, 2.5, "two")) {
    withr::local_options("HockeyModel.cores" = bad)
    expect_equal(
      HockeyModel:::.parseCores(NULL),
      as.integer(parallel::detectCores())
    )
  }
})

test_that(".parseCores explicit requests are independent of the option (#90)", {
  withr::local_options("HockeyModel.cores" = 1)
  expect_equal(HockeyModel:::.parseCores(2), min(2, parallel::detectCores()))
  expect_equal(
    HockeyModel:::.parseCores(999),
    as.integer(parallel::detectCores())
  )
})

test_that(".parseCores still rejects invalid explicit requests", {
  for (bad in c(0, -1, 2.5, "two")) {
    expect_equal(HockeyModel:::.parseCores(bad), 1)
  }
})
