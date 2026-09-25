test_that(".safe_post() reports success without warning (#noissue)", {
  local_mocked_bindings(
    post = function(...) invisible(NULL),
    .package = "atrrr"
  )

  expect_no_warning(
    result <- .safe_post("a test post", text = "hello")
  )
  expect_true(result$success)
  expect_identical(result$description, "a test post")
  expect_true(is.na(result$error))
})

test_that(".safe_post() captures failures instead of silently discarding them (#noissue)", {
  local_mocked_bindings(
    post = function(...) stop("network is down"),
    .package = "atrrr"
  )

  expect_warning(
    result <- .safe_post("a test post", text = "hello"),
    "network is down"
  )
  expect_false(result$success)
  expect_identical(result$description, "a test post")
  expect_identical(result$error, "network is down")
})

test_that(".summarize_post_results() aggregates and warns on failures (#noissue)", {
  results <- list(
    list(description = "post 1", success = TRUE, error = NA_character_),
    list(description = "post 2", success = FALSE, error = "boom"),
    list(description = "post 3", success = FALSE, error = "boom again")
  )

  expect_warning(
    summary <- .summarize_post_results(results),
    "2 of 3 social posts failed"
  )
  expect_equal(nrow(summary), 3)
  expect_equal(sum(summary$success), 1)
  expect_equal(sum(!summary$success), 2)
})

test_that(".summarize_post_results() handles an empty batch without warning (#noissue)", {
  expect_no_warning(summary <- .summarize_post_results(list()))
  expect_equal(nrow(summary), 0)
  expect_named(summary, c("description", "success", "error"))
})

test_that("tweet() reports (not silently swallows) a failed post (#noissue)", {
  withr::local_options(list(HockeyModel.graphics.path = withr::local_tempdir()))
  local_mocked_bindings(
    inRegularSeason = function() TRUE,
    .package = "HockeyModel"
  )
  local_mocked_bindings(
    post = function(...) stop("rate limited"),
    .package = "atrrr"
  )

  result <- suppressWarnings(suppressMessages(tweet(delay = 0)))
  expect_equal(nrow(result), 3)
  expect_true(all(!result$success))
  expect_true(all(result$error == "rate limited"))
})

test_that("tweet() does nothing outside the regular season (#noissue)", {
  local_mocked_bindings(
    inRegularSeason = function() FALSE,
    .package = "HockeyModel"
  )

  result <- tweet(delay = 0)
  expect_equal(nrow(result), 0)
})
