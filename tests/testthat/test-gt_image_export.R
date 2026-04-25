test_that("save_gt_as_png_ragg() creates a valid PNG file when ragg is available", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(x = 1:3, y = 4:6)
  gt_table <- gt::gt(test_df)

  temp_file <- withr::local_file(tempfile(fileext = ".png"))

  result <- save_gt_as_png_ragg(gt_table, temp_file)
  expect_equal(result, temp_file)
  expect_true(file.exists(temp_file))
})

test_that("save_gt_as_png_ragg() creates PNG file with auto-detected size", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(
    Name = c("Alice", "Bob", "Charlie"),
    Score = c(95, 87, 92),
    Grade = c("A", "B", "A")
  )
  gt_table <- gt::gt(test_df)

  temp_file <- withr::local_file(tempfile(fileext = ".png"))

  result <- save_gt_as_png_ragg(gt_table, temp_file)

  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)
  expect_equal(result, temp_file)
})

test_that("save_gt_as_png_ragg() auto-detected size is smaller than a fixed large size", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(x = 1:5, y = 6:10)
  gt_table <- gt::gt(test_df)

  temp_file_auto <- withr::local_file(tempfile(fileext = ".png"))
  temp_file_fixed <- withr::local_file(tempfile(fileext = ".png"))

  save_gt_as_png_ragg(gt_table, temp_file_auto)
  save_gt_as_png_ragg(gt_table, temp_file_fixed, width = 1400, height = 800)

  expect_true(file.exists(temp_file_auto))
  expect_true(file.exists(temp_file_fixed))
  expect_gt(file.size(temp_file_auto), 0)
  # Auto-detected image should be smaller than 1400x800
  expect_lt(file.size(temp_file_auto), file.size(temp_file_fixed))
})

test_that("save_gt_as_png_ragg() uses explicit width and height when provided", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(x = 1:5, y = 6:10)
  gt_table <- gt::gt(test_df)

  temp_file_small <- withr::local_file(tempfile(fileext = ".png"))
  temp_file_large <- withr::local_file(tempfile(fileext = ".png"))

  save_gt_as_png_ragg(gt_table, temp_file_small, width = 400, height = 300)
  save_gt_as_png_ragg(gt_table, temp_file_large, width = 1200, height = 900)

  expect_true(file.exists(temp_file_small))
  expect_true(file.exists(temp_file_large))
  expect_gt(file.size(temp_file_small), 0)
  expect_gt(file.size(temp_file_large), 0)
  expect_gt(file.size(temp_file_large), file.size(temp_file_small))
})

test_that("save_gt_as_png_ragg() works with styled gt tables", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(
    Team = c("Red", "Blue", "Green"),
    Wins = c(10, 8, 9),
    Losses = c(2, 4, 3)
  )

  gt_table <- gt::gt(test_df) |>
    gt::tab_header(title = "Team Performance", subtitle = "Regular Season")

  temp_file <- withr::local_file(tempfile(fileext = ".png"))

  result <- save_gt_as_png_ragg(gt_table, temp_file)

  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)
  expect_equal(result, temp_file)
})

test_that("save_gt_as_png_ragg() returns filename invisibly", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(a = 1:3, b = 4:6)
  gt_table <- gt::gt(test_df)

  temp_file <- withr::local_file(tempfile(fileext = ".png"))

  expect_invisible(
    save_gt_as_png_ragg(gt_table, temp_file)
  )
})

test_that("save_gt_as_png_ragg() accepts scale parameter", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(x = 1:3, y = 4:6)
  gt_table <- gt::gt(test_df)

  temp_file_scale1 <- withr::local_file(tempfile(fileext = ".png"))
  temp_file_scale2 <- withr::local_file(tempfile(fileext = ".png"))

  save_gt_as_png_ragg(gt_table, temp_file_scale1, scale = 1)
  save_gt_as_png_ragg(gt_table, temp_file_scale2, scale = 2)

  expect_true(file.exists(temp_file_scale1))
  expect_true(file.exists(temp_file_scale2))
  expect_gt(file.size(temp_file_scale1), 0)
  expect_gt(file.size(temp_file_scale2), 0)
})

test_that("save_gt_as_png_ragg() handles single row dataframes", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(x = 1, y = 2)
  gt_table <- gt::gt(test_df)

  temp_file <- withr::local_file(tempfile(fileext = ".png"))

  result <- save_gt_as_png_ragg(gt_table, temp_file)

  expect_true(file.exists(temp_file))
  expect_gt(file.size(temp_file), 0)
  expect_equal(result, temp_file)
})

test_that("save_gt_as_png_ragg() padding parameter controls extra space", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("gt")

  test_df <- data.frame(Team = c("A", "B"), Points = c(10, 8))
  gt_table <- gt::gt(test_df)

  temp_no_pad <- withr::local_file(tempfile(fileext = ".png"))
  temp_with_pad <- withr::local_file(tempfile(fileext = ".png"))

  save_gt_as_png_ragg(gt_table, temp_no_pad, padding = 0)
  save_gt_as_png_ragg(gt_table, temp_with_pad, padding = 100)

  expect_true(file.exists(temp_no_pad))
  expect_true(file.exists(temp_with_pad))
  # More padding means a larger canvas → larger PNG file
  expect_gt(file.size(temp_with_pad), file.size(temp_no_pad))
})
