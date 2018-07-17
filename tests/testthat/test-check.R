# Check inputs ------------------------------------------------------------

context("flag_multiple_vector")

single <- rep(1, 3)
num <- c(1:3)
chr <- c(letters[1:3])

test_that("behaves as expected", {
  expect_silent(flag_multiple_vector(single, warning))
  expect_warning(flag_multiple_vector(num, warning))
  expect_error(flag_multiple_vector(chr, stop, "Do something"), "Do something")
})

context("flag_multiple")

.df <- data.frame(a = 1:3, b = 1, stringsAsFactors = FALSE)

test_that("returns the expected messages and output x", {
  expect_silent(flag_multiple(.df, "b"))
  expect_warning(flag_multiple(.df, "a"), "Multiple values")
  expect_warning(flag_multiple(.df, "a", warning, "Custom msg"), "Custom msg")
  expect_warning(flag_multiple(.df, "a", rlang::warn, "Custom"), "Custom")
  expect_error(flag_multiple(.df, "a", stop, "do this"), "do this")
  expect_silent(out <- flag_multiple(.df, "b", stop, "do this"))
  expect_identical(.df, out)
})

test_that("doesn't deqal directly with grouped data to work within groups", {
  # Single within groups but multiple accross entire dataset
  .df <- data.frame(
    a = c(1, 1, 2, 2), b = c(1, 1, 2, 2),
    stringsAsFactors = FALSE
  )

  by_x <- dplyr::group_by(.df, a)
  expect_warning(flag_multiple(by_x, "b"), "Multiple values")

  # To deal with grouped data, apply flag_multiple to each group
  expect_silent(fgeo.tool::by_group(by_x, flag_multiple, "b"))
})
