context("flag_if")

test_that("flag_if flaggs if predicate is true", {
  dupl <- c(1, 1)
  expect_warning(flag_if(dupl, is_duplicated))
  expect_silent(flag_if(dupl, is_multiple))

  mult <- c(1, 2)
  expect_message(flag_if(mult, is_multiple, message, "Custom"), "Custom")
  expect_silent(flag_if(mult, is_duplicated))

  expect_silent(flag_if(c(1, NA), is_multiple))
  expect_silent(flag_if(c(1, NA), is_duplicated))
})
