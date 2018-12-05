context("condition_na.R")

test_that("outputs warnings as expected", {
  expect_warning(warn_na(c(x = 1, y = NA)))
  expect_error(warn_na(c(x = 1, 1)))
})

test_that("outputs error as expected", {
  expect_error(abort_na(c(x = 1, y = NA)))
})

test_that("outputs message as expected", {
  expect_message(inform_na(c(x = 1, y = NA)))
})
