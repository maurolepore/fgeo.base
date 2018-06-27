context("print_all.R")

test_that("errrs with informative message", {
  expect_error(print_all(data.frame(1)), "must be a tibble")
})

test_that("prints all rows", {
  skip_if_not_installed("tibble")
  tbl <- tibble::tibble(x = as.character(runif(21)))
  tbl[21, "x"] <-"row21"
  expect_output(print_all(print_all(tbl)), "row21")
})
