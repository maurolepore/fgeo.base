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



context("flag_name_if")

.df <- data.frame(a = 1:3, b = 1, stringsAsFactors = FALSE)

test_that("flags a variable with multiple values", {
  expect_silent(flag_name_if(.df, "b", is_multiple))
  expect_warning(flag_name_if(.df, "a", is_multiple))
  expect_error(flag_name_if(.df, "a", is_multiple, stop, "Custom"), "Custom")
})

test_that("is insensitive to name-case", {
  expect_error(flag_name_if(.df, "A", is_multiple, stop, "Custom"), "Custom")
  expect_silent(flag_name_if(.df, "B", is_multiple, stop, "Custom"))
})

test_that("returns `cond`", {
  msg <- "Flagged values were detected"
  dfm <- function(x) data.frame(Name = x, stringsAsFactors = TRUE)
  .data <- dfm(c(1, 1))
  expect_warning(flag_name_if(.data, "Name", is_duplicated, warning, msg))

  expect_error(flag_name_if(.data, "Name", is_duplicated, stop, msg))
  .data <- dfm(c(1, 2))
  expect_silent(flag_name_if(.data, "Name", is_duplicated, stop))
})

test_that("includes in the message the name of the variable being tested", {
  tree <- data.frame(treeID = c(1, 1), stringsAsFactors = FALSE)
  expect_warning(flag_name_if(tree, "treeID", is_duplicated), "treeid")
})

test_that("doesn't deal directly with grouped data to work within groups", {
  skip_if_not_installed("dplyr")
  library(dplyr)

  # Single within groups but multiple accross entire dataset
  .df <- tibble(a = c(1, 1, 2, 2), b = c(1, 1, 2, 2))

  by_a <- group_by(.df, a)
  warn_if_b_is_multiple <- function(.data) flag_name_if(.data, "b", is_multiple)
  expect_warning(warn_if_b_is_multiple(by_a), "Flagged values")

  # To deal with grouped data, apply flag_multiple_f to each group
  warn_if_b_is_multiple <- function(.data) flag_name_if(.data, "b", is_multiple)
  expect_silent(fgeo.tool::by_group(by_a, warn_if_b_is_multiple))
})



context("detect_if")

test_that("works with any case", {
  dfm <- data.frame(CensusID = c(1, 2, NA))
  expect_true(detect_if(dfm, "censusid", is_multiple))
  expect_false(detect_if(dfm, "censusid", is_duplicated))

  dfm <- data.frame(CensusID = c(1, 1))
  expect_true(detect_if(dfm, "censusid", is_duplicated))
  expect_false(detect_if(dfm, "censusid", is_multiple))

  dfm <- data.frame(CensusID = c(1, 1, 2))
  expect_true(detect_if(dfm, "censusid", is_duplicated))
  expect_true(detect_if(dfm, "censusid", is_multiple))
})

test_that("rejects invalid var", {
  dfm <- data.frame(CensusID = c(1, 2, NA))
  expect_error(detect_if(dfm, "bad", is_multiple), "invalid name")
  expect_error(detect_if(dfm, "bad", is_duplicated), "invalid name")
})

dfm <- function(x) data.frame(Name = x, stringsAsFactors = TRUE)
test_that("creates a function that detects duplicates on a specific variable", {
  expect_true(detect_if(dfm(c(1, 1)), "Name", is_duplicated))
  expect_false(detect_if(dfm(c(1, NA)), "Name", is_duplicated))
  expect_false(detect_if(dfm(c(1, 2)), "Name", is_duplicated))
})

test_that("works with upper or lowercase name", {
  expect_true(detect_if(dfm(c(1, 1)), "Name", is_duplicated))
  expect_true(detect_if(dfm(c(1, 1)), "name", is_duplicated))

  expect_false(detect_if(dfm(c(1, 2)), "Name", is_duplicated))
  expect_false(detect_if(dfm(c(1, 2)), "name", is_duplicated))
})

test_that("ignores groups but groups can be handled via map(nest()$data)", {
  skip_if_not_installed("tidyr")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("purrr")
  library(tidyr)
  library(dplyr)
  library(purrr)

  dfm <- data.frame(x = c(1, 1), g = c(1, 2), stringsAsFactors = TRUE)
  expect_true(detect_if(group_by(dfm, g), "x", is_duplicated))
  grouped <- group_by(dfm, g)
  expect_false(any(
    map_lgl(nest(grouped)$data, ~detect_if(.x, "x", is_duplicated))
  ))
})

