context("detect_duplicated")

describe("detect_duplicated", {
  it("returns true if any value of a variable is duplicated", {
    expect_true(detect_duplicated(c(1, 1)))
    expect_false(detect_duplicated(c(1, NA)))
    expect_false(detect_duplicated(c(1, 2)))
  })
})

describe("flag_duplicated", {
  it("returns `cond`", {
    msg <- "Duplicated values were detected"
    expect_warning(flag_duplicated(c(1, 1), warning), msg)
    expect_message(flag_duplicated(c(1, 1), rlang::inform), msg)
    expect_error(flag_duplicated(c(1, 1), stop, "Custom msg"), "Custom msg")
    expect_silent(flag_duplicated(c(1, NA), warning))
    expect_silent(flag_duplicated(c(1, 2), warning))
  })
})



context("flag_multiple")

single <- rep(1, 3)
num <- c(1:3)
chr <- c(letters[1:3])

test_that("behaves as expected", {
  expect_silent(flag_multiple(single, warning))
  expect_warning(flag_multiple(num, warning))
  expect_error(flag_multiple(chr, stop, "Do something"), "Do something")
})

test_that("ommits NAs", {
  expect_silent(flag_multiple(c(1, NA), warning))
})



context("flag_multiple_f")

.df <- data.frame(a = 1:3, b = 1, stringsAsFactors = FALSE)

test_that("returns the expected messages and output", {
  expect_silent(flag_multiple_f("b")(.df))
  expect_warning(flag_multiple_f("a")(.df), "Multiple values")
  expect_warning(flag_multiple_f("a", warning)(.df, "Custom msg"), "Custom msg")
  expect_warning(flag_multiple_f("a", rlang::warn)(.df, "Custom"), "Custom")
  expect_error(flag_multiple_f("a", stop)(.df, "do this"), "do this")
  expect_silent(out <- flag_multiple_f("b", stop)(.df, "do this"))
  expect_identical(.df, out)
})

test_that("doesn't deal directly with grouped data to work within groups", {
  skip_if_not_installed("dplyr")
  library(dplyr)

  # Single within groups but multiple accross entire dataset
  .df <- tibble(a = c(1, 1, 2, 2), b = c(1, 1, 2, 2))

  by_a <- group_by(.df, a)
  expect_warning(flag_multiple_f("b")(by_a), "Multiple values")

  # To deal with grouped data, apply flag_multiple_f to each group
  flag_if_multiple_b <- flag_multiple_f("b")
  expect_silent(fgeo.tool::by_group(by_a, flag_if_multiple_b))
})



context("detect_multiple_f")

test_that("multiple_censusid() works as epxected with any case", {
  multiple_censusid <- detect_multiple_f("censusid")
  expect_true(multiple_censusid(data.frame(CensusID = c(1, 2, NA))))
  expect_true(multiple_censusid(data.frame(censusid = c(1, 2, NA))))
})

test_that("rejects invalid var", {
  dfm <- data.frame(CensusID = c(1, 2, NA))
  expect_error(detect_multiple_f("bad")(dfm), "invalid name")
})
