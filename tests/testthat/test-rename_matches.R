context("rename_matches")

test_that("works as expected", {
  x <- data.frame(col1 = 5, col2 = 1, n = 5)

  y <- data.frame(COL1 = 1, COL2 = 1, COL3 = 1)
  expect_named(rename_matches(x, y), c("COL1", "COL2", "n"))

  y <- data.frame(COL1 = 1)
  expect_named(rename_matches(x, y), c("COL1", "col2", "n"))

  y <- data.frame(NOMATCH = 1)
  expect_named(rename_matches(x, y), c("col1", "col2", "n"))

  y <- data.frame(1)
  expect_named(rename_matches(x, y), c("col1", "col2", "n"))
})



context("detect_insensitive")
context("extract_insensitive")

test_that("detect_insensitive and extract_insensitive work as expected", {
  x <- c("stemid", "n")
  y <- c("StemID", "treeID")
  expect_equal(detect_insensitive(x, y), c(TRUE, FALSE))
  expect_equal(extract_insensitive(x, y), "StemID")
})

test_that("fail if input is not a string", {
  expect_error(detect_insensitive(1), "is not TRUE")
  expect_error(extract_insensitive(1), "is not TRUE")
  expect_error(detect_insensitive(data.frame(1)), "is not TRUE")
  expect_error(extract_insensitive(data.frame(1)), "is not TRUE")
})

