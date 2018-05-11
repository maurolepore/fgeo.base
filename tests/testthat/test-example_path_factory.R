context("test-example_path_factory.R")

test_that("passes as expected", {
  # Similar to fs::path
  expect_silent(example_path <- example_path_factory("fgeo.base", file.path))
  expect_is(example_path(path = "folder/file.csv"), "character")

  # Similar to fs::path_file
  example_path_file <- example_path_factory("fgeo.base", basename)
  expect_equal(example_path_file(path = "folder/file.csv"), "file.csv")
})

test_that("fails as expected", {
  expect_error(example_path_factory("fgeo.base", wrong), "is not TRUE")
  expect_error(example_path_factory(1, basename), "is not TRUE")
})

