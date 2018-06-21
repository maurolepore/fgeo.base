context("glue_comma")

test_that("output the expected string", {
  expect_equal(glue_comma(1:3), "1, 2, 3")
  expect_equal(glue_pipe(1:3), "1|2|3")
})
