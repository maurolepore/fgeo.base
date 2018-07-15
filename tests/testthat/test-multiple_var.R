context("multiple_var")

test_that("multiple_censusid() works as epxected with any case", {
  multiple_censusid <- multiple_var("censusid")
  expect_true(multiple_censusid(data.frame(CensusID = c(1, 2, NA))))
  expect_true(multiple_censusid(data.frame(censusid = c(1, 2, NA))))
})
