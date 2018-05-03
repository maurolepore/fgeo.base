context("test-find_datasets.R")

test_that("outputs expected datasets", {
  expected <- c("luquillo_stem_random_tiny", "luquillo_vft_4quad")
  out <- find_datasets("fgeo.base")
  expect_true(all(out %in% expected))
})
