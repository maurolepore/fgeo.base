context("test-nms_restore_matching")

test_that("works as expected", {
  new <- data.frame(Column = 5, Other = 1, n = 5)
  ref <- data.frame(COLUMN = 1, other = 1)
  expect_named(nms_restore_matching(new, ref), c("COLUMN", "other", "n"))
})
