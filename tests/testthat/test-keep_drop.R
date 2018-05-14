context("keep_drop")

cns <- data.frame(
  dbh = c(0, 50, 100, 150, NA, NA, NA),
  status = c(rep("A", 4), "M", "D", NA),
  stringsAsFactors = FALSE
)

test_that("works as expected", {
  expect_length(keep_dbh_max(cns, 100), 2)
  expect_equal(nrow(keep_dbh_max(cns, 100)), 6)

  x <- 100
  expect_equal(max(keep_dbh_max(cns, x)$dbh, na.rm = T), x)
  expect_equal(min(keep_dbh_min(cns, x)$dbh, na.rm = T), x)
  expect_true(min(keep_dbh_over(cns, x)$dbh, na.rm = T) > x)
  expect_true(max(keep_dbh_under(cns, x)$dbh, na.rm = T) < x)

  expect_equal(unique(keep_status(cns, "A")$status), c("A", NA))
  expect_equal(unique(drop_status(cns, "A", na.rm = T)$status), c("M", "D"))
})

test_that("fails with informative message", {
  expect_error(keep_dbh_min(1), "is not TRUE")
  expect_error(keep_dbh_min(cns), "is not TRUE")
  expect_error(keep_dbh_min(cns, 100, "not logical"), "is not TRUE")
})
