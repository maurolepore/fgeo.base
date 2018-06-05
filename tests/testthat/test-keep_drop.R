context("pick_drop")

cns <- data.frame(
  dbh = c(0, 50, 100, 150, NA, NA, NA),
  status = c(rep("A", 4), "M", "D", NA),
  stringsAsFactors = FALSE
)

test_that("works as expected", {
  expect_length(pick_dbh_max(cns, 100), 2)
  expect_equal(nrow(pick_dbh_max(cns, 100)), 6)

  x <- 100
  expect_equal(max(pick_dbh_max(cns, x)$dbh, na.rm = T), x)
  expect_equal(min(pick_dbh_min(cns, x)$dbh, na.rm = T), x)
  expect_true(min(pick_dbh_over(cns, x)$dbh, na.rm = T) > x)
  expect_true(max(pick_dbh_under(cns, x)$dbh, na.rm = T) < x)

  expect_equal(unique(pick_status(cns, "A")$status), c("A", NA))
  expect_equal(unique(drop_status(cns, "A", na.rm = T)$status), c("M", "D"))
})

test_that("fails with informative message", {
  not_lengh_1 <- c("A", "A")
  expect_error(pick_status(cns, not_lengh_1), "is not TRUE")
  not_lengh_1 <- c(1, 2)
  expect_error(pick_dbh_min(cns, not_lengh_1), "is not TRUE")

  expect_error(pick_dbh_min(1), "is not TRUE")
  expect_error(pick_dbh_min(cns), "is not TRUE")
  expect_error(pick_dbh_min(cns, 100, "not logical"), "is not TRUE")
})
