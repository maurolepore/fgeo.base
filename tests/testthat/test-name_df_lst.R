context("test-name_df_lst.R")

test_that("outputs list of dataframes with expected names", {
  dfs <- list(
    a = data.frame(x = 1),
    data.frame(x = 1)
  )
  out <- name_df_lst(dfs)
  expect_is(out, "list")
  expect_is(out[[1]], "data.frame")
  expect_equal(names(out[[1]]), c("x", "name"))
  expect_equal(names(out), c("a", "df2"))

  out2 <- name_df_lst(dfs, "custom")
  expect_equal(names(out2[[2]]), c("x", "custom"))
})

test_that("fails with wrong input", {
  expect_error(name_df_lst(1), "is not TRUE")
  expect_error(name_df_lst(list(1)), "is not TRUE")
  expect_error(name_df_lst(list(data.frame(1)), 1), "is not TRUE")
})
