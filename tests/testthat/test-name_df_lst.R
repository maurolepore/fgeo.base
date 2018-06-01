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

test_that("Errs if has cero row/column dataframe", {
  cero_row_col <-  data.frame()
  dfs3 <- list(a = cero_row_col, b = data.frame(1))
  expect_error(name_df_lst(dfs3), "must have at least one row/column")

  df <- data.frame(x = 1)
  cero_row <- df[0, , drop = FALSE]
  dfs4 <- list(a = cero_row, b = data.frame(1))
  expect_error(name_df_lst(dfs4), "must have at least one row/column")

  df <- data.frame(x = 1)
  cero_col <- df[0]
  dfs5 <- list(a = cero_row, b = data.frame(1))
  expect_error(name_df_lst(dfs5), "must have at least one row/column")
})
