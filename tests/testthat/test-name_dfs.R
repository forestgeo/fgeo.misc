context("name_dfs.R")

test_that("outputs list of dataframes with expected names", {
  dfs <- list(
    a = data.frame(x = 1),
    data.frame(x = 1)
  )
  out <- name_dfs(dfs)
  expect_is(out, "list")
  expect_is(out[[1]], "data.frame")
  expect_equal(names(out[[1]]), c("x", "name"))
  expect_equal(names(out), c("a", "df2"))

  out2 <- name_dfs(dfs, "custom")
  expect_equal(names(out2[[2]]), c("x", "custom"))
})

test_that("fails with wrong input", {
  expect_error(name_dfs(1), "is not TRUE")
  expect_error(name_dfs(list(1)), "is not TRUE")
  expect_error(name_dfs(list(data.frame(1)), 1), "is not TRUE")
})

test_that("Errs if has cero row/column dataframe", {
  cero_row_col <-  data.frame()
  dfs3 <- list(a = cero_row_col, b = data.frame(1))
  expect_error(name_dfs(dfs3), "must have at least one row/column")

  df <- data.frame(x = 1)
  cero_row <- df[0, , drop = FALSE]
  dfs4 <- list(a = cero_row, b = data.frame(1))
  expect_error(name_dfs(dfs4), "must have at least one row/column")

  df <- data.frame(x = 1)
  cero_col <- df[0]
  dfs5 <- list(a = cero_row, b = data.frame(1))
  expect_error(name_dfs(dfs5), "must have at least one row/column")
})
