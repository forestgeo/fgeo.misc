context("list_csv")

output <- tempdir()
test_that("errs with wrong input", {
  expect_error(list_csv(1, output))
  expect_error(list_csv(list(1), output))
})

test_that("works as expected", {
  lst <- list(df1 = data.frame(x = 1), df2 = data.frame(x = 2))
  output <- tempdir()
  list_csv(lst, output, prefix = "myfile-")
  files <- dir(output, pattern = "myfile")
  expect_true(length(files[grepl("^myfile.*csv$", files)]) > 0)
})



context("list_df")

lst <- list(
  a = data.frame(x = 1),
  b = data.frame(x = 2, y = 2),
  c = data.frame(x = 1, z = 3)
)

test_that("errs with wrong input", {
  expect_error(list_df(1))
  expect_error(list_df(data.frame(1)))
})

test_that("works as expected", {
  x <- list_df(lst, df_names = c("a", "c"))
  expect_equal(names(x), c("x", "z"))
  
  x <- list_df(lst, df_names = c("b", "c"))
  expect_equal(names(x), c("x", "y", "z"))
  expect_silent(list_df(list(data.frame(1))))
  expect_silent(
    list_df(
      list(data.frame(x = 1), data.frame(z = 2)),
      by = c("x" = "z")
    )
  )
})

