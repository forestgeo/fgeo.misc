context("fill_na")

test_that("returns expected value", {
  df <- data.frame(x = c(NA, 1), y = c("a", NA), stringsAsFactors = FALSE)
  out <- fill_na(df, "z")
  expect_equal(out$x[[1]], "z")
  expect_equal(out$y[[2]], "z")
})
