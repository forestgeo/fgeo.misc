context("pick_top")

df <- data.frame(x = 1:9, y = letters[1:3], stringsAsFactors = FALSE)

test_that("var can be reffered bared, quoted and by position", {
  # `var` can be bare or quoted
  (result <- pick_top(df, "y"))
  expect_equal(pick_top(df, y), result)

  # matching `var` by position starting from the left
  expect_equal(pick_top(df, var = y), pick_top(df, var = 2))
  # matching `var` by position starting from the right
  expect_equal(pick_top(df, var = y), pick_top(df, var = -1))
  expect_equal(
    unique(pick_top(df, y, n = 2)$y),
    c("a", "b")
  )
  # Negative values select from the tail
  expect_equal(
    unique(pick_top(df, y, n = -2)$y),
    c("b", "c")
  )
})
