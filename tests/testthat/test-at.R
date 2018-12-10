context("conv_unit")

test_that("output has correct structure", {
  expect_is(conv_unit(1, "mm", "m"), "numeric")
  
  dfm <- data.frame(a = 1, b = 1)
  expect_is(
    out <- convert_unit_at(dfm, .at = "a", "mm", "m"), "data.frame"
  )
  expect_named(out, c("a", "b"))
  expect_equal(out[[1]], 1 / 1000)
  expect_equal(out[[2]], 1)
})

test_that("converts correctly", {
  expect_equal(conv_unit(123, "kph", "m_per_sec"), 123 * 1000 / (60 * 60))
  expect_equal(conv_unit(1, "hectare", "m2"), 10000)
})
