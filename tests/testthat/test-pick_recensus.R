context("pick_recensus.R")

test_that("finds three stems known to be missing", {
  x <- dplyr::tribble(
    ~unique_stem, ~quadrat,
          "01_1",    "001",
          "02_1",    "001",
          "02_2",    "001"
  )
  y <- dplyr::tribble(
    ~unique_stem,
          "01_1",
          "02_2"
  )
  expect <- "02_1"
  
  expect_message({
    out <- pick_recensus(x, y)
  })
  expect_equal(out$unique_stem, expect)
  
})

test_that("works with parameter `by` as expected", {
  x <- dplyr::tribble(
    ~unique_stem, ~quadrat,
          "01_1",    "001",
          "02_1",    "001",
          "02_2",    "001"
  )
  y <- dplyr::tribble(
    ~unique_stem,
          "01_1",
          "02_2"
  )
  expect <- "02_1"

  expect_silent({
    out <- pick_recensus(x, y, by = "unique_stem")
  })
  expect_equal(out$unique_stem, expect)
  
  y2 <- dplyr::tribble(
    ~unq_stem,
    "01_1",
    "02_2"
  )
  out2 <- pick_recensus(x, y2, by = c("unique_stem" = "unq_stem"))
  expect_equal(out, out2)
})
