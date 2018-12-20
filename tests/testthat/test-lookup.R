context("lookup")

library(dplyr)

test_that("returns expected spliced list", {
  set.seed(1)
  # Lookup table
  look <- tibble::tibble(
    old = c("a", "b"),
    new = c("apple", "banana")
  )
  # Vector
  x <- sample(c("a", "b", "c"), 5, replace = TRUE)
  codes <- lookup(look$old, look$new)

  expect_type(codes, "list")
  expect_is(codes, "spliced")

  recoded <- c("apple", "banana", "banana", "c", "apple")
  expect_equal(result <- recode(x, codes), recoded)

  alternative <- recode(x, !!!as.list(set_names(look$new, look$old)))
  expect_identical(result, alternative)
})

test_that("fails with informative error", {
  expect_error(lookup(letters[1:3], LETTERS[1]), ("must be of equal length."))
  expect_error(lookup(letters[1], LETTERS[1:3]), ("must be of equal length."))
})
