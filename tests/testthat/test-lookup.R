context("lookup")

library(dplyr)

test_that("returns expected spliced list", {
  look <- tibble::tibble(
    old = c("a", "b"),
    new = c("apple", "banana")
  )
  
  codes <- lookup(look$old, look$new)
  expect_type(codes, "list")
  expect_is(codes, "rlang_box_splice")
  
  x <- c("a", "c", "a", "b", "a")
  recoded <- c("apple", "c", "apple", "banana", "apple")
  expect_equal(result <- recode(x, codes), recoded)

  alternative <- recode(x, !!!as.list(set_names(look$new, look$old)))
  expect_identical(result, alternative)
})

test_that("fails with informative error", {
  expect_error(lookup(letters[1:3], LETTERS[1]), ("must be of equal length."))
  expect_error(lookup(letters[1], LETTERS[1:3]), ("must be of equal length."))
})
