context("top")

test_that("picks the first from the head", {
  expect_equal(top(1:3), 1L)
  expect_equal(top(1:3, 1L), 1L)
  expect_equal(top(1:3, 2L), 1:2)
})

test_that("picks the first from the tail", {
  expect_equal(top(1:3, -1L), 3)
})



context("in_top")

test_that("returns TRUE in position 1", {
  expect_equal(in_top(1:3, 1L), c(TRUE, F, F))
})

test_that("returns TRUE in position 3", {
  expect_equal(in_top(1:3, -1L), c(F, F, TRUE))
})

test_that("works with character vectors", {
  expect_equal(top(letters[1:2]), "a")
  expect_equal(top(letters[1:2], -1L), "b")
})

test_that("works with doubles", {
  expect_equal(top(c(1.1, 2.2)), 1.1)
  expect_equal(top(c(1.1, 2.2), -1L), 2.2)
})

test_that("Errs with informative message", {
  expect_error(top(list(1, 2)), "must be atomic")
})
