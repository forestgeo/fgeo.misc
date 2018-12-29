context("pick_plotname")

vft <- data.frame(PlotName = c("a", "b"), stringsAsFactors = FALSE)

test_that("errs as expected", {
  expect_error(pick_plotname("not dfm", "a"), "not TRUE")
  expect_error(pick_plotname(vft, "bad"), "wasn't detected")
  expect_message(pick_plotname(vft), "Using")
})

test_that("has correct structure", {
  out <- suppressMessages(suppressWarnings(pick_plotname(vft, "a")))
  expect_is(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_equal(names(out), "PlotName")
  expect_equal(out[[1]], "a")

  out <- suppressMessages(suppressWarnings(pick_plotname(vft, "b")))
  expect_equal(out[[1]], "b")
})
