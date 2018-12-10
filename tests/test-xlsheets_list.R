context("xlsheets_list")

input <- misc_example("example.xlsx")
x <- xlsheets_list(input)

test_that("input is a list of data frames", {
  expect_type(x, "list")
  expect_true(each_list_item_is_df(x))
})
