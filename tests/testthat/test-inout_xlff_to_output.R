library(dplyr)
library(purrr)
library(readr)
library(fs)

context("xlff_to_list")

test_that("outputs expected dataframe", {
  out <- xlff_to_list(misc_example("two_files"))
  expect_is(out, "list")

  nms <- c(
    "submission_id", "section_id", "quadrat", "tag", "stem_tag", "species",
    "lx", "ly", "dbh", "status", "codes", "pom", "dbh_2018", "status_2018",
    "codes_2018", "notes", "data_check", "dbh_check", "sheet", "px", "py",
    "new_stem", "unique_stem", "date",
    "start_form_time_stamp", "end_form_time_stamp"
  )
  expect_named(out[[1]], nms, ignore.order = TRUE)
})



context("xlff_to_xl")

test_that("works as expected", {
  output <- tempdir()

  expect_silent(xlff_to_xl(misc_example("new_stem_1"), output))

  # Check output
  files <- dir(output)
  matching_file <- file <- files[files == "new_stem_1.xlsx"]
  expect_true(length(matching_file) > 0)
  matching_path <- fs::path(output, matching_file)
  expect_is(readxl::read_xlsx(matching_path), "data.frame")
})



context("xlff_to_csv")

test_that("errs if `dir` does not exist", {
  expect_error(
    xlff_to_csv("invalid_dir"),
    "must match a valid directory"
  )
})

test_that("errs without excel file", {
  # Empty tempdir()
  file_delete(dir_ls(tempdir()))

  msg <- "must contain at least one excel file"
  expect_error(xlff_to_csv(tempdir()), msg)
})

test_that("errs with informative message with input of wrong type", {
  not_a_string <- 1
  expect_error(xlff_to_csv(not_a_string))
  expect_error(xlff_to_csv("./sheets", not_a_string))
})

test_that("works as expected", {
  output <- tempdir()
  expect_silent(
    xlff_to_csv(misc_example("new_stem_1"), output)
  )

  files <- dir(output)
  matching_file <- file <- files[grepl("^new_stem_1.*csv$", files)]
  expect_true(length(matching_file) > 0)

  matching_path <- fs::path(output, matching_file)
  output_files <- names(suppressMessages(read_csv(matching_path)))
  expect_true("sheet" %in% output_files)
})

test_that("warns if it detects no new stem and fills cero-row dataframes", {
  output <- tempdir()
  expect_warning(
    xlff_to_csv(misc_example("new_stem_0"), output),
    "new_secondary_stems"
  )
  expect_warning(
    xlff_to_csv(misc_example("new_stem_0"), output),
    "Filling every cero-row"
  )
})

test_that("warns if it detects no recruits (#11)", {
  expect_warning(
    xlff_to_csv(misc_example("recruits_none"), tempdir()),
    "recruits"
  )
})

test_that("outputs column date (#12)", {
  output <- tempdir()
  xlff_to_csv(misc_example("new_stem_1"), output)

  exported <- read_csv(fs::path(output, "new_stem_1.csv"))
  expect_true(any(grepl("date", names(exported))))
  expect_equal(nrow(filter(exported, is.na(date))), 0)
})

test_that("outputs column codes with commas replaced by semicolon (#13)", {
  output <- tempdir()
  xlff_to_csv(misc_example("new_stem_1"), output)

  exported <- read_csv(fs::path(output, "new_stem_1.csv"))

  has_comma <- exported %>%
    select(matches("codes")) %>%
    map(~stringr::str_detect(.x, ",")) %>%
    map(~.x[!is.na(.x)]) %>%
    map_lgl(any) %>%
    any()
  expect_false(has_comma)
})

test_that("allows first_census", {
  output_dir <- tempdir()
  out <- xlff_to_list(misc_example("first_census"), first_census = TRUE)[[1]]

  nms <- c(
    "submission_id", "quadrat", "tag", "stem_tag", "species",
    "species_code", "dbh", "status", "codes", "notes", "pom", "sheet",
    "section_id", "unique_stem", "date",
    "start_form_time_stamp", "end_form_time_stamp"
  )
  expect_equal(sort(names(out)), sort(nms))
})

test_that("passes with input missing key sheets (#33)", {
  expect_warning(
    xlff_to_list(misc_example("missing_key")),
    "Adding missing sheets: original_stems, new_secondary_stems, recruits, root"
  )
})

