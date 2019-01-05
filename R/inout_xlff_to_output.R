#' Read and wrangle excel-FastField and output dataframes or .csv/.xlsx files.
#'
#' These functions read and wrangle excel workbooks produced by the FastField
#' app and output dataframes (`xlff_to_list()` and `xlff_to_df()`), .csv files
#' (`xlff_to_csv()`) or .xlsx files (`xlff_to_xl()`). Each dataframe or file
#' combines all spreadsheets from a single excel workbook in the input
#' directory. If the input directory has multiple workbooks, the output will be
#' multiple dataframes, multiple .csv files, or multiple .excel files.
#'
#' This is a rigid function with a very specific goal: To process data from
#' FastField forms. Specifically, this is what this function does:
#' * Reads each spreadsheet from each workbook and map it to a dataframe.
#' * Lowercases and links the names of each dataframe.
#' * Adds any missing __key-sheets__:
#'     * For first census: (1) "root", (2) "multi_stems", (3) "secondary_stems",
#'     and (4) "single_stems".
#'     * For recensus: (1) "root", (2) "original_stems", (3)
#'     "new_secondary_stems", and (4) "recruits"
#' * Dates the data by `submission_id` (`date` comes from the spreadsheet
#' `root`).
#' * Lowercases and links the names of each dataframe-variable.
#' * Drops fake stems.
#' * Output a common data structure of your choice.
#'
#' @param dir_in String giving the directory containing the excel workbooks
#'   to read from.
#' @param dir_out String giving the directory where to write .csv files to.
#' @param first_census Use `TRUE` if this is your first census. Use `FALSE`
#'   (default) if this is not your first census but a recensus.
#'
#' @keywords internal
#'
#' @return `xlff_to_csv()` and `xlff_to_xl()` write a .csv or excel (.xlsx) file
#'   per workbook -- combining all spreadsheets. `xlff_to_list` outputs a list
#'   where each dataframes combines all spreadsheeets of a workbook.
#'
#' @author Mauro Lepore and Jessica Shue.
#'
#' @section Acknowledgment:
#' * Sabrina Russo helped to make these functions useful with first censuses.
#' * David Orwig helped to fix a debug.
#'
#' @examples
#' library(fs)
#' library(readr)
#' library(readxl)
#'
#' # NOT A FIRST CENSUS
#' # Path to the folder I want to read excel files from
#' dir_in <- dirname(misc_example("two_files/new_stem_1.xlsx"))
#' dir_in
#'
#' # Files I want to read
#' dir(dir_in, pattern = "xlsx")
#'
#' # Path to the folder I want to write .csv files to
#' dir_out <- tempdir()
#'
#' # Output a csv file
#' xlff_to_csv(dir_in, dir_out)
#'
#' # Confirm
#' path_file(dir_ls(dir_out, regexp = "new_stem.*csv$"))
#'
#' # Also possible to output excel and a list of dataframe. See next section.
#'
#' # FIRST CENSUS
#' dir_in <- dirname(misc_example("first_census/census.xlsx"))
#' # As a reminder you will get a warning of missing sheets
#' # Output list of dataframes (one per input workbook -- here only one)
#' lst <- xlff_to_list(dir_in, first_census = TRUE)
#' str(lst, give.attr = FALSE)
#'
#' # Output excel
#' xlff_to_xl(dir_in, dir_out, first_census = TRUE)
#' # Read back
#' filename <- path(dir_out, "census.xlsx")
#' out <- read_excel(filename)
#' str(out, give.attr = FALSE)
#' @name xlff_to_output
NULL

xlff_to_file <- function(ext, fun_write) {
    function(dir_in, dir_out = "./", first_census = FALSE) {
    check_dir_out(dir_out = dir_out, print_as = "`dir_out`")
    lst <- xlff_to_list(dir_in = dir_in, first_census = first_census)
    files <- fs::path_ext_remove(names(lst))
    paths <- fs::path(dir_out, fs::path_ext_set(files, ext))
    purrr::walk2(lst, paths, fun_write)
  }
}

#' @export
#' @rdname xlff_to_output
xlff_to_csv <- xlff_to_file("csv", readr::write_csv)

#' @export
#' @rdname xlff_to_output
xlff_to_xl <- xlff_to_file("xlsx", writexl::write_xlsx)

#' @export
#' @rdname xlff_to_output
xlff_to_list <- function(dir_in, first_census = FALSE) {
  check_dir_in(dir_in = dir_in, print_as = "`dir_in`")
  out <- purrr::map(
    xl_workbooks_to_chr(dir_in),
    xlff_to_list_, first_census = first_census
  )
  rlang::set_names(out, basename(names(out)))
}

#' Do xlff_to_list() for each excel file.
#' @noRd
xlff_to_list_ <- function(file, first_census = FALSE) {
  dfm_list <- nms_tidy(xlsheets_list(file))

  if (first_census) {
    key <- key_first_census()
    dfm_list <- ensure_key_sheets(dfm_list, key)
  } else {
    key <- key_recensus()
    dfm_list <- ensure_key_sheets(dfm_list, key)
  }

  # Piping functions to avoid useless intermediate variables
  clean_dfm_list <- dfm_list %>%
    purrr::keep(~!purrr::is_empty(.)) %>%
    lapply(nms_tidy) %>%
    drop_fake_stems()

  # After dropping fake stems new_secondary_stems might be empty (0-row)
  purrr::walk(names(clean_dfm_list), ~warn_if_empty(clean_dfm_list, .x))

  # Sanitize
  sane <- clean_dfm_list %>%
    # Avoid error in naming cero-row dataframes
    warn_if_filling_cero_row_dataframe() %>%
    purrr::modify_if(~nrow(.x) == 0, ~purrr::map_df(.x, ~NA)) %>%
    name_dfs(name = "sheet") %>%
    # Avoid merge errors
    coerce_as_character()

  with_date <- join_and_date(sane)
  # In columns matching "codes", replace commas by semicolon
  .df <- purrr::modify_if(
    with_date, grepl("codes", names(with_date)), ~gsub(",", ";", .x)
  )
  .df
}

#' Check that key spreadsheets exist.
#' @noRd
ensure_key_sheets <- function(x, key) {
  missing_key_sheet <- !all(names(key) %in% names(x))
  if (missing_key_sheet) {
    missing_sheets <- setdiff(names(key), names(x))
    msg <- paste0(
      "Adding missing sheets: ", commas(missing_sheets), "."
    )
    warn(msg)

    missing_appendix <- purrr::map(key[missing_sheets], str_df)
    x <- append(x, missing_appendix)
  }
  x
}



#' Remove rows equal to cero from the spreadsheet sheet new_secondary_stem.
#' @noRd
drop_fake_stems <- function(.df) {
  dropped <- purrr::modify_at(
    .df, .at = "new_secondary_stems", ~.x[.x$new_stem != 0, ]
  )
  dropped
}

#' Warns if a dataframe in a list of dataframes has empty rows.
#' @noRd
warn_if_empty <- function(.x, dfm_nm) {
  dfm <- .x[[dfm_nm]]

  if (is.null(dfm)) {
    warn(paste("`.x` has no dataframe", dfm_nm), ". Is this intentional?")
    return(invisible(.x))
  }

  has_cero_rows <- nrow(dfm) == 0
  if (has_cero_rows) {
    warn(paste0("`", dfm_nm, "`", " has cero rows."))
  }
  invisible(.x)
}

warn_if_filling_cero_row_dataframe <- function(lst) {
  cero_row_dfs <- purrr::keep(lst, ~nrow(.x) == 0)
  if (length(cero_row_dfs) != 0) {
    warning(
      "Filling every cero-row dataframe with NAs (",
      commas(names(cero_row_dfs)), ").",
      call. = FALSE
    )
  }
  invisible(lst)
}

coerce_as_character <- function(.x, ...) {
  purrr::map(.x, ~purrr::modify(., .f = as.character, ...))
}

join_and_date <- function(.x) {
  # From `root`, pull only `date` (plus a column to merge by)
  date <- .x[["root"]][c("submission_id", "date")]

  # Join data from all sheets except from `root`
  is_not_root <- !grepl("root", names(.x))
  not_root_dfm <- purrr::keep(.x, is_not_root)

  # Collapse into a single dataframe, add variable, and join with date
  not_root_dfm %>%
    list_df() %>%
    dplyr::mutate(unique_stem = paste0(.data$tag, "_", .data$stem_tag)) %>%
    dplyr::left_join(date, by = "submission_id")
}

check_dir_in <- function(dir_in, print_as) {
  stopifnot(is.character(dir_in))
  validate_dir(dir_in, "`dir_in`")
  msg <- "`dir_in` must contain at least one excel file."
  file_names <- xl_workbooks_to_chr(dir_in)
  if (length(file_names) == 0) {
    abort(msg)
  }
  invisible()
}

check_dir_out <- function(dir_out, print_as) {
  stopifnot(is.character(dir_out))
  validate_dir(dir_out, "`dir_out`")
  invisible()
}

validate_dir <- function(dir, dir_name) {
  invalid_dir <- !fs::dir_exists(dir)
  if (invalid_dir) {
    msg <- paste0(
      dir_name, " must match a valid directory.\n",
      "bad ", dir_name, ": ", "'", dir, "'"
    )
    abort(msg)
  } else {
    invisible(dir)
  }
}

xl_workbooks_to_chr <- function(dir_in) {
  fs::dir_ls(dir_in, regexp = "\\.xls")
}

commas <- function(...) paste0(..., collapse = ", ")


each_list_item_is_df <- function(x) {
  if (!is.list(x) || is.data.frame(x)) {
    abort("`x` must be a list of datafraems (and not itself a dataframe).")
  }
  all(vapply(x, has_class_df, logical(1)))
}

has_class_df <- function(x) {
  any(grepl("data.frame", class(x)))
}


#' Path to directory containing example data.
#'
#' @param path Path to a file (with extension) from inst/extdata/.
#'
#' @return String (a path)
#' @export
#'
#' @examples
#' misc_example("xl")
#' dir(misc_example("xl"))
misc_example <- function(path) {
  system.file("extdata", path, package = "fgeo.misc")
}

nms_tidy <- function(x) {
  if (rlang::is_named(x)) {
    names(x) <- gsub(" ", "_", tolower(names(x)))
    return(x)
  }
  
  gsub(" ", "_", tolower(x))
}

