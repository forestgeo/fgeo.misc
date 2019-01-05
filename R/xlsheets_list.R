#' Import mapping each spreadsheet of an excel file to a dataframe in a list.
#'
#' @param path A path to a single excel file.
#'
#' @family functions to handle multiple spreadsheets of an excel workbook.
#' @family general functions to import data
#'
#' @source Adapted from an article by Jenny Bryan (https://goo.gl/ah8qkX).
#' @return A list of dataframes.
#'
#' @export
#' @examples
#' xlsheets_list(misc_example("multiple_sheets.xlsx"))
xlsheets_list <- function(path) {
  # Piping to avoid useless intermediate variables
  path %>%
    readxl::excel_sheets() %>%
    rlang::set_names() %>%
    purrr::map(readxl::read_excel, path = path)
}

#' Import excel files from a directory into a list.
#'
#' @param path String; the path to a directory containing the files to read
#'   (all must be of appropriate format; see examples).
#' @param ... Arguments passed to the reader function.
#' @examples
#' path_xl <- misc_example("xl")
#' path_xl
#' dir(path_xl)
#' xl_list(path_xl)
#' @name xl_list
NULL

#' @rdname xl_list
#' @export
xl_list <- function(path) {
  tor::list_any(path, .f = readxl::read_excel, regexp = "[.]xls$|[.]xlsx$")
}

#' @rdname xl_list
#' @export
xl_list <- function(path) {
  tor::list_any(path, .f = xlsheets_list, regexp = "[.]xls$|[.]xlsx$")
}
