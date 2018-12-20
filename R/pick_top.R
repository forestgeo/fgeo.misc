#' Pick dataframe rows by matching n values from the head (or tail) of column.
#'
#' This function picks rows from a dataframe by matching n unique values from
#' the head (or tail) of a column. It is similar to [dplyr::top_n()], but instead of
#' using `min_rank()` or `max_rank()`, it uses [utils::head()] or
#' [utils::tail()]; and `var` is flexible as in [dplyr::pull()].
#'
#' @param .data A table of data.
#' @param var A variable specified as:
#'   * a literal variable name
#'   * a positive integer, giving the position counting from the left
#'   * a negative integer, giving the position counting from the right.
#'   
#' The default returns the last column (on the assumption that is the column you
#' have created most recently). This argument is taken by expression and
#' supports quasiquotation (you can unquote column names and column positions).
#' @param n Number of values used for matching, from the head (or tail) of
#'   `var`.
#' 
#' @seealso [dplyr::pull], [dplyr::top_n], [utils::head()], [utils::tail()].
#'
#' @return A filtered version of the input dataset.
#' @export
#' @family general functions to pick or drop rows of a dataframe
#'
#' @examples
#' df <- data.frame(x = 1:9, y = letters[1:3], stringsAsFactors = FALSE)
#'
#' # `var` can be bare or quoted
#' (result <- pick_top(df, "y"))
#' identical(pick_top(df, y), result)
#'
#' # matching `var` by position starting from the left
#' identical(pick_top(df, var = y), pick_top(df, var = 2))
#' # matching `var` by position starting from the right
#' identical(pick_top(df, var = y), pick_top(df, var = -1))
#'
#' pick_top(df, y, n = 2)
#' # Negative values select from the tail
#' pick_top(df, y, n = -2)
pick_top <- function(.data, var, n = 1) {
  var <- enquo(var)
  pulled <- dplyr::pull(.data, !!var)
  sorted <- sort(unique(pulled))
  if (n > 0 ) {
    to_match <- head(sorted, n)
  } else {
    to_match <- tail(sorted, abs(n))
  }
  .data[pulled %in% to_match, ]
}
