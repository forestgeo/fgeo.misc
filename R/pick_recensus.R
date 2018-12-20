#' Pick rows from table x with no-match in table y (e.g. find recensused trees).
#'
#' Based on a reference dataset `x`, this function helps you
#' identify stems that remain to be recensused in a dataset `y`. This
#' function does the same work as [dplyr::anti_join()]. The only difference is
#' that the signature of `pick_recensus()` is a little simpler (irrelevant
#' arguments hidden via `...`) to focus your attention on the arguments that are
#' most useful in helping you identify stems to recensus. This function also
#' exists to help you discover the `*join()` functions of __dplyr__, which will
#' help you solve more general problems.
#' 
#' This function preserves __dplyr__'s style and thus non-standard evaluation.
#' If you want to use it inside your own functions, you should learn about tidy
#' eval (implemented via the __rlang__ package). A good place to start is at
#' __dplyr__'s website.
#'
#' @param x,y Dataframes to join:
#'   * `x`: Reference table with columns giving information on the unique
#'     identifier of each stem and the quadrat it occurs.
#'   * `y`: Table with column giving information on the unique identifier of
#'     each stem.
#' @inheritParams dplyr::anti_join
#' @param ... Other parameters passed onto [dplyr::anti_join].
#'
#' @return Returns all rows from `x` where there are not matching values in
#'   `y`, keeping just columns from `x`.
#'   
#' @family general functions to pick or drop rows of a dataframe
#'
#' @export
#'
#' @examples
#' x <- tibble::tribble(
#'   ~unique_stem, ~quadrat,
#'         "01_1",    "001",
#'         "02_1",    "001",
#'         "02_2",    "001",
#'         "04_1",    "002",
#'         "04_2",    "002",
#'         "05_1",    "002"
#' )
#' y <- tibble::tribble(
#'   ~unique_stem,
#'         "01_1",
#'         "02_2",
#'         "04_2"
#' )
#' 
#' pick_recensus(x, y)
#' 
#' # Same
#' pick_recensus(x, y, by = "unique_stem")
#' 
#' y2 <- dplyr::tribble(
#'   ~unq_stem,
#'      "01_1",
#'      "02_2",
#'      "04_2"
#' )
#' pick_recensus(x, y2, by = c("unique_stem" = "unq_stem"))
#' 
#' # For this and more general problems you can use `dplyr::*_join()` functions
#' dplyr::anti_join(x, y2, by = c("unique_stem" = "unq_stem"))
pick_recensus <- function(x, y, by = NULL, ...) {
  dplyr::anti_join(x = x, y = y, by = by, ...)
}
