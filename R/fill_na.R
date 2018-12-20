#' Modify vectors, dataframes, or matrices to fill missing values.
#'
#' This is a wrapper of [base::replace()] with a simplified interface and a name
#' that target the specific job of filling NA's. See also `tidyr::replace_na`
#' and `dplyr::coalesce`.
#'
#' @param x A dataframe.
#' @param filler A filler; whatever you want to replace NA's with.
#'
#' @family general functions to edit data in place
#'
#' @return A modified version of `x`.
#' 
#' @export
#' @examples
#' x <- data.frame(x = c(NA, 1), y = c("a", NA), stringsAsFactors = FALSE)
#'
#' a_dataframe <- x
#' fill_na(a_dataframe)
#' fill_na(a_dataframe, "")
#' fill_na(a_dataframe, "missing")
#'
#' a_matrix <- as.matrix(x)
#' fill_na(a_matrix)
#'
#' a_vector <- x$x
#' fill_na(a_vector)
#'
#' a_list <- list(x, x, x)
#' lapply(a_list, fill_na)
fill_na <- function(x, filler = 0) {
  x[is.na(x)] <- filler
  x
}
