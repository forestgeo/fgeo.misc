#' Modify a vector or dataframe-columns by converting units.
#' 
#' `convert_unit()` of a vector or `convert_unit_at()` specific columns of a
#' dataframe.
#'
#' @param x A vector or dataframe.
#' @inheritParams measurements::conv_unit
#' @inheritParams purrr::map_at
#' 
#' @seealso [measurements::conv_unit()], [purrr::map_at()].
#' 
#' @family general functions to perform common transforms
#'
#' @return An object with the same structure as `x` (vector or dataframe).
#'
#' @examples
#' # For all units see ?measurements::conv_unit()
#' convert_unit(10, "mm2", "hectare")
#' convert_unit(1:3, from = "m", to = "mm")
#' convert_unit(1:3, "mph", "kph")
#' 
#' dfm <- data.frame(dbh = c(10, 100), name = c(10, 100))
#' convert_unit_at(dfm, .at = "dbh", from = "mm2", to = "hectare")
#' # All columns
#' convert_unit_at(dfm, .at = c("dbh", "name"), from = "mm2", to = "hectare")
#' # Same
#' convert_unit_at(dfm, .at = names(dfm), from = "mm2", to = "hectare")
#' @name convert_unit
NULL 

#' @rdname convert_unit
#' @export
convert_unit <- measurements::conv_unit

#' @rdname convert_unit
#' @export
convert_unit_at <- function(x, .at, from, to) {
  x[] <- purrr::map_at(
    .x = x, .at = .at, ~ measurements::conv_unit(.x, from, to)
  )
  x
}


#' Modify dataframe-columns to standardize them by a constant denominator.
#' 
#' @param x Dataframe.
#' @param denominator A numeric vector of length 1.
#' @inheritParams purrr::map_at
#' 
#' @family general functions to perform common transforms
#'
#' @return A data.frame.
#' @export
#' 
#' @examples
#' dfm <- data.frame(a = 1:3, b = 11:13)
#' standardize_at(dfm, "a", denominator = 100)
#' standardize_at(dfm, names(dfm), denominator = 100)
standardize_at <- function(x, .at, denominator) {
  x[] <- purrr::map_at(x, .at = .at, ~ .x / denominator)
  x
}
