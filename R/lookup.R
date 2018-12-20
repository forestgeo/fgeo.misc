#' Modify a vector to replace old with new values from a lookup table.
#'
#' Use this function inside `dplyr::recode()` to recode a vector based on values
#' from two other vectors, where `old` and `new` codes are looked up. These
#' lookup vectors are commonly stored in a dataframe and come from a .csv or
#' spreadsheet file.
#'
#' @param old,new Vectors of equal length giving old and new codes.
#'
#' @seealso `dplyr::recode()`
#' 
#' @section Acknowledgment:
#' Thanks to David Kenfack for inspiring this function.
#'
#' @family general functions to edit data in place
#'
#' @return A "spliced" list with names from `old` and values from `new`. The
#' kind of data structure that you can feed to `...` in dplyr::recode()`.
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' look <- tibble::tibble(
#'   old = c("spp1", "unknown"),
#'   new = c("spp3", "spp4")
#' )
#'
#' sp <- c("spp1", "spp2", "spp3", "unknown", "spp3", "unknown", "spp1", "spp1")
#' recode(sp, lookup(look$old, look$new))
#'
#' census <- tibble::tibble(sp = sp)
#' mutate(census, new_sp = recode(sp, lookup(look$old, look$new)))
#' 
#' # Overwrite
#' mutate(census, sp = recode(sp, lookup(look$old, look$new)))
lookup <- function(old, new) {
  if (length(old) != length(new)) {
    stop("`old` and `new` must be of equal length.")
  }
  lookup <- set_names(new, old)
  rlang::splice(as.list(lookup))
}
