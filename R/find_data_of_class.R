#' Find and hide dataset names in a package that match a class.
#' 
#' @param package String; Package name to search datasets in.
#' @param .class Class of data to find or hide.
#' 
#' @examples
#' show_data_of_class("fgeo.tool", "tbl")
#' hide_data_of_class("fgeo.tool", "data.frame")
#' 
#' @family functions for developers
#' @keywords internal
#' @name find_data_of_class
NULL

#' Factory to filter dataset names in a package matching some class.
#' @noRd
string_datasets_of_class <- function(.f = purrr::keep) {
  function(package, .class) {
    dts <- string_datasets(package)
    cls <- dts %>% 
      lapply(get) %>% 
      purrr::set_names(dts) %>% 
      purrr::map(class)
    out <- cls %>% .f(~any(grepl(.class, .x)))
    unlist(out)
  }
}

#' @export
#' @rdname find_data_of_class
show_data_of_class <- string_datasets_of_class(.f = purrr::keep)

#' @rdname find_data_of_class
#' @export
hide_data_of_class <- string_datasets_of_class(.f = purrr::discard)

#' String datasets in a package.
#' @noRd
string_datasets <- function(package) {
  dinfo <- utils::data(package = package)
  dinfo[["results"]][, "Item"]
}
