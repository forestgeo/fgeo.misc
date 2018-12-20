#' Identify each dataframe in a list with a name.
#'
#' Identify each dataframe in a list with the name of the corresponding list
#' item.
#'
#' @param dfs A list of dataframes.
#' @param name Names of the columns that store the names and values.
#'
#' @return A list of dataframes.
#'
#' @examples
#' dfs <- list(a = data.frame(x = 1), b = data.frame(x = 1))
#' name_dfs(dfs)
#'
#' name_dfs(dfs, "custom_name")
#'
#' dfs2 <- list(data.frame(x = 1), data.frame(x = 1))
#' name_dfs(dfs2)
#' 
#' @family general functions to deal with names
#' @family functions for internal use in other fgeo packages
#' @keywords internal
#' @export
name_dfs <- function(dfs, name = "name") {
  check_name_dfs(dfs, name)

  dfs <- fill_names(dfs, "df")
  lst_nms <- names(dfs)
  for (i in seq_along(lst_nms)) {
    df_nms <- c(names(dfs[[i]]), name)
    dfs[[i]] <- stats::setNames(cbind(dfs[[i]], lst_nms[[i]]), df_nms)
  }
  dfs
}

check_name_dfs <- function(dfs, name) {
  stopifnot(is.list(dfs), is.data.frame(dfs[[1]]), is.character(name))

  any_df_has_cero_row <- any(
    unlist(lapply(dfs, function(x) nrow(x) == 0 || ncol(x) == 0))
  )
  if (any_df_has_cero_row) {
    stop("All dataframes must have at least one row/column.", call. = TRUE)
  }
}

#' Fill names of an unnamed list
#'
#' @param x A list.
#' @param prefix A prefix for the added names.
#'
#' @examples
#' \dontrun{
#' fill_names(list(1))
#' fill_names(list(1, named_df = 1), "df")
#' }
#' @noRd
fill_names <- function(x, prefix = NULL) {
  stopifnot(is.list(x))
  if (!is.null(prefix)) stopifnot(is.character(prefix))

  filler_names <- paste0(prefix, seq_along(x))
  if (is.null(names(x))) {
    names(x) <- filler_names
  }
  missing_names <- names(x) %in% ""
  if (any(missing_names)) {
    names(x)[missing_names] <- filler_names[missing_names]
  }
  x
}
