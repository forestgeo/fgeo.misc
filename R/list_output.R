#' Export mapping each dataframe in a list to a corresponding .csv file.
#'
#' @source Adapted from an article by Jenny Bryan (https://goo.gl/ah8qkX).
#'
#' @param lst A list of dataframes.
#' @param dir Character; the directory where the files will be saved.
#' @param prefix Character; a prefix to add to the file names.
#' 
#' @examples
#' lst <- list(df1 = data.frame(x = 1), df2 = data.frame(x = 2))
#' 
#' # Saving the output to a temporary file
#' output <- tempdir()
#' list_csv(lst, output, prefix = "myfile-")
#'
#' # Look inside the output directory to confirm it worked
#' dir(output, pattern = "myfile")
#'
#' @family functions to handle multiple spreadsheets of an excel workbook
#' @family general functions to export data
#' @export
list_csv <- function(lst, dir, prefix = NULL) {
  stopifnot(is.list(lst), each_list_item_is_df(lst), is.character(dir))
  if (!is.null(prefix)) {
    stopifnot(is.character(prefix))
  }
  validate_dir(dir = dir, dir_name = "`dir`")

  purrr::walk2(lst, names(lst), list_csv_, prefix = prefix, dir = dir)
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

#' Do list_csv() for each df.
#' @noRd
list_csv_ <- function(df, df_name,  prefix = NULL, dir) {
  path <- file.path(paste0(dir, "/", prefix, df_name, ".csv"))
  readr::write_csv(df, path)
}



#' Help export a single file by reducing a list of dataframes to a single one.
#'
#' This function wraps [purrr::reduce()] and [dplyr::full_join()] to reduce 
#' all or some dataframes in a list into a single dataframe.
#'
#' @param lst A list of dataframes.
#' @param df_names Names of the list elements to join. `NULL` defaults to use
#'   all list elements.
#' @param by A character vector of variables to join by. If NULL, the default,
#'   *_join() will do a natural join, using all variables with common names
#'   across the two tables. To join by different variables on x and y use a
#'   named vector. For example, by = c("a" = "b") will match x.a to y.b. Passed
#'   to [dplyr::full_join()]
#'
#' @seealso [purrr::reduce()] and [dplyr::full_join()].
#' 
#' @return A dataframe.
#'
#' @examples
#' dfs <- list(
#'   a = data.frame(x = 1),
#'   b = data.frame(x = 2, y = 2),
#'   c = data.frame(x = 1, z = 3)
#' )
#'
#' list_df(dfs)
#' 
#' list_df(dfs, df_names = c("a", "c"))
#'
#' list_df(dfs, df_names = c("b", "c"))
#'
#' # Use argument `by` if dataframes have no matching variable,
#' list_df(
#'   list(data.frame(x = 1), data.frame(z = 2)),
#'   by = c("x" = "z")
#' )
#' @family general functions to export data
#' @export
list_df <- function(lst, df_names = NULL, by = NULL) {
  stopifnot(is.list(lst), each_list_item_is_df(lst))

  if (is.null(names(lst))) {
    names(lst) <- paste0("df", seq_along(lst))
  }

  if (is.null(df_names)) {
    df_names <- names(lst)
  } else {
    stopifnot(is.character(df_names))

    all_valid_nms <- all(purrr::map_lgl(df_names, ~rlang::has_name(lst, .)))
    if (!all_valid_nms) {
      msg <- paste0(
        "Each value of `which` must be a valid name of `.df`\n",
        "* Values of `which` :", commas(df_names), "\n",
        "* Valid names of `.df` :", commas(names(lst))
      )
      abort(msg)
    }
  }

  want <- purrr::keep(lst, names(lst) %in% df_names)
  suppressMessages(purrr::reduce(want, dplyr::full_join, by = by))
}
