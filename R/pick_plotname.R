#' Pick the given or first (alpha sorted) plot from a ViewFullTable.
#'
#' @param vft Dataframe; particularly a ForestGEO ViewFullTable.
#' @param plot_nm Length-1 character vector of the value of `PlotName` (or
#'   `plotname`) to pick from `vft`.
#'
#' @return A dataframe.
#'
#' @examples
#' vft <- data.frame(PlotName = c("a", "b"), stringsAsFactors = FALSE)
#' pick_plotname(vft)
#' pick_plotname(vft, "b")
#'
#' @family functions for fgeo vft
#' @family functions to pick or drop rows of a ForestGEO dataframe
#' @export
pick_plotname <- function(vft, plot_nm = NULL) {
  stopifnot(is.data.frame(vft))

  old <- names(vft)
  names(vft) <- tolower(names(vft))

  check_crucial_names(vft, "plotname")

  plots <- sort(unique(vft$plotname))
  if (is.null(plot_nm)) plot_nm <- plots

  valid_plot <- sort(unique(vft$plotname))
  if (!all(plot_nm %in% valid_plot)) {
    stop("plotname = ", plot_nm, " wasn't detected.", call. = FALSE)
  }

  message("Using: ", plot_nm[[1]], ".")
  out <- vft[vft$plotname == plot_nm[[1]], , drop = FALSE]
  stats::setNames(out, old)
}

