#' Remove trees found dead in both the last and previous last censuses.
#'
#' Removes trees that were found dead both in the last and previous last
#' censuses. Thus the resulting data set contains trees that in the last census
#' were found either alive or dead for the first time.
#'
#' @param vft A dataframe -- specifically a ForestGEO ViewFullTable with
#'   variables `status_tree` (see [fgeo.tool::add_status_tree()]).
#' 
#' @return A modified version of the input data set:
#'     * With the rows removed of all censuses except the last two.
#'     * With the rows removed of trees found dead on both the last and previous
#'       last censuses.
#'
#' @examples
#' vft <- tibble::tribble(
#'   ~CensusID, ~Treeid,   ~Status,
#'   1,    1,   "alive",  # Irrelevant: not one of the last two censuses
#'   1,    1,    "dead",  # 
#'   
#'   1,    2,   "alive",  # Irrelevant: not one of the last two censuses
#'   1,    2,   "alive",  # 
#'   
#'   2,    1,   "alive",  # Tree is alive: at lease one stem is alive
#'   2,    1,    "dead",  # 
#'   
#'   2,    2,    "dead",  # Tree is dead: all stems are dead. Notice that
#'   2,    2,    "dead",  # this tree is also dead in census 3.
#'   
#'   3,    1,   "alive",  # Tree is alive: at lease one stem is alive
#'   3,    1,    "dead",  # 
#'   
#'   3,    2,    "dead",  # Tree is dead: all stems are dead. Notice that
#'   3,    2,    "dead"   # this tree is also dead in census 2.
#' )
#' 
#' # `Status` refers to stems while `status_tree` refers to trees.
#' vft <- fgeo.tool::add_status_tree(vft, status_a = "alive", status_d = "dead")
#' 
#' # * Remove all censuses except the last two.
#' # * Remove trees found dead on both the last and previous last censuses.
#' drop_twice_dead(vft)
#' 
#' @family functions to pick or drop rows of a ForestGEO dataframe
#' @keywords internal
#' @export
drop_twice_dead <- function(vft) {
  old_nms <- names(vft)
  vft <- set_names(vft, tolower)
  
  check_drop_twice_dead(vft)

  last <- max(vft$censusid, na.rm = TRUE)
  last2 <- vft[vft$censusid %in% c(last, last - 1), ]
  by_treeid <- group_by(last2, .data$treeid)
  last2 <- ungroup(
    mutate(by_treeid, to_keep = !identical(unique(.data$status_tree), "dead"))
  )
  keep <- select(dplyr::filter(last2, .data$to_keep), -.data$to_keep)

  set_names(keep, old_nms)
}

check_drop_twice_dead <- function(vft) {
  stopifnot(is.data.frame(vft))
  check_crucial_names(
    vft, c("censusid", "treeid", "status", "status_tree")
  )
  if (!length(unique(vft$censusid)) >= 2) {
    warning(
      "The data set has less than two censuses; Keeping all trees",
      call. = FALSE
    )
  }
  invisible(vft)
}
