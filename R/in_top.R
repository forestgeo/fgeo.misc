#' Pick the top `n` values from the head or tail of a vector.
#'
#' @param x An atomic vector.
#' @param n A single integer.
#'
#' @return
#'
#' @examples
#' 
#' # Pick from the head
#' top(1:3)
#' # Same
#' top(1:3, 1L)
#' 
#' # Pick from the tail
#' top(1:3, -2L)
#' 
#' # Useful for filtering
#' in_top(1:3)
#' in_top(1:3, -1L)
#' 
#' subset(mtcars, in_top(cyl))
#' 
#' subset(mtcars, in_top(cyl) & in_top(carb))
#' 
#' subset(mtcars, in_top(cyl, -2L))
#' @export
top <- function(x, n = 1L) {
  if (n < 0) {
    return(
      first_n(x, n, tail)
    )
  }
  
  first_n(x, n, head)
}

in_top <- function(x, n = 1L) {
  x %in% top(x, n)
}

first_n <- function(x, n, .f) {
  .f(sort(unique(x)), abs(as.integer(n)))
}
