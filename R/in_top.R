#' Pick the top `n` values from the head or tail of a vector.
#'
#' @param x An atomic vector.
#' @param n A single integer. Doubles are silently coerced to integer.
#'
#' @return 
#'   * `top()` returns a vector of the same type as the input.
#'   * `in_top()` returns a logical vector.
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
#' 
#' # Careful: Remember that `FALSE` evaluates to 0
#' lgl <- c(TRUE, FALSE, TRUE)
#' int <- as.integer(lgl)
#' int
#' 
#' top(lgl)
#' top(int)
#' 
#' top(lgl, -1)
#' top(int, -1)
#' 
#' @family general functions to pick or drop rows of a dataframe
#' @export
in_top <- function(x, n = 1L) {
  x %in% top(x, n)
}

#' @rdname in_top
#' @export
top <- function(x, n = 1L) {
  if (n < 0) {
    return(first_n(x, n, tail))
  }
  
  first_n(x, n, head)
}

first_n <- function(x, n, .f) {
  if (!is.numeric(n)) {
    abort("`n` must be numeric")
  }
  
  .f(sort(unique(x)), abs(as.integer(n)))
}
