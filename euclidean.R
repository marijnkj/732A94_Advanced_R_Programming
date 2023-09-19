#' Compute GCD using the Euclidean algorithm
#' 
#' ´euclidean()´ calculates the greatest common devisor (GCD),
#' of two scalar values.
#' 
#' @param x First input scalar numeric value.
#' @param y Second input scalar numeric value.
#' @returns The greatest common divisor between x and y, a numeric value.
#' @examples
#' euclidean(252,105)
#' @seeal
#' https://en.wikipedia.org/wiki/Euclidean_algorithm
#' 
#' @export


euclidean <-
function(x, y) {
  # https://en.wikipedia.org/wiki/Euclidean_algorithm
  if (!is.numeric(x) | !is.numeric(y) | length(x) != 1 | length(y) != 1) {
    stop("Check your variables! x and y must be scalar values.")
  }
  else {
    a = max(x, y)
    b = min(x, y)
    while (a != b) {
      # Will only run if a is greater than b
      while (a > b) {
        rem <- a %% b
        a <- a - b
      }
      # Will only run if b is greater than a
      while (b > a) {
        rem <- b %% a
        b <- b - a
      }
    }
    return(a)
  }
}
