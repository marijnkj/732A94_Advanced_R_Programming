euclidian <- function(x, y) {
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
