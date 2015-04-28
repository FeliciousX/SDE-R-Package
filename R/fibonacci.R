#' Returns the n-th value of the fibonacci sequence.
#' @param length of fibonacci sequence \code{index}.
#' @return null
#' @details http://dirk.eddelbuettel.com/blog/2011/09/08/
#' @export
fib <- local({
  memo <- c(1, 1, rep(NA, 100))
  f <- function(x) {
    if(x == 0) return(0)
    if(x < 0) return(NA)
    if(x > length(memo))
      stop("'x' too big for implementation")
    if(!is.na(memo[x])) return(memo[x])
    ans <- f(x-2) + f(x-1)
    memo[x] <<- ans
    ans
  }
})

letters <- function(str) {
  x <- stringr::str_length(str)
  tail <- "number of characters."
  paste(x, tail)
}