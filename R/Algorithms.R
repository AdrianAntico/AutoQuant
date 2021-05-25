#' @title Bisection
#'
#' @description Finds roots for a given interval of values for a given function using bisection method
#'
#' @author Adrian Antico
#' @description Algorithms
#'
#' @param f mathematical function
#' @param a lower bound numeric value
#' @param b upper bound numeric value
#'
#' @examples
#' \dontrun{
#' RemixAutoML::Bisection(f = function(x) x ^ 2 - 4 * x + 3, a = 0, b = 2)
#' # 1
#' }
#'
#' @export
Bisection <- function(f = function(x) x ^ 2 - 4 * x + 3, a = 0, b = 2) {
  start <- a
  end <- b
  if(f(a) == 0) {
    return(a)
  } else if(f(b) == 0) {
    return(b)
  } else if(f(a) * f(b) > 0) {
    stop("Could not find root in given interval")
  } else {
    mid <- 0.5 * (start + end)
    while(abs(start - end) > 10 ^ -7) {
      if(f(mid) == 0) {
        return(mid)
      } else if(f(mid) * f(start) < 0) {
        end <- mid
      } else {
        start <- mid
      }
    }
    return(mid)
  }
}

#' @noRd
BisectLeft <- function(sorted_collection = c(), item = 1L, lo = 1L, hi = 1L) {
  if(hi < 0) {
    hi <- length(sorted_collection)
    while(lo < hi) {
      mid <- floor((lo + hi) / 2)
      if(sorted_collection[mid] < item) {
        lo <- mid + 1
      } else {
        hi <- mid
      }
    }
    return(lo)
  }
}

#' @noRd
BisectLeft <- function(sorted_collection = c(), item = 1L, lo = 1L, hi = -1L) {
  if(hi < 0) hi <- length(sorted_collection)
  while(lo < hi) {
    mid <- floor((lo + hi) / 2)
    if(sorted_collection[mid] <= item) {
      lo <- mid + 1L
    } else {
      hi <- mid
    }
  }
  return(lo)
}

#' @title InsertSortedValue
#'
#' @description Update a sorted vector with a new value that preserves sort order
#'
#' @author Adrian Antico
#' @description Algorithms
#'
#' @param vec numeric vector
#' @param Val value to insert
#' @param order 'left' or 'right', insert value location
#'
#' @examples
#' \dontrun{
#' RemixAutoML::InsertSortedValue(vec = seq(2, 2000, 2), Val = 741, order = "left")
#' }
#'
#' @export
InsertSortedValue <- function(Vec, Val, order = "left") {
  if(order == "left") {
    Element <- BisectLeft(Vec, item = Val)
  } else {
    Element <- BisectRight(Vec, item = Val)
  }
  return(c(Vec[seq_len(Element-1L)], Val, Vec[Element:length(Vec)]))
}
