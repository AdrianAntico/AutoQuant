#' tempDatesFun Convert Excel datetime char columns to Date columns
#'
#' tempDatesFun takes the Excel datetime column, which imports as character, and converts it into a date type
#'
#' @author Adrian Antico
#' @family Misc
#' @param x The column of interest
#' @examples
#' Cdata <- data.table::data.table(DAY_DATE = "2018-01-01 8:53")
#' Cdata[, DAY_DATE := tempDatesFun(DAY_DATE)]
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
tempDatesFun <- base::Vectorize(function(x) {
  data.table::tstrsplit(x, " ")[[1]][1]
})

#' SimpleCap function is for capitalizing the first letter of words
#'
#' SimpleCap function is for capitalizing the first letter of words (need I say more?)
#'
#' @author Adrian Antico
#' @family Misc
#' @param x Column of interest
#' @examples
#' x <- "adrian"
#' x <- SimpleCap(x)
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
SimpleCap <- function(x) {
  s <- data.table::tstrsplit(x, " ")[[1]]
  paste(
    base::toupper(base::substring(s, 1, 1)),
    base::substring(s, 2),
    sep = "",
    collapse = " "
  )
}

#' PrintObjectsSize prints out the top N objects and their associated sizes, sorted by size
#'
#' @author Adrian Antico
#' @family Misc
#' @param N The number of objects to display
#' @examples
#' \donttest{
#' PrintObjectsSize(N = 10)
#' }
#' @return A print to your console of the sizes of the objects in your environment
#' @export
PrintObjectsSize <- function(N = 10) {
  m <- length(ls())
  z <- min(m, N)
  print(sort(-vapply(ls(), FUN.VALUE = 1.1, function(x) {
    object.size(get(x))
  }))[1:z] / 1024 / 1024)
}

#' Percentile rank function
#'
#' This function computes percentile ranks for each row in your data like Excel's PERCENT_RANK
#'
#' @author Adrian Antico
#' @family Misc
#' @param x X is your variable of interest
#' @examples
#' data <- data.table::data.table(A = runif(100))
#' data[, Rank := percRank(A)]
#' @return vector of percentile ranks
#' @examples
#' data <- data.table::data.table(A = runif(100))
#' data[, Percentile := percRank(A)]
#' @export
percRank <- function(x) {
  trunc(data.table::frank(x)) / length(x)
}
