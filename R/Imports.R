#' @import data.table
#' @import foreach
#' @importFrom data.table data.table %chin% .I .N .SD := as.data.table fwrite is.data.table rbindlist set setcolorder setnames setorderv as.IDate as.ITime %like%
#' @importFrom lubridate %m+%
#' @importFrom foreach %dopar%
#' @importFrom stats optimize pchisq
#' @import doParallel
#' @importFrom stats dbeta pbeta deltat dgeom pnorm qnorm reorder tsp
#' @importFrom utils installed.packages
NULL
.datatable.aware = TRUE
