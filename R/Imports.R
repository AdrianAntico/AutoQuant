#' @import data.table
#' @import foreach
#' @importFrom data.table data.table %chin% .I .N .SD := as.data.table fwrite is.data.table rbindlist set setcolorder setnames setorderv as.IDate as.ITime
#' @importFrom lubridate %m+%
#' @importFrom foreach %dopar%
#' @importFrom stats optimize pchisq
#' @import doParallel
NULL

.datatable.aware = TRUE
