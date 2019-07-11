#' Final Data Preparation Function
#'
#' This function replaces inf values with NA, converts characters to factors, and imputes with constants
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data This is your source data you'd like to modify
#' @param Impute Defaults to TRUE which tells the function to impute the data
#' @param CharToFactor Defaults to TRUE which tells the function to convert characters to factors
#' @param RemoveDates Defaults to FALSE. Set to TRUE to remove date columns from your data.table
#' @param MissFactor Supply the value to impute missing factor levels
#' @param MissNum Supply  the value to impute missing numeric values
#' @param IgnoreCols Supply column numbers for columns you want the function to ignore
#' @examples
#' data <- data.table::data.table(Value = runif(100000),
#'                                FactorCol = as.character(sample(x = c(letters,
#'                                                                      LETTERS,
#'                                                                      paste0(letters,letters),
#'                                                                      paste0(LETTERS,LETTERS),
#'                                                                      paste0(letters,LETTERS),
#'                                                                      paste0(LETTERS,letters)),
#'                                                                size = 100000,
#'                                                                replace = TRUE)))
#' data <- ModelDataPrep(data,
#'                       Impute = TRUE,
#'                       CharToFactor = TRUE,
#'                       MissFactor = "0",
#'                       MissNum    = -1)
#' @return Returns the original data table with corrected values
#' @export
ModelDataPrep <- function(data,
                          Impute       = TRUE,
                          CharToFactor = TRUE,
                          RemoveDates  = FALSE,
                          MissFactor   = "0",
                          MissNum      = -1,
                          IgnoreCols   = NULL) {
  # Check data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Prepare columns for action----
  x <- seq_along(data)
  if (!is.null(IgnoreCols)) {
    x <- setdiff(x, IgnoreCols)
  }
  
  # Replace any inf values with NA----
  for (col in x) {
    data.table::set(data,
                    j = col,
                    value = replace(data[[col]],
                                    is.infinite(data[[col]]), NA))
  }
  
  # Turn character columns into factors----
  if (CharToFactor) {
    for (col in x) {
      if (is.character(data[[col]])) {
        data.table::set(data,
                        j = col,
                        value = as.factor(data[[col]]))
      }
    }
  }
  
  # Impute missing values----
  if (Impute) {
    for (col in x) {
      if (is.factor(data[[col]]) | is.character(data[[col]]) {
        data.table::set(data,
                        which(!(data[[col]] %in% levels(data[[col]]))),
                        col,
                        MissFactor)
      } else {
        data.table::set(data,
                        which(base::is.na(data[[col]])),
                        col,
                        MissNum)
      }
    }
  }
  
  # Remove Dates----
  if (RemoveDates) {
    for (col in rev(x)) {
      if (!is.character(data[[col]]) &
          !is.factor(data[[col]]) &
          !is.numeric(data[[col]]) &
          !is.integer(data[[col]]) &
          !is.logical(data[[col]]) &
          !is.complex(data[[col]])) {
        data[, paste0(names(data)[col]) := NULL]
      }
    }
  }
  return(data)
}
