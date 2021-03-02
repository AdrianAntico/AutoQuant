#' Final Data Preparation Function
#'
#' This function replaces inf values with NA, converts characters to factors, and imputes with constants
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data This is your source data you'd like to modify
#' @param Impute Defaults to TRUE which tells the function to impute the data
#' @param CharToFactor Defaults to TRUE which tells the function to convert characters to factors
#' @param FactorToChar Converts to character
#' @param IntToNumeric Defaults to TRUE which tells the function to convert integers to numeric
#' @param DateToChar Converts date columns into character columns
#' @param LogicalToBinary Converts logical values to binary numeric values
#' @param RemoveDates Defaults to FALSE. Set to TRUE to remove date columns from your data.table
#' @param MissFactor Supply the value to impute missing factor levels
#' @param MissNum Supply  the value to impute missing numeric values
#' @param IgnoreCols Supply column numbers for columns you want the function to ignore
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.75,
#'   N = 250000L,
#'   ID = 2L,
#'   ZIP = 0L,
#'   FactorCount = 6L,
#'   AddDate = TRUE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Check column types
#' str(data)
#'
#' # Convert some factors to character
#' data <- RemixAutoML::ModelDataPrep(
#'   data,
#'   Impute       = TRUE,
#'   CharToFactor = FALSE,
#'   FactorToChar = TRUE,
#'   IntToNumeric = TRUE,
#'   LogicalToBinary = FALSE,
#'   DateToChar   = FALSE,
#'   RemoveDates  = TRUE,
#'   MissFactor   = "0",
#'   MissNum      = -1,
#'   IgnoreCols   = c("Factor_1"))
#'
#' # Check column types
#' str(data)
#' }
#' @return Returns the original data table with corrected values
#' @export
ModelDataPrep <- function(data,
                          Impute          = TRUE,
                          CharToFactor    = TRUE,
                          FactorToChar    = FALSE,
                          IntToNumeric    = TRUE,
                          LogicalToBinary = FALSE,
                          DateToChar      = FALSE,
                          RemoveDates     = FALSE,
                          MissFactor      = "0",
                          MissNum         = -1,
                          IgnoreCols      = NULL) {

  # Full speed ahead ----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))

  # Check data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Prepare columns for action----
  x <- as.integer(seq_along(data))
  if(!is.null(IgnoreCols)) if(class(IgnoreCols) == "character") x <- setdiff(x, which(names(data) %chin% IgnoreCols)) else x <- setdiff(x, IgnoreCols)

  # Replace any inf values with NA----
  for(col in x) data.table::set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]), NA))

  # Turn character columns into factors----
  if(CharToFactor) for(col in x) if(is.character(data[[col]])) data.table::set(data, j = col, value = as.factor(data[[col]]))

  # Turn factor columns into character----
  if(FactorToChar) for(col in x) if(is.factor(data[[col]])) data.table::set(data, j = col, value = as.character(data[[col]]))

  # Turn integers columns into numeric----
  if(IntToNumeric) for(col in x) if(is.integer(data[[col]])) data.table::set(data, j = col, value = as.numeric(data[[col]]))

  # Turn logical columns into numeric----
  if(IntToNumeric & !LogicalToBinary) LogicalToBinary <- TRUE # backwards compatability
  if(LogicalToBinary) for(col in x) if(is.logical(data[[col]])) data.table::set(data, j = col, value = as.numeric(data[[col]]))

  # Impute missing values----
  if(Impute) {
    for(col in x) {
      if(is.factor(data[[col]])) {
        data.table::set(data, which(!(data[[col]] %in% levels(data[[col]]))), col, MissFactor)
      } else if(is.character(data[[col]])) {
        data.table::set(data, which(base::is.na(data[[col]])), col, MissFactor)
      } else if(is.numeric(data[[col]]) | is.integer(data[[col]])) {
        data.table::set(data, which(base::is.na(data[[col]])), col, MissNum)
      }
    }
  }

  # Remove Dates----
  if(RemoveDates || DateToChar) {
    for(col in rev(x)) {
      if(!is.character(data[[col]]) & !is.factor(data[[col]]) & !is.numeric(data[[col]]) & !is.integer(data[[col]]) & !is.logical(data[[col]]) & !is.complex(data[[col]])) {
        if(DateToChar) {
          data.table::set(data, j = paste0(names(data)[col]), value = as.character(data[[eval(col)]]))
        } else {
          data.table::set(data, j = paste0(names(data)[col]), value = NULL)
        }
      }
    }
  }

  # Return data----
  return(data)
}
