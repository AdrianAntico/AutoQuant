#' Convert transactional data.table to a binary ratings matrix
#'
#' @author Adrian Antico and Douglas Pestana
#' @family Recommenders
#' @param data This is your transactional data.table. Must include an Entity (typically customer), ProductCode (such as SKU), and a sales metric (such as total sales).
#' @param EntityColName This is the column name in quotes that represents the column name for the Entity, such as customer
#' @param ProductColName This is the column name in quotes that represents the column name for the product, such as SKU
#' @param MetricColName This is the column name in quotes that represents the column name for the metric, such as total sales
#' @param ReturnMatrix Set to FALSE to coerce the object (desired route) or TRUE to return a matrix
#' @return A BinaryRatingsMatrix
#' @examples
#' \donttest{
#' RatingsMatrix <- AutoRecomDataCreate(data,
#'                                      EntityColName = "CustomerID",
#'                                      ProductColName = "StockCode",
#'                                      MetricColName = "TotalSales",
#'                                      ReturnMatrix = TRUE)
#' }
#' @export
AutoRecomDataCreate <- function(data,
                                EntityColName  = "CustomerID",
                                ProductColName = "StockCode",
                                MetricColName  = "TotalSales",
                                ReturnMatrix   = FALSE) {
  
  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))
  
  # Require RecommenderLab
  requireNamespace("recommenderlab", quietly = TRUE)
  
  # Ensure data is data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Ensure EntityColName is character type----
  if (!is.character(data[1, get(EntityColName)])) {
    data[, eval(EntityColName) := as.character(get(EntityColName))]
  }
  
  # Ensure ProductColName is character type----
  if (!is.character(data[1, get(ProductColName)])) {
    data[, eval(ProductColName) := as.character(get(ProductColName))]
  }
  
  # Ensure MetricColName is numeric----
  if (!is.numeric(data[1, get(MetricColName)])) {
    data[, eval(MetricColName) := as.numeric(get(MetricColName))]
  }
  
  # Only keep the necessary columns----
  keep <- c(EntityColName, ProductColName, MetricColName)
  data <- data[, ..keep]
  
  # CREATE BINARY RATING MATRIX-----
  train_data <- data.table::dcast(
    data,
    get(EntityColName) ~ get(ProductColName),
    value.var = eval(MetricColName),
    fun.aggregate = function(x)
      sum(!is.na(x)),
    fill = 0
  )
  
  # Change name back to original----
  data.table::setnames(train_data,
                       "EntityColName",
                       eval(EntityColName))
  
  # Convert Sales data to Binary----
  for (j in 2:ncol(train_data)) {
    data.table::set(x, j = j, value = data.table::fifelse(x[[j]] > 0, 1, 0))
  }
  
  # Store customerID for rownames----
  train_data_rownames <- train_data[[eval(EntityColName)]]
  
  # Remove CustomerID column----
  train_data[, eval(EntityColName) := NULL]
  
  # Convert train to matrix----
  train_data_matrix <- as.matrix(train_data)
  
  # Set rownames
  row.names(train_data_matrix) <- train_data_rownames
  
  # Return binary rating matrix----
  methods::as(object = train_data_matrix, Class = "binaryRatingMatrix")
}
