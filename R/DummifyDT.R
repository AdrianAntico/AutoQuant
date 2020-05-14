#' DummifyDT creates dummy variables for the selected columns.
#'
#' DummifyDT creates dummy variables for the selected columns. Either one-hot encoding, N+1 columns for N levels, or N columns for N levels.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data The data set to run the micro auc on
#' @param cols A vector with the names of the columns you wish to dichotomize
#' @param OneHot Set to TRUE to run one hot encoding, FALSE to generate N columns for N levels
#' @param KeepFactorCols Set to TRUE to keep the original columns used in the dichotomization process
#' @param SaveFactorLevels Set to TRUE to save unique levels of each factor column to file as a csv
#' @param SavePath Provide a file path to save your factor levels. Use this for models that you have to create dummy variables for.
#' @param ImportFactorLevels Instead of using the data you provide, import the factor levels csv to ensure you build out all of the columns you trained with in modeling.
#' @param FactorLevelsList Supply a list of factor variable levels
#' @param ClustScore This is for scoring AutoKMeans. Set to FALSE for all other applications.
#' @param ReturnFactorLevels If you want a named list of all the factor levels returned, set this to TRUE. Doing so will cause the function to return a list with the source data.table and the list of factor variables' levels
#' @examples
#' test <- data.table::data.table(Value = runif(100000),
#'                    FactorCol = sample(x = c(letters,
#'                                             LETTERS,
#'                                             paste0(letters,letters),
#'                                             paste0(LETTERS,LETTERS),
#'                                             paste0(letters,LETTERS),
#'                                             paste0(LETTERS,letters)),
#'                                       size = 100000,
#'                                       replace = TRUE))
#' test <- DummifyDT(data = test,
#'                   cols = "FactorCol",
#'                   KeepFactorCols = FALSE,
#'                   OneHot = FALSE,
#'                   SaveFactorLevels = FALSE,
#'                   SavePath = normalizePath("./"),
#'                   ImportFactorLevels = FALSE,
#'                   FactorLevelsList = NULL,
#'                   ClustScore = FALSE,
#'                   ReturnFactorLevels = FALSE)
#' ncol(test)
#' test[, sum(FactorCol_gg)]
#' @return Either a data table with new dummy variables columns and optionally removes base columns (if ReturnFactorLevels is FALSE), otherwise a list with the data.table and a list of the factor levels.
#' @export
DummifyDT <- function(data,
                      cols,
                      KeepFactorCols     = FALSE,
                      OneHot             = FALSE,
                      SaveFactorLevels   = FALSE,
                      SavePath           = NULL,
                      ImportFactorLevels = FALSE,
                      FactorLevelsList   = NULL,
                      ClustScore         = FALSE,
                      ReturnFactorLevels = FALSE) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2L))
  
  # Check arguments----
  if(!is.character(cols)) return("cols needs to be a character vector of names")
  if(!is.logical(KeepFactorCols)) return("KeepFactorCols needs to be either TRUE or FALSE")
  if(!is.logical(KeepFactorCols)) return("KeepFactorCols needs to be either TRUE or FALSE")
  if(!is.logical(OneHot)) return("OneHot needs to be either TRUE or FALSE")
  if(!is.logical(SaveFactorLevels)) return("SaveFactorLevels needs to be either TRUE or FALSE")
  if(!is.logical(ImportFactorLevels)) return("ImportFactorLevels needs to be either TRUE or FALSE")
  if(!is.logical(ClustScore)) return("ClustScore needs to be either TRUE or FALSE")
  if(!is.null(SavePath)) if(!is.character(SavePath)) return("SavePath needs to be a character value of a folder location")
  
  # Check data.table----
  if(!data.table::is.data.table(data)) data <- data.table::as.data.table(data)
  
  # Ensure correct argument settings----
  if(OneHot & ClustScore) {
    OneHot <- FALSE
    KeepFactorCols <- FALSE
  }
  
  # Build dummies start----
  FactorsLevelsList <- list()
  if(length(cols) > 1 & "GroupVar" %chin% cols) cols <- cols[!cols %chin% "GroupVar"]
  for(col in rev(cols)) {
    size <- ncol(data)
    Names <- setdiff(names(data), col)
    if(ImportFactorLevels) {
      temp <- data.table::fread(file.path(normalizePath(SavePath), paste0(col, ".csv")))
      inds <- sort(unique(temp[[eval(col)]]))
    } else if(!is.null(FactorLevelsList)) {
      temp <- FactorLevelsList[[eval(col)]]
      inds <- sort(unique(temp[[eval(col)]])) 
    } else {
      inds <- sort(unique(data[[eval(col)]]))
    }
    
    # Allocate columns----
    data.table::alloc.col(data, n = ncol(data) + length(inds))
    
    # Save factor levels for scoring later----
    if(SaveFactorLevels) data.table::fwrite(x = data[, get(col), by = eval(col)][, V1 := NULL], file = file.path(normalizePath(SavePath), paste0(col, ".csv")))
    
    # Collect Factor Levels----
    if(ReturnFactorLevels) FactorsLevelsList[[eval(col)]] <- data[, get(col), by = eval(col)][, V1 := NULL]
    
    # Convert to character if col is factor----
    if(is.factor(data[[eval(col)]])) data.table::set(data, j = eval(col), value = as.character(data[[eval(col)]]))
    
    # If for clustering set up old school way----
    if(!ClustScore) {
      data.table::set(data, j = paste0(col, "_", inds), value = 0L)
    } else {
      data.table::set(data, j = paste0(col, inds), value = 0L)
    }
    
    # Build dummies----
    for(ind in inds) {
      if(!ClustScore) {
        data.table::set(data, i = which(data[[col]] %chin% ind), j = paste0(col, "_", ind), value = 1L)
      } else {
        data.table::set(data, i = which(data[[col]] %chin% ind), j = paste0(col, ind),value = 1L)
      }
    }
    
    # Remove original factor columns----
    if(!KeepFactorCols) data.table::set(data, j = eval(col), value = NULL)
    if(ClustScore) setcolorder(data, c(setdiff(names(data), Names), Names))
    if(OneHot) data.table::set(data, j = paste0(col, "_Base"), value = 0L)
  }
  
  # Clustering section----
  if(ClustScore) data.table::setnames(data, names(data), tolower(gsub('[[:punct:] ]+', replacement = "", names(data))))
  
  # Return data----
  if(ReturnFactorLevels) {
    return(list(data = data, FactorLevelsList = FactorsLevelsList))
  } else {
    return(data)    
  }
}
