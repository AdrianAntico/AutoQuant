#' @title DummifyDT
#'
#' @description DummifyDT creates dummy variables for the selected columns. Either one-hot encoding, N+1 columns for N levels, or N columns for N levels.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data The data set to run the micro auc on
#' @param cols A vector with the names of the columns you wish to dichotomize
#' @param TopN Default is NULL. Scalar to apply to all categorical columns or a vector to apply to each categorical variable. Only create dummy variables for the TopN number of levels. Will be either TopN or max(levels)
#' @param OneHot Set to TRUE to run one hot encoding, FALSE to generate N columns for N levels
#' @param KeepFactorCols Set to TRUE to keep the original columns used in the dichotomization process
#' @param SaveFactorLevels Set to TRUE to save unique levels of each factor column to file as a csv
#' @param SavePath Provide a file path to save your factor levels. Use this for models that you have to create dummy variables for.
#' @param ImportFactorLevels Instead of using the data you provide, import the factor levels csv to ensure you build out all of the columns you trained with in modeling.
#' @param FactorLevelsList Supply a list of factor variable levels
#' @param ClustScore This is for scoring AutoKMeans. It converts the added dummy column names to conform with H2O dummy variable naming convention
#' @param ReturnFactorLevels If you want a named list of all the factor levels returned, set this to TRUE. Doing so will cause the function to return a list with the source data.table and the list of factor variables' levels
#' @param GroupVar Ignore this
#' @examples
#' \dontrun{
#  # Create fake data with 10 categorical columns
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 25000,
#'   ID = 2L,
#'   ZIP = 0,
#'   FactorCount = 10L,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Create dummy variables
#' data <- RemixAutoML::DummifyDT(
#'   data = data,
#'   cols = c("Factor_1",
#'            "Factor_2",
#'            "Factor_3",
#'            "Factor_4",
#'            "Factor_5",
#'            "Factor_6",
#'            "Factor_8",
#'            "Factor_9",
#'            "Factor_10"),
#'   TopN = c(rep(3,9)),
#'   KeepFactorCols = TRUE,
#'   OneHot = FALSE,
#'   SaveFactorLevels = TRUE,
#'   SavePath = getwd(),
#'   ImportFactorLevels = FALSE,
#'   FactorLevelsList = NULL,
#'   ClustScore = FALSE,
#'   ReturnFactorLevels = FALSE)
#'
#' # Create Fake Data for Scoring Replication
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 25000,
#'   ID = 2L,
#'   ZIP = 0,
#'   FactorCount = 10L,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Scoring Version
#' data <- RemixAutoML::DummifyDT(
#'   data = data,
#'   cols = c("Factor_1",
#'            "Factor_2",
#'            "Factor_3",
#'            "Factor_4",
#'            "Factor_5",
#'            "Factor_6",
#'            "Factor_8",
#'            "Factor_9",
#'            "Factor_10"),
#'   TopN = c(rep(3,9)),
#'   KeepFactorCols = TRUE,
#'   OneHot = FALSE,
#'   SaveFactorLevels = TRUE,
#'   SavePath = getwd(),
#'   ImportFactorLevels = TRUE,
#'   FactorLevelsList = NULL,
#'   ClustScore = FALSE,
#'   ReturnFactorLevels = FALSE)
#' }
#' @return Either a data table with new dummy variables columns and optionally removes base columns (if ReturnFactorLevels is FALSE), otherwise a list with the data.table and a list of the factor levels.
#' @export
DummifyDT <- function(data,
                      cols,
                      TopN               = NULL,
                      KeepFactorCols     = FALSE,
                      OneHot             = FALSE,
                      SaveFactorLevels   = FALSE,
                      SavePath           = NULL,
                      ImportFactorLevels = FALSE,
                      FactorLevelsList   = NULL,
                      ClustScore         = FALSE,
                      ReturnFactorLevels = FALSE,
                      GroupVar           = FALSE) {

  # Check data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Check arguments ----
  if(!is.null(TopN)) if(length(TopN) > 1L && length(TopN) != length(cols)) stop("TopN must match the length of cols")
  if(!is.null(TopN)) if(length(TopN) > 1L) TopN <- rev(TopN)
  if(!is.character(cols)) stop("cols needs to be a character vector of names")
  if(!is.logical(KeepFactorCols)) stop("KeepFactorCols needs to be either TRUE or FALSE")
  if(!is.logical(KeepFactorCols)) stop("KeepFactorCols needs to be either TRUE or FALSE")
  if(!is.logical(OneHot)) stop("OneHot needs to be either TRUE or FALSE")
  if(!is.logical(SaveFactorLevels)) stop("SaveFactorLevels needs to be either TRUE or FALSE")
  if(!is.logical(ImportFactorLevels)) stop("ImportFactorLevels needs to be either TRUE or FALSE")
  if(!is.logical(ClustScore)) stop("ClustScore needs to be either TRUE or FALSE")
  if(!is.null(SavePath)) if(!is.character(SavePath)) stop("SavePath needs to be a character value of a folder location")

  # Ensure correct argument settings ----
  if(OneHot && ClustScore) {
    OneHot <- FALSE
    KeepFactorCols <- FALSE
  }

  # Build dummies start ----
  FactorsLevelsList <- list()
  if(!GroupVar) if(length(cols) > 1L && "GroupVar" %chin% cols) cols <- cols[!cols %chin% "GroupVar"]
  if(length(TopN) > 1L) Counter <- 1L
  for(col in rev(cols)) {
    size <- ncol(data)
    Names <- setdiff(names(data), col)
    if(ImportFactorLevels) {
      temp <- data.table::fread(file.path(SavePath, paste0(col, ".csv")), sep = ",")
      inds <- sort(unique(temp[[eval(col)]]))
    } else if(!is.null(FactorLevelsList)) {
      temp <- FactorLevelsList[[eval(col)]]
      inds <- sort(unique(temp[[eval(col)]]))
    } else if(!is.null(TopN)) {
      if(length(TopN) > 1L) {
        indss <- data[, .N, by = eval(col)][order(-N)]
        inds <- sort(indss[seq_len(min(TopN[Counter], .N)), get(col)])
        if(length(TopN) > 1L) Counter <- Counter + 1L
      } else {
        indss <- data[, .N, by = eval(col)][order(-N)]
        inds <- sort(indss[seq_len(min(TopN, .N)), get(col)])
      }
    } else {
      indss <- data[, .N, by = eval(col)][order(-N)]
      inds <- sort(unique(data[[eval(col)]]))
    }

    # Allocate columns ----
    data.table::alloc.col(data, n = ncol(data) + length(inds))

    # Save factor levels for scoring later ----
    if(SaveFactorLevels) {
      if(!is.null(TopN)) {
        if(length(TopN) > 1L) {
          temp <- indss[seq_len(min(TopN[Counter-1L], .N))][, N := NULL]
          data.table::fwrite(x = temp, file = file.path(SavePath, paste0(col, ".csv")), sep = ",")
        } else {
          temp <- indss[seq_len(min(TopN, .N))][, N := NULL]
          data.table::fwrite(x = temp, file = file.path(SavePath, paste0(col, ".csv")), sep = ",")
        }
      } else {
        data.table::fwrite(x = data.table::set(indss, j = N, value = NULL), file = file.path(SavePath, paste0(col, ".csv")), sep = ",")
      }
    }

    # Collect Factor Levels ----
    if(ReturnFactorLevels && SaveFactorLevels) {
      FactorsLevelsList[[eval(col)]] <- temp
    } else if(ReturnFactorLevels) {
      data[, get(col), by = eval(col)][, V1 := NULL]
    }

    # Convert to character if col is factor ----
    if(is.factor(data[[eval(col)]])) data.table::set(data, j = eval(col), value = as.character(data[[eval(col)]]))

    # If for clustering set up old school way ----
    if(!ClustScore) {
      data.table::set(data, j = paste0(col, "_", inds), value = 0L)
    } else {
      data.table::set(data, j = paste0(col, inds), value = 0L)
    }

    # Build dummies ----
    for(ind in inds) {
      if(!ClustScore) {
        data.table::set(data, i = which(data[[col]] %in% ind), j = paste0(col, "_", ind), value = 1L)
      } else {
        data.table::set(data, i = which(data[[col]] %in% ind), j = paste0(col, ind),value = 1L)
      }
    }

    # Remove original factor columns ----
    if(!KeepFactorCols) data.table::set(data, j = eval(col), value = NULL)
    if(ClustScore) setcolorder(data, c(setdiff(names(data), Names), Names))
    if(OneHot) data.table::set(data, j = paste0(col, "_Base"), value = 0L)
  }

  # Clustering section ----
  if(ClustScore) data.table::setnames(data, names(data), tolower(gsub('[[:punct:] ]+', replacement = "", names(data))))

  # Return data ----
  if(ReturnFactorLevels) {
    return(list(data = data, FactorLevelsList = FactorsLevelsList))
  } else {
    return(data)
  }
}

#' @title ModelDataPrep
#'
#' @description This function replaces inf values with NA, converts characters to factors, and imputes with constants
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data This is your source data you'd like to modify
#' @param Impute Defaults to TRUE which tells the function to impute the data
#' @param CharToFactor Defaults to TRUE which tells the function to convert characters to factors
#' @param FactorToChar Converts to character
#' @param IntToNumeric Defaults to TRUE which tells the function to convert integers to numeric
#' @param DateToChar Converts date columns into character columns
#' @param IDateConversion Convert IDateTime to POSIXct and IDate to Date types
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
#'   IDateConversion = FALSE,
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
                          IDateConversion = FALSE,
                          RemoveDates     = FALSE,
                          MissFactor      = "0",
                          MissNum         = -1,
                          IgnoreCols      = NULL) {

  # Check data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Prepare columns for action ----
  x <- as.integer(seq_along(data))
  if(!is.null(IgnoreCols)) if(class(IgnoreCols)[1L] == "character") x <- setdiff(x, which(names(data) %chin% IgnoreCols)) else x <- setdiff(x, IgnoreCols)

  # Replace any inf values with NA ----
  for(col in x) data.table::set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]), NA))

  # Turn character columns into factors----
  if(CharToFactor) for(col in x) if(is.character(data[[col]])) data.table::set(data, j = col, value = as.factor(data[[col]]))

  # Convert IDate to Date and IDateTime to POSIXct ----
  if(IDateConversion) {
    for(col in x) if(any(class(data[[col]]) %chin% "IDateTime")) data.table::set(data, j = col, value = as.POSIXct(data[[col]]))
    for(col in x) if(any(class(data[[col]]) %chin% "IDate")) data.table::set(data, j = col, value = as.Date(data[[col]]))
  }

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
        data.table::set(data, which(is.na(data[[col]])), col, MissFactor)
      } else if(is.numeric(data[[col]]) | is.integer(data[[col]])) {
        data.table::set(data, which(is.na(data[[col]])), col, MissNum)
      }
    }
  }

  # Remove Dates----
  if(RemoveDates || DateToChar) {
    for(col in rev(x)) {
      if(!is.character(data[[col]]) && !is.factor(data[[col]]) && !is.numeric(data[[col]]) && !is.integer(data[[col]]) && !is.logical(data[[col]]) && !is.complex(data[[col]])) {
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

#' @title ColumnSubsetDataTable
#'
#' @description ColumnSubsetDataTable will subset data tables by column
#'
#' @family Data Wrangling
#'
#' @author Adrian Antico
#'
#' @param data data.table
#' @param TargetColumnName Target variable
#' @param DateColumnName Date variable
#' @param GroupVars Group variables
#'
#' @noRd
ColumnSubsetDataTable <- function(data,
                                  TargetColumnName = NULL,
                                  DateColumnName = NULL,
                                  GroupVars = NULL) {

  # Check to see if data is actual data----
  if(!(any(class(data) %in% c("data.table","data.frame","tibble")))) {
    return(NULL)
  }

  # Subset----
  data <- data[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName),eval(GroupVars))]

  # Ensure Date Column is Date----
  if(is.character(data[[eval(DateColumnName)]])) {
    x <- data[1,get(DateColumnName)]
    x1 <- lubridate::guess_formats(x, orders = c("mdY","BdY","Bdy","bdY","bdy","mdy","dby","Ymd","Ydm"))
    data[, eval(DateColumnName) := as.Date(get(DateColumnName), tryFormats = x1)]
  }

  # Return data----
  return(data)
}

#' @title DataDisplayMeta
#'
#' @description DataDisplayMeta
#'
#' @author Adrian Antico
#'
#' @family Data Wrangling
#'
#' @param data Source data
#'
#' @noRd
DataDisplayMeta <- function(data) {

  # Check to see if data is actual data ----
  if(!(any(class(data) %in% c("data.table","data.frame","tibble")))) stop("data is not a data.table")

  # Begin process----
  Counter <- 0L
  N <- data[, .N]
  x <- data.table::data.table(Variable = rep("donotuse", N), Type = rep("donotuse", N))
  for(name in names(data)) {
    Counter <- Counter + 1L
    data.table::set(x, i = Counter, j = "Variable", value = eval(name))
    data.table::set(x, i = Counter, j = "DataType", value = class(data[[eval(name)]]))
  }

  # Return table
  return(x[Variable != "donotuse"])
}

#' @title TimeSeriesMelt
#'
#' @description TimeSeriesMelt
#'
#' @family Data Wrangling
#'
#' @author Adrian Antico
#'
#' @param data source data
#' @param TargetVariable vector of target variable names
#' @param DateVariable Name of date variable
#' @param GroupVariables Vector of group variable names
#'
#' @noRd
TimeSeriesMelt <- function(data,
                           TargetVariable = NULL,
                           DateVariable = NULL,
                           GroupVariables = NULL) {

  # 2 Cases:
  #  Multiple Targets + Grouping Variables
  #  Multiple Targets + No Grouping Variables
  if(length(TargetVariable) > 1) {
    if(!is.null(GroupVariables)) {
      data <- data.table::melt(
        data = data,
        id.vars = c(eval(DateVariable),eval(GroupVariables)),
        measure.vars = eval(TargetVariable),
        variable.name = "GroupVar",
        value.name = "TargetSeries")
    } else {
      data <- data.table::melt(
        data = data,
        id.vars = eval(DateVariable),
        measure.vars = c(eval(TargetVariable)),
        variable.name = "GroupVar",
        value.name = "TargetSeries")
    }
  }

  # Return
  return(data)
}

#' @title Interact
#'
#' @family Feature Engineering
#'
#' @param x Names
#' @param i Iteration
#' @param NumVarOperations List of names
#' @param Standardize List of results
#'
#' @noRd
Interact <- function(x,
                     i,
                     NumVarOperations,
                     Standardize) {
  if(i > 2L) {
    temp <- Standardize[[NumVarOperations[[x]][[1L]]]]$Result * Standardize[[NumVarOperations[[x]][[2L]]]]$Result
    for(ggg in 3L:i) temp <- temp * Standardize[[NumVarOperations[[x]][[ggg]]]]$Result
  } else {
    temp <- Standardize[[NumVarOperations[[x]][[1L]]]]$Result * Standardize[[NumVarOperations[[x]][[2L]]]]$Result
  }
  temp
}

#' @title AutoInteraction
#'
#' @description AutoInteraction creates interaction variables from your numerical features in your data. Supply a set of column names to utilize and set the interaction level. Supply a character vector of columns to exclude and the function will ignore those features.
#'
#' @family Feature Engineering
#'
#' @author Adrian Antico
#'
#' @param data Source data.table
#' @param NumVars Names of numeric columns (if NULL, all numeric and integer columns will be used)
#' @param InteractionDepth The max K in N choose K. If NULL, K will loop through 1 to length(NumVars). Default is 2 for pairwise interactions
#' @param Center TRUE to center the data
#' @param Scale TRUE to scale the data
#' @param SkipCols Use this to exclude features from being created. An example could be, you build a model with all variables and then use the varaible importance list to determine which features aren't necessary and pass that set of features into this argument as a character vector.
#' @param Scoring Defaults to FALSE. Set to TRUE for generating these columns in a model scoring setting
#' @param File When Scoring is set to TRUE you have to supply either the .Rdata list with lookup values for recreating features or a pathfile to the .Rdata file with the lookup values. If you didn't center or scale the data then this argument can be ignored.
#'
#' @examples
#' \dontrun{
#'
#' #########################################
#' # Feature Engineering for Model Training
#' #########################################
#'
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 50000,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   ZIP = 0L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Print number of columns
#' print(ncol(data))
#'
#' # Store names of numeric and integer cols
#' Cols <-names(data)[c(which(unlist(lapply(data, is.numeric))),
#'                      which(unlist(lapply(data, is.integer))))]
#'
#' # Model Training Feature Engineering
#' system.time(data <- RemixAutoML::AutoInteraction(
#'   data = data,
#'   NumericVars = Cols,
#'   InteractionDepth = 4,
#'   Center = TRUE,
#'   Scale = TRUE,
#'   SkipCols = NULL,
#'   Scoring = FALSE,
#'   File = getwd()))
#'
#' # user  system elapsed
#' # 0.30    0.11    0.41
#'
#' # Print number of columns
#' print(ncol(data))
#'
#' ########################################
#' # Feature Engineering for Model Scoring
#' ########################################
#'
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   ZIP = 0L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Print number of columns
#' print(ncol(data))
#'
#' # Reduce to single row to mock a scoring scenario
#' data <- data[1L]
#'
#' # Model Scoring Feature Engineering
#' system.time(data <- RemixAutoML::AutoInteraction(
#'   data = data,
#'   NumericVars = names(data)[
#'     c(which(unlist(lapply(data, is.numeric))),
#'       which(unlist(lapply(data, is.integer))))],
#'   InteractionDepth = 4,
#'   Center = TRUE,
#'   Scale = TRUE,
#'   SkipCols = NULL,
#'   Scoring = TRUE,
#'   File = file.path(getwd(), "Standardize.Rdata")))
#'
#' # user  system elapsed
#' # 0.19    0.00    0.19
#'
#' # Print number of columns
#' print(ncol(data))
#' }
#' @export
AutoInteraction <- function(data = NULL,
                            NumericVars = NULL,
                            InteractionDepth = 2,
                            Center = TRUE,
                            Scale = TRUE,
                            SkipCols = NULL,
                            Scoring = FALSE,
                            File = NULL) {

  # Arg Check ----
  if(InteractionDepth > length(NumericVars)) stop("InteractionDepth cannot be greater than the length of NumericVars")

  # Check data ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Check args ----
  if(!is.logical(Center)) stop("Center must be either TRUE or FALSE")
  if(!is.logical(Scale)) stop("Scale must be either TRUE or FALSE")
  if(!is.logical(Scoring)) stop("Scoring must be either TRUE or FALSE")
  if(!is.null(NumericVars) && !is.character(NumericVars)) stop("NumericVars must be a character vector or NULL")
  if(!is.null(SkipCols) && !is.character(SkipCols)) stop("SkipCols must be a character vector")
  if(!is.null(InteractionDepth) && !(is.numeric(InteractionDepth) || is.integer(InteractionDepth))) stop("InteractionDepth must be numeric or NULL")

  # Check File ----
  if(Scoring && (Center || Scale) && is.null(File)) stop("You need to supply the path for the File argument")
  if(Scoring && !is.null(File)) if(!dir.exists(File) && !file.exists(File) && !is.list(File)) stop("File is not valid")
  if(!Scoring && !is.null(File)) if(!dir.exists(File)) stop("File is not valid path")

  # Columns Validity check ----
  if(is.null(NumericVars)) {
    NumericVars <- names(data)[c(which(unlist(lapply(data, is.numeric))), which(unlist(lapply(data, is.integer))))]
    if(identical(NumericVars, character(0))) stop("No numeric or integer valued columns in data")
    for(nam in NumericVars) {
      tmp <- class(data[1L, get(nam)])
      if(!any(tmp %chin% c("numeric","integer"))) NumericVars <- NumericVars[!NumericVars %chin% nam]
    }
    if(identical(NumericVars, character(0))) stop("No numeric or integer valued columns in data")
    if(length(NumericVars) == 1L) stop("Only one column exists so no interactions can be generated")
    if(length(NumericVars) < InteractionDepth) stop("You can't have an InteractionDepth that is greater than the number of NumericVars")
  } else {
    if(!all(NumericVars %chin% names(data))) stop("at least some NumericVars are not in data")
    for(nam in NumericVars) {
      tmp <- class(data[1L, get(nam)])
      if(!any(tmp %chin% c("numeric","integer"))) NumericVars <- NumericVars[!NumericVars %chin% nam]
    }
    if(identical(NumericVars, character(0))) stop("No numeric or integer valued columns in data")
    if(length(NumericVars) == 1L) stop("Only one column exists so no interactions can be generated")
    if(length(NumericVars) < InteractionDepth) stop("You can't have an InteractionDepth that is greater than the number of NumericVars")
  }

  # Define Number of computations and increase alloc if too many ----
  N1 <- length(NumericVars)
  N <- InteractionDepth
  Total <- c()
  for(com in seq_len(InteractionDepth)[-1L]) Total <- c(Total, ncol(combinat::combn(NumericVars, m = com)))
  if(sum(Total) + ncol(data) > 1028L) data.table::setalloccol(DT = data, n = sum(Total) + ncol(data), verbose = TRUE)

  # Standardize collection list ----
  if(Scoring && (Center || Scale) && !is.list(File)) load(file = file.path(File))
  if(!exists("Standardize")) Standardize <- list()

  # Get results and metadata ahead of time ----
  if(!Scoring) {
    for(nam in NumericVars) {
      if(Center && Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Mean <- Rfast::colmeans(a1)
        c1 <- t(a1) - Standardize[[nam]]$Mean
        Standardize[[nam]]$Denom <- sqrt(Rfast::rowsums(c1^2))
        Standardize[[nam]]$Factor <- sqrt((dim(a1)[1L] - 1))
        Standardize[[nam]]$Result <- t(c1/Standardize[[nam]]$Denom * Standardize[[nam]]$Factor)
      } else if(Center && !Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Mean <- Rfast::colmeans(a1)
        Standardize[[nam]]$Result <- t(a1) - Standardize[[nam]]$Mean
      } else if(!Center && Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Denom <- sqrt(Rfast::rowsums(a1^2))
        Standardize[[nam]]$Factor <- sqrt((dim(a1)[1L] - 1))
        Standardize[[nam]]$Result <- t(a1/Standardize[[nam]]$Denom * Standardize[[nam]]$Factor)
      } else {
        Standardize[[nam]]$Result <- data[[NumVarOperations[[x]][[2L]]]]
      }
    }
  } else {
    for(nam in NumericVars) {
      if(Center && Scale) {
        a1 <- as.matrix(data[[nam]])
        c1 <- t(a1) - Standardize[[nam]]$Mean
        Standardize[[nam]]$Result <- t(c1/Standardize[[nam]]$Denom * Standardize[[nam]]$Factor)
      } else if(Center && !Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Result <- t(a1) - Standardize[[nam]]$Mean
      } else if(!Center && Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Result <- t(a1/Standardize[[nam]]$Denom * Standardize[[nam]]$Factor)
      } else {
        Standardize[[nam]]$Result <- data[[NumVarOperations[[x]][[2L]]]]
      }
    }
  }

  # Define colnames and contributing features ----
  for(i in seq_len(N)[-1L]) {

    # Initialize lists and vector
    NumVarsNames <- c()
    NumVarOperations <- list()

    # Interaction Depth equals number of variables
    if(i == N1) {
      temp <- combinat::combn(NumericVars, m = i)
      for(m in N1) {
        temp2 <- paste(temp, collapse = "_")
        templist <- list()
        for(zz in seq_len(m)) templist[[zz]] <- temp[zz]
        NumVarOperations[[temp2]] <- templist
      }
      NumVarsNames <- c(NumVarsNames, temp2)
    } else if(i <= N1) {
      temp <- combinat::combn(NumericVars, m = i)
      temp2 <- c()
      for(k in seq_len(ncol(temp))) {
        for(l in seq_len(i)) {
          if(l == 1L) {
            temp2 <- temp[l,k]
          } else {
            temp2 <- paste(temp2,temp[l, k], sep = "_")
            templist <- list()
            for(zz in seq_len(l)) templist[[zz]] <- temp[zz, k]
            NumVarOperations[[temp2]] <- templist
          }
        }
        NumVarsNames <- c(NumVarsNames, temp2)
      }
    }

    # SkipCols ----
    if(!is.null(SkipCols)) NumVarsNames <- NumVarsNames[!NumVarsNames %chin% SkipCols]

    # Build features ----
    data[, (NumVarsNames) := lapply(NumVarsNames, Interact, i, NumVarOperations, Standardize)]
  }

  # Save Standardize if Center or Scale ----
  if(!Scoring && (Center || Scale)) {
    for(nam in names(Standardize)) Standardize[[nam]][["Result"]] <- NULL
    if(!is.null(File)) save(Standardize, file = file.path(File, "Standardize.Rdata"))
  }

  # Return data ----
  return(data)
}

#' @title DiffDT
#'
#' @description Difference a column in a data.table
#'
#' @author Adrian Antico
#' @family Misc
#'
#' @param data Source data
#' @param x Column name
#' @param NLag1 Numeric
#' @param NLag2 Numeric
#' @param Type Choose from 'numeric' or 'date'
#' @noRd
DiffDT <- function(data, x, NLag1, NLag2, Type = "numeric") {
  if(Type == "numeric") {
    if(NLag1 == 0) {
      temp <- data[[eval(x)]] - data[[paste0("Diff_", NLag2, "_", x)]]
    } else {
      temp <- data[[paste0("Diff_", NLag1, "_", x)]] - data[[paste0("Diff_", NLag1,"-", NLag2, "_", x)]]
    }
  } else if(Type == "date") {
    if(NLag1 == 0) {
      temp <- difftime(time1 = data[[eval(x)]], time2 = data[[paste0("Diff_", NLag2, "_", x, "_temp")]], units = "days")
    } else {
      temp <- difftime(time1 = data[[paste0("Diff_",NLag1, "_", x, "_temp")]], time2 = data[[paste0("Diff_", NLag1,"-", NLag2, "_", x, "_temp")]], units = "days")
    }
  } else if(Type == "categorical") {
    if(NLag1 == 0) {
      temp <- "No_Change"
      temp <- ifelse(data[[eval(x)]] != data[[paste0("Diff_", NLag2, "_", x)]], paste0("New=",data[[eval(x)]]," Old=",data[[paste0("Diff_", NLag2, "_", x)]]), "No_Change")
    } else {
      temp <- ifelse(data[[paste0("Diff_", NLag1, "_", x)]] != data[[paste0("Diff_", NLag2, "_", x)]], paste0("New=",data[[eval(x)]]," Old=",data[[paste0("Diff_", NLag2, "_", x)]]), "No_Change")
    }
  }
  if(Type == "categorical") {
    return(temp)
  } else {
    return(as.numeric(temp))
  }
}

#' @title AutoDiffLagN
#'
#' @description AutoDiffLagN create differences for selected numerical columns
#'
#' @family Feature Engineering
#' @author Adrian Antico
#'
#' @param data Source data
#' @param DateVariable Date column used for sorting
#' @param GroupVariables Difference data by group
#' @param DiffVariables Column names of numeric columns to difference
#' @param DiffDateVariables Columns names for date variables to difference. Output is a numeric value representing the difference in days.
#' @param DiffGroupVariables Column names for categorical variables to difference. If no change then the output is 'No_Change' else 'New=NEWVAL Old=OLDVAL' where NEWVAL and OLDVAL are placeholders for the actual values
#' @param NLag1 If the diff calc, we have column 1 - column 2. NLag1 is in reference to column 1. If you want to take the current value minus the previous weeks value, supply a zero. If you want to create a lag2 - lag4 NLag1 gets a 2.
#' @param NLag2 If the diff calc, we have column 1 - column 2. NLag2 is in reference to column 2. If you want to take the current value minus the previous weeks value, supply a 1. If you want to create a lag2 - lag4 NLag1 gets a 4.
#' @param Sort TRUE to sort your data inside the function
#' @param RemoveNA Set to TRUE to remove rows with NA generated by the lag operation
#' @examples
#' \dontrun{
#'
#' # Create fake data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 50000,
#'   ID = 2L,
#'   FactorCount = 3L,
#'   AddDate = TRUE,
#'   ZIP = 0L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Store Cols to diff
#' Cols <- names(data)[which(unlist(data[, lapply(.SD, is.numeric)]))]
#'
#' # Clean data before running AutoDiffLagN
#' data <- RemixAutoML::ModelDataPrep(data = data, Impute = FALSE, CharToFactor = FALSE, FactorToChar = TRUE)
#'
#' # Run function
#' data <- RemixAutoML::AutoDiffLagN(
#'   data,
#'   DateVariable = "DateTime",
#'   GroupVariables = c("Factor_1", "Factor_2"),
#'   DiffVariables = Cols,
#'   DiffDateVariables = NULL,
#'   DiffGroupVariables = NULL,
#'   NLag1 = 0L,
#'   NLag2 = 1L,
#'   Sort = TRUE,
#'   RemoveNA = TRUE)
#' }
#'
#' @export
AutoDiffLagN <- function(data,
                         DateVariable = NULL,
                         GroupVariables = NULL,
                         DiffVariables = NULL,
                         DiffDateVariables = NULL,
                         DiffGroupVariables = NULL,
                         NLag1 = 0L,
                         NLag2 = 1L,
                         Sort = FALSE,
                         RemoveNA = TRUE) {

  # Turn on full speed ahead ----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))

  # Ensure data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Check args ----
  if(!is.null(DateVariable) && !is.character(DateVariable)) stop("DateVariable needs to be a charcter valued vector or scalar")
  if(!is.null(GroupVariables) && !is.character(GroupVariables)) stop("GroupVariables needs to be a charcter valued vector or scalar")
  if(!is.character(DiffVariables)) stop("DiffVariables needs to be a charcter valued vector or scalar")
  if(!is.numeric(NLag1)) stop("NLag1 needs to be a numeric valued scalar")
  if(!is.numeric(NLag2)) stop("NLag2 needs to be a numeric valued scalar")
  if(NLag1 < 0) stop("NLag1 needs to be a positive numeric valued scalar")
  if(NLag2 < 0) stop("NLag2 needs to be a positive numeric valued scalar")
  if(!is.logical(Sort)) stop("Sort needs to be a logical valued scalar")

  # Sort if TRUE ----
  if(!is.null(GroupVariables)) {
    data.table::setorderv(x = data, cols = c(GroupVariables, DateVariable), order = c(rep(1, length(c(GroupVariables, DateVariable)))), na.last = FALSE)
  } else {
    data.table::setorderv(x = data, cols = c(DateVariable), order = 1L, na.last = FALSE)
  }

  # Diff numeric data ----
  if(!is.null(DiffVariables)) {
    if(NLag1 == 0L) {
      ModDiffVariables <- paste0("Diff_", NLag2, "_", DiffVariables)
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffVariables), by = eval(GroupVariables)]
      } else {
        data <- data[, (ModDiffVariables) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffVariables)]
      }
      data <- data[, (ModDiffVariables) := {g <- list(); for(x in DiffVariables) g[[x]] <- DiffDT(data, x, NLag1, NLag2, Type = "numeric"); g}]
    } else {
      ModDiffVariables1 <- paste0("Diff_", NLag1, "_", DiffVariables)
      ModDiffVariables2 <- paste0("Diff_", NLag1,"-", NLag2, "_", DiffVariables)
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = NA, type = "lag"), .SDcols = c(DiffVariables), by = eval(GroupVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffVariables), by = eval(GroupVariables)]
      } else {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = NA, type = "lag"), .SDcols = c(DiffVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffVariables)]
      }
      data <- data[, (ModDiffVariables2) := {g <- list(); for(x in DiffVariables) g[[x]] <- DiffDT(data, x, NLag1, NLag2, Type = "numeric"); g}]
      data.table::set(data, j = ModDiffVariables1, value = NULL)
    }
  }

  # Diff date data ----
  if(!is.null(DiffDateVariables)) {
    if(NLag1 == 0L) {
      ModDiffVariables1 <- paste0("Diff_", NLag2, "_", DiffDateVariables)
      ModDiffVariables2 <- paste0("Diff_", NLag2, "_", DiffDateVariables, "_temp")
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffDateVariables), by = eval(GroupVariables)]
      } else {
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffDateVariables)]
      }
      data <- data[, (ModDiffVariables1) := {g <- list(); for(x in DiffDateVariables) g[[x]] <- DiffDT(data, x, NLag1, NLag2, Type = "date"); g}]
      data.table::set(data, j = ModDiffVariables2, value = NULL)
    } else {
      ModDiffVariables1 <- paste0("Diff_", NLag1, "_", DiffDateVariables,"_temp")
      ModDiffVariables2 <- paste0("Diff_", NLag1,"-", NLag2, "_", DiffDateVariables,"_temp")
      ModDiffVariables22 <- paste0("Diff_", NLag1,"-", NLag2, "_", DiffDateVariables)
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = NA, type = "lag"), .SDcols = c(DiffDateVariables), by = eval(GroupVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffDateVariables), by = eval(GroupVariables)]
      } else {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = NA, type = "lag"), .SDcols = c(DiffDateVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = NA, type = "lag"), .SDcols = c(DiffDateVariables)]
      }
      data <- data[, (ModDiffVariables22) := {g <- list(); for(x in DiffDateVariables) g[[x]] <- DiffDT(data, x, NLag1, NLag2, Type = "date"); g}]
      data.table::set(data, j = c(ModDiffVariables1, ModDiffVariables2), value = NULL)
    }
  }

  # Diff categorical data ----
  if(!is.null(DiffGroupVariables)) {
    if(NLag1 == 0L) {
      ModDiffVariables <- paste0("Diff_", NLag2, "_", DiffGroupVariables)
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables) := data.table::shift(x = .SD, n = NLag2, fill = "missing", type = "lag"), .SDcols = c(DiffGroupVariables), by = eval(GroupVariables)]
      } else {
        data <- data[, (ModDiffVariables) := data.table::shift(x = .SD, n = NLag2, fill = "missing", type = "lag"), .SDcols = c(DiffGroupVariables)]
      }
      data <- data[, (ModDiffVariables) := {g <- list(); for(x in DiffGroupVariables) g[[x]] <- DiffDT(data, x, NLag1, NLag2, Type = "categorical"); g}]
    } else {
      ModDiffVariables1 <- paste0("Diff_", NLag1, "_", DiffGroupVariables)
      ModDiffVariables2 <- paste0("Diff_", NLag1,"-", NLag2, "_", DiffGroupVariables)
      if(!is.null(GroupVariables)) {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = "missing", type = "lag"), .SDcols = c(DiffGroupVariables), by = eval(GroupVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = "missing", type = "lag"), .SDcols = c(DiffGroupVariables), by = eval(GroupVariables)]
      } else {
        data <- data[, (ModDiffVariables1) := data.table::shift(x = .SD, n = NLag1, fill = "missing", type = "lag"), .SDcols = c(DiffGroupVariables)]
        data <- data[, (ModDiffVariables2) := data.table::shift(x = .SD, n = NLag2, fill = "missing", type = "lag"), .SDcols = c(DiffGroupVariables)]
      }
      data <- data[, (ModDiffVariables2) := {g <- list(); for(x in DiffGroupVariables) g[[x]] <- DiffDT(data, x, NLag1, NLag2, Type = "categorical"); g}]
      data.table::set(data, j = ModDiffVariables1, value = NULL)
    }
  }

  # Final prep ----
  if(RemoveNA) {
    if(NLag1 == 0L) {
      if(!is.null(DiffVariables)) {
        data <- data[!is.na(get(paste0("Diff_", NLag2, "_", DiffVariables[[1L]])))]
      } else if(!is.null(DiffDateVariables)) {
        data <- data[!is.na(get(paste0("Diff_", NLag2, "_", DiffDateVariables[[1L]])))]
      } else {
        data <- data[!is.na(get(paste0("Diff_", NLag2, "_", DiffGroupVariables[[1L]])))]
      }
    } else {
      if(!is.null(DiffVariables)) {
        data <- data[!is.na(get(paste0("Diff_", NLag1,"-", NLag2, "_", DiffVariables[[1L]])))]
      } else if(!is.null(DiffDateVariables)) {
        data <- data[!is.na(get(paste0("Diff_", NLag1,"-", NLag2, "_", DiffDateVariables[[1L]])))]
      } else {
        data <- data[!is.na(get(paste0("Diff_", NLag1,"-", NLag2, "_", DiffGroupVariables[[1L]])))]
      }
    }
  }

  # Return data ----
  return(data)
}
