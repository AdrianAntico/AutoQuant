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

#' @title CreateInteractions
#'
#' @description Create interaction variables
#'
#' @author Adrian Antico
#' @family Feature Engineering - Numeric Types
#'
#' @param data Source data
#' @param RunMode 'train' or 'score'
#' @param ArgsList ArgsList_FFE
#' @param SkipCols Vector of column names to remove from data
#' @param Depth Interaction depth
#'
#' @examples
#' \dontrun{
#' Output <- RemixAutoML:::CreateInteractions(
#'   data = data,
#'   RunMode = "train",
#'   ArgsList = ArgsList_FE,
#'   SkipCols = NULL,
#'   Depth = 3)
#' data <- Output$data
#' ArgsList_FE <- Output$ArgsList
#' }
#'
#' @return A list containing the data and the ArgsList
#' @noRd
CreateInteractions <- function(data = NULL,
                               RunMode = "train",
                               ArgsList = ArgsList_FFE,
                               SkipCols = NULL,
                               Depth = 3) {

  # Metadata
  Start <- Sys.time()

  # Run function
  if(tolower(RunMode) == "train") {

    # Metadata
    tempnames <- names(data.table::copy(data))

    # Manage factor variables
    if(any(ArgsList$Data$InteractionVariables %chin% ArgsList$Data$GroupVariables)) {
      FactorNumericInteraction <- ArgsList$Data$InteractionVariables[which(ArgsList$Data$InteractionVariables %chin% ArgsList$Data$GroupVariables)]
      ArgsList$Data$InteractionVariables <- ArgsList$Data$InteractionVariables[!ArgsList$Data$InteractionVariables %chin% FactorNumericInteraction]
      ArgsList$Data$InteractionVariables <- c(ArgsList$Data$InteractionVariables, ArgsList$FE_Columns$FE_DummyVariables %like% FactorNumericInteraction)
    }

    # Run function
    data <- RemixAutoML::AutoInteraction(
      data = data,
      NumericVars = ArgsList$Data$InteractionVariables,
      InteractionDepth = Depth,
      Center = TRUE,
      Scale = TRUE,
      SkipCols = NULL,
      Scoring = FALSE,
      File = file.path(ArgsList$MetaData$MetaData_Path))

    # Args Tracking
    ArgsList$FE_Interaction$NumericVars <- ArgsList$Data$InteractionVariables
    ArgsList$FE_Interaction$InteractionDepth <- Depth
    ArgsList$FE_Interaction$Center <- TRUE
    ArgsList$FE_Interaction$Scale <- TRUE
    ArgsList$FE_Interaction$SkipCols <- NA
    ArgsList$FE_Interaction$Scoring <- FALSE
    ArgsList$FE_Interaction$File <- file.path(ArgsList$MetaData$MetaData_Path)

    # New columns tracking
    ArgsList$FE_Columns$FE_Interaction <- setdiff(names(data), tempnames)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$FE_Interaction_Training <- difftime(End, Start, units = "mins")

  } else {

    # Metadata
    Start <- Sys.time()

    # Run function
    data <- RemixAutoML::AutoInteraction(
      data = data,
      NumericVars = ArgsList$FE_Interaction$NumericVars,
      InteractionDepth = ArgsList$FE_Interaction$InteractionDepth,
      Center = ArgsList$FE_Interaction$Center,
      Scale = ArgsList$FE_Interaction$Scale,
      SkipCols <- ArgsList$FE_Interaction$SkipCols,
      Scoring = TRUE,
      File = ArgsList$FE_Interaction$File)

    # Skip cols
    if(!is.null(SkipCols)) {
      temp <- names(data)
      temp <- temp[!temp %chin% SkipCols]
      temp <- setdiff(names(data), temp)
      data.table::set(data, j = temp, value = NULL)
    }

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$FE_Interaction_Scoring <- difftime(End, Start, units = "mins")
  }

  # Return
  return(list(data = data, ArgsList = ArgsList))
}
