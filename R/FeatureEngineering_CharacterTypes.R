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
  for(col in cols) {
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
        temp <- indss[, N := NULL]
        data.table::fwrite(x = temp, file = file.path(SavePath, paste0(col, ".csv")), sep = ",")
      }
    }

    # Collect Factor Levels ----
    if(ReturnFactorLevels && SaveFactorLevels) {
      FactorsLevelsList[[eval(col)]] <- temp
    } else if(ReturnFactorLevels) {
      FactorsLevelsList[[eval(col)]] <- data[, get(col), by = eval(col)][, V1 := NULL]
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

#' @title CategoricalEncoding
#'
#' @description Categorical encoding for factor and character columns
#'
#' @author Adrian Antico
#'
#' @family Feature Engineering
#'
#' @param data Source data
#' @param ML_Type Only use with Method "credibility'. Select from 'classification' or 'regression'.
#' @param GroupVariables Columns to encode
#' @param TargetVariabl Target column name
#' @param Method Method to utilize. Choose from 'm_estimator', 'credibility', 'woe', 'target_encoding', 'poly_encode', 'backward_difference', 'helmert'
#' @param SavePath Path to save artifacts for recreating in scoring environments
#' @param Scoring Set to TRUE for scoring mode.
#' @param ImputeValueScoring If levels cannot be matched on scoring data you can supply a value to impute the NA's. Otherwise, leave NULL and manage them outside the function
#' @param ReturnFactorLevelList TRUE by default. Returns a list of the factor variable and transformations needed for regenerating them in a scoring environment. Alternatively, if you save them to file, they can be called for use in a scoring environment.
#' @param SupplyFactorLevelList The FactorCompenents list that gets returned. Supply this to recreate features in scoring environment
#' @param KeepOriginalFactors Defaults to TRUE. Set to FALSE to remove the original factor columns
#'
#' @examples
#' \dontrun{
#' # Create fake data with 10 categorical
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000000,
#'   ID = 2L,
#'   ZIP = 0,
#'   FactorCount = 10L,
#'   AddDate = FALSE,
#'   Classification = TRUE,
#'   MultiClass = FALSE)
#'
#' # Take your pick
#' Meth <- c('m_estimator',
#'           'credibility',
#'           'woe',
#'           'target_encoding',
#'           'poly_encode',
#'           'backward_difference',
#'           'helmert')
#'
#' # Pass to function
#' MethNum <- 1
#'
#' # Mock test data with same factor levels
#' test <- data.table::copy(data)
#'
#' # Run in Train Mode
#' data <- RemixAutoML::CategoricalEncoding(
#'   data = data,
#'   ML_Type = "classification",
#'   GroupVariables = paste0("Factor_", 1:10),
#'   TargetVariable = "Adrian",
#'   Method = Meth[MethNum],
#'   SavePath = getwd(),
#'   Scoring = FALSE,
#'   ReturnFactorLevelList = FALSE,
#'   SupplyFactorLevelList = NULL,
#'   KeepOriginalFactors = FALSE)
#'
#' # View results
#' print(data)
#'
#' # Run in Score Mode by pulling in the csv's
#' test <- RemixAutoML::CategoricalEncoding(
#'   data = data,
#'   ML_Type = "classification",
#'   GroupVariables = paste0("Factor_", 1:10),
#'   TargetVariable = "Adrian",
#'   Method = Meth[MethNum],
#'   SavePath = getwd(),
#'   Scoring = TRUE,
#'   ImputeValueScoring = 222,
#'   ReturnFactorLevelList = FALSE,
#'   SupplyFactorLevelList = NULL,
#'   KeepOriginalFactors = FALSE)
#' }
#'
#' @export
CategoricalEncoding <- function(data = NULL,
                                ML_Type = "classification",
                                GroupVariables = NULL,
                                TargetVariable = NULL,
                                Method = NULL,
                                SavePath = NULL,
                                Scoring = FALSE,
                                ImputeValueScoring = NULL,
                                ReturnFactorLevelList = TRUE,
                                SupplyFactorLevelList = NULL,
                                KeepOriginalFactors = TRUE) {

  # Args Check
  if(length(Method) > 1L) stop("You can only run one Method per function call.")
  if(!any(ML_Type %chin% c("classification", "regression")) && Method == "credibility") stop("Must designate ML_Type to either 'classification' or 'regression' for Method 'credibility'")

  # Convert to data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Helmert Encoding ----
  if(tolower(Method) == "helmert") {
    if(!Scoring) ComponentList <- list()
    for(GroupVar in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = GroupVar)

      # Encode
      if(!Scoring) {
        Levels <- sort(data[, unique(get(GroupVar))])
        Levels_Count <- length(Levels)
        GroupMean <- data.table::as.data.table({
          m <- t((diag(seq(Levels_Count-1, 0)) - upper.tri(matrix(1, Levels_Count, Levels_Count)))[-Levels_Count,])
          t(apply(m, 1, rev))
        })
        GroupMean[, eval(GroupVar) := Levels]
        data.table::setnames(GroupMean, names(GroupMean)[-Levels_Count], paste0(GroupVar, Levels[-Levels_Count]))
        data.table::setkeyv(GroupMean, cols = GroupVar)
        if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupVar, "_Helmert.csv")))
      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupVar, "_Helmert.csv")))
        data.table::setkeyv(GroupMean, cols = GroupVar)
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[paste0(GroupVar, "_Helmert")]]
        data.table::setkeyv(GroupMean, cols = GroupVar)
      }

      # Merge back to data
      N <- GroupMean[,.N-1L]
      data[GroupMean, paste0(GroupVar, GroupMean[seq_len(N), get(GroupVar)]) := mget(paste0("i.", GroupVar, GroupMean[seq_len(N), get(GroupVar)]))]
      if(!KeepOriginalFactors) data.table::set(data, j = GroupVar, value = NULL)
      if(!Scoring) ComponentList[[paste0(GroupVar, "_Helmert")]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        Vars <- paste0(GroupVar, GroupMean[seq_len(N), get(GroupVar)])
        for(z in Vars) {
          data.table::set(data, i = which(is.na(data[[z]])), j = z, value = ImputeValueScoring)
        }
      }
    }

    # Return
    if(!Scoring && ReturnFactorLevelList) {
      return(list(data = data, FactorCompenents = ComponentList))
    } else {
      return(data)
    }
  }

  # Backward Difference Encoding ----
  if(tolower(Method) == "backward_difference") {
    if(!Scoring) ComponentList <- list()
    for(GroupVar in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = GroupVar)

      # Encode
      if(!Scoring) {
        Levels <- sort(data[, unique(get(GroupVar))])
        Levels_Count <- length(Levels)
        GroupMean <- data.table::as.data.table({
          Mat <- matrix(0:(Levels_Count-1), nrow = Levels_Count, ncol = Levels_Count)
          Mat <- Mat + upper.tri(matrix(1, Levels_Count, Levels_Count))
          m2 <- matrix(-(Levels_Count-1), Levels_Count, Levels_Count)
          m2[upper.tri(m2)] <- 0
          Mat <- (Mat + m2) / Levels_Count
          Mat <- (t(Mat))[, -Levels_Count]
          Mat
        })
        GroupMean[, eval(GroupVar) := Levels]
        data.table::setnames(GroupMean, names(GroupMean)[-Levels_Count], paste0(GroupVar, Levels[-Levels_Count]))
        data.table::setkeyv(GroupMean, cols = GroupVar)
        if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupVar, "_backward_difference.csv")))
      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupVar, "_backward_difference.csv")))
        data.table::setkeyv(GroupMean, cols = GroupVar)
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[paste0(GroupVar, "_backward_difference")]]
        data.table::setkeyv(GroupMean, cols = GroupVar)
      }

      # Merge back to data
      N <- GroupMean[,.N-1L]
      data[GroupMean, paste0(GroupVar, GroupMean[seq_len(N), get(GroupVar)]) := mget(paste0("i.", GroupVar, GroupMean[seq_len(N), get(GroupVar)]))]
      if(!KeepOriginalFactors) data.table::set(data, j = GroupVar, value = NULL)
      if(!Scoring) ComponentList[[paste0(GroupVar, "_backward_difference")]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        Vars <- paste0(GroupVar, GroupMean[seq_len(N), get(GroupVar)])
        for(z in Vars) {
          data.table::set(data, i = which(is.na(data[[z]])), j = z, value = ImputeValueScoring)
        }
      }
    }

    # Return
    if(!Scoring && ReturnFactorLevelList) {
      return(list(data = data, FactorCompenents = ComponentList))
    } else {
      return(data)
    }
  }

  # Polynomial Encoding ----
  if(tolower(Method) == "poly_encode") {
    if(!Scoring) ComponentList <- list()
    for(GroupVar in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = GroupVar)

      # Encode
      if(!Scoring) {
        Levels <- sort(data[, unique(get(GroupVar))])
        Levels_Count <- length(Levels)
        GroupMean <- data.table::as.data.table(contr.poly(Levels_Count))
        GroupMean[, eval(GroupVar) := Levels]
        data.table::setnames(GroupMean, names(GroupMean)[-Levels_Count], paste0(GroupVar, Levels[-Levels_Count]))
        data.table::setkeyv(GroupMean, cols = GroupVar)
        if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupVar, "_PolyEncode.csv")))
      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupVar, "_PolyEncode.csv")))
        data.table::setkeyv(GroupMean, cols = GroupVar)
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[paste0(GroupVar, "_PolyEncode")]]
        data.table::setkeyv(GroupMean, cols = GroupVar)
      }

      # Merge back to data
      N <- GroupMean[,.N-1L]
      data[GroupMean, eval(paste0(GroupVar, GroupMean[seq_len(N), get(GroupVar)])) := mget(paste0("i.", GroupVar, GroupMean[seq_len(N), get(GroupVar)]))]
      if(!KeepOriginalFactors) data.table::set(data, j = GroupVar, value = NULL)
      if(!Scoring) ComponentList[[paste0(GroupVar, "_PolyEncode")]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        Vars <- paste0(GroupVar, GroupMean[seq_len(N), get(GroupVar)])
        for(z in Vars) {
          data.table::set(data, i = which(is.na(data[[z]])), j = z, value = ImputeValueScoring)
        }
      }
    }

    # Return
    if(!Scoring && ReturnFactorLevelList) {
      return(list(data = data, FactorCompenents = ComponentList))
    } else {
      return(data)
    }
  }

  # Target Encoding ----
  if(tolower(Method) == "target_encoding") {
    if(!Scoring) ComponentList <- list()
    for(GroupVar in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = GroupVar)

      # Encode
      if(!Scoring) {
        GroupMean <- data[, list(Mean = mean(get(TargetVariable), na.rm = TRUE)), keyby = eval(GroupVar)]
        data.table::setnames(GroupMean, "Mean", paste0(GroupVar, "_TargetEncode"))
        if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupVar, "_TargetEncode.csv")))
      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupVar, "_TargetEncode.csv")))
        data.table::setkeyv(GroupMean, cols = GroupVar)
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[paste0(GroupVar, "_TargetEncode")]]
        data.table::setkeyv(GroupMean, cols = GroupVar)
      }

      # Merge back to data
      data[GroupMean, paste0(GroupVar, "_TargetEncode") := get(paste0("i.", paste0(GroupVar, "_TargetEncode")))]
      if(!KeepOriginalFactors) data.table::set(data, j = GroupVar, value = NULL)
      if(!Scoring) ComponentList[[paste0(GroupVar, "_TargetEncode")]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        data.table::set(data, i = which(is.na(data[[paste0(GroupVar, "_TargetEncode")]])), j = paste0(GroupVar, "_TargetEncode"), value = ImputeValueScoring)
      }
    }

    # Return
    if(!Scoring && ReturnFactorLevelList) {
      return(list(data = data, FactorCompenents = ComponentList))
    } else {
      return(data)
    }
  }

  # WOE Encoding ----
  if(tolower(Method) == "woe") {
    if(!Scoring) ComponentList <- list()
    for(GroupVar in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = GroupVar)

      # Encode
      if(!Scoring) {

        # Encode
        GroupMean <- data[, list(Mean = mean(get(TargetVariable), na.rm = TRUE)), keyby = eval(GroupVar)]
        GroupMean[, paste0(GroupVar, "_WOE") := log(((1 / Mean) - 1) * (data[, sum(get(TargetVariable))] / sum(1-data[[eval(TargetVariable)]])))]
        if(any(is.infinite(GroupMean[[paste0(GroupVar, "_WOE")]]))) {
          data.table::set(GroupMean, i = which(is.infinite(GroupMean[[paste0(GroupVar, "_WOE")]])), j = paste0(GroupVar, "_WOE"), value = 0)
        }
        GroupMean[, ":=" (Mean = NULL)]
        if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupVar, "_WOE.csv")))

      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupVar, "_WOE.csv")))
        data.table::setkeyv(GroupMean, cols = GroupVar)
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[paste0(GroupVar, "_WOE")]]
        data.table::setkeyv(GroupMean, cols = GroupVar)
      }

      # Merge back to data
      data[GroupMean, eval(paste0(GroupVar, "_WOE")) := get(paste0("i.", paste0(GroupVar, "_WOE")))]
      if(!KeepOriginalFactors) data.table::set(data, j = GroupVar, value = NULL)
      if(!Scoring) ComponentList[[paste0(GroupVar, "_WOE")]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        data.table::set(data, i = which(is.na(data[[paste0(GroupVar, "_WOE")]])), j = paste0(GroupVar, "_WOE"), value = ImputeValueScoring)
      }
    }

    # Return
    if(!Scoring && ReturnFactorLevelList) {
      return(list(data = data, FactorCompenents = ComponentList))
    } else {
      return(data)
    }
  }

  # credibility; a.k.a James Stein ----
  if(tolower(Method) == "credibility") {
    if(!Scoring) ComponentList <- list()
    for(GroupVar in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = GroupVar)

      # Encode
      if(!Scoring) {
        GrandMean <- data[, mean(get(TargetVariable), na.rm = TRUE)]
        if(ML_Type == "classification") {
          GroupMean <- data[, list(Mean = mean(get(TargetVariable), na.rm = TRUE), N = .N, Var_Group = mean(get(TargetVariable), na.rm = TRUE) * (1 - mean(get(TargetVariable), na.rm = TRUE)) / .N), keyby = eval(GroupVar)]
          PopVar <- (GrandMean * (1 - GrandMean)) / data[, .N]
          GroupMean[, Adj_Var_Group := Var_Group / (Var_Group + PopVar)]
          GroupMean[, paste0(GroupVar, "_Credibility") := (1 - Adj_Var_Group) * Mean + Adj_Var_Group * GrandMean]
          GroupMean[, ":=" (Mean = NULL, N = NULL, Var_Group = NULL, Adj_Var_Group = NULL)]
        } else if(ML_Type == "regression") {
          GroupMean <- data[, list(Mean = mean(get(TargetVariable), na.rm = TRUE), Var_Group = var(get(TargetVariable), na.rm = TRUE)), keyby = eval(GroupVar)]
          PopVar <- data[, var(get(TargetVariable), na.rm = TRUE)]
          GroupMean[, Adj_Var_Group := Var_Group / (Var_Group + PopVar)]
          GroupMean[, paste0(GroupVar, "_Credibility") := (1 - Adj_Var_Group) * Mean + Adj_Var_Group * GrandMean]
          GroupMean[, ":=" (Mean = NULL, N = NULL)]
        }
        if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupVar, "_Credibility.csv")))
      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupVar, "_Credibility.csv")))
        data.table::setkeyv(GroupMean, cols = GroupVar)
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[paste0(GroupVar, "_Credibility")]]
        data.table::setkeyv(GroupMean, cols = GroupVar)
      }

      # Merge back to data
      data[GroupMean, eval(paste0(GroupVar, "_Credibility")) := get(paste0("i.", paste0(GroupVar, "_Credibility")))]
      if(!KeepOriginalFactors) data.table::set(data, j = GroupVar, value = NULL)
      if(!Scoring) ComponentList[[paste0(GroupVar, "_Credibility")]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        data.table::set(data, i = which(is.na(data[[paste0(GroupVar, "_Credibility")]])), j = paste0(GroupVar, "_Credibility"), value = ImputeValueScoring)
      }
    }

    # Return
    if(!Scoring && ReturnFactorLevelList) {
      return(list(data = data, FactorCompenents = ComponentList))
    } else {
      return(data)
    }
  }

  # M Estimator ----
  if(tolower(Method) == "m_estimator") {

    # Loop through GroupVariables
    if(!Scoring) ComponentList <- list()
    for(GroupVar in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = GroupVar)

      # Encode
      if(!Scoring) {

        # Encode
        GrandMean <- data[, mean(get(TargetVariable), na.rm = TRUE)]
        GroupMean <- data[, list(Mean = sum(get(TargetVariable), na.rm = TRUE), N = .N), keyby = eval(GroupVar)]
        GroupMean[, paste0(GroupVar, "_Mest") := (Mean + GrandMean) / N]
        GroupMean[, ":=" (Mean = NULL, N = NULL)]
        if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupVar, "_Mest.csv")))

      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupVar, "_Mest.csv")))
        data.table::setkeyv(GroupMean, cols = GroupVar)
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[paste0(GroupVar, "_Mest")]]
        data.table::setkeyv(GroupMean, cols = GroupVar)
      }

      # Merge back to data
      data[GroupMean, eval(paste0(GroupVar, "_Mest")) := get(paste0("i.", paste0(GroupVar, "_Mest")))]
      if(!KeepOriginalFactors) data.table::set(data, j = GroupVar, value = NULL)
      if(!Scoring) ComponentList[[paste0(GroupVar, "_Mest")]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        data.table::set(data, i = which(is.na(data[[paste0(GroupVar, "_Mest")]])), j = paste0(GroupVar, "_Mest"), value = ImputeValueScoring)
      }
    }

    # Return
    if(!Scoring && ReturnFactorLevelList) {
      return(list(data = data, FactorCompenents = ComponentList))
    } else {
      return(data)
    }
  }
}

#' @title DummyVariables
#'
#' @description Create dummy variables for categorical variables. You can select the max amount of levels to return per feature.
#'
#' @author Adrian Antico
#' @family Feature Engineering - Character Types
#'
#' @param data Source data
#' @param RunMode 'train' or 'score'
#' @param ArgsList ArgsList_FFE
#' @param SkipCols Vector of column names to remove from data
#' @param NumberLevels Max number of levels per categorical variable
#'
#' @examples
#' \dontrun{
#' Output <- RemixAutoML::DummyVariables(
#'   data = data,
#'   RunMode = "train",
#'   ArgsList = ArgsList_FE,
#'   SkipCols = NULL,
#'   NumberLevels = 3)
#' data <- Output$data
#' ArgsList_FE <- Output$ArgsList
#' }
#'
#' @return A list containing the data and the ArgsList
#' @noRd
DummyVariables <- function(data,
                           RunMode = "train",
                           ArgsList = NULL,
                           SkipCols = NULL,
                           NumberLevels = 3,
                           KeepCharCols = TRUE) {

  # Metadata
  Start <- Sys.time()

  # Run function
  if(tolower(RunMode) == "train") {

    # Colnames
    tempnames <- names(data.table::copy(data))

    # Run function
    data <- RemixAutoML::DummifyDT(
      data = data,
      cols = ArgsList$Data$GroupVariables,
      TopN = NumberLevels,
      KeepFactorCols = KeepCharCols,
      OneHot = FALSE,
      SaveFactorLevels = TRUE,
      SavePath = file.path(ArgsList$MetaData$MetaData_Path),
      ImportFactorLevels = FALSE,
      FactorLevelsList = NULL,
      ClustScore = FALSE,
      ReturnFactorLevels = FALSE)

    # Args tracking
    ArgsList$DummyVariables$cols <- ArgsList$Data$GroupVariables
    ArgsList$DummyVariables$TopN <- NumberLevels
    ArgsList$DummyVariables$KeepFactorCols <- KeepCharCols
    ArgsList$DummyVariables$OneHot <- FALSE
    ArgsList$DummyVariables$SaveFactorLevels <- TRUE
    ArgsList$DummyVariables$SavePath <- ArgsList$MetaData$MetaData_Path
    ArgsList$DummyVariables$ImportFactorLevels <- FALSE
    ArgsList$DummyVariables$FactorLevelsList <- NA
    ArgsList$DummyVariables$ClustScore <- FALSE

    # Column tracking
    ArgsList$FE_Columns$FE_DummyVariables <- setdiff(names(data), tempnames)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$FE_DummyVariables_Training <- difftime(time1 = End, time2 = Start, units = "mins")

  } else {

    # Run function
    data <- RemixAutoML::DummifyDT(
      data = data,
      cols = ArgsList$DummyVariables$cols,
      TopN = ArgsList$DummyVariables$TopN,
      KeepFactorCols = ArgsList$DummyVariables$KeepFactorCols,
      OneHot = ArgsList$DummyVariables$OneHot,
      SaveFactorLevels = ArgsList$DummyVariables$SaveFactorLevels,
      SavePath = ArgsList$DummyVariables$SavePath,
      ImportFactorLevels = TRUE,
      FactorLevelsList = NULL,
      ClustScore = ArgsList$DummyVariables$ClustScore,
      ReturnFactorLevels = ArgsList$DummyVariables$ReturnFactorLevels)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$DummyVariables_Scoring <- difftime(time1 = End, time2 = Start, units = "mins")
  }

  # Return
  return(list(data = data, ArgsList = ArgsList))
}
