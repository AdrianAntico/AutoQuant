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

  # Convert to data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Helmert Encoding ----
  if(tolower(Method) == "helmert") {
    if(!Scoring) ComponentList <- list()
    for(GroupValue in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = eval(GroupValue))

      # Encode
      if(!Scoring) {
        Levels <- sort(data[, unique(get(GroupValue))])
        Levels_Count <- length(Levels)
        GroupMean <- data.table::as.data.table({
          m <- t((diag(seq(Levels_Count-1, 0)) - upper.tri(matrix(1, Levels_Count, Levels_Count)))[-Levels_Count,])
          t(apply(m, 1, rev))
        })
        GroupMean[, eval(GroupValue) := Levels]
        data.table::setnames(GroupMean, names(GroupMean)[-Levels_Count], paste0(GroupValue, Levels[-Levels_Count]))
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
        if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupValue, "_Helmert.csv")))
      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupValue, "_Helmert.csv")))
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[eval(GroupValue)]]
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      }

      # Merge back to data
      N <- GroupMean[,.N-1L]
      data[GroupMean, paste0(GroupValue, GroupMean[seq_len(N), get(GroupValue)]) := mget(paste0("i.", GroupValue, GroupMean[seq_len(N), get(GroupValue)]))]
      if(!KeepOriginalFactors) data.table::set(data, j = GroupValue, value = NULL)
      if(!Scoring) ComponentList[[eval(GroupValue)]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        Vars <- paste0(GroupValue, GroupMean[seq_len(N), get(GroupValue)])
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
    for(GroupValue in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = eval(GroupValue))

      # Encode
      if(!Scoring) {
        Levels <- sort(data[, unique(get(GroupValue))])
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
        GroupMean[, eval(GroupValue) := Levels]
        data.table::setnames(GroupMean, names(GroupMean)[-Levels_Count], paste0(GroupValue, Levels[-Levels_Count]))
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
        if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupValue, "_backward_difference.csv")))
      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupValue, "_backward_difference.csv")))
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[eval(GroupValue)]]
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      }

      # Merge back to data
      N <- GroupMean[,.N-1L]
      data[GroupMean, paste0(GroupValue, GroupMean[seq_len(N), get(GroupValue)]) := mget(paste0("i.", GroupValue, GroupMean[seq_len(N), get(GroupValue)]))]
      if(!KeepOriginalFactors) data.table::set(data, j = GroupValue, value = NULL)
      if(!Scoring) ComponentList[[eval(GroupValue)]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        Vars <- paste0(GroupValue, GroupMean[seq_len(N), get(GroupValue)])
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
    for(GroupValue in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = eval(GroupValue))

      # Encode
      if(!Scoring) {
        Levels <- sort(data[, unique(get(GroupValue))])
        Levels_Count <- length(Levels)
        GroupMean <- data.table::as.data.table(contr.poly(Levels_Count))
        GroupMean[, eval(GroupValue) := Levels]
        data.table::setnames(GroupMean, names(GroupMean)[-Levels_Count], paste0(GroupValue, Levels[-Levels_Count]))
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
        if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupValue, "_PolyEncode.csv")))
      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupValue, "_PolyEncode.csv")))
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[eval(GroupValue)]]
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      }

      # Merge back to data
      N <- GroupMean[,.N-1L]
      data[GroupMean, eval(paste0(GroupValue, GroupMean[seq_len(N), get(GroupValue)])) := mget(paste0("i.", GroupValue, GroupMean[seq_len(N), get(GroupValue)]))]
      if(!KeepOriginalFactors) data.table::set(data, j = GroupValue, value = NULL)
      if(!Scoring) ComponentList[[eval(GroupValue)]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        Vars <- paste0(GroupValue, GroupMean[seq_len(N), get(GroupValue)])
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
    for(GroupValue in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = eval(GroupValue))

      # Encode
      if(!Scoring) {
        if(tolower(ML_Type) == "multiclass") {
          GroupMean <- data[, list(N = .N), by = c(TargetVariable, GroupValue)]
          GroupMean[, paste0(GroupValue, "_TargetEncode") := N / sum(N), by = eval(TargetVariable)]
          GroupMean <- data.table::dcast.data.table(data = GroupMean, formula = get(GroupValue) ~ get(TargetVariable), fun.aggregate = sum, value.var = paste0(GroupValue, "_TargetEncode"), fill = 0)
          data.table::setnames(x = GroupMean, names(GroupMean), c(eval(GroupValue), paste0(GroupValue, "_TargetEncode_TargetLevel_", names(GroupMean)[-1L])))
          data.table::setkeyv(GroupMean, cols = eval(GroupValue))
        } else {
          GroupMean <- data[, list(Mean = mean(get(TargetVariable), na.rm = TRUE)), keyby = eval(GroupValue)]
          data.table::setnames(GroupMean, "Mean", paste0(GroupValue, "_TargetEncode"))
        }
        if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupValue, "_TargetEncode.csv")))
      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupValue, "_TargetEncode.csv")))
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[eval(GroupValue)]]
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      }

      # Merge back to data
      if(tolower(ML_Type) == "multiclass") {
        data[GroupMean, eval(names(GroupMean)[-1L]) := mget(paste0("i.", names(GroupMean)[-1L]))]
      } else {
        data[GroupMean, eval(names(GroupMean)[-1L]) := get(paste0("i.", names(GroupMean)[-1L]))]
      }
      if(!KeepOriginalFactors) data.table::set(data, j = GroupValue, value = NULL)
      if(!Scoring) ComponentList[[eval(GroupValue)]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        data.table::set(data, i = which(is.na(data[[paste0(GroupValue, "_TargetEncode")]])), j = paste0(GroupValue, "_TargetEncode"), value = ImputeValueScoring)
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
    for(GroupValue in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = eval(GroupValue))

      # Encode
      if(!Scoring) {

        # Encode
        if(tolower(ML_Type) == "multiclass") {
          GroupMean <- data[, list(N = .N), by = c(eval(TargetVariable), eval(GroupValue))]
          GroupMean[, N_Target := sum(N), by = c(eval(TargetVariable))]
          GroupMean[, N_All := sum(N)]
          GroupMean[, Mean := N / N_Target]
          GroupMean[, paste0(GroupValue, "_WOE") := log(((1 / Mean) - 1) * ((N_Target / N_All) / (N_All / N_Target)))]
          if(any(is.infinite(GroupMean[[paste0(GroupValue, "_WOE")]]))) {
            data.table::set(GroupMean, i = which(is.infinite(GroupMean[[paste0(GroupValue, "_WOE")]])), j = paste0(GroupValue, "_WOE"), value = 0)
          }
          GroupMean <- data.table::dcast.data.table(data = GroupMean, formula = get(GroupValue) ~ get(TargetVariable), fun.aggregate = sum, value.var = paste0(GroupValue, "_WOE"), fill = 0)
          data.table::setnames(x = GroupMean, names(GroupMean), c(eval(GroupValue), paste0(GroupValue, "_WOE_TargetLevel_", names(GroupMean)[-1L])))
          data.table::setkeyv(GroupMean, cols = eval(GroupValue))
        } else {
          GroupMean <- data[, list(Mean = mean(get(TargetVariable), na.rm = TRUE)), keyby = eval(GroupValue)]
          GroupMean[, paste0(GroupValue, "_WOE") := log(((1 / Mean) - 1) * (data[, sum(get(TargetVariable))] / sum(1-data[[eval(TargetVariable)]])))]
          if(any(is.infinite(GroupMean[[paste0(GroupValue, "_WOE")]]))) {
            data.table::set(GroupMean, i = which(is.infinite(GroupMean[[paste0(GroupValue, "_WOE")]])), j = paste0(GroupValue, "_WOE"), value = 0)
          }
          GroupMean[, ":=" (Mean = NULL)]
        }
        if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupValue, "_WOE.csv")))
      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupValue, "_WOE.csv")))
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[eval(GroupValue)]]
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      }

      # Merge back to data
      if(tolower(ML_Type) == "multiclass") {
        data[GroupMean, eval(names(GroupMean)[-1L]) := mget(paste0("i.", names(GroupMean)[-1L]))]
      } else {
        data[GroupMean, eval(names(GroupMean)[-1L]) := get(paste0("i.", names(GroupMean)[-1L]))]
      }
      if(!KeepOriginalFactors) data.table::set(data, j = GroupValue, value = NULL)
      if(!Scoring) ComponentList[[eval(GroupValue)]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        data.table::set(data, i = which(is.na(data[[paste0(GroupValue, "_WOE")]])), j = paste0(GroupValue, "_WOE"), value = ImputeValueScoring)
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
    for(GroupValue in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = eval(GroupValue))

      # Encode
      if(!Scoring) {
        if(tolower(ML_Type) == "multiclass") {
          GroupMean <- data[, list(N = .N), by = c(TargetVariable, GroupValue)]
          GroupMean[, GrandSum := sum(N)]
          GroupMean[, TargetSum := sum(N), by = eval(TargetVariable)]
          GroupMean[, TargetMean := TargetSum / GrandSum]
          GroupMean[, TargetGroupMean := N / TargetSum]
          GroupMean[, TargetVariance := TargetMean * (1 - TargetMean) / TargetSum]
          GroupMean[, TargetGroupVariance := TargetGroupMean * (1 - TargetGroupMean) / N]
          GroupMean[, Adj_Var_Group := TargetGroupVariance / (TargetGroupVariance + TargetVariance)]
          GroupMean[, paste0(GroupValue, "_Credibility") := (1 - Adj_Var_Group) * TargetGroupMean + Adj_Var_Group * TargetMean]
          GroupMean[, (setdiff(names(GroupMean), c(paste0(GroupValue, "_Credibility"), TargetVariable, GroupValue))) := NULL]
          GroupMean <- data.table::dcast.data.table(data = GroupMean, formula = get(GroupValue) ~ get(TargetVariable), fun.aggregate = sum, value.var = paste0(GroupValue, "_Credibility"), fill = 0)
          data.table::setnames(x = GroupMean, names(GroupMean), c(eval(GroupValue), paste0(GroupValue, "_Credibility_TargetLevel_", names(GroupMean)[-1L])))
          data.table::setkeyv(GroupMean, cols = eval(GroupValue))
        } else {
          GrandMean <- data[, mean(get(TargetVariable), na.rm = TRUE)]
          if(tolower(ML_Type) %chin% c("classification","classifier")) {
            GroupMean <- data[, list(Mean = mean(get(TargetVariable), na.rm = TRUE), N = .N, Var_Group = mean(get(TargetVariable), na.rm = TRUE) * (1 - mean(get(TargetVariable), na.rm = TRUE)) / .N), keyby = eval(GroupValue)]
            PopVar <- (GrandMean * (1 - GrandMean)) / data[, .N]
            GroupMean[, Adj_Var_Group := Var_Group / (Var_Group + PopVar)]
            GroupMean[, paste0(GroupValue, "_Credibility") := (1 - Adj_Var_Group) * Mean + Adj_Var_Group * GrandMean]
            GroupMean[, ":=" (Mean = NULL, N = NULL, Var_Group = NULL, Adj_Var_Group = NULL)]
          } else if(tolower(ML_Type) == "regression") {
            GroupMean <- data[, list(Mean = mean(get(TargetVariable), na.rm = TRUE), Var_Group = var(get(TargetVariable), na.rm = TRUE)), keyby = eval(GroupValue)]
            PopVar <- data[, var(get(TargetVariable), na.rm = TRUE)]
            GroupMean[, Adj_Var_Group := Var_Group / (Var_Group + PopVar)]
            GroupMean[, paste0(GroupValue, "_Credibility") := (1 - Adj_Var_Group) * Mean + Adj_Var_Group * GrandMean]
            GroupMean[, ":=" (Mean = NULL, Var_Group = NULL, Adj_Var_Group = NULL)]
          }
          if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupValue, "_Credibility.csv")))
        }
      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupValue, "_Credibility.csv")))
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[eval(GroupValue)]]
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      }

      # Merge back to data
      if(tolower(ML_Type) == "multiclass") {
        data[GroupMean, eval(names(GroupMean)[-1L]) := mget(paste0("i.", names(GroupMean)[-1L]))]
      } else {
        data[GroupMean, (eval(names(GroupMean)[-1L])) := get(paste0("i.", names(GroupMean)[-1L]))]
      }
      if(!KeepOriginalFactors) data.table::set(data, j = GroupValue, value = NULL)
      if(!Scoring) ComponentList[[eval(GroupValue)]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        data.table::set(data, i = which(is.na(data[[paste0(GroupValue, "_Credibility")]])), j = paste0(GroupValue, "_Credibility"), value = ImputeValueScoring)
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
    for(GroupValue in GroupVariables) {

      # Setkey to join easily
      data.table::setkeyv(x = data, cols = eval(GroupValue))

      # Encode
      if(!Scoring) {

        # Encode
        if(tolower(ML_Type) == "multiclass") {
          GroupMean <- data[, list(N = .N), by = c(TargetVariable, GroupValue)]
          GroupMean[, GrandSum := sum(N)]
          GroupMean[, TargetSum := sum(N), by = eval(TargetVariable)]
          GroupMean[, TargetMean := TargetSum / GrandSum]
          GroupMean[, TargetGroupMean := N / TargetSum]
          GroupMean[, paste0(GroupValue, "_Mest") := (TargetGroupMean + TargetMean) / N]
          GroupMean[, (setdiff(names(GroupMean), c(paste0(GroupValue, "_Mest"), TargetVariable, GroupValue))) := NULL]
          GroupMean <- data.table::dcast.data.table(data = GroupMean, formula = get(GroupValue) ~ get(TargetVariable), fun.aggregate = sum, value.var = paste0(GroupValue, "_Mest"), fill = 0)
          data.table::setnames(x = GroupMean, names(GroupMean), c(eval(GroupValue), paste0(GroupValue, "_Mest_TargetLevel_", names(GroupMean)[-1L])))
          data.table::setkeyv(GroupMean, cols = eval(GroupValue))
        } else {
          GrandMean <- data[, mean(get(TargetVariable), na.rm = TRUE)]
          GroupMean <- data[, list(Mean = sum(get(TargetVariable), na.rm = TRUE), N = .N), keyby = eval(GroupValue)]
          GroupMean[, paste0(GroupValue, "_Mest") := (Mean + GrandMean) / N]
          GroupMean[, ":=" (Mean = NULL, N = NULL)]
        }
        if(!is.null(SavePath)) data.table::fwrite(GroupMean, file = file.path(SavePath, paste0(GroupValue, "_Mest.csv")))
      } else if(Scoring && is.null(SupplyFactorLevelList)) {
        GroupMean <- data.table::fread(file = file.path(SavePath, paste0(GroupValue, "_Mest.csv")))
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      } else if(Scoring && !is.null(SupplyFactorLevelList)) {
        GroupMean <- SupplyFactorLevelList[[paste0(GroupValue)]]
        data.table::setkeyv(GroupMean, cols = eval(GroupValue))
      }

      # Merge back to data
      if(tolower(ML_Type) == "mutliclass") {
        data[GroupMean, eval(names(GroupMean)[-1L]) := mget(paste0("i.", names(GroupMean)[-1L]))]
      } else {
        data[GroupMean, eval(names(GroupMean)[-1L]) := get(paste0("i.", names(GroupMean)[-1L]))]
      }
      if(!KeepOriginalFactors) data.table::set(data, j = GroupValue, value = NULL)
      if(!Scoring) ComponentList[[eval(GroupValue)]] <- GroupMean
      if(Scoring && !is.null(ImputeValueScoring)) {
        data.table::set(data, i = which(is.na(data[[paste0(GroupValue, "_Mest")]])), j = paste0(GroupValue, "_Mest"), value = ImputeValueScoring)
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
#'
#' @examples
#' \dontrun{
#' Output <- RemixAutoML::DummyVariables(
#'   data = data,
#'   RunMode = "train",
#'   ArgsList = ArgsList_FE,
#'   SkipCols = NULL)
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
      TopN = ArgsList$FE_Args$Partial_Dummies$NumberLevels,
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
    ArgsList$DummyVariables$TopN <- ArgsList$FE_Args$Partial_Dummies$NumberLevels
    ArgsList$DummyVariables$KeepFactorCols <- KeepCharCols
    ArgsList$DummyVariables$OneHot <- FALSE
    ArgsList$DummyVariables$SaveFactorLevels <- TRUE
    ArgsList$DummyVariables$SavePath <- ArgsList$MetaData$MetaData_Path
    ArgsList$DummyVariables$ImportFactorLevels <- FALSE
    ArgsList$DummyVariables$FactorLevelsList <- NA
    ArgsList$DummyVariables$ClustScore <- FALSE

    # Column tracking
    ArgsList$FE_Columns$PartialDummies <- setdiff(names(data), tempnames)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$PartialDummies_Training <- difftime(time1 = End, time2 = Start, units = "mins")

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
    ArgsList$RunTime$PartialDummies_Scoring <- difftime(time1 = End, time2 = Start, units = "mins")
  }

  # Return
  return(list(data = data, ArgsList = ArgsList))
}

#' @param RunMode 'train' or 'score'
#' @param ModelType 'classification', 'regression', 'multiclass'
#' @param TrainData Must supply data.table
#' @param ValidationData Optional
#' @param TestData Optional
#' @param TargetVariableName Column name
#' @param CategoricalVariableNames Column names
#' @param EncodeMethod Choose from 'binary', 'm_estimator', 'credibility', 'woe', 'target_encoding', 'poly_encode', 'backward_difference', 'helmert'
#' @param KeepCategoricalVariables Logical
#' @param ReturnMetaData Logical
#' @param MetaDataPath Supply a directory path or NULL
#' @param MetaDataList Supply a metadata list or NULL
#' @param ImputeMissingValue Supply a value or leave NULL to handle elsewhere
#'
#' @noRd
EncodeCharacterVariables <- function(RunMode = 'train',
                                     ModelType = "classification",
                                     TrainData = NULL,
                                     ValidationData = NULL,
                                     TestData = NULL,
                                     TargetVariableName = NULL,
                                     CategoricalVariableNames = NULL,
                                     EncodeMethod = NULL,
                                     KeepCategoricalVariables = FALSE,
                                     ReturnMetaData = FALSE,
                                     MetaDataPath = NULL,
                                     MetaDataList = NULL,
                                     ImputeMissingValue = 0) {

  # Change of variable
  if(RunMode != 'train') Score <- TRUE else Score <- FALSE

  # Prepare data
  if(!is.null(ValidationData) && !is.null(TestData)) {
    data.table::set(TrainData, j = "ID_Factorizer", value = "TRAIN")
    data.table::set(ValidationData, j = "ID_Factorizer", value = "VALIDATE")
    data.table::set(TestData, j = "ID_Factorizer", value = "TEST")
    temp <- data.table::rbindlist(list(TrainData, ValidationData, TestData))
  } else if(!is.null(ValidationData)) {
    data.table::set(TrainData, j = "ID_Factorizer", value = "TRAIN")
    data.table::set(ValidationData, j = "ID_Factorizer", value = "VALIDATE")
    temp <- data.table::rbindlist(list(TrainData, ValidationData))
  } else {
    data.table::set(TrainData, j = "ID_Factorizer", value = "TRAIN")
    temp <- TrainData
  }

  # Encode
  if(EncodeMethod == "binary") {
    temp <- DummifyDT(data=temp, cols=CategoricalVariableNames, KeepFactorCols=KeepCategoricalVariables, OneHot=FALSE, SaveFactorLevels=SaveModelObjects., ReturnFactorLevels=ReturnMetaData, SavePath=MetaDataPath, ImportFactorLevels=FALSE, FactorLevelsList=MetaDataList)
    MetaData <- temp$FactorLevelsList
    temp <- temp$data
  } else if(EncodeMethod %chin% c('m_estimator', 'credibility', 'woe', 'target_encoding')) {
    temp_train <- temp[ID_Factorizer == "TRAIN"]
    temp1 <- CategoricalEncoding(data=temp_train, ML_Type=ModelType, GroupVariables=CategoricalVariableNames, TargetVariable=TargetVariableName, Method=EncodeMethod, SavePath=MetaDataPath, Scoring=Score, ImputeValueScoring=ImputeMissingValue, ReturnFactorLevelList=TRUE, SupplyFactorLevelList=MetaDataList, KeepOriginalFactors=KeepCategoricalVariables)
    MetaData <- temp1$FactorCompenents
    temp_train <- temp1$data
    if(!is.null(ValidationData) && !is.null(TestData)) {
      temp_validate <- temp[ID_Factorizer == "VALIDATE"]
      temp_test <- temp[ID_Factorizer == "TEST"]
      temp_other <- data.table::rbindlist(list(temp_validate, temp_test))
      temp2 <- CategoricalEncoding(data=temp_other, ML_Type=ModelType, GroupVariables=CategoricalVariableNames, TargetVariable=TargetVariableName, Method=EncodeMethod, SavePath=MetaDataPath, Scoring=Score, ImputeValueScoring=ImputeMissingValue, ReturnFactorLevelList=FALSE, SupplyFactorLevelList=MetaDataList, KeepOriginalFactors=KeepCategoricalVariables)
      temp <- data.table::rbindlist(list(temp2,temp_train))
    } else if(!is.null(ValidationData)) {
      temp_validate <- temp[ID_Factorizer == "VALIDATE"]
      temp2 <- CategoricalEncoding(data=temp_validate, ML_Type=ModelType, GroupVariables=CategoricalVariableNames, TargetVariable=TargetVariableName, Method=EncodeMethod, SavePath=MetaDataPath, Scoring=Score, ImputeValueScoring=ImputeMissingValue, ReturnFactorLevelList=FALSE, SupplyFactorLevelList=MetaDataList, KeepOriginalFactors=KeepCategoricalVariables)
      temp <- data.table::rbindlist(list(temp2,temp_train))
    } else {
      temp <- temp_train
    }
  } else {
    temp <- RemixAutoML::CategoricalEncoding(data=temp, ML_Type=ModelType, GroupVariables=CategoricalVariableNames, TargetVariable=TargetVariableName, Method=EncodeMethod, SavePath=MetaDataPath, Scoring=Score, ImputeValueScoring=ImputeMissingValue, ReturnFactorLevelList=TRUE, SupplyFactorLevelList=MetaDataList, KeepOriginalFactors=KeepCategoricalVariables)
    MetaData <- temp$FactorCompenents
    temp <- temp$data
  }

  # Finalize data
  TrainData <- temp[ID_Factorizer == "TRAIN"]
  data.table::set(TrainData, j = "ID_Factorizer", value = NULL)
  if(!is.null(ValidationData)) {
    ValidationData <- temp[ID_Factorizer == "VALIDATE"]
    data.table::set(ValidationData, j = "ID_Factorizer", value = NULL)
  }
  if(!is.null(TestData)) {
    TestData <- temp[ID_Factorizer == "TEST"]
    data.table::set(TestData, j = "ID_Factorizer", value = NULL)
  }

  # Return
  return(list(
    TrainData = TrainData,
    ValidationData = ValidationData,
    TestData = TestData,
    MetaData = if(exists("MetaData")) MetaData else NULL))
}

#' @param RunMode Passthrough
#' @param ArgsList Passthrough
#' @param TrainData Passthrough
#' @param ValidationData Passthrough
#' @param TestData Passthrough
#' @param ScoringData Passthrough
#'
#' @noRd
Encoding <- function(RunMode = 'train',
                     ArgsList = NULL,
                     TrainData = NULL,
                     ValidationData = NULL,
                     TestData = NULL,
                     ScoringData = NULL) {

  # Metadata
  Start <- Sys.time()

  # Run function
  if(tolower(RunMode) == "train") {

    # Colnames
    tempnames <- names(data.table::copy(TrainData))

    # Dummify dataTrain Categorical Features ----
    Output <- RemixAutoML:::EncodeCharacterVariables(
      RunMode = 'train',
      ModelType = ArgsList$MetaData$ModelType,
      TrainData = TrainData,
      ValidationData = ValidationData,
      TestData = TestData,
      TargetVariableName = ArgsList$Data$TargetVariables,
      CategoricalVariableNames = ArgsList$Data$GroupVariables,
      EncodeMethod = ArgsList$FE_Args$Encoding$EncodeMethod,
      KeepCategoricalVariables = ArgsList$FE_Args$Encoding$KeepCharColumns,
      ReturnMetaData = FALSE,
      MetaDataPath = ArgsList$MetaData$MetaData_Path,
      MetaDataList = NULL,
      ImputeMissingValue = ArgsList$FE_Args$Encoding$EncodeImpute)

    # Output
    TrainData <- Output$TrainData; Output$TrainData <- NULL
    ValidationData <- Output$ValidationData; Output$ValidationData <- NULL
    TestData <- Output$TestData; Output$TestData. <- NULL

    # Args tracking
    ArgsList$Encoding$ModelType <- ArgsList$MetaData$ModelType
    ArgsList$Encoding$TargetVariableName <- ArgsList$Data$TargetVariables
    ArgsList$Encoding$CategoricalVariableNames <- ArgsList$Data$GroupVariables
    ArgsList$Encoding$EncodeMethod <- ArgsList$FE_Args$Encoding$EncodeMethod
    ArgsList$Encoding$KeepCategoricalVariables <- ArgsList$FE_Args$Encoding$KeepCharColumns
    ArgsList$Encoding$ReturnMetaData <- FALSE
    ArgsList$Encoding$MetaDataPath <- ArgsList$MetaData$Results_Path
    ArgsList$Encoding$MetaDataList <- NULL
    ArgsList$Encoding$ImputeMissingValue <- ArgsList$FE_Args$Encoding$EncodeImpute

    # Column tracking
    ArgsList$FE_Columns$Encoding <- setdiff(names(TrainData), tempnames)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$Encoding_Training <- difftime(time1 = End, time2 = Start, units = "mins")

  } else {

    # Dummify dataTrain Categorical Features
    ScoringData <- CategoricalEncoding(
      data = ScoringData,
      ML_Type = ArgsList$Encoding$ModelType,
      GroupVariables = ArgsList$Encoding$CategoricalVariableNames,
      TargetVariable = ArgsList$Data$TargetVariables,
      Method = ArgsList$Encoding$EncodeMethod,
      SavePath = ArgsList$MetaData$Results_Path,
      Scoring = TRUE,
      ImputeValueScoring = ArgsList$Encoding$ImputeMissingValue,
      ReturnFactorLevelList = FALSE,
      SupplyFactorLevelList = NULL,
      KeepOriginalFactors = ArgsList$Encoding$KeepCategoricalVariables)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$Encoding_Scoring <- difftime(time1 = End, time2 = Start, units = "mins")
  }

  # Return
  return(list(TrainData = TrainData, ValidationData = ValidationData, TestData = TestData, ScoringData = ScoringData, ArgsList = ArgsList))
}
