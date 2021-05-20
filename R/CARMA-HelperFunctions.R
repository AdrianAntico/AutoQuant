#' @title DifferenceData
#'
#' @description DifferenceData differences your data set
#'
#' @family Feature Engineering
#'
#' @author Adrian Antico
#'
#' @param data Source data
#' @param ColumnsToDiff The column numbers you want differenced
#' @param CARMA Set to TRUE for CARMA functions
#' @param TargetVariable The target variable name
#' @param GroupingVariable Difference data by group
#'
#' @noRd
DifferenceData <- function(data,
                           ColumnsToDiff = c(names(data)[2:ncol(data)]),
                           CARMA = FALSE,
                           TargetVariable = NULL,
                           GroupingVariable = NULL) {

  # Keep First Row of Data
  if(!is.null(GroupingVariable)) {
    FirstRow <- data[data[, .I[1L], get(GroupingVariable)]$V1]
  } else {
    FirstRow <- data[1L]
  }

  # Keep Last Row of Target Variable
  if(!is.null(GroupingVariable)) {
    LastRow <- data[data[, .I[.N], get(GroupingVariable)]]$V1
  } else {
    LastRow <- data[data[, .I[.N]]]
  }

  # Diff data
  if(!is.null(GroupingVariable)) {
    DiffData <- cbind(data[seq_len(data[, .I[.N-1], get(GroupingVariable)]$V1),1],data[, lapply(.SD,diff), by = eval(GroupingVariable), .SDcols = ColumnsToDiff])
  } else {
    DiffData <- cbind(data[seq_len(nrow(data)-1),.SD, .SDcols = c(setdiff(names(data),ColumnsToDiff))],data[, lapply(.SD,diff), .SDcols = ColumnsToDiff])
  }

  # Return data
  if(!CARMA) {
    return(list(DiffData = DiffData, FirstRow = FirstRow, LastRow = data[nrow(data),]))
  } else {
    if(!is.null(GroupingVariable)) {
      FirstRow <- FirstRow[, get(TargetVariable), by = eval(GroupingVariable)]
      return(list(DiffData = DiffData, FirstRow = FirstRow, LastRow = LastRow))
    } else {
      return(list(DiffData = DiffData, FirstRow = FirstRow, LastRow = LastRow))
    }
  }
}

#' @title DifferenceDataReverse
#'
#' @description DifferenceDataReverse reverses the difference
#'
#' @family Feature Engineering
#'
#' @author Adrian Antico
#'
#' @param data Pre differenced scoring data
#' @param ScoreData Predicted values from ML model
#' @param LastRow The last row from training data target variables
#' @param TargetCol Target column name
#' @param CARMA Set to TRUE for CARMA utilization
#' @param FirstRow The first row of the target variable
#' @param GroupingVariables Group columns
#'
#' @noRd
DifferenceDataReverse <- function(data,
                                  ScoreData = Forecasts$Predictions,
                                  LastRow = DiffTrainOutput$LastRow$Weekly_Sales,
                                  CARMA = FALSE,
                                  TargetCol = TargetColumnName,
                                  FirstRow = DiffTrainOutput$FirstRow,
                                  GroupingVariables = NULL) {

  ModifiedData <- data.table::copy(data)
  if(!CARMA) {
    if(is.null(GroupingVariables)) {
      return(ModifiedData[, Predictions := cumsum(c(LastRow,ScoreData))])
    }
  } else {
    if(is.null(GroupingVariables)) {
      x <- cumsum(c(FirstRow,ModifiedData[[eval(TargetCol)]]))
      # XGBoostCARMA matches, catboostCARMA is off by 1
      if(length(x) != ModifiedData[,.N]) x <- x[-1L]
      return(ModifiedData[, eval(TargetCol) := x][, Predictions := x])
    }
  }
}

#' @title FullFactorialCatFeatures
#'
#' @description FullFactorialCatFeatures reverses the difference
#'
#' @family Data Wrangling
#'
#' @author Adrian Antico
#'
#' @param GroupVars Character vector of categorical columns to fully interact
#' @param MaxCombin The max K in N choose K. If NULL, K will loop through 1 to length(GroupVars)
#' @param BottomsUp TRUE or FALSE. TRUE starts with the most comlex interaction to the main effects
#'
#' @noRd
FullFactorialCatFeatures <- function(GroupVars = GroupVariables,
                                     MaxCombin = NULL,
                                     BottomsUp = TRUE) {

  if(is.null(MaxCombin)) {
    MaxCombin <- N <- length(GroupVars)
  } else {
    N <- MaxCombin
  }
  Categoricals <- c()

  # N choose 1 case
  for(j in seq_along(GroupVars)) Categoricals <- c(Categoricals,GroupVars[j])

  # N choose i for 2 <= i < N
  for(i in seq_len(N)[-1L]) {

    # Case 2: N choose 2 up to N choose N-1: Middle-Hierarchy Interactions
    if(MaxCombin == length(GroupVars)) {
      if(i < N) {
        temp <- combinat::combn(GroupVars, m = i)
        temp2 <- c()
        for(k in seq_len(ncol(temp))) {
          for(l in seq_len(i)) {
            if(l == 1L) {
              temp2 <- temp[l,k]
            } else {
              temp2 <- paste(temp2,temp[l,k], sep = "_")
            }
          }
          Categoricals <- c(Categoricals, temp2)
        }

        # Case 3: N choose N - Full Interaction
      } else if(i == length(GroupVars)) {
        temp <- combinat::combn(GroupVars, m = i)
        for(m in seq_len(N)) {
          if(m == 1) {
            temp2 <- temp[m]
          } else {
            temp2 <- paste(temp2,temp[m], sep = "_")
          }
        }
        Categoricals <- c(Categoricals, temp2)
      }
    } else {
      if(i <= N) {
        temp <- combinat::combn(GroupVars, m = i)
        temp2 <- c()
        for(k in seq_len(ncol(temp))) {
          for(l in seq_len(i)) {
            if(l == 1L) {
              temp2 <- temp[l,k]
            } else {
              temp2 <- paste(temp2,temp[l,k], sep = "_")
            }
          }
          Categoricals <- c(Categoricals, temp2)
        }

        # Case 3: N choose N - Full Interaction
      } else if(i == length(GroupVars)) {
        temp <- combinat::combn(GroupVars, m = i)
        for(m in seq_len(N)) {
          if(m == 1) {
            temp2 <- temp[m]
          } else {
            temp2 <- paste(temp2,temp[m], sep = "_")
          }
        }
        Categoricals <- c(Categoricals, temp2)
      }
    }

  }

  # Order of output
  if(BottomsUp) return(rev(Categoricals)) else return(Categoricals)
}

#' @title CARMA_GroupHierarchyCheck
#'
#' @author Adrian Antico
#' @family Carma Helper
#'
#' @param data data fed into function
#' @param Group_Variables Takes GroupVariables from caram function
#' @param HierarchyGroups Vector of group variables
#' @noRd
CARMA_GroupHierarchyCheck <- function(data = data,
                                      Group_Variables = GroupVariables,
                                      HierarchyGroups = HierarchGroups) {

  # Simple organization of option sets
  if(length(Group_Variables) > 1 && !is.null(HierarchyGroups)) {
    if("GroupVar" %chin% names(data)) data[, eval(Group_Variables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
    HierarchSupplyValue <- HierarchyGroups
  } else {
    HierarchSupplyValue <- NULL
  }
  if(!is.null(Group_Variables)) {
    IndependentSupplyValue <- CARMA_Get_IndepentVariablesPass(HierarchyGroups)
  } else {
    IndependentSupplyValue <- NULL
  }
  return(list(data = data, HierarchSupplyValue = HierarchyGroups, IndependentSupplyValue = IndependentSupplyValue))
}

#' @title CARMA_Define_Args
#'
#' @description CARMA_Define_Args is to help manage carma code
#'
#' @author Adrian Antico
#' @family Carma Helper
#'
#' @param TimeUnit = TimeUnit
#' @param TimeGroups = TimeGroups
#' @param HierarchGroups = HierarchGroups
#' @param GroupVariables = GroupVariables
#' @param FC_Periods = FC_Periods
#' @param PartitionType = PartitionType
#' @param TrainOnFull = TrainOnFull
#' @param SplitRatios = SplitRatios
#' @param SD_Periods = 0L turns it off, otherwise values must be greater than 1 such as c(2L,5L,6L,25L)
#' @param Skew_Periods = 0L turns it off, otherwise values must be greater than 2 such as c(3L,5L,6L,25L)
#' @param Kurt_Periods = 0L turns it off, otherwise values must be greater than 3 such as c(4L,5L,6L,25L)
#' @param Quantile_Periods = 0L turns it off, otherwise values must be greater than 3 such as c(5L,6L,25L)
#' @param TaskType = TaskType
#' @param BootStrapType = BootStrapType
#' @param GrowPolicy = GrowPolicy
#' @param TimeWeights = TimeWeights
#' @param HolidayLookback = HolidayLookback
#' @param Difference = Difference
#' @param NonNegativePred = NonNegativePred
#' @noRd
CARMA_Define_Args <- function(TimeUnit = NULL,
                              TimeGroups = NULL,
                              HierarchGroups = NULL,
                              GroupVariables = NULL,
                              FC_Periods = NULL,
                              PartitionType = NULL,
                              TrainOnFull = NULL,
                              SplitRatios = NULL,
                              SD_Periods = 0L,
                              Skew_Periods = 0L,
                              Kurt_Periods = 0L,
                              Quantile_Periods = 0L,
                              TaskType = "GPU",
                              BootStrapType = "MVS",
                              GrowPolicy = "SymmetricTree",
                              TimeWeights = NULL,
                              HolidayLookback = 7,
                              Difference = FALSE,
                              NonNegativePred = FALSE) {

  # TimeUnit and TimeGroups Args
  TimeGroupPlaceHolder <- c()
  if(any(tolower(c("hours","hour","hr","hrs","hourly")) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "hour")
  }
  if(any(tolower(c("hours","hour","hr","hrs","hourly")) %chin% tolower(TimeUnit))) {
    TimeUnit <- "hour"
  }
  if(any(tolower(c("days","day","dy","dd","d")) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "day")
  }
  if(any(tolower(c("days","day","dy","dd","d")) %chin% tolower(TimeUnit))) {
    TimeUnit <- "day"
  }
  if(any(tolower(c("weeks","week","weaks","weak","wk","wkly","wks")) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "weeks")
  }
  if(any(tolower(c("weeks","week","weaks","weak","wk","wkly","wks")) %chin% tolower(TimeUnit))) {
    TimeUnit <- "week"
  }
  if(any(tolower(c("months","month","mth","mnth","monthly","mnthly")) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "months")
  }
  if(any(tolower(c("months","month","mth","mnth","monthly","mnthly")) %chin% tolower(TimeUnit))) {
    TimeUnit <- "months"
  }
  if(any(tolower(c("quarter","qarter","quarterly","q","qtly")) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "quarter")
  }
  if(any(tolower(c("quarter","qarter","quarterly","q","qtly")) %chin% tolower(TimeUnit))) {
    TimeUnit <- "quarter"
  }
  if(any(tolower(c("years","year","annual","yearly","annually","ann","yr","yrly")) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, "year")
  }
  if(any(tolower(c("years","year","annual","yearly","annually","ann","yr","yrly")) %chin% tolower(TimeUnit))) {
    TimeUnit <- "year"
  }

  # IndependentVariablePass is for referencing the interaction group column names
  IndepentVariablesPass <- CARMA_Get_IndepentVariablesPass(HierarchGroups)

  # FC Periods
  if(FC_Periods <= 1L) {
    FC_Periods <- 2L
  } else {
    FC_Periods <- FC_Periods + 1L
  }

  # Check arguments
  if(!(tolower(PartitionType) %chin% c("random", "time", "timeseries"))) stop("PartitionType needs to be one of 'random', 'time', or 'timeseries'")
  if(tolower(PartitionType) == "timeseries" && is.null(GroupVariables)) PartitionType <- "time"

  # Additional Args check ----
  if(NonNegativePred && Difference) NonNegativePred <- FALSE
  if(!is.null(HolidayLookback) && !is.numeric(HolidayLookback)) stop("HolidayLookback has to be numeric")
  if(!is.null(TimeWeights) && length(TimeWeights) != 1L) TimeWeights <- NULL
  if(is.null(GrowPolicy)) {
    GrowPolicy <- "SymmetricTree"
  } else {
    if(tolower(GrowPolicy) == "lossguide") GrowPolicy <- "Lossguide"
    if(tolower(GrowPolicy) == "depthwise") GrowPolicy <- "Depthwise"
  }
  if(is.null(BootStrapType)) {
    if(TaskType == "GPU") BootStrapType <- "Bayesian" else BootStrapType <- "MVS"
  } else {
    if(TaskType == "GPU" && BootStrapType == "MVS") BootStrapType <- "Bayesian"
  }

  # Return args
  return(list(
    TimeUnit = TimeUnit,
    TimeGroups = TimeGroupPlaceHolder,
    IndepentVariablesPass = IndepentVariablesPass,
    HierarchGroups = HierarchGroups,
    GroupVariables = GroupVariables,
    FC_Periods = FC_Periods,
    TaskType = TaskType,
    BootStrapType = BootStrapType,
    GrowPolicy = GrowPolicy,
    TimeWeights = TimeWeights,
    HolidayLookback = HolidayLookback,
    Difference = Difference,
    NonNegativePred = NonNegativePred))
}

#' @param data. Passthrough
#' @param XREGS. Passthrough
#' @param TrainOnFull. Passthrough
#' @param Difference. Passthrough
#' @param FC_Periods. Passthrough
#' @param HoldOutPeriods. Passthrough
#' @param DateColumnName. Passthrough
#'
#' @noRd
CarmaFCHorizon <- function(data. = data,
                           XREGS. = XREGS,
                           TrainOnFull. = TrainOnFull,
                           Difference. = Difference,
                           FC_Periods. = FC_Periods,
                           HoldOutPeriods. = HoldOutPeriods,
                           DateColumnName. = DateColumnName) {
  if(!is.null(XREGS.) && TrainOnFull.) {
    if(Difference.) {
      FC_Periods. <- min(-1L + length(unique(XREGS.[[eval(DateColumnName.)]])) - length(unique(data.[[eval(DateColumnName.)]])), FC_Periods.)
    } else {
      FC_Periods. <- min(length(unique(XREGS.[[eval(DateColumnName.)]])) - length(unique(data.[[eval(DateColumnName.)]])), FC_Periods.)
    }
    if(FC_Periods. < 1) stop("Your XREGS does not have forward looking data")
  } else if(!is.null(XREGS.)) {
    FC_Periods. <- HoldOutPeriods.
    HoldOutPeriods. <- FC_Periods.
  }
  return(list(FC_Periods = FC_Periods., HoldOutPeriods = HoldOutPeriods.))
}

#' @param data. Passthrough
#' @param XREGS. Passthrough
#' @param GroupVariables. Passthrough
#' @param DateColumnName. Passthrough
#' @param TargetColumnName. Passthrough
#'
#' @noRd
CarmaMergeXREGS <- function(data. = data,
                            XREGS. = XREGS,
                            TargetColumnName. = TargetColumnName,
                            GroupVariables. = GroupVariables,
                            DateColumnName. = DateColumnName) {

  if(!is.null(GroupVariables.)) {
    if(!"GroupVar" %chin% names(XREGS.)) XREGS.[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables.]
    if(any(GroupVariables. %chin% names(XREGS.))) for(g in GroupVariables.) data.table::setnames(x = XREGS., old = eval(g), new = paste0("Add_",eval(g)))
    if(length(GroupVariables.) > 1) {
      data.[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables.]
      data. <- merge(data., XREGS., by = c("GroupVar", eval(DateColumnName.)), all.x = TRUE)
      data. <- ModelDataPrep(data = data., Impute = TRUE, CharToFactor = FALSE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
    } else {
      if(class(data.[[GroupVariables.]])[1L] != class(XREGS.[["GroupVar"]])[[1L]]) {
        data.table::set(data., j = GroupVariables., value = as.character(data.[[GroupVariables.]]))
        data.table::set(XREGS., j = "GroupVar", value = as.character(XREGS.[["GroupVar"]]))
      }
      data. <- merge(data., XREGS., by.x = c(eval(GroupVariables.), eval(DateColumnName.)), by.y = c("GroupVar", eval(DateColumnName.)), all.x = TRUE)
      data. <- ModelDataPrep(data = data., Impute = TRUE, CharToFactor = FALSE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
    }
  } else {
    data. <- merge(data., XREGS., by = c(eval(DateColumnName.)), all.x = TRUE)
    data. <- ModelDataPrep(data = data., Impute = TRUE, CharToFactor = FALSE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
  }
  if(any(eval(TargetColumnName.) %chin% names(XREGS.))) data.table::set(XREGS., j = eval(TargetColumnName.), value = NULL)
  return(list(data = data., XREGS = XREGS.))
}

#' @param data. Passthrough
#' @param XREGS. Passthrough
#' @param GroupVariables. Passthrough
#' @param DateColumnName. Passthrough
#' @param TargetColumnName. Passthrough
#'
#' @noRd
CarmaSubsetColumns <- function(data. = data,
                               XREGS. = XREGS,
                               GroupVariables. = GroupVariables,
                               DateColumnName. = DateColumnName,
                               TargetColumnName. = TargetColumnName) {
  if(!is.null(XREGS.)) {
    if(!is.null(GroupVariables.)) {
      xx <- unique(c(TargetColumnName.,DateColumnName.,GroupVariables., names(XREGS.)))
      xx <- xx[!xx %chin% "GroupVar"]
      data. <- data.[, .SD, .SDcols = c(xx)]
    } else {
      xx <- unique(c(TargetColumnName.,DateColumnName., names(XREGS.)))
      data. <- data.[, .SD, .SDcols = c(xx)]
    }
  } else {
    if(!is.null(GroupVariables.)) {
      data. <- data.[, .SD, .SDcols = c(DateColumnName., TargetColumnName., GroupVariables.)]
    } else {
      data. <- data.[, .SD, .SDcols = c(DateColumnName., TargetColumnName.)]
    }
  }
  return(data.)
}

#' @param data. Passthrough
#' @param XREGS. Passthrough
#' @param FourierTerms. Passthrough
#' @param TimeUnit. Passthrough
#' @param TargetColumnName. Passthrough
#' @param GroupVariables. Passthrough
#' @param DateColumnName. Passthrough
#' @param HierarchGroups. Passthrough
#'
#' @noRd
CarmaFourier <- function(data. = data,
                         XREGS. = XREGS,
                         FourierTerms. = FourierTerms,
                         TimeUnit. = TimeUnit,
                         TargetColumnName. = TargetColumnName,
                         GroupVariables. = GroupVariables,
                         DateColumnName. = DateColumnName,
                         HierarchGroups. = HierarchGroups) {
  if(FourierTerms. > 0L) {

    # Split GroupVar and Define HierarchyGroups and IndependentGroups
    Output <- CARMA_GroupHierarchyCheck(data = data., Group_Variables = GroupVariables., HierarchyGroups = HierarchGroups.)
    data. <- Output$data
    HierarchSupplyValue. <- Output$HierarchSupplyValue
    IndependentSupplyValue. <- Output$IndependentSupplyValue

    # Run Independently or Hierarchy (Source: EconometricsFunctions.R)
    Output <- tryCatch({AutoHierarchicalFourier(
      datax = data.,
      xRegs = names(XREGS.),
      FourierTermS = FourierTerms.,
      TimeUniT = TimeUnit.,
      FC_PeriodS = FC_Periods.,
      TargetColumN = TargetColumnName.,
      DateColumN = DateColumnName.,
      HierarchGroups = HierarchSupplyValue.,
      IndependentGroups = IndependentSupplyValue.)},
      error = function(x) NULL)

    # Store Objects If No Error in Hierarchy Run
    if(!is.null(Output)) {
      if(Output$data[, .N] != 0) {
        data. <- Output$data
        FourierFC. <- Output$FourierFC
      } else {
        print("Turning off Fourier Terms. Failed to build.")
        FourierTerms. <- 0
      }
    } else {
      print("Turning off Fourier Terms. Failed to build.")
      FourierTerms. <- 0
    }

    # If Fourier is turned off, concatenate grouping cols
    if(FourierTerms. == 0) {
      if(!is.null(HierarchGroups.)) {
        if(length(HierarchGroups.) > 1) {
          if(any(HierarchGroups. %chin% names(data.))) {
            data.[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = c(HierarchGroups.)]
            data.[, eval(HierarchGroups.) := NULL]
          }
        } else {
          data.[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = c(HierarchGroups.)]
          if(HierarchGroups. != "GroupVar") {
            data.[, eval(HierarchGroups.) := NULL]
          }
        }
      } else if(!is.null(GroupVariables.)) {
        if(all(GroupVariables. %chin% names(data.))) {
          data.[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = c(GroupVariables.)]
        }
      }
    } else if(!is.null(HierarchGroups.)) {
      if(!"GroupVar" %chin% names(data.)) data.[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = c(HierarchGroups.)]
    }
  } else {
    FourierFC. <- NULL
  }
  if(!exists("FourierFC.")) FourierFC. <- NULL
  return(list(data = data., FourierFC = FourierFC., FourierTerms = FourierTerms.))
}

#' @param GroupVariables. Passthrough
#' @param Difference. Passthrough
#' @param data. Passthrough
#' @param TargetColumnName. Passthrough
#' @param FC_Periods. Passthrough
#'
#' @noRd
CarmaDifferencing <- function(GroupVariables. = GroupVariables,
                              Difference. = Difference,
                              data. = data,
                              TargetColumnName. = TargetColumnName,
                              FC_Periods. = FC_Periods) {

  if(!is.null(GroupVariables.) && Difference.) {
    data.[, TargetDiffMidStep := data.table::shift(x = get(TargetColumnName.), n = 1, fill = NA, type = "lag"), by = c("GroupVar")][, ModTarget := get(TargetColumnName.) - TargetDiffMidStep]
    dataStart <- data.[is.na(TargetDiffMidStep)]
    data. <- data.[!is.na(TargetDiffMidStep)]
    FC_Periods. <- FC_Periods. + 1L
    Train <- NULL
    DiffTrainOutput <- NULL
  } else if(Difference.) {
    DiffTrainOutput <- DifferenceData(
      data = data.,
      ColumnsToDiff = eval(TargetColumnName.),
      CARMA = TRUE,
      TargetVariable = eval(TargetColumnName.),
      GroupingVariable = NULL)
    data. <- DiffTrainOutput$DiffData
    FC_Periods. <- FC_Periods. + 1L
    dataStart <- NULL
  } else {
    dataStart <- NULL
    Train <- NULL
    DiffTrainOutput <- NULL
  }
  return(list(
    data = data.,
    dataStart = dataStart,
    FC_Periods = FC_Periods.,
    Train = NULL,
    DiffTrainOutput = DiffTrainOutput))
}

#' @param data. Passthrough
#' @param XREGS. Passthrough
#' @param DateColumnName. Passthrough
#' @param TimeUnit. Passthrough
#'
#' @noRd
CarmaDateStandardize <- function(data. = data,
                                 XREGS. = NULL,
                                 DateColumnName. = DateColumnName,
                                 TimeUnit. = TimeUnit) {
  if(is.character(data.[[eval(DateColumnName.)]])) {
    if(!(tolower(TimeUnit.) %chin% c("1min","5min","10min","15min","30min","hour"))) {
      x <- data.[1L, get(DateColumnName.)]
      x1 <- lubridate::guess_formats(x, orders = c("mdY", "BdY", "Bdy", "bdY", "bdy", "mdy", "dby", "Ymd", "Ydm"))
      data.[, eval(DateColumnName.) := as.Date(get(DateColumnName.), tryFormats = x1)]
    } else {
      data.[, eval(DateColumnName.) := as.POSIXct(get(DateColumnName.))]
    }
  }
  if(!is.null(XREGS.) && is.character(XREGS.[[eval(DateColumnName.)]])) {
    if(!(tolower(TimeUnit.) %chin% c("1min","5min","10min","15min","30min","hour"))) {
      x <- XREGS.[1L, get(DateColumnName.)]
      x1 <- lubridate::guess_formats(x, orders = c("mdY", "BdY", "Bdy", "bdY", "bdy", "mdy", "dby", "Ymd", "Ydm"))
      XREGS.[, eval(DateColumnName.) := as.Date(get(DateColumnName.), tryFormats = x1)]
    } else {
      XREGS.[, eval(DateColumnName.) := as.POSIXct(get(DateColumnName.))]
    }
  }
  return(list(data = data., XREGS = XREGS.))
}

#' @param train. Passthrough
#' @param TimeWeights. Passthrough
#' @param GroupVariables. Passthrough
#' @param DateColumnName. Passthrough
#'
#' @noRd
CarmaTimeWeights <- function(train. = train,
                             TimeWeights. = TimeWeights,
                             GroupVariables. = GroupVariables,
                             DateColumnName. = DateColumnName) {
  if(!is.null(TimeWeights.)) {
    if(!is.null(GroupVariables.)) {
      data.table::setorderv(x = train., cols = c("GroupVar", DateColumnName.), order = c(1,-1))
      train.[, PowerValue := seq_len(.N), by = "GroupVar"]
      train.[, Weights := eval(TimeWeights.) ^ PowerValue]
      data.table::setorderv(x = train., cols = c("GroupVar", DateColumnName.), order = c(1,1))
      train.[, ":=" (PowerValue = NULL)]
    } else {
      data.table::setorderv(x = train., cols = c(DateColumnName.), order = c(-1))
      train.[, PowerValue := seq_len(.N)]
      train.[, Weights := eval(TimeWeights.) ^ PowerValue]
      data.table::setorderv(x = train., cols = c(DateColumnName.), order = c(1))
      train.[, ":=" (PowerValue = NULL)]
    }
    return(train.)
  } else {
    return(train.)
  }
}

#' @param data. Passthrough
#' @param DateColumnName. Passthrough
#' @param TimeUnit. Passthrough
#'
#' @noRd
CarmaTruncateData <- function(data. = data,
                              DateColumnName. = DateColumnName,
                              TimeUnit. = TimeUnit) {
  mindate <- data.[, min(get(DateColumnName.))]
  if(tolower(TimeUnit.) %chin% c("hour","hours")) {
    newdate <- mindate + lubridate::hours(val + 1)
  } else if(tolower(TimeUnit.) %chin% c("1min","1mins","1minute","1minutes")) {
    newdate <- mindate + lubridate::minutes(val + 1)
  } else if(tolower(TimeUnit.) %chin% c("5min","5mins","5minute","5minutes")) {
    newdate <- mindate + lubridate::minutes(val + 5)
  } else if(tolower(TimeUnit.) %chin% c("10min","10mins","10minute","10minutes")) {
    newdate <- mindate + lubridate::minutes(val + 10)
  } else if(tolower(TimeUnit.) %chin% c("15min","15mins","15minute","15minutes")) {
    newdate <- mindate + lubridate::minutes(val + 15)
  } else if(tolower(TimeUnit.) %chin% c("30min","30mins","30minute","30minutes")) {
    newdate <- mindate + lubridate::minutes(val + 30)
  } else if(tolower(TimeUnit.) %chin% c("day","days")) {
    newdate <- mindate + lubridate::days(val + 1)
  } else if(tolower(TimeUnit.) %chin% c("week","weeks")) {
    newdate <- mindate + lubridate::weeks(val + 1)
  } else if(tolower(TimeUnit.) %chin% c("month","months")) {
    newdate <- mindate %m+% months(val + 1)
  } else if(tolower(TimeUnit.) %chin% c("quarter","quarters")) {
    newdate <- mindate %m+% months(val + 1)
  } else if(tolower(TimeUnit.) %chin% c("years","year")) {
    newdate <- mindate + lubridate::years(val + 1)
  }
  data. <- data.[get(DateColumnName.) >= eval(newdate)]
  return(data.)
}

#' @title CARMA_Get_IndepentVariablesPass
#'
#' CARMA_Get_IndepentVariablesPass is to help manage carma code
#'
#' @author Adrian Antico
#' @family Carma Helper
#' @param HierarchGroups Supply HierarchGroups
#' @noRd
CARMA_Get_IndepentVariablesPass <- function(HierarchGroups) {

  # Determine how to define IndependentVariablePass based on HierarchGroups
  if(!is.null(HierarchGroups)) {
    for(zzz in seq_len(length(HierarchGroups))) {
      if(zzz == 1L) {
        IndepentVariablesPass <- HierarchGroups[zzz]
      } else {
        IndepentVariablesPass <- paste(IndepentVariablesPass, HierarchGroups[zzz], sep = "_")
      }
    }
  } else {
    IndepentVariablesPass <- "GroupVar"
  }

  # Return
  return(IndepentVariablesPass)
}

#' @param SplitRatios Passthrough
#' @param TrainOnFull. Passthrough
#' @param data. Passthrough
#' @param NumSets. Passthrough
#' @param PartitionType. Passthrough
#' @param GroupVariables. Passthrough
#' @param DateColumnName. Passthrough
#'
#' @noRd
CarmaPartition <- function(data. = data,
                           SplitRatios. = SplitRatios,
                           TrainOnFull. = TrainOnFull,
                           NumSets. = NumSets,
                           PartitionType. = PartitionType,
                           GroupVariables. = GroupVariables,
                           DateColumnName. = DateColumnName) {

  # Data Wrangling: Partition data with AutoDataPartition
  if(!is.null(SplitRatios.) || !TrainOnFull.) {
    DataSets <- AutoDataPartition(
      data = data.,
      NumDataSets = NumSets.,
      Ratios = SplitRatios.,
      PartitionType = PartitionType.,
      StratifyColumnNames = if(!is.null(GroupVariables.)) "GroupVar" else NULL,
      TimeColumnName = eval(DateColumnName.))

    # Remove ID Column
    if("ID" %chin% names(data.)) data.table::set(data., j = "ID", value = NULL)
  }

  # Variables for CARMA function: Define data sets
  if(!is.null(SplitRatios.) || !TrainOnFull.) {
    train <- DataSets$TrainData
    valid <- DataSets$ValidationData
    if(NumSets. == 2L) test  <- NULL else test <- DataSets$TestData
    rm(DataSets)
  } else {
    train <- data.
    valid <- NULL
    test  <- NULL
  }
  return(list(train = train, valid = valid, test = test, data = data.))
}

#' @param data. Passthrough
#' @param train. Passthrough
#' @param XREGS. Passthrough
#' @param Difference. Passthrough
#' @param TargetColumnName. Passthrough
#' @param DateColumnName. Passthrough
#' @param GroupVariables. Passthrough
#'
#' @noRd
CarmaFeatures <- function(data. = data,
                          train. = train,
                          XREGS. = XREGS,
                          Difference. = Difference,
                          TargetColumnName. = TargetColumnName,
                          DateColumnName. = DateColumnName,
                          GroupVariables. = GroupVariables) {
  if(!Difference. || is.null(GroupVariables.)) {
    if(!is.null(XREGS.)) ModelFeatures <- setdiff(names(data.), c(eval(TargetColumnName.), eval(DateColumnName.))) else ModelFeatures <- setdiff(names(train.), c(eval(TargetColumnName.), eval(DateColumnName.)))
    TargetVariable <- eval(TargetColumnName.)
  } else if(Difference. && !is.null(GroupVariables.)) {
    ModelFeatures <- setdiff(names(train.), c(eval(TargetColumnName.), "ModTarget", "TargetDiffMidStep", eval(DateColumnName.)))
    TargetVariable <- "ModTarget"
  } else {
    ModelFeatures <- setdiff(names(train.), c(eval(TargetColumnName.), eval(DateColumnName.)))
    TargetVariable <- eval(TargetColumnName.)
  }
  return(list(ModelFeatures = ModelFeatures, TargetVariable = TargetVariable))
}

#' @param Type 'catboost', 'xgboost', 'h2o'
#' @param i. Passthrough
#' @param N. Passthrough
#' @param GroupVariables. Passthrough
#' @param Difference. Passthrough
#' @param TargetColumnName. Passthrough
#' @param Step1SCore. Passthrough
#' @param ModelFeatures. Passthrough
#' @param Model. Passthrough
#' @param DateColumnName. Passthrough
#' @param FutureDateData. Passthrough
#' @param NonNegativePred. Passthrough
#' @param RoundPreds. Passthrough
#' @param HierarchGroups. Passthrough
#' @param UpdateData. Passthrough
#' @param FactorList. Passthrough
#' @param EncodingMethod. Passthrough
#'
#' @noRd
CarmaScore <- function(Type = "catboost",
                       i. = i,
                       N. = N,
                       GroupVariables. = GroupVariables,
                       HierarchGroups. = HierarchGroups,
                       DateColumnName. = DateColumnName,
                       ModelFeatures. = ModelFeatures,
                       Difference. = Difference,
                       TargetColumnName. = TargetColumnName,
                       Step1SCore. = Step1SCore,
                       Model. = Model,
                       FutureDateData. = FutureDateData,
                       NonNegativePred. = NonNegativePred,
                       RoundPreds. = RoundPreds,
                       UpdateData. = UpdateData,
                       FactorList. = NULL,
                       EncodingMethod. = NULL) {

  # Row counts
  if(i. != 1) N. <- as.integer(N. + 1L)

  # Machine Learning: Generate predictions
  if(i. == 1L) {

    # Score model
    if(!is.null(GroupVariables.)) {

      # Define IDcols
      if(Difference.) IDcols <- c("ModTarget","TargetDiffMidStep") else IDcols <- eval(TargetColumnName.)
      IDcols <- IDcols[IDcols %chin% names(Step1SCore.)]

      # Score Model With Group Variables
      if(Type == "catboost") {
        Preds <- AutoCatBoostScoring(
          TargetType = "regression",
          ScoringData = Step1SCore.,
          FeatureColumnNames = ModelFeatures.,
          FactorLevelsList = NULL,
          IDcols = IDcols,
          OneHot = FALSE,
          ModelObject = Model.,
          ModelPath = getwd(),
          ReturnShapValues = FALSE,
          MultiClassTargetLevels = NULL,
          RemoveModel = FALSE,
          ModelID = "ModelTest",
          ReturnFeatures = TRUE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TransformationObject = NULL,
          TransID = NULL,
          TransPath = NULL,
          TargetColumnName = NULL,
          MDP_Impute = FALSE,
          MDP_CharToFactor = FALSE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)
      } else if(Type == "xgboost") {
        Preds <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = Step1SCore.,
          FeatureColumnNames = ModelFeatures.,
          IDcols = IDcols,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          EncodingMethod = EncodingMethod.,
          ReturnFeatures = TRUE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = NULL,
          TransformationObject = NULL,
          FactorLevelsList = FactorList.,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)
      }

    } else {

      # Define IDcols
      IDcols <- eval(TargetColumnName.)
      IDcols <- IDcols[IDcols %chin% names(Step1SCore.)]

      # Score Model No Group Variables
      if(Type == "catboost") {
        Preds <- AutoCatBoostScoring(
          TargetType = "regression",
          ScoringData = Step1SCore.,
          FeatureColumnNames = ModelFeatures.,
          FactorLevelsList = NULL,
          IDcols = IDcols,
          OneHot = FALSE,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          ReturnFeatures = TRUE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TransformationObject = NULL,
          TransID = NULL,
          TransPath = NULL,
          TargetColumnName = NULL,
          MDP_Impute = FALSE,
          MDP_CharToFactor = FALSE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)
      } else if(Type == "xgboost") {
        Preds <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = Step1SCore.,
          FeatureColumnNames = ModelFeatures.,
          OneHot = FALSE,
          IDcols = IDcols,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          EncodingMethod = EncodingMethod.,
          ReturnFeatures = TRUE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = NULL,
          TransformationObject = NULL,
          FactorLevelsList = FactorList.,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)
      }
    }

    # Data Wrangline: grab historical data and one more future record
    if(Difference.) {
      #if(eval(TargetColumnName.) %chin% names(Step1SCore.)) if(eval(TargetColumnName.) %chin% names(Preds)) data.table::set(Preds, j = eval(TargetColumnName.), value = NULL)
      if(eval(DateColumnName.) %chin% names(Preds)) data.table::set(Preds, j = eval(DateColumnName.), value = NULL)
      UpdateData. <- cbind(FutureDateData. = FutureDateData.[seq_len(N.)], Preds)
    } else {
      if(NonNegativePred.) Preds[, Predictions := data.table::fifelse(Predictions < 0.5, 0, Predictions)]
      if(RoundPreds.) Preds[, Predictions := round(Predictions)]
      UpdateData. <- cbind(FutureDateData. = FutureDateData.[seq_len(N.)], Preds)
    }

    # Update names
    data.table::setnames(UpdateData., "FutureDateData.", eval(DateColumnName.))

  } else {
    if(!is.null(GroupVariables.)) {

      # Modify target reference
      if(Difference.) IDcols = "ModTarget" else IDcols <- eval(TargetColumnName.)

      # GroupVar or Hierarchical
      if(!is.null(HierarchGroups.)) {
        temp <- data.table::copy(UpdateData.[, ID := seq_len(.N), by = c(eval(GroupVariables.))])
        temp <- temp[ID == N.][, ID := NULL]
      } else {
        temp <- data.table::copy(UpdateData.[, ID := seq_len(.N), by = "GroupVar"])
        temp <- temp[ID == N.][, ID := NULL]
      }

      # Score model
      if(Type == "catboost") {
        Preds <- AutoCatBoostScoring(
          TargetType = "regression",
          ScoringData = temp,
          FeatureColumnNames = ModelFeatures.,
          FactorLevelsList = NULL,
          IDcols = IDcols,
          OneHot = FALSE,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          ReturnFeatures = FALSE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = NULL,
          TransformationObject = NULL,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = FALSE,
          MDP_CharToFactor = FALSE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)
      } else if(Type == "xgboost") {
        Preds <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = temp,
          FeatureColumnNames = ModelFeatures.,
          OneHot = FALSE,
          IDcols = IDcols,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          EncodingMethod = EncodingMethod.,
          ReturnFeatures = FALSE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = eval(TargetColumnName.),
          TransformationObject = NULL,
          FactorLevelsList = FactorList.,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)
      }

      # Update data group case
      data.table::setnames(Preds, "Predictions", "Preds")
      if(NonNegativePred. && !Difference.) Preds[, Preds := data.table::fifelse(Preds < 0.5, 0, Preds)]
      Preds <- cbind(UpdateData.[ID == N.], Preds)
      if(Difference.) Preds[, ModTarget := Preds][, eval(TargetColumnName.) := Preds] else Preds[, eval(TargetColumnName.) := Preds]
      Preds[, Predictions := Preds][, Preds := NULL]
      if(RoundPreds.) Preds[, Predictions := round(Predictions)]
      UpdateData. <- UpdateData.[ID != N.]
      if(any(class(UpdateData.[[eval(DateColumnName.)]]) %chin% c("POSIXct","POSIXt")) && any(class(Preds[[eval(DateColumnName.)]]) == "Date")) UpdateData.[, eval(DateColumnName.) := as.Date(get(DateColumnName.))]
      UpdateData. <- data.table::rbindlist(list(UpdateData., Preds))
      if(Difference.) UpdateData.[ID %in% c(N.-1, N.), eval(TargetColumnName.) := cumsum(get(TargetColumnName.)), by = "GroupVar"]
      UpdateData.[, ID := NULL]

    } else {

      # Score Model
      if(Type == "catboost") {
        Preds <- AutoCatBoostScoring(
          TargetType = "regression",
          ScoringData = UpdateData.[.N],
          FeatureColumnNames = ModelFeatures.,
          FactorLevelsList = NULL,
          IDcols = NULL,
          OneHot = FALSE,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          ReturnFeatures = FALSE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = eval(TargetColumnName.),
          TransformationObject = NULL,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = FALSE,
          MDP_CharToFactor = FALSE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)
      } else if(Type == "xgboost") {
        Preds <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = UpdateData.[.N, ],
          FeatureColumnNames = ModelFeatures.,
          OneHot = FALSE,
          IDcols = NULL,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          EncodingMethod = NULL,
          ReturnFeatures = FALSE,
          TransformNumeric = FALSE,
          BackTransNumeric = FALSE,
          TargetColumnName = eval(TargetColumnName.),
          TransformationObject = NULL,
          FactorLevelsList = NULL,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)
      }

      # Update data non-group case
      if(NonNegativePred.) Preds[, Predictions := data.table::fifelse(Predictions < 0.5, 0, Predictions)]
      if(RoundPreds.) Preds[, Predictions := round(Predictions)]
      data.table::set(UpdateData., i = N., j = c("Predictions",TargetColumnName.), value = Preds[[1L]])
    }
  }

  # Return
  return(list(UpdateData = UpdateData., Preds = Preds, N = N.))
}

#' @param UpdateData. Passthrough
#' @param TimeUnit. Passthrough
#' @param DateColumnName. Passthrough
#'
#' @noRd
NextTimePeriod <- function(UpdateData. = UpdateData,
                           TimeUnit. = TimeUnit,
                           DateColumnName. = DateColumnName) {
  d <- max(UpdateData.[[eval(DateColumnName.)]])
  if(tolower(TimeUnit.) %chin% c("hour","hours")) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::hours(1))
  } else if(tolower(TimeUnit.) %chin% c("1min","1mins","1minute","1minutes")) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(1))
  } else if(tolower(TimeUnit.) %chin% c("5min","5mins","5minute","5minutes")) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(5))
  } else if(tolower(TimeUnit.) %chin% c("10min","10mins","10minute","10minutes")) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(10))
  } else if(tolower(TimeUnit.) %chin% c("15min","15mins","15minute","15minutes")) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(15))
  } else if(tolower(TimeUnit.) %chin% c("30min","30mins","30minute","30minutes")) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(30))
  } else if(tolower(TimeUnit.) %chin% c("day","days")) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::days(1))
  } else if(tolower(TimeUnit.) %chin% c("week","weeks")) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::weeks(1))
  } else if(tolower(TimeUnit.) %chin% c("month","months")) {
    CalendarFeatures <- data.table::as.data.table(d %m+% months(1))
  } else if(tolower(TimeUnit.) %chin% c("quarter","quarters")) {
    CalendarFeatures <- data.table::as.data.table(d %m+% months(3))
  } else if(tolower(TimeUnit.) %chin% c("years","year")) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::years(1))
  }
  return(CalendarFeatures)
}

#' @param UpdateData. Passthrough
#' @param GroupVariables. Passthrough
#' @param CalendarFeatures. Passthrough
#' @param CalendarVariables. Passthrough
#' @param GroupVarVector. Passthrough
#' @param DateColumnName. Passthrough
#' @param XREGS. Passthrough
#' @param FourierTerms. Passthrough
#' @param FourierFC. Passthrough
#' @param TimeGroups. Passthrough
#' @param TimeTrendVariable. Passthrough
#' @param N. Passthrough
#' @param TargetColumnName. Passthrough
#' @param HolidayVariable. Passthrough
#' @param HolidayLookback. Passthrough
#' @param TimeUnit. Passthrough
#' @param AnomalyDetection. Passthrough
#' @param i. Passthrough
#'
#' @noRd
UpdateFeatures <- function(UpdateData. = UpdateData,
                           GroupVariables. = GroupVariables,
                           CalendarFeatures. = CalendarFeatures,
                           CalendarVariables. = CalendarVariables,
                           GroupVarVector. = GroupVarVector,
                           DateColumnName. = DateColumnName,
                           XREGS. = XREGS,
                           FourierTerms. = FourierTerms,
                           FourierFC. = NULL,
                           TimeGroups. = TimeGroups,
                           TimeTrendVariable. = TimeTrendVariable,
                           N. = N,
                           TargetColumnName. = TargetColumnName,
                           HolidayVariable. = HolidayVariable,
                           HolidayLookback. = HolidayLookback,
                           TimeUnit. = TimeUnit,
                           AnomalyDetection. = AnomalyDetection,
                           i. = i) {

  # Merge groups vars
  if(!is.null(GroupVariables.)) CalendarFeatures. <- cbind(unique(GroupVarVector.), CalendarFeatures.)

  # Update colname for date
  data.table::setnames(CalendarFeatures., names(CalendarFeatures.)[ncol(CalendarFeatures.)], eval(DateColumnName.))

  # Merge XREGS if not null
  if(!is.null(XREGS.)) {
    if(!is.null(GroupVariables.)) {
      CalendarFeatures. <- ModelDataPrep(data = CalendarFeatures., Impute = FALSE, CharToFactor = FALSE, FactorToChar = TRUE, IntToNumeric = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
      CalendarFeatures. <- merge(CalendarFeatures., XREGS., by = c("GroupVar", eval(DateColumnName.)), all = FALSE)
    } else {
      CalendarFeatures. <- merge(CalendarFeatures., XREGS., by = eval(DateColumnName.), all = FALSE)
    }
  }

  # Add fouier terms
  if(is.null(GroupVariables.) && FourierTerms. > 0) {
    CalendarFeatures. <- merge(CalendarFeatures., FourierFC., by = DateColumnName., all = FALSE)
  } else if(FourierTerms. > 0) {
    if(!is.null(FourierFC.)) {
      if(length(FourierFC.) != 0) CalendarFeatures. <- merge(CalendarFeatures., FourierFC., by = c("GroupVar",eval(DateColumnName.)), all = FALSE)
    }
  }

  # Prepare for more feature engineering
  if(!tolower(TimeGroups.[1L]) %chin% c("5min","10min","15min","30min","hour")) CalendarFeatures.[, eval(DateColumnName.) := data.table::as.IDate(get(DateColumnName.))]

  # Update calendar variables
  if(!is.null(CalendarVariables.)) {
    CalendarFeatures. <- CreateCalendarVariables(
      data = CalendarFeatures.,
      DateCols = eval(DateColumnName.),
      AsFactor = FALSE,
      TimeUnits = CalendarVariables.)
  }

  # Update Time Trend feature
  if(TimeTrendVariable.) CalendarFeatures.[, TimeTrend := eval(N.) + 1]

  # Prepare data for scoring
  for(zz in seq_len(length(TargetColumnName.))) {
    if(zz == 1) temp <- cbind(CalendarFeatures., 1) else temp <- cbind(temp, 1)
    data.table::setnames(temp, "V2", c(eval(TargetColumnName.[zz])))
  }
  if(any(class(UpdateData.[[eval(DateColumnName.)]]) %chin% c("POSIXct","POSIXt","IDate"))) UpdateData.[, eval(DateColumnName.) := as.Date(get(DateColumnName.))]
  if(any(class(temp[[eval(DateColumnName.)]]) %chin% c("POSIXct","POSIXt","IDate"))) temp[, eval(DateColumnName.) := as.Date(get(DateColumnName.))]
  UpdateData. <- data.table::rbindlist(list(UpdateData., temp), fill = TRUE)

  # Update holiday feature
  if(!is.null(HolidayVariable.)) {
    UpdateData. <- CreateHolidayVariables(
      UpdateData.,
      DateCols = eval(DateColumnName.),
      LookbackDays = if(!is.null(HolidayLookback.)) HolidayLookback. else LB(TimeUnit.),
      HolidayGroups = HolidayVariable.,
      Holidays = NULL)
  }

  # Update Anomaly Detection
  if(i. > 1 && !is.null(AnomalyDetection.)) {
    UpdateData.[, ":=" (AnomHigh = 0, AnomLow = 0)]
  }

  # Return
  return(UpdateData = UpdateData.)
}

#' @title CarmaTimeSeriesScorePrep
#'
#' @description CarmaTimeSeriesScorePrep is to help manage carma code
#'
#' @author Adrian Antico
#' @family Carma Helper
#'
#' @param UpdateData.. Passthrough
#' @param GroupVariables.. Passthrough
#' @param DateColumnName.. Passthrough
#'
#' @noRd
CarmaTimeSeriesScorePrep <- function(UpdateData.. = UpdateData.,
                                     GroupVariables.. = GroupVariables.,
                                     DateColumnName.. = DateColumnName.) {
  if(any(is.na(UpdateData..[["Predictions"]]))) {
    data.table::set(x = UpdateData.., i = which(is.na(UpdateData..[["Predictions"]])), j = "Predictions", value = 1.0)
  }
  if("ModTarget" %chin% names(UpdateData..) && any(is.na(UpdateData..[["ModTarget"]]))) {
    data.table::set(x = UpdateData.., i = which(is.na(UpdateData..[["ModTarget"]])), j = "ModTarget", value = 1.0)
  }
  if(!is.null(GroupVariables..)) {
    if(!"GroupVar" %chin% names(UpdateData..)) {
      data.table::setorderv(x = UpdateData.., cols = c(eval(GroupVariables..), eval(DateColumnName..)))
      UpdateData..[, ID := .N:1, by = c(eval(GroupVariables..))]
    } else {
      data.table::setorderv(x = UpdateData.., cols = c("GroupVar", eval(DateColumnName..)))
      UpdateData..[, ID := .N:1, by = "GroupVar"]
    }
  } else {
    data.table::setorderv(x = UpdateData.., cols = c(eval(DateColumnName..)))
    UpdateData..[, ID := .N:1]
  }
  keep <- setdiff(names(UpdateData..), names(UpdateData..)[grep(pattern = "LAG", x = names(UpdateData..))])
  UpdateData.. <- UpdateData..[, ..keep]
  return(list(data = UpdateData.., keep = keep))
}

#' @param data. Passthrough
#' @param TargetColumnName. Passthrough
#' @param DateColumnName. Passthrough
#' @param GroupVariables. Passthrough
#' @param HierarchGroups. Passthrough
#' @param Difference. Passthrough
#' @param TimeGroups. Passthrough
#' @param TimeUnit. Passthrough
#' @param MA_Periods. Passthrough
#' @param SD_Periods. Passthrough
#' @param Skew_Periods. Passthrough
#' @param Kurt_Periods. Passthrough
#' @param Quantile_Periods. Passthrough
#' @param Quantiles_Selected. Passthrough
#' @param HolidayVariable. Passthrough
#' @param HolidayLags. Passthrough
#' @param HolidayMovingAverages. Passthrough
#' @param DebugMode. Passthrough
#'
#' @noRd
CarmaTimeSeriesFeatures <- function(data. = data,
                                    TargetColumnName. = TargetColumnName,
                                    DateColumnName. = DateColumnName,
                                    GroupVariables. = GroupVariables,
                                    HierarchGroups. = HierarchGroups,
                                    Difference. = Difference,
                                    TimeGroups. = TimeGroups,
                                    TimeUnit. = TimeUnit,
                                    Lags. = Lags,
                                    MA_Periods. = MA_Periods,
                                    SD_Periods. = SD_Periods,
                                    Skew_Periods. = Skew_Periods,
                                    Kurt_Periods. = Kurt_Periods,
                                    Quantile_Periods. = Quantile_Periods,
                                    Quantiles_Selected. = Quantiles_Selected,
                                    HolidayVariable. = HolidayVariable,
                                    HolidayLags. = HolidayLags,
                                    HolidayMovingAverages. = HolidayMovingAverages,
                                    DebugMode. = DebugMode) {

  # Feature Engineering: Add GDL Features based on the TargetColumnName
  if(!is.null(Lags.)) {

    # Group and No Differencing
    if(!is.null(GroupVariables.) && !Difference.) {

      # Split GroupVar and Define HierarchyGroups and IndependentGroups
      Output <- CARMA_GroupHierarchyCheck(data = data., Group_Variables = GroupVariables., HierarchyGroups = HierarchGroups.)
      data. <- Output$data
      HierarchSupplyValue <- Output$HierarchSupplyValue
      IndependentSupplyValue <- Output$IndependentSupplyValue
      if(is.list(Lags.)) TimeGroups. <- names(Lags.)

      # Generate features----
      data. <- AutoLagRollStats(

        # Data
        data                 = data.,
        DateColumn           = eval(DateColumnName.),
        Targets              = eval(TargetColumnName.),
        HierarchyGroups      = HierarchSupplyValue,
        IndependentGroups    = IndependentSupplyValue,

        # Services
        TimeBetween          = NULL,
        TimeUnit             = TimeUnit.,
        TimeUnitAgg          = TimeUnit.,
        TimeGroups           = TimeGroups.,
        RollOnLag1           = TRUE,
        Type                 = "Lag",
        SimpleImpute         = TRUE,

        # Calculated Columns
        Lags                  = Lags.,
        MA_RollWindows        = MA_Periods.,
        SD_RollWindows        = SD_Periods.,
        Skew_RollWindows      = Skew_Periods.,
        Kurt_RollWindows      = Kurt_Periods.,
        Quantile_RollWindows  = Quantile_Periods.,
        Quantiles_Selected    = Quantiles_Selected.)

      # Keep interaction group as GroupVar
      if(length(GroupVariables.) > 1) {
        if(!"GroupVar" %chin% names(data.)) data.[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = c(GroupVariables.)]
        if(!is.null(HierarchGroups.)) Categoricals <- FullFactorialCatFeatures(GroupVars = HierarchGroups., BottomsUp = TRUE) else Categoricals <- NULL
        if(!is.null(HierarchGroups.)) GroupVarVector <- unique(data.[, .SD, .SDcols = c(Categoricals,"GroupVar")]) else GroupVarVector <- unique(data.[, .SD, .SDcols = c("GroupVar")])
      } else {
        if(!"GroupVar" %chin% names(data.)) data.[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables.]
        GroupVarVector <- unique(data.[, .SD, .SDcols = c("GroupVar")])
      }
    }

    # Group and No Differencing
    if(!is.null(GroupVariables.) && Difference.) {

      # Split GroupVar and Define HierarchyGroups and IndependentGroups
      Output <- CARMA_GroupHierarchyCheck(data = data., Group_Variables = GroupVariables., HierarchyGroups = HierarchGroups.)
      data. <- Output$data
      HierarchSupplyValue <- Output$HierarchSupplyValue
      IndependentSupplyValue <- Output$IndependentSupplyValue
      if(is.list(Lags.)) TimeGroups. <- names(Lags.)

      # Generate features
      data. <- AutoLagRollStats(

        # Data
        data                 = data.,
        DateColumn           = eval(DateColumnName.),
        Targets              = c("ModTarget"),
        HierarchyGroups      = HierarchSupplyValue,
        IndependentGroups    = IndependentSupplyValue,

        # Services
        TimeBetween          = NULL,
        TimeUnit             = TimeUnit.,
        TimeUnitAgg          = TimeUnit.,
        TimeGroups           = TimeGroups.,
        RollOnLag1           = TRUE,
        Type                 = "Lag",
        SimpleImpute         = TRUE,

        # Calculated Columns
        Lags                  = Lags.,
        MA_RollWindows        = MA_Periods.,
        SD_RollWindows        = SD_Periods.,
        Skew_RollWindows      = Skew_Periods.,
        Kurt_RollWindows      = Kurt_Periods.,
        Quantile_RollWindows  = Quantile_Periods.,
        Quantiles_Selected    = Quantiles_Selected.,
        Debug                 = DebugMode.)

      # Keep interaction group as GroupVar
      if(length(GroupVariables.) > 1L) {
        if(!"GroupVar" %chin% names(data.)) data.[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables.]
        if(!is.null(HierarchGroups.)) Categoricals <- FullFactorialCatFeatures(GroupVars = HierarchGroups., BottomsUp = TRUE) else Categoricals <- NULL
        if(!is.null(HierarchGroups.)) GroupVarVector <- data.[, .SD, .SDcols = c(Categoricals,"GroupVar")] else GroupVarVector <- unique(data.[, .SD, .SDcols = c("GroupVar")])
      } else {
        if(!"GroupVar" %chin% names(data.)) data.[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables.]
        GroupVarVector <- unique(data.[, .SD, .SDcols = c("GroupVar")])
      }
    }

    # No Group with or without Diff
    if(is.null(GroupVariables.)) {

      # TimeGroups----
      if(is.list(Lags.)) TimeGroups. <- names(Lags.)

      # Generate features
      data. <- AutoLagRollStats(

        # Data
        data                 = data.,
        DateColumn           = eval(DateColumnName.),
        Targets              = eval(TargetColumnName.),
        HierarchyGroups      = NULL,
        IndependentGroups    = NULL,

        # Services
        TimeBetween          = NULL,
        TimeUnit             = TimeUnit.,
        TimeUnitAgg          = TimeUnit.,
        TimeGroups           = TimeGroups.,
        RollOnLag1           = TRUE,
        Type                 = "Lag",
        SimpleImpute         = TRUE,

        # Calculated Columns
        Lags                  = Lags.,
        MA_RollWindows        = MA_Periods.,
        SD_RollWindows        = SD_Periods.,
        Skew_RollWindows      = Skew_Periods.,
        Kurt_RollWindows      = Kurt_Periods.,
        Quantile_RollWindows  = Quantile_Periods.,
        Quantiles_Selected    = Quantiles_Selected.,
        Debug                 = DebugMode.)
    }
  } else {
    Output <- CARMA_GroupHierarchyCheck(data = data., Group_Variables = GroupVariables., HierarchyGroups = HierarchGroups.)
    data. <- Output$data
    HierarchSupplyValue <- Output$HierarchSupplyValue
    IndependentSupplyValue <- Output$IndependentSupplyValue
    GroupVarVector <- NULL
    Categoricals <- NULL
  }

  # Feature Engineering: Add Lag / Lead, MA Holiday Variables----
  if(!is.null(HolidayVariable.) && max(HolidayLags.) > 0 && max(HolidayMovingAverages.) > 0) {
    if(!is.null(GroupVariables.)) {

      # Build Features----
      data. <- DT_GDL_Feature_Engineering(
        data            = data.,
        lags            = HolidayLags.,
        periods         = HolidayMovingAverages.[!HolidayMovingAverages. %in% 1],
        SDperiods       = 0,
        Skewperiods     = 0,
        Kurtperiods     = 0,
        Quantileperiods = 0,
        statsFUNs       = "mean",
        targets         = "HolidayCounts",
        groupingVars    = IndependentSupplyValue,
        sortDateName    = DateColumnName.,
        timeDiffTarget  = NULL,
        timeAgg         = TimeGroups.[1L],
        WindowingLag    = 1,
        Type            = "Lag",
        SimpleImpute    = TRUE)

    } else {

      data. <- DT_GDL_Feature_Engineering(
        data            = data.,
        lags            = HolidayLags.,
        periods         = HolidayMovingAverages.[!HolidayMovingAverages. %in% 1],
        SDperiods       = 0,
        Skewperiods     = 0,
        Kurtperiods     = 0,
        Quantileperiods = 0,
        statsFUNs       = "mean",
        targets         = "HolidayCounts",
        groupingVars    = NULL,
        sortDateName    = DateColumnName.,
        timeDiffTarget  = NULL,
        timeAgg         = TimeGroups.[1L],
        WindowingLag    = 1,
        Type            = "Lag",
        SimpleImpute    = TRUE)
    }
  }

  # Return
  return(list(
    data = data.,
    HierarchSupplyValue = if(!exists("HierarchSupplyValue")) NULL else HierarchSupplyValue,
    IndependentSupplyValue = if(!exists("IndependentSupplyValue")) NULL else IndependentSupplyValue,
    GroupVarVector = if(!exists("GroupVarVector")) NULL else GroupVarVector,
    Categoricals = if(!exists("Categoricals")) NULL else Categoricals))
}

#' @param ModelType 'catboost', 'xgboost', 'h2o'
#' @param DebugMode. Passthrough
#' @param UpdateData. Passthrough
#' @param GroupVariables. Passthrough
#' @param Difference. Passthrough
#' @param CalendarVariables. Passthrough
#' @param HolidayVariable. Passthrough
#' @param IndepVarPassTRUE. Passthrough
#' @param data. Passthrough
#' @param CalendarFeatures. Passthrough
#' @param XREGS. Passthrough
#' @param HierarchGroups. Passthrough
#' @param GroupVarVector. Passthrough
#' @param TargetColumnName. Passthrough
#' @param DateColumnName. Passthrough
#' @param Preds. Passthrough
#' @param HierarchSupplyValue. Passthrough
#' @param IndependentSupplyValue. Passthrough
#' @param TimeUnit. Passthrough
#' @param TimeGroups. Passthrough
#' @param Lags. Passthrough
#' @param MA_Periods. Passthrough
#' @param SD_Periods. Passthrough
#' @param Skew_Periods. Passthrough
#' @param Kurt_Periods. Passthrough
#' @param Quantile_Periods. Passthrough
#' @param Quantiles_Selected. Passthrough
#' @param HolidayLags. Passthrough
#' @param HolidayMovingAverages. Passthrough
#'
#' @noRd
CarmaRollingStatsUpdate <- function(ModelType = "catboost",
                                    DebugMode. = FALSE,
                                    UpdateData. = UpdateData,
                                    GroupVariables. = GroupVariables,
                                    Difference. = Difference,
                                    CalendarVariables. = CalendarVariables,
                                    HolidayVariable. = HolidayVariable,
                                    IndepVarPassTRUE. = IndepentVariablesPass,
                                    data. = data,
                                    CalendarFeatures. = CalendarFeatures,
                                    XREGS. = XREGS,
                                    HierarchGroups. = HierarchGroups,
                                    GroupVarVector. = GroupVarVector,
                                    TargetColumnName. = TargetColumnName,
                                    DateColumnName. = DateColumnName,
                                    Preds. = Preds,
                                    HierarchSupplyValue. = HierarchSupplyValue,
                                    IndependentSupplyValue. = IndependentSupplyValue,
                                    TimeUnit. = TimeUnit,
                                    TimeGroups. = TimeGroups[1],
                                    Lags. = Lags,
                                    MA_Periods. = MA_Periods,
                                    SD_Periods. = SD_Periods,
                                    Skew_Periods. = Skew_Periods,
                                    Kurt_Periods. = Kurt_Periods,
                                    Quantile_Periods. = Quantile_Periods,
                                    Quantiles_Selected. = Quantiles_Selected,
                                    HolidayLags. = HolidayLags,
                                    HolidayMovingAverages. = HolidayMovingAverages) {

  # Group with or No Diff
  if(!is.null(Lags.) && !is.null(GroupVariables.) && Difference.) {

    # Calendar and Holiday----
    if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
    if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE

    # Create data for GDL
    temp <- CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
    Temporary <- temp$data
    keep <- temp$keep; rm(temp)

    # Build Features
    Temporary <- AutoLagRollStatsScoring(

      # Data
      data                 = Temporary,
      RowNumsID            = "ID",
      RowNumsKeep          = 1,
      DateColumn           = eval(DateColumnName.),
      Targets              = "ModTarget",
      HierarchyGroups      = HierarchSupplyValue.,
      IndependentGroups    = IndependentSupplyValue.,

      # Services
      TimeBetween          = NULL,
      TimeUnit             = TimeUnit.,
      TimeUnitAgg          = TimeGroups.[1L],
      TimeGroups           = TimeGroups.,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = TRUE,

      # Calculated Columns
      Lags                 = Lags.,
      MA_RollWindows       = MA_Periods.,
      SD_RollWindows       = SD_Periods.,
      Skew_RollWindows     = Skew_Periods.,
      Kurt_RollWindows     = Kurt_Periods.,
      Quantile_RollWindows = Quantile_Periods.,
      Quantiles_Selected   = Quantiles_Selected.,
      Debug                = DebugMode.)

    # Lag / Lead, MA Holiday Variables
    if(!is.null(HolidayVariable.) && max(HolidayLags.) > 0 && max(HolidayMovingAverages.) > 0) {

      # Calendar and Holiday----
      if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
      if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE

      # Create copy of data
      temp <- CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
      Temporary1 <- temp$data
      keep <- temp$keep; rm(temp)

      # Generate GDL Features for Updated Records
      IndepVarPassTRUE. <- CARMA_Get_IndepentVariablesPass(HierarchGroups.)

      # Generate GDL Features for Updated Records
      Temporary1 <- AutoLagRollStatsScoring(

        # Data
        data                 = Temporary1,
        RowNumsID            = "ID",
        RowNumsKeep          = 1,
        DateColumn           = eval(DateColumnName.),
        Targets              = "HolidayCounts",
        HierarchyGroups      = NULL,
        IndependentGroups    = IndepVarPassTRUE.,

        # Services
        TimeUnit              = TimeUnit.,
        TimeUnitAgg          = TimeGroups.[1L],
        TimeGroups            = TimeGroups.[1L],
        TimeBetween          = NULL,
        RollOnLag1           = TRUE,
        Type                 = "Lag",
        SimpleImpute         = TRUE,

        # Calculated Columns
        Lags                 = HolidayLags.,
        MA_RollWindows       = HolidayMovingAverages.,
        SD_RollWindows       = NULL,
        Skew_RollWindows     = NULL,
        Kurt_RollWindows     = NULL,
        Quantile_RollWindows = NULL,
        Quantiles_Selected   = NULL,
        Debug                = DebugMode.)

      # Join Holiday Lags. and Moving Averages back to UpdateData.
      if(!"GroupVar" %chin% names(Temporary)) {
        keep <- c(eval(GroupVariables.),eval(DateColumnName.), setdiff(names(Temporary1), names(Temporary)))
        Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c(eval(GroupVariables.), eval(DateColumnName.)), all = FALSE)
      } else {
        keep <- c("GroupVar",eval(DateColumnName.), setdiff(names(Temporary1), names(Temporary)))
        Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c("GroupVar", eval(DateColumnName.)), all = FALSE)
      }
    }

    # Update data for scoring next iteration
    if(ModelType %chin% c("catboost","h2o")) {
      UpdateData. <- data.table::rbindlist(list(UpdateData.[ID != 1][, ID := NULL], Temporary), fill = TRUE, use.names = TRUE)
    } else if(ModelType %chin% c("xgboost")) {
      UpdateData. <- data.table::rbindlist(list(UpdateData.[ID != 1][, ID := NULL], Temporary), fill = TRUE, use.names = TRUE)
    }

  } else if(!is.null(GroupVariables.) && Difference.) {

    # Calendar and Holiday
    if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
    if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE
    CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
  }

  # Group and Diff
  if(!is.null(Lags.) && !is.null(GroupVariables.) && !Difference.) {

    # Calendar and Holiday
    if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
    if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE

    # Create data for GDL
    temp <- CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
    Temporary <- temp$data
    keep <- temp$keep; rm(temp)

    # Build Features
    Temporary <- AutoLagRollStatsScoring(

      # Data
      data                 = Temporary,
      RowNumsID            = "ID",
      RowNumsKeep          = 1,
      DateColumn           = eval(DateColumnName.),
      Targets              = eval(TargetColumnName.),
      HierarchyGroups      = HierarchSupplyValue.,
      IndependentGroups    = IndependentSupplyValue.,

      # Services
      TimeBetween          = NULL,
      TimeUnit             = TimeUnit.,
      TimeUnitAgg          = TimeGroups.[1L],
      TimeGroups           = TimeGroups.,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = TRUE,

      # Calculated Columns
      Lags                 = Lags.,
      MA_RollWindows       = MA_Periods.,
      SD_RollWindows       = SD_Periods.,
      Skew_RollWindows     = Skew_Periods.,
      Kurt_RollWindows     = Kurt_Periods.,
      Quantile_RollWindows = Quantile_Periods.,
      Quantiles_Selected   = Quantiles_Selected.,
      Debug                = DebugMode.)

    # Lag / Lead, MA Holiday Variables
    if(!is.null(HolidayVariable.) && max(HolidayLags.) > 0 && max(HolidayMovingAverages.) > 0) {

      # Calendar and Holiday----
      if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
      if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE

      # Generate GDL Features for Updated Records
      IndepVarPassTRUE. <- CARMA_Get_IndepentVariablesPass(HierarchGroups.)

      # Create copy of data
      temp <- CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
      Temporary1 <- temp$data
      keep <- temp$keep; rm(temp)

      # Generate GDL Features for Updated Records
      Temporary1 <- AutoLagRollStatsScoring(

        # Data
        data                 = Temporary1,
        RowNumsID            = "ID",
        RowNumsKeep          = 1,
        DateColumn           = eval(DateColumnName.),
        Targets              = "HolidayCounts",
        HierarchyGroups      = NULL,
        IndependentGroups    = IndepVarPassTRUE.,

        # Services
        TimeUnit             = TimeUnit.,
        TimeUnitAgg          = TimeGroups.[1L],
        TimeGroups           = TimeGroups.[1L],
        RollOnLag1           = TRUE,
        Type                 = "Lag",
        SimpleImpute         = TRUE,

        # Calculated Columns
        Lags                 = HolidayLags.,
        MA_RollWindows       = HolidayMovingAverages.[!HolidayMovingAverages. %in% 1],
        SD_RollWindows       = NULL,
        Skew_RollWindows     = NULL,
        Kurt_RollWindows     = NULL,
        Quantile_RollWindows = NULL,
        Quantiles_Selected   = NULL,
        Debug                = DebugMode.)

      # Join Holiday Lags. and Moving Averages back to UpdateData.
      if(ModelType %chin% c("catboost","h2o")) {
        if(!"GroupVar" %chin% names(Temporary)) {
          keep <- c(eval(GroupVariables.),eval(DateColumnName.), setdiff(names(Temporary1), names(Temporary)))
          if(eval(DateColumnName.) %chin% names(Temporary))
            Temporary <- Temporary[, .SD, .SDcols = unique(names(Temporary))]
            Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c(eval(GroupVariables.), eval(DateColumnName.)), all = FALSE)
        } else {
          keep <- c("GroupVar",eval(DateColumnName.),setdiff(names(Temporary1), names(Temporary)))
          Temporary <- Temporary[, .SD, .SDcols = unique(names(Temporary))]
          Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c("GroupVar", eval(DateColumnName.)), all = FALSE)
        }
      } else if(ModelType %chin% c("xgboost")) {
        if(!"GroupVar" %chin% names(Temporary)) {
          keep <- c(eval(GroupVariables.),eval(DateColumnName.), setdiff(names(Temporary1), names(Temporary)))
          Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c(eval(GroupVariables.), eval(DateColumnName.)), all = FALSE)
        } else {
          keep <- c("GroupVar",eval(DateColumnName.),setdiff(names(Temporary1), names(Temporary)))
          Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c("GroupVar", eval(DateColumnName.)), all = FALSE)
        }
      }
    }

    # Update data for scoring next iteration
    UpdateData. <- data.table::rbindlist(list(UpdateData.[ID != 1][, ID := NULL], Temporary), fill = TRUE, use.names = TRUE)

  } else if(!is.null(GroupVariables.) && !Difference.) {

    # No lags
    if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
    if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE
    CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
  }

  # No Group with or without Diff
  if(!is.null(Lags.) && is.null(GroupVariables.)) {

    # Calendar and Holiday
    if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
    if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE

    # Create data for GDL
    temp <- CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
    Temporary <- temp$data
    keep <- temp$keep; rm(temp)
    if("GroupVar" %chin% keep) keep <- keep[!keep %chin% "GroupVar"]

    # Generate GDL Features for Updated Records
    Temporary <- AutoLagRollStatsScoring(

      # Data
      data                 = Temporary,
      RowNumsID            = "ID",
      RowNumsKeep          = 1,
      DateColumn           = eval(DateColumnName.),
      Targets              = eval(TargetColumnName.),
      HierarchyGroups      = NULL,
      IndependentGroups    = NULL,

      # Services
      TimeBetween          = NULL,
      TimeUnit             = TimeUnit.,
      TimeUnitAgg          = TimeGroups.[1L],
      TimeGroups           = TimeGroups.,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = TRUE,

      # Calculated Columns
      Lags                 = Lags.,
      MA_RollWindows       = MA_Periods.,
      SD_RollWindows       = SD_Periods.,
      Skew_RollWindows     = Skew_Periods.,
      Kurt_RollWindows     = Kurt_Periods.,
      Quantile_RollWindows = Quantile_Periods.,
      Quantiles_Selected   = Quantiles_Selected.,
      Debug                = DebugMode.)

    # Lag / Lead, MA Holiday Variables
    if(!is.null(HolidayVariable.) && max(HolidayLags.) > 0 && max(HolidayMovingAverages.) > 0) {

      # Calendar and Holiday
      if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
      if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE

      # Copy data
      temp <- CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
      Temporary1 <- temp$data
      keep <- temp$keep; rm(temp)

      # Generate GDL Features for Updated Records
      Temporary1 <- AutoLagRollStatsScoring(

        # Data
        data                 = Temporary1,
        RowNumsID            = "ID",
        RowNumsKeep          = 1,
        DateColumn           = eval(DateColumnName.),
        Targets              = "HolidayCounts",
        HierarchyGroups      = NULL,
        IndependentGroups    = NULL,

        # Services
        TimeUnit             = TimeUnit.,
        TimeUnitAgg          = TimeGroups.[1L],
        TimeGroups           = TimeGroups.[1L],
        TimeBetween          = NULL,
        RollOnLag1           = TRUE,
        Type                 = "Lag",
        SimpleImpute         = TRUE,

        # Calculated Columns
        Lags                 = HolidayLags.,
        MA_RollWindows       = HolidayMovingAverages.[!HolidayMovingAverages. %in% 1],
        SD_RollWindows       = NULL,
        Skew_RollWindows     = NULL,
        Kurt_RollWindows     = NULL,
        Quantile_RollWindows = NULL,
        Quantiles_Selected   = NULL,
        Debug                = DebugMode.)

      # Join Holiday Lags. and Moving Averages back to UpdateData.
      keep <- unique(c(eval(DateColumnName.), setdiff(names(Temporary1), names(Temporary))))
      Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c(eval(DateColumnName.)), all = FALSE)
    }

    # Update data for scoring next iteration
    if(ModelType %chin% c("catboost","h2o")) {
      if(!"ID" %chin% c(names(UpdateData.))) data.table::set(UpdateData., j = "ID", value = nrow(UpdateData.):1L)
      UpdateData. <- data.table::rbindlist(list(UpdateData.[ID > 1L][, ID := NULL], Temporary), fill = TRUE, use.names = TRUE)
    } else if(ModelType %chin% c("xgboost")) {
      if("ID" %chin% names(UpdateData)) {
        UpdateData <- data.table::rbindlist(list(UpdateData[ID != 1], Temporary), fill = TRUE, use.names = TRUE)
      } else {
        if(!is.null(GroupVariables)) {
          UpdateData <- data.table::rbindlist(list(UpdateData, Temporary), fill = TRUE, use.names = TRUE)
        } else {
          UpdateData[, ID := .N:1L]
          UpdateData <- data.table::rbindlist(list(UpdateData[ID != 1L][, ID := NULL], Temporary), fill = TRUE, use.names = TRUE)
        }
      }
    }

  } else if(is.null(GroupVariables.)) {

    # Calendar and Holiday
    if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
    if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE
    if(ModelType %chin% c("catboost","h2o")) {
      CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
      UpdateData. <- UpdateData.[ID > 1L][, ID := NULL]
    }
  }

  # Return
  return(UpdateData.)
}

#' @param UpdateData. Passthrough
#' @param FutureDateData. Passthrough
#' @param dataStart. Passthrough
#' @param DateColumnName. Passthrough
#' @param TargetColumnName. Passthrough
#' @param GroupVariables. Passthrough
#' @param Difference. Passthrough
#' @param TargetTransformation. Passthrough
#' @param TransformObject. Passthrough
#' @param DiffTrainOutput. Passthrough
#'
#' @noRd
CarmaReturnDataPrep <- function(UpdateData. = UpdateData,
                                FutureDateData. = FutureDateData,
                                dataStart. = dataStart,
                                DateColumnName. = DateColumnName,
                                TargetColumnName. = TargetColumnName,
                                GroupVariables. = GroupVariables,
                                Difference. = Difference,
                                TargetTransformation. = TargetTransformation,
                                TransformObject. = TransformObject,
                                NonNegativePred. = NonNegativePred,
                                DiffTrainOutput. = NULL) {

  # Remove duplicate columns
  if(sum(names(UpdateData.) %chin% eval(DateColumnName.)) > 1) data.table::set(UpdateData., j = which(names(UpdateData.) %chin% eval(DateColumnName.))[2L], value = NULL)
  if(sum(names(UpdateData.) %chin% eval(TargetColumnName.)) > length(TargetColumnName.)) {
    x <- which(names(UpdateData.) %chin% eval(TargetColumnName.))
    x <- x[(length(TargetColumnName.) + 1L):length(x)]
    data.table::set(UpdateData., j = x, value = NULL)
  }

  # Reverse Difference
  if(is.null(GroupVariables.) && Difference.) {
    UpdateData. <- DifferenceDataReverse(data = UpdateData., ScoreData = NULL, CARMA = TRUE, TargetCol = eval(TargetColumnName.), FirstRow = DiffTrainOutput.$FirstRow[[eval(TargetColumnName.)]], LastRow = NULL)
  } else if(!is.null(GroupVariables.) && Difference.) {
    if(any(class(UpdateData.[[eval(DateColumnName.)]]) %chin% c("POSIXct","POSIXt")) && any(class(dataStart.[[eval(DateColumnName.)]]) == "Date")) UpdateData.[, eval(DateColumnName.) := as.Date(get(DateColumnName.))]
    UpdateData. <- data.table::rbindlist(list(dataStart.,UpdateData.), fill = TRUE)
    UpdateData. <- UpdateData.[, .SD, .SDcols = c(eval(DateColumnName.),eval(TargetColumnName.),"Predictions","GroupVar")]
    data.table::set(UpdateData., j = "Predictions", value = UpdateData.[[eval(TargetColumnName.)]])
    if(NonNegativePred.) UpdateData.[, Predictions := data.table::fifelse(Predictions < 0.5, 0, Predictions)]
  }

  # BackTransform
  if(TargetTransformation.) {
    if(length(TargetColumnName.) == 1L) {
      temptrans <- data.table::copy(TransformObject.)
      data.table::set(TransformObject., i = 1L, j = "ColumnName", value = "Predictions")
      TransformObject. <- data.table::rbindlist(list(temptrans, TransformObject.))

      # Ensure positive values in case transformation method requires that
      if(Difference. && !is.null(GroupVariables.)) {
        UpdateData.[!get(DateColumnName.) %in% FutureDateData., eval(TargetColumnName.) := 1, by = "GroupVar"]
      }

      # Backtrans
      UpdateData. <- AutoTransformationScore(
        ScoringData = UpdateData.,
        FinalResults = TransformObject.,
        Type = "Inverse",
        TransID = NULL,
        Path = NULL)
    } else {
      for(zz in seq_along(TargetColumnName.)) {

        # One at a time
        temptrans <- data.table::copy(TransformObject.[[zz]])
        data.table::set(TransformObject.[[zz]], i = 1L, j = "ColumnName", value = paste0("Predictions.V",zz))
        TransformObjectTemp <- data.table::rbindlist(list(temptrans,TransformObject.[[zz]]))

        # Ensure positive values in case transformation method requires so----
        if(Difference.) {
          if(!is.null(GroupVariables.) && TrainOnFull.) {
            UpdateData[!get(DateColumnName.) %in% FutureDateData., eval(TargetColumnName.) := 1, by = "GroupVar"]
          } else if(TrainOnFull) {
            UpdateData[!get(DateColumnName) %in% FutureDateData., eval(TargetColumnName.) := 1]
          }
        }

        # Backtrans----
        UpdateData. <- AutoTransformationScore(
          ScoringData = UpdateData.,
          FinalResults = TransformObjectTemp,
          Type = "Inverse",
          TransID = NULL,
          Path = NULL)
      }
    }
  }

  # Remove target variables values on FC periods
  if(!is.null(GroupVariables.)) {
    UpdateData.[!get(DateColumnName.) %in% FutureDateData., eval(TargetColumnName.) := NA, by = "GroupVar"]
  } else {
    UpdateData.[!get(DateColumnName.) %in% FutureDateData., eval(TargetColumnName.) := NA]
  }

  # Return data prep
  if(!is.null(GroupVariables.)) {
    keep <- c("GroupVar", eval(DateColumnName.), eval(TargetColumnName.), names(UpdateData.)[which(names(UpdateData.) %like% "Predictions")])
    UpdateData. <- UpdateData.[, ..keep]
    if(length(GroupVariables.) > 1L) {
      UpdateData.[, eval(GroupVariables.) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
    } else {
      UpdateData.[, eval(GroupVariables.) := GroupVar][, GroupVar := NULL]
    }
  }

  # Return data
  return(list(UpdateData = UpdateData., TransformObject = TransformObject.))
}

#' @param GroupVariables. Passthrough
#' @param Difference. Passthrough
#' @param Step1SCore. Passthrough
#'
#' @noRd
CarmaRecordCount <- function(GroupVariables. = GroupVariables,
                             Difference. = Difference,
                             Step1SCore. = Step1SCore) {
  if(!is.null(GroupVariables.)) {
    if(Difference.) {
      if(!"GroupVar" %chin% names(Step1SCore.)) N. <- as.integer(Step1SCore.[, .N, by = c(eval(GroupVariables.))][, max(N)]) else N. <- as.integer(Step1SCore.[, .N, by = "GroupVar"][, max(N)])
    } else {
      N. <- as.integer(Step1SCore.[, .N, by = "GroupVar"][, max(N)])
    }
  } else {
    N. <- as.integer(Step1SCore.[, .N])
  }
  return(N.)
}
