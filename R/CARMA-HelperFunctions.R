# AutoQuant is a package for quickly creating high quality visualizations under a common and easy api.
# Copyright (C) <year>  <name of author>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
                              TaskType = 'GPU',
                              BootStrapType = 'MVS',
                              GrowPolicy = 'SymmetricTree',
                              TimeWeights = NULL,
                              HolidayLookback = 7,
                              Difference = FALSE,
                              NonNegativePred = FALSE) {

  # TimeUnit and TimeGroups Args
  TimeGroupPlaceHolder <- c()
  if(any(tolower(c('hours','hour','hr','hrs','hourly')) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, 'hour')
  }
  if(any(tolower(c('hours','hour','hr','hrs','hourly')) %chin% tolower(TimeUnit))) {
    TimeUnit <- 'hour'
  }
  if(any(tolower(c('days','day','dy','dd','d','daily')) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, 'day')
  }
  if(any(tolower(c('days','day','dy','dd','d','daily')) %chin% tolower(TimeUnit))) {
    TimeUnit <- 'day'
  }
  if(any(tolower(c('weeks','week','weaks','weak','wk','wkly','wks','weekly')) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, 'weeks')
  }
  if(any(tolower(c('weeks','week','weaks','weak','wk','wkly','wks','weekly')) %chin% tolower(TimeUnit))) {
    TimeUnit <- 'week'
  }
  if(any(tolower(c('months','month','mth','mnth','monthly','mnthly')) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, 'months')
  }
  if(any(tolower(c('months','month','mth','mnth','monthly','mnthly')) %chin% tolower(TimeUnit))) {
    TimeUnit <- 'months'
  }
  if(any(tolower(c('quarter','qarter','quarterly','q','qtly')) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, 'quarter')
  }
  if(any(tolower(c('quarter','qarter','quarterly','q','qtly')) %chin% tolower(TimeUnit))) {
    TimeUnit <- 'quarter'
  }
  if(any(tolower(c('years','year','annual','yearly','annually','ann','yr','yrly')) %chin% tolower(TimeGroups))) {
    TimeGroupPlaceHolder <- c(TimeGroupPlaceHolder, 'year')
  }
  if(any(tolower(c('years','year','annual','yearly','annually','ann','yr','yrly')) %chin% tolower(TimeUnit))) {
    TimeUnit <- 'year'
  }

  # IndependentVariablePass is for referencing the interaction group column names
  IndepentVariablesPass <- CARMA_Get_IndepentVariablesPass(HierarchGroups)

  # FC Periods
  if(FC_Periods <= 1L) {
    FC_Periods <- 1L
  }

  # Check arguments
  if(!(tolower(PartitionType) %chin% c('random', 'time', 'timeseries'))) stop("PartitionType needs to be one of 'random', 'time', or 'timeseries'")
  if(tolower(PartitionType) == 'timeseries' && is.null(GroupVariables)) PartitionType <- 'time'

  # Additional Args check ----
  if(NonNegativePred && Difference) NonNegativePred <- FALSE
  if(!is.null(HolidayLookback) && !is.numeric(HolidayLookback)) stop('HolidayLookback has to be numeric')
  if(!is.null(TimeWeights) && length(TimeWeights) != 1L) TimeWeights <- NULL
  if(is.null(GrowPolicy)) {
    GrowPolicy <- 'SymmetricTree'
  } else {
    if(tolower(GrowPolicy) == 'lossguide') GrowPolicy <- 'Lossguide'
    if(tolower(GrowPolicy) == 'depthwise') GrowPolicy <- 'Depthwise'
  }
  if(is.null(BootStrapType)) {
    if(TaskType == 'GPU') BootStrapType <- 'Bayesian' else BootStrapType <- 'MVS'
  } else {
    if(is.list(BootStrapType)) {
      if(TaskType == 'GPU' && BootStrapType$classifier == 'MVS') BootStrapType$classifier <- 'Bayesian'
      if(TaskType == 'GPU' && BootStrapType$regression == 'MVS') BootStrapType$regression <- 'Bayesian'
    } else {
      if(TaskType == 'GPU' && BootStrapType == 'MVS') BootStrapType <- 'Bayesian'
    }
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
CarmaFCHorizon <- function(data. = NULL,
                           XREGS. = NULL,
                           TrainOnFull. = NULL,
                           Difference. = NULL,
                           FC_Periods. = NULL,
                           HoldOutPeriods. = NULL,
                           DateColumnName. = NULL) {
  if(!is.null(XREGS.) && TrainOnFull.) {
    if(Difference.) {
      FC_Periods. <- min(-1L + length(unique(XREGS.[[eval(DateColumnName.)]])) - length(unique(data.[[eval(DateColumnName.)]])), FC_Periods.)
    } else {
      FC_Periods. <- min(length(unique(XREGS.[[eval(DateColumnName.)]])) - length(unique(data.[[eval(DateColumnName.)]])), FC_Periods.)
    }
    if(FC_Periods. < 1) stop('Your XREGS does not have forward looking data')
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
CarmaMergeXREGS <- function(data. = NULL,
                            XREGS. = NULL,
                            TargetColumnName. = NULL,
                            GroupVariables. = NULL,
                            DateColumnName. = NULL) {

  if(length(GroupVariables.) > 0L) {
    if(!'GroupVar' %chin% names(XREGS.)) XREGS.[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = GroupVariables.]
    if(any(GroupVariables. %chin% names(XREGS.))) for(g in GroupVariables.) data.table::setnames(x = XREGS., old = eval(g), new = paste0('Add_',eval(g)))
    if(length(GroupVariables.) > 1) {
      data.[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = GroupVariables.]
      data. <- merge(data., XREGS., by = c('GroupVar', eval(DateColumnName.)), all.x = TRUE)
    } else {
      if(class(data.[[GroupVariables.]])[1L] != class(XREGS.[['GroupVar']])[[1L]]) {
        data.table::set(data., j = GroupVariables., value = as.character(data.[[GroupVariables.]]))
        data.table::set(XREGS., j = 'GroupVar', value = as.character(XREGS.[['GroupVar']]))
      }
      data. <- merge(data., XREGS., by.x = c(eval(GroupVariables.), eval(DateColumnName.)), by.y = c('GroupVar', eval(DateColumnName.)), all.x = TRUE)
    }
  } else {
    data. <- merge(data., XREGS., by = c(eval(DateColumnName.)), all.x = TRUE)
  }
  data. <- Rodeo::ModelDataPrep(data = data., Impute = TRUE, CharToFactor = FALSE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = '0', MissNum = -1, IgnoreCols = NULL)
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
CarmaSubsetColumns <- function(data. = NULL,
                               XREGS. = NULL,
                               GroupVariables. = NULL,
                               DateColumnName. = NULL,
                               TargetColumnName. = NULL) {
  if(!is.null(XREGS.)) {
    if(!is.null(GroupVariables.)) {
      xx <- unique(c(TargetColumnName.,DateColumnName.,GroupVariables., names(XREGS.)))
      xx <- xx[!xx %chin% 'GroupVar']
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
CarmaFourier <- function(data. = NULL,
                         XREGS. = NULL,
                         FourierTerms. = NULL,
                         TimeUnit. = NULL,
                         TargetColumnName. = NULL,
                         GroupVariables. = NULL,
                         DateColumnName. = NULL,
                         HierarchGroups. = NULL) {
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
        print('Turning off Fourier Terms. Failed to build.')
        FourierTerms. <- 0
      }
    } else {
      print('Turning off Fourier Terms. Failed to build.')
      FourierTerms. <- 0
    }

    # If Fourier is turned off, concatenate grouping cols
    if(FourierTerms. == 0) {
      if(!is.null(HierarchGroups.)) {
        if(length(HierarchGroups.) > 1) {
          if(any(HierarchGroups. %chin% names(data.))) {
            data.[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(HierarchGroups.)]
            data.[, eval(HierarchGroups.) := NULL]
          }
        } else {
          data.[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(HierarchGroups.)]
          if(HierarchGroups. != 'GroupVar') {
            data.[, eval(HierarchGroups.) := NULL]
          }
        }
      } else if(!is.null(GroupVariables.)) {
        if(all(GroupVariables. %chin% names(data.))) {
          data.[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(GroupVariables.)]
        }
      }
    } else if(!is.null(HierarchGroups.)) {
      if(!'GroupVar' %chin% names(data.)) data.[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(HierarchGroups.)]
    }
  } else {
    FourierFC. <- NULL
  }
  if(!exists('FourierFC.')) FourierFC. <- NULL
  return(list(data = data., FourierFC = FourierFC., FourierTerms = FourierTerms.))
}

#' @param GroupVariables. Passthrough
#' @param Difference. Passthrough
#' @param data. Passthrough
#' @param TargetColumnName. Passthrough
#' @param FC_Periods. Passthrough
#'
#' @noRd
CarmaDifferencing <- function(GroupVariables. = NULL,
                              Difference. = NULL,
                              data. = NULL,
                              TargetColumnName. = NULL,
                              FC_Periods. = NULL) {

  if(!is.null(GroupVariables.) && Difference.) {
    data.[, TargetDiffMidStep := data.table::shift(x = get(TargetColumnName.), n = 1, fill = NA, type = 'lag'), by = c('GroupVar')][, ModTarget := get(TargetColumnName.) - TargetDiffMidStep]
    dataStart <- data.[is.na(TargetDiffMidStep)]
    data. <- data.[!is.na(TargetDiffMidStep)]
    FC_Periods. <- FC_Periods.+1L
    Train <- NULL
    DiffTrainOutput <- NULL
  } else if(Difference.) {
    DiffTrainOutput <- AutoQuant:::DifferenceData(
      data = data.,
      ColumnsToDiff = eval(TargetColumnName.),
      CARMA = TRUE,
      TargetVariable = eval(TargetColumnName.),
      GroupingVariable = NULL)
    data. <- DiffTrainOutput$DiffData
    FC_Periods. <- FC_Periods.+1L
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
CarmaDateStandardize <- function(data. = NULL,
                                 XREGS. = NULL,
                                 DateColumnName. = NULL,
                                 TimeUnit. = NULL) {
  if(is.character(data.[[eval(DateColumnName.)]])) {
    if(!(tolower(TimeUnit.) %chin% c('1min','5min','10min','15min','30min','hour'))) {
      x <- data.[1L, get(DateColumnName.)]
      x1 <- lubridate::guess_formats(x, orders = c('mdY', 'BdY', 'Bdy', 'bdY', 'bdy', 'mdy', 'dby', 'Ymd', 'Ydm', 'dmy'))
      data.[, eval(DateColumnName.) := as.Date(get(DateColumnName.), tryFormats = x1)]
    } else {
      data.[, eval(DateColumnName.) := as.POSIXct(get(DateColumnName.))]
    }
  }
  if(!is.null(XREGS.) && is.character(XREGS.[[eval(DateColumnName.)]])) {
    if(!(tolower(TimeUnit.) %chin% c('1min','5min','10min','15min','30min','hour'))) {
      x <- XREGS.[1L, get(DateColumnName.)]
      x1 <- lubridate::guess_formats(x, orders = c('mdY', 'BdY', 'Bdy', 'bdY', 'bdy', 'mdy', 'dby', 'Ymd', 'Ydm', 'dmy'))
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
CarmaTimeWeights <- function(train. = NULL,
                             TimeWeights. = NULL,
                             GroupVariables. = NULL,
                             DateColumnName. = NULL) {
  if(!is.null(TimeWeights.)) {
    if(!is.null(GroupVariables.)) {
      data.table::setorderv(x = train., cols = c('GroupVar', DateColumnName.), order = c(1,-1))
      train.[, PowerValue := seq_len(.N), by = 'GroupVar']
      train.[, Weights := eval(TimeWeights.) ^ PowerValue]
      data.table::setorderv(x = train., cols = c('GroupVar', DateColumnName.), order = c(1,1))
      train.[, ':=' (PowerValue = NULL)]
    } else {
      data.table::setorderv(x = train., cols = c(DateColumnName.), order = c(-1))
      train.[, PowerValue := seq_len(.N)]
      train.[, Weights := eval(TimeWeights.) ^ PowerValue]
      data.table::setorderv(x = train., cols = c(DateColumnName.), order = c(1))
      train.[, ':=' (PowerValue = NULL)]
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
CarmaTruncateData <- function(data. = NULL,
                              DateColumnName. = NULL,
                              TimeUnit. = NULL) {
  mindate <- data.[, min(get(DateColumnName.))]
  if(tolower(TimeUnit.) %chin% c('hour','hours')) {
    newdate <- mindate + lubridate::hours(val + 1)
  } else if(tolower(TimeUnit.) %chin% c('1min','1mins','1minute','1minutes')) {
    newdate <- mindate + lubridate::minutes(val + 1)
  } else if(tolower(TimeUnit.) %chin% c('5min','5mins','5minute','5minutes')) {
    newdate <- mindate + lubridate::minutes(val + 5)
  } else if(tolower(TimeUnit.) %chin% c('10min','10mins','10minute','10minutes')) {
    newdate <- mindate + lubridate::minutes(val + 10)
  } else if(tolower(TimeUnit.) %chin% c('15min','15mins','15minute','15minutes')) {
    newdate <- mindate + lubridate::minutes(val + 15)
  } else if(tolower(TimeUnit.) %chin% c('30min','30mins','30minute','30minutes')) {
    newdate <- mindate + lubridate::minutes(val + 30)
  } else if(tolower(TimeUnit.) %chin% c('day','days')) {
    newdate <- mindate + lubridate::days(val + 1)
  } else if(tolower(TimeUnit.) %chin% c('week','weeks')) {
    newdate <- mindate + lubridate::weeks(val + 1)
  } else if(tolower(TimeUnit.) %chin% c('month','months')) {
    newdate <- mindate %m+% months(val + 1)
  } else if(tolower(TimeUnit.) %chin% c('quarter','quarters')) {
    newdate <- mindate %m+% months(val + 1)
  } else if(tolower(TimeUnit.) %chin% c('years','year')) {
    newdate <- mindate + lubridate::years(val + 1)
  }
  data. <- data.[get(DateColumnName.) >= eval(newdate)]
  return(data.)
}

#' @title CARMA_Get_IndepentVariablesPass
#'
#' @description CARMA_Get_IndepentVariablesPass is to help manage carma code
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
        IndepentVariablesPass <- paste(IndepentVariablesPass, HierarchGroups[zzz], sep = '_')
      }
    }
  } else {
    IndepentVariablesPass <- 'GroupVar'
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
#' @param TVT. Passthrough
#'
#' @noRd
CarmaPartition <- function(data. = data,
                           SplitRatios. = NULL,
                           TrainOnFull. = NULL,
                           NumSets. = NULL,
                           PartitionType. = NULL,
                           GroupVariables. = NULL,
                           DateColumnName. = NULL,
                           TVT. = NULL) {

  # Data Wrangling: Partition data with Rodeo::AutoDataPartition
  if((!is.null(SplitRatios.) || !TrainOnFull.) && length(TVT.) == 0L) {
    DataSets <- Rodeo::AutoDataPartition(
      data = data.,
      NumDataSets = NumSets.,
      Ratios = SplitRatios.,
      PartitionType = PartitionType.,
      StratifyColumnNames = if(!is.null(GroupVariables.)) 'GroupVar' else NULL,
      TimeColumnName = eval(DateColumnName.))

    # Remove ID Column
    if('ID' %chin% names(data.)) data.table::set(data., j = 'ID', value = NULL)
    TVT. <- DataSets$TVT
    train <- DataSets$TrainData
    valid <- DataSets$ValidationData
    if(NumSets. == 2L) test  <- NULL else test <- DataSets$TestData


  } else if(length(TVT.) > 0L) {
    train <- data.[eval(TVT.$Train)]
    if(length(TVT.$Valid) > 0L) valid <- data.[eval(TVT.$Valid)] else valid <- NULL
    if(length(TVT.$Test) > 0L) test  <- data.[eval(TVT.$Test)] else test <- NULL

  } else {
    train <- data.
    valid <- NULL
    test  <- NULL
    TVT. <- NULL
  }
  return(list(train = train, valid = valid, test = test, data = data., TVT = TVT.))
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
CarmaFeatures <- function(data. = NULL,
                          train. = NULL,
                          XREGS. = NULL,
                          Difference. = NULL,
                          TargetColumnName. = NULL,
                          DateColumnName. = NULL,
                          GroupVariables. = NULL) {
  if(!Difference. || is.null(GroupVariables.)) {
    if(!is.null(XREGS.)) ModelFeatures <- setdiff(names(data.), c(eval(TargetColumnName.), eval(DateColumnName.))) else ModelFeatures <- setdiff(names(train.), c(eval(TargetColumnName.), eval(DateColumnName.)))
    TargetVariable <- eval(TargetColumnName.)
  } else if(Difference. && !is.null(GroupVariables.)) {
    ModelFeatures <- setdiff(names(train.), c(eval(TargetColumnName.), 'ModTarget', 'TargetDiffMidStep', eval(DateColumnName.)))
    TargetVariable <- 'ModTarget'
  } else {
    ModelFeatures <- setdiff(names(train.), c(eval(TargetColumnName.), eval(DateColumnName.)))
    TargetVariable <- eval(TargetColumnName.)
  }
  ModelFeatures <- ModelFeatures[!ModelFeatures %in% 'Weights']
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
#' @param dt Passthrough
#' @param Debug = FALSE
#'
#' @noRd
CarmaScore <- function(Type = 'catboost',
                       i. = i,
                       N. = N,
                       GroupVariables. = NULL,
                       HierarchGroups. = NULL,
                       DateColumnName. = NULL,
                       ModelFeatures. = NULL,
                       Difference. = NULL,
                       TargetColumnName. = NULL,
                       Step1SCore. = NULL,
                       Model. = NULL,
                       FutureDateData. = NULL,
                       NonNegativePred. = NULL,
                       RoundPreds. = NULL,
                       UpdateData. = NULL,
                       FactorList. = NULL,
                       EncodingMethod. = NULL,
                       dt = NULL,
                       Debug = FALSE) {

  if(Debug) print('CarmaScore 1')
  if(Debug) print(i.)

  if(i. == 0L) {

    if(Debug) print('CarmaScore 2')

    # Modify target reference
    if(Difference.) IDcols <- 'ModTarget' else IDcols <- eval(TargetColumnName.)

    # Score model
    if(Type == 'catboost') {

      if(Debug) print('CarmaScore catboost score')

      x <- AutoCatBoostScoring(
        TargetType = 'regression',
        ScoringData = UpdateData.,
        FeatureColumnNames = ModelFeatures.,
        FactorLevelsList = FactorList.,
        IDcols = unique(c(IDcols, DateColumnName.)),
        OneHot = FALSE,
        ModelObject = Model.,
        ModelPath = getwd(),
        ModelID = 'ModelTest',
        ReturnFeatures = TRUE,
        TransformNumeric = FALSE,
        BackTransNumeric = FALSE,
        TargetColumnName = NULL,
        TransformationObject = NULL,
        TransID = NULL,
        TransPath = NULL,
        MDP_Impute = FALSE,
        MDP_CharToFactor = FALSE,
        MDP_RemoveDates = TRUE,
        MDP_MissFactor = '0',
        MDP_MissNum = -1)

      return(data.table::setnames(x = x, old = 'Predict', new = 'Predictions', skip_absent = TRUE))

      # TargetType = 'regression'
      # ScoringData = UpdateData.
      # FeatureColumnNames = ModelFeatures.
      # FactorLevelsList = FactorList.
      # IDcols = unique(c(IDcols, DateColumnName.))
      # OneHot = FALSE
      # ModelObject = Model.
      # ModelPath = getwd()
      # ModelID = 'ModelTest'
      # ReturnFeatures = TRUE
      # TransformNumeric = FALSE
      # BackTransNumeric = FALSE
      # TargetColumnName = NULL
      # TransformationObject = NULL
      # TransID = NULL
      # TransPath = NULL
      # MDP_Impute = FALSE
      # MDP_CharToFactor = FALSE
      # MDP_RemoveDates = TRUE
      # MDP_MissFactor = '0'
      # MDP_MissNum = -1
      # ReturnShapValues = FALSE
      # Debug = TRUE
    } else if(Type == 'xgboost') {

      if(Debug) print('CarmaScore xgboost score')
      IDcols <- unique(c(IDcols, DateColumnName.))

      x <- AutoXGBoostScoring(
        TargetType = 'regression',
        ScoringData = Step1SCore.,
        FeatureColumnNames = ModelFeatures.,
        IDcols = IDcols,
        ModelObject = Model.,
        ModelPath = getwd(),
        ModelID = 'ModelTest',
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
        MDP_MissFactor = '0',
        MDP_MissNum = -1)

      return(data.table::setnames(x = x, old = 'Predict', new = 'Predictions', skip_absent = TRUE))

      # TargetType = 'regression'
      # ScoringData = Step1SCore.
      # FeatureColumnNames = ModelFeatures.
      # IDcols = IDcols
      # ModelObject = Model.
      # ModelPath = getwd()
      # ModelID = 'ModelTest'
      # EncodingMethod = EncodingMethod.
      # ReturnFeatures = TRUE
      # TransformNumeric = FALSE
      # BackTransNumeric = FALSE
      # TargetColumnName = NULL
      # TransformationObject = NULL
      # FactorLevelsList = FactorList.
      # TransID = NULL
      # TransPath = NULL
      # MDP_Impute = TRUE
      # MDP_CharToFactor = TRUE
      # MDP_RemoveDates = TRUE
      # MDP_MissFactor = '0'
      # MDP_MissNum = -1

    } else if(Type == 'lightgbm') {

      if(Debug) print('CarmaScore lightgbm score')
      IDcols <- unique(c(IDcols, DateColumnName.))

      x <- AutoQuant::AutoLightGBMScoring(
        TargetType = 'regression',
        ScoringData = Step1SCore.,
        FeatureColumnNames = ModelFeatures.,
        IDcols = IDcols,
        ModelObject = Model.,
        ModelPath = getwd(),
        ModelID = 'ModelTest',
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
        MDP_MissFactor = '0',
        MDP_MissNum = -1)

      return(data.table::setnames(x = x, old = 'Predict', new = 'Predictions', skip_absent = TRUE))

      # TargetType = 'regression'
      # ScoringData = Step1SCore.
      # FeatureColumnNames = ModelFeatures.
      # IDcols = IDcols
      # ModelObject = Model.
      # ModelPath = getwd()
      # ModelID = 'ModelTest'
      # EncodingMethod = EncodingMethod.
      # ReturnFeatures = TRUE
      # TransformNumeric = FALSE
      # BackTransNumeric = FALSE
      # TargetColumnName = NULL
      # TransformationObject = NULL
      # FactorLevelsList = FactorList.
      # TransID = NULL
      # TransPath = NULL
      # MDP_Impute = TRUE
      # MDP_CharToFactor = TRUE
      # MDP_RemoveDates = TRUE
      # MDP_MissFactor = '0'
      # MDP_MissNum = -1
      # Debug = TRUE

    }
  }

  # Row counts
  if(i. != 1) N. <- as.integer(N. + 1L)

  # Machine Learning: Generate predictions
  if(i. == 1L) {

    # Score model
    if(!is.null(GroupVariables.)) {

      if(Debug) print("i == 1 && GroupVariables > 0")

      # Define IDcols
      if(Difference.) IDcols <- c('ModTarget','TargetDiffMidStep') else IDcols <- eval(TargetColumnName.)
      IDcols <- tryCatch({IDcols[IDcols %chin% names(Step1SCore.)]}, error = function(x) IDcols)

      if(Debug) {
        print('regression')
        print(Step1SCore.)
        print(ModelFeatures.)
        print(FactorList.)
        print(IDcols)
        print(FALSE)
        print(Model.)
        print(getwd())
        print(FALSE)
        print(NULL)
        print(FALSE)
        print('ModelTest')
        print(TRUE)
        print(FALSE)
        print(FALSE)
        print(NULL)
        print(NULL)
        print(NULL)
        print(NULL)
        print(FALSE)
        print(FALSE)
        print(TRUE)
        print('0')
        print(-1)
      }

      # Score Model With Group Variables
      if(Type == 'catboost') {
        Preds <- AutoCatBoostScoring(
          TargetType = 'regression',
          ScoringData = Step1SCore.,
          FeatureColumnNames = ModelFeatures.,
          FactorLevelsList = FactorList.,
          IDcols = IDcols,
          OneHot = FALSE,
          ModelObject = Model.,
          ModelPath = getwd(),
          ReturnShapValues = FALSE,
          MultiClassTargetLevels = NULL,
          RemoveModel = FALSE,
          ModelID = 'ModelTest',
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
          MDP_MissFactor = '0',
          MDP_MissNum = -1,
          Debug = Debug)


        # TargetType = 'regression'
        # ScoringData = Step1SCore.
        # FeatureColumnNames = ModelFeatures.
        # FactorLevelsList = FactorList.
        # IDcols = IDcols
        # OneHot = FALSE
        # ModelObject = Model.
        # ModelPath = getwd()
        # ReturnShapValues = FALSE
        # MultiClassTargetLevels = NULL
        # RemoveModel = FALSE
        # ModelID = 'ModelTest'
        # ReturnFeatures = TRUE
        # TransformNumeric = FALSE
        # BackTransNumeric = FALSE
        # TransformationObject = NULL
        # TransID = NULL
        # TransPath = NULL
        # TargetColumnName = NULL
        # MDP_Impute = FALSE
        # MDP_CharToFactor = FALSE
        # MDP_RemoveDates = TRUE
        # MDP_MissFactor = '0'
        # MDP_MissNum = -1


      } else if(Type == 'xgboost') {

        Preds <- AutoXGBoostScoring(
          TargetType = 'regression',
          ScoringData = Step1SCore.,
          FeatureColumnNames = ModelFeatures.,
          IDcols = IDcols,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
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
          MDP_MissFactor = '0',
          MDP_MissNum = -1)

        # TargetType = 'regression'
        # ScoringData = Step1SCore.
        # FeatureColumnNames = ModelFeatures.
        # IDcols = IDcols
        # ModelObject = Model.
        # ModelPath = getwd()
        # ModelID = 'ModelTest'
        # EncodingMethod = EncodingMethod.
        # ReturnFeatures = TRUE
        # TransformNumeric = FALSE
        # BackTransNumeric = FALSE
        # TargetColumnName = NULL
        # TransformationObject = NULL
        # FactorLevelsList = FactorList.
        # TransID = NULL
        # TransPath = NULL
        # MDP_Impute = TRUE
        # MDP_CharToFactor = TRUE
        # MDP_RemoveDates = TRUE
        # MDP_MissFactor = '0'
        # MDP_MissNum = -1

      } else if(Type == 'lightgbm') {

        Preds <- AutoQuant::AutoLightGBMScoring(
          TargetType = 'regression',
          ScoringData = Step1SCore.,
          FeatureColumnNames = ModelFeatures.,
          IDcols = IDcols,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
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
          MDP_MissFactor = '0',
          MDP_MissNum = -1)

        # TargetType = 'regression'
        # ScoringData = Step1SCore.
        # FeatureColumnNames = ModelFeatures.
        # IDcols = IDcols
        # ModelObject = Model.
        # ModelPath = getwd()
        # ModelID = 'ModelTest'
        # EncodingMethod = EncodingMethod.
        # ReturnFeatures = TRUE
        # TransformNumeric = FALSE
        # BackTransNumeric = FALSE
        # TargetColumnName = NULL
        # TransformationObject = NULL
        # FactorLevelsList = FactorList.
        # TransID = NULL
        # TransPath = NULL
        # MDP_Impute = TRUE
        # MDP_CharToFactor = TRUE
        # MDP_RemoveDates = TRUE
        # MDP_MissFactor = '0'
        # MDP_MissNum = -1
        # Debug = TRUE

      }

    } else {

      # Define IDcols
      IDcols <- eval(TargetColumnName.)
      IDcols <- IDcols[IDcols %chin% names(Step1SCore.)]
      if(length(IDcols) == 0L) IDcols <- NULL

      # Score Model No Group Variables
      if(Type == 'catboost') {
        Preds <- AutoCatBoostScoring(
          TargetType = 'regression',
          ScoringData = data.table::copy(Step1SCore.),
          FeatureColumnNames = ModelFeatures.,
          FactorLevelsList = EncodingMethod.,
          IDcols = IDcols,
          OneHot = FALSE,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
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
          MDP_MissFactor = '0',
          MDP_MissNum = -1)
      } else if(Type == 'xgboost') {
        Preds <- AutoXGBoostScoring(
          TargetType = 'regression',
          ScoringData = Step1SCore.,
          FeatureColumnNames = ModelFeatures.,
          OneHot = FALSE,
          IDcols = IDcols,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
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
          MDP_MissFactor = '0',
          MDP_MissNum = -1)
      } else if(Type == 'lightgbm') {
        Preds <- AutoLightGBMScoring(
          TargetType = 'regression',
          ScoringData = Step1SCore.,
          FeatureColumnNames = ModelFeatures.,
          OneHot = FALSE,
          IDcols = IDcols,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
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
          MDP_MissFactor = '0',
          MDP_MissNum = -1)
      }
    }

    # Data Wrangline: grab historical data and one more future record
    if(Debug) print("Hereye 1")
    if(Difference.) if(eval(DateColumnName.) %chin% names(Preds)) data.table::set(Preds, j = eval(DateColumnName.), value = NULL)
    if(Debug) print("Hereye 2")
    data.table::setnames(Preds, 'Predict', 'Predictions', skip_absent = TRUE)
    if(Debug) print("Hereye 3")
    if(length(dt) > 0L && 'GroupVar' %in% names(dt)) {
      if(Debug) print("Hereye 4")
      gg <- dt[,.N, by = 'GroupVar'][order(-N)]
      if(Debug) print("Hereye 5")
      ggg <- sort(gg[, unique(N)])
      if(Debug) print("Hereye 6")
      l <- length(FutureDateData.)
      if(Debug) print("Hereye 7")
      if(Debug) {
        print(gg)
        print(ggg)
        print(l)
      }
      if(length(ggg) > 1L) {
        # gggg = ggg[1]
        if(Debug) print("Hereye 8")
        max_ggg <- max(ggg)
        if(Debug) {
          print("Hereye 9")
          print(max_ggg)
        }
        Preds[, FutureDateData. := FutureDateData.[length(FutureDateData.)]]
        if(Debug) print("Hereye 10")
        co <- ncol(Preds)
        if(Debug) {
          print("Hereye 11")
          print(co)
          print(head(Preds))
        }
        data.table::setcolorder(Preds, neworder = c(co, 1L:(co-1L)))
        if(Debug) print("Hereye 12")
        for(gggg in ggg) {# gggg = ggg[1]
          if(Debug) print("Hereye 13")
          gv <- as.character(gg[N == eval(gggg)][['GroupVar']])
          if(Debug) {print("Hereye 14"); print(gv)}
          if(gggg == max_ggg) {
            if(Debug) print("Hereye 15.a")
            data.table::set(Preds, i = which(Preds[['GroupVar']] %in% gv), j = 'FutureDateData.', value = rep(eval(FutureDateData.), length(gv)))
          } else {
            if(Debug) {
              print("Hereye 15.b")
              print(which(Preds[['GroupVar']] %in% gv))
              print(length(gv))
              print(head(FutureDateData.))
              print(gggg)
              print(l)
              print(rep(eval(FutureDateData.[(l - gggg + 1L):l]), length(gv)))
            }
            data.table::set(Preds, i = which(Preds[['GroupVar']] %in% gv), j = 'FutureDateData.', value = rep(eval(FutureDateData.[(l - gggg + 1L):l]), length(gv)))
          }
        }

        if(Debug) print("Hereye 16")
        UpdateData. <- Preds

      } else {
        UpdateData. <- cbind(FutureDateData. = FutureDateData.[seq_len(N.)], Preds)
      }
    } else {
      UpdateData. <- cbind(FutureDateData. = FutureDateData.[seq_len(N.)], Preds)
    }

    # Update names
    if(Debug) print("Hereye 17")
    data.table::setnames(UpdateData., 'FutureDateData.', eval(DateColumnName.))

  } else {

    if(!is.null(GroupVariables.)) {

      # Modify target reference
      if(Difference.) IDcols = 'ModTarget' else IDcols <- eval(TargetColumnName.)

      # GroupVar or Hierarchical
      if(!is.null(HierarchGroups.)) {
        temp <- data.table::copy(UpdateData.[, ID := seq_len(.N), by = c(eval(GroupVariables.))])
        temp <- temp[ID == N.][, ID := NULL]
      } else {
        temp <- data.table::copy(UpdateData.[, ID := seq_len(.N), by = 'GroupVar'])
        temp <- temp[, ID2 := max(ID), by = 'GroupVar']
        temp <- temp[ID == ID2][, ID := NULL][, ID2 := NULL]
      }

      # Score model
      if(Type == 'catboost') {
        Preds <- AutoCatBoostScoring(
          TargetType = 'regression',
          ScoringData = temp,
          FeatureColumnNames = ModelFeatures.,
          FactorLevelsList = FactorList.,
          IDcols = IDcols,
          OneHot = FALSE,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
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
          MDP_MissFactor = '0',
          MDP_MissNum = -1)

        # TargetType = 'regression'
        # ScoringData = temp
        # FeatureColumnNames = ModelFeatures.
        # FactorLevelsList = FactorList.
        # IDcols = IDcols
        # OneHot = FALSE
        # ModelObject = Model.
        # ModelPath = getwd()
        # ModelID = 'ModelTest'
        # ReturnFeatures = FALSE
        # TransformNumeric = FALSE
        # BackTransNumeric = FALSE
        # TargetColumnName = NULL
        # TransformationObject = NULL
        # TransID = NULL
        # TransPath = NULL
        # MDP_Impute = FALSE
        # MDP_CharToFactor = FALSE
        # MDP_RemoveDates = TRUE
        # MDP_MissFactor = '0'
        # MDP_MissNum = -1
        # ReturnShapValues = FALSE
        # RemoveModel = FALSE
        # Debug = TRUE

      } else if(Type == 'xgboost') {
        Preds <- AutoXGBoostScoring(
          TargetType = 'regression',
          ScoringData = temp,
          FeatureColumnNames = ModelFeatures.,
          OneHot = FALSE,
          IDcols = IDcols,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
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
          MDP_MissFactor = '0',
          MDP_MissNum = -1)
      } else if(Type == 'lightgbm') {
        if("Weights" %chin% ModelFeatures. && !"Weights" %chin% names(temp)) temp[, Weights := 1]
        Preds <- AutoLightGBMScoring(
          TargetType = 'regression',
          ScoringData = temp,
          FeatureColumnNames = ModelFeatures.,
          OneHot = FALSE,
          IDcols = IDcols,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
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
          MDP_MissFactor = '0',
          MDP_MissNum = -1)
      }

      # Update data group case
      data.table::setnames(Preds, 'Predictions', 'Preds')
      if(NonNegativePred. && !Difference.) Preds[, Preds := data.table::fifelse(Preds < 0.5, 0, Preds)]
      Preds <- cbind(UpdateData.[, ID2 := max(ID), by = 'GroupVar'][ID == ID2][, ID2 := NULL], Preds)
      if(Difference.) Preds[, ModTarget := Preds][, eval(TargetColumnName.) := Preds] else Preds[, eval(TargetColumnName.) := Preds]
      Preds[, Predictions := Preds][, Preds := NULL]
      if(RoundPreds.) Preds[, Predictions := round(Predictions)]
      UpdateData. <- UpdateData.[, ID2 := max(ID), by = 'GroupVar'][ID != ID2][, ID2 := NULL]
      if(any(class(UpdateData.[[eval(DateColumnName.)]]) %chin% c('POSIXct','POSIXt')) && any(class(Preds[[eval(DateColumnName.)]]) == 'Date')) UpdateData.[, eval(DateColumnName.) := as.Date(get(DateColumnName.))]
      x_ <- setdiff(names(Preds),names(UpdateData.))
      if(length(x_) > 0L && x_ == "Predictions") {
        Preds[, Predict := Predictions][, Predictions := NULL]
      }

      # Stack Previous Data with 1-Step Ahead Forecast Data
      #   Datasets must have the same exact names
      UpdateData. <- data.table::rbindlist(list(UpdateData., Preds), fill = TRUE)
      if(Difference.) UpdateData. <- UpdateData.[, ID2 := max(ID), by = 'GroupVar'][ID %in% c(ID2-1, ID2), eval(TargetColumnName.) := cumsum(get(TargetColumnName.)), by = 'GroupVar'][, ID2 := NULL]
      UpdateData.[, ID := NULL]

    } else {

      # print("CARMA HELPER SCORING 5")

      # Score Model
      if(Type == 'catboost') {
        Preds <- AutoCatBoostScoring(
          TargetType = 'regression',
          ScoringData = UpdateData.[.N],
          FeatureColumnNames = ModelFeatures.,
          FactorLevelsList = EncodingMethod.,
          IDcols = NULL,
          OneHot = FALSE,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
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
          MDP_MissFactor = '0',
          MDP_MissNum = -1)
      } else if(Type == 'xgboost') {
        Preds <- AutoXGBoostScoring(
          TargetType = 'regression',
          ScoringData = UpdateData.[.N, ],
          FeatureColumnNames = ModelFeatures.,
          OneHot = FALSE,
          IDcols = NULL,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
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
          MDP_MissFactor = '0',
          MDP_MissNum = -1)
      } else if(Type == 'lightgbm') {
        if("Weights" %chin% ModelFeatures. && !"Weights" %chin% names(UpdateData.)) UpdateData.[, Weights := 1]
        Preds <- AutoLightGBMScoring(
          TargetType = 'regression',
          ScoringData = UpdateData.[.N],
          FeatureColumnNames = ModelFeatures.,
          OneHot = FALSE,
          IDcols = NULL,
          ModelObject = Model.,
          ModelPath = getwd(),
          ModelID = 'ModelTest',
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
          MDP_MissFactor = '0',
          MDP_MissNum = -1)
      }

      # Update data non-group case
      if(NonNegativePred.) Preds[, Predictions := data.table::fifelse(Predictions < 0.5, 0, Predictions)]
      if(RoundPreds.) Preds[, Predictions := round(Predictions)]
      data.table::set(UpdateData., i = N., j = c('Predictions',TargetColumnName.), value = Preds[[1L]])
    }
  }

  # Return
  if(Debug) print("Hereye 18")
  return(list(UpdateData = UpdateData., Preds = Preds, N = N.))
}

#' @param UpdateData. Passthrough
#' @param TimeUnit. Passthrough
#' @param DateColumnName. Passthrough
#'
#' @noRd
NextTimePeriod <- function(UpdateData. = NULL,
                           TimeUnit. = NULL,
                           DateColumnName. = NULL) {
  d <- max(UpdateData.[[DateColumnName.]])
  if(tolower(TimeUnit.) %chin% c('hour','hours')) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::hours(1))
  } else if(tolower(TimeUnit.) %chin% c('1min','1mins','1minute','1minutes')) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(1))
  } else if(tolower(TimeUnit.) %chin% c('5min','5mins','5minute','5minutes')) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(5))
  } else if(tolower(TimeUnit.) %chin% c('10min','10mins','10minute','10minutes')) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(10))
  } else if(tolower(TimeUnit.) %chin% c('15min','15mins','15minute','15minutes')) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(15))
  } else if(tolower(TimeUnit.) %chin% c('30min','30mins','30minute','30minutes')) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::minutes(30))
  } else if(tolower(TimeUnit.) %chin% c('day','days')) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::days(1))
  } else if(tolower(TimeUnit.) %chin% c('week','weeks')) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::weeks(1))
  } else if(tolower(TimeUnit.) %chin% c('month','months')) {
    CalendarFeatures <- data.table::as.data.table(d %m+% months(1))
  } else if(tolower(TimeUnit.) %chin% c('quarter','quarters')) {
    CalendarFeatures <- data.table::as.data.table(d %m+% months(3))
  } else if(tolower(TimeUnit.) %chin% c('years','year')) {
    CalendarFeatures <- data.table::as.data.table(d + lubridate::years(1))
  }
  return(CalendarFeatures)
}

#' @param UpdateData. Passthrough
#' @param TimeUnit. Passthrough
#' @param DateColumnName. Passthrough
#' @param GroupVariables. Passthrough
#'
#' @noRd
FutureTimePeriods <- function(UpdateData. = NULL,
                              TimeUnit. = NULL,
                              DateColumnName. = NULL,
                              FC_Periods = 5,
                              GroupVariables. = NULL,
                              SkipPeriods = NULL) {

  # Get latest date
  if(data.table::is.data.table(UpdateData.)) {
    d <- max(UpdateData.[[eval(DateColumnName.)]])
  } else {
    d <- max(UpdateData.)
  }

  # Computer periods ahead
  if(tolower(TimeUnit.) %chin% c('hour','hours')) {

    x <- c(d + lubridate::hours(1L)); for(i in seq_len(FC_Periods)[-1L]) x <- c(x, d + lubridate::hours(i))

  } else if(tolower(TimeUnit.) %chin% c('1min','1mins','1minute','1minutes')) {

    x <- c(d + lubridate::minutes(1L)); for(i in seq_len(FC_Periods)[-1L]) x <- c(x, d + lubridate::minutes(i))

  } else if(tolower(TimeUnit.) %chin% c('5min','5mins','5minute','5minutes')) {

    x <- c(d + lubridate::minutes(5L)); for(i in seq_len(FC_Periods)[-1L]) x <- c(x, d + lubridate::minutes(i*5))

  } else if(tolower(TimeUnit.) %chin% c('10min','10mins','10minute','10minutes')) {

    x <- c(d + lubridate::minutes(10L)); for(i in seq_len(FC_Periods)[-1L]) x <- c(x, d + lubridate::minutes(i*10))

  } else if(tolower(TimeUnit.) %chin% c('15min','15mins','15minute','15minutes')) {

    x <- c(d + lubridate::minutes(15L)); for(i in seq_len(FC_Periods)[-1L]) x <- c(x, d + lubridate::minutes(i*15))

  } else if(tolower(TimeUnit.) %chin% c('30min','30mins','30minute','30minutes')) {

    x <- c(d + lubridate::minutes(1L)); for(i in seq_len(FC_Periods)[-1L]) x <- c(x, d + lubridate::minutes(i*30))

  } else if(tolower(TimeUnit.) %chin% c('day','days')) {

    x <- c(d + lubridate::days(1L)); for(i in seq_len(FC_Periods)[-1L]) x <- c(x, d + lubridate::days(i))

  } else if(tolower(TimeUnit.) %chin% c('week','weeks')) {

    x <- c(d + lubridate::weeks(1L)); for(i in seq_len(FC_Periods)[-1L]) x <- c(x, d + lubridate::weeks(i))

  } else if(tolower(TimeUnit.) %chin% c('month','months')) {

    x <- c(d %m+% months(1L)); for(i in seq_len(FC_Periods)[-1L]) x <- c(x, d %m+% months(i))

  } else if(tolower(TimeUnit.) %chin% c('quarter','quarters')) {

    x <- c(d %m+% months(1L)); for(i in seq_len(FC_Periods)[-1L]) x <- c(x, d %m+% months(3*i))

  } else if(tolower(TimeUnit.) %chin% c('years','year')) {

    x <- c(d + lubridate::years(1L)); for(i in seq_len(FC_Periods)[-1L]) x <- c(x, d + lubridate::years(i))

  }

  # IF:
  #     Panel Case: Returns data.table
  # ELSE IF:
  #     used for XREGS Management at beginning of CARMA. Need to figure out the max_date for the forecast horizon
  if(length(GroupVariables.) > 0L && 'GroupVar' %in% names(UpdateData.)) {
    y <- unique(UpdateData.[, .SD, .SDcols = 'GroupVar'])
    x <- cbind(y, rep(x,y[,.N]))
    data.table::setnames(x = x, old = names(x)[ncol(x)], DateColumnName.)
    return(x)
  } else if(length(GroupVariables.) > 0L) {
    y <- unique(UpdateData.[, .SD, .SDcols = GroupVariables.])
    x <- cbind(y, rep(x,y[,.N]))
    data.table::setnames(x = x, old = names(x)[ncol(x)], DateColumnName.)
    return(x)
  }

  # Time Series Case: returns vector
  x <- data.table::as.data.table(x)
  data.table::setnames(x = x, old = names(x)[ncol(x)], DateColumnName.)
  return(x)
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
#' @param Debug Passthrough
#' @param RollingVars. Passthrough
#'
#' @noRd
UpdateFeatures <- function(UpdateData. = NULL,
                           GroupVariables. = NULL,
                           CalendarFeatures. = NULL,
                           CalendarVariables. = NULL,
                           GroupVarVector. = NULL,
                           DateColumnName. = NULL,
                           XREGS. = NULL,
                           FourierTerms. = NULL,
                           FourierFC. = NULL,
                           TimeGroups. = NULL,
                           TimeTrendVariable. = NULL,
                           N. = NULL,
                           TargetColumnName. = NULL,
                           HolidayVariable. = NULL,
                           HolidayLookback. = NULL,
                           TimeUnit. = NULL,
                           AnomalyDetection. = NULL,
                           i. = i,
                           RollingVars. = TRUE,
                           Debug = FALSE) {

  if(RollingVars.) {

    if(Debug) print('UpdateFeatures 1')

    # Merge groups vars
    if(length(GroupVariables.) > 0L) CalendarFeatures. <- cbind(unique(GroupVarVector.), CalendarFeatures.)

    if(Debug) print('UpdateFeatures 2')

    # Update colname for date
    data.table::setnames(CalendarFeatures., names(CalendarFeatures.)[ncol(CalendarFeatures.)], eval(DateColumnName.))

    if(Debug) print('UpdateFeatures 3')

    # Merge XREGS if not null
    if(length(XREGS.) > 0L) {
      if(!is.null(GroupVariables.)) {
        if(Debug) print('UpdateFeatures 4')
        CalendarFeatures. <- Rodeo::ModelDataPrep(data = CalendarFeatures., Impute = FALSE, CharToFactor = FALSE, FactorToChar = TRUE, IntToNumeric = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = '0', MissNum = -1, IgnoreCols = NULL)
        CalendarFeatures. <- merge(CalendarFeatures., XREGS., by = c('GroupVar', eval(DateColumnName.)), all = FALSE)
      } else {
        if(Debug) print('UpdateFeatures 4.1')
        CalendarFeatures. <- merge(CalendarFeatures., XREGS., by = eval(DateColumnName.), all = FALSE)
      }
    }

    if(Debug) print('UpdateFeatures 5')

    # Add fouier terms
    if(length(GroupVariables.) > 0L && FourierTerms. > 0) {
      if(Debug) print('UpdateFeatures 6')
      CalendarFeatures. <- merge(CalendarFeatures., FourierFC., by = DateColumnName., all = FALSE)
    } else if(FourierTerms. > 0L) {
      if(Debug) print('UpdateFeatures 6.1')
      if(!is.null(FourierFC.)) {
        if(Debug) print('UpdateFeatures 6.2')
        if(length(FourierFC.) != 0) CalendarFeatures. <- merge(CalendarFeatures., FourierFC., by = c('GroupVar',eval(DateColumnName.)), all = FALSE)
      }
    }

    if(Debug) print('UpdateFeatures 7')

    # Prepare for more feature engineering
    if(!tolower(TimeGroups.[1L]) %chin% c('5min','10min','15min','30min','hour')) CalendarFeatures.[, eval(DateColumnName.) := data.table::as.IDate(get(DateColumnName.))]

    if(Debug) print('UpdateFeatures 8')

    # Update calendar variables
    if(length(CalendarVariables.) > 0L) {

      if(Debug) print('UpdateFeatures 9')
      CalendarFeatures. <- Rodeo::CreateCalendarVariables(
        data = CalendarFeatures.,
        DateCols = eval(DateColumnName.),
        AsFactor = FALSE,
        TimeUnits = CalendarVariables.)
    }

    if(Debug) print('UpdateFeatures 10')

    # Update Time Trend feature
    if(TimeTrendVariable.) CalendarFeatures.[, TimeTrend := eval(N.) + 1]

    if(Debug) print('UpdateFeatures 11')

    # Prepare data for scoring
    for(zz in seq_along(TargetColumnName.)) {# zz = 1
      if(Debug) print('UpdateFeatures 12')
      if(zz == 1) temp <- cbind(CalendarFeatures., 1) else temp <- cbind(temp, 1)
      if(Debug) print('UpdateFeatures 12.1')
      data.table::setnames(temp, 'V2', c(eval(TargetColumnName.[zz])))
    }

    if(Debug) print('UpdateFeatures 13')

    if(any(class(UpdateData.[[eval(DateColumnName.)]]) %chin% c('POSIXct','POSIXt','IDate'))) UpdateData.[, eval(DateColumnName.) := as.Date(get(DateColumnName.))]

    if(Debug) print('UpdateFeatures 14')

    if(any(class(temp[[eval(DateColumnName.)]]) %chin% c('POSIXct','POSIXt','IDate'))) temp[, eval(DateColumnName.) := as.Date(get(DateColumnName.))]

    if(Debug) print('UpdateFeatures 15')

    #print(UpdateData.)
    #print("------------------------")
    #print(temp)
    UpdateData. <- data.table::rbindlist(list(UpdateData., temp), fill = TRUE, use.names = TRUE)

    if(Debug) print('UpdateFeatures 16')

    # Update holiday feature
    if(!is.null(HolidayVariable.) && any(is.na(UpdateData.[['HolidayCounts']]))) {

      if(Debug) print('Update Features 17.a')

      if(!is.null(HolidayLookback.)) LBD <- HolidayLookback. else if(!is.null(TimeUnit.)) LBD <- LB(TimeUnit.) else LBD <- 1L
      if(is.null(GroupVariables.)) {

        if(Debug) print('UpdateFeatures 18')

        temp <- UpdateData.[(.N-LBD):.N]

        if(Debug) print('UpdateFeatures 19')

        UpdateData. <- UpdateData.[1:(.N-LBD-1)]

        if(Debug) print('UpdateFeatures 20')

        temp <- Rodeo::CreateHolidayVariables(
          temp,
          DateCols = eval(DateColumnName.),
          LookbackDays = if(!is.null(HolidayLookback.)) HolidayLookback. else LB(TimeUnit.),
          HolidayGroups = HolidayVariable.,
          Holidays = NULL)

        if(Debug) print('UpdateFeatures 21')

        UpdateData. <- data.table::rbindlist(list(UpdateData., temp), fill = TRUE)

        if(Debug) print('UpdateFeatures 22')

      } else {
        if(Debug) print("Update Features 17.b holiday variables create")
        UpdateData. <- Rodeo::CreateHolidayVariables(
          UpdateData.,
          DateCols = eval(DateColumnName.),
          LookbackDays = if(!is.null(HolidayLookback.)) HolidayLookback. else if(!is.null(TimeUnit.)) LB(TimeUnit.) else 1L,
          HolidayGroups = HolidayVariable.,
          Holidays = NULL)
      }
    }

    if(Debug) print('UpdateFeatures 23')

    # Update Anomaly Detection
    if(i. > 1 && !is.null(AnomalyDetection.)) {
      if(Debug) print('UpdateFeatures 24')
      UpdateData.[, ':=' (AnomHigh = 0, AnomLow = 0)]
    }

    if(Debug) print('UpdateFeatures done')

    # Return
    return(UpdateData = UpdateData.)

  } else {

    # Merge groups vars
    if(Debug) print('UpdateFeatures 1')
    if(!is.null(GroupVariables.)) CalendarFeatures. <- cbind(unique(GroupVarVector.), CalendarFeatures.)

    # Update colname for date
    if(Debug) print('UpdateFeatures 2')
    data.table::setnames(CalendarFeatures., names(CalendarFeatures.)[ncol(CalendarFeatures.)], eval(DateColumnName.))

    # Merge XREGS if not null
    if(Debug) print('UpdateFeatures 3')
    if(!is.null(XREGS.)) {
      if(!is.null(GroupVariables.)) {
        if(Debug) print('UpdateFeatures 4')
        CalendarFeatures. <- Rodeo::ModelDataPrep(data = CalendarFeatures., Impute = FALSE, CharToFactor = FALSE, FactorToChar = TRUE, IntToNumeric = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = '0', MissNum = -1, IgnoreCols = NULL)
        CalendarFeatures. <- merge(CalendarFeatures., XREGS., by = c('GroupVar', eval(DateColumnName.)), all = FALSE)
      } else {
        if(Debug) print('UpdateFeatures 4.1')
        CalendarFeatures. <- merge(CalendarFeatures., XREGS., by = eval(DateColumnName.), all = FALSE)
      }
    }

    # Add fouier terms
    if(Debug) print('UpdateFeatures 5')
    if(is.null(GroupVariables.) && FourierTerms. > 0) {
      if(Debug) print('UpdateFeatures 6')
      CalendarFeatures. <- merge(CalendarFeatures., FourierFC., by = DateColumnName., all = FALSE)
    } else if(FourierTerms. > 0) {
      if(Debug) print('UpdateFeatures 6.1')
      if(!is.null(FourierFC.)) {
        if(Debug) print('UpdateFeatures 6.2')
        if(length(FourierFC.) != 0) CalendarFeatures. <- merge(CalendarFeatures., FourierFC., by = c('GroupVar',eval(DateColumnName.)), all = FALSE)
      }
    }

    # Prepare for more feature engineering
    if(Debug) print('UpdateFeatures 7')
    if(!tolower(TimeGroups.[1L]) %chin% c('5min','10min','15min','30min','hour')) CalendarFeatures.[, eval(DateColumnName.) := data.table::as.IDate(get(DateColumnName.))]

    # Update calendar variables
    if(Debug) print('UpdateFeatures 8')
    if(!is.null(CalendarVariables.)) {
      if(Debug) print('UpdateFeatures 9')
      CalendarFeatures. <- Rodeo::CreateCalendarVariables(
        data = CalendarFeatures.,
        DateCols = eval(DateColumnName.),
        AsFactor = FALSE,
        TimeUnits = CalendarVariables.)
    }

    # Update Time Trend feature
    if(Debug) print('UpdateFeatures 10')
    if(TimeTrendVariable. && length(GroupVariables.) > 0L) {
      data.table::setorderv(x = CalendarFeatures., cols = c('GroupVar', DateColumnName.))
      CalendarFeatures.[, TimeTrend := seq_len(.N), by = 'GroupVar'][, TimeTrend := TimeTrend + eval(N.)]
    } else {
      data.table::setorderv(x = CalendarFeatures., cols = c(DateColumnName.))
      CalendarFeatures.[, TimeTrend := seq_len(.N)][, TimeTrend := TimeTrend + eval(N.)]
    }

    # Prepare data for scoring
    if(Debug) print('UpdateFeatures 11')
    for(zz in seq_along(TargetColumnName.)) {
      if(Debug) print('UpdateFeatures 12')
      if(zz == 1) temp <- cbind(CalendarFeatures., 1) else temp <- cbind(temp, 1)
      if(Debug) print('UpdateFeatures 12.1')
      data.table::setnames(temp, 'V2', c(eval(TargetColumnName.[zz])))
    }

    if(Debug) print('UpdateFeatures 13')
    if(any(class(UpdateData.[[eval(DateColumnName.)]]) %chin% c('POSIXct','POSIXt','IDate'))) UpdateData.[, eval(DateColumnName.) := as.Date(get(DateColumnName.))]

    if(Debug) print('UpdateFeatures 14')
    if(any(class(temp[[eval(DateColumnName.)]]) %chin% c('POSIXct','POSIXt','IDate'))) temp[, eval(DateColumnName.) := as.Date(get(DateColumnName.))]

    if(Debug) print('UpdateFeatures 15')
    UpdateData. <- data.table::rbindlist(list(UpdateData., temp), fill = TRUE, use.names = TRUE)

    # Update holiday feature
    if(Debug) print('UpdateFeatures 16')
    if(!is.null(HolidayVariable.) && any(is.na(UpdateData.[['HolidayCounts']]))) {

      if(Debug) print('Update Features 17')
      if(!is.null(HolidayLookback.)) LBD <- HolidayLookback. else if(!is.null(TimeUnit.)) LBD <- LB(TimeUnit.) else LBD <- 1L
      if(is.null(GroupVariables.)) {

        if(Debug) print('UpdateFeatures 18')
        temp <- UpdateData.[(.N-LBD):.N]

        if(Debug) print('UpdateFeatures 19')
        UpdateData. <- UpdateData.[1:(.N-LBD-1)]

        if(Debug) print('UpdateFeatures 20')
        temp <- Rodeo::CreateHolidayVariables(
          temp,
          DateCols = eval(DateColumnName.),
          LookbackDays = if(!is.null(HolidayLookback.)) HolidayLookback. else LB(TimeUnit.),
          HolidayGroups = HolidayVariable.,
          Holidays = NULL)

        if(Debug) print('UpdateFeatures 21')
        UpdateData. <- data.table::rbindlist(list(UpdateData., temp), fill = TRUE)

      } else {
        if(Debug) print("Update Features 17.b holiday variables create")
        UpdateData. <- Rodeo::CreateHolidayVariables(
          UpdateData.,
          DateCols = eval(DateColumnName.),
          LookbackDays = if(!is.null(HolidayLookback.)) HolidayLookback. else if(!is.null(TimeUnit.)) LB(TimeUnit.) else 1L,
          HolidayGroups = HolidayVariable.,
          Holidays = NULL)
      }
    }

    # Update Anomaly Detection
    if(Debug) print('UpdateFeatures 23')
    if(i. > 1 && !is.null(AnomalyDetection.)) {
      if(Debug) print('UpdateFeatures 24')
      UpdateData.[, data.table::let(AnomHigh = 0, AnomLow = 0)]
    }

    # Return
    if(Debug) print('UpdateFeatures done')
    return(UpdateData.)
  }
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
CarmaTimeSeriesScorePrep <- function(UpdateData.. = NULL,
                                     GroupVariables.. = NULL,
                                     DateColumnName.. = NULL) {
  if(any(is.na(UpdateData..[['Predictions']]))) {
    data.table::set(x = UpdateData.., i = which(is.na(UpdateData..[['Predictions']])), j = 'Predictions', value = 1.0)
  }
  if('ModTarget' %chin% names(UpdateData..) && any(is.na(UpdateData..[['ModTarget']]))) {
    data.table::set(x = UpdateData.., i = which(is.na(UpdateData..[['ModTarget']])), j = 'ModTarget', value = 1.0)
  }
  if(!is.null(GroupVariables..)) {
    if(!'GroupVar' %chin% names(UpdateData..)) {
      data.table::setorderv(x = UpdateData.., cols = c(eval(GroupVariables..), eval(DateColumnName..)))
      UpdateData..[, ID := .N:1, by = c(eval(GroupVariables..))]
    } else {
      data.table::setorderv(x = UpdateData.., cols = c('GroupVar', eval(DateColumnName..)))
      UpdateData..[, ID := .N:1, by = 'GroupVar']
    }
  } else {
    data.table::setorderv(x = UpdateData.., cols = c(eval(DateColumnName..)))
    UpdateData..[, ID := .N:1]
  }
  keep <- setdiff(names(UpdateData..), names(UpdateData..)[grep(pattern = 'LAG', x = names(UpdateData..))])
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
                                    TargetColumnName. = NULL,
                                    DateColumnName. = NULL,
                                    GroupVariables. = NULL,
                                    HierarchGroups. = NULL,
                                    Difference. = NULL,
                                    TimeGroups. = NULL,
                                    TimeUnit. = NULL,
                                    Lags. = NULL,
                                    MA_Periods. = NULL,
                                    SD_Periods. = NULL,
                                    Skew_Periods. = NULL,
                                    Kurt_Periods. = NULL,
                                    Quantile_Periods. = NULL,
                                    Quantiles_Selected. = NULL,
                                    HolidayVariable. = NULL,
                                    HolidayLags. = NULL,
                                    HolidayMovingAverages. = NULL,
                                    DebugMode. = NULL) {

  Lags. <- Lags.[!Lags. %in% 0]
  HolidayVariable. <- HolidayVariable.[!HolidayVariable. %in% 0]
  MA_Periods. <- MA_Periods.[!MA_Periods. %in% 0]
  SD_Periods. <- SD_Periods.[!SD_Periods. %in% 0]
  Skew_Periods. <- Skew_Periods.[!Skew_Periods. %in% 0]
  Kurt_Periods. <- Kurt_Periods.[!Kurt_Periods. %in% 0]
  Quantile_Periods. <- Quantile_Periods.[!Quantile_Periods. %in% 0]
  if(length(Lags.) == 0L) Lags. <- NULL
  if(length(HolidayVariable.) == 0L) HolidayVariable. <- NULL
  if(length(MA_Periods.) == 0L) MA_Periods. <- NULL
  if(length(SD_Periods.) == 0L) SD_Periods. <- NULL
  if(length(Skew_Periods.) == 0L) Skew_Periods. <- NULL
  if(length(Kurt_Periods.) == 0L) Kurt_Periods. <- NULL
  if(length(Quantile_Periods.) == 0L) Quantile_Periods. <- NULL

  # Feature Engineering: Add GDL Features based on the TargetColumnName
  if(length(Lags.) > 0L) {

    # Group and No Differencing
    if(!is.null(GroupVariables.) && !Difference.) {

      # Split GroupVar and Define HierarchyGroups and IndependentGroups
      Output <- CARMA_GroupHierarchyCheck(data = data., Group_Variables = GroupVariables., HierarchyGroups = HierarchGroups.)
      data. <- Output$data
      HierarchSupplyValue <- Output$HierarchSupplyValue
      IndependentSupplyValue <- Output$IndependentSupplyValue
      if(is.list(Lags.)) TimeGroups. <- names(Lags.)

      # Generate features----
      data. <- Rodeo::AutoLagRollStats(

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
        Type                 = 'Lag',
        SimpleImpute         = TRUE,
        ShortName            = TRUE,

        # Calculated Columns
        Lags                 = Lags.,
        MA_RollWindows       = MA_Periods.,
        SD_RollWindows       = SD_Periods.,
        Skew_RollWindows     = Skew_Periods.,
        Kurt_RollWindows     = Kurt_Periods.,
        Quantile_RollWindows = Quantile_Periods.,
        Quantiles_Selected   = Quantiles_Selected.)

      # Keep interaction group as GroupVar
      if(length(GroupVariables.) > 1) {
        if(!'GroupVar' %chin% names(data.)) data.[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(GroupVariables.)]
        if(!is.null(HierarchGroups.)) Categoricals <- FullFactorialCatFeatures(GroupVars = HierarchGroups., BottomsUp = TRUE) else Categoricals <- NULL
        if(!is.null(HierarchGroups.)) GroupVarVector <- unique(data.[, .SD, .SDcols = c(Categoricals,'GroupVar')]) else GroupVarVector <- unique(data.[, .SD, .SDcols = c('GroupVar')])
      } else {
        if(!'GroupVar' %chin% names(data.)) data.[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = GroupVariables.]
        GroupVarVector <- unique(data.[, .SD, .SDcols = c('GroupVar')])
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
      data. <- Rodeo::AutoLagRollStats(

        # Data
        data                 = data.,
        DateColumn           = eval(DateColumnName.),
        Targets              = c('ModTarget'),
        HierarchyGroups      = HierarchSupplyValue,
        IndependentGroups    = IndependentSupplyValue,

        # Services
        TimeBetween          = NULL,
        TimeUnit             = TimeUnit.,
        TimeUnitAgg          = TimeUnit.,
        TimeGroups           = TimeGroups.,
        RollOnLag1           = TRUE,
        Type                 = 'Lag',
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
        if(!'GroupVar' %chin% names(data.)) data.[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = GroupVariables.]
        if(!is.null(HierarchGroups.)) Categoricals <- FullFactorialCatFeatures(GroupVars = HierarchGroups., BottomsUp = TRUE) else Categoricals <- NULL
        if(!is.null(HierarchGroups.)) GroupVarVector <- data.[, .SD, .SDcols = c(Categoricals,'GroupVar')] else GroupVarVector <- unique(data.[, .SD, .SDcols = c('GroupVar')])
      } else {
        if(!'GroupVar' %chin% names(data.)) data.[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = GroupVariables.]
        GroupVarVector <- unique(data.[, .SD, .SDcols = c('GroupVar')])
      }
    }

    # No Group with or without Diff
    if(is.null(GroupVariables.)) {

      # TimeGroups----
      if(is.list(Lags.)) TimeGroups. <- names(Lags.)

      # Generate features
      data. <- Rodeo::AutoLagRollStats(

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
        Type                 = 'Lag',
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
  if(length(HolidayVariable.) > 0L && (length(HolidayLags.) > 0L || length(HolidayMovingAverages.) > 0L)) {
    if(!is.null(GroupVariables.)) {

      # Build Features----
      data. <- Rodeo::DT_GDL_Feature_Engineering(
        data            = data.,
        lags            = HolidayLags.,
        periods         = HolidayMovingAverages.[HolidayMovingAverages. > 0],
        SDperiods       = 0,
        Skewperiods     = 0,
        Kurtperiods     = 0,
        Quantileperiods = 0,
        statsFUNs       = 'mean',
        targets         = 'HolidayCounts',
        groupingVars    = IndependentSupplyValue,
        sortDateName    = DateColumnName.,
        timeDiffTarget  = NULL,
        timeAgg         = TimeGroups.[1L],
        WindowingLag    = 1,
        Type            = 'Lag',
        SimpleImpute    = TRUE)

    } else {

      data. <- Rodeo::DT_GDL_Feature_Engineering(
        data            = data.,
        lags            = HolidayLags.,
        periods         = HolidayMovingAverages.[HolidayMovingAverages. > 0],
        SDperiods       = 0,
        Skewperiods     = 0,
        Kurtperiods     = 0,
        Quantileperiods = 0,
        statsFUNs       = 'mean',
        targets         = 'HolidayCounts',
        groupingVars    = NULL,
        sortDateName    = DateColumnName.,
        timeDiffTarget  = NULL,
        timeAgg         = TimeGroups.[1L],
        WindowingLag    = 1,
        Type            = 'Lag',
        SimpleImpute    = TRUE)
    }
  }

  # Return
  return(list(
    data = data.,
    HierarchSupplyValue = if(!exists('HierarchSupplyValue')) NULL else HierarchSupplyValue,
    IndependentSupplyValue = if(!exists('IndependentSupplyValue')) NULL else IndependentSupplyValue,
    GroupVarVector = if(!exists('GroupVarVector')) NULL else GroupVarVector,
    Categoricals = if(!exists('Categoricals')) NULL else Categoricals))
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
CarmaRollingStatsUpdate <- function(ModelType = 'catboost',
                                    DebugMode. = FALSE,
                                    UpdateData. = NULL,
                                    GroupVariables. = NULL,
                                    Difference. = NULL,
                                    CalendarVariables. = NULL,
                                    HolidayVariable. = NULL,
                                    IndepVarPassTRUE. = NULL,
                                    data. = NULL,
                                    CalendarFeatures. = NULL,
                                    XREGS. = NULL,
                                    HierarchGroups. = NULL,
                                    GroupVarVector. = NULL,
                                    TargetColumnName. = NULL,
                                    DateColumnName. = NULL,
                                    Preds. = NULL,
                                    HierarchSupplyValue. = NULL,
                                    IndependentSupplyValue. = NULL,
                                    TimeUnit. = NULL,
                                    TimeGroups. = NULL,
                                    Lags. = NULL,
                                    MA_Periods. = NULL,
                                    SD_Periods. = NULL,
                                    Skew_Periods. = NULL,
                                    Kurt_Periods. = NULL,
                                    Quantile_Periods. = NULL,
                                    Quantiles_Selected. = NULL,
                                    HolidayLags. = NULL,
                                    HolidayMovingAverages. = NULL) {

  # Group with or Diff
  if(!is.null(Lags.) && !is.null(GroupVariables.) && Difference.) {

    # Calendar and Holiday----
    if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
    if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE

    # Create data for GDL
    temp <- CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
    Temporary <- temp$data
    keep <- temp$keep; rm(temp)

    # Build Features
    Temporary <- Rodeo::AutoLagRollStatsScoring(

      # Data
      data                 = Temporary,
      RowNumsID            = 'ID',
      RowNumsKeep          = 1,
      DateColumn           = eval(DateColumnName.),
      Targets              = 'ModTarget',
      HierarchyGroups      = HierarchSupplyValue.,
      IndependentGroups    = IndependentSupplyValue.,

      # Services
      TimeBetween          = NULL,
      TimeUnit             = TimeUnit.,
      TimeUnitAgg          = TimeGroups.[1L],
      TimeGroups           = TimeGroups.,
      RollOnLag1           = TRUE,
      Type                 = 'Lag',
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
    if(length(HolidayVariable.) > 0L && length(HolidayLags.) > 0L && length(HolidayMovingAverages.) > 0L) {

      # Calendar and Holiday----
      if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
      if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE

      # Create copy of data
      temp <- AutoQuant:::CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
      Temporary1 <- temp$data
      keep <- temp$keep; rm(temp)

      # Generate GDL Features for Updated Records
      IndepVarPassTRUE. <- CARMA_Get_IndepentVariablesPass(HierarchGroups.)

      # Generate GDL Features for Updated Records
      Temporary1 <- Rodeo::AutoLagRollStatsScoring(

        # Data
        data                 = Temporary1,
        RowNumsID            = 'ID',
        RowNumsKeep          = 1,
        DateColumn           = eval(DateColumnName.),
        Targets              = 'HolidayCounts',
        HierarchyGroups      = NULL,
        IndependentGroups    = IndepVarPassTRUE.,

        # Services
        TimeUnit              = TimeUnit.,
        TimeUnitAgg          = TimeGroups.[1L],
        TimeGroups            = TimeGroups.[1L],
        TimeBetween          = NULL,
        RollOnLag1           = TRUE,
        Type                 = 'Lag',
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
      if(!'GroupVar' %chin% names(Temporary)) {
        keep <- c(eval(GroupVariables.),eval(DateColumnName.), setdiff(names(Temporary1), names(Temporary)))
        Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c(eval(GroupVariables.), eval(DateColumnName.)), all = FALSE)
      } else {
        keep <- c('GroupVar',eval(DateColumnName.), setdiff(names(Temporary1), names(Temporary)))
        Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c('GroupVar', eval(DateColumnName.)), all = FALSE)
      }
    }

    # Update data for scoring next iteration
    UpdateData. <- data.table::rbindlist(list(UpdateData.[ID != 1][, ID := NULL], Temporary), fill = TRUE, use.names = TRUE)

  } else if(!is.null(GroupVariables.) && Difference.) {

    # Calendar and Holiday
    if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
    if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE
    CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
  }

  # Group and No Diff
  if(!is.null(Lags.) && !is.null(GroupVariables.) && !Difference.) {

    # Calendar and Holiday
    if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
    if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE

    # Create data for GDL
    temp <- CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
    Temporary <- temp$data
    keep <- temp$keep; rm(temp)

    # Build Features
    Temporary <- Rodeo::AutoLagRollStatsScoring(

      # Data
      data                 = Temporary,
      RowNumsID            = 'ID',
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
      Type                 = 'Lag',
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
    if(length(HolidayVariable.) > 0L && length(HolidayLags.) > 0L && length(HolidayMovingAverages.) > 0L) {

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
      Temporary1 <- Rodeo::AutoLagRollStatsScoring(

        # Data
        data                 = Temporary1,
        RowNumsID            = 'ID',
        RowNumsKeep          = 1,
        DateColumn           = eval(DateColumnName.),
        Targets              = 'HolidayCounts',
        HierarchyGroups      = NULL,
        IndependentGroups    = IndepVarPassTRUE.,

        # Services
        TimeUnit             = TimeUnit.,
        TimeUnitAgg          = TimeGroups.[1L],
        TimeGroups           = TimeGroups.[1L],
        RollOnLag1           = TRUE,
        Type                 = 'Lag',
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
      if(ModelType %chin% c('catboost','h2o')) {
        if(!'GroupVar' %chin% names(Temporary)) {
          keep <- c(eval(GroupVariables.),eval(DateColumnName.), setdiff(names(Temporary1), names(Temporary)))
          if(eval(DateColumnName.) %chin% names(Temporary))
            Temporary <- Temporary[, .SD, .SDcols = unique(names(Temporary))]
            Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c(eval(GroupVariables.), eval(DateColumnName.)), all = FALSE)
        } else {
          keep <- c('GroupVar',eval(DateColumnName.),setdiff(names(Temporary1), names(Temporary)))
          Temporary <- Temporary[, .SD, .SDcols = unique(names(Temporary))]
          Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c('GroupVar', eval(DateColumnName.)), all = FALSE)
        }
      } else if(ModelType %chin% c('xgboost')) {
        if(!'GroupVar' %chin% names(Temporary)) {
          keep <- c(eval(GroupVariables.),eval(DateColumnName.), setdiff(names(Temporary1), names(Temporary)))
          Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c(eval(GroupVariables.), eval(DateColumnName.)), all = FALSE)
        } else {
          keep <- c('GroupVar',eval(DateColumnName.),setdiff(names(Temporary1), names(Temporary)))
          Temporary <- merge(Temporary, Temporary1[, .SD, .SDcols = c(keep)], by = c('GroupVar', eval(DateColumnName.)), all = FALSE)
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
    if('GroupVar' %chin% keep) keep <- keep[!keep %chin% 'GroupVar']

    # Generate GDL Features for Updated Records
    Temporary <- Rodeo::AutoLagRollStatsScoring(

      # Data
      data                 = Temporary,
      RowNumsID            = 'ID',
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
      Type                 = 'Lag',
      SimpleImpute         = TRUE,

      # Calculated Columns
      Lags                 = Lags.,
      MA_RollWindows       = MA_Periods.,
      ShortName            = TRUE,
      SD_RollWindows       = SD_Periods.,
      Skew_RollWindows     = Skew_Periods.,
      Kurt_RollWindows     = Kurt_Periods.,
      Quantile_RollWindows = Quantile_Periods.,
      Quantiles_Selected   = Quantiles_Selected.,
      Debug                = DebugMode.)

    Temporary <- Temporary[.N]

    # Lag / Lead, MA Holiday Variables
    if(length(HolidayVariable.) > 0L && length(HolidayLags.) > 0L && length(HolidayMovingAverages.) > 0L) {

      # Calendar and Holiday
      if(!is.null(CalendarVariables.)) CalVar <- TRUE else CalVar <- FALSE
      if(!is.null(HolidayVariable.)) HolVar <- TRUE else HolVar <- FALSE

      # Copy data
      temp <- CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
      Temporary1 <- temp$data
      keep <- temp$keep; rm(temp)

      # Generate GDL Features for Updated Records
      Temporary1 <- Rodeo::AutoLagRollStatsScoring(

        # Data
        data                 = Temporary1,
        RowNumsID            = 'ID',
        RowNumsKeep          = 1,
        DateColumn           = eval(DateColumnName.),
        Targets              = 'HolidayCounts',
        HierarchyGroups      = NULL,
        IndependentGroups    = NULL,

        # Services
        TimeUnit             = TimeUnit.,
        TimeUnitAgg          = TimeGroups.[1L],
        TimeGroups           = TimeGroups.[1L],
        TimeBetween          = NULL,
        RollOnLag1           = TRUE,
        Type                 = 'Lag',
        SimpleImpute         = TRUE,

        # Calculated Columns
        Lags                 = HolidayLags.,
        MA_RollWindows       = tryCatch({HolidayMovingAverages.[!HolidayMovingAverages. %in% 1]}, error = function(x) NULL),
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
    if(ModelType %chin% c('catboost','h2o')) {
      if(!'ID' %chin% c(names(UpdateData.))) data.table::set(UpdateData., j = 'ID', value = nrow(UpdateData.):1L)
      UpdateData. <- data.table::rbindlist(list(UpdateData.[ID > 1L][, ID := NULL], Temporary), fill = TRUE, use.names = TRUE)
    } else if(ModelType %chin% c('xgboost')) {
      if('ID' %chin% names(UpdateData)) {
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
    if(ModelType %chin% c('catboost','h2o')) {
      CarmaTimeSeriesScorePrep(UpdateData..=UpdateData., GroupVariables..=GroupVariables., DateColumnName..=DateColumnName.)
      UpdateData. <- UpdateData.[ID > 1L][, ID := NULL]
    }
  }

  # Return
  if(DebugMode.) print(UpdateData.)
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
#' @param MergeGroupVariablesBack Passthrough
#' @param Debug = FALSE
#'
#' @noRd
CarmaReturnDataPrep <- function(MaxDate. = NULL,
                                UpdateData. = NULL,
                                FutureDateData. = NULL,
                                dataStart. = NULL,
                                DateColumnName. = NULL,
                                TargetColumnName. = NULL,
                                GroupVariables. = NULL,
                                Difference. = NULL,
                                TargetTransformation. = NULL,
                                TransformObject. = NULL,
                                NonNegativePred. = NULL,
                                DiffTrainOutput. = NULL,
                                MergeGroupVariablesBack. = NULL,
                                Debug = FALSE) {

  if(Debug) print("CarmaReturnDataPrep 1")

  # Remove duplicate columns
  if(sum(names(UpdateData.) %chin% eval(DateColumnName.)) > 1) data.table::set(UpdateData., j = which(names(UpdateData.) %chin% eval(DateColumnName.))[2L], value = NULL)
  if(sum(names(UpdateData.) %chin% eval(TargetColumnName.)) > length(TargetColumnName.)) {
    x <- which(names(UpdateData.) %chin% eval(TargetColumnName.))
    x <- x[(length(TargetColumnName.) + 1L):length(x)]
    data.table::set(UpdateData., j = x, value = NULL)
  }

  if(Debug) print("CarmaReturnDataPrep 2")

  # Reverse Difference
  if(is.null(GroupVariables.) && Difference.) {

    print("UpdateData. before DifferenceDataReverse")
    print(UpdateData.)

    UpdateData. <- AutoQuant:::DifferenceDataReverse(
      UpdateData.,
      ScoreData = NULL,
      CARMA = TRUE,
      TargetCol = eval(TargetColumnName.),
      FirstRow = DiffTrainOutput.$FirstRow[[eval(TargetColumnName.)]],
      LastRow = NULL)

    print("UpdateData. after DifferenceDataReverse")
    print(UpdateData.)

  } else if(!is.null(GroupVariables.) && Difference.) {
    if(any(class(UpdateData.[[eval(DateColumnName.)]]) %chin% c('POSIXct','POSIXt')) && any(class(dataStart.[[eval(DateColumnName.)]]) == 'Date')) UpdateData.[, eval(DateColumnName.) := as.Date(get(DateColumnName.))]
    UpdateData. <- data.table::rbindlist(list(dataStart.,UpdateData.), fill = TRUE)
    UpdateData. <- UpdateData.[, .SD, .SDcols = c(eval(DateColumnName.),eval(TargetColumnName.),'Predictions','GroupVar')]
    data.table::set(UpdateData., i = c(which(is.na(UpdateData.[['Predictions']]))), j = 'Predictions', value = UpdateData.[which(is.na(UpdateData.[['Predictions']]))][[eval(TargetColumnName.)]])
    UpdateData.[, Predictionss := cumsum(Predictions), by = 'GroupVar']
    UpdateData.[, Predictions := Predictionss][, Predictionss := NULL]
    if(NonNegativePred.) UpdateData.[, Predictions := data.table::fifelse(Predictions < 0.5, 0, Predictions)]
  }

  if(Debug) print("CarmaReturnDataPrep 3")

  # BackTransform
  if(TargetTransformation.) {

    if(Debug) print("CarmaReturnDataPrep 4.1")

    if(length(TargetColumnName.) == 1L) {

      if(Debug) print("CarmaReturnDataPrep 4.2a")

      temptrans <- data.table::copy(TransformObject.)

      # If FALSE, then Method = 'Standardize'
      if('ColumnName' %in% names(TransformObject.)) {

        if(Debug) print("CarmaReturnDataPrep 4.3a")

        data.table::set(TransformObject., i = 1L, j = 'ColumnName', value = 'Predictions')
        TransformObject. <- data.table::rbindlist(list(temptrans, TransformObject.))

        if(Debug) print("CarmaReturnDataPrep 4.4")

        # Ensure positive values in case transformation method requires that
        if(Difference. && !is.null(GroupVariables.)) {
          UpdateData. <- UpdateData.[get(DateColumnName.) > eval(MaxDate.), eval(TargetColumnName.) := 1, by = 'GroupVar']
        }

        if(Debug) print("CarmaReturnDataPrep 4.5")

        # Backtrans
        UpdateData. <- Rodeo::AutoTransformationScore(
          ScoringData = UpdateData.,
          FinalResults = TransformObject.,
          Type = 'Inverse',
          TransID = NULL,
          Path = NULL)
        if(Debug) print("CarmaReturnDataPrep 4.6")

      } else {
        if(Debug) print("CarmaReturnDataPrep 4.3b")
        UpdateData. <- AutoQuant::StandardizeScoring(UpdateData., TransformObject., Apply = 'backtransform', GroupVars = GroupVariables.)
      }

    } else {

      if(Debug) print("CarmaReturnDataPrep 4.2b")

      for(zz in seq_along(TargetColumnName.)) {

        # One at a time
        temptrans <- data.table::copy(TransformObject.[[zz]])
        data.table::set(TransformObject.[[zz]], i = 1L, j = 'ColumnName', value = paste0('Predictions.V',zz))
        TransformObjectTemp <- data.table::rbindlist(list(temptrans,TransformObject.[[zz]]))

        # Ensure positive values in case transformation method requires so----
        if(Difference.) {
          if(!is.null(GroupVariables.) && TrainOnFull.) {
            UpdateData[!get(DateColumnName.) %in% FutureDateData., eval(TargetColumnName.) := 1, by = 'GroupVar']
          } else if(TrainOnFull) {
            UpdateData[!get(DateColumnName) %in% FutureDateData., eval(TargetColumnName.) := 1]
          }
        }

        # Backtrans----
        UpdateData. <- Rodeo::AutoTransformationScore(
          ScoringData = UpdateData.,
          FinalResults = TransformObjectTemp,
          Type = 'Inverse',
          TransID = NULL,
          Path = NULL)
      }
    }
  }

  if(Debug) print("CarmaReturnDataPrep 4.7")

  # Remove target variables values on FC periods
  UpdateData. <- UpdateData.[get(DateColumnName.) > eval(MaxDate.), eval(TargetColumnName.) := NA]

  if(Debug) print("CarmaReturnDataPrep 4.8")

  # Return data prep
  if(!is.null(GroupVariables.)) {
    keep <- c('GroupVar', eval(DateColumnName.), eval(TargetColumnName.), names(UpdateData.)[which(names(UpdateData.) %like% 'Predictions')])
    UpdateData. <- UpdateData.[, ..keep]

    if(length(MergeGroupVariablesBack.) > 0L) {

      if(length(GroupVariables.) > 1L) {
        UpdateData.[MergeGroupVariablesBack., on = .(GroupVar), paste0(GroupVariables.) := mget(paste0('i.', GroupVariables.))]
      } else {
        UpdateData.[MergeGroupVariablesBack., on = .(GroupVar), paste0(GroupVariables.) := get(paste0('i.', GroupVariables.))]
      }

    } else {

      if(length(GroupVariables.) > 1L) {
        UpdateData.[, eval(GroupVariables.) := data.table::tstrsplit(GroupVar, ' ')][, GroupVar := NULL]
      } else {
        UpdateData.[, eval(GroupVariables.) := GroupVar][, GroupVar := NULL]
      }
    }
  }

  if(Debug) print("CarmaReturnDataPrep 4.9")

  # Return data
  return(list(UpdateData = UpdateData., TransformObject = TransformObject.))
}

#' @param GroupVariables. Passthrough
#' @param Difference. Passthrough
#' @param Step1SCore. Passthrough
#'
#' @noRd
CarmaRecordCount <- function(GroupVariables. = NULL,
                             Difference. = NULL,
                             Step1SCore. = NULL) {
  if(!is.null(GroupVariables.)) {
    if(Difference.) {
      if(!'GroupVar' %chin% names(Step1SCore.)) N. <- as.integer(Step1SCore.[, .N, by = c(eval(GroupVariables.))][, max(N)]) else N. <- as.integer(Step1SCore.[, .N, by = 'GroupVar'][, max(N)])
    } else {
      N. <- as.integer(Step1SCore.[, .N, by = 'GroupVar'][, max(N)])
    }
  } else {
    N. <- as.integer(Step1SCore.[, .N])
  }
  return(N.)
}

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
                           ColumnsToDiff = NULL,
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
    DiffData <- cbind(data[2L:nrow(data),.SD, .SDcols = c(setdiff(names(data),ColumnsToDiff))],data[, lapply(.SD,diff), .SDcols = ColumnsToDiff])
  }

  # Return data
  if(!CARMA) {
    return(list(DiffData = DiffData, FirstRow = FirstRow, LastRow = data[nrow(data)]))
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
                                  ScoreData = NULL,
                                  LastRow = NULL,
                                  CARMA = FALSE,
                                  TargetCol = NULL,
                                  FirstRow = NULL,
                                  GroupingVariables = NULL) {

  if(!CARMA) {
    if(is.null(GroupingVariables)) {
      return(data[, Predictions := cumsum(c(LastRow,ScoreData))])
    }
  } else {
    if(is.null(GroupingVariables)) {

      print("FirstRow")
      print(FirstRow)

      x <- cumsum(c(FirstRow,data[[eval(TargetCol)]]))

      print("cumsum")
      print(x)

      # XGBoostCARMA matches, catboostCARMA is off by 1
      if(length(x) != data[,.N]) x <- x[-1L]
      data[, eval(TargetCol) := x][, Predictions := x]
      return(data)
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
FullFactorialCatFeatures <- function(GroupVars = NULL,
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
              temp2 <- paste(temp2,temp[l,k], sep = '_')
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
            temp2 <- paste(temp2,temp[m], sep = '_')
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
              temp2 <- paste(temp2,temp[l,k], sep = '_')
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
            temp2 <- paste(temp2,temp[m], sep = '_')
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
CARMA_GroupHierarchyCheck <- function(data = NULL,
                                      Group_Variables = NULL,
                                      HierarchyGroups = NULL) {

  # Simple organization of option sets
  if(length(Group_Variables) > 1 && !is.null(HierarchyGroups)) {
    if('GroupVar' %chin% names(data)) data[, eval(Group_Variables) := data.table::tstrsplit(GroupVar, ' ')][, GroupVar := NULL]
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
