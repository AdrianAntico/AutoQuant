#' @param Indicator Passthrough
#' @param ArgsList Passthrough
#'
#' @noRd
FeatureTuneUpdate <- function(Indicator = 1L,
                              ArgsList) {
  if(Indicator == 1L) {
    FeatureSets <- names(ArgsList$FE_Columns)[Indicator]
  } else if(Indicator == max(seq_along(ArgsList$FE_Columns))) {
    FeatureSets <- c()
    for(z in seq_along(ArgsList$FE_Columns)) {
      FeatureSets <- c(FeatureSets, names(ArgsList$FE_Columns)[z])
    }
  } else if(Indicator <= max(seq_along(ArgsList$FE_Columns))) {
    FeatureSets <- c()
    for(z in seq_along(ArgsList$FE_Columns)) {
      if(z != Indicator) {
        FeatureSets <- c(FeatureSets, names(ArgsList$FE_Columns)[z])
      }
    }
  }
  return(FeatureSets)
}

#' @param RunNumber Passthrough
#' @param Loop Passthrough
#' @param FeatureTune. Passthrough
#' @param GridTune. Passthrough
#' @param UseGrid. Passthrough
#' @param ArgsList Passthrough
#' @param TrainData. Passthrough
#'
#' @noRd
FeaturePrep <- function(RunNumber = 1,
                        Loop = 1,
                        FeatureTune. = FALSE,
                        GridTune. = FALSE,
                        UseGrid. = FALSE,
                        ArgsList = NULL,
                        TrainData. = NULL) {

  # Colnames
  TrainNames <- names(TrainData.)

  # Define features to use in training ----
  if(FeatureTune. && RunNumber == 1) {

    # Base features only
    if(is.null(ArgsList$FE_Columns_Mod)) {
      Features <- ArgsList$FE_Columns[[RunNumber]]
    } else {
      Features <- ArgsList$FE_Columns_Mod[[RunNumber]]
    }

    # Modified IDcols
    IDcols. <- setdiff(TrainNames, Features)
    IDcols. <- IDcols.[!IDcols. %chin% ArgsList$Data$TargetVariables]

    # Grid Options
    PassInGrid. <- NULL

  } else if(FeatureTune. && RunNumber <= max(Loop)) {

    # All FE Services
    Features <- c()
    if(is.null(ArgsList$FE_Columns_Mod)) {
      for(z in seq_along(ArgsList$FE_Columns)) {
        if(z == RunNumber) {
          next
        } else {
          if(length(ArgsList$FE_Columns[[z]]) == 1L) {
            Features <- c(Features, ArgsList$FE_Columns[[z]])
          } else {
            for(zz in seq_along(ArgsList$FE_Columns[[z]])) {
              Features <- c(Features, ArgsList$FE_Columns[[z]][[zz]])
            }
          }
        }
      }
    } else {
      for(z in seq_along(ArgsList$FE_Columns_Mod)) {
        if(z == RunNumber) {
          next
        } else {
          if(length(ArgsList$FE_Columns_Mod[[z]]) == 1L) {
            Features <- c(Features, ArgsList$FE_Columns_Mod[[z]])
          } else {
            for(zz in seq_along(ArgsList$FE_Columns_Mod[[z]])) {
              Features <- c(Features, ArgsList$FE_Columns_Mod[[z]][[zz]])
            }
          }
        }
      }
    }

    # Modified IDcols
    IDcols. <- setdiff(TrainNames, Features)
    IDcols. <- IDcols.[!IDcols. %chin% ArgsList$Data$TargetVariables]

    # Grid Options
    PassInGrid. <- NULL

  } else if(GridTune.) {

    # All FE Services
    Features <- c()
    if(is.null(ArgsList$FE_Columns_Mod)) {
      for(z in seq_along(ArgsList$FE_Columns)) {
        if(length(ArgsList$FE_Columns[[z]]) == 1L) {
          Features <- c(Features, ArgsList$FE_Columns[[z]])
        } else {
          for(zz in seq_along(ArgsList$FE_Columns[[z]])) {
            Features <- c(Features, ArgsList$FE_Columns[[z]][[zz]])
          }
        }
      }
    } else {
      for(z in seq_along(ArgsList$FE_Columns_Mod)) {
        if(length(ArgsList$FE_Columns_Mod[[z]]) == 1L) {
          Features <- c(Features, ArgsList$FE_Columns_Mod[[z]])
        } else {
          for(zz in seq_along(ArgsList$FE_Columns_Mod[[z]])) {
            Features <- c(Features, ArgsList$FE_Columns_Mod[[z]][[zz]])
          }
        }
      }
    }

    # Modified IDcols
    IDcols. <- setdiff(TrainNames, Features)
    IDcols. <- IDcols.[!IDcols. %chin% ArgsList$Data$TargetVariables]

    # Grid Options
    PassInGrid. <- NULL

  } else {

    # All FE Services
    Features <- c()
    if(is.null(ArgsList$FE_Columns_Mod)) {
      for(z in seq_along(ArgsList$FE_Columns)) {
        if(length(ArgsList$FE_Columns[[z]]) == 1L) {
          Features <- c(Features, ArgsList$FE_Columns[[z]])
        } else {
          for(zz in seq_along(ArgsList$FE_Columns[[z]])) {
            Features <- c(Features, ArgsList$FE_Columns[[z]][[zz]])
          }
        }
      }
    } else {
      for(z in seq_along(ArgsList$FE_Columns_Mod)) {
        if(length(ArgsList$FE_Columns_Mod[[z]]) == 1L) {
          Features <- c(Features, ArgsList$FE_Columns_Mod[[z]])
        } else {
          for(zz in seq_along(ArgsList$FE_Columns_Mod[[z]])) {
            Features <- c(Features, ArgsList$FE_Columns_Mod[[z]][[zz]])
          }
        }
      }
    }

    # Modified IDcols
    IDcols. <- setdiff(TrainNames, Features)
    IDcols. <- IDcols.[!IDcols. %chin% ArgsList$Data$TargetVariables]

    # Grid options
    if(!UseGrid. || !file.exists(file.path(ArgsList$MetaData$Results_Path, "PassInGrid.csv"))) {
      PassInGrid. <- NULL
    } else {
      PassInGrid. <- data.table::fread(file = file.path(ArgsList$MetaData$Results_Path, "PassInGrid.csv"))
    }
  }

  # Return
  return(list(Features = Features, IDcols = IDcols., PassInGrid = PassInGrid.))
}

#' @param ArgsList Passthrough
#' @param FeatureTuning Passthrough
#' @param GridTuning Passthrough
#' @param Trees Passthrough
#' @param Tree_Tuning Passthrough
#' @param Depth Passthrough
#' @param Depth_Tuning Passthrough
#' @param LearningRate Passthrough
#' @param LearningRate_Tuning Passthrough
#' @param L2_Leaf_Reg Passthrough
#' @param L2_Leaf_Reg_Tuning Passthrough
#' @param RandomStrength Passthrough
#' @param RandomStrength_Tuning Passthrough
#' @param BorderCount Passthrough
#' @param BorderCount_Tuning Passthrough
#' @param RSM Passthrough
#' @param RSM_Tuning Passthrough
#' @param BootStrapType Passthrough
#' @param BootStrapType_Tuning Passthrough
#' @param GrowPolicy Passthrough
#' @param GrowPolicy_Tuning Passthrough
#'
#' @noRd
GenerateArgs <- function(ArgsList = NULL,
                         FeatureTuning = FALSE,
                         GridTuning = FALSE,
                         Trees = 2000,
                         Tree_Tuning = seq(1000,5000,500),
                         Depth = 9,
                         Depth_Tuning = seq(4,10,1),
                         LearningRate = NULL,
                         LearningRate_Tuning = seq(0.05,0.50,0.05),
                         L2_Leaf_Reg = NULL,
                         L2_Leaf_Reg_Tuning = seq(0.0,5.0,1.0),
                         RandomStrength = 1,
                         RandomStrength_Tuning = seq(0.80,1,0.05),
                         BorderCount = 254,
                         BorderCount_Tuning = seq(32,256,32),
                         RSM = 1.0,
                         RSM_Tuning = seq(0.10,1.0,0.10),
                         BootStrapType = "MVS",
                         BootStrapType_Tuning = c("MVS","Bayesian","Bernoulli","No"),
                         GrowPolicy = "SymmetricTree",
                         GrowPolicy_Tuning = c("SymmetricTree", "Lossguide", "Depthwise")) {

  # Feature Tuning Run
  if(FeatureTuning) {

    # Feature Engineering Services
    ServicesCheck <- ArgsList$Services

    # Args Setup
    GridTune. <- FALSE
    Trees. <- ArgsList$Modeling$Trees
    Depth. <- ArgsList$Modeling$Depth
    LearningRate. <- ArgsList$Modeling$LearningRate
    L2_Leaf_Reg. <- ArgsList$Modeling$L2_Leaf_Reg
    RandomStrength. <- ArgsList$Modeling$RandomStrength
    BorderCount. <- ArgsList$Modeling$BorderCount
    RSM. <- ArgsList$Modeling$RSM
    BootStrapType. <- ArgsList$Modeling$BootStrapType
    GrowPolicy. <- ArgsList$Modeling$GrowPolicy

    # Iterator Update
    Loop <- seq_along(ArgsList$FE_Columns)
    if(!("BaseColumns" %in% names(ArgsList$FE_Columns) && length(names(ArgsList$FE_Columns)) == 1L)) {
      Loop <- c(Loop, max(Loop) + 1L)
    }
  }

  # Grid Tuning Run
  if(GridTuning && !FeatureTuning) {

    # Feature Engineering Services
    ServicesCheck <- ArgsList$Services

    # Args Setup
    GridTune. <- TRUE
    Trees. <- ArgsList$Modeling$Tree_Tuning
    Depth. <- ArgsList$Modeling$Depth_Tuning
    LearningRate. <- ArgsList$Modeling$LearningRate_Tuning
    L2_Leaf_Reg. <- ArgsList$Modeling$L2_Leaf_Reg_Tuning
    RandomStrength. <- ArgsList$Modeling$RandomStrength_Tuning
    BorderCount. <- ArgsList$Modeling$BorderCount_Tuning
    RSM. <- ArgsList$Modeling$RSM_Tuning
    BootStrapType. <- ArgsList$Modeling$BootStrapType_Tuning
    GrowPolicy. <- ArgsList$Modeling$GrowPolicy_Tuning

    # Update iterator
    Loop <- 1L
  }

  # Regular Run
  if(!any(FeatureTuning, GridTuning)) {

    # Feature Engineering Services
    ServicesCheck <- ArgsList$Services

    # Args Setup
    GridTune. <- FALSE
    Trees. <- ArgsList$Modeling$Trees
    Depth. <- ArgsList$Modeling$Depth
    LearningRate. <- ArgsList$Modeling$LearningRate
    L2_Leaf_Reg. <- ArgsList$Modeling$L2_Leaf_Reg
    RandomStrength. <- ArgsList$Modeling$RandomStrength
    BorderCount. <- ArgsList$Modeling$BorderCount
    RSM. <- ArgsList$Modeling$RSM
    BootStrapType. <- ArgsList$Modeling$BootStrapType
    GrowPolicy. <- ArgsList$Modeling$GrowPolicy

    # Iterator Update
    Loop <- 1L
  }

  # Return
  return(list(
    ServicesCheck = ServicesCheck,
    Loop = Loop,
    GridTune = GridTune.,
    Trees = Trees.,
    Depth = Depth.,
    LearningRate = LearningRate.,
    L2_Leaf_Reg = L2_Leaf_Reg.,
    RandomStrength = RandomStrength.,
    BorderCount = BorderCount.,
    RSM = RSM.,
    BootStrapType = BootStrapType.,
    GrowPolicy = GrowPolicy.))
}

#' @param TargetType = "classification"
#' @param Results. Passthrough
#' @param ArgsList Passthrough
#' @param FeatureTune. Passthrough
#' @param GridTune. Passthrough
#' @param LeaveOneOut. Passthrough
#' @param MetricName Passthrough
#'
#' @noRd
ML_TunePath <- function(TargetType = "classification",
                        Results. = Results,
                        ArgsList = NULL,
                        FeatureTune. = FeatureTune,
                        GridTune. = GridTune,
                        LeaveOneOut. = LeaveOneOut,
                        MetricName = "Utility") {
  if(FeatureTune.) {
    data <- data.table::fread(file = file.path(ArgsList$MetaData[["Results_Path"]], paste0(ArgsList$MetaData[["ProjectID"]], "_FeatureTuningMetrics.csv")))
    if("RunTime" %chin% names(data) && !is.null(ArgsList$UpdateTS)) data <- data[RunTime > ArgsList$UpdateTS]
    if(TargetType == "classification") {
      data <- data[, MaxVal := max(get(MetricName)), by = "RunNumber"]
      data <- data[, MaxVal := max(get(MetricName), na.rm = TRUE), by = "RunNumber"][order(RunNumber,-get(MetricName))][, ID := seq_len(.N), by = "RunNumber"]
      data <- data[ID == 1L][, ":=" (MaxVal = NULL, ID = NULL)]
    } else {
      FE_Winner <- data[Metric == eval(MetricName)]
      FE_Winner <- data[order(-MetricValue)][1L, RunNumber]
      FeatureSets <- FeatureTuneUpdate(Indicator = FE_Winner, ArgsList = ArgsList)
      ArgsList$FeatureSets <- unique(c(FeatureSets, "DataPartition"))
      ArgsList$FE_Columns_Mod <- ArgsList$FE_Columns[which(names(ArgsList$FE_Columns) %chin% c("BaseColumns", "DataPartition", paste0("FE_", ArgsList$FeatureSets)))]
      ArgsList$Data$IDVariables <- c(ArgsList$Data$IDVariables, setdiff(ArgsList$Services, ArgsList$FeatureSets))
      data <- data[, MaxVal := max(MetricValue, na.rm = TRUE), by = "RunNumber"][
        MetricValue == MaxVal][
          , ID := seq_len(.N), by = "RunNumber"][
            ID == 1][
              , ":=" (ID = NULL, MaxVal = NULL)][
                order(-MetricValue)]
    }
    data.table::fwrite(data, file = file.path(ArgsList$MetaData$Results_Path, "FE_TopPerformers.csv"), append = TRUE)
    LeaveOneOut. <- LeaveOneOut. - 1L
  } else if(GridTune.) {
    GridTune. <- FALSE
    GridMetrics <- Results.$GridMetrics
    if(!is.null(GridMetrics)) {
      GridMetrics[, RunDateTime := Sys.time()]
      data.table::fwrite(GridMetrics, file = file.path(ArgsList$MetaData$Results_Path, paste0(ArgsList$MetaData$ProjectID, "_GridMetrics.csv")), append = TRUE)
      PassInGrid. <- GridMetrics[1L]
      data.table::fwrite(PassInGrid., file = file.path(ArgsList$MetaData$Results_Path, paste0(ArgsList$MetaData$ProjectID, "_PassInGrid.csv")))
    }
  }
  return(list(
    ArgsList = ArgsList,
    FeatureTune = FeatureTune.,
    GridTune = GridTune.,
    LeaveOneOut = LeaveOneOut.))
}

#' @title MEOW
#'
#' @description Mixed Effects Offset Weighting
#'
#' @author Adrian Antico
#' @family Mixed Effects
#'
#' @param data data.table object
#' @param TargetVariable Colname
#' @param TargetType 'regression', 'classification', 'multiclass'
#' @param RandomEffects Colnames
#' @param FixedEffects Intended for predicted values from another model. Can use other variables, however
#' @param Nest Specify the outer and inner variable numbers. Function checks for successive numbers (1,2), (2,3), ... (N-1,N)
#' @param CollapseEPV CollapseEPV == TRUE gives you a constant EPV, otherwise it remains varied.
#' @param KeepSubStats FALSE. Set to TRUE to return intermediary statistics used in calculations
#' @param Fme Logical. Fast mixed effects predictions
#' @param Lmer Set to TRUE to use lmer to build the mixed effects model
#' @param LmerType Choose from 'add', 'add_nest', 'int_nest', or 'nest'
#' @param PolyN NULL. Add a numeric value to use poly(FixedEffect, PolyN) in the mixed effects adjustment for fixed effects. Only allowed when length(FixedEffects) == 1
#' @param Debug = FALSE
#'
#' @examples
#' \dontrun{
#' # Pull data and set vars to factors
#' data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/gpa.csv"); data[, student := as.factor(student)];data[, semester := as.factor(semester)]
#' data[, year := as.factor(year)]
#' data[, student := as.factor(student)]
#' data[, semester := as.factor(semester)]
#' data[, Mean := mean(gpa), by = c('sex','semester','year','student')]
#'
#' # Run function
#' TestModel <- RemixAutoML::AutoCatBoostRegression(
#'
#'   # GPU or CPU and the number of available GPUs
#'   task_type = 'GPU', NumGPUs = 1,
#'   OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData'),
#'   ReturnModelObjects = TRUE,
#'
#'   # Data args
#'   data = data, model_path = getwd(),
#'   TargetColumnName = 'gpa',
#'   FeatureColNames = c('sex','semester','year'),
#'   IDcols = setdiff(names(data), c('sex','semester','year','gpa')),
#'
#'   # ML args
#'   Trees = 250, RandomStrength = 1, RSM = 1, GrowPolicy = 'SymmetricTree',
#'   Depth = 9,
#'   L2_Leaf_Reg = 10,
#'   model_size_reg = 2)
#'
#' Output <- RemixAutoML:::MEOW(
#'   data = TestModel$TrainData,
#'   TargetType = 'regression',
#'   TargetVariable = TestModel$ArgsList$TargetColumnName,
#'   RandomEffects = 'student', # c('sex','semester','year','student'),
#'   FixedEffects = 'Predict',
#'   Nest = NULL,
#'   CollapseEPV = TRUE,
#'   KeepSubStats = FALSE,
#'   Lmer = TRUE,
#'   LmerType = 'add',
#'   Fme = TRUE,
#'   PolyN = NULL,
#'   Debug = FALSE)
#'
#' Output$DensityPlot()
#'
#' library(data.table)
#' library(collapse)
#' data = TestModel$TrainData
#' TargetType = 'regression'
#' TargetVariable = TestModel$ArgsList$TargetColumnName
#' RandomEffects = 'student'
#' FixedEffects = 'Predict'
#' Nest = NULL
#' CollapseEPV = TRUE
#' KeepSubStats = FALSE
#' Fme = TRUE
#' Lmer = TRUE
#' LmerType = 'add'
#' Pme = TRUE
#' PolyN = NULL
#' Debug = FALSE
#' }
#'
#' @noRd
MEOW <- function(data = NULL,
                 TargetType = 'regression',
                 TargetVariable = NULL,
                 RandomEffects = NULL,
                 FixedEffects = NULL,
                 PolyN = NULL,
                 Lmer = FALSE,
                 LmerType = 'add',
                 Fme = TRUE,
                 Nest = NULL,
                 CollapseEPV = TRUE,
                 KeepSubStats = FALSE,
                 Debug = FALSE) {

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Rename variables to reduce run times. Switch back at end
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  library(collapse); library(magrittr); dataNames <- data.table::copy(names(data))
  if(length(RandomEffects) > 1L) {
    old <- c(TargetVariable, RandomEffects); new <- c('TargetVar', paste0('RE', seq_along(RandomEffects)))
    data.table::setnames(data, old, new)
    GroupVarsNew <- RandomEffects
    RandomEffectsOld <- RandomEffects
    RandomEffects <- paste0("RE", seq_along(RandomEffects))
    old <- c('TargetVar', paste0("RE", seq_along(RandomEffects)), paste0('RE', seq_along(RandomEffects), "_MixedEffects"))
    new <- c(TargetVariable, GroupVarsNew, paste0(GroupVarsNew, "_MixedEffects"))
  } else {
    old <- c(TargetVariable, RandomEffects); new <- c('TargetVar', paste0('RE', seq_along(RandomEffects)))
    data.table::setnames(data, old, new)
    GroupVarsNew <- RandomEffects
    RandomEffectsOld <- RandomEffects
    RandomEffects <- paste0("RE", seq_along(RandomEffects))
    old <- c('TargetVar', paste0("RE", seq_along(RandomEffects)), paste0('RE', seq_along(RandomEffects), "_MixedEffects"))
    new <- c(TargetVariable, GroupVarsNew, paste0(GroupVarsNew, "_MixedEffects"))
  }

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Nested Random Effects
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  # Build Nested Random Effects
  for(i in seq_along(RandomEffects)) {

    # i = 1    i = 2
    # 1. Prior Stats; # 2. Posterior Stats; # 3. Merge prior to data (data holds posterior stats)
    if(i == 1L) {

      # Prior Stats ----
      data.table::setkey(x = data, cols = RE1)
      if(tolower(TargetType) == 'classification') {
        DT <- list()
        DT[['RE1']] <- collapse::funique(data$RE1)
        DT[['RE1_Mean']] <- (data %>% gby(RE1) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE1_N']] <- (data %>% gby(RE1) %>% gv('TargetVar') %>% fnobs)[[2L]]
        GroupMean <- data.table::setkey(data.table::setDT(DT), RE1)
        rm(DT); gc()
        GroupMean[, RE1_EPV := RE1_Mean * (1 - RE1_Mean) / RE1_N]
      } else if(tolower(TargetType) == 'regression') {
        DT <- list()
        DT[['RE1']] <- collapse::funique(data$RE1)
        DT[['RE1_Mean']] <- (data %>% gby(RE1) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE1_N']] <- (data %>% gby(RE1) %>% gv('TargetVar') %>% fnobs)[[2L]]
        DT[['RE1_EPV']] <- (data %>% gby(RE1) %>% gv('TargetVar') %>% fvar)[[2L]]
        GroupMean <- data.table::setkey(data.table::setDT(DT), RE1)
        rm(DT); gc()
      } else if(tolower(TargetType) == 'multiclass') {
        GroupMean <- data[, list(RE1_N = .N), by = c('TargetVar', "RE1")]
        GroupMean[, GrandSum := sum(RE1_N)]
        GroupMean[, TargetSum := sum(RE1_N), by = 'TargetVar']
        GroupMean[, TargetMean := TargetSum / GrandSum]
        GroupMean[, TargetGroupMean := N / TargetSum]
        GroupMean[, TargetVariance := TargetMean * (1 - TargetMean) / TargetSum]
        GroupMean[, TargetGroupVariance := TargetGroupMean * (1 - TargetGroupMean) / RE1_N]
        GroupMean[, Z := TargetGroupVariance / (TargetGroupVariance + TargetVariance)]
        GroupMean[, RE1_MixedEffects := (1 - Z) * TargetGroupMean + Z * TargetMean]
        GroupMean[, (setdiff(names(GroupMean), c("RE1_MixedEffects", 'TargetVar', 'RE1'))) := NULL]
        GroupMean <- data.table::dcast.data.table(data = GroupMean, formula = RE1 ~ get(TargetVariable), fun.aggregate = sum, value.var = "RE1_MixedEffects", fill = 0)
        data.table::setnames(x = GroupMean, names(GroupMean), c('RE1', paste0("RE1_MixedEffects_TargetLevel_", names(GroupMean)[-1L])))
        data.table::setkeyv(GroupMean, cols = 'RE1')
      }

      # Merge prior ----
      x <- data %>% gv('TargetVar') %>% fmean # 4x faster than GroupMean[, GrandMean := mean(data$TargetVar, na.rm = TRUE)]
      GroupMean[, RE1_GrandMean := x]; rm(x)

      if(CollapseEPV) {
        GroupMean[, RE1_EPV := collapse::fmean(RE1_EPV, na.rm = TRUE)]
      }

      # Posterior
      GroupMean[, RE1_VHM := collapse::fsum((RE1_Mean - RE1_GrandMean) ^ 2) / (.N-1) - RE1_EPV / RE1_N]
      data.table::set(GroupMean, i = which(GroupMean[['RE1_VHM']] < 0), j = 'RE1_VHM', value = 0)
      GroupMean[, RE1_K := RE1_EPV / RE1_VHM]
      GroupMean[, RE1_Z := RE1_N / (RE1_N + RE1_K)]
      GroupMean[, paste0(RandomEffects[i], "_MixedEffects") := RE1_Z * RE1_Mean + (1 - RE1_Z) * RE1_GrandMean]
      if(KeepSubStats && tolower(TargetType) != 'multiclass') {
        nam <- names(GroupMean)[-1L]
        data[GroupMean, paste0(nam) := mget(paste0("i.", nam))]
      } else if(tolower(TargetType) %in% c('regression','classification')) {
        data[GroupMean, RE1_MixedEffects := i.RE1_MixedEffects]
      } else {
        data[GroupMean, eval(names(GroupMean)[!names(GroupMean) %chin% 'RE1']) := mget(paste0("i.", names(GroupMean)[!names(GroupMean) %chin% 'RE1']))]
      }

    } else if(i == 2L) {

      # Prior Stats ----
      GrandMean <- data %>% gby(RE2) %>% gv('RE1_MixedEffects') %>% fmean
      data.table::setkeyv(GrandMean, 'RE2')
      data.table::setkeyv(x = data, cols = 'RE2')
      data.table::setnames(GrandMean, 'RE1_MixedEffects', 'RE2_GrandMean')

      # Posterior Stats ----
      DT <- list()
      DT[['RE2']] <- funique(data[['RE2']])
      if(all(c(1,2) %in% Nest)) {
        DT[['RE2_Mean']] <- ((data %>% gby(RE1,RE2) %>% gv('TargetVar') %>% fmean) %>% gby(RE2) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE2_N']] <- ((data %>% gby(RE1,RE2) %>% gv('TargetVar') %>% fnobs) %>% gby(RE2) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE2)
          rm(DT); gc()
          GroupMean[, RE2_EPV := RE2_Mean * (1 - RE2_Mean) / RE2_N]
        } else {
          DT[['RE2_EPV']] <- ((data %>% gby(RE1,RE2) %>% gv('TargetVar') %>% fmean) %>% gby(RE2) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE2)
          rm(DT); gc()
        }

      } else {

        DT[['RE2_Mean']] <- (data %>% gby(RE2) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE2_N']] <- (data %>% gby(RE2) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE2)
          rm(DT); gc()
          GroupMean[, RE2_EPV := RE2_Mean * (1 - RE2_Mean) / RE2_N]
        } else {
          DT[['RE2_EPV']] <- (data %>% gby(RE2) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE2)
          rm(DT); gc()
        }
      }

      # Merge Prior to data ----
      GroupMean[GrandMean, RE2_GrandMean := i.RE2_GrandMean]

      if(CollapseEPV) {
        GroupMean[, RE2_EPV := collapse::fmean(RE2_EPV, na.rm = TRUE)]
      }

      GroupMean[, RE2_VHM := collapse::fsum((RE2_Mean - RE2_GrandMean) ^ 2) / (.N-1) - RE2_EPV / RE2_N]
      data.table::set(GroupMean, i = which(GroupMean[['RE2_VHM']] < 0), j = 'RE2_VHM', value = 0)
      GroupMean[, RE2_K := RE2_EPV / RE2_VHM]
      GroupMean[, RE2_Z := RE2_N / (RE2_N + RE2_K)]
      GroupMean[, paste0(RandomEffects[i], "_MixedEffects") := RE2_Z * RE2_Mean + (1 - RE2_Z) * RE2_GrandMean]
      if(KeepSubStats) {
        nam <- names(GroupMean)[-1L]
        data[GroupMean, paste0(nam) := mget(paste0("i.", nam))]
      } else {
        data[GroupMean, RE2_MixedEffects := i.RE2_MixedEffects]
      }

    } else if(i == 3L) {

      # Prior Stats ----
      GrandMean <- data %>% gby(RE3) %>% gv('RE2_MixedEffects') %>% fmean
      data.table::setkey(GrandMean, RE3)
      data.table::setnames(GrandMean, 'RE2_MixedEffects', 'RE3_GrandMean')
      data.table::setkey(x = data, cols = RE3)

      # Posterior Stats
      DT <- list()
      DT[['RE3']] <- unique(data$RE3)
      if(all(c(2,3) %in% Nest)) {
        DT[['RE3_Mean']] <- ((data %>% gby(RE2,RE3) %>% gv('TargetVar') %>% fmean) %>% gby(RE3) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE3_N']] <- ((data %>% gby(RE2,RE3) %>% gv('TargetVar') %>% fnobs) %>% gby(RE3) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE3)
          rm(DT); gc()
          GroupMean[, RE3_EPV := RE3_Mean * (1 - RE3_Mean) / RE3_N]
        } else {
          DT[['RE3_EPV']] <- ((data %>% gby(RE2,RE3) %>% gv('TargetVar') %>% fmean) %>% gby(RE3) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE3)
          rm(DT); gc()
        }
      } else {
        DT[['RE3_Mean']] <- (data %>% gby(RE3) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE3_N']] <- (data %>% gby(RE3) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE3)
          rm(DT); gc()
          GroupMean[, RE3_EPV := RE3_Mean * (1 - RE3_Mean) / RE3_N]
        } else {
          DT[['RE3_EPV']] <- (data %>% gby(RE3) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE3)
          rm(DT); gc()
        }
      }

      # Merge Prior to data ----
      GroupMean[GrandMean, RE3_GrandMean := i.RE3_GrandMean]

      if(CollapseEPV) {
        GroupMean[, RE3_EPV := collapse::fmean(RE3_EPV, na.rm = TRUE)]
      }

      # Set to zero if negative
      GroupMean[, RE3_VHM := collapse::fsum((RE3_Mean - RE3_GrandMean) ^ 2) / (.N-1) - RE3_EPV / RE3_N]
      data.table::set(GroupMean, i = which(GroupMean[['RE3_VHM']] < 0), j = 'RE3_VHM', value = 0)
      GroupMean[, RE3_K := RE3_EPV / RE3_VHM]
      GroupMean[, RE3_Z := RE3_N / (RE3_N + RE3_K)]
      GroupMean[, paste0(RandomEffects[i], "_MixedEffects") := RE3_Z * RE3_Mean + (1 - RE3_Z) * RE3_GrandMean]
      if(KeepSubStats) {
        nam <- names(GroupMean)[-1L]
        data[GroupMean, paste0(nam) := mget(paste0("i.", nam))]
      } else {
        data[GroupMean, RE3_MixedEffects := i.RE3_MixedEffects]
      }

    } else if(i == 4L) {

      # Prior Stats ----
      GrandMean <- data %>% gby(RE4) %>% gv('RE2_MixedEffects') %>% fmean
      data.table::setkey(GrandMean, RE4)
      data.table::setnames(GrandMean, 'RE3_MixedEffects', 'RE4_GrandMean')
      data.table::setkey(x = data, cols = RE4)

      # Posterior Stats
      DT <- list()
      DT[['RE4']] <- unique(data$RE4)

      if(all(c(3,4) %in% Nest)) {
        DT[['RE4_Mean']] <- ((data %>% gby(RE3,RE4) %>% gv('TargetVar') %>% fmean) %>% gby(RE4) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE4_N']] <- ((data %>% gby(RE3,RE4) %>% gv('TargetVar') %>% fnobs) %>% gby(RE4) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE4)
          rm(DT); gc()
          GroupMean[, RE4_EPV := RE4_Mean * (1 - RE4_Mean) / RE4_N]
        } else {
          DT[['RE4_EPV']] <- ((data %>% gby(RE3,RE4) %>% gv('TargetVar') %>% fmean) %>% gby(RE4) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE4)
          rm(DT); gc()
        }

      } else {

        DT[['RE4_Mean']] <- (data %>% gby(RE4) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE4_N']] <- (data %>% gby(RE4) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE4)
          rm(DT); gc()
          GroupMean[, RE4_EPV := RE4_Mean * (1 - RE4_Mean) / RE4_N]
        } else {
          DT[['RE4_EPV']] <- (data %>% gby(RE4) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE4)
          rm(DT); gc()
        }
      }

      # Merge Prior to data ----
      GroupMean[GrandMean, RE4_GrandMean := i.RE4_GrandMean]

      if(CollapseEPV) {
        GroupMean[, RE4_EPV := collapse::fmean(RE4_EPV, na.rm = TRUE)]
      }

      # Set to zero if negative
      GroupMean[, RE4_VHM := collapse::fsum((RE4_Mean - RE4_GrandMean) ^ 2) / (.N-1) - RE4_EPV / RE4_N]
      data.table::set(GroupMean, i = which(GroupMean[['RE4_VHM']] < 0), j = 'RE4_VHM', value = 0)
      GroupMean[, RE4_K := RE4_EPV / RE4_VHM]
      GroupMean[, RE4_Z := RE4_N / (RE4_N + RE4_K)]
      GroupMean[, paste0(RandomEffects[i], "_MixedEffects") := RE4_Z * RE4_Mean + (1 - RE4_Z) * RE4_GrandMean]
      if(KeepSubStats) {
        nam <- names(GroupMean)[-1L]
        data[GroupMean, paste0(nam) := mget(paste0("i.", nam))]
      } else {
        data[GroupMean, RE4_MixedEffects := i.RE4_MixedEffects]
      }

    } else if(i == 5L) {

      # Prior Stats ----
      GrandMean <- data %>% gby(RE5) %>% gv('RE4_MixedEffects') %>% fmean
      data.table::setkey(GrandMean, RE5)
      data.table::setnames(GrandMean, 'RE4_MixedEffects', 'RE5_GrandMean')
      data.table::setkey(x = data, cols = RE5)

      # Posterior Stats
      DT <- list()
      DT[['RE5']] <- unique(data$RE3)

      if(all(c(4,5) %in% Nest)) {
        DT[['RE5_Mean']] <- ((data %>% gby(RE4,RE5) %>% gv('TargetVar') %>% fmean) %>% gby(RE5) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE5_N']] <- ((data %>% gby(RE4,RE5) %>% gv('TargetVar') %>% fnobs) %>% gby(RE5) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE5)
          rm(DT); gc()
          GroupMean[, RE5_EPV := RE5_Mean * (1 - RE5_Mean) / RE5_N]
        } else {
          DT[['RE5_EPV']] <- ((data %>% gby(RE4,RE5) %>% gv('TargetVar') %>% fmean) %>% gby(RE5) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE5)
          rm(DT); gc()
        }
      } else {
        DT[['RE5_Mean']] <- (data %>% gby(RE5) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE5_N']] <- (data %>% gby(RE5) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE5)
          rm(DT); gc()
          GroupMean[, RE5_EPV := RE5_Mean * (1 - RE5_Mean) / RE5_N]
        } else {
          DT[['RE5_EPV']] <- (data %>% gby(RE5) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE5)
          rm(DT); gc()
        }
      }

      # Merge Prior to data ----
      GroupMean[GrandMean, RE5_GrandMean := i.RE5_GrandMean]

      if(CollapseEPV) {
        GroupMean[, RE5_EPV := collapse::fmean(RE5_EPV, na.rm = TRUE)]
      }

      # Set to zero if negative
      GroupMean[, RE5_VHM := collapse::fsum((RE5_Mean - RE5_GrandMean) ^ 2) / (.N-1) - RE5_EPV / RE5_N]
      data.table::set(GroupMean, i = which(GroupMean[['RE5_VHM']] < 0), j = 'RE2_VHM', value = 0)
      GroupMean[, RE5_K := RE5_EPV / RE5_VHM]
      GroupMean[, RE5_Z := RE5_N / (RE5_N + RE5_K)]
      GroupMean[, paste0(RandomEffects[i], "_MixedEffects") := RE5_Z * RE5_Mean + (1 - RE5_Z) * RE5_GrandMean]
      if(KeepSubStats) {
        nam <- names(GroupMean)[-1L]
        data[GroupMean, paste0(nam) := mget(paste0("i.", nam))]
      } else {
        data[GroupMean, RE5_MixedEffects := i.RE5_MixedEffects]
      }
    }
  }

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Revert back to original names
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  data.table::setnames(data, 'TargetVar', TargetVariable)
  # i = 1  i = 2
  for(i in seq_along(GroupVarsNew)) {
    data.table::setnames(
      data,
      names(data)[which(names(data) %like% paste0('RE', i))],
      gsub(pattern = paste0('RE', i), replacement = GroupVarsNew[i], x = names(data)[which(names(data) %like% paste0('RE', i))]))
  }

  # Collect vars
  MeasureVariables <- c()
  MeasureVariables <- c(MeasureVariables,FixedEffects, paste0(RandomEffectsOld[length(RandomEffectsOld)], "_MixedEffects"))

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Fixed Effects
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  if(length(FixedEffects) > 0L && Lmer) {
    Lmer <- Lmer(data, TargetVariable = TargetVariable, FixedEffects = FixedEffects, RandomEffects = RandomEffectsOld, Type = LmerType, Poly = PolyN, Family = if(TargetType == 'regression') 'gaussian' else 'binomial')
    data <- Lmer$data
    Lmer$data <- NULL
    MeasureVariables <- c(MeasureVariables, paste0(RandomEffectsOld[length(RandomEffectsOld)], "_Lmer"))
  }

  if(length(FixedEffects) > 0L && Fme) {
    if(length(PolyN) == 1L && PolyN > 1L) {
      data[, paste0(RandomEffectsOld[length(RandomEffectsOld)], "_Fme") := lm(as.formula(paste0(TargetVariable, " ~ poly(", FixedEffects, ") + ", paste0("offset(", RandomEffectsOld[length(RandomEffectsOld)], "_MixedEffects)"))), data = data)$fitted.values]
    } else {
      data[, paste0(RandomEffectsOld[length(RandomEffectsOld)], "_Fme") := lm(as.formula(paste0(TargetVariable, " ~ ", paste0(FixedEffects, collapse =  " + "), " + ", paste0("offset(", RandomEffectsOld[length(RandomEffectsOld)], "_MixedEffects)"))), data = data)$fitted.values]
    }
    MeasureVariables <- c(MeasureVariables, paste0(RandomEffectsOld[length(RandomEffectsOld)], "_Fme"))
  }

  return(list(
    data = data,
    Lmer = if(exists('Lmer')) Lmer else NULL,
    DensityPlot = function() DensityPlot(data = data, GroupVariables = RandomEffectsOld, MeasureVars = MeasureVariables)
  ))
}

#' @title FastLM
#'
#' @description LM with coefficients greater than or equal to zero if Constrained is TRUE. Otherwise, a fast lm coefficient set is returned from armadillo
#'
#' @author Adrian Antico
#' @family Mixed Effects
#'
#' @param data data.table
#' @param yvar target variable name
#' @param xvar x variable names
#'
#' @noRd
FastLM <- function(data,
                   yvar = NULL,
                   xvar = NULL,
                   Constrained = TRUE) {
  if(Constrained) {
    solution <- matrix(as.matrix(x = data[, .SD, .SDcols = c(xvar)]), nrow = data[,.N], ncol = length(xvar))
    Y <- matrix(data[[yvar]])
    Rinv <- solve(chol(t(solution) %*% solution));
    C <- cbind(rep(1,length(xvar)), diag(length(xvar)))
    b <- c(1,rep(0,length(xvar)))
    d <- t(Y) %*% solution
    return(quadprog::solve.QP(Dmat = Rinv, factorized = TRUE, dvec = d, Amat = C, bvec = b, meq = 1))
  } else {
    return(unlist(RcppArmadillo::fastLmPure(y = data[[yvar]], X = Matrix::as.matrix(data[, .SD, .SDcols = c(xvar)], ncol = length(ncol(data)))))[seq_along(xvar)])
  }
}

#' @title Lmer
#'
#' @description LM with coefficients greater than or equal to zero if Constrained is TRUE. Otherwise, a fast lm coefficient set is returned from armadillo
#'
#' @author Adrian Antico
#' @family Mixed Effects
#'
#' @param data data.table
#' @param TargetVariable target variable name
#' @param RandomEffects NULL. Must include at least one variable
#' @param FixedEffects NULL. Optional
#' @param Poly NULL. If set to a numeric value, then polynomial terms will be used for the Fixed Effects. Used for when the FixedEffect is a Prediction output and a single value.
#' @param Family = 'gaussian' or 'binomial'
#'
#' @details
#' Up to 5 random effects variables can be included.
#'
#' There are
#'
#' @examples
#' FixedEffects <- 'Predict'; RandomEffects <- 'Factor_1', TargetVariable <- 'TargetVar'
#'
#' @noRd
Lmer <- function(data,
                       TargetVariable = NULL,
                       FixedEffects = NULL,
                       RandomEffects = NULL,
                       Type = 'add',
                       Poly = NULL,
                       Family = 'gaussian') {

  if(length(RandomEffects) == 1L) {

    # y = (1 | var1)
    if(length(Poly) > 0L && length(FixedEffects) == 1L) {
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ poly(", FixedEffects, ", ", Poly, ") + (1 | ", RandomEffects[1L], ")")))
    } else {
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", FixedEffects, " + (1 | ", RandomEffects[1L], ")")))
    }

    if(Family == 'gaussian') {
      model_lmer <- lme4::lmer(formula = x, data = data)
    } else {
      model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
    }

    data[, paste0(RandomEffects[1L], "_Lmer") := predict(model_lmer)]

  } else if(length(RandomEffects) == 2L) {

    if(Type == 'add') {

      #     y =  (1 | var1) + (1 | var2)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (1 | ", RandomEffects[2L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[2L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'add_nest') {

      #     y =  (1 | var1) + (var1 | var2)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (", RandomEffects[1], " | ", RandomEffects[2L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[2L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'int_nest') {

      #     y =  (1 + var1 | var2)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 + ", RandomEffects[1], " | ", RandomEffects[2L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[2L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'nest') {

      #     y =  (var1 | var2)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(", RandomEffects[1], " | ", RandomEffects[2L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[2L], "_Lmer") := predict(model_lmer)]

    }

  } else if(length(RandomEffects) == 3L) {

    if(Type == 'add') {

      #     y =  (1 | var1) + (1 | var2) + (1 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (1 | ", RandomEffects[2L],") + (1 | ", RandomEffects[3L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[3L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'add_nest') {

      #     y =  (1 | var1) + (var1 | var2) + (var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (", RandomEffects[1], " | ", RandomEffects[2L],") + (", RandomEffects[2L], " | ", RandomEffects[3L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[3L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'int_nest') {

      #     y =  (1 + var1 | var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 + ", RandomEffects[1], " | ", RandomEffects[2L], " | ", RandomEffects[3L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[3L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'nest') {

      #     y =  (var1 | var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(", RandomEffects[1], " | ", RandomEffects[2L], " | ", RandomEffects[3L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[3L], "_Lmer") := predict(model_lmer)]
    }

  } else if(length(RandomEffects) == 4L) {

    if(Type == 'add') {

      #     y =  (1 | var1) + (1 | var2) + (1 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (1 | ", RandomEffects[2L],") + (1 | ", RandomEffects[3L],") + (1 | ", RandomEffects[4L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[4L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'add_nest') {

      #     y =  (1 | var1) + (var1 | var2) + (var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (", RandomEffects[1], " | ", RandomEffects[2L],") + (", RandomEffects[2L], " | ", RandomEffects[3L],") + (", RandomEffects[3L]," | ", RandomEffects[4L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[4L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'int_nest') {

      #     y =  (1 + var1 | var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 + ", RandomEffects[1], " | ", RandomEffects[2L], " | ", RandomEffects[3L], " | ", RandomEffects[4L], ")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[4L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'nest') {

      #     y =  (var1 | var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(", RandomEffects[1], " | ", RandomEffects[2L], " | ", RandomEffects[3L], " | ", RandomEffects[4L], ")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[4L], "_Lmer") := predict(model_lmer)]
    }

  } else if(length(RandomEffects) == 5L) {

    if(Type == 'add') {

      #     y =  (1 | var1) + (1 | var2) + (1 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (1 | ", RandomEffects[2L],") + (1 | ", RandomEffects[3L],") + (1 | ", RandomEffects[4L],") + (1 | ", RandomEffects[5L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[5L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'add_nest') {

      #     y =  (1 | var1) + (var1 | var2) + (var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (", RandomEffects[1], " | ", RandomEffects[2L],") + (", RandomEffects[2L], " | ", RandomEffects[3L],") + (", RandomEffects[3L]," | ", RandomEffects[4L],") + (", RandomEffects[4L]," | ", RandomEffects[5L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[5L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'int_nest') {

      #     y =  (1 + var1 | var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 + ", RandomEffects[1], " | ", RandomEffects[2L], " | ", RandomEffects[3L], " | ", RandomEffects[4L], " | ", RandomEffects[5L], ")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[5L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'nest') {

      #     y =  (var1 | var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(", RandomEffects[1], " | ", RandomEffects[2L], " | ", RandomEffects[3L], " | ", RandomEffects[4L], " | ", RandomEffects[5L], ")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[5L], "_Lmer") := predict(model_lmer)]
    }

  }

  # Return data and model
  return(list(
    data = data,
    Model = model_lmer,
    EffectsInterval = function() EffectsInterval(Model = model_lmer)[],
    PredictionInterval = function() suppressWarnings(PredictionIntervals(Model = model_lmer))[]))
}

#' @title EffectsInterval
#'
#' @description Random Effects with confidence endpoints
#'
#' @author Adrian Antico
#' @family Mixed Effects
#'
#' @param Model lmer and brms models
#'
#' @noRd
EffectsInterval <- function(Model) {
  data.table::setDT(mixedup::extract_random_effects(Model))
}

#' @title EffectsInterval
#'
#' @description Random Effects Prediction Intervals: random effect, upper and lower 95th Percentile. For various Model predictions, possibly with new data
#'
#' @author Adrian Antico
#' @family Mixed Effects
#'
#' @param Model Output from RemixAutoML:: supervised learning functions
#'
#' @noRd
PredictionIntervals <- function(Model) {
  data.table::setDT(merTools::predictInterval(Model)) # for various Model predictions, possibly with new data
}
