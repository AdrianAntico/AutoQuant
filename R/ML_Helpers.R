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
