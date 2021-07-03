#' @title AutoXGBoostHurdleModel
#'
#' @description AutoXGBoostHurdleModel is generalized hurdle modeling framework
#'
#' @family Supervised Learning - Hurdle Modeling
#' @author Adrian Antico
#'
#' @param TrainOnFull Set to TRUE to train model on 100 percent of data
#' @param grid_eval_metric Select the metric to optimize in grid tuning. "accuracy", "microauc", "logloss"
#' @param BaselineComparison "default"
#' @param MaxRunsWithoutNewWinner Number of runs without a new winner before stopping the grid tuning
#' @param MaxRunMinutes Max number of minutes to allow the grid tuning to run for
#' @param data Source training data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param ValidationData Source validation data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param TestData Souce test data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param Buckets A numeric vector of the buckets used for subsetting the data. NOTE: the final Bucket value will first create a subset of data that is less than the value and a second one thereafter for data greater than the bucket value.
#' @param TargetColumnName Supply the column name or number for the target variable
#' @param FeatureColNames Supply the column names or number of the features (not included the PrimaryDateColumn)
#' @param PrimaryDateColumn Date column for sorting
#' @param WeightsColumnName Weighs column name
#' @param IDcols Includes PrimaryDateColumn and any other columns you want returned in the validation data with predictions
#' @param ClassWeights Look up the classifier model help file
#' @param DebugMode For debugging
#' @param EncodingMethod Choose from 'binary', 'poly_encode', 'backward_difference', 'helmert' for multiclass cases and additionally 'm_estimator', 'credibility', 'woe', 'target_encoding' for classification use cases.
#' @param TransformNumericColumns Transform numeric column inside the AutoCatBoostRegression() function
#' @param Methods Choose from 'Asinh', 'Asin', 'Log', 'LogPlus1', 'Sqrt', 'Logit'
#' @param SplitRatios Supply vector of partition ratios. For example, c(0.70,0.20,0,10)
#' @param TreeMethod Set to hist or gpu_hist depending on if you have an xgboost installation capable of gpu processing
#' @param NThreads Set to the number of threads you would like to dedicate to training
#' @param ModelID Define a character name for your models
#' @param Paths The path to your folder where you want your model information saved
#' @param MetaDataPaths A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to Paths.
#' @param ReturnModelObjects Set to TRUE to return all model objects
#' @param SaveModelObjects Set to TRUE to save the model objects to file in the folders listed in Paths
#' @param GridTune Set to TRUE if you want to grid tune the models
#' @param MaxModelsInGrid Set to a numeric value for the number of models to try in grid tune
#' @param NumOfParDepPlots Set to pull back N number of partial dependence calibration plots.
#' @param PassInGrid Pass in a grid for changing up the parameter settings for catboost
#' @param Trees Provide a named list to have different number of trees for each model. Trees = list("classifier" = seq(1000,2000,100), "regression" = seq(1000,2000,100))
#' @param eta Provide a named list to have different number of eta for each model.
#' @param max_depth Provide a named list to have different number of max_depth for each model.
#' @param min_child_weight Provide a named list to have different number of min_child_weight for each model.
#' @param subsample Provide a named list to have different number of subsample for each model.
#' @param colsample_bytree Provide a named list to have different number of colsample_bytree for each model.
#' @examples
#' \dontrun{
#' Output <- RemixAutoML::AutoXGBoostHurdleModel(
#'
#'    # Operationalization args
#'    TrainOnFull = FALSE,
#'    PassInGrid = NULL,
#'
#'    # Metadata args
#'    NThreads = max(1L, parallel::detectCores()-2L),
#'    ModelID = "ModelTest",
#'    Paths = normalizePath("./"),
#'    MetaDataPaths = NULL,
#'
#'    # data args
#'    data,
#'    ValidationData = NULL,
#'    TestData = NULL,
#'    Buckets = 0L,
#'    TargetColumnName = NULL,
#'    FeatureColNames = NULL,
#'    PrimaryDateColumn = NULL,
#'    WeightsColumnName = NULL,
#'    IDcols = NULL,
#'    ClassWeights = c(1,1),
#'    DebugMode = FALSE,
#'
#'    # options
#'    EncodingMethod = "credibility",
#'    TransformNumericColumns = NULL,
#'    Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'),
#'    SplitRatios = c(0.70, 0.20, 0.10),
#'    ReturnModelObjects = TRUE,
#'    SaveModelObjects = FALSE,
#'    NumOfParDepPlots = 10L,
#'
#'    # grid tuning args
#'    GridTune = FALSE,
#'    grid_eval_metric = "accuracy",
#'    MaxModelsInGrid = 1L,
#'    BaselineComparison = "default",
#'    MaxRunsWithoutNewWinner = 10L,
#'    MaxRunMinutes = 60L,
#'
#'    # XGBoost parameters
#'    TreeMethod = "hist",
#'    Trees = list("classifier" = 1000, "regression" = 1000),
#'    eta = list("classifier" = 0.05, "regression" = 0.05),
#'    max_depth = list("classifier" = 4L, "regression" = 4L),
#'    min_child_weight = list("classifier" = 1.0, "regression" = 1.0),
#'    subsample = list("classifier" = 0.55, "regression" = 0.55),
#'    colsample_bytree = list("classifier" = 0.55, "regression" = 0.55))
#' }
#' @export
AutoXGBoostHurdleModel <- function(TreeMethod = "hist",
                                   TrainOnFull = FALSE,
                                   PassInGrid = NULL,
                                   NThreads = max(1L, parallel::detectCores()-2L),
                                   ModelID = "ModelTest",
                                   Paths = NULL,
                                   MetaDataPaths = NULL,
                                   data,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   Buckets = 0L,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   WeightsColumnName = NULL,
                                   ClassWeights = c(1,1),
                                   IDcols = NULL,
                                   DebugMode = FALSE,
                                   EncodingMethod = "credibility",
                                   TransformNumericColumns = NULL,
                                   Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'),
                                   SplitRatios = c(0.70, 0.20, 0.10),
                                   SaveModelObjects = FALSE,
                                   ReturnModelObjects = TRUE,
                                   NumOfParDepPlots = 1L,
                                   GridTune = FALSE,
                                   grid_eval_metric = "accuracy",
                                   MaxModelsInGrid = 1L,
                                   BaselineComparison = "default",
                                   MaxRunsWithoutNewWinner = 10L,
                                   MaxRunMinutes = 60L,
                                   Trees = list("classifier" = 1000, "regression" = 1000),
                                   eta = list("classifier" = 0.05, "regression" = 0.05),
                                   max_depth = list("classifier" = 4L, "regression" = 4L),
                                   min_child_weight = list("classifier" = 1.0, "regression" = 1.0),
                                   subsample = list("classifier" = 0.55, "regression" = 0.55),
                                   colsample_bytree = list("classifier" = 0.55, "regression" = 0.55)) {

  # Store args ----
  ArgsList <- list()
  ArgsList[["Buckets"]] <- Buckets
  ArgsList[["TargetColumnName"]] <- TargetColumnName
  ArgsList[["FeatureColNames"]] <- FeatureColNames
  ArgsList[["IDcols"]] <- IDcols
  ArgsList[["TransformNumericColumns"]] <- TransformNumericColumns
  ArgsList[["SplitRatios"]] <- SplitRatios
  ArgsList[["TreeMethod"]] <- TreeMethod
  ArgsList[["ModelID"]] <- ModelID
  ArgsList[["Paths"]] <- Paths
  ArgsList[["MetaDataPaths"]] <- MetaDataPaths
  ArgsList[["SaveModelObjects"]] <- SaveModelObjects
  ArgsList[["EncodingMethod"]] <- EncodingMethod

  # Check args ----
  if(is.character(Buckets) || is.factor(Buckets) || is.logical(Buckets)) stop("Buckets needs to be a numeric scalar or vector")
  if(!is.logical(SaveModelObjects)) stop("SaveModelOutput needs to be set to either TRUE or FALSE")
  if(!is.logical(GridTune)) stop("GridTune needs to be either TRUE or FALSE")

  # Args management ----

  # Trees
  if(is.list(Trees)) {
    if(!GridTune) {
      ClassifierTrees <- Trees[["classifier"]]
      ClassifierTrees <- ClassifierTrees[length(ClassifierTrees)]
      RegressionTrees <- Trees[["regression"]]
      RegressionTrees <- RegressionTrees[length(RegressionTrees)]
    } else {
      ClassifierTrees <- Trees[["classifier"]]
      RegressionTrees <- Trees[["regression"]]
    }
  } else {
    if(!GridTune) {
      RegressionTrees <- Trees[length(Trees)]
      ClassifierTrees <- Trees[length(Trees)]
    } else {
      ClassifierTrees <- Trees
      RegressionTrees <- Trees
    }
  }

  # eta
  if(is.list(eta)) {
    if(!GridTune) {
      Classifiereta <- eta[["classifier"]]
      Classifiereta <- Classifiereta[length(Classifiereta)]
      Regressioneta <- eta[["regression"]]
      Regressioneta <- Regressioneta[length(Regressioneta)]
    } else {
      Classifiereta <- eta[["classifier"]]
      Regressioneta <- eta[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressioneta <- eta[length(eta)]
      Classifiereta <- eta[length(eta)]
    } else {
      Classifiereta <- eta
      Regressioneta <- eta
    }
  }

  # max_depth
  if(is.list(max_depth)) {
    if(!GridTune) {
      Classifiermax_depth <- max_depth[["classifier"]]
      Classifiermax_depth <- Classifiermax_depth[length(Classifiermax_depth)]
      Regressionmax_depth <- max_depth[["regression"]]
      Regressionmax_depth <- Regressionmax_depth[length(Regressionmax_depth)]
    } else {
      Classifiermax_depth <- max_depth[["classifier"]]
      Regressionmax_depth <- max_depth[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmax_depth <- max_depth[length(max_depth)]
      Classifiermax_depth <- max_depth[length(max_depth)]
    } else {
      Classifiermax_depth <- max_depth
      Regressionmax_depth <- max_depth
    }
  }

  # min_child_weight
  if(is.list(min_child_weight)) {
    if(!GridTune) {
      Classifiermin_child_weight <- min_child_weight[["classifier"]]
      Classifiermin_child_weight <- Classifiermin_child_weight[length(Classifiermin_child_weight)]
      Regressionmin_child_weight <- min_child_weight[["regression"]]
      Regressionmin_child_weight <- Regressionmin_child_weight[length(Regressionmin_child_weight)]
    } else {
      Classifiermin_child_weight <- min_child_weight[["classifier"]]
      Regressionmin_child_weight <- min_child_weight[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmin_child_weight <- min_child_weight[length(min_child_weight)]
      Classifiermin_child_weight <- min_child_weight[length(min_child_weight)]
    } else {
      Classifiermin_child_weight <- min_child_weight
      Regressionmin_child_weight <- min_child_weight
    }
  }

  # subsample
  if(is.list(subsample)) {
    if(!GridTune) {
      Classifiersubsample <- subsample[["classifier"]]
      Classifiersubsample <- Classifiersubsample[length(Classifiersubsample)]
      Regressionsubsample <- subsample[["regression"]]
      Regressionsubsample <- Regressionsubsample[length(Regressionsubsample)]
    } else {
      Classifiersubsample <- subsample[["classifier"]]
      Regressionsubsample <- subsample[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionsubsample <- subsample[length(subsample)]
      Classifiersubsample <- subsample[length(subsample)]
    } else {
      Classifiersubsample <- subsample
      Regressionsubsample <- subsample
    }
  }

  # colsample_bytree
  if(is.list(colsample_bytree)) {
    if(!GridTune) {
      Classifiercolsample_bytree <- colsample_bytree[["classifier"]]
      Classifiercolsample_bytree <- Classifiercolsample_bytree[length(Classifiercolsample_bytree)]
      Regressioncolsample_bytree <- colsample_bytree[["regression"]]
      Regressioncolsample_bytree <- Regressioncolsample_bytree[length(Regressioncolsample_bytree)]
    } else {
      Classifiercolsample_bytree <- colsample_bytree[["classifier"]]
      Regressioncolsample_bytree <- colsample_bytree[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressioncolsample_bytree <- colsample_bytree[length(colsample_bytree)]
      Classifiercolsample_bytree <- colsample_bytree[length(colsample_bytree)]
    } else {
      Classifiercolsample_bytree <- colsample_bytree
      Regressioncolsample_bytree <- colsample_bytree
    }
  }

  # Ensure Paths and metadata_path exists----
  if(!is.null(Paths)) if(!dir.exists(Paths)) dir.create(Paths)
  if(is.null(MetaDataPaths)) MetaDataPaths <- Paths else if(!dir.exists(MetaDataPaths)) dir.create(MetaDataPaths)

  # Data.table check----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(ValidationData)) if(!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)
  if(!is.null(TestData)) if(!data.table::is.data.table(TestData)) data.table::setDT(TestData)

  # IDcols to Names ----
  if(!is.null(IDcols)) if(is.numeric(IDcols) || is.integer(IDcols)) IDcols <- names(data)[IDcols]
  IDcols <- c(IDcols, TargetColumnName)

  # Primary Date Column ----
  if(is.numeric(PrimaryDateColumn) || is.integer(PrimaryDateColumn)) PrimaryDateColumn <- names(data)[PrimaryDateColumn]

  # FeatureColumnNames ----
  if(is.numeric(FeatureColNames) || is.integer(FeatureColNames)) FeatureNames <- names(data)[FeatureColNames] else FeatureNames <- FeatureColNames

  # Add target bucket column ----
  looper <- rev(seq_len(length(Buckets) + 1L))
  if(length(Buckets) == 1L) {
    data[, Target_Buckets := data.table::fifelse(get(TargetColumnName) <= eval(Buckets[1L]), 0, 1)]
  } else {
    for(i in seq_len(length(Buckets) + 1L)) {
      if(i == 1L) {
        data[which(data[[eval(TargetColumnName)]] <= Buckets[i]), Target_Buckets  := as.factor(Buckets[i])]
      } else if(i == length(Buckets) + 1L) {
        if(data[which(data[[eval(TargetColumnName)]] > Buckets[i - 1]), .N] != 0) {
          data[which(data[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(paste0(Buckets[i - 1], '+'))]
        } else {
          Buckets <- Buckets[!Buckets %in% Buckets[i]]
          looper <- looper[-1L]
        }
      } else {
        data[which(data[[eval(TargetColumnName)]] <= Buckets[i] & data[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(Buckets[i])]
      }
    }
  }

  # Store looper -----
  ArgsList[["looper"]] <- looper

  # Add target bucket column ----
  if(!is.null(ValidationData)) {
    if(length(Buckets) == 1L) {
      ValidationData[, Target_Buckets := data.table::fifelse(get(TargetColumnName) <= eval(Buckets[1L]), 0, 1)]
    } else {
      for(i in seq_len(length(Buckets) + 1L)) {
        if(i == 1) {
          ValidationData[which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i]), Target_Buckets := as.factor(Buckets[i])]
        } else if(i == length(Buckets) + 1L) {
          ValidationData[which(ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(paste0(Buckets[i - 1], '+'))]
        } else {
          if(data[which(data[[eval(TargetColumnName)]] > Buckets[i - 1]), .N] != 0) {
            ValidationData[which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i] & ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(Buckets[i])]
          }
        }
      }
    }
  }

  # Add target bucket column ----
  if(!is.null(TestData)) {
    if(length(Buckets) == 1L) {
      TestData[, Target_Buckets := data.table::fifelse(get(TargetColumnName) <= eval(Buckets[1L]), 0, 1)]
    } else {
      for(i in seq_len(length(Buckets) + 1L)) {
        if(i == 1) {
          TestData[which(TestData[[eval(TargetColumnName)]] <= Buckets[i]), Target_Buckets := as.factor(Buckets[i])]
        } else if(i == length(Buckets) + 1L) {
          TestData[which(TestData[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(paste0(Buckets[i - 1L], '+'))]
        } else {
          if(data[which(data[[eval(TargetColumnName)]] > Buckets[i - 1]), .N] != 0) {
            TestData[which(TestData[[eval(TargetColumnName)]] <= Buckets[i] & TestData[[eval(TargetColumnName)]] > Buckets[i - 1L]), Target_Buckets := as.factor(Buckets[i])]
          }
        }
      }
    }
  }

  # AutoDataPartition if Validation and TestData are NULL----
  if(is.null(ValidationData) && is.null(TestData) && !TrainOnFull) {
    DataSets <- AutoDataPartition(
      data = data,
      NumDataSets = 3L,
      Ratios = SplitRatios,
      PartitionType = 'random',
      StratifyColumnNames = 'Target_Buckets',
      TimeColumnName = NULL)
    data <- DataSets$TrainData
    ValidationData <- DataSets$ValidationData
    TestData <- DataSets$TestData
    rm(DataSets)

    # Add target bucket column----
    if(!is.null(ValidationData)) {
      if(length(Buckets) == 1L) {
        ValidationData[, Target_Buckets := data.table::fifelse(get(TargetColumnName) <= eval(Buckets[1L]), 0, 1)]
      } else {
        for(i in seq_len(length(Buckets) + 1L)) {
          if(i == 1) {
            ValidationData[which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i]), Target_Buckets := as.factor(Buckets[i])]
          } else if(i == length(Buckets) + 1L) {
            ValidationData[which(ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(paste0(Buckets[i - 1], '+'))]
          } else {
            if(data[which(data[[eval(TargetColumnName)]] > Buckets[i - 1]), .N] != 0) {
              ValidationData[which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i] & ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(Buckets[i])]
            }
          }
        }
      }
    }

    # Add target bucket column----
    if(!is.null(TestData)) {
      if(length(Buckets) == 1L) {
        TestData[, Target_Buckets := data.table::fifelse(get(TargetColumnName) <= eval(Buckets[1L]), 0, 1)]
      } else {
        for(i in seq_len(length(Buckets) + 1L)) {
          if(i == 1) {
            TestData[which(TestData[[eval(TargetColumnName)]] <= Buckets[i]), Target_Buckets := as.factor(Buckets[i])]
          } else if(i == length(Buckets) + 1L) {
            TestData[which(TestData[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(paste0(Buckets[i - 1L], '+'))]
          } else {
            if(data[which(data[[eval(TargetColumnName)]] > Buckets[i - 1]), .N] != 0) {
              TestData[which(TestData[[eval(TargetColumnName)]] <= Buckets[i] & TestData[[eval(TargetColumnName)]] > Buckets[i - 1L]), Target_Buckets := as.factor(Buckets[i])]
            }
          }
        }
      }
    }
  } else if(TrainOnFull) {
    ValidationData <- NULL
    TestData <- NULL
  }

  # Begin classification model building ----
  if(length(Buckets) == 1L) {
    ClassifierModel <- AutoXGBoostClassifier(

      # New
      OutputSelection = c("Importances", "EvalMetrics"),
      WeightsColumnName = WeightsColumnName,
      SaveInfoToPDF = FALSE,
      DebugMode = DebugMode,

      # general args
      eval_metric = "auc",
      TrainOnFull = TrainOnFull,
      TreeMethod = TreeMethod,
      PassInGrid = PassInGrid,
      NThreads = NThreads,
      model_path = Paths,
      metadata_path = MetaDataPaths,
      ModelID = ModelID,

      # options
      ReturnModelObjects = TRUE,
      EncodingMethod = EncodingMethod,
      ReturnFactorLevels = TRUE,
      Verbose = 1L,
      NumOfParDepPlots = NumOfParDepPlots,
      SaveModelObjects = SaveModelObjects,

      # data args
      TargetColumnName = "Target_Buckets",
      data = data.table::copy(data),
      ValidationData = data.table::copy(ValidationData),
      TestData = data.table::copy(TestData),
      FeatureColNames = FeatureNames,
      IDcols = IDcols,

      # Grid tuning args
      GridTune = GridTune,
      BaselineComparison = BaselineComparison,
      MaxModelsInGrid = MaxModelsInGrid,
      MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
      MaxRunMinutes = MaxRunMinutes,

      # bandit args
      Trees = ClassifierTrees,
      eta = Classifiereta,
      max_depth = Classifiermax_depth,

      # random args
      min_child_weight = Classifiermin_child_weight,
      subsample = Classifiersubsample,
      colsample_bytree = Classifiercolsample_bytree)

  } else {
    ClassifierModel <- RemixAutoML::AutoXGBoostMultiClass(

      # Udpated args
      OutputSelection = c("Importances", "EvalMetrics"),
      WeightsColumnName = WeightsColumnName,
      DebugMode = DebugMode,

      # type
      grid_eval_metric = grid_eval_metric,
      eval_metric = "merror",
      LossFunction = 'multi:softprob',

      # general args
      TrainOnFull = TrainOnFull,
      TreeMethod = TreeMethod,
      PassInGrid = PassInGrid,
      NThreads = NThreads,
      model_path = Paths,
      metadata_path = MetaDataPaths,
      ModelID = ModelID,
      EncodingMethod = EncodingMethod,

      # options
      ReturnModelObjects = TRUE,
      ReturnFactorLevels = TRUE,
      NumOfParDepPlots = NumOfParDepPlots,
      SaveModelObjects = SaveModelObjects,
      Verbose = 1L,

      # data args
      data = data.table::copy(data),
      ValidationData = data.table::copy(ValidationData),
      TestData = data.table::copy(TestData),
      TargetColumnName = "Target_Buckets",
      FeatureColNames = FeatureNames,
      IDcols = IDcols,

      # Grid tuning args
      GridTune = GridTune,
      BaselineComparison = BaselineComparison,
      MaxModelsInGrid = MaxModelsInGrid,
      MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
      MaxRunMinutes = MaxRunMinutes,

      # bandit args
      Trees = ClassifierTrees,
      eta = Classifiereta,
      max_depth = Classifiermax_depth,

      # random args
      min_child_weight = Classifiermin_child_weight,
      subsample = Classifiersubsample,
      colsample_bytree = Classifiercolsample_bytree)
  }

  # Store metadata ----
  ModelList <- list()
  ClassModel <- ClassifierModel$Model
  ModelList[["ClassificationModel"]] <- ClassModel
  ClassEvaluationMetrics <- ClassifierModel$EvaluationMetrics
  VariableImportance <- ClassifierModel$VariableImportance
  if(length(Buckets) > 1L) {
    TargetLevels <- ClassifierModel$TargetLevels
    ArgsList[["TargetLevels"]] <- TargetLevels
  } else {
    TargetLevels <- NULL
    ArgsList[["TargetLevels"]] <- NULL
  }
  FactorLevelsListOutput <- ClassifierModel$FactorLevels
  if(!is.null(FactorLevelsListOutput)) FactorLevelsList <- FactorLevelsListOutput else FactorLevelsList <- NULL
  ArgsList[["Class_FactorLevelsList"]] <- FactorLevelsList
  rm(ClassifierModel)

  # Define args ----
  if(length(Buckets) == 1L) {
    TargetType <- "Classification"
  } else {
    TargetType <- "Multiclass"
  }

  # Model Scoring ----
  if(!TrainOnFull || !is.null(ValidationData)) {
    if(is.null(TestData)) ValTrue <<- TRUE else ValTrue <<- FALSE
    temp <- AutoXGBoostScoring(
      ReturnShapValues = FALSE,
      EncodingMethod = EncodingMethod,
      TargetType = TargetType,
      ScoringData = if(!is.null(TestData)) data.table::copy(TestData) else if(!is.null(ValidationData)) data.table::copy(ValidationData) else data.table::copy(data),
      FeatureColumnNames = FeatureNames,
      IDcols = unique(c(IDcols, "Target_Buckets", WeightsColumnName)),
      FactorLevelsList = FactorLevelsList,
      TargetLevels = TargetLevels,
      OneHot = FALSE,
      ModelObject = ClassModel,
      ModelPath = if(!is.null(ClassModel)) NULL else Paths,
      ModelID = ModelID,
      ReturnFeatures = TRUE,
      TransformNumeric = FALSE,
      BackTransNumeric = FALSE,
      TargetColumnName = NULL,
      TransformationObject = NULL,
      TransID = NULL,
      TransPath = NULL,
      MDP_Impute = TRUE,
      MDP_CharToFactor = TRUE,
      MDP_RemoveDates = TRUE,
      MDP_MissFactor = "0",
      MDP_MissNum = -1)

    # Nuance
    if(TargetType != "Classification") {
      TestData <- cbind(temp, TestData[, .SD, .SDcols = c(setdiff(names(TestData),names(temp)))])
    } else {
      TestData <- temp
      rm(temp)
    }

    # Rename data output
    if(ValTrue) {
      ValidationData <- TestData
      TestData <- NULL
    }

    # Change name for classification----
    if(DebugMode) print('Change name for classification----')
    if(tolower(TargetType) == 'Classification') {
      if(!is.null(TestData)) {
        data.table::setnames(TestData, 'Predictions', 'Predictions_C1')
        TestData[, Predictions_C0 := 1 - Predictions_C1]
        data.table::setcolorder(TestData, c(ncol(TestData), 1L, 2L:(ncol(TestData) - 1L)))
      } else if(!is.null(ValidationData)) {
        data.table::setnames(ValidationData, 'Predictions', 'Predictions_C1')
        ValidationData[, Predictions_C0 := 1 - Predictions_C1]
        data.table::setcolorder(ValidationData, c(ncol(ValidationData), 1L, 2L:(ncol(ValidationData) - 1L)))
      }
    }

    # Change Name of Predicted MultiClass Column----
    if(DebugMode) print('Change Name of Predicted MultiClass Column----')
    if(tolower(TargetType) != 'classification') {
      if(!is.null(TestData)) {
        data.table::setnames(TestData, 'Predict', 'Predictions_MultiClass')
      } else if(!is.null(ValidationData)) {
        data.table::setnames(ValidationData, 'Predict', 'Predictions_MultiClass')
      }
    }

  } else {
    TestData <- NULL
    ValTrue <<- TRUE
  }

  # Remove Model Object----
  rm(ClassModel)

  # Prepare for regression runs ----
  if(DebugMode) print('Prepare for regression runs ----')
  IDcols <- unique(IDcols[!(IDcols %chin% TargetColumnName)])
  counter <- max(looper)
  Degenerate <- 0L
  R_VariableImportance <- list()
  R_ParDepPlots <- list()

  # Begin regression model building----
  if(DebugMode) print('Begin regression model building----')
  for(bucket in looper) {

    # Define data sets ----
    if(bucket == max(looper)) {
      if(!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- TestData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
      } else if(!is.null(ValidationData)) {
        trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- NULL
      } else {
        trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- NULL
        testBucket <- NULL
      }
    } else if(bucket == 1L) {
      if(!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket])]
        testBucket <- TestData[get(TargetColumnName) <= eval(Buckets[bucket])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
      } else if(!is.null(ValidationData)) {
        if(length(Buckets) == 1 && Buckets == 0) {
          trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
          validBucket <- ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
          testBucket <- NULL
        } else {
          trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
          validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket])]
          testBucket <- NULL
        }
      } else {
        if(length(Buckets) == 1 && Buckets == 0) {
          trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
          validBucket <- NULL
          testBucket <- NULL
        } else {
          trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
          validBucket <- NULL
          testBucket <- NULL
        }
      }
    } else {
      if(!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- TestData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
      } else if(!is.null(ValidationData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- NULL
      } else {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- NULL
        testBucket <- NULL
      }
    }

    # Create Modified IDcols ----
    if(!is.null(TestData)) {
      IDcolsModified <- unique(c(IDcols, TargetColumnName, WeightsColumnName, "Target_Buckets", setdiff(names(TestData), names(trainBucket))))
    } else if(!is.null(ValidationData)) {
      IDcolsModified <- unique(c(IDcols, TargetColumnName, WeightsColumnName, "Target_Buckets", setdiff(names(ValidationData), names(trainBucket))))
    } else {
      IDcolsModified <- unique(c(IDcols, TargetColumnName, WeightsColumnName, "Target_Buckets", setdiff(names(data), names(trainBucket))))
    }

    # AutoXGBoostRegression()----
    if(DebugMode) print('AutoXGBoostRegression()----')
    if(trainBucket[, .N] != 0L) {

      # If there is some variance then build model
      if(var(trainBucket[[eval(TargetColumnName)]]) > 0L) {

        # Increment----
        counter <- counter - 1L

        # Modify filepath and file name----
        if(bucket == max(looper)) ModelIDD <- paste0(ModelID,'_',bucket,'+') else ModelIDD <- paste0(ModelID, '_', bucket)

        # Build model----
        RegModel <- RemixAutoML::AutoXGBoostRegression(

          OutputSelection = c("Importances", "EvalPlots", "EvalMetrics"),
          PrimaryDateColumn = PrimaryDateColumn,
          WeightsColumnName = WeightsColumnName,
          DebugMode = DebugMode,
          SaveInfoToPDF = FALSE,

          # GPU or CPU
          TreeMethod = TreeMethod,
          NThreads = NThreads,

          # Metadata arguments
          model_path = Paths,
          metadata_path = MetaDataPaths,
          ModelID = ModelIDD,
          ReturnFactorLevels = TRUE,
          ReturnModelObjects = ReturnModelObjects,
          SaveModelObjects = SaveModelObjects,
          Verbose = 1L,
          EncodingMethod = EncodingMethod,

          # Data arguments
          data = data.table::copy(trainBucket),
          TrainOnFull = TrainOnFull,
          ValidationData = data.table::copy(validBucket),
          TestData = data.table::copy(testBucket),
          TargetColumnName = TargetColumnName,
          FeatureColNames = FeatureNames,
          IDcols = IDcolsModified,
          TransformNumericColumns = TransformNumericColumns,
          Methods = Methods,

          # Model evaluation
          eval_metric = "rmse",
          grid_eval_metric = "mse",
          NumOfParDepPlots = NumOfParDepPlots,

          # Grid tuning arguments - PassInGrid is the best of GridMetrics
          PassInGrid = PassInGrid,
          GridTune = GridTune,
          MaxModelsInGrid = MaxModelsInGrid,
          BaselineComparison = "default",
          MaxRunsWithoutNewWinner = 20L,
          MaxRunMinutes = 60*60,

          # Trees, Depth, and LearningRate used in the bandit grid tuning
          Trees = RegressionTrees,
          eta = Regressioneta,
          max_depth = Regressionmax_depth,
          min_child_weight = Regressionmin_child_weight,
          subsample = Regressionsubsample,
          colsample_bytree = Regressioncolsample_bytree)

        # Store Model----
        RegressionModel <- RegModel$Model
        if(ReturnModelObjects || SaveModelObjects) ModelList[[ModelIDD]] <- RegressionModel
        if(!is.null(TransformNumericColumns)) {
          ArgsList[[paste0("TransformationResults_", ModelIDD)]] <- RegModel$TransformationResults
        } else {
          ArgsList[[paste0("TransformationResults_", ModelIDD)]] <- NULL
        }
        R_VariableImportance[[paste0(ModelIDD)]] <- RegModel$VariableImportance
        R_ParDepPlots[[paste0(ModelIDD)]] <- RegModel$PlotList$Test_ParDepPlots
        ArgsList[[paste0(ModelIDD, "_FactorLevelsList")]] <- RegModel$FactorLevelsList

        # Garbage Collection----
        gc()

        # Score model----
        if(DebugMode) {
          print("TestData")
          print(TestData)
          print("ValidationData")
          print(ValidationData)
        }
        if(!is.null(TestData) || !is.null(ValidationData)) {
          TestData <- AutoXGBoostScoring(
            TargetType = "regression",
            ScoringData = if(!is.null(TestData)) TestData else ValidationData,
            FeatureColumnNames = FeatureNames,
            IDcols = IDcolsModified,
            FactorLevelsList = ArgsList[[paste0(ModelIDD, "_FactorLevelsList")]],
            EncodingMethod = EncodingMethod,
            OneHot = FALSE,
            ModelObject = RegressionModel,
            ModelPath = Paths,
            ModelID = ModelIDD,
            ReturnFeatures = TRUE,
            TransformNumeric = if(is.null(ArgsList[[paste0("TransformationResults_", ModelIDD)]])) FALSE else TRUE,
            BackTransNumeric = if(is.null(ArgsList[[paste0("TransformationResults_", ModelIDD)]])) FALSE else TRUE,
            TargetColumnName = eval(TargetColumnName),
            TransformationObject = ArgsList[[paste0("TransformationResults_", ModelIDD)]],
            TransID = NULL,
            TransPath = NULL,
            MDP_Impute = TRUE,
            MDP_CharToFactor = TRUE,
            MDP_RemoveDates = FALSE,
            MDP_MissFactor = "0",
            MDP_MissNum = -1)

          # Clear TestModel From Memory----
          rm(RegModel)
          gc()

          # Change prediction name to prevent duplicates----
          if(DebugMode) print('Change prediction name to prevent duplicates----')
          if(bucket == max(looper)) Val <- paste0('Predictions_', bucket - 1L, '+') else Val <- paste0('Predictions_', bucket)
          if(ValTrue) {
            data.table::setnames(ValidationData, 'Predictions', Val)
          } else {
            data.table::setnames(TestData, 'Predictions', Val)
          }

        } else {
          rm(RegModel); gc()
        }

      } else {

        # Check for TrainOnFull ----
        if(!is.null(TestData) || !is.null(ValidationData)) {

          # Account for degenerate distributions----
          ArgsList[['degenerate']] <- c(ArgsList[['degenerate']], bucket)

          # Use single value for predictions in the case of zero variance----
          if(!is.null(TestData)) {
            if(bucket == max(looper)) {
              Degenerate <- Degenerate + 1L
              data.table::set(TestData, j = paste0('Predictions', Buckets[bucket - 1L], '+'), value = Buckets[bucket])
              data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData) - 1L)))
            } else {
              Degenerate <- Degenerate + 1L
              data.table::set(TestData, j = paste0('Predictions', Buckets[bucket]), value = Buckets[bucket])
              data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData) - 1L)))
            }
          } else if(!is.null(ValidationData)) {
            if(bucket == max(looper)) {
              Degenerate <- Degenerate + 1L
              data.table::set(ValidationData, j = paste0('Predictions', Buckets[bucket - 1L], '+'), value = Buckets[bucket])
              data.table::setcolorder(ValidationData, c(ncol(ValidationData), 1L:(ncol(ValidationData) - 1L)))
            } else {
              Degenerate <- Degenerate + 1L
              data.table::set(ValidationData, j = paste0('Predictions', Buckets[bucket]), value = Buckets[bucket])
              data.table::setcolorder(ValidationData, c(ncol(ValidationData), 1L:(ncol(ValidationData) - 1L)))
            }
          }

        } else {

          # Account for degenerate distributions----
          ArgsList[['degenerate']] <- c(ArgsList[['degenerate']], bucket)

          # Use single value for predictions in the case of zero variance----
          if(bucket == max(looper)) {
            Degenerate <- Degenerate + 1L
          } else {
            Degenerate <- Degenerate + 1L
          }
        }
      }
    } else {

      # Account for degenerate distributions----
      ArgsList[['degenerate']] <- c(ArgsList[['degenerate']], bucket)
    }
  }

  # Rearrange Column order----
  if(DebugMode) print('Rearrange Column order----')
  if(!TrainOnFull || !is.null(ValidationData)) {

    # Change object names
    if(exists("ValTrue") && ValTrue) {
      TestData <- ValidationData
      ValidationData <- NULL
    }

    # Rearrange cols ----
    if(DebugMode) print('Rearrange cols ----')
    data.table::setcolorder(TestData, c(which(names(TestData) %like% "Predictions"), setdiff(seq_along(TestData), which(names(TestData) %like% "Predictions"))))

    # Final Combination of Predictions ----
    if(DebugMode) print('Final Combination of Predictions ----')
    if(counter > 2L || (counter == 2L && length(Buckets) != 1L)) {
      for(i in rev(looper)) {
        if(i == 1L) {
          TestData[, UpdatedPrediction := TestData[[i]] * TestData[[i + (length(looper))]]]
        } else {
          TestData[, UpdatedPrediction := UpdatedPrediction + TestData[[i]] * TestData[[i + length(looper)]]]
        }
      }
    } else {
      if(Buckets[1L] != 0) {
        TestData[, UpdatedPrediction := TestData[[1L]] * TestData[[3L]] + TestData[[2L]] * TestData[[4L]]]
      } else {
        TestData[, UpdatedPrediction := TestData[[1L]] * TestData[[3L]]]
      }
    }

    # Regression r2 via sqrt of correlation----
    r_squared <- (TestData[, stats::cor(get(TargetColumnName), UpdatedPrediction)]) ^ 2L

    # Regression Save Validation Data to File----
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        data.table::fwrite(TestData, file = file.path(MetaDataPaths, paste0(ModelID,'_ValidationData.csv')))
      } else {
        data.table::fwrite(TestData, file = file.path(Paths, paste0(ModelID,'_ValidationData.csv')))
      }
    }

    # Regression Evaluation Calibration Plot----
    EvaluationPlot <- EvalPlot(
      data = TestData,
      PredictionColName = 'UpdatedPrediction',
      TargetColName = eval(TargetColumnName),
      GraphType = 'calibration',
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))

    # Add Number of Trees to Title----
    EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0('Calibration Evaluation Plot: R2 = ',round(r_squared, 3L)))

    # Save plot to file----
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        ggplot2::ggsave(file.path(MetaDataPaths, paste0(ModelID, '_EvaluationPlot.png')))
      } else {
        ggplot2::ggsave(file.path(Paths, paste0(ModelID, '_EvaluationPlot.png')))
      }
    }

    # Regression Evaluation Calibration Plot----
    EvaluationBoxPlot <- EvalPlot(
      data = TestData,
      PredictionColName = 'UpdatedPrediction',
      TargetColName = eval(TargetColumnName),
      GraphType = 'boxplot',
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))

    # Add Number of Trees to Title----
    EvaluationBoxPlot <- EvaluationBoxPlot + ggplot2::ggtitle(paste0('Calibration Evaluation Plot: R2 = ',round(r_squared, 3L)))

    # Save plot to file----
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        ggplot2::ggsave(file.path(MetaDataPaths, paste0(ModelID,'_EvaluationBoxPlot.png')))
      } else {
        ggplot2::ggsave(file.path(Paths, paste0(ModelID,'_EvaluationBoxPlot.png')))
      }
    }

    # Regression Evaluation Metrics----
    EvaluationMetrics <- data.table::data.table(Metric = c('MAE','MAPE','MSE','R2'),MetricValue = rep(999999, 4L))
    i <- 0L
    MinVal <- min(TestData[, min(get(TargetColumnName))], TestData[, min(UpdatedPrediction)])
    for(metric in c('mae','mape','mse','r2')) {
      i <- i + 1L
      tryCatch({
        if(tolower(metric) == 'mae') {
          TestData[, Metric := abs(get(TargetColumnName) - UpdatedPrediction)]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == 'mape') {
          TestData[, Metric := abs((get(TargetColumnName) - UpdatedPrediction) / (get(TargetColumnName) + 1L))]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == 'mse') {
          TestData[, Metric := (get(TargetColumnName) - UpdatedPrediction) ^ 2L]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == 'r2') {
          TestData[, ':=' (
            Metric1 = (get(TargetColumnName) - mean(get(TargetColumnName))) ^ 2L,
            Metric2 = (get(TargetColumnName) - UpdatedPrediction) ^ 2L)]
          Metric <- 1 - TestData[, sum(Metric2, na.rm = TRUE)] / TestData[, sum(Metric1, na.rm = TRUE)]
        }
        data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric, 4L))
        data.table::set(EvaluationMetrics, i = i, j = 3L, value = NA)
      }, error = function(x) 'skip')
    }

    # Remove Cols----
    TestData[, ':=' (Metric = NULL, Metric1 = NULL, Metric2 = NULL)]

    # Save EvaluationMetrics to File----
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        data.table::fwrite(EvaluationMetrics, file = file.path(MetaDataPaths, paste0(ModelID, '_EvaluationMetrics.csv')))
      } else {
        data.table::fwrite(EvaluationMetrics, file = file.path(Paths, paste0(ModelID, '_EvaluationMetrics.csv')))
      }
    }

    # Regression Partial Dependence----
    ParDepPlots <- list()
    j <- 0L
    ParDepBoxPlots <- list()
    k <- 0L
    for(i in seq_len(min(length(FeatureColNames), NumOfParDepPlots, R_VariableImportance[[1L]][,.N]))) {
      tryCatch({
        Out <- ParDepCalPlots(
          data = TestData,
          PredictionColName = 'UpdatedPrediction',
          TargetColName = eval(TargetColumnName),
          IndepVar = VariableImportance[i, Variable],
          GraphType = 'calibration',
          PercentileBucket = 0.05,
          FactLevels = 10L,
          Function = function(x) mean(x, na.rm = TRUE))
        j <- j + 1L
        ParDepPlots[[paste0(VariableImportance[j, Variable])]] <- Out
      }, error = function(x) 'skip')
      tryCatch({
        Out1 <- ParDepCalPlots(
          data = TestData,
          PredictionColName = 'UpdatedPrediction',
          TargetColName = eval(TargetColumnName),
          IndepVar = VariableImportance[i, Variable],
          GraphType = 'boxplot',
          PercentileBucket = 0.05,
          FactLevels = 10,
          Function = function(x) mean(x, na.rm = TRUE))
        k <- k + 1L
        ParDepBoxPlots[[paste0(VariableImportance[k, Variable])]] <- Out1
      }, error = function(x) 'skip')
    }

    # Regression Save ParDepBoxPlots to file----
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        save(ParDepBoxPlots, file = file.path(MetaDataPaths, paste0(ModelID, '_ParDepBoxPlots.R')))
      } else {
        save(ParDepBoxPlots, file = file.path(Paths, paste0(ModelID, '_ParDepBoxPlots.R')))
      }
    }
  }

  # Save args list to file----
  if(SaveModelObjects) save(ArgsList, file = file.path(Paths, paste0(ModelID, '_HurdleArgList.Rdata')))
  rm(ValTrue, envir = .GlobalEnv)

  # Return Output ----
  if(DebugMode) print("Return Output ----")
  if(!TrainOnFull) {
    return(list(
      ArgsList = if(exists("ArgsList")) ArgsList else NULL,
      ModelList = if(exists("ModelList")) ModelList else NULL,
      ClassifierModel = if(exists("ClassifierModel")) ClassifierModel else NULL,
      ClassificationMetrics = if(exists("ClassEvaluationMetrics")) ClassEvaluationMetrics else NULL,
      FinalTestData = if(exists("TestData")) TestData else NULL,
      EvaluationPlot = if(exists("EvaluationPlot")) EvaluationPlot else NULL,
      EvaluationBoxPlot = if(exists("EvaluationBoxPlot")) EvaluationBoxPlot else NULL,
      EvaluationMetrics = if(exists("EvaluationMetrics")) EvaluationMetrics else NULL,
      ClassifierVariableImportance = if(exists("C_VariableImportance")) C_VariableImportance else NULL,
      RegressionVariableImportance = if(exists("R_VariableImportance")) R_VariableImportance else NULL,
      ClassifierParDepPlots = if(exists("C_ParDepPlots")) C_ParDepPlots else NULL,
      RegressionParDepPlots = if(exists("R_ParDepPlots")) R_ParDepPlots else NULL,
      PartialDependencePlots = if(exists("ParDepPlots")) ParDepPlots else NULL,
      PartialDependenceBoxPlots = if(exists("ParDepBoxPlots")) ParDepBoxPlots else NULL))
  } else {
    return(list(
      ArgsList = if(exists("ArgsList")) ArgsList else NULL,
      ModelList = if(exists("ModelList")) ModelList else NULL,
      ClassifierModel = if(exists("ClassifierModel")) ClassifierModel else NULL))
  }
}
