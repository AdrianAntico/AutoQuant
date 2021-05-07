#' @title AutoXGBoostHurdleModel
#'
#' @description AutoXGBoostHurdleModel is generalized hurdle modeling framework
#'
#' @family Supervised Learning - Compound
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
#' @param IDcols Includes PrimaryDateColumn and any other columns you want returned in the validation data with predictions
#' @param EncodingMethod Choose from 'binary', 'poly_encode', 'backward_difference', 'helmert' for multiclass cases and additionally 'm_estimator', 'credibility', 'woe', 'target_encoding' for classification use cases.
#' @param TransformNumericColumns Transform numeric column inside the AutoCatBoostRegression() function
#' @param SplitRatios Supply vector of partition ratios. For example, c(0.70,0.20,0,10).
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
#' @return Returns AutoXGBoostRegression() model objects: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and the grid used
#' @examples
#' \dontrun{
#' Output <- RemixAutoML::AutoXGBoostHurdleModel(
#'
#'    # Operationalization args
#'    TreeMethod = "hist",
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
#'    IDcols = NULL,
#'
#'    # options
#'    EncodingMethod = "binary",
#'    TransformNumericColumns = NULL,
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
#'    # bandit hyperparameters
#'    Trees = list("classifier" = seq(1000,2000,100),
#'                 "regression" = seq(1000,2000,100)),
#'    eta = list("classifier" = seq(0.05,0.40,0.05),
#'               "regression" = seq(0.05,0.40,0.05)),
#'    max_depth = list("classifier" = seq(4L,16L,2L),
#'                     "regression" = seq(4L,16L,2L)),
#'
#'    # random hyperparameters
#'    min_child_weight = list("classifier" = seq(1.0,10.0,1.0),
#'                            "regression" = seq(1.0,10.0,1.0)),
#'    subsample = list("classifier" = seq(0.55,1.0,0.05),
#'                     "regression" = seq(0.55,1.0,0.05)),
#'    colsample_bytree = list("classifier" = seq(0.55,1.0,0.05),
#'                            "regression" = seq(0.55,1.0,0.05)))
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
                                   IDcols = NULL,
                                   EncodingMethod = "binary",
                                   TransformNumericColumns = NULL,
                                   SplitRatios = c(0.70, 0.20, 0.10),
                                   SaveModelObjects = FALSE,
                                   ReturnModelObjects = TRUE,
                                   NumOfParDepPlots = 10L,
                                   GridTune = FALSE,
                                   grid_eval_metric = "accuracy",
                                   MaxModelsInGrid = 1L,
                                   BaselineComparison = "default",
                                   MaxRunsWithoutNewWinner = 10L,
                                   MaxRunMinutes = 60L,
                                   Trees = list("classifier" = seq(1000,2000,100), "regression" = seq(1000,2000,100)),
                                   eta = list("classifier" = seq(0.05,0.40,0.05), "regression" = seq(0.05,0.40,0.05)),
                                   max_depth = list("classifier" = seq(4L,16L,2L), "regression" = seq(4L,16L,2L)),
                                   min_child_weight = list("classifier" = seq(1.0,10.0,1.0), "regression" = seq(1.0,10.0,1.0)),
                                   subsample = list("classifier" = seq(0.55,1.0,0.05), "regression" = seq(0.55,1.0,0.05)),
                                   colsample_bytree = list("classifier" = seq(0.55,1.0,0.05), "regression" = seq(0.55,1.0,0.05))) {

  # Store args----
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

  # Check args----
  if(is.character(Buckets) | is.factor(Buckets) | is.logical(Buckets)) return("Buckets needs to be a numeric scalar or vector")
  if(!is.logical(SaveModelObjects)) return("SaveModelOutput needs to be set to either TRUE or FALSE")
  if(!is.logical(GridTune)) return("GridTune needs to be either TRUE or FALSE")

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

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Ensure Paths and metadata_path exists----
  if(!is.null(Paths)) if(!dir.exists(normalizePath(Paths))) dir.create(normalizePath(Paths))
  if(is.null(MetaDataPaths)) MetaDataPaths <- Paths else if(!dir.exists(normalizePath(MetaDataPaths))) dir.create(normalizePath(MetaDataPaths))

  # Data.table check----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(ValidationData)) if(!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)
  if(!is.null(TestData)) if(!data.table::is.data.table(TestData)) data.table::setDT(TestData)

  # IDcols to Names----
  if(!is.null(IDcols)) if(is.numeric(IDcols) | is.integer(IDcols)) IDcols <- names(data)[IDcols]

  # FeatureColumnNames----
  if(is.numeric(FeatureColNames) | is.integer(FeatureColNames)) FeatureNames <- names(data)[FeatureColNames] else FeatureNames <- FeatureColNames

  # Add target bucket column----
  if(length(Buckets) == 1L) {
    data.table::set(data, i = which(data[[eval(TargetColumnName)]] <= Buckets[1L]), j = "Target_Buckets", value = 0L)
    data.table::set(data, i = which(data[[eval(TargetColumnName)]] > Buckets[1L]), j = "Target_Buckets", value = 1L)
  } else {
    for(i in seq_len(length(Buckets) + 1L)) {
      if(i == 1L) {
        data.table::set(data, i = which(data[[eval(TargetColumnName)]] <= Buckets[i]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      } else if(i == length(Buckets) + 1L) {
        data.table::set(data, i = which(data[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(paste0(Buckets[i - 1L], "+")))
      } else {
        data.table::set(data, i = which(data[[eval(TargetColumnName)]] <= Buckets[i] & data[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      }
    }
  }

  # Add target bucket column----
  if(!is.null(ValidationData)) {
    ValidationData[, Target_Buckets := as.factor(Buckets[1])]
    for(i in seq_len(length(Buckets) + 1L)) {
      if(i == 1L) {
        data.table::set(ValidationData, i = which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      } else if(i == length(Buckets) + 1L) {
        data.table::set(ValidationData, i = which(ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(paste0(Buckets[i - 1L], "+")))
      } else {
        data.table::set(ValidationData, i = which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i] & ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      }
    }
  }

  # Add target bucket column----
  if (!is.null(TestData)) {
    TestData[, Target_Buckets := as.factor(Buckets[1L])]
    for(i in seq_len(length(Buckets) + 1L)) {
      if(i == 1L) {
        data.table::set(TestData, i = which(TestData[[eval(TargetColumnName)]] <= Buckets[i]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      } else if(i == length(Buckets) + 1L) {
        data.table::set(TestData, i = which(TestData[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(paste0(Buckets[i - 1L], "+")))
      } else {
        data.table::set(TestData, i = which(TestData[[eval(TargetColumnName)]] <= Buckets[i] & TestData[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      }
    }
  }

  # AutoDataPartition if Validation and TestData are NULL----
  if(is.null(ValidationData) & is.null(TestData) & !TrainOnFull) {
    DataSets <- AutoDataPartition(
      data = data,
      NumDataSets = 3L,
      Ratios = SplitRatios,
      PartitionType = "random",
      StratifyColumnNames = "Target_Buckets",
      TimeColumnName = NULL)
    data <- DataSets$TrainData
    ValidationData <- DataSets$ValidationData
    TestData <- DataSets$TestData
    rm(DataSets)
  }

  # Begin classification model building----
  if(length(Buckets) == 1L) {
    ClassifierModel <- AutoXGBoostClassifier(

      # general args----
      eval_metric = "auc",
      TrainOnFull = TrainOnFull,
      TreeMethod = TreeMethod,
      PassInGrid = PassInGrid,
      NThreads = NThreads,
      model_path = Paths,
      metadata_path = MetaDataPaths,
      ModelID = ModelID,

      # options----
      ReturnModelObjects = TRUE,
      EncodingMethod = EncodingMethod,
      ReturnFactorLevels = TRUE,
      Verbose = 1L,
      NumOfParDepPlots = NumOfParDepPlots,
      SaveModelObjects = SaveModelObjects,

      # data args----
      TargetColumnName = "Target_Buckets",
      data = data,
      ValidationData = ValidationData,
      TestData = TestData,
      FeatureColNames = FeatureNames,
      IDcols = IDcols,

      # Grid tuning args----
      GridTune = GridTune,
      BaselineComparison = BaselineComparison,
      MaxModelsInGrid = MaxModelsInGrid,
      MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
      MaxRunMinutes = MaxRunMinutes,

      # bandit args----
      Trees = ClassifierTrees,
      eta = Classifiereta,
      max_depth = Classifiermax_depth,

      # random args----
      min_child_weight = Classifiermin_child_weight,
      subsample = Classifiersubsample,
      colsample_bytree = Classifiercolsample_bytree)

  } else {
    ClassifierModel <- AutoXGBoostMultiClass(

      # type
      grid_eval_metric = grid_eval_metric,
      eval_metric = "merror",
      LossFunction = 'multi:softprob',

      # general args----
      TrainOnFull = TrainOnFull,
      TreeMethod = TreeMethod,
      PassInGrid = PassInGrid,
      NThreads = NThreads,
      model_path = Paths,
      metadata_path = MetaDataPaths,
      ModelID = ModelID,
      EncodingMethod = EncodingMethod,

      # options----
      ReturnModelObjects = TRUE,
      ReturnFactorLevels = TRUE,
      NumOfParDepPlots = NumOfParDepPlots,
      SaveModelObjects = SaveModelObjects,
      Verbose = 1L,

      # data args----
      data = data,
      ValidationData = ValidationData,
      TestData = TestData,
      TargetColumnName = "Target_Buckets",
      FeatureColNames = FeatureNames,
      IDcols = IDcols,

      # Grid tuning args----
      GridTune = GridTune,
      BaselineComparison = BaselineComparison,
      MaxModelsInGrid = MaxModelsInGrid,
      MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
      MaxRunMinutes = MaxRunMinutes,

      # bandit args----
      Trees = ClassifierTrees,
      eta = Classifiereta,
      max_depth = Classifiermax_depth,

      # random args----
      min_child_weight = Classifiermin_child_weight,
      subsample = Classifiersubsample,
      colsample_bytree = Classifiercolsample_bytree)
  }

  # Store metadata----
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
  ArgsList[["FactorLevelsList"]] <- FactorLevelsList
  rm(ClassifierModel)

  # Add Target to IDcols----
  IDcols <- c(IDcols, TargetColumnName)

  # Define args----
  if(length(Buckets) == 1L) {
    TargetType <- "Classification"
    Objective <- NULL
  } else {
    TargetType <- "Multiclass"
    Objective <- "multi:softprob"
  }

  # Model Scoring----
  if(!TrainOnFull) {
    TestData <- AutoXGBoostScoring(
      ReturnShapValues = FALSE,
      EncodingMethod = EncodingMethod,
      TargetType = TargetType,
      ScoringData = TestData,
      FeatureColumnNames = FeatureNames,
      IDcols = IDcols,
      FactorLevelsList = FactorLevelsList,
      TargetLevels = TargetLevels,
      Objective = Objective,
      OneHot = FALSE,
      ModelObject = ClassModel,
      ModelPath = NULL,
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

    # Change name for classification----
    if(TargetType == "Classification") {
      data.table::setnames(TestData, "Predictions", "Predictions_C1")
      TestData[, Predictions_C0 := 1 - Predictions_C1]
      data.table::setcolorder(TestData, c(ncol(TestData), 1L, 2L:(ncol(TestData) - 1L)))
    }

    # Updates
    if(length(Buckets) != 1L) data.table::setnames(TestData, "Predictions", "Predictions_MultiClass")
    data.table::set(ValidationData, j = "Target_Buckets", value = NULL)
  }


  # Remove Model Object----
  rm(ClassModel)

  # Remove Target_Buckets----
  data.table::set(data, j = "Target_Buckets", value = NULL)

  # Remove Target From IDcols----
  IDcols <- IDcols[!(IDcols %chin% TargetColumnName)]

  # Change Name of Predicted MultiClass Column----
  counter <- max(rev(seq_len(length(Buckets) + 1L))) + 1L
  Degenerate <- 0L

  # Begin regression model building----
  for(bucket in rev(seq_len(length(Buckets) + 1L))) {

    # Partition data----
    if(bucket == max(seq_len(length(Buckets) + 1L))) {
      if(!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- TestData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
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
      } else {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
        validBucket <- NULL
        testBucket <- NULL
      }
    } else {
      if(!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- TestData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
      } else {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- NULL
        testBucket <- NULL
      }
    }

    # Create Modified IDcols----
    IDcolsModified <- c(IDcols, setdiff(names(TestData), names(trainBucket)), TargetColumnName)

    # AutoCatBoostRegression()----
    if(trainBucket[, .N] != 0L) {

      # If there is some variance then build model
      if(var(trainBucket[[eval(TargetColumnName)]]) > 0L) {

        # Increment----
        counter <- counter - 1L

        # Modify filepath and file name----
        if(bucket == max(seq_len(length(Buckets) + 1L))) ModelIDD <- paste0(ModelID,"_",bucket,"+") else ModelIDD <- paste0(ModelID, "_", bucket)

        # Build model----
        RegModel <- AutoXGBoostRegression(

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
          data = trainBucket,
          TrainOnFull = TrainOnFull,
          ValidationData = validBucket,
          TestData = testBucket,
          TargetColumnName = TargetColumnName,
          FeatureColNames = FeatureNames,
          IDcols = IDcols,
          TransformNumericColumns = TransformNumericColumns,

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
        if(ReturnModelObjects | SaveModelObjects) ModelList[[ModelIDD]] <- RegressionModel
        if(!is.null(TransformNumericColumns)) {
          ArgsList[[paste0("TransformationResults_", ModelIDD)]] <- RegModel$TransformationResults
        } else {
          ArgsList[[paste0("TransformationResults_", ModelIDD)]] <- NULL
        }

        # Garbage Collection----
        gc()

        # Define TranformationResults----
        if(!is.null(TransformNumericColumns)) Trans <- TransformationResults  else TransformationResults <- NULL

        # Score model----
        TestData <- AutoXGBoostScoring(
          TargetType = "regression",
          ScoringData = TestData,
          FeatureColumnNames = FeatureNames,
          IDcols = IDcolsModified,
          FactorLevelsList = FactorLevelsList,
          EncodingMethod = EncodingMethod,
          OneHot = FALSE,
          ModelObject = RegressionModel,
          ModelPath = Paths,
          ModelID = ModelIDD,
          ReturnFeatures = TRUE,
          TransformNumeric = if(is.null(ArgsList[[paste0("TransformationResults_", ModelIDD)]])) FALSE else TRUE,
          BackTransNumeric = if(is.null(ArgsList[[paste0("TransformationResults_", ModelIDD)]])) FALSE else TRUE,
          TargetColumnName = eval(TargetColumnName),
          TransformationObject = TransformationResults,
          TransID = NULL,
          TransPath = NULL,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = FALSE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1)

        # Clear TestModel From Memory----
        rm(RegModel,RegressionModel)

        # Change prediction name to prevent duplicates----
        if(bucket == max(seq_len(length(Buckets) + 1L))) Val <- paste0("Predictions_", bucket - 1L, "+") else Val <- paste0("Predictions_", bucket)
        data.table::setnames(TestData, "Predictions", Val)

      } else {

        # Check for TrainOnFull ----
        if(!is.null(TestData)) {

          # Account for degenerate distributions----
          ArgsList[["constant"]] <- c(ArgsList[["constant"]], bucket)

          # Use single value for predictions in the case of zero variance----
          if(bucket == max(seq_len(length(Buckets) + 1L))) {
            Degenerate <- Degenerate + 1L
            data.table::set(TestData, j = paste0("Predictions", Buckets[bucket - 1L], "+"), value = Buckets[bucket])
            data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData) - 1L)))
          } else {
            Degenerate <- Degenerate + 1L
            data.table::set(TestData, j = paste0("Predictions", Buckets[bucket]), value = Buckets[bucket])
            data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData) - 1L)))
          }
        } else {

          # Account for degenerate distributions----
          ArgsList[["constant"]] <- c(ArgsList[["constant"]], bucket)

          # Use single value for predictions in the case of zero variance----
          if(bucket == max(seq_len(length(Buckets) + 1L))) {
            Degenerate <- Degenerate + 1L
          } else {
            Degenerate <- Degenerate + 1L
          }
        }
      }
    } else {

      # Account for degenerate distributions----
      ArgsList[["degenerate"]] <- c(ArgsList[["degenerate"]], bucket)
    }
  }

  # Rearrange Column order----
  if(!TrainOnFull) {
    counter <- length(Buckets)
    if(counter > 2L) {
      if(length(IDcols) != 0L) {
        if(Degenerate == 0L) {
          data.table::setcolorder(TestData, c(2L:(1L + length(IDcols)), 1L, (2L + length(IDcols)):ncol(TestData)))
          data.table::setcolorder(TestData, c(1L:length(IDcols), (length(IDcols) + counter + 1L), (length(IDcols) + counter + 1L + counter +1L):ncol(TestData), (length(IDcols) + 1L):(length(IDcols) + counter), (length(IDcols) + counter + 2L):(length(IDcols)+counter + 1L + counter)))
        } else {
          data.table::setcolorder(TestData, c(3L:(2L + length(IDcols)), 1L:2L, (3L + length(IDcols)):ncol(TestData)))
          data.table::setcolorder(TestData, c(1L:length(IDcols), (length(IDcols) + counter + 1L + Degenerate), (length(IDcols) + counter + 3L + counter + Degenerate):ncol(TestData), (length(IDcols) + 1L):(length(IDcols) + counter + Degenerate), (length(IDcols) + counter + 2L + Degenerate):(length(IDcols)+counter+counter+Degenerate + 2L)))
        }
      } else {
        data.table::setcolorder(TestData, c(1L:(counter + Degenerate), (2L + counter + Degenerate):(1L + 2L * (counter + Degenerate)), (1L + counter + Degenerate), (2L + 2L * (counter + Degenerate)):ncol(TestData)))
        data.table::setcolorder(TestData, c((2L * (counter + Degenerate) + 1L):ncol(TestData), 1L:(2L * (counter + Degenerate))))
      }
    } else if(counter == 2L & length(Buckets) == 1L) {
      if(length(IDcols) != 0L) data.table::setcolorder(TestData, c(1L,2L, (2L + length(IDcols) + 1L):ncol(TestData), 3L:(2L + length(IDcols))))
    } else if(counter == 2L & length(Buckets) != 1L) {
      if(length(IDcols) != 0L) {
        data.table::setcolorder(TestData, c(1L:counter, (counter + length(IDcols) + 1L):(counter + length(IDcols) + 2L + length(Buckets) + 1L), which(names(TestData) %in% c(setdiff(names(TestData), names(TestData)[c(1L:counter, (counter + length(IDcols) + 1L):(counter + length(IDcols) + 2L + length(Buckets) + 1L))])))))
        data.table::setcolorder(TestData, c(1L:(counter + 1L), (counter + 1L + 2L):(counter + 1L + 2L + counter), which(names(TestData) %in% setdiff(names(TestData), names(TestData)[c(1L:(counter + 1L), (counter + 1L + 2L):(counter + 1L + 2L + counter))]))))
      } else {
        data.table::setcolorder(TestData, c(1L:(counter + 1L), (counter + 3L):(3L + 2 * counter), (counter + 2L), which(!names(TestData) %in% names(TestData)[c(1L:(counter + 1L), (counter + 3L):(3L + 2 * counter), (counter + 2L))])))
      }
    } else {
      if(length(IDcols) != 0L) {
        data.table::setcolorder(TestData, c(1L:2L, (3L + length(IDcols)):((3L + length(IDcols)) + 1L), 3L:(2L + length(IDcols)), (((3L + length(IDcols)) + 2L):ncol(TestData))))
      }
    }

    # Final Combination of Predictions----
    if(counter > 2L | (counter == 2L & length(Buckets) != 1L)) {
      for(i in seq_len(length(Buckets) + 1L)) {
        if(i == 1L) {
          TestData[, UpdatedPrediction := TestData[[i]] * TestData[[i + (length(Buckets) + 1L)]]]
        } else {
          TestData[, UpdatedPrediction := UpdatedPrediction + TestData[[i]] * TestData[[i + (length(Buckets) + 1L)]]]
        }
      }
    } else {
      TestData[, UpdatedPrediction := TestData[[1L]] * TestData[[3L]] + TestData[[2L]] * TestData[[4L]]]
    }

    # R-Sq----
    r_squared <- (TestData[, stats::cor(get(TargetColumnName), UpdatedPrediction)]) ^ 2L

    # Regression Save Validation Data to File----
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        data.table::fwrite(TestData, file = file.path(normalizePath(MetaDataPaths), paste0(ModelID, "_ValidationData.csv")))
      } else {
        data.table::fwrite(TestData, file = file.path(normalizePath(Paths), paste0(ModelID, "_ValidationData.csv")))
      }
    }

    # Regression Evaluation Calibration Plot----
    EvaluationPlot <- EvalPlot(
      data = TestData,
      PredictionColName = "UpdatedPrediction",
      TargetColName = eval(TargetColumnName),
      GraphType = "calibration",
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))

    # Add Number of Trees to Title----
    EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(r_squared, 3L)))

    # Save plot to file----
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        ggplot2::ggsave(file.path(normalizePath(MetaDataPaths), paste0(ModelID, "_EvaluationPlot.png")))
      } else {
        ggplot2::ggsave(file.path(normalizePath(Paths), paste0(ModelID, "_EvaluationPlot.png")))
      }
    }

    # Regression Evaluation Calibration Plot----
    EvaluationBoxPlot <- EvalPlot(
      data = TestData,
      PredictionColName = "UpdatedPrediction",
      TargetColName = eval(TargetColumnName),
      GraphType = "boxplot",
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))

    # Add Number of Trees to Title----
    EvaluationBoxPlot <- EvaluationBoxPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(r_squared, 3L)))

    # Save plot to file----
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        ggplot2::ggsave(file.path(normalizePath(MetaDataPaths), paste0(ModelID, "_EvaluationBoxPlot.png")))
      } else {
        ggplot2::ggsave(file.path(normalizePath(Paths), paste0(ModelID, "_EvaluationBoxPlot.png")))
      }
    }

    # Regression Evaluation Metrics----
    EvaluationMetrics <- data.table::data.table(Metric = c("MAE","MAPE","MSE","R2"), MetricValue = rep(999999, 8L))
    i <- 0L
    MinVal <- min(TestData[, min(get(TargetColumnName))], TestData[, min(UpdatedPrediction)])
    for(metric in c("mae", "mape", "mse", "r2")) {
      i <- i + 1L
      tryCatch({
        if(tolower(metric) == "mae") {
          TestData[, Metric := abs(get(TargetColumnName) - UpdatedPrediction)]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == "mape") {
          TestData[, Metric := abs((get(TargetColumnName) - UpdatedPrediction) / (get(TargetColumnName) + 1L))]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == "mse") {
          TestData[, Metric := (get(TargetColumnName) - UpdatedPrediction) ^ 2L]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == "r2") {
          TestData[, ':=' (Metric1 = (get(TargetColumnName) - mean(get(TargetColumnName))) ^ 2L, Metric2 = (get(TargetColumnName) - UpdatedPrediction) ^ 2L)]
          Metric <- 1 - TestData[, sum(Metric2, na.rm = TRUE)] / TestData[, sum(Metric1, na.rm = TRUE)]
        }
        data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric, 4L))
        data.table::set(EvaluationMetrics, i = i, j = 3L, value = NA)
      }, error = function(x) "skip")
    }

    # Remove Cols----
    TestData[, ':=' (Metric = NULL, Metric1 = NULL, Metric2 = NULL, Metric3 = NULL)]

    # Save EvaluationMetrics to File
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999L]
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        data.table::fwrite(EvaluationMetrics, file = file.path(normalizePath(MetaDataPaths), paste0(ModelID, "_EvaluationMetrics.csv")))
      } else {
        data.table::fwrite(EvaluationMetrics, file = file.path(normalizePath(Paths), paste0(ModelID, "_EvaluationMetrics.csv")))
      }
    }

    # Regression Partial Dependence----
    ParDepPlots <- list()
    ParDepBoxPlots <- list()
    for(RowNum in seq_len(min(length(FeatureColNames), NumOfParDepPlots, VariableImportance[,.N]))) {
      tryCatch({
        Out <- ParDepCalPlots(
          data = TestData,
          PredictionColName = "UpdatedPrediction",
          TargetColName = eval(TargetColumnName),
          IndepVar = VariableImportance[RowNum, Feature],
          GraphType = "calibration",
          PercentileBucket = 0.05,
          FactLevels = 10L,
          Function = function(x) mean(x, na.rm = TRUE))
        ParDepPlots[[paste0(VariableImportance[RowNum, Feature])]] <- Out
      }, error = function(x) "skip")
      tryCatch({
        Out1 <- ParDepCalPlots(
          data = TestData,
          PredictionColName = "UpdatedPrediction",
          TargetColName = eval(TargetColumnName),
          IndepVar = VariableImportance[RowNum, Feature],
          GraphType = "boxplot",
          PercentileBucket = 0.05,
          FactLevels = 10L,
          Function = function(x) mean(x, na.rm = TRUE))
        ParDepBoxPlots[[paste0(VariableImportance[RowNum, Feature])]] <- Out1
      }, error = function(x) "skip")
    }

    # Regression Save ParDepBoxPlots to file----
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        save(ParDepPlots, file = file.path(normalizePath(MetaDataPaths), paste0(ModelID, "_ParDepPlots.R")))
        save(ParDepBoxPlots, file = file.path(normalizePath(MetaDataPaths), paste0(ModelID, "_ParDepBoxPlots.R")))
      } else {
        save(ParDepPlots, file = file.path(normalizePath(Paths), paste0(ModelID, "_ParDepPlots.R")))
        save(ParDepBoxPlots, file = file.path(normalizePath(Paths), paste0(ModelID, "_ParDepBoxPlots.R")))
      }
    }
  }

  # Save args list to file----
  if(SaveModelObjects) save(ArgsList, file = file.path(normalizePath(Paths), paste0(ModelID, "_HurdleArgList.Rdata")))

  # Return Output----
  if(!TrainOnFull) {
    return(list(
      ArgsList = ArgsList,
      ModelList = ModelList,
      ClassificationMetrics = ClassEvaluationMetrics,
      FinalTestData = TestData,
      EvaluationPlot = EvaluationPlot,
      EvaluationBoxPlot = EvaluationBoxPlot,
      EvaluationMetrics = EvaluationMetrics,
      PartialDependencePlots = ParDepPlots,
      PartialDependenceBoxPlots = ParDepBoxPlots))
  } else {
    return(list(
      ArgsList = ArgsList,
      ModelList = ModelList))
  }
}
