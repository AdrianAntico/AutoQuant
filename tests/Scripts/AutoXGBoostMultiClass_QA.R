# Testing
XGBoost_QA_Results_MultiClass <- data.table::CJ(
  TOF = c(TRUE,FALSE),
  GridTune = c(TRUE,FALSE),
  Success = "Failure",
  PartitionInFunction = c(TRUE,FALSE))

# Remove impossible combinations
XGBoost_QA_Results_MultiClass <- XGBoost_QA_Results_MultiClass[!(TOF & GridTune)]
XGBoost_QA_Results_MultiClass <- XGBoost_QA_Results_MultiClass[!(PartitionInFunction & TOF)]
XGBoost_QA_Results_MultiClass[, RunNumber := seq_len(.N)]
XGBoost_QA_Results_MultiClass[, Test_Accuracy := 0.0]
XGBoost_QA_Results_MultiClass[, Train_Accuracy := 0.0]
XGBoost_QA_Results_MultiClass[, Test_MicroAUC := 0.0]
XGBoost_QA_Results_MultiClass[, Train_MicroAUC := 0.0]
XGBoost_QA_Results_MultiClass[, Test_Logloss := 0.0]
XGBoost_QA_Results_MultiClass[, Train_Logloss := 0.0]

#      TOF GridTune Success RunNumber
# 1: FALSE    FALSE Failure         1
# 2: FALSE     TRUE Failure         2
# 3:  TRUE    FALSE Failure         3

# AutoXGBoostMultiClass
# run = 5
# run = 2
# run = 4
for(run in seq_len(XGBoost_QA_Results_MultiClass[,.N])) {

  # Define values
  tof <- XGBoost_QA_Results_MultiClass[run, TOF]
  PartitionInFunction <- XGBoost_QA_Results_MultiClass[run, PartitionInFunction]
  gridtune <- XGBoost_QA_Results_MultiClass[run, GridTune]
  Tar <- "Adrian"

  # Refresh data
  data <- RemixAutoML::FakeDataGenerator(
    Correlation = 0.85,
    N = 25000L,
    ID = 2L,
    AddWeightsColumn = TRUE,
    ZIP = 0L,
    AddDate = TRUE,
    Classification = FALSE,
    MultiClass = TRUE)

  # Add Diff data
  data <- RemixAutoML::AutoDiffLagN(
    data = data,
    DateVariable = "DateTime",
    GroupVariables = c("Factor_2"),
    DiffVariables = names(data)[!names(data) %in% c("IDcol_1","IDcol_2","Adrian","DateTime","Factor_1","Factor_2")],
    DiffDateVariables = NULL,
    DiffGroupVariables = NULL,
    NLag1 = 0,
    NLag2 = 1,
    Sort = TRUE,
    RemoveNA = TRUE)

  # Partition Data
  if(!tof && !PartitionInFunction) {
    Sets <- RemixAutoML::AutoDataPartition(
      data = data,
      NumDataSets = 3,
      Ratios = c(0.7,0.2,0.1),
      PartitionType = "random",
      StratifyColumnNames = "Adrian",
      TimeColumnName = NULL)
    TTrainData <- Sets$TrainData
    VValidationData <- Sets$ValidationData
    TTestData <- Sets$TestData
    rm(Sets)
  } else {
    TTrainData <- data.table::copy(data)
    VValidationData <- NULL
    TTestData <- NULL
  }

  # Run function
  TestModel <- tryCatch({RemixAutoML::AutoXGBoostMultiClass(

    # GPU or CPU
    TreeMethod = "hist",
    NThreads = parallel::detectCores(),

    # Metadata arguments
    OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
    model_path = normalizePath("./"),
    metadata_path = normalizePath("./"),
    ModelID = "Test_Model_1",
    ReturnFactorLevels = TRUE,
    ReturnModelObjects = TRUE,
    SaveModelObjects = FALSE,
    EncodingMethod = "credibility",
    DebugMode = TRUE,

    # Data arguments
    data = TTrainData,
    TrainOnFull = tof,
    ValidationData = VValidationData,
    TestData = TTestData,
    TargetColumnName = "Adrian",
    FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime",Tar)],
    IDcols = c("IDcol_1","IDcol_2","DateTime"),

    # Model evaluation
    eval_metric = "merror",
    LossFunction = 'multi:softprob',
    grid_eval_metric = "accuracy",
    NumOfParDepPlots = 3L,

    # Grid tuning arguments
    PassInGrid = NULL,
    GridTune = gridtune,
    BaselineComparison = "default",
    MaxModelsInGrid = 10L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    Verbose = 1L,

    # ML Args
    Trees = if(!gridtune) 50L else c(50,51,52,53,54,55),
    eta = if(!gridtune) 0.05 else c(0.05,0.06,0.07,0.08,0.09),
    max_depth = if(!gridtune) 4L else c(4,5,6,7,8,9,10),
    min_child_weight = if(!gridtune) 1.0 else c(1,2,3,4),
    subsample = if(!gridtune) 0.55 else c(0.50,0.55,0.60,0.65),
    colsample_bytree = if(!gridtune) 0.55 else c(0.55,0.65,0.7,0.75,0.8))}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) {
    XGBoost_QA_Results_MultiClass[run, Success := "Success"]
    XGBoost_QA_Results_MultiClass[run, Test_Accuracy := TestModel$MultinomialMetrics$TestData[Metric == 'Accuracy', MetricValue]]
    if(!tof) XGBoost_QA_Results_MultiClass[run, Train_Accuracy := TestModel$MultinomialMetrics$TrainData[Metric == 'Accuracy', MetricValue]]
    XGBoost_QA_Results_MultiClass[run, Test_MicroAUC := TestModel$MultinomialMetrics$TestData[Metric == 'MicroAUC', MetricValue]]
    if(!tof) XGBoost_QA_Results_MultiClass[run, Train_MicroAUC := TestModel$MultinomialMetrics$TrainData[Metric == 'MicroAUC', MetricValue]]
    XGBoost_QA_Results_MultiClass[run, Test_Logloss := TestModel$MultinomialMetrics$TestData[Metric == 'LogLoss', MetricValue]]
    if(!tof) XGBoost_QA_Results_MultiClass[run, Train_Logloss := TestModel$MultinomialMetrics$TrainData[Metric == 'LogLoss', MetricValue]]
  }
  TestModel <- NULL
  Sys.sleep(5)
  RemixAutoML:::Post_Append_Helper(XGBoost_QA_Results_MultiClass,'AutoXGBoostMultiClass_QA')
}

# Testing ----
# XGBoost_QA_Results_MultiClass <- data.table::CJ(
#   TOF = c(TRUE,FALSE),
#   GridTune = c(TRUE,FALSE),
#   Success = "Failure",
#   PartitionInFunction = c(TRUE,FALSE))
#
# # Remove impossible combinations
# XGBoost_QA_Results_MultiClass <- XGBoost_QA_Results_MultiClass[!(TOF & GridTune)]
# XGBoost_QA_Results_MultiClass <- XGBoost_QA_Results_MultiClass[!(PartitionInFunction & TOF)]
# XGBoost_QA_Results_MultiClass[, RunNumber := seq_len(.N)]
# XGBoost_QA_Results_MultiClass[, Test_Accuracy := 0.0]
# XGBoost_QA_Results_MultiClass[, Train_Accuracy := 0.0]
# XGBoost_QA_Results_MultiClass[, Test_MicroAUC := 0.0]
# XGBoost_QA_Results_MultiClass[, Train_MicroAUC := 0.0]
# XGBoost_QA_Results_MultiClass[, Test_Logloss := 0.0]
# XGBoost_QA_Results_MultiClass[, Train_Logloss := 0.0]
#
#
# # Main Function Defaults ----
# library(RemixAutoML)
# library(data.table)
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/GridTuning.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/XGBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CharacterTypes.R"))
#
# Testing
# XGBoost_QA_Results_MultiClass <- data.table::CJ(
#   TOF = c(TRUE,FALSE),
#   GridTune = c(TRUE,FALSE),
#   Success = "Failure",
#   PartitionInFunction = c(TRUE,FALSE))
#
# # Remove impossible combinations
# XGBoost_QA_Results_MultiClass <- XGBoost_QA_Results_MultiClass[!(TOF & GridTune)]
# XGBoost_QA_Results_MultiClass <- XGBoost_QA_Results_MultiClass[!(PartitionInFunction & TOF)]
# XGBoost_QA_Results_MultiClass[, RunNumber := seq_len(.N)]
#
# # Runs
# run = 2
# # run = 6
#
#
# # Define values
# tof <- XGBoost_QA_Results_MultiClass[run, TOF]
# gridtune <- XGBoost_QA_Results_MultiClass[run, GridTune]
# Tar <- "Adrian"
#
# # Refresh data
# data <- RemixAutoML::FakeDataGenerator(
#   Correlation = 0.85,
#   N = 25000L,
#   ID = 2L,
#   AddWeightsColumn = TRUE,
#   ZIP = 0L,
#   AddDate = TRUE,
#   Classification = FALSE,
#   MultiClass = TRUE)
#
# # Add Diff data
# data <- RemixAutoML::AutoDiffLagN(
#   data = data,
#   DateVariable = "DateTime",
#   GroupVariables = c("Factor_2"),
#   DiffVariables = names(data)[!names(data) %in% c("IDcol_1","IDcol_2","Adrian","DateTime","Factor_1","Factor_2")],
#   DiffDateVariables = NULL,
#   DiffGroupVariables = NULL,
#   NLag1 = 0,
#   NLag2 = 1,
#   Sort = TRUE,
#   RemoveNA = TRUE)
#
# # Partition Data
# if(!tof) {
#   Sets <- RemixAutoML::AutoDataPartition(
#     data = data,
#     NumDataSets = 3,
#     Ratios = c(0.7,0.2,0.1),
#     PartitionType = "random",
#     StratifyColumnNames = "Adrian",
#     TimeColumnName = NULL)
#   TTrainData <- Sets$TrainData
#   VValidationData <- Sets$ValidationData
#   TTestData <- Sets$TestData
#   rm(Sets)
# } else {
#   TTrainData <- data.table::copy(data)
#   VValidationData <- NULL
#   TTestData <- NULL
# }
#
# # Main Args
# OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData")
# TreeMethod = "hist"
# NThreads = parallel::detectCores()
# # Metadata argument
# model_path = normalizePath("./")
# metadata_path = normalizePath("./")
# ModelID = "Test_Model_1"
# ReturnFactorLevels = TRUE
# ReturnModelObjects = TRUE
# SaveModelObjects = TRUE
# EncodingMethod = "credibility"
# DebugMode = TRUE
# # Data argument
# data = TTrainData
# TrainOnFull = tof
# ValidationData = VValidationData
# TestData = TTestData
# TargetColumnName = "Adrian"
# FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime",Tar)]
# WeightsColumnName = "Weights"
# IDcols = c("IDcol_1","IDcol_2","DateTime")
# # Model evaluatio
# eval_metric = "merror"
# LossFunction = 'multi:softprob'
# grid_eval_metric = "accuracy"
# NumOfParDepPlots = 3L
# # Grid tuning argument
# PassInGrid = NULL
# GridTune = gridtune
# BaselineComparison = "default"
# MaxModelsInGrid = 10L
# MaxRunsWithoutNewWinner = 20L
# MaxRunMinutes = 24L*60L
# Verbose = 1L
# # ML Arg
# Trees = if(!gridtune) 50L else c(50,51,52,53,54,55)
# eta = if(!gridtune) 0.05 else c(0.05,0.06,0.07,0.08,0.09)
# max_depth = if(!gridtune) 4L else c(4,5,6,7,8,9,10)
# min_child_weight = if(!gridtune) 1.0 else c(1,2,3,4)
# subsample = if(!gridtune) 0.55 else c(0.50,0.55,0.60,0.65)
# colsample_bytree = if(!gridtune) 0.55 else c(0.55,0.65,0.7,0.75,0.8)

# XGBoostDataPrep(Algo="xgboost", ModelType="multiclass", data.=data, ValidationData.=ValidationData, TestData.=TestData, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, WeightsColumnName.=WeightsColumnName, IDcols.=IDcols, TransformNumericColumns.=NULL, Methods.=NULL, ModelID.=ModelID, model_path.=model_path, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, ReturnFactorLevels.=ReturnFactorLevels, EncodingMethod.=EncodingMethod, DebugMode.=DebugMode)
# Algo="xgboost"
# ModelType="multiclass"
# data.=data
# ValidationData.=ValidationData
# TestData.=TestData
# TargetColumnName.=TargetColumnName
# FeatureColNames.=FeatureColNames
# WeightsColumnName.=WeightsColumnName
# IDcols.=IDcols
# TransformNumericColumns.=NULL
# Methods.=NULL
# ModelID.=ModelID
# model_path.=model_path
# TrainOnFull.=TrainOnFull
# SaveModelObjects.=SaveModelObjects
# ReturnFactorLevels.=ReturnFactorLevels
# EncodingMethod.=EncodingMethod
# DebugMode.=DebugMode

# Train Validation Shap ----
#XGBoostValidationData(
# model.=model
# TestData.=NULL
# ModelType="multiclass"
# TrainOnFull.=TRUE
# TestDataCheck=FALSE
# FinalTestTarget.=FinalTestTarget
# TestTarget.=TestTarget
# TrainTarget.=TrainTarget
# TrainMerge.=TrainMerge
# TestMerge.=TestMerge
# dataTest.=dataTest
# data.=dataTrain
# predict.=predict
# TargetColumnName.=TargetColumnName
# SaveModelObjects.=SaveModelObjects
# metadata_path.=metadata_path
# model_path.=model_path
# ModelID.=ModelID
# LossFunction.=LossFunction
# TransformNumericColumns.=NULL
# GridTune.=GridTune
# TransformationResults.=NULL
# TargetLevels.=NULL
#)

# Validation Shap ----
# TrainMerge.=TrainMerge
# ModelType="multiclass"
# TestDataCheck=!is.null(TestData)
# TrainOnFull.=TrainOnFull
# model.=model
# TargetColumnName.=TargetColumnName
# SaveModelObjects.=SaveModelObjects
# metadata_path.=metadata_path
# model_path.=model_path
# ModelID.=ModelID
# TestData.=TestData
# TestTarget.=TestTarget
# FinalTestTarget.=FinalTestTarget
# TestMerge.=TestMerge
# dataTest.=dataTest
# TrainTarget.=TrainTarget
# predict.=predict
# TransformNumericColumns.=NULL
# TransformationResults.=NULL
# GridTune.=NULL
# data.=dataTrain
# LossFunction.=LossFunction

# Multinomial Metrics ----
# ModelClass="xgboost"
# SaveModelObjects.=SaveModelObjects
# ValidationData.=ValidationData
# PredictData.=predict
# TrainOnFull.=TrainOnFull
# TargetColumnName.=TargetColumnName
# TargetLevels.=TargetLevels
# ModelID.=ModelID
# model_path.=model_path
# metadata_path.=metadata_path

# Evaluation Metrics ----
# ModelClass="xgboost"
# SaveModelObjects.=SaveModelObjects
# ValidationData.=ValidationData
# PredictData.=predict
# TrainOnFull.=TrainOnFull
# TargetColumnName.=TargetColumnName
# TargetLevels.=TargetLevels
# ModelID.=ModelID
# model_path.=model_path
# metadata_path.=metadata_path

# Gridtuning ----
# ModelType="multiclass"
# TrainOnFull.=TrainOnFull
# TargetColumnName.=TargetColumnName
# DebugMode.=DebugMode
# TreeMethod.=TreeMethod
# Trees.=Trees
# Depth.=max_depth
# LearningRate.=eta
# min_child_weight.=min_child_weight
# subsample.=subsample
# colsample_bytree.=colsample_bytree
# LossFunction=LossFunction
# EvalMetric=eval_metric
# grid_eval_metric.=grid_eval_metric
# CostMatrixWeights=NULL
# datatrain.=datatrain
# datavalidate.=datavalidate
# datatest.=datatest
# EvalSets.=EvalSets
# TestTarget.=TestTarget
# FinalTestTarget.=FinalTestTarget
# TargetLevels.=TargetLevels
# MaxRunsWithoutNewWinner=MaxRunsWithoutNewWinner
# MaxModelsInGrid=MaxModelsInGrid
# MaxRunMinutes=MaxRunMinutes
# BaselineComparison.=BaselineComparison
# SaveModelObjects=SaveModelObjects
# metadata_path=metadata_path
# model_path=model_path
# ModelID=ModelID
# Verbose.=Verbose
# NumLevels.=NumLevels






