# Testing
XGBoost_QA_Results_Regression <- data.table::CJ(
  TOF = c(TRUE,FALSE),
  Transformation = c(TRUE,FALSE),
  GridTune = c(TRUE,FALSE),
  Success = "Failure",
  PartitionInFunction = c(TRUE,FALSE)
)

# Remove impossible combinations
XGBoost_QA_Results_Regression <- XGBoost_QA_Results_Regression[!(TOF & GridTune)]
XGBoost_QA_Results_Regression <- XGBoost_QA_Results_Regression[!(PartitionInFunction & TOF)]
XGBoost_QA_Results_Regression[, RunNumber := seq_len(.N)]


#      TOF Transformation GridTune Success PartitionInFunction
# 1: FALSE          FALSE    FALSE Failure               FALSE
# 2: FALSE          FALSE    FALSE Failure                TRUE
# 3: FALSE          FALSE     TRUE Failure               FALSE
# 4: FALSE          FALSE     TRUE Failure                TRUE
# 5: FALSE           TRUE    FALSE Failure               FALSE
# 6: FALSE           TRUE    FALSE Failure                TRUE
# 7: FALSE           TRUE     TRUE Failure               FALSE
# 8: FALSE           TRUE     TRUE Failure                TRUE
# 9:  TRUE          FALSE    FALSE Failure               FALSE
# 10: TRUE           TRUE    FALSE Failure               FALSE

# AutoXGBoostRegression
# run = 1
# run = 2
# run = 6
for(run in seq_len(XGBoost_QA_Results_Regression[,.N])) {

  # Define values
  if(XGBoost_QA_Results_Regression[run, Transformation]) {
    trans <- c("Adrian")
  } else {
    trans <- NULL
  }
  tof <- XGBoost_QA_Results_Regression[run, TOF]
  PartitionInFunction <- XGBoost_QA_Results_Regression[run, PartitionInFunction]
  gridtune <- XGBoost_QA_Results_Regression[run, GridTune]
  Tar <- "Adrian"

  # Refresh data
  data <- RemixAutoML::FakeDataGenerator(
    Correlation = 0.85,
    N = 25000L,
    ID = 2L,
    FactorCount = 3,
    AddWeightsColumn = TRUE,
    ZIP = 0L,
    AddDate = TRUE,
    Classification = FALSE,
    MultiClass = FALSE)

  # Add Diff data
  data <- RemixAutoML::AutoDiffLagN(
    data = data,
    DateVariable = "DateTime",
    GroupVariables = c("Factor_1", "Factor_2"),
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
  TestModel <- tryCatch({RemixAutoML::AutoXGBoostRegression(

    # GPU or CPU
    TreeMethod = "hist",
    NThreads = parallel::detectCores(),
    LossFunction = 'reg:squarederror',

    # Metadata arguments
    model_path = normalizePath("./"),
    metadata_path = NULL,
    ModelID = "Test_Model_1",
    EncodingMethod = "credibility",
    ReturnFactorLevels = TRUE,
    ReturnModelObjects = TRUE,
    SaveModelObjects = TRUE,
    DebugMode = TRUE,

    # Data arguments
    data = TTrainData,
    TrainOnFull = tof,
    ValidationData = VValidationData,
    TestData = TTestData,
    TargetColumnName = Tar,
    FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime",Tar)],
    WeightsColumnName = "Weights",
    IDcols = c("IDcol_1","IDcol_2","DateTime"),
    TransformNumericColumns = trans,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit", "YeoJohnson"),

    # Model evaluation
    eval_metric = "rmse",
    NumOfParDepPlots = 3L,

    # Grid tuning arguments
    PassInGrid = NULL,
    GridTune = gridtune,
    grid_eval_metric = "r2",
    BaselineComparison = "default",
    MaxModelsInGrid = 10L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    Verbose = 1L,
    SaveInfoToPDF = TRUE,

    # ML args
    Trees = if(!gridtune) 50L else c(50,51,52,53,54,55),
    eta = if(!gridtune) 0.05 else c(0.05,0.06,0.07,0.08,0.09),
    max_depth = if(!gridtune) 4L else c(4,5,6,7,8,9,10),
    min_child_weight = if(!gridtune) 1.0 else c(1,2,3,4),
    subsample = if(!gridtune) 0.55 else c(0.50,0.55,0.60,0.65),
    colsample_bytree = if(!gridtune) 0.55 else c(0.55,0.65,0.7,0.75,0.8))}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) XGBoost_QA_Results_Regression[run, Success := "Success"]
  TestModel <- NULL
  Sys.sleep(5)
  data.table::fwrite(XGBoost_QA_Results_Regression, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV/AutoXGBoostRegression_QA.csv")
}

# Remove all else
rm(list = ls()[!ls() %in% c(
  "XGBoost_QA_Results_MultiClass",
  "XGBoost_QA_Results_Regression",
  "XGBoost_QA_Results_Classifier",
  "CatBoost_QA_Results_MultiClass",
  "CatBoost_QA_Results_Regression",
  "CatBoost_QA_Results_Classifier")])

# Main Function Defaults ----
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
# run = 6
#
# # Define values
# if(XGBoost_QA_Results_Regression[run, Transformation]) {
#   trans <- c("Adrian")
# } else {
#   trans <- NULL
# }
# tof <- XGBoost_QA_Results_Regression[run, TOF]
# gridtune <- XGBoost_QA_Results_Regression[run, GridTune]
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
#   MultiClass = FALSE)
#
# # Add Diff data
# data <- RemixAutoML::AutoDiffLagN(
#   data = data,
#   DateVariable = "DateTime",
#   GroupVariables = c("Factor_1", "Factor_2"),
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
# # Main Function Defaults
#
# # GPU or CPU
# OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "PDFs", "Score_TrainData")
# TreeMethod = "hist"
# NThreads = parallel::detectCores()
# LossFunction = 'reg:squarederror'
# model_path = normalizePath("./")
# metadata_path = NULL
# ModelID = "Test_Model_1"
# EncodingMethod = "credibility"
# ReturnFactorLevels = TRUE
# ReturnModelObjects = TRUE
# SaveModelObjects = TRUE
# DebugMode = TRUE
# data = TTrainData
# TrainOnFull = tof
# ValidationData = VValidationData
# TestData = TTestData
# TargetColumnName = Tar
# FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime",Tar)]
# WeightsColumnName = "Weights"
# IDcols = c("IDcol_1","IDcol_2","DateTime")
# TransformNumericColumns = trans
# Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit", "YeoJohnson")
# eval_metric = "rmse"
# NumOfParDepPlots = 3L
# PassInGrid = NULL
# GridTune = gridtune
# grid_eval_metric = "r2"
# BaselineComparison = "default"
# MaxModelsInGrid = 10L
# MaxRunsWithoutNewWinner = 20L
# MaxRunMinutes = 24L*60L
# Verbose = 1L
# SaveInfoToPDF = TRUE
# Trees = if(!gridtune) 50L else c(50,51,52,53,54,55)
# eta = if(!gridtune) 0.05 else c(0.05,0.06,0.07,0.08,0.09)
# max_depth = if(!gridtune) 4L else c(4,5,6,7,8,9,10)
# min_child_weight = if(!gridtune) 1.0 else c(1,2,3,4)
# subsample = if(!gridtune) 0.55 else c(0.50,0.55,0.60,0.65)
# colsample_bytree = if(!gridtune) 0.55 else c(0.55,0.65,0.7,0.75,0.8)

# Data Prep ----
# ModelType="regression"
# data.=data
# ValidationData.=ValidationData
# TestData.=TestData
# TargetColumnName.=TargetColumnName
# FeatureColNames.=FeatureColNames
# WeightsColumnName.=WeightsColumnName
# IDcols.=IDcols
# TransformNumericColumns.=TransformNumericColumns
# Methods.=Methods
# ModelID.=ModelID
# model_path.=model_path
# TrainOnFull.=TrainOnFull
# SaveModelObjects.=SaveModelObjects
# ReturnFactorLevels.=ReturnFactorLevels
# EncodingMethod.=EncodingMethod

# Train Validation Data ----
# model.=model
# TestData.=NULL
# ModelType="regression"
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
# LossFunction.=NULL
# TransformNumericColumns.=NULL
# GridTune.=GridTune
# TransformationResults.=NULL
# TargetLevels.=NULL

# Validation Data ----
# TrainMerge. = NULL
# ModelType="regression"
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
# TransformNumericColumns.=TransformNumericColumns
# TransformationResults.=TransformationResults
# GridTune.=GridTune
# data.=data

# ML_EvalPlots ----
# ModelType="regression"
# TrainOnFull.=TrainOnFull
# ValidationData.=TrainData
# NumOfParDepPlots.=NumOfParDepPlots
# VariableImportance.=VariableImportance
# TargetColumnName.=TargetColumnName
# FeatureColNames.=FeatureColNames
# SaveModelObjects.=SaveModelObjects
# ModelID.=ModelID
# metadata_path.=metadata_path
# model_path.=model_path
# LossFunction.="RMSE"
# EvalMetric.=NULL
# EvaluationMetrics.=NULL
# predict.=NULL

