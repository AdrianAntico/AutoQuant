# Testing
CatBoost_QA_Results_Regression <- data.table::CJ(
  TOF = c(TRUE,FALSE),
  Transformation = c(TRUE,FALSE),
  GridTune = c(TRUE,FALSE),
  LossFunction = c("RMSE","MultiRMSE"),
  TaskType = c("CPU","GPU"),
  Success = "Failure",
  PartitionInFunction = c(TRUE,FALSE)
)

# Remove impossible combinations
CatBoost_QA_Results_Regression <- CatBoost_QA_Results_Regression[!(TOF & GridTune)]
CatBoost_QA_Results_Regression <- CatBoost_QA_Results_Regression[!(PartitionInFunction & TOF)]
CatBoost_QA_Results_Regression[, RunNumber := seq_len(.N)]

#       TOF Transformation GridTune LossFunction TaskType Success PartitionInFunction
# 1:  FALSE          FALSE    FALSE    MultiRMSE      CPU Failure               FALSE
# 2:  FALSE          FALSE    FALSE    MultiRMSE      CPU Failure                TRUE
# 3:  FALSE          FALSE    FALSE    MultiRMSE      GPU Failure               FALSE
# 4:  FALSE          FALSE    FALSE    MultiRMSE      GPU Failure                TRUE
# 5:  FALSE          FALSE    FALSE         RMSE      CPU Failure               FALSE
# 6:  FALSE          FALSE    FALSE         RMSE      CPU Failure                TRUE
# 7:  FALSE          FALSE    FALSE         RMSE      GPU Failure               FALSE
# 8:  FALSE          FALSE    FALSE         RMSE      GPU Failure                TRUE
# 9:  FALSE          FALSE     TRUE    MultiRMSE      CPU Failure               FALSE
# 10: FALSE          FALSE     TRUE    MultiRMSE      CPU Failure                TRUE
# 11: FALSE          FALSE     TRUE    MultiRMSE      GPU Failure               FALSE
# 12: FALSE          FALSE     TRUE    MultiRMSE      GPU Failure                TRUE
# 13: FALSE          FALSE     TRUE         RMSE      CPU Failure               FALSE
# 14: FALSE          FALSE     TRUE         RMSE      CPU Failure                TRUE
# 15: FALSE          FALSE     TRUE         RMSE      GPU Failure               FALSE
# 16: FALSE          FALSE     TRUE         RMSE      GPU Failure                TRUE
# 17: FALSE           TRUE    FALSE    MultiRMSE      CPU Failure               FALSE
# 18: FALSE           TRUE    FALSE    MultiRMSE      CPU Failure                TRUE
# 19: FALSE           TRUE    FALSE    MultiRMSE      GPU Failure               FALSE
# 20: FALSE           TRUE    FALSE    MultiRMSE      GPU Failure                TRUE
# 21: FALSE           TRUE    FALSE         RMSE      CPU Failure               FALSE
# 22: FALSE           TRUE    FALSE         RMSE      CPU Failure                TRUE
# 23: FALSE           TRUE    FALSE         RMSE      GPU Failure               FALSE
# 24: FALSE           TRUE    FALSE         RMSE      GPU Failure                TRUE
# 25: FALSE           TRUE     TRUE    MultiRMSE      CPU Failure               FALSE
# 26: FALSE           TRUE     TRUE    MultiRMSE      CPU Failure                TRUE
# 27: FALSE           TRUE     TRUE    MultiRMSE      GPU Failure               FALSE
# 28: FALSE           TRUE     TRUE    MultiRMSE      GPU Failure                TRUE
# 29: FALSE           TRUE     TRUE         RMSE      CPU Failure               FALSE
# 30: FALSE           TRUE     TRUE         RMSE      CPU Failure                TRUE
# 31: FALSE           TRUE     TRUE         RMSE      GPU Failure               FALSE
# 32: FALSE           TRUE     TRUE         RMSE      GPU Failure                TRUE
# 33:  TRUE          FALSE    FALSE    MultiRMSE      CPU Failure               FALSE
# 34:  TRUE          FALSE    FALSE    MultiRMSE      GPU Failure               FALSE
# 35:  TRUE          FALSE    FALSE         RMSE      CPU Failure               FALSE
# 36:  TRUE          FALSE    FALSE         RMSE      GPU Failure               FALSE
# 37:  TRUE           TRUE    FALSE    MultiRMSE      CPU Failure               FALSE
# 38:  TRUE           TRUE    FALSE    MultiRMSE      GPU Failure               FALSE
# 39:  TRUE           TRUE    FALSE         RMSE      CPU Failure               FALSE
# 40:  TRUE           TRUE    FALSE         RMSE      GPU Failure               FALSE

# AutoCatBoostRegression
# run = 7
for(run in seq_len(CatBoost_QA_Results_Regression[,.N])) {

  # Iteration number
  for(zzz in 1:10) print(run)

  # Define values
  if(CatBoost_QA_Results_Regression[run, Transformation]) {
    if(CatBoost_QA_Results_Regression[run, LossFunction == "MultiRMSE"]) trans <- c("Adrian","Adrian2")
    if(CatBoost_QA_Results_Regression[run, LossFunction != "MultiRMSE"]) trans <- c("Adrian")
  } else {
    trans <- NULL
  }
  tasktypemode <- CatBoost_QA_Results_Regression[run, TaskType]
  tof <- CatBoost_QA_Results_Regression[run, TOF]
  PartitionInFunction <- CatBoost_QA_Results_Regression[run, PartitionInFunction]
  gridtune <- CatBoost_QA_Results_Regression[run, GridTune]
  lossfunction <- CatBoost_QA_Results_Regression[run, LossFunction]
  if(lossfunction == "MultiRMSE") {
    Tar <- c("Adrian","Adrian2")
  } else {
    Tar <- "Adrian"
  }

  # Refresh data
  data <- RemixAutoML::FakeDataGenerator(
    Correlation = 0.85,
    N = 20000L,
    ID = 2L,
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

  # Vector
  if(lossfunction == "MultiRMSE") {
    TTrainData[, Adrian2 := jitter(Adrian, factor = 1000)]
    if(!is.null(VValidationData)) VValidationData[, Adrian2 := jitter(Adrian, factor = 1000)]
    if(!is.null(TTestData)) TTestData[, Adrian2 := jitter(Adrian, factor = 1000)]
  }

  # Run function
  TestModel <- tryCatch({RemixAutoML::AutoCatBoostRegression(

    # GPU or CPU and the number of available GPUs
    task_type = tasktypemode,
    NumGPUs = 1,

    # Metadata arguments
    OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "PDFs", "Score_TrainData"),
    ModelID = "Test_Model_1",
    model_path = normalizePath("./"),
    metadata_path = NULL,
    SaveModelObjects = FALSE,
    SaveInfoToPDF = FALSE,
    ReturnModelObjects = TRUE,

    # Data arguments
    data = data.table::copy(TTrainData),
    TrainOnFull = tof,
    ValidationData = data.table::copy(VValidationData),
    TestData = data.table::copy(TTestData),
    TargetColumnName = Tar,
    FeatureColNames = if(lossfunction == "RMSE") names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","Adrian")] else names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","Adrian","Adrian2")],
    PrimaryDateColumn = NULL,
    WeightsColumnName = "Weights",
    IDcols = c("IDcol_1","IDcol_2"),
    TransformNumericColumns =  trans,
    Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),

    # Model evaluation
    eval_metric = lossfunction,
    eval_metric_value = 1.5,
    loss_function = lossfunction,
    loss_function_value = 1.5,
    grid_eval_metric = "r2",
    MetricPeriods = 10L,
    NumOfParDepPlots = 3,

    # Grid tuning arguments
    PassInGrid = NULL,
    GridTune = gridtune,
    MaxModelsInGrid = 20L,
    MaxRunsWithoutNewWinner = 10L,
    MaxRunMinutes = 60*60,
    BaselineComparison = "default",

    # ML args
    Trees = if(!gridtune) 50L else c(50,60,70,80,90,100),
    Depth = if(!gridtune) 4L else c(4,5,6,7,8),
    LearningRate = if(!gridtune) 0.01 else c(0.01,0.02,0.03,0.04,0.05),
    L2_Leaf_Reg = if(!gridtune) 1.0 else c(1,2,3,4,5),
    RandomStrength = if(!gridtune) 1 else c(0.8,0.9,1),
    BorderCount = if(!gridtune) 128 else c(32,56,96,128,256),
    RSM = 1,
    BootStrapType = NULL,
    GrowPolicy = "SymmetricTree",
    langevin = FALSE,
    diffusion_temperature = 10000,
    model_size_reg = 0.5,
    feature_border_type = "GreedyLogSum",
    sampling_unit = "Group",
    subsample = NULL,
    score_function = "Cosine",
    min_data_in_leaf = 1,
    DebugMode = TRUE)}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) CatBoost_QA_Results_Regression[run, Success := "Success"]
  TestModel <- NULL
  gc(); Sys.sleep(5)
  data.table::fwrite(CatBoost_QA_Results_Regression, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV/AutoCatBoostRegression_QA.csv")
}

# Defaults ----
library(RemixAutoML)
library(data.table)

source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))

run = 7

# Iteration number
for(zzz in 1:10) print(run)

# Define values
if(CatBoost_QA_Results_Regression[run, Transformation]) {
  if(CatBoost_QA_Results_Regression[run, LossFunction == "MultiRMSE"]) trans <- c("Adrian","Adrian2")
  if(CatBoost_QA_Results_Regression[run, LossFunction != "MultiRMSE"]) trans <- c("Adrian")
} else {
  trans <- NULL
}
tasktypemode <- CatBoost_QA_Results_Regression[run, TaskType]
tof <- CatBoost_QA_Results_Regression[run, TOF]
PartitionInFunction <- CatBoost_QA_Results_Regression[run, PartitionInFunction]
gridtune <- CatBoost_QA_Results_Regression[run, GridTune]
lossfunction <- CatBoost_QA_Results_Regression[run, LossFunction]
if(lossfunction == "MultiRMSE") {
  Tar <- c("Adrian","Adrian2")
} else {
  Tar <- "Adrian"
}

# Refresh data
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 20000L,
  ID = 2L,
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

# Vector
if(lossfunction == "MultiRMSE") {
  TTrainData[, Adrian2 := jitter(Adrian, factor = 1000)]
  if(!is.null(VValidationData)) VValidationData[, Adrian2 := jitter(Adrian, factor = 1000)]
  if(!is.null(TTestData)) TTestData[, Adrian2 := jitter(Adrian, factor = 1000)]
}

# # AutoCatBoostRegression
OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "PDFs", "Score_TrainData")
task_type = "GPU"
NumGPUs = 1
ModelID = "Test_Model_1"
model_path = normalizePath("./")
metadata_path = NULL
SaveModelObjects = FALSE
SaveInfoToPDF = FALSE
ReturnModelObjects = TRUE
data = TTrainData
TrainOnFull = tof
ValidationData = VValidationData
TestData = TTestData
WeightsColumnName = "Weights"
TargetColumnName = Tar
FeatureColNames = if(lossfunction == "RMSE") names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","Adrian")] else names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","Adrian","Adrian2")]
PrimaryDateColumn = NULL
IDcols = c("IDcol_1","IDcol_2")
TransformNumericColumns = trans
Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit")
eval_metric = lossfunction
eval_metric_value = 1.5
loss_function = lossfunction
loss_function_value = 1.5
grid_eval_metric = "r2"
MetricPeriods = 10L
NumOfParDepPlots = 3
EvalPlots = TRUE
PassInGrid = NULL
GridTune = gridtune
MaxModelsInGrid = 20L
MaxRunsWithoutNewWinner = 10L
MaxRunMinutes = 60*60
Shuffles = 4L
BaselineComparison = "default"
Trees = if(!gridtune) 100L else c(50,60,70,80,90,100)
Depth = if(!gridtune) 4L else c(4,5,6,7,8)
LearningRate = if(!gridtune) 0.01 else c(0.01,0.02,0.03,0.04,0.05)
L2_Leaf_Reg = if(!gridtune) 1.0 else c(1,2,3,4,5)
RandomStrength = if(!gridtune) 1 else c(0.8,0.9,1)
BorderCount = if(!gridtune) 128 else c(32,56,96,128,256)
RSM = 1
BootStrapType = NULL
GrowPolicy = "SymmetricTree"
langevin = FALSE
diffusion_temperature = 10000
model_size_reg = 0.5
feature_border_type = "GreedyLogSum"
sampling_unit = "Group"
subsample = NULL
score_function = "Cosine"
min_data_in_leaf = 1
DebugMode = TRUE

# DataPrep ----
# OutputSelection.=OutputSelection
# ModelType="regression"
# data.=data
# ValidationData.=ValidationData
# TestData.=TestData
# TargetColumnName.=TargetColumnName
# FeatureColNames.=FeatureColNames
# PrimaryDateColumn.=PrimaryDateColumn
# WeightsColumnName.=WeightsColumnName
# IDcols.=IDcols
# TrainOnFull.=TrainOnFull
# SaveModelObjects.=SaveModelObjects
# TransformNumericColumns.=TransformNumericColumns
# Methods.=Methods
# model_path.=model_path
# ModelID.=ModelID
# LossFunction.=LossFunction
# EvalMetric.=EvalMetric

# Data Conversion ----
# CatFeatures.=CatFeatures
# dataTrain.=dataTrain
# dataTest.=dataTest
# TestData.=TestData
# TrainTarget.=TrainTarget
# TestTarget.=TestTarget
# FinalTestTarget.=FinalTestTarget
# TrainOnFull.=TrainOnFull

# Grid tuning function ----
# AlgoType="catboost"
# ModelType="regression"
# TrainOnFull.=TrainOnFull
# TargetColumnName.=TargetColumnName
# DebugMode.=DebugMode
# task_type.=task_type
# Trees.=Trees
# Depth.=Depth
# LearningRate.=LearningRate
# L2_Leaf_Reg.=L2_Leaf_Reg
# BorderCount.=BorderCount
# RandomStrength.=RandomStrength
# RSM.=RSM
# BootStrapType.=BootStrapType
# GrowPolicy.=GrowPolicy
# NumGPUs=NumGPUs
# LossFunction=LossFunction
# EvalMetric=EvalMetric
# grid_eval_metric = "r2"
# MetricPeriods=MetricPeriods
# ClassWeights=NULL
# model_path=model_path
# TrainPool.=TrainPool
# TestPool.=TestPool
# FinalTestTarget.=FinalTestTarget
# TestTarget.=TestTarget
# FinalTestPool. = FinalTestPool
# TestData.=TestData
# TestMerge.=TestMerge
# TargetLevels.=NULL
# MaxRunsWithoutNewWinner=MaxRunsWithoutNewWinner
# MaxModelsInGrid=MaxModelsInGrid
# MaxRunMinutes=MaxRunMinutes
# SaveModelObjects=SaveModelObjects
# metadata_path=metadata_path
# model_path=model_path
# ModelID=ModelID
# BaselineComparison.=BaselineComparison
# grid_eval_metric.=grid_eval_metric

# Regression metrics ----
# SaveModelObjects.=SaveModelObjects
# data.=data
# ValidationData.=ValidationData
# TrainOnFull.=TrainOnFull.
# LossFunction.=LossFunction
# EvalMetric.=EvalMetric
# TargetColumnName.=TargetColumnName.
# ModelID.=ModelID
# model_path.=model_path
# metadata_path.=metadata_path
# grid_eval_metric.=grid_eval_metric

# Args check ----
# ModelType=if(loss_function == "MultiRMSE") "vector" else "regression"
# PrimaryDateColumn.=PrimaryDateColumn
# GridTune.=GridTune
# model_path.=model_path
# metadata_path.=metadata_path
# ClassWeights.=NULL
# LossFunction.=NULL
# loss_function.=loss_function
# loss_function_value.=loss_function_value
# eval_metric.=eval_metric
# eval_metric_value.=eval_metric_value
# task_type.=task_type
# NumGPUs.=NumGPUs
# MaxModelsInGrid.=MaxModelsInGrid
# NumOfParDepPlots.=NumOfParDepPlots
# ReturnModelObjects.=ReturnModelObjects
# SaveModelObjects.=SaveModelObjects
# PassInGrid.=PassInGrid
# MetricPeriods.=MetricPeriods
# langevin.=langevin
# diffusion_temperature.=diffusion_temperature
# Trees.=Trees
# Depth.=Depth
# LearningRate.=LearningRate
# L2_Leaf_Reg.=L2_Leaf_Reg
# RandomStrength.=RandomStrength
# BorderCount.=BorderCount
# RSM.=RSM
# BootStrapType.=BootStrapType
# GrowPolicy.=GrowPolicy
# model_size_reg.=model_size_reg
# feature_border_type.=feature_border_type
# sampling_unit.=sampling_unit
# subsample.=subsample
# score_function.=score_function
# min_data_in_leaf.=min_data_in_leaf

# Train ValidationData ----
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
# SaveModelObjects. = SaveModelObjects
# metadata_path.=metadata_path
# model_path.=model_path
# ModelID.=ModelID
# LossFunction.=NULL
# TransformNumericColumns.=NULL
# GridTune.=GridTune
# TransformationResults.=NULL
# TargetLevels.=NULL

# Validation data ----
# ModelType="regression"
# TrainOnFull.=TrainOnFull
# TestDataCheck=!is.null(TestData)
# FinalTestTarget.=FinalTestTarget
# TestTarget.=TestTarget
# TrainTarget.=TrainTarget
# TestMerge.=TestMerge
# dataTest.=dataTest
# data.=data
# predict.=predict
# TargetColumnName.=TargetColumnName
# SaveModelObjects.= SaveModelObjects
# metadata_path.=metadata_path
# model_path.=model_path
# ModelID.=ModelID
# LossFunction.=LossFunction
# TransformNumericColumns.=TransformNumericColumns
# GridTune.=GridTune
# TransformationResults.=TransformationResults

# Importances ----
# ModelType = "regression"
# TargetColumnName.=TargetColumnName
# BestGrid.=BestGrid
# TrainOnFull. = TrainOnFull
# TrainPool.=TrainPool
# TestPool.=TestPool
# FinalTestPool.=FinalTestPool
# TestDataCheck=!is.null(TestData)
# ValidationData.=ValidationData
# FeatureColNames.=FeatureColNames
# GridTune.=GridTune
# task_type.=task_type
# SaveModelObjects.=SaveModelObjects
# model.=model
# ModelID.=ModelID
# model_path.=model_path
# metadata_path.=metadata_path
# GrowPolicy.=GrowPolicy

# Regression Metrics ----
# SaveModelObjects.=SaveModelObjects
# data.=data
# ValidationData.=ValidationData
# TrainOnFull.=TrainOnFull
# LossFunction.=LossFunction
# EvalMetric.=EvalMetric
# TargetColumnName.=TargetColumnName
# ModelID.=ModelID
# model_path.=model_path
# metadata_path.=metadata_path

# Eval Plots ----
# ModelType="regression"
# TrainOnFull.=TrainOnFull
# LossFunction.=LossFunction
# EvalMetric.=EvalMetric
# EvaluationMetrics.=EvalMetricsList
# ValidationData.=ValidationData
# NumOfParDepPlots.=NumOfParDepPlots
# VariableImportance.=VariableImportance
# TargetColumnName.=TargetColumnName
# FeatureColNames.=FeatureColNames
# SaveModelObjects.=SaveModelObjects
# ModelID.=ModelID
# metadata_path.=metadata_path
# model_path.=model_path
# predict.=NULL
# DateColumnName. = "DateTime"

# Residual Plots
# TestData=ValidationData.
# Target=TargetColumnName.
# Predicted="Predict"
# DateColumnName=DateColumnName.
# Gam_Fit=FALSE

# ScatterCopula
# data=TestData
# x_var=Predicted
# y_var=Target
# GroupVariable=NULL
# SampleCount=100000L
# FitGam=Gam_Fit
# color = "darkblue"
# point_size = 0.50
# text_size = 12
# x_axis_text_angle = 35
# y_axis_text_angle = 0
# chart_color = "lightsteelblue1"
# border_color = "darkblue"
# text_color = "darkblue"
# grid_color = "white"
# background_color = "gray95"
# legend_position = "bottom"

# Eval metrics ----
# ModelType="regression"
# TrainOnFull.=TrainOnFull
# LossFunction.=LossFunction
# EvalMetric.=EvalMetric
# EvaluationMetrics.=EvaluationMetrics
# ValidationData.=ValidationData
# NumOfParDepPlots.=NumOfParDepPlots
# VariableImportance.=VariableImportance
# TargetColumnName.=TargetColumnName[1L]
# FeatureColNames.=FeatureColNames
# SaveModelObjects.=SaveModelObjects
# ModelID.=ModelID
# metadata_path.=metadata_path
# model_path.=model_path

# PDFs ----
# ModelClass = "catboost"
# ModelType="regression"
# TrainOnFull.=TrainOnFull
# SaveInfoToPDF.=SaveInfoToPDF
# PlotList.=PlotList
# VariableImportance.=VariableImportance
# EvalMetricsList.=EvalMetricsList
# Interaction.=Interaction
# model_path.=model_path
# metadata_path.=metadata_path

# ----

# ----
