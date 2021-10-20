# Test data.table
CatBoost_QA_Results_MultiClass <- data.table::CJ(
  TOF = c(TRUE,FALSE),
  GridTune = c(TRUE,FALSE),
  TaskType = c("CPU","GPU"),
  Success = "Failure",
  PartitionInFunction = c(TRUE,FALSE),
  PlotList = 0,
  MultinomialMetrics = 0,
  EvaluationMetrics = 0,
  EvaluationMetrics2 = 0,
  VariableImportance = 0,
  InteractionImportance = 0,
  GridMetrics = 0,
  ColNames = 0,
  TargetLevels = 0,
  ArgsList = 0)

# Remove impossible combinations
CatBoost_QA_Results_MultiClass <- CatBoost_QA_Results_MultiClass[!(TOF & GridTune)]
CatBoost_QA_Results_MultiClass <- CatBoost_QA_Results_MultiClass[!(PartitionInFunction & TOF)]
CatBoost_QA_Results_MultiClass[, RunNumber := seq_len(.N)]

#      TOF GridTune TaskType Success PartitionInFunction
# 1: FALSE    FALSE      CPU Failure               FALSE
# 2: FALSE    FALSE      CPU Failure                TRUE
# 3: FALSE    FALSE      GPU Failure               FALSE
# 4: FALSE    FALSE      GPU Failure                TRUE
# 5: FALSE     TRUE      CPU Failure               FALSE
# 6: FALSE     TRUE      CPU Failure                TRUE
# 7: FALSE     TRUE      GPU Failure               FALSE
# 8: FALSE     TRUE      GPU Failure                TRUE
# 9:  TRUE    FALSE      CPU Failure               FALSE
# 10: TRUE    FALSE      GPU Failure               FALSE

# AutoCatBoostMultiClass
# run = 3
for(run in seq_len(CatBoost_QA_Results_MultiClass[,.N])) {

  # Define values
  tasktypemode <- CatBoost_QA_Results_MultiClass[run, TaskType]
  tof <- CatBoost_QA_Results_MultiClass[run, TOF]
  PartitionInFunction <- CatBoost_QA_Results_MultiClass[run, PartitionInFunction]
  gridtune <- CatBoost_QA_Results_MultiClass[run, GridTune]
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
    DiffVariables = names(data)[!names(data) %in% c("IDcol_1","IDcol_2","Adrian","DateTime","Factor_2")],
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
      StratifyColumnNames = Tar,
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
  TestModel <- tryCatch({RemixAutoML::AutoCatBoostMultiClass(

    # GPU or CPU and the number of available GPUs
    task_type = tasktypemode,
    NumGPUs = 1,

    # Metadata arguments
    OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
    ModelID = "Test_Model_1",
    model_path = normalizePath("./"),
    metadata_path = normalizePath("./"),
    SaveModelObjects = FALSE,
    ReturnModelObjects = TRUE,

    # Data arguments
    data = TTrainData,
    TrainOnFull = tof,
    ValidationData = VValidationData,
    TestData = TTestData,
    TargetColumnName = Tar,
    FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2",Tar)],
    PrimaryDateColumn = "DateTime",
    WeightsColumnName = "Weights",
    ClassWeights = c(1L,1L,1L,1L,1L),
    IDcols = c("IDcol_1","IDcol_2","DateTime"),

    # Model evaluation
    eval_metric = "MCC",
    loss_function = "MultiClassOneVsAll",
    grid_eval_metric = "Accuracy",
    MetricPeriods = 10L,

    # Grid tuning arguments
    PassInGrid = NULL,
    GridTune = gridtune,
    MaxModelsInGrid = 30L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    BaselineComparison = "default",

    # ML args
    Trees = if(!gridtune) 100L else c(50,60,70,80,90,100),
    Depth = if(!gridtune) 4L else c(4,5,6,7,8),
    LearningRate = if(!gridtune) 0.01 else c(0.01,0.02,0.03,0.04,0.05),
    L2_Leaf_Reg = if(!gridtune) 1.0 else c(1,2,3,4,5),
    RandomStrength = if(!gridtune) 1 else c(0.8,0.9,1),
    BorderCount = if(!gridtune) 128 else c(32,56,96,128,256),
    langevin = FALSE,
    diffusion_temperature = 10000,
    RSM = 0.80,
    BootStrapType = "Bayesian",
    GrowPolicy = "SymmetricTree",
    model_size_reg = 0.5,
    feature_border_type = "GreedyLogSum",
    sampling_unit = "Group",
    subsample = NULL,
    score_function = "Cosine",
    min_data_in_leaf = 1,
    DebugMode = TRUE)}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) {
    CatBoost_QA_Results_MultiClass[run, Success := "Success"]
    CatBoost_QA_Results_MultiClass[run, Success := "Success"]

  }
  TestModel <- NULL
  gc(); Sys.sleep(5)
  data.table::fwrite(CatBoost_QA_Results_MultiClass, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoCatBoostMultiClass_QA.csv")
}

# Defaults ----
library(RemixAutoML)
suppressMessages(library(data.table))

source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/GridTuning.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))

run = 3

# Define values
tasktypemode <- CatBoost_QA_Results_MultiClass[run, TaskType]
tof <- CatBoost_QA_Results_MultiClass[run, TOF]
gridtune <- CatBoost_QA_Results_MultiClass[run, GridTune]
Tar <- "Adrian"

# Refresh data
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 100000L,
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
  DiffVariables = names(data)[!names(data) %in% c("IDcol_1","IDcol_2","Adrian","DateTime","Factor_2")],
  DiffDateVariables = NULL,
  DiffGroupVariables = NULL,
  NLag1 = 0,
  NLag2 = 1,
  Sort = TRUE,
  RemoveNA = TRUE)

# Partition Data
if(!tof) {
  Sets <- RemixAutoML::AutoDataPartition(
    data = data,
    NumDataSets = 3,
    Ratios = c(0.7,0.2,0.1),
    PartitionType = "random",
    StratifyColumnNames = Tar,
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

NumOfParDepPlots = 1
OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData")
task_type = tasktypemode
NumGPUs = 1
ModelID = "Test_Model_1"
model_path = normalizePath("./")
metadata_path = normalizePath("./")
SaveModelObjects = FALSE
ReturnModelObjects = TRUE
data = TTrainData
TrainOnFull = tof
ValidationData = VValidationData
TestData = TTestData
TargetColumnName = "Adrian"
FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","Adrian")]
PrimaryDateColumn = "DateTime"
WeightsColumnName = "Weights"
ClassWeights = c(1L,1L,1L,1L,1L)
IDcols = c("IDcol_1","IDcol_2")
eval_metric = "MCC"
loss_function = "MultiClassOneVsAll"
grid_eval_metric = "Accuracy"
MetricPeriods = 10L
PassInGrid = NULL
GridTune = gridtune
MaxModelsInGrid = 15L
MaxRunsWithoutNewWinner = 10L
MaxRunMinutes = 24L*60L
BaselineComparison = "default"
langevin = FALSE
diffusion_temperature = 10000
Trees = if(!gridtune) 50L else c(50,60,70,80,90,100)
Depth = if(!gridtune) 4L else c(4,5,6,7,8)
LearningRate = if(!gridtune) 0.01 else c(0.01,0.02,0.03,0.04,0.05)
L2_Leaf_Reg = if(!gridtune) 1.0 else c(1,2,3,4,5)
RandomStrength = if(!gridtune) 1 else c(0.8,0.9,1)
BorderCount = if(!gridtune) 128 else c(32,56,96,128,256)
langevin = FALSE
diffusion_temperature = 10000
RSM = 0.80
BootStrapType = "Bayesian"
GrowPolicy = "SymmetricTree"
model_size_reg = 0.5
feature_border_type = "GreedyLogSum"
sampling_unit = "Group"
subsample = NULL
score_function = "Cosine"
min_data_in_leaf = 1
DebugMode = TRUE
#
# CatBoostValidationData(ModelType='multiclass', TrainOnFull.=TRUE, TestDataCheck=FALSE, FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, TrainTarget.=TrainTarget, TrainMerge.=TrainMerge, TestMerge.=TestMerge, dataTest.=dataTest, data.= TrainMerge, predict.=predict, TargetColumnName.=TargetColumnName, SaveModelObjects. = SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, LossFunction.=NULL, TransformNumericColumns.=NULL, GridTune.=GridTune, TransformationResults.=NULL, TargetLevels.=TargetLevels)
ModelType='multiclass'
TrainOnFull.=TRUE
TestDataCheck=FALSE
FinalTestTarget.=FinalTestTarget
TestTarget.=TestTarget
TrainTarget.=TrainTarget
TrainMerge.=TrainMerge
TestMerge.=TestMerge
dataTest.=dataTest
data.= TrainMerge
predict.=predict
TargetColumnName.=TargetColumnName
SaveModelObjects. = SaveModelObjects
metadata_path.=metadata_path
model_path.=model_path
ModelID.=ModelID
LossFunction.=NULL
TransformNumericColumns.=NULL
GridTune.=GridTune
TransformationResults.=NULL
TargetLevels.=TargetLevels
#
# MultiClassMetrics(ModelClass='catboost', DataType = 'Train', SaveModelObjects.=SaveModelObjects, ValidationData.=TrainData, PredictData.=predict, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, TargetLevels.=TargetLevels, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)
ModelClass='catboost'
DataType = 'Train'
SaveModelObjects.=SaveModelObjects
ValidationData.=TrainData
PredictData.=predict
TrainOnFull.=TrainOnFull
TargetColumnName.=TargetColumnName
TargetLevels.=TargetLevels
ModelID.=ModelID
model_path.=model_path
metadata_path.=metadata_path


# # Args check ----
# ModelType="multiclass"
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
# NumOfParDepPlots.=0
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

# DataPrep ----
OutputSelection.=OutputSelection
ModelType="multiclass"
data.=data
ValidationData.=ValidationData
TestData.=TestData
TargetColumnName.=TargetColumnName
FeatureColNames.=FeatureColNames
WeightsColumnName.=WeightsColumnName
PrimaryDateColumn.=PrimaryDateColumn
IDcols.=IDcols
TrainOnFull.=TrainOnFull
SaveModelObjects.=SaveModelObjects
TransformNumericColumns.=NULL
Methods.=NULL
model_path.=model_path
ModelID.=ModelID
DummifyCols.=FALSE
LossFunction.=LossFunction
EvalMetric.=NULL

# Data Conversion ----
# CatFeatures.=CatFeatures
# dataTrain.=dataTrain
# dataTest.=dataTest
# TestData.=TestData
# TrainTarget.=TrainTarget
# TestTarget.=TestTarget
# FinalTestTarget.=FinalTestTarget
# TrainOnFull.=TrainOnFull

# Grid tuning ----
# ModelType="multiclass"
# TrainOnFull.=TrainOnFull
# HasTime=HasTime
# BaselineComparison.=BaselineComparison
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
# MetricPeriods=MetricPeriods
# ClassWeights=NULL
# CostMatrixWeights=NULL
# data=data
# TrainPool.=TrainPool
# TestPool.=TestPool
# FinalTestTarget.=FinalTestTarget
# TestTarget.=TestTarget
# FinalTestPool.=FinalTestPool
# TestData.=TestData
# TestMerge.=TestMerge
# TargetLevels.=TargetLevels
# MaxRunsWithoutNewWinner=MaxRunsWithoutNewWinner
# MaxModelsInGrid=MaxModelsInGrid
# MaxRunMinutes=MaxRunMinutes
# SaveModelObjects=SaveModelObjects
# metadata_path=metadata_path
# model_path=model_path
# ModelID=ModelID
# grid_eval_metric.=grid_eval_metric

# Train Validation Data ----
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
# SaveModelObjects. = SaveModelObjects
# metadata_path.=metadata_path
# model_path.=model_path
# ModelID.=ModelID
# LossFunction.=NULL
# TransformNumericColumns.=NULL
# GridTune.=GridTune
# TransformationResults.=NULL
# TargetLevels.=TargetLevels

# Validation data ----
# ModelType="multiclass"
# TrainOnFull.=TrainOnFull
# TestDataCheck=!is.null(TestData)
# FinalTestTarget.=FinalTestTarget
# TestTarget.=TestTarget
# TrainTarget.=TrainTarget
# TestMerge.=TestMerge
# TrainMerge.=NULL
# dataTest.=dataTest
# data.=data
# predict.=predict
# TargetColumnName.=TargetColumnName
# SaveModelObjects.= SaveModelObjects
# metadata_path.=metadata_path
# model_path.=model_path
# ModelID.=ModelID
# LossFunction.=LossFunction
# TransformNumericColumns.=NULL
# GridTune.=GridTune
# TransformationResults.=NULL
# TargetLevels.=TargetLevels

# Importances ----
# ModelType="multiclass"
# TargetColumnName.=TargetColumnName
# BestGrid.=BestGrid
# TrainOnFull.=TrainOnFull
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

# Multinomial Metrics ----
# ModelClass="catboost"
# SaveModelObjects.=SaveModelObjects
# ValidationData.=ValidationData
# PredictData.=predict
# TrainOnFull.=TrainOnFull
# TargetColumnName.=TargetColumnName
# TargetLevels.=TargetLevels
# ModelID.=ModelID
# model_path.=model_path
# metadata_path.=metadata_path

# ----

# ----
