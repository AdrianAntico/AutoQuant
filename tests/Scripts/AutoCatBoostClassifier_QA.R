# Test data.table
CatBoost_QA_Results_Classifier <- data.table::CJ(
  TOF = c(TRUE,FALSE),
  GridTune = c(TRUE,FALSE),
  TaskType = c("CPU","GPU"),
  Success = "Failure",
  PartitionInFunction = c(TRUE,FALSE)
)

# Remove impossible combinations
CatBoost_QA_Results_Classifier <- CatBoost_QA_Results_Classifier[!(TOF & GridTune)]
CatBoost_QA_Results_Classifier <- CatBoost_QA_Results_Classifier[!(PartitionInFunction & TOF)]
CatBoost_QA_Results_Classifier[, RunNumber := seq_len(.N)]


#      TOF GridTune TaskType Success PartitionInFunction RunNumber
# 1: FALSE    FALSE      CPU Failure               FALSE         1
# 2: FALSE    FALSE      CPU Failure                TRUE         2
# 3: FALSE    FALSE      GPU Failure               FALSE         3
# 4: FALSE    FALSE      GPU Failure                TRUE         4
# 5: FALSE     TRUE      CPU Failure               FALSE         5
# 6: FALSE     TRUE      CPU Failure                TRUE         6
# 7: FALSE     TRUE      GPU Failure               FALSE         7
# 8: FALSE     TRUE      GPU Failure                TRUE         8
# 9:  TRUE    FALSE      CPU Failure               FALSE         9
# 10: TRUE    FALSE      GPU Failure               FALSE        10

# AutoCatBoostClassifier
# run = 1
# run = 6
for(run in seq_len(CatBoost_QA_Results_Classifier[,.N])) {

  # Define values
  tasktypemode <- CatBoost_QA_Results_Classifier[run, TaskType]
  tof <- CatBoost_QA_Results_Classifier[run, TOF]
  PartitionInFunction <- CatBoost_QA_Results_Classifier[run, PartitionInFunction]
  gridtune <- CatBoost_QA_Results_Classifier[run, GridTune]
  Tar <- "Adrian"

  # Refresh data
  data <- RemixAutoML::FakeDataGenerator(
    Correlation = 0.85,
    N = 25000L,
    ID = 2L,
    AddWeightsColumn = TRUE,
    ZIP = 0L,
    AddDate = TRUE,
    Classification = TRUE,
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

  # AutoCatBoostClassifier
  TestModel <- tryCatch({RemixAutoML::AutoCatBoostClassifier(

    # GPU or CPU and the number of available GPUs
    task_type = tasktypemode,
    NumGPUs = 1,

    # Metadata arguments
    OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
    ModelID = "Test_Model_1",
    model_path = normalizePath("./"),
    metadata_path = normalizePath("./"),
    SaveModelObjects = TRUE,
    ReturnModelObjects = TRUE,
    SaveInfoToPDF = TRUE,

    # Data arguments
    data = TTrainData,
    TrainOnFull = tof,
    ValidationData = VValidationData,
    TestData = TTestData,
    TargetColumnName = "Adrian",
    FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")],
    PrimaryDateColumn = "DateTime",
    WeightsColumnName = "Weights",
    ClassWeights = c(1L,1L),
    IDcols = c("IDcol_1","IDcol_2","DateTime"),

    # Model evaluation
    CostMatrixWeights = c(2,0,0,1),
    EvalMetric = "MCC",
    LossFunction = "Logloss",
    grid_eval_metric = "Utility",
    MetricPeriods = 10L,
    NumOfParDepPlots = 3,

    # Grid tuning arguments
    PassInGrid = NULL,
    GridTune = gridtune,
    MaxModelsInGrid = 30L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    BaselineComparison = "default",

    # ML args
    Trees = if(!gridtune) 100L else c(50,60),
    Depth = if(!gridtune) 4L else c(4,5),
    LearningRate = if(!gridtune) 0.01 else c(0.01,0.02),
    L2_Leaf_Reg = if(!gridtune) 1.0 else c(1,2),
    RandomStrength = if(!gridtune) 1 else c(0.8,0.9,1),
    BorderCount = if(!gridtune) 128 else c(32,56,96,128,256),
    RSM = 0.80,
    BootStrapType = "Bayesian",
    GrowPolicy = "SymmetricTree",
    langevin = FALSE,
    diffusion_temperature = 10000,
    model_size_reg = 0.5,
    feature_border_type = "GreedyLogSum",
    sampling_unit = "Object", # "Group",
    subsample = NULL,
    score_function = "Cosine",
    min_data_in_leaf = 1,
    DebugMode = TRUE)}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) CatBoost_QA_Results_Classifier[run, Success := "Success"]
  TestModel <- NULL
  gc(); Sys.sleep(5)
  data.table::fwrite(CatBoost_QA_Results_Classifier, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV/AutoCatBoostClassifier_QA.csv")
}

# Defaults ----
library(RemixAutoML)
library(data.table)

source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))

run = 1

# Define values
tasktypemode <- CatBoost_QA_Results_Classifier[run, TaskType]
tof <- CatBoost_QA_Results_Classifier[run, TOF]
gridtune <- CatBoost_QA_Results_Classifier[run, GridTune]
Tar <- "Adrian"

# Refresh data
data <- RemixAutoML::FakeDataGenerator(
  Correlation = 0.85,
  N = 100000L,
  ID = 2L,
  AddWeightsColumn = TRUE,
  ZIP = 0L,
  AddDate = TRUE,
  Classification = TRUE,
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
if(!tof) {
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

OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData")
task_type = tasktypemode
NumGPUs = 1
ModelID = "Test_Model_1"
model_path = normalizePath("./")
metadata_path = normalizePath("./")
SaveModelObjects = TRUE
ReturnModelObjects = TRUE
SaveInfoToPDF = TRUE
data = TTrainData
TrainOnFull = tof
ValidationData = VValidationData
TestData = TTestData
TargetColumnName = "Adrian"
FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1","IDcol_2","DateTime","Adrian")]
PrimaryDateColumn = "DateTime"
WeightsColumnName = "Weights"
ClassWeights = c(1L,1L)
CostMatrixWeights = c(1,0,0,1)
IDcols = c("IDcol_1","IDcol_2","DateTime")
EvalMetric = "MCC"
LossFunction = "Logloss"
grid_eval_metric = "MCC"
MetricPeriods = 10L
NumOfParDepPlots = ncol(data)-1L-2L
PassInGrid = NULL
GridTune = gridtune
MaxModelsInGrid = 30L
MaxRunsWithoutNewWinner = 20L
MaxRunMinutes = 24L*60L
Shuffles = 4L
BaselineComparison = "default"
Trees = 100L # c(100L,110L,120L,130L,140L)
Depth = 4L # c(4L, 5L, 6L, 7L, 8L)
LearningRate = NULL # c(0.01,0.02,0.03,0.04,0.05)
L2_Leaf_Reg = NULL # c(1.0,2.0,3.0,4.0,5.0)
RandomStrength = 1 # c(1, 0.95, 0.9, 0.85, 0.8)
BorderCount = 128 # c(128,196,254)
RSM = 0.80 # c(0.80,0.85,0.9,0.95)
BootStrapType = "Bayesian"
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

# Grid tuning function ----
# AlgoType="catboost"
# ModelType="classification"
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
# grid_eval_metric.=grid_eval_metric
# MetricPeriods=MetricPeriods
# ClassWeights=ClassWeights
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

# Grid Parameters in repeat{} ----
# NumGPUs.=NumGPUs
# LossFunction.=LossFunction
# counter.=counter
# BanditArmsN.=BanditArmsN
# HasTime.=HasTime
# MetricPeriods.=MetricPeriods
# ClassWeights.=ClassWeights
# EvalMetric.=EvalMetric
# task_type.=task_type
# model_path.=model_path
# Grid.=Grid
# ExperimentalGrid.=ExperimentalGrid
# GridClusters.=GridClusters
# NewGrid.=if(!exists("NewGrid")) NULL else NewGrid

# DataPrep ----
# ModelType = "classification"
# WeightsColumnName.=WeightsColumnName
# data.=data
# ValidationData.=ValidationData
# TestData.=TestData
# TargetColumnName.=TargetColumnName
# FeatureColNames.=FeatureColNames
# PrimaryDateColumn.=PrimaryDateColumn
# IDcols.=IDcols
# TrainOnFull.=TrainOnFull
# SaveModelObjects.=SaveModelObjects
# TransformNumericColumns.=NULL
# Methods.=NULL
# model_path.=model_path
# ModelID.=ModelID
# DummifyCols.=FALSE
# LossFunction.=NULL
# EvalMetric.=NULL

# Data Conversion ----
CatFeatures.=CatFeatures
dataTrain.=dataTrain
dataTest.=dataTest
TestData.=TestData
TrainTarget.=TrainTarget
TestTarget.=TestTarget
FinalTestTarget.=FinalTestTarget
TrainOnFull.=TrainOnFull
Weights. = WeightsColumnName

# Catboost final args ----
# ModelType="classification"
# UseBestModel.=UseBestModel
# ClassWeights.=ClassWeights
# PassInGrid.=PassInGrid
# BestGrid.=BestGrid
# ExperimentalGrid.=ExperimentalGrid
# GridTune.=GridTune
# TrainOnFull.=TrainOnFull
# MetricPeriods.=MetricPeriods
# LossFunction.=LossFunction
# EvalMetric.=EvalMetric
# score_function.=score_function
# HasTime.=HasTime
# task_type.=task_type
# NumGPUs.=NumGPUs
# NTrees.=Trees
# Depth.=Depth
# LearningRate.=LearningRate
# L2_Leaf_Reg.=L2_Leaf_Reg
# langevin.=langevin
# diffusion_temperature.=diffusion_temperature
# sampling_unit.=sampling_unit
# RandomStrength.=RandomStrength
# BorderCount.=BorderCount
# RSM.=RSM
# GrowPolicy.=GrowPolicy
# BootStrapType.=BootStrapType
# model_size_reg.=model_size_reg
# feature_border_type.=feature_border_type
# subsample.=subsample
# min_data_in_leaf.=min_data_in_leaf

# TrainData validation data ----
# ModelType="classification"
# TrainOnFull.=TRUE
# TestDataCheck=FALSE
# FinalTestTarget.=FinalTestTarget
# TestTarget.=TestTarget
# TrainTarget.=TrainTarget
# TrainMerge. = TrainMerge
# TestMerge.=TestMerge
# dataTest.=dataTest
# data.=data
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
# ModelType="classification"
# TrainOnFull.=TrainOnFull
# TestDataCheck=!is.null(TestData)
# FinalTestTarget.=FinalTestTarget
# TestTarget.=TestTarget
# TrainTarget.=TrainTarget
# TrainMerge. = TrainMerge
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
# TransformNumericColumns.=NULL
# GridTune.=GridTune
# TransformationResults.=NULL

# Importances ----
# GrowPolicy.=GrowPolicy
# BestGrid.=BestGrid
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

# BInary Metrics ----
# ClassWeights.=ClassWeights
# CostMatrixWeights.=CostMatrixWeights
# SaveModelObjects.=SaveModelObjects
# ValidationData.=ValidationData
# TrainOnFull.=TrainOnFull
# TargetColumnName.=TargetColumnName
# ModelID.=ModelID
# model_path.=model_path
# metadata_path.=metadata_path
# Method = "bins"

# Eval Plots ----
# ModelType="classification"
# TrainOnFull.=TrainOnFull
# LossFunction.=NULL
# EvalMetric.=NULL
# EvaluationMetrics.=NULL
# ValidationData.=ValidationData
# NumOfParDepPlots.=NumOfParDepPlots
# VariableImportance.=VariableImportance
# TargetColumnName.=TargetColumnName[1L]
# FeatureColNames.=FeatureColNames
# SaveModelObjects.=SaveModelObjects
# ModelID.=ModelID
# metadata_path.=metadata_path
# model_path.=model_path

# Gains chart ----
# data = ValidationData.
# PredictedColumnName = "p1"
# TargetColumnName = eval(TargetColumnName.)
# NumBins = 20
# SavePlot = SaveModelObjects.
# Name = ModelID.
# metapath = metadata_path.
# modelpath = model_path.

# PDF ----
# ModelClass = "catboost"
# ModelType = "classification"
# TrainOnFull. = TrainOnFull
# SaveInfoToPDF. = SaveInfoToPDF
# EvalMetricsList. = EvalMetricsList
# PlotList. = PlotList
# VariableImportance. = VariableImportance
# Interaction. = Interaction
# model_path. = model_path
# metadata_path. = metadata_path

# ----

# ----
