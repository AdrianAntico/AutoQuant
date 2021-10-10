# Test data.table
XGBoost_QA_Results_Classifier <- data.table::CJ(
  TOF = c(TRUE,FALSE),
  GridTune = c(TRUE,FALSE),
  Success = "Failure",
  PartitionInFunction = c(TRUE,FALSE)
)

# Remove impossible combinations
XGBoost_QA_Results_Classifier <- XGBoost_QA_Results_Classifier[!(TOF & GridTune)]
XGBoost_QA_Results_Classifier <- XGBoost_QA_Results_Classifier[!(PartitionInFunction & TOF)]
XGBoost_QA_Results_Classifier[, RunNumber := seq_len(.N)]

#      TOF GridTune Success PartitionInFunction
# 1: FALSE    FALSE Failure               FALSE
# 2: FALSE    FALSE Failure                TRUE
# 3: FALSE     TRUE Failure               FALSE
# 4: FALSE     TRUE Failure                TRUE
# 5:  TRUE    FALSE Failure               FALSE

# AutoXGBoostClassifier
# run = 4
# run = 2
# run = 3
for(run in seq_len(XGBoost_QA_Results_Classifier[,.N])) {

  # Define values
  tof <- XGBoost_QA_Results_Classifier[run, TOF]
  PartitionInFunction <- XGBoost_QA_Results_Classifier[run, PartitionInFunction]
  gridtune <- XGBoost_QA_Results_Classifier[run, GridTune]
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

  # Run function
  TestModel <- tryCatch({RemixAutoML::AutoXGBoostClassifier(

    # GPU or CPU
    TreeMethod = "hist",
    NThreads = parallel::detectCores(),

    # Metadata arguments
    OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
    model_path = normalizePath("./"),
    metadata_path = NULL,
    ModelID = "Test_Model_1",
    EncodingMethod = "credibility",
    ReturnFactorLevels = TRUE,
    ReturnModelObjects = TRUE,
    SaveModelObjects = TRUE,
    SaveInfoToPDF = TRUE,
    DebugMode = TRUE,

    # Data arguments
    data = TTrainData,
    TrainOnFull = tof,
    ValidationData = VValidationData,
    TestData = TTestData,
    TargetColumnName = Tar,
    FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime",Tar)],
    WeightsColumnName = "Weights",
    IDcols = c("IDcol_1","IDcol_2"),

    # Model evaluation
    LossFunction = 'reg:logistic',
    eval_metric = "auc",
    grid_eval_metric = "MCC",
    CostMatrixWeights = c(1,0,0,1),
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
  if(!is.null(TestModel)) XGBoost_QA_Results_Classifier[run, Success := "Success"]
  TestModel <- NULL
  Sys.sleep(5)
  data.table::fwrite(XGBoost_QA_Results_Classifier, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoXGBoostClassifier_QA.csv")
}

# Remove all else
rm(list = ls()[!ls() %in% c(
  "XGBoost_QA_Results_MultiClass",
  "XGBoost_QA_Results_Regression",
  "XGBoost_QA_Results_Classifier",
  "CatBoost_QA_Results_MultiClass",
  "CatBoost_QA_Results_Regression",
  "CatBoost_QA_Results_Classifier")])

# Defaults ----
# library(RemixAutoML)
# library(data.table)
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/XGBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
#
# run = 3
#
# tof <- XGBoost_QA_Results_Classifier[run, TOF]
# gridtune <- XGBoost_QA_Results_Classifier[run, GridTune]
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
#   Classification = TRUE,
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
# # GPU or CPU
# OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData")
# EncodingMethod = "credibility"
# TreeMethod = "hist"
# NThreads = parallel::detectCores()
# # Metadata arguments
# model_path = normalizePath("./")
# metadata_path = NULL
# ModelID = "Test_Model_1"
# ReturnFactorLevels = TRUE
# ReturnModelObjects = TRUE
# SaveModelObjects = TRUE
# SaveInfoToPDF = TRUE
# DebugMode = TRUE
# # Data argument
# data = TTrainData
# TrainOnFull = tof
# ValidationData = VValidationData
# TestData = TTestData
# TargetColumnName = "Adrian"
# FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")]
# WeightsColumnName = "Weights"
# IDcols = c("IDcol_1","IDcol_2")
# # Model evaluatio
# LossFunction = 'reg:logistic'
# eval_metric = "auc"
# grid_eval_metric = "MCC"
# CostMatrixWeights = c(1,0,0,1)
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

# Data Prep args ----
# ModelType="classification"
# data.=data
# ValidationData.=ValidationData
# TestData.=TestData
# TargetColumnName.=TargetColumnName
# FeatureColNames.=FeatureColNames
# IDcols.=IDcols
# TransformNumericColumns.=NULL
# Methods.=NULL
# ModelID.=ModelID
# model_path.=model_path
# TrainOnFull.=TrainOnFull
# SaveModelObjects.=SaveModelObjects
# ReturnFactorLevels.=ReturnFactorLevels
# EncodingMethod. = EncodingMethod
# WeightsColumnName. = WeightsColumnName

# Encoding ----
# RunMode='train'
# ModelType=ModelType
# TrainData=dataTrain
# ValidationData=dataTest
# TestData=TestData.
# TargetVariableName=TargetColumnName.
# CategoricalVariableNames=CatFeatures
# EncodeMethod=EncodingMethod.
# KeepCategoricalVariables=FALSE
# ReturnMetaData=TRUE
# MetaDataPath=model_path.
# MetaDataList=NULL
# ReturnMetaDataList=TRUE
# ImputeMissingValue=0

# Scoring ----
# TargetType = "classifier"
# ScoringData = data2
# FeatureColumnNames = names(data1)[!names(data1) %in% c("IDcol_1", "IDcol_2","Adrian")]
# OneHot = FALSE
# IDcols = c("IDcol_1","IDcol_2")
# ModelObject = TestModel$Model
# ModelPath = getwd()
# ModelID = "Test_Model_1"
# ReturnFeatures = TRUE
# TransformNumeric = FALSE
# BackTransNumeric = FALSE
# TargetColumnName = NULL
# TransformationObject = NULL
# FactorLevelsList = TestModel$FactorLevels
# TransID = NULL
# TransPath = NULL
# MDP_Impute = TRUE
# MDP_CharToFactor = TRUE
# MDP_RemoveDates = TRUE
# MDP_MissFactor = "0"
# MDP_MissNum = -1

# Train Validation Data ----
# model.=model
# TestData.=NULL
# ModelType="classification"
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

# ValidationData ----
# model.=model
# TestData.=TestData
# ModelType="classification"
# TrainOnFull.=TrainOnFull
# TestDataCheck=!is.null(TestData)
# FinalTestTarget.=FinalTestTarget
# TestTarget.=TestTarget
# TrainTarget.=TrainTarget
# TrainMerge.=TrainMerge
# TestMerge.=TestMerge
# dataTest.=dataTest
# data.=data
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

# Grid ----
# ModelType="classification"
# TrainOnFull.=TrainOnFull
# BaselineComparison.=BaselineComparison
# MaxRunsWithoutNewWinner=MaxRunsWithoutNewWinner
# MaxModelsInGrid=MaxModelsInGrid
# MaxRunMinutes=MaxRunMinutes
# LossFunction=LossFunction
# data=data
# TestData.=TestData
# TargetLevels.=NULL
# TestTarget.=TestTarget
# FinalTestTarget.=FinalTestTarget
# TestMerge.=TestMerge
# SaveModelObjects=SaveModelObjects
# metadata_path=metadata_path
# model_path=model_path
# ModelID=ModelID
# grid_eval_metric.=grid_eval_metric
# CostMatrixWeights=CostMatrixWeights
# EvalMetric=eval_metric
# TargetColumnName.=TargetColumnName
# DebugMode.=DebugMode
# Trees.=Trees
# Depth.=max_depth
# LearningRate.=eta
# min_child_weight.=min_child_weight
# SubSample=subsample.=subsample
# colsample_bytree.=colsample_bytree
# ColSampleByTree=colsample_bytree.
# N.=N
# NewGrid.=NewGrid
# Objective.=LossFunction
# counter.=counter
# BanditArmsN.=BanditArmsN
# EvalMetric.=EvalMetric
# TreeMethod.=TreeMethod.=TreeMethod
# model_path.=model_path
# Grid.=Grid
# GridClusters.=GridClusters
# datatrain.=datatrain
# watchlist=EvalSets.=EvalSets
# verbose=Verbose.=Verbose
# datatest.=datatest


# Binary metrics ----
# MetricPeriods=NULL
# TrainPool.=NULL
# TestPool.=NULL
# FinalTestPool.=FinalTestPool
# L2_Leaf_Reg.=L2_Leaf_Reg
# task_type.=task_type
# HasTime=NULL
# BorderCount.=BorderCount
# RandomStrength.=RandomStrength
# RSM.=RSM
# BootStrapType.=BootStrapType
# GrowPolicy.=GrowPolicy
# NumGPUs=NumGPUs
# ClassWeights=ClassWeights

# Dummify args ----
# data = temp
# cols = CatFeatures
# KeepFactorCols = FALSE
# OneHot = FALSE
# SaveFactorLevels = FALSE
# ReturnFactorLevels = ReturnFactorLevels.
# SavePath = NULL
# ImportFactorLevels = FALSE
#
# # Final Args Prep
# LossFunction.=LossFunction
# eval_metric.=eval_metric
# NThreads.=NThreads
# TreeMethod.=TreeMethod
# PassInGrid.=PassInGrid
# BestGrid.=BestGrid
# Trees.=Trees

# Eval Plots ----
# ModelType="classification"
# TrainOnFull.=TrainOnFull
# ValidationData.=ValidationData
# NumOfParDepPlots.=NumOfParDepPlots
# VariableImportance.=VariableImportance
# TargetColumnName.=TargetColumnName
# FeatureColNames.=FeatureColNames
# SaveModelObjects.=SaveModelObjects
# ModelID.=ModelID
# metadata_path.=metadata_path
# model_path.=model_path
# LossFunction.=NULL
# EvalMetric.=NULL
# EvaluationMetrics.=NULL
# predict.=NULL

# Binary Metrics ----
# MLModels.="xgboost"
# ClassWeights.=NULL
# CostMatrixWeights.=CostMatrixWeights
# SaveModelObjects.=SaveModelObjects
# ValidationData.=ValidationData
# TrainOnFull.=TrainOnFull
# TargetColumnName.=TargetColumnName
# ModelID.=ModelID
# model_path.=model_path
# metadata_path.=metadata_path

# PDFs ----
# ModelClass = "xgboost"
# ModelType="classification"
# TrainOnFull.=TrainOnFull
# SaveInfoToPDF.=SaveInfoToPDF
# PlotList.=PlotList
# VariableImportance.=VariableImportance
# EvalMetricsList.=EvalMetricsList
# Interaction.=NULL
# model_path.=model_path
# metadata_path.=metadata_path


