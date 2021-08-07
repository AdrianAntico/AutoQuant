# Test data.table
CatBoost_QA <- data.table::CJ(
  TOF = c(TRUE,FALSE),
  Classification = c(TRUE,FALSE),
  TaskType = c("CPU","GPU"),
  Success = "Failure",
  ScoreSuccess = "Failure",
  PartitionInFunction = c(TRUE,FALSE), sorted = FALSE
)

# Remove impossible combinations
CatBoost_QA <- CatBoost_QA[!(PartitionInFunction & TOF)]
CatBoost_QA[, RunNumber := seq_len(.N)]

# Path File
Path <- "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV"

#       TOF Classification TaskType Success PartitionInFunction RunNumber
# 1:   TRUE           TRUE      CPU Failure               FALSE         1  success
# 2:   TRUE           TRUE      GPU Failure               FALSE         2  success
# 3:   TRUE          FALSE      CPU Failure               FALSE         3  success
# 4:   TRUE          FALSE      GPU Failure               FALSE         4  success
# 5:  FALSE           TRUE      CPU Failure                TRUE         5  fail
# 6:  FALSE           TRUE      CPU Failure               FALSE         6  fail
# 7:  FALSE           TRUE      GPU Failure                TRUE         7  fail
# 8:  FALSE           TRUE      GPU Failure               FALSE         8  fail
# 9:  FALSE          FALSE      CPU Failure                TRUE         9  fail
# 10: FALSE          FALSE      CPU Failure               FALSE        10  fail
# 11: FALSE          FALSE      GPU Failure                TRUE        11  fail
# 12: FALSE          FALSE      GPU Failure               FALSE        12  fail

# AutoCatBoostHurdleModel
# run = 5
# run = 6
for(run in seq_len(CatBoost_QA[,.N])) {

  # Define values
  tasktypemode <- CatBoost_QA[run, TaskType]
  tof <- CatBoost_QA[run, TOF]
  PartitionInFunction <- CatBoost_QA[run, PartitionInFunction]
  Classify <- CatBoost_QA[run, Classification]
  Tar <- "Adrian"

  # Get data
  if(Classify) {
    data <- RemixAutoML::FakeDataGenerator(N = 15000, ZIP = 1)
  } else {
    data <- RemixAutoML::FakeDataGenerator(N = 15000, ZIP = 2)
  }

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
  TestModel <- tryCatch({RemixAutoML::AutoCatBoostHurdleModel(

    # Operationalization
    task_type = 'GPU',
    ModelID = 'ModelTest',
    SaveModelObjects = FALSE,
    ReturnModelObjects = TRUE,

    # Data related args
    data = data.table::copy(TTrainData),
    ValidationData = VValidationData,
    TestData = TTestData,
    WeightsColumnName = NULL,
    TrainOnFull = tof,
    Buckets = if(Classify) 0L else c(0,2,3),
    TargetColumnName = "Adrian",
    FeatureColNames = names(TTrainData)[!names(data) %in% c("Adrian","IDcol_1","IDcol_2","IDcol_3","IDcol_4","IDcol_5","DateTime")],
    PrimaryDateColumn = "DateTime",
    IDcols = c("IDcol_1","IDcol_2","IDcol_3","IDcol_4","IDcol_5","DateTime"),
    DebugMode = TRUE,

    # Metadata args
    Paths = normalizePath('./'),
    MetaDataPaths = NULL,
    TransformNumericColumns = NULL,
    Methods = c('Asinh', 'Asin', 'Log', 'LogPlus1', 'Logit'),
    ClassWeights = NULL,
    SplitRatios = if(PartitionInFunction) c(0.70, 0.20, 0.10) else NULL,
    NumOfParDepPlots = 10L,

    # Grid tuning setup
    PassInGrid = NULL,
    GridTune = FALSE,
    BaselineComparison = 'default',
    MaxModelsInGrid = 1L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 60L*60L,
    MetricPeriods = 25L,

    # Bandit grid args
    Langevin = FALSE,
    DiffusionTemperature = 10000,
    Trees = list('classifier' = 50, 'regression' = 50),
    Depth = list('classifier' = 4, 'regression' = 4),
    RandomStrength = list('classifier' = 1, 'regression' = 1),
    BorderCount = list('classifier' = 32, 'regression' = 32),
    LearningRate = list('classifier' = 0.01, 'regression' = 0.01),
    L2_Leaf_Reg = list('classifier' = 3.0, 'regression' = 1.0),
    RSM = list('classifier' = 0.80, 'regression' = 0.80),
    BootStrapType = list('classifier' = 'Bayesian', 'regression' = 'Bayesian'),
    GrowPolicy = list('classifier' = 'SymmetricTree', 'regression' = 'SymmetricTree'))}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) CatBoost_QA[run, Success := "Success"]
  data.table::fwrite(CatBoost_QA, file = file.path(Path, "AutoCatBoostHurdleModel_QA.csv"))

  # Remove Target Variable
  TTrainData[, c("Target_Buckets", "Adrian") := NULL]

  # Score CatBoost Hurdle Model
  Output <- tryCatch({RemixAutoML::AutoCatBoostHurdleModelScoring(
    TestData = TTrainData,
    Path = Path,
    ModelID = "ModelTest",
    ModelList = TestModel$ModelList,
    ArgsList = TestModel$ArgsList,
    Threshold = NULL)}, error = function(x) NULL)

  # Outcome
  if(!is.null(Output)) CatBoost_QA[run, ScoreSuccess := "Success"]
  TestModel <- NULL
  Output <- NULL
  TTrainData <- NULL
  VValidationData <- NULL
  TTestData <- NULL
  gc(); Sys.sleep(5)
  data.table::fwrite(CatBoost_QA, file = file.path(Path, "AutoCatBoostHurdleModel_QA.csv"))
}

# Defaults ----
# library(RemixAutoML)
# library(data.table)
#
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
#
# # Scoring
# TestData = TTrainData
# Path = Path
# ModelID = "ModelTest"
# ModelList = TestModel$ModelList
# ArgsList = TestModel$ArgsList
# Threshold = NULL
# CARMA = FALSE

# run = 5
#
# # Define values
# tasktypemode <- CatBoost_QA[run, TaskType]
# tof <- CatBoost_QA[run, TOF]
# PartitionInFunction <- CatBoost_QA[run, PartitionInFunction]
# Classify <- CatBoost_QA[run, Classification]
# Tar <- "Adrian"
#
# # Get data
# if(Classify) {
#   data <- RemixAutoML::FakeDataGenerator(N = 15000, ZIP = 1)
# } else {
#   data <- RemixAutoML::FakeDataGenerator(N = 15000, ZIP = 2)
# }
#
# # Partition Data
# if(!tof && !PartitionInFunction) {
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
# # Operationalization
# task_type = 'GPU'
# ModelID = 'ModelTest'
# SaveModelObjects = FALSE
# ReturnModelObjects = TRUE
#
# # Data related args
# data = TTrainData
# ValidationData = VValidationData
# TestData = TTestData
# WeightsColumnName = NULL
# TrainOnFull = tof
# Buckets = if(Classify) 0L else c(0,2,3)
# TargetColumnName = "Adrian"
# FeatureColNames = names(TTrainData)[!names(data) %in% c("Adrian","IDcol_1","IDcol_2","IDcol_3","IDcol_4","IDcol_5","DateTime")]
# PrimaryDateColumn = "DateTime"
# IDcols = c("IDcol_1","IDcol_2","IDcol_3","IDcol_4","IDcol_5","DateTime")
# DebugMode = FALSE
#
# # Metadata args
# Paths = normalizePath('./')
# MetaDataPaths = NULL
# TransformNumericColumns = NULL
# Methods = c('Asinh', 'Asin', 'Log', 'LogPlus1', 'Logit')
# ClassWeights = NULL
# SplitRatios = c(0.70, 0.20, 0.10)
# NumOfParDepPlots = 10L
#
# # Grid tuning setup
# PassInGrid = NULL
# GridTune = FALSE
# BaselineComparison = 'default'
# MaxModelsInGrid = 1L
# MaxRunsWithoutNewWinner = 20L
# MaxRunMinutes = 60L*60L
# MetricPeriods = 25L
#
# # Bandit grid args
# Langevin = FALSE
# DiffusionTemperature = 10000
# Trees = list('classifier' = 50, 'regression' = 50)
# Depth = list('classifier' = 4, 'regression' = 4)
# RandomStrength = list('classifier' = 1, 'regression' = 1)
# BorderCount = list('classifier' = 32, 'regression' = 32)
# LearningRate = list('classifier' = 0.01, 'regression' = 0.01)
# L2_Leaf_Reg = list('classifier' = 3.0, 'regression' = 1.0)
# RSM = list('classifier' = 0.80, 'regression' = 0.80)
# BootStrapType = list('classifier' = 'Bayesian', 'regression' = 'Bayesian')
# GrowPolicy = list('classifier' = 'SymmetricTree', 'regression' = 'SymmetricTree')

# Scoring Multiclass ----
# RemoveModel = TRUE
# TargetType = TargetType
# ScoringData = if(!is.null(TestData)) TestData else if(!is.null(ValidationData)) data.table::copy(ValidationData) else data
# FeatureColumnNames = FeatureNames
# IDcols = IDcols
# ModelObject = ClassModel
# ModelPath = if(!is.null(ClassModel)) NULL else Paths
# ModelID = ModelID
# ReturnFeatures = TRUE
# MultiClassTargetLevels = TargetLevels
# TransformNumeric = FALSE
# BackTransNumeric = FALSE
# ReturnShapValues = FALSE
# TargetColumnName = NULL
# TransformationObject = NULL
# TransID = NULL
# TransPath = Paths
# MDP_Impute = FALSE
# MDP_CharToFactor = TRUE
# MDP_RemoveDates = FALSE
# MDP_MissFactor = '0'
# MDP_MissNum = -1

# Multiclass model ----
# task_type = task_type
# OutputSelection = c('Importances','EvalMetrics','Score_TrainData')
# ModelID = ModelID
# model_path = Paths
# metadata_path = MetaDataPaths
# SaveModelObjects = SaveModelObjects
# ReturnModelObjects = ReturnModelObjects
# data = data.table::copy(data)
# TrainOnFull = TrainOnFull
# ValidationData = data.table::copy(ValidationData)
# TestData = data.table::copy(TestData)
# TargetColumnName = 'Target_Buckets'
# FeatureColNames = FeatureNames
# PrimaryDateColumn = PrimaryDateColumn
# WeightsColumnName = WeightsColumnName
# ClassWeights = ClassWeights
# IDcols = IDcols
# eval_metric = 'MultiClass'
# MetricPeriods = MetricPeriods
# grid_eval_metric = 'accuracy'
# PassInGrid = PassInGrid
# GridTune = GridTune
# MaxModelsInGrid = MaxModelsInGrid
# MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
# MaxRunMinutes = MaxRunMinutes
# BaselineComparison = BaselineComparison
# Trees = ClassifierTrees
# Depth = ClassifierDepth
# LearningRate = ClassifierLearningRate
# L2_Leaf_Reg = ClassifierL2_Leaf_Reg
# RandomStrength = ClassifierRandomStrength
# BorderCount = ClassifierBorderCount
# RSM = ClassifierRSM
# BootStrapType = ClassifierBootStrapType
# GrowPolicy = ClassifierGrowPolicy
# langevin = FALSE
# diffusion_temperature = 10000
# model_size_reg = 0.5
# feature_border_type = 'GreedyLogSum'
# sampling_unit = 'Object'
# subsample = NULL
# score_function = 'Cosine'
# min_data_in_leaf = 1
# eval_metric = 'MultiClassOneVsAll'
# loss_function = 'MultiClassOneVsAll'
# NumGPUs = 1

# Regression model ----
# task_type = task_type
# NumGPUs = 1
# DebugMode = DebugMode
# OutputSelection = c('Importances','EvalPlots','EvalMetrics','Score_TrainData')
# ModelID = ModelIDD
# model_path = Paths
# metadata_path = MetaDataPaths
# SaveModelObjects = SaveModelObjects
# ReturnModelObjects = ReturnModelObjects
# data = data.table::copy(trainBucket)
# TrainOnFull = TrainOnFull
# ValidationData = data.table::copy(validBucket)
# TestData = data.table::copy(testBucket)
# TargetColumnName = TargetColumnName
# FeatureColNames = FeatureNames
# WeightsColumnName = WeightsColumnName
# PrimaryDateColumn = PrimaryDateColumn
# IDcols = IDcolsModified
# TransformNumericColumns = TransformNumericColumns
# Methods = Methods
# eval_metric = 'RMSE'
# loss_function = 'RMSE'
# MetricPeriods = MetricPeriods
# NumOfParDepPlots = NumOfParDepPlots
# PassInGrid = PassInGrid
# GridTune = GridTune
# MaxModelsInGrid = MaxModelsInGrid
# MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
# MaxRunMinutes = MaxRunMinutes
# BaselineComparison = BaselineComparison
# Trees = RegressionTrees
# Depth = RegressionDepth
# LearningRate = RegressionLearningRate
# L2_Leaf_Reg = RegressionL2_Leaf_Reg
# RandomStrength = RegressionRandomStrength
# BorderCount = RegressionBorderCount
# RSM = RegressionRSM
# BootStrapType = RegressionBootStrapType
# GrowPolicy = RegressionGrowPolicy
# langevin = FALSE
# diffusion_temperature = 10000
# model_size_reg = 0.5
# feature_border_type = 'GreedyLogSum'
# sampling_unit = 'Object'
# subsample = NULL
# score_function = 'Cosine'
# min_data_in_leaf = 1

# Regression Data Prep ----
# OutputSelection.=OutputSelection
# ModelType='regression'
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

# Regression catboost conversion ----
# CatFeatures.=CatFeatures
# dataTrain.=dataTrain
# dataTest.=dataTest
# TestData.=TestData
# TrainTarget.=TrainTarget
# TestTarget.=TestTarget
# FinalTestTarget.=FinalTestTarget
# TrainOnFull.=TrainOnFull
# Weights.=WeightsColumnName
