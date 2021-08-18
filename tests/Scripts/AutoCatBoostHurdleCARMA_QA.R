# Collection data.table
QA_Results <- data.table::CJ(
  Group = c(0,1,2,3),
  xregs = c(0,1,2,3),
  TOF = c(TRUE, FALSE),
  Trans = c(TRUE, FALSE))

# Other tests
QA_Results[, TimeWeights := data.table::fifelse(runif(.N) < 0.5, 0.9999, 1)]
QA_Results[, TaskType := data.table::fifelse(runif(.N) < 0.5, "GPU", "CPU")]
QA_Results[, Success := "Failure"]

# run = 5
for(run in  seq_len(QA_Results[,.N])) {

  # Data
  if(QA_Results[run, Group] == 0L) {
    data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/NoGroup-Eval-Walmart.csv")
  } else if(QA_Results[run, Group] == 1L) {
    data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/OneGroup-Eval-Walmart.csv")
  } else if(QA_Results[run, Group] == 2L) {
    data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/TwoGroup-Eval-Walmart.csv")
  } else if(QA_Results[run, Group] == 3L) {
    data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ThreeGroup-Eval-Walmart.csv")
  }

  # xregs
  if(QA_Results[run, xregs] == 0L) {
    xregs <- NULL
  } else if(QA_Results[run, xregs] == 1L) {
    if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/NoGroup-FC-Walmart-XREG1.csv")
    if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/OneGroup-FC-Walmart-XREG1.csv")
    if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/TwoGroup-FC-Walmart-XREG1.csv")
    if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ThreeGroup-FC-Walmart-XREG1.csv")
  } else if(QA_Results[run, xregs] == 2L) {
    if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/NoGroup-FC-Walmart-XREG2.csv")
    if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/OneGroup-FC-Walmart-XREG2.csv")
    if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/TwoGroup-FC-Walmart-XREG2.csv")
    if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ThreeGroup-FC-Walmart-XREG2.csv")
  } else if(QA_Results[run, xregs] == 3L) {
    if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/NoGroup-FC-Walmart-XREG3.csv")
    if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/OneGroup-FC-Walmart-XREG3.csv")
    if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/TwoGroup-FC-Walmart-XREG3.csv")
    if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets/ThreeGroup-FC-Walmart-XREG3.csv")
  }

  # Testing params
  TOF <- QA_Results[run, TOF]
  Trans <- QA_Results[run, Trans]
  weights <- QA_Results[run, TimeWeights]
  tasktype <- QA_Results[run, TaskType]
  if(QA_Results[run, Group] == 0L) {
    groupvariables <- NULL
  } else if(QA_Results[run, Group] == 1L) {
    groupvariables <- "Dept"
  } else if(QA_Results[run, Group] == 2L) {
    groupvariables <- c("Store","Dept")
  } else if(QA_Results[run, Group] == 3L) {
    groupvariables <- c("Region","Store","Dept")
  }

  # Ensure series have no missing dates (also remove series with more than 25% missing values)
  data <- RemixAutoML::TimeSeriesFill(
    data,
    DateColumnName = "Date",
    GroupVariables = groupvariables,
    TimeUnit = "weeks",
    FillType = "maxmax",
    MaxMissingPercent = 0.25,
    SimpleImpute = TRUE)

  # Set negative numbers to 0
  data <- data[, Weekly_Sales := data.table::fifelse(Weekly_Sales < 0, 0, Weekly_Sales)]

  # Ensure series have no missing dates (also remove series with more than 25% missing values)
  if(QA_Results[run, xregs] != 0L) {
    xregs <- RemixAutoML::TimeSeriesFill(
      xregs,
      DateColumnName = "Date",
      GroupVariables = groupvariables,
      TimeUnit = "weeks",
      FillType = "maxmax",
      MaxMissingPercent = 0.25,
      SimpleImpute = TRUE)
  }

  # Copy data
  data[, Weekly_Sales := data.table::fifelse(runif(.N) < 0.35, 0, Weekly_Sales)]
  data1 <- data.table::copy(data)
  if(QA_Results[run, xregs] != 0L) xregs1 <- data.table::copy(xregs) else xregs1 <- NULL

  # Build forecast
  TestModel <- tryCatch({RemixAutoML::AutoCatBoostHurdleCARMA(

    # data args
    data = data1,
    XREGS = xregs1,
    TargetColumnName = "Weekly_Sales",
    DateColumnName = "Date",
    HierarchGroups = NULL,
    GroupVariables = groupvariables,
    TimeWeights = weights,
    TimeUnit = "weeks",
    TimeGroups = c("weeks","months"),

    # Production args
    TrainOnFull = TOF,
    SplitRatios = c(1 - 10 / 110, 10 / 110),
    PartitionType = "random",
    FC_Periods = 4,
    Timer = TRUE,
    DebugMode = TRUE,

    # Target transformations
    TargetTransformation = Trans,
    Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),
    Difference = FALSE,
    NonNegativePred = FALSE,
    Threshold = NULL,
    RoundPreds = FALSE,

    # Date features
    CalendarVariables = c("week", "wom", "month", "quarter"),
    HolidayVariable = c("USPublicHolidays", "EasterGroup", "ChristmasGroup","OtherEcclesticalFeasts"),
    HolidayLookback = NULL,
    HolidayLags = 1,
    HolidayMovingAverages = 1:2,

    # Time series features
    Lags = list("weeks" = c(1:5), "months" = c(1:3)),
    MA_Periods = list("weeks" = c(2:5), "months" = c(2,3)),
    SD_Periods = NULL,
    Skew_Periods = NULL,
    Kurt_Periods = NULL,
    Quantile_Periods = NULL,
    Quantiles_Selected = NULL,

    # Bonus features
    AnomalyDetection = NULL,
    FourierTerms = 0,
    TimeTrendVariable = TRUE,
    ZeroPadSeries = NULL,
    DataTruncate = FALSE,

    # ML Args
    NumOfParDepPlots = 3L,
    EvalMetric = "RMSE",
    GridTune = FALSE,
    PassInGrid = NULL,
    ModelCount = 5,
    TaskType = tasktype,
    NumGPU = 1,
    MaxRunsWithoutNewWinner = 50,
    MaxRunMinutes = 60*60,
    NTrees = list("classifier" = 50, "regression" = 50),
    Depth = list("classifier" = 4, "regression" = 4),
    LearningRate = list("classifier" = NULL, "regression" = NULL),
    L2_Leaf_Reg = list("classifier" = NULL, "regression" = NULL),
    RandomStrength = list("classifier" = 1, "regression" = 1),
    BorderCount = list("classifier" = 32, "regression" = 32),
    BootStrapType = list("classifier" = "Bayesian", "regression" = "Bayesian"))}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoCatBoostHurdleCARMA_QA.csv")
  Sys.sleep(5)
}

# Hurdle QA Defaults ----
# library(RemixAutoML)
# library(data.table)
# library(lubridate)
#
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CalendarTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CrossRowOperations.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CARMA-HelperFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
#
# run = 3
#
# # Data
# if(QA_Results[run, Group] == 0L) {
#   data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-Eval-Walmart.csv")
# } else if(QA_Results[run, Group] == 1L) {
#   data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-Eval-Walmart.csv")
# } else if(QA_Results[run, Group] == 2L) {
#   data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-Eval-Walmart.csv")
# } else if(QA_Results[run, Group] == 3L) {
#   data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-Eval-Walmart.csv")
# }
#
# # xregs
# if(QA_Results[run, xregs] == 0L) {
#   xregs <- NULL
# } else if(QA_Results[run, xregs] == 1L) {
#   if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-FC-Walmart-XREG1.csv")
#   if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-FC-Walmart-XREG1.csv")
#   if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-FC-Walmart-XREG1.csv")
#   if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-FC-Walmart-XREG1.csv")
# } else if(QA_Results[run, xregs] == 2L) {
#   if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-FC-Walmart-XREG2.csv")
#   if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-FC-Walmart-XREG2.csv")
#   if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-FC-Walmart-XREG2.csv")
#   if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-FC-Walmart-XREG2.csv")
# } else if(QA_Results[run, xregs] == 3L) {
#   if(QA_Results[run, Group] == 0L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/NoGroup-FC-Walmart-XREG3.csv")
#   if(QA_Results[run, Group] == 1L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/OneGroup-FC-Walmart-XREG3.csv")
#   if(QA_Results[run, Group] == 2L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/TwoGroup-FC-Walmart-XREG3.csv")
#   if(QA_Results[run, Group] == 3L) xregs <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/QA_DataSets/ThreeGroup-FC-Walmart-XREG3.csv")
# }
#
# # Testing params
# TOF <- QA_Results[run, TOF]
# Trans <- QA_Results[run, Trans]
# weights <- QA_Results[run, TimeWeights]
# tasktype <- QA_Results[run, TaskType]
# if(QA_Results[run, Group] == 0L) {
#   groupvariables <- NULL
# } else if(QA_Results[run, Group] == 1L) {
#   groupvariables <- "Dept"
# } else if(QA_Results[run, Group] == 2L) {
#   groupvariables <- c("Store","Dept")
# } else if(QA_Results[run, Group] == 3L) {
#   groupvariables <- c("Region","Store","Dept")
# }
#
# # Ensure series have no missing dates (also remove series with more than 25% missing values)
# data <- RemixAutoML::TimeSeriesFill(
#   data,
#   DateColumnName = "Date",
#   GroupVariables = groupvariables,
#   TimeUnit = "weeks",
#   FillType = "maxmax",
#   MaxMissingPercent = 0.25,
#   SimpleImpute = TRUE)
#
# # Set negative numbers to 0
# data <- data[, Weekly_Sales := data.table::fifelse(Weekly_Sales < 0, 0, Weekly_Sales)]
#
# # Ensure series have no missing dates (also remove series with more than 25% missing values)
# if(QA_Results[run, xregs] != 0L) {
#   xregs <- RemixAutoML::TimeSeriesFill(
#     xregs,
#     DateColumnName = "Date",
#     GroupVariables = groupvariables,
#     TimeUnit = "weeks",
#     FillType = "maxmax",
#     MaxMissingPercent = 0.25,
#     SimpleImpute = TRUE)
# }
#
# # Copy data
# data[, Weekly_Sales := data.table::fifelse(runif(.N) < 0.35, 0, Weekly_Sales)]
# data1 <- data.table::copy(data)
# if(QA_Results[run, xregs] != 0L) xregs1 <- data.table::copy(xregs) else xregs1 <- NULL
#
# data = data1
# XREGS = xregs1
# TargetColumnName = "Weekly_Sales"
# DateColumnName = "Date"
# HierarchGroups = NULL
# GroupVariables = groupvariables
# TimeWeights = 1
# TimeUnit = "weeks"
# TimeGroups = c("weeks","months")
# TrainOnFull = TRUE
# SplitRatios = c(1 - 10 / 110, 10 / 110)
# PartitionType = "random"
# FC_Periods = 4
# Timer = TRUE
# DebugMode = TRUE
# TargetTransformation = TRUE
# Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit")
# Difference = FALSE
# NonNegativePred = FALSE
# Threshold = NULL
# RoundPreds = FALSE
# CalendarVariables = c("week", "wom", "month", "quarter")
# HolidayVariable = c("USPublicHolidays", "EasterGroup", "ChristmasGroup","OtherEcclesticalFeasts")
# HolidayLookback = 7
# HolidayLags = 1
# HolidayMovingAverages = 1:2
# Lags = list("weeks" = seq(2L, 10L, 2L), "months" = c(1:3))
# MA_Periods = list("weeks" = seq(2L, 10L, 2L), "months" = c(2,3))
# SD_Periods = NULL
# Skew_Periods = NULL
# Kurt_Periods = NULL
# Quantile_Periods = NULL
# Quantiles_Selected = c("q5","q95")
# AnomalyDetection = NULL
# FourierTerms = 0
# TimeTrendVariable = TRUE
# ZeroPadSeries = NULL
# DataTruncate = FALSE
# NumOfParDepPlots = 100L
# EvalMetric = "RMSE"
# GridTune = FALSE
# PassInGrid = NULL
# ModelCount = 5
# TaskType = tasktype
# NumGPU = 1
# MaxRunsWithoutNewWinner = 50
# MaxRunMinutes = 60*60
# NTrees = list("classifier" = 100, "regression" = 100)
# Depth = list("classifier" = 9, "regression" = 9)
# LearningRate = list("classifier" = NULL, "regression" = NULL)
# L2_Leaf_Reg = list("classifier" = NULL, "regression" = NULL)
# RandomStrength = list("classifier" = 1, "regression" = 1)
# BorderCount = list("classifier" = 254, "regression" = 254)
# BootStrapType = list("classifier" = "Bayesian", "regression" = "Bayesian")
#
# # Hurdle Model ----
# task_type = TaskType
# ModelID = "ModelTest"
# SaveModelObjects = FALSE
# ReturnModelObjects = TRUE
# data = data.table::copy(train)
# TrainOnFull = TrainOnFull
# ValidationData = data.table::copy(valid)
# TestData = data.table::copy(test)
# Buckets = 0L
# TargetColumnName = TargetVariable
# FeatureColNames = ModelFeatures
# PrimaryDateColumn = eval(DateColumnName)
# WeightsColumnName = NULL
# IDcols = IDcols
# Paths = normalizePath("./")
# MetaDataPaths = NULL
# TransformNumericColumns = NULL
# Methods = NULL
# ClassWeights = c(1,1)
# SplitRatios = c(0.70, 0.20, 0.10)
# NumOfParDepPlots = NumOfParDepPlots
# PassInGrid = PassInGrid
# GridTune = GridTune
# BaselineComparison = "default"
# MaxModelsInGrid = 500L
# MaxRunsWithoutNewWinner = 100L
# MaxRunMinutes = 60*60
# MetricPeriods = 10L
# Trees = NTrees
# Depth = Depth
# RandomStrength = RandomStrength
# BorderCount = BorderCount
# LearningRate = LearningRate
# L2_Leaf_Reg = L2_Leaf_Reg
# RSM = list("classifier" = c(1.0), "regression" = c(1.0))
# BootStrapType = BootStrapType
# GrowPolicy = list("classifier" = c("SymmetricTree"), "regression" = c("SymmetricTree"))

# Hurdle regression ----
# task_type = task_type
# NumGPUs = 1
# DebugMode = DebugMode
# OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "PDFs", "Score_TrainData")
# # Metadata argument
# ModelID = ModelIDD
# model_path = Paths
# metadata_path = MetaDataPaths
# SaveModelObjects = SaveModelObjects
# ReturnModelObjects = ReturnModelObjects
# # Data argument
# data = data.table::copy(trainBucket)
# TrainOnFull = TrainOnFull
# ValidationData = data.table::copy(validBucket)
# TestData = data.table::copy(testBucket)
# TargetColumnName = TargetColumnName
# FeatureColNames = FeatureNames
# PrimaryDateColumn = PrimaryDateColumn
# WeightsColumnName = TimeWeights
# IDcols = IDcolsModified
# TransformNumericColumns = TransformNumericColumns
# Methods = Methods
# # Model evaluatio
# eval_metric = "RMSE"
# loss_function = "RMSE"
# MetricPeriods = MetricPeriods
# NumOfParDepPlots = NumOfParDepPlots
# # Grid tuning arguments - PassInGrid is the best otrics
# PassInGrid = PassInGrid
# GridTune = GridTune
# MaxModelsInGrid = MaxModelsInGrid
# MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
# MaxRunMinutes = MaxRunMinutes
# BaselineComparison = BaselineComparison
# # Trees, Depth, and LearningRate used in the bandi tuning
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
# feature_border_type = "GreedyLogSum"
# sampling_unit = "Object"
# subsample = NULL
# score_function = "Cosine"
# min_data_in_leaf = 1

# CatBoost data prep ----
# ModelType="regression"
# data.=data
# ValidationData.=ValidationData
# TestData.=TestData
# TargetColumnName.=TargetColumnName
# FeatureColNames.=FeatureColNames
# PrimaryDateColumn.=PrimaryDateColumn
# IDcols.=IDcols
# TrainOnFull.=TrainOnFull
# SaveModelObjects.=SaveModelObjects
# TransformNumericColumns.=TransformNumericColumns
# Methods.=Methods
# model_path.=model_path
# ModelID.=ModelID
# DummifyCols.=DummifyCols
# LossFunction.=LossFunction
# EvalMetric.=EvalMetric

# Classifier ----
# # GPU or CPU
# task_type = task_type
# NumGPUs = 1
#
# # Metadata arguments
# ModelID = ModelID
# model_path = Paths
# metadata_path = MetaDataPaths
# SaveModelObjects = SaveModelObjects
# ReturnModelObjects = ReturnModelObjects
#
# # Data arguments
# data = data.table::copy(data)
# TrainOnFull = FALSE
# ValidationData = data.table::copy(ValidationData)
# TestData = data.table::copy(TestData)
# TargetColumnName = "Target_Buckets"
# FeatureColNames = FeatureNames
# PrimaryDateColumn = PrimaryDateColumn
# ClassWeights = ClassWeights
# IDcols = IDcols
#
# # Model evaluation
# EvalMetric = "MCC"
# LossFunction = "Logloss"
# MetricPeriods = MetricPeriods
# NumOfParDepPlots = NumOfParDepPlots
#
# # Grid tuning arguments - PassInGrid is the best of GridMetrics
# PassInGrid = PassInGrid
# GridTune = GridTune
# MaxModelsInGrid = MaxModelsInGrid
# MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
# MaxRunMinutes = MaxRunMinutes
# BaselineComparison = BaselineComparison
#
# # Trees, Depth, and LearningRate used in the bandit grid tuning
# Trees = ClassifierTrees
# Depth = ClassifierDepth
# LearningRate = ClassifierLearningRate
# RandomStrength = ClassifierRandomStrength
# BorderCount = ClassifierBorderCount
# L2_Leaf_Reg = ClassifierL2_Leaf_Reg
# RSM = ClassifierRSM
# BootStrapType = ClassifierBootStrapType
# GrowPolicy = ClassifierGrowPolicy
# langevin = FALSE
# diffusion_temperature = 10000
# model_size_reg = 0.5
# feature_border_type = "GreedyLogSum"
# sampling_unit = "Object"
# subsample = NULL
# score_function = "Cosine"
# min_data_in_leaf = 1

# CatboostImportances ----
# ModelType="classification"
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

# Carma Scoring ----
# TestData = data.table::copy(Step1SCore)
# Path = NULL
# ModelID = "ModelTest"
# ModelClass = "catboost"
# ModelList = TestModel$ModelList
# ArgList = TestModel$ArgsList
# Threshold = Threshold
# CARMA = TRUE

# Catboost scoring ----
# RemoveModel = TRUE
# TargetType = TargetType
# ScoringData = if(!is.null(TestData)) TestData else if(!is.null(ValidationData)) ValidationData else data
# FeatureColumnNames = FeatureNames
# IDcols = IDcols
# ModelObject = ClassModel
# ModelPath = Paths
# ModelID = ModelID
# ReturnFeatures = TRUE
# MultiClassTargetLevels = TargetLevels
# TransformNumeric = FALSE
# BackTransNumeric = FALSE
# TargetColumnName = NULL
# TransformationObject = NULL
# TransID = NULL
# TransPath = Paths
# MDP_Impute = FALSE
# MDP_CharToFactor = TRUE
# MDP_RemoveDates = FALSE
# MDP_MissFactor = "0"
# MDP_MissNum = -1

# ----

# ----
