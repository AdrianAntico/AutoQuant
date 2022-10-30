# Collection data.table
QA_Results <- data.table::CJ(
  Group = c(0,1,2,3),
  xregs = c(0,1,2,3),
  TOF = c(TRUE, FALSE),
  Trans = c(TRUE, FALSE))

# Other tests
QA_Results[, TimeWeights := data.table::fifelse(runif(.N) < 0.5, 0.9999, 1)]
QA_Results[, Success := "Failure"]

# run = 17
for(run in  seq_len(QA_Results[,.N])) {

  # Data
  if(QA_Results[run, Group] == 0L) {
    data <- RemixAutoML:::Post_Query_Helper('"nogroupevalwalmart.csv"')[['data']]
  } else if(QA_Results[run, Group] == 1L) {
    data <- RemixAutoML:::Post_Query_Helper('"onegroupevalwalmart.csv"')[['data']]
  } else if(QA_Results[run, Group] == 2L) {
    data <- RemixAutoML:::Post_Query_Helper('"twogroupevalwalmart.csv"')[['data']]
  } else if(QA_Results[run, Group] == 3L) {
    data <- RemixAutoML:::Post_Query_Helper('"threegroupevalwalmart.csv"')[['data']]
  }

  # xregs
  if(QA_Results[run, xregs] == 0L) {
    xregs <- NULL
  } else if(QA_Results[run, xregs] == 1L) {
    if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg1.csv"))[['data']]
    if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg1.csv"))[['data']]
    if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg1.csv"))[['data']]
    if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg1.csv"))[['data']]
  } else if(QA_Results[run, xregs] == 2L) {
    if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg2.csv"))[['data']]
    if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg2.csv"))[['data']]
    if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg2.csv"))[['data']]
    if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg2.csv"))[['data']]
  } else if(QA_Results[run, xregs] == 3L) {
    if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg3.csv"))[['data']]
    if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg3.csv"))[['data']]
    if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg3.csv"))[['data']]
    if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg3.csv"))[['data']]
  }

  # Testing params)
  TOF <- QA_Results[run, TOF]
  Trans <- QA_Results[run, Trans]
  weights <- QA_Results[run, TimeWeights]
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
  TestModel <- tryCatch({RemixAutoML::AutoXGBoostHurdleCARMA(

    # data args
    data = data1,
    XREGS = xregs1,
    TargetColumnName = "Weekly_Sales",
    DateColumnName = "Date",
    HierarchGroups = NULL,
    GroupVariables = groupvariables,
    EncodingMethod = 'credibility',
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
    MaxRunsWithoutNewWinner = 50,
    MaxRunMinutes = 60*60,

    # XGBoost Args
    TreeMethod = "hist",
    Trees = list("classifier" = 100, "regression" = 100),
    eta = list("classifier" = 0.05, "regression" = 0.05),
    max_depth = list("classifier" = 4L, "regression" = 4L),
    min_child_weight = list("classifier" = 1.0, "regression" = 1.0),
    subsample = list("classifier" = 0.55, "regression" = 0.55),
    colsample_bytree = list("classifier" = 0.55, "regression" = 0.55))}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  TestModel <- NULL
  RemixAutoML:::Post_Append_Helper(QA_Results,'AutoXGBoostHurdleCARMA_QA')
  Sys.sleep(5)
}


# Hurdle QA Defaults ----
# library(RemixAutoML)
# library(data.table)
# library(lubridate)
#
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/AutoXGBoostRegression.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/XGBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CalendarTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CharacterTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CrossRowOperations.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CARMA-HelperFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
#
# # Collection data.table
# QA_Results <- data.table::CJ(
#   Group = c(0,1,2,3),
#   xregs = c(0,1,2,3),
#   TOF = c(TRUE, FALSE),
#   Trans = c(TRUE, FALSE))
#
# # Other tests
# QA_Results[, TimeWeights := data.table::fifelse(runif(.N) < 0.5, 0.9999, 1)]
# QA_Results[, Success := "Failure"]
#
# run = 1
#
# # Data
# if(QA_Results[run, Group] == 0L) {
#   data <- RemixAutoML:::Post_Query_Helper('"nogroupevalwalmart.csv"')[['data']]
# } else if(QA_Results[run, Group] == 1L) {
#   data <- RemixAutoML:::Post_Query_Helper('"onegroupevalwalmart.csv"')[['data']]
# } else if(QA_Results[run, Group] == 2L) {
#   data <- RemixAutoML:::Post_Query_Helper('"twogroupevalwalmart.csv"')[['data']]
# } else if(QA_Results[run, Group] == 3L) {
#   data <- RemixAutoML:::Post_Query_Helper('"threegroupevalwalmart.csv"')[['data']]
# }
#
# # xregs
# if(QA_Results[run, xregs] == 0L) {
#   xregs <- NULL
# } else if(QA_Results[run, xregs] == 1L) {
#   if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg1.csv"))[['data']]
#   if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg1.csv"))[['data']]
#   if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg1.csv"))[['data']]
#   if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg1.csv"))[['data']]
# } else if(QA_Results[run, xregs] == 2L) {
#   if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg2.csv"))[['data']]
#   if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg2.csv"))[['data']]
#   if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg2.csv"))[['data']]
#   if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg2.csv"))[['data']]
# } else if(QA_Results[run, xregs] == 3L) {
#   if(QA_Results[run, Group] == 0L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("nogroupfcwalmartxreg3.csv"))[['data']]
#   if(QA_Results[run, Group] == 1L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("onegroupfcwalmartxreg3.csv"))[['data']]
#   if(QA_Results[run, Group] == 2L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("twogroupfcwalmartxreg3.csv"))[['data']]
#   if(QA_Results[run, Group] == 3L) xregs <- RemixAutoML:::Post_Query_Helper(shQuote("threegroupfcwalmartxreg3.csv"))[['data']]
# }
#
# # Testing params)
# TOF <- QA_Results[run, TOF]
# Trans <- QA_Results[run, Trans]
# weights <- QA_Results[run, TimeWeights]
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
#
# # data args
# data = data1
# XREGS = xregs1
# TargetColumnName = "Weekly_Sales"
# DateColumnName = "Date"
# HierarchGroups = NULL
# GroupVariables = groupvariables
# EncodingMethod = 'credibility'
# TimeWeights = weights
# TimeUnit = "weeks"
# TimeGroups = c("weeks","months")
# TrainOnFull = TOF
# SplitRatios = c(1 - 10 / 110, 10 / 110)
# PartitionType = "random"
# FC_Periods = 4
# Timer = TRUE
# DebugMode = TRUE
# TargetTransformation = Trans
# Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit")
# Difference = FALSE
# NonNegativePred = FALSE
# Threshold = NULL
# RoundPreds = FALSE
# CalendarVariables = c("week", "wom", "month", "quarter")
# HolidayVariable = c("USPublicHolidays", "EasterGroup", "ChristmasGroup","OtherEcclesticalFeasts")
# HolidayLookback = NULL
# HolidayLags = 1
# HolidayMovingAverages = 1:2
# Lags = list("weeks" = c(1:5), "months" = c(1:3))
# MA_Periods = list("weeks" = c(2:5), "months" = c(2,3))
# SD_Periods = NULL
# Skew_Periods = NULL
# Kurt_Periods = NULL
# Quantile_Periods = NULL
# Quantiles_Selected = NULL
# AnomalyDetection = NULL
# FourierTerms = 0
# TimeTrendVariable = TRUE
# ZeroPadSeries = NULL
# DataTruncate = FALSE
# NumOfParDepPlots = 3L
# EvalMetric = "RMSE"
# GridTune = FALSE
# PassInGrid = NULL
# ModelCount = 5
# MaxRunsWithoutNewWinner = 50
# MaxRunMinutes = 60*60
# TreeMethod = "hist"
# Trees = list("classifier" = 100, "regression" = 100)
# eta = list("classifier" = 0.05, "regression" = 0.05)
# max_depth = list("classifier" = 4L, "regression" = 4L)
# min_child_weight = list("classifier" = 1.0, "regression" = 1.0)
# subsample = list("classifier" = 0.55, "regression" = 0.55)
# colsample_bytree = list("classifier" = 0.55, "regression" = 0.55)
#
# # Hurdle Model ----
# NThreads = 6
# ModelID = 'ModelTest'
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
# WeightsColumnName = if('Weights' %chin% names(train)) 'Weights' else NULL
# IDcols = IDcols
# EncodingMethod = EncodingMethod
# DebugMode = DebugMode
# Paths = getwd()
# MetaDataPaths = NULL
# TransformNumericColumns = NULL
# Methods = NULL
# ClassWeights = c(1,1)
# SplitRatios = c(0.70, 0.20, 0.10)
# NumOfParDepPlots = NumOfParDepPlots
# PassInGrid = PassInGrid
# GridTune = GridTune
# BaselineComparison = 'default'
# MaxModelsInGrid = 500L
# MaxRunsWithoutNewWinner = 100L
# MaxRunMinutes = 60*60
# Trees = Trees
# eta = eta
# max_depth = max_depth
# min_child_weight = min_child_weight
# subsample = subsample
# colsample_bytree = colsample_bytree

# XGBoostRegression DataPrep
# Algo='xgboost'
# ModelType='regression'
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
# DebugMode.=DebugMode

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
