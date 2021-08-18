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

# run = 3
for(run in seq_len(QA_Results[,.N])) {

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
  data1 <- data.table::copy(data)
  if(QA_Results[run, xregs] != 0L) xregs1 <- data.table::copy(xregs) else xregs1 <- NULL

  # Profit target variable
  data1[, Weekly_Profit := jitter(Weekly_Sales, factor = 2500)]

  # Build forecast
  TestModel <- tryCatch({RemixAutoML::AutoCatBoostVectorCARMA(

    # data args
    data = data1,
    XREGS = xregs1,
    TargetColumnName = c("Weekly_Sales","Weekly_Profit"),
    DateColumnName = "Date",
    HierarchGroups = NULL,
    GroupVariables = groupvariables,
    TimeWeights = weights,
    TimeUnit = "weeks",
    TimeGroups = c("weeks","months"),

    # Production args
    TrainOnFull = TOF,
    SplitRatios = c(1 - 10 / 100, 10 / 100),
    PartitionType = "random",
    FC_Periods = 4,
    Timer = TRUE,
    DebugMode = TRUE,

    # Target transformations
    TargetTransformation = Trans,
    Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),
    Difference = FALSE,
    NonNegativePred = FALSE,
    RoundPreds = FALSE,

    # Date features
    CalendarVariables = c("week", "month", "quarter"),
    HolidayVariable = c("USPublicHolidays", "EasterGroup", "ChristmasGroup", "OtherEcclesticalFeasts"),
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

    # Eval args
    NumOfParDepPlots = 1L,
    EvalMetric = "MultiRMSE",
    EvalMetricValue = 1.5,
    LossFunction = "MultiRMSE",
    LossFunctionValue = 1.5,

    # Grid args
    GridTune = FALSE,
    PassInGrid = NULL,
    ModelCount = 5,
    MaxRunsWithoutNewWinner = 50,
    MaxRunMinutes = 60*60,

    # ML Args
    NTrees = 50,
    Depth = 4,
    LearningRate = NULL,
    L2_Leaf_Reg = NULL,
    RandomStrength = 1,
    BorderCount = 32,
    RSM = 1,
    BootStrapType = "Bayesian",
    GrowPolicy = "SymmetricTree",
    Langevin = FALSE,
    DiffusionTemperature = 10000,
    ModelSizeReg = 0.5,
    FeatureBorderType = "GreedyLogSum",
    SamplingUnit = "Group",
    SubSample = NULL,
    ScoreFunction = "Cosine",
    MinDataInLeaf = 1)}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoCatBoostVectorCARMA_QA.csv")
  Sys.sleep(5)
}

# Vector carma qa ----
# library(RemixAutoML)
# library(data.table)
# library(lubridate)
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CalendarTypes.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CrossRowOperations.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CARMA-HelperFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
#
# run = 2
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
# data1 <- data.table::copy(data)
# if(QA_Results[run, xregs] != 0L) xregs1 <- data.table::copy(xregs) else xregs1 <- NULL
#
# # Profit target variable
# data1[, Weekly_Profit := jitter(Weekly_Sales, factor = 2500)]
#
# # data args
# data = data1
# XREGS = xregs1
# TimeWeights = 1
# TargetColumnName = c("Weekly_Sales","Weekly_Profit")
# DateColumnName = "Date"
# HierarchGroups = NULL
# GroupVariables = groupvariables
# TimeUnit = "weeks"
# TimeGroups = c("weeks","months")
# TrainOnFull = TOF
# SplitRatios = c(1 - 10 / 100, 10 / 100)
# PartitionType = "random"
# FC_Periods = 4
# Timer = TRUE
# DebugMode = TRUE
# TargetTransformation = Trans
# Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit")
# Difference = FALSE
# NonNegativePred = FALSE
# RoundPreds = FALSE
# CalendarVariables = c("week", "month", "quarter")
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
# NumOfParDepPlots = 100L
# EvalMetric = "MultiRMSE"
# EvalMetricValue = 1.5
# LossFunction = "MultiRMSE"
# LossFunctionValue = 1.5
# GridTune = FALSE
# PassInGrid = NULL
# ModelCount = 5
# TaskType = "GPU"
# NumGPU = 1
# MaxRunsWithoutNewWinner = 50
# MaxRunMinutes = 60*60
# Langevin = FALSE
# DiffusionTemperature = 10000
# NTrees = 50
# L2_Leaf_Reg = NULL
# LearningRate = NULL
# RandomStrength = 1
# BorderCount = 32
# Depth = 4
# RSM = 1
# BootStrapType = "Bayesian"
# GrowPolicy = "SymmetricTree"
# ModelSizeReg = 0.5
# FeatureBorderType = "GreedyLogSum"
# SamplingUnit = "Group"
# SubSample = NULL
# ScoreFunction = "Cosine"
# MinDataInLeaf = 1

# CatBoost regression ----
# OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData")
# task_type = TaskType
# NumGPUs = NumGPU
# ModelID = "ModelTest"
# model_path = getwd()
# metadata_path = getwd()
# SaveModelObjects = FALSE
# ReturnModelObjects = TRUE
# data = train
# TrainOnFull = TOF
# ValidationData = valid
# TestData = test
# WeightsColumnName = if("Weights" %in% names(train)) "Weights" else NULL
# TargetColumnName = TargetVariable
# FeatureColNames = ModelFeatures
# PrimaryDateColumn = eval(DateColumnName)
# IDcols = IDcols
# TransformNumericColumns = if(TargetTransformation) TargetVariable else NULL
# Methods = NULL
# eval_metric = EvalMetric
# eval_metric_value = EvalMetricValue
# loss_function = LossFunction
# loss_function_value = LossFunctionValue
# MetricPeriods = 10L
# NumOfParDepPlots = NumOfParDepPlots
# EvalPlots = TRUE
# PassInGrid = PassInGrid
# GridTune = GridTune
# MaxModelsInGrid = ModelCount
# MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
# MaxRunMinutes = 60*60
# BaselineComparison = "default"
# langevin = Langevin
# diffusion_temperature = DiffusionTemperature
# Trees = NTrees
# Depth = Depth
# LearningRate = LearningRate
# L2_Leaf_Reg = L2_Leaf_Reg
# RandomStrength = RandomStrength
# BorderCount = BorderCount
# RSM = if(TaskType == "GPU") NULL else RS
# BootStrapType = BootStrapType
# GrowPolicy = GrowPolicy
# model_size_reg = ModelSizeReg
# feature_border_type = FeatureBorderType
# sampling_unit = SamplingUnit
# subsample = SubSample
# score_function = ScoreFunction
# min_data_in_leaf = MinDataInLeaf

# DataPrep ----
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

# Dummify ----
# data = temp
# cols = if(!is.character(CatFeatures)) names(temp)[CatFeatures] else CatFeatures
# KeepFactorCols = FALSE
# OneHot = FALSE
# SaveFactorLevels = if(SaveModelObjects.) TRUE else FALSE
# ReturnFactorLevels = TRUE
# SavePath = if(SaveModelObjects.) model_path. else NULL
# ImportFactorLevels = FALSE
# GroupVar = TRUE

# Train Validation Data ----
# ModelType="regression"
# TrainOnFull.=TRUE
# TestDataCheck=FALSE
# FinalTestTarget.=FinalTestTarget
# TestTarget.=TestTarget
# TrainTarget.=TrainTarget
# TrainMerge.=TrainMerge
# TestMerge.=TestMerge
# dataTest.=NULL
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

# Validation Data ----
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
# SaveModelObjects.=SaveModelObjects
# metadata_path.=metadata_path
# model_path.=model_path
# ModelID.=ModelID
# LossFunction.=LossFunction
# TransformNumericColumns.=TransformNumericColumns
# GridTune.=GridTune
# TransformationResults.=TransformationResults
# TargetLevels.=NULL

# Regression metrics ----
# SaveModelObjects.=SaveModelObjects
# data.=data
# ValidationData.=ValidationData
# TestData.=TestData
# TrainOnFull.=TrainOnFull
# LossFunction.=LossFunction
# EvalMetric.=EvalMetric
# TargetColumnName.=TargetColumnName
# ModelID.=ModelID
# model_path.=model_path
# metadata_path.=metadata_path

# ML Eval Plots TrainData ----
# ModelType="regression"
# TrainOnFull.=TrainOnFull
# LossFunction.=LossFunction
# EvalMetric.=EvalMetric
# EvaluationMetrics.=EvalMetricsList
# ValidationData.=TrainData
# NumOfParDepPlots.=NumOfParDepPlots
# VariableImportance.=VariableImportance
# TargetColumnName.=TargetColumnName
# FeatureColNames.=FeatureColNames
# SaveModelObjects.=SaveModelObjects
# ModelID.=ModelID
# metadata_path.=metadata_path
# model_path.=model_path
# predict.=NULL

# ML Eval Plots Validation ----
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

# Score regression in carma ----
# TargetType = "multiregression"
# ScoringData = Step1SCore
# FeatureColumnNames = ModelFeatures
# ReturnShapValues = FALSE
# FactorLevelsList = TestModel$FactorLevelsList
# IDcols = IDcols
# OneHot = FALSE
# ModelObject = Model
# ModelPath = getwd()
# ModelID = "ModelTest"
# ReturnFeatures = TRUE
# TransformNumeric = FALSE
# BackTransNumeric = FALSE
# TransformationObject = NULL
# TransID = NULL
# TransPath = NULL
# TargetColumnName = TargetColumnName
# MDP_Impute = TRUE
# MDP_CharToFactor = TRUE
# MDP_RemoveDates = TRUE
# MDP_MissFactor = "0"
# MDP_MissNum = -1

# CarmaReturnDataPrep ----
# UpdateData.=UpdateData
# FutureDateData.=FutureDateData
# dataStart.=dataStart
# DateColumnName.=DateColumnName
# TargetColumnName.=TargetColumnName
# GroupVariables.=GroupVariables
# Difference.=Difference
# TargetTransformation.=TargetTransformation
# TransformObject.=TransformObject
# NonNegativePred.=NonNegativePred


# ----

# ----
