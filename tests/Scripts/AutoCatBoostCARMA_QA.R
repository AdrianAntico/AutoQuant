# Collection data.table
QA_Results <- data.table::CJ(
  Group = c(0,1,2,3),
  xregs = c(0,1,2,3),
  TOF = c(TRUE, FALSE),
  Trans = c(TRUE, FALSE),
  Diff = c(TRUE, FALSE))

# Other tests
QA_Results[, TimeWeights := data.table::fifelse(runif(.N) < 0.5, 0.9999, 1)]
QA_Results[, TaskType := data.table::fifelse(runif(.N) < 0.5, "GPU", "CPU")]
QA_Results[, Success := "Failure"]

# NOT Train On FULL TOF
# run = 1
for(run in seq_len(QA_Results[,.N])) {# seq_len(QA_Results[,.N])) {

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
  Diff <- QA_Results[run, Diff]
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

  # Build forecast
  TestModel <- tryCatch({RemixAutoML::AutoCatBoostCARMA(

    # data args
    data = data1,
    XREGS = xregs1,
    TimeWeights = weights,
    TargetColumnName = "Weekly_Sales",
    DateColumnName = "Date",
    HierarchGroups = NULL,
    GroupVariables = groupvariables,
    TimeUnit = "weeks",
    TimeGroups = c("weeks","months"),

    # Production args
    TrainOnFull = TOF,
    SplitRatios = c(1 - 10 / 100, 10 / 100),
    PartitionType = "random",
    FC_Periods = 33,
    TaskType = tasktype,
    NumGPU = 1,
    Timer = TRUE,
    DebugMode = TRUE,

    # Target variable transformations
    TargetTransformation = Trans,
    Methods = c("Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"),
    Difference = Diff,
    NonNegativePred = FALSE,
    RoundPreds = FALSE,

    # Calendar-related features
    CalendarVariables = c("week","wom","month","quarter"),
    HolidayVariable = c("USPublicHolidays"),
    HolidayLags = c(1,2,3),
    HolidayMovingAverages = c(2,3),

    # Lags, moving averages, and other rolling stats
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

    # ML grid tuning args
    GridTune = FALSE,
    PassInGrid = NULL,
    ModelCount = 5,
    MaxRunsWithoutNewWinner = 50,
    MaxRunMinutes = 60*60,

    # ML evaluation output
    PDFOutputPath = NULL,
    SaveDataPath = NULL,
    NumOfParDepPlots = 0L,

    # ML loss functions
    EvalMetric = "RMSE",
    EvalMetricValue = 1,
    LossFunction = "RMSE",
    LossFunctionValue = 1,

    # ML tuning args
    NTrees = 50L,
    Depth = 6L,
    L2_Leaf_Reg = NULL,
    LearningRate = NULL,
    Langevin = FALSE,
    DiffusionTemperature = 10000,
    RandomStrength = 1,
    BorderCount = 254,
    RSM = NULL,
    GrowPolicy = "SymmetricTree",
    BootStrapType = "Bayesian",
    ModelSizeReg = 0.5,
    FeatureBorderType = "GreedyLogSum",
    SamplingUnit = "Group",
    SubSample = NULL,
    ScoreFunction = "Cosine",
    MinDataInLeaf = 1)}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  rm(TestModel)
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/TestingData/AutoCatBoostCARMA_QA.csv")
  Sys.sleep(5)
}

# Defaults ----

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
# run = 6
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
# Diff <- QA_Results[run, Diff]
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
# data = data1
# XREGS = xregs1
# TimeWeights = weights
# TargetColumnName = "Weekly_Sales"
# DateColumnName = "Date"
# HierarchGroups = NULL
# GroupVariables = groupvariables
# TimeUnit = "weeks"
# TimeGroups = c("weeks","months")
# TrainOnFull = TOF
# SplitRatios = c(1 - 10 / 100, 10 / 100)
# PartitionType = "random"
# FC_Periods = 10
# TaskType = tasktype
# NumGPU = 1
# Timer = TRUE
# DebugMode = TRUE
# TargetTransformation = Trans
# Methods = c("Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit")
# Difference = Diff
# NonNegativePred = TRUE
# RoundPreds = FALSE
# CalendarVariables = c("week","wom","month","quarter")
# HolidayVariable = c("USPublicHolidays")
# HolidayLookback = 7
# HolidayLags = c(1,2,3)
# HolidayMovingAverages = c(2,3)
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
# GridTune = FALSE
# PassInGrid = NULL
# ModelCount = 5
# MaxRunsWithoutNewWinner = 50
# MaxRunMinutes = 60*60
# PDFOutputPath = NULL
# SaveDataPath = NULL
# NumOfParDepPlots = 0L
# EvalMetric = "RMSE"
# EvalMetricValue = 1
# LossFunction = "RMSE"
# LossFunctionValue = 1
# NTrees = 50L
# Depth = 6L
# L2_Leaf_Reg = NULL
# LearningRate = NULL
# Langevin = FALSE
# DiffusionTemperature = 10000
# RandomStrength = 1
# BorderCount = 254
# RSM = NULL
# GrowPolicy = "SymmetricTree"
# BootStrapType = "Bayesian"
# ModelSizeReg = 0.5
# FeatureBorderType = "GreedyLogSum"
# SamplingUnit = "Group"
# SubSample = NULL
# ScoreFunction = "Cosine"
# MinDataInLeaf = 1

# CarmaTimeSeriesFeatures ----
# data.=data
# TargetColumnName.=TargetColumnName
# DateColumnName.=DateColumnName
# GroupVariables.=GroupVariables
# HierarchGroups.=HierarchGroups
# Difference.=Difference
# TimeGroups.=TimeGroups
# TimeUnit.=TimeUnit
# Lags.=Lags
# MA_Periods.=MA_Periods
# SD_Periods.=SD_Periods
# Skew_Periods.=Skew_Periods
# Kurt_Periods.=Kurt_Periods
# Quantile_Periods.=Quantile_Periods
# Quantiles_Selected.=Quantiles_Selected
# HolidayVariable.=HolidayVariable
# HolidayLags.=HolidayLags
# HolidayMovingAverages.=HolidayMovingAverages
# DebugMode.=DebugMode
#
# Update time series features ----
# ModelType="catboost"
# DebugMode.=DebugMode
# UpdateData.=UpdateData
# GroupVariables.=GroupVariables
# Difference.=Difference
# CalendarVariables.=CalendarVariables
# HolidayVariable.=HolidayVariable
# IndepVarPassTRUE.=IndepentVariablesPass
# data.=data
# CalendarFeatures.=CalendarFeatures
# XREGS.=XREGS
# HierarchGroups.=HierarchGroups
# GroupVarVector.=GroupVarVector
# TargetColumnName.=TargetColumnName
# DateColumnName.=DateColumnName
# Preds.=Preds
# HierarchSupplyValue.=HierarchSupplyValue
# IndependentSupplyValue.=IndependentSupplyValue
# TimeUnit.=TimeUnit
# TimeGroups.=TimeGroups
# Lags.=Lags
# MA_Periods.=MA_Periods
# SD_Periods.=SD_Periods
# Skew_Periods.=Skew_Periods
# Kurt_Periods.=Kurt_Periods
# Quantile_Periods.=Quantile_Periods
# Quantiles_Selected.=Quantiles_Selected
# HolidayLags.=HolidayLags
# HolidayMovingAverages.=HolidayMovingAverages
#
# Data ----
# data                 = Temporary
# RowNumsID            = "ID"
# RowNumsKeep          = 1
# DateColumn           = eval(DateColumnName.)
# Targets              = eval(TargetColumnName.)
# HierarchyGroups      = NULL
# IndependentGroups    = NULL
# # Service
# TimeBetween          = NULL
# TimeUnit             = TimeUnit.
# TimeUnitAgg          = TimeGroups.[1]
# TimeGroups           = TimeGroups.
# RollOnLag1           = TRUE
# Type                 = "Lag"
# SimpleImpute         = TRUE
# # Calculated Column
# Lags                 = Lags.
# MA_RollWindows       = MA_Periods.
# SD_RollWindows       = SD_Periods.
# Skew_RollWindows     = Skew_Periods.
# Kurt_RollWindows     = Kurt_Periods.
# Quantile_RollWindows = Quantile_Periods.
# Quantiles_Selected   = Quantiles_Selected.
# Debug                = DebugMode.
#
# Keep GDL Features ----
# IndepVarPassTRUE = NULL
# data.
# UpdateData.
# CalendarFeatures.
# XREGS.
# Difference.
# HierarchGroups.
# GroupVariables.,
# GroupVarVector.
# CalendarVariables=CalVar
# HolidayVariable=HolVar
# TargetColumnName.,DateColumnName.
# Preds.

# AutoCatBoostRegression Args ----
# task_type = TaskType
# NumGPUs = NumGPU
# OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData")
# # Metadata argument
# ModelID = "CatBoost"
# model_path = getwd()
# metadata_path = if(!is.null(PDFOutputPath)) PDFOutputPath else getwd()
# SaveModelObjects = FALSE
# ReturnModelObjects = TRUE
# SaveInfoToPDF = if(!is.null(PDFOutputPath)) TRUE else FALSE
# # Data argument
# data = train
# TrainOnFull = TOF
# ValidationData = valid
# TestData = test
# TargetColumnName = TargetVariable
# FeatureColNames = ModelFeatures
# PrimaryDateColumn = eval(DateColumnName)
# WeightsColumnName = if("Weights" %in% names(train)) "Weights" else NULL
# IDcols = IDcols
# TransformNumericColumns = NULL
# Methods = NULL
# # Model evaluatio
# eval_metric = EvalMetric
# eval_metric_value = EvalMetricValue
# loss_function = LossFunction
# loss_function_value = LossFunctionValue
# MetricPeriods = 10L
# NumOfParDepPlots = NumOfParDepPlots
# # Grid tuning argument
# PassInGrid = PassInGrid
# GridTune = GridTune
# MaxModelsInGrid = ModelCount
# MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
# MaxRunMinutes = 60*60
# BaselineComparison = "default"
# # ML arg
# langevin = Langevin
# diffusion_temperature = DiffusionTemperature
# Trees = NTrees
# Depth = Depth
# LearningRate = LearningRate
# L2_Leaf_Reg = L2_Leaf_Reg
# RandomStrength = RandomStrength
# BorderCount = BorderCount
# RSM = if(TaskType == "GPU") NULL else RSM
# BootStrapType = BootStrapType
# GrowPolicy = GrowPolicy
# # New ML arg
# model_size_reg = ModelSizeReg
# feature_border_type = FeatureBorderType
# sampling_unit = SamplingUnit
# subsample = SubSample
# score_function = ScoreFunction
# min_data_in_leaf = MinDataInLeaf
# DebugMode = DebugMode

# Carma score ----
# i.=i
# N.=N
# GroupVariables.=GroupVariables
# ModelFeatures.=ModelFeatures
# HierarchGroups.=HierarchGroups
# DateColumnName.=DateColumnName
# Difference.=Difference
# TargetColumnName.=TargetColumnName
# Step1SCore.=Step1SCore
# Model.=Model
# FutureDateData.=FutureDateData
# NonNegativePred.=NonNegativePred
# UpdateData.=UpdateData

# Catboost scoring ----
# TargetType = "regression"
# ScoringData = Step1SCore.
# FeatureColumnNames = ModelFeatures.
# FactorLevelsList = NULL
# IDcols = IDcols
# OneHot = FALSE
# ModelObject = Model.
# ModelPath = getwd()
# ModelID = "ModelTest"
# ReturnFeatures = TRUE
# TransformNumeric = FALSE
# BackTransNumeric = FALSE# data = UpdateData.
# ScoreData = NULL
# CARMA = TRUE
# TargetCol = eval(TargetColumnName.)
# FirstRow = DiffTrainOutput.$FirstRow[[eval(TargetColumnName.)]]
# LastRow = NULL
# GroupVariables = NULL
# TransformationObject = NULL
# TransID = NULL
# TransPath = NULL
# TargetColumnName = NULL
# MDP_Impute = FALSE
# MDP_CharToFactor = FALSE
# MDP_RemoveDates = TRUE
# MDP_MissFactor = "0"
# MDP_MissNum = -1
# ReturnShapValues = FALSE

# Return data ----
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
# DiffTrainOutput.=DiffTrainOutput

# Difference data reverse single series


# ----

# ----
