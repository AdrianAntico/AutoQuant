# Collection data.table
QA_Results <- data.table::CJ(
  Group = c(0,1,2,3),
  xregs = c(0,1,2,3),
  TOF = c(TRUE, FALSE),
  Trans = c(TRUE, FALSE))

# Other tests
QA_Results[, TimeWeights := data.table::fifelse(runif(.N) < 0.5, 0.9999, 1)]
QA_Results[, Success := "Failure"]

# run = 1
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
  TestModel <- tryCatch({RemixAutoML::AutoLightGBMHurdleCARMA(

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

    # Core parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
    input_model = list('classifier' = NULL, 'regression' = NULL),
    task = list('classifier' = 'train', 'regression' = 'train'),
    device_type = list('classifier' = 'CPU', 'regression' = 'CPU'),
    objective = list('classifier' = 'binary', 'regression' = 'regression'),
    metric = list('classifier' = 'binary_logloss', 'regression' = 'rmse'),
    boosting = list('classifier' = 'gbdt', 'regression' = 'gbdt'),
    LinearTree = list('classifier' = FALSE, 'regression' = FALSE),
    Trees = list('classifier' = 1000L, 'regression' = 1000L),
    eta = list('classifier' = NULL, 'regression' = NULL),
    num_leaves = list('classifier' = 31, 'regression' = 31),
    deterministic = list('classifier' = TRUE, 'regression' = TRUE),

    # Learning Parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
    force_col_wise = list('classifier' = FALSE, 'regression' = FALSE),
    force_row_wise = list('classifier' = FALSE, 'regression' = FALSE),
    max_depth = list('classifier' = NULL, 'regression' = NULL),
    min_data_in_leaf = list('classifier' = 20, 'regression' = 20),
    min_sum_hessian_in_leaf = list('classifier' = 0.001, 'regression' = 0.001),
    bagging_freq = list('classifier' = 0, 'regression' = 0),
    bagging_fraction = list('classifier' = 1.0, 'regression' = 1.0),
    feature_fraction = list('classifier' = 1.0, 'regression' = 1.0),
    feature_fraction_bynode = list('classifier' = 1.0, 'regression' = 1.0),
    extra_trees = list('classifier' = FALSE, 'regression' = FALSE),
    early_stopping_round = list('classifier' = 10, 'regression' = 10),
    first_metric_only = list('classifier' = TRUE, 'regression' = TRUE),
    max_delta_step = list('classifier' = 0.0, 'regression' = 0.0),
    lambda_l1 = list('classifier' = 0.0, 'regression' = 0.0),
    lambda_l2 = list('classifier' = 0.0, 'regression' = 0.0),
    linear_lambda = list('classifier' = 0.0, 'regression' = 0.0),
    min_gain_to_split = list('classifier' = 0, 'regression' = 0),
    drop_rate_dart = list('classifier' = 0.10, 'regression' = 0.10),
    max_drop_dart = list('classifier' = 50, 'regression' = 50),
    skip_drop_dart = list('classifier' = 0.50, 'regression' = 0.50),
    uniform_drop_dart = list('classifier' = FALSE, 'regression' = FALSE),
    top_rate_goss = list('classifier' = FALSE, 'regression' = FALSE),
    other_rate_goss = list('classifier' = FALSE, 'regression' = FALSE),
    monotone_constraints = list('classifier' = NULL, 'regression' = NULL),
    monotone_constraints_method = list('classifier' = 'advanced', 'regression' = 'advanced'),
    monotone_penalty = list('classifier' = 0.0, 'regression' = 0.0),
    forcedsplits_filename = list('classifier' = NULL, 'regression' = NULL),
    refit_decay_rate = list('classifier' = 0.90, 'regression' = 0.90),
    path_smooth = list('classifier' = 0.0, 'regression' = 0.0),

    # IO Dataset Parameters
    max_bin = list('classifier' = 255, 'regression' = 255),
    min_data_in_bin = list('classifier' = 3, 'regression' = 3),
    data_random_seed = list('classifier' = 1, 'regression' = 1),
    is_enable_sparse = list('classifier' = TRUE, 'regression' = TRUE),
    enable_bundle = list('classifier' = TRUE, 'regression' = TRUE),
    use_missing = list('classifier' = TRUE, 'regression' = TRUE),
    zero_as_missing = list('classifier' = FALSE, 'regression' = FALSE),
    two_round = list('classifier' = FALSE, 'regression' = FALSE),

    # Convert Parameters
    convert_model = list('classifier' = NULL, 'regression' = NULL),
    convert_model_language = list('classifier' = "cpp", 'regression' = "cpp"),

    # Objective Parameters
    boost_from_average = list('classifier' = TRUE, 'regression' = TRUE),
    is_unbalance = list('classifier' = FALSE, 'regression' = FALSE),
    scale_pos_weight = list('classifier' = 1.0, 'regression' = 1.0),

    # Metric Parameters (metric is in Core)
    is_provide_training_metric = list('classifier' = TRUE, 'regression' = TRUE),
    eval_at = list('classifier' = c(1,2,3,4,5), 'regression' = c(1,2,3,4,5)),

    # Network Parameters
    num_machines = list('classifier' = 1, 'regression' = 1),

    # GPU Parameters
    gpu_platform_id = list('classifier' = -1, 'regression' = -1),
    gpu_device_id = list('classifier' = -1, 'regression' = -1),
    gpu_use_dp = list('classifier' = TRUE, 'regression' = TRUE),
    num_gpu = list('classifier' = 1, 'regression' = 1))}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) QA_Results[run, Success := "Success"]
  TestModel <- NULL
  data.table::fwrite(QA_Results, file = "C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data/AutoLightGBMHurdleCARMA_QA.csv")
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
# run = 1
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
# EncodingMethod = 'credibility'
# TimeWeights = weights
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
# MaxRunsWithoutNewWinner = 50
# MaxRunMinutes = 60*60
# input_model = list('classifier' = NULL, 'regression' = NULL)
# task = list('classifier' = 'train', 'regression' = 'train')
# device_type = list('classifier' = 'CPU', 'regression' = 'CPU')
# objective = list('classifier' = 'binary', 'regression' = 'regression')
# metric = list('classifier' = 'binary_logloss', 'regression' = 'rmse')
# boosting = list('classifier' = 'gbdt', 'regression' = 'gbdt')
# LinearTree = list('classifier' = FALSE, 'regression' = FALSE)
# Trees = list('classifier' = 1000L, 'regression' = 1000L)
# eta = list('classifier' = NULL, 'regression' = NULL)
# num_leaves = list('classifier' = 31, 'regression' = 31)
# deterministic = list('classifier' = TRUE, 'regression' = TRUE)
# force_col_wise = list('classifier' = FALSE, 'regression' = FALSE)
# force_row_wise = list('classifier' = FALSE, 'regression' = FALSE)
# max_depth = list('classifier' = NULL, 'regression' = NULL)
# min_data_in_leaf = list('classifier' = 20, 'regression' = 20)
# min_sum_hessian_in_leaf = list('classifier' = 0.001, 'regression' = 0.001)
# bagging_freq = list('classifier' = 0, 'regression' = 0)
# bagging_fraction = list('classifier' = 1.0, 'regression' = 1.0)
# feature_fraction = list('classifier' = 1.0, 'regression' = 1.0)
# feature_fraction_bynode = list('classifier' = 1.0, 'regression' = 1.0)
# extra_trees = list('classifier' = FALSE, 'regression' = FALSE)
# early_stopping_round = list('classifier' = 10, 'regression' = 10)
# first_metric_only = list('classifier' = TRUE, 'regression' = TRUE)
# max_delta_step = list('classifier' = 0.0, 'regression' = 0.0)
# lambda_l1 = list('classifier' = 0.0, 'regression' = 0.0)
# lambda_l2 = list('classifier' = 0.0, 'regression' = 0.0)
# linear_lambda = list('classifier' = 0.0, 'regression' = 0.0)
# min_gain_to_split = list('classifier' = 0, 'regression' = 0)
# drop_rate_dart = list('classifier' = 0.10, 'regression' = 0.10)
# max_drop_dart = list('classifier' = 50, 'regression' = 50)
# skip_drop_dart = list('classifier' = 0.50, 'regression' = 0.50)
# uniform_drop_dart = list('classifier' = FALSE, 'regression' = FALSE)
# top_rate_goss = list('classifier' = FALSE, 'regression' = FALSE)
# other_rate_goss = list('classifier' = FALSE, 'regression' = FALSE)
# monotone_constraints = list('classifier' = NULL, 'regression' = NULL)
# monotone_constraints_method = list('classifier' = 'advanced', 'regression' = 'advanced')
# monotone_penalty = list('classifier' = 0.0, 'regression' = 0.0)
# forcedsplits_filename = list('classifier' = NULL, 'regression' = NULL)
# refit_decay_rate = list('classifier' = 0.90, 'regression' = 0.90)
# path_smooth = list('classifier' = 0.0, 'regression' = 0.0)
# max_bin = list('classifier' = 255, 'regression' = 255)
# min_data_in_bin = list('classifier' = 3, 'regression' = 3)
# data_random_seed = list('classifier' = 1, 'regression' = 1)
# is_enable_sparse = list('classifier' = TRUE, 'regression' = TRUE)
# enable_bundle = list('classifier' = TRUE, 'regression' = TRUE)
# use_missing = list('classifier' = TRUE, 'regression' = TRUE)
# zero_as_missing = list('classifier' = FALSE, 'regression' = FALSE)
# two_round = list('classifier' = FALSE, 'regression' = FALSE)
# convert_model = list('classifier' = NULL, 'regression' = NULL)
# convert_model_language = list('classifier' = "cpp", 'regression' = "cpp")
# boost_from_average = list('classifier' = TRUE, 'regression' = TRUE)
# is_unbalance = list('classifier' = FALSE, 'regression' = FALSE)
# scale_pos_weight = list('classifier' = 1.0, 'regression' = 1.0)
# is_provide_training_metric = list('classifier' = TRUE, 'regression' = TRUE)
# eval_at = list('classifier' = c(1,2,3,4,5), 'regression' = c(1,2,3,4,5))
# num_machines = list('classifier' = 1, 'regression' = 1)
# gpu_platform_id = list('classifier' = -1, 'regression' = -1)
# gpu_device_id = list('classifier' = -1, 'regression' = -1)
# gpu_use_dp = list('classifier' = TRUE, 'regression' = TRUE)
# num_gpu = list('classifier' = 1, 'regression' = 1)

# Hurdle Model ----
# ModelID = "ModelTest"
# NThreads = parallel::detectCores() - 2
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
# TreeMethod = TreeMethod
# Trees = Trees
# eta = eta
# max_depth = max_depth
# min_child_weight = min_child_weight
# subsample = subsample
# colsample_bytree = colsample_bytree

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
