# Testing
LightGBM_QA_Results_Classifier <- data.table::CJ(
  TOF = c(TRUE,FALSE),
  GridTune = c(TRUE,FALSE),
  Success = "Failure",
  PartitionInFunction = c(TRUE,FALSE)
)

# Remove impossible combinations
LightGBM_QA_Results_Classifier <- LightGBM_QA_Results_Classifier[!(TOF & GridTune)]
LightGBM_QA_Results_Classifier <- LightGBM_QA_Results_Classifier[!(PartitionInFunction & TOF)]
LightGBM_QA_Results_Classifier[, RunNumber := seq_len(.N)]
LightGBM_QA_Results_Classifier[, Score := "Failure"]

#      TOF GridTune Success PartitionInFunction
# 1: FALSE    FALSE Failure               FALSE
# 2: FALSE    FALSE Failure                TRUE
# 3: FALSE     TRUE Failure               FALSE
# 4: FALSE     TRUE Failure                TRUE
# 5:  TRUE    FALSE Failure               FALSE

# AutoLightGBMClassifier
# run = 1
# run = 2
# run = 3
for(run in seq_len(LightGBM_QA_Results_Classifier[,.N])) {

  # Define values
  tof <- LightGBM_QA_Results_Classifier[run, TOF]
  PartitionInFunction <- LightGBM_QA_Results_Classifier[run, PartitionInFunction]
  gridtune <- LightGBM_QA_Results_Classifier[run, GridTune]
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
  TestModel <- tryCatch({RemixAutoML::AutoLightGBMClassifier(

    # GPU or CPU
    NThreads = parallel::detectCores(),

    # Metadata args
    OutputSelection = c("Importances","EvalPlots","EvalMetrics","Score_TrainData"),
    model_path = normalizePath("./"),
    metadata_path = NULL,
    ModelID = "Test_Model_1",
    NumOfParDepPlots = 3L,
    EncodingMethod = "credibility",
    ReturnFactorLevels = TRUE,
    ReturnModelObjects = TRUE,
    SaveModelObjects = TRUE,
    SaveInfoToPDF = FALSE,
    DebugMode = TRUE,

    # Data args
    data = TTrainData,
    TrainOnFull = tof,
    ValidationData = VValidationData,
    TestData = TTestData,
    TargetColumnName = "Adrian",
    FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime","Adrian")],
    PrimaryDateColumn = NULL,
    WeightsColumnName = "Weights",
    IDcols = c("IDcol_1","IDcol_2","DateTime"),
    CostMatrixWeights = c(1,0,0,1),

    # Grid parameters
    GridTune = gridtune,
    grid_eval_metric = "Utility",
    BaselineComparison = "default",
    MaxModelsInGrid = 10L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    PassInGrid = NULL,

    # Core parameters
    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
    input_model = NULL, # continue training a model that is stored to file
    task = "train",
    device_type = "CPU",
    objective = 'binary',
    metric = 'binary_logloss',
    boosting = "gbdt",
    LinearTree = FALSE,
    Trees = if(!gridtune) 50L else c(50,60,70),
    eta = if(!gridtune) NULL else c(.1, .2, .3),
    num_leaves = if(!gridtune) 31 else c(21,31,41),
    deterministic = TRUE,

    # Learning Parameters
    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
    force_col_wise = FALSE,
    force_row_wise = FALSE,
    max_depth = if(!gridtune) 6 else c(6,7,8),
    min_data_in_leaf = if(!gridtune) 20 else c(10,20,30),
    min_sum_hessian_in_leaf = 0.001,
    bagging_freq = if(!gridtune) 1.0 else c(1,2,3),
    bagging_fraction = if(!gridtune) 1.0 else c(1,0.9,0.8),
    feature_fraction = if(!gridtune) 1.0 else c(1,0.9,0.8),
    feature_fraction_bynode = if(!gridtune) 1.0 else c(1,0.9,0.8),
    lambda_l1 = if(!gridtune) 0.0 else c(0,1,2),
    lambda_l2 = if(!gridtune) 0.0 else c(0,1,2),
    extra_trees = FALSE,
    early_stopping_round = 10,
    first_metric_only = TRUE,
    max_delta_step = 0.0,
    linear_lambda = 0.0,
    min_gain_to_split = 0,
    drop_rate_dart = 0.10,
    max_drop_dart = 50,
    skip_drop_dart = 0.50,
    uniform_drop_dart = FALSE,
    top_rate_goss = FALSE,
    other_rate_goss = FALSE,
    monotone_constraints = NULL,
    monotone_constraints_method = 'advanced',
    monotone_penalty = 0.0,
    forcedsplits_filename = NULL, # use for AutoStack option; .json file
    refit_decay_rate = 0.90,
    path_smooth = 0.0,

    # IO Dataset Parameters
    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
    max_bin = 255,
    min_data_in_bin = 3,
    data_random_seed = 1,
    is_enable_sparse = TRUE,
    enable_bundle = TRUE,
    use_missing = TRUE,
    zero_as_missing = FALSE,
    two_round = FALSE,

    # Convert Parameters
    convert_model = NULL,
    convert_model_language = "cpp",

    # Objective Parameters
    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
    boost_from_average = TRUE,
    is_unbalance = FALSE,
    scale_pos_weight = 1.0,

    # Metric Parameters (metric is in Core)
    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
    is_provide_training_metric = TRUE,
    eval_at = c(1,2,3,4,5),

    # Network Parameters
    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
    num_machines = 1,

    # GPU Parameters
    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
    gpu_platform_id = -1,
    gpu_device_id = -1,
    gpu_use_dp = TRUE,
    num_gpu = 1)}, error = function(x) NULL)

  # Outcome
  if(!is.null(TestModel)) LightGBM_QA_Results_Classifier[run, Success := "Success"]
  if(!is.null(TestModel)) {
    ModelID = "Test_Model_1"
    colnames <- data.table::fread(file = file.path(getwd(), paste0(ModelID, "_ColNames.csv")))
    Preds <- tryCatch({RemixAutoML::AutoLightGBMScoring(
      TargetType = "classification",
      ScoringData = if(!tof && !PartitionInFunction) TTestData else TTrainData,
      ReturnShapValues = FALSE,
      FeatureColumnNames = colnames[[1L]],
      IDcols = c("IDcol_1","IDcol_2","DateTime"),
      EncodingMethod = "credibility",
      FactorLevelsList = NULL,
      TargetLevels = NULL,
      ModelObject = NULL,
      ModelPath = getwd(),
      ModelID = "Test_Model_1",
      ReturnFeatures = TRUE,
      TransformNumeric = FALSE,
      BackTransNumeric = FALSE,
      TargetColumnName = NULL,
      TransformationObject = NULL,
      TransID = NULL,
      TransPath = NULL,
      MDP_Impute = TRUE,
      MDP_CharToFactor = TRUE,
      MDP_RemoveDates = TRUE,
      MDP_MissFactor = "0",
      MDP_MissNum = -1)}, error = function(x) NULL)
    if(!is.null(Preds)) LightGBM_QA_Results_Classifier[run, Score := "Success"]
  }
  TestModel <- NULL
  Sys.sleep(5)
  data.table::fwrite(LightGBM_QA_Results_Classifier, file = "C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV/AutoLightGBMClassifier_QA.csv")
}

# Main Code ----
# library(RemixAutoML)
# library(data.table)
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/GridTuning.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/LightGBMHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/XGBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))
# source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/FeatureEngineering_CharacterTypes.R"))

# Scoring function ----
# TargetType = "classification"
# ScoringData = if(!tof && !PartitionInFunction) TTestData else TTrainData
# ReturnShapValues = FALSE
# FeatureColumnNames = colnames[[1L]]
# IDcols = c("IDcol_1","IDcol_2")
# EncodingMethod = "credibility"
# FactorLevelsList = NULL
# TargetLevels = NULL
# ModelObject = NULL
# ModelPath = getwd()
# ModelID = "Test_Model_1"
# ReturnFeatures = TRUE
# TransformNumeric = FALSE
# BackTransNumeric = FALSE
# TargetColumnName = NULL
# TransformationObject = NULL
# TransID = NULL
# TransPath = NULL
# MDP_Impute = TRUE
# MDP_CharToFactor = TRUE
# MDP_RemoveDates = TRUE
# MDP_MissFactor = "0"
# MDP_MissNum = -1

# Main Args ----
# options(scipen = 9999)
#
# run = 1
#
# # Define values
# tof <- LightGBM_QA_Results_Classifier[run, TOF]
# gridtune <- LightGBM_QA_Results_Classifier[run, GridTune]
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
# # Main Function Defaults
# TrainOnFull = tof
# data = TTrainData
# ValidationData = VValidationData
# TestData = TTestData
# TargetColumnName = Tar
# FeatureColNames = names(TTrainData)[!names(TTrainData) %in% c("IDcol_1", "IDcol_2","DateTime",Tar)]
# PrimaryDateColumn = "DateTime"
# WeightsColumnName = "Weights"
# IDcols = c("IDcol_1","IDcol_2","DateTime")
# CostMatrixWeights = c(1,0,0,1)
# # Metadata parameters
# OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData")
# model_path = NULL
# metadata_path = NULL
# DebugMode = TRUE
# SaveInfoToPDF = FALSE
# ModelID = "FirstModel"
# ReturnFactorLevels = TRUE
# ReturnModelObjects = TRUE
# SaveModelObjects = FALSE
# EncodingMethod = "credibility"
# Verbose = 0L
# NumOfParDepPlots = 3L
# # Grid parameters
# GridTune = gridtune
# grid_eval_metric = "Utility"
# BaselineComparison = "default"
# MaxModelsInGrid = 10L
# MaxRunsWithoutNewWinner = 20L
# MaxRunMinutes = 24L*60L
# PassInGrid = NULL
# # High Level Parameters
# input_model = NULL # continue training a model that is stored to file
# # Core parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
# task = "train"
# device_type = "CPU"
# NThreads = parallel::detectCores() / 2
# objective = 'binary'
# metric = "binary_logloss"
# boosting = "gbdt"
# LinearTree = FALSE
# Trees = if(!gridtune) 50L else c(50L,60L,70L)
# eta = if(!gridtune) NULL else c(.1, .2, .3)
# num_leaves = if(!gridtune) 31 else c(21,31,41)
# deterministic = TRUE
# # Learning Parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
# force_col_wise = FALSE
# force_row_wise = FALSE
# max_depth = if(!gridtune) 6 else c(6,7,8)
# min_data_in_leaf = if(!gridtune) 20 else c(10,20,30)
# min_sum_hessian_in_leaf = 0.001
# bagging_freq = if(!gridtune) 1.0 else c(1,2,3)
# bagging_fraction = if(!gridtune) 1.0 else c(1,0.9,0.8)
# feature_fraction = if(!gridtune) 1.0 else c(1,0.9,0.8)
# feature_fraction_bynode = if(!gridtune) 1.0 else c(1,0.9,0.8)
# extra_trees = FALSE
# early_stopping_round = 10
# first_metric_only = TRUE
# max_delta_step = 0.0
# lambda_l1 = if(!gridtune) 0.0 else c(0,1,2)
# lambda_l2 = if(!gridtune) 0.0 else c(0,1,2)
# linear_lambda = 0.0
# min_gain_to_split = 0
# drop_rate_dart = 0.10
# max_drop_dart = 50
# skip_drop_dart = 0.50
# uniform_drop_dart = FALSE
# top_rate_goss = FALSE
# other_rate_goss = FALSE
# monotone_constraints = NULL
# monotone_constraints_method = "advanced"
# monotone_penalty = 0.0
# forcedsplits_filename = NULL # use for AutoStack option; .json file
# refit_decay_rate = 0.90
# path_smooth = 0.0
# # IO Dataset Parameters
# max_bin = 255
# min_data_in_bin = 3
# data_random_seed = 1
# is_enable_sparse = TRUE
# enable_bundle = TRUE
# use_missing = TRUE
# zero_as_missing = FALSE
# two_round = FALSE
# # Convert Parameters
# convert_model = NULL #"gbdt_prediction.cpp"
# convert_model_language = "cpp"
# # Objective Parameters
# boost_from_average = TRUE
# is_unbalance = FALSE
# scale_pos_weight = 1.0
# # Metric Parameters (metric is in Core)
# is_provide_training_metric = TRUE
# eval_at = c(1,2,3,4,5)
# # Network Parameters
# num_machines = 1
# # GPU Parameters
# gpu_platform_id = -1
# gpu_device_id = -1
# gpu_use_dp = TRUE
# num_gpu = 1

# Data Prep ----
# Algo = "lightgbm"
# ModelType="classification"
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
# SaveModelObjects.=SaveModelObjects
# metadata_path.=metadata_path
# model_path.=model_path
# ModelID.=ModelID
# LossFunction.=NULL
# TransformNumericColumns.=NULL
# GridTune.=GridTune
# TransformationResults.=TransformationResults
# TargetLevels.=NULL

# Validation Data ----
# ModelType="classification"
# TestDataCheck=!is.null(TestData)
# TrainOnFull.=TrainOnFull
# model.=model
# TargetColumnName.=TargetColumnName
# SaveModelObjects.=SaveModelObjects
# metadata_path.=metadata_path
# model_path.=model_path
# ModelID.=ModelID
# TestData.=TestData
# TestTarget.=TestTarget
# FinalTestTarget.=FinalTestTarget
# TestMerge.=TestMerge
# dataTest.=dataTest
# TrainTarget.=TrainTarget
# predict.=predict
# TransformNumericColumns.=TransformNumericColumns
# TransformationResults.=TransformationResults
# GridTune.=GridTune
# data.=dataTrain

# Grid Tuning ----
# ModelType="classification"
# TrainOnFull.=TrainOnFull
# DebugMode.=DebugMode
# params.=params
# num_iterations.=params$num_iterations
# max_depth.=params$max_depth
# eta.=params$eta
# num_leaves.=params$num_leaves
# min_data_in_leaf.=params$min_data_in_leaf
# bagging_freq.=params$bagging_freq
# bagging_fraction.=params$bagging_fraction
# feature_fraction.=params$feature_fraction
# feature_fraction_bynode.=params$feature_fraction_bynode
# lambda_l1.=params$lambda_l1
# lambda_l2.=params$lambda_l2
# LossFunction=NULL
# EvalMetric=NULL
# grid_eval_metric.=grid_eval_metric
# CostMatrixWeights=NULL
# TargetColumnName.=TargetColumnName
# dataTrain.=dataTrain
# dataTest.=dataTest
# TestData.=TestData
# EvalSets.=EvalSets
# TestTarget.=TestTarget
# FinalTestTarget.=FinalTestTarget
# TargetLevels.=TargetLevels
# MaxRunsWithoutNewWinner=MaxRunsWithoutNewWinner
# MaxModelsInGrid=MaxModelsInGrid
# MaxRunMinutes=MaxRunMinutes
# BaselineComparison.=BaselineComparison
# SaveModelObjects=SaveModelObjects
# metadata_path=metadata_path
# model_path=model_path
# ModelID=ModelID
# NumLevels.=NULL

# Final Params ----
# params.=params
# GridTune.=GridTune
# PassInGrid.=PassInGrid
# TrainOnFull.=TrainOnFull
# BestGrid.=BestGrid
# Trees.=params[["Trees"]]
# eta.=params[["eta"]]
# num_leaves.=params[["num_leaves"]]
# max_depth.=params[["max_depth"]]
# min_data_in_leaf.=params[["min_data_in_leaf"]]
# bagging_freq.=params[["bagging_freq"]]
# bagging_fraction.=params[["bagging_fraction"]]
# feature_fraction.=params[["feature_fraction"]]
# feature_fraction_bynode.=params[["feature_fraction_bynode"]]
