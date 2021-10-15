# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# ----

#     Rmarkdown Reports QA Testing              ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# ----

#       Results Table Create  ----
RmarkdownQA <- data.table::data.table(
  Algo = rep('not run', 18),
  Type = rep('not run', 18),
  Success = rep('not run', 18))

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# ----

# ----

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# ----

#       REGRESSION MODELS (Runs: 1-6)           ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# ----

# ----

#         CatBoost Regression  ----
# Run = 1 # Pass RemixOutput and fill any gaps per parameter requests
# Run = 2 # Load RemixOutput objects and fill in any gaps per parameter requests
# Run = 3 # User supplied data, fill gaps that can be filled, skip the rest
for(Run in 1:3) {

  Run <- 1

  # Working directory
  setwd("C:/Users/Bizon/Documents/GitHub")

  # Clear output
  if(Run == 1 && file.exists(file.path(getwd(), "ModelInsights-Test_Model_1-regression.html"))) {
    file.remove(File = file.path(getwd(), "ModelInsights-Test_Model_1-regression.html"))
  }

  # Create some dummy correlated data
  data <- RemixAutoML::FakeDataGenerator(
    Correlation = 0.85,
    N = 10000,
    ID = 2,
    ZIP = 0,
    FactorCount = 2,
    AddDate = FALSE,
    Classification = FALSE,
    MultiClass = FALSE)

  # Copy data
  data1 <- data.table::copy(data)

  # Run function
  RemixOutput <- RemixAutoML::AutoCatBoostRegression(

    # GPU or CPU and the number of available GPUs
    TrainOnFull = FALSE,
    task_type = 'GPU',
    NumGPUs = 1,
    DebugMode = FALSE,

    # Metadata args
    OutputSelection = c('Importances','EvalPlots','EvalMetrics','Score_TrainData'),
    ModelID = 'Test_Model_1',
    model_path = getwd(),
    metadata_path = getwd(),
    SaveModelObjects =  if(Run %in% c(1,3)) FALSE else TRUE,
    SaveInfoToPDF = FALSE,
    ReturnModelObjects = if(Run %in% c(1,3)) TRUE else FALSE,

    # Data args
    data = data1,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = 'Adrian',
    FeatureColNames = names(data1)[!names(data1) %in% c('IDcol_1','IDcol_2','Adrian')],
    PrimaryDateColumn = NULL,
    WeightsColumnName = NULL,
    IDcols = c('IDcol_1','IDcol_2'),
    TransformNumericColumns = 'Adrian',
    Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'),

    # Model evaluation
    eval_metric = 'RMSE',
    eval_metric_value = 1.5,
    loss_function = 'RMSE',
    loss_function_value = 1.5,
    MetricPeriods = 10L,
    NumOfParDepPlots = ncol(data1)-1L-2L,

    # Grid tuning args
    PassInGrid = NULL,
    GridTune = FALSE,
    MaxModelsInGrid = 30L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 60*60,
    BaselineComparison = 'default',

    # ML args
    langevin = FALSE,
    diffusion_temperature = 10000,
    Trees = 50,
    Depth = 4,
    L2_Leaf_Reg = NULL,
    RandomStrength = 1,
    BorderCount = 128,
    LearningRate = NULL,
    RSM = 1,
    BootStrapType = NULL,
    GrowPolicy = 'SymmetricTree',
    model_size_reg = 0.5,
    feature_border_type = 'GreedyLogSum',
    sampling_unit = 'Object',
    subsample = NULL,
    score_function = 'Cosine',
    min_data_in_leaf = 1)

  # Create Model Insights Report
  if(Run %in% c(1)) train <- RemixOutput[['TrainData']] else train <- NULL
  if(Run %in% c(1)) valid <- RemixOutput[['TestData']] else valid <- NULL
  if(Run %in% c(1)) test <- RemixOutput[['TestData']] else test <- NULL
  if(Run == 3) rm(RemixOutput)
  tryCatch({

    RemixAutoML::ModelInsightsReport(

      # DataSets (use TestData for ValidationData)
      TrainData = train,
      ValidationData = valid,
      TestData = test,

      # Meta info
      TargetColumnName = 'Adrian',
      PredictionColumnName = 'Predict',
      FeatureColumnNames = names(data1)[!names(data1) %in% c('IDcol_1','IDcol_2','Adrian')],
      DateColumnName = NULL,

      # Control options
      TargetType = 'regression',
      ModelID = 'Test_Model_1',
      Algo = 'catboost',
      SourcePath = getwd(),
      OutputPath = getwd(),
      RemixOutput = if(Run %in% c(2,3)) NULL else RemixOutput)

    }, error = function(x) NULL)

  # Update
  if(file.exists(file.path(getwd(), "ModelInsights-Test_Model_1-regression.html"))) {
    RmarkdownQA[Run, Algo := 'catboost']
    RmarkdownQA[Run, Type := 'regression']
    RmarkdownQA[Run, Success := 'success']
    file.remove(File = file.path(getwd(), "ModelInsights-Test_Model_1-regression.html"))
  } else {
    RmarkdownQA[Run, Algo := 'catboost']
    RmarkdownQA[Run, Type := 'regression']
    RmarkdownQA[Run, Success := 'failure']
  }
}

# # Args ModelInsightsReport
TrainData = train
ValidationData = valid
TestData = test

# Meta info
TargetColumnName = 'Adrian'
PredictionColumnName = 'Predict'
FeatureColumnNames = names(data1)[!names(data1) %in% c('IDcol_1','IDcol_2','Adrian')]
DateColumnName = NULL

# Control options
TargetType = 'regression'
ModelID = 'Test_Model_1'
Algo = 'catboost'
SourcePath = getwd()
OutputPath = getwd()
RemixOutput = if(Run %in% c(1)) RemixOutput else NULL

# ----

#         XGBoost Regression   ----
# Run = 4
# Run = 5
# Run = 6
for(Run in 4:6) {

  # Working directory
  setwd("C:/Users/Bizon/Documents/GitHub")

  # Clear output
  if(Run == 4 && file.exists(file.path(getwd(), "ModelInsights-Test_Model_1-regression.html"))) {
    file.remove(File = file.path(getwd(), "ModelInsights-Test_Model_1-regression.html"))
  }

  # Create some dummy correlated data
  data <- RemixAutoML::FakeDataGenerator(
    Correlation = 0.85,
    N = 10000,
    ID = 2,
    ZIP = 0,
    AddDate = FALSE,
    Classification = FALSE,
    MultiClass = FALSE)

  # Run function
  RemixOutput <- RemixAutoML::AutoXGBoostRegression(

    # GPU or CPU
    TreeMethod = "hist",
    NThreads = parallel::detectCores(),
    LossFunction = 'reg:squarederror',

    # Metadata args
    OutputSelection = c("Importances", "EvalPlots", "EvalMetrics", "Score_TrainData"),
    model_path = normalizePath("./"),
    metadata_path = NULL,
    ModelID = "Test_Model_1",
    EncodingMethod = "binary",
    ReturnFactorLevels = TRUE,
    ReturnModelObjects = if(Run == 4) TRUE else FALSE,
    SaveModelObjects = if(Run == 4) FALSE else TRUE,
    SaveInfoToPDF = FALSE,
    DebugMode = FALSE,

    # Data args
    data = data,
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    PrimaryDateColumn = NULL,
    WeightsColumnName = NULL,
    IDcols = c("IDcol_1","IDcol_2"),
    TransformNumericColumns = NULL,
    Methods = c("Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit"),

    # Model evaluation args
    eval_metric = "rmse",
    NumOfParDepPlots = 3L,

    # Grid tuning args
    PassInGrid = NULL,
    GridTune = FALSE,
    grid_eval_metric = "r2",
    BaselineComparison = "default",
    MaxModelsInGrid = 10L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    Verbose = 1L,

    # ML args
    Trees = 50L,
    eta = 0.05,
    max_depth = 4L,
    min_child_weight = 1.0,
    subsample = 0.55,
    colsample_bytree = 0.55)

  # Create Model Insights Report
  if(Run %in% c(5)) train <- RemixOutput[['TrainData']] else train <- NULL
  if(Run %in% c(4)) valid <- RemixOutput[['TestData']] else valid <- NULL
  if(Run %in% c(3)) test <- RemixOutput[['TestData']] else test <- NULL
  if(Run == 3) rm(RemixOutput)
  tryCatch({

    RemixAutoML::ModelInsightsReport(

      # DataSets (use TestData for ValidationData)
      TrainData = train,
      ValidationData = valid,
      TestData = test,

      # Meta info
      TargetColumnName = 'Adrian',
      PredictionColumnName = 'Predict',
      FeatureColumnNames = names(data)[!names(data) %in% c('IDcol_1','IDcol_2','Adrian')],
      DateColumnName = NULL,

      # Control options
      TargetType = 'regression',
      ModelID = 'Test_Model_1',
      Algo = 'xgboost',
      SourcePath = getwd(),
      OutputPath = getwd(),
      RemixOutput = if(Run %in% c(4)) NULL else RemixOutput)

  }, error = function(x) NULL)

  # Update
  if(file.exists(file.path(getwd(), "ModelInsights-Test_Model_1-regression.html"))) {
    RmarkdownQA[Run, Algo := 'xgboost']
    RmarkdownQA[Run, Type := 'regression']
    RmarkdownQA[Run, Success := 'success']
    file.remove(File = file.path(getwd(), "ModelInsights-Test_Model_1-regression.html"))
  } else {
    RmarkdownQA[Run, Algo := 'xgboost']
    RmarkdownQA[Run, Type := 'regression']
    RmarkdownQA[Run, Success := 'failure']
  }
}

# # Args ModelInsightsReport
TrainData = train
ValidationData = valid
TestData = test

# Meta info
TargetColumnName = 'Adrian'
PredictionColumnName = 'Predict'
FeatureColumnNames = names(data1)[!names(data1) %in% c('IDcol_1','IDcol_2','Adrian')]
DateColumnName = NULL

# Control options
TargetType = 'regression'
ModelID = 'Test_Model_1'
Algo = 'catboost'
SourcePath = getwd()
OutputPath = getwd()
RemixOutput = if(Run %in% c(1)) RemixOutput else NULL



# ----

#         LightGBM Regression  ----
# Run = 5
# Run = 6
for(Run in 5:6) {

  # Working directory
  setwd("C:/Users/Bizon/Documents/GitHub")

  # Clear output
  if(Run == 5 && file.exists(file.path(getwd(), "ModelInsights-Test_Model_1-regression.html"))) {
    file.remove(File = file.path(getwd(), "ModelInsights-Test_Model_1-regression.html"))
  }

  # Create some dummy correlated data
  data <- RemixAutoML::FakeDataGenerator(
    Correlation = 0.85,
    N = 1000,
    ID = 2,
    ZIP = 0,
    AddDate = FALSE,
    Classification = FALSE,
    MultiClass = FALSE)

  # Run function
  RemixOutput <- RemixAutoML::AutoLightGBMRegression(

    # Metadata args
    OutputSelection = c('Importances','EvalPlots','EvalMetrics','Score_TrainData'),
    model_path = getwd(),
    metadata_path = NULL,
    ModelID = 'Test_Model_1',
    NumOfParDepPlots = 3L,
    EncodingMethod = 'credibility',
    ReturnFactorLevels = TRUE,
    ReturnModelObjects = if(Run == 5) TRUE else FALSE,
    SaveModelObjects = if(Run == 5) FALSE else TRUE,
    SaveInfoToPDF = FALSE,
    DebugMode = FALSE,

    # Data args
    data = data,
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = 'Adrian',
    FeatureColNames = names(data)[!names(data) %in% c('IDcol_1', 'IDcol_2','Adrian')],
    PrimaryDateColumn = NULL,
    WeightsColumnName = NULL,
    IDcols = c('IDcol_1','IDcol_2'),
    TransformNumericColumns = NULL,
    Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'),

    # Grid parameters
    GridTune = FALSE,
    grid_eval_metric = 'r2',
    BaselineComparison = 'default',
    MaxModelsInGrid = 10L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    PassInGrid = NULL,

    # Core parameters
    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
    input_model = NULL, # continue training a model that is stored to file
    task = 'train',
    device_type = 'CPU',
    NThreads = parallel::detectCores() / 2,
    objective = 'regression',
    metric = 'rmse',
    boosting = 'gbdt',
    LinearTree = FALSE,
    Trees = 50L,
    eta = NULL,
    num_leaves = 31,
    deterministic = TRUE,

    # Learning Parameters
    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
    force_col_wise = FALSE,
    force_row_wise = FALSE,
    max_depth = NULL,
    min_data_in_leaf = 20,
    min_sum_hessian_in_leaf = 0.001,
    bagging_freq = 0,
    bagging_fraction = 1.0,
    feature_fraction = 1.0,
    feature_fraction_bynode = 1.0,
    extra_trees = FALSE,
    early_stopping_round = 10,
    first_metric_only = TRUE,
    max_delta_step = 0.0,
    lambda_l1 = 0.0,
    lambda_l2 = 0.0,
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
    convert_model_language = 'cpp',

    # Objective Parameters
    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
    boost_from_average = TRUE,
    alpha = 0.90,
    fair_c = 1.0,
    poisson_max_delta_step = 0.70,
    tweedie_variance_power = 1.5,
    lambdarank_truncation_level = 30,

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
    num_gpu = 1)

  # Create Model Insights Report
  tryCatch({RemixAutoML::ModelInsightsReport(
    RemixOutput = if(Run == 5) RemixOutput else NULL,
    OutputPath = getwd(),
    TargetColumnName = 'Adrian',
    TargetType = 'regression',
    ModelID = 'Test_Model_1',
    Algo = 'lightgbm')}, error = NULL)

  # Update
  if(file.exists(file.path(getwd(), "ModelInsights-Test_Model_1-regression.html"))) {
    RmarkdownQA[Run, Algo := 'lightgbm']
    RmarkdownQA[Run, Type := 'regression']
    RmarkdownQA[Run, Success := 'success']
    file.remove(File = file.path(getwd(), "ModelInsights-Test_Model_1-regression.html"))
  } else {
    RmarkdownQA[Run, Algo := 'lightgbm']
    RmarkdownQA[Run, Type := 'regression']
    RmarkdownQA[Run, Success := 'failure']
  }
}

# Args
# RemixOutput = TestModel
# OutputPath = getwd()
# TargetType = 'regression'
# ModelID = 'Test_Model_1'
# Algo = 'xgboost'

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# ----

#       BINARY CLASSIFIER MODELS (Runs: 7-12)   ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# ----

# ----

#         CatBoost Classifier  ----
# Run = 7
# Run = 8
for(Run in 7:8) {

  # Working directory
  setwd("C:/Users/Bizon/Documents/GitHub")

  # Clear output
  if(Run == 7 && file.exists(file.path(getwd(), "ModelInsights-Test_Model_1-classification.html"))) {
    file.remove(File = file.path(getwd(), "ModelInsights-Test_Model_1-classification.html"))
  }

  # Create some dummy correlated data
  data <- RemixAutoML::FakeDataGenerator(
    Correlation = 0.85,
    N = 10000,
    ID = 2,
    ZIP = 0,
    AddDate = FALSE,
    Classification = TRUE,
    MultiClass = FALSE)

  # Copy data
  data1 <- data.table::copy(data)

  # Run function
  RemixOutput <- RemixAutoML::AutoCatBoostClassifier(

    # GPU or CPU and the number of available GPUs
    task_type = 'GPU',
    NumGPUs = 1,
    TrainOnFull = FALSE,
    DebugMode = FALSE,

    # Metadata args
    OutputSelection = c('Score_TrainData', 'Importances', 'EvalPlots', 'EvalMetrics'),
    ModelID = 'Test_Model_1',
    model_path = getwd(),
    metadata_path = getwd(),
    SaveModelObjects = if(Run == 7) FALSE else TRUE,
    ReturnModelObjects = if(Run == 7) TRUE else FALSE,
    SaveInfoToPDF = FALSE,

    # Data args
    data = data1,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = 'Adrian',
    FeatureColNames = names(data1)[!names(data1) %in% c('IDcol_1','IDcol_2','Adrian')],
    PrimaryDateColumn = NULL,
    WeightsColumnName = NULL,
    IDcols = c('IDcol_1','IDcol_2'),

    # Evaluation args
    ClassWeights = c(1L,1L),
    CostMatrixWeights = c(1,0,0,1),
    EvalMetric = 'AUC',
    grid_eval_metric = 'MCC',
    LossFunction = 'Logloss',
    MetricPeriods = 10L,
    NumOfParDepPlots = ncol(data1)-1L-2L,

    # Grid tuning args
    PassInGrid = NULL,
    GridTune = FALSE,
    MaxModelsInGrid = 30L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    BaselineComparison = 'default',

    # ML args
    Trees = 50,
    Depth = 9,
    LearningRate = NULL,
    L2_Leaf_Reg = NULL,
    model_size_reg = 0.5,
    langevin = FALSE,
    diffusion_temperature = 10000,
    RandomStrength = 1,
    BorderCount = 128,
    RSM = 1,
    BootStrapType = 'Bayesian',
    GrowPolicy = 'SymmetricTree',
    feature_border_type = 'GreedyLogSum',
    sampling_unit = 'Object',
    subsample = NULL,
    score_function = 'Cosine',
    min_data_in_leaf = 1)

  # Create Model Insights Report
  if(Run %in% c(9)) train <- RemixOutput[['TrainData']] else train <- NULL
  if(Run %in% c(9)) valid <- RemixOutput[['TestData']] else valid <- NULL
  if(Run %in% c(9)) test <- RemixOutput[['TestData']] else test <- NULL
  if(Run == 8) rm(RemixOutput)
  tryCatch({

    RemixAutoML::ModelInsightsReport(

      # DataSets (use TestData for ValidationData)
      TrainData = train,
      ValidationData = valid,
      TestData = test,

      # Meta info
      TargetColumnName = 'Adrian',
      PredictionColumnName = 'p1',
      FeatureColumnNames = names(data1)[!names(data1) %in% c('IDcol_1','IDcol_2','Adrian')],
      DateColumnName = NULL,

      # Control options
      TargetType = 'classification',
      ModelID = 'Test_Model_1',
      Algo = 'catboost',
      SourcePath = getwd(),
      OutputPath = getwd(),
      RemixOutput = if(Run %in% c(8)) NULL else RemixOutput)

  }, error = function(x) NULL)

  # Update
  if(file.exists(file.path(getwd(), "ModelInsights-Test_Model_1-classification.html"))) {
    RmarkdownQA[Run, Algo := 'catboost']
    RmarkdownQA[Run, Type := 'classification']
    RmarkdownQA[Run, Success := 'success']
    file.remove(File = file.path(getwd(), "ModelInsights-Test_Model_1-classification.html"))
  } else {
    RmarkdownQA[Run, Algo := 'catboost']
    RmarkdownQA[Run, Type := 'classification']
    RmarkdownQA[Run, Success := 'failure']
  }
}

library(data.table)
library(RemixAutoML)

# Create Model Insights Report
if(Run %in% c(9)) train <- RemixOutput[['TrainData']] else train <- NULL
if(Run %in% c(9)) valid <- RemixOutput[['TestData']] else valid <- NULL
if(Run %in% c(9)) test <- RemixOutput[['TestData']] else test <- NULL
if(Run == 3) rm(RemixOutput)

# DataSets (use TestData for ValidationData)
TrainData = train
ValidationData = valid
TestData = test

# Meta info
TargetColumnName = 'Adrian'
PredictionColumnName = 'p1'
FeatureColumnNames = names(data1)[!names(data1) %in% c('IDcol_1','IDcol_2','Adrian')]
DateColumnName = NULL

# Control options
TargetType = 'classification'
ModelID = 'Test_Model_1'
Algo = 'catboost'
SourcePath = getwd()
OutputPath = getwd()
RemixOutput = if(Run %in% c(8)) NULL else RemixOutput

Test_Importance_dt = NULL
Validation_Importance_dt = NULL
Train_Importance_dt = NULL
Test_Interaction_dt = NULL
Validation_Interaction_dt = NULL
Train_Interaction_dt = NULL
GlobalVars = ls()

# ----

#         XGBoost Classifier   ----
# Run = 9
# Run = 10
for(Run in 9:10) {

  # Working directory
  setwd("C:/Users/Bizon/Documents/GitHub")

  # Clear output
  if(Run == 9 && file.exists(file.path(getwd(), "ModelInsights-Test_Model_1-classification.html"))) {
    file.remove(File = file.path(getwd(), "ModelInsights-Test_Model_1-classification.html"))
  }

  # Create some dummy correlated data
  data <- RemixAutoML::FakeDataGenerator(
    Correlation = 0.85,
    N = 1000L,
    ID = 2L,
    ZIP = 0L,
    AddDate = FALSE,
    Classification = TRUE,
    MultiClass = FALSE)

  # Run function
  RemixOutput <- RemixAutoML::AutoXGBoostClassifier(

    # GPU or CPU
    TreeMethod = "hist",
    NThreads = parallel::detectCores(),

    # Metadata args
    OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData'),
    model_path = normalizePath("./"),
    metadata_path = NULL,
    ModelID = "Test_Model_1",
    EncodingMethod = "binary",
    ReturnFactorLevels = TRUE,
    ReturnModelObjects = if(Run == 9) TRUE else FALSE,
    SaveModelObjects = if(Run == 9) FALSE else TRUE,
    SaveInfoToPDF = FALSE,

    # Data args
    data = data,
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    WeightsColumnName = NULL,
    IDcols = c("IDcol_1","IDcol_2"),

    # Model evaluation
    LossFunction = 'reg:logistic',
    CostMatrixWeights = c(1,0,0,1),
    eval_metric = "auc",
    grid_eval_metric = "MCC",
    NumOfParDepPlots = 3L,

    # Grid tuning args
    PassInGrid = NULL,
    GridTune = FALSE,
    BaselineComparison = "default",
    MaxModelsInGrid = 10L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    Verbose = 1L,

    # ML args
    Trees = 500L,
    eta = 0.30,
    max_depth = 9L,
    min_child_weight = 1.0,
    subsample = 1,
    colsample_bytree = 1,
    DebugMode = FALSE)

  # Create Model Insights Report
  tryCatch({RemixAutoML::ModelInsightsReport(
    RemixOutput = if(Run == 9) RemixOutput else NULL,
    OutputPath = getwd(),
    TargetColumnName = 'Adrian',
    TargetType = 'classification',
    ModelID = 'Test_Model_1',
    Algo = 'xgboost')}, error = NULL)

  # Update
  if(file.exists(file.path(getwd(), "ModelInsights-Test_Model_1-classification.html"))) {
    RmarkdownQA[Run, Algo := 'xgboost']
    RmarkdownQA[Run, Type := 'classification']
    RmarkdownQA[Run, Success := 'success']
    file.remove(File = file.path(getwd(), "ModelInsights-Test_Model_1-classification.html"))
  } else {
    RmarkdownQA[Run, Algo := 'xgboost']
    RmarkdownQA[Run, Type := 'classification']
    RmarkdownQA[Run, Success := 'failure']
  }
}

# Args
# RemixOutput = RemixOutput
# OutputPath = getwd()
# TargetType = 'classification'
# ModelID = 'Test_Model_1'
# Algo = 'xgboost'

# ----

#         LightGBM Classifier  ----
# Run = 11
# Run = 12
for(Run in 11:12) {

  # Working directory
  setwd("C:/Users/Bizon/Documents/GitHub")

  # Clear output
  if(Run == 11 && file.exists(file.path(getwd(), "ModelInsights-Test_Model_1-classification.html"))) {
    file.remove(File = file.path(getwd(), "ModelInsights-Test_Model_1-classification.html"))
  }

  # Create some dummy correlated data
  data <- RemixAutoML::FakeDataGenerator(
    Correlation = 0.85,
    N = 1000,
    ID = 2,
    ZIP = 0,
    AddDate = FALSE,
    Classification = TRUE,
    MultiClass = FALSE)

  # Run function
  TestModel <- RemixAutoML::AutoLightGBMClassifier(

    # Metadata args
    OutputSelection = c("Importances","EvalPlots","EvalMetrics","Score_TrainData"),
    model_path = normalizePath("./"),
    metadata_path = NULL,
    ModelID = "Test_Model_1",
    NumOfParDepPlots = 3L,
    EncodingMethod = "credibility",
    ReturnFactorLevels = TRUE,
    ReturnModelObjects = if(Run == 9) TRUE else FALSE,
    SaveModelObjects = if(Run == 9) FALSE else TRUE,
    SaveInfoToPDF = FALSE,
    DebugMode = FALSE,

    # Data args
    data = data,
    TrainOnFull = FALSE,
    ValidationData = NULL,
    TestData = NULL,
    TargetColumnName = "Adrian",
    FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
    PrimaryDateColumn = NULL,
    WeightsColumnName = NULL,
    IDcols = c("IDcol_1","IDcol_2"),

    # Grid parameters
    GridTune = FALSE,
    grid_eval_metric = 'Utility',
    BaselineComparison = 'default',
    MaxModelsInGrid = 10L,
    MaxRunsWithoutNewWinner = 20L,
    MaxRunMinutes = 24L*60L,
    PassInGrid = NULL,

    # Core parameters
    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
    input_model = NULL, # continue training a model that is stored to file
    task = "train",
    device_type = 'CPU',
    NThreads = parallel::detectCores() / 2,
    objective = 'binary',
    metric = 'binary_logloss',
    boosting = 'gbdt',
    LinearTree = FALSE,
    Trees = 50L,
    eta = NULL,
    num_leaves = 31,
    deterministic = TRUE,

    # Learning Parameters
    # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
    force_col_wise = FALSE,
    force_row_wise = FALSE,
    max_depth = NULL,
    min_data_in_leaf = 20,
    min_sum_hessian_in_leaf = 0.001,
    bagging_freq = 0,
    bagging_fraction = 1.0,
    feature_fraction = 1.0,
    feature_fraction_bynode = 1.0,
    extra_trees = FALSE,
    early_stopping_round = 10,
    first_metric_only = TRUE,
    max_delta_step = 0.0,
    lambda_l1 = 0.0,
    lambda_l2 = 0.0,
    linear_lambda = 0.0,
    min_gain_to_split = 0,
    drop_rate_dart = 0.10,
    max_drop_dart = 50,
    skip_drop_dart = 0.50,
    uniform_drop_dart = FALSE,
    top_rate_goss = FALSE,
    other_rate_goss = FALSE,
    monotone_constraints = NULL,
    monotone_constraints_method = "advanced",
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
    num_gpu = 1)

  # Create Model Insights Report
  tryCatch({RemixAutoML::ModelInsightsReport(
    RemixOutput = if(Run == 11) RemixOutput else NULL,
    OutputPath = getwd(),
    TargetColumnName = 'Adrian',
    TargetType = 'classification',
    ModelID = 'Test_Model_1',
    Algo = 'lightgbm')}, error = NULL)

  # Update
  if(file.exists(file.path(getwd(), "ModelInsights-Test_Model_1-classification.html"))) {
    RmarkdownQA[Run, Algo := 'lightgbm']
    RmarkdownQA[Run, Type := 'classification']
    RmarkdownQA[Run, Success := 'success']
    file.remove(File = file.path(getwd(), "ModelInsights-Test_Model_1-classification.html"))
  } else {
    RmarkdownQA[Run, Algo := 'lightgbm']
    RmarkdownQA[Run, Type := 'classification']
    RmarkdownQA[Run, Success := 'failure']
  }
}

# Args
# RemixOutput = TestModel
# OutputPath = getwd()
# TargetType = 'classification'
# ModelID = 'Test_Model_1'
# Algo = 'xgboost'


# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
