#' @title AutoLightGBMHurdleModel
#'
#' @description AutoLightGBMHurdleModel is generalized hurdle modeling framework
#'
#' @family Supervised Learning - Hurdle Modeling
#' @author Adrian Antico
#'
#' @param TrainOnFull Set to TRUE to train model on 100 percent of data
#' @param grid_eval_metric Select the metric to optimize in grid tuning. "accuracy", "microauc", "logloss"
#' @param BaselineComparison "default"
#' @param MaxRunsWithoutNewWinner Number of runs without a new winner before stopping the grid tuning
#' @param MaxRunMinutes Max number of minutes to allow the grid tuning to run for
#' @param data Source training data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param ValidationData Source validation data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param TestData Souce test data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param Buckets A numeric vector of the buckets used for subsetting the data. NOTE: the final Bucket value will first create a subset of data that is less than the value and a second one thereafter for data greater than the bucket value.
#' @param TargetColumnName Supply the column name or number for the target variable
#' @param FeatureColNames Supply the column names or number of the features (not included the PrimaryDateColumn)
#' @param PrimaryDateColumn Date column for sorting
#' @param WeightsColumnName Weighs column name
#' @param IDcols Includes PrimaryDateColumn and any other columns you want returned in the validation data with predictions
#' @param ClassWeights Look up the classifier model help file
#' @param DebugMode For debugging
#' @param EncodingMethod Choose from 'binary', 'poly_encode', 'backward_difference', 'helmert' for multiclass cases and additionally 'm_estimator', 'credibility', 'woe', 'target_encoding' for classification use cases.
#' @param TransformNumericColumns Transform numeric column inside the AutoCatBoostRegression() function
#' @param Methods Choose from 'Asinh', 'Asin', 'Log', 'LogPlus1', 'Sqrt', 'Logit'
#' @param SplitRatios Supply vector of partition ratios. For example, c(0.70,0.20,0,10)
#' @param NThreads Set to the number of threads you would like to dedicate to training
#' @param ModelID Define a character name for your models
#' @param Paths The path to your folder where you want your model information saved
#' @param MetaDataPaths A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to Paths.
#' @param ReturnModelObjects Set to TRUE to return all model objects
#' @param SaveModelObjects Set to TRUE to save the model objects to file in the folders listed in Paths
#' @param GridTune Set to TRUE if you want to grid tune the models
#' @param MaxModelsInGrid Set to a numeric value for the number of models to try in grid tune
#' @param NumOfParDepPlots Set to pull back N number of partial dependence calibration plots.
#' @param PassInGrid Pass in a grid for changing up the parameter settings for catboost
#'
#' # Core parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameter
#' @param input_model = NULL, # continue training a model that is stored to fil
#' @param task 'train' or 'refit'
#' @param device_type 'cpu' or 'gpu'
#' @param NThreads only list up to number of cores, not threads. parallel::detectCores() / 2
#' @param objective 'binary'
#' @param metric 'binary_logloss', 'average_precision', 'auc', 'map', 'binary_error', 'auc_mu'
#' @param boosting 'gbdt', 'rf', 'dart', 'goss'
#' @param LinearTree FALSE
#' @param Trees 50L
#' @param eta NULL
#' @param num_leaves 31
#' @param deterministic TRUE
#'
#' # Learning Parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameter
#' @param force_col_wise FALSE
#' @param force_row_wise FALSE
#' @param max_depth NULL
#' @param min_data_in_leaf 20
#' @param min_sum_hessian_in_leaf 0.001
#' @param bagging_freq 0
#' @param bagging_fraction 1.0
#' @param feature_fraction 1.0
#' @param feature_fraction_bynode 1.0
#' @param extra_trees FALSE
#' @param early_stopping_round 10
#' @param first_metric_only TRUE
#' @param max_delta_step 0.0
#' @param lambda_l1 0.0
#' @param lambda_l2 0.0
#' @param linear_lambda 0.0
#' @param min_gain_to_split 0
#' @param drop_rate_dart 0.10
#' @param max_drop_dart 50
#' @param skip_drop_dart 0.50
#' @param uniform_drop_dart FALSE
#' @param top_rate_goss FALSE
#' @param other_rate_goss FALSE
#' @param monotone_constraints "gbdt_prediction.cpp"
#' @param monotone_constraints_method 'advanced'
#' @param monotone_penalty  0.0
#' @param forcedsplits_filename NULL # use for AutoStack option; .json fil
#' @param refit_decay_rate 0.90
#' @param path_smooth 0.0
#'
#' # IO Dataset Parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
#' @param max_bin 255
#' @param min_data_in_bin 3
#' @param data_random_seed 1
#' @param is_enable_sparse TRUE
#' @param enable_bundle TRUE
#' @param use_missing TRUE
#' @param zero_as_missing FALSE
#' @param two_round FALSE
#'
#' # Convert Parameters # https://lightgbm.readthedocs.io/en/latest/Parameters.html#convert-parameters
#' @param convert_model 'gbdt_prediction.cpp'
#' @param convert_model_language 'cpp'
#'
#' # Objective Parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
#' @param boost_from_average TRUE
#' @param is_unbalance FALSE
#' @param scale_pos_weight 1.0
#'
#' # Metric Parameters (metric is in Core)
#' @param is_provide_training_metric TRUE
#' @param eval_at c(1,2,3,4,5)
#'
#' # Network Parameter
#' @param num_machines 1
#'
#' # GPU Parameter
#' @param gpu_platform_id -1
#' @param gpu_device_id -1
#' @param gpu_use_dp TRUE
#' @param num_gpu 1
#' @examples
#' \dontrun{
#' Output <- RemixAutoML::AutoLightGBMHurdleModel(
#'
#'    # Operationalization args
#'    TrainOnFull = FALSE,
#'    PassInGrid = NULL,
#'
#'    # Metadata args
#'    NThreads = max(1L, parallel::detectCores()-2L),
#'    ModelID = "ModelTest",
#'    Paths = normalizePath("./"),
#'    MetaDataPaths = NULL,
#'
#'    # data args
#'    data,
#'    ValidationData = NULL,
#'    TestData = NULL,
#'    Buckets = 0L,
#'    TargetColumnName = NULL,
#'    FeatureColNames = NULL,
#'    PrimaryDateColumn = NULL,
#'    WeightsColumnName = NULL,
#'    IDcols = NULL,
#'    ClassWeights = c(1,1),
#'    DebugMode = FALSE,
#'
#'    # options
#'    EncodingMethod = "credibility",
#'    TransformNumericColumns = NULL,
#'    Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'),
#'    SplitRatios = c(0.70, 0.20, 0.10),
#'    ReturnModelObjects = TRUE,
#'    SaveModelObjects = FALSE,
#'    NumOfParDepPlots = 10L,
#'
#'    # grid tuning args
#'    GridTune = FALSE,
#'    grid_eval_metric = "accuracy",
#'    MaxModelsInGrid = 1L,
#'    BaselineComparison = "default",
#'    MaxRunsWithoutNewWinner = 10L,
#'    MaxRunMinutes = 60L,
#'
#'    # Core parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
#'    input_model = list('classifier' = NULL, 'regression' = NULL),
#'    task = list('classifier' = 'train', 'regression' = 'train'),
#'    device_type = list('classifier' = 'CPU', 'regression' = 'CPU'),
#'    objective = list('classifier' = 'binary', 'regression' = 'regression'),
#'    metric = list('classifier' = 'binary_logloss', 'regression' = 'rmse'),
#'    boosting = list('classifier' = 'gbdt', 'regression' = 'gbdt'),
#'    LinearTree = list('classifier' = FALSE, 'regression' = FALSE),
#'    Trees = list('classifier' = 1000L, 'regression' = 1000L),
#'    eta = list('classifier' = NULL, 'regression' = NULL),
#'    num_leaves = list('classifier' = 31, 'regression' = 31),
#'    deterministic = list('classifier' = TRUE, 'regression' = TRUE),
#'
#'    # Learning Parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
#'    force_col_wise = list('classifier' = FALSE, 'regression' = FALSE),
#'    force_row_wise = list('classifier' = FALSE, 'regression' = FALSE),
#'    max_depth = list('classifier' = NULL, 'regression' = NULL),
#'    min_data_in_leaf = list('classifier' = 20, 'regression' = 20),
#'    min_sum_hessian_in_leaf = list('classifier' = 0.001, 'regression' = 0.001),
#'    bagging_freq = list('classifier' = 0, 'regression' = 0),
#'    bagging_fraction = list('classifier' = 1.0, 'regression' = 1.0),
#'    feature_fraction = list('classifier' = 1.0, 'regression' = 1.0),
#'    feature_fraction_bynode = list('classifier' = 1.0, 'regression' = 1.0),
#'    extra_trees = list('classifier' = FALSE, 'regression' = FALSE),
#'    early_stopping_round = list('classifier' = 10, 'regression' = 10),
#'    first_metric_only = list('classifier' = TRUE, 'regression' = TRUE),
#'    max_delta_step = list('classifier' = 0.0, 'regression' = 0.0),
#'    lambda_l1 = list('classifier' = 0.0, 'regression' = 0.0),
#'    lambda_l2 = list('classifier' = 0.0, 'regression' = 0.0),
#'    linear_lambda = list('classifier' = 0.0, 'regression' = 0.0),
#'    min_gain_to_split = list('classifier' = 0, 'regression' = 0),
#'    drop_rate_dart = list('classifier' = 0.10, 'regression' = 0.10),
#'    max_drop_dart = list('classifier' = 50, 'regression' = 50),
#'    skip_drop_dart = list('classifier' = 0.50, 'regression' = 0.50),
#'    uniform_drop_dart = list('classifier' = FALSE, 'regression' = FALSE),
#'    top_rate_goss = list('classifier' = FALSE, 'regression' = FALSE),
#'    other_rate_goss = list('classifier' = FALSE, 'regression' = FALSE),
#'    monotone_constraints = list('classifier' = NULL, 'regression' = NULL),
#'    monotone_constraints_method = list('classifier' = 'advanced', 'regression' = 'advanced'),
#'    monotone_penalty = list('classifier' = 0.0, 'regression' = 0.0),
#'    forcedsplits_filename = list('classifier' = NULL, 'regression' = NULL),
#'    refit_decay_rate = list('classifier' = 0.90, 'regression' = 0.90),
#'    path_smooth = list('classifier' = 0.0, 'regression' = 0.0),
#'
#'    # IO Dataset Parameters
#'    max_bin = list('classifier' = 255, 'regression' = 255),
#'    min_data_in_bin = list('classifier' = 3, 'regression' = 3),
#'    data_random_seed = list('classifier' = 1, 'regression' = 1),
#'    is_enable_sparse = list('classifier' = TRUE, 'regression' = TRUE),
#'    enable_bundle = list('classifier' = TRUE, 'regression' = TRUE),
#'    use_missing = list('classifier' = TRUE, 'regression' = TRUE),
#'    zero_as_missing = list('classifier' = FALSE, 'regression' = FALSE),
#'    two_round = list('classifier' = FALSE, 'regression' = FALSE),
#'
#'    # Convert Parameters
#'    convert_model = list('classifier' = NULL, 'regression' = NULL),
#'    convert_model_language = list('classifier' = "cpp", 'regression' = "cpp"),
#'
#'    # Objective Parameters
#'    boost_from_average = list('classifier' = TRUE, 'regression' = TRUE),
#'    is_unbalance = list('classifier' = FALSE, 'regression' = FALSE),
#'    scale_pos_weight = list('classifier' = 1.0, 'regression' = 1.0),
#'
#'    # Metric Parameters (metric is in Core)
#'    is_provide_training_metric = list('classifier' = TRUE, 'regression' = TRUE),
#'    eval_at = list('classifier' = c(1,2,3,4,5), 'regression' = c(1,2,3,4,5)),
#'
#'    # Network Parameters
#'    num_machines = list('classifier' = 1, 'regression' = 1),
#'
#'    # GPU Parameters
#'    gpu_platform_id = list('classifier' = -1, 'regression' = -1),
#'    gpu_device_id = list('classifier' = -1, 'regression' = -1),
#'    gpu_use_dp = list('classifier' = TRUE, 'regression' = TRUE),
#'    num_gpu = list('classifier' = 1, 'regression' = 1))
#' }
#' @export
AutoLightGBMHurdleModel <- function(TrainOnFull = FALSE,
                                    PassInGrid = NULL,
                                    NThreads = max(1L, parallel::detectCores()-2L),
                                    ModelID = "ModelTest",
                                    Paths = NULL,
                                    MetaDataPaths = NULL,
                                    data,
                                    ValidationData = NULL,
                                    TestData = NULL,
                                    Buckets = 0L,
                                    TargetColumnName = NULL,
                                    FeatureColNames = NULL,
                                    PrimaryDateColumn = NULL,
                                    WeightsColumnName = NULL,
                                    ClassWeights = c(1,1),
                                    IDcols = NULL,
                                    DebugMode = FALSE,
                                    EncodingMethod = "credibility",
                                    TransformNumericColumns = NULL,
                                    Methods = c('Asinh','Asin','Log','LogPlus1','Sqrt','Logit'),
                                    SplitRatios = c(0.70, 0.20, 0.10),
                                    SaveModelObjects = FALSE,
                                    ReturnModelObjects = TRUE,
                                    NumOfParDepPlots = 1L,
                                    GridTune = FALSE,
                                    grid_eval_metric = "accuracy",
                                    MaxModelsInGrid = 1L,
                                    BaselineComparison = "default",
                                    MaxRunsWithoutNewWinner = 10L,
                                    MaxRunMinutes = 60L,

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
                                    num_gpu = list('classifier' = 1, 'regression' = 1)) {

  # Store args ----
  ArgsList <- list()
  ArgsList[["Buckets"]] <- Buckets
  ArgsList[["TargetColumnName"]] <- TargetColumnName
  ArgsList[["FeatureColNames"]] <- FeatureColNames
  ArgsList[["IDcols"]] <- IDcols
  ArgsList[["TransformNumericColumns"]] <- TransformNumericColumns
  ArgsList[["SplitRatios"]] <- SplitRatios
  ArgsList[["ModelID"]] <- ModelID
  ArgsList[["Paths"]] <- Paths
  ArgsList[["MetaDataPaths"]] <- MetaDataPaths
  ArgsList[["SaveModelObjects"]] <- SaveModelObjects
  ArgsList[["EncodingMethod"]] <- EncodingMethod

  # Check args ----
  if(is.character(Buckets) || is.factor(Buckets) || is.logical(Buckets)) stop("Buckets needs to be a numeric scalar or vector")
  if(!is.logical(SaveModelObjects)) stop("SaveModelOutput needs to be set to either TRUE or FALSE")
  if(!is.logical(GridTune)) stop("GridTune needs to be either TRUE or FALSE")

  # Args management ----

  # input_model
  if(is.list(input_model)) {
    if(!GridTune) {
      Classifierinput_model <- input_model[["classifier"]]
      Classifierinput_model <- Classifierinput_model[length(Classifierinput_model)]
      Regressioninput_model <- input_model[["regression"]]
      Regressioninput_model <- Regressioninput_model[length(Regressioninput_model)]
    } else {
      Classifierinput_model <- input_model[["classifier"]]
      Regressioninput_model <- input_model[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressioninput_model <- input_model[length(input_model)]
      Classifierinput_model <- input_model[length(input_model)]
    } else {
      Classifierinput_model <- input_model
      Regressioninput_model <- input_model
    }
  }

  # task
  if(is.list(task)) {
    if(!GridTune) {
      Classifiertask <- task[["classifier"]]
      Classifiertask <- Classifiertask[length(Classifiertask)]
      Regressiontask <- task[["regression"]]
      Regressiontask <- Regressiontask[length(Regressiontask)]
    } else {
      Classifiertask <- task[["classifier"]]
      Regressiontask <- task[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressiontask <- task[length(task)]
      Classifiertask <- task[length(task)]
    } else {
      Classifiertask <- task
      Regressiontask <- task
    }
  }

  # device_type
  if(is.list(device_type)) {
    if(!GridTune) {
      Classifierdevice_type <- device_type[["classifier"]]
      Classifierdevice_type <- Classifierdevice_type[length(Classifierdevice_type)]
      Regressiondevice_type <- device_type[["regression"]]
      Regressiondevice_type <- Regressiondevice_type[length(Regressiondevice_type)]
    } else {
      Classifierdevice_type <- device_type[["classifier"]]
      Regressiondevice_type <- device_type[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressiondevice_type <- device_type[length(device_type)]
      Classifierdevice_type <- device_type[length(device_type)]
    } else {
      Classifierdevice_type <- device_type
      Regressiondevice_type <- device_type
    }
  }

  # objective
  if(is.list(objective)) {
    if(!GridTune) {
      Classifierobjective <- objective[["classifier"]]
      Classifierobjective <- Classifierobjective[length(Classifierobjective)]
      Regressionobjective <- objective[["regression"]]
      Regressionobjective <- Regressionobjective[length(Regressionobjective)]
    } else {
      Classifierobjective <- objective[["classifier"]]
      Regressionobjective <- objective[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionobjective <- objective[length(objective)]
      Classifierobjective <- objective[length(objective)]
    } else {
      Classifierobjective <- objective
      Regressionobjective <- objective
    }
  }

  # metric
  if(is.list(metric)) {
    if(!GridTune) {
      Classifiermetric <- metric[["classifier"]]
      Classifiermetric <- Classifiermetric[length(Classifiermetric)]
      Regressionmetric <- metric[["regression"]]
      Regressionmetric <- Regressionmetric[length(Regressionmetric)]
    } else {
      Classifiermetric <- metric[["classifier"]]
      Regressionmetric <- metric[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmetric <- metric[length(metric)]
      Classifiermetric <- metric[length(metric)]
    } else {
      Classifiermetric <- metric
      Regressionmetric <- metric
    }
  }

  # boosting
  if(is.list(boosting)) {
    if(!GridTune) {
      Classifierboosting <- boosting[["classifier"]]
      Classifierboosting <- Classifierboosting[length(Classifierboosting)]
      Regressionboosting <- boosting[["regression"]]
      Regressionboosting <- Regressionboosting[length(Regressionboosting)]
    } else {
      Classifierboosting <- boosting[["classifier"]]
      Regressionboosting <- boosting[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionboosting <- boosting[length(boosting)]
      Classifierboosting <- boosting[length(boosting)]
    } else {
      Classifierboosting <- boosting
      Regressionboosting <- boosting
    }
  }

  # LinearTree
  if(is.list(LinearTree)) {
    if(!GridTune) {
      ClassifierLinearTree <- LinearTree[["classifier"]]
      ClassifierLinearTree <- ClassifierLinearTree[length(ClassifierLinearTree)]
      RegressionLinearTree <- LinearTree[["regression"]]
      RegressionLinearTree <- RegressionLinearTree[length(RegressionLinearTree)]
    } else {
      ClassifierLinearTree <- LinearTree[["classifier"]]
      RegressionLinearTree <- LinearTree[["regression"]]
    }
  } else {
    if(!GridTune) {
      RegressionLinearTree <- LinearTree[length(LinearTree)]
      ClassifierLinearTree <- LinearTree[length(LinearTree)]
    } else {
      ClassifierLinearTree <- LinearTree
      RegressionLinearTree <- LinearTree
    }
  }

  # Trees
  if(is.list(Trees)) {
    if(!GridTune) {
      ClassifierTrees <- Trees[["classifier"]]
      ClassifierTrees <- ClassifierTrees[length(ClassifierTrees)]
      RegressionTrees <- Trees[["regression"]]
      RegressionTrees <- RegressionTrees[length(RegressionTrees)]
    } else {
      ClassifierTrees <- Trees[["classifier"]]
      RegressionTrees <- Trees[["regression"]]
    }
  } else {
    if(!GridTune) {
      RegressionTrees <- Trees[length(Trees)]
      ClassifierTrees <- Trees[length(Trees)]
    } else {
      ClassifierTrees <- Trees
      RegressionTrees <- Trees
    }
  }

  # eta
  if(is.list(eta)) {
    if(!GridTune) {
      Classifiereta <- eta[["classifier"]]
      Classifiereta <- Classifiereta[length(Classifiereta)]
      Regressioneta <- eta[["regression"]]
      Regressioneta <- Regressioneta[length(Regressioneta)]
    } else {
      Classifiereta <- eta[["classifier"]]
      Regressioneta <- eta[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressioneta <- eta[length(eta)]
      Classifiereta <- eta[length(eta)]
    } else {
      Classifiereta <- eta
      Regressioneta <- eta
    }
  }

  # num_leaves
  if(is.list(num_leaves)) {
    if(!GridTune) {
      Classifiernum_leaves <- num_leaves[["classifier"]]
      Classifiernum_leaves <- Classifiernum_leaves[length(Classifiernum_leaves)]
      Regressionnum_leaves <- num_leaves[["regression"]]
      Regressionnum_leaves <- Regressionnum_leaves[length(Regressionnum_leaves)]
    } else {
      Classifiernum_leaves <- num_leaves[["classifier"]]
      Regressionnum_leaves <- num_leaves[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionnum_leaves <- num_leaves[length(num_leaves)]
      Classifiernum_leaves <- num_leaves[length(num_leaves)]
    } else {
      Classifiernum_leaves <- num_leaves
      Regressionnum_leaves <- num_leaves
    }
  }

  # deterministic
  if(is.list(deterministic)) {
    if(!GridTune) {
      Classifierdeterministic <- deterministic[["classifier"]]
      Classifierdeterministic <- Classifierdeterministic[length(Classifierdeterministic)]
      Regressiondeterministic <- deterministic[["regression"]]
      Regressiondeterministic <- Regressiondeterministic[length(Regressiondeterministic)]
    } else {
      Classifierdeterministic <- deterministic[["classifier"]]
      Regressiondeterministic <- deterministic[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressiondeterministic <- deterministic[length(deterministic)]
      Classifierdeterministic <- deterministic[length(deterministic)]
    } else {
      Classifierdeterministic <- deterministic
      Regressiondeterministic <- deterministic
    }
  }

  # force_col_wise
  if(is.list(force_col_wise)) {
    if(!GridTune) {
      Classifierforce_col_wise <- force_col_wise[["classifier"]]
      Classifierforce_col_wise <- Classifierforce_col_wise[length(Classifierforce_col_wise)]
      Regressionforce_col_wise <- force_col_wise[["regression"]]
      Regressionforce_col_wise <- Regressionforce_col_wise[length(Regressionforce_col_wise)]
    } else {
      Classifierforce_col_wise <- force_col_wise[["classifier"]]
      Regressionforce_col_wise <- force_col_wise[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionforce_col_wise <- force_col_wise[length(force_col_wise)]
      Classifierforce_col_wise <- force_col_wise[length(force_col_wise)]
    } else {
      Classifierforce_col_wise <- force_col_wise
      Regressionforce_col_wise <- force_col_wise
    }
  }

  # force_row_wise
  if(is.list(force_row_wise)) {
    if(!GridTune) {
      Classifierforce_row_wise <- force_row_wise[["classifier"]]
      Classifierforce_row_wise <- Classifierforce_row_wise[length(Classifierforce_row_wise)]
      Regressionforce_row_wise <- force_row_wise[["regression"]]
      Regressionforce_row_wise <- Regressionforce_row_wise[length(Regressionforce_row_wise)]
    } else {
      Classifierforce_row_wise <- force_row_wise[["classifier"]]
      Regressionforce_row_wise <- force_row_wise[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionforce_row_wise <- force_row_wise[length(force_row_wise)]
      Classifierforce_row_wise <- force_row_wise[length(force_row_wise)]
    } else {
      Classifierforce_row_wise <- force_row_wise
      Regressionforce_row_wise <- force_row_wise
    }
  }

  # max_depth
  if(is.list(max_depth)) {
    if(!GridTune) {
      Classifiermax_depth <- max_depth[["classifier"]]
      Classifiermax_depth <- Classifiermax_depth[length(Classifiermax_depth)]
      Regressionmax_depth <- max_depth[["regression"]]
      Regressionmax_depth <- Regressionmax_depth[length(Regressionmax_depth)]
    } else {
      Classifiermax_depth <- max_depth[["classifier"]]
      Regressionmax_depth <- max_depth[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmax_depth <- max_depth[length(max_depth)]
      Classifiermax_depth <- max_depth[length(max_depth)]
    } else {
      Classifiermax_depth <- max_depth
      Regressionmax_depth <- max_depth
    }
  }

  # min_data_in_leaf
  if(is.list(min_data_in_leaf)) {
    if(!GridTune) {
      Classifiermin_data_in_leaf <- min_data_in_leaf[["classifier"]]
      Classifiermin_data_in_leaf <- Classifiermin_data_in_leaf[length(Classifiermin_data_in_leaf)]
      Regressionmin_data_in_leaf <- min_data_in_leaf[["regression"]]
      Regressionmin_data_in_leaf <- Regressionmin_data_in_leaf[length(Regressionmin_data_in_leaf)]
    } else {
      Classifiermin_data_in_leaf <- min_data_in_leaf[["classifier"]]
      Regressionmin_data_in_leaf <- min_data_in_leaf[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmin_data_in_leaf <- min_data_in_leaf[length(min_data_in_leaf)]
      Classifiermin_data_in_leaf <- min_data_in_leaf[length(min_data_in_leaf)]
    } else {
      Classifiermin_data_in_leaf <- min_data_in_leaf
      Regressionmin_data_in_leaf <- min_data_in_leaf
    }
  }

  # min_sum_hessian_in_leaf
  if(is.list(min_sum_hessian_in_leaf)) {
    if(!GridTune) {
      Classifiermin_sum_hessian_in_leaf <- min_sum_hessian_in_leaf[["classifier"]]
      Classifiermin_sum_hessian_in_leaf <- Classifiermin_sum_hessian_in_leaf[length(Classifiermin_sum_hessian_in_leaf)]
      Regressionmin_sum_hessian_in_leaf <- min_sum_hessian_in_leaf[["regression"]]
      Regressionmin_sum_hessian_in_leaf <- Regressionmin_sum_hessian_in_leaf[length(Regressionmin_sum_hessian_in_leaf)]
    } else {
      Classifiermin_sum_hessian_in_leaf <- min_sum_hessian_in_leaf[["classifier"]]
      Regressionmin_sum_hessian_in_leaf <- min_sum_hessian_in_leaf[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmin_sum_hessian_in_leaf <- min_sum_hessian_in_leaf[length(min_sum_hessian_in_leaf)]
      Classifiermin_sum_hessian_in_leaf <- min_sum_hessian_in_leaf[length(min_sum_hessian_in_leaf)]
    } else {
      Classifiermin_sum_hessian_in_leaf <- min_sum_hessian_in_leaf
      Regressionmin_sum_hessian_in_leaf <- min_sum_hessian_in_leaf
    }
  }

  # bagging_freq
  if(is.list(bagging_freq)) {
    if(!GridTune) {
      Classifierbagging_freq <- bagging_freq[["classifier"]]
      Classifierbagging_freq <- Classifierbagging_freq[length(Classifierbagging_freq)]
      Regressionbagging_freq <- bagging_freq[["regression"]]
      Regressionbagging_freq <- Regressionbagging_freq[length(Regressionbagging_freq)]
    } else {
      Classifierbagging_freq <- bagging_freq[["classifier"]]
      Regressionbagging_freq <- bagging_freq[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionbagging_freq <- bagging_freq[length(bagging_freq)]
      Classifierbagging_freq <- bagging_freq[length(bagging_freq)]
    } else {
      Classifierbagging_freq <- bagging_freq
      Regressionbagging_freq <- bagging_freq
    }
  }

  # bagging_fraction
  if(is.list(bagging_fraction)) {
    if(!GridTune) {
      Classifierbagging_fraction <- bagging_fraction[["classifier"]]
      Classifierbagging_fraction <- Classifierbagging_fraction[length(Classifierbagging_fraction)]
      Regressionbagging_fraction <- bagging_fraction[["regression"]]
      Regressionbagging_fraction <- Regressionbagging_fraction[length(Regressionbagging_fraction)]
    } else {
      Classifierbagging_fraction <- bagging_fraction[["classifier"]]
      Regressionbagging_fraction <- bagging_fraction[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionbagging_fraction <- bagging_fraction[length(bagging_fraction)]
      Classifierbagging_fraction <- bagging_fraction[length(bagging_fraction)]
    } else {
      Classifierbagging_fraction <- bagging_fraction
      Regressionbagging_fraction <- bagging_fraction
    }
  }

  # feature_fraction
  if(is.list(feature_fraction)) {
    if(!GridTune) {
      Classifierfeature_fraction <- feature_fraction[["classifier"]]
      Classifierfeature_fraction <- Classifierfeature_fraction[length(Classifierfeature_fraction)]
      Regressionfeature_fraction <- feature_fraction[["regression"]]
      Regressionfeature_fraction <- Regressionfeature_fraction[length(Regressionfeature_fraction)]
    } else {
      Classifierfeature_fraction <- feature_fraction[["classifier"]]
      Regressionfeature_fraction <- feature_fraction[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionfeature_fraction <- feature_fraction[length(feature_fraction)]
      Classifierfeature_fraction <- feature_fraction[length(feature_fraction)]
    } else {
      Classifierfeature_fraction <- feature_fraction
      Regressionfeature_fraction <- feature_fraction
    }
  }

  # feature_fraction_bynode
  if(is.list(feature_fraction_bynode)) {
    if(!GridTune) {
      Classifierfeature_fraction_bynode <- feature_fraction_bynode[["classifier"]]
      Classifierfeature_fraction_bynode <- Classifierfeature_fraction_bynode[length(Classifierfeature_fraction_bynode)]
      Regressionfeature_fraction_bynode <- feature_fraction_bynode[["regression"]]
      Regressionfeature_fraction_bynode <- Regressionfeature_fraction_bynode[length(Regressionfeature_fraction_bynode)]
    } else {
      Classifierfeature_fraction_bynode <- feature_fraction_bynode[["classifier"]]
      Regressionfeature_fraction_bynode <- feature_fraction_bynode[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionfeature_fraction_bynode <- feature_fraction_bynode[length(feature_fraction_bynode)]
      Classifierfeature_fraction_bynode <- feature_fraction_bynode[length(feature_fraction_bynode)]
    } else {
      Classifierfeature_fraction_bynode <- feature_fraction_bynode
      Regressionfeature_fraction_bynode <- feature_fraction_bynode
    }
  }

  # lambda_l1
  if(is.list(lambda_l1)) {
    if(!GridTune) {
      Classifierlambda_l1 <- lambda_l1[["classifier"]]
      Classifierlambda_l1 <- Classifierlambda_l1[length(Classifierlambda_l1)]
      Regressionlambda_l1 <- lambda_l1[["regression"]]
      Regressionlambda_l1 <- Regressionlambda_l1[length(Regressionlambda_l1)]
    } else {
      Classifierlambda_l1 <- lambda_l1[["classifier"]]
      Regressionlambda_l1 <- lambda_l1[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionlambda_l1 <- lambda_l1[length(lambda_l1)]
      Classifierlambda_l1 <- lambda_l1[length(lambda_l1)]
    } else {
      Classifierlambda_l1 <- lambda_l1
      Regressionlambda_l1 <- lambda_l1
    }
  }

  # lambda_l2
  if(is.list(lambda_l2)) {
    if(!GridTune) {
      Classifierlambda_l2 <- lambda_l2[["classifier"]]
      Classifierlambda_l2 <- Classifierlambda_l2[length(Classifierlambda_l2)]
      Regressionlambda_l2 <- lambda_l2[["regression"]]
      Regressionlambda_l2 <- Regressionlambda_l2[length(Regressionlambda_l2)]
    } else {
      Classifierlambda_l2 <- lambda_l2[["classifier"]]
      Regressionlambda_l2 <- lambda_l2[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionlambda_l2 <- lambda_l2[length(lambda_l2)]
      Classifierlambda_l2 <- lambda_l2[length(lambda_l2)]
    } else {
      Classifierlambda_l2 <- lambda_l2
      Regressionlambda_l2 <- lambda_l2
    }
  }

  # extra_trees
  if(is.list(extra_trees)) {
    if(!GridTune) {
      Classifierextra_trees <- extra_trees[["classifier"]]
      Classifierextra_trees <- Classifierextra_trees[length(Classifierextra_trees)]
      Regressionextra_trees <- extra_trees[["regression"]]
      Regressionextra_trees <- Regressionextra_trees[length(Regressionextra_trees)]
    } else {
      Classifierextra_trees <- extra_trees[["classifier"]]
      Regressionextra_trees <- extra_trees[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionextra_trees <- extra_trees[length(extra_trees)]
      Classifierextra_trees <- extra_trees[length(extra_trees)]
    } else {
      Classifierextra_trees <- extra_trees
      Regressionextra_trees <- extra_trees
    }
  }

  # early_stopping_round
  if(is.list(early_stopping_round)) {
    if(!GridTune) {
      Classifierearly_stopping_round <- early_stopping_round[["classifier"]]
      Classifierearly_stopping_round <- Classifierearly_stopping_round[length(Classifierearly_stopping_round)]
      Regressionearly_stopping_round <- early_stopping_round[["regression"]]
      Regressionearly_stopping_round <- Regressionearly_stopping_round[length(Regressionearly_stopping_round)]
    } else {
      Classifierearly_stopping_round <- early_stopping_round[["classifier"]]
      Regressionearly_stopping_round <- early_stopping_round[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionearly_stopping_round <- early_stopping_round[length(early_stopping_round)]
      Classifierearly_stopping_round <- early_stopping_round[length(early_stopping_round)]
    } else {
      Classifierearly_stopping_round <- early_stopping_round
      Regressionearly_stopping_round <- early_stopping_round
    }
  }

  # first_metric_only
  if(is.list(first_metric_only)) {
    if(!GridTune) {
      Classifierfirst_metric_only <- first_metric_only[["classifier"]]
      Classifierfirst_metric_only <- Classifierfirst_metric_only[length(Classifierfirst_metric_only)]
      Regressionfirst_metric_only <- first_metric_only[["regression"]]
      Regressionfirst_metric_only <- Regressionfirst_metric_only[length(Regressionfirst_metric_only)]
    } else {
      Classifierfirst_metric_only <- first_metric_only[["classifier"]]
      Regressionfirst_metric_only <- first_metric_only[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionfirst_metric_only <- first_metric_only[length(first_metric_only)]
      Classifierfirst_metric_only <- first_metric_only[length(first_metric_only)]
    } else {
      Classifierfirst_metric_only <- first_metric_only
      Regressionfirst_metric_only <- first_metric_only
    }
  }

  # max_delta_step
  if(is.list(max_delta_step)) {
    if(!GridTune) {
      Classifiermax_delta_step <- max_delta_step[["classifier"]]
      Classifiermax_delta_step <- Classifiermax_delta_step[length(Classifiermax_delta_step)]
      Regressionmax_delta_step <- max_delta_step[["regression"]]
      Regressionmax_delta_step <- Regressionmax_delta_step[length(Regressionmax_delta_step)]
    } else {
      Classifiermax_delta_step <- max_delta_step[["classifier"]]
      Regressionmax_delta_step <- max_delta_step[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmax_delta_step <- max_delta_step[length(max_delta_step)]
      Classifiermax_delta_step <- max_delta_step[length(max_delta_step)]
    } else {
      Classifiermax_delta_step <- max_delta_step
      Regressionmax_delta_step <- max_delta_step
    }
  }

  # linear_lambda
  if(is.list(linear_lambda)) {
    if(!GridTune) {
      Classifierlinear_lambda <- linear_lambda[["classifier"]]
      Classifierlinear_lambda <- Classifierlinear_lambda[length(Classifierlinear_lambda)]
      Regressionlinear_lambda <- linear_lambda[["regression"]]
      Regressionlinear_lambda <- Regressionlinear_lambda[length(Regressionlinear_lambda)]
    } else {
      Classifierlinear_lambda <- linear_lambda[["classifier"]]
      Regressionlinear_lambda <- linear_lambda[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionlinear_lambda <- linear_lambda[length(linear_lambda)]
      Classifierlinear_lambda <- linear_lambda[length(linear_lambda)]
    } else {
      Classifierlinear_lambda <- linear_lambda
      Regressionlinear_lambda <- linear_lambda
    }
  }

  # min_gain_to_split
  if(is.list(min_gain_to_split)) {
    if(!GridTune) {
      Classifiermin_gain_to_split <- min_gain_to_split[["classifier"]]
      Classifiermin_gain_to_split <- Classifiermin_gain_to_split[length(Classifiermin_gain_to_split)]
      Regressionmin_gain_to_split <- min_gain_to_split[["regression"]]
      Regressionmin_gain_to_split <- Regressionmin_gain_to_split[length(Regressionmin_gain_to_split)]
    } else {
      Classifiermin_gain_to_split <- min_gain_to_split[["classifier"]]
      Regressionmin_gain_to_split <- min_gain_to_split[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmin_gain_to_split <- min_gain_to_split[length(min_gain_to_split)]
      Classifiermin_gain_to_split <- min_gain_to_split[length(min_gain_to_split)]
    } else {
      Classifiermin_gain_to_split <- min_gain_to_split
      Regressionmin_gain_to_split <- min_gain_to_split
    }
  }

  # drop_rate_dart
  if(is.list(drop_rate_dart)) {
    if(!GridTune) {
      Classifierdrop_rate_dart <- drop_rate_dart[["classifier"]]
      Classifierdrop_rate_dart <- Classifierdrop_rate_dart[length(Classifierdrop_rate_dart)]
      Regressiondrop_rate_dart <- drop_rate_dart[["regression"]]
      Regressiondrop_rate_dart <- Regressiondrop_rate_dart[length(Regressiondrop_rate_dart)]
    } else {
      Classifierdrop_rate_dart <- drop_rate_dart[["classifier"]]
      Regressiondrop_rate_dart <- drop_rate_dart[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressiondrop_rate_dart <- drop_rate_dart[length(drop_rate_dart)]
      Classifierdrop_rate_dart <- drop_rate_dart[length(drop_rate_dart)]
    } else {
      Classifierdrop_rate_dart <- drop_rate_dart
      Regressiondrop_rate_dart <- drop_rate_dart
    }
  }

  # max_drop_dart
  if(is.list(max_drop_dart)) {
    if(!GridTune) {
      Classifiermax_drop_dart <- max_drop_dart[["classifier"]]
      Classifiermax_drop_dart <- Classifiermax_drop_dart[length(Classifiermax_drop_dart)]
      Regressionmax_drop_dart <- max_drop_dart[["regression"]]
      Regressionmax_drop_dart <- Regressionmax_drop_dart[length(Regressionmax_drop_dart)]
    } else {
      Classifiermax_drop_dart <- max_drop_dart[["classifier"]]
      Regressionmax_drop_dart <- max_drop_dart[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmax_drop_dart <- max_drop_dart[length(max_drop_dart)]
      Classifiermax_drop_dart <- max_drop_dart[length(max_drop_dart)]
    } else {
      Classifiermax_drop_dart <- max_drop_dart
      Regressionmax_drop_dart <- max_drop_dart
    }
  }

  # skip_drop_dart
  if(is.list(skip_drop_dart)) {
    if(!GridTune) {
      Classifierskip_drop_dart <- skip_drop_dart[["classifier"]]
      Classifierskip_drop_dart <- Classifierskip_drop_dart[length(Classifierskip_drop_dart)]
      Regressionskip_drop_dart <- skip_drop_dart[["regression"]]
      Regressionskip_drop_dart <- Regressionskip_drop_dart[length(Regressionskip_drop_dart)]
    } else {
      Classifierskip_drop_dart <- skip_drop_dart[["classifier"]]
      Regressionskip_drop_dart <- skip_drop_dart[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionskip_drop_dart <- skip_drop_dart[length(skip_drop_dart)]
      Classifierskip_drop_dart <- skip_drop_dart[length(skip_drop_dart)]
    } else {
      Classifierskip_drop_dart <- skip_drop_dart
      Regressionskip_drop_dart <- skip_drop_dart
    }
  }

  # uniform_drop_dart
  if(is.list(uniform_drop_dart)) {
    if(!GridTune) {
      Classifieruniform_drop_dart <- uniform_drop_dart[["classifier"]]
      Classifieruniform_drop_dart <- Classifieruniform_drop_dart[length(Classifieruniform_drop_dart)]
      Regressionuniform_drop_dart <- uniform_drop_dart[["regression"]]
      Regressionuniform_drop_dart <- Regressionuniform_drop_dart[length(Regressionuniform_drop_dart)]
    } else {
      Classifieruniform_drop_dart <- uniform_drop_dart[["classifier"]]
      Regressionuniform_drop_dart <- uniform_drop_dart[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionuniform_drop_dart <- uniform_drop_dart[length(uniform_drop_dart)]
      Classifieruniform_drop_dart <- uniform_drop_dart[length(uniform_drop_dart)]
    } else {
      Classifieruniform_drop_dart <- uniform_drop_dart
      Regressionuniform_drop_dart <- uniform_drop_dart
    }
  }

  # top_rate_goss
  if(is.list(top_rate_goss)) {
    if(!GridTune) {
      Classifiertop_rate_goss <- top_rate_goss[["classifier"]]
      Classifiertop_rate_goss <- Classifiertop_rate_goss[length(Classifiertop_rate_goss)]
      Regressiontop_rate_goss <- top_rate_goss[["regression"]]
      Regressiontop_rate_goss <- Regressiontop_rate_goss[length(Regressiontop_rate_goss)]
    } else {
      Classifiertop_rate_goss <- top_rate_goss[["classifier"]]
      Regressiontop_rate_goss <- top_rate_goss[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressiontop_rate_goss <- top_rate_goss[length(top_rate_goss)]
      Classifiertop_rate_goss <- top_rate_goss[length(top_rate_goss)]
    } else {
      Classifiertop_rate_goss <- top_rate_goss
      Regressiontop_rate_goss <- top_rate_goss
    }
  }

  # other_rate_goss
  if(is.list(other_rate_goss)) {
    if(!GridTune) {
      Classifierother_rate_goss <- other_rate_goss[["classifier"]]
      Classifierother_rate_goss <- Classifierother_rate_goss[length(Classifierother_rate_goss)]
      Regressionother_rate_goss <- other_rate_goss[["regression"]]
      Regressionother_rate_goss <- Regressionother_rate_goss[length(Regressionother_rate_goss)]
    } else {
      Classifierother_rate_goss <- other_rate_goss[["classifier"]]
      Regressionother_rate_goss <- other_rate_goss[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionother_rate_goss <- other_rate_goss[length(other_rate_goss)]
      Classifierother_rate_goss <- other_rate_goss[length(other_rate_goss)]
    } else {
      Classifierother_rate_goss <- other_rate_goss
      Regressionother_rate_goss <- other_rate_goss
    }
  }

  # monotone_constraints
  if(is.list(monotone_constraints)) {
    if(!GridTune) {
      Classifiermonotone_constraints <- monotone_constraints[["classifier"]]
      Classifiermonotone_constraints <- Classifiermonotone_constraints[length(Classifiermonotone_constraints)]
      Regressionmonotone_constraints <- monotone_constraints[["regression"]]
      Regressionmonotone_constraints <- Regressionmonotone_constraints[length(Regressionmonotone_constraints)]
    } else {
      Classifiermonotone_constraints <- monotone_constraints[["classifier"]]
      Regressionmonotone_constraints <- monotone_constraints[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmonotone_constraints <- monotone_constraints[length(monotone_constraints)]
      Classifiermonotone_constraints <- monotone_constraints[length(monotone_constraints)]
    } else {
      Classifiermonotone_constraints <- monotone_constraints
      Regressionmonotone_constraints <- monotone_constraints
    }
  }

  # monotone_constraints_method
  if(is.list(monotone_constraints_method)) {
    if(!GridTune) {
      Classifiermonotone_constraints_method <- monotone_constraints_method[["classifier"]]
      Classifiermonotone_constraints_method <- Classifiermonotone_constraints_method[length(Classifiermonotone_constraints_method)]
      Regressionmonotone_constraints_method <- monotone_constraints_method[["regression"]]
      Regressionmonotone_constraints_method <- Regressionmonotone_constraints_method[length(Regressionmonotone_constraints_method)]
    } else {
      Classifiermonotone_constraints_method <- monotone_constraints_method[["classifier"]]
      Regressionmonotone_constraints_method <- monotone_constraints_method[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmonotone_constraints_method <- monotone_constraints_method[length(monotone_constraints_method)]
      Classifiermonotone_constraints_method <- monotone_constraints_method[length(monotone_constraints_method)]
    } else {
      Classifiermonotone_constraints_method <- monotone_constraints_method
      Regressionmonotone_constraints_method <- monotone_constraints_method
    }
  }

  # monotone_penalty
  if(is.list(monotone_penalty)) {
    if(!GridTune) {
      Classifiermonotone_penalty <- monotone_penalty[["classifier"]]
      Classifiermonotone_penalty <- Classifiermonotone_penalty[length(Classifiermonotone_penalty)]
      Regressionmonotone_penalty <- monotone_penalty[["regression"]]
      Regressionmonotone_penalty <- Regressionmonotone_penalty[length(Regressionmonotone_penalty)]
    } else {
      Classifiermonotone_penalty <- monotone_penalty[["classifier"]]
      Regressionmonotone_penalty <- monotone_penalty[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmonotone_penalty <- monotone_penalty[length(monotone_penalty)]
      Classifiermonotone_penalty <- monotone_penalty[length(monotone_penalty)]
    } else {
      Classifiermonotone_penalty <- monotone_penalty
      Regressionmonotone_penalty <- monotone_penalty
    }
  }

  # forcedsplits_filename
  if(is.list(forcedsplits_filename)) {
    if(!GridTune) {
      Classifierforcedsplits_filename <- forcedsplits_filename[["classifier"]]
      Classifierforcedsplits_filename <- Classifierforcedsplits_filename[length(Classifierforcedsplits_filename)]
      Regressionforcedsplits_filename <- forcedsplits_filename[["regression"]]
      Regressionforcedsplits_filename <- Regressionforcedsplits_filename[length(Regressionforcedsplits_filename)]
    } else {
      Classifierforcedsplits_filename <- forcedsplits_filename[["classifier"]]
      Regressionforcedsplits_filename <- forcedsplits_filename[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionforcedsplits_filename <- forcedsplits_filename[length(forcedsplits_filename)]
      Classifierforcedsplits_filename <- forcedsplits_filename[length(forcedsplits_filename)]
    } else {
      Classifierforcedsplits_filename <- forcedsplits_filename
      Regressionforcedsplits_filename <- forcedsplits_filename
    }
  }

  # refit_decay_rate
  if(is.list(refit_decay_rate)) {
    if(!GridTune) {
      Classifierrefit_decay_rate <- refit_decay_rate[["classifier"]]
      Classifierrefit_decay_rate <- Classifierrefit_decay_rate[length(Classifierrefit_decay_rate)]
      Regressionrefit_decay_rate <- refit_decay_rate[["regression"]]
      Regressionrefit_decay_rate <- Regressionrefit_decay_rate[length(Regressionrefit_decay_rate)]
    } else {
      Classifierrefit_decay_rate <- refit_decay_rate[["classifier"]]
      Regressionrefit_decay_rate <- refit_decay_rate[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionrefit_decay_rate <- refit_decay_rate[length(refit_decay_rate)]
      Classifierrefit_decay_rate <- refit_decay_rate[length(refit_decay_rate)]
    } else {
      Classifierrefit_decay_rate <- refit_decay_rate
      Regressionrefit_decay_rate <- refit_decay_rate
    }
  }

  # path_smooth
  if(is.list(path_smooth)) {
    if(!GridTune) {
      Classifierpath_smooth <- path_smooth[["classifier"]]
      Classifierpath_smooth <- Classifierpath_smooth[length(Classifierpath_smooth)]
      Regressionpath_smooth <- path_smooth[["regression"]]
      Regressionpath_smooth <- Regressionpath_smooth[length(Regressionpath_smooth)]
    } else {
      Classifierpath_smooth <- path_smooth[["classifier"]]
      Regressionpath_smooth <- path_smooth[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionpath_smooth <- path_smooth[length(path_smooth)]
      Classifierpath_smooth <- path_smooth[length(path_smooth)]
    } else {
      Classifierpath_smooth <- path_smooth
      Regressionpath_smooth <- path_smooth
    }
  }

  # max_bin
  if(is.list(max_bin)) {
    if(!GridTune) {
      Classifiermax_bin <- max_bin[["classifier"]]
      Classifiermax_bin <- Classifiermax_bin[length(Classifiermax_bin)]
      Regressionmax_bin <- max_bin[["regression"]]
      Regressionmax_bin <- Regressionmax_bin[length(Regressionmax_bin)]
    } else {
      Classifiermax_bin <- max_bin[["classifier"]]
      Regressionmax_bin <- max_bin[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmax_bin <- max_bin[length(max_bin)]
      Classifiermax_bin <- max_bin[length(max_bin)]
    } else {
      Classifiermax_bin <- max_bin
      Regressionmax_bin <- max_bin
    }
  }

  # min_data_in_bin
  if(is.list(min_data_in_bin)) {
    if(!GridTune) {
      Classifiermin_data_in_bin <- min_data_in_bin[["classifier"]]
      Classifiermin_data_in_bin <- Classifiermin_data_in_bin[length(Classifiermin_data_in_bin)]
      Regressionmin_data_in_bin <- min_data_in_bin[["regression"]]
      Regressionmin_data_in_bin <- Regressionmin_data_in_bin[length(Regressionmin_data_in_bin)]
    } else {
      Classifiermin_data_in_bin <- min_data_in_bin[["classifier"]]
      Regressionmin_data_in_bin <- min_data_in_bin[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionmin_data_in_bin <- min_data_in_bin[length(min_data_in_bin)]
      Classifiermin_data_in_bin <- min_data_in_bin[length(min_data_in_bin)]
    } else {
      Classifiermin_data_in_bin <- min_data_in_bin
      Regressionmin_data_in_bin <- min_data_in_bin
    }
  }

  # data_random_seed
  if(is.list(data_random_seed)) {
    if(!GridTune) {
      Classifierdata_random_seed <- data_random_seed[["classifier"]]
      Classifierdata_random_seed <- Classifierdata_random_seed[length(Classifierdata_random_seed)]
      Regressiondata_random_seed <- data_random_seed[["regression"]]
      Regressiondata_random_seed <- Regressiondata_random_seed[length(Regressiondata_random_seed)]
    } else {
      Classifierdata_random_seed <- data_random_seed[["classifier"]]
      Regressiondata_random_seed <- data_random_seed[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressiondata_random_seed <- data_random_seed[length(data_random_seed)]
      Classifierdata_random_seed <- data_random_seed[length(data_random_seed)]
    } else {
      Classifierdata_random_seed <- data_random_seed
      Regressiondata_random_seed <- data_random_seed
    }
  }

  # is_enable_sparse
  if(is.list(is_enable_sparse)) {
    if(!GridTune) {
      Classifieris_enable_sparse <- is_enable_sparse[["classifier"]]
      Classifieris_enable_sparse <- Classifieris_enable_sparse[length(Classifieris_enable_sparse)]
      Regressionis_enable_sparse <- is_enable_sparse[["regression"]]
      Regressionis_enable_sparse <- Regressionis_enable_sparse[length(Regressionis_enable_sparse)]
    } else {
      Classifieris_enable_sparse <- is_enable_sparse[["classifier"]]
      Regressionis_enable_sparse <- is_enable_sparse[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionis_enable_sparse <- is_enable_sparse[length(is_enable_sparse)]
      Classifieris_enable_sparse <- is_enable_sparse[length(is_enable_sparse)]
    } else {
      Classifieris_enable_sparse <- is_enable_sparse
      Regressionis_enable_sparse <- is_enable_sparse
    }
  }

  # enable_bundle
  if(is.list(enable_bundle)) {
    if(!GridTune) {
      Classifierenable_bundle <- enable_bundle[["classifier"]]
      Classifierenable_bundle <- Classifierenable_bundle[length(Classifierenable_bundle)]
      Regressionenable_bundle <- enable_bundle[["regression"]]
      Regressionenable_bundle <- Regressionenable_bundle[length(Regressionenable_bundle)]
    } else {
      Classifierenable_bundle <- enable_bundle[["classifier"]]
      Regressionenable_bundle <- enable_bundle[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionenable_bundle <- enable_bundle[length(enable_bundle)]
      Classifierenable_bundle <- enable_bundle[length(enable_bundle)]
    } else {
      Classifierenable_bundle <- enable_bundle
      Regressionenable_bundle <- enable_bundle
    }
  }

  # use_missing
  if(is.list(use_missing)) {
    if(!GridTune) {
      Classifieruse_missing <- use_missing[["classifier"]]
      Classifieruse_missing <- Classifieruse_missing[length(Classifieruse_missing)]
      Regressionuse_missing <- use_missing[["regression"]]
      Regressionuse_missing <- Regressionuse_missing[length(Regressionuse_missing)]
    } else {
      Classifieruse_missing <- use_missing[["classifier"]]
      Regressionuse_missing <- use_missing[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionuse_missing <- use_missing[length(use_missing)]
      Classifieruse_missing <- use_missing[length(use_missing)]
    } else {
      Classifieruse_missing <- use_missing
      Regressionuse_missing <- use_missing
    }
  }

  # zero_as_missing
  if(is.list(zero_as_missing)) {
    if(!GridTune) {
      Classifierzero_as_missing <- zero_as_missing[["classifier"]]
      Classifierzero_as_missing <- Classifierzero_as_missing[length(Classifierzero_as_missing)]
      Regressionzero_as_missing <- zero_as_missing[["regression"]]
      Regressionzero_as_missing <- Regressionzero_as_missing[length(Regressionzero_as_missing)]
    } else {
      Classifierzero_as_missing <- zero_as_missing[["classifier"]]
      Regressionzero_as_missing <- zero_as_missing[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionzero_as_missing <- zero_as_missing[length(zero_as_missing)]
      Classifierzero_as_missing <- zero_as_missing[length(zero_as_missing)]
    } else {
      Classifierzero_as_missing <- zero_as_missing
      Regressionzero_as_missing <- zero_as_missing
    }
  }

  # two_round
  if(is.list(two_round)) {
    if(!GridTune) {
      Classifiertwo_round <- two_round[["classifier"]]
      Classifiertwo_round <- Classifiertwo_round[length(Classifiertwo_round)]
      Regressiontwo_round <- two_round[["regression"]]
      Regressiontwo_round <- Regressiontwo_round[length(Regressiontwo_round)]
    } else {
      Classifiertwo_round <- two_round[["classifier"]]
      Regressiontwo_round <- two_round[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressiontwo_round <- two_round[length(two_round)]
      Classifiertwo_round <- two_round[length(two_round)]
    } else {
      Classifiertwo_round <- two_round
      Regressiontwo_round <- two_round
    }
  }

  # convert_model
  if(is.list(convert_model)) {
    if(!GridTune) {
      Classifierconvert_model <- convert_model[["classifier"]]
      Classifierconvert_model <- Classifierconvert_model[length(Classifierconvert_model)]
      Regressionconvert_model <- convert_model[["regression"]]
      Regressionconvert_model <- Regressionconvert_model[length(Regressionconvert_model)]
    } else {
      Classifierconvert_model <- convert_model[["classifier"]]
      Regressionconvert_model <- convert_model[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionconvert_model <- convert_model[length(convert_model)]
      Classifierconvert_model <- convert_model[length(convert_model)]
    } else {
      Classifierconvert_model <- convert_model
      Regressionconvert_model <- convert_model
    }
  }

  # convert_model_language
  if(is.list(convert_model_language)) {
    if(!GridTune) {
      Classifierconvert_model_language <- convert_model_language[["classifier"]]
      Classifierconvert_model_language <- Classifierconvert_model_language[length(Classifierconvert_model_language)]
      Regressionconvert_model_language <- convert_model_language[["regression"]]
      Regressionconvert_model_language <- Regressionconvert_model_language[length(Regressionconvert_model_language)]
    } else {
      Classifierconvert_model_language <- convert_model_language[["classifier"]]
      Regressionconvert_model_language <- convert_model_language[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionconvert_model_language <- convert_model_language[length(convert_model_language)]
      Classifierconvert_model_language <- convert_model_language[length(convert_model_language)]
    } else {
      Classifierconvert_model_language <- convert_model_language
      Regressionconvert_model_language <- convert_model_language
    }
  }

  # boost_from_average
  if(is.list(boost_from_average)) {
    if(!GridTune) {
      Classifierboost_from_average <- boost_from_average[["classifier"]]
      Classifierboost_from_average <- Classifierboost_from_average[length(Classifierboost_from_average)]
      Regressionboost_from_average <- boost_from_average[["regression"]]
      Regressionboost_from_average <- Regressionboost_from_average[length(Regressionboost_from_average)]
    } else {
      Classifierboost_from_average <- boost_from_average[["classifier"]]
      Regressionboost_from_average <- boost_from_average[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionboost_from_average <- boost_from_average[length(boost_from_average)]
      Classifierboost_from_average <- boost_from_average[length(boost_from_average)]
    } else {
      Classifierboost_from_average <- boost_from_average
      Regressionboost_from_average <- boost_from_average
    }
  }

  # is_unbalance
  if(is.list(is_unbalance)) {
    if(!GridTune) {
      Classifieris_unbalance <- is_unbalance[["classifier"]]
      Classifieris_unbalance <- Classifieris_unbalance[length(Classifieris_unbalance)]
      Regressionis_unbalance <- is_unbalance[["regression"]]
      Regressionis_unbalance <- Regressionis_unbalance[length(Regressionis_unbalance)]
    } else {
      Classifieris_unbalance <- is_unbalance[["classifier"]]
      Regressionis_unbalance <- is_unbalance[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionis_unbalance <- is_unbalance[length(is_unbalance)]
      Classifieris_unbalance <- is_unbalance[length(is_unbalance)]
    } else {
      Classifieris_unbalance <- is_unbalance
      Regressionis_unbalance <- is_unbalance
    }
  }

  # scale_pos_weight
  if(is.list(scale_pos_weight)) {
    if(!GridTune) {
      Classifierscale_pos_weight <- scale_pos_weight[["classifier"]]
      Classifierscale_pos_weight <- Classifierscale_pos_weight[length(Classifierscale_pos_weight)]
      Regressionscale_pos_weight <- scale_pos_weight[["regression"]]
      Regressionscale_pos_weight <- Regressionscale_pos_weight[length(Regressionscale_pos_weight)]
    } else {
      Classifierscale_pos_weight <- scale_pos_weight[["classifier"]]
      Regressionscale_pos_weight <- scale_pos_weight[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionscale_pos_weight <- scale_pos_weight[length(scale_pos_weight)]
      Classifierscale_pos_weight <- scale_pos_weight[length(scale_pos_weight)]
    } else {
      Classifierscale_pos_weight <- scale_pos_weight
      Regressionscale_pos_weight <- scale_pos_weight
    }
  }

  # is_provide_training_metric
  if(is.list(is_provide_training_metric)) {
    if(!GridTune) {
      Classifieris_provide_training_metric <- is_provide_training_metric[["classifier"]]
      Classifieris_provide_training_metric <- Classifieris_provide_training_metric[length(Classifieris_provide_training_metric)]
      Regressionis_provide_training_metric <- is_provide_training_metric[["regression"]]
      Regressionis_provide_training_metric <- Regressionis_provide_training_metric[length(Regressionis_provide_training_metric)]
    } else {
      Classifieris_provide_training_metric <- is_provide_training_metric[["classifier"]]
      Regressionis_provide_training_metric <- is_provide_training_metric[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionis_provide_training_metric <- is_provide_training_metric[length(is_provide_training_metric)]
      Classifieris_provide_training_metric <- is_provide_training_metric[length(is_provide_training_metric)]
    } else {
      Classifieris_provide_training_metric <- is_provide_training_metric
      Regressionis_provide_training_metric <- is_provide_training_metric
    }
  }

  # eval_at
  if(is.list(eval_at)) {
    if(!GridTune) {
      Classifiereval_at <- eval_at[["classifier"]]
      Classifiereval_at <- Classifiereval_at[length(Classifiereval_at)]
      Regressioneval_at <- eval_at[["regression"]]
      Regressioneval_at <- Regressioneval_at[length(Regressioneval_at)]
    } else {
      Classifiereval_at <- eval_at[["classifier"]]
      Regressioneval_at <- eval_at[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressioneval_at <- eval_at[length(eval_at)]
      Classifiereval_at <- eval_at[length(eval_at)]
    } else {
      Classifiereval_at <- eval_at
      Regressioneval_at <- eval_at
    }
  }

  # num_machines
  if(is.list(num_machines)) {
    if(!GridTune) {
      Classifiernum_machines <- num_machines[["classifier"]]
      Classifiernum_machines <- Classifiernum_machines[length(Classifiernum_machines)]
      Regressionnum_machines <- num_machines[["regression"]]
      Regressionnum_machines <- Regressionnum_machines[length(Regressionnum_machines)]
    } else {
      Classifiernum_machines <- num_machines[["classifier"]]
      Regressionnum_machines <- num_machines[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionnum_machines <- num_machines[length(num_machines)]
      Classifiernum_machines <- num_machines[length(num_machines)]
    } else {
      Classifiernum_machines <- num_machines
      Regressionnum_machines <- num_machines
    }
  }

  # gpu_platform_id
  if(is.list(gpu_platform_id)) {
    if(!GridTune) {
      Classifiergpu_platform_id <- gpu_platform_id[["classifier"]]
      Classifiergpu_platform_id <- Classifiergpu_platform_id[length(Classifiergpu_platform_id)]
      Regressiongpu_platform_id <- gpu_platform_id[["regression"]]
      Regressiongpu_platform_id <- Regressiongpu_platform_id[length(Regressiongpu_platform_id)]
    } else {
      Classifiergpu_platform_id <- gpu_platform_id[["classifier"]]
      Regressiongpu_platform_id <- gpu_platform_id[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressiongpu_platform_id <- gpu_platform_id[length(gpu_platform_id)]
      Classifiergpu_platform_id <- gpu_platform_id[length(gpu_platform_id)]
    } else {
      Classifiergpu_platform_id <- gpu_platform_id
      Regressiongpu_platform_id <- gpu_platform_id
    }
  }

  # gpu_device_id
  if(is.list(gpu_device_id)) {
    if(!GridTune) {
      Classifiergpu_device_id <- gpu_device_id[["classifier"]]
      Classifiergpu_device_id <- Classifiergpu_device_id[length(Classifiergpu_device_id)]
      Regressiongpu_device_id <- gpu_device_id[["regression"]]
      Regressiongpu_device_id <- Regressiongpu_device_id[length(Regressiongpu_device_id)]
    } else {
      Classifiergpu_device_id <- gpu_device_id[["classifier"]]
      Regressiongpu_device_id <- gpu_device_id[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressiongpu_device_id <- gpu_device_id[length(gpu_device_id)]
      Classifiergpu_device_id <- gpu_device_id[length(gpu_device_id)]
    } else {
      Classifiergpu_device_id <- gpu_device_id
      Regressiongpu_device_id <- gpu_device_id
    }
  }

  # gpu_use_dp
  if(is.list(gpu_use_dp)) {
    if(!GridTune) {
      Classifiergpu_use_dp <- gpu_use_dp[["classifier"]]
      Classifiergpu_use_dp <- Classifiergpu_use_dp[length(Classifiergpu_use_dp)]
      Regressiongpu_use_dp <- gpu_use_dp[["regression"]]
      Regressiongpu_use_dp <- Regressiongpu_use_dp[length(Regressiongpu_use_dp)]
    } else {
      Classifiergpu_use_dp <- gpu_use_dp[["classifier"]]
      Regressiongpu_use_dp <- gpu_use_dp[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressiongpu_use_dp <- gpu_use_dp[length(gpu_use_dp)]
      Classifiergpu_use_dp <- gpu_use_dp[length(gpu_use_dp)]
    } else {
      Classifiergpu_use_dp <- gpu_use_dp
      Regressiongpu_use_dp <- gpu_use_dp
    }
  }

  # num_gpu
  if(is.list(num_gpu)) {
    if(!GridTune) {
      Classifiernum_gpu <- num_gpu[["classifier"]]
      Classifiernum_gpu <- Classifiernum_gpu[length(Classifiernum_gpu)]
      Regressionnum_gpu <- num_gpu[["regression"]]
      Regressionnum_gpu <- Regressionnum_gpu[length(Regressionnum_gpu)]
    } else {
      Classifiernum_gpu <- num_gpu[["classifier"]]
      Regressionnum_gpu <- num_gpu[["regression"]]
    }
  } else {
    if(!GridTune) {
      Regressionnum_gpu <- num_gpu[length(num_gpu)]
      Classifiernum_gpu <- num_gpu[length(num_gpu)]
    } else {
      Classifiernum_gpu <- num_gpu
      Regressionnum_gpu <- num_gpu
    }
  }

  # Ensure Paths and metadata_path exists ----
  if(!is.null(Paths)) if(!dir.exists(Paths)) dir.create(Paths)
  if(is.null(MetaDataPaths)) MetaDataPaths <- Paths else if(!dir.exists(MetaDataPaths)) dir.create(MetaDataPaths)

  # Data.table check ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(ValidationData)) if(!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)
  if(!is.null(TestData)) if(!data.table::is.data.table(TestData)) data.table::setDT(TestData)

  # IDcols to Names ----
  if(!is.null(IDcols)) if(is.numeric(IDcols) || is.integer(IDcols)) IDcols <- names(data)[IDcols]
  IDcols <- c(IDcols, TargetColumnName)

  # Primary Date Column ----
  if(is.numeric(PrimaryDateColumn) || is.integer(PrimaryDateColumn)) PrimaryDateColumn <- names(data)[PrimaryDateColumn]

  # FeatureColumnNames ----
  if(is.numeric(FeatureColNames) || is.integer(FeatureColNames)) FeatureNames <- names(data)[FeatureColNames] else FeatureNames <- FeatureColNames

  # Add target bucket column ----
  looper <- rev(seq_len(length(Buckets) + 1L))
  if(length(Buckets) == 1L) {
    data[, Target_Buckets := data.table::fifelse(get(TargetColumnName) <= eval(Buckets[1L]), 0, 1)]
  } else {
    for(i in seq_len(length(Buckets) + 1L)) {
      if(i == 1L) {
        data[which(data[[eval(TargetColumnName)]] <= Buckets[i]), Target_Buckets  := as.factor(Buckets[i])]
      } else if(i == length(Buckets) + 1L) {
        if(data[which(data[[eval(TargetColumnName)]] > Buckets[i - 1]), .N] != 0) {
          data[which(data[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(paste0(Buckets[i - 1], '+'))]
        } else {
          Buckets <- Buckets[!Buckets %in% Buckets[i]]
          looper <- looper[-1L]
        }
      } else {
        data[which(data[[eval(TargetColumnName)]] <= Buckets[i] & data[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(Buckets[i])]
      }
    }
  }

  # Store looper -----
  ArgsList[["looper"]] <- looper

  # Add target bucket column ----
  if(!is.null(ValidationData)) {
    if(length(Buckets) == 1L) {
      ValidationData[, Target_Buckets := data.table::fifelse(get(TargetColumnName) <= eval(Buckets[1L]), 0, 1)]
    } else {
      for(i in seq_len(length(Buckets) + 1L)) {
        if(i == 1) {
          ValidationData[which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i]), Target_Buckets := as.factor(Buckets[i])]
        } else if(i == length(Buckets) + 1L) {
          ValidationData[which(ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(paste0(Buckets[i - 1], '+'))]
        } else {
          if(data[which(data[[eval(TargetColumnName)]] > Buckets[i - 1]), .N] != 0) {
            ValidationData[which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i] & ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(Buckets[i])]
          }
        }
      }
    }
  }

  # Add target bucket column ----
  if(!is.null(TestData)) {
    if(length(Buckets) == 1L) {
      TestData[, Target_Buckets := data.table::fifelse(get(TargetColumnName) <= eval(Buckets[1L]), 0, 1)]
    } else {
      for(i in seq_len(length(Buckets) + 1L)) {
        if(i == 1) {
          TestData[which(TestData[[eval(TargetColumnName)]] <= Buckets[i]), Target_Buckets := as.factor(Buckets[i])]
        } else if(i == length(Buckets) + 1L) {
          TestData[which(TestData[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(paste0(Buckets[i - 1L], '+'))]
        } else {
          if(data[which(data[[eval(TargetColumnName)]] > Buckets[i - 1]), .N] != 0) {
            TestData[which(TestData[[eval(TargetColumnName)]] <= Buckets[i] & TestData[[eval(TargetColumnName)]] > Buckets[i - 1L]), Target_Buckets := as.factor(Buckets[i])]
          }
        }
      }
    }
  }

  # AutoDataPartition if Validation and TestData are NULL----
  if(is.null(ValidationData) && is.null(TestData) && !TrainOnFull) {
    DataSets <- AutoDataPartition(
      data = data,
      NumDataSets = 3L,
      Ratios = SplitRatios,
      PartitionType = 'random',
      StratifyColumnNames = 'Target_Buckets',
      TimeColumnName = NULL)
    data <- DataSets$TrainData
    ValidationData <- DataSets$ValidationData
    TestData <- DataSets$TestData
    rm(DataSets)

    # Add target bucket column----
    if(!is.null(ValidationData)) {
      if(length(Buckets) == 1L) {
        ValidationData[, Target_Buckets := data.table::fifelse(get(TargetColumnName) <= eval(Buckets[1L]), 0, 1)]
      } else {
        for(i in seq_len(length(Buckets) + 1L)) {
          if(i == 1) {
            ValidationData[which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i]), Target_Buckets := as.factor(Buckets[i])]
          } else if(i == length(Buckets) + 1L) {
            ValidationData[which(ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(paste0(Buckets[i - 1], '+'))]
          } else {
            if(data[which(data[[eval(TargetColumnName)]] > Buckets[i - 1]), .N] != 0) {
              ValidationData[which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i] & ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(Buckets[i])]
            }
          }
        }
      }
    }

    # Add target bucket column----
    if(!is.null(TestData)) {
      if(length(Buckets) == 1L) {
        TestData[, Target_Buckets := data.table::fifelse(get(TargetColumnName) <= eval(Buckets[1L]), 0, 1)]
      } else {
        for(i in seq_len(length(Buckets) + 1L)) {
          if(i == 1) {
            TestData[which(TestData[[eval(TargetColumnName)]] <= Buckets[i]), Target_Buckets := as.factor(Buckets[i])]
          } else if(i == length(Buckets) + 1L) {
            TestData[which(TestData[[eval(TargetColumnName)]] > Buckets[i - 1]), Target_Buckets := as.factor(paste0(Buckets[i - 1L], '+'))]
          } else {
            if(data[which(data[[eval(TargetColumnName)]] > Buckets[i - 1]), .N] != 0) {
              TestData[which(TestData[[eval(TargetColumnName)]] <= Buckets[i] & TestData[[eval(TargetColumnName)]] > Buckets[i - 1L]), Target_Buckets := as.factor(Buckets[i])]
            }
          }
        }
      }
    }
  } else if(TrainOnFull) {
    ValidationData <- NULL
    TestData <- NULL
  }

  # Begin classification model building ----
  if(length(Buckets) == 1L) {
    ClassifierModel <- AutoLightGBMClassifier(

      # New
      OutputSelection = c("Importances", "EvalMetrics"),
      WeightsColumnName = WeightsColumnName,
      SaveInfoToPDF = FALSE,
      DebugMode = DebugMode,

      # general args
      TrainOnFull = TrainOnFull,
      PassInGrid = PassInGrid,
      NThreads = NThreads,
      model_path = Paths,
      metadata_path = MetaDataPaths,
      ModelID = ModelID,

      # options
      ReturnModelObjects = TRUE,
      EncodingMethod = EncodingMethod,
      ReturnFactorLevels = TRUE,
      Verbose = 1L,
      NumOfParDepPlots = NumOfParDepPlots,
      SaveModelObjects = SaveModelObjects,

      # data args
      TargetColumnName = "Target_Buckets",
      data = data,
      ValidationData = ValidationData,
      TestData = TestData,
      FeatureColNames = FeatureNames,
      IDcols = IDcols,

      # Grid tuning args
      GridTune = GridTune,
      BaselineComparison = BaselineComparison,
      MaxModelsInGrid = MaxModelsInGrid,
      MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
      MaxRunMinutes = MaxRunMinutes,

      # Core parameters
      input_model = Classifierinput_model,
      task = Classifiertask,
      device_type = Classifierdevice_type,
      objective = Classifierobjective,
      metric = Classifiermetric,
      boosting = Classifierboosting,
      LinearTree = ClassifierLinearTree,
      Trees = ClassifierTrees,
      eta = Classifiereta,
      num_leaves = Classifiernum_leaves,
      deterministic = Classifierdeterministic,

      # Learning Parameters
      force_col_wise = Classifierforce_col_wise,
      force_row_wise = Classifierforce_row_wise,
      max_depth = Classifiermax_depth,
      min_data_in_leaf = Classifiermin_data_in_leaf,
      min_sum_hessian_in_leaf = Classifiermin_sum_hessian_in_leaf,
      bagging_freq = Classifierbagging_freq,
      bagging_fraction = Classifierbagging_fraction,
      feature_fraction = Classifierfeature_fraction,
      feature_fraction_bynode = Classifierfeature_fraction_bynode,
      lambda_l1 = Classifierlambda_l1,
      lambda_l2 = Classifierlambda_l2,
      extra_trees = Classifierextra_trees,
      early_stopping_round = Classifierearly_stopping_round,
      first_metric_only = Classifierfirst_metric_only,
      max_delta_step = Classifiermax_delta_step,
      linear_lambda = Classifierlinear_lambda,
      min_gain_to_split = Classifiermin_gain_to_split,
      drop_rate_dart = Classifierdrop_rate_dart,
      max_drop_dart = Classifiermax_drop_dart,
      skip_drop_dart = Classifierskip_drop_dart,
      uniform_drop_dart = Classifieruniform_drop_dart,
      top_rate_goss = Classifiertop_rate_goss,
      other_rate_goss = Classifierother_rate_goss,
      monotone_constraints = Classifiermonotone_constraints,
      monotone_constraints_method = Classifiermonotone_constraints_method,
      monotone_penalty = Classifiermonotone_penalty,
      forcedsplits_filename = Classifierforcedsplits_filename,
      refit_decay_rate = Classifierrefit_decay_rate,
      path_smooth = Classifierpath_smooth,

      # IO Dataset Parameters
      max_bin = Classifiermax_bin,
      min_data_in_bin = Classifiermin_data_in_bin,
      data_random_seed = Classifierdata_random_seed,
      is_enable_sparse = Classifieris_enable_sparse,
      enable_bundle = Classifierenable_bundle,
      use_missing = Classifieruse_missing,
      zero_as_missing = Classifierzero_as_missing,
      two_round = Classifiertwo_round,

      # Convert Parameters
      convert_model = Classifierconvert_model,
      convert_model_language = Classifierconvert_model_language,

      # Objective Parameters
      boost_from_average = Classifierboost_from_average,
      is_unbalance = Classifieris_unbalance,
      scale_pos_weight = Classifierscale_pos_weight,

      # Metric Parameters (metric is in Core)
      is_provide_training_metric = Classifieris_provide_training_metric,
      eval_at = Classifiereval_at,

      # Network Parameters
      num_machines = Classifiernum_machines,

      # GPU Parameters
      gpu_platform_id = Classifiergpu_platform_id,
      gpu_device_id = Classifiergpu_device_id,
      gpu_use_dp = Classifiergpu_use_dp,
      num_gpu = Classifiernum_gpu)

  } else {
    ClassifierModel <- RemixAutoML::AutoLightGBMMultiClass(

      # Udpated args
      OutputSelection = c("Importances", "EvalMetrics"),
      WeightsColumnName = WeightsColumnName,
      DebugMode = DebugMode,

      # type
      grid_eval_metric = grid_eval_metric,

      # general args
      TrainOnFull = TrainOnFull,
      PassInGrid = PassInGrid,
      NThreads = NThreads,
      model_path = Paths,
      metadata_path = MetaDataPaths,
      ModelID = ModelID,
      EncodingMethod = EncodingMethod,

      # options
      ReturnModelObjects = TRUE,
      ReturnFactorLevels = TRUE,
      SaveInfoToPDF = FALSE,
      NumOfParDepPlots = NumOfParDepPlots,
      SaveModelObjects = SaveModelObjects,
      Verbose = 1L,

      # data args
      data = data,
      ValidationData = ValidationData,
      TestData = TestData,
      TargetColumnName = "Target_Buckets",
      FeatureColNames = FeatureNames,
      PrimaryDateColumn = PrimaryDateColumn,
      IDcols = IDcols,

      # Grid tuning args
      GridTune = GridTune,
      BaselineComparison = BaselineComparison,
      MaxModelsInGrid = MaxModelsInGrid,
      MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
      MaxRunMinutes = MaxRunMinutes,

      # Core parameters
      input_model = Classifierinput_model,
      task = Classifiertask,
      device_type = Classifierdevice_type,
      objective = Classifierobjective,
      multi_error_top_k = Classifiermulti_error_top_k,
      metric = Classifiermetric,
      boosting = Classifierboosting,
      LinearTree = ClassifierLinearTree,
      Trees = ClassifierTrees,
      eta = Classifiereta,
      num_leaves = Classifiernum_leaves,
      deterministic = Classifierdeterministic,

      # Learning Parameters
      force_col_wise = Classifierforce_col_wise,
      force_row_wise = Classifierforce_row_wise,
      max_depth = Classifiermax_depth,
      min_data_in_leaf = Classifiermin_data_in_leaf,
      min_sum_hessian_in_leaf = Classifiermin_sum_hessian_in_leaf,
      bagging_freq = Classifierbagging_freq,
      bagging_fraction = Classifierbagging_fraction,
      feature_fraction = Classifierfeature_fraction,
      feature_fraction_bynode = Classifierfeature_fraction_bynode,
      lambda_l1 = Classifierlambda_l1,
      lambda_l2 = Classifierlambda_l2,
      extra_trees = Classifierextra_trees,
      early_stopping_round = Classifierearly_stopping_round,
      first_metric_only = Classifierfirst_metric_only,
      max_delta_step = Classifiermax_delta_step,
      linear_lambda = Classifierlinear_lambda,
      min_gain_to_split = Classifiermin_gain_to_split,
      drop_rate_dart = Classifierdrop_rate_dart,
      max_drop_dart = Classifiermax_drop_dart,
      skip_drop_dart = Classifierskip_drop_dart,
      uniform_drop_dart = Classifieruniform_drop_dart,
      top_rate_goss = Classifiertop_rate_goss,
      other_rate_goss = Classifierother_rate_goss,
      monotone_constraints = Classifiermonotone_constraints,
      monotone_constraints_method = Classifiermonotone_constraints_method,
      monotone_penalty = Classifiermonotone_penalty,
      forcedsplits_filename = Classifierforcedsplits_filename,
      refit_decay_rate = Classifierrefit_decay_rate,
      path_smooth = Classifierpath_smooth,

      # IO Dataset Parameters
      max_bin = Classifiermax_bin,
      min_data_in_bin = Classifiermin_data_in_bin,
      data_random_seed = Classifierdata_random_seed,
      is_enable_sparse = Classifieris_enable_sparse,
      enable_bundle = Classifierenable_bundle,
      use_missing = Classifieruse_missing,
      zero_as_missing = Classifierzero_as_missing,
      two_round = Classifiertwo_round,

      # Convert Parameters
      convert_model = Classifierconvert_model,
      convert_model_language = Classifierconvert_model_language,

      # Objective Parameters
      boost_from_average = Classifierboost_from_average,
      is_unbalance = Classifieris_unbalance,
      scale_pos_weight = Classifierscale_pos_weight,

      # Metric Parameters (metric is in Core)
      is_provide_training_metric = Classifieris_provide_training_metric,
      eval_at = Classifiereval_at,

      # Network Parameters
      num_machines = Classifiernum_machines,

      # GPU Parameters
      gpu_platform_id = Classifiergpu_platform_id,
      gpu_device_id = Classifiergpu_device_id,
      gpu_use_dp = Classifiergpu_use_dp,
      num_gpu = Classifiernum_gpu)
  }

  # Store metadata ----
  if(DebugMode) print("Store metadata")
  ModelList <- list()
  ClassModel <- ClassifierModel$Model
  ModelList[["ClassificationModel"]] <- ClassModel
  ClassEvaluationMetrics <- ClassifierModel$EvaluationMetrics
  VariableImportance <- ClassifierModel$VariableImportance
  if(length(Buckets) > 1L) {
    TargetLevels <- ClassifierModel$TargetLevels
    ArgsList[["TargetLevels"]] <- TargetLevels
  } else {
    TargetLevels <- NULL
    ArgsList[["TargetLevels"]] <- NULL
  }
  FactorLevelsListOutput <- ClassifierModel$FactorLevels
  if(!is.null(FactorLevelsListOutput)) FactorLevelsList <- FactorLevelsListOutput else FactorLevelsList <- NULL
  ArgsList[["Class_FactorLevelsList"]] <- FactorLevelsList
  rm(ClassifierModel)

  # Define args ----
  if(DebugMode) print("Define args")
  if(length(Buckets) == 1L) {
    TargetType <- "Classification"
  } else {
    TargetType <- "Multiclass"
  }

  # Model Scoring ----
  if(DebugMode) {
    print("Model Scoring")
    if(!is.null(TestData)) print(data.table::copy(TestData)) else if(!is.null(ValidationData)) print(data.table::copy(ValidationData)) else print(data.table::copy(data))
  }
  if(!TrainOnFull || !is.null(ValidationData)) {
    if(is.null(TestData)) ValTrue <<- TRUE else ValTrue <<- FALSE
    temp <- AutoLightGBMScoring(
      ReturnShapValues = FALSE,
      EncodingMethod = EncodingMethod,
      TargetType = TargetType,
      ScoringData = if(!is.null(TestData)) data.table::copy(TestData) else if(!is.null(ValidationData)) data.table::copy(ValidationData) else data.table::copy(data),
      FeatureColumnNames = FeatureNames,
      IDcols = c(IDcols, "Target_Buckets"),
      FactorLevelsList = FactorLevelsList,
      TargetLevels = TargetLevels,
      OneHot = FALSE,
      ModelObject = ClassModel,
      ModelPath = if(!is.null(ClassModel)) NULL else Paths,
      ModelID = ModelID,
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
      MDP_MissNum = -1)

    # Nuance
    if(TargetType != "Classification") {
      TestData <- cbind(temp, TestData[, .SD, .SDcols = c(setdiff(names(TestData),names(temp)))])
    } else {
      TestData <- temp
      rm(temp)
    }

    # Rename data output
    if(ValTrue) {
      ValidationData <- TestData
      TestData <- NULL
    }

    # Change name for classification----
    if(DebugMode) print('Change name for classification----')
    if(tolower(TargetType) == 'Classification') {
      if(!is.null(TestData)) {
        data.table::setnames(TestData, 'Predictions', 'Predictions_C1')
        TestData[, Predictions_C0 := 1 - Predictions_C1]
        data.table::setcolorder(TestData, c(ncol(TestData), 1L, 2L:(ncol(TestData) - 1L)))
      } else if(!is.null(ValidationData)) {
        data.table::setnames(ValidationData, 'Predictions', 'Predictions_C1')
        ValidationData[, Predictions_C0 := 1 - Predictions_C1]
        data.table::setcolorder(ValidationData, c(ncol(ValidationData), 1L, 2L:(ncol(ValidationData) - 1L)))
      }
    }

    # Change Name of Predicted MultiClass Column----
    if(DebugMode) print('Change Name of Predicted MultiClass Column----')
    if(tolower(TargetType) != 'classification') {
      if(!is.null(TestData)) {
        data.table::setnames(TestData, 'Predict', 'Predictions_MultiClass')
      } else if(!is.null(ValidationData)) {
        data.table::setnames(ValidationData, 'Predict', 'Predictions_MultiClass')
      }
    }

  } else {
    TestData <- NULL
    ValTrue <<- TRUE
  }

  # Remove Model Object----
  rm(ClassModel)

  # Prepare for regression runs ----
  if(DebugMode) print('Prepare for regression runs ----')
  IDcols <- IDcols[!(IDcols %chin% TargetColumnName)]
  counter <- max(looper)
  Degenerate <- 0L
  R_VariableImportance <- list()
  R_ParDepPlots <- list()

  # Begin regression model building ----
  if(DebugMode) print('Begin regression model building----')
  for(bucket in looper) {

    # Define data sets ----
    if(bucket == max(looper)) {
      if(!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- TestData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
      } else if(!is.null(ValidationData)) {
        trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- NULL
      } else {
        trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- NULL
        testBucket <- NULL
      }
    } else if(bucket == 1L) {
      if(!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket])]
        testBucket <- TestData[get(TargetColumnName) <= eval(Buckets[bucket])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
      } else if(!is.null(ValidationData)) {
        if(length(Buckets) == 1 && Buckets == 0) {
          trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
          validBucket <- ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
          testBucket <- NULL
        } else {
          trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
          validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket])]
          testBucket <- NULL
        }
      } else {
        if(length(Buckets) == 1 && Buckets == 0) {
          trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
          validBucket <- NULL
          testBucket <- NULL
        } else {
          trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
          validBucket <- NULL
          testBucket <- NULL
        }
      }
    } else {
      if(!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- TestData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
      } else if(!is.null(ValidationData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- NULL
      } else {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- NULL
        testBucket <- NULL
      }
    }

    # Create Modified IDcols ----
    if(!is.null(TestData)) {
      IDcolsModified <- unique(c(IDcols, TargetColumnName, "Target_Buckets", setdiff(names(TestData), names(trainBucket))))
    } else if(!is.null(ValidationData)) {
      IDcolsModified <- unique(c(IDcols, TargetColumnName, "Target_Buckets", setdiff(names(ValidationData), names(trainBucket))))
    } else {
      IDcolsModified <- unique(c(IDcols, TargetColumnName, "Target_Buckets", setdiff(names(data), names(trainBucket))))
    }

    # AutoLightGBMRegression()----
    if(DebugMode) print('AutoLightGBMRegression()----')
    if(trainBucket[, .N] != 0L) {

      # If there is some variance then build model
      if(var(trainBucket[[eval(TargetColumnName)]]) > 0L) {

        # Increment----
        counter <- counter - 1L

        # Modify filepath and file name----
        if(bucket == max(looper)) ModelIDD <- paste0(ModelID,'_',bucket,'+') else ModelIDD <- paste0(ModelID, '_', bucket)

        # Build model----
        RegModel <- RemixAutoML::AutoLightGBMRegression(

          OutputSelection = c("Importances","EvalMetrics"),
          PrimaryDateColumn = PrimaryDateColumn,
          WeightsColumnName = WeightsColumnName,
          DebugMode = DebugMode,
          SaveInfoToPDF = FALSE,

          # GPU or CPU
          NThreads = NThreads,

          # Metadata arguments
          model_path = Paths,
          metadata_path = MetaDataPaths,
          ModelID = ModelIDD,
          ReturnFactorLevels = TRUE,
          ReturnModelObjects = ReturnModelObjects,
          SaveModelObjects = SaveModelObjects,
          Verbose = 1L,
          EncodingMethod = EncodingMethod,

          # Data arguments
          data = data.table::copy(trainBucket),
          TrainOnFull = TrainOnFull,
          ValidationData = data.table::copy(validBucket),
          TestData = data.table::copy(testBucket),
          TargetColumnName = TargetColumnName,
          FeatureColNames = FeatureNames,
          IDcols = IDcolsModified,
          TransformNumericColumns = TransformNumericColumns,
          Methods = Methods,

          # Model evaluation
          grid_eval_metric = "mse",
          NumOfParDepPlots = NumOfParDepPlots,

          # Grid tuning arguments - PassInGrid is the best of GridMetrics
          PassInGrid = PassInGrid,
          GridTune = GridTune,
          MaxModelsInGrid = MaxModelsInGrid,
          BaselineComparison = "default",
          MaxRunsWithoutNewWinner = 20L,
          MaxRunMinutes = 60*60,

          # Core parameters
          # https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
          input_model = Classifierinput_model, # continue training a model that is stored to file
          task = Regressiontask,
          device_type = Regressiondevice_type,
          objective = Regressionobjective,
          metric = Regressionmetric,
          boosting = Regressionboosting,
          LinearTree = RegressionLinearTree,
          Trees = RegressionTrees,
          eta = Regressioneta,
          num_leaves = Regressionnum_leaves,
          deterministic = Regressiondeterministic,

          # Learning Parameters
          # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
          force_col_wise = Regressionforce_col_wise,
          force_row_wise = Regressionforce_row_wise,
          max_depth = Regressionmax_depth,
          min_data_in_leaf = Regressionmin_data_in_leaf,
          min_sum_hessian_in_leaf = Regressionmin_sum_hessian_in_leaf,
          bagging_freq = Regressionbagging_freq,
          bagging_fraction = Regressionbagging_fraction,
          feature_fraction = Regressionfeature_fraction,
          feature_fraction_bynode = Regressionfeature_fraction_bynode,
          lambda_l1 = Regressionlambda_l1,
          lambda_l2 = Regressionlambda_l2,
          extra_trees = Regressionextra_trees,
          early_stopping_round = Regressionearly_stopping_round,
          first_metric_only = Regressionfirst_metric_only,
          max_delta_step = Regressionmax_delta_step,
          linear_lambda = Regressionlinear_lambda,
          min_gain_to_split = Regressionmin_gain_to_split,
          drop_rate_dart = Regressiondrop_rate_dart,
          max_drop_dart = Regressionmax_drop_dart,
          skip_drop_dart = Regressionskip_drop_dart,
          uniform_drop_dart = Regressionuniform_drop_dart,
          top_rate_goss = Regressiontop_rate_goss,
          other_rate_goss = Regressionother_rate_goss,
          monotone_constraints = Regressionmonotone_constraints,
          monotone_constraints_method = Regressionmonotone_constraints_method,
          monotone_penalty = Regressionmonotone_penalty,
          forcedsplits_filename = Regressionforcedsplits_filename,
          refit_decay_rate = Regressionrefit_decay_rate,
          path_smooth = Regressionpath_smooth,

          # IO Dataset Parameters
          # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
          max_bin = Regressionmax_bin,
          min_data_in_bin = Regressionmin_data_in_bin,
          data_random_seed = Regressiondata_random_seed,
          is_enable_sparse = Regressionis_enable_sparse,
          enable_bundle = Regressionenable_bundle,
          use_missing = Regressionuse_missing,
          zero_as_missing = Regressionzero_as_missing,
          two_round = Regressiontwo_round,

          # Convert Parameters
          convert_model = Regressionconvert_model,
          convert_model_language = Regressionconvert_model_language,

          # Objective Parameters
          # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
          boost_from_average = Regressionboost_from_average,
          alpha = Regressionalpha,
          fair_c = Regressionfair_c,
          poisson_max_delta_step = Regressionpoisson_max_delta_step,
          tweedie_variance_power = Regressiontweedie_variance_power,
          lambdarank_truncation_level = Regressionlambdarank_truncation_level,

          # Metric Parameters (metric is in Core)
          # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
          is_provide_training_metric = Regressionis_provide_training_metric,
          eval_at = Regressioneval_at,

          # Network Parameters
          # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
          num_machines = Regressionnum_machines,

          # GPU Parameters
          # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
          gpu_platform_id = Regressiongpu_platform_id,
          gpu_device_id = Regressiongpu_device_id,
          gpu_use_dp = Regressiongpu_use_dp,
          num_gpu = Regressionnum_gpu)

        # Store Model----
        RegressionModel <- RegModel$Model
        if(ReturnModelObjects || SaveModelObjects) ModelList[[ModelIDD]] <- RegressionModel
        if(!is.null(TransformNumericColumns)) {
          ArgsList[[paste0("TransformationResults_", ModelIDD)]] <- RegModel$TransformationResults
        } else {
          ArgsList[[paste0("TransformationResults_", ModelIDD)]] <- NULL
        }
        R_VariableImportance[[paste0(ModelIDD)]] <- RegModel$VariableImportance
        R_ParDepPlots[[paste0(ModelIDD)]] <- RegModel$PlotList$Test_ParDepPlots
        ArgsList[[paste0(ModelIDD, "_FactorLevelsList")]] <- RegModel$FactorLevelsList

        # Garbage Collection----
        gc()

        # Score model ----
        if(DebugMode) {
          print("TestData")
          print(TestData)
          print("ValidationData")
          print(ValidationData)
        }
        if(!is.null(TestData) || !is.null(ValidationData)) {
          TestData <- AutoLightGBMScoring(
            TargetType = "regression",
            ScoringData = if(!is.null(TestData)) TestData else ValidationData,
            FeatureColumnNames = FeatureNames,
            IDcols = IDcolsModified,
            FactorLevelsList = ArgsList[[paste0(ModelIDD, "_FactorLevelsList")]],
            EncodingMethod = EncodingMethod,
            OneHot = FALSE,
            ModelObject = RegressionModel,
            ModelPath = Paths,
            ModelID = ModelIDD,
            ReturnFeatures = TRUE,
            TransformNumeric = if(is.null(ArgsList[[paste0("TransformationResults_", ModelIDD)]])) FALSE else TRUE,
            BackTransNumeric = if(is.null(ArgsList[[paste0("TransformationResults_", ModelIDD)]])) FALSE else TRUE,
            TargetColumnName = eval(TargetColumnName),
            TransformationObject = ArgsList[[paste0("TransformationResults_", ModelIDD)]],
            TransID = NULL,
            TransPath = NULL,
            MDP_Impute = TRUE,
            MDP_CharToFactor = TRUE,
            MDP_RemoveDates = FALSE,
            MDP_MissFactor = "0",
            MDP_MissNum = -1)

          # Clear TestModel From Memory ----
          rm(RegModel)
          gc()

          # Change prediction name to prevent duplicates ----
          if(DebugMode) print('Change prediction name to prevent duplicates----')
          if(bucket == max(looper)) Val <- paste0('Predictions_', bucket - 1L, '+') else Val <- paste0('Predictions_', bucket)
          if(ValTrue) {
            x <- unique(which(names(ValidationData) %in% 'Predictions'))
            if(length(x) > 1L) data.table::set(ValidationData, j = x[length(x)], value = NULL)
            data.table::setnames(ValidationData, 'Predictions', Val)
          } else {
            x <- unique(which(names(TestData) %in% 'Predictions'))
            if(length(x) > 1L) data.table::set(TestData, j = x[length(x)], value = NULL)
            data.table::setnames(TestData, 'Predictions', Val)
          }

        } else {
          rm(RegModel); gc()
        }

      } else {

        # Check for TrainOnFull ----
        if(!is.null(TestData) || !is.null(ValidationData)) {

          # Account for degenerate distributions----
          ArgsList[['degenerate']] <- c(ArgsList[['degenerate']], bucket)

          # Use single value for predictions in the case of zero variance----
          if(!is.null(TestData)) {
            if(bucket == max(looper)) {
              Degenerate <- Degenerate + 1L
              data.table::set(TestData, j = paste0('Predictions', Buckets[bucket - 1L], '+'), value = Buckets[bucket])
              data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData) - 1L)))
            } else {
              Degenerate <- Degenerate + 1L
              data.table::set(TestData, j = paste0('Predictions', Buckets[bucket]), value = Buckets[bucket])
              data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData) - 1L)))
            }
          } else if(!is.null(ValidationData)) {
            if(bucket == max(looper)) {
              Degenerate <- Degenerate + 1L
              data.table::set(ValidationData, j = paste0('Predictions', Buckets[bucket - 1L], '+'), value = Buckets[bucket])
              data.table::setcolorder(ValidationData, c(ncol(ValidationData), 1L:(ncol(ValidationData) - 1L)))
            } else {
              Degenerate <- Degenerate + 1L
              data.table::set(ValidationData, j = paste0('Predictions', Buckets[bucket]), value = Buckets[bucket])
              data.table::setcolorder(ValidationData, c(ncol(ValidationData), 1L:(ncol(ValidationData) - 1L)))
            }
          }

        } else {

          # Account for degenerate distributions----
          ArgsList[['degenerate']] <- c(ArgsList[['degenerate']], bucket)

          # Use single value for predictions in the case of zero variance----
          if(bucket == max(looper)) {
            Degenerate <- Degenerate + 1L
          } else {
            Degenerate <- Degenerate + 1L
          }
        }
      }
    } else {

      # Account for degenerate distributions----
      ArgsList[['degenerate']] <- c(ArgsList[['degenerate']], bucket)
    }
  }

  # Rearrange Column order----
  if(DebugMode) print('Rearrange Column order----')
  if(!TrainOnFull || !is.null(ValidationData)) {

    # Change object names
    if(exists("ValTrue") && ValTrue) {
      TestData <- ValidationData
      ValidationData <- NULL
    }

    # Rearrange cols ----
    if(DebugMode) print('Rearrange cols ----')
    data.table::setcolorder(TestData, c(which(names(TestData) %like% "Predictions"), setdiff(seq_along(TestData), which(names(TestData) %like% "Predictions"))))

    # Final Combination of Predictions ----
    if(DebugMode) print('Final Combination of Predictions ----')
    if(counter > 2L || (counter == 2L && length(Buckets) != 1L)) {
      for(i in rev(looper)) {
        if(i == 1L) {
          TestData[, UpdatedPrediction := TestData[[i]] * TestData[[i + (length(looper))]]]
        } else {
          TestData[, UpdatedPrediction := UpdatedPrediction + TestData[[i]] * TestData[[i + length(looper)]]]
        }
      }
    } else {
      if(Buckets[1L] != 0) {
        TestData[, UpdatedPrediction := TestData[[1L]] * TestData[[3L]] + TestData[[2L]] * TestData[[4L]]]
      } else {
        TestData[, UpdatedPrediction := TestData[[1L]] * TestData[[3L]]]
      }
    }

    # Regression r2 via sqrt of correlation----
    r_squared <- (TestData[, stats::cor(get(TargetColumnName), UpdatedPrediction)]) ^ 2L

    # Regression Save Validation Data to File----
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        data.table::fwrite(TestData, file = file.path(MetaDataPaths, paste0(ModelID,'_ValidationData.csv')))
      } else {
        data.table::fwrite(TestData, file = file.path(Paths, paste0(ModelID,'_ValidationData.csv')))
      }
    }

    # Regression Evaluation Calibration Plot----
    EvaluationPlot <- EvalPlot(
      data = TestData,
      PredictionColName = 'UpdatedPrediction',
      TargetColName = eval(TargetColumnName),
      GraphType = 'calibration',
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))

    # Add Number of Trees to Title----
    EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0('Calibration Evaluation Plot: R2 = ',round(r_squared, 3L)))

    # Save plot to file----
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        ggplot2::ggsave(file.path(MetaDataPaths, paste0(ModelID, '_EvaluationPlot.png')))
      } else {
        ggplot2::ggsave(file.path(Paths, paste0(ModelID, '_EvaluationPlot.png')))
      }
    }

    # Regression Evaluation Calibration Plot----
    EvaluationBoxPlot <- EvalPlot(
      data = TestData,
      PredictionColName = 'UpdatedPrediction',
      TargetColName = eval(TargetColumnName),
      GraphType = 'boxplot',
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))

    # Add Number of Trees to Title----
    EvaluationBoxPlot <- EvaluationBoxPlot + ggplot2::ggtitle(paste0('Calibration Evaluation Plot: R2 = ',round(r_squared, 3L)))

    # Save plot to file----
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        ggplot2::ggsave(file.path(MetaDataPaths, paste0(ModelID,'_EvaluationBoxPlot.png')))
      } else {
        ggplot2::ggsave(file.path(Paths, paste0(ModelID,'_EvaluationBoxPlot.png')))
      }
    }

    # Regression Evaluation Metrics----
    EvaluationMetrics <- data.table::data.table(Metric = c('MAE','MAPE','MSE','R2'),MetricValue = rep(999999, 4L))
    i <- 0L
    MinVal <- min(TestData[, min(get(TargetColumnName))], TestData[, min(UpdatedPrediction)])
    for(metric in c('mae','mape','mse','r2')) {
      i <- i + 1L
      tryCatch({
        if(tolower(metric) == 'mae') {
          TestData[, Metric := abs(get(TargetColumnName) - UpdatedPrediction)]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == 'mape') {
          TestData[, Metric := abs((get(TargetColumnName) - UpdatedPrediction) / (get(TargetColumnName) + 1L))]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == 'mse') {
          TestData[, Metric := (get(TargetColumnName) - UpdatedPrediction) ^ 2L]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == 'r2') {
          TestData[, ':=' (
            Metric1 = (get(TargetColumnName) - mean(get(TargetColumnName))) ^ 2L,
            Metric2 = (get(TargetColumnName) - UpdatedPrediction) ^ 2L)]
          Metric <- 1 - TestData[, sum(Metric2, na.rm = TRUE)] / TestData[, sum(Metric1, na.rm = TRUE)]
        }
        data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric, 4L))
        data.table::set(EvaluationMetrics, i = i, j = 3L, value = NA)
      }, error = function(x) 'skip')
    }

    # Remove Cols----
    TestData[, ':=' (Metric = NULL, Metric1 = NULL, Metric2 = NULL)]

    # Save EvaluationMetrics to File----
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        data.table::fwrite(EvaluationMetrics, file = file.path(MetaDataPaths, paste0(ModelID, '_EvaluationMetrics.csv')))
      } else {
        data.table::fwrite(EvaluationMetrics, file = file.path(Paths, paste0(ModelID, '_EvaluationMetrics.csv')))
      }
    }

    # Regression Partial Dependence----
    ParDepPlots <- list()
    j <- 0L
    ParDepBoxPlots <- list()
    k <- 0L
    for(i in seq_len(min(length(FeatureColNames), NumOfParDepPlots, R_VariableImportance[[1L]][,.N]))) {
      tryCatch({
        Out <- ParDepCalPlots(
          data = TestData,
          PredictionColName = 'UpdatedPrediction',
          TargetColName = eval(TargetColumnName),
          IndepVar = VariableImportance[i, Variable],
          GraphType = 'calibration',
          PercentileBucket = 0.05,
          FactLevels = 10L,
          Function = function(x) mean(x, na.rm = TRUE))
        j <- j + 1L
        ParDepPlots[[paste0(VariableImportance[j, Variable])]] <- Out
      }, error = function(x) 'skip')
      tryCatch({
        Out1 <- ParDepCalPlots(
          data = TestData,
          PredictionColName = 'UpdatedPrediction',
          TargetColName = eval(TargetColumnName),
          IndepVar = VariableImportance[i, Variable],
          GraphType = 'boxplot',
          PercentileBucket = 0.05,
          FactLevels = 10,
          Function = function(x) mean(x, na.rm = TRUE))
        k <- k + 1L
        ParDepBoxPlots[[paste0(VariableImportance[k, Variable])]] <- Out1
      }, error = function(x) 'skip')
    }

    # Regression Save ParDepBoxPlots to file----
    if(SaveModelObjects) {
      if(!is.null(MetaDataPaths)) {
        save(ParDepBoxPlots, file = file.path(MetaDataPaths, paste0(ModelID, '_ParDepBoxPlots.R')))
      } else {
        save(ParDepBoxPlots, file = file.path(Paths, paste0(ModelID, '_ParDepBoxPlots.R')))
      }
    }
  }

  # Save args list to file----
  if(SaveModelObjects) save(ArgsList, file = file.path(Paths, paste0(ModelID, '_HurdleArgList.Rdata')))
  rm(ValTrue, envir = .GlobalEnv)

  # Return Output ----
  if(DebugMode) print("Return Output ----")
  if(!TrainOnFull) {
    return(list(
      ArgsList = if(exists("ArgsList")) ArgsList else NULL,
      ModelList = if(exists("ModelList")) ModelList else NULL,
      ClassifierModel = if(exists("ClassifierModel")) ClassifierModel else NULL,
      ClassificationMetrics = if(exists("ClassEvaluationMetrics")) ClassEvaluationMetrics else NULL,
      FinalTestData = if(exists("TestData")) TestData else NULL,
      EvaluationPlot = if(exists("EvaluationPlot")) EvaluationPlot else NULL,
      EvaluationBoxPlot = if(exists("EvaluationBoxPlot")) EvaluationBoxPlot else NULL,
      EvaluationMetrics = if(exists("EvaluationMetrics")) EvaluationMetrics else NULL,
      ClassifierVariableImportance = if(exists("C_VariableImportance")) C_VariableImportance else NULL,
      RegressionVariableImportance = if(exists("R_VariableImportance")) R_VariableImportance else NULL,
      ClassifierParDepPlots = if(exists("C_ParDepPlots")) C_ParDepPlots else NULL,
      RegressionParDepPlots = if(exists("R_ParDepPlots")) R_ParDepPlots else NULL,
      PartialDependencePlots = if(exists("ParDepPlots")) ParDepPlots else NULL,
      PartialDependenceBoxPlots = if(exists("ParDepBoxPlots")) ParDepBoxPlots else NULL))
  } else {
    return(list(
      ArgsList = if(exists("ArgsList")) ArgsList else NULL,
      ModelList = if(exists("ModelList")) ModelList else NULL,
      ClassifierModel = if(exists("ClassifierModel")) ClassifierModel else NULL))
  }
}
