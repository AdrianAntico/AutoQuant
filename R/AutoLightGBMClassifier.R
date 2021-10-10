#' @title AutoLightGBMClassifier
#'
#' @description AutoLightGBMClassifier is an automated lightgbm modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Binary Classification
#'
#' @param OutputSelection You can select what type of output you want returned. Choose from c("Importances", "EvalPlots", "EvalMetrics", "PDFs", "Score_TrainData")
#' @param TrainOnFull Set to TRUE to train on full data
#' @param DebugMode Set to TRUE to get a print out of the steps taken throughout the function
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param PrimaryDateColumn Supply a date or datetime column for catboost to utilize time as its basis for handling categorical features, instead of random shuffling
#' @param WeightsColumnName Supply a column name for your weights column. Leave NULL otherwise
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param ReturnFactorLevels Set to TRUE to have the factor levels returned with the other model objects
#' @param MaxModelsInGrid Number of models to test from grid options (243 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param Verbose Set to 0 if you want to suppress model evaluation updates in training
#' @param EncodingMethod Choose from 'binary', 'm_estimator', 'credibility', 'woe', 'target_encoding', 'poly_encode', 'backward_difference', 'helmert'
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param SaveInfoToPDF Set to TRUE to save model insights to pdf
#' @param grid_eval_metric "mae", "mape", "rmse", "r2". Case sensitive
#' @param PassInGrid Default is NULL. Provide a data.table of grid options from a previous run.
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxRunsWithoutNewWinner Runs without new winner to end procedure
#' @param MaxRunMinutes In minutes
#' @param BaselineComparison Set to either "default" or "best". Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
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
#'
#' @examples
#' \dontrun{
#' # Create some dummy correlated data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = FALSE,
#'   Classification = TRUE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' TestModel <- RemixAutoML::AutoLightGBMClassifier(
#'
#'   # Metadata args
#'   OutputSelection = c('Importances','EvalPlots','EvalMetrics','Score_TrainData'),
#'   model_path = normalizePath("./"),
#'   metadata_path = NULL,
#'   ModelID = "Test_Model_1",
#'   NumOfParDepPlots = 3L,
#'   EncodingMethod = "credibility",
#'   ReturnFactorLevels = TRUE,
#'   ReturnModelObjects = TRUE,
#'   SaveModelObjects = FALSE,
#'   SaveInfoToPDF = FALSE,
#'   DebugMode = FALSE,
#'
#'   # Data args
#'   data = data,
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = "Adrian",
#'   FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
#'   PrimaryDateColumn = NULL,
#'   WeightsColumnName = NULL,
#'   IDcols = c("IDcol_1","IDcol_2"),
#'
#'   # Grid parameters
#'   GridTune = FALSE,
#'   grid_eval_metric = 'Utility',
#'   BaselineComparison = 'default',
#'   MaxModelsInGrid = 10L,
#'   MaxRunsWithoutNewWinner = 20L,
#'   MaxRunMinutes = 24L*60L,
#'   PassInGrid = NULL,
#'
#'   # Core parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
#'   input_model = NULL, # continue training a model that is stored to file
#'   task = "train",
#'   device_type = 'CPU',
#'   NThreads = parallel::detectCores() / 2,
#'   objective = 'binary',
#'   metric = 'binary_logloss',
#'   boosting = 'gbdt',
#'   LinearTree = FALSE,
#'   Trees = 50L,
#'   eta = NULL,
#'   num_leaves = 31,
#'   deterministic = TRUE,
#'
#'   # Learning Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
#'   force_col_wise = FALSE,
#'   force_row_wise = FALSE,
#'   max_depth = NULL,
#'   min_data_in_leaf = 20,
#'   min_sum_hessian_in_leaf = 0.001,
#'   bagging_freq = 0,
#'   bagging_fraction = 1.0,
#'   feature_fraction = 1.0,
#'   feature_fraction_bynode = 1.0,
#'   extra_trees = FALSE,
#'   early_stopping_round = 10,
#'   first_metric_only = TRUE,
#'   max_delta_step = 0.0,
#'   lambda_l1 = 0.0,
#'   lambda_l2 = 0.0,
#'   linear_lambda = 0.0,
#'   min_gain_to_split = 0,
#'   drop_rate_dart = 0.10,
#'   max_drop_dart = 50,
#'   skip_drop_dart = 0.50,
#'   uniform_drop_dart = FALSE,
#'   top_rate_goss = FALSE,
#'   other_rate_goss = FALSE,
#'   monotone_constraints = NULL,
#'   monotone_constraints_method = "advanced",
#'   monotone_penalty = 0.0,
#'   forcedsplits_filename = NULL, # use for AutoStack option; .json file
#'   refit_decay_rate = 0.90,
#'   path_smooth = 0.0,
#'
#'   # IO Dataset Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#io-parameters
#'   max_bin = 255,
#'   min_data_in_bin = 3,
#'   data_random_seed = 1,
#'   is_enable_sparse = TRUE,
#'   enable_bundle = TRUE,
#'   use_missing = TRUE,
#'   zero_as_missing = FALSE,
#'   two_round = FALSE,
#'
#'   # Convert Parameters
#'   convert_model = NULL,
#'   convert_model_language = "cpp",
#'
#'   # Objective Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#objective-parameters
#'   boost_from_average = TRUE,
#'   is_unbalance = FALSE,
#'   scale_pos_weight = 1.0,
#'
#'   # Metric Parameters (metric is in Core)
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#metric-parameters
#'   is_provide_training_metric = TRUE,
#'   eval_at = c(1,2,3,4,5),
#'
#'   # Network Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#network-parameters
#'   num_machines = 1,
#'
#'   # GPU Parameters
#'   # https://lightgbm.readthedocs.io/en/latest/Parameters.html#gpu-parameters
#'   gpu_platform_id = -1,
#'   gpu_device_id = -1,
#'   gpu_use_dp = TRUE,
#'   num_gpu = 1)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and GridList
#' @export
AutoLightGBMClassifier <- function(# Data Args
                                   data = NULL,
                                   TrainOnFull = FALSE,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   IDcols = NULL,
                                   WeightsColumnName = NULL,
                                   CostMatrixWeights = c(1,0,0,1),
                                   EncodingMethod = 'credibility',

                                   # Metadata parameters
                                   OutputSelection = c('Importances', 'EvalPlots', 'EvalMetrics', 'Score_TrainData'),
                                   model_path = NULL,
                                   metadata_path = NULL,
                                   DebugMode = FALSE,
                                   SaveInfoToPDF = FALSE,
                                   ModelID = 'TestModel',
                                   ReturnFactorLevels = TRUE,
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE,
                                   NumOfParDepPlots = 3L,
                                   Verbose = 0L,

                                   # Grid parameters
                                   GridTune = FALSE,
                                   grid_eval_metric = 'Utility',
                                   BaselineComparison = 'default',
                                   MaxModelsInGrid = 10L,
                                   MaxRunsWithoutNewWinner = 20L,
                                   MaxRunMinutes = 24L*60L,
                                   PassInGrid = NULL,

                                   # High Level Parameters
                                   input_model = NULL, # continue training a model that is stored to file

                                   # Core parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
                                   task = 'train',
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

                                   # Learning Parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
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
                                   boost_from_average = TRUE,
                                   is_unbalance = FALSE,
                                   scale_pos_weight = 1.0,

                                   # Metric Parameters (metric is in Core)
                                   is_provide_training_metric = TRUE,
                                   eval_at = c(1,2,3,4,5),

                                   # Network Parameters
                                   num_machines = 1,

                                   # GPU Parameters
                                   gpu_platform_id = -1,
                                   gpu_device_id = -1,
                                   gpu_use_dp = TRUE,
                                   num_gpu = 1) {

  # Turn off warnings
  options(warn = -1)

  # LightGBM Parameters
  params <- LightGBMArgs(input_model.=input_model, task.=tolower(task), objective.=objective, boosting.=boosting, LinearTree.=LinearTree, Trees.=Trees, eta.=eta, num_leaves.=num_leaves, NThreads.=NThreads, device_type.=tolower(device_type), deterministic.=deterministic, force_col_wise.=force_col_wise, force_row_wise.=force_row_wise, max_depth.=max_depth, min_data_in_leaf.=min_data_in_leaf, min_sum_hessian_in_leaf.=min_sum_hessian_in_leaf, bagging_freq.=bagging_freq, bagging_fraction.=bagging_fraction, feature_fraction.=feature_fraction, feature_fraction_bynode.=feature_fraction_bynode, extra_trees.=extra_trees, early_stopping_round.=early_stopping_round, first_metric_only.=first_metric_only, max_delta_step.=max_delta_step, lambda_l1.=lambda_l1, lambda_l2.=lambda_l2, linear_lambda.=linear_lambda, min_gain_to_split.=min_gain_to_split, drop_rate_dart.=drop_rate_dart, max_drop_dart.=max_drop_dart, skip_drop_dart.=skip_drop_dart, uniform_drop_dart.=uniform_drop_dart, top_rate_goss.=top_rate_goss, other_rate_goss.=other_rate_goss, monotone_constraints.=monotone_constraints, monotone_constraints_method.=monotone_constraints_method, monotone_penalty.=monotone_penalty, forcedsplits_filename.=forcedsplits_filename, refit_decay_rate.=refit_decay_rate, path_smooth.=path_smooth, max_bin.=max_bin, min_data_in_bin.=min_data_in_bin, data_random_seed.=data_random_seed, is_enable_sparse.=is_enable_sparse, enable_bundle.=enable_bundle, use_missing.=use_missing, zero_as_missing.=zero_as_missing, two_round.=two_round, convert_model.=convert_model, convert_model_language.=convert_model_language, boost_from_average.=boost_from_average, alpha.=NULL, fair_c.=NULL, poisson_max_delta_step.=NULL, tweedie_variance_power.=NULL, lambdarank_truncation_level.=NULL, is_unbalance.=is_unbalance, scale_pos_weight.=scale_pos_weight, multi_error_top_k.=NULL, is_provide_training_metric.=is_provide_training_metric, eval_at.=eval_at, gpu_platform_id.=gpu_platform_id, gpu_device_id.=gpu_device_id, gpu_use_dp.=gpu_use_dp, num_gpu.=num_gpu)

  # Grab all official parameters and their evaluated arguments
  ArgsList <- c(as.list(environment()))
  ArgsList[['data']] <- NULL
  ArgsList[['ValidationData']] <- NULL
  ArgsList[['TestData']] <- NULL
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      save(ArgsList, file = file.path(metadata_path, paste0(ModelID, "_ArgsList.Rdata")))
    } else if(!is.null(model_path)) {
      save(ArgsList, file = file.path(model_path, paste0(ModelID, "_ArgsList.Rdata")))
    }
  }

  # Data prep ----
  if(DebugMode) print("Data prep ----")
  Output <- XGBoostDataPrep(Algo="lightgbm", ModelType="classification", data.=data, ValidationData.=ValidationData, TestData.=TestData, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, WeightsColumnName.=WeightsColumnName, IDcols.=IDcols, TransformNumericColumns.=NULL, Methods.=NULL, ModelID.=ModelID, model_path.=model_path, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, ReturnFactorLevels.=ReturnFactorLevels, EncodingMethod.=EncodingMethod)
  TransformNumericColumns <- Output$TransformNumericColumns; Output$TransformNumericColumns <- NULL
  TransformationResults <- Output$TransformationResults; Output$TransformationResults <- NULL
  FactorLevelsList <- Output$FactorLevelsList; Output$FactorLevelsList <- NULL
  FinalTestTarget <- Output$FinalTestTarget; Output$FinalTestTarget <- NULL
  WeightsVector <- Output$WeightsVector; Output$WeightsVector <- NULL
  datavalidate <- Output$datavalidate; Output$datavalidate <- NULL
  TargetLevels <- Output$TargetLevels; Output$TargetLevels <- NULL
  TrainTarget <- Output$TrainTarget; Output$TrainTarget <- NULL
  TrainMerge <- Output$TrainMerge; Output$TrainMerge <- NULL
  ValidMerge <- Output$ValidMerge; Output$ValidMerge <- NULL
  TestTarget <- Output$TestTarget; Output$TestTarget <- NULL
  datatrain <- Output$datatrain; Output$datatrain <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
  TestMerge <- Output$TestMerge; Output$TestMerge <- NULL
  TestData <- Output$TestData; Output$TestData <- NULL
  datatest <- Output$datatest; Output$datatest <- NULL
  EvalSets <- Output$EvalSets; Output$EvalSets <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  IDcols <- Output$IDcols; Output$IDcols <- NULL
  Names <- Output$Names; rm(Output)

  # Weights Column
  if(!is.null(WeightsColumnName)) {
    params[["weight_column"]] <- which(WeightsColumnName %chin% names(dataTrain)) - 1L
  }

  # Bring into existence
  ExperimentalGrid <- NULL; BestGrid <- NULL

  # Grid tuning ----
  if(DebugMode) print("Grid tuning ----")
  if(GridTune) {
    Output <- LightGBMGridTuner(ModelType="classification", TrainOnFull.=TrainOnFull, DebugMode.=DebugMode, params.=params, num_iterations.=params$num_iterations, max_depth.=params$max_depth, eta.=params$eta, num_leaves.=params$num_leaves, min_data_in_leaf.=params$min_data_in_leaf, bagging_freq.=params$bagging_freq, bagging_fraction.=params$bagging_fraction, feature_fraction.=params$feature_fraction, feature_fraction_bynode.=params$feature_fraction_bynode, lambda_l1.=params$lambda_l1, lambda_l2.=params$lambda_l2, LossFunction=NULL, EvalMetric=eval_metric, grid_eval_metric.=grid_eval_metric, CostMatrixWeights=CostMatrixWeights, TargetColumnName.=TargetColumnName, datatrain.=datatrain, dataTest.=dataTest, TestData.=TestData, EvalSets.=EvalSets, TestTarget.=TestTarget, FinalTestTarget.=FinalTestTarget, TargetLevels.=NULL, MaxRunsWithoutNewWinner=MaxRunsWithoutNewWinner, MaxModelsInGrid=MaxModelsInGrid, MaxRunMinutes=MaxRunMinutes, BaselineComparison.=BaselineComparison, SaveModelObjects=SaveModelObjects, metadata_path=metadata_path, model_path=model_path, ModelID=ModelID, NumLevels.=NULL)
    ExperimentalGrid <- Output$ExperimentalGrid
    BestGrid <- Output$BestGrid
  }

  # Final Params ----
  if(DebugMode) print("Final Params ----")
  params <- LightGBMFinalParams(params.=params, GridTune.=GridTune, PassInGrid.=PassInGrid, TrainOnFull.=TrainOnFull, BestGrid.=BestGrid, Trees.=params[["Trees"]], eta.=params[["eta"]], num_leaves.=params[["num_leaves"]], max_depth.=params[["max_depth"]], min_data_in_leaf.=params[["min_data_in_leaf"]], bagging_freq.=params[["bagging_freq"]], bagging_fraction.=params[["bagging_fraction"]], feature_fraction.=params[["feature_fraction"]], feature_fraction_bynode.=params[["feature_fraction_bynode"]])

  # Build model ----
  if(DebugMode) print("Build model ----")
  model <- lightgbm::lgb.train(params=params, data=datatrain, valids=EvalSets, nrounds = 5L)

  # Save Model ----
  if(DebugMode) print("Save Model ----")
  if(SaveModelObjects) lightgbm::lgb.save(booster=model, filename=file.path(model_path, paste0(ModelID, ".txt")))

  # TrainData + ValidationData Scoring + Shap ----
  if(DebugMode) print("TrainData + ValidationData Scoring + Shap ----")
  if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
    predict <- data.table::as.data.table(predict(model, as.matrix(dataTrain)))
    if(!is.null(datavalidate)) {
      predict_validate <- data.table::as.data.table(predict(model, as.matrix(dataTest)))
      predict <- data.table::rbindlist(list(predict, predict_validate))
      data.table::setnames(predict, names(predict), "Predict")
      rm(predict_validate)
    }
    Output <- XGBoostValidationData(model.=model, TestData.=NULL, ModelType="classification", TrainOnFull.=TRUE, TestDataCheck=FALSE, FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, TrainTarget.=TrainTarget, TrainMerge.=TrainMerge, TestMerge.=TestMerge, dataTest.=dataTest, data.=dataTrain, predict.=predict, TargetColumnName.=TargetColumnName, SaveModelObjects. = SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, LossFunction.=NULL, TransformNumericColumns.=TransformNumericColumns, GridTune.=GridTune, TransformationResults.=TransformationResults, TargetLevels.=NULL)
    TrainData <- Output$ValidationData; rm(Output)
    if(!"Predict" %chin% names(TrainData)) data.table::setnames(TrainData, "V1", "Predict")
  } else {
    TrainData <- NULL
  }

  # Grid Score Model ----
  if(DebugMode) print("Grid Score Model ----")
  predict <- predict(object = model, if(!is.null(TestData)) as.matrix(TestData) else if(!is.null(ValidationData) && !TrainOnFull) as.matrix(dataTest) else as.matrix(dataTrain))

  # Validation, Importance, Shap data ----
  if(DebugMode) print("Validation, Importance, Shap data ----")
  Output <- XGBoostValidationData(ModelType="classification", TestDataCheck=!is.null(TestData), TrainOnFull.=TrainOnFull, model.=model, TargetColumnName.=TargetColumnName, SaveModelObjects.=SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, TestData.=TestData, TestTarget.=TestTarget, FinalTestTarget.=FinalTestTarget, TestMerge.=TestMerge, dataTest.=dataTest, TrainTarget.=TrainTarget, predict.=predict, TransformNumericColumns.=TransformNumericColumns, TransformationResults.=TransformationResults, GridTune.=GridTune, data.=dataTrain)
  VariableImportance <- Output$VariableImportance; Output$VariableImportance <- NULL
  ValidationData <- Output$ValidationData; rm(Output)

  # Generate EvaluationMetrics ----
  if(DebugMode) print("Running BinaryMetrics()")
  EvalMetricsList <- list()
  EvalMetrics2List <- list()
  if("evalmetrics" %chin% tolower(OutputSelection)) {
    if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
      EvalMetricsList[["TrainData"]] <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=FALSE, ValidationData.=TrainData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, Method = "threshold")
      EvalMetrics2List[["TrainData"]] <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=FALSE, ValidationData.=TrainData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, Method = "bins")
      if(SaveModelObjects) {
        if(!is.null(metadata_path)) {
          data.table::fwrite(EvalMetricsList[['TestData']], file = file.path(metadata_path, paste0(ModelID, "_Test_EvaluationMetrics.csv")))
        } else if(!is.null(model_path)) {
          data.table::fwrite(EvalMetricsList[['TestData']], file = file.path(model_path, paste0(ModelID, "_Test_EvaluationMetrics.csv")))
        }
      }
    }
    EvalMetricsList[["TestData"]] <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=FALSE, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, Method = "threshold")
    EvalMetrics2List[["TestData"]] <- BinaryMetrics(ClassWeights.=NULL, CostMatrixWeights.=CostMatrixWeights, SaveModelObjects.=FALSE, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, Method = "bins")
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvalMetricsList[['TestData']], file = file.path(metadata_path, paste0(ModelID, "_Test_EvaluationMetrics.csv")))
      } else if(!is.null(model_path)) {
        data.table::fwrite(EvalMetricsList[['TestData']], file = file.path(model_path, paste0(ModelID, "_Test_EvaluationMetrics.csv")))
      }
    }
  }

  # Classification evaluation plots ----
  if(DebugMode) print("Running ML_EvalPlots()")
  PlotList <- list()
  if("evalplots" %chin% tolower(OutputSelection)) {
    if("score_traindata" %chin% tolower(OutputSelection) && !TrainOnFull) {
      Output <- ML_EvalPlots(ModelType="classification", DataType = 'Train', TrainOnFull.=TrainOnFull, ValidationData.=TrainData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)
      PlotList[["Train_EvaluationPlot"]] <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
      PlotList[["Train_ParDepPlots"]] <- Output$ParDepPlots; Output$ParDepPlots <- NULL
      PlotList[["Train_GainsPlot"]] <- Output$GainsPlot; Output$GainsPlot <- NULL
      PlotList[["Train_LiftPlot"]] <- Output$LiftPlot; Output$LiftPlot <- NULL
      PlotList[["Train_ROC_Plot"]] <- Output$ROC_Plot; rm(Output)
    }
    Output <- ML_EvalPlots(ModelType="classification", DataType = 'Test', TrainOnFull.=TrainOnFull, ValidationData.=ValidationData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)
    PlotList[["Test_EvaluationPlot"]] <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
    PlotList[["Test_ParDepPlots"]] <- Output$ParDepPlots; Output$ParDepPlots <- NULL
    PlotList[["Test_GainsPlot"]] <- Output$GainsPlot; Output$GainsPlot <- NULL
    PlotList[["Test_LiftPlot"]] <- Output$LiftPlot; Output$LiftPlot <- NULL
    PlotList[["Test_ROC_Plot"]] <- Output$ROC_Plot; rm(Output)
    if(!is.null(VariableImportance) && "plotly" %chin% installed.packages()) PlotList[["VariableImportance"]] <- plotly::ggplotly(VI_Plot(Type = "xgboost", VariableImportance)) else if(!is.null(VariableImportance)) PlotList[["VariableImportance"]] <- VI_Plot(Type = "xgboost", VariableImportance)
  }

  # Save PDF of model information ----
  if(DebugMode) print("Save PDF of model information ----")
  if("pdfs" %chin% tolower(OutputSelection) && SaveModelObjects) {
    CatBoostPDF(ModelType="regression", TrainOnFull.=TrainOnFull, SaveInfoToPDF.=SaveInfoToPDF, VariableImportance.=VariableImportance, Interaction.=NULL, model_path.=model_path, metadata_path.=metadata_path)
  }

  # FactorLevelsList ----
  if(!exists("FactorLevelsList")) FactorLevelsList <- NULL

  # turn back to normal
  options(warn = 1)

  # Return objects ----
  if(DebugMode) print("Return objects ----")
  if(ReturnModelObjects) {
    return(list(
      Model = model,
      TrainData = if(exists("TrainData")) TrainData else NULL,
      TestData = if(exists("ValidationData")) ValidationData else NULL,
      PlotList = if(exists("PlotList")) PlotList else NULL,
      EvaluationMetrics = if(exists("EvalMetricsList")) EvalMetricsList else NULL,
      EvaluationMetrics2 = if(exists("EvalMetrics2List")) EvalMetrics2List else NULL,
      VariableImportance = if(exists("VariableImportance")) VariableImportance else NULL,
      GridMetrics = if(exists("ExperimentalGrid") && !is.null(ExperimentalGrid)) data.table::setorderv(ExperimentalGrid, cols = "EvalMetric", order = -1L, na.last = TRUE) else NULL,
      ColNames = if(exists("Names")) Names else NULL,
      FactorLevelsList = if(exists("FactorLevelsList")) FactorLevelsList else NULL,
      ArgsList = ArgsList))
  }
}
