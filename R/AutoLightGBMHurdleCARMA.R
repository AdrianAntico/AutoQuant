#' @title AutoLightGBMHurdleCARMA
#'
#' @description AutoLightGBMHurdleCARMA is an intermittent demand, Mutlivariate Forecasting algorithms with calendar variables, Holiday counts, holiday lags, holiday moving averages, differencing, transformations, interaction-based categorical encoding using target variable and features to generate various time-based aggregated lags, moving averages, moving standard deviations, moving skewness, moving kurtosis, moving quantiles, parallelized interaction-based fourier pairs by grouping variables, and Trend Variables.
#'
#' @author Adrian Antico
#' @family Automated Panel Data Forecasting
#'
#' @param data Supply your full series data set here
#' @param TrainOnFull Set to TRUE to train on full data
#' @param TargetColumnName List the column name of your target variables column. E.g. 'Target'
#' @param NonNegativePred TRUE or FALSE
#' @param Threshold Select confusion matrix measure to optimize for pulling in threshold. Choose from 'MCC', 'Acc', 'TPR', 'TNR', 'FNR', 'FPR', 'FDR', 'FOR', 'F1_Score', 'F2_Score', 'F0.5_Score', 'NPV', 'PPV', 'ThreatScore', 'Utility'
#' @param RoundPreds Rounding predictions to an integer value. TRUE or FALSE. Defaults to FALSE
#' @param DateColumnName List the column name of your date column. E.g. 'DateTime'
#' @param GroupVariables Defaults to NULL. Use NULL when you have a single series. Add in GroupVariables when you have a series for every level of a group or multiple groups.
#' @param EncodingMethod Choose from 'binary', 'poly_encode', 'backward_difference', 'helmert' for multiclass cases and additionally 'm_estimator', 'credibility', 'woe', 'target_encoding' for classification use cases.
#' @param TimeWeights Timeweights creation. Supply a value, such as 0.9999
#' @param HierarchGroups Vector of hierachy categorical columns.
#' @param TimeUnit List the time unit your data is aggregated by. E.g. '1min', '5min', '10min', '15min', '30min', 'hour', 'day', 'week', 'month', 'quarter', 'year'.
#' @param TimeGroups Select time aggregations for adding various time aggregated GDL features.
#' @param FC_Periods Set the number of periods you want to have forecasts for. E.g. 52 for weekly data to forecast a year ahead
#' @param TargetTransformation Run AutoTransformationCreate() to find best transformation for the target variable. Tests YeoJohnson, BoxCox, and Asigh (also Asin and Logit for proportion target variables).
#' @param Methods Choose from 'YeoJohnson', 'BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', or 'Logit'. If more than one is selected, the one with the best normalization pearson statistic will be used. Identity is automatically selected and compared.
#' @param XREGS Additional data to use for model development and forecasting. Data needs to be a complete series which means both the historical and forward looking values over the specified forecast window needs to be supplied.
#' @param Timer Set to FALSE to turn off the updating print statements for progress
#' @param DebugMode Defaults to FALSE. Set to TRUE to get a print statement of each high level comment in function
#' @param Lags Select the periods for all lag variables you want to create. E.g. c(1:5,52)
#' @param MA_Periods Select the periods for all moving average variables you want to create. E.g. c(1:5,52)
#' @param SD_Periods Select the periods for all moving standard deviation variables you want to create. E.g. c(1:5,52)
#' @param Skew_Periods Select the periods for all moving skewness variables you want to create. E.g. c(1:5,52)
#' @param Kurt_Periods Select the periods for all moving kurtosis variables you want to create. E.g. c(1:5,52)
#' @param Quantile_Periods Select the periods for all moving quantiles variables you want to create. E.g. c(1:5,52)
#' @param Quantiles_Selected Select from the following 'q5', 'q10', 'q15', 'q20', 'q25', 'q30', 'q35', 'q40', 'q45', 'q50', 'q55', 'q60', 'q65', 'q70', 'q75', 'q80', 'q85', 'q90', 'q95'
#' @param AnomalyDetection NULL for not using the service. Other, provide a list, e.g. AnomalyDetection = list('tstat_high' = 4, tstat_low = -4)
#' @param Difference Puts the I in ARIMA for single series and grouped series.
#' @param FourierTerms Set to the max number of pairs. E.g. 2 means to generate two pairs for by each group level and interations if hierarchy is enabled.
#' @param CalendarVariables NULL, or select from 'second', 'minute', 'hour', 'wday', 'mday', 'yday', 'week', 'isoweek', 'month', 'quarter', 'year'
#' @param HolidayVariable NULL, or select from 'USPublicHolidays', 'EasterGroup', 'ChristmasGroup', 'OtherEcclesticalFeasts'
#' @param HolidayLookback Number of days in range to compute number of holidays from a given date in the data. If NULL, the number of days are computed for you.
#' @param HolidayLags Number of lags to build off of the holiday count variable.
#' @param HolidayMovingAverages Number of moving averages to build off of the holiday count variable.
#' @param TimeTrendVariable Set to TRUE to have a time trend variable added to the model. Time trend is numeric variable indicating the numeric value of each record in the time series (by group). Time trend starts at 1 for the earliest point in time and increments by one for each success time point.
#' @param DataTruncate Set to TRUE to remove records with missing values from the lags and moving average features created
#' @param ZeroPadSeries Set to 'all', 'inner', or NULL. See TimeSeriesFill for explanation
#' @param PartitionType Select 'random' for random data partitioning 'timeseries' for partitioning by time frames
#' @param SplitRatios E.g c(0.7,0.2,0.1) for train, validation, and test sets
#' @param NumOfParDepPlots Supply a number for the number of partial dependence plots you want returned
#' @param EvalMetric Select from 'RMSE', 'MAE', 'MAPE', 'Poisson', 'Quantile', 'LogLinQuantile', 'Lq', 'NumErrors', 'SMAPE', 'R2', 'MSLE', 'MedianAbsoluteError'
#' @param GridTune Set to TRUE to run a grid tune
#' @param PassInGrid Defaults to NULL
#' @param ModelCount Set the number of models to try in the grid tune
#' @param MaxRunsWithoutNewWinner Default is 50
#' @param MaxRunMinutes Default is 60*60
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
#'
#'  # Single group variable and xregs ----
#'
#'  # Load Walmart Data from Dropbox----
#'  data <- data.table::fread(
#'    'https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1')
#'
#'  # Subset for Stores / Departments With Full Series
#'  data <- data[, Counts := .N, by = c('Store','Dept')][Counts == 143][
#'    , Counts := NULL]
#'
#'  # Subset Columns (remove IsHoliday column)----
#'  keep <- c('Store','Dept','Date','Weekly_Sales')
#'  data <- data[, ..keep]
#'  data <- data[Store == 1][, Store := NULL]
#'  xregs <- data.table::copy(data)
#'  data.table::setnames(xregs, 'Dept', 'GroupVar')
#'  data.table::setnames(xregs, 'Weekly_Sales', 'Other')
#'  data <- data[as.Date(Date) < as.Date('2012-09-28')]
#'
#'  # Add zeros for testing
#'  data[runif(.N) < 0.25, Weekly_Sales := 0]
#'
#'  # Build forecast
#'  CatBoostResults <- RemixAutoML::AutoLightGBMHurdleCARMA(
#'
#'   # data args
#'   data = data, # TwoGroup_Data,
#'   TargetColumnName = 'Weekly_Sales',
#'   DateColumnName = 'Date',
#'   HierarchGroups = NULL,
#'   GroupVariables = c('Dept'),
#'   EncodingMethod = "credibility",
#'   TimeWeights = 1,
#'   TimeUnit = 'weeks',
#'   TimeGroups = c('weeks','months'),
#'
#'   # Production args
#'   TrainOnFull = FALSE,
#'   SplitRatios = c(1 - 20 / 138, 10 / 138, 10 / 138),
#'   PartitionType = 'random',
#'   FC_Periods = 4,
#'   Timer = TRUE,
#'   DebugMode = TRUE,
#'
#'   # Target transformations
#'   TargetTransformation = TRUE,
#'   Methods = c('BoxCox', 'Asinh', 'Asin', 'Log',
#'     'LogPlus1', 'Sqrt', 'Logit', 'YeoJohnson'),
#'   Difference = FALSE,
#'   NonNegativePred = FALSE,
#'   RoundPreds = FALSE,
#'
#'   # Date features
#'   CalendarVariables = c('week', 'wom', 'month', 'quarter'),
#'   HolidayVariable = c('USPublicHolidays',
#'     'EasterGroup',
#'     'ChristmasGroup','OtherEcclesticalFeasts'),
#'   HolidayLookback = NULL,
#'   HolidayLags = 1,
#'   HolidayMovingAverages = 1:2,
#'
#'   # Time series features
#'   Lags = list('weeks' = seq(2L, 10L, 2L),
#'     'months' = c(1:3)),
#'   MA_Periods = list('weeks' = seq(2L, 10L, 2L),
#'     'months' = c(2,3)),
#'   SD_Periods = NULL,
#'   Skew_Periods = NULL,
#'   Kurt_Periods = NULL,
#'   Quantile_Periods = NULL,
#'   Quantiles_Selected = c('q5','q95'),
#'
#'   # Bonus features
#'   AnomalyDetection = NULL,
#'   XREGS = xregs,
#'   FourierTerms = 2,
#'   TimeTrendVariable = TRUE,
#'   ZeroPadSeries = NULL,
#'   DataTruncate = FALSE,
#'
#'   # ML Args
#'   NumOfParDepPlots = 100L,
#'   EvalMetric = 'RMSE',
#'   GridTune = FALSE,
#'   PassInGrid = NULL,
#'   ModelCount = 5,
#'   MaxRunsWithoutNewWinner = 50,
#'   MaxRunMinutes = 60*60,
#'
#'   # Core parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
#'   input_model = list('classifier' = NULL, 'regression' = NULL),
#'   task = list('classifier' = 'train', 'regression' = 'train'),
#'   device_type = list('classifier' = 'CPU', 'regression' = 'CPU'),
#'   objective = list('classifier' = 'binary', 'regression' = 'regression'),
#'   metric = list('classifier' = 'binary_logloss', 'regression' = 'rmse'),
#'   boosting = list('classifier' = 'gbdt', 'regression' = 'gbdt'),
#'   LinearTree = list('classifier' = FALSE, 'regression' = FALSE),
#'   Trees = list('classifier' = 1000L, 'regression' = 1000L),
#'   eta = list('classifier' = NULL, 'regression' = NULL),
#'   num_leaves = list('classifier' = 31, 'regression' = 31),
#'   deterministic = list('classifier' = TRUE, 'regression' = TRUE),
#'
#'   # Learning Parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
#'   force_col_wise = list('classifier' = FALSE, 'regression' = FALSE),
#'   force_row_wise = list('classifier' = FALSE, 'regression' = FALSE),
#'   max_depth = list('classifier' = NULL, 'regression' = NULL),
#'   min_data_in_leaf = list('classifier' = 20, 'regression' = 20),
#'   min_sum_hessian_in_leaf = list('classifier' = 0.001, 'regression' = 0.001),
#'   bagging_freq = list('classifier' = 0, 'regression' = 0),
#'   bagging_fraction = list('classifier' = 1.0, 'regression' = 1.0),
#'   feature_fraction = list('classifier' = 1.0, 'regression' = 1.0),
#'   feature_fraction_bynode = list('classifier' = 1.0, 'regression' = 1.0),
#'   extra_trees = list('classifier' = FALSE, 'regression' = FALSE),
#'   early_stopping_round = list('classifier' = 10, 'regression' = 10),
#'   first_metric_only = list('classifier' = TRUE, 'regression' = TRUE),
#'   max_delta_step = list('classifier' = 0.0, 'regression' = 0.0),
#'   lambda_l1 = list('classifier' = 0.0, 'regression' = 0.0),
#'   lambda_l2 = list('classifier' = 0.0, 'regression' = 0.0),
#'   linear_lambda = list('classifier' = 0.0, 'regression' = 0.0),
#'   min_gain_to_split = list('classifier' = 0, 'regression' = 0),
#'   drop_rate_dart = list('classifier' = 0.10, 'regression' = 0.10),
#'   max_drop_dart = list('classifier' = 50, 'regression' = 50),
#'   skip_drop_dart = list('classifier' = 0.50, 'regression' = 0.50),
#'   uniform_drop_dart = list('classifier' = FALSE, 'regression' = FALSE),
#'   top_rate_goss = list('classifier' = FALSE, 'regression' = FALSE),
#'   other_rate_goss = list('classifier' = FALSE, 'regression' = FALSE),
#'   monotone_constraints = list('classifier' = NULL, 'regression' = NULL),
#'   monotone_constraints_method = list('classifier' = 'advanced', 'regression' = 'advanced'),
#'   monotone_penalty = list('classifier' = 0.0, 'regression' = 0.0),
#'   forcedsplits_filename = list('classifier' = NULL, 'regression' = NULL),
#'   refit_decay_rate = list('classifier' = 0.90, 'regression' = 0.90),
#'   path_smooth = list('classifier' = 0.0, 'regression' = 0.0),
#'
#'   # IO Dataset Parameters
#'   max_bin = list('classifier' = 255, 'regression' = 255),
#'   min_data_in_bin = list('classifier' = 3, 'regression' = 3),
#'   data_random_seed = list('classifier' = 1, 'regression' = 1),
#'   is_enable_sparse = list('classifier' = TRUE, 'regression' = TRUE),
#'   enable_bundle = list('classifier' = TRUE, 'regression' = TRUE),
#'   use_missing = list('classifier' = TRUE, 'regression' = TRUE),
#'   zero_as_missing = list('classifier' = FALSE, 'regression' = FALSE),
#'   two_round = list('classifier' = FALSE, 'regression' = FALSE),
#'
#'   # Convert Parameters
#'   convert_model = list('classifier' = NULL, 'regression' = NULL),
#'   convert_model_language = list('classifier' = "cpp", 'regression' = "cpp"),
#'
#'   # Objective Parameters
#'   boost_from_average = list('classifier' = TRUE, 'regression' = TRUE),
#'   is_unbalance = list('classifier' = FALSE, 'regression' = FALSE),
#'   scale_pos_weight = list('classifier' = 1.0, 'regression' = 1.0),
#'
#'   # Metric Parameters (metric is in Core)
#'   is_provide_training_metric = list('classifier' = TRUE, 'regression' = TRUE),
#'   eval_at = list('classifier' = c(1,2,3,4,5), 'regression' = c(1,2,3,4,5)),
#'
#'   # Network Parameters
#'   num_machines = list('classifier' = 1, 'regression' = 1),
#'
#'   # GPU Parameters
#'   gpu_platform_id = list('classifier' = -1, 'regression' = -1),
#'   gpu_device_id = list('classifier' = -1, 'regression' = -1),
#'   gpu_use_dp = list('classifier' = TRUE, 'regression' = TRUE),
#'   num_gpu = list('classifier' = 1, 'regression' = 1))
#'
#' # Two group variables and xregs
#'
#' # Load Walmart Data from Dropbox----
#' data <- data.table::fread(
#'  'https://www.dropbox.com/s/2str3ek4f4cheqi/walmart_train.csv?dl=1')
#'
#' # Subset for Stores / Departments With Full Series
#' data <- data[, Counts := .N, by = c('Store','Dept')][Counts == 143][
#'   , Counts := NULL]
#'
#' # Put negative values at 0
#' data[, Weekly_Sales := data.table::fifelse(Weekly_Sales < 0, 0, Weekly_Sales)]
#'
#' # Subset Columns (remove IsHoliday column)----
#' keep <- c('Store','Dept','Date','Weekly_Sales')
#' data <- data[, ..keep]
#' data <- data[Store %in% c(1,2)]
#'
#' xregs <- data.table::copy(data)
#' xregs[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c('Store','Dept')]
#' xregs[, c('Store','Dept') := NULL]
#' data.table::setnames(xregs, 'Weekly_Sales', 'Other')
#' xregs[, Other := jitter(Other, factor = 25)]
#' data <- data[as.Date(Date) < as.Date('2012-09-28')]
#'
#' # Add some zeros for testing
#' data[runif(.N) < 0.25, Weekly_Sales := 0]
#'
#' # Build forecast
#' Output <- RemixAutoML::AutoLightGBMHurdleCARMA(
#'
#'   # data args
#'   data = data,
#'   TargetColumnName = 'Weekly_Sales',
#'   DateColumnName = 'Date',
#'   HierarchGroups = NULL,
#'   GroupVariables = c('Store','Dept'),
#'   EncodingMethod = "credibility",
#'   TimeWeights = 1,
#'   TimeUnit = 'weeks',
#'   TimeGroups = c('weeks','months'),
#'
#'   # Production args
#'   TrainOnFull = TRUE,
#'   SplitRatios = c(1 - 20 / 138, 10 / 138, 10 / 138),
#'   PartitionType = 'random',
#'   FC_Periods = 4,
#'   Timer = TRUE,
#'   DebugMode = TRUE,
#'
#'   # Target transformations
#'   TargetTransformation = TRUE,
#'   Methods = c('BoxCox', 'Asinh', 'Asin', 'Log',
#'               'LogPlus1', 'Sqrt', 'Logit', 'YeoJohnson'),
#'   Difference = FALSE,
#'   NonNegativePred = FALSE,
#'   Threshold = NULL,
#'   RoundPreds = FALSE,
#'
#'   # Date features
#'   CalendarVariables = c('week', 'wom', 'month', 'quarter'),
#'   HolidayVariable = c('USPublicHolidays',
#'                       'EasterGroup',
#'                       'ChristmasGroup','OtherEcclesticalFeasts'),
#'   HolidayLookback = NULL,
#'   HolidayLags = 1,
#'   HolidayMovingAverages = 1:2,
#'
#'   # Time series features
#'   Lags = list('weeks' = seq(2L, 10L, 2L),
#'               'months' = c(1:3)),
#'   MA_Periods = list('weeks' = seq(2L, 10L, 2L),
#'                     'months' = c(2,3)),
#'   SD_Periods = NULL,
#'   Skew_Periods = NULL,
#'   Kurt_Periods = NULL,
#'   Quantile_Periods = NULL,
#'   Quantiles_Selected = c('q5','q95'),
#'
#'   # Bonus features
#'   AnomalyDetection = NULL,
#'   XREGS = xregs,
#'   FourierTerms = 2,
#'   TimeTrendVariable = TRUE,
#'   ZeroPadSeries = NULL,
#'   DataTruncate = FALSE,
#'
#'   # ML Args
#'   NumOfParDepPlots = 100L,
#'   EvalMetric = 'RMSE',
#'   GridTune = FALSE,
#'   PassInGrid = NULL,
#'   ModelCount = 5,
#'   MaxRunsWithoutNewWinner = 50,
#'   MaxRunMinutes = 60*60,
#'
#'   # Core parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
#'   input_model = list('classifier' = NULL, 'regression' = NULL),
#'   task = list('classifier' = 'train', 'regression' = 'train'),
#'   device_type = list('classifier' = 'CPU', 'regression' = 'CPU'),
#'   objective = list('classifier' = 'binary', 'regression' = 'regression'),
#'   metric = list('classifier' = 'binary_logloss', 'regression' = 'rmse'),
#'   boosting = list('classifier' = 'gbdt', 'regression' = 'gbdt'),
#'   LinearTree = list('classifier' = FALSE, 'regression' = FALSE),
#'   Trees = list('classifier' = 1000L, 'regression' = 1000L),
#'   eta = list('classifier' = NULL, 'regression' = NULL),
#'   num_leaves = list('classifier' = 31, 'regression' = 31),
#'   deterministic = list('classifier' = TRUE, 'regression' = TRUE),
#'
#'   # Learning Parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
#'   force_col_wise = list('classifier' = FALSE, 'regression' = FALSE),
#'   force_row_wise = list('classifier' = FALSE, 'regression' = FALSE),
#'   max_depth = list('classifier' = NULL, 'regression' = NULL),
#'   min_data_in_leaf = list('classifier' = 20, 'regression' = 20),
#'   min_sum_hessian_in_leaf = list('classifier' = 0.001, 'regression' = 0.001),
#'   bagging_freq = list('classifier' = 0, 'regression' = 0),
#'   bagging_fraction = list('classifier' = 1.0, 'regression' = 1.0),
#'   feature_fraction = list('classifier' = 1.0, 'regression' = 1.0),
#'   feature_fraction_bynode = list('classifier' = 1.0, 'regression' = 1.0),
#'   extra_trees = list('classifier' = FALSE, 'regression' = FALSE),
#'   early_stopping_round = list('classifier' = 10, 'regression' = 10),
#'   first_metric_only = list('classifier' = TRUE, 'regression' = TRUE),
#'   max_delta_step = list('classifier' = 0.0, 'regression' = 0.0),
#'   lambda_l1 = list('classifier' = 0.0, 'regression' = 0.0),
#'   lambda_l2 = list('classifier' = 0.0, 'regression' = 0.0),
#'   linear_lambda = list('classifier' = 0.0, 'regression' = 0.0),
#'   min_gain_to_split = list('classifier' = 0, 'regression' = 0),
#'   drop_rate_dart = list('classifier' = 0.10, 'regression' = 0.10),
#'   max_drop_dart = list('classifier' = 50, 'regression' = 50),
#'   skip_drop_dart = list('classifier' = 0.50, 'regression' = 0.50),
#'   uniform_drop_dart = list('classifier' = FALSE, 'regression' = FALSE),
#'   top_rate_goss = list('classifier' = FALSE, 'regression' = FALSE),
#'   other_rate_goss = list('classifier' = FALSE, 'regression' = FALSE),
#'   monotone_constraints = list('classifier' = NULL, 'regression' = NULL),
#'   monotone_constraints_method = list('classifier' = 'advanced', 'regression' = 'advanced'),
#'   monotone_penalty = list('classifier' = 0.0, 'regression' = 0.0),
#'   forcedsplits_filename = list('classifier' = NULL, 'regression' = NULL),
#'   refit_decay_rate = list('classifier' = 0.90, 'regression' = 0.90),
#'   path_smooth = list('classifier' = 0.0, 'regression' = 0.0),
#'
#'   # IO Dataset Parameters
#'   max_bin = list('classifier' = 255, 'regression' = 255),
#'   min_data_in_bin = list('classifier' = 3, 'regression' = 3),
#'   data_random_seed = list('classifier' = 1, 'regression' = 1),
#'   is_enable_sparse = list('classifier' = TRUE, 'regression' = TRUE),
#'   enable_bundle = list('classifier' = TRUE, 'regression' = TRUE),
#'   use_missing = list('classifier' = TRUE, 'regression' = TRUE),
#'   zero_as_missing = list('classifier' = FALSE, 'regression' = FALSE),
#'   two_round = list('classifier' = FALSE, 'regression' = FALSE),
#'
#'   # Convert Parameters
#'   convert_model = list('classifier' = NULL, 'regression' = NULL),
#'   convert_model_language = list('classifier' = "cpp", 'regression' = "cpp"),
#'
#'   # Objective Parameters
#'   boost_from_average = list('classifier' = TRUE, 'regression' = TRUE),
#'   is_unbalance = list('classifier' = FALSE, 'regression' = FALSE),
#'   scale_pos_weight = list('classifier' = 1.0, 'regression' = 1.0),
#'
#'   # Metric Parameters (metric is in Core)
#'   is_provide_training_metric = list('classifier' = TRUE, 'regression' = TRUE),
#'   eval_at = list('classifier' = c(1,2,3,4,5), 'regression' = c(1,2,3,4,5)),
#'
#'   # Network Parameters
#'   num_machines = list('classifier' = 1, 'regression' = 1),
#'
#'   # GPU Parameters
#'   gpu_platform_id = list('classifier' = -1, 'regression' = -1),
#'   gpu_device_id = list('classifier' = -1, 'regression' = -1),
#'   gpu_use_dp = list('classifier' = TRUE, 'regression' = TRUE),
#'   num_gpu = list('classifier' = 1, 'regression' = 1))
#' }
#' @return Returns a data.table of original series and forecasts, the catboost model objects (everything returned from AutoCatBoostRegression()), a time series forecast plot, and transformation info if you set TargetTransformation to TRUE. The time series forecast plot will plot your single series or aggregate your data to a single series and create a plot from that.
#' @export
AutoLightGBMHurdleCARMA <- function(data,
                                    NonNegativePred = FALSE,
                                    Threshold = NULL,
                                    RoundPreds = FALSE,
                                    TrainOnFull = FALSE,
                                    TargetColumnName = 'Target',
                                    DateColumnName = 'DateTime',
                                    HierarchGroups = NULL,
                                    GroupVariables = NULL,
                                    EncodingMethod = "credibility",
                                    TimeWeights = 1,
                                    FC_Periods = 30,
                                    TimeUnit = 'week',
                                    TimeGroups = c('weeks','months'),
                                    NumOfParDepPlots = 10L,
                                    TargetTransformation = FALSE,
                                    Methods = c('BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', 'Logit'),
                                    AnomalyDetection = NULL,
                                    XREGS = NULL,
                                    Lags = c(1L:5L),
                                    MA_Periods = c(2L:5L),
                                    SD_Periods = NULL,
                                    Skew_Periods = NULL,
                                    Kurt_Periods = NULL,
                                    Quantile_Periods = NULL,
                                    Quantiles_Selected = c('q5','q95'),
                                    Difference = TRUE,
                                    FourierTerms = 6L,
                                    CalendarVariables = c('second', 'minute', 'hour', 'wday', 'mday', 'yday', 'week', 'wom', 'isoweek', 'month', 'quarter', 'year'),
                                    HolidayVariable = c('USPublicHolidays','EasterGroup','ChristmasGroup','OtherEcclesticalFeasts'),
                                    HolidayLookback = NULL,
                                    HolidayLags = 1L,
                                    HolidayMovingAverages = 1L:2L,
                                    TimeTrendVariable = FALSE,
                                    ZeroPadSeries = NULL,
                                    DataTruncate = FALSE,
                                    SplitRatios = c(0.7, 0.2, 0.1),
                                    PartitionType = 'timeseries',
                                    Timer = TRUE,
                                    DebugMode = FALSE,
                                    EvalMetric = 'RMSE',
                                    GridTune = FALSE,
                                    PassInGrid = NULL,
                                    ModelCount = 100,
                                    MaxRunsWithoutNewWinner = 50,
                                    MaxRunMinutes = 24L*60L,

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

  # Purified args: see CARMA HELPER FUNCTIONS ----
  Args <- CARMA_Define_Args(TimeUnit=TimeUnit,TimeGroups=TimeGroups,HierarchGroups=HierarchGroups,GroupVariables=GroupVariables,FC_Periods=FC_Periods,PartitionType=PartitionType,TrainOnFull=TrainOnFull,SplitRatios=SplitRatios)
  IndepentVariablesPass <- Args$IndepentVariablesPass
  HoldOutPeriods <- Args$HoldOutPeriods
  HierarchGroups <- Args$HierarchGroups
  GroupVariables <- Args$GroupVariables
  TimeGroups <- Args$TimeGroups
  FC_Periods <- Args$FC_Periods
  TimeGroup <- Args$TimeGroupPlaceHolder
  TimeUnit <- Args$TimeUnit

  # Convert data to data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(XREGS) && !data.table::is.data.table(XREGS)) data.table::setDT(XREGS)

  # Variables for Program: Redefine HoldOutPerids ----
  if(!TrainOnFull) HoldOutPeriods <- round(SplitRatios[2L] * length(unique(data[[eval(DateColumnName)]])), 0L)

  # Feature Engineering: Add Zero Padding for missing dates ----
  if(DebugMode) print('Feature Engineering: Add Zero Padding for missing dates----')
  if(data[, .N] != unique(data)[, .N]) stop('There is duplicates in your data')
  if(!is.null(ZeroPadSeries)) {
    data <- TimeSeriesFill(data, DateColumnName=eval(DateColumnName), GroupVariables=GroupVariables, TimeUnit=TimeUnit, FillType=ZeroPadSeries, MaxMissingPercent=0.0, SimpleImpute=FALSE)
    data <- ModelDataPrep(data=data, Impute=TRUE, CharToFactor=FALSE, FactorToChar=FALSE, IntToNumeric=FALSE, LogicalToBinary=FALSE, DateToChar=FALSE, RemoveDates=FALSE, MissFactor='0', MissNum=0, IgnoreCols=NULL)
  } else {
    temp <- TimeSeriesFill(data, DateColumnName=eval(DateColumnName), GroupVariables=GroupVariables, TimeUnit=TimeUnit, FillType='maxmax', MaxMissingPercent=0.25, SimpleImpute=FALSE)
    if(temp[,.N] != data[,.N]) stop('There are missing dates in your series. You can utilize the ZeroPadSeries argument to handle this or manage it before running the function')
  }

  # Modify FC_Periods ----
  if(DebugMode) print('# Check lengths of XREGS')
  Output <- CarmaFCHorizon(data.=data, XREGS.=XREGS, TrainOnFull.=TrainOnFull, Difference.= Difference, FC_Periods.=FC_Periods, HoldOutPeriods.=HoldOutPeriods, DateColumnName.=DateColumnName)
  FC_Periods <- Output$FC_Periods
  HoldOutPeriods <- Output$HoldOutPeriods; rm(Output)

  # Merge data and XREG for Training ----
  if(DebugMode) print('merging xregs to data')
  if(!is.null(XREGS)) {
    Output <- CarmaMergeXREGS(data.=data, XREGS.=XREGS, TargetColumnName.=TargetColumnName, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName)
    data <- Output$data; Output$data <- NULL
    XREGS <- Output$XREGS; rm(Output)
  }

  # Set Keys for data.table usage ----
  if(DebugMode) print('# Set Keys for data.table usage ----')
  if(!is.null(GroupVariables)) {
    data.table::setkeyv(x = data, cols = c(eval(GroupVariables), eval(DateColumnName)))
    if(!is.null(XREGS)) data.table::setkeyv(x = XREGS, cols = c('GroupVar', eval(DateColumnName)))
  } else {
    data.table::setkeyv(x = data, cols = c(eval(DateColumnName)))
    if(!is.null(XREGS)) data.table::setkeyv(x = XREGS, cols = c(eval(DateColumnName)))
  }

  # Data Wrangling: Remove Unnecessary Columns ----
  if(DebugMode) print('Data Wrangling: Remove Unnecessary Columns ----')
  data <- CarmaSubsetColumns(data.=data, XREGS.=XREGS, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName, TargetColumnName.=TargetColumnName)

  # Feature Engineering: Concat Categorical Columns - easier to deal with this way ----
  if(DebugMode) print('Feature Engineering: Concat Categorical Columns - easier to deal with this way ----')
  if(!is.null(GroupVariables)) {
    data[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = GroupVariables]
    if(length(GroupVariables) > 1L) data[, eval(GroupVariables) := NULL] else if(GroupVariables != 'GroupVar') data[, eval(GroupVariables) := NULL]
  }

  # Variables for Program: Store unique values of GroupVar in GroupVarVector ----
  if(DebugMode) print('Variables for Program: Store unique values of GroupVar in GroupVarVector ----')
  if(!is.null(GroupVariables)) {
    GroupVarVector <- data.table::as.data.table(x = unique(as.character(data[['GroupVar']])))
    data.table::setnames(GroupVarVector, 'V1', 'GroupVar')
  }

  # Data Wrangling: Standardize column ordering ----
  if(DebugMode) print('Data Wrangling: Standardize column ordering ----')
  if(!is.null(GroupVariables)) data.table::setcolorder(data, c('GroupVar', eval(DateColumnName), eval(TargetColumnName))) else data.table::setcolorder(data, c(eval(DateColumnName), eval(TargetColumnName)))

  # Data Wrangling: Convert DateColumnName to Date or POSIXct ----
  if(DebugMode) print('Data Wrangling: Convert DateColumnName to Date or POSIXct ----')
  Output <- CarmaDateStandardize(data.=data, XREGS.=NULL, DateColumnName.=DateColumnName, TimeUnit.=TimeUnit)
  data <- Output$data; Output$data <- NULL
  XREGS <- Output$XREGS; rm(Output)

  # Data Wrangling: Ensure TargetColumnName is Numeric ----
  if(DebugMode) print('Data Wrangling: Ensure TargetColumnName is Numeric ----')
  if(!is.numeric(data[[eval(TargetColumnName)]])) data[, eval(TargetColumnName) := as.numeric(get(TargetColumnName))]

  # Variables for Program: Store number of data partitions in NumSets ----
  if(DebugMode) print('Variables for Program: Store number of data partitions in NumSets ----')
  NumSets <- length(SplitRatios)

  # Variables for Program: Store Maximum Value of TargetColumnName in val ----
  if(DebugMode) print('Variables for Program: Store Maximum Value of TargetColumnName in val ----')
  if(!is.null(Lags)) {
    if(is.list(Lags) && is.list(MA_Periods)) val <- max(unlist(Lags), unlist(MA_Periods)) else val <- max(Lags, MA_Periods)
  }

  # Data Wrangling: Sort data ----
  if(DebugMode) print('Data Wrangling: Sort data by GroupVar then DateColumnName ----')
  if(!is.null(GroupVariables)) data <- data[order(GroupVar, get(DateColumnName))] else data <- data[order(get(DateColumnName))]

  # Feature Engineering: Create Fourier Features ----
  if(DebugMode) print('Feature Engineering: Fourier Features ----')
  Output <- CarmaFourier(data.=data, XREGS.=XREGS, FourierTerms.=FourierTerms, TimeUnit.=TimeUnit, TargetColumnName.=TargetColumnName, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName, HierarchGroups.=HierarchGroups)
  FourierTerms <- Output$FourierTerms; Output$FourierTerms <- NULL
  FourierFC <- Output$FourierFC; Output$FourierFC <- NULL
  data <- Output$data; rm(Output)

  # Feature Engineering: Create Calendar Variables ----
  if(DebugMode) print('Feature Engineering: Add Create Calendar Variables ----')
  if(!is.null(CalendarVariables)) data <- CreateCalendarVariables(data=data, DateCols=eval(DateColumnName), AsFactor=FALSE, TimeUnits=CalendarVariables)

  # Feature Engineering: Create Holiday Variables ----
  if(DebugMode) print('Feature Engineering: Add Create Holiday Variables ----')
  if(!is.null(HolidayVariable)) {
    data <- CreateHolidayVariables(data, DateCols = eval(DateColumnName), LookbackDays = if(!is.null(HolidayLookback)) HolidayLookback else LB(TimeUnit), HolidayGroups = HolidayVariable, Holidays = NULL)
    if(!(tolower(TimeUnit) %chin% c('1min','5min','10min','15min','30min','hour'))) {
      data[, eval(DateColumnName) := lubridate::as_date(get(DateColumnName))]
    } else {
      data[, eval(DateColumnName) := as.POSIXct(get(DateColumnName))]
    }
  }

  # Anomaly detection by Group and Calendar Vars ----
  if(DebugMode) print('Anomaly detection by Group and Calendar Vars ----')
  if(!is.null(AnomalyDetection)) {
    data <- GenTSAnomVars(
      data = data, ValueCol = eval(TargetColumnName),
      GroupVars = if(!is.null(CalendarVariables) && !is.null(GroupVariables)) c('GroupVar', paste0(DateColumnName, '_', CalendarVariables[1])) else if(!is.null(GroupVariables)) 'GroupVar' else NULL,
      DateVar = eval(DateColumnName), KeepAllCols = TRUE, IsDataScaled = FALSE,
      HighThreshold = AnomalyDetection$tstat_high,
      LowThreshold = AnomalyDetection$tstat_low)
    data[, paste0(eval(TargetColumnName), '_zScaled') := NULL]
    data[, ':=' (RowNumAsc = NULL, CumAnomHigh = NULL, CumAnomLow = NULL, AnomHighRate = NULL, AnomLowRate = NULL)]
  }

  # Feature Engineering: Target Transformation ----
  if(DebugMode) print('Feature Engineering: Add Target Transformation ----')
  if(TargetTransformation) {
    TransformResults <- AutoTransformationCreate(data, ColumnNames=TargetColumnName, Methods=Methods, Path=NULL, TransID='Trans', SaveOutput=FALSE)
    data <- TransformResults$Data; TransformResults$Data <- NULL
    TransformObject <- TransformResults$FinalResults; rm(TransformResults)
  } else {
    TransformObject <- NULL
  }

  # Copy data for non grouping + difference ----
  if(DebugMode) print('Copy data for non grouping + difference ----')
  if(is.null(GroupVariables) && Difference) antidiff <- data.table::copy(data[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName))])

  # Feature Engineering: Differencing ----
  if(DebugMode) print('Feature Engineering: Add Difference Data ----')
  Output <- CarmaDifferencing(GroupVariables.=GroupVariables, Difference.=Difference, data.=data, TargetColumnName.=TargetColumnName, FC_Periods.=FC_Periods)
  data <- Output$data; Output$data <- NULL
  dataStart <- Output$dataStart; Output$dataStart <- NULL
  FC_Periods <- Output$FC_Periods; Output$FC_Periods <- NULL
  Train <- Output$Train; rm(Output)

  # Feature Engineering: Lags and Rolling Stats ----
  if(DebugMode) print('Feature Engineering: Lags and Rolling Stats ----')
  Output <- CarmaTimeSeriesFeatures(data.=data, TargetColumnName.=TargetColumnName, DateColumnName.=DateColumnName, GroupVariables.=GroupVariables, HierarchGroups.=HierarchGroups, Difference.=Difference, TimeGroups.=TimeGroups, TimeUnit.=TimeUnit, Lags.=Lags, MA_Periods.=MA_Periods, SD_Periods.=SD_Periods, Skew_Periods.=Skew_Periods, Kurt_Periods.=Kurt_Periods, Quantile_Periods.=Quantile_Periods, Quantiles_Selected.=Quantiles_Selected, HolidayVariable.=HolidayVariable, HolidayLags.=HolidayLags, HolidayMovingAverages.=HolidayMovingAverages, DebugMode.=DebugMode)
  IndependentSupplyValue <- Output$IndependentSupplyValue; Output$IndependentSupplyValue <- NULL
  HierarchSupplyValue <- Output$HierarchSupplyValue; Output$HierarchSupplyValue <- NULL
  GroupVarVector <- Output$GroupVarVector; Output$GroupVarVector <- NULL
  Categoricals <- Output$Categoricals; Output$Categoricals <- NULL
  data <- Output$data; rm(Output)

  # Create GroupVar ----
  if(!is.null(GroupVariables) && !'GroupVar' %chin% names(data)) data[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(GroupVariables)]

  # Data Wrangling: ModelDataPrep() to prepare data ----
  if(DebugMode) print('Data Wrangling: ModelDataPrep() to prepare data ----')
  data <- ModelDataPrep(data=data, Impute=TRUE, IntToNumeric=TRUE, DateToChar=FALSE, FactorToChar=FALSE, CharToFactor=TRUE, LogicalToBinary=FALSE, RemoveDates=FALSE, MissFactor='0', MissNum=-1, IgnoreCols=NULL)

  # Data Wrangling: Remove dates with imputed data from the DT_GDL_Feature_Engineering() features ----
  if(DebugMode) print('Data Wrangling: Remove dates with imputed data from the DT_GDL_Feature_Engineering() features ----')
  if(DataTruncate && !is.null(Lags)) data <- CarmaTruncateData(data.=data, DateColumnName.=DateColumnName, TimeUnit.=TimeUnit)

  # Feature Engineering: Add TimeTrend Variable ----
  if(DebugMode) print('Feature Engineering: Add TimeTrend Variable----')
  if(TimeTrendVariable) {
    if(!is.null(GroupVariables)) data[, TimeTrend := seq_len(.N), by = 'GroupVar'] else data[, TimeTrend := seq_len(.N)]
  }

  # Create TimeWeights ----
  if(DebugMode) print('Create TimeWeights ----')
  train <- CarmaTimeWeights(train.=data, TimeWeights.=TimeWeights, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName)

  # Store Date Info ----
  FutureDateData <- unique(data[, get(DateColumnName)])

  # Data Wrangling: Partition data ----
  if(DebugMode) print('Data Wrangling: Partition data with AutoDataPartition()----')
  Output <- CarmaPartition(data.=data, SplitRatios.=if(TrainOnFull) NULL else SplitRatios, TrainOnFull.=TrainOnFull, NumSets.=NumSets, PartitionType.=PartitionType, GroupVariables.=GroupVariables, DateColumnName.=DateColumnName)
  train <- Output$train; Output$train <- NULL
  valid <- Output$valid; Output$valid <- NULL
  data <- Output$data; Output$data <- NULL
  test <- Output$test; rm(Output)

  # Variables for CARMA function IDcols ----
  if(DebugMode) print('Variables for CARMA function:IDcols----')
  IDcols <- names(data)[which(names(data) %chin% DateColumnName)]
  IDcols <- c(IDcols, names(data)[which(names(data) == TargetColumnName)])

  # Data Wrangling: copy data or train for later in function since AutoRegression will modify data and train ----
  if(DebugMode) print('Data Wrangling: copy data or train for later in function since AutoRegression will modify data and train ----')
  if(!is.null(GroupVariables)) data.table::setorderv(x = data, cols = c('GroupVar',eval(DateColumnName)), order = c(1,1)) else data.table::setorderv(x = data, cols = c(eval(DateColumnName)), order = c(1))
  Step1SCore <- data.table::copy(data)

  # Define Target and Features ----
  if(DebugMode) print('Define ML args ----')
  Output <- CarmaFeatures(data.=data, train.=train, XREGS.=XREGS, Difference.=Difference, TargetColumnName.=TargetColumnName, DateColumnName.=DateColumnName, GroupVariables.=GroupVariables)
  ModelFeatures <- Output$ModelFeatures
  TargetVariable <- Output$TargetVariable; rm(Output)

  # Switch up TrainOnFull if SplitRatios is not null ----
  if(!is.null(SplitRatios) || !TrainOnFull) TOF <- FALSE else TOF <- TRUE

  # Run AutoLightGBMHurdleModel() and return list of ml objects ----
  if(DebugMode) print('Run AutoXGBoostHurdleModel() and return list of ml objects ----')
  TestModel <- AutoLightGBMHurdleModel(

    # GPU or CPU and the number of available GPUs
    ModelID = 'ModelTest',
    SaveModelObjects = FALSE,
    ReturnModelObjects = TRUE,

    # Data related args
    data = data.table::copy(train),
    TrainOnFull = TrainOnFull,
    ValidationData = data.table::copy(valid),
    TestData = data.table::copy(test),
    Buckets = 0L,
    TargetColumnName = TargetVariable,
    FeatureColNames = ModelFeatures,
    PrimaryDateColumn = eval(DateColumnName),
    WeightsColumnName = if('Weights' %chin% names(train)) 'Weights' else NULL,
    IDcols = IDcols,
    EncodingMethod = EncodingMethod,
    DebugMode = DebugMode,

    # Metadata args
    Paths = getwd(),
    MetaDataPaths = NULL,
    TransformNumericColumns = NULL,
    Methods = NULL,
    ClassWeights = c(1,1),
    SplitRatios = c(0.70, 0.20, 0.10),
    NumOfParDepPlots = NumOfParDepPlots,

    # Grid tuning setup
    PassInGrid = PassInGrid,
    GridTune = GridTune,
    BaselineComparison = 'default',
    MaxModelsInGrid = 500L,
    MaxRunsWithoutNewWinner = 100L,
    MaxRunMinutes = 60*60,

    # Core parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
    input_model = input_model,
    task = task,
    device_type = device_type,
    objective = objective,
    metric = metric,
    boosting = boosting,
    LinearTree = LinearTree,
    Trees = Trees,
    eta = eta,
    num_leaves = num_leaves,
    deterministic = deterministic,

    # Learning Parameters https://lightgbm.readthedocs.io/en/latest/Parameters.html#learning-control-parameters
    force_col_wise = force_col_wise,
    force_row_wise = force_row_wise,
    max_depth = max_depth,
    min_data_in_leaf = min_data_in_leaf,
    min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
    bagging_freq = bagging_freq,
    bagging_fraction = bagging_fraction,
    feature_fraction = feature_fraction,
    feature_fraction_bynode = feature_fraction_bynode,
    extra_trees = extra_trees,
    early_stopping_round = early_stopping_round,
    first_metric_only = first_metric_only,
    max_delta_step = max_delta_step,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    linear_lambda = linear_lambda,
    min_gain_to_split = min_gain_to_split,
    drop_rate_dart = drop_rate_dart,
    max_drop_dart = max_drop_dart,
    skip_drop_dart = skip_drop_dart,
    uniform_drop_dart = uniform_drop_dart,
    top_rate_goss = top_rate_goss,
    other_rate_goss = other_rate_goss,
    monotone_constraints = monotone_constraints,
    monotone_constraints_method = monotone_constraints_method,
    monotone_penalty = monotone_penalty,
    forcedsplits_filename = forcedsplits_filename,
    refit_decay_rate = refit_decay_rate,
    path_smooth = path_smooth,

    # IO Dataset Parameters
    max_bin = max_bin,
    min_data_in_bin = min_data_in_bin,
    data_random_seed = data_random_seed,
    is_enable_sparse = is_enable_sparse,
    enable_bundle = enable_bundle,
    use_missing = use_missing,
    zero_as_missing = zero_as_missing,
    two_round = two_round,

    # Convert Parameters
    convert_model = convert_model,
    convert_model_language = convert_model_language,

    # Objective Parameters
    boost_from_average = boost_from_average,
    is_unbalance = is_unbalance,
    scale_pos_weight = scale_pos_weight,

    # Metric Parameters (metric is in Core)
    is_provide_training_metric = is_provide_training_metric,
    eval_at = eval_at,

    # Network Parameters
    num_machines = num_machines,

    # GPU Parameters
    gpu_platform_id = gpu_platform_id,
    gpu_device_id = gpu_device_id,
    gpu_use_dp = gpu_use_dp,
    num_gpu = num_gpu)

  # Grab threshold if turned on ----
  if(!is.null(Threshold)) {
    threshold <- TestModel$ClassifierModel$EvaluationMetrics
    col <- names(threshold)[grep(pattern = Threshold, x = names(threshold))]
    Threshold <- threshold[, .SD, .SDcols = c('Threshold', eval(col))][order(-get(col))][1,1][[1]]
  }

  # Return model object for when TrainOnFull is FALSE ----
  if(!TrainOnFull) return(TestModel)

  # Turn warnings into errors back on
  if(DebugMode) options(warn = 2)

  # Variable for interation counts: max number of rows in Step1SCore data.table across all group ----
  if(DebugMode) print('Variable for interation counts: max number of rows in Step1SCore data.table across all group ----')
  N <- CarmaRecordCount(GroupVariables.=GroupVariables,Difference.=Difference, Step1SCore.=Step1SCore)

  #----

  #----

  # ARMA PROCESS FORECASTING ----
  if(DebugMode) print('ARMA PROCESS FORECASTING----')
  for(i in seq_len(FC_Periods+1L)) {

    # Row counts----
    if(DebugMode) print('Row counts----')
    if(i != 1) N <- as.integer(N + 1L)

    # Machine Learning: Generate predictions ----
    if(DebugMode) print('Machine Learning: Generate predictions----')
    if(i == 1L) {
      if(!is.null(GroupVariables)) {

        # Score model ----
        print("here yo")
        print(Step1SCore)
        Preds <- AutoLightGBMHurdleModelScoring(
          TestData = data.table::copy(Step1SCore),
          Path = NULL,
          ModelID = 'ModelTest',
          ModelList = TestModel$ModelList,
          ArgsList = TestModel$ArgsList,
          Threshold = Threshold,
          CARMA = TRUE)

        # Modify data to match AutoCatBoostCARMA output ----
        data.table::set(Preds, j = c(names(Preds)[2L:5L]), value = NULL)
        data.table::set(Preds, j = eval(DateColumnName), value = NULL)
        data.table::setnames(Preds, 'UpdatedPrediction', 'Predictions')
        data.table::setcolorder(Preds, c(2L,1L,3L:ncol(Preds)))

        # Rounding ----
        if(RoundPreds) Preds[, Predictions := round(Predictions)]

      } else {

        # Score model ----
        Preds <- AutoLightGBMHurdleModelScoring(
          TestData = data.table::copy(Step1SCore),
          Path = NULL,
          ModelID = 'ModelTest',
          ModelList = TestModel$ModelList,
          ArgsList = TestModel$ArgsList,
          Threshold = Threshold,
          CARMA = TRUE)

        # Modify data to match AutoCatBoostCARMA output ----
        data.table::set(Preds, j = c(names(Preds)[2L:5L]), value = NULL)
        if(DateColumnName %chin% names(Preds)) data.table::set(Preds, j = eval(DateColumnName), value = NULL)
        data.table::setnames(Preds, 'UpdatedPrediction', 'Predictions')
        data.table::setcolorder(Preds, c(2L,1L,3L:ncol(Preds)))

        # Rounding ----
        if(RoundPreds) Preds[, Predictions := round(Predictions)]
      }

      # Data Wrangline: grab historical data and one more future record ----
      if(Difference) {
        if(eval(TargetColumnName) %chin% names(Step1SCore) && eval(TargetColumnName) %chin% names(Preds)) {
          data.table::set(Preds, j = eval(TargetColumnName), value = NULL)
        }
        if(eval(DateColumnName) %chin% names(Step1SCore)) data.table::set(Step1SCore, j = eval(DateColumnName), value = NULL)
        if(eval(DateColumnName) %chin% names(Preds)) data.table::set(Preds, j = eval(DateColumnName), value = NULL)
        if(!is.null(GroupVariables)) {
          UpdateData <- cbind(FutureDateData, Step1SCore[, .SD, .SDcols = eval(TargetColumnName)],Preds)
        } else {
          UpdateData <- cbind(FutureDateData[2L:(nrow(Step1SCore)+1L)], Step1SCore[, .SD, .SDcols = eval(TargetColumnName)],Preds)
        }
        data.table::setnames(UpdateData, 'FutureDateData', eval(DateColumnName))
      } else {
        if(NonNegativePred) Preds[, Predictions := data.table::fifelse(Predictions < 0.5, 0, Predictions)]
        UpdateData <- cbind(FutureDateData[1L:N], Preds)
        data.table::setnames(UpdateData, c('V1'), c(eval(DateColumnName)))
      }

    } else {
      if(!is.null(GroupVariables)) {

        # Modify target reference ----
        if(Difference) IDcols = 'ModTarget' else IDcols <- eval(TargetColumnName)

        # GroupVar or Hierarchical----
        if(!is.null(HierarchGroups)) {
          temp <- data.table::copy(UpdateData[, ID := seq_len(.N), by = c(eval(GroupVariables))])
          temp <- temp[ID == N][, ID := NULL]
        } else {
          temp <- data.table::copy(UpdateData[, ID := seq_len(.N), by = 'GroupVar'])
          temp <- temp[ID == N][, ID := NULL]
        }

        # Remove 'Prediction' column
        if('Predictions' %chin% names(temp)) data.table::set(temp, j = 'Predictions', value = NULL)

        # Score model ----
        Preds <- AutoLightGBMHurdleModelScoring(
          TestData = temp,
          Path = NULL,
          ModelID = 'ModelTest',
          ModelList = TestModel$ModelList,
          ArgsList = TestModel$ArgsList,
          Threshold = Threshold,
          CARMA = TRUE)

        # Modify data ----
        Preds[, (setdiff(names(Preds),'UpdatedPrediction')) := NULL]
        data.table::setnames(Preds, 'UpdatedPrediction', 'Predictions')

        # Rounding ----
        if(RoundPreds) Preds[, Predictions := round(Predictions)]

        # Update data group case----
        if(DebugMode) print('Update data group case----')
        data.table::setnames(Preds, 'Predictions', 'Preds')
        if(NonNegativePred & !Difference) Preds[, Preds := data.table::fifelse(Preds < 0.5, 0, Preds)]
        Preds <- cbind(UpdateData[ID == N], Preds)
        if(Difference) Preds[, ModTarget := Preds][, eval(TargetColumnName) := Preds] else Preds[, eval(TargetColumnName) := Preds]
        Preds[, Predictions := Preds][, Preds := NULL]
        UpdateData <- UpdateData[ID != N]
        if(any(class(UpdateData$Date) %chin% c('POSIXct','POSIXt')) && any(class(Preds$Date) == 'Date')) UpdateData[, eval(DateColumnName) := as.Date(get(DateColumnName))]
        UpdateData <- data.table::rbindlist(list(UpdateData, Preds))
        if(Difference) UpdateData[ID %in% c(N-1,N), eval(TargetColumnName) := cumsum(get(TargetColumnName)), by = 'GroupVar']
        UpdateData[, ID := NULL]

      } else {

        # Score model ----
        temp <- UpdateData[.N, .SD, .SDcols = c(setdiff(names(UpdateData), "Predictions"))]
        Preds <- AutoLightGBMHurdleModelScoring(
          TestData = temp,
          Path = NULL,
          ModelID = 'ModelTest',
          ModelList = TestModel$ModelList,
          ArgsList = TestModel$ArgsList,
          Threshold = Threshold,
          CARMA = TRUE)

        # Modify data ----
        Preds[, (setdiff(names(Preds),'UpdatedPrediction')) := NULL]
        data.table::setnames(Preds, 'UpdatedPrediction', 'Predictions')

        # Rounding ----
        if(RoundPreds) Preds[, Predictions := round(Predictions)]

        # Update data non-group case ----
        if(DebugMode) print('Update data non-group case----')
        data.table::set(UpdateData, i = UpdateData[, .N], j = which(names(UpdateData) %chin% c(TargetColumnName, "Predictions")), value = Preds[[1L]])
      }
    }

    # Update lags and moving average features for next run ----
    if(i != FC_Periods+1L) {

      # Timer----
      if(DebugMode) print('Timer----')
      if(Timer) if(i != 1) print(paste('Forecast future step: ', i-1))
      if(Timer) starttime <- Sys.time()

      # Create single future record ----
      if(DebugMode) print('Create single future record ----')
      CalendarFeatures <- NextTimePeriod(UpdateData.=UpdateData, TimeUnit.=TimeUnit, DateColumnName.=DateColumnName)

      # Update flat feature engineering ----
      if(DebugMode) print('Update feature engineering ----')
      UpdateData <- UpdateFeatures(UpdateData.=UpdateData, GroupVariables.=GroupVariables, CalendarFeatures.=CalendarFeatures, CalendarVariables.=CalendarVariables, GroupVarVector.=GroupVarVector, DateColumnName.=DateColumnName, XREGS.=XREGS, FourierTerms.=FourierTerms, FourierFC.=FourierFC, TimeGroups.=TimeGroups, TimeTrendVariable.=TimeTrendVariable, N.=N, TargetColumnName.=TargetColumnName, HolidayVariable.=HolidayVariable, HolidayLookback.=HolidayLookback, TimeUnit.=TimeUnit, AnomalyDetection.=AnomalyDetection, i.=i)

      # Update Lags and MA's ----
      if(DebugMode) print('Update Lags and MAs ----')
      UpdateData <- CarmaRollingStatsUpdate(ModelType='catboost', DebugMode.=DebugMode, UpdateData.=UpdateData, GroupVariables.=GroupVariables, Difference.=Difference, CalendarVariables.=CalendarVariables, HolidayVariable.=HolidayVariable, IndepVarPassTRUE.=IndepentVariablesPass, data.=data, CalendarFeatures.=CalendarFeatures, XREGS.=XREGS, HierarchGroups.=HierarchGroups, GroupVarVector.=GroupVarVector, TargetColumnName.=TargetColumnName, DateColumnName.=DateColumnName, Preds.=Preds, HierarchSupplyValue.=HierarchSupplyValue, IndependentSupplyValue.=IndependentSupplyValue, TimeUnit.=TimeUnit, TimeGroups.=TimeGroups, Lags.=Lags, MA_Periods.=MA_Periods, SD_Periods.=SD_Periods, Skew_Periods.=Skew_Periods, Kurt_Periods.=Kurt_Periods, Quantile_Periods.=Quantile_Periods, Quantiles_Selected.=Quantiles_Selected, HolidayLags.=HolidayLags, HolidayMovingAverages.=HolidayMovingAverages)

      # WeightsColumn
      if("Weights" %chin% names(UpdateData)) data.table::set(UpdateData, i = N+1L, j = "Weights", value = 1.0)

      # Print time to complete ----
      if(Timer) endtime <- Sys.time()
      if(Timer && i != 1) print(endtime - starttime)
    }
  }

  # Memory support ----
  gc()

  # Return data prep ----
  if(DebugMode) print('Return data prep ----')
  Output <- CarmaReturnDataPrep(UpdateData.=UpdateData, FutureDateData.=FutureDateData, dataStart.=dataStart, DateColumnName.=DateColumnName, TargetColumnName.=TargetColumnName, GroupVariables.=GroupVariables, Difference.=Difference, TargetTransformation.=TargetTransformation, TransformObject.=TransformObject, NonNegativePred.=NonNegativePred)
  UpdateData <- Output$UpdateData; Output$UpdateData <- NULL
  TransformObject <- Output$TransformObject; rm(Output)

  # No Group ----
  if(is.null(GroupVariables) && "Predictions0" %chin% names(UpdateData)) data.table::set(UpdateData, j = 'Predictions0', value = NULL)

  # Return ----
  return(list(
    Forecast = UpdateData,
    ModelInformation = TestModel,
    TransformationDetail = if(exists('TransformObject') && !is.null(TransformObject)) TransformObject else NULL))
}
