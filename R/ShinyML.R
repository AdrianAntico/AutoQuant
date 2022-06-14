#' @title Shiny.ML.CatBoost.GridEvalMetricsOptions
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @keywords internal
Shiny.ML.CatBoost.GridEvalMetricsOptions <- function(TT) {
  if(TT == 'Regression') {
    choices <- c('mae','mape','rmse','r2')
    default <- 'rmse'
  } else if(TT == 'Binary Classification') {
    choices <- c('Utility','MCC','Acc','F1_Score','F2_Score','F0.5_Score','TPR','TNR','FNR','FPR','FDR','FOR','NPV','PPV','ThreatScore')
    default <- 'Utility'
  } else if(TT == 'MultiClass') {
    choices <- c('accuracy','microauc','logloss')
    default <- 'microauc'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.CatBoost.EvalMetricOptions
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @keywords internal
Shiny.ML.CatBoost.EvalMetricOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c('RMSE','MAE','MAPE','R2','Poisson','MedianAbsoluteError','SMAPE','MSLE','NumErrors','FairLoss','Tweedie','Huber','LogLinQuantile','Quantile','Lq','Expectile','MultiRMSE')
    default <- 'RMSE'
  } else if(TT == 'Binary Classification') {
    choices <- c('Logloss','CrossEntropy','Precision','Recall','F1','BalancedAccuracy','BalancedErrorRate','MCC','Accuracy','CtrFactor','AUC','BrierScore','HingeLoss','HammingLoss','ZeroOneLoss','Kappa','WKappa','LogLikelihoodOfPrediction','TotalF1','PairLogit','PairLogitPairwise','PairAccuracy','QueryCrossEntropy','QuerySoftMax','PFound','NDCG','AverageGain','PrecisionAt','RecallAt','MAP')
    default <- 'MCC'
  } else if(TT == 'MultiClass') {
    choices <- c('MultiClass','MultiClassOneVsAll','AUC','TotalF1','MCC','Accuracy','HingeLoss','HammingLoss','ZeroOneLoss','Kappa','WKappa')
    default <- 'MultiClassOneVsAll'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.XGBoost.EvalMetricOptions
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @keywords internal
Shiny.ML.XGBoost.EvalMetricOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c('rmse','mae','mape')
    default <- 'r2'
  } else if(TT == 'Binary Classification') {
    choices <- c('logloss','error','aucpr','auc')
    default <- 'logloss'
  } else if(TT == 'MultiClass') {
    choices <- c('merror','mlogloss')
    default <- 'mlogloss'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.CatBoost.LossFunctionOptions
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @keywords internal
Shiny.ML.CatBoost.LossFunctionOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c('MAPE','MAE','RMSE','Poisson','Tweedie','Huber','LogLinQuantile','Quantile','Lq','Expectile','MultiRMSE')
    default <- 'RMSE'
  } else if(TT == 'Binary Classification') {
    choices <- c('Logloss','CrossEntropy','Lq','PairLogit','PairLogitPairwise','YetiRank','YetiRankPairwise','QueryCrossEntropy','QuerySoftMax')
    default <- 'Logloss'
  } else if(TT == 'MultiClass') {
    choices <- c('MultiClass','MultiClassOneVsAll')
    default <- 'MultiClassOneVsAll'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.XGBoost.LossFunctionOptions
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @keywords internal
Shiny.ML.XGBoost.LossFunctionOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c('reg:squaredlogerror', 'reg:pseudohubererror', 'count:poisson', 'survival:cox', 'survival:aft', 'aft_loss_distribution', 'reg:gamma', 'reg:tweedie')
    default <- 'reg:squaredlogerror'
  } else if(TT == 'Binary Classification') {
    choices <- c('reg:logistic', 'binary:logistic')
    default <- 'reg:logistic'
  } else if(TT == 'MultiClass') {
    choices <- c('multi:softprob')
    default <- 'multi:softprob'
  }
  return(list(Choices = choices, Default = default))
}

#' @title LightGBMLossFunctionOptions
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @keywords internal
Shiny.ML.LightGBM.LossFunctionOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c('rmse', 'l1', 'l2', 'quantile', 'mape', 'huber', 'fair', 'poisson', 'gamma', 'gamma_deviance', 'tweedie', 'ndcg')
    default <- 'rmse'
  } else if(TT == 'Binary Classification') {
    choices <- c('binary_logloss', 'average_precision', 'auc', 'map', 'binary_error', 'auc_mu')
    default <- 'binary_logloss'
  } else if(TT == 'MultiClass') {
    choices <- c('multiclass', 'multiclassova')
    default <- 'multiclass'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.CatBoost_Params
#'
#' @param l ArgsList created inside server.R
#' @param input input from shiny app
#' @param Debug Debug. TRUE or FALSE
#' @param TT E.g. CatBoost_TargetType. Can be 'Regression', 'Binary Classification', or 'MultiClass'. Case sensitive
#'
#' @keywords internal
Shiny.ML.CatBoost_Params <- function(l,input,Debug,TT) {

  # CatBoost ML Algo Specific
  l[['task_type']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_task_type']]}, error=function(x) NULL), Type='character', Default='CPU')
  l[['NumGPUs']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_NumGPUs']]}, error=function(x) NULL), Type='numeric', Default=1)

  # CatBoost ML Parameters
  l[['Trees']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_Trees']]}, error=function(x) NULL), Type='numeric', Default=1000)
  l[['Depth']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_Depth']]}, error=function(x) NULL), Type='numeric', Default=8)
  l[['LearningRate']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_LearningRate']]}, error=function(x) NULL), Type='numeric', Default=NULL)
  l[['L2_Leaf_Reg']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_L2_Leaf_Reg']]}, error=function(x) NULL), Type='numeric', Default=NULL)
  l[['model_size_reg']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_model_size_reg']]}, error=function(x) NULL), Type='numeric', Default=0.50)
  l[['langevin']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_langevin']]}, error=function(x) NULL), Type='logical', Default=FALSE)
  l[['diffusion_temperature']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_diffusion_temperature']]}, error=function(x) NULL), Type='numeric', Default=10000)
  l[['RandomStrength']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_RandomStrength']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['BorderCount']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_BorderCount']]}, error=function(x) NULL), Type='numeric', Default=256)
  l[['RSM']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_RSM']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['BootStrapType']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_BootStrapType']]}, error=function(x) NULL), Type='character', Default='Bayesian')
  l[['GrowPolicy']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_GrowPolicy']]}, error=function(x) NULL), Type='character', Default='SymmetricTree')
  l[['feature_border_type']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_feature_border_type']]}, error=function(x) NULL), Type='character', Default='GreedyLogSum')
  l[['subsample']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_subsample']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['score_function']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_score_function']]}, error=function(x) NULL), Type='character', Default='Cosine')
  l[['min_data_in_leaf']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_min_data_in_leaf']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['sampling_unit']] <- 'Object'

  print(' ::  BuildModels 7  :: ')

  print(TT)
  if(TT == 'Regression') {
    print(' ::  BuildModels 8.1 :: ')
    l[['loss_function']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='RMSE')
    l[['loss_function_value']] <- 1.5
    l[['eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='RMSE')
    l[['eval_metric_value']] <- 1.5
    l[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='mse')
    l[['MetricPeriods']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_MetricPeriods']]}, error=function(x) NULL), Type='numeric', Default=10)
  } else if(TT == 'Binary Classification') {
    print(' ::  BuildModels 8.2 :: ')
    l[['LossFunction']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='Logloss')
    l[['EvalMetric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='AUC')
    l[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='MCC')
    cw0 <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_ClassWeights0']]}, error=function(x) NULL), Type='numeric', Default=1)
    cw1 <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_ClassWeights1']]}, error=function(x) NULL), Type='numeric', Default=1)
    l[['ClassWeights']] <- c(cw0, cw1)
    l[['MetricPeriods']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_MetricPeriods']]}, error=function(x) NULL), Type='numeric', Default=10)
  } else if(TT == 'MultiClass') {
    print(' ::  BuildModels 8.3 :: ')
    l[['loss_function']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='MultiClassOneVsAll')
    l[['eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='MultiClassOneVsAll')
    l[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='microauc')
    l[['MetricPeriods']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['CatBoost_MetricPeriods']]}, error=function(x) NULL), Type='numeric', Default=10)
  }
  return(l)
}

#' @title Shiny.ML.XGBoost_Params
#'
#' @param l ArgsList created inside server.R
#' @param input input from shiny app
#' @param Debug Debug. TRUE or FALSE
#' @param TT E.g. XGBoost_TargetType. Can be 'Regression', 'Binary Classification', or 'MultiClass'. Case sensitive
#'
#' @keywords internal
Shiny.ML.XGBoost_Params <- function(l,input,Debug,TT) {

  # XGBoost ML Parameters
  l[['Trees']]            <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_Trees']]}, error=function(x) NULL), Type='numeric', Default=1000)
  l[['max_depth']]        <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_max_depth']]}, error=function(x) NULL), Type='numeric', Default=8)
  l[['eta']]              <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_eta']]}, error=function(x) NULL), Type='numeric', Default=NULL)
  l[['min_child_weight']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_min_child_weight']]}, error=function(x) NULL), Type='numeric', Default=NULL)
  l[['subsample']]        <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_subsample']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['colsample_bytree']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_colsample_bytree']]}, error=function(x) NULL), Type='numeric', Default=1)

  if(Debug) print(' ::  BuildModels 7 :: ')
  if(Debug) print(TT)

  # XGBoost Eval Parameters
  if(TT == 'Regression') {
    print(' ::  BuildModels 7.1 :: ')
    l[['LossFunction']]     <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='reg:squarederror')
    l[['eval_metric']]      <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='rmse')
    l[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='r2')
  } else if(TT == 'Binary Classification') {
    print(' ::  BuildModels 7.2 :: ')
    l[['LossFunction']]     <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='binary:logistic')
    l[['eval_metric']]      <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='auc')
    l[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='MCC')
  } else {
    print(' ::  BuildModels 7.3 :: ')
    if(Debug) print(tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL))
    if(Debug) print(RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='multi:softprob'))
    l[['LossFunction']]     <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='multi:softprob')
    l[['eval_metric']]      <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='mlogloss')
    l[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='microauc')
  }
  return(l)
}

#' @title Shiny.ML.LightGBM_Params
#'
#' @param l ArgsList created inside server.R
#' @param input input from shiny app
#' @param Debug Debug. TRUE or FALSE
#' @param TT E.g. XGBoost_TargetType. Can be 'Regression', 'Binary Classification', or 'MultiClass'. Case sensitive
#'
#' @keywords internal
Shiny.ML.LightGBM_Params <- function(l,input,Debug,TT) {

  print(' ::  BuildModels 7  :: ')

  # LightGBM ML Parameters
  l[['Trees']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_Trees']]}, error=function(x) NULL), Type='numeric', Default=1000)
  l[['max_depth']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_max_depth']]}, error=function(x) NULL), Type='numeric', Default=8)
  l[['eta']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_eta']]}, error=function(x) NULL), Type='numeric', Default=NULL)
  l[['min_data_in_leaf']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_min_data_in_leaf']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['num_leaves']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_num_leaves']]}, error=function(x) NULL), Type='numeric', Default=31)
  l[['bagging_fraction']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_bagging_fraction']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['feature_fraction']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_feature_fraction']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['feature_fraction_bynode']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_feature_fraction_bynode']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['lambda_l1']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_lambda_l1']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['lambda_l2']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_lambda_l2']]}, error=function(x) NULL), Type='numeric', Default=1)

  # LightGBM Eval Parameters
  if(TT == 'Regression') {
    print(' ::  BuildModels 8.1  :: ')
    l[['objective']] <- 'regression'
    l[['metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_metric']]}, error=function(x) NULL), Type='character', Default='rmse')
    l[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='mse')
  } else if(TT == 'Binary Classification') {
    print(' ::  BuildModels 8.2  :: ')
    l[['objective']] <- 'binary'
    l[['metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_metric']]}, error=function(x) NULL), Type='character', Default='binary_logloss')
    l[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='MCC')
  } else if(TT == 'MultiClass') {
    print(' ::  BuildModels 8.3  :: ')
    l[['objective']] <- 'multiclass'
    l[['metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_metric']]}, error=function(x) NULL), Type='character', Default='multiclass')
    l[['grid_eval_metric']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='Accuracy')
  }
  return(l)
}

#' @title Shiny.ML.Trainer
#'
#' @param input shiny input
#' @param output shiny output
#' @param DataList DataList stores data in app
#' @param ArgsList CatBoostArgsList, XGBoostArgsList
#' @param TT TargetType
#' @param ML_ExperimentTable Collection table
#' @param run Cross Validation Iteration Number in app
#' @param n Total Cross Validation Iterations in app
#' @param Debug Debug from app
#' @param Algo 'CatBoost', 'XGBoost', 'LightGBM'
#'
#' @return a list of columns names by data type
#'
#' @keywords internal
Shiny.ML.Trainer <- function(input,
                             output,
                             DataList,
                             ArgsList,
                             TT,
                             ML_ExperimentTable,
                             run,
                             n,
                             Debug,
                             Algo = 'CatBoost') {
  # Model Pre Checks
  temp <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_data')]]}, error=function(x) NULL), Type='character', Default=NULL)
  if(length(temp) != 0) ArgsList[['data']] <- DataList[[temp]] else ArgsList[['data']] <- NULL
  ArgsList[['TargetColumnName']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_TargetColumnName')]]}, error=function(x) NULL), Type='character', Default=NULL)
  ArgsList[['FeatureColNames']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_FeatureColNames')]]}, error=function(x) NULL), Type='character', Default=NULL)
  print("class(ArgsList[['data']])[[1L]] %in% 'data.table' && length(ArgsList[['TargetColumnName']]) != 0L && length(ArgsList[['FeatureColNames']]) != 0L")

  # If run < n, combine data so that
  #   data is randomized and partitioned in the
  #   Auto__() function for Cross Validation Purposes
  RunReady <- FALSE
  if(class(ArgsList[['data']])[[1L]] %in% 'data.table' && length(ArgsList[['TargetColumnName']]) != 0L && length(ArgsList[['FeatureColNames']]) != 0L) {
    RunReady <- TRUE
    if(n > 1L) {
      if(class(ArgsList[['ValidationData']])[[1L]] %in% 'data.table') {
        ArgsList[['data']] <- data.table::rbindlist(list(ArgsList[['data']], ArgsList[['ValidationData']]), use.names = TRUE, fill = TRUE)
        ArgsList[['ValidationData']] <- NULL
      }
      if(class(ArgsList[['TestData']])[[1L]] %in% 'data.table') {
        ArgsList[['data']] <- data.table::rbindlist(list(ArgsList[['data']], ArgsList[['TestData']]), use.names = TRUE, fill = TRUE)
        ArgsList[['TestData']] <- NULL
      }
    }
  }
  print(' ::  BuildModels 1  :: ')

  # CatBoost ML Build
  if(!RunReady) {

    return(NULL)

  } else {

    # Starting logic: 100 rows - 100 rows = 0 row, need 1 row
    iter <- ML_ExperimentTable[, .N] - ML_ExperimentTable[ProjectID == 'zzz', .N] + 1L

    # Target Type
    if(TT == 'Regression' && class(ArgsList[['data']][[ArgsList[['TargetColumnName']]]])[1L] %in% c('character','factor')) {
      TT <- 'MultiClass'
    } else if(TT == 'MultiClass' && class(ArgsList[['data']][[ArgsList[['TargetColumnName']]]])[1L] %in% c('numeric','integer')) {
      TT <- 'Regression'
    } else if(TT == 'Binary Classification' && class(ArgsList[['data']][[ArgsList[['TargetColumnName']]]])[1L] %in% c('character','factor')) {
      TT <- 'MultiClass'
    }
    print(paste0('TT = ', TT))

    # Notification of starting
    shiny::showNotification(paste0(Algo, 'Building has Begun!'))

    # CatBoost MetaData Parameters
    if(run == n) {
      ArgsList[['OutputSelection']] <- c('Importances','EvalMetrics','EvalPlots','Score_TrainData')
    } else {
      ArgsList[['OutputSelection']] <- c('EvalMetrics','Score_TrainData','Importances')
    }
    ArgsList[['TrainOnFull']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_TrainOnFull')]]}, error=function(x) NULL), Type='logical', Default=FALSE)
    ArgsList[['ModelID']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_ModelID')]]}, error=function(x) NULL), Type='character', Default='Model_1')
    ArgsList[['DebugMode']] <- Debug
    ArgsList[['model_path']] <- getwd()
    ArgsList[['metadata_path']] <- getwd()
    ArgsList[['SaveModelObjects']] <- FALSE
    if(TT != 'MultiClass') ArgsList[['SaveInfoToPDF']] <- FALSE
    ArgsList[['ReturnModelObjects']] <- TRUE
    ArgsList[['NumOfParDepPlots']] <- 1

    print(' ::  BuildModels 2  :: ')

    # Data Parameters

    # Data
    temp <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_ValidationData')]]}, error=function(x) NULL), Type='character', Default=NULL)
    if(Debug) {print("RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_ValidationData')]]}, error=function(x) NULL), Type='character', Default=NULL)"); print(temp)}
    if(length(temp) != 0) ArgsList[['ValidationData']] <- DataList[[temp]] else ArgsList[['ValidationData']] <- NULL
    temp <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_TestData')]]}, error=function(x) NULL), Type='character', Default=NULL)
    if(Debug) {print("RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_TestData')]]}, error=function(x) NULL), Type='character', Default=NULL)"); print(temp)}
    if(length(temp) != 0) ArgsList[['TestData']] <- DataList[[temp]] else ArgsList[['TestData']] <- NULL

    print(' ::  BuildModels 3  :: ')

    # Data Args
    ArgsList[['WeightsColumnName']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_WeightsColumnName')]]}, error=function(x) NULL), Type='character', Default=NULL)
    ArgsList[['IDcols']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_IDcols')]]}, error=function(x) NULL), Type='character', Default=NULL)
    if(TT == 'Regression') {
      ArgsList[['TransformNumericColumns']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_TransformNumericColumns')]]}, error=function(x) NULL), Type='character', Default=NULL)
      ArgsList[['Methods']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_Methods')]]}, error=function(x) NULL), Type='character', Default=NULL)
    }

    print(' ::  BuildModels 5  :: ')

    # Grid Tuning Parameters
    ArgsList[['PassInGrid']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_PassInGrid')]]}, error=function(x) NULL), Type='character', Default=NULL)
    ArgsList[['GridTune']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_GridTune')]]}, error=function(x) NULL), Type='logical', Default=FALSE)
    ArgsList[['MaxModelsInGrid']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_MaxModelsInGrid')]]}, error=function(x) NULL), Type='numeric', Default=30)
    ArgsList[['MaxRunsWithoutNewWinner']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_MaxRunsWithoutNewWinner')]]}, error=function(x) NULL), Type='numeric', Default=15)
    ArgsList[['MaxRunMinutes']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_MaxRunMinutes')]]}, error=function(x) NULL), Type='numeric', Default=30)
    ArgsList[['BaselineComparison']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_BaselineComparison')]]}, error=function(x) NULL), Type='character', Default='default')

    # Build Model
    if(Algo == 'CatBoost') {

      ArgsList[['PrimaryDateColumn']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_PrimaryDateColumn')]]}, error=function(x) NULL), Type='character', Default=NULL)
      ArgsList[['EncodeMethod']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_EncodeMethod')]]}, error=function(x) NULL), Type='character', Default='credibility')

      print(' ::  BuildModels 6  :: ')

      # CatBoost ML Args
      ArgsList <- RemixAutoML:::Shiny.ML.CatBoost_Params(ArgsList,input,Debug,TT)

      # Build model
      if(TT == 'Regression') {
        ModelOutputList <- do.call(what = RemixAutoML::AutoCatBoostRegression, args = ArgsList)
      } else if(TT == 'Binary Classification') {
        ModelOutputList <- do.call(RemixAutoML::AutoCatBoostClassifier, ArgsList)
      } else if(TT == 'MultiClass') {
        ModelOutputList <- do.call(RemixAutoML::AutoCatBoostMultiClass, ArgsList)
      }

      # Store in DataList
      print(' ::  BuildModels 10 :: ')
      KeyName <- paste0(TT, "_", ArgsList[['ModelID']])
      Output <- RemixAutoML:::Shiny.ML.ModelDataObjects(ModelOutputList, TT = 'catboost')
      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_ScoringData')]] <- Output$ScoringDataCombined
      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_Test_VI_Data')]] <- Output$VI_Train
      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_Train_VI_Data')]] <- Output$VI_Validation
      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_Validation_VI_Data')]] <- Output$VI_Test
      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_All_II_Data')]] <- Output$II_Train
      rm(Output); gc()

    } else if(Algo == 'XGBoost') {

      print(' ::  BuildModels 6  :: ')

      # XGBoost ML Args
      temp <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_TreeMethod']]}, error=function(x) NULL), Type='character', Default = 'hist')
      if(temp == 'GPU') ArgsList[['TreeMethod']] <- 'gpu_hist' else ArgsList[['TreeMethod']] <- 'hist'
      ArgsList[['NThreads']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_NThreads']]}, error=function(x) NULL), Type='numeric', Default=-1)
      ArgsList[['EncodingMethod']] <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['XGBoost_EncodeMethod']]}, error=function(x) NULL), Type='character', Default='credibility')
      ArgsList[['Verbose']] <- 0L
      ArgsList <- RemixAutoML:::Shiny.ML.XGBoost_Params(ArgsList,input,Debug,TT)

      # Build model
      print(' ::  BuildModels 8 :: ')
      if(TT == 'Regression') {
        print(' ::  BuildModels 8.1 :: ')
        ArgsList[['PrimaryDateColumn']] <- RemixAutoML:::ReturnParam(xx=input[['XGBoost_PrimaryDateColumn']], Type='character', Default=NULL)
        ArgsList[['SaveInfoToPDF']] <- FALSE
        ModelOutputList <- do.call(RemixAutoML::AutoXGBoostRegression, ArgsList)
      } else if(TT == 'Binary Classification') {
        print(' ::  BuildModels 8.2 :: ')
        ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)
        ArgsList[['SaveInfoToPDF']] <- FALSE
        ModelOutputList <- do.call(RemixAutoML::AutoXGBoostClassifier, ArgsList)
      } else if(TT == 'MultiClass') {
        print(' ::  BuildModels 8.3 :: ')
        ModelOutputList <- do.call(RemixAutoML::AutoXGBoostMultiClass, ArgsList)
      }

      # Store in DataList
      print(' ::  BuildModels 10 :: ')
      KeyName <- paste0(TT, "_", ArgsList[['ModelID']])
      Output <- RemixAutoML:::Shiny.ML.ModelDataObjects(ModelOutputList, TT = 'xgboost')
      DataList[[paste0('XGBoost_', ArgsList[['ModelID']], '_ScoringData')]] <- Output$ScoringDataCombined
      DataList[[paste0('XGBoost_', ArgsList[['ModelID']], '_Test_VI_Data')]] <- Output$VI_Train
      rm(Output); gc()

    } else if(Algo == 'LightGBM') {

      print(' ::  BuildModels 6  :: ')

      # LightGBM ML Args
      ArgsList <- RemixAutoML:::Shiny.ML.LightGBM_Params(ArgsList,input,Debug,TT)

      # Build model
      print(' ::  BuildModels 8 :: ')
      if(TT == 'Regression') {
        print(' ::  BuildModels 8.1 :: ')
        ModelOutputList <- do.call(RemixAutoML::AutoLightGBMRegression, ArgsList)
      } else if(TT == 'Binary Classification') {
        print(' ::  BuildModels 8.2 :: ')
        ArgsList[['SaveInfoToPDF']] <- FALSE
        cw0 <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_ClassWeights0']]}, error=function(x) NULL), Type='numeric', Default=1)
        cw1 <- RemixAutoML:::ReturnParam(xx=tryCatch({input[['LightGBM_ClassWeights1']]}, error=function(x) NULL), Type='numeric', Default=1)
        ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)
        ModelOutputList <- do.call(RemixAutoML::AutoLightGBMClassifier, ArgsList)
      } else if(TT == 'MultiClass') {
        print(' ::  BuildModels 8.3 :: ')
        ModelOutputList <- do.call(RemixAutoML::AutoLightGBMMultiClass, ArgsList)
      }

      # Store in DataList
      print(' ::  BuildModels 10 :: ')
      KeyName <- paste0(TT, "_", ArgsList[['ModelID']])
      Output <- RemixAutoML:::Shiny.ML.ModelDataObjects(ModelOutputList, TT = 'lightgbm')
      DataList[[paste0('LightGBM_', ArgsList[['ModelID']], '_ScoringData')]] <- Output$ScoringDataCombined
      DataList[[paste0('LightGBM_', ArgsList[['ModelID']], '_Test_VI_Data')]] <- Output$VI_Train
      rm(Output); gc()
    }

    # Update ML_ExperimentTable
    data.table::set(ML_ExperimentTable, i = iter, j = 'ProjectID', value = 'AA')
    data.table::set(ML_ExperimentTable, i = iter, j = 'Date', value = Sys.time())
    data.table::set(ML_ExperimentTable, i = iter, j = 'ModelID', value = ModelOutputList$ArgsList$ModelID)
    data.table::set(ML_ExperimentTable, i = iter, j = 'TargetType', value = if(TT == 'Binary Classification') 'Classification' else TT)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Algorithm', value = Algo)
    data.table::set(ML_ExperimentTable, i = iter, j = 'GridTune', value = ArgsList[['GridTune']])

    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_r-sq',  value = if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TestData[Metric == 'R2', MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_r-sq', value = if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TrainData[Metric == 'R2', MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_RMSE',  value = if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TestData[Metric == 'RMSE', MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_RMSE', value = if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TrainData[Metric == 'RMSE', MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_MAE',   value = if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TestData[Metric == 'MAE', MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_MAE',  value = if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TrainData[Metric == 'MAE', MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_MAPE',  value = if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TestData[Metric == 'MAPE', MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_MAPE', value = if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TrainData[Metric == 'MAPE', MetricValue], 4L) else NA_real_)

    for(i in 1:10) print(names(ModelOutputList))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_Accuracy',         value = if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TestData[order(-Accuracy)][1L, Accuracy], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_Accuracy_Thresh',  value = if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TestData[order(-Accuracy)][1L, Threshold], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_Accuracy',        value = if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TrainData[order(-Accuracy)][1L, Accuracy], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_Accuracy_Thresh', value = if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TrainData[order(-Accuracy)][1L, Threshold], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_MCC',              value = if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TestData[order(-MCC)][1L, MCC], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_Accuracy_Thresh',  value = if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TestData[order(-MCC)][1L, Threshold], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_MCC',             value = if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TrainData[order(-MCC)][1L, MCC], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_Accuracy_Thresh', value = if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TrainData[order(-MCC)][1L, Threshold], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_Utility',          value = if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TestData[order(-Utility)][1L, Utility], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_Utility_Thresh',   value = if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TestData[order(-Utility)][1L, Threshold], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_Utility',         value = if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TrainData[order(-Utility)][1L, Utility], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_Utility_Thresh',  value = if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TrainData[order(-Utility)][1L, Threshold], 4L) else NA_real_)

    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_MultiClass_MCC',       value = if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TestData[Metric == 'MCC'][1L, MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_MultiClass_MCC',      value = if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TrainData[Metric == 'MCC'][1L, MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_MultiClass_Accuracy',  value = if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TestData[Metric == 'Accuracy'][1L, MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_MultiClass_Accuracy', value = if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TrainData[Metric == 'Accuracy'][1L, MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_MicroAUC',             value = if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TestData[Metric == 'MicroAUC'][1L, MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_MicroAUC',            value = if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TrainData[Metric == 'MicroAUC'][1L, MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test_LogLoss',              value = if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TestData[Metric == 'LogLoss'][1L, MetricValue], 4L) else NA_real_)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train_LogLoss',             value = if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TrainData[Metric == 'LogLoss'][1L, MetricValue], 4L) else NA_real_)
  }

  # Return output
  return(list(
    ML_ExperimentTable = ML_ExperimentTable,
    DataList = DataList,
    ArgsList = ArgsList
  ))
}

#' @title Shiny.ML.ModelDataObjects
#'
#' @param ModelOutput Output from RemixAutoML:: supervised learning functions
#'
#' @keywords internal
Shiny.ML.ModelDataObjects <- function(ModelOutput, TT = 'catboost') {
  if(!is.null(ModelOutput$TrainData) && !is.null(ModelOutput$TestData)) {
    temp1 <- data.table::rbindlist(list(
      'TRAIN' = ModelOutput$TrainData,
      'TEST' = ModelOutput$TestData
    ), use.names = TRUE, fill = TRUE, idcol = 'DataSet')
  } else if(is.null(ModelOutput$TrainData) && !is.null(ModelOutput$TestData)) {
    temp1 <- ModelOutput$TestData
  } else if(!is.null(ModelOutput$TrainData) && is.null(ModelOutput$TestData)) {
    temp1 <- ModelOutput$TrainData
  }
  if(tolower(TT) == 'catboost') {
    return(
      list(
        ScoringDataCombined = temp1,
        VI_Train = ModelOutput$VariableImportance$Train_Importance,
        VI_Validation = ModelOutput$VariableImportance$Validation_Importance,
        VI_Test = ModelOutput$VariableImportance$Test_Importance,
        II_Train = ModelOutput$InteractionImportance$Train_Interaction))
  } else if(tolower(TT) %in% c('xgboost','lightgbm')) {
    return(
      list(
        ScoringDataCombined = temp1,
        VI_Train = ModelOutput$VariableImportance,
        VI_Validation = NULL,
        VI_Test = NULL,
        II_Train = NULL))
  }
}
