#' An Automated Machine Learning Framework using H2O
#'
#' Steps in the function include:
#' See details below for information on using this function.
#'
#' 1. Logic: Error checking in the modeling arguments from your Construction file
#'
#' 2. ML: Build grid-tuned models and baseline models for comparison and checks which one performs better on validation data
#'
#' 3. Evaluation: Collects the performance metrics for both
#'
#' 4. Evaluation: Generates calibration plots (and boxplots for regression) for the winning model
#'
#' 5. Evaluation: Generates partial dependence calibration plots (and boxplots for regression) for the winning model
#'
#' 6. Evaluation: Generates variable importance tables and a table of non-important features
#'
#' 7. Production: Creates a storage file containing: model name, model path, grid tune performance, baseline performance, and threshold (if classification) and stores that file in your model_path location
#'
#' The Construct file must be a data.table and the columns need to be in the correct order (see examples). Character columns must be converted to type "Factor". You must remove date columns or convert them to "Factor". For classification models, your target variable needs to be a (0,1) of type "Factor." See the examples below for help with setting up the Construct file for various modeling target variable types. There are examples for regression, classification, multinomial, and quantile regression. For help on which parameters to use, look up the r/h2o documentation. If you misspecify the construct file, it will produce an error and outputfile of what was wrong and suggestions for fixing the error.
#'
#' Let's go over the construct file, column by column. The Targets column is where you specify the column number of your target variable (in quotes, e.g. "c(1)").
#'
#' The Distribution column is where you specify the distribution type for the modeling task. For classification use bernoulli, for multilabel use multinomial, for quantile use quantile, and for regression, you can choose from the list available in the H2O docs, such as gaussian, poisson, gamma, etc. It's not set up to handle tweedie distributions currently but I can add support if there is demand.
#'
#' The Loss column tells H2O which metric to use for the loss metrics. For regression, I typically use "mse", quantile regression, "mae", classification "auc", and multinomial "logloss". For deeplearning models, you need to use "quadratic", "absolute", and "crossentropy".
#'
#' The Quantile column tells H2O which quantile to use for quantile regression (in decimal form).
#'
#' The ModelName column is the name you wish to give your model as a prefix.
#'
#' The Algorithm column is the model you wish to use: gbm, randomForest, deeplearning, AutoML, XGBoost, LightGBM.
#'
#' The dataName column is the name of your data.
#'
#' The TargetCol column is the column number of your target variable.
#'
#' The FeatureCols column is the column numbers of your features.
#'
#' The CreateDate column is for tracking your model build dates.
#'
#' The GridTune column is a TRUE / FALSE column for whether you want to run a grid tune model for comparison.
#'
#' The ExportValidData column is a TRUE / FALSE column indicating if you want to export the validation data.
#'
#' The ParDep column is where you put the number of partial dependence calibration plots you wish to generate.
#'
#' The PD_Data column is where you specify if you want to generate the partial dependence plots on "All" data, "Validate" data, or "Train" data.
#'
#' The ThreshType column is for classification models. You can specify "f1", "f2", "f0point5", or "CS" for cost sentitive.
#'
#' The FSC column is the feature selection column. Specify the percentage importance cutoff to create a table of "unimportant" features.
#'
#' The tpProfit column is for when you specify "CS" in the ThreshType column. This is your true positive profit.
#'
#' The tnProfit column is for when you specify "CS" in the ThreshType column. This is your true negative profit.
#'
#' The fpProfit column is for when you specify "CS" in the ThreshType column. This is your false positive profit.
#'
#' The fnProfit column is for when you specify "CS" in the ThreshType column. This is your false negative profit.
#'
#' The SaveModel column is a TRUE / FALSE indicator. If you are just testing out models, set this to FALSE.
#'
#' The SaveModelType column is where you specify if you want a "standard" model object saveed or a "mojo" model object saved.
#'
#' The PredsAllData column is a TRUE / FALSE column. Set to TRUE if you want all the predicted values returns (for all data).
#'
#' The TargetEncoding column let's you specify the column number of features you wish to run target encoding on. Set to NA to not run this feature.
#'
#' The SupplyData column lets you supply the data names for training and validation data. Set to NULL if you want the data partitioning to be done internally.
#' @author Adrian Antico
#' @family Automated Model Scoring
#' @param Construct Core instruction file for automation (see Details below for more information on this)
#' @param max_memory The ceiling amount of memory H2O will utilize
#' @param ratios The percentage of train samples from source data (remainder goes to validation set)
#' @param BL_Trees The number of trees to build in baseline GBM or RandomForest
#' @param nthreads Set the number of threads to run function
#' @param model_path Directory path for where you want your models saved
#' @param MaxRuntimeSeconds Number of seconds of run time for grid tuning
#' @param MaxModels Number of models you'd like to have returned
#' @param TrainData Set to NULL or supply a data.table for training data
#' @param TestData Set to NULL or supply  a data.table for validation data
#' @param SaveToFile Set to TRUE to save models and output to model_path
#' @param ReturnObjects Set to TRUE to return objects from functioin
#' @return Returns saved models, corrected Construct file, variable importance tables, evaluation and partial dependence calibration plots, model performance measure, and a file called grid_tuned_paths.Rdata which contains paths to your saved models for operationalization.
#' @examples
#' \donttest{
#' # Classification Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' aa[, target := as.factor(ifelse(target > 0.5,1,0))]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("bernoulli",
#'                                                         "bernoulli",
#'                                                         "bernoulli"),
#'                                     Loss            = c("AUC","AUC","CrossEntropy"),
#'                                     Quantile        = rep(NA,3),
#'                                     ModelName       = c("GBM","DRF","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "randomForest",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",3),
#'                                     TargetCol       = rep(c("1"),3),
#'                                     FeatureCols     = rep(c("2:11"),3),
#'                                     CreateDate      = rep(Sys.time(),3),
#'                                     GridTune        = rep(FALSE,3),
#'                                     ExportValidData = rep(TRUE,3),
#'                                     ParDep          = rep(2,3),
#'                                     PD_Data         = rep("All",3),
#'                                     ThreshType      = rep("f1",3),
#'                                     FSC             = rep(0.001,3),
#'                                     tpProfit        = rep(NA,3),
#'                                     tnProfit        = rep(NA,3),
#'                                     fpProfit        = rep(NA,3),
#'                                     fnProfit        = rep(NA,3),
#'                                     SaveModel       = rep(FALSE,3),
#'                                     SaveModelType   = c("Mojo","standard","mojo"),
#'                                     PredsAllData    = rep(TRUE,3),
#'                                     TargetEncoding  = rep(NA,3),
#'                                     SupplyData      = rep(FALSE,3))
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'
#' # Multinomial Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' aa[, target := as.factor(ifelse(target < 0.33,"A",ifelse(target < 0.66, "B","C")))]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("multinomial",
#'                                                         "multinomial",
#'                                                         "multinomial"),
#'                                     Loss            = c("auc","logloss","accuracy"),
#'                                     Quantile        = rep(NA,3),
#'                                     ModelName       = c("GBM","DRF","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "randomForest",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",3),
#'                                     TargetCol       = rep(c("1"),3),
#'                                     FeatureCols     = rep(c("2:11"),3),
#'                                     CreateDate      = rep(Sys.time(),3),
#'                                     GridTune        = rep(FALSE,3),
#'                                     ExportValidData = rep(TRUE,3),
#'                                     ParDep          = rep(NA,3),
#'                                     PD_Data         = rep("All",3),
#'                                     ThreshType      = rep("f1",3),
#'                                     FSC             = rep(0.001,3),
#'                                     tpProfit        = rep(NA,3),
#'                                     tnProfit        = rep(NA,3),
#'                                     fpProfit        = rep(NA,3),
#'                                     fnProfit        = rep(NA,3),
#'                                     SaveModel       = rep(FALSE,3),
#'                                     SaveModelType   = c("Mojo","standard","mojo"),
#'                                     PredsAllData    = rep(TRUE,3),
#'                                     TargetEncoding  = rep(NA,3),
#'                                     SupplyData      = rep(FALSE,3))
#'
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'
#' # Regression Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("gaussian",
#'                                                         "gaussian",
#'                                                         "gaussian"),
#'                                     Loss            = c("MSE","MSE","Quadratic"),
#'                                     Quantile        = rep(NA,3),
#'                                     ModelName       = c("GBM","DRF","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "randomForest",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",3),
#'                                     TargetCol       = rep(c("1"),3),
#'                                     FeatureCols     = rep(c("2:11"),3),
#'                                     CreateDate      = rep(Sys.time(),3),
#'                                     GridTune        = rep(FALSE,3),
#'                                     ExportValidData = rep(TRUE,3),
#'                                     ParDep          = rep(2,3),
#'                                     PD_Data         = rep("All",3),
#'                                     ThreshType      = rep("f1",3),
#'                                     FSC             = rep(0.001,3),
#'                                     tpProfit        = rep(NA,3),
#'                                     tnProfit        = rep(NA,3),
#'                                     fpProfit        = rep(NA,3),
#'                                     fnProfit        = rep(NA,3),
#'                                     SaveModel       = rep(FALSE,3),
#'                                     SaveModelType   = c("Mojo","standard","mojo"),
#'                                     PredsAllData    = rep(TRUE,3),
#'                                     TargetEncoding  = rep(NA,3),
#'                                     SupplyData      = rep(FALSE,3))
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'
#' # Quantile Regression Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("quantile",
#'                                                         "quantile"),
#'                                     Loss            = c("MAE","Absolute"),
#'                                     Quantile        = rep(0.75,2),
#'                                     ModelName       = c("GBM","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",2),
#'                                     TargetCol       = rep(c("1"),2),
#'                                     FeatureCols     = rep(c("2:11"),2),
#'                                     CreateDate      = rep(Sys.time(),2),
#'                                     GridTune        = rep(FALSE,2),
#'                                     ExportValidData = rep(TRUE,2),
#'                                     ParDep          = rep(4,2),
#'                                     PD_Data         = rep("All",2),
#'                                     ThreshType      = rep("f1",2),
#'                                     FSC             = rep(0.001,2),
#'                                     tpProfit        = rep(NA,2),
#'                                     tnProfit        = rep(NA,2),
#'                                     fpProfit        = rep(NA,2),
#'                                     fnProfit        = rep(NA,2),
#'                                     SaveModel       = rep(FALSE,2),
#'                                     SaveModelType   = c("Mojo","mojo"),
#'                                     PredsAllData    = rep(TRUE,2),
#'                                     TargetEncoding  = rep(NA,2),
#'                                     SupplyData      = rep(FALSE,2))
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'}
#' @export
AutoH2OModeler <- function(Construct,
                           max_memory        = "28G",
                           ratios            = 0.80,
                           BL_Trees          = 500,
                           nthreads          = 1,
                           model_path        = NULL,
                           MaxRuntimeSeconds = 3600,
                           MaxModels         = 30,
                           TrainData         = NULL,
                           TestData          = NULL,
                           SaveToFile        = FALSE,
                           ReturnObjects     = TRUE) {
  ######################################
  # Error handling and prevention
  ######################################
  
  # Handle the multinomial case
  for (i in as.integer(seq_len(nrow(Construct)))) {
    if (tolower(Construct[i, 2][[1]]) == "multinomial" &&
        tolower(Construct[i, 3][[1]]) == "accuracy" &&
        tolower(Construct[i, 6][[1]]) != "deeplearning") {
      multinomialMetric <- "accuracy"
      data.table::set(Construct,
                      i = i,
                      j = 3L,
                      value = "logloss")
    } else if (tolower(Construct[i, 2][[1]]) == "multinomial" &&
               tolower(Construct[i, 3][[1]]) == "auc" &&
               tolower(Construct[i, 6][[1]]) != "deeplearning") {
      multinomialMetric <- "auc"
      data.table::set(Construct,
                      i = i,
                      j = 3L,
                      value = "logloss")
    } else if (tolower(Construct[i, 2][[1]]) == "multinomial" &&
               tolower(Construct[i, 3][[1]]) == "accuracy" &&
               tolower(Construct[i, 6][[1]]) == "deeplearning") {
      multinomialMetric <- "accuracy"
      data.table::set(Construct,
                      i = i,
                      j = 3L,
                      value = "crossentropy")
    } else if (tolower(Construct[i, 2][[1]]) == "multinomial" &&
               tolower(Construct[i, 3][[1]]) == "auc" &&
               tolower(Construct[i, 6][[1]]) == "deeplearning") {
      multinomialMetric <- "auc"
      data.table::set(Construct,
                      i = i,
                      j = 3L,
                      value = "crossentropy")
    }
  }
  
  ErrorCollection <-
    data.table::data.table(Row = rep(-720, 10000),
                           Msg = "I like modeling")
  j <- 0
  for (i in as.integer(seq_len(nrow(Construct)))) {
    # Algorithm specific
    if (tolower(Construct[i, 6][[1]]) %in% c("gbm",
                                             "randomforest",
                                             "automl",
                                             "xgboost",
                                             "lightgbm")) {
      # GBM and RF loss functions existence
      if (!(
        tolower(Construct[i, 3][[1]]) %in% c(
          "auto",
          "deviance",
          "mse",
          "rmse",
          "mae",
          "rmsle",
          "accuracy",
          "auc",
          "lift_top_group",
          "misclassification",
          "mean_per_class_error",
          "logloss"
        )
      )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = i,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: AUTO | deviance |
              logloss | MSE | RMSE |
              MAE | RMSLE | AUC | lift_top_group |
              misclassification | mean_per_class_error"
            )
          )
        )
      } else {
        temp <- tolower(Construct[i, 3][[1]])
        lower <-
          c(
            "auto",
            "deviance",
            "logloss",
            "mse",
            "rmse",
            "accuracy",
            "mae",
            "rmsle",
            "auc",
            "lift_top_group",
            "misclassification",
            "mean_per_class_error"
          )
        proper <-
          c(
            "AUTO",
            "deviance",
            "logloss",
            "MSE",
            "RMSE",
            "ACCURACY",
            "MAE",
            "RMSLE",
            "AUC",
            "lift_top_group",
            "misclassification",
            "mean_per_class_error"
          )
        distMatch <-
          data.table::data.table(act = rep(temp, 12),
                                 LCVals = lower,
                                 Proper = proper)
        ReplaceValue <- distMatch[act == LCVals][["Proper"]][[1]]
        data.table::set(Construct, i, 3L, value = ReplaceValue)
      }
      
      # GBM and RF distributions
      if (!(
        tolower(Construct[i, 2][[1]]) %in% c(
          "auto",
          "bernoulli",
          "quasibinomial",
          "multinomial",
          "gaussian",
          "poisson",
          "gamma",
          "tweedie",
          "laplace",
          "quantile",
          "huber"
        )
      )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Distribution ",
              Construct[i, 2][[1]],
              " is not in list: AUTO | bernoulli | quasibinomial |
              multinomial | gaussian | poisson | gamma | tweedie |
              laplace | quantile | huber"
            )
          )
        )
      } else {
        temp <- tolower(Construct[i, 2][[1]])
        lower <-
          c(
            "auto",
            "bernoulli",
            "quasibinomial",
            "multinomial",
            "gaussian",
            "poisson",
            "gamma",
            "tweedie",
            "laplace",
            "quantile",
            "huber"
          )
        proper <-
          c(
            "AUTO",
            "bernoulli",
            "quasibinomial",
            "multinomial",
            "gaussian",
            "poisson",
            "gamma",
            "tweedie",
            "laplace",
            "quantile",
            "huber"
          )
        distMatch <-
          data.table::data.table(act = rep(temp, 11),
                                 LCVals = lower,
                                 Proper = proper)
        ReplaceValue2 <- distMatch[act == LCVals][["Proper"]][[1]]
        data.table::set(Construct, i, 2L, value = ReplaceValue2)
      }
      
      # Distribution and loss combos for non-regression
      if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial", "binomial",
                                               "bernoulli", "multinomial") &&
          !(
            tolower(Construct[i, 3][[1]]) %in% c(
              "auc",
              "logloss",
              "accuracy",
              "auto",
              "lift_top_group",
              "misclassification",
              "mean_per_class_error"
            )
          )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: AUC | logloss | AUTO | lift_top_group |
              misclassification | mean_per_class_error | accuracy"
            )
          )
        )
      }
      
      # Distribution and loss combos for regression
      if (tolower(Construct[i, 2][[1]]) %in% c("gaussian",
                                               "poisson",
                                               "gamma",
                                               "tweedie",
                                               "laplace",
                                               "quantile",
                                               "huber") &&
          !(tolower(Construct[i, 3][[1]]) %in% c("auto", "mse", "rmse",
                                                 "mae", "rmsle"))) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 2][[1]],
              " is not in list: AUTO | MSE | RMSE | MAE | RMSLE"
            )
          )
        )
      }
      
      # Quantile Regression with GBM
      if (tolower(Construct[i, 2][[1]]) %in% c("quantile") &&
          (Construct[i, 4][[1]] > 1 ||
           Construct[i, 4][[1]] < 0 ||
           !is.numeric(Construct[i, 4][[1]]))) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantiles using ",
              Construct[i, 6][[1]],
              " must be a number less than or equal to 1 AND greater
              than or equal to 0"
            )
          )
        )
      }
      
      # RF Quantile regression fail
      if (tolower(Construct[i, 6][[1]]) == "randomforest" &&
          tolower(Construct[i, 2][[1]]) == "quantile") {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantile regression is only supported by GBM and
              Deeplearning models, not ",
              Construct[i, 6][[1]],
              " models"
            )
          )
        )
      }
      
      # Quantile regression loss metrics
      if (tolower(Construct[i, 2][[1]]) == "quantile" &&
          tolower(Construct[i, 3][[1]]) != "mae") {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantile regression is best supported by MAE when using ",
              Construct[i, 6][[1]],
              " models"
            )
          )
        )
      }
      
      if (tolower(Construct[i, 6][[1]]) == "automl" &
          Construct[i, 11][[1]] != TRUE) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c("using automl requires GridTune = TRUE")
        )
      }
    } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
      # Deeplearning loss functions
      if (!(
        tolower(Construct[i, 3][[1]]) %in% c(
          "automatic",
          "crossentropy",
          "quadratic",
          "accuracy",
          "auc",
          "huber",
          "absolute",
          "quantile"
        )
      )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: Automatic | CrossEntropy | Quadratic |
              Huber | Absolute | Quantile | AUC | ACCURACY"
            )
          )
        )
      } else {
        temp <- tolower(Construct[i, 3][[1]])
        lower <-
          c(
            "automatic",
            "crossentropy",
            "quadratic",
            "auc",
            "accuracy",
            "huber",
            "absolute",
            "quantile"
          )
        proper <-
          c(
            "Automatic",
            "CrossEntropy",
            "Quadratic",
            "AUC",
            "ACCURACY",
            "Huber",
            "Absolute",
            "Quantile"
          )
        distMatch <-
          data.table::data.table(act = rep(temp, 8),
                                 LCVals = lower,
                                 Proper = proper)
        ReplaceVal <- distMatch[act == LCVals][["Proper"]][[1]]
        data.table::set(Construct, i, 3L, value = ReplaceVal)
      }
      
      # Deeplearning distributions
      if (!(
        tolower(Construct[i, 2][[1]]) %in% c(
          "auto",
          "bernoulli",
          "multinomial",
          "gaussian",
          "poisson",
          "gamma",
          "tweedie",
          "laplace",
          "quantile",
          "huber"
        )
      )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Distributions ",
              Construct[i, 2][[1]],
              " is not in list: AUTO | bernoulli | multinomial | gaussian |
              poisson | gamma | tweedie | laplace | quantile | huber"
            )
          )
        )
      } else {
        temp <- tolower(Construct[i, 2][[1]])
        lower <-
          c(
            "auto",
            "bernoulli",
            "multinomial",
            "gaussian",
            "poisson",
            "gamma",
            "tweedie",
            "laplace",
            "quantile",
            "huber"
          )
        proper <-
          c(
            "AUTO",
            "bernoulli",
            "multinomial",
            "gaussian",
            "poisson",
            "gamma",
            "tweedie",
            "laplace",
            "quantile",
            "huber"
          )
        distMatch <-
          data.table::data.table(act = rep(temp, 10),
                                 LCVals = lower,
                                 Proper = proper)
        ReplaceVal2 <- distMatch[act == LCVals][["Proper"]][[1]]
        data.table::set(Construct, i, 2L, value = ReplaceVal2)
      }
      
      # Distribution and loss combos for non-regression
      if (tolower(Construct[i, 2][[1]]) %in% c("bernoulli",
                                               "multinomial") &&
          !(tolower(Construct[i, 3][[1]]) %in% c("automatic",
                                                 "crossentropy",
                                                 "auc",
                                                 "accuracy"))) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: Automatic | CrossEntropy | AUC | ACCURACY"
            )
          )
        )
      }
      
      # Distribution and loss combos for regression
      if (tolower(Construct[i, 2][[1]]) %in% c("gaussian",
                                               "poisson",
                                               "gamma",
                                               "tweedie",
                                               "laplace",
                                               "quantile",
                                               "huber") &&
          !(
            tolower(Construct[i, 3][[1]]) %in% c(
              "automatic",
              "quadratic",
              "huber",
              "absolute",
              "quantile"
            )
          )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: Automatic | Quadratic
              | Huber | Absolute | Quantile"
            )
          )
        )
      }
      
      # Quantile regression loss metrics
      if (tolower(Construct[i, 2][[1]]) == "quantile" &&
          tolower(Construct[i, 3][[1]]) != "quantile") {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantile regression needs to use
              Quantile for the loss function with ",
              Construct[i, 6][[1]],
              " models"
            )
          )
        )
      }
      
      # Quantile Regression with DL
      if (tolower(Construct[i, 2][[1]]) %in% c("quantile") &&
          (Construct[i, 4][[1]] > 1 ||
           Construct[i, 4][[1]] < 0 ||
           !is.numeric(Construct[i, 4][[1]]))) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantiles using ",
              Construct[i, 6][[1]],
              " must be a number less than or equal to
              1 AND greater than or equal to 0"
            )
          )
        )
      }
      
    } else {
      j <- j + 1
      data.table::set(ErrorCollection,
                      i = j,
                      j = 1L,
                      value = i)
      data.table::set(
        ErrorCollection,
        i = j,
        j = 2L,
        value = c(
          paste0(
            "Models supported are: GBM, randomForest,
            and deeplearning, while ",
            Construct[i, 6][[1]],
            " is not"
          )
        )
      )
    }
  }
  
  # Error stopping point and Construct file save
  ErrorCollection <- ErrorCollection[Row != -720]
  if (nrow(ErrorCollection) >= 1) {
    ErrorCollectionLog <- ErrorCollection
    if (SaveToFile == TRUE & ReturnObjects == TRUE) {
      message(
        "Your model construction file has errors and
        an error log has been returned and saved to model_path"
      )
      save(ErrorCollectionLog,
           file = paste0(model_path, "/ErrorCollectionLog.Rdata"))
      return(ErrorCollectionLog)
    } else if (SaveToFile == TRUE & ReturnObjects == FALSE) {
      save(ErrorCollectionLog,
           file = paste0(model_path, "/ErrorCollectionLog.Rdata"))
      warning(
        "Your model construction file has errors and
            an error log has been saved to your model_path"
      )
    } else if (SaveToFile == FALSE & ReturnObjects == TRUE) {
      message("Your model construction file has errors and
        an error log has
        been returned")
      return(ErrorCollectionLog)
    } else {
      warning(
        "Your model construction file has errors and
        an error log errors. Set ReturnObjects to TRUE to see ErrorLog"
      )
    }
  }
  
  # Clear table
  rm(distMatch)
  
  # Set up grid_tuned_paths.R file
  grid_tuned_paths <-
    data.table::data.table(
      Model     = rep("a", nrow(Construct)),
      Path      = rep("a", nrow(Construct)),
      GT_Metric = rep(1234.5678, nrow(Construct)),
      BL_Metric = rep(1234.5678, nrow(Construct)),
      BinThresh = rep(1234.5678, nrow(Construct)),
      PathJar   = rep("a", nrow(Construct))
    )
  
  ######################################
  # Loop through model building
  ######################################
  
  tryCatch({
    for (i in as.integer(seq_len(nrow(Construct)))) {
      # No deeplearning loss functions as stopping metrics
      if (tolower(Construct[i, 3][[1]]) == "crossentropy") {
        if (tolower(Construct[i, 2][[1]]) == "multinomial") {
          StoppingMetric <- "logloss"
        } else {
          StoppingMetric <- "auc"
        }
      } else {
        if (tolower(Construct[i, 3][[1]]) %in% c("quadratic",
                                                 "huber")) {
          StoppingMetric <- "mse"
        } else if (tolower(Construct[i, 3][[1]]) %in% c("absolute",
                                                        "quantile")) {
          StoppingMetric <- "mae"
        } else {
          StoppingMetric <- Construct[i, 3][[1]]
        }
      }
      
      # Define grid tune search scheme in a named list
      search_criteria  <-
        list(
          strategy             = "RandomDiscrete",
          max_runtime_secs     = MaxRuntimeSeconds,
          max_models           = MaxModels,
          seed                 = 1234,
          stopping_rounds      = 10,
          stopping_metric      = StoppingMetric,
          stopping_tolerance   = 1e-3
        )
      
      # Set up H2O environment instance
      Sys.sleep(10)
      h2o::h2o.init(
        nthreads = nthreads,
        max_mem_size = max_memory,
        enable_assertions = FALSE
      )
      
      # Define data sets
      if (Construct[i, "SupplyData"][[1]]) {
        train        <- h2o::as.h2o(TrainData)
        validate     <- h2o::as.h2o(TestData)
        data_h2o     <-
          h2o::as.h2o(data.table::rbindlist(list(TrainData,
                                                 TestData)))
      } else {
        data_h2o     <-
          eval(parse(text = paste0("h2o::as.h2o(",
                                   Construct[i, 7][[1]], ")")))
        data_train   <- h2o::h2o.splitFrame(data_h2o,
                                            ratios = ratios)
        train        <- data_train[[1]]
        validate     <- data_train[[2]]
      }
      
      # Define targets
      target         <-
        eval(parse(text = paste0(Construct[i, 8][[1]])))
      features       <-
        eval(parse(text = paste0(Construct[i, 9][[1]])))
      XGB            <- h2o::h2o.xgboost.available()
      if (XGB) {
        if (tolower(Construct[i, 2][[1]]) != "quantile") {
          ModelExclude   <- NULL
        } else {
          ModelExclude   <- c("XGBoost", "GLM", "DRF")
        }
      } else {
        if (tolower(Construct[i, 2][[1]]) != "quantile") {
          ModelExclude   <- c("XGBoost")
        } else {
          ModelExclude   <- c("XGBoost", "GLM", "DRF")
        }
      }
      
      if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
        N              <- length(features)
        P5             <- 2 ^ (-1 / 5)
        P4             <- 2 ^ (-1 / 4)
        P3             <- 2 ^ (-1 / 3)
      }
      data.table::set(grid_tuned_paths,
                      i = i,
                      j = 1L,
                      value = Construct[i, 5][[1]])
      
      ######################################
      # Target Encoding
      ######################################
      
      if (!is.na(Construct[i, "TargetEncoding"][[1]])) {
        TEncode <- eval(parse(text = Construct[i, "TargetEncoding"][[1]]))
        cols <- names(train)[TEncode]
        train[, Construct[i, "Targets"][[1]]] <-
          as.numeric(train[, Construct[i, "Targets"][[1]]])
        validate[, Construct[i, "Targets"][[1]]] <-
          as.numeric(validate[, Construct[i, "Targets"][[1]]])
        for (col in cols) {
          x     <- h2o::h2o.target_encode_create(data = train,
                                                 x = list(col),
                                                 y = Construct[i, "Targets"][[1]])
          # Apply to training data
          train <- h2o::h2o.target_encode_apply(
            train,
            x = list(col),
            y = Construct[i, "Targets"][[1]],
            target_encode_map = x,
            holdout_type = "None",
            blended_avg = TRUE,
            noise_level = 0
          )
          
          # Apply to validation data
          validate <- h2o::h2o.target_encode_apply(
            validate,
            x = list(col),
            y = Construct[i, "Targets"][[1]],
            target_encode_map = x,
            holdout_type = "None",
            blended_avg = TRUE,
            noise_level = 0
          )
          
          if (SaveToFile == TRUE) {
            save(x,
                 file = paste0(model_path,
                               "/" ,
                               Construct[i, "Targets"][[1]],
                               "_", col,
                               ".Rdata"))
          }
        }
        
        # Modify feature reference
        features <-
          c((min(features) + length(eval(
            parse(text = paste0(Construct[i, 24][[1]]))
          ))):max(features),
          (max(target) + 1):(max(target) +
                               length(eval(
                                 parse(text = paste0(Construct[i, 24][[1]]))
                               ))))
        
        # Turn target columns back to factor
        train[, Construct[i, "Targets"][[1]]] <-
          as.factor(train[, Construct[i, "Targets"][[1]]])
        validate[, Construct[i, "Targets"][[1]]] <-
          as.factor(validate[, Construct[i, "Targets"][[1]]])
        data.table::set(Construct,
                        i = i,
                        j = "PD_Data",
                        value = "Validate")
      }
      
      ######################################
      # Hyperparameters
      ######################################
      
      if (Construct[i, 11][[1]]) {
        if (tolower(Construct[i, 6][[1]]) == "gbm") {
          if (tolower(
            Construct[i, 3][[1]] %in% c(
              "auc",
              "logloss",
              "auto",
              "lift_top_group",
              "misclassification",
              "mean_per_class_error"
            )
          )) {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              balance_classes                  = c(TRUE, FALSE),
              ntrees                           = c(500, 750, 1000),
              sample_rate                      = seq(0.5, 1, 0.01),
              col_sample_rate                  = seq(0.2, 1, 0.01),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
              col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              nbins                            = 2 ^ seq(4, 10, 1),
              nbins_cats                       = 2 ^ seq(4, 12, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4),
              histogram_type                   = c(
                "UniformAdaptive",
                "QuantilesGlobal",
                "RoundRobin"
              )
            )
          } else {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              ntrees                           = c(500, 750, 1000),
              sample_rate                      = seq(0.5, 1, 0.01),
              col_sample_rate                  = seq(0.2, 1, 0.01),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
              col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              nbins                            = 2 ^ seq(4, 10, 1),
              nbins_cats                       = 2 ^ seq(4, 12, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4),
              histogram_type                   = c(
                "UniformAdaptive",
                "QuantilesGlobal",
                "RoundRobin"
              )
            )
          }
          
        } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
          if (tolower(Construct[i, 3][[1]] %in% c("automatic",
                                                  "crossentropy"))) {
            hyper_params <-
              list(
                activation = c(
                  "Rectifier",
                  "Maxout",
                  "Tanh",
                  "RectifierWithDropout",
                  "MaxoutWithDropout",
                  "TanhWithDropout"
                ),
                hidden              = list(
                  c(
                    floor(N * P5),
                    floor(N * P5 * P5),
                    floor(N * P5 * P5 * P5),
                    floor(N * P5 * P5 * P5 * P5),
                    floor(N * P5 * P5 * P5 * P5 * P5)
                  ),
                  c(
                    floor(N * P4),
                    floor(N * P4 * P4),
                    floor(N * P4 * P4 * P4),
                    floor(N * P4 * P4 * P4 * P4)
                  ),
                  c(floor(N * P3), floor(N *
                                           P3 * P3),
                    floor(N * P3 * P3 * P3))
                ),
                balance_classes     = c(TRUE, FALSE),
                epochs              = c(50, 100, 200),
                l1                  = c(0, 0.00001, 0.0001),
                l2                  = c(0, 0.00001, 0.0001),
                rate                = c(0, 0.01, 0.005, 0.001),
                rate_annealing      = c(1e-8, 1e-7, 1e-6),
                rho                 = c(0.9, 0.95, 0.99, 0.999),
                epsilon             = c(1e-10, 1e-8, 1e-6, 1e-4),
                momentum_start      = c(0, 0.5),
                momentum_stable     = c(0.99, 0.5, 0),
                input_dropout_ratio = c(0, 0.1, 0.2),
                max_w2              = c(10, 100, 1000, 3.4028235e+38)
              )
          } else {
            hyper_params <-
              list(
                activation = c(
                  "Rectifier",
                  "Maxout",
                  "Tanh",
                  "RectifierWithDropout",
                  "MaxoutWithDropout",
                  "TanhWithDropout"
                ),
                hidden              = list(
                  c(
                    floor(N * P5),
                    floor(N * P5 * P5),
                    floor(N * P5 * P5 * P5),
                    floor(N * P5 * P5 * P5 * P5),
                    floor(N * P5 * P5 * P5 * P5 * P5)
                  ),
                  c(
                    floor(N * P4),
                    floor(N * P4 * P4),
                    floor(N * P4 * P4 * P4),
                    floor(N * P4 * P4 * P4 * P4)
                  ),
                  c(floor(N * P3), floor(N *
                                           P3 * P3),
                    floor(N * P3 * P3 * P3))
                ),
                epochs              = c(50, 100, 200),
                l1                  = c(0, 0.00001, 0.0001),
                l2                  = c(0, 0.00001, 0.0001),
                rate                = c(0, 0.01, 0.005, 0.001),
                rate_annealing      = c(1e-8, 1e-7, 1e-6),
                rho                 = c(0.9, 0.95, 0.99, 0.999),
                epsilon             = c(1e-10, 1e-8, 1e-6, 1e-4),
                momentum_start      = c(0, 0.5),
                momentum_stable     = c(0.99, 0.5, 0),
                input_dropout_ratio = c(0, 0.1, 0.2),
                max_w2              = c(10, 100, 1000, 3.4028235e+38)
              )
          }
        } else if (tolower(Construct[i, 6][[1]]) == "randomforest") {
          if (tolower(
            Construct[i, 3][[1]] %in% c(
              "auc",
              "logloss",
              "auto",
              "lift_top_group",
              "misclassification",
              "mean_per_class_error"
            )
          )) {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              balance_classes                  = c(TRUE, FALSE),
              ntrees                           = c(500, 750, 1000),
              mtries                           = -1,
              sample_rate                      = seq(0.2, 1, 0.05),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.05),
              col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              nbins                            = 2 ^ seq(4, 10, 1),
              nbins_cats                       = 2 ^ seq(4, 12, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4),
              histogram_type                   = c(
                "UniformAdaptive",
                "QuantilesGlobal",
                "RoundRobin"
              )
            )
          } else {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              ntrees                           = c(500, 750, 1000),
              mtries                           = -1,
              sample_rate                      = seq(0.2, 1, 0.05),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.05),
              col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              nbins                            = 2 ^ seq(4, 10, 1),
              nbins_cats                       = 2 ^ seq(4, 12, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4),
              histogram_type                   = c(
                "UniformAdaptive",
                "QuantilesGlobal",
                "RoundRobin"
              )
            )
          }
        } else if (tolower(Construct[i, 6][[1]]) == "automl") {
          message("automl is preset with tuning parameters")
        } else if (tolower(Construct[i, 6][[1]]) == "xgboost") {
          if (tolower(
            Construct[i, 3][[1]] %in% c(
              "auc",
              "logloss",
              "auto",
              "lift_top_group",
              "misclassification",
              "mean_per_class_error"
            )
          )) {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              tree_method                      = c("hist", "AUTO"),
              grow_policy                      = c("lossguide", "depthwise"),
              balance_classes                  = c(TRUE, FALSE),
              ntrees                           = c(500, 750, 1000),
              sample_rate                      = seq(0.5, 1, 0.01),
              col_sample_rate                  = seq(0.2, 1, 0.01),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
              reg_lambda                       = c(0.001, 0.01, 0.05),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4)
            )
          } else {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              ntrees                           = c(500, 750, 1000),
              sample_rate                      = seq(0.5, 1, 0.01),
              col_sample_rate                  = seq(0.2, 1, 0.01),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
              reg_lambda                       = c(0.001, 0.01, 0.05),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4)
            )
          }
        } else if (tolower(Construct[i, 6][[1]]) == "lightgbm") {
          if (tolower(
            Construct[i, 3][[1]] %in% c(
              "auc",
              "logloss",
              "auto",
              "lift_top_group",
              "misclassification",
              "mean_per_class_error"
            )
          )) {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              tree_method                      = c("hist"),
              grow_policy                      = c("lossguide"),
              balance_classes                  = c(TRUE, FALSE),
              ntrees                           = c(500, 750, 1000),
              sample_rate                      = seq(0.5, 1, 0.01),
              col_sample_rate                  = seq(0.2, 1, 0.01),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
              reg_lambda                       = c(0.001, 0.01, 0.05),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4)
            )
          } else {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              ntrees                           = c(500, 750, 1000),
              sample_rate                      = seq(0.5, 1, 0.01),
              col_sample_rate                  = seq(0.2, 1, 0.01),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
              reg_lambda                       = c(0.001, 0.01, 0.05),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4)
            )
          }
        }
      }
      
      ######################################
      # Grid Tune Models
      ######################################
      
      # Check to see if GridTune is TRUE
      # Check to see if Distribution is quantile
      # Select model
      
      # Grid tuned model build
      if (Construct[i, 11][[1]]) {
        if (tolower(Construct[i, 2][[1]]) == "quantile") {
          if (tolower(Construct[i, 6][[1]]) == "gbm") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              distribution         = Construct[i, 2][[1]],
              quantile_alpha       = Construct[i, 4][[1]],
              learn_rate           = 0.05,
              learn_rate_annealing = 0.99,
              max_runtime_secs     = MaxRuntimeSeconds,
              stopping_rounds      = 5,
              stopping_tolerance   = 1e-4,
              stopping_metric      = StoppingMetric,
              score_tree_interval  = 10,
              seed                 = 1234
            )
          } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              distribution         = Construct[i, 2][[1]],
              quantile_alpha       = Construct[i, 4][[1]],
              seed                 = 42
            )
          }
        } else {
          if (tolower(Construct[i, 6][[1]]) == "gbm") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              distribution         = Construct[i, 2][[1]],
              learn_rate           = 0.05,
              learn_rate_annealing = 0.99,
              max_runtime_secs     = MaxRuntimeSeconds,
              stopping_rounds      = 5,
              stopping_tolerance   = 1e-4,
              stopping_metric      = StoppingMetric,
              score_tree_interval  = 10,
              seed                 = 1234
            )
          } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              distribution         = Construct[i, 2][[1]],
              seed                 = 42
            )
          } else if (tolower(Construct[i, 6][[1]]) == "randomforest") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              max_runtime_secs     = MaxRuntimeSeconds,
              stopping_rounds      = 5,
              stopping_tolerance   = 1e-4,
              stopping_metric      = StoppingMetric,
              score_tree_interval  = 10,
              seed                 = 1234
            )
          } else if (tolower(Construct[i, 6][[1]]) == "automl") {
            aml <- h2o::h2o.automl(
              x                  = features,
              y                  = target,
              training_frame     = train,
              validation_frame   = validate,
              max_models         = MaxModels,
              max_runtime_secs   = MaxRuntimeSeconds,
              stopping_metric    = StoppingMetric,
              stopping_tolerance = 1e-4,
              stopping_rounds    = 10,
              project_name       = "TestAML",
              exclude_algos      = ModelExclude,
              sort_metric        = StoppingMetric
            )
          } else if (tolower(Construct[i, 6][[1]]) == "xgboost") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              categorical_encoding = "Enum",
              distribution         = Construct[i, 2][[1]],
              learn_rate           = 0.05,
              max_runtime_secs     = MaxRuntimeSeconds,
              stopping_rounds      = 5,
              stopping_tolerance   = 1e-4,
              stopping_metric      = StoppingMetric,
              score_tree_interval  = 10,
              seed                 = 1234
            )
          } else if (tolower(Construct[i, 6][[1]]) == "lightgbm") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              categorical_encoding = "Enum",
              distribution         = Construct[i, 2][[1]],
              learn_rate           = 0.05,
              max_runtime_secs     = MaxRuntimeSeconds,
              stopping_rounds      = 5,
              stopping_tolerance   = 1e-4,
              stopping_metric      = StoppingMetric,
              score_tree_interval  = 10,
              seed                 = 1234
            )
          }
        }
        
        # Store all models built sorted by metric
        if (tolower(Construct[i, 6][[1]]) == "automl") {
          Grid_Out <- h2o::h2o.getAutoML(project_name = "TestAML")
        } else if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                        "binomial",
                                                        "bernoulli",
                                                        "multinomial")) {
          Decreasing <- TRUE
          Grid_Out   <-
            h2o::h2o.getGrid(
              grid_id = Construct[i, 5][[1]],
              sort_by = StoppingMetric,
              decreasing = Decreasing
            )
        } else {
          Decreasing <- FALSE
          Grid_Out   <-
            h2o::h2o.getGrid(
              grid_id = Construct[i, 5][[1]],
              sort_by = StoppingMetric,
              decreasing = Decreasing
            )
        }
        
        # Store best model
        if (tolower(Construct[i, 6][[1]]) == "automl") {
          best_model <- Grid_Out@leader
        } else {
          best_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1]])
        }
        
        # Collect accuracy metric on validation data
        if (tolower(Construct[i, 3][[1]]) == "crossentropy") {
          if (tolower(Construct[i, 2][[1]]) == "multinomial") {
            cc <-
              h2o::h2o.logloss(h2o::h2o.performance(best_model, valid = TRUE))
          } else {
            cc <- h2o::h2o.auc(h2o::h2o.performance(best_model, valid = TRUE))
          }
        } else if (tolower(Construct[i, 3][[1]]) == "absolute") {
          cc <- h2o::h2o.mae(h2o::h2o.performance(best_model, valid = TRUE))
        } else if (tolower(Construct[i, 3][[1]]) %in% c("quadratic", "huber")) {
          cc <- h2o::h2o.mse(h2o::h2o.performance(best_model, valid = TRUE))
        } else {
          cc <-
            eval(parse(
              text = paste0(
                "h2o::h2o.",
                tolower(StoppingMetric),
                "(h2o::h2o.performance(best_model, valid = TRUE))"
              )
            ))
        }
        # Store results in metadata file
        data.table::set(grid_tuned_paths,
                        i = i,
                        j = 3L,
                        value = cc)
      }
      
      ######################################
      # Baseline Models
      ######################################
      
      # Check to see if quantile is selected
      # Choose model
      if (tolower(Construct[i, 6][[1]]) != "automl") {
        if (tolower(Construct[i, 2][[1]]) == "quantile") {
          if (tolower(Construct[i, 6][[1]]) == "gbm") {
            bl_model <- h2o::h2o.gbm(
              x                = features,
              y                = target,
              training_frame   = train,
              validation_frame = validate,
              distribution     = Construct[i, 2][[1]],
              quantile_alpha   = Construct[i, 4][[1]],
              model_id         = paste0("BL_GBM_", Construct[i, 5][[1]]),
              ntrees           = BL_Trees
            )
          } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
            bl_model <- h2o::h2o.deeplearning(
              x                = features,
              y                = target,
              hidden           = c(
                floor(N * P4),
                floor(N * P4 * P4),
                floor(N * P4 * P4 * P4),
                floor(N * P4 * P4 * P4 * P4)
              ),
              training_frame   = train,
              validation_frame = validate,
              distribution     = Construct[i, 2][[1]],
              model_id         = paste0("BL_DL_", Construct[i, 5][[1]]),
              quantile_alpha   = Construct[i, 4][[1]]
            )
          }
        } else if (tolower(Construct[i, 6][[1]]) == "gbm") {
          bl_model <- h2o::h2o.gbm(
            x                = features,
            y                = target,
            training_frame   = train,
            validation_frame = validate,
            distribution     = Construct[i, 2][[1]],
            model_id         = paste0("BL_GBM_", Construct[i, 5][[1]]),
            ntrees           = BL_Trees
          )
        } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
          bl_model <- h2o::h2o.deeplearning(
            x                = features,
            y                = target,
            hidden           = c(
              floor(N * P4),
              floor(N * P4 * P4),
              floor(N * P4 * P4 * P4),
              floor(N * P4 * P4 * P4 * P4)
            ),
            training_frame   = train,
            validation_frame = validate,
            model_id         = paste0("BL_DL_", Construct[i, 5][[1]]),
            distribution     = Construct[i, 2][[1]]
          )
        } else if (tolower(Construct[i, 6][[1]]) == "randomforest") {
          bl_model <- h2o::h2o.randomForest(
            x                = features,
            y                = target,
            training_frame   = train,
            validation_frame = validate,
            model_id         = paste0("BL_RF_", Construct[i, 5][[1]]),
            ntrees           = BL_Trees
          )
        } else if (tolower(Construct[i, 6][[1]]) == "xgboost") {
          bl_model <- h2o::h2o.xgboost(
            x                = features,
            y                = target,
            training_frame   = train,
            validation_frame = validate,
            categorical_encoding = "Enum",
            distribution     = Construct[i, 2][[1]],
            model_id         = paste0("BL_XG_", Construct[i, 5][[1]]),
            ntrees           = BL_Trees
          )
        } else if (tolower(Construct[i, 6][[1]]) == "lightgbm") {
          bl_model <- h2o::h2o.xgboost(
            x                = features,
            y                = target,
            training_frame   = train,
            validation_frame = validate,
            categorical_encoding = "Enum",
            distribution     = Construct[i, 2][[1]],
            tree_method      = "hist",
            grow_policy      = "lossguide",
            model_id         = paste0("BL_lgbm_", Construct[i, 5][[1]]),
            ntrees           = BL_Trees
          )
        }
        
        # Collect accuracy metric on validation data
        if (tolower(Construct[i, 3][[1]]) == "crossentropy") {
          if (tolower(Construct[i, 2][[1]]) == "multinomial") {
            dd <- h2o::h2o.logloss(h2o::h2o.performance(bl_model, valid = TRUE))
          } else {
            dd <- h2o::h2o.auc(h2o::h2o.performance(bl_model, valid = TRUE))
          }
        } else if (tolower(Construct[i, 3][[1]]) == "absolute") {
          dd <- h2o::h2o.mae(h2o::h2o.performance(bl_model, valid = TRUE))
        } else if (tolower(Construct[i, 3][[1]]) %in% c("quadratic", "huber")) {
          dd <- h2o::h2o.mse(h2o::h2o.performance(bl_model, valid = TRUE))
        } else {
          dd <-
            eval(parse(
              text = paste0(
                "h2o::h2o.",
                tolower(StoppingMetric),
                "(h2o::h2o.performance(bl_model, valid = TRUE))"
              )
            ))
        }
        
        # Store results in metadata file
        data.table::set(grid_tuned_paths,
                        i = i,
                        j = 4L,
                        value = dd)
      }
      
      ######################################
      # Model Evaluation & Saving
      ######################################
      
      # Check to see if GridTune is TRUE
      # Check to see if Distribution is multinomial
      # Proceed
      
      if (tolower(Construct[i, 6][[1]]) == "automl") {
        if (Construct[i, 21][[1]] == TRUE) {
          if (grid_tuned_paths[i, 2][[1]] != "a")
            file.remove(grid_tuned_paths[i, 2][[1]])
          if (tolower(Construct[i, 22][[1]]) == "standard") {
            save_model <-
              h2o::h2o.saveModel(object = best_model,
                                 path = model_path,
                                 force = TRUE)
            data.table::set(
              grid_tuned_paths,
              i = i,
              j = 2L,
              value = save_model
            )
            if (SaveToFile == TRUE) {
              save(grid_tuned_paths,
                   file = paste0(model_path, "/grid_tuned_paths.Rdata"))
            }
          } else {
            save_model <-
              h2o::h2o.saveMojo(object = best_model,
                                path = model_path,
                                force = TRUE)
            h2o::h2o.download_mojo(
              model = best_model,
              path = model_path,
              get_genmodel_jar = TRUE,
              genmodel_path = model_path,
              genmodel_name = Construct[i, 5][[1]]
            )
            data.table::set(
              grid_tuned_paths,
              i = i,
              j = 2L,
              value = save_model
            )
            data.table::set(
              grid_tuned_paths,
              i = i,
              j = 6L,
              value = paste0(model_path, "\\", Construct[i, 5][[1]])
            )
            if (SaveToFile == TRUE) {
              save(grid_tuned_paths,
                   file = paste0(model_path, "/grid_tuned_paths.Rdata"))
            }
          }
        }
        
        # Save VarImp and VarNOTImp
        if (best_model@algorithm != "stackedensemble") {
          VIMP <- data.table::as.data.table(h2o::h2o.varimp(best_model))
          if (SaveToFile == TRUE) {
            save(VIMP,
                 file = paste0(model_path,
                               "/VarImp_",
                               Construct[i, 5][[1]],
                               ".Rdata"))
          }
          if (tolower(best_model@algorithm) != "glm") {
            NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
          } else {
            NIF <- NULL
          }
          if (length(NIF) > 0) {
            if (SaveToFile == TRUE) {
              save(NIF,
                   file = paste0(
                     model_path,
                     "/VarNOTImp_",
                     Construct[i, 5][[1]],
                     ".Rdata"
                   ))
            }
          }
        } else {
          data.table::set(Construct,
                          i = i,
                          j = 13L,
                          value = 0)
        }
        
        # Gather predicted values
        preds <-
          h2o::h2o.predict(best_model, newdata = validate)[, 1]
        if (Construct[i, 14][[1]] == "All") {
          predsPD <- h2o::h2o.predict(best_model, newdata = data_h2o)[, 1]
          PredsPD <- data.table::as.data.table(predsPD)
          if (SaveToFile == TRUE) {
            data.table::fwrite(PredsPD,
                               file = paste0(model_path,
                                             "/",
                                             Construct[i, 5][[1]],
                                             "_PredsAll.csv"))
          }
        } else if (Construct[i, 14][[1]] == "Train") {
          predsPD <- h2o::h2o.predict(best_model, newdata = train)[, 1]
        } else if (Construct[i, 14][[1]] == "Validate") {
          predsPD <- h2o::h2o.predict(best_model, newdata = validate)[, 1]
        }
      }
      
      if (Construct[i, 11][[1]] == TRUE &
          tolower(Construct[i, 6][[1]]) != "automl") {
        if (!(tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                   "binomial",
                                                   "bernoulli")) |
            tolower(Construct[i, 3][[1]]) == "logloss") {
          if (cc < dd) {
            # Save model
            if (Construct[i, 21][[1]] == TRUE) {
              if (grid_tuned_paths[i, 2][[1]] != "a")
                file.remove(grid_tuned_paths[i, 2][[1]])
              if (tolower(Construct[i, 22][[1]]) == "standard") {
                save_model <-
                  h2o::h2o.saveModel(object = best_model,
                                     path = model_path,
                                     force = TRUE)
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 2L,
                  value = save_model
                )
                if (SaveToFile == TRUE) {
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              } else {
                save_model <-
                  h2o::h2o.saveMojo(object = best_model,
                                    path = model_path,
                                    force = TRUE)
                h2o::h2o.download_mojo(
                  model = best_model,
                  path = model_path,
                  get_genmodel_jar = TRUE,
                  genmodel_path = model_path,
                  genmodel_name = Construct[i, 5][[1]]
                )
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 2L,
                  value = save_model
                )
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 6L,
                  value = paste0(model_path, "\\", Construct[i, 5][[1]])
                )
                if (SaveToFile == TRUE) {
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              }
            }
            
            # Save VarImp and VarNOTImp
            VIMP <-
              data.table::as.data.table(h2o::h2o.varimp(best_model))
            if (SaveToFile == TRUE) {
              save(VIMP,
                   file = paste0(model_path,
                                 "/VarImp_",
                                 Construct[i, 5][[1]],
                                 ".Rdata"))
            }
            NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
            if (length(NIF) > 0) {
              if (SaveToFile == TRUE) {
                save(NIF,
                     file = paste0(
                       model_path,
                       "/VarNOTImp_",
                       Construct[i, 5][[1]],
                       ".Rdata"
                     ))
              }
            }
            
            # Gather predicted values
            preds <-
              h2o::h2o.predict(best_model, newdata = validate)[, 1]
            if (Construct[i, 14][[1]] == "All") {
              predsPD <- h2o::h2o.predict(best_model, newdata = data_h2o)[, 1]
              PredsPD <- as.data.table(predsPD)
              if (SaveToFile == TRUE) {
                data.table::fwrite(PredsPD,
                                   file = paste0(model_path,
                                                 "/",
                                                 Construct[i, 5][[1]],
                                                 "_PredsAll.csv"))
              }
            } else if (Construct[i, 14][[1]] == "Train") {
              predsPD <- h2o::h2o.predict(best_model, newdata = train)[, 1]
            } else if (Construct[i, 14][[1]] == "Validate") {
              predsPD <- h2o::h2o.predict(best_model, newdata = validate)[, 1]
            }
          } else {
            # Save model
            if (Construct[i, 21][[1]] == TRUE) {
              if (grid_tuned_paths[i, 2][[1]] != "a")
                if (SaveToFile == TRUE) {
                  file.remove(grid_tuned_paths[i, 2][[1]])
                }
              if (tolower(Construct[i, 22][[1]]) == "standard") {
                if (SaveToFile == TRUE) {
                  save_model <-
                    h2o::h2o.saveModel(object = bl_model,
                                       path = model_path,
                                       force = TRUE)
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 2L,
                    value = save_model
                  )
                  if (SaveToFile == TRUE) {
                    save(
                      grid_tuned_paths,
                      file = paste0(model_path, "/grid_tuned_paths.Rdata")
                    )
                  }
                }
              } else {
                if (SaveToFile == TRUE) {
                  save_model <-
                    h2o::h2o.saveMojo(object = bl_model,
                                      path = model_path,
                                      force = TRUE)
                  h2o::h2o.download_mojo(
                    model = bl_model,
                    path = model_path,
                    get_genmodel_jar = TRUE,
                    genmodel_path = model_path,
                    genmodel_name = Construct[i, 5][[1]]
                  )
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 2L,
                    value = save_model
                  )
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 6L,
                    value = paste0(model_path, "\\", Construct[i, 5][[1]])
                  )
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              }
            }
            
            # Save VarImp
            VIMP <-
              data.table::as.data.table(h2o::h2o.varimp(bl_model))
            if (SaveToFile == TRUE) {
              save(VIMP,
                   file = paste0(model_path,
                                 "/VarImp_",
                                 Construct[i, 5][[1]],
                                 ".Rdata"))
            }
            NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
            if (length(NIF) > 0) {
              if (SaveToFile == TRUE) {
                save(NIF,
                     file = paste0(
                       model_path,
                       "/VarNOTImp_",
                       Construct[i, 5][[1]],
                       ".Rdata"
                     ))
              }
            }
            
            # Gather predicted values
            preds <-
              h2o::h2o.predict(bl_model, newdata = validate)[, 1]
            if (Construct[i, 14][[1]] == "All") {
              predsPD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 1]
              PredsPD <- data.table::as.data.table(predsPD)
              if (SaveToFile == TRUE) {
                data.table::fwrite(PredsPD,
                                   file = paste0(model_path,
                                                 "/", Construct[i, 5][[1]],
                                                 "_PredsAll.csv"))
              }
            } else if (Construct[i, 14][[1]] == "Train") {
              predsPD <- h2o::h2o.predict(bl_model, newdata = train)[, 1]
            } else if (Construct[i, 14][[1]] == "Validate") {
              predsPD <- h2o::h2o.predict(bl_model, newdata = validate)[, 1]
            }
          }
        } else {
          if (cc > dd) {
            # Save model
            if (Construct[i, 21][[1]] == TRUE) {
              if (grid_tuned_paths[i, 2][[1]] != "a")
                if (SaveToFile == TRUE) {
                  file.remove(grid_tuned_paths[i, 2][[1]])
                }
              if (tolower(Construct[i, 22][[1]]) == "standard") {
                if (SaveToFile == TRUE) {
                  save_model <-
                    h2o::h2o.saveModel(object = best_model,
                                       path = model_path,
                                       force = TRUE)
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 2L,
                    value = save_model
                  )
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              } else {
                if (SaveToFile == TRUE) {
                  save_model <-
                    h2o::h2o.saveMojo(object = best_model,
                                      path = model_path,
                                      force = TRUE)
                  h2o::h2o.download_mojo(
                    model = best_model,
                    path = model_path,
                    get_genmodel_jar = TRUE,
                    genmodel_path = model_path,
                    genmodel_name = Construct[i, 5][[1]]
                  )
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 2L,
                    value = save_model
                  )
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 6L,
                    value = paste0(model_path, "\\", Construct[i, 5][[1]])
                  )
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              }
            }
            
            # Store threshold
            store_results <-
              data.table::data.table(
                best_model@model$training_metrics@metrics$thresholds_and_metric_scores
              )
            if (tolower(Construct[i, 15][[1]]) == "f1" ||
                is.null(Construct[i, 15][[1]])) {
              Thresh <-
                tryCatch({
                  store_results[order(-f1)][1, 1][[1]]
                }, error = function(x)
                  1)
              Label  <- "f1"
            } else if (tolower(Construct[i, 15][[1]]) == "f2") {
              Thresh <-
                tryCatch({
                  store_results[order(-f2)][1, 1][[1]]
                }, error = function(x)
                  1)
              Label  <- "f2"
            } else if (tolower(Construct[i, 15][[1]]) == "f0point5") {
              Thresh <-
                tryCatch({
                  store_results[order(-f0point5)][1, 1][[1]]
                }, error = function(x)
                  1)
              Label <- "f0point5"
            } else if (tolower(Construct[i, 15][[1]]) == "cs") {
              predsPDD <- h2o::h2o.predict(bl_model,
                                           newdata = data_h2o)[, 3]
              data    <-
                data.table::as.data.table(h2o::h2o.cbind(data_h2o,
                                                         predsPDD))
              data[, eval(Construct[i, 1][[1]]) := as.numeric(as.character(get(Construct[i, 1][[1]])))]
              temp  <- threshOptim(
                data     = data,
                actTar   = Construct[i, 1][[1]],
                predTar  = 'p1',
                tpProfit = Construct[i, 17][[1]],
                tnProfit = Construct[i, 18][[1]],
                fpProfit = Construct[i, 19][[1]],
                fnProfit = Construct[i, 20][[1]]
              )
              Thresh <- temp[[1]]
              Label <- "CS"
            }
            data.table::set(
              grid_tuned_paths,
              i = i,
              j = 5L,
              value = Thresh
            )
            
            # Save VarImp
            VIMP <-
              data.table::as.data.table(h2o::h2o.varimp(best_model))
            if (SaveToFile == TRUE) {
              save(VIMP,
                   file = paste0(model_path,
                                 "/VarImp_", Construct[i, 5][[1]],
                                 ".Rdata"))
            }
            NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
            if (length(NIF) > 0) {
              if (SaveToFile == TRUE) {
                save(NIF,
                     file = paste0(
                       model_path,
                       "/VarNOTImp_",
                       Construct[i, 5][[1]],
                       ".Rdata"
                     ))
              }
            }
            
            # Gather predicted values
            preds <-
              h2o::h2o.predict(best_model, newdata = validate)[, 3]
            if (Construct[i, 14][[1]] == "All") {
              predsPD <- h2o::h2o.predict(best_model, newdata = data_h2o)[, 3]
              PredsPD <- data.table::as.data.table(predsPD)
              if (SaveToFile == TRUE) {
                data.table::fwrite(PredsPD,
                                   file = paste0(model_path,
                                                 "/", Construct[i, 5][[1]],
                                                 "_PredsAll.csv"))
              }
            } else if (Construct[i, 14][[1]] == "Train") {
              predsPD <- h2o::h2o.predict(best_model, newdata = train)[, 3]
            } else if (Construct[i, 14][[1]] == "Validate") {
              predsPD <- h2o::h2o.predict(best_model, newdata = validate)[, 3]
            }
          } else {
            # Save model
            if (Construct[i, 21][[1]] == TRUE) {
              if (grid_tuned_paths[i, 2][[1]] != "a")
                if (SaveToFile == TRUE) {
                  file.remove(grid_tuned_paths[i, 2][[1]])
                }
              if (tolower(Construct[i, 22][[1]]) == "standard") {
                if (SaveToFile == TRUE) {
                  save_model <-
                    h2o::h2o.saveModel(object = bl_model,
                                       path = model_path,
                                       force = TRUE)
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 2L,
                    value = save_model
                  )
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              } else {
                if (SaveToFile == TRUE) {
                  save_model <-
                    h2o::h2o.saveMojo(object = bl_model,
                                      path = model_path,
                                      force = TRUE)
                  h2o::h2o.download_mojo(
                    model = bl_model,
                    path = model_path,
                    get_genmodel_jar = TRUE,
                    genmodel_path = model_path,
                    genmodel_name = Construct[i, 5][[1]]
                  )
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 2L,
                    value = save_model
                  )
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 6L,
                    value = paste0(model_path, "\\", Construct[i, 5][[1]])
                  )
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              }
            }
            
            # Store threshold
            store_results <-
              data.table::data.table(
                bl_model@model$training_metrics@metrics$thresholds_and_metric_scores
              )
            if (tolower(Construct[i, 15][[1]]) == "f1" ||
                is.null(Construct[i, 15][[1]])) {
              Thresh <-
                tryCatch({
                  store_results[order(-f1)][1, 1][[1]]
                }, error = function(x)
                  1)
              Label  <- "f1"
            } else if (tolower(Construct[i, 15][[1]]) == "f2") {
              Thresh <-
                tryCatch({
                  store_results[order(-f2)][1, 1][[1]]
                }, error = function(x)
                  1)
              Label  <- "f2"
            } else if (tolower(Construct[i, 15][[1]]) == "f0point5") {
              Thresh <-
                tryCatch({
                  store_results[order(-f0point5)][1, 1][[1]]
                }, error = function(x)
                  1)
              Label <- "f0point5"
            } else if (tolower(Construct[i, 15][[1]]) == "CS") {
              predsPDD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 3]
              data    <-
                data.table::as.data.table(h2o::h2o.cbind(data_h2o, predsPDD))
              data[, eval(Construct[i, 1][[1]]) := as.numeric(as.character(get(Construct[i, 1][[1]])))]
              temp  <- threshOptim(
                data     = data,
                actTar   = Construct[i, 1][[1]],
                predTar  = 'p1',
                tpProfit = Construct[i, 17][[1]],
                tnProfit = Construct[i, 18][[1]],
                fpProfit = Construct[i, 19][[1]],
                fnProfit = Construct[i, 20][[1]]
              )
              Thresh <- temp[[1]]
              Label <- "CS"
            }
            data.table::set(
              grid_tuned_paths,
              i = i,
              j = 5L,
              value = Thresh
            )
            
            # Save VarImp
            VIMP <-
              data.table::as.data.table(h2o::h2o.varimp(bl_model))
            if (SaveToFile == TRUE) {
              save(VIMP,
                   file = paste0(model_path,
                                 "/VarImp_", Construct[i, 5][[1]],
                                 ".Rdata"))
            }
            NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
            if (length(NIF) > 0) {
              if (SaveToFile == TRUE) {
                save(NIF,
                     file = paste0(
                       model_path,
                       "/VarNOTImp_",
                       Construct[i, 5][[1]],
                       ".Rdata"
                     ))
              }
            }
            
            # Gather predicted values
            preds <-
              h2o::h2o.predict(bl_model, newdata = validate)[, 3]
            if (Construct[i, 14][[1]] == "All") {
              predsPD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 3]
              PredsPD <- data.table::as.data.table(predsPD)
              if (SaveToFile == TRUE) {
                data.table::fwrite(PredsPD,
                                   file = paste0(model_path,
                                                 "/",
                                                 Construct[i, 5][[1]],
                                                 "_PredsAll.csv"))
              }
            } else if (Construct[i, 14][[1]] == "Train") {
              predsPD <- h2o::h2o.predict(bl_model, newdata = train)[, 3]
            } else if (Construct[i, 14][[1]] == "Validate") {
              predsPD <- h2o::h2o.predict(bl_model, newdata = validate)[, 3]
            }
          }
        }
      } else if (tolower(Construct[i, 6][[1]]) != "automl") {
        # Save model
        if (Construct[i, 21][[1]] == TRUE) {
          if (grid_tuned_paths[i, 2][[1]] != "a")
            if (SaveToFile == TRUE) {
              file.remove(grid_tuned_paths[i, 2][[1]])
            }
          if (tolower(Construct[i, 22][[1]]) == "standard") {
            if (SaveToFile == TRUE) {
              save_model <-
                h2o::h2o.saveModel(object = bl_model,
                                   path = model_path,
                                   force = TRUE)
              data.table::set(
                grid_tuned_paths,
                i = i,
                j = 2L,
                value = save_model
              )
              save(grid_tuned_paths,
                   file = paste0(model_path,
                                 "/grid_tuned_paths.Rdata"))
            }
          } else {
            if (SaveToFile == TRUE) {
              save_model <-
                h2o::h2o.saveMojo(object = bl_model,
                                  path = model_path,
                                  force = TRUE)
              h2o::h2o.download_mojo(
                model = bl_model,
                path = model_path,
                get_genmodel_jar = TRUE,
                genmodel_path = model_path,
                genmodel_name = Construct[i, 5][[1]]
              )
              data.table::set(
                grid_tuned_paths,
                i = i,
                j = 2L,
                value = save_model
              )
              data.table::set(
                grid_tuned_paths,
                i = i,
                j = 6L,
                value = paste0(model_path, "\\", Construct[i, 5][[1]])
              )
              save(grid_tuned_paths,
                   file = paste0(model_path,
                                 "/grid_tuned_paths.Rdata"))
            }
          }
        }
        
        # Store threshold for binary classification
        if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                 "binomial",
                                                 "bernoulli")) {
          store_results <-
            data.table::data.table(bl_model@model$training_metrics@metrics$thresholds_and_metric_scores)
          if (tolower(Construct[i, 15][[1]]) == "f1" ||
              is.null(Construct[i, 15][[1]])) {
            Thresh <-
              tryCatch({
                store_results[order(-f1)][1, 1][[1]]
              }, error = function(x)
                1)
            Label  <- "f1"
          } else if (tolower(Construct[i, 15][[1]]) == "f2") {
            Thresh <-
              tryCatch({
                store_results[order(-f2)][1, 1][[1]]
              }, error = function(x)
                1)
            Label  <- "f2"
          } else if (tolower(Construct[i, 15][[1]]) == "f0point5") {
            Thresh <-
              tryCatch({
                store_results[order(-f0point5)][1, 1][[1]]
              }, error = function(x)
                1)
            Label <- "f0point5"
          } else if (tolower(Construct[i, 15][[1]]) == "cs") {
            predsPDD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 3]
            data    <-
              data.table::as.data.table(h2o::h2o.cbind(data_h2o, predsPDD))
            data[, eval(Construct[i, 1][[1]]) := as.numeric(as.character(get(Construct[i, 1][[1]])))]
            temp  <- threshOptim(
              data     = data,
              actTar   = Construct[i, 1][[1]],
              predTar  = 'p1',
              tpProfit = Construct[i, 17][[1]],
              tnProfit = Construct[i, 18][[1]],
              fpProfit = Construct[i, 19][[1]],
              fnProfit = Construct[i, 20][[1]]
            )
            Thresh <- temp[[1]]
            Label <- "CS"
          }
          data.table::set(
            grid_tuned_paths,
            i = i,
            j = 5L,
            value = Thresh
          )
          preds <-
            h2o::h2o.predict(bl_model, newdata = validate)[, 3]
          if (tolower(Construct[i, 14][[1]]) == "all") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 3]
            PredsPD <- data.table::as.data.table(predsPD)
            if (SaveToFile == TRUE) {
              fwrite(PredsPD,
                     file = paste0(model_path,
                                   "/", Construct[i, 5][[1]],
                                   "_PredsAll.csv"))
            }
          } else if (tolower(Construct[i, 14][[1]]) == "train") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = train)[, 3]
          } else if (tolower(Construct[i, 14][[1]]) == "validate") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = validate)[, 3]
          }
        } else {
          # Store predicted values against validate data for calibration plot
          preds <-
            h2o::h2o.predict(bl_model, newdata = validate)[, 1]
          if (tolower(Construct[i, 14][[1]]) == "all") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 1]
            PredsPD <- data.table::as.data.table(predsPD)
            if (SaveToFile == TRUE) {
              data.table::fwrite(PredsPD,
                                 file = paste0(model_path,
                                               "/",
                                               Construct[i, 5][[1]],
                                               "_PredsAll.csv"))
            }
          } else if (tolower(Construct[i, 14][[1]]) == "train") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = train)[, 1]
          } else if (tolower(Construct[i, 14][[1]]) == "validate") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = validate)[, 1]
          }
        }
        
        # Save VarImp
        VIMP <- data.table::as.data.table(h2o::h2o.varimp(bl_model))
        if (SaveToFile == TRUE) {
          save(VIMP,
               file = paste0(model_path,
                             "/VarImp_",
                             Construct[i, 5][[1]],
                             ".Rdata"))
        }
        NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
        if (length(NIF) > 0) {
          if (SaveToFile == TRUE) {
            save(NIF,
                 file = paste0(model_path,
                               "/VarNOTImp_",
                               Construct[i, 5][[1]],
                               ".Rdata"))
          }
        }
      }
      
      ######################################
      # Model Evaluation Plots
      ######################################
      
      # Generate plots
      col <- Construct[i, 1][[1]]
      calibration <-
        data.table::as.data.table(h2o::h2o.cbind(preds, validate[, col]))
      if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                               "binomial",
                                               "bernoulli")) {
        calibration[, eval(col) := as.numeric(as.character(get(col)))]
      }
      if (Construct[i, 13][[1]] >= 1) {
        if (tolower(Construct[i, 14][[1]]) == "all") {
          calibEval <-
            data.table::as.data.table(h2o::h2o.cbind(preds, validate))
          calib <-
            data.table::as.data.table(h2o::h2o.cbind(predsPD, data_h2o))
        } else if (tolower(Construct[i, 14][[1]]) == "train") {
          calibEval <-
            data.table::as.data.table(h2o::h2o.cbind(preds, validate))
          calib <- as.data.table(h2o::h2o.cbind(predsPD, train))
        } else if (tolower(Construct[i, 14][[1]]) == "validate") {
          calibEval <-
            data.table::as.data.table(h2o::h2o.cbind(preds, validate))
          calib <- as.data.table(h2o::h2o.cbind(predsPD, validate))
        }
        if (Construct[i, 12][[1]]) {
          if (SaveToFile == TRUE) {
            save(calibEval,
                 file = paste0(model_path,
                               "/", Construct[i, 5][[1]],
                               "_Validation.Rdata"))
          }
        }
      } else {
        if (Construct[i, 12][[1]]) {
          calibEval <-
            data.table::as.data.table(h2o::h2o.cbind(preds, validate))
          if (SaveToFile == TRUE) {
            save(calibEval,
                 file = paste0(model_path,
                               "/",
                               Construct[i, 5][[1]],
                               "_Validation.Rdata"))
          }
        }
      }
      predName <- names(calibration[, 1])
      
      # Generate evaluation plots
      if (tolower(Construct[i, 2][[1]]) != "multinomial") {
        if (tolower(Construct[i, 2][[1]]) == "quantile") {
          # Store best metric
          if (Construct[i, 11][[1]]) {
            if (cc < dd) {
              val <- cc
            } else {
              val <- dd
            }
          } else {
            val <- dd
          }
          
          # Calibration plot
          out1 <- EvalPlot(
            calibration,
            PredictionColName = predName,
            TargetColName  = Construct[i, 1][[1]],
            GraphType        = "calibration",
            PercentileBucket      = 0.05,
            aggrfun     = function(x)
              quantile(x,
                       probs = Construct[i, 4][[1]],
                       na.rm = TRUE)
          )
          out1 <- out1 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(Construct[i, 3][[1]]),
            ": ",
            round(val, 4)
          ))
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalP_",
                                   Construct[i, 5][[1]],
                                   ".png"))
          }
          
          # Calibration boxplot
          out2 <- EvalPlot(
            calibration,
            PredictionColName = predName,
            TargetColName  = Construct[i, 1][[1]],
            GraphType        = "boxplot",
            PercentileBucket      = 0.05
          )
          out2 <- out2 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(Construct[i, 3][[1]]),
            ": ",
            round(val, 4)
          ))
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalBP_",
                                   Construct[i, 5][[1]],
                                   ".png"))
          }
        } else if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                        "binomial",
                                                        "bernoulli")) {
          # Store best metric
          if (Construct[i, 11][[1]]) {
            if (cc < dd) {
              val <- cc
            } else {
              val <- dd
            }
          } else {
            val <- dd
          }
          
          out1 <- EvalPlot(
            calibration,
            PredictionColName = predName,
            TargetColName  = Construct[i, 1][[1]],
            GraphType        = "calibration",
            PercentileBucket      = 0.05,
            aggrfun     = function(x)
              base::mean(x, na.rm = TRUE)
          )
          out1 <- out1 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(Construct[i, 3][[1]]),
            ": ",
            round(val, 4)
          ))
          
          if (exists("Thresh")) {
            out1 <- out1 + ggplot2::geom_hline(yintercept = Thresh)
          }
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalP_",
                                   Construct[i, 5][[1]],
                                   ".png"))
          }
        } else {
          # Store best metric
          if (Construct[i, 11][[1]]) {
            if (cc < dd) {
              val <- cc
            } else {
              val <- dd
            }
          } else {
            val <- dd
          }
          
          # Calibration plot
          out1 <- EvalPlot(
            calibration,
            PredictionColName = predName,
            TargetColName  = Construct[i, 1][[1]],
            GraphType        = "calibration",
            PercentileBucket      = 0.05,
            aggrfun     = function(x)
              base::mean(x, na.rm = TRUE)
          )
          out1 <- out1 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(Construct[i, 3][[1]]),
            ": ",
            round(val, 4)
          ))
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalP_",
                                   Construct[i, 5][[1]],
                                   ".png"))
          }
          
          # Calibration boxplot
          out2 <- EvalPlot(
            calibration,
            PredictionColName = predName,
            TargetColName  = Construct[i, 1][[1]],
            GraphType        = "boxplot",
            PercentileBucket      = 0.05
          )
          out2 <- out2 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(Construct[i, 3][[1]]),
            ": ",
            round(val, 4)
          ))
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalBP_",
                                   Construct[i, 5][[1]],
                                   ".png"))
          }
        }
      } else {
        # Multinomial case
        # Stack each level's predicted values and actual values
        if (Construct[i, 11][[1]] && cc <= dd) {
          predsMulti <- h2o::h2o.predict(best_model, newdata = validate)
          col <- Construct[i, 1][[1]]
          xx <-
            data.table::as.data.table(h2o::h2o.cbind(validate[, col],
                                                     predsMulti))
          if (Construct[i, 12][[1]]) {
            calib <- data.table::as.data.table(h2o::h2o.cbind(validate,
                                                              preds))
            if (SaveToFile == TRUE) {
              save(calib,
                   file = paste0(
                     model_path,
                     "/",
                     Construct[i, 5][[1]],
                     "_Validation.Rdata"
                   ))
            }
          }
          N <- (ncol(xx) - 2)
          data <- eval(parse(text = Construct[i, 7][[1]]))
          for (lev in levels(data[[Construct[i, 1][[1]]]])) {
            xx[, paste0("V", lev) := data.table::fifelse(xx[[1]] %in% lev, 1, 0)]
          }
          RemoveCols <- names(xx)[1:2]
          KeepCols   <- names(xx)[3:length(names(xx))]
          xx[, (RemoveCols) := NULL]
          store <- list()
          for (k in 1:N) {
            j <- k + N
            temp <- cbind(xx[, ..k], xx[, ..j])
            data.table::setnames(temp, KeepCols[k], "Preds")
            data.table::setnames(temp, KeepCols[j], "Act")
            store[[k]] <- temp
          }
          xxx <- data.table::rbindlist(store)
          
          # Multinomial metric
          if (multinomialMetric == "auc") {
            val <- H2OMultinomialAUC(
              validate,
              best_model,
              targetColNum = 1,
              targetName = Construct[i, 1][[1]]
            )
          } else {
            xx <- data.table::as.data.table(h2o::h2o.cbind(
              validate[, 1],
              h2o::h2o.predict(best_model,
                               newdata = validate)[, 1]
            ))
            names(xx)
            val <-
              mean(xx[, Accuracy := as.numeric(data.table::fifelse(get(Construct[i, 1][[1]]) == predict, 1, 0))][["Accuracy"]],
                   na.rm = TRUE)
          }
          
          # Store baseline val
          temp <- H2OMultinomialAUC(validate,
                                    best_model,
                                    targetColNum = 1,
                                    targetName = Construct[i, 1][[1]])
          
          # Store micro auc
          data.table::set(
            grid_tuned_paths,
            i = i,
            j = 3L,
            value = val
          )
          data.table::set(
            grid_tuned_paths,
            i = i,
            j = 4L,
            value = temp
          )
          
          # Calibration plot
          out1 <- EvalPlot(
            xxx,
            PredictionColName = "Preds",
            TargetColName  = "Act",
            GraphType        = "calibration",
            PercentileBucket      = 0.05,
            aggrfun     = function(x)
              base::mean(x, na.rm = TRUE)
          )
          out1 <- out1 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(multinomialMetric),
            ": ",
            round(val, 4)
          ))
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalP_",
                                   Construct[i, 5][[1]],
                                   ".png"))
          }
          
        } else {
          predsMulti <- h2o::h2o.predict(bl_model, newdata = validate)
          col <- Construct[i, 1][[1]]
          xx <-
            data.table::as.data.table(h2o::h2o.cbind(validate[, col],
                                                     predsMulti))
          if (Construct[i, 12][[1]]) {
            calib <- data.table::as.data.table(h2o::h2o.cbind(validate,
                                                              preds))
            if (SaveToFile == TRUE) {
              save(calib,
                   file = paste0(
                     model_path,
                     "/",
                     Construct[i, 5][[1]],
                     "_Validation.Rdata"
                   ))
            }
          }
          N <- (ncol(xx) - 2)
          data <- eval(parse(text = Construct[i, 7][[1]]))
          for (lev in levels(data[[Construct[i, 1][[1]]]])) {
            xx[, paste0("V", lev) := data.table::fifelse(xx[[1]] %in% lev, 1, 0)]
          }
          RemoveCols <- names(xx)[1:2]
          KeepCols   <- names(xx)[3:length(names(xx))]
          xx[, (RemoveCols) := NULL]
          store <- list()
          for (k in seq_len(N)) {
            j <- k + N
            temp <- cbind(xx[, ..k], xx[, ..j])
            data.table::setnames(temp, KeepCols[k], "Preds")
            data.table::setnames(temp, KeepCols[j], "Act")
            store[[k]] <- temp
          }
          xxx <- data.table::rbindlist(store)
          
          # Multinomial metric
          if (multinomialMetric == "auc") {
            val <- H2OMultinomialAUC(validate,
                                     bl_model,
                                     targetColNum = 1,
                                     targetName = Construct[i, 1][[1]])
          } else {
            xx <- data.table::as.data.table(h2o::h2o.cbind(
              validate[, 1],
              h2o::h2o.predict(bl_model,
                               newdata = validate)[, 1]
            ))
            names(xx)
            val <-
              mean(xx[, Accuracy := as.numeric(data.table::fifelse(get(Construct[i, 1][[1]]) == predict, 1, 0))][["Accuracy"]],
                   na.rm = TRUE)
          }
          
          # Calibration plot
          out1 <- EvalPlot(
            xxx,
            PredictionColName = "Preds",
            TargetColName  = "Act",
            GraphType        = "calibration",
            PercentileBucket      = 0.05,
            aggrfun     = function(x)
              base::mean(x, na.rm = TRUE)
          )
          out1 <- out1 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(multinomialMetric),
            ": ",
            round(val, 4)
          ))
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalP_",
                                   Construct[i, 5][[1]], ".png"))
          }
        }
        
        # Store micro auc
        data.table::set(grid_tuned_paths,
                        i = i,
                        j = 4L,
                        value = val)
      }
      
      #######################################
      # Partial dependence calibration plots
      #######################################
      
      if (Construct[i, 13][[1]] >= 1) {
        VIMP <- VIMP[!is.na(VIMP[, 2][[1]])]
        rows <- nrow(VIMP)
        cols <- VIMP[1:min(Construct[i, 13][[1]], rows), 1][[1]]
        calibr <- list()
        boxplotr <- list()
        j <- 0
        if (!(tolower(Construct[i, 2][[1]]) %in% c("multinomial"))) {
          for (col in cols) {
            j <- j + 1
            if (tolower(Construct[i, 2][[1]]) == "quantile") {
              out1 <- tryCatch({
                ParDepCalPlots(
                  calib,
                  PredictionColName = predName,
                  TargetColName  = Construct[i, 1][[1]],
                  IndepVar    = col,
                  GraphType        = "calibration",
                  PercentileBucket      = 0.05,
                  FactLevels  = 10,
                  Function    = function(x)
                    quantile(x,
                             probs = Construct[i, 4][[1]],
                             na.rm = TRUE)
                )
              },
              error = function(x)
                "skip")
            } else {
              out1 <- tryCatch({
                ParDepCalPlots(
                  calib,
                  PredictionColName = predName,
                  TargetColName  = Construct[i, 1][[1]],
                  IndepVar    = col,
                  GraphType        = "calibration",
                  PercentileBucket      = 0.05,
                  FactLevels  = 10,
                  Function    = function(x)
                    base::mean(x, na.rm = TRUE)
                )
              },
              error = function(x)
                "skip")
            }
            
            # Add threshold line to charts
            if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                     "binomial",
                                                     "bernoulli")) {
              if (exists("Thresh")) {
                out1 <- out1 + ggplot2::geom_hline(yintercept = Thresh)
              }
              calibr[[paste0(col)]] <- out1
            } else {
              calibr[[paste0(col)]] <- out1
            }
            
            # Expected value regression
            if (!(tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                       "binomial",
                                                       "bernoulli"))) {
              boxplotr[[paste0(col)]] <- tryCatch({
                ParDepCalPlots(
                  calib,
                  PredictionColName = predName,
                  TargetColName  = Construct[i, 1][[1]],
                  IndepVar    = col,
                  GraphType        = "boxplot",
                  PercentileBucket      = 0.05,
                  FactLevels  = 10
                )
              },
              error = function(x)
                "skip")
            }
          }
          
          # Save output
          if (!(tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                     "binomial",
                                                     "bernoulli"))) {
            if (SaveToFile == TRUE) {
              save(
                boxplotr,
                file = paste0(
                  model_path,
                  "/",
                  Construct[i, 5][[1]],
                  "_ParDepCalBoxPlots.Rdata"
                )
              )
            }
            save(calibr,
                 file = paste0(
                   model_path,
                   "/",
                   Construct[i, 5][[1]],
                   "_ParDepCalPlots.Rdata"
                 ))
          }
        }
      }
      
      # Save grid_tuned_paths
      if (SaveToFile == TRUE) {
        save(grid_tuned_paths,
             file = paste0(model_path, "/grid_tuned_paths.Rdata"))
      }
      
      # Clear H2O environment between runs
      h2o::h2o.rm(data_h2o)
      if (!Construct[i, SupplyData][[1]]) {
        h2o::h2o.rm(data_train)
      }
      h2o::h2o.rm(train)
      h2o::h2o.rm(validate)
      if (Construct[i, 11][[1]]) {
        h2o::h2o.rm(best_model)
      }
      if (Construct[i, 6][[1]] != "automl") {
        h2o::h2o.rm(bl_model)
      }
      h2o::h2o.rm(preds)
      h2o::h2o.shutdown(prompt = FALSE)
      
      # Clear R environment between runs
      if (Construct[i, 11][[1]]) {
        if (Construct[i, 2][[1]] != "multinomial" &
            Construct[i, 21][[1]] == TRUE) {
          rm(grid,
             Grid_Out,
             cc,
             dd,
             VIMP,
             calibration,
             features,
             target,
             save_model)
        } else {
          rm(grid,
             Grid_Out,
             cc,
             dd,
             VIMP,
             features,
             target,
             predsMulti)
        }
      } else {
        if (Construct[i, 2][[1]] != "multinomial") {
          rm(dd,
             VIMP,
             calibration,
             features,
             target,
             save_model)
        } else {
          rm(dd, VIMP, features, target)
        }
      }
      
      # Remove data if no longer needed
      if (i > 1) {
        if (Construct[i, 7][[1]] != Construct[(i - 1), 7][[1]]) {
          eval(parse(text = paste0("rm(", Construct[(i - 1), 7][[1]], ")")))
        }
      }
    }
  }, error = function(x)
    h2o::h2o.shutdown(prompt = FALSE))
  if (ReturnObjects) {
    return(list(Construct = Construct, GridTunedPaths = grid_tuned_paths))
  }
}
