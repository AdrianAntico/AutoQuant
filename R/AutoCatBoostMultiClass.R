#' AutoCatBoostMultiClass is an automated catboost model grid-tuning multinomial classifier and evaluation systems
#'
#' AutoCatBoostMultiClass is an automated modeling function that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, ROC plot, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param TestData If you want to supply your own data for testing. Column names and column ordering must be the same as data.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located, but not mixed types. Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located, but not mixed types. Also, not zero-indexed.
#' @param CatFeatures A vector of column numbers of your categorical features, not zero indexed.
#' @param TrainSplitRatio A decimal between 0.01 and 0.99 that tells the function how much data to keep for training and validation.
#' @param task_type "GPU" Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param eval_metric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "AUC" is the default, but other options include "Logloss", "CrossEntropy", "Precision", "Recall", "F1", "BalancedAccuracy", "BalancedErrorRate", "MCC", "Accuracy", "CtrFactor", "AUC", "BrierScore", "HingeLoss", "HammingLoss", "ZeroOneLoss", "Kappa", "WKappa", "LogLikelihoodOfPrediction"
#' @param grid_eval_metric This is the metric used to find the threshold "f","auc","tpr","fnr","fpr","tnr","prbe","f","odds"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options. 1080 total possible options
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects. E.g. plots and evaluation metrics
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' # Dummify Categorical Variable
#' data <- RemixAutoML::DummifyDT(data = data,
#'                                cols = "Independent_Variable11",
#'                                KeepFactorCols = FALSE,
#'                                OneHot = TRUE,
#'                                ClustScore = FALSE)
#' data[, Predict := (pnorm(Correl * x1 +
#'                            sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' data[, Target := ifelse(Target < 0.5, 1, 0)]
#' TestModel <- AutoCatBoostClassifier(data,
#'                                     TestData = NULL,
#'                                     TargetColumnName = "Target",
#'                                     FeatureColNames = c(2:11),
#'                                     CatFeatures = NULL,
#'                                     MaxModelsInGrid = 3,
#'                                     TrainSplitRatio = 0.80,
#'                                     task_type = "GPU",
#'                                     eval_metric = "AUC",
#'                                     grid_eval_metric = "auc",
#'                                     Trees = 50,
#'                                     GridTune = FALSE,
#'                                     model_path = NULL,
#'                                     ModelID = "ModelTest",
#'                                     NumOfParDepPlots = 15,
#'                                     ReturnModelObjects = TRUE,
#'                                     SaveModelObjects = FALSE)
#' }
#' @return Saves to file and returned in list: _ModelID_VariableImportance.csv, _ModelID_ (the model), _ModelID_ValidationData.csv, _ModelID_ROC_Plot.png, _ModelID_EvalutionPlot.png, _ModelID_EvaluationMetrics.csv, _ModelID_ParDepPlots.R a named list of features with partial dependence calibration plots, _ModelID_ParDepBoxPlots.R, _ModelID_GridCollect, and _ModelID_GridList
#' @export
AutoCatBoostClassifier <- function(data,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   CatFeatures = NULL,
                                   TrainSplitRatio = 0.80,
                                   task_type = "GPU",
                                   eval_metric = "MultiClass",
                                   Trees = 50,
                                   GridTune = FALSE,
                                   grid_eval_metric = "f",
                                   MaxModelsInGrid = 10,
                                   model_path = NULL,
                                   ModelID = "FirstModel",
                                   NumOfParDepPlots = 3,
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE) {
  
  # Dont run if model_path is null
  if(is.null(model_path)) {
    warning("Need a model_path defined to run this function")
  } else {
    
    # Ensure packages are available
    requireNamespace('data.table', quietly = TRUE)
    if(!requireNamespace('catboost', quietly = TRUE)) {
      warning("catboost needs to be installed. See documentation")
    } else {
      
      # Binary Check Arguments----
      if(!(abs(TrainSplitRatio) <= 0.99)) warning("TrainSplitRatio needs to be less than or equal to 0.99")
      if(!(tolower(task_type) %chin% c("gpu","cpu"))) warning("task_type needs to be either 'GPU' or 'CPU'")
      if(!(tolower(eval_metric) %chin% c("logloss","crossentropy",
                                         "precision","recall",
                                         "f1","balancedaccuracy",
                                         "balancederrorrate","mcc",
                                         "accuracy","ctrfactor",
                                         "auc","brierscore",
                                         "hingeloss","hammingloss",
                                         "zerooneloss","kappa",
                                         "wkappa","loglikelihoodofprediction"))) {
        warning("eval_metric not in c('Logloss','CrossEntropy',
                              'Precision','Recall',
                              'F1','BalancedAccuracy',
                              'BalancedErrorRate','MCC',
                              'Accuracy','CtrFactor',
                              'AUC','BrierScore',
                              'HingeLoss','HammingLoss',
                              'ZeroOneLoss','Kappa',
                              'WKappa','LogLikelihoodOfPrediction')")
        
      }
      if(Trees < 1) warning("Trees must be greater than 1")
      if(!GridTune %in% c(TRUE,FALSE)) warning("GridTune needs to be TRUE or FALSE")
      if(!(tolower(grid_eval_metric) %chin% c("accuracy","auc","tpr","fnr","fpr","tnr","prbe","f","odds","chisq"))) {
        warning("grid_eval_metric not in c('accuracy','auc','tpr','fnr','fpr','tnr','prbe','f','odds','chisq')")
      }
      if(MaxModelsInGrid < 1 | MaxModelsInGrid > 1080 & GridTune == TRUE) {
        warning("MaxModelsInGrid needs to be at least 1 and less than 1080")
      }
      if(!is.character(model_path)) warning("model_path needs to be a character type")
      if(!is.character(ModelID)) warning("ModelID needs to be a character type")
      if(NumOfParDepPlots < 0) warning("NumOfParDepPlots needs to be a positive number")
      if(!(ReturnModelObjects %in% c(TRUE,FALSE))) warning("ReturnModelObjects needs to be TRUE or FALSE")
      if(!(SaveModelObjects %in% c(TRUE,FALSE))) warning("SaveModelObjects needs to be TRUE or FALSE")
      
      # Binary Ensure data is a data.table----
      if(!data.table::is.data.table(data)) {
        data <- data.table::as.data.table(data)
      }
      
      # Binary Convert CatFeatures to 1-indexed----
      if(!is.null(CatFeatures)) {
        CatFeatures <- c((CatFeatures[1]-1):(CatFeatures[length(CatFeatures)]-1))
      }
      
      # Binary Subset Columns Needed----
      if((is.numeric(TargetColumnName) | is.integer(TargetColumnName)) & (is.numeric(FeatureColNames) | is.integer(FeatureColNames))) {
        keep1 <- names(data)[c(FeatureColNames)]
        keep2 <- names(data)[c(TargetColumnName)]
        keep <- c(keep1, keep2)
        data <- data[, ..keep]
      } else if ((is.numeric(TargetColumnName) | is.integer(TargetColumnName)) & is.character(FeatureColNames)) {
        keep2 <- names(data)[c(TargetColumnName)]
        keep <- c(FeatureColNames, keep2)
        data <- data[, ..keep]
      } else if (is.character(TargetColumnName) & (is.numeric(FeatureColNames) | is.integer(FeatureColNames))) {
        keep1 <- names(data)[c(FeatureColNames)]
        keep <- c(TargetColumnName, keep1)
        data <- data[, ..keep]
      } else if (is.character(TargetColumnName) & is.character(FeatureColNames)) {
        keep <- c(FeatureColNames, TargetColumnName)
        data <- data[, ..keep]
      }
      
      # Binary Target Name Storage----
      if(is.character(TargetColumnName)) {
        Target <- TargetColumnName
      } else {
        Target <- names(data)[TargetColumnName]
      }
      
      # MultiClass Convert Target to Numeric Factor
      StoreLabels <- sort(unique(data[[eval(Target)]]))
      data[, Target := as.numeric(factor(Target))]
      NewLabels <- sort(unique(data[[eval(Target)]]))
      LabelLookup <- data.table::as.data.table(cbind(as.character(StoreLabels), NewLabels))
      data.table::setnames(LabelLookup, c("V1"), c("Target1"))
      LabelLookup[, NewLabels := as.numeric(NewLabels)]
      
      # Binary Save Names of data----
      Names <- data.table::as.data.table(names(data))
      data.table::setnames(Names, "V1", "ColNames")
      if(SaveModelObjects) {
        data.table::fwrite(Names, paste0(model_path, "/",ModelID,"_ColNames.csv"))
      }
      
      # Binary Data Partition----
      if(is.null(TestData)) {
        dataTrain <- data[, RANDOMNUMER := runif(nrow(data))][order(RANDOMNUMER)][1:(nrow(data)*TrainSplitRatio)]
        dataTest <- data[(nrow(data) * TrainSplitRatio + 1):nrow(data)]
      } else {
        dataTrain <- data[, RANDOMNUMER := runif(nrow(data))][order(RANDOMNUMER)][1:(nrow(data)*TrainSplitRatio)]
        dataTest <- TestData[, RANDOMNUMER := runif(nrow(data))][order(RANDOMNUMER)][1:(nrow(data)*TrainSplitRatio)]
      }
      dataTrain[, RANDOMNUMER := NULL]
      dataTest[, RANDOMNUMER := NULL]
      
      # Binary Subset Target Variables----
      TrainTarget <- tryCatch({dataTrain[, get(Target)]}, error = function(x) dataTrain[, eval(Target)])
      TestTarget <- tryCatch({dataTest[, get(Target)]}, error = function(x) dataTest[, eval(Target)])
      
      # Binary Initialize Catboost Data Conversion----
      if(!is.null(CatFeatures)) {
        TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget, cat_features = CatFeatures)
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
      } else {
        TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
      }
      
      
      # Binary Grid Tune or Not Check----
      if(GridTune) {
        
        # Binary Grid Create data.table To Store Results----
        GridCollect <- data.table::data.table(ParamRow = 1:(MaxModelsInGrid + 1),
                                              EvalStat = rep(9999999, MaxModelsInGrid + 1))
        
        # Binary Grid Define Hyper Parameters----
        catboostGridList <- data.table::CJ(l2_leaf_reg = c(0,0.01,0.02,0.03,0.04,0.05),
                                           learning_rate = c(0.01,0.02,0.03,0.04,0.05),
                                           bootstrap_type = c("Poisson","Bayesian","Bernoulli","No"),
                                           depth = c(4:12))
        catboostGridList[, ID := runif(nrow(catboostGridList))]
        catboostGridList <- catboostGridList[order(ID)][1:(MaxModelsInGrid + 1)][, ID := NULL]
        
        # Binary AUC List----
        AUC_List <- list()
        
        # Binary Grid Tuning Main Loop----
        for(i in as.integer(seq_len(MaxModelsInGrid + 1))) {
          
          # Print i
          print(i)
          
          # Binary Grid Define Base Parameters----
          base_params <- list(iterations           = Trees,
                              loss_function        = 'MultiClass',
                              eval_metric          = 'MultiClass',
                              use_best_model       = TRUE,
                              best_model_min_trees = 10,
                              metric_period        = 10,
                              task_type            = task_type)
          
          # Binary Grid Merge Model Parameters----
          # Have first model be the baseline model
          if(i != 1) {
            base_params <- c(as.list(catboostGridList[i,]), base_params)
          }
          
          # Binary Grid Train Model----
          model <- catboost::catboost.train(learn_pool = TrainPool,
                                            test_pool  = TestPool,
                                            params     = base_params)
          
          # Binary Grid Score Model----
          predict <- catboost::catboost.predict(model = model,
                                                pool = TestPool,
                                                prediction_type = "Probability")
          
          # Binary Grid Validation Data----
          calibEval <- data.table::as.data.table(
            cbind(Target = TestTarget, predict))
          calibEval <- merge(calibEval, 
                             LabelLookup, 
                             by.x = "Target", 
                             by.y = "NewLabels", 
                             all = FALSE)
          
          
          # Binary Grid Evaluation Metrics for Each Grid----
          if(tolower(grid_eval_metric) == "accuracy") {
            j <- 0
            x <- data.table::data.table(Metric = "Accuracy",
                                        MetricValue = 5.0,
                                        Threshold = seq(0.01,0.99,0.001))
            for (k in unique(x[["Threshold"]])) {
              j = as.integer(j + 1)
              Accuracy <- mean(calibEval[, ifelse(p1 > k & Target == 1 | p1 < k & Target == 0, 1, 0)])
              data.table::set(x, i = j, j = 2L, value = round(Accuracy,4))
            }
            data.table::setorderv(x, "MetricValue", order = -1, na.last = TRUE)
            Metric <- x[1,MetricValue]
          } else {
            x <- ROCR::prediction(predictions = calibEval[["p1"]], labels = calibEval[["Target"]])
            y <- ROCR::performance(prediction.obj = x, measure = grid_eval_metric)
            if(any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                   nrow(data.table::as.data.table(y@x.values)) <= 1)) {
              if(nrow(data.table::as.data.table(y@y.values)) <= 1 & nrow(data.table::as.data.table(y@x.values)) <= 1) {
                z <- data.table::as.data.table(cbind(Metric = y@y.values, Threshold = y@x.values))
                Metric <- z[[1]]
              } else if(nrow(data.table::as.data.table(y@y.values)) <= 1 & !(nrow(data.table::as.data.table(y@x.values) <= 1))) {
                z <- data.table::as.data.table(cbind(Metric = y@y.values, Threshold = y@x.values[[1]]))
                Metric <- z[!is.infinite(Threshold)][[1]]
              } else if(!(nrow(data.table::as.data.table(y@y.values)) <= 1) & nrow(data.table::as.data.table(y@x.values) <= 1)) {
                if(grid_eval_metric %chin% c("auc","tpr","tnr","prbe","f","odds")) {
                  z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values))
                  Metric <- z[order(-Metric)][!is.infinite(Metric)][[1]]
                } else {
                  z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values))
                  Metric <- z[order(Metric)][!is.infinite(Metric)][[1]]
                }
              }
            } else {
              if(metric %chin% c("auc","tpr","tnr","prbe","f","odds")) {
                z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values[[1]]))
                Metric <- z[order(-Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1,]
              } else {
                z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values[[1]]))
                Metric <- z[order(Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1,]
              }
            }
          }
          
          # Binary AUC Object Create----
          AUC_Metrics <- pROC::roc(response = calibEval[["Target"]],
                                   predictor = calibEval[["p1"]],
                                   na.rm = TRUE,
                                   algorithm = 3,
                                   auc = TRUE,
                                   ci = TRUE)
          
          # Binary AUC Conversion to data.table----
          AUC_List[[i]] <- data.table::data.table(
            ModelNumber = i,
            Sensitivity = as.numeric(AUC_Metrics$sensitivities+0.0001),
            Specificity = as.numeric(AUC_Metrics$specificities+0.0001))
          
          # Collect Metrics and Corresponding Grids
          # Store Output Information
          if(tolower(grid_eval_metric) == "accuracy") {
            data.table::set(GridCollect, i = i, j = 1L, value = i)
            data.table::set(GridCollect, i = i, j = 2L, value = Metric)
          } else if(any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                        nrow(data.table::as.data.table(y@x.values)) <= 1)) {
            data.table::set(GridCollect, i = i, j = 1L, value = i)
            data.table::set(GridCollect, i = i, j = 2L, value = Metric)
          } else {
            data.table::set(GridCollect, i = i, j = 1L, value = i)
            data.table::set(GridCollect, i = i, j = 2L, value = Metric[,1])
          }
        }
      }
      
      # Binary Define Final Model Parameters----
      if(GridTune) {
        if(grid_eval_metric %chin% c("accuracy","auc","tpr","tnr","prbe","f","odds")) {
          BestGrid <- GridCollect[order(-EvalStat)][1,ParamRow]
          BestThresh <- GridCollect[order(-EvalStat)][1,EvalStat]
        } else {
          BestGrid <- GridCollect[order(EvalStat)][1,ParamRow]
          BestThresh <- GridCollect[order(EvalStat)][1,EvalStat]
        }
        Base_params <- list(iterations           = Trees,
                            learning_rate        = 0.01,
                            depth                = 10,
                            loss_function        = "CrossEntropy",
                            eval_metric          = eval_metric,
                            use_best_model       = TRUE,
                            best_model_min_trees = 10,
                            metric_period        = 10,
                            task_type            = task_type)
        base_params <- c(as.list(catboostGridList[BestGrid,]), Base_params)
      } else {
        base_params <- list(iterations           = Trees,
                            learning_rate        = 0.01,
                            depth                = 10,
                            loss_function        = "CrossEntropy",
                            eval_metric          = eval_metric,
                            use_best_model       = TRUE,
                            best_model_min_trees = 10,
                            metric_period        = 10,
                            task_type            = task_type)
      }
      
      # Binary Train Final Model----
      model <- catboost::catboost.train(learn_pool = TrainPool,
                                        test_pool  = TestPool,
                                        params     = base_params)
      
      # Binary Save Model----
      if(SaveModelObjects) {
        setwd(model_path)
        catboost::catboost.save_model(model = model, model_path = paste0(ModelID))
      }
      
      # Binary Score Final Test Data----
      predict <- catboost::catboost.predict(model = model,
                                            pool = TestPool,
                                            prediction_type = "Probability")
      
      # Binary Validation Data----
      ValidationData <- data.table::as.data.table(
        cbind(Target = TestTarget, dataTest, p1 = predict))
      
      # Save Validation Data to File----
      if(SaveModelObjects) {
        data.table::fwrite(ValidationData,
                           file = paste0(model_path,"/", ModelID,"_ValidationData.csv"))
      }
      
      # Binary AUC Object Create----
      AUC_Metrics <- pROC::roc(response = ValidationData[["Target"]],
                               predictor = ValidationData[["p1"]],
                               na.rm = TRUE,
                               algorithm = 3,
                               auc = TRUE,
                               ci = TRUE)
      
      # Binary AUC Conversion to data.table----
      AUC_Data <- data.table::data.table(
        ModelNumber = 0,
        Sensitivity = AUC_Metrics$sensitivities,
        Specificity = AUC_Metrics$specificities)
      
      # Binary Rbind AUC
      if(GridTune == TRUE & MaxModelsInGrid <= 15) {
        temp <- data.table::rbindlist(AUC_List)
        AUC_Data <- data.table::rbindlist(list(temp,AUC_Data))
        AUC_Data[, ModelNumber := as.factor(ModelNumber)]
        
        # Binary Plot ROC Curve----
        ROC_Plot <- ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity,
                                                           group = ModelNumber,
                                                           color = ModelNumber)) +
          ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]])) +
          ggplot2::geom_abline(slope = 1, color = "black") +
          ggplot2::ggtitle(paste0("Catboost Best Model AUC: ",
                                  100 * round(AUC_Metrics$auc,3),"%")) +
          ChartTheme() + ggplot2::xlab("Specificity") +
          ggplot2::ylab("Sensitivity")
        
      } else {
        ROC_Plot <- ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity)) +
          ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]]), color = "blue") +
          ggplot2::geom_abline(slope = 1, color = "black") +
          ggplot2::ggtitle(paste0("Catboost AUC: ",
                                  100 * round(AUC_Metrics$auc,3),"%")) +
          ChartTheme() + ggplot2::xlab("Specificity") +
          ggplot2::ylab("Sensitivity")
      }
      
      # Save plot to file
      if(SaveModelObjects) {
        ggplot2::ggsave(paste0(model_path,"/", ModelID,"_ROC_Plot.png"))
      }
      
      # Binary Evaluation Calibration Plot----
      EvaluationPlot <- EvalPlot(data = ValidationData,
                                 PredictionColName = "p1",
                                 TargetColName = "Target",
                                 GraphType = "calibration",
                                 PercentileBucket = 0.05,
                                 aggrfun = function(x) mean(x, na.rm = TRUE))
      
      # Add Number of Trees to Title
      EvaluationPlot <- EvaluationPlot +
        ggplot2::ggtitle(
          paste0("Calibration Evaluation Plot: AUC = ",
                 round(AUC_Metrics$auc,3)))
      
      # Save plot to file
      if(SaveModelObjects) {
        ggplot2::ggsave(paste0(model_path,"/", ModelID,"_EvaluationPlot.png"))
      }
      
      # Evaluation Metrics at Optimial Threshold----
      x <- ROCR::prediction(predictions = ValidationData[["p1"]],
                            labels = ValidationData[["Target"]])
      EvaluationMetrics <- data.table::data.table(Metric = c("AUC","TruePositiveRate","FalseNegativeRate",
                                                             "FalsePositiveRate","TrueNegativeRate",
                                                             "PreceisionRecallBreakEven","F1_Score","Odds"),
                                                  MetricValue = rep(999999,8),
                                                  Threshold   = rep(999999,8))
      i <- 0
      for(metric in c("auc","tpr","fnr","fpr","tnr","prbe","f","odds")) {
        i <- as.integer(i + 1)
        tryCatch({
          y <- ROCR::performance(prediction.obj = x, measure = metric)
          if(any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                 nrow(data.table::as.data.table(y@x.values)) <= 1)) {
            if(nrow(data.table::as.data.table(y@y.values)) <= 1 & nrow(data.table::as.data.table(y@x.values)) <= 1) {
              z <- data.table::as.data.table(cbind(Metric = y@y.values, Threshold = y@x.values))
              Metric <- z[[1]]
            } else if(nrow(data.table::as.data.table(y@y.values)) <= 1 & !(nrow(data.table::as.data.table(y@x.values) <= 1))) {
              z <- data.table::as.data.table(cbind(Metric = y@y.values, Threshold = y@x.values[[1]]))
              Metric <- z[!is.infinite(Threshold)][[1]]
            } else if(!(nrow(data.table::as.data.table(y@y.values)) <= 1) & nrow(data.table::as.data.table(y@x.values) <= 1)) {
              if(metric %chin% c("auc","tpr","tnr","prbe","f","odds")) {
                z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values))
                Metric <- z[order(-Metric)][!is.infinite(Metric)][[1]]
              } else {
                z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values))
                Metric <- z[order(Metric)][!is.infinite(Metric)][[1]]
              }
            }
          } else {
            if(metric %chin% c("auc","tpr","tnr","prbe","f","odds")) {
              z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values[[1]]))
              Metric <- z[order(-Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1,]
            } else {
              z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]], Threshold = y@x.values[[1]]))
              Metric <- z[order(Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1,]
            }
          }
          
          # Store Output Information
          if(any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                 nrow(data.table::as.data.table(y@x.values)) <= 1)) {
            data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric[[1]],4))
            data.table::set(EvaluationMetrics, i = i, j = 3L, value = NA)
          } else {
            data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric[[1]],4))
            data.table::set(EvaluationMetrics, i = i, j = 3L, value = Metric[[2]])
          }
        }, error = function(x) "skip")
      }
      
      # Binary Accuracy Threshold and Metric----
      j <- 0
      x <- data.table(Metric = "Accuracy", MetricValue = 5.0, Threshold = seq(0.01,0.99,0.001))
      for (i in unique(x[["Threshold"]])) {
        j = as.integer(j + 1)
        Accuracy <- mean(ValidationData[, ifelse(p1 > i & Target == 1 | p1 < i & Target == 0, 1, 0)])
        set(x, i = j, j = 2L, value = round(Accuracy,4))
      }
      data.table::setorderv(x, "MetricValue", order = -1, na.last = TRUE)
      x <- x[1,]
      EvaluationMetrics <- data.table::rbindlist(list(EvaluationMetrics,x))
      
      # Save EvaluationMetrics to File
      EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
      if(SaveModelObjects) {
        data.table::fwrite(EvaluationMetrics,
                           file = paste0(model_path,"/", ModelID,"_EvaluationMetrics.csv"))
      }
      
      # Binary Variable Importance----
      temp <- catboost::catboost.get_feature_importance(model)
      VariableImportance <- data.table::data.table(cbind(Variable = rownames(temp), temp))
      data.table::setnames(VariableImportance, "V2", "Importance")
      VariableImportance[, Importance := round(as.numeric(Importance),4)]
      VariableImportance <- VariableImportance[order(-Importance)]
      if(SaveModelObjects) {
        data.table::fwrite(VariableImportance, file = paste0(model_path,"/", ModelID,"_VariableImportance.csv"))
      }
      
      # Binary Partial Dependence----
      ParDepPlots <- list()
      j <- 0
      ParDepBoxPlots <- list()
      k <- 0
      for(i in seq_len(min(length(FeatureColNames),NumOfParDepPlots))) {
        tryCatch({
          Out <- ParDepCalPlots(
            data = ValidationData,
            PredictionColName = "p1",
            TargetColName = "Target",
            IndepVar = VariableImportance[i, Variable],
            GraphType = "calibration",
            PercentileBucket = 0.05,
            FactLevels = 10,
            Function = function(x) mean(x, na.rm = TRUE))
          
          j <- j + 1
          ParDepPlots[[paste0(VariableImportance[j, Variable])]] <- Out
        }, error = function(x) "skip")
      }
      
      # Binary Save ParDepPlots to file----
      if(SaveModelObjects) {
        save(ParDepPlots, file = paste0(model_path,"/", ModelID,"_ParDepPlots.R"))
      }
      
      # Binary Save GridCollect and catboostGridList----
      if(SaveModelObjects & GridTune == TRUE) {
        data.table::fwrite(catboostGridList, file = paste0(model_path,"/",ModelID, "_/catboostGridList.csv"))
        data.table::fwrite(GridCollect, file = paste0(model_path,"/",ModelID, "_/GridCollect.csv"))
      }
      
      # Binary Return Model Objects----
      if(GridTune) {
        if(ReturnModelObjects) {
          return(
            list(Model = model,
                 ValidationData = ValidationData,
                 ROC_Plot = ROC_Plot,
                 EvaluationPlot = EvaluationPlot,
                 EvaluationMetrics = EvaluationMetrics,
                 VariableImportance = VariableImportance,
                 PartialDependencePlots = ParDepPlots,
                 PartialDependenceBoxPlots = ParDepBoxPlots,
                 GridList = catboostGridList,
                 GridMetrics = GridCollect,
                 ColNames = Names))
        }
      } else {
        if(ReturnModelObjects) {
          return(
            list(Model = model,
                 ValidationData = ValidationData,
                 ROC_Plot = ROC_Plot,
                 EvaluationPlot = EvaluationPlot,
                 EvaluationMetrics = EvaluationMetrics,
                 VariableImportance = VariableImportance,
                 PartialDependencePlots = ParDepPlots,
                 PartialDependenceBoxPlots = ParDepBoxPlots,
                 ColNames = Names))
        }
      }
    }
  }
}




Correl <- 0.85
N <- 1000
data <- data.table::data.table(Target = runif(N))
data[, x1 := qnorm(Target)]
data[, x2 := runif(N)]
data[, Independent_Variable1 := log(pnorm(Correl * x1 +
                                            sqrt(1-Correl^2) * qnorm(x2)))]
data[, Independent_Variable2 := (pnorm(Correl * x1 +
                                         sqrt(1-Correl^2) * qnorm(x2)))]
data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
                                            sqrt(1-Correl^2) * qnorm(x2)))]
data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
                                                sqrt(1-Correl^2) * qnorm(x2))))]
data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
                                             sqrt(1-Correl^2) * qnorm(x2)))]
data[, Independent_Variable6 := (pnorm(Correl * x1 +
                                         sqrt(1-Correl^2) * qnorm(x2)))^0.10]
data[, Independent_Variable7 := (pnorm(Correl * x1 +
                                         sqrt(1-Correl^2) * qnorm(x2)))^0.25]
data[, Independent_Variable8 := (pnorm(Correl * x1 +
                                         sqrt(1-Correl^2) * qnorm(x2)))^0.75]
data[, Independent_Variable9 := (pnorm(Correl * x1 +
                                         sqrt(1-Correl^2) * qnorm(x2)))^2]
data[, Independent_Variable10 := (pnorm(Correl * x1 +
                                          sqrt(1-Correl^2) * qnorm(x2)))^4]
data[, Target := as.factor(
  ifelse(Independent_Variable2 < 0.20, "A",
         ifelse(Independent_Variable2 < 0.40, "B",
                ifelse(Independent_Variable2 < 0.6,  "C",
                       ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
data[, ':=' (x1 = NULL, x2 = NULL)]

data <- RemixAutoML::ModelDataPrep(data = data, Impute = TRUE, CharToFactor = TRUE, MissFactor = "0", MissNum = -1)
str(data)







data
TestData = NULL
TargetColumnName = "Target"
FeatureColNames = c(2:ncol(data))
CatFeatures = NULL
TrainSplitRatio = 0.80
task_type = "GPU"
eval_metric = "MultiClass"
Trees = 50
GridTune = FALSE
grid_eval_metric = "f"
MaxModelsInGrid = 10
model_path = getwd()
ModelID = "FirstModel"
NumOfParDepPlots = 3
ReturnModelObjects = TRUE
SaveModelObjects = FALSE