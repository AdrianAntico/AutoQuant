#' @title AutoXGBoostRegression
#'
#' @description AutoXGBoostRegression is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Regression
#'
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param ReturnFactorLevels Set to TRUE to have the factor levels returned with the other model objects
#' @param TransformNumericColumns Set to NULL to do nothing; otherwise supply the column names of numeric variables you want transformed
#' @param Methods Choose from "BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit", "YeoJohnson". Function will determine if one cannot be used because of the underlying data.
#' @param LossFunction Default is 'reg:squarederror'. Other options include 'reg:squaredlogerror', 'reg:pseudohubererror', 'count:poisson', 'survival:cox', 'survival:aft', 'aft_loss_distribution', 'reg:gamma', 'reg:tweedie'
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "r2", "RMSE", "MSE", "MAE"
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param grid_eval_metric Choose from "poisson","mae","mape","mse","msle","kl","cs","r2"
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param MaxModelsInGrid Number of models to test from grid options (243 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param Verbose Set to 0 if you want to suppress model evaluation updates in training
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param SaveInfoToPDF Set to TRUE to save model insights to pdf
#' @param PassInGrid Default is NULL. Provide a data.table of grid options from a previous run.
#' @param MaxRunsWithoutNewWinner Runs without new winner to end procedure
#' @param MaxRunMinutes In minutes
#' @param BaselineComparison Set to either "default" or "best". Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
#' @param Trees Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the trees numbers you want to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1000L, 10000L, 1000L)
#' @param eta Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param max_depth Bandit grid partitioned. Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param min_child_weight Number, or vector for min_child_weight to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param subsample Number, or vector for subsample to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(0.55, 1.0, 0.05)
#' @param colsample_bytree Number, or vector for colsample_bytree to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(0.55, 1.0, 0.05)
#' @examples
#' \dontrun{
#' # Create some dummy correlated data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' TestModel <- RemixAutoML::AutoXGBoostRegression(
#'
#'     # GPU or CPU
#'     TreeMethod = "hist",
#'     NThreads = parallel::detectCores(),
#'     LossFunction = 'reg:squarederror',
#'
#'     # Metadata args
#'     model_path = normalizePath("./"),
#'     metadata_path = NULL,
#'     ModelID = "Test_Model_1",
#'     ReturnFactorLevels = TRUE,
#'     ReturnModelObjects = TRUE,
#'     SaveModelObjects = FALSE,
#'     SaveInfoToPDF = FALSE,
#'
#'     # Data args
#'     data = data,
#'     TrainOnFull = FALSE,
#'     ValidationData = NULL,
#'     TestData = NULL,
#'     TargetColumnName = "Adrian",
#'     FeatureColNames = names(data)[!names(data) %in%
#'       c("IDcol_1", "IDcol_2","Adrian")],
#'     IDcols = c("IDcol_1","IDcol_2"),
#'     TransformNumericColumns = NULL,
#'     Methods = c("BoxCox", "Asinh", "Asin", "Log",
#'       "LogPlus1", "Sqrt", "Logit", "YeoJohnson"),
#'
#'     # Model evaluation args
#'     eval_metric = "rmse",
#'     NumOfParDepPlots = 3L,
#'
#'     # Grid tuning args
#'     PassInGrid = NULL,
#'     GridTune = FALSE,
#'     grid_eval_metric = "mse",
#'     BaselineComparison = "default",
#'     MaxModelsInGrid = 10L,
#'     MaxRunsWithoutNewWinner = 20L,
#'     MaxRunMinutes = 24L*60L,
#'     Verbose = 1L,
#'
#'     # ML args
#'     Trees = 50L,
#'     eta = 0.05,
#'     max_depth = 4L,
#'     min_child_weight = 1.0,
#'     subsample = 0.55,
#'     colsample_bytree = 0.55)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and GridList
#' @export
AutoXGBoostRegression <- function(data,
                                  TrainOnFull = FALSE,
                                  ValidationData = NULL,
                                  TestData = NULL,
                                  TargetColumnName = NULL,
                                  FeatureColNames = NULL,
                                  IDcols = NULL,
                                  model_path = NULL,
                                  metadata_path = NULL,
                                  SaveInfoToPDF = FALSE,
                                  ModelID = "FirstModel",
                                  ReturnFactorLevels = TRUE,
                                  ReturnModelObjects = TRUE,
                                  SaveModelObjects = FALSE,
                                  TransformNumericColumns = NULL,
                                  Methods = c("BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"),
                                  Verbose = 0L,
                                  NumOfParDepPlots = 3L,
                                  NThreads = parallel::detectCores(),
                                  LossFunction = 'reg:squarederror',
                                  eval_metric = "rmse",
                                  TreeMethod = "hist",
                                  GridTune = FALSE,
                                  grid_eval_metric = "rmse",
                                  BaselineComparison = "default",
                                  MaxModelsInGrid = 10L,
                                  MaxRunsWithoutNewWinner = 20L,
                                  MaxRunMinutes = 24L*60L,
                                  PassInGrid = NULL,
                                  Trees = 50L,
                                  eta = NULL,
                                  max_depth = NULL,
                                  min_child_weight = NULL,
                                  subsample = NULL,
                                  colsample_bytree = NULL) {

  # Check args ----
  XGBoostArgsCheck(GridTune.=GridTune, model_path.=model_path, metadata_path.=metadata_path, Trees.=Trees, max_depth.=max_depth, eta.=eta, min_child_weight.=min_child_weight, subsample.=subsample, colsample_bytree.=colsample_bytree)

  # Data prep ----
  Output <- XGBoostDataPrep(ModelType="regression", data.=data, ValidationData.=ValidationData, TestData.=TestData, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, IDcols.=IDcols, TransformNumericColumns.=TransformNumericColumns, Methods.=Methods, ModelID.=ModelID, model_path.=model_path, TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, ReturnFactorLevels.=ReturnFactorLevels)
  FactorLevelsList <- Output$FactorLevelsList; Output$FactorLevelsList <- NULL
  datavalidate <- Output$datavalidate; Output$datavalidate <- NULL
  TrainTarget <- Output$TrainTarget; Output$TrainTarget <- NULL
  TestTarget <- Output$FinalTestTarget; Output$FinalTestTarget <- NULL
  datatrain <- Output$datatrain; Output$datatrain <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
  TestMerge <- Output$TestMerge; Output$TestMerge <- NULL
  TestData <- Output$TestData.; Output$TestData. <- NULL
  datatest <- Output$datatest; Output$datatest <- NULL
  EvalSets <- Output$EvalSets; Output$EvalSets <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  IDcols <- Output$IDcols; Output$IDcols <- NULL
  Names <- Output$Names; rm(Output)

  # Regression Grid Tune or Not Check----
  if(GridTune & !TrainOnFull) {

    # Pull in Grid sets----
    Grids <- XGBoostParameterGrids(TaskType=TreeMethod,Shuffles=Shuffles,NTrees=Trees,Depth=max_depth,LearningRate=eta,MinChildWeight=min_child_weight,SubSample=subsample,ColSampleByTree=colsample_bytree)
    Grid <- Grids$Grid
    GridClusters <- Grids$Grids
    ExperimentalGrid <- Grids$ExperimentalGrid

    # Initialize RL----
    RL_Start <- RL_Initialize(ParameterGridSet = GridClusters, Alpha = 1L, Beta = 1L, SubDivisions = 1000L)
    BanditArmsN <- RL_Start[["BanditArmsN"]]
    Successes <- RL_Start[["Successes"]]
    Trials <- RL_Start[["Trials"]]
    GridIDs <- RL_Start[["GridIDs"]]
    BanditProbs <- RL_Start[["BanditProbs"]]
    RunsWithoutNewWinner <- 0L
    rm(RL_Start)

    # Add bandit probs columns to ExperimentalGrid----
    data.table::set(ExperimentalGrid, j = paste0("BanditProbs_", names(GridClusters)), value = -10)

    # Binary Grid Tuning Main Loop----
    counter <- 0L
    TotalRunTime <- 0
    repeat {

      # Increment counter----
      counter <- counter + 1L

      # Check if grid still has elements in it----
      if(!exists("NewGrid")) zz <- counter else zz <- NewGrid
      if(!is.null(GridClusters[[paste0("Grid_",max(1L,zz-1L))]][["Depth"]][1L])) {

        # Define parameters----
        if(!exists("NewGrid")) {
          base_params <- XGBoostRegressionParams(objective=LossFunction,counter=counter,BanditArmsN=BanditArmsN,eval_metric=eval_metric,task_type=TreeMethod,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        } else {
          base_params <- XGBoostRegressionParams(objective=LossFunction,NewGrid=NewGrid,counter=counter,BanditArmsN=BanditArmsN,eval_metric=eval_metric,task_type=TreeMethod,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        }

        # Run model----
        if(counter <= BanditArmsN + 1L) {
          if(counter == 1L) {
            nrounds <- max(Grid$NTrees)
            print(base_params)
            RunTime <- system.time(model <- model <- xgboost::xgb.train(params=base_params, data=datatrain, nrounds = nrounds, watchlist=EvalSets, verbose=Verbose))
          } else {
            nrounds <- GridClusters[[paste0("Grid_",counter-1L)]][["NTrees"]][1L]
            print(base_params)
            RunTime <- system.time(model <- model <- xgboost::xgb.train(params=base_params, data=datatrain, nrounds = nrounds, watchlist=EvalSets, verbose=Verbose))
          }
        } else {
          nrounds <- GridClusters[[paste0("Grid_",NewGrid)]][["NTrees"]][1L]
          print(base_params)
          RunTime <- system.time(model <- model <- xgboost::xgb.train(params=base_params, data=datatrain, nrounds = nrounds, watchlist=EvalSets, verbose=Verbose))
        }

        # Binary Grid Score Model----
        if(!is.null(TestData)) {
          predict <- stats::predict(model, datatest)
          calibEval <- data.table::as.data.table(cbind(Target = FinalTestTarget, p1 = predict))
          NewPerformance <- XGBoostRegressionMetrics(grid_eval_metric,MinVal,calibEval)
        } else {
          predict <- stats::predict(model, datavalidate)
          calibEval <- data.table::as.data.table(cbind(Target = TestTarget, p1 = predict))
          NewPerformance <- XGBoostRegressionMetrics(grid_eval_metric,MinVal,calibEval)
        }

        # Update Experimental Grid with Param values----
        if(!exists("NewGrid")) {
          GridNumber <- counter - 1L
          data.table::set(ExperimentalGrid, i = counter, j = "GridNumber", value = GridNumber)
        } else {
          data.table::set(ExperimentalGrid, i = counter, j = "GridNumber", value = NewGrid)
        }
        data.table::set(ExperimentalGrid, i = counter, j = "RunTime", value = RunTime[[3L]])
        data.table::set(ExperimentalGrid, i = counter, j = "EvalMetric", value = NewPerformance)
        data.table::set(ExperimentalGrid, i = counter, j = "TreesBuilt", value = model$niter)
        if(counter == 1L) {
          BestPerformance <- 1L
        } else {
          if(tolower(BaselineComparison) == "default") {
            BestPerformance <- ExperimentalGrid[RunNumber == 1L][["EvalMetric"]]
          } else {
            BestPerformance <- ExperimentalGrid[RunNumber < counter, max(EvalMetric, na.rm = TRUE)]
          }
        }

        # Performance measures----
        TotalRunTime <- ExperimentalGrid[RunTime != -1L, sum(RunTime, na.rm = TRUE)]
        if(tolower(grid_eval_metric) != "r2") {
          if(NewPerformance < BestPerformance) {
            RunsWithoutNewWinner <- 0L
          } else {
            RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
          }
        } else {
          if(NewPerformance > BestPerformance) {
            RunsWithoutNewWinner <- 0L
          } else {
            RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
          }
        }

        # Binary Remove Model and Collect Garbage----
        rm(model)
        gc()
      } else {
        counter <- counter -1L
      }

      # Update bandit probabilities and whatnot----
      RL_Update_Output <- RL_ML_Update(ModelType="regression", Iteration=counter, NewGrid.=NewGrid, NewPerformance.=NewPerformance, BestPerformance.=BestPerformance, Trials.=Trials, Successes.=Successes, GridIDs.=GridIDs, BanditArmsN.=BanditArmsN, RunsWithoutNewWinner.=RunsWithoutNewWinner, MaxRunsWithoutNewWinner.=MaxRunsWithoutNewWinner, MaxModelsInGrid.=MaxModelsInGrid, MaxRunMinutes.=MaxRunMinutes, TotalRunTime.=TotalRunTime, BanditProbs.=BanditProbs)
      BanditProbs <- RL_Update_Output[["BanditProbs"]]
      Trials <- RL_Update_Output[["Trials"]]
      Successes <- RL_Update_Output[["Successes"]]
      NewGrid <- RL_Update_Output[["NewGrid"]]

      # Continue or stop----
      if(RL_Update_Output$BreakLoop != "stay") break else print("still going")
      data.table::set(ExperimentalGrid, i = counter+1L, j = "GridNumber", value = NewGrid)
      data.table::set(ExperimentalGrid, i = counter+1L, j = "NTrees", value = GridClusters[[paste0("Grid_",NewGrid)]][["NTrees"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "Depth", value = GridClusters[[paste0("Grid_",NewGrid)]][["Depth"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "LearningRate", value = GridClusters[[paste0("Grid_",NewGrid)]][["LearningRate"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "MinChildWeight", value = GridClusters[[paste0("Grid_",NewGrid)]][["MinChildWeight"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "SubSample", value = GridClusters[[paste0("Grid_",NewGrid)]][["SubSample"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "ColSampleByTree", value = GridClusters[[paste0("Grid_",NewGrid)]][["ColSampleByTree"]][Trials[NewGrid]+1L])
      for(bandit in seq_len(length(BanditProbs))) data.table::set(ExperimentalGrid, i = counter+1L, j = paste0("BanditProbs_Grid_",bandit), value = BanditProbs[bandit])
    }

    # Remove unneeded rows----
    ExperimentalGrid <- ExperimentalGrid[RunTime != -1L]
    gc()
  }

  # Final Params ----
  Output <- XGBoostFinalParams(PassInGrid.=PassInGrid, BestGrid.=BestGrid, GridTune.=GridTune, LossFunction.=LossFunction, eval_metric.=eval_metric, NThreads.=NThreads, TreeMethod.=TreeMethod, Trees.=Trees)
  base_params <- Output$base_params
  NTrees <- Output$NTrees; rm(Output)

  # Build model ----
  model <- xgboost::xgb.train(params = base_params, data = datatrain, watchlist = EvalSets, nrounds = NTrees, Verbose = Verbose)

  # Regression Save Model----
  if(SaveModelObjects) {
    if(getwd() == model_path) {
      xgboost::xgb.save(model = model, fname = ModelID)
    } else {
      save(model, file = file.path(model_path, ModelID))
    }
  }

  # Regression Grid Score Model----
  if(!is.null(TestData)) {
    predict <- stats::predict(model, datatest)
  } else if(!is.null(ValidationData) & !TrainOnFull) {
    predict <- stats::predict(model, datavalidate)
  } else {
    predict <- stats::predict(model, datatrain)
  }

  # Regression Validation Data----
  if(!is.null(TestData)) {
    ValidationData <- data.table::as.data.table(cbind(TestMerge, Predict = predict))
  } else if(!is.null(ValidationData) & !TrainOnFull) {
    ValidationData <- data.table::as.data.table(cbind(Target = TestTarget, ValidMerge, Predict = predict))
    data.table::setnames(ValidationData, "Target", eval(TargetColumnName))
  } else {
    ValidationData <- data.table::as.data.table(cbind(Target = TrainTarget, TrainMerge, Predict = predict))
    data.table::setnames(ValidationData, "Target", eval(TargetColumnName))
  }

  # Inverse Transform----
  if(!is.null(TransformNumericColumns)) {

    # Append record for Predicted Column----
    if(GridTune) TransformationResults <- TransformationResults[ColumnName != "Predict"]
    TransformationResults <- data.table::rbindlist(list(
      TransformationResults,
      data.table::data.table(
        ColumnName = "Predict",
        MethodName = rep(TransformationResults[ColumnName == eval(TargetColumnName), MethodName], 1L),
        Lambda = rep(TransformationResults[ColumnName == eval(TargetColumnName), Lambda], 1L),
        NormalizedStatistics = rep(0, 1))))

    # If Actual target columnname == "Target" remove the duplicate version----
    if(length(unique(TransformationResults[["ColumnName"]])) != nrow(TransformationResults)) {
      temp <- TransformationResults[, .N, by = "ColumnName"][N != 1L][[1L]]
      temp1 <- which(names(ValidationData) == temp)[1L]
      ValidationData[, eval(names(data)[temp1]) := NULL]
      TransformationResults <- TransformationResults[, ID := 1L:.N][ID != which(TransformationResults[["ID"]] == temp1)][, ID := NULL]
    }

    # Transform Target and Predicted Value----
    ValidationData <- AutoTransformationScore(
      ScoringData = ValidationData,
      Type = "Inverse",
      FinalResults = TransformationResults,
      TransID = NULL,
      Path = NULL)
  }

  # Regression r2 via sqrt of correlation
  if(!TrainOnFull) r_squared <- (ValidationData[, stats::cor(get(TargetColumnName), Predict)]) ^ 2

  # Save Validation Data to File----
  if(SaveModelObjects) {
    if(!TrainOnFull) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(ValidationData, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ValidationData.csv")))
      } else {
        data.table::fwrite(ValidationData, file = file.path(normalizePath(model_path), paste0(ModelID, "_ValidationData.csv")))
      }
    } else {
      if(!is.null(metadata_path)) {
        data.table::fwrite(ValidationData, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_FullDataPredictions.csv")))
      } else {
        data.table::fwrite(ValidationData, file = file.path(normalizePath(model_path), paste0(ModelID, "_FullDataPredictions.csv")))
      }
    }
  }

  # Regression Evaluation Calibration Plot----
  if(!TrainOnFull) {

    # Regression Evaluation Metrics----
    EvaluationMetrics <- data.table::data.table(Metric = c("MAE","MAPE","RMSE","R2"), MetricValue = rep(999999, 8L))
    i <- 0L
    for(metric in c("mae", "mape", "rmse", "r2")) {
      i <- i + 1L
      tryCatch({
        if(tolower(metric) == "mae") {
          ValidationData[, Metric := abs(get(TargetColumnName) - Predict)]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == "mape") {
          ValidationData[, Metric := abs((get(TargetColumnName) - Predict) / (get(TargetColumnName) + 1))]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == "rmse") {
          ValidationData[, Metric := (get(TargetColumnName) - Predict) ^ 2L]
          Metric <- sqrt(ValidationData[, mean(Metric, na.rm = TRUE)])
        } else if(tolower(metric) == "r2") {
          ValidationData[, ':=' (Metric1 = (ValidationData[[eval(TargetColumnName)]] - dataTrain[, mean(get(TargetColumnName))]) ^ 2, Metric2 = (ValidationData[[eval(TargetColumnName)]] - Predict) ^ 2)]
          Metric <- 1 - ValidationData[, sum(Metric2, na.rm = TRUE)] / ValidationData[, sum(Metric1, na.rm = TRUE)]
        }
        data.table::set(EvaluationMetrics, i = i, j = 2L, value = Metric)
      }, error = function(x) "skip")
    }

    # Save EvaluationMetrics to File
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvaluationMetrics, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationMetrics.csv")))
      } else {
        data.table::fwrite(EvaluationMetrics, file = file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationMetrics.csv")))
      }
    }

    # Evaluation Plot----
    EvaluationPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "Predict",
      TargetColName = eval(TargetColumnName),
      GraphType = "calibration",
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))

    # Add Number of Trees to Title
    EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(EvaluationMetrics[Metric == "R2", MetricValue], 3L)))

    # Save plot to file
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationPlot.png")))
      } else {
        ggplot2::ggsave(file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationPlot.png")))
      }
    }

    # Regression Evaluation Calibration Plot----
    EvaluationBoxPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "Predict",
      TargetColName = eval(TargetColumnName),
      GraphType = "boxplot",
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))

    # Add Number of Trees to Title
    EvaluationBoxPlot <- EvaluationBoxPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(EvaluationMetrics[Metric == "R2", MetricValue], 3L)))

    # Save plot to file
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationBoxPlot.png")))
      } else {
        ggplot2::ggsave(file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationBoxPlot.png")))
      }
    }

    # Regression Variable Importance----
    VariableImportance <- tryCatch({
      xgboost::xgb.importance(model = model)},
      error = function(x) NULL)

    # Variable Importance Formatting----
    if(!is.null(VariableImportance)) {
      VariableImportance[, ':=' (Gain = round(Gain, 4), Cover = round(Cover, 4), Frequency = round(Frequency, 4L))]
      if(SaveModelObjects) {
        if(!is.null(metadata_path)) {
          data.table::fwrite(VariableImportance, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_VariableImportance.csv")))
        } else {
          data.table::fwrite(VariableImportance, file = file.path(normalizePath(model_path), paste0(ModelID, "_VariableImportance.csv")))
        }
      }

      # Regression Partial Dependence----
      ParDepPlots <- list()
      ParDepBoxPlots <- list()
      if(NumOfParDepPlots != 0L) {
        j <- 0L
        k <- 0L
        for(i in seq_len(min(length(FeatureColNames), NumOfParDepPlots, VariableImportance[,.N]))) {
          tryCatch({
            Out <- ParDepCalPlots(
              data = ValidationData,
              PredictionColName = "Predict",
              TargetColName = eval(TargetColumnName),
              IndepVar = VariableImportance[i, Feature],
              GraphType = "calibration",
              PercentileBucket = 0.05,
              FactLevels = 10L,
              Function = function(x) mean(x, na.rm = TRUE))
            j <- j + 1L
            ParDepPlots[[paste0(VariableImportance[j, Feature])]] <- Out
          }, error = function(x) "skip")
          tryCatch({
            Out1 <- ParDepCalPlots(
              data = ValidationData,
              PredictionColName = "Predict",
              TargetColName = eval(TargetColumnName),
              IndepVar = VariableImportance[i, Feature],
              GraphType = "boxplot",
              PercentileBucket = 0.05,
              FactLevels = 10L,
              Function = function(x) mean(x, na.rm = TRUE))
            k <- k + 1L
            ParDepBoxPlots[[paste0(VariableImportance[k, Feature])]] <- Out1
          }, error = function(x) "skip")
        }
      }
    } else {
      ParDepPlots <- NULL
      ParDepBoxPlots <- NULL
    }

    # Regression Save ParDepPlots to file----
    if(SaveModelObjects) {
      if(!is.null(ParDepPlots)) {
        if(!is.null(metadata_path)) {
          save(ParDepPlots, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ParDepPlots.R")))
        } else {
          save(ParDepPlots, file = file.path(normalizePath(model_path), paste0(ModelID, "_ParDepPlots.R")))
        }
      }
    }

    # Regression Save ParDepBoxPlots to file----
    if(SaveModelObjects) {
      if(!is.null(ParDepPlots)) {
        if(!is.null(metadata_path)) {
          save(ParDepBoxPlots, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ParDepBoxPlots.R")))
        } else {
          save(ParDepBoxPlots, file = file.path(normalizePath(model_path), paste0(ModelID, "_ParDepBoxPlots.R")))
        }
      }
    }

    # Regression Save GridCollect and GridList----
    if(SaveModelObjects & GridTune) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(ExperimentalGrid, file = file.path(normalizePath(metadata_path), paste0(ModelID, "ExperimentalGrid.csv")))
      } else {
        data.table::fwrite(ExperimentalGrid, file = file.path(normalizePath(model_path), paste0(ModelID, "ExperimentalGrid.csv")))
      }
    }

    # Regression Remove Extraneous Columns----
    ValidationData[, ':=' (Metric = NULL)]

    # Regression Formal Evaluation Table
    EvaluationMetrics[, MetricValue := round(MetricValue, 4L)]
  }

  # Subset Transformation Object----
  if(!is.null(TransformNumericColumns)) {
    if(TargetColumnName == "Target") {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict"))]
    } else {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict", "Target"))]
    }
  }

  # VI_Plot_Function----
  VI_Plot <- function(VI_Data, ColorHigh = "darkblue", ColorLow = "white") {
    ggplot2::ggplot(VI_Data[1L:min(10L,.N)], ggplot2::aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_gradient2(mid = ColorLow,high = ColorHigh) +
      ChartTheme(Size = 12L, AngleX = 0L, LegendPosition = "right") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Global Variable Importance") +
      ggplot2::xlab("Top Model Features") +
      ggplot2::ylab("Value") +
      ggplot2::theme(legend.position = "none")
  }

  # Save PDF of model information ----
  if(!TrainOnFull & SaveInfoToPDF) {
    EvalPlotList <- list(EvaluationPlot, EvaluationBoxPlot, if(!is.null(VariableImportance)) VI_Plot(VariableImportance) else NULL)
    ParDepList <- list(if(!is.null(ParDepPlots)) ParDepPlots else NULL, if(!is.null(ParDepBoxPlots)) ParDepBoxPlots else NULL)
    TableMetrics <- list(EvaluationMetrics, if(!is.null(VariableImportance)) VariableImportance else NULL)
    PrintToPDF(
      Path = if(!is.null(metadata_path)) metadata_path else if(!is.null(model_path)) model_path else getwd(),
      OutputName = "EvaluationPlots",
      Tables = FALSE,
      ObjectList = EvalPlotList,
      Title = "Model Evaluation Plots",
      Width = 12,Height = 7,Paper = "USr",BackgroundColor = "transparent",ForegroundColor = "black")
    PrintToPDF(
      Path = if(!is.null(metadata_path)) metadata_path else if(!is.null(model_path)) model_path else getwd(),
      OutputName = "PartialDependencePlots",
      Tables = FALSE,
      ObjectList = ParDepList,
      Title = "Partial Dependence Calibration Plots",
      Width = 12,Height = 7,Paper = "USr",BackgroundColor = "transparent",ForegroundColor = "black")
    PrintToPDF(
      Path = if(!is.null(metadata_path)) metadata_path else if(!is.null(model_path)) model_path else getwd(),
      OutputName = "Metrics_and_Importances",
      Tables = TRUE,
      MaxPages = 100,
      ObjectList = TableMetrics,
      Title = "Model Metrics and Variable Importances",
      Width = 12,Height = 7,Paper = "USr",BackgroundColor = "transparent",ForegroundColor = "black")
    while(grDevices::dev.cur() > 1) grDevices::dev.off()
  }

  # FactorLevelsList ----
  if(!exists("FactorLevelsList")) FactorLevelsList <- NULL

  # Regression Return Model Objects----
  if(GridTune & !TrainOnFull) {
    if(!is.null(TransformNumericColumns)) {
      if(ReturnModelObjects) {
        if(ReturnFactorLevels & !is.null(CatFeatures)) {
          return(list(Model = model, ValidationData = ValidationData, EvaluationPlot = EvaluationPlot, EvaluationBoxPlot = EvaluationBoxPlot, EvaluationMetrics = EvaluationMetrics,
                      VariableImportance = VariableImportance, VI_Plot = VI_Plot(VI_Data = VariableImportance), PartialDependencePlots = ParDepPlots, PartialDependenceBoxPlots = ParDepBoxPlots,
                      GridMetrics = ExperimentalGrid, ColNames = Names, TransformationResults = TransformationResults, FactorLevelsList = FactorLevelsList))
        } else {
          return(list(Model = model, ValidationData = ValidationData, EvaluationPlot = EvaluationPlot, EvaluationBoxPlot = EvaluationBoxPlot, EvaluationMetrics = EvaluationMetrics,
                      VariableImportance = VariableImportance, VI_Plot = VI_Plot(VI_Data = VariableImportance), PartialDependencePlots = ParDepPlots, PartialDependenceBoxPlots = ParDepBoxPlots,
                      GridMetrics = ExperimentalGrid, ColNames = Names, TransformationResults = TransformationResults))
        }
      }
    } else {
      if(ReturnFactorLevels & !is.null(CatFeatures)) {
        return(list(Model = model, ValidationData = ValidationData, EvaluationPlot = EvaluationPlot, EvaluationBoxPlot = EvaluationBoxPlot, EvaluationMetrics = EvaluationMetrics,
                    VariableImportance = VariableImportance, VI_Plot = VI_Plot(VI_Data = VariableImportance), PartialDependencePlots = ParDepPlots, PartialDependenceBoxPlots = ParDepBoxPlots,
                    GridMetrics = ExperimentalGrid, ColNames = Names, FactorLevelsList = FactorLevelsList))
      } else {
        return(list(Model = model, ValidationData = ValidationData, EvaluationPlot = EvaluationPlot, EvaluationBoxPlot = EvaluationBoxPlot, EvaluationMetrics = EvaluationMetrics,
                    VariableImportance = VariableImportance, VI_Plot = VI_Plot(VI_Data = VariableImportance), PartialDependencePlots = ParDepPlots,
                    PartialDependenceBoxPlots = ParDepBoxPlots, GridMetrics = ExperimentalGrid, ColNames = Names))
      }
    }
  } else {
    if(!TrainOnFull) {
      if(!is.null(TransformNumericColumns)) {
        if(ReturnModelObjects) {
          if(ReturnFactorLevels & !is.null(CatFeatures)) {
            return(list(Model = model, ValidationData = ValidationData, EvaluationPlot = EvaluationPlot, EvaluationBoxPlot = EvaluationBoxPlot,EvaluationMetrics = EvaluationMetrics,
                        VariableImportance = VariableImportance, VI_Plot = VI_Plot(VI_Data = VariableImportance), PartialDependencePlots = ParDepPlots,
                        PartialDependenceBoxPlots = ParDepBoxPlots, ColNames = Names, TransformationResults = TransformationResults, FactorLevelsList = FactorLevelsList))
          } else {
            return(list(Model = model, ValidationData = ValidationData, EvaluationPlot = EvaluationPlot, EvaluationBoxPlot = EvaluationBoxPlot,
                        EvaluationMetrics = EvaluationMetrics, VariableImportance = VariableImportance, VI_Plot = VI_Plot(VI_Data = VariableImportance),
                        PartialDependencePlots = ParDepPlots, PartialDependenceBoxPlots = ParDepBoxPlots, ColNames = Names, TransformationResults = TransformationResults))
          }
        }
      } else {
        if(ReturnFactorLevels & !is.null(CatFeatures)) {
          return(list(Model = model, ValidationData = ValidationData, EvaluationPlot = EvaluationPlot, EvaluationBoxPlot = EvaluationBoxPlot,
                      EvaluationMetrics = EvaluationMetrics, VariableImportance = VariableImportance, VI_Plot = VI_Plot(VI_Data = VariableImportance),
                      PartialDependencePlots = ParDepPlots, PartialDependenceBoxPlots = ParDepBoxPlots, ColNames = Names, FactorLevelsList = FactorLevelsList))
        } else {
          return(list(Model = model, ValidationData = ValidationData, EvaluationPlot = EvaluationPlot, EvaluationBoxPlot = EvaluationBoxPlot,
                      EvaluationMetrics = EvaluationMetrics, VariableImportance = VariableImportance, VI_Plot = VI_Plot(VI_Data = VariableImportance),
                      PartialDependencePlots = ParDepPlots, PartialDependenceBoxPlots = ParDepBoxPlots, ColNames = Names))

        }
      }
    } else {
      if(!is.null(TransformNumericColumns)) {
        if(ReturnModelObjects) {
          if(ReturnFactorLevels & !is.null(CatFeatures)) {
            return(list(Model = model, ValidationData = ValidationData, ColNames = Names, TransformationResults = TransformationResults, FactorLevelsList = FactorLevelsList))
          } else {
            return(list(Model = model, ValidationData = ValidationData, ColNames = Names, TransformationResults = TransformationResults))
          }
        }
      } else {
        if(ReturnFactorLevels & !is.null(CatFeatures)) {
          return(list(Model = model, ValidationData = ValidationData, ColNames = Names, FactorLevelsList = FactorLevelsList))
        } else {
          return(list(Model = model, ValidationData = ValidationData, ColNames = Names))
        }
      }
    }
  }
}
