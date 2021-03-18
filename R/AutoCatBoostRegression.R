#' @title AutoCatBoostRegression
#'
#' @description AutoCatBoostRegression is an automated modeling function that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Regression
#'
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data and skip over evaluation steps
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param Weights Weights vector for train.pool in catboost
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param PrimaryDateColumn Supply a date or datetime column for catboost to utilize time as its basis for handling categorical features, instead of random shuffling
#' @param DummifyCols Logical. Will coerce to TRUE if loss_function or eval_metric is set to 'MultiRMSE'.
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param TransformNumericColumns Set to NULL to do nothing; otherwise supply the column names of numeric variables you want transformed
#' @param Methods Choose from "YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", or "Logit". If more than one is selected, the one with the best normalization pearson statistic will be used. Identity is automatically selected and compared.
#' @param task_type Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param NumGPUs Set to 1, 2, 3, etc.
#' @param eval_metric Select from 'RMSE', 'MAE', 'MAPE', 'R2', 'Poisson', 'MedianAbsoluteError', 'SMAPE', 'MSLE', 'NumErrors', 'FairLoss', 'Tweedie', 'Huber', 'LogLinQuantile', 'Quantile', 'Lq', 'Expectile', 'MultiRMSE'
#' @param eval_metric_value Used with the specified eval_metric. See https://catboost.ai/docs/concepts/loss-functions-regression.html
#' @param loss_function Used in model training for model fitting. 'MAPE', 'MAE', 'RMSE', 'Poisson', 'Tweedie', 'Huber', 'LogLinQuantile', 'Quantile', 'Lq', 'Expectile', 'MultiRMSE'
#' @param loss_function_value Used with the specified loss function if an associated value is required. 'Tweedie', 'Huber', 'LogLinQuantile', 'Quantile' 'Lq', 'Expectile'. See https://catboost.ai/docs/concepts/loss-functions-regression.html
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param SaveInfoToPDF Set to TRUE to save modeling information to PDF. If model_path or metadata_path aren't defined then output will be saved to the working directory
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param EvalPlots Defaults to TRUE. Set to FALSE to not generate and return these objects.
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param PassInGrid Defaults to NULL. Pass in a single row of grid from a previous output as a data.table (they are collected as data.tables)
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param BaselineComparison Set to either "default" or "best". Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
#' @param MaxModelsInGrid Number of models to test from grid options
#' @param MaxRunMinutes Maximum number of minutes to let this run
#' @param MaxRunsWithoutNewWinner Number of models built before calling it quits
#' @param MetricPeriods Number of periods to use between Catboost evaluations
#' @param Shuffles Number of times to randomize grid possibilities
#' @param langevin Set to TRUE to enable
#' @param diffusion_temperature Defaults to 10000
#' @param Trees Standard + Grid Tuning. Bandit grid partitioned. The maximum number of trees you want in your models
#' @param Depth Standard + Grid Tuning. Bandit grid partitioned. Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param L2_Leaf_Reg Standard + Grid Tuning. Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the L2_Leaf_Reg values to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param RandomStrength Standard + Grid Tuning. A multiplier of randomness added to split evaluations. Default value is 1 which adds no randomness.
#' @param BorderCount Standard + Grid Tuning. Number of splits for numerical features. Catboost defaults to 254 for CPU and 128 for GPU
#' @param LearningRate Standard + Grid Tuning. Default varies if RMSE, MultiClass, or Logloss is utilized. Otherwise default is 0.03. Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param RSM CPU only. Standard + Grid Tuning. If GPU is set, this is turned off. Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the RSM values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.80, 0.85, 0.90, 0.95, 1.0)
#' @param BootStrapType Standard + Grid Tuning. NULL value to default to catboost default (Bayesian for GPU and MVS for CPU). Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the BootStrapType values to test. For running grid tuning, a NULL value supplied will mean these values are tested c("Bayesian", "Bernoulli", "Poisson", "MVS", "No")
#' @param GrowPolicy Standard + Grid Tuning. Catboost default of SymmetricTree. Random testing. Default "SymmetricTree", character, or vector for GrowPolicy to test. For grid tuning, supply a vector of values. For running grid tuning, a NULL value supplied will mean these values are tested c("SymmetricTree", "Depthwise", "Lossguide")
#' @param model_size_reg Defaults to 0.5. Set to 0 to allow for bigger models. This is for models with high cardinality categorical features. Valuues greater than 0 will shrink the model and quality will decline but models won't be huge.
#' @param feature_border_type Defaults to "GreedyLogSum". Other options include: Median, Uniform, UniformAndQuantiles, MaxLogSum, MinEntropy
#' @param sampling_unit Default is Group. Other option is Object. if GPU is selected, this will be turned off unless the loss_function is YetiRankPairWise
#' @param subsample Default is NULL. Catboost will turn this into 0.66 for BootStrapTypes Poisson and Bernoulli. 0.80 for MVS. Doesn't apply to others.
#' @param score_function Default is Cosine. CPU options are Cosine and L2. GPU options are Cosine, L2, NewtonL2, and NewtomCosine (not available for Lossguide)
#' @param min_data_in_leaf Default is 1. Cannot be used with SymmetricTree is GrowPolicy
#' @param DebugMode Set to TRUE to get a printout of which step the function is on. FALSE, otherwise
#' @examples
#' \dontrun{
#' # Create some dummy correlated data
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 10000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' TestModel <- RemixAutoML::AutoCatBoostRegression(
#'
#'   # GPU or CPU and the number of available GPUs
#'   task_type = "GPU",
#'   NumGPUs = 1,
#'
#'   # Metadata args
#'   ModelID = "Test_Model_1",
#'   model_path = normalizePath("./"),
#'   metadata_path = normalizePath("./"),
#'   SaveModelObjects = FALSE,
#'   SaveInfoToPDF = FALSE,
#'   ReturnModelObjects = TRUE,
#'
#'   # Data args
#'   data = data,
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   Weights = NULL,
#'   TargetColumnName = "Adrian",
#'   FeatureColNames = names(data)[!names(data) %in%
#'     c("IDcol_1", "IDcol_2","Adrian")],
#'   PrimaryDateColumn = NULL,
#'   DummifyCols = FALSE,
#'   IDcols = c("IDcol_1","IDcol_2"),
#'   TransformNumericColumns = "Adrian",
#'   Methods = c("BoxCox", "Asinh", "Asin", "Log",
#'     "LogPlus1", "Sqrt", "Logit"),
#'
#'   # Model evaluation
#'   eval_metric = "RMSE",
#'   eval_metric_value = 1.5,
#'   loss_function = "RMSE",
#'   loss_function_value = 1.5,
#'   MetricPeriods = 10L,
#'   NumOfParDepPlots = ncol(data)-1L-2L,
#'   EvalPlots = TRUE,
#'
#'   # Grid tuning args
#'   PassInGrid = NULL,
#'   GridTune = FALSE,
#'   MaxModelsInGrid = 30L,
#'   MaxRunsWithoutNewWinner = 20L,
#'   MaxRunMinutes = 60*60,
#'   Shuffles = 4L,
#'   BaselineComparison = "default",
#'
#'   # ML args
#'   langevin = FALSE,
#'   diffusion_temperature = 10000,
#'   Trees = 1000,
#'   Depth = 9,
#'   L2_Leaf_Reg = NULL,
#'   RandomStrength = 1,
#'   BorderCount = 128,
#'   LearningRate = NULL,
#'   RSM = 1,
#'   BootStrapType = NULL,
#'   GrowPolicy = "SymmetricTree",
#'   model_size_reg = 0.5,
#'   feature_border_type = "GreedyLogSum",
#'   sampling_unit = "Object",
#'   subsample = NULL,
#'   score_function = "Cosine",
#'   min_data_in_leaf = 1,
#'   DebugMode = FALSE)
#'
#' # Output
#' TestModel$Model
#' TestModel$ValidationData
#' TestModel$EvaluationPlot
#' TestModel$EvaluationBoxPlot
#' TestModel$EvaluationMetrics
#' TestModel$VariableImportance
#' TestModel$InteractionImportance
#' TestModel$ShapValuesDT
#' TestModel$VI_Plot
#' TestModel$PartialDependencePlots
#' TestModel$PartialDependenceBoxPlots
#' TestModel$GridList
#' TestModel$ColNames
#' TestModel$TransformationResults
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, catboostgrid, and a transformation details file.
#' @export
AutoCatBoostRegression <- function(data,
                                   TrainOnFull = FALSE,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   Weights = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   DummifyCols = FALSE,
                                   IDcols = NULL,
                                   TransformNumericColumns = NULL,
                                   Methods = c("BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"),
                                   task_type = "GPU",
                                   NumGPUs = 1,
                                   eval_metric = "RMSE",
                                   eval_metric_value = 1.5,
                                   loss_function = "RMSE",
                                   loss_function_value = 1.5,
                                   model_path = NULL,
                                   metadata_path = NULL,
                                   SaveInfoToPDF = FALSE,
                                   ModelID = "FirstModel",
                                   NumOfParDepPlots = 0L,
                                   EvalPlots = TRUE,
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE,
                                   PassInGrid = NULL,
                                   GridTune = FALSE,
                                   MaxModelsInGrid = 30L,
                                   MaxRunsWithoutNewWinner = 20L,
                                   MaxRunMinutes = 24L*60L,
                                   Shuffles = 1L,
                                   BaselineComparison = "default",
                                   MetricPeriods = 10L,
                                   langevin = FALSE,
                                   diffusion_temperature = 10000,
                                   Trees = 500L,
                                   Depth = 9,
                                   L2_Leaf_Reg = 3.0,
                                   RandomStrength = 1,
                                   BorderCount = 254,
                                   LearningRate = NULL,
                                   RSM = 1,
                                   BootStrapType = NULL,
                                   GrowPolicy = "SymmetricTree",
                                   model_size_reg = 0.5,
                                   feature_border_type = "GreedyLogSum",
                                   sampling_unit = "Object",
                                   subsample = NULL,
                                   score_function = "Cosine",
                                   min_data_in_leaf = 1,
                                   DebugMode = FALSE) {
  # Load catboost ----
  loadNamespace(package = "catboost")

  # Args Checking (ensure args are set consistently) ----
  if(DebugMode) print("Running CatBoostArgsCheck()")
  Output <- CatBoostArgsCheck(ModelType=if(loss_function == "MultiRMSE") "vector" else "regression", DummifyCols.=DummifyCols, data.=data, FeatureColNames.=FeatureColNames, PrimaryDateColumn.=PrimaryDateColumn, GridTune.=GridTune, model_path.=model_path, metadata_path.=metadata_path, ClassWeights.=NULL, LossFunction.=NULL, loss_function.=loss_function, loss_function_value.=loss_function_value, eval_metric.=eval_metric, eval_metric_value.=eval_metric_value, task_type.=task_type, NumGPUs.=NumGPUs, MaxModelsInGrid.=MaxModelsInGrid, NumOfParDepPlots.=NumOfParDepPlots,ReturnModelObjects.=ReturnModelObjects, SaveModelObjects.=SaveModelObjects, PassInGrid.=PassInGrid, MetricPeriods.=MetricPeriods, langevin.=langevin, diffusion_temperature.=diffusion_temperature, Trees.=Trees, Depth.=Depth, LearningRate.=LearningRate, L2_Leaf_Reg.=L2_Leaf_Reg,RandomStrength.=RandomStrength, BorderCount.=BorderCount, RSM.=RSM, BootStrapType.=BootStrapType, GrowPolicy.=GrowPolicy, model_size_reg.=model_size_reg, feature_border_type.=feature_border_type, sampling_unit.=sampling_unit, subsample.=subsample, score_function.=score_function, min_data_in_leaf.=min_data_in_leaf)
  score_function <- Output$score_function
  BootStrapType <- Output$BootStrapType
  sampling_unit <- Output$sampling_unit
  LossFunction <- Output$LossFunction
  GrowPolicy <- Output$GrowPolicy
  EvalMetric <- Output$EvalMetric
  task_type <- Output$task_type
  GridTune <- Output$GridTune
  HasTime <- Output$HasTime
  NumGPUs <- Output$NumGPUs
  Depth <- Output$Depth
  RSM <- Output$RSM; rm(Output)

  # Data Prep (model data prep, dummify, create sets) ----
  if(DebugMode) print("Running CatBoostDataPrep()")
  Output <- CatBoostDataPrep(ModelType="regression", data.=data, ValidationData.=ValidationData, TestData.=TestData, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, PrimaryDateColumn.=PrimaryDateColumn,IDcols.=IDcols,TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, TransformNumericColumns.=TransformNumericColumns, Methods.=Methods, model_path.=model_path, ModelID.=ModelID, DummifyCols.=DummifyCols, LossFunction.=LossFunction, EvalMetric.=EvalMetric)
  TransformationResults <- Output$TransformationResults; Output$TransformationResults <- NULL
  FactorLevelsList <- Output$FactorLevelsList
  FinalTestTarget <- Output$FinalTestTarget; Output$FinalTestTarget <- NULL
  UseBestModel <- Output$UseBestModel; Output$UseBestModel <- NULL
  TrainTarget <- Output$TrainTarget; Output$TrainTarget <- NULL
  CatFeatures <- Output$CatFeatures; Output$CatFeatures <- NULL
  TestTarget <- Output$TestTarget; Output$TestTarget <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
  TestMerge <- Output$TestMerge; Output$TestMerge <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  TestData <- Output$TestData; Output$TestData <- NULL
  Names <- Output$Names; Output$Names <- NULL; rm(Output)

  # Create catboost data objects ----
  if(DebugMode) print("Running CatBoostDataConversion()")
  Output <- CatBoostDataConversion(CatFeatures.=CatFeatures, dataTrain.=dataTrain, dataTest.=dataTest, TestData.=TestData, TrainTarget.=TrainTarget, TestTarget.=TestTarget, FinalTestTarget.=FinalTestTarget, TrainOnFull.=TrainOnFull)
  FinalTestPool <- Output$FinalTestPool; Output$FinalTestPool <- NULL
  TrainPool <- Output$TrainPool; Output$TrainPool <- NULL
  TestPool <- Output$TestPool; Output$TestPool <- NULL; rm(Output)

  # Regression Grid Tune or Not Check ----
  if(GridTune & !TrainOnFull) {
    if(DebugMode) print("Running Grid Tuning")

    # Pull in Grid sets----
    Grids <- CatBoostParameterGrids(
      TaskType       = task_type,
      Shuffles       = Shuffles,
      NTrees         = Trees,
      Depth          = Depth,
      LearningRate   = LearningRate,
      L2_Leaf_Reg    = L2_Leaf_Reg,
      RandomStrength = RandomStrength,
      BorderCount    = BorderCount,
      RSM            = RSM,
      BootStrapType  = BootStrapType,
      GrowPolicy     = GrowPolicy)
    Grid <- Grids$Grid
    GridClusters <- Grids$Grids
    ExperimentalGrid <- Grids$ExperimentalGrid

    # Initialize RL----
    RL_Start <- RL_Initialize(
      ParameterGridSet = GridClusters,
      Alpha = 1L,
      Beta = 1L,
      SubDivisions = 1000L)
    BanditArmsN <- RL_Start[["BanditArmsN"]]
    Successes <- RL_Start[["Successes"]]
    Trials <- RL_Start[["Trials"]]
    GridIDs <- RL_Start[["GridIDs"]]
    BanditProbs <- RL_Start[["BanditProbs"]]
    RunsWithoutNewWinner <- 0L
    rm(RL_Start)

    # Add bandit probs columns to ExperimentalGrid----
    data.table::set(ExperimentalGrid, j = paste0("BanditProbs_", names(GridClusters)), value = -10)

    # Regression Grid Tuning Main Loop----
    counter <- 0L
    NewGrid <- 1L
    repeat {

      # Increment counter----
      counter <- counter + 1L

      # Check if there are any grid elements left in the specific grid----
      if(!is.null(GridClusters[[paste0("Grid_", max(1L, NewGrid))]][["BootStrapType"]][1L])) {

        # Define prameters----
        if(!exists("NewGrid")) {
          base_params <- CatBoostRegressionParams(LossFunction=LossFunction,NumGPUs=NumGPUs,BanditArmsN=BanditArmsN,counter=counter,HasTime=HasTime,MetricPeriods=MetricPeriods,eval_metric=EvalMetric,task_type=task_type,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        } else {
          base_params <- CatBoostRegressionParams(LossFunction=LossFunction,NumGPUs=NumGPUs,BanditArmsN=BanditArmsN,counter=counter,HasTime=HasTime,MetricPeriods=MetricPeriods,eval_metric=EvalMetric,task_type=task_type,model_path=model_path,NewGrid=NewGrid,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        }

        # Build model----
        RunTime <- system.time(model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params))

        # Score and measure model----
        if(!is.null(TestData)) {
          predict <- catboost::catboost.predict(model = model, pool = FinalTestPool, prediction_type = "RawFormulaVal", thread_count = parallel::detectCores())
          calibEval <- data.table::as.data.table(cbind(Target = FinalTestTarget, p1 = predict))
          calibEval[, Metric := (Target - p1) ^ 2L]
          NewPerformance <- calibEval[, mean(Metric, na.rm = TRUE)]
        } else {
          predict <- catboost::catboost.predict(model = model,pool = TestPool, prediction_type = "RawFormulaVal", thread_count = parallel::detectCores())
          calibEval <- data.table::as.data.table(cbind(Target = TestTarget, p1 = predict))
          calibEval[, Metric := (Target - p1) ^ 2L]
          NewPerformance <- calibEval[, mean(Metric, na.rm = TRUE)]
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
        data.table::set(ExperimentalGrid, i = counter, j = "TreesBuilt", value = model$tree_count)
        if(counter == 1L) {
          BestPerformance <- 1L
        } else {
          if(tolower(BaselineComparison) == "default") {
            BestPerformance <- ExperimentalGrid[RunNumber == 1L][["EvalMetric"]]
          } else {
            BestPerformance <- ExperimentalGrid[RunNumber < counter, min(EvalMetric, na.rm = TRUE)]
          }
        }

        # Performance measures----
        if(NewPerformance < BestPerformance) {
          RunsWithoutNewWinner <- 0L
        } else {
          RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
        }

        # Regression Remove Model and Collect Garbage----
        rm(model)
        gc()
      } else {
        counter <- counter -1L
      }

      # Update bandit probabilities and whatnot----
      RL_Update_Output <- RL_ML_Update(
        ExperimentGrid = ExperimentalGrid,
        ModelRun = counter,
        ModelType = "regression",
        NEWGrid = NewGrid,
        NewPerformance = NewPerformance,
        BestPerformance = BestPerformance,
        TrialVector = Trials,
        SuccessVector = Successes,
        GridIDS = GridIDs,
        BanditArmsCount = BanditArmsN,
        RunsWithoutNewWinner = RunsWithoutNewWinner,
        MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
        MaxNumberModels = MaxModelsInGrid,
        MaxRunMinutes = MaxRunMinutes,
        TotalRunTime = ExperimentalGrid[RunTime != -1L][, sum(RunTime, na.rm = TRUE)],
        BanditProbabilities = BanditProbs)
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
      data.table::set(ExperimentalGrid, i = counter+1L, j = "L2_Leaf_Reg", value = GridClusters[[paste0("Grid_",NewGrid)]][["L2_Leaf_Reg"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "RandomStrength", value = GridClusters[[paste0("Grid_",NewGrid)]][["RandomStrength"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "BorderCount", value = GridClusters[[paste0("Grid_",NewGrid)]][["BorderCount"]][Trials[NewGrid]+1L])
      if(!tolower(task_type) == "gpu") data.table::set(ExperimentalGrid, i = counter+1L, j = "RSM", value = GridClusters[[paste0("Grid_",NewGrid)]][["RSM"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "BootStrapType", value = GridClusters[[paste0("Grid_",NewGrid)]][["BootStrapType"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "GrowPolicy", value = GridClusters[[paste0("Grid_",NewGrid)]][["GrowPolicy"]][Trials[NewGrid]+1L])
      for(bandit in seq_len(length(BanditProbs))) data.table::set(ExperimentalGrid, i = counter + 1L, j = paste0("BanditProbs_Grid_", bandit), value = BanditProbs[bandit])
      print(counter)
    }

    # Remove unneeded rows----
    ExperimentalGrid <- ExperimentalGrid[RunTime != -1L]

    # Regression Save ExperimentalGrid ----
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(ExperimentalGrid, file = file.path(metadata_path, paste0(ModelID, "_ExperimentalGrid.csv")))
      } else {
        data.table::fwrite(ExperimentalGrid, file = file.path(model_path, paste0(ModelID, "_ExperimentalGrid.csv")))
      }
    }
  } else {
    ExperimentalGrid <- NULL
    BestGrid <- NULL
  }

  # Final Parameters (put parameters in list to pass into catboost) ----
  if(DebugMode) print("Running CatBoostFinalParams()")
  base_params <- CatBoostFinalParams(ModelType="regression", UseBestModel.=UseBestModel, ClassWeights.=NULL, PassInGrid. = PassInGrid, BestGrid.=BestGrid, ExperimentalGrid. = ExperimentalGrid, GridTune.=GridTune, TrainOnFull.=TrainOnFull, MetricPeriods.=MetricPeriods, LossFunction.=LossFunction, EvalMetric.=EvalMetric, score_function.=score_function, HasTime.=HasTime, task_type.=task_type, NumGPUs.=NumGPUs, NTrees.=Trees, Depth.=Depth, LearningRate.=LearningRate, L2_Leaf_Reg.=L2_Leaf_Reg, langevin.=langevin, diffusion_temperature.=diffusion_temperature, sampling_unit.=sampling_unit, RandomStrength.=RandomStrength, BorderCount.=BorderCount, RSM.=RSM, GrowPolicy.=GrowPolicy, BootStrapType.=BootStrapType, model_size_reg.=model_size_reg, feature_border_type.=feature_border_type, subsample.=subsample, min_data_in_leaf.=min_data_in_leaf)

  # Regression Train Final Model ----
  if(DebugMode) print("Running catboost.train")
  if(TrainOnFull) {
    model <- catboost::catboost.train(learn_pool = TrainPool, params = base_params)
  } else {
    model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params)
  }

  # Regression Save Model ----
  if(DebugMode) print("Running catboost.save_model")
  if(SaveModelObjects) catboost::catboost.save_model(model = model, model_path = file.path(model_path, ModelID))

  # Regression Score Final Test Data ----
  if(DebugMode) print("Running catboost.predict")
  predict <- catboost::catboost.predict(model = model, pool = if(!is.null(TestData)) FinalTestPool else if(TrainOnFull) TrainPool else TestPool, prediction_type = "RawFormulaVal", thread_count = parallel::detectCores())

  # Regression Validation Data (generate validation data, back transform, save to file) ----
  if(DebugMode) print("Running CatBoostValidationData()")
  ValidationData <- CatBoostValidationData(ModelType="regression", TrainOnFull.=TrainOnFull, TestDataCheck=!is.null(TestData), FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, TrainTarget.=TrainTarget, TestMerge.=TestMerge, dataTest.=dataTest, data.=data, predict.=predict, TargetColumnName.=TargetColumnName, SaveModelObjects. = SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, LossFunction.=LossFunction, TransformNumericColumns. = TransformNumericColumns, GridTune. = GridTune, TransformationResults. = TransformationResults, TargetLevels.=NULL)

  # Gather importance and shap values ----
  if(DebugMode) print("Running CatBoostImportances()")
  Output <- CatBoostImportances(ModelType="regression", TargetColumnName.=TargetColumnName, BestGrid.=BestGrid, TrainOnFull.=TrainOnFull, TrainPool.=TrainPool, TestPool.=TestPool, FinalTestPool.=FinalTestPool, TestDataCheck=!is.null(TestData), ValidationData.=ValidationData, FeatureColNames.=FeatureColNames, GridTune.=GridTune, task_type.=task_type, SaveModelObjects.=SaveModelObjects, model.=model, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, GrowPolicy.=GrowPolicy)
  Interaction <- Output$Interaction; Output$Interaction <- NULL
  VariableImportance <- Output$VariableImportance; Output$VariableImportance <- NULL
  ShapValues <- Output$ShapValues; Output$ShapValues <- NULL; rm(Output)

  # Regression Metrics ----
  if(DebugMode) print("Running RegressionMetrics()")
  EvaluationMetrics <- RegressionMetrics(SaveModelObjects.=SaveModelObjects, data.=data, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, LossFunction.=LossFunction, EvalMetric.=EvalMetric, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)

  # Regression Plots ----
  if(DebugMode) print("Running CatBoostEvalPlots()")
  Output <- CatBoostEvalPlots(ModelType="regression", TrainOnFull.=TrainOnFull, LossFunction.=LossFunction, EvalMetric.=EvalMetric, EvaluationMetrics.=EvaluationMetrics, ValidationData.=ValidationData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, predict.=NULL)
  EvaluationBoxPlot <- Output$EvaluationBoxPlot; Output$EvaluationBoxPlot <- NULL
  EvaluationPlot <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
  ParDepBoxPlots <- Output$ParDepBoxPlots; Output$ParDepBoxPlots <- NULL
  ParDepPlots <- Output$ParDepPlots; rm(Output)

  # Subset Transformation Object ----
  if(!is.null(TransformNumericColumns) && !((!is.null(LossFunction) && LossFunction == "MultiRMSE") || (!is.null(EvalMetric) && EvalMetric == "MultiRMSE"))) {
    if(TargetColumnName == "Target") {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict"))]
    } else {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict", "Target"))]
    }
  }

  # Remove extenal files if GridTune is TRUE ----
  if(DebugMode) print("Running CatBoostRemoveFiles()")
  CatBoostRemoveFiles(GridTune. = GridTune)

  # Send output to pdf ----
  if(DebugMode) print("Running CatBoostPDF()")
  CatBoostPDF(ModelType="regression", TrainOnFull.=TrainOnFull, SaveInfoToPDF.=SaveInfoToPDF, EvaluationPlot.=EvaluationPlot, EvaluationBoxPlot.=EvaluationBoxPlot, VariableImportance.=VariableImportance, ParDepPlots.=ParDepPlots, ParDepBoxPlots.=ParDepBoxPlots, EvalMetrics.=EvaluationMetrics, Interaction.=Interaction, model_path.=model_path, metadata_path.=metadata_path)

  # Final Garbage Collection ----
  if(tolower(task_type) == "gpu") gc()

  # Regression Return Model Objects ----
  if(DebugMode) print("Return Model Objects")
  if(!TrainOnFull) {
    if(ReturnModelObjects) {
      return(list(
        Model = model,
        ValidationData = ValidationData,
        EvaluationPlot = if(!is.null(EvaluationPlot) && !is.list(EvaluationPlot)) {if(all(c("plotly","dplyr") %chin% installed.packages())) plotly::ggplotly(EvaluationPlot) else EvaluationPlot} else NULL,
        EvaluationBoxPlot = if(!is.null(EvaluationBoxPlot)) EvaluationBoxPlot else NULL,
        EvaluationMetrics = EvaluationMetrics,
        VariableImportance = if(!is.null(VariableImportance)) VariableImportance else NULL,
        InteractionImportance = if(!is.null(Interaction)) Interaction else NULL,
        VI_Plot = if(!is.null(VariableImportance)) tryCatch({if(all(c("plotly","dplyr") %chin% installed.packages())) plotly::ggplotly(VI_Plot(Type = "catboost", VI_Data = VariableImportance)) else VI_Plot(Type = "catboost", VI_Data = VariableImportance)}, error = NULL) else NULL,
        PartialDependencePlots = if(!is.null(ParDepPlots) && !is.list(ParDepPlots)) {if(all(c("plotly","dplyr") %chin% installed.packages())) plotly::ggplotly(ParDepPlots) else ParDepPlots} else NULL,
        PartialDependenceBoxPlots = if(!is.null(VariableImportance)) ParDepBoxPlots else NULL,
        GridList = if(!is.null(ExperimentalGrid)) data.table::setorderv(ExperimentalGrid, cols = "EvalMetric", order = 1L, na.last = TRUE) else NULL,
        ColNames = Names,
        TransformationResults = if(exists("TransformationResults")) TransformationResults else NULL,
        FactorLevelsList = FactorLevelsList))
    }
  } else if(ReturnModelObjects) {
    return(list(Model = model, data = ValidationData, ColNames = Names, TransformationResults = if(exists("TransformationResults")) TransformationResults else NULL,FactorLevelsList = FactorLevelsList))
  }
}
