#' @title AutoCatBoostClassifier
#'
#' @description AutoCatBoostClassifier is an automated modeling function that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train, validation, and test sets (if not supplied). Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions (on test data), an ROC plot, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Binary Classification
#'
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data and skip over evaluation steps
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located, but not mixed types. Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located, but not mixed types. Also, not zero-indexed.
#' @param PrimaryDateColumn Supply a date or datetime column for catboost to utilize time as its basis for handling categorical features, instead of random shuffling
#' @param ClassWeights Supply a vector of weights for your target classes. E.g. c(0.25, 1) to weight your 0 class by 0.25 and your 1 class by 1.
#' @param CostMatrixWeights A vector with 4 elements c(True Positive Cost, False Negative Cost, False Positive Cost, True Negative Cost). Default c(1,0,0,1),
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param task_type Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param NumGPUs Numeric. If you have 4 GPUs supply 4 as a value.
#' @param EvalMetric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "AUC" is the default. 'Logloss', 'CrossEntropy', 'Precision', 'Recall', 'F1', 'BalancedAccuracy', 'BalancedErrorRate', 'MCC', 'Accuracy', 'CtrFactor', 'AUC', 'BrierScore', 'HingeLoss', 'HammingLoss', 'ZeroOneLoss', 'Kappa', 'WKappa', 'LogLikelihoodOfPrediction', 'TotalF1', 'PairLogit', 'PairLogitPairwise', 'PairAccuracy', 'QueryCrossEntropy', 'QuerySoftMax', 'PFound', 'NDCG', 'AverageGain', 'PrecisionAt', 'RecallAt', 'MAP'
#' @param LossFunction Default is NULL. Select the loss function of choice. c("MultiRMSE", 'Logloss','CrossEntropy','Lq','PairLogit','PairLogitPairwise','YetiRank','YetiRankPairwise','QueryCrossEntropy','QuerySoftMax')
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param SaveInfoToPDF Set to TRUE to save modeling information to PDF. If model_path or metadata_path aren't defined then output will be saved to the working directory
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects. E.g. plots and evaluation metrics
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param PassInGrid Defaults to NULL. Pass in a single row of grid from a previous output as a data.table (they are collected as data.tables)
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options.
#' @param MaxRunsWithoutNewWinner A number
#' @param MaxRunMinutes In minutes
#' @param BaselineComparison Set to either "default" or "best". Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
#' @param MetricPeriods Number of trees to build before evaluating intermediate metrics. Default is 10L
#' @param langevin TRUE or FALSE. TRUE enables
#' @param diffusion_temperature Default value is 10000
#' @param Trees Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the trees numbers you want to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1000L, 10000L, 1000L)
#' @param Depth Bandit grid partitioned Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param LearningRate Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param L2_Leaf_Reg Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the L2_Leaf_Reg values to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param RandomStrength A multiplier of randomness added to split evaluations. Default value is 1 which adds no randomness.
#' @param BorderCount Number of splits for numerical features. Catboost defaults to 254 for CPU and 128 for GPU
#' @param RSM CPU only. Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the RSM values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.80, 0.85, 0.90, 0.95, 1.0)
#' @param BootStrapType Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the BootStrapType values to test. For running grid tuning, a NULL value supplied will mean these values are tested c("Bayesian", "Bernoulli", "Poisson", "MVS", "No")
#' @param GrowPolicy Random testing. NULL, character, or vector for GrowPolicy to test. For grid tuning, supply a vector of values. For running grid tuning, a NULL value supplied will mean these values are tested c("SymmetricTree", "Depthwise", "Lossguide")
#' @param model_size_reg Defaults to 0.5. Set to 0 to allow for bigger models. This is for models with high cardinality categorical features. Valuues greater than 0 will shrink the model and quality will decline but models won't be huge.
#' @param feature_border_type Defaults to "GreedyLogSum". Other options include: Median, Uniform, UniformAndQuantiles, MaxLogSum, MinEntropy
#' @param sampling_unit Default is Group. Other option is Object. if GPU is selected, this will be turned off unless the LossFunction is YetiRankPairWise
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
#'   Classification = TRUE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' TestModel <- RemixAutoML::AutoCatBoostClassifier(
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
#'   ReturnModelObjects = TRUE,
#'   SaveInfoToPDF = FALSE,
#'
#'   # Data args
#'   data = data,
#'   TrainOnFull = FALSE,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   TargetColumnName = "Adrian",
#'   FeatureColNames = names(data)[!names(data) %in%
#'      c("IDcol_1","IDcol_2","Adrian")],
#'   PrimaryDateColumn = NULL,
#'   ClassWeights = c(1L,1L),
#'   IDcols = c("IDcol_1","IDcol_2"),
#'
#'   # Evaluation args
#'   CostMatrixWeights = c(1,0,0,1),
#'   EvalMetric = "AUC",
#'   LossFunction = "Logloss",
#'   MetricPeriods = 10L,
#'   NumOfParDepPlots = ncol(data)-1L-2L,
#'
#'   # Grid tuning args
#'   PassInGrid = NULL,
#'   GridTune = FALSE,
#'   MaxModelsInGrid = 30L,
#'   MaxRunsWithoutNewWinner = 20L,
#'   MaxRunMinutes = 24L*60L,
#'   BaselineComparison = "default",
#'
#'   # ML args
#'   Trees = 1000,
#'   Depth = 9,
#'   LearningRate = NULL,
#'   L2_Leaf_Reg = NULL,
#'   RandomStrength = 1,
#'   BorderCount = 128,
#'   RSM = 1,
#'   BootStrapType = "Bayesian",
#'   GrowPolicy = "SymmetricTree",
#'   langevin = FALSE,
#'   diffusion_temperature = 10000,
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
#' TestModel$ROC_Plot
#' TestModel$EvaluationPlot
#' TestModel$EvaluationMetrics
#' TestModel$VariableImportance
#' TestModel$InteractionImportance
#' TestModel$ShapValuesDT
#' TestModel$VI_Plot
#' TestModel$PartialDependencePlots
#' TestModel$GridMetrics
#' TestModel$ColNames
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model (the model), ValidationData.csv, ROC_Plot.png, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoCatBoostClassifier <- function(data,
                                   TrainOnFull = FALSE,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   ClassWeights = c(1,1),
                                   CostMatrixWeights = c(1,0,0,1),
                                   IDcols = NULL,
                                   task_type = "GPU",
                                   NumGPUs = 1,
                                   model_path = NULL,
                                   metadata_path = NULL,
                                   SaveInfoToPDF = FALSE,
                                   ModelID = "FirstModel",
                                   NumOfParDepPlots = 0L,
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE,
                                   PassInGrid = NULL,
                                   GridTune = FALSE,
                                   MaxModelsInGrid = 30L,
                                   MaxRunsWithoutNewWinner = 20L,
                                   MaxRunMinutes = 24L*60L,
                                   BaselineComparison = "default",
                                   MetricPeriods = 10L,
                                   EvalMetric = "MCC",
                                   LossFunction = NULL,
                                   langevin = FALSE,
                                   diffusion_temperature = 10000,
                                   Trees = 50L,
                                   Depth = 6,
                                   LearningRate = NULL,
                                   L2_Leaf_Reg = 3,
                                   RandomStrength = 1,
                                   BorderCount = 128,
                                   RSM = NULL,
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
  Output <- CatBoostArgsCheck(ModelType="classifier", DummifyCols.=FALSE, PrimaryDateColumn.=PrimaryDateColumn, GridTune.=GridTune, model_path.=model_path, metadata_path.=metadata_path, ClassWeights.=ClassWeights, LossFunction.=LossFunction, loss_function.=NULL, loss_function_value.=NULL, eval_metric.=NULL, eval_metric_value.=NULL, task_type.=task_type, NumGPUs.=NumGPUs, MaxModelsInGrid.=MaxModelsInGrid, NumOfParDepPlots.=NumOfParDepPlots,ReturnModelObjects.=ReturnModelObjects, SaveModelObjects.=SaveModelObjects, PassInGrid.=PassInGrid, MetricPeriods.=MetricPeriods, langevin.=langevin, diffusion_temperature.=diffusion_temperature, Trees.=Trees, Depth.=Depth, LearningRate.=LearningRate, L2_Leaf_Reg.=L2_Leaf_Reg,RandomStrength.=RandomStrength, BorderCount.=BorderCount, RSM.=RSM, BootStrapType.=BootStrapType, GrowPolicy.=GrowPolicy, model_size_reg.=model_size_reg, feature_border_type.=feature_border_type, sampling_unit.=sampling_unit, subsample.=subsample, score_function.=score_function, min_data_in_leaf.=min_data_in_leaf)
  score_function <- Output$score_function
  BootStrapType <- Output$BootStrapType
  sampling_unit <- Output$sampling_unit
  LossFunction <- Output$LossFunction
  GrowPolicy <- Output$GrowPolicy
  task_type <- Output$task_type
  GridTune <- Output$GridTune
  HasTime <- Output$HasTime
  NumGPUs <- Output$NumGPUs
  Depth <- Output$Depth
  RSM <- Output$RSM; rm(Output)

  # Data Prep (model data prep, dummify, create sets) ----
  if(DebugMode) print("Running CatBoostDataPrep()")
  Output <- CatBoostDataPrep(ModelType="classification", data.=data, ValidationData.=ValidationData, TestData.=TestData, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, PrimaryDateColumn.=PrimaryDateColumn,IDcols.=IDcols,TrainOnFull.=TrainOnFull, SaveModelObjects.=SaveModelObjects, TransformNumericColumns.=NULL, Methods.=NULL, model_path.=model_path, ModelID.=ModelID, DummifyCols.=FALSE, LossFunction.=NULL, EvalMetric.=NULL)
  FinalTestTarget <- Output$FinalTestTarget; Output$FinalTestTarget <- NULL
  UseBestModel <- Output$UseBestModel; Output$UseBestModel <- NULL
  TrainTarget <- Output$TrainTarget; Output$TrainTarget <- NULL
  CatFeatures <- Output$CatFeatures; Output$CatFeatures <- NULL
  TestTarget <- Output$TestTarget; Output$TestTarget <- NULL
  dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
  TestMerge <- Output$TestMerge; Output$TestMerge <- NULL
  dataTest <- Output$dataTest; Output$dataTest <- NULL
  TestData <- Output$TestData; Output$TestData <- NULL
  Names <- Output$Names; rm(Output)

  # Create catboost data objects ----
  if(DebugMode) print("Running CatBoostDataConversion()")
  Output <- CatBoostDataConversion(CatFeatures.=CatFeatures, dataTrain.=dataTrain, dataTest.=dataTest, TestData.=TestData, TrainTarget.=TrainTarget, TestTarget.=TestTarget, FinalTestTarget.=FinalTestTarget, TrainOnFull.=TrainOnFull)
  TrainPool <- Output$TrainPool; Output$TrainPool <- NULL
  TestPool <- Output$TestPool; Output$TestPool <- NULL
  FinalTestPool <- Output$FinalTestPool; rm(Output)

  # Binary Grid Tune or Not Check ----
  if(GridTune && !TrainOnFull) {
    if(DebugMode) print("Running Grid Tuning")

    # Pull in Grid sets ----
    Grids <- CatBoostParameterGrids(TaskType = task_type, Shuffles = 1, NTrees = Trees, Depth = Depth, LearningRate = LearningRate, L2_Leaf_Reg = L2_Leaf_Reg, BorderCount = BorderCount, RandomStrength = RandomStrength, RSM = RSM, BootStrapType = BootStrapType, GrowPolicy = GrowPolicy)
    Grid <- Grids$Grid
    GridClusters <- Grids$Grids
    ExperimentalGrid <- Grids$ExperimentalGrid
    rm(Grids)

    # Initialize RL ----
    RL_Start <- RL_Initialize(ParameterGridSet = GridClusters, Alpha = 1L, Beta = 1L, SubDivisions = 1000L)
    BanditArmsN <- RL_Start$BanditArmsN
    Successes <- RL_Start$Successes
    Trials <- RL_Start$Trials
    GridIDs <- RL_Start$GridIDs
    BanditProbs <- RL_Start$BanditProbs
    RunsWithoutNewWinner <- 0L
    rm(RL_Start)

    # Add bandit probs columns to ExperimentalGrid ----
    data.table::set(ExperimentalGrid, j = paste0("BanditProbs_", names(GridClusters)), value = -10)

    # Binary Grid Tuning Main Loop----
    counter <- 0L
    NewGrid <- 1L
    repeat {

      # Increment counter ----
      counter <- counter + 1L

      # Print iteration so people can see it on their screen ----
      for(i in seq_len(20)) print(paste0("You are on iteration number: ", counter))

      # Check if grid still has elements in it----
      if(!is.null(GridClusters[[paste0("Grid_",max(1L, NewGrid-1L))]][["L2_Leaf_Reg"]][1L])) {

        # Define parameters ----
        if(!exists("NewGrid")) {
          base_params <- CatBoostClassifierParams(NumGPUs=NumGPUs,LossFunction=LossFunction,counter=counter,BanditArmsN=BanditArmsN,HasTime=HasTime,MetricPeriods=MetricPeriods,ClassWeights=ClassWeights,eval_metric=EvalMetric,task_type=task_type,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        } else {
          base_params <- CatBoostClassifierParams(NumGPUs=NumGPUs,LossFunction=LossFunction,NewGrid=NewGrid,counter=counter,BanditArmsN=BanditArmsN,HasTime=HasTime,MetricPeriods=MetricPeriods,ClassWeights=ClassWeights,eval_metric=EvalMetric,task_type=task_type,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        }

        # Build model----
        RunTime <- system.time(model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params))

        # Score model----
        if(!is.null(TestData)) {
          predict <- catboost::catboost.predict(model = model, pool = FinalTestPool, prediction_type = "Probability", thread_count = parallel::detectCores())
          calibEval <- data.table::as.data.table(cbind(Target = FinalTestTarget, p1 = predict))
          AUC_Metrics <- pROC::roc(response = calibEval[["Target"]], predictor = calibEval[["p1"]], na.rm = TRUE, algorithm = 3L, auc = TRUE, ci = TRUE)
        } else {
          predict <- catboost::catboost.predict(model = model,pool = TestPool, prediction_type = "Probability", thread_count = parallel::detectCores())
          calibEval <- data.table::as.data.table(cbind(Target = TestTarget, p1 = predict))
          AUC_Metrics <- pROC::roc(response = calibEval[["Target"]], predictor = calibEval[["p1"]], na.rm = TRUE, algorithm = 3L, auc = TRUE, ci = TRUE)
        }

        # Update Experimental Grid with Param values----
        if(!exists("NewGrid")) {
          GridNumber <- counter - 1L
          data.table::set(ExperimentalGrid, i = counter, j = "GridNumber", value = GridNumber)
        } else {
          data.table::set(ExperimentalGrid, i = counter, j = "GridNumber", value = NewGrid)
        }
        NewPerformance <- as.numeric(AUC_Metrics$auc)
        data.table::set(ExperimentalGrid, i = counter, j = "RunTime", value = RunTime[[3L]])
        data.table::set(ExperimentalGrid, i = counter, j = "eval_metric", value = NewPerformance)
        data.table::set(ExperimentalGrid, i = counter, j = "TreesBuilt", value = model$tree_count)
        if(counter == 1L) {
          BestPerformance <- 1L
        } else {
          if(tolower(BaselineComparison) == "default") {
            BestPerformance <- ExperimentalGrid[RunNumber == 1L][["eval_metric"]]
          } else {
            BestPerformance <- max(ExperimentalGrid[RunNumber < counter][["eval_metric"]], na.rm = TRUE)
          }
        }

        # Performance measures----
        TotalRunTime <- ExperimentalGrid[RunTime != -1L, sum(RunTime, na.rm = TRUE)]
        if(NewPerformance > BestPerformance) {
          RunsWithoutNewWinner <- 0L
        } else {
          RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
        }

        # Binary Remove Model and Collect Garbage ----
        rm(model); gc()
      }

      # Update bandit probabilities and whatnot ----
      RL_Update_Output <- RL_ML_Update(
        ExperimentGrid = ExperimentalGrid,
        ModelRun = counter,
        ModelType = "classification",
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
        TotalRunTime = TotalRunTime,
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
      if(tolower(task_type) == "gpu") data.table::set(ExperimentalGrid, i = counter+1L, j = "GrowPolicy", value = GridClusters[[paste0("Grid_",NewGrid)]][["GrowPolicy"]][Trials[NewGrid]+1L])
      for(bandit in seq_len(length(BanditProbs))) data.table::set(ExperimentalGrid, i = counter+1L, j = paste0("BanditProbs_Grid_",bandit), value = BanditProbs[bandit])
    }

    # Remove unneeded rows ----
    ExperimentalGrid <- ExperimentalGrid[RunTime != -1L]
    BestGrid <- ExperimentalGrid.[order(-eval_metric)][1L]

    # Binary Save ExperimentalGrid ----
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
  base_params <- CatBoostFinalParams(ModelType="classification", UseBestModel.=UseBestModel, ClassWeights.=ClassWeights, PassInGrid.=PassInGrid, BestGrid.=BestGrid, ExperimentalGrid.=ExperimentalGrid, GridTune.=GridTune, TrainOnFull.=TrainOnFull, MetricPeriods.=MetricPeriods, LossFunction.=LossFunction, EvalMetric.=EvalMetric, score_function.=score_function, HasTime.=HasTime, task_type.=task_type, NumGPUs.=NumGPUs, NTrees.=Trees, Depth.=Depth, LearningRate.=LearningRate, L2_Leaf_Reg.=L2_Leaf_Reg, langevin.=langevin, diffusion_temperature.=diffusion_temperature, sampling_unit.=sampling_unit, RandomStrength.=RandomStrength, BorderCount.=BorderCount, RSM.=RSM, GrowPolicy.=GrowPolicy, BootStrapType.=BootStrapType, model_size_reg.=model_size_reg, feature_border_type.=feature_border_type, subsample.=subsample, min_data_in_leaf.=min_data_in_leaf)

  # Binary Train Final Model ----
  if(DebugMode) print("Running catboost.train")
  if(!TrainOnFull) {
    model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params)
  } else {
    model <- catboost::catboost.train(learn_pool = TrainPool, params = base_params)
  }

  # Binary Save Model ----
  if(DebugMode) print("Running catboost.save_model")
  if(SaveModelObjects) catboost::catboost.save_model(model = model, model_path = file.path(model_path, ModelID))

  # Binary Score Final Test Data ----
  if(DebugMode) print("Running catboost.predict")
  predict <- catboost::catboost.predict(model = model, pool = if(!is.null(TestData)) FinalTestPool else if(TrainOnFull) TrainPool else TestPool, prediction_type = "Probability", thread_count = parallel::detectCores())

  # Binary Validation Data (generate validation data, back transform, save to file) ----
  if(DebugMode) print("Running CatBoostValidationData()")
  ValidationData <- CatBoostValidationData(ModelType="classification", TrainOnFull.=TrainOnFull, TestDataCheck=!is.null(TestData), FinalTestTarget.=FinalTestTarget, TestTarget.=TestTarget, TrainTarget.=TrainTarget, TestMerge.=TestMerge, dataTest.=dataTest, data.=data, predict.=predict, TargetColumnName.=TargetColumnName, SaveModelObjects. = SaveModelObjects, metadata_path.=metadata_path, model_path.=model_path, ModelID.=ModelID, LossFunction.=NULL, TransformNumericColumns.=NULL, GridTune.=GridTune, TransformationResults.=NULL, TargetLevels.=NULL)

  # Gather importance and shap values ----
  if(DebugMode) print("Running CatBoostImportances()")
  Output <- CatBoostImportances(ModelType="classification", TargetColumnName.=TargetColumnName, BestGrid.=BestGrid, TrainOnFull.=TrainOnFull, TrainPool.=TrainPool, TestPool.=TestPool, FinalTestPool.=FinalTestPool, TestDataCheck=!is.null(TestData), ValidationData.=ValidationData, FeatureColNames.=FeatureColNames, GridTune.=GridTune, task_type.=task_type, SaveModelObjects.=SaveModelObjects, model.=model, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path, GrowPolicy.=GrowPolicy)
  VariableImportance <- Output$VariableImportance; Output$VariableImportance <- NULL
  Interaction <- Output$Interaction; Output$Interaction <- NULL
  ShapValues <- Output$ShapValues; Output$ShapValues <- NULL; rm(Output)

  # Generate EvaluationMetrics ----
  if(DebugMode) print("Running BinaryMetrics()")
  EvalMetrics <- BinaryMetrics(MLModels="catboost", ClassWeights.=ClassWeights, SaveModelObjects.=SaveModelObjects, ValidationData.=ValidationData, TrainOnFull.=TrainOnFull, TargetColumnName.=TargetColumnName, ModelID.=ModelID, model_path.=model_path, metadata_path.=metadata_path)

  # Classification evaluation plots ----
  if(DebugMode) print("Running CatBoostEvalPlots()")
  Output <- CatBoostEvalPlots(ModelType="classification", TrainOnFull.=TrainOnFull, ValidationData.=ValidationData, NumOfParDepPlots.=NumOfParDepPlots, VariableImportance.=VariableImportance, TargetColumnName.=TargetColumnName, FeatureColNames.=FeatureColNames, SaveModelObjects.=SaveModelObjects, ModelID.=ModelID, metadata_path.=metadata_path, model_path.=model_path, LossFunction.=NULL, EvalMetric.=NULL, EvaluationMetrics.=NULL, predict.=NULL)
  EvaluationPlot <- Output$EvaluationPlot; Output$EvaluationPlot <- NULL
  ParDepPlots <- Output$ParDepPlots; Output$ParDepPlots <- NULL
  ROC_Plot <- Output$ROC_Plot; rm(Output)

  # Remove extenal files if GridTune is TRUE ----
  if(DebugMode) print("Running CatBoostRemoveFiles()")
  CatBoostRemoveFiles(GridTune. = GridTune)

  # Send output to pdf ----
  if(DebugMode) print("Running CatBoostPDF()")
  CatBoostPDF(ModelType="classification", TrainOnFull.=TrainOnFull, SaveInfoToPDF.=SaveInfoToPDF, EvaluationPlot.=EvaluationPlot, EvaluationBoxPlot.=NULL, VariableImportance.=VariableImportance, ParDepPlots.=ParDepPlots, ParDepBoxPlots.=NULL, EvalMetrics.=EvalMetrics, Interaction.=Interaction, model_path.=model_path, metadata_path.=metadata_path)

  # Final Garbage Collection ----
  if(tolower(task_type) == "gpu") gc()

  # Binary Return Model Objects ----
  if(DebugMode) print("Return Model Objects")
  if(TrainOnFull && ReturnModelObjects) {
    return(list(
      Model = model,
      VariableImportance = if(!is.null(VariableImportance)) VariableImportance else NULL,
      InteractionImportance = if(!is.null(Interaction)) Interaction else NULL,
      ShapValuesDT = if(!is.null(VariableImportance)) ShapValues else NULL,
      VI_Plot = if(!is.null(VariableImportance)) tryCatch({if(all(c("plotly","dplyr") %chin% installed.packages())) plotly::ggplotly(VI_Plot(Type = "catboost", VariableImportance)) else VI_Plot(Type = "catboost", VariableImportance)}, error = NULL) else NULL,
      ColNames = Names))
  } else if(ReturnModelObjects) {
    return(list(
      Model = model,
      ValidationData = ValidationData,
      ROC_Plot = ROC_Plot,
      EvaluationPlot = EvaluationPlot,
      EvaluationMetrics = EvalMetrics,
      VariableImportance = if(!is.null(VariableImportance)) VariableImportance else NULL,
      InteractionImportance = if(!is.null(Interaction)) Interaction else NULL,
      ShapValuesDT = if(!is.null(VariableImportance)) ShapValues else NULL,
      VI_Plot = if(!is.null(VariableImportance)) tryCatch({if(all(c("plotly","dplyr") %chin% installed.packages())) plotly::ggplotly(VI_Plot(Type = "catboost", VariableImportance)) else VI_Plot(Type = "catboost", VariableImportance)}, error = NULL) else NULL,
      PartialDependencePlots = if(!is.null(ParDepPlots)) ParDepPlots else NULL,
      GridMetrics = if(!is.null(ExperimentalGrid)) data.table::setorderv(ExperimentalGrid, cols = "eval_metric", order = -1L, na.last = TRUE) else NULL,
      ColNames = Names))
  }
}
