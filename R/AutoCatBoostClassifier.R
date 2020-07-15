#' AutoCatBoostClassifier is an automated catboost model grid-tuning classifier and evaluation system
#'
#' AutoCatBoostClassifier is an automated modeling function that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train, validation, and test sets (if not supplied). Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions (on test data), an ROC plot, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
#' @author Adrian Antico
#' @family Automated Supervised Learning - Binary Classification
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data and skip over evaluation steps
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located, but not mixed types. Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located, but not mixed types. Also, not zero-indexed.
#' @param PrimaryDateColumn Supply a date or datetime column for catboost to utilize time as its basis for handling categorical features, instead of random shuffling
#' @param ClassWeights Supply a vector of weights for your target classes. E.g. c(0.25, 1) to weight your 0 class by 0.25 and your 1 class by 1.
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param task_type Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param NumGPUs Numeric. If you have 4 GPUs supply 4 as a value.
#' @param eval_metric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "AUC" is the default. 'Logloss', 'CrossEntropy', 'Precision', 'Recall', 'F1', 'BalancedAccuracy', 'BalancedErrorRate', 'MCC', 'Accuracy', 'CtrFactor', 'AUC', 'BrierScore', 'HingeLoss', 'HammingLoss', 'ZeroOneLoss', 'Kappa', 'WKappa', 'LogLikelihoodOfPrediction', 'TotalF1', 'PairLogit', 'PairLogitPairwise', 'PairAccuracy', 'QueryCrossEntropy', 'QuerySoftMax', 'PFound', 'NDCG', 'AverageGain', 'PrecisionAt', 'RecallAt', 'MAP'
#' @param loss_function Default is NULL. Select the loss function of choice. c("MultiRMSE", 'Logloss','CrossEntropy','Lq','PairLogit','PairLogitPairwise','YetiRank','YetiRankPairwise','QueryCrossEntropy','QuerySoftMax')
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects. E.g. plots and evaluation metrics
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param PassInGrid Defaults to NULL. Pass in a single row of grid from a previous output as a data.table (they are collected as data.tables)
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options.
#' @param MaxRunsWithoutNewWinner A number
#' @param MaxRunMinutes In minutes
#' @param Shuffles Numeric. List a number to let the program know how many times you want to shuffle the grids for grid tuning
#' @param BaselineComparison Set to either "default" or "best". Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
#' @param MetricPeriods Number of trees to build before evaluating intermediate metrics. Default is 10L
#' @param Trees Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the trees numbers you want to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1000L, 10000L, 1000L)
#' @param Depth Bandit grid partitioned Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param LearningRate Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param L2_Leaf_Reg Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the L2_Leaf_Reg values to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param RSM CPU only. Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the RSM values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.80, 0.85, 0.90, 0.95, 1.0)
#' @param BootStrapType Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the BootStrapType values to test. For running grid tuning, a NULL value supplied will mean these values are tested c("Bayesian", "Bernoulli", "Poisson", "MVS", "No")
#' @param GrowPolicy Random testing. NULL, character, or vector for GrowPolicy to test. For grid tuning, supply a vector of values. For running grid tuning, a NULL value supplied will mean these values are tested c("SymmetricTree", "Depthwise", "Lossguide")
#' @examples
#' \donttest{
#' # Create some dummy correlated data with numeric and categorical features
#' data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = TRUE, MultiClass = FALSE)
#' 
#' # Run function
#' TestModel <- RemixAutoML::AutoCatBoostClassifier(
#'     
#'     # GPU or CPU and the number of available GPUs
#'     task_type = "GPU",
#'     NumGPUs = 1,
#'     
#'     # Metadata arguments: 
#'     #   'ModelID' is used to create part of the file names generated when saving to file'
#'     #   'model_path' is where the minimal model objects for scoring will be stored
#'     #      'ModelID' will be the name of the saved model object
#'     #   'metadata_path' is where model evaluation and model interpretation files are saved
#'     #      objects saved to model_path if metadata_path is null
#'     #      Saved objects include: 
#'     #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
#'     #         'ModelID_ROC_Plot.png' and 'Model_ID_EvaluationPlot.png' calibration plot
#'     #         'ModelID_VariableImportance.csv' is the variable importance. 
#'     #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
#'     #         'ModelID_ExperimentGrid.csv' if GridTune = TRUE. 
#'     #            Results of all model builds including parameter settings, bandit probs, and grid IDs
#'     #         'ModelID_EvaluationMetrics.csv' which contains all confusion matrix measures across all thresholds
#'     ModelID = "Test_Model_1",
#'     model_path = normalizePath("./"),
#'     metadata_path = file.path(normalizePath("./"),"R_Model_Testing"),
#'     SaveModelObjects = FALSE,
#'     ReturnModelObjects = TRUE,
#'     
#'     # Data arguments:
#'     #   'TrainOnFull' is to train a model with 100 percent of your data. 
#'     #     That means no holdout data will be used for evaluation
#'     #   If ValidationData and TestData are NULL and TrainOnFull is FALSE then data will be split 70 20 10
#'     #   'PrimaryDateColumn' is a date column in data that is meaningful when sorted. 
#'     #     CatBoost categorical treatment is enhanced when supplied
#'     #   'IDcols' are columns in your data that you don't use for modeling but get returned with ValidationData
#'     data = data,
#'     TrainOnFull = FALSE,
#'     ValidationData = NULL,
#'     TestData = NULL,
#'     TargetColumnName = "Adrian",
#'     FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
#'     PrimaryDateColumn = NULL,
#'     ClassWeights = c(1L,1L),
#'     IDcols = c("IDcol_1","IDcol_2"),
#'     
#'     # Model evaluation: 
#'     #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
#'     #   'loss_function' the loss function used in training optimization
#'     #   'NumOfParDepPlots' Number of partial dependence calibration plots generated. 
#'     #     A value of 3 will return plots for the top 3 variables based on variable importance
#'     #     Won't be returned if GrowPolicy is either "Depthwise" or "Lossguide" is used
#'     #     Can run the RemixAutoML::ParDepCalPlots() with the outputted ValidationData
#'     eval_metric = "AUC",
#'     loss_function = "Logloss",
#'     MetricPeriods = 10L,
#'     NumOfParDepPlots = ncol(data)-1L-2L,
#'
#'     # Grid tuning arguments:
#'     #   'PassInGrid' is for retraining using a previous grid winning args
#'     #   'MaxModelsInGrid' is a cap on the number of models that will run
#'     #   'MaxRunsWithoutNewWinner' number of runs without a new winner before exiting grid tuning
#'     #   'MaxRunMinutes' is a cap on the number of minutes that will run
#'     #   'Shuffles' is the number of times you want the random grid arguments shuffled
#'     #   'BaselineComparison' default means to compare each model build with a default built of catboost using max(Trees)
#'     #   'MetricPeriods' is the number of trees built before evaluting holdoutdata internally. Used in finding actual Trees used.
#'     PassInGrid = NULL,
#'     GridTune = FALSE,
#'     MaxModelsInGrid = 100L,
#'     MaxRunsWithoutNewWinner = 20L,
#'     MaxRunMinutes = 24L*60L,
#'     Shuffles = 4L,
#'     BaselineComparison = "default",
#'     
#'     # Trees, Depth, and LearningRate used in the bandit grid tuning
#'     # Must set Trees to a single value if you are not grid tuning
#'     # The ones below can be set to NULL and the values in the example will be used
#'     # GrowPolicy is turned off for CPU runs
#'     # BootStrapType utilizes Poisson only for GPU and MVS only for CPU
#'     Trees = seq(100L, 500L, 50L),
#'     Depth = seq(4L, 8L, 1L), 
#'     LearningRate = seq(0.01,0.10,0.01), 
#'     L2_Leaf_Reg = seq(1.0, 10.0, 1.0), 
#'     RSM = c(0.80, 0.85, 0.90, 0.95, 1.0),
#'     BootStrapType = c("Bayesian", "Bernoulli", "Poisson", "MVS", "No"),
#'     GrowPolicy = c("SymmetricTree", "Depthwise", "Lossguide"))
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
                                   ClassWeights = NULL,
                                   IDcols = NULL,
                                   task_type = "GPU",
                                   NumGPUs = 1,
                                   eval_metric = "MCC",
                                   loss_function = NULL,
                                   model_path = NULL,
                                   metadata_path = NULL,
                                   ModelID = "FirstModel",
                                   NumOfParDepPlots = 0L,
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE,
                                   PassInGrid = NULL,
                                   GridTune = FALSE,
                                   MaxModelsInGrid = 10L,
                                   MaxRunsWithoutNewWinner = 20L,
                                   MaxRunMinutes = 24L*60L,
                                   Shuffles = 1L,
                                   BaselineComparison = "default",
                                   MetricPeriods = 10L,
                                   Trees = 50L,
                                   Depth = NULL, 
                                   LearningRate = NULL, 
                                   L2_Leaf_Reg = NULL, 
                                   RSM = NULL, 
                                   BootStrapType = NULL,
                                   GrowPolicy = NULL) {
  
  # Load catboost----
  loadNamespace(package = "catboost")
  
  # Loss Function----
  if(is.null(loss_function)) LossFunction <- "Logloss" else LossFunction <- loss_function
  
  # Turn on full speed ahead----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))
  
  # Ensure model_path and metadata_path exists----
  if(!is.null(model_path)) if(!dir.exists(file.path(normalizePath(model_path)))) dir.create(normalizePath(model_path))
  if(!is.null(metadata_path)) if(!is.null(metadata_path)) if(!dir.exists(file.path(normalizePath(metadata_path)))) dir.create(normalizePath(metadata_path))
  
  # Binary Check Arguments----
  if(!(tolower(task_type) %chin% c("gpu", "cpu"))) return("task_type needs to be either 'GPU' or 'CPU'")
  if(is.null(NumGPUs)) NumGPUs <- '0' else if(NumGPUs > 1L) NumGPUs <- paste0('0-', NumGPUs-1L) else NumGPUs <- '0'
  if(is.null(ClassWeights)) ClassWeights <- c(1,1)
  if(!is.null(PrimaryDateColumn)) HasTime <- TRUE else HasTime <- FALSE
  if(any(Trees < 1L)) return("Trees must be greater than 1")
  if(!GridTune & length(Trees) > 1L) Trees <- Trees[length(Trees)]
  if(!GridTune %in% c(TRUE, FALSE)) return("GridTune needs to be TRUE or FALSE")
  if(MaxModelsInGrid < 1L & GridTune) return("MaxModelsInGrid needs to be at least 1")
  if(!is.null(model_path)) if(!is.character(model_path)) return("model_path needs to be a character type")
  if(!is.null(metadata_path)) if(!is.character(metadata_path)) return("metadata_path needs to be a character type")
  if(!is.character(ModelID)) return("ModelID needs to be a character type")
  if(NumOfParDepPlots < 0L) return("NumOfParDepPlots needs to be a positive number")
  if(!(ReturnModelObjects %in% c(TRUE, FALSE))) return("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects %in% c(TRUE, FALSE))) return("SaveModelObjects needs to be TRUE or FALSE")
  
  # Binary Ensure data is a data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(ValidationData)) if(!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)
  if(!is.null(TestData)) if(!data.table::is.data.table(TestData)) data.table::setDT(TestData)
  
  # Binary Target Name Storage----
  if (is.character(TargetColumnName)) Target <- TargetColumnName else Target <- names(data)[TargetColumnName]
  
  # Binary IDcol Name Storage----
  if(!is.null(IDcols)) if(!is.character(IDcols)) IDcols <- names(data)[IDcols]
  
  # Binary Data Partition----
  if(is.null(ValidationData) & is.null(TestData) & TrainOnFull != TRUE) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3L,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = eval(Target),
      TimeColumnName = NULL)
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # Binary Sort data if PrimaryDateColumn----
  if(!is.null(PrimaryDateColumn)) {
    data.table::setorderv(x = data, cols = eval(PrimaryDateColumn), order = 1L)
    if(!(eval(PrimaryDateColumn) %in% IDcols)) data.table::set(data, j = eval(PrimaryDateColumn), value = NULL)
  }
  
  # Binary Sort ValidationData if PrimaryDateColumn----
  if(!is.null(PrimaryDateColumn) & TrainOnFull != TRUE) {
    data.table::setorderv(x = ValidationData, cols = eval(PrimaryDateColumn), order = 1L)
    if(!(eval(PrimaryDateColumn) %in% IDcols)) data.table::set(ValidationData, j = eval(PrimaryDateColumn), value = NULL)
  }
  
  # Binary Sort TestData if PrimaryDateColumn----
  if(!is.null(TestData) & TrainOnFull != TRUE) {
    if(!is.null(PrimaryDateColumn)) {
      data.table::setorderv(x = TestData, cols = eval(PrimaryDateColumn), order = -1L)
      if(!(eval(PrimaryDateColumn) %in% IDcols)) data.table::set(TestData, j = eval(PrimaryDateColumn), value = NULL)
    }
  }
  
  # Binary data Subset Columns Needed----
  if(is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    keep1 <- names(data)[c(FeatureColNames)]
    keep <- c(keep1, Target)
    dataTrain <- data[, ..keep]
    if(TrainOnFull != TRUE) dataTest <- ValidationData[, ..keep]
  } else {
    keep <- c(FeatureColNames, Target)
    dataTrain <- data[, ..keep]
    if(TrainOnFull != TRUE) dataTest <- ValidationData[, ..keep]
  }
  
  # Binary TestData Subset Columns Needed----
  if(!is.null(TestData)) {
    if(is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
      keep1 <- names(TestData)[c(FeatureColNames)]
      if(!is.null(IDcols)) keep <- c(IDcols, keep1, Target) else keep <- c(keep1, Target)
      TestData <- TestData[, ..keep]
    } else {
      keep1 <- c(FeatureColNames)
      if(!is.null(IDcols)) keep <- c(IDcols, FeatureColNames, Target) else keep <- c(FeatureColNames, Target)
      TestData <- TestData[, ..keep]
    }
    if(!is.null(IDcols)) {
      TestMerge <- data.table::copy(TestData)
      keep <- c(keep1, Target)
      TestData <- TestData[, ..keep]
    } else {
      TestMerge <- data.table::copy(TestData)
    }
  }
  
  # Binary Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(sapply(data, is.factor))), as.numeric(which(sapply(data, is.character)))))
  
  # Binary Convert CatFeatures to 1-indexed----
  if(length(CatFeatures) > 0L) for(i in seq_len(length(CatFeatures))) CatFeatures[i] <- CatFeatures[i] - 1L
  
  # Binary Train ModelDataPrep----
  dataTrain <- ModelDataPrep(data = dataTrain, Impute = TRUE, CharToFactor = TRUE, RemoveDates = TRUE, MissFactor = "0", MissNum = -1)  
  
  # Binary Validation ModelDataPrep----
  if(TrainOnFull != TRUE) dataTest <- ModelDataPrep(data = dataTest, Impute = TRUE, CharToFactor = TRUE, RemoveDates = TRUE, MissFactor = "0", MissNum = -1)  
  
  # Binary Test ModelDataPrep----
  if(!is.null(TestData)) TestData <- ModelDataPrep(data = TestData, Impute = TRUE, CharToFactor = TRUE, RemoveDates = TRUE, MissFactor = "0", MissNum = -1)
  
  # Binary Save Names of data----
  if(is.numeric(FeatureColNames)) {
    Names <- data.table::as.data.table(names(data)[FeatureColNames])
    data.table::setnames(Names, "V1", "ColNames")
  } else {
    Names <- data.table::as.data.table(FeatureColNames)
    if(!"V1" %chin% names(Names)) data.table::setnames(Names, "FeatureColNames", "ColNames") else data.table::setnames(Names, "V1", "ColNames")
  }
  if(SaveModelObjects) data.table::fwrite(Names, paste0(model_path, "/", ModelID, "_ColNames.csv"))
  
  # Binary Subset Target Variables----
  TrainTarget <- tryCatch({dataTrain[, get(Target)]}, error = function(x) dataTrain[, eval(Target)])
  if(TrainOnFull != TRUE) {
    TestTarget <- tryCatch({dataTest[, get(Target)]}, error = function(x) dataTest[, eval(Target)])
    if(!is.null(TestData)) FinalTestTarget <- tryCatch({TestData[, get(Target)]}, error = function(x) TestData[, eval(Target)])
  }
  
  # Binary Initialize Catboost Data Conversion----
  if(!is.null(CatFeatures)) {
    if(!is.null(TestData)) {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget, cat_features = CatFeatures)
      if(TrainOnFull != TRUE) {
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
        FinalTestPool <- catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget, cat_features = CatFeatures)        
      }
    } else {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget, cat_features = CatFeatures)
      if(TrainOnFull != TRUE) {
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)        
      }
    }
  } else {
    if(!is.null(TestData)) {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      if(TrainOnFull != TRUE) {
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
        FinalTestPool <- catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget)  
      }
    } else {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      if(TrainOnFull != TRUE) TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
    }
  }
  
  # Binary Grid Tune or Not Check----
  if(GridTune & !TrainOnFull) {
    
    # Pull in Grid sets----
    Grids <- CatBoostParameterGrids(TaskType=task_type,Shuffles=Shuffles,NTrees=Trees,Depth=Depth,LearningRate=LearningRate,L2_Leaf_Reg=L2_Leaf_Reg,RSM=RSM,BootStrapType=BootStrapType,GrowPolicy=GrowPolicy)
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
    data.table::set(ExperimentalGrid, j = paste0("BanditProbs_",names(GridClusters)), value = -10)
    
    # Binary Grid Tuning Main Loop----
    counter <- 0L
    NewGrid <- 1L
    repeat {
      
      # Increment counter----
      counter <- counter + 1L
      
      # Print iteration so people can see it on their screen----
      for(i in 1:20) print(paste0("You are on iteration number: ",counter))
      
      # Check if grid still has elements in it----
      if(!is.null(GridClusters[[paste0("Grid_",max(1L, NewGrid-1L))]][["L2_Leaf_Reg"]][1L])) {
        
        # Define parameters----
        if(!exists("NewGrid")) {
          base_params <- CatBoostClassifierParams(NumGPUs=NumGPUs,LossFunction=LossFunction,counter=counter,BanditArmsN=BanditArmsN,HasTime=HasTime,MetricPeriods=MetricPeriods,ClassWeights=ClassWeights,eval_metric=eval_metric,task_type=task_type,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        } else {
          base_params <- CatBoostClassifierParams(NumGPUs=NumGPUs,LossFunction=LossFunction,NewGrid=NewGrid,counter=counter,BanditArmsN=BanditArmsN,HasTime=HasTime,MetricPeriods=MetricPeriods,ClassWeights=ClassWeights,eval_metric=eval_metric,task_type=task_type,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        }
        
        # Build model----
        print(base_params)
        RunTime <- system.time(model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params))
        
        # Score model----
        if(!is.null(TestData)) {
          predict <- catboost::catboost.predict(model = model, pool = FinalTestPool, prediction_type = "Probability", thread_count = -1L)
          calibEval <- data.table::as.data.table(cbind(Target = FinalTestTarget, p1 = predict))
          AUC_Metrics <- pROC::roc(response = calibEval[["Target"]], predictor = calibEval[["p1"]], na.rm = TRUE, algorithm = 3L, auc = TRUE, ci = TRUE)
        } else {
          predict <- catboost::catboost.predict(model = model,pool = TestPool, prediction_type = "Probability", thread_count = -1L)
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
        data.table::set(ExperimentalGrid, i = counter, j = "EvalMetric", value = NewPerformance)
        data.table::set(ExperimentalGrid, i = counter, j = "TreesBuilt", value = model$tree_count)
        if(counter == 1L) {
          BestPerformance <- 1L
        } else {
          if(tolower(BaselineComparison) == "default") {
            BestPerformance <- ExperimentalGrid[RunNumber == 1L][["EvalMetric"]]
          } else {
            BestPerformance <- max(ExperimentalGrid[RunNumber < counter][["EvalMetric"]], na.rm = TRUE)
          }
        }
        
        # Performance measures----
        TotalRunTime <- ExperimentalGrid[RunTime != -1L, sum(RunTime, na.rm = TRUE)]
        if(NewPerformance > BestPerformance) {
          RunsWithoutNewWinner <- 0L
        } else {
          RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
        }
        
        # Binary Remove Model and Collect Garbage----
        rm(model)
        gc()
      }
      
      # Update bandit probabilities and whatnot----
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
      if(!tolower(task_type) == "gpu") data.table::set(ExperimentalGrid, i = counter+1L, j = "RSM", value = GridClusters[[paste0("Grid_",NewGrid)]][["RSM"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "BootStrapType", value = GridClusters[[paste0("Grid_",NewGrid)]][["BootStrapType"]][Trials[NewGrid]+1L])
      if(tolower(task_type) == "gpu") data.table::set(ExperimentalGrid, i = counter+1L, j = "GrowPolicy", value = GridClusters[[paste0("Grid_",NewGrid)]][["GrowPolicy"]][Trials[NewGrid]+1L])
      for(bandit in seq_len(length(BanditProbs))) data.table::set(ExperimentalGrid, i = counter+1L, j = paste0("BanditProbs_Grid_",bandit), value = BanditProbs[bandit])
    }
    
    # Remove unneeded rows----
    ExperimentalGrid <- ExperimentalGrid[RunTime != -1L]
  }
  
  # Define parameters for case where you pass in a winning GridMetrics from grid tuning----
  if(!is.null(PassInGrid)) {
    if(PassInGrid[,.N] > 1L) PassInGrid <- PassInGrid[order(EvalMetric)][1]
    if(PassInGrid[, BanditProbs_Grid_1] == -10) {
      PassInGrid <- NULL
    }
  }
  if(!is.null(PassInGrid)) {
    if(tolower(task_type) == "gpu") {
      base_params <- list(
        has_time             = HasTime,
        metric_period        = 1L,
        loss_function        = "Logloss",
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        task_type            = task_type,
        class_weights        = if(is.null(ClassWeights)) c(1,1) else ClassWeights,
        train_dir            = model_path,
        iterations           = PassInGrid[["TreesBuilt"]],
        depth                = PassInGrid[["Depth"]],
        learning_rate        = PassInGrid[["LearningRate"]],
        l2_leaf_reg          = PassInGrid[["L2_Leaf_Reg"]],
        bootstrap_type       = PassInGrid[["BootStrapType"]],
        grow_policy          = PassInGrid[["GrowPolicy"]])
    } else {
      base_params <- list(
        has_time             = HasTime,
        metric_period        = 1L,
        loss_function        = "Logloss",
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        task_type            = task_type,
        train_dir            = model_path,
        class_weights        = if(is.null(ClassWeights)) c(1,1) else ClassWeights,
        iterations           = PassInGrid[["TreesBuilt"]],
        depth                = PassInGrid[["Depth"]],
        learning_rate        = PassInGrid[["LearningRate"]],
        l2_leaf_reg          = PassInGrid[["L2_Leaf_Reg"]],
        bootstrap_type       = PassInGrid[["BootStrapType"]],
        grow_policy          = PassInGrid[["GrowPolicy"]],
        rsm                  = PassInGrid[["RSM"]])
    }
  }
  
  # Define parameters for case where you want to run grid tuning----
  if(GridTune & !TrainOnFull) {
    
    # Prepare winning grid----
    BestGrid <- ExperimentalGrid[order(-EvalMetric)][1L]
    if(tolower(task_type) == "gpu") grid_params <- as.list(BestGrid[, c(5L:12L)]) else grid_params <- as.list(BestGrid[, c(5L:11L)])
    if(tolower(task_type) == "gpu") grid_params <- grid_params[!names(grid_params) %chin% "RSM"]
    if(tolower(task_type) == "cpu") grid_params <- as.list(BestGrid[, c(5L:12L)]) else grid_params <- as.list(BestGrid[, c(5L:11L)])
    if(tolower(task_type) == "cpu") grid_params <- grid_params[!names(grid_params) %chin% "GrowPolicy"]
    
    # Set parameters from winning grid----
    if(BestGrid$RunNumber == 1L) {
      base_params <- list(
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        metric_period        = MetricPeriods,
        iterations           = BestGrid[["TreesBuilt"]],
        loss_function        = LossFunction,
        eval_metric          = eval_metric,
        has_time             = HasTime,
        task_type            = task_type,
        devices              = NumGPUs,
        class_weights        = ClassWeights)
    } else {
      if(tolower(task_type) == "gpu") {
        base_params <- list(
          has_time             = HasTime,
          metric_period        = MetricPeriods,
          loss_function        = LossFunction,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          best_model_min_trees = 10L,
          task_type            = task_type,
          devices              = NumGPUs,
          train_dir            = model_path,
          class_weights        = ClassWeights,
          iterations           = BestGrid[["NTrees"]],
          depth                = BestGrid[["Depth"]],
          learning_rate        = BestGrid[["LearningRate"]],
          l2_leaf_reg          = BestGrid[["L2_Leaf_Reg"]],
          bootstrap_type       = BestGrid[["BootStrapType"]],
          grow_policy          = BestGrid[["GrowPolicy"]])
      } else {
        base_params <- list(
          has_time             = HasTime,
          metric_period        = MetricPeriods,
          loss_function        = LossFunction,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          best_model_min_trees = 10L,
          task_type            = task_type,
          devices              = NumGPUs,
          train_dir            = model_path,
          class_weights        = ClassWeights,
          iterations           = BestGrid[["NTrees"]],
          depth                = BestGrid[["Depth"]],
          learning_rate        = BestGrid[["LearningRate"]],
          l2_leaf_reg          = BestGrid[["L2_Leaf_Reg"]],
          bootstrap_type       = BestGrid[["BootStrapType"]],
          rsm                  = BestGrid[["RSM"]])
      }
    }
  }
  
  # Define parameters Not pass in GridMetric and not grid tuning----
  if(is.null(PassInGrid) & !GridTune) {
    base_params <- list(
      use_best_model       = TRUE,
      best_model_min_trees = 10L,
      metric_period        = MetricPeriods,
      iterations           = Trees,
      loss_function        = LossFunction,
      eval_metric          = eval_metric,
      has_time             = HasTime,
      task_type            = task_type,
      devices              = NumGPUs,
      class_weights        = ClassWeights)
  }

  # Binary Train Final Model----
  if(!TrainOnFull) {
    model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params)
  } else {
    model <- catboost::catboost.train(learn_pool = TrainPool, params = base_params)
  }
  
  # Binary Save Model----
  if(SaveModelObjects) catboost::catboost.save_model(model = model, model_path = file.path(normalizePath(model_path), ModelID))
  
  # Binary Score Final Test Data----
  if(TrainOnFull != TRUE) {
    if(!is.null(TestData)) {
      predict <- catboost::catboost.predict(
        model = model,
        pool = FinalTestPool,
        prediction_type = "Probability",
        thread_count = -1L)
    } else if(TrainOnFull) {
      predict <- catboost::catboost.predict(
        model = model,
        pool = TrainPool,
        prediction_type = "Probability",
        thread_count = -1L)
    } else {
      predict <- catboost::catboost.predict(
        model = model,
        pool = TestPool,
        prediction_type = "Probability",
        thread_count = -1L)
    }
  }
  
  # Binary Validation Data----
  if(TrainOnFull != TRUE) {
    if(!is.null(TestData)) {
      ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, p1 = predict))
      data.table::setnames(ValidationData, "Target",eval(TargetColumnName))
    } else {
      ValidationData <- data.table::as.data.table(cbind(Target = TestTarget, dataTest, p1 = predict))
      data.table::setnames(ValidationData, "Target",eval(TargetColumnName))
    }  
  } else {
    data <- data.table::as.data.table(cbind(Target = TrainTarget, data, Predict = predict)) 
    data.table::setnames(data, "Target",eval(TargetColumnName))
  }
  
  # Save Validation Data to File----
  if(SaveModelObjects & !TrainOnFull) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(ValidationData, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ValidationData.csv")))
    } else {
      data.table::fwrite(ValidationData, file = file.path(normalizePath(model_path), paste0(ModelID, "_ValidationData.csv")))
    }
  }
  
  # Binary AUC Object Create----
  if(!TrainOnFull) {
    temp <- ValidationData[order(runif(ValidationData[,.N]))][1L:min(100000L, ValidationData[,.N])]
    AUC_Metrics <- pROC::roc(
      response = temp[[eval(TargetColumnName)]],
      predictor = temp[["p1"]],
      na.rm = TRUE,
      algorithm = 3L,
      auc = TRUE,
      ci = TRUE)
    rm(temp)
  }
  
  # Binary AUC Conversion to data.table----
  if(!TrainOnFull) {
    AUC_Data <- data.table::data.table(
      ModelNumber = 0L,
      Sensitivity = AUC_Metrics$sensitivities,
      Specificity = AUC_Metrics$specificities)  
  }
  
  # Binary Plot ROC Curve----
  if(!TrainOnFull) {
    if(GridTune == TRUE & MaxModelsInGrid <= 15L) {
      temp <- data.table::rbindlist(AUC_List)
      AUC_Data <- data.table::rbindlist(list(temp, AUC_Data))
      AUC_Data[, ModelNumber := as.factor(ModelNumber)]
      ROC_Plot <- ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity,group = ModelNumber,color = ModelNumber)) +
        ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]])) +
        ggplot2::geom_abline(slope = 1, color = "black") +
        ggplot2::ggtitle(paste0("Catboost Best Model AUC: ", 100 * round(AUC_Metrics$auc, 3), "%")) +
        ChartTheme() + ggplot2::xlab("Specificity") +
        ggplot2::ylab("Sensitivity")
    } else {
      ROC_Plot <- ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity)) +
        ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]]), color = "blue") +
        ggplot2::geom_abline(slope = 1, color = "black") +
        ggplot2::ggtitle(paste0("Catboost AUC: ", 100 * round(AUC_Metrics$auc, 3), "%")) + 
        ChartTheme() + ggplot2::xlab("Specificity") +
        ggplot2::ylab("Sensitivity")
    }  
  }
  
  # Save plot to file----
  if(!TrainOnFull) {
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(file.path(normalizePath(metadata_path), paste0(ModelID, "_ROC_Plot.png")))
      } else {
        ggplot2::ggsave(file.path(normalizePath(model_path), paste0(ModelID, "_ROC_Plot.png")))
      }
    }  
  }
  
  # Binary Evaluation Calibration Plot----
  if(!TrainOnFull) {
    EvaluationPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "p1",
      TargetColName = eval(TargetColumnName),
      GraphType = "calibration",
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))  
  }
  
  # Add Number of Trees to Title----
  if(!TrainOnFull) EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: AUC = ",round(AUC_Metrics$auc, 3L)))
  
  # Save plot to file----
  if(!TrainOnFull) {
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationPlot.png")))
      } else {
        ggplot2::ggsave(file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationPlot.png")))
      }
    }  
  }

  # Binary Variable Importance----
  if(GridTune) {
    if(tolower(task_type) == "gpu") {
      if(!BestGrid[["GrowPolicy"]] %chin% c("Depthwise","Lossguide")) {
        temp <- catboost::catboost.get_feature_importance(model)
        VariableImportance <- data.table::data.table(cbind(Variable = rownames(temp), temp))
        data.table::setnames(VariableImportance, "V2", "Importance")
        VariableImportance[, Importance := round(as.numeric(Importance), 4L)]
        VariableImportance <- VariableImportance[order(-Importance)]
        if(SaveModelObjects) {
          if(!is.null(metadata_path)) {
            data.table::fwrite(VariableImportance, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_VariableImportance.csv")))
          } else {
            data.table::fwrite(VariableImportance, file = file.path(normalizePath(model_path), paste0(ModelID, "_VariableImportance.csv")))
          }
        }
      } else {
        VariableImportance <- NULL
      }
    } else {
      temp <- catboost::catboost.get_feature_importance(model)
      VariableImportance <- data.table::data.table(cbind(Variable = rownames(temp), temp))
      data.table::setnames(VariableImportance, "V2", "Importance")
      VariableImportance[, Importance := round(as.numeric(Importance), 4L)]
      VariableImportance <- VariableImportance[order(-Importance)]
      if (SaveModelObjects) {
        if(!is.null(metadata_path)) {
          data.table::fwrite(VariableImportance, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_VariableImportance.csv")))
        } else {
          data.table::fwrite(VariableImportance, file = file.path(normalizePath(model_path), paste0(ModelID, "_VariableImportance.csv")))
        }
      }
    }
  } else {
    temp <- catboost::catboost.get_feature_importance(model)
    VariableImportance <- data.table::data.table(cbind(Variable = rownames(temp), temp))
    data.table::setnames(VariableImportance, "V2", "Importance")
    VariableImportance[, Importance := round(as.numeric(Importance), 4L)]
    VariableImportance <- VariableImportance[order(-Importance)]
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(VariableImportance, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_VariableImportance.csv")))
      } else {
        data.table::fwrite(VariableImportance, file = file.path(normalizePath(model_path), paste0(ModelID, "_VariableImportance.csv")))
      }
    }
  }
  
  # Binary Partial Dependence----
  ParDepPlots <- list()
  ParDepBoxPlots <- list()
  j <- 0L
  if(!is.null(VariableImportance) & NumOfParDepPlots > 0L) {
    for(i in seq_len(min(length(FeatureColNames), NumOfParDepPlots, VariableImportance[,.N]))) {
      tryCatch({
        Out <- ParDepCalPlots(
          data = ValidationData,
          PredictionColName = "p1",
          TargetColName = eval(TargetColumnName),
          IndepVar = VariableImportance[i, Variable],
          GraphType = "calibration",
          PercentileBucket = 0.05,
          FactLevels = 10L,
          Function = function(x) mean(x, na.rm = TRUE))
        j <- j + 1L
        ParDepPlots[[paste0(VariableImportance[j, Variable])]] <- Out
      }, error = function(x) "skip")
    }
  } else {
    ParDepPlots <- NULL
  }
  
  # Binary Save ParDepPlots to file----
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      if(!is.null(VariableImportance)) save(ParDepPlots, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ParDepPlots.R")))
    } else {
      if(!is.null(VariableImportance)) save(ParDepPlots, file = file.path(normalizePath(model_path), paste0(ModelID, "_ParDepPlots.R")))
    }
  }
  
  # Binary Save ExperimentalGrid----
  if(SaveModelObjects & GridTune == TRUE) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(ExperimentalGrid, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ExperimentalGrid.csv")))
    } else {
      data.table::fwrite(ExperimentalGrid, file = file.path(normalizePath(model_path), paste0(ModelID, "_ExperimentalGrid.csv")))
    }
  }
  
  # Save EvaluationMetrics to File
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(RemixClassificationMetrics(MLModels="catboost",TargetVariable=eval(TargetColumnName),Thresholds=seq(0.01,0.99,0.01),CostMatrix=c(1,0,0,1),ClassLabels=c(1,0),CatBoostTestData=ValidationData), file = file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationMetrics.csv")))
    } else {
      data.table::fwrite(RemixClassificationMetrics(MLModels="catboost",TargetVariable=eval(TargetColumnName),Thresholds=seq(0.01,0.99,0.01),CostMatrix=c(1,0,0,1),ClassLabels=c(1,0),CatBoostTestData=ValidationData), file = file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationMetrics.csv")))
    }
  }
  
  # Final Garbage Collection----
  gc()
  
  # VI_Plot_Function----
  VI_Plot <- function(VI_Data, ColorHigh = "darkblue", ColorLow = "white") {
    ggplot2::ggplot(VI_Data[1L:min(10L,.N)], ggplot2::aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_gradient2(mid = ColorLow,high = ColorHigh) +
      ChartTheme(Size = 12L, AngleX = 0L, LegendPosition = "right") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Global Variable Importance") +
      ggplot2::xlab("Top Model Features") +
      ggplot2::ylab("Value")
  }
  
  # Remove extenal files if GridTune is TRUE----
  if(GridTune) {
    if(file.exists(file.path(getwd(),"catboost_training.json"))) file.remove(file.path(getwd(),"catboost_training.json"))
    if(file.exists(file.path(getwd(),"learn_error.tsv"))) file.remove(file.path(getwd(),"learn_error.tsv"))
    if(file.exists(file.path(getwd(),"test_error.tsv"))) file.remove(file.path(getwd(),"test_error.tsv"))
    if(file.exists(file.path(getwd(),"time_left.tsv"))) file.remove(file.path(getwd(),"time_left.tsv"))
    if(dir.exists(file.path(getwd(),"catboost_info"))) unlink(x = file.path(getwd(),"catboost_info"), recursive = TRUE)
    if(dir.exists(file.path(getwd(),"learn"))) unlink(x = file.path(getwd(),"learn"), recursive = TRUE)
    if(dir.exists(file.path(getwd(),"test"))) unlink(x = file.path(getwd(),"test"), recursive = TRUE)
    if(dir.exists(file.path(getwd(),"tmp"))) unlink(x = file.path(getwd(),"tmp"), recursive = TRUE)
  } else {
    if(dir.exists(file.path(getwd(),"catboost_info"))) unlink(x = file.path(getwd(),"catboost_info"), recursive = TRUE)
  }
  
  # Binary Return Model Objects----
  if(TrainOnFull) {
    if(ReturnModelObjects) {
      return(list(
        Model = model,
        ValidationData = ValidationData,
        VariableImportance = VariableImportance,
        VI_Plot = tryCatch({VI_Plot(VariableImportance)}, error = NULL),
        ColNames = Names))
    }
  } else if(ReturnModelObjects) {
    return(list(
      Model = model,
      ValidationData = ValidationData,
      ROC_Plot = ROC_Plot,
      EvaluationPlot = EvaluationPlot,
      EvaluationMetrics = RemixClassificationMetrics(MLModels="catboost",TargetVariable=eval(TargetColumnName),Thresholds=seq(0.01,0.99,0.01),CostMatrix=c(1,0,0,1),ClassLabels=c(1,0),CatBoostTestData=ValidationData),
      VariableImportance = VariableImportance,
      VI_Plot = tryCatch({VI_Plot(VariableImportance)}, error = NULL),
      PartialDependencePlots = ParDepPlots,
      GridMetrics = if(exists("ExperimentalGrid")) data.table::setorderv(ExperimentalGrid, cols = "EvalMetric", order = -1L, na.last = TRUE) else NULL,
      ColNames = Names))
  }
}
