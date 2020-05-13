#' AutoCatBoostMultiClass is an automated catboost model grid-tuning multinomial classifier and evaluation system
#'
#' AutoCatBoostMultiClass is an automated modeling function that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, variable importance, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package').
#' 
#' @author Adrian Antico
#' @family Automated MultiClass Classification
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
#' @param eval_metric This is the metric used inside catboost to measure performance on validation data during a grid-tune. MultiClass or MultiClassOneVsAll
#' @param grid_eval_metric For evaluating models within grid tuning. Choices include, "accuracy", "microauc", "logloss"
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
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
#' @param Depth Bandit grid partitioned. Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param LearningRate Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param L2_Leaf_Reg Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the L2_Leaf_Reg values to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param RSM CPU only. Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the RSM values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.80, 0.85, 0.90, 0.95, 1.0)
#' @param BootStrapType Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the BootStrapType values to test. For running grid tuning, a NULL value supplied will mean these values are tested c("Bayesian", "Bernoulli", "Poisson", "MVS", "No")
#' @param GrowPolicy Random testing. NULL, character, or vector for GrowPolicy to test. For grid tuning, supply a vector of values. For running grid tuning, a NULL value supplied will mean these values are tested c("SymmetricTree", "Depthwise", "Lossguide")
#' @examples
#' \donttest{
#' # Create some dummy correlated data with numeric and categorical features
#' data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = FALSE, MultiClass = TRUE)
#' 
#' # Run function
#' TestModel <- RemixAutoML::AutoCatBoostMultiClass(
#'     
#'     # GPU or CPU and the number of available GPUs
#'     task_type = "GPU",
#'     
#'     # Metadata arguments: 
#'     #   'ModelID' is used to create part of the file names generated when saving to file'
#'     #   'model_path' is where the minimal model objects for scoring will be stored
#'     #      'ModelID' will be the name of the saved model object
#'     #   'metadata_path' is where model evaluation and model interpretation files are saved
#'     #      objects saved to model_path if metadata_path is null
#'     #      Saved objects include: 
#'     #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
#'     #         'ModelID_VariableImportance.csv' is the variable importance. 
#'     #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
#'     #         'ModelID_ExperimentGrid.csv' if GridTune = TRUE. 
#'     #            Results of all model builds including parameter settings, bandit probs, and grid IDs
#'     #         'ModelID_EvaluationMetrics.csv' which contains all confusion matrix measures across all thresholds
#'     ModelID = "Test_Model_1",
#'     model_path = getwd(),
#'     metadata_path = file.path(getwd(),"R_Model_Testing"),
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
#'     FeatureColNames = names(data)[4L:ncol(data)],
#'     PrimaryDateColumn = NULL,
#'     ClassWeights = c(1L,1L,1L,1L,1L),
#'     IDcols = c("x1","x2"),
#'     
#'     # Model evaluation: 
#'     #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
#'     #   'loss_function' the loss function used in training optimization
#'     eval_metric = "MultiClass",
#'     grid_eval_metric = "Accuracy",
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
#'     GridTune = TRUE,
#'     MaxModelsInGrid = 100L,
#'     MaxRunsWithoutNewWinner = 20L,
#'     MaxRunMinutes = 24L*60L,
#'     Shuffles = 4L,
#'     BaselineComparison = "default",
#'     MetricPeriods = 10L,
#'     
#'     # Trees, Depth, and LearningRate used in the bandit grid tuning
#'     # Must set Trees to a single value if you are not grid tuning
#'     # The ones below can be set to NULL and the values in the example will be used
#'     # GrowPolicy is turned off for CPU runs
#'     # BootStrapType utilizes Poisson only for GPU and MVS only for CPU
#'     Trees = seq(1000L, 5000L, 500L),
#'     Depth = seq(4L, 8L, 1L), 
#'     LearningRate = seq(0.01,0.10,0.01), 
#'     L2_Leaf_Reg = seq(1.0, 10.0, 1.0), 
#'     RSM = c(0.80, 0.85, 0.90, 0.95, 1.0),
#'     BootStrapType = c("Bayesian", "Bernoulli", "Poisson", "MVS", "No"),
#'     GrowPolicy = c("SymmetricTree", "Depthwise", "Lossguide"))
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model (the model), ValidationData.csv, EvaluationMetrics.csv, GridCollect, and GridList
#' @export
AutoCatBoostMultiClass <- function(data,
                                   TrainOnFull = FALSE,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   ClassWeights = NULL,
                                   IDcols = NULL,
                                   task_type = "GPU",
                                   eval_metric = "MultiClassOneVsAll",
                                   model_path = NULL,
                                   metadata_path = NULL,
                                   ModelID = "FirstModel",
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE,
                                   PassInGrid = NULL,
                                   GridTune = FALSE,
                                   MaxModelsInGrid = 10L,
                                   MaxRunsWithoutNewWinner = 20L,
                                   MaxRunMinutes = 24L*60L,
                                   grid_eval_metric = "Accuracy",
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
  
  # Turn on full speed ahead----
  data.table::setDTthreads(percent = 100L)
  
  # Ensure model_path and metadata_path exists----
  if(!dir.exists(file.path(model_path))) dir.create(model_path)
  if(!is.null(metadata_path)) if(!dir.exists(file.path(metadata_path))) dir.create(metadata_path)
  
  # MultiClass Check Arguments----
  if(!(tolower(task_type) %chin% c("gpu", "cpu"))) return("task_type needs to be either 'GPU' or 'CPU'")
  if(!(tolower(eval_metric) %chin% c("multiclass", "multiclassonevsall"))) return("eval_metric not in c('MultiClass','MultiClassOneVsAll')")
  if(!is.null(PrimaryDateColumn)) HasTime <- TRUE else HasTime <- FALSE
  if(any(Trees < 1L)) return("Trees must be greater than 1")
  if(!GridTune & length(Trees) > 1L) Trees <- Trees[length(Trees)]
  if(!GridTune %in% c(TRUE, FALSE)) return("GridTune needs to be TRUE or FALSE")
  if(MaxModelsInGrid < 1 & GridTune == TRUE) return("MaxModelsInGrid needs to be at least 1")
  if(!is.null(model_path)) if (!is.character(model_path)) return("model_path needs to be a character type")
  if(!is.null(metadata_path)) if(!is.character(metadata_path)) return("metadata_path needs to be a character type")
  if(!is.character(ModelID)) return("ModelID needs to be a character type")
  if(!(ReturnModelObjects %in% c(TRUE, FALSE))) return("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects %in% c(TRUE, FALSE))) return("SaveModelObjects needs to be TRUE or FALSE")

  # MultiClass Ensure data is a data.table----
  if(!data.table::is.data.table(data)) data <- data.table::as.data.table(data)
  if(!is.null(ValidationData)) if(!data.table::is.data.table(ValidationData)) ValidationData <- data.table::as.data.table(ValidationData)
  if(!is.null(TestData)) if(!data.table::is.data.table(TestData)) TestData <- data.table::as.data.table(TestData)

  # MultiClass Target Name Storage----
  if(is.character(TargetColumnName)) Target <- TargetColumnName else Target <- names(data)[TargetColumnName]
  
  # MultiClass IDcol Name Storage----
  if(!is.null(IDcols)) if(!is.character(IDcols)) IDcols <- names(data)[IDcols]
  
  # MultiClass Data Partition----
  if(is.null(ValidationData) & is.null(TestData) & !TrainOnFull) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3L,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = Target,
      TimeColumnName = NULL)
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # MultiClass Sort data if PrimaryDateColumn----
  if(!is.null(PrimaryDateColumn) & TrainOnFull) {
    data <- data[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) data.table::set(data,j = eval(PrimaryDateColumn),value = NULL)
  }
  
  # MultiClass Sort ValidationData if PrimaryDateColumn----
  if(!is.null(TestData) & TrainOnFull != TRUE) {
    if(!is.null(PrimaryDateColumn)) {
      data.table::setorderv(x = ValidationData, cols = eval(PrimaryDateColumn), order = 1L)
      if (!(eval(PrimaryDateColumn) %in% IDcols)) data.table::set(ValidationData,j = eval(PrimaryDateColumn),value = NULL)
    }
  }
  
  # MultiClass Sort TestData if PrimaryDateColumn----
  if(!is.null(TestData)) {
    if(!is.null(PrimaryDateColumn)) {
      TestData <- TestData[order(get(PrimaryDateColumn))]
      if(!(eval(PrimaryDateColumn) %in% IDcols)) data.table::set(TestData,j = eval(PrimaryDateColumn),value = NULL)
    }
  }
  
  # MultiClass data Subset Columns Needed----
  if(is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    keep1 <- names(data)[c(FeatureColNames)]
    keep <- c(keep1, Target)
    dataTrain <- data[, ..keep]
    if(TrainOnFull != TRUE) dataTest <- ValidationData[, ..keep] else dataTest <- NULL
  } else {
    keep <- c(FeatureColNames, Target)
    dataTrain <- data[, ..keep]
    if(TrainOnFull != TRUE) dataTest <- ValidationData[, ..keep] else dataTest <- NULL
  }
  
  # MultiClass TestData Subset Columns Needed----
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
  
  # Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(sapply(data, is.factor))), as.numeric(which(sapply(data, is.character)))))
  TargetNum <- which(names(data) == Target)
  CatFeatures <- setdiff(CatFeatures, TargetNum)
  
  # MultiClass Convert CatFeatures to 1-indexed----
  if(length(CatFeatures) > 0) for (i in seq_len(length(CatFeatures))) CatFeatures[i] <- CatFeatures[i] - 1L
  
  # MultiClass Train ModelDataPrep----
  dataTrain <- ModelDataPrep(
    data = dataTrain,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = TRUE,
    MissFactor = "0",
    MissNum = -1)  
  
  # MultiClass Validation ModelDataPrep----
  if(TrainOnFull != TRUE) {
    dataTest <- ModelDataPrep(
      data = dataTest,
      Impute = TRUE,
      CharToFactor = TRUE,
      RemoveDates = TRUE,
      MissFactor = "0",
      MissNum = -1)  
  }
  
  # MultiClass Test ModelDataPrep----
  if(!is.null(TestData)) {
    TestData <- ModelDataPrep(
      data = TestData,
      Impute = TRUE,
      CharToFactor = TRUE,
      RemoveDates = TRUE,
      MissFactor = "0",
      MissNum = -1)
  }
  
  # MultiClass Obtain Unique Target Levels----
  if(!is.null(TestData)) {
    temp <- data.table::rbindlist(list(dataTrain, dataTest, TestData))
  } else if(!is.null(dataTest)) {
    temp <- data.table::rbindlist(list(dataTrain, dataTest))
  } else {
    temp <- dataTrain
  }
  TargetLevels <- data.table::as.data.table(sort(unique(temp[[eval(TargetColumnName)]])))
  data.table::setnames(TargetLevels, "V1", "OriginalLevels")
  TargetLevels[, NewLevels := 1:.N]
  if(SaveModelObjects) data.table::fwrite(TargetLevels,file = paste0(model_path,"/",ModelID,"_TargetLevels.csv"))
  
  # MultiClass Convert Target to Numeric Factor----
  dataTrain <- merge(
    dataTrain,
    TargetLevels,
    by.x = eval(Target),
    by.y = "OriginalLevels",
    all = FALSE)
  dataTrain[, paste0(Target) := NewLevels]
  dataTrain[, NewLevels := NULL]
  if(TrainOnFull != TRUE) {
    dataTest <- merge(
      dataTest,
      TargetLevels,
      by.x = eval(Target),
      by.y = "OriginalLevels",
      all = FALSE)
    dataTest[, paste0(Target) := NewLevels]
    dataTest[, NewLevels := NULL]
    if(!is.null(TestData)) {
      TestData <- merge(
        TestData,
        TargetLevels,
        by.x = eval(Target),
        by.y = "OriginalLevels",
        all = FALSE)
      TestData[, paste0(Target) := NewLevels]
      TestData[, NewLevels := NULL]
    }
  }
  
  # Reorder Colnames----
  data.table::setcolorder(dataTrain, c(2L:ncol(dataTrain), 1L))
  if(!is.null(dataTest)) data.table::setcolorder(dataTest, c(2L:ncol(dataTest), 1L))
  if(!is.null(TestData)) data.table::setcolorder(TestData, c(2L:ncol(TestData), 1L))
  
  # MultiClass Save Names of data----
  if(is.numeric(FeatureColNames)) {
    Names <- data.table::as.data.table(names(data)[FeatureColNames])
    data.table::setnames(Names, "V1", "ColNames")
  } else {
    Names <- data.table::as.data.table(FeatureColNames)
    if(!"V1" %chin% names(Names)) {
      data.table::setnames(Names, "FeatureColNames", "ColNames")
    } else {
      data.table::setnames(Names, "V1", "ColNames")
    }
  }
  if(SaveModelObjects) data.table::fwrite(Names, paste0(model_path, "/", ModelID, "_ColNames.csv"))
  
  # MultiClass Subset Target Variables----
  TrainTarget <- tryCatch({dataTrain[, as.numeric(get(Target))]}, error = function(x) dataTrain[, as.numeric(eval(Target))])
  if(!is.null(dataTest)) TestTarget <- tryCatch({dataTest[, as.numeric(get(Target))]}, error = function(x) dataTest[, as.numeric(eval(Target))])  
  if(!is.null(TestData)) FinalTestTarget <- tryCatch({TestData[, as.numeric(get(Target))]}, error = function(x) TestData[, as.numeric(eval(Target))])
  
  # MultiClass Initialize Catboost Data Conversion----
  if(!is.null(CatFeatures)) {
    if(!is.null(TestData)) {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = as.integer(TrainTarget), cat_features = CatFeatures)
      if(!is.null(dataTest)) TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = as.integer(TestTarget), cat_features = CatFeatures)
      if(!is.null(TestData)) FinalTestPool <- catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = as.integer(FinalTestTarget), cat_features = CatFeatures)
    } else {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],label = as.integer(TrainTarget),cat_features = CatFeatures)
      if(!is.null(dataTest)) TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = as.integer(TestTarget), cat_features = CatFeatures)
    }
  } else {
    if(!is.null(TestData)) {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = as.integer(TrainTarget))
      if(!is.null(dataTest)) TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = as.integer(TestTarget))
      if(!is.null(TestData)) FinalTestPool <- catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = as.integer(FinalTestTarget))
    } else {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = as.integer(TrainTarget))
      if(!is.null(dataTest)) TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = as.integer(TestTarget))
    }
  }
  
  # MultiClass Grid Tune or Not Check----
  if(GridTune & !TrainOnFull) {
    
    # Pull in Grid sets----
    Grids <- CatBoostParameterGrids(TaskType=task_type,Shuffles=Shuffles,NTrees=Trees,Depth=Depth,LearningRate=LearningRate,L2_Leaf_Reg=L2_Leaf_Reg,RSM=RSM,BootStrapType=BootStrapType,GrowPolicy=GrowPolicy)
    Grid <- Grids$Grid
    GridClusters <- Grids$Grids
    ExperimentalGrid <- Grids$ExperimentalGrid
    
    # Initialize RL----
    RL_Start             <- RL_Initialize(ParameterGridSet = GridClusters, Alpha = 1L, Beta = 1L, SubDivisions = 1000L)
    BanditArmsN          <- RL_Start[["BanditArmsN"]]
    Successes            <- RL_Start[["Successes"]]
    Trials               <- RL_Start[["Trials"]]
    GridIDs              <- RL_Start[["GridIDs"]]
    BanditProbs          <- RL_Start[["BanditProbs"]]
    RunsWithoutNewWinner <- 0L
    rm(RL_Start)
    
    # Add bandit probs columns to ExperimentalGrid----
    data.table::set(ExperimentalGrid, j = paste0("BanditProbs_",names(GridClusters)), value = -10)
    
    # Binary Grid Tuning Main Loop----
    counter <- 0L
    repeat {
      
      # Increment counter----
      counter <- counter + 1L
      
      # Define parameters----
      if(!exists("NewGrid")) {
        base_params <- CatBoostMultiClassParams(counter=counter,BanditArmsN=BanditArmsN,HasTime=HasTime,MetricPeriods=MetricPeriods,ClassWeights=ClassWeights,eval_metric=eval_metric,task_type=task_type,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
      } else {
        base_params <- CatBoostMultiClassParams(NewGrid=NewGrid,counter=counter,BanditArmsN=BanditArmsN,HasTime=HasTime,MetricPeriods=MetricPeriods,ClassWeights=ClassWeights,eval_metric=eval_metric,task_type=task_type,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
      }
      
      # Build model----
      print(base_params)
      RunTime <- system.time(model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params))

      # MultiClass Score Final Test Data----
      if(!is.null(TestData)) {
        predict <- cbind(
          1 + catboost::catboost.predict(
            model = model,
            pool = FinalTestPool,
            prediction_type = "Class"),
          catboost::catboost.predict(
            model = model,
            pool = FinalTestPool,
            prediction_type = "Probability"))
      } else if(!TrainOnFull) {
        predict <- cbind(
          1 + catboost::catboost.predict(
            model = model,
            pool = TestPool,
            prediction_type = "Class"),
          catboost::catboost.predict(
            model = model,
            pool = TestPool,
            prediction_type = "Probability"))
      } else {
        predict <- cbind(
          1 + catboost::catboost.predict(
            model = model,
            pool = TrainPool,
            prediction_type = "Class"),
          catboost::catboost.predict(
            model = model,
            pool = TrainPool,
            prediction_type = "Probability"))
      }
      
      # MultiClass Grid Validation Data----
      if(!is.null(TestData)) {
        ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget, predict, TestMerge[, .SD, .SDcols = unique(names(TestMerge)[c(1L:(ncol(TestMerge)-1L))])]))
      } else if(!TrainOnFull) {
        ValidationData <- data.table::as.data.table(cbind(Target = TestTarget, predict))
      } else {
        ValidationData <- data.table::as.data.table(cbind(Target = TrainTarget, predict))
      }
      if(TrainOnFull) {
        ValidationData <- merge(
          ValidationData,
          TargetLevels,
          by.x = "Target",
          by.y = "NewLevels",
          all = FALSE)
        ValidationData[, Target := OriginalLevels][, OriginalLevels := NULL]
        ValidationData <- merge(
          ValidationData,
          TargetLevels,
          by.x = "V2",
          by.y = "NewLevels",
          all = FALSE)
        ValidationData[, V2 := OriginalLevels][, OriginalLevels := NULL] 
      } else {
        ValidationData <- merge(
          ValidationData,
          TargetLevels,
          by.x = "V1",
          by.y = "NewLevels",
          all = FALSE)
        ValidationData[, V1 := OriginalLevels][, OriginalLevels := NULL]
        ValidationData <- merge(
          ValidationData,
          TargetLevels,
          by.x = "Target",
          by.y = "NewLevels",
          all = FALSE)
        ValidationData[, Target := OriginalLevels][, OriginalLevels := NULL]  
      }
      
      # MultiClass Update Names for Predicted Value Columns----
      if(!TrainOnFull) k <- 1L else k <- 2L
      for(name in as.character(TargetLevels[[1L]])) {
        k <- k + 1L
        data.table::setnames(ValidationData, paste0("V", k), name)
      }
      if(!TrainOnFull) data.table::setnames(ValidationData, "V1", "Predict") else data.table::setnames(ValidationData, "V2", "Predict")
      data.table::set(ValidationData, j = "Target", value = as.character(ValidationData[["Target"]]))
      data.table::set(ValidationData, j = "Predict", value = as.character(ValidationData[["Predict"]]))
      
      # MultiClass Metrics Accuracy----
      if(tolower(grid_eval_metric) == "accuracy") {
        NewPerformance <- ValidationData[, mean(data.table::fifelse(as.character(Target) == as.character(Predict), 1.0, 0.0), na.rm = TRUE)]
        
      } else if(tolower(grid_eval_metric) == "microauc") {
        NewPerformance <- round(as.numeric(noquote(stringr::str_extract(pROC::multiclass.roc(response = ValidationData[["Target"]], predictor = as.matrix(ValidationData[, .SD, .SDcols = names(ValidationData)[3L:(ncol(predict)+1L)]]))$auc, "\\d+\\.*\\d*"))), 4L)
        
      } else if(tolower(grid_eval_metric) == "logloss") {
        temp <- ValidationData[, 1L]  
        temp[, Truth := get(TargetColumnName)]
        temp <- DummifyDT(
          data = temp, 
          cols = eval(TargetColumnName), 
          KeepFactorCols = FALSE, 
          OneHot = FALSE, 
          SaveFactorLevels = FALSE, 
          SavePath = NULL, 
          ImportFactorLevels = FALSE,
          FactorLevelsList = NULL, 
          ClustScore = FALSE, 
          ReturnFactorLevels = FALSE)
        N <- TargetLevels[, .N]
        NewPerformance <- MLmetrics::LogLoss(y_pred = as.matrix(ValidationData[, 3L:(2L+N)]), y_true = as.matrix(temp[, 2L:(1L+N)]))
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
          BestPerformance <- max(ExperimentalGrid[RunNumber == 1L][["EvalMetric"]], na.rm = TRUE)
        } else {
          BestPerformance <- max(ExperimentalGrid[RunNumber < counter][["EvalMetric"]], na.rm = TRUE)
        }
      }
      
      # Performance measures----
      TotalRunTime <- sum(ExperimentalGrid[RunTime != -1L][["RunTime"]], na.rm = TRUE)
      if(tolower(grid_eval_metric) %chin% c("accuracy","microauc")) {
        if(NewPerformance > BestPerformance) {
          RunsWithoutNewWinner <- 0L
        } else {
          RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
        }
      } else if(tolower(grid_eval_metric) %chin% c("logloss")) {
        if(NewPerformance < BestPerformance) {
          RunsWithoutNewWinner <- 0L
        } else {
          RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
        }
      }
      
      # Update bandit probabilities and whatnot----
      RL_Update_Output <- RL_ML_Update(
        ExperimentGrid = ExperimentalGrid,
        ModelRun = counter,
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
      
      # Binary Remove Model and Collect Garbage----
      rm(model)
      gc()
      
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
    if(tolower(task_type) == "gpu") {
      base_params <- list(
        has_time             = HasTime,
        metric_period        = MetricPeriods,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        task_type            = task_type,
        class_weights        = ClassWeights,
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
        metric_period        = MetricPeriods,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        task_type            = task_type,
        train_dir            = model_path,
        class_weights        = ClassWeights,
        iterations           = PassInGrid[["TreesBuilt"]],
        depth                = PassInGrid[["Depth"]],
        learning_rate        = PassInGrid[["LearningRate"]],
        l2_leaf_reg          = PassInGrid[["L2_Leaf_Reg"]],
        rsm                  = PassInGrid[["RSM"]],
        bootstrap_type       = PassInGrid[["BootStrapType"]])
    }
  }
  
  # Define parameters for case where you want to run grid tuning----
  if(GridTune & TrainOnFull == FALSE) {
    
    # Prepare winning grid----
    BestGrid <- ExperimentalGrid[order(-EvalMetric)][1L]
    if(tolower(task_type) == "gpu") grid_params <- as.list(BestGrid[, c(5L:12L)]) else grid_params <- as.list(BestGrid[, c(5L:11L)])
    if(tolower(task_type) == "gpu") grid_params <- grid_params[!names(grid_params) %chin% "RSM"]
    if(tolower(task_type) == "cpu") grid_params <- grid_params[!names(grid_params) %chin% "GrowPolicy"]
    
    # Set parameters from winning grid----
    if(BestGrid$RunNumber == 1L) {
      if(!is.null(ClassWeights)) {
        base_params <- list(
          use_best_model       = TRUE,
          best_model_min_trees = 10L,
          metric_period        = MetricPeriods,
          iterations           = BestGrid[["TreesBuilt"]],
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          has_time             = HasTime,
          task_type            = task_type,
          class_weights        = ClassWeights)
      } else {
        base_params <- list(
          use_best_model       = TRUE,
          best_model_min_trees = 10L,
          metric_period        = MetricPeriods,
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          has_time             = HasTime,
          task_type            = task_type)
      }
    } else {
      if(tolower(task_type) == "gpu") {
        base_params <- list(
          has_time             = HasTime,
          metric_period        = MetricPeriods,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          best_model_min_trees = 10L,
          task_type            = task_type,
          class_weights        = ClassWeights,
          train_dir            = model_path,
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
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          best_model_min_trees = 10L,
          task_type            = task_type,
          train_dir            = model_path,
          class_weights        = ClassWeights,
          iterations           = BestGrid[["NTrees"]],
          depth                = BestGrid[["Depth"]],
          learning_rate        = BestGrid[["LearningRate"]],
          l2_leaf_reg          = BestGrid[["L2_Leaf_Reg"]],
          rsm                  = BestGrid[["RSM"]],
          bootstrap_type       = BestGrid[["BootStrapType"]])
      }
    }
  }
  
  # Define parameters Not pass in GridMetric and not grid tuning----
  if(is.null(PassInGrid) & GridTune == FALSE) {
    if(!is.null(ClassWeights)) {
      base_params <- list(
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        metric_period        = MetricPeriods,
        iterations           = Trees,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        has_time             = HasTime,
        task_type            = task_type,
        class_weights        = ClassWeights)
    } else {
      base_params <- list(
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        metric_period        = MetricPeriods,
        iterations           = Trees,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        has_time             = HasTime,
        task_type            = task_type)
    }
  }
  
  # MultiClass Train Final Model----
  if(TrainOnFull) {
    model <- catboost::catboost.train(learn_pool = TrainPool, params = base_params)
  } else {
    model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params)
  }
  
  # MultiClass Save Model----
  if (SaveModelObjects) catboost::catboost.save_model(model = model, model_path = file.path(model_path, ModelID))
  
  # MultiClass Score Final Test Data----
  if(!is.null(TestData)) {
    predict <- cbind(
      1 + catboost::catboost.predict(
        model = model,
        pool = FinalTestPool,
        prediction_type = "Class"),
      catboost::catboost.predict(
        model = model,
        pool = FinalTestPool,
        prediction_type = "Probability"))
  } else if(!TrainOnFull) {
    predict <- cbind(
      1 + catboost::catboost.predict(
        model = model,
        pool = TestPool,
        prediction_type = "Class"),
      catboost::catboost.predict(
        model = model,
        pool = TestPool,
        prediction_type = "Probability"))
  } else {
    predict <- cbind(
      1 + catboost::catboost.predict(
        model = model,
        pool = TrainPool,
        prediction_type = "Class"),
      catboost::catboost.predict(
        model = model,
        pool = TrainPool,
        prediction_type = "Probability"))
  }
  
  # MultiClass Grid Validation Data----
  if(!is.null(TestData)) {
    ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget, predict, TestMerge[, .SD, .SDcols = unique(names(TestMerge)[c(1L:(ncol(TestMerge)-1L))])]))
  } else if(!TrainOnFull) {
    ValidationData <- data.table::as.data.table(cbind(Target = TestTarget, predict))
  } else {
    ValidationData <- data.table::as.data.table(cbind(Target = TrainTarget, predict))
  }
  if(TrainOnFull) {
    ValidationData <- merge(
      ValidationData,
      TargetLevels,
      by.x = "Target",
      by.y = "NewLevels",
      all = FALSE)
    ValidationData[, Target := OriginalLevels][, OriginalLevels := NULL]
    ValidationData <- merge(
      ValidationData,
      TargetLevels,
      by.x = "V2",
      by.y = "NewLevels",
      all = FALSE)
    ValidationData[, V2 := OriginalLevels][, OriginalLevels := NULL] 
  } else {
    ValidationData <- merge(
      ValidationData,
      TargetLevels,
      by.x = "V1",
      by.y = "NewLevels",
      all = FALSE)
    ValidationData[, V1 := OriginalLevels][, OriginalLevels := NULL]
    ValidationData <- merge(
      ValidationData,
      TargetLevels,
      by.x = "Target",
      by.y = "NewLevels",
      all = FALSE)
    ValidationData[, Target := OriginalLevels][, OriginalLevels := NULL]  
  }
  
  # MultiClass Update Names for Predicted Value Columns----
  if(!TrainOnFull) k <- 1L else k <- 2L
  for(name in as.character(TargetLevels[[1L]])) {
    k <- k + 1L
    data.table::setnames(ValidationData, paste0("V", k), name)
  }
  if(!TrainOnFull) data.table::setnames(ValidationData, "V1", "Predict") else data.table::setnames(ValidationData, "V2", "Predict")
  if(!TrainOnFull) {
    data.table::setnames(ValidationData, "Target", eval(TargetColumnName))
    data.table::set(ValidationData, j = eval(TargetColumnName), value = as.character(ValidationData[[eval(TargetColumnName)]]))
  } else {
    data.table::setnames(ValidationData, "Target", eval(TargetColumnName))
    data.table::set(ValidationData, j = eval(TargetColumnName), value = as.character(ValidationData[[eval(TargetColumnName)]]))
  }
  data.table::set(ValidationData, j = "Predict", value = as.character(ValidationData[["Predict"]]))
  
  # MultiClass Metrics Accuracy----
  MetricAcc <- ValidationData[, mean(data.table::fifelse(as.character(Target) == as.character(Predict), 1.0, 0.0), na.rm = TRUE)]
  
  # MultiClass Metrics MicroAUC----
  MetricAUC <- round(as.numeric(noquote(stringr::str_extract(pROC::multiclass.roc(response = ValidationData[[eval(TargetColumnName)]], predictor = as.matrix(ValidationData[, .SD, .SDcols = unique(names(ValidationData)[3L:(ncol(predict)+1L)])]))$auc, "\\d+\\.*\\d*"))), 4L)
  
  # Logloss----
  if(!TrainOnFull) temp <- ValidationData[, 1L] else temp <- ValidationData[, 2L]
  temp[, Truth := get(TargetColumnName)]
  temp <- DummifyDT(
    data = temp, 
    cols = eval(TargetColumnName), 
    KeepFactorCols = FALSE, 
    OneHot = FALSE, 
    SaveFactorLevels = FALSE, 
    SavePath = NULL, 
    ImportFactorLevels = FALSE,
    FactorLevelsList = NULL, 
    ClustScore = FALSE, 
    ReturnFactorLevels = FALSE)
  N <- TargetLevels[, .N]
  logloss <- MLmetrics::LogLoss(y_pred = as.matrix(ValidationData[, 3L:(2L+N)]), y_true = as.matrix(temp[, 2L:(1L+N)]))
  
  # MultiClass Save Validation Data to File----
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(ValidationData, file = file.path(metadata_path, paste0(ModelID, "_ValidationData.csv")))
    } else {
      data.table::fwrite(ValidationData, file = file.path(model_path, paste0(ModelID, "_ValidationData.csv")))
    }
  }
  
  # MultiClass Evaluation Metrics----
  if(!TrainOnFull) {
    EvaluationMetrics <- data.table::data.table(Metric = c("AUC", "Accuracy", "LogLoss"), MetricValue = c(MetricAUC, MetricAcc, logloss))
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvaluationMetrics, file = file.path(metadata_path, paste0(ModelID, "_EvaluationMetrics.csv")))
      } else {
        data.table::fwrite(EvaluationMetrics, file = file.path(model_path, paste0(ModelID, "_EvaluationMetrics.csv")))
      }
    }
  }
  
  # MultiClass Variable Importance----
  temp <- catboost::catboost.get_feature_importance(model)
  VariableImportance <- data.table::data.table(cbind(Variable = rownames(temp), temp))
  data.table::setnames(VariableImportance, "V2", "Importance")
  VariableImportance[, Importance := round(as.numeric(Importance), 4L)]
  VariableImportance <- VariableImportance[order(-Importance)]
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(VariableImportance, file = file.path(metadata_path, paste0(ModelID, "_VariableImportance.csv")))
    } else {
      data.table::fwrite(VariableImportance, file = file.path(model_path, paste0(ModelID, "_VariableImportance.csv")))
    }
  }
  
  # MultiClass Save Grid output----
  if(SaveModelObjects & GridTune) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(ExperimentalGrid, file = file.path(metadata_path, paste0(ModelID, "_ExperimentalGrid.csv")))
    } else {
      data.table::fwrite(ExperimentalGrid, file = file.path(model_path, paste0(ModelID, "_ExperimentalGrid.csv")))
    }
  }
  
  # Final Garbage Collection----
  if(tolower(task_type) == "gpu") gc()
  
  # VI_Plot_Function
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

  # MultiClass Return Model Objects----
  if(!TrainOnFull) {
    if(ReturnModelObjects) {
      return(list(
        Model = model,
        ValidationData = ValidationData,
        EvaluationMetrics = EvaluationMetrics,
        VariableImportance = VariableImportance,
        VI_Plot = tryCatch({VI_Plot(VariableImportance)}, error = NULL),
        GridMetrics = if(exists("ExperimentalGrid")) data.table::setorderv(ExperimentalGrid, cols = "EvalMetric", order = 1L, na.last = TRUE) else NULL,
        ColNames = Names,
        TargetLevels = TargetLevels))
    }
  } else {
    if(ReturnModelObjects) {
      return(list(
        Model = model,,
        ColNames = Names,
        TargetLevels = TargetLevels))
    }
  }
}
