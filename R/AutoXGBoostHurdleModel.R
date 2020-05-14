#' AutoXGBoostHurdleModel is generalized hurdle modeling framework
#' 
#' AutoXGBoostHurdleModel is generalized hurdle modeling framework
#'
#' @family Automated Regression
#' @author Adrian Antico
#' @param TrainOnFull Set to TRUE to train model on 100 percent of data
#' @param grid_eval_metric Select the metric to optimize in grid tuning. "accuracy", "microauc", "logloss"
#' @param BaselineComparison "default"
#' @param MaxRunsWithoutNewWinner Number of runs without a new winner before stopping the grid tuning
#' @param MaxRunMinutes Max number of minutes to allow the grid tuning to run for
#' @param eta Learning rates seq(0.05,0.40,0.05)
#' @param max_depth Depth seq(4L, 16L, 2L)
#' @param min_child_weight seq(1.0, 10.0, 1.0)
#' @param subsample seq(0.55, 1.0, 0.05)
#' @param colsample_bytree seq(0.55, 1.0, 0.05) 
#' @param data Source training data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param ValidationData Source validation data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param TestData Souce test data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param Buckets A numeric vector of the buckets used for subsetting the data. NOTE: the final Bucket value will first create a subset of data that is less than the value and a second one thereafter for data greater than the bucket value.
#' @param TargetColumnName Supply the column name or number for the target variable
#' @param FeatureColNames Supply the column names or number of the features (not included the PrimaryDateColumn)
#' @param IDcols Includes PrimaryDateColumn and any other columns you want returned in the validation data with predictions
#' @param TransformNumericColumns Transform numeric column inside the AutoCatBoostRegression() function
#' @param SplitRatios Supply vector of partition ratios. For example, c(0.70,0.20,0,10).
#' @param TreeMethod Set to hist or gpu_hist depending on if you have an xgboost installation capable of gpu processing
#' @param NThreads Set to the number of threads you would like to dedicate to training
#' @param ModelID Define a character name for your models
#' @param Paths The path to your folder where you want your model information saved
#' @param MetaDataPaths A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to Paths.
#' @param SaveModelObjects Set to TRUE to save the model objects to file in the folders listed in Paths
#' @param Trees Default 1000
#' @param GridTune Set to TRUE if you want to grid tune the models
#' @param MaxModelsInGrid Set to a numeric value for the number of models to try in grid tune
#' @param NumOfParDepPlots Set to pull back N number of partial dependence calibration plots.
#' @param PassInGrid Pass in a grid for changing up the parameter settings for catboost
#' @return Returns AutoXGBoostRegression() model objects: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and the grid used
#' @examples
#' \donttest{
#' Output <- RemixAutoML::AutoXGBoostHurdleModel(
#' 
#'    # Operationalization args
#'    TreeMethod = "hist",
#'    TrainOnFull = FALSE,
#'    PassInGrid = NULL,
#' 
#'    # Metadata args
#'    NThreads = max(1L, parallel::detectCores()-2L),
#'    ModelID = "ModelTest",
#'    Paths = normalizePath("./"),
#'    MetaDataPaths = NULL,
#' 
#'    # data args
#'    data,
#'    ValidationData = NULL,
#'    TestData = NULL,
#'    Buckets = 0L,
#'    TargetColumnName = NULL,
#'    FeatureColNames = NULL,
#'    IDcols = NULL,
#' 
#'    # options
#'    TransformNumericColumns = NULL,
#'    SplitRatios = c(0.70, 0.20, 0.10),
#'    SaveModelObjects = FALSE,
#'    NumOfParDepPlots = 10L,
#' 
#'    # grid tuning args
#'    GridTune = FALSE,
#'    grid_eval_metric = "accuracy",
#'    MaxModelsInGrid = 1L,
#'    BaselineComparison = "default",
#'    MaxRunsWithoutNewWinner = 10L,
#'    MaxRunMinutes = 60L,
#' 
#'    # bandit hyperparameters
#'    Trees = 1000L,
#'    eta = seq(0.05,0.40,0.05),
#'    max_depth = seq(4L, 16L, 2L),
#' 
#'    # random hyperparameters
#'    min_child_weight = seq(1.0, 10.0, 1.0),
#'    subsample = seq(0.55, 1.0, 0.05),
#'    colsample_bytree = seq(0.55, 1.0, 0.05))
#' }
#' @export
AutoXGBoostHurdleModel <- function(TreeMethod = "hist",
                                   TrainOnFull = FALSE,
                                   PassInGrid = NULL,
                                   NThreads = max(1L, parallel::detectCores()-2L),
                                   ModelID = "ModelTest",
                                   Paths = NULL,
                                   MetaDataPaths = NULL,
                                   data,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   Buckets = 0L,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   IDcols = NULL,
                                   TransformNumericColumns = NULL,
                                   SplitRatios = c(0.70, 0.20, 0.10),
                                   SaveModelObjects = FALSE,
                                   NumOfParDepPlots = 10L,
                                   GridTune = FALSE,
                                   grid_eval_metric = "accuracy",
                                   MaxModelsInGrid = 1L,
                                   BaselineComparison = "default",
                                   MaxRunsWithoutNewWinner = 10L,
                                   MaxRunMinutes = 60L,
                                   Trees = 1000L,
                                   eta = seq(0.05,0.40,0.05),
                                   max_depth = seq(4L, 16L, 2L),
                                   min_child_weight = seq(1.0, 10.0, 1.0),
                                   subsample = seq(0.55, 1.0, 0.05),
                                   colsample_bytree = seq(0.55, 1.0, 0.05)) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(percent = 100L)
  
  # Check args----
  if (is.character(Buckets) | is.factor(Buckets) | is.logical(Buckets)) return("Buckets needs to be a numeric scalar or vector")
  if (!is.logical(SaveModelObjects)) return("SaveModelOutput needs to be set to either TRUE or FALSE")
  if (is.character(Trees) | is.factor(Trees) | is.logical(Trees) | length(Trees) > 1L) return("NumTrees needs to be a numeric scalar")
  if(!GridTune & length(Trees) > 1L) Trees <- Trees[length(Trees)]
  if (!is.logical(GridTune)) return("GridTune needs to be either TRUE or FALSE")
  if (is.character(MaxModelsInGrid) | is.factor(MaxModelsInGrid) | is.logical(MaxModelsInGrid) | length(MaxModelsInGrid) > 1L) return("NumberModelsInGrid needs to be a numeric scalar")
  
  # Initialize collection and counter----
  ModelInformationList <- list()
  if(!is.null(Paths)) if (length(Paths) == 1L) Paths <- rep(Paths, length(Buckets) + 1L)
  if(!is.null(MetaDataPaths)) if (length(MetaDataPaths) == 1L) MetaDataPaths <- rep(MetaDataPaths, length(Buckets) + 1L)

  # Data.table check----
  if (!data.table::is.data.table(data)) data.table::setDT(data)
  if (!is.null(ValidationData)) if (!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)
  if (!is.null(TestData)) if (!data.table::is.data.table(TestData)) data.table::setDT(TestData)
  
  # IDcols to Names----
  if (!is.null(IDcols)) if (is.numeric(IDcols) | is.integer(IDcols)) IDcols <- names(data)[IDcols]
  
  # FeatureColumnNames----
  if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) FeatureNames <- names(data)[FeatureColNames] else FeatureNames <- FeatureColNames
  
  # Add target bucket column----
  if(length(Buckets) == 1L) {
    data.table::set(data, i = which(data[[eval(TargetColumnName)]] <= Buckets[1L]), j = "Target_Buckets", value = 0L)
    data.table::set(data, i = which(data[[eval(TargetColumnName)]] > Buckets[1L]), j = "Target_Buckets", value = 1L)
  } else {
    for (i in seq_len(length(Buckets) + 1L)) {
      if (i == 1L) {
        data.table::set(data, i = which(data[[eval(TargetColumnName)]] <= Buckets[i]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      } else if (i == length(Buckets) + 1L) {
        data.table::set(data, i = which(data[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(paste0(Buckets[i-1L], "+")))
      } else {
        data.table::set(data, i = which(data[[eval(TargetColumnName)]] <= Buckets[i] & data[[eval(TargetColumnName)]] > Buckets[i-1L]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      }      
    }
  }
  
  # Add target bucket column----
  if (!is.null(ValidationData)) {
    ValidationData[, Target_Buckets := as.factor(Buckets[1])]
    for (i in seq_len(length(Buckets) + 1L)) {
      if (i == 1L) {
        data.table::set(ValidationData, i = which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      } else if (i == length(Buckets) + 1L) {
        data.table::set(ValidationData, i = which(ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(paste0(Buckets[i - 1L], "+")))
      } else {
        data.table::set(ValidationData, i = which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i] & ValidationData[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      }
    }
  }
  
  # Add target bucket column----
  if (!is.null(TestData)) {
    TestData[, Target_Buckets := as.factor(Buckets[1L])]
    for (i in seq_len(length(Buckets) + 1L)) {
      if (i == 1L) {
        data.table::set(TestData, i = which(TestData[[eval(TargetColumnName)]] <= Buckets[i]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      } else if (i == length(Buckets) + 1L) {
        data.table::set(TestData, i = which(TestData[[eval(TargetColumnName)]] > Buckets[i-1L]), j = "Target_Buckets", value = as.factor(paste0(Buckets[i - 1L], "+")))
      } else {
        data.table::set(TestData, i = which(TestData[[eval(TargetColumnName)]] <= Buckets[i] & TestData[[eval(TargetColumnName)]] > Buckets[i - 1L]), j = "Target_Buckets", value = as.factor(Buckets[i]))
      }
    }
  }
  
  # AutoDataPartition if Validation and TestData are NULL----
  if (is.null(ValidationData) & is.null(TestData)) {
    DataSets <- AutoDataPartition(
      data = data,
      NumDataSets = 3L,
      Ratios = SplitRatios,
      PartitionType = "random",
      StratifyColumnNames = "Target_Buckets",
      TimeColumnName = NULL)
    data <- DataSets$TrainData
    ValidationData <- DataSets$ValidationData
    TestData <- DataSets$TestData
    rm(DataSets)
  }
  
  # Begin classification model building----
  if (length(Buckets) == 1L) {
    ClassifierModel <- AutoXGBoostClassifier(
      
      # general args----
      eval_metric = "auc",
      TrainOnFull = TrainOnFull,
      TreeMethod = TreeMethod,
      PassInGrid = PassInGrid,
      NThreads = NThreads,
      model_path = Paths[1L],
      metadata_path = MetaDataPaths[1L],
      ModelID = ModelID,
      
      # options----
      ReturnModelObjects = TRUE,
      ReturnFactorLevels = TRUE,
      Verbose = 1L,
      NumOfParDepPlots = NumOfParDepPlots,
      SaveModelObjects = SaveModelObjects,
      
      # data args----
      TargetColumnName = "Target_Buckets",
      data = data,
      ValidationData = ValidationData,
      TestData = TestData,
      FeatureColNames = FeatureNames,
      IDcols = IDcols,
      
      # Grid tuning args----
      GridTune = GridTune,
      BaselineComparison = BaselineComparison,
      MaxModelsInGrid = MaxModelsInGrid,
      MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
      MaxRunMinutes = MaxRunMinutes,
      Shuffles = 2L,
      
      # bandit args----
      Trees = Trees,
      eta = eta,
      max_depth = max_depth,
      
      # random args----
      min_child_weight = min_child_weight,
      subsample = subsample,
      colsample_bytree = colsample_bytree)
    
  } else {
    ClassifierModel <- AutoXGBoostMultiClass(
      
      # type
      grid_eval_metric = grid_eval_metric,
      eval_metric = "merror",
      Objective = 'multi:softprob',

            
      # general args----
      TrainOnFull = TrainOnFull,
      TreeMethod = TreeMethod,
      PassInGrid = PassInGrid,
      NThreads = NThreads,
      model_path = Paths[1L],
      metadata_path = MetaDataPaths[1L],
      ModelID = ModelID,
      
      # options----
      ReturnModelObjects = TRUE,
      ReturnFactorLevels = TRUE,
      NumOfParDepPlots = NumOfParDepPlots,
      SaveModelObjects = SaveModelObjects,
      Verbose = 1L,
      
      # data args----
      data = data,
      ValidationData = ValidationData,
      TestData = TestData,
      TargetColumnName = "Target_Buckets",
      FeatureColNames = FeatureNames,
      IDcols = IDcols,
      
      # Grid tuning args----
      GridTune = GridTune,
      BaselineComparison = BaselineComparison,
      MaxModelsInGrid = MaxModelsInGrid,
      MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
      MaxRunMinutes = MaxRunMinutes,
      Shuffles = 2L,
      
      # bandit args----
      Trees = Trees,
      eta = eta,
      max_depth = max_depth,
      
      # random args----
      min_child_weight = min_child_weight,
      subsample = subsample,
      colsample_bytree = colsample_bytree)
  }
  
  # Store metadata----
  ModelList <- list()
  ClassModel <- ClassifierModel$Model
  ModelList[["ClassificationModel"]] <- ClassModel
  ClassEvaluationMetrics <- ClassifierModel$EvaluationMetrics
  VariableImportance <- ClassifierModel$VariableImportance
  if(length(Buckets > 1L)) TargetLevels <- ClassifierModel$TargetLevels else TargetLevels <- NULL
  FactorLevelsListOutput <- ClassifierModel$FactorLevels
  if(!is.null(FactorLevelsListOutput)) FactorLevelsList <- FactorLevelsListOutput else FactorLevelsList <- NULL
  rm(ClassifierModel)
  
  # Add Target to IDcols----
  IDcols <- c(IDcols, TargetColumnName)
  
  # Define args----
  if (length(Buckets) == 1L) {
    TargetType <- "Classification"
    Objective <- NULL
  } else {
    TargetType <- "Multiclass"
    Objective <- "multi:softprob"
  }
  
  # Model Scoring----
  TestData <- AutoXGBoostScoring(
    TargetType = TargetType,
    ScoringData = TestData,
    FeatureColumnNames = FeatureNames,
    IDcols = IDcols,
    FactorLevelsList = FactorLevelsList,
    TargetLevels = TargetLevels,
    Objective = Objective,
    OneHot = FALSE,
    ModelObject = ClassModel,
    ModelPath = NULL,
    ModelID = ModelID,
    ReturnFeatures = TRUE,
    TransformNumeric = FALSE,
    BackTransNumeric = FALSE,
    TargetColumnName = NULL,
    TransformationObject = NULL,
    TransID = NULL,
    TransPath = NULL,
    MDP_Impute = TRUE,
    MDP_CharToFactor = TRUE,
    MDP_RemoveDates = TRUE,
    MDP_MissFactor = "0",
    MDP_MissNum = -1)
  
  # Change name for classification----
  if(TargetType == "Classification") {
    data.table::setnames(TestData, "Predictions","Predictions_C1")
    TestData[, Predictions_C0 := 1 - Predictions_C1]
    data.table::setcolorder(TestData, c(ncol(TestData),1L, 2L:(ncol(TestData)-1L)))
  }

  # Remove Model Object----
  rm(ClassModel)
  
  # Remove Target_Buckets----
  data[, Target_Buckets := NULL]
  ValidationData[, Target_Buckets := NULL]
  
  # Remove Target From IDcols----
  IDcols <- IDcols[!(IDcols %chin% TargetColumnName)]
  
  # Begin regression model building----
  counter <- 0L
  Degenerate <- 0L
  for(bucket in rev(seq_len(length(Buckets) + 1L))) {
    
    # Partition data----
    if (bucket == max(seq_len(length(Buckets) + 1L))) {
      if (!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- TestData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
      } else {
        trainBucket <- data[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- NULL
      }
    } else if (bucket == 1L) {
      if (!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket])]
        testBucket <- TestData[get(TargetColumnName) <= eval(Buckets[bucket])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
      } else {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket])]
        testBucket <- NULL
      }
    } else {
      if (!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])] 
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- TestData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        data.table::set(testBucket, j = setdiff(names(testBucket), names(data)), value = NULL)
      } else {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        validBucket <- ValidationData[get(TargetColumnName) <= eval(Buckets[bucket]) & get(TargetColumnName) > eval(Buckets[bucket - 1L])]
        testBucket <- NULL
      }
    }
    
    # Create Modified IDcols----
    IDcolsModified <- unique(c(IDcols, setdiff(names(TestData), names(trainBucket)), TargetColumnName))
    
    # Load Winning Grid if it exists----
    if (file.exists(paste0(Paths[bucket], "/grid", Buckets[bucket], ".csv"))) gridSaved <- data.table::fread(paste0(Paths[bucket], "/grid", Buckets[bucket], ".csv"))
    if (file.exists(paste0(MetaDataPaths[bucket], "/grid", Buckets[bucket], ".csv"))) gridSaved <- data.table::fread(paste0(MetaDataPaths[bucket], "/grid", Buckets[bucket], ".csv"))

    # AutoCatBoostRegression()----
    if(trainBucket[, .N] != 0L) {
      if(var(trainBucket[[eval(TargetColumnName)]]) > 0L) {
        
        # Increment----
        counter <- counter + 1L
        
        # Define ModelIDD----
        if (bucket == max(seq_len(length(Buckets) + 1L))) ModelIDD <- paste0(ModelID,"_",bucket-1L,"+") else ModelIDD <- paste0(ModelID, "_", bucket)
        
        # Build model----
        TestModel <- RemixAutoML::AutoXGBoostRegression(
          TrainOnFull = TrainOnFull,
          data = trainBucket,
          ValidationData = validBucket,
          TestData = testBucket,
          TargetColumnName = TargetColumnName,
          FeatureColNames = FeatureNames,
          IDcols = IDcols,
          ReturnFactorLevels = TRUE,
          TransformNumericColumns = TransformNumericColumns,
          NThreads = NThreads,
          model_path = Paths[1L],
          metadata_path = MetaDataPaths[1L],
          ModelID = ModelIDD,
          NumOfParDepPlots = NumOfParDepPlots,
          Verbose = 1L,
          ReturnModelObjects = TRUE,
          SaveModelObjects = SaveModelObjects,
          PassInGrid = PassInGrid,
          GridTune = GridTune,
          grid_eval_metric = "mse",
          eval_metric = "rmse",
          Trees = Trees,
          TreeMethod = TreeMethod,
          MaxModelsInGrid = MaxModelsInGrid,
          BaselineComparison = "default",
          MaxRunsWithoutNewWinner = 20L,
          MaxRunMinutes = 60*60, 
          Shuffles = 2L, 
          eta = eta, 
          max_depth = max_depth, 
          min_child_weight = min_child_weight, 
          subsample = subsample, 
          colsample_bytree = colsample_bytree)

        # Store Model----
        RegressionModel <- TestModel$Model
        ModelList[[paste0("RegressionModel_",bucket)]] <- RegressionModel
        FactorLevelsListOutput <- TestModel$FactorLevelsList
        if(!is.null(TransformNumericColumns)) TransformationResults <- TestModel[["TransformationResults"]]
        if(!is.null(FactorLevelsListOutput)) FactorLevelsList <- FactorLevelsListOutput else FactorLevelsList <- NULL
        
        # Garbage Collection----
        gc()
          
        # Define TranformationResults----
        if(!is.null(TransformNumericColumns)) Trans <- TransformationResults  else TransformationResults <- NULL
          
        # Score model----
        if(!is.null(TransformNumericColumns)) {
          TestData <- AutoXGBoostScoring(
            TargetType = "regression",
            ScoringData = TestData,
            FeatureColumnNames = FeatureNames,
            IDcols = IDcolsModified,
            FactorLevelsList = FactorLevelsList,
            OneHot = FALSE,
            ModelObject = RegressionModel,
            ModelPath = Paths[1L],
            ModelID = ModelIDD,
            ReturnFeatures = TRUE,
            TransformNumeric = TRUE,
            BackTransNumeric = TRUE,
            TargetColumnName = eval(TargetColumnName),
            TransformationObject = TransformationResults,
            TransID = NULL,
            TransPath = NULL,
            MDP_Impute = TRUE,
            MDP_CharToFactor = TRUE,
            MDP_RemoveDates = FALSE,
            MDP_MissFactor = "0",
            MDP_MissNum = -1)
        } else {
          TestData <- AutoXGBoostScoring(
            TargetType = "regression",
            ScoringData = TestData,
            FeatureColumnNames = FeatureNames,
            IDcols = IDcolsModified,
            FactorLevelsList = FactorLevelsList,
            OneHot = FALSE,
            ModelObject = RegressionModel,
            ModelPath = Paths[1L],
            ModelID = ModelIDD,
            ReturnFeatures = TRUE,
            TransformNumeric = FALSE,
            BackTransNumeric = FALSE,
            TargetColumnName = NULL,
            TransformationObject = NULL,
            TransID = NULL,
            TransPath = NULL,
            MDP_Impute = TRUE,
            MDP_CharToFactor = TRUE,
            MDP_RemoveDates = FALSE,
            MDP_MissFactor = "0",
            MDP_MissNum = -1)
        }
          
        # Clear TestModel From Memory----
        rm(TestModel, RegressionModel)
        
        # Change prediction name to prevent duplicates----
        if (bucket == max(seq_len(length(Buckets) + 1L))) {
          data.table::setnames(TestData, "Predictions", paste0("Predictions_", Buckets[bucket - 1L], "+"))
        } else {
          data.table::setnames(TestData, "Predictions", paste0("Predictions_", Buckets[bucket]))
        }
        
      } else {
        
        # Use single value for predictions in the case of zero variance----
        if (bucket == max(seq_len(length(Buckets) + 1L))) {
          Degenerate <- Degenerate + 1L
          data.table::set(TestData, j = paste0("Predictions_", Buckets[bucket - 1L], "+"), value = Buckets[bucket])
          data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData)-1L)))
        } else {
          Degenerate <- Degenerate + 1L
          data.table::set(TestData, j = paste0("Predictions_", Buckets[bucket]), value = Buckets[bucket])
          data.table::setcolorder(TestData, c(ncol(TestData), 1L:(ncol(TestData)-1L)))
        }
      }
    }
  }
  
  # Rearrange Column order----
  if(counter > 2L) {
    if(length(IDcols) != 0L) {
      if(Degenerate == 0L) {
        data.table::setcolorder(TestData, c(1L, (2L + length(IDcols)):(2L + length(IDcols) + 2L*counter-1L), 2L:(1L + length(IDcols)), (3L + length(IDcols) + 2L * counter - 1L):ncol(TestData)))
      } else {
        data.table::setcolorder(TestData, c(1L:2L, (3L + length(IDcols)):(3L + length(IDcols) + 2L*counter-1L), 3L:(2L + length(IDcols)), (4L + length(IDcols) + 2L * counter - 1L):ncol(TestData)))
      }
    }
  } else if(counter == 2L & length(Buckets) == 1L) {
    if(length(IDcols) != 0L) {
      data.table::setcolorder(TestData, c(1L, (2L+length(IDcols)):(4+length(IDcols)), 2L:(1L+length(IDcols)), (5L+length(IDcols)):ncol(TestData)))
    } 
  } else if(counter == 2L & length(Buckets) != 1L) {
    if(length(IDcols) != 0L) {
      if(Degenerate == 0L) {
        data.table::setcolorder(TestData, c(ncol(TestData), 1L, (2L + length(IDcols)):(3L + length(IDcols) + 2L*counter-2L), 2L:(1L + length(IDcols)), (3L + length(IDcols) + 2L * counter - 1L):(ncol(TestData)-1L)))
      } else {
        data.table::setcolorder(TestData, c(1L:2L, (3L + length(IDcols)):(4L + length(IDcols) + 2L*counter-2L), 3L:(2L + length(IDcols)), (4L + length(IDcols) + 2L * counter - 1L):(ncol(TestData)-1L)))
      }
    } else {
      data.table::setcolorder(TestData, c(ncol(TestData),1L:(ncol(TestData)-1L)))
    }
  } else {
    if(length(IDcols) != 0L) {
      data.table::setcolorder(TestData, c(1L:2L, (3L+length(IDcols)):(4L+length(IDcols)), 3L:(2L+length(IDcols)), (5L+length(IDcols)):ncol(TestData)))
    } 
  }

  # Final Combination of Predictions----
  # Logic: 1 Buckets --> 4 columns of preds
  #        2 Buckets --> 6 columns of preds
  #        3 Buckets --> 8 columns of preds
  # Secondary logic: for i == 1, need to create the final column first
  #                  for i > 1, need to take the final column and add the product of the next preds
  Cols <- ncol(TestData)
  if(counter > 2L) {
    for (i in seq_len(length(Buckets)+1)) {
      if (i == 1L) {
        data.table::set(TestData, j = "UpdatedPrediction", value = TestData[[i]] * TestData[[i + counter + Degenerate]])
      } else {
        data.table::set(TestData, j = "UpdatedPrediction", value = TestData[["UpdatedPrediction"]] + TestData[[i]] * TestData[[i + counter + Degenerate]])
      }
    }  
  } else if(counter == 2L & length(Buckets) != 1L) {
    for (i in seq_len(length(Buckets)+1)) {
      if (i == 1L) {
        data.table::set(TestData, j = "UpdatedPrediction", value = TestData[[i]] * TestData[[i + 1L + counter]])
      } else {
        data.table::set(TestData, j = "UpdatedPrediction", value = TestData[["UpdatedPrediction"]] + TestData[[i]] * TestData[[i + 1L + counter]])
      }
    }  
  } else if(counter == 2L & length(Buckets) == 1L) {
    data.table::set(TestData, j = "UpdatedPrediction", value = TestData[[1]] * TestData[[3]] + TestData[[2L]] * TestData[[4L]])
  } else {
    data.table::set(TestData, j = "UpdatedPrediction", value = TestData[[1L]] * TestData[[3L]] + TestData[[2L]] * TestData[[4L]])
  }
  
  # R-Sq----
  r_squared <- (TestData[, stats::cor(get(TargetColumnName), UpdatedPrediction)]) ^ 2L
  
  # Regression Save Validation Data to File----
  if (SaveModelObjects) {
    if(!is.null(MetaDataPaths[1L])) {
      data.table::fwrite(TestData, file = paste0(MetaDataPaths[1L], "/", ModelID, "_ValidationData.csv"))
    } else {
      data.table::fwrite(TestData, file = paste0(Paths[1L], "/", ModelID, "_ValidationData.csv"))      
    }
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = TestData,
    PredictionColName = "UpdatedPrediction",
    TargetColName = eval(TargetColumnName),
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x) mean(x, na.rm = TRUE))
  
  # Add Number of Trees to Title----
  EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(r_squared, 3L)))
  
  # Save plot to file----
  if (SaveModelObjects) {
    if(!is.null(MetaDataPaths[1L])) {
      ggplot2::ggsave(paste0(MetaDataPaths[1L], "/", ModelID, "_EvaluationPlot.png"))
    } else {
      ggplot2::ggsave(paste0(Paths[1L], "/", ModelID, "_EvaluationPlot.png"))      
    }
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationBoxPlot <- EvalPlot(
    data = TestData,
    PredictionColName = "UpdatedPrediction",
    TargetColName = eval(TargetColumnName),
    GraphType = "boxplot",
    PercentileBucket = 0.05,
    aggrfun = function(x) mean(x, na.rm = TRUE))
  
  # Add Number of Trees to Title----
  EvaluationBoxPlot <- EvaluationBoxPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(r_squared, 3L)))
  
  # Save plot to file----
  if (SaveModelObjects) {
    if(!is.null(MetaDataPaths[1L])) {
      ggplot2::ggsave(paste0(MetaDataPaths[1L], "/", ModelID, "_EvaluationBoxPlot.png"))
    } else {
      ggplot2::ggsave(paste0(Paths[1L], "/", ModelID, "_EvaluationBoxPlot.png"))      
    }
  }
  
  # Regression Evaluation Metrics----
  EvaluationMetrics <- data.table::data.table(Metric = c("Poisson", "MAE","MAPE", "MSE", "MSLE","KL", "CS", "R2"), MetricValue = rep(999999, 8L))
  i <- 0L
  MinVal <- min(TestData[, min(get(TargetColumnName))], TestData[, min(UpdatedPrediction)])
  for (metric in c("poisson", "mae", "mape", "mse", "msle", "kl", "cs", "r2")) {
    i <- as.integer(i + 1L)
    tryCatch({
      if (tolower(metric) == "poisson") {
        if (MinVal > 0L & min(TestData[["UpdatedPrediction"]], na.rm = TRUE) > 0L) {
          TestData[, Metric := UpdatedPrediction - get(TargetColumnName) * log(UpdatedPrediction + 1L)]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "mae") {
        TestData[, Metric := abs(get(TargetColumnName) - UpdatedPrediction)]
        Metric <- TestData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mape") {
        TestData[, Metric := abs((get(TargetColumnName) - UpdatedPrediction) / (get(TargetColumnName) + 1L))]
        Metric <- TestData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mse") {
        TestData[, Metric := (get(TargetColumnName) - UpdatedPrediction) ^ 2L]
        Metric <- TestData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "msle") {
        if (MinVal > 0L & min(TestData[["UpdatedPrediction"]], na.rm = TRUE) > 0L) {
          TestData[, Metric := (log(get(TargetColumnName) + 1) - log(UpdatedPrediction + 1)) ^ 2L]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "kl") {
        if (MinVal > 0L & min(TestData[["UpdatedPrediction"]], na.rm = TRUE) > 0L) {
          TestData[, Metric := get(TargetColumnName) * log((get(TargetColumnName) + 1) / (UpdatedPrediction + 1))]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "cs") {
        TestData[, ':=' (Metric1 = get(TargetColumnName) * UpdatedPrediction, Metric2 = get(TargetColumnName) ^ 2L, Metric3 = UpdatedPrediction ^ 2L)]
        Metric <- TestData[, sum(Metric1, na.rm = TRUE)] / (sqrt(TestData[, sum(Metric2, na.rm = TRUE)]) * sqrt(TestData[, sum(Metric3, na.rm = TRUE)]))
      } else if (tolower(metric) == "r2") {
        TestData[, ':=' (Metric1 = (get(TargetColumnName) - mean(get(TargetColumnName))) ^ 2L, Metric2 = (get(TargetColumnName) - UpdatedPrediction) ^ 2L)]
        Metric <- 1 - TestData[, sum(Metric2, na.rm = TRUE)] / TestData[, sum(Metric1, na.rm = TRUE)]
      }
      data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric, 4L))
      data.table::set(EvaluationMetrics, i = i, j = 3L, value = NA)
    }, error = function(x) "skip")
  }
  
  # Remove Cols----
  TestData[, ':=' (Metric = NULL, Metric1 = NULL, Metric2 = NULL, Metric3 = NULL)]
  
  # Save EvaluationMetrics to File
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999L]
  if (SaveModelObjects) {
    if(!is.null(MetaDataPaths)) {
      data.table::fwrite(EvaluationMetrics, file = paste0(MetaDataPaths[1L], "/", ModelID, "_EvaluationMetrics.csv"))
    } else {
      data.table::fwrite(EvaluationMetrics, file = paste0(Paths[1L], "/", ModelID, "_EvaluationMetrics.csv"))      
    }
  }
  
  # Regression Partial Dependence----
  ParDepPlots <- list()
  j <- 0L
  ParDepBoxPlots <- list()
  k <- 0L
  for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
    tryCatch({
      Out <- ParDepCalPlots(
        data = TestData,
        PredictionColName = "UpdatedPrediction",
        TargetColName = eval(TargetColumnName),
        IndepVar = VariableImportance[i, Variable],
        GraphType = "calibration",
        PercentileBucket = 0.05,
        FactLevels = 10L,
        Function = function(x) mean(x, na.rm = TRUE))
      j <- j + 1L
      ParDepPlots[[paste0(VariableImportance[j, Variable])]] <- Out
    }, error = function(x) "skip")
    tryCatch({
      Out1 <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "UpdatedPrediction",
        TargetColName = eval(TargetColumnName),
        IndepVar = VariableImportance[i, Variable],
        GraphType = "boxplot",
        PercentileBucket = 0.05,
        FactLevels = 10L,
        Function = function(x) mean(x, na.rm = TRUE))
      k <- k + 1L
      ParDepBoxPlots[[paste0(VariableImportance[k, Variable])]] <- Out1
    }, error = function(x) "skip")
  }
  
  # Regression Save ParDepBoxPlots to file----
  if (SaveModelObjects) {
    if(!is.null(MetaDataPaths)) {
      save(ParDepBoxPlots, file = paste0(MetaDataPaths[1L], "/", ModelID, "_ParDepBoxPlots.R"))
    } else {
      save(ParDepBoxPlots, file = paste0(Paths[1L], "/", ModelID, "_ParDepBoxPlots.R"))      
    }
  }

  # Return Output----
  return(
    list(
      ModelList = ModelList,
      ClassificationMetrics = ClassEvaluationMetrics,
      FinalTestData = TestData,
      EvaluationPlot = EvaluationPlot,
      EvaluationBoxPlot = EvaluationBoxPlot,
      EvaluationMetrics = EvaluationMetrics,
      PartialDependencePlots = ParDepPlots,
      PartialDependenceBoxPlots = ParDepBoxPlots))
}
