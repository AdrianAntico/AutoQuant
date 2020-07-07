#' AutoCatBoostRegression is an automated catboost model grid-tuning classifier and evaluation system
#'
#' AutoCatBoostRegression is an automated modeling function that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
#' @author Adrian Antico
#' @family Automated Supervised Learning - Regression
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data and skip over evaluation steps
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param PrimaryDateColumn Supply a date or datetime column for catboost to utilize time as its basis for handling categorical features, instead of random shuffling
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param TransformNumericColumns Set to NULL to do nothing; otherwise supply the column names of numeric variables you want transformed
#' @param Methods Choose from "BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson". Function will determine if one cannot be used because of the underlying data.
#' @param task_type Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param NumGPUs Set to 1, 2, 3, etc.
#' @param eval_metric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "RMSE" is the default, but other options include: "MAE", "MAPE", "Poisson", "Quantile", "LogLinQuantile", "Lq", "NumErrors", "SMAPE", "R2", "MSLE", "MedianAbsoluteError".
#' @param loss_function Used in model training for model fitting. Select from 'RMSE', 'MAE', 'Quantile', 'LogLinQuantile', 'MAPE', 'Poisson', 'PairLogitPairwise', 'Tweedie', 'QueryRMSE'
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
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
#' @param Trees Bandit grid partitioned. The maximum number of trees you want in your models
#' @param Depth Bandit grid partitioned. Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param LearningRate Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param L2_Leaf_Reg Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the L2_Leaf_Reg values to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param RSM CPU only. Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the RSM values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.80, 0.85, 0.90, 0.95, 1.0)
#' @param BootStrapType Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the BootStrapType values to test. For running grid tuning, a NULL value supplied will mean these values are tested c("Bayesian", "Bernoulli", "Poisson", "MVS", "No")
#' @param GrowPolicy Random testing. NULL, character, or vector for GrowPolicy to test. For grid tuning, supply a vector of values. For running grid tuning, a NULL value supplied will mean these values are tested c("SymmetricTree", "Depthwise", "Lossguide")
#' @examples
#' \donttest{
#' # Create some dummy correlated data with numeric and categorical features
#' data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = FALSE, MultiClass = FALSE)
#' 
#' # Run function
#' TestModel <- RemixAutoML::AutoCatBoostRegression(
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
#'     #         'ModelID_VariableImportance.csv' is the variable importance. 
#'     #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
#'     #         'ModelID_ExperimentGrid.csv' if GridTune = TRUE. 
#'     #            Results of all model builds including parameter settings, bandit probs, and grid IDs
#'     #         'ModelID_EvaluationMetrics.csv' which contains MSE, MAE, MAPE, R2
#'     ModelID = "Test_Model_1",
#'     model_path = normalizePath("./"),
#'     metadata_path = NULL,
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
#'     #   'TransformNumericColumns' is for transforming your target variable. Just supply the name of it
#'     data = data,
#'     TrainOnFull = FALSE,
#'     ValidationData = NULL,
#'     TestData = NULL,
#'     TargetColumnName = "Adrian",
#'     FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
#'     PrimaryDateColumn = NULL,
#'     IDcols = c("IDcol_1","IDcol_2"),
#'     TransformNumericColumns = "Adrian",
#'     Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),
#'     
#'     # Model evaluation: 
#'     #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
#'     #   'loss_function' the loss function used in training optimization
#'     #   'NumOfParDepPlots' Number of partial dependence calibration plots generated. 
#'     #     A value of 3 will return plots for the top 3 variables based on variable importance
#'     #     Won't be returned if GrowPolicy is either "Depthwise" or "Lossguide" is used
#'     #     Can run the RemixAutoML::ParDepCalPlots() with the outputted ValidationData
#'     eval_metric = "RMSE",
#'     loss_function = "RMSE",
#'     MetricPeriods = 10L,
#'     NumOfParDepPlots = ncol(data)-1L-2L,
#'     EvalPlots = TRUE,
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
#'     MaxRunsWithoutNewWinner = 100L, 
#'     MaxRunMinutes = 60*60,
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
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, catboostgrid, and a transformation details file.
#' @export
AutoCatBoostRegression <- function(data,
                                   TrainOnFull = FALSE,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   IDcols = NULL,
                                   TransformNumericColumns = NULL,
                                   Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),
                                   task_type = "GPU",
                                   NumGPUs = 1,
                                   eval_metric = "RMSE",
                                   loss_function = "RMSE",
                                   model_path = NULL,
                                   metadata_path = NULL,
                                   ModelID = "FirstModel",
                                   NumOfParDepPlots = 0L,
                                   EvalPlots = TRUE,
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
  if(is.null(loss_function)) LossFunction <- "RMSE" else LossFunction <- loss_function
  
  # Turn on full speed ahead----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))
  
  # Ensure model_path and metadata_path exists----
  if(!is.null(model_path)) if(!dir.exists(file.path(normalizePath(model_path)))) dir.create(normalizePath(model_path))
  if(!is.null(metadata_path)) if(!is.null(metadata_path)) if(!dir.exists(file.path(normalizePath(metadata_path)))) dir.create(normalizePath(metadata_path))
  
  # Regression Check Arguments----
  if(!(tolower(task_type) %chin% c("gpu", "cpu"))) return("task_type needs to be either 'GPU' or 'CPU'")
  if(!is.null(PrimaryDateColumn)) HasTime <- TRUE else HasTime <- FALSE
  if(is.null(NumGPUs)) NumGPUs <- '0' else if(NumGPUs > 1L) NumGPUs <- paste0('0-', NumGPUs-1L) else NumGPUs <- '0'
  if(!GridTune %in% c(TRUE, FALSE)) return("GridTune needs to be TRUE or FALSE")
  if(!is.null(model_path)) if(!is.character(model_path)) return("model_path needs to be a character type")
  if(!is.null(metadata_path)) if(!is.character(metadata_path)) return("metadata_path needs to be a character type")
  if(!is.character(ModelID)) return("ModelID needs to be a character type")
  if(NumOfParDepPlots < 0L) return("NumOfParDepPlots needs to be a positive number")
  if(!(ReturnModelObjects %in% c(TRUE, FALSE))) return("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects %in% c(TRUE, FALSE))) return("SaveModelObjects needs to be TRUE or FALSE")
  if(!GridTune & length(Trees) > 1L) Trees <- Trees[length(Trees)]
  
  # Regression Ensure data is a data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(ValidationData)) if(!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)
  if(!is.null(TestData)) if(!data.table::is.data.table(TestData)) data.table::setDT(TestData)
  
  # Transformation ColNames----
  if(!is.null(TransformNumericColumns)) if(!is.character(TransformNumericColumns)) TransformNumericColumns <- names(data)[TransformNumericColumns]
  
  # Transform data, ValidationData, and TestData----
  if(!is.null(TransformNumericColumns)) {
    MeanTrainTarget <- mean(data[[eval(TargetColumnName)]], na.rm = TRUE)
    Output <- AutoTransformationCreate(
      data,
      ColumnNames = TransformNumericColumns,
      Methods = Methods,
      Path = model_path,
      TransID = ModelID,
      SaveOutput = SaveModelObjects)
    data <- Output$Data
    TransformationResults <- Output$FinalResults
    
    # Transform ValidationData----
    if(!is.null(ValidationData)) {
      ValidationData <- AutoTransformationScore(
        ScoringData = ValidationData,
        Type = "Apply",
        FinalResults = TransformationResults,
        TransID = NULL,
        Path = NULL)
    }
    
    # Transform TestData----
    if(!is.null(TestData)) {
      TestData <- AutoTransformationScore(
        ScoringData = TestData,
        Type = "Apply",
        FinalResults = TransformationResults,
        TransID = NULL,
        Path = NULL)
    }
  }
  
  # Regression Target Name Storage----
  if(is.character(TargetColumnName)) Target <- TargetColumnName else Target <- names(data)[TargetColumnName]
  
  # Regression IDcol Name Storage----
  if(!is.null(IDcols)) if(!is.character(IDcols)) IDcols <- names(data)[IDcols]
  
  # Regression Data Partition----
  if(is.null(ValidationData) & is.null(TestData) & !TrainOnFull) {
    if(!is.null(TransformNumericColumns)) {
      dataSets <- AutoDataPartition(
        data,
        NumDataSets = 3L,
        Ratios = c(0.70, 0.20, 0.10),
        PartitionType = "random",
        StratifyColumnNames = NULL,
        TimeColumnName = NULL)
      data <- dataSets$TrainData
      ValidationData <- dataSets$ValidationData
      TestData <- dataSets$TestData
      
      # Mean of data----
      MeanTrainTarget <- mean(data[[eval(TargetColumnName)]], na.rm = TRUE)
      
      # Transform data sets----
      Output <- AutoTransformationCreate(
        data,
        ColumnNames = TransformNumericColumns,
        Methods = Methods,
        Path = model_path,
        TransID = ModelID,
        SaveOutput = SaveModelObjects)
      data <- Output$Data
      TransformationResults <- Output$FinalResults
      
      # Transform ValidationData----
      ValidationData <- AutoTransformationScore(
        ScoringData = ValidationData,
        Type = "Apply",
        FinalResults = TransformationResults,
        TransID = NULL,
        Path = NULL)
      
      # Transform TestData----
      if (!is.null(TestData)) {
        TestData <- AutoTransformationScore(
          ScoringData = TestData,
          Type = "Apply",
          FinalResults = TransformationResults,
          TransID = NULL,
          Path = NULL)
      }
    } else {
      dataSets <- AutoDataPartition(
        data,
        NumDataSets = 3L,
        Ratios = c(0.70, 0.20, 0.10),
        PartitionType = "random",
        StratifyColumnNames = NULL,
        TimeColumnName = NULL)
      data <- dataSets$TrainData
      ValidationData <- dataSets$ValidationData
      TestData <- dataSets$TestData
      MeanTrainTarget <- mean(data[[eval(TargetColumnName)]], na.rm = TRUE)
    }
  } else {
    UseBestModel <- FALSE 
  }
  
  # Regression Sort data if PrimaryDateColumn----
  if(!is.null(PrimaryDateColumn)) {
    data <- data[order(get(PrimaryDateColumn))]
    if(!(eval(PrimaryDateColumn) %in% IDcols)) data.table::set(data, j = eval(PrimaryDateColumn), value = NULL)
  }
  
  # Regression Sort ValidationData if PrimaryDateColumn----
  if(!is.null(PrimaryDateColumn) & TrainOnFull != TRUE) {
    ValidationData <- ValidationData[order(get(PrimaryDateColumn))]
    if(!(eval(PrimaryDateColumn) %in% IDcols)) data.table::set(ValidationData, j = eval(PrimaryDateColumn), value = NULL)
  }
  
  # Regression Sort TestData if PrimaryDateColumn----
  if(!is.null(TestData) & TrainOnFull != TRUE) {
    if(!is.null(PrimaryDateColumn)) {
      TestData <- TestData[order(get(PrimaryDateColumn))]
      if (!(eval(PrimaryDateColumn) %in% IDcols)) data.table::set(TestData, j = eval(PrimaryDateColumn), value = NULL)
    }
  }
  
  # Regression data Subset Columns Needed----
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
  
  # Regression TestData Subset Columns Needed----
  if(!is.null(TestData)) {
    if(is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
      keep1 <- names(TestData)[c(FeatureColNames)]
      if (!is.null(IDcols)) {
        keep <- c(IDcols, keep1, Target)
      } else {
        keep <- c(keep1, Target)
      }
      TestData <- TestData[, ..keep]
    } else {
      keep1 <- c(FeatureColNames)
      if (!is.null(IDcols)) {
        keep <- c(IDcols, FeatureColNames, Target)
      } else {
        keep <- c(FeatureColNames, Target)
      }
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
  
  # Regression Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(sapply(dataTrain, is.factor))), as.numeric(which(sapply(dataTrain, is.character)))))
  
  # Regression Convert CatFeatures to 1-indexed----
  if (length(CatFeatures) > 0L) for (i in seq_len(length(CatFeatures))) CatFeatures[i] <- CatFeatures[i] - 1L
  
  # Regression Train ModelDataPrep----
  dataTrain <- ModelDataPrep(
    data = dataTrain,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = TRUE,
    MissFactor = "0",
    MissNum = -1L)
  
  # Regression Validation ModelDataPrep----
  if(TrainOnFull != TRUE) {
    if(!is.null(dataTest)) {
      dataTest <- ModelDataPrep(
        data = dataTest,
        Impute = TRUE,
        CharToFactor = TRUE,
        RemoveDates = TRUE,
        MissFactor = "0",
        MissNum = -1)
    }    
  }
  
  # Regression Test ModelDataPrep----
  if(!is.null(TestData)) {
    TestData <- ModelDataPrep(
      data = TestData,
      Impute = TRUE,
      CharToFactor = TRUE,
      RemoveDates = TRUE,
      MissFactor = "0",
      MissNum = -1)
  }
  
  # Regression Save Names of data----
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
  
  # Regression Get Min Value of Target Data----
  MinVal <- min(data[[eval(Target)]], na.rm = TRUE)
  
  # Regression Subset Target Variables----
  TrainTarget <- dataTrain[, .SD, .SDcols = eval(Target)][[1L]]
  if(TrainOnFull != TRUE) {
    TestTarget <- dataTest[, .SD, .SDcols = eval(Target)][[1L]]
    if(!is.null(TestData)) FinalTestTarget <- TestData[, .SD, .SDcols = eval(Target)][[1L]]
  }
  
  # Regression eval_metric checks
  if(TrainOnFull != TRUE) if(tolower(eval_metric) == "poisson" & (min(TrainTarget) < 0L | min(TestTarget) < 0L)) return("eval_metric Poisson requires positive values for Target")
  
  # Regression Initialize Catboost Data Conversion----
  if(!is.null(CatFeatures)) {
    if(!is.null(TestData)) {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget, cat_features = CatFeatures)
      if(TrainOnFull != TRUE) TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
      if(TrainOnFull != TRUE) FinalTestPool <- catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget, cat_features = CatFeatures)
    } else {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget, cat_features = CatFeatures)
      if(TrainOnFull != TRUE) TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
    }
  } else {
    if(!is.null(TestData)) {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      if(TrainOnFull != TRUE) TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
      if(TrainOnFull != TRUE) FinalTestPool <- catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget)
    } else {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      if(TrainOnFull != TRUE) TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
    }
  }
  
  # Regression Grid Tune or Not Check----
  if(GridTune & !TrainOnFull) {
    
    # Pull in Grid sets----
    Grids <- CatBoostParameterGrids(
      TaskType      = task_type,
      Shuffles      = Shuffles,
      NTrees        = Trees,
      Depth         = Depth,
      LearningRate  = LearningRate,
      L2_Leaf_Reg   = L2_Leaf_Reg,
      RSM           = RSM,
      BootStrapType = BootStrapType,
      GrowPolicy    = GrowPolicy)
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
    
    # Binary Grid Tuning Main Loop----
    counter <- 0L
    NewGrid <- 1L
    repeat {
      
      # Increment counter----
      counter <- counter + 1L
      
      # Check if there are any grid elements left in the specific grid----
      if(!is.null(GridClusters[[paste0("Grid_", max(1L, NewGrid))]][["BootStrapType"]][1L])) {
        
        # Define prameters----
        if(!exists("NewGrid")) {
          base_params <- CatBoostRegressionParams(LossFunction=LossFunction,NumGPUs=NumGPUs,BanditArmsN=BanditArmsN,counter=counter,HasTime=HasTime,MetricPeriods=MetricPeriods,eval_metric=eval_metric,task_type=task_type,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        } else {
          base_params <- CatBoostRegressionParams(LossFunction=LossFunction,NumGPUs=NumGPUs,BanditArmsN=BanditArmsN,counter=counter,HasTime=HasTime,MetricPeriods=MetricPeriods,eval_metric=eval_metric,task_type=task_type,model_path=model_path,NewGrid=NewGrid,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        }
        
        # Build model----
        print(base_params)
        RunTime <- system.time(model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params))
        
        # Score and measure model----
        if(!is.null(TestData)) {
          predict <- catboost::catboost.predict(model = model, pool = FinalTestPool, prediction_type = "RawFormulaVal", thread_count = -1L)
          calibEval <- data.table::as.data.table(cbind(Target = FinalTestTarget, p1 = predict))
          calibEval[, Metric := (Target - p1) ^ 2L]
          NewPerformance <- calibEval[, mean(Metric, na.rm = TRUE)]
        } else {
          predict <- catboost::catboost.predict(model = model,pool = TestPool, prediction_type = "RawFormulaVal", thread_count = -1L)
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
      
      # ExperimentGrid = ExperimentalGrid
      # ModelRun = counter
      # ModelType = "regression"
      # NEWGrid = NewGrid
      # NewPerformance = NewPerformance
      # BestPerformance = BestPerformance
      # TrialVector = Trials
      # SuccessVector = Successes
      # GridIDS = GridIDs
      # BanditArmsCount = BanditArmsN
      # RunsWithoutNewWinner = RunsWithoutNewWinner
      # MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
      # MaxNumberModels = MaxModelsInGrid
      # MaxRunMinutes = MaxRunMinutes
      # TotalRunTime = ExperimentalGrid[RunTime != -1L][, sum(RunTime, na.rm = TRUE)]
      # BanditProbabilities = BanditProbs
      
      
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
      for(bandit in seq_len(length(BanditProbs))) data.table::set(ExperimentalGrid, i = counter + 1L, j = paste0("BanditProbs_Grid_", bandit), value = BanditProbs[bandit])
    }
    
    # Remove unneeded rows----
    ExperimentalGrid <- ExperimentalGrid[RunTime != -1L]
  }
  
  # Define parameters for case where you pass in a winning GridMetrics from grid tuning----
  if(!is.null(PassInGrid)) {
    if (tolower(task_type) == "gpu") {
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
        loss_function        = LossFunction,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        task_type            = task_type,
        devices              = NumGPUs,
        train_dir            = model_path,
        iterations           = PassInGrid[["TreesBuilt"]],
        depth                = PassInGrid[["Depth"]],
        learning_rate        = PassInGrid[["LearningRate"]],
        l2_leaf_reg          = PassInGrid[["L2_Leaf_Reg"]],
        rsm                  = PassInGrid[["RSM"]],
        bootstrap_type       = PassInGrid[["BootStrapType"]])
    }
  }
  
  # Define parameters for case where you want to run grid tuning----
  if(GridTune & !TrainOnFull) {
    
    # Prepare winning grid----
    BestGrid <- ExperimentalGrid[order(-EvalMetric)][1L]
    if(tolower(task_type) == "gpu") grid_params <- as.list(BestGrid[, c(5L:12L)]) else grid_params <- as.list(BestGrid[, c(5L:11L)])
    if(tolower(task_type) == "gpu") grid_params <- grid_params[!names(grid_params) %chin% "RSM"]
    if(tolower(task_type) == "cpu") grid_params <- grid_params[!names(grid_params) %chin% "GrowPolicy"]
    
    # Set parameters from winning grid----
    if (BestGrid$RunNumber[1L] == 1L) {
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
        allow_writing_files  = FALSE)
    } else {
      if (tolower(task_type) == "gpu") {
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
          iterations           = BestGrid[["NTrees"]],
          depth                = BestGrid[["Depth"]],
          learning_rate        = BestGrid[["LearningRate"]],
          l2_leaf_reg          = BestGrid[["L2_Leaf_Reg"]],
          bootstrap_type       = BestGrid[["BootStrapType"]],
          grow_policy          = BestGrid[["GrowPolicy"]],
          allow_writing_files  = FALSE)
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
          iterations           = BestGrid[["NTrees"]],
          depth                = BestGrid[["Depth"]],
          learning_rate        = BestGrid[["LearningRate"]],
          l2_leaf_reg          = BestGrid[["L2_Leaf_Reg"]],
          rsm                  = BestGrid[["RSM"]],
          bootstrap_type       = BestGrid[["BootStrapType"]],
          allow_writing_files  = FALSE)
      }
    }
  }
  
  # Not pass in GridMetric and not grid tuning----
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
      allow_writing_files  = FALSE)
  }
  
  # Regression Train Final Model----
  if(TrainOnFull) {
    model <- catboost::catboost.train(learn_pool = TrainPool, params = base_params)    
  } else {
    model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params)
  }
  
  # Regression Save Model----
  if (SaveModelObjects) catboost::catboost.save_model(model = model, model_path = file.path(normalizePath(model_path), ModelID))
  
  # Regression Score Final Test Data----
  if(!is.null(TestData)) {
    predict <- catboost::catboost.predict(model = model, pool = FinalTestPool, prediction_type = "RawFormulaVal", thread_count = -1L)
  } else if(TrainOnFull) {
    predict <- catboost::catboost.predict(model = model, pool = TrainPool, prediction_type = "RawFormulaVal", thread_count = -1L)
  } else {
    predict <- catboost::catboost.predict(model = model, pool = TestPool, prediction_type = "RawFormulaVal", thread_count = -1L)
  }
  
  # Regression Validation Data----
  if(!TrainOnFull) {
    if(!is.null(TestData)) {
      ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, Predict = predict))
      data.table::setnames(ValidationData, "Target",eval(TargetColumnName))
    } else {
      ValidationData <- data.table::as.data.table(cbind(Target = TestTarget, dataTest, Predict = predict))
      data.table::setnames(ValidationData, "Target",eval(TargetColumnName))
    }    
  } else {
    data <- data.table::as.data.table(cbind(Target = TrainTarget, data, Predict = predict)) 
    data.table::setnames(data, "Target",eval(TargetColumnName))
  }
  
  # Inverse Transform----
  if(!is.null(TransformNumericColumns)) {
    
    # Append record for Predicted Column----
    if(GridTune & !TrainOnFull) TransformationResults <- TransformationResults[ColumnName != "Predicted"]
    TransformationResults <- data.table::rbindlist(list(
      TransformationResults,
      data.table::data.table(
        ColumnName = c("Predict", eval(TargetColumnName)),
        MethodName = rep(TransformationResults[ColumnName == eval(TargetColumnName), MethodName], 2L),
        Lambda = rep(TransformationResults[ColumnName == eval(TargetColumnName), Lambda], 2L),
        NormalizedStatistics = rep(0L, 2L))))
    
    # If Actual target columnname == "Target" remove the duplicate version----
    if(length(unique(TransformationResults[["ColumnName"]])) != nrow(TransformationResults)) {
      temp <- TransformationResults[, .N, by = "ColumnName"][N != 1L][[1L]]
      if(!is.null(ValidationData)) temp1 <- which(names(ValidationData) == temp)[1L]
      if(!TrainOnFull) {
        ValidationData[, eval(names(data)[temp1]) := NULL]
      } else {
        if(TrainOnFull) {
          if(length(which(names(data) %chin% eval(TargetColumnName))) > 1L) {
            temp1 <- which(names(data) %chin% eval(TargetColumnName))[1L]
            data[, which(names(data) %chin% eval(TargetColumnName))[2L] := NULL]
          }
        } else {
          data[, eval(names(data)[temp]) := NULL]
        }
      }
      TransformationResults <- TransformationResults[, ID := 1L:.N][ID != max(ID)]
    }
    
    # Transform Target and Predicted Value----
    if(!TrainOnFull) {
      ValidationData <- AutoTransformationScore(
        ScoringData = ValidationData,
        Type = "Inverse",
        FinalResults = TransformationResults,
        TransID = NULL,
        Path = NULL)
    } else {
      data <- AutoTransformationScore(
        ScoringData = data,
        Type = "Inverse",
        FinalResults = TransformationResults,
        TransID = NULL,
        Path = NULL)
    }
  }
  
  # Regression r2 via sqrt of correlation
  if(!TrainOnFull) r_squared <- (ValidationData[, stats::cor(ValidationData[[eval(TargetColumnName)]], Predict)]) ^ 2L
  
  # Regression Save Validation Data to File----
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      if(!TrainOnFull) {
        data.table::fwrite(ValidationData, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ValidationData.csv")))
      } else {
        data.table::fwrite(data, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_FullDataPredictions.csv")))
      }
    } else {
      if(!TrainOnFull) {
        data.table::fwrite(ValidationData, file = file.path(normalizePath(model_path), paste0(ModelID, "_ValidationData.csv")))
      } else {
        data.table::fwrite(data, file = file.path(normalizePath(model_path), paste0(ModelID, "_FullDataPredictions.csv")))
      }
    }
  }
  
  # Regression Evaluation Metrics----
  if(!TrainOnFull) {
    EvaluationMetrics <- data.table::data.table(Metric = c("MAE","MAPE","RMSE","R2"), MetricValue = rep(999999, 8L))
    i <- 0L
    for(metric in c("mae", "mape", "rmse", "r2")) {
      i <- i + 1L
      tryCatch({
        if (tolower(metric) == "mae") {
          ValidationData[, Metric := abs(ValidationData[[eval(TargetColumnName)]] - Predict)]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == "mape") {
          ValidationData[, Metric := abs((ValidationData[[eval(TargetColumnName)]] - Predict) / (ValidationData[[eval(TargetColumnName)]] + 1))]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == "rmse") {
          ValidationData[, Metric := (ValidationData[[eval(TargetColumnName)]] - Predict) ^ 2]
          Metric <- sqrt(ValidationData[, mean(Metric, na.rm = TRUE)])
        } else if(tolower(metric) == "r2") {
          ValidationData[, ':=' (Metric1 = (ValidationData[[eval(TargetColumnName)]] - MeanTrainTarget) ^ 2, Metric2 = (ValidationData[[eval(TargetColumnName)]] - Predict) ^ 2)]
          Metric <- 1 - ValidationData[, sum(Metric2, na.rm = TRUE)] / ValidationData[, sum(Metric1, na.rm = TRUE)]
        }
        data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric, 4L))
      }, error = function(x) "skip")
    }
    
    # Remove Cols----
    ValidationData[, ':=' (Metric = NULL)]
    
    # Save EvaluationMetrics to File----
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvaluationMetrics, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationMetrics.csv")))
      } else {
        data.table::fwrite(EvaluationMetrics, file = file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationMetrics.csv")))
      }
    }
    
    # Regression Evaluation Calibration Plot----
    if(EvalPlots) {
      if(!TrainOnFull) {
        EvaluationPlot <- EvalPlot(
          data = ValidationData,
          PredictionColName = "Predict",
          TargetColName = eval(TargetColumnName),
          GraphType = "calibration",
          PercentileBucket = 0.05,
          aggrfun = function(x) mean(x, na.rm = TRUE))    
      }
      
      # Add Number of Trees to Title
      if(!TrainOnFull) EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(EvaluationMetrics[Metric == "R2", MetricValue], 3L)))
      
      # Save plot to file
      if(!TrainOnFull) {
        if(SaveModelObjects) {
          if(!is.null(metadata_path)) {
            ggplot2::ggsave(file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationPlot.png")))
          } else {
            ggplot2::ggsave(file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationPlot.png")))
          }
        }    
      }
      
      # Regression Evaluation Calibration BoxPlot----
      if(!TrainOnFull) {
        EvaluationBoxPlot <- EvalPlot(
          data = ValidationData,
          PredictionColName = "Predict",
          TargetColName = eval(TargetColumnName),
          GraphType = "boxplot",
          PercentileBucket = 0.05,
          aggrfun = function(x) mean(x, na.rm = TRUE))    
      }
      
      # Add Number of Trees to Title
      if(!TrainOnFull) EvaluationBoxPlot <- EvaluationBoxPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ", round(EvaluationMetrics[Metric == "R2", MetricValue], 3L)))
      
      # Save plot to file
      if(!TrainOnFull) {
        if(SaveModelObjects) {
          if(!is.null(metadata_path)) {
            ggplot2::ggsave(file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationBoxPlot.png")))
          } else {
            ggplot2::ggsave(file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationBoxPlot.png")))
          }
        }
      }
    } else {
      EvaluationPlot <- NULL
      EvaluationBoxPlot <- NULL
    }
    
    # Regression Variable Importance----
    if(tolower(task_type) == "gpu") {
      if(GridTune) {
        if(!BestGrid[["GrowPolicy"]] %chin% c("Depthwise","Lossguide")) {
          temp <- catboost::catboost.get_feature_importance(model)
          VariableImportance <- data.table::data.table(cbind(Variable = row.names(temp), temp))
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
        VariableImportance <- data.table::data.table(cbind(Variable = row.names(temp), temp))
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
    } else {
      if(GridTune) {
        if(!BestGrid[["GrowPolicy"]] %chin% c("Depthwise","Lossguide")) {
          temp <- catboost::catboost.get_feature_importance(model)
          VariableImportance <- data.table::data.table(cbind(Variable = row.names(temp), temp))
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
      } else {
        temp <- catboost::catboost.get_feature_importance(model)
        VariableImportance <- data.table::data.table(cbind(Variable = row.names(temp), temp))
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
    }
    
    # Regression Partial Dependence----
    if(!is.null(VariableImportance)) {
      ParDepBoxPlots <- list()
      ParDepPlots <- list()
      if(!is.null(VariableImportance) & NumOfParDepPlots > 0L) {
        j <- 0L
        k <- 0L
        for(i in seq_len(min(length(FeatureColNames), NumOfParDepPlots, VariableImportance[,.N]))) {
          tryCatch({
            Out <- ParDepCalPlots(
              data = ValidationData,
              PredictionColName = "Predict",
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
              PredictionColName = "Predict",
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
        
        # Regression Save ParDepPlots to file----
        if(SaveModelObjects) {
          if(!is.null(metadata_path)) {
            save(ParDepPlots, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ParDepPlots.R")))
          } else {
            save(ParDepPlots, file = file.path(normalizePath(model_path), paste0(ModelID, "_ParDepPlots.R")))
          }
        }
        
        # Regression Save ParDepBoxPlots to file----
        if(SaveModelObjects) {
          if(!is.null(metadata_path)) {
            save(ParDepBoxPlots, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ParDepBoxPlots.R")))
          } else {
            save(ParDepBoxPlots, file = file.path(normalizePath(model_path), paste0(ModelID, "_ParDepBoxPlots.R")))
          }
        }    
      }
    }
    
    # Regression Save Grid output----
    if(SaveModelObjects & GridTune) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(ExperimentalGrid, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ExperimentalGrid.csv")))
      } else {
        data.table::fwrite(ExperimentalGrid, file = file.path(normalizePath(model_path), paste0(ModelID, "_ExperimentalGrid.csv")))
      }
    }
  }

  # Final Garbage Collection----
  if(tolower(task_type) == "gpu") gc()
  
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
    ggplot2::ggplot(VI_Data[1L:min(10L, .N)], ggplot2::aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
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
  
  # Regression Return Model Objects----
  if(!TrainOnFull) {
    if(ReturnModelObjects) {
      return(list(
        Model = model,
        ValidationData = ValidationData,
        EvaluationPlot = EvaluationPlot,
        EvaluationBoxPlot = EvaluationBoxPlot,
        EvaluationMetrics = EvaluationMetrics,
        VariableImportance = VariableImportance,
        VI_Plot = tryCatch({VI_Plot(VariableImportance)}, error = NULL),
        PartialDependencePlots = ParDepPlots,
        PartialDependenceBoxPlots = ParDepBoxPlots,
        GridList = if(exists("ExperimentalGrid")) data.table::setorderv(ExperimentalGrid, cols = "EvalMetric", order = 1L, na.last = TRUE) else NULL,
        ColNames = Names,
        TransformationResults = if(exists("TransformationResults")) TransformationResults else NULL))
    }
  } else if(ReturnModelObjects) {
    return(list(Model = model, data = data, ColNames = Names, TransformationResults = if(exists("TransformationResults")) TransformationResults else NULL))
  }
}
