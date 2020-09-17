#' AutoXGBoostRegression is an automated XGBoost modeling framework with grid-tuning and model evaluation
#'
#' AutoXGBoostRegression is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Automated Supervised Learning - Regression
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param ReturnFactorLevels Set to TRUE to have the factor levels returned with the other model objects
#' @param TransformNumericColumns Set to NULL to do nothing; otherwise supply the column names of numeric variables you want transformed
#' @param Methods Choose from "BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson". Function will determine if one cannot be used because of the underlying data.
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
#' @param PassInGrid Default is NULL. Provide a data.table of grid options from a previous run.
#' @param MaxRunsWithoutNewWinner A number
#' @param MaxRunMinutes In minutes
#' @param Shuffles Numeric. List a number to let the program know how many times you want to shuffle the grids for grid tuning
#' @param BaselineComparison Set to either "default" or "best". Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
#' @param Trees Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the trees numbers you want to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1000L, 10000L, 1000L)
#' @param eta Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param max_depth Bandit grid partitioned. Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param min_child_weight Number, or vector for min_child_weight to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param subsample Number, or vector for subsample to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(0.55, 1.0, 0.05)
#' @param colsample_bytree Number, or vector for colsample_bytree to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(0.55, 1.0, 0.05)
#' @examples
#' \donttest{
#' # Create some dummy correlated data with numeric and categorical features
#' data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = FALSE, MultiClass = FALSE)
#'
#' # Run function
#' TestModel <- RemixAutoML::AutoXGBoostRegression(
#'
#'     # GPU or CPU
#'     TreeMethod = "hist",
#'     NThreads = 8L,
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
#'     model_path = normalizePath("./"),
#'     metadata_path = NULL,
#'     ModelID = "Test_Model_1",
#'     ReturnFactorLevels = TRUE,
#'     ReturnModelObjects = TRUE,
#'     SaveModelObjects = FALSE,
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
#'     IDcols = c("IDcol_1","IDcol_2"),
#'     TransformNumericColumns = NULL,
#'     Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),
#'
#'     # Model evaluation
#'     eval_metric = "rmse",
#'     NumOfParDepPlots = 3L,
#'
#'     # Grid tuning arguments - PassInGrid is the best of GridMetrics
#'     PassInGrid = NULL,
#'     GridTune = TRUE,
#'     grid_eval_metric = "mse",
#'     BaselineComparison = "default",
#'     MaxModelsInGrid = 10L,
#'     MaxRunsWithoutNewWinner = 20L,
#'     MaxRunMinutes = 24L*60L,
#'     Verbose = 1L,
#'
#'     # Trees, Depth, and LearningRate used in the bandit grid tuning
#'     # Must set Trees to a single value if you are not grid tuning
#'     # The ones below can be set to NULL and the values in the example will be used
#'     Shuffles = 1L,
#'     Trees = seq(50L, 500L, 50L),
#'     eta = seq(0.05,0.40,0.05),
#'     max_depth = seq(4L, 16L, 2L),
#'     min_child_weight = seq(1.0, 10.0, 1.0),
#'     subsample = seq(0.55, 1.0, 0.05),
#'     colsample_bytree = seq(0.55, 1.0, 0.05))
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
                                  ModelID = "FirstModel",
                                  ReturnFactorLevels = TRUE,
                                  ReturnModelObjects = TRUE,
                                  SaveModelObjects = FALSE,
                                  TransformNumericColumns = NULL,
                                  Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),
                                  Verbose = 0L,
                                  NumOfParDepPlots = 3L,
                                  NThreads = 8L,
                                  eval_metric = "rmse",
                                  TreeMethod = "hist",
                                  GridTune = FALSE,
                                  grid_eval_metric = "rmse",
                                  BaselineComparison = "default",
                                  MaxModelsInGrid = 10L,
                                  MaxRunsWithoutNewWinner = 20L,
                                  MaxRunMinutes = 24L*60L,
                                  PassInGrid = NULL,
                                  Shuffles = 1L,
                                  Trees = 50L,
                                  eta = NULL,
                                  max_depth = NULL,
                                  min_child_weight = NULL,
                                  subsample = NULL,
                                  colsample_bytree = NULL) {

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Ensure model_path and metadata_path exists----
  if(!is.null(model_path)) if(!dir.exists(file.path(normalizePath(model_path)))) dir.create(normalizePath(model_path))
  if(!is.null(metadata_path)) if(!is.null(metadata_path)) if(!dir.exists(file.path(normalizePath(metadata_path)))) dir.create(normalizePath(metadata_path))

  # Regression Check Arguments----
  if(!(tolower(eval_metric) %chin% c("rmse", "mae", "mape", "r2"))) return("eval_metric not in RMSE, MAE, MAPE, R2")
  if(any(Trees < 1)) return("Trees must be greater than 1")
  if(!GridTune & length(Trees) > 1L) Trees <- Trees[length(Trees)]
  if(!GridTune %in% c(TRUE, FALSE)) return("GridTune needs to be TRUE or FALSE")
  if(MaxModelsInGrid < 1 & GridTune == TRUE) return("MaxModelsInGrid needs to be at least 1 and less than 1080")
  if(!is.null(model_path)) if(!is.character(model_path)) return("model_path needs to be a character type")
  if(!is.null(metadata_path)) if(!is.character(metadata_path)) return("metadata_path needs to be a character type")
  if(!is.character(ModelID)) return("ModelID needs to be a character type")
  if(NumOfParDepPlots < 0) return("NumOfParDepPlots needs to be a positive number")
  if(!(ReturnModelObjects %in% c(TRUE, FALSE))) return("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects %in% c(TRUE, FALSE))) return("SaveModelObjects needs to be TRUE or FALSE")

  # Regression Ensure data is a data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(ValidationData)) if(!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)
  if(!is.null(TestData)) if(!data.table::is.data.table(TestData)) data.table::setDT(TestData)

  # Regression Target Name Storage----
  if(is.character(TargetColumnName)) Target <- TargetColumnName else Target <- names(data)[TargetColumnName]

  # Regression IDcol Name Storage----
  if(!is.null(IDcols)) if(!is.character(IDcols)) IDcols <- names(data)[IDcols]

  # Regression Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(sapply(data, is.factor))), as.numeric(which(sapply(data, is.character)))))
  CatFeatures <- names(data)[CatFeatures]
  CatFeatures <- CatFeatures[!CatFeatures %chin% IDcols]
  if(length(CatFeatures) == 0L) CatFeatures <- NULL

  # Transform data, ValidationData, and TestData----
  if(!is.null(ValidationData) & !is.null(TransformNumericColumns)) {
    MeanTrainTarget <- data[, mean(get(TargetColumnName))]
    Output <- AutoTransformationCreate(
      data,
      ColumnNames = TransformNumericColumns,
      Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),
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
    if(!is.null(TestData)) {
      TestData <- AutoTransformationScore(
        ScoringData = TestData,
        Type = "Apply",
        FinalResults = TransformationResults,
        TransID = NULL,
        Path = NULL)
    }
  }

  # Regression Data Partition----
  if(is.null(ValidationData) & is.null(TestData) & !TrainOnFull) {
    if(!is.null(TransformNumericColumns)) {

      # Partition----
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
      MeanTrainTarget <- data[, mean(get(TargetColumnName))]

      # Transform data sets----
      Output <- AutoTransformationCreate(
        data,
        ColumnNames = TransformNumericColumns,
        Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"),
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
      if(!is.null(TestData)) {
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
      MeanTrainTarget <- data[, mean(get(TargetColumnName))]
    }
  }

  # Regression data Subset Columns Needed----
  if((is.numeric(FeatureColNames) | is.integer(FeatureColNames)) & !TrainOnFull) {
    keep1 <- names(data)[c(FeatureColNames)]
    keep <- c(keep1, Target)
    TrainMerge <- data.table::copy(data)
    dataTrain <- data[, ..keep]
    ValidMerge <- data.table::copy(ValidationData)
    dataTest <- ValidationData[, ..keep]
  } else if((is.numeric(FeatureColNames) | is.integer(FeatureColNames)) & TrainOnFull) {
    keep1 <- names(data)[c(FeatureColNames)]
    keep <- c(keep1, Target)
    dataTrain <- data[, ..keep]
    TrainMerge <- data.table::copy(dataTrain)
    dataTest <- NULL
  } else if(!TrainOnFull) {
    keep <- c(FeatureColNames, Target)
    dataTrain <- data[, ..keep]
    TrainMerge <- data.table::copy(dataTrain)
    ValidMerge <- data.table::copy(ValidationData)
    dataTest <- ValidationData[, ..keep]
  } else {
    keep <- c(FeatureColNames, Target)
    dataTrain <- data[, ..keep]
    TrainMerge <- data.table::copy(dataTrain)
    dataTest <- NULL
  }

  # Regression TestData Subset Columns Needed----
  if(!is.null(TestData)) {
    if(is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
      keep1 <- names(TestData)[c(FeatureColNames)]
      if(!is.null(IDcols)) {
        keep <- c(IDcols, keep1, Target)
      } else {
        keep <- c(keep1, Target)
      }
      TestData <- TestData[, ..keep]
    } else {
      keep1 <- c(FeatureColNames)
      if(!is.null(IDcols)) {
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

  # Regression Dummify dataTrain Categorical Features----
  if(!is.null(CatFeatures)) {
    if(SaveModelObjects) {
      if(!is.null(dataTest) & !is.null(TestData) & TrainOnFull == FALSE) {
        data.table::set(dataTrain, j = "ID_Factorizer", value = "TRAIN")
        data.table::set(dataTest, j = "ID_Factorizer", value = "VALIDATE")
        data.table::set(TestData, j = "ID_Factorizer", value = "TEST")
        temp <- data.table::rbindlist(list(dataTrain, dataTest, TestData))
        if(ReturnFactorLevels) {
          if(!is.null(CatFeatures)) {
            temp <- DummifyDT(
              data = temp,
              cols = CatFeatures,
              KeepFactorCols = FALSE,
              OneHot = FALSE,
              SaveFactorLevels = TRUE,
              ReturnFactorLevels = ReturnFactorLevels,
              SavePath = model_path,
              ImportFactorLevels = FALSE)
            IDcols <- c(IDcols,CatFeatures)
            FactorLevelsList <- temp$FactorLevelsList
            temp <- temp$data
          } else {
            FactorLevelsList <- NULL
          }
        } else {
          if(!is.null(CatFeatures)) {
            temp <- DummifyDT(
              data = temp,
              cols = CatFeatures,
              KeepFactorCols = FALSE,
              OneHot = FALSE,
              SaveFactorLevels = FALSE,
              ReturnFactorLevels = ReturnFactorLevels,
              SavePath = model_path,
              ImportFactorLevels = FALSE)
            IDcols <- c(IDcols,CatFeatures)
          } else {
            FactorLevelsList <- NULL
          }
        }
        dataTrain <- temp[ID_Factorizer == "TRAIN"]
        data.table::set(dataTrain, j = "ID_Factorizer", value = NULL)
        dataTest <- temp[ID_Factorizer == "VALIDATE"]
        data.table::set(dataTest, j = "ID_Factorizer", value = NULL)
        TestData <- temp[ID_Factorizer == "TEST"]
        data.table::set(TestData, j = "ID_Factorizer", value = NULL)
      } else {
        data.table::set(dataTrain, j = "ID_Factorizer", value = "TRAIN")
        if(!TrainOnFull) {
          data.table::set(dataTest,j = "ID_Factorizer",value = "TRAIN")
          temp <- data.table::rbindlist(list(dataTrain, dataTest))
        } else {
          temp <- dataTrain
        }
        if(ReturnFactorLevels) {
          if(!is.null(CatFeatures)) {
            temp <- DummifyDT(
              data = temp,
              cols = CatFeatures,
              KeepFactorCols = FALSE,
              OneHot = FALSE,
              SaveFactorLevels = TRUE,
              ReturnFactorLevels = ReturnFactorLevels,
              SavePath = model_path,
              ImportFactorLevels = FALSE
            )
            IDcols <- c(IDcols,CatFeatures)
            FactorLevelsList <- temp$FactorLevelsList
            temp <- temp$data
          } else {
            FactorLevelsList <- NULL
          }
        } else {
          if(!is.null(CatFeatures)) {
            temp <- DummifyDT(
              data = temp,
              cols = CatFeatures,
              KeepFactorCols = FALSE,
              OneHot = FALSE,
              SaveFactorLevels = TRUE,
              ReturnFactorLevels = ReturnFactorLevels,
              SavePath = model_path,
              ImportFactorLevels = FALSE)
            IDcols <- c(IDcols,CatFeatures)
          } else {
            FactorLevelsList <- NULL
          }
        }
        dataTrain <- temp[ID_Factorizer == "TRAIN"]
        data.table::set(dataTrain, j = "ID_Factorizer", value = NULL)
        if(!TrainOnFull) {
          dataTest <- temp[ID_Factorizer == "VALIDATE"]
          data.table::set(dataTest, j = "ID_Factorizer", value = NULL)
        }
      }
    } else {
      if(!is.null(dataTest)) {
        data.table::set(dataTrain, j = "ID_Factorizer", value = "TRAIN")
        if(!TrainOnFull) {
          data.table::set(dataTest, j = "ID_Factorizer", value = "VALIDATE")
          if(!is.null(TestData)) {
            data.table::set(TestData, j = "ID_Factorizer", value = "TEST")
            temp <- data.table::rbindlist(list(dataTrain, dataTest, TestData))
          } else {
            temp <- data.table::rbindlist(list(dataTrain, dataTest))
          }
        } else {
          temp <- dataTrain
        }
        if(ReturnFactorLevels) {
          if(!is.null(CatFeatures)) {
            temp <- DummifyDT(
              data = temp,
              cols = CatFeatures,
              KeepFactorCols = FALSE,
              OneHot = FALSE,
              SaveFactorLevels = FALSE,
              ReturnFactorLevels = ReturnFactorLevels,
              FactorLevelsList = NULL,
              SavePath = NULL,
              ImportFactorLevels = FALSE)
            IDcols <- c(IDcols,CatFeatures)
            FactorLevelsList <- temp$FactorLevelsList
            temp <- temp$data
          } else {
            FactorLevelsList <- NULL
          }
        } else {
          if(!is.null(CatFeatures)) {
            temp <- DummifyDT(
              data = temp,
              cols = CatFeatures,
              KeepFactorCols = FALSE,
              OneHot = FALSE,
              SaveFactorLevels = FALSE,
              ReturnFactorLevels = ReturnFactorLevels,
              SavePath = NULL,
              ImportFactorLevels = FALSE)
            IDcols <- c(IDcols,CatFeatures)
          } else {
            FactorLevelsList <- NULL
          }
        }
        dataTrain <- temp[ID_Factorizer == "TRAIN"]
        data.table::set(dataTrain, j = "ID_Factorizer", value = NULL)
        if(!TrainOnFull) {
          dataTest <- temp[ID_Factorizer == "VALIDATE"]
          data.table::set(dataTest, j = "ID_Factorizer", value = NULL)
          if(!is.null(TestData)) {
            TestData <- temp[ID_Factorizer == "TEST"]
            data.table::set(TestData, j = "ID_Factorizer", value = NULL)
          }
        }
      } else {
        data.table::set(dataTrain, j = "ID_Factorizer", value = "TRAIN")
        if(!TrainOnFull) {
          data.table::set(dataTest, j = "ID_Factorizer", value = "TRAIN")
          FactorLevelsList <- temp$FactorLevelsList
          temp <- data.table::rbindlist(list(dataTrain, dataTest))
        } else {
          temp <- dataTrain
          FactorLevelsList <- NULL
        }

        # Dummify
        temp <- DummifyDT(
          data = temp,
          FactorLevelsList = FactorLevelsList,
          cols = CatFeatures,
          KeepFactorCols = FALSE,
          OneHot = FALSE,
          SaveFactorLevels = FALSE,
          ReturnFactorLevels = ReturnFactorLevels,
          SavePath = NULL,
          ImportFactorLevels = FALSE)
        IDcols <- c(IDcols,CatFeatures)
        FactorLevelsList <- temp$FactorLevelsList
        temp <- temp$data
        dataTrain <- temp[ID_Factorizer == "TRAIN"]
        data.table::set(dataTrain, j = "ID_Factorizer", value = NULL)
        if(!TrainOnFull) {
          dataTest <- temp[ID_Factorizer == "VALIDATE"]
          data.table::set(dataTest, j = "ID_Factorizer", value = NULL)
        }
      }
    }
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
  if(SaveModelObjects) data.table::fwrite(Names, file = file.path(normalizePath(model_path), paste0(ModelID, "_ColNames.csv")))

  # Regression Subset Target Variables----
  TrainTarget <- tryCatch({dataTrain[, get(Target)]}, error = function(x) dataTrain[, eval(Target)])
  if(!TrainOnFull) {
    TestTarget <- tryCatch({dataTest[, get(Target)]}, error = function(x) dataTest[, eval(Target)])
    if(!is.null(TestData)) {
      FinalTestTarget <- tryCatch({
        TestData[, get(Target)]
      }, error = function(x) TestData[, eval(Target)])
    }
  }

  # Regression Remove Target Variable from Feature Data
  dataTrain[, eval(Target) := NULL]
  if(!TrainOnFull) dataTest[, eval(Target) := NULL]
  if(!is.null(TestData)) TestData[, eval(Target) := NULL]

  # Regression Initialize Catboost Data Conversion----
  if("GroupVar" %chin% names(dataTrain)) data.table::set(dataTrain, j = "GroupVar", value = NULL)
  datatrain <- xgboost::xgb.DMatrix(as.matrix(dataTrain), label = TrainTarget)
  if(!TrainOnFull) {
    if("GroupVar" %chin% names(dataTest)) data.table::set(dataTest, j = "GroupVar", value = NULL)
    datavalidate <- xgboost::xgb.DMatrix(as.matrix(dataTest), label = TestTarget)
    if(!is.null(TestData)) {
      if("GroupVar" %chin% names(TestData)) data.table::set(TestData, j = "GroupVar", value = NULL)
      datatest <- xgboost::xgb.DMatrix(as.matrix(TestData), label = FinalTestTarget)
      EvalSets <- list(train = datavalidate, test = datatest)
    } else {
      EvalSets <- list(train = datatrain, test = datavalidate)
    }
  } else {
    EvalSets <- list(train = datatrain)
  }

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
      if(!is.null(GridClusters[[paste0("Grid_",max(1L,counter-1L))]][["Depth"]][1L])) {

        # Define parameters----
        if(!exists("NewGrid")) {
          base_params <- XGBoostRegressionParams(counter=counter,BanditArmsN=BanditArmsN,eval_metric=eval_metric,task_type=TreeMethod,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        } else {
          base_params <- XGBoostRegressionParams(NewGrid=NewGrid,counter=counter,BanditArmsN=BanditArmsN,eval_metric=eval_metric,task_type=TreeMethod,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
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
      data.table::set(ExperimentalGrid, i = counter+1L, j = "MinChildWeight", value = GridClusters[[paste0("Grid_",NewGrid)]][["MinChildWeight"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "SubSample", value = GridClusters[[paste0("Grid_",NewGrid)]][["SubSample"]][Trials[NewGrid]+1L])
      data.table::set(ExperimentalGrid, i = counter+1L, j = "ColSampleByTree", value = GridClusters[[paste0("Grid_",NewGrid)]][["ColSampleByTree"]][Trials[NewGrid]+1L])
      for(bandit in seq_len(length(BanditProbs))) data.table::set(ExperimentalGrid, i = counter+1L, j = paste0("BanditProbs_Grid_",bandit), value = BanditProbs[bandit])
    }

    # Remove unneeded rows----
    ExperimentalGrid <- ExperimentalGrid[RunTime != -1L]
    gc()
  }

  # Define parameters for case where you pass in a winning GridMetrics from grid tuning----
  # Define parameters for case where you pass in a winning GridMetrics from grid tuning----
  if(!is.null(PassInGrid)) {
    if(PassInGrid[,.N] > 1L) PassInGrid <- PassInGrid[order(EvalMetric)][1]
    if(PassInGrid[, BanditProbs_Grid_1] == -10) {
      PassInGrid <- NULL
    }
  }
  if(!is.null(PassInGrid)) {
    base_params <- list(
      booster               = "gbtree",
      objective             = 'reg:linear',
      eval_metric           = tolower(eval_metric),
      nthread               = NThreads,
      max_bin               = 64L,
      early_stopping_rounds = 10L,
      tree_method           = task_type,
      max_depth             = PassInGrid[["Depth"]],
      eta                   = PassInGrid[["LearningRate"]],
      subsample             = PassInGrid[["SubSample"]],
      colsample_bytree      = PassInGrid[["ColSampleByTree"]])

    # Binary Train Final Model----
    model <- xgboost::xgb.train(params=base_params, data=datatrain, watchlist=EvalSets, nrounds=PassInGrid[["NTrees"]])
  }

  # Define parameters for case where you want to run grid tuning----
  if(GridTune & !TrainOnFull) {

    # Prepare winning grid----
    BestGrid <- ExperimentalGrid[order(-EvalMetric)][1L]

    # Set parameters from winning grid----
    if(BestGrid$RunNumber == 1L) {
      base_params <- list(
        booster               = "gbtree",
        objective             = 'reg:squarederror',
        eval_metric           = tolower(eval_metric),
        nthread               = NThreads,
        max_bin               = 64L,
        early_stopping_rounds = 10L,
        eval_metric           = eval_metric,
        tree_method           = TreeMethod)

      # Binary Train Final Model----
      model <- xgboost::xgb.train(params = base_params, data = datatrain, watchlist = EvalSets, nrounds = max(ExperimentalGrid$NTrees), Verbose = Verbose)

    } else {
      base_params <- list(
        booster               = "gbtree",
        objective             = 'reg:squarederror',
        eval_metric           = tolower(eval_metric),
        nthread               = NThreads,
        max_bin               = 64L,
        early_stopping_rounds = 10L,
        tree_method           = TreeMethod,
        max_depth             = BestGrid[["Depth"]],
        eta                   = BestGrid[["LearningRate"]],
        subsample             = BestGrid[["SubSample"]],
        colsample_bytree      = BestGrid[["ColSampleByTree"]])

      # Binary Train Final Model----
      model <- xgboost::xgb.train(params = base_params, data = datatrain, watchlist = EvalSets, nrounds = BestGrid[["NTrees"]], verbose = Verbose)
    }
  }

  # Define parameters Not pass in GridMetric and not grid tuning----
  if(is.null(PassInGrid) & !GridTune) {
    base_params <- list(
      booster               = "gbtree",
      objective             = 'reg:squarederror',
      eval_metric           = tolower(eval_metric),
      nthread               = NThreads,
      max_bin               = 64L,
      tree_method           = TreeMethod,
      verbose               = Verbose,
      early_stopping_rounds = 10L)

    # Binary Train Final Model----
    model <- xgboost::xgb.train(params=base_params, data=datatrain, watchlist=EvalSets, nrounds=Trees)
  }

  # Regression Save Model----
  if(SaveModelObjects) {
    if(getwd() == model_path) {
      xgboost::xgb.save(model = model, fname = ModelID)
    } else {
      save(model, file = file.path(normalizePath(model_path), ModelID))
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
  if(!TrainOnFull) r_squared <- (ValidationData[, stats::cor(get(Target), Predict)]) ^ 2

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
          ValidationData[, Metric := abs(get(Target) - Predict)]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == "mape") {
          ValidationData[, Metric := abs((get(Target) - Predict) / (get(Target) + 1))]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == "rmse") {
          ValidationData[, Metric := (get(Target) - Predict) ^ 2L]
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
      data.table::as.data.table(xgboost::xgb.importance(model = model))},
      error = function(x) data.table(Gain = NULL, Cover = NULL, Frequency = NULL))

    # Variable Importance Formatting----
    if(VariableImportance[, .N] != 0L) {
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
      ParDepPlots <- list()
      ParDepBoxPlots <- list()
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
      ggplot2::ylab("Value")
  }

  # Regression Return Model Objects----
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
