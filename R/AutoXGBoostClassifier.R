#' AutoXGBoostClassifier is an automated XGBoost modeling framework with grid-tuning and model evaluation
#'
#' AutoXGBoostClassifier is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Automated Binary Classification
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "logloss","error","aucpr","auc"
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param model_path A character string of your path file to where you want your output saved
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param Verbose Set to 0 if you want to suppress model evaluation updates in training
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param ReturnFactorLevels TRUE or FALSE. Set to FALSE to not return factor levels.
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param GridTune Set to TRUE to run a grid tuning procedure
#' @param Trees Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the trees numbers you want to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1000L, 10000L, 1000L)
#' @param eta Bandit grid partitioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param max_depth Bandit grid partitioned. Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param min_child_weight Number, or vector for min_child_weight to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param subsample Number, or vector for subsample to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(0.55, 1.0, 0.05)
#' @param colsample_bytree Number, or vector for colsample_bytree to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(0.55, 1.0, 0.05)
#' @param PassInGrid Default is NULL. Provide a data.table of grid options from a previous run.
#' @param MaxModelsInGrid Number of models to test from grid options.
#' @param MaxRunsWithoutNewWinner A number
#' @param MaxRunMinutes In minutes
#' @param Shuffles Numeric. List a number to let the program know how many times you want to shuffle the grids for grid tuning
#' @param BaselineComparison Set to either "default" or "best". Default is to compare each successive model build to the baseline model using max trees (from function args). Best makes the comparison to the current best model.
#' @examples
#' \donttest{
#' # Create some dummy correlated data with numeric and categorical features
#' data <- RemixAutoML::FakeDataGenerator(Correlation = 0.85, N = 1000, ID = 2, ZIP = 0, AddDate = FALSE, Classification = TRUE, MultiClass = FALSE)
#' 
#' # Run function
#' TestModel <- RemixAutoML::AutoXGBoostClassifier(
#' 
#'     # GPU or CPU
#'     TreeMethod = "hist",
#'     NThreads = 8L,
#'   
#'     # Metadata arguments
#'     model_path = getwd(),
#'     metadata_path = file.path(getwd(),"R_Model_Testing"),
#'     ModelID = "Test_Model_1",
#'     ReturnFactorLevels = TRUE,
#'     ReturnModelObjects = TRUE,
#'     SaveModelObjects = FALSE,   
#'   
#'     # Data arguments
#'     data = data,
#'     TrainOnFull = FALSE,
#'     ValidationData = NULL,
#'     TestData = NULL,
#'     TargetColumnName = "Adrian",
#'     FeatureColNames = names(data)[4L:ncol(data)],
#'     IDcols = c("x1","x2"),
#'   
#'     # Model evaluation
#'     eval_metric = "auc",
#'     NumOfParDepPlots = 3L,
#'   
#'     # Grid tuning arguments - PassInGrid is the best of GridMetrics
#'     PassInGrid = NULL,
#'     GridTune = TRUE,
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
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoXGBoostClassifier <- function(data,
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
                                  Verbose = 0L,
                                  NumOfParDepPlots = 3L,
                                  NThreads = 8L,
                                  eval_metric = "auc",
                                  TreeMethod = "hist",
                                  GridTune = FALSE,
                                  BaselineComparison = "default",
                                  MaxModelsInGrid = 10L,
                                  MaxRunsWithoutNewWinner = 20L,
                                  MaxRunMinutes = 24L*60L,
                                  PassInGrid = NULL,
                                  Shuffles = 1L,
                                  Trees = 50L,
                                  eta = seq(0.05,0.40,0.05),
                                  max_depth = seq(4L, 16L, 2L),
                                  min_child_weight = seq(1.0, 10.0, 1.0),
                                  subsample = seq(0.55, 1.0, 0.05),
                                  colsample_bytree = seq(0.55, 1.0, 0.05)) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(percent = 100L)
  
  # Ensure model_path and metadata_path exists----
  if(!dir.exists(file.path(model_path))) dir.create(model_path)
  if(!is.null(metadata_path)) if(!dir.exists(file.path(metadata_path))) dir.create(metadata_path)
  
  # Binary Check Arguments----
  if(any(Trees < 1L)) return("Trees must be greater than 1")
  if(!GridTune & length(Trees) > 1L) Trees <- Trees[length(Trees)]
  if(!GridTune %in% c(TRUE, FALSE)) return("GridTune needs to be TRUE or FALSE")
  if(MaxModelsInGrid < 1L & GridTune == TRUE) return("MaxModelsInGrid needs to be at least 1 and less than 1080")
  if(!is.null(model_path)) if (!is.character(model_path)) return("model_path needs to be a character type")
  if(!is.null(metadata_path)) if (!is.character(metadata_path)) return("metadata_path needs to be a character type")
  if(!is.character(ModelID)) return("ModelID needs to be a character type")
  if(NumOfParDepPlots < 0L) return("NumOfParDepPlots needs to be a positive number")
  if(!(ReturnModelObjects %in% c(TRUE, FALSE))) return("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects %in% c(TRUE, FALSE))) return("SaveModelObjects needs to be TRUE or FALSE")
  
  # Binary Ensure data is a data.table----
  if(!data.table::is.data.table(data)) data <- data.table::as.data.table(data)
  if(!is.null(ValidationData)) if(!data.table::is.data.table(ValidationData)) ValidationData <- data.table::as.data.table(ValidationData)
  if(!is.null(TestData)) if(!data.table::is.data.table(TestData)) TestData <- data.table::as.data.table(TestData)

  # Binary Target Name Storage----
  if(is.character(TargetColumnName)) Target <- TargetColumnName else Target <- names(data)[TargetColumnName]
  
  # Binary IDcol Name Storage----
  if(!is.null(IDcols)) if(!is.character(IDcols)) IDcols <- names(data)[IDcols]
  
  # Binary Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(sapply(data, is.factor))), as.numeric(which(sapply(data, is.character)))))
  CatFeatures <- names(data)[CatFeatures]
  CatFeatures <- CatFeatures[!CatFeatures %chin% IDcols]
  if(length(CatFeatures) == 0L) CatFeatures <- NULL
  if(class(FeatureColNames) == "character") {
    CatFeatures <- CatFeatures[CatFeatures %chin% FeatureColNames]
  } else {
    CatFeatures <- CatFeatures[CatFeatures %chin% names(data)[FeatureColNames]]
  }
  
  # Binary Data Partition----
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
  
  # Binary data Subset Columns Needed----
  if(is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    keep1 <- names(data)[c(FeatureColNames)]
    keep <- c(keep1, Target)
    dataTrain <- data[, ..keep]
    if(!TrainOnFull) {
      dataTest <- ValidationData[, ..keep] 
    } else {
      dataTest <- NULL
    }
  } else {
    keep <- c(FeatureColNames, Target)
    dataTrain <- data[, ..keep]
    if(!TrainOnFull) {
      dataTest <- ValidationData[, ..keep] 
    } else {
      dataTest <- NULL
    }
  }
  
  # Binary TestData Subset Columns Needed----
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
      if(!is.null(dataTest) & !is.null(TestData) & !TrainOnFull) {
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
        if(ReturnFactorLevels) {
          temp <- DummifyDT(
            data = temp,
            cols = CatFeatures,
            KeepFactorCols = FALSE,
            OneHot = FALSE,
            SaveFactorLevels = FALSE,
            ReturnFactorLevels = ReturnFactorLevels,
            SavePath = NULL,
            ImportFactorLevels = FALSE)
        } else {
          temp <- DummifyDT(
            data = temp,
            cols = CatFeatures,
            KeepFactorCols = FALSE,
            OneHot = FALSE,
            SaveFactorLevels = FALSE,
            ReturnFactorLevels = ReturnFactorLevels,
            SavePath = NULL,
            ImportFactorLevels = FALSE)
        }
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
  
  # Binary Save Names of data----
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
  
  # Save column names----
  if(SaveModelObjects) data.table::fwrite(Names, file = file.path(model_path, paste0(ModelID, "_ColNames.csv")))
  
  # Binary Subset Target Variables----
  TrainTarget <- tryCatch({dataTrain[, get(Target)]}, error = function(x) dataTrain[, eval(Target)])
  if(!TrainOnFull) TestTarget <- tryCatch({dataTest[, get(Target)]}, error = function(x) dataTest[, eval(Target)])
  if(!is.null(TestData)) FinalTestTarget <- tryCatch({TestData[, get(Target)]}, error = function(x) TestData[, eval(Target)])
  
  # Binary Remove Target Variable from Feature Data
  dataTrain[, eval(Target) := NULL]
  if(!TrainOnFull) dataTest[, eval(Target) := NULL]
  if(!is.null(TestData)) TestData[, eval(Target) := NULL]
  
  # Binary Initialize xgboost Data Conversion----
  datatrain <- xgboost::xgb.DMatrix(as.matrix(dataTrain), label = TrainTarget)
  if(!TrainOnFull) datavalidate <- xgboost::xgb.DMatrix(as.matrix(dataTest), label = TestTarget)
  if(!is.null(TestData)) {
    datatest <- xgboost::xgb.DMatrix(as.matrix(TestData), label = FinalTestTarget)
    EvalSets <- list(train = datavalidate, test = datatest)
  } else if(!TrainOnFull) {
    EvalSets <- list(train = datatrain, test = datavalidate)
  } else {
    EvalSets <- list(train = datatrain, test = datatrain)
  }
  
  # Binary Grid Tune or Not Check----
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
          base_params <- XGBoostClassifierParams(counter=counter,BanditArmsN=BanditArmsN,eval_metric=eval_metric,task_type=TreeMethod,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        } else {
          base_params <- XGBoostClassifierParams(NewGrid=NewGrid,counter=counter,BanditArmsN=BanditArmsN,eval_metric=eval_metric,task_type=TreeMethod,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
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
          AUC_Metrics <- pROC::roc(response = calibEval[["Target"]], predictor = calibEval[["p1"]], na.rm = TRUE, algorithm = 3L, auc = TRUE, ci = TRUE)
        } else {
          predict <- stats::predict(model, datavalidate)
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
  if(!is.null(PassInGrid)) {
    base_params <- list(
      booster               = "gbtree",
      objective             = 'reg:logistic',
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
        objective             = 'reg:logistic',
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
        objective             = 'reg:logistic',
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
      objective             = 'reg:logistic',
      eval_metric           = tolower(eval_metric),
      nthread               = NThreads,
      max_bin               = 64L,
      tree_method           = TreeMethod,
      verbose               = Verbose,
      early_stopping_rounds = 10L)
    
    # Binary Train Final Model----
    model <- xgboost::xgb.train(params=base_params, data=datatrain, watchlist=EvalSets, nrounds=Trees)
  }
  
  # Binary Save Model----
  if(SaveModelObjects) {
    if(getwd() == model_path) {
      xgboost::xgb.save(model = model, fname = ModelID)  
    } else {
      save(model, file = file.path(model_path, ModelID))
    }    
  }
  
  # Binary Grid Score Model----
  if(!is.null(TestData)) {
    predict <- stats::predict(model, datatest)
  } else if(!TrainOnFull) {
    predict <- stats::predict(model, datavalidate)
  } else {
    predict <- stats::predict(model, datatrain)
  }
  
  # Binary Validation Data----
  if(!is.null(TestData)) {
    ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, p1 = predict))
  } else if(!TrainOnFull) {
    ValidationData <- data.table::as.data.table(cbind(Target = TestTarget, dataTest, p1 = predict))
  } else {
    ValidationData <- data.table::as.data.table(cbind(Target = TrainTarget, dataTrain, p1 = predict))
  }
  
  # Binary AUC Object Create----
  AUC_Metrics <- pROC::roc(
    response = ValidationData[["Target"]],
    predictor = ValidationData[["p1"]],
    na.rm = TRUE,
    algorithm = 3L,
    auc = TRUE,
    ci = TRUE)
  
  # Binary AUC Conversion to data.table----
  AUC_Data <- data.table::data.table(
    ModelNumber = 0L,
    Sensitivity = AUC_Metrics$sensitivities,
    Specificity = AUC_Metrics$specificities)
  
  # Binary Rbind AUC----
  ROC_Plot <- ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity)) +
    ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]]), color = "blue") +
    ggplot2::geom_abline(slope = 1L, color = "black") +
    ggplot2::ggtitle(paste0("Catboost AUC: ", 100 * round(AUC_Metrics$auc, 3), "%")) +
    ChartTheme() + ggplot2::xlab("Specificity") +
    ggplot2::ylab("Sensitivity")
  
  # Save plot to file----
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      ggplot2::ggsave(file.path(metadata_path, paste0(ModelID, "_ROC_Plot.png")))
    } else {
      ggplot2::ggsave(file.path(model_path, paste0(ModelID, "_ROC_Plot.png")))
    }
  }
  
  # Binary Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "p1",
    TargetColName = "Target",
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x) mean(x, na.rm = TRUE))
  
  # Add Number of Trees to Title
  EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("Calibration Evaluation Plot: AUC = ", round(AUC_Metrics$auc, 3)))
  
  # Save plot to file----
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      ggplot2::ggsave(file.path(metadata_path, paste0(ModelID, "_EvaluationPlot.png")))
    } else {
      ggplot2::ggsave(file.path(model_path, paste0(ModelID, "_EvaluationPlot.png")))
    }
  }
  
  # Save EvaluationMetrics to File
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(RemixClassificationMetrics(MLModels="xgboost",TargetVariable=Target,Thresholds=seq(0.01,0.99,0.01),CostMatrix=c(1,0,0,1),ClassLabels=c(1,0),XGBoostTestData=ValidationData), file = paste0(metadata_path, "/", ModelID, "_EvaluationMetrics.csv"))
    } else {
      data.table::fwrite(RemixClassificationMetrics(MLModels="xgboost",TargetVariable=Target,Thresholds=seq(0.01,0.99,0.01),CostMatrix=c(1,0,0,1),ClassLabels=c(1,0),XGBoostTestData=ValidationData), file = paste0(model_path, "/", ModelID, "_EvaluationMetrics.csv"))      
    }
  }
  
  # Binary Variable Importance----
  VariableImportance <- tryCatch({data.table::as.data.table(xgboost::xgb.importance(model = model))}, error = function(x) data.table(Gain = NULL, Cover = NULL, Frequency = NULL))
  if(VariableImportance[, .N] != 0L) {
    VariableImportance[, ':=' (Gain = round(Gain, 4L), Cover = round(Cover, 4L), Frequency = round(Frequency, 4L))]
    if (SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(VariableImportance, file = file.path(metadata_path, paste0(ModelID, "_VariableImportance.csv")))
      } else {
        data.table::fwrite(VariableImportance, file = file.path(model_path, paste0(ModelID, "_VariableImportance.csv")))
      }
    }
    
    # Binary Partial Dependence----
    ParDepPlots <- list()
    j <- 0L
    ParDepBoxPlots <- list()
    k <- 0L
    for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
      tryCatch({
        Out <- ParDepCalPlots(
          data = ValidationData,
          PredictionColName = "p1",
          TargetColName = Target,
          IndepVar = VariableImportance[i, Feature],
          GraphType = "calibration",
          PercentileBucket = 0.05,
          FactLevels = 10L,
          Function = function(x) mean(x, na.rm = TRUE))
        j <- j + 1L
        ParDepPlots[[paste0(VariableImportance[j, Feature])]] <- Out
      }, error = function(x) "skip")
    }  
  }
  
  # Binary Save ParDepPlots to file----
  if(SaveModelObjects) {
    if(!is.null(metadata_path)) {
      save(ParDepPlots, file = file.path(metadata_path, paste0(ModelID, "_ParDepPlots.R")))
    } else {
      save(ParDepPlots, file = file.path(model_path, paste0(ModelID, "_ParDepPlots.R")))
    }
  }
  
  # Binary Save GridCollect and GridList----
  if(SaveModelObjects & GridTune) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(ExperimentalGrid, file = file.path(metadata_path, paste0(ModelID, "ExperimentalGrid.csv")))
    } else {
      data.table::fwrite(ExperimentalGrid, file = file.path(model_path, paste0(ModelID, "ExperimentalGrid.csv")))
    }
  }
  
  # VI_Plot_Function----
  VI_Plot <- function(VI_Data, ColorHigh = "darkblue", ColorLow = "white") {
    ggplot2::ggplot(VI_Data[1L:min(10L,.N)], ggplot2::aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_gradient2(mid = ColorLow, high = ColorHigh) +
      ChartTheme(Size = 12L, AngleX = 0L, LegendPosition = "right") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Global Variable Importance") +
      ggplot2::xlab("Top Model Features") +
      ggplot2::ylab("Value")
  }
  
  # VI_Plot----
  VI_Plot_Object <- VI_Plot(VI_Data = VariableImportance)
  
  # Binary Return Model Objects----
  if(!exists("FactorLevelsList")) FactorLevelsList <- NULL
  
  # Return objects----
  if(GridTune) {
    if (ReturnModelObjects) {
      return(list(Model = model,
                  ValidationData = ValidationData,
                  ROC_Plot = ROC_Plot,
                  EvaluationPlot = EvaluationPlot,
                  EvaluationMetrics = RemixClassificationMetrics(MLModels="xgboost",TargetVariable=Target,Thresholds=seq(0.01,0.99,0.01),CostMatrix=c(1,0,0,1),ClassLabels=c(1,0),XGBoostTestData=ValidationData),
                  VariableImportance = VariableImportance,
                  VI_Plot = VI_Plot_Object,
                  PartialDependencePlots = ParDepPlots,
                  GridMetrics = data.table::setorderv(ExperimentalGrid, cols = "EvalMetric", order = -1L, na.last = TRUE),
                  ColNames = Names,
                  FactorLevels = FactorLevelsList))
    }
  } else {
    if (ReturnModelObjects) {
      return(list(Model = model,
                  ValidationData = ValidationData,
                  ROC_Plot = ROC_Plot,
                  EvaluationPlot = EvaluationPlot,
                  EvaluationMetrics = RemixClassificationMetrics(MLModels="xgboost",TargetVariable=Target,Thresholds=seq(0.01,0.99,0.01),CostMatrix=c(1,0,0,1),ClassLabels=c(1,0),XGBoostTestData=ValidationData),
                  VariableImportance = VariableImportance,
                  VI_Plot = VI_Plot_Object,
                  PartialDependencePlots = ParDepPlots,
                  ColNames = Names,
                  FactorLevels = FactorLevelsList))
    }
  }
}
