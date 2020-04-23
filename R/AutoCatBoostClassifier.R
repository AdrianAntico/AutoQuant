#' AutoCatBoostClassifier is an automated catboost model grid-tuning classifier and evaluation system
#'
#' AutoCatBoostClassifier is an automated modeling function that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train, validation, and test sets (if not supplied). Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions (on test data), an ROC plot, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
#' @author Adrian Antico
#' @family Automated Binary Classification
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
#' @param eval_metric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "AUC" is the default, but other options include "Logloss", "CrossEntropy", "Precision", "Recall", "F1", "BalancedAccuracy", "BalancedErrorRate", "MCC", "Accuracy", "CtrFactor", "AUC", "BrierScore", "HingeLoss", "HammingLoss", "ZeroOneLoss", "Kappa", "WKappa", "LogLikelihoodOfPrediction"
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
#' @param Trees Bandit grid partioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the trees numbers you want to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1000L, 10000L, 1000L)
#' @param Depth Bandit gartioned. Number, or vector for depth to test.  For running grid tuning, a NULL value supplied will mean these values are tested seq(4L, 16L, 2L)
#' @param LearningRate Bandit grid partioned. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the LearningRate values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.01,0.02,0.03,0.04)
#' @param L2_Leaf_Reg Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the L2_Leaf_Reg values to test. For running grid tuning, a NULL value supplied will mean these values are tested seq(1.0, 10.0, 1.0)
#' @param RSM CPU only. Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the RSM values to test. For running grid tuning, a NULL value supplied will mean these values are tested c(0.80, 0.85, 0.90, 0.95, 1.0)
#' @param BootStrapType Random testing. Supply a single value for non-grid tuning cases. Otherwise, supply a vector for the BootStrapType values to test. For running grid tuning, a NULL value supplied will mean these values are tested c("Bayesian", "Bernoulli", "Poisson", "MVS", "No")
#' @param GrowPolicy Random testing. NULL, character, or vector for GrowPolicy to test. For grid tuning, supply a vector of values. For running grid tuning, a NULL value supplied will mean these values are tested c("SymmetricTree", "Depthwise", "Lossguide")
#' @examples
#' \donttest{
#' # Create some dummy correlated data with numeric and categorical features
#' 
#' # Alter correlation value for the simulated data
#' Correl <- 0.85
#' 
#' # Number of rows you want to use
#' N <- 25000L 
#' 
#' data <- data.table::data.table(Adrian = runif(N))
#' data[, x1 := qnorm(Adrian)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^3]
#' data[, Independent_Variable11 := as.factor(
#'   data.table::fifelse(Independent_Variable2 < 0.20, "A",
#'          data.table::fifelse(Independent_Variable2 < 0.40, "B",
#'                 data.table::fifelse(Independent_Variable2 < 0.6,  "C",
#'                        data.table::fifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, Adrian := ifelse(Adrian < 0.5, 1, 0)]
#' 
#' # Run function
#' TestModel <- AutoCatBoostClassifier(
#'     
#'     # GPU or CPU
#'     task_type = "GPU",
#'     
#'     # Metadata arguments
#'     ModelID = "Test_Model_1",
#'     model_path = getwd(),
#'     metadata_path = file.path(getwd(),"R_Model_Testing"),
#'     SaveModelObjects = FALSE,
#'     ReturnModelObjects = TRUE,
#'     
#'     # Data arguments
#'     data = data,
#'     TrainOnFull = FALSE,
#'     ValidationData = NULL,
#'     TestData = NULL,
#'     TargetColumnName = "Adrian",
#'     FeatureColNames = names(data)[2L:ncol(data)],
#'     PrimaryDateColumn = NULL,
#'     ClassWeights = c(1L,1L),
#'     IDcols = c("x1","x2"),
#'     
#'     # Model evaluation
#'     eval_metric = "AUC",
#'     NumOfParDepPlots = ncol(data)-1L-2L,
#'
#'     # Grid tuning arguments - PassInGrid is the best of GridMetrics 
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
                                   eval_metric = "MCC",
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
  
  # Turn on full speed ahead----
  data.table::setDTthreads(percent = 100)
  
  # Binary Check Arguments----
  if (!(tolower(task_type) %chin% c("gpu", "cpu"))) stop("task_type needs to be either 'GPU' or 'CPU'")
  if (!(tolower(eval_metric) %chin% c("logloss","crossentropy","precision","recall","f1",
                                      "balancedaccuracy","balancederrorrate","mcc","accuracy","ctrfactor",
                                      "auc","brierscore","hingeloss","hammingloss","zerooneloss","kappa",
                                      "wkappa","loglikelihoodofprediction"))) {
    stop("eval_metric not in c('Logloss','CrossEntropy','Precision','Recall','F1','BalancedAccuracy','BalancedErrorRate','MCC',
    'Accuracy','CtrFactor','AUC','BrierScore','HingeLoss','HammingLoss','ZeroOneLoss','Kappa','WKappa','LogLikelihoodOfPrediction')")
  }
  if(!is.null(ClassWeights)) {
    LossFunction <- "Logloss"
  } else {
    ClassWeights <- c(1,1)
  }
  if (!is.null(PrimaryDateColumn)) {
    HasTime <- TRUE
  } else {
    HasTime <- FALSE
  }
  if (any(Trees) < 1) stop("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE)) stop("GridTune needs to be TRUE or FALSE")
  if(MaxModelsInGrid < 1 & GridTune == TRUE) {
    stop("MaxModelsInGrid needs to be at least 1 and less than 1080")
  }
  if(!is.null(model_path)) {
    if (!is.character(model_path)) stop("model_path needs to be a character type")
  } else {
    model_path <- getwd()
  }
  if (!is.null(metadata_path)) {
    if (!is.character(metadata_path)) stop("metadata_path needs to be a character type")
  }
  if (!is.character(ModelID)) stop("ModelID needs to be a character type")
  if (NumOfParDepPlots < 0) stop("NumOfParDepPlots needs to be a positive number")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE))) stop("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE))) stop("SaveModelObjects needs to be TRUE or FALSE")
  
  # Binary Ensure data is a data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  
  # Binary Ensure ValidationData is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)
  }
  
  # Binary Ensure TestData is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) data.table::setDT(TestData)
  }
  
  # Binary Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # Binary IDcol Name Storage----
  if (!is.null(IDcols)) {
    if (!is.character(IDcols)) {
      IDcols <- names(data)[IDcols]
    }
  }
  
  # Binary Data Partition----
  if (is.null(ValidationData) & is.null(TestData) & TrainOnFull != TRUE) {
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
  if (!is.null(PrimaryDateColumn)) {
    data <- data[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      data.table::set(data, j = eval(PrimaryDateColumn), value = NULL)
    }
  }
  
  # Binary Sort ValidationData if PrimaryDateColumn----
  if (!is.null(PrimaryDateColumn) & TrainOnFull != TRUE) {
    ValidationData <- ValidationData[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      data.table::set(ValidationData, j = eval(PrimaryDateColumn), value = NULL)
    }
  }
  
  # Binary Sort TestData if PrimaryDateColumn----
  if (!is.null(TestData) & TrainOnFull != TRUE) {
    if (!is.null(PrimaryDateColumn)) {
      data.table::setorderv(TestData, cols = eval(PrimaryDateColumn), order = -1L, na.last = TRUE)
      if (!(eval(PrimaryDateColumn) %in% IDcols)) data.table::set(TestData, j = eval(PrimaryDateColumn), value = NULL)
    }
  }
  
  # Binary data Subset Columns Needed----
  if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
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
  if (!is.null(TestData)) {
    if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
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
    if (!is.null(IDcols)) {
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
  if (length(CatFeatures) > 0) {
    for (i in seq_len(length(CatFeatures))) {
      CatFeatures[i] <- CatFeatures[i] - 1
    }
  }
  
  # Binary Train ModelDataPrep----
  dataTrain <- ModelDataPrep(
    data = dataTrain,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = TRUE,
    MissFactor = "0",
    MissNum = -1)  
  
  # Binary Validation ModelDataPrep----
  if(TrainOnFull != TRUE) {
    dataTest <- ModelDataPrep(
      data = dataTest,
      Impute = TRUE,
      CharToFactor = TRUE,
      RemoveDates = TRUE,
      MissFactor = "0",
      MissNum = -1)  
  }
  
  # Binary Test ModelDataPrep----
  if (!is.null(TestData)) {
    TestData <- ModelDataPrep(
      data = TestData,
      Impute = TRUE,
      CharToFactor = TRUE,
      RemoveDates = TRUE,
      MissFactor = "0",
      MissNum = -1)
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
  if (SaveModelObjects) {
    data.table::fwrite(Names, paste0(model_path, "/", ModelID, "_ColNames.csv"))
  }
  
  # Binary Subset Target Variables----
  TrainTarget <- tryCatch({dataTrain[, get(Target)]}, error = function(x) dataTrain[, eval(Target)])
  if(TrainOnFull != TRUE) {
    TestTarget <- tryCatch({dataTest[, get(Target)]}, error = function(x) dataTest[, eval(Target)])
    if (!is.null(TestData)) {
      FinalTestTarget <- tryCatch({TestData[, get(Target)]}, error = function(x) TestData[, eval(Target)])
    }  
  }
  
  # Binary Initialize Catboost Data Conversion----
  if (!is.null(CatFeatures)) {
    if (!is.null(TestData)) {
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
    if (!is.null(TestData)) {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      if(TrainOnFull != TRUE) {
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
        FinalTestPool <- catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget)  
      }
    } else {
      TrainPool <- catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      if(TrainOnFull != TRUE) {
        TestPool <- catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
      }
    }
  }
  
  # Binary Grid Tune or Not Check----
  if (GridTune == TRUE & TrainOnFull != TRUE) {
    
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
    repeat {
    
      # Increment counter----
      counter <- counter + 1L
      
      # Check if grid still has elements in it----
      if(!is.null(GridClusters[[paste0("Grid_",max(1L,counter-1L))]][["L2_Leaf_Reg"]][1L])) {
        
        # Define parameters----
        if(!exists("NewGrid")) {
          base_params <- CatBoostClassifierParams(counter=counter,BanditArmsN=BanditArmsN,HasTime=HasTime,MetricPeriods=MetricPeriods,ClassWeights=ClassWeights,eval_metric=eval_metric,task_type=task_type,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        } else {
          base_params <- CatBoostClassifierParams(NewGrid=NewGrid,counter=counter,BanditArmsN=BanditArmsN,HasTime=HasTime,MetricPeriods=MetricPeriods,ClassWeights=ClassWeights,eval_metric=eval_metric,task_type=task_type,model_path=model_path,Grid=Grid,ExperimentalGrid=ExperimentalGrid,GridClusters=GridClusters)
        }
        
        # Build model----
        print(base_params)
        RunTime <- system.time(model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params))
        
        # Score model----
        if (!is.null(TestData)) {
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
        TotalRunTime <- sum(ExperimentalGrid[RunTime != -1L][["RunTime"]], na.rm = TRUE)
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
      for(bandit in seq_len(length(BanditProbs))) {
        data.table::set(ExperimentalGrid, i = counter+1L, j = paste0("BanditProbs_Grid_",bandit), value = BanditProbs[bandit])
      }
    }
    
    # Remove unneeded rows----
    ExperimentalGrid <- ExperimentalGrid[RunTime != -1L]
  }
  
  # Define parameters for case where you pass in a winning GridMetrics from grid tuning----
  if (!is.null(PassInGrid)) {
    if (tolower(task_type) == "gpu") {
      base_params <- list(
        has_time             = HasTime,
        metric_period        = 1L,
        loss_function        = "Logloss",
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
        metric_period        = 1L,
        loss_function        = "Logloss",
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
  if (GridTune & TrainOnFull == FALSE) {
    
    # Prepare winning grid----
    BestGrid <- ExperimentalGrid[order(-EvalMetric)][1L]
    if(tolower(task_type) == "gpu") grid_params <- as.list(BestGrid[, c(5L:12L)]) else grid_params <- as.list(BestGrid[, c(5L:11L)])
    if(tolower(task_type) == "gpu") grid_params <- grid_params[!names(grid_params) %chin% "RSM"]
    if(tolower(task_type) == "cpu") grid_params <- grid_params[!names(grid_params) %chin% "GrowPolicy"]
    
    # Set parameters from winning grid----
    if (BestGrid$RunNumber[1L] == 1L) {
      if (!is.null(ClassWeights)) {
        base_params <- list(
          use_best_model       = TRUE,
          best_model_min_trees = 10L,
          metric_period        = MetricPeriods,
          iterations           = BestGrid[["TreesBuilt"]],
          loss_function        = "Logloss",
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
          loss_function        = "Logloss",
          eval_metric          = eval_metric,
          has_time             = HasTime,
          task_type            = task_type)
      }
    } else {
      if (tolower(task_type) == "gpu") {
        base_params <- list(
          has_time             = HasTime,
          metric_period        = MetricPeriods,
          loss_function        = "Logloss",
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
          loss_function        = "Logloss",
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
    if (!is.null(ClassWeights)) {
      base_params <- list(
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        metric_period        = 10L,
        iterations           = Trees,
        loss_function        = "Logloss",
        eval_metric          = eval_metric,
        has_time             = HasTime,
        task_type            = task_type,
        class_weights        = ClassWeights)
    } else {
      base_params <- list(
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        metric_period        = 10L,
        iterations           = Trees,
        loss_function        = "Logloss",
        eval_metric          = eval_metric,
        has_time             = HasTime,
        task_type            = task_type)
    }
  }

  # Binary Train Final Model----
  if(!TrainOnFull) {
    model <- catboost::catboost.train(learn_pool = TrainPool, test_pool = TestPool, params = base_params)
  } else {
    model <- catboost::catboost.train(learn_pool = TrainPool, params = base_params)
  }
  
  # Binary Save Model----
  if (SaveModelObjects) catboost::catboost.save_model(model = model, model_path = paste0(model_path, "/", ModelID))
  
  # Binary Score Final Test Data----
  if(TrainOnFull != TRUE) {
    if (!is.null(TestData)) {
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
    if (!is.null(TestData)) {
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
  if (SaveModelObjects & TrainOnFull != TRUE) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(ValidationData, file = paste0(metadata_path, "/", ModelID, "_ValidationData.csv"))
    } else {
      data.table::fwrite(ValidationData, file = paste0(model_path, "/", ModelID, "_ValidationData.csv"))      
    }
  }
  
  # Binary AUC Object Create----
  if(!TrainOnFull) {
    AUC_Metrics <- pROC::roc(
      response = ValidationData[[eval(TargetColumnName)]],
      predictor = ValidationData[["p1"]],
      na.rm = TRUE,
      algorithm = 3,
      auc = TRUE,
      ci = TRUE)  
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
    if (GridTune == TRUE & MaxModelsInGrid <= 15L) {
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
    if (SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(paste0(metadata_path, "/", ModelID, "_ROC_Plot.png"))
      } else {
        ggplot2::ggsave(paste0(model_path, "/", ModelID, "_ROC_Plot.png"))      
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
  if(!TrainOnFull) {
    EvaluationPlot <- EvaluationPlot +
      ggplot2::ggtitle(paste0("Calibration Evaluation Plot: AUC = ",round(AUC_Metrics$auc, 3)))  
  }
  
  # Save plot to file----
  if(!TrainOnFull) {
    if (SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(paste0(metadata_path, "/", ModelID, "_EvaluationPlot.png"))
      } else {
        ggplot2::ggsave(paste0(model_path, "/", ModelID, "_EvaluationPlot.png"))      
      }
    }  
  }
  
  # Evaluation Metrics at Optimial Threshold----
  if(!TrainOnFull) {
    x <- ROCR::prediction(predictions = ValidationData[["p1"]],
                          labels = ValidationData[[eval(TargetColumnName)]])
    EvaluationMetrics <- data.table::data.table(
      Metric = c("AUC","TruePositiveRate","FalseNegativeRate","FalsePositiveRate","TrueNegativeRate",
                 "PreceisionRecallBreakEven","F1_Score","Odds"),
      MetricValue = rep(999999, 8),
      Threshold   = rep(999999, 8))
    i <- 0
    for (metric in c("auc", "tpr", "fnr", "fpr", "tnr", "prbe", "f", "odds")) {
      i <- as.integer(i + 1)
      tryCatch({
        y <- ROCR::performance(prediction.obj = x, measure = metric)
        if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                nrow(data.table::as.data.table(y@x.values)) <= 1)) {
          if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
              nrow(data.table::as.data.table(y@x.values)) <= 1) {
            z <- data.table::as.data.table(cbind(Metric = y@y.values,Threshold = y@x.values))
            Metric <- z[[1]]
          } else if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
                     !(nrow(data.table::as.data.table(y@x.values) <= 1))) {
            z <- data.table::as.data.table(cbind(
              Metric = y@y.values,
              Threshold = y@x.values[[1]]))
            Metric <- z[!is.infinite(Threshold)][[1]]
          } else if(!(nrow(data.table::as.data.table(y@y.values)) <= 1) &
                    nrow(data.table::as.data.table(y@x.values) <= 1)) {
            if (metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
              z <- data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values))
              Metric <- z[order(-Metric)][!is.infinite(Metric)][[1]]
            } else {
              z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]],Threshold = y@x.values))
              Metric <- z[order(Metric)][!is.infinite(Metric)][[1]]
            }
          }
        } else {
          if (metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
            z <- data.table::as.data.table(cbind(
              Metric = y@y.values[[1]],
              Threshold = y@x.values[[1]]))
            Metric <- z[order(-Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1,]
          } else {
            z <- data.table::as.data.table(cbind(Metric = y@y.values[[1]],Threshold = y@x.values[[1]]))
            Metric <- z[order(Metric)][!is.infinite(Threshold) & !is.infinite(Metric)][1,]
          }
        }
        
        # Store Output Information
        if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                nrow(data.table::as.data.table(y@x.values)) <= 1)) {
          data.table::set(EvaluationMetrics,i = i,j = 2L,value = round(Metric[[1]], 4))
          data.table::set(EvaluationMetrics,i = i,j = 3L,value = NA)
        } else {
          data.table::set(EvaluationMetrics,i = i,j = 2L,value = round(Metric[[1]], 4))
          data.table::set(EvaluationMetrics,i = i,j = 3L,value = Metric[[2]])
        }
      }, error = function(x) "skip")
    }
  }
  
  # Binary Accuracy Threshold and Metric----
  if(!TrainOnFull) {
    j <- 0
    x <- data.table::data.table(
      Metric = "Accuracy",
      MetricValue = 5.0,
      Threshold = seq(0.01, 0.99, 0.001))
    for (i in unique(x[["Threshold"]])) {
      j = as.integer(j + 1)
      Accuracy <- mean(ValidationData[, data.table::fifelse(
        (p1 > i & get(TargetColumnName) == 1) | (p1 < i & get(TargetColumnName) == 0), 1, 0)])
      data.table::set(x, i = j, j = 2L, value = round(Accuracy, 4))
    }
    data.table::setorderv(x, "MetricValue", order = -1, na.last = TRUE)
    x <- x[1, ]
    EvaluationMetrics <- data.table::rbindlist(list(EvaluationMetrics, x))
    
    # Save EvaluationMetrics to File
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
    if (SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvaluationMetrics, file = paste0(metadata_path, "/", ModelID, "_EvaluationMetrics.csv"))
      } else {
        data.table::fwrite(EvaluationMetrics, file = paste0(model_path, "/", ModelID, "_EvaluationMetrics.csv"))      
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
        if (SaveModelObjects) {
          if(!is.null(metadata_path)) {
            data.table::fwrite(VariableImportance, file = paste0(metadata_path, "/", ModelID, "_VariableImportance.csv"))
          } else {
            data.table::fwrite(VariableImportance, file = paste0(model_path, "/", ModelID, "_VariableImportance.csv"))      
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
          data.table::fwrite(VariableImportance, file = paste0(metadata_path, "/", ModelID, "_VariableImportance.csv"))
        } else {
          data.table::fwrite(VariableImportance, file = paste0(model_path, "/", ModelID, "_VariableImportance.csv"))      
        }
      }
    }
  } else {
    temp <- catboost::catboost.get_feature_importance(model)
    VariableImportance <- data.table::data.table(cbind(Variable = rownames(temp), temp))
    data.table::setnames(VariableImportance, "V2", "Importance")
    VariableImportance[, Importance := round(as.numeric(Importance), 4)]
    VariableImportance <- VariableImportance[order(-Importance)]
    if (SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(VariableImportance, file = paste0(metadata_path, "/", ModelID, "_VariableImportance.csv"))
      } else {
        data.table::fwrite(VariableImportance, file = paste0(model_path, "/", ModelID, "_VariableImportance.csv"))      
      }
    }
  }
  
  # Binary Partial Dependence----
  ParDepPlots <- list()
  j <- 0
  ParDepBoxPlots <- list()
  k <- 0
  if(!is.null(VariableImportance)) {
    for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
      tryCatch({
        Out <- ParDepCalPlots(
          data = ValidationData,
          PredictionColName = "p1",
          TargetColName = eval(TargetColumnName),
          IndepVar = VariableImportance[i, Variable],
          GraphType = "calibration",
          PercentileBucket = 0.05,
          FactLevels = 10,
          Function = function(x) mean(x, na.rm = TRUE))
        j <- j + 1
        ParDepPlots[[paste0(VariableImportance[j, Variable])]] <- Out
      }, error = function(x) "skip")
    }
  } else {
    ParDepPlots <- NULL
  }
  
  # Binary Save ParDepPlots to file----
  if (SaveModelObjects) {
    if(!is.null(metadata_path)) {
      if(!is.null(VariableImportance)) save(ParDepPlots, file = paste0(metadata_path, "/", ModelID, "_ParDepPlots.R"))
    } else {
      if(!is.null(VariableImportance)) save(ParDepPlots, file = paste0(model_path, "/", ModelID, "_ParDepPlots.R"))      
    }
  }
  
  # Binary Save GridCollect and ExperimentalGrid----
  if (SaveModelObjects & GridTune == TRUE) {
    if(!is.null(metadata_path)) {
      data.table::fwrite(ExperimentalGrid, file = paste0(metadata_path, "/", ModelID, "_ExperimentalGrid.csv"))
    } else {
      data.table::fwrite(ExperimentalGrid, file = paste0(model_path, "/", ModelID, "_ExperimentalGrid.csv"))
    }
  }
  
  # Final Garbage Collection----
  gc()
  
  # VI_Plot_Function----
  VI_Plot <- function(VI_Data, ColorHigh = "darkblue", ColorLow = "white") {
    ggplot2::ggplot(VI_Data[1:min(10,.N)], ggplot2::aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_gradient2(mid = ColorLow,high = ColorHigh) +
      ChartTheme(Size = 12, AngleX = 0, LegendPosition = "right") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Global Variable Importance") +
      ggplot2::xlab("Top Model Features") +
      ggplot2::ylab("Value")
  }
  
  # Binary Return Model Objects----
  if (GridTune) {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          ROC_Plot = ROC_Plot,
          EvaluationPlot = EvaluationPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          VI_Plot = VI_Plot(VariableImportance),
          PartialDependencePlots = ParDepPlots,
          GridMetrics = data.table::setorderv(ExperimentalGrid, cols = "EvalMetric", order = -1L, na.last = TRUE),
          ColNames = Names))
    }
  } else if(TrainOnFull) {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          VariableImportance = VariableImportance,
          VI_Plot = VI_Plot(VariableImportance),
          ColNames = Names))
    }
  } else {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          ROC_Plot = ROC_Plot,
          EvaluationPlot = EvaluationPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          VI_Plot = VI_Plot(VariableImportance),
          PartialDependencePlots = ParDepPlots,
          ColNames = Names))
    }
  }
}
