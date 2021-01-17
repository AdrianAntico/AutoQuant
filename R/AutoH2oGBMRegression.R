#' AutoH2oGBMRegression is an automated H2O modeling framework with grid-tuning and model evaluation
#'
#' AutoH2oGBMRegression is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Automated Supervised Learning - Regression
#' @param data This is your data set for training and testing your model
#' @param TrainOnFull Set to TRUE to train on full data
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param WeightsColumn Column name of a weights column
#' @param TransformNumericColumns Set to NULL to do nothing; otherwise supply the column names of numeric variables you want transformed
#' @param Methods Choose from "YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", or "Logit". If more than one is selected, the one with the best normalization pearson statistic will be used. Identity is automatically selected and compared.
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param metadata_path A character string of your path file to where you want your model evaluation output saved. If left NULL, all output will be saved to model_path.
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param SaveInfoToPDF Set to TRUE to save insights to PDF
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @param H2OStartUp Defaults to TRUE which means H2O will be started inside the function
#' @param H2OShutdown Set to TRUE to shutdown H2O inside the function
#' @param Alpha This is the quantile value you want to use for quantile regression. Must be a decimal between 0 and 1.
#' @param MaxMem Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"
#' @param NThreads Set to the mamimum amount of threads you want to use for this function
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param Distribution Choose from gaussian",  "poisson",  "gamma",  "tweedie",  "laplace",  "quantile", "huber"
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "MSE", "RMSE", "MAE", "RMSLE"
#' @param GridStrategy Default "Cartesian"
#' @param StoppingRounds Number of runs
#' @param MaxRuntimeSecs Default 60*60*24
#' @param Trees The maximum number of trees you want in your models
#' @param MaxDepth Default 20
#' @param LearnRate Default 0.10
#' @param LearnRateAnnealing Default 1
#' @param SampleRate Default 0.632
#' @param ColSampleRate Default 1
#' @param ColSampleRatePerTree Default 1
#' @param ColSampleRatePerTreeLevel Default 1
#' @param MinRows Default 1
#' @param NBins Default 20
#' @param NBinsCats Default 1024
#' @param NBinsTopLevel Default 1024
#' @param HistogramType Default "AUTO"
#' @param CategoricalEncoding Default "AUTO"
#' @examples
#' \donttest{
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
#' TestModel <- RemixAutoML::AutoH2oGBMRegression(
#'
#'     # Compute management
#'     MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
#'     NThreads = max(1, parallel::detectCores()-2),
#'     H2OShutdown = TRUE,
#'     H2OStartUp = TRUE,
#'     IfSaveModel = "mojo",
#'
#'     # Model evaluation
#'     NumOfParDepPlots = 3,
#'
#'     # Metadata arguments:
#'     model_path = normalizePath("./"),
#'     metadata_path = file.path(normalizePath("./")),
#'     ModelID = "FirstModel",
#'     ReturnModelObjects = TRUE,
#'     SaveModelObjects = FALSE,
#'     SaveInfoToPDF = FALSE,
#'
#'     # Data arguments
#'     data = data,
#'     TrainOnFull = FALSE,
#'     ValidationData = NULL,
#'     TestData = NULL,
#'     TargetColumnName = "Adrian",
#'     FeatureColNames = names(data)[!names(data) %in% c("IDcol_1", "IDcol_2","Adrian")],
#'     WeightsColumn = NULL,
#'     TransformNumericColumns = NULL,
#'     Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Sqrt", "Logit","YeoJohnson"),
#'
#'     # ML grid tuning args
#'     GridTune = FALSE,
#'     GridStrategy = "Cartesian",
#'     MaxRuntimeSecs = 60*60*24,
#'     StoppingRounds = 10,
#'     MaxModelsInGrid = 2,
#'
#'     # Model args
#'     Trees = 50,
#'     LearnRate = 0.10,
#'     LearnRateAnnealing = 1,
#'     eval_metric = "RMSE",
#'     Alpha = NULL,
#'     Distribution = "poisson",
#'     MaxDepth = 20,
#'     SampleRate = 0.632,
#'     ColSampleRate = 1,
#'     ColSampleRatePerTree = 1,
#'     ColSampleRatePerTreeLevel  = 1,
#'     MinRows = 1,
#'     NBins = 20,
#'     NBinsCats = 1024,
#'     NBinsTopLevel = 1024,
#'     HistogramType = "AUTO",
#'     CategoricalEncoding = "AUTO")
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, GridList, and metadata
#' @export
AutoH2oGBMRegression <- function(data,
                                 TrainOnFull = FALSE,
                                 ValidationData,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 WeightsColumn = NULL,
                                 TransformNumericColumns = NULL,
                                 Methods = c("YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"),
                                 MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
                                 NThreads = max(1,parallel::detectCores()-2),
                                 model_path = NULL,
                                 metadata_path = NULL,
                                 ModelID = "FirstModel",
                                 NumOfParDepPlots = 3,
                                 ReturnModelObjects = TRUE,
                                 SaveModelObjects = FALSE,
                                 SaveInfoToPDF = FALSE,
                                 IfSaveModel = "mojo",
                                 H2OShutdown = TRUE,
                                 H2OStartUp = TRUE,
                                 GridTune = FALSE,
                                 GridStrategy = "Cartesian",
                                 MaxRuntimeSecs = 60*60*24,
                                 StoppingRounds = 10,
                                 MaxModelsInGrid = 2,
                                 eval_metric = "RMSE",
                                 Trees = 50,
                                 LearnRate = 0.10,
                                 LearnRateAnnealing = 1,
                                 Alpha = NULL,
                                 Distribution = "poisson",
                                 MaxDepth = 20,
                                 SampleRate = 0.632,
                                 MTries = -1,
                                 ColSampleRate = 1,
                                 ColSampleRatePerTree = 1,
                                 ColSampleRatePerTreeLevel  = 1,
                                 MinRows = 1,
                                 NBins = 20,
                                 NBinsCats = 1024,
                                 NBinsTopLevel = 1024,
                                 HistogramType = "AUTO",
                                 CategoricalEncoding = "AUTO") {

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Ensure model_path and metadata_path exists----
  if(!is.null(model_path)) if(!dir.exists(file.path(normalizePath(model_path)))) dir.create(normalizePath(model_path))
  if(!is.null(metadata_path)) if(!is.null(metadata_path)) if(!dir.exists(file.path(normalizePath(metadata_path)))) dir.create(normalizePath(metadata_path))

  # Regression Check Arguments----
  if(!(tolower(eval_metric) %chin% c("mse", "rmse", "mae", "rmsle"))) stop("eval_metric not in MSE, RMSE, MAE, RMSLE")
  if(Trees < 1) stop("Trees must be greater than 1")
  if(!GridTune %in% c(TRUE, FALSE)) stop("GridTune needs to be TRUE or FALSE")
  if(MaxModelsInGrid < 1 & GridTune) stop("MaxModelsInGrid needs to be at least 1")
  if(!is.null(model_path)) if(!is.character(model_path)) stop("model_path needs to be a character type")
  if(!is.null(metadata_path)) if(!is.character(metadata_path)) stop("metadata_path needs to be a character type")
  if(!is.character(ModelID) & !is.null(ModelID)) stop("ModelID needs to be a character type")
  if(NumOfParDepPlots < 0) stop("NumOfParDepPlots needs to be a positive number")
  if(!(ReturnModelObjects %in% c(TRUE, FALSE))) stop("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects %in% c(TRUE, FALSE))) stop("SaveModelObjects needs to be TRUE or FALSE")

  # Regression Ensure data is a data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Regression Ensure data is a data.table----
  if(!is.null(ValidationData)) if(!data.table::is.data.table(ValidationData)) data.table::setDT(ValidationData)

  # Regression Ensure data is a data.table----
  if(!is.null(TestData)) if(!data.table::is.data.table(TestData)) data.table::setDT(TestData)

  # Convert TransformNumericColumns to Names if not character----
  if(!is.null(TransformNumericColumns)) if(!is.character(TransformNumericColumns)) TransformNumericColumns <- names(data)[TransformNumericColumns]

  # Transform data, ValidationData, and TestData----
  if(!is.null(ValidationData) & !is.null(TransformNumericColumns)) {
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
    dataTest <- AutoTransformationScore(
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
      dataSets <- AutoDataPartition(
        data,
        NumDataSets = 3,
        Ratios = c(0.70, 0.20, 0.10),
        PartitionType = "random",
        StratifyColumnNames = NULL,
        TimeColumnName = NULL)
      dataTrain <- dataSets$TrainData
      dataTest <- dataSets$ValidationData
      TestData <- dataSets$TestData
      rm(dataSets)

      # Transform data sets----
      Output <- AutoTransformationCreate(
        dataTrain,
        ColumnNames = TransformNumericColumns,
        Methods = Methods,
        Path = model_path,
        TransID = ModelID,
        SaveOutput = SaveModelObjects)
      dataTrain <- Output$Data
      TransformationResults <- Output$FinalResults

      # Transform ValidationData----
      dataTest <- AutoTransformationScore(
        ScoringData = dataTest,
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
        NumDataSets = 3,
        Ratios = c(0.70, 0.20, 0.10),
        PartitionType = "random",
        StratifyColumnNames = NULL,
        TimeColumnName = NULL)
      dataTrain <- dataSets$TrainData
      dataTest <- dataSets$ValidationData
      TestData <- dataSets$TestData
      rm(dataSets)
    }
  }

  # Create dataTrain if not exists ----
  if(!exists("dataTrain")) dataTrain <- data
  if(!exists("dataTest") && !TrainOnFull) dataTest <- ValidationData

  # Regression ModelDataPrep----
  dataTrain <- ModelDataPrep(data = dataTrain, Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = TRUE, LogicalToBinary = TRUE, DateToChar = TRUE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
  dataTrain <- ModelDataPrep(data = dataTrain, Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
  if(!TrainOnFull) {
    dataTest <- ModelDataPrep(data = dataTest, Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = TRUE, LogicalToBinary = TRUE, DateToChar = TRUE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
    dataTest <- ModelDataPrep(data = dataTest, Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
  }
  if(!is.null(TestData)) {
    TestData <- ModelDataPrep(data = TestData, Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = TRUE, LogicalToBinary = TRUE, DateToChar = TRUE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
    TestData <- ModelDataPrep(data = TestData, Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
  }

  # Regression Get Min Value of Target Data----
  MinVal <- min(data[[eval(TargetColumnName)]], na.rm = TRUE)

  # Regression Proper Distribution Check----
  if(MinVal < 0 & tolower(Distribution) == "poisson") {
    Distribution <- "gaussian"
  } else if(MinVal == 0 & tolower(Distribution) %chin% c("gamma", "tweedie")) {
    Distribution <- "poisson"
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

  # Regression Grid Tune Check----
  if(GridTune & !TrainOnFull) {
    if(H2OStartUp) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)

    # Regression Grid Tune Search Criteria----
    search_criteria  <- list(
      strategy = GridStrategy,
      max_runtime_secs = MaxRuntimeSecs,
      max_models = MaxModelsInGrid,
      seed = 1234,
      stopping_rounds = StoppingRounds,
      stopping_metric = toupper(eval_metric),
      stopping_tolerance = 1e-3)

    # Regression Grid Parameters ----
    hyper_params <- list()
    hyper_params[["ntrees"]] <- Trees
    hyper_params[["max_depth"]] <- MaxDepth
    hyper_params[["learn_rate"]] <- LearnRate
    hyper_params[["learn_rate_annealing"]] <- LearnRateAnnealing
    hyper_params[["sample_rate"]] <- SampleRate
    hyper_params[["col_sample_rate"]] <- ColSampleRate
    hyper_params[["col_sample_rate_per_tree"]] <- ColSampleRatePerTree
    hyper_params[["col_sample_rate_change_per_level"]] <- ColSampleRatePerTreeLevel
    hyper_params[["min_rows"]] <- MinRows
    hyper_params[["nbins"]] <- NBins
    hyper_params[["nbins_cats"]] <- NBinsCats
    hyper_params[["histogram_type"]] <- HistogramType
    hyper_params[["nbins_top_level"]] <- NBinsTopLevel
    hyper_params[["categorical_encoding"]] <- CategoricalEncoding
    hyper_params[["distribution"]] <- Distribution

    # Regression Grid Train Model----
    if(!is.null(Alpha)) {
      grid <- h2o::h2o.grid(
        hyper_params = hyper_params,
        search_criteria = search_criteria,
        is_supervised = TRUE,
        algorithm = "gbm",
        grid_id = paste0(ModelID, "_Grid"),
        x = FeatureColNames,
        y = TargetColumnName,
        weights_column = WeightsColumn,
        ntrees = Trees,
        training_frame = datatrain,
        validation_frame = datavalidate,
        quantile_alpha = Alpha,
        score_tree_interval = 10,
        seed = 1234)
    } else {
      if(tolower(Distribution) == "tweedie") {
        grid <- h2o::h2o.grid(
          hyper_params = hyper_params,
          search_criteria = search_criteria,
          is_supervised = TRUE,
          algorithm = "gbm",
          grid_id = paste0(ModelID, "_Grid"),
          x = FeatureColNames,
          y = TargetColumnName,
          weights_column = WeightsColumn,
          training_frame = datatrain,
          validation_frame = datavalidate,
          tweedie_power = 1.5,
          score_tree_interval = 10,
          seed = 1234)
      } else {
        grid <- h2o::h2o.grid(
          hyper_params = hyper_params,
          search_criteria = search_criteria,
          is_supervised = TRUE,
          algorithm = "gbm",
          grid_id = paste0(ModelID, "_Grid"),
          x = FeatureColNames,
          y = TargetColumnName,
          weights_column = WeightsColumn,
          training_frame = datatrain,
          validation_frame = datavalidate,
          score_tree_interval = 10,
          seed = 1234)
      }
    }

    # Regression Get Best Model----
    Grid_Out <- h2o::h2o.getGrid(grid_id = paste0(ModelID, "_Grid"), sort_by = eval_metric, decreasing = FALSE)

    # Regression Collect Best Grid Model----
    grid_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1L]])
  }

  # Regression Start Up H2O----
  if(!GridTune) {
    if(H2OStartUp) localHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
    datatrain <- h2o::as.h2o(dataTrain, use_datatable = TRUE)
    if(!TrainOnFull) datavalidate <- h2o::as.h2o(dataTest, use_datatable = TRUE) else datavalidate <- NULL
  }

  # Define args ----
  H2OArgs <- list()
  H2OArgs[["x"]] <- FeatureColNames
  H2OArgs[["y"]] <- TargetColumnName
  H2OArgs[["weights_column"]] <- WeightsColumn[1L]
  H2OArgs[["training_frame"]] <- datatrain
  H2OArgs[["validation_frame"]] <- datavalidate
  H2OArgs[["distribution"]] <- Distribution[1L]
  H2OArgs[["quantile_alpha"]] <- Alpha[1L]
  H2OArgs[["model_id"]] <- ModelID[1L]
  H2OArgs[["ntrees"]] <- Trees[1L]
  H2OArgs[["max_depth"]] <- MaxDepth[1L]
  H2OArgs[["learn_rate"]] <- LearnRate[1L]
  H2OArgs[["learn_rate_annealing"]] <- LearnRateAnnealing[1L]
  H2OArgs[["sample_rate"]] <- SampleRate[1L]
  H2OArgs[["col_sample_rate"]] <- ColSampleRate[1L]
  H2OArgs[["col_sample_rate_per_tree"]] <- ColSampleRatePerTree[1L]
  H2OArgs[["col_sample_rate_change_per_level"]] <- ColSampleRatePerTreeLevel[1L]
  H2OArgs[["min_rows"]] <- MinRows[1L]
  H2OArgs[["nbins"]] <- NBins[1L]
  H2OArgs[["nbins_cats"]] <- NBinsCats[1L]
  H2OArgs[["nbins_top_level"]] <- NBinsTopLevel[1L]
  H2OArgs[["histogram_type"]] <- HistogramType[1L]
  H2OArgs[["categorical_encoding"]] <- CategoricalEncoding[1L]

  # Build model ----
  base_model <- do.call(h2o::h2o.gbm, H2OArgs)

  # Regression Grab Evaluation Metric ----
  if(GridTune & !TrainOnFull) {
    if(!is.null(TestData)) {
      datatest <-  h2o::as.h2o(TestData)
      if(tolower(eval_metric) == "mse") {
        GridModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = grid_model, newdata = datatest))
        BaseModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = base_model, newdata = datatest))
      } else if(tolower(eval_metric) == "rmse") {
        GridModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = grid_model, newdata = datatest))
        BaseModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = base_model, newdata = datatest))
      } else if(tolower(eval_metric) == "mae") {
        GridModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = grid_model, newdata = datatest))
        BaseModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = base_model, newdata = datatest))
      } else if(tolower(eval_metric) == "rmsle") {
        GridModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = grid_model, newdata = datatest))
        BaseModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = base_model, newdata = datatest))
      }
    } else {
      if(tolower(eval_metric) == "mse") {
        GridModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = grid_model, newdata = datavalidate))
        BaseModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      } else if(tolower(eval_metric) == "rmse") {
        GridModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = grid_model, newdata = datavalidate))
        BaseModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      } else if(tolower(eval_metric) == "mae") {
        GridModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = grid_model, newdata = datavalidate))
        BaseModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      } else if(tolower(eval_metric) == "rmsle") {
        GridModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = grid_model, newdata = datavalidate))
        BaseModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      }
    }
  } else if(!TrainOnFull) {
    if(!is.null(TestData)) {
      datatest <-  h2o::as.h2o(TestData)
      if(tolower(eval_metric) == "mse") {
        BaseModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = base_model, newdata = datatest))
      } else if (tolower(eval_metric) == "rmse") {
        BaseModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = base_model, newdata = datatest))
      } else if (tolower(eval_metric) == "mae") {
        BaseModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = base_model, newdata = datatest))
      } else if (tolower(eval_metric) == "rmsle") {
        BaseModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = base_model, newdata = datatest))
      }
    } else if(!is.null(ValidationData)) {
      if(tolower(eval_metric) == "mse") {
        BaseModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      } else if(tolower(eval_metric) == "rmse") {
        BaseModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      } else if(tolower(eval_metric) == "mae") {
        BaseModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      } else if(tolower(eval_metric) == "rmsle") {
        BaseModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = base_model, newdata = datavalidate))
      }
    } else {
      if(tolower(eval_metric) == "mse") {
        BaseModelEval <- h2o::h2o.mse(h2o::h2o.performance(model = base_model, newdata = datatrain))
      } else if(tolower(eval_metric) == "rmse") {
        BaseModelEval <- h2o::h2o.rmse(h2o::h2o.performance(model = base_model, newdata = datatrain))
      } else if(tolower(eval_metric) == "mae") {
        BaseModelEval <- h2o::h2o.mae(h2o::h2o.performance(model = base_model, newdata = datatrain))
      } else if(tolower(eval_metric) == "rmsle") {
        BaseModelEval <- h2o::h2o.rmsle(h2o::h2o.performance(model = base_model, newdata = datatrain))
      }
    }
  }

  # Regression Pick Winner ----
  if(GridTune & !TrainOnFull) {
    if(GridModelEval > BaseModelEval) {
      FinalModel <- grid_model
    } else {
      FinalModel <- base_model
    }
  } else {
    FinalModel <- base_model
  }

  # Regression Save Model ----
  if(SaveModelObjects) {
    if(tolower(IfSaveModel) == "mojo") {
      SaveModel <- h2o::h2o.save_mojo(object = FinalModel, path = model_path, force = TRUE)
      h2o::h2o.download_mojo(
        model = FinalModel,
        path = model_path,
        get_genmodel_jar = TRUE,
        genmodel_path = model_path,
        genmodel_name = ModelID)
    } else {
      SaveModel <- h2o::h2o.saveModel(object = FinalModel, path = model_path, force = TRUE)
    }
  }

  # Regression Score Final Test Data ----
  if(!is.null(TestData)) {
    Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel, newdata = datatest))
  } else if(!is.null(ValidationData) & TrainOnFull == FALSE) {
    Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel, newdata = datavalidate))
  } else {
    Predict <- data.table::as.data.table(h2o::h2o.predict(object = FinalModel, newdata = datatrain))
  }

  # Regression Variable Importance ----
  if(!TrainOnFull) {
    VariableImportance <- data.table::as.data.table(h2o::h2o.varimp(object = FinalModel))
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(VariableImportance, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_VariableImportance.csv")))
      } else {
        data.table::fwrite(VariableImportance, file = file.path(normalizePath(model_path), paste0(ModelID, "_VariableImportance.csv")))
      }
    }

    # Regression Format Variable Importance Table ----
    data.table::setnames(
      VariableImportance,
      c("variable", "relative_importance", "scaled_importance", "percentage" ),
      c("Variable", "RelativeImportance", "ScaledImportance", "Percentage"))
    VariableImportance[, ':=' (RelativeImportance = round(RelativeImportance, 4L), ScaledImportance = round(ScaledImportance, 4), Percentage = round(Percentage, 4))]
  }

  # Regression H2O Shutdown ----
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Regression Create Validation Data ----
  if(!is.null(TestData)) {
    ValidationData <- data.table::as.data.table(cbind(TestData, Predict))
  } else if(!is.null(ValidationData) & !TrainOnFull) {
    ValidationData <- data.table::as.data.table(cbind(dataTest, Predict))
  } else {
    ValidationData <- data.table::as.data.table(cbind(dataTrain, Predict))
  }

  # Regression Change Prediction Name ----
  data.table::setnames(ValidationData, "predict", "Predict")

  # Inverse Transform ----
  if(!is.null(TransformNumericColumns)) {
    if(GridTune) TransformationResults <- TransformationResults[ColumnName != "Predict"]
    TransformationResults <- data.table::rbindlist(list(
      TransformationResults,
      data.table::data.table(
        ColumnName = "Predict",
        MethodName = rep(TransformationResults[ColumnName == eval(TargetColumnName), MethodName], 1),
        Lambda = rep(TransformationResults[ColumnName == eval(TargetColumnName), Lambda], 1),
        NormalizedStatistics = rep(0, 1))))

    # If Actual target columnname == "Target" remove the duplicate version ----
    if(length(unique(TransformationResults[["ColumnName"]])) != nrow(TransformationResults)) {
      temp <- TransformationResults[, .N, by = "ColumnName"][N != 1][[1L]]
      temp1 <- which(names(ValidationData) == temp)[1L]
      ValidationData[, eval(names(data)[temp1]) := NULL]
      TransformationResults <- TransformationResults[, ID := 1:.N][ID != which(TransformationResults[["ID"]] == temp1)][, ID := NULL]
    }

    # Transform Target and Predicted Value ----
    ValidationData <- AutoTransformationScore(
      ScoringData = ValidationData,
      Type = "Inverse",
      FinalResults = TransformationResults,
      TransID = NULL,
      Path = NULL)
  }

  # Regression Get R2 ----
  if(!TrainOnFull) r_squared <- (ValidationData[, stats::cor(get(TargetColumnName), Predict)][[1]]) ^ 2L

  # Regression Save Validation Data to File ----
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

  # Model Insights ----
  if(!TrainOnFull) {

    # Eval Plot
    EvaluationPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "Predict",
      TargetColName = TargetColumnName,
      GraphType = "calibration",
      PercentileBucket = 0.05,
      aggrfun = if(!is.null(Alpha)) {function(x) quantile(x, probs = Alpha, na.rm = TRUE)} else {function(x) mean(x, na.rm = TRUE)})

    # Regression Evaluation Plot Update Title----
    if(GridTune) {
      val <- max(GridModelEval, BaseModelEval)
      EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("GBM Calibration Evaluation Plot: ", toupper(eval_metric), " = ", round(val, 3L)))
    } else {
      EvaluationPlot <- EvaluationPlot + ggplot2::ggtitle(paste0("GBM Calibration Evaluation Plot: ", toupper(eval_metric), " = ", round(BaseModelEval, 3L)))
    }

    # Save plot to file ----
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationPlot.png")))
      } else {
        ggplot2::ggsave(file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationPlot.png")))
      }
    }

    # Eval BoxPlot ----
    EvaluationBoxPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "Predict",
      TargetColName = TargetColumnName,
      GraphType = "boxplot",
      PercentileBucket = 0.05,
      aggrfun = function(x) mean(x, na.rm = TRUE))

    # Regression Evaluation BoxPlot Update Title ----
    if(GridTune) {
      val <- max(GridModelEval, BaseModelEval)
      EvaluationBoxPlot <- EvaluationBoxPlot + ggplot2::ggtitle(paste0("GBM Calibration Evaluation Plot: ", toupper(eval_metric), " = ", round(val, 3L)))
    } else {
      EvaluationBoxPlot <- EvaluationBoxPlot + ggplot2::ggtitle(paste0("GBM Calibration Evaluation Plot: ", toupper(eval_metric), " = ", round(BaseModelEval, 3L)))
    }

    # Save plot to file ----
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        ggplot2::ggsave(file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationBoxPlot.png")))
      } else {
        ggplot2::ggsave(file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationBoxPlot.png")))
      }
    }

    # Regression Evaluation Metrics ----
    EvaluationMetrics <- data.table::data.table(Metric = c("MAE","MAPE","RMSE","R2"), MetricValue = rep(999999, 8L))
    i <- 0L
    for(metric in c("mae", "mape", "rmse", "r2")) {
      i <- i + 1L
      tryCatch({
        if (tolower(metric) == "mae") {
          ValidationData[, Metric := abs(get(TargetColumnName) - Predict)]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == "mape") {
          ValidationData[, Metric := abs((get(TargetColumnName) - Predict) / (get(TargetColumnName) + 1))]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        } else if(tolower(metric) == "rmse") {
          ValidationData[, Metric := (get(TargetColumnName) - Predict) ^ 2]
          Metric <- sqrt(ValidationData[, mean(Metric, na.rm = TRUE)])
        } else if(tolower(metric) == "r2") {
          Metric <- (ValidationData[, stats::cor(get(TargetColumnName), Predict)][[1L]]) ^ 2L
        }
        data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(Metric, 4L))
      }, error = function(x) "skip")
    }

    # Remove Features ----
    ValidationData[, ':=' (Metric  = NULL)]

    # Regression Save EvaluationMetrics to File ----
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
    if(SaveModelObjects) {
      if(!is.null(metadata_path)) {
        data.table::fwrite(EvaluationMetrics, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_EvaluationMetrics.csv")))
      } else {
        data.table::fwrite(EvaluationMetrics, file = file.path(normalizePath(model_path), paste0(ModelID, "_EvaluationMetrics.csv")))
      }
    }

    # Regression Partial Dependence ----
    ParDepPlots <- list()
    ParDepBoxPlots <- list()
    if(NumOfParDepPlots != 0) {
      j <- 0L
      k <- 0L
      for(i in seq_len(min(length(FeatureColNames), NumOfParDepPlots, VariableImportance[,.N]))) {
        tryCatch({
          Out <- ParDepCalPlots(
            data = ValidationData,
            PredictionColName = "Predict",
            TargetColName = TargetColumnName,
            IndepVar = gsub("\\..*","",VariableImportance[i, Variable]),
            GraphType = "calibration",
            PercentileBucket = 0.05,
            FactLevels = 10L,
            Function = if(!is.null(Alpha)) {function(x) quantile(x, probs = Alpha, na.rm = TRUE)} else {function(x) mean(x, na.rm = TRUE)})
          j <- j + 1L
          ParDepPlots[[paste0(VariableImportance[j, Variable])]] <- Out
        }, error = function(x) "skip")
        tryCatch({
          Out1 <- ParDepCalPlots(
            data = ValidationData,
            PredictionColName = "Predict",
            TargetColName = TargetColumnName,
            IndepVar = gsub("\\..*","",VariableImportance[i, Variable]),
            GraphType = "boxplot",
            PercentileBucket = 0.05,
            FactLevels = 10L,
            Function = if(!is.null(Alpha)) {function(x) quantile(x, probs = Alpha, na.rm = TRUE)} else {function(x) mean(x, na.rm = TRUE)})
          k <- k + 1L
          ParDepBoxPlots[[paste0(VariableImportance[k, Variable])]] <- Out1
        }, error = function(x) "skip")
      }

      # Regression Save ParDepPlots to file ----
      if(SaveModelObjects) {
        if(!is.null(metadata_path)) {
          save(ParDepPlots, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ParDepPlots.R")))
        } else {
          save(ParDepPlots, file = file.path(normalizePath(model_path), paste0(ModelID, "_ParDepPlots.R")))
        }
      }

      # Regression Save ParDepBoxPlots to file ----
      if(SaveModelObjects) {
        if(!is.null(metadata_path)) {
          save(ParDepBoxPlots, file = file.path(normalizePath(metadata_path), paste0(ModelID, "_ParDepBoxPlots.R")))
        } else {
          save(ParDepBoxPlots, file = file.path(normalizePath(model_path), paste0(ModelID, "_ParDepBoxPlots.R")))
        }
      }
    }
  }

  # Subset Transformation Object ----
  if(!is.null(TransformNumericColumns)) {
    if(TargetColumnName == "Target") {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict"))]
    } else {
      TransformationResults <- TransformationResults[!(ColumnName %chin% c("Predict", eval(TargetColumnName)))]
    }
  }

  # VI_Plot_Function
  VI_Plot <- function(VI_Data, ColorHigh = "darkblue", ColorLow = "white") {
    ggplot2::ggplot(VI_Data[1:min(10,.N)], ggplot2::aes(x = reorder(Variable, ScaledImportance ), y = ScaledImportance , fill = ScaledImportance )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_gradient2(mid = ColorLow,high = ColorHigh) +
      ChartTheme(Size = 12,AngleX = 0,LegendPosition = "right") +
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

  # Regression Return Objects ----
  if(ReturnModelObjects) {
    if(!TrainOnFull) {
      return(list(
        Model = FinalModel,
        ValidationData = ValidationData,
        EvaluationPlot = EvaluationPlot,
        EvaluationBoxPlot = EvaluationBoxPlot,
        EvaluationMetrics = EvaluationMetrics,
        VariableImportance = VariableImportance,
        VI_Plot = VI_Plot(VI_Data = VariableImportance),
        PartialDependencePlots = ParDepPlots,
        PartialDependenceBoxPlots = ParDepBoxPlots,
        TransformationInformation = if(!is.null(TransformNumericColumns)) TransformationResults else NULL,
        ColNames = Names))
    } else {
      return(list(
        Model = FinalModel,
        ValidationData = ValidationData,
        TransformationInformation = if(!is.null(TransformNumericColumns)) TransformationResults else NULL,
        ColNames = Names))
    }
  }
}
