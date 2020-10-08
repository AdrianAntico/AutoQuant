#' AutoCatBoostSizeFreqDist
#'
#' AutoCatBoostSizeFreqDist for building size and frequency distributions via quantile regressions. Size (or severity) and frequency (or count) quantile regressions are build. Use this with the AutoQuantileGibbsSampler function to simulate the joint distribution.
#'
#' @author Adrian Antico
#' @family Supervised Learning - Compound
#' @param CountData This is your CountData generated from the IntermittentDemandBootStrapper() function
#' @param SizeData This is your SizeData generated from the IntermittentDemandBootStrapper() function
#' @param CountQuantiles The default are deciles, i.e. seq(0.10,0.90,0.10). More granularity the better, but it will take longer to run.
#' @param SizeQuantiles The default are deciles, i.e. seq(0.10,0.90,0.10). More granularity the better, but it will take longer to run.
#' @param AutoTransform Set to FALSE not to have the your target variables automatically transformed for the best normalization.
#' @param DataPartitionRatios The default is c(0.75,0.20,0.05). With CatBoost, you should allocate a decent amount to the validation data (second input). Three inputs are required.
#' @param StratifyColumnNames Specify grouping variables to stratify by
#' @param NTrees Default is 1500. If the best model utilizes all trees, you should consider increasing the argument.
#' @param TaskType The default is set to "GPU". If you do not have a GPU, set it to "CPU".
#' @param EvalMetric Set to "Quantile". Alternative quantile methods may become available in the future.
#' @param GridTune The default is set to FALSE. If you set to TRUE, make sure to specify MaxModelsGrid to a number greater than 1.
#' @param GridEvalMetric The default is set to "mae". Choose from 'poisson', 'mae', 'mape', 'mse', 'msle', 'kl', 'cs', 'r2'.
#' @param CountTargetColumnName Column names or column numbers
#' @param SizeTargetColumnName Column names or column numbers
#' @param CountFeatureColNames Column names or column numbers
#' @param SizeFeatureColNames Column names or column numbers
#' @param CountIDcols Column names or column numbers
#' @param SizeIDcols Column names or column numbers
#' @param ModelIDs A two element character vector. E.g. c("CountModel","SizeModel")
#' @param MaxModelsGrid Set to a number greater than 1 if GridTune is set to TRUE
#' @param ModelPath This path file is where all your models will be stored. If you leave MetaDataPath NULL, the evaluation metadata will also be stored here. If you leave this NULL, the function will not run.
#' @param MetaDataPath A separate path to store the model metadata for evaluation.
#' @param NumOfParDepPlots Set to a number greater than or equal to 1 to see the relationships between your features and targets.
#' @return This function does not return anything. It can only store your models and model evaluation metadata to file.
#' @examples
#' \dontrun{
#' AutoCatBoostSizeFreqDist(
#'   CountData = CountData,
#'   SizeData = SizeData,
#'   CountQuantiles = seq(0.10,0.90,0.10),
#'   SizeQuantiles = seq(0.10,0.90,0.10),
#'   AutoTransform = TRUE,
#'   DataPartitionRatios = c(0.75,0.20,0.05),
#'   StratifyColumnNames = NULL,
#'   NTrees = 1500,
#'   TaskType = "GPU",
#'   EvalMetric = "Quantile",
#'   GridTune = FALSE,
#'   GridEvalMetric = "mae",
#'   CountTargetColumnName = "Counts",
#'   SizeTargetColumnName = "Target_qty",
#'   CountFeatureColNames = 2:ncol(CountData),
#'   SizeFeatureColNames = 2:ncol(SizeData),
#'   CountIDcols = NULL,
#'   SizeIDcols = NULL,
#'   ModelIDs = c("CountModel","SizeModel"),
#'   MaxModelsGrid = 5,
#'   ModelPath = getwd(),
#'   MetaDataPath = paste0(getwd(),"/ModelMetaData"),
#'   NumOfParDepPlots = 1)
#' }
#' @export
AutoCatBoostSizeFreqDist <- function(CountData = NULL,
                                     SizeData = NULL,
                                     CountQuantiles = seq(0.10,0.90,0.10),
                                     SizeQuantiles = seq(0.10,0.90,0.10),
                                     AutoTransform = TRUE,
                                     DataPartitionRatios = c(0.75,0.20,0.05),
                                     StratifyColumnNames = NULL,
                                     NTrees = 1500,
                                     TaskType = "GPU",
                                     EvalMetric = "Quantile",
                                     GridTune = FALSE,
                                     GridEvalMetric = "mae",
                                     CountTargetColumnName = NULL,
                                     SizeTargetColumnName = NULL,
                                     CountFeatureColNames = NULL,
                                     SizeFeatureColNames = NULL,
                                     CountIDcols = NULL,
                                     SizeIDcols = NULL,
                                     ModelIDs = c("CountModel","SizeModel"),
                                     MaxModelsGrid = 5,
                                     ModelPath = NULL,
                                     MetaDataPath = NULL,
                                     NumOfParDepPlots = 0) {

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Return immediately if no paths are given----
  if(is.null(ModelPath)) {
    return("Need to supply a path in ModelPath for saving models")
  }

  # Count Model AutoTransform----
  if(AutoTransform) {
    TransFormCols <- CountTargetColumnName
  } else {
    TransFormCols <- NULL
  }

  # Count Model AutoDataPartition----
  CountDataSets <- AutoDataPartition(
    data = CountData,
    NumDataSets = 3,
    Ratios = DataPartitionRatios,
    PartitionType = "random",
    StratifyColumnNames = NULL,
    StratifyNumericTarget = NULL,
    StratTargetPrecision = NULL,
    TimeColumnName = NULL)

  # Store data sets----
  CountDataTrain <- CountDataSets$TrainData
  CountDataValidate <- CountDataSets$ValidationData
  CountDataTest <- CountDataSets$TestData

  # Clear GPU garbage----
  gc()

  # Build Count Models----
  for(quan in CountQuantiles) {

    # Copy data
    CountDataTrainCopy <- data.table::copy(CountDataTrain)
    CountDataValidateCopy <- data.table::copy(CountDataValidate)
    CountDataTestCopy <- data.table::copy(CountDataTest)

    # Build models----
    AutoCatBoostRegression(
      data = CountDataTrainCopy,
      ValidationData = CountDataValidateCopy,
      TestData = CountDataTestCopy,
      TargetColumnName = CountTargetColumnName,
      FeatureColNames = CountFeatureColNames,
      PrimaryDateColumn = NULL,
      IDcols = CountIDcols,
      TransformNumericColumns = TransFormCols,
      MaxModelsInGrid = MaxModelsGrid,
      task_type = TaskType,
      eval_metric = paste0(EvalMetric,":alpha=",quan),
      Trees = NTrees,
      GridTune = GridTune,
      model_path = ModelPath,
      metadata_path = MetaDataPath,
      ModelID = paste0(ModelIDs[1],"_",quan),
      NumOfParDepPlots = NumOfParDepPlots,
      ReturnModelObjects = FALSE,
      SaveModelObjects = TRUE,
      PassInGrid = NULL,
      Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"))

    # Clear GPU garbage----
    gc()

    # Pause Runs by 10 seconds
    Sys.sleep(10)
  }

  # Clear Count Model Data----
  rm(CountDataSets,CountData,CountDataTrain,CountDataValidate,CountDataTest)

  # Size Model AutoTransform----
  if(AutoTransform) {
    TransFormCols <- SizeTargetColumnName
  } else {
    TransFormCols <- NULL
  }

  # Size Model AutoDataPartition----
  SizeDataSets <- AutoDataPartition(
    data = SizeData,
    NumDataSets = 3,
    Ratios = DataPartitionRatios,
    PartitionType = "random",
    StratifyColumnNames = NULL,
    StratifyNumericTarget = NULL,
    StratTargetPrecision = NULL,
    TimeColumnName = NULL)

  # Store data sets----
  SizeDataTrain <- SizeDataSets$TrainData
  SizeDataValidate <- SizeDataSets$ValidationData
  SizeDataTest <- SizeDataSets$TestData

  # Clear data that isn't needed----
  rm(SizeDataSets,SizeData)

  # Clear GPU garbage----
  gc()

  # Build Count Models----
  for(quan in SizeQuantiles) {

    # Copy data----
    SizeDataTrainCopy <- data.table::copy(SizeDataTrain)
    SizeDataValidateCopy <- data.table::copy(SizeDataValidate)
    SizeDataTestCopy <- data.table::copy(SizeDataTest)

    # Build models----
    AutoCatBoostRegression(
      data = SizeDataTrainCopy,
      ValidationData = SizeDataValidateCopy,
      TestData = SizeDataTestCopy,
      TargetColumnName = SizeTargetColumnName,
      FeatureColNames = SizeFeatureColNames,
      PrimaryDateColumn = NULL,
      IDcols = SizeIDcols,
      TransformNumericColumns = TransFormCols,
      MaxModelsInGrid = MaxModelsGrid,
      task_type = TaskType,
      eval_metric = paste0(EvalMetric,":alpha=",quan),
      Trees = NTrees,
      GridTune = GridTune,
      model_path = ModelPath,
      metadata_path = MetaDataPath,
      ModelID = paste0(ModelIDs[2],"_",quan),
      NumOfParDepPlots = NumOfParDepPlots,
      ReturnModelObjects = FALSE,
      SaveModelObjects = TRUE,
      PassInGrid = NULL,
      Methods = c("BoxCox", "Asinh", "Asin", "Log", "LogPlus1", "Logit", "YeoJohnson"))

    # Clear GPU garbage----
    gc()

    # Pause Runs by 10 seconds
    Sys.sleep(10)
  }
}
