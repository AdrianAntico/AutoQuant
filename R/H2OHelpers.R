#' @title H2OArgsCheck
#'
#' @description Ensure arguments are defined correctly
#'
#' @author Adrian
#' @family H2O Helpers
#'
#' @param ModelType "drf",
#' @param TargetType "classification",
#' @param model_path. Passthrough
#' @param metadata_path. Passthrough
#' @param eval_metric. Passthrough
#' @param MaxModelsInGrid. Passthrough
#' @param ModelID. Passthrough
#' @param NumOfParDepPlots. Passthrough
#' @param ReturnModelObjects. Passthrough
#' @param SaveModelObjects. Passthrough
#' @param GridTune. Passthrough
#' @param GridStrategy. Passthrough
#' @param CostMatrixWeights. Passthrough
#' @param IfSaveModel. Passthrough
#' @param Trees. Passthrough
#' @param MaxDepth. Passthrough
#' @param SampleRate. Passthrough
#' @param MTries. Passthrough
#' @param ColSampleRatePerTree. Passthrough
#' @param ColSampleRatePerTreeLevel. Passthrough
#' @param MinRows. Passthrough
#' @param NBins. Passthrough
#' @param NBinsCats. Passthrough
#' @param NBinsTopLevel. Passthrough
#' @param HistogramType. Passthrough
#' @param CategoricalEncoding. Passthrough
#'
#' @noRd
H2OArgsCheck <- function(ModelType = "drf",
                         TargetType = "classification",
                         model_path. = model_path,
                         metadata_path. = metadata_path,
                         eval_metric. = eval_metric,
                         MaxModelsInGrid. = MaxModelsInGrid,
                         ModelID. = ModelID,
                         NumOfParDepPlots. = NumOfParDepPlots,
                         ReturnModelObjects. = ReturnModelObjects,
                         SaveModelObjects. = SaveModelObjects,
                         GridTune. = GridTune,
                         GridStrategy. = GridStrategy,
                         CostMatrixWeights. = CostMatrixWeights,
                         IfSaveModel. = IfSaveModel,
                         Trees. = Trees,
                         MaxDepth. = MaxDepth,
                         SampleRate. = SampleRate,
                         MTries. = MTries,
                         ColSampleRatePerTree. = ColSampleRatePerTree,
                         ColSampleRatePerTreeLevel. = ColSampleRatePerTreeLevel,
                         MinRows. = MinRows,
                         NBins. = NBins,
                         NBinsCats. = NBinsCats,
                         NBinsTopLevel. = NBinsTopLevel,
                         HistogramType. = HistogramType,
                         CategoricalEncoding. = CategoricalEncoding) {

  if(tolower(ModelType) %chin% c("drf","gbm")) {
    if(!is.null(model_path.)) if(!dir.exists(file.path(model_path.))) dir.create(model_path.)
    if(!is.null(metadata_path.)) if(!is.null(metadata_path.)) if(!dir.exists(file.path(metadata_path.))) dir.create(metadata_path.)
    if(TargetType == "classification") {
      if(!(tolower(eval_metric.) %chin% c("auc", "logloss"))) stop("eval_metric not in AUC, logloss")
    } else if(TargetType == "regression") {
      if(!(toupper(eval_metric.) %chin% c("MSE", "RMSE", "MAE", "RMSLE"))) stop("eval_metric not in 'MSE', 'RMSE', 'MAE', 'RMSLE'")
    }
    if(MaxModelsInGrid. < 1 && GridTune.) stop("MaxModelsInGrid needs to be at least 1")
    if(!is.null(model_path.)) if(!is.character(model_path.)) stop("model_path. needs to be a character type")
    if(!is.null(metadata_path.)) if(!is.character(metadata_path.)) stop("metadata_path. needs to be a character type")
    if(!is.character(ModelID.) && !is.null(ModelID.)) stop("ModelID needs to be a character type")
    if(NumOfParDepPlots. < 0) stop("NumOfParDepPlots needs to be a positive number")
    if(!(ReturnModelObjects. %in% c(TRUE, FALSE))) stop("ReturnModelObjects needs to be TRUE or FALSE")
    if(!(SaveModelObjects. %in% c(TRUE, FALSE))) stop("SaveModelObjects needs to be TRUE or FALSE")
    if(!(tolower(eval_metric.) == "auc")) eval_metric. <- tolower(eval_metric.) else eval_metric. <- toupper(eval_metric.)
    if(tolower(eval_metric.) %chin% c("auc")) Decreasing <- TRUE else Decreasing <- FALSE
    if(GridTune. && !GridStrategy. %chin% c("Cartesian","RandomDiscrete")) stop("GridStrategy must be either 'Random' or 'Cartesian'")
    if(TargetType == "classification") if(length(CostMatrixWeights.) != 4) stop("CostMatrixWeights needs to be a 4 element numeric vector")
    if(SaveModelObjects. && !tolower(IfSaveModel.) %chin% c("standard", "mojo")) stop("IfSaveModel needs to be either 'standard' or 'mojo'")
    if(!GridTune. && length(Trees.) > 1L) stop("Trees needs to be a single value unless you're grid tuning")
    if(!GridTune. && length(MaxDepth.) > 1L) stop("MaxDepth needs to be a single value unless you're grid tuning")
    if(!GridTune. && length(SampleRate.) > 1L) stop("SampleRate needs to be a single value unless you're grid tuning")
    if(!GridTune. && tolower(ModelType) == "drf" && length(MTries.) > 1L) stop("MTries needs to be a single value unless you're grid tuning")
    if(!GridTune. && length(ColSampleRatePerTree.) > 1L) stop("ColSampleRatePerTree needs to be a single value unless you're grid tuning")
    if(!GridTune. && length(ColSampleRatePerTreeLevel.) > 1L) stop("ColSampleRatePerTreeLevel needs to be a single value unless you're grid tuning")
    if(!GridTune. && length(MinRows.) > 1L) stop("MinRows needs to be a single value unless you're grid tuning")
    if(!GridTune. && length(NBins.) > 1L) stop("NBins needs to be a single value unless you're grid tuning")
    if(!GridTune. && length(NBinsCats.) > 1L) stop("NBinsCats needs to be a single value unless you're grid tuning")
    if(!GridTune. && length(NBinsTopLevel.) > 1L) stop("NBinsTopLevel needs to be a single value unless you're grid tuning")
    if(!GridTune. && length(HistogramType.) > 1L) stop("HistogramType needs to be a single value unless you're grid tuning")
    if(!GridTune. && length(CategoricalEncoding.) > 1L) stop("CategoricalEncoding needs to be a single value unless you're grid tuning")
    return(Decreasing)
  }
}

#' @title H2OCleanData
#'
#' @author Adrian Antico
#' @family H2O Helpers
#'
#' @param dataTrain. data
#' @param dataTest. data
#' @param TestData. data
#' @param TrainOnFull. TrainOnFull
#'
#' @noRd
H2OCleanData <- function(dataTrain. = NULL,
                         dataTest. = NULL,
                         TestData. = NULL,
                         TrainOnFull. = TrainOnFull) {
  dataTrain. <- ModelDataPrep(data = dataTrain., Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = TRUE, LogicalToBinary = TRUE, DateToChar = TRUE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
  dataTrain. <- ModelDataPrep(data = dataTrain., Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
  if(!TrainOnFull.) {
    dataTest. <- ModelDataPrep(data = dataTest., Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = TRUE, LogicalToBinary = TRUE, DateToChar = TRUE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
    dataTest. <- ModelDataPrep(data = dataTest., Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
  }
  if(!is.null(TestData.)) {
    TestData. <- ModelDataPrep(data = TestData., Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = TRUE, LogicalToBinary = TRUE, DateToChar = TRUE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
    TestData. <- ModelDataPrep(data = TestData., Impute = FALSE, CharToFactor = TRUE, FactorToChar = FALSE, IntToNumeric = FALSE, LogicalToBinary = FALSE, DateToChar = FALSE, RemoveDates = FALSE, MissFactor = "0", MissNum = -1, IgnoreCols = NULL)
  }
  return(list(dataTrain = dataTrain., dataTest = dataTest., TestData = TestData.))
}

#' @param dataTrain. Passthrough
#' @param FeatureColNames. Passthrough
#' @param SaveModelObjects. Passthrough
#' @param model_path. Passthrough
#' @param ModelID. Passthrough
#'
#' @noRd
FeatureStore <- function(dataTrain. = NULL,
                         FeatureColNames. = FeatureColNames,
                         SaveModelObjects. = SaveModelObjects,
                         model_path. = model_path,
                         ModelID. = ModelID) {
  if(is.numeric(FeatureColNames.)) {
    Names <- data.table::as.data.table(names(dataTrain.)[FeatureColNames.])
    data.table::setnames(Names, "V1", "ColNames")
  } else {
    Names <- data.table::as.data.table(FeatureColNames.)
    if(!"V1" %chin% names(Names)) {
      data.table::setnames(Names, "FeatureColNames.", "ColNames")
    } else {
      data.table::setnames(Names, "V1", "ColNames")
    }
  }
  if(SaveModelObjects.) data.table::fwrite(Names, file = file.path(model_path., paste0(ModelID., "_ColNames.csv")))
  return(Names)
}

#' @title H2ODataPrep
#'
#' @param TargetType. 'classifier', 'multiclass', 'regression'
#' @param TargetColumnName. Passthrough
#' @param data. Passthrough
#' @param ValidationData. Passthrough
#' @param TestData. Passthrough
#' @param TrainOnFull. Passthrough
#' @param FeatureColNames. Passthrough
#' @param SaveModelObjects. Passthrough
#' @param ModelID. Passthrough
#' @param model_path. Passthrough
#' @param TransformNumericColumns. Regression Passthrough
#' @param Methods. Regression Passthrough
#'
#' @noRd
H2ODataPrep <- function(TargetType. = "classifier",
                        TargetColumnName. = TargetColumnName,
                        data. = data,
                        ValidationData. = ValidationData,
                        TestData. = TestData,
                        TrainOnFull. = TrainOnFull,
                        FeatureColNames. = FeatureColNames,
                        SaveModelObjects. = SaveModelObjects,
                        model_path. = model_path,
                        ModelID. = ModelID,
                        TransformNumericColumns. = NULL,
                        Methods. = NULL) {

  if(TargetType. %chin% c("classifier","multiclass","regression")) {

    # Target Name Storage
    if(!is.character(TargetColumnName.)) TargetColumnName. <- names(data.)[TargetColumnName.]

    # Ensure data. is a data.table
    if(!data.table::is.data.table(data.)) data.table::setDT(data.)
    if(!is.null(ValidationData.)) if(!data.table::is.data.table(ValidationData.)) data.table::setDT(ValidationData.)
    if(!is.null(TestData.)) if(!data.table::is.data.table(TestData.)) data.table::setDT(TestData.)

    # Ensure Target is a factor
    if(TargetType. %chin% c("classifier","multiclass")) {
      if(!is.factor(data.[[eval(TargetColumnName.)]])) {
        data.[, eval(TargetColumnName.) := as.factor(get(TargetColumnName.))]
        if(!is.null(ValidationData.)) ValidationData.[, eval(TargetColumnName.) := as.factor(get(TargetColumnName.))]
        if(!is.null(TestData.)) TestData.[, eval(TargetColumnName.) := as.factor(get(TargetColumnName.))]
      }
    }

    # Target levels
    if(TargetType. == "multiclass") {
      if(!is.null(TestData.)) {
        TargetLevels <- unique(c(unique(as.character(data.[[eval(TargetColumnName.)]])), unique(as.character(ValidationData.[[eval(TargetColumnName.)]])), unique(as.character(TestData.[[eval(TargetColumnName.)]]))))
      } else {
        TargetLevels <- unique(as.character(data.[[eval(TargetColumnName.)]]))
      }
    } else {
      TargetLevels <- NULL
    }

    # Data Partition
    if(TargetType. != "regression") {
      if(is.null(ValidationData.) && is.null(TestData.) && !TrainOnFull.) {
        dataSets <- AutoDataPartition(
          data = data.,
          NumDataSets = 3L,
          Ratios = c(0.70, 0.20, 0.10),
          PartitionType = "random",
          StratifyColumnNames = TargetColumnName.,
          TimeColumnName = NULL)
        dataTrain <- dataSets$TrainData
        dataTest <- dataSets$ValidationData
        TestData. <- dataSets$TestData
      }
    } else {

      # Convert TransformNumericColumns to Names if not character
      if(!is.null(TransformNumericColumns.) && !is.character(TransformNumericColumns.)) TransformNumericColumns. <- names(data)[TransformNumericColumns.]

      # Transform data, ValidationData, and TestData ----
      if(!is.null(ValidationData.) && !is.null(TransformNumericColumns.)) {
        Output <- AutoTransformationCreate(
          data = data.,
          ColumnNames = TransformNumericColumns.,
          Methods = Methods.,
          Path = model_path.,
          TransID = ModelID.,
          SaveOutput = SaveModelObjects.)
        data. <- Output$Data
        TransformationResults <- Output$FinalResults

        # Transform ValidationData
        ValidationData. <- AutoTransformationScore(
          ScoringData = ValidationData.,
          Type = "Apply",
          FinalResults = TransformationResults,
          TransID = NULL,
          Path = NULL)

        # Transform TestData
        if(!is.null(TestData.)) {
          TestData. <- AutoTransformationScore(
            ScoringData = TestData.,
            Type = "Apply",
            FinalResults = TransformationResults,
            TransID = NULL,
            Path = NULL)
        }
      }

      # Regression Data Partition
      if(is.null(ValidationData.) && is.null(TestData.) && !TrainOnFull.) {
        if(!is.null(TransformNumericColumns.)) {
          dataSets <- AutoDataPartition(
            data = data.,
            NumDataSets = 3L,
            Ratios = c(0.70, 0.20, 0.10),
            PartitionType = "random",
            StratifyColumnNames = NULL,
            TimeColumnName = NULL)
          dataTrain <- dataSets$TrainData
          dataTest <- dataSets$ValidationData
          TestData <- dataSets$TestData
          rm(dataSets)

          # Transform data sets
          Output <- AutoTransformationCreate(
            dataTrain,
            ColumnNames = TransformNumericColumns.,
            Methods = Methods.,
            Path = model_path.,
            TransID = ModelID.,
            SaveOutput = SaveModelObjects.)
          dataTrain <- Output$Data
          TransformationResults <- Output$FinalResults

          # Transform ValidationData
          dataTest <- AutoTransformationScore(
            ScoringData = dataTest,
            Type = "Apply",
            FinalResults = TransformationResults,
            TransID = NULL,
            Path = NULL)

          # Transform TestData
          if(!is.null(TestData.)) {
            TestData. <- AutoTransformationScore(
              ScoringData = TestData.,
              Type = "Apply",
              FinalResults = TransformationResults,
              TransID = NULL,
              Path = NULL)
          }
        } else {
          dataSets <- AutoDataPartition(
            data = data.,
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

      # Get Min Value of TargetColumnName Data
      MinVal <- min(data.[[eval(TargetColumnName.)]], na.rm = TRUE)
    }

    # Extras
    if(!exists("TransformationResults")) TransformationResults <- NULL
    if(!exists("MinVal")) MinVal <- NULL

    # Create dataTrain if not exists
    if(!exists("dataTrain")) dataTrain <- data.
    if(!exists("dataTest") && !TrainOnFull.) {
      dataTest <- ValidationData.
    } else if(!exists("dataTest")) {
      dataTest <- NULL
    }

    # Data prep ----
    Output <- H2OCleanData(dataTrain. = dataTrain, dataTest. = dataTest, TestData. = TestData., TrainOnFull. = TrainOnFull.)
    dataTrain <- Output$dataTrain; Output$dataTrain <- NULL
    dataTest <- Output$dataTest; Output$dataTest <- NULL
    TestData <- Output$TestData; Output$TestData <- NULL

    # Save Names of data.
    Names <- FeatureStore(dataTrain.=dataTrain, FeatureColNames.=FeatureColNames., SaveModelObjects.=SaveModelObjects., model_path.=model_path., ModelID.=ModelID.)

    # Return
    return(list(
      dataTrain = dataTrain,
      dataTest = dataTest,
      TestData = TestData.,
      TargetColumnName = TargetColumnName.,
      Names = Names,
      TargetLevels = TargetLevels,
      TransformationResults = TransformationResults,
      MinVal = MinVal))
  }
}

#' @param SaveModelObjects. Passthrough
#' @param IfSaveModel. Passthrough
#' @param base_model. Passthrough
#' @param model_path. Passthrough
#' @param ModelID. Passthrough
#'
#' @noRd
H2OSaveModel <- function(SaveModelObjects. = SaveModelObjects,
                         IfSaveModel. = IfSaveModel,
                         base_model. = base_model,
                         model_path. = model_path,
                         ModelID. = ModelID) {
  if(SaveModelObjects.) {
    if(tolower(IfSaveModel.) == "mojo") {
      h2o::h2o.save_mojo(object = base_model., path = model_path., force = TRUE)
      h2o::h2o.download_mojo(model = base_model., path = model_path., get_genmodel_jar = TRUE, genmodel_path = model_path., genmodel_name = ModelID.)
    } else {
      h2o::h2o.saveModel(object = base_model., path = model_path., force = TRUE)
    }
  }
}

#' @param Predict. Passthrough
#' @param TestData. Passthrough
#' @param dataTest. Passthrough
#' @param dataTrain. Passthrough
#' @param TrainOnFull. Passthrough
#' @param SaveModelObjects. Passthrough
#' @param metadata_path. Passthrough
#' @param model_path. Passthrough
#' @param ModelID. Passthrough
#' @param TransformNumericColumns. Regression Passthrough
#' @param TransformationResults. Regression Passthrough
#' @param TargetColumnName. Regression Passthrough
#' @param data. Regression Passthrough
#'
#' @noRd
H2OValidationData <- function(Predict. = Predict,
                              TestData. = NULL,
                              dataTest. = NULL,
                              dataTrain. = NULL,
                              TrainOnFull. = TrainOnFull,
                              SaveModelObjects. = SaveModelObjects,
                              metadata_path. = metadata_path,
                              model_path. = model_path,
                              ModelID. = ModelID,
                              TransformNumericColumns. = NULL,
                              TransformationResults. = NULL,
                              TargetColumnName. = NULL,
                              data. = NULL) {

  # Create validation data
  ValidationData <- data.table::as.data.table(cbind(if(!is.null(TestData.)) TestData. else if(!TrainOnFull.) dataTest. else dataTrain., Predict.))
  data.table::setnames(ValidationData, "predict", "Predict")

  # Inverse Transform
  if(!is.null(TransformNumericColumns.)) {
    if(GridTune) TransformationResults. <- TransformationResults.[ColumnName != "Predict"]
    TransformationResults. <- data.table::rbindlist(list(
      TransformationResults.,
      data.table::data.table(
        ColumnName = "Predict",
        MethodName = rep(TransformationResults.[ColumnName == eval(TargetColumnName.), MethodName], 1L),
        Lambda = rep(TransformationResults.[ColumnName == eval(TargetColumnName.), Lambda], 1L),
        NormalizedStatistics = rep(0, 1L))))

    # If Actual target columnname == "Target" remove the duplicate version
    if(length(unique(TransformationResults.[["ColumnName"]])) != nrow(TransformationResults.)) {
      temp <- TransformationResults.[, .N, by = "ColumnName"][N != 1L][[1L]]
      temp1 <- which(names(ValidationData) == temp)[1L]
      data.table::set(ValidationData, j = eval(names(data.)[temp1]), value = NULL)
      TransformationResults. <- TransformationResults.[, ID := seq_len(.N)][ID != which(TransformationResults.[["ID"]] == temp1)][, ID := NULL]
    }

    # Transform Target and Predicted Value
    ValidationData <- AutoTransformationScore(
      ScoringData = ValidationData,
      Type = "Inverse",
      FinalResults = TransformationResults.,
      TransID = NULL,
      Path = NULL)
  }

  # Save validation data
  if(SaveModelObjects.) {
    if(!is.null(metadata_path.)) {
      data.table::fwrite(ValidationData, file = file.path(metadata_path., paste0(ModelID., "_ValidationData.csv")))
    } else {
      data.table::fwrite(ValidationData, file = file.path(model_path., paste0(ModelID., "_ValidationData.csv")))
    }
  }

  # Return output
  return(list(
    ValidationData = ValidationData,
    TransformationResults = TransformationResults.))
}

#' @param TrainOnFull. Passthrough
#' @param base_model. Passthrough
#' @param SaveModelObjects. Passthrough
#' @param metadata_path Passthrough
#' @param model_path. Passthrough
#' @param ModelID. Passthrough
#'
#' @noRd
H2OVariableImportance <- function(TrainOnFull. = TrainOnFull,
                                  base_model. = base_model,
                                  SaveModelObjects. = SaveModelObjects,
                                  metadata_path. = metadata_path,
                                  model_path. = model_path,
                                  ModelID. = ModelID) {
  # Variable Importance
  if(!TrainOnFull.) {
    VariableImportance <- data.table::as.data.table(h2o::h2o.varimp(object = base_model.))
    data.table::setnames(VariableImportance, c("variable","relative_importance","scaled_importance","percentage"), c("Variable","RelativeImportance","ScaledImportance","Percentage"))
    VariableImportance[, ':=' (RelativeImportance = round(RelativeImportance, 4L), ScaledImportance = round(ScaledImportance, 4L), Percentage = round(Percentage, 4L))]
    if(SaveModelObjects.) {
      if(!is.null(metadata_path.)) {
        data.table::fwrite(VariableImportance, file = file.path(metadata_path., paste0(ModelID., "_VariableImportance.csv")))
      } else {
        data.table::fwrite(VariableImportance, file = file.path(model_path., paste0(ModelID., "_VariableImportance.csv")))
      }
    }
  } else {
    VariableImportance <- NULL
  }

  # return
  return(VariableImportance)
}
