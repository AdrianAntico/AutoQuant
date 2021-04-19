#' @title CatBoostArgsCheck
#'
#' @description Ensure arguments are defined correctly
#'
#' @author Adrian
#' @family CatBoost Helpers
#'
#' @param ModelType Passthrough
#' @param DummifyCols. Passthrough
#' @param data. Passthrough
#' @param FeatureColNames. Passthrough
#' @param PrimaryDateColumn. Passthrough
#' @param GridTune. Passthrough
#' @param model_path. Passthrough
#' @param metadata_path. Passthrough
#' @param ClassWeights. Passthrough
#' @param LossFunction. Passthrough
#' @param loss_function. Passthrough regression
#' @param loss_function_value. Passthrough regression
#' @param eval_metric. Passthrough regression
#' @param eval_metric_value. Passthrough regression
#' @param task_type. Passthrough
#' @param NumGPUs. Passthrough
#' @param MaxModelsInGrid. Passthrough
#' @param NumOfParDepPlots. Passthrough
#' @param ReturnModelObject. Passthrough
#' @param SaveModelObjects. Passthrough
#' @param PassInGrid. Passthrough
#' @param MetricPeriods. Passthrough
#' @param langevin. Passthrough
#' @param diffusion_temperature. Passthrough
#' @param Trees. Passthrough
#' @param Depth. Passthrough
#' @param LearningRate. Passthrough
#' @param L2_Leaf_Reg. Passthrough
#' @param RandomStrength. Passthrough
#' @param BorderCount. Passthrough
#' @param RSM. Passthrough
#' @param BootStrapType. Passthrough
#' @param GrowPolicy. Passthrough
#' @param model_size_reg. Passthrough
#' @param feature_border_type. Passthrough
#' @param sampling_unit. Passthrough
#' @param subsample. Passthrough
#' @param score_function. Passthrough
#' @param min_data_in_leaf. Passthrough
#'
#' @noRd
CatBoostArgsCheck <- function(ModelType = "regression",
                              DummifyCols.=DummifyCols,
                              data.=data,
                              FeatureColNames.=FeatureColNames,
                              PrimaryDateColumn. = PrimaryDateColumn,
                              GridTune. = GridTune,
                              model_path. = model_path,
                              metadata_path. = metadata_path,
                              ClassWeights. = ClassWeights,
                              LossFunction. = LossFunction,
                              loss_function. = loss_function,
                              loss_function_value. = loss_function_value,
                              eval_metric. = eval_metric,
                              eval_metric_value. = eval_metric_value,
                              task_type. = task_type,
                              NumGPUs. = NumGPUs,
                              MaxModelsInGrid. = MaxModelsInGrid,
                              NumOfParDepPlots. = NumOfParDepPlots,
                              ReturnModelObjects. = ReturnModelObjects,
                              SaveModelObjects. = SaveModelObjects,
                              PassInGrid. = PassInGrid,
                              MetricPeriods. = MetricPeriods,
                              langevin. = langevin,
                              diffusion_temperature. = diffusion_temperature,
                              Trees. = Trees,
                              Depth. = Depth,
                              LearningRate. = LearningRate,
                              L2_Leaf_Reg. = L2_Leaf_Reg,
                              RandomStrength. = RandomStrength,
                              BorderCount. = BorderCount,
                              RSM. = RSM,
                              BootStrapType. = BootStrapType,
                              GrowPolicy. = GrowPolicy,
                              model_size_reg. = model_size_reg,
                              feature_border_type. = feature_border_type,
                              sampling_unit. = sampling_unit,
                              subsample. = subsample,
                              score_function. = score_function,
                              min_data_in_leaf. = min_data_in_leaf) {

  # Regression loss_function and eval_metric setup
  if(ModelType %chin% c("regression","vector")) {
    if(is.null(loss_function.)) LossFunction. <- "RMSE" else LossFunction. <- loss_function.
    if(is.null(eval_metric.)) EvalMetric. <- "RMSE" else EvalMetric. <- eval_metric.
    if(LossFunction. == "MultiRMSE" || EvalMetric. == "MultiRMSE") task_type. <- "CPU"
    if(tolower(eval_metric.) == "tweedie") EvalMetric. <- paste0('Tweedie:variance_power=',eval_metric_value.)
    if(tolower(loss_function.) == "tweedie") LossFunction. <- paste0('Tweedie:variance_power=',loss_function_value.)
    if(tolower(eval_metric.) == "fairloss") EvalMetric. <- paste0('FairLoss:smoothness=',eval_metric_value.)
    if(tolower(loss_function.) == "fairloss") EvalMetric. <- paste0('FairLoss:smoothness=',eval_metric_value.)
    if(tolower(eval_metric.) == "numerrors") EvalMetric. <- paste0('NumErrors:greater_than=',eval_metric_value.)
    if(tolower(loss_function.) == "numerrors") LossFunction. <- paste0('NumErrors:greater_than=',loss_function_value.)
    if(tolower(eval_metric.) == "lq") EvalMetric. <- paste0('Lq:q=',eval_metric_value.)
    if(tolower(loss_function.) == "lq") LossFunction. <- paste0('Lq:q=',loss_function_value.)
    if(tolower(eval_metric.) == "huber") {
      EvalMetric. <- paste0('Huber:delta=',eval_metric_value)
      task_type <- "CPU"
    }
    if(tolower(loss_function.) == "huber") {
      LossFunction. <- paste0('Huber:delta=',loss_function_value.)
      task_type <- "CPU"
    }
    if(tolower(eval_metric.) == "expectile") EvalMetric. <- paste0('Expectile:alpha=',eval_metric_value.)
    if(tolower(loss_function.) == "expectile") LossFunction. <- paste0('Expectile:alpha=',loss_function_value.)
    if(tolower(eval_metric.) == "quantile") EvalMetric. <- paste0('Quantile:alpha=',eval_metric_value.)
    if(tolower(loss_function.) == "quantile") LossFunction. <- paste0('Quantile:alpha=',loss_function_value.)
    if(tolower(eval_metric.) == "loglinquantile") EvalMetric. <- paste0('LogLinQuantile:alpha=',eval_metric_value.)
    if(tolower(loss_function.) == "loglinquantile") LossFunction. <- paste0('LogLinQuantile:alpha=',loss_function_value.)
  } else {
    EvalMetric. <- NULL
  }

  # Ensure model_path and metadata_path exists if supplied by user
  if(!is.null(model_path.)) if(!dir.exists(file.path(model_path.))) dir.create(model_path.)
  if(!is.null(metadata_path.)) if(!is.null(metadata_path.)) if(!dir.exists(file.path(metadata_path.))) dir.create(metadata_path.)

  # Classification Loss Function
  if(ModelType == "classification") {
    if(is.null(LossFunction.)) LossFunction. <- "Logloss"
  } else if(ModelType == "multiclass") {
    if(is.null(loss_function.)) LossFunction. <- "MultiClassOneVsAll" else LossFunction. <- loss_function.
  }

  # Ensure only one value if not grid tuning
  if(!GridTune. && length(MetricPeriods.) > 1) stop("MetricPeriods cannot have more than one value supplied")
  if(!GridTune. && length(langevin.) > 1) stop("langevin cannot have more than one value supplied")
  if(!GridTune. && length(diffusion_temperature.) > 1) stop("diffusion_temperature cannot have more than one value supplied")
  if(!GridTune. && length(Trees.) > 1) stop("Trees cannot have more than one value supplied")
  if(!GridTune. && length(Depth.) > 1) stop("Depth cannot have more than one value supplied")
  if(!GridTune. && length(LearningRate.) > 1) stop("LearningRate cannot have more than one value supplied")
  if(!GridTune. && length(L2_Leaf_Reg.) > 1) stop("L2_Leaf_Reg cannot have more than one value supplied")
  if(!GridTune. && length(RandomStrength.) > 1) stop("RandomStrength cannot have more than one value supplied")
  if(!GridTune. && length(BorderCount.) > 1) stop("BorderCount cannot have more than one value supplied")
  if(!GridTune. && length(RSM.) > 1) stop("RSM cannot have more than one value supplied")
  if(!GridTune. && length(BootStrapType.) > 1) stop("BootStrapType cannot have more than one value supplied")
  if(!GridTune. && length(GrowPolicy.) > 1) stop("GrowPolicy cannot have more than one value supplied")
  if(!GridTune. && length(model_size_reg.) > 1) stop("model_size_reg cannot have more than one value supplied")
  if(!GridTune. && length(feature_border_type.) > 1) stop("feature_border_type cannot have more than one value supplied")
  if(!GridTune. && length(sampling_unit.) > 1) stop("sampling_unit cannot have more than one value supplied")
  if(!GridTune. && length(subsample.) > 1) stop("subsample cannot have more than one value supplied")
  if(!GridTune. && length(score_function.) > 1) stop("score_function cannot have more than one value supplied")
  if(!GridTune. && length(min_data_in_leaf.) > 1) stop("min_data_in_leaf cannot have more than one value supplied")

  # Logic arg check ----
  if(ModelType %chin% c("classifier") && is.null(ClassWeights.)) ClassWeights. <- c(1,1)
  if(!(tolower(task_type.) %chin% c("gpu", "cpu"))) stop("task_type needs to be either 'GPU' or 'CPU'")
  if(is.null(NumGPUs.)) NumGPUs. <- '0' else if(NumGPUs. > 1L) NumGPUs. <- paste0('0-', NumGPUs.-1L) else NumGPUs. <- '0'
  if(!is.null(PrimaryDateColumn.)) HasTime <- TRUE else HasTime <- FALSE
  if(any(Trees. < 1L)) stop("Trees must be greater than 1")
  if(!GridTune. %in% c(TRUE, FALSE)) stop("GridTune needs to be TRUE or FALSE")
  if(MaxModelsInGrid. < 1L & GridTune.) stop("MaxModelsInGrid needs to be at least 1")
  if(!is.null(model_path.)) if(!is.character(model_path.)) stop("model_path needs to be a character type")
  if(!is.null(metadata_path.)) if(!is.character(metadata_path.)) stop("metadata_path needs to be a character type")
  if(NumOfParDepPlots. < 0L) stop("NumOfParDepPlots needs to be a positive number")
  if(!(ReturnModelObjects. %in% c(TRUE, FALSE))) stop("ReturnModelObjects needs to be TRUE or FALSE")
  if(!(SaveModelObjects. %in% c(TRUE, FALSE))) stop("SaveModelObjects needs to be TRUE or FALSE")
  if(!is.null(PassInGrid.)) GridTune. <- FALSE
  if(GridTune. && any(Depth. > 16)) {
    Depth. <- Depth.[!Depth. > 16]
    if(length(Depth.) == 0) Depth. <- 6
  }
  if(is.null(GrowPolicy.)) GrowPolicy. <- "SymmetricTree"
  if(langevin. && task_type. == "GPU") {
    task_type. <- "CPU"
    print("task_type switched to CPU to enable langevin boosting")
  }

  # RSM management
  if(task_type. == "GPU") {
    RSM. <- NULL
  } else if(is.null(RSM.)) {
    RSM. <- 1
  }
  if(!is.null(sampling_unit.) && LossFunction. != "YetiRankPairWise") sampling_unit. <- "Object"
  if(!is.null(score_function.)) {
    if(task_type. == "CPU" && score_function. %chin% c("NewtonL2","NewtonCosine")) {
      if(!is.null(GrowPolicy.)) {
        if(GrowPolicy. == "Lossguide") score_function. <- "L2"
      } else {
        score_function. <- "Cosine"
        GrowPolicy. <- "SymmetricTree"
      }
    } else if(!is.null(GrowPolicy.)) {
      if(GrowPolicy. == "Lossguide" && score_function. == "NewtonCosine") score_function. <- "NewtonL2"
    } else {
      GrowPolicy. <- "SymmetricTree"
    }
  }
  if(is.null(BootStrapType.)) {
    if(task_type. == "GPU") BootStrapType. <- "Bayesian"
    if(task_type. == "CPU") BootStrapType. <- "MVS"
  } else if(task_type. == "GPU" && any(BootStrapType. %chin% "MVS")) {
    if(GridTune. && length(BootStrapType.) > 1L) {
      BootStrapType. <- BootStrapType.[!BootStrapType. %chin% "MVS"]
    }
  }
  if(!is.null(sampling_unit.) && BootStrapType. == "MVS") sampling_unit. <- "Object"

  # Return
  return(list(
    HasTime = HasTime,
    GridTune = GridTune.,
    LossFunction = LossFunction.,
    EvalMetric = EvalMetric.,
    task_type = task_type.,
    NumGPUs = NumGPUs.,
    Depth = Depth.,
    RSM = RSM.,
    BootStrapType = BootStrapType.,
    sampling_unit = sampling_unit.,
    score_function = score_function.,
    GrowPolicy = GrowPolicy.))
}

#' @title CatBoostDataPrep
#'
#' @description Prepare data for loading into catboost format
#'
#' @family CatBoost Helpers
#' @author Adrian Antico
#'
#' @param ModelType 'regression', 'vector', 'classification', or 'multiclass'
#' @param data. Passthrough
#' @param ValidationData. Passthrough
#' @param TestData. Passthrough
#' @param TargetColumnName. Passthrough
#' @param FeatureColNames. Passthrough
#' @param PrimaryDateColumn. Passthrough
#' @param IDcols. Passthrough
#' @param TransformNumericColumns. Passthrough regression
#' @param Methods. Passthrough regression
#' @param ModelID. Passthrough regression
#' @param model_path. Passthrough regression
#' @param DummifyCols. Passthrough regression
#' @param LossFunction. Passthrough regression
#' @param EvalMetric. Passthrough regression
#' @param TrainOnFull. Passthrough
#' @param SaveModelObjects. Passthrough
#'
#' @noRd
CatBoostDataPrep <- function(ModelType = "regression",
                             data. = data,
                             ValidationData. = ValidationData,
                             TestData. = TestData,
                             TargetColumnName. = TargetColumnName,
                             FeatureColNames. = FeatureColNames,
                             PrimaryDateColumn. = PrimaryDateColumn,
                             IDcols. = IDcols,
                             TransformNumericColumns. = TransformNumericColumns,
                             Methods. = Methods,
                             ModelID. = ModelID,
                             model_path. = model_path,
                             DummifyCols. = DummifyCols,
                             LossFunction. = LossFunction,
                             EvalMetric. = EvalMetric,
                             TrainOnFull. = TrainOnFull,
                             SaveModelObjects. = SaveModelObjects) {

  # Ensure data is a data.table ----
  if(!data.table::is.data.table(data.)) data.table::setDT(data.)
  if(!is.null(ValidationData.)) if(!data.table::is.data.table(ValidationData.)) data.table::setDT(ValidationData.)
  if(!is.null(TestData.)) if(!data.table::is.data.table(TestData.)) data.table::setDT(TestData.)

  # Target Name Storage ----
  if(!is.character(TargetColumnName.)) TargetColumnName. <- names(data.)[TargetColumnName.]

  # IDcol Name Storage ----
  if(!is.null(IDcols.)) if(!is.character(IDcols.)) IDcols. <- names(data.)[IDcols.]

  # Identify column numbers for factor variables ----
  if(ModelType != "multiclass") {
    CatFeatures <- sort(c(as.numeric(which(sapply(data., is.factor))), as.numeric(which(sapply(data., is.character)))))
    if(length(CatFeatures) > 0) CatFeatureNames <- names(data.)[CatFeatures] else CatFeatureNames <- NULL
  } else {
    CatFeatures <- sort(c(as.numeric(which(sapply(data, is.factor))), as.numeric(which(sapply(data, is.character)))))
    TargetNum <- which(names(data) == TargetColumnName.)
    CatFeatures <- setdiff(CatFeatures, TargetNum)
    if(length(CatFeatures) > 0) CatFeatureNames <- names(data.)[CatFeatures] else CatFeatureNames <- NULL
  }

  # Partition
  if(is.null(ValidationData.) && is.null(TestData.) && !TrainOnFull.) {
    UseBestModel <- TRUE
    if(ModelType %chin% c("regression", "vector")) {
      if(!is.null(TransformNumericColumns.)) {
        dataSets <- AutoDataPartition(
          data = data.,
          NumDataSets = 3L,
          Ratios = c(0.70, 0.20, 0.10),
          PartitionType = "random",
          StratifyColumnNames = eval(TargetColumnName.[1L]),
          TimeColumnName = NULL)
        data. <- dataSets$TrainData
        ValidationData. <- dataSets$ValidationData
        TestData. <- dataSets$TestData
        rm(dataSets)

        # Mean of data----
        MeanTrainTarget <- mean(data.[[eval(TargetColumnName.[1L])]], na.rm = TRUE)

        # Transform data sets----
        Output <- AutoTransformationCreate(
          data.,
          ColumnNames = TransformNumericColumns.,
          Methods = Methods.,
          Path = model_path.,
          TransID = ModelID.,
          SaveOutput = SaveModelObjects.)
        data. <- Output$Data
        TransformationResults <- Output$FinalResults

        # Transform ValidationData----
        ValidationData. <- AutoTransformationScore(
          ScoringData = ValidationData.,
          Type = "Apply",
          FinalResults = TransformationResults,
          TransID = NULL,
          Path = NULL)

        # Transform TestData----
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
          NumDataSets = 3L,
          Ratios = c(0.70, 0.20, 0.10),
          PartitionType = "random",
          StratifyColumnNames = eval(TargetColumnName.[1L]),
          TimeColumnName = NULL)
        data. <- dataSets$TrainData
        ValidationData. <- dataSets$ValidationData
        TestData. <- dataSets$TestData
        rm(dataSets)
        if(length(TargetColumnName.) > 1) {
          MeanTrainTarget <- c()
          for(i in seq_len(length(TargetColumnName.))) MeanTrainTarget[i] <- mean(data.[[eval(TargetColumnName.[i])]], na.rm = TRUE)
          rm(i)
        } else {
          MeanTrainTarget <- mean(data.[[eval(TargetColumnName.)]], na.rm = TRUE)
        }
      }
    } else if(ModelType %chin% c("classification", "multiclass")) {
      dataSets <- AutoDataPartition(
        data = data.,
        NumDataSets = 3L,
        Ratios = c(0.70, 0.20, 0.10),
        PartitionType = "random",
        StratifyColumnNames = eval(TargetColumnName.),
        TimeColumnName = NULL)
      data. <- dataSets$TrainData
      ValidationData. <- dataSets$ValidationData
      TestData. <- dataSets$TestData
      TransformationResults <- NULL
    }
  } else {

    # Set value
    UseBestModel <- FALSE

    # Transform data sets----
    if(!is.null(TransformNumericColumns.)) {
      Output <- AutoTransformationCreate(
        data.,
        ColumnNames = TransformNumericColumns.,
        Methods = Methods.,
        Path = model_path.,
        TransID = ModelID.,
        SaveOutput = SaveModelObjects.)
      data. <- Output$Data
      TransformationResults <- Output$FinalResults
    } else {
      TransformationResults <- NULL
    }
  }

  # Dummify ----
  if(length(CatFeatures) > 0L && (DummifyCols. || (!is.null(LossFunction.) && LossFunction. == "MultiRMSE") || (!is.null(EvalMetric.) && EvalMetric. == "MultiRMSE"))) {

    # Regression Dummify Categorical Features ----
    if(!is.null(ValidationData.) && !is.null(TestData.) && !TrainOnFull.) {
      data.table::set(data., j = "ID_Factorizer", value = "TRAIN")
      data.table::set(ValidationData., j = "ID_Factorizer", value = "VALIDATE")
      data.table::set(TestData., j = "ID_Factorizer", value = "TEST")
      temp <- data.table::rbindlist(list(data., ValidationData., TestData.))
      temp <- DummifyDT(
        data = temp,
        cols = if(!is.character(CatFeatures)) names(temp)[CatFeatures] else CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = SaveModelObjects.,
        ReturnFactorLevels = TRUE,
        SavePath = if(SaveModelObjects.) model_path. else NULL,
        ImportFactorLevels = FALSE,
        GroupVar = TRUE)
      FactorLevelsList <- temp$FactorLevelsList
      temp <- temp$data
      data. <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(data., j = "ID_Factorizer", value = NULL)
      ValidationData. <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(ValidationData., j = "ID_Factorizer", value = NULL)
      TestData. <- temp[ID_Factorizer == "TEST"]
      data.table::set(TestData., j = "ID_Factorizer", value = NULL)
      rm(temp)

    } else {

      data.table::set(data., j = "ID_Factorizer", value = "TRAIN")
      if(!TrainOnFull.) {
        data.table::set(ValidationData.,j = "ID_Factorizer", value = "VALIDATE")
        temp <- data.table::rbindlist(list(data., ValidationData.))
      } else {
        temp <- data.
      }
      temp <- DummifyDT(
        data = temp,
        cols = if(!is.character(CatFeatures)) names(temp)[CatFeatures] else CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = if(SaveModelObjects.) TRUE else FALSE,
        ReturnFactorLevels = TRUE,
        SavePath = if(SaveModelObjects.) model_path. else NULL,
        ImportFactorLevels = FALSE,
        GroupVar = TRUE)
      CatFeatures <- numeric(0)
      FactorLevelsList <- temp$FactorLevelsList
      temp <- temp$data
      data. <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(data., j = "ID_Factorizer", value = NULL)
      if(!TrainOnFull.) {
        ValidationData. <- temp[ID_Factorizer == "VALIDATE"]
        data.table::set(ValidationData., j = "ID_Factorizer", value = NULL)
      }
      rm(temp)
    }
  } else {
    FactorLevelsList <- NULL
  }

  # Sort data if PrimaryDateColumn ----
  if(!is.null(PrimaryDateColumn.)) {
    data.table::setorderv(x = data., cols = eval(PrimaryDateColumn.), order = 1L)
    if(!(eval(PrimaryDateColumn.) %in% IDcols.)) data.table::set(data., j = eval(PrimaryDateColumn.), value = NULL)
  }

  # Sort ValidationData if PrimaryDateColumn ----
  if(!is.null(PrimaryDateColumn.) && !TrainOnFull.) {
    data.table::setorderv(x = ValidationData., cols = eval(PrimaryDateColumn.), order = 1L)
    if(!(eval(PrimaryDateColumn.) %in% IDcols.)) data.table::set(ValidationData., j = eval(PrimaryDateColumn.), value = NULL)
  }

  # Sort TestData if PrimaryDateColumn ----
  if(!is.null(TestData.) && !TrainOnFull. && !is.null(PrimaryDateColumn.)) {
    data.table::setorderv(x = TestData., cols = eval(PrimaryDateColumn.), order = -1L)
    if(!(eval(PrimaryDateColumn.) %in% IDcols.)) data.table::set(TestData., j = eval(PrimaryDateColumn.), value = NULL)
  }

  # Data Subset Columns Needed ----
  keep <- unique(c(PrimaryDateColumn., IDcols.))
  if(!is.null(keep)) data.table::set(data., j = c(keep), value = NULL)
  if(!TrainOnFull. && !is.null(keep)) data.table::set(ValidationData., j = c(keep), value = NULL) else ValidationData. <- NULL

  # TestData Subset Columns Needed ----
  if(!is.null(TestData.)) {
    TestMerge <- data.table::copy(TestData.)
    if(!is.null(keep)) data.table::set(TestData., j = c(keep), value = NULL)
  } else {
    TestMerge <- NULL
    TestData. <- NULL
  }

  # Train ModelDataPrep ----
  data. <- ModelDataPrep(data = data., Impute = TRUE, CharToFactor = TRUE, RemoveDates = TRUE, MissFactor = "0", MissNum = -1, IntToNumeric = TRUE, FactorToChar = FALSE, DateToChar = FALSE, IgnoreCols = NULL)
  if(!TrainOnFull.) ValidationData. <- ModelDataPrep(data = ValidationData., Impute = TRUE, CharToFactor = TRUE, RemoveDates = TRUE, MissFactor = "0", MissNum = -1, FactorToChar = FALSE, IntToNumeric = TRUE, DateToChar = FALSE, IgnoreCols = NULL)
  if(!is.null(TestData.)) TestData. <- ModelDataPrep(data = TestData., Impute = TRUE, CharToFactor = TRUE, RemoveDates = TRUE, MissFactor = "0", MissNum = -1, FactorToChar = FALSE, IntToNumeric = TRUE, DateToChar = FALSE, IgnoreCols = NULL)

  # Multiclass target levels
  if(ModelType == "multiclass") {

    # MultiClass Obtain Unique Target Levels
    if(!is.null(TestData.)) {
      temp <- data.table::rbindlist(list(data., ValidationData., TestData.))
    } else if(!is.null(ValidationData.)) {
      temp <- data.table::rbindlist(list(data., ValidationData.))
    } else {
      temp <- data.
    }
    TargetLevels <- data.table::as.data.table(sort(unique(temp[[eval(TargetColumnName.)]])))
    data.table::setnames(TargetLevels, "V1", "OriginalLevels")
    TargetLevels[, NewLevels := seq_len(.N)]
    if(SaveModelObjects.) data.table::fwrite(TargetLevels, file = file.path(model_path., ModelID., "_TargetLevels.csv"))

    # MultiClass Convert Target to Numeric Factor
    data. <- merge(
      data.,
      TargetLevels,
      by.x = eval(TargetColumnName.),
      by.y = "OriginalLevels",
      all = FALSE)
    data.[, eval(TargetColumnName.) := NewLevels]
    data.[, NewLevels := NULL]
    if(TrainOnFull. != TRUE) {
      ValidationData. <- merge(
        ValidationData.,
        TargetLevels,
        by.x = eval(TargetColumnName.),
        by.y = "OriginalLevels",
        all = FALSE)
      ValidationData.[, eval(TargetColumnName.) := NewLevels]
      ValidationData.[, NewLevels := NULL]
      if(!is.null(TestData.)) {
        TestData. <- merge(
          TestData.,
          TargetLevels,
          by.x = eval(TargetColumnName.),
          by.y = "OriginalLevels",
          all = FALSE)
        TestData.[, eval(TargetColumnName.) := NewLevels]
        TestData.[, NewLevels := NULL]
      }
    }

    # Reorder Colnames
    data.table::setcolorder(data., c(2L:ncol(data.), 1L))
    if(!is.null(ValidationData.)) data.table::setcolorder(ValidationData., c(2L:ncol(ValidationData.), 1L))
    if(!is.null(TestData.)) data.table::setcolorder(TestData., c(2L:ncol(TestData.), 1L))
  } else {
    TargetLevels <- NULL
  }

  # Save Names of data ----
  Names <- data.table::as.data.table(setdiff(names(data.), c(TargetColumnName., PrimaryDateColumn., IDcols.)))
  if(!"V1" %chin% names(Names)) data.table::setnames(Names, "FeatureColNames.", "ColNames", skip_absent = TRUE) else data.table::setnames(Names, "V1", "ColNames", skip_absent = TRUE)
  if(SaveModelObjects.) data.table::fwrite(Names, file.path(model_path., paste0(ModelID., "_ColNames.csv")))

  # Subset Target Variables----
  TrainTarget <- data.[, .SD, .SDcols = c(TargetColumnName.)]
  if(ncol(TrainTarget) > 1L) TrainTarget <- as.matrix(TrainTarget) else TrainTarget <- TrainTarget[[1L]]
  data.table::set(data., j = TargetColumnName., value = NULL)
  if(!TrainOnFull.) {
    TestTarget <- ValidationData.[, .SD, .SDcols = c(TargetColumnName.)]
    if(ncol(TestTarget) > 1L) TestTarget <- as.matrix(TestTarget) else TestTarget <- TestTarget[[1L]]
    data.table::set(ValidationData., j = TargetColumnName., value = NULL)
    if(!is.null(TestData.)) {
      FinalTestTarget <- TestData.[, .SD, .SDcols = c(TargetColumnName.)]
      if(ncol(FinalTestTarget) > 1L) FinalTestTarget <- as.matrix(FinalTestTarget) else FinalTestTarget <- FinalTestTarget[[1L]]
      data.table::set(TestData., j = TargetColumnName., value = NULL)
    } else {
      FinalTestTarget <- NULL
    }
  } else {
    TestTarget <- NULL
    FinalTestTarget <- NULL
  }

  # Identify column numbers for factor variables ----
  if(ModelType != "multiclass" && (DummifyCols. || (!is.null(LossFunction.) && LossFunction. == "MultiRMSE") || (!is.null(EvalMetric.) && EvalMetric. == "MultiRMSE"))) {
    CatFeatures <- sort(c(as.numeric(which(sapply(data., is.factor))), as.numeric(which(sapply(data., is.character)))))
    if(length(CatFeatures) > 0) CatFeatureNames <- names(data.)[CatFeatures] else CatFeatureNames <- NULL
  } else if((DummifyCols. || (!is.null(LossFunction.) && LossFunction. == "MultiRMSE") || (!is.null(EvalMetric.) && EvalMetric. == "MultiRMSE"))) {
    CatFeatures <- sort(c(as.numeric(which(sapply(data., is.factor))), as.numeric(which(sapply(data, is.character)))))
    TargetNum <- which(names(data.) == TargetColumnName.)
    CatFeatures <- setdiff(CatFeatures, TargetNum)
    if(length(CatFeatures) > 0) CatFeatureNames <- names(data.)[CatFeatures] else CatFeatureNames <- NULL
  }

  # Convert CatFeatures to 1-indexed----
  if(length(CatFeatures) > 0L) CatFeatures <- CatFeatures - 1L

  # Return
  return(list(dataTrain = data.,
              dataTest = ValidationData.,
              TestData = TestData.,
              TestMerge = TestMerge,
              TrainTarget = TrainTarget,
              TestTarget = TestTarget,
              FinalTestTarget = FinalTestTarget,
              CatFeatures = CatFeatures,
              Names = Names,
              UseBestModel = UseBestModel,
              TransformationResults = TransformationResults,
              FactorLevelsList = FactorLevelsList,
              TargetLevels = TargetLevels))
}

#' @title CatBoostDataConversion
#'
#' @description Convert data to catboost format
#'
#' @author Adrian
#' @family CatBoost Helpers
#'
#' @param CatFeatures. Passthrough
#' @param dataTrain. Passthrough
#' @param dataTest. Passthrough
#' @param TestData. Passthrough
#' @param TrainTarget. Passthrough
#' @param TestTarget. Passthrough
#' @param FinalTestTarget. Passthrough
#' @param TrainOnFull. Passthrough
#'
#' @noRd
CatBoostDataConversion <- function(CatFeatures. = CatFeatures,
                                   dataTrain. = dataTrain,
                                   dataTest. = dataTest,
                                   TestData. = TestData,
                                   TrainTarget. = TrainTarget,
                                   TestTarget. = TestTarget,
                                   FinalTestTarget. = FinalTestTarget,
                                   TrainOnFull. = TrainOnFull) {
  if(!is.null(CatFeatures.) || length(CatFeatures.) > 0) {
    if(!is.null(TestData.)) {
      TrainPool <- catboost::catboost.load_pool(dataTrain., label = TrainTarget., cat_features = CatFeatures.)
      if(!TrainOnFull.) {
        TestPool <- catboost::catboost.load_pool(dataTest., label = TestTarget., cat_features = CatFeatures.)
        FinalTestPool <- catboost::catboost.load_pool(TestData., label = FinalTestTarget., cat_features = CatFeatures.)
      }
    } else {
      TrainPool <- catboost::catboost.load_pool(dataTrain., label = TrainTarget., cat_features = CatFeatures.)
      if(!TrainOnFull.) {
        TestPool <- catboost::catboost.load_pool(dataTest., label = TestTarget., cat_features = CatFeatures.)
      }
    }
  } else {
    if(!is.null(TestData.)) {
      TrainPool <- catboost::catboost.load_pool(dataTrain., label = TrainTarget.)
      if(!TrainOnFull.) {
        TestPool <- catboost::catboost.load_pool(dataTest., label = TestTarget.)
        FinalTestPool <- catboost::catboost.load_pool(TestData., label = FinalTestTarget.)
      }
    } else {
      TrainPool <- catboost::catboost.load_pool(dataTrain., label = TrainTarget.)
      if(!TrainOnFull.) TestPool <- catboost::catboost.load_pool(dataTest., label = TestTarget.)
    }
  }

  # Check
  if(!exists("TestPool")) TestPool <- NULL
  if(!exists("FinalTestPool")) FinalTestPool <- NULL

  # Return
  return(list(TrainPool = TrainPool, TestPool = TestPool, FinalTestPool = FinalTestPool))
}

#' @title CatBoostFinalParams
#'
#' @description Convert data to catboost format
#'
#' @author Adrian
#' @family CatBoost Helpers
#'
#' @param ModelType 'regression', 'classification', 'multiclass', 'vector'
#' @param UseBestModel. Passthrough
#' @param ClassWeights. Passthrough
#' @param TargetColumnName. Passthrough
#' @param PassInGrid. Passthrough
#' @param BestGrid. Passthrough
#' @param ExperimentalGrid. Passthrough
#' @param GridTune. Passthrough
#' @param TrainOnFull. Passthrough
#' @param MetricPeriods. Passthrough
#' @param LossFunction. Passthrough
#' @param EvalMetric. Passthrough
#' @param score_function. Passthrough
#' @param HasTime. Passthrough
#' @param task_type. Passthrough
#' @param NumGPUs. Passthrough
#' @param NTrees. Passthrough
#' @param Depth. Passthrough
#' @param LearningRate. Passthrough
#' @param L2_Leaf_Reg. Passthrough
#' @param langevin. Passthrough
#' @param diffusion_temperature. Passthrough
#' @param sampling_unit. Passthrough
#' @param RandomStrength. Passthrough
#' @param BorderCount. Passthrough
#' @param RSM. Passthrough
#' @param GrowPolicy. Passthrough
#' @param BootStrapType. Passthrough
#' @param model_size_reg. Passthrough
#' @param feature_border_type. Passthrough
#' @param subsample. Passthrough
#' @param min_data_in_leaf. Passthrough
#'
#' @noRd
CatBoostFinalParams <- function(ModelType = "classification",
                                UseBestModel. = UseBestModel,
                                ClassWeights. = ClassWeights,
                                PassInGrid. = PassInGrid,
                                ExperimentalGrid. = ExperimentalGrid,
                                BestGrid. = BestGrid,
                                GridTune. = GridTune,
                                TrainOnFull. = TrainOnFull,
                                MetricPeriods. = MetricPeriods,
                                LossFunction. = LossFunction,
                                EvalMetric. = EvalMetric,
                                score_function. = score_function,
                                HasTime. = HasTime,
                                task_type. = task_type,
                                NumGPUs. = NumGPUs,
                                NTrees. = Trees,
                                Depth. = Depth,
                                LearningRate. = LearningRate,
                                L2_Leaf_Reg. = L2_Leaf_Reg,
                                langevin. = langevin,
                                diffusion_temperature. = diffusion_temperature,
                                sampling_unit. = sampling_unit,
                                RandomStrength. = RandomStrength,
                                BorderCount. = BorderCount,
                                RSM. = RSM,
                                GrowPolicy. = GrowPolicy,
                                BootStrapType. = BootStrapType,
                                model_size_reg. = model_size_reg,
                                feature_border_type. = feature_border_type,
                                subsample. = subsample,
                                min_data_in_leaf. = min_data_in_leaf) {

  # Define parameters for case where you pass in a winning GridMetrics from grid tuning
  if(!is.null(PassInGrid.)) {
    if(PassInGrid.[,.N] > 1L) stop("PassInGrid needs to be a single row data.table")
    if(PassInGrid.[, BanditProbs_Grid_1] == -10) {
      PassInGrid. <- NULL

    } else {

      # Base Parameters
      base_params <- list()
      base_params[["class_weights"]] <- if(ModelType %chin% c("classification","multiclass")) ClassWeights. else NULL
      base_params[["use_best_model"]] <- UseBestModel.
      base_params[["best_model_min_trees"]] <- 10L
      base_params[["allow_writing_files"]] <- FALSE
      base_params[["thread_count"]] <- parallel::detectCores()

      # Additional Parameters
      base_params[["iterations"]] <- PassInGrid.[["NTrees"]]
      base_params[["depth"]] <- PassInGrid.[["Depth"]]
      base_params[["langevin"]] <- langevin.
      base_params[["diffusion_temperature"]] <- if(langevin.) diffusion_temperature. else NULL
      base_params[["learning_rate"]] <- PassInGrid.[["LearningRate"]]

      base_params[["l2_leaf_reg"]] <- PassInGrid.[["L2_Leaf_Reg"]]
      base_params[["random_strength"]] <- PassInGrid.[["RandomStrength"]]
      base_params[["border_count"]] <- PassInGrid.[["BorderCount"]]
      base_params[["rsm"]] <- PassInGrid.[["RSM"]]
      base_params[["sampling_unit"]] <- sampling_unit.

      # Speedup
      base_params[["metric_period"]] <- MetricPeriods.

      # Style of model
      base_params[["grow_policy"]] <- PassInGrid.[["GrowPolicy"]]
      base_params[["bootstrap_type"]] <- PassInGrid.[["BootStrapType"]]

      # Loss functions
      base_params[["loss_function"]] <- LossFunction.
      base_params[["eval_metric"]] <- EvalMetric.
      base_params[["score_function"]] <- score_function.

      # Data ordering for quality improvement
      base_params[["has_time"]] <- HasTime.

      # Hardware
      base_params[["task_type"]] <- task_type.
      base_params[["devices"]] <- NumGPUs.

      # Categorical Feature Args
      base_params[["model_size_reg"]] <- model_size_reg.

      # Numerical Feature Args
      base_params[["feature_border_type"]] <- feature_border_type.
      base_params[["subsample"]] <- if(any(PassInGrid.[["BootStrapType"]] %chin% c("Bayesian","No"))) NULL else if(!is.null(subsample.)) subsample. else NULL
      base_params[["min_data_in_leaf"]] <- if(PassInGrid.[["GrowPolicy"]] %chin% c("SymmetricTree")) NULL else if(!is.null(min_data_in_leaf.)) min_data_in_leaf. else NULL
    }
  }

  # Define parameters for case where you want to run grid tuning
  if(GridTune. && !TrainOnFull.) {

    # BaseCase check: if grid tuned and default is best, set BaseCase to TRUE
    # When that happens, need to select which default values to ustilize for params
    if(BestGrid.[["RunNumber"]] == 1) BaseCase <- TRUE else BaseCase <- FALSE

    # Base Parameters
    base_params <- list()
    base_params[["class_weights"]] <- if(ModelType %chin% c("classification","multiclass")) ClassWeights. else NULL
    base_params[["use_best_model"]] <- UseBestModel.
    base_params[["best_model_min_trees"]] <- 10L
    base_params[["allow_writing_files"]] <- FALSE
    base_params[["thread_count"]] <- parallel::detectCores()

    # Additional Parameters
    base_params[["iterations"]] <- if(BestGrid.[["GrowPolicy"]] == "aa" || BaseCase) max(NTrees.) else BestGrid.[["NTrees"]]
    base_params[["depth"]] <- if(BestGrid.[["GrowPolicy"]] == "aa" || BaseCase) NULL else BestGrid.[["Depth"]]
    base_params[["langevin"]] <- langevin.
    base_params[["diffusion_temperature"]] <- if(langevin.) diffusion_temperature. else NULL
    base_params[["learning_rate"]] <- if(BestGrid.[["GrowPolicy"]] == "aa" || BaseCase) NULL else BestGrid.[["LearningRate"]]

    base_params[["l2_leaf_reg"]] <- if(BestGrid.[["GrowPolicy"]] == "aa" || BaseCase) NULL else BestGrid.[["L2_Leaf_Reg"]]
    base_params[["random_strength"]] <- if(BestGrid.[["GrowPolicy"]] == "aa" || BaseCase) NULL else BestGrid.[["RandomStrength"]]
    base_params[["border_count"]] <- if(BestGrid.[["GrowPolicy"]] == "aa" || BaseCase) NULL else BestGrid.[["BorderCount"]]
    base_params[["rsm"]] <- if(BestGrid.[["GrowPolicy"]] == "aa" || BestGrid.[["RSM"]] == -1 || BaseCase) NULL else BestGrid.[["RSM"]]
    base_params[["sampling_unit"]] <- sampling_unit.

    # Speedup
    base_params[["metric_period"]] <- MetricPeriods.

    # Style of model
    base_params[["grow_policy"]] <- if(BestGrid.[["GrowPolicy"]] == "aa" || BaseCase) "SymmetricTree" else BestGrid.[["GrowPolicy"]]
    base_params[["bootstrap_type"]] <- if(BestGrid.[["GrowPolicy"]] == "aa" || BaseCase) NULL else BestGrid.[["BootStrapType"]]

    # Loss functions
    base_params[["loss_function"]] <- LossFunction.
    base_params[["eval_metric"]] <- EvalMetric.
    base_params[["score_function"]] <- score_function.

    # Data ordering for quality improvement
    base_params[["has_time"]] <- HasTime.

    # Hardware
    base_params[["task_type"]] <- task_type.
    base_params[["devices"]] <- NumGPUs.

    # Categorical Feature Args
    base_params[["model_size_reg"]] <- model_size_reg.

    # Numerical Feature Args
    base_params[["feature_border_type"]] <- feature_border_type.
    base_params[["subsample"]] <- if(any(BootStrapType. %chin% c("Bayesian","No"))) NULL else if(!is.null(subsample.)) subsample. else NULL
    base_params[["min_data_in_leaf"]] <- if(GrowPolicy. %chin% c("SymmetricTree")) NULL else if(!is.null(min_data_in_leaf.)) min_data_in_leaf. else NULL
  }

  # Define parameters Not pass in GridMetric and not grid tuning
  if(is.null(PassInGrid.) && !GridTune.) {


    # Base Parameters
    base_params <- list()
    base_params[["class_weights"]] <- if(ModelType %chin% c("classification","multiclass")) ClassWeights. else NULL
    base_params[["use_best_model"]] <- UseBestModel.
    base_params[["best_model_min_trees"]] <- 10L
    base_params[["allow_writing_files"]] <- FALSE
    base_params[["thread_count"]] <- parallel::detectCores()

    # Additional Parameters
    base_params[["iterations"]] <- NTrees.
    base_params[["depth"]] <- Depth.
    base_params[["langevin"]] <- langevin.
    base_params[["diffusion_temperature"]] <- if(langevin.) diffusion_temperature. else NULL
    base_params[["learning_rate"]] <- LearningRate.

    base_params[["l2_leaf_reg"]] <- L2_Leaf_Reg.
    base_params[["random_strength"]] <- RandomStrength.
    base_params[["border_count"]] <- BorderCount.
    base_params[["rsm"]] <- RSM.
    base_params[["sampling_unit"]] <- sampling_unit.

    # Speedup
    base_params[["metric_period"]] <- MetricPeriods.

    # Style of model
    base_params[["grow_policy"]] <- GrowPolicy.
    base_params[["bootstrap_type"]] <- BootStrapType.

    # Loss functions
    base_params[["loss_function"]] <- LossFunction.
    base_params[["eval_metric"]] <- EvalMetric.
    base_params[["score_function"]] <- score_function.

    # Data ordering for quality improvement
    base_params[["has_time"]] <- HasTime.

    # Hardware
    base_params[["task_type"]] <- task_type.
    base_params[["devices"]] <- NumGPUs.

    # Categorical Feature Args
    base_params[["model_size_reg"]] <- model_size_reg.

    # Numerical Feature Args
    base_params[["feature_border_type"]] <- feature_border_type.
    base_params[["subsample"]] <- if(any(BootStrapType. %chin% c("Bayesian","No"))) NULL else if(!is.null(subsample.)) subsample. else NULL
    base_params[["min_data_in_leaf"]] <- if(GrowPolicy. %chin% c("SymmetricTree")) NULL else if(!is.null(min_data_in_leaf.)) min_data_in_leaf. else NULL
  }

  # Return params
  return(base_params)
}

#' @title CatBoostValidationData
#'
#' @description Return validation data with predictions and save to file if requested
#'
#' @author Adrian Antico
#' @family CatBoost Helpers
#'
#' @param ModelType = "classification",
#' @param TestDataCheck = !is.null(TestData),
#' @param TrainOnFull. Passthrough
#' @param FinalTestTarget. Passthrough
#' @param TestTarget. Passthrough
#' @param TrainTarget. Passthrough
#' @param TestMerge. Passthrough
#' @param dataTest. Passthrough
#' @param data. Passthrough
#' @param predict. Passthrough
#' @param TargetColumnName. Passthrough
#' @param SaveModelObjects. Passthrough
#' @param metadata_path. Passthrough
#' @param model_path. Passthrough
#' @param ModelID. Passthrough
#' @param LossFunction. Passthrough regression
#' @param TransformNumericColumns. Passthrough regression
#' @param GridTune. Passthrough regression
#' @param TransformationResults. Passthrough regression
#' @param TargetLevels. Passthrough multiclass
#'
#' @noRd
CatBoostValidationData <- function(ModelType = "classification",
                                   TrainOnFull. = TrainOnFull,
                                   TestDataCheck = !is.null(TestData),
                                   FinalTestTarget. = FinalTestTarget,
                                   TestTarget. = TestTarget,
                                   TrainTarget. = TrainTarget,
                                   TestMerge. = TestMerge,
                                   dataTest. = dataTest,
                                   data. = data,
                                   predict. = predict,
                                   TargetColumnName. = TargetColumnName,
                                   SaveModelObjects. = SaveModelObjects,
                                   metadata_path. = metadata_path,
                                   model_path. = model_path,
                                   ModelID. = ModelID,
                                   LossFunction. = LossFunction,
                                   TransformNumericColumns. = TransformNumericColumns,
                                   GridTune. = GridTune,
                                   TransformationResults. = TransformationResults,
                                   TargetLevels.=TargetLevels) {

  # Classification
  if(ModelType == "classification") {
    if(!TrainOnFull.) {
      if(TestDataCheck) {
        ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., TestMerge., p1 = predict.))
      } else {
        ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., dataTest., p1 = predict.))
      }
      data.table::setnames(ValidationData, "Target", eval(TargetColumnName.))
      if(SaveModelObjects.) {
        if(!is.null(metadata_path.)) {
          data.table::fwrite(ValidationData, file = file.path(metadata_path., paste0(ModelID., "_ValidationData.csv")))
        } else {
          data.table::fwrite(ValidationData, file = file.path(model_path., paste0(ModelID., "_ValidationData.csv")))
        }
      }
    } else {
      ValidationData <- data.table::as.data.table(cbind(Target = TrainTarget., data., p1 = predict.))
      data.table::setnames(ValidationData, "Target", eval(TargetColumnName.))
    }

  }

  # Regression
  if(ModelType == "regression") {

    # Generate validation data
    if(!TrainOnFull.) {
      if(TestDataCheck) {
        if(length(TargetColumnName.) > 1L) {
          ValidationData <- data.table::as.data.table(cbind(TestMerge., Predict = predict.))
        } else {
          ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., TestMerge., Predict = predict.))
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
        }
      } else {
        ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., dataTest., Predict = predict.))
        if(length(TargetColumnName.) > 1L) {
          data.table::setnames(ValidationData, c(names(ValidationData)[seq_along(TargetColumnName.)]), c(TargetColumnName.))
        } else {
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
        }
      }
    } else {
      if(!is.null(dataTest.)) {
        ValidationData <- data.table::as.data.table(cbind(dataTest., Predict = predict.))
      } else {
        ValidationData <- data.table::as.data.table(cbind(Target = TrainTarget., data., Predict = predict.))
        if(length(TargetColumnName.) > 1L) {
          data.table::setnames(ValidationData, c(names(ValidationData)[seq_along(TargetColumnName.)]), c(TargetColumnName.))
        } else {
          data.table::setnames(ValidationData, "Target", TargetColumnName.)
        }
      }
      if("ID_Factorizer" %chin% names(ValidationData)) data.table::set(ValidationData, j = "ID_Factorizer", value = NULL)
    }

    # Back transform before running metrics and plots
    if(!is.null(TransformNumericColumns.)) {
      if(GridTune. && !TrainOnFull.) TransformationResults. <- TransformationResults.[ColumnName != "Predicted"]
      if(length(TargetColumnName.) == 1L) {

        # Prepare transformation object
        TransformationResults. <- data.table::rbindlist(list(
          TransformationResults.,
          data.table::data.table(
            ColumnName = c("Predict"),
            MethodName = TransformationResults.[ColumnName == eval(TargetColumnName.), MethodName],
            Lambda = TransformationResults.[ColumnName == eval(TargetColumnName.), Lambda],
            NormalizedStatistics = 0L)))
        if(length(unique(TransformationResults.[["ColumnName"]])) != nrow(TransformationResults.)) {
          temp <- TransformationResults.[, .N, by = "ColumnName"][N != 1L][[1L]]
          if(!is.null(ValidationData)) temp1 <- which(names(ValidationData) == temp)[1L]
          if(!TrainOnFull.) {
            ValidationData[, eval(names(data.)[temp1]) := NULL]
          } else {
            if(TrainOnFull.) {
              if(length(which(names(data.) %chin% eval(TargetColumnName.))) > 1L) {
                temp1 <- which(names(data.) %chin% eval(TargetColumnName.))[1L]
                data.[, which(names(data.) %chin% eval(TargetColumnName.))[2L] := NULL]
              }
            } else {
              data.[, eval(names(data.)[temp]) := NULL]
            }
          }
          TransformationResults. <- TransformationResults.[, ID := 1L:.N][ID != max(ID)]
        }

        # Back transform
        ValidationData <- AutoTransformationScore(
          ScoringData = ValidationData,
          Type = "Inverse",
          FinalResults = TransformationResults.,
          TransID = NULL,
          Path = NULL)

      } else {

        # Prepare transformation object
        TransformationResults. <- data.table::rbindlist(list(TransformationResults., TransformationResults.))
        for(z in seq_along(TargetColumnName.)) TransformationResults.[length(TargetColumnName.) + z, ColumnName := paste0("Predict.V",z)]

        # Back transform
        ValidationData <- AutoTransformationScore(
          ScoringData = ValidationData,
          Type = "Inverse",
          FinalResults = TransformationResults.,
          TransID = NULL,
          Path = NULL)
      }
    }

    # Save validation data
    if(SaveModelObjects.) {
      if(!is.null(metadata_path.)) {
        data.table::fwrite(ValidationData, file = file.path(metadata_path., paste0(ModelID., "_ValidationData.csv")))
      } else {
        data.table::fwrite(ValidationData, file = file.path(model_path., paste0(ModelID., "_ValidationData.csv")))
      }
    }
  }

  # MultiClass
  if(ModelType == "multiclass") {

    # MultiClass Grid Validation Data ----
    if(TestDataCheck) {
      ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., predict., TestMerge.[, .SD, .SDcols = unique(names(TestMerge.)[c(1L:(ncol(TestMerge.)-1L))])]))
    } else if(!TrainOnFull.) {
      ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., predict.))
    } else {
      ValidationData <- data.table::as.data.table(cbind(Target = TrainTarget., predict.))
    }
    if(TrainOnFull.) {
      ValidationData <- merge(
        ValidationData,
        TargetLevels.,
        by.x = "Target",
        by.y = "NewLevels",
        all = FALSE)
      ValidationData[, Target := OriginalLevels][, OriginalLevels := NULL]
      ValidationData <- merge(
        ValidationData,
        TargetLevels.,
        by.x = "V2",
        by.y = "NewLevels",
        all = FALSE)
      ValidationData[, V2 := OriginalLevels][, OriginalLevels := NULL]
    } else {
      ValidationData <- merge(
        ValidationData,
        TargetLevels.,
        by.x = "V1",
        by.y = "NewLevels",
        all = FALSE)
      ValidationData[, V1 := OriginalLevels][, OriginalLevels := NULL]
      ValidationData <- merge(
        ValidationData,
        TargetLevels.,
        by.x = "Target",
        by.y = "NewLevels",
        all = FALSE)
      ValidationData[, Target := OriginalLevels][, OriginalLevels := NULL]
    }

    # MultiClass Update Names for Predicted Value Columns ----
    if(!TrainOnFull.) k <- 1L else k <- 2L
    for(name in as.character(TargetLevels.[[1L]])) {
      k <- k + 1L
      data.table::setnames(ValidationData, paste0("V", k), name)
    }
    if(!TrainOnFull.) {
      data.table::setnames(ValidationData, c("V1","Target"), c("Predict", eval(TargetColumnName.)))
      data.table::set(ValidationData, j = eval(TargetColumnName.), value = as.character(ValidationData[[eval(TargetColumnName.)]]))
    } else {
      data.table::setnames(ValidationData, c("V2","Target"), c("Predict", eval(TargetColumnName.)))
      data.table::set(ValidationData, j = eval(TargetColumnName.), value = as.character(ValidationData[[eval(TargetColumnName.)]]))
    }
    data.table::set(ValidationData, j = "Predict", value = as.character(ValidationData[["Predict"]]))
  }

  # Return
  return(ValidationData)
}

#' @title CatBoostImportances
#'
#' @description Generate variable importance, interaction importance, and shap values
#'
#' @author Adrian
#' @family CatBoost Helpers
#'
#' @param ModelType 'regression', 'classification', or 'multiclass'
#' @param TargetColumnName. Passthrough
#' @param BestGrid. Passthrough
#' @param TrainOnFull. Passthrough
#' @param TrainPool. Passthrough
#' @param TestPool. Passthrough
#' @param FinalTestPool. Passthrough
#' @param TestDataCheck Check if TestData is not null
#' @param ValidationData. Passthrough
#' @param FeatureColNames. Passthrough
#' @param GridTune. Passthrough
#' @param task_type. Passthrough
#' @param SaveModelObjects. Passthrough
#' @param model. Passthrough
#' @param ModelID. Passthrough
#' @param model_path. Passthrough
#' @param metadata_path. Passthrough
#' @param GrowPolicy. = GrowPolicy
#'
#' @noRd
CatBoostImportances <- function(ModelType = "regression",
                                TargetColumnName.=TargetColumnName,
                                BestGrid. = BestGrid,
                                TrainOnFull. = TrainOnFull,
                                TrainPool. = TrainPool,
                                TestPool. = TestPool,
                                FinalTestPool. = FinalTestPool,
                                TestDataCheck = !is.null(TestData),
                                ValidationData. = ValidationData,
                                FeatureColNames. = FeatureColNames,
                                GridTune. = GridTune,
                                task_type. = task_type,
                                SaveModelObjects. = SaveModelObjects,
                                model. = model,
                                ModelID. = ModelID,
                                model_path. = model_path,
                                metadata_path. = metadata_path,
                                GrowPolicy. = GrowPolicy) {

  # Gather artifacts
  if(!GrowPolicy. %chin% c("Depthwise","Lossguide")) {

    # Feature Information
    Interaction <- tryCatch({data.table::as.data.table(catboost::catboost.get_feature_importance(model., pool = if(TestDataCheck) FinalTestPool. else if(TrainOnFull.) TrainPool. else if(!is.null(ValidationData.)) TestPool., type = "Interaction"))}, error = function(x) NULL)
    Imp <- catboost::catboost.get_feature_importance(model., pool = if(TestDataCheck) FinalTestPool. else if(TrainOnFull.) TrainPool. else if(!is.null(ValidationData.)) TestPool., type = "PredictionValuesChange")
    VariableImportance <- data.table::data.table(cbind(Variable = rownames(Imp), Imp))
    if(ModelType != "multiclass" && length(TargetColumnName.) == 1L) ShapValues <- data.table::as.data.table(catboost::catboost.get_feature_importance(model., pool = if(TestDataCheck) FinalTestPool. else if(TrainOnFull.) TrainPool. else if(!is.null(ValidationData.)) TestPool., type = "ShapValues")) else ShapValues <- NULL

    # Gather importances
    temp <- data.table::data.table(ColNames = VariableImportance[[1L]])[, Index := 0:(.N - 1)]
    if(!is.null(ShapValues)) {
      data.table::setnames(ShapValues, names(ShapValues), c(paste0("Shap_temp",temp[[1L]]), "Predictions"), skip_absent = TRUE)
      ShapValues[, Predictions := NULL]
      ShapValues <- cbind(ValidationData., ShapValues)
    }

    # Gather interaction importances
    if(!is.null(Interaction)) {
      Interaction <- merge(Interaction, temp, by.x = "feature1_index", by.y = "Index", all = FALSE)
      data.table::setnames(Interaction, "ColNames", "Features1")
      Interaction <- merge(Interaction, temp, by.x = "feature2_index", by.y = "Index", all = FALSE)
      data.table::setnames(Interaction, "ColNames", "Features2")
      Interaction[, ":=" (feature2_index = NULL, feature1_index = NULL)]
      data.table::setcolorder(Interaction, c(2L,3L,1L))
      data.table::setorderv(Interaction, "score", -1)
    }

    # Structure data
    tryCatch({data.table::setnames(VariableImportance, "V2", "Importance")}, error = function(x) data.table::setnames(VariableImportance, "V1", "Importance"))
    VariableImportance[, Importance := round(as.numeric(Importance), 4L)]
    VariableImportance <- VariableImportance[order(-Importance)]
    if(SaveModelObjects.) {
      if(!is.null(metadata_path.)) {
        data.table::fwrite(VariableImportance, file = file.path(metadata_path., paste0(ModelID., "_VariableImportance.csv")))
        if(!is.null(ShapValues)) data.table::fwrite(ShapValues, file = file.path(metadata_path., paste0(ModelID., "_ShapValues.csv")))
        if(!is.null(Interaction)) data.table::fwrite(Interaction, file = file.path(metadata_path., paste0(ModelID., "_Interaction.csv")))
      } else {
        data.table::fwrite(VariableImportance, file = file.path(model_path., paste0(ModelID., "_VariableImportance.csv")))
        if(!is.null(ShapValues)) data.table::fwrite(ShapValues, file = file.path(model_path., paste0(ModelID., "_ShapValues.csv")))
        if(!is.null(Interaction)) data.table::fwrite(Interaction, file = file.path(model_path., paste0(ModelID., "_Interaction.csv")))
      }
    }
  } else {
    VariableImportance <- NULL
    Interaction <- NULL
    ShapValues <- NULL
  }

  # Return
  return(list(
    Interaction = Interaction,
    VariableImportance = VariableImportance,
    ShapValues = ShapValues))
}

#' @title CatBoostPDF
#'
#' @description Send model insights to pdf
#'
#' @author Adrian
#' @family CatBoost Helpers
#'
#' @param ModelClass 'catboost', 'xgboost', 'h2o'
#' @param ModelType 'regression', 'classification', 'multiclass', or 'vector'
#' @param TrainOnFull. Passthrough
#' @param SaveInfoToPDF. Passthrough
#' @param EvaluationPlot. Passthrough
#' @param EvaluationBoxPlot. Passthrough
#' @param VariableImportance. Passthrough
#' @param ParDepPlots. Passthrough
#' @param ParDepBoxPlots. Passthrough
#' @param EvalMetrics. Passthrough
#' @param Interaction. Passthrough
#' @param model_path. Passthrough
#' @param metadata_path. Passthrough
#'
#' @noRd
CatBoostPDF <- function(ModelClass = "catboost",
                        ModelType = "regression",
                        TrainOnFull. = TrainOnFull,
                        SaveInfoToPDF. = SaveInfoToPDF,
                        EvaluationPlot. = EvaluationPlot,
                        EvaluationBoxPlot. = EvaluationBoxPlot,
                        ParDepPlots. = ParDepPlots,
                        ParDepBoxPlots. = ParDepBoxPlots,
                        EvalMetrics. = EvalMetrics,
                        VariableImportance. = VariableImportance,
                        Interaction. = Interaction,
                        model_path. = model_path,
                        metadata_path. = metadata_path) {

  # Classification
  if(ModelType == "classification") {
    if(!TrainOnFull. && SaveInfoToPDF.) {
      EvalPlotList <- list(EvaluationPlot., if(!is.null(VariableImportance.)) VI_Plot(Type = ModelClass, VI_Data = VariableImportance.) else NULL)
      ParDepList <- list(if(!is.null(ParDepPlots.)) ParDepPlots. else NULL)
      TableMetrics <- list(EvalMetrics., if(!is.null(VariableImportance.)) VariableImportance. else NULL, if(!is.null(Interaction.)) Interaction. else NULL)
    } else {
      return(NULL)
    }
  }

  # Regression
  if(ModelType == "regression") {
    if(!TrainOnFull. && SaveInfoToPDF.) {
      EvalPlotList <- list(EvaluationPlot., EvaluationBoxPlot., if(!is.null(VariableImportance.)) VI_Plot(Type = ModelClass, VI_Data = VariableImportance.) else NULL)
      ParDepList <- list(if(!is.null(ParDepPlots.)) ParDepPlots. else NULL, if(!is.null(ParDepBoxPlots.)) ParDepBoxPlots. else NULL)
      TableMetrics <- list(EvalMetrics., if(!is.null(VariableImportance.)) VariableImportance. else NULL, if(!is.null(Interaction.)) Interaction. else NULL)
    } else {
      return(NULL)
    }
  }

  # Print to pdf
  try(PrintToPDF(
    Path = if(!is.null(metadata_path.)) metadata_path. else if(!is.null(model_path.)) model_path. else getwd(),
    OutputName = "EvaluationPlots",
    ObjectList = EvalPlotList,
    Title = "Model Evaluation Plots",
    Width = 12, Height = 7, Paper = "USr", BackgroundColor = "transparent", ForegroundColor = "black"))
  try(PrintToPDF(
    Path = if(!is.null(metadata_path.)) metadata_path. else if(!is.null(model_path.)) model_path. else getwd(),
    OutputName = "PartialDependencePlots",
    ObjectList = ParDepList,
    Title = "Partial Dependence Calibration Plots",
    Width = 12, Height = 7, Paper = "USr", BackgroundColor = "transparent", ForegroundColor = "black"))
  try(PrintToPDF(
    Path = if(!is.null(metadata_path.)) metadata_path. else if(!is.null(model_path.)) model_path. else getwd(),
    OutputName = "Metrics_and_Importances",
    ObjectList = TableMetrics,
    MaxPages = 100,
    Tables = TRUE,
    Title = "Model Metrics and Variable Importances",
    Width = 12, Height = 7, Paper = "USr", BackgroundColor = "transparent", ForegroundColor = "black"))
  while(dev.cur() > 1) grDevices::dev.off()
}

#' @title CatBoostRemoveFiles
#'
#' @description Remove temp files generated by catboost
#'
#' @author Adrian
#' @family CatBoost Helpers
#'
#' @param GridTune. Passthrough
#'
#' @noRd
CatBoostRemoveFiles <- function(GridTune. = GridTune, model_path.=model_path) {
  if(GridTune.) {
    if(file.exists(file.path(model_path.,"catboost_training.json"))) file.remove(file.path(model_path.,"catboost_training.json"))
    if(file.exists(file.path(model_path.,"learn_error.tsv"))) file.remove(file.path(model_path.,"learn_error.tsv"))
    if(file.exists(file.path(model_path.,"test_error.tsv"))) file.remove(file.path(model_path.,"test_error.tsv"))
    if(file.exists(file.path(model_path.,"time_left.tsv"))) file.remove(file.path(model_path.,"time_left.tsv"))
    if(dir.exists(file.path(model_path.,"catboost_info"))) unlink(x = file.path(model_path.,"catboost_info"), recursive = TRUE)
    if(dir.exists(file.path(model_path.,"learn"))) unlink(x = file.path(model_path.,"learn"), recursive = TRUE)
    if(dir.exists(file.path(model_path.,"test"))) unlink(x = file.path(model_path.,"test"), recursive = TRUE)
    if(dir.exists(file.path(model_path.,"tmp"))) unlink(x = file.path(model_path.,"tmp"), recursive = TRUE)
  } else {
    if(dir.exists(file.path(model_path.,"catboost_info"))) unlink(x = file.path(model_path.,"catboost_info"), recursive = TRUE)
  }
}

#' @title CatBoostParameterGrids
#'
#' @description CatBoostParameterGrids https://catboost.ai/docs/concepts/r-training-parameters.html
#'
#' @author Adrian Antico
#' @family CatBoost Helpers
#'
#' @param TaskType "GPU" or "CPU"
#' @param Shuffles The number of shuffles you want to apply to each grid
#' @param NTrees seq(1000L, 10000L, 1000L)
#' @param Depth seq(4L, 16L, 2L)
#' @param LearningRate seq(0.01,.10,0.01)
#' @param L2_Leaf_Reg c(1.0:10.0)
#' @param RandomStrength seq(1, 2, 0.1)
#' @param BorderCount seq(32,256,32)
#' @param RSM CPU ONLY, Random subspace method.c(0.80, 0.85, 0.90, 0.95, 1.0)
#' @param BootStrapType c("Bayesian", "Bernoulli", "Poisson", "MVS", "No")
#' @param GrowPolicy c("SymmetricTree", "Depthwise", "Lossguide")
#'
#' @return A list containing data.table's with the parameters shuffled and ready to test in the bandit framework
#' @noRd
CatBoostParameterGrids <- function(TaskType = "CPU",
                                   Shuffles = 1L,
                                   NTrees = seq(1000L, 10000L, 1000L),
                                   Depth = seq(4L, 16L, 2L),
                                   LearningRate = c(0.01,0.02,0.03,0.04),
                                   L2_Leaf_Reg = seq(1.0, 10.0, 1.0),
                                   RandomStrength = seq(1, 2, 0.1),
                                   BorderCount = seq(32,256,32),
                                   RSM = c(0.80, 0.85, 0.90, 0.95, 1.0),
                                   BootStrapType = c("Bayesian", "Bernoulli", "Poisson", "MVS", "No"),
                                   GrowPolicy = c("SymmetricTree", "Depthwise", "Lossguide")) {

  # Create grid sets----
  GridList <- list()
  if(!is.null(NTrees)) GridList[["NTrees"]] <- sort(NTrees, decreasing = FALSE) else GridList[["NTrees"]] <- seq(1000L, 10000L, 1000L)
  if(!is.null(Depth)) GridList[["Depth"]] <- sort(Depth, decreasing = FALSE) else GridList[["Depth"]] <- seq(4L, 16L, 2L)
  if(!is.null(LearningRate)) GridList[["LearningRate"]] <- sort(LearningRate, decreasing = FALSE) else GridList[["LearningRate"]] <- seq(0.01,0.10,0.01)
  if(!is.null(L2_Leaf_Reg)) GridList[["L2_Leaf_Reg"]] <- L2_Leaf_Reg else GridList[["L2_Leaf_Reg"]] <- seq(1.0, 10.0, 1.0)
  if(!is.null(RandomStrength)) GridList[["RandomStrength"]] <- RandomStrength else GridList[["RandomStrength"]] <- seq(1,2,0.1)
  if(!is.null(BorderCount)) GridList[["BorderCount"]] <- BorderCount else GridList[["BorderCount"]] <- seq(32,256,32)
  if(!is.null(RSM)) GridList[["RSM"]] <- RSM else GridList[["RSM"]] <- c(0.80, 0.85, 0.90, 0.95, 1.0)
  if(!is.null(BootStrapType)) GridList[["BootStrapType"]] <- BootStrapType else GridList[["BootStrapType"]] <- c("Bayesian", "Bernoulli", "Poisson", "MVS", "No")
  if(!is.null(GrowPolicy)) GridList[["GrowPolicy"]] <- GrowPolicy else GridList[["GrowPolicy"]] <- c("SymmetricTree", "Depthwise", "Lossguide")

  # Create grid ----
  Grid <- do.call(data.table::CJ, GridList)

  # Filter out invalid grids----
  if(tolower(TaskType) == "gpu") {
    data.table::set(Grid, j = "RSM", value = NULL)
    Grid <- Grid[!BootStrapType %chin% c("MVS")]
    Grid <- unique(Grid[BootStrapType != "Poisson" & GrowPolicy != "Lossguide"])
  } else {
    Grid <- Grid[!tolower(BootStrapType) %chin% c("poisson")]
  }

  # Total loops----
  N_NTrees <- length(unique(Grid[["NTrees"]]))
  N_Depth <- length(unique(Grid[["Depth"]]))
  N_LearningRate <- length(unique(Grid[["LearningRate"]]))
  N_L2_Leaf_Reg <- length(unique(Grid[["L2_Leaf_Reg"]]))
  Runs <- max(N_NTrees, N_Depth, N_LearningRate)
  Grids <- list()

  # Create grid sets----
  for(i in seq_len(Runs)) {
    if(i == 1L) {
      Grids[[paste0("Grid_",i)]] <- Grid[NTrees <= unique(Grid[["NTrees"]])[min(i,N_NTrees)] & Depth <= unique(Grid[["Depth"]])[min(i,N_Depth)] & LearningRate <= unique(Grid[["LearningRate"]])[min(i,N_LearningRate)]]
    } else {
      Grids[[paste0("Grid_",i)]] <- data.table::fsetdiff(
        Grid[NTrees <= unique(Grid[["NTrees"]])[min(i,N_NTrees)] & Depth <= unique(Grid[["Depth"]])[min(i,N_Depth)] & LearningRate <= unique(Grid[["LearningRate"]])[min(i,N_LearningRate)]],
        Grid[NTrees <= unique(Grid[["NTrees"]])[min(i-1L,N_NTrees)] & Depth <= unique(Grid[["Depth"]])[min(i-1L,N_Depth)] & LearningRate <= unique(Grid[["LearningRate"]])[min(i-1L,N_LearningRate)]])
    }
  }

  # Define experimental grid----
  eGrid <- data.table::data.table(
    GridNumber = rep(-1, 10000L),
    RunNumber = 1L:10000L,
    RunTime = rep(-1, 10000L),
    TreesBuilt = rep(-1,10000L),
    NTrees = rep(-1,10000L),
    Depth = rep(-1,10000L),
    LearningRate = rep(-1,10000L),
    L2_Leaf_Reg = rep(-1,10000L),
    RandomStrength = rep(-1,10000L),
    BorderCount = rep(-1,10000L),
    RSM = rep(-1,10000L),
    BootStrapType = rep("aa", 10000L),
    GrowPolicy = rep("aa", 10000L))

  # Add columns for bandit probs
  data.table::set(eGrid, j = paste0("BanditProbs_", names(Grids)), value = -10)

  # Shuffle grid sets----
  for(shuffle in seq_len(Shuffles)) for(i in seq_len(Runs)) Grids[[paste0("Grid_", i)]] <- Grids[[paste0("Grid_",i)]][order(runif(Grids[[paste0("Grid_",i)]][,.N]))]

  # Return grid----
  return(list(Grid = Grid, Grids = Grids, ExperimentalGrid = eGrid))
}

#' @title CatBoostClassifierParams
#'
#' @author Adrian Antico
#' @family CatBoost Helpers
#'
#' @param N. Iteration for specific grid in grid clusters
#' @param counter. Passthrough
#' @param BanditArmsN. Passthrough
#' @param HasTime. Passthrough
#' @param MetricPeriods. Passthrough
#' @param ClassWeights. Passthrough
#' @param EvalMetric. Passthrough
#' @param LossFunction. Passthrough
#' @param task_type. Passthrough
#' @param NumGPUs. Passthrough
#' @param model_path. Passthrough
#' @param NewGrid. Passthrough
#' @param Grid. Passthrough
#' @param GridClusters. Passthrough
#'
#' @noRd
CatBoostGridParams <- function(N.=N,
                               counter. = NULL,
                               BanditArmsN. = NULL,
                               HasTime. = NULL,
                               MetricPeriods. = NULL,
                               ClassWeights. = NULL,
                               EvalMetric. = NULL,
                               LossFunction. = NULL,
                               task_type. = NULL,
                               NumGPUs. = NULL,
                               model_path. = NULL,
                               NewGrid. = NULL,
                               Grid. = NULL,
                               GridClusters. = NULL) {

  # Create base_params (independent of runs)
  base_params <- list()
  base_params$has_time <- HasTime.
  base_params$metric_period <- MetricPeriods.
  base_params$loss_function <- LossFunction.
  base_params$eval_metric <- EvalMetric.
  base_params$use_best_model <- TRUE
  base_params$best_model_min_trees <- 10L
  base_params$task_type <- task_type.
  base_params$devices <- NumGPUs.
  base_params$thread_count <- parallel::detectCores()
  base_params$train_dir <- model_path.
  base_params$class_weights <- ClassWeights.

  # Run-dependent args and updates
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$iterations <- GridClusters.[[paste0("Grid_", counter.-1L)]][["NTrees"]][1L] else if(counter. != 1L) base_params$iterations <- GridClusters.[[paste0("Grid_",NewGrid.)]][["NTrees"]][N.] else base_params$iterations <- max(Grid.$NTrees)
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$depth <- GridClusters.[[paste0("Grid_", counter.-1L)]][["Depth"]][1L] else if(counter. != 1) base_params$depth <- GridClusters.[[paste0("Grid_",NewGrid.)]][["Depth"]][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$learning_rate <- GridClusters.[[paste0("Grid_", counter.-1L)]][["LearningRate"]][1L] else if(counter. != 1L) base_params$learning_rate <- GridClusters.[[paste0("Grid_",NewGrid.)]][["LearningRate"]][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$l2_leaf_reg <- GridClusters.[[paste0("Grid_",counter.-1L)]][["L2_Leaf_Reg"]][1L] else if(counter. != 1L) base_params$l2_leaf_reg <- GridClusters.[[paste0("Grid_",NewGrid.)]][["L2_Leaf_Reg"]][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$random_strength <- GridClusters.[[paste0("Grid_",counter.-1L)]][["RandomStrength"]][1L] else if(counter. != 1L) base_params$random_strength <- GridClusters.[[paste0("Grid_",NewGrid.)]][["RandomStrength"]][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$border_count <- GridClusters.[[paste0("Grid_",counter.-1L)]][["BorderCount"]][1L] else if(counter. != 1L) base_params$border_count <- GridClusters.[[paste0("Grid_",NewGrid.)]][["BorderCount"]][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$bootstrap_type <- GridClusters.[[paste0("Grid_",counter.-1L)]][["BootStrapType"]][1L] else if(counter. != 1L) base_params$bootstrap_type <- GridClusters.[[paste0("Grid_",NewGrid.)]][["BootStrapType"]][N.]

  # TaskType-dependent args
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) {
    if(tolower(task_type.) == "gpu") {
      base_params$rsm <- NULL
      base_params$grow_policy <- GridClusters.[[paste0("Grid_", counter.-1L)]][["GrowPolicy"]][N.]
    } else {
      base_params$rsm <- GridClusters.[[paste0("Grid_", counter.-1L)]][["RSM"]][N.]
      base_params$grow_policy <- NULL
    }
  } else if(counter. != 1L) {
    if(tolower(task_type.) == "gpu") {
      base_params$rsm <- NULL
      base_params$grow_policy <- GridClusters.[[paste0("Grid_",NewGrid.)]][["GrowPolicy"]][N.]
    } else {
      base_params$rsm <- GridClusters.[[paste0("Grid_", NewGrid.)]][["RSM"]][N.]
      base_params$grow_policy <- NULL
    }
  }

  # Return
  return(base_params)
}

#' @param GroupVariables. Passthrough
#' @param Difference. Passthrough
#' @param Step1SCore. Passthrough
#'
#' @noRd
CarmaRecordCount <- function(GroupVariables. = GroupVariables,
                             Difference. = Difference,
                             Step1SCore. = Step1SCore) {
  if(!is.null(GroupVariables.)) {
    if(Difference.) {
      if(!"GroupVar" %chin% names(Step1SCore.)) N. <- as.integer(Step1SCore.[, .N, by = c(eval(GroupVariables.))][, max(N)]) else N. <- as.integer(Step1SCore.[, .N, by = "GroupVar"][, max(N)])
    } else {
      N. <- as.integer(Step1SCore.[, .N, by = "GroupVar"][, max(N)])
    }
  } else {
    N. <- as.integer(Step1SCore.[, .N])
  }
  return(N.)
}
