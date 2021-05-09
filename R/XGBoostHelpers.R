#' @title XGBoostArgsCheck
#'
#' @author Adrian Antico
#' @family XGBoost Helpers
#'
#' @param GridTune. Passthrough
#' @param model_path. Passthrough
#' @param metadata_path. Passthrough
#' @param Trees. Passthrough
#' @param max_depth. Passthrough
#' @param eta. Passthrough
#' @param min_child_weight. Passthrough
#' @param subsample. Passthrough
#' @param colsample_bytree. Passthrough
#' @return A list containing data.table's with the parameters shuffled and ready to test in the bandit framework
#' @noRd
XGBoostArgsCheck <- function(GridTune.=GridTune,
                             model_path.=model_path,
                             metadata_path.=metadata_path,
                             Trees.=NTrees,
                             max_depth.=max_depth,
                             eta.=eta,
                             min_child_weight.=min_child_weight,
                             subsample.=subsample,
                             colsample_bytree.=colsample_bytree) {

  # Ensure model_path. and metadata_path. exists----
  if(!is.null(model_path.) && !dir.exists(file.path(model_path.))) dir.create(model_path.)
  if(!is.null(metadata_path.) && !is.null(metadata_path.)) if(!dir.exists(file.path(metadata_path.))) dir.create(metadata_path.)

  # Ensure only one value if not grid tuning
  if(!GridTune. && length(Trees.) > 1) stop("Trees cannot have more than one value supplied")
  if(!GridTune. && length(max_depth.) > 1) stop("Depth cannot have more than one value supplied")
  if(!GridTune. && length(eta.) > 1) stop("LearningRate cannot have more than one value supplied")
  if(!GridTune. && length(min_child_weight.) > 1) stop("L2_Leaf_Reg cannot have more than one value supplied")
  if(!GridTune. && length(subsample.) > 1) stop("L2_Leaf_Reg cannot have more than one value supplied")
  if(!GridTune. && length(colsample_bytree.) > 1) stop("L2_Leaf_Reg cannot have more than one value supplied")
}

#' @title XGBoostDataPrep
#'
#' @description Prepare data for XGBoost modeling
#'
#' @family XGBoost Helpers
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
#' @param ReturnFactorLevels. Passthrough
#' @param EncodingMethod. Passthrough
#'
#' @noRd
XGBoostDataPrep <- function(ModelType = "regression",
                            data. = data,
                            ValidationData. = ValidationData,
                            TestData. = TestData,
                            TargetColumnName. = TargetColumnName,
                            FeatureColNames. = FeatureColNames,
                            IDcols. = IDcols,
                            TransformNumericColumns. = TransformNumericColumns,
                            Methods. = Methods,
                            ModelID. = ModelID,
                            model_path. = model_path,
                            TrainOnFull. = TrainOnFull,
                            SaveModelObjects. = SaveModelObjects,
                            ReturnFactorLevels.=ReturnFactorLevels,
                            EncodingMethod. = EncodingMethod) {

  # Ensure data. is a data.table
  if(!data.table::is.data.table(data.)) data.table::setDT(data.)
  if(!is.null(ValidationData.) && !data.table::is.data.table(ValidationData.)) data.table::setDT(ValidationData.)
  if(!is.null(TestData.) && !data.table::is.data.table(TestData.)) data.table::setDT(TestData.)

  # Target Name Storage
  if(!is.character(TargetColumnName.)) TargetColumnName. <- names(data.)[TargetColumnName.]
  if(!is.character(FeatureColNames.)) FeatureColNames. <- names(data.)[FeatureColNames.]
  if(!is.null(IDcols.) && !is.character(IDcols.)) IDcols. <- names(data.)[IDcols.]

  # Identify column numbers for factor variables
  CatFeatures <- sort(c(as.numeric(which(sapply(data., is.factor))), as.numeric(which(sapply(data., is.character)))))
  if(!identical(numeric(0), CatFeatures)) {
    CatFeatures <- names(data.)[CatFeatures]
    CatFeatures <- CatFeatures[!CatFeatures %chin% IDcols.]
  } else if(length(CatFeatures) == 0L) {
    CatFeatures <- NULL
  }

  # Target var management
  if(ModelType == "multiclass") CatFeatures <- setdiff(CatFeatures, TargetColumnName.)

  # Classification
  if(ModelType == "classification") {

    # Data Partition
    if(is.null(ValidationData.) && is.null(TestData.) && !TrainOnFull.) {
      dataSets <- AutoDataPartition(
        data = data.,
        NumDataSets = 3L,
        Ratios = c(0.70, 0.20, 0.10),
        PartitionType = "random",
        StratifyColumnNames = TargetColumnName.,
        TimeColumnName = NULL)
      data. <- dataSets$TrainData
      ValidationData. <- dataSets$ValidationData
      TestData. <- dataSets$TestData
    }

    # Data Subset Columns Needed ----
    keep <- c(TargetColumnName., FeatureColNames.)
    dataTrain <- data.[, ..keep]
    if(!is.null(ValidationData.)) dataTest <- ValidationData.[, ..keep] else dataTest <- NULL

    # TestData Subset Columns Needed ----
    if(!is.null(TestData.)) {
      TestMerge <- data.table::copy(TestData.)
      TestData. <- TestData.[, ..keep]
    } else {
      TestMerge <- NULL
      TestData. <- NULL
    }

    # Dummify dataTrain Categorical Features ----
    Output <- RemixAutoML:::EncodeCharacterVariables(RunMode='train', ModelType=ModelType, TrainData=dataTrain, ValidationData=dataTest, TestData=TestData., TargetVariableName=TargetColumnName., CategoricalVariableNames=CatFeatures, EncodeMethod=EncodingMethod., KeepCategoricalVariables=FALSE, ReturnMetaData=TRUE, MetaDataPath=model_path., MetaDataList=NULL, ImputeMissingValue=0)
    dataTrain <- Output$TrainData; Output$TrainData <- NULL
    dataTest <- Output$ValidationData; Output$ValidationData <- NULL
    TestData. <- Output$TestData.; Output$TestData. <- NULL
    FactorLevelsList <- Output$MetaData; rm(Output)

    # Save Names of data
    if(is.numeric(FeatureColNames.)) {
      Names <- data.table::as.data.table(names(data.)[FeatureColNames.])
      data.table::setnames(Names, "V1", "ColNames")
    } else {
      Names <- data.table::as.data.table(FeatureColNames.)
      if(!"V1" %chin% names(Names)) {
        data.table::setnames(Names, "FeatureColNames.", "ColNames")
      } else {
        data.table::setnames(Names, "V1", "ColNames")
      }
    }

    # Save column names
    if(SaveModelObjects.) data.table::fwrite(Names, file = file.path(model_path., paste0(ModelID., "_ColNames.csv")))

    # Subset TargetColumnName. Variables----
    TrainTarget <- dataTrain[, get(TargetColumnName.)]
    if(!TrainOnFull.) TestTarget <- dataTest[, get(TargetColumnName.)]
    if(!is.null(TestData.)) FinalTestTarget <- TestData.[, get(TargetColumnName.)]

    # Remove TargetColumnName. Variable from Feature Data
    data.table::set(dataTrain, j = TargetColumnName., value = NULL)
    if(!TrainOnFull.) data.table::set(dataTest, j = TargetColumnName., value = NULL)
    if(!is.null(TestData.)) data.table::set(TestData., j = TargetColumnName., value = NULL)

    # Initialize xgboost Data Conversion
    datatrain <- xgboost::xgb.DMatrix(as.matrix(dataTrain), label = TrainTarget)
    if(!is.null(dataTest)) datavalidate <- xgboost::xgb.DMatrix(as.matrix(dataTest), label = TestTarget)
    if(!is.null(TestData.)) {
      datatest <- xgboost::xgb.DMatrix(as.matrix(TestData.), label = FinalTestTarget)
      EvalSets <- list(train = datavalidate, test = datatest)
    } else if(!TrainOnFull. && exists("datavalidate")) {
      EvalSets <- list(train = datatrain, test = datavalidate)
    } else {
      EvalSets <- list(train = datatrain)
      datavalidate <- NULL
      datatest <- NULL
      TestTarget <- NULL
      FinalTestTarget <- NULL
      dataTest <- NULL
    }
  }

  # Regression
  if(ModelType == "regression") {

    # Transform data., ValidationData., and TestData.
    if(!is.null(ValidationData.) && !is.null(TransformNumericColumns.)) {
      MeanTrainTarget <- data.[, mean(get(TargetColumnName.))]
      Output <- AutoTransformationCreate(
        data.,
        ColumnNames = TransformNumericColumns.,
        Methods = Methods.,
        Path = model_path.,
        TransID = ModelID.,
        SaveOutput = SaveModelObjects.)
      data. <- Output$Data
      TransformationResults <- Output$FinalResults

      # Transform ValidationData.
      ValidationData. <- AutoTransformationScore(
        ScoringData = ValidationData.,
        Type = "Apply",
        FinalResults = TransformationResults,
        TransID = NULL,
        Path = NULL)

      # Transform TestData.
      if(!is.null(TestData.)) {
        TestData. <- AutoTransformationScore(
          ScoringData = TestData.,
          Type = "Apply",
          FinalResults = TransformationResults,
          TransID = NULL,
          Path = NULL)
      }
    }

    # Regression Data Partition----
    if(is.null(ValidationData.) && is.null(TestData.) && !TrainOnFull.) {
      if(!is.null(TransformNumericColumns.)) {
        dataSets <- AutoDataPartition(
          data.,
          NumDataSets = 3L,
          Ratios = c(0.70, 0.20, 0.10),
          PartitionType = "random",
          StratifyColumnNames = NULL,
          TimeColumnName = NULL)
        data. <- dataSets$TrainData
        ValidationData. <- dataSets$ValidationData
        TestData. <- dataSets$TestData

        # Mean of data.
        MeanTrainTarget <- data.[, mean(get(TargetColumnName.))]

        # Transform data. sets
        Output <- AutoTransformationCreate(
          data.,
          ColumnNames = TransformNumericColumns.,
          Methods = Methods.,
          Path = model_path.,
          TransID = ModelID.,
          SaveOutput = SaveModelObjects.)
        data. <- Output$Data
        TransformationResults <- Output$FinalResults

        # Transform ValidationData.
        ValidationData. <- AutoTransformationScore(
          ScoringData = ValidationData.,
          Type = "Apply",
          FinalResults = TransformationResults,
          TransID = NULL,
          Path = NULL)

        # Transform TestData.
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
          data.,
          NumDataSets = 3L,
          Ratios = c(0.70, 0.20, 0.10),
          PartitionType = "random",
          StratifyColumnNames = NULL,
          TimeColumnName = NULL)
        data. <- dataSets$TrainData
        ValidationData. <- dataSets$ValidationData
        TestData. <- dataSets$TestData
        MeanTrainTarget <- data.[, mean(get(TargetColumnName.))]
      }
    }

    # Regression data. Subset Columns Needed
    if((is.numeric(FeatureColNames.) || is.integer(FeatureColNames.)) && !TrainOnFull.) {
      keep1 <- names(data.)[c(FeatureColNames.)]
      keep <- c(keep1, TargetColumnName.)
      TrainMerge <- data.table::copy(data.)
      dataTrain <- data.[, ..keep]
      ValidMerge <- data.table::copy(ValidationData.)
      dataTest <- ValidationData.[, ..keep]
    } else if((is.numeric(FeatureColNames.) || is.integer(FeatureColNames.)) && TrainOnFull.) {
      keep1 <- names(data.)[c(FeatureColNames.)]
      keep <- c(keep1, TargetColumnName.)
      dataTrain <- data.[, ..keep]
      TrainMerge <- data.table::copy(dataTrain)
      dataTest <- NULL
    } else if(!TrainOnFull.) {
      keep <- c(FeatureColNames., TargetColumnName.)
      dataTrain <- data.[, ..keep]
      TrainMerge <- data.table::copy(dataTrain)
      ValidMerge <- data.table::copy(ValidationData.)
      dataTest <- ValidationData.[, ..keep]
    } else {
      keep <- c(FeatureColNames., TargetColumnName.)
      dataTrain <- data.[, ..keep]
      TrainMerge <- data.table::copy(dataTrain)
      dataTest <- NULL
    }

    # Regression TestData. Subset Columns Needed
    if(!is.null(TestData.)) {
      if(is.numeric(FeatureColNames.) || is.integer(FeatureColNames.)) {
        keep1 <- names(TestData.)[c(FeatureColNames.)]
        if(!is.null(IDcols.)) keep <- c(IDcols., keep1, TargetColumnName.) else keep <- c(keep1, TargetColumnName.)
        TestData. <- TestData.[, ..keep]
      } else {
        keep1 <- c(FeatureColNames.)
        if(!is.null(IDcols.)) keep <- c(IDcols., FeatureColNames., TargetColumnName.) else keep <- c(FeatureColNames., TargetColumnName.)
        TestData. <- TestData.[, ..keep]
      }
      if(!is.null(IDcols.)) {
        TestMerge <- data.table::copy(TestData.)
        keep <- c(keep1, TargetColumnName.)
        TestData. <- TestData.[, ..keep]
      } else {
        TestMerge <- data.table::copy(TestData.)
      }
    }

    # Dummify dataTrain Categorical Features ----
    Output <- EncodeCharacterVariables(RunMode='train', ModelType=ModelType, TrainData=dataTrain, ValidationData=dataTest, TestData=TestData., TargetVariableName=TargetColumnName., CategoricalVariableNames=CatFeatures, EncodeMethod=EncodingMethod., KeepCategoricalVariables=FALSE, ReturnMetaData=TRUE, MetaDataPath=model_path., MetaDataList=NULL, ImputeMissingValue=0)
    dataTrain <- Output$TrainData; Output$TrainData <- NULL
    dataTest <- Output$ValidationData; Output$ValidationData <- NULL
    TestData. <- Output$TestData.; Output$TestData. <- NULL
    FactorLevelsList <- Output$MetaData; rm(Output)

    # Regression Save Names of data
    if(is.numeric(FeatureColNames.)) {
      Names <- data.table::as.data.table(names(data.)[FeatureColNames.])
      data.table::setnames(Names, "V1", "ColNames")
    } else {
      Names <- data.table::as.data.table(FeatureColNames.)
      if(!"V1" %chin% names(Names)) {
        data.table::setnames(Names, "FeatureColNames.", "ColNames")
      } else {
        data.table::setnames(Names, "V1", "ColNames")
      }
    }

    # Save feature names
    if(SaveModelObjects.) data.table::fwrite(Names, file = file.path(model_path., paste0(ModelID., "_ColNames.csv")))

    # Regression Subset Target Variables
    TrainTarget <- dataTrain[, get(TargetColumnName.)]
    if(!is.null(dataTest)) {
      TestTarget <- dataTest[, get(TargetColumnName.)]
      if(!is.null(TestData.)) FinalTestTarget <- TestData.[, get(TargetColumnName.)]
    }

    # Regression Remove Target Variable from Feature Data
    data.table::set(dataTrain, j = TargetColumnName., value = NULL)
    if(!is.null(dataTest)) data.table::set(dataTest, j = TargetColumnName., value = NULL)
    if(!is.null(TestData.)) data.table::set(TestData., j = TargetColumnName., value = NULL)

    # Regression Initialize Catboost Data Conversion
    if("GroupVar" %chin% names(dataTrain)) data.table::set(dataTrain, j = "GroupVar", value = NULL)
    datatrain <- xgboost::xgb.DMatrix(as.matrix(dataTrain), label = TrainTarget)
    if(!TrainOnFull.) {
      if("GroupVar" %chin% names(dataTest)) data.table::set(dataTest, j = "GroupVar", value = NULL)
      datavalidate <- xgboost::xgb.DMatrix(as.matrix(dataTest), label = TestTarget)
      if(!is.null(TestData.)) {
        if("GroupVar" %chin% names(TestData.)) data.table::set(TestData., j = "GroupVar", value = NULL)
        datatest <- xgboost::xgb.DMatrix(as.matrix(TestData.), label = FinalTestTarget)
        EvalSets <- list(train = datavalidate, test = datatest)
      } else {
        EvalSets <- list(train = datatrain, test = datavalidate)
      }
    } else {
      EvalSets <- list(train = datatrain)
    }
  }

  # MultiClass
  if(ModelType == "multiclass") {

    # MultiClass Data Partition
    if(is.null(ValidationData.) && is.null(TestData.) && !TrainOnFull.) {
      dataSets <- AutoDataPartition(
        data = data.,
        NumDataSets = 3L,
        Ratios = c(0.70, 0.20, 0.10),
        PartitionType = "random",
        StratifyColumnNames = TargetColumnName.,
        TimeColumnName = NULL)
      data. <- dataSets$TrainData
      ValidationData. <- dataSets$ValidationData
      TestData. <- dataSets$TestData
    }

    # MultiClass data. Subset Columns Needed
    if(is.numeric(FeatureColNames.) || is.integer(FeatureColNames.)) {
      keep1 <- names(data.)[c(FeatureColNames.)]
      keep <- c(keep1, TargetColumnName.)
      dataTrain <- data.[, ..keep]
      if(!is.null(ValidationData.)) dataTest <- ValidationData.[, ..keep] else dataTest <- NULL
    } else if(!TrainOnFull.) {
      keep <- c(FeatureColNames., TargetColumnName.)
      dataTrain <- data.[, ..keep]
      if(!is.null(ValidationData.)) dataTest <- ValidationData.[, ..keep] else dataTest <- NULL
    }

    # MultiClass TestData Subset Columns Needed
    if(!is.null(TestData.)) {
      if(is.numeric(FeatureColNames.) || is.integer(FeatureColNames.)) {
        keep1 <- names(TestData.)[c(FeatureColNames.)]
        if(!is.null(IDcols.)) keep <- c(IDcols., keep1, TargetColumnName.) else keep <- c(keep1, TargetColumnName.)
        TestData. <- TestData.[, ..keep]
      } else {
        keep1 <- c(FeatureColNames.)
        if(!is.null(IDcols.)) keep <- c(IDcols., FeatureColNames., TargetColumnName.) else keep <- c(FeatureColNames., TargetColumnName.)
        TestData. <- TestData.[, ..keep]
      }
      if(!is.null(IDcols.)) {
        TestMerge <- data.table::copy(TestData.)
        keep <- c(keep1, TargetColumnName.)
        TestData. <- TestData.[, ..keep]
      } else {
        TestMerge <- data.table::copy(TestData.)
      }
    }

    # Dummify dataTrain Categorical Features ----
    Output <- EncodeCharacterVariables(RunMode='train', ModelType=ModelType, TrainData=dataTrain, ValidationData=dataTest, TestData=TestData., TargetVariableName=TargetColumnName., CategoricalVariableNames=CatFeatures, EncodeMethod=EncodingMethod., KeepCategoricalVariables=FALSE, ReturnMetaData=TRUE, MetaDataPath=model_path., MetaDataList=NULL, ImputeMissingValue=0)
    dataTrain <- Output$TrainData; Output$TrainData <- NULL
    dataTest <- Output$ValidationData; Output$ValidationData <- NULL
    TestData. <- Output$TestData.; Output$TestData. <- NULL
    FactorLevelsList <- Output$MetaData; rm(Output)

    # MultiClass Obtain Unique Target Levels
    if(!is.null(dataTest) && !is.null(TestData.)) {
      temp <- data.table::rbindlist(list(dataTrain, dataTest, TestData.))
    } else if(!is.null(dataTest)) {
      temp <- data.table::rbindlist(list(dataTrain, dataTest))
    } else {
      temp <- dataTrain
    }
    TargetLevels <- data.table::as.data.table(sort(unique(temp[[eval(TargetColumnName.)]])))
    data.table::setnames(TargetLevels, "V1", "OriginalLevels")
    TargetLevels[, NewLevels := 0L:(.N - 1L)]
    if(SaveModelObjects.) data.table::fwrite(TargetLevels, file = file.path(model_path., paste0(ModelID., "_TargetLevels.csv")))

    # Number of levels
    NumLevels <- TargetLevels[, .N]

    # MultiClass Convert Target to Numeric Factor
    dataTrain <- merge(dataTrain, TargetLevels, by.x = eval(TargetColumnName.), by.y = "OriginalLevels", all = FALSE)
    dataTrain[, paste0(TargetColumnName.) := NewLevels]
    dataTrain[, NewLevels := NULL]
    if(!is.null(dataTest)) {
      dataTest <- merge(dataTest, TargetLevels, by.x = eval(TargetColumnName.), by.y = "OriginalLevels", all = FALSE)
      dataTest[, paste0(TargetColumnName.) := NewLevels]
      dataTest[, NewLevels := NULL]
      if(!is.null(TestData.)) {
        TestData. <- merge(TestData., TargetLevels, by.x = eval(TargetColumnName.), by.y = "OriginalLevels", all = FALSE)
        TestData.[, paste0(TargetColumnName.) := NewLevels]
        TestData.[, NewLevels := NULL]
      }
    }

    # MultiClass Update Colnames
    if(is.numeric(FeatureColNames.)) {
      Names <- data.table::as.data.table(names(data.)[FeatureColNames.])
      data.table::setnames(Names, "V1", "ColNames")
    } else {
      Names <- data.table::as.data.table(FeatureColNames.)
      if(!"V1" %chin% names(Names)) {
        data.table::setnames(Names, "FeatureColNames.", "ColNames")
      } else {
        data.table::setnames(Names, "V1", "ColNames")
      }
    }

    # Save column names
    if(SaveModelObjects.) data.table::fwrite(Names, file = file.path(model_path., paste0(ModelID., "_ColNames.csv")))

    # MultiClass Subset Target Variables----
    TrainTarget <- dataTrain[, get(TargetColumnName.)]
    if(!is.null(dataTest)) TestTarget <- dataTest[, get(TargetColumnName.)]
    if(!is.null(TestData.)) FinalTestTarget <- TestData.[, get(TargetColumnName.)]

    # MultiClass Remove Target Variable from Feature Data
    dataTrain[, eval(TargetColumnName.) := NULL]
    if(!is.null(dataTest)) dataTest[, eval(TargetColumnName.) := NULL]
    if(!is.null(TestData.)) TestData.[, eval(TargetColumnName.) := NULL]

    # MultiClass Initialize XGBoost Data Conversion ----
    datatrain <- xgboost::xgb.DMatrix(as.matrix(dataTrain), label = TrainTarget)
    if(!is.null(dataTest)) {
      datavalidate <- xgboost::xgb.DMatrix(as.matrix(dataTest), label = TestTarget)
      if(!is.null(TestData.)) {
        datatest <- xgboost::xgb.DMatrix(as.matrix(TestData.), label = FinalTestTarget)
        EvalSets <- list(train = datavalidate, test = datatest)
      } else {
        EvalSets <- list(train = datatrain, test = datavalidate)
      }
    } else {
      EvalSets <- list(train = datatrain)
    }
  }

  # Return objects
  return(list(
    datatrain = datatrain,
    datavalidate = if(exists("datavalidate")) datavalidate else NULL,
    datatest = if(exists("datatest")) datatest else NULL,
    EvalSets = EvalSets,
    dataTrain = dataTrain,
    dataTest = if(exists("dataTest")) dataTest else NULL,
    TrainMerge = if(exists("TrainMerge")) TrainMerge else NULL,
    ValidMerge = if(exists("ValidMerge")) ValidMerge else NULL,
    TestMerge = if(exists("TestMerge")) TestMerge else NULL,
    TestData = if(exists("TestData.")) TestData. else NULL,
    TrainTarget = TrainTarget,
    TestTarget = if(exists("TestTarget")) TestTarget else NULL,
    FinalTestTarget = if(exists("FinalTestTarget")) FinalTestTarget else NULL,
    TargetLevels = if(exists("TargetLevels")) TargetLevels else NULL,
    Names = Names,
    FactorLevelsList = if(exists("FactorLevelsList")) FactorLevelsList else NULL,
    IDcols = unique(IDcols.),
    TransformNumericColumns = TransformNumericColumns.,
    TransformationResults = if(exists("TransformationResults")) TransformationResults else NULL,
    NumLevels = if(exists("NumLevels")) NumLevels else NULL))
}

#' @title XGBoostFinalParams
#'
#' @description Parameters for xgboost fitting
#'
#' @author Adrian Antico
#' @family XGBoost Helpers
#'
#' @param LossFunction. Passthrough
#' @param eval_metric. Passthrough
#' @param NThreads Passthrough
#' @param TreeMethod. Passthrough
#' @param PassInGrid. Passthrough
#' @param BestGrid. Passthrough
#' @param Trees. Passthrough
#' @param NumLevels. Passthrough
#'
#' @noRd
XGBoostFinalParams <- function(GridTune.=GridTune,
                               PassInGrid.=PassInGrid,
                               TrainOnFull.=TrainOnFull,
                               LossFunction.=LossFunction,
                               eval_metric.=eval_metric,
                               NThreads.=NThreads,
                               TreeMethod.=TreeMethod,
                               BestGrid.=BestGrid,
                               Trees.=Trees,
                               NumLevels. = NumLevels) {

  # Parameter list
  base_params <- list()
  base_params$booster <- "gbtree"
  base_params$objective <- LossFunction.
  base_params$eval_metric <- tolower(eval_metric.)
  base_params$nthread <- NThreads.
  base_params$max_bin <- 64L
  base_params$early_stopping_rounds <- 10L
  base_params$tree_method <- TreeMethod.

  # Grid tuning
  if(!is.null(PassInGrid.)) {
    if(PassInGrid.[,.N] > 1L) stop("PassInGrid needs to be a single row data.table")
    if(PassInGrid.[, BanditProbs_Grid_1] == -10) {
      PassInGrid. <- NULL
    } else {
      base_params$max_depth <- PassInGrid.$Depth
      base_params$eta <- PassInGrid.$LearningRate
      base_params$subsample <- PassInGrid.$SubSample
      base_params$colsample_bytree <- PassInGrid.$ColSampleByTree
      Trees. <- PassInGrid.$NTrees
    }
  }

  # Define parameters for case where you want to run grid tuning
  if(GridTune. && !TrainOnFull. && !BestGrid.[["RunTime"]] != -1L) {
    base_params$max_depth <- BestGrid.$Depth
    base_params$eta <- BestGrid.$LearningRate
    base_params$subsample <- BestGrid.$SubSample
    base_params$colsample_bytree <- BestGrid.$ColSampleByTree
    Trees. <- BestGrid.$NTrees
  }

  # Return base_params
  return(list(base_params = base_params, NTrees = Trees.))
}

#' @title XGBoostParameterGrids
#'
#' @author Adrian Antico
#' @family SXGBoost Helpers
#'
#' @param TaskType "GPU" or "CPU"
#' @param Shuffles The number of shuffles you want to apply to each grid
#' @param NTrees seq(500L, 5000L, 500L)
#' @param Depth seq(4L, 16L, 2L)
#' @param LearningRate seq(0.05,0.40,0.05)
#' @param MinChildWeight seq(1.0, 10.0, 1.0)
#' @param SubSample seq(0.55, 1.0, 0.05)
#' @param ColSampleByTree seq(0.55, 1.0, 0.05)
#' @return A list containing data.table's with the parameters shuffled and ready to test in the bandit framework
#' @noRd
XGBoostParameterGrids <- function(TaskType = "CPU",
                                  Shuffles = 1L,
                                  NTrees = seq(500L, 5000L, 500L),
                                  Depth = seq(4L, 16L, 2L),
                                  LearningRate = seq(0.05,0.40,0.05),
                                  MinChildWeight = seq(1.0, 10.0, 1.0),
                                  SubSample = seq(0.55, 1.0, 0.05),
                                  ColSampleByTree = seq(0.55, 1.0, 0.05)) {

  # Create grid sets----
  Grid <- data.table::CJ(

    # Basis for creating parsimonous buckets----
    NTrees = if(!is.null(NTrees)) sort(NTrees, decreasing = FALSE) else seq(500L, 5000L, 500L),
    Depth = if(!is.null(Depth)) sort(Depth, decreasing = FALSE) else seq(4L, 16L, 2L),
    LearningRate = if(!is.null(LearningRate)) sort(LearningRate, decreasing = FALSE) else seq(0.01,0.10,0.01),

    # Random hyperparameters----
    MinChildWeight = if(!is.null(MinChildWeight)) MinChildWeight else seq(1.0, 10.0, 1.0),
    SubSample = if(!is.null(SubSample)) SubSample else seq(0.55, 1.0, 0.05),
    ColSampleByTree = if(!is.null(ColSampleByTree)) ColSampleByTree else seq(0.55, 1.0, 0.05))

  # Total loops----
  N_NTrees <- length(unique(Grid[["NTrees"]]))
  N_Depth <- length(unique(Grid[["Depth"]]))
  N_LearningRate <- length(unique(Grid[["LearningRate"]]))
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
    EvalMetric = rep(-1,10000L),
    TreesBuilt = rep(-1,10000L),
    NTrees = rep(-1,10000L),
    Depth = rep(-1,10000L),
    LearningRate = rep(-1,10000L),
    MinChildWeight = rep(-1,10000L),
    SubSample = rep(-1,10000L),
    ColSampleByTree = rep("aa", 10000L))

  # Shuffle grid sets----
  for(shuffle in seq_len(Shuffles)) for(i in seq_len(Runs)) Grids[[paste0("Grid_",i)]] <- Grids[[paste0("Grid_",i)]][order(runif(Grids[[paste0("Grid_",i)]][,.N]))]

  # Return grid----
  return(list(Grid = Grid, Grids = Grids, ExperimentalGrid = eGrid))
}

#' @title XGBoostGridParams
#'
#' @author Adrian Antico
#' @family XGBoost Helpers
#'
#' @param N. Passthrough
#' @param counter. Passthrough
#' @param Objective. Passthrough
#' @param NThreads. = -1L,
#' @param BanditArmsN. Passthrough
#' @param EvalMetric. Passthrough
#' @param TreeMethod. Passthrough
#' @param model_path. Passthrough
#' @param NewGrid. Passthrough
#' @param Grid. Passthrough
#' @param GridClusters. Passthrough
#' @noRd
XGBoostGridParams <- function(N. = N,
                              counter. = NULL,
                              NThreads. = -1L,
                              Objective. = 'reg:logistic',
                              BanditArmsN. = NULL,
                              EvalMetric. = NULL,
                              TreeMethod. = NULL,
                              model_path. = NULL,
                              NewGrid. = NULL,
                              Grid. = NULL,
                              GridClusters. = NULL) {

  # Create base_params (independent of runs)
  base_params <- list()
  base_params$booster <- "gbtree"
  base_params$objective <- Objective.
  base_params$eval_metric <- tolower(EvalMetric.)
  base_params$nthread <- NThreads.
  base_params$max_bin <- 64L
  base_params$early_stopping_rounds <- 10L
  base_params$tree_method <- TreeMethod.

  # Run-dependent args and updates
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$max_depth <- GridClusters.[[paste0("Grid_", counter.-1L)]][["Depth"]][1L] else if(counter. != 1) base_params$max_depth <- GridClusters.[[paste0("Grid_",NewGrid.)]][["Depth"]][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$eta <- GridClusters.[[paste0("Grid_", counter.-1L)]][["LearningRate"]][1L] else if(counter. != 1L) base_params$eta <- GridClusters.[[paste0("Grid_",NewGrid.)]][["LearningRate"]][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$subsample <- GridClusters.[[paste0("Grid_",counter.-1L)]][["SubSample"]][1L] else if(counter. != 1L) base_params$subsample <- GridClusters.[[paste0("Grid_",NewGrid.)]][["SubSample"]][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$colsample_bytree <- GridClusters.[[paste0("Grid_",counter.-1L)]][["ColSampleByTree"]][1L] else if(counter. != 1L) base_params$colsample_bytree <- GridClusters.[[paste0("Grid_",NewGrid.)]][["ColSampleByTree"]][N.]

  # Return
  return(base_params)
}

#' @title XGBoostValidation
#'
#' @description Generate validation, importance, and shap data
#'
#' @family XGBoost Helpers
#' @author Adrian Antico
#'
#' @param ModelType. Passthrough
#' @param TrainOnFull. Passthrough
#' @param model. Passthrough
#' @param TestData. Passthrough
#' @param TestTarget. Passthrough
#' @param FinalTestTarget. Passthrough
#' @param TestMerge. Passthrough
#' @param dataTest. Passthrough
#' @param TrainTarget. Passthrough
#' @param dataTrain. Passthrough
#' @param predict. Passthrough
#' @param TargetColumnName. Passthrough
#' @param SaveModelObjects. Passthrough
#' @param metadata_path. Passthrough
#' @param model_path. Passthrough
#' @param ModelID. Passthrough
#' @param Final. Passthrough
#' @param TransformNumericColumns. Regression Passthrough
#' @param TransformationResults. Regression Passthrough
#' @param GridTune. Regression Passthrough
#' @param data. Regression Passthrough
#' @param LossFunction. MultiClass Passthrough
#'
#' @noRd
XGBoostValidation <- function(ModelType. = "classifier",
                              TrainOnFull. = TrainOnFull,
                              model. = model,
                              TargetColumnName. = TargetColumnName,
                              SaveModelObjects. = SaveModelObjects,
                              metadata_path. = metadata_path,
                              model_path. = model_path,
                              ModelID. = ModelID,
                              TestData. = NULL,
                              TestTarget. = NULL,
                              FinalTestTarget. = NULL,
                              TestMerge. = NULL,
                              dataTest. = NULL,
                              TrainTarget. = NULL,
                              dataTrain. = NULL,
                              Final. = NULL,
                              predict. = NULL,
                              LossFunction. = NULL,
                              TransformNumericColumns. = NULL,
                              TransformationResults. = NULL,
                              GridTune. = GridTune,
                              data. = NULL) {

  if(ModelType. %chin% c("regression","classifier")) {

    # Validation and Shaps
    if(!is.null(TestData.)) {
      ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., TestMerge., p1 = predict.))
      ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(TestData.), model = model., features = names(TestData.))$shap_contrib)
    } else if(!TrainOnFull. || (!is.null(TestTarget.) && !is.null(dataTest.))) {
      ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., dataTest., p1 = predict.))
      ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(dataTest.), model = model., features = names(dataTest.))$shap_contrib)
    } else {
      ValidationData <- data.table::as.data.table(cbind(Target = TrainTarget., dataTrain., p1 = predict.))
      ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(dataTrain.), model = model., features = names(dataTrain.))$shap_contrib)
    }

    # Update names Validation Data
    if(ModelType. == "regression") {
      data.table::setnames(ValidationData, c("Target","p1"), c(TargetColumnName., "Predict"))
    } else {
      data.table::setnames(ValidationData, "Target", TargetColumnName.)
    }

    # Transformation
    if(ModelType. == "regression" && !is.null(TransformNumericColumns.)) {

      # Update
      if(GridTune.) TransformationResults. <- TransformationResults.[ColumnName != "Predict"]

      # Combine transform metadata
      TransformationResults. <- data.table::rbindlist(list(
        TransformationResults.,
        data.table::data.table(
          ColumnName = "Predict",
          MethodName = rep(TransformationResults.[ColumnName == eval(TargetColumnName.), MethodName], 1L),
          Lambda = rep(TransformationResults.[ColumnName == eval(TargetColumnName.), Lambda], 1L),
          NormalizedStatistics = rep(0, 1))))

      # Model output
      if(length(unique(TransformationResults.[["ColumnName"]])) != nrow(TransformationResults.)) {
        temp <- TransformationResults.[, .N, by = "ColumnName"][N != 1L][[1L]]
        temp1 <- which(names(ValidationData) == temp)[1L]
        ValidationData[, eval(names(data.)[temp1]) := NULL]
        TransformationResults. <- TransformationResults.[, ID := 1L:.N][ID != which(TransformationResults.[["ID"]] == temp1)][, ID := NULL]
      }

      # Transform Target and Predicted Value ----
      ValidationData <- AutoTransformationScore(
        ScoringData = ValidationData,
        Type = "Inverse",
        FinalResults = TransformationResults.,
        TransID = NULL,
        Path = NULL)
    }

  } else if(ModelType. == "multiclass") {
    if(LossFunction. == "multi:softprob") {
      if(!is.null(TestData.)) {
        ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., TestMerge., Final.))
        ShapValues <- xgboost:::xgb.shap.data(as.matrix(TestData.), model = model., features = names(TestData.))$shap_contrib
      } else if(!TrainOnFull. || (!is.null(TestTarget.) && !is.null(dataTest.))) {
        ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., dataTest., Final.))
        ShapValues <- xgboost:::xgb.shap.data(as.matrix(dataTest.), model = model., features = names(dataTest.))$shap_contrib
      } else {
        ValidationData <- data.table::as.data.table(cbind(Target = TrainTarget., dataTrain., Final.))
        ShapValues <- xgboost:::xgb.shap.data(as.matrix(dataTrain.), model = model., features = names(dataTrain.))$shap_contrib
      }
    } else {
      if(!is.null(TestData.)) {
        ValidationData <- data.table::as.data.table(cbind(Target = FinalTestTarget., TestMerge., p1 = predict.))
        ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(TestData.), model = model., features = names(TestData.))$shap_contrib)
      } else if(!TrainOnFull.) {
        ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., dataTest., p1 = predict.))
        ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(dataTest.), model = model., features = names(dataTest.))$shap_contrib)
      } else {
        ValidationData <- data.table::as.data.table(cbind(Target = TrainTarget., dataTrain., p1 = predict.))
        ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(dataTrain.), model = model., features = names(dataTrain.))$shap_contrib)
      }
    }
  }

  # Shap Values
  data.table::setnames(ShapValues, old = names(ShapValues), new = paste0("Shap_", names(ShapValues)))
  ShapValues <- cbind(ValidationData, ShapValues)

  # Variable Importance ----
  VariableImportance <- tryCatch({data.table::as.data.table(xgboost::xgb.importance(model = model.))}, error = function(x) NULL)
  if(!is.null(VariableImportance)) {
    VariableImportance[, ':=' (Gain = round(Gain, 4L), Cover = round(Cover, 4L), Frequency = round(Frequency, 4L))]
    if(SaveModelObjects.) {
      if(!is.null(metadata_path.)) {
        data.table::fwrite(VariableImportance, file = file.path(metadata_path., paste0(ModelID., "_VariableImportance.csv")))
      } else {
        data.table::fwrite(VariableImportance, file = file.path(model_path., paste0(ModelID., "_VariableImportance.csv")))
      }
    }
  }

  # Return
  return(list(
    ValidationData = ValidationData,
    VariableImportance = VariableImportance,
    ShapValues = ShapValues,
    TransformationResults = if(exists("TransformationResults.")) TransformationResults. else NULL))
}

#' @title XGBoostRegressionMetrics
#'
#' @author Adrian Antico
#' @family XGBoost Helpers
#'
#' @param grid_eval_metric Passthrough
#' @param MinVal = -1L,
#' @param calibEval Passthrough
#' @noRd
XGBoostRegressionMetrics <- function(grid_eval_metric,
                                     MinVal,
                                     calibEval) {
  if(tolower(grid_eval_metric) == "poisson") {
    if(MinVal > 0L && min(calibEval[["p1"]], na.rm = TRUE) > 0L) {
      calibEval[, Metric := p1 - Target * log(p1 + 1)]
      Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
    }
  } else if(tolower(grid_eval_metric) == "mae") {
    calibEval[, Metric := abs(Target - p1)]
    Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
  } else if(tolower(grid_eval_metric) == "mape") {
    calibEval[, Metric := abs((Target - p1) / (Target + 1))]
    Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
  } else if(tolower(grid_eval_metric) == "mse") {
    calibEval[, Metric := (Target - p1) ^ 2L]
    Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
  } else if(tolower(grid_eval_metric) == "msle") {
    if(MinVal > 0L && min(calibEval[["p1"]], na.rm = TRUE) > 0L) {
      calibEval[, Metric := (log(Target + 1) - log(p1 + 1)) ^ 2L]
      Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
    }
  } else if(tolower(grid_eval_metric) == "kl") {
    if(MinVal > 0L && min(calibEval[["p1"]], na.rm = TRUE) > 0L) {
      calibEval[, Metric := Target * log((Target + 1) / (p1 + 1))]
      Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
    }
  } else if(tolower(grid_eval_metric) == "cs") {
    calibEval[, ':=' (Metric1 = Target * p1, Metric2 = Target ^ 2L, Metric3 = p1 ^ 2L)]
    Metric <- calibEval[, sum(Metric1, na.rm = TRUE)] / (sqrt(calibEval[, sum(Metric2, na.rm = TRUE)]) * sqrt(calibEval[, sum(Metric3, na.rm = TRUE)]))
  } else if(tolower(grid_eval_metric) == "r2") {
    Metric <- (calibEval[, stats::cor(eval(Target), p1)][[1L]]) ^ 2L
  }
  return(Metric)
}
