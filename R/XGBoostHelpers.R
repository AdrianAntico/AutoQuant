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

  # Ensure model_path. and metadata_path. exists
  if(!is.null(model_path.) && !dir.exists(file.path(model_path.))) dir.create(model_path.)
  if(!is.null(metadata_path.) && !is.null(metadata_path.)) if(!dir.exists(file.path(metadata_path.))) dir.create(metadata_path.)

  # Ensure only one value if not grid tuning
  if(!GridTune. && length(Trees.) > 1) stop('Trees cannot have more than one value supplied')
  if(!GridTune. && length(max_depth.) > 1) stop('Depth cannot have more than one value supplied')
  if(!GridTune. && length(eta.) > 1) stop('LearningRate cannot have more than one value supplied')
  if(!GridTune. && length(min_child_weight.) > 1) stop('L2_Leaf_Reg cannot have more than one value supplied')
  if(!GridTune. && length(subsample.) > 1) stop('L2_Leaf_Reg cannot have more than one value supplied')
  if(!GridTune. && length(colsample_bytree.) > 1) stop('L2_Leaf_Reg cannot have more than one value supplied')
}

#' @title XGBoostDataPrep
#'
#' @description Prepare data for XGBoost modeling
#'
#' @family XGBoost Helpers
#' @author Adrian Antico
#'
#' @param Algo 'xgboost', 'lightgbm'
#' @param OutputSelection. Passthrough
#' @param ModelType 'regression', 'classification', or 'multiclass'
#' @param data. Passthrough
#' @param ValidationData. Passthrough
#' @param TestData. Passthrough
#' @param TargetColumnName. Passthrough
#' @param FeatureColNames. Passthrough
#' @param PrimaryDateColumn. Passthrough
#' @param WeightsColumnName. Passthrough
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
XGBoostDataPrep <- function(Algo = 'xgboost',
                            OutputSelection. = NULL,
                            ModelType = 'regression',
                            data. = data,
                            ValidationData. = ValidationData,
                            TestData. = TestData,
                            TargetColumnName. = TargetColumnName,
                            FeatureColNames. = FeatureColNames,
                            WeightsColumnName. = NULL,
                            IDcols. = NULL,
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

  # Remove WeightsVector
  if(!is.null(WeightsColumnName.)) {
    if(Algo == 'xgboost') {
      WeightsVector <- data.[[eval(WeightsColumnName.)]]
      data.table::set(data., j = WeightsColumnName., value = NULL)
      if(!is.null(ValidationData.) && WeightsColumnName. %chin% names(ValidationData.)) data.table::set(ValidationData., j = WeightsColumnName., value = NULL)
      if(!is.null(TestData.) && WeightsColumnName. %chin% names(TestData.)) data.table::set(TestData., j = WeightsColumnName., value = NULL)
      FeatureColNames. <- FeatureColNames.[!FeatureColNames. %chin% WeightsColumnName.]
    } else {
      if(!WeightsColumnName. %chin% FeatureColNames.) FeatureColNames. <- c(FeatureColNames., WeightsColumnName.)
    }
  }

  # Target var management
  if(ModelType == 'multiclass') CatFeatures <- setdiff(CatFeatures, TargetColumnName.)

  # Classification
  if(ModelType == 'classification') {

    # Data Partition
    if(is.null(ValidationData.) && is.null(TestData.) && !TrainOnFull.) {
      dataSets <- AutoDataPartition(
        data = data.,
        NumDataSets = 3L,
        Ratios = c(0.70, 0.20, 0.10),
        PartitionType = 'random',
        StratifyColumnNames = TargetColumnName.,
        TimeColumnName = NULL)
      data. <- dataSets$TrainData
      ValidationData. <- dataSets$ValidationData
      TestData. <- dataSets$TestData
    }

    # Classification data. Subset Columns Needed
    if(!is.null(ValidationData.)) {
      TrainMerge <- data.table::rbindlist(list(data.,ValidationData.))
      dataTrain <- data.[, .SD, .SDcols = c(FeatureColNames., TargetColumnName.)]
      dataTest <- ValidationData.[, .SD, .SDcols = c(FeatureColNames., TargetColumnName.)]
      if(!is.null(TestData.)) {
        TestMerge <- data.table::copy(TestData.)
        TestData. <- TestData.[, .SD, .SDcols = c(FeatureColNames., TargetColumnName.)]
      }
    } else {
      TrainMerge <- data.table::copy(data.)
      dataTrain <- data.[, .SD, .SDcols = c(FeatureColNames., TargetColumnName.)]
      dataTest <- NULL
      TestData. <- NULL
      TestMerge <- NULL
    }

    # Save Names of data
    Names <- data.table::as.data.table(names(dataTrain))
    if(!'V1' %chin% names(Names)) {
      data.table::setnames(Names, 'FeatureColNames.', 'ColNames')
    } else {
      data.table::setnames(Names, 'V1', 'ColNames')
    }

    # Dummify dataTrain Categorical Features ----
    Output <- RemixAutoML:::EncodeCharacterVariables(RunMode='train', ModelType=ModelType, TrainData=dataTrain, ValidationData=dataTest, TestData=TestData., TargetVariableName=TargetColumnName., CategoricalVariableNames=CatFeatures, EncodeMethod=EncodingMethod., KeepCategoricalVariables=FALSE, ReturnMetaData=TRUE, MetaDataPath=model_path., MetaDataList=NULL, ImputeMissingValue=0)
    dataTrain <- Output$TrainData; Output$TrainData <- NULL
    dataTest <- Output$ValidationData; Output$ValidationData <- NULL
    TestData. <- Output$TestData; Output$TestData. <- NULL
    FactorLevelsList <- Output$MetaData; rm(Output)

    # Subset TargetColumnName. Variables----
    TrainTarget <- dataTrain[, get(TargetColumnName.)]
    if(!TrainOnFull.) TestTarget <- dataTest[, get(TargetColumnName.)]
    if(!is.null(TestData.)) FinalTestTarget <- TestData.[, get(TargetColumnName.)]

    # Remove TargetColumnName. Variable from Feature Data
    data.table::set(dataTrain, j = TargetColumnName., value = NULL)
    if(!TrainOnFull.) data.table::set(dataTest, j = TargetColumnName., value = NULL)
    if(!is.null(TestData.)) data.table::set(TestData., j = TargetColumnName., value = NULL)

    # Save feature names
    Names <- Names[ColNames != eval(TargetColumnName.)]
    if(SaveModelObjects.) data.table::fwrite(Names, file = file.path(model_path., paste0(ModelID., '_ColNames.csv')))
  }

  # Regression
  if(ModelType == 'regression') {

    # Transform data., ValidationData., and TestData.
    if((TrainOnFull. || !is.null(ValidationData.)) && !is.null(TransformNumericColumns.)) {
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
      if(!is.null(ValidationData.)) {
        ValidationData. <- AutoTransformationScore(
          ScoringData = ValidationData.,
          Type = 'Apply',
          FinalResults = TransformationResults,
          TransID = NULL,
          Path = NULL)
      }

      # Transform TestData.
      if(!is.null(TestData.)) {
        TestData. <- AutoTransformationScore(
          ScoringData = TestData.,
          Type = 'Apply',
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
          PartitionType = 'random',
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
          Type = 'Apply',
          FinalResults = TransformationResults,
          TransID = NULL,
          Path = NULL)

        # Transform TestData.
        if(!is.null(TestData.)) {
          TestData. <- AutoTransformationScore(
            ScoringData = TestData.,
            Type = 'Apply',
            FinalResults = TransformationResults,
            TransID = NULL,
            Path = NULL)
        }
      } else {
        dataSets <- AutoDataPartition(
          data.,
          NumDataSets = 3L,
          Ratios = c(0.70, 0.20, 0.10),
          PartitionType = 'random',
          StratifyColumnNames = NULL,
          TimeColumnName = NULL)
        data. <- dataSets$TrainData
        ValidationData. <- dataSets$ValidationData
        TestData. <- dataSets$TestData
        MeanTrainTarget <- data.[, mean(get(TargetColumnName.))]
      }
    }

    # Regression data. Subset Columns Needed
    if(!is.null(ValidationData.)) {
      TrainMerge <- data.table::rbindlist(list(data.,ValidationData.))
      dataTrain <- data.[, .SD, .SDcols = c(FeatureColNames., TargetColumnName.)]
      dataTest <- ValidationData.[, .SD, .SDcols = c(FeatureColNames., TargetColumnName.)]
      if(!is.null(TestData.)) {
        TestMerge <- data.table::copy(TestData.)
        TestData. <- TestData.[, .SD, .SDcols = c(FeatureColNames., TargetColumnName.)]
      }
    } else {
      TrainMerge <- data.table::copy(data.)
      dataTrain <- data.[, .SD, .SDcols = c(FeatureColNames., TargetColumnName.)]
      dataTest <- NULL
      TestData. <- NULL
      TestMerge <- NULL
    }

    # Save Names of data
    Names <- data.table::as.data.table(names(dataTrain))
    if(!'V1' %chin% names(Names)) {
      data.table::setnames(Names, 'FeatureColNames.', 'ColNames')
    } else {
      data.table::setnames(Names, 'V1', 'ColNames')
    }

    # Dummify dataTrain Categorical Features ----
    if(!is.null(CatFeatures)) {
      Output <- EncodeCharacterVariables(RunMode='train', ModelType=ModelType, TrainData=dataTrain, ValidationData=dataTest, TestData=TestData., TargetVariableName=TargetColumnName., CategoricalVariableNames=CatFeatures, EncodeMethod=EncodingMethod., KeepCategoricalVariables=FALSE, ReturnMetaData=TRUE, MetaDataPath=model_path., MetaDataList=NULL, ImputeMissingValue=0)
      dataTrain <- Output$TrainData; Output$TrainData <- NULL
      dataTest <- Output$ValidationData; Output$ValidationData <- NULL
      TestData. <- Output$TestData; Output$TestData. <- NULL
      FactorLevelsList <- Output$MetaData; rm(Output)
    } else {
      FactorLevelsList <- NULL
    }

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

    # Save feature names
    if(Algo == "xgboost") {
      Names <- Names[!ColNames %chin% c(eval(TargetColumnName.), eval(IDcols.), eval(WeightsColumnName.))]
    } else if(Algo == "lightgbm") {
      Names <- Names[!ColNames %chin% c(eval(TargetColumnName.), eval(IDcols.))]
    }
    if(SaveModelObjects.) data.table::fwrite(Names, file = file.path(model_path., paste0(ModelID., '_ColNames.csv')))
  }

  # MultiClass
  if(ModelType == 'multiclass') {

    # MultiClass Data Partition
    if(is.null(ValidationData.) && is.null(TestData.) && !TrainOnFull.) {
      dataSets <- AutoDataPartition(
        data = data.,
        NumDataSets = 3L,
        Ratios = c(0.70, 0.20, 0.10),
        PartitionType = 'random',
        StratifyColumnNames = TargetColumnName.,
        TimeColumnName = NULL)
      data. <- dataSets$TrainData
      ValidationData. <- dataSets$ValidationData
      TestData. <- dataSets$TestData
    }

    # Regression data. Subset Columns Needed
    if(!is.null(ValidationData.)) {
      TrainMerge <- data.table::rbindlist(list(data.,ValidationData.))
      dataTrain <- data.[, .SD, .SDcols = c(FeatureColNames., TargetColumnName.)]
      dataTest <- ValidationData.[, .SD, .SDcols = c(FeatureColNames., TargetColumnName.)]
      if(!is.null(TestData.)) {
        TestMerge <- data.table::copy(TestData.)
        TestData. <- TestData.[, .SD, .SDcols = c(FeatureColNames., TargetColumnName.)]
      }
    } else {
      TrainMerge <- data.table::copy(data.)
      dataTrain <- data.[, .SD, .SDcols = c(FeatureColNames., TargetColumnName.)]
      dataTest <- NULL
      TestData. <- NULL
      TestMerge <- NULL
    }

    # Save Names of data
    Names <- data.table::as.data.table(names(dataTrain))
    if(!'V1' %chin% names(Names)) {
      data.table::setnames(Names, 'FeatureColNames.', 'ColNames')
    } else {
      data.table::setnames(Names, 'V1', 'ColNames')
    }

    # Dummify dataTrain Categorical Features ----
    Output <- EncodeCharacterVariables(RunMode='train', ModelType=ModelType, TrainData=dataTrain, ValidationData=dataTest, TestData=TestData., TargetVariableName=TargetColumnName., CategoricalVariableNames=CatFeatures, EncodeMethod=EncodingMethod., KeepCategoricalVariables=FALSE, ReturnMetaData=TRUE, MetaDataPath=model_path., MetaDataList=NULL, ImputeMissingValue=0)
    dataTrain <- Output$TrainData; Output$TrainData <- NULL
    dataTest <- Output$ValidationData; Output$ValidationData <- NULL
    TestData. <- Output$TestData; Output$TestData. <- NULL
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
    data.table::setnames(TargetLevels, 'V1', 'OriginalLevels')
    TargetLevels[, NewLevels := 0L:(.N - 1L)]
    if(SaveModelObjects.) data.table::fwrite(TargetLevels, file = file.path(model_path., paste0(ModelID., '_TargetLevels.csv')))

    # Number of levels
    NumLevels <- TargetLevels[, .N]

    # MultiClass Convert Target to Numeric Factor
    dataTrain <- merge(dataTrain, TargetLevels, by.x = eval(TargetColumnName.), by.y = 'OriginalLevels', all = FALSE)
    dataTrain[, paste0(TargetColumnName.) := NewLevels]
    dataTrain[, NewLevels := NULL]
    if(!is.null(dataTest)) {
      dataTest <- merge(dataTest, TargetLevels, by.x = eval(TargetColumnName.), by.y = 'OriginalLevels', all = FALSE)
      dataTest[, paste0(TargetColumnName.) := NewLevels]
      dataTest[, NewLevels := NULL]
      if(!is.null(TestData.)) {
        TestData. <- merge(TestData., TargetLevels, by.x = eval(TargetColumnName.), by.y = 'OriginalLevels', all = FALSE)
        TestData.[, paste0(TargetColumnName.) := NewLevels]
        TestData.[, NewLevels := NULL]
      }
    }

    # MultiClass Subset Target Variables----
    TrainTarget <- dataTrain[, get(TargetColumnName.)]
    if(!is.null(dataTest)) TestTarget <- dataTest[, get(TargetColumnName.)]
    if(!is.null(TestData.)) FinalTestTarget <- TestData.[, get(TargetColumnName.)]

    # MultiClass Remove Target Variable from Feature Data
    dataTrain[, eval(TargetColumnName.) := NULL]
    if(!is.null(dataTest)) dataTest[, eval(TargetColumnName.) := NULL]
    if(!is.null(TestData.)) TestData.[, eval(TargetColumnName.) := NULL]

    # Save feature names
    Names <- Names[ColNames != eval(TargetColumnName.)]
    if(SaveModelObjects.) data.table::fwrite(Names, file = file.path(model_path., paste0(ModelID., '_ColNames.csv')))
  }

  # Convert data to model object data
  if('GroupVar' %chin% names(dataTrain)) data.table::set(dataTrain, j = 'GroupVar', value = NULL)
  if(tolower(Algo) == 'xgboost') {
    datatrain <- xgboost::xgb.DMatrix(as.matrix(dataTrain), label = TrainTarget)
  } else if(tolower(Algo) == 'lightgbm') {
    datatrain <- lightgbm::lgb.Dataset(data=as.matrix(dataTrain), label=TrainTarget)
  }
  if(!TrainOnFull.) {
    if('GroupVar' %chin% names(dataTest)) data.table::set(dataTest, j = 'GroupVar', value = NULL)
    if(tolower(Algo) == 'xgboost') {
      datavalidate <- xgboost::xgb.DMatrix(as.matrix(dataTest), label = TestTarget)
    } else if(tolower(Algo) == 'lightgbm') {
      datavalidate <- lightgbm::lgb.Dataset(data=as.matrix(dataTest), label=TestTarget)
    }
    if(!is.null(TestData.)) {
      if('GroupVar' %chin% names(TestData.)) data.table::set(TestData., j = 'GroupVar', value = NULL)
      if(tolower(Algo) == 'xgboost') {
        datatest <- xgboost::xgb.DMatrix(as.matrix(TestData.), label = FinalTestTarget)
        EvalSets <- list(train = datavalidate, test = datatest)
      } else if(tolower(Algo) == 'lightgbm') {
        datatest <- lightgbm::lgb.Dataset(data=as.matrix(TestData.), label=FinalTestTarget)
        EvalSets <- list(ValidationData = datavalidate, TestData = datatest)
      }
    } else {
      if(tolower(Algo) == 'xgboost') {
        EvalSets <- list(train = datatrain, test = datavalidate)
      } else if(tolower(Algo) == 'lightgbm') {
        EvalSets <- list(ValidationData = datavalidate)
      }
    }
  } else {
    EvalSets <- list(train = datatrain)
  }

  # Return objects
  return(list(
    WeightsVector = if(exists('WeightsVector')) WeightsVector else NULL,
    datatrain = datatrain,
    datavalidate = if(exists('datavalidate')) datavalidate else NULL,
    datatest = if(exists('datatest')) datatest else NULL,
    EvalSets = EvalSets,
    dataTrain = dataTrain,
    dataTest = if(exists('dataTest')) dataTest else NULL,
    TrainMerge = if(exists('TrainMerge')) TrainMerge else NULL,
    TestMerge = if(exists('TestMerge')) TestMerge else NULL,
    TestData = if(exists('TestData.')) TestData. else NULL,
    TrainTarget = TrainTarget,
    TestTarget = if(exists('TestTarget')) TestTarget else NULL,
    FinalTestTarget = if(exists('FinalTestTarget')) FinalTestTarget else NULL,
    TargetLevels = if(exists('TargetLevels')) TargetLevels else NULL,
    Names = Names,
    FactorLevelsList = if(exists('FactorLevelsList')) FactorLevelsList else NULL,
    IDcols = unique(IDcols.),
    TransformNumericColumns = TransformNumericColumns.,
    TransformationResults = if(exists('TransformationResults')) TransformationResults else NULL,
    NumLevels = if(exists('NumLevels')) NumLevels else NULL))
}

#' @title XGBoostFinalParams
#'
#' @description Parameters for xgboost fitting
#'
#' @author Adrian Antico
#' @family XGBoost Helpers
#'
#' @param GridTune. Passthrough
#' @param TrainOnFull. Passthrough
#' @param LossFunction. Passthrough
#' @param eval_metric. Passthrough
#' @param NThreads. Passthrough
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
  base_params$booster <- 'gbtree'
  base_params$objective <- LossFunction.
  base_params$eval_metric <- tolower(eval_metric.)
  base_params$nthread <- NThreads.
  base_params$max_bin <- 64L
  base_params$early_stopping_rounds <- 10L
  base_params$tree_method <- TreeMethod.

  # Grid tuning
  if(!is.null(PassInGrid.)) {
    if(PassInGrid.[,.N] > 1L) stop('PassInGrid needs to be a single row data.table')
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
  if(GridTune. && !TrainOnFull. && BestGrid.[['RunNumber']] != 1L) {
    base_params$max_depth <- BestGrid.$Depth
    base_params$eta <- BestGrid.$LearningRate
    base_params$subsample <- BestGrid.$SubSample
    base_params$colsample_bytree <- BestGrid.$ColSampleByTree
    Trees. <- BestGrid.$NTrees
  } else {
    for(z in seq_along(base_params)) if(length(base_params[[z]]) > 1L) base_params[[z]] <- base_params[[z]][length(base_params[[z]])]
  }

  # Return base_params
  return(list(base_params = base_params, NTrees = Trees.))
}

#' @title XGBoostParameterGrids
#'
#' @author Adrian Antico
#' @family XGBoost Helpers
#'
#' @param Shuffles The number of shuffles you want to apply to each grid
#' @param NTrees seq(500L, 5000L, 500L)
#' @param Depth seq(4L, 16L, 2L)
#' @param LearningRate seq(0.05,0.40,0.05)
#' @param MinChildWeight seq(1.0, 10.0, 1.0)
#' @param SubSample seq(0.55, 1.0, 0.05)
#' @param ColSampleByTree seq(0.55, 1.0, 0.05)
#' @return A list containing data.table's with the parameters shuffled and ready to test in the bandit framework
#' @noRd
XGBoostParameterGrids <- function(Shuffles = 1L,
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
  N_NTrees <- length(unique(Grid[['NTrees']]))
  N_Depth <- length(unique(Grid[['Depth']]))
  N_LearningRate <- length(unique(Grid[['LearningRate']]))
  Runs <- max(N_NTrees, N_Depth, N_LearningRate)
  Grids <- list()

  # Create grid sets----
  for(i in seq_len(Runs)) {
    if(i == 1L) {
      Grids[[paste0('Grid_',i)]] <- Grid[NTrees <= unique(Grid[['NTrees']])[min(i,N_NTrees)] & Depth <= unique(Grid[['Depth']])[min(i,N_Depth)] & LearningRate <= unique(Grid[['LearningRate']])[min(i,N_LearningRate)]]
    } else {
      Grids[[paste0('Grid_',i)]] <- data.table::fsetdiff(
        Grid[NTrees <= unique(Grid[['NTrees']])[min(i,N_NTrees)] & Depth <= unique(Grid[['Depth']])[min(i,N_Depth)] & LearningRate <= unique(Grid[['LearningRate']])[min(i,N_LearningRate)]],
        Grid[NTrees <= unique(Grid[['NTrees']])[min(i-1L,N_NTrees)] & Depth <= unique(Grid[['Depth']])[min(i-1L,N_Depth)] & LearningRate <= unique(Grid[['LearningRate']])[min(i-1L,N_LearningRate)]])
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
    ColSampleByTree = rep('aa', 10000L))

  # Shuffle grid sets----
  for(shuffle in seq_len(Shuffles)) for(i in seq_len(Runs)) Grids[[paste0('Grid_',i)]] <- Grids[[paste0('Grid_',i)]][order(runif(Grids[[paste0('Grid_',i)]][,.N]))]

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
  base_params$booster <- 'gbtree'
  base_params$objective <- Objective.
  base_params$eval_metric <- tolower(EvalMetric.)
  base_params$nthread <- NThreads.
  base_params$max_bin <- 64L
  base_params$early_stopping_rounds <- 10L
  base_params$tree_method <- TreeMethod.

  # Run-dependent args and updates
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$max_depth <- GridClusters.[[paste0('Grid_', counter.-1L)]][['Depth']][1L] else if(counter. != 1) base_params$max_depth <- GridClusters.[[paste0('Grid_',NewGrid.)]][['Depth']][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$eta <- GridClusters.[[paste0('Grid_', counter.-1L)]][['LearningRate']][1L] else if(counter. != 1L) base_params$eta <- GridClusters.[[paste0('Grid_',NewGrid.)]][['LearningRate']][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$min_child_weight <- GridClusters.[[paste0('Grid_', counter.-1L)]][['MinChildWeight']][1L] else if(counter. != 1L) base_params$eta <- GridClusters.[[paste0('Grid_',NewGrid.)]][['MinChildWeight']][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$subsample <- GridClusters.[[paste0('Grid_',counter.-1L)]][['SubSample']][1L] else if(counter. != 1L) base_params$subsample <- GridClusters.[[paste0('Grid_',NewGrid.)]][['SubSample']][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) base_params$colsample_bytree <- GridClusters.[[paste0('Grid_',counter.-1L)]][['ColSampleByTree']][1L] else if(counter. != 1L) base_params$colsample_bytree <- GridClusters.[[paste0('Grid_',NewGrid.)]][['ColSampleByTree']][N.]

  # Return
  return(base_params)
}

#' @title XGBoostMultiClassPredict
#'
#' @description Create prediction output that matches catboost for multiclass models. Class prediction along with probabilities for each class
#'
#' @param model Passthrough
#' @param datatest Passthrough
#' @param TargetLevels Passthrough
#' @param NumLevels Passthrough
#' @param NumberRows rows in scoring data
#'
#' @noRd
XGBoostMultiClassPredict <- function(model = NULL,
                                     datatest = NULL,
                                     TargetLevels = NULL,
                                     NumLevels = NULL,
                                     NumberRows = NULL) {
  temp1 <- stats::predict(model, datatest)
  predict <- data.table::data.table(Preds = temp1, Label = 0L:(NumLevels-1L), ID = sort(rep(seq_len(NumberRows), NumLevels)))
  data.table::setkeyv(predict, 'Label')
  data.table::setkeyv(TargetLevels, 'NewLevels')
  predict[TargetLevels, OriginalLevels := i.OriginalLevels][, Predict := OriginalLevels][, OriginalLevels := NULL]
  data.table::setorderv(predict, c('ID','Preds'), c(1L,-1L))
  Class <- predict[, list(Predict = data.table::first(Predict)), keyby = 'ID']
  predict <- data.table::dcast.data.table(data = predict, formula = ID ~ Label, fun.aggregate = data.table::first, value.var = 'Preds', fill = 0)
  data.table::setkeyv(predict, 'ID')
  predict[Class, Predict := i.Predict][, ID := NULL]
  data.table::setcolorder(predict, c(ncol(predict), seq_len((ncol(predict)-1L))))
  data.table::setnames(predict, names(predict)[2L:ncol(predict)], as.character(TargetLevels[[1L]]))
  return(predict)
}

#' @title XGBoostValidation
#'
#' @description Generate validation, importance, and shap data
#'
#' @family XGBoost Helpers
#' @author Adrian Antico
#'
#' @param model. Passthrough
#' @param ModelType Passthrough
#' @param TrainOnFull. Passthrough
#' @param TestDataCheck Passthrough
#' @param FinalTestTarget. Passthrough
#' @param TestData. Passthrough
#' @param TestTarget. Passthrough
#' @param TrainTarget. Passthrough
#' @param TrainMerge. Passthrough
#' @param TestMerge. Passthrough
#' @param dataTest. Passthrough
#' @param data. Passthrough
#' @param predict. Passthrough
#' @param TargetColumnName. Passthrough
#' @param SaveModelObjects. Passthrough
#' @param metadata_path. Passthrough
#' @param model_path. Passthrough
#' @param ModelID. Passthrough
#' @param LossFunction. Passthrough
#' @param TransformNumericColumns. Passthrough
#' @param GridTune. Passthrough
#' @param TransformationResults. Passthrough
#' @param TargetLevels. Passthrough
#'
#' @noRd
XGBoostValidationData <- function(model.=model,
                                  ModelType = 'classification',
                                  TrainOnFull. = TrainOnFull,
                                  TestDataCheck = FALSE,
                                  FinalTestTarget. = FinalTestTarget,
                                  TestTarget. = TestTarget,
                                  TrainTarget. = TrainTarget,
                                  TrainMerge. = NULL,
                                  TestMerge. = TestMerge,
                                  TestData. = TestData,
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
  if(ModelType == 'classification') {

    # Generate validation data
    if(!TrainOnFull.) {
      if(TestDataCheck) {
        ValidationData <- data.table::as.data.table(cbind(TestMerge., p1 = predict.))
        if(!any(class(model.) %chin% c('lgb.Booster', 'R6'))) {
          ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(TestData.), model = model., features = names(TestData.))$shap_contrib)
        } else {
          ShapValues <- NULL
        }
      } else {
        ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., dataTest., p1 = predict.))
        data.table::setnames(ValidationData, 'Target', eval(TargetColumnName.), skip_absent = TRUE)
        if(!any(class(model.) %chin% c('lgb.Booster', 'R6'))) {
          ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(dataTest.), model = model., features = names(dataTest.))$shap_contrib)
        } else {
          ShapValues <- NULL
        }
      }
    } else if(!is.null(TrainMerge.)) {
      ValidationData <- data.table::as.data.table(cbind(TrainMerge., predict.))
      if(!any(class(model.) %chin% c('lgb.Booster', 'R6'))) {
        ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(data.), model = model., features = names(data.))$shap_contrib)
        Shap_test <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(dataTest.), model = model., features = names(dataTest.))$shap_contrib)
        ShapValues <- data.table::rbindlist(list(ShapValues, Shap_test))
        rm(Shap_test)
      } else {
        ShapValues <- NULL
      }
    } else {
      ValidationData <- data.table::as.data.table(cbind(Target = TrainTarget., data., p1 = predict.))
      data.table::setnames(ValidationData, 'Target', eval(TargetColumnName.), skip_absent = TRUE)
      if(!any(class(model.) %chin% c('lgb.Booster', 'R6'))) {
        ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(data.), model = model., features = names(data.))$shap_contrib)
      } else {
        ShapValues <- NULL
      }
    }
  }

  # Regression
  if(ModelType == 'regression') {

    # Generate validation data
    if(!TrainOnFull.) {
      if(TestDataCheck) {
        ValidationData <- data.table::as.data.table(cbind(TestMerge., Predict = predict.))
        data.table::setnames(ValidationData, 'Target', TargetColumnName., skip_absent = TRUE)
        if(!any(class(model.) %chin% c('lgb.Booster', 'R6'))) {
          ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(TestData.), model = model., features = names(TestData.))$shap_contrib)
        } else {
          ShapValues <- NULL
        }
      } else {
        ValidationData <- data.table::as.data.table(cbind(Target = TestTarget., dataTest., Predict = predict.))
        if(!any(class(model.) %chin% c('lgb.Booster', 'R6'))) {
          ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(dataTest.), model = model., features = names(dataTest.))$shap_contrib)
        } else {
          ShapValues <- NULL
        }
        if(length(TargetColumnName.) > 1L) {
          data.table::setnames(ValidationData, c(names(ValidationData)[seq_along(TargetColumnName.)]), c(TargetColumnName.))
        } else {
          data.table::setnames(ValidationData, 'Target', TargetColumnName.)
        }
      }
    } else if(!is.null(TrainMerge.)) {
      ValidationData <- data.table::as.data.table(cbind(TrainMerge., predict.))
      if(!any(class(model.) %chin% c('lgb.Booster', 'R6'))) {
        ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(data.), model = model., features = names(data.))$shap_contrib)
        Shap_test <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(dataTest.), model = model., features = names(dataTest.))$shap_contrib)
        ShapValues <- data.table::rbindlist(list(ShapValues, Shap_test))
        rm(Shap_test)
      } else {
        ShapValues <- NULL
      }
    } else {
      ValidationData <- data.table::as.data.table(cbind(Target = TrainTarget., data., Predict = predict.))
      if(!any(class(model.) %chin% c('lgb.Booster', 'R6'))) {
        ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(data.), model = model., features = names(data.))$shap_contrib)
      } else {
        ShapValues <- NULL
      }
      data.table::setnames(ValidationData, 'Target', TargetColumnName.)
    }

    # Back transform before running metrics and plots
    if(!is.null(TransformNumericColumns.)) {
      if(GridTune. && !TrainOnFull.) TransformationResults. <- TransformationResults.[ColumnName != 'Predicted']
      if(length(TargetColumnName.) == 1L) {

        # Prepare transformation object
        TransformationResults. <- data.table::rbindlist(list(
          TransformationResults.,
          data.table::data.table(
            ColumnName = c('Predict'),
            MethodName = TransformationResults.[ColumnName == eval(TargetColumnName.), MethodName],
            Lambda = TransformationResults.[ColumnName == eval(TargetColumnName.), Lambda],
            NormalizedStatistics = 0L)))
        if(length(unique(TransformationResults.[['ColumnName']])) != nrow(TransformationResults.)) {
          temp <- TransformationResults.[, .N, by = 'ColumnName'][N != 1L][[1L]]
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
          Type = 'Inverse',
          FinalResults = TransformationResults.,
          TransID = NULL,
          Path = NULL)

      } else {

        # Prepare transformation object
        TransformationResults. <- data.table::rbindlist(list(TransformationResults., TransformationResults.))
        for(z in seq_along(TargetColumnName.)) TransformationResults.[length(TargetColumnName.) + z, ColumnName := paste0('Predict.V',z)]

        # Back transform
        ValidationData <- AutoTransformationScore(
          ScoringData = ValidationData,
          Type = 'Inverse',
          FinalResults = TransformationResults.,
          TransID = NULL,
          Path = NULL)
      }
    }
  }

  # Multiclass
  if(ModelType == 'multiclass') {
    if(!TrainOnFull.) {
      if(TestDataCheck) {
        ValidationData <- data.table::as.data.table(cbind(predict., TestMerge.))
        data.table::setnames(ValidationData, 'Target', TargetColumnName., skip_absent = TRUE)
        if(!any(class(model.) %chin% c('lgb.Booster', 'R6'))) {
          ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(TestData.), model = model., features = names(TestData.))$shap_contrib)
        } else {
          ShapValues <- NULL
        }
      } else {
        ValidationData <- data.table::as.data.table(cbind(predict., Target = TestTarget., dataTest.))
        if(!any(class(model.) %chin% c('lgb.Booster', 'R6'))) {
          ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(dataTest.), model = model., features = names(dataTest.))$shap_contrib)
        } else {
          ShapValues <- NULL
        }
        if(length(TargetColumnName.) > 1L) {
          data.table::setnames(ValidationData, c(names(ValidationData)[seq_along(TargetColumnName.)]), c(TargetColumnName.))
        } else {
          data.table::setnames(ValidationData, 'Target', TargetColumnName.)
        }
      }
    } else if(!is.null(TrainMerge.)) {
      ValidationData <- data.table::as.data.table(cbind(predict., TrainMerge.))
      if(!any(class(model.) %chin% c('lgb.Booster', 'R6'))) {
        ShapValues <- data.table::as.data.table(xgboost:::xgb.shap.data(as.matrix(data.), model = model., features = names(data.))$shap_contrib)
      } else {
        ShapValues <- NULL
      }
    }
  }

  # Finalize data
  if('ID_Factorizer' %chin% names(ValidationData)) data.table::set(ValidationData, j = 'ID_Factorizer', value = NULL)
  if(!any(class(model.) %chin% c('lgb.Booster', 'R6')) && !is.null(ShapValues)) {
    data.table::setnames(ShapValues, names(ShapValues), paste0('Shap_', names(ShapValues)))
    ValidationData <- cbind(ValidationData, ShapValues)
  }

  # Save validation data
  if(SaveModelObjects. && !TrainOnFull.) {
    if(!is.null(metadata_path.)) {
      data.table::fwrite(ValidationData, file = file.path(metadata_path., paste0(ModelID., '_ValidationData.csv')))
    } else {
      data.table::fwrite(ValidationData, file = file.path(model_path., paste0(ModelID., '_ValidationData.csv')))
    }
  } else if(SaveModelObjects.) {
    if(!is.null(metadata_path.)) {
      data.table::fwrite(ValidationData, file = file.path(metadata_path., paste0(ModelID., '_TrainData.csv')))
    } else {
      data.table::fwrite(ValidationData, file = file.path(model_path., paste0(ModelID., '_TrainData.csv')))
    }
  }

  # Variable Importance
  if(!any(class(model.) %chin% c('lgb.Booster', 'R6')) && !is.null(ShapValues)) {
    VariableImportance <- tryCatch({data.table::as.data.table(xgboost::xgb.importance(model = model.))}, error = function(x) NULL)
  } else {
    VariableImportance <- tryCatch({data.table::as.data.table(lightgbm::lgb.importance(model = model., percentage = TRUE))}, error = function(x) NULL)
  }

  if(!is.null(VariableImportance)) {
    VariableImportance[, ':=' (Gain = round(Gain, 4L), Cover = round(Cover, 4L), Frequency = round(Frequency, 4L))]
    data.table::setnames(VariableImportance, c('Feature','Gain'), c('Variable','Importance'))
    if(SaveModelObjects.) {
      if(!is.null(metadata_path.)) {
        data.table::fwrite(VariableImportance, file = file.path(metadata_path., paste0(ModelID., '_VariableImportance.csv')))
      } else {
        data.table::fwrite(VariableImportance, file = file.path(model_path., paste0(ModelID., '_VariableImportance.csv')))
      }
    }
  }

  # Return
  return(list(
    ValidationData = ValidationData,
    VariableImportance = VariableImportance,
    ShapValues = ShapValues,
    TransformationResults = if(exists('TransformationResults.')) TransformationResults. else NULL))
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
  if(tolower(grid_eval_metric) == 'poisson') {
    if(MinVal > 0L && min(calibEval[['p1']], na.rm = TRUE) > 0L) {
      calibEval[, Metric := p1 - Target * log(p1 + 1)]
      Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
    }
  } else if(tolower(grid_eval_metric) == 'mae') {
    calibEval[, Metric := abs(Target - p1)]
    Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
  } else if(tolower(grid_eval_metric) == 'mape') {
    calibEval[, Metric := abs((Target - p1) / (Target + 1))]
    Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
  } else if(tolower(grid_eval_metric) == 'mse') {
    calibEval[, Metric := (Target - p1) ^ 2L]
    Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
  } else if(tolower(grid_eval_metric) == 'msle') {
    if(MinVal > 0L && min(calibEval[['p1']], na.rm = TRUE) > 0L) {
      calibEval[, Metric := (log(Target + 1) - log(p1 + 1)) ^ 2L]
      Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
    }
  } else if(tolower(grid_eval_metric) == 'kl') {
    if(MinVal > 0L && min(calibEval[['p1']], na.rm = TRUE) > 0L) {
      calibEval[, Metric := Target * log((Target + 1) / (p1 + 1))]
      Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
    }
  } else if(tolower(grid_eval_metric) == 'cs') {
    calibEval[, ':=' (Metric1 = Target * p1, Metric2 = Target ^ 2L, Metric3 = p1 ^ 2L)]
    Metric <- calibEval[, sum(Metric1, na.rm = TRUE)] / (sqrt(calibEval[, sum(Metric2, na.rm = TRUE)]) * sqrt(calibEval[, sum(Metric3, na.rm = TRUE)]))
  } else if(tolower(grid_eval_metric) == 'r2') {
    Metric <- (calibEval[, stats::cor(eval(Target), p1)][[1L]]) ^ 2L
  }
  return(Metric)
}
