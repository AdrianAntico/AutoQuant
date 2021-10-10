# Admin: Args ----
if("Args Setup" == "Args Setup") {

  # Admin: Define Command Line Defaults
  ProjectID_Default <- 'Sample_Project'

  # Admin: Project Directories
  Root_Path_Default <- 'C:/Users/Bizon/Documents/GitHub/QA_Code/Sample_Project'
  Data_Path_Default <- file.path(Root_Path_Default, 'Data')
  MetaData_Path_Default <- file.path(Root_Path_Default, 'MetaData')
  Models_Path_Default <- file.path(Root_Path_Default, 'Models')
  Results_Path_Default <- file.path(Root_Path_Default, 'Results')
  ModelType_Default <- 'classifier'

  # Admin: Categorize data base columns
  TargetVariables_Default <- c('Adrian')
  DateVariables_Default <- c('DateTime')
  GroupVariables_Default <- c('Factor_1', 'Factor_2')
  ByVariables_Default <- c('Factor_1')
  TextVariables_Default <- c('Comment')
  IDVariables_Default <- c('IDcol_1', 'IDcol_2')

  # Base columns for feature engineering
  InteractionVariables_Default <- c('Independent_Variable1', 'Independent_Variable2', 'Independent_Variable3')
  DiffVariables_Default <- NULL
  DiffDateVariables_Default <- c('DateTime')
  DiffGroupVariables_Default <- NULL

  # Services args ----

  # Clean data
  Impute_Default <- TRUE
  CharToFactor_Default <- FALSE
  FactorToChar_Default <- TRUE
  IntToNumeric_Default <- TRUE
  LogicalToBinary_Default <- TRUE
  DateToChar_Default <- FALSE
  IDateConversion_Default <- TRUE
  RemoveDates_Default <- FALSE
  MissFactor_Default <- '0'
  MissNum_Default <- -1
  IgnoreCols_Default <- NULL

  # Calendar Variables
  Calendar_Variables_Default <- c('week', 'wom', 'month', 'quarter')

  # Holiday Variables
  LookBackDays_Default <- 7
  Holiday_Variables_Default <- c('USPublicHolidays','EasterGroup','ChristmasGroup','OtherEcclesticalFeasts')

  # Partial Dummies
  NumberLevels_Default <- 3

  # Interaction Variables
  InteractionDepth_Default <- 3
  InteractionCenter_Default <- TRUE
  InteractionScale_Default <- TRUE

  # Differencing
  DiffPeriods_Default <- 3L

  # Time Series Variables
  TimeSeriesGroupVariables_Default <- c('Factor_1','Factor_2')
  TimeSeriesDateGroups_Default <- c('days','weeks')
  TimeUnitAgg_Default <- c('weeks')
  Lag_Periods_Default <- 1:5
  RollAverage_Periods_Default <- 2:5
  RollStandardDeviation_Periods_Default <- 5
  RollSkewness_Periods_Default <- 5
  RollKurtosis_Periods_Default <- 5
  RollQuantiles_Periods_Default <- 5
  RollQuantiles_Default <- c('q50')

  # Partition Data
  PartitionRatios_Default <- c(0.70, 0.20, 0.10)
  PartitionMethod_Default <- 'random'
  PartitionByVariables_Default <- 'Factor_1'
  PartitionTimeColumnName_Default <- 'DateTime'

  # Categorical Encoding
  EncodeMethod_Default <- c('credibility')
  EncodeImpute_Default <- 0
  KeepCharColumns_Default <- TRUE

  # H2O General
  H2O_Memory_Default <- '128g'

  # H2O Word2Vec Variables
  BuildType_Default <- 'individual'
  NumberVectors_Default <- 20
  Window_Default <- 5
  Iterations_Default <- 20
  MinWords_Default <- 2

  # H2O AutoEncoder
  AnomalyDetection_Default <- TRUE
  DimensionReduction_Default <- TRUE
  AD_PerFeature_Default <- FALSE
  RemoveBaseFeatures_Default <- FALSE
  NodeShrinkRate_Default <- (sqrt(5) - 1) / 2
  Epochs_Default <- 20
  L2_Default <- 0.10
  ElasticAveraging_Default <- TRUE
  ElasticAveragingMovingRate_Default <- 0.90
  ElasticAveragingRegularization_Default <- 0.001

  # H2O Isolation Forest
  Threshold_Default <- 0.95
  NTrees_Default <- 500
  MaxDepth_Default <- 8
  MinRows_Default <- 1
  RowSampleRate_Default <- (sqrt(5)-1)/2
  ColSampleRate_Default <- 1
  ColSampleRatePerLevel_Default <- 1
  ColSampleRatePerTree_Default <- 1

  # H2O Clustering
  MaxClusters_Default <- 50
  ClusterMetric_Default <- 'totss'
  Clustering_ShrinkRate_Default <- (sqrt(5) - 1) / 2
  Clustering_Epochs_Default <- 20
  Clustering_L2_Default <- 0.10
  Clustering_ElasticAveraging_Default <- TRUE
  Clustering_ElasticAveragingMovingRate_Default <- 0.90
  Clustering_ElasticAveragingRegularization_Default <- 0.001


  # Services default
  Services_Default <- c('CalendarVariables', 'HolidayVariables', 'PartialDummies', 'FeatureInteraction', 'Differencing', 'TimeSeries', 'DataPartition', 'Encoding') #, 'H2OWord2Vec', 'H2OAutoEncoder', 'H2OIsolationForest', 'H2OClustering')

  # Admin: OptParse() Setup ----
  OptionList <- list(

    # Services to run
    optparse::make_option(opt_str = '--Services', type = 'character', default = Services_Default, help = 'Feature engineering functions to run'),

    # Project Admin
    optparse::make_option(opt_str = '--ProjectID', type = 'character', default = ProjectID_Default, help = 'Project ID'),
    optparse::make_option(opt_str = '--Root_Path', type = 'character', default = Root_Path_Default, help = 'Root directory'),
    optparse::make_option(opt_str = '--Data_Path', type = 'character', default = Data_Path_Default, help = 'Path to Data folder'),
    optparse::make_option(opt_str = '--MetaData_Path', type = 'character', default = MetaData_Path_Default, help = 'Path to MetaData folder'),
    optparse::make_option(opt_str = '--Model_Path', type = 'character', default = Models_Path_Default, help = 'Path to Models folder'),
    optparse::make_option(opt_str = '--Results_Path', type = 'character', default = Results_Path_Default, help = 'Path to Results folder'),
    optparse::make_option(opt_str = '--ModelType', type = 'character', default = ModelType_Default, help = 'classifier regression or multiclass'),

    # Data args
    optparse::make_option(opt_str = '--TargetVariables', type = 'character', default = TargetVariables_Default, help = 'Target variables in a modeling sense'),
    optparse::make_option(opt_str = '--DateVariables', type = 'character', default = DateVariables_Default, help = 'Date variables in a modeling sense'),
    optparse::make_option(opt_str = '--GroupVariables', type = 'character', default = GroupVariables_Default, help = 'Group variables in a modeling sense'),
    optparse::make_option(opt_str = '--ByVariables', type = 'character', default = ByVariables_Default, help = 'Byvariables for data wrangling by group vars'),
    optparse::make_option(opt_str = '--TextVariables', type = 'character', default = TextVariables_Default, help = 'Text variables in a modeling sense'),
    optparse::make_option(opt_str = '--IDVariables', type = 'character', default = IDVariables_Default, help = 'Meta data columns'),
    optparse::make_option(opt_str = '--InteractionVariables', type = 'character', default = InteractionVariables_Default, help = 'Variables to use to create interaction variables'),
    optparse::make_option(opt_str = '--DiffVariables', type = 'character', default = DiffVariables_Default, help = 'Variables to use to create time based differences'),
    optparse::make_option(opt_str = '--DiffDateVariables', type = 'character', default = DiffDateVariables_Default, help = 'Variables to use to create time based differences'),
    optparse::make_option(opt_str = '--DiffGroupVariables', type = 'character', default = DiffGroupVariables_Default, help = 'Variables to use to create time based differences'),

    # Clean data
    optparse::make_option(opt_str = '--Impute', type = 'logical', default = Impute_Default, help = 'Impute on / off logical'),
    optparse::make_option(opt_str = '--CharToFactor', type = 'logical', default = CharToFactor_Default, help = 'Convert character columns to factor columns'),
    optparse::make_option(opt_str = '--FactorToChar', type = 'logical', default = FactorToChar_Default, help = 'Convert factor columns to character columns'),
    optparse::make_option(opt_str = '--IntToNumeric', type = 'logical', default = IntToNumeric_Default, help = 'Convert integer columns to numeric columns'),
    optparse::make_option(opt_str = '--LogicalToBinary', type = 'logical', default = LogicalToBinary_Default, help = 'Convert logical columns to numeric colums with binary values'),
    optparse::make_option(opt_str = '--DateToChar', type = 'logical', default = DateToChar_Default, help = 'Convert date column types to character column types'),
    optparse::make_option(opt_str = '--IDateConversion', type = 'logical', default = IDateConversion_Default, help = 'Convert IDate types to Date types and IDateTime to POSIXct types'),
    optparse::make_option(opt_str = '--RemoveDates', type = 'logical', default = RemoveDates_Default, help = 'Remove date columns from the data set'),
    optparse::make_option(opt_str = '--MissFactor', type = 'logical', default = MissFactor_Default, help = 'Imputation static value for factor and character types'),
    optparse::make_option(opt_str = '--MissNum', type = 'logical', default = MissNum_Default, help = 'Imputation static value for numeric and integer types'),
    optparse::make_option(opt_str = '--IgnoreCols', type = 'logical', default = IgnoreCols_Default, help = 'Columns to skip in cleaning process'),

    # Calendar Variables
    optparse::make_option(opt_str = '--Calendar_Variables', type = 'character', default = Calendar_Variables_Default, help = 'Calendar variable selection'),

    # Holiday Variables
    optparse::make_option(opt_str = '--LookBackDays', type = 'numeric', default = LookBackDays_Default, help = 'Lookback number of days to idenify holidays'),
    optparse::make_option(opt_str = '--Holiday_Variables', type = 'character', default = Holiday_Variables_Default, help = 'Holiday groups to look for'),

    # Partial Dummies
    optparse::make_option(opt_str = '--NumberLevels', type = 'numeric', default = NumberLevels_Default, help = 'Number of levels to dummify for each categorical variable'),

    # Interaction Variables
    optparse::make_option(opt_str = '--InteractionDepth', type = 'numeric', default = InteractionDepth_Default, help = 'Max number of interaction variables to utilize'),
    optparse::make_option(opt_str = '--InteractionCenter', type = 'logical', default = InteractionCenter_Default, help = 'Center values before creating interactions'),
    optparse::make_option(opt_str = '--InteractionScale', type = 'logical', default = InteractionScale_Default, help = 'Scale values before creating interactions'),

    # Differencing
    optparse::make_option(opt_str = '--DiffPeriods', type = 'numeric', default = DiffPeriods_Default, help = 'Max periods back to compute differences for base features. 1:DiffPeriods are utilized'),

    # Time Series Variables
    optparse::make_option(opt_str = '--TimeSeriesGroupVariables', type = 'character', default = TimeSeriesGroupVariables_Default, help = 'Variables to use to create time based differences'),
    optparse::make_option(opt_str = '--TimeSeriesDateGroups', type = 'character', default = TimeSeriesDateGroups_Default, help = 'Variables to use to create time based differences'),
    optparse::make_option(opt_str = '--TimeUnitAgg', type = 'character', default = TimeUnitAgg_Default, help = 'Level of time based aggregation'),
    optparse::make_option(opt_str = '--Lag_Periods', type = 'numeric', default = Lag_Periods_Default, help = 'Lags'),
    optparse::make_option(opt_str = '--RollAverage_Periods', type = 'numeric', default = RollAverage_Periods_Default, help = 'Rolling Mean'),
    optparse::make_option(opt_str = '--RollStandardDeviation_Periods', type = 'numeric', default = RollStandardDeviation_Periods_Default, help = 'Rolling Standard Deviation'),
    optparse::make_option(opt_str = '--RollSkewness_Periods', type = 'numeric', default = RollSkewness_Periods_Default, help = 'Rolling Skewnesss'),
    optparse::make_option(opt_str = '--RollKurtosis_Periods', type = 'numeric', default = RollKurtosis_Periods_Default, help = 'Rolling Kurtosis'),
    optparse::make_option(opt_str = '--RollQuantiles_Periods', type = 'numeric', default = RollQuantiles_Periods_Default, help = 'Rolling Quantiles'),
    optparse::make_option(opt_str = '--RollQuantiles', type = 'character', default = RollQuantiles_Default, help = 'Quantiles to utilize'),

    # Partition Data
    optparse::make_option(opt_str = '--PartitionRatios', type = 'numeric', default = PartitionRatios_Default, help = 'Vector of percentages for generating data sets for modeling'),
    optparse::make_option(opt_str = '--PartitionByVariables', type = 'character', default = PartitionByVariables_Default, help = 'Group variables to stratify by'),
    optparse::make_option(opt_str = '--PartitionTimeColumnName', type = 'character', default = PartitionTimeColumnName_Default, help = 'Time column name for time sampling'),
    optparse::make_option(opt_str = '--PartitionMethod', type = 'character', default = PartitionMethod_Default, help = 'Choose from random, time, or timeseries'),

    # Categorical Encoding
    optparse::make_option(opt_str = '--EncodeMethod', type = 'character', default = EncodeMethod_Default, help = 'Categorical encoding method'),
    optparse::make_option(opt_str = '--EncodeImpute', type = 'numeric', default = EncodeImpute_Default, help = 'Imputation value for new or missing levels'),
    optparse::make_option(opt_str = '--KeepCharColumns', type = 'logical', default = KeepCharColumns_Default, help = 'Keep character columns after creating numeric encodings'),

    # H2O General
    optparse::make_option(opt_str = '--H2O_Memory', type = 'character', default = H2O_Memory_Default, help = 'Level of time based aggregation'),

    # H2O Word2Vec Variables
    optparse::make_option(opt_str = '--BuildType', type = 'character', default = BuildType_Default, help = 'Word2Vec build type'),
    optparse::make_option(opt_str = '--NumberVectors', type = 'numeric', default = NumberVectors_Default, help = 'Number of vectors to create'),
    optparse::make_option(opt_str = '--Window', type = 'numeric', default = Window_Default, help = 'Word2Vec look forward and lookback window size'),
    optparse::make_option(opt_str = '--Iterations', type = 'numeric', default = Iterations_Default, help = 'Word2Vec learning iterations'),
    optparse::make_option(opt_str = '--MinWords', type = 'numeric', default = MinWords_Default, help = 'Minimum word count for inclusion'),

    # H2O AutoEncoder
    optparse::make_option(opt_str = '--AnomalyDetection', type = 'logical', default = AnomalyDetection_Default, help = 'Return anomaly detection variables'),
    optparse::make_option(opt_str = '--DimensionReduction', type = 'logical', default = DimensionReduction_Default, help = 'Return dim reduction variables'),
    optparse::make_option(opt_str = '--AD_PerFeature', type = 'logical', default = AD_PerFeature_Default, help = 'Anomaly detection for each column (TRUE) or overall (FALSE)'),
    optparse::make_option(opt_str = '--RemoveBaseFeatures', type = 'logical', default = RemoveBaseFeatures_Default, help = 'Retain original features'),
    optparse::make_option(opt_str = '--NodeShrinkRate', type = 'numeric', default = NodeShrinkRate_Default, help = 'Layer shrink rate layer to layer'),
    optparse::make_option(opt_str = '--Epochs', type = 'numeric', default = Epochs_Default, help = 'Number of epochs for the model'),
    optparse::make_option(opt_str = '--L2', type = 'numeric', default = L2_Default, help = 'L2 regularization'),
    optparse::make_option(opt_str = '--ElasticAveraging', type = 'logical', default = ElasticAveraging_Default, help = 'Utilize elastic averaging?'),
    optparse::make_option(opt_str = '--ElasticAveragingMovingRate', type = 'numeric', default = ElasticAveragingMovingRate_Default, help = 'Elastic averaging moving rate value'),
    optparse::make_option(opt_str = '--ElasticAveragingRegularization', type = 'numeric', default = ElasticAveragingRegularization_Default, help = 'Elastic averaging regularization value'),

    # H2O Isolation Forest
    optparse::make_option(opt_str = '--Threshold', type = 'numeric', default = Threshold_Default, help = 'Cutoff for classifying anomalies'),
    optparse::make_option(opt_str = '--NTrees', type = 'numeric', default = NTrees_Default, help = 'Number of trees to use'),
    optparse::make_option(opt_str = '--MaxDepth', type = 'numeric', default = MaxDepth_Default, help = 'Max tree depth'),
    optparse::make_option(opt_str = '--MinRows', type = 'numeric', default = MinRows_Default, help = 'Min rows per leaf'),
    optparse::make_option(opt_str = '--RowSampleRate', type = 'numeric', default = RowSampleRate_Default, help = 'Rows sampled per tree'),
    optparse::make_option(opt_str = '--ColSampleRate', type = 'numeric', default = ColSampleRate_Default, help = 'Columns sampled per split'),
    optparse::make_option(opt_str = '--ColSampleRatePerLevel', type = 'numeric', default = ColSampleRatePerLevel_Default, help = 'Columns sampled per level'),
    optparse::make_option(opt_str = '--ColSampleRatePerTree', type = 'numeric', default = ColSampleRatePerTree_Default, help = 'Columns sampled per tree'),

    # H2O Clustering
    optparse::make_option(opt_str = '--MaxClusters', type = 'numeric', default = MaxClusters_Default, help = 'Max number of clusters to generate'),
    optparse::make_option(opt_str = '--ClusterMetric', type = 'character', default = ClusterMetric_Default, help = 'Loss function metric'),
    optparse::make_option(opt_str = '--Clustering_ShrinkRate', type = 'numeric', default = Clustering_ShrinkRate_Default, help = 'Layer shrink rate for autoencoder'),
    optparse::make_option(opt_str = '--Clustering_Epochs', type = 'numeric', default = Clustering_Epochs_Default, help = 'Epochs for autoencoder'),
    optparse::make_option(opt_str = '--Clustering_L2', type = 'numeric', default = Clustering_L2_Default, help = 'L2 for autoencoder'),
    optparse::make_option(opt_str = '--Clustering_ElasticAveraging', type = 'logical', default = Clustering_ElasticAveraging_Default, help = 'Elastic averaging for autoencoder'),
    optparse::make_option(opt_str = '--Clustering_ElasticAveragingMovingRate', type = 'numeric', default = Clustering_ElasticAveragingMovingRate_Default, help = 'Elastic averaging value for autoencoder'),
    optparse::make_option(opt_str = '--Clustering_ElasticAveragingRegularization', type = 'numeric', default = Clustering_ElasticAveragingRegularization_Default, help = 'Elastic averaging regularization for autoencoder')
  )

  # Admin: Store Args
  obj <- optparse::OptionParser(option_list = OptionList, add_help_option = TRUE)
  CL_ArgsList_FE <- optparse::parse_args(object = obj)

  # Admin: Correct multi-value passthrough
  CL_ArgsList_FE$Services <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$Services)
  CL_ArgsList_FE$TargetVariables <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$TargetVariables)
  CL_ArgsList_FE$IDVariables <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$IDVariables)
  CL_ArgsList_FE$DateVariables <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$DateVariables)
  CL_ArgsList_FE$GroupVariables <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$GroupVariables)
  CL_ArgsList_FE$ByVariables <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$ByVariables)
  CL_ArgsList_FE$InteractionVariables <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$InteractionVariables)
  CL_ArgsList_FE$DiffVariables <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$DiffVariables)
  CL_ArgsList_FE$DiffDateVariables <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$DiffDateVariables)
  CL_ArgsList_FE$DiffGroupVariables <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$DiffGroupVariables)
  CL_ArgsList_FE$TimeSeriesGroupVariables <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$TimeSeriesGroupVariables)
  CL_ArgsList_FE$TimeSeriesDateGroups <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$TimeSeriesDateGroups)

  # Admin: Save CL_ArgsList_FE
  save(CL_ArgsList_FE, file = file.path(CL_ArgsList_FE$MetaData_Path, 'CL_ArgsList_FE.Rdata'))

  # Set up new list to obsorb CL_ArgsList_FE and collection information from Feature Engineering runs
  ArgsList <- list()

  # Service Args
  ArgsList$Services <- CL_ArgsList_FE$Services

  # Data Args
  ArgsList$Data$TargetVariables <- CL_ArgsList_FE$TargetVariables
  ArgsList$Data$DateVariables <- CL_ArgsList_FE$DateVariables
  ArgsList$Data$GroupVariables <- CL_ArgsList_FE$GroupVariables
  ArgsList$Data$ByVariables <- CL_ArgsList_FE$ByVariables
  ArgsList$Data$TextVariables <- CL_ArgsList_FE$TextVariables
  ArgsList$Data$InteractionVariables <- CL_ArgsList_FE$InteractionVariables
  ArgsList$Data$DiffVariables <- CL_ArgsList_FE$DiffVariables
  ArgsList$Data$IDVariables <- CL_ArgsList_FE$IDVariables
  ArgsList$Data$DiffDateVariables <- CL_ArgsList_FE$DiffDateVariables
  ArgsList$Data$DiffGroupVariables <- CL_ArgsList_FE$DiffGroupVariables
  ArgsList$Data$TimeSeriesGroupVariables <- CL_ArgsList_FE$TimeSeriesGroupVariables <- CL_ArgsList_FE$TimeSeriesGroupVariables
  ArgsList$Data$TimeSeriesDateGroups <- CL_ArgsList_FE$TimeSeriesDateGroups

  # MetaData Args
  ArgsList$MetaData$ProjectID <- CL_ArgsList_FE$ProjectID
  ArgsList$MetaData$Root_Path <- CL_ArgsList_FE$Root_Path
  ArgsList$MetaData$Data_Path <- CL_ArgsList_FE$Data_Path
  ArgsList$MetaData$MetaData_Path <- CL_ArgsList_FE$MetaData_Path
  ArgsList$MetaData$Model_Path <- CL_ArgsList_FE$Model_Path
  ArgsList$MetaData$Results_Path <- CL_ArgsList_FE$Results_Path
  ArgsList$MetaData$ModelType <- CL_ArgsList_FE$ModelType

  # Services Method Args ----

  # General
  ArgsList$FE_Args$General$H2O_Memory <- CL_ArgsList_FE$H2O_Memory

  # Clean data
  ArgsList$FE_Args$Clean$Impute <- CL_ArgsList_FE$Impute
  ArgsList$FE_Args$Clean$CharToFactor <- CL_ArgsList_FE$CharToFactor
  ArgsList$FE_Args$Clean$FactorToChar <- CL_ArgsList_FE$FactorToChar
  ArgsList$FE_Args$Clean$IntToNumeric <- CL_ArgsList_FE$IntToNumeric
  ArgsList$FE_Args$Clean$LogicalToBinary <- CL_ArgsList_FE$LogicalToBinary
  ArgsList$FE_Args$Clean$DateToChar <- CL_ArgsList_FE$DateToChar
  ArgsList$FE_Args$Clean$IDateConversion <- CL_ArgsList_FE$IDateConversion
  ArgsList$FE_Args$Clean$RemoveDates <- CL_ArgsList_FE$RemoveDates
  ArgsList$FE_Args$Clean$MissFactor <- CL_ArgsList_FE$MissFactor
  ArgsList$FE_Args$Clean$MissNum <- CL_ArgsList_FE$MissNum
  ArgsList$FE_Args$Clean$IgnoreCols <- CL_ArgsList_FE$IgnoreCols

  # Calendar Variables
  ArgsList$FE_Args$Calendar$CalendarVariables <- CL_ArgsList_FE$Calendar_Variables

  # Holiday Variables
  ArgsList$FE_Args$Holiday$LookBackDays <- CL_ArgsList_FE$LookBackDays
  ArgsList$FE_Args$Holiday$HolidayVariables <- CL_ArgsList_FE$Holiday_Variables

  # Partial Dummies
  ArgsList$FE_Args$Partial_Dummies$NumberLevels <- CL_ArgsList_FE$NumberLevels

  # Interaction Variables
  ArgsList$FE_Args$Interaction$InteractionDepth <- CL_ArgsList_FE$InteractionDepth
  ArgsList$FE_Args$Interaction$InteractionCenter <- CL_ArgsList_FE$InteractionCenter
  ArgsList$FE_Args$Interaction$InteractionScale <- CL_ArgsList_FE$InteractionScale

  # Differencing
  ArgsList$FE_Args$Differencing$DiffPeriods <- CL_ArgsList_FE$DiffPeriods

  # Time Series Variables
  ArgsList$FE_Args$TimeSeriesVariables$TimeSeriesGroupVariables <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$TimeSeriesGroupVariables)
  ArgsList$FE_Args$TimeSeriesVariables$TimeSeriesDateGroups <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$TimeSeriesDateGroups)
  ArgsList$FE_Args$TimeSeriesVariables$TimeUnitAgg <- CL_ArgsList_FE$TimeUnitAgg
  ArgsList$FE_Args$TimeSeriesVariables$Lag_Periods <- CL_ArgsList_FE$Lag_Periods
  ArgsList$FE_Args$TimeSeriesVariables$RollAverage_Periods <- CL_ArgsList_FE$RollAverage_Periods
  ArgsList$FE_Args$TimeSeriesVariables$RollStandardDeviation_Periods <- CL_ArgsList_FE$RollStandardDeviation_Periods
  ArgsList$FE_Args$TimeSeriesVariables$RollSkewness_Periods <- CL_ArgsList_FE$RollSkewness_Periods
  ArgsList$FE_Args$TimeSeriesVariables$RollKurtosis_Periods <- CL_ArgsList_FE$RollKurtosis_Periods
  ArgsList$FE_Args$TimeSeriesVariables$RollQuantiles_Periods <- CL_ArgsList_FE$RollQuantiles_Periods
  ArgsList$FE_Args$TimeSeriesVariables$RollQuantiles <- RemixAutoML:::ParseOptParse(CL_ArgsList_FE$RollQuantiles)

  # Partition Data
  ArgsList$FE_Args$Partition$PartitionRatios <- CL_ArgsList_FE$PartitionRatios
  ArgsList$FE_Args$Partition$PartitionByVariables <- CL_ArgsList_FE$PartitionByVariables
  ArgsList$FE_Args$Partition$PartitionTimeColumnName <- CL_ArgsList_FE$PartitionTimeColumn
  ArgsList$FE_Args$Partition$PartitionMethod <- CL_ArgsList_FE$PartitionMethod

  # Categorical Encoding
  ArgsList$FE_Args$Encoding$EncodeMethod <- CL_ArgsList_FE$EncodeMethod
  ArgsList$FE_Args$Encoding$EncodeImpute <- CL_ArgsList_FE$EncodeImpute
  ArgsList$FE_Args$Encoding$KeepCharColumns <- CL_ArgsList_FE$KeepCharColumns


  # H2O Word2Vec Variables
  ArgsList$FE_Args$H2O_Word2Vec$BuildType <- CL_ArgsList_FE$BuildType
  ArgsList$FE_Args$H2O_Word2Vec$NumberVectors <- CL_ArgsList_FE$NumberVectors
  ArgsList$FE_Args$H2O_Word2Vec$Window <- CL_ArgsList_FE$Window
  ArgsList$FE_Args$H2O_Word2Vec$Iterations <- CL_ArgsList_FE$Iterations
  ArgsList$FE_Args$H2O_Word2Vec$MinWords <- CL_ArgsList_FE$MinWords

  # H2O AutoEncoder
  ArgsList$FE_Args$H2O_Autoencoder$AnomalyDetection <- CL_ArgsList_FE$AnomalyDetection
  ArgsList$FE_Args$H2O_Autoencoder$DimensionReduction <- CL_ArgsList_FE$DimensionReduction
  ArgsList$FE_Args$H2O_Autoencoder$AD_PerFeature <- CL_ArgsList_FE$AD_PerFeature
  ArgsList$FE_Args$H2O_Autoencoder$RemoveBaseFeatures <- CL_ArgsList_FE$RemoveBaseFeatures
  ArgsList$FE_Args$H2O_Autoencoder$NodeShrinkRate <- CL_ArgsList_FE$NodeShrinkRate
  ArgsList$FE_Args$H2O_Autoencoder$Epochs <- CL_ArgsList_FE$Epochs
  ArgsList$FE_Args$H2O_Autoencoder$L2 <- CL_ArgsList_FE$L2
  ArgsList$FE_Args$H2O_Autoencoder$ElasticAveraging <- CL_ArgsList_FE$ElasticAveraging
  ArgsList$FE_Args$H2O_Autoencoder$ElasticAveragingMovingRate <- CL_ArgsList_FE$ElasticAveragingMovingRate
  ArgsList$FE_Args$H2O_Autoencoder$ElasticAveragingRegularization <- CL_ArgsList_FE$ElasticAveragingRegularization

  # H2O Isolation Forest
  ArgsList$FE_Args$H2O_IsolationForest$Threshold <- CL_ArgsList_FE$Threshold
  ArgsList$FE_Args$H2O_IsolationForest$NTrees <- CL_ArgsList_FE$NTrees
  ArgsList$FE_Args$H2O_IsolationForest$MaxDepth <- CL_ArgsList_FE$MaxDepth
  ArgsList$FE_Args$H2O_IsolationForest$MinRows <- CL_ArgsList_FE$MinRows
  ArgsList$FE_Args$H2O_IsolationForest$RowSampleRate <- CL_ArgsList_FE$RowSampleRate
  ArgsList$FE_Args$H2O_IsolationForest$ColSampleRate <- CL_ArgsList_FE$ColSampleRate
  ArgsList$FE_Args$H2O_IsolationForest$ColSampleRatePerLevel <- CL_ArgsList_FE$ColSampleRatePerLevel
  ArgsList$FE_Args$H2O_IsolationForest$ColSampleRatePerTree <- CL_ArgsList_FE$ColSampleRatePerTree

  # H2O Clustering
  ArgsList$FE_Args$H2O_Clustering$MaxClusters <- CL_ArgsList_FE$MaxClusters
  ArgsList$FE_Args$H2O_Clustering$ClusterMetric <- CL_ArgsList_FE$ClusterMetric
  ArgsList$FE_Args$H2O_Clustering$Clustering_ShrinkRate <- CL_ArgsList_FE$Clustering_ShrinkRate
  ArgsList$FE_Args$H2O_Clustering$Clustering_Epochs <- CL_ArgsList_FE$Clustering_Epochs
  ArgsList$FE_Args$H2O_Clustering$Clustering_L2 <- CL_ArgsList_FE$Clustering_L2
  ArgsList$FE_Args$H2O_Clustering$Clustering_ElasticAveraging <- CL_ArgsList_FE$Clustering_ElasticAveraging
  ArgsList$FE_Args$H2O_Clustering$Clustering_ElasticAveragingMovingRate <- CL_ArgsList_FE$Clustering_ElasticAveragingMovingRate
  ArgsList$FE_Args$H2O_Clustering$Clustering_ElasticAveragingRegularization <- CL_ArgsList_FE$Clustering_ElasticAveragingRegularization

  # Admin: Clear objects from memory
  rm(list = ls()[!ls() %in% "ArgsList"])

  # Save ArgsList
  save(ArgsList, file = file.path(ArgsList$MetaData$MetaData_Path, 'ArgsList.Rdata'))
}
