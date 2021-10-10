# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
# :: Model Training ::
#
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Environment Setup ----
if("Args Setup" == "Args Setup") {

  # Project
  ProjectID_Default <- "Sample_Project"
  Root_Path_Default <- "C:/Users/Bizon/Documents/GitHub/QA_Code/Sample_Project"

  # Target Type
  Algorithm_Default <- "catboost"
  CatBoost_TargetType_Default <- "classification"

  # Feature Tuning
  CatBoost_FeatureTune_Default <- FALSE
  CatBoost_LeaveOneOut <- 1

  # Save Plotting Output
  CatBoost_SavePlotOutput_Default <- FALSE

  # Production
  CatBoost_task_type_Default <- "GPU"
  CatBoost_NumGPUs_Default <- 1

  # CatBoost Classifier Args ----

  # Services
  CatBoost_ReturnServices_Default <- c("Importances","EvalPlots","EvalMetrics","Score_TrainData")

  # Loss function
  CatBoost_ClassWeights_Default <- c(1L, 1L)
  CatBoost_CostMatrixWeights_Default <- c(1L, 0L, 0L, 1L)
  CatBoost_EvalMetric_Default <- "MCC"
  CatBoost_grid_eval_metric_Default <- "MCC"
  CatBoost_LossFunction_Default <- "Logloss"
  CatBoost_MetricPeriods_Default <- 10
  CatBoost_NumOfParDepPlots_Default <- 0

  # ML Grid Tuning
  CatBoost_GridTune_Default <- FALSE
  CatBoost_MaxModelsInGrid_Default <- 30L
  CatBoost_MaxRunsWithoutNewWinner_Default <- 20L
  CatBoost_MaxRunMinutes_Default <- 24L*60L
  CatBoost_LeaveOneOut_Default <- FALSE
  CatBoost_TrainOnFull_Default <- FALSE
  CatBoost_UseGrid_Default <- FALSE

  # ML Tunable Args
  CatBoost_Trees_Default <- 100
  CatBoost_Tree_Tuning_Default <- seq(100,500,100)
  CatBoost_Depth_Default <- 9
  CatBoost_Depth_Tuning_Default <- seq(4,10,1)
  CatBoost_LearningRate_Default <- NULL
  CatBoost_LearningRate_Tuning_Default <- seq(0.05,0.50,0.05)
  CatBoost_L2_Leaf_Reg_Default <- NULL
  CatBoost_L2_Leaf_Reg_Tuning_Default <- seq(0.0,5.0,1.0)
  CatBoost_RandomStrength_Default <- 1
  CatBoost_RandomStrength_Tuning_Default <- seq(0.80,1,0.05)
  CatBoost_BorderCount_Default <- 254
  CatBoost_BorderCount_Tuning_Default <- seq(32,256,32)
  CatBoost_RSM_Default <- 1.0
  CatBoost_RSM_Tuning_Default <- seq(0.10,1.0,0.10)
  CatBoost_BootStrapType_Default <- "MVS"
  CatBoost_BootStrapType_Tuning_Default <- c("MVS","Bayesian","Bernoulli","No")
  CatBoost_GrowPolicy_Default <- "SymmetricTree"
  CatBoost_GrowPolicy_Tuning_Default <- c("SymmetricTree", "Lossguide", "Depthwise")

  # ML Non-Tunable Args
  CatBoost_model_size_reg_Default <- 0.5
  CatBoost_langevin_Default <- FALSE
  CatBoost_diffusion_temperature_Default <- 10000
  CatBoost_feature_border_type_Default <- "GreedyLogSum"
  CatBoost_sampling_unit_Default <- "Object"
  CatBoost_subsample_Default <- NULL
  CatBoost_score_function_Default <- "Cosine"
  CatBoost_min_data_in_leaf_Default <- 1

  # Admin: OptParse() Setup ----
  OptionList <- list(

    # Administration
    optparse::make_option(opt_str = "--ProjectID", type = "character", default = ProjectID_Default, help = "Project ID"),
    optparse::make_option(opt_str = "--Root_Path", type = "character", default = Root_Path_Default, help = "Root directory"),
    optparse::make_option(opt_str = "--Algorithm", type = "character", default = Algorithm_Default, help = "Target variable type"),

    # Return Services
    optparse::make_option(opt_str = "--CatBoost_ReturnServices", type = "character", default = CatBoost_ReturnServices_Default, help = "Content to include in the return list from model"),

    # ML Strategy
    optparse::make_option(opt_str = "--CatBoost_TargetType", type = "character", default = CatBoost_TargetType_Default, help = "Target variable type"),
    optparse::make_option(opt_str = "--CatBoost_SavePlotOutput", type = "logical", default = CatBoost_SavePlotOutput_Default, help = "Save output to file or not"),
    optparse::make_option(opt_str = "--CatBoost_FeatureTune", type = "logical", default = CatBoost_FeatureTune_Default, help = "Run feature tuning service"),
    optparse::make_option(opt_str = "--CatBoost_LeaveOneOut", type = "numeric", default = CatBoost_LeaveOneOut_Default, help = "Max number of feature tuning iterations"),
    optparse::make_option(opt_str = "--CatBoost_task_type", type = "character", default = CatBoost_task_type_Default, help = "Train with GPU or CPU"),
    optparse::make_option(opt_str = "--CatBoost_NumGPUs", type = "numeric", default = CatBoost_NumGPUs_Default, help = "Number of GPUs to utilize"),

    # ML Grid Tune
    optparse::make_option(opt_str = "--CatBoost_GridTune", type = "logical", default = CatBoost_GridTune_Default, help = "Run grid tuning service"),
    optparse::make_option(opt_str = "--CatBoost_MaxModelsInGrid", type = "numeric", default = CatBoost_MaxModelsInGrid_Default, help = "Max number of models to test"),
    optparse::make_option(opt_str = "--CatBoost_MaxRunsWithoutNewWinner", type = "numeric", default = CatBoost_MaxRunsWithoutNewWinner_Default, help = "Max runs without generating a new winner"),
    optparse::make_option(opt_str = "--CatBoost_MaxRunMinutes", type = "numeric", default = CatBoost_MaxRunMinutes_Default, help = "Max run time in minutes"),
    optparse::make_option(opt_str = "--CatBoost_TrainOnFull", type = "logical", default = CatBoost_TrainOnFull_Default, help = "Use full data to train or not"),
    optparse::make_option(opt_str = "--CatBoost_UseGrid", type = "logical", default = CatBoost_UseGrid_Default, help = "Use grid"),

    # ML Eval
    optparse::make_option(opt_str = "--CatBoost_ClassWeights", type = "numeric", default = CatBoost_ClassWeights_Default, help = "Positive weight and negative weight, e.g. c(1,1)"),
    optparse::make_option(opt_str = "--CatBoost_CostMatrixWeights", type = "numeric", default = CatBoost_CostMatrixWeights_Default, help = "TP profit, FP profit, FN profit, TN profit"),
    optparse::make_option(opt_str = "--CatBoost_EvalMetric", type = "character", default = CatBoost_EvalMetric_Default, help = "Evaluation metric for model"),
    optparse::make_option(opt_str = "--CatBoost_grid_eval_metric", type = "character", default = CatBoost_grid_eval_metric_Default, help = "Eval metric for grid tuning"),
    optparse::make_option(opt_str = "--CatBoost_LossFunction", type = "character", default = CatBoost_LossFunction_Default, help = "Loss function for model"),
    optparse::make_option(opt_str = "--CatBoost_MetricPeriods", type = "numeric", default = CatBoost_MetricPeriods_Default, help = "Number of trees to build between evaluations"),
    optparse::make_option(opt_str = "--CatBoost_NumOfParDepPlots", type = "numeric", default = CatBoost_NumOfParDepPlots_Default, help = "Number of partial dependence plots to build"),

    # ML Tunable Args
    optparse::make_option(opt_str = "--CatBoost_Trees", type = "numeric", default = CatBoost_Trees_Default, help = "Number of trees for model"),
    optparse::make_option(opt_str = "--CatBoost_Tree_Tuning", type = "numeric", default = CatBoost_Tree_Tuning_Default, help = "Sequence of trees for grid tuning"),
    optparse::make_option(opt_str = "--CatBoost_Depth", type = "numeric", default = CatBoost_Depth_Default, help = "Tree depth for model"),
    optparse::make_option(opt_str = "--CatBoost_Depth_Tuning", type = "numeric", default = CatBoost_Depth_Tuning_Default, help = "Tree depth sequence for grid tuning"),
    optparse::make_option(opt_str = "--CatBoost_LearningRate", type = "numeric", default = CatBoost_LearningRate_Default, help = "Learning rate for model"),
    optparse::make_option(opt_str = "--CatBoost_LearningRate_Tuning", type = "numeric", default = CatBoost_LearningRate_Tuning_Default, help = "Learning rate sequence for grid tuning"),
    optparse::make_option(opt_str = "--CatBoost_L2_Leaf_Reg", type = "numeric", default = CatBoost_L2_Leaf_Reg_Default, help = "L2 regularization for model"),
    optparse::make_option(opt_str = "--CatBoost_L2_Leaf_Reg_Tuning", type = "numeric", default = CatBoost_L2_Leaf_Reg_Tuning_Default, help = "L2 regularization sequence for grid tuning"),
    optparse::make_option(opt_str = "--CatBoost_RandomStrength", type = "numeric", default = CatBoost_RandomStrength_Default, help = "Random strength for model"),
    optparse::make_option(opt_str = "--CatBoost_RandomStrength_Tuning", type = "numeric", default = CatBoost_RandomStrength_Tuning_Default, help = "Random strength sequence for grid tuning"),
    optparse::make_option(opt_str = "--CatBoost_BorderCount", type = "numeric", default = CatBoost_BorderCount_Default, help = "Border count for model"),
    optparse::make_option(opt_str = "--CatBoost_BorderCount_Tuning", type = "numeric", default = CatBoost_BorderCount_Tuning_Default, help = "Border count sequence for grid tuning"),
    optparse::make_option(opt_str = "--CatBoost_RSM", type = "numeric", default = CatBoost_RSM_Default, help = "Random subspace method for model"),
    optparse::make_option(opt_str = "--CatBoost_RSM_Tuning", type = "numeric", default = CatBoost_RSM_Tuning_Default, help = "Random subspace method sequence for grid tuning"),
    optparse::make_option(opt_str = "--CatBoost_BootStrapType", type = "character", default = CatBoost_BootStrapType_Default, help = "Bootstrap type for model"),
    optparse::make_option(opt_str = "--CatBoost_BootStrapType_Tuning", type = "character", default = CatBoost_BootStrapType_Tuning_Default, help = "Bootstrap type sequence for grid tuning"),
    optparse::make_option(opt_str = "--CatBoost_GrowPolicy", type = "character", default = CatBoost_GrowPolicy_Default, help = "Grow policy for model"),
    optparse::make_option(opt_str = "--CatBoost_GrowPolicy_Tuning", type = "character", default = CatBoost_GrowPolicy_Tuning_Default, help = "Grow policy sequence for grid tuning"),

    # ML Non-Tunable Args
    optparse::make_option(opt_str = "--CatBoost_model_size_reg", type = "numeric", default = CatBoost_model_size_reg_Default, help = "Overall model size regularization"),
    optparse::make_option(opt_str = "--CatBoost_langevin", type = "logical", default = CatBoost_langevin_Default, help = "Stochastic gradient langevin boosting mode"),
    optparse::make_option(opt_str = "--CatBoost_diffusion_temperature", type = "numeric", default = CatBoost_diffusion_temperature_Default, help = "Diffusion temperature used with langevin"),
    optparse::make_option(opt_str = "--CatBoost_feature_border_type", type = "character", default = CatBoost_feature_border_type_Default, help = "Quantization mode"),
    optparse::make_option(opt_str = "--CatBoost_sampling_unit", type = "character", default = CatBoost_sampling_unit_Default, help = "Sampling scheme"),
    optparse::make_option(opt_str = "--CatBoost_subsample", type = "numeric", default = CatBoost_subsample_Default, help = "Can be used with MVS, Poisson and Bernoulli"),
    optparse::make_option(opt_str = "--CatBoost_score_function", type = "character", default = CatBoost_score_function_Default, help = "Score type for splitting"),
    optparse::make_option(opt_str = "--CatBoost_min_data_in_leaf", type = "numeric", default = CatBoost_min_data_in_leaf_Default, help = "Minimum rows per leaf")
  )

  # Admin: Store Args
  obj <- optparse::OptionParser(option_list = OptionList, add_help_option = TRUE)
  CL_ArgsList_ML <- optparse::parse_args(object = obj)

  # Load _FE args list
  load(file = file.path(CL_ArgsList_ML$Root_Path, "MetaData", "ArgsList.Rdata"))

  # High Level ----
  ArgsList$Modeling[["Algorithm"]] <- CL_ArgsList_ML[["Algorithm"]]

  # ML General Strategy ----
  ArgsList$Modeling$CatBoost[["TargetType"]] <- CL_ArgsList_ML[["CatBoost_TargetType"]]
  ArgsList$Modeling$CatBoost[["FeatureTune"]] <- CL_ArgsList_ML[["CatBoost_FeatureTune"]]
  ArgsList$Modeling$CatBoost[["LeaveOneOut"]] <- CL_ArgsList_ML[["CatBoost_LeaveOneOut"]]
  ArgsList$Modeling$CatBoost[["UseGrid"]] <- CL_ArgsList_ML[["CatBoost_UseGrid"]]

  # CatBoost Args ----

  # Return Services
  ArgsList$Modeling$CatBoost[["CatBoost_ReturnServices"]] <- RemixAutoML:::ParseOptParse(CL_ArgsList_ML[["CatBoost_ReturnServices"]])

  # Production Args
  ArgsList$Modeling$CatBoost[["task_type"]] <- CL_ArgsList_ML[["CatBoost_task_type"]]
  ArgsList$Modeling$CatBoost[["NumGPUs"]] <- CL_ArgsList_ML[["CatBoost_NumGPUs"]]

  # ML Grid Tune Args
  ArgsList$Modeling$CatBoost[["GridTune"]] <- CL_ArgsList_ML[["CatBoost_GridTune"]]
  ArgsList$Modeling$CatBoost[["MaxModelsInGrid"]] <- CL_ArgsList_ML[["CatBoost_MaxModelsInGrid"]]
  ArgsList$Modeling$CatBoost[["MaxRunsWithoutNewWinner"]] <- CL_ArgsList_ML[["CatBoost_MaxRunsWithoutNewWinner"]]
  ArgsList$Modeling$CatBoost[["MaxRunMinutes"]] <- CL_ArgsList_ML[["CatBoost_MaxRunMinutes"]]
  ArgsList$Modeling$CatBoost[["TrainOnFull"]] <- CL_ArgsList_ML[["CatBoost_TrainOnFull"]]

  # ML Eval
  ArgsList$Modeling$CatBoost[["ClassWeights"]] <- CL_ArgsList_ML[["CatBoost_ClassWeights"]]
  ArgsList$Modeling$CatBoost[["CostMatrixWeights"]] <- CL_ArgsList_ML[["CatBoost_CostMatrixWeights"]]
  ArgsList$Modeling$CatBoost[["EvalMetric"]] <- CL_ArgsList_ML[["CatBoost_EvalMetric"]]
  ArgsList$Modeling$CatBoost[["grid_eval_metric"]] <- CL_ArgsList_ML[["CatBoost_grid_eval_metric"]]
  ArgsList$Modeling$CatBoost[["LossFunction"]] <- CL_ArgsList_ML[["CatBoost_LossFunction"]]
  ArgsList$Modeling$CatBoost[["MetricPeriods"]] <- CL_ArgsList_ML[["CatBoost_MetricPeriods"]]
  ArgsList$Modeling$CatBoost[["NumOfParDepPlots"]] <- CL_ArgsList_ML[["CatBoost_NumOfParDepPlots"]]

  # ML Tunable Args
  ArgsList$Modeling$CatBoost[["Trees"]] <- CL_ArgsList_ML[["CatBoost_Trees"]]
  ArgsList$Modeling$CatBoost[["Tree_Tuning"]] <- CL_ArgsList_ML[["CatBoost_Tree_Tuning"]]
  ArgsList$Modeling$CatBoost[["Depth"]] <- CL_ArgsList_ML[["CatBoost_Depth"]]
  ArgsList$Modeling$CatBoost[["Depth_Tuning"]] <- CL_ArgsList_ML[["CatBoost_Depth_Tuning"]]
  ArgsList$Modeling$CatBoost[["LearningRate"]] <- CL_ArgsList_ML[["CatBoost_LearningRate"]]
  ArgsList$Modeling$CatBoost[["LearningRate_Tuning"]] <- CL_ArgsList_ML[["CatBoost_LearningRate_Tuning"]]
  ArgsList$Modeling$CatBoost[["L2_Leaf_Reg"]] <- CL_ArgsList_ML[["CatBoost_L2_Leaf_Reg"]]
  ArgsList$Modeling$CatBoost[["L2_Leaf_Reg_Tuning"]] <- CL_ArgsList_ML[["CatBoost_L2_Leaf_Reg_Tuning"]]
  ArgsList$Modeling$CatBoost[["RandomStrength"]] <- CL_ArgsList_ML[["CatBoost_RandomStrength"]]
  ArgsList$Modeling$CatBoost[["RandomStrength_Tuning"]] <- CL_ArgsList_ML[["CatBoost_RandomStrength_Tuning"]]
  ArgsList$Modeling$CatBoost[["BorderCount"]] <- CL_ArgsList_ML[["CatBoost_BorderCount"]]
  ArgsList$Modeling$CatBoost[["BorderCount_Tuning"]] <- CL_ArgsList_ML[["CatBoost_BorderCount_Tuning"]]
  ArgsList$Modeling$CatBoost[["RSM"]] <- CL_ArgsList_ML[["CatBoost_RSM"]]
  ArgsList$Modeling$CatBoost[["RSM_Tuning"]] <- CL_ArgsList_ML[["CatBoost_RSM_Tuning"]]
  ArgsList$Modeling$CatBoost[["BootStrapType"]] <- CL_ArgsList_ML[["CatBoost_BootStrapType"]]
  ArgsList$Modeling$CatBoost[["BootStrapType_Tuning"]] <- CL_ArgsList_ML[["CatBoost_BootStrapType_Tuning"]]
  ArgsList$Modeling$CatBoost[["GrowPolicy"]] <- CL_ArgsList_ML[["CatBoost_GrowPolicy"]]
  ArgsList$Modeling$CatBoost[["GrowPolicy_Tuning"]] <- CL_ArgsList_ML[["CatBoost_GrowPolicy_Tuning"]]

  # ML Non-Tunable Args
  ArgsList$Modeling$CatBoost[["model_size_reg"]] <- CL_ArgsList_ML[["CatBoost_model_size_reg"]]
  ArgsList$Modeling$CatBoost[["langevin"]] <- CL_ArgsList_ML[["CatBoost_langevin"]]
  ArgsList$Modeling$CatBoost[["diffusion_temperature"]] <- CL_ArgsList_ML[["CatBoost_diffusion_temperature"]]
  ArgsList$Modeling$CatBoost[["feature_border_type"]] <- CL_ArgsList_ML[["CatBoost_feature_border_type"]]
  ArgsList$Modeling$CatBoost[["sampling_unit"]] <- CL_ArgsList_ML[["CatBoost_sampling_unit"]]
  ArgsList$Modeling$CatBoost[["subsample"]] <- CL_ArgsList_ML[["CatBoost_subsample"]]
  ArgsList$Modeling$CatBoost[["score_function"]] <- CL_ArgsList_ML[["CatBoost_score_function"]]
  ArgsList$Modeling$CatBoost[["min_data_in_leaf"]] <- CL_ArgsList_ML[["CatBoost_min_data_in_leaf"]]

  # Dynamic args - change within process
  CatBoost_FeatureTune <- ArgsList$Modeling$CatBoost[["FeatureTune"]]
  CatBoost_GridTune <- ArgsList$Modeling$CatBoost[["GridTune"]]
  CatBoost_UseGrid <- ArgsList$Modeling$CatBoost[["UseGrid"]]
  CatBoost_LeaveOneOut <- ArgsList$Modeling$CatBoost[["LeaveOneOut"]]
  CatBoost_TrainOnFull <- ArgsList$Modeling$CatBoost[["TrainOnFull"]]
  CatBoost_SavePDFOutput <- FALSE

  # Remove CL args
  save(CL_ArgsList_ML, file = file.path(ArgsList$MetaData$MetaData_Path, "CL_ArgsList_ML.Rdata"))
  rm(CL_ArgsList_ML)
}

# Classification Model optimization ----
if(tolower(ArgsList$Modeling$CatBoost$TargetType) == "classification" && tolower(ArgsList$Modeling$Algorithm) == "catboost") {

  # Main loop
  Counter <- 0L
  repeat {

    # Print progress
    Counter <- Counter + 1L
    print(paste0("Total Runs: ", Counter))

    # Get args for run type
    Args <- RemixAutoML:::GenerateArgs(
      ArgsList = ArgsList, FeatureTuning = CatBoost_FeatureTune, GridTuning = CatBoost_GridTune,
      Trees = ArgsList$Modeling$CatBoost[["Trees"]], Tree_Tuning = ArgsList$Modeling$CatBoost[["Tree_Tuning"]],
      Depth = ArgsList$Modeling$CatBoost[["Depth"]], Depth_Tuning = ArgsList$Modeling$CatBoost[["Depth_Tuning"]],
      LearningRate = ArgsList$Modeling$CatBoost[["LearningRate"]], LearningRate_Tuning = ArgsList$Modeling$CatBoost[["LearningRate_Tuning"]],
      L2_Leaf_Reg = ArgsList$Modeling$CatBoost[["L2_Leaf_Reg"]], L2_Leaf_Reg_Tuning = ArgsList$Modeling$CatBoost[["L2_Leaf_Reg_Tuning"]],
      RandomStrength = ArgsList$Modeling$CatBoost[["RandomStrength"]], RandomStrength_Tuning = ArgsList$Modeling$CatBoost[["RandomStrength_Tuning"]],
      BorderCount = ArgsList$Modeling$CatBoost[["BorderCount"]], BorderCount_Tuning = ArgsList$Modeling$CatBoost[["BorderCount_Tuning"]],
      RSM = ArgsList$Modeling$CatBoost[["RSM"]], RSM_Tuning = ArgsList$Modeling$CatBoost[["RSM_Tuning"]],
      BootStrapType = ArgsList$Modeling$CatBoost[["BootStrapType"]], BootStrapType_Tuning = ArgsList$Modeling$CatBoost[["BootStrapType_Tuning"]],
      GrowPolicy = ArgsList$Modeling$CatBoost[["GrowPolicy"]], GrowPolicy_Tuning = ArgsList$Modeling$CatBoost[["GrowPolicy_Tuning"]])

    # Run procedures
    # Run = 1
    for(Run in Args$Loop) {

      # Load data sets
      TrainData <- data.table::fread(file = file.path(ArgsList$MetaData$Data_Path, "TrainData.csv"), colClasses = ArgsList$MetaData$ML_Train_DtaColumnTypes)
      ValidationData <- data.table::fread(file = file.path(ArgsList$MetaData$Data_Path, "ValidationData.csv"), colClasses = ArgsList$MetaData$ML_Train_DtaColumnTypes)
      TestData <- data.table::fread(file = file.path(ArgsList$MetaData$Data_Path, "TestData.csv"), colClasses = ArgsList$MetaData$ML_Train_DtaColumnTypes)

      # Stack data sets if TrainOnFull
      if(ArgsList$Modeling$CatBoost[["TrainOnFull"]]) {
        TrainData <- data.table::rbindlist(TrainData, ValidationData, TestData)
        ValidationData <- NULL
        TestData <- NULL
        TOF <- TRUE
      } else {
        TOF <- FALSE
      }

      # Prep time
      Output <- RemixAutoML:::FeaturePrep(RunNumber=Run, Loop=Args$Loop, FeatureTune.=CatBoost_FeatureTune, GridTune.=CatBoost_GridTune, UseGrid.=CatBoost_UseGrid, ArgsList=ArgsList, TrainData.=TrainData)
      PassInGrid. <- Output[["PassInGrid"]]
      Features <- Output[["Features"]]
      IDcols. <- Output[["IDcols"]]; rm(Output)

      # Run CatBoost Model
      Results <- RemixAutoML::AutoCatBoostClassifier(

        # Return services
        OutputSelection = ArgsList$Modeling$CatBoost[["CatBoost_ReturnServices"]],

        # GPU or CPU and the number of available GPUs
        task_type = ArgsList$Modeling$CatBoost[["task_type"]],
        NumGPUs = ArgsList$Modeling$CatBoost[["NumGPUs"]],
        TrainOnFull = TOF,
        PassInGrid = PassInGrid.,
        DebugMode = TRUE,

        # Metadata args
        ModelID = paste0(ArgsList$MetaData[["ProjectID"]], "_AutoCatBoostClassifier"),
        model_path = ArgsList$MetaData[["Model_Path"]],
        metadata_path = ArgsList$MetaData[["Results_Path"]],
        SaveModelObjects = if(CatBoost_FeatureTune) FALSE else TRUE,
        SaveInfoToPDF = if(CatBoost_FeatureTune || CatBoost_SavePDFOutput) FALSE else TRUE,
        ReturnModelObjects = TRUE,

        # Data args
        data = TrainData,
        ValidationData = ValidationData,
        TestData = TestData,
        TargetColumnName = ArgsList$Data[["TargetVariables"]],
        FeatureColNames = Features,
        PrimaryDateColumn = ArgsList$Data[["DateVariables"]][1L],
        IDcols = IDcols.,

        # Evaluation args
        ClassWeights = ArgsList$Modeling$CatBoost[["ClassWeights"]],
        CostMatrixWeights = ArgsList$Modeling$CatBoost[["CostMatrixWeights"]],
        EvalMetric = ArgsList$Modeling$CatBoost[["EvalMetric"]],
        grid_eval_metric = ArgsList$Modeling$CatBoost[["grid_eval_metric"]],
        LossFunction = ArgsList$Modeling$CatBoost[["LossFunction"]],
        MetricPeriods = ArgsList$Modeling$CatBoost[["MetricPeriods"]],
        NumOfParDepPlots = ArgsList$Modeling$CatBoost[["NumOfParDepPlots"]],

        # Grid tuning args
        GridTune = CatBoost_GridTune,
        MaxModelsInGrid = ArgsList$Modeling$CatBoost[["MaxModelsInGrid"]],
        MaxRunsWithoutNewWinner = ArgsList$Modeling$CatBoost[["MaxRunsWithoutNewWinner"]],
        MaxRunMinutes = ArgsList$Modeling$CatBoost[["MaxRunMinutes"]],
        BaselineComparison = "default",

        # ML args
        Trees = Args[["Trees"]],
        Depth = Args[["Depth"]],
        LearningRate = Args[["LearningRate"]],
        L2_Leaf_Reg = Args[["L2_Leaf_Reg"]],
        RandomStrength = Args[["RandomStrength"]],
        BorderCount = Args[["BorderCount"]],
        RSM = Args[["RSM"]],
        BootStrapType = Args[["BootStrapType"]],
        GrowPolicy = Args[["GrowPolicy"]],

        # Other ML args
        model_size_reg = ArgsList$Modeling$CatBoost[["model_size_reg"]],
        langevin = ArgsList$Modeling$CatBoost[["langevin"]],
        diffusion_temperature = ArgsList$Modeling$CatBoost[["diffusion_temperature"]],
        feature_border_type = ArgsList$Modeling$CatBoost[["feature_border_type"]],
        sampling_unit = ArgsList$Modeling$CatBoost[["sampling_unit"]],
        subsample = ArgsList$Modeling$CatBoost[["subsample"]],
        score_function = ArgsList$Modeling$CatBoost[["score_function"]],
        min_data_in_leaf = ArgsList$Modeling$CatBoost[["min_data_in_leaf"]])

      # Output
      if(CatBoost_FeatureTune) {
        TreeCount <- Results$Model$tree_count
        EvalMetrics <- Results[["EvaluationMetrics"]]
        EvalMetrics[, TreeCount := eval(TreeCount)]
        EvalMetrics[, RunNumber := eval(Run)]
        data.table::fwrite(EvalMetrics, file = file.path(ArgsList$MetaData[["Results_Path"]], paste0(ArgsList[["ProjectID"]], "_FeatureTuningMetrics.csv")), append = TRUE)
      }
    }

    # Feature tuning results
    Output <- RemixAutoML:::ML_TunePath(Results.=Results, ArgsList=ArgsList, FeatureTune.=CatBoost_FeatureTune, GridTune.=CatBoost_GridTune, LeaveOneOut.=CatBoost_LeaveOneOut, Metric="Utility")

    # Continue or stop?
    if(CatBoost_FeatureTune && !identical(ServicesCheck, ArgsList$Services) && !ArgsList$Services == "BaseColumns") {
      print("Still going")
    } else if(CatBoost_GridTune) {
      CatBoost_FeatureTune <- FALSE
    } else {
      break
    }
  }
}
