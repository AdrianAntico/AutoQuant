# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
# :: CatBoost MultiClass Training ::
#
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Environment Setup ----
if("Args Setup" == "Args Setup") {

  # Project
  ProjectID_Default <- "Sample_Project"
  Root_Path_Default <- "C:/Users/Bizon/Documents/GitHub/QA_Code/Sample_Project"

  # Target Type
  TargetType_Default <- "multiclass"

  # Feature Tuning
  FeatureTune_Default <- TRUE
  LeaveOneOut <- 1

  # Save Plotting Output
  SavePlotOutput_Default <- FALSE

  # Production
  task_type_Default <- "GPU"
  NumGPUs_Default <- 1

  # CatBoost Regression Args ----

  # Services
  ReturnServices_Default <- c("Importances","EvalPlots","EvalMetrics","Score_TrainData")

  # Loss function
  ClassWeights_Default <- c(1L, 1L)
  eval_metric_Default <- "MCC"
  loss_function_Default <- "MultiClassOneVsAll"

  grid_eval_metric_Default <- "mse"
  MetricPeriods_Default <- 10
  NumOfParDepPlots_Default <- 0

  # ML Grid Tuning
  GridTune_Default <- FALSE
  MaxModelsInGrid_Default <- 30L
  MaxRunsWithoutNewWinner_Default <- 20L
  MaxRunMinutes_Default <- 24L*60L
  LeaveOneOut_Default <- TRUE
  TrainOnFull_Default <- FALSE
  UseGrid_Default <- FALSE

  # ML Tunable Args
  Trees_Default <- 100
  Tree_Tuning_Default <- seq(100,500,100)
  Depth_Default <- 9
  Depth_Tuning_Default <- seq(4,10,1)
  LearningRate_Default <- NULL
  LearningRate_Tuning_Default <- seq(0.05,0.50,0.05)
  L2_Leaf_Reg_Default <- NULL
  L2_Leaf_Reg_Tuning_Default <- seq(0.0,5.0,1.0)
  RandomStrength_Default <- 1
  RandomStrength_Tuning_Default <- seq(0.80,1,0.05)
  BorderCount_Default <- 254
  BorderCount_Tuning_Default <- seq(32,256,32)
  RSM_Default <- 1.0
  RSM_Tuning_Default <- seq(0.10,1.0,0.10)
  BootStrapType_Default <- "MVS"
  BootStrapType_Tuning_Default <- c("MVS","Bayesian","Bernoulli","No")
  GrowPolicy_Default <- "SymmetricTree"
  GrowPolicy_Tuning_Default <- c("SymmetricTree", "Lossguide", "Depthwise")

  # ML Non-Tunable Args
  model_size_reg_Default <- 0.5
  langevin_Default <- FALSE
  diffusion_temperature_Default <- 10000
  feature_border_type_Default <- "GreedyLogSum"
  sampling_unit_Default <- "Object"
  subsample_Default <- NULL
  score_function_Default <- "Cosine"
  min_data_in_leaf_Default <- 1

  # Admin: OptParse() Setup ----
  OptionList <- list(

    # Administration
    optparse::make_option(opt_str = "--ProjectID", type = "character", default = ProjectID_Default, help = "Project ID"),
    optparse::make_option(opt_str = "--Root_Path", type = "character", default = Root_Path_Default, help = "Root directory"),

    # Return Services
    optparse::make_option(opt_str = "--ReturnServices", type = "character", default = ReturnServices_Default, help = "Content to include in the return list from model"),

    # ML Strategy
    optparse::make_option(opt_str = "--TargetType", type = "character", default = TargetType_Default, help = "Target variable type"),
    optparse::make_option(opt_str = "--SavePlotOutput", type = "logical", default = SavePlotOutput_Default, help = "Save output to file or not"),
    optparse::make_option(opt_str = "--FeatureTune", type = "logical", default = FeatureTune_Default, help = "Run feature tuning service"),
    optparse::make_option(opt_str = "--LeaveOneOut", type = "numeric", default = LeaveOneOut_Default, help = "Max number of feature tuning iterations"),
    optparse::make_option(opt_str = "--task_type", type = "character", default = task_type_Default, help = "Train with GPU or CPU"),
    optparse::make_option(opt_str = "--NumGPUs", type = "numeric", default = NumGPUs_Default, help = "Number of GPUs to utilize"),

    # ML Grid Tune
    optparse::make_option(opt_str = "--GridTune", type = "logical", default = GridTune_Default, help = "Run grid tuning service"),
    optparse::make_option(opt_str = "--MaxModelsInGrid", type = "numeric", default = MaxModelsInGrid_Default, help = "Max number of models to test"),
    optparse::make_option(opt_str = "--MaxRunsWithoutNewWinner", type = "numeric", default = MaxRunsWithoutNewWinner_Default, help = "Max runs without generating a new winner"),
    optparse::make_option(opt_str = "--MaxRunMinutes", type = "numeric", default = MaxRunMinutes_Default, help = "Max run time in minutes"),
    optparse::make_option(opt_str = "--TrainOnFull", type = "logical", default = TrainOnFull_Default, help = "Use full data to train or not"),
    optparse::make_option(opt_str = "--UseGrid", type = "logical", default = UseGrid_Default, help = "Use grid"),

    # ML Eval
    optparse::make_option(opt_str = "--ClassWeights", type = "numeric", default = ClassWeights_Default, help = "Positive weight and negative weight, e.g. c(1,1)"),
    optparse::make_option(opt_str = "--eval_metric", type = "character", default = eval_metric_Default, help = "Eval metric for training"),
    optparse::make_option(opt_str = "--loss_function", type = "character", default = loss_function_Default, help = "Loss function for training"),
    optparse::make_option(opt_str = "--grid_eval_metric", type = "character", default = grid_eval_metric_Default, help = "Eval metric for grid tuning"),
    optparse::make_option(opt_str = "--MetricPeriods", type = "numeric", default = MetricPeriods_Default, help = "Number of trees to build between evaluations"),
    optparse::make_option(opt_str = "--NumOfParDepPlots", type = "numeric", default = NumOfParDepPlots_Default, help = "Number of partial dependence plots to build"),

    # ML Tunable Args
    optparse::make_option(opt_str = "--Trees", type = "numeric", default = Trees_Default, help = "Number of trees for model"),
    optparse::make_option(opt_str = "--Tree_Tuning", type = "numeric", default = Tree_Tuning_Default, help = "Sequence of trees for grid tuning"),
    optparse::make_option(opt_str = "--Depth", type = "numeric", default = Depth_Default, help = "Tree depth for model"),
    optparse::make_option(opt_str = "--Depth_Tuning", type = "numeric", default = Depth_Tuning_Default, help = "Tree depth sequence for grid tuning"),
    optparse::make_option(opt_str = "--LearningRate", type = "numeric", default = LearningRate_Default, help = "Learning rate for model"),
    optparse::make_option(opt_str = "--LearningRate_Tuning", type = "numeric", default = LearningRate_Tuning_Default, help = "Learning rate sequence for grid tuning"),
    optparse::make_option(opt_str = "--L2_Leaf_Reg", type = "numeric", default = L2_Leaf_Reg_Default, help = "L2 regularization for model"),
    optparse::make_option(opt_str = "--L2_Leaf_Reg_Tuning", type = "numeric", default = L2_Leaf_Reg_Tuning_Default, help = "L2 regularization sequence for grid tuning"),
    optparse::make_option(opt_str = "--RandomStrength", type = "numeric", default = RandomStrength_Default, help = "Random strength for model"),
    optparse::make_option(opt_str = "--RandomStrength_Tuning", type = "numeric", default = RandomStrength_Tuning_Default, help = "Random strength sequence for grid tuning"),
    optparse::make_option(opt_str = "--BorderCount", type = "numeric", default = BorderCount_Default, help = "Border count for model"),
    optparse::make_option(opt_str = "--BorderCount_Tuning", type = "numeric", default = BorderCount_Tuning_Default, help = "Border count sequence for grid tuning"),
    optparse::make_option(opt_str = "--RSM", type = "numeric", default = RSM_Default, help = "Random subspace method for model"),
    optparse::make_option(opt_str = "--RSM_Tuning", type = "numeric", default = RSM_Tuning_Default, help = "Random subspace method sequence for grid tuning"),
    optparse::make_option(opt_str = "--BootStrapType", type = "character", default = BootStrapType_Default, help = "Bootstrap type for model"),
    optparse::make_option(opt_str = "--BootStrapType_Tuning", type = "character", default = BootStrapType_Tuning_Default, help = "Bootstrap type sequence for grid tuning"),
    optparse::make_option(opt_str = "--GrowPolicy", type = "character", default = GrowPolicy_Default, help = "Grow policy for model"),
    optparse::make_option(opt_str = "--GrowPolicy_Tuning", type = "character", default = GrowPolicy_Tuning_Default, help = "Grow policy sequence for grid tuning"),

    # ML Non-Tunable Args
    optparse::make_option(opt_str = "--model_size_reg", type = "numeric", default = model_size_reg_Default, help = "Overall model size regularization"),
    optparse::make_option(opt_str = "--langevin", type = "logical", default = langevin_Default, help = "Stochastic gradient langevin boosting mode"),
    optparse::make_option(opt_str = "--diffusion_temperature", type = "numeric", default = diffusion_temperature_Default, help = "Diffusion temperature used with langevin"),
    optparse::make_option(opt_str = "--feature_border_type", type = "character", default = feature_border_type_Default, help = "Quantization mode"),
    optparse::make_option(opt_str = "--sampling_unit", type = "character", default = sampling_unit_Default, help = "Sampling scheme"),
    optparse::make_option(opt_str = "--subsample", type = "numeric", default = subsample_Default, help = "Can be used with MVS, Poisson and Bernoulli"),
    optparse::make_option(opt_str = "--score_function", type = "character", default = score_function_Default, help = "Score type for splitting"),
    optparse::make_option(opt_str = "--min_data_in_leaf", type = "numeric", default = min_data_in_leaf_Default, help = "Minimum rows per leaf")
  )

  # Admin: Store Args
  obj <- optparse::OptionParser(option_list = OptionList, add_help_option = TRUE)
  CL_ArgsList_ML <- optparse::parse_args(object = obj)

  # Load _FE args list
  load(file = file.path(CL_ArgsList_ML$Root_Path, "MetaData", "ArgsList.Rdata"))

  # High Level ----
  ArgsList$Modeling[["Algorithm"]] <- CL_ArgsList_ML[["Algorithm"]]

  # ML General Strategy ----
  ArgsList$Modeling$CatBoost[["TargetType"]] <- CL_ArgsList_ML[["TargetType"]]
  ArgsList$Modeling$CatBoost[["FeatureTune"]] <- CL_ArgsList_ML[["FeatureTune"]]
  ArgsList$Modeling$CatBoost[["LeaveOneOut"]] <- CL_ArgsList_ML[["LeaveOneOut"]]
  ArgsList$Modeling$CatBoost[["UseGrid"]] <- CL_ArgsList_ML[["UseGrid"]]

  # CatBoost Args ----

  # Return Services
  ArgsList$Modeling$CatBoost[["ReturnServices"]] <- RemixAutoML:::ParseOptParse(CL_ArgsList_ML[["ReturnServices"]])

  # Production Args
  ArgsList$Modeling$CatBoost[["task_type"]] <- CL_ArgsList_ML[["task_type"]]
  ArgsList$Modeling$CatBoost[["NumGPUs"]] <- CL_ArgsList_ML[["NumGPUs"]]

  # ML Grid Tune Args
  ArgsList$Modeling$CatBoost[["GridTune"]] <- CL_ArgsList_ML[["GridTune"]]
  ArgsList$Modeling$CatBoost[["MaxModelsInGrid"]] <- CL_ArgsList_ML[["MaxModelsInGrid"]]
  ArgsList$Modeling$CatBoost[["MaxRunsWithoutNewWinner"]] <- CL_ArgsList_ML[["MaxRunsWithoutNewWinner"]]
  ArgsList$Modeling$CatBoost[["MaxRunMinutes"]] <- CL_ArgsList_ML[["MaxRunMinutes"]]
  ArgsList$Modeling$CatBoost[["TrainOnFull"]] <- CL_ArgsList_ML[["TrainOnFull"]]

  # ML Eval
  ArgsList$Modeling$CatBoost[["ClassWeights"]] <- CL_ArgsList_ML[["ClassWeights"]]
  ArgsList$Modeling$CatBoost[["eval_metric"]] <- CL_ArgsList_ML[["eval_metric"]]
  ArgsList$Modeling$CatBoost[["loss_function"]] <- CL_ArgsList_ML[["loss_function"]]
  ArgsList$Modeling$CatBoost[["grid_eval_metric"]] <- CL_ArgsList_ML[["grid_eval_metric"]]
  ArgsList$Modeling$CatBoost[["MetricPeriods"]] <- CL_ArgsList_ML[["MetricPeriods"]]
  ArgsList$Modeling$CatBoost[["NumOfParDepPlots"]] <- CL_ArgsList_ML[["NumOfParDepPlots"]]

  # ML Tunable Args
  ArgsList$Modeling$CatBoost[["Trees"]] <- CL_ArgsList_ML[["Trees"]]
  ArgsList$Modeling$CatBoost[["Tree_Tuning"]] <- CL_ArgsList_ML[["Tree_Tuning"]]
  ArgsList$Modeling$CatBoost[["Depth"]] <- CL_ArgsList_ML[["Depth"]]
  ArgsList$Modeling$CatBoost[["Depth_Tuning"]] <- CL_ArgsList_ML[["Depth_Tuning"]]
  ArgsList$Modeling$CatBoost[["LearningRate"]] <- CL_ArgsList_ML[["LearningRate"]]
  ArgsList$Modeling$CatBoost[["LearningRate_Tuning"]] <- CL_ArgsList_ML[["LearningRate_Tuning"]]
  ArgsList$Modeling$CatBoost[["L2_Leaf_Reg"]] <- CL_ArgsList_ML[["L2_Leaf_Reg"]]
  ArgsList$Modeling$CatBoost[["L2_Leaf_Reg_Tuning"]] <- CL_ArgsList_ML[["L2_Leaf_Reg_Tuning"]]
  ArgsList$Modeling$CatBoost[["RandomStrength"]] <- CL_ArgsList_ML[["RandomStrength"]]
  ArgsList$Modeling$CatBoost[["RandomStrength_Tuning"]] <- CL_ArgsList_ML[["RandomStrength_Tuning"]]
  ArgsList$Modeling$CatBoost[["BorderCount"]] <- CL_ArgsList_ML[["BorderCount"]]
  ArgsList$Modeling$CatBoost[["BorderCount_Tuning"]] <- CL_ArgsList_ML[["BorderCount_Tuning"]]
  ArgsList$Modeling$CatBoost[["RSM"]] <- CL_ArgsList_ML[["RSM"]]
  ArgsList$Modeling$CatBoost[["RSM_Tuning"]] <- CL_ArgsList_ML[["RSM_Tuning"]]
  ArgsList$Modeling$CatBoost[["BootStrapType"]] <- CL_ArgsList_ML[["BootStrapType"]]
  ArgsList$Modeling$CatBoost[["BootStrapType_Tuning"]] <- CL_ArgsList_ML[["BootStrapType_Tuning"]]
  ArgsList$Modeling$CatBoost[["GrowPolicy"]] <- CL_ArgsList_ML[["GrowPolicy"]]
  ArgsList$Modeling$CatBoost[["GrowPolicy_Tuning"]] <- CL_ArgsList_ML[["GrowPolicy_Tuning"]]

  # ML Non-Tunable Args
  ArgsList$Modeling$CatBoost[["model_size_reg"]] <- CL_ArgsList_ML[["model_size_reg"]]
  ArgsList$Modeling$CatBoost[["langevin"]] <- CL_ArgsList_ML[["langevin"]]
  ArgsList$Modeling$CatBoost[["diffusion_temperature"]] <- CL_ArgsList_ML[["diffusion_temperature"]]
  ArgsList$Modeling$CatBoost[["feature_border_type"]] <- CL_ArgsList_ML[["feature_border_type"]]
  ArgsList$Modeling$CatBoost[["sampling_unit"]] <- CL_ArgsList_ML[["sampling_unit"]]
  ArgsList$Modeling$CatBoost[["subsample"]] <- CL_ArgsList_ML[["subsample"]]
  ArgsList$Modeling$CatBoost[["score_function"]] <- CL_ArgsList_ML[["score_function"]]
  ArgsList$Modeling$CatBoost[["min_data_in_leaf"]] <- CL_ArgsList_ML[["min_data_in_leaf"]]

  # Dynamic args - change within process
  FeatureTune <- ArgsList$Modeling$CatBoost[["FeatureTune"]]
  GridTune <- ArgsList$Modeling$CatBoost[["GridTune"]]
  UseGrid <- ArgsList$Modeling$CatBoost[["UseGrid"]]
  LeaveOneOut <- ArgsList$Modeling$CatBoost[["LeaveOneOut"]]
  TrainOnFull <- ArgsList$Modeling$CatBoost[["TrainOnFull"]]
  SavePDFOutput <- FALSE

  # Remove CL args
  save(CL_ArgsList_ML, file = file.path(ArgsList$MetaData$MetaData_Path, "CL_ArgsList_ML.Rdata"))
  rm(CL_ArgsList_ML)
}

# Main loop
Counter <- 0L
repeat {

  # Print progress
  Counter <- Counter + 1L
  print(paste0("Total Runs: ", Counter))

  # Get args for run type
  Args <- RemixAutoML:::GenerateArgs(
    ArgsList = ArgsList, FeatureTuning = FeatureTune, GridTuning = GridTune,
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
    Output <- RemixAutoML:::FeaturePrep(RunNumber=Run, Loop=Args$Loop, FeatureTune.=FeatureTune, GridTune.=GridTune, UseGrid.=UseGrid, ArgsList=ArgsList, TrainData.=TrainData)
    PassInGrid. <- Output[["PassInGrid"]]
    Features <- Output[["Features"]]
    IDcols. <- Output[["IDcols"]]; rm(Output)

    # Class Weights
    if(!is.null(ArgsList$Modeling$CatBoost[["ClassWeights"]])) {
      zz <- length(unique(TrainData[[ArgsList$Data$TargetVariables]]))
      if(zz != length(ArgsList$Modeling$CatBoost[["ClassWeights"]])) {
        ArgsList$Modeling$CatBoost[["ClassWeights"]] <- rep(1, zz)
      }
    }

    # Run CatBoost Model
    Results <- RemixAutoML::AutoCatBoostMultiClass(

      # Return services
      OutputSelection = if(FeatureTune) c("EvalMetrics") else ArgsList$Modeling$CatBoost[["ReturnServices"]],

      # GPU or CPU and the number of available GPUs
      task_type = ArgsList$Modeling$CatBoost[["task_type"]],
      NumGPUs = ArgsList$Modeling$CatBoost[["NumGPUs"]],
      TrainOnFull = TOF,
      PassInGrid = PassInGrid.,
      DebugMode = TRUE,

      # Metadata args
      ModelID = paste0(ArgsList$MetaData[["ProjectID"]], "_AutoCatBoostRegression"),
      model_path = ArgsList$MetaData[["Model_Path"]],
      metadata_path = ArgsList$MetaData[["Results_Path"]],
      SaveModelObjects = if(FeatureTune) FALSE else TRUE,
      ReturnModelObjects = TRUE,

      # Data args
      data = TrainData,
      ValidationData = ValidationData,
      TestData = TestData,
      TargetColumnName = ArgsList$Data[["TargetVariables"]],
      FeatureColNames = Features,
      PrimaryDateColumn = ArgsList$Data[["DateVariables"]][1L],
      WeightsColumnName = ArgsList$Data[["WeightsColumnName"]],
      IDcols = IDcols.,

      # Evaluation args
      ClassWeights = ArgsList$Modeling$CatBoost[["ClassWeights"]],
      eval_metric = ArgsList$Modeling$CatBoost[["eval_metric"]],
      loss_function = ArgsList$Modeling$CatBoost[["loss_function"]],
      grid_eval_metric = ArgsList$Modeling$CatBoost[["grid_eval_metric"]],
      MetricPeriods = ArgsList$Modeling$CatBoost[["MetricPeriods"]],
      NumOfParDepPlots = ArgsList$Modeling$CatBoost[["NumOfParDepPlots"]],

      # Grid tuning args
      GridTune = GridTune,
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
    if(FeatureTune) {
      TreeCount <- Results$Model$tree_count
      EvalMetrics <- Results$MultinomialMetrics$TestData
      EvalMetrics[, TreeCount := eval(TreeCount)]
      EvalMetrics[, RunNumber := eval(Run)]
      EvalMetrics[, RunTime := Sys.time()]
      data.table::fwrite(EvalMetrics, file = file.path(ArgsList$MetaData[["Results_Path"]], paste0(ArgsList$MetaData[["ProjectID"]], "_FeatureTuningMetrics.csv")), append = TRUE)
    }
  }

  # Feature tuning results
  Output <- RemixAutoML:::ML_TunePath(TargetType="multiclass", MetricName="MicroAUC", Results.=Results, ArgsList=ArgsList, FeatureTune.=FeatureTune, GridTune.=GridTune, LeaveOneOut.=LeaveOneOut)
  Output$LeaveOneOut

  # Continue or stop?
  if(FeatureTune && !identical(Args$ServicesCheck, ArgsList$Services) && !ArgsList$Services == "BaseColumns") {
    print("Still going")
  } else if(GridTune) {
    FeatureTune <- FALSE
  } else {
    break
  }
}
