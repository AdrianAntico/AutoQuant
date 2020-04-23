#' RPM_Binomial_Bandit
#'
#' RPM_Binomial_Bandit computes randomized probability matching probabilities for each arm being best in a multi-armed bandit. Close cousin to Thomson Sampling.
#'
#' @author Adrian Antico
#' @family Misc
#' @param Success Vector of successes. One slot per arm.
#' @param Trials Vector of trials. One slot per arm.
#' @param Alpha Prior parameter for success
#' @param Beta Prior parameter for trials
#' @param SubDivisions Default is 100L in the stats package. Changed it to 1000 for this function.
#' @return Probability of each arm being the best arm compared to all other arms.
#' @export 
RPM_Binomial_Bandit <- function(Success,
                                Trials,
                                Alpha = 1L,
                                Beta = 1L,
                                SubDivisions = 1000L) {
  k <- length(Success)
  ans <- numeric(k)
  for (i in (1:k)) {
    indx <- (1:k)[-i]
    CollapsedGibbsSampler <- function(z) {
      r <- stats::dbeta(z, Success[i] + Alpha, Trials[i] - Success[i] + Beta)
      for (j in indx) {
        r <- r * stats::pbeta(z, Success[j] + Alpha, Trials[j] - Success[j] + Beta)
      }
      return(r)
    }
    ans[i] <- stats::integrate(CollapsedGibbsSampler, 0, 1, subdivisions = SubDivisions)$value
  }
  return(ans)
}

#' RL_Initialize
#' 
#' RL_Initialize sets up the components necessary for RL
#'
#' @author Adrian Antico
#' @family Reinforcement Learning
#' @param ParameterGridSet This is a list of tuning grids
#' @param Alpha Prior successes
#' @param Beta Prior trials
#' @param SubDivisions Tolerance for integration
#' @examples 
#' RL_Start <- RL_Initialize(
#'   ParameterGridSet = GridClusters, 
#'   Alpha = Alpha, 
#'   Beta = Beta, 
#'   SubDivisions = 1000L)
#' BanditArmsN <- RL_Start[["BanditArmsN"]]
#' Successes <- RL_Start[["Successes"]]
#' Trials <- RL_Start[["Trials"]]
#' GridIDs <- RL_Start[["GridIDs"]]
#' BanditProbs <- RL_Start[["BanditProbs"]]
#' @export 
RL_Initialize <- function(ParameterGridSet = NULL, 
                          Alpha = 1L, 
                          Beta = 1L,
                          SubDivisions = 1000L) {
  
  # Initialization----
  BanditArmsN <- length(ParameterGridSet)
  Successes <- c(rep(0,BanditArmsN))
  Trials <- c(rep(0,BanditArmsN))
  GridIDs <- c(seq_len(BanditArmsN))
  BanditProbs <- RPM_Binomial_Bandit(
    Success = Successes, 
    Trials = Trials, 
    Alpha = Alpha, 
    Beta = Beta, 
    SubDivisions = SubDivisions)
  
  # Return----
  return(
    list(
      BanditProbs = BanditProbs,
      GridIDs = GridIDs,
      Trials = Trials,
      Successes = Successes,
      BanditArmsN = BanditArmsN))
}

#' RL_Update 
#' 
#' RL_Update updates the bandit probabilities for selecting different grids
#'
#' @author Adrian Antico
#' @family Reinforcement Learning
#' @param ExperimentGrid This is a data.table of grid params and model results
#' @param MetricSelection The chosen metric to evalute models
#' @param ModelRun Model iteration number
#' @param NEWGrid Previous grid passed in
#' @param TrialVector Numeric vector with the total trials for each arm
#' @param SuccessVector Numeric vector with the total successes for each arm
#' @param GridIDS The numeric vector that identifies which grid is which
#' @param BanditArmsCount The number of arms in the bandit
#' @param RunsWithoutNewWinner Counter of the number of models previously built without being a new winner
#' @param MaxRunsWithoutNewWinner Maximum number of models built without a new best model (constraint)
#' @param MaxNumberModels Maximum number of models to build (constraint)
#' @param MaxRunMinutes Run time constraint
#' @param TotalRunTime Cumulative run time in minutes
#' @param BanditProbabilities Inital probabilities from RL_Initialize()
#' @examples 
#' RL_Update_Output <- RL_Update(
#'   ExperimentGrid = ExperimentGrid,
#'   MetricSelection = MetricSelection,
#'   ModelRun = run,
#'   NEWGrid = NewGrid,
#'   TrialVector = Trials,
#'   SuccessVector = Successes,
#'   GridIDS = GridIDs,
#'   BanditArmsCount = BanditArmsN,
#'   RunsWithoutNewWinner = RunsWithoutNewWinner,
#'   MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
#'   MaxNumberModels = MaxNumberModels,
#'   MaxRunMinutes = MaxRunMinutes,
#'   TotalRunTime = TotalRunTime,
#'   BanditProbabilities = BanditProbs)
#' BanditProbs <- RL_Update_Output[["BanditProbs"]]
#' Trials <- RL_Update_Output[["Trials"]]
#' Successes <- RL_Update_Output[["Successes"]]
#' NewGrid <- RL_Update_Output[["NewGrid"]]
#' @export 
RL_Update <- function(ExperimentGrid = ExperimentGrid,
                      MetricSelection = MetricSelection,
                      ModelRun = run,
                      NEWGrid = NewGrid,
                      TrialVector = Trials,
                      SuccessVector = Successes,
                      GridIDS = GridIDs,
                      BanditArmsCount = BanditArmsN,
                      RunsWithoutNewWinner = RunsWithoutNewWinner,
                      MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
                      MaxNumberModels = MaxNumberModels,
                      MaxRunMinutes = MaxRunMinutes,
                      TotalRunTime = TotalRunTime,
                      BanditProbabilities = BanditProbs) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2L))
  
  # Compute Baseline performance----
  if(ModelRun == 1L) Baseline <- min(ExperimentGrid[Blended_MSE > 0L & Blended_MAE > 0L & Blended_MAPE > 0L][[paste0("Blended_",MetricSelection)]], na.rm = TRUE)
  
  # Compute best performance----
  if(is.na(min(ExperimentGrid[Blended_MSE > 0L & Blended_MAE > 0L & Blended_MAPE > 0L][[paste0("Blended_",MetricSelection)]], na.rm = TRUE))) {
    BestPerformance <- -10L
  } else {
    BestPerformance <- min(ExperimentGrid[Blended_MSE > 0L & Blended_MAE > 0L & Blended_MAPE > 0L][[paste0("Blended_",MetricSelection)]], na.rm = TRUE)
  }

  
  # New performance----
  if(is.na(ExperimentGrid[ModelRun, get(paste0("Blended_",MetricSelection))])) {
    NewPerformance <- -10L
  } else {
    NewPerformance <- ExperimentGrid[ModelRun, get(paste0("Blended_",MetricSelection))]
  }
  
  # Comparison----
  if(NewPerformance <= BestPerformance & ModelRun <= BanditArmsCount + 1L) BestGrid <- ModelRun
  
  # Update trial counts----
  if(ModelRun != 1L) TrialVector[NEWGrid] <- TrialVector[NEWGrid] + 1L
  
  # Best Metric----
  if(ModelRun != 1L) {
    
    # Compute Runs of Consecutive Failures----
    if(NewPerformance < BestPerformance) {
      RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
    } else {
      RunsWithoutNewWinner <- 0L
      if(ModelRun == BanditArmsCount) {
        SuccessVector[BestGrid] <- SuccessVector[BestGrid] + 1L
      } else {
        SuccessVector[NEWGrid] <- SuccessVector[NEWGrid] + 1L
      }
    }
    
    # Update Bandit Probabilities----
    if(any(TrialVector < SuccessVector )) {
      TrialVector[which(TrialVector < SuccessVector)] <- TrialVector[which(TrialVector < SuccessVector)] + 1L
    }
    
    # Create Bandit Probabilities----
    BanditProbabilities <- RPM_Binomial_Bandit(Success = SuccessVector, Trials = TrialVector, SubDivisions = 1000L)
    
    # Sample from bandit to select next grid row----
    NewGrid <- GridIDS[sample.int(n = BanditArmsCount, size = 1L, replace = TRUE, prob = BanditProbabilities)]
  } else {
    
    # Sample from bandit to select next grid row----
    NewGrid <- GridIDS[sample.int(n = BanditArmsCount, size = 1L, replace = TRUE, prob = BanditProbabilities)]
  }
  
  # Loop Break Conditions (No new winners; Max models built; Max time reached)----
  if(RunsWithoutNewWinner >= MaxRunsWithoutNewWinner | ModelRun > MaxNumberModels | TotalRunTime > MaxRunMinutes * 60L) {
    Break <- "exit"      
  } else {
    Break <- "stay"
  }
  
  # Return----
  return(list(
    NewGrid = NewGrid,
    Trials = TrialVector,
    Successes = SuccessVector,
    BanditProbs = BanditProbabilities,
    BreakLoop = Break))
}

#' RL_ML_Update 
#' 
#' RL_ML_Update updates the bandit probabilities for selecting different grids
#'
#' @author Adrian Antico
#' @family Reinforcement Learning
#' @param ExperimentGrid This is a data.table of grid params and model results
#' @param ModelRun Model iteration number
#' @param ModelType "classification", "regression", and "multiclass"
#' @param NEWGrid Previous grid passed in
#' @param TrialVector Numeric vector with the total trials for each arm
#' @param SuccessVector Numeric vector with the total successes for each arm
#' @param GridIDS The numeric vector that identifies which grid is which
#' @param BanditArmsCount The number of arms in the bandit
#' @param RunsWithoutNewWinner Counter of the number of models previously built without being a new winner
#' @param MaxRunsWithoutNewWinner Maximum number of models built without a new best model (constraint)
#' @param MaxNumberModels Maximum number of models to build (constraint)
#' @param MaxRunMinutes Run time constraint
#' @param TotalRunTime Cumulative run time in minutes
#' @param BanditProbabilities Inital probabilities from RL_Initialize()
#' @examples 
#' RL_Update_Output <- RL_ML_Update(
#'   ExperimentGrid = ExperimentGrid,
#'   ModelRun = run,
#'   ModelType = "classification",
#'   NEWGrid = NewGrid,
#'   TrialVector = Trials,
#'   SuccessVector = Successes,
#'   GridIDS = GridIDs,
#'   BanditArmsCount = BanditArmsN,
#'   RunsWithoutNewWinner = RunsWithoutNewWinner,
#'   MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
#'   MaxNumberModels = MaxNumberModels,
#'   MaxRunMinutes = MaxRunMinutes,
#'   TotalRunTime = TotalRunTime,
#'   BanditProbabilities = BanditProbs)
#' BanditProbs <- RL_Update_Output[["BanditProbs"]]
#' Trials <- RL_Update_Output[["Trials"]]
#' Successes <- RL_Update_Output[["Successes"]]
#' NewGrid <- RL_Update_Output[["NewGrid"]]
#' @export 
RL_ML_Update <- function(ExperimentGrid = ExperimentGrid,
                         ModelType = "classification",
                         ModelRun = counter,
                         NEWGrid = NewGrid,
                         NewPerformance = NewPerformance,
                         BestPerformance = BestPerformance,
                         TrialVector = Trials,
                         SuccessVector = Successes,
                         GridIDS = GridIDs,
                         BanditArmsCount = BanditArmsN,
                         RunsWithoutNewWinner = RunsWithoutNewWinner,
                         MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
                         MaxNumberModels = MaxNumberModels,
                         MaxRunMinutes = MaxRunMinutes,
                         TotalRunTime = TotalRunTime,
                         BanditProbabilities = BanditProbs) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2L))
  
  # Comparison----
  if(ModelRun <= BanditArmsCount + 1L) {
    BestGrid <- ModelRun - 1L
  } else {
    BestGrid <- NEWGrid
  }
  
  # Update trial counts----
  if(ModelRun > 1L) TrialVector[BestGrid] <- TrialVector[BestGrid] + 1L
  
  # Best Metric----
  if(ModelRun != 1L) {
    
    # Consecutive failures and updating success vectors----
    if(ModelRun > 1L) {
      if(tolower(ModelType) == "classification") if(NewPerformance > BestPerformance) SuccessVector[BestGrid] <- SuccessVector[BestGrid] + 1L
      if(tolower(ModelType) %chin% c("regression","multiclass")) if(NewPerformance < BestPerformance) SuccessVector[BestGrid] <- SuccessVector[BestGrid] + 1L
    }
    
    # Update Bandit Probabilities----
    if(any(TrialVector < SuccessVector )) {
      TrialVector[which(TrialVector < SuccessVector)] <- TrialVector[which(TrialVector < SuccessVector)] + 1L
    }
    
    # Create Bandit Probabilities----
    BanditProbabilities <- RPM_Binomial_Bandit(Success = SuccessVector, Trials = TrialVector, SubDivisions = 1000L)
    
    # Sample from bandit to select next grid row----
    NewGrid <- GridIDS[sample.int(n = BanditArmsCount, size = 1L, replace = TRUE, prob = BanditProbabilities)]
  } else {
    
    # Sample from bandit to select next grid row----
    NewGrid <- GridIDS[sample.int(n = BanditArmsCount, size = 1L, replace = TRUE, prob = BanditProbabilities)]
  }
  
  # Loop Break Conditions (No new winners; Max models built; Max time reached)----
  if(RunsWithoutNewWinner >= MaxRunsWithoutNewWinner | ModelRun > MaxNumberModels | TotalRunTime > MaxRunMinutes * 60L) {
    Break <- "exit"      
  } else {
    Break <- "stay"
  }
  
  # Return----
  return(list(
    NewGrid = NewGrid,
    Trials = TrialVector,
    Successes = SuccessVector,
    BanditProbs = BanditProbabilities,
    BreakLoop = Break))
}

#' CatBoostParameterGrids 
#' 
#' CatBoostParameterGrids https://catboost.ai/docs/concepts/r-training-parameters.html
#' 
#' @author Adrian Antico
#' @family Supervised Learning 
#' @param TaskType "GPU" or "CPU"
#' @param Shuffles The number of shuffles you want to apply to each grid
#' @param BootStrapType c("Bayesian", "Bernoulli", "Poisson", "MVS", "No")
#' @param NTrees seq(1000L, 10000L, 1000L)
#' @param Depth seq(4L, 16L, 2L)
#' @param LearningRate seq(0.01,.10,0.01)
#' @param L2_Leaf_Reg c(1.0:10.0)
#' @param GrowPolicy c("SymmetricTree", "Depthwise", "Lossguide")
#' @param RSM CPU ONLY, Random subspace method.c(0.80, 0.85, 0.90, 0.95, 1.0) 
#' @return A list containing data.table's with the parameters shuffled and ready to test in the bandit framework
#' @export
CatBoostParameterGrids <- function(TaskType = "CPU",
                                   Shuffles = 1L,
                                   NTrees = seq(1000L, 10000L, 1000L),
                                   Depth = seq(4L, 16L, 2L),
                                   LearningRate = c(0.01,0.02,0.03,0.04),
                                   L2_Leaf_Reg = seq(1.0, 10.0, 1.0),
                                   RSM = c(0.80, 0.85, 0.90, 0.95, 1.0),
                                   BootStrapType = c("Bayesian", "Bernoulli", "Poisson", "MVS", "No"),
                                   GrowPolicy = c("SymmetricTree", "Depthwise", "Lossguide")) {
  
  # Create grid sets----
  Grid <- data.table::CJ(
    
    # Basis for creating parsimonous buckets----
    NTrees = if(!is.null(NTrees)) sort(NTrees, decreasing = FALSE) else seq(1000L, 10000L, 1000L),
    Depth = if(!is.null(Depth)) sort(Depth, decreasing = FALSE) else seq(4L, 16L, 2L),
    LearningRate = if(!is.null(LearningRate)) sort(LearningRate, decreasing = FALSE) else seq(0.01,0.10,0.01),
    
    # Random hyperparameters----
    L2_Leaf_Reg = if(!is.null(L2_Leaf_Reg)) L2_Leaf_Reg else seq(1.0, 10.0, 1.0),
    RSM = if(!is.null(RSM)) RSM else c(0.80, 0.85, 0.90, 0.95, 1.0),
    BootStrapType = if(!is.null(BootStrapType)) BootStrapType else c("Bayesian", "Bernoulli", "Poisson", "MVS", "No"),
    GrowPolicy = if(!is.null(GrowPolicy)) GrowPolicy else c("SymmetricTree", "Depthwise", "Lossguide"))
  
  # Filter out invalid grids----  
  if(tolower(TaskType) == "gpu") {
    data.table::set(Grid, j = "RSM", value = NULL)
    Grid <- Grid[!BootStrapType %chin% c("MVS")]
    Grid <- unique(Grid[BootStrapType != "Poisson" & GrowPolicy != "Lossguide"])
  } else {
    Grid <- Grid[!tolower(BootStrapType) %chin% c("poisson")]
    Grid <- unique(Grid[, GrowPolicy := NULL])
  }
  

  # Total loops----
  N_NTrees <- length(unique(Grid[["NTrees"]]))
  N_Depth <- length(unique(Grid[["Depth"]]))
  N_LearningRate <- length(unique(Grid[["LearningRate"]]))
  N_L2_Leaf_Reg <- length(unique(Grid[["L2_Leaf_Reg"]]))
  Runs <- max(N_NTrees, N_Depth, N_LearningRate)
  Grids <- list()
  
  # Create grid sets----
  for (i in seq_len(Runs)) {
    if(i == 1L) {
      Grids[[paste0("Grid_",i)]] <- 
        Grid[NTrees <= unique(Grid[["NTrees"]])[min(i,N_NTrees)] & Depth <= unique(Grid[["Depth"]])[min(i,N_Depth)] & LearningRate <= unique(Grid[["LearningRate"]])[min(i,N_LearningRate)]]
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
    L2_Leaf_Reg = rep(-1,10000L),
    RSM = rep(-1,10000L),
    BootStrapType = rep("aa", 10000L),
    GrowPolicy = rep("aa", 10000L))
  
  # Shuffle grid sets----
  for(shuffle in seq_len(Shuffles)) for(i in seq_len(Runs)) Grids[[paste0("Grid_",i)]] <- Grids[[paste0("Grid_",i)]][order(runif(Grids[[paste0("Grid_",i)]][,.N]))]
  
  # Return grid----
  return(list(Grid = Grid, Grids = Grids, ExperimentalGrid = eGrid))
}



#' CatBoostRegressionParams
#'
#' CatBoostRegressionParams
#' 
#' @author Adrian Antico
#' @family Supervised Learning 
#' @param counter Passthrough
#' @param BanditArmsN Passthrough
#' @param HasTime Passthrough
#' @param MetricPeriods Passthrough
#' @param eval_metric Passthrough
#' @param task_type Passthrough
#' @param model_path Passthrough
#' @param NewGrid Passthrough
#' @param Grid Passthrough
#' @param ExperimentalGrid Passthrough
#' @param GridClusters Passthrough
#' @noRd
CatBoostRegressionParams <- function(counter = NULL,
                                     BanditArmsN = NULL,
                                     HasTime = NULL,
                                     MetricPeriods = NULL,
                                     eval_metric = NULL,
                                     task_type = NULL,
                                     model_path = NULL,
                                     NewGrid = NULL,
                                     Grid = NULL,
                                     ExperimentalGrid = NULL,
                                     GridClusters = NULL) {
  
  # Select Grid----
  if(counter <= BanditArmsN + 1L) {
    
    # Run default catboost model, with max trees from grid, and use this as the measure to beat for success / failure in bandit framework
    # Then run through a single model from each grid cluster to get the starting point for the bandit calcs
    if(counter == 1L) {
      base_params <- list(
        has_time             = HasTime,
        metric_period        = MetricPeriods,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        task_type            = task_type,
        train_dir            = model_path,
        iterations           = max(Grid$NTrees))
    } else {
      if(counter > 1L) data.table::set(ExperimentalGrid, i = counter-1L, j = "GridNumber", value = counter-1L)
      if(tolower(task_type) == "gpu") {
        base_params <- list(
          has_time             = HasTime,
          metric_period        = MetricPeriods,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          best_model_min_trees = 10L,
          task_type            = task_type,
          train_dir            = model_path,
          iterations           = GridClusters[[paste0("Grid_",counter-1L)]][["NTrees"]][1L],
          depth                = GridClusters[[paste0("Grid_",counter-1L)]][["Depth"]][1L],
          learning_rate        = GridClusters[[paste0("Grid_",counter-1L)]][["LearningRate"]][1L],
          l2_leaf_reg          = GridClusters[[paste0("Grid_",counter-1L)]][["L2_Leaf_Reg"]][1L],
          bootstrap_type       = GridClusters[[paste0("Grid_",counter-1L)]][["BootStrapType"]][1L],
          grow_policy          = GridClusters[[paste0("Grid_",counter-1L)]][["GrowPolicy"]][1L])
      } else {
        base_params <- list(
          has_time             = HasTime,
          metric_period        = MetricPeriods,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          best_model_min_trees = 10L,
          task_type            = task_type,
          train_dir            = model_path,
          iterations           = GridClusters[[paste0("Grid_",counter-1L)]][["NTrees"]][1L],
          depth                = GridClusters[[paste0("Grid_",counter-1L)]][["Depth"]][1L],
          learning_rate        = GridClusters[[paste0("Grid_",counter-1L)]][["LearningRate"]][1L],
          l2_leaf_reg          = GridClusters[[paste0("Grid_",counter-1L)]][["L2_Leaf_Reg"]][1L],
          rsm                  = GridClusters[[paste0("Grid_",counter-1L)]][["RSM"]][1L],
          bootstrap_type       = GridClusters[[paste0("Grid_",counter-1L)]][["BootStrapType"]][1L])
      }
    }
  } else {
    data.table::set(ExperimentalGrid, i = counter-1L, j = "GridNumber", value = NewGrid)
    if (tolower(task_type) == "gpu") {
      base_params <- list(
        has_time             = HasTime,
        metric_period        = MetricPeriods,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        task_type            = task_type,
        train_dir            = model_path,
        iterations           = GridClusters[[paste0("Grid_",NewGrid)]][["NTrees"]][1L],
        depth                = GridClusters[[paste0("Grid_",NewGrid)]][["Depth"]][1L],
        learning_rate        = GridClusters[[paste0("Grid_",NewGrid)]][["LearningRate"]][1L],
        l2_leaf_reg          = GridClusters[[paste0("Grid_",NewGrid)]][["L2_Leaf_Reg"]][1L],
        bootstrap_type       = GridClusters[[paste0("Grid_",NewGrid)]][["BootStrapType"]][1L],
        grow_policy          = GridClusters[[paste0("Grid_",NewGrid)]][["GrowPolicy"]][1L])
    } else {
      base_params <- list(
        has_time             = HasTime,
        metric_period        = MetricPeriods,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        task_type            = task_type,
        train_dir            = model_path,
        iterations           = GridClusters[[paste0("Grid_",NewGrid-1L)]][["NTrees"]][1L],
        depth                = GridClusters[[paste0("Grid_",NewGrid-1L)]][["Depth"]][1L],
        learning_rate        = GridClusters[[paste0("Grid_",NewGrid-1L)]][["LearningRate"]][1L],
        l2_leaf_reg          = GridClusters[[paste0("Grid_",NewGrid-1L)]][["L2_Leaf_Reg"]][1L],
        rsm                  = GridClusters[[paste0("Grid_",NewGrid-1L)]][["RSM"]][1L],
        bootstrap_type       = GridClusters[[paste0("Grid_",NewGrid-1L)]][["BootStrapType"]][1L])
    }
  }
  return(base_params)
}

#' CatBoostRegressionParams
#'
#' CatBoostRegressionParams
#' 
#' @author Adrian Antico
#' @family Supervised Learning 
#' @param counter Passthrough
#' @param BanditArmsN Passthrough
#' @param HasTime Passthrough
#' @param MetricPeriods Passthrough
#' @param ClassWeights Passthrough
#' @param eval_metric Passthrough
#' @param task_type Passthrough
#' @param model_path Passthrough
#' @param NewGrid Passthrough
#' @param Grid Passthrough
#' @param ExperimentalGrid Passthrough
#' @param GridClusters Passthrough
#' @noRd
CatBoostClassifierParams <- function(counter = NULL,
                                     BanditArmsN = NULL,
                                     HasTime = NULL,
                                     MetricPeriods = NULL,
                                     ClassWeights = NULL,
                                     eval_metric = NULL,
                                     task_type = NULL,
                                     model_path = NULL,
                                     NewGrid = NULL,
                                     Grid = NULL,
                                     ExperimentalGrid = NULL,
                                     GridClusters = NULL) {
  
  # Select Grid
  if(counter <= BanditArmsN + 1L) {
    
    # Run default catboost model, with max trees from grid, and use this as the measure to beat for success / failure in bandit framework
    # Then run through a single model from each grid cluster to get the starting point for the bandit calcs
    if(counter == 1L) {
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
        iterations           = max(Grid$NTrees))
    } else {
      if(counter > 1L) data.table::set(ExperimentalGrid, i = counter-1L, j = "GridNumber", value = counter-1L)
      if(tolower(task_type) == "gpu") {
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
          iterations           = GridClusters[[paste0("Grid_",counter-1L)]][["NTrees"]][1L],
          depth                = GridClusters[[paste0("Grid_",counter-1L)]][["Depth"]][1L],
          learning_rate        = GridClusters[[paste0("Grid_",counter-1L)]][["LearningRate"]][1L],
          l2_leaf_reg          = GridClusters[[paste0("Grid_",counter-1L)]][["L2_Leaf_Reg"]][1L],
          bootstrap_type       = GridClusters[[paste0("Grid_",counter-1L)]][["BootStrapType"]][1L],
          grow_policy          = GridClusters[[paste0("Grid_",counter-1L)]][["GrowPolicy"]][1L])
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
          iterations           = GridClusters[[paste0("Grid_",counter-1L)]][["NTrees"]][1L],
          depth                = GridClusters[[paste0("Grid_",counter-1L)]][["Depth"]][1L],
          learning_rate        = GridClusters[[paste0("Grid_",counter-1L)]][["LearningRate"]][1L],
          l2_leaf_reg          = GridClusters[[paste0("Grid_",counter-1L)]][["L2_Leaf_Reg"]][1L],
          rsm                  = GridClusters[[paste0("Grid_",counter-1L)]][["RSM"]][1L],
          bootstrap_type       = GridClusters[[paste0("Grid_",counter-1L)]][["BootStrapType"]][1L])
      }
    }
  } else {
    data.table::set(ExperimentalGrid, i = counter-1L, j = "GridNumber", value = NewGrid)
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
        iterations           = GridClusters[[paste0("Grid_",NewGrid)]][["NTrees"]][1L],
        depth                = GridClusters[[paste0("Grid_",NewGrid)]][["Depth"]][1L],
        learning_rate        = GridClusters[[paste0("Grid_",NewGrid)]][["LearningRate"]][1L],
        l2_leaf_reg          = GridClusters[[paste0("Grid_",NewGrid)]][["L2_Leaf_Reg"]][1L],
        bootstrap_type       = GridClusters[[paste0("Grid_",NewGrid)]][["BootStrapType"]][1L],
        grow_policy          = GridClusters[[paste0("Grid_",NewGrid)]][["GrowPolicy"]][1L])
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
        iterations           = GridClusters[[paste0("Grid_",NewGrid-1L)]][["NTrees"]][1L],
        depth                = GridClusters[[paste0("Grid_",NewGrid-1L)]][["Depth"]][1L],
        learning_rate        = GridClusters[[paste0("Grid_",NewGrid-1L)]][["LearningRate"]][1L],
        l2_leaf_reg          = GridClusters[[paste0("Grid_",NewGrid-1L)]][["L2_Leaf_Reg"]][1L],
        rsm                  = GridClusters[[paste0("Grid_",NewGrid-1L)]][["RSM"]][1L],
        bootstrap_type       = GridClusters[[paste0("Grid_",NewGrid-1L)]][["BootStrapType"]][1L])
    }
  }
  return(base_params)
}

#' CatBoostMultiClassParams
#'
#' CatBoostMultiClassParams
#' 
#' @author Adrian Antico
#' @family Supervised Learning 
#' @param counter Passthrough
#' @param BanditArmsN Passthrough
#' @param HasTime Passthrough
#' @param MetricPeriods Passthrough
#' @param ClassWeights Passthrough
#' @param eval_metric Passthrough
#' @param task_type Passthrough
#' @param model_path Passthrough
#' @param NewGrid Passthrough
#' @param Grid Passthrough
#' @param ExperimentalGrid Passthrough
#' @param GridClusters Passthrough
#' @noRd
CatBoostMultiClassParams <- function(counter = NULL,
                                     BanditArmsN = NULL,
                                     HasTime = NULL,
                                     MetricPeriods = NULL,
                                     ClassWeights = NULL,
                                     eval_metric = NULL,
                                     task_type = NULL,
                                     model_path = NULL,
                                     NewGrid = NULL,
                                     Grid = NULL,
                                     ExperimentalGrid = NULL,
                                     GridClusters = NULL) {
  
  # Select Grid
  if(counter <= BanditArmsN + 1L) {
    
    # Run default catboost model, with max trees from grid, and use this as the measure to beat for success / failure in bandit framework
    # Then run through a single model from each grid cluster to get the starting point for the bandit calcs
    if(counter == 1L) {
      base_params <- list(
        has_time             = HasTime,
        metric_period        = MetricPeriods,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        task_type            = task_type,
        class_weights        = ClassWeights,
        train_dir            = model_path,
        iterations           = max(Grid$NTrees))
    } else {
      if(counter > 1L) data.table::set(ExperimentalGrid, i = counter-1L, j = "GridNumber", value = counter-1L)
      if(tolower(task_type) == "gpu") {
        base_params <- list(
          has_time             = HasTime,
          metric_period        = MetricPeriods,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          best_model_min_trees = 10L,
          task_type            = task_type,
          class_weights        = ClassWeights,
          train_dir            = model_path,
          iterations           = GridClusters[[paste0("Grid_",counter-1L)]][["NTrees"]][1L],
          depth                = GridClusters[[paste0("Grid_",counter-1L)]][["Depth"]][1L],
          learning_rate        = GridClusters[[paste0("Grid_",counter-1L)]][["LearningRate"]][1L],
          l2_leaf_reg          = GridClusters[[paste0("Grid_",counter-1L)]][["L2_Leaf_Reg"]][1L],
          bootstrap_type       = GridClusters[[paste0("Grid_",counter-1L)]][["BootStrapType"]][1L],
          grow_policy          = GridClusters[[paste0("Grid_",counter-1L)]][["GrowPolicy"]][1L])
      } else {
        base_params <- list(
          has_time             = HasTime,
          metric_period        = MetricPeriods,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          best_model_min_trees = 10L,
          task_type            = task_type,
          train_dir            = model_path,
          iterations           = GridClusters[[paste0("Grid_",counter-1L)]][["NTrees"]][1L],
          depth                = GridClusters[[paste0("Grid_",counter-1L)]][["Depth"]][1L],
          learning_rate        = GridClusters[[paste0("Grid_",counter-1L)]][["LearningRate"]][1L],
          l2_leaf_reg          = GridClusters[[paste0("Grid_",counter-1L)]][["L2_Leaf_Reg"]][1L],
          rsm                  = GridClusters[[paste0("Grid_",counter-1L)]][["RSM"]][1L],
          bootstrap_type       = GridClusters[[paste0("Grid_",counter-1L)]][["BootStrapType"]][1L])
      }
    }
  } else {
    data.table::set(ExperimentalGrid, i = counter-1L, j = "GridNumber", value = NewGrid)
    if (tolower(task_type) == "gpu") {
      base_params <- list(
        has_time             = HasTime,
        metric_period        = MetricPeriods,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        task_type            = task_type,
        class_weights        = ClassWeights,
        train_dir            = model_path,
        iterations           = GridClusters[[paste0("Grid_",NewGrid)]][["NTrees"]][1L],
        depth                = GridClusters[[paste0("Grid_",NewGrid)]][["Depth"]][1L],
        learning_rate        = GridClusters[[paste0("Grid_",NewGrid)]][["LearningRate"]][1L],
        l2_leaf_reg          = GridClusters[[paste0("Grid_",NewGrid)]][["L2_Leaf_Reg"]][1L],
        bootstrap_type       = GridClusters[[paste0("Grid_",NewGrid)]][["BootStrapType"]][1L],
        grow_policy          = GridClusters[[paste0("Grid_",NewGrid)]][["GrowPolicy"]][1L])
    } else {
      base_params <- list(
        has_time             = HasTime,
        metric_period        = MetricPeriods,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        best_model_min_trees = 10L,
        task_type            = task_type,
        train_dir            = model_path,
        class_weights        = ClassWeights,
        iterations           = GridClusters[[paste0("Grid_",NewGrid-1L)]][["NTrees"]][1L],
        depth                = GridClusters[[paste0("Grid_",NewGrid-1L)]][["Depth"]][1L],
        learning_rate        = GridClusters[[paste0("Grid_",NewGrid-1L)]][["LearningRate"]][1L],
        l2_leaf_reg          = GridClusters[[paste0("Grid_",NewGrid-1L)]][["L2_Leaf_Reg"]][1L],
        rsm                  = GridClusters[[paste0("Grid_",NewGrid-1L)]][["RSM"]][1L],
        bootstrap_type       = GridClusters[[paste0("Grid_",NewGrid-1L)]][["BootStrapType"]][1L])
    }
  }
  return(base_params)
}
