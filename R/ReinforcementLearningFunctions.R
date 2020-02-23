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
                                Alpha = 1, 
                                Beta = 1, 
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
                          Alpha = 1, 
                          Beta = 1, 
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
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Compute Baseline performance----
  if(ModelRun == 1L) {
    Baseline <- min(ExperimentGrid[Blended_MSE > 0 & Blended_MAE > 0 & Blended_MAPE > 0][[paste0("Blended_",MetricSelection)]], na.rm = TRUE)
  }
  
  # Compute best performance----
  if(is.na(min(ExperimentGrid[Blended_MSE > 0 & Blended_MAE > 0 & Blended_MAPE > 0][[paste0("Blended_",MetricSelection)]], na.rm = TRUE))) {
    BestPerformance <- -10
  } else {
    BestPerformance <- min(ExperimentGrid[Blended_MSE > 0 & Blended_MAE > 0 & Blended_MAPE > 0][[paste0("Blended_",MetricSelection)]], na.rm = TRUE)
  }
  
  # New performance----
  if(is.na(ExperimentGrid[ModelRun, get(paste0("Blended_",MetricSelection))])) {
    NewPerformance <- -10
  } else {
    NewPerformance <- ExperimentGrid[ModelRun, get(paste0("Blended_",MetricSelection))]
  }
  
  # Comparison----
  if(NewPerformance <= BestPerformance & ModelRun <= BanditArmsCount + 1) {
    BestGrid <- ModelRun
  }
  
  # Update trial counts----
  if(ModelRun != 1) {
    TrialVector[NEWGrid] <- TrialVector[NEWGrid] + 1
  }
  
  # Best Metric----
  if(ModelRun != 1) {
    
    # Compute Runs of Consecutive Failures----
    if(NewPerformance > BestPerformance) {
      RunsWithoutNewWinner <- RunsWithoutNewWinner + 1L
    } else {
      RunsWithoutNewWinner <- 0L
      if(ModelRun == BanditArmsCount) {
        SuccessVector[BestGrid] <- SuccessVector[BestGrid] + 1
      } else {
        SuccessVector[NEWGrid] <- SuccessVector[NEWGrid] + 1            
      }
    }
    
    # Update Bandit Probabilities----
    if(any(TrialVector < SuccessVector )) {
      TrialVector[which(TrialVector < SuccessVector)] <- TrialVector[which(TrialVector < SuccessVector)] + 1
    }
    
    # Create Bandit Probabilities----
    BanditProbabilities <- RPM_Binomial_Bandit(Success = SuccessVector, Trials = TrialVector, SubDivisions = 1000L)
    
    # Sample from bandit to select next grid row----
    NewGrid <- GridIDS[sample.int(n = BanditArmsCount, size = 1, replace = TRUE, prob = BanditProbabilities)]
  } else {
    
    # Sample from bandit to select next grid row----
    NewGrid <- GridIDS[sample.int(n = BanditArmsCount, size = 1, replace = TRUE, prob = BanditProbabilities)]
  }
  
  # Loop Break Conditions (No new winners; Max models built; Max time reached)----
  if(RunsWithoutNewWinner >= MaxRunsWithoutNewWinner | ModelRun > MaxNumberModels | TotalRunTime > MaxRunMinutes * 60) {
    Break <- "exit"      
  } else {
    Break <- "stay"
  }
  
  # Return----
  return(
    list(
      NewGrid = NewGrid,
      Trials = TrialVector,
      Successes = SuccessVector,
      BanditProbs = BanditProbabilities,
      BreakLoop = Break
    )
  )
}

#' AutoCatBoostGridSet
#' 
#' AutoCatBoostGridSet lets the user decide on grid options
#' 
#' @author Adrian Antico
#' @family Supervised Learning
#' @param NumberPartitions = 10 
#' @param BootstrapType = c("Bayesian","Bernoulli","Poisson","MVS","No")
#' @param BaggingTemperature = seq(1,3,1)
#' @param MinDataInLeaf = c(1,5,10)
#' @param MaxLeaves = c(15,31,45)
#' @param SubSample = c(seq(0.6,1,0.10))
#' @param RandomStrength = c(0.90,1,1.10)
#' @param SamplingFrequency = c("PerTree","PreTreeLevel") CPU Only
#' @param RandomSubspaceMethod = c(0.65,0.80,1) CPU Only
#' @param ODWait = 50
#' @param L2LeafReg = c(seq(2.0,4.0,1.0))
#' @param LearningRate = c(seq(0.01,0.05,0.01))
#' @param Depth = c(seq(4,16,1))
#' @export
AutoCatBoostGridSet <- function(NumberPartitions = 10,
                                BootstrapType = c("Bayesian","Bernoulli","Poisson","MVS","No"),
                                BaggingTemperature = seq(1,3,1),
                                MinDataInLeaf = c(1,5,10),
                                MaxLeaves = c(15,31,45),
                                SubSample = c(seq(0.6,1,0.10)),
                                RandomStrength = c(0.90,1,1.10),
                                SamplingFrequency = c("PerTree","PreTreeLevel"),
                                RandomSubspaceMethod = c(0.65,0.80,1),
                                ODWait = 50,
                                L2LeafReg = c(seq(2.0,4.0,1.0)),
                                LearningRate = c(seq(0.01,0.05,0.01)),
                                Depth = c(seq(4,16,1))) {
  
  # CatBoost Docs: how are MVS, Poisson, Bayesian, etc, how does catboost decide?
  # QueryCrossEntropy, YetiRankPairwise, PairLogitPairwise: Bernoulli with the subsample parameter set to 0.5
  # MultiClass and MultiClassOneVsAll: Bayesian
  # Other modes:
  #   
  # GPU: Bayesian
  # CPU: MVS with the subsample parameter set to 0.8.
  # 
  # # grid tuning advice by catboost
  # https://catboost.ai/docs/concepts/parameter-tuning.html
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Bayesian Grid Set----
  if(tolower(task_type) == "gpu") {
    BayesianGrid <- data.table::CJ(
      bootstrap_type = "Bayesian",
      bagging_temperature = seq(1,3,1),
      random_strength = c(0.90,1,1.10),
      min_data_in_leaf = c(1,5,10),
      max_leaves = c(15,31,45),
      od_type = "Iter",
      od_wait = 50,
      l2_leaf_reg = c(seq(2.0,4.0,1.0)),
      learning_rate = c(seq(0.01,0.05,0.01)),
      depth = c(seq(4,8,1)))
  } else {
    BayesianGrid <- data.table::CJ(
      bootstrap_type = "Bayesian",
      bagging_temperature = seq(1,3,1),
      random_strength = c(0.90,1,1.10),
      sampling_frequency = c("PerTree","PreTreeLevel"),
      rsm = c(0.65,0.80,1),
      od_type = "Iter",
      od_wait = 50,
      l2_leaf_reg = c(seq(2.0,4.0,1.0)),
      learning_rate = c(seq(0.01,0.05,0.01)),
      depth = c(seq(4,16,1)))
  }
  
  # Bayesian Grid List----
  BayesianGridClusters <- list()
  N <- floor(BayesianGrid[,.N]/10)
  for(i in seq_len(10)) {
    if(i == 1) {
      BayesianGridClusters[[paste0("Bayesian_Grid_",i)]] <- BayesianGrid[1:N][order(runif(N))]
    } else if(i < 10) {
      BayesianGridClusters[[paste0("Bayesian_Grid_",i)]] <- BayesianGrid[(N*(i-1)+1):(N*i)][order(runif(N))]
    } else {
      BayesianGridClusters[[paste0("Bayesian_Grid_",i)]] <- BayesianGrid[(N*(i-1)+1):BayesianGrid[,.N]][order(runif(BayesianGrid[(N*(i-1)+1):BayesianGrid[,.N],.N]))]
    }
  }
  
  # Bernoulli Grid Set----
  if(tolower(task_type) == "gpu") {
    BernoulliGrid <- data.table::CJ(
      bootstrap_type = "Bernoulli",
      subsample = c(seq(0.6,1,0.10)),
      random_strength = c(0.90,1,1.10),
      min_data_in_leaf = c(1,5,10), # GPU only
      max_leaves = c(15,31,45), # GPU only
      od_type = "Iter",
      od_wait = 50,
      l2_leaf_reg = c(seq(2.0,4.0,1.0)),
      learning_rate = c(seq(0.01,0.05,0.01)),
      depth = c(seq(4,8,1)))
  } else {
    BernoulliGrid <- data.table::CJ(
      bootstrap_type = "Bernoulli",
      subsample = c(seq(0.6,1,0.10)),
      random_strength = c(0.90,1,1.10),
      sampling_frequency = c("PerTree","PreTreeLevel"), # CPU only
      rsm = c(0.65,0.80,1), # random subspace method CPU only
      od_type = "Iter",
      od_wait = 50,
      l2_leaf_reg = c(seq(2.0,4.0,1.0)),
      learning_rate = c(seq(0.01,0.05,0.01)),
      depth = c(seq(4,16,1)))
  }
  
  # Bernoulli Grid List----
  BernoulliGridClusters <- list()
  N <- floor(BernoulliGrid[,.N]/10)
  for(i in seq_len(10)) {
    if(i == 1) {
      BernoulliGridClusters[[paste0("Bayesian_Grid_",i)]] <- BernoulliGrid[1:N][order(runif(N))]
    } else if(i < 10) {
      BernoulliGridClusters[[paste0("Bayesian_Grid_",i)]] <- BernoulliGrid[(N*(i-1)+1):(N*i)][order(runif(N))]
    } else {
      BernoulliGridClusters[[paste0("Bayesian_Grid_",i)]] <- BernoulliGrid[(N*(i-1)+1):BernoulliGrid[,.N]][order(runif(BernoulliGrid[(N*(i-1)+1):BernoulliGrid[,.N],.N]))]
    }
  }
  
  # 'No' Grid Set----
  if(tolower(task_type) == "gpu") {
    NoGrid <- data.table::CJ(
      bootstrap_type = "No",
      random_strength = c(0.90,1,1.10),
      min_data_in_leaf = c(1,5,10), # GPU only
      max_leaves = c(15,31,45), # GPU only
      od_type = "Iter",
      od_wait = 50,
      l2_leaf_reg = c(seq(2.0,4.0,1.0)),
      learning_rate = c(seq(0.01,0.05,0.01)),
      depth = c(seq(4,8,1)))
  } else {
    NoGrid <- data.table::CJ(
      bootstrap_type = "No",
      random_strength = c(0.90,1,1.10),
      sampling_frequency = c("PerTree","PreTreeLevel"), # CPU only
      rsm = c(0.65,0.80,1), # random subspace method CPU only
      od_type = "Iter",
      od_wait = 50,
      l2_leaf_reg = c(seq(2.0,4.0,1.0)),
      learning_rate = c(seq(0.01,0.05,0.01)),
      depth = c(seq(4,16,1)))
  }
  
  # 'No' Grid List----
  NoGridClusters <- list()
  N <- floor(NoGrid[,.N]/10)
  for(i in seq_len(10)) {
    if(i == 1) {
      NoGridClusters[[paste0("Bayesian_Grid_",i)]] <- NoGrid[1:N][order(runif(N))]
    } else if(i < 10) {
      NoGridClusters[[paste0("Bayesian_Grid_",i)]] <- NoGrid[(N*(i-1)+1):(N*i)][order(runif(N))]
    } else {
      NoGridClusters[[paste0("Bayesian_Grid_",i)]] <- NoGrid[(N*(i-1)+1):NoGrid[,.N]][order(runif(NoGrid[(N*(i-1)+1):NoGrid[,.N],.N]))]
    }
  }
  
  # Poisson Grid Set----
  if(tolower(task_type) == "gpu") {
    PoissonGrid <- data.table::CJ(
      bootstrap_type = "Poisson",
      subsample = c(seq(0.6,1,0.10)),
      random_strength = c(0.90,1,1.10),
      min_data_in_leaf = c(1,5,10), # GPU only
      max_leaves = c(15,31,45), # GPU only
      od_type = "Iter",
      od_wait = 50,
      l2_leaf_reg = c(seq(2.0,4.0,1.0)),
      learning_rate = c(seq(0.01,0.05,0.01)),
      depth = c(seq(4,8,1)))
  }
  
  # Poisson Grid List----
  PoissonGridClusters <- list()
  N <- floor(PoissonGrid[,.N]/10)
  for(i in seq_len(10)) {
    if(i == 1) {
      PoissonGridClusters[[paste0("Bayesian_Grid_",i)]] <- PoissonGrid[1:N][order(runif(N))]
    } else if(i < 10) {
      PoissonGridClusters[[paste0("Bayesian_Grid_",i)]] <- PoissonGrid[(N*(i-1)+1):(N*i)][order(runif(N))]
    } else {
      PoissonGridClusters[[paste0("Bayesian_Grid_",i)]] <- PoissonGrid[(N*(i-1)+1):PoissonGrid[,.N]][order(runif(PoissonGrid[(N*(i-1)+1):PoissonGrid[,.N],.N]))]
    }
  }
  
  # MVS Grid Set----
  if(tolower(task_type) == "cpu") {
    MVSGrid <- data.table::CJ(
      bootstrap_type = "MVS",
      subsample = c(seq(0.6,1,0.10)),
      random_strength = c(0.90,1,1.10),
      sampling_frequency = c("PerTree","PreTreeLevel"), # CPU only
      rsm = c(0.65,0.80,1), # random subspace method CPU only
      od_type = "Iter",
      od_wait = 50,
      l2_leaf_reg = c(seq(2.0,4.0,1.0)),
      learning_rate = c(seq(0.01,0.05,0.01)),
      depth = c(seq(6,16,1)))
  }
  
  # MVS Grid List----
  MVSGridClusters <- list()
  N <- floor(MVSGrid[,.N]/10)
  for(i in seq_len(10)) {
    if(i == 1) {
      MVSGridClusters[[paste0("Bayesian_Grid_",i)]] <- MVSGrid[1:N][order(runif(N))]
    } else if(i < 10) {
      MVSGridClusters[[paste0("Bayesian_Grid_",i)]] <- MVSGrid[(N*(i-1)+1):(N*i)][order(runif(N))]
    } else {
      MVSGridClusters[[paste0("Bayesian_Grid_",i)]] <- MVSGrid[(N*(i-1)+1):MVSGrid[,.N]][order(runif(MVSGrid[(N*(i-1)+1):MVSGrid[,.N],.N]))]
    }
  }
  
  
  
  
  
  
  
  # Add evaluation metrics columns and fill with dummy values----
  for(trainvalidate in c("Train_","Validate_","Blended_")) {
    for(tseval in c("MSE","MAE","MAPE")) {
      data.table::set(GridClusters[["ParsimonousGrid"]], j = paste0(trainvalidate,tseval), value = -10)
      data.table::set(GridClusters[["RandomGrid"]], j = paste0(trainvalidate,tseval), value = -10)
      data.table::set(Grid, j = paste0(trainvalidate,tseval), value = -10)
      for(i in seq_len(TotalStratGrids)) {
        data.table::set(GridClusters[[paste0("StratifyParsimonousGrid_",i)]],j = paste0(trainvalidate,tseval), value = -10)
      }
    }
  }
  
  # Set up results grid to collect parameters tested and results----
  ExperimentGrid <- data.table::copy(Grid)
  ExperimentGrid[, ModelRunNumber := seq_len(ExperimentGrid[, .N])]
  data.table::set(ExperimentGrid, j = "GridName", value = "xxx")
  for(i in seq_len(ncol(ExperimentGrid))[-1]) {
    if(is.character(ExperimentGrid[[i]])) {
      data.table::set(ExperimentGrid, j = i, value = "xxx")
      data.table::set(ExperimentGrid, i = 1L, j = i, value = "AutoArima")
    } else if(is.numeric(ExperimentGrid[[i]]) | is.integer(ExperimentGrid[[i]])) {
      data.table::set(ExperimentGrid, j = i, value = -10)
      data.table::set(ExperimentGrid, i = 1L, j = i, value = -7)
    } else if(is.logical(ExperimentGrid[[i]])) {
      data.table::set(ExperimentGrid, j = i, value = FALSE)
    }
  }
  
  # Return objects----
  return(
    list(Grid = Grid,
         GridClusters = GridClusters,
         ExperimentGrid = ExperimentGrid))
  
}

