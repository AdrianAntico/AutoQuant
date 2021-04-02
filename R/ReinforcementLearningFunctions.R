#' @title RPM_Binomial_Bandit
#'
#' @description RPM_Binomial_Bandit computes randomized probability matching probabilities for each arm being best in a multi-armed bandit. Close cousin to Thomson Sampling.
#'
#' @author Adrian Antico
#' @family Reinforcement Learning
#'
#' @param Success Vector of successes. One slot per arm.
#' @param Trials Vector of trials. One slot per arm.
#' @param Alpha Prior parameter for success
#' @param Beta Prior parameter for trials
#' @param SubDivisions Default is 100L in the stats package. Changed it to 1000 for this function.
#' @return Probability of each arm being the best arm compared to all other arms.
#' @noRd
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

#' @title RL_Initialize
#'
#' RL_Initialize sets up the components necessary for RL
#'
#' @author Adrian Antico
#' @family Reinforcement Learning
#'
#' @param ParameterGridSet This is a list of tuning grids
#' @param Alpha Prior successes
#' @param Beta Prior trials
#' @param SubDivisions Tolerance for integration
#' @examples
#' \dontrun{
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
#' }
#' @noRd
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

#' @title RL_Update
#'
#' RL_Update updates the bandit probabilities for selecting different grids
#'
#' @author Adrian Antico
#' @family Reinforcement Learning
#'
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
#' \dontrun{
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
#' }
#' @noRd
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
      SuccessVector[NEWGrid] <- SuccessVector[NEWGrid] + 1L
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

#' @title RL_ML_Update
#'
#' @description RL_ML_Update updates the bandit probabilities for selecting different grids
#'
#' @author Adrian Antico
#' @family Reinforcement Learning
#'
#' @param ModelType "classification", "regression", and "multiclass"
#' @param grid_eval_metric grid_eval_metric.
#' @param Iteration Model iteration number
#' @param NewGrid. Previous grid passed in
#' @param NewPerformance. Internal
#' @param BestPerformance. Internal
#' @param Trials. Numeric vector with the total trials for each arm
#' @param Successes. Numeric vector with the total successes for each arm
#' @param GridIDs. The numeric vector that identifies which grid is which
#' @param BanditArmsN. The number of arms in the bandit
#' @param RunsWithoutNewWinner. Counter of the number of models previously built without being a new winner
#' @param MaxRunsWithoutNewWinner. Maximum number of models built without a new best model (constraint)
#' @param MaxModelsInGrid. Maximum number of models to build (constraint)
#' @param MaxRunMinutes. Run time constraint
#' @param TotalRunTime. Cumulative run time in minutes
#' @param BanditProbs. Inital probabilities from RL_Initialize()
#' @examples
#' \dontrun{
#' RL_Update_Output <- RL_ML_Update(
#'   ModelType = "classification",
#'   grid_eval_metric = grid_eval_metric.,
#'   Iteration = run,
#'   NewGrid. = NewGrid,
#'   NewPerformance. = NewPerformance,
#'   BestPerformance. = BestPerformance,
#'   Trials. = Trials,
#'   Successes. = Successes,
#'   GridIDs. = GridIDs,
#'   BanditArmsN. = BanditArmsN,
#'   RunsWithoutNewWinner. = RunsWithoutNewWinner,
#'   MaxRunsWithoutNewWinner. = MaxRunsWithoutNewWinner,
#'   MaxNumberModels. = MaxNumberModels,
#'   MaxRunMinutes. = MaxRunMinutes,
#'   TotalRunTime. = TotalRunTime,
#'   BanditProbs. = BanditProbs)
#' BanditProbs <- RL_Update_Output[["BanditProbs"]]
#' Trials <- RL_Update_Output[["Trials"]]
#' Successes <- RL_Update_Output[["Successes"]]
#' NewGrid <- RL_Update_Output[["NewGrid"]]
#' }
#' @noRd
RL_ML_Update <- function(ModelType = "classification",
                         grid_eval_metric = grid_eval_metric.,
                         Iteration = counter,
                         NewGrid. = NewGrid,
                         NewPerformance. = NewPerformance,
                         BestPerformance. = BestPerformance,
                         Trials. = Trials,
                         Successes. = Successes,
                         GridIDs. = GridIDs,
                         BanditArmsN. = BanditArmsN,
                         RunsWithoutNewWinner. = RunsWithoutNewWinner,
                         MaxRunsWithoutNewWinner. = MaxRunsWithoutNewWinner,
                         MaxModelsInGrid. = MaxModelsInGrid,
                         MaxRunMinutes. = MaxRunMinutes,
                         TotalRunTime. = TotalRunTime,
                         BanditProbs. = BanditProbs) {

  # Comparison ----
  if(Iteration <= BanditArmsN. + 1L) GridRun <- Iteration - 1L else GridRun <- NewGrid.

  # Update trial counts ----
  if(Iteration > 1L) Trials.[GridRun] <- Trials.[GridRun] + 1L

  # Generate NewGrid ----
  if(Iteration == 1L) {

    # Base case: very first run is with default model settings
    # This case is grabbing info from the very first grid set
    NewGrid. <- 1

  } else if(Iteration < BanditArmsN.) {

    # Increment over the entire set of grid sets a single time before using bandit probabilities to select next grid
    NewGrid. <- NewGrid. + 1L

  } else {

    # Print exit values
    print(paste0("Iteration number------  : ", Iteration))
    print(paste0("Runs without new winner : ", RunsWithoutNewWinner.))
    print(paste0("Total run time--------- : ", TotalRunTime.))

    # Select grid using performance data and bandit probabilities
    # Consecutive failures and updating success vectors ----
    if(grid_eval_metric %chin% c("Utility", "MCC", "Acc", "F1_Score", "F2_Score", "F0.5_Score", "NPV", "PPV", "TPR", "TNR", "ThreatScore", "r2","MicroAUC","Accuracy")) {
      if(NewPerformance. > BestPerformance.) Successes.[GridRun] <- Successes.[GridRun] + 1L
    } else if(tolower(ModelType) %chin% c("regression","multiclass")) {
      if(NewPerformance. < BestPerformance.) Successes.[GridRun] <- Successes.[GridRun] + 1L
    }

    # Create Bandit Probabilities ----
    BanditProbs. <- RPM_Binomial_Bandit(Success = Successes., Trials = Trials., SubDivisions = 1000L)

    # Sample from GridIDs. using Bandit Probabilities ----
    NewGrid. <- GridIDs.[sample.int(n = BanditArmsN., size = 1L, replace = TRUE, prob = BanditProbs.)]
  }

  # Check to see if we should stop testing
  if(RunsWithoutNewWinner. >= MaxRunsWithoutNewWinner. || Iteration > MaxModelsInGrid. || TotalRunTime. > MaxRunMinutes. * 60L) {
    Break <- "exit"
  } else {
    Break <- "stay"
  }

  # Return
  return(list(
    NewGrid = NewGrid.,
    Trials = Trials.,
    Successes = Successes.,
    BanditProbs = BanditProbs.,
    BreakLoop = Break))
}
