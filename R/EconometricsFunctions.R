#' Regular_Performance creates and stores model results in Experiment Grid
#' 
#' Regular_Performance creates and stores model results in Experiment Grid
#'
#' @author Adrian Antico
#' @family Time Series
#' @param Model Set to ets, tbats, arfima, tslm, nnetar
#' @param Results This is a time series model
#' @param TrainValidateShare The values used to blend training and validation performance
#' @param ExperimentGrid The results collection table
#' @param run Iterator
#' @param train Data set
#' @param ValidationData Data set
#' @param HoldOutPeriods Passthrough
#' @export 
Regular_Performance <- function(Model = NULL,
                                Results = Results, 
                                GridList = GridList,
                                TrainValidateShare = c(0.5,0.5), 
                                ExperimentGrid = ExperimentGrid,
                                run = run, 
                                train = train,
                                ValidationData = ValidationData,
                                HoldOutPeriods = HoldOutPeriods) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
  
  # Train Performance----
  if(!is.null(Results)) {
    if(run == 1L) {
      
      # AutoETS----
      TrainMetrics <- data.table::data.table(Target = as.numeric(train), FC = as.numeric(Results$fitted))
      
      # Compute residuals----
      data.table::set(TrainMetrics, j = "Residuals", value = TrainMetrics[["Target"]] - TrainMetrics[["FC"]])
      
      # Compute intermediary metrics----
      data.table::set(TrainMetrics, 
                      j = c("SquaredError","AbsoluteError","AbsolutePercentageError"),
                      value = list(TrainMetrics[["Residuals"]]^2,
                                   abs(TrainMetrics[["Residuals"]]),
                                   abs(TrainMetrics[["Residuals"]])/(TrainMetrics[["Target"]])))
      
      # Fill ExperimentGrid Table----
      data.table::set(ExperimentGrid,
                      i = run,
                      j = c("Train_MSE","Train_MAE","Train_MAPE"),
                      value = list(mean(TrainMetrics[["SquaredError"]], na.rm = TRUE),
                                   mean(TrainMetrics[["AbsoluteError"]], na.rm = TRUE),
                                   mean(TrainMetrics[["AbsolutePercentageError"]], na.rm = TRUE)))
    } else {
      
      # Back cast----
      TrainMetrics <- data.table::data.table(Target = as.numeric(train), FC = as.numeric(Results$fitted))
      
      # Compute residuals----
      data.table::set(TrainMetrics, j = "Residuals", value = TrainMetrics[["Target"]] - TrainMetrics[["FC"]])
      
      # Compute intermediary metrics----
      data.table::set(TrainMetrics, 
                      j = c("SquaredError","AbsoluteError","AbsolutePercentageError"),
                      value = list(TrainMetrics[["Residuals"]] ^ 2L,
                                   abs(TrainMetrics[["Residuals"]]),
                                   abs(TrainMetrics[["Residuals"]])/(TrainMetrics[["Target"]])))
      
      # Fill ExperimentGrid Table----
      data.table::set(ExperimentGrid,
                      i = run,
                      j = c("Train_MSE","Train_MAE","Train_MAPE"),
                      value = list(mean(TrainMetrics[["SquaredError"]], na.rm = TRUE),
                                   mean(TrainMetrics[["AbsoluteError"]], na.rm = TRUE),
                                   mean(TrainMetrics[["AbsolutePercentageError"]], na.rm = TRUE)))
    }
  } else {
    
    # Fill ExperimentGrid Table----
    data.table::set(ExperimentGrid, 
                    i = run,
                    j = c("Train_MSE","Train_MAE","Train_MAPE"),
                    value = list(NA, NA, NA))
  }
  
  # Validation Performance----
  if(!is.null(Results)) {
    if(run == 1L) {
      
      # ets----
      ValidationMetrics <- data.table::data.table(Target = as.numeric(ValidationData[[2L]]), FC = as.numeric(forecast::forecast(Results, h = HoldOutPeriods)$mean))
      
      # Compute residuals----
      data.table::set(ValidationMetrics, j = "Residuals", value = ValidationMetrics[["Target"]] - ValidationMetrics[["FC"]])
      
      # Compute intermediary metrics----
      data.table::set(ValidationMetrics, 
                      j = c("SquaredError","AbsoluteError","AbsolutePercentageError"),
                      value = list(ValidationMetrics[["Residuals"]] ^ 2L,
                                   abs(ValidationMetrics[["Residuals"]]),
                                   abs(ValidationMetrics[["Residuals"]])/(ValidationMetrics[["Target"]])))
      
      # Fill ExperimentGrid Table----
      data.table::set(ExperimentGrid, 
                      i = run,
                      j = c("Validate_MSE","Validate_MAE","Validate_MAPE"),
                      value = list(mean(ValidationMetrics[["SquaredError"]], na.rm = TRUE),
                                   mean(ValidationMetrics[["AbsoluteError"]], na.rm = TRUE),
                                   mean(ValidationMetrics[["AbsolutePercentageError"]], na.rm = TRUE)))
    } else {
      
      # Forecast----
      ValidationMetrics <- data.table::data.table(Target = as.numeric(ValidationData[[2L]]), FC = as.numeric(forecast::forecast(Results, h = HoldOutPeriods)$mean))
      
      # Compute residuals----
      data.table::set(ValidationMetrics, j = "Residuals", value = ValidationMetrics[["Target"]] - ValidationMetrics[["FC"]])
      
      # Compute intermediary metrics----
      data.table::set(ValidationMetrics, 
                      j = c("SquaredError","AbsoluteError","AbsolutePercentageError"),
                      value = list(ValidationMetrics[["Residuals"]] ^ 2L,
                                   abs(ValidationMetrics[["Residuals"]]),
                                   abs(ValidationMetrics[["Residuals"]])/(ValidationMetrics[["Target"]])))
      
      # Fill ExperimentGrid Table----
      data.table::set(ExperimentGrid, 
                      i = run,
                      j = c("Validate_MSE","Validate_MAE","Validate_MAPE"),
                      value = list(mean(ValidationMetrics[["SquaredError"]], na.rm = TRUE),
                                   mean(ValidationMetrics[["AbsoluteError"]], na.rm = TRUE),
                                   mean(ValidationMetrics[["AbsolutePercentageError"]], na.rm = TRUE)))          
    }
  } else {
    
    # Fill ExperimentGrid Table----
    data.table::set(ExperimentGrid,
                    i = run,
                    j = c("Validate_MSE","Validate_MAE","Validate_MAPE"),
                    value = list(NA, NA, NA))
  }
  
  # Blended Performance----
  data.table::set(ExperimentGrid, 
                  i = run,
                  j = c("Blended_MSE","Blended_MAE","Blended_MAPE"),
                  value = list(TrainValidateShare[1] * ExperimentGrid[run, Train_MSE] + TrainValidateShare[1] * ExperimentGrid[run, Validate_MSE],
                               TrainValidateShare[1] * ExperimentGrid[run, Train_MAE] + TrainValidateShare[1] * ExperimentGrid[run, Validate_MAE],
                               TrainValidateShare[1] * ExperimentGrid[run, Train_MAPE] + TrainValidateShare[1] * ExperimentGrid[run, Validate_MAPE]))
  
  # Fill in Result Values----
  if(tolower(Model) == "ets") {
    if(run != 1L) {
      for (params in c("Damped","Lambda","ModelParam1","ModelParam2","ModelParam3","BiasAdj")) {
        data.table::set(ExperimentGrid,
                        i = run,
                        j = params,
                        value = GridList[[params]][[run]])
      }    
    }
  } else if(tolower(Model) == "tbats") {
    if(run != 1L) {
      for (params in c("Lambda","Trend","Damped","SeasonalPeriods","UseARMAErrors","Lags","MovingAverages","BiasAdj")) {
        data.table::set(ExperimentGrid,
                        i = run,
                        j = params,
                        value = GridList[[params]][[run]])
      }
    }
  } else if(tolower(Model) == "arfima") {
    if(run != 1L) {
      for (params in c("Lambda","Lags","MovingAverages","Drange","BiasAdj")) {
        data.table::set(ExperimentGrid,
                        i = run,
                        j = params,
                        value = GridList[[params]][[run]])
      }
    }
  }
  
  # Return----
  return(ExperimentGrid)
}

#' ARIMA_Performance creates and stores model results in Experiment Grid
#' 
#' ARIMA_Performance creates and stores model results in Experiment Grid
#'
#' @author Adrian Antico
#' @family Time Series
#' @param Results This is a time series model
#' @param TrainValidateShare The values used to blend training and validation performance
#' @param MaxFourierTerms Numeric value
#' @param XREGFC Fourier terms for forecasting
#' @param ExperimentGrid The results collection table
#' @param NextGrid Bandit grid
#' @param run Iterator
#' @param train Data set
#' @param ValidationData Data set
#' @param HoldOutPeriods Passthrough
#' @export 
RL_Performance <- function(Results = Results, 
                           NextGrid = NextGrid,
                           TrainValidateShare = c(0.5,0.5), 
                           MaxFourierTerms = NULL,
                           XREGFC = XREGFC,
                           ExperimentGrid = ExperimentGrid,
                           run = run, 
                           train = train,
                           ValidationData = ValidationData,
                           HoldOutPeriods = HoldOutPeriods) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
  
  # Train Performance----
  if(!is.null(Results)) {
    if(run == 1L) {
      
      # Train Metrics----
      TrainMetrics <- data.table::data.table(Target = as.numeric(train), FC = as.numeric(Results$fitted))
      
      # Compute residuals----
      data.table::set(TrainMetrics, j = "Residuals", value = TrainMetrics[["Target"]] - TrainMetrics[["FC"]])
      
      # Compute intermediary metrics----
      data.table::set(TrainMetrics, 
                      j = c("SquaredError","AbsoluteError","AbsolutePercentageError"),
                      value = list(TrainMetrics[["Residuals"]] ^ 2L,
                                   abs(TrainMetrics[["Residuals"]]),
                                   abs(TrainMetrics[["Residuals"]])/(TrainMetrics[["Target"]])))
      
      # Fill ExperimentGrid Table----
      data.table::set(ExperimentGrid,
                      i = run,
                      j = c("Train_MSE","Train_MAE","Train_MAPE"),
                      value = list(mean(TrainMetrics[["SquaredError"]], na.rm = TRUE),
                                   mean(TrainMetrics[["AbsoluteError"]], na.rm = TRUE),
                                   mean(TrainMetrics[["AbsolutePercentageError"]], na.rm = TRUE)))
    } else {
      if(NextGrid[["MaxFourierTerms"]][1L] == 0L) {
        TrainMetrics <- data.table::data.table(Target = as.numeric(train), FC = as.numeric(Results$fitted))
      } else {
        TrainMetrics <- data.table::data.table(Target = as.numeric(train), FC = as.numeric(Results$fitted))
      }
      
      # Compute residuals----
      data.table::set(TrainMetrics, j = "Residuals", value = TrainMetrics[["Target"]] - TrainMetrics[["FC"]])
      
      # Compute intermediary metrics----
      data.table::set(TrainMetrics, 
                      j = c("SquaredError","AbsoluteError","AbsolutePercentageError"),
                      value = list(TrainMetrics[["Residuals"]] ^ 2L,
                                   abs(TrainMetrics[["Residuals"]]),
                                   abs(TrainMetrics[["Residuals"]])/(TrainMetrics[["Target"]])))
      
      # Fill ExperimentGrid Table----
      data.table::set(ExperimentGrid,
                      i = run,
                      j = c("Train_MSE","Train_MAE","Train_MAPE"),
                      value = list(mean(TrainMetrics[["SquaredError"]], na.rm = TRUE),
                                   mean(TrainMetrics[["AbsoluteError"]], na.rm = TRUE),
                                   mean(TrainMetrics[["AbsolutePercentageError"]], na.rm = TRUE)))
    }
  } else {
    
    # Fill ExperimentGrid Table----
    data.table::set(ExperimentGrid, 
                    i = run,
                    j = c("Train_MSE","Train_MAE","Train_MAPE"),
                    value = list(NA,NA,NA))
  }
  
  # Validation Performance----
  if(!is.null(Results)) {
    if(run == 1L) {
      
      # Validation Metrics----
      ValidationMetrics <- data.table::data.table(Target = as.numeric(ValidationData[[2L]]), FC = as.numeric(forecast::forecast(Results, h = HoldOutPeriods)$mean))
      
      # Compute residuals----
      data.table::set(ValidationMetrics, j = "Residuals", value = ValidationMetrics[["Target"]] - ValidationMetrics[["FC"]])
      
      # Compute intermediary metrics----
      data.table::set(ValidationMetrics, 
                      j = c("SquaredError","AbsoluteError","AbsolutePercentageError"),
                      value = list(ValidationMetrics[["Residuals"]] ^ 2L,
                                   abs(ValidationMetrics[["Residuals"]]),
                                   abs(ValidationMetrics[["Residuals"]])/(ValidationMetrics[["Target"]])))
      
      # Fill ExperimentGrid Table----
      data.table::set(ExperimentGrid, 
                      i = run,
                      j = c("Validate_MSE","Validate_MAE","Validate_MAPE"),
                      value = list(mean(ValidationMetrics[["SquaredError"]], na.rm = TRUE),
                                   mean(ValidationMetrics[["AbsoluteError"]], na.rm = TRUE),
                                   mean(ValidationMetrics[["AbsolutePercentageError"]], na.rm = TRUE)))
    } else {
      if(NextGrid[["MaxFourierTerms"]][1L] == 0L) {
        ValidationMetrics <- data.table::data.table(Target = as.numeric(ValidationData[[2L]]), FC = as.numeric(forecast::forecast(Results, h = HoldOutPeriods)$mean))
      } else {
        ValidationMetrics <- data.table::data.table(Target = as.numeric(ValidationData[[2L]]), FC = as.numeric(forecast::forecast(Results, xreg = XREGFC, h = HoldOutPeriods)$mean))
      }
      
      # Compute residuals----
      data.table::set(ValidationMetrics, j = "Residuals", value = ValidationMetrics[["Target"]] - ValidationMetrics[["FC"]])
      
      # Compute intermediary metrics----
      data.table::set(ValidationMetrics, 
                      j = c("SquaredError","AbsoluteError","AbsolutePercentageError"),
                      value = list(ValidationMetrics[["Residuals"]] ^ 2L,
                                   abs(ValidationMetrics[["Residuals"]]),
                                   abs(ValidationMetrics[["Residuals"]])/(ValidationMetrics[["Target"]])))
      
      # Fill ExperimentGrid Table----
      data.table::set(ExperimentGrid, 
                      i = run,
                      j = c("Validate_MSE","Validate_MAE","Validate_MAPE"),
                      value = list(mean(ValidationMetrics[["SquaredError"]], na.rm = TRUE),
                                   mean(ValidationMetrics[["AbsoluteError"]], na.rm = TRUE),
                                   mean(ValidationMetrics[["AbsolutePercentageError"]], na.rm = TRUE)))          
    }
  } else {
    
    # Fill ExperimentGrid Table----
    data.table::set(ExperimentGrid,
                    i = run,
                    j = c("Validate_MSE","Validate_MAE","Validate_MAPE"),
                    value = list(NA, NA, NA))
  }
  
  # Blended Performance----
  data.table::set(ExperimentGrid, 
                  i = run,
                  j = c("Blended_MSE","Blended_MAE","Blended_MAPE"),
                  value = list(TrainValidateShare[1] * ExperimentGrid[run, Train_MSE] + TrainValidateShare[1] * ExperimentGrid[run, Validate_MSE],
                               TrainValidateShare[1] * ExperimentGrid[run, Train_MAE] + TrainValidateShare[1] * ExperimentGrid[run, Validate_MAE],
                               TrainValidateShare[1] * ExperimentGrid[run, Train_MAPE] + TrainValidateShare[1] * ExperimentGrid[run, Validate_MAPE]))
  
  # Return Grid----
  return(ExperimentGrid)
}

#' GenerateParameterGrids creates and stores model results in Experiment Grid
#' 
#' GenerateParameterGrids creates and stores model results in Experiment Grid
#'
#' @author Adrian Antico
#' @family Time Series
#' @param Model 'arima', 'ets', 'tbats', 'nnet', 'arfima', 'tslm', 'dshw'
#' @param test validation data
#' @param MinVal Minimum value of time series
#' @param DataSetName Passthrough
#' @param SeasonalDifferences Passthrough
#' @param SeasonalMovingAverages Passthrough
#' @param SeasonalLags Passthrough
#' @param MaxFourierTerms Passthrough
#' @param Differences Passthrough
#' @param MovingAverages Passthrough
#' @param Lags Passthrough
#' @export 
GenerateParameterGrids <- function(Model = NULL, 
                                   test = NULL,
                                   MinVal = NULL, 
                                   DataSetName = NULL,
                                   SeasonalDifferences = NULL,
                                   SeasonalMovingAverages = NULL,
                                   SeasonalLags = NULL,
                                   MaxFourierTerms = NULL,
                                   Differences = NULL,
                                   MovingAverages = NULL,
                                   Lags = NULL) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
  
  # Select Model----
  if(tolower(Model) == "arima") {
    
    # Arima Grid----
    if(MinVal < 0) {
      Grid <- data.table::CJ(
        DataSetName = DataSetName,
        BoxCox = c("skip"),
        IncludeDrift = c(FALSE,TRUE),
        SeasonalDifferences = c(0L, seq_len(SeasonalDifferences)),
        SeasonalMovingAverages = c(0L, seq_len(SeasonalMovingAverages)),
        SeasonalLags = c(0L, seq_len(SeasonalLags)),
        MaxFourierTerms = c(0L, seq_len(MaxFourierTerms)),
        Differences = c(0L, seq_len(Differences)),
        MovingAverages = c(0L, seq_len(MovingAverages)),
        Lags = c(0L, seq_len(Lags)))
    } else {
      Grid <- data.table::CJ(
        DataSetName = DataSetName,
        BoxCox = c("auto","skip"),
        IncludeDrift = c(FALSE,TRUE),
        SeasonalDifferences = c(0L, seq_len(SeasonalDifferences)),
        SeasonalMovingAverages = c(0L, seq_len(SeasonalMovingAverages)),
        SeasonalLags = c(0L, seq_len(SeasonalLags)),
        MaxFourierTerms = c(0L, seq_len(MaxFourierTerms)),
        Differences = c(0L, seq_len(Differences)),
        MovingAverages = c(0L, seq_len(MovingAverages)),
        Lags = c(0L, seq_len(Lags)))
    }
    
    # Grid info for Statification Parsimonous----
    l <- as.list(Grid[.N][, 4L:ncol(Grid)][1L,])
    TotalStratGrids <- max(Grid[, 4L:ncol(Grid)])
    
    # Add BiasAdj and GridName to Grid----
    data.table::set(Grid, j = "BiasAdj", value = data.table::fifelse(Grid[["BoxCox"]] == "auto", TRUE, FALSE))
    data.table::set(Grid, j = "GridName", value = "XXX")
    data.table::set(Grid, j = "ModelRunNumber", value = -10)
    
    # Create GridClusters List----
    GridClusters <- list()
    
    # Create ParsimonousGrid----
    GridClusters[["ParsimonousGrid"]] <- data.table::copy(Grid)
    
    # Create RandomGrid----
    data.table::set(Grid, j = "Random", value = runif(Grid[,.N]))
    data.table::setorderv(Grid, cols = "Random", order = 1L)
    data.table::set(Grid, j = "Random", value = NULL)
    GridClusters[["RandomGrid"]] <- data.table::copy(Grid)
    
    # Create Mutually Exclusive StratifyParsimonous Grids----
    for (i in seq_len(TotalStratGrids)) {
      if(i == 1) {
        GridClusters[[paste0("StratifyParsimonousGrid_",i)]] <- 
          Grid[MovingAverages <= min(i,l[["MovingAverages"]]) & Lags <= min(i,l[["Lags"]]) & 
                 SeasonalDifferences <= min(i,l[["SeasonalDifferences"]]) & 
                 SeasonalMovingAverages <= min(i,l[["SeasonalMovingAverages"]]) & SeasonalLags <= min(i,l[["SeasonalLags"]]) & 
                 MaxFourierTerms <= min(i,l[["MaxFourierTerms"]]) & Differences <= min(i,l[["Differences"]])][, temp := runif(.N)][order(temp)][, temp := NULL]
      } else {
        GridClusters[[paste0("StratifyParsimonousGrid_",i)]] <- data.table::fsetdiff(
          Grid[MovingAverages <= min(i,l[["MovingAverages"]]) & Lags <= min(i,l[["Lags"]]) & SeasonalDifferences <= min(i,l[["SeasonalDifferences"]]) & 
                 SeasonalMovingAverages <= min(i,l[["SeasonalMovingAverages"]]) & SeasonalLags <= min(i,l[["SeasonalLags"]]) & 
                 MaxFourierTerms <= min(i,l[["MaxFourierTerms"]]) & Differences <= min(i,l[["Differences"]])][, temp := runif(.N)][order(temp)][, temp := NULL],
          Grid[MovingAverages <= min(i-1L,l[["MovingAverages"]]) & Lags <= min(i-1L,l[["Lags"]]) & SeasonalDifferences <= min(i-1L,l[["SeasonalDifferences"]]) & 
                 SeasonalMovingAverages <= min(i-1,l[["SeasonalMovingAverages"]]) & SeasonalLags <= min(i-1,l[["SeasonalLags"]]) & 
                 MaxFourierTerms <= min(i-1L,l[["MaxFourierTerms"]]) & Differences <= min(i-1L,l[["Differences"]])][, temp := runif(.N)][order(temp)][, temp := NULL])
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
    return(list(
      Grid = Grid,
      GridClusters = GridClusters,
      ExperimentGrid = ExperimentGrid))
    
  } else if(tolower(Model) == "ets") {
    
    # ETS Grid----
    if(MinVal < 0) {
      Grid <- data.table::CJ(
        DataSetName = DataSetName,
        Damped = c(TRUE,FALSE),
        Lambda = c("skip"),
        ModelParam1 = c("A","M"),
        ModelParam2 = c("N","A","M"),
        ModelParam3 = c("N","A","M"))
    } else {
      Grid <- data.table::CJ(
        DataSetName = DataSetName,
        Damped = c(TRUE,FALSE),
        Lambda = c("auto","skip"),
        ModelParam1 = c("A","M"),
        ModelParam2 = c("N","A","M"),
        ModelParam3 = c("N","A","M"))
    }
    
    # Add BiasAdj and GridName to Grid----
    data.table::set(Grid, j = "BiasAdj", value = data.table::fifelse(Grid[["Lambda"]] == "auto", TRUE, FALSE))
    data.table::set(Grid, j = "GridName", value = "Custom")
    
    # Store grid in list----
    GridList <- as.list(Grid)
    
    # Add evaluation metrics columns and fill with dummy values----
    for(trainvalidate in c("Train_","Validate_","Blended_")) {
      for(tseval in c("MSE","MAE","MAPE")) data.table::set(Grid, j = paste0(trainvalidate,tseval), value = -10)
    }
    
    # Set up results grid to collect parameters tested and results----
    ExperimentGrid <- data.table::copy(Grid)
    for(i in seq_len(ncol(ExperimentGrid))[-1]) {
      if(i != 8L) {
        if(is.character(ExperimentGrid[[i]])) {
          data.table::set(ExperimentGrid, j = i, value = "xxx")
          data.table::set(ExperimentGrid, i = 1L, j = i, value = "AutoETS")
        } else if(is.numeric(ExperimentGrid[[i]]) | is.integer(ExperimentGrid[[i]])) {
          data.table::set(ExperimentGrid, j = i, value = -10)
          data.table::set(ExperimentGrid, i = 1L, j = i, value = -7)
        } else if(is.logical(ExperimentGrid[[i]])) {
          data.table::set(ExperimentGrid, j = i, value = FALSE)
        }
      }
    }
    data.table::set(ExperimentGrid, i = 1L, j= "GridName", value = "AutoETS")
    
    # HoldOutData----
    ValidationData <- data.table::copy(test)
    
    # Return objects----
    return(list(
      Grid = Grid, 
      GridList = GridList, 
      ExperimentGrid = ExperimentGrid,
      ValidationData = ValidationData))
  } else if(tolower(Model) == "tbats") {
    
    # TBATS Grid----
    if(MinVal < 0) {
      Grid <- data.table::CJ(
        DataSetName = DataSetName,
        Lambda = c(FALSE),
        Trend = c(TRUE,FALSE),
        Damped = c(TRUE,FALSE),
        SeasonalPeriods = c(0,1,2),
        UseARMAErrors = c(TRUE,FALSE),
        Lags = c(0L, Lags), 
        MovingAverages = c(0L, MovingAverages))
    } else {
      Grid <- data.table::CJ(
        DataSetName = DataSetName,
        Lambda = c(TRUE,FALSE),
        Trend = c(TRUE,FALSE),
        Damped = c(TRUE,FALSE),
        SeasonalPeriods = c(0L, 1L, 2L),
        UseARMAErrors = c(TRUE,FALSE),
        Lags = c(0L, Lags), 
        MovingAverages = c(0L, MovingAverages))
    }
    
    # Remove non options----
    Grid <- Grid[!(UseARMAErrors == FALSE & (Lags > 0L | MovingAverages > 0L))]
    
    # Add BiasAdj and GridName to Grid----
    data.table::set(Grid, j = "BiasAdj", value = data.table::fifelse(Grid[["Lambda"]] == "auto", TRUE, FALSE))
    data.table::set(Grid, j = "GridName", value = "Custom")
    
    # Store grid in list----
    GridList <- as.list(Grid)
    
    # Add evaluation metrics columns and fill with dummy values----
    for(trainvalidate in c("Train_","Validate_","Blended_")) {
      for(tseval in c("MSE","MAE","MAPE")) {
        data.table::set(Grid, j = paste0(trainvalidate,tseval), value = -10)
      }  
    }
    
    # Set up results grid to collect parameters tested and results----
    ExperimentGrid <- data.table::copy(Grid)
    for(i in seq_len(ncol(ExperimentGrid))[-1L]) {
      if(i != 8L) {
        if(is.character(ExperimentGrid[[i]])) {
          data.table::set(ExperimentGrid, j = i, value = "xxx")
          data.table::set(ExperimentGrid, i = 1L, j = i, value = "AutoTBATS")
        } else if(is.numeric(ExperimentGrid[[i]]) | is.integer(ExperimentGrid[[i]])) {
          data.table::set(ExperimentGrid, j = i, value = -10)
          data.table::set(ExperimentGrid, i = 1L, j = i, value = -7)
        } else if(is.logical(ExperimentGrid[[i]])) {
          data.table::set(ExperimentGrid, j = i, value = FALSE)
        }
      }
    }
    data.table::set(ExperimentGrid, i = 1L, j= "GridName", value = "AutoETS")
    
    # HoldOutData----
    ValidationData <- data.table::copy(test)
    
    # Return objects----
    return(list(
      Grid = Grid, 
      GridList = GridList, 
      ExperimentGrid = ExperimentGrid,
      ValidationData = ValidationData))
  } else if(tolower(Model) == "nnet") {
    
    # NNET Grid----
    if(MinVal < 0L) {
      Grid <- data.table::CJ(
        DataSetName = DataSetName,
        Scale = c(TRUE,FALSE),
        Lambda = c("skip"),
        LayerNodes = c(MaxFourierTerms + SeasonalLags + Lags, 
                       ceiling(sqrt(MaxFourierTerms + SeasonalLags + Lags)), 
                       ceiling((MaxFourierTerms + SeasonalLags + Lags)^0.90)),
        Repeats = c(seq_len(20L)),
        MaxFourierTerms = c(0L, seq_len(MaxFourierTerms)),
        SeasonalLags = c(0L, max(SeasonalLags)),
        Lags = c(0L, seq_len(Lags)))
    } else {
      Grid <- data.table::CJ(
        DataSetName = DataSetName,
        Scale = c(TRUE,FALSE),
        Lambda = c("auto","skip"),
        LayerNodes = c(MaxFourierTerms + SeasonalLags + Lags, 
                       ceiling(sqrt(MaxFourierTerms + SeasonalLags + Lags)), 
                       ceiling((MaxFourierTerms + SeasonalLags + Lags)^0.90)),
        Repeats = c(seq_len(20L)),
        MaxFourierTerms = c(0L, seq_len(MaxFourierTerms)),
        SeasonalLags = c(0L, max(SeasonalLags)),
        Lags = c(0L, seq_len(Lags)))
    }
    
    # Grid info for Statification Parsimonous----
    l <- as.list(Grid[.N][,4:ncol(Grid)][1,])
    TotalStratGrids <- max(Grid[,6:ncol(Grid)])
    
    # Remove non options----
    Grid <- Grid[!(MaxFourierTerms == 0 & SeasonalLags == 0 & Lags == 0)]
    
    # Add BiasAdj and GridName to Grid----
    data.table::set(Grid, j = "BiasAdj", value = data.table::fifelse(Grid[["Lambda"]] == "auto", TRUE, FALSE))
    data.table::set(Grid, j = "GridName", value = "Custom")
    
    # Create GridClusters List----
    GridClusters <- list()
    
    # Create ParsimonousGrid----
    GridClusters[["ParsimonousGrid"]] <- data.table::copy(Grid)
    
    # Create RandomGrid----
    data.table::set(Grid, j = "Random", value = runif(Grid[,.N]))
    data.table::setorderv(Grid, cols = "Random", order = 1)
    data.table::set(Grid, j = "Random", value = NULL)
    GridClusters[["RandomGrid"]] <- data.table::copy(Grid)
    
    # Create Mutually Exclusive StratifyParsimonous Grids----
    for (i in seq_len(TotalStratGrids)) {
      if(i == 1) {
        GridClusters[[paste0("StratifyParsimonousGrid_",i)]] <- 
          Grid[(Lags <= min(i,l[["Lags"]]) & SeasonalLags <= min(i,l[["SeasonalLags"]]) & 
                  MaxFourierTerms <= min(i,l[["MaxFourierTerms"]]))][, temp := runif(.N)][order(temp)][, temp := NULL]
      } else {
        GridClusters[[paste0("StratifyParsimonousGrid_",i)]] <- data.table::fsetdiff(
          Grid[(Lags <= min(i,l[["Lags"]]) & SeasonalLags <= min(i,l[["SeasonalLags"]]) & 
                  MaxFourierTerms <= min(i,l[["MaxFourierTerms"]]))][, temp := runif(.N)][order(temp)][, temp := NULL],
          Grid[(Lags <= min(i-1,l[["Lags"]]) & SeasonalLags <= min(i-1,l[["SeasonalLags"]]) & 
                  MaxFourierTerms <= min(i-1,l[["MaxFourierTerms"]]))][, temp := runif(.N)][order(temp)][, temp := NULL])
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
        data.table::set(ExperimentGrid, i = 1L, j = i, value = "AutoNNET")
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
  } else if(tolower(Model) == "arfima") {
    
    # ARFIMA Grid----
    if(MinVal < 0) {
      Grid <- data.table::CJ(
        DataSetName = DataSetName,
        Lambda = c("skip"),
        Lags = c(0,Lags), 
        MovingAverages = c(0,MovingAverages),
        Drange = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90))
    } else {
      Grid <- data.table::CJ(
        DataSetName = DataSetName,
        Lambda = c("auto","skip"),
        Lags = c(0,Lags), 
        MovingAverages = c(0,MovingAverages),
        Drange = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90))
    }
    
    # Add BiasAdj and GridName to Grid----
    data.table::set(Grid, j = "BiasAdj", value = data.table::fifelse(Grid[["Lambda"]] == "auto", TRUE, FALSE))
    data.table::set(Grid, j = "GridName", value = "Custom")
    
    # Store grid in list----
    GridList <- as.list(Grid)
    
    # Add evaluation metrics columns and fill with dummy values----
    for(trainvalidate in c("Train_","Validate_","Blended_")) {
      for(tseval in c("MSE","MAE","MAPE")) {
        data.table::set(Grid, j = paste0(trainvalidate,tseval), value = -10)
      }  
    }
    
    # Set up results grid to collect parameters tested and results----
    ExperimentGrid <- data.table::copy(Grid)
    for(i in seq_len(ncol(ExperimentGrid))[-1]) {
      if(i != 8) {
        if(is.character(ExperimentGrid[[i]])) {
          data.table::set(ExperimentGrid, j = i, value = "xxx")
          data.table::set(ExperimentGrid, i = 1L, j = i, value = "AutoArfima")
        } else if(is.numeric(ExperimentGrid[[i]]) | is.integer(ExperimentGrid[[i]])) {
          data.table::set(ExperimentGrid, j = i, value = -10)
          data.table::set(ExperimentGrid, i = 1L, j = i, value = -7)
        } else if(is.logical(ExperimentGrid[[i]])) {
          data.table::set(ExperimentGrid, j = i, value = FALSE)
        }
      }
    }
    data.table::set(ExperimentGrid, i = 1L, j= "GridName", value = "AutoETS")
    
    # HoldOutData----
    ValidationData <- data.table::copy(test)
    
    # Return objects----
    return(
      list(
        Grid = Grid, 
        GridList = GridList, 
        ExperimentGrid = ExperimentGrid,
        ValidationData = ValidationData))
  } else if(tolower(Model) == "tslm") {
    
    # TSLM Grid----
    if(MinVal < 0L) {
      Grid <- data.table::CJ(
        DataSetName = DataSetName,
        Lambda = c("skip"))
    } else {
      Grid <- data.table::CJ(
        DataSetName = DataSetName,
        Lambda = c("auto","skip"))
    }
    
    # Add BiasAdj and GridName to Grid----
    data.table::set(Grid, j = "BiasAdj", value = data.table::fifelse(Grid[["Lambda"]] == "auto", TRUE, FALSE))
    data.table::set(Grid, j = "GridName", value = "Custom")
    
    # Store grid in list----
    GridList <- as.list(Grid)
    
    # Add evaluation metrics columns and fill with dummy values----
    for(trainvalidate in c("Train_","Validate_","Blended_")) {
      for(tseval in c("MSE","MAE","MAPE")) {
        data.table::set(Grid, j = paste0(trainvalidate,tseval), value = -10)
      }  
    }
    
    # Set up results grid to collect parameters tested and results----
    ExperimentGrid <- data.table::copy(Grid)
    for(i in seq_len(ncol(ExperimentGrid))[-1]) {
      if(i != 8) {
        if(is.character(ExperimentGrid[[i]])) {
          data.table::set(ExperimentGrid, j = i, value = "xxx")
          data.table::set(ExperimentGrid, i = 1L, j = i, value = "AutoTSLM")
        } else if(is.numeric(ExperimentGrid[[i]]) | is.integer(ExperimentGrid[[i]])) {
          data.table::set(ExperimentGrid, j = i, value = -10)
          data.table::set(ExperimentGrid, i = 1L, j = i, value = -7)
        } else if(is.logical(ExperimentGrid[[i]])) {
          data.table::set(ExperimentGrid, j = i, value = FALSE)
        }
      }
    }
    data.table::set(ExperimentGrid, i = 1L, j= "GridName", value = "AutoETS")
    
    # HoldOutData----
    ValidationData <- data.table::copy(test)
    
    # Return objects----
    return(
      list(
        Grid = Grid, 
        GridList = GridList, 
        ExperimentGrid = ExperimentGrid,
        ValidationData = ValidationData))
  }
}

#' TimeSeriesDataPrepare is a function that takes raw data and returns time series data
#' 
#' TimeSeriesDataPrepare is a function that takes raw data and returns the necessary time series data and objects for model building. It also fills any time gaps with zeros. Use this before you run any time series model functions.
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param data Source data.table for forecasting
#' @param TargetName Name of your target variable
#' @param DateName Name of your date variable
#' @param Lags The max number of lags you want to test
#' @param SeasonalLags The max number of seasonal lags you want to test
#' @param MovingAverages The max number of moving average terms
#' @param SeasonalMovingAverages The max number of seasonal moving average terms
#' @param TimeUnit The level of aggregation your dataset comes in. Choices include: 1Min, 5Min, 10Min, 15Min, and 30Min, hour, day, week, month, quarter, year
#' @param FCPeriods The number of forecast periods you want to have forecasted
#' @param HoldOutPeriods The number of holdout samples to compare models against
#' @param TSClean TRUE or FALSE. TRUE will kick off a time series cleaning operation. Outliers will be smoothed and imputation will be conducted.
#' @param ModelFreq TRUE or FALSE. TRUE will enable a model-based time frequency calculation for an alternative frequency value to test models on.
#' @param FinalBuild Set to TRUE to create data sets with full data
#' @return Time series data sets to pass onto auto modeling functions
#' @examples
#' data <- data.table::fread(file.path(PathNormalizer("C:\\Users\\aantico\\Documents\\Package\\data"),"tsdata.csv"))
#' TimeSeriesDataPrepare(
#'   data = data, 
#'   TargetName = "Weekly_Sales",
#'   DateName = "Date", 
#'   Lags = 5, 
#'   MovingAverages,
#'   SeasonalMovingAverages,
#'   SeasonalLags = 1,
#'   TimeUnit = "week",
#'   FCPeriods = 10, 
#'   HoldOutPeriods = 10,
#'   TSClean = TRUE, 
#'   ModelFreq = TRUE,
#'   FinalBuild = FALSE)
#' @export
TimeSeriesDataPrepare <- function(data, 
                                  TargetName,
                                  DateName,
                                  Lags,
                                  SeasonalLags,
                                  MovingAverages,
                                  SeasonalMovingAverages,
                                  TimeUnit, 
                                  FCPeriods, 
                                  HoldOutPeriods, 
                                  TSClean = TRUE, 
                                  ModelFreq = TRUE,
                                  FinalBuild = FALSE) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
  
  # Turn off warnings----
  options(warn = -1L)
  
  # Convert to data.table if not already
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  
  # Time series fill----
  data <- TimeSeriesFill(
    data = data, 
    DateColumnName = DateName, 
    GroupVariables = NULL, 
    TimeUnit = TimeUnit, 
    FillType = "Inner")
  
  # Ensure correct ordering and subsetting of data
  keep <- c(DateName, TargetName)
  data <- data[, ..keep]
  
  # Convert to lubridate as_date() or POSIXct----
  if(!tolower(TimeUnit) %chin% tolower(c("1min","1mins","5min","5mins","10min","10mins","15min","15mins","30min","30mins","hour","hours","hr","hrs"))) {
    data[, eval(DateName) := lubridate::as_date(get(DateName))]
  } else {
    data[, eval(DateName) := as.POSIXct(get(DateName))]
  }
  
  # Correct ordering----
  if(is.numeric(data[[1L]]) | is.integer(data[[1L]])) data.table::setcolorder(data, c(2L, 1L))
  
  # Check for min value of data----
  MinVal <- min(data[[2L]])
  
  # Ensure data is sorted----
  data.table::setorderv(x = data, cols = eval(DateName), order = 1L)
  
  # Change Target Name----
  TempTargetName <- TargetName
  data.table::setnames(data, paste0(eval(TargetName)), "Target")
  TargetName <- "Target"
  
  # Create data----
  if(!FinalBuild) {
    data_train <- data[1L:(nrow(data) - HoldOutPeriods)]
    data_test <- data[(nrow(data) - HoldOutPeriods + 1L):nrow(data)]
  } else {
    data_train <- data
    data_test <- NULL
  }
  
  # Data for fourier features----
  data_test_fourier <- data
  
  # Check for different time aggregations----
  MaxDate <- data[, max(get(DateName))]
  FC_Data <- data.table::data.table(Date = seq_len(FCPeriods))
  
  # Define TS Frequency----
  if(tolower(TimeUnit) == "hour") {
    UserSuppliedFreq <- 24
    FC_Data[, Date := MaxDate + lubridate::hours(Date)]
  } else if(tolower(TimeUnit) == "1min") {
    UserSuppliedFreq <- 60
    FC_Data[, Date := MaxDate + lubridate::minutes(Date)]
  } else if(tolower(TimeUnit) == "5min") {
    UserSuppliedFreq <- 12
    FC_Data[, Date := MaxDate + lubridate::minutes(5 * Date)]
  } else if(tolower(TimeUnit) == "10min") {
    UserSuppliedFreq <- 6
    FC_Data[, Date := MaxDate + lubridate::minutes(10 * Date)]
  } else if(tolower(TimeUnit) == "15min") {
    UserSuppliedFreq <- 4
    FC_Data[, Date := MaxDate + lubridate::minutes(15 * Date)]
  } else if(tolower(TimeUnit) == "30min") {
    UserSuppliedFreq <- 2
    FC_Data[, Date := MaxDate + lubridate::minutes(30 * Date)]
  } else if (tolower(TimeUnit) == "day") {
    UserSuppliedFreq <- 365
    FC_Data[, Date := MaxDate + lubridate::days(Date)]
  } else if (tolower(TimeUnit) == "week") {
    UserSuppliedFreq <- 52
    FC_Data[, Date := MaxDate + lubridate::weeks(Date)]
  } else if (tolower(TimeUnit) == "month") {
    UserSuppliedFreq <- 12
    FC_Data[, Date := as.Date(MaxDate) %m+% months(Date)]
  } else if (tolower(TimeUnit) == "quarter") {
    UserSuppliedFreq <- 4
    FC_Data[, Date := as.Date(MaxDate)  %m+% months(3 * Date)]
  } else if (tolower(TimeUnit) == "year") {
    UserSuppliedFreq <- 1
    FC_Data[, Date := MaxDate + lubridate::years(Date)]
  } else {
    return("TimeUnit is not in hour, day, week, month,
    quarter, or year")
  }
  
  # Coerce SeasonalLags if too large----
  if(UserSuppliedFreq * SeasonalLags > nrow(data_train)) SeasonalLags <- floor(nrow(data_train) / UserSuppliedFreq)
  
  # Coerce SeasonalMovingAverages----
  if(UserSuppliedFreq * SeasonalMovingAverages > nrow(data_train)) SeasonalMovingAverages <- floor(nrow(data_train) / UserSuppliedFreq)
  
  # User Supplied Frequency
  UserSuppliedData <- stats::ts(
    data = data_train,
    start = data_train[, min(get(DateName))][[1L]],
    frequency = UserSuppliedFreq)[, TargetName]
  UserSuppliedDiff <- tryCatch({forecast::ndiffs(x = UserSuppliedData)},error = function(x) 0L)
  UserSuppliedSeasonalDiff <- tryCatch({forecast::nsdiffs(x = UserSuppliedData)},error = function(x) 0L)
  
  # TSClean Version----
  if(TSClean) {
    if(MinVal > 0) {
      TSCleanData <- forecast::tsclean(
        x = UserSuppliedData,
        replace.missing = TRUE,
        lambda = "auto")
    } else {
      TSCleanData <- forecast::tsclean(
        x = UserSuppliedData,
        replace.missing = TRUE,
        lambda = NULL)
    }
    TSCleanDiff <- tryCatch({forecast::ndiffs(x = TSCleanData)},error = function(x) 0L)
    TSCleanSeasonalDiff <- tryCatch({forecast::nsdiffs(x = TSCleanData)},error = function(x) 0L)
  }
  
  # Model-Based Frequency----
  if(ModelFreq) {
    ModelFreqFrequency <- forecast::findfrequency(data_train[, get(names(data_train)[2L])])
    ModelFreqData <-
      stats::ts(data = data_train,
                start = data_train[, min(get(DateName))][[1]],
                frequency = ModelFreqFrequency)[, TargetName]
    
    ModelFreqDiff <- tryCatch({forecast::ndiffs(x = ModelFreqData)},error = function(x) 0L)
    ModelFreqSeasonalDiff <- tryCatch({forecast::nsdiffs(x = ModelFreqData)},error = function(x) 0L)
  }
  
  # TSClean & ModelFreq Version----
  if(TSClean & ModelFreq) {
    if(MinVal > 0) {
      TSCleanModelFreqData <- forecast::tsclean(x = ModelFreqData,
                                                replace.missing = TRUE,
                                                lambda = "auto")
    } else {
      TSCleanModelFreqData <- forecast::tsclean(x = ModelFreqData,
                                                replace.missing = TRUE,
                                                lambda = NULL)
    }
    
    # Differencing----
    TSCleanModelFreqDiff <- tryCatch({forecast::ndiffs(x = TSCleanModelFreqData)},error = function(x) 0L)
    TSCleanModelFreqSeasonalDiff <- tryCatch({forecast::nsdiffs(x = TSCleanModelFreqData)},error = function(x) 0L)
  }
  
  # Return time series artifacts----
  return(list(
    TestData = tryCatch({data_test}, error = function(x) NULL),
    FullData = tryCatch({data_test_fourier}, error = function(x) NULL),
    UserSuppliedData = tryCatch({UserSuppliedData}, error = function(x) NULL),
    ModelFreqData = tryCatch({ModelFreqData}, error = function(x) NULL),
    TSCleanData = tryCatch({TSCleanData}, error = function(x) NULL),
    TSCleanModelFreqData = tryCatch({TSCleanModelFreqData}, error = function(x) NULL), 
    UserSuppliedDiff = tryCatch({UserSuppliedDiff}, error = function(x) NULL),
    UserSuppliedSeasonalDiff = tryCatch({UserSuppliedSeasonalDiff}, error = function(x) NULL),
    TSCleanDiff = tryCatch({TSCleanDiff}, error = function(x) NULL),
    TSCleanSeasonalDiff = tryCatch({TSCleanSeasonalDiff}, error = function(x) NULL),
    ModelFreqDiff = tryCatch({ModelFreqDiff}, error = function(x) NULL),
    ModelFreqSeasonalDiff = tryCatch({ModelFreqSeasonalDiff}, error = function(x) NULL),
    TSCleanModelFreqDiff = tryCatch({TSCleanModelFreqDiff}, error = function(x) NULL),
    TSCleanModelFreqSeasonalDiff = tryCatch({TSCleanModelFreqSeasonalDiff}, error = function(x) NULL),
    Lags = tryCatch({Lags}, error = function(x) NULL),
    SeasonalLags = tryCatch({SeasonalLags}, error = function(x) NULL),
    MovingAverages = tryCatch({MovingAverages}, error = function(x) NULL),
    SeasonalMovingAverages = tryCatch({SeasonalMovingAverages}, error = function(x) NULL),
    HoldOutPeriods = HoldOutPeriods,
    FCPeriods = FCPeriods,
    TargetName = TargetName,
    DateName = DateName,
    TempTargetName = TempTargetName,
    TSClean = tryCatch({TSClean}, error = function(x) FALSE),
    ModelFreq = tryCatch({ModelFreq}, error = function(x) FALSE),
    MinVal = tryCatch({MinVal}, error = function(x) NULL),
    UserSuppliedFrequency = tryCatch({UserSuppliedFreq}, error = function(x) NULL),
    ModelFreqFrequency = tryCatch({ModelFreqFrequency}, error = function(x) NULL),
    MaxDate = tryCatch({MaxDate}, error = function(x) NULL),
    FC_Data = tryCatch({FC_Data}, error = function(x) NULL),
    data = tryCatch({data}, error = function(x) NULL)))
}

#' OptimizeArima is a function that takes raw data and returns time series data
#' 
#' OptimizeArima is a function that takes raw data and returns the necessary time series data and objects for model building. It also fills any time gaps with zeros. Use this before you run any time series model functions.
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param Output This is passed through as output from TimeSeriesDataPrepare() and passed through ParallelArima()
#' @param MetricSelection Select from "MSE", "MAE", or "MAPE"
#' @param DataSetName This is the name of the data set passed through in parallel loop
#' @param train Training data returned from TimeSeriesDataPrepare()
#' @param test Test data returned from TimeSeriesDataPrepare()
#' @param FullData Full series data for scoring and ensemble
#' @param HoldOutPeriods Holdout periods returned from TimeSeriesDataPrepare() 
#' @param MinVal Minimum value of target variable returned from TimeSeriesDataPrepare()
#' @param TargetName Target variable name returned from TimeSeriesDataPrepare()
#' @param DateName Date variable name returned from TimeSeriesDataPrepare()
#' @param Lags Max value of lag returned from TimeSeriesDataPrepare()
#' @param SeasonalLags Max value of seasonal lags returned from TimeSeriesDataPrepare()
#' @param MovingAverages Max value of moving averages
#' @param SeasonalMovingAverages Max value of seasonal moving average
#' @param Differences Max value of difference returned from TimeSeriesDataPrepare()
#' @param SeasonalDifferences Max value of seasonal difference returned from TimeSeriesDataPrepare()
#' @param MaxFourierTerms Max value of fourier pairs
#' @param TrainValidateShare A two-element numeric vector. The first element is the weight applied to the training performance and the remainder is applied to the validation performance.
#' @param MaxRunsWithoutNewWinner The number of runs without a new winner which if passed tells the function to stop
#' @param MaxNumberModels The number of models you want to test.
#' @param FinalGrid If NULL, regular train optimization occurs. If the grid is supplied, final builds are conducted.
#' @return Time series data sets to pass onto auto modeling functions
#' @examples
#' Results <- OptimizeArima(
#'   Output,
#'   MetricSelection = "MAE",
#'   DataSetName = NULL,
#'   train = NULL,
#'   test = NULL,
#'   FullData = NULL,
#'   HoldOutPeriods = NULL,
#'   MinVal = NULL,
#'   TargetName = NULL,
#'   DateName = NULL,
#'   Lags = NULL,
#'   SeasonalLags = NULL,
#'   MovingAverages = NULL,
#'   SeasonalMovingAverages = NULL,
#'   Differences = NULL,
#'   SeasonalDifferences = NULL,
#'   MaxFourierTerms = NULL,
#'   TrainValidateShare = NULL,
#'   MaxRunsWithoutNewWinner = 20,
#'   MaxNumberModels = 5,
#'   MaxRunMinutes = NULL,
#'   FinalGrid = NULL)
#' @export
OptimizeArima <- function(Output,
                          MetricSelection = "MAE",
                          DataSetName = NULL,
                          train = NULL,
                          test = NULL,
                          FullData = NULL,
                          HoldOutPeriods = NULL,
                          MinVal = NULL,
                          TargetName = NULL,
                          DateName = NULL,
                          Lags = NULL, 
                          SeasonalLags = NULL,
                          MovingAverages = NULL,
                          SeasonalMovingAverages = NULL,
                          Differences = NULL,
                          SeasonalDifferences = NULL,
                          MaxFourierTerms = NULL,
                          TrainValidateShare = NULL,
                          MaxRunsWithoutNewWinner = 20,
                          MaxNumberModels = NULL,
                          MaxRunMinutes = NULL,
                          FinalGrid = NULL) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2L))
  
  # Go to scoring model if FinalGrid is supplied----
  if(is.null(FinalGrid)) {
    
    # Get grid objects----
    GridObjects <- GenerateParameterGrids(
      Model = "arima", 
      MinVal = Output$MinVal, 
      DataSetName = DataSetName,
      SeasonalDifferences = SeasonalDifferences,
      SeasonalMovingAverages = SeasonalMovingAverages,
      SeasonalLags = SeasonalLags,
      MaxFourierTerms = MaxFourierTerms,
      Differences = Differences,
      MovingAverages = MovingAverages,
      Lags = Lags)
    Grid <- GridObjects[["Grid"]]
    GridClusters <- GridObjects[["GridClusters"]]
    ExperimentGrid <- GridObjects[["ExperimentGrid"]]
    rm(GridObjects)
    
    # Initialize RL----
    RL_Start <- RL_Initialize(
      ParameterGridSet = GridClusters, 
      Alpha = 1, 
      Beta = 1, 
      SubDivisions = 1000L)
    BanditArmsN <- RL_Start[["BanditArmsN"]]
    Successes <- RL_Start[["Successes"]]
    Trials <- RL_Start[["Trials"]]
    GridIDs <- RL_Start[["GridIDs"]]
    BanditProbs <- RL_Start[["BanditProbs"]]
    rm(RL_Start)
    
    # Add bandit probs columns to ExperimentGrid----
    data.table::set(ExperimentGrid, j = paste0("BanditProbs_",names(GridClusters)), value = -10)
    
    # HoldOutData----
    ValidationData <- data.table::copy(test)
    
    # Intitalize Counter----
    run <- 0L
    
    # Initialize TotalRunTime----
    TotalRunTime <- 0
    
    # Sample from bandit to select next grid row----
    NewGrid <- 1L
    RunsWithoutNewWinner <- 0L
    
    # Build models----
    repeat {
      
      # Increment Counter----
      run <- run + 1L
      
      # Select new grid----
      if(run <= BanditArmsN + 1L) {
        if(run != 1L) NextGrid <- GridClusters[[names(GridClusters)[run-1L]]][1L]
      } else {
        NextGrid <- as.list(GridClusters[[names(GridClusters)[NewGrid]]][Trials[NewGrid]+1L])
      }
      
      # Update Grid Column Values----
      if(run == 1L) {
        data.table::set(ExperimentGrid, i = run, j = "GridName", value = "DefaultAutoArima")
      } else {
        for(cols in 1L:12L) {
          if(cols == 1L) {
            data.table::set(ExperimentGrid, i = run, j = cols, value = GridClusters[[names(GridClusters)[NewGrid]]][["DataSetName"]][Trials[NewGrid]+1L])
          } else if(cols == 12L) {
            data.table::set(ExperimentGrid, i = run, j = cols, value = names(GridClusters)[NewGrid])
          } else {
            # Grab correct cluster group, cluster column, and cluster row
            data.table::set(ExperimentGrid, i = run, j = cols, value = GridClusters[[names(GridClusters)[NewGrid]]][[cols]][Trials[NewGrid]+1L])
          }
        }
        
        # Fill bandit probabilities
        banditindex <- 0L
        for(BanditCols in c(ncol(ExperimentGrid)-BanditArmsN):(ncol(ExperimentGrid)-1L)) {
          banditindex <- banditindex + 1L 
          data.table::set(ExperimentGrid, i = run, j = BanditCols, value = round(BanditProbs[banditindex], 2L))
        }
      }
      
      # Define lambda----
      if(run != 1L) {
        tryCatch({if(NextGrid$BoxCox[1L] == "skip") {
          lambda <- NULL
        } else {
          lambda <- "auto"
        }}, error = function(x) lambda <- NULL)
      }
      
      # Define Fourier Terms----
      if(run != 1) {
        if(NextGrid[["MaxFourierTerms"]][1L] == 0L) {
          XREG <- FALSE
          XREGFC <- FALSE
        } else {
          XREG <- tryCatch({forecast::fourier(train, K = NextGrid[["MaxFourierTerms"]][1L])}, error = function(x) FALSE)
          XREGFC <- tryCatch({forecast::fourier(train, K = NextGrid[["MaxFourierTerms"]][1L], h = HoldOutPeriods)}, error = function(x) FALSE)
        }
      }
      
      # Start time---
      Start <- Sys.time()
      
      # Build Models----
      if(run == 1L) {
        if(MinVal > 0) {
          Results <- forecast::auto.arima(
            y=train,max.p=Lags,max.q=MovingAverages,max.P=SeasonalLags,max.Q=SeasonalMovingAverages,max.d=Differences,max.D=SeasonalDifferences,
            ic="aicc",lambda=TRUE,biasadj=TRUE,stepwise=TRUE,parallel=FALSE,1L)
        } else {
          Results <- forecast::auto.arima(
            y=train,max.p=Lags,max.q=MovingAverages,max.P=SeasonalLags,max.Q=SeasonalMovingAverages,max.d=Differences,max.D=SeasonalDifferences,
            ic="aicc",lambda=FALSE,biasadj=FALSE,stepwise=TRUE,parallel=FALSE,num.cores=1L)
        }
      } else {
        if(!is.numeric(XREG) & !is.numeric(XREGFC)) {
          Results <- tryCatch({forecast::Arima(
            train, 
            order = c(NextGrid[["Lags"]][1L], NextGrid[["Differences"]][1], NextGrid[["MovingAverages"]][1]), 
            seasonal = c(NextGrid[["SeasonalLags"]][1L], NextGrid[["SeasonalDifferences"]][1], NextGrid[["SeasonalMovingAverages"]][1]), 
            include.drift = NextGrid$IncludeDrift[1L],
            lambda = lambda,  
            biasadj = NextGrid$BiasAdj[1])},
            error = function(x) NULL)
        } else {
          Results <- tryCatch({forecast::Arima(
            train, 
            order = c(NextGrid[["Lags"]][1], NextGrid[["Differences"]][1L], NextGrid[["MovingAverages"]][1L]), 
            seasonal = c(NextGrid[["SeasonalLags"]][1L], NextGrid[["SeasonalDifferences"]][1L], NextGrid[["SeasonalMovingAverages"]][1L]), 
            include.drift = NextGrid$IncludeDrift[1L], 
            lambda = lambda, 
            xreg = XREG,
            biasadj = NextGrid$BiasAdj[1L])},
            error = function(x) NULL)
        }
      }
      
      # End time---
      End <- Sys.time()    
      
      # Performance Metrics----
      tryCatch({ExperimentGrid <- RL_Performance(
        Results = Results,
        NextGrid = NextGrid,
        TrainValidateShare = TrainValidateShare,
        MaxFourierTerms = NextGrid[["MaxFourierTerms"]][1L],
        XREGFC = XREGFC,
        ExperimentGrid = ExperimentGrid,
        run = run, 
        train = train,
        ValidationData = ValidationData,
        HoldOutPeriods = HoldOutPeriods)}, error = function(x) NULL)
      
      # Add run time to ExperimentGrid----
      data.table::set(ExperimentGrid, i = run, j = "RunTime", value = End - Start)
      TotalRunTime <- TotalRunTime + as.numeric((End - Start))
      
      # RL Update----
      tryCatch({RL_Update_Output <- RL_Update(
        ExperimentGrid = ExperimentGrid,
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
        BanditProbabilities = BanditProbs)
      
      #
      # ExperimentGrid = ExperimentGrid
      # MetricSelection = MetricSelection
      # ModelRun = run
      # NEWGrid = NewGrid
      # TrialVector = Trials
      # SuccessVector = Successes
      # GridIDS = GridIDs
      # BanditArmsCount = BanditArmsN
      # RunsWithoutNewWinner = RunsWithoutNewWinner
      # MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
      # MaxNumberModels = MaxNumberModels
      # MaxRunMinutes = MaxRunMinutes
      # TotalRunTime = TotalRunTime
      # BanditProbabilities = BanditProbs
      # 
      
      BanditProbs <- RL_Update_Output[["BanditProbs"]]
      Trials <- RL_Update_Output[["Trials"]]
      Successes <- RL_Update_Output[["Successes"]]
      NewGrid <- RL_Update_Output[["NewGrid"]]
      Break <- RL_Update_Output[["BreakLoop"]]}, error = function(x) NULL)
      
      # Exit repeat loop upon conditions----
      if(Break == "exit") break
    }
    
    # Remove Invalid Columns----
    ResultsGrid <- ExperimentGrid[!is.na(Blended_MAE) & SeasonalLags != -10]
    
    # Add Rank Values----
    ResultsGrid <- ResultsGrid[, ModelRunNumber := seq_len(ResultsGrid[, .N])][order(Blended_MAE)][, ModelRankByDataType := seq_len(ResultsGrid[, .N])][order(ModelRunNumber)]
    
    # Return Results----
    return(ResultsGrid)
    
    # Forecast Code
  } else {
    
    # Remove Validation Metrics but Fill in the Train Metrics to Compare Against Initial Train----
    FinalGrid[, ':=' (Validate_MSE = NULL, Validate_MAE = NULL, Blended_MSE = NULL, Blended_MAE = NULL, Blended_MAPE = NULL)]
    
    # Create list to extract elements for modeling----
    TSGridList <- as.list(FinalGrid)
    
    # Train number of rows----
    TrainRows <- length(train)
    
    # Build models----
    for(run in seq_len(FinalGrid[, .N])) {
      
      # Define lambda----
      if(TSGridList$BoxCox[run] == "skip") {
        lambda <- NULL
      } else {
        lambda <- "auto"
      }
      
      # Build final models----
      if(FinalGrid[1,GridName] == "DefaultAutoArima") {
        if(Output$MinVal > 0) {
          Results <- forecast::auto.arima(
            y=train,max.p=Lags,max.q=MovingAverages,max.P=SeasonalLags,max.Q=SeasonalMovingAverages,max.d=Differences,max.D=SeasonalDifferences,
            ic="aicc",lambda=TRUE,biasadj=TRUE,stepwise=TRUE,parallel=TRUE,num.cores=parallel::detectCores())
        } else {
          Results <- forecast::auto.arima(
            y=train,max.p=Lags,max.q=MovingAverages,max.P=SeasonalLags,max.Q=SeasonalMovingAverages,max.d=Differences,max.D=SeasonalDifferences,
            ic="aicc",lambda=FALSE,biasadj=FALSE,stepwise=TRUE,parallel=TRUE,num.cores=1L)
        }
      } else {
        if(TSGridList[["MaxFourierTerms"]][run] != 0) {
          XREG <- tryCatch({forecast::fourier(train, K = TSGridList[["MaxFourierTerms"]][run])}, error = function(x) FALSE)
          XREGFC <- tryCatch({forecast::fourier(train, K = TSGridList[["MaxFourierTerms"]][run], h = FCPeriods)}, error = function(x) FALSE)
          if(!is.logical(XREG) & !is.logical(XREGFC)) {
            Results <- tryCatch({forecast::Arima(
              train, 
              order = c(TSGridList[["Lags"]][run], TSGridList[["Differences"]][run], TSGridList[["MovingAverages"]][run]), 
              seasonal = c(TSGridList[["SeasonalLags"]][run], TSGridList[["SeasonalDifferences"]][run], TSGridList[["SeasonalMovingAverages"]][run]), 
              xreg = XREG,
              include.drift = TSGridList$IncludeDrift[run], 
              lambda = lambda, 
              biasadj = TSGridList$BiasAdj[run])},
              error = function(x) NULL)
          } else {
            Results <- NULL
          }
        } else {
          Results <- tryCatch({forecast::Arima(
            train, 
            order = c(TSGridList[["Lags"]][run], TSGridList[["Differences"]][run], TSGridList[["MovingAverages"]][run]), 
            seasonal = c(TSGridList[["SeasonalLags"]][run], TSGridList[["SeasonalDifferences"]][run], TSGridList[["SeasonalMovingAverages"]][run]), 
            include.drift = TSGridList$IncludeDrift[run],
            lambda = lambda,  
            biasadj = TSGridList$BiasAdj[run])},
            error = function(x) NULL)
        }
      }
      
      # Collect Forecast Inputs----
      FC_Data <- data.table::copy(Output$FC_Data)
      FC_Data[, Target := NA]
      FCPeriods <- Output$FCPeriods
      Train_Score <- data.table::copy(Output$FullData)
      Train_Score[, Target := as.numeric(Target)]
      
      # Generate Forecasts for Forecast Periods----
      if(!is.null(Results)) {
        
        # Score Training Data for Full Set of Predicted Values----
        Train_Score[, Forecast := as.numeric(Results$fitted)]
        
        # Forecast----
        tryCatch({FC_Data[, Forecast := as.numeric(forecast::forecast(Results, h = FCPeriods, xreg = XREGFC)$mean)]}, error = function(x) {
          FC_Data[, Forecast := as.numeric(forecast::forecast(Results, h = FCPeriods)$mean)]
        })
        tryCatch({FC_Data[, Low95 := as.numeric(forecast::forecast(Results, h = FCPeriods, xreg = XREGFC)$lower)[1:FCPeriods]]}, error = function(x) {
          FC_Data[, Low95 := as.numeric(forecast::forecast(Results, h = FCPeriods)$lower)[1:FCPeriods]]
        })
        tryCatch({FC_Data[, Low80 := as.numeric(forecast::forecast(Results, h = FCPeriods, xreg = XREGFC)$lower)[(FCPeriods+1):(2*FCPeriods)]]}, error = function(x) {
          FC_Data[, Low80 := as.numeric(forecast::forecast(Results, h = FCPeriods)$lower)[(FCPeriods+1):(2*FCPeriods)]]
        })
        tryCatch({FC_Data[, High80 := as.numeric(forecast::forecast(Results, h = FCPeriods, xreg = XREGFC)$upper)[1:FCPeriods]]}, error = function(x) {
          FC_Data[, High80 := as.numeric(forecast::forecast(Results, h = FCPeriods)$upper)[1:FCPeriods]]
        })
        tryCatch({FC_Data[, High95 := as.numeric(forecast::forecast(Results, h = FCPeriods, xreg = XREGFC)$upper)[(FCPeriods+1):(2*FCPeriods)]]}, error = function(x) {
          FC_Data[, High95 := as.numeric(forecast::forecast(Results, h = FCPeriods)$upper)[(FCPeriods+1):(2*FCPeriods)]]
        })
        
        # If model fails to rebuild----  
      } else {
        Train_Score[, Forecast := NA]
        FC_Data[, Forecast := NA]
        FC_Data[, Low80 := NA]
        FC_Data[, Low95 := NA]
        FC_Data[, High80 := NA]
        FC_Data[, High95 := NA]
      }
      
      # Rbind train and forecast data----
      FinalForecastData <- data.table::rbindlist(list(Train_Score,FC_Data), fill = TRUE)
      
      # Add Model Identifier Column----
      FinalForecastData[, ModelID := "Supercharged-SARIMA"][, ModelRank := FinalGrid[["ModelRank"]][[1L]]]
      
      # Rbind final forecast data sets----
      if(run == 1) {
        ReturnData <- FinalForecastData
      } else {
        ReturnData <- data.table::rbindlist(list(ReturnData, FinalForecastData))
      }
    }
    
    # Return forecast values for all models----
    return(ReturnData)
  }
}

#' OptimizeETS is a function that takes raw data and returns time series data
#' 
#' OptimizeETS is a function that takes raw data and returns the necessary time series data and objects for model building. It also fills any time gaps with zeros. Use this before you run any time series model functions.
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param Output This is passed through as output from TimeSeriesDataPrepare() and passed through ParallelArima()
#' @param MetricSelection Select from "MSE", "MAE", or "MAPE"
#' @param DataSetName This is the name of the data set passed through in parallel loop
#' @param train Training data returned from TimeSeriesDataPrepare()
#' @param test Test data returned from TimeSeriesDataPrepare()
#' @param FullData Full series data for scoring and ensemble
#' @param HoldOutPeriods Holdout periods returned from TimeSeriesDataPrepare() 
#' @param MinVal Minimum value of target variable returned from TimeSeriesDataPrepare()
#' @param TargetName Target variable name returned from TimeSeriesDataPrepare()
#' @param DateName Date variable name returned from TimeSeriesDataPrepare()
#' @param TrainValidateShare A two-element numeric vector. The first element is the weight applied to the training performance and the remainder is applied to the validation performance.
#' @param FinalGrid Grid for forecasting models
#' @return Time series data sets to pass onto auto modeling functions
#' @examples
#' Results <- OptimizeETS(
#'   Output,
#'   MetricSelection = "MAE",
#'   DataSetName = NULL,
#'   train = NULL,
#'   test = NULL,
#'   FullData = NULL,
#'   HoldOutPeriods = NULL,
#'   MinVal = NULL,
#'   TargetName = NULL,
#'   DateName = NULL,
#'   TrainValidateShare = NULL,
#'   FinalGrid = NULL)
#' @export
OptimizeETS <- function(Output,
                        MetricSelection = "MAE",
                        DataSetName = NULL,
                        train = NULL,
                        test = NULL,
                        FullData = NULL,
                        HoldOutPeriods = NULL,
                        MinVal = NULL,
                        TargetName = NULL,
                        DateName = NULL,
                        TrainValidateShare = NULL,
                        FinalGrid = NULL) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
  
  # Go to scoring model if FinalGrid is supplied----
  if(is.null(FinalGrid)) {
    
    # Generate Grid Objects----
    GridOutput <- GenerateParameterGrids(
      Model = "ets",
      test = test,
      MinVal = Output$MinVal, 
      DataSetName = DataSetName)
    Grid <- GridOutput[["Grid"]]
    GridList <- GridOutput[["GridList"]]
    ExperimentGrid <- GridOutput[["ExperimentGrid"]]
    ValidationData <- GridOutput[["ValidationData"]]
    
    # Build models----
    for (run in seq_len(Grid[,.N])) {
      
      # Update Grid Column Values----
      if(run == 1L) data.table::set(ExperimentGrid,  i = run, j = "GridName", value = "DefaultETS")
      
      # Define lambda----
      if(run != 1) {
        if(GridList[["Lambda"]][run] == "skip") {
          lambda <- NULL
        } else {
          lambda <- "auto"
        }        
      }
      
      # Build Models----
      if(run == 1L) {
        if(Output$UserSuppliedFrequency > 24) {
          modelparam <- "ZZN"
        } else {
          modelparam <- "ZZZ"
        }
        Results <- tryCatch({
          forecast::ets(
            y = train,
            lower = 0.0001, 
            upper = 0.9999, 
            model = modelparam,
            damped = GridList[["Damped"]][run],lambda = NULL)}, error = function(x) NULL)
        
      } else {
        Results <- tryCatch({
          forecast::ets(
            y = train,
            lower = 0.0001, 
            upper = 0.9999, 
            model = paste0(GridList[["ModelParam1"]][run], GridList[["ModelParam2"]][run], GridList[["ModelParam3"]][run]),
            damped = GridList[["Damped"]][run],lambda = NULL)}, error = function(x) NULL)
      }
      
      # Generate performance measures----
      ExperimentGrid <- Regular_Performance(
        Model = "ets",
        Results = Results, 
        GridList = GridList,
        TrainValidateShare = TrainValidateShare, 
        ExperimentGrid = ExperimentGrid,
        run = run,
        train = train,
        ValidationData = ValidationData,
        HoldOutPeriods = HoldOutPeriods)
    }
    
    # Return Experimental Grid----
    ResultsGrid <- ExperimentGrid[Blended_MAE != -10]
    
    # Return Table----
    return(ResultsGrid[, ModelRunNumber := seq_len(ResultsGrid[, .N])][order(Blended_MAE)][, ModelRankByDataType := seq_len(ResultsGrid[, .N])][order(ModelRunNumber)])
    
    # Forecast Code
  } else {
    
    # Remove Validation Metrics but Fill in the Train Metrics to Compare Against Initial Train----
    FinalGrid[, ':=' (Validate_MSE = NULL, Validate_MAE = NULL, Blended_MSE = NULL, Blended_MAE = NULL, Blended_MAPE = NULL)]
    
    # Create list to extract elements for modeling----
    GridList <- as.list(FinalGrid)
    
    # Train number of rows----
    TrainRows <- length(train)
    
    # Build models----
    for(run in seq_len(FinalGrid[, .N])) {
      
      # Define lambda----
      if(GridList$Lambda[run] == "AutoETS" | GridList$Lambda[run] == "skip") {
        lambda <- NULL
      } else {
        lambda <- "auto"
      }
      
      # Build Models----
      if(run == 1) {
        if(Output$UserSuppliedFrequency > 24) {
          modelparam <- "ZZN"
        } else {
          modelparam <- "ZZZ"
        }
        Results <- tryCatch({
          forecast::ets(
            y = train,
            lower = 0.0001, 
            upper = 0.9999, 
            model = modelparam,
            damped = GridList[["Damped"]][run],lambda = NULL)}, error = function(x) NULL)
        
      } else {
        Results <- tryCatch({
          forecast::ets(
            y = train,
            lower = 0.0001, 
            upper = 0.9999, 
            model = paste0(GridList[["ModelParam1"]][run], GridList[["ModelParam2"]][run], GridList[["ModelParam3"]][run]),
            damped = GridList[["Damped"]][run],lambda = NULL)}, error = function(x) NULL)
      }
      
      # Collect Forecast Inputs----
      FC_Data <- data.table::copy(Output$FC_Data)
      FC_Data[, Target := NA]
      FCPeriods <- Output$FCPeriods
      Train_Score <- data.table::copy(Output$FullData)
      Train_Score[, Target := as.numeric(Target)]
      
      # Generate Forecasts for Forecast Periods----
      if(!is.null(Results)) {
        
        # Score Training Data for Full Set of Predicted Values----
        Train_Score[, Forecast := as.numeric(Results$fitted)]
        
        # Forecast----
        FC_Data[, Forecast := as.numeric(forecast::forecast(Results, h = FCPeriods)$mean)]
        FC_Data[, Low95 := as.numeric(forecast::forecast(Results, h = FCPeriods)$lower)[1:FCPeriods]]
        FC_Data[, Low80 := as.numeric(forecast::forecast(Results, h = FCPeriods)$lower)[(FCPeriods+1):(2*FCPeriods)]]
        FC_Data[, High80 := as.numeric(forecast::forecast(Results, h = FCPeriods)$upper)[1:FCPeriods]]
        FC_Data[, High95 := as.numeric(forecast::forecast(Results, h = FCPeriods)$upper)[(FCPeriods+1):(2*FCPeriods)]]
        
        # If model fails to rebuild----  
      } else {
        Train_Score[, Forecast := NA]
        FC_Data[, Forecast := NA]
        FC_Data[, Low95 := NA]
        FC_Data[, Low80 := NA]
        FC_Data[, High80 := NA]
        FC_Data[, High95 := NA]
      }
      
      # Bind data----
      FinalForecastData <- data.table::rbindlist(list(Train_Score,FC_Data), fill = TRUE)
      
      # Add Identifier Column to Later data.table::dcast by----
      FinalForecastData[, ModelID := "ETS"][, ModelRank := FinalGrid[["ModelRank"]][[1]]]
      
      # Create Final Data----
      if(run == 1) {
        ReturnData <<- FinalForecastData
      } else {
        ReturnData <<- data.table::rbindlist(list(ReturnData, FinalForecastData))
      }
    }
    
    # Return forecast values for all models----
    return(ReturnData)
  }
}

#' OptimizeTBATS is a function that takes raw data and returns time series data
#' 
#' OptimizeTBATS is a function that takes raw data and returns the necessary time series data and objects for model building. It also fills any time gaps with zeros. Use this before you run any time series model functions.
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param Output This is passed through as output from TimeSeriesDataPrepare() and passed through ParallelArima()
#' @param MetricSelection Select from "MSE", "MAE", or "MAPE"
#' @param DataSetName This is the name of the data set passed through in parallel loop
#' @param train Training data returned from TimeSeriesDataPrepare()
#' @param test Test data returned from TimeSeriesDataPrepare()
#' @param Lags Max lags
#' @param MovingAverages Max moving averages
#' @param FullData Full series data for scoring and ensemble
#' @param HoldOutPeriods Holdout periods returned from TimeSeriesDataPrepare() 
#' @param MinVal Minimum value of target variable returned from TimeSeriesDataPrepare()
#' @param TargetName Target variable name returned from TimeSeriesDataPrepare()
#' @param DateName Date variable name returned from TimeSeriesDataPrepare()
#' @param TrainValidateShare A two-element numeric vector. The first element is the weight applied to the training performance and the remainder is applied to the validation performance.
#' @param FinalGrid Grid for forecasting models
#' @return Time series data sets to pass onto auto modeling functions
#' @examples
#' Results <- OptimizeTBATS(
#'   Output,
#'   MetricSelection = "MAE",
#'   DataSetName = NULL,
#'   train = NULL,
#'   test = NULL,
#'   Lags = NULL,
#'   MovingAverages = NULL,
#'   FullData = NULL,
#'   HoldOutPeriods = NULL,
#'   MinVal = NULL,
#'   TargetName = NULL,
#'   DateName = NULL,
#'   TrainValidateShare = NULL,
#'   FinalGrid = NULL)
#' @export
OptimizeTBATS <- function(Output,
                          MetricSelection = "MAE",
                          DataSetName = NULL,
                          train = NULL,
                          test = NULL,
                          Lags = NULL,
                          MovingAverages = NULL,
                          FullData = NULL,
                          HoldOutPeriods = NULL,
                          MinVal = NULL,
                          TargetName = NULL,
                          DateName = NULL,
                          TrainValidateShare = NULL,
                          FinalGrid = NULL) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Go to scoring model if FinalGrid is supplied----
  if(is.null(FinalGrid)) {
    
    # Generate Grid Objects----
    GridOutput <- GenerateParameterGrids(
      Model = "tbats",
      test = test,
      Lags = Lags,
      MovingAverages = MovingAverages,
      MinVal = Output$MinVal, 
      DataSetName = DataSetName)
    Grid <- GridOutput[["Grid"]]
    GridList <- GridOutput[["GridList"]]
    ExperimentGrid <- GridOutput[["ExperimentGrid"]]
    ValidationData <- GridOutput[["ValidationData"]]
    
    # Build models----
    for(run in seq_len(Grid[,.N])) {
      
      # Update Grid Column Values----
      if(run == 1L) data.table::set(ExperimentGrid, i = run, j = "GridName", value = "DefaultTBATS")
      
      # Define lambda----
      if(run != 1L) {
        if(GridList[["Lambda"]][run] == "skip") {
          lambda <- NULL
        } else {
          lambda <- "auto"
        }        
      }
      
      # Build Models----
      if(run == 1L) {
        Results <- tryCatch({forecast::tbats(y = train)}, error = function(x) NULL)
        
      } else {
        Results <- tryCatch({
          forecast::tbats(
            y = train,
            use.box.cox = GridList[["Lambda"]][run],
            use.trend = GridList[["Trend"]][run],
            use.damped.trend = GridList[["Damped"]][run],
            seasonal.periods = GridList[["SeasonalPeriods"]][run],
            use.arma.errors = GridList[["UseARMAErrors"]][run],
            use.parallel = TRUE,
            biasadj = GridList[["BiasAdj"]][run],
            max.p = GridList[["Lags"]][run],
            max.q = GridList[["MovingAverages"]][run])}, error = function(x) NULL)
      }
      
      # Generate performance measures----
      ExperimentGrid <- Regular_Performance(
        Model = "tbats",
        Results = Results, 
        GridList = GridList,
        TrainValidateShare = TrainValidateShare, 
        ExperimentGrid = ExperimentGrid,
        run = run,
        train = train,
        ValidationData = ValidationData,
        HoldOutPeriods = HoldOutPeriods)
    }
    
    # Return Experimental Grid----
    ResultsGrid <- ExperimentGrid[Blended_MAE != -10]
    
    # Return Table----
    return(ResultsGrid[, ModelRunNumber := seq_len(ResultsGrid[, .N])][order(Blended_MAE)][, ModelRankByDataType := seq_len(ResultsGrid[, .N])][order(ModelRunNumber)])
    
    # Forecast Code
  } else {
    
    # Remove Validation Metrics but Fill in the Train Metrics to Compare Against Initial Train----
    FinalGrid[, ':=' (Validate_MSE = NULL, Validate_MAE = NULL, Blended_MSE = NULL, Blended_MAE = NULL, Blended_MAPE = NULL)]
    
    # Create list to extract elements for modeling----
    GridList <- as.list(FinalGrid)
    
    # Train number of rows----
    TrainRows <- length(train)
    
    # Build models----
    for(run in seq_len(FinalGrid[, .N])) {
      
      # Define lambda----
      if(GridList$Lambda[run] == "AutoETS" | GridList$Lambda[run] == "skip") {
        lambda <- NULL
      } else {
        lambda <- "auto"
      }
      
      # Build Models----
      if(run == 1L) {
        Results <- tryCatch({
          forecast::tbats(
            y = Output$UserSuppliedData, 
            use.box.cox = TRUE, 
            use.trend = TRUE, 
            use.damped.trend = TRUE, 
            seasonal.periods = 1, 
            use.arma.errors = TRUE, 
            use.parallel = FALSE, 
            biasadj = TRUE, 
            max.p = Output$Lags, 
            max.q = Output$MovingAverages)}, error = function(x) NULL)
        
      } else {
        Results <- tryCatch({
          forecast::tbats(
            y = train,
            use.box.cox = GridList[["Lambda"]][run],
            use.trend = GridList[["Trend"]][run],
            use.damped.trend = GridList[["Damped"]][run],
            seasonal.periods = GridList[["SeasonalPeriods"]][run],
            use.arma.errors = GridList[["UseARMAErrors"]][run],
            use.parallel = TRUE,
            biasadj = GridList[["BiasAdj"]][run],
            max.p = GridList[["Lags"]][run],
            max.q = GridList[["MovingAverages"]][run])}, error = function(x) NULL)
      }
      
      # Collect Forecast Inputs----
      FC_Data <- data.table::copy(Output$FC_Data)
      FC_Data[, Target := NA]
      FCPeriods <- Output$FCPeriods
      Train_Score <- data.table::copy(Output$FullData)
      Train_Score[, Target := as.numeric(Target)]
      
      # Generate Forecasts for Forecast Periods----
      if(!is.null(Results)) {
        
        # Score Training Data for Full Set of Predicted Values----
        Train_Score[, Forecast := as.numeric(Results$fitted)]
        
        # Forecast----
        FC_Data[, Forecast := as.numeric(forecast::forecast(Results, h = FCPeriods)$mean)]
        FC_Data[, Low95 := as.numeric(forecast::forecast(Results, h = FCPeriods)$lower)[1L:FCPeriods]]
        FC_Data[, Low80 := as.numeric(forecast::forecast(Results, h = FCPeriods)$lower)[(FCPeriods + 1L):(2 * FCPeriods)]]
        FC_Data[, High80 := as.numeric(forecast::forecast(Results, h = FCPeriods)$upper)[1L:FCPeriods]]
        FC_Data[, High95 := as.numeric(forecast::forecast(Results, h = FCPeriods)$upper)[(FCPeriods + 1L):(2L * FCPeriods)]]
        
        # If model fails to rebuild----  
      } else {
        Train_Score[, Forecast := NA]
        FC_Data[, Forecast := NA]
        FC_Data[, Low95 := NA]
        FC_Data[, Low80 := NA]
        FC_Data[, High80 := NA]
        FC_Data[, High95 := NA]
      }
      
      # Bind data----
      FinalForecastData <- data.table::rbindlist(list(Train_Score,FC_Data), fill = TRUE)
      
      # Add Identifier Column to Later data.table::dcast by----
      FinalForecastData[, ModelID := "TBATS"][, ModelRank := FinalGrid[["ModelRank"]][[1L]]]
      
      # Create Final Data----
      if(run == 1L) {
        ReturnData <<- FinalForecastData
      } else {
        ReturnData <<- data.table::rbindlist(list(ReturnData, FinalForecastData))
      }
    }
    
    # Return forecast values for all models----
    return(ReturnData)
  }
}

#' OptimizeNNET is a function that takes raw data and returns time series data
#' 
#' OptimizeNNET is a function that takes raw data and returns the necessary time series data and objects for model building. It also fills any time gaps with zeros. Use this before you run any time series model functions.
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param Output This is passed through as output from TimeSeriesDataPrepare() and passed through ParallelArima()
#' @param MetricSelection Select from "MSE", "MAE", or "MAPE"
#' @param DataSetName This is the name of the data set passed through in parallel loop
#' @param train Training data returned from TimeSeriesDataPrepare()
#' @param test Test data returned from TimeSeriesDataPrepare()
#' @param FullData Full series data for scoring and ensemble
#' @param HoldOutPeriods Holdout periods returned from TimeSeriesDataPrepare() 
#' @param MinVal Minimum value of target variable returned from TimeSeriesDataPrepare()
#' @param TargetName Target variable name returned from TimeSeriesDataPrepare()
#' @param DateName Date variable name returned from TimeSeriesDataPrepare()
#' @param Lags Max value of lag returned from TimeSeriesDataPrepare()
#' @param SeasonalLags Max value of seasonal lags returned from TimeSeriesDataPrepare()
#' @param MaxFourierTerms Max value of fourier pairs
#' @param TrainValidateShare A two-element numeric vector. The first element is the weight applied to the training performance and the remainder is applied to the validation performance.
#' @param MaxRunsWithoutNewWinner The number of runs without a new winner which if passed tells the function to stop
#' @param MaxNumberModels The number of models you want to test.
#' @param FinalGrid If NULL, regular train optimization occurs. If the grid is supplied, final builds are conducted.
#' @return Time series data sets to pass onto auto modeling functions
#' @examples
#' Results <- OptimizeNNET(
#'   Output,
#'   MetricSelection = "MAE",
#'   DataSetName = NULL,
#'   train = NULL,
#'   test = NULL,
#'   FullData = NULL,
#'   HoldOutPeriods = NULL,
#'   MinVal = NULL,
#'   TargetName = NULL,
#'   DateName = NULL,
#'   Lags = NULL,
#'   SeasonalLags = NULL,
#'   MaxFourierTerms = NULL,
#'   TrainValidateShare = NULL,
#'   MaxRunsWithoutNewWinner = 20,
#'   MaxNumberModels = 5,
#'   MaxRunMinutes = NULL,
#'   FinalGrid = NULL)
#' @export
OptimizeNNET <- function(Output,
                         MetricSelection = "MAE",
                         DataSetName = NULL,
                         train = NULL,
                         test = NULL,
                         FullData = NULL,
                         HoldOutPeriods = NULL,
                         MinVal = NULL,
                         TargetName = NULL,
                         DateName = NULL,
                         Lags = NULL, 
                         SeasonalLags = NULL,
                         MaxFourierTerms = NULL,
                         TrainValidateShare = NULL,
                         MaxRunsWithoutNewWinner = 20,
                         MaxNumberModels = NULL,
                         MaxRunMinutes = NULL,
                         FinalGrid = NULL) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
  
  # Go to scoring model if FinalGrid is supplied----
  if(is.null(FinalGrid)) {
    
    # Get grid objects----
    GridObjects <- GenerateParameterGrids(
      Model = "nnet", 
      MinVal = Output$MinVal, 
      DataSetName = DataSetName,
      SeasonalDifferences = SeasonalDifferences,
      SeasonalMovingAverages = SeasonalMovingAverages,
      SeasonalLags = SeasonalLags,
      MaxFourierTerms = MaxFourierTerms,
      Differences = Differences,
      MovingAverages = MovingAverages,
      Lags = Lags)
    Grid <- GridObjects[["Grid"]]
    GridClusters <- GridObjects[["GridClusters"]]
    ExperimentGrid <- GridObjects[["ExperimentGrid"]]
    rm(GridObjects)
    
    # Initialize RL----
    RL_Start <- RL_Initialize(
      ParameterGridSet = GridClusters, 
      Alpha = 1, 
      Beta = 1, 
      SubDivisions = 1000L)
    BanditArmsN <- RL_Start[["BanditArmsN"]]
    Successes <- RL_Start[["Successes"]]
    Trials <- RL_Start[["Trials"]]
    GridIDs <- RL_Start[["GridIDs"]]
    BanditProbs <- RL_Start[["BanditProbs"]]
    rm(RL_Start)
    
    # Add bandit probs columns to ExperimentGrid----
    data.table::set(ExperimentGrid, j = paste0("BanditProbs_",names(GridClusters)), value = -10)
    
    # HoldOutData----
    ValidationData <- data.table::copy(test)
    
    # Intitalize Counter----
    run <- 0L
    
    # Initialize TotalRunTime----
    TotalRunTime <- 0
    
    # Sample from bandit to select next grid row----
    NewGrid <- 1L
    RunsWithoutNewWinner <- 0L
    
    # Build models----
    repeat {
      
      # Increment Counter----
      run <- run + 1L
      
      # Select new grid----
      if(run <= BanditArmsN + 1) {
        if(run != 1)
          NextGrid <- GridClusters[[names(GridClusters)[run-1]]][1]
      } else {
        NextGrid <- as.list(GridClusters[[names(GridClusters)[NewGrid]]][Trials[NewGrid]+1])
      }
      
      # Update Grid Column Values----
      if(run == 1) {
        data.table::set(
          ExperimentGrid, 
          i = run,
          j = "GridName",
          value = "DefaultAutoNNet")
      } else {
        for(cols in as.integer(1:10)) {
          if(cols == 1L) {
            data.table::set(
              ExperimentGrid, 
              i = run,
              j = cols,
              value = GridClusters[[names(GridClusters)[NewGrid]]][["DataSetName"]][Trials[NewGrid]+1])
          } else if(cols == 12) {
            data.table::set(
              ExperimentGrid, 
              i = run,
              j = cols,
              value = names(GridClusters)[NewGrid])
          } else {
            # Grab correct cluster group, cluster column, and cluster row
            data.table::set(
              ExperimentGrid, 
              i = run,
              j = cols,
              value = GridClusters[[names(GridClusters)[NewGrid]]][[cols]][Trials[NewGrid]+1])
          }
        }
        
        # Fill bandit probabilities
        banditindex <- 0L
        for(BanditCols in (ncol(ExperimentGrid)-BanditArmsN):(ncol(ExperimentGrid) - 1L)) {
          banditindex <- banditindex + 1L 
          data.table::set(
            ExperimentGrid, 
            i = run, 
            j = BanditCols, 
            value = round(BanditProbs[banditindex], 2L))          
        }
      }
      
      # Define lambda----
      if(run != 1L) {
        if(NextGrid$Lambda[1L] == "skip") {
          lambda <- NULL
        } else {
          lambda <- "auto"
        }        
      }
      
      # Define Fourier Terms----
      if(run != 1L) {
        if(NextGrid[["MaxFourierTerms"]][1L] == 0L) {
          XREG <- FALSE
          XREGFC <- FALSE
        } else {
          XREG <- tryCatch({forecast::fourier(train, K = NextGrid[["MaxFourierTerms"]][1L])}, error = function(x) FALSE)
          XREGFC <- tryCatch({forecast::fourier(train, K = NextGrid[["MaxFourierTerms"]][1L], h = HoldOutPeriods)}, error = function(x) FALSE)
        }
      }
      
      # Start time----
      Start <- Sys.time()
      
      # Build Models----
      if(run == 1L) {
        if (MinVal > 0L) {
          Results <- tryCatch({forecast::nnetar(y = train, p = Lags, P = SeasonalLags, lambda = "auto")}, error = function(x) NULL)
        } else {
          Results <- tryCatch({forecast::nnetar(y = train, p = Lags, P = SeasonalLags)}, error = function(x) NULL)
        }
      } else {
        if(is.numeric(XREG) & is.numeric(XREGFC)) {
          Results <- tryCatch({forecast::nnetar(
            y = train, p = NextGrid[["Lags"]][1], P = NextGrid[["SeasonalLags"]][1], size = NextGrid[["LayerNodes"]][1],
            repeats = NextGrid[["Repeats"]][1], xreg = XREG, scale.inputs = NextGrid[["Scale"]][1])}, error = function(x) NULL)
        } else {
          Results <- tryCatch({forecast::nnetar(
            y = train, p = NextGrid[["Lags"]][1], P = NextGrid[["SeasonalLags"]][1], size = NextGrid[["LayerNodes"]][1],
            repeats = NextGrid[["Repeats"]][1], scale.inputs = NextGrid[["Scale"]][1])}, error = function(x) NULL)
        }
      }
      
      # End time---
      End <- Sys.time()
      
      # Performance Metrics----
      tryCatch({ExperimentGrid <- RL_Performance(
        Results = Results, 
        NextGrid = NextGrid,
        TrainValidateShare = TrainValidateShare, 
        MaxFourierTerms = NextGrid[["MaxFourierTerms"]][1L],
        XREGFC = XREGFC,
        ExperimentGrid = ExperimentGrid,
        run = run, 
        train = train,
        ValidationData = ValidationData,
        HoldOutPeriods = HoldOutPeriods)}, error = function(x) NULL)
      
      # Add run time to ExperimentGrid----
      data.table::set(ExperimentGrid, i = run, j = "RunTime", value = End - Start)
      TotalRunTime <- TotalRunTime + as.numeric((End - Start))
      
      # RL Update----
      tryCatch({RL_Update_Output <- RL_Update(
        ExperimentGrid = ExperimentGrid,
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
        BanditProbabilities = BanditProbs)
      BanditProbs <- RL_Update_Output[["BanditProbs"]]
      Trials <- RL_Update_Output[["Trials"]]
      Successes <- RL_Update_Output[["Successes"]]
      NewGrid <- RL_Update_Output[["NewGrid"]]
      Break <- RL_Update_Output[["BreakLoop"]]}, error = function(x) NULL)
      
      # Exit repeat loop upon conditions----
      if(Break == "exit") break
    }
    
    # Remove Invalid Columns----
    ResultsGrid <- ExperimentGrid[!is.na(Blended_MAE) & SeasonalLags != -10]
    
    # Add Rank Values----
    ResultsGrid <- ResultsGrid[, ModelRunNumber := seq_len(ResultsGrid[, .N])][order(Blended_MAE)][, ModelRankByDataType := seq_len(ResultsGrid[, .N])][order(ModelRunNumber)]
    
    # Return Results----
    return(ResultsGrid)
    
    # Forecast Code
  } else {
    
    # Remove Validation Metrics but Fill in the Train Metrics to Compare Against Initial Train----
    FinalGrid[, ':=' (Validate_MSE = NULL, Validate_MAE = NULL, Blended_MSE = NULL, Blended_MAE = NULL, Blended_MAPE = NULL)]
    
    # Create list to extract elements for modeling----
    TSGridList <- as.list(FinalGrid)
    
    # Train number of rows----
    TrainRows <- length(train)
    
    # Build models----
    for(run in seq_len(FinalGrid[, .N])) {
      
      if(run != 1L) {
        if(TSGridList$Lambda[1L] == "skip") {
          lambda <- NULL
        } else {
          lambda <- "auto"
        }
      }
      
      # Build Models----
      if(FinalGrid[1L, GridName] == "DefaultAutoArima") {
        if (MinVal > 0L) {
          Results <- tryCatch({forecast::nnetar(y = train, p = Lags, P = SeasonalLags, lambda = "auto")}, error = function(x) NULL)
        } else {
          Results <- tryCatch({forecast::nnetar(y = train, p = Lags, P = SeasonalLags)}, error = function(x) NULL)
        }
      } else {
        if(TSGridList[["MaxFourierTerms"]][1L] != 0L) {
          XREG <- tryCatch({forecast::fourier(train, K = TSGridList[["MaxFourierTerms"]][1])}, error = function(x) FALSE)
          XREGFC <- tryCatch({forecast::fourier(train, K = TSGridList[["MaxFourierTerms"]][1], h = FCPeriods)}, error = function(x) FALSE)
          if(is.numeric(XREG) & is.numeric(XREGFC)) {
            Results <- tryCatch({forecast::nnetar(
              y = train, p = TSGridList[["Lags"]][1], P = TSGridList[["SeasonalLags"]][1], size = TSGridList[["LayerNodes"]][1],
              repeats = TSGridList[["Repeats"]][1], xreg = XREG, scale.inputs = TSGridList[["Scale"]][1])}, error = function(x) NULL)
          } else {
            Results <- NULL
          }
        } else {
          Results <- tryCatch({forecast::nnetar(
            y = train, p = TSGridList[["Lags"]][1], P = TSGridList[["SeasonalLags"]][1], size = TSGridList[["LayerNodes"]][1],
            repeats = TSGridList[["Repeats"]][1], scale.inputs = TSGridList[["Scale"]][1])}, error = function(x) NULL)
        }
      }
      
      # Collect Forecast Inputs----
      FC_Data <- data.table::copy(Output$FC_Data)
      FC_Data[, Target := NA]
      FCPeriods <- Output$FCPeriods
      Train_Score <- data.table::copy(Output$FullData)
      Train_Score[, Target := as.numeric(Target)]
      
      # Generate Forecasts for Forecast Periods----
      if(!is.null(Results)) {
        
        # Score Training Data for Full Set of Predicted Values----
        Train_Score[, Forecast := as.numeric(Results$fitted)]
        
        # Forecast----
        if(TSGridList[["MaxFourierTerms"]][run] != 0) {
          xx <- forecast::forecast(Results, PI=TRUE, xreg = XREGFC, h = FCPeriods)
          FC_Data[, Forecast := as.numeric(xx$mean)]
          FC_Data[, Low95 := as.numeric(xx$lower)[1L:FCPeriods]]
          FC_Data[, Low80 := as.numeric(xx$lower)[(FCPeriods + 1L):(2L * FCPeriods)]]
          FC_Data[, High80 := as.numeric(xx$upper)[1L:FCPeriods]]
          FC_Data[, High95 := as.numeric(xx$upper)[(FCPeriods + 1L):(2L * FCPeriods)]]
        } else {
          xx <- tryCatch({forecast::forecast(Results, PI=TRUE, h = FCPeriods)}, error = function(x) {
            xx <- forecast::forecast(Results, h = FCPeriods)
            FC_Data[, Forecast := as.numeric(xx$mean)] 
            FC_Data[, Low80 := NA]
            FC_Data[, Low95 := NA]
            FC_Data[, High80 := NA]
            FC_Data[, High95 := NA]
          })
          if(ncol(FC_Data) != 3) {
            xx <- forecast::forecast(Results, h = FCPeriods)
            FC_Data[, Forecast := as.numeric(xx$mean)]
            FC_Data[, Low95 := as.numeric(xx$lower)[1L:FCPeriods]]
            FC_Data[, Low80 := as.numeric(xx$lower)[(FCPeriods + 1L):(2L * FCPeriods)]]
            FC_Data[, High80 := as.numeric(xx$upper)[1L:FCPeriods]]
            FC_Data[, High95 := as.numeric(xx$upper)[(FCPeriods + 1L):(2L * FCPeriods)]]                  
          }
        }
        
        # If model fails to rebuild----  
      } else {
        Train_Score[, Forecast := NA]
        FC_Data[, Forecast := NA]
        FC_Data[, Low80 := NA]
        FC_Data[, Low95 := NA]
        FC_Data[, High80 := NA]
        FC_Data[, High95 := NA]
      }
      
      # Rbind train and forecast data----
      FinalForecastData <- data.table::rbindlist(list(Train_Score,FC_Data), fill = TRUE)
      
      # Add Model Identifier Column----
      FinalForecastData[, ModelID := "Supercharged-NNET"][, ModelRank := FinalGrid[["ModelRank"]][[1]]]
      
      # Rbind final forecast data sets----
      if(run == 1L) {
        ReturnData <- FinalForecastData
      } else {
        ReturnData <- data.table::rbindlist(list(ReturnData, FinalForecastData))
      }
    }
    
    # Return forecast values for all models----
    return(ReturnData)
  }
}

#' OptimizeArfima is a function that takes raw data and returns time series data
#' 
#' OptimizeArfima is a function that takes raw data and returns the necessary time series data and objects for model building. It also fills any time gaps with zeros. Use this before you run any time series model functions.
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param Output This is passed through as output from TimeSeriesDataPrepare() and passed through ParallelArima()
#' @param MetricSelection Select from "MSE", "MAE", or "MAPE"
#' @param DataSetName This is the name of the data set passed through in parallel loop
#' @param train Training data returned from TimeSeriesDataPrepare()
#' @param test Test data returned from TimeSeriesDataPrepare()
#' @param Lags Max lags
#' @param MovingAverages Max moving averages
#' @param FullData Full series data for scoring and ensemble
#' @param HoldOutPeriods Holdout periods returned from TimeSeriesDataPrepare() 
#' @param MinVal Minimum value of target variable returned from TimeSeriesDataPrepare()
#' @param TargetName Target variable name returned from TimeSeriesDataPrepare()
#' @param DateName Date variable name returned from TimeSeriesDataPrepare()
#' @param TrainValidateShare A two-element numeric vector. The first element is the weight applied to the training performance and the remainder is applied to the validation performance.
#' @param FinalGrid Grid for forecasting models
#' @return Time series data sets to pass onto auto modeling functions
#' @examples
#' Results <- OptimizeArfima(
#'   Output,
#'   MetricSelection = "MAE",
#'   DataSetName = NULL,
#'   train = NULL,
#'   test = NULL,
#'   Lags = NULL,
#'   MovingAverages = NULL,
#'   FullData = NULL,
#'   HoldOutPeriods = NULL,
#'   MinVal = NULL,
#'   TargetName = NULL,
#'   DateName = NULL,
#'   TrainValidateShare = NULL,
#'   FinalGrid = NULL)
#' @export
OptimizeArfima <- function(Output,
                           MetricSelection = "MAE",
                           DataSetName = NULL,
                           train = NULL,
                           test = NULL,
                           Lags = NULL,
                           MovingAverages = NULL,
                           FullData = NULL,
                           HoldOutPeriods = NULL,
                           MinVal = NULL,
                           TargetName = NULL,
                           DateName = NULL,
                           TrainValidateShare = NULL,
                           FinalGrid = NULL) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
  
  # Go to scoring model if FinalGrid is supplied----
  if(is.null(FinalGrid)) {
    
    # Generate Grid Objects----
    GridOutput <- GenerateParameterGrids(
      Model = "arfima",
      test = test,
      Lags = Lags,
      MovingAverages = MovingAverages,
      MinVal = Output$MinVal, 
      DataSetName = DataSetName)
    Grid <- GridOutput[["Grid"]]
    GridList <- GridOutput[["GridList"]]
    ExperimentGrid <- GridOutput[["ExperimentGrid"]]
    ValidationData <- GridOutput[["ValidationData"]]
    
    # Build models----
    for (run in seq_len(Grid[,.N])) {
      
      # Update Grid Column Values----
      if(run == 1L) {
        data.table::set(
          ExperimentGrid, 
          i = run,
          j = "GridName",
          value = "DefaultArfima")
      }
      
      # Define lambda----
      if(run != 1L) {
        if(GridList[["Lambda"]][run] == "skip") {
          lambda <- NULL
        } else {
          lambda <- "auto"
        }        
      }
      
      # Build Models----
      if(run == 1) {
        Results <- tryCatch({
          forecast::arfima(y = train)}, error = function(x) NULL)
        
      } else {
        Results <- tryCatch({
          forecast::arfima(
            y = train, 
            drange = c(GridList[["Drange"]][run]-0.10,GridList[["Drange"]][run]),
            lambda = lambda,
            max.p = GridList[["Lags"]][run],
            max.q = GridList[["MovingAverages"]][run])}, error = function(x) NULL)
      }
      
      # Generate performance measures----
      ExperimentGrid <- Regular_Performance(
        Model = "arfima",
        Results = Results, 
        GridList = GridList,
        TrainValidateShare = TrainValidateShare, 
        ExperimentGrid = ExperimentGrid,
        run = run,
        train = train,
        ValidationData = ValidationData,
        HoldOutPeriods = HoldOutPeriods)
    }
    
    # Return Experimental Grid----
    ResultsGrid <- ExperimentGrid[Blended_MAE != -10]
    
    # Return Table----
    return(ResultsGrid[, ModelRunNumber := seq_len(ResultsGrid[, .N])][order(Blended_MAE)][, ModelRankByDataType := seq_len(ResultsGrid[, .N])][order(ModelRunNumber)])
    
    # Forecast Code
  } else {
    
    # Remove Validation Metrics but Fill in the Train Metrics to Compare Against Initial Train----
    FinalGrid[, ':=' (Validate_MSE = NULL, Validate_MAE = NULL, Blended_MSE = NULL, Blended_MAE = NULL, Blended_MAPE = NULL)]
    
    # Create list to extract elements for modeling----
    GridList <- as.list(FinalGrid)
    
    # Train number of rows----
    TrainRows <- length(train)
    
    # Build models----
    for(run in seq_len(FinalGrid[, .N])) {
      
      # Define lambda----
      if(GridList$Lambda[run] == "AutoETS" | GridList$Lambda[run] == "skip") {
        lambda <- NULL
      } else {
        lambda <- "auto"
      }
      
      # Build Models----
      if(run == 1L) {
        Results <- tryCatch({
          forecast::arfima(y = train)}, error = function(x) NULL)
        
      } else {
        Results <- tryCatch({
          forecast::arfima(
            y = train, 
            drange = c(GridList[["Drange"]][run]-0.10,GridList[["Drange"]][run]),
            lambda = lambda,
            max.p = GridList[["Lags"]][run],
            max.q = GridList[["MovingAverages"]][run])}, error = function(x) NULL)
      }
      
      # Collect Forecast Inputs----
      FC_Data <- data.table::copy(Output$FC_Data)
      FC_Data[, Target := NA]
      FCPeriods <- Output$FCPeriods
      Train_Score <- data.table::copy(Output$FullData)
      Train_Score[, Target := as.numeric(Target)]
      
      # Generate Forecasts for Forecast Periods----
      if(!is.null(Results)) {
        
        # Score Training Data for Full Set of Predicted Values----
        Train_Score[, Forecast := as.numeric(Results$fitted)]
        
        # Forecast----
        FC_Data[, Forecast := as.numeric(forecast::forecast(Results, h = FCPeriods)$mean)]
        FC_Data[, Low95 := as.numeric(forecast::forecast(Results, h = FCPeriods)$lower)[1L:FCPeriods]]
        FC_Data[, Low80 := as.numeric(forecast::forecast(Results, h = FCPeriods)$lower)[(FCPeriods + 1L):(2L * FCPeriods)]]
        FC_Data[, High80 := as.numeric(forecast::forecast(Results, h = FCPeriods)$upper)[1L:FCPeriods]]
        FC_Data[, High95 := as.numeric(forecast::forecast(Results, h = FCPeriods)$upper)[(FCPeriods + 1L):(2L * FCPeriods)]]
        
        # If model fails to rebuild----  
      } else {
        Train_Score[, Forecast := NA]
        FC_Data[, Forecast := NA]
        FC_Data[, Low95 := NA]
        FC_Data[, Low80 := NA]
        FC_Data[, High80 := NA]
        FC_Data[, High95 := NA]
      }
      
      # Bind data----
      FinalForecastData <- data.table::rbindlist(list(Train_Score,FC_Data), fill = TRUE)
      
      # Add Identifier Column to Later data.table::dcast by----
      FinalForecastData[, ModelID := "ARFIMA"][, ModelRank := FinalGrid[["ModelRank"]][[1]]]
      
      # Create Final Data----
      if(run == 1L) {
        ReturnData <<- FinalForecastData
      } else {
        ReturnData <<- data.table::rbindlist(list(ReturnData, FinalForecastData))
      }
    }
    
    # Return forecast values for all models----
    return(ReturnData)
  }
}

#' OptimizeTSLM is a function that takes raw data and returns time series data
#' 
#' OptimizeTSLM is a function that takes raw data and returns the necessary time series data and objects for model building. It also fills any time gaps with zeros. Use this before you run any time series model functions.
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param Output This is passed through as output from TimeSeriesDataPrepare() and passed through ParallelArima()
#' @param MetricSelection Select from "MSE", "MAE", or "MAPE"
#' @param DataSetName This is the name of the data set passed through in parallel loop
#' @param train Training data returned from TimeSeriesDataPrepare()
#' @param test Test data returned from TimeSeriesDataPrepare()
#' @param FullData Full series data for scoring and ensemble
#' @param HoldOutPeriods Holdout periods returned from TimeSeriesDataPrepare() 
#' @param MinVal Minimum value of target variable returned from TimeSeriesDataPrepare()
#' @param TargetName Target variable name returned from TimeSeriesDataPrepare()
#' @param DateName Date variable name returned from TimeSeriesDataPrepare()
#' @param TrainValidateShare A two-element numeric vector. The first element is the weight applied to the training performance and the remainder is applied to the validation performance.
#' @param FinalGrid Grid for forecasting models
#' @return Time series data sets to pass onto auto modeling functions
#' @examples
#' Results <- OptimizeTSLM(
#'   Output,
#'   MetricSelection = "MAE",
#'   DataSetName = NULL,
#'   train = NULL,
#'   test = NULL,
#'   FullData = NULL,
#'   HoldOutPeriods = NULL,
#'   MinVal = NULL,
#'   TargetName = NULL,
#'   DateName = NULL,
#'   TrainValidateShare = NULL,
#'   FinalGrid = NULL)
#' @export
OptimizeTSLM <- function(Output,
                         MetricSelection = "MAE",
                         DataSetName = NULL,
                         train = NULL,
                         test = NULL,
                         FullData = NULL,
                         HoldOutPeriods = NULL,
                         MinVal = NULL,
                         TargetName = NULL,
                         DateName = NULL,
                         TrainValidateShare = NULL,
                         FinalGrid = NULL) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
  
  # Go to scoring model if FinalGrid is supplied----
  if(is.null(FinalGrid)) {
    
    # Generate Grid Objects----
    GridOutput <- GenerateParameterGrids(
      Model = "tslm",
      test = test,
      MinVal = Output$MinVal, 
      DataSetName = DataSetName)
    Grid <- GridOutput[["Grid"]]
    GridList <- GridOutput[["GridList"]]
    ExperimentGrid <- GridOutput[["ExperimentGrid"]]
    ValidationData <- GridOutput[["ValidationData"]]
    
    # Build models----
    for (run in seq_len(Grid[,.N])) {
      
      # Define lambda----
      if(GridList[["Lambda"]][run] == "skip") {
        lambda <- NULL
      } else {
        lambda <- "auto"
      }        
      
      # Build Models----
      Results <- tryCatch({forecast::tslm(train ~ trend+season,data=train,lambda=lambda,biasadj=GridList[["BiasAdj"]][run])},error=function(x) NULL)
      
      # Generate performance measures----
      ExperimentGrid <- Regular_Performance(
        Model = "tslm",
        Results = Results, 
        GridList = GridList,
        TrainValidateShare = TrainValidateShare, 
        ExperimentGrid = ExperimentGrid,
        run = run,
        train = train,
        ValidationData = ValidationData,
        HoldOutPeriods = HoldOutPeriods)
    }
    
    # Return Experimental Grid----
    ResultsGrid <- ExperimentGrid[Blended_MAE != -10]
    
    # Return Table----
    return(ResultsGrid[, ModelRunNumber := seq_len(ResultsGrid[, .N])][order(Blended_MAE)][, ModelRankByDataType := seq_len(ResultsGrid[, .N])][order(ModelRunNumber)])
    
    # Forecast Code
  } else {
    
    # Remove Validation Metrics but Fill in the Train Metrics to Compare Against Initial Train----
    FinalGrid[, ':=' (Validate_MSE = NULL, Validate_MAE = NULL, Blended_MSE = NULL, Blended_MAE = NULL, Blended_MAPE = NULL)]
    
    # Create list to extract elements for modeling----
    GridList <- as.list(FinalGrid)
    
    # Train number of rows----
    TrainRows <- length(train)
    
    # Build models----
    for (run in seq_len(FinalGrid[,.N])) {
      
      # Define lambda----
      if(GridList$Lambda[run] == "skip") {
        lambda <- NULL
      } else {
        lambda <- "auto"
      }
      
      # Build Models----
      Results <- tryCatch({forecast::tslm(train ~ trend+season,data=train,lambda=lambda,biasadj=GridList[["BiasAdj"]][run])},error=function(x) NULL)
      
      # Collect Forecast Inputs----
      FC_Data <- data.table::copy(Output$FC_Data)
      FC_Data[, Target := NA]
      FCPeriods <- Output$FCPeriods
      Train_Score <- data.table::copy(Output$FullData)
      Train_Score[, Target := as.numeric(Target)]
      
      # Generate Forecasts for Forecast Periods----
      if(!is.null(Results)) {
        
        # Score Training Data for Full Set of Predicted Values----
        Train_Score[, Forecast := as.numeric(Results$fitted)]
        
        # Forecast----
        FC_Data[, Forecast := as.numeric(forecast::forecast(Results, h = FCPeriods)$mean)]
        FC_Data[, Low95 := as.numeric(forecast::forecast(Results, h = FCPeriods)$lower)[1L:FCPeriods]]
        FC_Data[, Low80 := as.numeric(forecast::forecast(Results, h = FCPeriods)$lower)[(FCPeriods + 1L):(2L * FCPeriods)]]
        FC_Data[, High80 := as.numeric(forecast::forecast(Results, h = FCPeriods)$upper)[1L:FCPeriods]]
        FC_Data[, High95 := as.numeric(forecast::forecast(Results, h = FCPeriods)$upper)[(FCPeriods + 1L):(2L * FCPeriods)]]
        
        # If model fails to rebuild----  
      } else {
        Train_Score[, Forecast := NA]
        FC_Data[, Forecast := NA]
        FC_Data[, Low95 := NA]
        FC_Data[, Low80 := NA]
        FC_Data[, High80 := NA]
        FC_Data[, High95 := NA]
      }
      
      # Bind data----
      FinalForecastData <- data.table::rbindlist(list(Train_Score,FC_Data), fill = TRUE)
      
      # Add Identifier Column to Later data.table::dcast by----
      FinalForecastData[, ModelID := "TSLM"][, ModelRank := FinalGrid[["ModelRank"]][[1]]]
      
      # Create Final Data----
      if(run == 1) {
        ReturnData <<- FinalForecastData
      } else {
        ReturnData <<- data.table::rbindlist(list(ReturnData, FinalForecastData))
      }
    }
    
    # Return forecast values for all models----
    return(ReturnData)
  }
}

#' ParallelAutoARIMA to run the 4 data sets at once
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param MetricSelection Choose from MAE, MSE, and MAPE
#' @param Output The output returned from TimeSeriesDataPrepare()
#' @param TrainValidateShare The value returned from TimeSeriesPrepare()
#' @param MaxFourierTerms Fourier pairs
#' @param TrainValidateShare c(0.50,0.50)
#' @param MaxNumberModels 20
#' @param MaxRunMinutes 5
#' @param MaxRunsWithoutNewWinner 12
#' @return Time series data sets to pass onto auto modeling functions
#' @examples 
#' ParallelAutoARIMA(
#'   MetricSelection = "MAE",
#'   Output = NULL, 
#'   MaxRunsWithoutNewWinner = 20,
#'   TrainValidateShare = c(0.50,0.50),
#'   MaxNumberModels = 5,
#'   MaxRunMinutes = 5)
#' @export
ParallelAutoARIMA <- function(
  Output,
  MetricSelection = "MAE",
  MaxFourierTerms = 1L,
  TrainValidateShare = c(0.50,0.50),
  MaxNumberModels = 20,
  MaxRunMinutes = 5L,
  MaxRunsWithoutNewWinner = 12) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Define Modeling Artifacts----
  TrainArtifacts = list(
    UserSupplied = list(
      Data = Output$UserSuppliedData,
      Diff = if(is.null(Output$UserSuppliedDiff)) 0 else Output$UserSuppliedDiff,
      SDiff = if(is.null(Output$UserSuppliedSeasonalDiff)) 0 else Output$UserSuppliedSeasonalDiff,
      Name = "UserSupplied"),
    ModelFrequency = list(
      Data = Output$ModelFreqData,
      Diff = if(is.null(Output$ModelFreqDiff)) 0 else Output$ModelFreqDiff,
      SDiff = if(is.null(Output$ModelFreqSeasonalDiff)) 0 else Output$ModelFreqSeasonalDiff,
      Name = "ModelFrequency"),
    TSClean = list(
      Data = Output$TSCleanData,
      Diff = if(is.null(Output$TSCleanDiff)) 0 else Output$TSCleanDiff,
      SDiff = if(is.null(Output$TSCleanSeasonalDiff)) 0 else Output$TSCleanSeasonalDiff,
      Name = "TSClean"),
    TSCleanModelFreq = list(
      Data = Output$TSCleanModelFreqData,
      Diff = if(is.null(Output$TSCleanModelFreqDiff)) 0 else Output$TSCleanModelFreqDiff,
      SDiff = if(is.null(Output$TSCleanModelFreqSeasonalDiff)) 0 else Output$TSCleanModelFreqSeasonalDiff,
      Name = "TSCleanModelFrequency"))
  
  # Idenity the number of non-null data sets to run through OptimizeArima----
  Counter <- 0L
  for(i in seq_len(length(TrainArtifacts))) if(!is.null(TrainArtifacts[[i]][["Data"]])) Counter <- Counter + 1L    
  
  # Setup the parallel environment----
  packages <- c("RemixAutoML","data.table","forecast")
  cores    <- parallel::detectCores()
  cl       <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  library(doParallel)
  Results <- foreach::foreach(
    i = seq_len(Counter),
    .combine = function(...) data.table::rbindlist(list(...), fill = TRUE),
    .multicombine = TRUE,
    .packages = packages) %dopar% {
      
      OptimizeArima(
        Output = Output,
        MetricSelection = MetricSelection,
        DataSetName = TrainArtifacts[[i]][["Name"]],
        train = TrainArtifacts[[i]][["Data"]],
        test = Output$TestData,
        Lags = Output$Lags,
        SeasonalLags = Output$SeasonalLags,
        MovingAverages = Output$MovingAverages,
        SeasonalMovingAverages = Output$SeasonalMovingAverages,
        Differences = TrainArtifacts[[i]][["Diff"]],
        SeasonalDifferences = TrainArtifacts[[i]][["SDiff"]],
        FullData = Output$FullData,
        HoldOutPeriods = Output$HoldOutPeriods,
        MinVal = Output$MinVal,
        TargetName = Output$TargetName,
        DateName = Output$DateName,
        MaxFourierTerms = MaxFourierTerms,
        TrainValidateShare = c(TrainValidateShare),
        MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
        MaxNumberModels = MaxNumberModels,
        MaxRunMinutes = MaxRunMinutes)
      
      #
      # Output = Output
      # MetricSelection = MetricSelection
      # DataSetName = TrainArtifacts[[i]][["Name"]]
      # train = TrainArtifacts[[i]][["Data"]]
      # test = Output$TestData
      # Lags = Output$Lags
      # SeasonalLags = Output$SeasonalLags
      # MovingAverages = Output$MovingAverages
      # SeasonalMovingAverages = Output$SeasonalMovingAverages
      # Differences = TrainArtifacts[[i]][["Diff"]]
      # SeasonalDifferences = TrainArtifacts[[i]][["SDiff"]]
      # FullData = Output$FullData
      # HoldOutPeriods = Output$HoldOutPeriods
      # MinVal = Output$MinVal
      # TargetName = Output$TargetName
      # DateName = Output$DateName
      # MaxFourierTerms = MaxFourierTerms
      # TrainValidateShare = c(TrainValidateShare)
      # MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner
      # MaxNumberModels = MaxNumberModels
      # MaxRunMinutes = MaxRunMinutes
      # FinalGrid = NULL
      #
      
    }
  
  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  
  # Add final rank by all data----
  Results <- Results[Validate_MSE != -10]
  Results <- Results[order(Blended_MAE)][, ModelRank := seq_len(Results[, .N])]
  
  # Reorder columns----
  data.table::setcolorder(x = Results, neworder = c(1:12,14:ncol(Results),13))
  
  # Return
  return(Results)
}

#' ParallelAutoETS
#' 
#' ParallelAutoETS to run the 4 data sets at once
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param Output The output returned from TimeSeriesDataPrepare()
#' @param MetricSelection Choose from MAE, MSE, and MAPE
#' @param TrainValidateShare The value returned from TimeSeriesPrepare()
#' @return Time series data sets to pass onto auto modeling functions
#' @examples 
#' ParallelAutoETS(
#'   MetricSelection = "MAE",
#'   Output = NULL,
#'   TrainValidateShare = c(0.50,0.50))
#' @export
ParallelAutoETS <- function(
  Output,
  MetricSelection = "MAE",
  TrainValidateShare = c(0.50, 0.50)) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2L))
  
  # Define Modeling Artifacts----
  TrainArtifacts = list(
    UserSupplied = list(
      Data = Output$UserSuppliedData,
      Diff = if(is.null(Output$UserSuppliedDiff)) 0 else Output$UserSuppliedDiff,
      SDiff = if(is.null(Output$UserSuppliedSeasonalDiff)) 0 else Output$UserSuppliedSeasonalDiff,
      Name = "UserSupplied"),
    ModelFrequency = list(
      Data = Output$ModelFreqData,
      Diff = if(is.null(Output$ModelFreqDiff)) 0 else Output$ModelFreqDiff,
      SDiff = if(is.null(Output$ModelFreqSeasonalDiff)) 0 else Output$ModelFreqSeasonalDiff,
      Name = "ModelFrequency"),
    TSClean = list(
      Data = Output$TSCleanData,
      Diff = if(is.null(Output$TSCleanDiff)) 0 else Output$TSCleanDiff,
      SDiff = if(is.null(Output$TSCleanSeasonalDiff)) 0 else Output$TSCleanSeasonalDiff,
      Name = "TSClean"),
    TSCleanModelFreq = list(
      Data = Output$TSCleanModelFreqData,
      Diff = if(is.null(Output$TSCleanModelFreqDiff)) 0 else Output$TSCleanModelFreqDiff,
      SDiff = if(is.null(Output$TSCleanModelFreqSeasonalDiff)) 0 else Output$TSCleanModelFreqSeasonalDiff,
      Name = "TSCleanModelFrequency"))
  
  # Idenity the number of non-null data sets to run through OptimizeETS----
  Counter <- 0L
  for(i in seq_len(length(TrainArtifacts))) {
    if(!is.null(TrainArtifacts[[i]][["Data"]])) {
      Counter <- Counter + 1L    
    }
  }
  
  # Setup the parallel environment----
  packages <- c("RemixAutoML","data.table","forecast")
  cores    <- parallel::detectCores()
  cl       <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  Results <- foreach::foreach(
    i = seq_len(Counter),
    .combine = function(...) data.table::rbindlist(list(...), fill = TRUE),
    .multicombine = TRUE,
    .packages = packages) %dopar% {
      OptimizeETS(
        Output = Output,
        MetricSelection = MetricSelection,
        DataSetName = TrainArtifacts[[i]][["Name"]],
        train = TrainArtifacts[[i]][["Data"]],
        test = Output$TestData,
        FullData = Output$FullData,
        HoldOutPeriods = Output$HoldOutPeriods,
        MinVal = Output$MinVal,
        TargetName = Output$TargetName,
        DateName = Output$DateName,
        TrainValidateShare = TrainValidateShare,
        FinalGrid = NULL)
    }
  
  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  
  # Add final rank by all data----
  Results <- Results[order(Blended_MAE)][, ModelRank := seq_len(Results[, .N])]
  
  # Return----
  return(Results)
}

#' ParallelAutoTBATS
#' 
#' ParallelAutoTBATS to run the 4 data sets at once
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param Output The output returned from TimeSeriesDataPrepare()
#' @param MetricSelection Choose from MAE, MSE, and MAPE
#' @param TrainValidateShare The value returned from TimeSeriesPrepare()
#' @return Time series data sets to pass onto auto modeling functions
#' @examples 
#' ParallelAutoTBATS(
#'   MetricSelection = "MAE",
#'   Output = NULL,
#'   TrainValidateShare = c(0.50,0.50))
#' @export
ParallelAutoTBATS <- function(
  Output,
  MetricSelection = "MAE",
  TrainValidateShare = c(0.50, 0.50)) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Define Modeling Artifacts----
  TrainArtifacts = list(
    UserSupplied = list(
      Data = Output$UserSuppliedData,
      Diff = if(is.null(Output$UserSuppliedDiff)) 0 else Output$UserSuppliedDiff,
      SDiff = if(is.null(Output$UserSuppliedSeasonalDiff)) 0 else Output$UserSuppliedSeasonalDiff,
      Name = "UserSupplied"),
    ModelFrequency = list(
      Data = Output$ModelFreqData,
      Diff = if(is.null(Output$ModelFreqDiff)) 0 else Output$ModelFreqDiff,
      SDiff = if(is.null(Output$ModelFreqSeasonalDiff)) 0 else Output$ModelFreqSeasonalDiff,
      Name = "ModelFrequency"),
    TSClean = list(
      Data = Output$TSCleanData,
      Diff = if(is.null(Output$TSCleanDiff)) 0 else Output$TSCleanDiff,
      SDiff = if(is.null(Output$TSCleanSeasonalDiff)) 0 else Output$TSCleanSeasonalDiff,
      Name = "TSClean"),
    TSCleanModelFreq = list(
      Data = Output$TSCleanModelFreqData,
      Diff = if(is.null(Output$TSCleanModelFreqDiff)) 0 else Output$TSCleanModelFreqDiff,
      SDiff = if(is.null(Output$TSCleanModelFreqSeasonalDiff)) 0 else Output$TSCleanModelFreqSeasonalDiff,
      Name = "TSCleanModelFrequency"))
  
  # Idenity the number of non-null data sets to run through OptimizeArima----
  Counter <- 0L
  for(i in seq_len(length(TrainArtifacts))) if(!is.null(TrainArtifacts[[i]][["Data"]])) Counter <- Counter + 1L
  
  # Setup the parallel environment----
  packages <- c("RemixAutoML","data.table","forecast")
  cores <- parallel::detectCores()
  cl <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  Results <- foreach::foreach(
    i = seq_len(Counter),
    .combine = function(...) data.table::rbindlist(list(...), fill = TRUE),
    .multicombine = TRUE,
    .packages = packages) %dopar% {
      OptimizeTBATS(
        Output = Output,
        MetricSelection = MetricSelection,
        DataSetName = TrainArtifacts[[i]][["Name"]],
        train = TrainArtifacts[[i]][["Data"]],
        test = Output$TestData,
        Lags = Output$Lags,
        MovingAverages = Output$MovingAverages,
        FullData = Output$FullData,
        HoldOutPeriods = Output$HoldOutPeriods,
        MinVal = Output$MinVal,
        TargetName = Output$TargetName,
        DateName = Output$DateName,
        TrainValidateShare = TrainValidateShare,
        FinalGrid = NULL)
      
      #
      # MetricSelection = MetricSelection
      # DataSetName = TrainArtifacts[[i]][["Name"]]
      # train = TrainArtifacts[[i]][["Data"]]
      # test = Output$TestData
      # Lags = Output$Lags
      # MovingAverages = Output$MovingAverages
      # FullData = Output$FullData
      # HoldOutPeriods = Output$HoldOutPeriods
      # MinVal = Output$MinVal
      # TargetName = Output$TargetName
      # DateName = Output$DateName
      # TrainValidateShare = TrainValidateShare
      # FinalGrid = NULL
      #
      
    }
  
  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  
  # Add final rank by all data----
  Results <- Results[order(Blended_MAE)][, ModelRank := seq_len(Results[, .N])]
  
  # Return----
  return(Results)
}

#' ParallelAutoNNET to run the 4 data sets at once
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param MetricSelection Choose from MAE, MSE, and MAPE
#' @param Output The output returned from TimeSeriesDataPrepare()
#' @param TrainValidateShare The value returned from TimeSeriesPrepare()
#' @param MaxFourierTerms Fourier pairs
#' @param TrainValidateShare c(0.50,0.50)
#' @param MaxNumberModels 20
#' @param MaxRunMinutes 5
#' @param MaxRunsWithoutNewWinner 12
#' @return Time series data sets to pass onto auto modeling functions
#' @examples 
#' ParallelAutoNNET(
#'   MetricSelection = "MAE",
#'   Output = NULL, 
#'   MaxRunsWithoutNewWinner = 20,
#'   TrainValidateShare = c(0.50,0.50),
#'   MaxNumberModels = 5,
#'   MaxRunMinutes = 5)
#' @export
ParallelAutoNNET <- function(
  Output,
  MetricSelection = "MAE",
  MaxFourierTerms = 1,
  TrainValidateShare = c(0.50,0.50),
  MaxNumberModels = 20,
  MaxRunMinutes = 5,
  MaxRunsWithoutNewWinner = 12) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Define Modeling Artifacts----
  TrainArtifacts = list(
    UserSupplied = list(
      Data = Output$UserSuppliedData,
      Diff = if(is.null(Output$UserSuppliedDiff)) 0 else Output$UserSuppliedDiff,
      SDiff = if(is.null(Output$UserSuppliedSeasonalDiff)) 0 else Output$UserSuppliedSeasonalDiff,
      Name = "UserSupplied"),
    ModelFrequency = list(
      Data = Output$ModelFreqData,
      Diff = if(is.null(Output$ModelFreqDiff)) 0 else Output$ModelFreqDiff,
      SDiff = if(is.null(Output$ModelFreqSeasonalDiff)) 0 else Output$ModelFreqSeasonalDiff,
      Name = "ModelFrequency"),
    TSClean = list(
      Data = Output$TSCleanData,
      Diff = if(is.null(Output$TSCleanDiff)) 0 else Output$TSCleanDiff,
      SDiff = if(is.null(Output$TSCleanSeasonalDiff)) 0 else Output$TSCleanSeasonalDiff,
      Name = "TSClean"),
    TSCleanModelFreq = list(
      Data = Output$TSCleanModelFreqData,
      Diff = if(is.null(Output$TSCleanModelFreqDiff)) 0 else Output$TSCleanModelFreqDiff,
      SDiff = if(is.null(Output$TSCleanModelFreqSeasonalDiff)) 0 else Output$TSCleanModelFreqSeasonalDiff,
      Name = "TSCleanModelFrequency"))
  
  # Idenity the number of non-null data sets to run through OptimizeArima----
  Counter <- 0L
  for(i in seq_len(length(TrainArtifacts))) if(!is.null(TrainArtifacts[[i]][["Data"]])) Counter <- Counter + 1L
  
  # Setup the parallel environment----
  packages <- c("RemixAutoML","data.table","forecast")
  cores <- parallel::detectCores()
  cl <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  Results <- foreach::foreach(
    i = seq_len(Counter),
    .combine = function(...) data.table::rbindlist(list(...), fill = TRUE),
    .multicombine = TRUE,
    .packages = packages) %dopar% {
      OptimizeNNET(
        Output = Output,
        MetricSelection = MetricSelection,
        DataSetName = TrainArtifacts[[i]][["Name"]],
        train = TrainArtifacts[[i]][["Data"]],
        test = Output$TestData,
        Lags = Output$Lags,
        SeasonalLags = Output$SeasonalLags,
        FullData = Output$FullData,
        HoldOutPeriods = Output$HoldOutPeriods,
        MinVal = Output$MinVal,
        TargetName = Output$TargetName,
        DateName = Output$DateName,
        MaxFourierTerms = MaxFourierTerms,
        TrainValidateShare = TrainValidateShare,
        MaxRunsWithoutNewWinner = MaxRunsWithoutNewWinner,
        MaxNumberModels = MaxNumberModels,
        MaxRunMinutes = MaxRunMinutes)
    }
  
  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  
  # Add final rank by all data----
  Results <- Results[Validate_MSE != -10]
  Results <- Results[order(Blended_MAE)][, ModelRank := seq_len(Results[, .N])]
  
  # Reorder columns----
  data.table::setcolorder(x = Results, neworder = c(1L:9L, 11L:ncol(Results), 10L))
  
  # Return
  return(Results)
}

#' ParallelAutoArfima
#' 
#' ParallelAutoArfima to run the 4 data sets at once
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param Output The output returned from TimeSeriesDataPrepare()
#' @param MetricSelection Choose from MAE, MSE, and MAPE
#' @param TrainValidateShare The value returned from TimeSeriesPrepare()
#' @return Time series data sets to pass onto auto modeling functions
#' @examples 
#' ParallelAutoArfima(
#'   MetricSelection = "MAE",
#'   Output = NULL,
#'   TrainValidateShare = c(0.50,0.50))
#' @export
ParallelAutoArfima <- function(
  Output,
  MetricSelection = "MAE",
  TrainValidateShare = c(0.50,0.50)) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Define Modeling Artifacts----
  TrainArtifacts = list(
    UserSupplied = list(
      Data = Output$UserSuppliedData,
      Diff = if(is.null(Output$UserSuppliedDiff)) 0 else Output$UserSuppliedDiff,
      SDiff = if(is.null(Output$UserSuppliedSeasonalDiff)) 0 else Output$UserSuppliedSeasonalDiff,
      Name = "UserSupplied"),
    ModelFrequency = list(
      Data = Output$ModelFreqData,
      Diff = if(is.null(Output$ModelFreqDiff)) 0 else Output$ModelFreqDiff,
      SDiff = if(is.null(Output$ModelFreqSeasonalDiff)) 0 else Output$ModelFreqSeasonalDiff,
      Name = "ModelFrequency"),
    TSClean = list(
      Data = Output$TSCleanData,
      Diff = if(is.null(Output$TSCleanDiff)) 0 else Output$TSCleanDiff,
      SDiff = if(is.null(Output$TSCleanSeasonalDiff)) 0 else Output$TSCleanSeasonalDiff,
      Name = "TSClean"),
    TSCleanModelFreq = list(
      Data = Output$TSCleanModelFreqData,
      Diff = if(is.null(Output$TSCleanModelFreqDiff)) 0 else Output$TSCleanModelFreqDiff,
      SDiff = if(is.null(Output$TSCleanModelFreqSeasonalDiff)) 0 else Output$TSCleanModelFreqSeasonalDiff,
      Name = "TSCleanModelFrequency"))
  
  # Idenity the number of non-null data sets to run through OptimizeArima----
  Counter <- 0L
  for(i in seq_len(length(TrainArtifacts))) if(!is.null(TrainArtifacts[[i]][["Data"]])) Counter <- Counter + 1L
  
  # Setup the parallel environment----
  packages <- c("RemixAutoML","data.table","forecast")
  cores <- parallel::detectCores()
  cl <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  Results <- foreach::foreach(
    i = seq_len(Counter),
    .combine = function(...) data.table::rbindlist(list(...), fill = TRUE),
    .multicombine = TRUE,
    .packages = packages) %dopar% {
      OptimizeArfima(
        Output = Output,
        MetricSelection = MetricSelection,
        DataSetName = TrainArtifacts[[i]][["Name"]],
        train = TrainArtifacts[[i]][["Data"]],
        test = Output$TestData,
        Lags = Output$Lags,
        MovingAverages = Output$MovingAverages,
        FullData = Output$FullData,
        HoldOutPeriods = Output$HoldOutPeriods,
        MinVal = Output$MinVal,
        TargetName = Output$TargetName,
        DateName = Output$DateName,
        TrainValidateShare = TrainValidateShare,
        FinalGrid = NULL)
    }
  
  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  
  # Add final rank by all data----
  Results <- Results[order(Blended_MAE)][, ModelRank := seq_len(Results[, .N])]
  
  # Return----
  return(Results)
}

#' ParallelAutoTSLM
#' 
#' ParallelAutoTSLM to run the 4 data sets at once
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param Output The output returned from TimeSeriesDataPrepare()
#' @param MetricSelection Choose from MAE, MSE, and MAPE
#' @param TrainValidateShare The value returned from TimeSeriesPrepare()
#' @return Time series data sets to pass onto auto modeling functions
#' @examples 
#' ParallelAutoTSLM(
#'   MetricSelection = "MAE",
#'   Output = NULL,
#'   TrainValidateShare = c(0.50,0.50))
#' @export
ParallelAutoTSLM <- function(
  Output,
  MetricSelection = "MAE",
  TrainValidateShare = c(0.50, 0.50)) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2L))
  
  # Define Modeling Artifacts----
  TrainArtifacts = list(
    UserSupplied = list(
      Data = Output$UserSuppliedData,
      Diff = if(is.null(Output$UserSuppliedDiff)) 0 else Output$UserSuppliedDiff,
      SDiff = if(is.null(Output$UserSuppliedSeasonalDiff)) 0 else Output$UserSuppliedSeasonalDiff,
      Name = "UserSupplied"),
    ModelFrequency = list(
      Data = Output$ModelFreqData,
      Diff = if(is.null(Output$ModelFreqDiff)) 0 else Output$ModelFreqDiff,
      SDiff = if(is.null(Output$ModelFreqSeasonalDiff)) 0 else Output$ModelFreqSeasonalDiff,
      Name = "ModelFrequency"),
    TSClean = list(
      Data = Output$TSCleanData,
      Diff = if(is.null(Output$TSCleanDiff)) 0 else Output$TSCleanDiff,
      SDiff = if(is.null(Output$TSCleanSeasonalDiff)) 0 else Output$TSCleanSeasonalDiff,
      Name = "TSClean"),
    TSCleanModelFreq = list(
      Data = Output$TSCleanModelFreqData,
      Diff = if(is.null(Output$TSCleanModelFreqDiff)) 0 else Output$TSCleanModelFreqDiff,
      SDiff = if(is.null(Output$TSCleanModelFreqSeasonalDiff)) 0 else Output$TSCleanModelFreqSeasonalDiff,
      Name = "TSCleanModelFrequency"))
  
  # Idenity the number of non-null data sets to run through OptimizeArima----
  Counter <- 0L
  for(i in seq_len(length(TrainArtifacts))) if(!is.null(TrainArtifacts[[i]][["Data"]])) Counter <- Counter + 1L
  
  # Setup the parallel environment----
  packages <- c("RemixAutoML","data.table","forecast")
  cores <- parallel::detectCores()
  cl <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  Results <- foreach::foreach(
    i = seq_len(Counter),
    .combine = function(...) data.table::rbindlist(list(...), fill = TRUE),
    .multicombine = TRUE,
    .packages = packages) %dopar% {
      OptimizeTSLM(
        Output = Output,
        MetricSelection = MetricSelection,
        DataSetName = TrainArtifacts[[i]][["Name"]],
        train = TrainArtifacts[[i]][["Data"]],
        test = Output$TestData,
        FullData = Output$FullData,
        HoldOutPeriods = Output$HoldOutPeriods,
        MinVal = Output$MinVal,
        TargetName = Output$TargetName,
        DateName = Output$DateName,
        TrainValidateShare = TrainValidateShare,
        FinalGrid = NULL)
    }
  
  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  
  # Add final rank by all data----
  Results <- Results[order(Blended_MAE)][, ModelRank := seq_len(Results[, .N])]
  
  # Return----
  return(Results)
}

#' FinalBuildArima
#' 
#' FinalBuildArima to generate forecasts and ensemble data
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param ModelOutputGrid Pass along the grid output from ParallelOptimzeArima()
#' @param TimeSeriesPrepareOutput Output from TimeSeriesPrepare()
#' @param FCPeriods The number of periods ahead to forecast
#' @param MetricSelection The value returned from TimeSeriesPrepare()
#' @param NumberModelsScore The value returned from TimeSeriesPrepare()
#' @param ByDataType Set to TRUE if you want to have models represented from all data sets utilized in training
#' @return Time series data sets to pass onto auto modeling functions
#' @examples 
#' FinalBuildArima(
#'   Output = NULL, 
#'   TimeSeriesPrepareOutput = NULL,
#'   MaxFourierTerms = 0,
#'   TrainValidateShare = c(0.50,0.50),
#'   MaxNumberModels = 5,
#'   MaxRunMinutes = 5)
#' @export
FinalBuildArima <- function(
  ModelOutputGrid = NULL,
  TimeSeriesPrepareOutput = NULL,
  FCPeriods = 1,
  MetricSelection = "MAE",
  NumberModelsScore = 1,
  ByDataType = TRUE) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Subset ModelOutputGrid-----
  if(ByDataType) {
    if(toupper(MetricSelection) == "MAE") {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
    } else if(toupper(MetricSelection) == "MSE") {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
    } else {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
    }
  } else {
    if(toupper(MetricSelection) == "MAE") {
      ScoreGrid <- ModelOutputGrid[order(Blended_MAE)][seq_len(NumberModelsScore)]
    } else if(toupper(MetricSelection) == "MSE") {
      ScoreGrid <- ModelOutputGrid[order(Blended_MSE)][seq_len(NumberModelsScore)]
    } else {
      ScoreGrid <- ModelOutputGrid[order(Blended_MAPE)][seq_len(NumberModelsScore)]      
    }
  } 
  
  # Store Artifacts----
  TrainArtifacts = list(
    UserSupplied = list(
      Data = TimeSeriesPrepareOutput$UserSuppliedData,
      Diff = if(is.null(TimeSeriesPrepareOutput$UserSuppliedDiff)) 0 else TimeSeriesPrepareOutput$UserSuppliedDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$UserSuppliedSeasonalDiff)) 0 else TimeSeriesPrepareOutput$UserSuppliedSeasonalDiff,
      Name = "UserSupplied"),
    ModelFrequency = list(
      Data = TimeSeriesPrepareOutput$ModelFreqData,
      Diff = if(is.null(TimeSeriesPrepareOutput$ModelFreqDiff)) 0 else TimeSeriesPrepareOutput$ModelFreqDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$ModelFreqSeasonalDiff)) 0 else TimeSeriesPrepareOutput$ModelFreqSeasonalDiff,
      Name = "ModelFrequency"),
    TSClean = list(
      Data = TimeSeriesPrepareOutput$TSCleanData,
      Diff = if(is.null(TimeSeriesPrepareOutput$TSCleanDiff)) 0 else TimeSeriesPrepareOutput$TSCleanDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$TSCleanSeasonalDiff)) 0 else TimeSeriesPrepareOutput$TSCleanSeasonalDiff,
      Name = "TSClean"),
    TSCleanModelFrequency = list(
      Data = TimeSeriesPrepareOutput$TSCleanModelFreqData,
      Diff = if(is.null(TimeSeriesPrepareOutput$TSCleanModelFreqDiff)) 0 else TimeSeriesPrepareOutput$TSCleanModelFreqDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$TSCleanModelFreqSeasonalDiff)) 0 else TimeSeriesPrepareOutput$TSCleanModelFreqSeasonalDiff,
      Name = "TSCleanModelFrequency"))
  
  # Idenity the number of non-null data sets to run through OptimizeArima----
  Counter <- 0L
  for(i in seq_len(ScoreGrid[, .N])) {
    if(!is.null(TrainArtifacts[[i]][["Data"]])) {
      Counter <- Counter + 1L    
    }
  }
  
  # Setup the parallel environment----
  packages <- c("RemixAutoML","data.table","forecast")
  cores <- parallel::detectCores()
  cl <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  Results <- foreach::foreach(
    i = seq_len(Counter),
    .combine = function(...) data.table::rbindlist(list(...), fill = TRUE),
    .multicombine = TRUE,
    .packages = packages) %dopar% {
      
      # Score models----
      if(ByDataType) {
        Forecasts <- OptimizeArima(
          Output = TimeSeriesPrepareOutput,
          MetricSelection = MetricSelection,
          DataSetName = TrainArtifacts[[i]][["Name"]],
          train = TrainArtifacts[[i]][["Data"]],
          test = ModelOutputGrid$TestData,
          Lags = TimeSeriesPrepareOutput$Lags,
          SeasonalLags = TimeSeriesPrepareOutput$SeasonalLags,
          MovingAverages = TimeSeriesPrepareOutput$MovingAverages,
          SeasonalMovingAverages = TimeSeriesPrepareOutput$SeasonalMovingAverages,
          Differences = TrainArtifacts[[i]][["Diff"]],
          SeasonalDifferences = TrainArtifacts[[i]][["SDiff"]],
          FullData = TimeSeriesPrepareOutput$FullData,
          HoldOutPeriods = TimeSeriesPrepareOutput$HoldOutPeriods,
          MinVal = TimeSeriesPrepareOutput$MinVal,
          TargetName = TimeSeriesPrepareOutput$TargetName,
          DateName = TimeSeriesPrepareOutput$DateName,
          MaxFourierTerms = 0,
          TrainValidateShare = c(1.0,0.0),
          MaxNumberModels = NumberModelsScore,
          MaxRunMinutes = 100,
          FinalGrid = ScoreGrid[DataSetName == TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]]])
        
        # Output = TimeSeriesPrepareOutput
        # MetricSelection = MetricSelection
        # DataSetName = TrainArtifacts[[i]][["Name"]]
        # train = TrainArtifacts[[i]][["Data"]]
        # test = ModelOutputGrid$TestData
        # Lags = TimeSeriesPrepareOutput$Lags
        # SeasonalLags = TimeSeriesPrepareOutput$SeasonalLags
        # MovingAverages = TimeSeriesPrepareOutput$MovingAverages
        # SeasonalMovingAverages = TimeSeriesPrepareOutput$SeasonalMovingAverages
        # Differences = TrainArtifacts[[i]][["Diff"]]
        # SeasonalDifferences = TrainArtifacts[[i]][["SDiff"]]
        # FullData = TimeSeriesPrepareOutput$FullData
        # HoldOutPeriods = TimeSeriesPrepareOutput$HoldOutPeriods
        # MinVal = TimeSeriesPrepareOutput$MinVal
        # TargetName = TimeSeriesPrepareOutput$TargetName
        # DateName = TimeSeriesPrepareOutput$DateName
        # MaxFourierTerms = 0
        # TrainValidateShare = c(1.0,0.0)
        # MaxNumberModels = NumberModelsScore
        # MaxRunMinutes = 100
        # FinalGrid = ScoreGrid[DataSetName == TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]]]
        
      } else {
        Forecasts <- OptimizeArima(
          Output = TimeSeriesPrepareOutput,
          DataSetName = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]],
          train = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Data"]],
          test = TimeSeriesPrepareOutput$TestData,
          Lags = TimeSeriesPrepareOutput$Lags,
          SeasonalLags = TimeSeriesPrepareOutput$SeasonalLags,
          MovingAverages = TimeSeriesPrepareOutput$MovingAverages,
          SeasonalMovingAverages = TimeSeriesPrepareOutput$SeasonalMovingAverages,
          Differences = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Diff"]],
          SeasonalDifferences = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["SDiff"]],
          FullData = TimeSeriesPrepareOutput$FullData,
          HoldOutPeriods = TimeSeriesPrepareOutput$HoldOutPeriods,
          MinVal = TimeSeriesPrepareOutput$MinVal,
          TargetName = TimeSeriesPrepareOutput$TargetName,
          DateName = TimeSeriesPrepareOutput$DateName,
          MaxFourierTerms = 0,
          TrainValidateShare = c(1.0,0.0),
          MaxNumberModels = NumberModelsScore,
          MaxRunMinutes = 100,
          FinalGrid = ScoreGrid[i])
      }
      Forecasts
    }
  
  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  
  # Return----
  return(Results[!is.na(Forecast)])
}

#' FinalBuildETS
#' 
#' FinalBuildETS to generate forecasts and ensemble data
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param ModelOutputGrid Pass along the grid output from ParallelOptimzeArima()
#' @param TimeSeriesPrepareOutput Output from TimeSeriesPrepare()
#' @param FCPeriods The number of periods ahead to forecast
#' @param MetricSelection The value returned from TimeSeriesPrepare()
#' @param NumberModelsScore The value returned from TimeSeriesPrepare()
#' @param ByDataType Set to TRUE if you want to have models represented from all data sets utilized in training
#' @return Time series data sets to pass onto auto modeling functions
#' @examples 
#' FinalBuildETS(
#'   Output = NULL, 
#'   TimeSeriesPrepareOutput = NULL,
#'   MaxFourierTerms = 0,
#'   TrainValidateShare = c(0.50,0.50),
#'   MaxNumberModels = 5,
#'   MaxRunMinutes = 5)
#' @export
FinalBuildETS <- function(
  ModelOutputGrid = NULL,
  TimeSeriesPrepareOutput = NULL,
  FCPeriods = 1,
  MetricSelection = "MAE",
  NumberModelsScore = 12,
  ByDataType = TRUE) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Subset ModelOutputGrid-----
  if(ByDataType) {
    if(toupper(MetricSelection) == "MAE") {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
    } else if(toupper(MetricSelection) == "MSE") {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
    } else {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
    }
  } else {
    if(toupper(MetricSelection) == "MAE") {
      ScoreGrid <- ModelOutputGrid[order(Blended_MAE)][seq_len(NumberModelsScore)]
    } else if(toupper(MetricSelection) == "MSE") {
      ScoreGrid <- ModelOutputGrid[order(Blended_MSE)][seq_len(NumberModelsScore)]
    } else {
      ScoreGrid <- ModelOutputGrid[order(Blended_MAPE)][seq_len(NumberModelsScore)]      
    }
  } 
  
  # Store Artifacts----
  TrainArtifacts = list(
    UserSupplied = list(
      Data = TimeSeriesPrepareOutput$UserSuppliedData,
      Diff = if(is.null(TimeSeriesPrepareOutput$UserSuppliedDiff)) 0 else TimeSeriesPrepareOutput$UserSuppliedDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$UserSuppliedSeasonalDiff)) 0 else TimeSeriesPrepareOutput$UserSuppliedSeasonalDiff,
      Name = "UserSupplied"),
    ModelFrequency = list(
      Data = TimeSeriesPrepareOutput$ModelFreqData,
      Diff = if(is.null(TimeSeriesPrepareOutput$ModelFreqDiff)) 0 else TimeSeriesPrepareOutput$ModelFreqDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$ModelFreqSeasonalDiff)) 0 else TimeSeriesPrepareOutput$ModelFreqSeasonalDiff,
      Name = "ModelFrequency"),
    TSClean = list(
      Data = TimeSeriesPrepareOutput$TSCleanData,
      Diff = if(is.null(TimeSeriesPrepareOutput$TSCleanDiff)) 0 else TimeSeriesPrepareOutput$TSCleanDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$TSCleanSeasonalDiff)) 0 else TimeSeriesPrepareOutput$TSCleanSeasonalDiff,
      Name = "TSClean"),
    TSCleanModelFrequency = list(
      Data = TimeSeriesPrepareOutput$TSCleanModelFreqData,
      Diff = if(is.null(TimeSeriesPrepareOutput$TSCleanModelFreqDiff)) 0 else TimeSeriesPrepareOutput$TSCleanModelFreqDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$TSCleanModelFreqSeasonalDiff)) 0 else TimeSeriesPrepareOutput$TSCleanModelFreqSeasonalDiff,
      Name = "TSCleanModelFrequency"))
  
  # Idenity the number of non-null data sets to run through OptimizeArima----
  Counter <- 0L
  for(i in seq_len(ScoreGrid[, .N])) {
    if(!is.null(TrainArtifacts[[i]][["Data"]])) {
      Counter <- Counter + 1L    
    }
  }
  
  # Setup the parallel environment----
  packages <- c("RemixAutoML","data.table","forecast")
  cores <- parallel::detectCores()
  cl <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  Results <- foreach::foreach(
    i = seq_len(Counter),
    .combine = function(...) data.table::rbindlist(list(...), fill = TRUE),
    .multicombine = TRUE,
    .packages = packages) %dopar% {
      
      # Score models----
      if(ByDataType) {
        Forecasts <- OptimizeETS(
          Output = TimeSeriesPrepareOutput,
          MetricSelection = MetricSelection,
          DataSetName = TrainArtifacts[[i]][["Name"]],
          train = TrainArtifacts[[i]][["Data"]],
          test = ModelOutputGrid$TestData,
          FullData = ModelOutputGrid$FullData,
          HoldOutPeriods = ModelOutputGrid$HoldOutPeriods,
          MinVal = ModelOutputGrid$MinVal,
          TargetName = ModelOutputGrid$TargetName,
          DateName = ModelOutputGrid$DateName,
          TrainValidateShare = c(1.0,0.0,0.0),
          FinalGrid = ScoreGrid[DataSetName == TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]]])
      } else {
        Forecasts <- OptimizeETS(
          Output = TimeSeriesPrepareOutput,
          MetricSelection = MetricSelection,
          DataSetName = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]],
          train = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Data"]],
          test = ModelOutputGrid$TestData,
          FullData = ModelOutputGrid$FullData,
          HoldOutPeriods = ModelOutputGrid$HoldOutPeriods,
          MinVal = ModelOutputGrid$MinVal,
          TargetName = ModelOutputGrid$TargetName,
          DateName = ModelOutputGrid$DateName,
          TrainValidateShare = c(1.0,0.0,0.0),
          FinalGrid = ScoreGrid[i])
      }
      Forecasts
    }
  
  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  
  # Return----
  return(Results[!is.na(Forecast)])
}

#' FinalBuildTBATS
#' 
#' FinalBuildTBATS to generate forecasts and ensemble data
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param ModelOutputGrid Pass along the grid output from ParallelOptimzeArima()
#' @param TimeSeriesPrepareOutput Output from TimeSeriesPrepare()
#' @param FCPeriods The number of periods ahead to forecast
#' @param MetricSelection The value returned from TimeSeriesPrepare()
#' @param NumberModelsScore The value returned from TimeSeriesPrepare()
#' @param ByDataType Set to TRUE if you want to have models represented from all data sets utilized in training
#' @return Time series data sets to pass onto auto modeling functions
#' @examples 
#' FinalBuildTBATS(
#'   Output = NULL, 
#'   TimeSeriesPrepareOutput = NULL,
#'   MaxFourierTerms = 0,
#'   TrainValidateShare = c(0.50,0.50),
#'   MaxNumberModels = 5,
#'   MaxRunMinutes = 5)
#' @export
FinalBuildTBATS <- function(
  ModelOutputGrid = NULL,
  TimeSeriesPrepareOutput = NULL,
  FCPeriods = 1,
  MetricSelection = "MAE",
  NumberModelsScore = 1,
  ByDataType = TRUE) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Subset ModelOutputGrid-----
  if(ByDataType) {
    if(toupper(MetricSelection) == "MAE") {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
    } else if(toupper(MetricSelection) == "MSE") {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
    } else {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
    }
  } else {
    if(toupper(MetricSelection) == "MAE") {
      ScoreGrid <- ModelOutputGrid[order(Blended_MAE)][seq_len(NumberModelsScore)]
    } else if(toupper(MetricSelection) == "MSE") {
      ScoreGrid <- ModelOutputGrid[order(Blended_MSE)][seq_len(NumberModelsScore)]
    } else {
      ScoreGrid <- ModelOutputGrid[order(Blended_MAPE)][seq_len(NumberModelsScore)]      
    }
  } 
  
  # Store Artifacts----
  TrainArtifacts = list(
    UserSupplied = list(
      Data = TimeSeriesPrepareOutput$UserSuppliedData,
      Diff = if(is.null(TimeSeriesPrepareOutput$UserSuppliedDiff)) 0 else TimeSeriesPrepareOutput$UserSuppliedDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$UserSuppliedSeasonalDiff)) 0 else TimeSeriesPrepareOutput$UserSuppliedSeasonalDiff,
      Name = "UserSupplied"),
    ModelFrequency = list(
      Data = TimeSeriesPrepareOutput$ModelFreqData,
      Diff = if(is.null(TimeSeriesPrepareOutput$ModelFreqDiff)) 0 else TimeSeriesPrepareOutput$ModelFreqDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$ModelFreqSeasonalDiff)) 0 else TimeSeriesPrepareOutput$ModelFreqSeasonalDiff,
      Name = "ModelFrequency"),
    TSClean = list(
      Data = TimeSeriesPrepareOutput$TSCleanData,
      Diff = if(is.null(TimeSeriesPrepareOutput$TSCleanDiff)) 0 else TimeSeriesPrepareOutput$TSCleanDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$TSCleanSeasonalDiff)) 0 else TimeSeriesPrepareOutput$TSCleanSeasonalDiff,
      Name = "TSClean"),
    TSCleanModelFrequency = list(
      Data = TimeSeriesPrepareOutput$TSCleanModelFreqData,
      Diff = if(is.null(TimeSeriesPrepareOutput$TSCleanModelFreqDiff)) 0 else TimeSeriesPrepareOutput$TSCleanModelFreqDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$TSCleanModelFreqSeasonalDiff)) 0 else TimeSeriesPrepareOutput$TSCleanModelFreqSeasonalDiff,
      Name = "TSCleanModelFrequency"))
  
  # Idenity the number of non-null data sets to run through OptimizeArima----
  Counter <- 0L
  for(i in seq_len(ScoreGrid[, .N])) if(!is.null(TrainArtifacts[[i]][["Data"]])) Counter <- Counter + 1L
  
  # Setup the parallel environment----
  packages <- c("RemixAutoML","data.table","forecast")
  cores <- parallel::detectCores()
  cl <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  Results <- foreach::foreach(
    i = seq_len(Counter),
    .combine = function(...) data.table::rbindlist(list(...), fill = TRUE),
    .multicombine = TRUE,
    .packages = packages) %dopar% {
      
      # Score models----
      if(ByDataType) {
        Forecasts <- OptimizeTBATS(
          Output = TimeSeriesPrepareOutput,
          MetricSelection = MetricSelection,
          DataSetName = TrainArtifacts[[i]][["Name"]],
          train = TrainArtifacts[[i]][["Data"]],
          test = TimeSeriesPrepareOutput$TestData,
          FullData = TimeSeriesPrepareOutput$FullData,
          HoldOutPeriods = TimeSeriesPrepareOutput$HoldOutPeriods,
          MinVal = TimeSeriesPrepareOutput$MinVal,
          TargetName = TimeSeriesPrepareOutput$TargetName,
          DateName = TimeSeriesPrepareOutput$DateName,
          TrainValidateShare = c(1.0,0.0,0.0),
          FinalGrid = ScoreGrid[DataSetName == TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]]])
      } else {
        Forecasts <- OptimizeTBATS(
          Output = TimeSeriesPrepareOutput,
          MetricSelection = MetricSelection,
          DataSetName = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]],
          train = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Data"]],
          test = TimeSeriesPrepareOutput$TestData,
          FullData = TimeSeriesPrepareOutput$FullData,
          HoldOutPeriods = TimeSeriesPrepareOutput$HoldOutPeriods,
          MinVal = TimeSeriesPrepareOutput$MinVal,
          TargetName = TimeSeriesPrepareOutput$TargetName,
          DateName = TimeSeriesPrepareOutput$DateName,
          TrainValidateShare = c(1.0,0.0,0.0),
          FinalGrid = ScoreGrid[i])
      }
      Forecasts
    }
  
  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  
  # Return----
  return(Results[!is.na(Forecast)])
}

#' FinalBuildNNET
#' 
#' FinalBuildNNET to generate forecasts and ensemble data
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param ModelOutputGrid Pass along the grid output from ParallelOptimzeArima()
#' @param TimeSeriesPrepareOutput Output from TimeSeriesPrepare()
#' @param FCPeriods The number of periods ahead to forecast
#' @param MetricSelection The value returned from TimeSeriesPrepare()
#' @param NumberModelsScore The value returned from TimeSeriesPrepare()
#' @param ByDataType Set to TRUE if you want to have models represented from all data sets utilized in training
#' @return Time series data sets to pass onto auto modeling functions
#' @examples 
#' FinalBuildNNET(
#'   Output = NULL, 
#'   TimeSeriesPrepareOutput = NULL,
#'   MaxFourierTerms = 0,
#'   TrainValidateShare = c(0.50,0.50),
#'   MaxNumberModels = 5,
#'   MaxRunMinutes = 5)
#' @export
FinalBuildNNET <- function(
  ModelOutputGrid = NULL,
  TimeSeriesPrepareOutput = NULL,
  FCPeriods = 1,
  MetricSelection = "MAE",
  NumberModelsScore = 1,
  ByDataType = TRUE) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Subset ModelOutputGrid-----
  if(ByDataType) {
    if(toupper(MetricSelection) == "MAE") {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
    } else if(toupper(MetricSelection) == "MSE") {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
    } else {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
    }
  } else {
    if(toupper(MetricSelection) == "MAE") {
      ScoreGrid <- ModelOutputGrid[order(Blended_MAE)][seq_len(NumberModelsScore)]
    } else if(toupper(MetricSelection) == "MSE") {
      ScoreGrid <- ModelOutputGrid[order(Blended_MSE)][seq_len(NumberModelsScore)]
    } else {
      ScoreGrid <- ModelOutputGrid[order(Blended_MAPE)][seq_len(NumberModelsScore)]      
    }
  } 
  
  # Store Artifacts----
  TrainArtifacts = list(
    UserSupplied = list(
      Data = TimeSeriesPrepareOutput$UserSuppliedData,
      Diff = if(is.null(TimeSeriesPrepareOutput$UserSuppliedDiff)) 0 else TimeSeriesPrepareOutput$UserSuppliedDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$UserSuppliedSeasonalDiff)) 0 else TimeSeriesPrepareOutput$UserSuppliedSeasonalDiff,
      Name = "UserSupplied"),
    ModelFrequency = list(
      Data = TimeSeriesPrepareOutput$ModelFreqData,
      Diff = if(is.null(TimeSeriesPrepareOutput$ModelFreqDiff)) 0 else TimeSeriesPrepareOutput$ModelFreqDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$ModelFreqSeasonalDiff)) 0 else TimeSeriesPrepareOutput$ModelFreqSeasonalDiff,
      Name = "ModelFrequency"),
    TSClean = list(
      Data = TimeSeriesPrepareOutput$TSCleanData,
      Diff = if(is.null(TimeSeriesPrepareOutput$TSCleanDiff)) 0 else TimeSeriesPrepareOutput$TSCleanDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$TSCleanSeasonalDiff)) 0 else TimeSeriesPrepareOutput$TSCleanSeasonalDiff,
      Name = "TSClean"),
    TSCleanModelFrequency = list(
      Data = TimeSeriesPrepareOutput$TSCleanModelFreqData,
      Diff = if(is.null(TimeSeriesPrepareOutput$TSCleanModelFreqDiff)) 0 else TimeSeriesPrepareOutput$TSCleanModelFreqDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$TSCleanModelFreqSeasonalDiff)) 0 else TimeSeriesPrepareOutput$TSCleanModelFreqSeasonalDiff,
      Name = "TSCleanModelFrequency"))
  
  # Idenity the number of non-null data sets to run through OptimizeArima----
  Counter <- 0L
  for(i in seq_len(ScoreGrid[, .N])) if(!is.null(TrainArtifacts[[i]][["Data"]])) Counter <- Counter + 1L
  
  # Setup the parallel environment----
  packages <- c("RemixAutoML","data.table","forecast")
  cores <- parallel::detectCores()
  cl <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  Results <- foreach::foreach(
    i = seq_len(Counter),
    .combine = function(...) data.table::rbindlist(list(...), fill = TRUE),
    .multicombine = TRUE,
    .packages = packages) %dopar% {
      
      # Score models----
      if(ByDataType) {
        Forecasts <- OptimizeNNET(
          Output = TimeSeriesPrepareOutput,
          MetricSelection = MetricSelection,
          DataSetName = TrainArtifacts[[i]][["Name"]],
          train = TrainArtifacts[[i]][["Data"]],
          test = TimeSeriesPrepareOutput$TestData,
          Lags = TimeSeriesPrepareOutput$Lags,
          SeasonalLags = TimeSeriesPrepareOutput$SeasonalLags,
          FullData = TimeSeriesPrepareOutput$FullData,
          HoldOutPeriods = TimeSeriesPrepareOutput$HoldOutPeriods,
          MinVal = TimeSeriesPrepareOutput$MinVal,
          TargetName = TimeSeriesPrepareOutput$TargetName,
          DateName = TimeSeriesPrepareOutput$DateName,
          MaxFourierTerms = 0,
          TrainValidateShare = c(1.0,0.0,0.0),
          MaxNumberModels = NumberModelsScore,
          MaxRunMinutes = 100,
          FinalGrid = ScoreGrid[DataSetName == TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]]])
      } else {
        Forecasts <- OptimizeNNET(
          Output = TimeSeriesPrepareOutput,
          DataSetName = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]],
          train = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Data"]],
          test = TimeSeriesPrepareOutput$TestData,
          Lags = TimeSeriesPrepareOutput$Lags,
          SeasonalLags = TimeSeriesPrepareOutput$SeasonalLags,
          FullData = TimeSeriesPrepareOutput$FullData,
          HoldOutPeriods = TimeSeriesPrepareOutput$HoldOutPeriods,
          MinVal = TimeSeriesPrepareOutput$MinVal,
          TargetName = TimeSeriesPrepareOutput$TargetName,
          DateName = TimeSeriesPrepareOutput$DateName,
          MaxFourierTerms = 0,
          TrainValidateShare = c(1.0,0.0,0.0),
          MaxNumberModels = NumberModelsScore,
          MaxRunMinutes = 100,
          FinalGrid = ScoreGrid[i])
      }
      Forecasts
    }
  
  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  
  # Return----
  return(Results[!is.na(Forecast)])
}

#' FinalBuildArfima
#' 
#' FinalBuildArfima to generate forecasts and ensemble data
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param ModelOutputGrid Pass along the grid output from ParallelOptimzeArima()
#' @param TimeSeriesPrepareOutput Output from TimeSeriesPrepare()
#' @param FCPeriods The number of periods ahead to forecast
#' @param MetricSelection The value returned from TimeSeriesPrepare()
#' @param NumberModelsScore The value returned from TimeSeriesPrepare()
#' @param ByDataType Set to TRUE if you want to have models represented from all data sets utilized in training
#' @return Time series data sets to pass onto auto modeling functions
#' @examples 
#' FinalBuildArfima(
#'   Output = NULL, 
#'   TimeSeriesPrepareOutput = NULL,
#'   MaxFourierTerms = 0,
#'   TrainValidateShare = c(0.50,0.50),
#'   MaxNumberModels = 5,
#'   MaxRunMinutes = 5)
#' @export
FinalBuildArfima <- function(
  ModelOutputGrid = NULL,
  TimeSeriesPrepareOutput = NULL,
  FCPeriods = 1,
  MetricSelection = "MAE",
  NumberModelsScore = 1,
  ByDataType = TRUE) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
  
  # Subset ModelOutputGrid-----
  if(ByDataType) {
    if(toupper(MetricSelection) == "MAE") {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
    } else if(toupper(MetricSelection) == "MSE") {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
    } else {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
    }
  } else {
    if(toupper(MetricSelection) == "MAE") {
      ScoreGrid <- ModelOutputGrid[order(Blended_MAE)][seq_len(NumberModelsScore)]
    } else if(toupper(MetricSelection) == "MSE") {
      ScoreGrid <- ModelOutputGrid[order(Blended_MSE)][seq_len(NumberModelsScore)]
    } else {
      ScoreGrid <- ModelOutputGrid[order(Blended_MAPE)][seq_len(NumberModelsScore)]      
    }
  } 
  
  # Store Artifacts----
  TrainArtifacts = list(
    UserSupplied = list(
      Data = TimeSeriesPrepareOutput$UserSuppliedData,
      Diff = if(is.null(TimeSeriesPrepareOutput$UserSuppliedDiff)) 0 else TimeSeriesPrepareOutput$UserSuppliedDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$UserSuppliedSeasonalDiff)) 0 else TimeSeriesPrepareOutput$UserSuppliedSeasonalDiff,
      Name = "UserSupplied"),
    ModelFrequency = list(
      Data = TimeSeriesPrepareOutput$ModelFreqData,
      Diff = if(is.null(TimeSeriesPrepareOutput$ModelFreqDiff)) 0 else TimeSeriesPrepareOutput$ModelFreqDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$ModelFreqSeasonalDiff)) 0 else TimeSeriesPrepareOutput$ModelFreqSeasonalDiff,
      Name = "ModelFrequency"),
    TSClean = list(
      Data = TimeSeriesPrepareOutput$TSCleanData,
      Diff = if(is.null(TimeSeriesPrepareOutput$TSCleanDiff)) 0 else TimeSeriesPrepareOutput$TSCleanDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$TSCleanSeasonalDiff)) 0 else TimeSeriesPrepareOutput$TSCleanSeasonalDiff,
      Name = "TSClean"),
    TSCleanModelFrequency = list(
      Data = TimeSeriesPrepareOutput$TSCleanModelFreqData,
      Diff = if(is.null(TimeSeriesPrepareOutput$TSCleanModelFreqDiff)) 0 else TimeSeriesPrepareOutput$TSCleanModelFreqDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$TSCleanModelFreqSeasonalDiff)) 0 else TimeSeriesPrepareOutput$TSCleanModelFreqSeasonalDiff,
      Name = "TSCleanModelFrequency"))
  
  # Idenity the number of non-null data sets to run through OptimizeArima----
  Counter <- 0L
  for(i in seq_len(ScoreGrid[, .N])) if(!is.null(TrainArtifacts[[i]][["Data"]])) Counter <- Counter + 1L
  
  # Setup the parallel environment----
  packages <- c("RemixAutoML","data.table","forecast")
  cores <- parallel::detectCores()
  cl <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  Results <- foreach::foreach(
    i = seq_len(Counter),
    .combine = function(...) data.table::rbindlist(list(...), fill = TRUE),
    .multicombine = TRUE,
    .packages = packages) %dopar% {
      
      # Score models----
      if(ByDataType) {
        Forecasts <- OptimizeArfima(
          Output = TimeSeriesPrepareOutput,
          MetricSelection = MetricSelection,
          DataSetName = TrainArtifacts[[i]][["Name"]],
          train = TrainArtifacts[[i]][["Data"]],
          test = TimeSeriesPrepareOutput$TestData,
          FullData = TimeSeriesPrepareOutput$FullData,
          HoldOutPeriods = TimeSeriesPrepareOutput$HoldOutPeriods,
          MinVal = TimeSeriesPrepareOutput$MinVal,
          TargetName = TimeSeriesPrepareOutput$TargetName,
          DateName = TimeSeriesPrepareOutput$DateName,
          TrainValidateShare = c(1.0,0.0,0.0),
          FinalGrid = ScoreGrid[DataSetName == TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]]])
      } else {
        Forecasts <- OptimizeArfima(
          Output = TimeSeriesPrepareOutput,
          MetricSelection = MetricSelection,
          DataSetName = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]],
          train = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Data"]],
          test = TimeSeriesPrepareOutput$TestData,
          FullData = TimeSeriesPrepareOutput$FullData,
          HoldOutPeriods = TimeSeriesPrepareOutput$HoldOutPeriods,
          MinVal = TimeSeriesPrepareOutput$MinVal,
          TargetName = TimeSeriesPrepareOutput$TargetName,
          DateName = TimeSeriesPrepareOutput$DateName,
          TrainValidateShare = c(1.0,0.0,0.0),
          FinalGrid = ScoreGrid[i])
      }
      Forecasts
    }
  
  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  
  # Return----
  return(Results[!is.na(Forecast)])
}

#' FinalBuildTSLM
#' 
#' FinalBuildTSLM to generate forecasts and ensemble data
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param ModelOutputGrid Pass along the grid output from ParallelOptimzeArima()
#' @param TimeSeriesPrepareOutput Output from TimeSeriesPrepare()
#' @param FCPeriods The number of periods ahead to forecast
#' @param MetricSelection The value returned from TimeSeriesPrepare()
#' @param NumberModelsScore The value returned from TimeSeriesPrepare()
#' @param ByDataType Set to TRUE if you want to have models represented from all data sets utilized in training
#' @return Time series data sets to pass onto auto modeling functions
#' @examples 
#' FinalBuildTSLM(
#'   Output = NULL, 
#'   TimeSeriesPrepareOutput = NULL,
#'   MaxFourierTerms = 0,
#'   TrainValidateShare = c(0.50,0.50),
#'   MaxNumberModels = 5,
#'   MaxRunMinutes = 5)
#' @export
FinalBuildTSLM <- function(
  ModelOutputGrid = NULL,
  TimeSeriesPrepareOutput = NULL,
  FCPeriods = 1,
  MetricSelection = "MAE",
  NumberModelsScore = 1,
  ByDataType = TRUE) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Subset ModelOutputGrid-----
  if(ByDataType) {
    if(toupper(MetricSelection) == "MAE") {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MAE)][seq_len(ceiling(NumberModelsScore))]))
    } else if(toupper(MetricSelection) == "MSE") {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MSE)][seq_len(ceiling(NumberModelsScore))]))
    } else {
      ScoreGrid <- ModelOutputGrid[DataSetName == "UserSupplied"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "ModelFrequency"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSClean"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
      ScoreGrid <- data.table::rbindlist(list(ScoreGrid, ModelOutputGrid[DataSetName == "TSCleanModelFrequency"][order(Blended_MAPE)][seq_len(ceiling(NumberModelsScore))]))
    }
  } else {
    if(toupper(MetricSelection) == "MAE") {
      ScoreGrid <- ModelOutputGrid[order(Blended_MAE)][seq_len(NumberModelsScore)]
    } else if(toupper(MetricSelection) == "MSE") {
      ScoreGrid <- ModelOutputGrid[order(Blended_MSE)][seq_len(NumberModelsScore)]
    } else {
      ScoreGrid <- ModelOutputGrid[order(Blended_MAPE)][seq_len(NumberModelsScore)]      
    }
  } 
  
  # Store Artifacts----
  TrainArtifacts = list(
    UserSupplied = list(
      Data = TimeSeriesPrepareOutput$UserSuppliedData,
      Diff = if(is.null(TimeSeriesPrepareOutput$UserSuppliedDiff)) 0 else TimeSeriesPrepareOutput$UserSuppliedDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$UserSuppliedSeasonalDiff)) 0 else TimeSeriesPrepareOutput$UserSuppliedSeasonalDiff,
      Name = "UserSupplied"),
    ModelFrequency = list(
      Data = TimeSeriesPrepareOutput$ModelFreqData,
      Diff = if(is.null(TimeSeriesPrepareOutput$ModelFreqDiff)) 0 else TimeSeriesPrepareOutput$ModelFreqDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$ModelFreqSeasonalDiff)) 0 else TimeSeriesPrepareOutput$ModelFreqSeasonalDiff,
      Name = "ModelFrequency"),
    TSClean = list(
      Data = TimeSeriesPrepareOutput$TSCleanData,
      Diff = if(is.null(TimeSeriesPrepareOutput$TSCleanDiff)) 0 else TimeSeriesPrepareOutput$TSCleanDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$TSCleanSeasonalDiff)) 0 else TimeSeriesPrepareOutput$TSCleanSeasonalDiff,
      Name = "TSClean"),
    TSCleanModelFrequency = list(
      Data = TimeSeriesPrepareOutput$TSCleanModelFreqData,
      Diff = if(is.null(TimeSeriesPrepareOutput$TSCleanModelFreqDiff)) 0 else TimeSeriesPrepareOutput$TSCleanModelFreqDiff,
      SDiff = if(is.null(TimeSeriesPrepareOutput$TSCleanModelFreqSeasonalDiff)) 0 else TimeSeriesPrepareOutput$TSCleanModelFreqSeasonalDiff,
      Name = "TSCleanModelFrequency"))
  
  # Idenity the number of non-null data sets to run through OptimizeArima----
  Counter <- 0L
  for(i in seq_len(ScoreGrid[, .N])) if(!is.null(TrainArtifacts[[i]][["Data"]])) Counter <- Counter + 1L
  
  # Setup the parallel environment----
  packages <- c("RemixAutoML","data.table","forecast")
  cores <- parallel::detectCores()
  cl <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  Results <- foreach::foreach(
    i = seq_len(Counter),
    .combine = function(...) data.table::rbindlist(list(...), fill = TRUE),
    .multicombine = TRUE,
    .packages = packages) %dopar% {
      
      # Score models----
      if(ByDataType) {
        Forecasts <- OptimizeTSLM(
          Output = TimeSeriesPrepareOutput,
          MetricSelection = MetricSelection,
          DataSetName = TrainArtifacts[[i]][["Name"]],
          train = TrainArtifacts[[i]][["Data"]],
          test = ModelOutputGrid$TestData,
          FullData = ModelOutputGrid$FullData,
          HoldOutPeriods = ModelOutputGrid$HoldOutPeriods,
          MinVal = ModelOutputGrid$MinVal,
          TargetName = TimeSeriesPrepareOutput$TargetName,
          DateName = TimeSeriesPrepareOutput$DateName,
          TrainValidateShare = c(1.0,0.0,0.0),
          FinalGrid = ScoreGrid[DataSetName == TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]]])
      } else {
        Forecasts <- OptimizeTSLM(
          Output = TimeSeriesPrepareOutput,
          MetricSelection = MetricSelection,
          DataSetName = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Name"]],
          train = TrainArtifacts[[ScoreGrid[i,1][[1]]]][["Data"]],
          test = TimeSeriesPrepareOutput$TestData,
          FullData = TimeSeriesPrepareOutput$FullData,
          HoldOutPeriods = TimeSeriesPrepareOutput$HoldOutPeriods,
          MinVal = TimeSeriesPrepareOutput$MinVal,
          TargetName = TimeSeriesPrepareOutput$TargetName,
          DateName = TimeSeriesPrepareOutput$DateName,
          TrainValidateShare = c(1.0,0.0,0.0),
          FinalGrid = ScoreGrid[i])
      }
      Forecasts
    }
  
  # shut down parallel objects----
  parallel::stopCluster(cl)
  rm(cl)
  
  # Return----
  return(Results[!is.na(Forecast)])
}

#' TimeSeriesEnsembleForecast
#' 
#' TimeSeriesEnsembleForecast to generate forecasts and ensemble data
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param TS_Models Select which ts model forecasts to ensemble
#' @param ML_Methods Select which models to build for the ensemble
#' @param CalendarFeatures TRUE or FALSE
#' @param HolidayFeatures TRUE or FALSE
#' @param FourierFeatures Full set of fourier features for train and score
#' @param Path The path to the folder where the ts forecasts are stored
#' @param TargetName "Weekly_Sales"
#' @param DateName "Date"
#' @param NTrees Select the number of trees to utilize in ML models
#' @param GridTune Set to TRUE to grid tune the ML models
#' @param FCPeriods Number of periods to forecast
#' @param MaxNumberModels The number of models to try for each ML model
#' @export
StackedTimeSeriesEnsembleForecast <- function(TS_Models = c("arima","tbats","nnet"),
                                              ML_Methods = c("CatBoost","XGBoost","H2oGBM","H2oDRF"),
                                              CalendarFeatures = TRUE,
                                              HolidayFeatures = TRUE,
                                              FourierFeatures = NULL,
                                              Path = "C:/Users/aantico/Documents/Package",
                                              TargetName = "Weekly_Sales",
                                              DateName = "Date",
                                              NTrees = 750,
                                              TaskType = "GPU",
                                              GridTune = FALSE,
                                              FCPeriods = 5,
                                              MaxNumberModels = 5) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Pull in time series models forecast files----
  i = 1L
  TS_Models <- TS_Models[!TS_Models %chin% "Supercharged-NNET"]
  for(tsf in TS_Models) {
    if(i == 1L) {
      if(file.exists(file.path(file.path(Path,paste0(TargetName,"-",tsf,".csv"))))) {
        data <- data.table::fread(file.path(Path,paste0(TargetName,"-",tsf,".csv")))
        if(tsf %chin% c("XGBoostCARMA","CatBoostCARMA","H2ODRFCARMA","H2OGBMCARMA")) {
          data.table::setnames(data,"Predictions","Forecast")
          data <- data[, .SD, .SDcols = c(eval(DateName),eval(TargetName),"Forecast")]
          if(tsf == "H2OGBMCARMA") {
            data.table::set(data, j = "ModelID", value = "H2O-GBM-CARMA")
          } else if(tsf == "H2ODRFCARMA") {
            data.table::set(data, j = "ModelID", value = "H2O-RandomForest-CARMA")
          } else if(tsf == "CatBoostCARMA") {
            data.table::set(data, j = "ModelID", value = "CatBoost-CARMA")
          } else {
            data.table::set(data, j = "ModelID", value = "XGBoost-CARMA")
          }
        }
        if("Target" %chin% names(data)) data.table::setnames(data, "Target",eval(TargetName))
        if("ModelRank" %chin% names(data)) {
          FourierFeaturesFull <- cbind(data[ModelRank == min(ModelRank),.SD, .SDcols = eval(DateName)],FourierFeatures)
        } else {
          FourierFeaturesFull <- cbind(data[, .SD, .SDcols = eval(DateName)],FourierFeatures)
        }
        i <- i + 1
      }
    } else {
      temp <- data.table::fread(file.path(Path,paste0(TargetName,"-",tsf,".csv")))
      if(tsf %chin% c("XGBoostCARMA","CatBoostCARMA","H2ODRFCARMA","H2OGBMCARMA")) {
        data.table::setnames(temp,"Predictions","Forecast")
        temp <- temp[, .SD, .SDcols = c(eval(DateName),eval(TargetName),"Forecast")]
        if(tsf == "H2OGBMCARMA") {
          data.table::set(temp, j = "ModelID", value = "H2O-GBM-CARMA")
        } else if(tsf == "H2ODRFCARMA") {
          data.table::set(temp, j = "ModelID", value = "H2O-RandomForest-CARMA")
        } else if(tsf == "CatBoostCARMA") {
          data.table::set(temp, j = "ModelID", value = "CatBoost-CARMA")
        } else {
          data.table::set(temp, j = "ModelID", value = "XGBoost-CARMA")
        }
      }
      if("Target" %chin% names(temp)) data.table::setnames(temp, "Target",eval(TargetName))
      data <- data.table::rbindlist(list(data,temp), fill = TRUE, use.names = TRUE)
      i <- i + 1
    }
  }
  
  # Merge Fourier Features----
  data <- merge(data, FourierFeaturesFull, by = eval(DateName), all.x = TRUE)
  
  # Fill in missing ModelRank for CARMA functions----
  if(any(TS_Models %chin% c("arima","tbats"))) {
    data.table::set(data, i = which(is.na(data[["ModelRank"]])), j = "ModelRank", value = 1)  
  } else {
    data.table::set(data, j = "ModelRank", value = 1)
  }
  
  # Add Calendar Variables----
  if(CalendarFeatures) {
    data <- CreateCalendarVariables(
      data = data,
      DateCols = eval(DateName),
      AsFactor = FALSE,
      TimeUnits = c("second","minute","hour","wday","mday","yday","week","isoweek","month","quarter","year"))    
  }
  
  # Add Holiday Counts----
  if(HolidayFeatures) {
    data <- CreateHolidayVariables(
      data,
      DateCols = eval(DateName),
      HolidayGroups = c("USPublicHolidays"),
      Holidays = NULL,
      GroupingVars = "ModelID")    
  }
  
  # Subset and Split out data sets----
  keep <- c(eval(DateName),eval(TargetName),"Forecast","ModelID",names(data)[9:ncol(data)])
  if(any(TS_Models %chin% c("arima","tbats"))) {
    ForecastStartDate <- min(data[is.na(get(TargetName))][[eval(DateName)]])  
  } else {
    startrow <- nrow(data) / length(TS_Models) - FCPeriods + 1L
    ForecastStartDate <- data[startrow, get(DateName)]
  }
  TrainData <- data[get(DateName) < eval(ForecastStartDate)]
  TrainData <- TrainData[, ..keep]
  TrainData <- ModelDataPrep(
    data = TrainData, 
    Impute = FALSE, 
    CharToFactor = TRUE, 
    IntToNumeric = TRUE, 
    RemoveDates = FALSE, 
    MissFactor = "0",
    MissNum = -1, 
    IgnoreCols = NULL)
  if(any(TS_Models %chin% c("arima","tbats"))) {
    keep <- c(eval(DateName),eval(TargetName),"Forecast","ModelID","Low80","Low95","High80","High95",names(data)[9:ncol(data)])  
  } else {
    keep <- c(eval(DateName),eval(TargetName),"Forecast","ModelID",names(data)[9:ncol(data)])
  }
  ForecastData <- data[get(DateName) >= eval(ForecastStartDate)]
  ForecastData <- ForecastData[, ..keep]
  ForecastData <- ModelDataPrep(
    data = ForecastData, 
    Impute = FALSE, 
    CharToFactor = TRUE, 
    IntToNumeric = TRUE,
    RemoveDates = FALSE, 
    MissFactor = "0",
    MissNum = -1, 
    IgnoreCols = NULL)
  FullData <- data.table::rbindlist(list(TrainData,ForecastData), fill = TRUE)
  data.table::setorderv(FullData, cols = c("ModelID","ModelRank","Date"), order = c(1,1,1))
  
  # Difference series data to build models off of----
  TrainData[, ForecastDiff := data.table::shift(x = Forecast, n = 1, fill = NA, type = "lag"), by = c("ModelID","ModelRank")][, ModForecast := Forecast - ForecastDiff]
  TrainData[, TargetDiff := data.table::shift(x = get(TargetName), n = 1, fill = NA, type = "lag"), by = c("ModelID","ModelRank")][, ModTarget := get(TargetName) - TargetDiff]
  FullData[, ForecastDiff := data.table::shift(x = Forecast, n = 1, fill = NA, type = "lag"), by = c("ModelID","ModelRank")][, ModForecast := Forecast - ForecastDiff]
  FullData[, TargetDiff := data.table::shift(x = get(TargetName), n = 1, fill = NA, type = "lag"), by = c("ModelID","ModelRank")][, ModTarget := get(TargetName) - TargetDiff]
  
  # Subset data for modeling----
  TrainDataModel <- TrainData[!is.na(ModTarget)]
  TrainDataStart <- TrainData[is.na(ModTarget)][, eval(DateName) := as.Date(get(DateName))]
  FullDataModel <- FullData[!is.na(ModForecast)]
  
  # Define model args----
  if(any(TS_Models %chin% c("arima","tbats"))) {
    Features <- setdiff(names(TrainData),c(eval(DateName),eval(TargetName),"ModTarget","TargetDiff","ModelRank","Low80","Low95","High80","High95"))
    idcols <- c(eval(DateName),eval(TargetName),"ModTarget","TargetDiff","ModelRank","Low80","Low95","High80","High95")
  } else {
    Features <- setdiff(names(TrainData),c(eval(DateName),eval(TargetName),"ModTarget","TargetDiff","ModelRank"))  
    idcols <- c(eval(DateName),eval(TargetName),"ModTarget","TargetDiff","ModelRank")
  }
  
  # Build ML Models----
  ForecastOutputList <- list()
  Counter <- 0L
  for(models in ML_Methods) {
    if(tolower(models) == "h2odrf") {
      
      # Increment----
      Counter <- Counter + 1L
      
      # Convert date to character----
      data.table::set(
        TrainDataModel, 
        j = eval(DateName), 
        value = as.character(TrainDataModel[[eval(DateName)]]))
      data.table::set(
        FullDataModel, 
        j = eval(DateName), 
        value = as.character(FullDataModel[[eval(DateName)]]))
      
      # Build H2O RandomForest----
      Ensemble <- AutoH2oDRFRegression(
        data = TrainDataModel,
        TrainOnFull = TRUE,
        ValidationData = NULL,
        TestData = NULL,
        TargetColumnName = "ModTarget",
        FeatureColNames = Features,
        TransformNumericColumns = NULL,
        eval_metric = "MSE",
        Trees = NTrees,
        GridTune = FALSE,
        MaxMem = "28G",
        NThreads = max(1, parallel::detectCores()),
        MaxModelsInGrid = 10,
        model_path = NULL,
        metadata_path = NULL,
        ModelID = "FC_Model",
        NumOfParDepPlots = 0,
        ReturnModelObjects = TRUE,
        SaveModelObjects = FALSE,
        IfSaveModel = "mojo",
        H2OShutdown = FALSE)
      
      # Store Model----
      Model <- Ensemble$Model
      
      # Score H2O RandomForest Model----
      Forecasts <- AutoH2OMLScoring(
        ScoringData = FullDataModel,
        ModelObject = Model,
        ModelType = "mojo",
        H2OShutdown = TRUE,
        MaxMem = "28G",
        NThreads = max(1, parallel::detectCores()-2),
        JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
        ModelPath = NULL,
        ModelID = "FC_Model",
        ReturnFeatures = TRUE,
        TransformNumeric = FALSE,
        BackTransNumeric = FALSE,
        TargetColumnName = NULL,
        TransformationObject = NULL,
        TransID = NULL,
        TransPath = NULL,
        MDP_Impute = FALSE,
        MDP_CharToFactor = TRUE,
        MDP_RemoveDates = FALSE,
        MDP_MissFactor = "0",
        MDP_MissNum = -1)
      
      # Recombine data----
      Forecasts[, Date := as.Date(Date)]
      Forecasts <- data.table::rbindlist(list(Forecasts,TrainDataStart),fill = TRUE)
      data.table::setorderv(Forecasts, cols = c("ModelID","ModelRank",eval(DateName)), order = c(1,1,1))
      
      # Fill in NA's----
      data.table::set(Forecasts, i = which(is.na(Forecasts[["Predictions"]])), j = "Predictions", value = Forecasts[which(is.na(Forecasts[["Predictions"]]))][["Forecast"]])
      
      # Overwrite Predictions and Forecast with Actuals----
      Forecasts[, ID := seq_len(.N), by = c("ModelID","ModelRank")]
      Forecasts[ID %in% c(1:length(TrainData[, unique(get(DateName))])), Predictions := get(TargetName)]
      Forecasts[ID %in% c(length(TrainData[, unique(get(DateName))]):length(FullData[, unique(get(DateName))])), Predictions := cumsum(Predictions), by = c("ModelID","ModelRank")][, ID := NULL]
      
      # Store forecast data----
      ForecastOutputList[[Counter]] <- data.table::copy(Forecasts)
      
    } else if(tolower(models) == "h2ogbm") {
      
      # Increment----
      Counter <- Counter + 1L
      
      # Convert date to character----
      data.table::set(
        TrainDataModel, 
        j = eval(DateName), 
        value = as.character(TrainDataModel[[eval(DateName)]]))
      data.table::set(
        FullDataModel, 
        j = eval(DateName), 
        value = as.character(FullDataModel[[eval(DateName)]]))
      
      # Build H2O GBM----
      Ensemble <- AutoH2oGBMRegression(
        data = TrainDataModel,
        TrainOnFull = TRUE,
        ValidationData = NULL,
        TestData = NULL,
        TargetColumnName = "ModTarget",
        FeatureColNames = Features,
        TransformNumericColumns = NULL,
        eval_metric = "MSE",
        Trees = NTrees,
        GridTune = FALSE,
        MaxMem = "28G",
        NThreads = max(1, parallel::detectCores()),
        MaxModelsInGrid = 10,
        model_path = NULL,
        metadata_path = NULL,
        ModelID = "FC_Model",
        NumOfParDepPlots = 0,
        ReturnModelObjects = TRUE,
        SaveModelObjects = FALSE,
        IfSaveModel = "mojo",
        H2OShutdown = FALSE)
      
      # Store Model----
      Model <- Ensemble$Model
      
      # Score H2O GBM Model----
      Forecasts <- AutoH2OMLScoring(
        ScoringData = FullDataModel,
        ModelObject = Model,
        ModelType = "mojo",
        H2OShutdown = TRUE,
        MaxMem = "28G",
        NThreads = max(1, parallel::detectCores()-2),
        JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
        ModelPath = NULL,
        ModelID = "FC_Model",
        ReturnFeatures = TRUE,
        TransformNumeric = FALSE,
        BackTransNumeric = FALSE,
        TargetColumnName = NULL,
        TransformationObject = NULL,
        TransID = NULL,
        TransPath = NULL,
        MDP_Impute = FALSE,
        MDP_CharToFactor = TRUE,
        MDP_RemoveDates = FALSE,
        MDP_MissFactor = "0",
        MDP_MissNum = -1)
      
      # Recombine data----
      Forecasts[, Date := as.Date(Date)]
      Forecasts <- data.table::rbindlist(list(Forecasts,TrainDataStart),fill = TRUE)
      data.table::setorderv(Forecasts, cols = c("ModelID","ModelRank",eval(DateName)), order = c(1,1,1))
      
      # Fill in NA's----
      data.table::set(Forecasts, i = which(is.na(Forecasts[["Predictions"]])), j = "Predictions", value = Forecasts[which(is.na(Forecasts[["Predictions"]]))][["Forecast"]])
      
      # Overwrite Predictions and Forecast with Actuals----
      Forecasts[, ID := seq_len(.N), by = c("ModelID","ModelRank")]
      Forecasts[ID %in% c(1:length(TrainData[, unique(get(DateName))])), Predictions := get(TargetName)]
      Forecasts[ID %in% c(length(TrainData[, unique(get(DateName))]):length(FullData[, unique(get(DateName))])), Predictions := cumsum(Predictions), by = c("ModelID","ModelRank")][, ID := NULL]
      
      # Store forecast data----
      ForecastOutputList[[Counter]] <- data.table::copy(Forecasts)
      
    } else if(tolower(models) == "xgboost") {
      
      # Increment----
      Counter <- Counter + 1L
      
      # Convert date to character----
      data.table::set(
        TrainDataModel, 
        j = eval(DateName), 
        value = as.POSIXct(TrainDataModel[[eval(DateName)]]))
      data.table::set(
        FullDataModel, 
        j = eval(DateName), 
        value = as.POSIXct(FullDataModel[[eval(DateName)]]))
      
      # Build XGBoost----
      Ensemble <- AutoXGBoostRegression(
        data = TrainDataModel,
        TrainOnFull = TRUE,
        ValidationData = NULL,
        TestData = NULL,
        TargetColumnName = "ModTarget",
        FeatureColNames = Features,
        IDcols = idcols,
        ReturnFactorLevels = TRUE,
        TreeMethod = "hist",
        TransformNumericColumns = NULL,
        eval_metric = "RMSE",
        Trees = NTrees,
        GridTune = FALSE,
        NThreads = parallel::detectCores(),
        MaxModelsInGrid = 10,
        model_path = NULL,
        metadata_path = NULL,
        ModelID = "FC_Model",
        NumOfParDepPlots = 0,
        ReturnModelObjects = TRUE,
        SaveModelObjects = FALSE)
      
      # Store Model----
      Model <- Ensemble$Model
      FactorLevelsList <- Ensemble$FactorLevelsList
      
      # Score XGBoost Model----
      Forecasts <- AutoXGBoostScoring(
        ScoringData = FullDataModel,
        ModelObject = Model,
        TargetType = "regression", 
        FeatureColumnNames = Features,
        IDcols = idcols, 
        FactorLevelsList = FactorLevelsList,
        TargetLevels = NULL, 
        ModelPath = NULL,
        ModelID = "FC_Model",
        ReturnFeatures = TRUE,
        TransformNumeric = FALSE,
        BackTransNumeric = FALSE,
        TargetColumnName = NULL,
        TransformationObject = NULL,
        TransID = NULL,
        TransPath = NULL,
        MDP_Impute = FALSE,
        MDP_CharToFactor = TRUE,
        MDP_RemoveDates = FALSE,
        MDP_MissFactor = "0",
        MDP_MissNum = -1)
      
      # Recombine data----
      Forecasts[, Date := as.Date(Date)]
      Forecasts <- data.table::rbindlist(list(Forecasts,TrainDataStart),fill = TRUE)
      data.table::setorderv(Forecasts, cols = c("ModelID","ModelRank",eval(DateName)), order = c(1,1,1))
      
      # Fill in NA's----
      data.table::set(Forecasts, i = which(is.na(Forecasts[["Predictions"]])), j = "Predictions", value = Forecasts[which(is.na(Forecasts[["Predictions"]]))][["Forecast"]])
      
      # Overwrite Predictions and Forecast with Actuals----
      Forecasts[, ID := seq_len(.N), by = c("ModelID","ModelRank")]
      Forecasts[ID %in% c(1:length(TrainData[, unique(get(DateName))])), Predictions := get(TargetName)]
      Forecasts[ID %in% c(length(TrainData[, unique(get(DateName))]):length(FullData[, unique(get(DateName))])), Predictions := cumsum(Predictions), by = c("ModelID","ModelRank")][, ID := NULL]
      
      # Store forecast data----
      ForecastOutputList[[Counter]] <- data.table::copy(Forecasts)
      
    } else if(tolower(models) == "catboost") {
      
      # Increment----
      Counter <- Counter + 1L
      
      # Convert date to character----
      data.table::set(
        TrainDataModel, 
        j = eval(DateName), 
        value = as.character(TrainDataModel[[eval(DateName)]]))
      data.table::set(
        FullDataModel, 
        j = eval(DateName), 
        value = as.character(FullDataModel[[eval(DateName)]]))
      
      # Build CatBoost----
      Ensemble <- AutoCatBoostRegression(
        data = TrainDataModel,
        TrainOnFull = TRUE,
        ValidationData = NULL,
        TestData = NULL,
        TargetColumnName = "ModTarget",
        FeatureColNames = Features,
        PrimaryDateColumn = eval(DateName),
        IDcols = idcols,
        task_type = TaskType,
        TransformNumericColumns = NULL,
        eval_metric = "RMSE",
        Trees = NTrees,
        GridTune = FALSE,
        MaxModelsInGrid = 10,
        model_path = NULL,
        metadata_path = NULL,
        ModelID = "FC_Model",
        NumOfParDepPlots = 0,
        ReturnModelObjects = TRUE,
        SaveModelObjects = FALSE)
      
      # Store Model----
      Model <- Ensemble$Model
      
      # Score CatBoost Model----
      Forecasts <- AutoCatBoostScoring(
        ScoringData = FullDataModel,
        ModelObject = Model,
        TargetType = "regression", 
        FeatureColumnNames = Features,
        IDcols = idcols,
        RemoveModel = TRUE,
        ModelPath = NULL,
        ModelID = "FC_Model",
        ReturnFeatures = TRUE,
        TransformNumeric = FALSE,
        BackTransNumeric = FALSE,
        TargetColumnName = NULL,
        TransformationObject = NULL,
        TransID = NULL,
        TransPath = NULL,
        MDP_Impute = FALSE,
        MDP_CharToFactor = TRUE,
        MDP_RemoveDates = FALSE,
        MDP_MissFactor = "0",
        MDP_MissNum = -1)
      
      # Recombine data----
      Forecasts[, Date := as.Date(Date)]
      Forecasts <- data.table::rbindlist(list(Forecasts,TrainDataStart),fill = TRUE)
      data.table::setorderv(Forecasts, cols = c("ModelID","ModelRank",eval(DateName)), order = c(1,1,1))
      
      # Fill in NA's----
      data.table::set(Forecasts, i = which(is.na(Forecasts[["Predictions"]])), j = "Predictions", value = Forecasts[which(is.na(Forecasts[["Predictions"]]))][["Forecast"]])
      
      # Overwrite Predictions and Forecast with Actuals----
      Forecasts[, ID := seq_len(.N), by = c("ModelID","ModelRank")]
      Forecasts[ID %in% c(1:length(TrainData[, unique(get(DateName))])), Predictions := get(TargetName)]
      Forecasts[ID %in% c(length(TrainData[, unique(get(DateName))]):length(FullData[, unique(get(DateName))])), Predictions := cumsum(Predictions), by = c("ModelID","ModelRank")][, ID := NULL]
      
      # Store forecast data----
      ForecastOutputList[[Counter]] <- data.table::copy(Forecasts)
    }
  }
  
  # Rbind And Aggregate Data----
  FinalForecast <- data.table::rbindlist(ForecastOutputList, fill = TRUE)
  if(any(TS_Models %chin% c("arima","tbats"))) {
    FinalForecast <- FinalForecast[, .(V1 = mean(get(TargetName), na.rm = TRUE),
                                       Ensemble = mean(Predictions, na.rm = TRUE),
                                       Forecast = mean(Forecast, na.rm = TRUE),
                                       Low80 = mean(Low80, na.rm = TRUE),
                                       Low95 = mean(Low95, na.rm = TRUE),
                                       High80 = mean(High80, na.rm = TRUE),
                                       High95 = mean(High95, na.rm = TRUE)), by = eval(DateName)]  
  } else {
    FinalForecast <- FinalForecast[, .(V1 = mean(get(TargetName), na.rm = TRUE),
                                       Ensemble = mean(Predictions, na.rm = TRUE),
                                       Forecast = mean(Forecast, na.rm = TRUE)), by = eval(DateName)]
  }
  data.table::setnames(FinalForecast, "V1",eval(TargetName))
  data.table::set(
    FinalForecast, 
    i = (length(TrainData[, unique(get(DateName))])+1L):(length(FullDataModel[,unique(get(DateName))])+1L), 
    j = eval(TargetName), 
    value = NA)
  data.table::set(
    FinalForecast, 
    i = (length(TrainData[, unique(get(DateName))])+1L):(length(FullDataModel[,unique(get(DateName))])+1L), 
    j = eval(TargetName), 
    value = NA)
  
  # Return Forecast----
  return(FinalForecast)
}

#' WideTimeSeriesEnsembleForecast
#' 
#' WideTimeSeriesEnsembleForecast to generate forecasts and ensemble data
#' 
#' @author Adrian Antico
#' @family Time Series
#' @param TS_Models Select which ts model forecasts to ensemble
#' @param ML_Methods Select which models to build for the ensemble
#' @param Path The path to the folder where the ts forecasts are stored
#' @param TargetName "Weekly_Sales"
#' @param DateName "Date"
#' @param NTrees Select the number of trees to utilize in ML models
#' @param GridTune Set to TRUE to grid tune the ML models
#' @param MaxNumberModels The number of models to try for each ML model
#' @export
WideTimeSeriesEnsembleForecast <- function(TS_Models = c("arima","tbats","nnet"),
                                           ML_Methods = c("CatBoost","XGBoost","H2oGBM","H2oDRF"),
                                           Path = "C:/Users/aantico/Documents/Package",
                                           TargetName = "Weekly_Sales",
                                           DateName = "Date",
                                           NTrees = 750,
                                           TaskType = "GPU",
                                           GridTune = FALSE,
                                           MaxNumberModels = 5) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores()-2))
  
  # Pull in time series models forecast files----
  i = 1L
  for(tsf in c(TS_Models)) {
    if(i == 1) {
      if(file.exists(file.path(file.path(Path,paste0(tsf,".csv"))))) {
        data <- data.table::fread(file.path(Path,paste0(tsf,".csv")))        
        i <- i + 1
      }
    } else {
      temp <- data.table::fread(file.path(Path,paste0(tsf,".csv")))
      data <- data.table::rbindlist(list(data,temp))
      i <- i + 1
    }
  }
  
  # Subset and Split out data sets----
  keep <- c(eval(DateName),"Target","Forecast","ModelID")
  TrainData <- data[!is.na(Target), ..keep]
  keep <- c(eval(DateName),"Target","Forecast","ModelID","Low80","Low95","High80","High95")
  ForecastData <- data[is.na(Target), ..keep]
  data.table::setnames(TrainData, "Target",eval(TargetName))
  data.table::setnames(ForecastData, "Target",eval(TargetName))
  TrainDataWide <- data.table::dcast(data = TrainData, Date + Weekly_Sales ~ ModelID, value.var = "Forecast", fun = mean)
  ForecastDataWide <<- data.table::dcast(data = ForecastData, Date ~ ModelID, value.var = "Forecast", fun = mean)
  TrainDataWide <- ModelDataPrep(
    data = TrainDataWide, 
    Impute = FALSE, 
    CharToFactor = TRUE, 
    IntToNumeric = TRUE, 
    RemoveDates = FALSE, 
    MissFactor = "0",
    MissNum = -1, 
    IgnoreCols = NULL)
  ForecastDataWide <- ModelDataPrep(
    data = ForecastDataWide, 
    Impute = FALSE, 
    CharToFactor = TRUE, 
    IntToNumeric = TRUE, 
    RemoveDates = FALSE, 
    MissFactor = "0",
    MissNum = -1, 
    IgnoreCols = NULL)
  
  # Training Diff----
  DiffTrainOutput <- DifferenceData(data = TrainDataWide)
  Train <- DiffTrainOutput$DiffData
  
  # Scoring Diff----
  DiffScoreOutput <- DifferenceData(data = ForecastDataWide)
  Score <- DiffScoreOutput$DiffData
  
  # Build ML Models----
  ForecastOutputList <- list()
  Counter <- 0L
  for(models in ML_Methods) {
    if(tolower(models) == "h2odrf") {
      
      # Increment----
      Counter <- Counter + 1L
      
      # Build H2O RandomForest----
      Ensemble <- AutoH2oDRFRegression(
        data = Train,
        ValidationData = NULL,
        TestData = NULL,
        TargetColumnName = TargetName,
        FeatureColNames = 3:ncol(Train),
        TransformNumericColumns = NULL,
        eval_metric = "MSE",
        Trees = NTrees,
        GridTune = FALSE,
        MaxMem = "28G",
        NThreads = max(1, parallel::detectCores()),
        MaxModelsInGrid = 10,
        model_path = NULL,
        metadata_path = NULL,
        ModelID = "FC_Model",
        NumOfParDepPlots = 0,
        ReturnModelObjects = TRUE,
        SaveModelObjects = FALSE,
        IfSaveModel = "mojo",
        H2OShutdown = FALSE)
      
      # Store Model----
      Model <- Ensemble$Model
      
      # Score H2O RandomForest Model----
      Forecasts <- AutoH2OMLScoring(
        ScoringData = Score,
        ModelObject = Model,
        ModelType = "mojo",
        H2OShutdown = TRUE,
        MaxMem = "28G",
        NThreads = max(1, parallel::detectCores()-2),
        JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
        ModelPath = NULL,
        ModelID = "FC_Model",
        ReturnFeatures = TRUE,
        TransformNumeric = FALSE,
        BackTransNumeric = FALSE,
        TargetColumnName = NULL,
        TransformationObject = NULL,
        TransID = NULL,
        TransPath = NULL,
        MDP_Impute = FALSE,
        MDP_CharToFactor = TRUE,
        MDP_RemoveDates = FALSE,
        MDP_MissFactor = "0",
        MDP_MissNum = -1)
      
      # Back Transform Differencing----
      Forecasts <- DifferenceDataReverse(
        data = ForecastDataWide, 
        ScoreData = Forecasts$Predictions, 
        LastRow = DiffTrainOutput$LastRow[["TargetName"]])
      
      # StackData
      PlotForecast <- data.table::melt(
        data = Forecasts, 
        id.vars = eval(DateName),
        variable.name = "Model",
        value.name = "Forecast")
      PlotForecast[Model == "Forecast", Model := "H2oDRF"]
      
      # Store forecast data----
      ForecastOutputList[[Counter]] <- PlotForecast
      
    } else if(tolower(models) == "h2ogbm") {
      
      # Increment----
      Counter <- Counter + 1L
      
      # Build H2O GBM----
      Ensemble <- AutoH2oGBMRegression(
        data = Train,
        ValidationData = NULL,
        TestData = NULL,
        TargetColumnName = TargetName,
        FeatureColNames = 3:4,
        TransformNumericColumns = NULL,
        eval_metric = "MSE",
        Trees = NTrees,
        GridTune = FALSE,
        MaxMem = "28G",
        NThreads = max(1, parallel::detectCores()),
        MaxModelsInGrid = 10,
        model_path = NULL,
        metadata_path = NULL,
        ModelID = "FC_Model",
        NumOfParDepPlots = 0,
        ReturnModelObjects = TRUE,
        SaveModelObjects = FALSE,
        IfSaveModel = "mojo",
        H2OShutdown = FALSE)
      
      # Store Model----
      Model <- Ensemble$Model
      
      # Score H2O GBM Model----
      Forecasts <- AutoH2OMLScoring(
        ScoringData = Score,
        ModelObject = Model,
        ModelType = "mojo",
        H2OShutdown = TRUE,
        MaxMem = "28G",
        NThreads = max(1, parallel::detectCores()-2),
        JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
        ModelPath = NULL,
        ModelID = "FC_Model",
        ReturnFeatures = TRUE,
        TransformNumeric = FALSE,
        BackTransNumeric = FALSE,
        TargetColumnName = NULL,
        TransformationObject = NULL,
        TransID = NULL,
        TransPath = NULL,
        MDP_Impute = FALSE,
        MDP_CharToFactor = TRUE,
        MDP_RemoveDates = FALSE,
        MDP_MissFactor = "0",
        MDP_MissNum = -1)
      
      # Back Transform Differencing----
      Forecasts <- DifferenceDataReverse(
        data = ForecastDataWide, 
        ScoreData = Forecasts$Predictions, 
        LastRow = DiffTrainOutput$LastRow[["TargetName"]])
      
      # StackData
      PlotForecast <- data.table::melt(
        data = Forecasts, 
        id.vars = eval(DateName),
        variable.name = "Model",
        value.name = "Forecast")
      PlotForecast[Model == "Forecast", Model := "H2oGBM"]
      
      # Store forecast data----
      ForecastOutputList[[Counter]] <- PlotForecast
      
    } else if(tolower(models) == "xgboost") {
      
      # Increment----
      Counter <- Counter + 1L
      
      # Build XGBoost----
      Ensemble <- AutoXGBoostRegression(
        data = Train,
        ValidationData = NULL,
        TestData = NULL,
        TargetColumnName = TargetName,
        FeatureColNames = 3:ncol(Train),
        IDcols = eval(DateName),
        ReturnFactorLevels = TRUE,
        TreeMethod = "hist",
        TransformNumericColumns = NULL,
        eval_metric = "RMSE",
        Trees = NTrees,
        GridTune = FALSE,
        NThreads = parallel::detectCores(),
        MaxModelsInGrid = 10,
        model_path = NULL,
        metadata_path = NULL,
        ModelID = "FC_Model",
        NumOfParDepPlots = 0,
        ReturnModelObjects = TRUE,
        SaveModelObjects = FALSE)
      
      # Store Model----
      Model <- Ensemble$Model
      FactorLevelsList <- Ensemble$FactorLevelsList
      
      # Score XGBoost Model----
      Forecasts <- AutoXGBoostScoring(
        ScoringData = Score,
        ModelObject = Model,
        TargetType = "regression", 
        FeatureColumnNames = 2:ncol(Score), 
        IDcols = eval(DateName), 
        FactorLevelsList = FactorLevelsList,
        TargetLevels = NULL, 
        ModelPath = NULL,
        ModelID = "FC_Model",
        ReturnFeatures = TRUE,
        TransformNumeric = FALSE,
        BackTransNumeric = FALSE,
        TargetColumnName = NULL,
        TransformationObject = NULL,
        TransID = NULL,
        TransPath = NULL,
        MDP_Impute = FALSE,
        MDP_CharToFactor = TRUE,
        MDP_RemoveDates = FALSE,
        MDP_MissFactor = "0",
        MDP_MissNum = -1)
      
      # Back Transform Differencing----
      Forecasts <- DifferenceDataReverse(
        data = ForecastDataWide, 
        ScoreData = Forecasts$Predictions, 
        LastRow = DiffTrainOutput$LastRow[["TargetName"]])
      
      # StackData
      PlotForecast <- data.table::melt(
        data = Forecasts, 
        id.vars = eval(DateName),
        variable.name = "Model",
        value.name = "Forecast")
      PlotForecast[Model == "Forecast", Model := "XGBoost"]
      
      # Store forecast data----
      ForecastOutputList[[Counter]] <- PlotForecast
      
    } else if(tolower(models) == "catboost") {
      
      # Increment----
      Counter <- Counter + 1L
      
      # Build CatBoost----
      Ensemble <- AutoCatBoostRegression(
        data = Train,
        ValidationData = NULL,
        TestData = NULL,
        TargetColumnName = TargetName,
        FeatureColNames = 3:ncol(Score),
        PrimaryDateColumn = eval(DateName),
        IDcols = eval(DateName),
        task_type = TaskType,
        TransformNumericColumns = NULL,
        eval_metric = "RMSE",
        Trees = NTrees,
        GridTune = FALSE,
        MaxModelsInGrid = 10,
        model_path = NULL,
        metadata_path = NULL,
        ModelID = "FC_Model",
        NumOfParDepPlots = 0,
        ReturnModelObjects = TRUE,
        SaveModelObjects = FALSE)
      
      # Store Model----
      Model <- Ensemble$Model
      
      # Score CatBoost Model----
      Forecasts <- AutoCatBoostScoring(
        ScoringData = Score,
        ModelObject = Model,
        TargetType = "regression", 
        FeatureColumnNames = 2:ncol(Score), 
        IDcols = eval(DateName),
        RemoveModel = TRUE,
        ModelPath = NULL,
        ModelID = "FC_Model",
        ReturnFeatures = TRUE,
        TransformNumeric = FALSE,
        BackTransNumeric = FALSE,
        TargetColumnName = NULL,
        TransformationObject = NULL,
        TransID = NULL,
        TransPath = NULL,
        MDP_Impute = FALSE,
        MDP_CharToFactor = TRUE,
        MDP_RemoveDates = FALSE,
        MDP_MissFactor = "0",
        MDP_MissNum = -1)
      
      # Back Transform Differencing----
      Forecasts <- DifferenceDataReverse(
        data = ForecastDataWide, 
        ScoreData = Forecasts$Predictions, 
        LastRow = DiffTrainOutput$LastRow[["TargetName"]])
      
      # StackData
      PlotForecast <- data.table::melt(
        data = Forecasts, 
        id.vars = eval(DateName),
        variable.name = "Model",
        value.name = "Forecast")
      PlotForecast[Model == "Forecast", Model := "CatBoost"]
      
      # Store forecast data----
      ForecastOutputList[[Counter]] <- PlotForecast
    }
  }
  
  # Rbind And Aggregate Data----
  data.table::set(ForecastData, j = c("Forecast", eval(TargetName)), value = NULL)
  ForecastDataIntervals <- ForecastData[, .(Low80 = mean(Low80, na.rm = TRUE), Low95 = mean(Low95, na.rm = TRUE), 
                                            High80 = mean(High80, na.rm = TRUE), High95 = mean(High95, na.rm = TRUE)), by = eval(DateName)]
  FinalForecast <- data.table::rbindlist(ForecastOutputList)
  FinalForecast <- FinalForecast[Model %in% ML_Methods]
  FinalForecast <- FinalForecast[, .(Forecast = mean(Forecast)), by = eval(DateName)]
  FinalForecast <- merge(x = FinalForecast, y = ForecastDataIntervals, by = c(eval(DateName)), all = FALSE)
  
  # Rbind data----
  TrainData <- TrainData[, .(Target = max(get(TargetName))), by = eval(DateName)]
  data.table::setnames(TrainData, "Target", eval(TargetName))
  ReturnData <- unique(data.table::rbindlist(list(TrainData,FinalForecast), fill = TRUE))
  ReturnData[, eval(TargetName) := as.numeric(get(TargetName))]
  ReturnData[, Forecast := as.numeric(Forecast)]
  ReturnData[, eval(DateName) := as.Date(get(DateName))]
  
  # Return Forecast----
  return(ReturnData)
}

#' AutoFourierFeatures
#' 
#' #' AutoFourierFeatures
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data The source data
#' @param FourierPairs A number indicating the max number of fourier pairs that will be built
#' @param TargetColumn The name of your target column
#' @param DateColumn The name of your date column
#' @param GroupVariable The name of your group variable
#' @param xregs Extra data to merge in
#' @export
AutoFourierFeatures <- function(data,
                                FourierPairs = NULL,
                                FCPeriods = NULL,
                                Time_Unit = NULL,
                                TargetColumn = NULL,
                                DateColumn = NULL,
                                GroupVariable = NULL,
                                xregs = NonGroupDateNames) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
  
  # Build features----
  if(!is.null(GroupVariable)) {
    
    # Grouping Variable Case----
    packages <- c("RemixAutoML","data.table","forecast","lubridate","stats")
    cores <- parallel::detectCores()
    cl <- parallel::makePSOCKcluster(cores)
    doParallel::registerDoParallel(cl)
    Results <- tryCatch({foreach::foreach(
      i = unique(data[, GroupVar]),
      .combine = function(x,...) mapply(function(...) data.table::rbindlist(list(...), fill = TRUE), x, ..., SIMPLIFY = FALSE),
      .multicombine = TRUE,
      .packages = packages) %dopar% {
        
        # Step0: Setup----
        tempdatamerge <- data[get(GroupVariable) == eval(i), .SD, .SDcols = c(eval(GroupVariable), eval(DateColumn), eval(xregs))]
        tempdata <- data[get(GroupVariable) == eval(i), .SD, .SDcols = eval(TargetColumn)]
        minDate <- min(data[get(GroupVariable) == eval(i), get(DateColumn)])
        maxDate <- max(data[get(GroupVariable) == eval(i), get(DateColumn)])
        
        # Step1: Find frequency----
        ModelFreqFrequency <- forecast::findfrequency(as.matrix(tempdata))
        
        # Step2: Model based frequency----
        ModelFreqData <- stats::ts(
          data = tempdata,
          start = minDate,
          frequency = ModelFreqFrequency)[, TargetColumn]
        
        # Step3: Clean data----
        TSCleanModelFreqData <- forecast::tsclean(
          x = ModelFreqData,
          replace.missing = TRUE,
          lambda = "auto")
        
        # Step4: Find kmax----
        kmax <- min(FourierPairs, floor(ModelFreqFrequency/2))
        
        # Step5: Get training values
        HistoricalFourier <- tryCatch({data.table::as.data.table(forecast::fourier(
          TSCleanModelFreqData,
          K = kmax))},
          error = function(x) NULL)
        
        # Step6: Get scoring values
        FutureFourier <- tryCatch({data.table::as.data.table(forecast::fourier(
          TSCleanModelFreqData,
          K = kmax,
          h = (FCPeriods-1)))},
          error = function(x) NULL)
        
        # Step7: Create future date records FutureFourier----
        for(time in seq_len(FCPeriods - 1)) {
          d <- maxDate
          if (tolower(Time_Unit) %chin% c("hour","hours")) {
            temp <- data.table::as.data.table(d + lubridate::hours(1*time))
          } else if(tolower(Time_Unit) == "1min") {
            temp <- data.table::as.data.table(d + lubridate::minutes(1*time))
          } else if(tolower(Time_Unit) == "5min") {
            temp <- data.table::as.data.table(d + lubridate::minutes(5*time))
          } else if(tolower(Time_Unit) == "10min") {
            temp <- data.table::as.data.table(d + lubridate::minutes(10*time))
          } else if(tolower(Time_Unit) == "15min") {
            temp <- data.table::as.data.table(d + lubridate::minutes(15*time))
          } else if(tolower(Time_Unit) == "30min") {
            temp <- data.table::as.data.table(d + lubridate::minutes(30*time))
          } else if(tolower(Time_Unit) %chin% c("day","days")) {
            temp <- data.table::as.data.table(d + lubridate::days(1*time))
          } else if(tolower(Time_Unit) %chin% c("week","weeks")) {
            temp <- data.table::as.data.table(d + lubridate::weeks(1*time))
          } else if(tolower(Time_Unit) %chin% c("month","months")) {
            temp <- data.table::as.data.table(d %m+% months(1*time))
          } else if(tolower(Time_Unit) %chin% c("quarter","quarters")) {
            temp <- data.table::as.data.table(d %m+% months(3*time))
          } else if(tolower(Time_Unit) %chin% c("year","years")) {
            temp <- data.table::as.data.table(d + lubridate::years(1*time))
          }
          data.table::setnames(temp, "V1", eval(DateColumn))
          
          # Rbind----
          if(time == 1) {
            tempdate <- temp
          } else {
            tempdate <- data.table::rbindlist(list(tempdate,temp))
          }
        }
        FutureFourier <- cbind(GroupVar = i, tempdate, FutureFourier)
        
        # Step8: Create future date records HistoricalFourier----
        HistoricalFourier <- cbind(data[get(GroupVariable) == eval(i), .SD, .SDcols = c(eval(GroupVariable),eval(DateColumn))], HistoricalFourier)
        
        # Step9: Rename columns----
        for(cols in seq_len(ncol(HistoricalFourier)-2)) {
          data.table::setnames(HistoricalFourier, old = names(HistoricalFourier)[cols+2], paste0("Fourier_",cols))
          data.table::setnames(FutureFourier, old = names(FutureFourier)[cols+2], paste0("Fourier_",cols))
        }
        
        # Step10: Merge Training Fouier Terms----
        list(HistoricalData = tempdatamerge,
             HistoricalFourier = HistoricalFourier,
             FutureFourier = FutureFourier)
      }}, error = function(x) {
        parallel::stopCluster(cl)
      })
    
    # shut down parallel objects----
    tryCatch({parallel::stopCluster(cl)}, error = function(x) "Parallel is already shut down")
    
    # Return Features
    return(Results)
    
  } else {
    
    # No Group Variables----
    tempdata <- data.table::copy(data)
    minDate <- min(data[[eval(DateColumn)]])
    tempdata <- tempdata[, .SD, .SDcols = eval(TargetColumn)]
    
    # Find frequency----
    ModelFreqFrequency <- forecast::findfrequency(as.matrix(tempdata))
    
    # Model based frequency----
    ModelFreqData <- stats::ts(
      data = tempdata,
      start = minDate,
      frequency = ModelFreqFrequency)[, TargetColumn]
    
    # Clean data----
    TSCleanModelFreqData <- forecast::tsclean(
      x = ModelFreqData,
      replace.missing = TRUE,
      lambda = "auto")
    
    # Find kmax----
    kmax <- min(FourierPairs, floor(ModelFreqFrequency/2))
    
    # Training values
    HistoricalFourier <- tryCatch({data.table::as.data.table(forecast::fourier(
      TSCleanModelFreqData,
      K = kmax))},
      error = function(x) FALSE)
    
    # Scoring values
    FutureFourier <- tryCatch({data.table::as.data.table(forecast::fourier(
      TSCleanModelFreqData,
      K = kmax,
      h = (FCPeriods-1)))},
      error = function(x) FALSE)
    
    # Merge Training Fouier Terms----
    data <- cbind(data, HistoricalFourier)
    return(list(HistoricalData = data,
                FutureFourier = FutureFourier,
                HistoricalFourier = HistoricalFourier))
  }
}

#' AutoHierarchicalFourier
#' 
#' AutoHierarchicalFourier reverses the difference
#' @family Data Wrangling
#' @author Adrian Antico
#' @param datax data
#' @param xRegs The XREGS
#' @param FourierTermS Number of fourier pairs
#' @param TimeUniT Time unit
#' @param FC_PeriodS Number of forecast periods
#' @param TargetColumN Target column name
#' @param DateColumN Date column name
#' @param HierarchGroups Character vector of categorical columns to fully interact
#' @param IndependentGroups Character vector of categorical columns to run independently
#' @export
AutoHierarchicalFourier <- function(datax = data,
                                    xRegs = names(XREGS),
                                    FourierTermS = FourierTerms,
                                    TimeUniT = TimeUnit,
                                    FC_PeriodS = FC_Periods,
                                    TargetColumN = TargetColumn,
                                    DateColumN = DateColumnName,
                                    HierarchGroups = NULL,
                                    IndependentGroups = NULL) {
  
  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))
  
  # Stack Fouier Forecast Data----
  overlap <- intersect(names(FourierFC),names(datax))
  FourierFC <- data.table::rbindlist(list(
    FourierFC[, .SD, .SDcols = c(overlap)],
    datax[, .SD, .SDcols = c(overlap)]))
  data.table::setorderv(FourierFC, cols = names(FourierFC)[c(ncol(FourierFC),1)], order = c(1L, 1L))
  
  # Data Wrangling: Standardize column ordering----
  data.table::setkey(x = datax,GroupVar)
  data.table::setkey(x = FourierFC,GroupVar)
  data.table::setcolorder(datax, c(ncol(datax), 1:(ncol(datax)-1)))
  data.table::setcolorder(FourierFC, c(ncol(FourierFC), 1:(ncol(FourierFC)-1)))
  
  # Return data
  return(list(data = datax, FourierFC = FourierFC))
}
