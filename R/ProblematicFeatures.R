#' ProblematicFeatures identifies problematic features for machine learning
#'
#' ProblematicFeatures identifies problematic features for machine learning and outputs a data.table of the feature names in the first column and the metrics they failed to pass in the columns.
#'
#' @author Adrian Antico
#' @family EDA
#' @param data The data.table with the columns you wish to have analyzed
#' @param ColumnNumbers A vector with the column numbers you wish to analyze
#' @param NearZeroVarThresh Set to NULL to not run NearZeroVar(). Checks to see if the percentage of values in your numeric columns that are not constant are greater than the value you set here. If not, the feature is collects and returned with the percentage unique value.
#' @param CharUniqThresh Set to NULL to not run CharUniqthresh(). Checks to see if the percentage of unique levels / groups in your categorical feature is greater than the value you supply. If it is, the feature name is returned with the percentage unique value.
#' @param NA_Rate Set to NULL to not run NA_Rate(). Checks to see if the percentage of NA's in your features is greater than the value you supply. If it is, the feature name is returned with the percentage of NA values.
#' @param Zero_Rate Set to NULL to not run Zero_Rate(). Checks to see if the percentage of zero's in your features is greater than the value you supply. If it is, the feature name is returned with the percentage of zero values.
#' @param HighSkewThresh Set to NULL to not run HighSkew(). Checks for numeric columns whose ratio of the sum of the top 5th percentile of values to the bottom 95th percentile of values is greater than the value you supply. If true, the column name and value is returned.
#' @examples
#' test <- data.table::data.table(RandomNum = runif(1000))
#' test[, NearZeroVarEx := ifelse(runif(1000) > 0.99, runif(1), 1)]
#' test[, CharUniqueEx := as.factor(ifelse(RandomNum < 0.95, sample(letters, size = 1), "FFF"))]
#' test[, NA_RateEx := ifelse(RandomNum < 0.95, NA, "A")]
#' test[, ZeroRateEx := ifelse(RandomNum < 0.95, 0, runif(1))]
#' test[, HighSkewThreshEx := ifelse(RandomNum > 0.96, 100000, 1)]
#' ProblematicFeatures(test,
#'                     ColumnNumbers = 2:ncol(test),
#'                     NearZeroVarThresh = 0.05,
#'                     CharUniqThresh = 0.50,
#'                     NA_Rate = 0.20,
#'                     Zero_Rate = 0.20,
#'                     HighSkewThresh = 10)
#' @return data table with new dummy variables columns and optionally removes base columns
#' @export
ProblematicFeatures <- function(data,
                                ColumnNumbers = c(1:ncol(data)),
                                NearZeroVarThresh = 0.05,
                                CharUniqThresh = 0.50,
                                NA_Rate = 0.20,
                                Zero_Rate = 0.20,
                                HighSkewThresh = 10) {
  # Convert to data.table----
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)
  
  # Subset columns of interest----
  keep <- names(data)[ColumnNumbers]
  data <- data[, ..keep]
  
  # Define Functions for Calculations----
  LowVarianceFeatures <- function(data, NearZeroVarThresh = 0.05) {
    # Skip Option----
    if (is.null(NearZeroVarThresh))
      return(NULL)
    
    # Ensure argument is valid----
    if (NearZeroVarThresh > 1)
      warning("NearZeroVarThresh should be between zero and one")
    
    # Get Row Count----
    xx <- data[, .N]
    
    # Begin process----
    NumNearZeroVariance <- list()
    for (i in seq_len(ncol(data))) {
      if (is.numeric(data[[i]]) &
          length(unique(data[[i]])) / xx < NearZeroVarThresh) {
        NumNearZeroVariance[names(data)[i]] <-
          round(length(unique(data[[i]])) / xx, 4)
      }
    }
    
    if (length(NumNearZeroVariance) > 0) {
      a <-
        tryCatch({
          data.table::as.data.table(data.table::melt(NumNearZeroVariance))
        },
        error = function(x)
          NULL)
      if (dim(a)[1] != 0) {
        data.table::setnames(a,
                             c("L1", "value"),
                             c("ColName", "LowVarianceFeatures"))
        data.table::setcolorder(a, c(2, 1))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  HighCardinalityFeatures <- function(data, CharUniqThresh = 0.50) {
    # Skip Option----
    if (is.null(CharUniqThresh))
      return(NULL)
    
    # Ensure argument is valid----
    if (CharUniqThresh > 1)
      warning("CharUniqThresh should be between zero and one")
    
    # Get Row Count----
    xx <- data[, .N]
    
    # Begin process----
    CharUniqueTooHigh <- list()
    for (i in seq_len(ncol(data))) {
      if ((is.character(data[[i]]) |
           is.factor(data[[i]])) &
          length(unique(data[[i]])) / xx > CharUniqThresh) {
        CharUniqueTooHigh[names(data)[i]] <-
          round(length(unique(data[[i]])) / xx, 4)
      }
    }
    if (length(CharUniqueTooHigh) > 0) {
      a <-
        tryCatch({
          data.table::as.data.table(data.table::melt(CharUniqueTooHigh))
        },
        error = function(x)
          NULL)
      if (dim(a)[1] != 0) {
        data.table::setnames(a,
                             c("L1", "value"),
                             c("ColName", "HighCardinalityFeatures"))
        data.table::setcolorder(a, c(2, 1))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  HighMissingCountFeatures <- function(data, NA_Rate = 0.20) {
    # Skip Option----
    if (is.null(NA_Rate))
      return(NULL)
    
    # Ensure argument is valid----
    if (NA_Rate > 1)
      warning("HighSkewThresh should be between zero and one")
    
    # Get Row Count----
    xx <- data[, .N]
    
    # Begin process----
    LargeNAs <- list()
    for (i in seq_len(ncol(data))) {
      if (sum(is.na(data[[i]]) / xx) > NA_Rate) {
        LargeNAs[names(data)[i]] <- round(sum(is.na(data[[i]])) / xx, 4)
      }
    }
    if (length(LargeNAs) > 0) {
      a <-
        tryCatch({
          data.table::as.data.table(data.table::melt(LargeNAs))
        },
        error = function(x)
          NULL)
      if (dim(a)[1] != 0) {
        data.table::setnames(a,
                             c("L1", "value"),
                             c("ColName", "HighMissingCountFeatures"))
        data.table::setcolorder(a, c(2, 1))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  HighZeroCountFeatures <- function(data, Zero_Rate = 0.20) {
    # Skip Option----
    if (is.null(Zero_Rate))
      return(NULL)
    
    # Get Row Count----
    xx <- data[, .N]
    
    # Begin process----
    LargeZeros <- list()
    for (i in seq_len(ncol(data))) {
      if (is.numeric(data[[i]]) &
          data[get(names(data)[i]) == 0, .N] / xx > Zero_Rate) {
        LargeZeros[names(data)[i]] <-
          round(data[get(names(data)[i]) == 0, .N] / xx, 4)
      }
    }
    if (length(LargeZeros) > 0) {
      a <-
        tryCatch({
          data.table::as.data.table(data.table::melt(LargeZeros))
        },
        error = function(x)
          NULL)
      if (dim(a)[1] != 0) {
        data.table::setnames(a,
                             c("L1", "value"),
                             c("ColName", "HighZeroCountFeatures"))
        data.table::setcolorder(a, c(2, 1))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  HighSkewFeatures <- function(data, HighSkewThresh = 10) {
    # Skip Option----
    if (is.null(HighSkewThresh))
      return(NULL)
    
    # Ensure argument is valid----
    if (!is.numeric(HighSkewThresh) & !is.integer(HighSkewThresh)) {
      warning("HighSkewThresh should a numeric value")
    }
    
    # Get Row Count----
    xx <- data[, .N]
    
    # Begin process----
    HighSkew <- list()
    for (i in seq_len(ncol(data))) {
      if (is.numeric(data[[i]]) | is.integer(data[[i]])) {
        x <- sort(x = data[[i]],
                  na.last = TRUE,
                  decreasing = TRUE)
        if (!(max(data[[i]], na.rm = TRUE) == 0 &
              min(data[[i]], na.rm = TRUE) == 0)) {
          if (sum(x[1:(length(x) * (1 - 0.95))], na.rm = TRUE) /
              sum(x[(length(x) * (1 - 0.95)):xx], na.rm = TRUE) > HighSkewThresh) {
            HighSkew[names(data)[i]] <-
              round(sum(x[1:length(x) * 0.05], na.rm = TRUE) /
                      sum(x[(floor(length(x) *
                                     (0.95)) + 1):length(x)], na.rm = TRUE), 4)
          }
        }
      }
    }
    if (length(HighSkew) > 0) {
      a <-
        tryCatch({
          data.table::as.data.table(data.table::melt(HighSkew))
        },
        error = function(x)
          NULL)
      if (dim(a)[1] != 0) {
        data.table::setnames(a, c("L1", "value"), c("ColName", "HighSkewFeatures"))
        data.table::setcolorder(a, c(2, 1))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  
  # Initalize collection
  collect <- list()
  z <- 0
  
  # LowVarianceFeatures Run----
  a <-
    tryCatch({
      LowVarianceFeatures(data, NearZeroVarThresh = NearZeroVarThresh)
    },
    error = function(x)
      NULL)
  if (!is.null(a)) {
    z <- z + 1
    collect[[z]] <- a
  }
  
  # HighCardinalityFeatures Run----
  b <-
    tryCatch({
      HighCardinalityFeatures(data, CharUniqThresh = CharUniqThresh)
    },
    error = function(x)
      NULL)
  if (!is.null(b)) {
    z <- z + 1
    collect[[z]] <- b
  }
  
  # HighMissingCountFeatures Run----
  c <- tryCatch({
    HighMissingCountFeatures(data, NA_Rate = NA_Rate)
  },
  error = function(x)
    NULL)
  if (!is.null(c)) {
    z <- z + 1
    collect[[z]] <- c
  }
  
  # HighZeroCountFeatures Run----
  d <-
    tryCatch({
      HighZeroCountFeatures(data, Zero_Rate = Zero_Rate)
    },
    error = function(x)
      NULL)
  if (!is.null(d)) {
    z <- z + 1
    collect[[z]] <- d
  }
  
  # HighSkewFeatures Run----
  e <-
    tryCatch({
      HighSkewFeatures(data, HighSkewThresh = HighSkewThresh)
    },
    error = function(x)
      NULL)
  if (!is.null(e)) {
    z <- z + 1
    collect[[z]] <- e
  }
  
  # Combine Outputs
  if (length(collect) == 0) {
    return(NULL)
  } else if (length(collect) == 1) {
    return(collect[[1]])
  } else {
    for (x in seq_len(length(collect))) {
      if (x == 1) {
        val <- collect[[x]]
      } else {
        temp <- collect[[x]]
        val <- merge(val, temp, by = "ColName", all = TRUE)
      }
    }
    return(val)
  }
}