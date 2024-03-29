# AutoQuant is a package for quickly creating high quality visualizations under a common and easy api.
# Copyright (C) <year>  <name of author>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' @title ProblematicFeatures
#'
#' @description ProblematicFeatures identifies problematic features for machine learning and outputs a data.table of the feature names in the first column and the metrics they failed to pass in the columns.
#'
#' @author Adrian Antico
#' @family EDA
#'
#' @param data The data.table with the columns you wish to have analyzed
#' @param ColumnNumbers A vector with the column numbers you wish to analyze
#' @param NearZeroVarThresh Set to NULL to not run NearZeroVar(). Checks to see if the percentage of values in your numeric columns that are not constant are greater than the value you set here. If not, the feature is collects and returned with the percentage unique value.
#' @param CharUniqThresh Set to NULL to not run CharUniqthresh(). Checks to see if the percentage of unique levels / groups in your categorical feature is greater than the value you supply. If it is, the feature name is returned with the percentage unique value.
#' @param NA_Rate Set to NULL to not run NA_Rate(). Checks to see if the percentage of NA's in your features is greater than the value you supply. If it is, the feature name is returned with the percentage of NA values.
#' @param Zero_Rate Set to NULL to not run Zero_Rate(). Checks to see if the percentage of zero's in your features is greater than the value you supply. If it is, the feature name is returned with the percentage of zero values.
#' @param HighSkewThresh Set to NULL to not run HighSkew(). Checks for numeric columns whose ratio of the sum of the top 5th percentile of values to the bottom 95th percentile of values is greater than the value you supply. If true, the column name and value is returned.
#' @examples
#' \dontrun{
#' test <- data.table::data.table(RandomNum = runif(1000))
#' test[, NearZeroVarEx := ifelse(runif(1000) > 0.99, runif(1), 1)]
#' test[, CharUniqueEx := as.factor(ifelse(RandomNum < 0.95, sample(letters, size = 1), "FFF"))]
#' test[, NA_RateEx := ifelse(RandomNum < 0.95, NA, "A")]
#' test[, ZeroRateEx := ifelse(RandomNum < 0.95, 0, runif(1))]
#' test[, HighSkewThreshEx := ifelse(RandomNum > 0.96, 100000, 1)]
#' ProblematicFeatures(
#'   test,
#'   ColumnNumbers = 2:ncol(test),
#'   NearZeroVarThresh = 0.05,
#'   CharUniqThresh = 0.50,
#'   NA_Rate = 0.20,
#'   Zero_Rate = 0.20,
#'   HighSkewThresh = 10)
#' }
#' @return data table with new dummy variables columns and optionally removes base columns
#' @noRd
ProblematicFeatures <- function(data,
                                ColumnNumbers = c(1:ncol(data)),
                                NearZeroVarThresh = 0.05,
                                CharUniqThresh = 0.50,
                                NA_Rate = 0.20,
                                Zero_Rate = 0.20,
                                HighSkewThresh = 10) {

  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))

  # Convert to data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Subset columns of interest----
  data <- data[, .SD, .SDcols = names(data)[ColumnNumbers]]

  # Define Functions for Calculations ----
  LowVarianceFeatures <- function(data, NearZeroVarThresh = 0.05) {
    if(is.null(NearZeroVarThresh)) stop("NearZeroVarThresh cannot be NULL")
    if(NearZeroVarThresh > 1) stop("NearZeroVarThresh should be between zero and one")
    xx <- data[, .N]
    NumNearZeroVariance <- list()
    for(i in seq_len(ncol(data))) {
      if(is.numeric(data[[i]]) && length(unique(data[[i]])) / xx < NearZeroVarThresh) {
        NumNearZeroVariance[names(data)[i]] <- round(length(unique(data[[i]])) / xx, 4L)
      }
    }
    if(length(NumNearZeroVariance) > 0L) {
      a <- tryCatch({data.table::as.data.table(data.table::melt(NumNearZeroVariance))}, error = function(x) NULL)
      if(dim(a)[1L] != 0L) {
        data.table::setnames(a, c("L1", "value"), c("ColName", "LowVarianceFeatures"))
        data.table::setcolorder(a, c(2L, 1L))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  HighCardinalityFeatures <- function(data, CharUniqThresh = 0.50) {
    if(is.null(CharUniqThresh)) return(NULL)
    if(CharUniqThresh > 1L) return("CharUniqThresh should be between zero and one")
    xx <- data[, .N]
    CharUniqueTooHigh <- list()
    for(i in seq_len(ncol(data))) {
      if((is.character(data[[i]]) | is.factor(data[[i]])) & length(unique(data[[i]])) / xx > CharUniqThresh) {
        CharUniqueTooHigh[names(data)[i]] <- round(length(unique(data[[i]])) / xx, 4L)
      }
    }
    if(length(CharUniqueTooHigh) > 0L) {
      a <- tryCatch({data.table::as.data.table(data.table::melt(CharUniqueTooHigh))}, error = function(x) NULL)
      if(dim(a)[1L] != 0L) {
        data.table::setnames(a, c("L1", "value"), c("ColName", "HighCardinalityFeatures"))
        data.table::setcolorder(a, c(2L, 1L))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  HighMissingCountFeatures <- function(data, NA_Rate = 0.20) {
    if(is.null(NA_Rate)) return(NULL)
    if(NA_Rate > 1L) return("HighSkewThresh should be between zero and one")
    xx <- data[, .N]
    LargeNAs <- list()
    for(i in seq_len(ncol(data))) {
      if(sum(is.na(data[[i]]) / xx) > NA_Rate) {
        LargeNAs[names(data)[i]] <- round(sum(is.na(data[[i]])) / xx, 4L)
      }
    }
    if(length(LargeNAs) > 0L) {
      a <- tryCatch({data.table::as.data.table(data.table::melt(LargeNAs))}, error = function(x) NULL)
      if(dim(a)[1L] != 0L) {
        data.table::setnames(a, c("L1", "value"), c("ColName", "HighMissingCountFeatures"))
        data.table::setcolorder(a, c(2L, 1L))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  HighZeroCountFeatures <- function(data, Zero_Rate = 0.20) {
    if(is.null(Zero_Rate)) return(NULL)
    xx <- data[, .N]
    LargeZeros <- list()
    for(i in seq_len(ncol(data))) {
      if(is.numeric(data[[i]]) & data[get(names(data)[i]) == 0, .N] / xx > Zero_Rate) {
        LargeZeros[names(data)[i]] <- round(data[get(names(data)[i]) == 0, .N] / xx, 4L)
      }
    }
    if(length(LargeZeros) > 0L) {
      a <- tryCatch({data.table::as.data.table(data.table::melt(LargeZeros))}, error = function(x) NULL)
      if(dim(a)[1L] != 0L) {
        data.table::setnames(a, c("L1", "value"), c("ColName", "HighZeroCountFeatures"))
        data.table::setcolorder(a, c(2L, 1L))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  HighSkewFeatures <- function(data, HighSkewThresh = 10) {
    if(is.null(HighSkewThresh)) return(NULL)
    if(!is.numeric(HighSkewThresh) & !is.integer(HighSkewThresh)) return("HighSkewThresh should a numeric value")
    xx <- data[, .N]
    HighSkew <- list()
    for(i in seq_len(ncol(data))) {
      if(is.numeric(data[[i]]) | is.integer(data[[i]])) {
        x <- sort(x = data[[i]], na.last = TRUE, decreasing = TRUE)
        if(!(max(data[[i]], na.rm = TRUE) == 0 & min(data[[i]], na.rm = TRUE) == 0)) {
          if(sum(x[1L:(length(x) * (1 - 0.95))], na.rm = TRUE) / sum(x[(length(x) * (1 - 0.95)):xx], na.rm = TRUE) > HighSkewThresh) {
            HighSkew[names(data)[i]] <- round(sum(x[1:length(x) * 0.05], na.rm = TRUE) / sum(x[(floor(length(x) * (0.95)) + 1):length(x)], na.rm = TRUE), 4L)
          }
        }
      }
    }
    if(length(HighSkew) > 0L) {
      a <- tryCatch({data.table::as.data.table(data.table::melt(HighSkew))}, error = function(x) NULL)
      if(dim(a)[1L] != 0L) {
        data.table::setnames(a, c("L1", "value"), c("ColName", "HighSkewFeatures"))
        data.table::setcolorder(a, c(2L, 1L))
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
  z <- 0L

  # LowVarianceFeatures Run ----
  a <- tryCatch({LowVarianceFeatures(data, NearZeroVarThresh = NearZeroVarThresh)}, error = function(x) NULL)
  if(!is.null(a)) {
    z <- z + 1L
    collect[[z]] <- a
  }

  # HighCardinalityFeatures Run----
  b <- tryCatch({HighCardinalityFeatures(data, CharUniqThresh = CharUniqThresh)}, error = function(x) NULL)
  if(!is.null(b)) {
    z <- z + 1L
    collect[[z]] <- b
  }

  # HighMissingCountFeatures Run----
  c <- tryCatch({HighMissingCountFeatures(data, NA_Rate = NA_Rate)}, error = function(x) NULL)
  if(!is.null(c)) {
    z <- z + 1L
    collect[[z]] <- c
  }

  # HighZeroCountFeatures Run----
  d <- tryCatch({HighZeroCountFeatures(data, Zero_Rate = Zero_Rate)}, error = function(x) NULL)
  if(!is.null(d)) {
    z <- z + 1L
    collect[[z]] <- d
  }

  # HighSkewFeatures Run----
  e <- tryCatch({HighSkewFeatures(data, HighSkewThresh = HighSkewThresh)}, error = function(x) NULL)
  if(!is.null(e)) {
    z <- z + 1L
    collect[[z]] <- e
  }

  # Combine Outputs
  if(length(collect) == 0L) {
    return(NULL)
  } else if (length(collect) == 1L) {
    return(collect[[1L]])
  } else {
    for(x in seq_len(length(collect))) {
      if(x == 1L) {
        val <- collect[[x]]
      } else {
        temp <- collect[[x]]
        val <- merge(val, temp, by = "ColName", all = TRUE)
      }
    }
    return(val)
  }
}
