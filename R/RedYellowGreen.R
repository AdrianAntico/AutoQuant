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

#' @title RedYellowGreen
#'
#' @description This function will find the optimial thresholds for applying the main label and for finding the optimial range for doing nothing when you can quantity the cost of doing nothing
#'
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#'
#' @param data data is the data table with your predicted and actual values from a classification model
#' @param PredictColNumber The column number where the prediction variable is located (in binary form)
#' @param ActualColNumber The column number where the target variable is located
#' @param TruePositiveCost This is the utility for generating a true positive prediction
#' @param TrueNegativeCost This is the utility for generating a true negative prediction
#' @param FalsePositiveCost This is the cost of generating a false positive prediction
#' @param FalseNegativeCost This is the cost of generating a false negative prediction
#' @param MidTierCost This is the cost of doing nothing (or whatever it means to not classify in your case)
#' @param Cores Number of cores on your machine
#' @param Precision Set the decimal number to increment by between 0 and 1
#' @param Boundaries Supply a vector of two values c(lower bound, upper bound) where the first value is the smallest threshold you want to test and the second value is the largest value you want to test. Note, if your results are at the boundaries you supplied, you should extent the boundary that was reached until the values is within both revised boundaries.
#' @import foreach
#' @examples
#' \dontrun{
#' data <- data.table::data.table(Target = runif(10))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(10)]
#' data[, Predict := log(pnorm(0.85 * x1 +
#'   sqrt(1-0.85^2) * qnorm(x2)))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' data <- RedYellowGreen(
#'   data,
#'   PredictColNumber  = 2,
#'   ActualColNumber   = 1,
#'   TruePositiveCost  = 0,
#'   TrueNegativeCost  = 0,
#'   FalsePositiveCost = -1,
#'   FalseNegativeCost = -2,
#'   MidTierCost = -0.5,
#'   Precision = 0.01,
#'   Cores = 1,
#'   Boundaries = c(0.05,0.75))
#' }
#' @return A data table with all evaluated strategies, parameters, and utilities, along with a 3d scatterplot of the results
#' @export
RedYellowGreen <- function(data,
                           PredictColNumber  = 2,
                           ActualColNumber   = 1,
                           TruePositiveCost  = 0,
                           TrueNegativeCost  = 0,
                           FalsePositiveCost = -10,
                           FalseNegativeCost = -50,
                           MidTierCost       = -2,
                           Cores             = 8,
                           Precision         = 0.01,
                           Boundaries        = c(0.05, 0.75)) {

  # Turn on full speed ahead ----
  requireNamespace('doParallel', quietly = FALSE)
  requireNamespace('parallel', quietly = FALSE)

  # Check data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Ensure arguments are valid ----
  if(is.character(TruePositiveCost)) stop("TruePositiveCost must be numeric")
  if(is.character(TrueNegativeCost)) stop("TruePositiveCost must be numeric")
  if(is.character(FalsePositiveCost)) stop("TruePositiveCost must be numeric")
  if(is.character(FalseNegativeCost)) return("TruePositiveCost must be numeric")
  if(is.character(MidTierCost)) stop("TruePositiveCost must be numeric")
  if(Precision < 0 || Precision > 0.5) stop("Precision should be a decimal value greater than 0 and less than 0.5")
  if(min(Boundaries) < 0 || max(Boundaries) > 0.99999999999999999999) stop("Boundaries should be a decimal value greater than 0 and less than 0.99999999999999999999")
  if(Boundaries[1L] > Boundaries[2L]) stop("The first Boundaries element should be less than the second element")

  # Set up evaluation table ----
  analysisTable <- data.table::data.table(
    TPP = rep(TruePositiveCost, 1L),
    TNP = rep(TrueNegativeCost, 1L),
    FPP = rep(FalsePositiveCost, 1L),
    FNP = rep(FalseNegativeCost, 1L),
    MTDN = rep(TRUE, 1L),
    MTC = rep(MidTierCost, 1L),
    Threshold = runif(1L))

  # Do nothing possibilities ----
  temp <- data.table::CJ(MTLT = seq(Boundaries[1L], Boundaries[2L], Precision), MTHT = seq(Boundaries[1L], Boundaries[2L], Precision))[MTHT > MTLT]
  new <- cbind(analysisTable, temp)
  new[, Utility := stats::runif(nrow(new))]

  # Parallel setup ----
  requireNamespace(c("parallel", "doParallel", "foreach"))
  packages <- c("data.table","AutoQuant")
  cores <- Cores
  bat <- ceiling(nrow(new) / cores)
  parts <- floor(nrow(new) / bat)
  cl <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))

  # Kick off run ----
  results <- foreach::foreach(
    i = itertools::isplitRows(new, chunks = parts),
    .combine = function(...) data.table::rbindlist(list(...)),
    .multicombine = TRUE,
    .packages     = packages
    ) %dopar% {

      # Define functions for use inside function
      RedYellowGreenParallel <- function(data,
                                         PredictColNumber  = 1,
                                         ActualColNumber   = 767,
                                         TruePositiveCost  = 0,
                                         TrueNegativeCost  = 0,
                                         FalsePositiveCost = -1,
                                         FalseNegativeCost = -10,
                                         MidTierCost       = -5,
                                         new = i) {
        # Loop through all combos
        for(k in seq_len(nrow(new))) {
          x <- threshOptimz(
            data = data,
            actTar = names(data)[ActualColNumber],
            predTar = names(data)[PredictColNumber],
            tpProfit = TruePositiveCost,
            tnProfit = TrueNegativeCost,
            fpProfit = FalsePositiveCost,
            fnProfit = FalseNegativeCost,
            MidTierDoNothing = TRUE,
            MidTierCost = MidTierCost,
            MidTierLowThresh = new[k, 8L][[1L]],
            MidTierHighThresh = new[k, 9L][[1L]])
          data.table::set(new, i = k, j = 7L, value = x[[1L]])
          temp <- x[[2L]]
          data.table::set(new, i = k, j = 10L, value = temp[Thresholds == eval(x[[1L]]), "Utilities"][[1L]])
        }
        return(new)
      }

      # Inner function for threshold optimizataion
      threshOptimz <- function(data,
                               actTar   = 1,
                               predTar  = 2,
                               tpProfit = 1,
                               tnProfit = 5,
                               fpProfit = -1,
                               fnProfit = -1,
                               MidTierDoNothing = FALSE,
                               MidTierCost = -100,
                               MidTierLowThresh = 0.25,
                               MidTierHighThresh = 0.75) {
        # Convert factor target to numeric
        data[, eval(actTar) := as.numeric(as.character(get(actTar)))]

        # Optimize each column's classification threshold ::
        popTrue <- mean(data[[(actTar)]])
        store   <- list()
        j <- 0L
        options(warn = -1L)
        for(i in c(MidTierHighThresh)) {
          j <- j + 1L
          if(tpProfit != 0) {
            tp <- sum(data.table::fifelse(!(data[[predTar]] < MidTierHighThresh & data[[predTar]] > MidTierLowThresh) & data[[actTar]] == 1 & data[[predTar]] >= i, 1, 0))
          } else {
            tp <- 0
          }
          if(tnProfit != 0) {
            tn <- sum(data.table::fifelse(!(data[[predTar]] < MidTierHighThresh & data[[predTar]] > MidTierLowThresh) & data[[actTar]] == 0 & data[[predTar]] <  i,1,0))
          } else {
            tn <- 0
          }
          if(fpProfit != 0) {
            fp <- sum(data.table::fifelse(!(data[[predTar]] < MidTierHighThresh & data[[predTar]] > MidTierLowThresh) & data[[actTar]] == 0 & data[[predTar]] >= i,1,0))
          } else {
            fp <- 0
          }
          if(fnProfit != 0) {
            fn <- sum(data.table::fifelse(!(data[[predTar]] < MidTierHighThresh & data[[predTar]] > MidTierLowThresh) & data[[actTar]] == 1 & data[[predTar]] <  i,1,0))
          } else {
            fp <- 0
          }
          none <- sum(data.table::fifelse(data[[predTar]] <= MidTierHighThresh & data[[predTar]] >= MidTierLowThresh,1,0))
          tpr <- data.table::fifelse((tp + fn) == 0, 0, tp / (tp + fn))
          fpr <- data.table::fifelse((fp + tn) == 0, 0, fp / (fp + tn))
          noneRate <- none / nrow(data)
          utility <- (1 - noneRate) * (popTrue * (tpProfit * tpr + fnProfit * (1 - tpr)) + (1 - popTrue) * (fpProfit * fpr + tnProfit * (1 - fpr))) + noneRate * MidTierCost
          store[[j]] <- c(i, utility)
        }
        all <- data.table::rbindlist(list(store))
        utilities <- data.table::melt(all[2L,])
        data.table::setnames(utilities, "value", "Utilities")
        thresholds <- data.table::melt(all[1L,])
        data.table::setnames(thresholds, "value", "Thresholds")
        results <- cbind(utilities, thresholds)[, c(-1L,-3L)]
        thresh <- results[Thresholds <= eval(MidTierLowThresh) | Thresholds >= eval(MidTierHighThresh)][order(-Utilities)][1L, 2L][[1L]]
        options(warn = 1L)
        return(list(thresh, results))
      }

      # Run core function
      data <- RedYellowGreenParallel(
        data,
        PredictColNumber  = PredictColNumber,
        ActualColNumber   = ActualColNumber,
        TruePositiveCost  = TruePositiveCost,
        TrueNegativeCost  = TrueNegativeCost,
        FalsePositiveCost = FalsePositiveCost,
        FalseNegativeCost = FalseNegativeCost,
        MidTierCost       = MidTierCost,
        new               = i)

      # Return data table
      data
    }

  # 3D Scatterplot ----
  s3d <- scatterplot3d::scatterplot3d(
    x = results[["MTLT"]],
    y = results[["MTHT"]],
    z = results[["Utility"]],
    type = "p",
    color = "#401a50",
    angle = 35L,
    pch = 16L,
    main = paste0("Utility Maximizer - Main Threshold at ", results[order(-Utility)][1, "MTHT"][[1L]]),
    sub = paste0("Lower Thresh = ", results[order(-Utility)][1L, "MTLT"][[1L]], " and Upper Thresh = ", results[order(-Utility)][1L, "MTHT"][[1L]]), xlab = "Mid Tier Lower Threshold", ylab = "Mid Tier Higher Threshold", zlab = "Utility")
  model <- stats::lm(results[["Utility"]] ~ results[["MTLT"]] + results[["MTHT"]])
  s3d$plane3d(model)
  N <- nrow(results)
  s3d$points3d(
    x = results[order(-Utility)][seq_len(N / 100), "MTLT"][[1L]],
    y = results[order(-Utility)][seq_len(N / 100), "MTHT"][[1L]],
    z = results[order(-Utility)][seq_len(N / 100), "Utility"][[1L]],
    col = "#00aa9d",
    type = "h",
    pch = 1L)
  s3d$points3d(
    x = results[order(-Utility)][1L, "MTLT"][[1L]],
    y = results[order(-Utility)][1L, "MTHT"][[1L]],
    z = results[order(-Utility)][1L, "Utility"][[1L]],
    col = "black",
    type = "h",
    pch = 10L)
  return(results)
}
