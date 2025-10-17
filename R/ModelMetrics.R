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

#' @title CarmaHoldoutMetrics
#'
#' @description  CarmaHoldoutMetrics
#'
#' @author Adrian Antico
#' @family Carma Helper
#'
#' @param DATA TestDataEval
#' @param TARGETCOLUMNNAME TargetColumnName
#' @param GROUPVARIABLES GroupVariables
#' @noRd
CarmaHoldoutMetrics <- function(DATA = TestDataEval,
                                TARGETCOLUMNNAME = TargetColumnName,
                                GROUPVARIABLES = GroupingVariables) {

  # Start----
  if(!is.null(GROUPVARIABLES)) {
    MetricCollection <- DATA[, GroupVar, by = "GroupVar"][, GroupVar := NULL]
  } else {
    MetricCollection <- data.table::data.table(Metric = c("MAE","MSE","MAPE","R2"), MetricValue = rep(0,4))
  }

  # MAE----
  if(!is.null(GROUPVARIABLES)) {
    data.table::set(DATA, j = "Metric", value = abs(DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]))
    MetricCollection <- merge(MetricCollection, DATA[, .(MAE_Metric = mean(Metric, na.rm = TRUE)), by = list(GroupVar)],
                              by = "GroupVar",
                              all = FALSE)
  } else {
    data.table::set(DATA, j = "Metric", value = abs(DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]))
    data.table::set(MetricCollection, i = 1L, j = "MetricValue", value = mean(DATA[["Metric"]], na.rm = TRUE))
  }

  # MAPE----
  if(!is.null(GROUPVARIABLES)) {
    data.table::set(DATA, j = "Metric", value = abs((DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) / (DATA[[eval(TARGETCOLUMNNAME)]] + 1)))
    MetricCollection <- merge(MetricCollection, DATA[, .(MAPE_Metric = mean(Metric, na.rm = TRUE)), by = list(GroupVar)],
                              by = "GroupVar",
                              all = FALSE)
  } else {
    data.table::set(DATA, j = "Metric", value = abs((DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) / (DATA[[eval(TARGETCOLUMNNAME)]] + 1)))
    data.table::set(MetricCollection, i = 3L, j = "MetricValue", value = round(mean(DATA[["Metric"]], na.rm = TRUE),3))
  }

  # MSE----
  if(!is.null(GROUPVARIABLES)) {
    data.table::set(DATA, j = "Metric", value = (DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) ^ 2)
    MetricCollection <- merge(MetricCollection, DATA[, .(MSE_Metric = mean(Metric, na.rm = TRUE)), by = list(GroupVar)],
                              by = "GroupVar",
                              all = FALSE)
  } else {
    data.table::set(DATA, j = "Metric", value = (DATA[[eval(TARGETCOLUMNNAME)]] - DATA[["Predictions"]]) ^ 2)
    data.table::set(MetricCollection, i = 2L, j = "MetricValue", value = round(mean(DATA[["Metric"]], na.rm = TRUE),3))
  }

  # R2----
  tryCatch({if(!is.null(GROUPVARIABLES)) {
    MetricCollection <- merge(MetricCollection,
                              DATA[, .(R2_Metric = stats::cor(get(TARGETCOLUMNNAME), Predictions)), by = list(GroupVar)],
                              by = "GroupVar",
                              all = FALSE)
    data.table::set(MetricCollection, j = "R2_Metric", value = MetricCollection[["R2_Metric"]]^2)
  } else {
    data.table::set(MetricCollection, i = 4L, j = "MetricValue", value = round(stats::cor(DATA[[eval(TARGETCOLUMNNAME)]], DATA[["Predictions"]]) ^ 2,3))
  }}, error = function(x) NULL)

  # Return----
  return(MetricCollection)
}

#' @title DT_BinaryConfusionMatrix
#'
#' @description DT_BinaryConfusionMatrix is for computing all metrics related to binary modeling outcomes
#'
#' @family Model Evaluation
#' @author Adrian Antico
#'
#' @param data Supply your model validation data with predictions
#' @param GroupVariables Supply grouping variables to generate statistics by groups
#' @param Target The name of your target variable column
#' @param Predicted The name of your predicted value column#'
#' @examples
#' \dontrun{
#' AggMetricsByGroup <- DT_BinaryConfusionMatrix(
#'   data,
#'   GroupVariables = c("Store","Dept"),
#'   Target = "HitTarget",
#'   Predicted = "p1")
#' }
#' @noRd
DT_BinaryConfusionMatrix <- function(data = MetricsData,
                                     GroupVariables = "IntervalNum",
                                     Target = "ActiveAtInterval",
                                     Predicted = "p1") {

  # Build columns to get info----
  data.table::set(data, j = "Correct", value = data.table::fifelse(data[[eval(Predicted)]] == data[[eval(Target)]], 1, 0))
  data.table::set(data, j = "Correct_1", value = data.table::fifelse(data[[eval(Predicted)]] == data[[(Target)]] & data[[eval(Target)]] == 1, 1, 0))
  data.table::set(data, j = "Correct_0", value = data.table::fifelse(data[[eval(Predicted)]] == data[[eval(Target)]] & data[[eval(Target)]] == 0, 1, 0))
  data.table::set(data, j = "Incorrect_1", value = data.table::fifelse(data[[(Predicted)]] != data[[eval(Target)]] & data[[eval(Target)]] == 0, 1, 0))
  data.table::set(data, j = "Incorrect_0", value = data.table::fifelse(data[[eval(Predicted)]] != data[[eval(Target)]] & data[[eval(Target)]] == 1, 1, 0))

  # Compute confusion matrix by group----
  if(!is.null(GroupVariables)) {
    AggData <- data[, .(Counts = .N,
                        P  = sum(get(Target) == 0, na.rm = TRUE),
                        N  = sum(get(Target) == 1, na.rm = TRUE),
                        TP = sum(Correct_1, na.rm = TRUE),
                        TN = sum(Correct_0, na.rm = TRUE),
                        FP = sum(Incorrect_1, na.rm = TRUE),
                        FN = sum(Incorrect_0, na.rm = TRUE)),
                    by = c(GroupVariables)][order(GroupVariables)]
  } else {

    # No grouping variables----
    AggData <- data[, .(Counts = .N,
                        P  = sum(get(Target) == 0, na.rm = TRUE),
                        N  = sum(get(Target) == 1, na.rm = TRUE),
                        TP = sum(Correct_1, na.rm = TRUE),
                        TN = sum(Correct_0, na.rm = TRUE),
                        FP = sum(Incorrect_1, na.rm = TRUE),
                        FN = sum(Incorrect_0, na.rm = TRUE))]
  }

  # Add other confusion matrix measures----
  data.table::set(AggData, j = "Accuracy",     value = (AggData[["TP"]] + AggData[["TN"]]) / (AggData[["Counts"]]))
  data.table::set(AggData, j = "Precision",    value = AggData[["TP"]] / (AggData[["TP"]] + AggData[["FP"]]))
  data.table::set(AggData, j = "NegPredValue", value = AggData[["TN"]] / (AggData[["TN"]] + AggData[["FN"]]))
  data.table::set(AggData, j = "F1_Score",     value = 2 * AggData[["TP"]] / (2 * AggData[["TP"]] + data[["FP"]] + data[["FN"]]))
  data.table::set(AggData, j = "MCC",          value = (AggData[["TP"]] * AggData[["TN"]] - AggData[["FP"]] * AggData[["FN"]]) / sqrt((AggData[["TP"]] + AggData[["FP"]]) * (AggData[["TP"]] + AggData[["FN"]]) * (AggData[["TN"]] + AggData[["FP"]]) * (AggData[["TN"]] + AggData[["FN"]])))
  data.table::set(AggData, j = "TPR",          value = AggData[["TP"]] / (AggData[["TP"]] + AggData[["FN"]]))
  data.table::set(AggData, j = "TNR",          value = AggData[["TN"]] / (AggData[["TN"]] + AggData[["FP"]]))
  data.table::set(AggData, j = "FNR",          value = AggData[["FN"]] / (AggData[["FN"]] + AggData[["TP"]]))
  data.table::set(AggData, j = "FPR",          value = AggData[["FP"]] / (AggData[["FP"]] + AggData[["TN"]]))
  data.table::set(AggData, j = "FDR",          value = AggData[["FP"]] / (AggData[["FP"]] + AggData[["TP"]]))
  data.table::set(AggData, j = "FOR",          value = AggData[["FN"]] / (AggData[["FN"]] + AggData[["TN"]]))
  data.table::set(AggData, j = "ThreatScore",  value = AggData[["TP"]] / (AggData[["TP"]] + AggData[["FN"]] + AggData[["FP"]]))

  # return AggData----
  return(AggData)
}

#' @title ClassificationMetrics (vectorized, NA-safe, ties-positive, overflow-safe)
#' @description Fast confusion metrics for many thresholds; exact >= tie handling.
#' @author Adrian Antico
#' @family Model Evaluation
#' @param TestData data.table with target and prediction columns
#' @param Thresholds numeric vector of decision thresholds (pred >= t => positive)
#' @param Target character, name of the target column
#' @param PredictColumnName character, name of the prediction column (numeric; higher=more positive)
#' @param PositiveOutcome value in Target representing the positive class
#' @param NegativeOutcome value in Target representing the negative class
#' @param CostMatrix numeric length-4: c(TP, FN, FP, TN) utilities/costs (default c(1, -1, -1, 0))
#' @noRd
ClassificationMetrics <- function(TestData,
                                  Thresholds,
                                  Target,
                                  PredictColumnName,
                                  PositiveOutcome,
                                  NegativeOutcome,
                                  CostMatrix = c(1, -1, -1, 0)) {
  stopifnot(data.table::is.data.table(TestData))

  # Minimal columns; coerce pred to numeric; drop bad preds or NA target
  dt <- TestData[, .(
    .__pred = suppressWarnings(as.numeric(get(PredictColumnName))),
    .__y    = get(Target)
  )]
  dt <- dt[is.finite(.__pred) & !is.na(.__y)]
  if (!nrow(dt)) return(data.table::data.table())

  # Strict class flags (as doubles to avoid integer overflow later)
  dt[, .__pos := as.numeric(.__y == PositiveOutcome)]
  dt[, .__neg := as.numeric(.__y == NegativeOutcome)]
  # Optional sanity warning if other labels exist
  if (!all(dt$.__y %in% c(PositiveOutcome, NegativeOutcome))) {
    warning("Targets other than PositiveOutcome/NegativeOutcome are present; ",
            "they won't contribute to TP/TN/FP/FN but are included in PredPos/PredNeg.")
  }

  # --- ASCENDING sort on predictions ---
  data.table::setorder(dt, .__pred)

  # Suffix cumsums for >= thresholds (ties positive)
  # cTP_ge[i] = sum(pos for rows i..N); same for FP
  dt[, .__cTP_ge := rev(cumsum(rev(.__pos)))]
  dt[, .__cFP_ge := rev(cumsum(rev(.__neg)))]

  # Totals
  P1       <- sum(dt$.__pos)
  NegCount <- sum(dt$.__neg)
  N1       <- nrow(dt)

  # Clean thresholds
  Thresholds <- as.numeric(Thresholds)
  if (any(!is.finite(Thresholds))) {
    keep <- is.finite(Thresholds)
    warning(sprintf("Dropping %d non-finite thresholds", sum(!keep)))
    Thresholds <- Thresholds[keep]
  }
  if (!length(Thresholds)) return(data.table::data.table())

  # For ascending vec, findInterval(t, pred, rightmost.closed = FALSE) gives # of elements < t.
  # So index of first element with pred >= t is idx = k_lt + 1.
  k_lt <- findInterval(Thresholds, dt$.__pred, rightmost.closed = FALSE)  # 0..N
  idx  <- pmin.int(k_lt + 1L, N1)                                         # clamp to [1..N]

  # Confusion counts (as doubles)
  TP <- ifelse(k_lt < N1, dt$.__cTP_ge[idx], 0.0)  # if t > max(pred), TP=0
  FP <- ifelse(k_lt < N1, dt$.__cFP_ge[idx], 0.0)
  TN <- as.double(NegCount) - FP
  FN <- as.double(P1) - TP

  PredPos <- as.double(N1 - k_lt)  # # of rows with pred >= t
  PredNeg <- as.double(k_lt)

  # Safe division
  safe_div <- function(num, den) ifelse(den > 0, num / den, NA_real_)

  # Metrics (double to avoid overflow)
  MCC_denom <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  MCC       <- ifelse(is.finite(MCC_denom) & MCC_denom > 0,
                      ((TP * TN) - (FP * FN)) / MCC_denom, NA_real_)
  Accuracy  <- (TP + TN) / N1
  TPR       <- safe_div(TP, P1)
  TNR       <- safe_div(TN, NegCount)
  FNR       <- safe_div(FN, P1)
  FPR       <- safe_div(FP, FP + TN)
  FDR       <- safe_div(FP, FP + TP)
  FOR       <- safe_div(FN, FN + TN)
  F1_Score  <- safe_div(2 * TP, 2 * TP + FP + FN)
  F2_Score  <- safe_div(5 * TP, 5 * TP + 4 * FN + FP)
  F0_5_Score<- safe_div(1.25 * TP, 1.25 * TP + 0.25 * FN + FP)
  NPV       <- safe_div(TN, TN + FN)
  PPV       <- safe_div(TP, TP + FP)
  Threat    <- safe_div(TP, TP + FP + FN)

  base_rate <- if (N1 > 0) P1 / N1 else NA_real_
  Utility   <- base_rate * (CostMatrix[1L] * TPR + CostMatrix[2L] * (1 - TPR)) +
    (1 - base_rate) * (CostMatrix[3L] * FPR + CostMatrix[4L] * (1 - FPR))

  data.table::data.table(
    Threshold   = Thresholds,
    TN          = TN,
    TP          = TP,
    FN          = FN,
    FP          = FP,
    PredNeg     = PredNeg,
    PredPos     = PredPos,
    MCC         = MCC,
    Accuracy    = Accuracy,
    TPR         = TPR,
    TNR         = TNR,
    FNR         = FNR,
    FPR         = FPR,
    FDR         = FDR,
    FOR         = FOR,
    F1_Score    = F1_Score,
    F2_Score    = F2_Score,
    `F0.5_Score`= F0_5_Score,
    NPV         = NPV,
    PPV         = PPV,
    ThreatScore = Threat,
    Utility     = Utility
  )[]
}

#' @title ClassificationMetricsTwoThresholds (fast, vectorized, >= ties, NA-safe)
#' @description
#'   Compute counts/metrics for a two-threshold classifier with a middle "reject" band,
#'   plus the utility surface U(t1, t2) using class priors and a reject utility.
#'   Regions: Auto-Neg (pred < t1), Reject (t1 <= pred < t2), Auto-Pos (pred >= t2).
#'
#' @param TestData data.table with target and prediction columns.
#' @param LowerThresholds numeric vector of lower thresholds (t1).
#' @param UpperThresholds numeric vector of upper thresholds (t2).
#' @param Target character, name of target column.
#' @param PredictColumnName character, name of prediction column (numeric; higher=more positive).
#' @param PositiveOutcome value representing the positive class.
#' @param NegativeOutcome value representing the negative class.
#' @param RejectUtility scalar u_r utility/cost applied to items sent to Reject (review) band.
#' @param CostMatrix length-4 numeric c(TP, FN, FP, TN). For the utility surface from the paper,
#'        set TP=0, TN=0 and use FN<0, FP<0 (or positive costs if you're minimizing cost).
#'        Defaults here maintain your prior signature (but TP/TN are not used in U by default).
#'
#' @return data.table with one row per valid (t1,t2) pair (t1 <= t2), including:
#'   t1, t2, counts per region, rates (TPR/FPR at t1,t2), review_rate, and UtilitySurface.
#' @noRd
ClassificationMetricsTwoThresholds <- function(
    TestData,
    LowerThresholds,
    UpperThresholds,
    Target,
    PredictColumnName,
    PositiveOutcome,
    NegativeOutcome,
    RejectUtility = 0,
    CostMatrix = c(0, -1, -1, 0)
) {
  stopifnot(data.table::is.data.table(TestData))

  # Minimal columns; coerce pred; drop bad rows
  dt <- TestData[, .(
    .__pred = suppressWarnings(as.numeric(get(PredictColumnName))),
    .__y    = get(Target)
  )]
  dt <- dt[is.finite(.__pred) & !is.na(.__y)]
  if (!nrow(dt)) return(data.table::data.table())

  # Class flags (as doubles to avoid integer overflow later)
  dt[, .__pos := as.numeric(.__y == PositiveOutcome)]
  dt[, .__neg := as.numeric(.__y == NegativeOutcome)]

  # Sort ASCENDING to enable ">= t" via suffix sums (ties counted as positive)
  data.table::setorder(dt, .__pred)

  # Prefix sums for "< t" region
  pos_lt <- cumsum(dt$.__pos)
  neg_lt <- cumsum(dt$.__neg)

  # Suffix sums for ">= t" region
  pos_ge <- rev(cumsum(rev(dt$.__pos)))
  neg_ge <- rev(cumsum(rev(dt$.__neg)))

  # Totals and priors
  P1 <- sum(dt$.__pos)
  N0 <- sum(dt$.__neg)
  N  <- nrow(dt)
  r_p <- if (N > 0) P1 / N else NA_real_
  r_n <- if (N > 0) N0 / N else NA_real_

  # Threshold vectors (clean)
  LowerThresholds <- as.numeric(LowerThresholds)
  UpperThresholds <- as.numeric(UpperThresholds)
  if (any(!is.finite(LowerThresholds))) LowerThresholds <- LowerThresholds[is.finite(LowerThresholds)]
  if (any(!is.finite(UpperThresholds))) UpperThresholds <- UpperThresholds[is.finite(UpperThresholds)]
  if (!length(LowerThresholds) || !length(UpperThresholds)) return(data.table::data.table())

  # For ascending vec, findInterval(t, pred, rightmost.closed = FALSE) returns # of rows with pred < t
  k1_lt <- findInterval(LowerThresholds, dt$.__pred, rightmost.closed = FALSE)  # length L1
  k2_lt <- findInterval(UpperThresholds, dt$.__pred, rightmost.closed = FALSE)  # length L2

  # TPR/FPR at a threshold t defined by "pred >= t"
  # TP_ge(t) = pos_ge[idx2], idx2 = k2_lt + 1 if k2_lt < N else 1 past end (we'll guard)
  idx2 <- pmin.int(k2_lt + 1L, N)  # vector length L2
  TP_ge_vec <- ifelse(k2_lt < N, pos_ge[idx2], 0.0)
  FP_ge_vec <- ifelse(k2_lt < N, neg_ge[idx2], 0.0)
  TPR_t2    <- if (P1 > 0) TP_ge_vec / P1 else NA_real_
  FPR_t2    <- if (N0 > 0) FP_ge_vec / N0 else NA_real_

  # FN/TN in "< t1" region
  FN_lt_vec <- ifelse(k1_lt > 0, as.double(pos_lt[k1_lt]), 0.0)  # length L1
  TN_lt_vec <- ifelse(k1_lt > 0, as.double(neg_lt[k1_lt]), 0.0)

  # TPR/FPR at t1 (using ">= t1"): TP_ge(t1), FP_ge(t1)
  idx1 <- pmin.int(k1_lt + 1L, N)
  TP_ge_t1 <- ifelse(k1_lt < N, pos_ge[idx1], 0.0)
  FP_ge_t1 <- ifelse(k1_lt < N, neg_ge[idx1], 0.0)
  TPR_t1   <- if (P1 > 0) TP_ge_t1 / P1 else NA_real_
  FPR_t1   <- if (N0 > 0) FP_ge_t1 / N0 else NA_real_

  # Prefix at t2 for building reject band counts: pos_lt(t2), neg_lt(t2)
  pos_lt_t2 <- ifelse(k2_lt > 0, as.double(pos_lt[k2_lt]), 0.0)  # length L2
  neg_lt_t2 <- ifelse(k2_lt > 0, as.double(neg_lt[k2_lt]), 0.0)

  # Cross all (t1,t2) pairs, but keep only t1 <= t2
  grid <- data.table::CJ(t1 = LowerThresholds, t2 = UpperThresholds, sorted = FALSE)
  grid <- grid[t1 <= t2]

  if (!nrow(grid)) return(data.table::data.table())

  # Map indices into the precomputed vectors
  # (match allows us to fetch the precomputed k1_lt/k2_lt/metrics by threshold value)
  # For speed, build hash maps once:
  idxL <- match(grid$t1, LowerThresholds)
  idxU <- match(grid$t2, UpperThresholds)

  # Auto-negative region (pred < t1): TN, FN
  TN_auto <- TN_lt_vec[idxL]
  FN_auto <- FN_lt_vec[idxL]

  # Auto-positive region (pred >= t2): TP, FP
  TP_auto <- TP_ge_vec[idxU]
  FP_auto <- FP_ge_vec[idxU]

  # Reject (t1 <= pred < t2): counts by difference of prefixes at t2 and t1
  # pos in reject = pos_lt(t2) - pos_lt(t1); neg in reject = neg_lt(t2) - neg_lt(t1)
  # We need pos_lt(t1) and neg_lt(t1) too:
  pos_lt_t1 <- ifelse(k1_lt > 0, as.double(pos_lt[k1_lt]), 0.0)
  neg_lt_t1 <- ifelse(k1_lt > 0, as.double(neg_lt[k1_lt]), 0.0)

  RejectPos <- pos_lt_t2[idxU] - pos_lt_t1[idxL]
  RejectNeg <- neg_lt_t2[idxU] - neg_lt_t1[idxL]

  # Sanity: sizes
  PredAutoNeg <- as.double(k1_lt[idxL])                 # count(pred < t1)
  PredAutoPos <- as.double(N - k2_lt[idxU])             # count(pred >= t2)
  PredReject  <- as.double(k2_lt[idxU] - k1_lt[idxL])   # count(t1 <= pred < t2)

  # Review (reject) rate per paper: r_n*(FPR(t1)-FPR(t2)) + r_p*(TPR(t1)-TPR(t2))
  review_rate <- r_n * (FPR_t1[idxL] - FPR_t2[idxU]) + r_p * (TPR_t1[idxL] - TPR_t2[idxU])

  # Utility surface per Kruchten:
  # U(t1,t2) = u_fn * (r_p * (1 - TPR(t1))) + u_fp * (r_n * FPR(t2))
  #          + u_r  * (  r_n*(FPR(t1)-FPR(t2)) + r_p*(TPR(t1)-TPR(t2))  )
  u_tp <- CostMatrix[1L]; u_fn <- CostMatrix[2L]; u_fp <- CostMatrix[3L]; u_tn <- CostMatrix[4L]
  # (u_tp/u_tn are not in the paper’s simplified expression; kept for signature parity.)
  UtilitySurface <- (u_fn * (r_p * (1 - TPR_t1[idxL]))) +
    (u_fp * (r_n * FPR_t2[idxU])) +
    (RejectUtility * review_rate)

  # Output table
  out <- data.table::data.table(
    LowerThreshold = grid$t1,
    UpperThreshold = grid$t2,

    # Region sizes
    PredAutoNeg = PredAutoNeg,
    PredReject  = PredReject,
    PredAutoPos = PredAutoPos,

    # Region confusion counts
    TN_auto = TN_auto,
    FN_auto = FN_auto,
    RejectNeg = RejectNeg,
    RejectPos = RejectPos,
    FP_auto = FP_auto,
    TP_auto = TP_auto,

    # Rates at t1 and t2 (useful for plotting/validating)
    TPR_t1 = TPR_t1[idxL],
    FPR_t1 = FPR_t1[idxL],
    TPR_t2 = TPR_t2[idxU],
    FPR_t2 = FPR_t2[idxU],

    # Workload proxy (fraction sent to review)
    ReviewRate = review_rate,

    # Utility surface
    UtilitySurface = UtilitySurface
  )

  # (Optional) you could also compute "overall" Accuracy over auto-decisions only:
  # AutoAccuracy = (TP_auto + TN_auto) / pmax(PredAutoNeg + PredAutoPos, 1)
  out[, AutoAccuracy := (TP_auto + TN_auto) / pmax(PredAutoNeg + PredAutoPos, 1)]

  out[]
}


#' @title RemixClassificationMetrics
#'
#' @description RemixClassificationMetrics
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param TargetVariable Name of your target variable
#' @param Thresholds seq(0.01,0.99,0.01),
#' @param CostMatrix c(1,0,0,1) c(TP utility, FN utility, FP utility, TN utility)
#' @param ClassLabels c(1,0),
#' @param ValidationData. Test data
#' @examples
#' \dontrun{
#' RemixClassificationMetrics <- function(
#'   TargetVariable = "Adrian",
#'   Thresholds = seq(0.01,0.99,0.01),
#'   CostMatrix = c(1,0,0,1),
#'   ClassLabels = c(1,0),
#'   ValidationData. = ValidationData)
#' }
#' @noRd
RemixClassificationMetrics <- function(TargetVariable = NULL,
                                       Thresholds = seq(0.01,0.99,0.01),
                                       CostMatrix = c(0,-1,-1,0),
                                       ClassLabels = c(1,0),
                                       ValidationData. = NULL) {

  # Create metrics
  if(!"p1" %chin% names(ValidationData.)) data.table::setnames(ValidationData., "Predict", "p1")
  temp <- ClassificationMetrics(
    TestData = ValidationData.,
    Target = eval(TargetVariable),
    PredictColumnName = "p1",
    Thresholds = Thresholds,
    PositiveOutcome = ClassLabels[1L],
    NegativeOutcome = ClassLabels[2L],
    CostMatrix = CostMatrix)
  if(temp[,.N] > 95) data.table::setorderv(temp, cols = "MCC", order = -1L, na.last = TRUE)

  # Return values----
  return(temp)
}

#' @title BinaryMetrics
#'
#' @description Compute binary metrics and save them to file
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param ClassWeights. = ClassWeights
#' @param CostMatrixWeights. = CostMatrixWeights
#' @param SaveModelObjects. = SaveModelObjects
#' @param ValidationData. = ValidationData
#' @param TrainOnFull. = TrainOnFull
#' @param TargetColumnName. = TargetColumnName
#' @param ModelID. = ModelID
#' @param model_path. = model_path
#' @param metadata_path. = metadata_path
#' @param Method 'threshold' for 0.01 to 0.99 by 0.01 thresholds or 'bins' for 20 equally sized bins
#'
#' @noRd
BinaryMetrics <- function(ClassWeights. = ClassWeights,
                          CostMatrixWeights. = CostMatrixWeights,
                          SaveModelObjects. = SaveModelObjects,
                          ValidationData. = ValidationData,
                          TrainOnFull. = TrainOnFull,
                          TargetColumnName. = TargetColumnName,
                          ModelID. = ModelID,
                          model_path. = model_path,
                          metadata_path. = metadata_path,
                          Method = "threshold") {
  if(is.null(CostMatrixWeights.)) CostMatrixWeights. <- c(0,-1,-1,0)
  if(Method == "threshold") {
    vals <- seq(0.01,0.99,0.01)
  } else if(Method == "bins") {
    temp <- ValidationData.$p1
    vals <- quantile(temp, probs = seq(0.05,1,0.05), type = 7)
  }
  if(SaveModelObjects. && !TrainOnFull.) {
    EvalMetrics <- RemixClassificationMetrics(TargetVariable = eval(TargetColumnName.), Thresholds = unique(vals), CostMatrix = CostMatrixWeights., ClassLabels = c(1,0), ValidationData. = ValidationData.)
  } else {
    EvalMetrics <- RemixClassificationMetrics(TargetVariable = eval(TargetColumnName.), Thresholds = unique(vals), CostMatrix = CostMatrixWeights., ClassLabels = c(1,0), ValidationData. = ValidationData.)
  }
  EvalMetrics[, P_Predicted := TP + FP]
  data.table::setcolorder(EvalMetrics, c(1,ncol(EvalMetrics),2:(ncol(EvalMetrics)-1)))
  data.table::setcolorder(EvalMetrics, c(1:8, ncol(EvalMetrics), 9:10, 17:19, 11:16, 20:(ncol(EvalMetrics)-1)))
  data.table::setcolorder(EvalMetrics, c(1:14, ncol(EvalMetrics), 15:(ncol(EvalMetrics)-1)))
  data.table::setorderv(EvalMetrics, "Utility", -1)
  return(EvalMetrics)
}

#' @title RegressionMetrics
#'
#' @description Compute regression metrics and save them to file
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param SaveModelObjects. = SaveModelObjects
#' @param data. = data
#' @param ValidationData. = ValidationData
#' @param TrainOnFull. = TrainOnFull
#' @param LossFunction. = LossFunction
#' @param EvalMetric. = EvalMetric
#' @param TargetColumnName. = TargetColumnName
#' @param ModelID. = ModelID
#' @param model_path. = model_path
#' @param metadata_path. = metadata_path
#'
#' @noRd
RegressionMetrics <- function(SaveModelObjects. = SaveModelObjects,
                              data. = data,
                              ValidationData. = ValidationData,
                              TrainOnFull. = TrainOnFull,
                              LossFunction. = LossFunction,
                              EvalMetric. = EvalMetric,
                              TargetColumnName. = TargetColumnName,
                              ModelID. = ModelID,
                              model_path. = model_path,
                              metadata_path. = metadata_path) {

  if(!TrainOnFull. && (!is.null(LossFunction.) && LossFunction. != "MultiRMSE") || (!is.null(EvalMetric.) && EvalMetric. != "MultiRMSE")) {
    EvaluationMetrics <- data.table::data.table(Metric = c("MAE","MAPE","RMSE","R2"), MetricValue = rep(999999, 4L))
    i <- 0L
    # metric <- 'r2'
    for(metric in c("mae", "mape", "rmse", "r2")) {# metric = "r2"
      i <- i + 1L
      if(tolower(metric) == "mae") {
        ValidationData.[, Metric := abs(get(TargetColumnName.) - Predict)]
        MetricVal <- ValidationData.[, mean(Metric, na.rm = TRUE)]
      } else if(tolower(metric) == "mape") {
        ValidationData.[, Metric := abs((get(TargetColumnName.) - Predict) / (get(TargetColumnName.) + 1))]
        MetricVal <- ValidationData.[, mean(Metric, na.rm = TRUE)]
      } else if(tolower(metric) == "rmse") {
        ValidationData.[, Metric := (get(TargetColumnName.) - Predict) ^ 2]
        MetricVal <- sqrt(ValidationData.[, mean(Metric, na.rm = TRUE)])
      } else if(tolower(metric) == "r2") {
        ValidationData.[, ':=' (Metric1 = (get(TargetColumnName.) - data.[, mean(get(TargetColumnName.))]) ^ 2, Metric2 = (get(TargetColumnName.) - Predict) ^ 2)]
        MetricVal <- 1 - ValidationData.[, sum(Metric2, na.rm = TRUE)] / ValidationData.[, sum(Metric1, na.rm = TRUE)]
      }
      data.table::set(EvaluationMetrics, i = i, j = 2L, value = round(MetricVal, 4L))
    }

    # Save EvaluationMetrics to File
    EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  } else {
    EvaluationMetrics <- list()

    # Loop through Target Variables
    for(TV in seq_along(TargetColumnName.)) {

      # Eval Metrics
      EvaluationMetrics[[TargetColumnName.[TV]]] <- data.table::data.table(Metric = c("MAE","MAPE","RMSE","R2"), MetricValue = rep(999999, 4L))
      i <- 0L
      for(metric in c("mae", "mape", "rmse", "r2")) {
        i <- i + 1L
        tryCatch({
          if(tolower(metric) == "mae") {
            ValidationData.[, Metric := abs(ValidationData.[[eval(TargetColumnName.[TV])]] - ValidationData.[[eval(paste0("Predict.V", TV))]])]
            MetricVal <- ValidationData.[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(metric) == "mape") {
            ValidationData.[, Metric := abs((ValidationData.[[eval(TargetColumnName.[TV])]] - ValidationData.[[eval(paste0("Predict.V", TV))]]) / (ValidationData.[[eval(TargetColumnName.[TV])]] + 1))]
            MetricVal <- ValidationData.[, mean(Metric, na.rm = TRUE)]
          } else if(tolower(metric) == "rmse") {
            ValidationData.[, Metric := (ValidationData.[[eval(TargetColumnName.[TV])]] - ValidationData.[[eval(paste0("Predict.V", TV))]]) ^ 2]
            MetricVal <- sqrt(ValidationData.[, mean(Metric, na.rm = TRUE)])
          } else if(tolower(metric) == "r2") {
            ValidationData.[, ':=' (Metric1 = (ValidationData.[[eval(TargetColumnName.[TV])]] - data.[, mean(get(TargetColumnName.[TV]))]) ^ 2, Metric2 = (ValidationData.[[eval(TargetColumnName.[TV])]] - ValidationData.[[eval(paste0("Predict.V",TV))]]) ^ 2)]
            MetricVal <- 1 - ValidationData.[, sum(Metric2, na.rm = TRUE)] / ValidationData.[, sum(Metric1, na.rm = TRUE)]
          }
          data.table::set(EvaluationMetrics[[TargetColumnName.[TV]]], i = i, j = 2L, value = round(MetricVal, 4L))
        }, error = function(x) "skip")
      }

      # Save EvaluationMetrics to File
      EvaluationMetrics[[TargetColumnName.[TV]]] <- EvaluationMetrics[[TargetColumnName.[TV]]][MetricValue != 999999]
    }
  }

  # Remove Columns
  if("Metric" %chin% names(ValidationData.)) data.table::set(ValidationData., j = "Metric", value = NULL)
  if("Metric1" %chin% names(ValidationData.)) data.table::set(ValidationData., j = "Metric1", value = NULL)
  if("Metric2" %chin% names(ValidationData.)) data.table::set(ValidationData., j = "Metric2", value = NULL)

  return(EvaluationMetrics)
}


#' @title MultiClassMetrics
#'
#' @description Compute regression metrics and save them to file
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param ModelClass "catboost"
#' @param DataType "test" or "train"
#' @param SaveModelObjects. = SaveModelObjects
#' @param ValidationData. = ValidationData
#' @param PredictData. = predict
#' @param TrainOnFull. = TrainOnFull
#' @param TargetColumnName. = TargetColumnName
#' @param TargetLevels. = TargetLevels
#' @param ModelID. = ModelID
#' @param model_path. = model_path
#' @param metadata_path. = metadata_path
#' @param Debug = FALSE
#'
#' @noRd
MultiClassMetrics <- function(ModelClass = "catboost",
                              DataType = "test",
                              SaveModelObjects. = SaveModelObjects,
                              ValidationData. = ValidationData,
                              PredictData. = predict,
                              TrainOnFull. = TrainOnFull,
                              TargetColumnName. = TargetColumnName,
                              TargetLevels. = TargetLevels,
                              ModelID. = ModelID,
                              model_path. = NULL,
                              metadata_path. = NULL,
                              Debug = FALSE) {

  # Convert Target Variable back to Categorical
  if(Debug) print("# Convert Target Variable back to Categorical")
  if(is.numeric(ValidationData.[[TargetColumnName.]])) {
    data.table::setkeyv(ValidationData., TargetColumnName.)
    data.table::setkeyv(TargetLevels., 'NewLevels')
    ValidationData.[TargetLevels., OriginalLevels := i.OriginalLevels][, eval(TargetColumnName.) := OriginalLevels][, OriginalLevels := NULL]
  }

  # MultiClass Metrics Accuracy
  if(Debug) print("# MultiClass Metrics Accuracy")
  MetricAcc <- ValidationData.[, mean(data.table::fifelse(as.character(get(TargetColumnName.)) == as.character(Predict), 1.0, 0.0), na.rm = TRUE)]

  # MultiClass Metrics MicroAUC Setup
  if(Debug) print("# MultiClass Metrics MicroAUC Setup")
  Response <- ValidationData.[[eval(TargetColumnName.)]]
  if(ModelClass == "catboost") {
    Predictor <- as.matrix(ValidationData.[, .SD, .SDcols = unique(as.character(TargetLevels.[['OriginalLevels']]))])
  } else if(ModelClass == "h2o") {
    Predictor <- as.matrix(ValidationData.[, .SD, .SDcols = unique(names(ValidationData.)[(ncol(ValidationData.) + 1 - length(TargetLevels.)):(ncol(ValidationData.))])])
  } else if(ModelClass == "xgboost") {
    Predictor <- as.matrix(ValidationData.[, .SD, .SDcols = c(unique(as.character(TargetLevels.[["OriginalLevels"]])))])
  }

  # Generate metric
  if(Debug) print("# Generate metric")
  MetricAUC <- round(as.numeric(noquote(stringr::str_extract(pROC::multiclass.roc(response = Response, predictor = Predictor)$auc, "\\d+\\.*\\d*"))), 4L)

  # Logloss
  if(Debug) print("# Logloss")
  if(!data.table::is.data.table(TargetLevels.)) N <- length(TargetLevels.) else N <- TargetLevels.[, .N]
  temp <- ValidationData.[, .SD, .SDcols = c(TargetColumnName., "Predict")]
  temp <- Rodeo::DummifyDT(data=temp, cols=eval(TargetColumnName.), KeepFactorCols=FALSE, OneHot=FALSE, SaveFactorLevels=FALSE, SavePath=NULL, ImportFactorLevels=FALSE, FactorLevelsList=NULL, ClustScore=FALSE, ReturnFactorLevels=FALSE)
  if(ModelClass == "xgboost") {
    if(Debug) print("# ModelClass == xgboost")
    logloss <- MLmetrics::LogLoss(
      y_pred = as.matrix(ValidationData.[, .SD, .SDcols = c(as.character(TargetLevels.[["OriginalLevels"]]))]),
      y_true = as.matrix(temp[, .SD, .SDcols = c(names(temp)[c(2L:(1L+N))])]))
  } else if(ModelClass == "catboost") {
    if(Debug) print("# ModelClass == catboost")
    temp <- Rodeo::DummifyDT(data=temp, cols='Predict', KeepFactorCols=FALSE, OneHot=FALSE, SaveFactorLevels=FALSE, SavePath=NULL, ImportFactorLevels=FALSE, FactorLevelsList=NULL, ClustScore=FALSE, ReturnFactorLevels=FALSE)
    logloss <- MLmetrics::LogLoss(
      y_pred = as.matrix(ValidationData.[, .SD, .SDcols = unique(as.character(TargetLevels.[['OriginalLevels']]))]),
      y_true = as.matrix(temp[, .SD, .SDcols = c(names(temp)[seq_len(N)])]))
  } else if(ModelClass == "h2o") {
    if(Debug) print("# ModelClass == h2o")
    logloss <- MLmetrics::LogLoss(
      y_pred = as.matrix(ValidationData.[, .SD, .SDcols = c(unique(names(ValidationData.)[(ncol(ValidationData.) + 1 - length(TargetLevels.)):(ncol(ValidationData.))]))]),
      y_true = as.matrix(temp[, .SD, .SDcols = c(names(temp)[c(2L:(1L+N))])]))
  }

  # MCC for MultiClass
  if(Debug) print("# MCC for MultiClass")
  ConfusionMatrix <- table(ValidationData.$Predict, ValidationData.[[eval(TargetColumnName.)]])
  c <- sum(diag(ConfusionMatrix))
  s <- sum(ConfusionMatrix)

  # MCC
  if(ModelClass != 'h2o') {

    # pk * tk
    if(Debug) print("# pk * tk")
    sumPkTk <- c()
    for(i in as.character(TargetLevels.[['OriginalLevels']])) {
      pk <- ValidationData.[Predict == eval(i), .N]
      tk <- ValidationData.[get(TargetColumnName.) == eval(i), .N]
      sumPkTk <- c(sumPkTk, pk/100 * tk*100)
    }

    # (s^2 - sum(pk^2))
    if(Debug) print("# (s^2 - sum(pk^2))")
    sumPk2 <- c()
    for(i in as.character(TargetLevels.[['OriginalLevels']])) {
      pk <- ValidationData.[Predict == eval(i), .N]
      sumPk2 <- c(sumPk2, pk ^ 2)
    }

    # (s^2 - sum(tk^2))
    if(Debug) print("# (s^2 - sum(tk^2))")
    sumTk2 <- c()
    for(i in as.character(TargetLevels.[['OriginalLevels']])) {
      tk <- ValidationData.[get(TargetColumnName.) == eval(i), .N]
      sumTk2 <- c(sumTk2, tk ^ 2)
    }

    # Result
    if(Debug) print("# Result")
    denom <- sqrt((s^2 - sum(sumPk2))) * sqrt((s^2 - sum(sumTk2)))
    MCC <- (c / denom * s  - sum(sumPkTk) / denom)
  } else {
    MCC <- NA_real_
  }

  # MultiClass Evaluation Metrics
  if(Debug) print("# MultiClass Evaluation Metrics")
  EvaluationMetrics <- data.table::data.table(Metric = c("MCC","MicroAUC","Accuracy","LogLoss"), MetricValue = c(MCC, MetricAUC, MetricAcc, logloss))
  if(SaveModelObjects.) {
    if(!is.null(metadata_path.)) {
      if(tolower(DataType) == "train") data.table::fwrite(EvaluationMetrics, file = file.path(metadata_path., paste0(ModelID., "_Train_EvaluationMetrics.csv")))
      if(tolower(DataType) == "test") data.table::fwrite(EvaluationMetrics, file = file.path(metadata_path., paste0(ModelID., "_Test_EvaluationMetrics.csv")))
    }
  }
  if(Debug) print("# Return")
  return(EvaluationMetrics)
}
