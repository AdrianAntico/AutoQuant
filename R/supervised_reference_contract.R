# Language-independent supervised reference semantics and CatBoost conformance.

#' Supervised Reference Contract 1.1
#'
#' @param task Supervised task: `"regression"` or `"binary"`.
#' @return A language-independent supervised capability contract.
#' @export
aq_supervised_reference_contract <- function(task = c("regression", "binary")) {
  task <- match.arg(task)
  common_core <- c(
    "model_revision", "fitted_state", "feature_contract", "partition_identity",
    "implementation_descriptor", "resource_evidence", "training_status",
    "predictions", "observation_counts", "missing_excluded_counts", "lineage"
  )
  common_extended <- c(
    "permutation_importance", "partial_dependence", "ice", "local_explanations",
    "subgroup_diagnostics", "sensitivity_evidence"
  )
  evidence <- if (identical(task, "regression")) list(
    core = c(common_core, "residuals", "rmse", "mae", "mse", "r2", "bias"),
    derived = c(
      "residual_summary", "residual_quantiles", "prediction_distribution",
      "observed_distribution", "observed_vs_predicted", "feature_importance",
      "training_history"
    ),
    extended = c(common_extended, "shap_summary", "shap_values", "error_slices"),
    specialized = c("shap_interactions", "resampling_uncertainty", "prediction_intervals")
  ) else list(
    core = c(
      common_core, "positive_probability", "decision_policy_reference",
      "confusion_matrix", "class_balance", "logloss", "roc_auc", "brier_score"
    ),
    derived = c(
      "roc_curve", "precision_recall_curve", "pr_auc", "sensitivity", "specificity",
      "precision", "f1", "balanced_accuracy", "probability_distribution",
      "calibration_curve", "calibration_intercept_slope", "lift_gain",
      "threshold_table", "feature_importance", "training_history"
    ),
    extended = c(
      common_extended, "shap_summary", "shap_values", "subgroup_calibration",
      "threshold_cost_curve", "decision_curve"
    ),
    specialized = c("shap_interactions", "resampling_calibration", "conformal_probability_bounds")
  )
  structure(list(
    contract_id = "autoquant.supervised.reference",
    capability_id = paste0("supervised.", task),
    contract_version = "1.1.0",
    schema_version = "aq_supervised_reference_contract_v1_1",
    task = task,
    purpose = if (identical(task, "regression"))
      "Estimate a continuous response from governed predictors." else
      "Estimate positive-class probability from governed predictors; decisions are separate policies.",
    input_contract = list(
      exact_dataset_revision = TRUE,
      required_roles = c("outcome", "predictor"),
      optional_roles = "case_weight",
      feature_types = c("numeric", "categorical", "logical", "ordered", "temporal_when_supported"),
      preserve_feature_order = TRUE,
      preserve_categorical_identity = TRUE,
      missingness_must_be_explicit = TRUE,
      leakage_policy_required = TRUE,
      partitions = c("train", "validation", "future:test", "future:cross_validation", "future:grouped", "future:temporal")
    ),
    output_contract = list(
      model_revision = TRUE,
      probabilities_are_model_output = identical(task, "binary"),
      decisions_are_separate_policy = identical(task, "binary"),
      evidence_tiers = evidence,
      progressive_disclosure = TRUE,
      retained_computational_sufficiency = TRUE
    ),
    application_modes = if (identical(task, "binary")) c("probability", "decision_policy") else "predict",
    failure_vocabulary = c(
      "invalid_contract", "invalid_task", "invalid_outcome", "invalid_features",
      "schema_mismatch", "feature_type_mismatch", "missingness_unsupported",
      "invalid_partition", "leakage_detected", "implementation_unavailable",
      "resource_limit", "fit_failed", "application_incompatible", "evidence_unavailable"
    ),
    universal_semantics = c(
      "exact revision and partition identity", "immutable fitted state",
      "typed validation", "probability/decision separation", "evidence inventory",
      "resource and implementation provenance", "no silent implementation fallback"
    ),
    implementation_dimensions = c("algorithm_family", "engine", "ensemble_regime"),
    engine_specific_extension_allowed = TRUE,
    h2o_allowed = FALSE
  ), class = c("aq_supervised_reference_contract", "list"))
}

#' CatBoost vNext Implementation Descriptor
#'
#' @param task Supervised task.
#' @param engine_params Optional resolved CatBoost parameters.
#' @return A governed implementation descriptor.
#' @export
aq_catboost_implementation_descriptor <- function(task = c("regression", "binary"), engine_params = list()) {
  task <- match.arg(task)
  params <- aq_vnext_engine_params(engine_params, task = task)
  structure(list(
    implementation_id = paste0("autoquant.catboost.", task, ".r"),
    capability_id = paste0("supervised.", task),
    capability_version = "1.0.0",
    descriptor_version = "aq_implementation_descriptor_v1",
    contract_owner = "AutoQuant",
    implementation_package = "AutoQuant",
    language = "R",
    runtime = "r-package-worker",
    engine = "catboost",
    algorithm_family = "tree_ensemble",
    ensemble_regime = "boosting",
    qualified_ensemble_regimes = c("boosting", "stochastic_boosting"),
    engine_package_version = if (requireNamespace("catboost", quietly = TRUE))
      as.character(utils::packageVersion("catboost")) else NA_character_,
    hardware = tolower(params$task_type %||% "CPU"),
    cpu_gpu = toupper(params$task_type %||% "CPU"),
    thread_count = as.integer(params$thread_count %||% 1L),
    parallelism = if ((params$thread_count %||% 1L) > 1L) "multithreaded" else "single_threaded",
    categorical_support = "native_catboost_categorical_features",
    missingness = "catboost_native_numeric_missingness; categorical missingness requires explicit value representation",
    fitted_state_format = "catboost_r_model_in_autoquant_bundle",
    application_modes = if (identical(task, "binary")) c("probability", "decision_policy") else "predict",
    determinism = list(
      seedable = TRUE,
      seed = params$random_seed,
      cpu_reproducibility = "seed_and_thread_policy_recorded",
      gpu_difference = "GPU is not assumed numerically or deterministically identical to CPU"
    ),
    resource_model = list(
      memory = "in_memory_catboost_pool_and_fitted_model",
      batch_application = TRUE,
      sparse_support = "engine_dependent_not_qualified_by_this_descriptor",
      gpu_optional = TRUE,
      gpu_qualification = "not_qualified_in_reference_wave_1"
    ),
    engine_specific_semantics = c(
      "ordered target statistics", "native categorical handling", "native numeric missing values",
      "evaluation set and early stopping", "CatBoost importance and SHAP", "CPU/GPU parameter differences"
    ),
    h2o_dependency = FALSE,
    fallback_policy = "no_h2o_fallback",
    failure_behavior = "fail_typed; never fall back to H2O or another engine"
  ), class = c("aq_implementation_descriptor", "list"))
}

#' CatBoost vNext Tuning Space
#'
#' @param task Supervised task.
#' @return A machine-readable governed tuning-space table.
#' @export
aq_catboost_tuning_space <- function(task = c("regression", "binary")) {
  task <- match.arg(task)
  data.table::data.table(
    parameter = c("iterations", "learning_rate", "depth", "l2_leaf_reg", "random_strength", "bootstrap_type", "thread_count", "task_type"),
    type = c("integer", "double", "integer", "double", "double", "categorical", "integer", "categorical"),
    lower = c(25, 0.005, 3, 1e-3, 0, NA, 1, NA),
    upper = c(5000, 0.5, 12, 100, 20, NA, parallel::detectCores(logical = TRUE), NA),
    scale = c("linear", "log", "linear", "log", "linear", "options", "linear", "options"),
    options = c(NA, NA, NA, NA, NA, "Bayesian|Bernoulli|MVS|No", NA, "CPU|GPU"),
    condition = c(NA, NA, NA, NA, NA, "subsample validity depends on bootstrap_type and task_type", NA, "GPU requires qualified hardware/runtime"),
    resource_implication = c("training duration/model size", "iterations required", "memory/compute", "regularization", "stochasticity", "sampling behavior", "CPU contention", "hardware/determinism/parameter support"),
    default_qualification = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
    task = task
  )
}

aq_supervised_training_history <- function(model) {
  history <- tryCatch(catboost::catboost.get_evals_result(model), error = function(e) NULL)
  best_iteration <- tryCatch(catboost::catboost.get_best_iteration(model), error = function(e) NA_integer_)
  best_score <- tryCatch(catboost::catboost.get_best_score(model), error = function(e) NULL)
  list(history = history, best_iteration = best_iteration, best_score = best_score)
}

aq_supervised_feature_importance <- function(fit) {
  values <- tryCatch(catboost::catboost.get_feature_importance(fit$model, type = "FeatureImportance"), error = function(e) NULL)
  if (is.null(values) || length(values) != length(fit$spec$features)) return(data.table::data.table())
  data.table::data.table(feature = fit$spec$features, importance = as.numeric(values))[order(-importance, feature)]
}

aq_supervised_roc_curve <- function(actual, probability) {
  keep <- stats::complete.cases(actual, probability)
  actual <- as.integer(actual[keep]); probability <- as.numeric(probability[keep])
  thresholds <- sort(unique(c(Inf, probability, -Inf)), decreasing = TRUE)
  rows <- lapply(thresholds, function(threshold) {
    predicted <- probability >= threshold
    tp <- sum(actual == 1L & predicted); fp <- sum(actual == 0L & predicted)
    fn <- sum(actual == 1L & !predicted); tn <- sum(actual == 0L & !predicted)
    data.table::data.table(threshold = threshold,
      tpr = if (tp + fn) tp / (tp + fn) else NA_real_,
      fpr = if (fp + tn) fp / (fp + tn) else NA_real_)
  })
  data.table::rbindlist(rows)
}

aq_supervised_pr_curve <- function(actual, probability) {
  keep <- stats::complete.cases(actual, probability)
  actual <- as.integer(actual[keep]); probability <- as.numeric(probability[keep])
  thresholds <- sort(unique(c(Inf, probability, -Inf)), decreasing = TRUE)
  rows <- lapply(thresholds, function(threshold) {
    predicted <- probability >= threshold
    tp <- sum(actual == 1L & predicted); fp <- sum(actual == 0L & predicted); fn <- sum(actual == 1L & !predicted)
    data.table::data.table(threshold = threshold,
      precision = if (tp + fp) tp / (tp + fp) else 1,
      recall = if (tp + fn) tp / (tp + fn) else NA_real_)
  })
  data.table::rbindlist(rows)
}

aq_supervised_curve_auc <- function(x, y) {
  keep <- is.finite(x) & is.finite(y); x <- x[keep]; y <- y[keep]
  if (length(x) < 2L) return(NA_real_)
  o <- order(x, y); x <- x[o]; y <- y[o]
  sum(diff(x) * (head(y, -1L) + tail(y, -1L)) / 2)
}

aq_supervised_threshold_table <- function(actual, probability, thresholds = seq(0.05, 0.95, by = 0.05), costs = list(false_positive = 1, false_negative = 1)) {
  rows <- lapply(thresholds, function(threshold) {
    predicted <- probability >= threshold
    tp <- sum(actual == 1 & predicted, na.rm = TRUE); tn <- sum(actual == 0 & !predicted, na.rm = TRUE)
    fp <- sum(actual == 0 & predicted, na.rm = TRUE); fn <- sum(actual == 1 & !predicted, na.rm = TRUE)
    div <- function(a, b) if (b > 0) a / b else NA_real_
    data.table::data.table(threshold = threshold, tp = tp, tn = tn, fp = fp, fn = fn,
      precision = div(tp, tp + fp), recall = div(tp, tp + fn), specificity = div(tn, tn + fp),
      f1 = div(2 * tp, 2 * tp + fp + fn),
      expected_cost = fp * costs$false_positive + fn * costs$false_negative)
  })
  data.table::rbindlist(rows)
}

aq_supervised_evidence_manifest <- function(fit, assessment = NULL) {
  contract <- aq_supervised_reference_contract(fit$task)
  tiers <- contract$output_contract$evidence_tiers
  materialized <- unique(c(
    "model_revision", "fitted_state", "feature_contract", "partition_identity",
    "implementation_descriptor", "resource_evidence", "training_status", "training_history",
    if (!is.null(assessment)) names(assessment$tables %||% list()) else character()
  ))
  rows <- lapply(names(tiers), function(tier) lapply(tiers[[tier]], function(type) {
    is_materialized <- type %in% materialized
    data.table::data.table(
      evidence_id = paste(fit$fit_id, type, sep = ":"), evidence_type = type,
      tier = toupper(tier), availability = if (is_materialized) "available" else "contract_available",
      state = if (is_materialized) "materialized" else if (tier %in% c("core", "derived")) "derivable" else "on_demand",
      cost_class = switch(tier, core = "low", derived = "low", extended = "medium", specialized = "high"),
      requires_refit = FALSE, artifact_reference = NA_character_, dependencies = fit$fit_id
    )
  }))
  data.table::rbindlist(unlist(rows, recursive = FALSE), use.names = TRUE, fill = TRUE)
}

aq_supervised_complete_assessment <- function(fit, predictions, assessment) {
  if (is.null(fit) || !inherits(fit, "aq_fit_result")) return(assessment)
  data <- data.table::as.data.table(data.table::copy(predictions$data))
  feature_importance <- aq_supervised_feature_importance(fit)
  assessment$tables$feature_importance <- feature_importance
  if (identical(fit$task, "regression")) {
    residual <- as.numeric(data[[predictions$target_col]]) - as.numeric(data[[predictions$prediction_col]])
    assessment$tables$residual_summary <- aq_vnext_numeric_summary(residual, "residual")
    assessment$tables$residual_quantiles <- data.table::data.table(
      probability = c(0, .01, .05, .25, .5, .75, .95, .99, 1),
      residual = as.numeric(stats::quantile(residual, c(0, .01, .05, .25, .5, .75, .95, .99, 1), na.rm = TRUE, names = FALSE))
    )
    assessment$tables$prediction_distribution <- aq_vnext_numeric_summary(data[[predictions$prediction_col]], "prediction")
    assessment$tables$observed_distribution <- aq_vnext_numeric_summary(data[[predictions$target_col]], "observed")
  } else {
    actual <- as.integer(as.character(data[[predictions$target_col]]) == as.character(predictions$positive_class))
    probability <- as.numeric(data[[predictions$probability_col]])
    roc <- aq_supervised_roc_curve(actual, probability)
    pr <- aq_supervised_pr_curve(actual, probability)
    thresholds <- aq_supervised_threshold_table(actual, probability)
    assessment$tables$roc_curve <- roc
    assessment$tables$precision_recall_curve <- pr
    assessment$tables$threshold_table <- thresholds
    assessment$tables$probability_distribution <- aq_vnext_numeric_summary(probability, "positive_probability")
    brier <- mean((probability - actual)^2, na.rm = TRUE)
    pr_auc <- aq_supervised_curve_auc(pr$recall, pr$precision)
    current <- assessment$metrics
    cm <- thresholds[which.min(abs(threshold - predictions$threshold_policy$threshold))]
    balanced <- mean(c(cm$recall, cm$specificity), na.rm = TRUE)
    assessment$metrics <- data.table::rbindlist(list(current,
      data.table::data.table(metric = c("brier_score", "pr_auc", "balanced_accuracy"), value = c(brier, pr_auc, balanced))))
    p <- pmin(pmax(probability, 1e-6), 1 - 1e-6)
    cal <- tryCatch(stats::coef(stats::glm(actual ~ stats::qlogis(p), family = stats::binomial())), error = function(e) c(NA_real_, NA_real_))
    assessment$tables$calibration_intercept_slope <- data.table::data.table(
      calibration_intercept = unname(cal[1L]), calibration_slope = unname(cal[2L]),
      brier_score = brier, logloss = assessment$metrics[metric == "logloss", value][1L]
    )
  }
  assessment$reference_contract <- fit$reference_contract
  assessment$implementation_descriptor <- fit$implementation_descriptor
  assessment$tuning_space <- fit$tuning_space
  assessment$evidence_manifest <- aq_supervised_evidence_manifest(fit, assessment)
  assessment$retained_computational_sufficiency <- list(
    fitted_model = TRUE, feature_contract = TRUE, evaluation_rows = TRUE,
    training_history = TRUE, raw_training_rows_removed_from_saved_bundle = TRUE,
    extended_evidence_without_refit = c("feature_importance", "SHAP", "PDP", "ICE", "threshold_policy"),
    specialized_may_require_additional_computation = TRUE
  )
  assessment
}

#' QA Supervised Reference Contract and CatBoost Conformance
#'
#' @return A validation table.
#' @export
qa_supervised_reference_contract <- function() {
  regression <- aq_supervised_reference_contract("regression")
  binary <- aq_supervised_reference_contract("binary")
  descriptor <- aq_catboost_implementation_descriptor("binary", list(thread_count = 2L))
  tuning <- aq_catboost_tuning_space("binary")
  source <- paste(vapply(c("aq_fit_model", "aq_predict_model", "aq_score_model", "aq_assess_model"), function(fn)
    paste(deparse(body(get(fn, envir = asNamespace("AutoQuant")))), collapse = "\n"), character(1)), collapse = "\n")
  data.table::data.table(
    check = c("contract_identity", "task_semantics", "probability_first", "evidence_tiers", "descriptor", "parallel_cpu", "tuning_metadata", "no_h2o", "typed_failure_vocabulary"),
    status = c(
      if (identical(regression$contract_version, "1.1.0") &&
          identical(regression$implementation_dimensions,
            c("algorithm_family", "engine", "ensemble_regime"))) "success" else "error",
      if (identical(regression$task, "regression") && identical(binary$task, "binary")) "success" else "error",
      if (isTRUE(binary$output_contract$probabilities_are_model_output) && isTRUE(binary$output_contract$decisions_are_separate_policy)) "success" else "error",
      if (identical(sort(names(binary$output_contract$evidence_tiers)), sort(c("core", "derived", "extended", "specialized")))) "success" else "error",
      if (identical(descriptor$implementation_id, "autoquant.catboost.binary.r") &&
          identical(descriptor$algorithm_family, "tree_ensemble") &&
          identical(descriptor$ensemble_regime, "boosting")) "success" else "error",
      if (descriptor$thread_count > 1L && identical(descriptor$parallelism, "multithreaded")) "success" else "error",
      if (nrow(tuning) >= 8L && all(c("condition", "resource_implication") %in% names(tuning))) "success" else "error",
      if (!grepl("h2o", source, ignore.case = TRUE) && !isTRUE(descriptor$h2o_dependency) && !isTRUE(binary$h2o_allowed)) "success" else "error",
      if (all(c("leakage_detected", "application_incompatible", "evidence_unavailable") %in% binary$failure_vocabulary)) "success" else "error"
    ),
    detail = c("versioned universal contract", "regression and binary", "model probability separated from policy", "CORE/DERIVED/EXTENDED/SPECIALIZED", "CatBoost R descriptor", "thread governance", "typed conditional tuning space", "canonical vNext path contains no H2O", "portable typed failures")
  )
}

#' QA CatBoost Against the Supervised Reference Contract
#'
#' @description Executes controlled regression and binary fits and validates
#' language-independent output, persistence, application, evidence, and policy
#' semantics. The checks intentionally avoid R object-layout assertions.
#' @return A semantic conformance table.
#' @export
qa_catboost_supervised_conformance <- function() {
  rows <- list()
  add <- function(task, check, ok, detail) rows[[length(rows) + 1L]] <<-
    data.table::data.table(task = task, check = check,
      status = if (isTRUE(ok)) "success" else "error", detail = detail)

  regression_data <- aq_vnext_catboost_fixture()
  regression_data[seq(5L, nrow(regression_data), by = 17L), spend := NA_real_]
  regression_spec <- aq_model_spec("regression", "catboost", "revenue",
    c("channel", "region", "spend", "clicks", "discount"),
    partition = aq_partition_spec("time", split_col = "event_date",
      train_fraction = 0.8, seed = 814L),
    engine_params = list(iterations = 40L, depth = 4L, thread_count = 2L),
    seed = 814L, dataset_id = "controlled_regression_revision")
  regression_fit <- aq_fit_model(regression_spec, regression_data)
  regression_prediction <- aq_predict_model(regression_fit, dataset = "validation")
  regression_assessment <- aq_assess_model(regression_fit, regression_prediction)
  regression_bundle <- aq_vnext_create_model_bundle(regression_fit)
  regression_bundle_validation <- aq_validate_model_bundle(regression_bundle)
  regression_restarted <- unserialize(serialize(regression_bundle, NULL, version = 3L))
  class(regression_restarted) <- class(regression_bundle)
  regression_applied <- aq_predict_model(regression_restarted,
    new_data = regression_data[1:20, .(channel, region, spend, clicks, discount)],
    dataset = "new", dataset_id = "controlled_regression_application")
  add("regression", "exact_revision_and_partition",
    identical(regression_fit$spec$dataset_id, "controlled_regression_revision") &&
      nzchar(regression_fit$partition$partition_id), "exact evaluation authority retained")
  add("regression", "native_categorical_and_missingness",
    all(c("channel", "region") %in% regression_fit$feature_contract$categorical_features) &&
      isTRUE(regression_fit$feature_contract$missingness$native_numeric),
    "categorical identity and native numeric missingness retained")
  add("regression", "core_and_derived_evidence",
    all(c("residual_summary", "residual_quantiles", "feature_importance") %in%
      names(regression_assessment$tables)), "regression evidence tables materialized")
  add("regression", "evidence_universe",
    identical(sort(unique(regression_assessment$evidence_manifest$tier)),
      sort(c("CORE", "DERIVED", "EXTENDED", "SPECIALIZED"))),
    "all four evidence tiers discoverable")
  add("regression", "durable_application_without_refit",
    inherits(regression_applied, "aq_prediction_result") &&
      identical(regression_restarted$fit_id, regression_fit$fit_id),
    "serialized exact fitted state applies after restart")
  add("regression", "bundle_contract",
    !any(regression_bundle_validation$status %in% c("fail", "error")),
    "durable bundle validates against reference contract")

  binary_data <- aq_vnext_catboost_binary_fixture()
  binary_data[seq(7L, nrow(binary_data), by = 19L), clicks := NA_real_]
  policy <- aq_threshold_policy(0.4, "yes", false_positive_cost = 2,
    false_negative_cost = 7, prevalence_assumption = 0.2,
    constraints = list(minimum_recall = 0.7))
  binary_spec <- aq_model_spec("binary", "catboost", "converted",
    c("channel", "region", "spend", "clicks", "discount"),
    partition = aq_partition_spec("time", split_col = "event_date",
      train_fraction = 0.8, seed = 815L), threshold_policy = policy,
    engine_params = list(iterations = 40L, depth = 4L, thread_count = 2L),
    seed = 815L, dataset_id = "controlled_binary_revision")
  binary_fit <- aq_fit_model(binary_spec, binary_data)
  binary_prediction <- aq_predict_model(binary_fit, dataset = "validation")
  binary_assessment <- aq_assess_binary_model(binary_fit, binary_prediction)
  alternate <- aq_predict_model(binary_fit, dataset = "validation",
    threshold_policy = aq_threshold_policy(0.7, "yes", "no",
      false_positive_cost = 5, false_negative_cost = 1))
  binary_bundle <- aq_vnext_create_model_bundle(binary_fit)
  binary_bundle_validation <- aq_validate_model_bundle(binary_bundle)
  add("binary", "probability_first",
    "PositiveProbability" %in% names(binary_prediction$data) &&
      all(binary_prediction$data$PositiveProbability >= 0 &
        binary_prediction$data$PositiveProbability <= 1),
    "probability is canonical model output")
  add("binary", "decision_policy_separate",
    identical(binary_prediction$data$PositiveProbability,
      alternate$data$PositiveProbability) &&
      !identical(binary_prediction$threshold_policy$threshold,
        alternate$threshold_policy$threshold),
    "decision policy changes without refit or rescoring")
  add("binary", "calibration_and_discrimination",
    all(c("roc_curve", "precision_recall_curve", "threshold_table",
      "calibration_intercept_slope") %in% names(binary_assessment$tables)) &&
      all(c("brier_score", "pr_auc") %in% binary_assessment$metrics$metric),
    "reusable curve and calibration data materialized")
  add("binary", "governed_cost_policy",
    identical(policy$cost_policy$false_negative, 7) &&
      identical(policy$cost_policy$constraints$minimum_recall, 0.7),
    "cost, prevalence, and constraint metadata retained")
  add("binary", "bundle_contract",
    !any(binary_bundle_validation$status %in% c("fail", "error")),
    "durable bundle validates against reference contract")
  add("both", "parallel_cpu",
    regression_fit$resource_evidence$thread_count == 2L &&
      binary_fit$resource_evidence$thread_count == 2L,
    "qualified fits use governed multithreaded CPU execution")
  add("both", "no_h2o_fallback",
    identical(regression_fit$implementation_descriptor$fallback_policy,
      "no_h2o_fallback") &&
      identical(binary_fit$implementation_descriptor$fallback_policy,
        "no_h2o_fallback"), "typed failure replaces engine fallback")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Qualify CatBoost vNext CPU Scaling
#'
#' @param rows Number of controlled rows.
#' @param iterations Number of CatBoost iterations.
#' @param thread_count Governed CPU thread count.
#' @return A bounded performance-evidence table.
#' @export
qa_catboost_supervised_performance <- function(rows = 50000L,
    iterations = 25L, thread_count = max(2L,
      parallel::detectCores(logical = TRUE) - 1L)) {
  rows <- as.integer(rows)
  iterations <- as.integer(iterations)
  thread_count <- as.integer(thread_count)
  if (rows < 1000L || iterations < 5L || thread_count < 1L)
    stop("Performance qualification bounds are invalid.", call. = FALSE)
  index <- seq_len(rows)
  data <- data.table::data.table(
    outcome = 15 + sqrt(index) + 4 * sin(index / 37) +
      ifelse(index %% 5L == 0L, 8, -2),
    numeric_a = index / rows,
    numeric_b = log1p(index),
    category = factor(paste0("segment_", index %% 37L)),
    logical_flag = index %% 3L == 0L
  )
  data[index %% 41L == 0L, numeric_a := NA_real_]
  spec <- aq_model_spec("regression", "catboost", "outcome",
    c("numeric_a", "numeric_b", "category", "logical_flag"),
    partition = aq_partition_spec("random", train_fraction = 0.8,
      seed = 814L),
    engine_params = list(iterations = iterations, depth = 5L,
      thread_count = thread_count, verbose = FALSE),
    seed = 814L, dataset_id = "controlled_large_performance_revision")
  fit_time <- system.time(fit <- aq_fit_model(spec, data))[["elapsed"]]
  application_data <- data[seq_len(min(10000L, rows)),
    .(numeric_a, numeric_b, category, logical_flag)]
  application_time <- system.time(prediction <- aq_predict_model(fit,
    new_data = application_data, dataset = "new",
    dataset_id = "controlled_large_application"))[["elapsed"]]
  peak_owned_bytes <- as.numeric(object.size(data)) +
    as.numeric(object.size(fit))
  data.table::data.table(
    check = c("meaningful_scale", "parallel_cpu", "fit_completed",
      "bounded_application", "resource_evidence"),
    status = c(
      if (rows >= 50000L) "success" else "warning",
      if (fit$resource_evidence$thread_count > 1L) "success" else "error",
      if (is.finite(fit_time) && fit_time > 0) "success" else "error",
      if (nrow(prediction$data) == nrow(application_data) &&
          is.finite(application_time)) "success" else "error",
      if (peak_owned_bytes > 0 && nzchar(fit$fit_id)) "success" else "error"),
    rows = rows, iterations = iterations,
    thread_count = fit$resource_evidence$thread_count,
    fit_elapsed_seconds = fit_time,
    application_rows = nrow(application_data),
    application_elapsed_seconds = application_time,
    approximate_owned_bytes = peak_owned_bytes,
    implementation_mode = fit$implementation_descriptor$cpu_gpu
  )
}
