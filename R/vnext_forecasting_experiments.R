# AutoQuant vNext governed forecasting experiment campaigns.

aq_forecast_experiment_type_levels <- function() {
  c("strategy", "feature", "model")
}

aq_forecast_experiment_outcome_levels <- function() {
  c("accepted", "rejected", "inconclusive", "negative_evidence", "partial_improvement", "problem_specific_improvement")
}

aq_forecast_experiment_recommendation_levels <- function() {
  c("adopt_challenger", "retain_baseline", "run_feature_experiment", "run_strategy_experiment", "run_model_experiment", "collect_more_evidence", "stop")
}

aq_forecast_experiment_strategy_levels <- function() {
  c("independent", "grouped", "global", "cross_target", "direct", "recursive")
}

aq_forecast_experiment_feature_levels <- function() {
  inv <- aq_forecasting_carma_mechanism_inventory()
  inv[tuning_layer == "feature_tuning", mechanism]
}

aq_forecast_experiment_model_levels <- function() {
  c("naive", "seasonal_naive", "ets", "arima", "catboost", "croston", "sba", "tsb", "hurdle")
}

aq_forecast_experiment_normalize_candidate <- function(x, experiment_type) {
  if (is.list(x)) {
    out <- x
  } else {
    out <- list(id = as.character(x)[1L])
  }
  if (is.null(out$id) || !nzchar(out$id)) {
    stop("baseline and challenger must include a non-empty id.", call. = FALSE)
  }
  out$id <- tolower(as.character(out$id)[1L])
  if (is.null(out$engine) && experiment_type %in% c("model", "strategy")) {
    out$engine <- if (out$id %in% aq_forecast_engine_levels()) out$id else "naive"
  }
  if (is.null(out$forecast_strategy)) {
    out$forecast_strategy <- if (out$id %in% c("direct", "recursive")) out$id else "direct"
  }
  if (is.null(out$feature_mechanism) && experiment_type == "feature") {
    out$feature_mechanism <- out$id
  }
  out$supported <- switch(
    experiment_type,
    strategy = out$id %in% aq_forecast_experiment_strategy_levels(),
    feature = out$id %in% aq_forecast_experiment_feature_levels(),
    model = out$id %in% aq_forecast_experiment_model_levels(),
    FALSE
  )
  out
}

aq_forecast_experiment_planning_artifact <- function(planning_artifact = NULL, planning_result = NULL) {
  if (!is.null(planning_result)) {
    if (!inherits(planning_result, "aq_forecasting_strategy_plan")) {
      stop("planning_result must be returned by aq_plan_forecasting_strategy().", call. = FALSE)
    }
    return(planning_result$artifact)
  }
  if (is.null(planning_artifact)) return(NULL)
  if (!inherits(planning_artifact, "aq_artifact")) {
    stop("planning_artifact must be the artifact from aq_plan_forecasting_strategy().", call. = FALSE)
  }
  planning_artifact
}

aq_forecast_experiment_lineage_from_plan <- function(planning_artifact) {
  if (is.null(planning_artifact)) return(list())
  envelope <- aq_artifact_envelope(planning_artifact)
  lineage <- aq_vnext_default(envelope$lineage, list())
  metadata <- aq_vnext_default(planning_artifact$metadata, list())
  list(
    planning_artifact_id = aq_vnext_default(envelope$artifact_id, planning_artifact$id),
    discovery_id = aq_vnext_default(metadata$discovery_id, lineage$discovery_id),
    dataset_id = aq_vnext_default(envelope$dataset_id, lineage$dataset_id),
    date = aq_vnext_default(lineage$date, NA_character_),
    targets = aq_vnext_unique_chr(lineage$targets),
    horizon = aq_vnext_default(lineage$horizon, NA_integer_),
    forecast_origin = aq_vnext_default(lineage$forecast_origin, as.Date(NA))
  )
}

aq_forecast_experiment_default_acceptance <- function(primary_metric = "rmse", min_relative_improvement = 0) {
  list(
    primary_metric = as.character(primary_metric)[1L],
    direction = "lower_is_better",
    min_relative_improvement = as.numeric(min_relative_improvement)[1L],
    require_baseline_assessment = TRUE,
    require_challenger_assessment = TRUE
  )
}

aq_forecast_experiment_default_rejection <- function(max_relative_degradation = 0) {
  list(
    max_relative_degradation = as.numeric(max_relative_degradation)[1L],
    preserve_negative_evidence = TRUE
  )
}

#' Create a Governed Forecast Experiment Specification
#'
#' @description
#' Creates one bounded, evidence-guided forecasting experiment. The
#' specification records a frozen baseline, one deterministic challenger, the
#' planning evidence that motivated the experiment, and advisory acceptance and
#' rejection criteria. It does not perform brute-force search or automatic
#' adoption.
#'
#' @param planning_artifact Optional artifact from
#'   `aq_plan_forecasting_strategy()`.
#' @param planning_result Optional full result from
#'   `aq_plan_forecasting_strategy()`.
#' @param experiment_type One of `"strategy"`, `"feature"`, or `"model"`.
#' @param baseline Baseline candidate id or list.
#' @param challenger Challenger candidate id or list.
#' @param target Forecast target. Defaults to the first target in the planning
#'   artifact lineage when available.
#' @param date Forecast date column. Defaults to the planning artifact lineage
#'   when available.
#' @param horizon Forecast horizon. Defaults to the planning artifact lineage
#'   when available.
#' @param forecast_origin Optional forecast origin.
#' @param hypothesis Human-readable hypothesis being tested.
#' @param expected_evidence Expected evidence that should decide the test.
#' @param acceptance_criteria Optional acceptance criteria list.
#' @param rejection_criteria Optional rejection criteria list.
#' @param dataset_id Optional dataset id.
#' @param experiment_id Optional stable experiment id.
#' @param supported_actions Supported downstream actions.
#'
#' @return An `aq_forecast_experiment_spec`.
#' @export
aq_forecast_experiment_spec <- function(
  planning_artifact = NULL,
  planning_result = NULL,
  experiment_type = "model",
  baseline = "naive",
  challenger = "ets",
  target = NULL,
  date = NULL,
  horizon = NULL,
  forecast_origin = NULL,
  hypothesis = NULL,
  expected_evidence = c("out_of_sample_metrics", "baseline_challenger_delta", "warnings"),
  acceptance_criteria = NULL,
  rejection_criteria = NULL,
  dataset_id = NULL,
  experiment_id = NULL,
  supported_actions = c("execute", "compare", "report", "campaign_review", "knowledge_promotion")
) {
  experiment_type <- match.arg(tolower(experiment_type), aq_forecast_experiment_type_levels())
  planning_artifact <- aq_forecast_experiment_planning_artifact(planning_artifact, planning_result)
  plan_lineage <- aq_forecast_experiment_lineage_from_plan(planning_artifact)
  baseline <- aq_forecast_experiment_normalize_candidate(baseline, experiment_type)
  challenger <- aq_forecast_experiment_normalize_candidate(challenger, experiment_type)
  if (!isTRUE(baseline$supported)) {
    stop(paste("unsupported baseline for", experiment_type, "experiment:", baseline$id), call. = FALSE)
  }
  if (!isTRUE(challenger$supported)) {
    stop(paste("unsupported challenger for", experiment_type, "experiment:", challenger$id), call. = FALSE)
  }
  target <- aq_vnext_default(target, plan_lineage$targets[1L])
  date <- aq_vnext_default(date, plan_lineage$date)
  horizon <- as.integer(aq_vnext_default(horizon, plan_lineage$horizon))[1L]
  if (!is.finite(horizon) || horizon < 1L) horizon <- 1L
  forecast_origin <- aq_vnext_default(forecast_origin, plan_lineage$forecast_origin)
  dataset_id <- aq_vnext_default(dataset_id, plan_lineage$dataset_id)
  if (is.null(experiment_id)) {
    experiment_id <- aq_vnext_id(paste("forecast_experiment", experiment_type, baseline$id, challenger$id, sep = "_"))
  }
  if (is.null(acceptance_criteria)) acceptance_criteria <- aq_forecast_experiment_default_acceptance()
  if (is.null(rejection_criteria)) rejection_criteria <- aq_forecast_experiment_default_rejection()
  if (is.null(hypothesis)) {
    hypothesis <- paste("Test whether", challenger$id, "improves forecast evidence relative to frozen baseline", baseline$id)
  }
  spec <- list(
    experiment_id = as.character(experiment_id)[1L],
    schema_version = "aq_forecast_experiment_spec_v1",
    experiment_type = experiment_type,
    planning_artifact_id = aq_vnext_default(plan_lineage$planning_artifact_id, NA_character_),
    discovery_id = aq_vnext_default(plan_lineage$discovery_id, NA_character_),
    baseline = baseline,
    challenger = challenger,
    target = as.character(target)[1L],
    date = as.character(date)[1L],
    horizon = horizon,
    forecast_origin = if (is.null(forecast_origin) || is.na(forecast_origin)) NULL else as.Date(forecast_origin)[1L],
    hypothesis = as.character(hypothesis)[1L],
    expected_evidence = aq_vnext_unique_chr(expected_evidence),
    acceptance_criteria = acceptance_criteria,
    rejection_criteria = rejection_criteria,
    dataset_id = aq_vnext_default(dataset_id, NA_character_),
    supported_actions = aq_vnext_unique_chr(supported_actions),
    auto_adoption = FALSE,
    created_at = aq_vnext_now()
  )
  class(spec) <- c("aq_forecast_experiment_spec", "list")
  spec
}

aq_forecast_experiment_candidate_forecast_spec <- function(experiment_spec, candidate, role) {
  aq_forecast_spec(
    target = experiment_spec$target,
    date = experiment_spec$date,
    horizon = experiment_spec$horizon,
    forecast_origin = experiment_spec$forecast_origin,
    engine = aq_vnext_default(candidate$engine, "naive"),
    forecast_strategy = aq_vnext_default(candidate$forecast_strategy, "direct"),
    prediction_intervals = FALSE,
    forecast_spec_id = aq_vnext_id(paste("forecast_spec", experiment_spec$experiment_id, role, candidate$id, sep = "_")),
    dataset_id = experiment_spec$dataset_id
  )
}

aq_forecast_experiment_metric_value <- function(assessment, metric) {
  metric_name <- as.character(metric)[1L]
  value <- assessment$metrics[metric == metric_name, value]
  if (!length(value)) NA_real_ else as.numeric(value[1L])
}

aq_forecast_experiment_learning <- function(experiment_spec, baseline_assessment, challenger_assessment, execution_status, warnings = character()) {
  metric <- aq_vnext_default(experiment_spec$acceptance_criteria$primary_metric, "rmse")
  baseline_value <- if (inherits(baseline_assessment, "aq_forecast_assessment_result")) aq_forecast_experiment_metric_value(baseline_assessment, metric) else NA_real_
  challenger_value <- if (inherits(challenger_assessment, "aq_forecast_assessment_result")) aq_forecast_experiment_metric_value(challenger_assessment, metric) else NA_real_
  relative_improvement <- if (is.finite(baseline_value) && abs(baseline_value) > .Machine$double.eps && is.finite(challenger_value)) {
    (baseline_value - challenger_value) / abs(baseline_value)
  } else {
    NA_real_
  }
  min_improvement <- aq_vnext_default(experiment_spec$acceptance_criteria$min_relative_improvement, 0)
  max_degradation <- aq_vnext_default(experiment_spec$rejection_criteria$max_relative_degradation, 0)
  if (!identical(execution_status, "success")) {
    outcome <- "negative_evidence"
    recommendation <- "retain_baseline"
    reason <- "The challenger did not produce complete assessable evidence."
  } else if (!is.finite(relative_improvement)) {
    outcome <- "inconclusive"
    recommendation <- "collect_more_evidence"
    reason <- "The primary metric was not available for both baseline and challenger."
  } else if (relative_improvement >= min_improvement) {
    outcome <- if (relative_improvement > 0) "accepted" else "partial_improvement"
    recommendation <- "adopt_challenger"
    reason <- paste("Challenger improved", metric, "relative to the frozen baseline.")
  } else if (relative_improvement < -abs(max_degradation)) {
    outcome <- "rejected"
    recommendation <- "retain_baseline"
    reason <- paste("Challenger degraded", metric, "relative to the frozen baseline.")
  } else {
    outcome <- "inconclusive"
    recommendation <- "collect_more_evidence"
    reason <- "The difference did not clear acceptance or rejection criteria."
  }
  data.table::data.table(
    metric = metric,
    baseline_value = baseline_value,
    challenger_value = challenger_value,
    relative_improvement = relative_improvement,
    outcome = outcome,
    recommendation = recommendation,
    reason = reason,
    uncertainty_decreased = isTRUE(is.finite(relative_improvement)),
    warnings = paste(aq_vnext_unique_chr(warnings), collapse = "; ")
  )
}

aq_forecast_experiment_artifact <- function(experiment_spec, baseline_result, challenger_result, baseline_assessment, challenger_assessment, learning, warnings = character()) {
  comparison <- data.table::data.table(
    role = c("baseline", "challenger"),
    candidate_id = c(experiment_spec$baseline$id, experiment_spec$challenger$id),
    forecast_id = c(aq_vnext_default(baseline_result$forecast_id, NA_character_), aq_vnext_default(challenger_result$forecast_id, NA_character_)),
    assessment_id = c(aq_vnext_default(baseline_assessment$assessment_id, NA_character_), aq_vnext_default(challenger_assessment$assessment_id, NA_character_)),
    engine = c(aq_vnext_default(experiment_spec$baseline$engine, NA_character_), aq_vnext_default(experiment_spec$challenger$engine, NA_character_)),
    forecast_strategy = c(aq_vnext_default(experiment_spec$baseline$forecast_strategy, NA_character_), aq_vnext_default(experiment_spec$challenger$forecast_strategy, NA_character_))
  )
  experiment_artifact_id <- aq_vnext_id("forecast_experiment_artifact")
  parent_ids <- aq_vnext_unique_chr(c(
    experiment_spec$planning_artifact_id,
    aq_vnext_default(baseline_result$forecast_id, NA_character_),
    aq_vnext_default(challenger_result$forecast_id, NA_character_),
    aq_vnext_default(baseline_assessment$assessment_id, NA_character_),
    aq_vnext_default(challenger_assessment$assessment_id, NA_character_)
  ))
  artifact <- new_table_artifact(
    id = experiment_artifact_id,
    title = "Forecast Experiment Evidence",
    data = learning,
    source_generator = "aq_run_forecast_experiment",
    tags = c("vnext", "forecast", "experiment", experiment_spec$experiment_type),
    dependencies = parent_ids,
    version = "aq_forecast_experiment_artifact_v1",
    metadata = list(
      artifact_type = "forecast_experiment_artifact",
      experiment_id = experiment_spec$experiment_id,
      experiment_type = experiment_spec$experiment_type,
      baseline_identity = experiment_spec$baseline,
      challenger_identity = experiment_spec$challenger,
      hypothesis = experiment_spec$hypothesis,
      expected_evidence = experiment_spec$expected_evidence,
      acceptance_criteria = experiment_spec$acceptance_criteria,
      rejection_criteria = experiment_spec$rejection_criteria,
      comparison = comparison,
      learning = learning,
      negative_evidence_preserved = learning$outcome %in% c("negative_evidence", "rejected"),
      auto_adoption = FALSE,
      warnings = aq_vnext_unique_chr(warnings),
      supported_actions = c("compare", "report", "campaign_review", "knowledge_promotion")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = experiment_artifact_id,
    artifact_type = "forecast_experiment_artifact",
    artifact_version = "aq_forecast_experiment_artifact_v1",
    parent_artifact_ids = parent_ids,
    lineage = list(
      experiment_id = experiment_spec$experiment_id,
      planning_artifact_id = experiment_spec$planning_artifact_id,
      baseline_identity = experiment_spec$baseline$id,
      challenger_identity = experiment_spec$challenger$id,
      forecast_origin = experiment_spec$forecast_origin,
      horizon = experiment_spec$horizon,
      target = experiment_spec$target,
      date = experiment_spec$date,
      auto_adoption = FALSE
    ),
    task = "forecast_experiment_campaign",
    operator = "forecast_experiment",
    engine = "deterministic_experiment",
    specification_id = experiment_spec$experiment_id,
    dataset_id = experiment_spec$dataset_id,
    campaign_references = list(experiment_id = experiment_spec$experiment_id),
    warnings = warnings,
    supported_actions = c("compare", "report", "campaign_review", "knowledge_promotion"),
    producer = "aq_run_forecast_experiment"
  )
  artifact
}

#' Run One Governed Forecast Experiment
#'
#' @description
#' Executes one deterministic challenger against a frozen baseline and returns
#' canonical experiment evidence. This function is intentionally bounded: it
#' does not enumerate additional challengers and never adopts a result
#' automatically.
#'
#' @param spec An `aq_forecast_experiment_spec`.
#' @param data Forecasting data.
#' @param baseline_result Optional precomputed frozen baseline forecast.
#' @param challenger_result Optional precomputed challenger forecast.
#'
#' @return An `aq_forecast_experiment_result`.
#' @export
aq_run_forecast_experiment <- function(spec, data, baseline_result = NULL, challenger_result = NULL) {
  if (!inherits(spec, "aq_forecast_experiment_spec")) {
    stop("spec must be created by aq_forecast_experiment_spec().", call. = FALSE)
  }
  warnings <- character()
  run_one <- function(candidate, role, supplied) {
    if (!is.null(supplied)) return(supplied)
    forecast_spec <- aq_forecast_experiment_candidate_forecast_spec(spec, candidate, role)
    aq_fit_forecast(forecast_spec, data)
  }
  baseline_error <- NULL
  challenger_error <- NULL
  baseline_result <- tryCatch(run_one(spec$baseline, "baseline", baseline_result), error = function(e) {
    baseline_error <<- conditionMessage(e)
    NULL
  })
  challenger_result <- tryCatch(run_one(spec$challenger, "challenger", challenger_result), error = function(e) {
    challenger_error <<- conditionMessage(e)
    NULL
  })
  if (!is.null(baseline_error)) warnings <- c(warnings, paste("baseline failed:", baseline_error))
  if (!is.null(challenger_error)) warnings <- c(warnings, paste("challenger failed:", challenger_error))
  baseline_assessment <- if (inherits(baseline_result, "aq_forecast_result")) {
    tryCatch(aq_assess_forecast(baseline_result), error = function(e) {
      warnings <<- c(warnings, paste("baseline assessment failed:", conditionMessage(e)))
      NULL
    })
  } else NULL
  challenger_assessment <- if (inherits(challenger_result, "aq_forecast_result")) {
    tryCatch(aq_assess_forecast(challenger_result), error = function(e) {
      warnings <<- c(warnings, paste("challenger assessment failed:", conditionMessage(e)))
      NULL
    })
  } else NULL
  execution_status <- if (inherits(baseline_assessment, "aq_forecast_assessment_result") && inherits(challenger_assessment, "aq_forecast_assessment_result")) "success" else "failed"
  learning <- aq_forecast_experiment_learning(spec, baseline_assessment, challenger_assessment, execution_status, warnings = warnings)
  artifact <- aq_forecast_experiment_artifact(spec, baseline_result, challenger_result, baseline_assessment, challenger_assessment, learning, warnings = warnings)
  result <- list(
    experiment_id = spec$experiment_id,
    schema_version = "aq_forecast_experiment_result_v1",
    status = execution_status,
    spec = spec,
    baseline_result = baseline_result,
    challenger_result = challenger_result,
    baseline_assessment = baseline_assessment,
    challenger_assessment = challenger_assessment,
    learning = learning,
    recommendation = learning$recommendation[1L],
    outcome = learning$outcome[1L],
    artifact = artifact,
    warnings = aq_vnext_unique_chr(warnings),
    auto_adoption = FALSE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_forecast_experiment_result", "list")
  result
}

aq_forecast_experiment_campaign_artifact <- function(campaign_id, specs, results) {
  summary <- data.table::rbindlist(lapply(results, function(result) {
    data.table::data.table(
      experiment_id = result$experiment_id,
      experiment_type = result$spec$experiment_type,
      baseline = result$spec$baseline$id,
      challenger = result$spec$challenger$id,
      status = result$status,
      outcome = result$outcome,
      recommendation = result$recommendation,
      warnings = paste(result$warnings, collapse = "; ")
    )
  }), use.names = TRUE, fill = TRUE)
  artifact_id <- aq_vnext_id("forecast_experiment_campaign")
  parent_ids <- aq_vnext_unique_chr(vapply(results, function(result) aq_artifact_envelope(result$artifact)$artifact_id, character(1L)))
  artifact <- new_table_artifact(
    id = artifact_id,
    title = "Forecast Experiment Campaign",
    data = summary,
    source_generator = "aq_run_forecast_experiment_campaign",
    tags = c("vnext", "forecast", "experiment", "campaign"),
    dependencies = parent_ids,
    version = "aq_forecast_experiment_campaign_artifact_v1",
    metadata = list(
      artifact_type = "forecast_experiment_campaign_artifact",
      campaign_id = campaign_id,
      experiment_count = length(specs),
      summary = summary,
      supported_actions = c("report", "campaign_review", "knowledge_promotion")
    )
  )
  aq_vnext_attach_envelope(
    artifact,
    artifact_id = artifact_id,
    artifact_type = "forecast_experiment_campaign_artifact",
    artifact_version = "aq_forecast_experiment_campaign_artifact_v1",
    parent_artifact_ids = parent_ids,
    lineage = list(campaign_id = campaign_id, experiment_ids = vapply(specs, `[[`, character(1L), "experiment_id")),
    task = "forecast_experiment_campaign",
    operator = "forecast_experiment_campaign_summary",
    engine = "deterministic_experiment",
    specification_id = campaign_id,
    dataset_id = aq_vnext_default(specs[[1L]]$dataset_id, NA_character_),
    campaign_references = list(campaign_id = campaign_id),
    supported_actions = c("report", "campaign_review", "knowledge_promotion"),
    producer = "aq_run_forecast_experiment_campaign"
  )
}

#' Run a Governed Forecast Experiment Campaign
#'
#' @description
#' Runs a bounded sequence of already-specified forecast experiments. The
#' campaign records outcomes and recommendations, but it does not branch into a
#' search tree.
#'
#' @param experiment_specs A list of `aq_forecast_experiment_spec` objects.
#' @param data Forecasting data.
#' @param campaign_id Optional campaign id.
#' @param stop_on_failure Whether to stop after the first failed experiment.
#'
#' @return An `aq_forecast_experiment_campaign_result`.
#' @export
aq_run_forecast_experiment_campaign <- function(experiment_specs, data, campaign_id = NULL, stop_on_failure = FALSE) {
  if (!is.list(experiment_specs) || !length(experiment_specs)) {
    stop("experiment_specs must be a non-empty list of aq_forecast_experiment_spec objects.", call. = FALSE)
  }
  if (!all(vapply(experiment_specs, inherits, logical(1L), what = "aq_forecast_experiment_spec"))) {
    stop("every experiment_specs entry must be an aq_forecast_experiment_spec.", call. = FALSE)
  }
  campaign_id <- aq_vnext_default(campaign_id, aq_vnext_id("forecast_experiment_campaign"))
  results <- list()
  for (i in seq_along(experiment_specs)) {
    result <- aq_run_forecast_experiment(experiment_specs[[i]], data)
    results[[length(results) + 1L]] <- result
    if (isTRUE(stop_on_failure) && !identical(result$status, "success")) break
  }
  artifact <- aq_forecast_experiment_campaign_artifact(campaign_id, experiment_specs, results)
  out <- list(
    campaign_id = campaign_id,
    schema_version = "aq_forecast_experiment_campaign_result_v1",
    status = if (all(vapply(results, function(x) identical(x$status, "success"), logical(1L)))) "success" else "partial",
    results = results,
    summary = artifact$data,
    artifact = artifact,
    created_at = aq_vnext_now()
  )
  class(out) <- c("aq_forecast_experiment_campaign_result", "list")
  out
}

#' QA for vNext Forecasting Experiment Campaigns
#'
#' @return A data.table of deterministic QA checks.
#' @export
qa_vnext_forecasting_experiment_campaigns <- function() {
  rows <- list()
  add <- function(check, ok, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_forecasting_experiment_campaigns",
      check = check,
      status = if (isTRUE(ok)) "pass" else "fail",
      message = message
    )
  }
  dt <- aq_vnext_forecasting_planner_fixture()
  discovery <- aq_discover_forecasting_capabilities(dt, target = "leads", date = "date", horizon = 3L, forecast_origin = max(dt$date) - 21L, dataset_id = "qa_forecast_experiment")
  plan <- aq_plan_forecasting_strategy(discovery)
  spec <- aq_forecast_experiment_spec(
    planning_result = plan,
    experiment_type = "model",
    baseline = "naive",
    challenger = "seasonal_naive",
    target = "leads",
    date = "date",
    horizon = 3L,
    forecast_origin = max(dt$date) - 21L,
    hypothesis = "Seasonal naive should be tested against a frozen naive baseline before any adoption decision."
  )
  add("spec_constructed", inherits(spec, "aq_forecast_experiment_spec") && identical(spec$experiment_type, "model"))
  add("baseline_frozen_identity", identical(spec$baseline$id, "naive") && identical(spec$baseline$engine, "naive"))
  add("challenger_identity", identical(spec$challenger$id, "seasonal_naive") && identical(spec$challenger$engine, "seasonal_naive"))
  feature_spec <- aq_forecast_experiment_spec(plan$artifact, experiment_type = "feature", baseline = "temporal_lag_roll_features", challenger = "fourier_seasonality", target = "leads", date = "date")
  add("feature_hypothesis_governed", identical(feature_spec$experiment_type, "feature") && feature_spec$challenger$id %in% aq_forecast_experiment_feature_levels())
  execution_dt <- aq_vnext_forecast_fixture(130L)
  execution_discovery <- aq_discover_forecasting_capabilities(
    execution_dt,
    target = "demand",
    date = "date",
    horizon = 3L,
    forecast_origin = max(execution_dt$date) - 3L,
    dataset_id = "qa_forecast_experiment_execution"
  )
  execution_plan <- aq_plan_forecasting_strategy(execution_discovery)
  execution_spec <- aq_forecast_experiment_spec(
    planning_result = execution_plan,
    experiment_type = "model",
    baseline = "naive",
    challenger = "ets",
    target = "demand",
    date = "date",
    horizon = 3L,
    forecast_origin = max(execution_dt$date) - 3L,
    hypothesis = "ETS should be tested against a frozen naive baseline before any adoption decision."
  )
  result <- aq_run_forecast_experiment(execution_spec, execution_dt)
  add("challenger_execution", inherits(result, "aq_forecast_experiment_result") && result$status == "success")
  add("learning_assessment", result$outcome %in% aq_forecast_experiment_outcome_levels() && result$recommendation %in% aq_forecast_experiment_recommendation_levels())
  add("no_auto_adoption", identical(result$auto_adoption, FALSE) && identical(result$artifact$metadata$auto_adoption, FALSE))
  add("artifact_lineage", inherits(result$artifact, "aq_artifact") && !aq_vnext_has_validation_error(aq_validate_artifact(result$artifact)))
  bad_spec <- aq_forecast_experiment_spec(plan$artifact, experiment_type = "model", baseline = "naive", challenger = "arima", target = "missing_target", date = "date", horizon = 3L, forecast_origin = max(dt$date) - 21L)
  bad_result <- aq_run_forecast_experiment(bad_spec, dt)
  add("negative_evidence_preserved", bad_result$status == "failed" && bad_result$outcome == "negative_evidence" && isTRUE(bad_result$artifact$metadata$negative_evidence_preserved))
  campaign <- aq_run_forecast_experiment_campaign(list(execution_spec), execution_dt)
  add("campaign_constructed", inherits(campaign, "aq_forecast_experiment_campaign_result") && nrow(campaign$summary) == 1L)
  add("campaign_artifact_integrity", inherits(campaign$artifact, "aq_artifact") && !aq_vnext_has_validation_error(aq_validate_artifact(campaign$artifact)))
  add("analytics_shinyapp_compatibility", all(vapply(list(result$artifact, campaign$artifact), function(x) inherits(x, "aq_artifact") && !is.null(x$artifact_envelope) && "campaign_review" %in% aq_supported_actions(x), logical(1L))))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
