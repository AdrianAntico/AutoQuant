# AutoQuant canonical decision-management framework.

aq_decision_supported_actions <- function() {
  c(
    "validate_decision_context",
    "assess_alternatives",
    "assess_optionality",
    "recommend",
    "decision_review",
    "outcome_review",
    "campaign_review",
    "experiment_planning",
    "report",
    "genai_context",
    "knowledge_promotion"
  )
}

aq_decision_record_dt <- function(x, id_col, object_type) {
  out <- aq_business_intent_as_dt(x, id_col, object_type)
  scalar_id_cols <- names(out)[grepl("(^|_)id$", names(out)) | grepl("_alternative_id$", names(out))]
  for (col in scalar_id_cols) {
    out[, (col) := vapply(get(col), function(value) aq_vnext_default(aq_business_flat_chr(value)[1L], NA_character_), character(1L))]
  }
  out
}

aq_decision_empty <- function(cols) {
  out <- data.table::as.data.table(stats::setNames(rep(list(character()), length(cols)), cols))
  out[0]
}

aq_decision_numeric_vec <- function(x) {
  suppressWarnings(as.numeric(unlist(x, recursive = TRUE, use.names = FALSE)))
}

aq_decision_col_num <- function(dt, col, default = NA_real_) {
  if (!col %in% names(dt)) return(rep(default, nrow(dt)))
  values <- dt[[col]]
  if (is.list(values)) {
    out <- vapply(values, aq_business_num, numeric(1L), default = default)
  } else {
    out <- suppressWarnings(as.numeric(values))
  }
  out[is.na(out)] <- default
  out
}

aq_decision_summary_max <- function(x) {
  x <- aq_decision_numeric_vec(x)
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else max(x)
}

aq_decision_summary_min <- function(x) {
  x <- aq_decision_numeric_vec(x)
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else min(x)
}

aq_decision_chr1 <- function(x, default = NA_character_) {
  out <- aq_business_flat_chr(x)
  aq_vnext_default(out[1L], default)
}

aq_decision_alternative_levels <- function() {
  c("do_nothing", "defer", "pilot", "stage", "partial_implementation", "full_implementation", "expand", "contract", "abandon", "switch", "collect_more_evidence")
}

aq_decision_recommendation_levels <- function() {
  c("proceed", "proceed_within_validated_range", "proceed_with_monitoring", "pilot", "stage_investment", "defer", "collect_more_evidence", "modify_alternative", "choose_alternative", "retain_baseline", "reject", "abandon", "switch", "escalate", "insufficient_information", "no_authorized_action_available")
}

#' Create Decision Alternatives
#'
#' @param alternatives Data frame or list of alternative records.
#'
#' @return A data.table of typed decision alternatives.
#' @export
aq_decision_alternative <- function(alternatives) {
  out <- aq_decision_record_dt(alternatives, "alternative_id", "alternative")
  if (!"decision_context_id" %in% names(out)) out[, decision_context_id := NA_character_]
  if (!"name" %in% names(out)) out[, name := alternative_id]
  if (!"alternative_type" %in% names(out)) out[, alternative_type := "full_implementation"]
  if (!"baseline" %in% names(out)) out[, baseline := FALSE]
  if (!"status" %in% names(out)) out[, status := "candidate"]
  out
}

aq_decision_criteria <- function(criteria) {
  out <- aq_decision_record_dt(criteria, "criterion_id", "criterion")
  if (!"direction" %in% names(out)) out[, direction := "maximize"]
  if (!"weight" %in% names(out)) out[, weight := NA_real_]
  if (!"hard_constraint" %in% names(out)) out[, hard_constraint := FALSE]
  if (!"scale" %in% names(out)) out[, scale := "raw"]
  out
}

aq_financial_impact <- function(financial_impacts) {
  out <- aq_decision_record_dt(financial_impacts, "financial_id", "financial_impact")
  required <- c("alternative_id", "initial_cost", "recurring_cost", "expected_benefit", "expected_revenue", "expected_savings", "downside_estimate", "upside_estimate", "time_to_value", "payback_period", "opportunity_cost", "recovery_value", "confidence", "source_type")
  for (col in setdiff(required, names(out))) out[, (col) := NA]
  total_cost <- aq_decision_col_num(out, "initial_cost", 0) + aq_decision_col_num(out, "recurring_cost", 0)
  out[, net_benefit := aq_decision_col_num(out, "expected_benefit", 0) + aq_decision_col_num(out, "expected_revenue", 0) + aq_decision_col_num(out, "expected_savings", 0) - total_cost - aq_decision_col_num(out, "opportunity_cost", 0)]
  out[, simple_roi := data.table::fifelse(total_cost > 0, net_benefit / total_cost, NA_real_)]
  out
}

aq_uncertainty_evidence <- function(uncertainties) {
  out <- aq_decision_record_dt(uncertainties, "uncertainty_id", "uncertainty")
  required <- c("alternative_id", "criterion_id", "uncertainty_category", "source", "direction", "magnitude", "reducibility", "candidate_experiment", "time_to_resolve", "decision_sensitivity", "residual_uncertainty", "evidence")
  for (col in setdiff(required, names(out))) out[, (col) := NA]
  out
}

#' Create Decision Optionality Records
#'
#' @param optionality Data frame or list of optionality records.
#'
#' @return A data.table of typed optionality records.
#' @export
aq_decision_optionality <- function(optionality) {
  out <- aq_decision_record_dt(optionality, "optionality_id", "optionality")
  required <- c("alternative_id", "option_type", "underlying_opportunity", "enabling_action", "exercise_action", "exercise_window", "expiration", "enabling_cost", "exercise_cost", "expansion_capacity", "abandonment_value", "switching_cost", "reversibility", "uncertainty_drivers", "future_decisions_enabled", "future_decisions_constrained", "options_foreclosed", "dependencies", "compound_option_relationships", "valuation_approach", "confidence", "limitations")
  for (col in setdiff(required, names(out))) out[, (col) := NA]
  out
}

aq_decision_recommendations <- function(recommendations) {
  out <- aq_decision_record_dt(recommendations, "recommendation_id", "decision_recommendation")
  if (!"recommendation_category" %in% names(out)) out[, recommendation_category := NA_character_]
  out
}

aq_decision_records <- function(decisions) {
  out <- aq_decision_record_dt(decisions, "decision_id", "decision_record")
  if (!"selected_alternative_id" %in% names(out)) out[, selected_alternative_id := NA_character_]
  out
}

aq_outcome_reviews <- function(outcomes) {
  out <- aq_decision_record_dt(outcomes, "outcome_review_id", "outcome_review")
  if (!"decision_id" %in% names(out)) out[, decision_id := NA_character_]
  if (!"actual_execution_state" %in% names(out)) out[, actual_execution_state := "not_started"]
  out
}

aq_decision_lifecycle_status_levels <- function() {
  c("recommended", "approved", "rejected", "modified", "awaiting_approval", "awaiting_implementation", "implemented", "completed", "reviewed", "retired", "superseded")
}

aq_decision_memory_supported_actions <- function() {
  c("decision_review", "outcome_review", "campaign_review", "report", "genai_context", "knowledge_promotion")
}

#' Create a Canonical Decision Context
#'
#' @description
#' Creates a typed decision-management object containing a decision question,
#' alternatives, criteria, financial impact, uncertainty, optionality, authority,
#' coverage, recommendations, decisions, and outcome follow-up.
#'
#' @param context Decision context record.
#' @param alternatives Alternative records.
#' @param criteria Evaluation criteria records.
#' @param financial_impacts Financial impact records.
#' @param uncertainties Uncertainty records.
#' @param optionality Optionality records.
#' @param recommendations Recommendation records.
#' @param decisions Decision records.
#' @param outcomes Outcome-review records.
#' @param business_intent Optional `aq_business_intent` object.
#' @param variable_semantics Optional `aq_variable_semantics` object.
#' @param decision_context_id Optional context id.
#' @param supported_downstream_actions Supported downstream actions.
#'
#' @return An `aq_decision_context` object.
#' @export
aq_decision_context <- function(
  context,
  alternatives = NULL,
  criteria = NULL,
  financial_impacts = NULL,
  uncertainties = NULL,
  optionality = NULL,
  recommendations = NULL,
  decisions = NULL,
  outcomes = NULL,
  business_intent = NULL,
  variable_semantics = NULL,
  decision_context_id = NULL,
  supported_downstream_actions = aq_decision_supported_actions()
) {
  context <- aq_decision_record_dt(context, "decision_context_id", "decision_context")
  if (nrow(context) != 1L) stop("context must describe exactly one decision context.", call. = FALSE)
  if (!is.null(decision_context_id)) context[, decision_context_id := as.character(decision_context_id)[1L]]
  if (!"decision_question" %in% names(context)) stop("context must include decision_question.", call. = FALSE)
  if (!"status" %in% names(context)) context[, status := "draft"]
  alternatives <- aq_decision_alternative(aq_vnext_default(alternatives, list(alternative_id = "baseline", decision_context_id = context$decision_context_id, name = "Do nothing", alternative_type = "do_nothing", baseline = TRUE)))
  criteria <- if (is.null(criteria)) aq_decision_empty(c("criterion_id")) else aq_decision_criteria(criteria)
  financial_impacts <- if (is.null(financial_impacts)) aq_decision_empty(c("financial_id")) else aq_financial_impact(financial_impacts)
  uncertainties <- if (is.null(uncertainties)) aq_decision_empty(c("uncertainty_id")) else aq_uncertainty_evidence(uncertainties)
  optionality <- if (is.null(optionality)) aq_decision_empty(c("optionality_id")) else aq_decision_optionality(optionality)
  recommendations <- if (is.null(recommendations)) aq_decision_empty(c("recommendation_id")) else aq_decision_recommendations(recommendations)
  decisions <- if (is.null(decisions)) aq_decision_empty(c("decision_id")) else aq_decision_records(decisions)
  outcomes <- if (is.null(outcomes)) aq_decision_empty(c("outcome_review_id")) else aq_outcome_reviews(outcomes)
  out <- list(
    decision_context_id = context$decision_context_id[1L],
    schema_version = "aq_decision_context_v1",
    context = context,
    alternatives = alternatives,
    criteria = criteria,
    financial_impacts = financial_impacts,
    uncertainties = uncertainties,
    optionality = optionality,
    recommendations = recommendations,
    decisions = decisions,
    outcomes = outcomes,
    business_intent = business_intent,
    variable_semantics = variable_semantics,
    supported_downstream_actions = aq_vnext_unique_chr(supported_downstream_actions),
    created_at = aq_vnext_now()
  )
  out$optionality_assessment <- aq_assess_decision_optionality(out)
  out$alternative_assessment <- aq_assess_decision_alternatives(out)
  out$validation <- aq_validate_decision_context(out)
  class(out) <- c("aq_decision_context", "list")
  out
}

aq_decision_validation_row <- function(check, status, message, object_type = NA_character_, object_id = NA_character_, severity = status, recommendation = NA_character_) {
  data.table::data.table(check = check, status = status, severity = severity, object_type = object_type, object_id = object_id, message = message, recommendation = recommendation)
}

aq_decision_ref_check <- function(rows, from_dt, from_type, from_id_col, ref_col, valid, check, message) {
  if (!nrow(from_dt) || !ref_col %in% names(from_dt)) return(rows)
  refs <- aq_business_intent_list_col(from_dt, ref_col)
  for (i in seq_len(nrow(from_dt))) {
    missing <- setdiff(refs[[i]], valid)
    if (length(missing)) rows[[length(rows) + 1L]] <- aq_decision_validation_row(check, "fail", paste(message, paste(missing, collapse = ", ")), from_type, from_dt[[from_id_col]][i], recommendation = "Create the referenced record or remove the invalid reference.")
  }
  rows
}

#' Validate a Decision Context
#'
#' @param decision_context An `aq_decision_context` object.
#'
#' @return A data.table of deterministic validation diagnostics.
#' @export
aq_validate_decision_context <- function(decision_context) {
  rows <- list()
  add <- function(check, status, message, object_type = NA_character_, object_id = NA_character_, severity = status, recommendation = NA_character_) {
    rows[[length(rows) + 1L]] <<- aq_decision_validation_row(check, status, message, object_type, object_id, severity, recommendation)
  }
  if (!is.list(decision_context) || is.null(decision_context$context) || is.null(decision_context$alternatives)) {
    add("decision_context_object", "fail", "decision_context must be created by aq_decision_context().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  context_id <- decision_context$decision_context_id
  add("decision_context_object", "pass", "decision context object is structured.", severity = "info")
  if (!nzchar(context_id)) add("decision_context_id", "fail", "decision_context_id is required.") else add("decision_context_id", "pass", "decision_context_id is present.", severity = "info")
  question <- aq_business_flat_chr(decision_context$context$decision_question)
  if (!length(question) || !nzchar(question[1L])) add("decision_question", "fail", "decision_question is required.") else add("decision_question", "pass", "decision_question is explicit.", severity = "info")
  if (!nrow(decision_context$alternatives)) {
    add("alternatives", "fail", "at least one alternative is required.")
  } else {
    add("alternatives", "pass", paste("alternatives:", nrow(decision_context$alternatives)), severity = "info")
  }
  baseline_count <- sum(vapply(decision_context$alternatives$baseline, aq_business_bool, logical(1L)))
  if (baseline_count < 1L) add("baseline_alternative", "fail", "a do-nothing or current-policy baseline is required.") else add("baseline_alternative", "pass", "baseline alternative is present.", severity = "info")
  invalid_types <- setdiff(aq_business_flat_chr(decision_context$alternatives$alternative_type), aq_decision_alternative_levels())
  if (length(invalid_types)) add("alternative_type", "fail", paste("unsupported alternative type:", paste(invalid_types, collapse = ", "))) else add("alternative_type", "pass", "alternative types are supported.", severity = "info")
  alt_ids <- aq_vnext_unique_chr(decision_context$alternatives$alternative_id)
  rows <- aq_decision_ref_check(rows, decision_context$financial_impacts, "financial_impact", "financial_id", "alternative_id", alt_ids, "financial_alternative_reference", "financial impact references missing alternative:")
  rows <- aq_decision_ref_check(rows, decision_context$uncertainties, "uncertainty", "uncertainty_id", "alternative_id", alt_ids, "uncertainty_alternative_reference", "uncertainty references missing alternative:")
  rows <- aq_decision_ref_check(rows, decision_context$optionality, "optionality", "optionality_id", "alternative_id", alt_ids, "optionality_alternative_reference", "optionality references missing alternative:")
  rows <- aq_decision_ref_check(rows, decision_context$recommendations, "decision_recommendation", "recommendation_id", "preferred_alternative_id", alt_ids, "recommendation_alternative_reference", "recommendation references missing preferred alternative:")
  rows <- aq_decision_ref_check(rows, decision_context$decisions, "decision_record", "decision_id", "selected_alternative_id", alt_ids, "decision_alternative_reference", "decision references missing selected alternative:")
  invalid_rec <- if (nrow(decision_context$recommendations) && "recommendation_category" %in% names(decision_context$recommendations)) setdiff(aq_business_flat_chr(decision_context$recommendations$recommendation_category), c(aq_decision_recommendation_levels(), NA_character_)) else character()
  if (length(invalid_rec)) add("recommendation_category", "fail", paste("unsupported recommendation category:", paste(invalid_rec, collapse = ", "))) else add("recommendation_category", "pass", "recommendation categories are supported.", severity = "info")
  if (!any(vapply(rows, function(x) identical(x$status, "fail"), logical(1L)))) add("validation_summary", "pass", "decision-context validation completed without failures.", severity = "info")
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_optionality_score <- function(row) {
  option_type <- aq_vnext_default(aq_business_flat_chr(row$option_type)[1L], "unknown")
  foreclosed <- length(aq_business_flat_chr(row$options_foreclosed)) > 0L
  confidence <- aq_business_num(row$confidence, 0.5)
  future_enabled <- length(aq_business_flat_chr(row$future_decisions_enabled)) > 0L
  reversible <- aq_business_bool(row$reversibility, FALSE)
  if (foreclosed && !future_enabled && !reversible) return(list(category = "material_options_foreclosed", reason = "Alternative forecloses future options and is not reversible."))
  if (option_type %in% c("stage", "learn", "defer", "expand", "grow", "compound") && future_enabled && confidence >= 0.5) return(list(category = "high_positive_optionality", reason = "Alternative preserves or creates future decisions with credible option evidence."))
  if (option_type %in% c("stage", "learn", "defer", "expand", "grow", "compound")) return(list(category = "moderate_positive_optionality", reason = "Alternative creates future choices, but evidence is partial."))
  if (foreclosed) return(list(category = "negative_optionality", reason = "Alternative forecloses some future choices."))
  if (option_type %in% c("abandon", "switch", "contract")) return(list(category = "moderate_positive_optionality", reason = "Alternative preserves flexibility to limit downside."))
  list(category = "insufficient_information", reason = "Optionality record lacks enough structure for categorical assessment.")
}

#' Assess Decision Optionality
#'
#' @param decision_context An `aq_decision_context` object.
#'
#' @return A data.table of optionality assessments.
#' @export
aq_assess_decision_optionality <- function(decision_context) {
  opt <- decision_context$optionality
  if (is.null(opt) || !nrow(opt)) return(data.table::data.table(optionality_id = character(), alternative_id = character(), option_type = character(), optionality_assessment = character(), reason = character()))
  rows <- lapply(seq_len(nrow(opt)), function(i) {
    score <- aq_optionality_score(opt[i])
    data.table::data.table(
      optionality_id = opt$optionality_id[i],
      alternative_id = aq_vnext_default(aq_business_flat_chr(opt$alternative_id[i])[1L], NA_character_),
      option_type = aq_vnext_default(aq_business_flat_chr(opt$option_type[i])[1L], NA_character_),
      optionality_assessment = score$category,
      reason = score$reason
    )
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_decision_alt_metrics <- function(decision_context) {
  alts <- data.table::copy(decision_context$alternatives)
  fin <- data.table::copy(decision_context$financial_impacts)
  unc <- data.table::copy(decision_context$uncertainties)
  opt <- data.table::copy(decision_context$optionality_assessment)
  if (!nrow(fin)) fin <- data.table::data.table(alternative_id = alts$alternative_id, net_benefit = NA_real_, simple_roi = NA_real_, downside_estimate = NA_real_, upside_estimate = NA_real_, source_type = NA_character_)
  fin_summary <- fin[, .(
    net_benefit = aq_decision_summary_max(net_benefit),
    simple_roi = aq_decision_summary_max(simple_roi),
    downside_estimate = aq_decision_summary_min(downside_estimate),
    upside_estimate = aq_decision_summary_max(upside_estimate),
    financial_source = paste(unique(aq_business_flat_chr(source_type)), collapse = ", ")
  ), by = alternative_id]
  fin_summary[!is.finite(net_benefit), net_benefit := NA_real_]
  fin_summary[!is.finite(simple_roi), simple_roi := NA_real_]
  fin_summary[!is.finite(downside_estimate), downside_estimate := NA_real_]
  fin_summary[!is.finite(upside_estimate), upside_estimate := NA_real_]
  unc_summary <- if (nrow(unc)) unc[, .(uncertainty_count = .N, high_sensitivity_count = sum(aq_business_flat_chr(decision_sensitivity) %in% c("high", "critical"), na.rm = TRUE), reducible_uncertainty_count = sum(aq_business_flat_chr(reducibility) %in% c("reducible", "partially_reducible"), na.rm = TRUE)), by = alternative_id] else data.table::data.table(alternative_id = alts$alternative_id, uncertainty_count = 0L, high_sensitivity_count = 0L, reducible_uncertainty_count = 0L)
  opt_summary <- if (nrow(opt)) opt[, .(optionality = paste(unique(optionality_assessment), collapse = ", ")), by = alternative_id] else data.table::data.table(alternative_id = alts$alternative_id, optionality = NA_character_)
  out <- merge(alts, fin_summary, by = "alternative_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, unc_summary, by = "alternative_id", all.x = TRUE, sort = FALSE)
  out <- merge(out, opt_summary, by = "alternative_id", all.x = TRUE, sort = FALSE)
  for (col in c("uncertainty_count", "high_sensitivity_count", "reducible_uncertainty_count")) {
    if (col %in% names(out)) out[is.na(get(col)), (col) := 0L]
  }
  out[, baseline := vapply(baseline, aq_business_bool, logical(1L))]
  out[, authority_compatible := if ("authority_compatible" %in% names(out)) vapply(authority_compatible, aq_business_bool, logical(1L)) else TRUE]
  out[, scope_compatible := if ("scope_compatible" %in% names(out)) vapply(scope_compatible, aq_business_bool, logical(1L)) else TRUE]
  out
}

#' Assess Decision Alternatives
#'
#' @description
#' Produces structured tradeoff evidence for alternatives. It does not force
#' all dimensions into one opaque score.
#'
#' @param decision_context An `aq_decision_context` object.
#'
#' @return A data.table of alternative-level assessment evidence.
#' @export
aq_assess_decision_alternatives <- function(decision_context) {
  metrics <- aq_decision_alt_metrics(decision_context)
  if (!nrow(metrics)) return(metrics)
  max_value <- aq_decision_summary_max(metrics$net_benefit)
  metrics[, hard_constraint_failure := !scope_compatible]
  metrics[, authority_failure := !authority_compatible]
  metrics[, coverage_limitation := if ("coverage_limitation" %in% names(metrics)) vapply(coverage_limitation, function(x) paste(aq_business_flat_chr(x), collapse = ", "), character(1L)) else NA_character_]
  metrics[, missing_information := is.na(net_benefit) | uncertainty_count > 0L & high_sensitivity_count > 0L]
  metrics[, dominated := FALSE]
  if (sum(!is.na(metrics$net_benefit)) > 1L) {
    for (i in seq_len(nrow(metrics))) {
      better <- metrics[alternative_id != metrics$alternative_id[i] & !is.na(net_benefit) & net_benefit >= metrics$net_benefit[i] & !authority_failure & !hard_constraint_failure]
      if (nrow(better) && isTRUE(metrics$authority_failure[i] || metrics$hard_constraint_failure[i] || any(better$net_benefit > metrics$net_benefit[i]))) metrics$dominated[i] <- TRUE
    }
  }
  metrics[, recommendation_category := data.table::fifelse(authority_failure, "escalate",
    data.table::fifelse(hard_constraint_failure, "no_authorized_action_available",
      data.table::fifelse(missing_information & reducible_uncertainty_count > 0L, "collect_more_evidence",
        data.table::fifelse(!is.na(max_value) & net_benefit == max_value & !baseline, "choose_alternative",
          data.table::fifelse(baseline & (is.na(max_value) | net_benefit == max_value), "retain_baseline", "modify_alternative")
        )
      )
    )
  )]
  metrics[, required_human_judgment := authority_failure | hard_constraint_failure | missing_information | grepl("foreclosed", aq_vnext_default(optionality, ""), fixed = TRUE)]
  metrics[, supported_next_actions := mapply(function(cat, uncertain) {
    paste(unique(c(cat, if (uncertain) "experiment_planning" else character(), "decision_review", "report")), collapse = ", ")
  }, recommendation_category, missing_information)]
  metrics[, .(alternative_id, decision_context_id, name, alternative_type, baseline, net_benefit, simple_roi, downside_estimate, upside_estimate, financial_source, uncertainty_count, high_sensitivity_count, reducible_uncertainty_count, optionality, hard_constraint_failure, authority_failure, coverage_limitation, missing_information, dominated, recommendation_category, required_human_judgment, supported_next_actions)]
}

#' Create a Decision Context Artifact
#'
#' @param decision_context An `aq_decision_context` object.
#' @param artifact_id Optional artifact id.
#'
#' @return A canonical decision-context artifact.
#' @export
aq_decision_context_artifact <- function(decision_context, artifact_id = NULL) {
  if (!inherits(decision_context, "aq_decision_context")) stop("decision_context must be created by aq_decision_context().", call. = FALSE)
  validation <- aq_validate_decision_context(decision_context)
  assessment <- aq_assess_decision_alternatives(decision_context)
  optionality <- aq_assess_decision_optionality(decision_context)
  warnings <- validation[status %in% c("warning", "fail", "error"), message]
  artifact_id <- aq_vnext_default(artifact_id, aq_vnext_id("decision_context_artifact"))
  artifact <- new_table_artifact(
    id = artifact_id,
    title = "Decision Context and Alternatives",
    data = assessment,
    source_generator = "aq_decision_context_artifact",
    tags = c("vnext", "decision_management", "alternatives", "optionality"),
    dependencies = aq_vnext_default(decision_context$business_intent$intent_id, character()),
    version = "aq_decision_context_artifact_v1",
    metadata = list(
      artifact_type = "decision_context_artifact",
      decision_context_id = decision_context$decision_context_id,
      schema_version = decision_context$schema_version,
      context = decision_context$context,
      alternatives = decision_context$alternatives,
      criteria = decision_context$criteria,
      financial_impacts = decision_context$financial_impacts,
      uncertainties = decision_context$uncertainties,
      optionality = decision_context$optionality,
      optionality_assessment = optionality,
      alternative_assessment = assessment,
      recommendations = decision_context$recommendations,
      decisions = decision_context$decisions,
      outcomes = decision_context$outcomes,
      validation = validation,
      business_intent_id = aq_vnext_default(decision_context$business_intent$intent_id, NA_character_),
      variable_semantics_id = aq_vnext_default(decision_context$variable_semantics$semantics_id, NA_character_),
      warnings = warnings,
      supported_actions = decision_context$supported_downstream_actions
    )
  )
  aq_vnext_attach_envelope(
    artifact,
    artifact_id = artifact_id,
    artifact_type = "decision_context_artifact",
    artifact_version = "aq_decision_context_artifact_v1",
    parent_artifact_ids = aq_vnext_unique_chr(c(decision_context$business_intent$intent_id, decision_context$variable_semantics$semantics_id)),
    lineage = list(decision_context_id = decision_context$decision_context_id, alternative_ids = decision_context$alternatives$alternative_id),
    task = "decision_management",
    operator = "decision_alternative_assessment",
    engine = "deterministic_decision_validator",
    specification_id = decision_context$decision_context_id,
    warnings = warnings,
    supported_actions = decision_context$supported_downstream_actions,
    producer = "aq_decision_context_artifact"
  )
}

aq_decision_expected_outcome <- function(decision_context, decision_id = NULL) {
  decisions <- decision_context$decisions
  if (!nrow(decisions)) return(list(decision_id = NA_character_, selected_alternative_id = NA_character_, expected_net_benefit = NA_real_, expected_outcome = NA_character_))
  if (!is.null(decision_id)) decisions <- decisions[decision_id == as.character(decision_id)[1L]]
  if (!nrow(decisions)) return(list(decision_id = NA_character_, selected_alternative_id = NA_character_, expected_net_benefit = NA_real_, expected_outcome = NA_character_))
  decision <- decisions[1L]
  alt_id <- aq_vnext_default(decision$selected_alternative_id[[1]], NA_character_)
  fin <- decision_context$financial_impacts
  expected_net <- if (nrow(fin) && "alternative_id" %in% names(fin)) aq_decision_summary_max(fin[alternative_id == alt_id, net_benefit]) else NA_real_
  expected_outcome <- if ("expected_outcome" %in% names(decision)) aq_vnext_default(aq_business_flat_chr(decision$expected_outcome)[1L], NA_character_) else NA_character_
  list(decision_id = decision$decision_id[[1]], selected_alternative_id = alt_id, expected_net_benefit = expected_net, expected_outcome = expected_outcome)
}

aq_decision_actual_value <- function(x, default = NA_real_) {
  if (is.null(x)) return(default)
  if (is.data.frame(x) && "actual_value" %in% names(x)) return(aq_business_num(x$actual_value, default))
  if (is.list(x) && !is.null(x$actual_value)) return(aq_business_num(x$actual_value, default))
  aq_business_num(x, default)
}

aq_decision_review_status <- function(expected_net, actual_value, assumption_status = NA_character_) {
  assumption_status <- aq_vnext_default(as.character(assumption_status)[1L], NA_character_)
  if (is.na(actual_value)) return("outcome_missing")
  if (!is.na(expected_net) && actual_value >= expected_net && !identical(assumption_status, "failed")) return("validated")
  if (!is.na(expected_net) && actual_value >= 0 && actual_value < expected_net) return("partial")
  if (identical(assumption_status, "failed")) return("assumption_failed")
  "negative_evidence"
}

#' Review a Decision Outcome
#'
#' @param decision_context An `aq_decision_context` object.
#' @param decision_id Optional decision id to review.
#' @param realized_outcome Realized outcome description or structured record.
#' @param actual_value Optional numeric realized value.
#' @param actual_execution_state Implementation/execution state.
#' @param lessons_learned Lessons learned.
#' @param strategy_implications Strategy implications.
#' @param lever_implications Lever implications.
#' @param assumption_updates Assumption update notes.
#' @param future_recommendations Future recommendation notes.
#' @param review_id Optional review id.
#' @param review_date Optional review date.
#' @param assumption_status Whether assumptions held, failed, or remain partial.
#'
#' @return A data.table decision review record.
#' @export
aq_review_decision <- function(
  decision_context,
  decision_id = NULL,
  realized_outcome = NA_character_,
  actual_value = NA_real_,
  actual_execution_state = "completed",
  lessons_learned = NA_character_,
  strategy_implications = NA_character_,
  lever_implications = NA_character_,
  assumption_updates = NA_character_,
  future_recommendations = NA_character_,
  review_id = NULL,
  review_date = Sys.Date(),
  assumption_status = NA_character_
) {
  if (!inherits(decision_context, "aq_decision_context")) stop("decision_context must be created by aq_decision_context().", call. = FALSE)
  expected <- aq_decision_expected_outcome(decision_context, decision_id)
  actual_value <- aq_decision_actual_value(actual_value)
  variance <- if (!is.na(actual_value) && !is.na(expected$expected_net_benefit)) actual_value - expected$expected_net_benefit else NA_real_
  status <- aq_decision_review_status(expected$expected_net_benefit, actual_value, assumption_status)
  data.table::data.table(
    review_id = aq_vnext_default(review_id, aq_vnext_id("decision_review")),
    decision_context_id = decision_context$decision_context_id,
    decision_id = expected$decision_id,
    selected_alternative_id = expected$selected_alternative_id,
    expected_outcome = expected$expected_outcome,
    expected_net_benefit = expected$expected_net_benefit,
    realized_outcome = paste(aq_business_flat_chr(realized_outcome), collapse = ", "),
    actual_value = actual_value,
    variance = variance,
    actual_execution_state = actual_execution_state,
    review_status = status,
    assumption_status = aq_vnext_default(assumption_status, NA_character_),
    lessons_learned = paste(aq_business_flat_chr(lessons_learned), collapse = ", "),
    strategy_implications = paste(aq_business_flat_chr(strategy_implications), collapse = ", "),
    lever_implications = paste(aq_business_flat_chr(lever_implications), collapse = ", "),
    assumption_updates = paste(aq_business_flat_chr(assumption_updates), collapse = ", "),
    future_recommendations = paste(aq_business_flat_chr(future_recommendations), collapse = ", "),
    review_date = as.character(review_date),
    created_at = as.character(aq_vnext_now())
  )
}

#' Build a Decision Timeline
#'
#' @param decision_context An `aq_decision_context` object.
#' @param reviews Optional decision review rows.
#'
#' @return A data.table timeline.
#' @export
aq_decision_timeline <- function(decision_context, reviews = NULL) {
  if (!inherits(decision_context, "aq_decision_context")) stop("decision_context must be created by aq_decision_context().", call. = FALSE)
  rows <- list()
  add <- function(event_type, event_id, status, title, detail = NA_character_, event_time = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      decision_context_id = decision_context$decision_context_id,
      event_type = event_type,
      event_id = event_id,
      status = status,
      title = title,
      detail = detail,
      event_time = as.character(event_time)
    )
  }
  add(
    "decision_context",
    decision_context$decision_context_id,
    aq_decision_chr1(decision_context$context$status, "draft"),
    aq_decision_chr1(decision_context$context$title, "Decision Context"),
    aq_decision_chr1(decision_context$context$decision_question),
    decision_context$created_at
  )
  if (nrow(decision_context$alternatives)) {
    for (i in seq_len(nrow(decision_context$alternatives))) {
      add(
        "alternative",
        aq_decision_chr1(decision_context$alternatives$alternative_id[i]),
        aq_decision_chr1(decision_context$alternatives$status[i], "candidate"),
        aq_decision_chr1(decision_context$alternatives$name[i], aq_decision_chr1(decision_context$alternatives$alternative_id[i], "Alternative")),
        aq_decision_chr1(decision_context$alternatives$alternative_type[i])
      )
    }
  }
  if (nrow(decision_context$recommendations)) {
    for (i in seq_len(nrow(decision_context$recommendations))) {
      add(
        "recommendation",
        aq_decision_chr1(decision_context$recommendations$recommendation_id[i]),
        aq_decision_chr1(decision_context$recommendations$recommendation_category[i], "recommended"),
        aq_decision_chr1(decision_context$recommendations$preferred_alternative_id[i], "Recommendation"),
        aq_decision_chr1(decision_context$recommendations$evidence_basis[i])
      )
    }
  }
  if (nrow(decision_context$decisions)) {
    for (i in seq_len(nrow(decision_context$decisions))) {
      add(
        "decision",
        aq_decision_chr1(decision_context$decisions$decision_id[i]),
        aq_decision_chr1(decision_context$decisions$decision[i], "recorded"),
        aq_decision_chr1(decision_context$decisions$selected_alternative_id[i], "Decision"),
        aq_decision_chr1(decision_context$decisions$approver[i]),
        aq_decision_chr1(decision_context$decisions$review_date[i])
      )
    }
  }
  if (nrow(decision_context$outcomes)) {
    for (i in seq_len(nrow(decision_context$outcomes))) {
      add(
        "outcome",
        aq_decision_chr1(decision_context$outcomes$outcome_review_id[i]),
        aq_decision_chr1(decision_context$outcomes$actual_execution_state[i], "not_started"),
        aq_decision_chr1(decision_context$outcomes$decision_id[i], "Outcome Review"),
        paste(aq_business_flat_chr(decision_context$outcomes$realized_outcomes[i]), collapse = ", ")
      )
    }
  }
  reviews <- if (is.null(reviews)) data.table::data.table() else data.table::as.data.table(reviews)
  if (nrow(reviews)) {
    for (i in seq_len(nrow(reviews))) {
      add(
        "learning",
        aq_decision_chr1(reviews$review_id[i]),
        aq_decision_chr1(reviews$review_status[i], "reviewed"),
        aq_decision_chr1(reviews$decision_id[i], "Learning"),
        aq_decision_chr1(reviews$lessons_learned[i]),
        aq_decision_chr1(reviews$review_date[i])
      )
    }
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Summarize Decision Learning
#'
#' @param decision_context An `aq_decision_context` object.
#' @param reviews Decision review rows.
#'
#' @return A data.table learning summary.
#' @export
aq_decision_learning_summary <- function(decision_context, reviews) {
  reviews <- data.table::as.data.table(reviews)
  if (!nrow(reviews)) {
    return(data.table::data.table(decision_context_id = decision_context$decision_context_id, learning_state = "no_reviews", validated_decisions = 0L, negative_evidence = 0L, recommendation = "Schedule outcome review."))
  }
  data.table::data.table(
    decision_context_id = decision_context$decision_context_id,
    reviews = nrow(reviews),
    validated_decisions = sum(reviews$review_status == "validated", na.rm = TRUE),
    partial_decisions = sum(reviews$review_status == "partial", na.rm = TRUE),
    failed_assumptions = sum(reviews$review_status == "assumption_failed", na.rm = TRUE),
    negative_evidence = sum(reviews$review_status == "negative_evidence", na.rm = TRUE),
    total_variance = sum(reviews$variance, na.rm = TRUE),
    learning_state = if (any(reviews$review_status == "validated")) "validated_organizational_evidence" else if (any(reviews$review_status %in% c("negative_evidence", "assumption_failed"))) "revision_evidence" else "partial_learning",
    recommendation = if (any(reviews$review_status == "validated")) "Promote decision pattern as reusable evidence with applicability limits." else if (any(reviews$review_status %in% c("negative_evidence", "assumption_failed"))) "Review strategy, tactic, lever guidance, and assumptions before reusing this decision pattern." else "Keep monitoring before promoting durable knowledge."
  )
}

#' Create a Decision Memory Artifact
#'
#' @param decision_context An `aq_decision_context` object.
#' @param reviews Decision review rows.
#' @param artifact_id Optional artifact id.
#'
#' @return A canonical decision-memory artifact.
#' @export
aq_decision_memory_artifact <- function(decision_context, reviews = NULL, artifact_id = NULL) {
  if (!inherits(decision_context, "aq_decision_context")) stop("decision_context must be created by aq_decision_context().", call. = FALSE)
  reviews <- if (is.null(reviews)) data.table::data.table() else data.table::as.data.table(reviews)
  timeline <- aq_decision_timeline(decision_context, reviews)
  learning <- aq_decision_learning_summary(decision_context, reviews)
  artifact_id <- aq_vnext_default(artifact_id, aq_vnext_id("decision_memory_artifact"))
  artifact <- new_table_artifact(
    id = artifact_id,
    title = "Decision Lifecycle and Organizational Learning",
    data = timeline,
    source_generator = "aq_decision_memory_artifact",
    tags = c("vnext", "decision_management", "organizational_memory", "learning"),
    dependencies = aq_vnext_unique_chr(c(decision_context$decision_context_id, decision_context$business_intent$intent_id)),
    version = "aq_decision_memory_artifact_v1",
    metadata = list(
      artifact_type = "decision_memory_artifact",
      decision_context_id = decision_context$decision_context_id,
      reviews = reviews,
      timeline = timeline,
      learning = learning,
      supported_actions = aq_decision_memory_supported_actions()
    )
  )
  aq_vnext_attach_envelope(
    artifact,
    artifact_id = artifact_id,
    artifact_type = "decision_memory_artifact",
    artifact_version = "aq_decision_memory_artifact_v1",
    parent_artifact_ids = aq_vnext_unique_chr(c(decision_context$decision_context_id, decision_context$business_intent$intent_id)),
    lineage = list(
      decision_context_id = decision_context$decision_context_id,
      review_ids = if ("review_id" %in% names(reviews)) reviews$review_id else character()
    ),
    task = "decision_management",
    operator = "decision_lifecycle_review",
    engine = "deterministic_decision_memory",
    specification_id = decision_context$decision_context_id,
    supported_actions = aq_decision_memory_supported_actions(),
    producer = "aq_decision_memory_artifact"
  )
}

aq_decision_context_fixture <- function() {
  intent <- aq_business_intent_fixture()
  aq_decision_context(
    context = list(
      decision_context_id = "decision_next_quarter_budget",
      title = "Next-Quarter Paid Search Budget",
      decision_question = "Should paid-search budget be increased next quarter, piloted outside the validated range, or kept at baseline?",
      organizational_owner = "CMO",
      decision_domain = "marketing",
      organizational_scope = "function",
      related_mission = "mission_growth",
      related_objectives = list("objective_revenue_growth"),
      related_strategies = list("strategy_qualified_demand"),
      related_tactics = list("tactic_paid_search"),
      related_levers = list("lever_paid_search_budget"),
      decision_deadline = "2026-09-30",
      time_horizon = "next_quarter",
      current_state = "baseline_budget",
      authority = "authority_marketing_advisory",
      coverage = "coverage_marketing_only",
      status = "draft"
    ),
    alternatives = list(
      list(alternative_id = "alt_baseline", decision_context_id = "decision_next_quarter_budget", name = "Continue current policy", alternative_type = "do_nothing", baseline = TRUE, levers_changed = list(character()), authority_compatible = TRUE, scope_compatible = TRUE),
      list(alternative_id = "alt_increase_validated", decision_context_id = "decision_next_quarter_budget", name = "Increase within validated range", alternative_type = "partial_implementation", baseline = FALSE, levers_changed = list("lever_paid_search_budget"), proposed_lever_settings = list("110000 weekly budget"), authority_compatible = TRUE, scope_compatible = TRUE),
      list(alternative_id = "alt_pilot_outside_range", decision_context_id = "decision_next_quarter_budget", name = "Pilot outside validated range", alternative_type = "pilot", baseline = FALSE, levers_changed = list("lever_paid_search_budget"), proposed_lever_settings = list("140000 weekly budget"), authority_compatible = FALSE, scope_compatible = TRUE, coverage_limitation = "requires approval beyond current delegated authority")
    ),
    criteria = list(
      list(criterion_id = "criterion_net_value", name = "Expected net value", direction = "maximize", weight = 0.4, hard_constraint = FALSE),
      list(criterion_id = "criterion_downside", name = "Downside risk", direction = "minimize", weight = 0.2, hard_constraint = FALSE),
      list(criterion_id = "criterion_authority", name = "Authority compatibility", direction = "maximize", hard_constraint = TRUE)
    ),
    financial_impacts = list(
      list(financial_id = "fin_baseline", alternative_id = "alt_baseline", initial_cost = 0, recurring_cost = 100000, expected_benefit = 110000, downside_estimate = 0, upside_estimate = 20000, opportunity_cost = 15000, source_type = "observed", confidence = 0.8),
      list(financial_id = "fin_validated", alternative_id = "alt_increase_validated", initial_cost = 0, recurring_cost = 110000, expected_benefit = 140000, downside_estimate = -10000, upside_estimate = 45000, opportunity_cost = 0, source_type = "modeled", confidence = 0.7),
      list(financial_id = "fin_pilot", alternative_id = "alt_pilot_outside_range", initial_cost = 5000, recurring_cost = 140000, expected_benefit = 160000, downside_estimate = -35000, upside_estimate = 80000, opportunity_cost = 0, source_type = "scenario_assumption", confidence = 0.45)
    ),
    uncertainties = list(
      list(uncertainty_id = "unc_pilot_transfer", alternative_id = "alt_pilot_outside_range", criterion_id = "criterion_net_value", uncertainty_category = "transfer_uncertainty", source = "scenario_assumption", direction = "two_sided", magnitude = "high", reducibility = "reducible", candidate_experiment = "bounded_pilot", time_to_resolve = "4 weeks", decision_sensitivity = "high", residual_uncertainty = "medium"),
      list(uncertainty_id = "unc_validated_cost", alternative_id = "alt_increase_validated", criterion_id = "criterion_downside", uncertainty_category = "execution_uncertainty", source = "modeled", direction = "downside", magnitude = "medium", reducibility = "partially_reducible", candidate_experiment = "monitoring", time_to_resolve = "2 weeks", decision_sensitivity = "medium")
    ),
    optionality = list(
      list(optionality_id = "opt_pilot_learn", alternative_id = "alt_pilot_outside_range", option_type = "learn", underlying_opportunity = "discover response outside validated range", enabling_action = "bounded pilot", exercise_action = "expand if response holds", exercise_window = "next quarter", enabling_cost = 5000, reversibility = TRUE, future_decisions_enabled = list(c("expand", "abandon")), options_foreclosed = list(character()), valuation_approach = "categorical", confidence = 0.65),
      list(optionality_id = "opt_baseline_delay", alternative_id = "alt_baseline", option_type = "defer", underlying_opportunity = "wait for more evidence", enabling_action = "continue baseline", exercise_action = "revisit later", exercise_window = "next planning cycle", reversibility = TRUE, future_decisions_enabled = list("stage"), options_foreclosed = list("near_term_growth"), valuation_approach = "categorical", confidence = 0.55)
    ),
    recommendations = list(recommendation_id = "rec_budget_validated", preferred_alternative_id = "alt_increase_validated", viable_alternatives = list(c("alt_baseline", "alt_pilot_outside_range")), recommendation_category = "choose_alternative", evidence_basis = "validated range has positive modeled value and authority compatibility", required_approvers = list("CMO")),
    decisions = list(decision_id = "decision_budget_pending", selected_alternative_id = "alt_increase_validated", alternatives_considered = list(c("alt_baseline", "alt_increase_validated", "alt_pilot_outside_range")), decision = "awaiting_approval", approver = "CMO", review_date = "2026-12-31"),
    outcomes = list(outcome_review_id = "outcome_budget_pending", decision_id = "decision_budget_pending", actual_execution_state = "not_started", realized_outcomes = NA_character_),
    business_intent = intent,
    variable_semantics = intent$variable_semantics
  )
}

#' QA for Decision Management
#'
#' @return A data.table of deterministic QA checks.
#' @export
qa_decision_management_framework <- function() {
  rows <- list()
  add <- function(check, ok, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(suite = "decision_management_framework", check = check, status = if (isTRUE(ok)) "pass" else "fail", message = message)
  }
  ctx <- aq_decision_context_fixture()
  add("decision_context_object", inherits(ctx, "aq_decision_context") && nzchar(ctx$decision_context_id))
  add("explicit_question", nzchar(aq_business_flat_chr(ctx$context$decision_question)[1L]))
  add("baseline_supported", any(vapply(ctx$alternatives$baseline, aq_business_bool, logical(1L))) && "do_nothing" %in% aq_business_flat_chr(ctx$alternatives$alternative_type))
  add("multiple_alternatives", nrow(ctx$alternatives) >= 3L)
  add("financial_calculations", all(c("net_benefit", "simple_roi") %in% names(ctx$financial_impacts)) && any(is.finite(ctx$financial_impacts$net_benefit)))
  add("uncertainty_structured", nrow(ctx$uncertainties) >= 2L && all(c("uncertainty_category", "reducibility", "decision_sensitivity") %in% names(ctx$uncertainties)))
  opt <- aq_assess_decision_optionality(ctx)
  add("optionality_structured", all(c("learn", "defer") %in% opt$option_type) && any(opt$optionality_assessment %in% c("high_positive_optionality", "moderate_positive_optionality")))
  assessment <- aq_assess_decision_alternatives(ctx)
  add("alternative_assessment", nrow(assessment) == nrow(ctx$alternatives) && all(c("authority_failure", "missing_information", "recommendation_category") %in% names(assessment)))
  add("authority_escalation", any(assessment$alternative_id == "alt_pilot_outside_range" & assessment$authority_failure & assessment$recommendation_category == "escalate"))
  add("information_value_candidate", any(assessment$alternative_id == "alt_pilot_outside_range" & assessment$reducible_uncertainty_count > 0L))
  validation <- aq_validate_decision_context(ctx)
  add("validation_passes", !any(validation$status %in% c("fail", "error")), paste(validation$message, collapse = " | "))
  bad <- aq_decision_context(context = list(decision_context_id = "bad_decision", decision_question = "Bad?"), alternatives = list(alternative_id = "alt_only", alternative_type = "bogus", baseline = FALSE))
  bad_validation <- aq_validate_decision_context(bad)
  add("invalid_alternative_detected", any(bad_validation$status == "fail"))
  artifact <- aq_decision_context_artifact(ctx)
  add("artifact_constructed", inherits(artifact, "aq_artifact") && identical(artifact$metadata$artifact_type, "decision_context_artifact"))
  add("artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(artifact)) && identical(aq_artifact_envelope(artifact)$operator, "decision_alternative_assessment"))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' QA for Decision Lifecycle and Organizational Memory
#'
#' @return A data.table of deterministic QA checks.
#' @export
qa_decision_lifecycle_framework <- function() {
  rows <- list()
  add <- function(check, ok, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "decision_lifecycle_framework",
      check = check,
      status = if (isTRUE(ok)) "pass" else "fail",
      message = message
    )
  }

  ctx <- aq_decision_context_fixture()
  review <- aq_review_decision(
    ctx,
    decision_id = "decision_budget_pending",
    realized_outcome = "Validated paid-search response inside delegated range.",
    actual_value = 36000,
    lessons_learned = "Validated range remains a usable decision boundary.",
    strategy_implications = "Continue scaling qualified-demand strategy inside validated range.",
    lever_implications = "Paid-search budget can be adjusted within current guardrails.",
    assumption_updates = "Response curve held for the next-quarter campaign window.",
    future_recommendations = "Monitor saturation before testing outside range.",
    assumption_status = "held"
  )
  add("review_created", nrow(review) == 1L && review$decision_id[[1]] == "decision_budget_pending")
  add("review_variance", is.finite(review$variance[[1]]) && review$variance[[1]] == 6000)
  add("review_status_validated", identical(review$review_status[[1]], "validated"))

  partial <- aq_review_decision(ctx, decision_id = "decision_budget_pending", actual_value = 10000, assumption_status = "held")
  add("partial_status", identical(partial$review_status[[1]], "partial"))

  negative <- aq_review_decision(ctx, decision_id = "decision_budget_pending", actual_value = -1000, assumption_status = "held")
  add("negative_status", identical(negative$review_status[[1]], "negative_evidence"))

  failed_assumption <- aq_review_decision(ctx, decision_id = "decision_budget_pending", actual_value = 36000, assumption_status = "failed")
  add("assumption_failure_status", identical(failed_assumption$review_status[[1]], "assumption_failed"))

  timeline <- aq_decision_timeline(ctx, review)
  add("timeline_events", all(c("decision_context", "alternative", "recommendation", "decision", "outcome", "learning") %in% timeline$event_type))
  add("timeline_statuses", all(nzchar(timeline$status)))

  learning <- aq_decision_learning_summary(ctx, review)
  add("learning_summary", learning$validated_decisions[[1]] == 1L && identical(learning$learning_state[[1]], "validated_organizational_evidence"))

  memory <- aq_decision_memory_artifact(ctx, review)
  add("memory_artifact_constructed", inherits(memory, "aq_artifact") && identical(memory$metadata$artifact_type, "decision_memory_artifact"))
  add("memory_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(memory)))
  add("memory_artifact_learning", identical(memory$metadata$learning$learning_state[[1]], "validated_organizational_evidence"))
  add("memory_artifact_supported_actions", all(c("decision_review", "outcome_review", "knowledge_promotion") %in% memory$metadata$supported_actions))

  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
