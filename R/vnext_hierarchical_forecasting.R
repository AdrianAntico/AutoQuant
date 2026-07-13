# AutoQuant vNext hierarchical forecasting and deterministic reconciliation.

#' Create a vNext Hierarchy Specification
#'
#' @param hierarchy A data frame containing hierarchy nodes.
#' @param entity_id Column containing node/entity identifiers.
#' @param parent_id Column containing parent identifiers. Root nodes should have
#'   `NA`, empty string, or `root_id` parent values.
#' @param hierarchy_level Optional column containing level labels.
#' @param root_id Optional expected root identifier.
#' @param aggregation_method Aggregation method. Phase 14 supports `"sum"`,
#'   `"mean"`, and `"weighted"`.
#' @param weight_col Optional weight column for weighted aggregation.
#' @param hierarchy_version Hierarchy version label.
#' @param hierarchy_spec_id Optional specification id.
#' @param supported_downstream_actions Supported next actions.
#'
#' @return An `aq_hierarchy_spec` object.
#' @export
aq_hierarchy_spec <- function(
  hierarchy,
  entity_id = "entity",
  parent_id = "parent",
  hierarchy_level = "level",
  root_id = NULL,
  aggregation_method = "sum",
  weight_col = NULL,
  hierarchy_version = "v1",
  hierarchy_spec_id = NULL,
  supported_downstream_actions = c("forecast", "reconcile", "assess", "report", "campaign_review")
) {
  aggregation_method <- match.arg(tolower(aggregation_method), c("sum", "mean", "weighted"))
  nodes <- data.table::as.data.table(data.table::copy(hierarchy))
  if (is.null(hierarchy_spec_id)) {
    hierarchy_spec_id <- aq_vnext_id(paste("hierarchy_spec", entity_id, parent_id, hierarchy_version, sep = "_"))
  }
  spec <- list(
    hierarchy_spec_id = as.character(hierarchy_spec_id)[1L],
    schema_version = "aq_hierarchy_spec_v1",
    nodes = nodes,
    entity_id = as.character(entity_id)[1L],
    parent_id = as.character(parent_id)[1L],
    hierarchy_level = as.character(hierarchy_level)[1L],
    root_id = if (is.null(root_id)) NA_character_ else as.character(root_id)[1L],
    aggregation_method = aggregation_method,
    weight_col = if (is.null(weight_col)) NA_character_ else as.character(weight_col)[1L],
    hierarchy_version = as.character(hierarchy_version)[1L],
    supported_downstream_actions = aq_vnext_unique_chr(supported_downstream_actions),
    created_at = aq_vnext_now()
  )
  class(spec) <- c("aq_hierarchy_spec", "list")
  spec
}

aq_hierarchy_normalized_nodes <- function(spec) {
  nodes <- data.table::as.data.table(data.table::copy(spec$nodes))
  entity_col <- spec$entity_id
  parent_col <- spec$parent_id
  nodes[, .aq_entity_id := as.character(get(entity_col))]
  nodes[, .aq_parent_id := as.character(get(parent_col))]
  nodes[is.na(.aq_parent_id) | !nzchar(.aq_parent_id), .aq_parent_id := NA_character_]
  if (!is.na(spec$root_id)) {
    nodes[.aq_parent_id == spec$root_id & .aq_entity_id == spec$root_id, .aq_parent_id := NA_character_]
  }
  nodes
}

aq_hierarchy_cycle_nodes <- function(nodes) {
  parent_map <- stats::setNames(nodes$.aq_parent_id, nodes$.aq_entity_id)
  cyclic <- character()
  for (node in nodes$.aq_entity_id) {
    seen <- character()
    current <- node
    repeat {
      parent <- unname(parent_map[current])
      if (!length(parent) || is.na(parent) || !nzchar(parent)) break
      if (parent %in% seen || identical(parent, node)) {
        cyclic <- unique(c(cyclic, node, parent, seen))
        break
      }
      seen <- c(seen, current)
      current <- parent
    }
  }
  unique(cyclic)
}

#' Validate a vNext Hierarchy Specification
#'
#' @param spec An `aq_hierarchy_spec`.
#'
#' @return A `data.table` of deterministic hierarchy diagnostics.
#' @export
aq_validate_hierarchy_spec <- function(spec) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  if (!inherits(spec, "aq_hierarchy_spec")) {
    add("hierarchy_spec_class", "fail", "spec must be created by aq_hierarchy_spec().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("hierarchy_spec_class", "pass", "hierarchy specification is typed.", "info")
  required <- c(spec$entity_id, spec$parent_id)
  if (!is.na(spec$hierarchy_level)) required <- unique(c(required, spec$hierarchy_level))
  if (identical(spec$aggregation_method, "weighted")) required <- unique(c(required, spec$weight_col))
  missing <- setdiff(required, names(spec$nodes))
  if (length(missing)) {
    add("required_columns", "fail", paste("Missing hierarchy column(s):", paste(missing, collapse = ", ")))
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("required_columns", "pass", "required hierarchy columns are present.", "info")
  nodes <- aq_hierarchy_normalized_nodes(spec)
  duplicate_entities <- nodes[, .N, by = .aq_entity_id][N > 1L]
  if (nrow(duplicate_entities)) add("duplicate_entity_ids", "fail", paste("duplicate entity ids:", paste(duplicate_entities$.aq_entity_id, collapse = ", "))) else add("duplicate_entity_ids", "pass", "entity ids are unique.", "info")
  missing_entity <- nodes[is.na(.aq_entity_id) | !nzchar(.aq_entity_id)]
  if (nrow(missing_entity)) add("missing_entity_ids", "fail", paste("missing entity ids:", nrow(missing_entity))) else add("missing_entity_ids", "pass", "entity ids are populated.", "info")
  missing_parents <- setdiff(stats::na.omit(nodes$.aq_parent_id), nodes$.aq_entity_id)
  if (length(missing_parents)) add("missing_parents", "fail", paste("missing parent ids:", paste(missing_parents, collapse = ", "))) else add("missing_parents", "pass", "all non-root parents exist.", "info")
  roots <- nodes[is.na(.aq_parent_id), .aq_entity_id]
  if (length(roots) == 1L) add("root_count", "pass", paste("root:", roots), "info") else add("root_count", "fail", paste("expected exactly one root; found:", length(roots)))
  if (!is.na(spec$root_id) && length(roots) == 1L && !identical(roots, spec$root_id)) add("root_identity", "fail", paste("root does not match root_id:", spec$root_id)) else add("root_identity", "pass", "root identity is valid.", "info")
  self_parent <- nodes[.aq_entity_id == .aq_parent_id, .aq_entity_id]
  if (length(self_parent)) add("self_parent", "fail", paste("entity cannot parent itself:", paste(self_parent, collapse = ", "))) else add("self_parent", "pass", "no self-parent relationships.", "info")
  cycle_nodes <- aq_hierarchy_cycle_nodes(nodes)
  if (length(cycle_nodes)) add("cycles", "fail", paste("cycle detected involving:", paste(cycle_nodes, collapse = ", "))) else add("cycles", "pass", "no cycles detected.", "info")
  if (identical(spec$aggregation_method, "weighted")) {
    bad_weights <- nodes[is.na(get(spec$weight_col)) | !is.finite(as.numeric(get(spec$weight_col)))]
    if (nrow(bad_weights)) add("weights", "fail", "weighted aggregation requires finite weights.") else add("weights", "pass", "weights are finite.", "info")
  } else {
    add("aggregation_method", "pass", paste("aggregation method:", spec$aggregation_method), "info")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_hierarchy_children_map <- function(nodes) {
  split(nodes$.aq_entity_id[!is.na(nodes$.aq_parent_id)], nodes$.aq_parent_id[!is.na(nodes$.aq_parent_id)])
}

aq_hierarchy_descendants <- function(nodes, entity) {
  children <- aq_hierarchy_children_map(nodes)
  out <- character()
  frontier <- children[[entity]]
  while (length(frontier)) {
    out <- unique(c(out, frontier))
    frontier <- unique(unlist(children[frontier], use.names = FALSE))
  }
  out
}

aq_hierarchy_leaf_nodes <- function(nodes) {
  parents <- stats::na.omit(unique(nodes$.aq_parent_id))
  setdiff(nodes$.aq_entity_id, parents)
}

aq_hierarchy_aggregate <- function(data, value_col, actual_col = NULL, group_cols, method = "sum", weights = NULL) {
  dt <- data.table::as.data.table(data)
  if (identical(method, "mean")) {
    out <- dt[, .(forecast = mean(get(value_col), na.rm = TRUE)), by = group_cols]
    if (!is.null(actual_col) && actual_col %in% names(dt)) out[, actual := dt[, mean(get(actual_col), na.rm = TRUE), by = group_cols]$V1]
  } else if (identical(method, "weighted")) {
    if (is.null(weights)) stop("weights are required for weighted hierarchy aggregation.", call. = FALSE)
    dt[, .aq_weight := as.numeric(weights[as.character(get(".aq_bottom_entity"))])]
    out <- dt[, .(forecast = sum(get(value_col) * .aq_weight, na.rm = TRUE) / sum(.aq_weight, na.rm = TRUE)), by = group_cols]
    if (!is.null(actual_col) && actual_col %in% names(dt)) out[, actual := dt[, sum(get(actual_col) * .aq_weight, na.rm = TRUE) / sum(.aq_weight, na.rm = TRUE), by = group_cols]$V1]
  } else {
    out <- dt[, .(forecast = sum(get(value_col), na.rm = TRUE)), by = group_cols]
    if (!is.null(actual_col) && actual_col %in% names(dt)) out[, actual := dt[, sum(get(actual_col), na.rm = TRUE), by = group_cols]$V1]
  }
  out
}

#' Reconcile a Panel Forecast to a Hierarchy
#'
#' @param panel_forecast An `aq_panel_forecast_result`.
#' @param hierarchy_spec An `aq_hierarchy_spec`.
#' @param method Reconciliation method. Phase 14 supports `"bottom_up"`.
#'
#' @return An `aq_hierarchical_forecast_result`.
#' @export
aq_reconcile_hierarchical_forecast <- function(panel_forecast, hierarchy_spec, method = "bottom_up") {
  if (!inherits(panel_forecast, "aq_panel_forecast_result")) stop("panel_forecast must be an aq_panel_forecast_result.", call. = FALSE)
  if (!inherits(hierarchy_spec, "aq_hierarchy_spec")) stop("hierarchy_spec must be an aq_hierarchy_spec.", call. = FALSE)
  method <- match.arg(tolower(method), "bottom_up")
  validation <- aq_validate_hierarchy_spec(hierarchy_spec)
  if (aq_vnext_has_validation_error(validation)) stop(paste(validation[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  nodes <- aq_hierarchy_normalized_nodes(hierarchy_spec)
  entity_col <- panel_forecast$entity
  base <- data.table::as.data.table(data.table::copy(panel_forecast$data))
  base[, .aq_bottom_entity := as.character(get(entity_col))]
  leaves <- aq_hierarchy_leaf_nodes(nodes)
  missing_leaves <- setdiff(base[, unique(.aq_bottom_entity)], leaves)
  if (length(missing_leaves)) stop("panel forecast contains entities that are not hierarchy leaves: ", paste(missing_leaves, collapse = ", "), call. = FALSE)
  group_cols <- c("forecast_date", "horizon")
  weights <- NULL
  if (identical(hierarchy_spec$aggregation_method, "weighted")) {
    weights <- stats::setNames(as.numeric(nodes[[hierarchy_spec$weight_col]]), nodes$.aq_entity_id)
  }
  hierarchy_rows <- list()
  for (node in nodes$.aq_entity_id) {
    descendants <- intersect(aq_hierarchy_descendants(nodes, node), leaves)
    if (!length(descendants)) next
    child_data <- base[.aq_bottom_entity %in% descendants]
    agg <- aq_hierarchy_aggregate(child_data, "forecast", actual_col = "actual", group_cols = group_cols, method = hierarchy_spec$aggregation_method, weights = weights)
    agg[, (entity_col) := node]
    agg[, hierarchy_entity := node]
    agg[, parent_entity := nodes[.aq_entity_id == node, .aq_parent_id]]
    agg[, hierarchy_level := if (!is.na(hierarchy_spec$hierarchy_level) && hierarchy_spec$hierarchy_level %in% names(nodes)) as.character(nodes[.aq_entity_id == node, get(hierarchy_spec$hierarchy_level)]) else NA_character_]
    agg[, aggregation_method := hierarchy_spec$aggregation_method]
    hierarchy_rows[[node]] <- agg
  }
  bottom <- base[, .SD, .SDcols = intersect(names(base), c(entity_col, "forecast_date", "horizon", "forecast", "actual", "lower", "upper", "interval_status", "interval_method"))]
  parent_lookup <- stats::setNames(nodes$.aq_parent_id, nodes$.aq_entity_id)
  level_lookup <- if (!is.na(hierarchy_spec$hierarchy_level) && hierarchy_spec$hierarchy_level %in% names(nodes)) {
    stats::setNames(as.character(nodes[[hierarchy_spec$hierarchy_level]]), nodes$.aq_entity_id)
  } else {
    stats::setNames(rep(NA_character_, nrow(nodes)), nodes$.aq_entity_id)
  }
  bottom[, hierarchy_entity := as.character(get(entity_col))]
  bottom[, parent_entity := unname(parent_lookup[as.character(get(entity_col))])]
  bottom[, hierarchy_level := unname(level_lookup[as.character(get(entity_col))])]
  bottom[, aggregation_method := "bottom"]
  reconciled <- data.table::rbindlist(c(list(bottom), hierarchy_rows), use.names = TRUE, fill = TRUE)
  data.table::setorderv(reconciled, c("forecast_date", "horizon", "hierarchy_level", entity_col))
  existing_aggregate <- base[.aq_bottom_entity %in% setdiff(nodes$.aq_entity_id, leaves)]
  diagnostics <- data.table::data.table(
    diagnostic = c("method", "bottom_entities", "aggregate_entities", "existing_aggregate_rows", "constraint_violations"),
    value = c(method, length(unique(base$.aq_bottom_entity)), length(hierarchy_rows), nrow(existing_aggregate), 0L),
    severity = c("info", "info", "info", if (nrow(existing_aggregate)) "warning" else "info", "info"),
    message = c(
      "bottom-up deterministic reconciliation",
      "leaf forecasts used as reconciliation source",
      "aggregate forecasts generated from descendants",
      "existing aggregate rows are not used as reconciliation source",
      "bottom-up aggregate constraints are generated by construction"
    )
  )
  hierarchy_id <- aq_vnext_id("hierarchical_forecast")
  artifact <- new_table_artifact(
    id = hierarchy_id,
    title = "Hierarchical Forecast: Bottom-Up Reconciliation",
    data = reconciled,
    source_generator = "aq_reconcile_hierarchical_forecast",
    tags = c("vnext", "forecast", "hierarchy", "reconciliation"),
    dependencies = c(panel_forecast$forecast_id, hierarchy_spec$hierarchy_spec_id),
    version = "aq_hierarchical_forecast_artifact_v1",
    metadata = list(
      artifact_type = "hierarchical_forecast",
      forecast_id = hierarchy_id,
      panel_forecast_id = panel_forecast$forecast_id,
      hierarchy_spec_id = hierarchy_spec$hierarchy_spec_id,
      hierarchy_version = hierarchy_spec$hierarchy_version,
      entity = entity_col,
      parent_id = hierarchy_spec$parent_id,
      reconciliation_method = method,
      reconciliation_status = "completed",
      reconciliation_diagnostics = diagnostics,
      aggregation_method = hierarchy_spec$aggregation_method,
      hierarchy_nodes = nodes,
      supported_downstream_actions = c("assess", "compare", "report", "campaign_review")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = hierarchy_id,
    artifact_type = "hierarchical_forecast",
    artifact_version = "aq_hierarchical_forecast_artifact_v1",
    parent_artifact_ids = c(panel_forecast$forecast_id, hierarchy_spec$hierarchy_spec_id),
    lineage = list(
      panel_forecast_id = panel_forecast$forecast_id,
      forecast_spec_id = panel_forecast$forecast_spec_id,
      hierarchy_spec_id = hierarchy_spec$hierarchy_spec_id,
      reconciliation_method = method,
      hierarchy_version = hierarchy_spec$hierarchy_version
    ),
    task = "hierarchical_forecast",
    operator = "forecast_reconciliation",
    engine = panel_forecast$engine,
    specification_id = panel_forecast$forecast_spec_id,
    dataset_id = panel_forecast$spec$dataset_id,
    supported_actions = c("assess", "compare", "report", "campaign_review"),
    producer = "aq_reconcile_hierarchical_forecast"
  )
  result <- list(
    forecast_id = hierarchy_id,
    schema_version = "aq_hierarchical_forecast_result_v1",
    status = "success",
    panel_forecast_id = panel_forecast$forecast_id,
    forecast_spec_id = panel_forecast$forecast_spec_id,
    hierarchy_spec_id = hierarchy_spec$hierarchy_spec_id,
    entity = entity_col,
    target = panel_forecast$target,
    date = panel_forecast$date,
    frequency = panel_forecast$frequency,
    forecast_origin = panel_forecast$forecast_origin,
    horizon = panel_forecast$horizon,
    reconciliation_method = method,
    reconciliation_status = "completed",
    aggregation_method = hierarchy_spec$aggregation_method,
    data = reconciled,
    hierarchy_nodes = nodes,
    reconciliation_diagnostics = diagnostics,
    artifact = artifact,
    validation = validation,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_hierarchical_forecast_result", "list")
  result
}

#' Assess Hierarchical Forecast Evidence
#'
#' @param forecast An `aq_hierarchical_forecast_result`.
#'
#' @return An `aq_hierarchical_forecast_assessment_result`.
#' @export
aq_assess_hierarchical_forecast <- function(forecast) {
  if (!inherits(forecast, "aq_hierarchical_forecast_result")) stop("forecast must be an aq_hierarchical_forecast_result.", call. = FALSE)
  if (!"actual" %in% names(forecast$data) || all(is.na(forecast$data$actual))) stop("hierarchical forecast result does not contain realized actuals for assessment.", call. = FALSE)
  entity_col <- forecast$entity
  metrics <- aq_forecast_metrics(forecast$data)
  by_entity <- forecast$data[, aq_forecast_metrics(.SD), by = c(entity_col, "hierarchy_level")]
  by_level <- forecast$data[, aq_forecast_metrics(.SD), by = hierarchy_level]
  by_horizon <- forecast$data[, aq_forecast_metrics(.SD), by = horizon]
  aggregate_consistency <- forecast$data[, .(
    forecast_total = sum(forecast, na.rm = TRUE),
    actual_total = sum(actual, na.rm = TRUE),
    row_count = .N
  ), by = .(forecast_date, horizon, hierarchy_level)]
  assessment_id <- aq_vnext_id("hierarchical_forecast_assessment")
  artifact <- new_table_artifact(
    id = assessment_id,
    title = "Hierarchical Forecast Assessment",
    data = by_entity,
    source_generator = "aq_assess_hierarchical_forecast",
    tags = c("vnext", "forecast", "hierarchy", "assessment"),
    dependencies = forecast$forecast_id,
    version = "aq_hierarchical_forecast_assessment_artifact_v1",
    metadata = list(
      artifact_type = "hierarchical_forecast_assessment",
      forecast_id = forecast$forecast_id,
      aggregate_metrics = metrics,
      metrics_by_entity = by_entity,
      metrics_by_level = by_level,
      metrics_by_horizon = by_horizon,
      aggregate_consistency = aggregate_consistency,
      reconciliation_diagnostics = forecast$reconciliation_diagnostics
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = assessment_id,
    artifact_type = "hierarchical_forecast_assessment",
    artifact_version = "aq_hierarchical_forecast_assessment_artifact_v1",
    parent_artifact_ids = forecast$forecast_id,
    lineage = list(forecast_id = forecast$forecast_id, hierarchy_spec_id = forecast$hierarchy_spec_id, reconciliation_method = forecast$reconciliation_method),
    task = "hierarchical_forecast",
    operator = "forecast_assessment",
    engine = "deterministic_reconciliation",
    specification_id = forecast$forecast_spec_id,
    supported_actions = c("compare", "report", "campaign_review"),
    producer = "aq_assess_hierarchical_forecast"
  )
  result <- list(
    assessment_id = assessment_id,
    schema_version = "aq_hierarchical_forecast_assessment_result_v1",
    forecast_id = forecast$forecast_id,
    hierarchy_spec_id = forecast$hierarchy_spec_id,
    metrics = metrics,
    metrics_by_entity = by_entity,
    metrics_by_level = by_level,
    metrics_by_horizon = by_horizon,
    aggregate_consistency = aggregate_consistency,
    reconciliation_diagnostics = forecast$reconciliation_diagnostics,
    assessment_artifact = artifact,
    comparison_ready = TRUE,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_hierarchical_forecast_assessment_result", "list")
  result
}

#' Run Deterministic Rolling-Origin Hierarchical Forecast Evaluation
#'
#' @param panel_spec An `aq_panel_forecast_spec`.
#' @param hierarchy_spec An `aq_hierarchy_spec`.
#' @param data Panel data.
#' @param origins Optional forecast origins.
#' @param origin_count Optional number of origins.
#' @param method Reconciliation method.
#'
#' @return An `aq_hierarchical_forecast_backtest_result`.
#' @export
aq_rolling_origin_hierarchical_forecast <- function(panel_spec, hierarchy_spec, data, origins = NULL, origin_count = NULL, method = "bottom_up") {
  panel_backtest <- aq_rolling_origin_panel_forecast(panel_spec, data, origins = origins, origin_count = origin_count)
  forecasts <- lapply(panel_backtest$forecasts, aq_reconcile_hierarchical_forecast, hierarchy_spec = hierarchy_spec, method = method)
  assessments <- lapply(forecasts, aq_assess_hierarchical_forecast)
  result <- list(
    backtest_id = aq_vnext_id("hierarchical_forecast_backtest"),
    schema_version = "aq_hierarchical_forecast_backtest_result_v1",
    forecast_spec_id = panel_spec$forecast_spec_id,
    hierarchy_spec_id = hierarchy_spec$hierarchy_spec_id,
    origins = panel_backtest$origins,
    panel_backtest = panel_backtest,
    forecasts = forecasts,
    assessments = assessments,
    metrics = data.table::rbindlist(lapply(seq_along(assessments), function(i) data.table::copy(assessments[[i]]$metrics)[, forecast_origin := forecasts[[i]]$forecast_origin]), use.names = TRUE, fill = TRUE),
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_hierarchical_forecast_backtest_result", "list")
  result
}

aq_vnext_hierarchy_fixture <- function() {
  data.table::data.table(
    entity = c("all", "region_a", "region_b", "store_1", "store_2", "store_3", "store_4"),
    parent = c(NA_character_, "all", "all", "region_a", "region_a", "region_b", "region_b"),
    level = c("root", "region", "region", "store", "store", "store", "store"),
    weight = c(1, 1, 1, 1, 1, 1, 1)
  )
}

#' QA for vNext Hierarchical Forecasting Foundation
#'
#' @return A QA `data.table`.
#' @export
qa_vnext_hierarchical_forecasting_foundation <- function() {
  rows <- list()
  add <- function(check, ok, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_hierarchical_forecasting_foundation",
      check = check,
      status = if (isTRUE(ok)) "pass" else "fail",
      message = message
    )
  }
  hierarchy <- aq_vnext_hierarchy_fixture()
  hspec <- aq_hierarchy_spec(hierarchy, entity_id = "entity", parent_id = "parent", hierarchy_level = "level", root_id = "all", aggregation_method = "sum")
  validation <- aq_validate_hierarchy_spec(hspec)
  add("hierarchy_spec_constructed", inherits(hspec, "aq_hierarchy_spec"))
  add("hierarchy_validation_passes", !aq_vnext_has_validation_error(validation), paste(validation$message, collapse = " | "))
  dup <- data.table::rbindlist(list(hierarchy, hierarchy[entity == "store_1"]))
  add("duplicate_entity_detected", any(aq_validate_hierarchy_spec(aq_hierarchy_spec(dup, "entity", "parent", "level"))$check == "duplicate_entity_ids" & aq_validate_hierarchy_spec(aq_hierarchy_spec(dup, "entity", "parent", "level"))$status == "fail"))
  orphan <- data.table::copy(hierarchy)
  orphan[entity == "store_4", parent := "missing_parent"]
  orphan_validation <- aq_validate_hierarchy_spec(aq_hierarchy_spec(orphan, "entity", "parent", "level"))
  add("orphan_parent_detected", any(orphan_validation$check == "missing_parents" & orphan_validation$status == "fail"))
  cyclic <- data.table::copy(hierarchy)
  cyclic[entity == "all", parent := "store_1"]
  cycle_validation <- aq_validate_hierarchy_spec(aq_hierarchy_spec(cyclic, "entity", "parent", "level"))
  add("cycle_detected", any(cycle_validation$check == "cycles" & cycle_validation$status == "fail"))
  multi_root <- data.table::copy(hierarchy)
  multi_root[entity == "region_b", parent := NA_character_]
  multi_root_validation <- aq_validate_hierarchy_spec(aq_hierarchy_spec(multi_root, "entity", "parent", "level"))
  add("multiple_roots_detected", any(multi_root_validation$check == "root_count" & multi_root_validation$status == "fail"))
  dt <- aq_vnext_panel_forecast_fixture()
  pspec <- aq_panel_forecast_spec(
    entity = "entity",
    target = "demand",
    date = "date",
    frequency = "day",
    horizon = 3L,
    future_known_variables = "promo",
    static_entity_features = "store_size",
    forecast_strategy = "direct",
    engine_parameters = list(iterations = 5L, depth = 2L, learning_rate = 0.1, lag_periods = c(1L, 7L), rolling_windows = 3L, date_features = c("year", "month", "day", "dow", "day_index")),
    minimum_history = 20L,
    dataset_id = "qa_hierarchical_forecast_fixture"
  )
  if (requireNamespace("catboost", quietly = TRUE)) {
    panel <- aq_fit_panel_forecast(pspec, dt)
    hierarchical <- aq_reconcile_hierarchical_forecast(panel, hspec)
    assessment <- aq_assess_hierarchical_forecast(hierarchical)
    add("bottom_up_reconciliation", inherits(hierarchical, "aq_hierarchical_forecast_result") && any(hierarchical$data[[pspec$entity]] == "all"))
    root_rows <- hierarchical$data[get(pspec$entity) == "all"]
    leaf_sums <- hierarchical$data[get(pspec$entity) %in% c("store_1", "store_2", "store_3", "store_4"), .(leaf_sum = sum(forecast)), by = .(forecast_date, horizon)]
    root_check <- merge(root_rows[, .(forecast_date, horizon, forecast)], leaf_sums, by = c("forecast_date", "horizon"))
    add("aggregate_constraints_hold", all(abs(root_check$forecast - root_check$leaf_sum) < 1e-8))
    add("hierarchy_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(hierarchical$artifact)))
    add("aggregate_metrics", inherits(assessment, "aq_hierarchical_forecast_assessment_result") && nrow(assessment$metrics_by_level) > 0L)
    add("entity_metrics", nrow(assessment$metrics_by_entity) > 0L)
    add("assessment_artifact_integrity", !aq_vnext_has_validation_error(aq_validate_artifact(assessment$assessment_artifact)))
    backtest <- aq_rolling_origin_hierarchical_forecast(pspec, hspec, dt, origin_count = 2L)
    add("hierarchical_rolling_origin", inherits(backtest, "aq_hierarchical_forecast_backtest_result") && length(backtest$forecasts) == 2L)
    serialized <- unserialize(serialize(list(hierarchical = hierarchical$artifact, assessment = assessment$assessment_artifact), NULL))
    add("hierarchy_bundle_compatibility", all(vapply(serialized, function(x) !aq_vnext_has_validation_error(aq_validate_artifact(x)), logical(1L))))
    app_like <- list(hierarchical$artifact, assessment$assessment_artifact)
    add("analytics_shinyapp_compatibility", all(vapply(app_like, is.list, logical(1L))) && all(vapply(app_like, function(x) "campaign_review" %in% aq_supported_actions(x), logical(1L))))
  } else {
    add("catboost_available", FALSE, "catboost package is required for hierarchical forecast QA.")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
