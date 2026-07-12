#' AutoQuant Artifact Schema Framework
#'
#' Lightweight S3/list contracts for reusable AutoQuant artifacts. These
#' constructors are the foundation for future generator outputs consumed by
#' reports, dashboards, Shiny apps, APIs, and LLM agents.
#'
#' @section Contract:
#' Every artifact has common metadata:
#' `id`, `type`, `title`, `subtitle`, `description`, `tags`,
#' `dependencies`, `source_generator`, `creation_time`, and `version`.
#'
#' Type-specific payloads live in fields such as `data`, `object`,
#' `claim`, `status`, `nodes`, or `sections`.
#'
#' @name artifact_schema_framework
#' @keywords internal
NULL

aq_artifact_types <- function() {
  c(
    "table",
    "plot",
    "diagnostic",
    "finding",
    "warning",
    "metadata",
    "computation_graph",
    "display_plan",
    "quality_gate"
  )
}

aq_artifact_null_default <- function(value, default) {
  if (is.null(value)) default else value
}

aq_artifact_chr <- function(value, default = character()) {
  if (is.null(value)) {
    return(default)
  }
  value <- as.character(value)
  value[!is.na(value)]
}

aq_artifact_scalar_chr <- function(value, name, allow_null = FALSE) {
  if (is.null(value)) {
    if (isTRUE(allow_null)) {
      return(NULL)
    }
    stop(name, " is required.", call. = FALSE)
  }
  value <- as.character(value)[1L]
  if (is.na(value) || !nzchar(trimws(value))) {
    if (isTRUE(allow_null)) {
      return(NULL)
    }
    stop(name, " must be a non-empty character value.", call. = FALSE)
  }
  value
}

aq_artifact_now <- function() {
  Sys.time()
}

aq_artifact_new_common <- function(
  id,
  type,
  title,
  subtitle = NULL,
  description = NULL,
  tags = character(),
  dependencies = character(),
  source_generator = NULL,
  creation_time = aq_artifact_now(),
  version = "0.1.0",
  metadata = list()
) {
  type <- match.arg(type, aq_artifact_types())

  list(
    id = aq_artifact_scalar_chr(id, "id"),
    type = type,
    title = aq_artifact_scalar_chr(title, "title"),
    subtitle = aq_artifact_scalar_chr(subtitle, "subtitle", allow_null = TRUE),
    description = aq_artifact_scalar_chr(description, "description", allow_null = TRUE),
    tags = aq_artifact_chr(tags),
    dependencies = aq_artifact_chr(dependencies),
    source_generator = aq_artifact_scalar_chr(source_generator, "source_generator", allow_null = TRUE),
    creation_time = creation_time,
    version = aq_artifact_scalar_chr(version, "version"),
    metadata = aq_artifact_null_default(metadata, list())
  )
}

aq_artifact_finish <- function(x, type) {
  class(x) <- c(paste0("aq_", type, "_artifact"), "aq_artifact", "list")
  x
}

aq_artifact_first_non_null <- function(...) {
  values <- list(...)
  for (value in values) {
    if (!is.null(value) && length(value) > 0L) {
      return(value)
    }
  }
  NULL
}

aq_artifact_scalar_default <- function(value, default = NA_character_) {
  if (is.null(value) || !length(value)) {
    return(default)
  }
  value <- as.character(value)[1L]
  if (is.na(value) || !nzchar(trimws(value))) default else value
}

aq_artifact_actions <- function(value) {
  if (is.null(value)) {
    return(character())
  }
  unique(as.character(value[!is.na(value) & nzchar(trimws(as.character(value)))]))
}

aq_new_artifact_envelope <- function(
  artifact_id,
  artifact_type,
  artifact_version = "aq_artifact_envelope_v1",
  parent_artifact_ids = character(),
  lineage = list(),
  task = NA_character_,
  operator = NA_character_,
  engine = NA_character_,
  specification_id = NA_character_,
  dataset_id = NA_character_,
  prepared_dataset_id = NA_character_,
  transformation_id = NA_character_,
  model_id = NA_character_,
  campaign_references = list(),
  warnings = character(),
  supported_actions = character(),
  producer = NA_character_,
  consumer_expectations = list(),
  created_at = aq_artifact_now()
) {
  x <- list(
    artifact_id = aq_artifact_scalar_default(artifact_id),
    artifact_type = aq_artifact_scalar_default(artifact_type),
    artifact_version = aq_artifact_scalar_default(artifact_version),
    envelope_version = "aq_artifact_envelope_v1",
    parent_artifact_ids = aq_artifact_actions(parent_artifact_ids),
    lineage = aq_artifact_null_default(lineage, list()),
    task = aq_artifact_scalar_default(task),
    operator = aq_artifact_scalar_default(operator),
    engine = aq_artifact_scalar_default(engine),
    specification_id = aq_artifact_scalar_default(specification_id),
    dataset_id = aq_artifact_scalar_default(dataset_id),
    prepared_dataset_id = aq_artifact_scalar_default(prepared_dataset_id),
    transformation_id = aq_artifact_scalar_default(transformation_id),
    model_id = aq_artifact_scalar_default(model_id),
    campaign_references = aq_artifact_null_default(campaign_references, list()),
    warnings = aq_artifact_actions(warnings),
    supported_actions = aq_artifact_actions(supported_actions),
    producer = aq_artifact_scalar_default(producer),
    consumer_expectations = aq_artifact_null_default(consumer_expectations, list()),
    created_at = created_at
  )
  class(x) <- c("aq_artifact_envelope", "list")
  x
}

aq_artifact_infer_type <- function(x) {
  aq_artifact_scalar_default(aq_artifact_first_non_null(
    x$artifact_type,
    x$metadata$artifact_type,
    x$type,
    class(x)[1L]
  ))
}

aq_artifact_infer_id <- function(x) {
  aq_artifact_scalar_default(aq_artifact_first_non_null(
    x$artifact_id,
    x$id,
    x$spec_id,
    x$fit_id,
    x$prediction_id,
    x$scoring_id,
    x$assessment_id,
    x$monitoring_id,
    x$bundle_id
  ))
}

#' Return the Canonical AutoQuant Artifact Envelope
#'
#' @description
#' Returns the canonical analytical artifact envelope for an AutoQuant object.
#' Existing vNext artifacts carry an explicit envelope. Older table/plot/report
#' artifacts are normalized into the same shape from their common metadata.
#'
#' @param artifact An AutoQuant artifact or vNext result object.
#'
#' @return An `aq_artifact_envelope` list.
#' @export
aq_artifact_envelope <- function(artifact) {
  if (!is.null(artifact$artifact_envelope) && inherits(artifact$artifact_envelope, "aq_artifact_envelope")) {
    return(artifact$artifact_envelope)
  }
  metadata <- aq_artifact_null_default(artifact$metadata, list())
  aq_new_artifact_envelope(
    artifact_id = aq_artifact_infer_id(artifact),
    artifact_type = aq_artifact_infer_type(artifact),
    artifact_version = aq_artifact_scalar_default(aq_artifact_first_non_null(
      artifact$schema_version,
      artifact$version,
      metadata$schema_version
    )),
    parent_artifact_ids = aq_artifact_first_non_null(
      artifact$parent_artifact_ids,
      artifact$dependencies,
      metadata$parent_artifact_ids,
      metadata$dependencies
    ),
    lineage = aq_artifact_first_non_null(artifact$lineage, metadata$lineage, list()),
    task = aq_artifact_first_non_null(artifact$task, metadata$task),
    operator = aq_artifact_first_non_null(artifact$operator, metadata$operator, artifact$source_generator),
    engine = aq_artifact_first_non_null(artifact$engine, metadata$engine),
    specification_id = aq_artifact_first_non_null(artifact$spec_id, artifact$model_spec_id, metadata$model_spec_id),
    dataset_id = aq_artifact_first_non_null(artifact$dataset_id, metadata$dataset_id),
    prepared_dataset_id = aq_artifact_first_non_null(artifact$prepared_dataset_id, metadata$prepared_dataset_id),
    transformation_id = aq_artifact_first_non_null(
      artifact$fitted_transformation_id,
      metadata$fitted_transformation_id,
      metadata$transformation_spec_id
    ),
    model_id = aq_artifact_first_non_null(artifact$model_id, metadata$model_id),
    campaign_references = aq_artifact_first_non_null(artifact$campaign_references, metadata$campaign_references, list()),
    warnings = aq_artifact_first_non_null(artifact$warnings, metadata$warnings, character()),
    supported_actions = aq_artifact_first_non_null(
      artifact$supported_downstream_actions,
      artifact$supported_actions,
      metadata$supported_downstream_actions,
      metadata$supported_actions
    ),
    producer = aq_artifact_first_non_null(artifact$producer, metadata$producer, artifact$source_generator),
    consumer_expectations = aq_artifact_first_non_null(artifact$consumer_expectations, metadata$consumer_expectations, list()),
    created_at = aq_artifact_first_non_null(artifact$created_at, artifact$creation_time, metadata$created_at, aq_artifact_now())
  )
}

#' Return Supported Downstream Actions for an AutoQuant Artifact
#'
#' @param artifact An AutoQuant artifact or vNext result object.
#'
#' @return Character vector of supported actions.
#' @export
aq_supported_actions <- function(artifact) {
  aq_artifact_envelope(artifact)$supported_actions
}

#' Return Deterministic Artifact Relationships
#'
#' @param artifact An AutoQuant artifact or vNext result object.
#'
#' @return A `data.table` with parent artifact relationships.
#' @export
aq_artifact_relationships <- function(artifact) {
  envelope <- aq_artifact_envelope(artifact)
  parents <- aq_artifact_actions(envelope$parent_artifact_ids)
  if (!length(parents)) {
    return(data.table::data.table(
      artifact_id = character(),
      parent_artifact_id = character(),
      relationship = character(),
      artifact_type = character()
    ))
  }
  data.table::data.table(
    artifact_id = envelope$artifact_id,
    parent_artifact_id = parents,
    relationship = "derived_from",
    artifact_type = envelope$artifact_type
  )
}

#' Validate the Canonical AutoQuant Artifact Contract
#'
#' @param artifact An AutoQuant artifact or vNext result object.
#'
#' @return A `data.table` of deterministic validation diagnostics.
#' @export
aq_validate_artifact <- function(artifact) {
  envelope <- tryCatch(aq_artifact_envelope(artifact), error = identity)
  if (inherits(envelope, "error")) {
    return(data.table::data.table(
      check = "artifact_envelope",
      status = "fail",
      severity = "fail",
      message = conditionMessage(envelope)
    ))
  }
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      check = check,
      status = status,
      severity = severity,
      message = message
    )
  }
  required <- c("artifact_id", "artifact_type", "artifact_version", "envelope_version", "producer")
  missing <- required[!vapply(required, function(field) {
    value <- envelope[[field]]
    !is.null(value) && length(value) > 0L && !is.na(as.character(value)[1L]) && nzchar(trimws(as.character(value)[1L]))
  }, logical(1L))]
  if (length(missing)) {
    add("required_metadata", "fail", paste("Missing required envelope field(s):", paste(missing, collapse = ", ")))
  } else {
    add("required_metadata", "pass", "required envelope metadata is present.", "info")
  }
  if (!identical(envelope$envelope_version, "aq_artifact_envelope_v1")) {
    add("envelope_version", "fail", paste("Unsupported envelope version:", envelope$envelope_version))
  } else {
    add("envelope_version", "pass", "canonical envelope version is supported.", "info")
  }
  if (!is.list(envelope$lineage)) {
    add("lineage", "fail", "lineage must be a list.")
  } else {
    add("lineage", "pass", "lineage is list-based.", "info")
  }
  if (!is.character(envelope$supported_actions)) {
    add("supported_actions", "fail", "supported actions must be character.")
  } else {
    add("supported_actions", "pass", "supported actions are normalized.", "info")
  }
  if (!is.list(envelope$consumer_expectations)) {
    add("consumer_expectations", "fail", "consumer expectations must be a list.")
  } else {
    add("consumer_expectations", "pass", "consumer expectations are available.", "info")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create a Table Artifact
#'
#' @param id Artifact identifier.
#' @param title Artifact title.
#' @param data Table-like object, preferably a `data.table`.
#' @param subtitle,description Optional display text.
#' @param tags Character tags.
#' @param dependencies Artifact IDs this artifact depends on.
#' @param source_generator Generator that created the artifact.
#' @param creation_time Creation timestamp.
#' @param version Schema version.
#' @param metadata Additional metadata.
#' @param schema Optional column schema.
#'
#' @return An object with class `c("aq_table_artifact", "aq_artifact", "list")`.
#'
#' @family AutoQuant artifact constructors
#' @export
new_table_artifact <- function(
  id,
  title,
  data,
  subtitle = NULL,
  description = NULL,
  tags = character(),
  dependencies = character(),
  source_generator = NULL,
  creation_time = aq_artifact_now(),
  version = "0.1.0",
  metadata = list(),
  schema = NULL
) {
  x <- aq_artifact_new_common(
    id = id,
    type = "table",
    title = title,
    subtitle = subtitle,
    description = description,
    tags = tags,
    dependencies = dependencies,
    source_generator = source_generator,
    creation_time = creation_time,
    version = version,
    metadata = metadata
  )
  x$data <- data
  x$schema <- aq_artifact_null_default(schema, list())
  aq_artifact_finish(x, "table")
}

#' Create a Plot Artifact
#'
#' @inheritParams new_table_artifact
#' @param object Plot/widget object.
#' @param config Optional renderer/display config.
#'
#' @return An object with class `c("aq_plot_artifact", "aq_artifact", "list")`.
#'
#' @family AutoQuant artifact constructors
#' @export
new_plot_artifact <- function(
  id,
  title,
  object,
  subtitle = NULL,
  description = NULL,
  tags = character(),
  dependencies = character(),
  source_generator = NULL,
  creation_time = aq_artifact_now(),
  version = "0.1.0",
  metadata = list(),
  config = list()
) {
  x <- aq_artifact_new_common(
    id = id,
    type = "plot",
    title = title,
    subtitle = subtitle,
    description = description,
    tags = tags,
    dependencies = dependencies,
    source_generator = source_generator,
    creation_time = creation_time,
    version = version,
    metadata = metadata
  )
  x["object"] <- list(object)
  x$config <- aq_artifact_null_default(config, list())
  aq_artifact_finish(x, "plot")
}

#' Create a Diagnostic Artifact
#'
#' @inheritParams new_table_artifact
#' @param status Diagnostic status, such as `pass`, `warning`, or `fail`.
#' @param severity Diagnostic severity.
#' @param details Additional diagnostic details.
#'
#' @return An object with class `c("aq_diagnostic_artifact", "aq_artifact", "list")`.
#'
#' @family AutoQuant artifact constructors
#' @export
new_diagnostic_artifact <- function(
  id,
  title,
  status,
  severity = "info",
  details = list(),
  subtitle = NULL,
  description = NULL,
  tags = character(),
  dependencies = character(),
  source_generator = NULL,
  creation_time = aq_artifact_now(),
  version = "0.1.0",
  metadata = list()
) {
  x <- aq_artifact_new_common(
    id = id,
    type = "diagnostic",
    title = title,
    subtitle = subtitle,
    description = description,
    tags = tags,
    dependencies = dependencies,
    source_generator = source_generator,
    creation_time = creation_time,
    version = version,
    metadata = metadata
  )
  x$status <- aq_artifact_scalar_chr(status, "status")
  x$severity <- aq_artifact_scalar_chr(severity, "severity")
  x$details <- aq_artifact_null_default(details, list())
  aq_artifact_finish(x, "diagnostic")
}

#' Create a Finding Artifact
#'
#' @inheritParams new_table_artifact
#' @param claim Analytical claim.
#' @param evidence_ids Artifact or evidence IDs supporting the finding.
#' @param confidence Numeric confidence from 0 to 1, or `NA_real_` if unknown.
#' @param severity Finding severity.
#' @param caveats Character caveats.
#' @param recommended_follow_up Character follow-up actions.
#'
#' @return An object with class `c("aq_finding_artifact", "aq_artifact", "list")`.
#'
#' @family AutoQuant artifact constructors
#' @export
new_finding_artifact <- function(
  id,
  title,
  claim,
  evidence_ids = character(),
  confidence = NA_real_,
  severity = "info",
  caveats = character(),
  recommended_follow_up = character(),
  subtitle = NULL,
  description = NULL,
  tags = character(),
  dependencies = character(),
  source_generator = NULL,
  creation_time = aq_artifact_now(),
  version = "0.1.0",
  metadata = list()
) {
  x <- aq_artifact_new_common(
    id = id,
    type = "finding",
    title = title,
    subtitle = subtitle,
    description = description,
    tags = tags,
    dependencies = dependencies,
    source_generator = source_generator,
    creation_time = creation_time,
    version = version,
    metadata = metadata
  )
  x$claim <- aq_artifact_scalar_chr(claim, "claim")
  x$evidence_ids <- aq_artifact_chr(evidence_ids)
  x$confidence <- suppressWarnings(as.numeric(confidence[1L]))
  x$severity <- aq_artifact_scalar_chr(severity, "severity")
  x$caveats <- aq_artifact_chr(caveats)
  x$recommended_follow_up <- aq_artifact_chr(recommended_follow_up)
  aq_artifact_finish(x, "finding")
}

#' Create a Warning Artifact
#'
#' @inheritParams new_table_artifact
#' @param message Warning message.
#' @param severity Warning severity.
#' @param affected_artifacts Affected artifact IDs.
#'
#' @return An object with class `c("aq_warning_artifact", "aq_artifact", "list")`.
#'
#' @family AutoQuant artifact constructors
#' @export
new_warning_artifact <- function(
  id,
  title,
  message,
  severity = "warning",
  affected_artifacts = character(),
  subtitle = NULL,
  description = NULL,
  tags = character(),
  dependencies = character(),
  source_generator = NULL,
  creation_time = aq_artifact_now(),
  version = "0.1.0",
  metadata = list()
) {
  x <- aq_artifact_new_common(
    id = id,
    type = "warning",
    title = title,
    subtitle = subtitle,
    description = description,
    tags = tags,
    dependencies = dependencies,
    source_generator = source_generator,
    creation_time = creation_time,
    version = version,
    metadata = metadata
  )
  x$message <- aq_artifact_scalar_chr(message, "message")
  x$severity <- aq_artifact_scalar_chr(severity, "severity")
  x$affected_artifacts <- aq_artifact_chr(affected_artifacts)
  aq_artifact_finish(x, "warning")
}

#' Create a Metadata Artifact
#'
#' @inheritParams new_table_artifact
#' @param values Named metadata values.
#'
#' @return An object with class `c("aq_metadata_artifact", "aq_artifact", "list")`.
#'
#' @family AutoQuant artifact constructors
#' @export
new_metadata_artifact <- function(
  id,
  title,
  values = list(),
  subtitle = NULL,
  description = NULL,
  tags = character(),
  dependencies = character(),
  source_generator = NULL,
  creation_time = aq_artifact_now(),
  version = "0.1.0",
  metadata = list()
) {
  x <- aq_artifact_new_common(
    id = id,
    type = "metadata",
    title = title,
    subtitle = subtitle,
    description = description,
    tags = tags,
    dependencies = dependencies,
    source_generator = source_generator,
    creation_time = creation_time,
    version = version,
    metadata = metadata
  )
  x$values <- aq_artifact_null_default(values, list())
  aq_artifact_finish(x, "metadata")
}

#' Create a Computation Graph Artifact
#'
#' @inheritParams new_table_artifact
#' @param nodes Graph node table/list.
#' @param edges Graph edge table/list.
#' @param cache_keys Cache keys used by graph nodes.
#'
#' @return An object with class `c("aq_computation_graph_artifact", "aq_artifact", "list")`.
#'
#' @family AutoQuant artifact constructors
#' @export
new_computation_graph_artifact <- function(
  id,
  title,
  nodes = list(),
  edges = list(),
  cache_keys = character(),
  subtitle = NULL,
  description = NULL,
  tags = character(),
  dependencies = character(),
  source_generator = NULL,
  creation_time = aq_artifact_now(),
  version = "0.1.0",
  metadata = list()
) {
  x <- aq_artifact_new_common(
    id = id,
    type = "computation_graph",
    title = title,
    subtitle = subtitle,
    description = description,
    tags = tags,
    dependencies = dependencies,
    source_generator = source_generator,
    creation_time = creation_time,
    version = version,
    metadata = metadata
  )
  x$nodes <- aq_artifact_null_default(nodes, list())
  x$edges <- aq_artifact_null_default(edges, list())
  x$cache_keys <- aq_artifact_chr(cache_keys)
  aq_artifact_finish(x, "computation_graph")
}

#' Create a Display Plan Artifact
#'
#' @inheritParams new_table_artifact
#' @param sections Display section definitions.
#' @param artifact_ids Artifact IDs included in the plan.
#' @param audience Intended audience.
#' @param layout_type Layout type.
#'
#' @return An object with class `c("aq_display_plan_artifact", "aq_artifact", "list")`.
#'
#' @family AutoQuant artifact constructors
#' @export
new_display_plan_artifact <- function(
  id,
  title,
  sections = list(),
  artifact_ids = character(),
  audience = "analyst",
  layout_type = "sections",
  subtitle = NULL,
  description = NULL,
  tags = character(),
  dependencies = character(),
  source_generator = NULL,
  creation_time = aq_artifact_now(),
  version = "0.1.0",
  metadata = list()
) {
  x <- aq_artifact_new_common(
    id = id,
    type = "display_plan",
    title = title,
    subtitle = subtitle,
    description = description,
    tags = tags,
    dependencies = dependencies,
    source_generator = source_generator,
    creation_time = creation_time,
    version = version,
    metadata = metadata
  )
  x$sections <- aq_artifact_null_default(sections, list())
  x$artifact_ids <- aq_artifact_chr(artifact_ids)
  x$audience <- aq_artifact_scalar_chr(audience, "audience")
  x$layout_type <- aq_artifact_scalar_chr(layout_type, "layout_type")
  aq_artifact_finish(x, "display_plan")
}

#' Create a Quality Gate Artifact
#'
#' @inheritParams new_table_artifact
#' @param gate_type Quality gate type.
#' @param status Gate status.
#' @param severity Gate severity.
#' @param message Gate message.
#' @param diagnostic_value Optional diagnostic value.
#' @param threshold Optional threshold.
#' @param recommended_action Recommended action.
#'
#' @return An object with class `c("aq_quality_gate_artifact", "aq_artifact", "list")`.
#'
#' @family AutoQuant artifact constructors
#' @export
new_quality_gate_artifact <- function(
  id,
  title,
  gate_type,
  status,
  severity,
  message,
  diagnostic_value = NULL,
  threshold = NULL,
  recommended_action = NULL,
  subtitle = NULL,
  description = NULL,
  tags = character(),
  dependencies = character(),
  source_generator = NULL,
  creation_time = aq_artifact_now(),
  version = "0.1.0",
  metadata = list()
) {
  x <- aq_artifact_new_common(
    id = id,
    type = "quality_gate",
    title = title,
    subtitle = subtitle,
    description = description,
    tags = tags,
    dependencies = dependencies,
    source_generator = source_generator,
    creation_time = creation_time,
    version = version,
    metadata = metadata
  )
  x$gate_type <- aq_artifact_scalar_chr(gate_type, "gate_type")
  x$status <- aq_artifact_scalar_chr(status, "status")
  x$severity <- aq_artifact_scalar_chr(severity, "severity")
  x$message <- aq_artifact_scalar_chr(message, "message")
  x["diagnostic_value"] <- list(diagnostic_value)
  x["threshold"] <- list(threshold)
  x["recommended_action"] <- list(aq_artifact_scalar_chr(recommended_action, "recommended_action", allow_null = TRUE))
  aq_artifact_finish(x, "quality_gate")
}

aq_artifact_validation_row <- function(check, status, message) {
  data.table::data.table(
    check = check,
    status = status,
    message = message
  )
}

aq_artifact_required_fields <- function() {
  c(
    "id",
    "type",
    "title",
    "subtitle",
    "description",
    "tags",
    "dependencies",
    "source_generator",
    "creation_time",
    "version"
  )
}

aq_artifact_type_required_fields <- function(type) {
  switch(
    type,
    table = c("data", "schema"),
    plot = c("object", "config"),
    diagnostic = c("status", "severity", "details"),
    finding = c("claim", "evidence_ids", "confidence", "severity", "caveats", "recommended_follow_up"),
    warning = c("message", "severity", "affected_artifacts"),
    metadata = c("values"),
    computation_graph = c("nodes", "edges", "cache_keys"),
    display_plan = c("sections", "artifact_ids", "audience", "layout_type"),
    quality_gate = c("gate_type", "status", "severity", "message", "diagnostic_value", "threshold", "recommended_action"),
    character()
  )
}

#' Validate an AutoQuant Artifact
#'
#' @param artifact Artifact object.
#'
#' @return A `data.table` with `check`, `status`, and `message` columns.
#'
#' @family AutoQuant artifact validation
#' @export
validate_artifact <- function(artifact) {
  rows <- list()
  add <- function(check, status, message) {
    rows[[length(rows) + 1L]] <<- aq_artifact_validation_row(check, status, message)
  }

  if (!inherits(artifact, "aq_artifact")) {
    add("class", "error", "Artifact must inherit from aq_artifact.")
  } else {
    add("class", "success", "Artifact inherits from aq_artifact.")
  }

  required <- aq_artifact_required_fields()
  missing <- setdiff(required, names(artifact))
  if (length(missing)) {
    add("common_fields", "error", paste("Missing common fields:", paste(missing, collapse = ", ")))
  } else {
    add("common_fields", "success", "All common fields are present.")
  }

  type <- artifact$type
  if (is.null(type) || length(type) != 1L || is.na(type) || !(type %in% aq_artifact_types())) {
    add("type", "error", "Artifact type is missing or unsupported.")
    type <- NULL
  } else {
    add("type", "success", paste("Artifact type is supported:", type))
  }

  for (field in c("id", "title", "version")) {
    value <- artifact[[field]]
    if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(as.character(value)))) {
      add(paste0("field_", field), "error", paste(field, "must be a non-empty scalar value."))
    } else {
      add(paste0("field_", field), "success", paste(field, "is valid."))
    }
  }

  if (!is.null(type)) {
    type_required <- aq_artifact_type_required_fields(type)
    type_missing <- setdiff(type_required, names(artifact))
    if (length(type_missing)) {
      add("type_fields", "error", paste("Missing", type, "fields:", paste(type_missing, collapse = ", ")))
    } else {
      add("type_fields", "success", paste("All", type, "fields are present."))
    }
  }

  if (!is.character(artifact$tags)) {
    add("tags", "error", "tags must be a character vector.")
  } else {
    add("tags", "success", "tags is a character vector.")
  }

  if (!is.character(artifact$dependencies)) {
    add("dependencies", "error", "dependencies must be a character vector.")
  } else {
    add("dependencies", "success", "dependencies is a character vector.")
  }

  if (!is.null(artifact$creation_time) && !inherits(artifact$creation_time, c("POSIXct", "POSIXt", "Date"))) {
    add("creation_time", "warning", "creation_time is present but is not a date/time object.")
  } else {
    add("creation_time", "success", "creation_time is date/time compatible.")
  }

  if (identical(type, "finding")) {
    if (!is.na(artifact$confidence) && (artifact$confidence < 0 || artifact$confidence > 1)) {
      add("finding_confidence", "error", "finding confidence must be between 0 and 1.")
    } else {
      add("finding_confidence", "success", "finding confidence is valid or unknown.")
    }
  }

  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Validate an AutoQuant Artifact Collection
#'
#' @param artifacts A list of artifacts.
#'
#' @return A `data.table` with artifact-level validation checks.
#'
#' @family AutoQuant artifact validation
#' @export
validate_artifact_collection <- function(artifacts) {
  if (inherits(artifacts, "aq_artifact")) {
    artifacts <- list(artifacts)
  }
  if (!is.list(artifacts)) {
    return(aq_artifact_validation_row("collection_type", "error", "artifacts must be a list or aq_artifact."))
  }

  validations <- lapply(seq_along(artifacts), function(i) {
    artifact <- artifacts[[i]]
    result <- validate_artifact(artifact)
    artifact_id <- if (!is.null(artifact$id)) as.character(artifact$id)[1L] else paste0("artifact_", i)
    result[, artifact_id := artifact_id]
    data.table::setcolorder(result, c("artifact_id", "check", "status", "message"))
    result
  })

  result <- data.table::rbindlist(validations, use.names = TRUE, fill = TRUE)
  ids <- vapply(artifacts, function(x) {
    if (!is.null(x$id)) as.character(x$id)[1L] else NA_character_
  }, character(1L))
  duplicate_ids <- unique(ids[!is.na(ids) & duplicated(ids)])

  if (length(duplicate_ids)) {
    dup <- data.table::data.table(
      artifact_id = duplicate_ids,
      check = "unique_ids",
      status = "error",
      message = "Artifact IDs must be unique within a collection."
    )
  } else {
    dup <- data.table::data.table(
      artifact_id = NA_character_,
      check = "unique_ids",
      status = "success",
      message = "Artifact IDs are unique within the collection."
    )
  }

  dependencies <- unique(unlist(lapply(artifacts, function(x) x$dependencies), use.names = FALSE))
  dependencies <- dependencies[!is.na(dependencies) & nzchar(dependencies)]
  missing_deps <- setdiff(dependencies, ids)
  if (length(missing_deps)) {
    deps <- data.table::data.table(
      artifact_id = missing_deps,
      check = "dependencies_resolve",
      status = "warning",
      message = "Dependency ID is not present in this collection."
    )
  } else {
    deps <- data.table::data.table(
      artifact_id = NA_character_,
      check = "dependencies_resolve",
      status = "success",
      message = "All declared dependencies resolve within the collection or no dependencies were declared."
    )
  }

  data.table::rbindlist(list(result, dup, deps), use.names = TRUE, fill = TRUE)
}

#' QA AutoQuant Artifact Schema Framework
#'
#' Builds each supported artifact type and validates both individual artifacts
#' and an artifact collection.
#'
#' @return A `data.table` of QA checks.
#'
#' @family QA
#' @export
qa_artifact_schema_framework <- function() {
  sample_table <- data.table::data.table(feature = c("A", "B"), importance = c(0.7, 0.3))

  artifacts <- list(
    new_table_artifact(
      id = "tbl_global_importance",
      title = "Global Importance",
      data = sample_table,
      source_generator = "qa_artifact_schema_framework",
      tags = c("qa", "table")
    ),
    new_plot_artifact(
      id = "plt_global_importance",
      title = "Global Importance Plot",
      object = NULL,
      dependencies = "tbl_global_importance",
      source_generator = "qa_artifact_schema_framework",
      tags = c("qa", "plot")
    ),
    new_diagnostic_artifact(
      id = "diag_schema",
      title = "Schema Diagnostic",
      status = "pass",
      severity = "info",
      source_generator = "qa_artifact_schema_framework"
    ),
    new_finding_artifact(
      id = "finding_top_driver",
      title = "Top Driver Finding",
      claim = "Feature A is the top driver in the QA fixture.",
      evidence_ids = "tbl_global_importance",
      confidence = 0.8,
      source_generator = "qa_artifact_schema_framework"
    ),
    new_warning_artifact(
      id = "warn_fixture",
      title = "Fixture Warning",
      message = "This warning is generated by the QA fixture.",
      affected_artifacts = "tbl_global_importance",
      source_generator = "qa_artifact_schema_framework"
    ),
    new_metadata_artifact(
      id = "meta_run",
      title = "Run Metadata",
      values = list(problem_type = "regression"),
      source_generator = "qa_artifact_schema_framework"
    ),
    new_computation_graph_artifact(
      id = "graph_run",
      title = "Computation Graph",
      nodes = data.table::data.table(node_id = c("n1", "n2"), artifact_id = c("tbl_global_importance", "plt_global_importance")),
      edges = data.table::data.table(from = "n1", to = "n2"),
      cache_keys = c("qa-table", "qa-plot"),
      source_generator = "qa_artifact_schema_framework"
    ),
    new_display_plan_artifact(
      id = "display_plan",
      title = "Display Plan",
      sections = list(list(section_id = "overview", artifact_ids = c("tbl_global_importance", "plt_global_importance"))),
      artifact_ids = c("tbl_global_importance", "plt_global_importance"),
      source_generator = "qa_artifact_schema_framework"
    ),
    new_quality_gate_artifact(
      id = "gate_schema",
      title = "Schema Quality Gate",
      gate_type = "schema",
      status = "pass",
      severity = "info",
      message = "The QA schema is valid.",
      source_generator = "qa_artifact_schema_framework"
    )
  )

  validation <- validate_artifact_collection(artifacts)
  constructor_types <- vapply(artifacts, function(x) x$type, character(1L))

  data.table::data.table(
    check = c(
      "all_artifact_types_constructed",
      "all_artifacts_inherit_aq_artifact",
      "individual_validation_has_no_errors",
      "collection_validation_has_no_errors",
      "dependency_resolution_passes",
      "finding_evidence_preserved",
      "display_plan_references_artifacts"
    ),
    status = c(
      if (setequal(constructor_types, aq_artifact_types())) "success" else "error",
      if (all(vapply(artifacts, inherits, logical(1L), "aq_artifact"))) "success" else "error",
      if (!any(validation[check != "unique_ids" & check != "dependencies_resolve", status] == "error")) "success" else "error",
      if (!any(validation$status == "error")) "success" else "error",
      if (nrow(validation[check == "dependencies_resolve" & status == "success"]) == 1L) "success" else "error",
      if (identical(artifacts[[4L]]$evidence_ids, "tbl_global_importance")) "success" else "error",
      if (all(artifacts[[8L]]$artifact_ids %in% vapply(artifacts, function(x) x$id, character(1L)))) "success" else "error"
    ),
    message = c(
      "Every required artifact type has a constructor fixture.",
      "Every constructor returns an aq_artifact.",
      "Individual artifact validations do not report errors.",
      "The collection validation does not report errors.",
      "Declared dependencies resolve inside the QA collection.",
      "Finding evidence IDs are preserved for traceability.",
      "Display plan artifact references resolve to collection artifacts."
    )
  )
}
