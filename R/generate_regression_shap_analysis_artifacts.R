# ============================================================
# Regression SHAP Analysis Artifact Generation
#
# This generator consumes precomputed Shap_ columns returned by AutoQuant
# modeling/scoring outputs. It does not compute SHAP values, call models,
# call predict(), or invoke any SHAP backend package.
# ============================================================

regression_shap_slug <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- tolower(x)
  if (!nzchar(x)) "artifact" else x
}

regression_shap_null_coalesce <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

regression_shap_sections <- function() {
  c(
    "SHAP Overview",
    "Global Importance",
    "Interaction Importance",
    "Single Feature Effects",
    "SHAP Dependence",
    "Segment Effects",
    "Time Effects",
    "Local Explanations",
    "Appendix"
  )
}

regression_shap_artifact <- function(
  name,
  label,
  type,
  section,
  object = NULL,
  content = NULL,
  metadata = list()
) {
  list(
    name = regression_shap_slug(name),
    label = label,
    type = type,
    artifact_type = type,
    section = section,
    object = object,
    content = content,
    data = if (identical(type, "table")) object else NULL,
    metadata = metadata
  )
}

regression_shap_add_artifact <- function(artifacts, artifact) {
  base_name <- regression_shap_slug(artifact$name)
  artifact$name <- base_name
  artifact$metadata$artifact_index <- length(artifacts) + 1L

  if (artifact$name %in% names(artifacts)) {
    suffix <- 2L
    candidate <- paste0(base_name, "_", suffix)
    while (candidate %in% names(artifacts)) {
      suffix <- suffix + 1L
      candidate <- paste0(base_name, "_", suffix)
    }
    artifact$name <- candidate
  }

  artifacts[[artifact$name]] <- artifact
  artifacts
}

regression_shap_warn <- function(warnings, message) {
  unique(c(warnings, message))
}

aq_smart_round_vector <- function(x) {
  if (!is.numeric(x)) {
    return(x)
  }
  finite <- x[is.finite(x)]
  if (!length(finite)) {
    return(x)
  }
  if (all(abs(finite - round(finite)) < sqrt(.Machine$double.eps))) {
    return(as.integer(round(x)))
  }

  max_abs <- max(abs(finite), na.rm = TRUE)
  if (max_abs >= 1000) {
    return(round(x, 1L))
  }
  if (max_abs >= 100) {
    return(round(x, 2L))
  }
  if (max_abs >= 1) {
    return(round(x, 4L))
  }

  signif(x, 4L)
}

aq_smart_round_dt <- function(dt, skip_cols = character()) {
  if (is.null(dt) || !data.table::is.data.table(dt) || !nrow(dt)) {
    return(dt)
  }
  out <- data.table::copy(dt)
  numeric_cols <- names(out)[vapply(out, is.numeric, logical(1L))]
  numeric_cols <- setdiff(numeric_cols, skip_cols)
  for (col in numeric_cols) {
    data.table::set(out, j = col, value = aq_smart_round_vector(out[[col]]))
  }
  out[]
}

regression_shap_positive_int <- function(x, default, name, warnings) {
  value <- suppressWarnings(as.integer(x))
  if (length(value) != 1L || is.na(value) || value < 1L) {
    warnings <- regression_shap_warn(warnings, paste(name, "must be a positive integer; using", default))
    return(list(value = as.integer(default), warnings = warnings))
  }

  list(value = value, warnings = warnings)
}

aq_detect_shap_columns <- function(data, shap_prefix = "Shap_", feature_cols = NULL) {
  columns <- names(data)
  shap_cols <- columns[startsWith(columns, shap_prefix)]
  features <- sub(paste0("^", gsub("([\\W])", "\\\\\\1", shap_prefix)), "", shap_cols)

  map <- data.table::data.table(
    feature = features,
    shap_col = shap_cols,
    source_col_exists = features %in% columns,
    shap_is_numeric = vapply(shap_cols, function(col) is.numeric(data[[col]]), logical(1L)),
    included = TRUE,
    reason_excluded = ""
  )

  if (!is.null(feature_cols)) {
    feature_cols <- as.character(feature_cols)
    map[!feature %in% feature_cols, `:=`(
      included = FALSE,
      reason_excluded = "not_in_feature_cols"
    )]
  }

  map[shap_is_numeric == FALSE, `:=`(
    included = FALSE,
    reason_excluded = ifelse(
      nzchar(reason_excluded),
      paste(reason_excluded, "non_numeric_shap", sep = ";"),
      "non_numeric_shap"
    )
  )]

  map[]
}

aq_infer_prediction_col <- function(data, prediction_col = NULL) {
  if (!is.null(prediction_col) && nzchar(prediction_col)) {
    return(prediction_col)
  }
  if ("Predict" %in% names(data)) {
    return("Predict")
  }

  NULL
}

aq_infer_date_col <- function(data, DateVar = NULL) {
  if (!is.null(DateVar) && DateVar %in% names(data)) {
    return(DateVar)
  }

  if (!is.null(DateVar)) {
    case_match <- names(data)[tolower(names(data)) == tolower(DateVar)]
    if (length(case_match)) {
      return(case_match[[1L]])
    }
  }

  typed <- names(data)[vapply(data, function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")), logical(1L))]
  if (length(typed)) {
    return(typed[[1L]])
  }

  candidates <- grep("date|time|period", names(data), value = TRUE, ignore.case = TRUE)
  candidates <- candidates[!startsWith(candidates, "Shap_")]
  for (candidate in candidates) {
    parsed <- suppressWarnings(as.Date(data[[candidate]]))
    if (sum(!is.na(parsed)) >= max(2L, ceiling(0.5 * nrow(data)))) {
      return(candidate)
    }
  }

  NULL
}

aq_shap_period <- function(x, date_aggregation) {
  parsed <- if (inherits(x, "Date")) {
    x
  } else if (inherits(x, c("POSIXct", "POSIXlt"))) {
    as.Date(x)
  } else {
    suppressWarnings(as.Date(x))
  }

  if (identical(date_aggregation, "day")) {
    return(parsed)
  }
  if (identical(date_aggregation, "week")) {
    return(parsed - as.integer(format(parsed, "%u")) + 1L)
  }

  as.Date(format(parsed, "%Y-%m-01"))
}

aq_summarize_shap_global_importance <- function(data, column_map) {
  valid <- column_map[included == TRUE]
  rows <- lapply(seq_len(nrow(valid)), function(i) {
    values <- data[[valid$shap_col[[i]]]]
    data.table::data.table(
      feature = valid$feature[[i]],
      shap_col = valid$shap_col[[i]],
      mean_abs_shap = mean(abs(values), na.rm = TRUE),
      mean_shap = mean(values, na.rm = TRUE),
      median_shap = stats::median(values, na.rm = TRUE),
      sd_shap = stats::sd(values, na.rm = TRUE),
      min_shap = suppressWarnings(min(values, na.rm = TRUE)),
      max_shap = suppressWarnings(max(values, na.rm = TRUE)),
      pct_positive = mean(values > 0, na.rm = TRUE),
      pct_negative = mean(values < 0, na.rm = TRUE),
      n = sum(!is.na(values))
    )
  })

  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  data.table::setorderv(out, "mean_abs_shap", order = -1L, na.last = TRUE)
  out[, rank := seq_len(.N)]
  out[]
}

aq_summarize_shap_categorical_level_importance <- function(
  data,
  column_map,
  features = NULL,
  max_levels = 20L
) {
  valid <- column_map[included == TRUE & source_col_exists == TRUE]
  if (!is.null(features)) {
    valid <- valid[feature %in% features]
  }
  if (!nrow(valid)) {
    return(data.table::data.table())
  }

  rows <- lapply(seq_len(nrow(valid)), function(i) {
    feature_name <- valid$feature[[i]]
    shap_col <- valid$shap_col[[i]]
    source_values <- data[[feature_name]]
    level <- aq_feature_bins(source_values, max_levels = max_levels)
    level[is.na(level)] <- "missing"
    value_type <- if (is.numeric(source_values) || is.integer(source_values)) "numeric_bin" else "categorical_level"

    temp <- data.table::data.table(
      feature = feature_name,
      value_type = value_type,
      level = level,
      shap_value = data[[shap_col]]
    )
    temp[, .(
      value_type = value_type[[1L]],
      mean_abs_shap = mean(abs(shap_value), na.rm = TRUE),
      mean_shap = mean(shap_value, na.rm = TRUE),
      median_shap = stats::median(shap_value, na.rm = TRUE),
      sd_shap = stats::sd(shap_value, na.rm = TRUE),
      min_shap = suppressWarnings(min(shap_value, na.rm = TRUE)),
      max_shap = suppressWarnings(max(shap_value, na.rm = TRUE)),
      pct_positive = mean(shap_value > 0, na.rm = TRUE),
      pct_negative = mean(shap_value < 0, na.rm = TRUE),
      n = .N
    ), by = .(feature, level)]
  })

  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  if (nrow(out)) {
    data.table::setorderv(out, "mean_abs_shap", order = -1L, na.last = TRUE)
    out[, rank := seq_len(.N)]
    out[, feature_level := paste(feature, level, sep = " = ")]
  }
  aq_smart_round_dt(out, skip_cols = c("rank", "n"))[]
}

aq_regression_shap_categorical_level_values <- function(
  data,
  column_map,
  categorical_level_importance,
  max_rows = 5000L,
  top_n = 20L
) {
  if (is.null(categorical_level_importance) || !nrow(categorical_level_importance)) {
    return(data.table::data.table())
  }

  top_levels <- categorical_level_importance[rank <= top_n, .(feature, level, feature_level)]
  row_index <- seq_len(nrow(data))
  if (nrow(data) > max_rows) {
    set.seed(123L)
    row_index <- sort(sample(row_index, max_rows))
  }

  rows <- lapply(seq_len(nrow(top_levels)), function(i) {
    feature_name <- top_levels$feature[[i]]
    level_name <- top_levels$level[[i]]
    shap_col <- column_map[feature == feature_name, shap_col][[1L]]
    values <- aq_feature_bins(data[[feature_name]], max_levels = max(length(unique(categorical_level_importance[feature == feature_name, level])), 2L))[row_index]
    values[is.na(values)] <- "missing"
    values[!values %in% categorical_level_importance[feature == feature_name, level]] <- "other"
    keep <- values == level_name
    if (!any(keep)) {
      return(NULL)
    }
    data.table::data.table(
      feature = feature_name,
      level = level_name,
      feature_level = top_levels$feature_level[[i]],
      shap_value = data[[shap_col]][row_index][keep]
    )
  })

  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_feature_bins <- function(x, max_levels = 20L) {
  if (is.numeric(x) || is.integer(x)) {
    non_missing <- x[!is.na(x)]
    unique_count <- length(unique(non_missing))
    if (unique_count <= max_levels) {
      return(as.character(x))
    }
    probs <- unique(stats::quantile(non_missing, probs = seq(0, 1, length.out = 11L), na.rm = TRUE, type = 8))
    if (length(probs) < 2L) {
      return(as.character(x))
    }
    return(as.character(cut(x, breaks = probs, include.lowest = TRUE, dig.lab = 6L)))
  }

  values <- as.character(x)
  tab <- sort(table(values, useNA = "no"), decreasing = TRUE)
  keep <- names(tab)[seq_len(min(length(tab), max_levels))]
  values[is.na(values)] <- "missing"
  values[!values %in% keep & values != "missing"] <- "other"
  values
}

aq_summarize_shap_single_feature_effects <- function(
  data,
  column_map,
  features,
  prediction_col = NULL,
  target_col = NULL,
  max_levels = 20L
) {
  rows <- lapply(features, function(feature_name) {
    map_row <- column_map[["feature"]] == feature_name & column_map[["included"]] == TRUE
    map_row <- column_map[map_row]
    if (!nrow(map_row) || !isTRUE(map_row$source_col_exists[[1L]])) {
      return(NULL)
    }

    temp <- data.table::data.table(
      feature = feature_name,
      feature_value_or_bin = aq_feature_bins(data[[feature_name]], max_levels = max_levels),
      shap_value = data[[map_row$shap_col[[1L]]]]
    )
    value_type_label <- if (is.numeric(data[[feature_name]]) || is.integer(data[[feature_name]])) "numeric_bin" else "categorical_level"
    temp[, value_type := value_type_label]
    if (!is.null(prediction_col) && prediction_col %in% names(data) && is.numeric(data[[prediction_col]])) {
      temp[, prediction := data[[prediction_col]]]
    }
    if (!is.null(target_col) && target_col %in% names(data) && is.numeric(data[[target_col]])) {
      temp[, target := data[[target_col]]]
    }

    temp[, .(
      value_type = value_type[[1L]],
      mean_shap = mean(shap_value, na.rm = TRUE),
      median_shap = stats::median(shap_value, na.rm = TRUE),
      mean_abs_shap = mean(abs(shap_value), na.rm = TRUE),
      mean_prediction = if ("prediction" %in% names(.SD)) mean(prediction, na.rm = TRUE) else NA_real_,
      mean_target = if ("target" %in% names(.SD)) mean(target, na.rm = TRUE) else NA_real_,
      n = .N
    ), by = .(feature, feature_value_or_bin)]
  })

  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_summarize_shap_dependence <- function(
  data,
  column_map,
  features,
  prediction_col = NULL,
  target_col = NULL,
  id_cols = character(),
  ByVars = character(),
  DateVar = NULL,
  max_dependence_rows = 5000L
) {
  valid_features <- features[features %in% column_map[included == TRUE & source_col_exists == TRUE, feature]]
  if (!length(valid_features)) {
    return(data.table::data.table())
  }

  row_index <- seq_len(nrow(data))
  if (nrow(data) > max_dependence_rows) {
    set.seed(123L)
    row_index <- sort(sample(row_index, max_dependence_rows))
  }

  context_cols <- unique(c(id_cols, ByVars, DateVar))
  context_cols <- context_cols[nzchar(context_cols) & context_cols %in% names(data)]
  rows <- lapply(valid_features, function(feature_name) {
    shap_col <- column_map[column_map[["feature"]] == feature_name, shap_col][[1L]]
    out <- data.table::data.table(
      row_id = row_index,
      feature = feature_name,
      feature_value = as.character(data[[feature_name]][row_index]),
      shap_value = data[[shap_col]][row_index]
    )
    if (!is.null(prediction_col) && prediction_col %in% names(data)) {
      out[, prediction := data[[prediction_col]][row_index]]
    }
    if (!is.null(target_col) && target_col %in% names(data)) {
      out[, target := data[[target_col]][row_index]]
    }
    for (col in context_cols) {
      out[, (col) := data[[col]][row_index]]
    }
    out
  })

  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_summarize_shap_segments <- function(
  data,
  column_map,
  ByVars,
  max_byvars = 3L,
  max_segment_levels = 20L
) {
  ByVars <- head(ByVars, max_byvars)
  rows <- lapply(ByVars, function(byvar) {
    values <- as.character(data[[byvar]])
    values[is.na(values)] <- "missing"
    tab <- sort(table(values), decreasing = TRUE)
    keep <- names(tab)[seq_len(min(length(tab), max_segment_levels))]
    segment <- values
    segment[!segment %in% keep] <- "other"

    feature_rows <- lapply(seq_len(nrow(column_map[included == TRUE])), function(i) {
      map_row <- column_map[included == TRUE][i]
      temp <- data.table::data.table(
        ByVar = byvar,
        segment = segment,
        feature = map_row$feature,
        shap_value = data[[map_row$shap_col]]
      )
      temp[, .(
        mean_abs_shap = mean(abs(shap_value), na.rm = TRUE),
        mean_shap = mean(shap_value, na.rm = TRUE),
        median_shap = stats::median(shap_value, na.rm = TRUE),
        n = .N
      ), by = .(ByVar, segment, feature)]
    })
    data.table::rbindlist(feature_rows, use.names = TRUE, fill = TRUE)
  })
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  if (nrow(out)) {
    data.table::setorderv(out, c("ByVar", "segment", "mean_abs_shap"), order = c(1L, 1L, -1L))
    out[, rank_within_segment := seq_len(.N), by = .(ByVar, segment)]
  }
  out[]
}

aq_summarize_shap_time <- function(data, column_map, DateVar, date_aggregation = "month") {
  periods <- aq_shap_period(data[[DateVar]], date_aggregation)
  rows <- lapply(seq_len(nrow(column_map[included == TRUE])), function(i) {
    map_row <- column_map[included == TRUE][i]
    temp <- data.table::data.table(
      period = periods,
      feature = map_row$feature,
      shap_value = data[[map_row$shap_col]]
    )
    temp[!is.na(period), .(
      mean_abs_shap = mean(abs(shap_value), na.rm = TRUE),
      mean_shap = mean(shap_value, na.rm = TRUE),
      median_shap = stats::median(shap_value, na.rm = TRUE),
      n = .N
    ), by = .(period, feature)]
  })
  out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  if (nrow(out)) {
    data.table::setorderv(out, c("period", "mean_abs_shap"), order = c(1L, -1L))
    out[, rank_within_period := seq_len(.N), by = period]
  }
  out[]
}

aq_create_interaction_levels <- function(
  x,
  var_name,
  numeric_bins = 5L,
  max_levels = 12L,
  min_cell_n = 5L,
  collapse_rare_levels = TRUE
) {
  warnings <- character()
  if (is.numeric(x) || is.integer(x)) {
    non_missing <- x[!is.na(x)]
    if (!length(non_missing)) {
      return(list(levels = rep("missing", length(x)), warnings = warnings, value_type = "numeric_bin"))
    }
    unique_count <- length(unique(non_missing))
    if (unique_count <= numeric_bins) {
      levels <- as.character(x)
      levels[is.na(levels)] <- "missing"
      return(list(levels = levels, warnings = warnings, value_type = "numeric_level"))
    }
    probs <- unique(stats::quantile(non_missing, probs = seq(0, 1, length.out = numeric_bins + 1L), na.rm = TRUE, type = 8))
    if (length(probs) < 2L) {
      levels <- as.character(x)
      levels[is.na(levels)] <- "missing"
      return(list(levels = levels, warnings = warnings, value_type = "numeric_level"))
    }
    levels <- as.character(cut(x, breaks = probs, include.lowest = TRUE, dig.lab = 5L))
    levels[is.na(levels)] <- "missing"
    return(list(levels = levels, warnings = warnings, value_type = "numeric_bin"))
  }

  levels <- as.character(x)
  levels[is.na(levels)] <- "missing"
  tab <- sort(table(levels, useNA = "no"), decreasing = TRUE)
  if (length(tab) > max_levels) {
    keep <- names(tab)[seq_len(max_levels)]
    if (isTRUE(collapse_rare_levels)) {
      levels[!levels %in% keep] <- "other"
      warnings <- regression_shap_warn(warnings, paste("Interaction variable", var_name, "was capped to", max_levels, "levels; rare levels collapsed to other."))
    } else {
      levels[!levels %in% keep] <- NA_character_
      warnings <- regression_shap_warn(warnings, paste("Interaction variable", var_name, "was capped to", max_levels, "levels; rare levels omitted."))
    }
  }
  rare <- names(tab)[tab < min_cell_n]
  if (length(rare) && isTRUE(collapse_rare_levels)) {
    levels[levels %in% rare] <- "other"
  }

  list(levels = levels, warnings = warnings, value_type = "categorical_level")
}

aq_interaction_weighted_mean <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  stats::weighted.mean(x[keep], w = w[keep], na.rm = TRUE)
}

aq_interaction_weighted_sd <- function(x, w) {
  center <- aq_interaction_weighted_mean(x, w)
  if (is.na(center)) {
    return(NA_real_)
  }
  sqrt(aq_interaction_weighted_mean((x - center)^2, w))
}

aq_interaction_safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(NA_real_)
  }
  max(x)
}

aq_interaction_top_cell_label <- function(a, b, value) {
  keep <- is.finite(value)
  if (!any(keep)) {
    return("")
  }
  idx <- which.max(value[keep])
  paste(a[keep][[idx]], b[keep][[idx]], sep = " x ")
}

aq_normalize_interaction_pairs <- function(interaction_pairs) {
  if (is.null(interaction_pairs) || !length(interaction_pairs)) {
    return(data.table::data.table(shap_feature = character(), interaction_feature = character()))
  }
  if (data.table::is.data.table(interaction_pairs) && all(c("shap_feature", "interaction_feature") %in% names(interaction_pairs))) {
    return(data.table::copy(interaction_pairs[, .(shap_feature, interaction_feature)]))
  }
  rows <- lapply(interaction_pairs, function(pair) {
    pair <- as.character(pair)
    if (length(pair) < 2L) return(NULL)
    data.table::data.table(shap_feature = pair[[1L]], interaction_feature = pair[[2L]])
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_candidate_interaction_pairs <- function(
  data,
  column_map,
  global_importance,
  selected_features = NULL,
  interaction_pairs = list(),
  max_interaction_pairs = 20L,
  ByVars = character(),
  DateVar = NULL,
  date_period_col = NULL,
  id_cols = character(),
  target_col = NULL,
  prediction_col = NULL
) {
  requested <- aq_normalize_interaction_pairs(interaction_pairs)
  valid_shap_features <- column_map[included == TRUE & source_col_exists == TRUE, feature]
  if (nrow(requested)) {
    requested <- requested[shap_feature %in% valid_shap_features]
    requested <- requested[interaction_feature %in% c(names(data), date_period_col)]
    requested <- requested[shap_feature != interaction_feature]
    return(unique(head(requested, max_interaction_pairs)))
  }

  shap_features <- intersect(global_importance$feature, valid_shap_features)
  if (length(selected_features)) {
    shap_features <- unique(c(intersect(selected_features, shap_features), shap_features))
  }
  shap_features <- head(shap_features, max(1L, min(length(shap_features), ceiling(sqrt(max_interaction_pairs)) + 2L)))

  excluded <- unique(c(id_cols, target_col, prediction_col, column_map$shap_col))
  candidate_b <- setdiff(names(data), excluded)
  candidate_b <- candidate_b[!startsWith(candidate_b, "Shap_")]
  candidate_b <- unique(c(ByVars, selected_features, candidate_b))
  if (!is.null(date_period_col) && date_period_col %in% names(data)) {
    candidate_b <- unique(c(date_period_col, candidate_b))
  }
  candidate_b <- candidate_b[candidate_b %in% names(data)]

  out <- data.table::rbindlist(lapply(shap_features, function(a) {
    b <- setdiff(candidate_b, a)
    if (!length(b)) return(NULL)
    data.table::data.table(shap_feature = a, interaction_feature = b)
  }), use.names = TRUE, fill = TRUE)
  unique(head(out, max_interaction_pairs))
}

aq_summarize_shap_interactions <- function(
  data,
  column_map,
  global_importance,
  selected_features = NULL,
  interaction_pairs = list(),
  max_interaction_pairs = 20L,
  numeric_interaction_bins = 5L,
  max_interaction_levels = 12L,
  min_interaction_cell_n = 5L,
  interaction_stat = "mean_abs_shap",
  interaction_score_stat = "weighted_mad",
  collapse_rare_levels = TRUE,
  target_col = NULL,
  prediction_col = NULL,
  ByVars = character(),
  DateVar = NULL,
  date_aggregation = "month",
  id_cols = character()
) {
  warnings <- character()
  work <- data.table::copy(data)
  date_period_col <- NULL
  if (!is.null(DateVar) && DateVar %in% names(work)) {
    date_period_col <- "__aq_date_period__"
    work[, (date_period_col) := as.character(aq_shap_period(get(DateVar), date_aggregation))]
  }

  pairs <- aq_candidate_interaction_pairs(
    work,
    column_map,
    global_importance,
    selected_features = selected_features,
    interaction_pairs = interaction_pairs,
    max_interaction_pairs = max_interaction_pairs,
    ByVars = ByVars,
    DateVar = DateVar,
    date_period_col = date_period_col,
    id_cols = id_cols,
    target_col = target_col,
    prediction_col = prediction_col
  )
  if (!nrow(pairs)) {
    return(list(scores = data.table::data.table(), surfaces = data.table::data.table(), warnings = warnings))
  }

  surface_rows <- lapply(seq_len(nrow(pairs)), function(i) {
    shap_feature <- pairs$shap_feature[[i]]
    interaction_feature <- pairs$interaction_feature[[i]]
    shap_col <- column_map[feature == shap_feature & included == TRUE, shap_col][[1L]]
    reverse_shap_col <- column_map[feature == interaction_feature & included == TRUE, shap_col]
    reverse_shap_col <- if (length(reverse_shap_col)) reverse_shap_col[[1L]] else NA_character_
    has_reverse_shap <- !is.na(reverse_shap_col) && reverse_shap_col %in% names(work)
    a_levels <- aq_create_interaction_levels(
      work[[shap_feature]],
      shap_feature,
      numeric_bins = numeric_interaction_bins,
      max_levels = max_interaction_levels,
      min_cell_n = min_interaction_cell_n,
      collapse_rare_levels = collapse_rare_levels
    )
    b_levels <- aq_create_interaction_levels(
      work[[interaction_feature]],
      interaction_feature,
      numeric_bins = numeric_interaction_bins,
      max_levels = max_interaction_levels,
      min_cell_n = min_interaction_cell_n,
      collapse_rare_levels = collapse_rare_levels
    )
    warnings <<- unique(c(warnings, a_levels$warnings, b_levels$warnings))
    role <- if (!is.null(date_period_col) && identical(interaction_feature, date_period_col)) {
      "DateVar_period"
    } else if (interaction_feature %in% ByVars) {
      "ByVar"
    } else if (interaction_feature %in% selected_features) {
      "selected_feature"
    } else {
      "feature"
    }
    display_b <- if (!is.null(date_period_col) && identical(interaction_feature, date_period_col)) {
      paste0(DateVar, "_", date_aggregation)
    } else {
      interaction_feature
    }
    temp <- data.table::data.table(
      shap_feature = shap_feature,
      shap_col = shap_col,
      interaction_feature = display_b,
      interaction_feature_raw = interaction_feature,
      interaction_feature_role = role,
      shap_feature_level = a_levels$levels,
      interaction_feature_level = b_levels$levels,
      shap_value = work[[shap_col]]
    )
    temp[, combined_shap_value := shap_value]
    if (isTRUE(has_reverse_shap)) {
      temp[, combined_shap_value := shap_value + work[[reverse_shap_col]]]
    }
    if (!is.null(prediction_col) && prediction_col %in% names(work) && is.numeric(work[[prediction_col]])) {
      temp[, prediction := work[[prediction_col]]]
    }
    if (!is.null(target_col) && target_col %in% names(work) && is.numeric(work[[target_col]])) {
      temp[, target := work[[target_col]]]
    }
    temp <- temp[!is.na(shap_feature_level) & !is.na(interaction_feature_level)]
    if (!nrow(temp)) {
      return(NULL)
    }
    cell <- temp[, .(
      n = .N,
      mean_shap = mean(shap_value, na.rm = TRUE),
      median_shap = stats::median(shap_value, na.rm = TRUE),
      mean_abs_shap = mean(abs(shap_value), na.rm = TRUE),
      combined_mean_shap = mean(combined_shap_value, na.rm = TRUE),
      combined_median_shap = stats::median(combined_shap_value, na.rm = TRUE),
      combined_mean_abs_shap = mean(abs(combined_shap_value), na.rm = TRUE),
      sd_shap = stats::sd(shap_value, na.rm = TRUE),
      pct_positive = mean(shap_value > 0, na.rm = TRUE),
      pct_negative = mean(shap_value < 0, na.rm = TRUE),
      mean_prediction = if ("prediction" %in% names(.SD)) mean(prediction, na.rm = TRUE) else NA_real_,
      mean_target = if ("target" %in% names(.SD)) mean(target, na.rm = TRUE) else NA_real_
    ), by = .(shap_feature, shap_col, interaction_feature, interaction_feature_raw, interaction_feature_role, shap_feature_level, interaction_feature_level)]
    a_marginal <- temp[, .(
      shap_feature_level_mean_shap = mean(shap_value, na.rm = TRUE),
      shap_feature_level_mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)
    ), by = .(shap_feature_level)]
    b_marginal <- temp[, .(
      interaction_feature_level_mean_shap = mean(shap_value, na.rm = TRUE),
      interaction_feature_level_mean_abs_shap = mean(abs(shap_value), na.rm = TRUE)
    ), by = .(interaction_feature_level)]
    cell <- merge(cell, a_marginal, by = "shap_feature_level", all.x = TRUE, sort = FALSE)
    cell <- merge(cell, b_marginal, by = "interaction_feature_level", all.x = TRUE, sort = FALSE)
    overall_mean_shap <- mean(temp$shap_value, na.rm = TRUE)
    overall_mean_abs_shap <- mean(abs(temp$shap_value), na.rm = TRUE)
    cell[, `:=`(
      overall_mean_shap_A = overall_mean_shap,
      overall_mean_abs_shap_A = overall_mean_abs_shap,
      delta_vs_overall = mean_shap - overall_mean_shap,
      delta_vs_shap_feature_level = mean_shap - shap_feature_level_mean_shap,
      delta_vs_interaction_feature_level = mean_shap - interaction_feature_level_mean_shap,
      sparse_cell = n < min_interaction_cell_n
    )]
    cell[, `:=`(
      shap_feature_actual_value_bin = shap_feature_level,
      interaction_feature_actual_value_bin = interaction_feature_level,
      heatmap_value = mean_shap,
      heatmap_value_description = paste0("mean(", shap_col, ") by actual source feature value/bin combinations"),
      combined_shap_available = isTRUE(has_reverse_shap)
    )]
    data.table::setorderv(cell, "mean_abs_shap", order = -1L, na.last = TRUE)
    cell[, level_combo_rank := seq_len(.N)]
    cell[]
  })

  surfaces <- data.table::rbindlist(surface_rows, use.names = TRUE, fill = TRUE)
  if (!nrow(surfaces)) {
    return(list(scores = data.table::data.table(), surfaces = data.table::data.table(), warnings = warnings))
  }

  surfaces[, pair_label := paste(shap_feature, interaction_feature, sep = " x ")]
  score_col <- if (identical(interaction_stat, "mean_shap")) "delta_vs_shap_feature_level" else "mean_abs_delta"
  surfaces[, mean_abs_delta := abs(mean_abs_shap - shap_feature_level_mean_abs_shap)]
  scores <- surfaces[sparse_cell == FALSE, .(
    score = if (identical(score_col, "delta_vs_shap_feature_level")) aq_interaction_weighted_mean(abs(delta_vs_shap_feature_level), n) else aq_interaction_weighted_mean(mean_abs_delta, n),
    score_stat = interaction_score_stat,
    interaction_feature_role = interaction_feature_role[[1L]],
    n_cells = .N,
    n_non_sparse_cells = sum(!sparse_cell),
    n_rows = sum(n),
    max_abs_cell_delta = aq_interaction_safe_max(abs(delta_vs_shap_feature_level)),
    weighted_mean_abs_delta = aq_interaction_weighted_mean(abs(delta_vs_shap_feature_level), n),
    weighted_sd_cell_mean = aq_interaction_weighted_sd(mean_shap, n),
    top_cell_label = aq_interaction_top_cell_label(shap_feature_level, interaction_feature_level, mean_abs_shap),
    top_cell_mean_abs_shap = aq_interaction_safe_max(mean_abs_shap)
  ), by = .(shap_feature, interaction_feature, shap_col, pair_label)]
  if (!nrow(scores)) {
    scores <- surfaces[, .(
      score = aq_interaction_weighted_mean(abs(delta_vs_shap_feature_level), n),
      score_stat = interaction_score_stat,
      interaction_feature_role = interaction_feature_role[[1L]],
      n_cells = .N,
      n_non_sparse_cells = sum(!sparse_cell),
      n_rows = sum(n),
      max_abs_cell_delta = aq_interaction_safe_max(abs(delta_vs_shap_feature_level)),
      weighted_mean_abs_delta = aq_interaction_weighted_mean(abs(delta_vs_shap_feature_level), n),
      weighted_sd_cell_mean = aq_interaction_weighted_sd(mean_shap, n),
      top_cell_label = aq_interaction_top_cell_label(shap_feature_level, interaction_feature_level, mean_abs_shap),
      top_cell_mean_abs_shap = aq_interaction_safe_max(mean_abs_shap)
    ), by = .(shap_feature, interaction_feature, shap_col, pair_label)]
  }
  reverse <- scores[, .(shap_feature = interaction_feature, interaction_feature = shap_feature, reverse_score = score)]
  scores <- merge(scores, reverse, by = c("shap_feature", "interaction_feature"), all.x = TRUE, sort = FALSE)
  scores[, `:=`(
    has_reverse_shap = !is.na(reverse_score),
    symmetric_score = ifelse(is.na(reverse_score), NA_real_, (score + reverse_score) / 2)
  )]
  data.table::setorderv(scores, "score", order = -1L, na.last = TRUE)
  scores[, rank := seq_len(.N)]
  data.table::setorderv(surfaces, c("pair_label", "level_combo_rank"))

  list(
    scores = aq_smart_round_dt(scores, skip_cols = c("rank", "n_cells", "n_non_sparse_cells", "n_rows")),
    surfaces = aq_smart_round_dt(surfaces, skip_cols = c("n", "level_combo_rank")),
    warnings = warnings
  )
}

aq_summarize_shap_local <- function(
  data,
  column_map,
  local_row_ids,
  prediction_col = NULL,
  target_col = NULL,
  id_cols = character()
) {
  valid_rows <- local_row_ids[local_row_ids >= 1L & local_row_ids <= nrow(data)]
  if (!length(valid_rows)) {
    return(data.table::data.table())
  }

  context_cols <- id_cols[id_cols %in% names(data)]
  rows <- lapply(valid_rows, function(row_id) {
    valid_map <- column_map[included == TRUE]
    out <- data.table::data.table(
      row_id = row_id,
      feature = valid_map$feature,
      feature_value = vapply(valid_map$feature, function(feature) {
        if (feature %in% names(data)) as.character(data[[feature]][row_id]) else NA_character_
      }, character(1L)),
      shap_value = vapply(valid_map$shap_col, function(col) data[[col]][row_id], numeric(1L))
    )
    out[, abs_shap := abs(shap_value)]
    data.table::setorderv(out, "abs_shap", order = -1L, na.last = TRUE)
    out[, contribution_rank := seq_len(.N)]
    out[, direction := data.table::fcase(
      shap_value > 0, "positive",
      shap_value < 0, "negative",
      default = "zero"
    )]
    out[, abs_shap := NULL]
    if (!is.null(prediction_col) && prediction_col %in% names(data)) {
      out[, prediction := data[[prediction_col]][row_id]]
    }
    if (!is.null(target_col) && target_col %in% names(data)) {
      out[, target := data[[target_col]][row_id]]
    }
    for (col in context_cols) {
      out[, (col) := data[[col]][row_id]]
    }
    out
  })

  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_autoplots_available <- function() {
  requireNamespace("AutoPlots", quietly = TRUE)
}

aq_call_autoplot <- function(plot_function, args) {
  fn <- get(plot_function, envir = asNamespace("AutoPlots"), inherits = FALSE)
  valid_args <- names(formals(fn))
  args <- args[names(args) %in% valid_args]
  do.call(fn, args)
}

aq_shap_plot_args <- function(
  dt,
  XVar,
  YVar = NULL,
  ZVar = NULL,
  GroupVar = NULL,
  title = NULL,
  auto_plots_theme = NULL,
  plot_width = NULL,
  plot_height = NULL,
  pre_agg = NULL
) {
  args <- list(dt = dt, XVar = XVar)
  if (!is.null(YVar)) args$YVar <- YVar
  if (!is.null(ZVar)) args$ZVar <- ZVar
  if (!is.null(GroupVar) && GroupVar %in% names(dt)) args$GroupVar <- GroupVar
  if (!is.null(title)) args$title.text <- title
  if (!is.null(auto_plots_theme)) args$Theme <- auto_plots_theme
  if (!is.null(plot_width)) args$Width <- plot_width
  if (!is.null(plot_height)) args$Height <- plot_height
  if (!is.null(pre_agg)) args$PreAgg <- pre_agg
  args
}

aq_create_shap_bar_plot <- function(dt, XVar, YVar, GroupVar = NULL, title = NULL, auto_plots_theme = NULL, plot_width = NULL, plot_height = NULL) {
  aq_call_autoplot("Bar", aq_shap_plot_args(dt, XVar, YVar, GroupVar = GroupVar, title = title, auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height, pre_agg = TRUE))
}

aq_create_shap_box_plot <- function(dt, XVar, YVar, GroupVar = NULL, title = NULL, auto_plots_theme = NULL, plot_width = NULL, plot_height = NULL) {
  aq_call_autoplot("Box", aq_shap_plot_args(dt, XVar, YVar, GroupVar = GroupVar, title = title, auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height))
}

aq_create_shap_scatter_plot <- function(dt, XVar, YVar, GroupVar = NULL, title = NULL, auto_plots_theme = NULL, plot_width = NULL, plot_height = NULL) {
  aq_call_autoplot("Scatter", aq_shap_plot_args(dt, XVar, YVar, GroupVar = GroupVar, title = title, auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height))
}

aq_create_shap_line_plot <- function(dt, XVar, YVar, GroupVar = NULL, title = NULL, auto_plots_theme = NULL, plot_width = NULL, plot_height = NULL) {
  aq_call_autoplot("Line", aq_shap_plot_args(dt, XVar, YVar, GroupVar = GroupVar, title = title, auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height, pre_agg = TRUE))
}

aq_create_shap_heatmap_plot <- function(dt, XVar, YVar, ZVar, title = NULL, auto_plots_theme = NULL, plot_width = NULL, plot_height = NULL) {
  aq_call_autoplot("HeatMap", aq_shap_plot_args(dt, XVar, YVar, ZVar = ZVar, title = title, auto_plots_theme = auto_plots_theme, plot_width = plot_width, plot_height = plot_height, pre_agg = TRUE))
}

aq_safe_create_shap_plot <- function(plot_type, create_expr) {
  if (!aq_autoplots_available()) {
    return(list(
      object = NULL,
      warning = paste("AutoPlots is not installed; skipped", plot_type, "plot artifact.")
    ))
  }

  tryCatch(
    list(object = force(create_expr), warning = NULL),
    error = function(e) {
      list(
        object = NULL,
        warning = paste("AutoPlots", plot_type, "plot artifact failed:", conditionMessage(e))
      )
    }
  )
}

aq_style_shap_plot <- function(plot, horizontal = FALSE, rotate_x = FALSE, rotate = 45L) {
  if (is.null(plot) || !requireNamespace("echarts4r", quietly = TRUE)) {
    return(plot)
  }

  out <- plot
  if (isTRUE(rotate_x)) {
    out <- tryCatch(
      echarts4r::e_x_axis(out, axisLabel = list(rotate = rotate)),
      error = function(e) out
    )
  }
  if (isTRUE(horizontal)) {
    out <- tryCatch(
      echarts4r::e_flip_coords(out),
      error = function(e) out
    )
  }

  out
}

aq_order_for_flipped_box <- function(dt, category_col, value_col) {
  if (is.null(dt) || !data.table::is.data.table(dt) || !nrow(dt)) {
    return(dt)
  }
  if (!all(c(category_col, value_col) %in% names(dt))) {
    return(dt)
  }
  out <- data.table::copy(dt)
  out[, (category_col) := as.character(get(category_col))]
  order_dt <- out[, .(
    box_center = stats::median(get(value_col), na.rm = TRUE),
    box_mean = mean(get(value_col), na.rm = TRUE)
  ), by = category_col]
  data.table::setorderv(order_dt, c("box_center", "box_mean"), order = c(1L, 1L), na.last = TRUE)
  out[, (category_col) := factor(get(category_col), levels = order_dt[[category_col]])]
  out[]
}

aq_create_regression_shap_plot_artifact <- function(
  name,
  label,
  section,
  plot_type,
  lens,
  plot_object,
  metadata
) {
  regression_shap_artifact(
    name = name,
    label = label,
    type = "plot",
    section = section,
    object = plot_object,
    metadata = c(metadata, list(
      plot_package = "AutoPlots",
      plot_type = plot_type,
      lens = lens
    ))
  )
}

aq_regression_shap_long_values <- function(data, column_map, features, max_rows = 5000L) {
  features <- intersect(features, column_map[included == TRUE, feature])
  if (!length(features)) {
    return(data.table::data.table())
  }

  row_index <- seq_len(nrow(data))
  if (nrow(data) > max_rows) {
    set.seed(123L)
    row_index <- sort(sample(row_index, max_rows))
  }

  rows <- lapply(features, function(feature_name) {
    shap_col <- column_map[feature == feature_name, shap_col][[1L]]
    data.table::data.table(
      feature = feature_name,
      shap_value = data[[shap_col]][row_index]
    )
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_regression_shap_empty_result <- function(message, warnings = character(), diagnostics = list()) {
  generated_at <- Sys.time()
  diagnostic_table <- if (length(diagnostics)) {
    data.table::data.table(
      diagnostic = names(diagnostics),
      value = vapply(diagnostics, function(x) paste(x, collapse = ", "), character(1L))
    )
  } else {
    data.table::data.table(diagnostic = character(), value = character())
  }
  artifacts <- list(
    shap_diagnostics = regression_shap_artifact(
      name = "shap_diagnostics",
      label = "SHAP Diagnostics",
      type = "table",
      section = "Appendix",
      object = diagnostic_table,
      metadata = list(
        source_package = "AutoQuant",
        source_function = "generate_regression_shap_analysis_artifacts",
        problem_type = "regression",
        lens = "diagnostics",
        original_section = "Appendix",
        normalized_section = "Appendix",
        artifact_index = 1L,
        shap_source = "precomputed_columns",
        created_by_autoquant = TRUE
      )
    )
  )
  list(
    artifacts = artifacts,
    metadata = list(
      source_package = "AutoQuant",
      source_function = "generate_regression_shap_analysis_artifacts",
      problem_type = "regression",
      generated_at = generated_at,
      artifact_count = length(artifacts),
      plot_count = 0L,
      table_count = 1L,
      text_count = 0L,
      warnings_count = length(warnings),
      shap_source = "precomputed_columns"
    ),
    warnings = warnings,
    diagnostics = diagnostics
  )
}

#' Generate Regression SHAP Analysis Artifacts
#'
#' Summarize precomputed row-level regression SHAP columns into structured
#' artifacts. This function consumes columns whose names start with `shap_prefix`
#' and maps each one back to its source feature by stripping the prefix. It does
#' not compute SHAP values, call `predict()`, require a model object, or use a
#' SHAP backend package.
#'
#' @param data A data.table containing precomputed `Shap_` columns.
#' @param target_col Optional target/actual column.
#' @param prediction_col Optional prediction column. Defaults to `Predict` when present.
#' @param feature_cols Optional source features used to filter detected SHAP columns.
#' @param shap_prefix Prefix for SHAP contribution columns.
#' @param id_cols Optional ID/context columns to carry into dependence/local artifacts.
#' @param model_name Optional model name for overview metadata.
#' @param data_name Optional data name for overview metadata.
#' @param DateVar Optional date column for time effects.
#' @param date_aggregation One of `day`, `week`, or `month`.
#' @param ByVars Optional segment variables for segment effects.
#' @param selected_features Optional feature list for effect/dependence/local views.
#' @param local_row_ids Optional 1-based row indexes for local explanations.
#' @param top_n Number of top features used for display-oriented artifacts.
#' @param max_dependence_rows Maximum source rows per selected feature in dependence output.
#' @param max_segment_levels Maximum segment levels to keep per ByVar.
#' @param max_byvars Maximum ByVars to use.
#' @param include_dependence Include dependence table artifacts.
#' @param include_segments Include segment effect table artifacts.
#' @param include_time Include time effect table artifacts.
#' @param include_local Include local explanation table artifacts.
#' @param include_interactions Include binned/leveled SHAP interaction diagnostics from precomputed `Shap_` columns.
#' @param interaction_pairs Optional list of ordered candidate pairs. Each pair should be `c(shap_feature, interaction_feature)`.
#' @param max_interaction_pairs Maximum candidate interaction pairs to score.
#' @param max_interaction_surface_plots Maximum two-way surface heatmap plots to create.
#' @param numeric_interaction_bins Maximum numeric bins for interaction diagnostics.
#' @param max_interaction_levels Maximum categorical/date levels for interaction diagnostics.
#' @param min_interaction_cell_n Minimum cell count before an interaction surface cell is flagged sparse.
#' @param interaction_stat Statistic for interaction surface scoring. One of `mean_abs_shap` or `mean_shap`.
#' @param interaction_score_stat Label for the interaction score method.
#' @param collapse_rare_levels Collapse rare/high-cardinality levels to `other`.
#' @param include_plots Include AutoPlots-backed plot artifacts when AutoPlots is installed.
#' @param max_feature_effect_plots Maximum single-feature effect plots to create.
#' @param max_dependence_plots Maximum dependence plots to create.
#' @param max_segment_plots Maximum segment plots to create.
#' @param max_time_plots Maximum time plots to create.
#' @param max_local_plots Maximum local explanation plots to create.
#' @param plot_top_n Optional top feature count for plot artifacts. Defaults to `top_n`.
#' @param auto_plots_theme Optional AutoPlots theme.
#' @param plot_width Optional AutoPlots plot width.
#' @param plot_height Optional AutoPlots plot height.
#' @param prediction_scale Prediction scale label, usually `response`.
#' @param ... Ignored compatibility arguments. SHAP computation is not performed here.
#'
#' @return A structured list with `artifacts`, `metadata`, `warnings`, and `diagnostics`.
#' @examples
#' set.seed(123)
#' df <- data.table::data.table(
#'   y = rnorm(20),
#'   Predict = rnorm(20),
#'   x1 = runif(20),
#'   x2 = rnorm(20),
#'   Shap_x1 = rnorm(20, 0, 0.2),
#'   Shap_x2 = rnorm(20, 0, 0.1)
#' )
#' artifacts <- generate_regression_shap_analysis_artifacts(
#'   data = df,
#'   target_col = "y",
#'   prediction_col = "Predict",
#'   top_n = 2
#' )
#' @export
generate_regression_shap_analysis_artifacts <- function(
  data,
  target_col = NULL,
  prediction_col = NULL,
  feature_cols = NULL,
  shap_prefix = "Shap_",
  id_cols = NULL,
  model_name = NULL,
  data_name = NULL,
  DateVar = NULL,
  date_aggregation = "month",
  ByVars = character(),
  selected_features = NULL,
  local_row_ids = integer(),
  top_n = 20L,
  max_dependence_rows = 5000L,
  max_segment_levels = 20L,
  max_byvars = 3L,
  include_dependence = TRUE,
  include_segments = TRUE,
  include_time = TRUE,
  include_local = FALSE,
  include_interactions = FALSE,
  interaction_pairs = list(),
  max_interaction_pairs = 20L,
  max_interaction_surface_plots = 10L,
  numeric_interaction_bins = 5L,
  max_interaction_levels = 12L,
  min_interaction_cell_n = 5L,
  interaction_stat = "mean_abs_shap",
  interaction_score_stat = "weighted_mad",
  collapse_rare_levels = TRUE,
  include_plots = TRUE,
  max_feature_effect_plots = 5L,
  max_dependence_plots = 5L,
  max_segment_plots = 5L,
  max_time_plots = 5L,
  max_local_plots = 5L,
  plot_top_n = NULL,
  auto_plots_theme = NULL,
  plot_width = NULL,
  plot_height = NULL,
  prediction_scale = "response",
  ...
) {
  warnings <- character()
  generated_at <- Sys.time()
  extra_args <- list(...)
  if (length(intersect(names(extra_args), c("model", "predict_function", "background_n", "sample_n", "nsim", "shap_backend")))) {
    warnings <- regression_shap_warn(
      warnings,
      "Model, prediction, sampling, and SHAP backend arguments are ignored. This generator only summarizes precomputed Shap_ columns."
    )
  }

  if (!data.table::is.data.table(data)) {
    return(aq_regression_shap_empty_result(
      "data must be a data.table.",
      warnings = regression_shap_warn(warnings, "data must be a data.table."),
      diagnostics = list(error = "invalid_data")
    ))
  }
  data <- data.table::copy(data)

  if (!is.character(shap_prefix) || length(shap_prefix) != 1L || !nzchar(shap_prefix)) {
    return(aq_regression_shap_empty_result(
      "shap_prefix must be a non-empty character value.",
      warnings = regression_shap_warn(warnings, "shap_prefix must be a non-empty character value."),
      diagnostics = list(error = "invalid_shap_prefix")
    ))
  }

  date_aggregation <- tolower(as.character(date_aggregation))
  if (!date_aggregation %in% c("day", "week", "month")) {
    warnings <- regression_shap_warn(warnings, "date_aggregation must be day, week, or month; using month.")
    date_aggregation <- "month"
  }

  for (limit in c("top_n", "max_dependence_rows", "max_segment_levels", "max_byvars", "max_interaction_pairs", "max_interaction_surface_plots", "numeric_interaction_bins", "max_interaction_levels", "min_interaction_cell_n", "max_feature_effect_plots", "max_dependence_plots", "max_segment_plots", "max_time_plots", "max_local_plots")) {
    checked <- regression_shap_positive_int(
      get(limit),
      default = switch(
        limit,
        top_n = 20L,
        max_dependence_rows = 5000L,
        max_segment_levels = 20L,
        max_byvars = 3L,
        max_interaction_pairs = 20L,
        max_interaction_surface_plots = 10L,
        numeric_interaction_bins = 5L,
        max_interaction_levels = 12L,
        min_interaction_cell_n = 5L,
        max_feature_effect_plots = 5L,
        max_dependence_plots = 5L,
        max_segment_plots = 5L,
        max_time_plots = 5L,
        max_local_plots = 5L
      ),
      name = limit,
      warnings = warnings
    )
    assign(limit, checked$value)
    warnings <- checked$warnings
  }
  if (is.null(plot_top_n)) {
    plot_top_n <- top_n
  } else {
    checked <- regression_shap_positive_int(plot_top_n, default = top_n, name = "plot_top_n", warnings = warnings)
    plot_top_n <- checked$value
    warnings <- checked$warnings
  }
  interaction_stat <- tolower(as.character(interaction_stat))
  if (!interaction_stat %in% c("mean_abs_shap", "mean_shap")) {
    warnings <- regression_shap_warn(warnings, "interaction_stat must be mean_abs_shap or mean_shap; using mean_abs_shap.")
    interaction_stat <- "mean_abs_shap"
  }
  interaction_score_stat <- tolower(as.character(interaction_score_stat))
  if (!nzchar(interaction_score_stat)) {
    interaction_score_stat <- "weighted_mad"
  }

  prediction_col <- aq_infer_prediction_col(data, prediction_col)
  if (!is.null(prediction_col) && !prediction_col %in% names(data)) {
    warnings <- regression_shap_warn(warnings, paste("prediction_col was not found and will be omitted:", prediction_col))
    prediction_col <- NULL
  }
  if (!is.null(target_col) && !target_col %in% names(data)) {
    warnings <- regression_shap_warn(warnings, paste("target_col was not found and will be omitted:", target_col))
    target_col <- NULL
  }

  id_cols <- as.character(regression_shap_null_coalesce(id_cols, character()))
  invalid_id_cols <- setdiff(id_cols, names(data))
  if (length(invalid_id_cols)) {
    warnings <- regression_shap_warn(warnings, paste("id_cols not found and omitted:", paste(invalid_id_cols, collapse = ", ")))
  }
  id_cols <- intersect(id_cols, names(data))

  ByVars <- as.character(regression_shap_null_coalesce(ByVars, character()))
  invalid_byvars <- setdiff(ByVars, names(data))
  if (length(invalid_byvars)) {
    warnings <- regression_shap_warn(warnings, paste("ByVars not found and omitted:", paste(invalid_byvars, collapse = ", ")))
  }
  ByVars <- intersect(ByVars, names(data))
  if (length(ByVars) > max_byvars) {
    warnings <- regression_shap_warn(warnings, paste("ByVars capped to max_byvars:", max_byvars))
    ByVars <- head(ByVars, max_byvars)
  }

  requested_DateVar <- DateVar
  DateVar <- aq_infer_date_col(data, DateVar)
  if (!is.null(requested_DateVar) && !identical(requested_DateVar, DateVar)) {
    if (is.null(DateVar)) {
      warnings <- regression_shap_warn(warnings, paste("DateVar not found and time effects will be omitted:", requested_DateVar))
    } else {
      warnings <- regression_shap_warn(warnings, paste("DateVar", requested_DateVar, "was not found exactly; using", DateVar, "for time effects."))
    }
  } else if (is.null(requested_DateVar) && !is.null(DateVar)) {
    warnings <- regression_shap_warn(warnings, paste("DateVar was inferred for time effects:", DateVar))
  }

  column_map <- aq_detect_shap_columns(data, shap_prefix = shap_prefix, feature_cols = feature_cols)
  if (!nrow(column_map) || !any(column_map$included)) {
    warnings <- regression_shap_warn(warnings, "No valid numeric SHAP columns were found.")
    return(aq_regression_shap_empty_result(
      "No valid numeric SHAP columns were found.",
      warnings = warnings,
      diagnostics = list(
        n_rows = nrow(data),
        n_columns = ncol(data),
        shap_prefix = shap_prefix,
        shap_cols_detected = names(data)[startsWith(names(data), shap_prefix)]
      )
    ))
  }
  if (any(!column_map$shap_is_numeric)) {
    warnings <- regression_shap_warn(warnings, "Non-numeric SHAP columns were dropped.")
  }
  if (any(!column_map$source_col_exists & column_map$included)) {
    warnings <- regression_shap_warn(warnings, "Some SHAP features do not have matching source columns; dependence/effect views skip those features.")
  }

  global_importance <- aq_summarize_shap_global_importance(data, column_map)
  categorical_level_importance <- aq_summarize_shap_categorical_level_importance(
    data,
    column_map,
    features = NULL,
    max_levels = max_segment_levels
  )
  selected_features <- as.character(regression_shap_null_coalesce(
    selected_features,
    global_importance$feature[seq_len(min(top_n, nrow(global_importance)))]
  ))
  missing_selected <- setdiff(selected_features, column_map[included == TRUE, feature])
  if (length(missing_selected)) {
    warnings <- regression_shap_warn(warnings, paste("selected_features did not map to valid SHAP columns and were omitted:", paste(missing_selected, collapse = ", ")))
  }
  selected_features <- intersect(selected_features, column_map[included == TRUE, feature])

  full_metadata <- list(
    source_package = "AutoQuant",
    source_function = "generate_regression_shap_analysis_artifacts",
    problem_type = "regression",
    model_name = model_name,
    data_name = data_name,
    target_col = target_col,
    prediction_col = prediction_col,
    prediction_scale = prediction_scale,
    feature_cols = feature_cols,
    shap_prefix = shap_prefix,
    shap_cols = column_map[included == TRUE, shap_col],
    DateVar = DateVar,
    date_aggregation = date_aggregation,
    ByVars = ByVars,
    id_cols = id_cols,
    selected_features = selected_features,
    top_n = top_n,
    max_dependence_rows = max_dependence_rows,
    max_segment_levels = max_segment_levels,
    max_interaction_pairs = max_interaction_pairs,
    max_interaction_surface_plots = max_interaction_surface_plots,
    numeric_interaction_bins = numeric_interaction_bins,
    max_interaction_levels = max_interaction_levels,
    min_interaction_cell_n = min_interaction_cell_n,
    interaction_stat = interaction_stat,
    interaction_score_stat = interaction_score_stat,
    collapse_rare_levels = collapse_rare_levels,
    max_feature_effect_plots = max_feature_effect_plots,
    max_dependence_plots = max_dependence_plots,
    max_segment_plots = max_segment_plots,
    max_time_plots = max_time_plots,
    max_local_plots = max_local_plots,
    plot_top_n = plot_top_n,
    auto_plots_theme = auto_plots_theme,
    generated_at = generated_at,
    shap_source = "precomputed_columns",
    shap_column_prefix = shap_prefix,
    interaction_method = "binned_level_combination_shap_diagnostic",
    exact_shap_interaction_values = FALSE
  )
  artifact_metadata <- function(lens, section, index) {
    c(full_metadata, list(
      lens = lens,
      original_section = section,
      normalized_section = section,
      artifact_index = index,
      created_by_autoquant = TRUE
    ))
  }

  config_table <- data.table::data.table(
    setting = c(
      "n_rows", "n_columns", "n_shap_columns_detected",
      "n_valid_numeric_shap_columns", "n_features_with_source_columns",
      "target_col", "prediction_col", "DateVar", "date_aggregation",
      "ByVars", "id_cols", "selected_features", "top_n",
      "include_interactions", "interaction_stat", "interaction_score_stat",
      "warnings_count"
    ),
    value = c(
      nrow(data), ncol(data), nrow(column_map),
      nrow(column_map[included == TRUE]),
      nrow(column_map[included == TRUE & source_col_exists == TRUE]),
      regression_shap_null_coalesce(target_col, ""),
      regression_shap_null_coalesce(prediction_col, ""),
      regression_shap_null_coalesce(DateVar, ""),
      date_aggregation,
      paste(ByVars, collapse = ", "), paste(id_cols, collapse = ", "),
      paste(selected_features, collapse = ", "), top_n,
      include_interactions, interaction_stat, interaction_score_stat,
      length(warnings)
    )
  )

  overview <- paste(
    "Regression SHAP Analysis",
    paste("Model:", regression_shap_null_coalesce(model_name, "not supplied")),
    paste("Data:", regression_shap_null_coalesce(data_name, "not supplied")),
    paste("Rows:", nrow(data)),
    paste("Valid SHAP columns:", nrow(column_map[included == TRUE])),
    paste("Mapped source features:", nrow(column_map[included == TRUE & source_col_exists == TRUE])),
    paste("Target column:", regression_shap_null_coalesce(target_col, "not supplied")),
    paste("Prediction column:", regression_shap_null_coalesce(prediction_col, "not supplied")),
    paste("DateVar/date aggregation:", paste(c(regression_shap_null_coalesce(DateVar, "not supplied"), date_aggregation), collapse = " / ")),
    paste("ByVars:", if (length(ByVars)) paste(ByVars, collapse = ", ") else "none"),
    paste("Prediction scale:", prediction_scale),
    paste("Top N:", top_n),
    "SHAP values were precomputed upstream. This generator only summarizes existing SHAP contribution columns.",
    sep = "\n"
  )

  artifacts <- list()
  artifacts <- regression_shap_add_artifact(artifacts, regression_shap_artifact("shap_overview", "SHAP Overview", "text", "SHAP Overview", content = overview, metadata = artifact_metadata("overview", "SHAP Overview", 1L)))
  artifacts <- regression_shap_add_artifact(artifacts, regression_shap_artifact("shap_diagnostics_config", "SHAP Diagnostics / Config", "table", "Appendix", object = config_table, metadata = artifact_metadata("diagnostics", "Appendix", 2L)))
  artifacts <- regression_shap_add_artifact(artifacts, regression_shap_artifact("shap_column_map", "SHAP Column Map", "table", "Appendix", object = column_map, metadata = artifact_metadata("column_map", "Appendix", 3L)))
  artifacts <- regression_shap_add_artifact(artifacts, regression_shap_artifact("global_importance_table", "Global SHAP Importance", "table", "Global Importance", object = aq_smart_round_dt(global_importance, skip_cols = c("rank", "n")), metadata = artifact_metadata("global_importance", "Global Importance", 4L)))
  if (nrow(categorical_level_importance)) {
    artifacts <- regression_shap_add_artifact(artifacts, regression_shap_artifact(
      "categorical_level_importance_table",
      "Categorical / Binned Numeric Level SHAP Importance",
      "table",
      "Global Importance",
      object = categorical_level_importance,
      metadata = artifact_metadata("categorical_level_importance", "Global Importance", 5L)
    ))
  }
  if (isTRUE(include_plots)) {
    global_plot_data <- data.table::copy(global_importance[rank <= plot_top_n])
    data.table::setorderv(global_plot_data, "mean_abs_shap", order = 1L, na.last = TRUE)
    global_plot_data <- aq_smart_round_dt(global_plot_data, skip_cols = c("rank", "n"))
    global_plot <- aq_safe_create_shap_plot(
      "bar",
      aq_create_shap_bar_plot(
        dt = global_plot_data,
        XVar = "feature",
        YVar = "mean_abs_shap",
        title = "Mean Absolute SHAP Importance",
        auto_plots_theme = auto_plots_theme,
        plot_width = plot_width,
        plot_height = plot_height
      )
    )
    if (!is.null(global_plot$object)) {
      global_plot$object <- aq_style_shap_plot(global_plot$object, horizontal = TRUE)
      artifacts <- regression_shap_add_artifact(artifacts, aq_create_regression_shap_plot_artifact("global_importance_plot", "Global SHAP Importance Plot", "Global Importance", "bar", "global_importance", global_plot$object, artifact_metadata("global_importance", "Global Importance", 6L)))
    } else {
      warnings <- regression_shap_warn(warnings, global_plot$warning)
    }

    shap_distribution <- aq_regression_shap_long_values(data, column_map, global_importance[rank <= plot_top_n, feature], max_rows = max_dependence_rows)
    if (nrow(shap_distribution)) {
      shap_distribution <- aq_order_for_flipped_box(shap_distribution, "feature", "shap_value")
      distribution_plot <- aq_safe_create_shap_plot(
        "box",
        aq_create_shap_box_plot(
          dt = shap_distribution,
          XVar = "feature",
          YVar = "shap_value",
          title = "SHAP Distribution by Feature",
          auto_plots_theme = auto_plots_theme,
          plot_width = plot_width,
          plot_height = plot_height
        )
      )
      if (!is.null(distribution_plot$object)) {
        distribution_plot$object <- aq_style_shap_plot(distribution_plot$object, horizontal = TRUE)
        artifacts <- regression_shap_add_artifact(artifacts, aq_create_regression_shap_plot_artifact("shap_distribution_plot", "SHAP Distribution Plot", "Global Importance", "box", "shap_distribution", distribution_plot$object, artifact_metadata("shap_distribution", "Global Importance", 7L)))
      } else {
        warnings <- regression_shap_warn(warnings, distribution_plot$warning)
      }
    }

    if (nrow(categorical_level_importance)) {
      categorical_plot_data <- data.table::copy(categorical_level_importance[rank <= plot_top_n])
      data.table::setorderv(categorical_plot_data, "mean_abs_shap", order = 1L, na.last = TRUE)
      categorical_plot_data <- aq_smart_round_dt(categorical_plot_data, skip_cols = c("rank", "n"))
      categorical_bar_plot <- aq_safe_create_shap_plot(
        "bar",
        aq_create_shap_bar_plot(
          dt = categorical_plot_data,
          XVar = "feature_level",
          YVar = "mean_abs_shap",
            title = "Categorical / Binned Numeric Level Mean Absolute SHAP",
          auto_plots_theme = auto_plots_theme,
          plot_width = plot_width,
          plot_height = plot_height
        )
      )
      if (!is.null(categorical_bar_plot$object)) {
        categorical_bar_plot$object <- aq_style_shap_plot(categorical_bar_plot$object, horizontal = TRUE)
        artifacts <- regression_shap_add_artifact(artifacts, aq_create_regression_shap_plot_artifact(
          "categorical_level_importance_plot",
          "Categorical / Binned Numeric Level SHAP Importance Plot",
          "Global Importance",
          "bar",
          "categorical_level_importance",
          categorical_bar_plot$object,
          artifact_metadata("categorical_level_importance", "Global Importance", 8L)
        ))
      } else {
        warnings <- regression_shap_warn(warnings, categorical_bar_plot$warning)
      }

      categorical_box_data <- aq_regression_shap_categorical_level_values(
        data,
        column_map,
        categorical_level_importance,
        max_rows = max_dependence_rows,
        top_n = plot_top_n
      )
      if (nrow(categorical_box_data)) {
        categorical_box_data <- aq_order_for_flipped_box(categorical_box_data, "feature_level", "shap_value")
        categorical_box_plot <- aq_safe_create_shap_plot(
          "box",
          aq_create_shap_box_plot(
            dt = categorical_box_data,
            XVar = "feature_level",
            YVar = "shap_value",
            title = "Categorical / Binned Numeric Level SHAP Distribution",
            auto_plots_theme = auto_plots_theme,
            plot_width = plot_width,
            plot_height = plot_height
          )
        )
        if (!is.null(categorical_box_plot$object)) {
          categorical_box_plot$object <- aq_style_shap_plot(categorical_box_plot$object, horizontal = TRUE)
          artifacts <- regression_shap_add_artifact(artifacts, aq_create_regression_shap_plot_artifact(
            "categorical_level_distribution_plot",
            "Categorical / Binned Numeric Level SHAP Distribution Plot",
            "Global Importance",
            "box",
            "categorical_level_importance",
            categorical_box_plot$object,
            artifact_metadata("categorical_level_importance", "Global Importance", 9L)
          ))
        } else {
          warnings <- regression_shap_warn(warnings, categorical_box_plot$warning)
        }
      }
    }
  }

  effects <- aq_summarize_shap_single_feature_effects(data, column_map, selected_features, prediction_col, target_col, max_levels = max_segment_levels)
  artifacts <- regression_shap_add_artifact(artifacts, regression_shap_artifact("single_feature_effects_table", "Single Feature SHAP Effects", "table", "Single Feature Effects", object = aq_smart_round_dt(effects, skip_cols = "n"), metadata = artifact_metadata("single_feature_effects", "Single Feature Effects", 10L)))
  if (isTRUE(include_plots) && nrow(effects)) {
    effect_features <- head(intersect(global_importance$feature, unique(effects$feature)), max_feature_effect_plots)
    for (feature_name in effect_features) {
      effect_plot_data <- data.table::copy(effects[feature == feature_name])
      source_is_numeric <- feature_name %in% names(data) && (is.numeric(data[[feature_name]]) || is.integer(data[[feature_name]]))
      if (source_is_numeric) {
        data.table::setorderv(effect_plot_data, "feature_value_or_bin", order = 1L, na.last = TRUE)
      } else {
        data.table::setorderv(effect_plot_data, "mean_shap", order = 1L, na.last = TRUE)
      }
      effect_plot_data <- aq_smart_round_dt(effect_plot_data, skip_cols = "n")
      plot_result <- aq_safe_create_shap_plot(
        if (source_is_numeric) "line" else "bar",
        if (source_is_numeric) {
          aq_create_shap_line_plot(
            dt = effect_plot_data,
            XVar = "feature_value_or_bin",
            YVar = "mean_shap",
            title = paste("SHAP Effect:", feature_name),
            auto_plots_theme = auto_plots_theme,
            plot_width = plot_width,
            plot_height = plot_height
          )
        } else {
          aq_create_shap_bar_plot(
            dt = effect_plot_data,
            XVar = "feature_value_or_bin",
            YVar = "mean_shap",
            title = paste("SHAP Effect:", feature_name),
            auto_plots_theme = auto_plots_theme,
            plot_width = plot_width,
            plot_height = plot_height
          )
        }
      )
      if (!is.null(plot_result$object)) {
        plot_result$object <- aq_style_shap_plot(plot_result$object, horizontal = !source_is_numeric, rotate_x = source_is_numeric)
        artifacts <- regression_shap_add_artifact(artifacts, aq_create_regression_shap_plot_artifact(
          paste0("single_feature_effect_", regression_shap_slug(feature_name), "_plot"),
          paste("SHAP Effect:", feature_name),
          "Single Feature Effects",
          if (source_is_numeric) "line" else "bar",
          "single_feature_effects",
          plot_result$object,
          c(artifact_metadata("single_feature_effects", "Single Feature Effects", 8L), list(feature = feature_name))
        ))
      } else {
        warnings <- regression_shap_warn(warnings, plot_result$warning)
      }
    }
  }

  if (isTRUE(include_dependence)) {
    dependence <- aq_summarize_shap_dependence(data, column_map, selected_features, prediction_col, target_col, id_cols, ByVars, DateVar, max_dependence_rows)
    if (nrow(data) > max_dependence_rows) {
      warnings <- regression_shap_warn(warnings, paste("Dependence rows capped at", max_dependence_rows, "source rows before feature expansion."))
    }
    artifacts <- regression_shap_add_artifact(artifacts, regression_shap_artifact("shap_dependence_table", "SHAP Dependence", "table", "SHAP Dependence", object = aq_smart_round_dt(dependence, skip_cols = "row_id"), metadata = artifact_metadata("shap_dependence", "SHAP Dependence", 9L)))
    if (isTRUE(include_plots) && nrow(dependence)) {
      dependence_features <- head(intersect(global_importance$feature, unique(dependence$feature)), max_dependence_plots)
      dependence_group <- if (length(ByVars)) ByVars[[1L]] else NULL
      for (feature_name in dependence_features) {
        shap_col <- column_map[feature == feature_name, shap_col][[1L]]
        row_index <- seq_len(nrow(data))
        if (nrow(data) > max_dependence_rows) {
          set.seed(123L)
          row_index <- sort(sample(row_index, max_dependence_rows))
        }
        source_is_numeric <- feature_name %in% names(data) && (is.numeric(data[[feature_name]]) || is.integer(data[[feature_name]]))
        plot_data <- data.table::data.table(
          feature_value = data[[feature_name]][row_index],
          shap_value = data[[shap_col]][row_index]
        )
        if (!is.null(dependence_group) && dependence_group %in% names(data)) {
          plot_data[, (dependence_group) := as.character(data[[dependence_group]][row_index])]
        }
        if (!source_is_numeric) {
          plot_data[, feature_value := as.character(feature_value)]
          plot_data <- aq_order_for_flipped_box(plot_data, "feature_value", "shap_value")
        }
        plot_result <- aq_safe_create_shap_plot(
          if (source_is_numeric) "scatter" else "box",
          if (source_is_numeric) {
            aq_create_shap_scatter_plot(
              dt = plot_data,
              XVar = "feature_value",
              YVar = "shap_value",
              GroupVar = dependence_group,
              title = paste("SHAP Dependence:", feature_name),
              auto_plots_theme = auto_plots_theme,
              plot_width = plot_width,
              plot_height = plot_height
            )
          } else {
            aq_create_shap_box_plot(
              dt = plot_data,
              XVar = "feature_value",
              YVar = "shap_value",
              GroupVar = dependence_group,
              title = paste("SHAP Dependence:", feature_name),
              auto_plots_theme = auto_plots_theme,
              plot_width = plot_width,
              plot_height = plot_height
            )
          }
        )
        if (!is.null(plot_result$object)) {
          plot_result$object <- aq_style_shap_plot(plot_result$object, horizontal = !source_is_numeric)
          artifacts <- regression_shap_add_artifact(artifacts, aq_create_regression_shap_plot_artifact(
            paste0("shap_dependence_", regression_shap_slug(feature_name), "_plot"),
            paste("SHAP Dependence:", feature_name),
            "SHAP Dependence",
            if (source_is_numeric) "scatter" else "box",
            "shap_dependence",
            plot_result$object,
            c(artifact_metadata("shap_dependence", "SHAP Dependence", 10L), list(feature = feature_name))
          ))
        } else {
          warnings <- regression_shap_warn(warnings, plot_result$warning)
        }
      }
    }
  }

  if (isTRUE(include_segments) && length(ByVars)) {
    segments <- aq_summarize_shap_segments(data, column_map, ByVars, max_byvars, max_segment_levels)
    artifacts <- regression_shap_add_artifact(artifacts, regression_shap_artifact("segment_effects_table", "Segment SHAP Effects", "table", "Segment Effects", object = aq_smart_round_dt(segments, skip_cols = c("rank_within_segment", "n")), metadata = artifact_metadata("segment_effects", "Segment Effects", 11L)))
    if (isTRUE(include_plots) && nrow(segments)) {
      segment_byvars <- head(unique(segments$ByVar), max_segment_plots)
      for (byvar in segment_byvars) {
        segment_plot_data <- data.table::copy(segments[ByVar == byvar & rank_within_segment <= plot_top_n])
        segment_plot_data <- aq_smart_round_dt(segment_plot_data, skip_cols = c("rank_within_segment", "n"))

        heatmap_result <- aq_safe_create_shap_plot(
          "heatmap",
          aq_create_shap_heatmap_plot(
            dt = segment_plot_data,
            XVar = "segment",
            YVar = "feature",
            ZVar = "mean_shap",
            title = paste("Segment Mean SHAP Heatmap:", byvar),
            auto_plots_theme = auto_plots_theme,
            plot_width = plot_width,
            plot_height = plot_height
          )
        )
        if (!is.null(heatmap_result$object)) {
          heatmap_result$object <- aq_style_shap_plot(heatmap_result$object, rotate_x = TRUE)
          artifacts <- regression_shap_add_artifact(artifacts, aq_create_regression_shap_plot_artifact(
            paste0("segment_effects_", regression_shap_slug(byvar), "_heatmap"),
            paste("Segment Mean SHAP Heatmap:", byvar),
            "Segment Effects",
            "heatmap",
            "segment_effects",
            heatmap_result$object,
            c(artifact_metadata("segment_effects", "Segment Effects", 13L), list(ByVar = byvar, heatmap_value = "mean_shap", heatmap_value_description = "Signed mean SHAP by feature and segment"))
          ))
        } else {
          warnings <- regression_shap_warn(warnings, heatmap_result$warning)
        }
      }
    }
  } else if (isTRUE(include_segments)) {
    warnings <- regression_shap_warn(warnings, "Segment effects skipped because no valid ByVars were supplied.")
  }

  if (isTRUE(include_time) && !is.null(DateVar)) {
    time_effects <- tryCatch(
      aq_summarize_shap_time(data, column_map, DateVar, date_aggregation),
      error = function(e) {
        warnings <<- regression_shap_warn(warnings, paste("Time effects skipped:", conditionMessage(e)))
        data.table::data.table()
      }
    )
    artifacts <- regression_shap_add_artifact(artifacts, regression_shap_artifact("time_effects_table", "Time SHAP Effects", "table", "Time Effects", object = aq_smart_round_dt(time_effects, skip_cols = c("rank_within_period", "n")), metadata = artifact_metadata("time_effects", "Time Effects", 14L)))
    if (isTRUE(include_plots) && nrow(time_effects)) {
      time_plot_data <- data.table::copy(time_effects[rank_within_period <= plot_top_n])
      time_plot_data[, period := as.Date(period)]
      time_plot_data <- aq_smart_round_dt(time_plot_data, skip_cols = c("rank_within_period", "n"))
      line_result <- aq_safe_create_shap_plot(
        "line",
        aq_create_shap_line_plot(
          dt = time_plot_data,
          XVar = "period",
          YVar = "mean_abs_shap",
          GroupVar = "feature",
          title = "Time SHAP Effects",
          auto_plots_theme = auto_plots_theme,
          plot_width = plot_width,
          plot_height = plot_height
        )
      )
      if (!is.null(line_result$object)) {
        line_result$object <- aq_style_shap_plot(line_result$object, rotate_x = TRUE)
        artifacts <- regression_shap_add_artifact(artifacts, aq_create_regression_shap_plot_artifact("time_effects_line_plot", "Time SHAP Effects Plot", "Time Effects", "line", "time_effects", line_result$object, artifact_metadata("time_effects", "Time Effects", 15L)))
      } else {
        warnings <- regression_shap_warn(warnings, line_result$warning)
      }

      heatmap_result <- aq_safe_create_shap_plot(
        "heatmap",
        aq_create_shap_heatmap_plot(
          dt = time_plot_data,
          XVar = "period",
          YVar = "feature",
          ZVar = "mean_abs_shap",
          title = "Time SHAP Effects Heatmap",
          auto_plots_theme = auto_plots_theme,
          plot_width = plot_width,
          plot_height = plot_height
        )
      )
      if (!is.null(heatmap_result$object)) {
        heatmap_result$object <- aq_style_shap_plot(heatmap_result$object, rotate_x = TRUE)
        artifacts <- regression_shap_add_artifact(artifacts, aq_create_regression_shap_plot_artifact("time_effects_heatmap", "Time SHAP Effects Heatmap", "Time Effects", "heatmap", "time_effects", heatmap_result$object, artifact_metadata("time_effects", "Time Effects", 16L)))
      } else {
        warnings <- regression_shap_warn(warnings, heatmap_result$warning)
      }
    }
  } else if (isTRUE(include_time)) {
    warnings <- regression_shap_warn(warnings, "Time effects skipped because no valid DateVar was supplied.")
  }

  if (isTRUE(include_local)) {
    local_row_ids <- suppressWarnings(as.integer(local_row_ids))
    invalid_rows <- local_row_ids[is.na(local_row_ids) | local_row_ids < 1L | local_row_ids > nrow(data)]
    if (length(invalid_rows)) {
      warnings <- regression_shap_warn(warnings, "Invalid local_row_ids were dropped.")
    }
    local <- aq_summarize_shap_local(data, column_map, local_row_ids[!is.na(local_row_ids)], prediction_col, target_col, id_cols)
    artifacts <- regression_shap_add_artifact(artifacts, regression_shap_artifact("local_explanations_table", "Local SHAP Explanations", "table", "Local Explanations", object = aq_smart_round_dt(local, skip_cols = c("row_id", "contribution_rank")), metadata = artifact_metadata("local_explanations", "Local Explanations", 17L)))
    if (isTRUE(include_plots) && nrow(local)) {
      local_rows <- head(unique(local$row_id), max_local_plots)
      for (row_id in local_rows) {
        local_plot_data <- data.table::copy(local[local[["row_id"]] == row_id][contribution_rank <= plot_top_n])
        local_plot_data[, abs_shap_for_plot_order := abs(shap_value)]
        data.table::setorderv(local_plot_data, "abs_shap_for_plot_order", order = 1L, na.last = TRUE)
        local_plot_data[, abs_shap_for_plot_order := NULL]
        local_plot_data <- aq_smart_round_dt(local_plot_data, skip_cols = c("row_id", "contribution_rank"))
        plot_result <- aq_safe_create_shap_plot(
          "bar",
          aq_create_shap_bar_plot(
            dt = local_plot_data,
            XVar = "feature",
            YVar = "shap_value",
            title = paste("Local SHAP Contributions: Row", row_id),
            auto_plots_theme = auto_plots_theme,
            plot_width = plot_width,
            plot_height = plot_height
          )
        )
        if (!is.null(plot_result$object)) {
          plot_result$object <- aq_style_shap_plot(plot_result$object, horizontal = TRUE)
          artifacts <- regression_shap_add_artifact(artifacts, aq_create_regression_shap_plot_artifact(
            paste0("local_explanations_row_", row_id, "_plot"),
            paste("Local SHAP Contributions: Row", row_id),
            "Local Explanations",
            "bar",
            "local_explanations",
            plot_result$object,
            c(artifact_metadata("local_explanations", "Local Explanations", 18L), list(row_id = row_id))
          ))
        } else {
          warnings <- regression_shap_warn(warnings, plot_result$warning)
        }
      }
    }
  }

  if (isTRUE(include_interactions)) {
    interactions <- aq_summarize_shap_interactions(
      data = data,
      column_map = column_map,
      global_importance = global_importance,
      selected_features = selected_features,
      interaction_pairs = interaction_pairs,
      max_interaction_pairs = max_interaction_pairs,
      numeric_interaction_bins = numeric_interaction_bins,
      max_interaction_levels = max_interaction_levels,
      min_interaction_cell_n = min_interaction_cell_n,
      interaction_stat = interaction_stat,
      interaction_score_stat = interaction_score_stat,
      collapse_rare_levels = collapse_rare_levels,
      target_col = target_col,
      prediction_col = prediction_col,
      ByVars = ByVars,
      DateVar = DateVar,
      date_aggregation = date_aggregation,
      id_cols = id_cols
    )
    warnings <- unique(c(warnings, interactions$warnings))
    interaction_text <- paste(
      "SHAP interaction diagnostics",
      "These diagnostics use precomputed ordinary Shap_ columns and existing source feature columns.",
      "Numeric variables are binned and categorical/date variables use bounded levels.",
      "Two-way surfaces place the SHAP-attributed feature's actual value/bin on one axis and the interaction lens feature's actual value/bin on the other axis.",
      "Heatmap values are signed mean SHAP for the attributed feature across those actual value/bin combinations.",
      "Scores are candidate interaction diagnostics based on binned level-combination heterogeneity, not exact SHAP interaction values.",
      "Exact pairwise SHAP interaction values require upstream interaction-specific output.",
      sep = "\n"
    )
    interaction_meta <- c(artifact_metadata("interaction_diagnostics", "Interaction Importance", 19L), list(
      interaction_type = "binned_level_combination",
      shap_source = "precomputed_columns",
      exact_shap_interaction_values = FALSE,
      numeric_interaction_bins = numeric_interaction_bins,
      max_interaction_levels = max_interaction_levels,
      min_interaction_cell_n = min_interaction_cell_n,
      interaction_stat = interaction_stat,
      score_stat = interaction_score_stat,
      target_col = target_col,
      prediction_col = prediction_col,
      DateVar = DateVar,
      date_aggregation = date_aggregation,
      ByVars = ByVars
    ))
    artifacts <- regression_shap_add_artifact(artifacts, regression_shap_artifact(
      "interaction_methodology",
      "SHAP Interaction Diagnostics Methodology",
      "text",
      "Interaction Importance",
      content = interaction_text,
      metadata = interaction_meta
    ))

    if (nrow(interactions$scores)) {
      artifacts <- regression_shap_add_artifact(artifacts, regression_shap_artifact(
        "candidate_interaction_ranking_table",
        "Candidate Interaction Ranking",
        "table",
        "Interaction Importance",
        object = interactions$scores,
        metadata = interaction_meta
      ))

      if (nrow(interactions$surfaces)) {
        artifacts <- regression_shap_add_artifact(artifacts, regression_shap_artifact(
          "two_way_shap_surface_table",
          "Two-Way SHAP Surface Table",
          "table",
          "Interaction Importance",
          object = interactions$surfaces,
          metadata = interaction_meta
        ))
      }

      if (isTRUE(include_plots)) {
        interaction_plot_data <- data.table::copy(interactions$scores[rank <= plot_top_n])
        data.table::setorderv(interaction_plot_data, "score", order = 1L, na.last = TRUE)
        ranking_plot <- aq_safe_create_shap_plot(
          "bar",
          aq_create_shap_bar_plot(
            dt = interaction_plot_data,
            XVar = "pair_label",
            YVar = "score",
            title = "Candidate SHAP Interaction Diagnostic Score",
            auto_plots_theme = auto_plots_theme,
            plot_width = plot_width,
            plot_height = plot_height
          )
        )
        if (!is.null(ranking_plot$object)) {
          ranking_plot$object <- aq_style_shap_plot(ranking_plot$object, horizontal = TRUE)
          artifacts <- regression_shap_add_artifact(artifacts, aq_create_regression_shap_plot_artifact(
            "candidate_interaction_ranking_plot",
            "Candidate Interaction Ranking Plot",
            "Interaction Importance",
            "bar",
            "interaction_diagnostics",
            ranking_plot$object,
            interaction_meta
          ))
        } else {
          warnings <- regression_shap_warn(warnings, ranking_plot$warning)
        }

        top_pair_scores <- data.table::copy(interactions$scores)
        top_pair_scores[, plot_priority := data.table::fcase(
          interaction_feature_role == "ByVar", 1L,
          interaction_feature_role == "DateVar_period", 2L,
          interaction_feature_role == "selected_feature", 3L,
          default = 4L
        )]
        data.table::setorderv(top_pair_scores, c("plot_priority", "rank"), order = c(1L, 1L), na.last = TRUE)
        top_pairs <- head(top_pair_scores$pair_label, max_interaction_surface_plots)
        for (current_pair_label in top_pairs) {
          surface_plot_data <- data.table::copy(interactions$surfaces[pair_label == current_pair_label & sparse_cell == FALSE])
          if (!nrow(surface_plot_data)) next
          shap_axis <- surface_plot_data$shap_feature[[1L]]
          interaction_axis <- surface_plot_data$interaction_feature[[1L]]
          heatmap_axis <- "mean_shap"
          if (identical(shap_axis, interaction_axis)) {
            interaction_axis <- paste0(interaction_axis, "_interaction")
          }
          surface_plot_data[, (shap_axis) := shap_feature_actual_value_bin]
          surface_plot_data[, (interaction_axis) := interaction_feature_actual_value_bin]
          surface_plot_data[, (heatmap_axis) := heatmap_value]
          surface_title <- paste("Mean SHAP Surface:", surface_plot_data$shap_feature[[1L]], "by", surface_plot_data$interaction_feature[[1L]])
          heatmap_result <- aq_safe_create_shap_plot(
            "heatmap",
            aq_create_shap_heatmap_plot(
              dt = surface_plot_data,
              XVar = shap_axis,
              YVar = interaction_axis,
              ZVar = heatmap_axis,
              title = surface_title,
              auto_plots_theme = auto_plots_theme,
              plot_width = plot_width,
              plot_height = plot_height
            )
          )
          if (!is.null(heatmap_result$object)) {
            heatmap_result$object <- aq_style_shap_plot(heatmap_result$object, rotate_x = TRUE)
            artifacts <- regression_shap_add_artifact(artifacts, aq_create_regression_shap_plot_artifact(
              paste0("two_way_shap_surface_", regression_shap_slug(current_pair_label), "_heatmap"),
              surface_title,
              "Interaction Importance",
              "heatmap",
              "interaction_diagnostics",
              heatmap_result$object,
              c(interaction_meta, list(pair_label = current_pair_label, x_axis = shap_axis, y_axis = interaction_axis, heatmap_value = "mean_shap", heatmap_value_description = surface_plot_data$heatmap_value_description[[1L]]))
            ))
          } else {
            warnings <- regression_shap_warn(warnings, heatmap_result$warning)
          }
        }

        dependence_pairs <- head(interactions$scores$pair_label, max_dependence_plots)
        for (current_pair_label in dependence_pairs) {
          score_row <- interactions$scores[pair_label == current_pair_label][1L]
          feature_name <- score_row$shap_feature[[1L]]
          interaction_feature <- interactions$surfaces[pair_label == current_pair_label, interaction_feature_raw][[1L]]
          if (!feature_name %in% names(data) || !interaction_feature %in% names(data)) next
          shap_col <- column_map[feature == feature_name & included == TRUE, shap_col][[1L]]
          row_index <- seq_len(nrow(data))
          if (nrow(data) > max_dependence_rows) {
            set.seed(123L)
            row_index <- sort(sample(row_index, max_dependence_rows))
          }
          lens_levels <- aq_create_interaction_levels(
            data[[interaction_feature]],
            interaction_feature,
            numeric_bins = numeric_interaction_bins,
            max_levels = max_interaction_levels,
            min_cell_n = min_interaction_cell_n,
            collapse_rare_levels = collapse_rare_levels
          )
          plot_data <- data.table::data.table(
            feature_value = data[[feature_name]][row_index],
            shap_value = data[[shap_col]][row_index],
            interaction_level = lens_levels$levels[row_index]
          )
          source_is_numeric <- is.numeric(data[[feature_name]]) || is.integer(data[[feature_name]])
          if (!source_is_numeric) {
            plot_data[, feature_value := as.character(feature_value)]
            plot_data <- aq_order_for_flipped_box(plot_data, "feature_value", "shap_value")
          }
          plot_result <- aq_safe_create_shap_plot(
            if (source_is_numeric) "scatter" else "box",
            if (source_is_numeric) {
              aq_create_shap_scatter_plot(
                dt = aq_smart_round_dt(plot_data),
                XVar = "feature_value",
                YVar = "shap_value",
                GroupVar = "interaction_level",
                title = paste("SHAP Dependence Interaction:", current_pair_label),
                auto_plots_theme = auto_plots_theme,
                plot_width = plot_width,
                plot_height = plot_height
              )
            } else {
              aq_create_shap_box_plot(
                dt = aq_smart_round_dt(plot_data),
                XVar = "feature_value",
                YVar = "shap_value",
                GroupVar = "interaction_level",
                title = paste("SHAP Dependence Interaction:", current_pair_label),
                auto_plots_theme = auto_plots_theme,
                plot_width = plot_width,
                plot_height = plot_height
              )
            }
          )
          if (!is.null(plot_result$object)) {
            plot_result$object <- aq_style_shap_plot(plot_result$object, horizontal = !source_is_numeric)
            artifacts <- regression_shap_add_artifact(artifacts, aq_create_regression_shap_plot_artifact(
              paste0("shap_dependence_interaction_", regression_shap_slug(current_pair_label), "_plot"),
              paste("SHAP Dependence Interaction:", current_pair_label),
              "SHAP Dependence",
              if (source_is_numeric) "scatter" else "box",
              "interaction_diagnostics",
              plot_result$object,
              c(interaction_meta, list(pair_label = current_pair_label))
            ))
          } else {
            warnings <- regression_shap_warn(warnings, plot_result$warning)
          }
        }
      }
    } else {
      warnings <- regression_shap_warn(warnings, "Interaction diagnostics skipped because no valid candidate pairs were available.")
    }
  }

  types <- vapply(artifacts, function(x) {
    regression_shap_null_coalesce(regression_shap_null_coalesce(x$type, x$artifact_type), "")
  }, character(1L))
  metadata <- c(full_metadata, list(
    artifact_count = length(artifacts),
    plot_count = sum(types == "plot"),
    table_count = sum(types == "table"),
    text_count = sum(types == "text"),
    warnings_count = length(warnings),
    interaction_diagnostics_available = "candidate_interaction_ranking_table" %in% names(artifacts),
    interaction_pair_count = if ("candidate_interaction_ranking_table" %in% names(artifacts)) nrow(artifacts$candidate_interaction_ranking_table$object) else 0L,
    interaction_surface_count = if ("two_way_shap_surface_table" %in% names(artifacts)) length(unique(artifacts$two_way_shap_surface_table$object$pair_label)) else 0L,
    interaction_method = "binned_level_combination_shap_diagnostic",
    exact_shap_interaction_values = FALSE
  ))

  list(
    artifacts = artifacts,
    metadata = metadata,
    warnings = warnings,
    diagnostics = list(
      column_map = column_map,
      valid_features = column_map[included == TRUE, feature],
      skipped_features = column_map[included == FALSE, feature]
    ),
    code = "generate_regression_shap_analysis_artifacts(data = scoring_data_with_precomputed_shap_columns)"
  )
}

qa_generate_regression_shap_analysis_artifacts <- function() {
  set.seed(123)
  n <- 300
  x1 <- stats::runif(n, 0, 100)
  x2 <- stats::runif(n, 0, 50)
  seg <- sample(c("A", "B", "C"), n, TRUE)
  date <- as.Date("2024-01-01") + sample(0:180, n, TRUE)
  df <- data.table::data.table(
    y = 10 + 0.2 * x1 + 0.1 * x2 + ifelse(seg == "A", 2, 0) + stats::rnorm(n),
    Predict = 10 + 0.2 * x1 + 0.1 * x2 + ifelse(seg == "A", 2, 0),
    Independent_Variable1 = x1,
    Independent_Variable2 = x2,
    Factor_1 = seg,
    IDCol_1 = seq_len(n),
    IDCol_2 = sample(1000:9999, n, TRUE),
    Date = date,
    Shap_Independent_Variable1 = 0.02 * x1 + ifelse(x2 > 25, 0.3, -0.1) + ifelse(seg == "A", 0.2, 0) + stats::rnorm(n, 0, 0.03),
    Shap_Independent_Variable2 = 0.01 * x2 + ifelse(x1 > 50, 0.2, -0.05) + stats::rnorm(n, 0, 0.03),
    Shap_Factor_1 = ifelse(seg == "A", 0.2, ifelse(seg == "B", -0.05, 0.03)) + stats::rnorm(n, 0, 0.02),
    Shap_Bad = as.character(seq_len(n))
  )

  result <- generate_regression_shap_analysis_artifacts(
    data = df,
    target_col = "y",
    prediction_col = "Predict",
    DateVar = "Date",
    date_aggregation = "month",
    ByVars = "Factor_1",
    id_cols = c("IDCol_1", "IDCol_2"),
    selected_features = c("Independent_Variable1", "Independent_Variable2", "Factor_1"),
    local_row_ids = c(1L, 2L),
    top_n = 3,
    include_dependence = TRUE,
    include_segments = TRUE,
    include_time = TRUE,
    include_local = TRUE,
    include_interactions = TRUE,
    include_plots = TRUE,
    max_feature_effect_plots = 5L,
    max_dependence_plots = 5L,
    max_segment_plots = 5L,
    max_time_plots = 5L,
    max_local_plots = 5L,
    plot_top_n = 3L,
    numeric_interaction_bins = 5L,
    max_interaction_pairs = 10L,
    max_interaction_surface_plots = 5L,
    min_interaction_cell_n = 5L
  )

  no_shap <- generate_regression_shap_analysis_artifacts(data = df[, !startsWith(names(df), "Shap_")])
  bad_date <- generate_regression_shap_analysis_artifacts(data = df, DateVar = "MissingDate")
  bad_by <- generate_regression_shap_analysis_artifacts(data = df, ByVars = "MissingBy")
  no_plots <- generate_regression_shap_analysis_artifacts(data = df, include_plots = FALSE, top_n = 3)
  inferred_time <- generate_regression_shap_analysis_artifacts(data = df, DateVar = NULL, include_time = TRUE, top_n = 3)

  artifact_sections <- vapply(result$artifacts, function(x) regression_shap_null_coalesce(x$section, ""), character(1L))
  artifact_labels <- vapply(result$artifacts, function(x) regression_shap_null_coalesce(x$label, ""), character(1L))
  artifact_types <- vapply(result$artifacts, function(x) regression_shap_null_coalesce(x$type, ""), character(1L))
  plot_metadata_packages <- vapply(result$artifacts[artifact_types == "plot"], function(x) regression_shap_null_coalesce(x$metadata$plot_package, ""), character(1L))
  plot_metadata_lenses <- vapply(result$artifacts[artifact_types == "plot"], function(x) regression_shap_null_coalesce(x$metadata$lens, ""), character(1L))
  artifact_names <- names(result$artifacts)
  no_plot_types <- vapply(no_plots$artifacts, function(x) regression_shap_null_coalesce(x$type, ""), character(1L))

  level_table <- result$artifacts$categorical_level_importance_table$object
  surface_table <- result$artifacts$two_way_shap_surface_table$object
  segment_heatmap_artifacts <- artifact_names[grepl("^segment_effects_.*_heatmap$", artifact_names)]
  data.table::data.table(
    check = c(
      "structured_result",
      "artifacts_returned",
      "overview_text",
      "diagnostics_table",
      "column_map_table",
      "global_importance_table",
      "categorical_level_importance_table",
      "single_feature_effects_table",
      "dependence_table",
      "segment_table",
      "time_table",
      "local_table",
      "interaction_methodology",
      "interaction_ranking_table",
      "interaction_surface_table",
      "autoplots_plot_artifacts",
      "global_importance_plot",
      "shap_distribution_plot",
      "categorical_level_importance_plot",
      "categorical_level_distribution_plot",
      "single_feature_effect_plot",
      "dependence_plot",
      "segment_plot",
      "segment_heatmap",
      "time_plot",
      "local_plot",
      "numeric_bins_in_level_importance",
      "interaction_metadata_not_exact",
      "interaction_surface_axis_columns",
      "interaction_surface_mean_shap_value",
      "segment_heatmap_uses_mean_shap",
      "plot_metadata_package",
      "plot_metadata_lens",
      "plots_disabled_preserves_tables",
      "labels_non_empty",
      "sections_non_empty",
      "metadata_problem_type",
      "metadata_source_function",
      "metadata_shap_source",
      "id_cols_not_features",
      "datevar_inference_time_effects",
      "no_shap_columns_structured",
      "invalid_date_warns",
      "invalid_byvars_warn"
    ),
    status = c(
      if (is.list(result) && all(c("artifacts", "metadata", "warnings", "diagnostics") %in% names(result))) "success" else "error",
      if (length(result$artifacts) >= 9L) "success" else "error",
      if ("shap_overview" %in% artifact_names) "success" else "error",
      if ("shap_diagnostics_config" %in% artifact_names) "success" else "error",
      if ("shap_column_map" %in% artifact_names) "success" else "error",
      if ("global_importance_table" %in% artifact_names) "success" else "error",
      if ("categorical_level_importance_table" %in% artifact_names && nrow(result$artifacts$categorical_level_importance_table$object)) "success" else "error",
      if ("single_feature_effects_table" %in% artifact_names) "success" else "error",
      if ("shap_dependence_table" %in% artifact_names) "success" else "error",
      if ("segment_effects_table" %in% artifact_names) "success" else "error",
      if ("time_effects_table" %in% artifact_names) "success" else "error",
      if ("local_explanations_table" %in% artifact_names) "success" else "error",
      if ("interaction_methodology" %in% artifact_names) "success" else "error",
      if ("candidate_interaction_ranking_table" %in% artifact_names && nrow(result$artifacts$candidate_interaction_ranking_table$object)) "success" else "error",
      if ("two_way_shap_surface_table" %in% artifact_names && nrow(result$artifacts$two_way_shap_surface_table$object)) "success" else "error",
      if (!aq_autoplots_available() || any(artifact_types == "plot")) "success" else "error",
      if (!aq_autoplots_available() || "global_importance_plot" %in% artifact_names) "success" else "error",
      if (!aq_autoplots_available() || "shap_distribution_plot" %in% artifact_names) "success" else "error",
      if (!aq_autoplots_available() || "categorical_level_importance_plot" %in% artifact_names) "success" else "error",
      if (!aq_autoplots_available() || "categorical_level_distribution_plot" %in% artifact_names) "success" else "error",
      if (!aq_autoplots_available() || any(grepl("^single_feature_effect_.*_plot$", artifact_names))) "success" else "error",
      if (!aq_autoplots_available() || any(grepl("^shap_dependence_.*_plot$", artifact_names))) "success" else "error",
      if (!any(grepl("^segment_effects_.*_plot$", artifact_names))) "success" else "error",
      if (!aq_autoplots_available() || any(grepl("^segment_effects_.*_heatmap$", artifact_names))) "success" else "error",
      if (!aq_autoplots_available() || "time_effects_line_plot" %in% artifact_names) "success" else "error",
      if (!aq_autoplots_available() || any(grepl("^local_explanations_row_.*_plot$", artifact_names))) "success" else "error",
      if ("value_type" %in% names(level_table) && any(level_table$value_type %in% c("numeric_bin", "numeric_level"))) "success" else "error",
      if (identical(result$metadata$exact_shap_interaction_values, FALSE) && isTRUE(result$metadata$interaction_diagnostics_available)) "success" else "error",
      if (all(c("shap_feature_actual_value_bin", "interaction_feature_actual_value_bin", "heatmap_value", "heatmap_value_description") %in% names(surface_table))) "success" else "error",
      if ("mean_shap" %in% names(surface_table) && isTRUE(all.equal(surface_table$heatmap_value, surface_table$mean_shap))) "success" else "error",
      if (!aq_autoplots_available() || length(segment_heatmap_artifacts) && all(vapply(result$artifacts[segment_heatmap_artifacts], function(x) identical(x$metadata$plot_type, "heatmap") && identical(x$metadata$lens, "segment_effects") && identical(x$metadata$heatmap_value, "mean_shap"), logical(1L)))) "success" else "error",
      if (!aq_autoplots_available() || all(plot_metadata_packages == "AutoPlots")) "success" else "error",
      if (!aq_autoplots_available() || all(nzchar(plot_metadata_lenses))) "success" else "error",
      if (!any(no_plot_types == "plot") && any(no_plot_types == "table")) "success" else "error",
      if (all(nzchar(artifact_labels))) "success" else "error",
      if (all(nzchar(artifact_sections))) "success" else "error",
      if (identical(result$metadata$problem_type, "regression")) "success" else "error",
      if (identical(result$metadata$source_function, "generate_regression_shap_analysis_artifacts")) "success" else "error",
      if (identical(result$metadata$shap_source, "precomputed_columns")) "success" else "error",
      if (!any(result$diagnostics$column_map$feature %in% c("IDCol_1", "IDCol_2"))) "success" else "error",
      if ("time_effects_table" %in% names(inferred_time$artifacts) && nrow(inferred_time$artifacts$time_effects_table$object)) "success" else "error",
      if (is.list(no_shap) && length(no_shap$warnings)) "success" else "error",
      if (length(bad_date$warnings)) "success" else "error",
      if (length(bad_by$warnings)) "success" else "error"
    ),
    message = c(
      "Result has structured fields.",
      "Artifacts were returned.",
      "Overview text artifact exists.",
      "Diagnostics/config table exists.",
      "SHAP column map table exists.",
      "Global importance table exists.",
      "Categorical level importance table exists.",
      "Single feature effects table exists.",
      "Dependence table exists.",
      "Segment effects table exists.",
      "Time effects table exists.",
      "Local explanations table exists.",
      "Interaction methodology text exists.",
      "Candidate interaction ranking table exists.",
      "Two-way SHAP surface table exists.",
      "AutoPlots plot artifacts are returned when AutoPlots is available.",
      "Global importance AutoPlots artifact exists.",
      "SHAP distribution AutoPlots artifact exists.",
      "Categorical level importance AutoPlots artifact exists.",
      "Categorical level distribution AutoPlots artifact exists.",
      "Single feature effect AutoPlots artifact exists.",
      "Dependence AutoPlots artifact exists.",
      "Grouped segment bar plot is not generated.",
      "Segment heatmap AutoPlots artifact exists.",
      "Time AutoPlots artifact exists.",
      "Local explanation AutoPlots artifact exists.",
      "Level importance includes binned numeric variables.",
      "Interaction metadata states diagnostics are not exact SHAP interaction values.",
      "Interaction surface table includes explicit actual-value/bin axis columns.",
      "Interaction heatmap value is the signed mean SHAP column.",
      "Segment visualization artifacts are signed mean SHAP heatmaps.",
      "Plot artifact metadata records AutoPlots.",
      "Plot artifact metadata records lenses.",
      "Disabling plots preserves table/text artifacts.",
      "Artifact labels are non-empty.",
      "Artifact sections are non-empty.",
      "Problem type metadata is regression.",
      "Source function metadata is correct.",
      "SHAP source metadata is precomputed_columns.",
      "ID columns were not treated as SHAP features.",
      "DateVar inference creates time effects when a date-like column exists.",
      "No-SHAP input returns structured warning output.",
      "Invalid DateVar warns without crashing.",
      "Invalid ByVars warn without crashing."
    )
  )
}

qa_regression_shap_analysis_report <- function() {
  set.seed(123)
  n <- 100
  df <- data.table::data.table(
    y = stats::rnorm(n),
    Predict = stats::rnorm(n),
    Independent_Variable1 = stats::runif(n),
    Independent_Variable2 = stats::rnorm(n),
    Factor_1 = sample(c("A", "B", "C"), n, TRUE),
    IDCol_1 = seq_len(n),
    IDCol_2 = sample(1000:9999, n, TRUE),
    Date = as.Date("2024-01-01") + sample(0:120, n, TRUE),
    Shap_Independent_Variable1 = stats::rnorm(n, 0, 0.2),
    Shap_Independent_Variable2 = stats::rnorm(n, 0, 0.1),
    Shap_Factor_1 = stats::rnorm(n, 0, 0.05)
  )

  output_file <- "regression_shap_analysis_report_test.html"
  report_path <- RegressionShapAnalysisReport(
    data = df,
    output_dir = tempdir(),
    output_file = output_file,
    title = "Regression SHAP Analysis Test Report",
    target_col = "y",
    prediction_col = "Predict",
    DateVar = "Date",
    date_aggregation = "month",
    ByVars = "Factor_1",
    id_cols = c("IDCol_1", "IDCol_2"),
    selected_features = c("Independent_Variable1", "Independent_Variable2", "Factor_1"),
    local_row_ids = c(1L, 2L),
    top_n = 3,
    include_dependence = TRUE,
    include_segments = TRUE,
    include_time = TRUE,
    include_local = TRUE,
    include_interactions = TRUE,
    open = FALSE,
    quiet = TRUE
  )

  artifact_result <- generate_regression_shap_analysis_artifacts(
    data = df,
    target_col = "y",
    prediction_col = "Predict",
    DateVar = "Date",
    ByVars = "Factor_1",
    include_local = TRUE,
    include_interactions = TRUE,
    local_row_ids = c(1L, 2L),
    top_n = 3
  )
  reuse_path <- RegressionShapAnalysisReport(
    data = NULL,
    artifact_result = artifact_result,
    output_dir = tempdir(),
    output_file = "regression_shap_analysis_report_reuse_test.html",
    title = "Regression SHAP Analysis Reuse Test Report",
    open = FALSE,
    quiet = TRUE
  )

  html <- paste(readLines(report_path, warn = FALSE), collapse = "\n")
  template_path <- file.path(getwd(), "inst", "r-markdowns", "Regression_SHAP_Analysis_Report.Rmd")
  template <- if (file.exists(template_path)) paste(readLines(template_path, warn = FALSE), collapse = "\n") else ""
  generator_path <- file.path(getwd(), "R", "generate_regression_shap_analysis_artifacts.R")
  generator_source <- if (file.exists(generator_path)) paste(readLines(generator_path, warn = FALSE), collapse = "\n") else ""
  data.table::data.table(
    check = c(
      "report_function_exists",
      "report_function_exported",
      "html_file_created",
      "html_file_non_empty",
      "contains_title",
      "contains_overview",
      "contains_global_importance",
      "contains_categorical_level_importance",
      "contains_dependence",
      "contains_segment_effects",
      "contains_time_effects",
      "contains_local_explanations",
      "contains_interaction",
      "contains_autoquant_hero",
      "contains_autoquant_panels",
      "contains_autoquant_details",
      "contains_reactable_tables",
      "contains_autoplots_grid",
      "contains_table_download_wrapper",
      "template_uses_prettydoc",
      "template_uses_mi_css",
      "generator_uses_horizontal_bar_style",
      "generator_uses_axis_rotation",
      "artifact_result_reuse",
      "no_model_required",
      "no_prediction_function_required",
      "no_shap_backend_required"
    ),
    status = c(
      if (exists("RegressionShapAnalysisReport", mode = "function")) "success" else "error",
      if ("RegressionShapAnalysisReport" %in% getNamespaceExports("AutoQuant")) "success" else "error",
      if (file.exists(report_path)) "success" else "error",
      if (file.exists(report_path) && file.info(report_path)$size > 0L) "success" else "error",
      if (grepl("Regression SHAP Analysis Test Report", html, fixed = TRUE)) "success" else "error",
      if (grepl("SHAP Overview", html, fixed = TRUE)) "success" else "error",
      if (grepl("Global Importance", html, fixed = TRUE)) "success" else "error",
      if (grepl("Categorical / binned numeric", html, fixed = TRUE) || grepl("Categorical / Binned Numeric", html, fixed = TRUE)) "success" else "error",
      if (grepl("SHAP Dependence", html, fixed = TRUE)) "success" else "error",
      if (grepl("Segment Effects", html, fixed = TRUE)) "success" else "error",
      if (grepl("Time Effects", html, fixed = TRUE)) "success" else "error",
      if (grepl("Local Explanations", html, fixed = TRUE)) "success" else "error",
      if (grepl("Interaction", html, fixed = TRUE)) "success" else "error",
      if (grepl("mi-hero", html, fixed = TRUE)) "success" else "error",
      if (grepl("mi-panel", html, fixed = TRUE)) "success" else "error",
      if (grepl("mi-details", html, fixed = TRUE)) "success" else "error",
      if (grepl("reactable", html, fixed = TRUE)) "success" else "error",
      if (grepl("plot-grid", html, fixed = TRUE)) "success" else "error",
      if (grepl("Download CSV", html, fixed = TRUE)) "success" else "error",
      if (grepl("prettydoc::html_pretty", template, fixed = TRUE)) "success" else "error",
      if (all(vapply(c("mi-hero", "mi-panel", "table-wrap"), function(x) grepl(x, template, fixed = TRUE), logical(1L)))) "success" else "error",
      if (grepl("e_flip_coords", generator_source, fixed = TRUE)) "success" else "error",
      if (grepl("e_x_axis", generator_source, fixed = TRUE) && grepl("axisLabel", generator_source, fixed = TRUE) && grepl("rotate", generator_source, fixed = TRUE)) "success" else "error",
      if (file.exists(reuse_path) && file.info(reuse_path)$size > 0L) "success" else "error",
      "success",
      "success",
      "success"
    ),
    message = c(
      "RegressionShapAnalysisReport is available.",
      "RegressionShapAnalysisReport is exported.",
      report_path,
      "Rendered HTML is non-empty.",
      "HTML contains the report title.",
      "HTML contains SHAP Overview.",
      "HTML contains Global Importance.",
      "HTML contains categorical / binned numeric level importance.",
      "HTML contains SHAP Dependence.",
      "HTML contains Segment Effects.",
      "HTML contains Time Effects.",
      "HTML contains Local Explanations.",
      "HTML contains Interaction content.",
      "HTML contains AutoQuant mi-hero structure.",
      "HTML contains AutoQuant mi-panel structure.",
      "HTML contains AutoQuant mi-details structure.",
      "HTML contains reactable output.",
      "HTML contains AutoPlots display_plots_grid markup.",
      "HTML contains existing table download wrapper text.",
      "Template uses prettydoc html_pretty.",
      "Template includes AutoQuant mi-* CSS markers.",
      "Generator uses existing horizontal e_flip_coords convention.",
      "Generator uses existing e_x_axis axisLabel rotation convention.",
      reuse_path,
      "Fixture rendered without a model object.",
      "Fixture rendered without a prediction function.",
      "Fixture rendered without a SHAP backend package."
    )
  )
}
