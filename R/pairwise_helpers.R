#' Create a Canonical Unordered Pair Key
#'
#' Creates a stable key for unordered feature pairs so A/B and B/A map to the
#' same pair unless a caller explicitly opts into directional behavior.
#'
#' @param a,b Feature names or scalar values.
#' @param sep Separator used in the pair key.
#'
#' @return A scalar character key.
#'
#' @keywords internal
aq_canonical_pair_key <- function(a, b, sep = " x ") {
  a <- as.character(a)[1L]
  b <- as.character(b)[1L]
  if (is.na(a) || is.na(b)) {
    return(NA_character_)
  }

  paste(sort(c(a, b)), collapse = sep)
}

#' Add a Canonical Pair Key Column
#'
#' Adds a canonical unordered pair key column to pairwise diagnostic tables.
#'
#' @param data A table-like object.
#' @param feature_a_col,feature_b_col Column names containing feature pairs.
#' @param pair_col Name of the output pair-key column.
#'
#' @return A data.table with the pair-key column added.
#'
#' @keywords internal
aq_canonical_pair_table <- function(data, feature_a_col, feature_b_col, pair_col = "pair_key") {
  data <- data.table::as.data.table(data.table::copy(data))
  if (!all(c(feature_a_col, feature_b_col) %in% names(data))) {
    stop("feature_a_col and feature_b_col must exist in data.", call. = FALSE)
  }

  data[, (pair_col) := mapply(
    aq_canonical_pair_key,
    get(feature_a_col),
    get(feature_b_col),
    USE.NAMES = FALSE
  )]
  data[]
}

#' Deduplicate Unordered Pair Rows
#'
#' Removes mirrored pair rows such as A/B and B/A within the same analytical
#' view, while preserving directional analyses when requested.
#'
#' @param data A table-like object.
#' @param feature_a_col,feature_b_col Column names containing feature pairs.
#' @param extra_key_cols Additional columns that define separate analytical
#'   views, such as metric, section, or plot type.
#' @param pair_col Name of the canonical pair-key column.
#' @param directional If TRUE, preserve A/B and B/A as distinct rows.
#' @param allow_self_pairs If TRUE, keep A/A rows.
#'
#' @return A deduplicated data.table.
#'
#' @keywords internal
aq_dedupe_unordered_pairs <- function(
  data,
  feature_a_col,
  feature_b_col,
  extra_key_cols = character(),
  pair_col = "pair_key",
  directional = FALSE,
  allow_self_pairs = FALSE
) {
  data <- aq_canonical_pair_table(data, feature_a_col, feature_b_col, pair_col = pair_col)
  if (!isTRUE(allow_self_pairs)) {
    data <- data[as.character(get(feature_a_col)) != as.character(get(feature_b_col))]
  }
  if (!nrow(data)) {
    return(data)
  }

  key_cols <- if (isTRUE(directional)) {
    c(feature_a_col, feature_b_col, extra_key_cols)
  } else {
    c(pair_col, extra_key_cols)
  }
  key_cols <- unique(key_cols[key_cols %in% names(data)])
  data <- data[!duplicated(data[, ..key_cols])]
  data[]
}

#' QA Unordered Pair Helpers
#'
#' Verifies that shared pair helpers canonicalize and deduplicate unordered
#' feature pairs while preserving explicit directional escape hatches.
#'
#' @return A data.table of QA checks.
#'
#' @family QA
#' @export
qa_unordered_pair_key_helpers <- function() {
  pairs <- data.table::data.table(
    feature_a = c("A", "B", "A", "C", "A"),
    feature_b = c("B", "A", "A", "D", "B"),
    view = c("heatmap", "heatmap", "heatmap", "heatmap", "table")
  )
  keyed <- aq_canonical_pair_table(pairs, "feature_a", "feature_b", "pair_key")
  deduped <- aq_dedupe_unordered_pairs(pairs, "feature_a", "feature_b", extra_key_cols = "view")
  directional <- aq_dedupe_unordered_pairs(pairs, "feature_a", "feature_b", directional = TRUE, allow_self_pairs = TRUE)

  data.table::data.table(
    check = c(
      "key_symmetric",
      "self_pair_key",
      "dedupe_drops_mirror",
      "extra_key_preserves_views",
      "directional_escape_hatch"
    ),
    status = c(
      if (identical(aq_canonical_pair_key("A", "B"), aq_canonical_pair_key("B", "A"))) "success" else "error",
      if (identical(keyed[feature_a == "A" & feature_b == "A", pair_key][[1L]], "A x A")) "success" else "error",
      if (nrow(deduped[pair_key == "A x B" & view == "heatmap"]) == 1L) "success" else "error",
      if (nrow(deduped[pair_key == "A x B"]) == 2L) "success" else "error",
      if (nrow(directional[feature_a %in% c("A", "B") & feature_b %in% c("A", "B")]) >= 2L) "success" else "error"
    ),
    message = c(
      "A/B and B/A share the same canonical key.",
      "Self-pairs can be represented when explicitly allowed.",
      "Mirrored unordered pairs collapse within the same view.",
      "Extra key columns keep separate analytical views.",
      "Directional analyses can opt out explicitly."
    )
  )
}

aq_pairwise_duplicate_keys <- function(keys) {
  keys <- keys[!is.na(keys) & nzchar(keys)]
  unique(keys[duplicated(keys)])
}

aq_pairwise_default <- function(value, default) {
  if (is.null(value)) default else value
}

#' QA SHAP Interaction Pair Deduplication
#'
#' Verifies that regression and binary SHAP interaction outputs use canonical
#' unordered pair keys and do not emit mirrored duplicate artifacts.
#'
#' @return A data.table of QA checks.
#'
#' @family QA
#' @export
qa_shap_interaction_pair_deduplication <- function() {
  set.seed(202)
  n <- 160L
  base_dt <- data.table::data.table(
    Target = stats::rnorm(n),
    Predict = stats::rnorm(n),
    FeatureA = sample(c("Low", "Mid", "High"), n, TRUE),
    FeatureB = stats::runif(n, 0, 100),
    Segment = sample(c("S1", "S2"), n, TRUE)
  )
  base_dt[, Shap_FeatureA := fifelse(FeatureA == "High", 0.25, -0.05) + stats::rnorm(.N, 0, 0.02)]
  base_dt[, Shap_FeatureB := 0.01 * FeatureB + fifelse(FeatureA == "Low", -0.10, 0.08) + stats::rnorm(.N, 0, 0.02)]

  mirrored_pairs <- list(c("FeatureA", "FeatureB"), c("FeatureB", "FeatureA"))
  reg <- generate_regression_shap_analysis_artifacts(
    data = base_dt,
    target_col = "Target",
    prediction_col = "Predict",
    selected_features = c("FeatureA", "FeatureB"),
    interaction_pairs = mirrored_pairs,
    include_interactions = TRUE,
    include_plots = TRUE,
    max_interaction_pairs = 10L,
    max_interaction_surface_plots = 10L,
    min_interaction_cell_n = 1L,
    top_n = 2L
  )

  bin_dt <- data.table::copy(base_dt)
  bin_dt[, Target := sample(c("Yes", "No"), .N, TRUE)]
  bin_dt[, Predict := stats::runif(.N)]
  bin_dt[, PredictedClass := ifelse(Predict >= 0.5, "Yes", "No")]
  bin <- generate_binary_classification_shap_analysis_artifacts(
    data = bin_dt,
    target_col = "Target",
    prediction_col = "Predict",
    predicted_class_col = "PredictedClass",
    positive_class = "Yes",
    prediction_scale = "probability",
    threshold = 0.5,
    selected_features = c("FeatureA", "FeatureB"),
    include_interactions = TRUE,
    include_plots = TRUE,
    max_interaction_pairs = 10L,
    max_interaction_surface_plots = 10L,
    min_interaction_cell_n = 1L,
    top_n = 2L
  )

  reg_scores <- reg$artifacts$candidate_interaction_ranking_table$object
  reg_surfaces <- reg$artifacts$two_way_shap_surface_table$object
  bin_scores <- bin$artifacts$candidate_interaction_ranking_table$object
  bin_surfaces <- bin$artifacts$two_way_shap_surface_table$object
  reg_surface_orientations <- unique(reg_surfaces[, list(interaction_pair_key, shap_feature, interaction_feature)])
  bin_surface_orientations <- unique(bin_surfaces[, list(interaction_pair_key, shap_feature, interaction_feature)])

  reg_heatmaps <- reg$artifacts[grepl("^two_way_shap_surface_.*_heatmap$", names(reg$artifacts))]
  bin_heatmaps <- bin$artifacts[grepl("^two_way_shap_surface_.*_heatmap$", names(bin$artifacts))]
  reg_heatmap_keys <- vapply(reg_heatmaps, function(artifact) aq_pairwise_default(artifact$metadata$interaction_pair_key, NA_character_), character(1L))
  bin_heatmap_keys <- vapply(bin_heatmaps, function(artifact) aq_pairwise_default(artifact$metadata$interaction_pair_key, NA_character_), character(1L))

  dependence_heatmaps <- c(
    names(reg$artifacts)[vapply(reg$artifacts, function(artifact) identical(artifact$section, "SHAP Dependence") && identical(aq_pairwise_default(artifact$metadata$plot_type, ""), "heatmap"), logical(1L))],
    names(bin$artifacts)[vapply(bin$artifacts, function(artifact) identical(artifact$section, "SHAP Dependence") && identical(aq_pairwise_default(artifact$metadata$plot_type, ""), "heatmap"), logical(1L))]
  )

  data.table::data.table(
    check = c(
      "regression_score_pair_keys_unique",
      "regression_surface_pair_keys_unique",
      "regression_heatmap_pair_keys_unique",
      "binary_score_pair_keys_unique",
      "binary_surface_pair_keys_unique",
      "binary_heatmap_pair_keys_unique",
      "metadata_has_pair_fields",
      "dependence_contains_no_heatmaps",
      "interaction_heatmaps_in_interaction_importance",
      "artifact_ids_unique"
    ),
    status = c(
      if (!length(aq_pairwise_duplicate_keys(reg_scores$interaction_pair_key))) "success" else "error",
      if (!any(duplicated(reg_surface_orientations$interaction_pair_key))) "success" else "error",
      if (!length(aq_pairwise_duplicate_keys(reg_heatmap_keys))) "success" else "error",
      if (!length(aq_pairwise_duplicate_keys(bin_scores$interaction_pair_key))) "success" else "error",
      if (!any(duplicated(bin_surface_orientations$interaction_pair_key))) "success" else "error",
      if (!length(aq_pairwise_duplicate_keys(bin_heatmap_keys))) "success" else "error",
      if (all(c("feature_x", "feature_y", "interaction_pair_key", "directional") %in% names(reg_scores)) && all(!reg_scores$directional)) "success" else "error",
      if (!length(dependence_heatmaps)) "success" else "error",
      if (all(vapply(c(reg_heatmaps, bin_heatmaps), function(artifact) identical(artifact$section, "Interaction Importance"), logical(1L)))) "success" else "error",
      if (!any(duplicated(names(reg$artifacts))) && !any(duplicated(names(bin$artifacts)))) "success" else "error"
    ),
    message = c(
      "Regression interaction ranking has one unordered key per pair.",
      "Regression surface table has one orientation per unordered pair.",
      "Regression heatmap artifacts have unique unordered pair keys.",
      "Binary interaction ranking has one unordered key per pair.",
      "Binary surface table has one orientation per unordered pair.",
      "Binary heatmap artifacts have unique unordered pair keys.",
      "Interaction outputs include feature_x, feature_y, interaction_pair_key, and directional = FALSE.",
      "SHAP Dependence contains no interaction heatmaps.",
      "Interaction heatmaps are sectioned under Interaction Importance.",
      "Generated artifact IDs are unique within each result set."
    )
  )
}

#' QA Pairwise Report Artifact Uniqueness
#'
#' Runs the shared unordered-pair helper checks and the SHAP interaction
#' pair-deduplication checks as one compact QA result.
#'
#' @return A data.table of QA checks.
#'
#' @family QA
#' @export
qa_pairwise_report_artifact_uniqueness <- function() {
  shap_checks <- qa_shap_interaction_pair_deduplication()
  helper_checks <- qa_unordered_pair_key_helpers()
  data.table::rbindlist(
    list(
      data.table::data.table(scope = "pair_helpers", helper_checks),
      data.table::data.table(scope = "shap_interactions", shap_checks)
    ),
    use.names = TRUE,
    fill = TRUE
  )
}
