# Applicability-driven EDA communication envelope.
# Canonical analytical output for capability_communication_v1.
# HTML/RMarkdown is a later projection. Does not recompute SHAP/regression.

eda_coalesce <- function(x, y) {
  if (is.null(x) || !length(x) || (is.atomic(x) && length(x) == 1L && is.na(x)))
    y else x
}

eda_communication_envelope_schema <- function() "eda_communication_envelope_v1"

eda_envelope_avenues <- function() {
  c("schema_types", "cardinality", "grain_keys_duplicates", "missingness_structure",
    "distributions_tails_zeros", "rare_levels", "numeric_relationships",
    "nonlinear_categorical_numeric", "temporal_coverage", "periodicity_seasonality",
    "regime_change", "target_relationships", "leakage_post_treatment", "drift",
    "anomalies", "clustering_segmentation", "transformations", "unresolved_dq")
}

eda_envelope_item <- function(kind, statement, avenue = NA_character_,
    refs = list()) {
  list(kind = kind, statement = as.character(statement)[1L],
    avenue = avenue, refs = refs)
}

eda_envelope_sample <- function(data, max_rows = 50000L) {
  n <- nrow(data)
  if (n <= max_rows) return(list(data = data, sampled = FALSE, n = n))
  idx <- seq.int(1L, n, length.out = max_rows)
  list(data = data[as.integer(idx)], sampled = TRUE, n = n)
}

eda_envelope_col_kind <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("temporal")
  if (is.numeric(x) && !is.factor(x)) return("numeric")
  if (is.logical(x) || is.factor(x) || is.character(x)) return("categorical")
  "other"
}

#' Characterize Dataset archetypes that drive EDA applicability.
#' @export
eda_characterize_dataset <- function(data, target_var = NULL, date_var = NULL,
    entity_var = NULL) {
  data <- data.table::as.data.table(data)
  kinds <- vapply(data, eda_envelope_col_kind, character(1))
  n <- nrow(data)
  p <- ncol(data)
  miss <- mean(vapply(data, function(x) mean(is.na(x)), numeric(1)))
  if (!is.finite(miss)) miss <- 0
  n_num <- sum(kinds == "numeric")
  n_cat <- sum(kinds == "categorical")
  n_time <- sum(kinds == "temporal")
  date_var <- eda_coalesce(date_var, names(data)[kinds == "temporal"][1L])
  archetypes <- character()
  if (n_time > 0L) archetypes <- c(archetypes, "temporal")
  if (n_cat > 0L && n_cat / max(p, 1L) >= 0.45) archetypes <- c(archetypes,
    "categorical_heavy")
  if (n_num >= 20L || (p >= 40L)) archetypes <- c(archetypes, "high_dim_numeric")
  if (isTRUE(miss >= 0.15) || any(vapply(data, function(x) mean(is.na(x)),
    numeric(1)) >= 0.4)) archetypes <- c(archetypes, "missing_heavy")
  if (!is.null(target_var) && target_var %in% names(data))
    archetypes <- c(archetypes, "supervised_target")
  if (!is.null(entity_var) && entity_var %in% names(data) && n_time > 0L)
    archetypes <- c(archetypes, "panel_grouped")
  if (n_time > 0L && n >= 24L) archetypes <- c(archetypes, "regime_candidate")
  if (!length(archetypes)) archetypes <- "mixed_tabular"
  if (!"mixed_tabular" %in% archetypes && n_num > 0L && n_cat > 0L)
    archetypes <- c("mixed_tabular", archetypes)
  list(
    n_rows = n, n_cols = p, missing_rate = miss,
    n_numeric = n_num, n_categorical = n_cat, n_temporal = n_time,
    date_var = date_var, target_var = target_var, entity_var = entity_var,
    archetypes = unique(archetypes), column_kinds = kinds
  )
}

#' Decide which EDA avenues to run. Skip inapplicable; bound expensive ones.
#' @export
eda_applicability_plan <- function(profile, max_pairwise_cols = 30L,
    max_cluster_rows = 4000L) {
  arch <- profile$archetypes
  temporal <- "temporal" %in% arch || "regime_candidate" %in% arch
  target <- "supervised_target" %in% arch
  cat_heavy <- "categorical_heavy" %in% arch
  high_dim <- "high_dim_numeric" %in% arch
  miss_heavy <- "missing_heavy" %in% arch
  decision <- function(run, reason) list(decision = run, reason = reason)
  plan <- list(
    schema_types = decision("run", "Always required."),
    cardinality = decision("run", "Always required."),
    grain_keys_duplicates = decision("run", "Duplicate/grain risks apply to tabular data."),
    missingness_structure = decision("run",
      if (miss_heavy) "Missingness is material." else "Always inspect missingness structure."),
    distributions_tails_zeros = decision(
      if (profile$n_numeric > 0L) "run" else "skip_inapplicable",
      if (profile$n_numeric > 0L) "Numeric fields present." else "No numeric fields."),
    rare_levels = decision(
      if (profile$n_categorical > 0L) "run" else "skip_inapplicable",
      if (cat_heavy) "Categorical-heavy data." else if (profile$n_categorical)
        "Categorical fields present." else "No categorical fields."),
    numeric_relationships = decision(
      if (profile$n_numeric >= 2L) "run" else "skip_inapplicable",
      if (high_dim) "High-dimensional numeric; bounded pairwise." else
        if (profile$n_numeric >= 2L) "Numeric pairs present." else "Need two numeric fields."),
    nonlinear_categorical_numeric = decision(
      if (profile$n_numeric >= 2L || (profile$n_numeric >= 1L &&
          profile$n_categorical >= 1L)) "run" else "skip_inapplicable",
      "Cheap Pearson-vs-Spearman and group-mean hints only."),
    temporal_coverage = decision(
      if (temporal) "run" else "skip_inapplicable",
      if (temporal) "Temporal fields present." else "No temporal fields."),
    periodicity_seasonality = decision(
      if (temporal && profile$n_rows >= 24L) "run" else
        if (temporal) "skip_inapplicable" else "skip_inapplicable",
      if (temporal && profile$n_rows >= 24L) "Enough periods for cheap seasonality." else
        "Seasonality not informative."),
    regime_change = decision(
      if (temporal && profile$n_rows >= 24L) "run" else "skip_inapplicable",
      if (temporal) "Cheap first/last-third mean shift." else "No temporal fields."),
    target_relationships = decision(
      if (target) "run" else "skip_inapplicable",
      if (target) "Target declared; predictive association only." else
        "No target declared."),
    leakage_post_treatment = decision(
      if (target) "run" else "skip_inapplicable",
      if (target) "Name-based leakage screen; full registry owned by target_analysis." else
        "No target declared."),
    drift = decision(
      if (temporal && target) "run" else "skip_inapplicable",
      if (temporal && target) "Target+time: cheap early/recent split." else
        "Drift requires time and usually a target; full drift is target_analysis."),
    anomalies = decision("run", "Cheap IQR/MAD counts; optional."),
    clustering_segmentation = decision(
      if (profile$n_rows <= max_cluster_rows && profile$n_numeric >= 2L)
        "skip_expensive" else "skip_expensive",
      "Clustering is optional VOI; not run by default (resource)."),
    transformations = decision(
      if (profile$n_numeric > 0L) "run" else "skip_inapplicable",
      "Skew/zero suggestions only."),
    unresolved_dq = decision("run", "Always required.")
  )
  plan$max_pairwise_cols <- max_pairwise_cols
  plan
}

eda_envelope_run <- function(plan, avenue) {
  identical(plan[[avenue]]$decision, "run")
}

#' Build a canonical EDA communication envelope without HTML authority.
#' @export
generate_eda_communication_envelope <- function(data, DataName = NULL,
    TargetVar = NULL, DateVar = NULL, EntityVar = NULL,
    max_rows = 50000L, max_pairwise_cols = 30L) {
  if (missing(data) || is.null(data)) stop("`data` is required.", call. = FALSE)
  data <- data.table::as.data.table(data)
  profile <- eda_characterize_dataset(data, TargetVar, DateVar, EntityVar)
  plan <- eda_applicability_plan(profile, max_pairwise_cols = max_pairwise_cols)
  sampled <- eda_envelope_sample(data, max_rows)
  d <- sampled$data
  kinds <- profile$column_kinds
  findings <- list()
  diagnostics <- list()
  warnings <- character()
  limitations <- c(
    "EDA is exploratory. Association is not causation.",
    "HTML/RMarkdown is not the analytical contract."
  )
  next_ex <- character()
  unresolved <- character()
  visual_specs <- list()
  push_f <- function(stmt, avenue) {
    findings[[length(findings) + 1L]] <<- eda_envelope_item("finding", stmt, avenue)
  }
  push_d <- function(name, value, avenue) {
    diagnostics[[length(diagnostics) + 1L]] <<- list(name = name, value = value,
      avenue = avenue)
  }

  if (eda_envelope_run(plan, "schema_types")) {
    push_f(sprintf("%s: %d rows x %d fields (%d numeric, %d categorical, %d temporal).",
      eda_coalesce(DataName, "Dataset"), profile$n_rows, profile$n_cols, profile$n_numeric,
      profile$n_categorical, profile$n_temporal), "schema_types")
    push_d("column_kinds", as.list(kinds), "schema_types")
  }
  if (eda_envelope_run(plan, "cardinality")) {
    card <- vapply(d, function(x) data.table::uniqueN(x), integer(1))
    push_d("cardinality", as.list(card), "cardinality")
    push_f(sprintf("Median field cardinality is %s.",
      stats::median(as.numeric(card))), "cardinality")
  }
  if (eda_envelope_run(plan, "grain_keys_duplicates")) {
    dup <- sum(duplicated(d))
    push_d("duplicate_rows", dup, "grain_keys_duplicates")
    if (dup > 0L) {
      warnings <- c(warnings, sprintf("%d fully duplicated rows.", dup))
      push_f(sprintf("%d duplicate rows; grain/keys need review.", dup),
        "grain_keys_duplicates")
    } else {
      push_f("No fully duplicated rows in the inspected sample.",
        "grain_keys_duplicates")
    }
  }
  if (eda_envelope_run(plan, "missingness_structure")) {
    miss_col <- vapply(d, function(x) mean(is.na(x)), numeric(1))
    push_d("missing_rate_by_field", as.list(miss_col), "missingness_structure")
    heavy <- names(miss_col)[miss_col >= 0.4]
    if (length(heavy)) {
      warnings <- c(warnings, paste("High missingness:", paste(heavy, collapse = ", ")))
    }
    miss_names <- names(sort(miss_col, decreasing = TRUE))
    miss_names <- miss_names[miss_col[miss_names] > 0]
    miss_names <- utils::head(miss_names, 6L)
    if (length(miss_names) >= 2L) {
      flags <- lapply(d[, miss_names, with = FALSE], is.na)
      co <- 0
      for (i in seq_len(length(miss_names) - 1L)) {
        for (j in (i + 1L):length(miss_names)) {
          both <- mean(flags[[i]] & flags[[j]])
          if (isTRUE(both > 0.05)) co <- co + 1L
        }
      }
      push_d("missing_pairwise_cooccur_pairs", co, "missingness_structure")
      push_f(sprintf("Missingness is not only a rate: %d field-pairs co-miss in >5%% of rows.",
        co), "missingness_structure")
    } else {
      push_f(sprintf("Overall missing rate %.1f%%.", 100 * profile$missing_rate),
        "missingness_structure")
    }
  }
  if (eda_envelope_run(plan, "distributions_tails_zeros")) {
    nums <- names(kinds)[kinds == "numeric"]
    nums <- utils::head(nums, 25L)
    zeros <- vapply(d[, nums, with = FALSE], function(x) mean(x == 0, na.rm = TRUE),
      numeric(1))
    skew <- vapply(d[, nums, with = FALSE], function(x) {
      x <- as.numeric(x[is.finite(x)])
      if (length(x) < 8L) return(NA_real_)
      m <- mean(x); s <- stats::sd(x)
      if (!is.finite(s) || s == 0) return(0)
      mean((x - m)^3) / (s^3)
    }, numeric(1))
    push_d("zero_share", as.list(zeros), "distributions_tails_zeros")
    push_d("approx_skew", as.list(skew), "distributions_tails_zeros")
    hi <- names(skew)[is.finite(skew) & abs(skew) >= 2]
    if (length(hi)) push_f(paste("Highly skewed numeric fields:",
      paste(utils::head(hi, 8L), collapse = ", ")), "distributions_tails_zeros")
  }
  if (eda_envelope_run(plan, "rare_levels")) {
    cats <- names(kinds)[kinds == "categorical"]
    rare <- list()
    for (nm in utils::head(cats, 20L)) {
      tab <- table(d[[nm]], useNA = "no")
      rare[[nm]] <- as.integer(sum(tab < max(5L, 0.01 * nrow(d))))
    }
    push_d("rare_level_counts", rare, "rare_levels")
    if (any(unlist(rare) > 0L))
      push_f("Rare categorical levels are present and can destabilize encoding.",
        "rare_levels")
  }
  nums <- names(kinds)[kinds == "numeric"]
  if (eda_envelope_run(plan, "numeric_relationships") && length(nums) >= 2L) {
    use <- utils::head(nums, max_pairwise_cols)
    mat <- suppressWarnings(stats::cor(as.data.frame(d[, use, with = FALSE]),
      use = "pairwise.complete.obs", method = "spearman"))
    high <- which(abs(mat) >= 0.7 & upper.tri(mat), arr.ind = TRUE)
    push_d("high_spearman_pairs", nrow(high), "numeric_relationships")
    push_f(sprintf("%d numeric pairs |Spearman| >= 0.70 (bounded to %d fields).",
      nrow(high), length(use)), "numeric_relationships")
    visual_specs <- c(visual_specs, list(list(id = "corr_spearman",
      kind = "heatmap", fields = use)))
  }
  if (eda_envelope_run(plan, "nonlinear_categorical_numeric") && length(nums) >= 2L) {
    use <- utils::head(nums, min(12L, max_pairwise_cols))
    df <- as.data.frame(d[, use, with = FALSE])
    pcor <- suppressWarnings(stats::cor(df, use = "pairwise.complete.obs",
      method = "pearson"))
    scor <- suppressWarnings(stats::cor(df, use = "pairwise.complete.obs",
      method = "spearman"))
    delta <- abs(pcor - scor)
    nflag <- sum(delta >= 0.25 & upper.tri(delta), na.rm = TRUE)
    push_d("pearson_spearman_discrepant_pairs", nflag, "nonlinear_categorical_numeric")
    if (nflag > 0L)
      push_f(sprintf("%d pairs show |Pearson-Spearman| >= 0.25 (nonlinearity hint, not a model).",
        nflag), "nonlinear_categorical_numeric")
    limitations <- c(limitations,
      "Nonlinearity is hinted by rank-vs-linear correlation, not a fitted nonlinear model.")
  }
  date_var <- profile$date_var
  if (eda_envelope_run(plan, "temporal_coverage") && !is.null(date_var) &&
      date_var %in% names(d)) {
    tcol <- d[[date_var]]
    tcol <- tcol[!is.na(tcol)]
    if (length(tcol)) {
      rng <- range(tcol)
      push_f(sprintf("Temporal coverage %s to %s on `%s`.",
        as.character(rng[1L]), as.character(rng[2L]), date_var),
        "temporal_coverage")
      push_d("temporal_range", as.character(rng), "temporal_coverage")
    }
  }
  if (eda_envelope_run(plan, "periodicity_seasonality") && !is.null(date_var) &&
      date_var %in% names(d) && length(nums)) {
    y <- as.numeric(d[[nums[[1L]]]])
    tm <- d[[date_var]]
    ok <- !is.na(y) & !is.na(tm)
    if (sum(ok) >= 24L) {
      mon <- as.integer(format(as.Date(tm[ok]), "%m"))
      bym <- tapply(y[ok], mon, mean, na.rm = TRUE)
      amp <- if (length(bym) >= 4L) diff(range(bym, na.rm = TRUE)) else 0
      push_d("monthly_mean_amplitude", amp, "periodicity_seasonality")
      if (isTRUE(amp > 0))
        push_f("Monthly mean amplitude is nonzero; seasonality may be material.",
          "periodicity_seasonality")
    }
  }
  if (eda_envelope_run(plan, "regime_change") && !is.null(date_var) &&
      date_var %in% names(d) && length(nums)) {
    ord <- order(d[[date_var]])
    y <- as.numeric(d[[nums[[1L]]]][ord])
    y <- y[is.finite(y)]
    if (length(y) >= 24L) {
      k <- floor(length(y) / 3)
      m1 <- mean(y[seq_len(k)], na.rm = TRUE)
      m3 <- mean(y[(length(y) - k + 1L):length(y)], na.rm = TRUE)
      push_d("first_vs_last_third_mean", c(m1, m3), "regime_change")
      if (is.finite(m1) && is.finite(m3) && abs(m3 - m1) > 0.25 * (abs(m1) + 1e-9)) {
        warnings <- c(warnings, "Possible regime/level shift (first vs last third).")
        push_f("Cheap regime screen suggests a level shift; not a causal break test.",
          "regime_change")
      }
    }
  }
  tgt <- profile$target_var
  if (eda_envelope_run(plan, "target_relationships") && !is.null(tgt) &&
      tgt %in% names(d)) {
    limitations <- c(limitations,
      "Target associations are predictive only and do not imply causation.")
    if (tgt %in% nums && length(setdiff(nums, tgt)) >= 1L) {
      others <- utils::head(setdiff(nums, tgt), 15L)
      cors <- suppressWarnings(stats::cor(as.data.frame(d[, c(tgt, others),
        with = FALSE]), use = "pairwise.complete.obs", method = "spearman"))
      cors <- cors[tgt, others]
      top <- names(sort(abs(cors), decreasing = TRUE))[1L]
      push_f(sprintf("Strongest Spearman association with `%s` is `%s` (%.2f).",
        tgt, top, cors[[top]]), "target_relationships")
    }
    next_ex <- c(next_ex,
      "Run analytics.target_analysis for leakage, collider, and drift registries.")
  }
  if (eda_envelope_run(plan, "leakage_post_treatment") && !is.null(tgt)) {
    nm <- names(d)
    leak <- nm[grepl("(^post_|_after$|_future|leakage|outcome_lag)", nm,
      ignore.case = TRUE)]
    leak <- setdiff(leak, tgt)
    push_d("name_based_leakage_candidates", leak, "leakage_post_treatment")
    if (length(leak)) {
      warnings <- c(warnings, paste("Name-based leakage candidates:",
        paste(leak, collapse = ", ")))
      push_f("Name-based leakage screen found suspicious fields; this is not a causal identification result.",
        "leakage_post_treatment")
    } else {
      push_f("No name-based leakage candidates; full leakage registry remains target_analysis.",
        "leakage_post_treatment")
    }
  }
  if (eda_envelope_run(plan, "anomalies") && length(nums)) {
    nms <- utils::head(nums, 20L)
    iqr_n <- vapply(d[, nms, with = FALSE], function(x) {
      x <- as.numeric(x[is.finite(x)])
      if (length(x) < 8L) return(0L)
      q <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
      iqr <- q[2L] - q[1L]
      sum(x < q[1L] - 1.5 * iqr | x > q[2L] + 1.5 * iqr)
    }, integer(1))
    mad_n <- vapply(d[, nms, with = FALSE], function(x) {
      x <- as.numeric(x[is.finite(x)])
      if (length(x) < 8L) return(0L)
      m <- stats::median(x); md <- stats::mad(x)
      if (!is.finite(md) || md == 0) return(0L)
      sum(abs(x - m) / md > 6)
    }, integer(1))
    push_d("iqr_outlier_counts", as.list(iqr_n), "anomalies")
    push_d("mad_outlier_counts", as.list(mad_n), "anomalies")
    push_f("Outlier screens use both IQR and MAD, not IQR count alone.",
      "anomalies")
  }
  if (identical(plan$clustering_segmentation$decision, "skip_expensive")) {
    next_ex <- c(next_ex,
      "Clustering/segmentation skipped by default; run unsupervised only if VOI justifies it.")
  }
  if (eda_envelope_run(plan, "transformations") && length(nums)) {
    skew <- vapply(utils::head(nums, 15L), function(nm) {
      x <- as.numeric(d[[nm]]); x <- x[is.finite(x)]
      if (length(x) < 8L) return(0)
      m <- mean(x); s <- stats::sd(x)
      if (!is.finite(s) || s == 0) 0 else mean((x - m)^3) / (s^3)
    }, numeric(1))
    sug <- names(skew)[abs(skew) >= 2]
    if (length(sug)) push_f(paste("Consider log/offset transforms for:",
      paste(sug, collapse = ", ")), "transformations")
  }
  skipped <- Filter(function(a) plan[[a]]$decision != "run",
    eda_envelope_avenues())
  if (eda_envelope_run(plan, "unresolved_dq")) {
    if (length(warnings)) unresolved <- c(unresolved, warnings)
    unresolved <- c(unresolved, "Confirm grain/keys with domain owners.")
  }

  list(
    schema_version = eda_communication_envelope_schema(),
    report_type = "eda_communication_envelope",
    archetypes = profile$archetypes,
    profile = profile[c("n_rows", "n_cols", "missing_rate", "n_numeric",
      "n_categorical", "n_temporal", "date_var", "target_var")],
    applicability = lapply(eda_envelope_avenues(), function(a) {
      c(list(avenue = a), plan[[a]])
    }),
    findings = findings,
    diagnostics = diagnostics,
    visual_specs = visual_specs,
    warnings = unique(warnings),
    limitations = unique(limitations),
    next_examination = unique(next_ex),
    unresolved = unique(unresolved),
    skipped_inapplicable = skipped[vapply(skipped, function(a)
      identical(plan[[a]]$decision, "skip_inapplicable"), logical(1))],
    skipped_expensive = skipped[vapply(skipped, function(a)
      identical(plan[[a]]$decision, "skip_expensive"), logical(1))],
    resource = list(sampled = sampled$sampled, inspected_rows = nrow(d),
      source_rows = sampled$n, max_pairwise_cols = max_pairwise_cols),
    provenance = list(generator = "generate_eda_communication_envelope",
      dataset_name = DataName, created_at = as.character(Sys.time()))
  )
}

#' Project existing target/model-readiness artifacts into a communication envelope.
#' Does not recompute leakage math; lists generated plots even if HTML omits them.
#' @export
generate_target_communication_envelope <- function(data, TargetVar,
    TrendDateVar = NULL, TrendGroupVar = NULL, DataName = NULL,
    RunGAMDiagnostics = FALSE, artifacts = NULL) {
  if (is.null(artifacts)) {
    if (!exists("generate_model_assessment_artifacts", mode = "function"))
      stop("generate_model_assessment_artifacts is required.", call. = FALSE)
    artifacts <- generate_model_assessment_artifacts(
      data = data, DataName = eda_coalesce(DataName, "Target"), TargetVar = TargetVar,
      TrendDateVar = TrendDateVar, TrendGroupVar = TrendGroupVar,
      RunGAMDiagnostics = isTRUE(RunGAMDiagnostics), ExportPNG = FALSE,
      ExportHTML = FALSE)
  }
  plot_names <- character()
  if (is.list(artifacts$plots)) plot_names <- names(artifacts$plots)
  tables <- artifacts$tables %||% list()
  findings <- list()
  warnings <- as.character(artifacts$warnings %||% character())
  if (is.data.frame(artifacts$context$top_feature_risks) ||
      is.data.frame(tables$TargetFeatureRiskRegistry)) {
    findings <- c(findings, list(eda_envelope_item("finding",
      "Target feature-risk registry is available (leakage/collider/drift scores).",
      "leakage_post_treatment")))
  }
  findings <- c(findings, list(eda_envelope_item("finding",
    sprintf("Target `%s` readiness envelope produced; plots generated: %s.",
      TargetVar, paste(plot_names, collapse = ", ")), "target_relationships")))
  list(
    schema_version = "target_communication_envelope_v1",
    report_type = "target_communication_envelope",
    target = TargetVar,
    findings = findings,
    diagnostics = list(list(name = "plot_keys", value = as.list(plot_names),
      avenue = "target_relationships")),
    visual_specs = lapply(plot_names, function(nm) list(id = nm, kind = "plot_ref",
      rendered_in_html = FALSE)),
    warnings = warnings,
    limitations = c(
      "Target associations and risk flags are not causal identification.",
      "Generated plots are communication visual refs even if the Rmd omits them."
    ),
    next_examination = "Review leakage/collider flags before explanatory modeling.",
    provenance = list(generator = "generate_model_assessment_artifacts",
      envelope = "generate_target_communication_envelope"),
    artifacts_ref = "model_assessment_artifacts"
  )
}
