# ============================================================
# Regression Model Insights Artifact Generation
# Converted from RMarkdown chunks into regular R code, then expanded into
# a complete regression model audit / insights artifact generator.
#
# Purpose:
#   1. Generate all regression model insights artifacts before rendering.
#   2. Pass those artifacts into RMarkdown for dynamic display.
#   3. Preserve legacy Regression_ModelInsights.Rmd behavior.
#   4. Add expert-level diagnostics: residuals, calibration, error analysis,
#      uplift-by-feature, stratified effects, Simpson's Paradox flags,
#      segment performance, stability, and deployment-readiness checks.
#   5. Optionally export PNG/HTML/data-url sidecars for LLM vision/API usage.
#
# Notes:
#   - Visuals are built with AutoPlots wrappers/primitives.
#   - Data wrangling happens before calling AutoPlots.
#   - Export work delegates to object_export_helpers.R:
#       ObjectToPNG(), ObjectToHTML(), ObjectFileToDataURL(), ObjectFileToMarkdown()
#   - No duplicated artifacts$artifacts branch is returned.
# ============================================================


# ============================================================
# Generic Artifact Sidecar Helpers
# ============================================================

model_insights_slug <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- tolower(x)
  if (!nzchar(x)) x <- "artifact"
  x
}

model_insights_object_export_helpers_available <- function() {
  all(vapply(
    c("ObjectToPNG", "ObjectToHTML", "ObjectFileToDataURL", "ObjectFileToMarkdown"),
    exists,
    logical(1L),
    mode = "function"
  ))
}

model_insights_require_object_export_helpers <- function() {
  if (!model_insights_object_export_helpers_available()) {
    stop(
      paste(
        "Object export helpers are required for ExportPNG/ExportHTML/IncludeDataURL.",
        "Source object_export_helpers.R first, or include it in the package R/ directory.",
        "Required functions: ObjectToPNG(), ObjectToHTML(), ObjectFileToDataURL(), ObjectFileToMarkdown()."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

model_insights_is_renderable_object <- function(object) {
  inherits(object, "ggplot") ||
    inherits(object, "htmlwidget") ||
    inherits(object, "recordedplot") ||
    inherits(object, "reactable") ||
    is.function(object)
}

model_insights_artifact <- function(
    object,
    section,
    artifact_type,
    name,
    title = NULL,
    description = NULL,
    metadata = list()
) {
  list(
    object = object,
    png = NULL,
    html = NULL,
    data_url = NULL,
    markdown = NULL,
    section = section,
    artifact_type = artifact_type,
    name = name,
    title = title,
    description = description,
    metadata = metadata
  )
}

model_insights_wrap_named_objects <- function(x, section, artifact_type, metadata = list()) {
  if (is.null(x)) {
    return(NULL)
  }

  if (!is.list(x) ||
      inherits(x, "htmlwidget") ||
      inherits(x, "ggplot") ||
      inherits(x, "reactable") ||
      inherits(x, "datatables")) {
    nm <- artifact_type
    return(model_insights_artifact(
      object = x,
      section = section,
      artifact_type = artifact_type,
      name = nm,
      metadata = metadata
    ))
  }

  nms <- names(x)
  if (is.null(nms)) nms <- as.character(seq_along(x))

  stats::setNames(
    lapply(seq_along(x), function(i) {
      nm <- nms[[i]]
      obj <- x[[i]]

      if (is.list(obj) &&
          !inherits(obj, "htmlwidget") &&
          !inherits(obj, "ggplot") &&
          !inherits(obj, "reactable") &&
          !inherits(obj, "datatables")) {
        model_insights_wrap_named_objects(
          obj,
          section = section,
          artifact_type = artifact_type,
          metadata = c(metadata, list(parent = nm))
        )
      } else {
        model_insights_artifact(
          object = obj,
          section = section,
          artifact_type = artifact_type,
          name = nm,
          metadata = c(metadata, list(name = nm))
        )
      }
    }),
    nms
  )
}

model_insights_export_artifact_tree <- function(
    tree,
    output_path,
    export_png = FALSE,
    export_html = FALSE,
    width = 1400,
    height = 900,
    dpi = 150,
    background = "white",
    delay = 0.2,
    include_data_url = FALSE,
    path_parts = character()
) {
  if (is.null(tree)) {
    return(tree)
  }

  if (isTRUE(export_png) || isTRUE(export_html) || isTRUE(include_data_url)) {
    model_insights_require_object_export_helpers()
  }

  is_artifact_node <- is.list(tree) &&
    all(c("object", "png", "html", "section", "artifact_type", "name") %in% names(tree))

  if (is_artifact_node) {
    obj <- tree$object

    if (model_insights_is_renderable_object(obj)) {
      stem <- model_insights_slug(
        paste(c(path_parts, tree$section, tree$artifact_type, tree$name), collapse = "_")
      )

      if (isTRUE(export_png)) {
        png_file <- file.path(output_path, "images", paste0(stem, ".png"))

        tree$png <- tryCatch(
          ObjectToPNG(
            object = obj,
            file = png_file,
            width = width,
            height = height,
            dpi = dpi,
            background = background,
            overwrite = TRUE,
            delay = delay
          ),
          error = function(e) {
            tree$png_error <<- conditionMessage(e)
            NULL
          }
        )

        if (!is.null(tree$png)) {
          tree$markdown <- ObjectFileToMarkdown(
            file = tree$png,
            alt = ifelse(is.null(tree$title), tree$name, tree$title)
          )

          if (isTRUE(include_data_url)) {
            tree$data_url <- tryCatch(
              ObjectFileToDataURL(tree$png, mime_type = "image/png"),
              error = function(e) {
                tree$data_url_error <<- conditionMessage(e)
                NULL
              }
            )
          }
        }
      }

      if (isTRUE(export_html)) {
        html_file <- file.path(output_path, "html", paste0(stem, ".html"))

        tree$html <- tryCatch(
          ObjectToHTML(
            object = obj,
            file = html_file,
            width = width,
            height = height,
            dpi = dpi,
            background = background,
            overwrite = TRUE
          ),
          error = function(e) {
            tree$html_error <<- conditionMessage(e)
            NULL
          }
        )
      }
    }

    return(tree)
  }

  if (is.list(tree)) {
    nms <- names(tree)
    if (is.null(nms)) nms <- as.character(seq_along(tree))

    for (i in seq_along(tree)) {
      tree[[i]] <- model_insights_export_artifact_tree(
        tree[[i]],
        output_path = output_path,
        export_png = export_png,
        export_html = export_html,
        width = width,
        height = height,
        dpi = dpi,
        background = background,
        delay = delay,
        include_data_url = include_data_url,
        path_parts = c(path_parts, nms[[i]])
      )
    }
  }

  tree
}

model_insights_collect_llm_image_manifest <- function(tree, path = character()) {
  if (is.null(tree)) return(list())

  is_artifact_node <- is.list(tree) &&
    all(c("object", "png", "html", "section", "artifact_type", "name") %in% names(tree))

  if (is_artifact_node) {
    return(list(list(
      id = paste(c(path, tree$section, tree$artifact_type, tree$name), collapse = "/"),
      section = tree$section,
      artifact_type = tree$artifact_type,
      name = tree$name,
      title = tree$title,
      description = tree$description,
      png = tree$png,
      html = tree$html,
      data_url = tree$data_url,
      markdown = tree$markdown,
      metadata = tree$metadata,
      png_error = tree$png_error,
      html_error = tree$html_error,
      data_url_error = tree$data_url_error
    )))
  }

  if (!is.list(tree)) return(list())

  nms <- names(tree)
  if (is.null(nms)) nms <- as.character(seq_along(tree))

  unlist(
    lapply(seq_along(tree), function(i) {
      model_insights_collect_llm_image_manifest(tree[[i]], c(path, nms[[i]]))
    }),
    recursive = FALSE
  )
}

model_insights_count_renderable_objects <- function(x) {

  if (is.null(x)) {
    return(0L)
  }

  if (
    inherits(x, "htmlwidget") ||
    inherits(x, "ggplot") ||
    inherits(x, "reactable") ||
    inherits(x, "recordedplot") ||
    is.function(x)
  ) {
    return(1L)
  }

  if (!is.list(x)) {
    return(0L)
  }

  sum(vapply(
    x,
    model_insights_count_renderable_objects,
    integer(1L)
  ))
}

# ============================================================
# Small Utility Helpers
# ============================================================

model_insights_null_coalesce <- function(x, y) {
  if (is.null(x)) y else x
}

model_insights_as_dt <- function(x) {
  if (is.null(x)) return(NULL)
  data.table::as.data.table(x)
}

model_insights_empty_dt <- function() {
  data.table::data.table()
}

model_insights_safe_nrow <- function(x) {
  if (is.null(x)) return(0L)
  tryCatch(nrow(x), error = function(e) 0L)
}

model_insights_has_rows <- function(x) {
  !is.null(x) && model_insights_safe_nrow(x) > 0L
}

model_insights_safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

model_insights_is_numeric_col <- function(x) {
  is.numeric(x) || is.integer(x)
}

model_insights_is_categorical_col <- function(x) {
  is.character(x) || is.factor(x) || is.logical(x)
}

model_insights_finite <- function(x) {
  !is.na(x) & is.finite(x)
}

model_insights_safe_mean <- function(x) {
  x <- model_insights_safe_numeric(x)
  x <- x[model_insights_finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

model_insights_safe_median <- function(x) {
  x <- model_insights_safe_numeric(x)
  x <- x[model_insights_finite(x)]
  if (!length(x)) return(NA_real_)
  stats::median(x)
}

model_insights_safe_sd <- function(x) {
  x <- model_insights_safe_numeric(x)
  x <- x[model_insights_finite(x)]
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x)
}

model_insights_safe_min <- function(x) {
  x <- model_insights_safe_numeric(x)
  x <- x[model_insights_finite(x)]
  if (!length(x)) return(NA_real_)
  min(x)
}

model_insights_safe_max <- function(x) {
  x <- model_insights_safe_numeric(x)
  x <- x[model_insights_finite(x)]
  if (!length(x)) return(NA_real_)
  max(x)
}

model_insights_safe_quantile <- function(x, p) {
  x <- model_insights_safe_numeric(x)
  x <- x[model_insights_finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 7))
}

model_insights_safe_cor <- function(x, y, method = "pearson") {
  x <- model_insights_safe_numeric(x)
  y <- model_insights_safe_numeric(y)
  keep <- model_insights_finite(x) & model_insights_finite(y)
  if (sum(keep) < 3L) return(NA_real_)
  suppressWarnings(stats::cor(x[keep], y[keep], method = method, use = "complete.obs"))
}

model_insights_safe_skewness <- function(x) {
  x <- model_insights_safe_numeric(x)
  x <- x[model_insights_finite(x)]
  if (length(x) < 3L || isTRUE(all.equal(stats::sd(x), 0))) return(NA_real_)

  if (requireNamespace("e1071", quietly = TRUE)) {
    return(e1071::skewness(x, na.rm = TRUE))
  }

  m <- mean(x)
  s <- stats::sd(x)
  mean(((x - m) / s)^3)
}

model_insights_safe_kurtosis <- function(x) {
  x <- model_insights_safe_numeric(x)
  x <- x[model_insights_finite(x)]
  if (length(x) < 4L || isTRUE(all.equal(stats::sd(x), 0))) return(NA_real_)

  if (requireNamespace("e1071", quietly = TRUE)) {
    return(e1071::kurtosis(x, na.rm = TRUE))
  }

  m <- mean(x)
  s <- stats::sd(x)
  mean(((x - m) / s)^4) - 3
}

model_insights_make_bins <- function(x, n_bins = 20L, label_prefix = "Bin") {
  x_num <- model_insights_safe_numeric(x)
  out <- rep(NA_character_, length(x_num))

  keep <- model_insights_finite(x_num)
  if (sum(keep) == 0L) return(out)

  if (data.table::uniqueN(x_num[keep]) <= 1L) {
    out[keep] <- paste0(label_prefix, "_01")
    return(out)
  }

  ranks <- data.table::frank(x_num[keep], ties.method = "average")
  probs <- seq(0, 1, length.out = as.integer(n_bins) + 1L)
  breaks <- unique(stats::quantile(ranks, probs = probs, na.rm = TRUE, names = FALSE))

  if (length(breaks) <= 2L) {
    out[keep] <- paste0(label_prefix, "_01")
    return(out)
  }

  bin_id <- cut(ranks, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  out[keep] <- sprintf("%s_%02d", label_prefix, as.integer(bin_id))
  out
}

model_insights_metric_direction <- function(x, min_effect_size = NULL) {
  if (is.null(min_effect_size) || is.na(min_effect_size)) {
    min_effect_size <- 0
  }

  out <- rep("Flat", length(x))
  out[!is.na(x) & x > min_effect_size] <- "Positive"
  out[!is.na(x) & x < -min_effect_size] <- "Negative"
  out
}

model_insights_sample_dt <- function(dt, sample_size = NULL) {
  if (is.null(dt)) return(NULL)
  dt <- data.table::as.data.table(dt)

  if (is.null(sample_size) || is.na(sample_size) || sample_size <= 0L) {
    return(dt)
  }

  sample_size <- as.integer(sample_size)

  if (nrow(dt) <= sample_size) {
    return(dt)
  }

  dt[sample.int(nrow(dt), sample_size)]
}

model_insights_try_plot <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}

model_insights_try_dt <- function(expr) {
  tryCatch(expr, error = function(e) data.table::data.table())
}


# ============================================================
# Input Extraction
# ============================================================

model_insights_extract_regression_inputs <- function(
    ModelObject = NULL,
    Algo = NULL,
    TrainData = NULL,
    TestData = NULL,
    TargetColumnName = NULL,
    PredictionColumnName = "Predict",
    FeatureColumnNames = NULL,
    SegmentVars = NULL,
    ByVars = NULL,
    DateVar = NULL,
    IDColumn = NULL,
    Theme = "dark"
) {

  context <- list(
    extraction_notes = character(),
    warnings = character()
  )

  if (!is.null(ModelObject)) {
    TestData <- model_insights_null_coalesce(TestData, ModelObject[["TestData"]])
    TrainData <- model_insights_null_coalesce(TrainData, ModelObject[["TrainData"]])

    ArgsList <- ModelObject[["ArgsList"]]
    GridMetrics <- ModelObject[["GridMetrics"]]

    TargetColumnName <- model_insights_null_coalesce(
      TargetColumnName,
      ModelObject[["ArgsList"]][["TargetColumnName"]]
    )

    PredictionColumnName <- model_insights_null_coalesce(PredictionColumnName, "Predict")

    if (is.null(FeatureColumnNames)) {
      FeatureColumnNames <- ModelObject[["ColNames"]][[1L]]
    }

    Algo <- model_insights_null_coalesce(Algo, ModelObject[["ArgsList"]][["Algo"]])
    Algo <- model_insights_null_coalesce(Algo, ModelObject[["Algo"]])

    context$extraction_notes <- c(
      context$extraction_notes,
      "Inputs extracted from ModelObject when available."
    )
  } else {
    ArgsList <- NULL
    GridMetrics <- NULL
  }

  TrainData <- model_insights_as_dt(TrainData)
  TestData <- model_insights_as_dt(TestData)

  if (is.null(PredictionColumnName) || !nzchar(PredictionColumnName)) {
    PredictionColumnName <- "Predict"
  }

  available_names <- unique(c(names(TrainData), names(TestData)))

  if (is.null(FeatureColumnNames)) {
    FeatureColumnNames <- setdiff(
      available_names,
      c(TargetColumnName, PredictionColumnName, SegmentVars, ByVars, DateVar, IDColumn)
    )
  }

  FeatureColumnNames <- intersect(as.character(FeatureColumnNames), available_names)

  if (is.null(SegmentVars)) {
    candidate_data <- if (!is.null(TestData)) TestData else TrainData

    if (!is.null(candidate_data) && length(FeatureColumnNames)) {
      SegmentVars <- FeatureColumnNames[
        vapply(FeatureColumnNames, function(v) {
          v %in% names(candidate_data) &&
            model_insights_is_categorical_col(candidate_data[[v]]) &&
            data.table::uniqueN(candidate_data[[v]], na.rm = TRUE) <= 25L
        }, logical(1L))
      ]
    }
  }

  SegmentVars <- intersect(as.character(SegmentVars), available_names)

  if (is.null(ByVars)) {
    ByVars <- SegmentVars
  }

  ByVars <- intersect(as.character(ByVars), available_names)

  metadata <- list(
    report_type = "Regression Model Insights",
    artifact_type = "regression_model_insights",
    created_at = as.character(Sys.time()),
    algo = Algo,
    target_column = TargetColumnName,
    prediction_column = PredictionColumnName,
    feature_columns = FeatureColumnNames,
    n_features = length(FeatureColumnNames),
    segment_vars = SegmentVars,
    by_vars = ByVars,
    date_var = DateVar,
    id_column = IDColumn,
    has_model_object = !is.null(ModelObject),
    has_train_data = !is.null(TrainData),
    has_test_data = !is.null(TestData),
    train_rows = if (!is.null(TrainData)) nrow(TrainData) else 0L,
    test_rows = if (!is.null(TestData)) nrow(TestData) else 0L,
    theme = Theme,
    autoquant_version = tryCatch(as.character(utils::packageVersion("AutoQuant")), error = function(e) NA_character_),
    autoplots_version = tryCatch(as.character(utils::packageVersion("AutoPlots")), error = function(e) NA_character_)
  )

  list(
    ModelObject = ModelObject,
    Algo = Algo,
    TrainData = TrainData,
    TestData = TestData,
    ArgsList = ArgsList,
    GridMetrics = GridMetrics,
    TargetColumnName = TargetColumnName,
    PredictionColumnName = PredictionColumnName,
    FeatureColumnNames = FeatureColumnNames,
    SegmentVars = SegmentVars,
    ByVars = ByVars,
    DateVar = DateVar,
    IDColumn = IDColumn,
    Theme = Theme,
    metadata = metadata,
    context = context
  )
}


# ============================================================
# QA / Validation
# ============================================================

model_insights_build_regression_qa <- function(inputs) {

  check_split <- function(dt, split) {
    if (is.null(dt)) {
      return(data.table::data.table(
        Split = split,
        Check = "Data Exists",
        Status = "Fail",
        Detail = "Data is NULL"
      ))
    }

    target <- inputs$TargetColumnName
    pred <- inputs$PredictionColumnName

    checks <- list(
      data.table::data.table(
        Split = split,
        Check = "Data Exists",
        Status = "Pass",
        Detail = paste0(nrow(dt), " rows")
      ),
      data.table::data.table(
        Split = split,
        Check = "Target Column Exists",
        Status = if (!is.null(target) && target %in% names(dt)) "Pass" else "Fail",
        Detail = as.character(target)
      ),
      data.table::data.table(
        Split = split,
        Check = "Prediction Column Exists",
        Status = if (!is.null(pred) && pred %in% names(dt)) "Pass" else "Fail",
        Detail = as.character(pred)
      )
    )

    if (!is.null(target) && target %in% names(dt)) {
      y <- model_insights_safe_numeric(dt[[target]])

      checks[[length(checks) + 1L]] <- data.table::data.table(
        Split = split,
        Check = "Target Valid Values",
        Status = if (sum(!is.na(y) & is.finite(y)) > 0L) "Pass" else "Fail",
        Detail = paste0(
          "NA=", sum(is.na(y)),
          "; Inf=", sum(is.infinite(y)),
          "; finite=", sum(!is.na(y) & is.finite(y))
        )
      )
    }

    if (!is.null(pred) && pred %in% names(dt)) {
      p <- model_insights_safe_numeric(dt[[pred]])

      checks[[length(checks) + 1L]] <- data.table::data.table(
        Split = split,
        Check = "Prediction Valid Values",
        Status = if (sum(!is.na(p) & is.finite(p)) > 0L) "Pass" else "Fail",
        Detail = paste0(
          "NA=", sum(is.na(p)),
          "; Inf=", sum(is.infinite(p)),
          "; finite=", sum(!is.na(p) & is.finite(p))
        )
      )

      checks[[length(checks) + 1L]] <- data.table::data.table(
        Split = split,
        Check = "Prediction Variance",
        Status = if (data.table::uniqueN(p[!is.na(p) & is.finite(p)]) > 1L) "Pass" else "Warning",
        Detail = paste0("Unique finite predictions=", data.table::uniqueN(p[!is.na(p) & is.finite(p)]))
      )
    }

    data.table::rbindlist(checks, fill = TRUE)
  }

  validation <- data.table::rbindlist(
    list(
      check_split(inputs$TrainData, "Train"),
      check_split(inputs$TestData, "Test")
    ),
    fill = TRUE
  )

  feature_qa <- data.table::data.table(
    Feature = inputs$FeatureColumnNames,
    InTrain = if (!is.null(inputs$TrainData)) inputs$FeatureColumnNames %in% names(inputs$TrainData) else FALSE,
    InTest = if (!is.null(inputs$TestData)) inputs$FeatureColumnNames %in% names(inputs$TestData) else FALSE
  )

  list(
    validation = validation,
    feature_qa = feature_qa,
    has_failures = any(validation$Status == "Fail", na.rm = TRUE),
    has_warnings = any(validation$Status == "Warning", na.rm = TRUE)
  )
}


# ============================================================
# Model Metadata Tables
# ============================================================

model_insights_build_model_metadata_tables <- function(inputs) {

  args_table <- if (!is.null(inputs$ArgsList)) {
    data.table::data.table(
      Argument = names(inputs$ArgsList),
      Value = vapply(inputs$ArgsList, function(x) {
        if (length(x) == 0L) return(NA_character_)
        if (is.atomic(x) && length(x) == 1L) return(as.character(x))
        paste(utils::capture.output(str(x, give.attr = FALSE)), collapse = " ")
      }, character(1L))
    )
  } else {
    data.table::data.table()
  }

  grid_metrics <- if (!is.null(inputs$GridMetrics)) {
    data.table::as.data.table(inputs$GridMetrics)
  } else {
    data.table::data.table()
  }

  feature_summary <- data.table::data.table(
    Metric = c(
      "Algorithm",
      "Target Column",
      "Prediction Column",
      "Feature Count",
      "Segment Variable Count",
      "By Variable Count",
      "Train Rows",
      "Test Rows"
    ),
    Value = c(
      as.character(inputs$Algo),
      as.character(inputs$TargetColumnName),
      as.character(inputs$PredictionColumnName),
      as.character(length(inputs$FeatureColumnNames)),
      as.character(length(inputs$SegmentVars)),
      as.character(length(inputs$ByVars)),
      as.character(if (!is.null(inputs$TrainData)) nrow(inputs$TrainData) else 0L),
      as.character(if (!is.null(inputs$TestData)) nrow(inputs$TestData) else 0L)
    )
  )

  list(
    args_list = args_table,
    grid_metrics = grid_metrics,
    model_summary = feature_summary
  )
}


# ============================================================
# Data Audit Tables
# ============================================================

model_insights_build_data_audit_tables <- function(inputs) {

  audit_split <- function(dt, split) {
    if (is.null(dt)) return(data.table::data.table())

    vars <- unique(c(
      inputs$TargetColumnName,
      inputs$PredictionColumnName,
      inputs$FeatureColumnNames,
      inputs$SegmentVars,
      inputs$ByVars
    ))

    vars <- vars[!is.na(vars) & nzchar(vars)]
    vars <- intersect(vars, names(dt))

    if (!length(vars)) return(data.table::data.table())

    data.table::rbindlist(lapply(vars, function(v) {
      x <- dt[[v]]

      data.table::data.table(
        Split = split,
        Variable = v,
        Type = class(x)[1L],
        Rows = length(x),
        NA_N = sum(is.na(x)),
        NA_Pct = mean(is.na(x)) * 100,
        Infinite_N = if (model_insights_is_numeric_col(x)) sum(is.infinite(x)) else 0L,
        Unique_N = if (is.list(x)) NA_integer_ else data.table::uniqueN(x, na.rm = FALSE),
        NonMissingUnique_N = if (is.list(x)) NA_integer_ else data.table::uniqueN(x, na.rm = TRUE),
        IsFeature = v %in% inputs$FeatureColumnNames,
        IsTarget = identical(v, inputs$TargetColumnName),
        IsPrediction = identical(v, inputs$PredictionColumnName),
        IsSegment = v %in% inputs$SegmentVars,
        IsByVar = v %in% inputs$ByVars
      )
    }), fill = TRUE)
  }

  column_audit <- data.table::rbindlist(
    list(
      audit_split(inputs$TrainData, "Train"),
      audit_split(inputs$TestData, "Test")
    ),
    fill = TRUE
  )

  target_prediction_summary <- function(dt, split) {
    if (is.null(dt) ||
        is.null(inputs$TargetColumnName) ||
        is.null(inputs$PredictionColumnName) ||
        !inputs$TargetColumnName %in% names(dt) ||
        !inputs$PredictionColumnName %in% names(dt)) {
      return(data.table::data.table())
    }

    y <- model_insights_safe_numeric(dt[[inputs$TargetColumnName]])
    p <- model_insights_safe_numeric(dt[[inputs$PredictionColumnName]])

    data.table::data.table(
      Split = split,
      Variable = c(inputs$TargetColumnName, inputs$PredictionColumnName),
      Mean = c(model_insights_safe_mean(y), model_insights_safe_mean(p)),
      Median = c(model_insights_safe_median(y), model_insights_safe_median(p)),
      SD = c(model_insights_safe_sd(y), model_insights_safe_sd(p)),
      Min = c(model_insights_safe_min(y), model_insights_safe_min(p)),
      Max = c(model_insights_safe_max(y), model_insights_safe_max(p)),
      P05 = c(model_insights_safe_quantile(y, 0.05), model_insights_safe_quantile(p, 0.05)),
      P95 = c(model_insights_safe_quantile(y, 0.95), model_insights_safe_quantile(p, 0.95))
    )
  }

  list(
    column_audit = column_audit,
    target_prediction_summary = data.table::rbindlist(
      list(
        target_prediction_summary(inputs$TrainData, "Train"),
        target_prediction_summary(inputs$TestData, "Test")
      ),
      fill = TRUE
    )
  )
}


# ============================================================
# Evaluation Metrics
# ============================================================

model_insights_build_scored_data <- function(dt, split, target, pred, feature_cols = NULL, id_col = NULL) {
  if (is.null(dt) || is.null(target) || is.null(pred) || !target %in% names(dt) || !pred %in% names(dt)) {
    return(data.table::data.table())
  }

  keep_cols <- unique(c(id_col, target, pred, feature_cols))
  keep_cols <- intersect(keep_cols, names(dt))

  out <- data.table::copy(data.table::as.data.table(dt)[, ..keep_cols])
  out[, Split := split]
  out[, Actual := model_insights_safe_numeric(get(target))]
  out[, Prediction := model_insights_safe_numeric(get(pred))]
  out[, Residual := Actual - Prediction]
  out[, Error := Prediction - Actual]
  out[, AbsResidual := abs(Residual)]
  out[, SquaredResidual := Residual^2]
  out[, AbsPercentError := data.table::fifelse(Actual == 0 | is.na(Actual), NA_real_, abs(Residual / Actual))]
  out[, PredictionBin := model_insights_make_bins(Prediction, 20L, "PredBin")]
  out[, ActualBin := model_insights_make_bins(Actual, 20L, "ActualBin")]
  out[]
}

model_insights_regression_metrics <- function(dt, split) {
  if (is.null(dt) || !nrow(dt)) {
    return(data.table::data.table())
  }

  keep <- model_insights_finite(dt$Actual) & model_insights_finite(dt$Prediction)
  y <- dt$Actual[keep]
  p <- dt$Prediction[keep]

  if (!length(y)) {
    return(data.table::data.table())
  }

  r <- y - p
  denom_r2 <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  mape <- mean(abs(r / y), na.rm = TRUE)
  smape <- mean(2 * abs(p - y) / (abs(y) + abs(p)), na.rm = TRUE)
  wape <- sum(abs(r), na.rm = TRUE) / sum(abs(y), na.rm = TRUE)

  data.table::data.table(
    Split = split,
    Metric = c(
      "N",
      "MAE",
      "MedianAE",
      "MSE",
      "RMSE",
      "NRMSE_Mean",
      "NRMSE_Range",
      "R2",
      "Correlation",
      "Spearman_Correlation",
      "MAPE",
      "SMAPE",
      "WAPE",
      "Bias",
      "Mean_Error",
      "Median_Error",
      "Mean_Actual",
      "Mean_Prediction",
      "SD_Actual",
      "SD_Prediction",
      "Min_Actual",
      "Max_Actual",
      "Min_Prediction",
      "Max_Prediction"
    ),
    Value = c(
      length(y),
      mean(abs(r), na.rm = TRUE),
      stats::median(abs(r), na.rm = TRUE),
      mean(r^2, na.rm = TRUE),
      sqrt(mean(r^2, na.rm = TRUE)),
      sqrt(mean(r^2, na.rm = TRUE)) / abs(mean(y, na.rm = TRUE)),
      sqrt(mean(r^2, na.rm = TRUE)) / abs(max(y, na.rm = TRUE) - min(y, na.rm = TRUE)),
      ifelse(denom_r2 == 0, NA_real_, 1 - sum(r^2, na.rm = TRUE) / denom_r2),
      model_insights_safe_cor(y, p, "pearson"),
      model_insights_safe_cor(y, p, "spearman"),
      mape,
      smape,
      wape,
      mean(r, na.rm = TRUE),
      mean(p - y, na.rm = TRUE),
      stats::median(p - y, na.rm = TRUE),
      mean(y, na.rm = TRUE),
      mean(p, na.rm = TRUE),
      stats::sd(y, na.rm = TRUE),
      stats::sd(p, na.rm = TRUE),
      min(y, na.rm = TRUE),
      max(y, na.rm = TRUE),
      min(p, na.rm = TRUE),
      max(p, na.rm = TRUE)
    )
  )
}

model_insights_build_regression_evaluation_artifacts <- function(inputs, SampleSize = 100000L, Theme = "dark") {

  scored_train <- model_insights_build_scored_data(
    inputs$TrainData,
    "Train",
    inputs$TargetColumnName,
    inputs$PredictionColumnName,
    inputs$FeatureColumnNames,
    inputs$IDColumn
  )

  scored_test <- model_insights_build_scored_data(
    inputs$TestData,
    "Test",
    inputs$TargetColumnName,
    inputs$PredictionColumnName,
    inputs$FeatureColumnNames,
    inputs$IDColumn
  )

  computed_train <- model_insights_regression_metrics(scored_train, "Train")
  computed_test <- model_insights_regression_metrics(scored_test, "Test")
  computed_combined <- data.table::rbindlist(list(computed_train, computed_test), fill = TRUE)

  metric_gap <- data.table::data.table()

  if (nrow(computed_train) && nrow(computed_test)) {
    tr <- data.table::dcast(computed_train, Metric ~ Split, value.var = "Value")
    te <- data.table::dcast(computed_test, Metric ~ Split, value.var = "Value")
    metric_gap <- merge(tr, te, by = "Metric", all = TRUE)

    if (all(c("Train", "Test") %in% names(metric_gap))) {
      metric_gap[, Absolute_Gap := Test - Train]
      metric_gap[, Percent_Gap := data.table::fifelse(
        is.na(Train) | Train == 0,
        NA_real_,
        (Test - Train) / abs(Train)
      )]
      metric_gap[, Generalization_Flag := data.table::fifelse(
        Metric %in% c("MAE", "MSE", "RMSE", "MAPE", "SMAPE", "WAPE") & Percent_Gap > 0.25,
        "Potential Degradation",
        data.table::fifelse(Metric %in% c("R2", "Correlation", "Spearman_Correlation") & Absolute_Gap < -0.10,
                            "Potential Degradation", "OK")
      )]
    }
  }

  provided_metrics <- list(
    train = if (!is.null(inputs$ModelObject)) inputs$ModelObject[["EvaluationMetrics"]][["TrainData"]] else NULL,
    test = if (!is.null(inputs$ModelObject)) inputs$ModelObject[["EvaluationMetrics"]][["TestData"]] else NULL
  )

  plots <- list()

  make_eval_plots <- function(scored, split_label) {
    out <- list()

    if (!nrow(scored)) return(out)

    scored_sample <- data.table::copy(model_insights_sample_dt(scored, SampleSize))
    scored_sample[, Ideal := Actual]

    out$actual_vs_predicted_scatter <- model_insights_try_plot({
      chart <- AutoPlots::Scatter(
        dt = scored_sample,
        XVar = "Actual",
        YVar = "Prediction",
        title.text = paste0("Observed vs Predicted: ", split_label, " Data"),
        xAxis.title = "Observed",
        yAxis.title = "Predicted",
        Theme = Theme,
        legend.show = FALSE
      )
      chart <- echarts4r::e_line(
        chart,
        Ideal,
        symbol = "none",
        name = "Ideal fit",
        lineStyle = list(type = "dashed", width = 2)
      )
      AutoPlots::e_grid_full(
        chart,
        grid.left = "8%",
        grid.right = "5%",
        grid.top = "16%",
        grid.bottom = "18%",
        grid.containLabel = TRUE
      )
    })

    out$prediction_histogram <- model_insights_try_plot(
      AutoPlots::Histogram(
        dt = scored_sample[model_insights_finite(Prediction)],
        XVar = "Prediction",
        title.text = paste0("Prediction Distribution: ", split_label, " Data"),
        xAxis.title = "Prediction",
        yAxis.title = "Count",
        Theme = Theme
      )
    )

    out$actual_histogram <- model_insights_try_plot(
      AutoPlots::Histogram(
        dt = scored_sample[model_insights_finite(Actual)],
        XVar = "Actual",
        title.text = paste0("Actual Distribution: ", split_label, " Data"),
        xAxis.title = "Actual",
        yAxis.title = "Count",
        Theme = Theme
      )
    )

    out
  }

  plots$train <- make_eval_plots(scored_train, "Train")
  plots$test <- make_eval_plots(scored_test, "Test")

  list(
    tables = list(
      scored_train = scored_train,
      scored_test = scored_test,
      provided_metrics_train = model_insights_as_dt(provided_metrics$train),
      provided_metrics_test = model_insights_as_dt(provided_metrics$test),
      computed_metrics_train = computed_train,
      computed_metrics_test = computed_test,
      computed_metrics_combined = computed_combined,
      train_test_metric_gap = metric_gap
    ),
    plots = plots
  )
}


# ============================================================
# Residual Diagnostics
# ============================================================

model_insights_build_regression_residual_artifacts <- function(
    inputs,
    scored_train,
    scored_test,
    SampleSize = 100000L,
    NumberBins = 20L,
    Theme = "dark"
) {

  residual_summary <- function(scored, split) {
    if (is.null(scored) || !nrow(scored)) return(data.table::data.table())

    r <- scored$Residual[model_insights_finite(scored$Residual)]

    if (!length(r)) return(data.table::data.table())

    data.table::data.table(
      Split = split,
      N = length(r),
      MeanResidual = mean(r, na.rm = TRUE),
      MedianResidual = stats::median(r, na.rm = TRUE),
      MeanAbsResidual = mean(abs(r), na.rm = TRUE),
      MedianAbsResidual = stats::median(abs(r), na.rm = TRUE),
      RMSE = sqrt(mean(r^2, na.rm = TRUE)),
      ResidualSD = stats::sd(r, na.rm = TRUE),
      ResidualMAD = stats::mad(r, na.rm = TRUE),
      ResidualSkewness = model_insights_safe_skewness(r),
      ResidualKurtosis = model_insights_safe_kurtosis(r),
      P01 = model_insights_safe_quantile(r, 0.01),
      P05 = model_insights_safe_quantile(r, 0.05),
      P10 = model_insights_safe_quantile(r, 0.10),
      P25 = model_insights_safe_quantile(r, 0.25),
      P50 = model_insights_safe_quantile(r, 0.50),
      P75 = model_insights_safe_quantile(r, 0.75),
      P90 = model_insights_safe_quantile(r, 0.90),
      P95 = model_insights_safe_quantile(r, 0.95),
      P99 = model_insights_safe_quantile(r, 0.99),
      MinResidual = min(r, na.rm = TRUE),
      MaxResidual = max(r, na.rm = TRUE)
    )
  }

  by_bin <- function(scored, split, bin_col) {
    if (is.null(scored) || !nrow(scored) || !bin_col %in% names(scored)) return(data.table::data.table())

    scored[
      !is.na(get(bin_col)) & model_insights_finite(Residual),
      .(
        N = .N,
        ActualMean = mean(Actual, na.rm = TRUE),
        PredictionMean = mean(Prediction, na.rm = TRUE),
        ResidualMean = mean(Residual, na.rm = TRUE),
        AbsResidualMean = mean(AbsResidual, na.rm = TRUE),
        RMSE = sqrt(mean(Residual^2, na.rm = TRUE))
      ),
      by = c("Split", bin_col)
    ]
  }

  tables <- list(
    residual_summary_train = residual_summary(scored_train, "Train"),
    residual_summary_test = residual_summary(scored_test, "Test"),
    residual_by_prediction_bin_train = by_bin(scored_train, "Train", "PredictionBin"),
    residual_by_prediction_bin_test = by_bin(scored_test, "Test", "PredictionBin"),
    residual_by_actual_bin_train = by_bin(scored_train, "Train", "ActualBin"),
    residual_by_actual_bin_test = by_bin(scored_test, "Test", "ActualBin")
  )

  plots <- list()

  make_residual_plots <- function(raw_dt, scored, split_label) {
    out <- list()

    if (is.null(raw_dt) || !model_insights_has_rows(raw_dt)) return(out)

    out$histogram <- model_insights_try_plot(
      AutoPlots::Residuals.Histogram(
        dt = raw_dt,
        AggMethod = "mean",
        SampleSize = SampleSize,
        XVar = inputs$PredictionColumnName,
        YVar = inputs$TargetColumnName,
        NumberBins = NumberBins,
        title.text = paste0("Residuals Histogram: ", split_label, " Data"),
        yAxis.title = inputs$TargetColumnName,
        xAxis.title = inputs$PredictionColumnName,
        Theme = Theme,
        TimeLine = FALSE
      )
    )

    if (!is.null(scored) && nrow(scored)) {
      scored_sample <- model_insights_sample_dt(scored, SampleSize)

      out$residual_vs_prediction <- model_insights_try_plot(
        AutoPlots::Scatter(
          dt = scored_sample,
          XVar = "Prediction",
          YVar = "Residual",
          title.text = paste0("Residual vs Prediction: ", split_label, " Data"),
          xAxis.title = "Prediction",
          yAxis.title = "Residual",
          Theme = Theme,
          legend.show = FALSE
        )
      )

      out$residual_vs_actual <- model_insights_try_plot(
        AutoPlots::Scatter(
          dt = scored_sample,
          XVar = "Actual",
          YVar = "Residual",
          title.text = paste0("Residual vs Actual: ", split_label, " Data"),
          xAxis.title = "Actual",
          yAxis.title = "Residual",
          Theme = Theme,
          legend.show = FALSE
        )
      )

      out$abs_residual_vs_prediction <- model_insights_try_plot(
        AutoPlots::Scatter(
          dt = scored_sample,
          XVar = "Prediction",
          YVar = "AbsResidual",
          title.text = paste0("Absolute Residual vs Prediction: ", split_label, " Data"),
          xAxis.title = "Prediction",
          yAxis.title = "Absolute Residual",
          Theme = Theme,
          legend.show = FALSE
        )
      )

      out$residual_box_by_prediction_bin <- model_insights_try_plot(
        AutoPlots::Box(
          dt = scored_sample[!is.na(PredictionBin)],
          XVar = "PredictionBin",
          YVar = "Residual",
          title.text = paste0("Residual by Prediction Bin: ", split_label, " Data"),
          xAxis.title = "Prediction Bin",
          yAxis.title = "Residual",
          Theme = Theme,
          xAxis.axisLabel.rotate = 45,
          tooltip.show = FALSE
        )
      )
    }

    out
  }

  plots$train <- make_residual_plots(inputs$TrainData, scored_train, "Train")
  plots$test <- make_residual_plots(inputs$TestData, scored_test, "Test")

  list(tables = tables, plots = plots)
}


# ============================================================
# Calibration Diagnostics
# ============================================================

model_insights_build_calibration_table <- function(scored, split, bins = 21L) {
  if (is.null(scored) || !nrow(scored)) return(data.table::data.table())

  dt <- data.table::copy(scored[model_insights_finite(Actual) & model_insights_finite(Prediction)])
  if (!nrow(dt)) return(data.table::data.table())

  dt[, CalibrationBin := model_insights_make_bins(Prediction, bins, "CalBin")]

  out <- dt[
    !is.na(CalibrationBin),
    .(
      N = .N,
      Prediction_Min = min(Prediction, na.rm = TRUE),
      Prediction_Max = max(Prediction, na.rm = TRUE),
      Prediction_Mean = mean(Prediction, na.rm = TRUE),
      Prediction_Median = stats::median(Prediction, na.rm = TRUE),
      Actual_Mean = mean(Actual, na.rm = TRUE),
      Actual_Median = stats::median(Actual, na.rm = TRUE),
      Residual_Mean = mean(Residual, na.rm = TRUE),
      AbsResidual_Mean = mean(AbsResidual, na.rm = TRUE),
      RMSE = sqrt(mean(Residual^2, na.rm = TRUE)),
      Bias = mean(Prediction - Actual, na.rm = TRUE)
    ),
    by = .(Split, CalibrationBin)
  ]

  out[, BinIndex := as.integer(gsub("[^0-9]", "", CalibrationBin))]
  data.table::setorder(out, BinIndex)
  out
}

model_insights_build_regression_calibration_artifacts <- function(
    inputs,
    scored_train,
    scored_test,
    SampleSize = 100000L,
    CalibrationBins = 21L,
    Theme = "dark"
) {

  tables <- list(
    calibration_bins_train = model_insights_build_calibration_table(scored_train, "Train", CalibrationBins),
    calibration_bins_test = model_insights_build_calibration_table(scored_test, "Test", CalibrationBins)
  )

  calibration_error_summary <- data.table::rbindlist(
    list(tables$calibration_bins_train, tables$calibration_bins_test),
    fill = TRUE
  )[
    ,
    .(
      MeanAbsCalibrationBias = mean(abs(Bias), na.rm = TRUE),
      MaxAbsCalibrationBias = max(abs(Bias), na.rm = TRUE),
      MeanBinRMSE = mean(RMSE, na.rm = TRUE),
      MaxBinRMSE = max(RMSE, na.rm = TRUE)
    ),
    by = Split
  ]

  tables$calibration_error_summary <- calibration_error_summary

  plots <- list()

  make_calibration_plots <- function(raw_dt, scored, cal_table, split_label) {
    out <- list()

    if (!is.null(raw_dt) && model_insights_has_rows(raw_dt)) {
      out$line <- model_insights_try_plot(
        AutoPlots::Calibration.Line(
          dt = raw_dt,
          AggMethod = "mean",
          XVar = inputs$PredictionColumnName,
          YVar = inputs$TargetColumnName,
          NumberBins = CalibrationBins,
          title.text = paste0("Calibration Line Plot: ", split_label, " Data"),
          yAxis.title = inputs$TargetColumnName,
          xAxis.title = inputs$PredictionColumnName,
          Theme = Theme,
          TimeLine = TRUE
        )
      )

      out$box <- model_insights_try_plot(
        AutoPlots::Calibration.Box(
          dt = raw_dt,
          SampleSize = SampleSize,
          AggMethod = "mean",
          XVar = inputs$PredictionColumnName,
          YVar = inputs$TargetColumnName,
          NumberBins = CalibrationBins,
          title.text = paste0("Calibration Plot: ", split_label, " Data"),
          yAxis.title = inputs$TargetColumnName,
          xAxis.title = inputs$PredictionColumnName,
          Theme = Theme,
          TimeLine = TRUE
        )
      )
    }

    if (!is.null(cal_table) && nrow(cal_table)) {
      out$bias_by_prediction_bin <- model_insights_try_plot(
        AutoPlots::Line(
          dt = cal_table,
          XVar = "BinIndex",
          YVar = "Bias",
          title.text = paste0("Calibration Bias by Prediction Bin: ", split_label, " Data"),
          xAxis.title = "Prediction Bin",
          yAxis.title = "Bias",
          Theme = Theme
        )
      )

      out$rmse_by_prediction_bin <- model_insights_try_plot(
        AutoPlots::Line(
          dt = cal_table,
          XVar = "BinIndex",
          YVar = "RMSE",
          title.text = paste0("RMSE by Prediction Bin: ", split_label, " Data"),
          xAxis.title = "Prediction Bin",
          yAxis.title = "RMSE",
          Theme = Theme
        )
      )
    }

    out
  }

  plots$train <- make_calibration_plots(inputs$TrainData, scored_train, tables$calibration_bins_train, "Train")
  plots$test <- make_calibration_plots(inputs$TestData, scored_test, tables$calibration_bins_test, "Test")

  list(tables = tables, plots = plots)
}


# ============================================================
# Error Analysis
# ============================================================

model_insights_build_regression_error_analysis_artifacts <- function(
    inputs,
    scored_train,
    scored_test,
    MaxTopErrors = 100L,
    Theme = "dark"
) {

  top_errors <- function(scored, split, type = c("abs", "positive", "negative")) {
    type <- match.arg(type)

    if (is.null(scored) || !nrow(scored)) return(data.table::data.table())

    dt <- data.table::copy(scored[model_insights_finite(Residual)])

    if (!nrow(dt)) return(data.table::data.table())

    if (type == "abs") {
      data.table::setorder(dt, -AbsResidual)
    } else if (type == "positive") {
      dt <- dt[Residual > 0]
      data.table::setorder(dt, -Residual)
    } else {
      dt <- dt[Residual < 0]
      data.table::setorder(dt, Residual)
    }

    utils::head(dt, MaxTopErrors)
  }

  error_decile <- function(scored, split) {
    if (is.null(scored) || !nrow(scored)) return(data.table::data.table())

    dt <- data.table::copy(scored[model_insights_finite(AbsResidual)])
    if (!nrow(dt)) return(data.table::data.table())

    dt[, AbsErrorBin := model_insights_make_bins(AbsResidual, 10L, "AbsErrBin")]

    dt[
      !is.na(AbsErrorBin),
      .(
        N = .N,
        MeanActual = mean(Actual, na.rm = TRUE),
        MeanPrediction = mean(Prediction, na.rm = TRUE),
        MeanResidual = mean(Residual, na.rm = TRUE),
        MeanAbsResidual = mean(AbsResidual, na.rm = TRUE),
        RMSE = sqrt(mean(Residual^2, na.rm = TRUE))
      ),
      by = .(Split, AbsErrorBin)
    ][
      ,
      BinIndex := as.integer(gsub("[^0-9]", "", AbsErrorBin))
    ][order(BinIndex)]
  }

  tables <- list(
    largest_absolute_errors_train = top_errors(scored_train, "Train", "abs"),
    largest_absolute_errors_test = top_errors(scored_test, "Test", "abs"),
    largest_positive_errors_train = top_errors(scored_train, "Train", "positive"),
    largest_positive_errors_test = top_errors(scored_test, "Test", "positive"),
    largest_negative_errors_train = top_errors(scored_train, "Train", "negative"),
    largest_negative_errors_test = top_errors(scored_test, "Test", "negative"),
    error_decile_summary_train = error_decile(scored_train, "Train"),
    error_decile_summary_test = error_decile(scored_test, "Test")
  )

  plots <- list()

  make_error_plots <- function(scored, error_decile_table, split_label) {
    out <- list()

    if (!is.null(scored) && nrow(scored)) {
      out$absolute_error_distribution <- model_insights_try_plot(
        AutoPlots::Histogram(
          dt = scored[model_insights_finite(AbsResidual)],
          XVar = "AbsResidual",
          title.text = paste0("Absolute Error Distribution: ", split_label, " Data"),
          xAxis.title = "Absolute Error",
          yAxis.title = "Count",
          Theme = Theme
        )
      )
    }

    if (!is.null(error_decile_table) && nrow(error_decile_table)) {
      out$error_decile_mae <- model_insights_try_plot(
        AutoPlots::Bar(
          dt = error_decile_table,
          XVar = "AbsErrorBin",
          YVar = "MeanAbsResidual",
          title.text = paste0("Mean Absolute Error by Error Decile: ", split_label, " Data"),
          xAxis.title = "Absolute Error Decile",
          yAxis.title = "Mean Absolute Error",
          Theme = Theme
        )
      )
    }

    out
  }

  plots$train <- make_error_plots(scored_train, tables$error_decile_summary_train, "Train")
  plots$test <- make_error_plots(scored_test, tables$error_decile_summary_test, "Test")

  list(tables = tables, plots = plots)
}


# ============================================================
# Variable Importance and Interaction Importance
# ============================================================

model_insights_merge_importance_tables <- function(x, keys) {
  x <- Filter(Negate(is.null), x)

  if (!length(x)) return(NULL)

  x <- lapply(x, data.table::as.data.table)

  Reduce(function(a, b) {
    merge(a, b, by = keys, all = TRUE)
  }, x)
}

model_insights_add_importance_summary <- function(dt, importance_cols = NULL, key_cols = "Variable") {
  if (is.null(dt) || !nrow(dt)) return(data.table::data.table())

  dt <- data.table::copy(data.table::as.data.table(dt))

  if (is.null(importance_cols)) {
    importance_cols <- grep("Importance$", names(dt), value = TRUE)
  }

  importance_cols <- intersect(importance_cols, names(dt))

  if (!length(importance_cols)) return(dt)

  for (cc in importance_cols) {
    dt[, (cc) := model_insights_safe_numeric(get(cc))]
  }

  dt[, Mean_Importance := rowMeans(.SD, na.rm = TRUE), .SDcols = importance_cols]
  dt[, Max_Importance := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = importance_cols]
  dt[, Min_Importance := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = importance_cols]
  dt[, SD_Importance := apply(.SD, 1L, stats::sd, na.rm = TRUE), .SDcols = importance_cols]
  dt[, CV_Importance := data.table::fifelse(Mean_Importance == 0, NA_real_, SD_Importance / abs(Mean_Importance))]

  for (cc in importance_cols) {
    rank_col <- paste0("Rank_", sub("_Importance$", "", cc))
    dt[, (rank_col) := data.table::frank(-get(cc), ties.method = "average", na.last = "keep")]
  }

  rank_cols <- grep("^Rank_", names(dt), value = TRUE)
  if (length(rank_cols)) {
    dt[, Mean_Rank := rowMeans(.SD, na.rm = TRUE), .SDcols = rank_cols]
    dt[, Rank_SD := apply(.SD, 1L, stats::sd, na.rm = TRUE), .SDcols = rank_cols]
  }

  data.table::setorder(dt, -Mean_Importance)
  dt
}

model_insights_build_regression_importance_artifacts <- function(
    inputs,
    MaxInteractionRows = 200L,
    Theme = "dark"
) {

  Algo <- tolower(as.character(inputs$Algo))

  if (!is.null(inputs$ModelObject)) {

    if (identical(Algo, "catboost")) {
      Test_Importance <- inputs$ModelObject[["VariableImportance"]][["Test_Importance"]]
      Validation_Importance <- inputs$ModelObject[["VariableImportance"]][["Validation_Importance"]]
      Train_Importance <- inputs$ModelObject[["VariableImportance"]][["Train_Importance"]]

      if (!is.null(Test_Importance)) data.table::setnames(Test_Importance, old = "Importance", new = "Test_Importance", skip_absent = TRUE)
      if (!is.null(Validation_Importance)) data.table::setnames(Validation_Importance, old = "Importance", new = "Validation_Importance", skip_absent = TRUE)
      if (!is.null(Train_Importance)) data.table::setnames(Train_Importance, old = "Importance", new = "Train_Importance", skip_absent = TRUE)

      All_Importance <- model_insights_merge_importance_tables(
        list(Test_Importance, Validation_Importance, Train_Importance),
        keys = "Variable"
      )

    } else {
      Test_Importance <- inputs$ModelObject[["VariableImportance"]]
      Validation_Importance <- NULL
      Train_Importance <- NULL

      if (!is.null(Test_Importance)) {
        Test_Importance <- data.table::as.data.table(Test_Importance)

        if (Algo %in% c("xgboost", "lightgbm")) {
          if (ncol(Test_Importance) >= 2L) {
            data.table::setnames(Test_Importance, old = names(Test_Importance)[2L], new = "Test_Importance", skip_absent = TRUE)
          }
        } else {
          if (ncol(Test_Importance) >= 3L) {
            data.table::setnames(Test_Importance, old = names(Test_Importance)[3L], new = "Test_Importance", skip_absent = TRUE)
          }
        }
      }

      All_Importance <- Test_Importance
    }

  } else {
    All_Importance <- NULL
  }

  variable_importance <- if (!is.null(All_Importance)) {
    model_insights_add_importance_summary(All_Importance, key_cols = "Variable")
  } else {
    data.table::data.table()
  }

  plots <- list()

  if (nrow(variable_importance) && "Variable" %in% names(variable_importance)) {
    y_col <- if ("Mean_Importance" %in% names(variable_importance)) "Mean_Importance" else {
      grep("Importance$", names(variable_importance), value = TRUE)[1L]
    }

    if (!is.na(y_col) && y_col %in% names(variable_importance)) {
      top_vi <- utils::head(variable_importance[order(-get(y_col))], 50L)
      top_vi <- aq_report_sort_for_flipped_bar(top_vi, y_col, "Variable")

      plots$variable_importance_bar <- model_insights_try_plot(
        AutoPlots::Bar(
          dt = top_vi,
          XVar = "Variable",
          YVar = y_col,
          title.text = "Variable Importance",
          xAxis.title = "Variable",
          yAxis.title = "Importance",
          Theme = Theme
        ) |>
          echarts4r::e_flip_coords()
      )

      cumulative <- data.table::copy(variable_importance[order(-get(y_col))])
      cumulative[, ImportanceShare := get(y_col) / sum(get(y_col), na.rm = TRUE)]
      cumulative[, CumulativeImportance := cumsum(ImportanceShare)]
      cumulative[, VariableRank := .I]

      plots$cumulative_importance <- model_insights_try_plot(
        AutoPlots::Line(
          dt = cumulative,
          XVar = "VariableRank",
          YVar = "CumulativeImportance",
          title.text = "Cumulative Variable Importance",
          xAxis.title = "Variable Rank",
          yAxis.title = "Cumulative Importance Share",
          Theme = Theme
        )
      )
    }
  }

  list(
    tables = list(
      variable_importance = variable_importance,
      top_n_importance = if (nrow(variable_importance)) utils::head(variable_importance, 50L) else data.table::data.table()
    ),
    plots = plots
  )
}

model_insights_build_regression_interaction_artifacts <- function(
    inputs,
    MaxInteractionRows = 200L,
    Theme = "dark"
) {

  Algo <- tolower(as.character(inputs$Algo))

  if (!is.null(inputs$ModelObject) && identical(Algo, "catboost")) {
    Test_Interaction <- inputs$ModelObject[["InteractionImportance"]][["Test_Interaction"]]
    Validation_Interaction <- inputs$ModelObject[["InteractionImportance"]][["Validation_Interaction"]]
    Train_Interaction <- inputs$ModelObject[["InteractionImportance"]][["Train_Interaction"]]

    if (!is.null(Test_Interaction)) data.table::setnames(Test_Interaction, old = "score", new = "Test_Importance", skip_absent = TRUE)
    if (!is.null(Validation_Interaction)) data.table::setnames(Validation_Interaction, old = "score", new = "Validation_Importance", skip_absent = TRUE)
    if (!is.null(Train_Interaction)) data.table::setnames(Train_Interaction, old = "score", new = "Train_Importance", skip_absent = TRUE)

    All_Interaction <- model_insights_merge_importance_tables(
      list(Test_Interaction, Validation_Interaction, Train_Interaction),
      keys = c("Features1", "Features2")
    )

  } else {
    All_Interaction <- NULL
  }

  interaction_importance <- if (!is.null(All_Interaction)) {
    out <- model_insights_add_importance_summary(All_Interaction, key_cols = c("Features1", "Features2"))
    out[, Pair := paste(Features1, Features2, sep = " × ")]
    data.table::setorder(out, -Mean_Importance)
    if (nrow(out) > MaxInteractionRows) out <- out[seq_len(MaxInteractionRows)]
    out
  } else {
    data.table::data.table()
  }

  degree <- data.table::data.table()

  if (nrow(interaction_importance)) {
    degree <- data.table::rbindlist(
      list(
        interaction_importance[, .(Feature = Features1, Importance = Mean_Importance)],
        interaction_importance[, .(Feature = Features2, Importance = Mean_Importance)]
      ),
      fill = TRUE
    )[
      ,
      .(
        InteractionCount = .N,
        TotalInteractionImportance = sum(Importance, na.rm = TRUE),
        MeanInteractionImportance = mean(Importance, na.rm = TRUE)
      ),
      by = Feature
    ][order(-TotalInteractionImportance)]
  }

  plots <- list()

  if (nrow(interaction_importance)) {
    top_int <- utils::head(interaction_importance, 50L)
    top_int <- aq_report_sort_for_flipped_bar(top_int, "Mean_Importance", "Pair")

    plots$interaction_importance_bar <- model_insights_try_plot(
      AutoPlots::Bar(
        dt = top_int,
        XVar = "Pair",
        YVar = "Mean_Importance",
        title.text = "CatBoost Interaction Importance",
        xAxis.title = "Feature Pair",
        yAxis.title = "Mean Importance",
        Theme = Theme
      ) |>
        echarts4r::e_flip_coords()
    )
  }

  if (nrow(degree)) {
    degree_plot_data <- aq_report_sort_for_flipped_bar(utils::head(degree, 50L), "TotalInteractionImportance", "Feature")

    plots$feature_interaction_degree_bar <- model_insights_try_plot(
      AutoPlots::Bar(
        dt = degree_plot_data,
        XVar = "Feature",
        YVar = "TotalInteractionImportance",
        title.text = "Feature Interaction Degree",
        xAxis.title = "Feature",
        yAxis.title = "Total Interaction Importance",
        Theme = Theme
      ) |>
        echarts4r::e_flip_coords()
    )
  }

  list(
    tables = list(
      interaction_importance = interaction_importance,
      feature_interaction_degree = degree
    ),
    plots = plots
  )
}


# ============================================================
# Calibration-style Partial Dependence
# ============================================================

model_insights_build_calibration_by_feature_artifacts <- function(
    inputs,
    SampleSize = 100000L,
    NumberBins = 20L,
    MaxPDPFeatures = 50L,
    Theme = "dark"
) {

  numeric_features <- inputs$FeatureColumnNames[
    vapply(inputs$FeatureColumnNames, function(v) {
      (!is.null(inputs$TestData) && v %in% names(inputs$TestData) && model_insights_is_numeric_col(inputs$TestData[[v]])) ||
        (!is.null(inputs$TrainData) && v %in% names(inputs$TrainData) && model_insights_is_numeric_col(inputs$TrainData[[v]]))
    }, logical(1L))
  ]

  categorical_features <- inputs$FeatureColumnNames[
    vapply(inputs$FeatureColumnNames, function(v) {
      (!is.null(inputs$TestData) && v %in% names(inputs$TestData) && model_insights_is_categorical_col(inputs$TestData[[v]])) ||
        (!is.null(inputs$TrainData) && v %in% names(inputs$TrainData) && model_insights_is_categorical_col(inputs$TrainData[[v]]))
    }, logical(1L))
  ]

  numeric_features <- utils::head(numeric_features, MaxPDPFeatures)
  categorical_features <- utils::head(categorical_features, MaxPDPFeatures)

  plots <- list(
    numeric_line = list(test = list(), train = list()),
    numeric_box = list(test = list(), train = list()),
    categorical_heatmap = list(test = list(), train = list())
  )

  failures <- data.table::data.table()

  add_failure <- function(variable, split, plot_type, error_message) {
    data.table::data.table(
      Variable = variable,
      Split = split,
      PlotType = plot_type,
      Error = error_message
    )
  }

  for (g in numeric_features) {
    if (!is.null(inputs$TestData) && g %in% names(inputs$TestData) && model_insights_is_numeric_col(inputs$TestData[[g]])) {

      plots$numeric_line$test[[g]] <- tryCatch(
        AutoPlots::PartialDependence.Line(
          dt = inputs$TestData,
          XVar = g,
          YVar = inputs$TargetColumnName,
          ZVar = inputs$PredictionColumnName,
          NumberBins = NumberBins,
          AggMethod = "mean",
          title.text = paste0("Calibration PDP Line: Test Data - ", g),
          yAxis.title = inputs$TargetColumnName,
          xAxis.title = g,
          Theme = Theme,
          TimeLine = TRUE
        ),
        error = function(e) {
          failures <<- data.table::rbindlist(list(failures, add_failure(g, "Test", "numeric_line", conditionMessage(e))), fill = TRUE)
          NULL
        }
      )

      plots$numeric_box$test[[g]] <- tryCatch(
        AutoPlots::PartialDependence.Box(
          dt = inputs$TestData,
          XVar = g,
          YVar = inputs$TargetColumnName,
          ZVar = inputs$PredictionColumnName,
          NumberBins = NumberBins,
          AggMethod = "mean",
          title.text = paste0("Calibration PDP Box: Test Data - ", g),
          yAxis.title = inputs$TargetColumnName,
          xAxis.title = g,
          Theme = Theme,
          TimeLine = TRUE
        ),
        error = function(e) {
          failures <<- data.table::rbindlist(list(failures, add_failure(g, "Test", "numeric_box", conditionMessage(e))), fill = TRUE)
          NULL
        }
      )
    }

    if (!is.null(inputs$TrainData) && g %in% names(inputs$TrainData) && model_insights_is_numeric_col(inputs$TrainData[[g]])) {

      plots$numeric_line$train[[g]] <- tryCatch(
        AutoPlots::PartialDependence.Line(
          dt = inputs$TrainData,
          XVar = g,
          YVar = inputs$TargetColumnName,
          ZVar = inputs$PredictionColumnName,
          NumberBins = NumberBins,
          AggMethod = "mean",
          title.text = paste0("Calibration PDP Line: Train Data - ", g),
          yAxis.title = inputs$TargetColumnName,
          xAxis.title = g,
          Theme = Theme,
          TimeLine = TRUE
        ),
        error = function(e) {
          failures <<- data.table::rbindlist(list(failures, add_failure(g, "Train", "numeric_line", conditionMessage(e))), fill = TRUE)
          NULL
        }
      )

      plots$numeric_box$train[[g]] <- tryCatch(
        AutoPlots::PartialDependence.Box(
          dt = inputs$TrainData,
          XVar = g,
          YVar = inputs$TargetColumnName,
          ZVar = inputs$PredictionColumnName,
          NumberBins = NumberBins,
          AggMethod = "mean",
          title.text = paste0("Calibration PDP Box: Train Data - ", g),
          yAxis.title = inputs$TargetColumnName,
          xAxis.title = g,
          Theme = Theme,
          TimeLine = TRUE
        ),
        error = function(e) {
          failures <<- data.table::rbindlist(list(failures, add_failure(g, "Train", "numeric_box", conditionMessage(e))), fill = TRUE)
          NULL
        }
      )
    }
  }

  for (g in categorical_features) {
    if (!is.null(inputs$TestData) && g %in% names(inputs$TestData) && model_insights_is_categorical_col(inputs$TestData[[g]])) {
      plots$categorical_heatmap$test[[g]] <- tryCatch(
        AutoPlots::PartialDependence.HeatMap(
          dt = inputs$TestData,
          XVar = g,
          YVar = inputs$TargetColumnName,
          ZVar = inputs$PredictionColumnName,
          NumberBins = NumberBins,
          AggMethod = "mean",
          title.text = paste0("Calibration PDP Heatmap: Test Data - ", g),
          yAxis.title = inputs$TargetColumnName,
          xAxis.title = g,
          Theme = Theme,
          TimeLine = TRUE,
          xAxis.axisLabel.rotate = 45,
          tooltip.show = aq_report_dense_axis_tooltip(data.table::as.data.table(inputs$TestData), g, inputs$TargetColumnName)
        ),
        error = function(e) {
          failures <<- data.table::rbindlist(list(failures, add_failure(g, "Test", "categorical_heatmap", conditionMessage(e))), fill = TRUE)
          NULL
        }
      )
    }

    if (!is.null(inputs$TrainData) && g %in% names(inputs$TrainData) && model_insights_is_categorical_col(inputs$TrainData[[g]])) {
      plots$categorical_heatmap$train[[g]] <- tryCatch(
        AutoPlots::PartialDependence.HeatMap(
          dt = inputs$TrainData,
          XVar = g,
          YVar = inputs$TargetColumnName,
          ZVar = inputs$PredictionColumnName,
          NumberBins = NumberBins,
          AggMethod = "mean",
          title.text = paste0("Calibration PDP Heatmap: Train Data - ", g),
          yAxis.title = inputs$TargetColumnName,
          xAxis.title = g,
          Theme = Theme,
          TimeLine = TRUE,
          xAxis.axisLabel.rotate = 45,
          tooltip.show = aq_report_dense_axis_tooltip(data.table::as.data.table(inputs$TrainData), g, inputs$TargetColumnName)
        ),
        error = function(e) {
          failures <<- data.table::rbindlist(list(failures, add_failure(g, "Train", "categorical_heatmap", conditionMessage(e))), fill = TRUE)
          NULL
        }
      )
    }
  }

  tables <- list(
    feature_summary = data.table::data.table(
      Metric = c("Numeric Features", "Categorical Features", "PDP Failures"),
      Count = c(length(numeric_features), length(categorical_features), nrow(failures)),
      Values = c(
        paste(numeric_features, collapse = ", "),
        paste(categorical_features, collapse = ", "),
        if (nrow(failures)) paste(unique(failures$Variable), collapse = ", ") else ""
      )
    ),
    failures = failures
  )

  list(tables = tables, plots = plots)
}


# ============================================================
# Uplift-by-Feature
# ============================================================

model_insights_build_numeric_uplift_table <- function(
    dt,
    split,
    variable,
    target,
    pred,
    bins = 20L,
    baseline = c("lowest_bin", "overall_mean", "median_bin")
) {
  baseline <- match.arg(baseline)

  if (is.null(dt) || !nrow(dt) || !variable %in% names(dt) || !target %in% names(dt) || !pred %in% names(dt)) {
    return(data.table::data.table())
  }

  data <- data.table::copy(data.table::as.data.table(dt))
  data <- data[model_insights_finite(get(variable)) & model_insights_finite(get(pred))]

  if (!nrow(data)) return(data.table::data.table())

  data[, FeatureBin := model_insights_make_bins(get(variable), bins, "FeatureBin")]

  out <- data[
    !is.na(FeatureBin),
    .(
      N = .N,
      BinMin = min(get(variable), na.rm = TRUE),
      BinMax = max(get(variable), na.rm = TRUE),
      BinMid = mean(get(variable), na.rm = TRUE),
      ActualMean = mean(model_insights_safe_numeric(get(target)), na.rm = TRUE),
      PredictionMean = mean(model_insights_safe_numeric(get(pred)), na.rm = TRUE),
      PredictionMedian = stats::median(model_insights_safe_numeric(get(pred)), na.rm = TRUE),
      ResidualMean = mean(model_insights_safe_numeric(get(target)) - model_insights_safe_numeric(get(pred)), na.rm = TRUE),
      AbsResidualMean = mean(abs(model_insights_safe_numeric(get(target)) - model_insights_safe_numeric(get(pred))), na.rm = TRUE)
    ),
    by = FeatureBin
  ]

  out[, BinIndex := as.integer(gsub("[^0-9]", "", FeatureBin))]
  data.table::setorder(out, BinIndex)

  baseline_prediction <- if (baseline == "overall_mean") {
    mean(out$PredictionMean, na.rm = TRUE)
  } else if (baseline == "median_bin") {
    out$PredictionMean[which.min(abs(out$BinIndex - stats::median(out$BinIndex, na.rm = TRUE)))]
  } else {
    out$PredictionMean[which.min(out$BinIndex)]
  }

  baseline_actual <- if (baseline == "overall_mean") {
    mean(out$ActualMean, na.rm = TRUE)
  } else if (baseline == "median_bin") {
    out$ActualMean[which.min(abs(out$BinIndex - stats::median(out$BinIndex, na.rm = TRUE)))]
  } else {
    out$ActualMean[which.min(out$BinIndex)]
  }

  prediction_uplift_pct <- if (is.na(baseline_prediction) || baseline_prediction == 0) {
    rep(NA_real_, nrow(out))
  } else {
    (out$PredictionMean - baseline_prediction) / abs(baseline_prediction)
  }

  out[, `:=`(
    Split = split,
    Variable = variable,
    VariableType = "numeric",
    BaselinePrediction = baseline_prediction,
    PredictionUplift = PredictionMean - baseline_prediction,
    PredictionUpliftPct = prediction_uplift_pct,
    BaselineActual = baseline_actual,
    ActualUplift = ActualMean - baseline_actual,
    Bias = PredictionMean - ActualMean
  )]

  data.table::setcolorder(
    out,
    c(
      "Split", "Variable", "VariableType", "FeatureBin", "BinIndex", "BinMin", "BinMax", "BinMid",
      "N", "ActualMean", "PredictionMean", "PredictionMedian", "ResidualMean", "AbsResidualMean",
      "BaselinePrediction", "PredictionUplift", "PredictionUpliftPct",
      "BaselineActual", "ActualUplift", "Bias"
    )
  )

  out[]
}

model_insights_build_categorical_uplift_table <- function(
    dt,
    split,
    variable,
    target,
    pred,
    max_levels = 25L,
    reference_levels = NULL
) {
  if (is.null(dt) || !nrow(dt) || !variable %in% names(dt) || !target %in% names(dt) || !pred %in% names(dt)) {
    return(data.table::data.table())
  }

  data <- data.table::copy(data.table::as.data.table(dt))
  data <- data[!is.na(get(variable)) & model_insights_finite(get(pred))]
  if (!nrow(data)) return(data.table::data.table())

  data[, FeatureLevel := as.character(get(variable))]

  level_counts <- data[, .N, by = FeatureLevel][order(-N)]
  keep_levels <- level_counts$FeatureLevel[seq_len(min(nrow(level_counts), max_levels))]

  data[!FeatureLevel %in% keep_levels, FeatureLevel := "Other"]

  out <- data[
    ,
    .(
      N = .N,
      ActualMean = mean(model_insights_safe_numeric(get(target)), na.rm = TRUE),
      PredictionMean = mean(model_insights_safe_numeric(get(pred)), na.rm = TRUE),
      PredictionMedian = stats::median(model_insights_safe_numeric(get(pred)), na.rm = TRUE),
      ResidualMean = mean(model_insights_safe_numeric(get(target)) - model_insights_safe_numeric(get(pred)), na.rm = TRUE),
      AbsResidualMean = mean(abs(model_insights_safe_numeric(get(target)) - model_insights_safe_numeric(get(pred))), na.rm = TRUE)
    ),
    by = FeatureLevel
  ][order(-N)]

  ref_level <- NULL

  if (!is.null(reference_levels) && variable %in% names(reference_levels)) {
    ref_level <- as.character(reference_levels[[variable]])
  }

  if (is.null(ref_level) || !ref_level %in% out$FeatureLevel) {
    ref_level <- out$FeatureLevel[which.max(out$N)]
  }

  baseline_prediction <- out[FeatureLevel == ref_level, PredictionMean][1L]
  baseline_actual <- out[FeatureLevel == ref_level, ActualMean][1L]

  prediction_uplift_pct <- if (is.na(baseline_prediction) || baseline_prediction == 0) {
    rep(NA_real_, nrow(out))
  } else {
    (out$PredictionMean - baseline_prediction) / abs(baseline_prediction)
  }

  out[, `:=`(
    Split = split,
    Variable = variable,
    VariableType = "categorical",
    ReferenceLevel = ref_level,
    BaselinePrediction = baseline_prediction,
    PredictionUplift = PredictionMean - baseline_prediction,
    PredictionUpliftPct = prediction_uplift_pct,
    BaselineActual = baseline_actual,
    ActualUplift = ActualMean - baseline_actual,
    Bias = PredictionMean - ActualMean
  )]

  data.table::setcolorder(
    out,
    c(
      "Split", "Variable", "VariableType", "FeatureLevel", "N",
      "ActualMean", "PredictionMean", "PredictionMedian", "ResidualMean", "AbsResidualMean",
      "ReferenceLevel", "BaselinePrediction", "PredictionUplift", "PredictionUpliftPct",
      "BaselineActual", "ActualUplift", "Bias"
    )
  )

  out[]
}

model_insights_build_uplift_summary <- function(uplift_numeric, uplift_categorical, min_effect_size = NULL) {

  num_summary <- if (!is.null(uplift_numeric) && nrow(uplift_numeric)) {
    uplift_numeric[
      ,
      .(
        N = sum(N, na.rm = TRUE),
        LevelsOrBins = data.table::uniqueN(FeatureBin),
        MinPredictionMean = min(PredictionMean, na.rm = TRUE),
        MaxPredictionMean = max(PredictionMean, na.rm = TRUE),
        PredictionRange = max(PredictionMean, na.rm = TRUE) - min(PredictionMean, na.rm = TRUE),
        MinActualMean = min(ActualMean, na.rm = TRUE),
        MaxActualMean = max(ActualMean, na.rm = TRUE),
        ActualRange = max(ActualMean, na.rm = TRUE) - min(ActualMean, na.rm = TRUE),
        BiasRange = max(Bias, na.rm = TRUE) - min(Bias, na.rm = TRUE),
        FirstPrediction = PredictionMean[which.min(BinIndex)],
        LastPrediction = PredictionMean[which.max(BinIndex)],
        Effect = PredictionMean[which.max(BinIndex)] - PredictionMean[which.min(BinIndex)]
      ),
      by = .(Split, Variable, VariableType)
    ]
  } else {
    data.table::data.table()
  }

  cat_summary <- if (!is.null(uplift_categorical) && nrow(uplift_categorical)) {
    uplift_categorical[
      ,
      .(
        N = sum(N, na.rm = TRUE),
        LevelsOrBins = data.table::uniqueN(FeatureLevel),
        MinPredictionMean = min(PredictionMean, na.rm = TRUE),
        MaxPredictionMean = max(PredictionMean, na.rm = TRUE),
        PredictionRange = max(PredictionMean, na.rm = TRUE) - min(PredictionMean, na.rm = TRUE),
        MinActualMean = min(ActualMean, na.rm = TRUE),
        MaxActualMean = max(ActualMean, na.rm = TRUE),
        ActualRange = max(ActualMean, na.rm = TRUE) - min(ActualMean, na.rm = TRUE),
        BiasRange = max(Bias, na.rm = TRUE) - min(Bias, na.rm = TRUE),
        TopLevel = FeatureLevel[which.max(PredictionMean)],
        BottomLevel = FeatureLevel[which.min(PredictionMean)],
        Effect = max(PredictionMean, na.rm = TRUE) - min(PredictionMean, na.rm = TRUE)
      ),
      by = .(Split, Variable, VariableType)
    ]
  } else {
    data.table::data.table()
  }

  out <- data.table::rbindlist(list(num_summary, cat_summary), fill = TRUE)

  if (nrow(out)) {
    out[, Direction := model_insights_metric_direction(Effect, min_effect_size)]
    out[, PredictionRangePct := data.table::fifelse(
      is.na(MinPredictionMean) | MinPredictionMean == 0,
      NA_real_,
      PredictionRange / abs(MinPredictionMean)
    )]
    data.table::setorder(out, -PredictionRange)
  }

  out
}

model_insights_build_uplift_by_feature_artifacts <- function(
    inputs,
    UpliftBins = 20L,
    MaxPDPFeatures = 50L,
    MaxCategoricalLevels = 25L,
    ReferenceLevels = NULL,
    PDPBaseline = "lowest_bin",
    MinEffectSize = NULL,
    Theme = "dark"
) {

  build_split <- function(dt, split) {
    if (is.null(dt) || !nrow(dt)) {
      return(list(numeric = data.table::data.table(), categorical = data.table::data.table()))
    }

    features <- intersect(inputs$FeatureColumnNames, names(dt))
    features <- utils::head(features, MaxPDPFeatures)

    numeric_features <- features[vapply(features, function(v) model_insights_is_numeric_col(dt[[v]]), logical(1L))]
    categorical_features <- features[vapply(features, function(v) model_insights_is_categorical_col(dt[[v]]), logical(1L))]

    num <- data.table::rbindlist(
      lapply(numeric_features, function(v) {
        model_insights_build_numeric_uplift_table(
          dt = dt,
          split = split,
          variable = v,
          target = inputs$TargetColumnName,
          pred = inputs$PredictionColumnName,
          bins = UpliftBins,
          baseline = PDPBaseline
        )
      }),
      fill = TRUE
    )

    cat <- data.table::rbindlist(
      lapply(categorical_features, function(v) {
        model_insights_build_categorical_uplift_table(
          dt = dt,
          split = split,
          variable = v,
          target = inputs$TargetColumnName,
          pred = inputs$PredictionColumnName,
          max_levels = MaxCategoricalLevels,
          reference_levels = ReferenceLevels
        )
      }),
      fill = TRUE
    )

    list(numeric = num, categorical = cat)
  }

  train <- build_split(inputs$TrainData, "Train")
  test <- build_split(inputs$TestData, "Test")

  numeric <- data.table::rbindlist(list(train$numeric, test$numeric), fill = TRUE)
  categorical <- data.table::rbindlist(list(train$categorical, test$categorical), fill = TRUE)

  summary <- model_insights_build_uplift_summary(numeric, categorical, MinEffectSize)

  plots <- list(
    numeric_line = list(train = list(), test = list()),
    categorical_bar = list(train = list(), test = list())
  )

  if (nrow(numeric)) {
    for (sp in unique(numeric$Split)) {
      for (v in unique(numeric[Split == sp]$Variable)) {
        pd <- numeric[Split == sp & Variable == v][order(BinIndex)]

        split_name <- tolower(sp)

        plots$numeric_line[[split_name]][[v]] <- model_insights_try_plot(
          AutoPlots::Line(
            dt = pd,
            XVar = "BinMid",
            YVar = "PredictionUplift",
            title.text = paste0("Predicted Uplift by ", v, ": ", sp, " Data"),
            xAxis.title = v,
            yAxis.title = "Predicted Uplift",
            Theme = Theme
          )
        )
      }
    }
  }

  if (nrow(categorical)) {
    for (sp in unique(categorical$Split)) {
      for (v in unique(categorical[Split == sp]$Variable)) {
        pd <- categorical[Split == sp & Variable == v][order(-PredictionUplift)]
        pd <- aq_report_sort_for_flipped_bar(pd, "PredictionUplift", "FeatureLevel")

        split_name <- tolower(sp)

        plots$categorical_bar[[split_name]][[v]] <- model_insights_try_plot(
          AutoPlots::Bar(
            dt = pd,
            XVar = "FeatureLevel",
            YVar = "PredictionUplift",
            title.text = paste0("Predicted Uplift by ", v, ": ", sp, " Data"),
            xAxis.title = v,
            yAxis.title = "Predicted Uplift",
            Theme = Theme
          ) |>
            echarts4r::e_flip_coords()
        )
      }
    }
  }

  if (nrow(summary)) {
    effect_range_plot_data <- aq_report_sort_for_flipped_bar(utils::head(summary[order(-PredictionRange)], 50L), "PredictionRange", "Variable")

    plots$effect_range_bar <- model_insights_try_plot(
      AutoPlots::Bar(
        dt = effect_range_plot_data,
        XVar = "Variable",
        YVar = "PredictionRange",
        title.text = "Largest Modeled Prediction Ranges by Feature",
        xAxis.title = "Feature",
        yAxis.title = "Prediction Range",
        Theme = Theme
      ) |>
        echarts4r::e_flip_coords()
    )
  }

  list(
    tables = list(
      numeric = numeric,
      categorical = categorical,
      summary = summary,
      top_positive_uplift_features = if (nrow(summary)) utils::head(summary[order(-Effect)], 25L) else data.table::data.table(),
      top_negative_uplift_features = if (nrow(summary)) utils::head(summary[order(Effect)], 25L) else data.table::data.table(),
      largest_effect_range = if (nrow(summary)) utils::head(summary[order(-PredictionRange)], 25L) else data.table::data.table()
    ),
    plots = plots
  )
}


# ============================================================
# Stratified Effects and Simpson's Paradox
# ============================================================

model_insights_build_stratified_numeric_effect <- function(
    dt,
    split,
    feature,
    by_var,
    target,
    pred,
    bins = 10L,
    min_group_n = 50L
) {
  if (is.null(dt) ||
      !nrow(dt) ||
      !all(c(feature, by_var, target, pred) %in% names(dt)) ||
      !model_insights_is_numeric_col(dt[[feature]])) {
    return(data.table::data.table())
  }

  data <- data.table::copy(data.table::as.data.table(dt))
  data <- data[
    !is.na(get(by_var)) &
      model_insights_finite(get(feature)) &
      model_insights_finite(get(pred))
  ]

  if (!nrow(data)) return(data.table::data.table())

  data[, FeatureBin := model_insights_make_bins(get(feature), bins, "FeatureBin")]
  data[, ByLevel := as.character(get(by_var))]

  agg <- data[
    !is.na(FeatureBin),
    .(
      N = .N,
      BinMid = mean(get(feature), na.rm = TRUE),
      PredictionMean = mean(model_insights_safe_numeric(get(pred)), na.rm = TRUE),
      ActualMean = mean(model_insights_safe_numeric(get(target)), na.rm = TRUE)
    ),
    by = .(ByLevel, FeatureBin)
  ]

  agg[, BinIndex := as.integer(gsub("[^0-9]", "", FeatureBin))]

  out <- agg[
    ,
    {
      low <- .SD[which.min(BinIndex)]
      high <- .SD[which.max(BinIndex)]

      data.table::data.table(
        N = sum(N, na.rm = TRUE),
        LowBin = low$FeatureBin[1L],
        HighBin = high$FeatureBin[1L],
        LowPrediction = low$PredictionMean[1L],
        HighPrediction = high$PredictionMean[1L],
        GroupEffect = high$PredictionMean[1L] - low$PredictionMean[1L],
        LowActual = low$ActualMean[1L],
        HighActual = high$ActualMean[1L],
        GroupActualEffect = high$ActualMean[1L] - low$ActualMean[1L]
      )
    },
    by = ByLevel
  ][N >= min_group_n]

  if (!nrow(out)) return(data.table::data.table())

  global <- model_insights_build_numeric_uplift_table(
    dt = dt,
    split = split,
    variable = feature,
    target = target,
    pred = pred,
    bins = bins,
    baseline = "lowest_bin"
  )

  global_effect <- if (nrow(global)) {
    global$PredictionMean[which.max(global$BinIndex)] - global$PredictionMean[which.min(global$BinIndex)]
  } else {
    NA_real_
  }

  global_direction <- model_insights_metric_direction(global_effect)

  out[, `:=`(
    Split = split,
    Feature = feature,
    FeatureType = "numeric",
    ByVariable = by_var,
    GlobalEffect = global_effect,
    GlobalDirection = global_direction,
    GroupDirection = model_insights_metric_direction(GroupEffect),
    DirectionMatchesGlobal = sign(global_effect) == sign(GroupEffect),
    EffectDifference = GroupEffect - global_effect
  )]

  data.table::setcolorder(
    out,
    c(
      "Split", "Feature", "FeatureType", "ByVariable", "ByLevel", "N",
      "GlobalEffect", "GlobalDirection", "GroupEffect", "GroupDirection",
      "DirectionMatchesGlobal", "EffectDifference",
      "LowBin", "HighBin", "LowPrediction", "HighPrediction",
      "LowActual", "HighActual", "GroupActualEffect"
    )
  )

  out[]
}

model_insights_build_stratified_categorical_effect <- function(
    dt,
    split,
    feature,
    by_var,
    target,
    pred,
    max_levels = 25L,
    min_group_n = 50L,
    reference_levels = NULL
) {
  if (is.null(dt) ||
      !nrow(dt) ||
      !all(c(feature, by_var, target, pred) %in% names(dt)) ||
      !model_insights_is_categorical_col(dt[[feature]])) {
    return(data.table::data.table())
  }

  data <- data.table::copy(data.table::as.data.table(dt))
  data <- data[
    !is.na(get(by_var)) &
      !is.na(get(feature)) &
      model_insights_finite(get(pred))
  ]

  if (!nrow(data)) return(data.table::data.table())

  data[, ByLevel := as.character(get(by_var))]
  data[, FeatureLevel := as.character(get(feature))]

  global_cat <- model_insights_build_categorical_uplift_table(
    dt = dt,
    split = split,
    variable = feature,
    target = target,
    pred = pred,
    max_levels = max_levels,
    reference_levels = reference_levels
  )

  if (!nrow(global_cat)) return(data.table::data.table())

  global_effect <- max(global_cat$PredictionMean, na.rm = TRUE) - min(global_cat$PredictionMean, na.rm = TRUE)

  out <- data[
    ,
    .(
      N = .N,
      PredictionMean = mean(model_insights_safe_numeric(get(pred)), na.rm = TRUE),
      ActualMean = mean(model_insights_safe_numeric(get(target)), na.rm = TRUE)
    ),
    by = .(ByLevel, FeatureLevel)
  ][
    ,
    .(
      N = sum(N, na.rm = TRUE),
      LowLevel = FeatureLevel[which.min(PredictionMean)],
      HighLevel = FeatureLevel[which.max(PredictionMean)],
      LowPrediction = min(PredictionMean, na.rm = TRUE),
      HighPrediction = max(PredictionMean, na.rm = TRUE),
      GroupEffect = max(PredictionMean, na.rm = TRUE) - min(PredictionMean, na.rm = TRUE),
      LowActual = ActualMean[which.min(PredictionMean)],
      HighActual = ActualMean[which.max(PredictionMean)]
    ),
    by = ByLevel
  ][N >= min_group_n]

  if (!nrow(out)) return(data.table::data.table())

  out[, `:=`(
    Split = split,
    Feature = feature,
    FeatureType = "categorical",
    ByVariable = by_var,
    GlobalEffect = global_effect,
    GlobalDirection = model_insights_metric_direction(global_effect),
    GroupDirection = model_insights_metric_direction(GroupEffect),
    DirectionMatchesGlobal = sign(global_effect) == sign(GroupEffect),
    EffectDifference = GroupEffect - global_effect
  )]

  data.table::setcolorder(
    out,
    c(
      "Split", "Feature", "FeatureType", "ByVariable", "ByLevel", "N",
      "GlobalEffect", "GlobalDirection", "GroupEffect", "GroupDirection",
      "DirectionMatchesGlobal", "EffectDifference",
      "LowLevel", "HighLevel", "LowPrediction", "HighPrediction",
      "LowActual", "HighActual"
    )
  )

  out[]
}

model_insights_detect_simpsons_paradox <- function(
    stratified_effects,
    MinReversalShare = 0.50,
    MinValidGroups = 2L,
    MinEffectSize = NULL
) {
  if (is.null(stratified_effects) || !nrow(stratified_effects)) {
    return(data.table::data.table())
  }

  dt <- data.table::copy(stratified_effects)

  if (is.null(MinEffectSize) || is.na(MinEffectSize)) {
    MinEffectSize <- 0
  }

  dt <- dt[
    !is.na(GlobalEffect) &
      !is.na(GroupEffect) &
      abs(GlobalEffect) >= MinEffectSize &
      abs(GroupEffect) >= MinEffectSize
  ]

  if (!nrow(dt)) return(data.table::data.table())

  dt[, OppositeDirection := sign(GlobalEffect) != sign(GroupEffect)]

  out <- dt[
    ,
    .(
      GlobalEffect = GlobalEffect[1L],
      GlobalDirection = GlobalDirection[1L],
      ValidGroups = .N,
      GroupsSameDirection = sum(!OppositeDirection, na.rm = TRUE),
      GroupsOppositeDirection = sum(OppositeDirection, na.rm = TRUE),
      TotalN = sum(N, na.rm = TRUE),
      OppositeDirectionN = sum(data.table::fifelse(OppositeDirection, N, 0), na.rm = TRUE),
      OppositeDirectionShare = sum(data.table::fifelse(OppositeDirection, N, 0), na.rm = TRUE) / sum(N, na.rm = TRUE)
    ),
    by = .(Split, Feature, FeatureType, ByVariable)
  ]

  out[, SimpsonsFlag := ValidGroups >= MinValidGroups & OppositeDirectionShare >= MinReversalShare]
  out[, Severity := data.table::fifelse(
    OppositeDirectionShare >= 0.75,
    "High",
    data.table::fifelse(OppositeDirectionShare >= 0.50, "Medium",
                        data.table::fifelse(OppositeDirectionShare >= 0.25, "Low", "None"))
  )]

  data.table::setorder(out, -OppositeDirectionShare)
  out[]
}

model_insights_build_stratified_effects_artifacts <- function(
    inputs,
    UpliftBins = 10L,
    MaxByLevels = 10L,
    MaxCategoricalLevels = 25L,
    MinByGroupN = 50L,
    MinEffectSize = NULL,
    MinReversalShare = 0.50,
    MinValidGroups = 2L,
    ReferenceLevels = NULL,
    Theme = "dark"
) {

  split_data <- list(Train = inputs$TrainData, Test = inputs$TestData)

  effects <- data.table::rbindlist(
    unlist(
      lapply(names(split_data), function(split) {
        dt <- split_data[[split]]
        if (is.null(dt) || !nrow(dt)) return(list())

        by_vars <- inputs$ByVars[
          inputs$ByVars %in% names(dt) &
            vapply(inputs$ByVars, function(v) data.table::uniqueN(dt[[v]], na.rm = TRUE) <= MaxByLevels, logical(1L))
        ]

        features <- setdiff(intersect(inputs$FeatureColumnNames, names(dt)), by_vars)

        unlist(
          lapply(by_vars, function(by_var) {
            lapply(features, function(feature) {
              if (model_insights_is_numeric_col(dt[[feature]])) {
                model_insights_build_stratified_numeric_effect(
                  dt = dt,
                  split = split,
                  feature = feature,
                  by_var = by_var,
                  target = inputs$TargetColumnName,
                  pred = inputs$PredictionColumnName,
                  bins = UpliftBins,
                  min_group_n = MinByGroupN
                )
              } else if (model_insights_is_categorical_col(dt[[feature]])) {
                model_insights_build_stratified_categorical_effect(
                  dt = dt,
                  split = split,
                  feature = feature,
                  by_var = by_var,
                  target = inputs$TargetColumnName,
                  pred = inputs$PredictionColumnName,
                  max_levels = MaxCategoricalLevels,
                  min_group_n = MinByGroupN,
                  reference_levels = ReferenceLevels
                )
              } else {
                data.table::data.table()
              }
            })
          }),
          recursive = FALSE
        )
      }),
      recursive = FALSE
    ),
    fill = TRUE
  )

  simpsons <- model_insights_detect_simpsons_paradox(
    effects,
    MinReversalShare = MinReversalShare,
    MinValidGroups = MinValidGroups,
    MinEffectSize = MinEffectSize
  )

  heterogeneity <- if (nrow(effects)) {
    effects[
      ,
      .(
        ValidGroups = .N,
        TotalN = sum(N, na.rm = TRUE),
        GlobalEffect = GlobalEffect[1L],
        MeanGroupEffect = mean(GroupEffect, na.rm = TRUE),
        SDGroupEffect = stats::sd(GroupEffect, na.rm = TRUE),
        MinGroupEffect = min(GroupEffect, na.rm = TRUE),
        MaxGroupEffect = max(GroupEffect, na.rm = TRUE),
        EffectRange = max(GroupEffect, na.rm = TRUE) - min(GroupEffect, na.rm = TRUE)
      ),
      by = .(Split, Feature, FeatureType, ByVariable)
    ][order(-EffectRange)]
  } else {
    data.table::data.table()
  }

  plots <- list()

  if (nrow(simpsons)) {
    simpsons_plot_data <- simpsons[, Feature_ByVariable := paste(Feature, ByVariable, sep = " by ")][]
    simpsons_plot_data <- aq_report_sort_for_flipped_bar(utils::head(simpsons_plot_data[order(-OppositeDirectionShare)], 50L), "OppositeDirectionShare", "Feature_ByVariable")

    plots$simpsons_flags_bar <- model_insights_try_plot(
      AutoPlots::Bar(
        dt = simpsons_plot_data,
        XVar = "Feature_ByVariable",
        YVar = "OppositeDirectionShare",
        title.text = "Possible Simpson's Paradox Flags",
        xAxis.title = "Feature by Segment",
        yAxis.title = "Opposite Direction Share",
        Theme = Theme
      ) |>
        echarts4r::e_flip_coords()
    )
  }

  if (nrow(heterogeneity)) {
    heterogeneity[, Feature_ByVariable := paste(Feature, ByVariable, sep = " by ")]
    heterogeneity_plot_data <- aq_report_sort_for_flipped_bar(utils::head(heterogeneity[order(-EffectRange)], 50L), "EffectRange", "Feature_ByVariable")

    plots$effect_heterogeneity_bar <- model_insights_try_plot(
      AutoPlots::Bar(
        dt = heterogeneity_plot_data,
        XVar = "Feature_ByVariable",
        YVar = "EffectRange",
        title.text = "Effect Heterogeneity by Segment",
        xAxis.title = "Feature by Segment",
        yAxis.title = "Group Effect Range",
        Theme = Theme
      ) |>
        echarts4r::e_flip_coords()
    )
  }

  list(
    tables = list(
      stratified_effects = effects,
      simpsons_paradox_flags = simpsons,
      effect_heterogeneity_summary = heterogeneity
    ),
    plots = plots
  )
}


# ============================================================
# Segment Performance
# ============================================================

model_insights_segment_metrics <- function(scored, segment_var, min_n = 25L) {

  if (is.null(scored) || !nrow(scored) || !segment_var %in% names(scored)) {
    return(data.table::data.table())
  }

  dt <- data.table::copy(scored)

  dt <- dt[
    !is.na(get(segment_var)) &
      model_insights_finite(Actual) &
      model_insights_finite(Prediction)
  ]

  if (!nrow(dt)) {
    return(data.table::data.table())
  }

  dt[, SegmentVariable := segment_var]
  dt[, SegmentLevel := as.character(get(segment_var))]

  out <- dt[
    ,
    {
      r <- Actual - Prediction
      denom_r2 <- sum((Actual - mean(Actual, na.rm = TRUE))^2, na.rm = TRUE)

      data.table::data.table(
        N = .N,
        MeanActual = mean(Actual, na.rm = TRUE),
        MeanPrediction = mean(Prediction, na.rm = TRUE),
        Bias = mean(Prediction - Actual, na.rm = TRUE),
        MAE = mean(abs(r), na.rm = TRUE),
        RMSE = sqrt(mean(r^2, na.rm = TRUE)),
        MAPE = mean(abs(r / Actual), na.rm = TRUE),
        WAPE = sum(abs(r), na.rm = TRUE) / sum(abs(Actual), na.rm = TRUE),
        R2 = ifelse(
          denom_r2 == 0,
          NA_real_,
          1 - sum(r^2, na.rm = TRUE) / denom_r2
        )
      )
    },
    by = .(Split, SegmentVariable, SegmentLevel)
  ]

  out[N >= min_n][]
}

model_insights_build_segment_performance_artifacts <- function(
    inputs,
    scored_train,
    scored_test,
    MaxSegmentLevels = 25L,
    MinSegmentN = 25L,
    Theme = "dark"
) {

  scored <- data.table::rbindlist(list(scored_train, scored_test), fill = TRUE)

  if (!nrow(scored) || !length(inputs$SegmentVars)) {
    return(list(tables = list(), plots = list()))
  }

  segment_vars <- inputs$SegmentVars[
    inputs$SegmentVars %in% names(scored) &
      vapply(inputs$SegmentVars, function(v) data.table::uniqueN(scored[[v]], na.rm = TRUE) <= MaxSegmentLevels, logical(1L))
  ]

  segment_summary <- data.table::rbindlist(
    lapply(segment_vars, function(v) model_insights_segment_metrics(scored, v, MinSegmentN)),
    fill = TRUE
  )

  worst_rmse <- if (nrow(segment_summary)) segment_summary[order(-RMSE)][seq_len(min(.N, 50L))] else data.table::data.table()
  worst_bias <- if (nrow(segment_summary)) segment_summary[order(-abs(Bias))][seq_len(min(.N, 50L))] else data.table::data.table()

  plots <- list()

  if (nrow(worst_rmse)) {
    worst_rmse[, SegmentLabel := paste(SegmentVariable, SegmentLevel, sep = ": ")]
    worst_rmse_plot_data <- aq_report_sort_for_flipped_bar(worst_rmse, "RMSE", "SegmentLabel")

    plots$worst_segments_by_rmse <- model_insights_try_plot(
      AutoPlots::Bar(
        dt = worst_rmse_plot_data,
        XVar = "SegmentLabel",
        YVar = "RMSE",
        title.text = "Worst Segments by RMSE",
        xAxis.title = "Segment",
        yAxis.title = "RMSE",
        Theme = Theme
      ) |>
        echarts4r::e_flip_coords()
    )
  }

  if (nrow(worst_bias)) {
    worst_bias[, SegmentLabel := paste(SegmentVariable, SegmentLevel, sep = ": ")]
    worst_bias_plot_data <- aq_report_sort_for_flipped_bar(worst_bias, "Bias", "SegmentLabel")

    plots$worst_segments_by_bias <- model_insights_try_plot(
      AutoPlots::Bar(
        dt = worst_bias_plot_data,
        XVar = "SegmentLabel",
        YVar = "Bias",
        title.text = "Worst Segments by Bias",
        xAxis.title = "Segment",
        yAxis.title = "Bias",
        Theme = Theme
      ) |>
        echarts4r::e_flip_coords()
    )
  }

  list(
    tables = list(
      segment_metric_summary = segment_summary,
      worst_segments_by_rmse = worst_rmse,
      worst_segments_by_bias = worst_bias
    ),
    plots = plots
  )
}


# ============================================================
# Stability / Drift
# ============================================================

model_insights_psi <- function(expected, actual, bins = 10L) {
  expected <- model_insights_safe_numeric(expected)
  actual <- model_insights_safe_numeric(actual)

  expected <- expected[model_insights_finite(expected)]
  actual <- actual[model_insights_finite(actual)]

  if (length(expected) < 10L || length(actual) < 10L) return(NA_real_)

  breaks <- unique(stats::quantile(expected, probs = seq(0, 1, length.out = bins + 1L), na.rm = TRUE))

  if (length(breaks) <= 2L) return(NA_real_)

  e_bin <- cut(expected, breaks = breaks, include.lowest = TRUE)
  a_bin <- cut(actual, breaks = breaks, include.lowest = TRUE)

  e_pct <- prop.table(table(e_bin))
  a_pct <- prop.table(table(a_bin))

  all_bins <- union(names(e_pct), names(a_pct))
  e <- as.numeric(e_pct[all_bins])
  a <- as.numeric(a_pct[all_bins])

  e[is.na(e)] <- 1e-6
  a[is.na(a)] <- 1e-6
  e[e == 0] <- 1e-6
  a[a == 0] <- 1e-6

  sum((a - e) * log(a / e), na.rm = TRUE)
}

model_insights_build_stability_artifacts <- function(
    inputs,
    scored_train,
    scored_test,
    DateVar = NULL,
    TimeBins = "month",
    Theme = "dark"
) {

  feature_shift <- data.table::data.table()

  if (!is.null(inputs$TrainData) && !is.null(inputs$TestData)) {
    common_features <- intersect(inputs$FeatureColumnNames, intersect(names(inputs$TrainData), names(inputs$TestData)))

    feature_shift <- data.table::rbindlist(
      lapply(common_features, function(v) {
        train_x <- inputs$TrainData[[v]]
        test_x <- inputs$TestData[[v]]

        if (model_insights_is_numeric_col(train_x) && model_insights_is_numeric_col(test_x)) {
          data.table::data.table(
            Variable = v,
            Type = "numeric",
            TrainMean = model_insights_safe_mean(train_x),
            TestMean = model_insights_safe_mean(test_x),
            TrainSD = model_insights_safe_sd(train_x),
            TestSD = model_insights_safe_sd(test_x),
            MeanDiff = model_insights_safe_mean(test_x) - model_insights_safe_mean(train_x),
            MeanDiffPct = data.table::fifelse(model_insights_safe_mean(train_x) == 0, NA_real_,
                                              (model_insights_safe_mean(test_x) - model_insights_safe_mean(train_x)) / abs(model_insights_safe_mean(train_x))),
            PSI = model_insights_psi(train_x, test_x)
          )
        } else {
          data.table::data.table(
            Variable = v,
            Type = class(train_x)[1L],
            TrainUnique = data.table::uniqueN(train_x, na.rm = TRUE),
            TestUnique = data.table::uniqueN(test_x, na.rm = TRUE),
            PSI = NA_real_
          )
        }
      }),
      fill = TRUE
    )

    if (nrow(feature_shift)) {
      data.table::setorder(feature_shift, -PSI)
    }
  }

  prediction_shift <- data.table::data.table()

  if (!is.null(scored_train) && nrow(scored_train) && !is.null(scored_test) && nrow(scored_test)) {
    prediction_shift <- data.table::data.table(
      Variable = c("Prediction", "Actual", "Residual"),
      PSI = c(
        model_insights_psi(scored_train$Prediction, scored_test$Prediction),
        model_insights_psi(scored_train$Actual, scored_test$Actual),
        model_insights_psi(scored_train$Residual, scored_test$Residual)
      ),
      TrainMean = c(
        model_insights_safe_mean(scored_train$Prediction),
        model_insights_safe_mean(scored_train$Actual),
        model_insights_safe_mean(scored_train$Residual)
      ),
      TestMean = c(
        model_insights_safe_mean(scored_test$Prediction),
        model_insights_safe_mean(scored_test$Actual),
        model_insights_safe_mean(scored_test$Residual)
      )
    )
  }

  metric_by_time <- data.table::data.table()

  if (!is.null(DateVar) && !is.null(scored_test) && DateVar %in% names(scored_test)) {
    dt <- data.table::copy(scored_test)
    dt[, DateValue := as.Date(get(DateVar))]

    if (any(!is.na(dt$DateValue))) {
      if (TimeBins == "month") {
        dt[, TimeBin := as.Date(format(DateValue, "%Y-%m-01"))]
      } else {
        dt[, TimeBin := DateValue]
      }

      metric_by_time <- dt[
        !is.na(TimeBin) & model_insights_finite(Actual) & model_insights_finite(Prediction),
        {
          r <- Actual - Prediction
          data.table::data.table(
            N = .N,
            MAE = mean(abs(r), na.rm = TRUE),
            RMSE = sqrt(mean(r^2, na.rm = TRUE)),
            Bias = mean(Prediction - Actual, na.rm = TRUE),
            MeanActual = mean(Actual, na.rm = TRUE),
            MeanPrediction = mean(Prediction, na.rm = TRUE)
          )
        },
        by = TimeBin
      ][order(TimeBin)]
    }
  }

  plots <- list()

  if (nrow(feature_shift) && "PSI" %in% names(feature_shift)) {
    feature_shift_plot_data <- aq_report_sort_for_flipped_bar(utils::head(feature_shift[!is.na(PSI)][order(-PSI)], 50L), "PSI", "Variable")

    plots$feature_shift_psi <- model_insights_try_plot(
      AutoPlots::Bar(
        dt = feature_shift_plot_data,
        XVar = "Variable",
        YVar = "PSI",
        title.text = "Train/Test Feature Shift PSI",
        xAxis.title = "Feature",
        yAxis.title = "PSI",
        Theme = Theme
      ) |>
        echarts4r::e_flip_coords()
    )
  }

  if (nrow(metric_by_time)) {
    plots$rmse_over_time <- model_insights_try_plot(
      AutoPlots::Line(
        dt = metric_by_time,
        XVar = "TimeBin",
        YVar = "RMSE",
        title.text = "RMSE Over Time",
        xAxis.title = "Time",
        yAxis.title = "RMSE",
        Theme = Theme
      )
    )

    plots$bias_over_time <- model_insights_try_plot(
      AutoPlots::Line(
        dt = metric_by_time,
        XVar = "TimeBin",
        YVar = "Bias",
        title.text = "Bias Over Time",
        xAxis.title = "Time",
        yAxis.title = "Bias",
        Theme = Theme
      )
    )
  }

  list(
    tables = list(
      train_test_feature_shift = feature_shift,
      train_test_prediction_shift = prediction_shift,
      metric_by_time = metric_by_time
    ),
    plots = plots
  )
}


# ============================================================
# Deployment Readiness
# ============================================================

model_insights_build_deployment_readiness <- function(
    qa,
    tables,
    MinCalibrationBias = NULL,
    MaxRMSEGapPct = 0.25,
    MaxPSI = 0.25
) {

  flags <- list()

  add_flag <- function(flag, severity, status, detail) {
    data.table::data.table(
      Flag = flag,
      Severity = severity,
      Status = status,
      Detail = detail
    )
  }

  flags[[length(flags) + 1L]] <- add_flag(
    "Input validation failures",
    "High",
    if (isTRUE(qa$has_failures)) "Fail" else "Pass",
    if (isTRUE(qa$has_failures)) "One or more required inputs failed validation." else "No validation failures."
  )

  metric_gap <- tables$evaluation$train_test_metric_gap

  if (!is.null(metric_gap) && nrow(metric_gap) && "Metric" %in% names(metric_gap)) {
    rmse_gap <- metric_gap[Metric == "RMSE"]$Percent_Gap[1L]

    flags[[length(flags) + 1L]] <- add_flag(
      "High train/test RMSE gap",
      "Medium",
      if (!is.na(rmse_gap) && rmse_gap > MaxRMSEGapPct) "Warning" else "Pass",
      paste0("RMSE percent gap = ", round(rmse_gap, 4))
    )
  }

  residual_summary <- data.table::rbindlist(
    list(tables$residuals$residual_summary_train, tables$residuals$residual_summary_test),
    fill = TRUE
  )

  if (nrow(residual_summary)) {
    max_abs_bias <- max(abs(residual_summary$MeanResidual), na.rm = TRUE)

    flags[[length(flags) + 1L]] <- add_flag(
      "Residual bias",
      "Medium",
      if (is.finite(max_abs_bias) && max_abs_bias > 0) "Review" else "Pass",
      paste0("Max absolute mean residual = ", round(max_abs_bias, 4))
    )
  }

  simpsons <- tables$stratified_effects$simpsons_paradox_flags

  if (!is.null(simpsons) && nrow(simpsons)) {
    n_flags <- simpsons[SimpsonsFlag == TRUE, .N]

    flags[[length(flags) + 1L]] <- add_flag(
      "Possible Simpson's Paradox effects",
      "Medium",
      if (n_flags > 0L) "Review" else "Pass",
      paste0(n_flags, " feature/by-variable combinations flagged.")
    )
  }

  stability <- tables$stability$train_test_feature_shift

  if (!is.null(stability) && nrow(stability) && "PSI" %in% names(stability)) {
    high_psi <- stability[!is.na(PSI) & PSI >= MaxPSI, .N]

    flags[[length(flags) + 1L]] <- add_flag(
      "Large train/test feature shift",
      "Medium",
      if (high_psi > 0L) "Review" else "Pass",
      paste0(high_psi, " variables with PSI >= ", MaxPSI)
    )
  }

  risk_flags <- data.table::rbindlist(flags, fill = TRUE)

  recommended_actions <- risk_flags[
    Status %in% c("Fail", "Warning", "Review"),
    .(
      Issue = Flag,
      Severity = Severity,
      Recommendation = data.table::fifelse(
        grepl("Simpson", Flag),
        "Review stratified effect tables before making global feature-effect claims.",
        data.table::fifelse(
          grepl("feature shift", Flag),
          "Review feature drift and consider retraining, reweighting, or stronger validation splits.",
          data.table::fifelse(
            grepl("RMSE", Flag),
            "Review train/test metric gaps and inspect residual/error diagnostics by segment.",
            "Review the corresponding artifact tables and plots."
          )
        )
      )
    )
  ]

  list(
    risk_flags = risk_flags,
    recommended_actions = recommended_actions
  )
}


# ============================================================
# Layout and Export
# ============================================================

model_insights_build_regression_layout <- function() {
  list(
    sections = list(
      overview = c(
        "metadata",
        "qa",
        "model_metadata",
        "data_audit",
        "deployment_readiness"
      ),
      performance = c(
        "evaluation",
        "residuals",
        "calibration",
        "error_analysis"
      ),
      feature_diagnostics = c(
        "calibration_by_feature",
        "uplift_by_feature",
        "stratified_effects",
        "simpsons_paradox"
      ),
      interpretation = c(
        "importance",
        "interactions"
      ),
      stability = c(
        "segment_performance",
        "stability"
      )
    ),
    plot_cols = list(
      evaluation = 2L,
      residuals = 2L,
      calibration = 2L,
      calibration_by_feature = 2L,
      uplift_by_feature = 2L,
      stratified_effects = 1L,
      segment_performance = 1L,
      stability = 1L
    )
  )
}

model_insights_export_sidecars <- function(
    widgets,
    plots,
    OutputPath = NULL,
    ExportPNG = FALSE,
    ExportHTML = FALSE,
    PNGWidth = 1400,
    PNGHeight = 900,
    PNGDPI = 150,
    PNGBackground = "white",
    WebshotDelay = 0.2,
    IncludeDataURL = FALSE
) {

  exports <- list(
    image_manifest = data.table::data.table()
  )

  if (is.null(OutputPath) || !(isTRUE(ExportPNG) || isTRUE(ExportHTML) || isTRUE(IncludeDataURL))) {
    return(exports)
  }

  dir.create(file.path(OutputPath, "images"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(OutputPath, "html"), recursive = TRUE, showWarnings = FALSE)

  wrapped <- list(
    widgets = model_insights_wrap_named_objects(widgets, section = "regression_model_insights", artifact_type = "widget"),
    plots = model_insights_wrap_named_objects(plots, section = "regression_model_insights", artifact_type = "plot")
  )

  exported <- model_insights_export_artifact_tree(
    wrapped,
    output_path = OutputPath,
    export_png = ExportPNG,
    export_html = ExportHTML,
    width = PNGWidth,
    height = PNGHeight,
    dpi = PNGDPI,
    background = PNGBackground,
    delay = WebshotDelay,
    include_data_url = IncludeDataURL
  )

  manifest <- model_insights_collect_llm_image_manifest(exported)

  exports$image_manifest <- if (length(manifest)) {
    data.table::rbindlist(manifest, fill = TRUE)
  } else {
    data.table::data.table()
  }

  exports
}


# ============================================================
# Main Generator
# ============================================================

#' Generate Regression Model Insights Artifacts
#'
#' Generates a complete canonical artifact set for regression model insights.
#' This function preserves the legacy Regression_ModelInsights.Rmd outputs and
#' adds expert-level regression diagnostics, including computed metrics,
#' residual analysis, calibration, top-error analysis, importance, CatBoost
#' interactions, calibration-style partial dependence, uplift-by-feature,
#' stratified effects, possible Simpson's Paradox flags, segment performance,
#' stability/drift, and deployment-readiness flags.
#'
#' @param ModelObject Optional AutoQuant model object.
#' @param Algo Optional model algorithm name. If NULL, extracted from ModelObject when available.
#' @param TrainData Optional training data.
#' @param TestData Optional test data.
#' @param TargetColumnName Optional target column name.
#' @param PredictionColumnName Prediction column name. Defaults to "Predict".
#' @param FeatureColumnNames Optional feature columns. If NULL, extracted from ModelObject when available.
#' @param SegmentVars Optional segment variables for segment performance.
#' @param ByVars Optional by-variables for stratified effects / Simpson's Paradox checks.
#' @param DateVar Optional date variable for time-based stability diagnostics.
#' @param IDColumn Optional ID column retained in top-error tables.
#' @param Theme AutoPlots theme.
#' @param GenerateCalibrationPDP Logical. Generate existing calibration-style PDP plots.
#' @param GenerateUpliftPDP Logical. Generate prediction-uplift feature summaries and plots.
#' @param GenerateStratifiedEffects Logical. Generate by-variable stratified effects.
#' @param DetectSimpsonsParadox Logical. Generate Simpson's Paradox flags.
#' @param PDPBaseline Baseline strategy for numeric uplift tables.
#' @param ReferenceLevels Optional named list of categorical reference levels.
#' @param SampleSize Sample size used by large-row visual wrappers.
#' @param NumberBins Number of bins for residual/PDP visuals.
#' @param CalibrationBins Number of bins for calibration plots.
#' @param UpliftBins Number of bins for uplift summaries.
#' @param MaxPDPFeatures Maximum features for PDP/uplift plots.
#' @param MaxByLevels Maximum levels allowed for by-variable diagnostics.
#' @param MaxCategoricalLevels Maximum categorical levels kept before Other.
#' @param MaxSegmentLevels Maximum levels allowed for segment performance.
#' @param MinByGroupN Minimum row count per by-variable group.
#' @param MinSegmentN Minimum row count per segment.
#' @param MinEffectSize Minimum effect size for direction/reversal checks.
#' @param MinReversalShare Minimum weighted opposite-direction share for Simpson's flag.
#' @param MinValidGroups Minimum valid groups for Simpson's flag.
#' @param MaxTopErrors Maximum top-error rows.
#' @param MaxInteractionRows Maximum CatBoost interaction rows.
#' @param OutputPath Optional sidecar export path.
#' @param ExportPNG Logical. Export renderable artifacts as PNG.
#' @param ExportHTML Logical. Export renderable artifacts as HTML.
#' @param PNGWidth,PNGHeight PNG/html export dimensions.
#' @param PNGDPI PNG export resolution.
#' @param PNGBackground PNG/html export background.
#' @param WebshotDelay Delay before webshot capture.
#' @param IncludeDataURL Logical. Include Base64 data URLs for exported PNGs.
#'
#' @return A canonical artifact list with report_type, metadata, qa, tables,
#' widgets, plots, layout, exports, and context.
#'
#' @family Reports
#' @export
generate_regression_model_insights_artifacts <- function(
    ModelObject = NULL,
    Algo = NULL,
    TrainData = NULL,
    TestData = NULL,
    TargetColumnName = NULL,
    PredictionColumnName = "Predict",
    FeatureColumnNames = NULL,
    SegmentVars = NULL,
    ByVars = NULL,
    DateVar = NULL,
    IDColumn = NULL,
    Theme = "dark",

    GenerateCalibrationPDP = TRUE,
    GenerateUpliftPDP = TRUE,
    GenerateStratifiedEffects = TRUE,
    DetectSimpsonsParadox = TRUE,
    PDPBaseline = c("lowest_bin", "overall_mean", "median_bin"),
    ReferenceLevels = NULL,

    SampleSize = 100000L,
    NumberBins = 20L,
    CalibrationBins = 21L,
    UpliftBins = 20L,
    MaxPDPFeatures = 50L,
    MaxByLevels = 10L,
    MaxCategoricalLevels = 25L,
    MaxSegmentLevels = 25L,
    MinByGroupN = 50L,
    MinSegmentN = 25L,
    MinEffectSize = NULL,
    MinReversalShare = 0.50,
    MinValidGroups = 2L,
    MaxTopErrors = 100L,
    MaxInteractionRows = 200L,

    OutputPath = NULL,
    ExportPNG = FALSE,
    ExportHTML = FALSE,
    PNGWidth = 1400,
    PNGHeight = 900,
    PNGDPI = 150,
    PNGBackground = "white",
    WebshotDelay = 0.2,
    IncludeDataURL = FALSE
) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.", call. = FALSE)
  }
  if (!requireNamespace("AutoPlots", quietly = TRUE)) {
    stop("Package 'AutoPlots' is required.", call. = FALSE)
  }
  if (!requireNamespace("echarts4r", quietly = TRUE)) {
    stop("Package 'echarts4r' is required.", call. = FALSE)
  }

  PDPBaseline <- match.arg(PDPBaseline)

  inputs <- model_insights_extract_regression_inputs(
    ModelObject = ModelObject,
    Algo = Algo,
    TrainData = TrainData,
    TestData = TestData,
    TargetColumnName = TargetColumnName,
    PredictionColumnName = PredictionColumnName,
    FeatureColumnNames = FeatureColumnNames,
    SegmentVars = SegmentVars,
    ByVars = ByVars,
    DateVar = DateVar,
    IDColumn = IDColumn,
    Theme = Theme
  )

  qa <- model_insights_build_regression_qa(inputs)

  tables <- list()
  widgets <- list()
  plots <- list()

  tables$model_metadata <- model_insights_build_model_metadata_tables(inputs)
  tables$data_audit <- model_insights_build_data_audit_tables(inputs)

  evaluation <- model_insights_build_regression_evaluation_artifacts(
    inputs,
    SampleSize = SampleSize,
    Theme = Theme
  )
  tables$evaluation <- evaluation$tables
  plots$evaluation <- evaluation$plots

  scored_train <- tables$evaluation$scored_train
  scored_test <- tables$evaluation$scored_test

  residuals <- model_insights_build_regression_residual_artifacts(
    inputs,
    scored_train = scored_train,
    scored_test = scored_test,
    SampleSize = SampleSize,
    NumberBins = NumberBins,
    Theme = Theme
  )
  tables$residuals <- residuals$tables
  plots$residuals <- residuals$plots

  calibration <- model_insights_build_regression_calibration_artifacts(
    inputs,
    scored_train = scored_train,
    scored_test = scored_test,
    SampleSize = SampleSize,
    CalibrationBins = CalibrationBins,
    Theme = Theme
  )
  tables$calibration <- calibration$tables
  plots$calibration <- calibration$plots

  errors <- model_insights_build_regression_error_analysis_artifacts(
    inputs,
    scored_train = scored_train,
    scored_test = scored_test,
    MaxTopErrors = MaxTopErrors,
    Theme = Theme
  )
  tables$error_analysis <- errors$tables
  plots$error_analysis <- errors$plots

  importance <- model_insights_build_regression_importance_artifacts(
    inputs,
    MaxInteractionRows = MaxInteractionRows,
    Theme = Theme
  )
  tables$importance <- importance$tables
  plots$importance <- importance$plots

  interactions <- model_insights_build_regression_interaction_artifacts(
    inputs,
    MaxInteractionRows = MaxInteractionRows,
    Theme = Theme
  )
  tables$interactions <- interactions$tables
  plots$interactions <- interactions$plots

  if (isTRUE(GenerateCalibrationPDP)) {
    calibration_by_feature <- model_insights_build_calibration_by_feature_artifacts(
      inputs,
      SampleSize = SampleSize,
      NumberBins = NumberBins,
      MaxPDPFeatures = MaxPDPFeatures,
      Theme = Theme
    )
    tables$calibration_by_feature <- calibration_by_feature$tables
    plots$calibration_by_feature <- calibration_by_feature$plots
  } else {
    tables$calibration_by_feature <- list()
    plots$calibration_by_feature <- list()
  }

  if (isTRUE(GenerateUpliftPDP)) {
    uplift_by_feature <- model_insights_build_uplift_by_feature_artifacts(
      inputs,
      UpliftBins = UpliftBins,
      MaxPDPFeatures = MaxPDPFeatures,
      MaxCategoricalLevels = MaxCategoricalLevels,
      ReferenceLevels = ReferenceLevels,
      PDPBaseline = PDPBaseline,
      MinEffectSize = MinEffectSize,
      Theme = Theme
    )
    tables$uplift_by_feature <- uplift_by_feature$tables
    plots$uplift_by_feature <- uplift_by_feature$plots
  } else {
    tables$uplift_by_feature <- list()
    plots$uplift_by_feature <- list()
  }

  if (isTRUE(GenerateStratifiedEffects)) {
    stratified <- model_insights_build_stratified_effects_artifacts(
      inputs,
      UpliftBins = min(UpliftBins, 10L),
      MaxByLevels = MaxByLevels,
      MaxCategoricalLevels = MaxCategoricalLevels,
      MinByGroupN = MinByGroupN,
      MinEffectSize = MinEffectSize,
      MinReversalShare = MinReversalShare,
      MinValidGroups = MinValidGroups,
      ReferenceLevels = ReferenceLevels,
      Theme = Theme
    )
    tables$stratified_effects <- stratified$tables
    plots$stratified_effects <- stratified$plots
  } else {
    tables$stratified_effects <- list(
      stratified_effects = data.table::data.table(),
      simpsons_paradox_flags = data.table::data.table(),
      effect_heterogeneity_summary = data.table::data.table()
    )
    plots$stratified_effects <- list()
  }

  if (!isTRUE(DetectSimpsonsParadox)) {
    tables$stratified_effects$simpsons_paradox_flags <- data.table::data.table()
  }

  segment_performance <- model_insights_build_segment_performance_artifacts(
    inputs,
    scored_train = scored_train,
    scored_test = scored_test,
    MaxSegmentLevels = MaxSegmentLevels,
    MinSegmentN = MinSegmentN,
    Theme = Theme
  )
  tables$segment_performance <- segment_performance$tables
  plots$segment_performance <- segment_performance$plots

  stability <- model_insights_build_stability_artifacts(
    inputs,
    scored_train = scored_train,
    scored_test = scored_test,
    DateVar = DateVar,
    Theme = Theme
  )
  tables$stability <- stability$tables
  plots$stability <- stability$plots

  deployment <- model_insights_build_deployment_readiness(
    qa = qa,
    tables = tables
  )
  tables$deployment_readiness <- deployment

  layout <- model_insights_build_regression_layout()

  exports <- model_insights_export_sidecars(
    widgets = widgets,
    plots = plots,
    OutputPath = OutputPath,
    ExportPNG = ExportPNG,
    ExportHTML = ExportHTML,
    PNGWidth = PNGWidth,
    PNGHeight = PNGHeight,
    PNGDPI = PNGDPI,
    PNGBackground = PNGBackground,
    WebshotDelay = WebshotDelay,
    IncludeDataURL = IncludeDataURL
  )

  artifact_counts <- data.table::data.table(
    ArtifactGroup = c(
      "model_metadata_tables",
      "data_audit_tables",
      "evaluation_tables",
      "evaluation_plots",
      "residual_tables",
      "residual_plots",
      "calibration_tables",
      "calibration_plots",
      "error_analysis_tables",
      "importance_tables",
      "importance_plots",
      "interaction_tables",
      "interaction_plots",
      "calibration_by_feature_tables",
      "calibration_by_feature_plots",
      "uplift_by_feature_tables",
      "uplift_by_feature_plots",
      "stratified_effect_tables",
      "stratified_effect_plots",
      "segment_performance_tables",
      "segment_performance_plots",
      "stability_tables",
      "stability_plots",
      "deployment_readiness_tables"
    ),
    Count = c(
      length(tables$model_metadata),
      length(tables$data_audit),
      length(tables$evaluation),
      model_insights_count_renderable_objects(plots$evaluation),
      length(tables$residuals),
      model_insights_count_renderable_objects(plots$residuals),
      length(tables$calibration),
      model_insights_count_renderable_objects(plots$calibration),
      length(tables$error_analysis),
      length(tables$importance),
      model_insights_count_renderable_objects(plots$importance),
      length(tables$interactions),
      model_insights_count_renderable_objects(plots$interactions),
      length(tables$calibration_by_feature),
      model_insights_count_renderable_objects(plots$calibration_by_feature),
      length(tables$uplift_by_feature),
      model_insights_count_renderable_objects(plots$uplift_by_feature),
      length(tables$stratified_effects),
      model_insights_count_renderable_objects(plots$stratified_effects),
      length(tables$segment_performance),
      model_insights_count_renderable_objects(plots$segment_performance),
      length(tables$stability),
      model_insights_count_renderable_objects(plots$stability),
      length(tables$deployment_readiness)
    )
  )

  llm_context <- list(
    ArtifactInstruction = paste(
      "Use dynamic `object` values for RMarkdown display.",
      "Use `exports$image_manifest` PNG sidecars for LLM vision input when present.",
      "Use compact tables and metadata for exact numeric statements.",
      "Calibration-by-feature plots show actual-vs-predicted behavior by feature.",
      "Uplift-by-feature tables/plots show modeled prediction deltas by feature.",
      "Stratified effects and Simpson's Paradox flags should be reviewed before making global feature-effect claims.",
      "SHAP artifacts are intentionally excluded from this generator."
    ),
    ImageManifest = exports$image_manifest,
    ArtifactCounts = artifact_counts
  )

  out <- list(
    report_type = "regression_model_insights",
    metadata = c(
      inputs$metadata,
      list(
        artifact_counts = artifact_counts,
        output_path = OutputPath,
        export_png = ExportPNG,
        export_html = ExportHTML,
        include_data_url = IncludeDataURL
      )
    ),
    qa = qa,
    tables = tables,
    widgets = widgets,
    plots = plots,
    layout = layout,
    exports = exports,
    context = c(inputs$context, llm_context)
  )

  class(out) <- c(
    "autoquant_regression_model_insights_artifacts",
    "autoquant_report_artifacts",
    "list"
  )

  out
}


# ============================================================
# Minimal RMarkdown Usage Pattern
# ============================================================
#
# artifacts <- generate_regression_model_insights_artifacts(
#   ModelObject = ModelObject,
#   Algo = Algo,
#   Theme = Theme,
#   OutputPath = OutputPath,
#   ExportPNG = ExportPNG,
#   ExportHTML = ExportHTML,
#   IncludeDataURL = IncludeDataURL
# )
#
# In RMarkdown display chunks:
#
# artifacts$tables$evaluation$computed_metrics_combined
# artifacts$plots$residuals$test$histogram
# artifacts$plots$calibration$test$line
# artifacts$plots$calibration_by_feature$numeric_line$test[[1]]
# artifacts$plots$uplift_by_feature$numeric_line$test[[1]]
# artifacts$tables$stratified_effects$simpsons_paradox_flags
# artifacts$exports$image_manifest
# ============================================================
