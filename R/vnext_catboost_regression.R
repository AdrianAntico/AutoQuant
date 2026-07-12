# AutoQuant vNext supervised learning foundation: CatBoost regression and binary classification.

aq_vnext_now <- function() {
  Sys.time()
}

aq_vnext_default <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

aq_vnext_slug <- function(x, default = "autoquant") {
  x <- as.character(aq_vnext_default(x, default))[1L]
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- tolower(x)
  if (!nzchar(x)) default else x
}

aq_vnext_id <- function(prefix, seed = NULL) {
  seed_int <- suppressWarnings(as.integer(seed)[1L])
  paste0(
    aq_vnext_slug(prefix, "aq"),
    "_",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "_",
    sprintf("%06d", sample.int(999999L, 1L)),
    if (!is.null(seed) && !is.na(seed_int)) paste0("_s", seed_int) else ""
  )
}

aq_vnext_unique_chr <- function(x) {
  unique(as.character(x[!is.na(x) & nzchar(trimws(as.character(x)))]))
}

aq_vnext_clean_chr <- function(x) {
  as.character(x[!is.na(x) & nzchar(trimws(as.character(x)))])
}

aq_vnext_validation_table <- function(check, status, message, severity = status) {
  data.table::data.table(
    check = check,
    status = status,
    severity = severity,
    message = message
  )
}

aq_vnext_supported_engine_params <- function() {
  c(
    "iterations",
    "depth",
    "learning_rate",
    "loss_function",
    "eval_metric",
    "random_seed",
    "thread_count",
    "verbose",
    "task_type",
    "l2_leaf_reg",
    "random_strength",
    "bootstrap_type"
  )
}

aq_vnext_engine_params <- function(engine_params = list(), seed = 20260712L, task = "regression") {
  if (is.null(engine_params)) {
    engine_params <- list()
  }
  unknown <- setdiff(names(engine_params), aq_vnext_supported_engine_params())
  if (length(unknown)) {
    stop("Unsupported CatBoost engine parameter(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  defaults <- list(
    iterations = 100L,
    depth = 6L,
    learning_rate = 0.05,
    loss_function = if (identical(task, "binary")) "Logloss" else "RMSE",
    eval_metric = if (identical(task, "binary")) "AUC" else "RMSE",
    random_seed = as.integer(seed),
    thread_count = max(1L, parallel::detectCores(logical = TRUE) - 1L),
    verbose = 0L,
    task_type = "CPU"
  )
  out <- utils::modifyList(defaults, engine_params)
  if (is.logical(out$verbose)) {
    out$verbose <- if (isTRUE(out$verbose)) 1L else 0L
  }
  out
}

#' Create a vNext Threshold Policy
#'
#' @param threshold Decision threshold for positive-class assignment.
#' @param positive_class Optional positive class label.
#' @param negative_class Optional negative class label.
#' @param policy_id Optional threshold policy identifier.
#'
#' @return An `aq_threshold_policy` object.
#' @export
aq_threshold_policy <- function(
  threshold = 0.5,
  positive_class = NULL,
  negative_class = NULL,
  policy_id = NULL
) {
  threshold <- as.numeric(threshold)[1L]
  if (is.null(policy_id)) {
    policy_id <- aq_vnext_id("threshold_policy")
  }
  x <- list(
    threshold_policy_id = as.character(policy_id)[1L],
    schema_version = "aq_threshold_policy_v1",
    threshold = threshold,
    positive_class = if (is.null(positive_class)) NULL else as.character(positive_class)[1L],
    negative_class = if (is.null(negative_class)) NULL else as.character(negative_class)[1L],
    calibration_method = "none",
    created_at = aq_vnext_now()
  )
  class(x) <- c("aq_threshold_policy", "list")
  x
}

aq_validate_threshold_policy <- function(policy) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  if (!inherits(policy, "aq_threshold_policy")) {
    add("threshold_policy_class", "fail", "threshold_policy must be created by aq_threshold_policy().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  if (!is.finite(policy$threshold) || policy$threshold <= 0 || policy$threshold >= 1) {
    add("threshold_policy_threshold", "fail", "threshold must be in (0, 1).")
  } else {
    add("threshold_policy_threshold", "pass", "threshold is valid.", "info")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Create a vNext Model Specification
#'
#' @description
#' Defines the first AutoQuant vNext supervised-learning specification contract.
#' Phase 3 supports CatBoost regression and binary classification.
#'
#' @param task Task family. Supports `"regression"` and `"binary"`.
#' @param engine Modeling engine. Supports `"catboost"`.
#' @param target Target column.
#' @param features Feature columns.
#' @param partition Optional partition specification from `aq_partition_spec()`.
#' @param engine_params Named CatBoost engine parameters.
#' @param metrics Metrics to compute during assessment.
#' @param positive_class Optional positive class for binary classification.
#' @param threshold_policy Optional threshold policy from `aq_threshold_policy()`.
#' @param seed Reproducibility seed.
#' @param model_id Optional model identifier.
#' @param dataset_id Optional source dataset identifier.
#' @param transformation_spec Optional Rodeo transformation specification. When
#'   supplied, the transformation is fitted during model training and replayed
#'   during scoring.
#' @param supported_downstream_actions Supported next actions.
#'
#' @return An `aq_model_spec` object.
#' @export
aq_model_spec <- function(
  task = "regression",
  engine = "catboost",
  target,
  features,
  partition = aq_partition_spec(seed = seed),
  engine_params = list(),
  metrics = NULL,
  positive_class = NULL,
  threshold_policy = NULL,
  seed = 20260712L,
  model_id = NULL,
  dataset_id = NULL,
  transformation_spec = NULL,
  supported_downstream_actions = c("predict", "assess", "compare")
) {
  task <- match.arg(tolower(task), c("regression", "binary"))
  engine <- match.arg(tolower(engine), c("catboost"))
  target <- as.character(target)[1L]
  features <- aq_vnext_clean_chr(features)
  seed <- as.integer(seed)[1L]
  if (is.null(metrics)) {
    metrics <- if (identical(task, "binary")) {
      c("logloss", "auc", "accuracy", "precision", "recall", "f1", "specificity", "sensitivity")
    } else {
      c("rmse", "mae", "r2")
    }
  }
  if (identical(task, "binary") && is.null(threshold_policy)) {
    threshold_policy <- aq_threshold_policy(positive_class = positive_class)
  }
  if (identical(task, "binary") && is.null(positive_class) && inherits(threshold_policy, "aq_threshold_policy")) {
    positive_class <- threshold_policy$positive_class
  }
  if (is.null(model_id)) {
    model_id <- aq_vnext_id(paste("model", engine, task, target, sep = "_"), seed = seed)
  }
  spec <- list(
    spec_id = aq_vnext_id("spec", seed = seed),
    schema_version = "aq_model_spec_v1",
    task = task,
    engine = engine,
    target = target,
    features = features,
    partition = partition,
    engine_params = aq_vnext_engine_params(engine_params, seed = seed, task = task),
    metrics = aq_vnext_unique_chr(metrics),
    positive_class = if (is.null(positive_class)) NULL else as.character(positive_class)[1L],
    threshold_policy = threshold_policy,
    seed = seed,
    model_id = as.character(model_id)[1L],
    dataset_id = aq_vnext_default(dataset_id, NA_character_),
    transformation_spec = transformation_spec,
    transformation_required = !is.null(transformation_spec),
    supported_downstream_actions = aq_vnext_unique_chr(supported_downstream_actions),
    created_at = aq_vnext_now()
  )
  class(spec) <- c("aq_model_spec", "list")
  spec
}

#' Create a vNext Partition Specification
#'
#' @param method Partition method. Supports `"random"` and `"time"`.
#' @param train_fraction Fraction of rows assigned to training when validation data is not supplied.
#' @param split_col Optional time/order column for time partitions.
#' @param seed Reproducibility seed.
#' @param partition_id Optional partition identifier.
#'
#' @return An `aq_partition_spec` object.
#' @export
aq_partition_spec <- function(
  method = "random",
  train_fraction = 0.8,
  split_col = NULL,
  seed = 20260712L,
  partition_id = NULL
) {
  method <- match.arg(tolower(method), c("random", "time"))
  if (is.null(partition_id)) {
    partition_id <- aq_vnext_id(paste0("partition_", method), seed = seed)
  }
  x <- list(
    partition_id = as.character(partition_id)[1L],
    schema_version = "aq_partition_spec_v1",
    method = method,
    train_fraction = as.numeric(train_fraction)[1L],
    split_col = if (is.null(split_col)) NULL else as.character(split_col)[1L],
    seed = as.integer(seed)[1L],
    created_at = aq_vnext_now()
  )
  class(x) <- c("aq_partition_spec", "list")
  x
}

aq_validate_partition_spec <- function(partition, data = NULL) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  if (!inherits(partition, "aq_partition_spec")) {
    add("partition_class", "fail", "partition must be created by aq_partition_spec().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  if (!partition$method %in% c("random", "time")) {
    add("partition_method", "fail", "partition method must be random or time.")
  } else {
    add("partition_method", "pass", paste("partition method:", partition$method), "info")
  }
  if (!is.finite(partition$train_fraction) || partition$train_fraction <= 0 || partition$train_fraction >= 1) {
    add("partition_train_fraction", "fail", "train_fraction must be in (0, 1).")
  } else {
    add("partition_train_fraction", "pass", "train_fraction is valid.", "info")
  }
  if (!is.null(data) && identical(partition$method, "time")) {
    if (is.null(partition$split_col) || !partition$split_col %in% names(data)) {
      add("partition_split_col", "fail", "time partition requires split_col to exist in data.")
    } else {
      add("partition_split_col", "pass", "time split_col exists.", "info")
    }
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Validate a vNext Model Specification
#'
#' @param spec An `aq_model_spec`.
#' @param data Optional data for column/type validation.
#'
#' @return A `data.table` of validation checks.
#' @export
aq_validate_model_spec <- function(spec, data = NULL) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  if (!inherits(spec, "aq_model_spec")) {
    add("spec_class", "fail", "spec must be created by aq_model_spec().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("spec_class", "pass", "spec inherits from aq_model_spec.", "info")
  if (!spec$task %in% c("regression", "binary")) {
    add("task_supported", "fail", "Phase 3 supports regression and binary tasks.")
  } else {
    add("task_supported", "pass", paste(spec$task, "task is supported."), "info")
  }
  if (!identical(spec$engine, "catboost")) {
    add("engine_supported", "fail", "Phase 2 supports CatBoost only.")
  } else {
    add("engine_supported", "pass", "CatBoost engine is supported.", "info")
  }
  if (is.null(spec$target) || !nzchar(spec$target)) {
    add("target_nonempty", "fail", "target must be a non-empty column name.")
  } else {
    add("target_nonempty", "pass", "target is non-empty.", "info")
  }
  if (!length(spec$features)) {
    add("features_nonempty", "fail", "features must contain at least one column.")
  } else {
    add("features_nonempty", "pass", "features are non-empty.", "info")
  }
  if (anyDuplicated(spec$features)) {
    add("features_unique", "fail", "features must not contain duplicates.")
  } else {
    add("features_unique", "pass", "features are unique.", "info")
  }
  if (spec$target %in% spec$features) {
    add("target_not_feature", "fail", "target must not be included in features.")
  } else {
    add("target_not_feature", "pass", "target is not included in features.", "info")
  }
  unknown_params <- setdiff(names(spec$engine_params), aq_vnext_supported_engine_params())
  if (length(unknown_params)) {
    add("engine_params_supported", "fail", paste("Unsupported engine parameters:", paste(unknown_params, collapse = ", ")))
  } else {
    add("engine_params_supported", "pass", "engine parameters are supported.", "info")
  }
  if (spec$engine_params$task_type != "CPU") {
    add("engine_task_type", "fail", "Phase 3 vNext CatBoost supports CPU task_type only.")
  } else {
    add("engine_task_type", "pass", "CPU task_type is supported.", "info")
  }
  has_transformation <- !is.null(spec$transformation_spec)
  if (has_transformation) {
    if (!requireNamespace("Rodeo", quietly = TRUE)) {
      add("transformation_dependency", "fail", "Rodeo is required when transformation_spec is supplied.")
    } else if (!inherits(spec$transformation_spec, "rodeo_transformation_spec")) {
      add("transformation_spec_class", "fail", "transformation_spec must be created by Rodeo::rodeo_transformation_spec().")
    } else {
      add("transformation_spec_class", "pass", paste("Rodeo transformation spec:", spec$transformation_spec$id), "info")
    }
  } else {
    add("transformation_spec_class", "pass", "no Rodeo transformation spec supplied.", "info")
  }
  if (!is.null(data)) {
    if (!is.data.frame(data) && !data.table::is.data.table(data)) {
      add("data_frame_like", "fail", "data must be data.frame/data.table-like.")
    } else {
      add("data_frame_like", "pass", "data is data.frame/data.table-like.", "info")
      raw_required_cols <- if (has_transformation && inherits(spec$transformation_spec, "rodeo_transformation_spec")) {
        c(spec$target, spec$transformation_spec$input_columns)
      } else {
        c(spec$target, spec$features)
      }
      missing_cols <- setdiff(raw_required_cols, names(data))
      if (length(missing_cols)) {
        add("columns_exist", "fail", paste("Missing columns:", paste(missing_cols, collapse = ", ")))
      } else {
        add("columns_exist", "pass", if (has_transformation) "target and Rodeo transformation input columns exist in data." else "target and features exist in data.", "info")
        if (identical(spec$task, "regression") && !is.numeric(data[[spec$target]])) {
          add("target_type", "fail", "regression target must be numeric.")
        } else if (identical(spec$task, "regression")) {
          add("target_type", "pass", "regression target is numeric.", "info")
        } else {
          target_values <- data[[spec$target]]
          target_levels <- sort(unique(as.character(target_values[!is.na(target_values)])))
          if (length(target_levels) != 2L) {
            add("binary_target_levels", "fail", "binary target must contain exactly two non-missing classes.")
          } else {
            add("binary_target_levels", "pass", "binary target contains exactly two classes.", "info")
            positive_class <- aq_vnext_resolve_positive_class(target_values, spec$positive_class)
            if (is.null(positive_class) || !positive_class %in% target_levels) {
              add("positive_class_present", "fail", "positive_class must exist in the binary target.")
            } else {
              add("positive_class_present", "pass", paste("positive_class:", positive_class), "info")
            }
            class_counts <- table(as.character(target_values), useNA = "no")
            minority_share <- min(class_counts) / sum(class_counts)
            if (minority_share < 0.1) {
              add("class_imbalance", "warning", sprintf("Minority class share is %.3f.", minority_share), "warning")
            } else {
              add("class_imbalance", "pass", "No severe class imbalance detected.", "info")
            }
          }
          threshold_checks <- aq_validate_threshold_policy(spec$threshold_policy)
          rows <- c(rows, list(threshold_checks))
        }
        missing_features <- setdiff(spec$features, names(data))
        if (length(missing_features) && has_transformation) {
          add("feature_validation_deferred", "pass", paste("Feature validation deferred until Rodeo replay creates prepared feature(s):", paste(missing_features, collapse = ", ")), "info")
        } else {
          feature_data <- as.data.frame(data)[, spec$features, drop = FALSE]
          unsupported <- spec$features[vapply(feature_data, function(x) {
            is.list(x) || is.matrix(x) || inherits(x, "POSIXlt")
          }, logical(1L))]
          if (length(unsupported)) {
            add("feature_types", "fail", paste("Unsupported feature column types:", paste(unsupported, collapse = ", ")))
          } else {
            add("feature_types", "pass", "feature column types are supported.", "info")
          }
        }
      }
    }
  }
  partition_checks <- aq_validate_partition_spec(spec$partition, data = data)
  data.table::rbindlist(c(rows, list(partition_checks)), use.names = TRUE, fill = TRUE)
}

aq_vnext_has_validation_error <- function(validation) {
  any(tolower(validation$status) %in% c("fail", "error"))
}

aq_vnext_resolve_positive_class <- function(target, positive_class = NULL) {
  levels <- sort(unique(as.character(target[!is.na(target)])))
  if (!length(levels)) {
    return(NULL)
  }
  if (!is.null(positive_class)) {
    return(as.character(positive_class)[1L])
  }
  if ("1" %in% levels) {
    return("1")
  }
  levels[length(levels)]
}

aq_vnext_resolve_binary_classes <- function(target, positive_class = NULL) {
  levels <- sort(unique(as.character(target[!is.na(target)])))
  positive <- aq_vnext_resolve_positive_class(target, positive_class)
  negative <- setdiff(levels, positive)[1L]
  list(
    positive_class = positive,
    negative_class = negative,
    class_levels = levels
  )
}

aq_vnext_target_label <- function(data, spec) {
  if (identical(spec$task, "binary")) {
    as.numeric(as.character(data[[spec$target]]) == as.character(spec$positive_class))
  } else {
    data[[spec$target]]
  }
}

aq_vnext_make_partition <- function(data, partition) {
  n <- nrow(data)
  train_n <- max(1L, min(n - 1L, floor(n * partition$train_fraction)))
  if (identical(partition$method, "time")) {
    order_index <- order(data[[partition$split_col]], na.last = TRUE)
    train_index <- sort(order_index[seq_len(train_n)])
  } else {
    set.seed(partition$seed)
    train_index <- sort(sample.int(n, train_n))
  }
  validation_index <- setdiff(seq_len(n), train_index)
  list(
    partition_id = partition$partition_id,
    method = partition$method,
    train_index = train_index,
    validation_index = validation_index,
    summary = data.table::data.table(
      split = c("train", "validation"),
      rows = c(length(train_index), length(validation_index)),
      row_share = c(length(train_index), length(validation_index)) / n
    )
  )
}

aq_vnext_feature_schema <- function(data, features) {
  feature_data <- as.data.frame(data)[, features, drop = FALSE]
  data.table::data.table(
    feature = features,
    class = vapply(feature_data, function(x) paste(class(x), collapse = "/"), character(1L)),
    missing_count = vapply(feature_data, function(x) sum(is.na(x)), integer(1L)),
    unique_count = vapply(feature_data, function(x) length(unique(x)), integer(1L))
  )
}

aq_vnext_feature_levels <- function(data, features) {
  dt <- data.table::as.data.table(data)
  out <- lapply(intersect(features, names(dt)), function(feature) {
    x <- dt[[feature]]
    if (!is.character(x) && !is.factor(x)) {
      return(NULL)
    }
    data.table::data.table(
      feature = feature,
      level = sort(unique(as.character(x[!is.na(x)])))
    )
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) {
    return(data.table::data.table(feature = character(), level = character()))
  }
  data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
}

aq_vnext_schema_fingerprint <- function(data, columns = names(data)) {
  dt <- data.table::as.data.table(data)
  columns <- intersect(as.character(columns), names(dt))
  parts <- paste(
    columns,
    vapply(dt[, ..columns], function(x) paste(class(x), collapse = "/"), character(1L)),
    sep = ":"
  )
  paste0("rows=", nrow(dt), "|cols=", paste(parts, collapse = ","))
}

aq_vnext_null_default <- function(x, y) {
  if (is.null(x)) y else x
}

aq_vnext_drop_transformation <- function(spec) {
  prepared_spec <- spec
  prepared_spec$transformation_spec <- NULL
  prepared_spec$transformation_required <- FALSE
  prepared_spec
}

aq_vnext_transformation_summary <- function(fitted, prepared_data = NULL) {
  if (is.null(fitted)) {
    return(list(
      replay_required = FALSE,
      replay_supported = FALSE,
      transformation_spec_id = NA_character_,
      fitted_transformation_id = NA_character_,
      transformation_type = NA_character_,
      transformation_version = NA_character_,
      metadata = list()
    ))
  }
  metadata <- Rodeo::rodeo_transformation_metadata(fitted, data = prepared_data)
  list(
    replay_required = TRUE,
    replay_supported = TRUE,
    transformation_spec_id = fitted$id,
    fitted_transformation_id = fitted$id,
    transformation_type = fitted$type,
    transformation_version = fitted$version,
    input_columns = fitted$input_columns,
    output_columns = aq_vnext_null_default(fitted$output_columns, character()),
    columns_added = aq_vnext_null_default(fitted$metadata$columns_added, character()),
    columns_removed = aq_vnext_null_default(fitted$metadata$columns_removed, character()),
    schema_fingerprint = aq_vnext_schema_fingerprint(prepared_data),
    metadata = metadata,
    warnings = aq_vnext_null_default(fitted$warnings, character()),
    diagnostics = aq_vnext_null_default(fitted$diagnostics, data.table::data.table())
  )
}

aq_vnext_prepare_dataset_id <- function(dataset_id, transformation_id, role) {
  aq_vnext_id(
    paste0("prepared_", role),
    paste(aq_vnext_default(dataset_id, "dataset"), aq_vnext_default(transformation_id, "no_transform"), role, sep = "|")
  )
}

aq_vnext_fit_rodeo_transformation <- function(train_raw, validation_raw, spec) {
  if (is.null(spec$transformation_spec)) {
    return(list(
      train_data = train_raw,
      validation_data = validation_raw,
      fitted_transformation = NULL,
      serialized_fitted_transformation = NULL,
      transformation_lineage = aq_vnext_transformation_summary(NULL),
      validation = data.table::data.table(),
      warnings = character()
    ))
  }
  if (!requireNamespace("Rodeo", quietly = TRUE)) {
    stop("Rodeo is required to fit transformation_spec.", call. = FALSE)
  }
  fitted <- Rodeo::rodeo_fit_transformation(train_raw, spec$transformation_spec)
  train_prepared <- data.table::as.data.table(Rodeo::rodeo_apply_transformation(train_raw, fitted, copy_data = TRUE))
  validation_prepared <- data.table::as.data.table(Rodeo::rodeo_apply_transformation(validation_raw, fitted, copy_data = TRUE))
  transformation_validation <- Rodeo::rodeo_validate_transformation_schema(validation_raw, fitted)
  lineage <- aq_vnext_transformation_summary(fitted, train_prepared)
  lineage$training_raw_schema_fingerprint <- aq_vnext_schema_fingerprint(train_raw)
  lineage$validation_raw_schema_fingerprint <- aq_vnext_schema_fingerprint(validation_raw)
  lineage$prepared_training_schema_fingerprint <- aq_vnext_schema_fingerprint(train_prepared)
  lineage$prepared_validation_schema_fingerprint <- aq_vnext_schema_fingerprint(validation_prepared)
  lineage$prepared_training_dataset_id <- aq_vnext_prepare_dataset_id(spec$dataset_id, fitted$id, "training")
  lineage$prepared_validation_dataset_id <- aq_vnext_prepare_dataset_id(spec$dataset_id, fitted$id, "validation")
  list(
    train_data = train_prepared,
    validation_data = validation_prepared,
    fitted_transformation = fitted,
    serialized_fitted_transformation = serialize(fitted, NULL),
    transformation_lineage = lineage,
    validation = transformation_validation$diagnostics,
    warnings = unique(c(fitted$warnings, transformation_validation$warnings))
  )
}

aq_vnext_replay_rodeo_transformation <- function(raw_data, fit, dataset_id = NULL) {
  if (!isTRUE(fit$transformation_required)) {
    return(list(
      status = "not_required",
      data = raw_data,
      diagnostics = data.table::data.table(),
      warnings = character(),
      lineage = aq_vnext_transformation_summary(NULL)
    ))
  }
  if (!requireNamespace("Rodeo", quietly = TRUE)) {
    stop("Rodeo is required to replay the fitted transformation.", call. = FALSE)
  }
  fitted <- fit$fitted_transformation
  if (!inherits(fitted, "rodeo_fitted_transformation")) {
    stop("fit does not contain a fitted Rodeo transformation for replay.", call. = FALSE)
  }
  validation <- Rodeo::rodeo_validate_transformation_schema(raw_data, fitted)
  if (!isTRUE(validation$valid)) {
    stop(paste("Rodeo transformation replay failed:", paste(validation$errors, collapse = " | ")), call. = FALSE)
  }
  prepared <- data.table::as.data.table(Rodeo::rodeo_apply_transformation(raw_data, fitted, copy_data = TRUE))
  lineage <- aq_vnext_transformation_summary(fitted, prepared)
  lineage$raw_scoring_schema_fingerprint <- aq_vnext_schema_fingerprint(raw_data)
  lineage$prepared_scoring_schema_fingerprint <- aq_vnext_schema_fingerprint(prepared)
  lineage$prepared_scoring_dataset_id <- aq_vnext_prepare_dataset_id(
    aq_vnext_default(dataset_id, fit$spec$dataset_id),
    fitted$id,
    "scoring"
  )
  list(
    status = "success",
    data = prepared,
    diagnostics = validation$diagnostics,
    warnings = unique(c(fitted$warnings, validation$warnings)),
    lineage = lineage
  )
}

aq_vnext_preserve_identity_columns <- function(prepared_data, raw_data, columns) {
  dt <- data.table::as.data.table(prepared_data)
  raw <- data.table::as.data.table(raw_data)
  for (col in aq_vnext_unique_chr(columns)) {
    if (!col %in% names(dt) && col %in% names(raw) && nrow(dt) == nrow(raw)) {
      dt[, (col) := raw[[col]]]
    }
  }
  dt
}

aq_vnext_numeric_summary <- function(x, name) {
  x <- as.numeric(x)
  data.table::data.table(
    field = name,
    n = sum(!is.na(x)),
    missing = sum(is.na(x)),
    min = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
    q25 = if (all(is.na(x))) NA_real_ else as.numeric(stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE)),
    mean = if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE),
    median = if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE),
    q75 = if (all(is.na(x))) NA_real_ else as.numeric(stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE)),
    max = if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  )
}

aq_vnext_pool <- function(data, spec, label = NULL) {
  x <- as.data.frame(data)[, spec$features, drop = FALSE]
  for (col in names(x)) {
    if (is.character(x[[col]])) {
      x[[col]] <- as.factor(x[[col]])
    } else if (is.integer(x[[col]])) {
      x[[col]] <- as.numeric(x[[col]])
    }
  }
  catboost::catboost.load_pool(data = x, label = label)
}

aq_vnext_consumer_expectations <- function(render_targets = c("analytics_shinyapp", "campaigns", "future_operators")) {
  list(
    envelope_contract = "aq_artifact_envelope_v1",
    render_targets = aq_vnext_unique_chr(render_targets),
    stable_identity_required = TRUE,
    lineage_required = TRUE,
    missing_optional_payload_policy = "diagnostic_not_failure"
  )
}

aq_vnext_attach_envelope <- function(
  artifact,
  artifact_id = NULL,
  artifact_type = NULL,
  artifact_version = NULL,
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
  consumer_expectations = aq_vnext_consumer_expectations()
) {
  artifact$artifact_envelope <- aq_new_artifact_envelope(
    artifact_id = aq_vnext_default(artifact_id, aq_vnext_default(artifact$artifact_id, artifact$id)),
    artifact_type = aq_vnext_default(artifact_type, aq_vnext_default(artifact$artifact_type, artifact$metadata$artifact_type)),
    artifact_version = aq_vnext_default(artifact_version, aq_vnext_default(artifact$schema_version, artifact$version)),
    parent_artifact_ids = parent_artifact_ids,
    lineage = lineage,
    task = task,
    operator = operator,
    engine = engine,
    specification_id = specification_id,
    dataset_id = dataset_id,
    prepared_dataset_id = prepared_dataset_id,
    transformation_id = transformation_id,
    model_id = model_id,
    campaign_references = campaign_references,
    warnings = warnings,
    supported_actions = supported_actions,
    producer = producer,
    consumer_expectations = consumer_expectations,
    created_at = aq_vnext_default(artifact$created_at, aq_vnext_now())
  )
  artifact
}

aq_vnext_fit_artifact <- function(fit) {
  artifact <- list(
    artifact_id = fit$fit_id,
    artifact_type = paste0("supervised_", fit$spec$task, "_fit"),
    schema_version = "aq_supervised_fit_artifact_v1",
    task = fit$spec$task,
    engine = fit$spec$engine,
    model_id = fit$model_id,
    spec_id = fit$spec$spec_id,
    dataset_id = fit$spec$dataset_id,
    partition_id = fit$partition$partition_id,
    target = fit$spec$target,
    features = fit$spec$features,
    positive_class = aq_vnext_default(fit$spec$positive_class, NA_character_),
    negative_class = aq_vnext_default(fit$negative_class, NA_character_),
    threshold_policy = fit$threshold_policy,
    feature_schema = fit$feature_schema,
    transformation_required = isTRUE(fit$transformation_required),
    transformation_spec_id = fit$transformation_lineage$transformation_spec_id,
    fitted_transformation_id = fit$transformation_lineage$fitted_transformation_id,
    transformation_metadata = fit$transformation_lineage,
    prepared_feature_manifest = fit$feature_schema,
    prepared_training_dataset_id = fit$transformation_lineage$prepared_training_dataset_id,
    prepared_validation_dataset_id = fit$transformation_lineage$prepared_validation_dataset_id,
    replay_behavior = list(
      required = isTRUE(fit$transformation_required),
      supported = isTRUE(fit$transformation_lineage$replay_supported),
      scoring_policy = if (isTRUE(fit$transformation_required)) "raw_scoring_data_must_replay_fitted_rodeo_transformation" else "scoring_data_must_match_feature_manifest"
    ),
    training_metadata = fit$training_metadata,
    engine_params = fit$engine_params,
    warnings = fit$warnings,
    supported_downstream_actions = fit$spec$supported_downstream_actions,
    created_at = fit$created_at
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    parent_artifact_ids = fit$spec$spec_id,
    lineage = list(
      specification_id = fit$spec$spec_id,
      dataset_id = fit$spec$dataset_id,
      partition_id = fit$partition$partition_id,
      raw_training_schema_fingerprint = fit$raw_training_schema_fingerprint,
      prepared_training_schema_fingerprint = fit$prepared_training_schema_fingerprint,
      transformation = fit$transformation_lineage
    ),
    task = fit$spec$task,
    operator = "supervised_fit",
    engine = fit$spec$engine,
    specification_id = fit$spec$spec_id,
    dataset_id = fit$spec$dataset_id,
    prepared_dataset_id = fit$transformation_lineage$prepared_training_dataset_id,
    transformation_id = fit$transformation_lineage$fitted_transformation_id,
    model_id = fit$model_id,
    warnings = fit$warnings,
    supported_actions = fit$spec$supported_downstream_actions,
    producer = "aq_fit_model"
  )
  class(artifact) <- c("aq_supervised_fit_artifact", "aq_result_artifact", "list")
  artifact
}

aq_vnext_bundle_id <- function(fit) {
  aq_vnext_slug(paste("bundle", fit$model_id, fit$fit_id, fit$transformation_lineage$fitted_transformation_id, sep = "_"), "aq_bundle")
}

aq_vnext_bundle_paths <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "rds")) {
    dir <- dirname(path)
    base <- tools::file_path_sans_ext(basename(path))
    list(
      bundle_path = path,
      metadata_path = file.path(dir, paste0(base, "_metadata.rds")),
      directory = dir
    )
  } else {
    list(
      bundle_path = file.path(path, "model_bundle.rds"),
      metadata_path = file.path(path, "metadata.rds"),
      directory = path
    )
  }
}

aq_vnext_model_bundle_metadata <- function(bundle, bundle_path = NA_character_) {
  list(
    bundle_id = bundle$bundle_id,
    bundle_version = bundle$bundle_version,
    schema_version = bundle$schema_version,
    created_at = bundle$bundle_created_at,
    model_id = bundle$model_id,
    fit_id = bundle$fit_id,
    spec_id = bundle$spec$spec_id,
    task = bundle$task,
    engine = bundle$engine,
    target = bundle$spec$target,
    features = bundle$spec$features,
    partition_id = bundle$partition$partition_id,
    transformation_required = isTRUE(bundle$transformation_required),
    transformation_spec_id = bundle$transformation_lineage$transformation_spec_id,
    fitted_transformation_id = bundle$transformation_lineage$fitted_transformation_id,
    raw_training_schema_fingerprint = bundle$raw_training_schema_fingerprint,
    prepared_training_schema_fingerprint = bundle$prepared_training_schema_fingerprint,
    feature_schema = bundle$feature_schema,
    feature_levels = bundle$feature_levels,
    training_metadata = bundle$training_metadata,
    supported_actions = bundle$supported_downstream_actions,
    bundle_path = bundle_path
  )
}

aq_vnext_create_model_bundle <- function(fit) {
  if (!inherits(fit, "aq_fit_result")) {
    stop("fit must be an aq_fit_result.", call. = FALSE)
  }
  bundle <- fit
  bundle$bundle_id <- aq_vnext_bundle_id(fit)
  bundle$bundle_version <- "aq_model_bundle_v1"
  bundle$schema_version <- "aq_model_bundle_v1"
  bundle$bundle_created_at <- aq_vnext_now()
  bundle$training_data <- NULL
  bundle$validation_data <- NULL
  bundle$raw_training_data <- NULL
  bundle$raw_validation_data <- NULL
  bundle$supported_downstream_actions <- unique(c(
    fit$spec$supported_downstream_actions,
    "score",
    "attach_outcomes",
    "assess_realized",
    "monitor",
    "validate_bundle"
  ))
  bundle$bundle_metadata <- aq_vnext_model_bundle_metadata(bundle)
  bundle <- aq_vnext_attach_envelope(
    bundle,
    artifact_id = bundle$bundle_id,
    artifact_type = "model_bundle",
    artifact_version = bundle$bundle_version,
    parent_artifact_ids = fit$fit_id,
    lineage = list(
      fit_id = fit$fit_id,
      model_id = fit$model_id,
      specification_id = fit$spec$spec_id,
      transformation = fit$transformation_lineage,
      raw_training_schema_fingerprint = fit$raw_training_schema_fingerprint,
      prepared_training_schema_fingerprint = fit$prepared_training_schema_fingerprint
    ),
    task = fit$task,
    operator = "model_bundle",
    engine = fit$engine,
    specification_id = fit$spec$spec_id,
    dataset_id = fit$spec$dataset_id,
    prepared_dataset_id = fit$transformation_lineage$prepared_training_dataset_id,
    transformation_id = fit$transformation_lineage$fitted_transformation_id,
    model_id = fit$model_id,
    warnings = fit$warnings,
    supported_actions = bundle$supported_downstream_actions,
    producer = "aq_save_model_bundle"
  )
  class(bundle) <- c("aq_model_bundle", "aq_fit_result", "list")
  bundle
}

#' Save a vNext Model Bundle
#'
#' @param fit An `aq_fit_result` or `aq_model_bundle`.
#' @param path Directory path or `.rds` bundle file path.
#' @param overwrite Whether to overwrite an existing bundle file.
#'
#' @return An `aq_model_bundle_manifest`.
#' @export
aq_save_model_bundle <- function(fit, path, overwrite = FALSE) {
  bundle <- if (inherits(fit, "aq_model_bundle")) fit else aq_vnext_create_model_bundle(fit)
  paths <- aq_vnext_bundle_paths(path)
  if (!dir.exists(paths$directory)) {
    dir.create(paths$directory, recursive = TRUE, showWarnings = FALSE)
  }
  if (file.exists(paths$bundle_path) && !isTRUE(overwrite)) {
    stop("model bundle already exists. Use overwrite = TRUE to replace it.", call. = FALSE)
  }
  bundle$bundle_path <- paths$bundle_path
  bundle$bundle_metadata <- aq_vnext_model_bundle_metadata(bundle, bundle_path = paths$bundle_path)
  saveRDS(bundle, paths$bundle_path, version = 3)
  saveRDS(bundle$bundle_metadata, paths$metadata_path, version = 3)
  manifest <- list(
    bundle_id = bundle$bundle_id,
    bundle_version = bundle$bundle_version,
    status = "success",
    bundle_path = normalizePath(paths$bundle_path, winslash = "/", mustWork = FALSE),
    metadata_path = normalizePath(paths$metadata_path, winslash = "/", mustWork = FALSE),
    model_id = bundle$model_id,
    fit_id = bundle$fit_id,
    task = bundle$task,
    engine = bundle$engine,
    transformation_required = isTRUE(bundle$transformation_required),
    supported_actions = bundle$supported_downstream_actions,
    saved_at = aq_vnext_now()
  )
  class(manifest) <- c("aq_model_bundle_manifest", "list")
  manifest
}

#' Load a vNext Model Bundle
#'
#' @param path Directory path, `.rds` bundle path, or metadata path.
#' @param metadata_only Return only bundle metadata without loading the model.
#' @param validate Validate the loaded bundle.
#'
#' @return An `aq_model_bundle` or `aq_model_bundle_metadata`.
#' @export
aq_load_model_bundle <- function(path, metadata_only = FALSE, validate = TRUE) {
  paths <- aq_vnext_bundle_paths(path)
  if (grepl("_metadata\\.rds$", path, ignore.case = TRUE) || identical(tolower(basename(path)), "metadata.rds")) {
    paths$metadata_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (identical(tolower(basename(path)), "metadata.rds")) {
      paths$bundle_path <- file.path(dirname(path), "model_bundle.rds")
    }
  }
  if (isTRUE(metadata_only)) {
    if (!file.exists(paths$metadata_path)) {
      stop("bundle metadata file does not exist: ", paths$metadata_path, call. = FALSE)
    }
    metadata <- readRDS(paths$metadata_path)
    class(metadata) <- c("aq_model_bundle_metadata", "list")
    return(metadata)
  }
  if (!file.exists(paths$bundle_path)) {
    stop("model bundle file does not exist: ", paths$bundle_path, call. = FALSE)
  }
  bundle <- readRDS(paths$bundle_path)
  if (!inherits(bundle, "aq_model_bundle")) {
    stop("Serialized object is not an aq_model_bundle.", call. = FALSE)
  }
  if (isTRUE(validate)) {
    diagnostics <- aq_validate_model_bundle(bundle)
    if (aq_vnext_has_validation_error(diagnostics)) {
      stop("model bundle validation failed: ", paste(diagnostics[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
    }
  }
  bundle
}

#' Validate a vNext Model Bundle
#'
#' @param bundle An `aq_model_bundle`, `aq_fit_result`, or saved bundle path.
#'
#' @return A `data.table` of deterministic diagnostics.
#' @export
aq_validate_model_bundle <- function(bundle) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  if (is.character(bundle) && length(bundle) == 1L) {
    loaded <- tryCatch(aq_load_model_bundle(bundle, metadata_only = FALSE, validate = FALSE), error = identity)
    if (inherits(loaded, "error")) {
      add("bundle_readable", "fail", conditionMessage(loaded))
      return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
    }
    bundle <- loaded
  }
  if (!inherits(bundle, "aq_model_bundle")) {
    add("bundle_class", "fail", "bundle must be loaded by aq_load_model_bundle() or created from aq_save_model_bundle().")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("bundle_class", "pass", "bundle inherits from aq_model_bundle.", "info")
  required <- c("bundle_id", "bundle_version", "spec", "model", "model_id", "fit_id", "feature_schema", "training_metadata", "fit_artifact")
  missing <- required[!vapply(required, function(field) !is.null(bundle[[field]]), logical(1L))]
  if (length(missing)) {
    add("required_fields", "fail", paste("Missing required bundle field(s):", paste(missing, collapse = ", ")))
  } else {
    add("required_fields", "pass", "required bundle fields are present.", "info")
  }
  if (!identical(bundle$bundle_version, "aq_model_bundle_v1")) {
    add("bundle_version", "fail", paste("Unsupported bundle version:", bundle$bundle_version))
  } else {
    add("bundle_version", "pass", "bundle version is supported.", "info")
  }
  if (!inherits(bundle$spec, "aq_model_spec")) {
    add("model_spec", "fail", "bundle spec is not an aq_model_spec.")
  } else {
    add("model_spec", "pass", "model specification is present.", "info")
  }
  if (!bundle$task %in% c("regression", "binary")) {
    add("task_supported", "fail", "bundle task is not supported.")
  } else {
    add("task_supported", "pass", paste("task:", bundle$task), "info")
  }
  if (!identical(bundle$engine, "catboost")) {
    add("engine_supported", "fail", "bundle engine is not supported.")
  } else {
    add("engine_supported", "pass", "CatBoost engine is supported.", "info")
  }
  if (is.null(bundle$model)) {
    add("model_available", "fail", "bundle model object is missing.")
  } else {
    add("model_available", "pass", "model object is present.", "info")
  }
  if (isTRUE(bundle$transformation_required)) {
    if (is.null(bundle$serialized_fitted_transformation) && is.null(bundle$fitted_transformation)) {
      add("transformation_available", "fail", "bundle requires transformation replay but no fitted transformation is present.")
    } else if (!is.null(bundle$fitted_transformation) && !inherits(bundle$fitted_transformation, "rodeo_fitted_transformation")) {
      add("transformation_available", "fail", "fitted transformation does not inherit from rodeo_fitted_transformation.")
    } else {
      add("transformation_available", "pass", "fitted Rodeo transformation is available.", "info")
    }
  } else {
    add("transformation_available", "pass", "no transformation replay required.", "info")
  }
  if (!all(bundle$spec$features %in% bundle$feature_schema$feature)) {
    add("feature_manifest", "fail", "feature schema does not cover all model features.")
  } else {
    add("feature_manifest", "pass", "feature manifest covers model features.", "info")
  }
  actions <- c("score", "attach_outcomes", "assess_realized", "monitor")
  if (!all(actions %in% bundle$supported_downstream_actions)) {
    add("supported_actions", "warning", "bundle does not advertise all standard downstream actions.", "warning")
  } else {
    add("supported_actions", "pass", "standard downstream actions are advertised.", "info")
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' Fit a vNext CatBoost Model
#'
#' @param spec An `aq_model_spec`.
#' @param data Training/source data.
#' @param validation_data Optional explicit validation data.
#'
#' @return An `aq_fit_result`.
#' @export
aq_fit_model <- function(spec, data, validation_data = NULL) {
  start_time <- Sys.time()
  if (!requireNamespace("catboost", quietly = TRUE)) {
    stop("The catboost package is required for aq_fit_model() with engine = 'catboost'.", call. = FALSE)
  }
  dt <- data.table::as.data.table(data.table::copy(data))
  validation <- aq_validate_model_spec(spec, data = dt)
  if (aq_vnext_has_validation_error(validation)) {
    stop(paste(validation[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  }
  spec <- spec
  binary_classes <- NULL
  if (identical(spec$task, "binary")) {
    binary_classes <- aq_vnext_resolve_binary_classes(dt[[spec$target]], spec$positive_class)
    spec$positive_class <- binary_classes$positive_class
    spec$threshold_policy$positive_class <- binary_classes$positive_class
    spec$threshold_policy$negative_class <- binary_classes$negative_class
  }
  set.seed(spec$seed)
  if (is.null(validation_data)) {
    partition <- aq_vnext_make_partition(dt, spec$partition)
    train_raw_dt <- data.table::copy(dt[partition$train_index])
    validation_raw_dt <- data.table::copy(dt[partition$validation_index])
  } else {
    validation_raw_dt <- data.table::as.data.table(data.table::copy(validation_data))
    validation_checks <- aq_validate_model_spec(spec, data = validation_raw_dt)
    if (aq_vnext_has_validation_error(validation_checks)) {
      stop("validation_data does not satisfy model spec: ", paste(validation_checks[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
    }
    train_raw_dt <- dt
    partition <- list(
      partition_id = spec$partition$partition_id,
      method = "explicit_validation_data",
      train_index = seq_len(nrow(train_raw_dt)),
      validation_index = seq_len(nrow(validation_raw_dt)),
      summary = data.table::data.table(
        split = c("train", "validation"),
        rows = c(nrow(train_raw_dt), nrow(validation_raw_dt)),
        row_share = c(NA_real_, NA_real_)
      )
    )
  }
  transformation <- aq_vnext_fit_rodeo_transformation(train_raw_dt, validation_raw_dt, spec)
  train_dt <- transformation$train_data
  validation_dt <- transformation$validation_data
  prepared_spec <- aq_vnext_drop_transformation(spec)
  prepared_train_validation <- aq_validate_model_spec(prepared_spec, data = train_dt)
  prepared_validation_checks <- aq_validate_model_spec(prepared_spec, data = validation_dt)
  if (aq_vnext_has_validation_error(prepared_train_validation)) {
    stop("prepared training data does not satisfy model spec after Rodeo replay: ", paste(prepared_train_validation[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  }
  if (aq_vnext_has_validation_error(prepared_validation_checks)) {
    stop("prepared validation data does not satisfy model spec after Rodeo replay: ", paste(prepared_validation_checks[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  }
  train_pool <- aq_vnext_pool(train_dt, spec, label = aq_vnext_target_label(train_dt, spec))
  validation_pool <- aq_vnext_pool(validation_dt, spec, label = aq_vnext_target_label(validation_dt, spec))
  params <- spec$engine_params
  model <- catboost::catboost.train(
    learn_pool = train_pool,
    test_pool = validation_pool,
    params = params
  )
  end_time <- Sys.time()
  fit <- list(
    fit_id = aq_vnext_id(paste0("fit_catboost_", spec$task), seed = spec$seed),
    status = "success",
    schema_version = "aq_fit_result_v1",
    spec = spec,
    model_id = spec$model_id,
    model = model,
    engine = spec$engine,
    task = spec$task,
    engine_params = params,
    partition = partition,
    threshold_policy = spec$threshold_policy,
    positive_class = aq_vnext_default(spec$positive_class, NA_character_),
    negative_class = if (!is.null(binary_classes)) binary_classes$negative_class else NA_character_,
    feature_schema = aq_vnext_feature_schema(train_dt, spec$features),
    feature_levels = aq_vnext_feature_levels(train_dt, spec$features),
    raw_feature_schema = aq_vnext_feature_schema(dt, intersect(names(dt), names(data.table::as.data.table(dt)))),
    transformation_required = !is.null(spec$transformation_spec),
    transformation_spec = spec$transformation_spec,
    fitted_transformation = transformation$fitted_transformation,
    serialized_fitted_transformation = transformation$serialized_fitted_transformation,
    transformation_lineage = transformation$transformation_lineage,
    transformation_diagnostics = transformation$validation,
    raw_training_schema_fingerprint = aq_vnext_schema_fingerprint(train_raw_dt),
    raw_validation_schema_fingerprint = aq_vnext_schema_fingerprint(validation_raw_dt),
    prepared_training_schema_fingerprint = aq_vnext_schema_fingerprint(train_dt),
    prepared_validation_schema_fingerprint = aq_vnext_schema_fingerprint(validation_dt),
    training_metadata = list(
      train_rows = nrow(train_dt),
      validation_rows = nrow(validation_dt),
      raw_train_rows = nrow(train_raw_dt),
      raw_validation_rows = nrow(validation_raw_dt),
      feature_count = length(spec$features),
      transformation_required = !is.null(spec$transformation_spec),
      prepared_training_dataset_id = transformation$transformation_lineage$prepared_training_dataset_id,
      prepared_validation_dataset_id = transformation$transformation_lineage$prepared_validation_dataset_id,
      target_summary_train = if (identical(spec$task, "binary")) {
        mean(aq_vnext_target_label(train_dt, spec), na.rm = TRUE)
      } else {
        mean(train_dt[[spec$target]], na.rm = TRUE)
      },
      target_summary_validation = if (identical(spec$task, "binary")) {
        mean(aq_vnext_target_label(validation_dt, spec), na.rm = TRUE)
      } else {
        mean(validation_dt[[spec$target]], na.rm = TRUE)
      },
      started_at = start_time,
      completed_at = end_time,
      elapsed_seconds = as.numeric(difftime(end_time, start_time, units = "secs"))
    ),
    training_data = train_dt,
    validation_data = validation_dt,
    raw_training_data = train_raw_dt,
    raw_validation_data = validation_raw_dt,
    validation = validation,
    prepared_validation = data.table::rbindlist(list(prepared_train_validation, prepared_validation_checks), use.names = TRUE, fill = TRUE),
    warnings = transformation$warnings,
    created_at = end_time
  )
  fit$fit_artifact <- aq_vnext_fit_artifact(fit)
  class(fit) <- c("aq_fit_result", "list")
  fit
}

#' Predict From a vNext Fit Result
#'
#' @param fit An `aq_fit_result`.
#' @param new_data Optional new data.
#' @param dataset Dataset to score when `new_data` is not supplied.
#' @param dataset_id Optional dataset identifier.
#' @param threshold_policy Optional threshold policy override for binary prediction.
#'
#' @return An `aq_prediction_result`.
#' @export
aq_predict_model <- function(fit, new_data = NULL, dataset = c("validation", "training", "all", "new"), dataset_id = NULL, threshold_policy = NULL) {
  if (!inherits(fit, "aq_fit_result")) {
    stop("fit must be an aq_fit_result.", call. = FALSE)
  }
  dataset <- match.arg(dataset)
  if (!is.null(new_data)) {
    score_dt <- data.table::as.data.table(data.table::copy(new_data))
    if (isTRUE(fit$transformation_required)) {
      score_dt <- aq_vnext_replay_rodeo_transformation(score_dt, fit, dataset_id = dataset_id)$data
    }
    split <- "new"
  } else if (identical(dataset, "training")) {
    score_dt <- data.table::copy(fit$training_data)
    split <- "train"
  } else if (identical(dataset, "validation")) {
    score_dt <- data.table::copy(fit$validation_data)
    split <- "validation"
  } else if (identical(dataset, "all")) {
    train_dt <- data.table::copy(fit$training_data)
    validation_dt <- data.table::copy(fit$validation_data)
    train_dt[, .split := "train"]
    validation_dt[, .split := "validation"]
    score_dt <- data.table::rbindlist(list(train_dt, validation_dt), use.names = TRUE, fill = TRUE)
    split <- NULL
  } else {
    stop("new_data is required when dataset = 'new'.", call. = FALSE)
  }
  missing_features <- setdiff(fit$spec$features, names(score_dt))
  if (length(missing_features)) {
    stop("new data is missing feature(s): ", paste(missing_features, collapse = ", "), call. = FALSE)
  }
  pool <- aq_vnext_pool(score_dt, fit$spec, label = if (fit$spec$target %in% names(score_dt)) aq_vnext_target_label(score_dt, fit$spec) else NULL)
  out <- data.table::copy(score_dt)
  if (!".split" %in% names(out)) {
    out[, .split := split]
  }
  threshold_policy <- aq_vnext_default(threshold_policy, fit$threshold_policy)
  if (identical(fit$spec$task, "binary")) {
    prob <- as.numeric(catboost::catboost.predict(fit$model, pool = pool, prediction_type = "Probability"))
    out[, PositiveProbability := prob]
    out[, Predict := ifelse(PositiveProbability >= threshold_policy$threshold, fit$positive_class, fit$negative_class)]
    if (fit$spec$target %in% names(out)) {
      out[, actual_binary := as.numeric(as.character(get(fit$spec$target)) == as.character(fit$positive_class))]
    }
  } else {
    pred <- as.numeric(catboost::catboost.predict(fit$model, pool = pool, prediction_type = "RawFormulaVal"))
    out[, Predict := pred]
    if (fit$spec$target %in% names(out)) {
      out[, residual := as.numeric(get(fit$spec$target)) - Predict]
    }
  }
  prediction_id <- aq_vnext_id(paste0("prediction_catboost_", fit$spec$task), seed = fit$spec$seed)
  artifact <- new_table_artifact(
    id = prediction_id,
    title = if (identical(fit$spec$task, "binary")) "CatBoost Binary Predictions" else "CatBoost Regression Predictions",
    data = out,
    source_generator = "aq_predict_model",
    tags = c("vnext", "prediction", "catboost", fit$spec$task),
    dependencies = fit$fit_id,
    version = "aq_prediction_artifact_v1",
    metadata = list(
      artifact_type = paste0("supervised_", fit$spec$task, "_prediction"),
      task = fit$spec$task,
      model_id = fit$model_id,
      fit_id = fit$fit_id,
      dataset_id = aq_vnext_default(dataset_id, fit$spec$dataset_id),
      prediction_col = "Predict",
      probability_col = if (identical(fit$spec$task, "binary")) "PositiveProbability" else NA_character_,
      target_col = fit$spec$target,
      prediction_scale = if (identical(fit$spec$task, "binary")) "probability_and_class" else "response",
      threshold_policy = threshold_policy,
      positive_class = aq_vnext_default(fit$positive_class, NA_character_),
      negative_class = aq_vnext_default(fit$negative_class, NA_character_),
      row_count = nrow(out),
      supported_downstream_actions = c("assess", "compare")
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = prediction_id,
    artifact_type = paste0("supervised_", fit$spec$task, "_prediction"),
    artifact_version = "aq_prediction_artifact_v1",
    parent_artifact_ids = fit$fit_id,
    lineage = list(
      fit_id = fit$fit_id,
      model_id = fit$model_id,
      specification_id = fit$spec$spec_id,
      dataset_id = aq_vnext_default(dataset_id, fit$spec$dataset_id),
      prediction_col = "Predict",
      probability_col = if (identical(fit$spec$task, "binary")) "PositiveProbability" else NA_character_
    ),
    task = fit$spec$task,
    operator = "prediction",
    engine = fit$spec$engine,
    specification_id = fit$spec$spec_id,
    dataset_id = aq_vnext_default(dataset_id, fit$spec$dataset_id),
    model_id = fit$model_id,
    supported_actions = c("assess", "compare"),
    producer = "aq_predict_model"
  )
  result <- list(
    prediction_id = prediction_id,
    status = "success",
    schema_version = "aq_prediction_result_v1",
    task = fit$spec$task,
    fit_id = fit$fit_id,
    model_id = fit$model_id,
    dataset_id = aq_vnext_default(dataset_id, fit$spec$dataset_id),
    prediction_col = "Predict",
    probability_col = if (identical(fit$spec$task, "binary")) "PositiveProbability" else NA_character_,
    target_col = fit$spec$target,
    prediction_scale = if (identical(fit$spec$task, "binary")) "probability_and_class" else "response",
    threshold_policy = threshold_policy,
    positive_class = aq_vnext_default(fit$positive_class, NA_character_),
    negative_class = aq_vnext_default(fit$negative_class, NA_character_),
    data = out,
    artifact = artifact,
    warnings = character(),
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_prediction_result", "list")
  result
}

#' Create a vNext Scoring Specification
#'
#' @param fit Optional fitted vNext model artifact used to populate defaults.
#' @param row_id_cols Optional row identity columns. Generated row ids are used when omitted.
#' @param outcome_col Optional realized outcome column when already available in scoring data.
#' @param threshold_policy Optional binary threshold policy.
#' @param prediction_type Prediction type. Supports `"response"` for regression and
#'   `"probability_and_class"` for binary classification.
#' @param requested_output_fields Fields to preserve in the scoring artifact.
#' @param requested_monitoring_summaries Monitoring summaries requested by downstream consumers.
#' @param schema_strictness Schema strictness. Supports `"moderate"` and `"strict"`.
#' @param extra_column_behavior Extra scoring-data column policy. Supports `"allow"`,
#'   `"drop"`, and `"fail"`.
#' @param scoring_spec_id Optional scoring specification identifier.
#'
#' @return An `aq_scoring_spec` object.
#' @export
aq_scoring_spec <- function(
  fit = NULL,
  row_id_cols = NULL,
  outcome_col = NULL,
  threshold_policy = NULL,
  prediction_type = NULL,
  requested_output_fields = NULL,
  requested_monitoring_summaries = c("schema", "missingness", "prediction_distribution"),
  schema_strictness = "moderate",
  extra_column_behavior = "allow",
  scoring_spec_id = NULL
) {
  schema_strictness <- match.arg(tolower(schema_strictness), c("moderate", "strict"))
  extra_column_behavior <- match.arg(tolower(extra_column_behavior), c("allow", "drop", "fail"))
  task <- if (!is.null(fit) && inherits(fit, "aq_fit_result")) fit$task else NA_character_
  engine <- if (!is.null(fit) && inherits(fit, "aq_fit_result")) fit$engine else NA_character_
  if (is.null(prediction_type)) {
    prediction_type <- if (identical(task, "binary")) "probability_and_class" else "response"
  }
  if (is.null(requested_output_fields)) {
    requested_output_fields <- if (identical(task, "binary")) {
      c("row_identity", "PositiveProbability", "Predict")
    } else {
      c("row_identity", "Predict")
    }
  }
  if (is.null(scoring_spec_id)) {
    scoring_spec_id <- aq_vnext_id("scoring_spec")
  }
  if (is.null(threshold_policy) && !is.null(fit) && inherits(fit, "aq_fit_result")) {
    threshold_policy <- fit$threshold_policy
  }
  spec <- list(
    scoring_spec_id = as.character(scoring_spec_id)[1L],
    schema_version = "aq_scoring_spec_v1",
    fit_id = if (!is.null(fit) && inherits(fit, "aq_fit_result")) fit$fit_id else NA_character_,
    model_id = if (!is.null(fit) && inherits(fit, "aq_fit_result")) fit$model_id else NA_character_,
    model_spec_id = if (!is.null(fit) && inherits(fit, "aq_fit_result")) fit$spec$spec_id else NA_character_,
    task = task,
    engine = engine,
    prediction_type = as.character(prediction_type)[1L],
    row_id_cols = aq_vnext_unique_chr(row_id_cols),
    generated_row_id = !length(aq_vnext_unique_chr(row_id_cols)),
    outcome_col = if (is.null(outcome_col)) NULL else as.character(outcome_col)[1L],
    threshold_policy = threshold_policy,
    requested_output_fields = aq_vnext_unique_chr(requested_output_fields),
    requested_monitoring_summaries = aq_vnext_unique_chr(requested_monitoring_summaries),
    schema_strictness = schema_strictness,
    extra_column_behavior = extra_column_behavior,
    supported_next_actions = c("attach_outcomes", "assess_realized", "monitor", "compare"),
    created_at = aq_vnext_now()
  )
  class(spec) <- c("aq_scoring_spec", "list")
  spec
}

aq_validate_scoring_data <- function(fit, data, scoring_spec) {
  rows <- list()
  add <- function(check, status, message, severity = status) {
    rows[[length(rows) + 1L]] <<- aq_vnext_validation_table(check, status, message, severity)
  }
  if (!inherits(fit, "aq_fit_result")) {
    add("fit_class", "fail", "fit must be an aq_fit_result.")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  add("fit_class", "pass", "fit inherits from aq_fit_result.", "info")
  if (!inherits(scoring_spec, "aq_scoring_spec")) {
    add("scoring_spec_class", "fail", "scoring_spec must be created by aq_scoring_spec().")
  } else {
    add("scoring_spec_class", "pass", "scoring_spec inherits from aq_scoring_spec.", "info")
  }
  if (!is.data.frame(data) && !data.table::is.data.table(data)) {
    add("scoring_data_frame_like", "fail", "new_data must be data.frame/data.table-like.")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }
  dt <- data.table::as.data.table(data)
  if (!anyDuplicated(fit$spec$features)) {
    add("model_features_unique", "pass", "model feature manifest is unique.", "info")
  } else {
    add("model_features_unique", "fail", "model feature manifest contains duplicate feature names.")
  }
  missing_features <- setdiff(fit$spec$features, names(dt))
  if (length(missing_features)) {
    add("required_features_exist", "fail", paste("Missing scoring feature(s):", paste(missing_features, collapse = ", ")))
  } else {
    add("required_features_exist", "pass", "all required model features exist in scoring data.", "info")
  }
  extra_cols <- setdiff(names(dt), c(fit$spec$features, scoring_spec$row_id_cols, scoring_spec$outcome_col))
  target_extra <- fit$spec$target %in% extra_cols
  if (isTRUE(target_extra) && is.null(scoring_spec$outcome_col)) {
    add("target_leakage_column", "warning", paste("Target column", fit$spec$target, "is present but not declared as outcome_col."), "warning")
  } else {
    add("target_leakage_column", "pass", "no undeclared target leakage column detected.", "info")
  }
  if (length(extra_cols) && identical(scoring_spec$extra_column_behavior, "fail")) {
    add("extra_columns_policy", "fail", paste("Extra scoring column(s) are not allowed:", paste(extra_cols, collapse = ", ")))
  } else if (length(extra_cols)) {
    add("extra_columns_policy", "warning", paste("Extra scoring column(s) will not be used for scoring:", paste(extra_cols, collapse = ", ")), "warning")
  } else {
    add("extra_columns_policy", "pass", "no extra scoring columns detected.", "info")
  }
  row_id_cols <- scoring_spec$row_id_cols
  if (length(row_id_cols)) {
    missing_id <- setdiff(row_id_cols, names(dt))
    if (length(missing_id)) {
      add("row_identity_available", "fail", paste("Missing row identity column(s):", paste(missing_id, collapse = ", ")))
    } else {
      add("row_identity_available", "pass", "row identity columns exist.", "info")
      key <- do.call(paste, c(dt[, ..row_id_cols], sep = "\r"))
      if (anyDuplicated(key)) {
        add("row_identity_unique", "fail", "row identity values must be unique for scoring.")
      } else {
        add("row_identity_unique", "pass", "row identity values are unique.", "info")
      }
    }
  } else {
    add("row_identity_generated", "pass", "explicit row ids were not supplied; .aq_row_id will be generated.", "info")
  }
  if (!length(missing_features)) {
    feature_schema_df <- as.data.frame(fit$feature_schema)
    for (feature in fit$spec$features) {
      old_class <- feature_schema_df$class[match(feature, feature_schema_df$feature)]
      if (!length(old_class)) {
        old_class <- NA_character_
      }
      new_class <- paste(class(dt[[feature]]), collapse = "/")
      old_base <- strsplit(old_class[1L], "/", fixed = TRUE)[[1L]][1L]
      new_base <- strsplit(new_class, "/", fixed = TRUE)[[1L]][1L]
      compatible <- identical(old_base, new_base) ||
        (old_base %in% c("integer", "numeric") && new_base %in% c("integer", "numeric")) ||
        (old_base %in% c("character", "factor") && new_base %in% c("character", "factor"))
      if (!compatible) {
        add("feature_type_compatibility", "fail", paste("Feature", feature, "has incompatible type. Expected", old_class[1L], "but found", new_class))
      }
      if (anyNA(dt[[feature]])) {
        add("feature_missingness", "warning", paste("Feature", feature, "contains", sum(is.na(dt[[feature]])), "missing value(s)."), "warning")
      }
      if (feature %in% fit$feature_schema$feature && old_base %in% c("character", "factor")) {
        train_values <- if (!is.null(fit$feature_levels) && nrow(fit$feature_levels)) {
          fit$feature_levels$level[fit$feature_levels$feature == feature]
        } else if (!is.null(fit$training_data) && feature %in% names(fit$training_data)) {
          unique(as.character(fit$training_data[[feature]]))
        } else {
          character()
        }
        new_values <- unique(as.character(dt[[feature]]))
        unseen <- setdiff(new_values[!is.na(new_values)], train_values[!is.na(train_values)])
        if (length(train_values) && length(unseen)) {
          add("unseen_categorical_levels", "warning", paste("Feature", feature, "contains unseen level(s):", paste(utils::head(unseen, 5L), collapse = ", ")), "warning")
        }
      }
    }
    if (!any(vapply(rows, function(x) identical(x$check[[1L]], "feature_type_compatibility"), logical(1L)))) {
      add("feature_type_compatibility", "pass", "scoring feature types are compatible with the fit manifest.", "info")
    }
  }
  if (identical(fit$task, "binary")) {
    threshold_checks <- aq_validate_threshold_policy(scoring_spec$threshold_policy)
    rows <- c(rows, list(threshold_checks))
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

aq_vnext_scoring_output <- function(data, row_id_cols, generated_row_id) {
  dt <- data.table::as.data.table(data.table::copy(data))
  if (isTRUE(generated_row_id)) {
    dt[, .aq_row_id := seq_len(.N)]
    return(list(data = dt, row_id_cols = ".aq_row_id"))
  }
  list(data = dt, row_id_cols = row_id_cols)
}

#' Score New Data With a vNext Model
#'
#' @param fit An `aq_fit_result`.
#' @param new_data Data to score.
#' @param row_id_cols Optional row identity columns.
#' @param scoring_spec Optional `aq_scoring_spec`.
#' @param threshold_policy Optional binary threshold policy override.
#' @param outcome_col Optional realized outcome column already present in `new_data`.
#' @param dataset_id Optional scoring dataset identifier.
#'
#' @return An `aq_scoring_result`.
#' @export
aq_score_model <- function(
  fit,
  new_data,
  row_id_cols = NULL,
  scoring_spec = NULL,
  threshold_policy = NULL,
  outcome_col = NULL,
  dataset_id = NULL
) {
  if (!inherits(fit, "aq_fit_result")) {
    stop("fit must be an aq_fit_result.", call. = FALSE)
  }
  if (is.null(scoring_spec)) {
    scoring_spec <- aq_scoring_spec(
      fit = fit,
      row_id_cols = row_id_cols,
      outcome_col = outcome_col,
      threshold_policy = aq_vnext_default(threshold_policy, fit$threshold_policy)
    )
  } else {
    if (!inherits(scoring_spec, "aq_scoring_spec")) {
      stop("scoring_spec must be created by aq_scoring_spec().", call. = FALSE)
    }
    if (!is.null(row_id_cols)) {
      scoring_spec$row_id_cols <- aq_vnext_unique_chr(row_id_cols)
      scoring_spec$generated_row_id <- !length(scoring_spec$row_id_cols)
    }
    if (!is.null(outcome_col)) scoring_spec$outcome_col <- as.character(outcome_col)[1L]
    if (!is.null(threshold_policy)) scoring_spec$threshold_policy <- threshold_policy
  }
  raw_score_dt <- data.table::as.data.table(data.table::copy(new_data))
  replay <- aq_vnext_replay_rodeo_transformation(raw_score_dt, fit, dataset_id = dataset_id)
  score_dt <- replay$data
  score_dt <- aq_vnext_preserve_identity_columns(
    score_dt,
    raw_score_dt,
    c(scoring_spec$row_id_cols, scoring_spec$outcome_col)
  )
  validation <- aq_validate_scoring_data(fit, score_dt, scoring_spec)
  if (aq_vnext_has_validation_error(validation)) {
    stop(paste(validation[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  }
  row_identity <- aq_vnext_scoring_output(score_dt, scoring_spec$row_id_cols, scoring_spec$generated_row_id)
  score_dt <- row_identity$data
  row_id_cols <- row_identity$row_id_cols
  scoring_features <- score_dt[, fit$spec$features, with = FALSE]
  pool <- aq_vnext_pool(scoring_features, fit$spec, label = NULL)
  output <- score_dt[, ..row_id_cols]
  output[, .split := "scoring"]
  threshold_policy <- aq_vnext_default(scoring_spec$threshold_policy, fit$threshold_policy)
  decision_history <- data.table::data.table()
  if (identical(fit$task, "binary")) {
    prob <- as.numeric(catboost::catboost.predict(fit$model, pool = pool, prediction_type = "Probability"))
    output[, PositiveProbability := prob]
    output[, Predict := ifelse(PositiveProbability >= threshold_policy$threshold, fit$positive_class, fit$negative_class)]
    decision_history <- data.table::data.table(
      threshold_decision_id = aq_vnext_id("threshold_decision", seed = fit$spec$seed),
      threshold_policy_id = threshold_policy$threshold_policy_id,
      threshold = threshold_policy$threshold,
      positive_class = fit$positive_class,
      negative_class = fit$negative_class,
      applied_at = aq_vnext_now()
    )
  } else {
    pred <- as.numeric(catboost::catboost.predict(fit$model, pool = pool, prediction_type = "RawFormulaVal"))
    output[, Predict := pred]
  }
  outcome_status <- "not_attached"
  if (!is.null(scoring_spec$outcome_col) && scoring_spec$outcome_col %in% names(score_dt)) {
    output[, (fit$spec$target) := score_dt[[scoring_spec$outcome_col]]]
    outcome_status <- "attached_at_scoring"
    if (identical(fit$task, "binary")) {
      output[, actual_binary := as.numeric(as.character(get(fit$spec$target)) == as.character(fit$positive_class))]
    } else {
      output[, residual := as.numeric(get(fit$spec$target)) - as.numeric(Predict)]
    }
  }
  prediction_cols <- if (identical(fit$task, "binary")) c("PositiveProbability", "Predict") else "Predict"
  scoring_id <- aq_vnext_id(paste0("scoring_catboost_", fit$task), seed = fit$spec$seed)
  artifact <- new_table_artifact(
    id = scoring_id,
    title = if (identical(fit$task, "binary")) "CatBoost Binary Scoring" else "CatBoost Regression Scoring",
    data = output,
    source_generator = "aq_score_model",
    tags = c("vnext", "scoring", "catboost", fit$task),
    dependencies = fit$fit_id,
    version = "aq_scoring_artifact_v1",
    metadata = list(
      artifact_type = paste0("supervised_", fit$task, "_scoring"),
      scoring_id = scoring_id,
      scoring_spec_id = scoring_spec$scoring_spec_id,
      model_id = fit$model_id,
      fit_id = fit$fit_id,
      model_spec_id = fit$spec$spec_id,
      task = fit$task,
      engine = fit$engine,
      dataset_id = aq_vnext_default(dataset_id, fit$spec$dataset_id),
      raw_dataset_id = aq_vnext_default(dataset_id, fit$spec$dataset_id),
      prepared_dataset_id = replay$lineage$prepared_scoring_dataset_id,
      transformation_replay_status = replay$status,
      transformation_required = isTRUE(fit$transformation_required),
      transformation_spec_id = replay$lineage$transformation_spec_id,
      fitted_transformation_id = replay$lineage$fitted_transformation_id,
      transformation_metadata = replay$lineage,
      prediction_cols = prediction_cols,
      row_id_cols = row_id_cols,
      outcome_status = outcome_status,
      threshold_policy = threshold_policy,
      supported_downstream_actions = scoring_spec$supported_next_actions
    )
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    artifact_id = scoring_id,
    artifact_type = paste0("supervised_", fit$task, "_scoring"),
    artifact_version = "aq_scoring_artifact_v1",
    parent_artifact_ids = fit$fit_id,
    lineage = list(
      fit_id = fit$fit_id,
      model_id = fit$model_id,
      specification_id = fit$spec$spec_id,
      scoring_spec_id = scoring_spec$scoring_spec_id,
      raw_scoring_dataset_fingerprint = aq_vnext_schema_fingerprint(raw_score_dt),
      prepared_scoring_dataset_fingerprint = aq_vnext_schema_fingerprint(score_dt),
      transformation_replay = replay$lineage
    ),
    task = fit$task,
    operator = "scoring",
    engine = fit$engine,
    specification_id = fit$spec$spec_id,
    dataset_id = aq_vnext_default(dataset_id, fit$spec$dataset_id),
    prepared_dataset_id = replay$lineage$prepared_scoring_dataset_id,
    transformation_id = replay$lineage$fitted_transformation_id,
    model_id = fit$model_id,
    warnings = unique(c(validation[status == "warning", message], replay$warnings)),
    supported_actions = scoring_spec$supported_next_actions,
    producer = "aq_score_model"
  )
  result <- list(
    scoring_id = scoring_id,
    prediction_id = scoring_id,
    status = "success",
    schema_version = "aq_scoring_result_v1",
    scoring_spec = scoring_spec,
    scoring_spec_id = scoring_spec$scoring_spec_id,
    task = fit$task,
    engine = fit$engine,
    fit_id = fit$fit_id,
    model_id = fit$model_id,
    model_spec_id = fit$spec$spec_id,
    dataset_id = aq_vnext_default(dataset_id, fit$spec$dataset_id),
    raw_scoring_dataset_fingerprint = aq_vnext_schema_fingerprint(raw_score_dt),
    prepared_scoring_dataset_fingerprint = aq_vnext_schema_fingerprint(score_dt),
    scoring_dataset_fingerprint = aq_vnext_schema_fingerprint(score_dt),
    prepared_dataset_id = replay$lineage$prepared_scoring_dataset_id,
    row_id_cols = row_id_cols,
    generated_row_id = scoring_spec$generated_row_id,
    feature_manifest = fit$feature_schema,
    transformation_required = isTRUE(fit$transformation_required),
    transformation_replay = list(
      status = replay$status,
      warnings = replay$warnings,
      diagnostics = replay$diagnostics,
      lineage = replay$lineage
    ),
    prediction_col = "Predict",
    prediction_cols = prediction_cols,
    probability_col = if (identical(fit$task, "binary")) "PositiveProbability" else NA_character_,
    target_col = fit$spec$target,
    prediction_scale = if (identical(fit$task, "binary")) "probability_and_class" else "response",
    threshold_policy = threshold_policy,
    threshold_decision_history = decision_history,
    positive_class = aq_vnext_default(fit$positive_class, NA_character_),
    negative_class = aq_vnext_default(fit$negative_class, NA_character_),
    schema_diagnostics = data.table::rbindlist(list(validation, replay$diagnostics), use.names = TRUE, fill = TRUE),
    warnings = unique(c(validation[status == "warning", message], replay$warnings)),
    runtime_metadata = list(scored_rows = nrow(output), scored_at = aq_vnext_now()),
    outcome_status = outcome_status,
    outcome_attachment = NULL,
    data = output,
    artifact = artifact,
    supported_downstream_actions = scoring_spec$supported_next_actions,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_scoring_result", "aq_prediction_result", "list")
  result
}

#' Apply a Binary Threshold Policy to Existing Scoring Evidence
#'
#' @param scoring An `aq_scoring_result` from a binary model.
#' @param threshold_policy A threshold policy from `aq_threshold_policy()`.
#'
#' @return An updated `aq_scoring_result` with preserved probabilities.
#' @export
aq_apply_threshold_policy <- function(scoring, threshold_policy) {
  if (!inherits(scoring, "aq_scoring_result")) {
    stop("scoring must be an aq_scoring_result.", call. = FALSE)
  }
  if (!identical(scoring$task, "binary")) {
    stop("threshold policies can only be applied to binary scoring artifacts.", call. = FALSE)
  }
  checks <- aq_validate_threshold_policy(threshold_policy)
  if (aq_vnext_has_validation_error(checks)) {
    stop(paste(checks[status %in% c("fail", "error"), message], collapse = " "), call. = FALSE)
  }
  out <- scoring
  out$data <- data.table::as.data.table(data.table::copy(scoring$data))
  positive_class <- aq_vnext_default(threshold_policy$positive_class, scoring$positive_class)
  negative_class <- aq_vnext_default(threshold_policy$negative_class, scoring$negative_class)
  out$data[, Predict := ifelse(PositiveProbability >= threshold_policy$threshold, positive_class, negative_class)]
  decision <- data.table::data.table(
    threshold_decision_id = aq_vnext_id("threshold_decision"),
    threshold_policy_id = threshold_policy$threshold_policy_id,
    threshold = threshold_policy$threshold,
    positive_class = positive_class,
    negative_class = negative_class,
    applied_at = aq_vnext_now()
  )
  out$threshold_policy <- threshold_policy
  out$positive_class <- positive_class
  out$negative_class <- negative_class
  out$threshold_decision_history <- data.table::rbindlist(list(scoring$threshold_decision_history, decision), use.names = TRUE, fill = TRUE)
  out$artifact$payload <- out$data
  out$artifact$metadata$threshold_policy <- threshold_policy
  out$artifact$metadata$threshold_decision_count <- nrow(out$threshold_decision_history)
  out
}

#' Attach Realized Outcomes to a Scoring Artifact
#'
#' @param scoring An `aq_scoring_result`.
#' @param outcomes Data containing row identity and realized outcomes.
#' @param outcome_col Outcome column in `outcomes`.
#' @param row_id_cols Optional row identity columns. Defaults to the scoring artifact row ids.
#' @param outcome_timestamp Optional timestamp for the realized outcomes.
#' @param dataset_id Optional outcome dataset identifier.
#' @param allow_partial Whether missing realized outcomes are allowed.
#'
#' @return An updated `aq_scoring_result` with an outcome attachment artifact.
#' @export
aq_attach_outcomes <- function(
  scoring,
  outcomes,
  outcome_col,
  row_id_cols = NULL,
  outcome_timestamp = NULL,
  dataset_id = NULL,
  allow_partial = TRUE
) {
  if (!inherits(scoring, "aq_scoring_result")) {
    stop("scoring must be an aq_scoring_result.", call. = FALSE)
  }
  outcome_dt <- data.table::as.data.table(data.table::copy(outcomes))
  row_id_cols <- aq_vnext_default(aq_vnext_unique_chr(row_id_cols), scoring$row_id_cols)
  outcome_col <- as.character(outcome_col)[1L]
  missing_cols <- setdiff(c(row_id_cols, outcome_col), names(outcome_dt))
  if (length(missing_cols)) {
    stop("outcomes are missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  key <- do.call(paste, c(outcome_dt[, ..row_id_cols], sep = "\r"))
  if (anyDuplicated(key)) {
    stop("outcomes contain duplicate row identity values.", call. = FALSE)
  }
  if (identical(scoring$task, "regression") && !is.numeric(outcome_dt[[outcome_col]])) {
    stop("regression outcomes must be numeric.", call. = FALSE)
  }
  if (identical(scoring$task, "binary")) {
    outcome_levels <- unique(as.character(outcome_dt[[outcome_col]][!is.na(outcome_dt[[outcome_col]])]))
    bad_levels <- setdiff(outcome_levels, c(scoring$positive_class, scoring$negative_class))
    if (length(bad_levels)) {
      stop("binary outcomes contain class(es) not seen by the scoring artifact: ", paste(bad_levels, collapse = ", "), call. = FALSE)
    }
  }
  scored_dt <- data.table::as.data.table(data.table::copy(scoring$data))
  outcome_keep <- outcome_dt[, c(row_id_cols, outcome_col), with = FALSE]
  data.table::setnames(outcome_keep, outcome_col, scoring$target_col)
  merged <- merge(scored_dt, outcome_keep, by = row_id_cols, all.x = TRUE, all.y = FALSE, sort = FALSE)
  missing_outcomes <- sum(is.na(merged[[scoring$target_col]]))
  if (missing_outcomes && !isTRUE(allow_partial)) {
    stop("outcomes are missing for ", missing_outcomes, " scored row(s).", call. = FALSE)
  }
  extra_key <- do.call(paste, c(outcome_dt[, ..row_id_cols], sep = "\r"))
  scored_key <- do.call(paste, c(scored_dt[, ..row_id_cols], sep = "\r"))
  extra_rows <- sum(!extra_key %in% scored_key)
  if (identical(scoring$task, "binary")) {
    merged[, actual_binary := as.numeric(as.character(get(scoring$target_col)) == as.character(scoring$positive_class))]
  } else {
    merged[, residual := as.numeric(get(scoring$target_col)) - as.numeric(Predict)]
  }
  attachment_id <- aq_vnext_id("outcome_attachment")
  attachment <- list(
    artifact_id = attachment_id,
    artifact_type = paste0("supervised_", scoring$task, "_outcome_attachment"),
    schema_version = "aq_outcome_attachment_artifact_v1",
    scoring_id = scoring$scoring_id,
    model_id = scoring$model_id,
    fit_id = scoring$fit_id,
    outcome_dataset_id = aq_vnext_default(dataset_id, NA_character_),
    row_id_cols = row_id_cols,
    outcome_col = scoring$target_col,
    scored_rows = nrow(scored_dt),
    attached_rows = nrow(scored_dt) - missing_outcomes,
    missing_outcome_rows = missing_outcomes,
    unexpected_extra_outcome_rows = extra_rows,
    partial_outcomes = missing_outcomes > 0L,
    outcome_timestamp = aq_vnext_default(outcome_timestamp, aq_vnext_now()),
    created_at = aq_vnext_now()
  )
  attachment <- aq_vnext_attach_envelope(
    attachment,
    parent_artifact_ids = scoring$scoring_id,
    lineage = list(
      scoring_id = scoring$scoring_id,
      model_id = scoring$model_id,
      fit_id = scoring$fit_id,
      outcome_dataset_id = aq_vnext_default(dataset_id, NA_character_),
      row_id_cols = row_id_cols,
      missing_outcome_rows = missing_outcomes
    ),
    task = scoring$task,
    operator = "outcome_attachment",
    engine = scoring$engine,
    specification_id = scoring$model_spec_id,
    dataset_id = aq_vnext_default(dataset_id, scoring$dataset_id),
    model_id = scoring$model_id,
    warnings = if (missing_outcomes) paste("Missing outcomes for", missing_outcomes, "scored row(s).") else character(),
    supported_actions = c("assess_realized", "monitor", "compare"),
    producer = "aq_attach_outcomes"
  )
  class(attachment) <- c("aq_outcome_attachment_artifact", "aq_result_artifact", "list")
  out <- scoring
  out$data <- merged
  out$outcome_status <- if (missing_outcomes) "partially_attached" else "attached"
  out$outcome_attachment <- attachment
  out$artifact$payload <- merged
  out$artifact$metadata$outcome_status <- out$outcome_status
  out$artifact$metadata$outcome_attachment_id <- attachment_id
  out$warnings <- unique(c(out$warnings, if (missing_outcomes) paste("Missing outcomes for", missing_outcomes, "scored row(s).") else character()))
  out
}

#' Assess Realized Scoring Evidence
#'
#' @param scoring An `aq_scoring_result` with attached outcomes.
#' @param by Optional subgroup columns present in scoring evidence.
#'
#' @return An `aq_assessment_result` linked to the scoring artifact.
#' @export
aq_assess_scoring <- function(scoring, by = NULL) {
  if (!inherits(scoring, "aq_scoring_result")) {
    stop("scoring must be an aq_scoring_result.", call. = FALSE)
  }
  if (!scoring$target_col %in% names(scoring$data)) {
    stop("scoring evidence does not contain attached outcomes.", call. = FALSE)
  }
  assessed <- aq_assess_model(fit = NULL, predictions = scoring, by = by)
  assessed$scoring_id <- scoring$scoring_id
  assessed$outcome_attachment_id <- if (!is.null(scoring$outcome_attachment)) scoring$outcome_attachment$artifact_id else NA_character_
  assessed$evaluated_row_count <- sum(!is.na(scoring$data[[scoring$target_col]]))
  assessed$missing_outcome_count <- sum(is.na(scoring$data[[scoring$target_col]]))
  assessed$assessment_artifact$scoring_id <- scoring$scoring_id
  assessed$assessment_artifact$outcome_attachment_id <- assessed$outcome_attachment_id
  assessed$assessment_artifact$evaluated_row_count <- assessed$evaluated_row_count
  assessed$assessment_artifact$missing_outcome_count <- assessed$missing_outcome_count
  assessed
}

#' Create Bounded Monitoring Evidence From Scoring Artifacts
#'
#' @param scoring An `aq_scoring_result`.
#' @param baseline Optional compatible `aq_scoring_result` or `aq_assessment_result`.
#'
#' @return An `aq_scoring_monitoring_result`.
#' @export
aq_monitor_scoring <- function(scoring, baseline = NULL) {
  if (!inherits(scoring, "aq_scoring_result")) {
    stop("scoring must be an aq_scoring_result.", call. = FALSE)
  }
  baseline_id <- NA_character_
  baseline_summary <- data.table::data.table()
  if (!is.null(baseline)) {
    if (inherits(baseline, "aq_scoring_result")) {
      if (!identical(scoring$task, baseline$task) || !identical(scoring$model_id, baseline$model_id)) {
        stop("baseline scoring artifact must have the same task and model_id.", call. = FALSE)
      }
      baseline_id <- baseline$scoring_id
      baseline_col <- if (identical(baseline$task, "binary")) baseline$probability_col else baseline$prediction_col
      baseline_summary <- aq_vnext_numeric_summary(baseline$data[[baseline_col]], paste0("baseline_", baseline_col))
    } else if (inherits(baseline, "aq_assessment_result")) {
      if (!identical(scoring$model_id, baseline$model_id)) {
        stop("baseline assessment must have the same model_id.", call. = FALSE)
      }
      baseline_id <- baseline$assessment_id
      baseline_summary <- data.table::copy(baseline$metrics)
    } else {
      stop("baseline must be an aq_scoring_result or aq_assessment_result.", call. = FALSE)
    }
  }
  monitoring_id <- aq_vnext_id("scoring_monitoring")
  distribution_col <- if (identical(scoring$task, "binary")) scoring$probability_col else scoring$prediction_col
  prediction_distribution <- aq_vnext_numeric_summary(scoring$data[[distribution_col]], distribution_col)
  probability_distribution <- if (identical(scoring$task, "binary")) {
    aq_vnext_numeric_summary(scoring$data[[scoring$probability_col]], scoring$probability_col)
  } else {
    data.table::data.table()
  }
  class_distribution <- if (identical(scoring$task, "binary")) {
    scoring$data[, .(rows = .N), by = .(predicted_class = Predict)][order(predicted_class)]
  } else {
    data.table::data.table()
  }
  outcome_distribution <- if (scoring$target_col %in% names(scoring$data)) {
    if (identical(scoring$task, "binary")) {
      scoring$data[, .(rows = .N), by = .(actual_class = get(scoring$target_col))][order(actual_class)]
    } else {
      aq_vnext_numeric_summary(scoring$data[[scoring$target_col]], scoring$target_col)
    }
  } else {
    data.table::data.table()
  }
  evidence <- list(
    schema_diagnostics = scoring$schema_diagnostics,
    transformation_replay = scoring$transformation_replay,
    transformation_compatibility = data.table::data.table(
      check = "transformation_replay",
      status = aq_vnext_default(scoring$transformation_replay$status, "not_required"),
      detail = if (isTRUE(scoring$transformation_required)) {
        paste(
          "fitted_transformation_id=",
          aq_vnext_default(scoring$transformation_replay$lineage$fitted_transformation_id, NA_character_),
          "; prepared_dataset_id=",
          aq_vnext_default(scoring$prepared_dataset_id, NA_character_),
          sep = ""
        )
      } else {
        "no transformation replay required"
      }
    ),
    prediction_distribution = prediction_distribution,
    probability_distribution = probability_distribution,
    predicted_class_distribution = class_distribution,
    outcome_distribution = outcome_distribution,
    baseline_summary = baseline_summary
  )
  artifact <- list(
    artifact_id = monitoring_id,
    artifact_type = paste0("supervised_", scoring$task, "_scoring_monitoring"),
    schema_version = "aq_scoring_monitoring_artifact_v1",
    scoring_id = scoring$scoring_id,
    model_id = scoring$model_id,
    fit_id = scoring$fit_id,
    dataset_id = scoring$dataset_id,
    baseline_id = baseline_id,
    scoring_population_size = nrow(scoring$data),
    outcome_status = scoring$outcome_status,
    warning_count = length(scoring$warnings),
    evidence = evidence,
    supported_downstream_actions = c("compare", "campaign_review"),
    created_at = aq_vnext_now()
  )
  artifact <- aq_vnext_attach_envelope(
    artifact,
    parent_artifact_ids = aq_vnext_unique_chr(c(scoring$scoring_id, baseline_id)),
    lineage = list(
      scoring_id = scoring$scoring_id,
      baseline_id = baseline_id,
      model_id = scoring$model_id,
      fit_id = scoring$fit_id,
      outcome_status = scoring$outcome_status,
      evidence_names = names(evidence)
    ),
    task = scoring$task,
    operator = "monitoring",
    engine = scoring$engine,
    specification_id = scoring$model_spec_id,
    dataset_id = scoring$dataset_id,
    prepared_dataset_id = scoring$prepared_dataset_id,
    transformation_id = scoring$transformation_replay$lineage$fitted_transformation_id,
    model_id = scoring$model_id,
    warnings = scoring$warnings,
    supported_actions = c("compare", "campaign_review"),
    producer = "aq_monitor_scoring"
  )
  class(artifact) <- c("aq_scoring_monitoring_artifact", "aq_result_artifact", "list")
  result <- list(
    monitoring_id = monitoring_id,
    status = "success",
    schema_version = "aq_scoring_monitoring_result_v1",
    scoring_id = scoring$scoring_id,
    model_id = scoring$model_id,
    task = scoring$task,
    engine = scoring$engine,
    baseline_id = baseline_id,
    evidence = evidence,
    monitoring_artifact = artifact,
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_scoring_monitoring_result", "list")
  result
}

aq_regression_metrics <- function(data, target_col, prediction_col = "Predict") {
  actual <- as.numeric(data[[target_col]])
  pred <- as.numeric(data[[prediction_col]])
  keep <- stats::complete.cases(actual, pred)
  actual <- actual[keep]
  pred <- pred[keep]
  residual <- actual - pred
  denom <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  data.table::data.table(
    metric = c("rmse", "mae", "r2", "mean_residual", "n"),
    value = c(
      sqrt(mean(residual^2, na.rm = TRUE)),
      mean(abs(residual), na.rm = TRUE),
      if (is.finite(denom) && denom > 0) 1 - sum(residual^2, na.rm = TRUE) / denom else NA_real_,
      mean(residual, na.rm = TRUE),
      length(residual)
    )
  )
}

aq_vnext_auc <- function(actual_binary, score) {
  keep <- stats::complete.cases(actual_binary, score)
  actual_binary <- as.numeric(actual_binary[keep])
  score <- as.numeric(score[keep])
  n_pos <- sum(actual_binary == 1)
  n_neg <- sum(actual_binary == 0)
  if (!n_pos || !n_neg) {
    return(NA_real_)
  }
  ranks <- rank(score, ties.method = "average")
  (sum(ranks[actual_binary == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

aq_binary_metrics <- function(data, target_col, prediction_col = "Predict", probability_col = "PositiveProbability", positive_class, threshold = 0.5) {
  actual <- as.numeric(as.character(data[[target_col]]) == as.character(positive_class))
  prob <- pmin(pmax(as.numeric(data[[probability_col]]), 1e-15), 1 - 1e-15)
  pred <- as.numeric(as.character(data[[prediction_col]]) == as.character(positive_class))
  keep <- stats::complete.cases(actual, prob, pred)
  actual <- actual[keep]
  prob <- prob[keep]
  pred <- pred[keep]
  tp <- sum(actual == 1 & pred == 1)
  tn <- sum(actual == 0 & pred == 0)
  fp <- sum(actual == 0 & pred == 1)
  fn <- sum(actual == 1 & pred == 0)
  safe_div <- function(num, den) if (is.finite(den) && den > 0) num / den else NA_real_
  precision <- safe_div(tp, tp + fp)
  recall <- safe_div(tp, tp + fn)
  specificity <- safe_div(tn, tn + fp)
  f1 <- if (is.finite(precision + recall) && precision + recall > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    NA_real_
  }
  data.table::data.table(
    metric = c("logloss", "auc", "accuracy", "precision", "recall", "f1", "specificity", "sensitivity", "threshold", "n"),
    value = c(
      -mean(actual * log(prob) + (1 - actual) * log(1 - prob), na.rm = TRUE),
      aq_vnext_auc(actual, prob),
      safe_div(tp + tn, tp + tn + fp + fn),
      precision,
      recall,
      f1,
      specificity,
      recall,
      threshold,
      length(actual)
    )
  )
}

aq_binary_confusion_matrix <- function(data, target_col, prediction_col = "Predict", positive_class, negative_class) {
  actual <- ifelse(as.character(data[[target_col]]) == as.character(positive_class), positive_class, negative_class)
  pred <- ifelse(as.character(data[[prediction_col]]) == as.character(positive_class), positive_class, negative_class)
  data.table::as.data.table(as.data.frame.matrix(table(
    actual = factor(actual, levels = c(negative_class, positive_class)),
    predicted = factor(pred, levels = c(negative_class, positive_class))
  )), keep.rownames = "actual")
}

aq_binary_calibration <- function(data, target_col, probability_col = "PositiveProbability", positive_class, bins = 10L) {
  dt <- data.table::as.data.table(data.table::copy(data))
  dt[, actual_binary := as.numeric(as.character(get(target_col)) == as.character(positive_class))]
  dt[, probability_bin := cut(
    as.numeric(get(probability_col)),
    breaks = seq(0, 1, length.out = bins + 1L),
    include.lowest = TRUE
  )]
  dt[, .(
    rows = .N,
    mean_probability = mean(as.numeric(get(probability_col)), na.rm = TRUE),
    observed_rate = mean(actual_binary, na.rm = TRUE)
  ), by = probability_bin][order(probability_bin)]
}

#' Assess a vNext Prediction Result
#'
#' @param fit Optional `aq_fit_result`.
#' @param predictions An `aq_prediction_result`.
#' @param by Optional subgroup columns for subgroup metrics.
#'
#' @return An `aq_assessment_result`.
#' @export
aq_assess_model <- function(fit = NULL, predictions, by = NULL) {
  if (!inherits(predictions, "aq_prediction_result")) {
    stop("predictions must be an aq_prediction_result.", call. = FALSE)
  }
  if (identical(predictions$task, "binary")) {
    return(aq_assess_binary_model(fit = fit, predictions = predictions, by = by))
  }
  data <- data.table::as.data.table(data.table::copy(predictions$data))
  target_col <- predictions$target_col
  if (!target_col %in% names(data)) {
    stop("prediction data must contain the target column for assessment.", call. = FALSE)
  }
  overall <- aq_regression_metrics(data, target_col = target_col)
  subgroup <- data.table::data.table()
  by <- intersect(aq_vnext_unique_chr(by), names(data))
  if (length(by)) {
    subgroup <- data[, aq_regression_metrics(.SD, target_col = target_col), by = by]
  }
  residuals <- data[, .(
    .split,
    actual = as.numeric(get(target_col)),
    prediction = as.numeric(Predict),
    residual = as.numeric(get(target_col)) - as.numeric(Predict)
  )]
  calibration <- data[, .(
    rows = .N,
    mean_actual = mean(as.numeric(get(target_col)), na.rm = TRUE),
    mean_prediction = mean(as.numeric(Predict), na.rm = TRUE),
    mean_residual = mean(as.numeric(get(target_col)) - as.numeric(Predict), na.rm = TRUE)
  ), by = .split]
  assessment_id <- aq_vnext_id("assessment_catboost_regression", seed = if (!is.null(fit)) fit$spec$seed else NULL)
  tables <- list(
    metrics = overall,
    residuals = residuals,
    calibration = calibration,
    subgroup_metrics = subgroup
  )
  artifacts <- list(
    metrics = new_table_artifact(
      id = paste0(assessment_id, "_metrics"),
      title = "Regression Metrics",
      data = overall,
      source_generator = "aq_assess_model",
      tags = c("vnext", "assessment", "regression", "metrics"),
      dependencies = predictions$prediction_id,
      version = "aq_assessment_table_v1",
      metadata = list(assessment_id = assessment_id, artifact_type = "regression_metrics")
    ),
    residuals = new_table_artifact(
      id = paste0(assessment_id, "_residuals"),
      title = "Regression Residuals",
      data = residuals,
      source_generator = "aq_assess_model",
      tags = c("vnext", "assessment", "regression", "residuals"),
      dependencies = predictions$prediction_id,
      version = "aq_assessment_table_v1",
      metadata = list(assessment_id = assessment_id, artifact_type = "regression_residuals")
    ),
    calibration = new_table_artifact(
      id = paste0(assessment_id, "_calibration"),
      title = "Regression Calibration",
      data = calibration,
      source_generator = "aq_assess_model",
      tags = c("vnext", "assessment", "regression", "calibration"),
      dependencies = predictions$prediction_id,
      version = "aq_assessment_table_v1",
      metadata = list(assessment_id = assessment_id, artifact_type = "regression_calibration")
    )
  )
  if (nrow(subgroup)) {
    artifacts$subgroup_metrics <- new_table_artifact(
      id = paste0(assessment_id, "_subgroup_metrics"),
      title = "Regression Subgroup Metrics",
      data = subgroup,
      source_generator = "aq_assess_model",
      tags = c("vnext", "assessment", "regression", "subgroup"),
      dependencies = predictions$prediction_id,
      version = "aq_assessment_table_v1",
      metadata = list(assessment_id = assessment_id, artifact_type = "regression_subgroup_metrics")
    )
  }
  assessment_artifact <- list(
    artifact_id = assessment_id,
    artifact_type = "supervised_regression_assessment",
    schema_version = "aq_regression_assessment_artifact_v1",
    prediction_id = predictions$prediction_id,
    fit_id = predictions$fit_id,
    model_id = predictions$model_id,
    dataset_id = predictions$dataset_id,
    target_col = target_col,
    prediction_col = predictions$prediction_col,
    metrics = overall,
    calibration = calibration,
    subgroup_metrics = subgroup,
    supported_downstream_actions = c("compare"),
    created_at = aq_vnext_now()
  )
  assessment_artifact <- aq_vnext_attach_envelope(
    assessment_artifact,
    parent_artifact_ids = predictions$prediction_id,
    lineage = list(
      prediction_id = predictions$prediction_id,
      fit_id = predictions$fit_id,
      model_id = predictions$model_id,
      dataset_id = predictions$dataset_id,
      metric_count = nrow(overall),
      subgroup_available = nrow(subgroup) > 0L
    ),
    task = "regression",
    operator = "assessment",
    engine = if (!is.null(fit)) fit$engine else NA_character_,
    specification_id = if (!is.null(fit)) fit$spec$spec_id else NA_character_,
    dataset_id = predictions$dataset_id,
    model_id = predictions$model_id,
    supported_actions = c("compare", "report", "campaign_review", "knowledge_promotion"),
    producer = "aq_assess_model"
  )
  class(assessment_artifact) <- c("aq_regression_assessment_artifact", "aq_result_artifact", "list")
  result <- list(
    assessment_id = assessment_id,
    status = "success",
    schema_version = "aq_assessment_result_v1",
    prediction_id = predictions$prediction_id,
    fit_id = predictions$fit_id,
    model_id = predictions$model_id,
    metrics = overall,
    tables = tables,
    artifacts = artifacts,
    assessment_artifact = assessment_artifact,
    comparison_ready = TRUE,
    warnings = character(),
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_assessment_result", "list")
  result
}

aq_assess_binary_model <- function(fit = NULL, predictions, by = NULL) {
  data <- data.table::as.data.table(data.table::copy(predictions$data))
  target_col <- predictions$target_col
  if (!target_col %in% names(data)) {
    stop("prediction data must contain the target column for assessment.", call. = FALSE)
  }
  if (!"PositiveProbability" %in% names(data)) {
    stop("binary prediction data must contain PositiveProbability for assessment.", call. = FALSE)
  }
  positive_class <- predictions$positive_class
  negative_class <- predictions$negative_class
  threshold <- predictions$threshold_policy$threshold
  overall <- aq_binary_metrics(
    data,
    target_col = target_col,
    prediction_col = predictions$prediction_col,
    probability_col = predictions$probability_col,
    positive_class = positive_class,
    threshold = threshold
  )
  confusion <- aq_binary_confusion_matrix(
    data,
    target_col = target_col,
    prediction_col = predictions$prediction_col,
    positive_class = positive_class,
    negative_class = negative_class
  )
  calibration <- aq_binary_calibration(
    data,
    target_col = target_col,
    probability_col = predictions$probability_col,
    positive_class = positive_class
  )
  probability_diagnostics <- data.table::data.table(
    probability_col = predictions$probability_col,
    min_probability = min(data[[predictions$probability_col]], na.rm = TRUE),
    mean_probability = mean(data[[predictions$probability_col]], na.rm = TRUE),
    max_probability = max(data[[predictions$probability_col]], na.rm = TRUE),
    threshold = threshold,
    positive_class = positive_class,
    negative_class = negative_class
  )
  subgroup <- data.table::data.table()
  by <- intersect(aq_vnext_unique_chr(by), names(data))
  if (length(by)) {
    subgroup <- data[, aq_binary_metrics(
      .SD,
      target_col = target_col,
      prediction_col = predictions$prediction_col,
      probability_col = predictions$probability_col,
      positive_class = positive_class,
      threshold = threshold
    ), by = by]
  }
  assessment_id <- aq_vnext_id("assessment_catboost_binary", seed = if (!is.null(fit)) fit$spec$seed else NULL)
  tables <- list(
    metrics = overall,
    confusion_matrix = confusion,
    calibration = calibration,
    probability_diagnostics = probability_diagnostics,
    subgroup_metrics = subgroup
  )
  artifacts <- list(
    metrics = new_table_artifact(
      id = paste0(assessment_id, "_metrics"),
      title = "Binary Classification Metrics",
      data = overall,
      source_generator = "aq_assess_model",
      tags = c("vnext", "assessment", "binary", "metrics"),
      dependencies = predictions$prediction_id,
      version = "aq_assessment_table_v1",
      metadata = list(assessment_id = assessment_id, artifact_type = "binary_metrics")
    ),
    confusion_matrix = new_table_artifact(
      id = paste0(assessment_id, "_confusion_matrix"),
      title = "Binary Confusion Matrix",
      data = confusion,
      source_generator = "aq_assess_model",
      tags = c("vnext", "assessment", "binary", "confusion_matrix"),
      dependencies = predictions$prediction_id,
      version = "aq_assessment_table_v1",
      metadata = list(assessment_id = assessment_id, artifact_type = "binary_confusion_matrix")
    ),
    calibration = new_table_artifact(
      id = paste0(assessment_id, "_calibration"),
      title = "Binary Calibration Summary",
      data = calibration,
      source_generator = "aq_assess_model",
      tags = c("vnext", "assessment", "binary", "calibration"),
      dependencies = predictions$prediction_id,
      version = "aq_assessment_table_v1",
      metadata = list(assessment_id = assessment_id, artifact_type = "binary_calibration")
    ),
    probability_diagnostics = new_table_artifact(
      id = paste0(assessment_id, "_probability_diagnostics"),
      title = "Binary Probability Diagnostics",
      data = probability_diagnostics,
      source_generator = "aq_assess_model",
      tags = c("vnext", "assessment", "binary", "probability"),
      dependencies = predictions$prediction_id,
      version = "aq_assessment_table_v1",
      metadata = list(assessment_id = assessment_id, artifact_type = "binary_probability_diagnostics")
    )
  )
  if (nrow(subgroup)) {
    artifacts$subgroup_metrics <- new_table_artifact(
      id = paste0(assessment_id, "_subgroup_metrics"),
      title = "Binary Subgroup Metrics",
      data = subgroup,
      source_generator = "aq_assess_model",
      tags = c("vnext", "assessment", "binary", "subgroup"),
      dependencies = predictions$prediction_id,
      version = "aq_assessment_table_v1",
      metadata = list(assessment_id = assessment_id, artifact_type = "binary_subgroup_metrics")
    )
  }
  assessment_artifact <- list(
    artifact_id = assessment_id,
    artifact_type = "supervised_binary_assessment",
    schema_version = "aq_binary_assessment_artifact_v1",
    prediction_id = predictions$prediction_id,
    fit_id = predictions$fit_id,
    model_id = predictions$model_id,
    dataset_id = predictions$dataset_id,
    target_col = target_col,
    prediction_col = predictions$prediction_col,
    probability_col = predictions$probability_col,
    positive_class = positive_class,
    negative_class = negative_class,
    threshold_policy = predictions$threshold_policy,
    metrics = overall,
    confusion_matrix = confusion,
    calibration = calibration,
    probability_diagnostics = probability_diagnostics,
    subgroup_metrics = subgroup,
    supported_downstream_actions = c("compare"),
    created_at = aq_vnext_now()
  )
  assessment_artifact <- aq_vnext_attach_envelope(
    assessment_artifact,
    parent_artifact_ids = predictions$prediction_id,
    lineage = list(
      prediction_id = predictions$prediction_id,
      fit_id = predictions$fit_id,
      model_id = predictions$model_id,
      dataset_id = predictions$dataset_id,
      metric_count = nrow(overall),
      confusion_matrix_available = nrow(confusion) > 0L,
      calibration_available = nrow(calibration) > 0L,
      subgroup_available = nrow(subgroup) > 0L
    ),
    task = "binary",
    operator = "assessment",
    engine = if (!is.null(fit)) fit$engine else NA_character_,
    specification_id = if (!is.null(fit)) fit$spec$spec_id else NA_character_,
    dataset_id = predictions$dataset_id,
    model_id = predictions$model_id,
    supported_actions = c("compare", "report", "campaign_review", "knowledge_promotion"),
    producer = "aq_assess_model"
  )
  class(assessment_artifact) <- c("aq_binary_assessment_artifact", "aq_result_artifact", "list")
  result <- list(
    assessment_id = assessment_id,
    status = "success",
    schema_version = "aq_assessment_result_v1",
    task = "binary",
    prediction_id = predictions$prediction_id,
    fit_id = predictions$fit_id,
    model_id = predictions$model_id,
    metrics = overall,
    tables = tables,
    artifacts = artifacts,
    assessment_artifact = assessment_artifact,
    comparison_ready = TRUE,
    warnings = character(),
    created_at = aq_vnext_now()
  )
  class(result) <- c("aq_assessment_result", "list")
  result
}

#' List vNext Model Operator Capabilities
#'
#' @return A `data.table` of implemented vNext model capabilities.
#' @export
aq_model_operator_capabilities <- function() {
  data.table::rbindlist(list(
    data.table::data.table(
      operator_id = "catboost_regression_vnext",
      display_name = "CatBoost Regression vNext",
      task_family = "supervised_regression",
      engine = "catboost",
      specification = "aq_model_spec(task = 'regression', engine = 'catboost')",
      fit = "aq_fit_model",
      prediction = "aq_predict_model",
      scoring = "aq_score_model",
      assessment = "aq_assess_model",
      scoring_assessment = "aq_assess_scoring",
      monitoring = "aq_monitor_scoring",
      comparison = "comparison_ready_artifacts_only",
      explanation = "not_implemented_phase_4",
      threshold_policy = NA_character_,
      delayed_outcomes = TRUE,
      threshold_reapplication = FALSE,
      prediction_types = "response",
      valid_next_actions = "score,attach_outcomes,assess_realized,monitor,compare",
      status = "implemented",
      limitations = "CPU CatBoost regression only; no SHAP/report/tuning/deployment side effects."
    ),
    data.table::data.table(
      operator_id = "catboost_binary_vnext",
      display_name = "CatBoost Binary Classification vNext",
      task_family = "supervised_binary",
      engine = "catboost",
      specification = "aq_model_spec(task = 'binary', engine = 'catboost')",
      fit = "aq_fit_model",
      prediction = "aq_predict_model",
      scoring = "aq_score_model",
      assessment = "aq_assess_model",
      scoring_assessment = "aq_assess_scoring",
      monitoring = "aq_monitor_scoring",
      comparison = "comparison_ready_artifacts_only",
      explanation = "not_implemented_phase_4",
      threshold_policy = "aq_threshold_policy",
      delayed_outcomes = TRUE,
      threshold_reapplication = TRUE,
      prediction_types = "positive_probability,class_decision",
      valid_next_actions = "score,apply_threshold,attach_outcomes,assess_realized,monitor,compare",
      status = "implemented",
      limitations = "CPU CatBoost binary classification only; no SHAP/report/tuning/threshold optimization/deployment side effects."
    )
  ), use.names = TRUE, fill = TRUE)
}

aq_vnext_catboost_fixture <- function(n = 220L) {
  set.seed(20260712)
  dt <- data.table::data.table(
    id = seq_len(n),
    event_date = as.Date("2025-01-01") + seq_len(n) - 1L,
    channel = sample(c("Search", "Email", "Social", "Direct"), n, replace = TRUE),
    region = sample(c("West", "Midwest", "South"), n, replace = TRUE),
    spend = stats::runif(n, 50, 500),
    clicks = stats::rpois(n, 40),
    discount = stats::runif(n, 0, 0.3)
  )
  dt[, revenue := 10 + 2.5 * spend + 1.8 * clicks - 120 * discount +
    data.table::fifelse(channel == "Search", 80, 0) +
    data.table::fifelse(region == "West", 40, 0) +
    stats::rnorm(.N, 0, 35)]
  dt[]
}

aq_vnext_catboost_binary_fixture <- function(n = 260L) {
  set.seed(20260713)
  dt <- data.table::data.table(
    id = seq_len(n),
    event_date = as.Date("2025-01-01") + seq_len(n) - 1L,
    channel = sample(c("Search", "Email", "Social", "Direct"), n, replace = TRUE),
    region = sample(c("West", "Midwest", "South"), n, replace = TRUE),
    spend = stats::runif(n, 50, 500),
    clicks = stats::rpois(n, 40),
    discount = stats::runif(n, 0, 0.3)
  )
  linear <- -2 + 0.008 * dt$spend + 0.03 * dt$clicks - 2.5 * dt$discount +
    ifelse(dt$channel == "Search", 0.8, 0) +
    ifelse(dt$region == "West", 0.4, 0)
  prob <- 1 / (1 + exp(-linear))
  dt[, converted := factor(ifelse(stats::runif(.N) < prob, "yes", "no"), levels = c("no", "yes"))]
  dt[]
}

#' QA for vNext CatBoost Regression
#'
#' @return A `data.table` of deterministic QA checks.
#' @export
qa_vnext_catboost_regression <- function() {
  rows <- list()
  add <- function(check, passed, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_catboost_regression",
      check = check,
      status = if (isTRUE(passed)) "pass" else "fail",
      message = message
    )
  }
  dt <- aq_vnext_catboost_fixture()
  spec <- aq_model_spec(
    task = "regression",
    engine = "catboost",
    target = "revenue",
    features = c("channel", "region", "spend", "clicks", "discount"),
    partition = aq_partition_spec(method = "time", split_col = "event_date", train_fraction = 0.8, seed = 20260712),
    engine_params = list(iterations = 25L, depth = 4L, learning_rate = 0.08, verbose = FALSE),
    metrics = c("rmse", "mae", "r2"),
    seed = 20260712,
    dataset_id = "qa_vnext_catboost_fixture"
  )
  validation <- aq_validate_model_spec(spec, dt)
  add("spec_constructed", inherits(spec, "aq_model_spec"))
  add("validation_passes", !aq_vnext_has_validation_error(validation), paste(validation$message, collapse = " | "))
  duplicate_spec <- aq_model_spec(target = "revenue", features = c("spend", "spend"), seed = 20260712)
  duplicate_validation <- aq_validate_model_spec(duplicate_spec, dt)
  add("duplicate_feature_detected", any(duplicate_validation$check == "features_unique" & duplicate_validation$status == "fail"))
  missing_validation <- aq_validate_model_spec(aq_model_spec(target = "missing_target", features = "spend"), dt)
  add("missing_target_detected", any(missing_validation$status == "fail"))
  fit <- aq_fit_model(spec, dt)
  add("fit_result_class", inherits(fit, "aq_fit_result"))
  add("fit_artifact_class", inherits(fit$fit_artifact, "aq_supervised_fit_artifact"))
  add("fit_preserves_lineage", identical(fit$fit_artifact$model_id, spec$model_id) && identical(fit$fit_artifact$spec_id, spec$spec_id))
  pred_validation <- aq_predict_model(fit, dataset = "validation")
  pred_train <- aq_predict_model(fit, dataset = "training")
  pred_new <- aq_predict_model(fit, new_data = dt[1:10], dataset_id = "qa_new_data")
  add("validation_prediction_result_class", inherits(pred_validation, "aq_prediction_result"))
  add("training_prediction_result_class", inherits(pred_train, "aq_prediction_result"))
  add("new_prediction_result_class", inherits(pred_new, "aq_prediction_result"))
  add("prediction_contains_predict", "Predict" %in% names(pred_validation$data))
  add("prediction_artifact_is_table", inherits(pred_validation$artifact, "aq_table_artifact"))
  assessment <- aq_assess_model(fit, pred_validation, by = "channel")
  add("assessment_result_class", inherits(assessment, "aq_assessment_result"))
  add("assessment_artifact_class", inherits(assessment$assessment_artifact, "aq_regression_assessment_artifact"))
  add("assessment_metrics_present", all(c("rmse", "mae", "r2") %in% assessment$metrics$metric))
  add("subgroup_metrics_present", nrow(assessment$tables$subgroup_metrics) > 0L)
  add("comparison_ready", isTRUE(assessment$comparison_ready))
  serialized <- unserialize(serialize(list(spec = spec, fit_artifact = fit$fit_artifact, prediction_artifact = pred_validation$artifact, assessment_artifact = assessment$assessment_artifact), NULL))
  add("artifact_serialization_round_trip", inherits(serialized$fit_artifact, "aq_supervised_fit_artifact") && inherits(serialized$prediction_artifact, "aq_table_artifact"))
  capabilities <- aq_model_operator_capabilities()
  add("capability_metadata_registered", "catboost_regression_vnext" %in% capabilities$operator_id)
  app_like_artifacts <- c(list(fit$fit_artifact), list(pred_validation$artifact), assessment$artifacts, list(assessment$assessment_artifact))
  add("analytics_shinyapp_consumable_artifacts", all(vapply(app_like_artifacts, is.list, logical(1L))))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' QA for vNext CatBoost Binary Classification
#'
#' @return A `data.table` of deterministic QA checks.
#' @export
qa_vnext_catboost_binary <- function() {
  rows <- list()
  add <- function(check, passed, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_catboost_binary",
      check = check,
      status = if (isTRUE(passed)) "pass" else "fail",
      message = message
    )
  }
  dt <- aq_vnext_catboost_binary_fixture()
  policy <- aq_threshold_policy(threshold = 0.45, positive_class = "yes")
  spec <- aq_model_spec(
    task = "binary",
    engine = "catboost",
    target = "converted",
    features = c("channel", "region", "spend", "clicks", "discount"),
    partition = aq_partition_spec(method = "time", split_col = "event_date", train_fraction = 0.8, seed = 20260713),
    engine_params = list(iterations = 25L, depth = 4L, learning_rate = 0.08, verbose = FALSE),
    threshold_policy = policy,
    seed = 20260713,
    dataset_id = "qa_vnext_catboost_binary_fixture"
  )
  validation <- aq_validate_model_spec(spec, dt)
  add("binary_spec_constructed", inherits(spec, "aq_model_spec") && identical(spec$task, "binary"))
  add("threshold_policy_constructed", inherits(policy, "aq_threshold_policy"))
  add("binary_validation_passes", !aq_vnext_has_validation_error(validation), paste(validation$message, collapse = " | "))
  single_class_dt <- data.table::copy(dt)
  single_class_dt[, converted := factor("yes", levels = c("no", "yes"))]
  single_class_validation <- aq_validate_model_spec(spec, single_class_dt)
  add("single_class_detected", any(single_class_validation$check == "binary_target_levels" & single_class_validation$status == "fail"))
  missing_positive_spec <- aq_model_spec(
    task = "binary",
    target = "converted",
    features = "spend",
    positive_class = "missing",
    threshold_policy = aq_threshold_policy(positive_class = "missing"),
    seed = 20260713
  )
  missing_positive_validation <- aq_validate_model_spec(missing_positive_spec, dt)
  add("missing_positive_class_detected", any(missing_positive_validation$check == "positive_class_present" & missing_positive_validation$status == "fail"))
  bad_threshold_validation <- aq_validate_model_spec(aq_model_spec(
    task = "binary",
    target = "converted",
    features = "spend",
    threshold_policy = aq_threshold_policy(threshold = 1.2, positive_class = "yes"),
    seed = 20260713
  ), dt)
  add("bad_threshold_detected", any(bad_threshold_validation$check == "threshold_policy_threshold" & bad_threshold_validation$status == "fail"))
  fit <- aq_fit_model(spec, dt)
  add("binary_fit_result_class", inherits(fit, "aq_fit_result"))
  add("binary_fit_artifact_class", inherits(fit$fit_artifact, "aq_supervised_fit_artifact"))
  add("binary_fit_preserves_classes", identical(fit$positive_class, "yes") && identical(fit$negative_class, "no"))
  pred_validation <- aq_predict_model(fit, dataset = "validation")
  pred_override <- aq_predict_model(fit, dataset = "validation", threshold_policy = aq_threshold_policy(threshold = 0.65, positive_class = "yes", negative_class = "no"))
  add("binary_prediction_result_class", inherits(pred_validation, "aq_prediction_result"))
  add("probability_output_present", "PositiveProbability" %in% names(pred_validation$data))
  add("predicted_class_output_present", "Predict" %in% names(pred_validation$data))
  add("threshold_policy_preserved", inherits(pred_validation$threshold_policy, "aq_threshold_policy") && identical(pred_validation$threshold_policy$threshold, 0.45))
  add("threshold_override_applied", identical(pred_override$threshold_policy$threshold, 0.65))
  add("probabilities_in_unit_interval", all(pred_validation$data$PositiveProbability >= 0 & pred_validation$data$PositiveProbability <= 1))
  add("prediction_artifact_is_table", inherits(pred_validation$artifact, "aq_table_artifact"))
  assessment <- aq_assess_model(fit, pred_validation, by = "channel")
  add("binary_assessment_result_class", inherits(assessment, "aq_assessment_result"))
  add("binary_assessment_artifact_class", inherits(assessment$assessment_artifact, "aq_binary_assessment_artifact"))
  add("binary_metrics_present", all(c("logloss", "auc", "accuracy", "precision", "recall", "f1", "specificity", "sensitivity") %in% assessment$metrics$metric))
  add("confusion_matrix_present", nrow(assessment$tables$confusion_matrix) == 2L)
  add("calibration_summary_present", nrow(assessment$tables$calibration) > 0L)
  add("probability_diagnostics_present", nrow(assessment$tables$probability_diagnostics) == 1L)
  add("binary_subgroup_metrics_present", nrow(assessment$tables$subgroup_metrics) > 0L)
  add("binary_comparison_ready", isTRUE(assessment$comparison_ready))
  serialized <- unserialize(serialize(list(spec = spec, fit_artifact = fit$fit_artifact, prediction_artifact = pred_validation$artifact, assessment_artifact = assessment$assessment_artifact), NULL))
  add("binary_artifact_serialization_round_trip", inherits(serialized$fit_artifact, "aq_supervised_fit_artifact") && inherits(serialized$assessment_artifact, "aq_binary_assessment_artifact"))
  capabilities <- aq_model_operator_capabilities()
  add("binary_capability_metadata_registered", "catboost_binary_vnext" %in% capabilities$operator_id)
  app_like_artifacts <- c(list(fit$fit_artifact), list(pred_validation$artifact), assessment$artifacts, list(assessment$assessment_artifact))
  add("binary_analytics_shinyapp_consumable_artifacts", all(vapply(app_like_artifacts, is.list, logical(1L))))
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' QA for vNext Rodeo Transformation Replay
#'
#' @return A `data.table` of deterministic QA checks.
#' @export
qa_vnext_rodeo_transformation_replay <- function() {
  rows <- list()
  add <- function(check, passed, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_rodeo_transformation_replay",
      check = check,
      status = if (isTRUE(passed)) "pass" else "fail",
      message = message
    )
  }
  if (!requireNamespace("Rodeo", quietly = TRUE)) {
    add("rodeo_available", FALSE, "Rodeo is required for transformation replay QA.")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }

  regression_dt <- aq_vnext_catboost_fixture()
  date_spec <- Rodeo::rodeo_transformation_spec(
    "date_features",
    input_columns = "event_date",
    parameters = list(features = c("year", "month"))
  )
  model_spec <- aq_model_spec(
    task = "regression",
    engine = "catboost",
    target = "revenue",
    features = c("channel", "region", "spend", "clicks", "discount", "event_date_year", "event_date_month"),
    partition = aq_partition_spec(method = "time", split_col = "event_date", train_fraction = 0.8, seed = 20260716),
    engine_params = list(iterations = 18L, depth = 4L, learning_rate = 0.08, verbose = FALSE),
    seed = 20260716,
    dataset_id = "qa_vnext_rodeo_raw_training",
    transformation_spec = date_spec
  )
  raw_validation <- aq_validate_model_spec(model_spec, regression_dt)
  add("raw_spec_validation_defers_generated_features", !aq_vnext_has_validation_error(raw_validation) &&
    any(raw_validation$check == "feature_validation_deferred"))
  fit <- aq_fit_model(model_spec, regression_dt)
  add("fit_contains_fitted_rodeo_transformation", inherits(fit$fitted_transformation, "rodeo_fitted_transformation"))
  add("fit_serializes_fitted_transformation", inherits(unserialize(fit$serialized_fitted_transformation), "rodeo_fitted_transformation"))
  add("prepared_features_created", all(c("event_date_year", "event_date_month") %in% names(fit$training_data)))
  add("fit_artifact_records_replay_requirement", isTRUE(fit$fit_artifact$transformation_required) &&
    isTRUE(fit$fit_artifact$replay_behavior$required))
  add("prepared_lineage_present", !is.null(fit$transformation_lineage$prepared_training_dataset_id) &&
    !is.null(fit$transformation_lineage$prepared_validation_dataset_id))

  raw_score <- regression_dt[1:25, .(id, event_date, channel, region, spend, clicks, discount)]
  raw_score_copy <- data.table::copy(raw_score)
  score <- aq_score_model(fit, raw_score, row_id_cols = "id", dataset_id = "qa_vnext_rodeo_raw_scoring")
  add("scoring_replays_transformation", identical(score$transformation_replay$status, "success"))
  add("scoring_uses_prepared_dataset_identity", !is.null(score$prepared_dataset_id) && nzchar(score$prepared_dataset_id))
  add("scoring_preserves_row_identity", identical(score$row_id_cols, "id") && identical(sort(score$data$id), sort(raw_score$id)))
  add("raw_scoring_data_not_mutated", identical(raw_score, raw_score_copy))
  add("score_artifact_records_transformation", isTRUE(score$artifact$metadata$transformation_required) &&
    identical(score$artifact$metadata$transformation_replay_status, "success"))
  missing_input_rejected <- tryCatch({
    aq_score_model(fit, raw_score[, .SD, .SDcols = setdiff(names(raw_score), "event_date")], row_id_cols = "id")
    FALSE
  }, error = function(e) grepl("Rodeo transformation replay failed", conditionMessage(e), fixed = TRUE))
  add("missing_transformation_input_rejected", missing_input_rejected)
  attached <- aq_attach_outcomes(score, regression_dt[1:25, .(id, realized = revenue)], outcome_col = "realized")
  assessment <- aq_assess_scoring(attached)
  monitor <- aq_monitor_scoring(attached)
  add("assessment_after_replay_scoring", inherits(assessment, "aq_assessment_result") && nrow(assessment$metrics) > 0L)
  add("monitoring_reports_transformation_compatibility", inherits(monitor, "aq_scoring_monitoring_result") &&
    "transformation_compatibility" %in% names(monitor$evidence))

  binary_dt <- aq_vnext_catboost_binary_fixture()
  factor_spec <- Rodeo::rodeo_transformation_spec(
    "factor_levels",
    input_columns = "channel",
    parameters = list(unseen_level = "__UNSEEN__", include_missing_level = TRUE)
  )
  binary_spec <- aq_model_spec(
    task = "binary",
    engine = "catboost",
    target = "converted",
    features = c("channel", "region", "spend", "clicks", "discount"),
    partition = aq_partition_spec(method = "time", split_col = "event_date", train_fraction = 0.8, seed = 20260717),
    threshold_policy = aq_threshold_policy(threshold = 0.45, positive_class = "yes"),
    engine_params = list(iterations = 18L, depth = 4L, learning_rate = 0.08, verbose = FALSE),
    seed = 20260717,
    dataset_id = "qa_vnext_rodeo_binary_raw_training",
    transformation_spec = factor_spec
  )
  binary_fit <- aq_fit_model(binary_spec, binary_dt)
  binary_score_data <- binary_dt[1:30, .(id, channel, region, spend, clicks, discount)]
  binary_score_data[1L, channel := "Affiliate"]
  binary_score <- aq_score_model(binary_fit, binary_score_data, row_id_cols = "id")
  add("binary_replay_handles_unseen_category", inherits(binary_score, "aq_scoring_result") &&
    any(grepl("__UNSEEN__", binary_score$warnings, fixed = TRUE)))
  add("analytics_shinyapp_consumable_replay_artifacts", all(vapply(list(fit$fit_artifact, score$artifact, monitor$monitoring_artifact, binary_score$artifact), is.list, logical(1L))))

  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' QA for vNext Portable Model Bundles
#'
#' @return A `data.table` of deterministic QA checks.
#' @export
qa_vnext_model_bundle <- function() {
  rows <- list()
  add <- function(check, passed, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_model_bundle",
      check = check,
      status = if (isTRUE(passed)) "pass" else "fail",
      message = message
    )
  }
  if (!requireNamespace("Rodeo", quietly = TRUE)) {
    add("rodeo_available", FALSE, "Rodeo is required for model bundle QA.")
    return(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
  }

  dt <- aq_vnext_catboost_fixture()
  date_spec <- Rodeo::rodeo_transformation_spec(
    "date_features",
    input_columns = "event_date",
    parameters = list(features = c("year", "month"))
  )
  spec <- aq_model_spec(
    task = "regression",
    engine = "catboost",
    target = "revenue",
    features = c("channel", "region", "spend", "clicks", "discount", "event_date_year", "event_date_month"),
    partition = aq_partition_spec(method = "time", split_col = "event_date", train_fraction = 0.8, seed = 20260718),
    engine_params = list(iterations = 18L, depth = 4L, learning_rate = 0.08, verbose = FALSE),
    seed = 20260718,
    dataset_id = "qa_vnext_bundle_raw_training",
    transformation_spec = date_spec
  )
  fit <- aq_fit_model(spec, dt)
  raw_score <- dt[1:25, .(id, event_date, channel, region, spend, clicks, discount)]
  original_score <- aq_score_model(fit, raw_score, row_id_cols = "id", dataset_id = "qa_vnext_bundle_scoring")

  bundle_dir <- file.path(tempdir(), paste0("aq_bundle_", Sys.getpid(), "_", sample.int(999999L, 1L)))
  manifest <- aq_save_model_bundle(fit, bundle_dir)
  add("bundle_save_manifest", inherits(manifest, "aq_model_bundle_manifest") &&
    file.exists(manifest$bundle_path) && file.exists(manifest$metadata_path))
  metadata <- aq_load_model_bundle(bundle_dir, metadata_only = TRUE)
  add("metadata_only_load", inherits(metadata, "aq_model_bundle_metadata") &&
    identical(metadata$model_id, fit$model_id) && isTRUE(metadata$transformation_required))
  bundle <- aq_load_model_bundle(bundle_dir)
  add("bundle_load_class", inherits(bundle, "aq_model_bundle") && inherits(bundle, "aq_fit_result"))
  validation <- aq_validate_model_bundle(bundle)
  add("bundle_validation_passes", !aq_vnext_has_validation_error(validation), paste(validation$message, collapse = " | "))
  add("bundle_drops_training_rows", is.null(bundle$training_data) && is.null(bundle$validation_data) &&
    is.null(bundle$raw_training_data) && is.null(bundle$raw_validation_data))
  add("bundle_preserves_identity", identical(bundle$bundle_id, metadata$bundle_id) &&
    identical(bundle$model_id, fit$model_id) && identical(bundle$fit_id, fit$fit_id))
  add("bundle_preserves_lineage", identical(bundle$transformation_lineage$fitted_transformation_id, fit$transformation_lineage$fitted_transformation_id) &&
    identical(bundle$prepared_training_schema_fingerprint, fit$prepared_training_schema_fingerprint))
  reloaded_score <- aq_score_model(bundle, raw_score, row_id_cols = "id", dataset_id = "qa_vnext_bundle_scoring")
  add("reloaded_scoring_identical", isTRUE(all.equal(original_score$data$Predict, reloaded_score$data$Predict, tolerance = 1e-12)) &&
    identical(reloaded_score$transformation_replay$status, "success"))
  attached <- aq_attach_outcomes(reloaded_score, dt[1:25, .(id, realized = revenue)], outcome_col = "realized")
  assessment <- aq_assess_scoring(attached)
  add("reloaded_assessment_compatible", inherits(assessment, "aq_assessment_result") && nrow(assessment$metrics) > 0L)

  corrupt_path <- file.path(tempdir(), paste0("corrupt_bundle_", Sys.getpid(), ".rds"))
  saveRDS(list(not = "a bundle"), corrupt_path, version = 3)
  corrupt_detected <- tryCatch({
    aq_load_model_bundle(corrupt_path)
    FALSE
  }, error = function(e) grepl("not an aq_model_bundle", conditionMessage(e), fixed = TRUE))
  add("corrupt_bundle_rejected", corrupt_detected)

  missing_field_bundle <- bundle
  missing_field_bundle$model <- NULL
  missing_validation <- aq_validate_model_bundle(missing_field_bundle)
  add("missing_model_detected", any(missing_validation$check == "required_fields" & missing_validation$status == "fail") ||
    any(missing_validation$check == "model_available" & missing_validation$status == "fail"))

  version_bundle <- bundle
  version_bundle$bundle_version <- "aq_model_bundle_future"
  version_validation <- aq_validate_model_bundle(version_bundle)
  add("version_mismatch_detected", any(version_validation$check == "bundle_version" & version_validation$status == "fail"))
  add("analytics_shinyapp_consumable_bundle_metadata", is.list(metadata) &&
    all(c("bundle_id", "model_id", "task", "engine", "features", "supported_actions") %in% names(metadata)))

  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' QA for vNext Canonical Analytical Artifact Framework
#'
#' @return A `data.table` of deterministic QA checks.
#' @export
qa_vnext_artifact_framework <- function() {
  rows <- list()
  add <- function(check, passed, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_artifact_framework",
      check = check,
      status = if (isTRUE(passed)) "pass" else "fail",
      message = message
    )
  }

  has_validation_error <- function(x) any(x$status %in% c("fail", "error"))

  regression_dt <- aq_vnext_catboost_fixture()
  regression_spec <- aq_model_spec(
    task = "regression",
    engine = "catboost",
    target = "revenue",
    features = c("channel", "region", "spend", "clicks", "discount"),
    partition = aq_partition_spec(method = "time", split_col = "event_date", train_fraction = 0.8, seed = 20260712),
    engine_params = list(iterations = 20L, depth = 4L, learning_rate = 0.08, verbose = FALSE),
    metrics = c("rmse", "mae", "r2"),
    seed = 20260712,
    dataset_id = "qa_vnext_artifact_regression"
  )
  regression_fit <- aq_fit_model(regression_spec, regression_dt)
  regression_prediction <- aq_predict_model(regression_fit, dataset = "validation")
  regression_assessment <- aq_assess_model(regression_fit, regression_prediction, by = "channel")
  regression_score <- aq_score_model(regression_fit, regression_dt[1:30], row_id_cols = "id", outcome_col = "revenue", dataset_id = "qa_vnext_artifact_score")
  regression_monitor <- aq_monitor_scoring(regression_score)
  bundle <- aq_vnext_create_model_bundle(regression_fit)

  binary_dt <- aq_vnext_catboost_binary_fixture()
  binary_spec <- aq_model_spec(
    task = "binary",
    engine = "catboost",
    target = "converted",
    features = c("channel", "region", "spend", "clicks", "discount"),
    partition = aq_partition_spec(method = "time", split_col = "event_date", train_fraction = 0.8, seed = 20260713),
    engine_params = list(iterations = 20L, depth = 4L, learning_rate = 0.08, verbose = FALSE),
    threshold_policy = aq_threshold_policy(threshold = 0.45, positive_class = "yes"),
    seed = 20260713,
    dataset_id = "qa_vnext_artifact_binary"
  )
  binary_fit <- aq_fit_model(binary_spec, binary_dt)
  binary_prediction <- aq_predict_model(binary_fit, dataset = "validation")
  binary_assessment <- aq_assess_model(binary_fit, binary_prediction, by = "channel")

  artifacts <- list(
    regression_fit = regression_fit$fit_artifact,
    regression_prediction = regression_prediction$artifact,
    regression_assessment = regression_assessment$assessment_artifact,
    regression_scoring = regression_score$artifact,
    regression_monitoring = regression_monitor$monitoring_artifact,
    bundle = bundle,
    binary_fit = binary_fit$fit_artifact,
    binary_prediction = binary_prediction$artifact,
    binary_assessment = binary_assessment$assessment_artifact
  )

  validations <- lapply(artifacts, aq_validate_artifact)
  add("all_artifacts_validate", all(!vapply(validations, has_validation_error, logical(1L))),
    paste(names(artifacts)[vapply(validations, has_validation_error, logical(1L))], collapse = ", "))

  envelopes <- lapply(artifacts, aq_artifact_envelope)
  add("canonical_envelope_class", all(vapply(envelopes, inherits, logical(1L), what = "aq_artifact_envelope")))
  add("canonical_identity_present", all(vapply(envelopes, function(x) nzchar(x$artifact_id) && nzchar(x$artifact_type), logical(1L))))
  add("task_and_engine_normalized", identical(envelopes$regression_fit$task, "regression") &&
    identical(envelopes$binary_fit$task, "binary") &&
    identical(envelopes$regression_fit$engine, "catboost"))
  add("fit_to_prediction_relationship", regression_fit$fit_id %in% aq_artifact_relationships(regression_prediction$artifact)$parent_artifact_id)
  add("prediction_to_assessment_relationship", regression_prediction$prediction_id %in% aq_artifact_relationships(regression_assessment$assessment_artifact)$parent_artifact_id)
  add("scoring_to_monitoring_relationship", regression_score$scoring_id %in% aq_artifact_relationships(regression_monitor$monitoring_artifact)$parent_artifact_id)
  add("bundle_relationship_to_fit", regression_fit$fit_id %in% aq_artifact_relationships(bundle)$parent_artifact_id)
  add("supported_actions_exposed", all(c("score", "attach_outcomes", "assess_realized", "monitor") %in% aq_supported_actions(bundle)) &&
    "compare" %in% aq_supported_actions(regression_assessment$assessment_artifact))
  add("analytics_shinyapp_compatibility", all(vapply(envelopes, function(x) {
    is.list(x) && all(c("artifact_id", "artifact_type", "supported_actions", "lineage", "consumer_expectations") %in% names(x))
  }, logical(1L))))
  add("campaign_compatibility", all(vapply(envelopes, function(x) {
    is.list(x$campaign_references) && is.list(x$lineage) &&
      (x$operator %in% c("supervised_fit", "prediction", "scoring", "model_bundle") ||
        "campaign_review" %in% x$supported_actions)
  }, logical(1L))))
  add("future_operator_compatibility", all(vapply(envelopes, function(x) {
    identical(x$envelope_version, "aq_artifact_envelope_v1") && is.list(x$consumer_expectations)
  }, logical(1L))))
  serialized <- unserialize(serialize(artifacts, NULL))
  add("artifact_serialization_round_trip", all(vapply(serialized, function(x) !has_validation_error(aq_validate_artifact(x)), logical(1L))))

  broken <- regression_fit$fit_artifact
  broken$artifact_envelope$artifact_id <- NA_character_
  broken_validation <- aq_validate_artifact(broken)
  add("missing_identity_detected", any(broken_validation$check == "required_metadata" & broken_validation$status == "fail"))

  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

#' QA for vNext Canonical Scoring Lifecycle
#'
#' @return A `data.table` of deterministic QA checks.
#' @export
qa_vnext_scoring_lifecycle <- function() {
  rows <- list()
  add <- function(check, passed, message = "") {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      suite = "vnext_scoring_lifecycle",
      check = check,
      status = if (isTRUE(passed)) "pass" else "fail",
      message = message
    )
  }

  regression_dt <- aq_vnext_catboost_fixture()
  regression_spec <- aq_model_spec(
    task = "regression",
    engine = "catboost",
    target = "revenue",
    features = c("channel", "region", "spend", "clicks", "discount"),
    partition = aq_partition_spec(method = "time", split_col = "event_date", train_fraction = 0.8, seed = 20260714),
    engine_params = list(iterations = 20L, depth = 4L, learning_rate = 0.08, verbose = FALSE),
    seed = 20260714,
    dataset_id = "qa_vnext_scoring_regression_fit"
  )
  regression_fit <- aq_fit_model(regression_spec, regression_dt)
  scoring_data <- regression_dt[1:30, .(id, channel, region, spend, clicks, discount)]
  scoring_copy <- data.table::copy(scoring_data)
  regression_score <- aq_score_model(
    regression_fit,
    scoring_data,
    row_id_cols = "id",
    dataset_id = "qa_vnext_scoring_population"
  )
  add("regression_scoring_result_class", inherits(regression_score, "aq_scoring_result"))
  add("regression_prediction_values_present", "Predict" %in% names(regression_score$data) && all(is.finite(regression_score$data$Predict)))
  add("regression_row_identity_preserved", identical(regression_score$row_id_cols, "id") && identical(sort(regression_score$data$id), sort(scoring_data$id)))
  add("regression_no_source_data_mutation", identical(scoring_data, scoring_copy))
  add("regression_artifact_lineage", identical(regression_score$model_id, regression_fit$model_id) && identical(regression_score$fit_id, regression_fit$fit_id))
  missing_feature_error <- tryCatch({
    aq_score_model(regression_fit, scoring_data[, .SD, .SDcols = setdiff(names(scoring_data), "spend")], row_id_cols = "id")
    FALSE
  }, error = function(e) grepl("Missing scoring feature", conditionMessage(e), fixed = TRUE))
  add("missing_feature_rejected", missing_feature_error)
  duplicate_id_error <- tryCatch({
    bad <- data.table::copy(scoring_data)
    bad[2L, id := bad[1L, id]]
    aq_score_model(regression_fit, bad, row_id_cols = "id")
    FALSE
  }, error = function(e) grepl("row identity", conditionMessage(e), ignore.case = TRUE))
  add("duplicate_row_identity_rejected", duplicate_id_error)
  type_mismatch_error <- tryCatch({
    bad <- data.table::copy(scoring_data)
    bad[, spend := as.character(spend)]
    aq_score_model(regression_fit, bad, row_id_cols = "id")
    FALSE
  }, error = function(e) grepl("incompatible type", conditionMessage(e), fixed = TRUE))
  add("type_mismatch_rejected", type_mismatch_error)
  strict_extra_error <- tryCatch({
    spec <- aq_scoring_spec(regression_fit, row_id_cols = "id", extra_column_behavior = "fail")
    aq_score_model(regression_fit, regression_dt[1:10], scoring_spec = spec)
    FALSE
  }, error = function(e) grepl("Extra scoring column", conditionMessage(e), fixed = TRUE))
  add("extra_column_policy_rejected", strict_extra_error)
  regression_attached <- aq_attach_outcomes(
    regression_score,
    regression_dt[1:30, .(id, realized_revenue = revenue)],
    outcome_col = "realized_revenue"
  )
  add("regression_outcome_attachment_class", inherits(regression_attached$outcome_attachment, "aq_outcome_attachment_artifact"))
  add("regression_outcomes_attached_without_rescoring", identical(regression_score$data$Predict, regression_attached$data$Predict))
  regression_assessment <- aq_assess_scoring(regression_attached)
  add("regression_realized_assessment", inherits(regression_assessment, "aq_assessment_result") && regression_assessment$evaluated_row_count == nrow(regression_attached$data))
  regression_monitor <- aq_monitor_scoring(regression_attached)
  add("regression_monitoring_evidence", inherits(regression_monitor, "aq_scoring_monitoring_result") && nrow(regression_monitor$evidence$prediction_distribution) == 1L)
  regression_serialized <- unserialize(serialize(regression_attached, NULL))
  regression_reassessment <- aq_assess_scoring(regression_serialized)
  add("regression_serialization_replay", identical(regression_assessment$metrics$metric, regression_reassessment$metrics$metric))

  binary_dt <- aq_vnext_catboost_binary_fixture()
  policy <- aq_threshold_policy(threshold = 0.45, positive_class = "yes")
  binary_spec <- aq_model_spec(
    task = "binary",
    engine = "catboost",
    target = "converted",
    features = c("channel", "region", "spend", "clicks", "discount"),
    partition = aq_partition_spec(method = "time", split_col = "event_date", train_fraction = 0.8, seed = 20260715),
    threshold_policy = policy,
    engine_params = list(iterations = 20L, depth = 4L, learning_rate = 0.08, verbose = FALSE),
    seed = 20260715,
    dataset_id = "qa_vnext_scoring_binary_fit"
  )
  binary_fit <- aq_fit_model(binary_spec, binary_dt)
  binary_score <- aq_score_model(
    binary_fit,
    binary_dt[1:40, .(id, channel, region, spend, clicks, discount)],
    row_id_cols = "id",
    dataset_id = "qa_vnext_binary_scoring_population"
  )
  add("binary_scoring_result_class", inherits(binary_score, "aq_scoring_result"))
  add("binary_probability_output", all(c("PositiveProbability", "Predict") %in% names(binary_score$data)))
  add("binary_threshold_policy_preserved", inherits(binary_score$threshold_policy, "aq_threshold_policy") && identical(binary_score$threshold_policy$threshold, 0.45))
  rethresholded <- aq_apply_threshold_policy(binary_score, aq_threshold_policy(threshold = 0.7, positive_class = "yes", negative_class = "no"))
  add("binary_threshold_reapplied_without_rescoring", identical(binary_score$data$PositiveProbability, rethresholded$data$PositiveProbability) && identical(rethresholded$threshold_policy$threshold, 0.7))
  add("binary_threshold_decision_history", nrow(rethresholded$threshold_decision_history) >= 2L)
  binary_attached <- aq_attach_outcomes(
    rethresholded,
    binary_dt[1:40, .(id, outcome = converted)],
    outcome_col = "outcome"
  )
  add("binary_outcome_attachment", identical(binary_attached$outcome_status, "attached"))
  binary_assessment <- aq_assess_scoring(binary_attached, by = NULL)
  add("binary_realized_assessment", inherits(binary_assessment, "aq_assessment_result") && all(c("auc", "accuracy", "threshold") %in% binary_assessment$metrics$metric))
  add("binary_confusion_and_calibration", nrow(binary_assessment$tables$confusion_matrix) == 2L && nrow(binary_assessment$tables$calibration) > 0L)
  binary_monitor <- aq_monitor_scoring(binary_attached, baseline = binary_score)
  add("binary_monitoring_with_compatible_baseline", inherits(binary_monitor, "aq_scoring_monitoring_result") && identical(binary_monitor$baseline_id, binary_score$scoring_id))
  incompatible_baseline_error <- tryCatch({
    aq_monitor_scoring(binary_attached, baseline = regression_score)
    FALSE
  }, error = function(e) grepl("same task and model_id", conditionMessage(e), fixed = TRUE))
  add("incompatible_baseline_rejected", incompatible_baseline_error)
  app_like_artifacts <- list(
    regression_score$artifact,
    regression_attached$outcome_attachment,
    regression_monitor$monitoring_artifact,
    binary_score$artifact,
    binary_attached$outcome_attachment,
    binary_monitor$monitoring_artifact
  )
  add("analytics_shinyapp_consumable_scoring_artifacts", all(vapply(app_like_artifacts, is.list, logical(1L))))
  capabilities <- aq_model_operator_capabilities()
  add("capability_metadata_scoring_registered", all(c("scoring", "scoring_assessment", "monitoring", "delayed_outcomes") %in% names(capabilities)) &&
    all(capabilities$scoring == "aq_score_model") &&
    all(capabilities$monitoring == "aq_monitor_scoring") &&
    all(capabilities$delayed_outcomes))

  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}
