# AutoQuant vNext supervised-learning quick start

library(data.table)
library(AutoQuant)

set.seed(20260712)

dt <- data.table(
  id = seq_len(80),
  spend = runif(80, 10, 100),
  clicks = rpois(80, 40),
  channel = sample(c("Search", "Social", "Email"), 80, TRUE)
)
dt[, revenue := 25 + 1.8 * spend + 0.7 * clicks + fifelse(channel == "Search", 12, 0) + rnorm(.N, 0, 5)]
dt[, converted := factor(ifelse(revenue > median(revenue), "yes", "no"), levels = c("no", "yes"))]

regression_spec <- aq_model_spec(
  task = "regression",
  target = "revenue",
  features = c("spend", "clicks", "channel"),
  engine_params = list(iterations = 5, depth = 2, learning_rate = 0.15),
  dataset_id = "vnext_supervised_example"
)

regression_fit <- aq_fit_model(regression_spec, dt)
regression_score <- aq_score_model(
  regression_fit,
  dt[1:10],
  row_id_cols = "id",
  outcome_col = "revenue",
  dataset_id = "vnext_supervised_score_example"
)

binary_spec <- aq_model_spec(
  task = "binary",
  target = "converted",
  features = c("spend", "clicks", "channel"),
  positive_class = "yes",
  engine_params = list(iterations = 5, depth = 2, learning_rate = 0.15),
  dataset_id = "vnext_binary_example"
)

binary_fit <- aq_fit_model(binary_spec, dt)
binary_score <- aq_score_model(
  binary_fit,
  dt[1:10],
  row_id_cols = "id",
  outcome_col = "converted",
  dataset_id = "vnext_binary_score_example"
)

bundle_dir <- file.path(tempdir(), "autoquant_vnext_bundle_example")
saved_bundle <- aq_save_model_bundle(regression_fit, bundle_dir, overwrite = TRUE)
reloaded_bundle <- aq_load_model_bundle(bundle_dir)
reloaded_score <- aq_score_model(
  reloaded_bundle,
  dt[1:5],
  row_id_cols = "id",
  outcome_col = "revenue",
  dataset_id = "vnext_reloaded_bundle_score_example"
)

stopifnot(
  inherits(regression_fit, "aq_fit_result"),
  inherits(regression_score, "aq_prediction_result"),
  inherits(binary_fit, "aq_fit_result"),
  inherits(binary_score, "aq_prediction_result"),
  inherits(saved_bundle, "aq_model_bundle_manifest"),
  inherits(reloaded_bundle, "aq_model_bundle"),
  inherits(reloaded_score, "aq_prediction_result")
)

list(
  regression_fit = regression_fit,
  regression_score = regression_score,
  binary_fit = binary_fit,
  binary_score = binary_score,
  saved_bundle = saved_bundle,
  reloaded_score = reloaded_score
)
