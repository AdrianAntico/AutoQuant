# Hostile reconstruction 2.0 checks. Not exported.
pkgload::load_all("C:/Users/Bizon/Documents/GitHub/AutoQuant-backend-recovery",
  export_all = FALSE, quiet = TRUE)

spec <- AutoQuant:::aq_forecast_spec(
  "value", "date", engine = "catboost",
  engine_parameters = list(
    has_time = TRUE, boosting_type = "Ordered",
    monotone_constraints = c(1L, 0L, -1L), langevin = TRUE, iterations = 50L
  )
)
val <- AutoQuant:::aq_validate_forecast_spec(spec)
fail_n <- sum(val$status %in% c("fail", "error"))
cat("VALIDATOR_FAIL_ROWS", fail_n, "\n")
print(val[grepl("param|gbdt|engine", check)])

lgb <- AutoQuant:::aq_vnext_lightgbm_params(list(
  num_iterations = 400L, num_leaves = 63L, linear_tree = TRUE
))
cat("LGB", lgb$num_iterations, lgb$linear_tree, "\n")

xgb <- AutoQuant:::aq_vnext_xgboost_params(list(
  nrounds = 400L, max_depth = 8L, tree_method = "hist"
))
cat("XGB", xgb$nrounds, xgb$tree_method, "\n")

bad <- try(AutoQuant:::aq_vnext_engine_params(list(has_time = TRUE, not_real = 1)),
  silent = TRUE)
cat("UNKNOWN_FAILS", inherits(bad, "try-error"), "\n")
cat("ENGINES", paste(AutoQuant:::aq_forecast_engine_levels(), collapse = ","), "\n")
cat("FUNNEL", paste(names(formals(AutoQuant:::aq_funnel_forecast_spec)), collapse = ","), "\n")
cat("HAS_TIME_DEFAULT", AutoQuant:::aq_vnext_engine_params()$has_time, "\n")
cat("EXPORTS", length(getNamespaceExports("AutoQuant")), "\n")
cat("HAS_FORECAST_ENGINE_CONTROL",
  "forecast_engine_control" %in% getNamespaceExports("AutoQuant"), "\n")
cat("NO_BARPLOT", !"BarPlot" %in% getNamespaceExports("AutoQuant"), "\n")

defaults <- AutoQuant:::aq_forecast_catboost_model_params(list(engine_parameters = list()))
cat("CATBOOST_MODEL_HAS_TIME", defaults$has_time, "\n")
cat("CATBOOST_ITERS", defaults$iterations, "DEPTH", defaults$depth, "\n")

stopifnot(identical(fail_n, 0L) || identical(fail_n, 0))
stopifnot(isTRUE(lgb$linear_tree), identical(as.integer(lgb$num_iterations), 400L))
stopifnot(identical(xgb$tree_method, "hist"))
stopifnot(inherits(bad, "try-error"))
stopifnot(isTRUE(AutoQuant:::aq_vnext_engine_params()$has_time))
stopifnot(isTRUE(defaults$has_time))
cat("HOSTILE_QA_PASS\n")
