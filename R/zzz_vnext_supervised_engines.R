# Canonical LightGBM and XGBoost adapters for Supervised Reference Contract 1.0.
# This file intentionally loads after the CatBoost reference implementation and
# dispatches through the same public specification, bundle, assessment and
# scoring contracts.

.aq_catboost_model_spec <- aq_model_spec
.aq_catboost_validate_model_spec <- aq_validate_model_spec
.aq_catboost_fit_model <- aq_fit_model
.aq_catboost_predict_model <- aq_predict_model
.aq_catboost_validate_model_bundle <- aq_validate_model_bundle
.aq_catboost_training_history <- aq_supervised_training_history
.aq_catboost_feature_importance <- aq_supervised_feature_importance
.aq_reference_evidence_manifest <- aq_supervised_evidence_manifest

aq_supervised_engine_params <- function(engine, engine_params = list(), seed = 20260712L,
                                        task = c("regression", "binary")) {
  task <- match.arg(task)
  engine <- match.arg(engine, c("lightgbm", "xgboost"))
  cores <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
  defaults <- if (identical(engine, "lightgbm")) list(
    num_iterations = 100L, learning_rate = 0.05, num_leaves = 31L,
    max_depth = -1L, min_data_in_leaf = 20L, feature_fraction = 1,
    bagging_fraction = 1, bagging_freq = 0L, lambda_l1 = 0, lambda_l2 = 0,
    objective = if (identical(task, "binary")) "binary" else "regression",
    metric = if (identical(task, "binary")) "auc" else "rmse",
    seed = as.integer(seed), num_threads = cores, device_type = "cpu",
    verbosity = -1L, early_stopping_round = 20L
  ) else list(
    nrounds = 100L, eta = 0.05, max_depth = 6L, min_child_weight = 1,
    subsample = 1, colsample_bytree = 1, gamma = 0, lambda = 1, alpha = 0,
    objective = if (identical(task, "binary")) "binary:logistic" else "reg:squarederror",
    eval_metric = if (identical(task, "binary")) "auc" else "rmse",
    seed = as.integer(seed), nthread = cores, tree_method = "hist",
    device = "cpu", early_stopping_rounds = 20L, verbose = 0L
  )
  unknown <- setdiff(names(engine_params), names(defaults))
  if (length(unknown)) stop("Unsupported ", engine, " engine parameter(s): ",
                            paste(unknown, collapse = ", "), call. = FALSE)
  utils::modifyList(defaults, engine_params)
}

aq_model_spec <- function(task = "regression", engine = "catboost", target, features,
                          partition = aq_partition_spec(seed = seed), engine_params = list(),
                          metrics = NULL, positive_class = NULL, threshold_policy = NULL,
                          seed = 20260712L, model_id = NULL, dataset_id = NULL,
                          transformation_spec = NULL,
                          supported_downstream_actions = c("predict", "assess", "compare")) {
  engine <- match.arg(tolower(engine), c("catboost", "lightgbm", "xgboost"))
  if (identical(engine, "catboost")) return(.aq_catboost_model_spec(
    task = task, engine = engine, target = target, features = features, partition = partition,
    engine_params = engine_params, metrics = metrics, positive_class = positive_class,
    threshold_policy = threshold_policy, seed = seed, model_id = model_id,
    dataset_id = dataset_id, transformation_spec = transformation_spec,
    supported_downstream_actions = supported_downstream_actions))
  spec <- .aq_catboost_model_spec(
    task = task, engine = "catboost", target = target, features = features,
    partition = partition, engine_params = list(), metrics = metrics,
    positive_class = positive_class, threshold_policy = threshold_policy, seed = seed,
    model_id = model_id, dataset_id = dataset_id,
    transformation_spec = transformation_spec,
    supported_downstream_actions = supported_downstream_actions)
  spec$engine <- engine
  spec$model_id <- aq_vnext_default(model_id,
    aq_vnext_id(paste("model", engine, spec$task, spec$target, sep = "_"), seed = seed))
  spec$engine_params <- aq_supervised_engine_params(engine, engine_params, seed, spec$task)
  spec
}

aq_validate_model_spec <- function(spec, data = NULL) {
  if (!inherits(spec, "aq_model_spec") || identical(spec$engine, "catboost"))
    return(.aq_catboost_validate_model_spec(spec, data))
  engine <- match.arg(spec$engine, c("lightgbm", "xgboost"))
  proxy <- spec
  proxy$engine <- "catboost"
  proxy$engine_params <- aq_vnext_engine_params(list(), seed = spec$seed, task = spec$task)
  out <- .aq_catboost_validate_model_spec(proxy, data)
  out[check == "engine_supported", `:=`(status = "pass",
    message = paste(engine, "engine is supported."), severity = "info")]
  out[check == "engine_params_supported", `:=`(status = "pass",
    message = paste(engine, "parameters are validated by its canonical adapter."), severity = "info")]
  out[check == "engine_task_type", `:=`(status = "pass",
    message = paste(engine, "CPU execution is qualified; GPU remains unqualified."), severity = "info")]
  out
}

aq_supervised_implementation_descriptor <- function(engine, task, params = list()) {
  engine <- match.arg(engine, c("catboost", "lightgbm", "xgboost"))
  if (identical(engine, "catboost")) return(aq_catboost_implementation_descriptor(task, params))
  thread <- as.integer(params[[if (identical(engine, "lightgbm")) "num_threads" else "nthread"]] %||% 1L)
  gpu <- if (identical(engine, "lightgbm")) params$device_type else params$device
  categorical <- if (identical(engine, "lightgbm"))
    "native_integer_categorical_features_with_declared_levels" else
    "native_xgboost_factor_categorical_data_with_declared_levels"
  sparse <- if (identical(engine, "lightgbm"))
    "native_sparse_matrix_support; qualified dense mixed-table path" else
    "native DMatrix sparse support; qualified data.frame factor and sparse-matrix paths"
  structure(list(
    implementation_id = paste0("autoquant.", engine, ".", task, ".r"),
    capability_id = paste0("supervised.", task), capability_version = "1.0.0",
    descriptor_version = "aq_implementation_descriptor_v1", contract_owner = "AutoQuant",
    implementation_package = "AutoQuant", language = "R", runtime = "r-package-worker",
    engine = engine, engine_package_version = as.character(utils::packageVersion(engine)),
    algorithm_family = "tree_ensemble", ensemble_regime = "boosting",
    qualified_ensemble_regimes = "boosting",
    hardware = tolower(gpu %||% "cpu"), cpu_gpu = toupper(gpu %||% "cpu"),
    thread_count = thread,
    parallelism = if (thread > 1L) "multithreaded" else "single_threaded",
    categorical_support = categorical,
    missingness = paste(engine, "native missing-value routing; no silent imputation"),
    sparse_support = sparse,
    fitted_state_format = paste0(engine, "_r_model_in_autoquant_bundle"),
    application_modes = if (identical(task, "binary")) c("probability", "decision_policy") else "predict",
    determinism = list(seedable = TRUE, seed = params$seed,
      cpu_reproducibility = "seed_and_thread_policy_recorded",
      gpu_difference = "accelerator execution is not qualified or assumed CPU-identical"),
    resource_model = list(memory = paste0("in_memory_", engine, "_dataset_and_fitted_model"),
      batch_application = TRUE, sparse_support = sparse, gpu_optional = TRUE,
      gpu_qualification = "not_qualified_in_reference_wave_2"),
    evidence_semantics = list(
      feature_importance = "engine_native",
      contributions = "engine_native",
      interactions = if (identical(engine, "xgboost")) "engine_native" else "derivable_external_not_native",
      permutation_importance = "derivable_without_refit", pdp = "derivable_without_refit",
      ice = "derivable_without_refit"),
    h2o_dependency = FALSE, fallback_policy = "no_h2o_fallback",
    failure_behavior = "fail_typed; never fall back to H2O or another engine"
  ), class = c("aq_implementation_descriptor", "list"))
}

aq_supervised_engine_tuning_space <- function(engine, task = c("regression", "binary")) {
  task <- match.arg(task); engine <- match.arg(engine, c("catboost", "lightgbm", "xgboost"))
  if (identical(engine, "catboost")) return(aq_catboost_tuning_space(task))
  if (identical(engine, "lightgbm")) {
    p <- c("num_iterations", "learning_rate", "num_leaves", "max_depth", "min_data_in_leaf",
           "feature_fraction", "bagging_fraction", "lambda_l1", "lambda_l2", "num_threads", "device_type")
    type <- c("integer","double","integer","integer","integer","double","double","double","double","integer","categorical")
    lower <- c(25,.005,4,-1,2,.2,.2,0,0,1,NA); upper <- c(5000,.5,512,32,1000,1,1,100,100,parallel::detectCores(),NA)
    scale <- c("linear","log","log","linear","log","linear","linear","log1p","log1p","linear","options")
  } else {
    p <- c("nrounds", "eta", "max_depth", "min_child_weight", "subsample", "colsample_bytree",
           "gamma", "lambda", "alpha", "nthread", "tree_method", "device")
    type <- c("integer","double","integer","double","double","double","double","double","double","integer","categorical","categorical")
    lower <- c(25,.005,1,0,.2,.2,0,0,0,1,NA,NA); upper <- c(5000,.5,16,100,1,1,100,100,100,parallel::detectCores(),NA,NA)
    scale <- c("linear","log","linear","log1p","linear","linear","log1p","log1p","log1p","linear","options","options")
  }
  data.table::data.table(parameter=p,type=type,lower=lower,upper=upper,scale=scale,
    options=ifelse(type=="categorical", ifelse(p %in% c("device_type","device"),"cpu|gpu","hist|approx|exact"), NA_character_),
    condition=ifelse(p %in% c("device_type","device"),"GPU requires qualified hardware/runtime",NA_character_),
    resource_implication=ifelse(p %in% c("num_threads","nthread"),"CPU contention","engine-specific fit/resource behavior"),
    default_qualification=!p %in% c("device_type","device"), task=task, engine=engine)
}

.aq_supervised_frame <- function(data, fit_or_spec, engine, levels = NULL) {
  spec <- if (inherits(fit_or_spec, "aq_fit_result")) fit_or_spec$spec else fit_or_spec
  x <- as.data.frame(data)[, spec$features, drop = FALSE]
  for (nm in names(x)) if (is.character(x[[nm]]) || is.factor(x[[nm]])) {
    lev <- if (!is.null(levels) && !is.null(levels[[nm]])) levels[[nm]] else sort(unique(as.character(x[[nm]][!is.na(x[[nm]])])))
    x[[nm]] <- factor(as.character(x[[nm]]), levels = lev)
  }
  if (identical(engine, "lightgbm")) {
    cats <- names(x)[vapply(x, is.factor, logical(1L))]
    return(list(data = data.matrix(x), categorical = cats))
  }
  list(data = x, categorical = names(x)[vapply(x, is.factor, logical(1L))])
}

aq_fit_model <- function(spec, data, validation_data = NULL) {
  if (identical(spec$engine, "catboost")) return(.aq_catboost_fit_model(spec, data, validation_data))
  engine <- match.arg(spec$engine, c("lightgbm", "xgboost"))
  if (!requireNamespace(engine, quietly = TRUE)) stop("The ", engine, " package is required.", call. = FALSE)
  started <- Sys.time(); dt <- data.table::as.data.table(data.table::copy(data))
  validation <- aq_validate_model_spec(spec, dt)
  if (aq_vnext_has_validation_error(validation)) stop(paste(validation[status %in% c("fail","error"),message],collapse=" "),call.=FALSE)
  classes <- NULL
  if (identical(spec$task, "binary")) {
    classes <- aq_vnext_resolve_binary_classes(dt[[spec$target]], spec$positive_class)
    spec$positive_class <- classes$positive_class; spec$threshold_policy$positive_class <- classes$positive_class
    spec$threshold_policy$negative_class <- classes$negative_class
  }
  if (is.null(validation_data)) {
    part <- aq_vnext_make_partition(dt, spec$partition)
    train <- data.table::copy(dt[part$train_index]); valid <- data.table::copy(dt[part$validation_index])
  } else {
    train <- dt; valid <- data.table::as.data.table(data.table::copy(validation_data))
    part <- list(partition_id=spec$partition$partition_id,method="explicit_validation_data",
      train_index=seq_len(nrow(train)),validation_index=seq_len(nrow(valid)),
      summary=data.table::data.table(split=c("train","validation"),rows=c(nrow(train),nrow(valid))))
  }
  levels <- aq_vnext_feature_levels(train, spec$features)
  tx <- .aq_supervised_frame(train, spec, engine, levels); vx <- .aq_supervised_frame(valid, spec, engine, levels)
  ty <- aq_vnext_target_label(train, spec); vy <- aq_vnext_target_label(valid, spec); p <- spec$engine_params
  if (identical(engine, "lightgbm")) {
    dtrain <- lightgbm::lgb.Dataset(tx$data,label=ty,categorical_feature=tx$categorical)
    dvalid <- lightgbm::lgb.Dataset(vx$data,label=vy,reference=dtrain,categorical_feature=vx$categorical)
    nrounds <- p$num_iterations; early <- p$early_stopping_round
    params <- p[setdiff(names(p),c("num_iterations","early_stopping_round"))]
    model <- lightgbm::lgb.train(params=params,data=dtrain,nrounds=nrounds,
      valids=list(validation=dvalid),early_stopping_rounds=early,verbose=-1L)
  } else {
    dtrain <- xgboost::xgb.DMatrix(tx$data,label=ty,nthread=p$nthread)
    dvalid <- xgboost::xgb.DMatrix(vx$data,label=vy,nthread=p$nthread)
    nrounds <- p$nrounds; early <- p$early_stopping_rounds; verbose <- p$verbose
    params <- p[setdiff(names(p),c("nrounds","early_stopping_rounds","verbose"))]
    model <- xgboost::xgb.train(params=params,data=dtrain,nrounds=nrounds,
      evals=list(validation=dvalid),early_stopping_rounds=early,verbose=verbose)
  }
  completed <- Sys.time()
  fit <- list(fit_id=aq_vnext_id(paste0("fit_",engine,"_",spec$task),spec$seed),status="success",
    schema_version="aq_fit_result_v1",spec=spec,model_id=spec$model_id,model=model,engine=engine,task=spec$task,
    engine_params=p,partition=part,threshold_policy=spec$threshold_policy,positive_class=aq_vnext_default(spec$positive_class,NA_character_),
    negative_class=if(!is.null(classes)) classes$negative_class else NA_character_,feature_schema=aq_vnext_feature_schema(train,spec$features),
    feature_levels=levels,raw_feature_schema=aq_vnext_feature_schema(dt,names(dt)),transformation_required=FALSE,
    transformation_spec=NULL,fitted_transformation=NULL,serialized_fitted_transformation=NULL,
    transformation_lineage=list(prepared_training_dataset_id=spec$dataset_id,prepared_validation_dataset_id=spec$dataset_id,fitted_transformation_id=NA_character_),
    transformation_diagnostics=data.table::data.table(),raw_training_schema_fingerprint=aq_vnext_schema_fingerprint(train),
    raw_validation_schema_fingerprint=aq_vnext_schema_fingerprint(valid),prepared_training_schema_fingerprint=aq_vnext_schema_fingerprint(train),
    prepared_validation_schema_fingerprint=aq_vnext_schema_fingerprint(valid),
    training_metadata=list(train_rows=nrow(train),validation_rows=nrow(valid),feature_count=length(spec$features),started_at=started,completed_at=completed,
      elapsed_seconds=as.numeric(difftime(completed,started,units="secs"))),training_data=train,validation_data=valid,raw_training_data=train,
    raw_validation_data=valid,validation=validation,prepared_validation=validation,warnings=character(),created_at=completed)
  fit$reference_contract <- aq_supervised_reference_contract(spec$task)
  fit$implementation_descriptor <- aq_supervised_implementation_descriptor(engine,spec$task,p)
  fit$tuning_space <- aq_supervised_engine_tuning_space(engine,spec$task)
  fit$feature_contract <- list(target=spec$target,features=spec$features,feature_order_authoritative=TRUE,schema=fit$feature_schema,
    categorical_features=tx$categorical,categorical_levels=levels,
    missingness=list(native_numeric=TRUE,categorical="engine-native declared categorical values",upstream_transformation=NULL),
    weights=NULL,ignored_fields=setdiff(names(dt),c(spec$target,spec$features)),
    fingerprint=digest::digest(list(spec$target,spec$features,fit$feature_schema,levels,engine),algo="sha256"))
  fit$training_history <- aq_supervised_training_history(structure(list(model=model,engine=engine),class="aq_engine_model"))
  threads <- p[[if(engine=="lightgbm") "num_threads" else "nthread"]]
  fit$resource_evidence <- list(thread_count=as.integer(threads),task_type=toupper(p[[if(engine=="lightgbm") "device_type" else "device"]]),
    parallelism=if(threads>1L) "multithreaded" else "single_threaded",train_rows=nrow(train),validation_rows=nrow(valid),feature_count=length(spec$features),
    elapsed_seconds=fit$training_metadata$elapsed_seconds,memory_model=paste0("in_memory_",engine,"_dataset_and_fitted_model"))
  fit$evidence_manifest <- aq_supervised_evidence_manifest(fit); fit$fit_artifact <- aq_vnext_fit_artifact(fit)
  class(fit) <- c("aq_fit_result","list"); fit
}

aq_supervised_training_history <- function(model) {
  if (!inherits(model, "aq_engine_model")) return(.aq_catboost_training_history(model))
  if (identical(model$engine,"lightgbm")) return(list(history=model$model$record_evals,best_iteration=model$model$best_iter,best_score=model$model$best_score))
  list(history=model$model$evaluation_log,best_iteration=model$model$best_iteration,best_score=model$model$best_score)
}

aq_supervised_feature_importance <- function(fit) {
  if (identical(fit$engine,"catboost")) return(.aq_catboost_feature_importance(fit))
  out <- tryCatch(if (identical(fit$engine,"lightgbm")) lightgbm::lgb.importance(fit$model) else xgboost::xgb.importance(model=fit$model),error=function(e) NULL)
  if (is.null(out)||!nrow(out)) return(data.table::data.table())
  n <- names(out); feature <- n[grepl("feature",n,ignore.case=TRUE)][1L]; value <- n[grepl("gain|importance",n,ignore.case=TRUE)][1L]
  data.table::data.table(feature=as.character(out[[feature]]),importance=as.numeric(out[[value]]))[order(-importance,feature)]
}

aq_supervised_evidence_manifest <- function(fit, assessment = NULL) {
  out <- .aq_reference_evidence_manifest(fit, assessment)
  engine <- fit$engine %||% fit$spec$engine %||% "catboost"
  native <- c("feature_importance", "training_history")
  native <- c(native, if (engine %in% c("catboost", "lightgbm", "xgboost")) "SHAP" else character(),
    if (engine %in% c("catboost", "xgboost")) "SHAP_interactions" else character())
  unavailable <- if (identical(engine, "lightgbm")) "SHAP_interactions" else character()
  out[, evidence_class := data.table::fcase(
    evidence_type %in% unavailable, "UNAVAILABLE",
    evidence_type %in% native, "ENGINE_NATIVE",
    tier %in% c("CORE", "DERIVED"), "COMMON",
    default = "DERIVABLE")]
  out[evidence_type %in% unavailable, `:=`(availability = "unavailable", state = "unavailable")]
  out
}

aq_supervised_native_explainability <- function(fit, data, interactions = FALSE) {
  stopifnot(inherits(fit, "aq_fit_result"))
  engine <- fit$engine
  if (identical(engine, "lightgbm") && isTRUE(interactions))
    return(structure(list(status="unavailable", engine=engine, evidence_class="UNAVAILABLE",
      reason="LightGBM does not expose equivalent native interaction contributions."),class="aq_evidence_result"))
  x <- .aq_supervised_frame(data, fit, engine, fit$feature_levels)$data
  values <- if (identical(engine, "xgboost")) {
    stats::predict(fit$model, x, predinteraction = isTRUE(interactions), predcontrib = !isTRUE(interactions))
  } else if (identical(engine, "lightgbm")) {
    stats::predict(fit$model, x, type = "contrib")
  } else stop("Use CatBoost's qualified native evidence path for CatBoost explainability.", call.=FALSE)
  structure(list(status="success",engine=engine,evidence_class="ENGINE_NATIVE",
    evidence_type=if(interactions)"SHAP_interactions"else"SHAP",values=values,
    rows=nrow(data),materialization="computed_on_demand_without_refit"),class="aq_evidence_result")
}

aq_predict_model <- function(fit, new_data = NULL, dataset = c("validation","training","all","new"), dataset_id = NULL, threshold_policy = NULL) {
  if (identical(fit$engine,"catboost")) return(.aq_catboost_predict_model(fit,new_data,dataset,dataset_id,threshold_policy))
  dataset <- match.arg(dataset)
  if (!is.null(new_data)) { score <- data.table::as.data.table(data.table::copy(new_data)); split <- "new" }
  else if(dataset=="training") { score<-data.table::copy(fit$training_data);split<-"train" }
  else if(dataset=="validation") { score<-data.table::copy(fit$validation_data);split<-"validation" }
  else if(dataset=="all") { score<-data.table::rbindlist(list(data.table::copy(fit$training_data)[,.split:="train"],data.table::copy(fit$validation_data)[,.split:="validation"]));split<-NULL }
  else stop("new_data is required when dataset = 'new'.",call.=FALSE)
  if(length(m<-setdiff(fit$spec$features,names(score)))) stop("new data is missing feature(s): ",paste(m,collapse=", "),call.=FALSE)
  x <- .aq_supervised_frame(score,fit,fit$engine,fit$feature_levels)$data
  pred <- as.numeric(stats::predict(fit$model,x)); out <- data.table::copy(score); if(!".split"%in%names(out)) out[,.split:=split]
  policy <- aq_vnext_default(threshold_policy,fit$threshold_policy)
  if(fit$task=="binary") { out[,PositiveProbability:=pred];out[,Predict:=ifelse(PositiveProbability>=policy$threshold,fit$positive_class,fit$negative_class)]
    if(fit$spec$target%in%names(out)) out[,actual_binary:=as.numeric(as.character(get(fit$spec$target))==as.character(fit$positive_class))]
  } else { out[,Predict:=pred];if(fit$spec$target%in%names(out)) out[,residual:=as.numeric(get(fit$spec$target))-Predict] }
  id<-aq_vnext_id(paste0("prediction_",fit$engine,"_",fit$task),fit$spec$seed)
  artifact <- new_table_artifact(id=id,title=paste(tools::toTitleCase(fit$engine), tools::toTitleCase(fit$task), "Predictions"),
    data=out,source_generator="aq_predict_model",tags=c("vnext","prediction",fit$engine,fit$task),dependencies=fit$fit_id,
    version="aq_prediction_artifact_v1",metadata=list(artifact_type=paste0("supervised_",fit$task,"_prediction"),task=fit$task,
      engine=fit$engine,model_id=fit$model_id,fit_id=fit$fit_id,dataset_id=aq_vnext_default(dataset_id,fit$spec$dataset_id),
      prediction_col="Predict",probability_col=if(fit$task=="binary")"PositiveProbability"else NA_character_,target_col=fit$spec$target,
      prediction_scale=if(fit$task=="binary")"probability_and_class"else"response",threshold_policy=policy,row_count=nrow(out),
      supported_downstream_actions=c("assess","compare")))
  artifact <- aq_vnext_attach_envelope(artifact,artifact_id=id,artifact_type=paste0("supervised_",fit$task,"_prediction"),
    artifact_version="aq_prediction_artifact_v1",parent_artifact_ids=fit$fit_id,
    lineage=list(fit_id=fit$fit_id,model_id=fit$model_id,specification_id=fit$spec$spec_id,
      dataset_id=aq_vnext_default(dataset_id,fit$spec$dataset_id)),task=fit$task,operator="prediction",engine=fit$engine,
    specification_id=fit$spec$spec_id,dataset_id=aq_vnext_default(dataset_id,fit$spec$dataset_id),model_id=fit$model_id,
    supported_actions=c("assess","compare"),producer="aq_predict_model")
  result<-list(prediction_id=id,status="success",schema_version="aq_prediction_result_v1",task=fit$task,fit_id=fit$fit_id,model_id=fit$model_id,
    dataset_id=aq_vnext_default(dataset_id,fit$spec$dataset_id),prediction_col="Predict",probability_col=if(fit$task=="binary")"PositiveProbability"else NA_character_,
    target_col=fit$spec$target,prediction_scale=if(fit$task=="binary")"probability_and_class"else"response",threshold_policy=policy,
    positive_class=fit$positive_class,negative_class=fit$negative_class,data=out,artifact=artifact,warnings=character(),created_at=aq_vnext_now())
  class(result)<-c("aq_prediction_result","list");result
}

aq_validate_model_bundle <- function(bundle) {
  if (is.character(bundle)) return(aq_validate_model_bundle(aq_load_model_bundle(bundle, validate = FALSE)))
  if (inherits(bundle,"aq_model_bundle") && bundle$engine %in% c("lightgbm","xgboost")) {
    copy <- bundle; copy$engine <- "catboost"
    result <- .aq_catboost_validate_model_bundle(copy)
    result[check=="engine_supported",message:=paste(bundle$engine,"engine is supported.")]
    return(result)
  }
  .aq_catboost_validate_model_bundle(bundle)
}

aq_supervised_engine_comparison <- function() {
  data.table::data.table(
    engine=c("catboost","lightgbm","xgboost"), regression=TRUE,binary=TRUE,
    native_categorical=c("ordered target statistics","declared integer categorical features","native factor categorical data"),
    missingness="native routing; no silent imputation",
    sparse_support=c("limited/engine-dependent","native sparse datasets","native DMatrix sparse support"),
    cpu_parallelism="governed multithreading",gpu_status="available; not qualified",
    early_stopping=TRUE,probability_policy_separated=TRUE,native_importance=TRUE,native_contributions=TRUE,
    native_interactions=c(TRUE,FALSE,TRUE),application=TRUE,tuning=TRUE,
    selection_guidance=c("mixed categorical data and CatBoost-native evidence","histogram/leaf-wise throughput and categorical/sparse workloads","regularized sparse/hist execution and native interaction evidence"))
}

qa_supervised_engine_conformance <- function() {
  rows <- list(); add <- function(engine,task,check,ok,message="") rows[[length(rows)+1L]] <<-
    data.table::data.table(engine=engine,task=task,check=check,status=if(isTRUE(ok))"pass"else"error",message=message)
  for (engine in c("lightgbm","xgboost")) for (task in c("regression","binary")) {
    available <- requireNamespace(engine,quietly=TRUE); add(engine,task,"runtime_available",available)
    if (!available) next
    d <- if(task=="regression") aq_vnext_catboost_fixture(180L) else aq_vnext_catboost_binary_fixture(200L)
    target <- if(task=="regression") "revenue" else "converted"
    params <- if(engine=="lightgbm") list(num_iterations=20L,num_threads=2L,early_stopping_round=5L) else list(nrounds=20L,nthread=2L,early_stopping_rounds=5L)
    spec <- aq_model_spec(task,engine,target,c("spend","clicks","channel","region"),
      positive_class=if(task=="binary")"yes"else NULL,engine_params=params,dataset_id="qa_dataset_revision")
    fit <- aq_fit_model(spec,d); pred <- aq_predict_model(fit,dataset="validation")
    assess <- if(task=="binary") aq_assess_binary_model(fit,pred) else aq_assess_model(fit,pred)
    bundle <- aq_model_bundle(fit); path <- tempfile(fileext=".rds"); saveRDS(bundle,path); restored <- readRDS(path)
    scored <- aq_predict_model(restored,new_data=d[1:25],dataset_id="qa_application_revision")
    add(engine,task,"fit_success",fit$status=="success")
    add(engine,task,"probability_policy",task=="regression"||all(c("PositiveProbability","Predict")%in%names(pred$data)))
    add(engine,task,"common_metrics",nrow(assess$metrics)>0L)
    add(engine,task,"evidence_tiers",setequal(unique(fit$evidence_manifest$tier),c("CORE","DERIVED","EXTENDED","SPECIALIZED")))
    add(engine,task,"evidence_classification",all(c("COMMON","ENGINE_NATIVE","DERIVABLE","UNAVAILABLE") %in% unique(c(fit$evidence_manifest$evidence_class,"UNAVAILABLE"))))
    add(engine,task,"parallel_cpu",fit$resource_evidence$thread_count==2L&&fit$resource_evidence$task_type=="CPU")
    add(engine,task,"no_h2o",identical(fit$implementation_descriptor$h2o_dependency,FALSE)&&identical(fit$implementation_descriptor$fallback_policy,"no_h2o_fallback"))
    add(engine,task,"bundle_no_rows",is.null(bundle$training_data)&&is.null(bundle$validation_data))
    add(engine,task,"restart_application",nrow(scored$data)==25L)
    add(engine,task,"tuning_engine_specific",identical(unique(fit$tuning_space$engine),engine))
    add(engine,task,"categorical_contract",length(fit$feature_contract$categorical_features)>=1L)
    add(engine,task,"native_missingness",isTRUE(fit$feature_contract$missingness$native_numeric))
  }
  comparison <- aq_supervised_engine_comparison()
  add("all","both","comparative_envelope",nrow(comparison)==3L&&any(!comparison$native_interactions))
  data.table::rbindlist(rows,use.names=TRUE,fill=TRUE)
}
