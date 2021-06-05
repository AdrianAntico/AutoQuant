#' @title LightGBMArgs
#'
#' @author Adrian Antico
#' @family LightGBMArgs Helpers
#'
#' @param data. only used for mulitclass
#' @param TargetColumnName. only used for multiclass
#' @param input_model. = input_model,
#'
#' # Core parameters
#' @param task. = tolower(task),
#' @param objective. = objective,
#' @param boosting. = boosting,
#' @param LinearTree. = LinearTree,
#' @param Trees. = Trees,
#' @param eta. = eta,
#' @param num_leaves. = num_leaves,
#' @param NThreads. = NThreads,
#' @param device_type. = tolower(device_type),
#' @param deterministic. = deterministic,
#'
#' # Learning Control Parameters
#' @param force_col_wise. = force_col_wise,
#' @param force_row_wise. = force_row_wise,
#' @param max_depth. = max_depth,
#' @param min_data_in_leaf. = min_data_in_leaf,
#' @param min_sum_hessian_in_leaf. = min_sum_hessian_in_leaf,
#' @param bagging_freq. = bagging_freq,
#' @param bagging_fraction. = bagging_fraction,
#' @param feature_fraction. = feature_fraction,
#' @param feature_fraction_bynode. = feature_fraction_bynode,
#' @param extra_trees. = extra_trees,
#' @param early_stopping_round. = early_stopping_round,
#' @param first_metric_only. = first_metric_only,
#' @param max_delta_step. = max_delta_step,
#' @param lambda_l1. = lambda_l1,
#' @param lambda_l2. = lambda_l2,
#' @param linear_lambda. = linear_lambda,
#' @param min_gain_to_split. = min_gain_to_split,
#' @param drop_rate_dart. = drop_rate_dart,
#' @param max_drop_dart. = max_drop_dart,
#' @param skip_drop_dart. = skip_drop_dart,
#' @param uniform_drop_dart. = uniform_drop_dart,
#' @param top_rate_goss. = top_rate_goss,
#' @param other_rate_goss. = other_rate_goss,
#' @param monotone_constraints. = monotone_constraints,
#' @param monotone_constraints_method. = monotone_constraints_method,
#' @param monotone_penalty. = monotone_penalty,
#' @param forcedsplits_filename. = forcedsplits_filename,
#' @param refit_decay_rate. = refit_decay_rate,
#' @param path_smooth. = path_smooth,
#'
#' # IO Parameters (dataset parameters)
#' @param max_bin. = max_bin,
#' @param min_data_in_bin. = min_data_in_bin,
#' @param data_random_seed. = data_random_seed,
#' @param is_enable_sparse. = is_enable_sparse,
#' @param enable_bundle. = enable_bundle,
#' @param use_missing. = use_missing,
#' @param zero_as_missing. = zero_as_missing,
#' @param two_round. = two_round,
#'
#' # Convert Parameters (export code to cpp, wrap in cppCompile() for fast scoring on demand)
#' @param convert_model. = convert_model,
#' @param convert_model_language. = convert_model_language,
#'
#' # Objective Parameters
#' @param boost_from_average. = boost_from_average,
#' @param alpha. = alpha,                                              # regression
#' @param fair_c. = fair_c,                                            # regression
#' @param poisson_max_delta_step. = poisson_max_delta_step,            # regression
#' @param tweedie_variance_power. = tweedie_variance_power,            # regression
#' @param lambdarank_truncation_level. = lambdarank_truncation_level   # regression
#' @param is_unbalance. is_unbalance                                   # classification
#' @param scale_pos_weight. scale_pos_weight                           # classification
#' @param multi_error_top_k. multi_error_top_k                         # multiclass
#'
#' # Metric Parameters
#' @param is_provide_training_metric. = is_provide_training_metric,
#' @param eval_at. = eval_at,
#'
#' # GPU Parameters
#' @param gpu_platform_id. = gpu_platform_id,
#' @param gpu_device_id. = gpu_device_id,
#' @param gpu_use_dp. = gpu_use_dp,
#' @param num_gpu. = num_gpu
#'
#' @noRd
LightGBMArgs <- function(data. = NULL,
                         TargetColumnName. = NULL,
                         input_model. = NULL,

                         # Core parameters
                         task. = NULL,
                         objective. = NULL,
                         multi_error_top_k. = NULL,
                         boosting. = NULL,
                         LinearTree. = NULL,
                         Trees. = NULL,
                         eta. = NULL,
                         num_leaves. = NULL,
                         NThreads. = NULL,
                         device_type. = NULL,
                         deterministic. = NULL,

                         # Learning Control Parameters
                         force_col_wise. = NULL,
                         force_row_wise. = NULL,
                         max_depth. = NULL,
                         min_data_in_leaf. = NULL,
                         min_sum_hessian_in_leaf. = NULL,
                         bagging_freq. = NULL,
                         bagging_fraction. = NULL,
                         feature_fraction. = NULL,
                         feature_fraction_bynode. = NULL,
                         extra_trees. = NULL,
                         early_stopping_round. = NULL,
                         first_metric_only. = NULL,
                         max_delta_step. = NULL,
                         lambda_l1. = NULL,
                         lambda_l2. = NULL,
                         linear_lambda. = NULL,
                         min_gain_to_split. = NULL,
                         drop_rate_dart. = NULL,
                         max_drop_dart. = NULL,
                         skip_drop_dart. = NULL,
                         uniform_drop_dart. = NULL,
                         top_rate_goss. = NULL,
                         other_rate_goss. = NULL,
                         monotone_constraints. = NULL,
                         monotone_constraints_method. = NULL,
                         monotone_penalty. = NULL,
                         forcedsplits_filename. = NULL,
                         refit_decay_rate. = NULL,
                         path_smooth. = NULL,

                         # IO Parameters (dataset parameters)
                         max_bin. = NULL,
                         min_data_in_bin. = NULL,
                         data_random_seed. = NULL,
                         is_enable_sparse. = NULL,
                         enable_bundle. = NULL,
                         use_missing. = NULL,
                         zero_as_missing. = NULL,
                         two_round. = NULL,

                         # Convert Parameters (export code to cpp, wrap in cppCompile() for fast scoring on demand)
                         convert_model. = NULL,
                         convert_model_language. = NULL,

                         # Objective Parameters
                         boost_from_average. = NULL,
                         alpha. = NULL,
                         fair_c. = NULL,
                         poisson_max_delta_step. = NULL,
                         tweedie_variance_power. = NULL,
                         lambdarank_truncation_level. = NULL,
                         is_unbalance. = NULL,
                         scale_pos_weight. = NULL,

                         # Metric Parameters
                         is_provide_training_metric. = NULL,
                         eval_at. = NULL,

                         # GPU Parameters
                         gpu_platform_id. = NULL,
                         gpu_device_id. = NULL,
                         gpu_use_dp. = NULL,
                         num_gpu. = NULL) {

  # Parameter Setup
  params <- list()

  # Multiclass only
  params[['num_class']] <- if(!is.null(data.)) length(data.[, unique(get(TargetColumnName.))]) else NULL

  # High Level Parameters
  params[['input_model']] <- input_model.

  # Core parameters
  params[['task']] <- tolower(task.)
  params[['objective']] <- objective.
  params[['boosting']] <- boosting.
  params[['linear_tree']] <- LinearTree. # scale data (copula scaling :) ), CPU only, regression_l1 not supported
  params[['num_iterations']] <- as.integer(Trees.)
  params[['eta']] <- eta.
  params[['num_leaves']] <- num_leaves.
  params[['num_threads']] <- NThreads.
  params[['device_type']] <- if(!is.null(device_type.)) tolower(device_type.) else NULL
  params[['deterministic']] <- if(!is.null(device_type.) && tolower(device_type.) == 'cpu') deterministic. else NULL

  # Learning Control Parameters
  params[['force_col_wise']] <- if(!is.null(device_type.) && tolower(device_type.) == 'cpu') force_col_wise. else NULL
  params[['force_row_wise']] <- if(!is.null(device_type.) && tolower(device_type.) == 'cpu') force_row_wise. else NULL
  params[['max_depth']] <- max_depth.
  params[['min_data_in_leaf']] <- min_data_in_leaf.
  params[['min_sum_hessian_in_leaf']] <- min_sum_hessian_in_leaf.
  params[['bagging_freq']] <- bagging_freq.
  params[['bagging_fraction']] <- bagging_fraction.
  params[['feature_fraction']] <- feature_fraction.
  params[['feature_fraction_bynode']] <- feature_fraction_bynode.
  params[['extra_trees']] <- extra_trees.
  params[['early_stopping_round']] <- early_stopping_round.
  params[['first_metric_only']] <- first_metric_only.
  params[['max_delta_step']] <- max_delta_step.
  params[['lambda_l1']] <- lambda_l1.
  params[['lambda_l2']] <- lambda_l2.
  params[['linear_lambda']] <- linear_lambda.
  params[['min_gain_to_split']] <- min_gain_to_split.
  params[['drop_rate']] <- if(boosting. == 'dart') drop_rate_dart. else NULL
  params[['max_drop']] <- if(boosting. == 'dart') max_drop_dart. else NULL
  params[['skip_drop']] <- if(boosting. == 'dart') skip_drop_dart. else NULL
  params[['uniform_drop']] <- if(boosting. == 'dart') uniform_drop_dart. else NULL
  params[['top_rate']] <- if(boosting. == 'goss') top_rate_goss. else NULL
  params[['other_rate']] <- if(boosting. == 'goss') other_rate_goss. else NULL
  params[['monotone_constraints']] <- monotone_constraints.
  params[['monotone_constraints_method']] <- if(!is.null(monotone_constraints.)) monotone_constraints_method. else NULL
  params[['monotone_penalty']] <- if(!is.null(monotone_constraints.)) monotone_penalty. else NULL
  params[['forcedsplits_filename']] <- forcedsplits_filename.
  params[['refit_decay_rate']] <- if(!is.null(params[['task']]) && params[['task']] == 'refit') refit_decay_rate. else NULL
  params[['path_smooth']] <- if(!is.null(params[['min_data_in_leaf']]) && params[['min_data_in_leaf']] >= 2) path_smooth. else NULL

  # IO Parameters (dataset parameters)
  params[['max_bin']] <- max_bin.
  params[['min_data_in_bin']] <- min_data_in_bin.
  params[['data_random_seed']] <- data_random_seed.
  params[['is_enable_sparse']] <- is_enable_sparse.
  params[['enable_bundle']] <- enable_bundle.
  params[['use_missing']] <- use_missing.
  params[['zero_as_missing']] <- zero_as_missing.
  params[['two_round']] <- two_round.

  # Convert Parameters (export code to cpp, wrap in cppCompile() for fast scoring on demand)
  params[['convert_model']] <- convert_model.
  params[['convert_model_language']] <- convert_model_language.

  # Objective Parameters
  params[['boost_from_average']] <- boost_from_average.
  params[['alpha']] <- if(tolower(objective.) == 'quantile') alpha. else NULL
  params[['fair_c']] <- if(tolower(objective.) == 'fair') fair_c. else NULL
  params[['poisson_max_delta_step']] <- if(tolower(objective.) == 'poisson') poisson_max_delta_step. else NULL
  params[['tweedie_variance_power']] <- if(tolower(objective.) == 'tweedie') tweedie_variance_power. else NULL
  params[['lambdarank_truncation_level']] <- if(tolower(objective.) == 'lambdarank') lambdarank_truncation_level.
  params[['is_unbalance']] <- if(tolower(objective.) == 'binary') is_unbalance. else NULL
  params[['scale_pos_weight']] <- if(tolower(objective.) == 'binary') scale_pos_weight. else NULL
  params[['multi_error_top_k']] <- if(tolower(objective.) %chin% c('multi_logloss','multi_error','kullback_leibler','cross_entropy','cross_entropy_lambda')) multi_error_top_k. else NULL

  # Metric Parameters
  params[['is_provide_training_metric']] <- is_provide_training_metric.
  params[['eval_at']] <- if(tolower(objective.) %chin% c('lambdarank','rank_xendcg')) eval_at. else NULL

  # GPU Parameters
  params[['gpu_platform_id']] <- if(tolower(device_type.) == 'gpu') gpu_platform_id. else NULL
  params[['gpu_device_id']] <- if(tolower(device_type.) == 'gpu') gpu_device_id. else NULL
  params[['gpu_use_dp']] <- if(tolower(device_type.) == 'gpu') gpu_use_dp. else NULL
  params[['num_gpu']] <- if(tolower(device_type.) == 'gpu') num_gpu. else NULL

  # Return
  return(params)
}

#' @title LightGBMFinalParams
#'
#' @description Parameters for xgboost fitting
#'
#' @author Adrian Antico
#' @family LightGBM Helpers
#'
#' @param params. Passthrough
#' @param GridTune. Passthrough
#' @param TrainOnFull. Passthrough
#' @param NThreads. Passthrough
#' @param PassInGrid. Passthrough
#' @param BestGrid. Passthrough
#' @param Trees. = Trees,
#' @param eta. = eta,
#' @param num_leaves. = num_leaves,
#' @param max_depth. = max_depth,
#' @param min_data_in_leaf. = min_data_in_leaf,
#' @param bagging_freq. = bagging_freq,
#' @param bagging_fraction. = bagging_fraction,
#' @param feature_fraction. = feature_fraction,
#' @param feature_fraction_bynode. = feature_fraction_bynode,
#' @param lambda_l1. = lambda_l1,
#' @param lambda_l2. = lambda_l2,
#'
#' @noRd
LightGBMFinalParams <- function(params. = NULL,
                                GridTune.=FALSE,
                                PassInGrid.=NULL,
                                TrainOnFull.=FALSE,
                                BestGrid.=NULL,
                                Trees. = NULL,
                                eta. = NULL,
                                num_leaves. = NULL,
                                max_depth. = NULL,
                                min_data_in_leaf. = NULL,
                                bagging_freq. = NULL,
                                bagging_fraction. = NULL,
                                feature_fraction. = NULL,
                                feature_fraction_bynode. = NULL,
                                lambda_l1. = NULL,
                                lambda_l2. = NULL) {

  # Params
  if(is.null(params.)) params. <- list()

  # Grid tuning
  if(!is.null(PassInGrid.)) {
    if(PassInGrid.[,.N] > 1L) stop('PassInGrid needs to be a single row data.table')
    if(PassInGrid.[, BanditProbs_Grid_1] == -10) {
      PassInGrid. <- NULL
    } else {
      params.[['num_iterations']] <- PassInGrid.$Trees.
      params.[['eta']] <- PassInGrid.$eta.
      params.[['num_leaves']] <- PassInGrid.$num_leaves.
      params.[['max_depth']] <- PassInGrid.$max_depth.
      params.[['min_data_in_leaf']] <- PassInGrid.$min_data_in_leaf.
      params.[['bagging_freq']] <- PassInGrid.$bagging_freq.
      params.[['bagging_fraction']] <- PassInGrid.$bagging_fraction.
      params.[['feature_fraction']] <- PassInGrid.$feature_fraction.
      params.[['feature_fraction_bynode']] <- PassInGrid.$feature_fraction_bynode.
      params.[['lambda_l1']] <- PassInGrid.$lambda_l1.
      params.[['lambda_l2']] <- PassInGrid.$lambda_l2.
    }
  }

  # Define parameters for case where you want to run grid tuning
  if(GridTune. && !TrainOnFull. && BestGrid.[['RunNumber']] != 1L) {
    params.[['num_iterations']] <- BestGrid.$Trees.
    params.[['eta']] <- BestGrid.$eta.
    params.[['num_leaves']] <- BestGrid.$num_leaves.
    params.[['max_depth']] <- BestGrid.$max_depth.
    params.[['min_data_in_leaf']] <- BestGrid.$min_data_in_leaf.
    params.[['bagging_freq']] <- BestGrid.$bagging_freq.
    params.[['bagging_fraction']] <- BestGrid.$bagging_fraction.
    params.[['feature_fraction']] <- BestGrid.$feature_fraction.
    params.[['feature_fraction_bynode']] <- BestGrid.$feature_fraction_bynode.
    params.[['lambda_l1']] <- BestGrid.$lambda_l1.
    params.[['lambda_l2']] <- BestGrid.$lambda_l2.
  } else {
    for(z in seq_along(params.)) if(length(params.[[z]]) > 1L) params.[[z]] <- params.[[z]][length(params.[[z]])]
  }

  # Return params.
  return(params.)
}

#' @noRd
RemixLightGBM_Shap <- function(tree_dt,
                               num_class,
                               tree_index_mat,
                               leaf_index_mat) {
  tree_interpretation <- vector(mode = 'list', length = num_class)
  for(i in seq_len(num_class)) {
    next_interp_dt <- RemixLightGBM_ShapeShap(tree_dt = tree_dt, tree_index = tree_index_mat[, i], leaf_index = leaf_index_mat[, i])
    if(num_class > 1L) {
      data.table::setnames(x = next_interp_dt, old = 'Contribution', new = paste('Class', i - 1L))
    }
    tree_interpretation[[i]] <- next_interp_dt
  }
  if(num_class == 1L) {
    tree_interpretation_dt <- tree_interpretation[[1L]]
  } else {
    tree_interpretation_dt <- Reduce(f = function(x, y) {merge(x, y, by = 'Feature', all = TRUE)}, x = tree_interpretation)
    for(j in 2L:ncol(tree_interpretation_dt)) {
      data.table::set(x = tree_interpretation_dt, i = which(is.na(tree_interpretation_dt[[j]])), j = j, value = 0)
    }
  }
  return(tree_interpretation_dt)
}

#' @noRd
RemixLightGBM_ShapeShap <- function(tree_dt,
                                    tree_index,
                                    leaf_index) {
  interp_dt <- data.table::rbindlist(
    l = mapply(
      FUN = lightgbm:::single.tree.interprete,
      tree_id = tree_index, leaf_id = leaf_index, MoreArgs = list(tree_dt = tree_dt),
      SIMPLIFY = FALSE, USE.NAMES = TRUE),
    use.names = TRUE)
  interp_dt <- interp_dt[, .(Contribution = sum(Contribution)), by = 'Feature']
  x <- data.table::dcast.data.table(data = interp_dt, formula = 'A' ~ Feature, fun.aggregate = sum, value.var = 'Contribution')[, '.' := NULL]
  data.table::setnames(x, names(x), paste0('Shap_', names(x)))
  return(x)
}

#' @noRd
RemixLightGBM_FlatRowShap <- function(data = as.matrix(data.),
                                      model = model.,
                                      idxset = seq_len(data[,.N]),
                                      num_iteration = num_iteration) {

  tree_dt <- lightgbm::lgb.model.dt.tree(model = model, num_iteration = num_iteration)
  num_class <- model$.__enclos_env__$private$num_class
  tree_interpretation_dt_list <- vector(mode = 'list', length = length(idxset))
  pred_mat <- model$predict(data = data[idxset, , drop = FALSE], num_iteration = num_iteration, predleaf = TRUE)
  leaf_index_dt <- data.table::as.data.table(x = pred_mat)
  x <- lapply(X = leaf_index_dt, FUN = function(x) matrix(x, ncol = num_class, byrow = TRUE))
  tree_index_mat_list <- lapply(X = leaf_index_mat_list, FUN = function(x) {
    matrix(seq_len(length(x)) - 1L, ncol = num_class, byrow = TRUE)
  })
  for(i in seq_along(idxset)) {
    tree_interpretation_dt_list[[i]] <- RemixLightGBM_Shap(
      tree_dt = tree_dt,
      num_class = num_class,
      tree_index_mat = tree_index_mat_list[[i]],
      leaf_index_mat = leaf_index_mat_list[[i]])
  }
  return(tree_interpretation_dt_list)
}

#' @title LightGBMParameterGrids
#'
#' @author Adrian Antico
#' @family LightGBM Helpers
#'
#' @param num_iterations = Trees,
#' @param eta = eta,
#' @param max_depth = max_depth,
#' @param num_leaves = num_leaves,
#' @param min_data_in_leaf = min_data_in_leaf,
#' @param bagging_freq = bagging_freq,
#' @param bagging_fraction = bagging_fraction,
#' @param feature_fraction = feature_fraction,
#' @param feature_fraction_bynode = feature_fraction_bynode,
#' @param lambda_l1 = lambda_l1
#' @param lambda_l2 = lambda_l2
#' @return A list containing data.table's with the parameters shuffled and ready to test in the bandit framework
#' @noRd
LightGBMParameterGrids <- function(num_iterations = seq(100L, 5000L, 1000L),
                                   max_depth = seq(6L, 12L, 2L),
                                   eta = seq(0.10,0.50,0.10),
                                   num_leaves = c(11,21,31),
                                   min_data_in_leaf = c(5,10,15),
                                   bagging_freq = c(0,1,2),
                                   bagging_fraction = c(1,0.95,0.9),
                                   feature_fraction = c(1,0.9,0.8),
                                   feature_fraction_bynode = c(1,0.9,0.8),
                                   lambda_l1 = c(0,1,2),
                                   lambda_l2 = c(0,1,2)) {

  # Bucket params
  if(!is.null(num_iterations) && length(num_iterations) > 5L) num_iterations <- num_iterations[seq_len(5L)]
  if(!is.null(max_depth) && length(max_depth) > 5L) max_depth <- max_depth[seq_len(5L)]
  if(!is.null(eta) && length(eta) > 5L) eta <- eta[seq_len(5L)]

  # Random params
  if(!is.null(num_leaves) && length(num_leaves) > 3) num_leaves <- num_leaves[seq_len(3L)]
  if(!is.null(min_data_in_leaf) && length(min_data_in_leaf) > 3) min_data_in_leaf <- min_data_in_leaf[seq_len(3L)]
  if(!is.null(bagging_freq) && length(bagging_freq) > 3) bagging_freq <- bagging_freq[seq_len(3L)]
  if(!is.null(bagging_fraction) && length(bagging_fraction) > 3) bagging_fraction <- bagging_fraction[seq_len(3L)]
  if(!is.null(feature_fraction) && length(feature_fraction) > 3) feature_fraction <- feature_fraction[seq_len(3L)]
  if(!is.null(feature_fraction_bynode) && length(feature_fraction_bynode) > 3) feature_fraction_bynode <- feature_fraction_bynode[seq_len(3L)]
  if(!is.null(lambda_l1) && length(lambda_l1) > 3) lambda_l1 <- lambda_l1[seq_len(3L)]
  if(!is.null(lambda_l2) && length(lambda_l2) > 3) lambda_l2 <- lambda_l2[seq_len(3L)]

  # Create grid sets
  Grid <- data.table::CJ(

    # Basis for creating parsimonous buckets
    num_iterations = if(!is.null(num_iterations)) sort(num_iterations, decreasing = FALSE) else seq(500L, 5000L, 1000L),
    max_depth = if(!is.null(max_depth)) sort(max_depth, decreasing = FALSE) else seq(6L, 12L, 2L),
    eta = if(!is.null(eta)) sort(eta, decreasing = FALSE) else seq(0.10,0.50,0.10),

    # Random hyperparameters
    num_leaves = if(!is.null(num_leaves)) num_leaves else c(11,21,31),
    min_data_in_leaf = if(!is.null(min_data_in_leaf)) min_data_in_leaf else c(5,10,15),
    bagging_freq = if(!is.null(bagging_freq)) bagging_freq else c(0,1,2,3),
    bagging_fraction = if(!is.null(bagging_fraction)) bagging_fraction else c(1,0.95,0.9),
    feature_fraction = if(!is.null(feature_fraction)) feature_fraction else c(1,0.8,0.6),
    feature_fraction_bynode = if(!is.null(feature_fraction_bynode)) feature_fraction_bynode else c(1,0.9,0.8),
    lambda_l1 = if(!is.null(lambda_l1)) lambda_l1 else c(0,1,2),
    lambda_l2 = if(!is.null(lambda_l2)) lambda_l2 else c(0,1,2))

  # Total loops
  N_Trees <- length(unique(Grid[['num_iterations']]))
  N_Depth <- length(unique(Grid[['max_depth']]))
  N_LearningRate <- length(unique(Grid[['eta']]))
  Runs <- max(N_Trees, N_Depth, N_LearningRate)
  Grids <- list()

  # Create grid sets
  for(i in seq_len(Runs)) {
    if(i == 1L) {
      Grids[[paste0('Grid_',i)]] <- Grid[num_iterations <= unique(Grid[['num_iterations']])[min(i,N_Trees)] & max_depth <= unique(Grid[['max_depth']])[min(i,N_Depth)] & eta <= unique(Grid[['eta']])[min(i,N_LearningRate)]]
    } else {
      Grids[[paste0('Grid_',i)]] <- data.table::fsetdiff(
        Grid[num_iterations <= unique(Grid[['num_iterations']])[min(i,N_Trees)] & max_depth <= unique(Grid[['max_depth']])[min(i,N_Depth)] & eta <= unique(Grid[['eta']])[min(i,N_LearningRate)]],
        Grid[num_iterations <= unique(Grid[['num_iterations']])[min(i-1L,N_Trees)] & max_depth <= unique(Grid[['max_depth']])[min(i-1L,N_Depth)] & eta <= unique(Grid[['eta']])[min(i-1L,N_LearningRate)]])
    }
  }

  # Define experimental grid
  eGrid <- data.table::data.table(
    GridNumber = rep(-1, 10000L),
    RunNumber = 1L:10000L,
    RunTime = rep(-1, 10000L),
    EvalMetric = rep(-1,10000L),
    num_iterations = rep(-1,10000L),
    max_depth = rep(-1,10000L),
    eta = rep(-1,10000L),
    num_leaves = rep(-1,10000L),
    min_data_in_leaf = rep(-1,10000L),
    bagging_freq = rep(-1,10000L),
    bagging_fraction = rep(-1,10000L),
    feature_fraction = rep(-1,10000L),
    feature_fraction_bynode = rep(-1,10000L),
    lambda_l1 = rep(-1,10000L),
    lambda_l2 = rep(-1,10000L))

  # Shuffle grid sets
  for(shuffle in seq_len(1)) for(i in seq_len(Runs)) Grids[[paste0('Grid_',i)]] <- Grids[[paste0('Grid_',i)]][order(runif(Grids[[paste0('Grid_',i)]][,.N]))]

  # Return grid
  return(list(Grid = Grid, Grids = Grids, ExperimentalGrid = eGrid))
}

#' @title LightGBMGridParams
#'
#' @author Adrian Antico
#' @family LightGBM Helpers
#'
#' @param N. Passthrough
#' @param params Passthrough
#' @param counter. Passthrough
#' @param BanditArmsN. Passthrough
#' @param model_path. Passthrough
#' @param NewGrid. Passthrough
#' @param Grid. Passthrough
#' @param GridClusters. Passthrough
#' @noRd
LightGBMGridParams <- function(N. = N,
                               params = params,
                               counter. = NULL,
                               BanditArmsN. = NULL,
                               model_path. = NULL,
                               NewGrid. = NULL,
                               Grid. = NULL,
                               GridClusters. = NULL) {

  # Bucket params
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) params$num_iterations <- GridClusters.[[paste0('Grid_', counter.-1L)]][['num_iterations']][1L] else if(counter. != 1) params$num_iterations <- GridClusters.[[paste0('Grid_',NewGrid.)]][['num_iterations']][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) params$max_depth <- GridClusters.[[paste0('Grid_', counter.-1L)]][['max_depth']][1L] else if(counter. != 1) params$max_depth <- GridClusters.[[paste0('Grid_',NewGrid.)]][['max_depth']][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) params$eta <- GridClusters.[[paste0('Grid_', counter.-1L)]][['eta']][1L] else if(counter. != 1) params$eta <- GridClusters.[[paste0('Grid_',NewGrid.)]][['eta']][N.]

  # Random params
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) params$num_leaves <- GridClusters.[[paste0('Grid_', counter.-1L)]][['num_leaves']][1L] else if(counter. != 1) params$num_leaves <- GridClusters.[[paste0('Grid_',NewGrid.)]][['num_leaves']][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) params$min_data_in_leaf <- GridClusters.[[paste0('Grid_', counter.-1L)]][['min_data_in_leaf']][1L] else if(counter. != 1) params$min_data_in_leaf <- GridClusters.[[paste0('Grid_',NewGrid.)]][['min_data_in_leaf']][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) params$bagging_freq <- GridClusters.[[paste0('Grid_', counter.-1L)]][['bagging_freq']][1L] else if(counter. != 1) params$bagging_freq <- GridClusters.[[paste0('Grid_',NewGrid.)]][['bagging_freq']][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) params$bagging_fraction <- GridClusters.[[paste0('Grid_', counter.-1L)]][['bagging_fraction']][1L] else if(counter. != 1) params$bagging_fraction <- GridClusters.[[paste0('Grid_',NewGrid.)]][['bagging_fraction']][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) params$feature_fraction <- GridClusters.[[paste0('Grid_', counter.-1L)]][['feature_fraction']][1L] else if(counter. != 1) params$feature_fraction <- GridClusters.[[paste0('Grid_',NewGrid.)]][['feature_fraction']][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) params$feature_fraction_bynode <- GridClusters.[[paste0('Grid_', counter.-1L)]][['feature_fraction_bynode']][1L] else if(counter. != 1) params$feature_fraction_bynode <- GridClusters.[[paste0('Grid_',NewGrid.)]][['feature_fraction_bynode']][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) params$lambda_l1 <- GridClusters.[[paste0('Grid_', counter.-1L)]][['lambda_l1']][1L] else if(counter. != 1) params$lambda_l1 <- GridClusters.[[paste0('Grid_',NewGrid.)]][['lambda_l1']][N.]
  if(counter. != 1L && counter. <= BanditArmsN. + 1L) params$lambda_l2 <- GridClusters.[[paste0('Grid_', counter.-1L)]][['lambda_l2']][1L] else if(counter. != 1) params$lambda_l2 <- GridClusters.[[paste0('Grid_',NewGrid.)]][['lambda_l2']][N.]

  # Return
  return(params)
}
