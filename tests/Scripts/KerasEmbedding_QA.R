
keras::layer_input <- function (shape = NULL, batch_shape = NULL, name = NULL, dtype = NULL,
          sparse = FALSE, tensor = NULL, ragged = FALSE) {

  fn_arg_nms <- names(formals(fn))

  args <- keras:::capture_args(match.call(), list(shape = normalize_shape,batch_shape = normalize_shape))
  do.call(keras$layers$Input, args)
}






LayerList = NULL
object = text_input
trainable = FALSE
input_dim =  length(unique(Tokenize_Object$word_index)) + 1
output_dim = ncol(word2vec_embedding)
embeddings_initializer = "uniform"
embeddings_regularizer = keras::regularizer_l2(l = 0.05)
activity_regularizer = NA
embeddings_constraint = NA
mask_zero = TRUE
input_length = max_len # list(max_len)
batch_size = NA
name = NA
weights = list(data.table::as.data.table(word2vec_embedding))
i = 1

ArgsList <- list(
  'Embedding_trainable' = 'NULL',
  'Embedding_object' = 'NULL',
  'Embedding_input_dim' = 'NULL',
  'Embedding_output_dim' = 'NULL',
  'Embedding_embeddings_initializer' = 'uniform',
  'Embedding_embeddings_regularizer' = 'NULL',
  'Embedding_activity_regularizer' = 'NULL',
  'Embedding_embeddings_constraint' = 'NULL',
  'Embedding_mask_zero' = 'NULL',
  'Embedding_input_length' = 'NULL',
  'Embedding_batch_size' = 'NULL',
  'Embedding_name' = 'NULL',
  'Embedding_weights' = 'NULL',
  'Embedding_trainable' = 'NULL')

keras::layer_embedding(
  trainable = tryCatch({ArgsList$Embedding_trainable[[i]]}, error = function(x) NULL),
  object = tryCatch({ArgsList$Embedding_object[[i]]}, error = function(x) NULL),
  input_dim = tryCatch({ArgsList$Embedding_input_dim[[i]]}, error = function(x) NULL),
  output_dim = tryCatch({ArgsList$Embedding_output_dim[[i]]}, error = function(x) NULL),
  embeddings_initializer = tryCatch({ArgsList$Embedding_embeddings_initializer[[i]]}, error = function(x) NULL),
  embeddings_regularizer = tryCatch({ArgsList$Embedding_embeddings_regularizer[[i]]}, error = function(x) NULL),
  activity_regularizer = tryCatch({ArgsList$Embedding_activity_regularizer[[i]]}, error = function(x) NULL),
  embeddings_constraint = tryCatch({ArgsList$Embedding_embeddings_constraint[[i]]}, error = function(x) NULL),
  mask_zero = tryCatch({ArgsList$Embedding_mask_zero[[i]]}, error = function(x) NULL),
  input_length = tryCatch({ArgsList$Embedding_input_length[[i]]}, error = function(x) NULL),
  batch_size = tryCatch({ArgsList$Embedding_batch_size[[i]]}, error = function(x) NULL),
  name = tryCatch({ArgsList$Embedding_name[[i]]}, error = function(x) NULL),
  weights = tryCatch({list(ArgsList$Embedding_weights[[i]])}, error = function(x) NULL))

create_layer <- function (layer_class, object, args = list()) {

  safe_to_drop_nulls <- c("input_shape", "batch_input_shape", "batch_size", "dtype", "name", "trainable", "weights")

  for (nm in safe_to_drop_nulls) args[[nm]] <- args[[nm]]

  constraint_args <- grepl("^.*_constraint$", names(args))

  constraint_args <- names(args)[constraint_args]

  for (arg in constraint_args) args[[arg]] <- keras:::as_constraint(args[[arg]])

  if(inherits(layer_class, "R6ClassGenerator")) {

    if(identical(layer_class$get_inherit(), KerasLayer)) {

      c(layer, args) %<-% ckeras:::ompat_custom_KerasLayer_handler(layer_class, args)
      layer_class <- function(...) layer

    } else {

      layer_class <- r_to_py(layer_class, convert = TRUE)

    }
  }

  layer <- do.call(layer_class, args)

  if(missing(object) || is.null(object)) layer else invisible(keras:::compose_layer(object, layer))

}


function (object,
          input_dim,
          output_dim,
          embeddings_initializer = "uniform",
          embeddings_regularizer = NULL,
          activity_regularizer = NULL,
          embeddings_constraint = NULL,
          mask_zero = FALSE,
          input_length = NULL,
          batch_size = NULL,
          name = NULL,
          trainable = NULL,
          weights = NULL) {


  keras::create_layer(
    keras$layers$Embedding,
    object,
    list(
      input_dim = as.integer(input_dim),
      output_dim = as.integer(output_dim),
      embeddings_initializer = embeddings_initializer,
      embeddings_regularizer = embeddings_regularizer,
      activity_regularizer = activity_regularizer,
      embeddings_constraint = embeddings_constraint,
      mask_zero = mask_zero,
      input_length = if(!is.null(input_length)) as.integer(input_length) else NULL,
      batch_size = as_nullable_integer(batch_size),
      name = name,
      trainable = trainable,
      weights = weights))
}






API = TRUE
API_PreviousOut = Output
API_inputs = c(Output$LayerList$text, Output$LayerList$features)
API_outputs = Output$LayerList$outputtot
API_Name = 'model_glove_2'
LayerList = NULL
YTrain = y_train
FeaturesTrain = list(T2S_Output[[1L]], as.matrix(x_train_features))
YTest = as.matrix(y_test)
FeaturesTest = list(T2S_Output[[2L]], as.matrix(x_test_features))
Compile_optimizer = 'rmsprop'
Compile_loss = 'binary_crossentropy'
Compile_metrics = c("acc", "AUC")
Fit_validation_split = 0
Fit_class_weight = list("0" = 1, "1" = nrow(y_train) / sum(y_train))
Fit_sample_weight = NULL
Fit_batch_size = 5000
Fit_epochs = 20
Fit_shuffle = TRUE
Fit_callbacks = NULL
Fit_view_metrics = getOption("keras.view_metrics", default = "auto")
Fit_initial_epoch = 0
Fit_steps_per_epoch = NULL
Fit_validation_steps = NULL
ConfMatThresh = 0.50


CompiledModel = API_PreviousOut$LayerList[[length(API_PreviousOut$LayerList) - 1L]]
YTest = YTest
Features = FeaturesTest
ConfMatThresh = ConfMatThresh
ReturnFeatures = TRUE
ReturnMetrics = TRUE




























