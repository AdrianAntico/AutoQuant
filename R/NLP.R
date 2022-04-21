# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Text Preparation                                            ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# ----

# ----

#' @title KerasTokenizerFit
#'
#' @description Tokenize and Fit with the one function. Convert column of long text within rows to a long vector of words
#'
#' @author Adrian Antico
#' @family NLP
#'
#' @param Fit_Object Object to be passed into layer
#' @param Fit_Text = x_train_text, character vector of text. e.g. data[['TextColumn']]
#' @param Tok_num_words = NULL,
#' @param Tok_filters see example below
#' @param Tok_lower = TRUE,
#' @param Tok_split = " ",
#' @param Tok_char_level = FALSE,
#' @param Tok_oov_token = NULL,
#' @param RunTokenizer = TRUE,
#' @param RunFitTextTokenizer = TRUE
#'
#' @examples
#' \dontrun{
#' Tokenize_Object <- RemixAutoML:::KerasTokenizerFit(
#'   Fit_Text = x_train_text,
#'   Fit_Object = NULL,
#'   Tok_num_words = NULL,
#'   Tok_filters = "!\"#$%&()*+,-./:;<=>?@[\\]^_`{|}~\t\n",
#'   Tok_lower = TRUE,
#'   Tok_split = " ",
#'   Tok_char_level = FALSE,
#'   Tok_oov_token = NULL,
#'   RunTokenizer = TRUE,
#'   RunFitTextTokenizer = TRUE)
#' }
#'
#' @return List with two elements: VectorizedText and Tokenizer.
#' @export
KerasTokenizerFit <- function(Fit_Object = NULL,
                              Fit_Text = NULL,
                              Tok_num_words = NULL,
                              Tok_filters = "!\"#$%&()*+,-./:;<=>?@[\\]^_`{|}~\t\n",
                              Tok_lower = TRUE,
                              Tok_split = " ",
                              Tok_char_level = FALSE,
                              Tok_oov_token = NULL,
                              RunTokenizer = TRUE,
                              RunFitTextTokenizer = TRUE) {

  # Tokenize
  if(RunTokenizer) {
    TextOutput <- keras::text_tokenizer(
      num_words = Tok_num_words,
      filters = Tok_filters,
      lower = Tok_lower,
      split = Tok_split,
      char_level = Tok_char_level,
      oov_token = Tok_oov_token)
  }

  # Fit Text Tokenizer
  if(RunFitTextTokenizer) {
    TextOutput <- keras::fit_text_tokenizer(
      x = Fit_Text,
      object = if(RunTokenizer) TextOutput else Fit_Object)
  }

  # Return
  return(TextOutput)
}

#' @title Keras_text2seq_pad
#'
#' @description return padded text to sequence keras object
#'
#' @author Adrian Antico
#' @family NLP
#'
#' @param RunText2Seq = TRUE,
#' @param RunPadding = FALSE,
#'
#' @param T2S_tokenizer = NULL, keras::text_tokenizer keras::fit_text_tokenizer
#' @param T2S_text text character vector or a list() of text character vectors. If the list is named, those names will be used for the names in the output list that contain the matching data sets
#'
#' @param PadSeq_sequences NULL. Either a result from keras::texts_to_sequences() or a list() of results. If the list is named, those names will be used for the names in the output list that contain the matching data sets
#' @param PadSeq_padding = "pre",
#' @param PadSeq_maxlen = 150,
#' @param PadSeq_value = 0L,
#' @param PadSeq_truncating = "pre",
#' @param PadSeq_dtype = "int32"
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
Keras_t2s_pad <- function(RunText2Seq = TRUE,
                          RunPadding = FALSE,

                          # keras::text_to_sequences
                          T2S_tokenizer = NULL,
                          T2S_text = NULL,

                          # keras::pad_sequences
                          PadSeq_sequences = NULL,
                          PadSeq_padding = "pre",
                          PadSeq_maxlen = 150,
                          PadSeq_value = 0L,
                          PadSeq_truncating = "pre",
                          PadSeq_dtype = "int32") {

  # T2V then Padding
  if(RunPadding && RunText2Seq) {

    # List or vector supplied by user: list for multiple data sets. Vector for single
    if(length(T2S_text) == 1) {
      return(
        keras::pad_sequences(
          sequences  = keras::texts_to_sequences(tokenizer = T2S_tokenizer, T2S_text),
          padding    = PadSeq_padding,
          maxlen     = PadSeq_maxlen,
          value      = PadSeq_value,
          truncating = PadSeq_truncating,
          dtype      = PadSeq_dtype))

    } else {

      # Loop through datasets
      outlist <- list()
      for(i in seq_along(T2S_text)) {
        outlist[[i]] <- keras::pad_sequences(
          sequences  = keras::texts_to_sequences(tokenizer = T2S_tokenizer, T2S_text[[i]]),
          padding    = PadSeq_padding,
          maxlen     = PadSeq_maxlen,
          value      = PadSeq_value,
          truncating = PadSeq_truncating,
          dtype      = PadSeq_dtype)
      }

      # Name the list of elements
      if(length(names(T2S_text)) == length(outlist)) {
        names(outlist) <- names(T2S_text)
      } else if(length(names(T2S_text)) == 0) {
        names(outlist) <- paste0('Data', seq_along(outlist))
      }

      # Return
      return(outlist)
    }
  }

  # T2V Only
  if(!RunPadding && RunText2Seq) {

    # List or vector supplied by user: list for multiple data sets. Vector for single
    if(length(T2S_text) == 1) {
      return(
        keras::texts_to_sequences(
          tokenizer = T2S_tokenizer,
          T2S_text))

    } else {

      # Loop through datasets
      outlist <- list()
      for(i in seq_along(T2S_text)) {
        outlist[[i]] <- keras::texts_to_sequences(
          tokenizer = T2S_tokenizer,
          T2S_text)
      }

      # Name the list of elements
      if(length(names(T2S_text)) == length(outlist)) {
        names(outlist) <- names(T2S_text)
      } else if(length(names(T2S_text)) == 0) {
        names(outlist) <- paste0('Data', seq_along(outlist))
      }

      # Return
      return(outlist)
    }
  }

  # Padding Only
  if(RunPadding && !RunText2Seq && length(PadSeq_sequences) != 0) {

    # List or vector supplied by user: list for multiple data sets. Vector for single
    if(length(PadSeq_sequences) %in% c(0L, 1L)) {
      return(
        keras::pad_sequences(
          sequences  = PadSeq_sequences,
          padding    = PadSeq_padding,
          maxlen     = PadSeq_maxlen,
          value      = PadSeq_value,
          truncating = PadSeq_truncating,
          dtype      = PadSeq_dtype))
    } else {

      # Loop through datasets
      outlist <- list()
      for(i in seq_along(PadSeq_sequences)) {
        outlist[[i]] <- keras::pad_sequences(
          sequences  = PadSeq_sequences,
          padding    = PadSeq_padding,
          maxlen     = PadSeq_maxlen,
          value      = PadSeq_value,
          truncating = PadSeq_truncating,
          dtype      = PadSeq_dtype)
      }

      # Name the list of elements
      if(length(names(PadSeq_sequences)) == length(outlist)) {
        names(outlist) <- names(PadSeq_sequences)
      } else if(length(names(PadSeq_sequences)) == 0) {
        names(outlist) <- paste0('Data', seq_along(outlist))
      }

      # Return
      return(outlist)
    }

  } else {
    cat('Nothing Ran \n RunText2Seq = FALSE, or either \n  1. PadSeq_sequences = NULL & RunPadding = TRUE or FALSE \n  OR \n  2. PadSeq_sequences = !NULL & RunPadding = FALSE)')
  }
}

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Layers                                                      ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# ----

# ----

#' @title Keras
#'
#' @description Keras functions simplified for tracking args
#'
#' @family DL
#'
#' @author Adrian Antico
#'
#' @param LayerFunc Required, Positional. E.g. keras::layer_input without quotes
#' @param PriorOutput The resulting list contained in Output <- RemixAutoML::Keras(). If you pass this LayerList and ArgsList will be ignored. Same Output from ArgsList and LayerList.
#' @param Name Name for the LayerList and ArgsList elements
#' @param Debug FALSE
#' @param ... Passthrough args for keras layer_input
#'
#' @examples
#' \dontrun{
#' Output <- RemixAutoML::KerasInputs(keras::flatten, PriorOutput = Output, Name = 'flatten1', object = Output$LayerList$PreviousLayer)
#' }
#'
#'
#' @export
Keras <- function(LayerFunc,
                  PriorOutput = NULL,
                  Name = NULL,
                  Debug = FALSE,
                  ...) {

  if(missing(LayerFunc)) stop(cat('LayerFunc is required. \nAn example: \n keras::layer_input \nNote: \n do not add quotes and do not add parentheses \n parameter is positional so I typically start with RemixAutoML::Keras(keras::layer_input, LayerList = NULL, Name =', if(is.null(Name)) shQuote('Args1') else shQuote(Name), ')'))

  # Args fed to the function LayerFunc in: output <- do.call(LayerFunc, Args)
  Args <- list(...) #; for(i in seq_along(ArgsList)) assign(x = names(ArgsList)[i], value = ArgsList[[i]])
  if(Debug) {
    print('names(Args()'); print(names(Args))
    print('length(LayerList) == 0L'); print(length(LayerList) == 0L)
  }

  # PriorOutput is an Output <- RemixAutoML::Keras() object
  if(length(PriorOutput) > 0) {
    LayerList <- PriorOutput$LayerList
    ArgsList <- PriorOutput$ArgsList
  } else {
    LayerList <- list()
    ArgsList <- list()
  }

  # LayerList
  if(length(LayerList) == 0L) {
    if(length(Name) == 0L) {
      LayerList[['keras_1']] <- do.call(LayerFunc, Args)
    } else {
      LayerList[[Name]] <- do.call(LayerFunc, Args)
    }
  } else {
    if(length(Name) == 0L) {
      if(length(LayerList) != 0) {
        N <- length(names(LayerList))
      } else {
        N <- 1L
      }
      LayerList[[paste0('keras_', N + 1L)]] <- do.call(LayerFunc, Args)
    } else {
      LayerList[[Name]] <- do.call(LayerFunc, Args)
    }
  }

  # ArgsList
  if(length(ArgsList) == 0L && length(Name) == 0L) {
    ArgsList[['Args1']] <- Args
  } else if(length(ArgsList) == 0L && length(Name) == 1L) {
    ArgsList[[Name]] <- Args
  } else if(length(ArgsList) == 1L && length(Name) == 0L) {
    N <- grepl(pattern = ".*?([0-9]+).*", "\\1", x = names(ArgsList)[length(names(ArgsList))])
    N <- N + 1L
    ArgsList[[paste0('Args', N)]] <- Args
  } else if(length(ArgsList) == 1L && length(Name) == 1L) {
    ArgsList[[Name]] <- Args
  }

  # Return
  return(
    list(
      LayerList = LayerList,
      ArgsList = ArgsList))
}

#' @title KerasTrainEval
#'
#' @description Train model and generate evaluation metrics
#'
#' @author Adrian Antico
#' @family DL
#'
#' @param PriorOutput Default NULL. E.g. Output from a previous RemixAutoML::Keras() call. Ignore if using Sequential Mode (API = FALSE)
#' @param Name Default NULL. A name that is used to store and reference objects in the output list
#' @param inputs Default NULL. E.g. c(Output$LayerList$text, Output$LayerList$features)
#' @param outputs Default NULL. E.g. Output$LayerList$outputtot
#' @param Data_YTrain as.array(vector)
#' @param Data_FeaturesTrain input_train_text. Vector, matrix, or array of training data (or list if the model has multiple inputs). If all inputs in the model are named, you can also pass a list mapping input names to data. x can be NULL (default) if feeding from framework-native tensors (e.g. TensorFlow data tensors). You can also pass a tfdataset or a generator returning a list with (inputs, targets) or (inputs, targets, sample_weights).
#' @param Data_YTest as.array(vector). Vector, matrix, or array of target (label) data (or list if the model has multiple outputs). If all outputs in the model are named, you can also pass a list mapping output names to data. y can be NULL (default) if feeding from framework-native tensors (e.g. TensorFlow data tensors).
#' @param Data_FeaturesTest input_test_text
#' @param Fit_validation_split Default 0. Float between 0 and 1. Fraction of the training data to be used as validation data. The model will set apart this fraction of the training data, will not train on it, and will evaluate the loss and any model metrics on this data at the end of each epoch. The validation data is selected from the last samples in the x and y data provided, before shuffling.
#' @param Fit_class_weight E.g. list('0' = 1, '1' = nrow(YTrain) / sum(YTrain))
#' @param Fit_sample_weight = NULL
#' @param Fit_batch_size Default 32. Integer or NULL. Number of samples per gradient update. If unspecified, batch_size will default to 32.
#' @param Fit_epochs Default 10. Number of epochs to train the model. Note that in conjunction with initial_epoch, epochs is to be understood as "final epoch". The model is not trained for a number of iterations given by epochs, but merely until the epoch of index epochs is reached.
#' @param Fit_shuffle = TRUE, shuffle: Logical (whether to shuffle the training data before each epoch) or string (for "batch"). "batch" is a special option for dealing with the limitations of HDF5 data; it shuffles in batch-sized chunks. Has no effect when steps_per_epoch is not NULL.
#' @param Fit_callbacks Default NULL. List of callbacks to be called during training.
#' @param Fit_view_metrics Default getOption("keras.view_metrics", default = "auto"). View realtime plot of training metrics (by epoch). The default ("auto") will display the plot when running within RStudio, metrics were specified during model compile(), epochs > 1 and verbose > 0. Use the global keras.view_metrics option to establish a different default.
#' @param Fit_initial_epoch = 0,
#' @param Fit_steps_per_epoch = NULL,
#' @param Fit_validation_steps = NULL,
#' @param ConfMatThresh 0.50
#'
#' @examples
#' \dontrun{
#'
#' Output <- RemixAutoML:::KerasTrainEval(
#'   PriorOutput = NULL, # Output
#'   inputs = NULL, # c(Output$LayerList$text, Output$LayerList$features),
#'   outputs = NULL, # Output$LayerList$outputtot,
#'   Name = NULL,
#'   Data_YTrain = y_train,
#'   Data_FeaturesTrain = T2S_Output[[1L]],
#'   Data_YTest = y_test,
#'   Data_FeaturesTest = T2S_Output[[2L]],
#'   Compile_optimizer = 'rmsprop',
#'   Compile_loss = 'binary_crossentropy',
#'   Compile_metrics = c("acc", "AUC"),
#'   Fit_validation_split = 0,
#'   Fit_class_weight = NULL,
#'   Fit_sample_weight = NULL,
#'   Fit_batch_size = 2000,
#'   Fit_epochs = 10,
#'   ConfMatThresh = 0.50)
#'
#' # Step through function
#' PriorOutput = NULL # Output
#' inputs = c(Output$LayerList$text, Output$LayerList$features) # NULL
#' outputs = Output$LayerList$outputtot # NULL
#' Name = 'Model'
#' Data_YTrain = y_train
#' Data_FeaturesTrain = T2S_Output[[1L]]
#' Data_YTest = y_test
#' Data_FeaturesTest = T2S_Output[[2L]]
#' Compile_optimizer = 'rmsprop'
#' Compile_loss = 'binary_crossentropy'
#' Compile_metrics = c("acc", "AUC")
#' Fit_validation_split = 0
#' Fit_class_weight = NULL
#' Fit_sample_weight = NULL
#' Fit_batch_size = 2000
#' Fit_epochs = 10
#' Fit_shuffle = TRUE
#' Fit_callbacks = NULL
#' Fit_view_metrics = getOption("keras.view_metrics", default = "auto")
#' Fit_initial_epoch = 0
#' Fit_steps_per_epoch = NULL
#' Fit_validation_steps = NULL
#' ConfMatThresh = 0.50
#' }
#'
#' @export
KerasTrainEval <- function(PriorOutput = NULL,
                           inputs = NULL,
                           outputs = NULL,
                           Name = NULL,
                           Data_YTrain = NULL,
                           Data_FeaturesTrain = NULL,
                           Data_YTest = NULL,
                           Data_FeaturesTest = NULL,
                           Compile_optimizer = 'rmsprop',
                           Compile_loss = 'binary_crossentropy',
                           Compile_metrics = c("acc", "AUC"),
                           Fit_validation_split = 0,
                           Fit_class_weight = NULL,
                           Fit_sample_weight = NULL,
                           Fit_batch_size = 32,
                           Fit_epochs = 10,
                           Fit_shuffle = TRUE,
                           Fit_callbacks = NULL,
                           Fit_view_metrics = getOption("keras.view_metrics", default = "auto"),
                           Fit_initial_epoch = 0,
                           Fit_steps_per_epoch = NULL,
                           Fit_validation_steps = NULL,
                           ConfMatThresh = 0.50) {

  # ... equivalent
  Args <- c(as.list(environment()))

  # PriorOutput is an Output <- RemixAutoML::Keras() object
  if(length(PriorOutput) > 0) {

    # meta
    if(length(Name) == 0) nam <- 'Model' else nam <- Name
    ArgsList <- PriorOutput$ArgsList

    # ArgsList
    if(length(ArgsList) == 0L && length(Name) == 0L) {
      ArgsList <- list()
      ArgsList[['Args1']] <- Args
    } else if(length(ArgsList) == 0L && length(Name) == 1L) {
      ArgsList <- list()
      ArgsList[[Name]] <- Args
    } else if(length(ArgsList) == 1L && length(Name) == 0L) {
      N <- grepl(pattern = ".*?([0-9]+).*", "\\1", x = names(ArgsList)[length(names(ArgsList))])
      N <- N + 1L
      ArgsList[[paste0('Args', N)]] <- Args
    } else if(length(ArgsList) == 1L && length(Name) == 1L) {
      ArgsList[[Name]] <- Args
    }

  } else {
    stop('Must supply PriorOutput to use API = TRUE')
  }

  # Build model structure
  PriorOutput <- RemixAutoML::Keras(
    keras::keras_model, PriorOutput = PriorOutput, Name = nam,
    inputs = inputs,
    outputs = outputs)

  # Compile model
  PriorOutput <- RemixAutoML::Keras(
    keras::compile, PriorOutput = PriorOutput, Name = nam,
    object = PriorOutput$LayerList[[length(PriorOutput$LayerList)]],
    optimizer = Compile_optimizer,
    loss = Compile_loss,           # we have a binary classification, a single unit sigmoid in the dense layer so binary_crossentropy
    metrics = Compile_metrics)

  # Train model
  PriorOutput <- RemixAutoML::Keras(
    keras::fit, PriorOutput = PriorOutput, Name = paste0(nam, '_history'),
    object = PriorOutput$LayerList[[length(PriorOutput$LayerList)]],
    x = Data_FeaturesTrain, y = Data_YTrain,
    epochs = Fit_epochs,                         # maximum number of iterations, since we did not add any new information we will keep it at 1
    batch_size = Fit_batch_size,                 # how many reviews do we offer in each batch
    class_weight = Fit_class_weight,             # we have little Michelin restaurants, so we need to focus more on classifying these (set weights)
    sample_weight = Fit_sample_weight,
    validation_data = if(length(FeaturesTest) != 0 & length(Data_YTest) != 0) list(Data_FeaturesTest, Data_YTest) else NULL, # check train results againts test data
    validation_split = Fit_validation_split,
    shuffle = Fit_shuffle,
    callbacks = Fit_callbacks,
    view_metrics = Fit_view_metrics,
    initial_epoch = Fit_initial_epoch,
    steps_per_epoch = Fit_steps_per_epoch,
    validation_steps = Fit_validation_steps)

  # Evaluate model
  temp <- RemixAutoML:::Keras_BinaryScoreEval(
    CompiledModel = PriorOutput$LayerList[[length(PriorOutput$LayerList) - 1L]],
    YTest = YTest,
    Features = FeaturesTest,
    ConfMatThresh = ConfMatThresh,
    ReturnFeatures = TRUE,
    ReturnMetrics = TRUE)
  PriorOutput[[paste0(nam, '_ScoreData')]] <- temp$ScoreData
  PriorOutput[[paste0(nam, '_EvalMetrics')]] <- temp$Metrics
  PriorOutput[[paste0(nam, '_ConfusionMatrix')]] <- temp[["ConfusionMatrix"]]
  PriorOutput[[paste0(nam, '_PlotTrainHistory')]] <- plot(PriorOutput$LayerList[[length(PriorOutput$LayerList)]]) + RemixAutoML::ChartTheme() + ggplot2::ylab('')

  # Return output
  return(PriorOutput)
}

#' @title Keras_BinaryScoreEval
#'
#' @description
#'
#' @author Adrian Antico
#' @family DL
#'
#' @param CompiledModel = model_word2vec,
#' @param YTest Vector of 0s and 1s
#' @param Features Input features as a matrix
#' @param CostMatrixWeights = c(0,1,1,0) TP, FN, FP, TN
#' @param ConfMatThresh 'max' or 'min'. Tells function whether to grab the threshold associated with the max or min of the chose CutOffMetric
#' @param ReturnMetrics = TRUE. FALSE and only the score data will be returned
#' @param ReturnFeatures = TRUE. FALSE and only the predicted values will be returned
#'
#' @export
Keras_BinaryScoreEval <- function(CompiledModel = NULL, # model_word2vec
                                  YTest = NULL,         # y_test
                                  Features = NULL,      # input_test_text
                                  CostMatrixWeights = c(0,1,1,0),
                                  ConfMatThresh = 0.50,
                                  ReturnMetrics = TRUE,
                                  ReturnFeatures = TRUE) {

  # Score and prepare data
  s <- data.table::as.data.table(predict(CompiledModel, Features))
  if(is.list(Features)) {
    temp <- Features[[1L]]
    for(i in seq_along(Features)[-1L]) {
      temp <- cbind(temp, Features[[1L]])
    }
  } else {
    temp <- Features
  }
  data.table::setnames(s, 'V1', 'p1')
  s[, Actual := YTest]

  # Remix Metrics
  if(ReturnMetrics) {
    m <- RemixAutoML:::BinaryMetrics(
      ValidationData. = s,
      TargetColumnName. = 'Actual',
      ClassWeights. = c(1,1),
      CostMatrixWeights. = CostMatrixWeights,
      ModelID. = 'keras',
      SaveModelObjects. = FALSE,
      Method = 'threshold')
  } else {
    m <- NULL
  }

  # Add Classification
  s[, Predict := data.table::fifelse(p1 >= ConfMatThresh, 1, 0)]
  c <- table(s$Actual, s$Predict, dnn = c('Actual', 'Predict'))

  # Return list
  return(list(
    ScoreData = if(ReturnFeatures) cbind(s, temp) else s,
    Metrics = m,
    ConfusionMatrix = c
  ))
}
