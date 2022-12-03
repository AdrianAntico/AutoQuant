#' @title AutoWord2VecModeler
#'
#' @description This function allows you to automatically build a word2vec model and merge the data onto your supplied dataset
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data Source data table to merge vects onto
#' @param BuildType Choose from "individual" or "combined". Individual will build a model for every text column. Combined will build a single model for all columns.
#' @param stringCol A string name for the column to convert via word2vec
#' @param KeepStringCol Set to TRUE if you want to keep the original string column that you convert via word2vec
#' @param model_path A string path to the location where you want the model and metadata stored
#' @param vects The number of vectors to retain from the word2vec model
#' @param MinWords For H2O word2vec model
#' @param WindowSize For H2O word2vec model
#' @param Epochs For H2O word2vec model
#' @param SaveModel Set to "standard" to save normally; set to "mojo" to save as mojo. NOTE: while you can save a mojo, I haven't figured out how to score it in the AutoH20Scoring function.
#' @param Threads Number of available threads you want to dedicate to model building
#' @param MaxMemory Amount of memory you want to dedicate to model building
#' @param ModelID Name for saving to file
#' @examples
#' \dontrun{
#'
#' # Create fake data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000L,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   AddComment = TRUE,
#'   ZIP = 2L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Create Model and Vectors
#' data <- AutoQuant::AutoWord2VecModeler(
#'   data,
#'   BuildType = "individual",
#'   stringCol = c("Comment"),
#'   KeepStringCol = FALSE,
#'   ModelID = "Model_1",
#'   model_path = getwd(),
#'   vects = 10,
#'   MinWords = 1,
#'   WindowSize = 1,
#'   Epochs = 25,
#'   SaveModel = "standard",
#'   Threads = max(1,parallel::detectCores()-2),
#'   MaxMemory = "28G")
#'
#' # Remove data
#' rm(data)
#'
#' # Create fake data for mock scoring
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000L,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   AddComment = TRUE,
#'   ZIP = 2L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Give h2o a few seconds
#' Sys.sleep(5L)
#'
#' # Create vectors for scoring
#' data <- AutoQuant::AutoWord2VecScoring(
#'   data,
#'   BuildType = 'individual',
#'   ModelObject = NULL,
#'   ModelID = "Model_1",
#'   model_path = getwd(),
#'   stringCol = "Comment",
#'   KeepStringCol = FALSE,
#'   H2OStartUp = TRUE,
#'   H2OShutdown = TRUE,
#'   Threads = max(1L, parallel::detectCores() - 2L),
#'   MaxMemory = "28G")
#'}
#' @export
AutoWord2VecModeler <- function(data,
                                BuildType     = "Combined",
                                stringCol     = c("Text_Col1","Text_Col2"),
                                KeepStringCol = FALSE,
                                model_path    = NULL,
                                vects         = 100,
                                MinWords      = 1,
                                WindowSize    = 12,
                                Epochs        = 25,
                                SaveModel     = "standard",
                                Threads       = max(1L, parallel::detectCores() - 2L),
                                MaxMemory     = "28G",
                                ModelID       = "Model_1") {

  # Arg check
  if(is.null(stringCol)) stop("stringCol cannot be NULL")
  if(length(stringCol) == 1L) BuildType <- "individual"

  # Ensure data is a data.table
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Two processes
  if(tolower(BuildType) == "combined") {

    # Create storage file----
    N <- length(stringCol)
    StoreFile <- data.table::data.table(ModelName = rep("a", 1), Path = rep("a", 1), Jar = rep("a", 1))
    i <- 0L
    for(string in stringCol) {

      # Increment
      i <- i + 1L

      # Convert to character
      if(!is.character(data[[eval(string)]])) data[, eval(string) := as.character(get(string))]

      # Build single column----
      if(i == 1L) {
        Final <- data[, list(get(string))]
        data.table::setnames(Final, "V1", "Text")
      } else {
        temp <- data[, list(get(string))]
        data.table::setnames(temp, "V1", "Text")
        Final <- data.table::rbindlist(list(Final, temp))
      }
    }

    # Remove Temp ----
    if(exists("temp")) rm(temp)

    # word2vec time ----
    localH2O <- h2o::h2o.init(nthreads = Threads, max_mem_size = MaxMemory, enable_assertions = FALSE)

    # It is important to remove "\n"----
    Final[, Text := gsub(pattern = "[[:punct:][:blank:]]", replacement = " ", x = Final[["Text"]])]
    Final <- Final[, list(Text)]

    # Tokenize----
    tokenized_words <- tokenizeH2O(Final)

    # Build model----
    w2v.model <- h2o::h2o.word2vec(
      tokenized_words,
      model_id = ModelID,
      word_model = "SkipGram",
      norm_model = "HSM",
      vec_size = vects,
      min_word_freq = MinWords,
      window_size = WindowSize,
      init_learning_rate = 0.025,
      sent_sample_rate = 0.05,
      epochs = Epochs)

    # Save model----
    if(!is.null(model_path)) {
      if(tolower(SaveModel) == "standard") {
        w2vPath <- h2o::h2o.saveModel(w2v.model, path = model_path, force = TRUE)
      } else {
        w2vPath <- h2o::h2o.saveMojo(w2v.model, path = model_path, force = TRUE)
        h2o::h2o.download_mojo(model = w2v.model, path = model_path, get_genmodel_jar = TRUE, genmodel_path = model_path, genmodel_name = model_id)
      }
    }

    # Loop through all the string columns and score them ----
    for(string in stringCol) {
      if(!is.character(data[[eval(string)]])) data[, eval(string) := as.character(get(string))]

      # word2vec time ----
      i <- i + 1L
      Sys.sleep(10L)
      h2o::h2o.init(nthreads = Threads, max_mem_size = MaxMemory)

      # It is important to remove "\n" ----
      data[, eval(string) := gsub(pattern = "[[:punct:][:blank:]]", replacement = " ", x = data[[eval(string)]])]
      data2 <- data[, list(get(string))]

      # Tokenize ----
      tokenized_words <- tokenizeH2O(data2)
      rm(data2)

      # Score model ----
      all_vecs <- h2o::h2o.transform(w2v.model, tokenized_words, aggregate_method = "AVERAGE")

      # Convert to data.table ----
      all_vecs <- data.table::as.data.table(all_vecs)
      data.table::setnames(all_vecs, names(all_vecs), paste0(string, "_", names(all_vecs)))
      data <- cbind(data, all_vecs)

      # Remove string cols ----
      if(!KeepStringCol) data[, eval(string) := NULL]
    }

    # Final Prep
    h2o::h2o.rm(w2v.model)
    h2o::h2o.shutdown(prompt = FALSE)

  } else {

    # Create storage file
    N <- length(stringCol)
    StoreFile <- data.table::data.table(ModelName = rep("a", N), Path = rep("a", N), Jar = rep("a", N))
    i <- 0L
    for(string in stringCol) {
      if(!is.character(data[[eval(string)]])) data[, eval(string) := as.character(get(string))]
      i <- i + 1L
      Sys.sleep(10L)
      h2o::h2o.init(nthreads = Threads, max_mem_size = MaxMemory)

      # It is important to remove "\n"
      data[, eval(string) := gsub(pattern = "[[:punct:][:blank:]]", replacement = " ", x = data[[eval(string)]])]
      data2 <- data[, list(get(string))]

      # Tokenize ----
      tokenized_words <- tokenizeH2O(data2)
      rm(data2)

      # Build model ----
      w2v.model <- h2o::h2o.word2vec(
        tokenized_words,
        model_id           = paste0(ModelID, "_", string),
        word_model         = "SkipGram",
        norm_model         = "HSM",
        vec_size           = vects,
        min_word_freq      = MinWords,
        window_size        = WindowSize,
        init_learning_rate = 0.025,
        sent_sample_rate   = 0.05,
        epochs             = Epochs)

      # Save model ----
      if(!is.null(model_path)) {
        if(tolower(SaveModel) == "standard") {
          w2vPath <- h2o::h2o.saveModel(w2v.model, path = model_path, force = TRUE)
        } else {
          w2vPath <- h2o::h2o.saveMojo(w2v.model, path = model_path, force = TRUE)
          h2o::h2o.download_mojo(model = w2v.model, path = model_path, get_genmodel_jar = TRUE, genmodel_path = model_path, genmodel_name = string)
        }
      }

      # Score model ----
      all_vecs <- h2o::h2o.transform(w2v.model, tokenized_words, aggregate_method = "AVERAGE")

      # Convert to data.table ----
      all_vecs <- data.table::as.data.table(all_vecs)
      data.table::setnames(all_vecs, names(all_vecs), paste0(string, "_", names(all_vecs)))
      data <- cbind(data, all_vecs)

      # Remove string cols ----
      if(!KeepStringCol) data[, eval(string) := NULL]

      # Final Prep ----
      h2o::h2o.rm(w2v.model)
      h2o::h2o.shutdown(prompt = FALSE)
    }
  }

  # Return data ----
  return(data)
}

#' @title AutoWord2VecScoring
#'
#' @description AutoWord2VecScoring is for scoring models generated by AutoWord2VecModeler()
#'
#' @family Feature Engineering
#' @author Adrian Antico
#'
#' @param data data.table
#' @param BuildType "individual" or "combined". Used to locate model in file
#' @param ModelObject NULL if you want it loaded in the function
#' @param ModelID Same as in training
#' @param model_path Location of model
#' @param stringCol Columns to transform
#' @param KeepStringCol FALSE to remove string col after creating vectors
#' @param H2OStartUp = TRUE,
#' @param Threads max(1L, parallel::detectCores() - 2L)
#' @param MaxMemory "28G"
#'
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000L,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   AddComment = TRUE,
#'   ZIP = 2L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Create Model and Vectors
#' data <- AutoQuant::AutoWord2VecModeler(
#'   data,
#'   BuildType = "individual",
#'   stringCol = c("Comment"),
#'   KeepStringCol = FALSE,
#'   ModelID = "Model_1",
#'   model_path = getwd(),
#'   vects = 10,
#'   MinWords = 1,
#'   WindowSize = 1,
#'   Epochs = 25,
#'   SaveModel = "standard",
#'   Threads = max(1,parallel::detectCores()-2),
#'   MaxMemory = "28G")
#'
#' # Remove data
#' rm(data)
#'
#' # Create fake data for mock scoring
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000L,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   AddComment = TRUE,
#'   ZIP = 2L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Create vectors for scoring
#' data <- AutoQuant::AutoWord2VecScoring(
#'   data,
#'   BuildType = "individual",
#'   ModelObject = NULL,
#'   ModelID = "Model_1",
#'   model_path = getwd(),
#'   stringCol = "Comment",
#'   KeepStringCol = FALSE,
#'   H2OStartUp = TRUE,
#'   H2OShutdown = TRUE,
#'   Threads = max(1L, parallel::detectCores() - 2L),
#'   MaxMemory = "28G")
#' }
#'
#' @export
AutoWord2VecScoring <- function(data,
                                BuildType = "individual",
                                ModelObject = NULL,
                                ModelID = "Model_1",
                                model_path = NULL,
                                stringCol = NULL,
                                KeepStringCol = FALSE,
                                H2OStartUp = TRUE,
                                H2OShutdown = TRUE,
                                Threads = max(1L, parallel::detectCores() - 2L),
                                MaxMemory = "28G") {

  # Check args
  if(is.null(stringCol)) stop("stringCol cannot be NULL")
  if(is.null(ModelObject) && tolower(BuildType) == "combined" && !file.exists(file.path(model_path, ModelID))) stop(paste0("Model not found at ", file.path(model_path, ModelID)))
  if(is.null(ModelObject) && tolower(BuildType) == "individual" && !file.exists(file.path(model_path, paste0(ModelID, "_", stringCol)))) stop(paste0("Model not found at ", file.path(model_path, paste0(ModelID, "_", stringCol))))

  # Instantiate H2O ----
  if(H2OStartUp) localH2O <- h2o::h2o.init(nthreads = Threads, max_mem_size = MaxMemory, enable_assertions = FALSE)

  # Build vecs
  if(tolower(BuildType) == "individual") {
    for(textCol in stringCol) {
      model <- h2o::h2o.loadModel(path = file.path(model_path, paste0(ModelID, "_", textCol)))
      data1 <- AutoH2OTextPrepScoring(data = data, string = textCol, NThreads = Threads, MaxMem = MaxMemory, StartH2O = FALSE)
      Scores <- data.table::as.data.table(h2o::h2o.transform(model, words = data1, aggregate_method = "AVERAGE"))
      data.table::setnames(Scores, names(Scores), paste0(textCol, "_", names(Scores)))
      if(!KeepStringCol) {
        data[, eval(textCol) := NULL]
        data <- cbind(data, Scores)
      } else {
        data <- cbind(data, Scores)
      }
    }
  } else {
    model <- h2o::h2o.loadModel(path = file.path(model_path, ModelID))
    for(textCol in stringCol) {
      data1 <- AutoH2OTextPrepScoring(data = data, string = textCol, NThreads = Threads, MaxMem = MaxMemory, StartH2O = FALSE)
      Scores <- data.table::as.data.table(h2o::h2o.transform(model, words = data1, aggregate_method = "AVERAGE"))
      data.table::setnames(Scores, names(Scores), paste0(textCol, "_", names(Scores)))
      if(!KeepStringCol) {
        data[, eval(textCol) := NULL]
        data <- cbind(data, Scores)
      } else {
        data <- cbind(data, Scores)
      }
    }
  }

  # Remove H2O Objects
  rm(localH2O, model)

  # H2O Shutdown
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Return
  return(data)
}

#' @title Word2Vec_H2O
#'
#' @description Word2Vec modeling and scoring. Features are automatically attached to source data sets
#'
#' @author Adrian Antico
#' @family Feature Engineering - Model Based
#'
#' @param TrainData. Source data
#' @param ValidationData. Source data
#' @param TestData. Source data
#' @param ScoringData. Source data
#' @param ArgsList ArgsList_FFE
#' @param RunMode 'train' or 'score'
#' @param SkipCols Colnames to skip over
#'
#' @noRd
Word2Vec_H2O <- function(TrainData. = NULL,
                         ValidationData. = NULL,
                         TestData. = NULL,
                         ScoringData. = NULL,
                         ArgsList = ArgsList,
                         RunMode = "train",
                         SkipCols = NULL) {

  # Set path if not provided
  if(length(ArgsList$model_path) == 0L) ArgsList$model_path <- getwd()
  if(length(ArgsList$Threads) == 0L) ArgsList$Threads <- parallel::detectCores()-2L
  if(length(ArgsList$MaxMemory) == 0L) ArgsList$MaxMemory <- {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")}

  # Run mode
  if(tolower(RunMode) == "train") {

    # MetaData
    Start <- Sys.time()
    tempnames <- names(TrainData.)

    # Run AutoWord2VecModeler
    TrainData. <- AutoQuant::AutoWord2VecModeler(
      data = TrainData.,
      BuildType = ArgsList$BuildType,
      stringCol = ArgsList$stringCol,
      KeepStringCol = TRUE,
      ModelID = ArgsList$ModelID,
      model_path = ArgsList$model_path,
      vects = ArgsList$vects,
      MinWords = ArgsList$MinWords,
      WindowSize = ArgsList$WindowSize,
      Epochs = ArgsList$Epochs,
      SaveModel = "standard",
      Threads = ArgsList$Threads,
      MaxMemory = ArgsList$MaxMemory)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$H2OWord2Vec_Training <- difftime(End, Start, units = "mins")

    # New column tracking
    ArgsList$H2OWord2Vec <- setdiff(names(data.table::copy(TrainData.)), tempnames)

    # Score new data
    if(!is.null(ValidationData.)) {
      ValidationData. <- AutoQuant::AutoWord2VecScoring(
        data = ValidationData.,
        BuildType = ArgsList$BuildType,
        stringCol = ArgsList$stringCol,
        KeepStringCol = TRUE,
        ModelID = ArgsList$ModelID,
        ModelObject = NULL,
        model_path = ArgsList$model_path,
        H2OStartUp = TRUE,
        H2OShutdown = TRUE,
        Threads = ArgsList$Threads,
        MaxMemory = ArgsList$MaxMemory)
    }

    # Score new data
    if(!is.null(ValidationData.)) {
      TestData. <- AutoQuant::AutoWord2VecScoring(
        data = TestData.,
        BuildType = ArgsList$BuildType,
        stringCol = ArgsList$stringCol,
        KeepStringCol = ArgsList$KeepStringCol,
        ModelID = ArgsList$ModelID,
        ModelObject = NULL,
        model_path = ArgsList$model_path,
        H2OStartUp = TRUE,
        H2OShutdown = TRUE,
        Threads = ArgsList$Threads,
        MaxMemory = ArgsList$MaxMemory)
    }

    # Return
    return(list(TrainData = TrainData., ValidationData = ValidationData., TestData = TestData., ArgsList = ArgsList))


  } else {

    # MetaData
    Start <- Sys.time()
    tempnames <- names(ScoringData.)

    # Score new data
    data <- AutoQuant::AutoWord2VecScoring(
      data = ScoringData.,
      BuildType = ArgsList$FE_H2OWord2VecScoring$BuildType,
      stringCol = ArgsList$FE_H2OWord2VecScoring$stringCol,
      KeepStringCol = ArgsList$FE_H2OWord2VecScoring$KeepStringCol,
      ModelID = ArgsList$FE_H2OWord2VecScoring$ModelID,
      ModelObject = NULL,
      model_path = ArgsList$FE_H2OWord2VecScoring$model_path,
      H2OStartUp = TRUE,
      H2OShutdown = TRUE,
      Threads = ArgsList$FE_H2OWord2VecScoring$Threads,
      MaxMemory = ArgsList$FE_H2OWord2VecScoring$MaxMemory)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$H2OWord2Vec_Scoring <- difftime(End, Start, units = "mins")
  }

  # Return
  return(list(ScoringData = ScoringData., ArgsList = ArgsList))
}

#' @title H2OAutoencoder
#'
#' @description H2OAutoencoder for anomaly detection and or dimensionality reduction
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param AnomalyDetection Set to TRUE to run anomaly detection
#' @param DimensionReduction Set to TRUE to run dimension reduction
#' @param data The data.table with the columns you wish to have analyzed
#' @param Features NULL Column numbers or column names
#' @param RemoveFeatures Set to TRUE if you want the features you specify in the Features argument to be removed from the data returned
#' @param NThreads max(1L, parallel::detectCores()-2L)
#' @param MaxMem "28G"
#' @param LayerStructure If NULL, layers and sizes will be created for you, using NodeShrinkRate and 7 layers will be created.
#' @param NodeShrinkRate = (sqrt(5) - 1) / 2,
#' @param ReturnFactorCount Default is NULL. If you supply a number, the final layer will be that number. Otherwise, it will be based on the NodeShrinkRate math.
#' @param H2OStart TRUE to start H2O inside the function
#' @param H2OShutdown Setting to TRUE will shutdown H2O when it done being used internally.
#' @param ModelID "TestModel"
#' @param model_path If NULL no model will be saved. If a valid path is supplied the model will be saved there
#' @param ReturnLayer Which layer of the NNet to return. Choose from 1-7 with 4 being the layer with the least amount of nodes
#' @param per_feature Set to TRUE to have per feature anomaly detection generated. Otherwise and overall value will be generated
#' @param Activation Choose from "Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout","Maxout", "MaxoutWithDropout"
#' @param Epochs Quantile value to find the cutoff value for classifying outliers
#' @param L2 Specify the amount of memory to allocate to H2O. E.g. "28G"
#' @param ElasticAveraging Specify the number of threads (E.g. cores * 2)
#' @param ElasticAveragingMovingRate Specify the number of decision trees to build
#' @param ElasticAveragingRegularization Specify the row sample rate per tree
#' @examples
#' \dontrun{
#' ############################
#' # Training
#' ############################
#'
#' # Create simulated data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000L,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   AddComment = FALSE,
#'   ZIP = 2L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run algo
#' Output <- AutoQuant::H2OAutoencoder(
#'
#'   # Select the service
#'   AnomalyDetection = TRUE,
#'   DimensionReduction = TRUE,
#'
#'   # Data related args
#'   data = data,
#'   Features = names(data)[2L:(ncol(data)-1L)],
#'   per_feature = FALSE,
#'   RemoveFeatures = FALSE,
#'   ModelID = "TestModel",
#'   model_path = getwd(),
#'
#'   # H2O Environment
#'   NThreads = max(1L, parallel::detectCores()-2L),
#'   MaxMem = "28G",
#'   H2OStart = TRUE,
#'   H2OShutdown = TRUE,
#'
#'   # H2O ML Args
#'   LayerStructure = NULL,
#'   NodeShrinkRate = (sqrt(5) - 1) / 2,
#'   ReturnLayer = 4L,
#'   ReturnFactorCount = NULL,
#'   Activation = "Tanh",
#'   Epochs = 5L,
#'   L2 = 0.10,
#'   ElasticAveraging = TRUE,
#'   ElasticAveragingMovingRate = 0.90,
#'   ElasticAveragingRegularization = 0.001)
#'
#' # Inspect output
#' data <- Output$Data
#' Model <- Output$Model
#'
#' # If ValidationData is not null
#' ValidationData <- Output$ValidationData
#'
#' ############################
#' # Scoring
#' ############################
#'
#' # Create simulated data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000L,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   AddComment = FALSE,
#'   ZIP = 2L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run algo
#' data <- AutoQuant::H2OAutoencoderScoring(
#'
#'   # Select the service
#'   AnomalyDetection = TRUE,
#'   DimensionReduction = TRUE,
#'
#'   # Data related args
#'   data = data,
#'   Features = names(data)[2L:ncol(data)],
#'   RemoveFeatures = TRUE,
#'   per_feature = FALSE,
#'   ModelObject = NULL,
#'   ModelID = "TestModel",
#'   model_path = getwd(),
#'
#'   # H2O args
#'   NThreads = max(1L, parallel::detectCores()-2L),
#'   MaxMem = "28G",
#'   H2OStart = TRUE,
#'   H2OShutdown = TRUE,
#'   ReturnLayer = 4L)
#' }
#' @return A data.table
#' @export
H2OAutoencoder <- function(AnomalyDetection = FALSE,
                           DimensionReduction = TRUE,
                           data,
                           Features = NULL,
                           RemoveFeatures = FALSE,
                           NThreads = max(1L, parallel::detectCores()-2L),
                           MaxMem = "28G",
                           H2OStart = TRUE,
                           H2OShutdown = TRUE,
                           ModelID = "TestModel",
                           model_path = NULL,
                           LayerStructure  = NULL,
                           NodeShrinkRate = (sqrt(5) - 1) / 2,
                           ReturnLayer = 4L,
                           ReturnFactorCount = NULL,
                           per_feature = TRUE,
                           Activation = "Tanh",
                           Epochs = 5L,
                           L2 = 0.10,
                           ElasticAveraging = TRUE,
                           ElasticAveragingMovingRate = 0.90,
                           ElasticAveragingRegularization = 0.001) {

  print('AE 1')

  # Ensure data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  print('AE 2')

  # Return because of mispecified arguments----
  if(!AnomalyDetection && !DimensionReduction) stop("Why are you running this function if you do not want anomaly detection nor dimension reduction?")

  print('AE 3')

  # Constants ----
  F_Length <- length(Features)
  if(is.numeric(Features) || is.integer(Features)) Features <- names(data)[Features]

  print('AE 4')

  # Ensure categoricals are set as factors ----
  temp <- data[, .SD, .SDcols = c(Features)]
  data[, (Features) := NULL]
  temp <- ModelDataPrep(data=temp, Impute=FALSE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=FALSE, DateToChar=FALSE, RemoveDates=TRUE, MissFactor="0", MissNum=-1, IgnoreCols=NULL)

  print('AE 5')

  # Initialize H2O----
  if(H2OStart) LocalHost <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
  print('AE 6')
  print(names(temp))
  print(str(temp))
  H2O_Data <- h2o::as.h2o(temp)
  print('AE 7')

  # Layer selection - Eventually put in an arg for Type for some alternative pre-set LayerStructure----
  if(is.null(LayerStructure)) LayerStructure <- c(F_Length, ceiling(F_Length * NodeShrinkRate), ceiling(F_Length * NodeShrinkRate ^ 2), ceiling(F_Length * NodeShrinkRate ^ 3), ceiling(F_Length * NodeShrinkRate ^ 2), ceiling(F_Length * NodeShrinkRate), F_Length)

  print('AE 8')

  # Update Features ----
  Features <- Features[Features %chin% names(H2O_Data)]

  print('AE 9')

  # Build Model ----
  Model <- h2o::h2o.deeplearning(
    autoencoder = TRUE,
    model_id = ModelID,
    training_frame = H2O_Data,
    x = Features,
    l2 = L2,
    epochs = Epochs,
    hidden = LayerStructure,
    activation = Activation,
    elastic_averaging = ElasticAveraging,
    elastic_averaging_moving_rate = ElasticAveragingMovingRate,
    elastic_averaging_regularization = ElasticAveragingRegularization)

  print('AE 10')

  # Save Model
  if(!is.null(model_path)) SaveModel <- h2o::h2o.saveModel(object = Model, path = model_path, force = TRUE)

  print('AE 11')

  # Create and Merge features----
  if(AnomalyDetection && DimensionReduction) {
    print('AE 12.1')
    Data <- cbind(
      data,
      temp,
      data.table::as.data.table(h2o::h2o.deepfeatures(object = Model, data = H2O_Data, layer = ReturnLayer)),
      data.table::as.data.table(h2o::h2o.anomaly(object = Model, data = H2O_Data, per_feature = per_feature)))
  } else if(!AnomalyDetection && DimensionReduction) {
    print('AE 12.2')
    Data <- cbind(
      data,
      temp,
      data.table::as.data.table(h2o::h2o.deepfeatures(object = Model, data = H2O_Data, layer = ReturnLayer)))
  } else if(AnomalyDetection && !DimensionReduction) {
    print('AE 12.3')
    Data <- cbind(
      data,
      temp,
      data.table::as.data.table(h2o::h2o.anomaly(object = Model, data = H2O_Data, per_feature = per_feature)))
  }

  # Remove H2O Objects ----
  print('AE 13')
  h2o::h2o.rm(Model, H2O_Data)

  # Shutdown h2o ----
  print('AE 14')
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Remove features ----
  print('AE 15')
  if(RemoveFeatures) data.table::set(Data, j = Features, value = NULL)

  # Return output ----
  print('AE 16')
  return(Data)
}

#' @title H2OAutoencoderScoring
#'
#' @description H2OAutoencoderScoring for anomaly detection and or dimensionality reduction
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param AnomalyDetection Set to TRUE to run anomaly detection
#' @param DimensionReduction Set to TRUE to run dimension reduction
#' @param data The data.table with the columns you wish to have analyzed
#' @param Features NULL Column numbers or column names
#' @param RemoveFeatures Set to TRUE if you want the features you specify in the Features argument to be removed from the data returned
#' @param ModelObject If NULL then the model will be loaded from file. Otherwise, it will use what is supplied
#' @param NThreads max(1L, parallel::detectCores()-2L)
#' @param MaxMem "28G"
#' @param H2OStart TRUE to start H2O inside the function
#' @param H2OShutdown Setting to TRUE will shutdown H2O when it done being used internally.
#' @param ModelID "TestModel"
#' @param model_path If NULL no model will be saved. If a valid path is supplied the model will be saved there
#' @param ReturnLayer Which layer of the NNet to return. Choose from 1-7 with 4 being the layer with the least amount of nodes
#' @param per_feature Set to TRUE to have per feature anomaly detection generated. Otherwise and overall value will be generated
#' @examples
#' \dontrun{
#' ############################
#' # Training
#' ############################
#'
#' # Create simulated data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000L,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   AddComment = FALSE,
#'   ZIP = 2L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run algo
#' data <- AutoQuant::H2OAutoencoder(
#'
#'   # Select the service
#'   AnomalyDetection = TRUE,
#'   DimensionReduction = TRUE,
#'
#'   # Data related args
#'   data = data,
#'   ValidationData = NULL,
#'   Features = names(data)[2L:(ncol(data)-1L)],
#'   per_feature = FALSE,
#'   RemoveFeatures = TRUE,
#'   ModelID = "TestModel",
#'   model_path = getwd(),
#'
#'   # H2O Environment
#'   NThreads = max(1L, parallel::detectCores()-2L),
#'   MaxMem = "28G",
#'   H2OStart = TRUE,
#'   H2OShutdown = TRUE,
#'
#'   # H2O ML Args
#'   LayerStructure = NULL,
#'   ReturnLayer = 4L,
#'   Activation = "Tanh",
#'   Epochs = 5L,
#'   L2 = 0.10,
#'   ElasticAveraging = TRUE,
#'   ElasticAveragingMovingRate = 0.90,
#'   ElasticAveragingRegularization = 0.001)
#'
#' ############################
#' # Scoring
#' ############################
#'
#' # Create simulated data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000L,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   AddComment = FALSE,
#'   ZIP = 2L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run algo
#' data <- AutoQuant::H2OAutoencoderScoring(
#'
#'   # Select the service
#'   AnomalyDetection = TRUE,
#'   DimensionReduction = TRUE,
#'
#'   # Data related args
#'   data = data,
#'   Features = names(data)[2L:ncol(data)],
#'   RemoveFeatures = TRUE,
#'   per_feature = FALSE,
#'   ModelObject = NULL,
#'   ModelID = "TestModel",
#'   model_path = getwd(),
#'
#'   # H2O args
#'   NThreads = max(1L, parallel::detectCores()-2L),
#'   MaxMem = "28G",
#'   H2OStart = TRUE,
#'   H2OShutdown = TRUE,
#'   ReturnLayer = 4L)
#' }
#' @return A data.table
#' @export
H2OAutoencoderScoring <- function(data,
                                  Features = NULL,
                                  RemoveFeatures = FALSE,
                                  ModelObject = NULL,
                                  AnomalyDetection = TRUE,
                                  DimensionReduction = TRUE,
                                  ReturnLayer = 4L,
                                  per_feature = TRUE,
                                  NThreads = max(1L, parallel::detectCores()-2L),
                                  MaxMem = "28G",
                                  H2OStart = TRUE,
                                  H2OShutdown = TRUE,
                                  ModelID = "TestModel",
                                  model_path = NULL) {

  print('AES 1')

  # Ensure data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Return because of mispecified arguments ----
  if(!AnomalyDetection && !DimensionReduction) stop("At least one of AnomalyDetection or DimensionReduction must be set to TRUE")

  print('AES 2')

  # Constants ----
  if(is.numeric(Features) || is.integer(Features)) Features <- names(data)[Features]

  print('AES 3')

  # Ensure categoricals are set as factors ----
  temp <- data[, .SD, .SDcols = c(Features)]
  data[, (Features) := NULL]
  temp <- ModelDataPrep(data=temp, Impute=FALSE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=FALSE, DateToChar=FALSE, RemoveDates=TRUE, MissFactor="0", MissNum=-1, IgnoreCols=NULL)

  print('AES 4')

  # Initialize H2O ----
  if(H2OStart) h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
  print('AES 5')
  H2O_Data <- h2o::as.h2o(temp)
  print('AES 6')
  if(is.null(ModelObject)) ModelObject <- h2o::h2o.loadModel(path = file.path(model_path, ModelID))
  print('AES 7')

  # Create and Merge features ----
  if(AnomalyDetection && DimensionReduction) {
    print('AES 8.1')
    Data <- cbind(
      data,
      temp,
      data.table::as.data.table(h2o::h2o.deepfeatures(object = ModelObject, data = H2O_Data, layer = ReturnLayer)),
      data.table::as.data.table(h2o::h2o.anomaly(object = ModelObject, data = H2O_Data, per_feature = per_feature)))
  } else if(!AnomalyDetection && DimensionReduction) {
    print('AES 8.2')
    Data <- cbind(
      data,
      temp,
      data.table::as.data.table(h2o::h2o.deepfeatures(object = ModelObject, data = H2O_Data, layer = ReturnLayer)))
  } else if(AnomalyDetection && !DimensionReduction) {
    print('AES 8.3')
    Data <- cbind(
      data,
      temp,
      data.table::as.data.table(h2o::h2o.anomaly(object = ModelObject, data = H2O_Data, per_feature = per_feature)))
  }

  print('AES 9')

  # Remove H2O Objects ----
  h2o::h2o.rm(ModelObject, H2O_Data)

  print('AES 10')

  # Shutdown h2o ----
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  print('AES 11')

  # Remove features ----
  if(RemoveFeatures) data.table::set(Data, j = Features, value = NULL)

  print('AES 12')

  # Return output ----
  return(Data)
}

#' @title AutoEncoder_H2O
#'
#' @description Utilize an H2O autoencoder to provide dimensionality reduction and anomaly detection
#'
#' @author Adrian Antico
#' @family Feature Engineering - Model Based
#'
#' @param RunMode 'train' will run in train mode. Supply any other character to run scoring mode. Must supply a character
#' @param ArgsList ArgsList_FEE
#' @param TrainData. data
#' @param ValidationData. data
#' @param TestData. data
#' @param ScoringData. data
#' @param Pause 0L Sys.sleep(Pause)
#'
#' @noRd
AutoEncoder_H2O <- function(RunMode = 'train',
                            ArgsList = NULL,
                            TrainData. = NULL,
                            ValidationData. = NULL,
                            TestData. = NULL,
                            ScoringData. = NULL,
                            Pause = 0L) {

  print('AE ::: 1')

  # Give h2o some sleep time
  if(Pause > 0L) Sys.sleep(Pause)
  if(length(ArgsList$ModelID) == 0L) {
    ArgsList$ModelID <- 'temp1'
  }
  if(length(ArgsList$model_path) == 0L) {
    ArgsList$model_path <- getwd()
  }

  # Run Mode
  if(tolower(RunMode) == "train") {

    print('AE ::: 2')

    # Metadata
    Start <- Sys.time()
    tempnames <- names(data.table::copy(TrainData.))

    # Run function
    TrainData. <- AutoQuant::H2OAutoencoder(

      # Select the service
      AnomalyDetection = ArgsList$AnomalyDetection,
      DimensionReduction = ArgsList$DimensionReduction,

      # Data related args
      data = TrainData.,
      Features = ArgsList$Features,
      per_feature = ArgsList$per_feature,
      RemoveFeatures = ArgsList$RemoveFeatures,
      ModelID = ArgsList$ModelID,
      model_path = ArgsList$Models_Path,

      # H2O Environment
      NThreads = ArgsList$NThreads,
      MaxMem = ArgsList$MaxMem,
      H2OStart = TRUE,
      H2OShutdown = TRUE,

      # H2O ML Args
      LayerStructure = ArgsList$LayerStructure,
      NodeShrinkRate = ArgsList$NodeShrinkRate,
      ReturnLayer = ArgsList$ReturnLayer,
      Activation = "Tanh",
      Epochs = ArgsList$Epochs,
      L2 = ArgsList$L2,
      ElasticAveraging = ArgsList$ElasticAveraging,
      ElasticAveragingMovingRate = ArgsList$ElasticAveragingMovingRate,
      ElasticAveragingRegularization = ArgsList$ElasticAveragingRegularization)

    print('AE ::: 3')

    # New columns tracking
    ArgsList$NewColumns <- setdiff(names(data.table::copy(TrainData.)), tempnames)

    print('AE ::: 4')

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime_Training <- difftime(End, Start, units = "mins")

    print('AE ::: 5')

    # Score validation data
    if(!is.null(ValidationData.)) {

      print('AE ::: 6')

      # Pause
      Sys.sleep(8L)

      # Score model
      ValidationData. <- AutoQuant::H2OAutoencoderScoring(

        # Select the service
        AnomalyDetection = ArgsList$AnomalyDetection,
        DimensionReduction = ArgsList$DimensionReduction,

        # Data related args
        data = ValidationData.,
        Features = ArgsList$Features,
        per_feature = ArgsList$per_feature,
        RemoveFeatures = ArgsList$RemoveFeatures,
        ModelObject = NULL,
        ModelID = ArgsList$ModelID,
        model_path = ArgsList$model_path,

        # H2O args
        NThreads = ArgsList$NThreads,
        MaxMem = ArgsList$MaxMem,
        H2OStart = TRUE,
        H2OShutdown = TRUE,
        ReturnLayer = ArgsList$ReturnLayer)
    }

    # Score Test Data
    if(!is.null(TestData.)) {

      print('AE ::: 7')

      # Pause
      Sys.sleep(8L)

      # Score model
      TestData. <- AutoQuant::H2OAutoencoderScoring(

        # Select the service
        AnomalyDetection = ArgsList$AnomalyDetection,
        DimensionReduction = ArgsList$DimensionReduction,

        # Data related args
        data = TestData.,
        Features = ArgsList$Features,
        per_feature = ArgsList$per_feature,
        RemoveFeatures = ArgsList$RemoveFeatures,
        ModelObject = NULL,
        ModelID = ArgsList$ModelID,
        model_path = ArgsList$model_path,

        # H2O args
        NThreads = ArgsList$NThreads,
        MaxMem = ArgsList$MaxMem,
        H2OStart = TRUE,
        H2OShutdown = TRUE,
        ReturnLayer = ArgsList$ReturnLayer)
    }

  } else {

    print('AE ::: 10')

    # Score model
    ScoringData. <- AutoQuant::H2OAutoencoderScoring(

      # Select the service
      AnomalyDetection = ArgsList$AnomalyDetection,
      DimensionReduction = ArgsList$DimensionReduction,

      # Data related args
      data = ScoringData.,
      Features = ArgsList$Features,
      per_feature = ArgsList$per_feature,
      RemoveFeatures = ArgsList$RemoveFeatures,
      ModelObject = NULL,
      ModelID = ArgsList$ModelID,
      model_path = ArgsList$model_path,

      # H2O args
      NThreads = ArgsList$NThreads,
      MaxMem = ArgsList$MaxMem,
      H2OStart = TRUE,
      H2OShutdown = TRUE,
      ReturnLayer = 4L)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$H2OAutoEncoder_Scoring <- difftime(End, Start, units = "mins")
  }

  print('AE ::: 8')

  # Return
  return(list(TrainData = TrainData., ValidationData = ValidationData., TestData = TestData., ScoringData = ScoringData., ArgsList = ArgsList))
}

#' @title H2OIsolationForest
#'
#' @description H2OIsolationForestScoring for dimensionality reduction and / or anomaly detection
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#'
#' @param data The data.table with the columns you wish to have analyzed
#' @param Features A character vector with the column names to utilize in the isolation forest
#' @param IDcols A character vector with the column names to not utilize in the isolation forest but have returned with the data output. Otherwise those columns will be removed
#' @param ModelID Name for model that gets saved to file if SavePath is supplied and valid
#' @param SavePath Path directory to store saved model
#' @param Threshold Quantile value to find the cutoff value for classifying outliers
#' @param MaxMem Specify the amount of memory to allocate to H2O. E.g. "28G"
#' @param NThreads Specify the number of threads (E.g. cores * 2)
#' @param NTrees Specify the number of decision trees to build
#' @param MaxDepth Max tree depth
#' @param MinRows Minimum number of rows allowed per leaf
#' @param RowSampleRate Number of rows to sample per tree
#' @param ColSampleRate Sample rate for each split
#' @param ColSampleRatePerLevel Sample rate for each level
#' @param ColSampleRatePerTree Sample rate per tree
#' @param CategoricalEncoding Choose from "AUTO", "Enum", "OneHotInternal", "OneHotExplicit", "Binary", "Eigen", "LabelEncoder", "SortByResponse", "EnumLimited"
#' @param Debug Debugging
#' @examples
#' \dontrun{
#' # Create simulated data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 50000,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   ZIP = 0L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run algo
#' data <- AutoQuant::H2OIsolationForest(
#'   data,
#'   Features = names(data)[2L:ncol(data)],
#'   IDcols = c("Adrian", "IDcol_1", "IDcol_2"),
#'   ModelID = "Adrian",
#'   SavePath = getwd(),
#'   Threshold = 0.95,
#'   MaxMem = "28G",
#'   NThreads = -1,
#'   NTrees = 100,
#'   MaxDepth = 8,
#'   MinRows = 1,
#'   RowSampleRate = (sqrt(5)-1)/2,
#'   ColSampleRate = 1,
#'   ColSampleRatePerLevel = 1,
#'   ColSampleRatePerTree = 1,
#'   CategoricalEncoding = c("AUTO"),
#'   Debug = TRUE)
#'
#' # Remove output from data and then score
#' data[, eval(names(data)[17:ncol(data)]) := NULL]
#'
#' # Run algo
#' Outliers <- AutoQuant::H2OIsolationForestScoring(
#'   data,
#'   Features = names(data)[2:ncol(data)],
#'   IDcols = c("Adrian", "IDcol_1", "IDcol_2"),
#'   H2OStart = TRUE,
#'   H2OShutdown = TRUE,
#'   ModelID = "TestModel",
#'   SavePath = getwd(),
#'   Threshold = 0.95,
#'   MaxMem = "28G",
#'   NThreads = -1,
#'   Debug = FALSE)
#' }
#' @return Source data.table with predictions. Note that any columns not listed in Features nor IDcols will not be returned with data. If you want columns returned but not modeled, supply them as IDcols
#' @export
H2OIsolationForest <- function(data,
                               Features = NULL,
                               IDcols = NULL,
                               ModelID = "TestModel",
                               SavePath = NULL,
                               Threshold = 0.975,
                               MaxMem = "28G",
                               NThreads = -1,
                               NTrees = 100,
                               MaxDepth = 8,
                               MinRows = 1,
                               RowSampleRate = (sqrt(5)-1)/2,
                               ColSampleRate = 1,
                               ColSampleRatePerLevel = 1,
                               ColSampleRatePerTree = 1,
                               CategoricalEncoding = c("AUTO"),
                               Debug = FALSE) {

  # Arg checks ----
  if(!is.null(SavePath) && !dir.exists(SavePath)) stop("SavePath is not a valid directory")
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(IDcols) && !is.character(IDcols)) stop("IDcols needs to be a character scalar or vector")
  if(!is.null(ModelID) && !is.character(ModelID)) stop("ModelID needs to be a character scalar or vector")
  if(!is.null(Features) && !is.character(ModelID)) stop("Features needs to be a character scalar or vector")
  if(!is.null(SavePath) && !is.character(SavePath)) stop("SavePath needs to be a character scalar or vector")
  if(!is.null(SavePath) && is.character(SavePath) && !dir.exists(SavePath)) warning("SavePath directory did not exist but one was made")

  # Get date col names if exist ----
  ID <- IDcols
  for(i in seq_len(length(names(data)))) {
    if(any(class(data[[i]]) %in% c("Date","POSIXct","IDate","IDateTime"))) {
      Features <- Features[!Features %in% names(data)[i]]
      ID <- c(ID, names(data)[i])
    }
    if(names(data)[i] %chin% ID) {
      Features <- Features[!Features %in% names(data)[i]]
    }
  }

  # Unique ----
  Features <- unique(Features)
  ID <- unique(ID)

  # Subset ID ----
  if(!is.null(ID) && (length(ID) + length(Features) == length(names(data)))) {
    IDcolData <- data[, .SD, .SDcols = c(ID)]
    data[, (ID) := NULL]
  } else if(!is.null(ID) && (length(ID) + length(Features) != length(names(data)))) {
    ID <- c(ID, setdiff(names(data), c(Features, ID)))
    IDcolData <- data[, .SD, .SDcols = c(ID)]
    data[, (ID) := NULL]
  }

  # Ensure Characters are Converted to Factors ----
  data <- ModelDataPrep(
    data = data,
    Impute = TRUE,
    CharToFactor = TRUE,
    FactorToChar = FALSE,
    IntToNumeric = TRUE,
    LogicalToBinary = TRUE,
    DateToChar = FALSE,
    IDateConversion = FALSE,
    RemoveDates = FALSE,
    MissFactor = "0",
    MissNum = -1,
    IgnoreCols = NULL)

  # Debug
  if(Debug) print(str(data))
  if(Debug) print(str(IDcolData))
  if(Debug) print(Features)

  # Initialize H2O ----
  localH2O <- h2o::h2o.init(max_mem_size = MaxMem, nthreads = NThreads, enable_assertions = FALSE)

  # Convert data to H2O Frame ----
  Data <- h2o::as.h2o(data)

  # Build Isolation Forest ----
  IsolationForest <- h2o::h2o.isolationForest(
    training_frame = Data,
    x = Features,
    model_id = ModelID,
    ntrees = NTrees,
    sample_rate = RowSampleRate,
    max_depth = MaxDepth,
    min_rows = MinRows,
    stopping_rounds = 0,
    stopping_metric = "AUTO",
    col_sample_rate_change_per_level = ColSampleRatePerLevel,
    col_sample_rate_per_tree = ColSampleRatePerTree,
    categorical_encoding = CategoricalEncoding)

  # Generate Outliers data.table ----
  OutliersRaw <- data.table::as.data.table(h2o::h2o.predict(object = IsolationForest, newdata = Data))

  # Save model ----
  if(!is.null(SavePath)) SaveModel <- h2o::h2o.saveModel(object = IsolationForest, path = SavePath, force = TRUE)

  # Shutdown H2O ----
  h2o::h2o.shutdown(prompt = FALSE)

  # Add column for outlier indicator ----
  data.table::setnames(OutliersRaw, c("predict", "mean_length"), c("PredictIsoForest", "MeanLength"))
  Cutoff <- quantile(OutliersRaw[["PredictIsoForest"]], probs = Threshold)[[1L]]
  OutliersRaw[, PredictedOutlier := data.table::fifelse(PredictIsoForest > eval(Cutoff), 1, 0)]
  OutliersRaw[, Rank := data.table::frank(PredictIsoForest) / .N]
  data.table::setcolorder(OutliersRaw, c(4L, 3L, 1L, 2L))

  # Merge back with source data ----
  data <- cbind(data, OutliersRaw)

  # Merge data back with IDcolData ----
  if(exists("IDcolData")) data <- cbind(IDcolData, data)

  # Return data ----
  return(data)
}

#' @title H2OIsolationForestScoring
#'
#' @description H2OIsolationForestScoring for dimensionality reduction and / or anomaly detection scoring on new data
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#'
#' @param data The data.table with the columns you wish to have analyzed
#' @param Features A character vector with the column names to utilize in the isolation forest
#' @param IDcols A character vector with the column names to not utilize in the isolation forest but have returned with the data output. Otherwise those columns will be removed
#' @param H2OStart TRUE to have H2O started inside function
#' @param H2OShutdown TRUE to shutdown H2O inside function
#' @param ModelID Name for model that gets saved to file if SavePath is supplied and valid
#' @param SavePath Path directory to store saved model
#' @param Threshold Quantile value to find the cutoff value for classifying outliers
#' @param MaxMem Specify the amount of memory to allocate to H2O. E.g. "28G"
#' @param NThreads Specify the number of threads (E.g. cores * 2)
#' @param Debug Debugging
#' @examples
#' \dontrun{
#' # Create simulated data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 50000,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   ZIP = 0L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run algo
#' data <- AutoQuant::H2OIsolationForest(
#'   data,
#'   Features = names(data)[2L:ncol(data)],
#'   IDcols = c("Adrian", "IDcol_1", "IDcol_2"),
#'   ModelID = "Adrian",
#'   SavePath = getwd(),
#'   Threshold = 0.95,
#'   MaxMem = "28G",
#'   NThreads = -1,
#'   NTrees = 100,
#'   SampleRate = (sqrt(5)-1)/2,
#'   MaxDepth = 8,
#'   MinRows = 1,
#'   ColSampleRate = 1,
#'   ColSampleRatePerLevel = 1,
#'   ColSampleRatePerTree = 1,
#'   CategoricalEncoding = c("AUTO"),
#'   Debug = TRUE)
#'
#' # Remove output from data and then score
#' data[, eval(names(data)[17:ncol(data)]) := NULL]
#'
#' # Run algo
#' Outliers <- AutoQuant::H2OIsolationForestScoring(
#'   data,
#'   Features = names(data)[2:ncol(data)],
#'   IDcols = c("Adrian", "IDcol_1", "IDcol_2"),
#'   H2OStart = TRUE,
#'   H2OShutdown = TRUE,
#'   ModelID = "TestModel",
#'   SavePath = getwd(),
#'   Threshold = 0.95,
#'   MaxMem = "28G",
#'   NThreads = -1,
#'   Debug = FALSE)
#' }
#' @return Source data.table with predictions. Note that any columns not listed in Features nor IDcols will not be returned with data. If you want columns returned but not modeled, supply them as IDcols
#' @export
H2OIsolationForestScoring <- function(data,
                                      Features = NULL,
                                      IDcols = NULL,
                                      H2OStart = TRUE,
                                      H2OShutdown = TRUE,
                                      ModelID = "TestModel",
                                      SavePath = NULL,
                                      Threshold = 0.975,
                                      MaxMem = "28G",
                                      NThreads = -1,
                                      Debug = FALSE) {

  # Arg checks ----
  if(!is.null(SavePath) && !dir.exists(SavePath)) stop("SavePath is not a valid directory")
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!is.null(IDcols) && !is.character(IDcols)) stop("IDcols needs to be a character scalar or vector")
  if(!is.null(ModelID) && !is.character(ModelID)) stop("ModelID needs to be a character scalar or vector")
  if(!is.null(Features) && !is.character(ModelID)) stop("Features needs to be a character scalar or vector")
  if(!is.null(SavePath) && !is.character(SavePath)) stop("SavePath needs to be a character scalar or vector")
  if(!is.null(SavePath) && is.character(SavePath) && !dir.exists(SavePath)) warning("SavePath directory did not exist but one was made")

  # Get date col names if exist ----
  ID <- IDcols
  for(i in seq_len(length(names(data)))) {
    if(any(class(data[[i]]) %in% c("Date","POSIXct","IDate","IDateTime"))) {
      Features <- Features[!Features %in% names(data)[i]]
      ID <- c(ID, names(data)[i])
    }
    if(names(data)[i] %chin% ID) {
      Features <- Features[!Features %in% names(data)[i]]
    }
  }

  # Unique ----
  Features <- unique(Features)
  ID <- unique(ID)

  # Subset ID ----
  if(!is.null(ID) && (length(ID) + length(Features) == length(names(data)))) {
    IDcolData <- data[, .SD, .SDcols = c(ID)]
    data[, (ID) := NULL]
  } else if(!is.null(ID) && (length(ID) + length(Features) != length(names(data)))) {
    ID <- c(ID, setdiff(names(data), c(Features, ID)))
    IDcolData <- data[, .SD, .SDcols = c(ID)]
    data[, (ID) := NULL]
  }

  # Ensure Characters are Converted to Factors ----
  data <- ModelDataPrep(
    data = data,
    Impute = TRUE,
    CharToFactor = TRUE,
    FactorToChar = FALSE,
    IntToNumeric = TRUE,
    LogicalToBinary = TRUE,
    DateToChar = FALSE,
    IDateConversion = FALSE,
    RemoveDates = FALSE,
    MissFactor = "0",
    MissNum = -1,
    IgnoreCols = NULL)

  # Debug
  if(Debug) print(str(data))
  if(Debug) print(str(IDcolData))
  if(Debug) print(Features)

  # Prepare H2O ----
  if(H2OStart) localH2O <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem, enable_assertions = FALSE)
  H2O_Data <- h2o::as.h2o(data)
  ModelObject <- h2o::h2o.loadModel(path = file.path(SavePath, ModelID))

  # Generate Outliers data.table ----
  OutliersRaw <- data.table::as.data.table(h2o::h2o.predict(object = ModelObject, newdata = H2O_Data))
  rm(H2O_Data, ModelObject)

  # Shutdown h2o----
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Add column for outlier indicator ----
  data.table::setnames(OutliersRaw, c("predict", "mean_length"), c("PredictIsoForest", "MeanLength"))
  Cutoff <- quantile(OutliersRaw[["PredictIsoForest"]], probs = Threshold)[[1L]]
  OutliersRaw[, PredictedOutlier := data.table::fifelse(PredictIsoForest > eval(Cutoff), 1, 0)]
  OutliersRaw[, Rank := data.table::frank(PredictIsoForest) / .N]
  data.table::setcolorder(OutliersRaw, c(4L, 3L, 1L, 2L))

  # Merge back with source data ----
  data <- cbind(data, OutliersRaw)

  # Merge data back with IDcolData ----
  if(exists("IDcolData")) data <- cbind(IDcolData, data)

  # Return data ----
  return(data)
}

#' @title IsolationForest_H2O
#'
#' @description Utilize an H2O isolation forest to provide anomaly detection
#'
#' @author Adrian Antico
#' @family Feature Engineering - Model Based
#'
#' @param ArgsList ArgsList
#' @param TrainData. data
#' @param ValidationData. data
#' @param TestData. data
#' @param ScoringData. data
#'
#' @noRd
IsolationForest_H2O <- function(ArgsList=ArgsList,
                                TrainData. = NULL,
                                ValidationData. = NULL,
                                TestData. = NULL,
                                ScoringData. = NULL) {

  # Give h2o some sleep time
  if(any(ArgsList$Services %chin% c("H2OWord2Vec","H2OAutoEncoder"))) Sys.sleep(10L)

  # Run Mode
  if(tolower(RunMode) == "train") {

    # Remove old model files
    if(file.exists(file.path(ArgsList$MetaData$Models_Path, paste0(ArgsList$MetaData$ProjectID, "_IsolationForest")))) {
      file.remove(file.path(ArgsList$MetaData$Models_Path, paste0(ArgsList$MetaData$ProjectID, "_IsolationForest")))
    }

    # Metadata
    Start <- Sys.time()
    tempnames <- names(data.table::copy(TrainData.))
    ColsUsed <- names(TrainData.)
    ColsUsed <- ColsUsedp[!ColsUsed %chin% c(ArgsList$Data$TargetVariables, ArgsList$Data$PrimaryDateVariables, ArgsList$Data$IDVariables)]

    # Run function
    TrainData. <- AutoQuant::H2OIsolationForest(
      data = TrainData.,
      Features = ColsUsed,
      IDcols = c(ArgsList$Data$TargetVariables, ArgsList$Data$PrimaryDateVariables, ArgsList$Data$IDVariables),
      ModelID = paste0(ArgsList$MetaData$ProjectID, "_IsolationForest"),
      SavePath = ArgsList$MetaData$Models_Path,
      NThreads = max(1L, parallel::detectCores()-2L),
      MaxMem = ArgsList$FE_Args$General$H2O_Memory,
      Threshold = ArgsList$FE_Args$H2O_IsolationForest$Threshold,
      NTrees = ArgsList$FE_Args$H2O_IsolationForest$NTrees,
      MaxDepth = ArgsList$FE_Args$H2O_IsolationForest$MaxDepth,
      MinRows = ArgsList$FE_Args$H2O_IsolationForest$MinRows,
      RowSampleRate = ArgsList$FE_Args$H2O_IsolationForest$RowSampleRate,
      ColSampleRate = ArgsList$FE_Args$H2O_IsolationForest$ColSampleRate,
      ColSampleRatePerLevel = ArgsList$FE_Args$H2O_IsolationForest$ColSampleRatePerLevel,
      ColSampleRatePerTree = ArgsList$FE_Args$H2O_IsolationForest$ColSampleRatePerTree,
      CategoricalEncoding = c("AUTO"),
      Debug = TRUE)

    # Args tracking
    ArgsList$FE_H2OIsolationForest$Features <- ColsUsed
    ArgsList$FE_H2OIsolationForest$IDcols <- c(ArgsList$Data$TargetVariables, ArgsList$Data$PrimaryDateVariables, ArgsList$Data$IDVariables)
    ArgsList$FE_H2OIsolationForest$ModelID <- paste0(ArgsList$MetaData$ProjectID, "_IsolationForest")
    ArgsList$FE_H2OIsolationForest$SavePath <- ArgsList$MetaData$Models_Path
    ArgsList$FE_H2OIsolationForest$NThreads <- max(1L, parallel::detectCores()-2L)
    ArgsList$FE_H2OIsolationForest$MaxMem <- ArgsList$FE_Args$General$H2O_Memory
    ArgsList$FE_H2OIsolationForest$Threshold <- ArgsList$FE_Args$H2O_IsolationForest$Threshold
    ArgsList$FE_H2OIsolationForest$NTrees <- ArgsList$FE_Args$H2O_IsolationForest$NTrees
    ArgsList$FE_H2OIsolationForest$MaxDepth <- ArgsList$FE_Args$H2O_IsolationForest$MaxDepth
    ArgsList$FE_H2OIsolationForest$MinRows <- ArgsList$FE_Args$H2O_IsolationForest$MinRows
    ArgsList$FE_H2OIsolationForest$RowSampleRate <- ArgsList$FE_Args$H2O_IsolationForest$RowSampleRate
    ArgsList$FE_H2OIsolationForest$ColSampleRate <- ArgsList$FE_Args$H2O_IsolationForest$ColSampleRate
    ArgsList$FE_H2OIsolationForest$ColSampleRatePerLevel <- ArgsList$FE_Args$H2O_IsolationForest$ColSampleRatePerLevel
    ArgsList$FE_H2OIsolationForest$ColSampleRatePerTree <- ArgsList$FE_Args$H2O_IsolationForest$ColSampleRatePerTree

    # New columns tracking
    ArgsList$FE_Columns$H2OIsolationForest <- setdiff(names(data.table::copy(TrainData.), tempnames))

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$H2OIsolationForest_Training <- difftime(End, Start, units = "mins")

    # Score validation data
    if(!is.null(ValidationData.)) {

      # Pause
      Sys.sleep(10L)

      # Score model
      ValidationData. <- AutoQuant::H2OIsolationForestScoring(
        data = ValidationData.,
        Features = ArgsList$FE_H2OIsolationForest$Features,
        IDcols = ArgsList$FE_H2OIsolationForest$IDcols,
        H2OStart = TRUE,
        H2OShutdown = TRUE,
        ModelID = ArgsList$FE_H2OIsolationForest$ModelID,
        SavePath = ArgsList$FE_H2OIsolationForest$SavePath,
        Threshold = ArgsList$FE_H2OIsolationForest$Threshold,
        MaxMem = ArgsList$FE_H2OIsolationForest$MaxMem,
        NThreads = ArgsList$FE_H2OIsolationForest$NThreads,
        Debug = FALSE)

      # Args tracking
      ArgsList$FE_H2OIsolationForestScoring$Features <- ArgsList$FE_H2OIsolationForest$Features
      ArgsList$FE_H2OIsolationForestScoring$IDcols <- ArgsList$FE_H2OIsolationForest$IDcols
      ArgsList$FE_H2OIsolationForestScoring$ModelID <- ArgsList$FE_H2OIsolationForest$ModelID
      ArgsList$FE_H2OIsolationForestScoring$SavePath <- ArgsList$FE_H2OIsolationForest$SavePath
      ArgsList$FE_H2OIsolationForestScoring$NThreads <- ArgsList$FE_H2OIsolationForest$NThreads
      ArgsList$FE_H2OIsolationForestScoring$MaxMem <- ArgsList$FE_H2OIsolationForest$MaxMem
      ArgsList$FE_H2OIsolationForestScoring$Threshold <- ArgsList$FE_H2OIsolationForest$Threshold
    }

    # Score Test Data
    if(!is.null(TestData.)) {

      # Pause
      Sys.sleep(10L)

      # Score model
      TestData. <- AutoQuant::H2OIsolationForestScoring(
        data = TestData.,
        Features = ArgsList$FE_H2OIsolationForestScoring$Features,
        IDcols = ArgsList$FE_H2OIsolationForestScoring$IDcols,
        H2OStart = TRUE,
        H2OShutdown = TRUE,
        ModelID = ArgsList$FE_H2OIsolationForestScoring$ModelID,
        SavePath = ArgsList$FE_H2OIsolationForestScoring$SavePath,
        Threshold = ArgsList$FE_H2OIsolationForestScoring$Threshold,
        MaxMem = ArgsList$FE_H2OIsolationForestScoring$MaxMem,
        NThreads = ArgsList$FE_H2OIsolationForestScoring$NThreads,
        Debug = FALSE)
    }

  } else {

    # Score model
    ScoringData. <- AutoQuant::H2OIsolationForestScoring(
      data = ScoringData.,
      Features = ArgsList$FE_H2OIsolationForestScoring$Features,
      IDcols = ArgsList$FE_H2OIsolationForestScoring$IDcols,
      H2OStart = TRUE,
      H2OShutdown = TRUE,
      ModelID = ArgsList$FE_H2OIsolationForestScoring$ModelID,
      SavePath = ArgsList$FE_H2OIsolationForestScoring$SavePath,
      Threshold = ArgsList$FE_H2OIsolationForestScoring$Threshold,
      MaxMem = ArgsList$FE_H2OIsolationForestScoring$MaxMem,
      NThreads = ArgsList$FE_H2OIsolationForestScoring$NThreads,
      Debug = FALSE)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$H2OIsolationForest_Scoring <- difftime(End, Start, units = "mins")
  }

  # Return
  return(list(TrainData = TrainData., ValidationData = ValidationData., TestData = TestData., ScoringData = ScoringData., ArgsList = ArgsList))
}

#' @title AutoClustering
#'
#' @description AutoClustering adds a column to your original data with a cluster number identifier. You can run request an autoencoder to be built to reduce the dimensionality of your data before running the clusering algo.
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#'
#' @param data is the source time series data.table
#' @param FeatureColumns Independent variables
#' @param ModelID For naming the files to save
#' @param SavePath Directory path for saving models
#' @param NThreads set based on number of threads your machine has available
#' @param MaxMemory set based on the amount of memory your machine has available
#' @param MaxClusters number of factors to test out in k-means to find the optimal number
#' @param ClusterMetric pick the metric to identify top model in grid tune c('totss','betweenss','withinss')
#' @param RunDimReduction If TRUE, an autoencoder will be built to reduce the feature space. Otherwise, all features in FeatureColumns will be used for clustering
#' @param ShrinkRate Node shrink rate for H2OAutoencoder. See that function for details.
#' @param Epochs For the autoencoder
#' @param L2_Reg For the autoencoder
#' @param ElasticAveraging For the autoencoder
#' @param ElasticAveragingMovingRate For the autoencoder
#' @param ElasticAveragingRegularization For the autoencoder
#'
#' @examples
#' \dontrun{
#' #########################
#' # Training Setup
#' #########################
#'
#' # Create fake data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = TRUE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' data <- AutoQuant::AutoClustering(
#'   data,
#'   FeatureColumns = names(data)[2:(ncol(data)-1)],
#'   ModelID = 'TestModel',
#'   SavePath = getwd(),
#'   NThreads = 8,
#'   MaxMemory = '28G',
#'   MaxClusters = 50,
#'   ClusterMetric = 'totss',
#'   RunDimReduction = TRUE,
#'   ShrinkRate = (sqrt(5) - 1) / 2,
#'   Epochs = 5L,
#'   L2_Reg = 0.10,
#'   ElasticAveraging = TRUE,
#'   ElasticAveragingMovingRate = 0.90,
#'   ElasticAveragingRegularization = 0.001)
#'
#' #########################
#' # Scoring Setup
#' #########################
#'
#' Sys.sleep(10)
#'
#' # Create fake data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = TRUE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' data <- AutoQuant::AutoClusteringScoring(
#'   data,
#'   FeatureColumns = names(data)[2:(ncol(data)-1)],
#'   ModelID = 'TestModel',
#'   SavePath = getwd(),
#'   NThreads = 8,
#'   MaxMemory = '28G',
#'   DimReduction = TRUE)
#' }
#'
#' @return Original data.table with added column with cluster number identifier
#' @export
AutoClustering <- function(data,
                           FeatureColumns = NULL,
                           ModelID = 'TestModel',
                           SavePath = NULL,
                           NThreads = 8,
                           MaxMemory = '28G',
                           MaxClusters = 50,
                           ClusterMetric = 'totss',
                           RunDimReduction = TRUE,
                           ShrinkRate = (sqrt(5) - 1) / 2,
                           Epochs = 5L,
                           L2_Reg = 0.10,
                           ElasticAveraging = TRUE,
                           ElasticAveragingMovingRate = 0.90,
                           ElasticAveragingRegularization = 0.001) {

  # Check data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Dim Reduction
  if(RunDimReduction) {

    # Column names ----
    tempnames <- names(data.table::copy(data))

    # H2OAutoencoder ----
    data <- AutoQuant::H2OAutoencoder(

      # Select the service
      AnomalyDetection = FALSE,
      DimensionReduction = TRUE,

      # Data related args
      data = data,
      Features = FeatureColumns,
      per_feature = FALSE,
      RemoveFeatures = FALSE,
      ModelID = paste0(ModelID,'_Cluster_H2OAutoencoder'),
      model_path = SavePath,

      # H2O Environment
      NThreads = NThreads,
      MaxMem = MaxMemory,
      H2OStart = TRUE,
      H2OShutdown = TRUE,

      # H2O ML Args
      NodeShrinkRate = ShrinkRate,
      LayerStructure = NULL,
      ReturnLayer = 4L,
      Activation = 'Tanh',
      Epochs = Epochs,
      L2 = L2_Reg,
      ElasticAveraging = ElasticAveraging,
      ElasticAveragingMovingRate = ElasticAveragingMovingRate,
      ElasticAveragingRegularization = ElasticAveragingRegularization)

    # Sleep
    Sys.sleep(10L)

    # New cols
    FeatureColumns <- setdiff(names(data), tempnames)

    # New data
    temp <- data[, .SD, .SDcols = c(FeatureColumns)]

  } else {
    temp <- data[, .SD, .SDcols = c(FeatureColumns)]
  }

  # Convert to h2o frame
  LocalH2O <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMemory, enable_assertions = FALSE)
  H2OData <- h2o::as.h2o(temp)

  # Define grid tune search scheme in a named list ----
  search_criteria  <- list(
    strategy = 'RandomDiscrete',
    max_runtime_secs = 3600,
    max_models = 30,
    seed = 1234,
    stopping_rounds = 10)

  # Define hyperparameters----
  HyperParams <- list(max_iterations = c(10, 20, 50, 100), init = c('Random','PlusPlus','Furthest'))

  # Run grid tune ----
  grid <- h2o::h2o.grid(
    'kmeans',
    search_criteria = search_criteria,
    training_frame = H2OData,
    x = FeatureColumns,
    k = MaxClusters,
    grid_id = paste0(ModelID,'_KMeans'),
    estimate_k = TRUE,
    hyper_params = HyperParams)

  # Get best performer----
  Grid_Out <- h2o::h2o.getGrid(grid_id = paste0(ModelID,'_KMeans'), sort_by = ClusterMetric, decreasing = FALSE)
  ClusterModel <- h2o::h2o.getModel(model_id = Grid_Out@model_ids[[1L]])

  # Save ClusterModel if requested ----
  if(!is.null(SavePath)) save_model <- h2o::h2o.saveModel(object = ClusterModel, path = SavePath, force = TRUE)

  # Combine outputs ----
  preds <- data.table::as.data.table(h2o::h2o.predict(ClusterModel, H2OData))
  h2o::h2o.shutdown(prompt = FALSE)
  data <- data.table::as.data.table(cbind(data, preds))
  data.table::setnames(data, 'predict', 'ClusterID')
  file.rename(from = save_model, to = file.path(SavePath, paste0(ModelID, '_KMeans')))
  return(data)
}

#' @title AutoClusteringScoring
#'
#' @description AutoClusteringScoring adds a column to your original data with a cluster number identifier. You can run request an autoencoder to be built to reduce the dimensionality of your data before running the clusering algo.
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#'
#' @param data is the source time series data.table
#' @param FeatureColumns Independent variables
#' @param ModelID This is returned from the training run in the output list with element named 'model_name'. It's not identical to the ModelID used in training due to the grid tuning.
#' @param SavePath Directory path for saving models
#' @param NThreads set based on number of threads your machine has available
#' @param MaxMemory set based on the amount of memory your machine has available
#' @param DimReduction Set to TRUE if you set RunDimReduction in the training version of this function
#'
#' @examples
#' \dontrun{
#' #########################
#' # Training Setup
#' #########################
#'
#' # Create fake data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = TRUE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' data <- AutoQuant::AutoClustering(
#'   data,
#'   FeatureColumns = names(data)[2:(ncol(data)-1)],
#'   ModelID = 'TestModel',
#'   SavePath = getwd(),
#'   NThreads = 8,
#'   MaxMemory = '28G',
#'   MaxClusters = 50,
#'   ClusterMetric = 'totss',
#'   RunDimReduction = TRUE,
#'   ShrinkRate = (sqrt(5) - 1) / 2,
#'   Epochs = 5L,
#'   L2_Reg = 0.10,
#'   ElasticAveraging = TRUE,
#'   ElasticAveragingMovingRate = 0.90,
#'   ElasticAveragingRegularization = 0.001)
#'
#' #########################
#' # Scoring Setup
#' #########################
#'
#' Sys.sleep(10)
#'
#' # Create fake data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 1000,
#'   ID = 2,
#'   ZIP = 0,
#'   AddDate = TRUE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Run function
#' data <- AutoQuant::AutoClusteringScoring(
#'   data,
#'   FeatureColumns = names(data)[2:(ncol(data)-1)],
#'   ModelID = 'TestModel',
#'   SavePath = getwd(),
#'   NThreads = 8,
#'   MaxMemory = '28G',
#'   DimReduction = TRUE)
#' }
#'
#' @return Original data.table with added column with cluster number identifier
#' @export
AutoClusteringScoring <- function(data,
                                  FeatureColumns = NULL,
                                  ModelID = 'TestModel',
                                  SavePath = NULL,
                                  NThreads = 8,
                                  MaxMemory = '28G',
                                  DimReduction = TRUE) {

  # Check data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Dim Reduction
  if(DimReduction) {

    # Column names ----
    tempnames <- names(data.table::copy(data))

    # Score H2OAutoencoder
    data <- AutoQuant::H2OAutoencoderScoring(

      # Select the service
      AnomalyDetection = FALSE,
      DimensionReduction = TRUE,

      # Data related args
      data = data,
      Features = FeatureColumns,
      RemoveFeatures = FALSE,
      per_feature = FALSE,
      ModelObject = NULL,
      ModelID = paste0(ModelID,'_Cluster_H2OAutoencoder'),
      model_path = SavePath,

      # H2O args
      NThreads = NThreads,
      MaxMem = MaxMemory,
      H2OStart = TRUE,
      H2OShutdown = TRUE,
      ReturnLayer = 4L)

    # Sleep
    Sys.sleep(10L)

    # New cols
    FeatureColumns <- setdiff(names(data), tempnames)

    # New data
    temp <- data[, .SD, .SDcols = c(FeatureColumns)]

  } else {
    temp <- data[, .SD, .SDcols = c(FeatureColumns)]
  }

  # Convert to h2o frame
  LocalH2O <- h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMemory, enable_assertions = FALSE)
  H2OData <- h2o::as.h2o(temp)

  # Load model
  ClusterModel <- h2o::h2o.loadModel(path = file.path(SavePath, paste0(ModelID, '_KMeans')))

  # Combine outputs ----
  preds <- data.table::as.data.table(h2o::h2o.predict(ClusterModel, H2OData))
  h2o::h2o.shutdown(prompt = FALSE)
  data <- data.table::as.data.table(cbind(data, preds))
  data.table::setnames(data, 'predict', 'ClusterID')
  return(data)
}

#' @title Clustering_H2O
#'
#' @description Utilize an H2O autoencoder and kmeans to create a cluster id column
#'
#' @author Adrian Antico
#' @family Feature Engineering - Model Based
#'
#' @param ArgsList ArgsList_FEE
#' @param TrainData. data
#' @param ValidationData. data
#' @param TestData. data
#' @param ScoringData. data
#'
#' @noRd
Clustering_H2O <- function(ArgsList=ArgsList_FEE,
                           TrainData. = NULL,
                           ValidationData. = NULL,
                           TestData. = NULL,
                           ScoringData. = NULL) {

  # Give h2o some sleep time
  if(any(ArgsList$Services %chin% c("H2OWord2Vec","H2OAutoEncoder"))) Sys.sleep(10L)

  # Run Mode
  if(tolower(RunMode) == "train") {

    # Remove old model files
    if(file.exists(file.path(ArgsList$MetaData$Models_Path, paste0(ArgsList$MetaData$ProjectID, "_Clustering")))) {
      file.remove(file.path(ArgsList$MetaData$Models_Path, paste0(ArgsList$MetaData$ProjectID, "_Clustering")))
    }

    # Metadata
    Start <- Sys.time()
    tempnames <- names(data.table::copy(TrainData.))

    # Features
    if(!is.null(ArgsList$FE_Columns$H2OAutoEncoder)) {
      ColsUsed <- ArgsList$FE_Columns$H2OAutoEncoder
      IDcols. <- setdiff(names(data.table::copy(TrainData.)), ColsUsed)
      RunDimReduction. <- FALSE
    } else {
      ColsUsed <- names(TrainData.)
      ColsUsed <- ColsUsedp[!ColsUsed %chin% c(ArgsList$Data$TargetVariables, ArgsList$Data$PrimaryDateVariables, ArgsList$Data$IDVariables)]
      RunDimReduction. <- TRUE
    }

    # Run function
    TrainData. <- AutoQuant::AutoClustering(
      data = TrainData.,
      Features = ColsUsed,
      ModelID = paste0(ArgsList$MetaData$ProjectID, "_Clustering"),
      IDcols = c(ArgsList$Data$TargetVariables, ArgsList$Data$PrimaryDateVariables, ArgsList$Data$IDVariables),
      SavePath = ArgsList$MetaData$Models_Path,
      NThreads = max(1L, parallel::detectCores()-2L),
      MaxMem = ArgsList$FE_Args$General$H2O_Memory,
      MaxClusters = ArgsList$FE_Args$H2O_Clustering$MaxClusters,
      ClusterMetric = ArgsList$FE_Args$H2O_Clustering$ClusterMetric,
      RunDimReduction = RunDimReduction.,
      ShrinkRate = ArgsList$FE_Args$H2O_Clustering$Clustering_ShrinkRate,
      Epochs = ArgsList$FE_Args$H2O_Clustering$Clustering_Epochs,
      L2_Reg = ArgsList$FE_Args$H2O_Clustering$Clustering_L2,
      ElasticAveraging = ArgsList$FE_Args$H2O_Clustering$Clustering_ElasticAveraging,
      ElasticAveragingMovingRate = ArgsList$FE_Args$H2O_Clustering$Clustering_ElasticAveragingMovingRate,
      ElasticAveragingRegularization = ArgsList$FE_Args$H2O_Clustering$Clustering_ElasticAveragingRegularization)

    # Args tracking
    ArgsList$FE_H2OClustering$Features <- ColsUsed
    ArgsList$FE_H2OClustering$IDcols <- c(ArgsList$Data$TargetVariables, ArgsList$Data$PrimaryDateVariables, ArgsList$Data$IDVariables)
    ArgsList$FE_H2OClustering$ModelID <- paste0(ArgsList$MetaData$ProjectID, "_Clustering")
    ArgsList$FE_H2OClustering$SavePath <- ArgsList$MetaData$Models_Path
    ArgsList$FE_H2OClustering$NThreads <- max(1L, parallel::detectCores()-2L)
    ArgsList$FE_H2OClustering$MaxMem <- ArgsList$FE_Args$General$H2O_Memory
    ArgsList$FE_H2OClustering$MaxClusters <- ArgsList$FE_Args$H2O_Clustering$MaxClusters
    ArgsList$FE_H2OClustering$ClusterMetric <- ArgsList$FE_Args$H2O_Clustering$ClusterMetric
    ArgsList$FE_H2OClustering$RunDimReduction <- RunDimReduction.
    ArgsList$FE_H2OClustering$ShrinkRate <- ArgsList$FE_Args$H2O_Clustering$Clustering_ShrinkRate
    ArgsList$FE_H2OClustering$Epochs <- ArgsList$FE_Args$H2O_Clustering$Clustering_Epochs
    ArgsList$FE_H2OClustering$L2_Reg <- ArgsList$FE_Args$H2O_Clustering$Clustering_L2
    ArgsList$FE_H2OClustering$ElasticAveraging <- ArgsList$FE_Args$H2O_Clustering$Clustering_ElasticAveraging
    ArgsList$FE_H2OClustering$ElasticAveragingMovingRate <- ArgsList$FE_Args$H2O_Clustering$Clustering_ElasticAveragingMovingRate
    ArgsList$FE_H2OClustering$ElasticAveragingRegularization <- ArgsList$FE_Args$H2O_Clustering$Clustering_ElasticAveragingRegularization

    # New columns tracking
    ArgsList$FE_Columns$H2OClustering <- setdiff(names(data.table::copy(TrainData.), tempnames))

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$H2OClustering_Training <- difftime(End, Start, units = "mins")

    # Score validation data
    if(!is.null(ValidationData.)) {

      # Pause
      Sys.sleep(10L)

      # Score model
      ValidationData. <- AutoQuant::AutoClusteringScoring(
        data = ValidationData.,
        FeatureColumns = ArgsList$FE_H2OClustering$Features,
        ModelID = ArgsList$FE_H2OClustering$ModelID,
        SavePath = ArgsList$FE_H2OClustering$SavePath,
        NThreads = ArgsList$FE_H2OClustering$NThreads,
        MaxMemory = ArgsList$FE_H2OClustering$MaxMem,
        DimReduction = ArgsList$FE_H2OClustering$RunDimReduction)

      # Args tracking
      ArgsList$FE_H2OClusteringScoring$FeatureColumns <- ArgsList$FE_H2OClustering$Features
      ArgsList$FE_H2OClusteringScoring$ModelID <- ArgsList$FE_H2OClustering$ModelID
      ArgsList$FE_H2OClusteringScoring$SavePath <- ArgsList$FE_H2OClustering$SavePath
      ArgsList$FE_H2OClusteringScoring$NThreads <- ArgsList$FE_H2OClustering$NThreads
      ArgsList$FE_H2OClusteringScoring$MaxMemory <- ArgsList$FE_H2OClustering$MaxMem
      ArgsList$FE_H2OClusteringScoring$DimReduction <- ArgsList$FE_H2OClustering$RunDimReduction
    }

    # Score Test Data
    if(!is.null(TestData.)) {

      # Pause
      Sys.sleep(10L)

      # Score model
      TestData. <- AutoQuant::AutoClusteringScoring(
        data = TestData.,
        FeatureColumns = ArgsList$FE_H2OClusteringScoring$FeatureColumns,
        ModelID = ArgsList$FE_H2OClusteringScoring$ModelID,
        SavePath = ArgsList$FE_H2OClusteringScoring$SavePath,
        NThreads = ArgsList$FE_H2OClusteringScoring$NThreads,
        MaxMemory = ArgsList$FE_H2OClusteringScoring$MaxMemory,
        DimReduction = ArgsList$FE_H2OClusteringScoring$DimReduction)
    }

  } else {

    # Score model
    ScoringData. <- AutoQuant::AutoClusteringScoring(
      data = ScoringData.,
      FeatureColumns = ArgsList$FE_H2OClusteringScoring$FeatureColumns,
      ModelID = ArgsList$FE_H2OClusteringScoring$ModelID,
      SavePath = ArgsList$FE_H2OClusteringScoring$SavePath,
      NThreads = ArgsList$FE_H2OClusteringScoring$NThreads,
      MaxMemory = ArgsList$FE_H2OClusteringScoring$MaxMemory,
      DimReduction = ArgsList$FE_H2OClusteringScoring$DimReduction)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$H2OClustering_Scoring <- difftime(End, Start, units = "mins")
  }

  # Return
  return(list(TrainData = TrainData., ValidationData = ValidationData., TestData = TestData., ScoringData = ScoringData., ArgsList = ArgsList))
}
