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
#' data <- RemixAutoML::FakeDataGenerator(
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
#' data <- RemixAutoML::AutoWord2VecModeler(
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
#' data <- RemixAutoML::FakeDataGenerator(
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
#' data <- RemixAutoML::AutoWord2VecScoring(
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

  # Arg check ----
  if(is.null(stringCol)) stop("stringCol cannot be NULL")
  if(length(stringCol) == 1L) BuildType <- "individual"

  # Ensure data is a data.table ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Two processes ----
  if(tolower(BuildType) == "combined") {

    # Create storage file----
    N <- length(stringCol)
    StoreFile <- data.table::data.table(ModelName = rep("a", 1), Path = rep("a", 1), Jar = rep("a", 1))
    i <- 0L
    for(string in stringCol) {

      # Increment----
      i <- i + 1L

      # Ensure stringCol is character (not factor)----
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

    # Create storage file ----
    N <- length(stringCol)
    StoreFile <- data.table::data.table(ModelName = rep("a", N), Path = rep("a", N), Jar = rep("a", N))
    i <- 0L
    for(string in stringCol) {
      if(!is.character(data[[eval(string)]])) data[, eval(string) := as.character(get(string))]
      i <- i + 1L
      Sys.sleep(10L)
      h2o::h2o.init(nthreads = Threads, max_mem_size = MaxMemory)

      # It is important to remove "\n" --
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
#' data <- RemixAutoML::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000L,
#'   ID = 2L,H2
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
#' data <- RemixAutoML::AutoWord2VecModeler(
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
#' data <- RemixAutoML::FakeDataGenerator(
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
#' data <- RemixAutoML::AutoWord2VecScoring(
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

  # Check args ----
  if(is.null(stringCol)) stop("stringCol cannot be NULL")
  if(is.null(ModelObject) && tolower(BuildType) == "combined" && !file.exists(file.path(model_path, ModelID))) stop(paste0("Model not found at ", file.path(model_path, ModelID)))
  if(is.null(ModelObject) && tolower(BuildType) == "individual" && !file.exists(file.path(model_path, paste0(ModelID, "_", stringCol)))) stop(paste0("Model not found at ", file.path(model_path, paste0(ModelID, "_", stringCol))))

  # Instantiate H2O ----
  if(H2OStartUp) localH2O <- h2o::h2o.init(nthreads = Threads, max_mem_size = MaxMemory, enable_assertions = FALSE)

  # Build vecs ----
  if(tolower(BuildType) == "individual") {
    for(textCol in stringCol) {
      model <- h2o::h2o.loadModel(path = file.path(model_path, paste0(ModelID, "_", textCol)))
      data1 <- AutoH2OTextPrepScoring(
        data = data,
        string = textCol,
        NThreads = Threads,
        MaxMem = MaxMemory,
        StartH2O = FALSE)
      Scores <- data.table::as.data.table(h2o::h2o.transform(
        model,
        words = data1,
        aggregate_method = "AVERAGE"))
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
      data1 <- AutoH2OTextPrepScoring(
        data = data,
        string = textCol,
        NThreads = Threads,
        MaxMem = MaxMemory,
        StartH2O = FALSE)
      Scores <- data.table::as.data.table(h2o::h2o.transform(
        model,
        words = data1,
        aggregate_method = "AVERAGE"))
      data.table::setnames(Scores, names(Scores), paste0(textCol, "_", names(Scores)))
      if(!KeepStringCol) {
        data[, eval(textCol) := NULL]
        data <- cbind(data, Scores)
      } else {
        data <- cbind(data, Scores)
      }
    }
  }

  # Remove H2O Objects ----
  rm(localH2O, model)

  # H2O Shutdown ----
  if(H2OShutdown) h2o::h2o.shutdown(prompt = FALSE)

  # Return ----
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

  # Run mode
  if(tolower(RunMode) == "train") {

    # Remove stale model if it exists
    if(file.exists(file.path(ArgsList$MetaData$Models_Path, paste0(ArgsList$MetaData$ProjectID, "_Word2Vec")))) {
      file.remove(file.path(ArgsList$MetaData$Models_Path, paste0(ArgsList$MetaData$ProjectID, "_Word2Vec")))
    }

    # MetaData
    Start <- Sys.time()
    tempnames <- names(TrainData)

    # Run AutoWord2VecModeler
    TrainData. <- RemixAutoML::AutoWord2VecModeler(
      data = TrainData.,
      BuildType = ArgsList$FE_Args$H2O_Word2Vec$BuildType,
      stringCol = ArgsList$Data$TextVariables,
      KeepStringCol = FALSE,
      ModelID = paste0(ArgsList$MetaData$ProjectID, "_Word2Vec"),
      model_path = ArgsList$MetaData$Models_Path,
      vects = ArgsList$FE_Args$H2O_Word2Vec$NumberVectors,
      MinWords = ArgsList$FE_Args$H2O_Word2Vec$MinWords,
      WindowSize = ArgsList$FE_Args$H2O_Word2Vec$Window,
      Epochs = ArgsList$FE_Args$H2O_Word2Vec$Iterations,
      SaveModel = "standard",
      Threads = max(1L, parallel::detectCores()-2L),
      MaxMemory = ArgsList$FE_Args$General$H2O_Memory)

    # Args tracking
    ArgsList$FE_H2OWord2Vec$BuildType <- ArgsList$FE_Args$H2O_Word2Vec$BuildType
    ArgsList$FE_H2OWord2Vec$stringCol <- ArgsList$Data$TextVariables
    ArgsList$FE_H2OWord2Vec$KeepStringCol <- FALSE
    ArgsList$FE_H2OWord2Vec$ModelID <- paste0(ArgsList$MetaData$ProjectID, "_Word2Vec")
    ArgsList$FE_H2OWord2Vec$model_path <- ArgsList$MetaData$Models_Path
    ArgsList$FE_H2OWord2Vec$vects <- ArgsList$FE_Args$H2O_Word2Vec$NumberVectors
    ArgsList$FE_H2OWord2Vec$WindowSize <- ArgsList$FE_Args$H2O_Word2Vec$Window
    ArgsList$FE_H2OWord2Vec$Epochs <- ArgsList$FE_Args$H2O_Word2Vec$Iterations
    ArgsList$FE_H2OWord2Vec$Threads <- max(1L, parallel::detectCores()-2L)
    ArgsList$FE_H2OWord2Vec$MaxMemory <- ArgsList$FE_Args$General$H2O_Memory

    # Update IDVariables
    if(!ArgsList$FE_H2OWord2Vec$KeepStringCol) {
      ArgsList$Data$TextVariables <- NULL
    }

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$H2OWord2Vec_Training <- difftime(End, Start, units = "mins")

    # New column tracking
    ArgsList$FE_Columns$H2OWord2Vec <- setdiff(names(data.table::copy(TrainData.)), tempnames)

    # Score new data
    if(!is.null(ValidationData.)) {
      ValidationData. <- RemixAutoML::AutoWord2VecScoring(
        data = ValidationData.,
        BuildType = ArgsList$FE_H2OWord2Vec$BuildType,
        stringCol = ArgsList$FE_H2OWord2Vec$stringCol,
        KeepStringCol = ArgsList$FE_H2OWord2Vec$KeepStringCol,
        ModelID = ArgsList$FE_H2OWord2Vec$ModelID,
        ModelObject = NULL,
        model_path = ArgsList$FE_H2OWord2Vec$model_path,
        H2OStartUp = TRUE,
        H2OShutdown = TRUE,
        Threads = ArgsList$FE_H2OWord2Vec$Threads,
        MaxMemory = ArgsList$FE_H2OWord2Vec$MaxMemory)

      # Args tracking
      ArgsList$FE_H2OWord2VecScoring$BuildType <- ArgsList$FE_H2OWord2Vec$BuildType
      ArgsList$FE_H2OWord2VecScoring$stringCol <- ArgsList$FE_H2OWord2Vec$stringCol
      ArgsList$FE_H2OWord2VecScoring$KeepStringCol <- ArgsList$FE_H2OWord2Vec$KeepStringCol
      ArgsList$FE_H2OWord2VecScoring$ModelID <- ArgsList$FE_H2OWord2Vec$ModelID
      ArgsList$FE_H2OWord2VecScoring$model_path <- ArgsList$FE_H2OWord2Vec$model_path
      ArgsList$FE_H2OWord2VecScoring$Threads <- ArgsList$FE_H2OWord2Vec$Threads
      ArgsList$FE_H2OWord2VecScoring$MaxMemory <- ArgsList$FE_H2OWord2Vec$MaxMemory
    }

    # Score new data
    if(!is.null(ValidationData.)) {
      TestData. <- RemixAutoML::AutoWord2VecScoring(
        data = TestData.,
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
    }

    # Return
    return(list(TrainData = TrainData., ValidationData = ValidationData., TestData = TestData., ArgsList = ArgsList))


  } else {

    # MetaData
    Start <- Sys.time()
    tempnames <- names(ScoringData.)

    # Score new data
    data <- RemixAutoML::AutoWord2VecScoring(
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

#' @title AutoEncoder_H2O
#'
#' @description Utilize an H2O autoencoder to provide dimensionality reduction and anomaly detection
#'
#' @author Adrian Antico
#' @family Feature Engineering - Model Based
#'
#' @param ArgsList ArgsList_FEE
#' @param TrainData. data
#' @param ValidationData. data
#' @param TestData. data
#' @param ScoringData. data
#' @param AnomalyDetection. TRUE
#' @param DimensionReduction. TRUE
#' @param AD_PerFeature FALSE
#' @param RemoveBaseFeatures FALSE
#' @param NodeShrinkRate. (sqrt(5) - 1) / 2
#' @param Epochs. 20
#' @param L2. 0.10
#' @param ElasticAveraging. TRUE
#' @param ElasticAveragingMovingRate. 0.90
#' @param ElasticAveragingRegularization. 0.001
#'
#' @noRd
AutoEncoder_H2O <- function(ArgsList=ArgsList_FEE,
                            TrainData. = NULL,
                            ValidationData. = NULL,
                            TestData. = NULL,
                            ScoringData. = NULL) {

  # Give h2o some sleep time
  if(any(ArgsList$Services %chin% "H2OWord2Vec")) Sys.sleep(10L)

  # Run Mode
  if(tolower(RunMode) == "train") {

    # Remove old model files
    if(file.exists(file.path(ArgsList$MetaData$Models_Path, paste0(ArgsList$MetaData$ProjectID, "_AutoEncoder")))) {
      file.remove(file.path(ArgsList$MetaData$Models_Path, paste0(ArgsList$MetaData$ProjectID, "_AutoEncoder")))
    }

    # Metadata
    Start <- Sys.time()
    tempnames <- names(data.table::copy(TrainData.))
    ColsUsed <- names(TrainData.)
    ColsUsed <- ColsUsedp[!ColsUsed %chin% c(ArgsList$Data$TargetVariables, ArgsList$Data$PrimaryDateVariables, ArgsList$Data$IDVariables)]

    # Run function
    TrainData. <- RemixAutoML::H2OAutoencoder(

      # Select the service
      AnomalyDetection = ArgsList$FE_Args$H2O_Autoencoder$AnomalyDetection,
      DimensionReduction = ArgsList$FE_Args$H2O_Autoencoder$DimensionReduction,

      # Data related args
      data = TrainData.,
      Features = ColsUsed,
      per_feature = ArgsList$FE_Args$H2O_Autoencoder$AD_PerFeature,
      RemoveFeatures = ArgsList$FE_Args$H2O_Autoencoder$RemoveBaseFeatures,
      ModelID = paste0(ArgsList$MetaData$ProjectID, "_AutoEncoder"),
      model_path = ArgsList$MetaData$Models_Path,

      # H2O Environment
      NThreads = max(1L, parallel::detectCores()-2L),
      MaxMem = ArgsList$FE_Args$General$H2O_Memory,
      H2OStart = TRUE,
      H2OShutdown = TRUE,

      # H2O ML Args
      LayerStructure = NULL,
      NodeShrinkRate = ArgsList$FE_Args$H2O_Autoencoder$NodeShrinkRate,
      ReturnLayer = 4L,
      Activation = "Tanh",
      Epochs = ArgsList$FE_Args$H2O_Autoencoder$Epochs,
      L2 = ArgsList$FE_Args$H2O_Autoencoder$L2,
      ElasticAveraging = ArgsList$FE_Args$H2O_Autoencoder$ElasticAveraging,
      ElasticAveragingMovingRate = ArgsList$FE_Args$H2O_Autoencoder$ElasticAveragingMovingRate,
      ElasticAveragingRegularization = ArgsList$FE_Args$H2O_Autoencoder$ElasticAveragingRegularization)

    # Args tracking
    ArgsList$FE_H2OAutoEncoder$AnomalyDetection <- ArgsList$FE_Args$H2O_Autoencoder$AnomalyDetection
    ArgsList$FE_H2OAutoEncoder$DimensionReduction <- ArgsList$FE_Args$H2O_Autoencoder$DimensionReduction
    ArgsList$FE_H2OAutoEncoder$Features <- ColsUsed
    ArgsList$FE_H2OAutoEncoder$per_feature <- ArgsList$FE_Args$H2O_Autoencoder$AD_PerFeature
    ArgsList$FE_H2OAutoEncoder$RemoveFeatures <- ArgsList$FE_Args$H2O_Autoencoder$RemoveBaseFeatures
    ArgsList$FE_H2OAutoEncoder$ModelID <- paste0(ArgsList$MetaData$ProjectID, "_AutoEncoder")
    ArgsList$FE_H2OAutoEncoder$model_path <- ArgsList$MetaData$Models_Path
    ArgsList$FE_H2OAutoEncoder$NThreads <- max(1L, parallel::detectCores()-2L)
    ArgsList$FE_H2OAutoEncoder$MaxMem <- ArgsList$FE_Args$General$H2O_Memory
    ArgsList$FE_H2OAutoEncoder$H2OStart <- TRUE
    ArgsList$FE_H2OAutoEncoder$H2OShutdown <- TRUE
    ArgsList$FE_H2OAutoEncoder$NodeShrinkRate <- ArgsList$FE_Args$H2O_Autoencoder$NodeShrinkRate
    ArgsList$FE_H2OAutoEncoder$Epochs <- ArgsList$FE_Args$H2O_Autoencoder$Epochs
    ArgsList$FE_H2OAutoEncoder$L2 <- ArgsList$FE_Args$H2O_Autoencoder$L2
    ArgsList$FE_H2OAutoEncoder$ElasticAveraging <- ArgsList$FE_Args$H2O_Autoencoder$ElasticAveraging
    ArgsList$FE_H2OAutoEncoder$ElasticAveragingMovingRate <- ArgsList$FE_Args$H2O_Autoencoder$ElasticAveragingMovingRate
    ArgsList$FE_H2OAutoEncoder$ElasticAveragingRegularization <- ArgsList$FE_Args$H2O_Autoencoder$ElasticAveragingRegularization

    # New columns tracking
    ArgsList$FE_Columns$H2OAutoEncoder <- setdiff(names(data.table::copy(TrainData.), tempnames))

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$H2OAutoEncoder_Training <- difftime(End, Start, units = "mins")

    # Score validation data
    if(!is.null(ValidationData.)) {

      # Pause
      Sys.sleep(10L)

      # Score model
      ValidationData. <- RemixAutoML::H2OAutoencoderScoring(

        # Select the service
        AnomalyDetection = ArgsList$FE_H2OAutoEncoder$AnomalyDetection,
        DimensionReduction = ArgsList$FE_H2OAutoEncoder$DimensionReduction,

        # Data related args
        data = ValidationData.,
        Features = ArgsList$FE_H2OAutoEncoder$Features,
        per_feature = ArgsList$FE_H2OAutoEncoder$per_feature,
        RemoveFeatures = ArgsList$FE_H2OAutoEncoder$RemoveFeatures,
        ModelObject = NULL,
        ModelID = ArgsList$FE_H2OAutoEncoder$ModelID,
        model_path = ArgsList$FE_H2OAutoEncoder$model_path,

        # H2O args
        NThreads = ArgsList$FE_H2OAutoEncoder$NThreads,
        MaxMem = ArgsList$FE_H2OAutoEncoder$MaxMem,
        H2OStart = ArgsList$FE_H2OAutoEncoder$H2OStart,
        H2OShutdown = ArgsList$FE_H2OAutoEncoder$H2OShutdown,
        ReturnLayer = 4L)

      # Args tracking
      ArgsList$FE_H2OAutoEncoderScoring$AnomalyDetection <- ArgsList$FE_H2OAutoEncoder$AnomalyDetection
      ArgsList$FE_H2OAutoEncoderScoring$DimensionReduction <- ArgsList$FE_H2OAutoEncoder$DimensionReduction
      ArgsList$FE_H2OAutoEncoderScoring$Features <- ArgsList$FE_H2OAutoEncoder$Features
      ArgsList$FE_H2OAutoEncoderScoring$per_feature <- ArgsList$FE_H2OAutoEncoder$per_feature
      ArgsList$FE_H2OAutoEncoderScoring$RemoveFeatures <- ArgsList$FE_H2OAutoEncoder$RemoveFeatures
      ArgsList$FE_H2OAutoEncoderScoring$ModelID <- ArgsList$FE_H2OAutoEncoder$ModelID
      ArgsList$FE_H2OAutoEncoderScoring$model_path <- ArgsList$FE_H2OAutoEncoder$model_path
      ArgsList$FE_H2OAutoEncoderScoring$NThreads <- ArgsList$FE_H2OAutoEncoder$NThreads
      ArgsList$FE_H2OAutoEncoderScoring$MaxMem <- ArgsList$FE_H2OAutoEncoder$MaxMem
      ArgsList$FE_H2OAutoEncoderScoring$H2OStart <- ArgsList$FE_H2OAutoEncoder$H2OStart
      ArgsList$FE_H2OAutoEncoderScoring$H2OShutdown <- ArgsList$FE_H2OAutoEncoder$H2OShutdown
    }

    # Score Test Data
    if(!is.null(TestData.)) {

      # Pause
      Sys.sleep(10L)

      # Score model
      TestData. <- RemixAutoML::H2OAutoencoderScoring(

        # Select the service
        AnomalyDetection = ArgsList$FE_H2OAutoEncoderScoring$AnomalyDetection,
        DimensionReduction = ArgsList$FE_H2OAutoEncoderScoring$DimensionReduction,

        # Data related args
        data = TestData.,
        Features = ArgsList$FE_H2OAutoEncoderScoring$Features,
        per_feature = ArgsList$FE_H2OAutoEncoderScoring$per_feature,
        RemoveFeatures = ArgsList$FE_H2OAutoEncoderScoring$RemoveFeatures,
        ModelObject = NULL,
        ModelID = ArgsList$FE_H2OAutoEncoderScoring$ModelID,
        model_path = ArgsList$FE_H2OAutoEncoderScoring$model_path,

        # H2O args
        NThreads = ArgsList$FE_H2OAutoEncoderScoring$NThreads,
        MaxMem = ArgsList$FE_H2OAutoEncoderScoring$MaxMem,
        H2OStart = ArgsList$FE_H2OAutoEncoderScoring$H2OStart,
        H2OShutdown = ArgsList$FE_H2OAutoEncoderScoring$H2OShutdown,
        ReturnLayer = 4L)
    }

  } else {

    # Score model
    ScoringData. <- RemixAutoML::H2OAutoencoderScoring(

      # Select the service
      AnomalyDetection = ArgsList$FE_H2OAutoEncoderScoring$AnomalyDetection,
      DimensionReduction = ArgsList$FE_H2OAutoEncoderScoring$DimensionReduction,

      # Data related args
      data = ScoringData.,
      Features = ArgsList$FE_H2OAutoEncoderScoring$Features,
      per_feature = ArgsList$FE_H2OAutoEncoderScoring$per_feature,
      RemoveFeatures = ArgsList$FE_H2OAutoEncoderScoring$RemoveFeatures,
      ModelObject = NULL,
      ModelID = ArgsList$FE_H2OAutoEncoderScoring$ModelID,
      model_path = ArgsList$FE_H2OAutoEncoderScoring$model_path,

      # H2O args
      NThreads = ArgsList$FE_H2OAutoEncoderScoring$NThreads,
      MaxMem = ArgsList$FE_H2OAutoEncoderScoring$MaxMem,
      H2OStart = ArgsList$FE_H2OAutoEncoderScoring$H2OStart,
      H2OShutdown = ArgsList$FE_H2OAutoEncoderScoring$H2OShutdown,
      ReturnLayer = 4L)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$H2OAutoEncoder_Scoring <- difftime(End, Start, units = "mins")
  }

  # Return
  return(list(TrainData = TrainData., ValidationData = ValidationData., TestData = TestData., ScoringData = ScoringData., ArgsList = ArgsList))
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
    TrainData. <- RemixAutoML::H2OIsolationForest(
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
      ValidationData. <- RemixAutoML::H2OIsolationForestScoring(
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
      TestData. <- RemixAutoML::H2OIsolationForestScoring(
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
    ScoringData. <- RemixAutoML::H2OIsolationForestScoring(
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
    TrainData. <- RemixAutoML::AutoClustering(
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
      ValidationData. <- RemixAutoML::AutoClusteringScoring(
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
      TestData. <- RemixAutoML::AutoClusteringScoring(
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
    ScoringData. <- RemixAutoML::AutoClusteringScoring(
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
