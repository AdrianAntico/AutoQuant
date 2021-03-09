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

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Ensure data is a data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Two processes----
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
    Final[, Text := gsub("  ", " ", Text)]
    Final[, Text := stringr::str_replace_all(Text, "[[:punct:]]", "")]
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
      data[, eval(string) := gsub("  ", " ", get(string))]
      data[, eval(string) := stringr::str_replace_all(get(string), "[[:punct:]]", "")]
      data2 <- data[, .(get(string))]

      # Tokenize ----
      tokenized_words <- tokenizeH2O(data2)
      rm(data2)

      # Score model ----
      all_vecs <- h2o::h2o.transform(w2v.model, tokenized_words, aggregate_method = "AVERAGE")

      # Convert to data.table ----
      all_vecs <- data.table::as.data.table(all_vecs)
      data <- data.table::data.table(cbind(data, all_vecs))

      # Remove string cols ----
      if(!KeepStringCol) data[, eval(string) := NULL]

      # Replace Colnames ----
      cols <- names(data)[(ncol(data) - vects + 1):ncol(data)]
      for(c in cols) {
        data[, paste0(string, "_", c) := get(c)]
        data[, eval(c) := NULL]
      }
    }

    # Final Prep
    h2o::h2o.rm(w2v.model)
    h2o::h2o.shutdown(prompt = FALSE)

  } else {

    # Create storage file----
    N <- length(stringCol)
    StoreFile <- data.table::data.table(ModelName = rep("a", N), Path = rep("a", N), Jar = rep("a", N))
    i <- 0L
    for(string in stringCol) {
      if(!is.character(data[[eval(string)]])) data[, eval(string) := as.character(get(string))]
      i <- i + 1L
      Sys.sleep(10L)
      h2o::h2o.init(nthreads = Threads, max_mem_size = MaxMemory)

      # It is important to remove "\n" --
      data[, eval(string) := gsub("  ", " ", get(string))]
      data[, eval(string) := stringr::str_replace_all(get(string), "[[:punct:]]", "")]
      data2 <- data[, .(get(string))]

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
      data <- data.table::data.table(cbind(data, all_vecs))

      # Remove string cols ----
      if(!KeepStringCol) data[, eval(string) := NULL]

      # Replace Colnames ----
      cols <- names(data[, (ncol(data) - vects + 1):ncol(data)])
      for(c in cols) {
        data[, paste0(string, "_", c) := get(c)]
        data[, eval(c) := NULL]
      }

      # Final Prep ----
      h2o::h2o.rm(w2v.model)
      h2o::h2o.shutdown(prompt = FALSE)
    }
  }

  # Return data ----
  return(data)
}
