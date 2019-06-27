#' Automated word2vec data generation via H2O
#'
#' This function allows you to automatically build a word2vec model and merge the data onto your supplied dataset
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data Source data table to merge vects onto
#' @param stringCol A string name for the column to convert via word2vec
#' @param KeepStringCol Set to TRUE if you want to keep the original string column that you convert via word2vec
#' @param model_path A string path to the location where you want the model and metadata stored
#' @param vects The number of vectors to retain from the word2vec model
#' @param SaveStopWords Set to TRUE to save the stop words used
#' @param MinWords For H2O word2vec model
#' @param WindowSize For H2O word2vec model
#' @param Epochs For H2O word2vec model
#' @param StopWords For H2O word2vec model
#' @param SaveModel Set to "standard" to save normally; set to "mojo" to save as mojo. NOTE: while you can save a mojo, I haven't figured out how to score it in the AutoH20Scoring function.
#' @param Threads Number of available threads you want to dedicate to model building
#' @param MaxMemory Amount of memory you want to dedicate to model building
#' @param SaveOutput Set to TRUE to save your models to file
#' @examples
#' \donttest{
#' data <- AutoWord2VecModeler(data,
#'                             BuildType = "individual",
#'                             stringCol = c("Text_Col1",
#'                                           "Text_Col2"),
#'                             KeepStringCol = FALSE,
#'                             model_path = NULL,
#'                             vects = 100,
#'                             SaveStopWords = FALSE,
#'                             MinWords = 1,
#'                             WindowSize = 1,
#'                             Epochs = 25,
#'                             StopWords = NULL,
#'                             SaveModel = "standard",
#'                             Threads = max(1,parallel::detectCores()-2),
#'                             MaxMemory = "28G",
#'                             SaveOutput = TRUE)
#'}
#' @export
AutoWord2VecModeler <- function(data,
                                BuildType     = "Combined",
                                stringCol     = c("Text_Col1",
                                                  "Text_Col2"),
                                KeepStringCol = FALSE,
                                model_path    = NULL,
                                vects         = 100,
                                SaveStopWords = FALSE,
                                MinWords      = 1,
                                WindowSize    = 12,
                                Epochs        = 25,
                                StopWords     = NULL,
                                SaveModel     = "standard",
                                Threads       = max(1, parallel::detectCores() - 2),
                                MaxMemory     = "28G",
                                SaveOutput    = FALSE) {
  
  # Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Two processes----
  if(tolower(BuildType) == "combined") {
    
    # Create storage file----
    N <- length(stringCol)
    StoreFile <-
      data.table::data.table(
        ModelName = rep("a", 1),
        Path = rep("a", 1),
        Jar = rep("a", 1)
      )
    i <- 0
    
    # Loop through all the string columns----
    for (string in stringCol) {
      
      # Increment----
      i <- as.integer(i + 1)
      
      # Ensure stringCol is character (not factor)----
      if (!is.character(data[[eval(string)]])) {
        data[, eval(string) := as.character(get(string))]
      }
      
      # Build single column----
      if(i == 1) {
        Final <- data[, .(get(string))]
        data.table::setnames(Final, "V1", "Text")
      } else {
        temp <- data[, .(get(string))]
        data.table::setnames(temp, "V1", "Text")
        Final <- data.table::rbindlist(
          list(Final, temp))
      }
    }
    
    # Remove Temp
    rm(temp)
    
    # word2vec time----
    Sys.sleep(10)
    h2o::h2o.init(nthreads = Threads, max_mem_size = MaxMemory)
    
    # It is important to remove "\n"----
    Final[, Text := gsub("  ", " ", Text)]
    Final[, Text := stringr::str_replace_all(Text, "[[:punct:]]", "")]
    Final <- Final[, .(Text)]
    
    # Tokenize----
    tokenized_words <- tokenizeH2O(Final)
    
    # Build model----
    w2v.model <- h2o::h2o.word2vec(
      tokenized_words,
      model_id           = string,
      word_model         = "SkipGram",
      norm_model         = "HSM",
      vec_size           = vects,
      min_word_freq      = MinWords,
      window_size        = WindowSize,
      init_learning_rate = 0.025,
      sent_sample_rate   = 0.05,
      epochs             = Epochs
    )
    
    # Save model----
    if (SaveOutput) {
      if (tolower(SaveModel) == "standard") {
        w2vPath <-
          h2o::h2o.saveModel(w2v.model, path = model_path, force = TRUE)
        data.table::set(StoreFile,
                        i = 1L,
                        j = 1L,
                        value = string)
        data.table::set(StoreFile,
                        i = 1L,
                        j = 2L,
                        value = w2vPath)
        data.table::set(StoreFile,
                        i = 1L,
                        j = 3L,
                        value = "NA")
        save(StoreFile, file = paste0(model_path, "/StoreFile.Rdata"))
      } else {
        w2vPath <-
          h2o::h2o.saveMojo(w2v.model, path = model_path, force = TRUE)
        h2o::h2o.download_mojo(
          model = w2v.model,
          path = model_path,
          get_genmodel_jar = TRUE,
          genmodel_path = model_path,
          genmodel_name = string
        )
        data.table::set(StoreFile,
                        i = 1L,
                        j = 1L,
                        value = string)
        data.table::set(StoreFile,
                        i = 1L,
                        j = 2L,
                        value = w2vPath)
        data.table::set(
          StoreFile,
          i = 1L,
          j = 3L,
          value = paste0(model_path, "/", string)
        )
        save(StoreFile, file = paste0(model_path, "/StoreFile.Rdata"))
      }
    }
    
    # Loop through all the string columns and score them----
    for (string in stringCol) {
      # Ensure stringCol is character (not factor)
      if (!is.character(data[[eval(string)]])) {
        data[, eval(string) := as.character(get(string))]
      }
      
      # word2vec time
      i <- as.integer(i + 1)
      Sys.sleep(10)
      h2o::h2o.init(nthreads = Threads, max_mem_size = MaxMemory)
      
      # It is important to remove "\n" --
      data[, eval(string) := gsub("  ", " ", get(string))]
      data[, eval(string) := stringr::str_replace_all(get(string), "[[:punct:]]", "")]
      data2 <- data[, .(get(string))]
      
      # Tokenize
      tokenized_words <- tokenizeH2O(data2)
      rm(data2)
      
      # Score model----
      all_vecs <-
        h2o::h2o.transform(w2v.model, tokenized_words,
                           aggregate_method = "AVERAGE")
      
      # Convert to data.table----
      all_vecs <- data.table::as.data.table(all_vecs)
      data <- data.table::data.table(cbind(data, all_vecs))
      
      # Remove string cols----
      if (!KeepStringCol) {
        data[, eval(string) := NULL]
      }
    }
    
    # Replace Colnames----
    cols <- names(data)[(ncol(data) - vects + 1):ncol(data)]
    for (c in cols) {
      data[, paste0(string, "_", c) := get(c)]
      data[, eval(c) := NULL]
    }
    
    # Final Prep
    h2o::h2o.rm(w2v.model)
    h2o::h2o.shutdown(prompt = FALSE)
    
  } else {
    
    # Create storage file----
    N <- length(stringCol)
    StoreFile <-
      data.table::data.table(
        ModelName = rep("a", N),
        Path = rep("a", N),
        Jar = rep("a", N)
      )
    i <- 0
    
    # Loop through all the string columns
    for (string in stringCol) {
      # Ensure stringCol is character (not factor)
      if (!is.character(data[[eval(string)]])) {
        data[, eval(string) := as.character(get(string))]
      }
      
      # word2vec time
      i <- as.integer(i + 1)
      Sys.sleep(10)
      h2o::h2o.init(nthreads = Threads, max_mem_size = MaxMemory)
      
      # It is important to remove "\n" --
      data[, eval(string) := gsub("  ", " ", get(string))]
      data[, eval(string) := stringr::str_replace_all(get(string), "[[:punct:]]", "")]
      data2 <- data[, .(get(string))]
      
      # Tokenize
      tokenized_words <- tokenizeH2O(data2)
      rm(data2)
      
      # Build model
      w2v.model <- h2o::h2o.word2vec(
        tokenized_words,
        model_id           = string,
        word_model         = "SkipGram",
        norm_model         = "HSM",
        vec_size           = vects,
        min_word_freq      = MinWords,
        window_size        = WindowSize,
        init_learning_rate = 0.025,
        sent_sample_rate   = 0.05,
        epochs             = Epochs
      )
      
      # Save model
      if (SaveOutput) {
        if (tolower(SaveModel) == "standard") {
          w2vPath <-
            h2o::h2o.saveModel(w2v.model, path = model_path, force = TRUE)
          data.table::set(StoreFile,
                          i = i,
                          j = 1L,
                          value = string)
          data.table::set(StoreFile,
                          i = i,
                          j = 2L,
                          value = w2vPath)
          data.table::set(StoreFile,
                          i = i,
                          j = 3L,
                          value = "NA")
          save(StoreFile, file = paste0(model_path, "/StoreFile.Rdata"))
        } else {
          w2vPath <-
            h2o::h2o.saveMojo(w2v.model, path = model_path, force = TRUE)
          h2o::h2o.download_mojo(
            model = w2v.model,
            path = model_path,
            get_genmodel_jar = TRUE,
            genmodel_path = model_path,
            genmodel_name = string
          )
          data.table::set(StoreFile,
                          i = i,
                          j = 1L,
                          value = string)
          data.table::set(StoreFile,
                          i = i,
                          j = 2L,
                          value = w2vPath)
          data.table::set(
            StoreFile,
            i = i,
            j = 3L,
            value = paste0(model_path, "/", string)
          )
          save(StoreFile, file = paste0(model_path, "/StoreFile.Rdata"))
        }
      }
      
      # Score model
      all_vecs <-
        h2o::h2o.transform(w2v.model, tokenized_words,
                           aggregate_method = "AVERAGE")
      
      # Convert to data.table
      all_vecs <- data.table::as.data.table(all_vecs)
      data <- data.table::data.table(cbind(data, all_vecs))
      
      # Remove string cols
      if (!KeepStringCol) {
        data[, eval(string) := NULL]
      }
      
      # Replace Colnames
      cols <- names(data[, (ncol(data) - vects + 1):ncol(data)])
      for (c in cols) {
        data[, paste0(string, "_", c) := get(c)]
        data[, eval(c) := NULL]
      }
      
      # Final Prep
      h2o::h2o.rm(w2v.model)
      h2o::h2o.shutdown(prompt = FALSE)
    }
  }
  return(data) 
}
