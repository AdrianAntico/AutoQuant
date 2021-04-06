#' For NLP work
#'
#' This function tokenizes text data
#' @author Adrian Antico
#' @family Misc
#' @param data The text data
#' @examples
#' \dontrun{
#' data <- tokenizeH2O(data = data[["StringColumn"]])
#' }
#' @noRd
tokenizeH2O <- function(data) {
  data <- h2o::as.h2o(data, col.types = c("String"))
  tokenized <- h2o::h2o.tokenize(data, "\\\\W+")
  tokenized.lower <- h2o::h2o.tolower(tokenized)
  tokenized.words <- tokenized.lower[h2o::h2o.grep("[0-9]", tokenized.lower, invert = TRUE, output.logical = TRUE),]
  tokenized.words
}

#' Automated Word Frequency and Word Cloud Creation
#'
#' This function builds a word frequency table and a word cloud. It prepares data, cleans text, and generates output.
#' @author Adrian Antico
#' @family EDA
#' @param data Source data table
#' @param TextColName A string name for the column
#' @param GroupColName Set to NULL to ignore, otherwise set to Cluster column name (or factor column name)
#' @param GroupLevel Must be set if GroupColName is defined. Set to cluster ID (or factor level)
#' @param RemoveEnglishStopwords Set to TRUE to remove English stop words, FALSE to ignore
#' @param Stemming Set to TRUE to run stemming on your text data
#' @param StopWords Add your own stopwords, in vector format
#' @examples
#' \dontrun{
#' data <- data.table::data.table(
#' DESCR = c(
#'   "Gru", "Gru", "Gru", "Gru", "Gru", "Gru", "Gru",
#'   "Gru", "Gru", "Gru", "Gru", "Gru", "Gru", "Urkle",
#'   "Urkle", "Urkle", "Urkle", "Urkle", "Urkle", "Urkle",
#'   "Gru", "Gru", "Gru", "bears", "bears", "bears",
#'   "bears", "bears", "bears", "smug", "smug", "smug", "smug",
#'   "smug", "smug", "smug", "smug", "smug", "smug",
#'   "smug", "smug", "smug", "smug", "smug", "eats", "eats",
#'   "eats", "eats", "eats", "eats", "beats", "beats", "beats", "beats",
#'   "beats", "beats", "beats", "beats", "beats", "beats",
#'   "beats", "science", "science", "Dwigt", "Dwigt", "Dwigt", "Dwigt",
#'   "Dwigt", "Dwigt", "Dwigt", "Dwigt", "Dwigt", "Dwigt",
#'   "Schrute", "Schrute", "Schrute", "Schrute", "Schrute",
#'   "Schrute", "Schrute", "James", "James", "James", "James",
#'   "James", "James", "James", "James", "James", "James",
#'   "Halpert", "Halpert", "Halpert", "Halpert",
#'   "Halpert", "Halpert", "Halpert", "Halpert"))
#' data <- AutoWordFreq(
#'   data,
#'   TextColName = "DESCR",
#'   GroupColName = NULL,
#'   GroupLevel = NULL,
#'   RemoveEnglishStopwords = FALSE,
#'   Stemming = FALSE,
#'   StopWords = c("Bla"))
#' }
#' @export
AutoWordFreq <- function(data,
                         TextColName = "DESCR",
                         GroupColName = "ClusterAllNoTarget",
                         GroupLevel = 0,
                         RemoveEnglishStopwords = TRUE,
                         Stemming = TRUE,
                         StopWords = c("bla",
                                       "bla2")) {
  # Check data.table
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Ensure stringCol is character (not factor)
  if(!is.character(data[[eval(TextColName)]])) data[, eval(TextColName) := as.character(get(TextColName))]

  # Prepare data
  if(is.null(GroupColName)) {
    desc <- tm::Corpus(tm::VectorSource(data[[eval(TextColName)]]))
  } else {
    if(!is.character(data[[GroupColName]])) {
      data[, eval(GroupColName) := as.character(get(GroupColName))]
      desc <- tm::Corpus(tm::VectorSource(data[get(GroupColName) == eval(GroupLevel)][[eval(TextColName)]]))
    }
  }

  # Clean text
  toSpace <- tm::content_transformer(function(x , pattern) gsub(pattern, " ", x))
  text <- tm::tm_map(desc, toSpace, "/")
  text <- tm::tm_map(text, toSpace, "@")
  text <- tm::tm_map(text, toSpace, "\\|")

  # Convert the text to lower case
  text <- tm::tm_map(text, tm::content_transformer(tolower))

  # Remove numbers
  text <- tm::tm_map(text, tm::removeNumbers)

  # Remove english common stopwords
  if(RemoveEnglishStopwords)
    text <- tm::tm_map(text, tm::removeWords, tm::stopwords("english"))

  # specify your stopwords as a character vector
  text <- tm::tm_map(text, tm::removeWords, StopWords)

  # Remove punctuations
  text <- tm::tm_map(text, tm::removePunctuation)

  # Eliminate extra white spaces
  text <- tm::tm_map(text, tm::stripWhitespace)

  # Text stemming
  if(Stemming) text <- tm::tm_map(text, tm::stemDocument)

  # Finalize
  dtm <- tm::TermDocumentMatrix(text)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.table::data.table(word = names(v), freq = v)
  print(head(d, 10))

  # Word Cloud
  print(
    wordcloud::wordcloud(
      words = d$word,
      freq = d$freq,
      min.freq = 1,
      max.words = 200,
      random.order = FALSE,
      rot.per = 0.35,
      colors = RColorBrewer::brewer.pal(8, "Dark2")))

  # Return
  return(d)
}

#' AutoH2OTextPrepScoring is for NLP scoring
#'
#' This function returns prepared tokenized data for H2O Word2VecModeler scoring
#' @author Adrian Antico
#' @family Misc
#' @param data The text data
#' @param string The name of the string column to prepare
#' @param MaxMem Amount of memory you want to let H2O utilize
#' @param NThreads The number of threads you want to let H2O utilize
#' @param StartH2O Set to TRUE to have H2O start inside this function
#' @examples
#' \dontrun{
#' data <- AutoH2OTextPrepScoring(data = x,
#'                                string = "text_column",
#'                                MaxMem = "28G",
#'                                NThreads = 8,
#'                                StartH2O = TRUE)
#' }
#' @noRd
AutoH2OTextPrepScoring <- function(data,
                                   string = NULL,
                                   MaxMem = NULL,
                                   NThreads = NULL,
                                   StartH2O = TRUE) {
  # Ensure data.table
  if(!is.data.table(data)) data.table::setDT((data))
  data[, eval(string) := as.character(get(string))]
  if(StartH2O) h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem)

  # It is important to remove "\n"
  data[, eval(string) := gsub(pattern = "[[:punct:][:blank:]]", replacement = " ", x = data[[eval(string)]])]
  data2 <- data[, list(get(string))]

  # Tokenize
  tokenized_words <- tokenizeH2O(data2)
  return(tokenized_words)
}
