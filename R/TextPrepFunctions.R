# AutoQuant is a package for quickly creating high quality visualizations under a common and easy api.
# Copyright (C) <year>  <name of author>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
