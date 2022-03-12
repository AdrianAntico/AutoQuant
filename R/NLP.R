

#' @title Keras_text2seq_pad
#'
#' @description return padded text to sequence keras object
#'
#' @author Adrian Antico
#' @family NLP
#'
#' @param Tokenize tokenizer <- keras::text_tokenizer() %>% keras::fit_text_tokenizer(text)
#' @param Text text character vector
#' @param Pad_Max_Len is the max length for padding in keras::pad_sequences()
#'
#' @noRd
Keras_t2s_pad <- function(Tokenizer = NULL,
                          Text = NULL,
                          Pad_Max_Len = 150) {
  keras::pad_sequences(
    sequences = keras::texts_to_sequences(tokenizer = Tokenizer, Text),
    maxlen    = Pad_Max_Len)
}
