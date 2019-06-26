#' H2OMultinomialAUC computes the micro auc from a multinomial model
#'
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @noRd
#' @param validate the data set to run the micro auc on
#' @param best_model the model object you wish to test
#' @param targetColNum the column number of the target variable
#' @param targetName the name, in quotes, of the target variable
#' @examples
#' \donttest{
#' auc_val <- H2OMultinomialAUC(validate, best_model, targetColNum = 1, targetName = "TargetVar")
#' }
#' @return Micro AUC
H2OMultinomialAUC <-
  function(validate,
           best_model,
           targetColNum = 1,
           targetName = "TargetVar") {
    xx <-
      data.table::as.data.table(h2o::h2o.cbind(validate[, targetColNum],
                                               h2o::h2o.predict(best_model,
                                                                newdata = validate)))
    xx[, predict := as.character(predict)]
    xx[, vals := 0.5]
    z <- ncol(xx)
    col <- targetName
    for (l in seq_len(nrow(xx))) {
      cols <- xx[l, get(col)][[1]]
      valss <- xx[l, ..cols][[1]]
      data.table::set(xx, l, j = z, value = valss)
    }
    return(round(as.numeric(noquote(
      stringr::str_extract(
        pROC::multiclass.roc(xx$target, xx$vals)$auc,
        "\\d+\\.*\\d*"
      )
    )), 4))
  }