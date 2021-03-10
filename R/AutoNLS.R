#' @title AutoNLS
#'
#' @description This function will build models for 9 different nls models, along with a non-parametric monotonic regression and a polynomial regression. The models are evaluated, a winner is picked, and the predicted values are stored in your data table.
#'
#' @author Adrian Antico
#' @family Automated Supervised Learning - Regression
#'
#' @param data Data is the data table you are building the modeling on
#' @param y Y is the target variable name in quotes
#' @param x X is the independent variable name in quotes
#' @param monotonic This is a TRUE/FALSE indicator - choose TRUE if you want monotonic regression over polynomial regression
#' @examples
#' \dontrun{
#' # Create Growth Data
#' data <- data.table::data.table(Target = seq(1, 500, 1),
#'   Variable = rep(1, 500))
#' for (i in as.integer(1:500)) {
#'   if (i == 1) {
#'     var <- data[i, "Target"][[1]]
#'     data.table::set(data, i = i, j = 2L,
#'       value = var * (1 + runif(1) / 100))
#'   } else {
#'     var <- data[i - 1, "Variable"][[1]]
#'     data.table::set(data, i = i, j = 2L,
#'       value = var * (1 + runif(1) / 100))
#'   }
#' }
#'
#' # Add jitter to Target
#' data[, Target := jitter(Target, factor = 0.25)]
#'
#' # To keep original values
#' data1 <- data.table::copy(data)
#'
#' # Merge and Model data
#' data11 <- AutoNLS(
#'   data = data,
#'   y = "Target",
#'   x = "Variable",
#'   monotonic = TRUE)
#'
#' # Join predictions to source data
#' data2 <- merge(
#'   data1,
#'   data11$PredictionData,
#'   by = "Variable",
#'   all = FALSE)
#'
#' # Plot output
#' ggplot2::ggplot(data2, ggplot2::aes(x = Variable)) +
#'   ggplot2::geom_line(ggplot2::aes(y = data2[["Target.x"]],
#'                                   color = "Target")) +
#'   ggplot2::geom_line(ggplot2::aes(y = data2[["Target.y"]],
#'                                   color = "Predicted")) +
#'  RemixAutoML::ChartTheme(Size = 12) +
#'   ggplot2::ggtitle(paste0("Growth Models AutoNLS: ",
#'     data11$ModelName)) +
#'   ggplot2::ylab("Target Variable") +
#'   ggplot2::xlab("Independent Variable") +
#'   ggplot2::scale_colour_manual("Values",
#'     breaks = c("Target", "Predicted"),
#'     values = c("red", "blue"))
#'
#' summary(data11$ModelObject)
#' data11$EvaluationMetrics
#' }
#' @return A list containing "PredictionData" which is a data table with your original column replaced by the nls model predictions; "ModelName" the model name; "ModelObject" The winning model to later use; "EvaluationMetrics" Model metrics for models with ability to build.
#' @export
AutoNLS <- function(data,
                    y,
                    x,
                    monotonic = TRUE) {

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Begin ----
  DATA <- data
  nls_collection <- data.table::data.table(
    ModelName = c("Poly","Asymp","AsympOff","AsympOrig","Biexp","FourParmLog","Gompertz","Logistic","Michal_Menton","Weilbull"),
    MeanAbsError = rep(999, 10))

  # Convert to data.table if not already ----
  if(!data.table::is.data.table(DATA)) data.table::setDT(DATA)
  data.table::setnames(DATA, c(eval(y), eval(x)), c("Target", "Variable"))
  z <- DATA[["Variable"]]
  zz <- DATA[["Target"]]
  tryCatch({
    if(monotonic == TRUE) {
      tryCatch({
        baseline <- monreg::monreg(z, zz, hr = 0.5, hd = 0.5)
        preds <- baseline$estimation
        preds[preds < 0] <- 0
        val0 <- mean(abs(zz - preds))
        data.table::set(nls_collection, 1L, 2L, value = val0)
      }, error = function(x) return("skip"))
    } else {
      tryCatch({
        baseline <- stats::lm(as.formula(Target ~ poly(Variable, 5)), data = DATA)
        preds <- baseline$fitted.values
        preds[preds < 0] <- 0
        val0 <- mean(abs(zz - preds))
        data.table::set(nls_collection, 1L, 2L, value = val0)
      }, error = function(x) return("skip"))
    }
  }, error = function(x) return("skip"))

  # Asymp model ----
  tryCatch({
    model1 <- stats::nls(Target ~ SSasymp(Variable, Asym, R0, lrc), data = DATA)
    preds1 <- stats::fitted(model1, DATA)
    preds1[preds1 < 0] <- 0
    val <- mean(abs(zz - preds1))
    data.table::set(nls_collection, 2L, 2L, value = val)
  }, error = function(x) return("skip"))

  # Asymp offset model ----
  tryCatch({
    model2 <- stats::nls(Target ~ SSasympOff(Variable, Asym, lrc, c0), data = DATA)
    preds2 <- stats::fitted(model2, DATA)
    preds2[preds2 < 0] <- 0
    val2 <- mean(abs(zz - preds2))
    data.table::set(nls_collection, 3L, 2L, value = val2)
  }, error = function(x) return("skip"))

  # Asymp origin model ----
  tryCatch({
    model3 <- stats::nls(Target ~ SSasympOrig(Variable, Asym, lrc), data = DATA)
    preds3 <- stats::fitted(model3, DATA)
    preds3[preds3 < 0] <- 0
    val3 <- mean(abs(zz - preds3))
    data.table::set(nls_collection, 4L, 2L, value = val3)
  }, error = function(x) return("skip"))

  # Biexp model ----
  tryCatch({
    model4 <- stats::nls(Target ~ SSbiexp(Variable, A1, lrc1, A2, lrc2), data = DATA)
    preds4 <- stats::fitted(model4, DATA)
    preds4[preds4 < 0] <- 0
    val4 <- mean(abs(zz - preds4))
    data.table::set(nls_collection, 5L, 2L, value = val4)
  }, error = function(x) return("skip"))

  # Four parameter logistic model ----
  tryCatch({
    model5 <- stats::nls(Target ~ SSfpl(Variable, A, B, xmid, scal), data = DATA)
    preds5 <- stats::fitted(model5, DATA)
    preds5[preds5 < 0] <- 0
    val5 <- mean(abs(zz - preds5))
    data.table::set(nls_collection, 6L, 2L, value = val5)
  }, error = function(x) return("skip"))

  # Gompertz model ----
  tryCatch({
    model6 <- stats::nls(Target ~ SSgompertz(Variable, Asym, b2, b3), data = DATA)
    preds6 <- stats::fitted(model6, DATA)
    preds6[preds6 < 0] <- 0
    val6 <- mean(abs(zz - preds6))
    data.table::set(nls_collection, 7L, 2L, value = val6)
  }, error = function(x) return("skip"))

  # Logistic model ----
  tryCatch({
    model7 <- stats::nls(Target ~ SSlogis(Variable, Asym, xmid, scal), data = DATA)
    preds7 <- stats::fitted(model7, DATA)
    preds7[preds7 < 0] <- 0
    val7 <- mean(abs(zz - preds7))
    data.table::set(nls_collection, 8L, 2L, value = val7)
  }, error = function(x) return("skip"))

  # Michaelis-Menton model ----
  tryCatch({
    model8 <- stats::nls(Target ~ SSmicmen(Variable, Vm, K), data = DATA)
    preds8 <- stats::fitted(model8, DATA)
    preds8[preds8 < 0] <- 0
    val8 <- mean(abs(zz - preds8))
    data.table::set(nls_collection, 9L, 2L, value = val8)
  }, error = function(x) return("skip"))

  # Weibull Growth model ----
  tryCatch({
    model9 <- stats::nls(Target ~ SSweibull(Variable, Asym, Drop, lrc, pwr), data = DATA)
    preds9 <- stats::fitted(model9, DATA)
    preds9[preds9 < 0] <- 0
    val9 <- mean(abs(zz - preds9))
    data.table::set(nls_collection, 10L, 2L, value = val9)
  }, error = function(x) return("skip"))

  # Store best model name ----
  name <- nls_collection[MeanAbsError != 999][order(MeanAbsError)][1, 1][[1]]

  # Collect metrics for all models fitted ----
  temp <- nls_collection[MeanAbsError != 999][order(MeanAbsError)]

  # Create column using best model ----
  if (name == nls_collection[10, 1][[1]]) {
    DATA[, Target := preds9]
    data.table::setnames(DATA, c("Target", "Variable"), c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model9,
        EvaluationMetrics = temp))
  } else if (name == nls_collection[2L, 1L][[1L]]) {
    DATA[, Target := preds1]
    data.table::setnames(DATA, c("Target", "Variable"), c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model1,
        EvaluationMetrics = temp))
  } else if (name == nls_collection[3L, 1L][[1L]]) {
    DATA[, Target := preds2]
    data.table::setnames(DATA, c("Target", "Variable"), c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model2,
        EvaluationMetrics = temp))
  } else if (name == nls_collection[4L, 1L][[1L]]) {
    DATA[, Target := preds3]
    data.table::setnames(DATA, c("Target", "Variable"), c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model3,
        EvaluationMetrics = temp))
  } else if (name == nls_collection[5L, 1L][[1L]]) {
    DATA[, Target := preds4]
    data.table::setnames(DATA, c("Target", "Variable"), c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model4,
        EvaluationMetrics = temp))
  } else if (name == nls_collection[6L, 1L][[1L]]) {
    DATA[, Target := preds5]
    data.table::setnames(DATA, c("Target", "Variable"), c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model5,
        EvaluationMetrics = temp))
  } else if (name == nls_collection[7L, 1L][[1L]]) {
    DATA[, Target := preds6]
    data.table::setnames(DATA, c("Target", "Variable"), c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model6,
        EvaluationMetrics = temp))
  } else if (name == nls_collection[8L, 1L][[1L]]) {
    DATA[, Target := preds7]
    data.table::setnames(DATA, c("Target", "Variable"), c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model7,
        EvaluationMetrics = temp))
  } else if (name == nls_collection[9L, 1L][[1L]]) {
    DATA[, Target := preds8]
    data.table::setnames(DATA, c("Target", "Variable"), c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model8,
        EvaluationMetrics = temp))
  } else {
    DATA[, Target := preds]
    data.table::setnames(DATA, c("Target", "Variable"), c(eval(y), eval(x)))
    if (monotonic) {
      name <- "Monotonic Regression"
    } else {
      name <- "Polynomial Regression"
    }
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = baseline,
        EvaluationMetrics = temp))
  }
}
