#' Test YeoJohnson Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param eps erorr tolerance
#' @param ... Arguments to pass along
#' @return YeoJohnson results
Test_YeoJohnson <- function(x, 
                            eps = 0.001, 
                            ...) {
  stopifnot(is.numeric(x))
  lambda <- Estimate_YeoJohnson_Lambda(x, eps = eps, ...)
  trans_data <- x
  na_idx <- is.na(x)
  trans_data[!na_idx] <- Apply_YeoJohnson(x[!na_idx], lambda, eps)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(
    Name = "YeoJohnson",
    Data = trans_data,
    Lambda = lambda,
    Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Estimate YeoJohnson Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lower the lower bound for search
#' @param upper the upper bound for search
#' @param eps erorr tolerance
#' @return YeoJohnson results
Estimate_YeoJohnson_Lambda <- function(x,
                                       lower = -5,
                                       upper = 5,
                                       eps = 0.001) {
    
  n <- length(x)
  ccID <- !is.na(x)
  x <- x[ccID]
  
  # See references, Yeo & Johnson Biometrika (2000)
  yj_loglik <- function(lambda) {
    x_t <- Apply_YeoJohnson(x, lambda, eps)
    x_t_bar <- mean(x_t)
    x_t_var <- var(x_t) * (n - 1) / n
    constant <- sum(sign(x) * log(abs(x) + 1)) - 0.5 * n * log(x_t_var) + (lambda - 1) * constant
  }
  
  results <- optimize(
    yj_loglik,
    lower = lower,
    upper = upper,
    maximum = TRUE,
    tol = .0001)
    results$maximum
  }

#' Apply YeoJohnson Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lambda optimal lambda
#' @param eps erorr tolerance
#' @return YeoJohnson results
Apply_YeoJohnson <- function(x, 
                             lambda, 
                             eps = 0.001) {
  pos_idx <- x >= 0
  neg_idx <- x < 0
  
  # Transform negative values
  if(any(pos_idx)) {
    if(abs(lambda) < eps) {
      x[pos_idx] <- log(x[pos_idx] + 1)
    } else {
      x[pos_idx] <- ((x[pos_idx] + 1) ^ lambda - 1) / lambda
    }
  }
  
  # Transform nonnegative values
  if(any(neg_idx)) {
    if(abs(lambda - 2) < eps) {
      x[neg_idx] <- -log(-x[neg_idx] + 1)
    } else {
      x[neg_idx] <- -((-x[neg_idx] + 1) ^ (2 - lambda) - 1) / (2 - lambda)
    }
  }
  x
}

#' Inverse YeoJohnson Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lambda optimal lambda
#' @param eps erorr tolerance
#' @return YeoJohnson results
InvApply_YeoJohnson <- function(x, 
                                lambda, 
                                eps = 0.001) {
  val <- x
  neg_idx <- x < 0
  
  if(any(!neg_idx)) {
    if(abs(lambda) < eps) {
      val[!neg_idx] <- exp(x[!neg_idx]) - 1
    } else {
      val[!neg_idx] <- (x[!neg_idx] * lambda + 1) ^ (1 / lambda) - 1
    }
  }
  if(any(neg_idx)) {
    if(abs(lambda - 2) < eps) {
      val[neg_idx] <- -expm1(-x[neg_idx])
    } else {
      val[neg_idx] <- 1 - (-(2 - lambda) * x[neg_idx] + 1) ^ (1 / (2 - lambda))
    }
  }
  val
}

#' Test BoxCox Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param ... Arguments to pass along
#' @return BoxCox results
Test_BoxCox <- function(x, ...) {
  stopifnot(is.numeric(x))
  lambda <- Estimate_BoxCox_Lambda(x, ...)
  trans_data <- Apply_BoxCox(x, lambda)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(
    Name = "BoxCox",
    Data = trans_data,
    Lambda = lambda,
    Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Estimate BoxCox Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lower the lower bound for search
#' @param upper the upper bound for search
#' @param eps erorr tolerance
#' @return BoxCox results
Estimate_BoxCox_Lambda <- function(x,
                                   lower = -1,
                                   upper = 2,
                                   eps = 0.001) {
  
  # Kick off
  n <- length(x)
  ccID <- !is.na(x)
  x <- x[ccID]
  if(any(x <= 0)) return("x must be positive")
  log_x <- log(x)
  xbar <- exp(mean(log_x))
  fit <- lm(x ~ 1, data = data.frame(x = x))
  xqr <- fit$qr
    
  # Define function
  boxcox_loglik <- function(lambda) {
    if(abs(lambda) > eps) {
      xt <- (x ^ lambda - 1) / lambda 
    } else {
      xt <- log_x * (1 + (lambda * log_x) / 2 * (1 + (lambda * log_x) / 3 * (1 + (lambda * log_x) / 4))) - n / 2 * log(sum(qr.resid(xqr, xt / xbar ^ (lambda - 1)) ^ 2))
    }
  }
  
  # Optimize
  results <- optimize(
    boxcox_loglik,
    lower = lower,
    upper = upper,
    maximum = TRUE,
    tol = .0001)
  results$maximum
}

#' Apply BoxCox Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lambda optimal lambda
#' @param eps erorr tolerance
#' @return BoxCox results
Apply_BoxCox <- function(x, 
                         lambda, 
                         eps = 0.001) {
  if(lambda < 0) x[x < 0] <- NA
  if(abs(lambda) < eps) val <- log(x) else val <- (sign(x) * abs(x) ^ lambda - 1) / lambda
  val
}

#' Inverse BoxCox Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lambda optimal lambda
#' @param eps erorr tolerance
#' @return BoxCox results
InvApply_BoxCox <- function(x, 
                            lambda, 
                            eps = 0.001) {
  if(lambda < 0) x[x > -1 / lambda] <- NA
  if(abs(lambda) < eps) {
    val <- exp(x)
  } else {
    x <- x * lambda + 1
    val <- sign(x) * abs(x) ^ (1 / lambda)
  }
  val
}

#' Test Asinh Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asinh results
Test_Asinh <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- asinh(x)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(
    Name = "Asinh",
    Data = trans_data,
    Lambda = NA,
    Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Inverse Asinh Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asinh results
Apply_Asinh <- function(x) return(asinh(x))

#' Inverse Asinh Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asinh results
InvApply_Asinh <- function(x) return(sinh(x))

#' Test Asin Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asin results
Test_Asin <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- asin(sqrt(x))
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(
    Name = "Asin",
    Data = trans_data,
    Lambda = NA,
    Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Inverse Asin Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asin results
Apply_Asin <- function(x) return(asin(sqrt(x)))

#' Inverse Asin Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asin results
InvApply_Asin <- function(x) return(sin(x) ^ 2)

#' Test Logit Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Logit results
Test_Logit <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- log(x / (1 - x))
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(
    Name = "Logit",
    Data = trans_data,
    Lambda = NA,
    Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply Logit Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Logit results
Apply_Logit <- function(x) return(log(x / (1 - x)))

#' Inverse Logit Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Logit results
InvApply_Logit <- function(x) return(1 / (1 + exp(-x)))

#' Test Identity Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Identity results
Test_Identity <- function(x) {
  stopifnot(is.numeric(x))
  x.t <- x
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  x.t <- (x.t - mu) / sigma
  ptest <- nortest::pearson.test(x.t)
  val <- list(
    Name = "Identity",
    Data = x,
    Lambda = NA,
    Normalized_Statistic = unname(ptest$statistic / ptest$df))
  val
}

#' Test Log Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Test_Log <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- log(x)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(
    Name = "Log",
    Data = trans_data,
    Lambda = NA,
    Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply Log Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Apply_Log <- function(x) return(log(x))

#' Inverse Log Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
InvApply_Log <- function(x) return(exp(x))

#' Test LogPlus1 Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return LogPlus1 results
Test_LogPlus1 <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- log(x+1)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(
    Name = "LogPlus1",
    Data = trans_data,
    Lambda = NA,
    Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply LogPlus1 Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Apply_LogPlus1 <- function(x) return(log(x+1))

#' Inverse LogPlus1 Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
InvApply_LogPlus1 <- function(x) return(exp(x)-1)

#' AutoTransformationCreate is a function for automatically identifying the optimal transformations for numeric features and transforming them once identified.
#'
#' AutoTransformationCreate is a function for automatically identifying the optimal transformations for numeric features and transforming them once identified. This function will loop through your selected transformation options (YeoJohnson, BoxCox, Asinh, Asin, and Logit) and find the one that produces data that is the closest to normally distributed data. It then makes the transformation and collects the metadata information for use in the AutoTransformationScore() function, either by returning the objects (always) or saving them to file (optional).
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data This is your source data
#' @param ColumnNames List your columns names in a vector, for example, c("Target", "IV1")
#' @param Methods Choose from "YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Asin", "Logit", and "Identity".
#' @param Path Set to the directly where you want to save all of your modeling files
#' @param TransID Set to a character value that corresponds with your modeling project
#' @param SaveOutput Set to TRUE to save necessary file to run AutoTransformationScore()
#' @return data with transformed columns and the transformation object for back-transforming later
#' @examples
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Adrian = runif(N))
#' data[, x1 := qnorm(Adrian)]
#' data[, x2 := runif(N)]
#' data[, Adrian1 := log(pnorm(Correl * x1 +
#'                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data <- AutoTransformationCreate(data,
#'                                  ColumnNames = "Sample",
#'                                  Methods = c("BoxCox",
#'                                              "YeoJohnson",
#'                                              "Asinh",
#'                                              "Log",
#'                                              "LogPlus1",
#'                                              "Asin",
#'                                              "Logit",
#'                                              "Identity"),
#'                                  Path = NULL,
#'                                  TransID = "Trans",
#'                                  SaveOutput = FALSE)
#' @export
AutoTransformationCreate <- function(data,
                                     ColumnNames = NULL,
                                     Methods = c("BoxCox",
                                                 "YeoJohnson",
                                                 "Asinh",
                                                 "Log",
                                                 "LogPlus1",
                                                 "Asin",
                                                 "Logit",
                                                 "Identity"),
                                     Path = NULL,
                                     TransID = "ModelID",
                                     SaveOutput = FALSE) {
  
  # Check arguments----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  
  # Check arguments----
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!any(tolower(Methods) %chin% c("boxcox", "yeojohnson", "asinh", "log", "logplus1", "asin", "logit"))) return("Methods not supported")
  if(is.numeric(ColumnNames) | is.integer(ColumnNames)) ColumnNames <- names(data)[ColumnNames]
  for(i in ColumnNames) if(!(class(data[[eval(i)]]) %chin% c("numeric", "integer"))) return("ColumnNames must be for numeric or integer columns")
  
  # Loop through ColumnNames----
  for(colNames in as.integer(seq_along(ColumnNames))) {
    
    # Collection Object----
    if(length(Methods) < 5) {
      EvaluationTable <- data.table::data.table(
        ColumnName = rep("BLABLA", length(ColumnNames) * (length(Methods)+1)),
        MethodName = rep("BLABLA", length(ColumnNames) * (length(Methods)+1)),
        Lambda = rep(1.0, length(ColumnNames) * (length(Methods)+1)),
        NormalizedStatistics = rep(1.0, length(ColumnNames) * (length(Methods)+1)))
    } else {
      EvaluationTable <- data.table::data.table(
        ColumnName = rep("BLABLA", length(ColumnNames) * (length(Methods) + 1)),
        MethodName = rep("BLABLA", length(ColumnNames) * (length(Methods) + 1)),
        Lambda = rep(1.0, length(ColumnNames) * (length(Methods) + 1)),
        NormalizedStatistics = rep(1.0, length(ColumnNames) * (length(Methods) + 1)))
    }
    DataCollection <- list()
    Counter <- 0L
    
    # Check range of data----
    MinVal <- min(data[[eval(ColumnNames[colNames])]], na.rm = TRUE)
    MaxVal <- max(data[[eval(ColumnNames[colNames])]], na.rm = TRUE)
    
    # Create Final Methods Object
    FinalMethods <- Methods
    
    # Update Methods----
    if(MinVal <= 0) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("boxcox","log"))]
    if(MinVal < 0) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("asinh","logplus1"))]
    if(MaxVal - MinVal > 1) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("asin", "logit"))]
    
    # Store column data as vector----
    x <- data[[eval(ColumnNames[colNames])]]
    
    # YeoJohnson----
    if(any(tolower(FinalMethods) %chin% "yeojohnson")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_YeoJohnson(x)
      DataCollection[["yeojohnson"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }
    
    # Log----
    if (any(tolower(FinalMethods) %chin% "log")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Log(x)
      DataCollection[["log"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = NA)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }
    
    # LogPlus1----
    if(any(tolower(FinalMethods) %chin% "logplus1")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_LogPlus1(x)
      DataCollection[["logplus1"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = NA)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }
    
    # BoxCox----
    if (any(tolower(FinalMethods) %chin% "boxcox")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_BoxCox(x)
      DataCollection[["boxcox"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }
    
    # Asinh----
    if(any(tolower(FinalMethods) %chin% "asinh")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Asinh(x)
      DataCollection[["asinh"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }
    
    # Asin----
    if(any(tolower(FinalMethods) %chin% "asin")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Asin(x)
      DataCollection[["asin"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }
    
    # Logit----
    if(any(tolower(FinalMethods) %chin% "logit")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Logit(x)
      DataCollection[["logit"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }
    
    # Identity----
    if(any(tolower(FinalMethods) %chin% "identity")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Identity(x)
      DataCollection[["identity"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }
    
    # Pick winner----
    EvaluationTable <- EvaluationTable[MethodName != "BLABLA"]
    if(colNames == 1) {
      Results <- EvaluationTable[order(NormalizedStatistics)][1L, ]
    } else {
      Results <- data.table::rbindlist(list(Results, EvaluationTable[order(NormalizedStatistics)][1L, ]))
    }
    
    # Apply to data----
    data[, ColumnNames[colNames] := DataCollection[[tolower(Results[eval(colNames), MethodName])]]]
  }
  
  # Save output----
  if(SaveOutput & !is.null(Path)) data.table::fwrite(Results, file = file.path(normalizePath(Path), paste0(TransID, "_transformation.csv")))
  
  # Return data----
  return(list(Data = data, FinalResults = Results))
}

#' AutoTransformationScore() is a the complimentary function to AutoTransformationCreate()
#'
#' AutoTransformationScore() is a the compliment function to AutoTransformationCreate(). Automatically apply or inverse the transformations you identified in AutoTransformationCreate() to other data sets. This is useful for applying transformations to your validation and test data sets for modeling. It's also useful for back-transforming your target and prediction columns after you have build and score your models so you can obtain statistics on the original features.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param ScoringData This is your source data
#' @param FinalResults This is the FinalResults output object from AutoTransformationCreate().
#' @param Type Set to "Inverse" to back-transfrom or "Apply" for applying the transformation.
#' @param Path Set to the directly where you want to save all of your modeling files
#' @param TransID Set to a character value that corresponds with your modeling project
#' @return data with transformed columns
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Adrian = runif(N))
#' data[, x1 := qnorm(Adrian)]
#' data[, x2 := runif(N)]
#' data[, Adrian1 := log(pnorm(Correl * x1 +
#'                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data <- AutoTransformationScore(data,
#'                                 FinalResults,
#'                                 Path = NULL,
#'                                 TransID = "Trans")
#' }
#' @export
AutoTransformationScore <- function(ScoringData,
                                    FinalResults,
                                    Type = "Inverse",
                                    TransID = "TestModel",
                                    Path = NULL) {
  
  # Check arguments----
  if(!data.table::is.data.table(ScoringData)) data.table::setDT(ScoringData)
  
  # Pull in Results File----
  if(!is.null(FinalResults)) {
    Results <- FinalResults
  } else {
    Results <- data.table::fread(file = file.path(normalizePath(Path), paste0(TransID, "_transformation.csv")))
  }
  
  # Loop through ColumnNames----
  for(colNames in Results[["ColumnName"]]) {
    
    # YeoJohnson----
    if(Results[ColumnName == eval(colNames), MethodName] == "YeoJohnson") {
      if(tolower(Type) != "inverse") {
        data.table::set(ScoringData, j = eval(colNames), value = Apply_YeoJohnson(ScoringData[[eval(colNames)]], Results[ColumnName == eval(colNames), Lambda]))
      } else {
        data.table::set(ScoringData, j = eval(colNames), value = InvApply_YeoJohnson(x = ScoringData[[eval(colNames)]], lambda = Results[ColumnName == eval(colNames), Lambda]))
      }
    }
    
    # Log----
    if(Results[ColumnName == eval(colNames), MethodName] == "Log") {
      if(tolower(Type) != "inverse") {
        data.table::set(ScoringData, j = eval(colNames), value = Apply_Log(ScoringData[[eval(colNames)]]))
      } else {
        data.table::set(ScoringData, j = eval(colNames), value = InvApply_Log(x = ScoringData[[eval(colNames)]]))
      }
    }
    
    # LogPlus1----
    if(Results[ColumnName == eval(colNames), MethodName] == "LogPlus1") {
      if(tolower(Type) != "inverse") {
        data.table::set(ScoringData, j = eval(colNames), value = Apply_LogPlus1(ScoringData[[eval(colNames)]]))
      } else {
        data.table::set(ScoringData, j = eval(colNames), value = InvApply_LogPlus1(x = ScoringData[[eval(colNames)]]))
      }
    }
    
    # BoxCox----
    if(Results[ColumnName == eval(colNames), MethodName] == "BoxCox") {
      if(tolower(Type) != "inverse") {
        data.table::set(ScoringData, j = eval(colNames), value = Apply_BoxCox(ScoringData[[eval(colNames)]], Results[ColumnName == eval(colNames), Lambda]))
      } else {
        data.table::set(ScoringData, j = eval(colNames), value = InvApply_BoxCox(ScoringData[[eval(colNames)]], Results[ColumnName == eval(colNames), Lambda]))
      }
    }
    
    # Asinh----
    if(Results[ColumnName == eval(colNames), MethodName] == "Asinh") {
      if(tolower(Type) != "inverse") {
        data.table::set(ScoringData, j = eval(colNames), value = Apply_Asinh(ScoringData[[eval(colNames)]]))
      } else {
        data.table::set(ScoringData, j = eval(colNames), value = InvApply_Asinh(ScoringData[[eval(colNames)]]))
      }
    }
    
    # Asin----
    if(Results[ColumnName == eval(colNames), MethodName] == "Asin") {
      if(tolower(Type) != "inverse") {
        data.table::set(ScoringData, j = eval(colNames), value = Apply_Asin(ScoringData[[eval(colNames)]]))
      } else {
        data.table::set(ScoringData, j = eval(colNames), value = InvApply_Asin(ScoringData[[eval(colNames)]]))
      }
    }
    
    # Logit----
    if(Results[ColumnName == eval(colNames), MethodName] == "Logit") {
      if(tolower(Type) != "inverse") {
        data.table::set(ScoringData, j = eval(colNames), value = Apply_Logit(ScoringData[[eval(colNames)]]))
      } else {
        data.table::set(ScoringData, j = eval(colNames), value = InvApply_Logit(ScoringData[[eval(colNames)]]))
      }
    }
  }
  
  # Return data----
  return(ScoringData)
}
