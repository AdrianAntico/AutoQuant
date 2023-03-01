#' @title Standardize
#'
#' @description Generate standardized values for multiple variables, by groups if provided, and with a selected granularity
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data Source data.table
#' @param ColNames Character vector of column names
#' @param GroupVars Character vector of column names to have percent ranks by the group levels
#' @param Center TRUE
#' @param Scale TRUE
#' @param ScoreTable FALSE. Set to TRUE to return a data.table that can be used to apply or backtransform via StandardizeScoring
#'
#' @examples
#' \dontrun{
#' data <- data.table::fread(file.choose())
#' x <- Standardize(data = data, ColNames = c('Weekly_Sales', 'XREG3'), GroupVars = c('Region','Store','Dept'), Center = TRUE, Scale = TRUE, ScoreTable = TRUE)
#' }
#'
#' @export
Standardize <- function(data, ColNames, GroupVars = NULL, Center = TRUE, Scale = TRUE, ScoreTable = FALSE) {

  # Standardize
  if(length(GroupVars) == 0L) {
    data[, paste0(ColNames, '_Standardize') := lapply(.SD, FUN = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)), .SDcols = c(ColNames)]
  } else {
    data[, paste0(ColNames, '_Standardize') := lapply(.SD, FUN = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)), .SDcols = c(ColNames), by = c(eval(GroupVars))]
  }

  # ScoreTable creation
  if(ScoreTable) {
    x <- data[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ColNames), by = c(GroupVars)]
    data.table::setnames(x = x, old = ColNames, new = paste0(ColNames, "_mean"))
    y <- data[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ColNames), by = c(GroupVars)]
    data.table::setnames(x = y, old = ColNames, new = paste0(ColNames, "_sd"))
    xy <- cbind(x,y[, (GroupVars) := NULL])
  }

  # Return
  if(!ScoreTable) {
    return(data)
  } else {
    return(list(
      data = data,
      ScoreTable = xy
    ))
  }
}

#' @title StandardizeScoring
#'
#' @description Generate standardized values for multiple variables, by groups if provided, and with a selected granularity
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data Source data.table
#' @param Apply 'apply' or 'backtransform'
#' @param ColNames Character vector of column names
#' @param GroupVars Character vector of column names to have percent ranks by the group levels
#' @param Center TRUE
#' @param Scale TRUE
#'
#' @examples
#' \dontrun{
#' x <- Standardize(data = data, ColNames = c('Weekly_Sales', 'XREG1'), GroupVars = c('Region','Store','Dept'), Center = TRUE, Scale = TRUE)
#' }
#'
#' @export
StandardizeScoring <- function(data, ScoreTable, Apply = 'apply', GroupVars = NULL) {

  # Facts
  nam <- names(ScoreTable)[which(!names(ScoreTable) %in% GroupVars)]

  # Apply will apply standardization to new data
  # Backtransform will undo standardization
  if(Apply == 'apply') {
    data.table::setkeyv(x = data, cols = GroupVars)
    data.table::setkeyv(x = ScoreTable, cols = GroupVars)
    data[ScoreTable, paste0(nam) := mget(paste0('i.', nam))]
    nams <- nam[seq_len(length(nam) / 2)]
    ColNames <- gsub(pattern = "_mean", replacement = "", x = nams)
    for(i in ColNames) data[, paste0(i, "_Standardize") := (get(i) - get(paste0(i, "_mean"))) / get(paste0(i, "_sd"))]
    data.table::set(data, j = c(nam), value = NULL)
  } else {
    data.table::setkeyv(x = data, cols = GroupVars)
    data.table::setkeyv(x = ScoreTable, cols = GroupVars)
    data[ScoreTable, paste0(nam) := mget(paste0('i.', nam))]
    nams <- nam[seq_len(length(nam) / 2)]
    ColNames <- gsub(pattern = "_mean", replacement = "", x = nams)
    for(i in ColNames) data[, eval(i) := get(paste0(i, "_Standardize")) * get(paste0(i, "_sd")) + get(paste0(i, "_mean"))]
    data.table::set(data, j = c(nam), value = NULL)
  }

  # Return
  return(data)
}

#' @title PercRank
#'
#' @description Generate percent ranks for multiple variables, by groups if provided, and with a selected granularity
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data Source data.table
#' @param ColNames Character vector of column names
#' @param GroupVars Character vector of column names to have percent ranks by the group levels
#' @param Granularity Provide a value such that data.table::frank(Variable) * (1 / Granularity) / .N * Granularity. Default is 0.001
#' @param ScoreTable = FALSE. Set to TRUE to get the reference values for applying to new data. Pass to scoring version of this function
#'
#' @examples
#' \dontrun{
#' data <- data.table::fread(file.choose())
#' x <- PercRank(data, ColNames = c('Weekly_Sales', 'XREG1'), GroupVars = c('Region','Store','Dept'), Granularity = 0.001, ScoreTable = TRUE)
#' }
#'
#' @export
PercRank <- function(data, ColNames, GroupVars = NULL, Granularity = 0.001, ScoreTable = FALSE) {
  if(length(GroupVars) == 0L) {
    data[, paste0(ColNames, '_PercRank') := lapply(.SD, FUN = function(x) data.table::frank(x) * (1 / Granularity) / .N * Granularity), .SDcols = c(ColNames)]
  } else {
    data[, paste0(ColNames, '_PercRank') := lapply(.SD, FUN = function(x) data.table::frank(x) * (1 / Granularity) / .N * Granularity), .SDcols = c(ColNames), by = c(eval(GroupVars))]
  }
  if(!ScoreTable) {
    return(data)
  } else {
    return(list(
      data = data,
      ScoreTable = unique(data[, .SD, .SDcols = c(ColNames, paste0(ColNames, '_PercRank'))])
    ))
  }
}

#' @title PercRankScoring
#'
#' @description Generate percent ranks for multiple variables, by groups if provided, and with a selected granularity, via list passed from PercRank
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data Source data.table
#' @param GroupVars Character vector of column names to have percent ranks by the group levels
#' @param ScoreTable list of values returned from PercRank
#' @param RollDirection "forward" or "backward"
#'
#' @export
PercRankScoring <- function(data, ScoreTable, GroupVars = NULL, RollDirection = 'forward') {
  # nam <- names(ScoreTable)[1] # nam <- names(ScoreTable)[2]
  for(nam in names(ScoreTable)) {
    data.table::setkeyv(data, cols = c(unique(gsub(pattern = "_PercRank", replacement = "", x = nam))))
    data.table::setkeyv(ScoreTable, cols = nam)
    if('forward' == tolower(RollDirection)) {
      data <- ScoreTable[data, roll = Inf]
    } else {
      data <- ScoreTable[data, roll = -Inf]
    }
  }
  return(data)
}

#' @title Interact
#'
#' @family Feature Engineering
#'
#' @param x Names
#' @param i Iteration
#' @param NumVarOperations List of names
#' @param Standardize List of results
#'
#' @noRd
Interact <- function(x, i, NumVarOperations, Standardize) {
  if(i > 2L) {
    temp <- Standardize[[NumVarOperations[[x]][[1L]]]]$Result * Standardize[[NumVarOperations[[x]][[2L]]]]$Result
    for(ggg in 3L:i) temp <- temp * Standardize[[NumVarOperations[[x]][[ggg]]]]$Result
  } else {
    temp <- Standardize[[NumVarOperations[[x]][[1L]]]]$Result * Standardize[[NumVarOperations[[x]][[2L]]]]$Result
  }
  temp
}

#' @title AutoInteraction
#'
#' @description AutoInteraction creates interaction variables from your numerical features in your data. Supply a set of column names to utilize and set the interaction level. Supply a character vector of columns to exclude and the function will ignore those features.
#'
#' @family Feature Engineering
#'
#' @author Adrian Antico
#'
#' @param data Source data.table
#' @param NumVars Names of numeric columns (if NULL, all numeric and integer columns will be used)
#' @param InteractionDepth The max K in N choose K. If NULL, K will loop through 1 to length(NumVars). Default is 2 for pairwise interactions
#' @param Center TRUE to center the data
#' @param Scale TRUE to scale the data
#' @param SkipCols Use this to exclude features from being created. An example could be, you build a model with all variables and then use the varaible importance list to determine which features aren't necessary and pass that set of features into this argument as a character vector.
#' @param Scoring Defaults to FALSE. Set to TRUE for generating these columns in a model scoring setting
#' @param File When Scoring is set to TRUE you have to supply either the .Rdata list with lookup values for recreating features or a pathfile to the .Rdata file with the lookup values. If you didn't center or scale the data then this argument can be ignored.
#'
#' @examples
#' \dontrun{
#'
#' #########################################
#' # Feature Engineering for Model Training
#' #########################################
#'
#' # Create fake data
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
#' # Print number of columns
#' print(ncol(data))
#'
#' # Store names of numeric and integer cols
#' Cols <-names(data)[c(which(unlist(lapply(data, is.numeric))),
#'                      which(unlist(lapply(data, is.integer))))]
#'
#' # Model Training Feature Engineering
#' system.time(data <- AutoQuant::AutoInteraction(
#'   data = data,
#'   NumericVars = Cols,
#'   InteractionDepth = 4,
#'   Center = TRUE,
#'   Scale = TRUE,
#'   SkipCols = NULL,
#'   Scoring = FALSE,
#'   File = getwd()))
#'
#' # user  system elapsed
#' # 0.30    0.11    0.41
#'
#' # Print number of columns
#' print(ncol(data))
#'
#' ########################################
#' # Feature Engineering for Model Scoring
#' ########################################
#'
#' # Create fake data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.70,
#'   N = 1000,
#'   ID = 2L,
#'   FactorCount = 2L,
#'   AddDate = TRUE,
#'   ZIP = 0L,
#'   TimeSeries = FALSE,
#'   ChainLadderData = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Print number of columns
#' print(ncol(data))
#'
#' # Reduce to single row to mock a scoring scenario
#' data <- data[1L]
#'
#' # Model Scoring Feature Engineering
#' system.time(data <- AutoQuant::AutoInteraction(
#'   data = data,
#'   NumericVars = names(data)[
#'     c(which(unlist(lapply(data, is.numeric))),
#'       which(unlist(lapply(data, is.integer))))],
#'   InteractionDepth = 4,
#'   Center = TRUE,
#'   Scale = TRUE,
#'   SkipCols = NULL,
#'   Scoring = TRUE,
#'   File = file.path(getwd(), "Standardize.Rdata")))
#'
#' # user  system elapsed
#' # 0.19    0.00    0.19
#'
#' # Print number of columns
#' print(ncol(data))
#' }
#' @export
AutoInteraction <- function(data = NULL,
                            NumericVars = NULL,
                            InteractionDepth = 2,
                            Center = TRUE,
                            Scale = TRUE,
                            SkipCols = NULL,
                            Scoring = FALSE,
                            File = NULL) {

  # Arg Check ----
  if(InteractionDepth > length(NumericVars)) stop("InteractionDepth cannot be greater than the length of NumericVars")

  # Check data ----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Check args ----
  if(!is.logical(Center)) stop("Center must be either TRUE or FALSE")
  if(!is.logical(Scale)) stop("Scale must be either TRUE or FALSE")
  if(!is.logical(Scoring)) stop("Scoring must be either TRUE or FALSE")
  if(!is.null(NumericVars) && !is.character(NumericVars)) stop("NumericVars must be a character vector or NULL")
  if(!is.null(SkipCols) && !is.character(SkipCols)) stop("SkipCols must be a character vector")
  if(!is.null(InteractionDepth) && !(is.numeric(InteractionDepth) || is.integer(InteractionDepth))) stop("InteractionDepth must be numeric or NULL")

  # Check File ----
  if(Scoring && (Center || Scale) && is.null(File)) stop("You need to supply the path for the File argument")
  if(Scoring && !is.null(File)) if(!dir.exists(File) && !file.exists(File) && !is.list(File)) stop("File is not valid")
  if(!Scoring && !is.null(File)) if(!dir.exists(File)) stop("File is not valid path")

  # Columns Validity check ----
  if(is.null(NumericVars)) {
    NumericVars <- names(data)[c(which(unlist(lapply(data, is.numeric))), which(unlist(lapply(data, is.integer))))]
    if(identical(NumericVars, character(0))) stop("No numeric or integer valued columns in data")
    for(nam in NumericVars) {
      tmp <- class(data[1L, get(nam)])
      if(!any(tmp %chin% c("numeric","integer"))) NumericVars <- NumericVars[!NumericVars %chin% nam]
    }
    if(identical(NumericVars, character(0))) stop("No numeric or integer valued columns in data")
    if(length(NumericVars) == 1L) stop("Only one column exists so no interactions can be generated")
    if(length(NumericVars) < InteractionDepth) stop("You can't have an InteractionDepth that is greater than the number of NumericVars")
  } else {
    if(!all(NumericVars %chin% names(data))) stop("at least some NumericVars are not in data")
    for(nam in NumericVars) {
      tmp <- class(data[1L, get(nam)])
      if(!any(tmp %chin% c("numeric","integer"))) NumericVars <- NumericVars[!NumericVars %chin% nam]
    }
    if(identical(NumericVars, character(0))) stop("No numeric or integer valued columns in data")
    if(length(NumericVars) == 1L) stop("Only one column exists so no interactions can be generated")
    if(length(NumericVars) < InteractionDepth) stop("You can't have an InteractionDepth that is greater than the number of NumericVars")
  }

  # Define Number of computations and increase alloc if too many ----
  N1 <- length(NumericVars)
  N <- InteractionDepth
  Total <- c()
  for(com in seq_len(InteractionDepth)[-1L]) Total <- c(Total, ncol(combinat::combn(NumericVars, m = com)))
  if(sum(Total) + ncol(data) > 1028L) data.table::setalloccol(DT = data, n = sum(Total) + ncol(data), verbose = TRUE)

  # Standardize collection list ----
  if(Scoring && (Center || Scale) && !is.list(File)) load(file = file.path(File))
  if(!exists("Standardize")) Standardize <- list()

  # Get results and metadata ahead of time ----
  if(!Scoring) {
    for(nam in NumericVars) {
      if(Center && Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Mean <- Rfast::colmeans(a1)
        c1 <- t(a1) - Standardize[[nam]]$Mean
        Standardize[[nam]]$Denom <- sqrt(Rfast::rowsums(c1^2))
        Standardize[[nam]]$Factor <- sqrt((dim(a1)[1L] - 1))
        Standardize[[nam]]$Result <- t(c1/Standardize[[nam]]$Denom * Standardize[[nam]]$Factor)
      } else if(Center && !Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Mean <- Rfast::colmeans(a1)
        Standardize[[nam]]$Result <- t(a1) - Standardize[[nam]]$Mean
      } else if(!Center && Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Denom <- sqrt(Rfast::rowsums(a1^2))
        Standardize[[nam]]$Factor <- sqrt((dim(a1)[1L] - 1))
        Standardize[[nam]]$Result <- t(a1/Standardize[[nam]]$Denom * Standardize[[nam]]$Factor)
      } else {
        Standardize[[nam]]$Result <- data[[NumVarOperations[[x]][[2L]]]]
      }
    }
  } else {
    for(nam in NumericVars) {
      if(Center && Scale) {
        a1 <- as.matrix(data[[nam]])
        c1 <- t(a1) - Standardize[[nam]]$Mean
        Standardize[[nam]]$Result <- t(c1/Standardize[[nam]]$Denom * Standardize[[nam]]$Factor)
      } else if(Center && !Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Result <- t(a1) - Standardize[[nam]]$Mean
      } else if(!Center && Scale) {
        a1 <- as.matrix(data[[nam]])
        Standardize[[nam]]$Result <- t(a1/Standardize[[nam]]$Denom * Standardize[[nam]]$Factor)
      } else {
        Standardize[[nam]]$Result <- data[[NumVarOperations[[x]][[2L]]]]
      }
    }
  }

  # Define colnames and contributing features ----
  for(i in seq_len(N)[-1L]) {

    # Initialize lists and vector
    NumVarsNames <- c()
    NumVarOperations <- list()

    # Interaction Depth equals number of variables
    if(i == N1) {
      temp <- combinat::combn(NumericVars, m = i)
      for(m in N1) {
        temp2 <- paste(temp, collapse = "_")
        templist <- list()
        for(zz in seq_len(m)) templist[[zz]] <- temp[zz]
        NumVarOperations[[temp2]] <- templist
      }
      NumVarsNames <- c(NumVarsNames, temp2)
    } else if(i <= N1) {
      temp <- combinat::combn(NumericVars, m = i)
      temp2 <- c()
      for(k in seq_len(ncol(temp))) {
        for(l in seq_len(i)) {
          if(l == 1L) {
            temp2 <- temp[l,k]
          } else {
            temp2 <- paste(temp2,temp[l, k], sep = "_")
            templist <- list()
            for(zz in seq_len(l)) templist[[zz]] <- temp[zz, k]
            NumVarOperations[[temp2]] <- templist
          }
        }
        NumVarsNames <- c(NumVarsNames, temp2)
      }
    }

    # SkipCols ----
    if(!is.null(SkipCols)) NumVarsNames <- NumVarsNames[!NumVarsNames %chin% SkipCols]

    # Build features ----
    data[, (NumVarsNames) := lapply(NumVarsNames, Interact, i, NumVarOperations, Standardize)]
  }

  # Save Standardize if Center or Scale ----
  if(!Scoring && (Center || Scale)) {
    for(nam in names(Standardize)) Standardize[[nam]][["Result"]] <- NULL
    if(!is.null(File)) save(Standardize, file = file.path(File, "Standardize.Rdata"))
  }

  # Return data ----
  return(data)
}

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
  val <- list(Name = "YeoJohnson", Data = trans_data, Lambda = lambda, Normalized_Statistic = unname(ptest$statistic / ptest$df))
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
    constant <- sum(sign(x) * log(abs(x) + 1))
    - 0.5 * n * log(x_t_var) + (lambda - 1) * constant
  }

  results <- optimize(
    yj_loglik,
    lower = lower,
    upper = upper,
    maximum = TRUE,
    tol = .0001)
  return(results$maximum)
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
  return(x)
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
  return(val)
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
  val <- list(Name = "BoxCox", Data = trans_data, Lambda = lambda, Normalized_Statistic = unname(ptest$statistic / ptest$df))
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
  n <- length(x)
  ccID <- !is.na(x)
  x <- x[ccID]
  if (any(x <= 0)) stop("x must be positive")
  log_x <- log(x)
  xbar <- exp(mean(log_x))
  fit <- lm(x ~ 1, data = data.frame(x = x))
  xqr <- fit$qr
  boxcox_loglik <- function(lambda) {
    if (abs(lambda) > eps)
      xt <- (x ^ lambda - 1) / lambda
    else
      xt <- log_x * (1 + (lambda * log_x) / 2 *
                       (1 + (lambda * log_x) / 3 *
                          (1 + (lambda * log_x) / 4)))
    - n / 2 * log(sum(qr.resid(xqr, xt / xbar ^ (lambda - 1)) ^ 2))
  }

  results <- optimize(
    boxcox_loglik,
    lower = lower,
    upper = upper,
    maximum = TRUE,
    tol = .0001)
  return(results$maximum)
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
  if(abs(lambda) < eps) {
    val <- log(x)
  } else {
    val <- (sign(x) * abs(x) ^ lambda - 1) / lambda
  }
  return(val)
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
  return(val)
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
  val <- list(Name = "Asinh", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Inverse Asinh Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asinh results
Apply_Asinh <- function(x) {
  return(asinh(x))
}

#' Inverse Asinh Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asinh results
InvApply_Asinh <- function(x) {
  return(sinh(x))
}

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
  val <- list(Name = "Asin", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Inverse Asin Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asin results
Apply_Asin <- function(x) {
  return(asin(sqrt(x)))
}

#' Inverse Asin Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asin results
InvApply_Asin <- function(x) {
  return(sin(x) ^ 2)
}

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
  val <- list(Name = "Logit", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply Logit Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Logit results
Apply_Logit <- function(x) {
  return(log(x / (1 - x)))
}

#' Inverse Logit Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Logit results
InvApply_Logit <- function(x) {
  return(1 / (1 + exp(-x)))
}

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
  val <- list(Name = "Identity", Data = x, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
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
  val <- list(Name = "Log", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply Log Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Apply_Log <- function(x) {
  return(log(x))
}

#' Inverse Log Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
InvApply_Log <- function(x) {
  return(exp(x))
}

#' Test LogPlus1 Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return LogPlus1 results
Test_LogPlus1 <- function(x) {
  stopifnot(is.numeric(x))
  xx <- min(x, na.rm = TRUE)
  if(xx < 0) trans_data <- log(x+xx) else trans_data <- log(x)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "LogPlus1", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply LogPlus1 Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Apply_LogPlus1 <- function(x) {
  return(log(x+1))
}

#' Inverse LogPlus1 Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
InvApply_LogPlus1 <- function(x) {
  return(exp(x)-1)
}

#' Test Sqrt Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Sqrt results
Test_Sqrt <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- sqrt(x)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "Sqrt", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply Sqrt Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Apply_Sqrt <- function(x) {
  return(sqrt(x))
}

#' Inverse Sqrt Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
InvApply_Sqrt <- function(x) {
  return(x^2)
}

#' @title AutoTransformationCreate
#'
#' @description AutoTransformationCreate is a function for automatically identifying the optimal transformations for numeric features and transforming them once identified. This function will loop through your selected transformation options (YeoJohnson, BoxCox, Asinh, Asin, and Logit) and find the one that produces data that is the closest to normally distributed data. It then makes the transformation and collects the metadata information for use in the AutoTransformationScore() function, either by returning the objects (always) or saving them to file (optional).
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data This is your source data
#' @param ColumnNames List your columns names in a vector, for example, c("Target", "IV1")
#' @param Methods Choose from "YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Asin", "Logit", and "Identity". Note, LogPlus1 runs
#' @param Path Set to the directly where you want to save all of your modeling files
#' @param TransID Set to a character value that corresponds with your modeling project
#' @param SaveOutput Set to TRUE to save necessary file to run AutoTransformationScore()
#' @return data with transformed columns and the transformation object for back-transforming later
#' @examples
#' \dontrun{
#' # Create Fake Data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 25000,
#'   ID = 2L,
#'   ZIP = 0,
#'   FactorCount = 2L,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Columns to transform
#' Cols <- names(data)[1L:11L]
#' print(Cols)
#'
#' # Run function
#' data <- AutoQuant::AutoTransformationCreate(
#'   data,
#'   ColumnNames = Cols,
#'   Methods = c("YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "Identity"),
#'   Path = getwd(),
#'   TransID = "Trans",
#'   SaveOutput = TRUE)
#' }
#' @export
AutoTransformationCreate <- function(data,
                                     ColumnNames = NULL,
                                     Methods = c("BoxCox","YeoJohnson","Asinh","Log","LogPlus1","Sqrt","Asin","Logit","Identity"),
                                     Path = NULL,
                                     TransID = "ModelID",
                                     SaveOutput = FALSE) {

  # Check arguments ----
  Methods <- unique(tolower(Methods))
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!any(tolower(Methods) %chin% c("boxcox", "yeojohnson", "asinh", "sqrt", "log", "logplus1", "asin", "logit"))) stop("Methods not supported")
  if(!"identity" %chin% Methods) Methods <- c(Methods, "identity")
  if(is.numeric(ColumnNames) || is.integer(ColumnNames)) ColumnNames <- names(data)[ColumnNames]
  for(i in ColumnNames) if(!(any(class(data[[eval(i)]]) %chin% c("numeric", "integer")))) stop("ColumnNames must be for numeric or integer columns")

  # Loop through ColumnNames ----
  # colNames = 1
  for(colNames in seq_along(ColumnNames)) {

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
    if(MinVal <= 0) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("boxcox","log","logit"))]
    if(MinVal < 0) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("logplus1","sqrt","asin"))]
    if(MaxVal > 1) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("asin"))]
    if(MaxVal >= 1) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("logit"))]

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
    if(any(tolower(FinalMethods) %chin% "log")) {
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

    # Sqrt----
    if(any(tolower(FinalMethods) %chin% "sqrt")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Sqrt(x)
      DataCollection[["sqrt"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = NA)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # BoxCox----
    if(any(tolower(FinalMethods) %chin% "boxcox")) {
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
    if(colNames == 1L) {
      Results <- EvaluationTable[order(NormalizedStatistics)][1L]
    } else {
      Results <- data.table::rbindlist(list(Results, EvaluationTable[order(NormalizedStatistics)][1L]))
    }

    # Apply to data----
    data[, ColumnNames[colNames] := DataCollection[[tolower(Results[eval(colNames), MethodName])]]]
  }

  # Save output----
  if(SaveOutput & !is.null(Path)) data.table::fwrite(Results, file = file.path(normalizePath(Path), paste0(TransID, "_transformation.csv")))

  # Return data----
  return(list(Data = data, FinalResults = Results))
}

#' @title AutoTransformationScore() is a the complimentary function to AutoTransformationCreate()
#'
#' @description AutoTransformationScore() is a the compliment function to AutoTransformationCreate(). Automatically apply or inverse the transformations you identified in AutoTransformationCreate() to other data sets. This is useful for applying transformations to your validation and test data sets for modeling. It's also useful for back-transforming your target and prediction columns after you have build and score your models so you can obtain statistics on the original features.
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
#' \dontrun{
#' # Create Fake Data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 25000,
#'   ID = 2L,
#'   ZIP = 0,
#'   FactorCount = 2L,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Columns to transform
#' Cols <- names(data)[1L:11L]
#' print(Cols)
#'
#' data <- data[1]
#'
#' # Run function
#' Output <- AutoQuant::AutoTransformationCreate(
#'   data,
#'   ColumnNames = Cols,
#'   Methods = c("YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "Identity"),
#'   Path = getwd(),
#'   TransID = "Model_1",
#'   SaveOutput = TRUE)
#'
#' # Output
#' data <- Output$Data
#' TransInfo <- Output$FinalResults
#'
#' # Back Transform
#' data <- AutoQuant::AutoTransformationScore(
#'   data,
#'   FinalResults = TransInfo,
#'   Path = NULL,
#'   TransID = "Model_1")
#' }
#' @export
AutoTransformationScore <- function(ScoringData,
                                    FinalResults,
                                    Type = "Inverse",
                                    TransID = "TestModel",
                                    Path = NULL) {

  # Check arguments----
  if(!data.table::is.data.table(ScoringData)) ScoringData <- data.table::as.data.table(ScoringData)

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

    # Sqrt----
    if(Results[ColumnName == eval(colNames), MethodName] == "Sqrt") {
      if(tolower(Type) != "inverse") {
        data.table::set(ScoringData, j = eval(colNames), value = Apply_Sqrt(ScoringData[[eval(colNames)]]))
      } else {
        data.table::set(ScoringData, j = eval(colNames), value = InvApply_Sqrt(x = ScoringData[[eval(colNames)]]))
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

#' @title CreateInteractions
#'
#' @description Create interaction variables
#'
#' @author Adrian Antico
#' @family Feature Engineering - Numeric Types
#'
#' @param data Source data
#' @param RunMode 'train' or 'score'
#' @param ArgsList ArgsList_FFE
#' @param SkipCols Vector of column names to remove from data
#'
#' @examples
#' \dontrun{
#' Output <- AutoQuant:::CreateInteractions(
#'   data = data,
#'   RunMode = "train",
#'   ArgsList = ArgsList_FE,
#'   SkipCols = NULL)
#' data <- Output$data
#' ArgsList <- Output$ArgsList
#' }
#'
#' @return A list containing the data and the ArgsList
#' @noRd
CreateInteractions <- function(data = NULL,
                               RunMode = "train",
                               ArgsList = ArgsList,
                               SkipCols = NULL) {

  # Metadata
  Start <- Sys.time()

  # Run function
  if(tolower(RunMode) == "train") {

    # Metadata
    tempnames <- names(data.table::copy(data))

    # Manage factor variables
    if(any(ArgsList$Data$InteractionVariables %chin% ArgsList$Data$GroupVariables)) {
      FactorNumericInteraction <- ArgsList$Data$InteractionVariables[which(ArgsList$Data$InteractionVariables %chin% ArgsList$Data$GroupVariables)]
      ArgsList$Data$InteractionVariables <- ArgsList$Data$InteractionVariables[!ArgsList$Data$InteractionVariables %chin% FactorNumericInteraction]
      ArgsList$Data$InteractionVariables <- c(ArgsList$Data$InteractionVariables, ArgsList$FE_Columns$FE_DummyVariables %like% FactorNumericInteraction)
    }

    # Run function
    data <- AutoQuant::AutoInteraction(
      data = data,
      NumericVars = ArgsList$Data$InteractionVariables,
      InteractionDepth = ArgsList$FE_Args$Interaction$InteractionDepth,
      Center = ArgsList$FE_Args$Interaction$InteractionCenter,
      Scale = ArgsList$FE_Args$Interaction$InteractionScale,
      SkipCols = NULL,
      Scoring = FALSE,
      File = file.path(ArgsList$MetaData$MetaData_Path))

    # Args Tracking
    ArgsList$FE_Interaction$NumericVars <- ArgsList$Data$InteractionVariables
    ArgsList$FE_Interaction$InteractionDepth <- ArgsList$FE_Args$Interaction$Depth
    ArgsList$FE_Interaction$Center <- ArgsList$FE_Args$Interaction$InteractionCenter
    ArgsList$FE_Interaction$Scale <- ArgsList$FE_Args$Interaction$InteractionScale
    ArgsList$FE_Interaction$SkipCols <- NULL
    ArgsList$FE_Interaction$File <- file.path(ArgsList$MetaData$MetaData_Path)

    # New columns tracking
    ArgsList$FE_Columns$Interaction <- setdiff(names(data), tempnames)

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$Interaction_Training <- difftime(End, Start, units = "mins")

  } else {

    # Metadata
    Start <- Sys.time()

    # Run function
    data <- AutoQuant::AutoInteraction(
      data = data,
      NumericVars = ArgsList$FE_Interaction$NumericVars,
      InteractionDepth = ArgsList$FE_Interaction$InteractionDepth,
      Center = ArgsList$FE_Interaction$Center,
      Scale = ArgsList$FE_Interaction$Scale,
      SkipCols <- ArgsList$FE_Interaction$SkipCols,
      Scoring = TRUE,
      File = ArgsList$FE_Interaction$File)

    # Skip cols
    if(!is.null(SkipCols)) {
      temp <- names(data)
      temp <- temp[!temp %chin% SkipCols]
      temp <- setdiff(names(data), temp)
      data.table::set(data, j = temp, value = NULL)
    }

    # Run time tracking
    End <- Sys.time()
    ArgsList$RunTime$Interaction_Scoring <- difftime(End, Start, units = "mins")
  }

  # Return
  return(list(data = data, ArgsList = ArgsList))
}
