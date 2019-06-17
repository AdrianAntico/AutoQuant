utils::globalVariables(
  names = c(
    "Temporary",
    "Predictions",
    "IDcols",
    "AbsoluteError",
    "GroupVar",
    "MAE",
    "MSE",
    "PercentileRank",
    "PredictIsoForest",
    "SquaredError",
    "..keep1",
    "GroupVar",
    "R2_Metric",
    "test",
    "RelativeImportance",
    "ScaledImportance",
    "Percentage",
    "PredictedOutlier",
    "idx",
    "Value",
    "p0",
    ":=",
    ".I",
    "catboostGridList",
    "fwrite",
    "setnames",
    "Gain",
    "RANDOMNUMBER",
    "Frequency",
    "Cover",
    "MinVal",
    "Feature",
    "NewLevels",
    "OriginalLevels",
    "bootstrap_type",
    "Metric1",
    "TargetLevels",
    "Metric2",
    "Metric3",
    "Predict",
    "Predicted",
    "setorderv",
    "rbindlist",
    "is.data.table",
    "as.data.table",
    ".N",
    "%chin%",
    "setcolorder",
    ".SD",
    "SFreq",
    "BinaryRatingsMatrix",
    "Residuals",
    "ind",
    "RANDOMNUMER",
    "p1",
    "MetricValue",
    "Threshold",
    "EvalStat",
    "ParamRow",
    "ModelNumber",
    "Specificity",
    "Importance",
    "Variable",
    "ObsNum",
    "type",
    "KMeansModelFile",
    "FilePath1",
    "FilePath2",
    "fitY",
    "Nam",
    "RatingMatrix",
    "ProductRank",
    "model",
    "n_products",
    "TPR",
    "item",
    "value",
    "act",
    "MeanAbsError",
    "SupplyData",
    "string",
    "..string",
    "NThreads",
    "MaxMem",
    "LCVals",
    "Row",
    "ErrorCollectionLog",
    "prophet",
    "pushViewport",
    "quantile",
    "removeNumbers",
    "removePunctuation",
    "removeWords",
    "residuals",
    "rnorm",
    "runif",
    "sd",
    "stemDocument",
    "stopCluster",
    "stopwords",
    "stripWhitespace",
    "theme",
    "tm_map",
    "ts",
    "val1",
    "val2",
    "val3",
    "vals",
    "viewport",
    "wordcloud",
    "xlab",
    "years",
    "ylab",
    "TermDocumentMatrix",
    "Thresholds",
    "Type",
    "Utilities",
    "Utility",
    "V1",
    "V2",
    "V3",
    "VectorSource",
    "acts",
    "aes",
    "as.formula",
    "as_date",
    "coefhat",
    "content_transformer",
    "ds",
    "f0point5",
    "f1",
    "f2",
    "fitted",
    "head",
    "i",
    "id",
    "lm",
    "margin",
    "meanResid",
    "monreg",
    "nls",
    "object.size",
    "output",
    "percentaR",
    "CMD",
    "check",
    "results",
    "Thresh",
    "Label",
    "..k",
    "..j",
    "Date",
    "Target",
    "RowNumAsc",
    "AnomHigh",
    "AnomLow",
    "CumAnomHigh",
    "CumAnomLow",
    "AnomHighRate",
    "AnomLowRate",
    "predict",
    "..cols",
    ".",
    "..keep",
    "Output",
    "MTHT",
    "MTLT",
    "TEMP",
    "Accuracy",
    "FC_Eval",
    "DateTime",
    "Resid",
    "PercentError",
    "AbsolutePercentError",
    "ModelName",
    "MAPE",
    "ID",
    "Forecast_PROPHET",
    "percentage",
    "data",
    "FilePath",
    "grid_tuned_paths",
    "StoreFile",
    "temp",
    "Path"
  )
)

#' tempDatesFun Convert Excel datetime char columns to Date columns
#'
#' tempDatesFun takes the Excel datetime column, which imports as character, and converts it into a date type
#'
#' @author Adrian Antico
#' @family Misc
#' @param x The column of interest
#' @examples
#' Cdata <- data.table::data.table(DAY_DATE = "2018-01-01 8:53")
#' Cdata[, DAY_DATE := tempDatesFun(DAY_DATE)]
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
tempDatesFun <- base::Vectorize(function(x) {
  data.table::tstrsplit(x, " ")[[1]][1]
})

#' SimpleCap function is for capitalizing the first letter of words
#'
#' SimpleCap function is for capitalizing the first letter of words (need I say more?)
#'
#' @author Adrian Antico
#' @family Misc
#' @param x Column of interest
#' @examples
#' x <- "adrian"
#' x <- SimpleCap(x)
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
SimpleCap <- function(x) {
  s <- data.table::tstrsplit(x, " ")[[1]]
  paste(
    base::toupper(base::substring(s, 1, 1)),
    base::substring(s, 2),
    sep = "",
    collapse = " "
  )
}

#' PrintObjectsSize prints out the top N objects and their associated sizes, sorted by size
#'
#' @author Adrian Antico
#' @family Misc
#' @param N The number of objects to display
#' @examples
#' \donttest{
#' PrintObjectsSize(N = 10)
#' }
#' @return A print to your console of the sizes of the objects in your environment
#' @export
PrintObjectsSize <- function(N = 10) {
  m <- length(ls())
  z <- min(m, N)
  print(sort(-vapply(ls(), FUN.VALUE = 1.1, function(x) {
    object.size(get(x))
  }))[1:z] / 1024 / 1024)
}

#' RemixTheme function is a ggplot theme generator for ggplots
#'
#' This function adds the Remix Theme to ggplots
#'
#' @author Douglas Pestana
#' @family Misc
#' @examples
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(1000,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' p <- ggplot2::ggplot(data, ggplot2::aes(x = DateTime, y = Target)) + ggplot2::geom_line()
#' p <- p + RemixTheme()
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
RemixTheme <- function() {
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 11),
    axis.text = ggplot2::element_text(size = 11),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(color = "#1c1c1c",
                                        size = 11),
    legend.title = ggplot2::element_blank(),
    legend.justification = 0,
    legend.position = "top",
    plot.background = ggplot2::element_rect(fill = "#E7E7E7"),
    panel.background = ggplot2::element_rect(fill = "#E7E7E7"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "white"),
    panel.grid.minor.y = ggplot2::element_line(color = "white"),
    plot.title = ggplot2::element_text(
      color = "#1c1c1c",
      size = 28,
      hjust = 0,
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      color = "#1c1c1c",
      size = 16,
      hjust = 0
    ),
    plot.caption = ggplot2::element_text(
      size = 9,
      hjust = 0,
      face = "italic"
    )
  )
}

#' Multiplot is a function for combining multiple plots
#'
#' Sick of copying this one into your code? Well, not anymore.
#'
#' @author Adrian Antico
#' @family Misc
#' @param plotlist This is the list of your charts
#' @param cols This is the number of columns in your multiplot
#' @param layout Leave NULL
#' @param ... Passthrough arguments
#' @examples
#' Correl <- 0.85
#' data <- data.table::data.table(Target = runif(100))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(100)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Predict := (pnorm(Correl * x1 +
#'                            sqrt(1-Correl^2) * qnorm(x2)))]
#' p1 <- RemixAutoML::ParDepCalPlots(data,
#'                                   PredictionColName = "Predict",
#'                                   TargetColName = "Target",
#'                                   IndepVar = "Independent_Variable1",
#'                                   GraphType = "calibration",
#'                                   PercentileBucket = 0.20,
#'                                   FactLevels = 10,
#'                                   Function = function(x) mean(x, na.rm = TRUE))
#' p2 <- RemixAutoML::ParDepCalPlots(data,
#'                                   PredictionColName = "Predict",
#'                                   TargetColName = "Target",
#'                                   IndepVar = "Independent_Variable1",
#'                                   GraphType = "boxplot",
#'                                   PercentileBucket = 0.20,
#'                                   FactLevels = 10,
#'                                   Function = function(x) mean(x, na.rm = TRUE))
#' RemixAutoML::multiplot(plotlist = list(p1,p2), cols = 2)
#' @return Multiple ggplots on a single image
#' @export
multiplot <-
  function(...,
           plotlist = NULL,
           cols     = 2,
           layout   = NULL) {
    plots <- c(list(...), plotlist)
    
    numPlots <- length(plots)
    
    if (is.null(layout)) {
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols,
                       nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
      
    } else {
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout),
                                                                   ncol(layout))))
      
      for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(
          plots[[i]],
          vp = grid::viewport(
            layout.pos.row = matchidx$row,
            layout.pos.col = matchidx$col
          )
        )
      }
    }
  }

#' ChartTheme function is a ggplot theme generator for ggplots
#'
#' This function helps your ggplots look professional with the choice of the two main colors that will dominate the theme
#'
#' @author Adrian Antico
#' @family Misc
#' @param Size The size of the axis labels and title
#' @examples
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(1000,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' p <- ggplot2::ggplot(data, ggplot2::aes(x = DateTime, y = Target)) + ggplot2::geom_line()
#' p <- p + ChartTheme(Size = 12)
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
ChartTheme <- function(Size = 12) {
  chart_theme <-
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "gray94"),
      panel.background = ggplot2::element_rect(
        fill = "lightsteelblue1",
        colour = "darkblue",
        size = 0.25,
        color = "darkblue"
      ),
      panel.grid.major = ggplot2::element_line(
        colour = "darkblue",
        size = 0.01,
        color = "white",
        linetype = 1
      ),
      panel.grid.minor = ggplot2::element_line(
        colour = "darkblue",
        size = 0.01,
        color = "white",
        linetype = 1
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(
        color = "darkblue",
        size = Size,
        face = "bold"
      ),
      legend.background = ggplot2::element_rect(
        fill = "gray95",
        size = 1,
        linetype = "solid",
        color = "darkblue"
      ),
      plot.title = ggplot2::element_text(
        color = "darkblue",
        size = Size,
        face = "bold"
      ),
      axis.title = ggplot2::element_text(
        color = "darkblue",
        size = Size,
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        colour = "darkblue",
        face = "bold",
        angle = 90
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(
        t = 20,
        r = 20,
        b = 20,
        l = 20
      )),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(
        t = 20,
        r = 20,
        b = 20,
        l = 20
      )),
      panel.border = ggplot2::element_rect(
        colour = "darkblue",
        fill = NA,
        size = 1.5
      )
    )
  chart_theme
}

#' Percentile rank function
#'
#' This function computes percentile ranks for each row in your data like Excel's PERCENT_RANK
#'
#' @author Adrian Antico
#' @family Misc
#' @param x X is your variable of interest
#' @examples
#' data <- data.table::data.table(A = runif(100))
#' data[, Rank := percRank(A)]
#' @return vector of percentile ranks
#' @examples
#' data <- data.table::data.table(A = runif(100))
#' data[, Percentile := percRank(A)]
#' @export
percRank <- function(x) {
  trunc(rank(x)) / length(x)
}

#' ProblematicFeatures identifies problematic features for machine learning
#'
#' ProblematicFeatures identifies problematic features for machine learning and outputs a data.table of the feature names in the first column and the metrics they failed to pass in the columns.
#'
#' @author Adrian Antico
#' @family EDA
#' @param data The data.table with the columns you wish to have analyzed
#' @param ColumnNumbers A vector with the column numbers you wish to analyze
#' @param NearZeroVarThresh Set to NULL to not run NearZeroVar(). Checks to see if the percentage of values in your numeric columns that are not constant are greater than the value you set here. If not, the feature is collects and returned with the percentage unique value.
#' @param CharUniqThresh Set to NULL to not run CharUniqthresh(). Checks to see if the percentage of unique levels / groups in your categorical feature is greater than the value you supply. If it is, the feature name is returned with the percentage unique value.
#' @param NA_Rate Set to NULL to not run NA_Rate(). Checks to see if the percentage of NA's in your features is greater than the value you supply. If it is, the feature name is returned with the percentage of NA values.
#' @param Zero_Rate Set to NULL to not run Zero_Rate(). Checks to see if the percentage of zero's in your features is greater than the value you supply. If it is, the feature name is returned with the percentage of zero values.
#' @param HighSkewThresh Set to NULL to not run HighSkew(). Checks for numeric columns whose ratio of the sum of the top 5th percentile of values to the bottom 95th percentile of values is greater than the value you supply. If true, the column name and value is returned.
#' @examples
#' test <- data.table::data.table(RandomNum = runif(1000))
#' test[, NearZeroVarEx := ifelse(runif(1000) > 0.99, runif(1), 1)]
#' test[, CharUniqueEx := as.factor(ifelse(RandomNum < 0.95, sample(letters, size = 1), "FFF"))]
#' test[, NA_RateEx := ifelse(RandomNum < 0.95, NA, "A")]
#' test[, ZeroRateEx := ifelse(RandomNum < 0.95, 0, runif(1))]
#' test[, HighSkewThreshEx := ifelse(RandomNum > 0.96, 100000, 1)]
#' ProblematicFeatures(test,
#'                     ColumnNumbers = 2:ncol(test),
#'                     NearZeroVarThresh = 0.05,
#'                     CharUniqThresh = 0.50,
#'                     NA_Rate = 0.20,
#'                     Zero_Rate = 0.20,
#'                     HighSkewThresh = 10)
#' @return data table with new dummy variables columns and optionally removes base columns
#' @export
ProblematicFeatures <- function(data,
                                ColumnNumbers = c(1:ncol(data)),
                                NearZeroVarThresh = 0.05,
                                CharUniqThresh = 0.50,
                                NA_Rate = 0.20,
                                Zero_Rate = 0.20,
                                HighSkewThresh = 10) {
  # Convert to data.table----
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)
  
  # Subset columns of interest----
  keep <- names(data)[ColumnNumbers]
  data <- data[, ..keep]
  
  # Define Functions for Calculations----
  LowVarianceFeatures <- function(data, NearZeroVarThresh = 0.05) {
    # Skip Option----
    if (is.null(NearZeroVarThresh))
      return(NULL)
    
    # Ensure argument is valid----
    if (NearZeroVarThresh > 1)
      warning("NearZeroVarThresh should be between zero and one")
    
    # Get Row Count----
    xx <- data[, .N]
    
    # Begin process----
    NumNearZeroVariance <- list()
    for (i in seq_len(ncol(data))) {
      if (is.numeric(data[[i]]) &
          length(unique(data[[i]])) / xx < NearZeroVarThresh) {
        NumNearZeroVariance[names(data)[i]] <-
          round(length(unique(data[[i]])) / xx, 4)
      }
    }
    
    if (length(NumNearZeroVariance) > 0) {
      a <-
        tryCatch({
          data.table::as.data.table(data.table::melt(NumNearZeroVariance))
        },
        error = function(x)
          NULL)
      if (dim(a)[1] != 0) {
        data.table::setnames(a,
                             c("L1", "value"),
                             c("ColName", "LowVarianceFeatures"))
        data.table::setcolorder(a, c(2, 1))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  HighCardinalityFeatures <- function(data, CharUniqThresh = 0.50) {
    # Skip Option----
    if (is.null(CharUniqThresh))
      return(NULL)
    
    # Ensure argument is valid----
    if (CharUniqThresh > 1)
      warning("CharUniqThresh should be between zero and one")
    
    # Get Row Count----
    xx <- data[, .N]
    
    # Begin process----
    CharUniqueTooHigh <- list()
    for (i in seq_len(ncol(data))) {
      if ((is.character(data[[i]]) |
           is.factor(data[[i]])) &
          length(unique(data[[i]])) / xx > CharUniqThresh) {
        CharUniqueTooHigh[names(data)[i]] <-
          round(length(unique(data[[i]])) / xx, 4)
      }
    }
    if (length(CharUniqueTooHigh) > 0) {
      a <-
        tryCatch({
          data.table::as.data.table(data.table::melt(CharUniqueTooHigh))
        },
        error = function(x)
          NULL)
      if (dim(a)[1] != 0) {
        data.table::setnames(a,
                             c("L1", "value"),
                             c("ColName", "HighCardinalityFeatures"))
        data.table::setcolorder(a, c(2, 1))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  HighMissingCountFeatures <- function(data, NA_Rate = 0.20) {
    # Skip Option----
    if (is.null(NA_Rate))
      return(NULL)
    
    # Ensure argument is valid----
    if (NA_Rate > 1)
      warning("HighSkewThresh should be between zero and one")
    
    # Get Row Count----
    xx <- data[, .N]
    
    # Begin process----
    LargeNAs <- list()
    for (i in seq_len(ncol(data))) {
      if (sum(is.na(data[[i]]) / xx) > NA_Rate) {
        LargeNAs[names(data)[i]] <- round(sum(is.na(data[[i]])) / xx, 4)
      }
    }
    if (length(LargeNAs) > 0) {
      a <-
        tryCatch({
          data.table::as.data.table(data.table::melt(LargeNAs))
        },
        error = function(x)
          NULL)
      if (dim(a)[1] != 0) {
        data.table::setnames(a,
                             c("L1", "value"),
                             c("ColName", "HighMissingCountFeatures"))
        data.table::setcolorder(a, c(2, 1))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  HighZeroCountFeatures <- function(data, Zero_Rate = 0.20) {
    # Skip Option----
    if (is.null(Zero_Rate))
      return(NULL)
    
    # Get Row Count----
    xx <- data[, .N]
    
    # Begin process----
    LargeZeros <- list()
    for (i in seq_len(ncol(data))) {
      if (is.numeric(data[[i]]) &
          data[get(names(data)[i]) == 0, .N] / xx > Zero_Rate) {
        LargeZeros[names(data)[i]] <-
          round(data[get(names(data)[i]) == 0, .N] / xx, 4)
      }
    }
    if (length(LargeZeros) > 0) {
      a <-
        tryCatch({
          data.table::as.data.table(data.table::melt(LargeZeros))
        },
        error = function(x)
          NULL)
      if (dim(a)[1] != 0) {
        data.table::setnames(a,
                             c("L1", "value"),
                             c("ColName", "HighZeroCountFeatures"))
        data.table::setcolorder(a, c(2, 1))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  HighSkewFeatures <- function(data, HighSkewThresh = 10) {
    # Skip Option----
    if (is.null(HighSkewThresh))
      return(NULL)
    
    # Ensure argument is valid----
    if (!is.numeric(HighSkewThresh) & !is.integer(HighSkewThresh)) {
      warning("HighSkewThresh should a numeric value")
    }
    
    # Get Row Count----
    xx <- data[, .N]
    
    # Begin process----
    HighSkew <- list()
    for (i in seq_len(ncol(data))) {
      if (is.numeric(data[[i]]) | is.integer(data[[i]])) {
        x <- sort(x = data[[i]],
                  na.last = TRUE,
                  decreasing = TRUE)
        if (!(max(data[[i]], na.rm = TRUE) == 0 &
              min(data[[i]], na.rm = TRUE) == 0)) {
          if (sum(x[1:(length(x) * (1 - 0.95))], na.rm = TRUE) /
              sum(x[(length(x) * (1 - 0.95)):xx], na.rm = TRUE) > HighSkewThresh) {
            HighSkew[names(data)[i]] <-
              round(sum(x[1:length(x) * 0.05], na.rm = TRUE) /
                      sum(x[(floor(length(x) *
                                     (0.95)) + 1):length(x)], na.rm = TRUE), 4)
          }
        }
      }
    }
    if (length(HighSkew) > 0) {
      a <-
        tryCatch({
          data.table::as.data.table(data.table::melt(HighSkew))
        },
        error = function(x)
          NULL)
      if (dim(a)[1] != 0) {
        data.table::setnames(a, c("L1", "value"), c("ColName", "HighSkewFeatures"))
        data.table::setcolorder(a, c(2, 1))
        return(a)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  
  # Initalize collection
  collect <- list()
  z <- 0
  
  # LowVarianceFeatures Run----
  a <-
    tryCatch({
      LowVarianceFeatures(data, NearZeroVarThresh = NearZeroVarThresh)
    },
    error = function(x)
      NULL)
  if (!is.null(a)) {
    z <- z + 1
    collect[[z]] <- a
  }
  
  # HighCardinalityFeatures Run----
  b <-
    tryCatch({
      HighCardinalityFeatures(data, CharUniqThresh = CharUniqThresh)
    },
    error = function(x)
      NULL)
  if (!is.null(b)) {
    z <- z + 1
    collect[[z]] <- b
  }
  
  # HighMissingCountFeatures Run----
  c <- tryCatch({
    HighMissingCountFeatures(data, NA_Rate = NA_Rate)
  },
  error = function(x)
    NULL)
  if (!is.null(c)) {
    z <- z + 1
    collect[[z]] <- c
  }
  
  # HighZeroCountFeatures Run----
  d <-
    tryCatch({
      HighZeroCountFeatures(data, Zero_Rate = Zero_Rate)
    },
    error = function(x)
      NULL)
  if (!is.null(d)) {
    z <- z + 1
    collect[[z]] <- d
  }
  
  # HighSkewFeatures Run----
  e <-
    tryCatch({
      HighSkewFeatures(data, HighSkewThresh = HighSkewThresh)
    },
    error = function(x)
      NULL)
  if (!is.null(e)) {
    z <- z + 1
    collect[[z]] <- e
  }
  
  # Combine Outputs
  if (length(collect) == 0) {
    return(NULL)
  } else if (length(collect) == 1) {
    return(collect[[1]])
  } else {
    for (x in seq_len(length(collect))) {
      if (x == 1) {
        val <- collect[[x]]
      } else {
        temp <- collect[[x]]
        val <- merge(val, temp, by = "ColName", all = TRUE)
      }
    }
    return(val)
  }
}

#' ProblematicRecords identifies problematic records for further investigation
#'
#' ProblematicRecords identifies problematic records for further investigation and data.table with 3 additional columns at the beginning of the data.table: PredictedOutlier (0 = no outlier, 1 = outlier), predict (raw H2O predicted value from Isolation Forest), and mean_length (mean length of number of splits)
#'
#' @author Adrian Antico
#' @family EDA
#' @param data The data.table with the columns you wish to have analyzed
#' @param ColumnNumbers A vector with the column numbers you wish to analyze
#' @param Threshold Quantile value to find the cutoff value for classifying outliers
#' @param MaxMem Specify the amount of memory to allocate to H2O. E.g. "28G"
#' @param NThreads Specify the number of threads (E.g. cores * 2)
#' @param NTrees Specify the number of decision trees to build
#' @param SampleRate Specify the row sample rate per tree
#' @examples
#' \donttest{
#'  Correl <- 0.85
#' N <- 10000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Target := as.factor(
#'  ifelse(Independent_Variable2 < 0.20, "A",
#'         ifelse(Independent_Variable2 < 0.40, "B",
#'                ifelse(Independent_Variable2 < 0.6,  "C",
#'                       ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, Independent_Variable11 := as.factor(
#'  ifelse(Independent_Variable2 < 0.15, "A",
#'         ifelse(Independent_Variable2 < 0.45, "B",
#'                ifelse(Independent_Variable2 < 0.65,  "C",
#'                       ifelse(Independent_Variable2 < 0.85,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' Outliers <- ProblematicRecords(data,
#'                               ColumnNumbers = NULL,
#'                               Threshold = 0.95,
#'                               MaxMem = "28G",
#'                               NThreads = -1)
#' }
#' @return A data.table
#' @export
ProblematicRecords <- function(data,
                               ColumnNumbers = NULL,
                               Threshold = 0.975,
                               MaxMem = "28G",
                               NThreads = -1,
                               NTrees = 100,
                               SampleRate = (sqrt(5) - 1) / 2) {
  # Ensure H2O is installed----
  if (!requireNamespace("h2o")) {
    warning("Install H2O to run this function")
  }
  
  # Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Initialize H2O----
  h2o::h2o.init(
    max_mem_size = MaxMem,
    nthreads = NThreads,
    enable_assertions = FALSE
  )
  
  # Ensure Characters are Converted to Factors----
  data <- RemixAutoML::ModelDataPrep(data,
                                     Impute = FALSE,
                                     CharToFactor = TRUE)
  
  # Convert data to H2O Frame----
  Data <- h2o::as.h2o(data)
  
  # Build Isolation Forest----
  if (is.null(ColumnNumbers)) {
    IsolationForest <- h2o::h2o.isolationForest(
      training_frame = Data,
      x = names(data),
      model_id = "test",
      ntrees = NTrees,
      sample_rate = SampleRate
    )
  } else {
    IsolationForest <- h2o::h2o.isolationForest(
      training_frame = Data,
      x = names(data)[ColumnNumbers],
      model_id = "test",
      ntrees = NTrees,
      sample_rate = SampleRate
    )
  }
  
  # Generate Outliers data.table----
  OutliersRaw <-
    data.table::as.data.table(h2o::h2o.predict(object = IsolationForest,
                                               newdata = Data))
  
  # Shutdown H2O
  h2o::h2o.shutdown(prompt = FALSE)
  
  # Add column for outlier indicator----
  setnames(OutliersRaw,
           c("predict", "mean_length"),
           c("PredictIsoForest", "MeanLength"))
  Cutoff <- quantile(OutliersRaw[["PredictIsoForest"]],
                     probs = Threshold)[[1]]
  OutliersRaw[, PredictedOutlier := ifelse(PredictIsoForest > Cutoff, 1, 0)]
  OutliersRaw[, PercentileRank := percRank(PredictIsoForest)]
  data.table::setcolorder(OutliersRaw, c(4, 3, 1, 2))
  
  # Merge back with source data----
  OutputData <- cbind(OutliersRaw, data)
  
  # Return data----
  return(OutputData[order(-PredictIsoForest)])
}

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

#' GenTSAnomVars is an automated z-score anomaly detection via GLM-like procedure
#'
#' GenTSAnomVars is an automated z-score anomaly detection via GLM-like procedure. Data is z-scaled and grouped by factors and time periods to determine which points are above and below the control limits in a cumulative time fashion. Then a cumulative rate is created as the final variable. Set KeepAllCols to FALSE to utilize the intermediate features to create rolling stats from them. The anomalies are separated into those that are extreme on the positive end versus those that are on the negative end.
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#' @param data the source residuals data.table
#' @param ValueCol the numeric column to run anomaly detection over
#' @param GroupVar1 this is a group by variable
#' @param GroupVar2 this is another group by variable
#' @param DateVar this is a time variable for grouping
#' @param HighThreshold this is the threshold on the high end
#' @param LowThreshold this is the threshold on the low end
#' @param KeepAllCols set to TRUE to remove the intermediate features
#' @param IsDataScaled set to TRUE if you already scaled your data
#' @examples
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(10000,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:10000)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' x <- data.table::as.data.table(sde::GBM(N=10000)*1000)
#' data[, predicted := x[-1,]]
#' stuff <- GenTSAnomVars(data,
#'                        ValueCol = "Target",
#'                        GroupVar1 = NULL,
#'                        GroupVar2 = NULL,
#'                        DateVar = "DateTime",
#'                        HighThreshold = 1.96,
#'                        LowThreshold = -1.96,
#'                        KeepAllCols = TRUE,
#'                        IsDataScaled  = FALSE)
#' @return The original data.table with the added columns merged in. When KeepAllCols is set to FALSE, you will get back two columns: AnomHighRate and AnomLowRate - these are the cumulative anomaly rates over time for when you get anomalies from above the thresholds (e.g. 1.96) and below the thresholds.
#' @export
GenTSAnomVars <- function(data,
                          ValueCol    = "Value",
                          GroupVar1   = "SKU",
                          GroupVar2   = NULL,
                          DateVar     = "DATE",
                          HighThreshold = 1.96,
                          LowThreshold  = -1.96,
                          KeepAllCols = FALSE,
                          IsDataScaled  = TRUE) {
  # Check data.table
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)
  
  # Scale data if not already
  if (!IsDataScaled) {
    data[, eval(ValueCol) := scale(get(ValueCol),
                                   center = TRUE,
                                   scale = TRUE)]
  }
  
  # Global check for date
  if (!is.null(DateVar)) {
    if (is.null(GroupVar1) & is.null(GroupVar2)) {
      data <- data[order(get(DateVar))]
      data[, RowNumAsc := 1:.N]
      data[, AnomHigh := as.numeric(ifelse(get(ValueCol) > HighThreshold,
                                           1, 0))]
      data[, AnomLow := as.numeric(ifelse(get(ValueCol) < LowThreshold,
                                          1, 0))]
      data[, CumAnomHigh := cumsum(AnomHigh)]
      data[, CumAnomLow := cumsum(AnomLow)]
      data[, AnomHighRate := CumAnomHigh / RowNumAsc]
      data[, AnomLowRate := CumAnomLow / RowNumAsc]
      if (!KeepAllCols) {
        data[, ':=' (
          AnomHigh = NULL,
          AnomLow = NULL,
          CumAnomHigh = NULL,
          CumAnomLow = NULL,
          RowNumAsc = NULL
        )]
      }
    } else if (is.null(GroupVar2) & !is.null(GroupVar1)) {
      data <- data[order(get(GroupVar1), get(DateVar))]
      data[, RowNumAsc := 1:.N, by = get(GroupVar1)]
      data[, AnomHigh := as.numeric(ifelse(get(ValueCol) > HighThreshold,
                                           1, 0))]
      data[, AnomLow := as.numeric(ifelse(get(ValueCol) < LowThreshold,
                                          1, 0))]
      data[, CumAnomHigh := cumsum(AnomHigh), by = get(GroupVar1)]
      data[, CumAnomLow := cumsum(AnomLow), by = get(GroupVar1)]
      data[, AnomHighRate := CumAnomHigh / RowNumAsc]
      data[, AnomLowRate := CumAnomLow / RowNumAsc]
      if (!KeepAllCols) {
        data[, ':=' (
          AnomHigh = NULL,
          AnomLow = NULL,
          CumAnomHigh = NULL,
          CumAnomLow = NULL,
          RowNumAsc = NULL
        )]
      }
    } else if (!is.null(GroupVar1) & !is.null(GroupVar2)) {
      data <- data[order(get(GroupVar1), get(GroupVar2),
                         get(DateVar))]
      data[, RowNumAsc := 1:.N, by = list(get(GroupVar1),
                                          get(GroupVar2))]
      data[, AnomHigh := as.numeric(ifelse(get(ValueCol) > HighThreshold,
                                           1, 0))]
      data[, AnomLow := as.numeric(ifelse(get(ValueCol) < LowThreshold,
                                          1, 0))]
      data[, CumAnomHigh := cumsum(AnomHigh),
           by = list(get(GroupVar1),
                     get(GroupVar2))]
      data[, CumAnomLow := cumsum(AnomLow),
           by = list(get(GroupVar1),
                     get(GroupVar2))]
      data[, paste0(GroupVar2,
                    "AnomHighRate") := CumAnomHigh / RowNumAsc]
      data[, paste0(GroupVar2,
                    "AnomLowRate") := CumAnomLow / RowNumAsc]
      if (!KeepAllCols) {
        data[, ':=' (
          AnomHigh = NULL,
          AnomLow = NULL,
          CumAnomHigh = NULL,
          CumAnomLow = NULL,
          RowNumAsc = NULL
        )]
      }
    }
    return(data)
  }
  return(NULL)
}

#' ResidualOutliers is an automated time series outlier detection function
#'
#' ResidualOutliers is an automated time series outlier detection function that utilizes tsoutliers and auto.arima. It looks for five types of outliers: "AO" Additive outliter - a singular extreme outlier that surrounding values aren't affected by; "IO" Innovational outlier - Initial outlier with subsequent anomalous values; "LS" Level shift - An initial outlier with subsequent observations being shifted by some constant on average; "TC" Transient change - initial outlier with lingering effects that dissapate exponentially over time; "SLS" Seasonal level shift - similar to level shift but on a seasonal scale.
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#' @param data the source residuals data.table
#' @param DateColName The name of your data column to use in reference to the target variable
#' @param TargetColName The name of your target variable column
#' @param PredictedColName The name of your predicted value column. If you supply this, you will run anomaly detection of the difference between the target variable and your predicted value. If you leave PredictedColName NULL then you will run anomaly detection over the target variable.
#' @param TimeUnit The time unit of your date column: hour, day, week, month, quarter, year
#' @param maxN the largest lag or moving average (seasonal too) values for the arima fit
#' @param tstat the t-stat value for tsoutliers
#' @examples
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'                                Target = as.numeric(stats::filter(rnorm(1000,
#'                                                                        mean = 50,
#'                                                                        sd = 20),
#'                                                                  filter=rep(1,10),
#'                                                                  circular=TRUE)))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' data[, Predicted := as.numeric(stats::filter(rnorm(1000,
#'                                                    mean = 50,
#'                                                    sd = 20),
#'                                              filter=rep(1,10),
#'                                              circular=TRUE))]
#' stuff <- ResidualOutliers(data = data,
#'                           DateColName = "DateTime",
#'                           TargetColName = "Target",
#'                           PredictedColName = NULL,
#'                           TimeUnit = "day",
#'                           maxN = 5,
#'                           tstat = 4)
#' data     <- stuff[[1]]
#' model    <- stuff[[2]]
#' outliers <- data[type != "<NA>"]
#' @return A named list containing FullData = original data.table with outliers data and ARIMA_MODEL = the arima model.
#' @export
ResidualOutliers <- function(data,
                             DateColName = "DateTime",
                             TargetColName = "Target",
                             PredictedColName = NULL,
                             TimeUnit = "day",
                             maxN = 5,
                             tstat = 2) {
  # Define TS Frequency
  if (tolower(TimeUnit) == "hour") {
    freq <- 24
  } else if (tolower(TimeUnit) == "day") {
    freq <- 365
  } else if (tolower(TimeUnit) == "week") {
    freq <- 52
  } else if (tolower(TimeUnit) == "month") {
    freq <- 12
  } else if (tolower(TimeUnit) == "quarter") {
    freq <- 4
  } else if (tolower(TimeUnit) == "year") {
    freq <- 1
  } else {
    warning("TimeUnit is not in hour, day, week, month,
    quarter, or year")
  }
  
  # Ensure data is a data.table
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Ensure data is sorted
  data.table::setorderv(x = data,
                        cols = eval(DateColName),
                        order = 1)
  
  # Keep columns
  if (!is.null(PredictedColName)) {
    data[, Residuals := get(TargetColName) - get(PredictedColName)]
  } else {
    data[, Residuals := get(TargetColName)]
  }
  keep <- c(DateColName, "Residuals")
  temp <- data[, ..keep]
  MinVal <- min(data[[eval(TargetColName)]], na.rm = TRUE)
  
  # Convert to time series object
  tsData <- stats::ts(temp,
                      start = temp[, min(get(DateColName))][[1]],
                      frequency = freq)
  
  # Build the auto arimia
  if (MinVal > 0) {
    fit <-
      tryCatch({
        forecast::auto.arima(
          y = tsData[, "Residuals"],
          max.p = maxN,
          max.q = maxN,
          max.P = maxN,
          max.Q = maxN,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          lambda = TRUE,
          biasadj = TRUE,
          stepwise = TRUE
        )
      },
      error = function(x)
        "empty")
  } else {
    fit <-
      tryCatch({
        forecast::auto.arima(
          y = tsData[, "Residuals"],
          max.p = maxN,
          max.q = maxN,
          max.P = maxN,
          max.Q = maxN,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          lambda = FALSE,
          biasadj = FALSE,
          stepwise = TRUE
        )
      },
      error = function(x)
        "empty")
  }
  
  # Store the arima parameters
  pars  <- tsoutliers::coefs2poly(fit)
  
  # Store the arima residuals
  resid <- cbind(tsData, stats::residuals(fit))
  
  # Find the outliers
  x <- data.table::as.data.table(tsoutliers::locate.outliers(
    resid = resid[, 3],
    pars = pars,
    cval = tstat,
    types = c("AO", "TC", "LS", "IO", "SLS")
  ))
  
  # Merge back to source data
  residDT <- data.table::as.data.table(resid)
  z <- cbind(data, residDT)
  z[, ind := 1:.N]
  data.table::setnames(z,
                       names(z)[c((ncol(z) - 3):(ncol(z) - 1))],
                       c("ObsNum", "Preds", "ARIMA_Residuals"))
  z[, ObsNum := NULL]
  data <- merge(z, x, by = "ind", all.x = TRUE)
  data[, ':=' (ind = NULL, coefhat = NULL)]
  data[type == "<NA>", type := NA]
  
  # Reorder data, remove the coefhat column to send to database or stakeholder
  return(list(FullData = data, ARIMA_MODEL = fit))
}

#' AutoKMeans Automated row clustering for mixed column types
#'
#' AutoKMeans adds a column to your original data with a cluster number identifier. Uses glrm (grid tune-able) and then k-means to find optimal k.
#'
#' @author Adrian Antico
#' @family Unsupervised Learning
#' @param data is the source time series data.table
#' @param GridTuneGLRM If you want to grid tune the glrm model, set to TRUE, FALSE otherwise
#' @param GridTuneKMeans If you want to grid tuen the KMeans model, set to TRUE, FALSE otherwise
#' @param nthreads set based on number of threads your machine has available
#' @param MaxMem set based on the amount of memory your machine has available
#' @param glrmCols the column numbers for the glrm
#' @param IgnoreConstCols tell H2O to ignore any columns that have zero variance
#' @param glrmFactors similar to the number of factors to return from PCA
#' @param Loss set to one of "Quadratic", "Absolute", "Huber", "Poisson", "Hinge", "Logistic", "Periodic"
#' @param glrmMaxIters max number of iterations
#' @param SVDMethod choose from "Randomized","GramSVD","Power"
#' @param MaxRunTimeSecs set the timeout for max run time
#' @param KMeansK number of factors to test out in k-means to find the optimal number
#' @param KMeansMetric pick the metric to identify top model in grid tune c("totss","betweenss","withinss")
#' @param SaveModels Set to "standard", "mojo", or NULL (default)
#' @param PathFile Set to folder where you will keep the models
#' @examples
#' \donttest{
#' data <- data.table::as.data.table(iris)
#' data <- AutoKMeans(data,
#'                    nthreads = 8,
#'                    MaxMem = "28G",
#'                    SaveModels = NULL,
#'                    PathFile = NULL,
#'                    GridTuneGLRM = TRUE,
#'                    GridTuneKMeans = TRUE,
#'                    glrmCols = 1:(ncol(data)-1),
#'                    IgnoreConstCols = TRUE,
#'                    glrmFactors = 2,
#'                    Loss = "Absolute",
#'                    glrmMaxIters = 1000,
#'                    SVDMethod = "Randomized",
#'                    MaxRunTimeSecs = 3600,
#'                    KMeansK = 5,
#'                    KMeansMetric = "totss")
#' unique(data[["Species"]])
#' unique(data[["ClusterID"]])
#' temp <- data[, mean(ClusterID), by = "Species"]
#' Setosa <- round(temp[Species == "setosa", V1][[1]],0)
#' Versicolor <- round(temp[Species == "versicolor", V1][[1]],0)
#' Virginica <- round(temp[Species == "virginica", V1][[1]],0)
#' data[, Check := "a"]
#' data[ClusterID == eval(Setosa), Check := "setosa"]
#' data[ClusterID == eval(Virginica), Check := "virginica"]
#' data[ClusterID == eval(Versicolor), Check := "versicolor"]
#' data[, Acc := as.numeric(ifelse(Check == Species, 1, 0))]
#' data[, mean(Acc)][[1]]
#' }
#' @return Original data.table with added column with cluster number identifier
#' @export
AutoKMeans <- function(data,
                       nthreads        = 8,
                       MaxMem          = "28G",
                       SaveModels      = NULL,
                       PathFile        = NULL,
                       GridTuneGLRM    = TRUE,
                       GridTuneKMeans  = TRUE,
                       glrmCols        = c(1:5),
                       IgnoreConstCols = TRUE,
                       glrmFactors     = 5,
                       Loss            = "Absolute",
                       glrmMaxIters    = 1000,
                       SVDMethod       = "Randomized",
                       MaxRunTimeSecs  = 3600,
                       KMeansK         = 50,
                       KMeansMetric    = "totss") {
  # Check Arguments----
  if (nthreads < 0) {
    warning("nthreads needs to be a positive integer")
  }
  if (!is.character(MaxMem)) {
    warning("MaxMem needs to be a character value. E.g. MaxMem = '28G'")
  }
  if (!is.null(SaveModels)) {
    if (!(tolower(SaveModels) %chin% c("mojo", "standard"))) {
      warning("SaveModels needs to be either NULL, 'mojo', or 'standard'")
    }
  }
  if (!is.null(FilePath)) {
    if (!is.character(FilePath)) {
      warning("FilePath needs to resolve to a character value. E.g. getwd()")
    }
  }
  if (!is.logical(GridTuneGLRM)) {
    warning("GridTuneGLRM needs to be either TRUE or FALSE")
  }
  if (!is.logical(GridTuneKMeans)) {
    warning("GridTuneKMeans needs to be either TRUE or FALSE")
  }
  if (!(is.numeric(glrmCols) | is.integer(glrmCols))) {
    warning("glrmCols needs to be the column numbers")
  }
  if (!is.logical(IgnoreConstCols)) {
    warning("IgnoreConstCols needs to be either TRUE or FALSE")
  }
  if (!(is.numeric(glrmFactors) | is.integer(glrmFactors))) {
    warning("glrmFactors needs to be an integer value")
  }
  
  # Check data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Set up Scoring File if SaveModels is not NULL----
  if (!is.null(SaveModels)) {
    KMeansModelFile <- data.table::data.table(
      Name = c("GLMR", "AutoKMeans"),
      FilePath1 = rep("bla", 2),
      FilePath2 = rep("bla", 2)
    )
  }
  
  # Build glmr model----
  h2o::h2o.init(nthreads = nthreads, max_mem_size = MaxMem)
  datax <- h2o::as.h2o(data)
  if (GridTuneGLRM) {
    # Define grid tune search scheme in a named list----
    search_criteria  <-
      list(
        strategy             = "RandomDiscrete",
        max_runtime_secs     = 3600,
        max_models           = 30,
        seed                 = 1234,
        stopping_rounds      = 10,
        stopping_metric      = "MSE",
        stopping_tolerance   = 1e-3
      )
    
    # Define hyperparameters----
    HyperParams <-
      list(
        transform        = c("NONE",
                             "DEMEAN",
                             "DESCALE",
                             "STANDARDIZE"),
        k                = 1:5,
        regularization_x = c(
          "None",
          "Quadratic",
          "L2",
          "L1",
          "NonNegative",
          "OneSparse",
          "UnitOneSparse",
          "Simplex"
        ),
        regularization_y = c(
          "None",
          "Quadratic",
          "L2",
          "L1",
          "NonNegative",
          "OneSparse",
          "UnitOneSparse",
          "Simplex"
        ),
        gamma_x          = seq(0.01, 0.10, 0.01),
        gamma_y          = seq(0.01, 0.10, 0.01),
        svd_method       = c("Randomized",
                             "GramSVD",
                             "Power")
      )
    
    # Run grid tune----
    grid <- h2o::h2o.grid(
      "glrm",
      search_criteria   = search_criteria,
      training_frame    = datax,
      grid_id           = "GLRM",
      ignore_const_cols = IgnoreConstCols,
      loss              = Loss,
      hyper_params      = HyperParams
    )
    
    # Get best performer----
    Grid_Out <-
      h2o::h2o.getGrid(
        grid_id = "GLRM",
        sort_by = search_criteria$stopping_metric,
        decreasing = FALSE
      )
    model <- h2o::h2o.getModel(model_id = Grid_Out@model_ids[[1]])
  } else {
    model <- h2o::h2o.glrm(
      training_frame    = datax,
      cols              = glrmCols,
      ignore_const_cols = IgnoreConstCols,
      k                 = glrmFactors,
      loss              = Loss,
      max_iterations    = glrmMaxIters,
      svd_method        = SVDMethod,
      max_runtime_secs  = MaxRunTimeSecs
    )
  }
  
  # Save model if requested----
  if (!is.null(SaveModels)) {
    # Save archetypes and colnames----
    fitY <- model@model$archetypes
    save(fitY, file = paste0(PathFile, "/fitY"))
    data.table::set(
      KMeansModelFile,
      i = 1L,
      j = 2L,
      value = paste0(PathFile, "/fitY")
    )
  }
  
  # Run k-means----
  if (GridTuneKMeans) {
    # GLRM output----
    x_raw <- h2o::h2o.getFrame(model@model$representation_name)
    Names <- colnames(x_raw)
    if (!is.null(SaveModels)) {
      save(Names, file = paste0(PathFile, "/Names.Rdata"))
      data.table::set(
        KMeansModelFile,
        i = 1L,
        j = 3L,
        value = paste0(PathFile, "/Names.Rdata")
      )
      save(KMeansModelFile,
           file = paste0(PathFile, "/KMeansModelFile.Rdata"))
    }
    
    # Define grid tune search scheme in a named list----
    search_criteria  <-
      list(
        strategy             = "RandomDiscrete",
        max_runtime_secs     = 3600,
        max_models           = 30,
        seed                 = 1234,
        stopping_rounds      = 10
      )
    
    # Define hyperparameters----
    HyperParams <- list(
      max_iterations   = c(10, 20, 50, 100),
      init             = c("Random", "PlusPlus", "Furthest")
    )
    
    # Run grid tune----
    grid <- h2o::h2o.grid(
      "kmeans",
      search_criteria   = search_criteria,
      training_frame    = x_raw,
      x                 = Names,
      k                 = KMeansK,
      grid_id           = "KMeans",
      estimate_k        = TRUE,
      hyper_params      = HyperParams
    )
    
    # Get best performer----
    Grid_Out <-
      h2o::h2o.getGrid(grid_id = "KMeans",
                       sort_by = KMeansMetric,
                       decreasing = FALSE)
    model <- h2o::h2o.getModel(model_id = Grid_Out@model_ids[[1]])
  } else {
    # GLRM output----
    x_raw <- h2o::h2o.getFrame(model@model$representation_name)
    Names <- colnames(x_raw)
    if (!is.null(SaveModels)) {
      save(Names, file = paste0(PathFile, "/Names.Rdata"))
      data.table::set(
        KMeansModelFile,
        i = 1L,
        j = 3L,
        value = paste0(PathFile, "/Names.Rdata")
      )
      save(KMeansModelFile,
           file = paste0(PathFile, "/KMeansModelFile.Rdata"))
    }
    
    # Train KMeans----
    model <- h2o::h2o.kmeans(
      training_frame = x_raw,
      x              = Names,
      k              = KMeansK,
      estimate_k     = TRUE
    )
  }
  
  # Save model if requested----
  if (!is.null(SaveModels)) {
    if (tolower(SaveModels) == "mojo") {
      save_model <-
        h2o::h2o.saveMojo(object = model,
                          path = PathFile,
                          force = TRUE)
      h2o::h2o.download_mojo(
        model = model,
        path = PathFile,
        get_genmodel_jar = TRUE,
        genmodel_path = PathFile,
        genmodel_name = "KMeans"
      )
      data.table::set(KMeansModelFile,
                      i = 2L,
                      j = 2L,
                      value = save_model)
      data.table::set(
        KMeansModelFile,
        i = 2L,
        j = 3L,
        value = paste0(PathFile, "/KMeans")
      )
      save(KMeansModelFile,
           file = paste0(PathFile,
                         "/KMeansModelFile.Rdata"))
    } else if (tolower(SaveModels) == "standard") {
      save_model <-
        h2o::h2o.saveModel(object = model,
                           path = PathFile,
                           force = TRUE)
      data.table::set(KMeansModelFile,
                      i = 2L,
                      j = 2L,
                      value = save_model)
      save(KMeansModelFile,
           file = paste0(PathFile,
                         "/KMeansModelFile.Rdata"))
    }
  }
  
  # Combine outputs----
  preds <- data.table::as.data.table(h2o::h2o.predict(model, x_raw))
  h2o::h2o.shutdown(prompt = FALSE)
  data <- data.table::as.data.table(cbind(preds, data))
  data.table::setnames(data, "predict", "ClusterID")
  return(data)
}

#' Final Data Preparation Function
#'
#' This function replaces inf values with NA, converts characters to factors, and imputes with constants
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data This is your source data you'd like to modify
#' @param Impute Defaults to TRUE which tells the function to impute the data
#' @param CharToFactor Defaults to TRUE which tells the function to convert characters to factors
#' @param RemoveDates Defaults to FALSE. Set to TRUE to remove date columns from your data.table
#' @param MissFactor Supply the value to impute missing factor levels
#' @param MissNum Supply  the value to impute missing numeric values
#' @param IgnoreCols Supply column numbers for columns you want the function to ignore
#' @examples
#' data <- data.table::data.table(Value = runif(100000),
#'                                FactorCol = as.character(sample(x = c(letters,
#'                                                                      LETTERS,
#'                                                                      paste0(letters,letters),
#'                                                                      paste0(LETTERS,LETTERS),
#'                                                                      paste0(letters,LETTERS),
#'                                                                      paste0(LETTERS,letters)),
#'                                                                size = 100000,
#'                                                                replace = TRUE)))
#' data <- ModelDataPrep(data,
#'                       Impute = TRUE,
#'                       CharToFactor = TRUE,
#'                       MissFactor = "0",
#'                       MissNum    = -1)
#' @return Returns the original data table with corrected values
#' @export
ModelDataPrep <- function(data,
                          Impute       = TRUE,
                          CharToFactor = TRUE,
                          RemoveDates  = FALSE,
                          MissFactor   = "0",
                          MissNum      = -1,
                          IgnoreCols   = NULL) {
  # Check data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Prepare columns for action----
  x <- seq_along(data)
  if (!is.null(IgnoreCols)) {
    x <- setdiff(x, IgnoreCols)
  }
  
  # Replace any inf values with NA----
  for (col in x) {
    data.table::set(data,
                    j = col,
                    value = replace(data[[col]],
                                    is.infinite(data[[col]]), NA))
  }
  
  # Turn character columns into factors----
  if (CharToFactor) {
    for (col in x) {
      if (is.character(data[[col]])) {
        data.table::set(data,
                        j = col,
                        value = as.factor(data[[col]]))
      }
    }
  }
  
  # Impute missing values----
  if (Impute) {
    for (col in x) {
      if (is.factor(data[[col]])) {
        data.table::set(data,
                        which(!(data[[col]] %in% levels(data[[col]]))),
                        col,
                        MissFactor)
      } else {
        data.table::set(data,
                        which(base::is.na(data[[col]])),
                        col,
                        MissNum)
      }
    }
  }
  
  # Remove Dates----
  if (RemoveDates) {
    for (col in rev(x)) {
      if (!is.character(data[[col]]) &
          !is.factor(data[[col]]) &
          !is.numeric(data[[col]]) &
          !is.integer(data[[col]]) &
          !is.logical(data[[col]]) &
          !is.complex(data[[col]])) {
        data[, paste0(names(data)[col]) := NULL]
      }
    }
  }
  return(data)
}

#' RedYellowGreen is for determining the optimal thresholds for binary classification when do-nothing is an option
#'
#' This function will find the optimial thresholds for applying the main label and for finding the optimial range for doing nothing when you can quantity the cost of doing nothing
#'
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @param data data is the data table with your predicted and actual values from a classification model
#' @param PredictColNumber The column number where the prediction variable is located (in binary form)
#' @param ActualColNumber The column number where the target variable is located
#' @param TruePositiveCost This is the utility for generating a true positive prediction
#' @param TrueNegativeCost This is the utility for generating a true negative prediction
#' @param FalsePositiveCost This is the cost of generating a false positive prediction
#' @param FalseNegativeCost This is the cost of generating a false negative prediction
#' @param MidTierCost This is the cost of doing nothing (or whatever it means to not classify in your case)
#' @param Cores Number of cores on your machine
#' @param Precision Set the decimal number to increment by between 0 and 1
#' @param Boundaries Supply a vector of two values c(lower bound, upper bound) where the first value is the smallest threshold you want to test and the second value is the largest value you want to test. Note, if your results are at the boundaries you supplied, you should extent the boundary that was reached until the values is within both revised boundaries.
#' @import foreach
#' @examples
#' data <- data.table::data.table(Target = runif(10))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(10)]
#' data[, Predict := log(pnorm(0.85 * x1 +
#'                               sqrt(1-0.85^2) * qnorm(x2)))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' data <- RedYellowGreen(data,
#'                        PredictColNumber  = 2,
#'                        ActualColNumber   = 1,
#'                        TruePositiveCost  = 0,
#'                        TrueNegativeCost  = 0,
#'                        FalsePositiveCost = -1,
#'                        FalseNegativeCost = -2,
#'                        MidTierCost = -0.5,
#'                        Precision = 0.5,
#'                        Cores = 1,
#'                        Boundaries = c(0.05,0.75))
#' @return A data table with all evaluated strategies, parameters, and utilities, along with a 3d scatterplot of the results
#' @export
RedYellowGreen <- function(data,
                           PredictColNumber  = 2,
                           ActualColNumber   = 1,
                           TruePositiveCost  = 0,
                           TrueNegativeCost  = 0,
                           FalsePositiveCost = -10,
                           FalseNegativeCost = -50,
                           MidTierCost       = -2,
                           Cores             = 8,
                           Precision         = 0.01,
                           Boundaries        = c(0.05, 0.75)) {
  requireNamespace('doParallel', quietly = FALSE)
  requireNamespace('parallel', quietly = FALSE)
  
  # Check data.table
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)
  
  # Ensure arguments are valid
  if (is.character(TruePositiveCost))
    warning("TruePositiveCost must be numeric")
  if (is.character(TrueNegativeCost))
    warning("TruePositiveCost must be numeric")
  if (is.character(FalsePositiveCost))
    warning("TruePositiveCost must be numeric")
  if (is.character(FalseNegativeCost))
    warning("TruePositiveCost must be numeric")
  if (is.character(MidTierCost))
    warning("TruePositiveCost must be numeric")
  if (Precision < 0 | Precision > 0.5)
    warning("Precision should be a decimal value greater than 0 and less than 0.5")
  if (min(Boundaries) < 0 | max(Boundaries) > 0.5)
    warning("Boundaries should be a decimal value greater than 0 and less than 0.5")
  if (Boundaries[1] > Boundaries[2])
    warning("The first Boundaries element should be less than the second element")
  
  # Set up evaluation table
  analysisTable <- data.table::data.table(
    TPP = base::rep(TruePositiveCost, 1),
    TNP = base::rep(TrueNegativeCost, 1),
    FPP = base::rep(FalsePositiveCost, 1),
    FNP = base::rep(FalseNegativeCost, 1),
    MTDN = base::rep(TRUE, 1),
    MTC = base::rep(MidTierCost, 1),
    Threshold = runif(1)
  )
  
  # Do nothing possibilities
  temp     <-
    data.table::CJ(
      MTLT = seq(Boundaries[1], Boundaries[2], Precision),
      MTHT = seq(Boundaries[1], Boundaries[2], Precision)
    )[MTHT > MTLT]
  new      <- cbind(analysisTable, temp)
  new[, Utility := stats::runif(nrow(new))]
  
  # Parallel components
  requireNamespace(c("parallel", "doParallel", "foreach"))
  packages <- c("data.table")
  cores    <- Cores
  bat      <- base::ceiling(nrow(new) / cores)
  parts    <- base::floor(nrow(new) / bat)
  cl       <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  
  # Kick off run
  results <-
    foreach::foreach(
      i            = itertools::isplitRows(new, chunks = parts),
      .combine      = function(...)
        data.table::rbindlist(list(...)),
      .multicombine = TRUE,
      .packages     = packages
    ) %dopar% {
      RedYellowGreenParallel <- function(data,
                                         PredictColNumber  = 1,
                                         ActualColNumber   = 767,
                                         TruePositiveCost  = 0,
                                         TrueNegativeCost  = 0,
                                         FalsePositiveCost = -1,
                                         FalseNegativeCost = -10,
                                         MidTierCost       = -5,
                                         new = i) {
        # Loop through all combos
        for (k in base::as.integer(seq_len(nrow(new)))) {
          x <- threshOptim(
            data = data,
            actTar = base::names(data)[ActualColNumber],
            predTar = base::names(data)[PredictColNumber],
            tpProfit = TruePositiveCost,
            tnProfit = TrueNegativeCost,
            fpProfit = FalsePositiveCost,
            fnProfit = FalseNegativeCost,
            MidTierDoNothing = TRUE,
            MidTierCost = MidTierCost,
            MidTierLowThresh = new[k, 8][[1]],
            MidTierHighThresh = new[k, 9][[1]]
          )
          data.table::set(new,
                          i = k,
                          j = 7L,
                          value = x[[1]])
          temp <- x[[2]]
          data.table::set(new,
                          i = k,
                          j = 10L,
                          value = temp[Thresholds == eval(x[[1]]),
                                       "Utilities"][[1]])
        }
        base::return(new)
      }
      
      # Inner function for threshold optimizataion
      threshOptim <- function(data,
                              actTar   = 1,
                              predTar  = 2,
                              tpProfit = 1,
                              tnProfit = 5,
                              fpProfit = -1,
                              fnProfit = -1,
                              MidTierDoNothing = FALSE,
                              MidTierCost = -100,
                              MidTierLowThresh = 0.25,
                              MidTierHighThresh = 0.75) {
        # Convert factor target to numeric
        data[, eval(actTar) := base::as.numeric(base::as.character(base::get(actTar)))]
        
        # Optimize each column's classification threshold ::
        popTrue <- base::mean(data[[(actTar)]])
        store   <- list()
        j <- 0
        base::options(warn = -1)
        for (i in c(MidTierHighThresh)) {
          j <- j + 1
          if (tpProfit != 0) {
            tp <- base::sum(base::ifelse(
              !(data[[predTar]] < MidTierHighThresh &
                  data[[predTar]] > MidTierLowThresh) &
                data[[actTar]] == 1 & data[[predTar]] >= i,
              1,
              0
            ))
          } else {
            tp <- 0
          }
          if (tnProfit != 0) {
            tn <-
              base::sum(base::ifelse(
                !(data[[predTar]] < MidTierHighThresh &
                    data[[predTar]] > MidTierLowThresh) &
                  data[[actTar]] == 0 & data[[predTar]] <  i,
                1,
                0
              ))
          } else {
            tn <- 0
          }
          if (fpProfit != 0) {
            fp <-
              base::sum(base::ifelse(
                !(data[[predTar]] < MidTierHighThresh &
                    data[[predTar]] > MidTierLowThresh) &
                  data[[actTar]] == 0 & data[[predTar]] >= i,
                1,
                0
              ))
          } else {
            fp <- 0
          }
          if (fnProfit != 0) {
            fn <-
              base::sum(base::ifelse(
                !(data[[predTar]] < MidTierHighThresh &
                    data[[predTar]] > MidTierLowThresh) &
                  data[[actTar]] == 1 & data[[predTar]] <  i,
                1,
                0
              ))
          } else {
            fp <- 0
          }
          none <-
            base::sum(base::ifelse(
              data[[predTar]] <= MidTierHighThresh &
                data[[predTar]] >= MidTierLowThresh,
              1,
              0
            ))
          tpr     <-
            base::ifelse((tp + fn) == 0, 0, tp / (tp + fn))
          fpr     <-
            base::ifelse((fp + tn) == 0, 0, fp / (fp + tn))
          noneRate <- none / base::nrow(data)
          utility <-
            (1 - noneRate) * (
              popTrue * (tpProfit * tpr + fnProfit * (1 - tpr)) +
                (1 - popTrue) * (fpProfit * fpr + tnProfit * (1 - fpr))
            ) + noneRate * MidTierCost
          store[[j]] <- base::c(i, utility)
        }
        all <- data.table::rbindlist(list(store))
        utilities <- data.table::melt(all[2, ])
        data.table::setnames(utilities, "value", "Utilities")
        thresholds <- data.table::melt(all[1, ])
        data.table::setnames(thresholds, "value", "Thresholds")
        results <- cbind(utilities, thresholds)[, c(-1, -3)]
        thresh <-
          results[Thresholds <= eval(MidTierLowThresh) |
                    Thresholds >= eval(MidTierHighThresh)][order(-Utilities)][1,
                                                                              2][[1]]
        options(warn = 1)
        return(list(thresh, results))
      }
      
      # Run core function
      data <- RedYellowGreenParallel(
        data,
        PredictColNumber  = PredictColNumber,
        ActualColNumber   = ActualColNumber,
        TruePositiveCost  = TruePositiveCost,
        TrueNegativeCost  = TrueNegativeCost,
        FalsePositiveCost = FalsePositiveCost,
        FalseNegativeCost = FalseNegativeCost,
        MidTierCost       = MidTierCost,
        new = i
      )
      
      # Return data table
      data
    }
  
  # Shut down cluster
  parallel::stopCluster(cl)
  
  # 3D Scatterplot
  s3d <-
    scatterplot3d::scatterplot3d(
      x = results[["MTLT"]],
      y = results[["MTHT"]],
      z = results[["Utility"]],
      type = "p",
      color = "#401a50",
      angle = 45,
      pch = 16,
      main = paste0("Utility Maximizer - Main Threshold at ",
                    results[order(-Utility)][1, "MTHT"][[1]]),
      sub = paste0("Lower Thresh = ",
                   results[order(-Utility)][1,
                                            "MTLT"][[1]],
                   " and Upper Thresh = ",
                   results[order(-Utility)][1, "MTHT"][[1]]),
      xlab = "Mid Tier Lower Threshold",
      ylab = "Mid Tier Higher Threshold",
      zlab = "Utility"
    )
  model <-
    stats::lm(results[["Utility"]] ~ results[["MTLT"]] + results[["MTHT"]])
  s3d$plane3d(model)
  N <- nrow(results)
  s3d$points3d(
    x = results[order(-Utility)][1:(N / 100), "MTLT"][[1]],
    y = results[order(-Utility)][1:(N / 100), "MTHT"][[1]],
    z = results[order(-Utility)][1:(N / 100), "Utility"][[1]],
    col = "#00aa9d",
    type = "h",
    pch = 1
  )
  s3d$points3d(
    x = results[order(-Utility)][1, "MTLT"][[1]],
    y = results[order(-Utility)][1, "MTHT"][[1]],
    z = results[order(-Utility)][1, "Utility"][[1]],
    col = "black",
    type = "h",
    pch = 10
  )
  return(results)
}

#' Utility maximizing thresholds for binary classification
#'
#' This function will return the utility maximizing threshold for future predictions along with the data generated to estimate the threshold
#'
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @param data data is the data table you are building the modeling on
#' @param actTar The column name where the actual target variable is located (in binary form)
#' @param predTar The column name where the predicted values are located
#' @param tpProfit This is the utility for generating a true positive prediction
#' @param tnProfit This is the utility for generating a true negative prediction
#' @param fpProfit This is the cost of generating a false positive prediction
#' @param fnProfit This is the cost of generating a false negative prediction
#' @examples
#' data <- data.table::data.table(Target = runif(10))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(10)]
#' data[, Predict := log(pnorm(0.85 * x1 +
#'                               sqrt(1-0.85^2) * qnorm(x2)))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' data <- threshOptim(data     = data,
#'                     actTar   = "Target",
#'                     predTar  = "Predict",
#'                     tpProfit = 0,
#'                     tnProfit = 0,
#'                     fpProfit = -1,
#'                     fnProfit = -2)
#' optimalThreshold <- data$Thresholds
#' allResults <- data$EvaluationTable
#' @return Optimal threshold and corresponding utilities for the range of thresholds tested
#' @export
threshOptim <- function(data,
                        actTar   = "target",
                        predTar  = "p1",
                        tpProfit = 0,
                        tnProfit = 0,
                        fpProfit = -1,
                        fnProfit = -2) {
  # Check data.table
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Convert factor target to numeric
  data[, eval(actTar) := as.numeric(as.character(get(actTar)))]
  
  # Optimize each column's classification threshold ::
  popTrue <- base::mean(data[[(actTar)]])
  store   <- list()
  j <- 0
  options(warn = -1)
  for (i in seq(from = 0.01, to = 0.99, by = 0.01)) {
    j <- j + 1
    tp      <-
      base::sum(ifelse(data[[actTar]] == 1 &
                         data[[predTar]] >= i, 1, 0))
    tn      <-
      base::sum(ifelse(data[[actTar]] == 0 &
                         data[[predTar]] <  i, 1, 0))
    fp      <-
      base::sum(ifelse(data[[actTar]] == 0 &
                         data[[predTar]] >= i, 1, 0))
    fn      <-
      base::sum(ifelse(data[[actTar]] == 1 &
                         data[[predTar]] <  i, 1, 0))
    tpr     <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
    fpr     <- ifelse((fp + tn) == 0, 0, fp / (fp + tn))
    utility <-
      popTrue * (tpProfit * tpr +
                   fnProfit * (1 - tpr)) +
      (1 - popTrue) * (fpProfit * fpr + tnProfit * (1 - fpr))
    store[[j]] <- c(i, utility)
  }
  all <- data.table::rbindlist(list(store))
  utilities <- data.table::melt(all[2, ])
  data.table::setnames(utilities, "value", "Utilities")
  thresholds <- data.table::melt(all[1, ])
  data.table::setnames(thresholds, "value", "Thresholds")
  results <- cbind(utilities, thresholds)[, c(-1, -3)]
  thresh <- results[order(-Utilities)][1, 2][[1]]
  options(warn = 1)
  return(list(Thresholds = thresh, EvaluationTable = results))
}

#' ParDepCalPlots automatically builds partial dependence calibration plots for model evaluation
#'
#' This function automatically builds partial dependence calibration plots and partial dependence calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @param data Data containing predicted values and actual values for comparison
#' @param PredictionColName Predicted values column names
#' @param TargetColName Target value column names
#' @param IndepVar Independent variable column names
#' @param GraphType calibration or boxplot - calibration aggregated data based on summary statistic; boxplot shows variation
#' @param PercentileBucket Number of buckets to partition the space on (0,1) for evaluation
#' @param FactLevels The number of levels to show on the chart (1. Levels are chosen based on frequency; 2. all other levels grouped and labeled as "Other")
#' @param Function Supply the function you wish to use for aggregation.
#' @return Partial dependence calibration plot or boxplot
#' @examples
#' Correl <- 0.85
#' data <- data.table::data.table(Target = runif(100))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(100)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Predict := (pnorm(Correl * x1 +
#'                            sqrt(1-Correl^2) * qnorm(x2)))]
#' p1 <- RemixAutoML::ParDepCalPlots(data,
#'                                   PredictionColName = "Predict",
#'                                   TargetColName = "Target",
#'                                   IndepVar = "Independent_Variable1",
#'                                   GraphType = "calibration",
#'                                   PercentileBucket = 0.20,
#'                                   FactLevels = 10,
#'                                   Function = function(x) mean(x, na.rm = TRUE))
#' p1
#' @export
ParDepCalPlots <- function(data,
                           PredictionColName = c("PredictedValues"),
                           TargetColName  = c("ActualValues"),
                           IndepVar    = c("Independent_Variable_Name"),
                           GraphType        = c("calibration"),
                           PercentileBucket = 0.05,
                           FactLevels  = 10,
                           Function    = function(x)
                             base::mean(x, na.rm = TRUE)) {
  # Turn off ggplot2 warnings
  options(warn = -1)
  
  # Build buckets by independent variable of choice
  preds2 <- data.table::as.data.table(data)
  
  # Subset columns
  cols <- c(PredictionColName, TargetColName, IndepVar)
  preds2 <- preds2[, ..cols]
  
  # Structure data
  data <- data[, ..cols]
  data.table::setcolorder(data, c(PredictionColName, TargetColName, IndepVar))
  
  # If actual is in factor form, convert to numeric
  if (!is.numeric(preds2[[TargetColName]])) {
    preds2[, eval(TargetColName) := as.numeric(as.character(get(TargetColName)))]
    GraphType <- "calibration"
  }
  
  # Prepare for both calibration and boxplot
  if (is.numeric(preds2[[IndepVar]]) ||
      is.integer(preds2[[IndepVar]])) {
    preds2[, rank := 100 *
             (round(percRank(preds2[[IndepVar]]) / PercentileBucket) * PercentileBucket)]
  } else {
    GraphType <- "FactorVar"
    preds2[, id := seq_len(.N), by = get(IndepVar)]
    preds2 <-
      preds2[, .(Function(get(TargetColName)),
                 Function(get(PredictionColName)),
                 max(id)),
             by = get(IndepVar)][order(-V3)]
    if (nrow(preds2) > FactLevels) {
      temp1 <- preds2[1:FactLevels][, V3 := NULL]
      temp2 <- preds2[(FactLevels + 1):nrow(preds2)]
      temp2[, ':=' (V1 = V1 * V3 / base::sum(V3),
                    V2 = V2 * V3 / base::sum(V3))]
      temp3 <- temp2[, .(base::sum(V1), base::sum(V2))]
      temp3[, get := "Other"]
      data.table::setcolorder(temp3, c(3, 1, 2))
    }
    preds2[, V3 := NULL]
    if (nrow(preds2) > FactLevels) {
      preds3 <- data.table::rbindlist(list(temp1, temp3))
    } else {
      preds3 <- preds2
    }
    data.table::setnames(
      preds3,
      old = c("get", "V1", "V2"),
      new = c(IndepVar, TargetColName, PredictionColName)
    )
    preds3 <- preds3[order(-get(PredictionColName))]
  }
  
  # Build plots
  if (GraphType == "calibration") {
    # Aggregate by rank for calibration
    preds3 <-
      preds2[, lapply(.SD, noquote(Function)), by = rank][order(rank)]
    preds3[, eval(IndepVar) := as.numeric(get(IndepVar))]
    
    # Partial dependence calibration plot
    plot <-
      ggplot2::ggplot(preds3, ggplot2::aes(x = preds3[[IndepVar]])) +
      ggplot2::geom_line(ggplot2::aes(y = preds3[[PredictionColName]],
                                      color = "Predicted")) +
      ggplot2::geom_line(ggplot2::aes(y = preds3[[TargetColName]],
                                      color = "Actuals")) +
      ggplot2::ylab("Actual | Predicted") +
      ggplot2::xlab(IndepVar) +
      ggplot2::scale_colour_manual(
        "",
        breaks = c("Actuals", "Predicted"),
        values = c("blue", "red")
      ) +
      ChartTheme(Size = 15) +
      ggplot2::ggtitle("Partial Dependence Calibration Plot")
  } else if (GraphType == "boxplot") {
    # Partial dependence boxplot
    keep <- c("rank", TargetColName, IndepVar)
    actual <- preds2[, ..keep]
    actual[, Type := "actual"]
    data.table::setnames(actual, TargetColName, "Output")
    
    keep <- c("rank", PredictionColName, IndepVar)
    predicted <- preds2[, ..keep]
    predicted[, Type := "predicted"]
    data.table::setnames(predicted, PredictionColName, "Output")
    
    data <-
      data.table::rbindlist(list(actual, predicted))[order(rank)]
    data[, rank := as.factor(rank)]
    data <- data[, eval(IndepVar) := as.numeric(get(IndepVar))]
    data <-
      data[, eval(IndepVar) := round(Function(get(IndepVar)), 3),
           by = rank]
    data[, eval(IndepVar) := as.factor(get(IndepVar))]
    data[, rank := NULL]
    plot <-
      ggplot2::ggplot(data, ggplot2::aes(x = data[[IndepVar]],
                                         y = Output)) +
      ggplot2::geom_boxplot(ggplot2::aes(fill = Type)) +
      ggplot2::scale_fill_manual(values = c("red", "blue")) +
      ggplot2::ggtitle("Partial Dependence Calibration Boxplot") +
      ggplot2::xlab(eval(IndepVar)) +
      ggplot2::ylab("Actual | Predicted") +
      ChartTheme(Size = 15)
  } else if (GraphType == "FactorVar") {
    keep <- c(IndepVar, TargetColName)
    actual <- preds3[, ..keep]
    actual[, Type := "actual"]
    data.table::setnames(actual, TargetColName, "Output")
    
    keep <- c(IndepVar, PredictionColName)
    predicted <- preds3[, ..keep]
    predicted[, Type := "predicted"]
    data.table::setnames(predicted, PredictionColName, "Output")
    data <-
      data.table::rbindlist(list(actual,
                                 predicted))[order(-Output)]
    
    plot <-
      ggplot2::ggplot(data, ggplot2::aes(x = data[[IndepVar]],
                                         y = Output)) +
      ggplot2::geom_bar(stat = "identity",
                        position = "dodge",
                        ggplot2::aes(fill = Type)) +
      ggplot2::scale_fill_manual(values = c("red",
                                            "blue")) +
      ggplot2::ggtitle("Partial Dependence Calibration Barplot") +
      ggplot2::xlab(eval(IndepVar)) +
      ggplot2::ylab("Actual | Predicted") +
      ChartTheme(Size = 15)
  }
  return(plot)
}

#' EvalPlot automatically builds calibration plots for model evaluation
#'
#' This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#' @author Adrian Antico
#' @family Model Evaluation and Interpretation
#' @param data Data containing predicted values and actual values for comparison
#' @param PredictionColName String representation of column name with predicted values from model
#' @param TargetColName String representation of column name with target values from model
#' @param GraphType Calibration or boxplot - calibration aggregated data based on summary statistic; boxplot shows variation
#' @param PercentileBucket Number of buckets to partition the space on (0,1) for evaluation
#' @param aggrfun The statistics function used in aggregation, listed as a function
#' @return Calibration plot or boxplot
#' @examples
#' Correl <- 0.85
#' data <- data.table::data.table(Target = runif(100))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(100)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Predict := (pnorm(Correl * x1 +
#'                            sqrt(1-Correl^2) * qnorm(x2)))]
#' EvalPlot(data,
#'          PredictionColName = "Predict",
#'          TargetColName = "Target",
#'          GraphType = "calibration",
#'          PercentileBucket = 0.05,
#'          aggrfun = function(x) quantile(x, probs = 0.50, na.rm = TRUE))
#' @export
EvalPlot <- function(data,
                     PredictionColName = c("PredictedValues"),
                     TargetColName  = c("ActualValues"),
                     GraphType        = c("calibration"),
                     PercentileBucket = 0.05,
                     aggrfun     = function(x)
                       mean(x, na.rm = TRUE)) {
  # Turn data into data.table if not already
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)
  
  # Structure data
  cols <- c(eval(PredictionColName), eval(TargetColName))
  data <- data[, ..cols]
  data.table::setcolorder(data, c(PredictionColName, TargetColName))
  data.table::setnames(data,
                       c(PredictionColName, TargetColName),
                       c("preds", "acts"))
  
  # If actual is in factor form, convert to numeric
  if (!is.numeric(data[["acts"]])) {
    data.table::set(data, j = "acts", value = as.numeric(as.character(data[["acts"]])))
    GraphType <- "calibration"
  }
  
  # Add a column that ranks predicted values
  data.table::set(data,
                  j = "rank",
                  value = 100 * (round(percRank(data[[1]]) / PercentileBucket) * PercentileBucket))
  
  # Plot
  if (GraphType == "boxplot") {
    # Remove classification and non-event predicted values
    data.table::set(data, j = "rank", value = as.factor(data[["rank"]]))
    cols <- c("rank", "preds")
    zz1 <- data[, ..cols]
    zz1[, Type := 'predicted']
    data.table::setnames(zz1, c("preds"), c("output"))
    
    cols <- c("rank", "acts")
    zz2 <- data[, ..cols]
    zz2[, Type := 'actual']
    data.table::setnames(zz2, c("acts"), c("output"))
    data <- data.table::rbindlist(list(zz1, zz2))
    plot <-
      ggplot2::ggplot(data, ggplot2::aes(x = rank,
                                         y = output,
                                         fill = Type)) +
      ggplot2::geom_boxplot(outlier.color = "red",
                            color = "black") +
      ggplot2::ggtitle("Calibration Evaluation Boxplot") +
      ggplot2::xlab("Predicted Percentile") +
      ggplot2::ylab("Observed Values") +
      ChartTheme(Size = 15) +
      ggplot2::scale_fill_manual(values = c("blue",
                                            "red"))
    
  } else {
    # Aggregate all columns by rank, utilizing mean as the aggregator statistic
    data <- data[, lapply(.SD, noquote(aggrfun)), by = rank]
    
    # Build calibration plot
    plot  <- ggplot2::ggplot(data, ggplot2::aes(x = rank))  +
      ggplot2::geom_line(ggplot2::aes(y = data[[3]],
                                      color = "Actual")) +
      ggplot2::geom_line(ggplot2::aes(y = data[[2]],
                                      color = "Predicted")) +
      ggplot2::xlab("Predicted Percentile") +
      ggplot2::ylab("Observed Values") +
      ggplot2::scale_color_manual(values = c("red", "blue")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         hjust = 1)) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::ggtitle("Calibration Evaluation Plot") +
      ChartTheme(Size = 15) +
      ggplot2::scale_fill_manual(values = c("blue",
                                            "gold"))
  }
  return(plot)
}

#' For NLP work
#'
#' This function tokenizes text data
#' @author Adrian Antico
#' @family Misc
#' @param data The text data
#' @examples
#' \donttest{
#' data <- tokenizeH2O(data = data[["StringColumn"]])
#' }
#' @export
tokenizeH2O <- function(data) {
  data <- h2o::as.h2o(data, col.types = c("String"))
  tokenized <- h2o::h2o.tokenize(data, "\\\\W+")
  tokenized.lower <- h2o::h2o.tolower(tokenized)
  tokenized.words <-
    tokenized.lower[h2o::h2o.grep("[0-9]",
                                  tokenized.lower,
                                  invert = TRUE,
                                  output.logical = TRUE), ]
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
#' data <- data.table::data.table(
#' DESCR = c("Gru, Gru, Gru, Gru, Gru, Gru, Gru, Gru, Gru, Gru, Gru, Gru, Gru,
#'            Urkle, Urkle, Urkle, Urkle, Urkle, Urkle, Urkle, Gru, Gru, Gru,
#'            bears, bears, bears, bears, bears, bears, smug, smug, smug, smug,
#'            smug, smug, smug, smug, smug, smug, smug, smug, smug, smug, smug,
#'            eats, eats, eats, eats, eats, eats, beats, beats, beats, beats,
#'            beats, beats, beats, beats, beats, beats, beats, science, science,
#'            Dwigt, Dwigt, Dwigt, Dwigt, Dwigt, Dwigt, Dwigt, Dwigt, Dwigt, Dwigt,
#'            Schrute, Schrute, Schrute, Schrute, Schrute, Schrute, Schrute,
#'            James, James, James, James, James, James, James, James, James, James,
#'            Halpert, Halpert, Halpert, Halpert, Halpert, Halpert, Halpert, Halpert"))
#' data <- AutoWordFreq(data,
#'                      TextColName = "DESCR",
#'                      GroupColName = NULL,
#'                      GroupLevel = NULL,
#'                      RemoveEnglishStopwords = FALSE,
#'                      Stemming = FALSE,
#'                      StopWords = c("Bla"))
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
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Ensure stringCol is character (not factor)
  if (!is.character(data[[eval(TextColName)]]))
    data[, eval(TextColName) := as.character(get(TextColName))]
  
  # Prepare data
  if (is.null(GroupColName)) {
    desc <- tm::Corpus(tm::VectorSource(data[[eval(TextColName)]]))
  } else {
    if (!is.character(data[[GroupColName]])) {
      data[, eval(GroupColName) := as.character(get(GroupColName))]
      desc <-
        tm::Corpus(tm::VectorSource(data[get(GroupColName) == eval(GroupLevel)][[eval(TextColName)]]))
    }
  }
  
  # Clean text
  toSpace <-
    tm::content_transformer(function (x , pattern)
      gsub(pattern, " ", x))
  text <- tm::tm_map(desc, toSpace, "/")
  text <- tm::tm_map(text, toSpace, "@")
  text <- tm::tm_map(text, toSpace, "\\|")
  
  # Convert the text to lower case
  text <- tm::tm_map(text, tm::content_transformer(tolower))
  
  # Remove numbers
  text <- tm::tm_map(text, tm::removeNumbers)
  
  # Remove english common stopwords
  if (RemoveEnglishStopwords)
    text <-
    tm::tm_map(text, tm::removeWords, tm::stopwords("english"))
  
  # specify your stopwords as a character vector
  text <- tm::tm_map(text, tm::removeWords, StopWords)
  
  # Remove punctuations
  text <- tm::tm_map(text, tm::removePunctuation)
  
  # Eliminate extra white spaces
  text <- tm::tm_map(text, tm::stripWhitespace)
  
  # Text stemming
  if (Stemming)
    text <- tm::tm_map(text, tm::stemDocument)
  
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
      colors = RColorBrewer::brewer.pal(8, "Dark2")
    )
  )
  
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
#' @examples
#' \donttest{
#' data <- AutoH2OTextPrepScoring(data = x,
#'                                string = "text_column",
#'                                MaxMem = "28G",
#'                                NThreads = 8)
#' }
#' @export
AutoH2OTextPrepScoring <- function(data,
                                   string,
                                   MaxMem,
                                   NThreads) {
  # Ensure data.table----
  if (!is.data.table(data)) {
    data <- data.table::as.data.table((data))
  }
  data[, eval(string) := as.character(get(string))]
  h2o::h2o.init(nthreads = NThreads, max_mem_size = MaxMem)
  
  # It is important to remove "\n" --
  data[, eval(string) := gsub("  ", " ", get(string))]
  data[, eval(string) := stringr::str_replace_all(get(string), "[[:punct:]]", "")]
  data2 <- data[, ..string]
  
  # Tokenize
  tokenized_words <- RemixAutoML::tokenizeH2O(data2)
  return(tokenized_words)
}

#' CreateCalendarVariables Create Caledar Variables
#'
#' CreateCalendarVariables Rapidly creates calendar variables based on the date column you provide
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data This is your data
#' @param DateCols Supply either column names or column numbers of your date columns you want to use for creating calendar variables
#' @param AsFactor Set to TRUE if you want factor type columns returned; otherwise integer type columns will be returned
#' @param TimeUnits Supply a character vector of time units for creating calendar variables. Options include: "second", "minute", "hour", "wday", "mday", "yday", "week", "isoweek", "month", "quarter", "year"
#' @examples
#' data <- data.table::data.table(Date = "2018-01-01 00:00:00")
#' data <- CreateCalendarVariables(data,
#'                                 DateCols = "Date",
#'                                 AsFactor = FALSE,
#'                                 TimeUnits = c("wday", "month", "year"))
#' @return Returns your data.table with the added calendar variables at the end
#' @export
CreateCalendarVariables <- function(data,
                                    DateCols = c("Date", "Date2"),
                                    AsFactor = FALSE,
                                    TimeUnits = "wday") {
  # Convert to data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Check args----
  if (!is.logical(AsFactor)) {
    warning("AsFactor needs to be TRUE or FALSE")
  }
  if (!(any(
    tolower(TimeUnits) %chin% c(
      "second",
      "minute",
      "hour",
      "wday",
      "mday",
      "yday",
      "week",
      "isoweek",
      "month",
      "quarter",
      "year"
    )
  ))) {
    warning(
      "TimeUnits needs to be one of 'minute', 'hour', 'wday',
            'mday', 'yday','week', 'month', 'quarter', 'year'"
    )
  }
  
  # Turn DateCols into character names if not already----
  for (i in DateCols) {
    if (!is.character(DateCols[i])) {
      DateCols[i] <- names(data)[DateCols[i]]
    }
  }
  
  # Revise TimeUnits Based on Data----
  x <- 0
  TimeList <- list()
  Cols <- c()
  for (i in seq_len(length(DateCols))) {
    if (any(TimeUnits %chin% c("second", "minute", "hour"))) {
      if (min(as.ITime(data[[eval(DateCols[i])]])) - max(as.ITime(data[[eval(DateCols[i])]])) == 0) {
        TimeList[[i]] <-
          TimeUnits[!(tolower(TimeUnits) %chin% c("second", "minute", "hour"))]
        Cols[i] <- length(TimeList[[i]])
      } else {
        TimeList[[i]] <- TimeUnits
        Cols[i] <- length(TimeList[[i]])
      }
    } else {
      TimeList[[i]] <- TimeUnits
      Cols[i] <- length(TimeList[[i]])
    }
  }
  
  # Allocate data.table cols
  data.table::alloc.col(DT = data, ncol(data) + sum(Cols))
  
  # Create DateCols to data.table IDateTime types----
  for (i in seq_len(length(DateCols))) {
    if (length(TimeList) != 0) {
      if (any(tolower(TimeList[[i]]) %chin% c("second", "minute", "hour"))) {
        data.table::set(data,
                        j = paste0("TIME_", eval(DateCols[i])),
                        value = as.ITime(data[[eval(DateCols[i])]]))
      }
      if (any(
        tolower(TimeList[[i]]) %chin% c(
          "wday",
          "mday",
          "yday",
          "week",
          "isoweek",
          "month",
          "quarter",
          "year"
        )
      )) {
        data.table::set(data,
                        j = paste0("DATE_", eval(DateCols[i])),
                        value = data.table::as.IDate(data[[eval(DateCols[i])]]))
      }
    }
  }
  
  # Build Features----
  for (i in seq_len(length(DateCols))) {
    for (j in TimeList[[i]]) {
      if (tolower(j) == "second") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.factor(data.table::second(get(
              paste0("TIME_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::second(data[[paste0("TIME_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "minute") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::minute(get(
              paste0("TIME_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::minute(data[[paste0("TIME_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "hour") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::hour(get(
              paste0("TIME_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::hour(data[[paste0("TIME_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "wday") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::wday(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::wday(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "mday") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::mday(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::mday(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "yday") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::yday(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::yday(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "week") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::week(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::week(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "isoweek") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::isoweek(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::isoweek(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "month") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::month(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::month(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "quarter") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::quarter(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::quarter(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      } else if (tolower(j) == "year") {
        if (AsFactor) {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", TimeList[[i]][j]),
            value = as.factor(data.table::year(get(
              paste0("DATE_", DateCols[i])
            )))
          )
        } else {
          data.table::set(
            data,
            j = paste0(DateCols[i], "_", j),
            value = as.integer(data.table::year(data[[paste0("DATE_", DateCols[i])]]))
          )
        }
      }
    }
    if (any(tolower(TimeList[[i]]) %chin% c("second", "minute", "hour"))) {
      data.table::set(data,
                      j = paste0("TIME_", DateCols[i]),
                      value = NULL)
    }
    if (any(
      tolower(TimeList[[i]]) %chin% c(
        "wday",
        "mday",
        "yday",
        "week",
        "isoweek",
        "month",
        "quarter",
        "year"
      )
    )) {
      data.table::set(data,
                      j = paste0("DATE_", DateCols[i]),
                      value = NULL)
    }
  }
  return(data)
}

#' DummifyDT creates dummy variables for the selected columns.
#'
#' DummifyDT creates dummy variables for the selected columns. Either one-hot encoding, N+1 columns for N levels, or N columns for N levels.
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data The data set to run the micro auc on
#' @param cols A vector with the names of the columns you wish to dichotomize
#' @param OneHot Set to TRUE to run one hot encoding, FALSE to generate N columns for N levels
#' @param KeepFactorCols Set to TRUE to keep the original columns used in the dichotomization process
#' @param SaveFactorLevels Set to TRUE to save unique levels of each factor column to file as a csv
#' @param SavePath Provide a file path to save your factor levels. Use this for models that you have to create dummy variables for.
#' @param ImportFactorLevels Instead of using the data you provide, import the factor levels csv to ensure you build out all of the columns you trained with in modeling.
#' @param ClustScore This is for scoring AutoKMeans. Set to FALSE for all other applications.
#' @examples
#' test <- data.table::data.table(Value = runif(100000),
#'                    FactorCol = sample(x = c(letters,
#'                                             LETTERS,
#'                                             paste0(letters,letters),
#'                                             paste0(LETTERS,LETTERS),
#'                                             paste0(letters,LETTERS),
#'                                             paste0(LETTERS,letters)),
#'                                       size = 100000,
#'                                       replace = TRUE))
#' test <- DummifyDT(data = test,
#'                   cols = "FactorCol",
#'                   KeepFactorCols = FALSE)
#' ncol(test)
#' test[, sum(FactorCol_gg)]
#' @return data table with new dummy variables columns and optionally removes base columns
#' @export
DummifyDT <- function(data,
                      cols,
                      KeepFactorCols     = FALSE,
                      OneHot             = FALSE,
                      SaveFactorLevels   = FALSE,
                      SavePath           = NULL,
                      ImportFactorLevels = FALSE,
                      ClustScore         = FALSE) {
  # Check arguments----
  if (!is.character(cols)) {
    warning("cols needs to be a character vector of names")
  }
  if (!is.logical(KeepFactorCols)) {
    warning("KeepFactorCols needs to be either TRUE or FALSE")
  }
  if (!is.logical(KeepFactorCols)) {
    warning("KeepFactorCols needs to be either TRUE or FALSE")
  }
  if (!is.logical(OneHot)) {
    warning("OneHot needs to be either TRUE or FALSE")
  }
  if (!is.logical(SaveFactorLevels)) {
    warning("SaveFactorLevels needs to be either TRUE or FALSE")
  }
  if (!is.logical(ImportFactorLevels)) {
    warning("ImportFactorLevels needs to be either TRUE or FALSE")
  }
  if (!is.logical(ClustScore)) {
    warning("ClustScore needs to be either TRUE or FALSE")
  }
  if (!is.null(SavePath)) {
    if (!is.character(SavePath)) {
      warning("SavePath needs to be a character value of a folder location")
    }
  }
  
  # Check data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Ensure correct argument settings----
  if (OneHot == TRUE & ClustScore == TRUE) {
    OneHot <- FALSE
    KeepFactorCols <- FALSE
  }
  
  # Build dummies start----
  for (col in rev(cols)) {
    size <- ncol(data)
    Names <- setdiff(names(data), col)
    
    # Import factor levels for scoring models----
    if (ImportFactorLevels) {
      temp <- data.table::fread(paste0(SavePath, "/", col, ".csv"))
      inds <- sort(unique(temp[[eval(col)]]))
    } else {
      inds <- sort(unique(data[[eval(col)]]))
    }
    
    # Allocate columns----
    data.table::alloc.col(data, n = ncol(data) + length(inds))
    
    # Save factor levels for scoring later----
    if (SaveFactorLevels) {
      data.table::fwrite(x = data[, get(col), by = eval(col)][, V1 := NULL],
                         file = paste0(SavePath, "/", col, ".csv"))
    }
    
    # Convert to character if col is factor----
    if (is.factor(data[[eval(col)]])) {
      data.table::set(data, j = eval(col), value = as.character(data[[eval(col)]]))
    }
    
    # If for clustering set up old school way----
    if (!ClustScore) {
      data.table::set(data,
                      j = paste0(col, "_", inds),
                      value = 0L)
    } else {
      data.table::set(data,
                      j = paste0(col, inds),
                      value = 0L)
    }
    
    # Build dummies----
    for (ind in inds) {
      if (!ClustScore) {
        data.table::set(
          data,
          i = which(data[[col]] %chin% ind),
          j = paste0(col, "_", ind),
          value = 1L
        )
      } else {
        data.table::set(
          data,
          i = which(data[[col]] %chin% ind),
          j = paste0(col, ind),
          value = 1L
        )
      }
    }
    
    # Remove original factor columns----
    if (!KeepFactorCols) {
      data.table::set(data, j = eval(col), value = NULL)
    }
    if (ClustScore) {
      setcolorder(data,
                  c(setdiff(names(data),
                            Names),
                    Names))
    }
    
    # If onehot, add extra column----
    if (OneHot) {
      data.table::set(data, j = paste0(col, "_Base"), value = 0L)
    }
  }
  
  # Clustering section----
  if (ClustScore) {
    setnames(data, names(data),
             tolower(gsub(
               '[[:punct:] ]+',
               replacement = "",
               names(data)
             )))
  }
  return(data)
}

#' An Automated Feature Engineering Function
#'
#' Builds autoregressive and rolling stats from target columns and distributed lags and distributed rolling stats for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and rolling stats. This function works for data with groups and without groups.
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data A data.table you want to run the function on
#' @param lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param periods A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param statsNames A character vector of the corresponding names to create for the rollings stats variables.
#' @param statsFUNs Vector that holds functions for your rolling stats, such as function(x) mean(x), function(x) sd(x), or function(x) quantile(x)
#' @param targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param groupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param sortDateName The column name of your date column used to sort events over time
#' @param timeDiffTarget Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param timeAgg List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you percentage complete tracker printout
#' @param SkipCols Defaults to NULL; otherwise supply a character vector of the names of columns to skip
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' N = 25116
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(N,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:N)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' data <- GDL_Feature_Engineering(data,
#'            lags           = c(seq(1,1,1)),
#'            periods        = c(3),
#'            statsFUNs      = c(function(x) quantile(x, probs = 0.20, na.rm = TRUE)),
#'            statsNames     = c("q20"),
#'            targets        = c("Target"),
#'            groupingVars   = NULL,
#'            sortDateName   = "DateTime",
#'            timeDiffTarget = NULL,
#'            timeAgg        = "days",
#'            WindowingLag   = 1,
#'            Type           = "Lag",
#'            Timer          = TRUE,
#'            SkipCols       = FALSE,
#'            SimpleImpute   = TRUE)
#' @export
GDL_Feature_Engineering <- function(data,
                                    lags           = c(seq(1, 5, 1)),
                                    periods        = c(3, 5, 10, 15, 20, 25),
                                    statsFUNs      = c(function(x)
                                      quantile(x, probs = 0.1, na.rm = TRUE),
                                      function(x)
                                        quantile(x, probs = 0.9, na.rm = TRUE),
                                      function(x)
                                        base::mean(x, na.rm = TRUE),
                                      function(x)
                                        sd(x, na.rm = TRUE),
                                      function(x)
                                        quantile(x, probs = 0.25, na.rm = TRUE),
                                      function(x)
                                        quantile(x, probs = 0.75, na.rm = TRUE)),
                                    statsNames     = c("q10",
                                                       "q90",
                                                       "mean",
                                                       "sd",
                                                       "q25",
                                                       "q75"),
                                    targets        = c("qty"),
                                    groupingVars   = c("Group1",
                                                       "Group2"),
                                    sortDateName   = c("date"),
                                    timeDiffTarget = c("TimeDiffName"),
                                    timeAgg        = c("days"),
                                    WindowingLag   = 0,
                                    Type           = c("Lag"),
                                    Timer          = TRUE,
                                    SkipCols       = NULL,
                                    SimpleImpute   = TRUE) {
  # Argument Checks----
  if (is.null(lags) & WindowingLag == 1) {
    lags <- 1
  }
  if (!(1 %in% lags) & WindowingLag == 1) {
    lags <- c(1, lags)
  }
  if (any(lags < 0)) {
    warning("lags need to be positive integers")
  }
  if (length(statsFUNs) != length(statsNames)) {
    warning("statsFuns and statsNames aren't the same length")
  }
  if (!is.character(statsNames)) {
    warning("statsNames needs to be a character scalar or vector")
  }
  if (!is.null(groupingVars)) {
    if (!is.character(groupingVars)) {
      warning("groupingVars needs to be a character scalar or vector")
    }
  }
  if (!is.character(targets)) {
    warning("targets needs to be a character scalar or vector")
  }
  if (!is.character(sortDateName)) {
    warning("sortDateName needs to be a character scalar or vector")
  }
  if (!is.null(timeDiffTarget)) {
    if (!is.character(timeDiffTarget)) {
      warning("timeDiffTarget needs to be a character scalar or vector")
    }
  }
  if (!is.null(timeAgg)) {
    if (!is.character(timeAgg)) {
      warning("timeAgg needs to be a character scalar or vector")
    }
  }
  if (!(WindowingLag %in% c(0, 1))) {
    warning("WindowingLag needs to be either 0 or 1")
  }
  if (!(tolower(Type) %chin% c("lag", "lead"))) {
    warning("Type needs to be either Lag or Lead")
  }
  if (!is.logical(Timer)) {
    warning("Timer needs to be TRUE or FALSE")
  }
  if (!is.logical(SimpleImpute)) {
    warning("SimpleImpute needs to be TRUE or FALSE")
  }
  if (!is.null(SkipCols)) {
    if (!is.character(SkipCols)) {
      warning("SkipCols needs to be a character scalar or vector")
    }
  }
  
  # Convert to data.table if not already----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Ensure target is numeric----
  data[, eval(targets) := as.numeric(get(targets))]
  
  # Set up counter for countdown----
  CounterIndicator <- 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }
  
  # Define total runs----
  if (!is.null(groupingVars)) {
    runs <-
      length(groupingVars) * tarNum * (length(periods) *
                                         length(statsNames) +
                                         length(lags))
  } else {
    runs <-
      tarNum * (length(periods) * length(statsNames) +
                  length(lags))
  }
  
  # Begin feature engineering----
  if (!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {
      Targets <- targets
      
      # Sort data----
      if (tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = -1)
      }
      
      # Generate Lags----
      for (l in seq_along(lags)) {
        for (t in Targets) {
          if (!(paste0(groupingVars[i], "_LAG_",
                       lags[l], "_", t) %in% SkipCols)) {
            data[, paste0(groupingVars[i],
                          "_LAG_", lags[l], "_", t) := data.table::shift(get(t), n = lags[l], type = "lag"),
                 by = get(groupingVars[i])]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }
      
      # Time lags----
      if (!is.null(timeDiffTarget)) {
        # Lag the dates first
        for (l in seq_along(lags)) {
          if (!(paste0(groupingVars[i], "TEMP", lags[l]) %in% SkipCols)) {
            data[, paste0(groupingVars[i], "TEMP",
                          lags[l]) := data.table::shift(get(sortDateName),
                                                        n = lags[l],
                                                        type = "lag"),
                 by = get(groupingVars[i])]
          }
        }
        
        # Difference the lag dates----
        if (WindowingLag != 0) {
          for (l in seq_along(lags)) {
            if (!(paste0(timeDiffTarget, lags[l]) %in% SkipCols) & l == 1) {
              data[, paste0(groupingVars[i],
                            timeDiffTarget, lags[l]) := as.numeric(difftime(
                              get(sortDateName),
                              get(paste0(
                                groupingVars[i], "TEMP", lags[l]
                              )),
                              units = eval(timeAgg)
                            )), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            } else {
              if (!(paste0(groupingVars[i], timeDiffTarget,
                           lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],
                              timeDiffTarget, lags[l]) := as.numeric(difftime(get(
                                paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                              ),
                              get(
                                paste0(groupingVars[i], "TEMP", lags[l])
                              ),
                              units = eval(timeAgg))), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        } else {
          for (l in seq_along(lags)) {
            if (l == 1) {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget, lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],
                              timeDiffTarget, lags[l]) := as.numeric(difftime(
                                get(sortDateName),
                                get(paste0(
                                  groupingVars[i], "TEMP", lags[l]
                                )),
                                units = eval(timeAgg)
                              )), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            } else {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget, lags[l]) %in% SkipCols)) {
                data[, paste0(groupingVars[i],
                              timeDiffTarget,
                              lags[l]) := as.numeric(difftime(get(
                                paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                              ),
                              get(
                                paste0(groupingVars[i], "TEMP", lags[l])
                              ),
                              units = eval(timeAgg))), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        }
        
        # Remove temporary lagged dates----
        for (l in seq_along(lags)) {
          data[, paste0(groupingVars[i], "TEMP", lags[l]) := NULL]
        }
        
        # Store new target----
        timeTarget <- paste0(groupingVars[i],
                             timeDiffTarget, "1")
      }
      
      # Define targets----
      if (WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_",
                     WindowingLag, "_", Targets),
              timeTarget)
        } else {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_",
                     WindowingLag, "_", Targets))
        }
      } else {
        if (!is.null(timeDiffTarget)) {
          Targets <- c(Targets, timeTarget)
        } else {
          Targets <- Targets
        }
      }
      
      # Moving stats----
      for (j in seq_along(periods)) {
        for (k in seq_along(statsNames)) {
          for (t in Targets) {
            if (!(paste0(groupingVars[i], statsNames[k], "_",
                         periods[j], "_", t) %in% SkipCols)) {
              data[, paste0(groupingVars[i],
                            statsNames[k], "_",
                            periods[j], "_", t) := zoo::rollapply(get(t), periods[j],
                                                                  statsFUNs[k][[1]], partial = TRUE),
                   by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }
    }
    
    # Replace any inf values with NA----
    for (col in seq_along(data)) {
      data.table::set(data,
                      j = col,
                      value = replace(data[[col]],
                                      is.infinite(data[[col]]), NA))
    }
    
    # Turn character columns into factors----
    for (col in seq_along(data)) {
      if (is.character(data[[col]])) {
        data.table::set(data, j = col, value = as.factor(data[[col]]))
      }
    }
    
    # Impute missing values----
    if (SimpleImpute) {
      for (j in seq_along(data)) {
        if (is.factor(data[[j]])) {
          data.table::set(data,
                          which(!(data[[j]] %in% levels(data[[j]]))),
                          j, "0")
        } else {
          data.table::set(data,
                          which(is.na(data[[j]])), j, -1)
        }
      }
    }
    
    # Done!!
    return(data)
    
  } else {
    # Sort data----
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = -1)
    }
    Targets <- targets
    
    # Generate Lags----
    for (l in seq_along(lags)) {
      for (t in Targets) {
        if (!(paste0("LAG_", lags[l], "_", t) %in% SkipCols)) {
          data[, paste0("LAG_",
                        lags[l],
                        "_",
                        t) := data.table::shift(get(t),
                                                n = lags[l],
                                                type = "lag")]
          CounterIndicator <- CounterIndicator + 1
          if (Timer) {
            print(CounterIndicator / runs)
          }
        }
      }
    }
    
    # Time lags----
    if (!is.null(timeDiffTarget)) {
      # Lag the dates first
      for (l in seq_along(lags)) {
        if (!(paste0("TEMP", lags[l]) %in% SkipCols)) {
          data[, paste0("TEMP",
                        lags[l]) := data.table::shift(get(sortDateName),
                                                      n = lags[l],
                                                      type = "lag")]
        }
      }
      
      # Difference the lag dates----
      if (WindowingLag != 0) {
        for (l in seq_along(lags)) {
          if (!(paste0(timeDiffTarget, "_", lags[l]) %in% SkipCols) &
              l == 1) {
            data[, paste0(timeDiffTarget,
                          "_",
                          lags[l]) := as.numeric(difftime(get(sortDateName),
                                                          get(paste0(
                                                            "TEMP", lags[l]
                                                          )),
                                                          units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          } else {
            data[, paste0(timeDiffTarget,
                          "_",
                          lags[l]) := as.numeric(difftime(get(paste0(
                            "TEMP", lags[l] - 1
                          )),
                          get(paste0(
                            "TEMP", lags[l]
                          )),
                          units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      } else {
        for (l in seq_along(lags)) {
          if (l == 1) {
            if (!(paste0(timeDiffTarget,
                         "_",
                         lags[l]) %in% SkipCols)) {
              data[, paste0(timeDiffTarget,
                            "_",
                            lags[l]) := as.numeric(difftime(
                              get(sortDateName),
                              get(paste0("TEMP", lags[l])),
                              units = eval(timeAgg)
                            ))]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          } else {
            if (!(paste0(timeDiffTarget, "_", lags[l]) %in% SkipCols)) {
              data[, paste0(timeDiffTarget,
                            "_",
                            lags[l]) := as.numeric(difftime(get(paste0(
                              "TEMP", (lags[l - 1])
                            )),
                            get(paste0(
                              "TEMP", lags[l]
                            )),
                            units = eval(timeAgg)))]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }
      
      # Remove temporary lagged dates----
      for (l in seq_along(lags)) {
        data[, paste0("TEMP", lags[l]) := NULL]
      }
      
      # Store new target----
      timeTarget <- paste0(timeDiffTarget, "_1")
    }
    
    # Define targets----
    if (WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets),
            timeTarget)
      } else {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets))
      }
    } else {
      if (!is.null(timeDiffTarget)) {
        Targets <- c(Targets, timeTarget)
      } else {
        Targets <- Targets
      }
    }
    
    # Moving stats----
    for (j in seq_along(periods)) {
      for (k in seq_along(statsNames)) {
        for (t in Targets) {
          if (!(paste0(statsNames[k],
                       "_",
                       periods[j],
                       "_", t) %in% SkipCols)) {
            data[, paste0(statsNames[k],
                          "_",
                          periods[j],
                          "_",
                          t) := zoo::rollapply(get(t),
                                               periods[j],
                                               statsFUNs[k][[1]],
                                               partial = TRUE)]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }
    }
    
    # Replace any inf values with NA----
    for (col in seq_along(data)) {
      data.table::set(data,
                      j = col,
                      value = replace(data[[col]],
                                      is.infinite(data[[col]]), NA))
    }
    
    # Turn character columns into factors----
    for (col in seq_along(data)) {
      if (is.character(data[[col]])) {
        data.table::set(data,
                        j = col,
                        value = as.factor(data[[col]]))
      }
    }
    
    # Impute missing values----
    if (SimpleImpute) {
      for (j in seq_along(data)) {
        if (is.factor(data[[j]])) {
          data.table::set(data,
                          which(!(data[[j]] %in% levels(data[[j]]))),
                          j, "0")
        } else {
          data.table::set(data,
                          which(is.na(data[[j]])), j, -1)
        }
      }
    }
    
    # Done!!
    return(data)
  }
}

#' An Automated Feature Engineering Function Using data.table frollmean
#'
#' Builds autoregressive and moving average from target columns and distributed lags and distributed moving average for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and moving averages. This function works for data with groups and without groups.
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data A data.table you want to run the function on
#' @param lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param periods A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param statsNames A character vector of the corresponding names to create for the rollings stats variables.
#' @param targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param groupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param sortDateName The column name of your date column used to sort events over time
#' @param timeDiffTarget Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param timeAgg List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you percentage complete tracker printout
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' N = 25116
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'                                Target = stats::filter(rnorm(N,
#'                                                             mean = 50,
#'                                                             sd = 20),
#'                                                       filter=rep(1,10),
#'                                                       circular=TRUE))
#' data[, temp := seq(1:N)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' data <- DT_GDL_Feature_Engineering(data,
#'                                    lags           = c(seq(1,5,1)),
#'                                    periods        = c(3,5,10,15,20,25),
#'                                    statsNames     = c("MA"),
#'                                    targets        = c("Target"),
#'                                    groupingVars   = NULL,
#'                                    sortDateName   = "DateTime",
#'                                    timeDiffTarget = c("Time_Gap"),
#'                                    timeAgg        = c("days"),
#'                                    WindowingLag   = 1,
#'                                    Type           = "Lag",
#'                                    Timer          = TRUE,
#'                                    SimpleImpute   = TRUE)
#' @export
DT_GDL_Feature_Engineering <- function(data,
                                       lags           = c(seq(1, 50, 1)),
                                       periods        = c(seq(5, 95, 5)),
                                       statsNames     = c("MA"),
                                       targets        = c("qty"),
                                       groupingVars   = c("Group1",
                                                          "Group2"),
                                       sortDateName   = c("date"),
                                       timeDiffTarget = c("TimeDiffName"),
                                       timeAgg        = c("days"),
                                       WindowingLag   = 0,
                                       Type           = c("Lag"),
                                       Timer          = TRUE,
                                       SimpleImpute   = TRUE) {
  # Argument Checks----
  if (is.null(lags) & WindowingLag == 1) {
    lags <- 1
  }
  if (!(1 %in% lags) & WindowingLag == 1) {
    lags <- c(1, lags)
  }
  if (any(lags < 0)) {
    warning("lags need to be positive integers")
  }
  if (!is.character(statsNames)) {
    warning("statsNames needs to be a character scalar or vector")
  }
  if (!is.null(groupingVars)) {
    if (!is.character(groupingVars)) {
      warning("groupingVars needs to be a character scalar or vector")
    }
  }
  if (!is.character(targets)) {
    warning("targets needs to be a character scalar or vector")
  }
  if (!is.character(sortDateName)) {
    warning("sortDateName needs to be a character scalar or vector")
  }
  if (!is.null(timeDiffTarget)) {
    if (!is.character(timeDiffTarget)) {
      warning("timeDiffTarget needs to be a character scalar or vector")
    }
  }
  if (!is.null(timeAgg)) {
    if (!is.character(timeAgg)) {
      warning("timeAgg needs to be a character scalar or vector")
    }
  }
  if (!(WindowingLag %in% c(0, 1))) {
    warning("WindowingLag needs to be either 0 or 1")
  }
  if (!(tolower(Type) %chin% c("lag", "lead"))) {
    warning("Type needs to be either Lag or Lead")
  }
  if (!is.logical(Timer)) {
    warning("Timer needs to be TRUE or FALSE")
  }
  if (!is.logical(SimpleImpute)) {
    warning("SimpleImpute needs to be TRUE or FALSE")
  }
  
  # Convert to data.table if not already----
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)
  
  # Ensure target is numeric----
  data[, eval(targets) := as.numeric(get(targets))]
  
  # Set up counter for countdown----
  CounterIndicator <- 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }
  
  # Define total runs----
  if (!is.null(groupingVars)) {
    runs <-
      length(groupingVars) * tarNum * (length(periods) *
                                         length(statsNames) +
                                         length(lags))
  } else {
    runs <-
      tarNum * (length(periods) * length(statsNames) +
                  length(lags))
  }
  
  # Begin feature engineering----
  if (!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {
      Targets <- targets
      
      # Sort data----
      if (tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = -1)
      }
      
      # Lags----
      for (l in seq_along(lags)) {
        for (t in Targets) {
          data[, paste0(groupingVars[i],
                        "_LAG_",
                        lags[l],
                        "_",
                        t) := data.table::shift(get(t),
                                                n = lags[l],
                                                type = "lag"),
               by = get(groupingVars[i])]
          CounterIndicator <- CounterIndicator + 1
          if (Timer) {
            print(CounterIndicator / runs)
          }
        }
      }
      
      # Time lags----
      if (!is.null(timeDiffTarget)) {
        # Lag the dates first----
        for (l in seq_along(lags)) {
          data[, paste0(groupingVars[i],
                        "TEMP",
                        lags[l]) := data.table::shift(get(sortDateName),
                                                      n = lags[l],
                                                      type = "lag"),
               by = get(groupingVars[i])]
        }
        
        # Difference the lag dates----
        if (WindowingLag != 0) {
          for (l in seq_along(lags)) {
            if (l == 1) {
              data[, paste0(groupingVars[i],
                            timeDiffTarget,
                            lags[l]) := as.numeric(difftime(
                              get(sortDateName),
                              get(paste0(
                                groupingVars[i], "TEMP", lags[l]
                              )),
                              units = eval(timeAgg)
                            )), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            } else {
              data[, paste0(groupingVars[i],
                            timeDiffTarget,
                            lags[l]) := as.numeric(difftime(get(
                              paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                            ),
                            get(
                              paste0(groupingVars[i], "TEMP", lags[l])
                            ),
                            units = eval(timeAgg))), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        } else {
          for (l in seq_along(lags)) {
            if (l == 1) {
              data[, paste0(groupingVars[i],
                            timeDiffTarget,
                            lags[l]) := as.numeric(difftime(
                              get(sortDateName),
                              get(paste0(
                                groupingVars[i], "TEMP", lags[l]
                              )),
                              units = eval(timeAgg)
                            )), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            } else {
              data[, paste0(groupingVars[i],
                            timeDiffTarget,
                            lags[l]) := as.numeric(difftime(get(
                              paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                            ),
                            get(
                              paste0(groupingVars[i], "TEMP", lags[l])
                            ),
                            units = eval(timeAgg))), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
        
        # Remove temporary lagged dates----
        for (l in seq_along(lags)) {
          data[, paste0(groupingVars[i], "TEMP",
                        lags[l]) := NULL]
        }
        
        # Store new target----
        timeTarget <- paste0(groupingVars[i],
                             timeDiffTarget, "1")
      }
      
      # Define targets----
      if (WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          Targets <-
            c(paste0(groupingVars[i],
                     "_LAG_",
                     WindowingLag,
                     "_",
                     Targets),
              timeTarget)
        } else {
          Targets <-
            c(paste0(groupingVars[i],
                     "_LAG_",
                     WindowingLag,
                     "_",
                     Targets))
        }
      } else {
        if (!is.null(timeDiffTarget)) {
          Targets <- c(Targets, timeTarget)
        } else {
          Targets <- Targets
        }
      }
      
      # Moving stats----
      for (j in seq_along(periods)) {
        for (k in seq_along(statsNames)) {
          for (t in Targets) {
            data[, paste0(groupingVars[i],
                          statsNames[k],
                          "_",
                          periods[j],
                          "_",
                          t) := data.table::frollmean(
                            x = get(t),
                            n = periods[j],
                            fill = NA,
                            algo = "fast",
                            align = "right",
                            na.rm = TRUE,
                            hasNA = TRUE,
                            adaptive = FALSE
                          ),
                 by = get(groupingVars[i])]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }
    }
    
    # Replace any inf values with NA----
    for (col in seq_along(data)) {
      data.table::set(data,
                      j = col,
                      value = replace(data[[col]],
                                      is.infinite(data[[col]]), NA))
    }
    
    # Turn character columns into factors----
    for (col in seq_along(data)) {
      if (is.character(data[[col]])) {
        data.table::set(data, j = col, value = as.factor(data[[col]]))
      }
    }
    
    # Impute missing values----
    if (SimpleImpute) {
      for (j in seq_along(data)) {
        if (is.factor(data[[j]])) {
          data.table::set(data,
                          which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data,
                          which(is.na(data[[j]])), j, -1)
        }
      }
    }
    
    # Done!!----
    if (Timer) {
      print(CounterIndicator)
    }
    return(data)
    
  } else {
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = -1)
    }
    Targets <- targets
    
    # Lags----
    for (l in seq_along(lags)) {
      for (t in Targets) {
        data.table::set(
          data,
          j = paste0("LAG_", lags[l], "_", t),
          value = data.table::shift(data[[eval(t)]], n = lags[l], type = "lag")
        )
        CounterIndicator <- CounterIndicator + 1
        if (Timer) {
          print(CounterIndicator / runs)
        }
      }
    }
    
    # Time lags----
    if (!is.null(timeDiffTarget)) {
      # Lag the dates first
      for (l in seq_along(lags)) {
        data.table::set(
          data,
          j = paste0("TEMP", lags[l]),
          value = data.table::shift(data[[eval(sortDateName)]], n = lags[l], type = "lag")
        )
      }
      
      # Difference the lag dates----
      if (WindowingLag != 0) {
        for (l in seq_along(lags)) {
          if (l == 1) {
            data.table::set(
              data,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                data[[eval(sortDateName)]],
                data[[eval(paste0("TEMP", lags[l]))]],
                units = eval(timeAgg)
              ))
            )
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          } else {
            data.table::set(
              data,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                data[[eval(paste0("TEMP", lags[l] - 1))]],
                data[[eval(paste0("TEMP", lags[l]))]],
                units = eval(timeAgg)
              ))
            )
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      } else {
        for (l in seq_along(lags)) {
          if (l == 1) {
            data.table::set(
              data,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                data[[eval(sortDateName)]],
                data[[eval(paste0("TEMP", lags[l]))]],
                units = eval(timeAgg)
              ))
            )
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          } else {
            data.table::set(
              data,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                data[[eval(paste0("TEMP", (lags[l - 1])))]],
                data[[eval(paste0("TEMP", lags[l]))]],
                units = eval(timeAgg)
              ))
            )
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }
      
      # Remove temporary lagged dates----
      for (l in seq_along(lags)) {
        data.table::set(data, j = paste0("TEMP", lags[l]), value = NULL)
      }
      
      # Store new target----
      timeTarget <- paste0(timeDiffTarget, "_1")
    }
    
    # Define targets----
    if (WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets),
            timeTarget)
      } else {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets))
      }
    } else {
      if (!is.null(timeDiffTarget)) {
        Targets <- c(Targets, timeTarget)
      } else {
        Targets <- Targets
      }
    }
    
    # Moving stats----
    for (j in seq_along(periods)) {
      for (k in seq_along(statsNames)) {
        for (t in Targets) {
          data.table::set(
            data,
            j = paste0(statsNames[k], "_", periods[j], "_", t),
            value = data.table::frollmean(
              x = data[[eval(t)]],
              n = periods[j],
              fill = NA,
              algo = "fast",
              align = "right",
              na.rm = TRUE,
              hasNA = TRUE,
              adaptive = FALSE
            )
          )
          CounterIndicator <- CounterIndicator + 1
          if (Timer) {
            print(CounterIndicator / runs)
          }
        }
      }
    }
    
    # Replace any inf values with NA----
    for (col in seq_along(data)) {
      data.table::set(data,
                      j = col,
                      value = replace(data[[col]],
                                      is.infinite(data[[col]]), NA))
    }
    
    # Turn character columns into factors----
    for (col in seq_along(data)) {
      if (is.character(data[[col]])) {
        data.table::set(data,
                        j = col,
                        value = as.factor(data[[col]]))
      }
    }
    
    # Impute missing values----
    if (SimpleImpute) {
      for (j in seq_along(data)) {
        if (is.factor(data[[j]])) {
          data.table::set(data,
                          which(!(data[[j]] %in% levels(data[[j]]))), j, "0")
        } else {
          data.table::set(data,
                          which(is.na(data[[j]])), j, -1)
        }
      }
    }
    
    # Done!!----
    if (Timer) {
      return(data)
    }
  }
}

#' An Automated Scoring Feature Engineering Function
#'
#' For scoring purposes (brings back a single row by group), this function creates autoregressive and rolling stats from target columns and distributed lags and distributed rolling stats for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and rolling stats. This function works for data with groups and without groups.
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data A data.table you want to run the function on
#' @param lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param periods A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param statsNames A character vector of the corresponding names to create for the rollings stats variables.
#' @param targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param groupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param sortDateName The column name of your date column used to sort events over time
#' @param timeDiffTarget Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param timeAgg List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you percentage complete tracker printout
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param AscRowByGroup Required to have a column with a Row Number by group (if grouping) with 1 being the record for scoring (typically the most current in time)
#' @param RecordsKeep List the number of records to retain (1 for last record, 2 for last 2 records, etc.)
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' N = 25116
#' data1 <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'                                 Target = stats::filter(rnorm(N,
#'                                                              mean = 50,
#'                                                              sd = 20),
#'                                                        filter=rep(1,10),
#'                                                        circular=TRUE))
#' data1[, temp := seq(1:N)][, DateTime := DateTime - temp]
#' data1 <- data1[order(DateTime)]
#' data1 <- Scoring_GDL_Feature_Engineering(data1,
#'                                          lags           = c(seq(1,5,1)),
#'                                          periods        = c(3,5,10,15,20,25),
#'                                          statsNames     = c("MA"),
#'                                          targets        = c("Target"),
#'                                          groupingVars   = NULL,
#'                                          sortDateName   = c("DateTime"),
#'                                          timeDiffTarget = c("Time_Gap"),
#'                                          timeAgg        = "days",
#'                                          WindowingLag   = 1,
#'                                          Type           = "Lag",
#'                                          Timer          = TRUE,
#'                                          SimpleImpute   = TRUE,
#'                                          AscRowByGroup  = "temp",
#'                                          RecordsKeep    = 1)
#' @export
Scoring_GDL_Feature_Engineering <- function(data,
                                            lags           = c(seq(1, 5, 1)),
                                            periods        = c(3, 5, 10, 15, 20, 25),
                                            statsNames     = c("MA"),
                                            targets        = c("Target"),
                                            groupingVars   = NULL,
                                            sortDateName   = c("DateTime"),
                                            timeDiffTarget = c("Time_Gap"),
                                            timeAgg        = "days",
                                            WindowingLag   = 1,
                                            Type           = "Lag",
                                            Timer          = TRUE,
                                            SimpleImpute   = TRUE,
                                            AscRowByGroup  = "temp",
                                            RecordsKeep    = 1) {
  # Argument Checks----
  if (is.null(lags) & WindowingLag == 1) {
    lags <- 1
  }
  if (!(1 %in% lags) & WindowingLag == 1) {
    lags <- c(1, lags)
  }
  if (any(lags < 0)) {
    warning("lags need to be positive integers")
  }
  if (!is.character(statsNames)) {
    warning("statsNames needs to be a character scalar or vector")
  }
  if (!is.null(groupingVars)) {
    if (!is.character(groupingVars)) {
      warning("groupingVars needs to be a character scalar or vector")
    }
  }
  if (!is.character(targets)) {
    warning("targets needs to be a character scalar or vector")
  }
  if (!is.character(sortDateName)) {
    warning("sortDateName needs to be a character scalar or vector")
  }
  if (!is.null(timeDiffTarget)) {
    if (!is.character(timeDiffTarget)) {
      warning("timeDiffTarget needs to be a character scalar or vector")
    }
  }
  if (!is.null(timeAgg)) {
    if (!is.character(timeAgg)) {
      warning("timeAgg needs to be a character scalar or vector")
    }
  }
  if (!(WindowingLag %in% c(0, 1))) {
    warning("WindowingLag needs to be either 0 or 1")
  }
  if (!(tolower(Type) %chin% c("lag", "lead"))) {
    warning("Type needs to be either Lag or Lead")
  }
  if (!is.logical(Timer)) {
    warning("Timer needs to be TRUE or FALSE")
  }
  if (!is.logical(SimpleImpute)) {
    warning("SimpleImpute needs to be TRUE or FALSE")
  }
  if (!is.character(AscRowByGroup)) {
    warning("AscRowByGroup needs to be a character scalar for the name of your RowID column")
  }
  if (RecordsKeep < 1) {
    warning("RecordsKeep less than 1 means zero data. Why run this?")
  }
  
  # Convert to data.table if not already----
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)
  
  # Max data to keep----
  MAX_RECORDS_FULL <-
    max(max(lags + 1), max(periods + 1), RecordsKeep)
  MAX_RECORDS_LAGS <- max(max(lags + 1), RecordsKeep)
  MAX_RECORDS_ROLL <- max(max(periods + 1), RecordsKeep)
  
  # Set up counter for countdown----
  CounterIndicator <- 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }
  
  # Define total runs----
  if (!is.null(groupingVars)) {
    runs <-
      length(groupingVars) * tarNum * (length(periods) *
                                         length(statsNames) +
                                         length(lags))
  } else {
    runs <-
      tarNum * (length(periods) * length(statsNames) +
                  length(lags))
  }
  
  # Begin feature engineering----
  if (!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {
      Targets <- targets
      # Sort data
      if (tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = -1)
      }
      
      # Remove records----
      tempData <- data[get(AscRowByGroup) <= MAX_RECORDS_FULL]
      
      # Lags
      for (l in seq_along(lags)) {
        for (t in Targets) {
          tempData[, paste0(groupingVars[i],
                            "_LAG_",
                            lags[l], "_", t) := data.table::shift(get(t), n = lags[l], type = "lag"),
                   by = get(groupingVars[i])]
          CounterIndicator <- CounterIndicator + 1
          if (Timer) {
            print(CounterIndicator / runs)
          }
        }
      }
      
      # Time lags----
      if (!is.null(timeDiffTarget)) {
        # Lag the dates first
        for (l in seq_along(lags)) {
          tempData[, paste0(groupingVars[i],
                            "TEMP",
                            lags[l]) := data.table::shift(get(sortDateName),
                                                          n = lags[l],
                                                          type = "lag"), by = get(groupingVars[i])]
        }
        
        # Difference the lag dates----
        if (WindowingLag != 0) {
          for (l in seq_along(lags)) {
            if (l == 1) {
              tempData[, paste0(groupingVars[i],
                                timeDiffTarget,
                                lags[l]) := as.numeric(difftime(
                                  get(sortDateName),
                                  get(paste0(
                                    groupingVars[i], "TEMP", lags[l]
                                  )),
                                  units = eval(timeAgg)
                                )), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            } else {
              tempData[, paste0(groupingVars[i],
                                timeDiffTarget,
                                lags[l]) := as.numeric(difftime(get(
                                  paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                                ),
                                get(
                                  paste0(groupingVars[i], "TEMP", lags[l])
                                ),
                                units = eval(timeAgg))), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        } else {
          for (l in seq_along(lags)) {
            if (l == 1) {
              tempData[, paste0(groupingVars[i],
                                timeDiffTarget,
                                lags[l]) := as.numeric(difftime(
                                  get(sortDateName),
                                  get(paste0(
                                    groupingVars[i], "TEMP", lags[l]
                                  )),
                                  units = eval(timeAgg)
                                )), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            } else {
              tempData[, paste0(groupingVars[i],
                                timeDiffTarget,
                                lags[l]) := as.numeric(difftime(get(
                                  paste0(groupingVars[i], "TEMP", (lags[l - 1]))
                                ),
                                get(
                                  paste0(groupingVars[i], "TEMP", lags[l])
                                ),
                                units = eval(timeAgg))), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
        
        # Remove temporary lagged dates----
        for (l in seq_along(lags)) {
          tempData[, paste0(groupingVars[i], "TEMP", lags[l]) := NULL]
        }
        
        # Store new target----
        timeTarget <- paste0(groupingVars[i], timeDiffTarget, "1")
      }
      
      # Define targets----
      if (WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_", WindowingLag, "_", Targets),
              timeTarget)
        } else {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_", WindowingLag, "_", Targets))
        }
      } else {
        if (!is.null(timeDiffTarget)) {
          Targets <- c(Targets, timeTarget)
        } else {
          Targets <- Targets
        }
      }
      
      # Keep final values----
      tempData1 <-
        tempData#[get(AscRowByGroup) <= eval(RecordsKeep)]
      
      # Moving stats----
      for (j in seq_along(periods)) {
        for (k in seq_along(statsNames)) {
          for (t in Targets) {
            keep <- c(groupingVars[i], t, AscRowByGroup)
            temp2 <-
              tempData[get(AscRowByGroup) <= MAX_RECORDS_ROLL][, ..keep]
            temp3 <-
              temp2[, paste0(groupingVars[i],
                             statsNames[k],
                             "_",
                             periods[j],
                             "_",
                             t) :=  lapply(.SD, mean, na.rm = TRUE),
                    by = get(groupingVars[i]), .SDcols = eval(t)]
            if (Timer) {
              CounterIndicator <- CounterIndicator + 1
              print(CounterIndicator / runs)
            }
            # Merge files----
            temp4 <-
              temp3[get(AscRowByGroup) <=
                      eval(RecordsKeep)][, c(eval(t)) := NULL]
            tempData1 <-
              merge(tempData1,
                    temp4,
                    by = c(eval(groupingVars[i]), eval(AscRowByGroup)))
          }
        }
      }
    }
    
    # Replace any inf values with NA----
    for (col in seq_along(tempData1)) {
      data.table::set(tempData1,
                      j = col,
                      value = replace(tempData1[[col]],
                                      is.infinite(tempData1[[col]]), NA))
    }
    
    # Turn character columns into factors----
    for (col in seq_along(tempData1)) {
      if (is.character(tempData1[[col]])) {
        data.table::set(tempData1,
                        j = col,
                        value = as.factor(tempData1[[col]]))
      }
    }
    
    # Impute missing values----
    if (SimpleImpute) {
      for (j in seq_along(tempData1)) {
        if (is.factor(tempData1[[j]])) {
          data.table::set(tempData1, which(!(
            tempData1[[j]] %in% levels(tempData1[[j]])
          )), j, "0")
        } else {
          data.table::set(tempData1,
                          which(is.na(tempData1[[j]])),
                          j, -1)
        }
      }
    }
    
    # Done!!
    return(tempData1)
    
  } else {
    # Sort data----
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = -1)
    }
    Targets <- targets
    
    # Remove records----
    tempData <- data[get(AscRowByGroup) <= MAX_RECORDS_FULL]
    
    # Lags
    for (l in seq_along(lags)) {
      for (t in Targets) {
        data.table::set(
          tempData,
          j = paste0("LAG_", lags[l], "_", t),
          value = data.table::shift(tempData[[eval(t)]],
                                    n = lags[l],
                                    type = "lag")
        )
        CounterIndicator <- CounterIndicator + 1
        if (Timer) {
          print(CounterIndicator / runs)
        }
      }
    }
    
    # Time lags----
    if (!is.null(timeDiffTarget)) {
      # Lag the dates first----
      for (l in seq_along(lags)) {
        data.table::set(
          tempData,
          j = paste0("TEMP", lags[l]),
          value = data.table::shift(tempData[[eval(sortDateName)]],
                                    n = lags[l],
                                    type = "lag")
        )
      }
      
      # Difference the lag dates----
      if (WindowingLag != 0) {
        for (l in seq_along(lags)) {
          if (l == 1) {
            data.table::set(
              tempData,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                tempData[[eval(sortDateName)]],
                tempData[[eval(paste0("TEMP", lags[l]))]],
                units = eval(timeAgg)
              ))
            )
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          } else {
            data.table::set(
              tempData,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                tempData[[eval(paste0("TEMP", lags[l] - 1))]],
                tempData[[eval(paste0("TEMP", lags[l]))]],
                units = eval(timeAgg)
              ))
            )
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      } else {
        for (l in seq_along(lags)) {
          if (l == 1) {
            data.table::set(
              tempData,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                tempData[[eval(sortDateName)]],
                tempData[[eval(paste0("TEMP", lags[l]))]],
                units = eval(timeAgg)
              ))
            )
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          } else {
            data.table::set(
              tempData,
              j = paste0(timeDiffTarget, "_", lags[l]),
              value = as.numeric(difftime(
                tempData[[eval(paste0("TEMP", (lags[l - 1])))]],
                tempData[[eval(paste0("TEMP", lags[l]))]],
                units = eval(timeAgg)
              ))
            )
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }
      
      # Remove temporary lagged dates----
      for (l in seq_along(lags)) {
        data.table::set(tempData,
                        j = paste0("TEMP", lags[l]),
                        value = NULL)
      }
      
      # Store new target----
      timeTarget <- paste0(timeDiffTarget, "_1")
    }
    
    # Define targets----
    if (WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets),
            timeTarget)
      } else {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets))
      }
    } else {
      if (!is.null(timeDiffTarget)) {
        Targets <- c(Targets, timeTarget)
      } else {
        Targets <- Targets
      }
    }
    
    # Keep final values----
    tempData1 <- tempData[get(AscRowByGroup) <= eval(RecordsKeep)]
    
    # Moving stats----
    for (j in seq_along(periods)) {
      for (k in seq_along(statsNames)) {
        for (t in Targets) {
          keep <- c(t, AscRowByGroup)
          temp2 <-
            tempData[get(AscRowByGroup) <=
                       MAX_RECORDS_FULL][, ..keep]
          data.table::set(
            temp2,
            j = paste0(statsNames[k], "_", periods[j], "_", t),
            value = mean(temp2[[eval(t)]], na.rm = TRUE)
          )
          if (Timer) {
            CounterIndicator <- CounterIndicator + 1
            print(CounterIndicator / runs)
          }
          # Merge files----
          temp4 <-
            temp2[get(AscRowByGroup) <=
                    eval(RecordsKeep)][, c(eval(AscRowByGroup),
                                           eval(t)) := NULL]
          tempData1 <- cbind(tempData1, temp4)
        }
      }
    }
    
    # Replace any inf values with NA----
    for (col in seq_along(tempData1)) {
      data.table::set(tempData1,
                      j = col,
                      value = replace(tempData1[[col]],
                                      is.infinite(tempData1[[col]]), NA))
    }
    
    # Turn character columns into factors----
    for (col in seq_along(tempData1)) {
      if (is.character(tempData1[[col]])) {
        data.table::set(tempData1,
                        j = col,
                        value = as.factor(tempData1[[col]]))
      }
    }
    
    # Impute missing values----
    if (SimpleImpute) {
      for (j in seq_along(tempData1)) {
        if (is.factor(tempData1[[j]])) {
          data.table::set(tempData1, which(!(
            tempData1[[j]] %in% levels(tempData1[[j]])
          )), j, "0")
        } else {
          data.table::set(tempData1,
                          which(is.na(tempData1[[j]])), j, -1)
        }
      }
    }
    
    # Done!!
    return(tempData1)
  }
}

#' An Fast Automated Feature Engineering Function
#'
#' For models with target variables within the realm of the current time frame but not too far back in time, this function creates autoregressive and rolling stats from target columns and distributed lags and distributed rolling stats for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and rolling stats. This function works for data with groups and without groups.
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data A data.table you want to run the function on
#' @param lags A numeric vector of the specific lags you want to have generated. You must include 1 if WindowingLag = 1.
#' @param periods A numeric vector of the specific rolling statistics window sizes you want to utilize in the calculations.
#' @param statsFUNs Vector of functions for your rolling windows, such as mean, sd, min, max, quantile
#' @param statsNames A character vector of the corresponding names to create for the rollings stats variables.
#' @param targets A character vector of the column names for the reference column in which you will build your lags and rolling stats
#' @param groupingVars A character vector of categorical variable names you will build your lags and rolling stats by
#' @param sortDateName The column name of your date column used to sort events over time
#' @param timeDiffTarget Specify a desired name for features created for time between events. Set to NULL if you don't want time between events features created.
#' @param timeAgg List the time aggregation level for the time between events features, such as "hour", "day", "week", "month", "quarter", or "year"
#' @param WindowingLag Set to 0 to build rolling stats off of target columns directly or set to 1 to build the rolling stats off of the lag-1 target
#' @param Type List either "Lag" if you want features built on historical values or "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you percentage complete tracker printout
#' @param SkipCols Defaults to NULL; otherwise supply a character vector of the names of columns to skip
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @param AscRowByGroup Required to have a column with a Row Number by group (if grouping) with 1 being the record for scoring (typically the most current in time)
#' @param RecordsKeep List the number of records to retain (1 for last record, 2 for last 2 records, etc.)
#' @return data.table of original data plus created lags, rolling stats, and time between event lags and rolling stats
#' @examples
#' N = 25116
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(N,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:N)][, DateTime := DateTime - temp]
#' data <- data[order(DateTime)]
#' data <- FAST_GDL_Feature_Engineering(data,
#'                                      lags           = c(1:5),
#'                                      periods        = c(seq(10,50,10)),
#'                                      statsFUNs      = c("mean",
#'                                                         "median",
#'                                                         "sd",
#'                                                         "quantile85",
#'                                                         "quantile95"),
#'                                      statsNames     = c("mean",
#'                                                         "median",
#'                                                         "sd",
#'                                                         "quantile85",
#'                                                         "quantile95"),
#'                                      targets        = c("Target"),
#'                                      groupingVars   = NULL,
#'                                      sortDateName   = "DateTime",
#'                                      timeDiffTarget = c("Time_Gap"),
#'                                      timeAgg        = "days",
#'                                      WindowingLag   = 1,
#'                                      Type           = "Lag",
#'                                      Timer          = TRUE,
#'                                      SkipCols       = FALSE,
#'                                      SimpleImpute   = TRUE,
#'                                      AscRowByGroup  = "temp")
#' @export
FAST_GDL_Feature_Engineering <- function(data,
                                         lags           = c(1:5),
                                         periods        = c(seq(10, 50, 10)),
                                         statsFUNs      = c("mean",
                                                            "median",
                                                            "sd",
                                                            "quantile85",
                                                            "quantile95"),
                                         statsNames     = c("mean",
                                                            "median",
                                                            "sd",
                                                            "quantile85",
                                                            "quantile95"),
                                         targets        = c("Target"),
                                         groupingVars   = c("GroupVariable"),
                                         sortDateName   = c("DateTime"),
                                         timeDiffTarget = NULL,
                                         timeAgg        = c("hours"),
                                         WindowingLag   = 1,
                                         Type           = c("Lag"),
                                         Timer          = FALSE,
                                         SkipCols       = FALSE,
                                         SimpleImpute   = TRUE,
                                         AscRowByGroup  = c("temp"),
                                         RecordsKeep    = 1) {
  # Argument Checks----
  if (is.null(lags) & WindowingLag == 1) {
    lags <- 1
  }
  if (!(1 %in% lags) & WindowingLag == 1) {
    lags <- c(1, lags)
  }
  if (any(lags < 0)) {
    warning("lags need to be positive integers")
  }
  if (!is.character(statsNames)) {
    warning("statsNames needs to be a character scalar or vector")
  }
  if (!is.null(groupingVars)) {
    if (!is.character(groupingVars)) {
      warning("groupingVars needs to be a character scalar or vector")
    }
  }
  if (!is.character(targets)) {
    warning("targets needs to be a character scalar or vector")
  }
  if (!is.character(sortDateName)) {
    warning("sortDateName needs to be a character scalar or vector")
  }
  if (!is.null(timeDiffTarget)) {
    if (!is.character(timeDiffTarget)) {
      warning("timeDiffTarget needs to be a character scalar or vector")
    }
  }
  if (!is.null(timeAgg)) {
    if (!is.character(timeAgg)) {
      warning("timeAgg needs to be a character scalar or vector")
    }
  }
  if (!(WindowingLag %in% c(0, 1))) {
    warning("WindowingLag needs to be either 0 or 1")
  }
  if (!(tolower(Type) %chin% c("lag", "lead"))) {
    warning("Type needs to be either Lag or Lead")
  }
  if (!is.logical(Timer)) {
    warning("Timer needs to be TRUE or FALSE")
  }
  if (!is.logical(SimpleImpute)) {
    warning("SimpleImpute needs to be TRUE or FALSE")
  }
  if (!is.null(SkipCols)) {
    if (!is.character(SkipCols)) {
      warning("SkipCols needs to be a character scalar or vector")
    }
  }
  if (!is.character(AscRowByGroup)) {
    warning("AscRowByGroup needs to be a character scalar for the name of your RowID column")
  }
  if (RecordsKeep < 1) {
    warning("RecordsKeep less than 1 means zero data. Why run this?")
  }
  
  # Convert to data.table if not already----
  if (!data.table::is.data.table(data))
    data <- data.table::as.data.table(data)
  
  # Max data to keep----
  MAX_RECORDS_FULL <-
    max(max(lags + 1), max(periods + 1), RecordsKeep)
  MAX_RECORDS_LAGS <- max(max(lags + 1), RecordsKeep)
  MAX_RECORDS_ROLL <- max(max(periods + 1), RecordsKeep)
  
  # Set up counter for countdown----
  CounterIndicator <- 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }
  
  # Define total runs----
  if (!is.null(groupingVars)) {
    runs <-
      length(groupingVars) * tarNum * (length(periods) *
                                         length(statsNames) +
                                         length(lags))
  } else {
    runs <-
      tarNum * (length(periods) * length(statsNames) +
                  length(lags))
  }
  
  # Begin feature engineering----
  if (!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {
      Targets <- targets
      # Sort data
      if (tolower(Type) == "lag") {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i], sortDateName[1])
        data.table::setorderv(data, colVar, order = -1)
      }
      
      # Remove records----
      tempData <- data[get(AscRowByGroup) <= MAX_RECORDS_FULL]
      
      # Lags----
      for (l in seq_along(lags)) {
        for (t in Targets) {
          if (!(paste0(groupingVars[i],
                       "_LAG_", lags[l], "_", t) %in% SkipCols)) {
            tempData[, paste0(groupingVars[i],
                              "_LAG_",
                              lags[l], "_", t) := data.table::shift(get(t), n = lags[l], type = "lag"),
                     by = get(groupingVars[i])]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }
      
      # Time lags----
      if (!is.null(timeDiffTarget)) {
        # Lag the dates first----
        for (l in seq_along(lags)) {
          if (!(paste0(groupingVars[i], "TEMP", lags[l]) %in% SkipCols)) {
            tempData[, paste0(groupingVars[i],
                              "TEMP",
                              lags[l]) := data.table::shift(get(sortDateName),
                                                            n = lags[l],
                                                            type = "lag"), by = get(groupingVars[i])]
          }
        }
        
        # Difference the lag dates----
        if (WindowingLag != 0) {
          for (l in seq_along(lags)) {
            if (!(paste0(timeDiffTarget,
                         lags[l]) %in% SkipCols) & l == 1) {
              tempData[, paste0(groupingVars[i],
                                timeDiffTarget,
                                lags[l]) := as.numeric(difftime(
                                  get(sortDateName),
                                  get(paste0(
                                    groupingVars[i], "TEMP", lags[l]
                                  )),
                                  units = eval(timeAgg)
                                )), by = get(groupingVars[i])]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            } else {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget, lags[l]) %in% SkipCols)) {
                tempData[, paste0(groupingVars[i],
                                  timeDiffTarget,
                                  lags[l]) := as.numeric(difftime(get(
                                    paste0(groupingVars[i],
                                           "TEMP",
                                           (lags[l - 1]))
                                  ),
                                  get(
                                    paste0(groupingVars[i],
                                           "TEMP",
                                           lags[l])
                                  ),
                                  units = eval(timeAgg))),
                         by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        } else {
          for (l in seq_along(lags)) {
            if (l == 1) {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget, lags[l]) %in% SkipCols)) {
                tempData[, paste0(groupingVars[i],
                                  timeDiffTarget,
                                  lags[l]) := as.numeric(difftime(
                                    get(sortDateName),
                                    get(paste0(
                                      groupingVars[i], "TEMP", lags[l]
                                    )),
                                    units = eval(timeAgg)
                                  )), by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            } else {
              if (!(paste0(groupingVars[i],
                           timeDiffTarget, lags[l]) %in% SkipCols)) {
                tempData[, paste0(groupingVars[i],
                                  timeDiffTarget,
                                  lags[l]) := as.numeric(difftime(get(
                                    paste0(groupingVars[i],
                                           "TEMP",
                                           (lags[l - 1]))
                                  ),
                                  get(
                                    paste0(groupingVars[i],
                                           "TEMP",
                                           lags[l])
                                  ),
                                  units = eval(timeAgg))),
                         by = get(groupingVars[i])]
                CounterIndicator <- CounterIndicator + 1
                if (Timer) {
                  print(CounterIndicator / runs)
                }
              }
            }
          }
        }
        
        # Remove temporary lagged dates----
        for (l in seq_along(lags)) {
          tempData[, paste0(groupingVars[i], "TEMP", lags[l]) := NULL]
        }
        
        # Store new target----
        timeTarget <- paste0(groupingVars[i], timeDiffTarget, "1")
      }
      
      # Define targets----
      if (WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_", WindowingLag, "_", Targets),
              timeTarget)
        } else {
          Targets <-
            c(paste0(groupingVars[i], "_LAG_", WindowingLag, "_", Targets))
        }
      } else {
        if (!is.null(timeDiffTarget)) {
          Targets <- c(Targets, timeTarget)
        } else {
          Targets <- Targets
        }
      }
      
      # Keep final values----
      tempData1 <- tempData[get(AscRowByGroup) <= eval(RecordsKeep)]
      
      # Moving stats----
      for (j in seq_along(periods)) {
        for (k in seq_along(statsNames)) {
          for (t in Targets) {
            if (!(paste0(groupingVars[i],
                         statsNames[k],
                         "_",
                         periods[j],
                         "_", t) %in% SkipCols)) {
              keep <- c(groupingVars[i], t, AscRowByGroup)
              temp2 <-
                tempData[get(AscRowByGroup) <=
                           MAX_RECORDS_ROLL][, ..keep]
              if (statsFUNs[k] == "mean") {
                temp3 <-
                  temp2[, paste0(groupingVars[i],
                                 statsNames[k],
                                 "_",
                                 periods[j],
                                 "_",
                                 t) := caTools::runmean(
                                   get(t),
                                   k = periods[j],
                                   endrule = "mean",
                                   alg = "C"
                                 ), by = get(groupingVars[i])]
              } else if (statsFUNs[k] == "median") {
                temp3 <-
                  temp2[, paste0(groupingVars[i],
                                 statsNames[k],
                                 "_",
                                 periods[j],
                                 "_",
                                 t) := caTools::runquantile(
                                   get(t),
                                   probs = 0.50,
                                   k = periods[j],
                                   endrule = "quantile"
                                 ), by = get(groupingVars[i])]
              } else if (statsFUNs[k] == "sd") {
                temp3 <-
                  temp2[, paste0(groupingVars[i],
                                 statsNames[k],
                                 "_",
                                 periods[j],
                                 "_",
                                 t) := caTools::runsd(get(t),
                                                      k = periods[j],
                                                      endrule = "sd"),
                        by = get(groupingVars[i])]
              } else if (statsFUNs[k] == "quantile85") {
                temp3 <-
                  temp2[, paste0(groupingVars[i],
                                 statsNames[k],
                                 "_",
                                 periods[j],
                                 "_",
                                 t) := caTools::runquantile(
                                   get(t),
                                   probs = 0.85,
                                   k = periods[j],
                                   endrule = "quantile"
                                 ), by = get(groupingVars[i])]
              } else if (statsFUNs[k] == "quantile95") {
                temp3 <-
                  temp2[, paste0(groupingVars[i],
                                 statsNames[k],
                                 "_",
                                 periods[j],
                                 "_",
                                 t) := caTools::runquantile(
                                   get(t),
                                   probs = 0.95,
                                   k = periods[j],
                                   endrule = "quantile"
                                 ), by = get(groupingVars[i])]
              }
              if (Timer) {
                CounterIndicator <- CounterIndicator + 1
                print(CounterIndicator / runs)
              }
              # Merge files----
              temp4 <-
                temp3[get(AscRowByGroup) <= eval(RecordsKeep)][, c(eval(t)) := NULL]
              tempData1 <-
                merge(tempData1, temp4, by = c(eval(groupingVars[i]),
                                               eval(AscRowByGroup)))
            }
          }
        }
      }
    }
    
    # Replace any inf values with NA----
    for (col in seq_along(tempData1)) {
      data.table::set(tempData1,
                      j = col,
                      value = replace(tempData1[[col]],
                                      is.infinite(tempData1[[col]]), NA))
    }
    
    # Turn character columns into factors----
    for (col in seq_along(tempData1)) {
      if (is.character(tempData1[[col]])) {
        data.table::set(tempData1,
                        j = col,
                        value = as.factor(tempData1[[col]]))
      }
    }
    
    # Impute missing values----
    if (SimpleImpute) {
      for (j in seq_along(tempData1)) {
        if (is.factor(tempData1[[j]])) {
          data.table::set(tempData1, which(!(
            tempData1[[j]] %in% levels(tempData1[[j]])
          )), j, "0")
        } else {
          data.table::set(tempData1, which(is.na(tempData1[[j]])), j, -1)
        }
      }
    }
    
    # Done!!
    return(tempData1)
    
  } else {
    # Sort data----
    if (tolower(Type) == "lag") {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      data.table::setorderv(data, colVar, order = -1)
    }
    Targets <- targets
    
    # Remove records----
    tempData <- data[get(AscRowByGroup) <= MAX_RECORDS_FULL]
    
    # Lags
    for (l in seq_along(lags)) {
      for (t in Targets) {
        if (!(paste0("LAG_", lags[l], "_", t) %in% SkipCols)) {
          tempData[, paste0("LAG_", lags[l], "_", t) := data.table::shift(get(t), n = lags[l], type = "lag")]
          CounterIndicator <- CounterIndicator + 1
          if (Timer) {
            print(CounterIndicator / runs)
          }
        }
      }
    }
    
    # Time lags----
    if (!is.null(timeDiffTarget)) {
      # Lag the dates first
      for (l in seq_along(lags)) {
        if (!(paste0("TEMP", lags[l]) %in% SkipCols)) {
          tempData[, paste0("TEMP",
                            lags[l]) := data.table::shift(get(sortDateName), n = lags[l], type = "lag")]
        }
      }
      
      # Difference the lag dates----
      if (WindowingLag != 0) {
        for (l in seq_along(lags)) {
          if (!(paste0(timeDiffTarget, "_", lags[l]) %in% SkipCols) &
              l == 1) {
            tempData[, paste0(timeDiffTarget,
                              "_",
                              lags[l]) := as.numeric(difftime(get(sortDateName),
                                                              get(paste0(
                                                                "TEMP", lags[l]
                                                              )),
                                                              units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          } else {
            tempData[, paste0(timeDiffTarget,
                              "_", lags[l]) := as.numeric(difftime(get(paste0(
                                "TEMP", lags[l] - 1
                              )),
                              get(paste0(
                                "TEMP", lags[l]
                              )),
                              units = eval(timeAgg)))]
            CounterIndicator <- CounterIndicator + 1
            if (Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      } else {
        for (l in seq_along(lags)) {
          if (l == 1) {
            if (!(paste0(timeDiffTarget,
                         "_", lags[l]) %in% SkipCols)) {
              tempData[, paste0(timeDiffTarget,
                                "_", lags[l]) := as.numeric(difftime(
                                  get(sortDateName),
                                  get(paste0("TEMP", lags[l])),
                                  units = eval(timeAgg)
                                ))]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          } else {
            if (!(paste0(timeDiffTarget,
                         "_",
                         lags[l]) %in% SkipCols)) {
              tempData[, paste0(timeDiffTarget,
                                "_",
                                lags[l]) := as.numeric(difftime(get(paste0(
                                  "TEMP", (lags[l - 1])
                                )),
                                get(paste0(
                                  "TEMP", lags[l]
                                )),
                                units = eval(timeAgg)))]
              CounterIndicator <- CounterIndicator + 1
              if (Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }
      
      # Remove temporary lagged dates----
      for (l in seq_along(lags)) {
        tempData[, paste0("TEMP", lags[l]) := NULL]
      }
      
      # Store new target----
      timeTarget <- paste0(timeDiffTarget, "_1")
    }
    
    # Define targets----
    if (WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets),
            timeTarget)
      } else {
        Targets <-
          c(paste0("LAG_", WindowingLag, "_", Targets))
      }
    } else {
      if (!is.null(timeDiffTarget)) {
        Targets <- c(Targets, timeTarget)
      } else {
        Targets <- Targets
      }
    }
    
    # Keep final values----
    tempData1 <- tempData[get(AscRowByGroup) <= eval(RecordsKeep)]
    
    # Moving stats----
    for (j in seq_along(periods)) {
      for (k in seq_along(statsNames)) {
        for (t in Targets) {
          if (!(paste0(statsNames[k],
                       "_",
                       periods[j],
                       "_",
                       t) %in% SkipCols)) {
            keep <- c(t, AscRowByGroup)
            temp2 <-
              tempData1[get(AscRowByGroup) <= MAX_RECORDS_ROLL][, ..keep]
            if (statsFUNs[k] == "mean") {
              temp3 <-
                temp2[, paste0(statsNames[k],
                               "_",
                               periods[j],
                               "_",
                               t) := caTools::runmean(get(t),
                                                      k = periods[j],
                                                      endrule = "mean",
                                                      alg = "C")]
            } else if (statsFUNs[k] == "median") {
              temp3 <-
                temp2[, paste0(statsNames[k],
                               "_",
                               periods[j],
                               "_",
                               t) := caTools::runquantile(
                                 get(t),
                                 probs = 0.50,
                                 k = periods[j],
                                 endrule = "quantile"
                               )]
            } else if (statsFUNs[k] == "sd") {
              temp3 <-
                temp2[, paste0(statsNames[k],
                               "_",
                               periods[j],
                               "_",
                               t) := caTools::runsd(get(t),
                                                    k = periods[j],
                                                    endrule = "sd")]
            } else if (statsFUNs[k] == "quantile85") {
              temp3 <-
                temp2[, paste0(statsNames[k],
                               "_",
                               periods[j],
                               "_",
                               t) := caTools::runquantile(
                                 get(t),
                                 probs = 0.85,
                                 k = periods[j],
                                 endrule = "quantile"
                               )]
            } else if (statsFUNs[k] == "quantile95") {
              temp3 <-
                temp2[, paste0(statsNames[k],
                               "_",
                               periods[j],
                               "_",
                               t) := caTools::runquantile(
                                 get(t),
                                 probs = 0.95,
                                 k = periods[j],
                                 endrule = "quantile"
                               )]
            }
            if (Timer) {
              CounterIndicator <- CounterIndicator + 1
              print(CounterIndicator / runs)
            }
            # Merge files----
            temp4 <-
              temp3[get(AscRowByGroup) <=
                      eval(RecordsKeep)][, c(eval(t)) := NULL]
            tempData1 <-
              merge(tempData1, temp4, by = c(eval(AscRowByGroup)))
          }
        }
      }
    }
    
    # Replace any inf values with NA----
    for (col in seq_along(tempData1)) {
      data.table::set(tempData1,
                      j = col,
                      value = replace(tempData1[[col]],
                                      is.infinite(tempData1[[col]]),
                                      NA))
    }
    
    # Turn character columns into factors----
    for (col in seq_along(tempData1)) {
      if (is.character(tempData1[[col]])) {
        data.table::set(tempData1,
                        j = col,
                        value = as.factor(tempData1[[col]]))
      }
    }
    
    # Impute missing values----
    if (SimpleImpute) {
      for (j in seq_along(tempData1)) {
        if (is.factor(tempData1[[j]])) {
          data.table::set(tempData1,
                          which(!(
                            tempData1[[j]] %in% levels(tempData1[[j]])
                          )),
                          j,
                          "0")
        } else {
          data.table::set(tempData1,
                          which(is.na(tempData1[[j]])),
                          j, -1)
        }
      }
    }
    
    # Ensure correct order of columns----
    setcolorder(tempData1, c(2, 3, 1, 4:ncol(tempData1)))
    
    # Done!!
    return(tempData1)
  }
}

#' The AutoDataPartition function
#'
#' This function will take your ratings matrix and model and score your data in parallel.
#' @author Adrian Antico and Douglas Pestana
#' @family Feature Engineering
#' @param data Source data to do your partitioning on
#' @param NumDataSets The number of total data sets you want built
#' @param Ratios A vector of values for how much data each data set should get in each split. E.g. c(0.70, 0.20, 0.10)
#' @param PartitionType Set to either "random", "timeseries", or "time". With "random", your data will be paritioned randomly (with stratified sampling if column names are supplied). With "timeseries", you can partition by time with a stratify option (so long as you have an equal number of records for each strata). With "time" you will have data sets generated so that the training data contains the earliest records in time, validation data the second earliest, test data the third earliest, etc.
#' @param StratifyColumnNames Supply column names of categorical features to use in a stratified sampling procedure for partitioning the data. Partition type must be "random" to use this option
#' @param StratifyNumericTarget Supply a column name that is numeric. Use for "random" PartitionType, you can stratify your numeric variable by splitting up based on percRank to ensure a proper allocation of extreme values in your created data sets.
#' @param StratTargetPrecision For "random" PartitionType and when StratifyNumericTarget is not null, precision will be the number of decimals used in the percentile calculation. If you supply a value of 1, deciles will be used. For a value of 2, percentiles will be used. Larger values are supported.
#' @param TimeColumnName Supply a date column name or a name of a column with an ID for sorting by time such that the smallest number is the earliest in time.
#' @return Returns a list of data.tables
#' @examples
#' \donttest{
#' dataSets <- AutoDataPartition(data,
#'                               NumDataSets = 3,
#'                               Ratios = c(0.70,0.20,0.10),
#'                               PartitionType = "random",
#'                               StratifyColumnNames = NULL,
#'                               StratifyNumericTarget = NULL,
#'                               StratTargetPrecision = 1,
#'                               TimeColumnName = NULL)
#' }
#' @export
AutoDataPartition <- function(data,
                              NumDataSets = 3,
                              Ratios = c(0.70, 0.20, 0.10),
                              PartitionType = "random",
                              StratifyColumnNames = NULL,
                              StratifyNumericTarget = NULL,
                              StratTargetPrecision = 3,
                              TimeColumnName = NULL) {
  # Arguments----
  if (NumDataSets < 0) {
    warning("NumDataSets needs to be a positive integer. Typically 3 modeling sets are used.")
  }
  if (!is.null(StratifyNumericTarget)) {
    if (!is.character(StratifyNumericTarget)) {
      warning("StratifyNumericTarget your target column name in quotes")
    }
    if (!is.numeric(StratTargetPrecision)) {
      warning("StratTargetPrecision needs to be values of 1,2,...,N")
    }
  }
  if (abs(round(NumDataSets) - NumDataSets) > 0.01) {
    warning("NumDataSets needs to be an integer valued positive number")
  }
  if (length(Ratios) != NumDataSets) {
    warning("You need to supply the percentage of data used for each data set.")
  }
  if (sum(Ratios) != 1.0) {
    warning("The sum of Ratios needs to equal 1.0")
  }
  if (!(tolower(PartitionType) %chin% c("random", "time", "timeseries"))) {
    warning("PartitionType needs to be either 'random', 'timeseries' or 'time'.")
  }
  if (!is.null(StratifyColumnNames)) {
    if (!is.character(StratifyColumnNames)) {
      warning("StratifyColumnNames needs to be a character vector of column names")
    }
    if (!all(StratifyColumnNames %chin% names(data))) {
      warning("StratifyColumnNames not in vector of data names")
    }
  }
  if (!is.null(TimeColumnName)) {
    if (!(TimeColumnName %chin% names(data))) {
      warning("TimeColumnName not in vector of data names")
    }
    if (is.character(data[[eval(TimeColumnName)]]) |
        is.factor(data[[eval(TimeColumnName)]])) {
      warning("TimeColumnName is not a data, Posix_, numeric, or integer valued column")
    }
  }
  
  # Ensure data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Stratify Numeric Target----
  if (PartitionType == "random") {
    if (!is.null(StratifyNumericTarget)) {
      data[, StratCol := as.factor(round(percRank(data[[eval(StratifyNumericTarget)]]), StratTargetPrecision))]
      StratifyColumnNames <- "StratCol"
    }
  }
  
  # Partition Steps----
  if (is.null(TimeColumnName)) {
    # Copy data----
    copy_data <- data.table::copy(data)
    
    # Put Stratify Column Names in Variable----
    DataCollect <- list()
    if (!is.null(StratifyColumnNames)) {
      keep <- c(eval(StratifyColumnNames))
    }
    
    # Modify ratios to account for data decrements----
    RatioList <- c()
    RatioList[NumDataSets] <- Ratios[NumDataSets]
    for (i in (NumDataSets - 1):1) {
      tempRatio <- 0
      for (j in (i + 1):NumDataSets) {
        tempRatio <- Ratios[j] + tempRatio
      }
      RatioList[i] <- Ratios[i] * (1 / (1 - tempRatio))
    }
    
    # Gather Row Numbers----
    RowList <- list()
    for (i in NumDataSets:1) {
      if (!is.null(StratifyColumnNames)) {
        if (i == 1) {
          temp <- copy_data
        } else {
          x <-
            copy_data[, .I[sample(.N, .N * RatioList[i])], by = list(get(keep))]$V1
          RowList[[i]] <- x
          copy_data <- copy_data[-x]
        }
      } else {
        if (i == 1) {
          temp <- copy_data
        } else {
          x <- copy_data[, .I[sample(.N, .N * RatioList[i])]]
          RowList[[i]] <- x
          copy_data <- copy_data[-x]
        }
      }
    }
    
    # Partition Data----
    for (i in seq_len(NumDataSets)) {
      if (i == 1) {
        DataCollect[["TrainData"]] <- temp
      } else if (i == 2) {
        DataCollect[["ValidationData"]] <- data[eval(RowList[[i]])]
      } else if (i == 3) {
        DataCollect[["TestData"]] <- data[RowList[[i]]]
      } else {
        DataCollect[[paste0("TestData", NumDataSets - 2)]] <-
          data[RowList[[i]]]
      }
    }
    
    # Remove StratCol from StratifyNumericTarget----
    if (PartitionType == "random") {
      if (!is.null(StratifyNumericTarget)) {
        x1 <- DataCollect$TrainData
        x1[, StratCol := NULL]
        x2 <- DataCollect$ValidationData
        x2[, StratCol := NULL]
        x3 <- DataCollect$TestData
        x3[, StratCol := NULL]
        DataCollect$TrainData <- x1
        DataCollect$Validation <- x2
        DataCollect$TestData <- x3
      }
    }
    
  } else if (tolower(PartitionType) == "timeseries" &
             !is.null(StratifyColumnNames)) {
    # Initialize DataCollect
    DataCollect <- list()
    
    # Add ID by Strata Groups
    data[, ID := 1:.N, by = c(eval(StratifyColumnNames))]
    
    # Ensure row counts are all equal by strata
    if (var(data[, mean(ID), by = c(eval(StratifyColumnNames))][["V1"]]) != 0) {
      return(
        "There are an unequal number of records by strata.
             PartitionType 'timeseries' requires equal number of observations for each strata"
      )
    }
    
    # Get Total Row Count
    Rows <- data[, .N, by = c(eval(StratifyColumnNames))][1, N]
    
    # Figure out which rows go to which data set
    for (i in NumDataSets:1) {
      if (i == 1) {
        DataCollect[["TrainData"]] <- data
      } else if (i == 2) {
        RowEnd <- data[, .N, by = c(eval(StratifyColumnNames))][1, N]
        NumRows <- floor(Ratios[i] * Rows)
        DataCollect[["ValidationData"]] <-
          data[ID %in% (RowEnd - NumRows + 1):RowEnd]
        DataCollect[["ValidationData"]] <-
          DataCollect[["ValidationData"]][, ID := NULL]
        data <- data[-((RowEnd - NumRows + 1):RowEnd)][, ID := NULL]
      } else if (i == 3) {
        RowEnd <- data[, .N, by = c(eval(StratifyColumnNames))][1, N]
        NumRows <- floor(Ratios[i] * Rows)
        DataCollect[["TestData"]] <-
          data[ID %in% (RowEnd - NumRows + 1):RowEnd]
        DataCollect[["TestData"]] <-
          DataCollect[["TestData"]][, ID := NULL]
        data <- data[-((RowEnd - NumRows + 1):RowEnd)]
      } else {
        RowEnd <- data[, .N, by = c(eval(StratifyColumnNames))][1, N]
        NumRows <- floor(Ratios[i] * Rows)
        DataCollect[[paste0("TestData", NumDataSets - 2)]] <-
          data[ID %in% (RowEnd - NumRows + 1):RowEnd]
        DataCollect[[paste0("TestData", NumDataSets - 2)]] <-
          DataCollect[[paste0("TestData", NumDataSets - 2)]][, ID := NULL]
        data <- data[-((RowEnd - NumRows + 1):RowEnd)]
      }
    }
  } else {
    # Initialize DataCollect
    DataCollect <- list()
    
    # Sort data by TimeColumnName
    data <- data[order(get(TimeColumnName))]
    
    # Get Total Row Count
    Rows <- data[, .N]
    
    # Figure out which rows go to which data set
    for (i in NumDataSets:1) {
      if (i == 1) {
        DataCollect[["TrainData"]] <- data
      } else if (i == 2) {
        RowEnd <- data[, .N]
        NumRows <- floor(Ratios[i] * Rows)
        DataCollect[["ValidationData"]] <-
          data[(RowEnd - NumRows + 1):RowEnd]
        data <- data[-((RowEnd - NumRows + 1):RowEnd)]
      } else if (i == 3) {
        RowEnd <- data[, .N]
        NumRows <- floor(Ratios[i] * Rows)
        DataCollect[["TestData"]] <-
          data[(RowEnd - NumRows + 1):RowEnd]
        data <- data[-((RowEnd - NumRows + 1):RowEnd)]
      } else {
        RowEnd <- data[, .N]
        NumRows <- floor(Ratios[i] * Rows)
        DataCollect[[paste0("TestData", NumDataSets - 2)]] <-
          data[(RowEnd - NumRows + 1):RowEnd]
        data <- data[-((RowEnd - NumRows + 1):RowEnd)]
      }
    }
  }
  return(DataCollect)
}

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
                                Threads       = max(1, parallel::detectCores() -
                                                      2),
                                MaxMemory     = "28G",
                                SaveOutput    = FALSE) {
  # Ensure data is a data.table
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Create storage file
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
  return(data)
}

#' AutoNLS is a function for automatically building nls models
#'
#' This function will build models for 9 different nls models, along with a non-parametric monotonic regression and a polynomial regression. The models are evaluated, a winner is picked, and the predicted values are stored in your data table.
#'
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data Data is the data table you are building the modeling on
#' @param y Y is the target variable name in quotes
#' @param x X is the independent variable name in quotes
#' @param monotonic This is a TRUE/FALSE indicator - choose TRUE if you want monotonic regression over polynomial regression
#' @examples
#' # Create Growth Data
#' data <-
#'   data.table::data.table(Target = seq(1, 500, 1),
#'                          Variable = rep(1, 500))
#' for (i in as.integer(1:500)) {
#'   if (i == 1) {
#'     var <- data[i, "Target"][[1]]
#'     data.table::set(data,
#'                     i = i,
#'                     j = 2L,
#'                     value = var * (1 + runif(1) / 100))
#'   } else {
#'     var <- data[i - 1, "Variable"][[1]]
#'     data.table::set(data,
#'                     i = i,
#'                     j = 2L,
#'                     value = var * (1 + runif(1) / 100))
#'   }
#' }
#'
#' # Add jitter to Target
#' data[, Target := jitter(Target,
#'                         factor = 0.25)]
#'
#' # To keep original values
#' data1 <- data.table::copy(data)
#'
#' # Merge and Model data
#' data11 <- AutoNLS(
#'   data = data,
#'   y = "Target",
#'   x = "Variable",
#'   monotonic = TRUE
#' )
#'
#' # Join predictions to source data
#' data2 <- merge(
#'   data1,
#'   data11$PredictionData,
#'   by = "Variable",
#'   all = FALSE
#' )
#'
#' # Plot output
#' ggplot2::ggplot(data2, ggplot2::aes(x = Variable)) +
#'   ggplot2::geom_line(ggplot2::aes(y = data2[["Target.x"]],
#'                                   color = "Target")) +
#'   ggplot2::geom_line(ggplot2::aes(y = data2[["Target.y"]],
#'                                   color = "Predicted")) +
#'  RemixAutoML::ChartTheme(Size = 12) +
#'   ggplot2::ggtitle(paste0("Growth Models AutoNLS: ",
#'                           data11$ModelName)) +
#'   ggplot2::ylab("Target Variable") +
#'   ggplot2::xlab("Independent Variable") +
#'   ggplot2::scale_colour_manual("Values",
#'                                breaks = c("Target",
#'                                           "Predicted"),
#'                                values = c("red",
#'                                           "blue"))
#' summary(data11$ModelObject)
#' data11$EvaluationMetrics
#' @return A list containing "PredictionData" which is a data table with your original column replaced by the nls model predictions; "ModelName" the model name; "ModelObject" The winning model to later use; "EvaluationMetrics" Model metrics for models with ability to build.
#' @export
AutoNLS <- function(data,
                    y,
                    x,
                    monotonic = TRUE) {
  # Begin
  DATA <- data
  nls_collection <-
    data.table::data.table(
      ModelName = c(
        "Poly",
        "Asymp",
        "AsympOff",
        "AsympOrig",
        "Biexp",
        "FourParmLog",
        "Gompertz",
        "Logistic",
        "Michal_Menton",
        "Weilbull"
      ),
      MeanAbsError = rep(999, 10)
    )
  
  # Convert to data.table if not already
  if (!data.table::is.data.table(data))
    DATA <- data.table::as.data.table(data)
  
  data.table::setnames(DATA,
                       c(eval(y), eval(x)),
                       c("Target", "Variable"))
  
  z <- DATA[["Variable"]]
  zz <- DATA[["Target"]]
  tryCatch({
    if (monotonic == TRUE) {
      tryCatch({
        baseline <- monreg::monreg(z, zz, hr = 0.5, hd = 0.5)
        preds    <- baseline$estimation
        preds[preds < 0] <- 0
        val0     <- base::mean(abs(zz - preds))
        data.table::set(nls_collection, 1L, 2L, value = val0)
      }, error = function(x) {
        return("skip")
      })
    } else {
      tryCatch({
        baseline <-
          stats::lm(as.formula(Target ~ poly(Variable,
                                             5)),
                    data = DATA)
        preds    <- baseline$fitted.values
        preds[preds < 0] <- 0
        val0     <- base::mean(abs(zz - preds))
        data.table::set(nls_collection, 1L, 2L, value = val0)
      }, error = function(x) {
        return("skip")
      })
    }
  }, error = function(x) {
    return("skip")
  })
  
  # Asymp model
  tryCatch({
    model1 <-
      stats::nls(Target ~ SSasymp(Variable, Asym, R0, lrc),
                 data = DATA)
    preds1 <- stats::fitted(model1, DATA)
    preds1[preds1 < 0] <- 0
    val    <- base::mean(abs(zz - preds1))
    data.table::set(nls_collection, 2L, 2L, value = val)
  }, error = function(x) {
    return("skip")
  })
  
  # Asymp offset model
  tryCatch({
    model2 <-
      stats::nls(Target ~ SSasympOff(Variable, Asym, lrc, c0),
                 data = DATA)
    preds2 <- stats::fitted(model2, DATA)
    preds2[preds2 < 0] <- 0
    val2    <- base::mean(abs(zz - preds2))
    data.table::set(nls_collection, 3L, 2L, value = val2)
  }, error = function(x) {
    return("skip")
  })
  
  # Asymp origin model
  tryCatch({
    model3 <-
      stats::nls(Target ~ SSasympOrig(Variable, Asym, lrc),
                 data = DATA)
    preds3 <- stats::fitted(model3, DATA)
    preds3[preds3 < 0] <- 0
    val3    <- base::mean(abs(zz - preds3))
    data.table::set(nls_collection, 4L, 2L, value = val3)
  }, error = function(x) {
    return("skip")
  })
  
  # Biexp model
  tryCatch({
    model4 <-
      stats::nls(Target ~ SSbiexp(Variable, A1, lrc1, A2, lrc2),
                 data = DATA)
    preds4 <- stats::fitted(model4, DATA)
    preds4[preds4 < 0] <- 0
    val4   <- base::mean(abs(zz - preds4))
    data.table::set(nls_collection, 5L, 2L, value = val4)
  }, error = function(x) {
    return("skip")
  })
  
  # Four parameter logistic model
  tryCatch({
    model5 <-
      stats::nls(Target ~ SSfpl(Variable, A, B, xmid, scal),
                 data = DATA)
    preds5 <- stats::fitted(model5, DATA)
    preds5[preds5 < 0] <- 0
    val5   <- base::mean(abs(zz - preds5))
    data.table::set(nls_collection, 6L, 2L, value = val5)
  }, error = function(x) {
    return("skip")
  })
  
  # Gompertz model
  tryCatch({
    model6 <-
      stats::nls(Target ~ SSgompertz(Variable, Asym, b2, b3),
                 data = DATA)
    preds6 <- stats::fitted(model6, DATA)
    preds6[preds6 < 0] <- 0
    val6   <- base::mean(abs(zz - preds6))
    data.table::set(nls_collection, 7L, 2L, value = val6)
  }, error = function(x) {
    return("skip")
  })
  
  # Logistic model
  tryCatch({
    model7 <-
      stats::nls(Target ~ SSlogis(Variable, Asym, xmid, scal),
                 data = DATA)
    preds7 <- stats::fitted(model7, DATA)
    preds7[preds7 < 0] <- 0
    val7   <- base::mean(abs(zz - preds7))
    data.table::set(nls_collection, 8L, 2L, value = val7)
  }, error = function(x) {
    return("skip")
  })
  
  # Michaelis-Menton model
  tryCatch({
    model8 <-
      stats::nls(Target ~ SSmicmen(Variable, Vm, K),
                 data = DATA)
    preds8 <- stats::fitted(model8, DATA)
    preds8[preds8 < 0] <- 0
    val8   <- base::mean(abs(zz - preds8))
    data.table::set(nls_collection, 9L, 2L,
                    value = val8)
  }, error = function(x) {
    return("skip")
  })
  
  # Weibull Growth model
  tryCatch({
    model9 <-
      stats::nls(Target ~ SSweibull(Variable, Asym, Drop, lrc, pwr),
                 data = DATA)
    preds9 <- stats::fitted(model9, DATA)
    preds9[preds9 < 0] <- 0
    val9   <- base::mean(abs(zz - preds9))
    data.table::set(nls_collection, 10L, 2L, value = val9)
  }, error = function(x) {
    return("skip")
  })
  
  # Store best model name
  name <-
    nls_collection[MeanAbsError != 999][order(MeanAbsError)][1, 1][[1]]
  
  # Collect metrics for all models fitted
  temp <- nls_collection[MeanAbsError != 999][order(MeanAbsError)]
  
  # Create column using best model
  if (name == nls_collection[10, 1][[1]]) {
    DATA[, Target := preds9]
    data.table::setnames(DATA,
                         c("Target", "Variable"),
                         c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model9,
        EvaluationMetrics = temp
      )
    )
  } else if (name == nls_collection[2, 1][[1]]) {
    DATA[, Target := preds1]
    data.table::setnames(DATA,
                         c("Target", "Variable"),
                         c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model1,
        EvaluationMetrics = temp
      )
    )
  } else if (name == nls_collection[3, 1][[1]]) {
    DATA[, Target := preds2]
    data.table::setnames(DATA,
                         c("Target", "Variable"),
                         c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model2,
        EvaluationMetrics = temp
      )
    )
  } else if (name == nls_collection[4, 1][[1]]) {
    DATA[, Target := preds3]
    data.table::setnames(DATA,
                         c("Target", "Variable"),
                         c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model3,
        EvaluationMetrics = temp
      )
    )
  } else if (name == nls_collection[5, 1][[1]]) {
    DATA[, Target := preds4]
    data.table::setnames(DATA,
                         c("Target", "Variable"),
                         c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model4,
        EvaluationMetrics = temp
      )
    )
  } else if (name == nls_collection[6, 1][[1]]) {
    DATA[, Target := preds5]
    data.table::setnames(DATA,
                         c("Target", "Variable"),
                         c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model5,
        EvaluationMetrics = temp
      )
    )
  } else if (name == nls_collection[7, 1][[1]]) {
    DATA[, Target := preds6]
    data.table::setnames(DATA,
                         c("Target", "Variable"),
                         c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model6,
        EvaluationMetrics = temp
      )
    )
  } else if (name == nls_collection[8, 1][[1]]) {
    DATA[, Target := preds7]
    data.table::setnames(DATA,
                         c("Target", "Variable"),
                         c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model7,
        EvaluationMetrics = temp
      )
    )
  } else if (name == nls_collection[9, 1][[1]]) {
    DATA[, Target := preds8]
    data.table::setnames(DATA,
                         c("Target", "Variable"),
                         c(eval(y), eval(x)))
    return(
      list(
        PredictionData = DATA,
        ModelName = name,
        ModelObject = model8,
        EvaluationMetrics = temp
      )
    )
  } else {
    DATA[, Target := preds]
    data.table::setnames(DATA,
                         c("Target", "Variable"),
                         c(eval(y), eval(x)))
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
        EvaluationMetrics = temp
      )
    )
  }
}

#' AutoTS is an automated time series modeling function
#'
#' Step 1 is to build all the models and evaluate them on the number of HoldOutPeriods periods you specify. Step 2 is to pick the winner and rebuild the winning model on the full data set. Step 3 is to generate forecasts with the final model for FCPeriods that you specify.
#' AutoTS builds the best time series models for each type, using optimized box-cox transformations and using a user-supplied frequency for the ts data conversion along with a model-based frequency for the ts data conversion, compares all types, selects the winner, and generates a forecast. Models include:
#'
#' DSHW: Double Seasonal Holt Winters
#'
#' ARFIMA: Auto Regressive Fractional Integrated Moving Average
#'
#' ARIMIA: Stepwise Auto Regressive Integrated Moving Average with specified max lags, seasonal lags, moving averages, and seasonal moving averages
#'
#' ETS: Additive and Multiplicitive Exponential Smoothing and Holt Winters
#'
#' NNetar: Auto Regressive Neural Network models automatically compares models with 1 lag or 1 seasonal lag compared to models with up to N lags and N seasonal lags
#'
#' TBATS: Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components
#'
#' TSLM: Time Series Linear Model - builds a linear model with trend and season components extracted from the data
#'
#' @author Adrian Antico and Douglas Pestana
#' @family Time Series
#' @param data is the source time series data as a data.table - or a data structure that can be converted to a data.table
#' @param TargetName is the name of the target variable in your data.table
#' @param DateName is the name of the date column in your data.table
#' @param FCPeriods is the number of periods into the future you wish to forecast
#' @param HoldOutPeriods is the number of periods to use for validation testing
#' @param EvaluationMetric Set this to either "MAPE", "MSE", or "MAE". Default is "MAPE"
#' @param TimeUnit is the level of aggregation your dataset comes in
#' @param Lags is the number of lags you wish to test in various models (same as moving averages)
#' @param SLags is the number of seasonal lags you wish to test in various models (same as moving averages)
#' @param NumCores is the number of cores available on your computer
#' @param SkipModels Don't run specified models - e.g. exclude all models "DSHW" "ARFIMA" "ARIMA" "ETS" "NNET" "TBATS" "TSLM"
#' @param StepWise Set to TRUE to have ARIMA and ARFIMA run a stepwise selection process. Otherwise, all models will be generated in parallel execution, but still run much slower.
#' @param TSClean Set to TRUE to have missing values interpolated and outliers replaced with interpolated values: creates separate models for a larger comparison set
#' @param ModelFreq Set to TRUE to run a separate version of all models where the time series frequency is chosen algorithmically
#' @param PrintUpdates Set to TRUE for a print to console of function progress
#' @examples
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(100,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:100)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' output <-   AutoTS(data,
#'                    TargetName       = "Target",
#'                    DateName         = "DateTime",
#'                    FCPeriods        = 1,
#'                    HoldOutPeriods   = 1,
#'                    EvaluationMetric = "MAPE",
#'                    TimeUnit         = "day",
#'                    Lags             = 1,
#'                    SLags            = 1,
#'                    NumCores         = 4,
#'                    SkipModels       = c("NNET","TBATS","ETS","TSLM","ARFIMA","DSHW"),
#'                    StepWise         = TRUE,
#'                    TSClean          = FALSE,
#'                    ModelFreq        = TRUE,
#'                    PrintUpdates     = FALSE)
#' ForecastData <- output$Forecast
#' ModelEval    <- output$EvaluationMetrics
#' WinningModel <- output$TimeSeriesModel
#' @return Returns a list containing 1: A data.table object with a date column and the forecasted values; 2: The model evaluation results; 3: The champion model for later use if desired; 4: The name of the champion model; 5. A time series ggplot with historical values and forecasted values.
#' @export
AutoTS <- function(data,
                   TargetName     = "Target",
                   DateName       = "DateTime",
                   FCPeriods      = 30,
                   HoldOutPeriods = 30,
                   EvaluationMetric = "MAPE",
                   TimeUnit       = "day",
                   Lags           = 25,
                   SLags          = 2,
                   NumCores       = 4,
                   SkipModels     = NULL,
                   StepWise       = TRUE,
                   TSClean        = TRUE,
                   ModelFreq      = TRUE,
                   PrintUpdates   = FALSE) {
  # Check arguments----
  if (!is.character(TargetName)) {
    warning("TargetName needs to be a character value")
  }
  if (!is.character(DateName)) {
    warning("DateName needs to be a character value")
  }
  if (FCPeriods < 0) {
    warning("FCPeriods needs to be greater than 0")
  }
  if (HoldOutPeriods < 0) {
    warning("HoldOutPeriods needs to be greater than 0")
  }
  if (!is.character(TimeUnit)) {
    warning("TimeUnit needs to be a character value")
  }
  if (Lags < 0) {
    warning("Lags needs to be greater than 0")
  }
  if (!is.null(SkipModels)) {
    if (!any(
      toupper(SkipModels) %chin% c("DSHW", "ARFIMA", "ARIMA", "ETS", "NNET", "TBATS", "TSLM")
    )) {
      warning("SkipModels needs to be one of DSHW, ARFIMA, ARIMA, ETS, NNET, TBATS, TSLM")
    }
  }
  
  # Turn off warnings
  options(warn = -1)
  
  # Initialize collection variables
  i <- 0
  EvalList <- list()
  
  # Convert to data.table if not already
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Ensure correct ordering and subsetting of data
  keep <- c(DateName, TargetName)
  data <- data[, ..keep]
  
  # Check for min value of data
  MinVal <- data[, min(get(TargetName))]
  
  # Convert to lubridate as_date() or POSIXct
  if (tolower(TimeUnit) != "hour") {
    data[, eval(DateName) := lubridate::as_date(get(DateName))]
  } else {
    data[, eval(DateName) := as.POSIXct(get(DateName))]
  }
  
  # Correct ordering----
  if (is.numeric(data[[1]]) | is.integer(data[[1]])) {
    data.table::setcolorder(data, c(2, 1))
  }
  
  # Ensure data is sorted----
  data <- data[order(get(DateName))]
  
  # Change Target Name----
  TempTargetName <- TargetName
  data.table::setnames(data, paste0(eval(TargetName)), "Target")
  TargetName <- "Target"
  
  # Create Training data----
  data_train <- data[1:(nrow(data) - HoldOutPeriods)]
  
  # Create Test data----
  data_test <- data[(nrow(data) - HoldOutPeriods + 1):nrow(data)]
  
  # Check for different time aggregations
  MaxDate <- data[, max(get(DateName))]
  FC_Data <- data.table::data.table(Date = seq(1:FCPeriods))
  
  # Define TS Frequency----
  if (tolower(TimeUnit) == "hour") {
    freq <- 24
    FC_Data[, Date := MaxDate + lubridate::hours(Date)]
  } else if (tolower(TimeUnit) == "day") {
    freq <- 365
    FC_Data[, Date := MaxDate + lubridate::days(Date)]
  } else if (tolower(TimeUnit) == "week") {
    freq <- 52
    FC_Data[, Date := MaxDate + lubridate::weeks(Date)]
  } else if (tolower(TimeUnit) == "month") {
    freq <- 12
    FC_Data[, Date := as.Date(MaxDate) %m+% months(Date)]
  } else if (tolower(TimeUnit) == "quarter") {
    freq <- 4
    FC_Data[, Date := MaxDate + lubridate::month(4 * Date)]
  } else if (tolower(TimeUnit) == "year") {
    freq <- 1
    FC_Data[, Date := MaxDate + lubridate::years(Date)]
  } else {
    return("TimeUnit is not in hour, day, week, month,
    quarter, or year")
  }
  
  # Coerce SLags if too large----
  if (freq * SLags > nrow(data_train)) {
    SLags <- floor(nrow(data_train) / freq)
  }
  
  # Convert data.tables to stats::ts objects----
  # User Supplied Frequency
  dataTSTrain <-
    stats::ts(data = data_train,
              start = data_train[, min(get(DateName))][[1]],
              frequency = freq)
  
  # TSClean Version----
  if (TSClean) {
    if (MinVal > 0) {
      Target <- forecast::tsclean(x = dataTSTrain[, TargetName],
                                  replace.missing = TRUE,
                                  lambda = "auto")
    } else {
      Target <- forecast::tsclean(x = dataTSTrain[, TargetName],
                                  replace.missing = TRUE,
                                  lambda = NULL)
    }
  }
  
  # Model-Based Frequency----
  SFreq <- forecast::findfrequency(as.matrix(data_train[, 2]))
  dataTSTrain1 <-
    stats::ts(data = data_train,
              start = data_train[, min(get(DateName))][[1]],
              frequency = SFreq)
  
  # TSClean Version
  if (TSClean) {
    if (MinVal > 0) {
      TargetMB <- forecast::tsclean(x = dataTSTrain1[, TargetName],
                                    replace.missing = TRUE,
                                    lambda = "auto")
    } else {
      TargetMB <- forecast::tsclean(x = dataTSTrain1[, TargetName],
                                    replace.missing = TRUE,
                                    lambda = NULL)
    }
  }
  
  # DSHW-------------
  if (!("DSHW" %in% toupper(SkipModels))) {
    # 1)
    if (PrintUpdates)
      message("DSHW FITTING")
    if (MinVal > 0) {
      # User-Supplied-Freq
      DSHW_Model <-
        tryCatch({
          forecast::dshw(
            y = dataTSTrain[, TargetName],
            period1 = freq,
            period2 = freq * 2,
            alpha = NULL,
            beta = NULL,
            gamma = NULL,
            omega = NULL,
            phi = NULL,
            lambda = "auto",
            biasadj = TRUE,
            armethod = TRUE,
            model = NULL
          )
        },
        error = function(x)
          "empty")
      
      # Model-Supplied-Freq
      if (ModelFreq) {
        DSHW_Model1 <-
          tryCatch({
            forecast::dshw(
              y = dataTSTrain1[, TargetName],
              period1 = SFreq,
              period2 = SFreq * 2,
              alpha = NULL,
              beta = NULL,
              gamma = NULL,
              omega = NULL,
              phi = NULL,
              lambda = "auto",
              biasadj = TRUE,
              armethod = TRUE,
              model = NULL
            )
          },
          error = function(x)
            "empty")
      }
      
      # Run for outlier removal and imputation
      if (TSClean) {
        # User-Supplied-Freq
        DSHW_Model2 <-
          tryCatch({
            forecast::dshw(
              y = Target,
              period1 = freq,
              period2 = freq * 2,
              alpha = NULL,
              beta = NULL,
              gamma = NULL,
              omega = NULL,
              phi = NULL,
              lambda = "auto",
              biasadj = TRUE,
              armethod = TRUE,
              model = NULL
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          DSHW_Model3 <-
            tryCatch({
              forecast::dshw(
                y = TargetMB,
                period1 = SFreq,
                period2 = SFreq * 2,
                alpha = NULL,
                beta = NULL,
                gamma = NULL,
                omega = NULL,
                phi = NULL,
                lambda = "auto",
                biasadj = TRUE,
                armethod = TRUE,
                model = NULL
              )
            },
            error = function(x)
              "empty")
        }
      }
    } else {
      # User-Supplied-Freq
      DSHW_Model <-
        tryCatch({
          forecast::dshw(
            y = dataTSTrain[, TargetName],
            period1 = freq,
            period2 = freq * 2,
            alpha = NULL,
            beta = NULL,
            gamma = NULL,
            omega = NULL,
            phi = NULL,
            lambda = NULL,
            biasadj = FALSE,
            armethod = TRUE,
            model = NULL
          )
        },
        error = function(x)
          "empty")
      
      # Model-Supplied-Freq
      if (ModelFreq) {
        DSHW_Model1 <-
          tryCatch({
            forecast::dshw(
              y = dataTSTrain1[, TargetName],
              period1 = SFreq,
              period2 = SFreq * 2,
              alpha = NULL,
              beta = NULL,
              gamma = NULL,
              omega = NULL,
              phi = NULL,
              lambda = NULL,
              biasadj = FALSE,
              armethod = TRUE,
              model = NULL
            )
          },
          error = function(x)
            "empty")
      }
      
      # tsclean: impute and replace outliers
      if (TSClean) {
        # User-Supplied-Freq
        DSHW_Model2 <-
          tryCatch({
            forecast::dshw(
              y = Target,
              period1 = freq,
              period2 = freq * 2,
              alpha = NULL,
              beta = NULL,
              gamma = NULL,
              omega = NULL,
              phi = NULL,
              lambda = NULL,
              biasadj = FALSE,
              armethod = TRUE,
              model = NULL
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          DSHW_Model3 <-
            tryCatch({
              forecast::dshw(
                y = TargetMB,
                period1 = SFreq,
                period2 = SFreq * 2,
                alpha = NULL,
                beta = NULL,
                gamma = NULL,
                omega = NULL,
                phi = NULL,
                lambda = NULL,
                biasadj = FALSE,
                armethod = TRUE,
                model = NULL
              )
            },
            error = function(x)
              "empty")
        }
      }
    }
    
    # Collect Test Data for Model Comparison
    # 2: User-Supplied-Freq
    if (tolower(class(DSHW_Model)) == "forecast") {
      tryCatch({
        data_test_DSHW <- data.table::copy(data_test)
        data_test_DSHW[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("DSHW", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(DSHW_Model, h = HoldOutPeriods)$mean
          )
        )]
        
        # Add Evaluation Columns
        # 3)
        data_test_DSHW[, ':=' (
          Resid = Target - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_DSHW
      }, error = function(x)
        "skip")
    }
    
    # 2: Model-Supplied-Freq
    if (ModelFreq) {
      if (tolower(class(DSHW_Model1)) == "forecast") {
        tryCatch({
          data_test_DSHW1 <- data.table::copy(data_test)
          data_test_DSHW1[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("DSHW_ModelFreq", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(DSHW_Model1, h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_DSHW1[, ':=' (
            Resid = Target - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_DSHW1
        }, error = function(x)
          "skip")
      }
    }
    
    # If TSClean is TRUE
    if (TSClean) {
      # 2: Model-Supplied-Freq
      if (tolower(class(DSHW_Model2)) == "forecast") {
        tryCatch({
          data_test_DSHW2 <- data.table::copy(data_test)
          data_test_DSHW2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("DSHW_TSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(DSHW_Model2, h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_DSHW2[, ':=' (
            Resid = Target - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_DSHW2
        }, error = function(x)
          "skip")
      }
      
      # 2: Model-Supplied-Freq
      if (ModelFreq) {
        if (tolower(class(DSHW_Model3)) == "forecast") {
          tryCatch({
            data_test_DSHW3 <- data.table::copy(data_test)
            data_test_DSHW3[, ':=' (
              Target = as.numeric(Target),
              ModelName = rep("DSHW_ModelFreqTSC", HoldOutPeriods),
              FC_Eval = as.numeric(
                forecast::forecast(DSHW_Model3, h = HoldOutPeriods)$mean
              )
            )]
            
            # Add Evaluation Columns
            # 3)
            data_test_DSHW3[, ':=' (
              Resid = Target - FC_Eval,
              PercentError = get(TargetName) / (FC_Eval +
                                                  1) - 1,
              AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                              1) - 1),
              AbsoluteError = abs(get(TargetName) - FC_Eval),
              SquaredError = (get(TargetName) - FC_Eval) ^ 2
            )]
            
            # Increment
            i <- i + 1
            
            # Collect model filename
            EvalList[[i]] <- data_test_DSHW3
          }, error = function(x)
            "skip")
        }
      }
    }
  }
  
  # ARFIMA Modeling----
  if (!("ARFIMA" %in% toupper(SkipModels))) {
    # ARFIMA-------------
    # 1)
    if (PrintUpdates)
      message("ARFIMA FITTING")
    if (StepWise) {
      if (MinVal > 0) {
        # User-Supplied-Freq
        ARFIMA_model <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain[, TargetName],
              lambda = TRUE,
              biasadj = TRUE,
              max.p = Lags,
              max.q = Lags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              stepwise = StepWise,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          ARFIMA_model1 <-
            tryCatch({
              forecast::arfima(
                y = dataTSTrain1[, TargetName],
                lambda = TRUE,
                biasadj = TRUE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARFIMA_model2 <-
            tryCatch({
              forecast::arfima(
                y = Target,
                lambda = TRUE,
                biasadj = TRUE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
          
          # Model-Supplied-Freq
          if (ModelFreq) {
            ARFIMA_model3 <-
              tryCatch({
                forecast::arfima(
                  y = TargetMB,
                  lambda = TRUE,
                  biasadj = TRUE,
                  max.p = Lags,
                  max.q = Lags,
                  max.d = 1,
                  max.D = 1,
                  ic = "bic",
                  stepwise = StepWise,
                  num.cores = NumCores
                )
              },
              error = function(x)
                "empty")
          }
        }
      } else {
        # User-Supplied-Freq
        ARFIMA_model <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain[, TargetName],
              lambda = FALSE,
              biasadj = FALSE,
              max.p = Lags,
              max.q = Lags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              stepwise = StepWise,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          ARFIMA_model1 <-
            tryCatch({
              forecast::arfima(
                y = dataTSTrain1[, TargetName],
                lambda = FALSE,
                biasadj = FALSE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARFIMA_model2 <-
            tryCatch({
              forecast::arfima(
                y = Target,
                lambda = FALSE,
                biasadj = FALSE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
          
          # Model-Supplied-Freq
          if (ModelFreq) {
            ARFIMA_model3 <-
              tryCatch({
                forecast::arfima(
                  y = TargetMB,
                  lambda = FALSE,
                  biasadj = FALSE,
                  max.p = Lags,
                  max.q = Lags,
                  max.d = 1,
                  max.D = 1,
                  ic = "bic",
                  stepwise = StepWise,
                  num.cores = NumCores
                )
              },
              error = function(x)
                "empty")
          }
        }
      }
    } else {
      if (MinVal > 0) {
        # User-Supplied-Freq
        ARFIMA_model <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain[, TargetName],
              lambda = TRUE,
              biasadj = TRUE,
              max.p = Lags,
              max.q = Lags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              stepwise = StepWise,
              parallel = TRUE,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          ARFIMA_model1 <-
            tryCatch({
              forecast::arfima(
                y = dataTSTrain1[, TargetName],
                lambda = TRUE,
                biasadj = TRUE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARFIMA_model2 <-
            tryCatch({
              forecast::arfima(
                y = Target,
                lambda = TRUE,
                biasadj = TRUE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
          
          # Model-Supplied-Freq
          if (ModelFreq) {
            ARFIMA_model3 <-
              tryCatch({
                forecast::arfima(
                  y = TargetMB,
                  lambda = TRUE,
                  biasadj = TRUE,
                  max.p = Lags,
                  max.q = Lags,
                  max.d = 1,
                  max.D = 1,
                  ic = "bic",
                  stepwise = StepWise,
                  parallel = TRUE,
                  num.cores = NumCores
                )
              },
              error = function(x)
                "empty")
          }
        }
      } else {
        # User-Supplied-Freq
        ARFIMA_model <-
          tryCatch({
            forecast::arfima(
              y = dataTSTrain[, TargetName],
              lambda = FALSE,
              biasadj = FALSE,
              max.p = Lags,
              max.q = Lags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              stepwise = StepWise,
              parallel = TRUE,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          ARFIMA_model1 <-
            tryCatch({
              forecast::arfima(
                y = dataTSTrain1[, TargetName],
                lambda = FALSE,
                biasadj = FALSE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARFIMA_model2 <-
            tryCatch({
              forecast::arfima(
                y = Target,
                lambda = FALSE,
                biasadj = FALSE,
                max.p = Lags,
                max.q = Lags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
          
          # Model-Supplied-Freq
          if (ModelFreq) {
            ARFIMA_model3 <-
              tryCatch({
                forecast::arfima(
                  y = TargetMB,
                  lambda = FALSE,
                  biasadj = FALSE,
                  max.p = Lags,
                  max.q = Lags,
                  max.d = 1,
                  max.D = 1,
                  ic = "bic",
                  stepwise = StepWise,
                  parallel = TRUE,
                  num.cores = NumCores
                )
              },
              error = function(x)
                "empty")
          }
        }
      }
    }
    
    
    # Collect Test Data for Model Comparison
    # 2: User-Supplied-Freq
    if (tolower(class(ARFIMA_model)) == "fracdiff") {
      tryCatch({
        data_test_ARF <- data.table::copy(data_test)
        data_test_ARF[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("ARFIMA", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(ARFIMA_model, h = HoldOutPeriods)$mean
          )
        )]
        
        # Add Evaluation Columns
        # 3)
        data_test_ARF[, ':=' (
          Resid = Target - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_ARF
      }, error = function(x)
        "skip")
    }
    
    # 2: Model-Supplied-Freq
    if (ModelFreq) {
      if (tolower(class(ARFIMA_model1)) == "fracdiff") {
        tryCatch({
          data_test_ARF1 <- data.table::copy(data_test)
          data_test_ARF1[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("ARFIMA_ModelFreq", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(ARFIMA_model1, h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_ARF1[, ':=' (
            Resid = Target - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_ARF1
        }, error = function(x)
          "skip")
      }
    }
    
    # TSClean Version
    if (TSClean) {
      # 2: User-Supplied-Freq
      if (tolower(class(ARFIMA_model2)) == "fracdiff") {
        tryCatch({
          data_test_ARF2 <- data.table::copy(data_test)
          data_test_ARF2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("ARFIMA_TSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(ARFIMA_model2, h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_ARF2[, ':=' (
            Resid = Target - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_ARF2
        }, error = function(x)
          "skip")
      }
      
      # 2: Model-Supplied-Freq
      if (ModelFreq) {
        if (tolower(class(ARFIMA_model3)) == "fracdiff") {
          tryCatch({
            data_test_ARF3 <- data.table::copy(data_test)
            data_test_ARF3[, ':=' (
              Target = as.numeric(Target),
              ModelName = rep("ARFIMA_ModelFreqTSC", HoldOutPeriods),
              FC_Eval = as.numeric(
                forecast::forecast(ARFIMA_model3, h = HoldOutPeriods)$mean
              )
            )]
            
            # Add Evaluation Columns
            # 3)
            data_test_ARF3[, ':=' (
              Resid = Target - FC_Eval,
              PercentError = get(TargetName) / (FC_Eval +
                                                  1) - 1,
              AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                              1) - 1),
              AbsoluteError = abs(get(TargetName) - FC_Eval),
              SquaredError = (get(TargetName) - FC_Eval) ^ 2
            )]
            
            # Increment
            i <- i + 1
            
            # Collect model filename
            EvalList[[i]] <- data_test_ARF3
          }, error = function(x)
            "skip")
        }
      }
    }
  }
  
  # Arima----
  if (!("ARIMA" %in% toupper(SkipModels))) {
    # ARIMA-------------
    # 1)
    if (PrintUpdates)
      message("ARIMA FITTING")
    if (StepWise) {
      if (MinVal > 0) {
        # User-Supplied-Freq
        ARIMA_model <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain[, TargetName],
              max.p = Lags,
              max.q = Lags,
              max.P = SLags,
              max.Q = SLags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              lambda = TRUE,
              biasadj = TRUE,
              stepwise = StepWise,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          ARIMA_model1 <-
            tryCatch({
              forecast::auto.arima(
                y = dataTSTrain1[, TargetName],
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                lambda = TRUE,
                biasadj = TRUE,
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
        
        # TSClean Verison
        if (TSClean) {
          # User-Supplied-Freq
          ARIMA_model2 <-
            tryCatch({
              forecast::auto.arima(
                y = Target,
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                lambda = TRUE,
                biasadj = TRUE,
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
          
          # Model-Supplied-Freq
          if (ModelFreq) {
            ARIMA_model3 <-
              tryCatch({
                forecast::auto.arima(
                  y = TargetMB,
                  max.p = Lags,
                  max.q = Lags,
                  max.P = SLags,
                  max.Q = SLags,
                  max.d = 1,
                  max.D = 1,
                  ic = "bic",
                  lambda = TRUE,
                  biasadj = TRUE,
                  stepwise = StepWise,
                  num.cores = NumCores
                )
              },
              error = function(x)
                "empty")
          }
        }
      } else {
        # User-Supplied-Freq
        ARIMA_model <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain[, TargetName],
              max.p = Lags,
              max.q = Lags,
              max.P = SLags,
              max.Q = SLags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              lambda = FALSE,
              biasadj = FALSE,
              stepwise = StepWise,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          ARIMA_model1 <-
            tryCatch({
              forecast::auto.arima(
                y = dataTSTrain1[, TargetName],
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                lambda = FALSE,
                biasadj = FALSE,
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARIMA_model2 <-
            tryCatch({
              forecast::auto.arima(
                y = Target,
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                lambda = FALSE,
                biasadj = FALSE,
                stepwise = StepWise,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
          
          # Model-Supplied-Freq
          if (ModelFreq) {
            ARIMA_model3 <-
              tryCatch({
                forecast::auto.arima(
                  y = TargetMB,
                  max.p = Lags,
                  max.q = Lags,
                  max.P = SLags,
                  max.Q = SLags,
                  max.d = 1,
                  max.D = 1,
                  ic = "bic",
                  lambda = FALSE,
                  biasadj = FALSE,
                  stepwise = StepWise,
                  num.cores = NumCores
                )
              },
              error = function(x)
                "empty")
          }
        }
      }
    } else {
      if (MinVal > 0) {
        # User-Supplied-Freq
        ARIMA_model <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain[, TargetName],
              max.p = Lags,
              max.q = Lags,
              max.P = SLags,
              max.Q = SLags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              lambda = TRUE,
              biasadj = TRUE,
              stepwise = StepWise,
              parallel = TRUE,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          ARIMA_model1 <-
            tryCatch({
              forecast::auto.arima(
                y = dataTSTrain1[, TargetName],
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                lambda = TRUE,
                biasadj = TRUE,
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARIMA_model2 <-
            tryCatch({
              forecast::auto.arima(
                y = Target,
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                lambda = TRUE,
                biasadj = TRUE,
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
          
          # Model-Supplied-Freq
          if (ModelFreq) {
            ARIMA_model3 <-
              tryCatch({
                forecast::auto.arima(
                  y = TargetMB,
                  max.p = Lags,
                  max.q = Lags,
                  max.P = SLags,
                  max.Q = SLags,
                  max.d = 1,
                  max.D = 1,
                  ic = "bic",
                  lambda = TRUE,
                  biasadj = TRUE,
                  stepwise = StepWise,
                  parallel = TRUE,
                  num.cores = NumCores
                )
              },
              error = function(x)
                "empty")
          }
        }
      } else {
        # User-Supplied-Freq
        ARIMA_model <-
          tryCatch({
            forecast::auto.arima(
              y = dataTSTrain[, TargetName],
              max.p = Lags,
              max.q = Lags,
              max.P = SLags,
              max.Q = SLags,
              max.d = 1,
              max.D = 1,
              ic = "bic",
              lambda = FALSE,
              biasadj = FALSE,
              stepwise = StepWise,
              parallel = TRUE,
              num.cores = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          ARIMA_model1 <-
            tryCatch({
              forecast::auto.arima(
                y = dataTSTrain1[, TargetName],
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                lambda = FALSE,
                biasadj = FALSE,
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
        }
        
        # TSClean Version
        if (TSClean) {
          # User-Supplied-Freq
          ARIMA_model2 <-
            tryCatch({
              forecast::auto.arima(
                y = Target,
                max.p = Lags,
                max.q = Lags,
                max.P = SLags,
                max.Q = SLags,
                max.d = 1,
                max.D = 1,
                ic = "bic",
                lambda = FALSE,
                biasadj = FALSE,
                stepwise = StepWise,
                parallel = TRUE,
                num.cores = NumCores
              )
            },
            error = function(x)
              "empty")
          
          # Model-Supplied-Freq
          if (ModelFreq) {
            ARIMA_model3 <-
              tryCatch({
                forecast::auto.arima(
                  y = TargetMB,
                  max.p = Lags,
                  max.q = Lags,
                  max.P = SLags,
                  max.Q = SLags,
                  max.d = 1,
                  max.D = 1,
                  ic = "bic",
                  lambda = FALSE,
                  biasadj = FALSE,
                  stepwise = StepWise,
                  parallel = TRUE,
                  num.cores = NumCores
                )
              },
              error = function(x)
                "empty")
          }
        }
      }
    }
    
    # Collect Test Data for Model Comparison
    # 2: User-Supplied-Freq
    if (tolower(class(ARIMA_model)[1]) == "arima") {
      tryCatch({
        data_test_ARI <- data.table::copy(data_test)
        data_test_ARI[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("ARIMA", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(ARIMA_model, h = HoldOutPeriods)$mean
          )
        )]
        
        # Add Evaluation Columns
        # 3)
        data_test_ARI[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_ARI
      }, error = function(x)
        "skip")
    }
    
    # Model-Supplied-Freq
    if (ModelFreq) {
      if (tolower(class(ARIMA_model1)[1]) == "arima") {
        tryCatch({
          data_test_ARI1 <- data.table::copy(data_test)
          data_test_ARI1[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("ARIMA_ModelFreq", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(ARIMA_model1, h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_ARI1[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_ARI1
        }, error = function(x)
          "skip")
      }
    }
    
    # TSClean Version
    if (TSClean) {
      # 2: User-Supplied-Freq
      if (tolower(class(ARIMA_model2)[1]) == "arima") {
        tryCatch({
          data_test_ARI2 <- data.table::copy(data_test)
          data_test_ARI2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("ARIMA_TSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(ARIMA_model2, h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_ARI2[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_ARI2
        }, error = function(x)
          "skip")
      }
      
      # Model-Supplied-Freq
      if (ModelFreq) {
        if (tolower(class(ARIMA_model3)[1]) == "arima") {
          tryCatch({
            data_test_ARI3 <- data.table::copy(data_test)
            data_test_ARI3[, ':=' (
              Target = as.numeric(Target),
              ModelName = rep("ARIMA_ModelFreqTSC", HoldOutPeriods),
              FC_Eval = as.numeric(
                forecast::forecast(ARIMA_model3, h = HoldOutPeriods)$mean
              )
            )]
            
            # Add Evaluation Columns
            # 3)
            data_test_ARI3[, ':=' (
              Resid = get(TargetName) - FC_Eval,
              PercentError = get(TargetName) / (FC_Eval +
                                                  1) - 1,
              AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                              1) - 1),
              AbsoluteError = abs(get(TargetName) - FC_Eval),
              SquaredError = (get(TargetName) - FC_Eval) ^ 2
            )]
            
            # Increment
            i <- i + 1
            
            # Collect model filename
            EvalList[[i]] <- data_test_ARI3
          }, error = function(x)
            "skip")
        }
      }
    }
  }
  
  # ETS----
  if (!("ETS" %in% toupper(SkipModels))) {
    # EXPONENTIAL SMOOTHING-------------
    # 1)
    if (PrintUpdates)
      message("ETS FITTING")
    
    # User-Supplied-Freq
    if (freq > 24) {
      if (MinVal > 0) {
        # when > 24, model's third letter has to be N for none
        EXPSMOOTH_model <-
          tryCatch({
            forecast::ets(
              y = dataTSTrain[, TargetName],
              model = "ZZN",
              allow.multiplicative.trend = TRUE,
              restrict = TRUE,
              lambda = TRUE,
              biasadj = TRUE
            )
          }, error = function(x)
            "empty")
        
        # TSClean Version
        if (TSClean) {
          EXPSMOOTH_model2 <-
            tryCatch({
              forecast::ets(
                y = Target,
                model = "ZZN",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = TRUE,
                biasadj = TRUE
              )
            }, error = function(x)
              "empty")
        }
      } else {
        # when > 24, model's third letter has to be N for none
        EXPSMOOTH_model <-
          tryCatch({
            forecast::ets(
              y = dataTSTrain[, TargetName],
              model = "ZZN",
              allow.multiplicative.trend = TRUE,
              restrict = TRUE,
              lambda = FALSE,
              biasadj = FALSE
            )
          }, error = function(x)
            "empty")
        
        # TSClean Version
        if (TSClean) {
          EXPSMOOTH_model2 <-
            tryCatch({
              forecast::ets(
                y = Target,
                model = "ZZN",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = FALSE,
                biasadj = FALSE
              )
            }, error = function(x)
              "empty")
        }
      }
    } else {
      if (MinVal > 0) {
        EXPSMOOTH_model <-
          tryCatch({
            forecast::ets(
              y = dataTSTrain[, TargetName],
              model = "ZZZ",
              allow.multiplicative.trend = TRUE,
              restrict = TRUE,
              lambda = TRUE,
              biasadj = TRUE
            )
          },
          error = function(x)
            "empty")
        
        # TSClean Version
        if (TSClean) {
          EXPSMOOTH_model2 <-
            tryCatch({
              forecast::ets(
                y = Target,
                model = "ZZZ",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = TRUE,
                biasadj = TRUE
              )
            },
            error = function(x)
              "empty")
        }
      } else {
        EXPSMOOTH_model <-
          tryCatch({
            forecast::ets(
              y = dataTSTrain[, TargetName],
              model = "ZZZ",
              allow.multiplicative.trend = TRUE,
              restrict = TRUE,
              lambda = FALSE,
              biasadj = FALSE
            )
          },
          error = function(x)
            "empty")
        
        # TSClean Version
        if (TSClean) {
          EXPSMOOTH_model2 <-
            tryCatch({
              forecast::ets(
                y = Target,
                model = "ZZZ",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = FALSE,
                biasadj = FALSE
              )
            },
            error = function(x)
              "empty")
        }
      }
    }
    
    # Model-Supplied-Freq
    if (ModelFreq) {
      if (SFreq > 24) {
        if (MinVal > 0) {
          # when > 24, model's third letter has to be N for none
          EXPSMOOTH_model1 <-
            tryCatch({
              forecast::ets(
                y = dataTSTrain1[, TargetName],
                model = "ZZN",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = TRUE,
                biasadj = TRUE
              )
            }, error = function(x)
              "empty")
          
          # TSClean Version
          if (TSClean) {
            EXPSMOOTH_model3 <-
              tryCatch({
                forecast::ets(
                  y = TargetMB,
                  model = "ZZN",
                  allow.multiplicative.trend = TRUE,
                  restrict = TRUE,
                  lambda = TRUE,
                  biasadj = TRUE
                )
              }, error = function(x)
                "empty")
          }
        } else {
          # when > 24, model's third letter has to be N for none
          EXPSMOOTH_model1 <-
            tryCatch({
              forecast::ets(
                y = dataTSTrain1[, TargetName],
                model = "ZZN",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = FALSE,
                biasadj = FALSE
              )
            }, error = function(x)
              "empty")
          
          # TSClean Version
          if (TSClean) {
            EXPSMOOTH_model3 <-
              tryCatch({
                forecast::ets(
                  y = TargetMB,
                  model = "ZZN",
                  allow.multiplicative.trend = TRUE,
                  restrict = TRUE,
                  lambda = FALSE,
                  biasadj = FALSE
                )
              }, error = function(x)
                "empty")
          }
        }
      } else {
        if (MinVal > 0) {
          EXPSMOOTH_model1 <-
            tryCatch({
              forecast::ets(
                y = dataTSTrain1[, TargetName],
                model = "ZZZ",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = TRUE,
                biasadj = TRUE
              )
            },
            error = function(x)
              "empty")
          
          # TSClean Version
          if (TSClean) {
            EXPSMOOTH_model3 <-
              tryCatch({
                forecast::ets(
                  y = TargetMB,
                  model = "ZZZ",
                  allow.multiplicative.trend = TRUE,
                  restrict = TRUE,
                  lambda = TRUE,
                  biasadj = TRUE
                )
              },
              error = function(x)
                "empty")
          }
        } else {
          EXPSMOOTH_model1 <-
            tryCatch({
              forecast::ets(
                y = dataTSTrain1[, TargetName],
                model = "ZZZ",
                allow.multiplicative.trend = TRUE,
                restrict = TRUE,
                lambda = FALSE,
                biasadj = FALSE
              )
            },
            error = function(x)
              "empty")
          
          # TSClean Version
          if (TSClean) {
            EXPSMOOTH_model3 <-
              tryCatch({
                forecast::ets(
                  y = TargetMB,
                  model = "ZZZ",
                  allow.multiplicative.trend = TRUE,
                  restrict = TRUE,
                  lambda = FALSE,
                  biasadj = FALSE
                )
              },
              error = function(x)
                "empty")
          }
        }
      }
    }
    
    # Collect Test Data for Model Comparison
    # 2: User-Supplied-Freq
    if (tolower(class(EXPSMOOTH_model)) == "ets") {
      tryCatch({
        data_test_ETS <- data.table::copy(data_test)
        data_test_ETS[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("ETS", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(EXPSMOOTH_model, h = HoldOutPeriods)$mean
          )
        )]
        
        # Add Evaluation Columns
        # 3)
        data_test_ETS[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_ETS
      }, error = function(x)
        "skip")
    }
    
    # 2: Model-Based-Freq
    if (ModelFreq) {
      if (tolower(class(EXPSMOOTH_model1)) == "ets") {
        tryCatch({
          data_test_ETS1 <- data.table::copy(data_test)
          data_test_ETS1[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("ETS_ModelFreq", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(EXPSMOOTH_model1, h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_ETS1[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_ETS1
        }, error = function(x)
          "skip")
      }
    }
    
    # TSClean Version
    if (TSClean) {
      # 2: User-Supplied-Freq
      if (tolower(class(EXPSMOOTH_model2)) == "ets") {
        tryCatch({
          data_test_ETS2 <- data.table::copy(data_test)
          data_test_ETS2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("ETS", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(EXPSMOOTH_model2, h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_ETS2[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_ETS2
        }, error = function(x)
          "skip")
      }
      
      # 2: Model-Based-Freq
      if (ModelFreq) {
        if (tolower(class(EXPSMOOTH_model3)) == "ets") {
          tryCatch({
            data_test_ETS3 <- data.table::copy(data_test)
            data_test_ETS3[, ':=' (
              Target = as.numeric(Target),
              ModelName = rep("ETS_ModelFreqTSC", HoldOutPeriods),
              FC_Eval = as.numeric(
                forecast::forecast(EXPSMOOTH_model3, h = HoldOutPeriods)$mean
              )
            )]
            
            # Add Evaluation Columns
            # 3)
            data_test_ETS3[, ':=' (
              Resid = get(TargetName) - FC_Eval,
              PercentError = get(TargetName) / (FC_Eval +
                                                  1) - 1,
              AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                              1) - 1),
              AbsoluteError = abs(get(TargetName) - FC_Eval),
              SquaredError = (get(TargetName) - FC_Eval) ^ 2
            )]
            
            # Increment
            i <- i + 1
            
            # Collect model filename
            EvalList[[i]] <- data_test_ETS3
          }, error = function(x)
            "skip")
        }
      }
    }
  }
  
  # TBATS----
  if (!("TBATS" %in% toupper(SkipModels))) {
    # TBATS-------------
    # 1)
    if (PrintUpdates)
      message("TBATS FITTING")
    if (MinVal > 0) {
      # User-Supplied-Freq
      TBATS_model <-
        tryCatch({
          forecast::tbats(
            y               = dataTSTrain[, TargetName],
            use.arma.errors = TRUE,
            lambda          = TRUE,
            biasadj         = TRUE,
            max.p           = Lags,
            max.q           = Lags,
            max.P           = SLags,
            max.Q           = SLags,
            max.d           = 1,
            max.D           = 1,
            num.cores       = NumCores
          )
        },
        error = function(x)
          "empty")
      
      # Model-Supplied-Freq
      if (ModelFreq) {
        TBATS_model1 <-
          tryCatch({
            forecast::tbats(
              y               = dataTSTrain1[, TargetName],
              use.arma.errors = TRUE,
              lambda          = TRUE,
              biasadj         = TRUE,
              max.p           = Lags,
              max.q           = Lags,
              max.P           = SLags,
              max.Q           = SLags,
              max.d           = 1,
              max.D           = 1,
              num.cores       = NumCores
            )
          },
          error = function(x)
            "empty")
      }
      
      # TSClean Version
      if (TSClean) {
        # User-Supplied-Freq
        TBATS_model2 <-
          tryCatch({
            forecast::tbats(
              y               = Target,
              use.arma.errors = TRUE,
              lambda          = TRUE,
              biasadj         = TRUE,
              max.p           = Lags,
              max.q           = Lags,
              max.P           = SLags,
              max.Q           = SLags,
              max.d           = 1,
              max.D           = 1,
              num.cores       = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          TBATS_model3 <-
            tryCatch({
              forecast::tbats(
                y               = TargetMB,
                use.arma.errors = TRUE,
                lambda          = TRUE,
                biasadj         = TRUE,
                max.p           = Lags,
                max.q           = Lags,
                max.P           = SLags,
                max.Q           = SLags,
                max.d           = 1,
                max.D           = 1,
                num.cores       = NumCores
              )
            },
            error = function(x)
              "empty")
        }
      }
    } else {
      # User-Supplied-Freq
      TBATS_model <-
        tryCatch({
          forecast::tbats(
            y               = dataTSTrain[, TargetName],
            use.arma.errors = TRUE,
            lambda          = FALSE,
            biasadj         = FALSE,
            max.p           = Lags,
            max.q           = Lags,
            max.P           = SLags,
            max.Q           = SLags,
            max.d           = 1,
            max.D           = 1,
            num.cores       = NumCores
          )
        },
        error = function(x)
          "empty")
      
      # Model-Supplied-Freq
      if (ModelFreq) {
        TBATS_model1 <-
          tryCatch({
            forecast::tbats(
              y               = dataTSTrain1[, TargetName],
              use.arma.errors = TRUE,
              lambda          = FALSE,
              biasadj         = FALSE,
              max.p           = Lags,
              max.q           = Lags,
              max.P           = SLags,
              max.Q           = SLags,
              max.d           = 1,
              max.D           = 1,
              num.cores       = NumCores
            )
          },
          error = function(x)
            "empty")
      }
      
      # TSClean Version
      if (TSClean) {
        # User-Supplied-Freq
        TBATS_model2 <-
          tryCatch({
            forecast::tbats(
              y               = Target,
              use.arma.errors = TRUE,
              lambda          = FALSE,
              biasadj         = FALSE,
              max.p           = Lags,
              max.q           = Lags,
              max.P           = SLags,
              max.Q           = SLags,
              max.d           = 1,
              max.D           = 1,
              num.cores       = NumCores
            )
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          TBATS_model3 <-
            tryCatch({
              forecast::tbats(
                y               = TargetMB,
                use.arma.errors = TRUE,
                lambda          = FALSE,
                biasadj         = FALSE,
                max.p           = Lags,
                max.q           = Lags,
                max.P           = SLags,
                max.Q           = SLags,
                max.d           = 1,
                max.D           = 1,
                num.cores       = NumCores
              )
            },
            error = function(x)
              "empty")
        }
      }
    }
    
    # User-Supplied-Freq
    if (class(TBATS_model)[1] == "tbats" |
        class(TBATS_model)[1] == "bats") {
      tryCatch({
        # Collect Test Data for Model Comparison
        # 2)
        data_test_TBATS <- data.table::copy(data_test)
        data_test_TBATS[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("TBATS", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(TBATS_model, h = HoldOutPeriods)$mean
          )
        )]
        
        # Add Evaluation Columns
        # 3)
        data_test_TBATS[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_TBATS
      }, error = function(x)
        "skip")
    }
    
    # Model-Supplied-Freq
    if (ModelFreq) {
      if (class(TBATS_model1)[1] == "tbats" |
          class(TBATS_model1)[1] == "bats") {
        tryCatch({
          # Collect Test Data for Model Comparison
          # 2)
          data_test_TBATS1 <- data.table::copy(data_test)
          data_test_TBATS1[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("TBATS_ModelFreq", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(TBATS_model1, h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_TBATS1[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_TBATS1
        }, error = function(x)
          "skip")
      }
    }
    
    # TSClean Version
    if (TSClean) {
      # User-Supplied-Freq
      if (class(TBATS_model2)[1] == "tbats" |
          class(TBATS_model2)[1] == "bats") {
        tryCatch({
          # Collect Test Data for Model Comparison
          # 2)
          data_test_TBATS2 <- data.table::copy(data_test)
          data_test_TBATS2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("TBATS_TSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(TBATS_model2, h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_TBATS2[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_TBATS2
        }, error = function(x)
          "skip")
      }
      
      # Model-Supplied-Freq
      if (ModelFreq) {
        if (class(TBATS_model3)[1] == "tbats" |
            class(TBATS_model3)[1] == "bats") {
          tryCatch({
            # Collect Test Data for Model Comparison
            # 2)
            data_test_TBATS3 <- data.table::copy(data_test)
            data_test_TBATS3[, ':=' (
              Target = as.numeric(Target),
              ModelName = rep("TBATS_ModelFreqTSC", HoldOutPeriods),
              FC_Eval = as.numeric(
                forecast::forecast(TBATS_model3, h = HoldOutPeriods)$mean
              )
            )]
            
            # Add Evaluation Columns
            # 3)
            data_test_TBATS3[, ':=' (
              Resid = get(TargetName) - FC_Eval,
              PercentError = get(TargetName) / (FC_Eval +
                                                  1) - 1,
              AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                              1) - 1),
              AbsoluteError = abs(get(TargetName) - FC_Eval),
              SquaredError = (get(TargetName) - FC_Eval) ^ 2
            )]
            
            # Increment
            i <- i + 1
            
            # Collect model filename
            EvalList[[i]] <- data_test_TBATS3
          }, error = function(x)
            "skip")
        }
      }
    }
  }
  
  # TSLM----
  if (!("TSLM" %in% toupper(SkipModels))) {
    # LINEAR MODEL WITH TIME SERIES COMPONENTS-------------
    # 1)
    if (PrintUpdates)
      message("TSLM FITTING")
    if (MinVal > 0) {
      # User-Supplied-Freq
      TSLM_model <-
        tryCatch({
          forecast::tslm(dataTSTrain[, TargetName] ~ trend + season,
                         lambda = TRUE,
                         biasadj = TRUE)
        },
        error = function(x)
          "empty")
      
      # Model-Supplied-Freq
      if (ModelFreq) {
        TSLM_model1 <-
          tryCatch({
            forecast::tslm(dataTSTrain1[, TargetName] ~ trend + season,
                           lambda = TRUE,
                           biasadj = TRUE)
          },
          error = function(x)
            "empty")
      }
      
      # TSClean Version
      if (TSClean) {
        # User-Supplied-Freq
        TSLM_model2 <-
          tryCatch({
            forecast::tslm(Target ~ trend + season,
                           lambda = TRUE,
                           biasadj = TRUE)
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          TSLM_model3 <-
            tryCatch({
              forecast::tslm(TargetMB ~ trend + season,
                             lambda = TRUE,
                             biasadj = TRUE)
            },
            error = function(x)
              "empty")
        }
      }
    } else {
      # User-Supplied-Freq
      TSLM_model <-
        tryCatch({
          forecast::tslm(dataTSTrain[, TargetName] ~ trend + season,
                         lambda = FALSE,
                         biasadj = FALSE)
        },
        error = function(x)
          "empty")
      
      # Model-Supplied-Freq
      if (ModelFreq) {
        TSLM_model1 <-
          tryCatch({
            forecast::tslm(dataTSTrain1[, TargetName] ~ trend + season,
                           lambda = TRUE,
                           biasadj = TRUE)
          },
          error = function(x)
            "empty")
      }
      
      # TSClean Version
      if (TSClean) {
        TSLM_model2 <-
          tryCatch({
            forecast::tslm(Target ~ trend + season,
                           lambda = FALSE,
                           biasadj = FALSE)
          },
          error = function(x)
            "empty")
        
        # Model-Supplied-Freq
        if (ModelFreq) {
          TSLM_model3 <-
            tryCatch({
              forecast::tslm(TargetMB ~ trend + season,
                             lambda = TRUE,
                             biasadj = TRUE)
            },
            error = function(x)
              "empty")
        }
      }
    }
    
    # User-Supplied-Freq
    if (tolower(class(TSLM_model)[1]) == "tslm") {
      tryCatch({
        # Collect Test Data for Model Comparison
        # 2)
        data_test_TSLM <- data.table::copy(data_test)
        data_test_TSLM[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("TSLM", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(TSLM_model,
                               h = HoldOutPeriods)$mean
          )
        )]
        
        # Add Evaluation Columns
        # 3)
        data_test_TSLM[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval +
                                              1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_TSLM
      }, error = function(x)
        "skip")
    }
    
    # Model-Supplied-Freq
    if (ModelFreq) {
      if (tolower(class(TSLM_model1)[1]) == "tslm") {
        tryCatch({
          # Collect Test Data for Model Comparison
          # 2)
          data_test_TSLM1 <- data.table::copy(data_test)
          data_test_TSLM1[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("TSLM_ModelFreq", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(TSLM_model1,
                                 h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_TSLM1[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_TSLM1
        }, error = function(x)
          "skip")
      }
    }
    
    # TSClean Version
    if (TSClean) {
      # User-Supplied-Freq
      if (tolower(class(TSLM_model2)[1]) == "tslm") {
        tryCatch({
          # Collect Test Data for Model Comparison
          # 2)
          data_test_TSLM2 <- data.table::copy(data_test)
          data_test_TSLM2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("TSLM_TSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(TSLM_model2,
                                 h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_TSLM2[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval +
                                                1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_TSLM2
        }, error = function(x)
          "skip")
      }
      
      # Model-Supplied-Freq
      if (ModelFreq) {
        if (tolower(class(TSLM_model3)[1]) == "tslm") {
          tryCatch({
            # Collect Test Data for Model Comparison
            # 2)
            data_test_TSLM3 <- data.table::copy(data_test)
            data_test_TSLM3[, ':=' (
              Target = as.numeric(Target),
              ModelName = rep("TSLM_ModelFreqTSC", HoldOutPeriods),
              FC_Eval = as.numeric(
                forecast::forecast(TSLM_model3,
                                   h = HoldOutPeriods)$mean
              )
            )]
            
            # Add Evaluation Columns
            # 3)
            data_test_TSLM3[, ':=' (
              Resid = get(TargetName) - FC_Eval,
              PercentError = get(TargetName) / (FC_Eval +
                                                  1) - 1,
              AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                              1) - 1),
              AbsoluteError = abs(get(TargetName) - FC_Eval),
              SquaredError = (get(TargetName) - FC_Eval) ^ 2
            )]
            
            # Increment
            i <- i + 1
            
            # Collect model filename
            EvalList[[i]] <- data_test_TSLM3
          }, error = function(x)
            "skip")
        }
      }
    }
  }
  
  # NNET----
  if (!("NNET" %in% toupper(SkipModels))) {
    # Neural Network-------------
    # 1)
    if (PrintUpdates)
      message("NNet FITTING")
    k <- 0L
    temp <-
      data.table::data.table(
        Lag = rep(1L, Lags * SLags),
        Slag = rep(1L, Lags * SLags),
        meanResid = rnorm(Lags * SLags),
        sdResid = rnorm(Lags * SLags)
      )
    for (lags in seq_len(Lags)) {
      for (slags in seq_len(SLags)) {
        k <- k + 1L
        if (PrintUpdates)
          print(paste0("NNet Iteration: ", k))
        NNETAR_model_temp <-
          tryCatch({
            forecast::nnetar(
              y = dataTSTrain[, TargetName],
              p = lags,
              P = slags,
              lambda = "auto"
            )
          }, error = function(x)
            "error")
        
        if (length(NNETAR_model_temp) == 1) {
          data.table::set(temp,
                          i = k,
                          j = 1L,
                          value = lags)
          data.table::set(temp,
                          i = k,
                          j = 2L,
                          value = slags)
          data.table::set(temp,
                          i = k,
                          j = 3L,
                          value = 999999999)
          data.table::set(temp,
                          i = k,
                          j = 4L,
                          value = 999999999)
          
        } else {
          data.table::set(temp,
                          i = k,
                          j = 1L,
                          value = lags)
          data.table::set(temp,
                          i = k,
                          j = 2L,
                          value = slags)
          data.table::set(
            temp,
            i = k,
            j = 3L,
            value = base::mean(abs(NNETAR_model_temp$residuals),
                               na.rm = TRUE)
          )
          data.table::set(
            temp,
            i = k,
            j = 4L,
            value = sd(NNETAR_model_temp$residuals,
                       na.rm = TRUE)
          )
        }
      }
    }
    
    # Identify best model and retrain it
    LagNN <- temp[order(meanResid)][1, ][, 1][[1]]
    SLagNN <- temp[order(meanResid)][1, ][, 2][[1]]
    NNETAR_model <-
      tryCatch({
        forecast::nnetar(
          y = dataTSTrain[, TargetName],
          p = LagNN,
          P = SLagNN,
          lambda = "auto"
        )
      },
      error = function(x)
        "empty")
    
    # Collect Test Data for Model Comparison
    # 2)
    if (tolower(class(NNETAR_model)) == "nnetar") {
      tryCatch({
        data_test_NN <- data.table::copy(data_test)
        data_test_NN[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("NN", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(NNETAR_model, h = HoldOutPeriods)$mean
          )
        )]
        
        # Add Evaluation Columns
        # 3)
        data_test_NN[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval + 1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_NN
      }, error = function(x)
        "skip")
    }
    
    k <- 0L
    temp <-
      data.table::data.table(
        Lag = rep(1L, Lags * SLags),
        Slag = rep(1L, Lags * SLags),
        meanResid = rnorm(Lags * SLags),
        sdResid = rnorm(Lags * SLags)
      )
    for (lags in seq_len(Lags)) {
      for (slags in seq_len(SLags)) {
        k <- k + 1L
        if (PrintUpdates)
          print(paste0("NNet 2 Iteration: ", k))
        NNETAR_model_temp <-
          tryCatch({
            forecast::nnetar(
              y = dataTSTrain1[, TargetName],
              p = lags,
              P = slags,
              lambda = "auto"
            )
          }, error = function(x)
            "error")
        
        if (length(NNETAR_model_temp) == 1) {
          data.table::set(temp,
                          i = k,
                          j = 1L,
                          value = lags)
          data.table::set(temp,
                          i = k,
                          j = 2L,
                          value = slags)
          data.table::set(temp,
                          i = k,
                          j = 3L,
                          value = 999999999)
          data.table::set(temp,
                          i = k,
                          j = 4L,
                          value = 999999999)
          
        } else {
          data.table::set(temp,
                          i = k,
                          j = 1L,
                          value = lags)
          data.table::set(temp,
                          i = k,
                          j = 2L,
                          value = slags)
          data.table::set(
            temp,
            i = k,
            j = 3L,
            value = base::mean(abs(NNETAR_model_temp$residuals),
                               na.rm = TRUE)
          )
          data.table::set(
            temp,
            i = k,
            j = 4L,
            value = sd(NNETAR_model_temp$residuals,
                       na.rm = TRUE)
          )
        }
      }
    }
    
    # Identify best model and retrain it
    LagNN <- temp[order(meanResid)][1, ][, 1][[1]]
    SLagNN <- temp[order(meanResid)][1, ][, 2][[1]]
    NNETAR_model1 <-
      tryCatch({
        forecast::nnetar(
          y = dataTSTrain1[, TargetName],
          p = LagNN,
          P = SLagNN,
          lambda = "auto"
        )
      },
      error = function(x)
        "empty")
    
    # Collect Test Data for Model Comparison
    # 2)
    if (tolower(class(NNETAR_model1)) == "nnetar") {
      tryCatch({
        data_test_NN1 <- data.table::copy(data_test)
        data_test_NN1[, ':=' (
          Target = as.numeric(Target),
          ModelName = rep("NN_ModelFreq", HoldOutPeriods),
          FC_Eval = as.numeric(
            forecast::forecast(NNETAR_model1, h = HoldOutPeriods)$mean
          )
        )]
        
        # Add Evaluation Columns
        # 3)
        data_test_NN1[, ':=' (
          Resid = get(TargetName) - FC_Eval,
          PercentError = get(TargetName) / (FC_Eval + 1) - 1,
          AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                          1) - 1),
          AbsoluteError = abs(get(TargetName) - FC_Eval),
          SquaredError = (get(TargetName) - FC_Eval) ^ 2
        )]
        
        # Increment
        i <- i + 1
        
        # Collect model filename
        EvalList[[i]] <- data_test_NN1
      }, error = function(x)
        "skip")
    }
    
    # TSClean Version
    if (TSClean) {
      k <- 0L
      temp <-
        data.table::data.table(
          Lag = rep(1L, Lags * SLags),
          Slag = rep(1L, Lags * SLags),
          meanResid = rnorm(Lags * SLags),
          sdResid = rnorm(Lags * SLags)
        )
      for (lags in seq_len(Lags)) {
        for (slags in seq_len(SLags)) {
          k <- k + 1L
          if (PrintUpdates)
            print(paste0("NNet 3 Iteration: ", k))
          NNETAR_model_temp <-
            tryCatch({
              forecast::nnetar(
                y = Target,
                p = lags,
                P = slags,
                lambda = "auto"
              )
            }, error = function(x)
              "error")
          
          if (length(NNETAR_model_temp) == 1) {
            data.table::set(temp,
                            i = k,
                            j = 1L,
                            value = lags)
            data.table::set(temp,
                            i = k,
                            j = 2L,
                            value = slags)
            data.table::set(temp,
                            i = k,
                            j = 3L,
                            value = 999999999)
            data.table::set(temp,
                            i = k,
                            j = 4L,
                            value = 999999999)
            
          } else {
            data.table::set(temp,
                            i = k,
                            j = 1L,
                            value = lags)
            data.table::set(temp,
                            i = k,
                            j = 2L,
                            value = slags)
            data.table::set(
              temp,
              i = k,
              j = 3L,
              value = base::mean(abs(
                NNETAR_model_temp$residuals
              ),
              na.rm = TRUE)
            )
            data.table::set(
              temp,
              i = k,
              j = 4L,
              value = sd(NNETAR_model_temp$residuals,
                         na.rm = TRUE)
            )
          }
        }
      }
      
      # Identify best model and retrain it
      LagNN <- temp[order(meanResid)][1, ][, 1][[1]]
      SLagNN <- temp[order(meanResid)][1, ][, 2][[1]]
      NNETAR_model2 <-
        tryCatch({
          forecast::nnetar(
            y = Target,
            p = LagNN,
            P = SLagNN,
            lambda = "auto"
          )
        },
        error = function(x)
          "empty")
      
      # Collect Test Data for Model Comparison
      # 2)
      if (tolower(class(NNETAR_model2)) == "nnetar") {
        tryCatch({
          data_test_NN2 <- data.table::copy(data_test)
          data_test_NN2[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("NN_TSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(NNETAR_model2, h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_NN2[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval + 1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_NN2
        }, error = function(x)
          "skip")
      }
      
      k <- 0L
      temp <-
        data.table::data.table(
          Lag = rep(1L, Lags * SLags),
          Slag = rep(1L, Lags * SLags),
          meanResid = rnorm(Lags * SLags),
          sdResid = rnorm(Lags * SLags)
        )
      for (lags in seq_len(Lags)) {
        for (slags in seq_len(SLags)) {
          k <- k + 1L
          if (PrintUpdates)
            print(paste0("NNet 4 Iteration: ", k))
          NNETAR_model_temp <-
            tryCatch({
              forecast::nnetar(
                y = TargetMB,
                p = lags,
                P = slags,
                lambda = "auto"
              )
            }, error = function(x)
              "error")
          
          if (length(NNETAR_model_temp) == 1) {
            data.table::set(temp,
                            i = k,
                            j = 1L,
                            value = lags)
            data.table::set(temp,
                            i = k,
                            j = 2L,
                            value = slags)
            data.table::set(temp,
                            i = k,
                            j = 3L,
                            value = 999999999)
            data.table::set(temp,
                            i = k,
                            j = 4L,
                            value = 999999999)
            
          } else {
            data.table::set(temp,
                            i = k,
                            j = 1L,
                            value = lags)
            data.table::set(temp,
                            i = k,
                            j = 2L,
                            value = slags)
            data.table::set(
              temp,
              i = k,
              j = 3L,
              value = base::mean(abs(
                NNETAR_model_temp$residuals
              ),
              na.rm = TRUE)
            )
            data.table::set(
              temp,
              i = k,
              j = 4L,
              value = sd(NNETAR_model_temp$residuals,
                         na.rm = TRUE)
            )
          }
        }
      }
      
      # Identify best model and retrain it
      LagNN <- temp[order(meanResid)][1, ][, 1][[1]]
      SLagNN <- temp[order(meanResid)][1, ][, 2][[1]]
      NNETAR_model3 <-
        tryCatch({
          forecast::nnetar(
            y = dataTSTrain1[, TargetName],
            p = LagNN,
            P = SLagNN,
            lambda = "auto"
          )
        },
        error = function(x)
          "empty")
      
      # Collect Test Data for Model Comparison
      # 2)
      if (tolower(class(NNETAR_model3)) == "nnetar") {
        tryCatch({
          data_test_NN3 <- data.table::copy(data_test)
          data_test_NN3[, ':=' (
            Target = as.numeric(Target),
            ModelName = rep("NN_ModelFreqTSC", HoldOutPeriods),
            FC_Eval = as.numeric(
              forecast::forecast(NNETAR_model3, h = HoldOutPeriods)$mean
            )
          )]
          
          # Add Evaluation Columns
          # 3)
          data_test_NN3[, ':=' (
            Resid = get(TargetName) - FC_Eval,
            PercentError = get(TargetName) / (FC_Eval + 1) - 1,
            AbsolutePercentError = abs(get(TargetName) / (FC_Eval +
                                                            1) - 1),
            AbsoluteError = abs(get(TargetName) - FC_Eval),
            SquaredError = (get(TargetName) - FC_Eval) ^ 2
          )]
          
          # Increment
          i <- i + 1
          
          # Collect model filename
          EvalList[[i]] <- data_test_NN3
        }, error = function(x)
          "skip")
      }
    }
  }
  
  # Model Collection----
  if (PrintUpdates)
    message("FIND WINNER")
  dataEval <- data.table::rbindlist(EvalList)
  
  # Model Evaluation----
  if (tolower(EvaluationMetric) == "mae") {
    Eval <- dataEval[, .(
      MeanResid = round(base::mean(Resid, na.rm = TRUE), 2),
      MeanPercError = round(base::mean(PercentError, na.rm = TRUE), 5),
      MAPE = round(mean(AbsolutePercentError, na.rm = TRUE), 5),
      MAE = round(mean(AbsoluteError, na.rm = TRUE), 4),
      MSE = round(mean(SquaredError, na.rm = TRUE), 4)
    ),
    by = "ModelName"][order(MAE)][, ID := 1:.N]
  } else if (tolower(EvaluationMetric) == "mape") {
    Eval <- dataEval[, .(
      MeanResid = round(base::mean(Resid, na.rm = TRUE), 2),
      MeanPercError = round(base::mean(PercentError, na.rm = TRUE), 5),
      MAPE = round(mean(AbsolutePercentError, na.rm = TRUE), 5),
      MAE = round(mean(AbsoluteError, na.rm = TRUE), 4),
      MSE = round(mean(SquaredError, na.rm = TRUE), 4)
    ),
    by = "ModelName"][order(MAPE)][, ID := 1:.N]
  } else if (tolower(EvaluationMetric) == "mse") {
    Eval <- dataEval[, .(
      MeanResid = round(base::mean(Resid, na.rm = TRUE), 2),
      MeanPercError = round(base::mean(PercentError, na.rm = TRUE), 5),
      MAPE = round(mean(AbsolutePercentError, na.rm = TRUE), 5),
      MAE = round(mean(AbsoluteError, na.rm = TRUE), 4),
      MSE = round(mean(SquaredError, na.rm = TRUE), 4)
    ),
    by = "ModelName"][order(MSE)][, ID := 1:.N]
  }
  
  # Grab Winning Model----
  BestModel <- Eval[1, "ModelName"][[1]]
  
  # Generate Forecasts----
  if (PrintUpdates)
    message("GENERATE FORECASTS")
  
  # Create Training data----
  data_train <- data[seq_len(nrow(data))]
  
  # Create Full Training Data for Final Rebruild----
  if (grepl("ModelFreq", BestModel)) {
    if (grepl("TSC", BestModel)) {
      if (MinVal > 0) {
        # Model-Supplied-Freq
        SFreq <- forecast::findfrequency(as.matrix(data_train[, 2]))
        dataTSTrain <-
          stats::ts(data = data_train,
                    start = data_train[, min(get(DateName))][[1]],
                    frequency = SFreq)
        
        # TSClean Version
        dataTSTrain <-
          forecast::tsclean(x = dataTSTrain[, TargetName],
                            replace.missing = TRUE,
                            lambda = "auto")
      } else {
        # Model-Supplied-Freq
        SFreq <- forecast::findfrequency(as.matrix(data_train[, 2]))
        dataTSTrain <-
          stats::ts(data = data_train,
                    start = data_train[, min(get(DateName))][[1]],
                    frequency = SFreq)
        
        # TSClean Version
        dataTSTrain <-
          forecast::tsclean(x = dataTSTrain[, TargetName],
                            replace.missing = TRUE,
                            lambda = NULL)
      }
      
    } else {
      # Model-Supplied-Freq
      SFreq <- forecast::findfrequency(as.matrix(data_train[, 2]))
      dataTSTrain <-
        stats::ts(data = data_train,
                  start = data_train[, min(get(DateName))][[1]],
                  frequency = SFreq)
      
      # Only Target as Numeric Vector
      dataTSTrain <- dataTSTrain[, TargetName]
    }
  } else {
    if (grepl("TSC", BestModel)) {
      if (MinVal > 0) {
        # User-Supplied-Freq
        dataTSTrain <-
          stats::ts(data = data_train,
                    start = data_train[, min(get(DateName))][[1]],
                    frequency = freq)
        
        # TSClean Version
        dataTSTrain <-
          forecast::tsclean(x = dataTSTrain[, TargetName],
                            replace.missing = TRUE,
                            lambda = "auto")
      } else {
        # User-Supplied-Freq
        dataTSTrain <-
          stats::ts(data = data_train,
                    start = data_train[, min(get(DateName))][[1]],
                    frequency = freq)
        
        # TSClean Version
        dataTSTrain <-
          forecast::tsclean(x = dataTSTrain[, TargetName],
                            replace.missing = TRUE,
                            lambda = NULL)
      }
    } else {
      # User-Supplied-Freq
      dataTSTrain <-
        stats::ts(data = data_train,
                  start = data_train[, min(get(DateName))][[1]],
                  frequency = freq)
      
      # Only Target as Numeric Vector
      dataTSTrain <- dataTSTrain[, TargetName]
    }
  }
  
  # Retrain best model
  if (grepl(pattern = "DSHW", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA DSHW FITTING")
    if (BestModel == "DSHW_ModelFreq") {
      freq <- SFreq
    }
    if (MinVal > 0) {
      DSHW_Model <-
        tryCatch({
          forecast::dshw(
            y = dataTSTrain,
            period1 = freq,
            period2 = freq * 2,
            alpha = NULL,
            beta = NULL,
            gamma = NULL,
            omega = NULL,
            phi = NULL,
            lambda = "auto",
            biasadj = TRUE,
            armethod = TRUE,
            model = NULL
          )
        },
        error = function(x)
          "empty")
    } else {
      DSHW_Model <-
        tryCatch({
          forecast::dshw(
            y = dataTSTrain,
            period1 = freq,
            period2 = freq * 2,
            alpha = NULL,
            beta = NULL,
            gamma = NULL,
            omega = NULL,
            phi = NULL,
            lambda = NULL,
            biasadj = FALSE,
            armethod = TRUE,
            model = NULL
          )
        },
        error = function(x)
          "empty")
    }
    
    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(forecast::forecast(DSHW_Model,
                                                                              h = FCPeriods)$mean)]
    
    # Store model
    model <- DSHW_Model
    
  } else if (grepl(pattern = "ARFIMA", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA ARFIMA FITTING")
    # Rebuild model on full data
    if (StepWise) {
      if (MinVal > 0) {
        ARFIMA_model <- forecast::arfima(
          y = dataTSTrain,
          lambda = TRUE,
          biasadj = TRUE,
          max.p = Lags,
          max.q = Lags,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          stepwise = StepWise,
          num.cores = NumCores
        )
      } else {
        ARFIMA_model <- forecast::arfima(
          y = dataTSTrain,
          lambda = FALSE,
          biasadj = FALSE,
          max.p = Lags,
          max.q = Lags,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          stepwise = StepWise,
          num.cores = NumCores
        )
      }
    } else {
      if (MinVal > 0) {
        ARFIMA_model <- forecast::arfima(
          y = dataTSTrain,
          lambda = TRUE,
          biasadj = TRUE,
          max.p = Lags,
          max.q = Lags,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          stepwise = StepWise,
          parallel = TRUE,
          num.cores = NumCores
        )
      } else {
        ARFIMA_model <- forecast::arfima(
          y = dataTSTrain,
          lambda = FALSE,
          biasadj = FALSE,
          max.p = Lags,
          max.q = Lags,
          max.d = 1,
          max.D = 1,
          ic = "bic",
          stepwise = StepWise,
          parallel = TRUE,
          num.cores = NumCores
        )
      }
    }
    
    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(forecast::forecast(ARFIMA_model,
                                                                              h = FCPeriods)$mean)]
    
    # Store model
    model <- ARFIMA_model
    
  } else if (grepl(pattern = "ARIMA", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA ARIMA FITTING")
    # Rebuild model on full data
    if (StepWise) {
      if (MinVal > 0) {
        ARIMA_model <-
          forecast::auto.arima(
            y     = dataTSTrain,
            max.p = Lags,
            max.q = Lags,
            max.P = SLags,
            max.Q = SLags,
            max.d = 1,
            max.D = 1,
            ic = "bic",
            lambda = TRUE,
            biasadj = TRUE,
            stepwise = StepWise,
            num.cores = NumCores
          )
      } else {
        ARIMA_model <-
          forecast::auto.arima(
            y     = dataTSTrain,
            max.p = Lags,
            max.q = Lags,
            max.P = SLags,
            max.Q = SLags,
            max.d = 1,
            max.D = 1,
            ic = "bic",
            lambda = FALSE,
            biasadj = FALSE,
            stepwise = StepWise,
            num.cores = NumCores
          )
      }
    } else {
      if (MinVal > 0) {
        ARIMA_model <-
          forecast::auto.arima(
            y     = dataTSTrain,
            max.p = Lags,
            max.q = Lags,
            max.P = SLags,
            max.Q = SLags,
            max.d = 1,
            max.D = 1,
            ic = "bic",
            lambda = TRUE,
            biasadj = TRUE,
            stepwise = StepWise,
            parallel = TRUE,
            num.cores = NumCores
          )
      } else {
        ARIMA_model <-
          forecast::auto.arima(
            y     = dataTSTrain,
            max.p = Lags,
            max.q = Lags,
            max.P = SLags,
            max.Q = SLags,
            max.d = 1,
            max.D = 1,
            ic = "bic",
            lambda = FALSE,
            biasadj = FALSE,
            stepwise = StepWise,
            parallel = TRUE,
            num.cores = NumCores
          )
      }
    }
    
    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(forecast::forecast(ARIMA_model,
                                                                              h = FCPeriods)$mean)]
    
    # Store model
    model <- ARIMA_model
    
  } else if (grepl(pattern = "ETS", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA ETS FITTING")
    # Rebuild model on full data
    if (freq > 24) {
      if (MinVal > 0) {
        # when > 24, model's third letter has to be N for none
        EXPSMOOTH_model <-
          forecast::ets(
            y                          = dataTSTrain,
            model                      = "ZZN",
            allow.multiplicative.trend = TRUE,
            restrict                   = TRUE,
            lambda                     = TRUE,
            biasadj                    = TRUE
          )
      } else {
        # when > 24, model's third letter has to be N for none
        EXPSMOOTH_model <-
          forecast::ets(
            y                          = dataTSTrain,
            model                      = "ZZN",
            allow.multiplicative.trend = TRUE,
            restrict                   = TRUE,
            lambda                     = FALSE,
            biasadj                    = FALSE
          )
      }
    } else {
      if (MinVal > 0) {
        EXPSMOOTH_model <-
          forecast::ets(
            y                          = dataTSTrain,
            model                      = "ZZZ",
            allow.multiplicative.trend = TRUE,
            restrict                   = TRUE,
            lambda                     = TRUE,
            biasadj                    = TRUE
          )
      } else {
        EXPSMOOTH_model <-
          forecast::ets(
            y                          = dataTSTrain,
            model                      = "ZZZ",
            allow.multiplicative.trend = TRUE,
            restrict                   = TRUE,
            lambda                     = FALSE,
            biasadj                    = FALSE
          )
      }
    }
    
    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(forecast::forecast(EXPSMOOTH_model,
                                                                              h = FCPeriods)$mean)]
    
    # Store model
    model <- EXPSMOOTH_model
    
  } else if (grepl(pattern = "TBATS", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA TBATS FITTING")
    if (MinVal > 0) {
      # Rebuild model on full data
      TBATS_model <- forecast::tbats(
        y = dataTSTrain,
        use.arma.errors = TRUE,
        lambda = TRUE,
        biasadj = TRUE,
        max.p = Lags,
        max.q = Lags,
        max.P = SLags,
        max.Q = SLags,
        max.d = 1,
        max.D = 1,
        num.cores = NumCores
      )
    } else {
      # Rebuild model on full data
      TBATS_model <- forecast::tbats(
        y = dataTSTrain,
        use.arma.errors = TRUE,
        lambda = FALSE,
        biasadj = FALSE,
        max.p = Lags,
        max.q = Lags,
        max.P = SLags,
        max.Q = SLags,
        max.d = 1,
        max.D = 1,
        num.cores = NumCores
      )
    }
    
    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(forecast::forecast(TBATS_model,
                                                                              h = FCPeriods)$mean)]
    
    # Store model
    model <- TBATS_model
    
  } else if (grepl(pattern = "TSLM", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA TSLM FITTING")
    if (MinVal > 0) {
      # Rebuild model on full data
      TSLM_model <-
        forecast::tslm(dataTSTrain ~ trend + season,
                       lambda = TRUE,
                       biasadj = TRUE)
    } else {
      # Rebuild model on full data
      TSLM_model <-
        forecast::tslm(dataTSTrain ~ trend + season,
                       lambda = FALSE,
                       biasadj = FALSE)
    }
    
    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(forecast::forecast(TSLM_model, h = FCPeriods)$mean)]
    
    # Store model
    model <- TSLM_model
    
  } else if (grepl(pattern = "NN", BestModel)) {
    if (PrintUpdates)
      message("FULL DATA NN FITTING")
    # Rebuild model on full data
    k <- 0L
    temp <-
      data.table::data.table(
        Lag = rep(1L, Lags * SLags),
        Slag = rep(1L, Lags * SLags),
        meanResid = rnorm(Lags * SLags),
        sdResid = rnorm(Lags * SLags)
      )
    for (lags in seq_len(Lags)) {
      for (slags in seq_len(SLags)) {
        k <- k + 1L
        if (PrintUpdates)
          print(k)
        NNETAR_model_temp <-
          tryCatch({
            forecast::nnetar(
              y = dataTSTrain,
              p = lags,
              P = slags,
              lambda = "auto"
            )
          }, error = function(x)
            "error")
        
        if (length(NNETAR_model_temp) == 1) {
          data.table::set(temp,
                          i = k,
                          j = 1L,
                          value = lags)
          data.table::set(temp,
                          i = k,
                          j = 2L,
                          value = slags)
          data.table::set(temp,
                          i = k,
                          j = 3L,
                          value = 999999999)
          data.table::set(temp,
                          i = k,
                          j = 4L,
                          value = 999999999)
          
        } else {
          data.table::set(temp,
                          i = k,
                          j = 1L,
                          value = lags)
          data.table::set(temp,
                          i = k,
                          j = 2L,
                          value = slags)
          data.table::set(
            temp,
            i = k,
            j = 3L,
            value = base::mean(abs(NNETAR_model_temp$residuals),
                               na.rm = TRUE)
          )
          data.table::set(
            temp,
            i = k,
            j = 4L,
            value = sd(NNETAR_model_temp$residuals,
                       na.rm = TRUE)
          )
        }
      }
    }
    
    # Identify best model and retrain it
    LagNN <- temp[order(meanResid)][1, ][, 1][[1]]
    SLagNN <- temp[order(meanResid)][1, ][, 2][[1]]
    NNETAR_model <-
      tryCatch({
        forecast::nnetar(y = dataTSTrain,
                         p = LagNN,
                         P = SLagNN)
      },
      error = function(x)
        "empty")
    
    # Forecast with new model
    FC_Data[, paste0("Forecast_", BestModel) := as.numeric(forecast::forecast(NNETAR_model, h = FCPeriods)$mean)]
    
    # Store model
    model <- NNETAR_model
    
  }
  
  # Create plot
  temp <- data.table::copy(FC_Data)
  data.table::setnames(data, c(eval(DateName)), "Date")
  Time <-
    data.table::rbindlist(list(data[, "Date"], temp[, "Date"]))
  z <-
    data.table::rbindlist(list(data[, Date := NULL], temp[, Date := NULL]), fill = TRUE)
  z <- cbind(Time, z)
  z[, eval(TargetName) := as.numeric(get(TargetName))]
  logo = magick::image_read(
    "https://www.remixinstitute.com/wp-content/uploads/7b-Cheetah_Charcoal_Inline_No_Sub_No_BG.png"
  )
  TimeSeriesPlot <-
    ggplot2::ggplot(z, ggplot2::aes(x = z[["Date"]])) +
    ggplot2::geom_line(ggplot2::aes(y = z[[eval(TargetName)]]), color = "#005B80") +
    ggplot2::geom_line(ggplot2::aes(y = z[[3]]), color = "#1c1c1c") +
    ggplot2::geom_vline(
      xintercept = max(data_test[[eval(DateName)]],
                       na.rm = TRUE),
      color = "#FF4F00",
      lty = "dotted",
      lwd = 1
    ) +
    RemixTheme() +
    ggplot2::labs(
      title = paste0(FCPeriods, "-", TimeUnit, " Forecast for ", TempTargetName),
      subtitle = paste0(
        "Champion Model: ",
        BestModel,
        " | Mean Absolute Percentage Error: ",
        paste(round(min(Eval$MAPE), 3) * 100, "%", sep = "")
      ),
      caption = "Forecast generated by Remix Institute's RemixAutoML R package"
    ) +
    ggplot2::xlab(eval(DateName)) + ggplot2::ylab(eval(TempTargetName))
  
  # Get back to adding image to plot----
  # TimeSeriesPlot
  # grid::grid.raster(logo, x = .73, y = 0.01, just = c('left', 'bottom'), width = 0.25)
  
  options(warn = 0)
  
  # Return values
  return(
    list(
      Forecast = FC_Data,
      EvaluationMetrics = Eval,
      TimeSeriesModel = model,
      ChampionModel = BestModel,
      TimeSeriesPlot = TimeSeriesPlot
    )
  )
}

#' AutoCatBoostCARMA Automated CatBoost Calendar and ARMA Variables Forecasting
#'
#' AutoCatBoostCARMA Automated CatBoost Calendar and ARMA Variables Forecasting. Create hundreds of thousands of time series forecasts using this function.
#'
#' @family Time Series
#' @param data Supply your full series data set here
#' @param TargetColumnName List the column name of your target variables column. E.g. "Target"
#' @param DateColumnName List the column name of your date column. E.g. "DateTime"
#' @param GroupVariables Defaults to NULL. Use NULL when you have a single series. Add in GroupVariables when you have a series for every level of a group or multiple groups.
#' @param FC_Periods Set the number of periods you want to have forecasts for. E.g. 52 for weekly data to forecast a year ahead
#' @param TimeUnit List the time unit your data is aggregated by. E.g. "hour", "day", "week", "year"
#' @param Lags Select the periods for all lag variables you want to create. E.g. I use this for weekly data c(1:5,52)
#' @param MA_Periods Select the periods for all moving average variables you want to create. E.g. I use this for weekly data c(1:5,52)
#' @param CalendarVariables Set to TRUE to have calendar variables created. The calendar variables are numeric representations of second, minute, hour, week day, month day, year day, week, isoweek, quarter, and year
#' @param TimeTrendVariable Set to TRUE to have a time trend variable added to the model. Time trend is numeric variable indicating the numeric value of each record in the time series (by group). Time trend starts at 1 for the earliest point in time and increments by one for each success time point.
#' @param DataTruncate Set to TRUE to remove records with missing values from the lags and moving average features created
#' @param SplitRatios E.g c(0.7,0.2,0.1) for train, validation, and test sets
#' @param EvalMetric Select from "RMSE", "MAE", "MAPE", "Poisson", "Quantile", "LogLinQuantile", "Lq", "NumErrors", "SMAPE", "R2", "MSLE", "MedianAbsoluteError"
#' @param GridEvalMetric This is the metric used to find the threshold 'poisson', 'mae', 'mape', 'mse', 'msle', 'kl', 'cs', 'r2'
#' @param TaskType Default is "GPU" but you can also set it to "CPU"
#' @param GridTune Set to TRUE to run a grid tune
#' @param ModelCount Set the number of models to try in the grid tune
#' @param NTrees Select the number of trees you want to have built to train the model
#' @param PartitionType Select "random" for random data partitioning "time" for partitioning by time frames
#' @param Timer = TRUE
#' @examples
#' \donttest{
#' Results <- AutoCatBoostCARMA(data,
#'                              TargetColumnName = "Weekly_Sales",
#'                              DateColumnName = "Date",
#'                              GroupVariables = c("Store","Dept"),
#'                              FC_Periods = 52,
#'                              TimeUnit = "week",
#'                              Lags = c(1:5,52),
#'                              MA_Periods = c(1:5,52),
#'                              CalendarVariables = TRUE,
#'                              TimeTrendVariable = TRUE,
#'                              DataTruncate = FALSE,
#'                              SplitRatios = c(1-2*30/143,30/143,30/143),
#'                              TaskType = "GPU",
#'                              EvalMetric = "MAE",
#'                              GridTune = FALSE,
#'                              GridEvalMetric = "mae",
#'                              ModelCount = 1,
#'                              NTrees = 1000,
#'                              PartitionType = "time")
#' Results$TimeSeriesPlot
#' Results$Forecast
#' Results$ModelInformation$...
#' }
#' @return Returns a data.table of original series and forecasts, the catboost model objects (everything returned from AutoCatBoostRegression()), and a time series forecast plot. The time series forecast plot will plot your single series or aggregate your data to a single series and create a plot from that.
#' @export
AutoCatBoostCARMA <- function(data,
                              TargetColumnName = "Target",
                              DateColumnName = "DateTime",
                              GroupVariables = NULL,
                              FC_Periods = 30,
                              TimeUnit = "week",
                              Lags = c(1:5),
                              MA_Periods = c(1:5),
                              CalendarVariables = FALSE,
                              TimeTrendVariable = FALSE,
                              DataTruncate = FALSE,
                              SplitRatios = c(0.7, 0.2, 0.1),
                              TaskType = "GPU",
                              EvalMetric = "MAPE",
                              GridTune = FALSE,
                              GridEvalMetric = "mape",
                              ModelCount = 1,
                              NTrees = 1000,
                              PartitionType = "timeseries",
                              Timer = TRUE) {
  # Load catboost----
  loadNamespace(package = "catboost")
  
  # Check arguments----
  if (!(tolower(PartitionType) %chin% c("random", "time", "timeseries"))) {
    return("PartitionType needs to be one of 'random', 'time', or 'timeseries'")
  }
  if (tolower(PartitionType) == "timeseries" &
      is.null(GroupVariables)) {
    PartitionType <- "time"
    warning("PartitionType was converted to 'time' because there were no GroupVariables")
  }
  
  # Convert to data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Subset Columns----
  keep <- c(DateColumnName, TargetColumnName, GroupVariables)
  data <- data[, ..keep]
  
  # Group Concatenation----
  if (!is.null(GroupVariables)) {
    data[, GroupVar := do.call(paste, c(.SD, sep = " ")), .SDcols = GroupVariables]
    data[, eval(GroupVariables) := NULL]
  }
  
  # Get unique set of GroupVar----
  GroupVarVector <- unique(as.character(data[["GroupVar"]]))
  
  # Change column ordering
  if (!is.null(GroupVariables)) {
    data.table::setcolorder(data,
                            c("GroupVar",
                              eval(DateColumnName),
                              eval(TargetColumnName)))
  } else {
    data.table::setcolorder(data,
                            c(eval(DateColumnName),
                              eval(TargetColumnName)))
  }
  
  
  # Convert to lubridate as_date() or POSIXct----
  if (tolower(TimeUnit) != "hour") {
    data[, eval(DateColumnName) := lubridate::as_date(get(DateColumnName))]
  } else {
    data[, eval(DateColumnName) := as.POSIXct(get(DateColumnName))]
  }
  
  # Define NumSets
  NumSets <- length(SplitRatios)
  
  # Set max vals----
  val <- max(Lags, MA_Periods)
  
  # Ensure data is sorted----
  if (!is.null(GroupVariables)) {
    data <- data[order(GroupVar, get(DateColumnName))]
  } else {
    data <- data[order(get(DateColumnName))]
  }
  
  # Create Calendar Variables----
  if (CalendarVariables) {
    data <- RemixAutoML::CreateCalendarVariables(
      data = data,
      DateCols = eval(DateColumnName),
      AsFactor = FALSE,
      TimeUnits = c(
        "second",
        "minute",
        "hour",
        "wday",
        "mday",
        "yday",
        "week",
        "isoweek",
        "month",
        "quarter",
        "year"
      )
    )
  }
  
  # GDL Features----
  if (!is.null(GroupVariables)) {
    data <- RemixAutoML::DT_GDL_Feature_Engineering(
      data,
      lags           = c(Lags),
      periods        = c(MA_Periods),
      statsNames     = c("MA"),
      targets        = eval(TargetColumnName),
      groupingVars   = "GroupVar",
      sortDateName   = eval(DateColumnName),
      timeDiffTarget = NULL,
      timeAgg        = NULL,
      WindowingLag   = 1,
      Type           = "Lag",
      Timer          = FALSE,
      SimpleImpute   = TRUE
    )
  } else {
    data <- RemixAutoML::DT_GDL_Feature_Engineering(
      data,
      lags           = c(Lags),
      periods        = c(MA_Periods),
      statsNames     = c("MA"),
      targets        = eval(TargetColumnName),
      groupingVars   = NULL,
      sortDateName   = eval(DateColumnName),
      timeDiffTarget = NULL,
      timeAgg        = NULL,
      WindowingLag   = 1,
      Type           = "Lag",
      Timer          = FALSE,
      SimpleImpute   = TRUE
    )
  }
  
  # TimeTrend Variable----
  if (TimeTrendVariable) {
    if (!is.null(GroupVariables)) {
      data[, TimeTrend := 1:.N, by = "GroupVar"]
    } else {
      data[, TimeTrend := 1:.N]
    }
  }
  
  # Prepare data----
  data <- RemixAutoML::ModelDataPrep(
    data,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = FALSE,
    MissFactor = "0",
    MissNum    = -1
  )
  
  # Subset Data----
  if (DataTruncate) {
    data <- data[val:.N]
  }
  
  # Partition Data----
  if (tolower(PartitionType) == "timeseries") {
    DataSets <- RemixAutoML::AutoDataPartition(
      data,
      NumDataSets = NumSets,
      Ratios = SplitRatios,
      PartitionType = "timeseries",
      StratifyColumnNames = "GroupVar",
      TimeColumnName = NULL
    )
  } else if (tolower(PartitionType) == "random") {
    if (!is.null(GroupVariables)) {
      DataSets <- RemixAutoML::AutoDataPartition(
        data,
        NumDataSets = NumSets,
        Ratios = SplitRatios,
        PartitionType = "random",
        StratifyColumnNames = "GroupVar",
        TimeColumnName = eval(DateColumnName)
      )
    } else {
      DataSets <- RemixAutoML::AutoDataPartition(
        data,
        NumDataSets = NumSets,
        Ratios = SplitRatios,
        PartitionType = "random",
        StratifyColumnNames = NULL,
        TimeColumnName = eval(DateColumnName)
      )
    }
  } else {
    DataSets <- RemixAutoML::AutoDataPartition(
      data,
      NumDataSets = NumSets,
      Ratios = SplitRatios,
      PartitionType = "time",
      StratifyColumnNames = NULL,
      TimeColumnName = eval(DateColumnName)
    )
  }
  
  # Define data sets----
  if (NumSets == 2) {
    train <- DataSets$TrainData
    valid <- DataSets$ValidationData
  } else if (NumSets == 3) {
    train <- DataSets$TrainData
    valid <- DataSets$ValidationData
    test  <- DataSets$TestData
  }
  
  # Pass along base data unperturbed----
  dataFuture <- data.table::copy(data)
  
  # Build Model----
  if (NumSets == 2) {
    if (!is.null(GroupVariables)) {
      TestModel <- RemixAutoML::AutoCatBoostRegression(
        data = train,
        ValidationData = valid,
        TestData = NULL,
        TargetColumnName = eval(TargetColumnName),
        FeatureColNames = setdiff(names(data),
                                  eval(TargetColumnName)),
        PrimaryDateColumn = eval(DateColumnName),
        IDcols = 2,
        MaxModelsInGrid = 1,
        task_type = TaskType,
        eval_metric = EvalMetric,
        grid_eval_metric = GridEvalMetric,
        Trees = NTrees,
        GridTune = FALSE,
        model_path = getwd(),
        ModelID = "ModelTest",
        NumOfParDepPlots = 3,
        ReturnModelObjects = TRUE,
        SaveModelObjects = FALSE,
        PassInGrid = NULL
      )
    } else {
      if (!is.null(GroupVariables)) {
        TestModel <- RemixAutoML::AutoCatBoostRegression(
          data = train,
          ValidationData = valid,
          TestData = NULL,
          TargetColumnName = eval(TargetColumnName),
          FeatureColNames = setdiff(names(data),
                                    eval(TargetColumnName)),
          PrimaryDateColumn = eval(DateColumnName),
          IDcols = 2,
          MaxModelsInGrid = 1,
          task_type = TaskType,
          eval_metric = EvalMetric,
          grid_eval_metric = GridEvalMetric,
          Trees = NTrees,
          GridTune = FALSE,
          model_path = getwd(),
          ModelID = "ModelTest",
          NumOfParDepPlots = 3,
          ReturnModelObjects = TRUE,
          SaveModelObjects = FALSE,
          PassInGrid = NULL
        )
      } else {
        TestModel <- RemixAutoML::AutoCatBoostRegression(
          data = train,
          ValidationData = valid,
          TestData = NULL,
          TargetColumnName = eval(TargetColumnName),
          FeatureColNames = setdiff(names(data),
                                    eval(TargetColumnName)),
          PrimaryDateColumn = eval(DateColumnName),
          IDcols = 2,
          MaxModelsInGrid = 1,
          task_type = TaskType,
          eval_metric = EvalMetric,
          grid_eval_metric = GridEvalMetric,
          Trees = NTrees,
          GridTune = FALSE,
          model_path = getwd(),
          ModelID = "ModelTest",
          NumOfParDepPlots = 3,
          ReturnModelObjects = TRUE,
          SaveModelObjects = FALSE,
          PassInGrid = NULL
        )
      }
    }
  } else if (NumSets == 3) {
    if (!is.null(GroupVariables)) {
      TestModel <- RemixAutoML::AutoCatBoostRegression(
        data = train,
        ValidationData = valid,
        TestData = test,
        TargetColumnName = eval(TargetColumnName),
        FeatureColNames = setdiff(names(data), eval(TargetColumnName)),
        PrimaryDateColumn = eval(DateColumnName),
        IDcols = 2,
        MaxModelsInGrid = 1,
        task_type = TaskType,
        eval_metric = EvalMetric,
        grid_eval_metric = GridEvalMetric,
        Trees = NTrees,
        GridTune = FALSE,
        model_path = getwd(),
        ModelID = "ModelTest",
        NumOfParDepPlots = 3,
        ReturnModelObjects = TRUE,
        SaveModelObjects = FALSE,
        PassInGrid = NULL
      )
    } else {
      TestModel <- RemixAutoML::AutoCatBoostRegression(
        data = train,
        ValidationData = valid,
        TestData = test,
        TargetColumnName = eval(TargetColumnName),
        FeatureColNames = setdiff(names(data), eval(TargetColumnName)),
        PrimaryDateColumn = eval(DateColumnName),
        IDcols = NULL,
        MaxModelsInGrid = 1,
        task_type = TaskType,
        eval_metric = EvalMetric,
        grid_eval_metric = GridEvalMetric,
        Trees = NTrees,
        GridTune = FALSE,
        model_path = getwd(),
        ModelID = "ModelTest",
        NumOfParDepPlots = 3,
        ReturnModelObjects = TRUE,
        SaveModelObjects = FALSE,
        PassInGrid = NULL
      )
    }
  }
  
  # Store Model----
  Model <- TestModel$Model
  
  # Update ValidationData and Create Metrics Data----
  TestDataEval <- TestModel$ValidationData
  TestDataEval[, ':=' (Target = NULL, Date = NULL)]
  MinVal <- TestDataEval[, min(get(TargetColumnName), na.rm = TRUE)]
  if (!is.null(GroupVariables)) {
    Metric <-
      TestDataEval[, .(GroupVar, get(TargetColumnName), Predict)]
    data.table::setnames(Metric, "V2", eval(TargetColumnName))
    MetricCollection <-
      Metric[, GroupVar, by = "GroupVar"][, GroupVar := NULL]
  }
  
  # poisson----
  if (MinVal > 0 &
      min(TestDataEval[["Predict"]], na.rm = TRUE) > 0) {
    if (!is.null(GroupVariables)) {
      TestDataEval[, Metric := Predict - get(TargetColumnName) * log(Predict + 1)]
      MetricCollection <-
        merge(MetricCollection,
              TestDataEval[, .(Poisson_Metric = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
              by = "GroupVar",
              all = FALSE)
    } else {
      TestDataEval[, Metric := Predict - get(TargetColumnName) * log(Predict + 1)]
      Metric <-
        TestDataEval[, .(Poisson_Metric = mean(Metric, na.rm = TRUE))]
    }
  }
  
  # mae----
  if (!is.null(GroupVariables)) {
    TestDataEval[, Metric := abs(get(TargetColumnName) - Predict)]
    MetricCollection <-
      merge(MetricCollection,
            TestDataEval[, .(MAE_Metric = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
            by = "GroupVar",
            all = FALSE)
  } else {
    TestDataEval[, Metric := abs(get(TargetColumnName) - Predict)]
    Metric <- TestDataEval[, mean(Metric, na.rm = TRUE)]
  }
  
  # mape----
  if (!is.null(GroupVariables)) {
    TestDataEval[, Metric := abs((get(TargetColumnName) - Predict) / (get(TargetColumnName) + 1))]
    MetricCollection <-
      merge(MetricCollection,
            TestDataEval[, .(MAPE_Metric = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
            by = "GroupVar",
            all = FALSE)
  } else {
    TestDataEval[, Metric := abs((get(TargetColumnName) - Predict) / (get(TargetColumnName) + 1))]
    Metric <-
      TestDataEval[, .(MAPE_Metric = mean(Metric, na.rm = TRUE))]
  }
  
  # mse----
  if (!is.null(GroupVariables)) {
    TestDataEval[, Metric := (get(TargetColumnName) - Predict) ^ 2]
    MetricCollection <-
      merge(MetricCollection,
            TestDataEval[, .(MSE_Metric = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
            by = "GroupVar",
            all = FALSE)
  } else {
    TestDataEval[, Metric := (get(TargetColumnName) - Predict) ^ 2]
    Metric <-
      TestDataEval[, .(MSE_Metric = mean(Metric, na.rm = TRUE))]
  }
  
  # msle----
  if (MinVal > 0 &
      min(TestDataEval[["Predict"]], na.rm = TRUE) > 0) {
    if (!is.null(GroupVariables)) {
      TestDataEval[, Metric := (log(get(TargetColumnName) + 1) - log(Predict + 1)) ^ 2]
      MetricCollection <-
        merge(MetricCollection,
              TestDataEval[, .(MSLE = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
              by = "GroupVar",
              all = FALSE)
    } else {
      TestDataEval[, Metric := (log(get(TargetColumnName) + 1) - log(Predict + 1)) ^ 2]
      Metric <- TestDataEval[, .(MSLE = mean(Metric, na.rm = TRUE))]
    }
  }
  
  # kl----
  if (MinVal > 0 &
      min(TestDataEval[["Predict"]], na.rm = TRUE) > 0) {
    if (!is.null(GroupVariables)) {
      TestDataEval[, Metric := get(TargetColumnName) * log((get(TargetColumnName) + 1) / (Predict + 1))]
      MetricCollection <-
        merge(MetricCollection,
              TestDataEval[, .(KL_Metric = mean(Metric, na.rm = TRUE)), by = "GroupVar"],
              by = "GroupVar",
              all = FALSE)
    } else {
      TestDataEval[, Metric := get(TargetColumnName) * log((get(TargetColumnName) + 1) / (Predict + 1))]
      Metric <-
        TestDataEval[, .(KL_Metric = mean(Metric, na.rm = TRUE))]
    }
  }
  
  # r2----
  if (!is.null(GroupVariables)) {
    MetricCollection <-
      merge(MetricCollection,
            TestDataEval[, .(R2_Metric = stats::cor(get(TargetColumnName), Predict)), by = "GroupVar"],
            by = "GroupVar",
            all = FALSE)
    MetricCollection[, R2_Metric := R2_Metric ^ 2]
  } else {
    Metric <-
      (TestDataEval[, .(R2_Metric = stats::cor(get(TargetColumnName), Predict))]) ^ 2
  }
  
  # Update GroupVar with Original Columns, reorder columns, add to model objects----
  if (!is.null(GroupVariables)) {
    MetricCollection[, eval(GroupVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
    NumGroupVars <- length(GroupVariables)
    data.table::setcolorder(MetricCollection,
                            c((ncol(MetricCollection) - NumGroupVars + 1):ncol(MetricCollection),
                              1:(ncol(MetricCollection) - NumGroupVars)
                            ))
    TestModel[["EvaluationMetricsByGroup"]] <- MetricCollection
    TestModel$EvaluationMetricsByGroup
  }
  
  # Store Date Info----
  if (!is.null(GroupVariables)) {
    FutureDateData <- unique(dataFuture[, get(DateColumnName)])
    for (i in  seq_len(FC_Periods)) {
      FutureDateData <- c(FutureDateData, max(FutureDateData) + 1)
    }
  } else {
    FutureDateData <- dataFuture[, get(DateColumnName)]
    for (i in  seq_len(FC_Periods)) {
      FutureDateData <- c(FutureDateData, max(FutureDateData) + 1)
    }
  }
  
  # Row Count----
  if (!is.null(GroupVariables)) {
    N <- data[, .N, by = "GroupVar"][, max(N)]
  } else {
    N <- data[, .N]
  }
  
  # Begin loop for generating forecasts----
  for (i in seq_len(FC_Periods)) {
    # Row counts----
    if (i != 1) {
      N <- N + 1
    }
    
    # Generate predictions----
    if (i == 1) {
      Preds <- RemixAutoML::AutoCatBoostScoring(
        TargetType = "regression",
        ScoringData = data,
        FeatureColumnNames = setdiff(names(data),
                                     c(
                                       eval(DateColumnName),
                                       eval(TargetColumnName)
                                     )),
        IDcols = NULL,
        Model = Model,
        ModelPath = getwd(),
        ModelID = "ModelTest",
        ReturnFeatures = TRUE,
        MDP_Impute = TRUE,
        MDP_CharToFactor = TRUE,
        MDP_RemoveDates = TRUE,
        MDP_MissFactor = "0",
        MDP_MissNum = -1
      )
      
      # Update data----
      UpdateData <- cbind(FutureDateData[i:N],
                          data[, get(TargetColumnName)], Preds)
      data.table::setnames(UpdateData,
                           c("V1", "V2"),
                           c(eval(DateColumnName),
                             eval(TargetColumnName)))
    } else {
      if (!is.null(GroupVariables)) {
        temp <- data.table::copy(UpdateData[, ID := 1:.N, by = "GroupVar"])
        temp <- temp[ID == N][, ID := NULL]
        Preds <- RemixAutoML::AutoCatBoostScoring(
          TargetType = "regression",
          ScoringData = temp,
          FeatureColumnNames = setdiff(names(temp),
                                       c(
                                         "Predictions",
                                         eval(DateColumnName),
                                         eval(TargetColumnName)
                                       )),
          IDcols = NULL,
          Model = Model,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          ReturnFeatures = FALSE,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1
        )
        
        # Update data group case----
        data.table::setnames(Preds, "Predictions", "Preds")
        Preds <- cbind(UpdateData[ID == N], Preds)
        Preds[, eval(TargetColumnName) := Preds]
        Preds[, Predictions := Preds][, Preds := NULL]
        UpdateData <- UpdateData[ID != N]
        UpdateData <- data.table::rbindlist(list(UpdateData, Preds))
        UpdateData[, ID := NULL]
      } else {
        Preds <- RemixAutoML::AutoCatBoostScoring(
          TargetType = "regression",
          ScoringData = UpdateData[.N,],
          FeatureColNames = setdiff(names(UpdateData),
                                    c(
                                      "Predictions",
                                      eval(DateColumnName),
                                      eval(TargetColumnName)
                                    )),
          IDcols = NULL,
          Model = Model,
          ModelPath = getwd(),
          ModelID = "ModelTest",
          ReturnFeatures = FALSE,
          MDP_Impute = TRUE,
          MDP_CharToFactor = TRUE,
          MDP_RemoveDates = TRUE,
          MDP_MissFactor = "0",
          MDP_MissNum = -1
        )
        
        # Update data non-group case----
        data.table::set(UpdateData,
                        i = N,
                        j = 2:3,
                        value = Preds[[1]])
      }
    }
    
    # Timer----
    if (Timer) {
      print(paste("Forecast future step: ", i))
    }
    
    # Create single future record----
    d <- max(UpdateData[[eval(DateColumnName)]])
    if (tolower(TimeUnit) == "hour") {
      CalendarFeatures <-
        data.table::as.data.table(d + lubridate::hours(1))
    } else if (tolower(TimeUnit) == "day") {
      CalendarFeatures <-
        data.table::as.data.table(d + lubridate::days(1))
    } else if (tolower(TimeUnit) == "week") {
      CalendarFeatures <-
        data.table::as.data.table(d + lubridate::weeks(1))
    } else if (tolower(TimeUnit) == "month") {
      CalendarFeatures <- data.table::as.data.table(d %m+% months(1))
    } else if (tolower(TimeUnit) == "quarter") {
      CalendarFeatures <- data.table::as.data.table(d %m+% months(4))
    } else if (tolower(TimeUnit) == "year") {
      CalendarFeatures <-
        data.table::as.data.table(d + lubridate::years(1))
    }
    
    # Prepare for more feature engineering----
    data.table::setnames(CalendarFeatures, "V1", eval(DateColumnName))
    CalendarFeatures[, eval(DateColumnName) := data.table::as.IDate(get(DateColumnName))]
    if (!is.null(GroupVariables)) {
      CalendarFeatures <- cbind(GroupVarVector, CalendarFeatures)
      data.table::setnames(CalendarFeatures, "GroupVarVector", "GroupVar")
    }
    
    # Add calendar variables----
    if (CalendarVariables) {
      CalendarFeatures <- RemixAutoML::CreateCalendarVariables(
        data = CalendarFeatures,
        DateCols = eval(DateColumnName),
        AsFactor = FALSE,
        TimeUnits = c(
          "second",
          "minute",
          "hour",
          "wday",
          "mday",
          "yday",
          "week",
          "isoweek",
          "month",
          "quarter",
          "year"
        )
      )
    }
    
    # Add TimeTrendVariable----
    if (TimeTrendVariable) {
      CalendarFeatures[, TimeTrend := N + 1]
    }
    
    # Update features for next run----
    if (i != max(FC_Periods)) {
      temp <- cbind(CalendarFeatures, 1)
      if (tolower(TimeUnit) != "hour") {
        temp[, eval(DateColumnName) := lubridate::as_date(get(DateColumnName))]
      } else {
        temp[, eval(DateColumnName) := as.POSIXct(get(DateColumnName))]
      }
      temp[, eval(DateColumnName) := lubridate::as_date(get(DateColumnName))]
      data.table::setnames(temp, c("V2"), c(eval(TargetColumnName)))
      UpdateData <-
        data.table::rbindlist(list(UpdateData, temp), fill = TRUE)
      
      # Update Lags and MA's----
      if (!is.null(GroupVariables)) {
        UpdateData <- UpdateData[order(GroupVar, get(DateColumnName))]
        UpdateData[, ID := .N:1, by = "GroupVar"]
        keep <- unique(c(
          eval(DateColumnName),
          eval(TargetColumnName),
          "Predictions",
          "GroupVar",
          "ID",
          names(CalendarFeatures)
        ))
        Temporary <- data.table::copy(UpdateData[, ..keep])
        Temporary <- Scoring_GDL_Feature_Engineering(
          data = Temporary,
          lags           = c(Lags),
          periods        = c(MA_Periods),
          statsNames     = c("MA"),
          targets        = eval(TargetColumnName),
          groupingVars   = "GroupVar",
          sortDateName   = eval(DateColumnName),
          timeDiffTarget = NULL,
          timeAgg        = NULL,
          WindowingLag   = 1,
          Type           = "Lag",
          Timer          = FALSE,
          SimpleImpute   = TRUE,
          AscRowByGroup  = "ID",
          RecordsKeep    = 1
        )
        
        # Not lining up - Updatedata and Temporary
        UpdateData <-
          data.table::rbindlist(list(UpdateData[ID != 1], Temporary), use.names = TRUE)
      } else {
        UpdateData <- UpdateData[order(get(DateColumnName))]
        UpdateData[, ID := .N:1]
        keep <- unique(c(
          eval(DateColumnName),
          eval(TargetColumnName),
          "Predictions",
          "ID",
          names(CalendarFeatures)
        ))
        Temporary <- data.table::copy(UpdateData[, ..keep])
        Temporary <- Scoring_GDL_Feature_Engineering(
          data = Temporary,
          lags           = c(Lags),
          periods        = c(MA_Periods),
          statsNames     = c("MA"),
          targets        = eval(TargetColumnName),
          groupingVars   = NULL,
          sortDateName   = eval(DateColumnName),
          timeDiffTarget = NULL,
          timeAgg        = NULL,
          WindowingLag   = 1,
          Type           = "Lag",
          Timer          = FALSE,
          SimpleImpute   = TRUE,
          AscRowByGroup  = "ID",
          RecordsKeep    = 1
        )
        UpdateData <-
          data.table::rbindlist(list(UpdateData, Temporary), use.names = TRUE)
      }
      gc()
    }
    gc()
  }
  
  # Metrics----
  EvalMetric <-
    TestModel$EvaluationMetrics[Metric == "MAPE", MetricValue]
  
  # Define plot theme----
  Temp <- function () {
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(color = "#1c1c1c",
                                          size = 11),
      legend.background = ggplot2::element_rect(
        fill = "snow3",
        size = 0.25,
        colour = "darkblue"
      ),
      legend.justification = 0,
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = "#E7E7E7"),
      panel.background = ggplot2::element_rect(fill = "#E7E7E7"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "white"),
      panel.grid.minor.y = ggplot2::element_line(color = "white"),
      plot.title = ggplot2::element_text(
        color = "#1c1c1c",
        size = 25,
        hjust = 0,
        face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        color = "#1c1c1c",
        size = 14,
        hjust = 0
      ),
      plot.caption = ggplot2::element_text(
        size = 9,
        hjust = 0,
        face = "italic"
      )
    )
  }
  
  # Data Manipulation----
  if (!is.null(GroupVariables)) {
    PlotData <- data.table::copy(UpdateData)
    PlotData <- PlotData[, .(sum(get(TargetColumnName)),
                             sum(Predictions)),
                         by = eval(DateColumnName)]
    data.table::setnames(PlotData, c("V1", "V2"), c(eval(TargetColumnName), "Predictions"))
  } else {
    PlotData <- data.table::copy(UpdateData)
    data.table::set(
      PlotData,
      i = (data[, .N] + 1):PlotData[, .N],
      j = 2,
      value = NA
    )
    data.table::set(PlotData,
                    i = 1:data[, .N],
                    j = 3,
                    value = NA)
  }
  
  # Plot Time Series----
  TimeSeriesPlot <-
    ggplot2::ggplot(PlotData, ggplot2::aes(x = PlotData[[eval(DateColumnName)]])) +
    ggplot2::geom_line(ggplot2::aes(y = PlotData[[eval(TargetColumnName)]],
                                    color = "Actual")) +
    ggplot2::geom_line(ggplot2::aes(y = PlotData[["Predictions"]],
                                    color = "Forecast"))
  
  # Modify title----
  if (!is.null(GroupVariables)) {
    TimeSeriesPlot <- TimeSeriesPlot +
      ggplot2::geom_vline(
        xintercept = UpdateData[data[, .N, by = "GroupVar"][1, 2][[1]],
                                max(get(DateColumnName), na.rm = TRUE)],
        color = "#FF4F00",
        lty = "dotted",
        lwd = 1
      ) +
      #RemixTheme()
      Temp()
    TimeSeriesPlot <- TimeSeriesPlot +
      ggplot2::labs(
        title = paste0(
          FC_Periods,
          " - Period Forecast for Aggregate ",
          eval(TargetColumnName)
        ),
        subtitle = paste0(
          "Catboost Model: Mean Absolute Percentage Error = ",
          paste0(round(EvalMetric, 3) * 100, "%")
        ),
        caption = "Forecast generated by Remix Institute's RemixAutoML R package"
      ) +
      ggplot2::scale_colour_manual(
        "",
        breaks = c("Actual", "Forecast"),
        values = c("Actual" = "red", "Forecast" =
                     "blue")
      ) +
      ggplot2::xlab(eval(DateColumnName)) + ggplot2::ylab(eval(TargetColumnName))
  } else {
    TimeSeriesPlot <- TimeSeriesPlot +
      ggplot2::labs(
        title = paste0(FC_Periods, " - Period Forecast for ", eval(TargetColumnName)),
        subtitle = paste0(
          "Catboost Model: Mean Absolute Percentage Error = ",
          paste0(round(EvalMetric, 3) * 100, "%")
        ),
        caption = "Forecast generated by Remix Institute's RemixAutoML R package"
      ) +
      ggplot2::scale_colour_manual("",
                                   breaks = c(eval(TargetColumnName), "Forecast"),
                                   values = c("red", "blue")) +
      ggplot2::xlab(eval(DateColumnName)) + ggplot2::ylab(eval(TargetColumnName))
  }
  
  # Return data----
  if (!is.null(GroupVariables)) {
    # Variables to keep----
    keep <-
      c("GroupVar",
        eval(DateColumnName),
        eval(TargetColumnName),
        "Predictions")
    UpdateData <- UpdateData[, ..keep]
    UpdateData[, eval(GroupVariables) := data.table::tstrsplit(GroupVar, " ")][, GroupVar := NULL]
    return(
      list(
        Forecast = UpdateData,
        TimeSeriesPlot = TimeSeriesPlot,
        ModelInformation = TestModel
      )
    )
  } else {
    # Variables to keep----
    keep <- c(eval(DateColumnName), "Predictions")
    return(
      list(
        Forecast = PlotData[, ..keep],
        TimeSeriesPlot = TimeSeriesPlot,
        ModelInformation = TestModel
      )
    )
  }
}

#' An Automated Machine Learning Framework using H2O
#'
#' Steps in the function include:
#' See details below for information on using this function.
#'
#' 1. Logic: Error checking in the modeling arguments from your Construction file
#'
#' 2. ML: Build grid-tuned models and baseline models for comparison and checks which one performs better on validation data
#'
#' 3. Evaluation: Collects the performance metrics for both
#'
#' 4. Evaluation: Generates calibration plots (and boxplots for regression) for the winning model
#'
#' 5. Evaluation: Generates partial dependence calibration plots (and boxplots for regression) for the winning model
#'
#' 6. Evaluation: Generates variable importance tables and a table of non-important features
#'
#' 7. Production: Creates a storage file containing: model name, model path, grid tune performance, baseline performance, and threshold (if classification) and stores that file in your model_path location
#'
#' The Construct file must be a data.table and the columns need to be in the correct order (see examples). Character columns must be converted to type "Factor". You must remove date columns or convert them to "Factor". For classification models, your target variable needs to be a (0,1) of type "Factor." See the examples below for help with setting up the Construct file for various modeling target variable types. There are examples for regression, classification, multinomial, and quantile regression. For help on which parameters to use, look up the r/h2o documentation. If you misspecify the construct file, it will produce an error and outputfile of what was wrong and suggestions for fixing the error.
#'
#' Let's go over the construct file, column by column. The Targets column is where you specify the column number of your target variable (in quotes, e.g. "c(1)").
#'
#' The Distribution column is where you specify the distribution type for the modeling task. For classification use bernoulli, for multilabel use multinomial, for quantile use quantile, and for regression, you can choose from the list available in the H2O docs, such as gaussian, poisson, gamma, etc. It's not set up to handle tweedie distributions currently but I can add support if there is demand.
#'
#' The Loss column tells H2O which metric to use for the loss metrics. For regression, I typically use "mse", quantile regression, "mae", classification "auc", and multinomial "logloss". For deeplearning models, you need to use "quadratic", "absolute", and "crossentropy".
#'
#' The Quantile column tells H2O which quantile to use for quantile regression (in decimal form).
#'
#' The ModelName column is the name you wish to give your model as a prefix.
#'
#' The Algorithm column is the model you wish to use: gbm, randomForest, deeplearning, AutoML, XGBoost, LightGBM.
#'
#' The dataName column is the name of your data.
#'
#' The TargetCol column is the column number of your target variable.
#'
#' The FeatureCols column is the column numbers of your features.
#'
#' The CreateDate column is for tracking your model build dates.
#'
#' The GridTune column is a TRUE / FALSE column for whether you want to run a grid tune model for comparison.
#'
#' The ExportValidData column is a TRUE / FALSE column indicating if you want to export the validation data.
#'
#' The ParDep column is where you put the number of partial dependence calibration plots you wish to generate.
#'
#' The PD_Data column is where you specify if you want to generate the partial dependence plots on "All" data, "Validate" data, or "Train" data.
#'
#' The ThreshType column is for classification models. You can specify "f1", "f2", "f0point5", or "CS" for cost sentitive.
#'
#' The FSC column is the feature selection column. Specify the percentage importance cutoff to create a table of "unimportant" features.
#'
#' The tpProfit column is for when you specify "CS" in the ThreshType column. This is your true positive profit.
#'
#' The tnProfit column is for when you specify "CS" in the ThreshType column. This is your true negative profit.
#'
#' The fpProfit column is for when you specify "CS" in the ThreshType column. This is your false positive profit.
#'
#' The fnProfit column is for when you specify "CS" in the ThreshType column. This is your false negative profit.
#'
#' The SaveModel column is a TRUE / FALSE indicator. If you are just testing out models, set this to FALSE.
#'
#' The SaveModelType column is where you specify if you want a "standard" model object saveed or a "mojo" model object saved.
#'
#' The PredsAllData column is a TRUE / FALSE column. Set to TRUE if you want all the predicted values returns (for all data).
#'
#' The TargetEncoding column let's you specify the column number of features you wish to run target encoding on. Set to NA to not run this feature.
#'
#' The SupplyData column lets you supply the data names for training and validation data. Set to NULL if you want the data partitioning to be done internally.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param Construct Core instruction file for automation (see Details below for more information on this)
#' @param max_memory The ceiling amount of memory H2O will utilize
#' @param ratios The percentage of train samples from source data (remainder goes to validation set)
#' @param BL_Trees The number of trees to build in baseline GBM or RandomForest
#' @param nthreads Set the number of threads to run function
#' @param model_path Directory path for where you want your models saved
#' @param MaxRuntimeSeconds Number of seconds of run time for grid tuning
#' @param MaxModels Number of models you'd like to have returned
#' @param TrainData Set to NULL or supply a data.table for training data
#' @param TestData Set to NULL or supply  a data.table for validation data
#' @param SaveToFile Set to TRUE to save models and output to model_path
#' @param ReturnObjects Set to TRUE to return objects from functioin
#' @return Returns saved models, corrected Construct file, variable importance tables, evaluation and partial dependence calibration plots, model performance measure, and a file called grid_tuned_paths.Rdata which contains paths to your saved models for operationalization.
#' @examples
#' \donttest{
#' # Classification Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' aa[, target := as.factor(ifelse(target > 0.5,1,0))]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("bernoulli",
#'                                                         "bernoulli",
#'                                                         "bernoulli"),
#'                                     Loss            = c("AUC","AUC","CrossEntropy"),
#'                                     Quantile        = rep(NA,3),
#'                                     ModelName       = c("GBM","DRF","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "randomForest",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",3),
#'                                     TargetCol       = rep(c("1"),3),
#'                                     FeatureCols     = rep(c("2:11"),3),
#'                                     CreateDate      = rep(Sys.time(),3),
#'                                     GridTune        = rep(FALSE,3),
#'                                     ExportValidData = rep(TRUE,3),
#'                                     ParDep          = rep(2,3),
#'                                     PD_Data         = rep("All",3),
#'                                     ThreshType      = rep("f1",3),
#'                                     FSC             = rep(0.001,3),
#'                                     tpProfit        = rep(NA,3),
#'                                     tnProfit        = rep(NA,3),
#'                                     fpProfit        = rep(NA,3),
#'                                     fnProfit        = rep(NA,3),
#'                                     SaveModel       = rep(FALSE,3),
#'                                     SaveModelType   = c("Mojo","standard","mojo"),
#'                                     PredsAllData    = rep(TRUE,3),
#'                                     TargetEncoding  = rep(NA,3),
#'                                     SupplyData      = rep(FALSE,3))
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'
#' # Multinomial Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' aa[, target := as.factor(ifelse(target < 0.33,"A",ifelse(target < 0.66, "B","C")))]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("multinomial",
#'                                                         "multinomial",
#'                                                         "multinomial"),
#'                                     Loss            = c("auc","logloss","accuracy"),
#'                                     Quantile        = rep(NA,3),
#'                                     ModelName       = c("GBM","DRF","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "randomForest",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",3),
#'                                     TargetCol       = rep(c("1"),3),
#'                                     FeatureCols     = rep(c("2:11"),3),
#'                                     CreateDate      = rep(Sys.time(),3),
#'                                     GridTune        = rep(FALSE,3),
#'                                     ExportValidData = rep(TRUE,3),
#'                                     ParDep          = rep(NA,3),
#'                                     PD_Data         = rep("All",3),
#'                                     ThreshType      = rep("f1",3),
#'                                     FSC             = rep(0.001,3),
#'                                     tpProfit        = rep(NA,3),
#'                                     tnProfit        = rep(NA,3),
#'                                     fpProfit        = rep(NA,3),
#'                                     fnProfit        = rep(NA,3),
#'                                     SaveModel       = rep(FALSE,3),
#'                                     SaveModelType   = c("Mojo","standard","mojo"),
#'                                     PredsAllData    = rep(TRUE,3),
#'                                     TargetEncoding  = rep(NA,3),
#'                                     SupplyData      = rep(FALSE,3))
#'
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'
#' # Regression Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("gaussian",
#'                                                         "gaussian",
#'                                                         "gaussian"),
#'                                     Loss            = c("MSE","MSE","Quadratic"),
#'                                     Quantile        = rep(NA,3),
#'                                     ModelName       = c("GBM","DRF","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "randomForest",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",3),
#'                                     TargetCol       = rep(c("1"),3),
#'                                     FeatureCols     = rep(c("2:11"),3),
#'                                     CreateDate      = rep(Sys.time(),3),
#'                                     GridTune        = rep(FALSE,3),
#'                                     ExportValidData = rep(TRUE,3),
#'                                     ParDep          = rep(2,3),
#'                                     PD_Data         = rep("All",3),
#'                                     ThreshType      = rep("f1",3),
#'                                     FSC             = rep(0.001,3),
#'                                     tpProfit        = rep(NA,3),
#'                                     tnProfit        = rep(NA,3),
#'                                     fpProfit        = rep(NA,3),
#'                                     fnProfit        = rep(NA,3),
#'                                     SaveModel       = rep(FALSE,3),
#'                                     SaveModelType   = c("Mojo","standard","mojo"),
#'                                     PredsAllData    = rep(TRUE,3),
#'                                     TargetEncoding  = rep(NA,3),
#'                                     SupplyData      = rep(FALSE,3))
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'
#' # Quantile Regression Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("quantile",
#'                                                         "quantile"),
#'                                     Loss            = c("MAE","Absolute"),
#'                                     Quantile        = rep(0.75,2),
#'                                     ModelName       = c("GBM","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",2),
#'                                     TargetCol       = rep(c("1"),2),
#'                                     FeatureCols     = rep(c("2:11"),2),
#'                                     CreateDate      = rep(Sys.time(),2),
#'                                     GridTune        = rep(FALSE,2),
#'                                     ExportValidData = rep(TRUE,2),
#'                                     ParDep          = rep(4,2),
#'                                     PD_Data         = rep("All",2),
#'                                     ThreshType      = rep("f1",2),
#'                                     FSC             = rep(0.001,2),
#'                                     tpProfit        = rep(NA,2),
#'                                     tnProfit        = rep(NA,2),
#'                                     fpProfit        = rep(NA,2),
#'                                     fnProfit        = rep(NA,2),
#'                                     SaveModel       = rep(FALSE,2),
#'                                     SaveModelType   = c("Mojo","mojo"),
#'                                     PredsAllData    = rep(TRUE,2),
#'                                     TargetEncoding  = rep(NA,2),
#'                                     SupplyData      = rep(FALSE,2))
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'}
#' @export
AutoH2OModeler <- function(Construct,
                           max_memory        = "28G",
                           ratios            = 0.80,
                           BL_Trees          = 500,
                           nthreads          = 1,
                           model_path        = NULL,
                           MaxRuntimeSeconds = 3600,
                           MaxModels         = 30,
                           TrainData         = NULL,
                           TestData          = NULL,
                           SaveToFile        = FALSE,
                           ReturnObjects     = TRUE) {
  ######################################
  # Error handling and prevention
  ######################################
  
  # Handle the multinomial case
  for (i in as.integer(seq_len(nrow(Construct)))) {
    if (tolower(Construct[i, 2][[1]]) == "multinomial" &&
        tolower(Construct[i, 3][[1]]) == "accuracy" &&
        tolower(Construct[i, 6][[1]]) != "deeplearning") {
      multinomialMetric <- "accuracy"
      data.table::set(Construct,
                      i = i,
                      j = 3L,
                      value = "logloss")
    } else if (tolower(Construct[i, 2][[1]]) == "multinomial" &&
               tolower(Construct[i, 3][[1]]) == "auc" &&
               tolower(Construct[i, 6][[1]]) != "deeplearning") {
      multinomialMetric <- "auc"
      data.table::set(Construct,
                      i = i,
                      j = 3L,
                      value = "logloss")
    } else if (tolower(Construct[i, 2][[1]]) == "multinomial" &&
               tolower(Construct[i, 3][[1]]) == "accuracy" &&
               tolower(Construct[i, 6][[1]]) == "deeplearning") {
      multinomialMetric <- "accuracy"
      data.table::set(Construct,
                      i = i,
                      j = 3L,
                      value = "crossentropy")
    } else if (tolower(Construct[i, 2][[1]]) == "multinomial" &&
               tolower(Construct[i, 3][[1]]) == "auc" &&
               tolower(Construct[i, 6][[1]]) == "deeplearning") {
      multinomialMetric <- "auc"
      data.table::set(Construct,
                      i = i,
                      j = 3L,
                      value = "crossentropy")
    }
  }
  
  ErrorCollection <-
    data.table::data.table(Row = rep(-720, 10000),
                           Msg = "I like modeling")
  j <- 0
  for (i in as.integer(seq_len(nrow(Construct)))) {
    # Algorithm specific
    if (tolower(Construct[i, 6][[1]]) %in% c("gbm",
                                             "randomforest",
                                             "automl",
                                             "xgboost",
                                             "lightgbm")) {
      # GBM and RF loss functions existence
      if (!(
        tolower(Construct[i, 3][[1]]) %in% c(
          "auto",
          "deviance",
          "mse",
          "rmse",
          "mae",
          "rmsle",
          "accuracy",
          "auc",
          "lift_top_group",
          "misclassification",
          "mean_per_class_error",
          "logloss"
        )
      )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = i,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: AUTO | deviance |
              logloss | MSE | RMSE |
              MAE | RMSLE | AUC | lift_top_group |
              misclassification | mean_per_class_error"
            )
          )
        )
      } else {
        temp <- tolower(Construct[i, 3][[1]])
        lower <-
          c(
            "auto",
            "deviance",
            "logloss",
            "mse",
            "rmse",
            "accuracy",
            "mae",
            "rmsle",
            "auc",
            "lift_top_group",
            "misclassification",
            "mean_per_class_error"
          )
        proper <-
          c(
            "AUTO",
            "deviance",
            "logloss",
            "MSE",
            "RMSE",
            "ACCURACY",
            "MAE",
            "RMSLE",
            "AUC",
            "lift_top_group",
            "misclassification",
            "mean_per_class_error"
          )
        distMatch <-
          data.table::data.table(act = rep(temp, 12),
                                 LCVals = lower,
                                 Proper = proper)
        ReplaceValue <- distMatch[act == LCVals][["Proper"]][[1]]
        data.table::set(Construct, i, 3L, value = ReplaceValue)
      }
      
      # GBM and RF distributions
      if (!(
        tolower(Construct[i, 2][[1]]) %in% c(
          "auto",
          "bernoulli",
          "quasibinomial",
          "multinomial",
          "gaussian",
          "poisson",
          "gamma",
          "tweedie",
          "laplace",
          "quantile",
          "huber"
        )
      )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Distribution ",
              Construct[i, 2][[1]],
              " is not in list: AUTO | bernoulli | quasibinomial |
              multinomial | gaussian | poisson | gamma | tweedie |
              laplace | quantile | huber"
            )
          )
        )
      } else {
        temp <- tolower(Construct[i, 2][[1]])
        lower <-
          c(
            "auto",
            "bernoulli",
            "quasibinomial",
            "multinomial",
            "gaussian",
            "poisson",
            "gamma",
            "tweedie",
            "laplace",
            "quantile",
            "huber"
          )
        proper <-
          c(
            "AUTO",
            "bernoulli",
            "quasibinomial",
            "multinomial",
            "gaussian",
            "poisson",
            "gamma",
            "tweedie",
            "laplace",
            "quantile",
            "huber"
          )
        distMatch <-
          data.table::data.table(act = rep(temp, 11),
                                 LCVals = lower,
                                 Proper = proper)
        ReplaceValue2 <- distMatch[act == LCVals][["Proper"]][[1]]
        data.table::set(Construct, i, 2L, value = ReplaceValue2)
      }
      
      # Distribution and loss combos for non-regression
      if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial", "binomial",
                                               "bernoulli", "multinomial") &&
          !(
            tolower(Construct[i, 3][[1]]) %in% c(
              "auc",
              "logloss",
              "accuracy",
              "auto",
              "lift_top_group",
              "misclassification",
              "mean_per_class_error"
            )
          )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: AUC | logloss | AUTO | lift_top_group |
              misclassification | mean_per_class_error | accuracy"
            )
          )
        )
      }
      
      # Distribution and loss combos for regression
      if (tolower(Construct[i, 2][[1]]) %in% c("gaussian",
                                               "poisson",
                                               "gamma",
                                               "tweedie",
                                               "laplace",
                                               "quantile",
                                               "huber") &&
          !(tolower(Construct[i, 3][[1]]) %in% c("auto", "mse", "rmse",
                                                 "mae", "rmsle"))) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 2][[1]],
              " is not in list: AUTO | MSE | RMSE | MAE | RMSLE"
            )
          )
        )
      }
      
      # Quantile Regression with GBM
      if (tolower(Construct[i, 2][[1]]) %in% c("quantile") &&
          (Construct[i, 4][[1]] > 1 ||
           Construct[i, 4][[1]] < 0 ||
           !is.numeric(Construct[i, 4][[1]]))) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantiles using ",
              Construct[i, 6][[1]],
              " must be a number less than or equal to 1 AND greater
              than or equal to 0"
            )
          )
        )
      }
      
      # RF Quantile regression fail
      if (tolower(Construct[i, 6][[1]]) == "randomforest" &&
          tolower(Construct[i, 2][[1]]) == "quantile") {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantile regression is only supported by GBM and
              Deeplearning models, not ",
              Construct[i, 6][[1]],
              " models"
            )
          )
        )
      }
      
      # Quantile regression loss metrics
      if (tolower(Construct[i, 2][[1]]) == "quantile" &&
          tolower(Construct[i, 3][[1]]) != "mae") {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantile regression is best supported by MAE when using ",
              Construct[i, 6][[1]],
              " models"
            )
          )
        )
      }
      
      if (tolower(Construct[i, 6][[1]]) == "automl" &
          Construct[i, 11][[1]] != TRUE) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c("using automl requires GridTune = TRUE")
        )
      }
    } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
      # Deeplearning loss functions
      if (!(
        tolower(Construct[i, 3][[1]]) %in% c(
          "automatic",
          "crossentropy",
          "quadratic",
          "accuracy",
          "auc",
          "huber",
          "absolute",
          "quantile"
        )
      )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: Automatic | CrossEntropy | Quadratic |
              Huber | Absolute | Quantile | AUC | ACCURACY"
            )
          )
        )
      } else {
        temp <- tolower(Construct[i, 3][[1]])
        lower <-
          c(
            "automatic",
            "crossentropy",
            "quadratic",
            "auc",
            "accuracy",
            "huber",
            "absolute",
            "quantile"
          )
        proper <-
          c(
            "Automatic",
            "CrossEntropy",
            "Quadratic",
            "AUC",
            "ACCURACY",
            "Huber",
            "Absolute",
            "Quantile"
          )
        distMatch <-
          data.table::data.table(act = rep(temp, 8),
                                 LCVals = lower,
                                 Proper = proper)
        ReplaceVal <- distMatch[act == LCVals][["Proper"]][[1]]
        data.table::set(Construct, i, 3L, value = ReplaceVal)
      }
      
      # Deeplearning distributions
      if (!(
        tolower(Construct[i, 2][[1]]) %in% c(
          "auto",
          "bernoulli",
          "multinomial",
          "gaussian",
          "poisson",
          "gamma",
          "tweedie",
          "laplace",
          "quantile",
          "huber"
        )
      )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Distributions ",
              Construct[i, 2][[1]],
              " is not in list: AUTO | bernoulli | multinomial | gaussian |
              poisson | gamma | tweedie | laplace | quantile | huber"
            )
          )
        )
      } else {
        temp <- tolower(Construct[i, 2][[1]])
        lower <-
          c(
            "auto",
            "bernoulli",
            "multinomial",
            "gaussian",
            "poisson",
            "gamma",
            "tweedie",
            "laplace",
            "quantile",
            "huber"
          )
        proper <-
          c(
            "AUTO",
            "bernoulli",
            "multinomial",
            "gaussian",
            "poisson",
            "gamma",
            "tweedie",
            "laplace",
            "quantile",
            "huber"
          )
        distMatch <-
          data.table::data.table(act = rep(temp, 10),
                                 LCVals = lower,
                                 Proper = proper)
        ReplaceVal2 <- distMatch[act == LCVals][["Proper"]][[1]]
        data.table::set(Construct, i, 2L, value = ReplaceVal2)
      }
      
      # Distribution and loss combos for non-regression
      if (tolower(Construct[i, 2][[1]]) %in% c("bernoulli",
                                               "multinomial") &&
          !(tolower(Construct[i, 3][[1]]) %in% c("automatic",
                                                 "crossentropy",
                                                 "auc",
                                                 "accuracy"))) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: Automatic | CrossEntropy | AUC | ACCURACY"
            )
          )
        )
      }
      
      # Distribution and loss combos for regression
      if (tolower(Construct[i, 2][[1]]) %in% c("gaussian",
                                               "poisson",
                                               "gamma",
                                               "tweedie",
                                               "laplace",
                                               "quantile",
                                               "huber") &&
          !(
            tolower(Construct[i, 3][[1]]) %in% c(
              "automatic",
              "quadratic",
              "huber",
              "absolute",
              "quantile"
            )
          )) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Loss function ",
              Construct[i, 3][[1]],
              " is not in list: Automatic | Quadratic
              | Huber | Absolute | Quantile"
            )
          )
        )
      }
      
      # Quantile regression loss metrics
      if (tolower(Construct[i, 2][[1]]) == "quantile" &&
          tolower(Construct[i, 3][[1]]) != "quantile") {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantile regression needs to use
              Quantile for the loss function with ",
              Construct[i, 6][[1]],
              " models"
            )
          )
        )
      }
      
      # Quantile Regression with DL
      if (tolower(Construct[i, 2][[1]]) %in% c("quantile") &&
          (Construct[i, 4][[1]] > 1 ||
           Construct[i, 4][[1]] < 0 ||
           !is.numeric(Construct[i, 4][[1]]))) {
        j <- j + 1
        data.table::set(ErrorCollection,
                        i = j,
                        j = 1L,
                        value = i)
        data.table::set(
          ErrorCollection,
          i = j,
          j = 2L,
          value = c(
            paste0(
              "Quantiles using ",
              Construct[i, 6][[1]],
              " must be a number less than or equal to
              1 AND greater than or equal to 0"
            )
          )
        )
      }
      
    } else {
      j <- j + 1
      data.table::set(ErrorCollection,
                      i = j,
                      j = 1L,
                      value = i)
      data.table::set(
        ErrorCollection,
        i = j,
        j = 2L,
        value = c(
          paste0(
            "Models supported are: GBM, randomForest,
            and deeplearning, while ",
            Construct[i, 6][[1]],
            " is not"
          )
        )
      )
    }
  }
  
  # Error stopping point and Construct file save
  ErrorCollection <- ErrorCollection[Row != -720]
  if (nrow(ErrorCollection) >= 1) {
    ErrorCollectionLog <- ErrorCollection
    if (SaveToFile == TRUE & ReturnObjects == TRUE) {
      message(
        "Your model construction file has errors and
        an error log has been returned and saved to model_path"
      )
      save(ErrorCollectionLog,
           file = paste0(model_path, "/ErrorCollectionLog.Rdata"))
      return(ErrorCollectionLog)
    } else if (SaveToFile == TRUE & ReturnObjects == FALSE) {
      save(ErrorCollectionLog,
           file = paste0(model_path, "/ErrorCollectionLog.Rdata"))
      warning(
        "Your model construction file has errors and
            an error log has been saved to your model_path"
      )
    } else if (SaveToFile == FALSE & ReturnObjects == TRUE) {
      message("Your model construction file has errors and
        an error log has
        been returned")
      return(ErrorCollectionLog)
    } else {
      warning(
        "Your model construction file has errors and
        an error log errors. Set ReturnObjects to TRUE to see ErrorLog"
      )
    }
  }
  
  # Clear table
  rm(distMatch)
  
  # Set up grid_tuned_paths.R file
  grid_tuned_paths <-
    data.table::data.table(
      Model     = rep("a", nrow(Construct)),
      Path      = rep("a", nrow(Construct)),
      GT_Metric = rep(1234.5678, nrow(Construct)),
      BL_Metric = rep(1234.5678, nrow(Construct)),
      BinThresh = rep(1234.5678, nrow(Construct)),
      PathJar   = rep("a", nrow(Construct))
    )
  
  ######################################
  # Loop through model building
  ######################################
  
  tryCatch({
    for (i in as.integer(seq_len(nrow(Construct)))) {
      # No deeplearning loss functions as stopping metrics
      if (tolower(Construct[i, 3][[1]]) == "crossentropy") {
        if (tolower(Construct[i, 2][[1]]) == "multinomial") {
          StoppingMetric <- "logloss"
        } else {
          StoppingMetric <- "auc"
        }
      } else {
        if (tolower(Construct[i, 3][[1]]) %in% c("quadratic",
                                                 "huber")) {
          StoppingMetric <- "mse"
        } else if (tolower(Construct[i, 3][[1]]) %in% c("absolute",
                                                        "quantile")) {
          StoppingMetric <- "mae"
        } else {
          StoppingMetric <- Construct[i, 3][[1]]
        }
      }
      
      # Define grid tune search scheme in a named list
      search_criteria  <-
        list(
          strategy             = "RandomDiscrete",
          max_runtime_secs     = MaxRuntimeSeconds,
          max_models           = MaxModels,
          seed                 = 1234,
          stopping_rounds      = 10,
          stopping_metric      = StoppingMetric,
          stopping_tolerance   = 1e-3
        )
      
      # Set up H2O environment instance
      Sys.sleep(10)
      h2o::h2o.init(
        nthreads = nthreads,
        max_mem_size = max_memory,
        enable_assertions = FALSE
      )
      
      # Define data sets
      if (Construct[i, "SupplyData"][[1]]) {
        train        <- h2o::as.h2o(TrainData)
        validate     <- h2o::as.h2o(TestData)
        data_h2o     <-
          h2o::as.h2o(data.table::rbindlist(list(TrainData,
                                                 TestData)))
      } else {
        data_h2o     <-
          eval(parse(text = paste0("h2o::as.h2o(",
                                   Construct[i, 7][[1]], ")")))
        data_train   <- h2o::h2o.splitFrame(data_h2o,
                                            ratios = ratios)
        train        <- data_train[[1]]
        validate     <- data_train[[2]]
      }
      
      # Define targets
      target         <-
        eval(parse(text = paste0(Construct[i, 8][[1]])))
      features       <-
        eval(parse(text = paste0(Construct[i, 9][[1]])))
      XGB            <- h2o::h2o.xgboost.available()
      if (XGB) {
        if (tolower(Construct[i, 2][[1]]) != "quantile") {
          ModelExclude   <- NULL
        } else {
          ModelExclude   <- c("XGBoost", "GLM", "DRF")
        }
      } else {
        if (tolower(Construct[i, 2][[1]]) != "quantile") {
          ModelExclude   <- c("XGBoost")
        } else {
          ModelExclude   <- c("XGBoost", "GLM", "DRF")
        }
      }
      
      if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
        N              <- length(features)
        P5             <- 2 ^ (-1 / 5)
        P4             <- 2 ^ (-1 / 4)
        P3             <- 2 ^ (-1 / 3)
      }
      data.table::set(grid_tuned_paths,
                      i = i,
                      j = 1L,
                      value = Construct[i, 5][[1]])
      
      ######################################
      # Target Encoding
      ######################################
      
      if (!is.na(Construct[i, "TargetEncoding"][[1]])) {
        TEncode <- eval(parse(text = Construct[i, "TargetEncoding"][[1]]))
        cols <- names(train)[TEncode]
        train[, Construct[i, "Targets"][[1]]] <-
          as.numeric(train[, Construct[i, "Targets"][[1]]])
        validate[, Construct[i, "Targets"][[1]]] <-
          as.numeric(validate[, Construct[i, "Targets"][[1]]])
        for (col in cols) {
          x     <- h2o::h2o.target_encode_create(data = train,
                                                 x = list(col),
                                                 y = Construct[i, "Targets"][[1]])
          # Apply to training data
          train <- h2o::h2o.target_encode_apply(
            train,
            x = list(col),
            y = Construct[i, "Targets"][[1]],
            target_encode_map = x,
            holdout_type = "None",
            blended_avg = TRUE,
            noise_level = 0
          )
          
          # Apply to validation data
          validate <- h2o::h2o.target_encode_apply(
            validate,
            x = list(col),
            y = Construct[i, "Targets"][[1]],
            target_encode_map = x,
            holdout_type = "None",
            blended_avg = TRUE,
            noise_level = 0
          )
          
          if (SaveToFile == TRUE) {
            save(x,
                 file = paste0(model_path,
                               "/" ,
                               Construct[i, "Targets"][[1]],
                               "_", col,
                               ".Rdata"))
          }
        }
        
        # Modify feature reference
        features <-
          c((min(features) + length(eval(
            parse(text = paste0(Construct[i, 24][[1]]))
          ))):max(features),
          (max(target) + 1):(max(target) +
                               length(eval(
                                 parse(text = paste0(Construct[i, 24][[1]]))
                               ))))
        
        # Turn target columns back to factor
        train[, Construct[i, "Targets"][[1]]] <-
          as.factor(train[, Construct[i, "Targets"][[1]]])
        validate[, Construct[i, "Targets"][[1]]] <-
          as.factor(validate[, Construct[i, "Targets"][[1]]])
        data.table::set(Construct,
                        i = i,
                        j = "PD_Data",
                        value = "Validate")
      }
      
      ######################################
      # Hyperparameters
      ######################################
      
      if (Construct[i, 11][[1]]) {
        if (tolower(Construct[i, 6][[1]]) == "gbm") {
          if (tolower(
            Construct[i, 3][[1]] %in% c(
              "auc",
              "logloss",
              "auto",
              "lift_top_group",
              "misclassification",
              "mean_per_class_error"
            )
          )) {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              balance_classes                  = c(TRUE, FALSE),
              ntrees                           = c(500, 750, 1000),
              sample_rate                      = seq(0.5, 1, 0.01),
              col_sample_rate                  = seq(0.2, 1, 0.01),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
              col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              nbins                            = 2 ^ seq(4, 10, 1),
              nbins_cats                       = 2 ^ seq(4, 12, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4),
              histogram_type                   = c(
                "UniformAdaptive",
                "QuantilesGlobal",
                "RoundRobin"
              )
            )
          } else {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              ntrees                           = c(500, 750, 1000),
              sample_rate                      = seq(0.5, 1, 0.01),
              col_sample_rate                  = seq(0.2, 1, 0.01),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
              col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              nbins                            = 2 ^ seq(4, 10, 1),
              nbins_cats                       = 2 ^ seq(4, 12, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4),
              histogram_type                   = c(
                "UniformAdaptive",
                "QuantilesGlobal",
                "RoundRobin"
              )
            )
          }
          
        } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
          if (tolower(Construct[i, 3][[1]] %in% c("automatic",
                                                  "crossentropy"))) {
            hyper_params <-
              list(
                activation = c(
                  "Rectifier",
                  "Maxout",
                  "Tanh",
                  "RectifierWithDropout",
                  "MaxoutWithDropout",
                  "TanhWithDropout"
                ),
                hidden              = list(
                  c(
                    floor(N * P5),
                    floor(N * P5 * P5),
                    floor(N * P5 * P5 * P5),
                    floor(N * P5 * P5 * P5 * P5),
                    floor(N * P5 * P5 * P5 * P5 * P5)
                  ),
                  c(
                    floor(N * P4),
                    floor(N * P4 * P4),
                    floor(N * P4 * P4 * P4),
                    floor(N * P4 * P4 * P4 * P4)
                  ),
                  c(floor(N * P3), floor(N *
                                           P3 * P3),
                    floor(N * P3 * P3 * P3))
                ),
                balance_classes     = c(TRUE, FALSE),
                epochs              = c(50, 100, 200),
                l1                  = c(0, 0.00001, 0.0001),
                l2                  = c(0, 0.00001, 0.0001),
                rate                = c(0, 0.01, 0.005, 0.001),
                rate_annealing      = c(1e-8, 1e-7, 1e-6),
                rho                 = c(0.9, 0.95, 0.99, 0.999),
                epsilon             = c(1e-10, 1e-8, 1e-6, 1e-4),
                momentum_start      = c(0, 0.5),
                momentum_stable     = c(0.99, 0.5, 0),
                input_dropout_ratio = c(0, 0.1, 0.2),
                max_w2              = c(10, 100, 1000, 3.4028235e+38)
              )
          } else {
            hyper_params <-
              list(
                activation = c(
                  "Rectifier",
                  "Maxout",
                  "Tanh",
                  "RectifierWithDropout",
                  "MaxoutWithDropout",
                  "TanhWithDropout"
                ),
                hidden              = list(
                  c(
                    floor(N * P5),
                    floor(N * P5 * P5),
                    floor(N * P5 * P5 * P5),
                    floor(N * P5 * P5 * P5 * P5),
                    floor(N * P5 * P5 * P5 * P5 * P5)
                  ),
                  c(
                    floor(N * P4),
                    floor(N * P4 * P4),
                    floor(N * P4 * P4 * P4),
                    floor(N * P4 * P4 * P4 * P4)
                  ),
                  c(floor(N * P3), floor(N *
                                           P3 * P3),
                    floor(N * P3 * P3 * P3))
                ),
                epochs              = c(50, 100, 200),
                l1                  = c(0, 0.00001, 0.0001),
                l2                  = c(0, 0.00001, 0.0001),
                rate                = c(0, 0.01, 0.005, 0.001),
                rate_annealing      = c(1e-8, 1e-7, 1e-6),
                rho                 = c(0.9, 0.95, 0.99, 0.999),
                epsilon             = c(1e-10, 1e-8, 1e-6, 1e-4),
                momentum_start      = c(0, 0.5),
                momentum_stable     = c(0.99, 0.5, 0),
                input_dropout_ratio = c(0, 0.1, 0.2),
                max_w2              = c(10, 100, 1000, 3.4028235e+38)
              )
          }
        } else if (tolower(Construct[i, 6][[1]]) == "randomforest") {
          if (tolower(
            Construct[i, 3][[1]] %in% c(
              "auc",
              "logloss",
              "auto",
              "lift_top_group",
              "misclassification",
              "mean_per_class_error"
            )
          )) {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              balance_classes                  = c(TRUE, FALSE),
              ntrees                           = c(500, 750, 1000),
              mtries                           = -1,
              sample_rate                      = seq(0.2, 1, 0.05),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.05),
              col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              nbins                            = 2 ^ seq(4, 10, 1),
              nbins_cats                       = 2 ^ seq(4, 12, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4),
              histogram_type                   = c(
                "UniformAdaptive",
                "QuantilesGlobal",
                "RoundRobin"
              )
            )
          } else {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              ntrees                           = c(500, 750, 1000),
              mtries                           = -1,
              sample_rate                      = seq(0.2, 1, 0.05),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.05),
              col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              nbins                            = 2 ^ seq(4, 10, 1),
              nbins_cats                       = 2 ^ seq(4, 12, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4),
              histogram_type                   = c(
                "UniformAdaptive",
                "QuantilesGlobal",
                "RoundRobin"
              )
            )
          }
        } else if (tolower(Construct[i, 6][[1]]) == "automl") {
          message("automl is preset with tuning parameters")
        } else if (tolower(Construct[i, 6][[1]]) == "xgboost") {
          if (tolower(
            Construct[i, 3][[1]] %in% c(
              "auc",
              "logloss",
              "auto",
              "lift_top_group",
              "misclassification",
              "mean_per_class_error"
            )
          )) {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              tree_method                      = c("hist", "AUTO"),
              grow_policy                      = c("lossguide", "depthwise"),
              balance_classes                  = c(TRUE, FALSE),
              ntrees                           = c(500, 750, 1000),
              sample_rate                      = seq(0.5, 1, 0.01),
              col_sample_rate                  = seq(0.2, 1, 0.01),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
              reg_lambda                       = c(0.001, 0.01, 0.05),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4)
            )
          } else {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              ntrees                           = c(500, 750, 1000),
              sample_rate                      = seq(0.5, 1, 0.01),
              col_sample_rate                  = seq(0.2, 1, 0.01),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
              reg_lambda                       = c(0.001, 0.01, 0.05),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4)
            )
          }
        } else if (tolower(Construct[i, 6][[1]]) == "lightgbm") {
          if (tolower(
            Construct[i, 3][[1]] %in% c(
              "auc",
              "logloss",
              "auto",
              "lift_top_group",
              "misclassification",
              "mean_per_class_error"
            )
          )) {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              tree_method                      = c("hist"),
              grow_policy                      = c("lossguide"),
              balance_classes                  = c(TRUE, FALSE),
              ntrees                           = c(500, 750, 1000),
              sample_rate                      = seq(0.5, 1, 0.01),
              col_sample_rate                  = seq(0.2, 1, 0.01),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
              reg_lambda                       = c(0.001, 0.01, 0.05),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4)
            )
          } else {
            hyper_params <- list(
              max_depth                        = seq(5, 8, 1),
              ntrees                           = c(500, 750, 1000),
              sample_rate                      = seq(0.5, 1, 0.01),
              col_sample_rate                  = seq(0.2, 1, 0.01),
              col_sample_rate_per_tree         = seq(0.2, 1, 0.01),
              reg_lambda                       = c(0.001, 0.01, 0.05),
              min_rows                         = 2 ^ seq(0, log2(eval(
                parse(text = paste0("nrow(", Construct[i, 7][[1]], ")"))
              ) * ratios[1]) - 1, 1),
              min_split_improvement            = c(0, 1e-8, 1e-6, 1e-4)
            )
          }
        }
      }
      
      ######################################
      # Grid Tune Models
      ######################################
      
      # Check to see if GridTune is TRUE
      # Check to see if Distribution is quantile
      # Select model
      
      # Grid tuned model build
      if (Construct[i, 11][[1]]) {
        if (tolower(Construct[i, 2][[1]]) == "quantile") {
          if (tolower(Construct[i, 6][[1]]) == "gbm") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              distribution         = Construct[i, 2][[1]],
              quantile_alpha       = Construct[i, 4][[1]],
              learn_rate           = 0.05,
              learn_rate_annealing = 0.99,
              max_runtime_secs     = MaxRuntimeSeconds,
              stopping_rounds      = 5,
              stopping_tolerance   = 1e-4,
              stopping_metric      = StoppingMetric,
              score_tree_interval  = 10,
              seed                 = 1234
            )
          } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              distribution         = Construct[i, 2][[1]],
              quantile_alpha       = Construct[i, 4][[1]],
              seed                 = 42
            )
          }
        } else {
          if (tolower(Construct[i, 6][[1]]) == "gbm") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              distribution         = Construct[i, 2][[1]],
              learn_rate           = 0.05,
              learn_rate_annealing = 0.99,
              max_runtime_secs     = MaxRuntimeSeconds,
              stopping_rounds      = 5,
              stopping_tolerance   = 1e-4,
              stopping_metric      = StoppingMetric,
              score_tree_interval  = 10,
              seed                 = 1234
            )
          } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              distribution         = Construct[i, 2][[1]],
              seed                 = 42
            )
          } else if (tolower(Construct[i, 6][[1]]) == "randomforest") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              max_runtime_secs     = MaxRuntimeSeconds,
              stopping_rounds      = 5,
              stopping_tolerance   = 1e-4,
              stopping_metric      = StoppingMetric,
              score_tree_interval  = 10,
              seed                 = 1234
            )
          } else if (tolower(Construct[i, 6][[1]]) == "automl") {
            aml <- h2o::h2o.automl(
              x                  = features,
              y                  = target,
              training_frame     = train,
              validation_frame   = validate,
              max_models         = MaxModels,
              max_runtime_secs   = MaxRuntimeSeconds,
              stopping_metric    = StoppingMetric,
              stopping_tolerance = 1e-4,
              stopping_rounds    = 10,
              project_name       = "TestAML",
              exclude_algos      = ModelExclude,
              sort_metric        = StoppingMetric
            )
          } else if (tolower(Construct[i, 6][[1]]) == "xgboost") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              categorical_encoding = "Enum",
              distribution         = Construct[i, 2][[1]],
              learn_rate           = 0.05,
              max_runtime_secs     = MaxRuntimeSeconds,
              stopping_rounds      = 5,
              stopping_tolerance   = 1e-4,
              stopping_metric      = StoppingMetric,
              score_tree_interval  = 10,
              seed                 = 1234
            )
          } else if (tolower(Construct[i, 6][[1]]) == "lightgbm") {
            grid <- h2o::h2o.grid(
              hyper_params         = hyper_params,
              search_criteria      = search_criteria,
              algorithm            = Construct[i, 6][[1]],
              grid_id              = Construct[i, 5][[1]],
              x                    = features,
              y                    = target,
              training_frame       = train,
              validation_frame     = validate,
              categorical_encoding = "Enum",
              distribution         = Construct[i, 2][[1]],
              learn_rate           = 0.05,
              max_runtime_secs     = MaxRuntimeSeconds,
              stopping_rounds      = 5,
              stopping_tolerance   = 1e-4,
              stopping_metric      = StoppingMetric,
              score_tree_interval  = 10,
              seed                 = 1234
            )
          }
        }
        
        # Store all models built sorted by metric
        if (tolower(Construct[i, 6][[1]]) == "automl") {
          Grid_Out <- h2o::h2o.getAutoML(project_name = "TestAML")
        } else if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                        "binomial",
                                                        "bernoulli",
                                                        "multinomial")) {
          Decreasing <- TRUE
          Grid_Out   <-
            h2o::h2o.getGrid(
              grid_id = Construct[i, 5][[1]],
              sort_by = StoppingMetric,
              decreasing = Decreasing
            )
        } else {
          Decreasing <- FALSE
          Grid_Out   <-
            h2o::h2o.getGrid(
              grid_id = Construct[i, 5][[1]],
              sort_by = StoppingMetric,
              decreasing = Decreasing
            )
        }
        
        # Store best model
        if (tolower(Construct[i, 6][[1]]) == "automl") {
          best_model <- Grid_Out@leader
        } else {
          best_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1]])
        }
        
        # Collect accuracy metric on validation data
        if (tolower(Construct[i, 3][[1]]) == "crossentropy") {
          if (tolower(Construct[i, 2][[1]]) == "multinomial") {
            cc <-
              h2o::h2o.logloss(h2o::h2o.performance(best_model, valid = TRUE))
          } else {
            cc <- h2o::h2o.auc(h2o::h2o.performance(best_model, valid = TRUE))
          }
        } else if (tolower(Construct[i, 3][[1]]) == "absolute") {
          cc <- h2o::h2o.mae(h2o::h2o.performance(best_model, valid = TRUE))
        } else if (tolower(Construct[i, 3][[1]]) %in% c("quadratic", "huber")) {
          cc <- h2o::h2o.mse(h2o::h2o.performance(best_model, valid = TRUE))
        } else {
          cc <-
            eval(parse(
              text = paste0(
                "h2o::h2o.",
                tolower(StoppingMetric),
                "(h2o::h2o.performance(best_model, valid = TRUE))"
              )
            ))
        }
        # Store results in metadata file
        data.table::set(grid_tuned_paths,
                        i = i,
                        j = 3L,
                        value = cc)
      }
      
      ######################################
      # Baseline Models
      ######################################
      
      # Check to see if quantile is selected
      # Choose model
      if (tolower(Construct[i, 6][[1]]) != "automl") {
        if (tolower(Construct[i, 2][[1]]) == "quantile") {
          if (tolower(Construct[i, 6][[1]]) == "gbm") {
            bl_model <- h2o::h2o.gbm(
              x                = features,
              y                = target,
              training_frame   = train,
              validation_frame = validate,
              distribution     = Construct[i, 2][[1]],
              quantile_alpha   = Construct[i, 4][[1]],
              model_id         = paste0("BL_GBM_", Construct[i, 5][[1]]),
              ntrees           = BL_Trees
            )
          } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
            bl_model <- h2o::h2o.deeplearning(
              x                = features,
              y                = target,
              hidden           = c(
                floor(N * P4),
                floor(N * P4 * P4),
                floor(N * P4 * P4 * P4),
                floor(N * P4 * P4 * P4 * P4)
              ),
              training_frame   = train,
              validation_frame = validate,
              distribution     = Construct[i, 2][[1]],
              model_id         = paste0("BL_DL_", Construct[i, 5][[1]]),
              quantile_alpha   = Construct[i, 4][[1]]
            )
          }
        } else if (tolower(Construct[i, 6][[1]]) == "gbm") {
          bl_model <- h2o::h2o.gbm(
            x                = features,
            y                = target,
            training_frame   = train,
            validation_frame = validate,
            distribution     = Construct[i, 2][[1]],
            model_id         = paste0("BL_GBM_", Construct[i, 5][[1]]),
            ntrees           = BL_Trees
          )
        } else if (tolower(Construct[i, 6][[1]]) == "deeplearning") {
          bl_model <- h2o::h2o.deeplearning(
            x                = features,
            y                = target,
            hidden           = c(
              floor(N * P4),
              floor(N * P4 * P4),
              floor(N * P4 * P4 * P4),
              floor(N * P4 * P4 * P4 * P4)
            ),
            training_frame   = train,
            validation_frame = validate,
            model_id         = paste0("BL_DL_", Construct[i, 5][[1]]),
            distribution     = Construct[i, 2][[1]]
          )
        } else if (tolower(Construct[i, 6][[1]]) == "randomforest") {
          bl_model <- h2o::h2o.randomForest(
            x                = features,
            y                = target,
            training_frame   = train,
            validation_frame = validate,
            model_id         = paste0("BL_RF_", Construct[i, 5][[1]]),
            ntrees           = BL_Trees
          )
        } else if (tolower(Construct[i, 6][[1]]) == "xgboost") {
          bl_model <- h2o::h2o.xgboost(
            x                = features,
            y                = target,
            training_frame   = train,
            validation_frame = validate,
            categorical_encoding = "Enum",
            distribution     = Construct[i, 2][[1]],
            model_id         = paste0("BL_XG_", Construct[i, 5][[1]]),
            ntrees           = BL_Trees
          )
        } else if (tolower(Construct[i, 6][[1]]) == "lightgbm") {
          bl_model <- h2o::h2o.xgboost(
            x                = features,
            y                = target,
            training_frame   = train,
            validation_frame = validate,
            categorical_encoding = "Enum",
            distribution     = Construct[i, 2][[1]],
            tree_method      = "hist",
            grow_policy      = "lossguide",
            model_id         = paste0("BL_lgbm_", Construct[i, 5][[1]]),
            ntrees           = BL_Trees
          )
        }
        
        # Collect accuracy metric on validation data
        if (tolower(Construct[i, 3][[1]]) == "crossentropy") {
          if (tolower(Construct[i, 2][[1]]) == "multinomial") {
            dd <- h2o::h2o.logloss(h2o::h2o.performance(bl_model, valid = TRUE))
          } else {
            dd <- h2o::h2o.auc(h2o::h2o.performance(bl_model, valid = TRUE))
          }
        } else if (tolower(Construct[i, 3][[1]]) == "absolute") {
          dd <- h2o::h2o.mae(h2o::h2o.performance(bl_model, valid = TRUE))
        } else if (tolower(Construct[i, 3][[1]]) %in% c("quadratic", "huber")) {
          dd <- h2o::h2o.mse(h2o::h2o.performance(bl_model, valid = TRUE))
        } else {
          dd <-
            eval(parse(
              text = paste0(
                "h2o::h2o.",
                tolower(StoppingMetric),
                "(h2o::h2o.performance(bl_model, valid = TRUE))"
              )
            ))
        }
        
        # Store results in metadata file
        data.table::set(grid_tuned_paths,
                        i = i,
                        j = 4L,
                        value = dd)
      }
      
      ######################################
      # Model Evaluation & Saving
      ######################################
      
      # Check to see if GridTune is TRUE
      # Check to see if Distribution is multinomial
      # Proceed
      
      if (tolower(Construct[i, 6][[1]]) == "automl") {
        if (Construct[i, 21][[1]] == TRUE) {
          if (grid_tuned_paths[i, 2][[1]] != "a")
            file.remove(grid_tuned_paths[i, 2][[1]])
          if (tolower(Construct[i, 22][[1]]) == "standard") {
            save_model <-
              h2o::h2o.saveModel(object = best_model,
                                 path = model_path,
                                 force = TRUE)
            data.table::set(
              grid_tuned_paths,
              i = i,
              j = 2L,
              value = save_model
            )
            if (SaveToFile == TRUE) {
              save(grid_tuned_paths,
                   file = paste0(model_path, "/grid_tuned_paths.Rdata"))
            }
          } else {
            save_model <-
              h2o::h2o.saveMojo(object = best_model,
                                path = model_path,
                                force = TRUE)
            h2o::h2o.download_mojo(
              model = best_model,
              path = model_path,
              get_genmodel_jar = TRUE,
              genmodel_path = model_path,
              genmodel_name = Construct[i, 5][[1]]
            )
            data.table::set(
              grid_tuned_paths,
              i = i,
              j = 2L,
              value = save_model
            )
            data.table::set(
              grid_tuned_paths,
              i = i,
              j = 6L,
              value = paste0(model_path, "\\", Construct[i, 5][[1]])
            )
            if (SaveToFile == TRUE) {
              save(grid_tuned_paths,
                   file = paste0(model_path, "/grid_tuned_paths.Rdata"))
            }
          }
        }
        
        # Save VarImp and VarNOTImp
        if (best_model@algorithm != "stackedensemble") {
          VIMP <- data.table::as.data.table(h2o::h2o.varimp(best_model))
          if (SaveToFile == TRUE) {
            save(VIMP,
                 file = paste0(model_path,
                               "/VarImp_",
                               Construct[i, 5][[1]],
                               ".Rdata"))
          }
          if (tolower(best_model@algorithm) != "glm") {
            NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
          } else {
            NIF <- NULL
          }
          if (length(NIF) > 0) {
            if (SaveToFile == TRUE) {
              save(NIF,
                   file = paste0(
                     model_path,
                     "/VarNOTImp_",
                     Construct[i, 5][[1]],
                     ".Rdata"
                   ))
            }
          }
        } else {
          data.table::set(Construct,
                          i = i,
                          j = 13L,
                          value = 0)
        }
        
        # Gather predicted values
        preds <-
          h2o::h2o.predict(best_model, newdata = validate)[, 1]
        if (Construct[i, 14][[1]] == "All") {
          predsPD <- h2o::h2o.predict(best_model, newdata = data_h2o)[, 1]
          PredsPD <- data.table::as.data.table(predsPD)
          if (SaveToFile == TRUE) {
            data.table::fwrite(PredsPD,
                               file = paste0(model_path,
                                             "/",
                                             Construct[i, 5][[1]],
                                             "_PredsAll.csv"))
          }
        } else if (Construct[i, 14][[1]] == "Train") {
          predsPD <- h2o::h2o.predict(best_model, newdata = train)[, 1]
        } else if (Construct[i, 14][[1]] == "Validate") {
          predsPD <- h2o::h2o.predict(best_model, newdata = validate)[, 1]
        }
      }
      
      if (Construct[i, 11][[1]] == TRUE &
          tolower(Construct[i, 6][[1]]) != "automl") {
        if (!(tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                   "binomial",
                                                   "bernoulli")) |
            tolower(Construct[i, 3][[1]]) == "logloss") {
          if (cc < dd) {
            # Save model
            if (Construct[i, 21][[1]] == TRUE) {
              if (grid_tuned_paths[i, 2][[1]] != "a")
                file.remove(grid_tuned_paths[i, 2][[1]])
              if (tolower(Construct[i, 22][[1]]) == "standard") {
                save_model <-
                  h2o::h2o.saveModel(object = best_model,
                                     path = model_path,
                                     force = TRUE)
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 2L,
                  value = save_model
                )
                if (SaveToFile == TRUE) {
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              } else {
                save_model <-
                  h2o::h2o.saveMojo(object = best_model,
                                    path = model_path,
                                    force = TRUE)
                h2o::h2o.download_mojo(
                  model = best_model,
                  path = model_path,
                  get_genmodel_jar = TRUE,
                  genmodel_path = model_path,
                  genmodel_name = Construct[i, 5][[1]]
                )
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 2L,
                  value = save_model
                )
                data.table::set(
                  grid_tuned_paths,
                  i = i,
                  j = 6L,
                  value = paste0(model_path, "\\", Construct[i, 5][[1]])
                )
                if (SaveToFile == TRUE) {
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              }
            }
            
            # Save VarImp and VarNOTImp
            VIMP <-
              data.table::as.data.table(h2o::h2o.varimp(best_model))
            if (SaveToFile == TRUE) {
              save(VIMP,
                   file = paste0(model_path,
                                 "/VarImp_",
                                 Construct[i, 5][[1]],
                                 ".Rdata"))
            }
            NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
            if (length(NIF) > 0) {
              if (SaveToFile == TRUE) {
                save(NIF,
                     file = paste0(
                       model_path,
                       "/VarNOTImp_",
                       Construct[i, 5][[1]],
                       ".Rdata"
                     ))
              }
            }
            
            # Gather predicted values
            preds <-
              h2o::h2o.predict(best_model, newdata = validate)[, 1]
            if (Construct[i, 14][[1]] == "All") {
              predsPD <- h2o::h2o.predict(best_model, newdata = data_h2o)[, 1]
              PredsPD <- as.data.table(predsPD)
              if (SaveToFile == TRUE) {
                data.table::fwrite(PredsPD,
                                   file = paste0(model_path,
                                                 "/",
                                                 Construct[i, 5][[1]],
                                                 "_PredsAll.csv"))
              }
            } else if (Construct[i, 14][[1]] == "Train") {
              predsPD <- h2o::h2o.predict(best_model, newdata = train)[, 1]
            } else if (Construct[i, 14][[1]] == "Validate") {
              predsPD <- h2o::h2o.predict(best_model, newdata = validate)[, 1]
            }
          } else {
            # Save model
            if (Construct[i, 21][[1]] == TRUE) {
              if (grid_tuned_paths[i, 2][[1]] != "a")
                if (SaveToFile == TRUE) {
                  file.remove(grid_tuned_paths[i, 2][[1]])
                }
              if (tolower(Construct[i, 22][[1]]) == "standard") {
                if (SaveToFile == TRUE) {
                  save_model <-
                    h2o::h2o.saveModel(object = bl_model,
                                       path = model_path,
                                       force = TRUE)
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 2L,
                    value = save_model
                  )
                  if (SaveToFile == TRUE) {
                    save(
                      grid_tuned_paths,
                      file = paste0(model_path, "/grid_tuned_paths.Rdata")
                    )
                  }
                }
              } else {
                if (SaveToFile == TRUE) {
                  save_model <-
                    h2o::h2o.saveMojo(object = bl_model,
                                      path = model_path,
                                      force = TRUE)
                  h2o::h2o.download_mojo(
                    model = bl_model,
                    path = model_path,
                    get_genmodel_jar = TRUE,
                    genmodel_path = model_path,
                    genmodel_name = Construct[i, 5][[1]]
                  )
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 2L,
                    value = save_model
                  )
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 6L,
                    value = paste0(model_path, "\\", Construct[i, 5][[1]])
                  )
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              }
            }
            
            # Save VarImp
            VIMP <-
              data.table::as.data.table(h2o::h2o.varimp(bl_model))
            if (SaveToFile == TRUE) {
              save(VIMP,
                   file = paste0(model_path,
                                 "/VarImp_",
                                 Construct[i, 5][[1]],
                                 ".Rdata"))
            }
            NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
            if (length(NIF) > 0) {
              if (SaveToFile == TRUE) {
                save(NIF,
                     file = paste0(
                       model_path,
                       "/VarNOTImp_",
                       Construct[i, 5][[1]],
                       ".Rdata"
                     ))
              }
            }
            
            # Gather predicted values
            preds <-
              h2o::h2o.predict(bl_model, newdata = validate)[, 1]
            if (Construct[i, 14][[1]] == "All") {
              predsPD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 1]
              PredsPD <- data.table::as.data.table(predsPD)
              if (SaveToFile == TRUE) {
                data.table::fwrite(PredsPD,
                                   file = paste0(model_path,
                                                 "/", Construct[i, 5][[1]],
                                                 "_PredsAll.csv"))
              }
            } else if (Construct[i, 14][[1]] == "Train") {
              predsPD <- h2o::h2o.predict(bl_model, newdata = train)[, 1]
            } else if (Construct[i, 14][[1]] == "Validate") {
              predsPD <- h2o::h2o.predict(bl_model, newdata = validate)[, 1]
            }
          }
        } else {
          if (cc > dd) {
            # Save model
            if (Construct[i, 21][[1]] == TRUE) {
              if (grid_tuned_paths[i, 2][[1]] != "a")
                if (SaveToFile == TRUE) {
                  file.remove(grid_tuned_paths[i, 2][[1]])
                }
              if (tolower(Construct[i, 22][[1]]) == "standard") {
                if (SaveToFile == TRUE) {
                  save_model <-
                    h2o::h2o.saveModel(object = best_model,
                                       path = model_path,
                                       force = TRUE)
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 2L,
                    value = save_model
                  )
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              } else {
                if (SaveToFile == TRUE) {
                  save_model <-
                    h2o::h2o.saveMojo(object = best_model,
                                      path = model_path,
                                      force = TRUE)
                  h2o::h2o.download_mojo(
                    model = best_model,
                    path = model_path,
                    get_genmodel_jar = TRUE,
                    genmodel_path = model_path,
                    genmodel_name = Construct[i, 5][[1]]
                  )
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 2L,
                    value = save_model
                  )
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 6L,
                    value = paste0(model_path, "\\", Construct[i, 5][[1]])
                  )
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              }
            }
            
            # Store threshold
            store_results <-
              data.table::data.table(
                best_model@model$training_metrics@metrics$thresholds_and_metric_scores
              )
            if (tolower(Construct[i, 15][[1]]) == "f1" ||
                is.null(Construct[i, 15][[1]])) {
              Thresh <-
                tryCatch({
                  store_results[order(-f1)][1, 1][[1]]
                }, error = function(x)
                  1)
              Label  <- "f1"
            } else if (tolower(Construct[i, 15][[1]]) == "f2") {
              Thresh <-
                tryCatch({
                  store_results[order(-f2)][1, 1][[1]]
                }, error = function(x)
                  1)
              Label  <- "f2"
            } else if (tolower(Construct[i, 15][[1]]) == "f0point5") {
              Thresh <-
                tryCatch({
                  store_results[order(-f0point5)][1, 1][[1]]
                }, error = function(x)
                  1)
              Label <- "f0point5"
            } else if (tolower(Construct[i, 15][[1]]) == "cs") {
              predsPDD <- h2o::h2o.predict(bl_model,
                                           newdata = data_h2o)[, 3]
              data    <-
                data.table::as.data.table(h2o::h2o.cbind(data_h2o,
                                                         predsPDD))
              data[, eval(Construct[i, 1][[1]]) := as.numeric(as.character(get(Construct[i, 1][[1]])))]
              temp  <- threshOptim(
                data     = data,
                actTar   = Construct[i, 1][[1]],
                predTar  = 'p1',
                tpProfit = Construct[i, 17][[1]],
                tnProfit = Construct[i, 18][[1]],
                fpProfit = Construct[i, 19][[1]],
                fnProfit = Construct[i, 20][[1]]
              )
              Thresh <- temp[[1]]
              Label <- "CS"
            }
            data.table::set(
              grid_tuned_paths,
              i = i,
              j = 5L,
              value = Thresh
            )
            
            # Save VarImp
            VIMP <-
              data.table::as.data.table(h2o::h2o.varimp(best_model))
            if (SaveToFile == TRUE) {
              save(VIMP,
                   file = paste0(model_path,
                                 "/VarImp_", Construct[i, 5][[1]],
                                 ".Rdata"))
            }
            NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
            if (length(NIF) > 0) {
              if (SaveToFile == TRUE) {
                save(NIF,
                     file = paste0(
                       model_path,
                       "/VarNOTImp_",
                       Construct[i, 5][[1]],
                       ".Rdata"
                     ))
              }
            }
            
            # Gather predicted values
            preds <-
              h2o::h2o.predict(best_model, newdata = validate)[, 3]
            if (Construct[i, 14][[1]] == "All") {
              predsPD <- h2o::h2o.predict(best_model, newdata = data_h2o)[, 3]
              PredsPD <- data.table::as.data.table(predsPD)
              if (SaveToFile == TRUE) {
                data.table::fwrite(PredsPD,
                                   file = paste0(model_path,
                                                 "/", Construct[i, 5][[1]],
                                                 "_PredsAll.csv"))
              }
            } else if (Construct[i, 14][[1]] == "Train") {
              predsPD <- h2o::h2o.predict(best_model, newdata = train)[, 3]
            } else if (Construct[i, 14][[1]] == "Validate") {
              predsPD <- h2o::h2o.predict(best_model, newdata = validate)[, 3]
            }
          } else {
            # Save model
            if (Construct[i, 21][[1]] == TRUE) {
              if (grid_tuned_paths[i, 2][[1]] != "a")
                if (SaveToFile == TRUE) {
                  file.remove(grid_tuned_paths[i, 2][[1]])
                }
              if (tolower(Construct[i, 22][[1]]) == "standard") {
                if (SaveToFile == TRUE) {
                  save_model <-
                    h2o::h2o.saveModel(object = bl_model,
                                       path = model_path,
                                       force = TRUE)
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 2L,
                    value = save_model
                  )
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              } else {
                if (SaveToFile == TRUE) {
                  save_model <-
                    h2o::h2o.saveMojo(object = bl_model,
                                      path = model_path,
                                      force = TRUE)
                  h2o::h2o.download_mojo(
                    model = bl_model,
                    path = model_path,
                    get_genmodel_jar = TRUE,
                    genmodel_path = model_path,
                    genmodel_name = Construct[i, 5][[1]]
                  )
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 2L,
                    value = save_model
                  )
                  data.table::set(
                    grid_tuned_paths,
                    i = i,
                    j = 6L,
                    value = paste0(model_path, "\\", Construct[i, 5][[1]])
                  )
                  save(grid_tuned_paths,
                       file = paste0(model_path, "/grid_tuned_paths.Rdata"))
                }
              }
            }
            
            # Store threshold
            store_results <-
              data.table::data.table(
                bl_model@model$training_metrics@metrics$thresholds_and_metric_scores
              )
            if (tolower(Construct[i, 15][[1]]) == "f1" ||
                is.null(Construct[i, 15][[1]])) {
              Thresh <-
                tryCatch({
                  store_results[order(-f1)][1, 1][[1]]
                }, error = function(x)
                  1)
              Label  <- "f1"
            } else if (tolower(Construct[i, 15][[1]]) == "f2") {
              Thresh <-
                tryCatch({
                  store_results[order(-f2)][1, 1][[1]]
                }, error = function(x)
                  1)
              Label  <- "f2"
            } else if (tolower(Construct[i, 15][[1]]) == "f0point5") {
              Thresh <-
                tryCatch({
                  store_results[order(-f0point5)][1, 1][[1]]
                }, error = function(x)
                  1)
              Label <- "f0point5"
            } else if (tolower(Construct[i, 15][[1]]) == "CS") {
              predsPDD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 3]
              data    <-
                data.table::as.data.table(h2o::h2o.cbind(data_h2o, predsPDD))
              data[, eval(Construct[i, 1][[1]]) := as.numeric(as.character(get(Construct[i, 1][[1]])))]
              temp  <- threshOptim(
                data     = data,
                actTar   = Construct[i, 1][[1]],
                predTar  = 'p1',
                tpProfit = Construct[i, 17][[1]],
                tnProfit = Construct[i, 18][[1]],
                fpProfit = Construct[i, 19][[1]],
                fnProfit = Construct[i, 20][[1]]
              )
              Thresh <- temp[[1]]
              Label <- "CS"
            }
            data.table::set(
              grid_tuned_paths,
              i = i,
              j = 5L,
              value = Thresh
            )
            
            # Save VarImp
            VIMP <-
              data.table::as.data.table(h2o::h2o.varimp(bl_model))
            if (SaveToFile == TRUE) {
              save(VIMP,
                   file = paste0(model_path,
                                 "/VarImp_", Construct[i, 5][[1]],
                                 ".Rdata"))
            }
            NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
            if (length(NIF) > 0) {
              if (SaveToFile == TRUE) {
                save(NIF,
                     file = paste0(
                       model_path,
                       "/VarNOTImp_",
                       Construct[i, 5][[1]],
                       ".Rdata"
                     ))
              }
            }
            
            # Gather predicted values
            preds <-
              h2o::h2o.predict(bl_model, newdata = validate)[, 3]
            if (Construct[i, 14][[1]] == "All") {
              predsPD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 3]
              PredsPD <- data.table::as.data.table(predsPD)
              if (SaveToFile == TRUE) {
                data.table::fwrite(PredsPD,
                                   file = paste0(model_path,
                                                 "/",
                                                 Construct[i, 5][[1]],
                                                 "_PredsAll.csv"))
              }
            } else if (Construct[i, 14][[1]] == "Train") {
              predsPD <- h2o::h2o.predict(bl_model, newdata = train)[, 3]
            } else if (Construct[i, 14][[1]] == "Validate") {
              predsPD <- h2o::h2o.predict(bl_model, newdata = validate)[, 3]
            }
          }
        }
      } else if (tolower(Construct[i, 6][[1]]) != "automl") {
        # Save model
        if (Construct[i, 21][[1]] == TRUE) {
          if (grid_tuned_paths[i, 2][[1]] != "a")
            if (SaveToFile == TRUE) {
              file.remove(grid_tuned_paths[i, 2][[1]])
            }
          if (tolower(Construct[i, 22][[1]]) == "standard") {
            if (SaveToFile == TRUE) {
              save_model <-
                h2o::h2o.saveModel(object = bl_model,
                                   path = model_path,
                                   force = TRUE)
              data.table::set(
                grid_tuned_paths,
                i = i,
                j = 2L,
                value = save_model
              )
              save(grid_tuned_paths,
                   file = paste0(model_path,
                                 "/grid_tuned_paths.Rdata"))
            }
          } else {
            if (SaveToFile == TRUE) {
              save_model <-
                h2o::h2o.saveMojo(object = bl_model,
                                  path = model_path,
                                  force = TRUE)
              h2o::h2o.download_mojo(
                model = bl_model,
                path = model_path,
                get_genmodel_jar = TRUE,
                genmodel_path = model_path,
                genmodel_name = Construct[i, 5][[1]]
              )
              data.table::set(
                grid_tuned_paths,
                i = i,
                j = 2L,
                value = save_model
              )
              data.table::set(
                grid_tuned_paths,
                i = i,
                j = 6L,
                value = paste0(model_path, "\\", Construct[i, 5][[1]])
              )
              save(grid_tuned_paths,
                   file = paste0(model_path,
                                 "/grid_tuned_paths.Rdata"))
            }
          }
        }
        
        # Store threshold for binary classification
        if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                 "binomial",
                                                 "bernoulli")) {
          store_results <-
            data.table::data.table(bl_model@model$training_metrics@metrics$thresholds_and_metric_scores)
          if (tolower(Construct[i, 15][[1]]) == "f1" ||
              is.null(Construct[i, 15][[1]])) {
            Thresh <-
              tryCatch({
                store_results[order(-f1)][1, 1][[1]]
              }, error = function(x)
                1)
            Label  <- "f1"
          } else if (tolower(Construct[i, 15][[1]]) == "f2") {
            Thresh <-
              tryCatch({
                store_results[order(-f2)][1, 1][[1]]
              }, error = function(x)
                1)
            Label  <- "f2"
          } else if (tolower(Construct[i, 15][[1]]) == "f0point5") {
            Thresh <-
              tryCatch({
                store_results[order(-f0point5)][1, 1][[1]]
              }, error = function(x)
                1)
            Label <- "f0point5"
          } else if (tolower(Construct[i, 15][[1]]) == "cs") {
            predsPDD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 3]
            data    <-
              data.table::as.data.table(h2o::h2o.cbind(data_h2o, predsPDD))
            data[, eval(Construct[i, 1][[1]]) := as.numeric(as.character(get(Construct[i, 1][[1]])))]
            temp  <- threshOptim(
              data     = data,
              actTar   = Construct[i, 1][[1]],
              predTar  = 'p1',
              tpProfit = Construct[i, 17][[1]],
              tnProfit = Construct[i, 18][[1]],
              fpProfit = Construct[i, 19][[1]],
              fnProfit = Construct[i, 20][[1]]
            )
            Thresh <- temp[[1]]
            Label <- "CS"
          }
          data.table::set(
            grid_tuned_paths,
            i = i,
            j = 5L,
            value = Thresh
          )
          preds <-
            h2o::h2o.predict(bl_model, newdata = validate)[, 3]
          if (tolower(Construct[i, 14][[1]]) == "all") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 3]
            PredsPD <- data.table::as.data.table(predsPD)
            if (SaveToFile == TRUE) {
              fwrite(PredsPD,
                     file = paste0(model_path,
                                   "/", Construct[i, 5][[1]],
                                   "_PredsAll.csv"))
            }
          } else if (tolower(Construct[i, 14][[1]]) == "train") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = train)[, 3]
          } else if (tolower(Construct[i, 14][[1]]) == "validate") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = validate)[, 3]
          }
        } else {
          # Store predicted values against validate data for calibration plot
          preds <-
            h2o::h2o.predict(bl_model, newdata = validate)[, 1]
          if (tolower(Construct[i, 14][[1]]) == "all") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = data_h2o)[, 1]
            PredsPD <- data.table::as.data.table(predsPD)
            if (SaveToFile == TRUE) {
              data.table::fwrite(PredsPD,
                                 file = paste0(model_path,
                                               "/",
                                               Construct[i, 5][[1]],
                                               "_PredsAll.csv"))
            }
          } else if (tolower(Construct[i, 14][[1]]) == "train") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = train)[, 1]
          } else if (tolower(Construct[i, 14][[1]]) == "validate") {
            predsPD <- h2o::h2o.predict(bl_model, newdata = validate)[, 1]
          }
        }
        
        # Save VarImp
        VIMP <- data.table::as.data.table(h2o::h2o.varimp(bl_model))
        if (SaveToFile == TRUE) {
          save(VIMP,
               file = paste0(model_path,
                             "/VarImp_",
                             Construct[i, 5][[1]],
                             ".Rdata"))
        }
        NIF <- VIMP[percentage < Construct[i, 16][[1]], 1][[1]]
        if (length(NIF) > 0) {
          if (SaveToFile == TRUE) {
            save(NIF,
                 file = paste0(model_path,
                               "/VarNOTImp_",
                               Construct[i, 5][[1]],
                               ".Rdata"))
          }
        }
      }
      
      ######################################
      # Model Evaluation Plots
      ######################################
      
      # Generate plots
      col <- Construct[i, 1][[1]]
      calibration <-
        data.table::as.data.table(h2o::h2o.cbind(preds, validate[, col]))
      if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                               "binomial",
                                               "bernoulli")) {
        calibration[, eval(col) := as.numeric(as.character(get(col)))]
      }
      if (Construct[i, 13][[1]] >= 1) {
        if (tolower(Construct[i, 14][[1]]) == "all") {
          calibEval <-
            data.table::as.data.table(h2o::h2o.cbind(preds, validate))
          calib <-
            data.table::as.data.table(h2o::h2o.cbind(predsPD, data_h2o))
        } else if (tolower(Construct[i, 14][[1]]) == "train") {
          calibEval <-
            data.table::as.data.table(h2o::h2o.cbind(preds, validate))
          calib <- as.data.table(h2o::h2o.cbind(predsPD, train))
        } else if (tolower(Construct[i, 14][[1]]) == "validate") {
          calibEval <-
            data.table::as.data.table(h2o::h2o.cbind(preds, validate))
          calib <- as.data.table(h2o::h2o.cbind(predsPD, validate))
        }
        if (Construct[i, 12][[1]]) {
          if (SaveToFile == TRUE) {
            save(calibEval,
                 file = paste0(model_path,
                               "/", Construct[i, 5][[1]],
                               "_Validation.Rdata"))
          }
        }
      } else {
        if (Construct[i, 12][[1]]) {
          calibEval <-
            data.table::as.data.table(h2o::h2o.cbind(preds, validate))
          if (SaveToFile == TRUE) {
            save(calibEval,
                 file = paste0(model_path,
                               "/",
                               Construct[i, 5][[1]],
                               "_Validation.Rdata"))
          }
        }
      }
      predName <- names(calibration[, 1])
      
      # Generate evaluation plots
      if (tolower(Construct[i, 2][[1]]) != "multinomial") {
        if (tolower(Construct[i, 2][[1]]) == "quantile") {
          # Store best metric
          if (Construct[i, 11][[1]]) {
            if (cc < dd) {
              val <- cc
            } else {
              val <- dd
            }
          } else {
            val <- dd
          }
          
          # Calibration plot
          out1 <- EvalPlot(
            calibration,
            PredictionColName = predName,
            TargetColName  = Construct[i, 1][[1]],
            GraphType        = "calibration",
            PercentileBucket      = 0.05,
            aggrfun     = function(x)
              quantile(x,
                       probs = Construct[i, 4][[1]],
                       na.rm = TRUE)
          )
          out1 <- out1 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(Construct[i, 3][[1]]),
            ": ",
            round(val, 4)
          ))
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalP_",
                                   Construct[i, 5][[1]],
                                   ".png"))
          }
          
          # Calibration boxplot
          out2 <- EvalPlot(
            calibration,
            PredictionColName = predName,
            TargetColName  = Construct[i, 1][[1]],
            GraphType        = "boxplot",
            PercentileBucket      = 0.05
          )
          out2 <- out2 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(Construct[i, 3][[1]]),
            ": ",
            round(val, 4)
          ))
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalBP_",
                                   Construct[i, 5][[1]],
                                   ".png"))
          }
        } else if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                        "binomial",
                                                        "bernoulli")) {
          # Store best metric
          if (Construct[i, 11][[1]]) {
            if (cc < dd) {
              val <- cc
            } else {
              val <- dd
            }
          } else {
            val <- dd
          }
          
          out1 <- EvalPlot(
            calibration,
            PredictionColName = predName,
            TargetColName  = Construct[i, 1][[1]],
            GraphType        = "calibration",
            PercentileBucket      = 0.05,
            aggrfun     = function(x)
              base::mean(x, na.rm = TRUE)
          )
          out1 <- out1 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(Construct[i, 3][[1]]),
            ": ",
            round(val, 4)
          ))
          
          if (exists("Thresh")) {
            out1 <- out1 + ggplot2::geom_hline(yintercept = Thresh)
          }
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalP_",
                                   Construct[i, 5][[1]],
                                   ".png"))
          }
        } else {
          # Store best metric
          if (Construct[i, 11][[1]]) {
            if (cc < dd) {
              val <- cc
            } else {
              val <- dd
            }
          } else {
            val <- dd
          }
          
          # Calibration plot
          out1 <- EvalPlot(
            calibration,
            PredictionColName = predName,
            TargetColName  = Construct[i, 1][[1]],
            GraphType        = "calibration",
            PercentileBucket      = 0.05,
            aggrfun     = function(x)
              base::mean(x, na.rm = TRUE)
          )
          out1 <- out1 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(Construct[i, 3][[1]]),
            ": ",
            round(val, 4)
          ))
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalP_",
                                   Construct[i, 5][[1]],
                                   ".png"))
          }
          
          # Calibration boxplot
          out2 <- EvalPlot(
            calibration,
            PredictionColName = predName,
            TargetColName  = Construct[i, 1][[1]],
            GraphType        = "boxplot",
            PercentileBucket      = 0.05
          )
          out2 <- out2 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(Construct[i, 3][[1]]),
            ": ",
            round(val, 4)
          ))
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalBP_",
                                   Construct[i, 5][[1]],
                                   ".png"))
          }
        }
      } else {
        # Multinomial case
        # Stack each level's predicted values and actual values
        if (Construct[i, 11][[1]] && cc <= dd) {
          predsMulti <- h2o::h2o.predict(best_model, newdata = validate)
          col <- Construct[i, 1][[1]]
          xx <-
            data.table::as.data.table(h2o::h2o.cbind(validate[, col],
                                                     predsMulti))
          if (Construct[i, 12][[1]]) {
            calib <- data.table::as.data.table(h2o::h2o.cbind(validate,
                                                              preds))
            if (SaveToFile == TRUE) {
              save(calib,
                   file = paste0(
                     model_path,
                     "/",
                     Construct[i, 5][[1]],
                     "_Validation.Rdata"
                   ))
            }
          }
          N <- (ncol(xx) - 2)
          data <- eval(parse(text = Construct[i, 7][[1]]))
          for (lev in levels(data[[Construct[i, 1][[1]]]])) {
            xx[, paste0("V", lev) := ifelse(xx[[1]] %in% lev, 1, 0)]
          }
          RemoveCols <- names(xx)[1:2]
          KeepCols   <- names(xx)[3:length(names(xx))]
          xx[, (RemoveCols) := NULL]
          store <- list()
          for (k in 1:N) {
            j <- k + N
            temp <- cbind(xx[, ..k], xx[, ..j])
            data.table::setnames(temp, KeepCols[k], "Preds")
            data.table::setnames(temp, KeepCols[j], "Act")
            store[[k]] <- temp
          }
          xxx <- data.table::rbindlist(store)
          
          # Multinomial metric
          if (multinomialMetric == "auc") {
            val <- H2OMultinomialAUC(
              validate,
              best_model,
              targetColNum = 1,
              targetName = Construct[i, 1][[1]]
            )
          } else {
            xx <- data.table::as.data.table(h2o::h2o.cbind(
              validate[, 1],
              h2o::h2o.predict(best_model,
                               newdata = validate)[, 1]
            ))
            names(xx)
            val <-
              mean(xx[, Accuracy := as.numeric(ifelse(get(Construct[i, 1][[1]]) == predict, 1, 0))][["Accuracy"]],
                   na.rm = TRUE)
          }
          
          # Store baseline val
          temp <- H2OMultinomialAUC(validate,
                                    best_model,
                                    targetColNum = 1,
                                    targetName = Construct[i, 1][[1]])
          
          # Store micro auc
          data.table::set(
            grid_tuned_paths,
            i = i,
            j = 3L,
            value = val
          )
          data.table::set(
            grid_tuned_paths,
            i = i,
            j = 4L,
            value = temp
          )
          
          # Calibration plot
          out1 <- EvalPlot(
            xxx,
            PredictionColName = "Preds",
            TargetColName  = "Act",
            GraphType        = "calibration",
            PercentileBucket      = 0.05,
            aggrfun     = function(x)
              base::mean(x, na.rm = TRUE)
          )
          out1 <- out1 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(multinomialMetric),
            ": ",
            round(val, 4)
          ))
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalP_",
                                   Construct[i, 5][[1]],
                                   ".png"))
          }
          
        } else {
          predsMulti <- h2o::h2o.predict(bl_model, newdata = validate)
          col <- Construct[i, 1][[1]]
          xx <-
            data.table::as.data.table(h2o::h2o.cbind(validate[, col],
                                                     predsMulti))
          if (Construct[i, 12][[1]]) {
            calib <- data.table::as.data.table(h2o::h2o.cbind(validate,
                                                              preds))
            if (SaveToFile == TRUE) {
              save(calib,
                   file = paste0(
                     model_path,
                     "/",
                     Construct[i, 5][[1]],
                     "_Validation.Rdata"
                   ))
            }
          }
          N <- (ncol(xx) - 2)
          data <- eval(parse(text = Construct[i, 7][[1]]))
          for (lev in levels(data[[Construct[i, 1][[1]]]])) {
            xx[, paste0("V", lev) := ifelse(xx[[1]] %in% lev, 1, 0)]
          }
          RemoveCols <- names(xx)[1:2]
          KeepCols   <- names(xx)[3:length(names(xx))]
          xx[, (RemoveCols) := NULL]
          store <- list()
          for (k in seq_len(N)) {
            j <- k + N
            temp <- cbind(xx[, ..k], xx[, ..j])
            data.table::setnames(temp, KeepCols[k], "Preds")
            data.table::setnames(temp, KeepCols[j], "Act")
            store[[k]] <- temp
          }
          xxx <- data.table::rbindlist(store)
          
          # Multinomial metric
          if (multinomialMetric == "auc") {
            val <- H2OMultinomialAUC(validate,
                                     bl_model,
                                     targetColNum = 1,
                                     targetName = Construct[i, 1][[1]])
          } else {
            xx <- data.table::as.data.table(h2o::h2o.cbind(
              validate[, 1],
              h2o::h2o.predict(bl_model,
                               newdata = validate)[, 1]
            ))
            names(xx)
            val <-
              mean(xx[, Accuracy := as.numeric(ifelse(get(Construct[i, 1][[1]]) == predict, 1, 0))][["Accuracy"]],
                   na.rm = TRUE)
          }
          
          # Calibration plot
          out1 <- EvalPlot(
            xxx,
            PredictionColName = "Preds",
            TargetColName  = "Act",
            GraphType        = "calibration",
            PercentileBucket      = 0.05,
            aggrfun     = function(x)
              base::mean(x, na.rm = TRUE)
          )
          out1 <- out1 + ggplot2::ggtitle(paste0(
            "Calibration Evaluation Plot ",
            toupper(multinomialMetric),
            ": ",
            round(val, 4)
          ))
          if (SaveToFile == TRUE) {
            ggplot2::ggsave(paste0(model_path,
                                   "/CalP_",
                                   Construct[i, 5][[1]], ".png"))
          }
        }
        
        # Store micro auc
        data.table::set(grid_tuned_paths,
                        i = i,
                        j = 4L,
                        value = val)
      }
      
      #######################################
      # Partial dependence calibration plots
      #######################################
      
      if (Construct[i, 13][[1]] >= 1) {
        VIMP <- VIMP[!is.na(VIMP[, 2][[1]])]
        rows <- nrow(VIMP)
        cols <- VIMP[1:min(Construct[i, 13][[1]], rows), 1][[1]]
        calibr <- list()
        boxplotr <- list()
        j <- 0
        if (!(tolower(Construct[i, 2][[1]]) %in% c("multinomial"))) {
          for (col in cols) {
            j <- j + 1
            if (tolower(Construct[i, 2][[1]]) == "quantile") {
              out1 <- tryCatch({
                ParDepCalPlots(
                  calib,
                  PredictionColName = predName,
                  TargetColName  = Construct[i, 1][[1]],
                  IndepVar    = col,
                  GraphType        = "calibration",
                  PercentileBucket      = 0.05,
                  FactLevels  = 10,
                  Function    = function(x)
                    quantile(x,
                             probs = Construct[i, 4][[1]],
                             na.rm = TRUE)
                )
              },
              error = function(x)
                "skip")
            } else {
              out1 <- tryCatch({
                ParDepCalPlots(
                  calib,
                  PredictionColName = predName,
                  TargetColName  = Construct[i, 1][[1]],
                  IndepVar    = col,
                  GraphType        = "calibration",
                  PercentileBucket      = 0.05,
                  FactLevels  = 10,
                  Function    = function(x)
                    base::mean(x, na.rm = TRUE)
                )
              },
              error = function(x)
                "skip")
            }
            
            # Add threshold line to charts
            if (tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                     "binomial",
                                                     "bernoulli")) {
              if (exists("Thresh")) {
                out1 <- out1 + ggplot2::geom_hline(yintercept = Thresh)
              }
              calibr[[paste0(col)]] <- out1
            } else {
              calibr[[paste0(col)]] <- out1
            }
            
            # Expected value regression
            if (!(tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                       "binomial",
                                                       "bernoulli"))) {
              boxplotr[[paste0(col)]] <- tryCatch({
                ParDepCalPlots(
                  calib,
                  PredictionColName = predName,
                  TargetColName  = Construct[i, 1][[1]],
                  IndepVar    = col,
                  GraphType        = "boxplot",
                  PercentileBucket      = 0.05,
                  FactLevels  = 10
                )
              },
              error = function(x)
                "skip")
            }
          }
          
          # Save output
          if (!(tolower(Construct[i, 2][[1]]) %in% c("quasibinomial",
                                                     "binomial",
                                                     "bernoulli"))) {
            if (SaveToFile == TRUE) {
              save(
                boxplotr,
                file = paste0(
                  model_path,
                  "/",
                  Construct[i, 5][[1]],
                  "_ParDepCalBoxPlots.Rdata"
                )
              )
            }
            save(calibr,
                 file = paste0(
                   model_path,
                   "/",
                   Construct[i, 5][[1]],
                   "_ParDepCalPlots.Rdata"
                 ))
          }
        }
      }
      
      # Save grid_tuned_paths
      if (SaveToFile == TRUE) {
        save(grid_tuned_paths,
             file = paste0(model_path, "/grid_tuned_paths.Rdata"))
      }
      
      # Clear H2O environment between runs
      h2o::h2o.rm(data_h2o)
      if (!Construct[i, SupplyData][[1]]) {
        h2o::h2o.rm(data_train)
      }
      h2o::h2o.rm(train)
      h2o::h2o.rm(validate)
      if (Construct[i, 11][[1]]) {
        h2o::h2o.rm(best_model)
      }
      if (Construct[i, 6][[1]] != "automl") {
        h2o::h2o.rm(bl_model)
      }
      h2o::h2o.rm(preds)
      h2o::h2o.shutdown(prompt = FALSE)
      
      # Clear R environment between runs
      if (Construct[i, 11][[1]]) {
        if (Construct[i, 2][[1]] != "multinomial" &
            Construct[i, 21][[1]] == TRUE) {
          rm(grid,
             Grid_Out,
             cc,
             dd,
             VIMP,
             calibration,
             features,
             target,
             save_model)
        } else {
          rm(grid,
             Grid_Out,
             cc,
             dd,
             VIMP,
             features,
             target,
             predsMulti)
        }
      } else {
        if (Construct[i, 2][[1]] != "multinomial") {
          rm(dd,
             VIMP,
             calibration,
             features,
             target,
             save_model)
        } else {
          rm(dd, VIMP, features, target)
        }
      }
      
      # Remove data if no longer needed
      if (i > 1) {
        if (Construct[i, 7][[1]] != Construct[(i - 1), 7][[1]]) {
          eval(parse(text = paste0("rm(", Construct[(i - 1), 7][[1]], ")")))
        }
      }
    }
  }, error = function(x)
    h2o::h2o.shutdown(prompt = FALSE))
  if (ReturnObjects) {
    return(list(Construct = Construct, GridTunedPaths = grid_tuned_paths))
  }
}

#' AutoH2OScoring is the complement of AutoH20Modeler.
#'
#' AutoH2OScoring is the complement of AutoH20Modeler. Use this for scoring models. You can score regression, quantile regression, classification, multinomial, clustering, and text models (built with the Word2VecModel function). You can also use this to score multioutcome models so long as the there are two models: one for predicting the count of outcomes (a count outcome in character form) and a multinomial model on the label data. You will want to ensure you have a record for each label in your training data in (0,1) as factor form.
#'
#' @author Adrian Antico
#' @family Supervised Learning
#' @param Features This is a data.table of features for scoring.
#' @param GridTuneRow Numeric. The row numbers of grid_tuned_paths, KMeansModelFile, or StoreFile containing the model you wish to score
#' @param ScoreMethod "Standard" or "Mojo": Mojo is available for supervised models; use standard for all others
#' @param TargetType "Regression", "Classification", "Multinomial", "MultiOutcome", "Text", "Clustering". MultiOutcome must be two multinomial models, a count model (the count of outcomes, as a character value), and the multinomial model predicting the labels.
#' @param ClassVals Choose from "p1", "Probs", "Label", or "All" for classification and multinomial models.
#' @param NThreads Number of available threads for H2O
#' @param MaxMem Amount of memory to dedicate to H2O
#' @param JavaOptions Modify to your machine if the default doesn't work
#' @param SaveToFile Set to TRUE if you want your model scores saved to file.
#' @param FilesPath Set this to the folder where your models and model files are saved
#' @param H20ShutDown TRUE to shutdown H2O after the run. Use FALSE if you will be repeatedly scoring and shutdown somewhere else in your environment.
#' @return Returns a list of predicted values. Each list element contains the predicted values from a single model predict call.
#' @examples
#' \donttest{
#' # Multinomial Example
#' Correl <- 0.85
#' aa <- data.table::data.table(target = runif(1000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(1000)]
#' aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                               sqrt(1-Correl^2) * qnorm(x2))))]
#' aa[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                            sqrt(1-Correl^2) * qnorm(x2)))]
#' aa[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' aa[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' aa[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' aa[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                        sqrt(1-Correl^2) * qnorm(x2)))^2]
#' aa[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                         sqrt(1-Correl^2) * qnorm(x2)))^4]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' aa[, target := as.factor(ifelse(target < 0.33,"A",ifelse(target < 0.66, "B","C")))]
#' Construct <- data.table::data.table(Targets = rep("target",3),
#'                                     Distribution    = c("multinomial",
#'                                                         "multinomial",
#'                                                         "multinomial"),
#'                                     Loss            = c("logloss","logloss","CrossEntropy"),
#'                                     Quantile        = rep(NA,3),
#'                                     ModelName       = c("GBM","DRF","DL"),
#'                                     Algorithm       = c("gbm",
#'                                                         "randomForest",
#'                                                         "deeplearning"),
#'                                     dataName        = rep("aa",3),
#'                                     TargetCol       = rep(c("1"),3),
#'                                     FeatureCols     = rep(c("2:11"),3),
#'                                     CreateDate      = rep(Sys.time(),3),
#'                                     GridTune        = rep(FALSE,3),
#'                                     ExportValidData = rep(TRUE,3),
#'                                     ParDep          = rep(NA,3),
#'                                     PD_Data         = rep("All",3),
#'                                     ThreshType      = rep("f1",3),
#'                                     FSC             = rep(0.001,3),
#'                                     tpProfit        = rep(NA,3),
#'                                     tnProfit        = rep(NA,3),
#'                                     fpProfit        = rep(NA,3),
#'                                     fnProfit        = rep(NA,3),
#'                                     SaveModel       = rep(FALSE,3),
#'                                     SaveModelType   = c("Mojo","mojo","mojo"),
#'                                     PredsAllData    = rep(TRUE,3),
#'                                     TargetEncoding  = rep(NA,3),
#'                                     SupplyData      = rep(FALSE,3))
#'
#' AutoH2OModeler(Construct,
#'                max_memory = "28G",
#'                ratios = 0.75,
#'                BL_Trees = 500,
#'                nthreads = 5,
#'                model_path = NULL,
#'                MaxRuntimeSeconds = 3600,
#'                MaxModels = 30,
#'                TrainData = NULL,
#'                TestData  = NULL,
#'                SaveToFile = FALSE,
#'                ReturnObjects = TRUE)
#'
#' N <- 3
#' data <- AutoH2OScoring(Features     = aa,
#'                        GridTuneRow  = c(1:N),
#'                        ScoreMethod  = "standard",
#'                        TargetType   = rep("multinomial",N),
#'                        ClassVals    = rep("Probs",N),
#'                        NThreads     = 6,
#'                        MaxMem       = "28G",
#'                        JavaOptions  = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
#'                        SaveToFile   = FALSE,
#'                        FilesPath    = NULL,
#'                        H20ShutDown  = rep(FALSE,N))
#'}
#' @export
AutoH2OScoring <- function(Features     = data,
                           GridTuneRow  = c(1:3),
                           ScoreMethod  = "Standard",
                           TargetType   = rep("multinomial", 3),
                           ClassVals    = rep("probs", 3),
                           NThreads     = 6,
                           MaxMem       = "28G",
                           JavaOptions  = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
                           SaveToFile   = FALSE,
                           FilesPath    = NULL,
                           H20ShutDown  = rep(FALSE, 3)) {
  # If FilesPath is NULL, skip function
  if (is.null(FilesPath)) {
    # Only run text or other models types
    if (any(tolower(TargetType) %in% "clustering") &
        any(tolower(TargetType) %in% "text") &
        any(
          tolower(TargetType) %in% c(
            "regression",
            "classification",
            "multinomial",
            "multioutcome"
          )
        )) {
      warning("Run either text models, supervised models,
         or unsupervised models, but only one")
    }
    
    # Import grid_tuned_paths or StoreFile
    if (any(
      tolower(TargetType) %in% c(
        "regression",
        "classification",
        "multinomial",
        "multioutcome"
      )
    )) {
      load(paste0(FilesPath, "/grid_tuned_paths.Rdata"))
    } else if (any(tolower(TargetType) %in% "text")) {
      load(paste0(FilesPath, "/StoreFile.Rdata"))
    } else if (any(tolower(TargetType) %in% "clustering")) {
      load(paste0(FilesPath, "/KMeansModelFile.Rdata"))
    } else {
      warning("TargetType not a valid option")
    }
    
    # Ensure GridTuneRow is not out of bounds
    if (any(
      tolower(TargetType) %in% c(
        "regression",
        "classification",
        "multinomial",
        "multioutcome"
      )
    )) {
      if (nrow(grid_tuned_paths) < max(GridTuneRow)) {
        warning("GridTuneRow is greater than
          the number of rows in grid_tuned_paths")
      }
    } else if (any(tolower(TargetType) %in% "text")) {
      if (nrow(StoreFile) < max(GridTuneRow)) {
        warning("GridTuneRow is greater than
          the number of rows in StoreFile")
      }
    } else if (any(tolower(TargetType) %in% "clustering")) {
      if (nrow(KMeansModelFile) < max(GridTuneRow)) {
        warning("GridTuneRow is greater than
            the number of rows in KMeansModelFile")
      }
    } else {
      warning("TargetType not a valid option")
    }
    
    ScoresList <- list()
    for (i in as.integer(seq_along(GridTuneRow))) {
      # Scoring
      if (tolower(ScoreMethod) == "mojo") {
        if (tolower(TargetType[i]) == "multinomial") {
          if (tolower(ClassVals[i]) == c("probs")) {
            if (SaveToFile) {
              data.table::fwrite(Features, file.path(FilesPath, 'Features.csv'))
            }
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath, 'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i, 2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i, 6][[1]],
                verbose = FALSE
              )[,-1]
            )
          } else if (tolower(ClassVals[i]) == "label") {
            if (SaveToFile) {
              data.table::fwrite(Features, file.path(FilesPath, 'Features.csv'))
            }
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath, 'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i, 2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i, 6][[1]],
                verbose = FALSE
              )[, 1]
            )
            data.table::setnames(Scores, "predict", "Class")
          } else if (tolower(ClassVals[i]) == "all") {
            if (SaveToFile) {
              data.table::fwrite(Features, file.path(FilesPath, 'Features.csv'))
            }
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath, 'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i, 2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i, 6][[1]],
                verbose = FALSE
              )
            )
            data.table::setnames(Scores, "predict", "Class")
          } else {
            warning("ClassVals can only be Probs, Label or All")
          }
        } else if (tolower(TargetType[i]) == "classification") {
          if (tolower(ClassVals[i]) == c("p1")) {
            if (SaveToFile) {
              data.table::fwrite(Features, file.path(FilesPath, 'Features.csv'))
            }
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath, 'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i, 2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i, 6][[1]],
                verbose = FALSE
              )[, 3]
            )
          } else if (tolower(ClassVals[i]) == c("probs")) {
            if (SaveToFile) {
              data.table::fwrite(Features, file.path(FilesPath, 'Features.csv'))
            }
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath, 'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i, 2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i, 6][[1]],
                verbose = FALSE
              )[,-1]
            )
          } else if (tolower(ClassVals[i]) == "label") {
            data.table::fwrite(Features, file.path(FilesPath, 'Features.csv'))
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath, 'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i, 2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i, 6][[1]],
                verbose = FALSE
              )[, 1]
            )
            data.table::setnames(Scores, "predict", "Class")
          } else if (tolower(ClassVals[i]) == "all") {
            if (SaveToFile) {
              data.table::fwrite(Features, file.path(FilesPath, 'Features.csv'))
            }
            Scores <- data.table::as.data.table(
              h2o::h2o.mojo_predict_csv(
                input_csv_path = file.path(FilesPath, 'Features.csv'),
                mojo_zip_path = grid_tuned_paths[i, 2][[1]],
                java_options = JavaOptions,
                genmodel_jar_path = grid_tuned_paths[i, 6][[1]],
                verbose = FALSE
              )
            )
            data.table::setnames(Scores, "predict", "Class")
          } else {
            warning("ClassVals can only be Probs, Label or All")
          }
        } else if (tolower(TargetType[i]) == "regression") {
          if (SaveToFile) {
            data.table::fwrite(Features, file.path(FilesPath, 'Features.csv'))
          }
          Scores <- data.table::as.data.table(
            h2o::h2o.mojo_predict_csv(
              input_csv_path = file.path(FilesPath, 'Features.csv'),
              mojo_zip_path = grid_tuned_paths[i, 2][[1]],
              java_options = JavaOptions,
              genmodel_jar_path = grid_tuned_paths[i, 6][[1]],
              verbose = FALSE
            )
          )
        } else if (tolower(TargetType[i]) == "text") {
          keep <- StoreFile[i, 1][[1]]
          temp <- AutoH2OTextPrepScoring(data = Features[, ..keep],
                                         string = StoreFile[i, 1][[1]])
          if (SaveToFile) {
            data.table::fwrite(Features, file.path(FilesPath, 'Features.csv'))
          }
          Scores <- data.table::as.data.table(
            h2o::h2o.mojo_predict_csv(
              input_csv_path = file.path(FilesPath, 'Features.csv'),
              mojo_zip_path = StoreFile[i, 2][[1]],
              java_options = JavaOptions,
              genmodel_jar_path = StoreFile[i, 3][[1]],
              verbose = FALSE
            )
          )
        } else if (tolower(TargetType[i]) == "multioutcome") {
          if (SaveToFile) {
            data.table::fwrite(Features, file.path(FilesPath, 'Features.csv'))
          }
          Counts <- as.numeric(as.character(
            h2o::h2o.mojo_predict_csv(
              input_csv_path = file.path(FilesPath, 'Features.csv'),
              mojo_zip_path = grid_tuned_paths[i, 2][[1]],
              java_options = JavaOptions,
              genmodel_jar_path = grid_tuned_paths[i, 6][[1]],
              verbose = FALSE
            )
          ))
          if (SaveToFile) {
            data.table::fwrite(Features, paste0(FilesPath, "/Features.csv"))
          }
          Temp <- data.table::as.data.table(
            h2o::h2o.mojo_predict_csv(
              input_csv_path = file.path(FilesPath, 'Features.csv'),
              mojo_zip_path = grid_tuned_paths[i, 2][[1]],
              java_options = JavaOptions,
              genmodel_jar_path = grid_tuned_paths[i, 6][[1]],
              verbose = FALSE
            )
          )
          Vals <-
            names(sort(Temp[1, 2:ncol(Temp)], decreasing = TRUE))
          Scores <- paste0(Vals, collapse = " ")
          preds$ModelName[i] <- grid_tuned_paths[i, 1][[1]]
          preds$Scores[i] <- Scores
        } else {
          warning("TargetType is not Multinomial,
          Classification, Regression, or Text")
        }
      } else if (tolower(ScoreMethod) == "standard") {
        # H2O Startup function
        startH2o <- function() {
          h2o::h2o.init(nthreads     = NThreads,
                        max_mem_size = MaxMem)
        }
        # Check if H2O is running
        tryCatch(
          expr = {
            h2o::h2o.init(startH2O = FALSE)
          },
          error = function(e) {
            startH2o()
          }
        )
        
        # Load model
        if (tolower(TargetType[i]) == "text") {
          model <- h2o::h2o.loadModel(path = StoreFile[i, Path])
        } else if (TargetType[i] != "clustering") {
          model <- h2o::h2o.loadModel(path = grid_tuned_paths[i, Path])
        } else {
          KMeans <-
            h2o::h2o.loadModel(path = KMeansModelFile[i + 1, FilePath1])
        }
        # Load Features
        if (i == 1 && tolower(TargetType[i]) != "text") {
          if (tolower(TargetType[i]) == "clustering") {
            x <- c()
            z <- 0
            for (nam in names(Features)) {
              if (is.factor(Features[1, get(nam)]) |
                  is.character(Features[1, get(nam)])) {
                z <- z + 1
                x[z] <- nam
              }
            }
            features <- data.table::copy(Features)
            features <- DummifyDT(
              features,
              cols = x,
              KeepFactorCols = FALSE,
              OneHot = FALSE,
              ClustScore = TRUE
            )
            features <- h2o::as.h2o(features)
          } else {
            features <- h2o::as.h2o(Features)
          }
        }
        if (tolower(TargetType[i]) == "multinomial") {
          if (tolower(ClassVals[i]) == "probs") {
            Scores <- data.table::as.data.table(h2o::h2o.predict(model,
                                                                 newdata = features)[,-1])
          } else if (tolower(ClassVals[i]) == "label") {
            Scores <- data.table::as.data.table(h2o::h2o.predict(model,
                                                                 newdata = features)[, 1])
            data.table::setnames(Scores, "predict", "Class")
          } else if (tolower(ClassVals[i]) == "all") {
            Scores <- data.table::as.data.table(h2o::h2o.predict(model,
                                                                 newdata = features))
            data.table::setnames(Scores, "predict", "Class")
          } else {
            warning("ClassVals can only be Probs, Label, or All")
          }
        } else if (tolower(TargetType[i]) == "classification") {
          if (tolower(ClassVals[i]) == "p1") {
            Scores <- data.table::as.data.table(h2o::h2o.predict(model,
                                                                 newdata = features)[, 3])
          } else if (tolower(ClassVals[i]) == "probs") {
            Scores <- data.table::as.data.table(h2o::h2o.predict(model,
                                                                 newdata = features)[,-1])
          } else if (tolower(ClassVals[i]) == "label") {
            Scores <- data.table::as.data.table(h2o::h2o.predict(model,
                                                                 newdata = features)[, 1])
            data.table::setnames(Scores, "predict", "Class")
          } else if (tolower(ClassVals[i]) == "all") {
            Scores <- data.table::as.data.table(h2o::h2o.predict(model,
                                                                 newdata = features))
            data.table::setnames(Scores, "predict", "Class")
          } else {
            warning("ClassVals can only be Probs, Label, or All")
          }
        } else if (tolower(TargetType[i]) == "regression") {
          Scores <- data.table::as.data.table(h2o::h2o.predict(model,
                                                               newdata = features)[, 1])
        } else if (tolower(TargetType[i]) == c("text")) {
          name <- StoreFile[i, ModelName][[1]]
          data <- AutoH2OTextPrepScoring(
            data = Features,
            string = name,
            NThreads = NThreads,
            MaxMem = MaxMem
          )
          Scores <- data.table::as.data.table(h2o::h2o.transform(
            model,
            words = data,
            aggregate_method = "AVERAGE"
          ))
          setnames(Scores, names(Scores), paste0(name,
                                                 "_",
                                                 names(Scores)))
          Features <-
            cbind(Features[, paste0(name) := NULL], Scores)
        } else if (tolower(TargetType[i]) == "multioutcome") {
          Counts <- data.table::as.data.table(h2o::h2o.predict(model,
                                                               newdata = features)[1, 1])
          Temp <- data.table::as.data.table(h2o::h2o.predict(model,
                                                             newdata = features))
          Vals <-
            names(sort(Temp[1, 2:ncol(Temp)], decreasing = TRUE))
          Scores <- paste0(Vals, collapse = " ")
        } else if (tolower(TargetType[i]) == "clustering") {
          load(file = KMeansModelFile[i, FilePath1][[1]])
          load(file = KMeansModelFile[i, FilePath2][[1]])
          NewGLRM <-
            h2o::h2o.glrm(
              training_frame = features,
              init = "User",
              user_y = fitY
            )
          x_raw <-
            h2o::h2o.getFrame(NewGLRM@model$representation_name)
          Scores <-
            data.table::as.data.table(h2o::h2o.predict(object = KMeans,
                                                       newdata = x_raw))
          Scores <- cbind(data.table::as.data.table(Scores),
                          Features)
        } else {
          warning(
            "TargetType is not Multinomial,
          Classification, Regression, Text, Multioutcome,
          or Clustering."
          )
        }
      } else {
        warning("ScoreMethod must be Standard or Mojo")
      }
      if (H20ShutDown[i] && tolower(ScoreMethod) == "standard") {
        h2o::h2o.shutdown(prompt = FALSE)
      }
      if (any(tolower(TargetType) == "text")) {
        ScoresList <- Features
      } else {
        ScoresList[[i]] <- Scores
      }
    }
    return(ScoresList)
  }
}

#' Convert transactional data.table to a binary ratings matrix
#'
#' @author Adrian Antico and Douglas Pestana
#' @family Recommenders
#' @param data This is your transactional data.table. Must include an Entity (typically customer), ProductCode (such as SKU), and a sales metric (such as total sales).
#' @param EntityColName This is the column name in quotes that represents the column name for the Entity, such as customer
#' @param ProductColName This is the column name in quotes that represents the column name for the product, such as SKU
#' @param MetricColName This is the column name in quotes that represents the column name for the metric, such as total sales
#' @param ReturnMatrix Set to FALSE to coerce the object (desired route) or TRUE to return a matrix
#' @return A BinaryRatingsMatrix
#' @examples
#' data <- data.table::data.table(CustomerID = c(1,1,2,2,3,3),
#'                                StockCode = c("A","B","A","A","B","A"),
#'                                TotalSales = c(2,3,4,5,1,2))
#' RatingsMatrix <- AutoRecomDataCreate(data,
#'                                      EntityColName = "CustomerID",
#'                                      ProductColName = "StockCode",
#'                                      MetricColName = "TotalSales",
#'                                      ReturnMatrix = TRUE)
#' @export
AutoRecomDataCreate <- function(data,
                                EntityColName  = "CustomerID",
                                ProductColName = "StockCode",
                                MetricColName  = "TotalSales",
                                ReturnMatrix   = FALSE) {
  # Ensure data is data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Ensure EntityColName is character type----
  if (!is.character(data[1, get(EntityColName)])) {
    data[, eval(EntityColName) := as.character(get(EntityColName))]
  }
  
  # Ensure ProductColName is character type----
  if (!is.character(data[1, get(ProductColName)])) {
    data[, eval(ProductColName) := as.character(get(ProductColName))]
  }
  
  # Ensure MetricColName is numeric----
  if (!is.numeric(data[1, get(MetricColName)])) {
    data[, eval(MetricColName) := as.numeric(get(MetricColName))]
  }
  
  # Only keep the necessary columns----
  keep <- c(EntityColName, ProductColName, MetricColName)
  data <- data[, ..keep]
  
  # CREATE BINARY RATING MATRIX-----
  train_data <- data.table::dcast(
    data,
    get(EntityColName) ~ get(ProductColName),
    value.var = eval(MetricColName),
    fun.aggregate = function(x)
      sum(!is.na(x)),
    fill = 0
  )
  
  # Change name back to original----
  data.table::setnames(train_data,
                       "EntityColName",
                       eval(EntityColName))
  
  # Convert Sales data to Binary (60% faster than ifelse)----
  for (j in 2:ncol(train_data)) {
    data.table::set(train_data, which(train_data[[j]] > 0), j, 1)
    data.table::set(train_data, which(train_data[[j]] <= 0), j, 0)
  }
  
  # Store customerID for rownames----
  train_data_rownames <- train_data[[eval(EntityColName)]]
  
  # Remove CustomerID column----
  train_data[, eval(EntityColName) := NULL]
  
  # Convert train to matrix----
  train_data_matrix <- as.matrix(train_data)
  
  # Set rownames
  row.names(train_data_matrix) <- train_data_rownames
  
  # Return binary rating matrix----
  if (ReturnMatrix) {
    return(recommenderlab::coerce(from = train_data_matrix,
                                  to = "BinaryRatingsMatrix"))
  } else {
    return(methods::as(object = train_data_matrix,
                       Class = "binaryRatingMatrix"))
  }
}

#' Automatically build the best recommendere model among models available.
#'
#' This function returns the winning model that you pass onto AutoRecommenderScoring
#' @author Adrian Antico and Douglas Pestana
#' @family Recommenders
#' @param data This is your BinaryRatingsMatrix. See function RecomDataCreate
#' @param Partition Choose from "split", "cross-validation", "bootstrap". See evaluationScheme in recommenderlab for details.
#' @param KFolds Choose 2 for traditional train and test. Choose greater than 2 for the number of cross validations
#' @param Ratio The ratio for train and test. E.g. 0.75 for 75 percent data allocated to training
#' @param RatingType Choose from "TopN", "ratings", "ratingMatrix"
#' @param RatingsKeep The total ratings you wish to return. Default is 20.
#' @param SkipModels AssociationRules runs the slowest and may crash your system. Choose from: "AssociationRules","ItemBasedCF","UserBasedCF","PopularItems","RandomItems"
#' @param ModelMetric Choose from "Precision", "Recall", "TPR", or "FPR"
#' @examples
#' \donttest{
#' WinningModel <- AutoRecommender(RatingsMatrix,
#'                                 Partition = "Split",
#'                                 KFolds = 2,
#'                                 Ratio = 0.75,
#'                                 RatingType = "TopN",
#'                                 RatingsKeep = 20,
#'                                 SkipModels = "AssociationRules",
#'                                 ModelMetric = "TPR")
#' }
#' @return The winning model used for scoring in the AutoRecommenderScoring function
#' @export
AutoRecommender <- function(data,
                            Partition   = "Split",
                            KFolds      = 2,
                            Ratio       = 0.75,
                            RatingType  = "TopN",
                            RatingsKeep = 20,
                            SkipModels  = "AssociationRules",
                            ModelMetric = "TPR") {
  # Ensure data is proper----
  if (class(data)[1] != "binaryRatingMatrix") {
    warning("data must be of class binaryRatingMatrix")
  }
  
  # Ensure KFolds is correct----
  if (tolower(Partition) == "split") {
    KFolds <- 1
  }
  
  # Ensure Ratio is proper----
  if (abs(Ratio) > 1 | Ratio == 0) {
    warning("Ratio must be a decimal between 0 and 1.
         Default is 0.75")
  }
  
  # Ensure RatingType is real----
  if (tolower(RatingType) == "topn") {
    RatingType <- "topNList"
  } else if (tolower(RatingType) == "ratings") {
    RatingType <- "ratings"
  } else if (tolower(RatingType) == "ratingMatrix") {
    RatingType <- "ratingMatrix"
  }
  
  # Pick winning model based max TPR for 10th recommendation----
  if (tolower(ModelMetric) == "precision") {
    ModelMetric <- "precision"
  } else if (tolower(ModelMetric) == "recall") {
    ModelMetric <- "recall"
  } else if (tolower(ModelMetric) == "tpr") {
    ModelMetric <- "TPR"
  } else if (tolower(ModelMetric) == "fpr") {
    ModelMetric <- "FPR"
  } else {
    warning("ModelMetric not in list of usable metrics")
  }
  
  # Evaluation setup----
  scheme <- recommenderlab::evaluationScheme(
    data,
    method     = tolower(Partition),
    k          = KFolds,
    train      = Ratio,
    given      = 1,
    goodRating = 1
  )
  
  # Store algorithms in nested list----
  algorithms <- list(
    "RandomItems"  = list(name = "RANDOM",  param = NULL),
    "PopularItems" = list(name = "POPULAR", param = NULL),
    "UserBasedCF" = list(name = "UBCF",    param = NULL),
    "ItemBasedCF" = list(name = "IBCF",    param = NULL),
    "AssociationRules" = list(
      name = "AR",
      param = list(support = 0.001, confidence = 0.05)
    )
  )
  
  # Remove all algos in SkipModels----
  if (any(tolower(SkipModels) == "associationrules")) {
    algorithms[["AssociationRules"]] <- NULL
  }
  if (any(tolower(SkipModels) == "itembasedcf")) {
    algorithms[["ItemBasedCF"]] <- NULL
  }
  if (any(tolower(SkipModels) == "userbasedcf")) {
    algorithms[["UserBasedCF"]] <- NULL
  }
  if (any(tolower(SkipModels) == "popularitems")) {
    algorithms[["PopularItems"]] <- NULL
  }
  if (any(tolower(SkipModels) == "randomitems")) {
    algorithms[["RandomItems"]] <- NULL
  }
  if (length(algorithms) == 0) {
    warning("You must have at least one algorithm to run")
  }
  
  # evauluate predicted ratings from each algorithm----
  results <- recommenderlab::evaluate(
    x      = scheme,
    method = algorithms,
    type   = RatingType,
    n      = 1:RatingsKeep
  )
  
  # determine winning model - highest TPR for next best 10 products----
  # start by averaging Confusion Matrix for all k-fold runs
  n <- length(results)
  store <- list()
  for (i in 1:n) {
    temp <- data.table(recommenderlab::avg(results)[[i]])
    temp[, model := results[[i]]@method]
    temp[, n_products := seq(1:RatingsKeep)]
    store[[i]] <- temp
  }
  
  # Collect results in one data.table----
  x <- data.table::rbindlist(store)
  
  WinningModel <-
    x[n_products == 10][order(-get(ModelMetric))][1, "model"][[1]]
  return(WinningModel)
}

#' The AutoRecomScoring function scores recommender models from AutoRecommender()
#'
#' This function will take your ratings matrix and model and score your data in parallel.
#' @author Adrian Antico and Douglas Pestana
#' @family Recommenders
#' @param data The binary ratings matrix from RecomDataCreate()
#' @param WinningModel The winning model returned from AutoRecommender()
#' @param EntityColName Typically your customer ID
#' @param ProductColName Something like "StockCode"
#' @return Returns the prediction data
#' @examples
#' \donttest{
#' # F(G(Z(x))): AutoRecommenderScoring(AutoRecommender(RecomDataCreate(TransactionData)))
#' Results <- AutoRecommenderScoring(
#'   data = AutoRecomDataCreate(
#'       data,
#'       EntityColName = "CustomerID",
#'       ProductColName = "StockCode",
#'       MetricColName = "TotalSales"),
#'   WinningModel = AutoRecommender(
#'       AutoRecomDataCreate(
#'         data,
#'         EntityColName = "CustomerID",
#'         ProductColName = "StockCode",
#'         MetricColName = "TotalSales"),
#'       Partition = "Split",
#'       KFolds = 2,
#'       Ratio = 0.75,
#'       RatingType = "TopN",
#'       RatingsKeep = 20,
#'       SkipModels = "AssociationRules",
#'       ModelMetric = "TPR"),
#'   EntityColName = "CustomerID",
#'   ProductColName = "StockCode")
#' }
#' @export
AutoRecommenderScoring <- function(data,
                                   WinningModel,
                                   EntityColName  = "CustomerID",
                                   ProductColName = "StockCode") {
  requireNamespace('parallel', quietly = FALSE)
  requireNamespace('doParallel', quietly = FALSE)
  
  # Setup winning model and arguments
  if (WinningModel == "AR") {
    recommender <- recommenderlab::Recommender(
      data = data,
      method = "AR",
      parameter = list(support = 0.001,
                       confidence = 0.05)
    )
  } else {
    recommender <- recommenderlab::Recommender(data = data,
                                               method = WinningModel)
  }
  
  # Setup the parallel environment
  packages <- c("curl", "reshape2", "recommenderlab", "data.table")
  cores    <- 8
  parts    <- floor(nrow(data) * ncol(data) / 250000)
  cl       <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(cl)
  
  # Begin scoring
  results <- foreach::foreach(
    i = itertools::isplitRows(data,
                              chunks = parts),
    .combine = function(...)
      data.table::rbindlist(list(...)),
    .multicombine = TRUE,
    .packages = packages
  ) %dopar% {
    data <- methods::as(recommenderlab::predict(recommender,
                                                i,
                                                type = "topNList",
                                                n = 10),
                        "list")
    
    # Data transformations
    temp <- data.table::data.table(data.table::melt(data))
    data.table::setcolorder(temp, c(2, 1))
    data.table::setnames(temp,
                         c("L1", "value"),
                         c(EntityColName, ProductColName))
    temp
  }
  
  # shut down parallel objects
  parallel::stopCluster(cl)
  rm(cl)
  
  # Finalize data transformations: append list of data.tables, add ProductRank, gsub x 2, add ts
  results[, ProductRank := seq_len(.N), by = eval(EntityColName)]
  results[, ':=' (TimeStamp = as.character(Sys.time()))]
  
  return(results)
}

#' AutoCatBoostClassifier is an automated catboost model grid-tuning classifier and evaluation system
#'
#' AutoCatBoostClassifier is an automated modeling function that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train, validation, and test sets (if not supplied). Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions (on test data), an ROC plot, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located, but not mixed types. Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located, but not mixed types. Also, not zero-indexed.
#' @param PrimaryDateColumn Supply a date or datetime column for catboost to utilize time as its basis for handling categorical features, instead of random shuffling
#' @param ClassWeights Supply a vector of weights for your target classes. E.g. c(0.25, 1) to weight your 0 class by 0.25 and your 1 class by 1.
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param task_type Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param eval_metric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "AUC" is the default, but other options include "Logloss", "CrossEntropy", "Precision", "Recall", "F1", "BalancedAccuracy", "BalancedErrorRate", "MCC", "Accuracy", "CtrFactor", "AUC", "BrierScore", "HingeLoss", "HammingLoss", "ZeroOneLoss", "Kappa", "WKappa", "LogLikelihoodOfPrediction"
#' @param grid_eval_metric This is the metric used to find the threshold "f", "auc", "tpr", "fnr", "fpr", "tnr", "prbe", "f", "odds"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options. 1080 total possible options
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects. E.g. plots and evaluation metrics
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param PassInGrid Defaults to NULL. Pass in a single row of grid from a previous output as a data.table (they are collected as data.tables)
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' data[, Target := ifelse(Target < 0.5, 1, 0)]
#' TestModel <- AutoCatBoostClassifier(data,
#'                                     ValidationData = NULL,
#'                                     TestData = NULL,
#'                                     TargetColumnName = "Target",
#'                                     FeatureColNames = c(2:12),
#'                                     PrimaryDateColumn = NULL,
#'                                     ClassWeights = NULL,
#'                                     IDcols = NULL,
#'                                     MaxModelsInGrid = 3,
#'                                     task_type = "GPU",
#'                                     eval_metric = "AUC",
#'                                     grid_eval_metric = "auc",
#'                                     Trees = 50,
#'                                     GridTune = FALSE,
#'                                     model_path = NULL,
#'                                     ModelID = "ModelTest",
#'                                     NumOfParDepPlots = 15,
#'                                     ReturnModelObjects = TRUE,
#'                                     SaveModelObjects = FALSE,
#'                                     PassInGrid = NULL)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model (the model), ValidationData.csv, ROC_Plot.png, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoCatBoostClassifier <- function(data,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   ClassWeights = NULL,
                                   IDcols = NULL,
                                   task_type = "GPU",
                                   eval_metric = "AUC",
                                   Trees = 50,
                                   GridTune = FALSE,
                                   grid_eval_metric = "f",
                                   MaxModelsInGrid = 10,
                                   model_path = NULL,
                                   ModelID = "FirstModel",
                                   NumOfParDepPlots = 3,
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE,
                                   PassInGrid = NULL) {
  # Load catboost----
  loadNamespace(package = "catboost")
  
  # Binary Check Arguments----
  if (!(tolower(task_type) %chin% c("gpu", "cpu")))
    warning("task_type needs to be either 'GPU' or 'CPU'")
  if (!(
    tolower(eval_metric) %chin% c(
      "logloss",
      "crossentropy",
      "precision",
      "recall",
      "f1",
      "balancedaccuracy",
      "balancederrorrate",
      "mcc",
      "accuracy",
      "ctrfactor",
      "auc",
      "brierscore",
      "hingeloss",
      "hammingloss",
      "zerooneloss",
      "kappa",
      "wkappa",
      "loglikelihoodofprediction"
    )
  )) {
    warning(
      "eval_metric not in c('Logloss','CrossEntropy',
                          'Precision','Recall',
                          'F1','BalancedAccuracy',
                          'BalancedErrorRate','MCC',
                          'Accuracy','CtrFactor',
                          'AUC','BrierScore',
                          'HingeLoss','HammingLoss',
                          'ZeroOneLoss','Kappa',
                          'WKappa','LogLikelihoodOfPrediction')"
    )
    
  }
  if (!is.null(ClassWeights)) {
    LossFunction <- "Logloss"
  } else {
    LossFunction <- "CrossEntropy"
  }
  if (!is.null(PrimaryDateColumn)) {
    HasTime <- TRUE
  } else {
    HasTime <- FALSE
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (!(
    tolower(grid_eval_metric) %chin% c(
      "accuracy",
      "auc",
      "tpr",
      "fnr",
      "fpr",
      "tnr",
      "prbe",
      "f",
      "odds",
      "chisq"
    )
  )) {
    warning(
      "grid_eval_metric not in c('accuracy','auc','tpr','fnr','fpr','tnr','prbe','f','odds','chisq')"
    )
  }
  if (MaxModelsInGrid < 1 |
      MaxModelsInGrid > 1080 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1 and less than 1080")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path))
      warning("model_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (NumOfParDepPlots < 0)
    warning("NumOfParDepPlots needs to be a positive number")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  
  # Binary Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Binary Ensure ValidationData is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # Binary Ensure TestData is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # Binary Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # Binary IDcol Name Storage----
  if (!is.null(IDcols)) {
    if (!is.character(IDcols)) {
      IDcols <- names(data)[IDcols]
    }
  }
  
  # Binary Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = eval(Target),
      TimeColumnName = NULL
    )
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # Binary Sort data if PrimaryDateColumn----
  if (!is.null(PrimaryDateColumn)) {
    data <- data[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      # data[, eval(PrimaryDateColumn) := NULL]
      data.table::set(data,
                      j = eval(PrimaryDateColumn),
                      value = NULL)
    }
  }
  
  # Binary Sort ValidationData if PrimaryDateColumn----
  if (!is.null(PrimaryDateColumn)) {
    ValidationData <- ValidationData[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      data.table::set(ValidationData,
                      j = eval(PrimaryDateColumn),
                      value = NULL)
    }
  }
  
  # Binary Sort TestData if PrimaryDateColumn----
  if (!is.null(TestData)) {
    if (!is.null(PrimaryDateColumn)) {
      TestData <- TestData[order(get(PrimaryDateColumn))]
      if (!(eval(PrimaryDateColumn) %in% IDcols)) {
        data.table::set(TestData,
                        j = eval(PrimaryDateColumn),
                        value = NULL)
      }
    }
  }
  
  # Binary data Subset Columns Needed----
  if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    keep1 <- names(data)[c(FeatureColNames)]
    keep <- c(keep1, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  } else {
    keep <- c(FeatureColNames, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  }
  
  # Binary TestData Subset Columns Needed----
  if (!is.null(TestData)) {
    if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
      keep1 <- names(TestData)[c(FeatureColNames)]
      if (!is.null(IDcols)) {
        keep <- c(IDcols, keep1, Target)
      } else {
        keep <- c(keep1, Target)
      }
      TestData <- TestData[, ..keep]
    } else {
      keep1 <- c(FeatureColNames)
      if (!is.null(IDcols)) {
        keep <- c(IDcols, FeatureColNames, Target)
      } else {
        keep <- c(FeatureColNames, Target)
      }
      TestData <- TestData[, ..keep]
    }
    if (!is.null(IDcols)) {
      TestMerge <- data.table::copy(TestData)
      keep <- c(keep1, Target)
      TestData <- TestData[, ..keep]
    } else {
      TestMerge <- data.table::copy(TestData)
    }
  }
  
  # Binary Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(
    sapply(data, is.factor)
  )),
  as.numeric(which(
    sapply(data, is.character)
  ))))
  
  # Binary Convert CatFeatures to 1-indexed----
  if (length(CatFeatures) > 0) {
    for (i in seq_len(length(CatFeatures))) {
      CatFeatures[i] <- CatFeatures[i] - 1
    }
  }
  
  # Binary Train ModelDataPrep----
  dataTrain <- ModelDataPrep(
    data = dataTrain,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = TRUE,
    MissFactor = "0",
    MissNum = -1
  )
  
  # Binary Validation ModelDataPrep----
  dataTest <- ModelDataPrep(
    data = dataTest,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = TRUE,
    MissFactor = "0",
    MissNum = -1
  )
  
  # Binary Test ModelDataPrep----
  if (!is.null(TestData)) {
    TestData <- ModelDataPrep(
      data = TestData,
      Impute = TRUE,
      CharToFactor = TRUE,
      RemoveDates = TRUE,
      MissFactor = "0",
      MissNum = -1
    )
  }
  
  # Binary Save Names of data----
  Names <- data.table::as.data.table(names(data))
  data.table::setnames(Names, "V1", "ColNames")
  if (SaveModelObjects) {
    data.table::fwrite(Names, paste0(model_path, "/", ModelID, "_ColNames.csv"))
  }
  
  # Binary Subset Target Variables----
  TrainTarget <-
    tryCatch({
      dataTrain[, get(Target)]
    }, error = function(x)
      dataTrain[, eval(Target)])
  TestTarget <-
    tryCatch({
      dataTest[, get(Target)]
    }, error = function(x)
      dataTest[, eval(Target)])
  if (!is.null(TestData)) {
    FinalTestTarget <-
      tryCatch({
        TestData[, get(Target)]
      }, error = function(x)
        TestData[, eval(Target)])
  }
  
  # Binary Initialize Catboost Data Conversion----
  if (!is.null(CatFeatures)) {
    if (!is.null(TestData)) {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                     label = TrainTarget,
                                     cat_features = CatFeatures)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures, )
      FinalTestPool <-
        catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget, cat_features = CatFeatures)
    } else {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                     label = TrainTarget,
                                     cat_features = CatFeatures)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
    }
  } else {
    if (!is.null(TestData)) {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
      FinalTestPool <-
        catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget)
    } else {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
    }
  }
  
  # Binary Grid Tune or Not Check----
  if (GridTune) {
    # Binary Grid Create data.table To Store Results----
    GridCollect <-
      data.table::data.table(
        ParamRow = 1:(MaxModelsInGrid + 1),
        EvalStat = rep(9999999, MaxModelsInGrid + 1)
      )
    
    # Binary Grid Define Hyper Parameters----
    if (!is.null(PassInGrid)) {
      if (!data.table::is.data.table(PassInGrid)) {
        PassInGrid <- data.table::as.data.table(PassInGrid)
      }
      catboostGridList <- data.table::CJ(
        l2_leaf_reg = c(0, 1, 2, 3),
        learning_rate = c(0.01, 0.02, 0.03, 0.04, 0.05),
        bootstrap_type = c("Poisson", "Bayesian", "Bernoulli", "No"),
        depth = c(4:12)
      )
      if (tolower(task_type) != "gpu") {
        catboostGridList <- catboostGridList[bootstrap_type != "Poisson"]
      }
      catboostGridList[, ID := runif(nrow(catboostGridList))]
      catboostGridList <-
        catboostGridList[order(ID)][1:(MaxModelsInGrid)][, ID := NULL]
      catboostGridList <-
        data.table::rbindlist(list(PassInGrid, catboostGridList))
    } else {
      catboostGridList <- data.table::CJ(
        l2_leaf_reg = c(0, 1, 2, 3),
        learning_rate = c(0.01, 0.02, 0.03, 0.04, 0.05),
        bootstrap_type = c("Poisson", "Bayesian", "Bernoulli", "No"),
        depth = c(4:12)
      )
      if (tolower(task_type) != "gpu") {
        catboostGridList <- catboostGridList[bootstrap_type != "Poisson"]
      }
      catboostGridList[, ID := runif(nrow(catboostGridList))]
      catboostGridList <-
        catboostGridList[order(ID)][1:(MaxModelsInGrid + 1)][, ID := NULL]
    }
    
    # Binary AUC List----
    AUC_List <- list()
    
    # Binary Grid Tuning Main Loop----
    for (i in as.integer(seq_len(MaxModelsInGrid + 1))) {
      # Print i
      print(i)
      
      # Binary Grid Define Base Parameters----
      if (!is.null(ClassWeights)) {
        base_params <- list(
          iterations           = Trees,
          loss_function        = LossFunction,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type,
          class_weights        = ClassWeights
        )
      } else {
        base_params <- list(
          iterations           = Trees,
          loss_function        = LossFunction,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
        )
      }
      
      # Binary Grid Merge Model Parameters----
      # Have first model be the baseline model
      if (i != 1) {
        base_params <- c(as.list(catboostGridList[i,]), base_params)
      }
      
      # Binary Grid Train Model----
      model <- catboost::catboost.train(learn_pool = TrainPool,
                                        test_pool  = TestPool,
                                        params     = base_params)
      
      # Binary Grid Score Model----
      if (!is.null(TestData)) {
        predict <- catboost::catboost.predict(
          model = model,
          pool = FinalTestPool,
          prediction_type = "Probability",
          thread_count = -1
        )
      } else {
        predict <- catboost::catboost.predict(
          model = model,
          pool = TestPool,
          prediction_type = "Probability",
          thread_count = -1
        )
      }
      
      # Binary Remove Model and Collect Garbage----
      rm(model)
      gc()
      
      # Binary Grid Validation Data----
      if (!is.null(TestData)) {
        calibEval <-
          data.table::as.data.table(cbind(Target = FinalTestTarget, p1 = predict))
      } else {
        calibEval <-
          data.table::as.data.table(cbind(Target = TestTarget, p1 = predict))
      }
      
      # Binary Grid Evaluation Metrics for Each Grid----
      if (tolower(grid_eval_metric) == "accuracy") {
        j <- 0
        x <- data.table::data.table(
          Metric = "Accuracy",
          MetricValue = 5.0,
          Threshold = seq(0.01, 0.99, 0.001)
        )
        for (k in unique(x[["Threshold"]])) {
          j = as.integer(j + 1)
          Accuracy <-
            mean(calibEval[, ifelse(p1 > k &
                                      Target == 1 |
                                      p1 < k & Target == 0, 1, 0)])
          data.table::set(x,
                          i = j,
                          j = 2L,
                          value = round(Accuracy, 4))
        }
        data.table::setorderv(x,
                              "MetricValue",
                              order = -1,
                              na.last = TRUE)
        Metric <- x[1, MetricValue]
      } else {
        x <-
          ROCR::prediction(predictions = calibEval[["p1"]], labels = calibEval[["Target"]])
        y <-
          ROCR::performance(prediction.obj = x, measure = grid_eval_metric)
        if (any(
          nrow(data.table::as.data.table(y@y.values)) <= 1 |
          nrow(data.table::as.data.table(y@x.values)) <= 1
        )) {
          if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
              nrow(data.table::as.data.table(y@x.values)) <= 1) {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values,
                Threshold = y@x.values
              ))
            Metric <- z[[1]]
          } else if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
                     !(nrow(data.table::as.data.table(y@x.values) <= 1))) {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values,
                Threshold = y@x.values[[1]]
              ))
            Metric <- z[!is.infinite(Threshold)][[1]]
          } else if (!(nrow(data.table::as.data.table(y@y.values)) <= 1) &
                     nrow(data.table::as.data.table(y@x.values) <= 1)) {
            if (grid_eval_metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
              z <-
                data.table::as.data.table(cbind(
                  Metric = y@y.values[[1]],
                  Threshold = y@x.values
                ))
              Metric <-
                z[order(-Metric)][!is.infinite(Metric)][[1]]
            } else {
              z <-
                data.table::as.data.table(cbind(
                  Metric = y@y.values[[1]],
                  Threshold = y@x.values
                ))
              Metric <-
                z[order(Metric)][!is.infinite(Metric)][[1]]
            }
          }
        } else {
          if (metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values[[1]]
              ))
            Metric <-
              z[order(-Metric)][!is.infinite(Threshold) &
                                  !is.infinite(Metric)][1,]
          } else {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values[[1]]
              ))
            Metric <-
              z[order(Metric)][!is.infinite(Threshold) &
                                 !is.infinite(Metric)][1,]
          }
        }
      }
      
      # Binary AUC Object Create----
      AUC_Metrics <- pROC::roc(
        response = calibEval[["Target"]],
        predictor = calibEval[["p1"]],
        na.rm = TRUE,
        algorithm = 3,
        auc = TRUE,
        ci = TRUE
      )
      
      # Binary AUC Conversion to data.table----
      AUC_List[[i]] <- data.table::data.table(
        ModelNumber = i,
        Sensitivity = as.numeric(AUC_Metrics$sensitivities + 0.0001),
        Specificity = as.numeric(AUC_Metrics$specificities + 0.0001)
      )
      
      # Collect Metrics and Corresponding Grids
      # Store Output Information
      if (tolower(grid_eval_metric) == "accuracy") {
        data.table::set(GridCollect,
                        i = i,
                        j = 1L,
                        value = i)
        data.table::set(GridCollect,
                        i = i,
                        j = 2L,
                        value = Metric)
      } else if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                     nrow(data.table::as.data.table(y@x.values)) <= 1)) {
        data.table::set(GridCollect,
                        i = i,
                        j = 1L,
                        value = i)
        data.table::set(GridCollect,
                        i = i,
                        j = 2L,
                        value = Metric)
      } else {
        data.table::set(GridCollect,
                        i = i,
                        j = 1L,
                        value = i)
        data.table::set(GridCollect,
                        i = i,
                        j = 2L,
                        value = Metric[, 1])
      }
    }
  }
  
  # Binary Define Final Model Parameters----
  if (GridTune) {
    if (grid_eval_metric %chin% c("accuracy", "auc", "tpr", "tnr", "prbe", "f", "odds")) {
      BestGrid <- GridCollect[order(-EvalStat)][1, ParamRow]
      if (BestGrid == 1) {
        BestThresh <- GridCollect[order(-EvalStat)][1, EvalStat]
        if (!is.null(ClassWeights)) {
          base_params <- list(
            iterations           = Trees,
            loss_function        = LossFunction,
            eval_metric          = eval_metric,
            use_best_model       = TRUE,
            has_time             = HasTime,
            best_model_min_trees = 10,
            metric_period        = 10,
            task_type            = task_type,
            class_weights        = ClassWeights
          )
        } else {
          base_params <- list(
            iterations           = Trees,
            loss_function        = LossFunction,
            eval_metric          = eval_metric,
            use_best_model       = TRUE,
            has_time             = HasTime,
            best_model_min_trees = 10,
            metric_period        = 10,
            task_type            = task_type
          )
        }
      } else {
        BestThresh <- GridCollect[order(-EvalStat)][1, EvalStat]
        if (!is.null(ClassWeights)) {
          base_params <- list(
            iterations           = Trees,
            loss_function        = LossFunction,
            eval_metric          = eval_metric,
            use_best_model       = TRUE,
            has_time             = HasTime,
            best_model_min_trees = 10,
            metric_period        = 10,
            task_type            = task_type,
            class_weights        = ClassWeights
          )
        } else {
          base_params <- list(
            iterations           = Trees,
            loss_function        = LossFunction,
            eval_metric          = eval_metric,
            use_best_model       = TRUE,
            has_time             = HasTime,
            best_model_min_trees = 10,
            metric_period        = 10,
            task_type            = task_type
          )
        }
        base_params <-
          c(as.list(catboostGridList[BestGrid,]), base_params)
      }
    } else {
      BestGrid <- GridCollect[order(EvalStat)][1, ParamRow]
      BestThresh <- GridCollect[order(EvalStat)][1, EvalStat]
    }
    if (!is.null(ClassWeights)) {
      base_params <- list(
        iterations           = Trees,
        loss_function        = LossFunction,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        has_time             = HasTime,
        best_model_min_trees = 10,
        metric_period        = 10,
        task_type            = task_type,
        class_weights        = ClassWeights
      )
    } else {
      base_params <- list(
        iterations           = Trees,
        loss_function        = LossFunction,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        has_time             = HasTime,
        best_model_min_trees = 10,
        metric_period        = 10,
        task_type            = task_type
      )
    }
    base_params <-
      c(as.list(catboostGridList[BestGrid,]), base_params)
  } else {
    if (!is.null(ClassWeights)) {
      base_params <- list(
        iterations           = Trees,
        loss_function        = LossFunction,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        has_time             = HasTime,
        best_model_min_trees = 10,
        metric_period        = 10,
        task_type            = task_type,
        class_weights        = ClassWeights
      )
    } else {
      base_params <- list(
        iterations           = Trees,
        loss_function        = LossFunction,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        has_time             = HasTime,
        best_model_min_trees = 10,
        metric_period        = 10,
        task_type            = task_type
      )
    }
    if (!is.null(PassInGrid)) {
      base_params <- c(base_params, as.list(PassInGrid[1, ]))
    }
  }
  
  # Binary Train Final Model----
  model <- catboost::catboost.train(learn_pool = TrainPool,
                                    test_pool  = TestPool,
                                    params     = base_params)
  
  # Binary Save Model----
  if (SaveModelObjects) {
    catboost::catboost.save_model(model = model,
                                  model_path = paste0(model_path, "/", ModelID))
  }
  
  # Binary Score Final Test Data----
  if (!is.null(TestData)) {
    predict <- catboost::catboost.predict(
      model = model,
      pool = FinalTestPool,
      prediction_type = "Probability",
      thread_count = -1
    )
  } else {
    predict <- catboost::catboost.predict(
      model = model,
      pool = TestPool,
      prediction_type = "Probability",
      thread_count = -1
    )
  }
  
  # Binary Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, p1 = predict))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(Target = TestTarget, dataTest, p1 = predict))
  }
  
  # Save Validation Data to File----
  if (SaveModelObjects) {
    data.table::fwrite(ValidationData,
                       file = paste0(model_path, "/", ModelID, "_ValidationData.csv"))
  }
  
  # Binary AUC Object Create----
  AUC_Metrics <- pROC::roc(
    response = ValidationData[["Target"]],
    predictor = ValidationData[["p1"]],
    na.rm = TRUE,
    algorithm = 3,
    auc = TRUE,
    ci = TRUE
  )
  
  # Binary AUC Conversion to data.table----
  AUC_Data <- data.table::data.table(
    ModelNumber = 0,
    Sensitivity = AUC_Metrics$sensitivities,
    Specificity = AUC_Metrics$specificities
  )
  
  # Binary Rbind AUC
  if (GridTune == TRUE & MaxModelsInGrid <= 15) {
    temp <- data.table::rbindlist(AUC_List)
    AUC_Data <- data.table::rbindlist(list(temp, AUC_Data))
    AUC_Data[, ModelNumber := as.factor(ModelNumber)]
    
    # Binary Plot ROC Curve----
    ROC_Plot <-
      ggplot2::ggplot(AUC_Data,
                      ggplot2::aes(
                        x = 1 - Specificity,
                        group = ModelNumber,
                        color = ModelNumber
                      )) +
      ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]])) +
      ggplot2::geom_abline(slope = 1, color = "black") +
      ggplot2::ggtitle(paste0(
        "Catboost Best Model AUC: ",
        100 * round(AUC_Metrics$auc, 3),
        "%"
      )) +
      ChartTheme() + ggplot2::xlab("Specificity") +
      ggplot2::ylab("Sensitivity")
    
  } else {
    ROC_Plot <-
      ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity)) +
      ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]]), color = "blue") +
      ggplot2::geom_abline(slope = 1, color = "black") +
      ggplot2::ggtitle(paste0("Catboost AUC: ",
                              100 * round(AUC_Metrics$auc, 3), "%")) +
      ChartTheme() + ggplot2::xlab("Specificity") +
      ggplot2::ylab("Sensitivity")
  }
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path, "/", ModelID, "_ROC_Plot.png"))
  }
  
  # Binary Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "p1",
    TargetColName = "Target",
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Add Number of Trees to Title
  EvaluationPlot <- EvaluationPlot +
    ggplot2::ggtitle(paste0(
      "Calibration Evaluation Plot: AUC = ",
      round(AUC_Metrics$auc, 3)
    ))
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path, "/", ModelID, "_EvaluationPlot.png"))
  }
  
  # Evaluation Metrics at Optimial Threshold----
  x <- ROCR::prediction(predictions = ValidationData[["p1"]],
                        labels = ValidationData[["Target"]])
  EvaluationMetrics <-
    data.table::data.table(
      Metric = c(
        "AUC",
        "TruePositiveRate",
        "FalseNegativeRate",
        "FalsePositiveRate",
        "TrueNegativeRate",
        "PreceisionRecallBreakEven",
        "F1_Score",
        "Odds"
      ),
      MetricValue = rep(999999, 8),
      Threshold   = rep(999999, 8)
    )
  i <- 0
  for (metric in c("auc", "tpr", "fnr", "fpr", "tnr", "prbe", "f", "odds")) {
    i <- as.integer(i + 1)
    tryCatch({
      y <- ROCR::performance(prediction.obj = x, measure = metric)
      if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
              nrow(data.table::as.data.table(y@x.values)) <= 1)) {
        if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
            nrow(data.table::as.data.table(y@x.values)) <= 1) {
          z <-
            data.table::as.data.table(cbind(
              Metric = y@y.values,
              Threshold = y@x.values
            ))
          Metric <- z[[1]]
        } else if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
                   !(nrow(data.table::as.data.table(y@x.values) <= 1))) {
          z <-
            data.table::as.data.table(cbind(
              Metric = y@y.values,
              Threshold = y@x.values[[1]]
            ))
          Metric <- z[!is.infinite(Threshold)][[1]]
        } else if (!(nrow(data.table::as.data.table(y@y.values)) <= 1) &
                   nrow(data.table::as.data.table(y@x.values) <= 1)) {
          if (metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values
              ))
            Metric <- z[order(-Metric)][!is.infinite(Metric)][[1]]
          } else {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values
              ))
            Metric <- z[order(Metric)][!is.infinite(Metric)][[1]]
          }
        }
      } else {
        if (metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
          z <-
            data.table::as.data.table(cbind(
              Metric = y@y.values[[1]],
              Threshold = y@x.values[[1]]
            ))
          Metric <-
            z[order(-Metric)][!is.infinite(Threshold) &
                                !is.infinite(Metric)][1,]
        } else {
          z <-
            data.table::as.data.table(cbind(
              Metric = y@y.values[[1]],
              Threshold = y@x.values[[1]]
            ))
          Metric <-
            z[order(Metric)][!is.infinite(Threshold) &
                               !is.infinite(Metric)][1,]
        }
      }
      
      # Store Output Information
      if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
              nrow(data.table::as.data.table(y@x.values)) <= 1)) {
        data.table::set(
          EvaluationMetrics,
          i = i,
          j = 2L,
          value = round(Metric[[1]], 4)
        )
        data.table::set(EvaluationMetrics,
                        i = i,
                        j = 3L,
                        value = NA)
      } else {
        data.table::set(
          EvaluationMetrics,
          i = i,
          j = 2L,
          value = round(Metric[[1]], 4)
        )
        data.table::set(
          EvaluationMetrics,
          i = i,
          j = 3L,
          value = Metric[[2]]
        )
      }
    }, error = function(x)
      "skip")
  }
  
  # Binary Accuracy Threshold and Metric----
  j <- 0
  x <-
    data.table::data.table(
      Metric = "Accuracy",
      MetricValue = 5.0,
      Threshold = seq(0.01, 0.99, 0.001)
    )
  for (i in unique(x[["Threshold"]])) {
    j = as.integer(j + 1)
    Accuracy <-
      mean(ValidationData[, ifelse(p1 > i &
                                     Target == 1 |
                                     p1 < i & Target == 0, 1, 0)])
    data.table::set(x,
                    i = j,
                    j = 2L,
                    value = round(Accuracy, 4))
  }
  data.table::setorderv(x, "MetricValue", order = -1, na.last = TRUE)
  x <- x[1,]
  EvaluationMetrics <-
    data.table::rbindlist(list(EvaluationMetrics, x))
  
  # Save EvaluationMetrics to File
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  if (SaveModelObjects) {
    data.table::fwrite(EvaluationMetrics,
                       file = paste0(model_path, "/", ModelID, "_EvaluationMetrics.csv"))
  }
  
  # Binary Variable Importance----
  temp <- catboost::catboost.get_feature_importance(model)
  VariableImportance <-
    data.table::data.table(cbind(Variable = rownames(temp), temp))
  data.table::setnames(VariableImportance, "V2", "Importance")
  VariableImportance[, Importance := round(as.numeric(Importance), 4)]
  VariableImportance <- VariableImportance[order(-Importance)]
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path, "/", ModelID, "_VariableImportance.csv"))
  }
  
  # Binary Partial Dependence----
  ParDepPlots <- list()
  j <- 0
  ParDepBoxPlots <- list()
  k <- 0
  for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
    tryCatch({
      Out <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "p1",
        TargetColName = "Target",
        IndepVar = VariableImportance[i, Variable],
        GraphType = "calibration",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      j <- j + 1
      ParDepPlots[[paste0(VariableImportance[j, Variable])]] <-
        Out
    }, error = function(x)
      "skip")
  }
  
  # Binary Save ParDepPlots to file----
  if (SaveModelObjects) {
    save(ParDepPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepPlots.R"))
  }
  
  # Binary Save GridCollect and catboostGridList----
  if (SaveModelObjects & GridTune == TRUE) {
    data.table::fwrite(catboostGridList,
                       file = paste0(model_path, "/", ModelID, "_catboostGridList.csv"))
    data.table::fwrite(GridCollect,
                       file = paste0(model_path, "/", ModelID, "_GridCollect.csv"))
  }
  
  # Final Garbage Collection----
  if (tolower(task_type) == "gpu") {
    gc()
  }
  
  # Binary Return Model Objects----
  if (GridTune) {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          ROC_Plot = ROC_Plot,
          EvaluationPlot = EvaluationPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          PartialDependencePlots = ParDepPlots,
          GridList = catboostGridList,
          GridMetrics = GridCollect,
          ColNames = Names
        )
      )
    }
  } else {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          ROC_Plot = ROC_Plot,
          EvaluationPlot = EvaluationPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          PartialDependencePlots = ParDepPlots,
          ColNames = Names
        )
      )
    }
  }
}

#' AutoCatBoostRegression is an automated catboost model grid-tuning classifier and evaluation system
#'
#' AutoCatBoostRegression is an automated modeling function that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param PrimaryDateColumn Supply a date or datetime column for catboost to utilize time as its basis for handling categorical features, instead of random shuffling
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param task_type Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param eval_metric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "RMSE" is the default, but other options include: "MAE", "MAPE", "Poisson", "Quantile", "LogLinQuantile", "Lq", "NumErrors", "SMAPE", "R2", "MSLE", "MedianAbsoluteError".
#' @param Alpha This is the quantile value you want to use for quantile regression. Must be a decimal between 0 and 1.
#' @param grid_eval_metric This is the metric used to find the threshold 'poisson', 'mae', 'mape', 'mse', 'msle', 'kl', 'cs', 'r2'
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param PassInGrid Defaults to NULL. Pass in a single row of grid from a previous output as a data.table (they are collected as data.tables)
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' TestModel <- AutoCatBoostRegression(data,
#'                                     ValidationData = NULL,
#'                                     TestData = NULL,
#'                                     TargetColumnName = "Target",
#'                                     FeatureColNames = c(2:12),
#'                                     PrimaryDateColumn = NULL,
#'                                     IDcols = NULL,
#'                                     MaxModelsInGrid = 1,
#'                                     task_type = "GPU",
#'                                     eval_metric = "RMSE",
#'                                     grid_eval_metric = "r2",
#'                                     Trees = 50,
#'                                     GridTune = FALSE,
#'                                     model_path = NULL,
#'                                     ModelID = "ModelTest",
#'                                     NumOfParDepPlots = 3,
#'                                     ReturnModelObjects = TRUE,
#'                                     SaveModelObjects = FALSE,
#'                                     PassInGrid = NULL)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and catboostgrid
#' @export
AutoCatBoostRegression <- function(data,
                                   ValidationData,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   IDcols = NULL,
                                   task_type = "GPU",
                                   eval_metric = "RMSE",
                                   Alpha = NULL,
                                   Trees = 50,
                                   GridTune = FALSE,
                                   grid_eval_metric = "mae",
                                   MaxModelsInGrid = 10,
                                   model_path = NULL,
                                   ModelID = "FirstModel",
                                   NumOfParDepPlots = 3,
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE,
                                   PassInGrid = NULL) {
  # Load catboost----
  loadNamespace(package = "catboost")
  
  # Regression Check Arguments----
  if (!(tolower(task_type) %chin% c("gpu", "cpu")))
    warning("task_type needs to be either 'GPU' or 'CPU'")
  if (!(
    tolower(eval_metric) %chin% c(
      "rmse",
      "mae",
      "mape",
      "poisson",
      "quantile",
      "loglinquantile",
      "lq",
      "numerrors",
      "smape",
      "r2",
      "msle",
      "medianabsoluteerror"
    )
  )) {
    warning(
      "eval_metric not in c(RMSE,MAE,MAPE,Poisson,Quantile,
         LogLinQuantile,Lq,NumErrors,SMAPE,R2,MSLE,MedianAbsoluteError)"
    )
    
  }
  if (!is.null(PrimaryDateColumn)) {
    HasTime <- TRUE
  } else {
    HasTime <- FALSE
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (!(
    tolower(grid_eval_metric) %chin% c("poisson", "mae", "mape", "mse", "msle", "kl", "cs", "r2")
  )) {
    warning(
      "grid_eval_metric not in c('poisson','mae','mape','mse','msle','kl','cs','r2')"
    )
  }
  if (MaxModelsInGrid < 1 |
      MaxModelsInGrid > 1080 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1 and less than 1080")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path))
      warning("model_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (NumOfParDepPlots < 0)
    warning("NumOfParDepPlots needs to be a positive number")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  
  # Regression Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Regression Ensure ValidationData is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # Regression Ensure TestData is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # Regression Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # Regression IDcol Name Storage----
  if (!is.null(IDcols)) {
    if (!is.character(IDcols)) {
      IDcols <- names(data)[IDcols]
    }
  }
  
  # Regression Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = NULL,
      TimeColumnName = NULL
    )
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # Regression Sort data if PrimaryDateColumn----
  if (!is.null(PrimaryDateColumn)) {
    data <- data[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      data.table::set(data,
                      j = eval(PrimaryDateColumn),
                      value = NULL)
    }
  }
  
  # Regression Sort ValidationData if PrimaryDateColumn----
  if (!is.null(PrimaryDateColumn)) {
    ValidationData <- ValidationData[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      data.table::set(ValidationData,
                      j = eval(PrimaryDateColumn),
                      value = NULL)
    }
  }
  
  # Regression Sort TestData if PrimaryDateColumn----
  if (!is.null(TestData)) {
    if (!is.null(PrimaryDateColumn)) {
      TestData <- TestData[order(get(PrimaryDateColumn))]
      if (!(eval(PrimaryDateColumn) %in% IDcols)) {
        data.table::set(TestData,
                        j = eval(PrimaryDateColumn),
                        value = NULL)
      }
    }
  }
  
  # Regression data Subset Columns Needed----
  if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    keep1 <- names(data)[c(FeatureColNames)]
    keep <- c(keep1, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  } else {
    keep <- c(FeatureColNames, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  }
  
  # Regression TestData Subset Columns Needed----
  if (!is.null(TestData)) {
    if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
      keep1 <- names(TestData)[c(FeatureColNames)]
      if (!is.null(IDcols)) {
        keep <- c(IDcols, keep1, Target)
      } else {
        keep <- c(keep1, Target)
      }
      TestData <- TestData[, ..keep]
    } else {
      keep1 <- c(FeatureColNames)
      if (!is.null(IDcols)) {
        keep <- c(IDcols, FeatureColNames, Target)
      } else {
        keep <- c(FeatureColNames, Target)
      }
      TestData <- TestData[, ..keep]
    }
    if (!is.null(IDcols)) {
      TestMerge <- data.table::copy(TestData)
      keep <- c(keep1, Target)
      TestData <- TestData[, ..keep]
    } else {
      TestMerge <- data.table::copy(TestData)
    }
  }
  
  # Regression Identify column numbers for factor variables----
  CatFeatures <-
    sort(c(as.numeric(which(
      sapply(dataTrain, is.factor)
    )),
    as.numeric(which(
      sapply(dataTrain, is.character)
    ))))
  
  # Regression Convert CatFeatures to 1-indexed----
  if (length(CatFeatures) > 0) {
    for (i in seq_len(length(CatFeatures))) {
      CatFeatures[i] <- CatFeatures[i] - 1
    }
  }
  
  # Regression Train ModelDataPrep----
  dataTrain <- ModelDataPrep(
    data = dataTrain,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = TRUE,
    MissFactor = "0",
    MissNum = -1
  )
  
  # Regression Validation ModelDataPrep----
  dataTest <- ModelDataPrep(
    data = dataTest,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = TRUE,
    MissFactor = "0",
    MissNum = -1
  )
  
  # Regression Test ModelDataPrep----
  if (!is.null(TestData)) {
    TestData <- ModelDataPrep(
      data = TestData,
      Impute = TRUE,
      CharToFactor = TRUE,
      RemoveDates = TRUE,
      MissFactor = "0",
      MissNum = -1
    )
  }
  
  # Regression Save Names of data----
  Names <- data.table::as.data.table(names(data))
  data.table::setnames(Names, "V1", "ColNames")
  if (SaveModelObjects) {
    data.table::fwrite(Names, paste0(model_path,
                                     "/"
                                     , ModelID, "_ColNames.csv"))
  }
  
  # Regression Get Min Value of Target Data----
  MinVal <- min(data[[eval(Target)]], na.rm = TRUE)
  
  # Regression Subset Target Variables----
  TrainTarget <-
    tryCatch({
      dataTrain[, get(Target)]
    }, error = function(x)
      dataTrain[, eval(Target)])
  TestTarget <-
    tryCatch({
      dataTest[, get(Target)]
    }, error = function(x)
      dataTest[, eval(Target)])
  if (!is.null(TestData)) {
    FinalTestTarget <-
      tryCatch({
        TestData[, get(Target)]
      }, error = function(x)
        TestData[, eval(Target)])
  }
  
  # Regression eval_metric checks
  if (tolower(eval_metric) == "poisson" & (min(TrainTarget) < 0 |
                                           min(TestTarget) < 0)) {
    warning("eval_metric Poisson requires positive values for Target")
  }
  
  # Regression Initialize Catboost Data Conversion----
  if (!is.null(CatFeatures)) {
    if (!is.null(TestData)) {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                     label = TrainTarget,
                                     cat_features = CatFeatures)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
      FinalTestPool <-
        catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget, cat_features = CatFeatures)
    } else {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                     label = TrainTarget,
                                     cat_features = CatFeatures)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
    }
  } else {
    if (!is.null(TestData)) {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
      FinalTestPool <-
        catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget)
    } else {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
    }
  }
  
  # Regression Grid Tune or Not Check----
  if (GridTune) {
    # Regression Grid Create data.table To Store Results----
    GridCollect <-
      data.table::data.table(
        ParamRow = 1:(MaxModelsInGrid + 1),
        EvalStat = rep(9999999, MaxModelsInGrid + 1)
      )
    
    # Regression Grid Define Hyper Parameters----
    if (!is.null(PassInGrid)) {
      if (!data.table::is.data.table(PassInGrid)) {
        PassInGrid <- data.table::as.data.table(PassInGrid)
      }
      catboostGridList <- data.table::CJ(
        l2_leaf_reg = c(0, 1, 2, 3),
        learning_rate = c(0.01, 0.02, 0.03, 0.04, 0.05),
        bootstrap_type = c("Poisson", "Bayesian", "Bernoulli", "No"),
        depth = c(4:12)
      )
      if (tolower(task_type) != "gpu") {
        catboostGridList <- catboostGridList[bootstrap_type != "Poisson"]
      }
      catboostGridList[, ID := runif(nrow(catboostGridList))]
      catboostGridList <-
        catboostGridList[order(ID)][1:(MaxModelsInGrid)][, ID := NULL]
      catboostGridList <-
        data.table::rbindlist(list(PassInGrid, catboostGridList))
    } else {
      catboostGridList <- data.table::CJ(
        l2_leaf_reg = c(0, 1, 2, 3),
        learning_rate = c(0.01, 0.02, 0.03, 0.04, 0.05),
        bootstrap_type = c("Poisson", "Bayesian", "Bernoulli", "No"),
        depth = c(4:12)
      )
      if (tolower(task_type) != "gpu") {
        catboostGridList <- catboostGridList[bootstrap_type != "Poisson"]
      }
      catboostGridList[, ID := runif(nrow(catboostGridList))]
      catboostGridList <-
        catboostGridList[order(ID)][1:(MaxModelsInGrid + 1)][, ID := NULL]
    }
    
    # Regression Grid Tuning Main Loop----
    for (i in as.integer(seq_len(MaxModelsInGrid + 1))) {
      # Print i
      print(i)
      
      # Regression Grid Define Base Parameters----
      if (eval_metric != "Quantile" &
          eval_metric != "LogLinQuantile") {
        base_params <- list(
          iterations           = Trees,
          loss_function        = 'RMSE',
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
        )
      } else {
        base_params <- list(
          iterations           = Trees,
          loss_function        = 'Quantile',
          eval_metric          = eval_metric,
          has_time             = HasTime,
          alpha                = Alpha,
          use_best_model       = TRUE,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
        )
      }
      
      # Regression Grid Merge Model Parameters----
      # Have first model be the baseline model
      if (i != 1) {
        base_params <- c(as.list(catboostGridList[i,]), base_params)
      }
      
      # Regression Grid Train Model----
      model <- catboost::catboost.train(learn_pool = TrainPool,
                                        test_pool  = TestPool,
                                        params     = base_params)
      
      # Regression Grid Score Model----
      if (!is.null(TestData)) {
        predict <- catboost::catboost.predict(
          model = model,
          pool = FinalTestPool,
          prediction_type = "RawFormulaVal",
          thread_count = -1
        )
      } else {
        predict <- catboost::catboost.predict(
          model = model,
          pool = TestPool,
          prediction_type = "RawFormulaVal",
          thread_count = -1
        )
      }
      
      # Regression Remove Model and Collect Garbage----
      rm(model)
      gc()
      
      # Regression Grid Validation Data----
      if (!is.null(TestData)) {
        calibEval <-
          data.table::as.data.table(cbind(Target = FinalTestTarget, Predicted = predict))
      } else {
        calibEval <-
          data.table::as.data.table(cbind(Target = TestTarget, Predicted = predict))
      }
      
      # Regression Grid Evaluation Metrics----
      if (tolower(grid_eval_metric) == "poisson") {
        if (MinVal > 0 & min(calibEval[["Predicted"]], na.rm = TRUE) > 0) {
          calibEval[, Metric := Predicted - Target * log(Predicted + 1)]
          Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(grid_eval_metric) == "mae") {
        calibEval[, Metric := abs(Target - Predicted)]
        Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(grid_eval_metric) == "mape") {
        calibEval[, Metric := abs((Target - Predicted) / (Target + 1))]
        Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(grid_eval_metric) == "mse") {
        calibEval[, Metric := (Target - Predicted) ^ 2]
        Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(grid_eval_metric) == "msle") {
        if (MinVal > 0 & min(calibEval[["Predicted"]], na.rm = TRUE) > 0) {
          calibEval[, Metric := (log(Target + 1) - log(Predicted + 1)) ^ 2]
          Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(grid_eval_metric) == "kl") {
        if (MinVal > 0 & min(calibEval[["Predicted"]], na.rm = TRUE) > 0) {
          calibEval[, Metric := Target * log((Target + 1) / (Predicted + 1))]
          Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(grid_eval_metric) == "cs") {
        calibEval[, ':=' (
          Metric1 = Target * Predicted,
          Metric2 = Target ^ 2,
          Metric3 = Predicted ^ 2
        )]
        Metric <-
          calibEval[, sum(Metric1, na.rm = TRUE)] / (sqrt(calibEval[, sum(Metric2, na.rm = TRUE)]) *
                                                       sqrt(calibEval[, sum(Metric3, na.rm = TRUE)]))
      } else if (tolower(grid_eval_metric) == "r2") {
        Metric <- (calibEval[, stats::cor(Target, Predicted)]) ^ 2
      }
      
      # Regression Metrics Collection----
      data.table::set(GridCollect,
                      i = i,
                      j = 1L,
                      value = i)
      data.table::set(
        GridCollect,
        i = i,
        j = 2L,
        value = round(Metric, 4)
      )
    }
  }
  
  # Regression Define Final Model Parameters----
  if (GridTune) {
    if (grid_eval_metric %chin% c("poisson", "mae", "mape", "mse", "msle", "kl", "cs", "r2")) {
      BestGrid <- GridCollect[order(-EvalStat)][1, ParamRow]
      if (BestGrid == 1) {
        BestThresh <- GridCollect[order(-EvalStat)][1, EvalStat]
        base_params <- list(
          iterations           = Trees,
          learning_rate        = 0.01,
          depth                = 10,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
        )
        
      } else {
        BestThresh <- GridCollect[order(-EvalStat)][1, EvalStat]
        base_params <- list(
          iterations           = Trees,
          learning_rate        = 0.01,
          depth                = 10,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
        )
        base_params <-
          c(as.list(catboostGridList[BestGrid,]), base_params)
      }
    } else {
      BestGrid <- GridCollect[order(EvalStat)][1, ParamRow]
      BestThresh <- GridCollect[order(EvalStat)][1, EvalStat]
    }
    base_params <- list(
      iterations           = Trees,
      learning_rate        = 0.01,
      depth                = 10,
      loss_function        = "RMSE",
      eval_metric          = eval_metric,
      use_best_model       = TRUE,
      has_time             = HasTime,
      best_model_min_trees = 10,
      metric_period        = 10,
      task_type            = task_type
    )
    base_params <- c(as.list(catboostGridList[BestGrid,]),
                     base_params)
  } else {
    base_params <- list(
      iterations           = Trees,
      learning_rate        = 0.01,
      depth                = 10,
      loss_function        = "RMSE",
      eval_metric          = eval_metric,
      use_best_model       = TRUE,
      has_time             = HasTime,
      best_model_min_trees = 10,
      metric_period        = 10,
      task_type            = task_type
    )
    if (!is.null(PassInGrid)) {
      base_params <- c(base_params, as.list(PassInGrid[1, ]))
    }
  }
  
  # Regression Train Final Model----
  model <- catboost::catboost.train(learn_pool = TrainPool,
                                    test_pool  = TestPool,
                                    params     = base_params)
  
  # Regression Save Model----
  if (SaveModelObjects) {
    catboost::catboost.save_model(model = model,
                                  model_path = paste0(model_path, "/", ModelID))
  }
  
  # Regression Score Final Test Data----
  if (!is.null(TestData)) {
    predict <- catboost::catboost.predict(
      model = model,
      pool = FinalTestPool,
      prediction_type = "RawFormulaVal",
      thread_count = -1
    )
  } else {
    predict <- catboost::catboost.predict(
      model = model,
      pool = TestPool,
      prediction_type = "RawFormulaVal",
      thread_count = -1
    )
  }
  
  # Regression Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, Predict = predict))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(Target = TestTarget, dataTest, Predict = predict))
  }
  
  # Regression r2 via sqrt of correlation
  r_squared <- (ValidationData[, stats::cor(Target, Predict)]) ^ 2
  
  # Regression Save Validation Data to File----
  if (SaveModelObjects) {
    data.table::fwrite(ValidationData,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_ValidationData.csv"))
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "Predict",
    TargetColName = "Target",
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Add Number of Trees to Title
  EvaluationPlot <- EvaluationPlot +
    ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ",
                            round(r_squared, 3)))
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID, "_EvaluationPlot.png"))
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationBoxPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "Predict",
    TargetColName = "Target",
    GraphType = "boxplot",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Add Number of Trees to Title
  EvaluationBoxPlot <- EvaluationBoxPlot +
    ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ",
                            round(r_squared, 3)))
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID,
                           "_EvaluationBoxPlot.png"))
  }
  
  # Regression Evaluation Metrics----
  EvaluationMetrics <-
    data.table::data.table(
      Metric = c("Poisson", "MAE",
                 "MAPE", "MSE", "MSLE",
                 "KL", "CS", "R2"),
      MetricValue = rep(999999, 8)
    )
  i <- 0
  for (metric in c("poisson", "mae", "mape", "mse", "msle", "kl", "cs", "r2")) {
    i <- as.integer(i + 1)
    tryCatch({
      # Regression Grid Evaluation Metrics----
      if (tolower(metric) == "poisson") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := Predict - Target * log(Predict + 1)]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "mae") {
        ValidationData[, Metric := abs(Target - Predict)]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mape") {
        ValidationData[, Metric := abs((Target - Predict) / (Target + 1))]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mse") {
        ValidationData[, Metric := (Target - Predict) ^ 2]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "msle") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := (log(Target + 1) - log(Predict + 1)) ^ 2]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "kl") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := Target * log((Target + 1) /
                                                    (Predict + 1))]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "cs") {
        ValidationData[, ':=' (
          Metric1 = Target * Predict,
          Metric2 = Target ^ 2,
          Metric3 = Predict ^ 2
        )]
        Metric <-
          ValidationData[, sum(Metric1, na.rm = TRUE)] / (sqrt(ValidationData[, sum(Metric2, na.rm = TRUE)]) *
                                                            sqrt(ValidationData[, sum(Metric3, na.rm = TRUE)]))
      } else if (tolower(metric) == "r2") {
        ValidationData[, ':=' (Metric1 = (Target - mean(Target)) ^ 2,
                               Metric2 = (Target - Predict) ^ 2)]
        Metric <-
          1 - ValidationData[, sum(Metric2, na.rm = TRUE)] /
          ValidationData[, sum(Metric1, na.rm = TRUE)]
      }
      data.table::set(
        EvaluationMetrics,
        i = i,
        j = 2L,
        value = round(Metric, 4)
      )
      data.table::set(EvaluationMetrics,
                      i = i,
                      j = 3L,
                      value = NA)
    }, error = function(x)
      "skip")
  }
  
  # Remove Cols
  ValidationData[, ':=' (
    Metric = NULL,
    Metric1 = NULL,
    Metric2 = NULL,
    Metric3 = NULL
  )]
  
  # Save EvaluationMetrics to File
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  if (SaveModelObjects) {
    data.table::fwrite(EvaluationMetrics,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_EvaluationMetrics.csv"))
  }
  
  # Regression Variable Importance----
  temp <- catboost::catboost.get_feature_importance(model)
  VariableImportance <-
    data.table::data.table(cbind(Variable = rownames(temp), temp))
  data.table::setnames(VariableImportance, "V2", "Importance")
  VariableImportance[, Importance := round(as.numeric(Importance), 4)]
  VariableImportance <- VariableImportance[order(-Importance)]
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_VariableImportance.csv"))
  }
  
  # Regression Partial Dependence----
  ParDepPlots <- list()
  j <- 0
  ParDepBoxPlots <- list()
  k <- 0
  for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
    tryCatch({
      Out <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "Predict",
        TargetColName = "Target",
        IndepVar = VariableImportance[i, Variable],
        GraphType = "calibration",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      j <- j + 1
      ParDepPlots[[paste0(VariableImportance[j, Variable])]] <-
        Out
    }, error = function(x)
      "skip")
    tryCatch({
      Out1 <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "Predict",
        TargetColName = "Target",
        IndepVar = VariableImportance[i, Variable],
        GraphType = "boxplot",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      k <- k + 1
      ParDepBoxPlots[[paste0(VariableImportance[k, Variable])]] <-
        Out1
    }, error = function(x)
      "skip")
  }
  
  # Regression Save ParDepPlots to file----
  if (SaveModelObjects) {
    save(ParDepPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepPlots.R"))
  }
  
  # Regression Save ParDepBoxPlots to file----
  if (SaveModelObjects) {
    save(ParDepBoxPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepBoxPlots.R"))
  }
  
  # Regression Save GridCollect and catboostGridList----
  if (SaveModelObjects & GridTune == TRUE) {
    data.table::fwrite(catboostGridList,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_catboostGridList.csv"))
    data.table::fwrite(GridCollect,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_GridCollect.csv"))
  }
  
  # Final Garbage Collection----
  if (tolower(task_type) == "gpu") {
    gc()
  }
  
  # Regression Return Model Objects----
  if (GridTune) {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          EvaluationPlot = EvaluationPlot,
          EvaluationBoxPlot = EvaluationBoxPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          PartialDependencePlots = ParDepPlots,
          PartialDependenceBoxPlots = ParDepBoxPlots,
          GridList = catboostGridList,
          GridMetrics = GridCollect,
          ColNames = Names
        )
      )
    }
  } else {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          EvaluationPlot = EvaluationPlot,
          EvaluationBoxPlot = EvaluationBoxPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          PartialDependencePlots = ParDepPlots,
          PartialDependenceBoxPlots = ParDepBoxPlots,
          ColNames = Names
        )
      )
    }
  }
}

#' AutoCatBoostMultiClass is an automated catboost model grid-tuning multinomial classifier and evaluation system
#'
#' AutoCatBoostMultiClass is an automated modeling function that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, variable importance, and column names used in model fitting. You can download the catboost package using devtools, via: devtools::install_github('catboost/catboost', subdir = 'catboost/R-package').
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located, but not mixed types.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located, but not mixed types. Also, not zero-indexed.
#' @param PrimaryDateColumn Supply a date or datetime column for catboost to utilize time as its basis for handling categorical features, instead of random shuffling
#' @param ClassWeights Supply a vector of weights for your target classes. E.g. c(0.25, 1) to weight your 0 class by 0.25 and your 1 class by 1.
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param task_type Set to "GPU" to utilize your GPU for training. Default is "CPU".
#' @param eval_metric This is the metric used inside catboost to measure performance on validation data during a grid-tune. "MultiClass" or "MultiClassOneVsAll"
#' @param grid_eval_metric This is the metric used to find the threshold "auc","accuracy"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxModelsInGrid Number of models to test from grid options. 1080 total possible options
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param ReturnModelObjects Set to TRUE to output all modeling objects. E.g. plots and evaluation metrics
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param PassInGrid Defaults to NULL. Pass in a single row of grid from a previous output as a data.table (they are collected as data.tables)
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Target := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' TestModel <- AutoCatBoostMultiClass(data,
#'                                     ValidationData = NULL,
#'                                     TestData = NULL,
#'                                     TargetColumnName = "Target",
#'                                     FeatureColNames = c(2:11),
#'                                     PrimaryDateColumn = NULL,
#'                                     ClassWeights = NULL,
#'                                     IDcols = NULL,
#'                                     MaxModelsInGrid = 1,
#'                                     task_type = "GPU",
#'                                     eval_metric = "MultiClass",
#'                                     grid_eval_metric = "Accuracy",
#'                                     Trees = 50,
#'                                     GridTune = FALSE,
#'                                     model_path = NULL,
#'                                     ModelID = "ModelTest",
#'                                     ReturnModelObjects = TRUE,
#'                                     SaveModelObjects = FALSE,
#'                                     PassInGrid = NULL)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model (the model), ValidationData.csv, EvaluationMetrics.csv, GridCollect, and GridList
#' @export
AutoCatBoostMultiClass <- function(data,
                                   ValidationData = NULL,
                                   TestData = NULL,
                                   TargetColumnName = NULL,
                                   FeatureColNames = NULL,
                                   PrimaryDateColumn = NULL,
                                   ClassWeights = NULL,
                                   IDcols = NULL,
                                   task_type = "GPU",
                                   eval_metric = "MultiClassOneVsAll",
                                   Trees = 50,
                                   GridTune = FALSE,
                                   grid_eval_metric = "Accuracy",
                                   MaxModelsInGrid = 10,
                                   model_path = NULL,
                                   ModelID = "FirstModel",
                                   ReturnModelObjects = TRUE,
                                   SaveModelObjects = FALSE,
                                   PassInGrid = NULL) {
  # Load catboost----
  loadNamespace(package = "catboost")
  
  # MultiClass Check Arguments----
  if (!(tolower(task_type) %chin% c("gpu", "cpu")))
    warning("task_type needs to be either 'GPU' or 'CPU'")
  if (!(tolower(eval_metric) %chin% c("multiclass", "multiclassonevsall"))) {
    warning("eval_metric not in c('MultiClass','MultiClassOneVsAll')")
  }
  if (!is.null(PrimaryDateColumn)) {
    HasTime <- TRUE
  } else {
    HasTime <- FALSE
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (!(tolower(grid_eval_metric) %chin% c("accuracy", "auc"))) {
    warning("grid_eval_metric not in c('accuracy','auc')")
  }
  if (MaxModelsInGrid < 1 |
      MaxModelsInGrid > 1080 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1 and less than 1080")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path))
      warning("model_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  
  # MultiClass Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # MultiClass Ensure ValidationData is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # MultiClass Ensure TestData is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # MultiClass Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # MultiClass IDcol Name Storage----
  if (!is.null(IDcols)) {
    if (!is.character(IDcols)) {
      IDcols <- names(data)[IDcols]
    }
  }
  
  # MultiClass Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = Target,
      TimeColumnName = NULL
    )
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # MultiClass Sort data if PrimaryDateColumn----
  if (!is.null(PrimaryDateColumn)) {
    data <- data[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      data.table::set(data,
                      j = eval(PrimaryDateColumn),
                      value = NULL)
    }
  }
  
  # MultiClass Sort ValidationData if PrimaryDateColumn----
  if (!is.null(PrimaryDateColumn)) {
    ValidationData <- ValidationData[order(get(PrimaryDateColumn))]
    if (!(eval(PrimaryDateColumn) %in% IDcols)) {
      data.table::set(ValidationData,
                      j = eval(PrimaryDateColumn),
                      value = NULL)
    }
  }
  
  # MultiClass Sort TestData if PrimaryDateColumn----
  if (!is.null(TestData)) {
    if (!is.null(PrimaryDateColumn)) {
      TestData <- TestData[order(get(PrimaryDateColumn))]
      if (!(eval(PrimaryDateColumn) %in% IDcols)) {
        data.table::set(TestData,
                        j = eval(PrimaryDateColumn),
                        value = NULL)
      }
    }
  }
  
  # MultiClass data Subset Columns Needed----
  if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    keep1 <- names(data)[c(FeatureColNames)]
    keep <- c(keep1, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  } else {
    keep <- c(FeatureColNames, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  }
  
  # MultiClass TestData Subset Columns Needed----
  if (!is.null(TestData)) {
    if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
      keep1 <- names(TestData)[c(FeatureColNames)]
      if (!is.null(IDcols)) {
        keep <- c(IDcols, keep1, Target)
      } else {
        keep <- c(keep1, Target)
      }
      TestData <- TestData[, ..keep]
    } else {
      keep1 <- c(FeatureColNames)
      if (!is.null(IDcols)) {
        keep <- c(IDcols, FeatureColNames, Target)
      } else {
        keep <- c(FeatureColNames, Target)
      }
      TestData <- TestData[, ..keep]
    }
    if (!is.null(IDcols)) {
      TestMerge <- data.table::copy(TestData)
      keep <- c(keep1, Target)
      TestData <- TestData[, ..keep]
    } else {
      TestMerge <- data.table::copy(TestData)
    }
  }
  
  # Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(
    sapply(data, is.factor)
  )),
  as.numeric(which(
    sapply(data, is.character)
  ))))
  TargetNum <- which(names(data) == Target)
  CatFeatures <- setdiff(CatFeatures, TargetNum)
  
  # MultiClass Convert CatFeatures to 1-indexed----
  if (length(CatFeatures) > 0) {
    for (i in seq_len(length(CatFeatures))) {
      CatFeatures[i] <- CatFeatures[i] - 1
    }
  }
  
  # MultiClass Train ModelDataPrep----
  dataTrain <- ModelDataPrep(
    data = dataTrain,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = TRUE,
    MissFactor = "0",
    MissNum = -1
  )
  
  # MultiClass Validation ModelDataPrep----
  dataTest <- ModelDataPrep(
    data = dataTest,
    Impute = TRUE,
    CharToFactor = TRUE,
    RemoveDates = TRUE,
    MissFactor = "0",
    MissNum = -1
  )
  
  # MultiClass Test ModelDataPrep----
  if (!is.null(TestData)) {
    TestData <- ModelDataPrep(
      data = TestData,
      Impute = TRUE,
      CharToFactor = TRUE,
      RemoveDates = TRUE,
      MissFactor = "0",
      MissNum = -1
    )
  }
  
  # MultiClass Obtain Unique Target Levels
  if (!is.null(TestData)) {
    temp <- data.table::rbindlist(list(dataTrain, dataTest, TestData))
  } else {
    temp <- data.table::rbindlist(list(dataTrain, dataTest))
  }
  TargetLevels <-
    data.table::as.data.table(sort(unique(temp[[eval(TargetColumnName)]])))
  data.table::setnames(TargetLevels, "V1", "OriginalLevels")
  TargetLevels[, NewLevels := 1:.N]
  if (SaveModelObjects) {
    data.table::fwrite(TargetLevels,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_TargetLevels.csv"))
  }
  
  # MultiClass Convert Target to Numeric Factor
  dataTrain <- merge(
    dataTrain,
    TargetLevels,
    by.x = eval(Target),
    by.y = "OriginalLevels",
    all = FALSE
  )
  dataTrain[, paste0(Target) := NewLevels]
  dataTrain[, NewLevels := NULL]
  dataTest <- merge(
    dataTest,
    TargetLevels,
    by.x = eval(Target),
    by.y = "OriginalLevels",
    all = FALSE
  )
  dataTest[, paste0(Target) := NewLevels]
  dataTest[, NewLevels := NULL]
  if (!is.null(TestData)) {
    TestData <- merge(
      TestData,
      TargetLevels,
      by.x = eval(Target),
      by.y = "OriginalLevels",
      all = FALSE
    )
    TestData[, paste0(Target) := NewLevels]
    TestData[, NewLevels := NULL]
  }
  
  # Reorder Colnames
  data.table::setcolorder(dataTrain, c(2:ncol(dataTrain), 1))
  data.table::setcolorder(dataTest, c(2:ncol(dataTest), 1))
  if (!is.null(TestData)) {
    data.table::setcolorder(TestData, c(2:ncol(TestData), 1))
  }
  
  # MultiClass Save Names of data----
  Names <- data.table::as.data.table(names(data))
  data.table::setnames(Names, "V1", "ColNames")
  if (SaveModelObjects) {
    data.table::fwrite(Names, paste0(model_path, "/", ModelID, "_ColNames.csv"))
  }
  
  # MultiClass Subset Target Variables----
  TrainTarget <-
    tryCatch({
      dataTrain[, as.numeric(get(Target))]
    }, error = function(x)
      dataTrain[, as.numeric(eval(Target))])
  TestTarget <-
    tryCatch({
      dataTest[, as.numeric(get(Target))]
    }, error = function(x)
      dataTest[, as.numeric(eval(Target))])
  if (!is.null(TestData)) {
    FinalTestTarget <-
      tryCatch({
        TestData[, as.numeric(get(Target))]
      }, error = function(x)
        TestData[, as.numeric(eval(Target))])
  }
  
  # MultiClass Initialize Catboost Data Conversion----
  if (!is.null(CatFeatures)) {
    if (!is.null(TestData)) {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                     label = TrainTarget,
                                     cat_features = CatFeatures)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
      FinalTestPool <-
        catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget, cat_features = CatFeatures)
    } else {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL],
                                     label = TrainTarget,
                                     cat_features = CatFeatures)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget, cat_features = CatFeatures)
    }
  } else {
    if (!is.null(TestData)) {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
      FinalTestPool <-
        catboost::catboost.load_pool(TestData[, eval(Target) := NULL], label = FinalTestTarget)
    } else {
      TrainPool <-
        catboost::catboost.load_pool(dataTrain[, eval(Target) := NULL], label = TrainTarget)
      TestPool <-
        catboost::catboost.load_pool(dataTest[, eval(Target) := NULL], label = TestTarget)
    }
  }
  
  # MultiClass Grid Tune or Not Check----
  if (GridTune) {
    # MultiClass Grid Create data.table To Store Results----
    GridCollect <-
      data.table::data.table(
        ParamRow = 1:(MaxModelsInGrid + 1),
        EvalStat = rep(9999999, MaxModelsInGrid + 1)
      )
    
    # MultiClass Grid Define Hyper Parameters----
    if (!is.null(PassInGrid)) {
      if (!data.table::is.data.table(PassInGrid)) {
        PassInGrid <- data.table::as.data.table(PassInGrid)
      }
      catboostGridList <- data.table::CJ(
        l2_leaf_reg = c(0, 1, 2, 3),
        learning_rate = c(0.01, 0.02, 0.03, 0.04, 0.05),
        bootstrap_type = c("Poisson", "Bayesian", "Bernoulli", "No"),
        depth = c(4:12)
      )
      if (tolower(task_type) != "gpu") {
        catboostGridList <- catboostGridList[bootstrap_type != "Poisson"]
      }
      catboostGridList[, ID := runif(nrow(catboostGridList))]
      catboostGridList <-
        catboostGridList[order(ID)][1:(MaxModelsInGrid)][, ID := NULL]
      catboostGridList <-
        data.table::rbindlist(list(PassInGrid, catboostGridList))
    } else {
      catboostGridList <- data.table::CJ(
        l2_leaf_reg = c(0, 1, 2, 3),
        learning_rate = c(0.01, 0.02, 0.03, 0.04, 0.05),
        bootstrap_type = c("Poisson", "Bayesian", "Bernoulli", "No"),
        depth = c(4:12)
      )
      if (tolower(task_type) != "gpu") {
        catboostGridList <- catboostGridList[bootstrap_type != "Poisson"]
      }
      catboostGridList[, ID := runif(nrow(catboostGridList))]
      catboostGridList <-
        catboostGridList[order(ID)][1:(MaxModelsInGrid + 1)][, ID := NULL]
    }
    
    # MultiClass Grid Tuning Main Loop----
    for (i in as.integer(seq_len(MaxModelsInGrid + 1))) {
      # Print i
      print(i)
      
      # MultiClass Grid Define Base Parameters----
      if (!is.null(ClassWeights)) {
        base_params <- list(
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type,
          class_weights        = ClassWeights
        )
      } else {
        base_params <- list(
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
        )
      }
      
      # MultiClass Grid Merge Model Parameters----
      # Have first model be the baseline model
      if (i != 1) {
        base_params <- c(as.list(catboostGridList[i,]), base_params)
      }
      
      # MultiClass Grid Train Model----
      model <- catboost::catboost.train(learn_pool = TrainPool,
                                        test_pool  = TestPool,
                                        params     = base_params)
      
      # MultiClass Grid Score Model----
      tryCatch({
        if (!is.null(TestData)) {
          predict <- cbind(
            1 + catboost::catboost.predict(
              model = model,
              pool = FinalTestPool,
              prediction_type = "Class"
            ),
            catboost::catboost.predict(
              model = model,
              pool = FinalTestPool,
              prediction_type = "Probability"
            )
          )
        } else {
          predict <- cbind(
            1 + catboost::catboost.predict(
              model = model,
              pool = TestPool,
              prediction_type = "Class"
            ),
            catboost::catboost.predict(
              model = model,
              pool = TestPool,
              prediction_type = "Probability"
            )
          )
        }
        
        # MultiClass Remove Model and Collect Garbage----
        rm(model)
        gc()
        
        # MultiClass Grid Validation Data----
        if (!is.null(TestData)) {
          calibEval <-
            data.table::as.data.table(cbind(Target = FinalTestTarget, predict))
        } else {
          calibEval <-
            data.table::as.data.table(cbind(Target = TestTarget, predict))
        }
        ValidationData <- merge(
          calibEval,
          TargetLevels,
          by.x = "V2",
          by.y = "NewLevels",
          all = FALSE
        )
        ValidationData[, V2 := OriginalLevels][, OriginalLevels := NULL]
        ValidationData <- merge(
          ValidationData,
          TargetLevels,
          by.x = "Target",
          by.y = "NewLevels",
          all = FALSE
        )
        ValidationData[, Target := OriginalLevels][, OriginalLevels := NULL]
        
        # MultiClass Update Names for Predicted Value Columns
        k <- 2
        for (name in as.character(TargetLevels[[1]])) {
          k <- k + 1
          data.table::setnames(ValidationData, paste0("V", k), name)
        }
        data.table::setnames(ValidationData, "V2", "Predict")
        data.table::set(ValidationData,
                        j = "Target",
                        value = as.character(ValidationData[["Target"]]))
        data.table::set(ValidationData,
                        j = "Predict",
                        value = as.character(ValidationData[["Predict"]]))
        
        # MultiClass Metric----
        if (tolower(grid_eval_metric) == "accuracy") {
          Metric <- ValidationData[, mean(ifelse(as.character(Target) ==
                                                   as.character(Predict),
                                                 1,
                                                 0),
                                          na.rm = TRUE)]
        } else {
          # MultiClass Metric for MicroAUC----
          ValidationData[, vals := 0.5]
          z <- ncol(ValidationData)
          col <- "Target"
          for (l in seq_len(nrow(ValidationData))) {
            cols <- ValidationData[l, get(col)][[1]]
            valss <- ValidationData[l, ..cols][[1]]
            data.table::set(
              ValidationData,
              i = l,
              j = z,
              value = valss
            )
          }
          Metric <- round(as.numeric(noquote(
            stringr::str_extract(
              pROC::multiclass.roc(ValidationData[["Target"]], ValidationData[["vals"]])$auc,
              "\\d+\\.*\\d*"
            )
          )), 4)
        }
        
        # Collect Metrics and Corresponding Grids
        # Store Output Information
        data.table::set(GridCollect,
                        i = i,
                        j = 1L,
                        value = i)
        data.table::set(GridCollect,
                        i = i,
                        j = 2L,
                        value = Metric)
      }, error = function(x)
        "skip")
    }
  }
  
  # MultiClass Define Final Model Parameters----
  if (GridTune) {
    BestGrid <- GridCollect[order(-EvalStat)][1, ParamRow]
    if (BestGrid == 1) {
      BestThresh <- GridCollect[order(-EvalStat)][1, EvalStat]
      if (!is.null(ClassWeights)) {
        base_params <- list(
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type,
          class_weights        = ClassWeights
        )
      } else {
        base_params <- list(
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
        )
      }
    } else {
      BestThresh <- GridCollect[order(-EvalStat)][1, EvalStat]
      if (!is.null(ClassWeights)) {
        base_params <- list(
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type,
          class_weights        = ClassWeights
        )
      } else {
        base_params <- list(
          iterations           = Trees,
          loss_function        = eval_metric,
          eval_metric          = eval_metric,
          use_best_model       = TRUE,
          has_time             = HasTime,
          best_model_min_trees = 10,
          metric_period        = 10,
          task_type            = task_type
        )
      }
      base_params <-
        c(as.list(catboostGridList[BestGrid,]), base_params)
    }
  } else {
    if (!is.null(ClassWeights)) {
      base_params <- list(
        iterations           = Trees,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        has_time             = HasTime,
        best_model_min_trees = 10,
        metric_period        = 10,
        task_type            = task_type,
        class_weights        = ClassWeights
      )
    } else {
      base_params <- list(
        iterations           = Trees,
        loss_function        = eval_metric,
        eval_metric          = eval_metric,
        use_best_model       = TRUE,
        has_time             = HasTime,
        best_model_min_trees = 10,
        metric_period        = 10,
        task_type            = task_type
      )
    }
    if (!is.null(PassInGrid)) {
      base_params <- c(base_params, as.list(PassInGrid[1, ]))
    }
  }
  
  # MultiClass Train Final Model----
  model <- catboost::catboost.train(learn_pool = TrainPool,
                                    test_pool  = TestPool,
                                    params     = base_params)
  
  # MultiClass Save Model----
  if (SaveModelObjects) {
    catboost::catboost.save_model(model = model,
                                  model_path = paste0(model_path, "/", ModelID))
  }
  
  # MultiClass Score Final Test Data----
  if (!is.null(TestData)) {
    predict <- cbind(
      1 + catboost::catboost.predict(
        model = model,
        pool = FinalTestPool,
        prediction_type = "Class"
      ),
      catboost::catboost.predict(
        model = model,
        pool = FinalTestPool,
        prediction_type = "Probability"
      )
    )
  } else {
    predict <- cbind(
      1 + catboost::catboost.predict(
        model = model,
        pool = TestPool,
        prediction_type = "Class"
      ),
      catboost::catboost.predict(
        model = model,
        pool = TestPool,
        prediction_type = "Probability"
      )
    )
  }
  
  # MultiClass Grid Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(Target = FinalTestTarget, predict, TestMerge))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(Target = TestTarget, predict))
  }
  ValidationData <- merge(
    ValidationData,
    TargetLevels,
    by.x = "V1",
    by.y = "NewLevels",
    all = FALSE
  )
  ValidationData[, V1 := OriginalLevels][, OriginalLevels := NULL]
  ValidationData <- merge(
    ValidationData,
    TargetLevels,
    by.x = "Target",
    by.y = "NewLevels",
    all = FALSE
  )
  ValidationData[, Target := OriginalLevels][, OriginalLevels := NULL]
  
  # MultiClass Update Names for Predicted Value Columns
  k <- 1
  for (name in as.character(TargetLevels[[1]])) {
    k <- k + 1
    data.table::setnames(ValidationData, paste0("V", k), name)
  }
  data.table::setnames(ValidationData, "V1", "Predict")
  data.table::set(ValidationData,
                  j = "Target",
                  value = as.character(ValidationData[["Target"]]))
  data.table::set(ValidationData,
                  j = "Predict",
                  value = as.character(ValidationData[["Predict"]]))
  
  # MultiClass Metrics Accuracy----
  MetricAcc <-
    ValidationData[, mean(ifelse(as.character(Target) ==
                                   as.character(Predict),
                                 1.0,
                                 0.0),
                          na.rm = TRUE)]
  
  # MultiClass Metrics MicroAUC----
  y <- ValidationData[[eval(Target)]]
  keep <- names(ValidationData)[3:(ncol(predict) + 1)]
  x <- as.matrix(ValidationData[, ..keep])
  z <- pROC::multiclass.roc(response = y, predictor = x)
  MetricAUC <- round(as.numeric(noquote(
    stringr::str_extract(z$auc, "\\d+\\.*\\d*")
  )), 4)
  
  # MultiClass Save Validation Data to File----
  if (SaveModelObjects) {
    data.table::fwrite(ValidationData,
                       file = paste0(model_path, "/", ModelID, "_ValidationData.csv"))
  }
  
  # MultiClass Evaluation Metrics----
  EvaluationMetrics <-
    data.table::data.table(
      Metric = c("AUC", "Accuracy"),
      MetricValue = c(MetricAUC, MetricAcc)
    )
  
  # MultiClass Save EvaluationMetrics to File
  if (SaveModelObjects) {
    data.table::fwrite(EvaluationMetrics,
                       file = paste0(model_path, "/", ModelID, "_EvaluationMetrics.csv"))
  }
  
  # MultiClass Variable Importance----
  temp <- catboost::catboost.get_feature_importance(model)
  VariableImportance <-
    data.table::data.table(cbind(Variable = rownames(temp), temp))
  data.table::setnames(VariableImportance, "V2", "Importance")
  VariableImportance[, Importance := round(as.numeric(Importance), 4)]
  VariableImportance <- VariableImportance[order(-Importance)]
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path, "/", ModelID, "_VariableImportance.csv"))
  }
  
  # MultiClass Save GridCollect and catboostGridList----
  if (SaveModelObjects & GridTune == TRUE) {
    data.table::fwrite(catboostGridList,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_catboostGridList.csv"))
    data.table::fwrite(GridCollect,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_GridCollect.csv"))
  }
  
  # Final Garbage Collection----
  if (tolower(task_type) == "gpu") {
    gc()
  }
  
  # MultiClass Return Model Objects----
  if (GridTune) {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          GridList = catboostGridList,
          GridMetrics = GridCollect,
          ColNames = Names
        )
      )
    }
  } else {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          ColNames = Names
        )
      )
    }
  }
}

#' AutoH2oGBMRegression is an automated H2O modeling framework with grid-tuning and model evaluation
#'
#' AutoH2oGBMRegression is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param Alpha This is the quantile value you want to use for quantile regression. Must be a decimal between 0 and 1.
#' @param Distribution Choose from gaussian",  "poisson",  "gamma",  "tweedie",  "laplace",  "quantile", "huber"
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "MSE", "RMSE", "MAE", "RMSLE"
#' @param TrainSplitRatio A decimal between 0.01 and 0.99 that tells the function how much data to keep for training and validation.
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxMem Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' TestModel <- AutoH2oGBMRegression(data,
#'                                   ValidationData = NULL,
#'                                   TestData = NULL,
#'                                   TargetColumnName = "Target",
#'                                   FeatureColNames = 2:ncol(data),
#'                                   Alpha = NULL,
#'                                   Distribution = "poisson",
#'                                   eval_metric = "RMSE",
#'                                   Trees = 50,
#'                                   GridTune = FALSE,
#'                                   MaxMem = "32G",
#'                                   MaxModelsInGrid = 10,
#'                                   model_path = NULL,
#'                                   ModelID = "FirstModel",
#'                                   NumOfParDepPlots = 3,
#'                                   ReturnModelObjects = TRUE,
#'                                   SaveModelObjects = FALSE,
#'                                   IfSaveModel = "mojo")
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and GridList
#' @export
AutoH2oGBMRegression <- function(data,
                                 ValidationData,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 Alpha = NULL,
                                 Distribution = "poisson",
                                 eval_metric = "RMSE",
                                 TrainSplitRatio = 0.80,
                                 Trees = 50,
                                 GridTune = FALSE,
                                 MaxMem = "32G",
                                 MaxModelsInGrid = 2,
                                 model_path = NULL,
                                 ModelID = "FirstModel",
                                 NumOfParDepPlots = 3,
                                 ReturnModelObjects = TRUE,
                                 SaveModelObjects = FALSE,
                                 IfSaveModel = "mojo") {
  # Regression Check Arguments----
  if (!(tolower(eval_metric) %chin% c("mse", "rmse", "mae", "rmsle"))) {
    warning("eval_metric not in MSE, RMSE, MAE, RMSLE")
    
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (MaxModelsInGrid < 1 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path))
      warning("model_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (NumOfParDepPlots < 0)
    warning("NumOfParDepPlots needs to be a positive number")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  
  # Regression Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Regression Ensure data is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # Regression Ensure data is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # Regression Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = NULL,
      TimeColumnName = NULL
    )
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # Regression ModelDataPrep----
  dataTrain <- ModelDataPrep(data = data,
                             Impute = FALSE,
                             CharToFactor = TRUE)
  
  # Regression ModelDataPrep----
  dataTest <- ModelDataPrep(data = ValidationData,
                            Impute = FALSE,
                            CharToFactor = TRUE)
  
  # Regression ModelDataPrep----
  if (!is.null(TestData)) {
    TestData <- ModelDataPrep(data = TestData,
                              Impute = FALSE,
                              CharToFactor = TRUE)
  }
  
  # Regression Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # Regression Get Min Value of Target Data----
  MinVal <- min(data[[eval(Target)]], na.rm = TRUE)
  
  # Regression Proper Distribution Check----
  if (MinVal < 0 & tolower(Distribution) == "poisson") {
    Distribution <- "gaussian"
  } else if (MinVal == 0 &
             tolower(Distribution) %chin% c("gamma", "tweedie")) {
    Distribution <- "poisson"
  }
  
  # Regression Grid Tune Check----
  if (GridTune) {
    # Regression Start Up H2O----
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    
    # Regression Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
    
    # Regression Grid Tune Search Criteria----
    search_criteria  <- list(
      strategy             = "RandomDiscrete",
      max_runtime_secs     = 3600 * 24 * 7,
      max_models           = MaxModelsInGrid,
      seed                 = 1234,
      stopping_rounds      = 10,
      stopping_metric      = toupper(eval_metric),
      stopping_tolerance   = 1e-3
    )
    
    # Regression Grid Parameters----
    hyper_params <- list(
      max_depth                        = c(6, 9, 12),
      sample_rate                      = c(0.5, 0.75, 1.0),
      col_sample_rate                  = c(0.5, 0.75, 1.0),
      col_sample_rate_per_tree         = c(0.5, 0.75, 1.0),
      col_sample_rate_change_per_level = c(0.9, 1.0, 1.1),
      min_rows                         = c(1, 10),
      nbins                            = c(10, 20, 30),
      nbins_cats                       = c(64, 256, 512),
      histogram_type                   = c("UniformAdaptive",
                                           "QuantilesGlobal",
                                           "RoundRobin")
    )
    
    # Regression Grid Train Model----
    if (!is.null(Alpha)) {
      grid <- h2o::h2o.grid(
        hyper_params         = hyper_params,
        search_criteria      = search_criteria,
        is_supervised        = TRUE,
        algorithm            = "gbm",
        grid_id              = paste0(ModelID, "_Grid"),
        x                    = FeatureColNames,
        y                    = TargetColumnName,
        ntrees               = Trees,
        training_frame       = datatrain,
        validation_frame     = datavalidate,
        distribution         = "quantile",
        quantile_alpha       = Alpha,
        learn_rate           = 0.05,
        learn_rate_annealing = 0.99,
        max_runtime_secs     = 3600 * 24 * 7,
        stopping_rounds      = 10,
        stopping_tolerance   = 1e-3,
        stopping_metric      = toupper(eval_metric),
        score_tree_interval  = 10,
        seed                 = 1234
      )
    } else {
      if (tolower(Distribution) == "tweedie") {
        grid <- h2o::h2o.grid(
          hyper_params         = hyper_params,
          search_criteria      = search_criteria,
          is_supervised        = TRUE,
          algorithm            = "gbm",
          grid_id              = paste0(ModelID, "_Grid"),
          x                    = FeatureColNames,
          y                    = TargetColumnName,
          ntrees               = Trees,
          training_frame       = datatrain,
          validation_frame     = datavalidate,
          distribution         = tolower(Distribution),
          tweedie_power        = 1.5,
          learn_rate           = 0.05,
          learn_rate_annealing = 0.99,
          max_runtime_secs     = 3600 * 24 * 7,
          stopping_rounds      = 10,
          stopping_tolerance   = 1e-3,
          stopping_metric      = toupper(eval_metric),
          score_tree_interval  = 10,
          seed                 = 1234
        )
      } else {
        grid <- h2o::h2o.grid(
          hyper_params         = hyper_params,
          search_criteria      = search_criteria,
          is_supervised        = TRUE,
          algorithm            = "gbm",
          grid_id              = paste0(ModelID, "_Grid"),
          x                    = FeatureColNames,
          y                    = TargetColumnName,
          ntrees               = Trees,
          training_frame       = datatrain,
          validation_frame     = datavalidate,
          distribution         = tolower(Distribution),
          learn_rate           = 0.05,
          learn_rate_annealing = 0.99,
          max_runtime_secs     = 3600 * 24 * 7,
          stopping_rounds      = 10,
          stopping_tolerance   = 1e-3,
          stopping_metric      = toupper(eval_metric),
          score_tree_interval  = 10,
          seed                 = 1234
        )
      }
    }
    
    # Regression Get Best Model----
    Grid_Out   <- h2o::h2o.getGrid(
      grid_id = paste0(ModelID, "_Grid"),
      sort_by = eval_metric,
      decreasing = FALSE
    )
    
    # Regression Collect Best Grid Model----
    grid_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1]])
  }
  
  # Regression Start Up H2O----
  if (!GridTune) {
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    
    # Regression Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
  }
  
  # Regression Baseline Model----
  if (!is.null(Alpha)) {
    base_model <- h2o::h2o.gbm(
      x                = FeatureColNames,
      y                = TargetColumnName,
      training_frame   = datatrain,
      validation_frame = datavalidate,
      distribution     = Distribution,
      quantile_alpha   = Alpha,
      model_id         = ModelID,
      ntrees           = Trees
    )
  } else {
    base_model <- h2o::h2o.gbm(
      x                = FeatureColNames,
      y                = TargetColumnName,
      training_frame   = datatrain,
      validation_frame = datavalidate,
      distribution     = Distribution,
      model_id         = ModelID,
      ntrees           = Trees
    )
  }
  
  # Regression Grab Evaluation Metric----
  if (GridTune) {
    if (!is.null(TestData)) {
      datatest        <-  h2o::as.h2o(TestData)
      if (tolower(eval_metric) == "mse") {
        GridModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = grid_model,
                                            newdata = datatest))
        BaseModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = base_model,
                                            newdata = datatest))
      } else if (tolower(eval_metric) == "rmse") {
        GridModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = grid_model,
                                             newdata = datatest))
        BaseModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = base_model,
                                             newdata = datatest))
      } else if (tolower(eval_metric) == "mae") {
        GridModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = grid_model,
                                            newdata = datatest))
        BaseModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = base_model,
                                            newdata = datatest))
      } else if (tolower(eval_metric) == "rmsle") {
        GridModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = grid_model,
                                              newdata = datatest))
        BaseModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = base_model,
                                              newdata = datatest))
      }
    } else {
      if (tolower(eval_metric) == "mse") {
        GridModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = grid_model,
                                            newdata = datavalidate))
        BaseModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = base_model,
                                            newdata = datavalidate))
      } else if (tolower(eval_metric) == "rmse") {
        GridModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = grid_model,
                                             newdata = datavalidate))
        BaseModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = base_model,
                                             newdata = datavalidate))
      } else if (tolower(eval_metric) == "mae") {
        GridModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = grid_model,
                                            newdata = datavalidate))
        BaseModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = base_model,
                                            newdata = datavalidate))
      } else if (tolower(eval_metric) == "rmsle") {
        GridModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = grid_model,
                                              newdata = datavalidate))
        BaseModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = base_model,
                                              newdata = datavalidate))
      }
      
    }
  } else {
    if (!is.null(TestData)) {
      datatest        <-  h2o::as.h2o(TestData)
      if (tolower(eval_metric) == "mse") {
        BaseModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = base_model,
                                            newdata = datatest))
      } else if (tolower(eval_metric) == "rmse") {
        BaseModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = base_model,
                                             newdata = datatest))
      } else if (tolower(eval_metric) == "mae") {
        BaseModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = base_model,
                                            newdata = datatest))
      } else if (tolower(eval_metric) == "rmsle") {
        BaseModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = base_model,
                                              newdata = datatest))
      }
    } else {
      if (tolower(eval_metric) == "mse") {
        BaseModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = base_model,
                                            newdata = datavalidate))
      } else if (tolower(eval_metric) == "rmse") {
        BaseModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = base_model,
                                             newdata = datavalidate))
      } else if (tolower(eval_metric) == "mae") {
        BaseModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = base_model,
                                            newdata = datavalidate))
      } else if (tolower(eval_metric) == "rmsle") {
        BaseModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = base_model,
                                              newdata = datavalidate))
      }
    }
  }
  
  # Regression Pick Winner----
  if (GridTune) {
    if (GridModelEval > BaseModelEval) {
      FinalModel <- grid_model
    } else {
      FinalModel <- base_model
    }
  } else {
    FinalModel <- base_model
  }
  
  # Regression Save Model----
  if (SaveModelObjects) {
    if (tolower(IfSaveModel) == "mojo") {
      SaveModel <- h2o::h2o.saveMojo(object = FinalModel,
                                     path = model_path,
                                     force = TRUE)
      h2o::h2o.download_mojo(
        model = FinalModel,
        path = model_path,
        get_genmodel_jar = TRUE,
        genmodel_path = model_path,
        genmodel_name = ModelID
      )
    } else {
      SaveModel <- h2o::h2o.saveModel(object = FinalModel,
                                      path = model_path,
                                      force = TRUE)
    }
  }
  
  # Regression Score Final Test Data----
  if (!is.null(TestData)) {
    Predict <-
      data.table::as.data.table(h2o::h2o.predict(object = FinalModel,
                                                 newdata = datatest))
    
  } else {
    Predict <-
      data.table::as.data.table(h2o::h2o.predict(object = FinalModel,
                                                 newdata = datavalidate))
  }
  
  # Regression Variable Importance----
  VariableImportance <-
    data.table::as.data.table(h2o::h2o.varimp(object = FinalModel))
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_VariableImportance.csv"))
  }
  
  # Regression Format Variable Importance Table----
  data.table::setnames(
    VariableImportance,
    c(
      "variable",
      "relative_importance",
      "scaled_importance",
      "percentage"
    ),
    c(
      "Variable",
      "RelativeImportance",
      "ScaledImportance",
      "Percentage"
    )
  )
  VariableImportance[, ':=' (
    RelativeImportance = round(RelativeImportance, 4),
    ScaledImportance = round(ScaledImportance, 4),
    Percentage = round(Percentage, 4)
  )]
  
  # Regression H2O Shutdown----
  h2o::h2o.shutdown(prompt = FALSE)
  
  # Regression Create Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(TestData, Predict))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(dataTest, Predict))
  }
  
  # Regression Change Prediction Name----
  data.table::setnames(ValidationData, "predict", "Predict")
  
  # Regression Get R2----
  r_squared <-
    (ValidationData[, stats::cor(eval(Target), Predict)][[1]]) ^ 2
  
  # Regression Save Validation Data to File----
  if (SaveModelObjects) {
    data.table::fwrite(ValidationData,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_ValidationData.csv"))
  }
  
  # Regression Evaluation Calibration Plot----
  if (!is.null(Alpha)) {
    EvaluationPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "Predict",
      TargetColName = Target,
      GraphType = "calibration",
      PercentileBucket = 0.05,
      aggrfun = function(x)
        quantile(x,
                 probs = Alpha,
                 na.rm = TRUE)
    )
  } else {
    EvaluationPlot <- EvalPlot(
      data = ValidationData,
      PredictionColName = "Predict",
      TargetColName = Target,
      GraphType = "calibration",
      PercentileBucket = 0.05,
      aggrfun = function(x)
        mean(x, na.rm = TRUE)
    )
  }
  
  # Regression Evaluation Plot Update Title----
  if (GridTune) {
    val <- max(GridModelEval, BaseModelEval)
    EvaluationPlot <- EvaluationPlot +
      ggplot2::ggtitle(paste0(
        "GBM Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(val, 3)
      ))
  } else {
    EvaluationPlot <- EvaluationPlot +
      ggplot2::ggtitle(paste0(
        "GBM Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(BaseModelEval, 3)
      ))
  }
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID,
                           "_EvaluationPlot.png"))
  }
  
  # Regression Evaluation BoxPlot----
  EvaluationBoxPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "Predict",
    TargetColName = Target,
    GraphType = "boxplot",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Regression Evaluation BoxPlot Update Title----
  if (GridTune) {
    val <- max(GridModelEval, BaseModelEval)
    EvaluationBoxPlot <- EvaluationBoxPlot +
      ggplot2::ggtitle(paste0(
        "GBM Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(val, 3)
      ))
  } else {
    EvaluationBoxPlot <- EvaluationBoxPlot +
      ggplot2::ggtitle(paste0(
        "GBM Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(BaseModelEval, 3)
      ))
  }
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID,
                           "_EvaluationBoxPlot.png"))
  }
  
  # Regression Evaluation Metrics----
  EvaluationMetrics <-
    data.table::data.table(
      Metric = c("Poisson", "MAE",
                 "MAPE", "MSE", "MSLE",
                 "KL", "CS", "R2"),
      MetricValue = rep(999999, 8)
    )
  i <- 0
  for (metric in c("poisson", "mae", "mape", "mse", "msle", "kl", "cs", "r2")) {
    i <- as.integer(i + 1)
    tryCatch({
      # Regression Grid Evaluation Metrics----
      if (tolower(metric) == "poisson") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := Predict - get(Target) * log(Predict + 1)]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "mae") {
        ValidationData[, Metric := abs(get(Target) - Predict)]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mape") {
        ValidationData[, Metric := abs((get(Target) - Predict) / (Target + 1))]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mse") {
        ValidationData[, Metric := (get(Target) - Predict) ^ 2]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "msle") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := (log(get(Target) + 1) - log(Predict + 1)) ^ 2]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "kl") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := get(Target) * log((get(Target) + 1) /
                                                         (Predict + 1))]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "cs") {
        ValidationData[, ':=' (
          Metric1 = get(Target) * Predict,
          Metric2 = get(Target) ^ 2,
          Metric3 = Predict ^ 2
        )]
        Metric <-
          ValidationData[, sum(Metric1, na.rm = TRUE)] / (sqrt(ValidationData[, sum(Metric2, na.rm = TRUE)]) *
                                                            sqrt(ValidationData[, sum(Metric3, na.rm = TRUE)]))
      } else if (tolower(metric) == "r2") {
        Metric <-
          (ValidationData[, stats::cor(get(Target), Predict)][[1]]) ^ 2
      }
      data.table::set(
        EvaluationMetrics,
        i = i,
        j = 2L,
        value = round(Metric, 4)
      )
      data.table::set(EvaluationMetrics,
                      i = i,
                      j = 3L,
                      value = NA)
    }, error = function(x)
      "skip")
  }
  
  # Remove Features
  ValidationData[, ':=' (Metric1 = NULL,
                         Metric2 = NULL,
                         Metric3 = NULL)]
  
  # Regression Save EvaluationMetrics to File----
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  if (SaveModelObjects) {
    data.table::fwrite(EvaluationMetrics,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_EvaluationMetrics.csv"))
  }
  
  # Regression Partial Dependence----
  ParDepPlots <- list()
  j <- 0
  ParDepBoxPlots <- list()
  k <- 0
  for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
    if (!is.null(Alpha)) {
      tryCatch({
        Out <- ParDepCalPlots(
          data = ValidationData,
          PredictionColName = "Predict",
          TargetColName = Target,
          IndepVar = VariableImportance[i, Variable],
          GraphType = "calibration",
          PercentileBucket = 0.05,
          FactLevels = 10,
          Function = function(x)
            quantile(x,
                     probs = Alpha,
                     na.rm = TRUE)
        )
        
        j <- j + 1
        ParDepPlots[[paste0(VariableImportance[j, Variable])]] <-
          Out
      }, error = function(x)
        "skip")
      tryCatch({
        Out1 <- ParDepCalPlots(
          data = ValidationData,
          PredictionColName = "Predict",
          TargetColName = Target,
          IndepVar = VariableImportance[i, Variable],
          GraphType = "boxplot",
          PercentileBucket = 0.05,
          FactLevels = 10,
          Function = function(x)
            quantile(x,
                     probs = Alpha,
                     na.rm = TRUE)
        )
        
        k <- k + 1
        ParDepBoxPlots[[paste0(VariableImportance[k, Variable])]] <-
          Out1
      }, error = function(x)
        "skip")
    } else {
      tryCatch({
        Out <- ParDepCalPlots(
          data = ValidationData,
          PredictionColName = "Predict",
          TargetColName = Target,
          IndepVar = VariableImportance[i, Variable],
          GraphType = "calibration",
          PercentileBucket = 0.05,
          FactLevels = 10,
          Function = function(x)
            mean(x, na.rm = TRUE)
        )
        
        j <- j + 1
        ParDepPlots[[paste0(VariableImportance[j, Variable])]] <-
          Out
      }, error = function(x)
        "skip")
      tryCatch({
        Out1 <- ParDepCalPlots(
          data = ValidationData,
          PredictionColName = "Predict",
          TargetColName = Target,
          IndepVar = VariableImportance[i, Variable],
          GraphType = "boxplot",
          PercentileBucket = 0.05,
          FactLevels = 10,
          Function = function(x)
            mean(x, na.rm = TRUE)
        )
        
        k <- k + 1
        ParDepBoxPlots[[paste0(VariableImportance[k, Variable])]] <-
          Out1
      }, error = function(x)
        "skip")
    }
  }
  
  # Regression Save ParDepPlots to file----
  if (SaveModelObjects) {
    save(ParDepPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepPlots.R"))
  }
  
  # Regression Save ParDepBoxPlots to file----
  if (SaveModelObjects) {
    save(ParDepBoxPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepBoxPlots.R"))
  }
  
  # Regression Return Objects----
  if (ReturnModelObjects) {
    return(
      list(
        Model = FinalModel,
        ValidationData = ValidationData,
        EvaluationPlot = EvaluationPlot,
        EvaluationBoxPlot = EvaluationBoxPlot,
        EvaluationMetrics = EvaluationMetrics,
        VariableImportance = VariableImportance,
        PartialDependencePlots = ParDepPlots,
        PartialDependenceBoxPlots = ParDepBoxPlots
      )
    )
  }
}

#' AutoH2oDRFRegression is an automated H2O modeling framework with grid-tuning and model evaluation
#'
#' AutoH2oDRFRegression is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "MSE", "RMSE", "MAE", "RMSLE"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxMem Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create. Calibration boxplots will only be created for numerical features (not dummy variables)
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' TestModel <- AutoH2oDRFRegression(data,
#'                                   ValidationData = NULL,
#'                                   TestData = NULL,
#'                                   TargetColumnName = "Target",
#'                                   FeatureColNames = 2:ncol(data),
#'                                   eval_metric = "RMSE",
#'                                   Trees = 50,
#'                                   GridTune = FALSE,
#'                                   MaxMem = "32G",
#'                                   MaxModelsInGrid = 10,
#'                                   model_path = NULL,
#'                                   ModelID = "FirstModel",
#'                                   NumOfParDepPlots = 3,
#'                                   ReturnModelObjects = TRUE,
#'                                   SaveModelObjects = FALSE,
#'                                   IfSaveModel = "mojo")
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and GridList
#' @export
AutoH2oDRFRegression <- function(data,
                                 ValidationData = NULL,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 eval_metric = "RMSE",
                                 Trees = 50,
                                 GridTune = FALSE,
                                 MaxMem = "32G",
                                 MaxModelsInGrid = 2,
                                 model_path = NULL,
                                 ModelID = "FirstModel",
                                 NumOfParDepPlots = 3,
                                 ReturnModelObjects = TRUE,
                                 SaveModelObjects = FALSE,
                                 IfSaveModel = "mojo") {
  # Regression Check Arguments----
  if (!(tolower(eval_metric) %chin% c("mse", "rmse", "mae", "rmsle"))) {
    warning("eval_metric not in MSE, RMSE, MAE, RMSLE")
    
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (MaxModelsInGrid < 1 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path))
      warning("model_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (NumOfParDepPlots < 0)
    warning("NumOfParDepPlots needs to be a positive number")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  
  # Regression Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Regression Ensure data is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # Regression Ensure data is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # Regression Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = NULL,
      TimeColumnName = NULL
    )
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # Regression ModelDataPrep----
  dataTrain <- ModelDataPrep(data = data,
                             Impute = FALSE,
                             CharToFactor = TRUE)
  
  # Regression ModelDataPrep----
  dataTest <- ModelDataPrep(data = ValidationData,
                            Impute = FALSE,
                            CharToFactor = TRUE)
  
  # Regression ModelDataPrep----
  if (!is.null(TestData)) {
    TestData <- ModelDataPrep(data = TestData,
                              Impute = FALSE,
                              CharToFactor = TRUE)
  }
  
  # Regression Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # Regression Get Min Value of Target Data----
  MinVal <- min(data[[eval(Target)]], na.rm = TRUE)
  
  # Regression Grid Tune Check----
  if (GridTune) {
    # Regression Start Up H2O----
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    
    # Regression Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
    
    # Regression Grid Tune Search Criteria----
    search_criteria  <- list(
      strategy             = "RandomDiscrete",
      max_runtime_secs     = 3600 * 24 * 7,
      max_models           = MaxModelsInGrid,
      seed                 = 1234,
      stopping_rounds      = 10,
      stopping_metric      = toupper(eval_metric),
      stopping_tolerance   = 1e-3
    )
    
    # Regression Grid Parameters----
    hyper_params <- list(
      max_depth                        = c(6, 9, 12),
      sample_rate                      = c(0.5, 0.75, 1.0),
      col_sample_rate_per_tree         = c(0.5, 0.75, 1.0),
      col_sample_rate_change_per_level = c(0.9, 1.0, 1.1),
      min_rows                         = c(1, 10),
      nbins                            = c(10, 20, 30),
      nbins_cats                       = c(64, 256, 512),
      histogram_type                   = c("UniformAdaptive",
                                           "QuantilesGlobal",
                                           "RoundRobin")
    )
    
    # Regression Grid Train Model----
    grid <- h2o::h2o.grid(
      hyper_params         = hyper_params,
      search_criteria      = search_criteria,
      is_supervised        = TRUE,
      algorithm            = "randomForest",
      grid_id              = paste0(ModelID, "_Grid"),
      x                    = FeatureColNames,
      y                    = TargetColumnName,
      ntrees               = Trees,
      training_frame       = datatrain,
      validation_frame     = datavalidate,
      max_runtime_secs     = 3600 * 24 * 7,
      stopping_rounds      = 10,
      stopping_tolerance   = 1e-3,
      stopping_metric      = toupper(eval_metric),
      score_tree_interval  = 10,
      seed                 = 1234
    )
    
    # Regression Get Best Model----
    Grid_Out   <- h2o::h2o.getGrid(
      grid_id = paste0(ModelID, "_Grid"),
      sort_by = eval_metric,
      decreasing = FALSE
    )
    
    # Regression Collect Best Grid Model----
    grid_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1]])
  }
  
  # Regression Start Up H2O----
  if (!GridTune) {
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    
    # Regression Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
  }
  
  # Regression Baseline Model----
  base_model <- h2o::h2o.randomForest(
    x                = FeatureColNames,
    y                = TargetColumnName,
    training_frame   = datatrain,
    validation_frame = datavalidate,
    model_id         = ModelID,
    ntrees           = Trees
  )
  
  # Regression Grab Evaluation Metric----
  if (GridTune) {
    if (!is.null(TestData)) {
      datatest        <-  h2o::as.h2o(TestData)
      if (tolower(eval_metric) == "mse") {
        GridModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = grid_model,
                                            newdata = datatest))
        BaseModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = base_model,
                                            newdata = datatest))
      } else if (tolower(eval_metric) == "rmse") {
        GridModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = grid_model,
                                             newdata = datatest))
        BaseModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = base_model,
                                             newdata = datatest))
      } else if (tolower(eval_metric) == "mae") {
        GridModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = grid_model,
                                            newdata = datatest))
        BaseModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = base_model,
                                            newdata = datatest))
      } else if (tolower(eval_metric) == "rmsle") {
        GridModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = grid_model,
                                              newdata = datatest))
        BaseModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = base_model,
                                              newdata = datatest))
      }
    } else {
      if (tolower(eval_metric) == "mse") {
        GridModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = grid_model,
                                            newdata = datavalidate))
        BaseModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = base_model,
                                            newdata = datavalidate))
      } else if (tolower(eval_metric) == "rmse") {
        GridModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = grid_model,
                                             newdata = datavalidate))
        BaseModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = base_model,
                                             newdata = datavalidate))
      } else if (tolower(eval_metric) == "mae") {
        GridModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = grid_model,
                                            newdata = datavalidate))
        BaseModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = base_model,
                                            newdata = datavalidate))
      } else if (tolower(eval_metric) == "rmsle") {
        GridModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = grid_model,
                                              newdata = datavalidate))
        BaseModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = base_model,
                                              newdata = datavalidate))
      }
      
    }
  } else {
    if (!is.null(TestData)) {
      datatest        <-  h2o::as.h2o(TestData)
      if (tolower(eval_metric) == "mse") {
        BaseModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = base_model,
                                            newdata = datatest))
      } else if (tolower(eval_metric) == "rmse") {
        BaseModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = base_model,
                                             newdata = datatest))
      } else if (tolower(eval_metric) == "mae") {
        BaseModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = base_model,
                                            newdata = datatest))
      } else if (tolower(eval_metric) == "rmsle") {
        BaseModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = base_model,
                                              newdata = datatest))
      }
    } else {
      if (tolower(eval_metric) == "mse") {
        BaseModelEval <-
          h2o::h2o.mse(h2o::h2o.performance(model = base_model,
                                            newdata = datavalidate))
      } else if (tolower(eval_metric) == "rmse") {
        BaseModelEval <-
          h2o::h2o.rmse(h2o::h2o.performance(model = base_model,
                                             newdata = datavalidate))
      } else if (tolower(eval_metric) == "mae") {
        BaseModelEval <-
          h2o::h2o.mae(h2o::h2o.performance(model = base_model,
                                            newdata = datavalidate))
      } else if (tolower(eval_metric) == "rmsle") {
        BaseModelEval <-
          h2o::h2o.rmsle(h2o::h2o.performance(model = base_model,
                                              newdata = datavalidate))
      }
    }
  }
  
  # Regression Pick Winner----
  if (GridTune) {
    if (GridModelEval < BaseModelEval) {
      FinalModel <- grid_model
    } else {
      FinalModel <- base_model
    }
  } else {
    FinalModel <- base_model
  }
  
  # Regression Save Model----
  if (SaveModelObjects) {
    if (tolower(IfSaveModel) == "mojo") {
      SaveModel <- h2o::h2o.saveMojo(object = FinalModel,
                                     path = model_path,
                                     force = TRUE)
      h2o::h2o.download_mojo(
        model = FinalModel,
        path = model_path,
        get_genmodel_jar = TRUE,
        genmodel_path = model_path,
        genmodel_name = ModelID
      )
    } else {
      SaveModel <- h2o::h2o.saveModel(object = FinalModel,
                                      path = model_path,
                                      force = TRUE)
    }
  }
  
  # Regression Score Final Test Data----
  if (!is.null(TestData)) {
    Predict <-
      data.table::as.data.table(h2o::h2o.predict(object = FinalModel,
                                                 newdata = datatest))
    
  } else {
    Predict <-
      data.table::as.data.table(h2o::h2o.predict(object = FinalModel,
                                                 newdata = datavalidate))
  }
  
  # Regression Variable Importance----
  VariableImportance <-
    data.table::as.data.table(h2o::h2o.varimp(object = FinalModel))
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_VariableImportance.csv"))
  }
  
  # Regression Format Variable Importance Table----
  data.table::setnames(
    VariableImportance,
    c(
      "variable",
      "relative_importance",
      "scaled_importance",
      "percentage"
    ),
    c(
      "Variable",
      "RelativeImportance",
      "ScaledImportance",
      "Percentage"
    )
  )
  VariableImportance[, ':=' (
    RelativeImportance = round(RelativeImportance, 4),
    ScaledImportance = round(ScaledImportance, 4),
    Percentage = round(Percentage, 4)
  )]
  
  # Regression H2O Shutdown----
  h2o::h2o.shutdown(prompt = FALSE)
  
  # Regression Create Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(TestData, Predict))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(dataTest, Predict))
  }
  
  # Regression Change Prediction Name----
  data.table::setnames(ValidationData, "predict", "Predict")
  
  # Regression Get R2----
  r_squared <-
    (ValidationData[, stats::cor(eval(Target), Predict)][[1]]) ^ 2
  
  # Regression Save Validation Data to File----
  if (SaveModelObjects) {
    data.table::fwrite(ValidationData,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_ValidationData.csv"))
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "Predict",
    TargetColName = Target,
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Regression Evaluation Plot Update Title----
  if (GridTune) {
    val <- max(GridModelEval, BaseModelEval)
    EvaluationPlot <- EvaluationPlot +
      ggplot2::ggtitle(paste0(
        "Random Forest Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(val, 3)
      ))
  } else {
    EvaluationPlot <- EvaluationPlot +
      ggplot2::ggtitle(paste0(
        "Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(BaseModelEval, 3)
      ))
  }
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID,
                           "_EvaluationPlot.png"))
  }
  
  # Regression Evaluation BoxPlot----
  EvaluationBoxPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "Predict",
    TargetColName = Target,
    GraphType = "boxplot",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Regression Evaluation Plot Update Title----
  if (GridTune) {
    val <- max(GridModelEval, BaseModelEval)
    EvaluationBoxPlot <- EvaluationBoxPlot +
      ggplot2::ggtitle(paste0(
        "Random Forest Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(val, 3)
      ))
  } else {
    EvaluationBoxPlot <- EvaluationBoxPlot +
      ggplot2::ggtitle(paste0(
        "Random Forest Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(BaseModelEval, 3)
      ))
  }
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID,
                           "_EvaluationBoxPlot.png"))
  }
  
  # Regression Evaluation Metrics----
  EvaluationMetrics <-
    data.table::data.table(
      Metric = c("Poisson", "MAE",
                 "MAPE", "MSE", "MSLE",
                 "KL", "CS", "R2"),
      MetricValue = rep(999999, 8)
    )
  i <- 0
  for (metric in c("poisson", "mae", "mape", "mse", "msle", "kl", "cs", "r2")) {
    i <- as.integer(i + 1)
    tryCatch({
      # Regression Grid Evaluation Metrics----
      if (tolower(metric) == "poisson") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := Predict - get(Target) * log(Predict + 1)]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "mae") {
        ValidationData[, Metric := abs(get(Target) - Predict)]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mape") {
        ValidationData[, Metric := abs((get(Target) - Predict) / (Target + 1))]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mse") {
        ValidationData[, Metric := (get(Target) - Predict) ^ 2]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "msle") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := (log(get(Target) + 1) - log(Predict + 1)) ^ 2]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "kl") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := get(Target) * log((get(Target) + 1) /
                                                         (Predict + 1))]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "cs") {
        ValidationData[, ':=' (
          Metric1 = get(Target) * Predict,
          Metric2 = get(Target) ^ 2,
          Metric3 = Predict ^ 2
        )]
        Metric <-
          ValidationData[, sum(Metric1, na.rm = TRUE)] / (sqrt(ValidationData[, sum(Metric2, na.rm = TRUE)]) *
                                                            sqrt(ValidationData[, sum(Metric3, na.rm = TRUE)]))
      } else if (tolower(metric) == "r2") {
        Metric <-
          (ValidationData[, stats::cor(get(Target), Predict)][[1]]) ^ 2
      }
      data.table::set(
        EvaluationMetrics,
        i = i,
        j = 2L,
        value = round(Metric, 4)
      )
      data.table::set(EvaluationMetrics,
                      i = i,
                      j = 3L,
                      value = NA)
    }, error = function(x)
      "skip")
  }
  
  # Regression Save EvaluationMetrics to File----
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  if (SaveModelObjects) {
    data.table::fwrite(EvaluationMetrics,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_EvaluationMetrics.csv"))
  }
  
  # Regression Partial Dependence----
  ParDepPlots <- list()
  j <- 0
  ParDepBoxPlots <- list()
  k <- 0
  for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
    tryCatch({
      Out <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "Predict",
        TargetColName = Target,
        IndepVar = VariableImportance[i, Variable],
        GraphType = "calibration",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      j <- j + 1
      ParDepPlots[[paste0(VariableImportance[j, Variable])]] <-
        Out
    }, error = function(x)
      "skip")
    tryCatch({
      Out1 <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "Predict",
        TargetColName = Target,
        IndepVar = VariableImportance[i, Variable],
        GraphType = "boxplot",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      k <- k + 1
      ParDepBoxPlots[[paste0(VariableImportance[k, Variable])]] <-
        Out1
    }, error = function(x)
      "skip")
    
  }
  
  # Regression Save ParDepPlots to file----
  if (SaveModelObjects) {
    save(ParDepPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepPlots.R"))
  }
  
  # Regression Save ParDepBoxPlots to file----
  if (SaveModelObjects) {
    save(ParDepBoxPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepBoxPlots.R"))
  }
  
  # Regression Return Objects----
  if (ReturnModelObjects) {
    return(
      list(
        Model = FinalModel,
        ValidationData = ValidationData,
        EvaluationPlot = EvaluationPlot,
        EvaluationBoxPlot = EvaluationBoxPlot,
        EvaluationMetrics = EvaluationMetrics,
        VariableImportance = VariableImportance,
        PartialDependencePlots = ParDepPlots,
        PartialDependenceBoxPlots = ParDepBoxPlots
      )
    )
  }
}

#' AutoH2oGBMClassifier is an automated H2O modeling framework with grid-tuning and model evaluation
#'
#' AutoH2oGBMClassifier is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "AUC" or "logloss"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxMem Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' data[, Target := as.factor(ifelse(Independent_Variable2 < 0.5, 1, 0))]
#' TestModel <- AutoH2oGBMClassifier(data,
#'                                   ValidationData = NULL,
#'                                   TestData = NULL,
#'                                   TargetColumnName = "Target",
#'                                   FeatureColNames = 2:ncol(data),
#'                                   eval_metric = "auc",
#'                                   Trees = 50,
#'                                   GridTune = FALSE,
#'                                   MaxMem = "32G",
#'                                   MaxModelsInGrid = 10,
#'                                   model_path = NULL,
#'                                   ModelID = "FirstModel",
#'                                   NumOfParDepPlots = 3,
#'                                   ReturnModelObjects = TRUE,
#'                                   SaveModelObjects = FALSE,
#'                                   IfSaveModel = "mojo")
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoH2oGBMClassifier <- function(data,
                                 ValidationData = NULL,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 eval_metric = "auc",
                                 Trees = 50,
                                 GridTune = FALSE,
                                 MaxMem = "32G",
                                 MaxModelsInGrid = 2,
                                 model_path = NULL,
                                 ModelID = "FirstModel",
                                 NumOfParDepPlots = 3,
                                 ReturnModelObjects = TRUE,
                                 SaveModelObjects = FALSE,
                                 IfSaveModel = "mojo") {
  # Binary Check Arguments----
  if (!(tolower(eval_metric) %chin% c("auc", "logloss"))) {
    warning("eval_metric not in AUC, logloss")
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (MaxModelsInGrid < 1 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path))
      warning("model_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (NumOfParDepPlots < 0)
    warning("NumOfParDepPlots needs to be a positive number")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  if (!(tolower(eval_metric) == "auc")) {
    eval_metric <- tolower(eval_metric)
  } else {
    eval_metric <- toupper(eval_metric)
  }
  if (tolower(eval_metric) %chin% c("auc")) {
    Decreasing <- TRUE
  } else {
    Decreasing <- FALSE
  }
  
  # Binary Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # Binary Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Binary Ensure data is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # Binary Ensure data is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # Binary Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = Target,
      TimeColumnName = NULL
    )
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # Binary ModelDataPrep----
  dataTrain <- ModelDataPrep(data = data,
                             Impute = FALSE,
                             CharToFactor = TRUE)
  
  # Binary ModelDataPrep----
  dataTest <- ModelDataPrep(data = ValidationData,
                            Impute = FALSE,
                            CharToFactor = TRUE)
  
  # Binary ModelDataPrep----
  if (!is.null(TestData)) {
    TestData <- ModelDataPrep(data = TestData,
                              Impute = FALSE,
                              CharToFactor = TRUE)
  }
  
  # Binary Get Min Value of Target Data----
  MinVal <- min(as.numeric(data[[eval(Target)]]), na.rm = TRUE)
  MaxVal <- max(as.numeric(data[[eval(Target)]]), na.rm = TRUE)
  if (MaxVal - MinVal > 1)
    warning("Target Variable is not binary")
  
  # Binary Ensure Target Is a Factor Type----
  if (!is.factor(dataTrain[[eval(Target)]])) {
    dataTrain[, eval(Target) := as.factor(get(Target))]
  }
  
  # Binary Ensure Target Is a Factor Type----
  if (!is.factor(dataTest[[eval(Target)]])) {
    dataTest[, eval(Target) := as.factor(get(Target))]
  }
  
  # Binary Ensure Target Is a Factor Type----
  if (!is.null(TestData)) {
    if (!is.factor(TestData[[eval(Target)]])) {
      TestData[, eval(Target) := as.factor(get(Target))]
    }
  }
  
  # Binary Grid Tune Check----
  if (GridTune) {
    # Binary Start Up H2O----
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    
    # Binary Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
    
    # Binary Grid Tune Search Criteria----
    search_criteria  <- list(
      strategy             = "RandomDiscrete",
      max_runtime_secs     = 3600 * 24 * 7,
      max_models           = MaxModelsInGrid,
      seed                 = 1234,
      stopping_rounds      = 10,
      stopping_metric      = eval_metric,
      stopping_tolerance   = 1e-3
    )
    
    # Binary Grid Parameters----
    hyper_params <- list(
      max_depth                        = c(6, 9, 12),
      balance_classes                  = c(TRUE, FALSE),
      sample_rate                      = c(0.5, 0.75, 1.0),
      col_sample_rate_per_tree         = c(0.5, 0.75, 1.0),
      col_sample_rate_change_per_level = c(0.9, 1.0, 1.1),
      min_rows                         = c(1, 5),
      nbins                            = c(10, 20, 30),
      nbins_cats                       = c(64, 256, 512),
      histogram_type                   = c("UniformAdaptive",
                                           "QuantilesGlobal",
                                           "RoundRobin")
    )
    
    # Binary Grid Train Model----
    grid <- h2o::h2o.grid(
      hyper_params         = hyper_params,
      search_criteria      = search_criteria,
      is_supervised        = TRUE,
      algorithm            = "gbm",
      distribution         = "bernoulli",
      grid_id              = paste0(ModelID, "_Grid"),
      x                    = FeatureColNames,
      y                    = TargetColumnName,
      ntrees               = Trees,
      training_frame       = datatrain,
      validation_frame     = datavalidate,
      max_runtime_secs     = 3600 * 24 * 7,
      stopping_rounds      = 10,
      stopping_tolerance   = 1e-3,
      stopping_metric      = eval_metric,
      score_tree_interval  = 10,
      seed                 = 1234
    )
    
    # Binary Get Best Model----
    Grid_Out   <- h2o::h2o.getGrid(
      grid_id = paste0(ModelID, "_Grid"),
      sort_by = eval_metric,
      decreasing = Decreasing
    )
    
    # Binary Collect Best Grid Model----
    grid_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1]])
  }
  
  # Binary Start Up H2O----
  if (!GridTune) {
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    
    # Binary Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
  }
  
  # Binary Build Baseline Model----
  base_model <- h2o::h2o.gbm(
    x                = FeatureColNames,
    y                = TargetColumnName,
    distribution     = "bernoulli",
    training_frame   = datatrain,
    validation_frame = datavalidate,
    model_id         = ModelID,
    ntrees           = Trees
  )
  
  # Binary Get Metrics----
  if (GridTune) {
    if (!is.null(TestData)) {
      datatest        <-  h2o::as.h2o(TestData)
      GridMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datatest)
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datatest)
    } else {
      GridMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datavalidate)
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datavalidate)
    }
  } else {
    if (!is.null(TestData)) {
      datatest        <-  h2o::as.h2o(TestData)
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datatest)
    } else {
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datavalidate)
    }
  }
  
  # Binary Evaluate Metrics----
  if (GridTune) {
    if (tolower(eval_metric) == "auc") {
      BaseMetric <- BaseMetrics@metrics$AUC
      GridMetric <- GridMetrics@metrics$AUC
      if (GridMetric > BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        FinalThresholdTable <-
          data.table::as.data.table(GridMetrics@metrics$max_criteria_and_metric_scores)
        data.table::setnames(
          FinalThresholdTable,
          c("metric", "threshold", "value"),
          c("Metric", "Threshold", "Value")
        )
        FinalThresholdTable[, idx := NULL]
        FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4),
                                    Value = round(Value, 4))]
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        FinalThresholdTable <-
          data.table::as.data.table(BaseMetrics@metrics$max_criteria_and_metric_scores)
        data.table::setnames(
          FinalThresholdTable,
          c("metric", "threshold", "value"),
          c("Metric", "Threshold", "Value")
        )
        FinalThresholdTable[, idx := NULL]
        FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4),
                                    Value = round(Value, 4))]
      }
    } else if (tolower(eval_metric) == "logloss") {
      BaseMetric <- BaseMetrics@metrics$logloss
      GridMetric <- GridMetrics@metrics$logloss
      if (GridMetric < BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        FinalThresholdTable <-
          data.table::as.data.table(GridMetrics@metrics$max_criteria_and_metric_scores)
        data.table::setnames(
          FinalThresholdTable,
          c("metric", "threshold", "value"),
          c("Metric", "Threshold", "Value")
        )
        FinalThresholdTable[, idx := NULL]
        FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4),
                                    Value = round(Value, 4))]
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        FinalThresholdTable <-
          data.table::as.data.table(BaseMetrics@metrics$max_criteria_and_metric_scores)
        data.table::setnames(
          FinalThresholdTable,
          c("metric", "threshold", "value"),
          c("Metric", "Threshold", "Value")
        )
        FinalThresholdTable[, idx := NULL]
        FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4),
                                    Value = round(Value, 4))]
      }
    }
  } else {
    if (tolower(eval_metric) == "auc") {
      BaseMetric <- BaseMetrics@metrics$AUC
      FinalModel <- base_model
      EvalMetric <- BaseMetrics@metrics$AUC
      FinalThresholdTable <-
        data.table::as.data.table(BaseMetrics@metrics$max_criteria_and_metric_scores)
      data.table::setnames(
        FinalThresholdTable,
        c("metric", "threshold", "value"),
        c("Metric", "Threshold", "Value")
      )
      FinalThresholdTable[, idx := NULL]
      FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4),
                                  Value = round(Value, 4))]
    } else {
      BaseMetric <- BaseMetrics@metrics$logloss
      FinalModel <- base_model
      EvalMetric <- BaseMetric
      FinalThresholdTable <-
        data.table::as.data.table(BaseMetrics@metrics$max_criteria_and_metric_scores)
      data.table::setnames(
        FinalThresholdTable,
        c("metric", "threshold", "value"),
        c("Metric", "Threshold", "Value")
      )
      FinalThresholdTable[, idx := NULL]
      FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4),
                                  Value = round(Value, 4))]
    }
  }
  
  # Binary Save Final Model----
  if (SaveModelObjects) {
    if (tolower(IfSaveModel) == "mojo") {
      SaveModel <- h2o::h2o.saveMojo(object = FinalModel,
                                     path = model_path,
                                     force = TRUE)
      h2o::h2o.download_mojo(
        model = FinalModel,
        path = model_path,
        get_genmodel_jar = TRUE,
        genmodel_path = model_path,
        genmodel_name = ModelID
      )
    } else {
      SaveModel <- h2o::h2o.saveModel(object = FinalModel,
                                      path = model_path,
                                      force = TRUE)
    }
  }
  
  # Binary Score Final Test Data----
  if (!is.null(TestData)) {
    Predict <-
      data.table::as.data.table(h2o::h2o.predict(object = FinalModel,
                                                 newdata = datatest))
    Predict[, p0 := NULL]
    
  } else {
    Predict <-
      data.table::as.data.table(h2o::h2o.predict(object = FinalModel,
                                                 newdata = datavalidate))
    Predict[, p0 := NULL]
  }
  
  # Binary Variable Importance----
  VariableImportance <-
    data.table::as.data.table(h2o::h2o.varimp(object = FinalModel))
  
  # Binary Format Variable Importance Table----
  data.table::setnames(
    VariableImportance,
    c(
      "variable",
      "relative_importance",
      "scaled_importance",
      "percentage"
    ),
    c(
      "Variable",
      "RelativeImportance",
      "ScaledImportance",
      "Percentage"
    )
  )
  VariableImportance[, ':=' (
    RelativeImportance = round(RelativeImportance, 4),
    ScaledImportance = round(ScaledImportance, 4),
    Percentage = round(Percentage, 4)
  )]
  
  # Binary Save Variable Importance----
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_VariableImportance.csv"))
  }
  
  # Binary H2O Shutdown----
  h2o::h2o.shutdown(prompt = FALSE)
  
  # Binary Create Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(TestData, Predict))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(dataTest, Predict))
  }
  
  # Binary Change Prediction Name----
  data.table::setnames(ValidationData, "predict", "Predict")
  
  # Binary Save Validation Data to File----
  if (SaveModelObjects) {
    data.table::fwrite(ValidationData,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_ValidationData.csv"))
  }
  
  # Binary Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "p1",
    TargetColName = Target,
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Binary Evaluation Plot Update Title----
  if (GridTune) {
    EvaluationPlot <- EvaluationPlot +
      ggplot2::ggtitle(paste0(
        "GBM Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(EvalMetric@metrics$AUC, 3)
      ))
  } else {
    EvaluationPlot <- EvaluationPlot +
      ggplot2::ggtitle(paste0(
        "Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(EvalMetric, 3)
      ))
  }
  
  # Binary Save plot to file----
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID,
                           "_EvaluationPlot.png"))
  }
  
  # Binary AUC Object Create----
  AUC_Metrics <-
    pROC::roc(
      response = ValidationData[[eval(Target)]],
      predictor = ValidationData[["p1"]],
      na.rm = TRUE,
      algorithm = 3,
      auc = TRUE,
      ci = TRUE
    )
  
  # Binary AUC Conversion to data.table----
  AUC_Data <- data.table::data.table(
    ModelNumber = 0,
    Sensitivity = AUC_Metrics$sensitivities,
    Specificity = AUC_Metrics$specificities
  )
  
  # Binary Plot ROC Curve----
  ROC_Plot <-
    ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity)) +
    ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]]), color = "blue") +
    ggplot2::geom_abline(slope = 1, color = "black") +
    ggplot2::ggtitle(paste0("GBM AUC: ",
                            100 * round(AUC_Metrics$auc, 3), "%")) +
    ChartTheme() + ggplot2::xlab("Specificity") +
    ggplot2::ylab("Sensitivity")
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID,
                           "_ROC_Plot.png"))
  }
  
  # Binary Save EvaluationMetrics to File----
  if (SaveModelObjects) {
    data.table::fwrite(FinalThresholdTable,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_EvaluationMetrics.csv"))
  }
  
  # Binary Partial Dependence----
  ParDepPlots <- list()
  j <- 0
  for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
    tryCatch({
      Out <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "p1",
        TargetColName = Target,
        IndepVar = VariableImportance[i, Variable],
        GraphType = "calibration",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      j <- j + 1
      ParDepPlots[[paste0(VariableImportance[j, Variable])]] <-
        Out
    }, error = function(x)
      "skip")
  }
  
  # Binary Save ParDepPlots to file----
  if (SaveModelObjects) {
    save(ParDepPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepPlots.R"))
  }
  
  # Binary Return Objects----
  if (ReturnModelObjects) {
    return(
      list(
        Model = FinalModel,
        ValidationData = ValidationData,
        ROC_Plot = ROC_Plot,
        EvaluationPlot = EvaluationPlot,
        EvaluationMetrics = FinalThresholdTable,
        VariableImportance = VariableImportance,
        PartialDependencePlots = ParDepPlots
      )
    )
  }
}

#' AutoH2oDRFClassifier is an automated H2O modeling framework with grid-tuning and model evaluation
#'
#' AutoH2oDRFClassifier is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation metrics, variable importance, partial dependence calibration plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "AUC" or "logloss"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxMem Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' data[, Target := as.factor(ifelse(Independent_Variable2 < 0.5, 1, 0))]
#' TestModel <- AutoH2oDRFClassifier(data,
#'                                   ValidationData = NULL,
#'                                   TestData = NULL,
#'                                   TargetColumnName = "Target",
#'                                   FeatureColNames = 2:ncol(data),
#'                                   eval_metric = "auc",
#'                                   Trees = 50,
#'                                   GridTune = FALSE,
#'                                   MaxMem = "32G",
#'                                   MaxModelsInGrid = 10,
#'                                   model_path = NULL,
#'                                   ModelID = "FirstModel",
#'                                   NumOfParDepPlots = 3,
#'                                   ReturnModelObjects = TRUE,
#'                                   SaveModelObjects = FALSE,
#'                                   IfSaveModel = "mojo")
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoH2oDRFClassifier <- function(data,
                                 ValidationData = NULL,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 eval_metric = "auc",
                                 Trees = 50,
                                 GridTune = FALSE,
                                 MaxMem = "32G",
                                 MaxModelsInGrid = 2,
                                 model_path = NULL,
                                 ModelID = "FirstModel",
                                 NumOfParDepPlots = 3,
                                 ReturnModelObjects = TRUE,
                                 SaveModelObjects = FALSE,
                                 IfSaveModel = "mojo") {
  # Binary Check Arguments----
  if (!(tolower(eval_metric) %chin% c("auc", "logloss"))) {
    warning("eval_metric not in AUC, logloss")
    
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (MaxModelsInGrid < 1 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path))
      warning("model_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (NumOfParDepPlots < 0)
    warning("NumOfParDepPlots needs to be a positive number")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  if (!(tolower(eval_metric) == "auc")) {
    eval_metric <- tolower(eval_metric)
  } else {
    eval_metric <- toupper(eval_metric)
  }
  if (tolower(eval_metric) %chin% c("auc")) {
    Decreasing <- TRUE
  } else {
    Decreasing <- FALSE
  }
  
  # Binary Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # Binary Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Binary Ensure data is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # Binary Ensure data is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # Binary Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = Target,
      TimeColumnName = NULL
    )
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # Binary ModelDataPrep----
  dataTrain <- ModelDataPrep(data = data,
                             Impute = FALSE,
                             CharToFactor = TRUE)
  
  # Binary ModelDataPrep----
  dataTest <- ModelDataPrep(data = ValidationData,
                            Impute = FALSE,
                            CharToFactor = TRUE)
  
  # Binary ModelDataPrep----
  if (!is.null(TestData)) {
    TestData <- ModelDataPrep(data = TestData,
                              Impute = FALSE,
                              CharToFactor = TRUE)
  }
  
  # Binary Get Min Value of Target Data----
  MinVal <- min(as.numeric(data[[eval(Target)]]), na.rm = TRUE)
  MaxVal <- max(as.numeric(data[[eval(Target)]]), na.rm = TRUE)
  if (MaxVal - MinVal > 1)
    warning("Target Variable is not binary")
  
  # Binary Ensure Target Is a Factor Type----
  if (!is.factor(dataTrain[[eval(Target)]])) {
    dataTrain[, eval(Target) := as.factor(get(Target))]
  }
  
  # Binary Ensure Target Is a Factor Type----
  if (!is.factor(dataTest[[eval(Target)]])) {
    dataTest[, eval(Target) := as.factor(get(Target))]
  }
  
  # Binary Ensure Target Is a Factor Type----
  if (!is.null(TestData)) {
    if (!is.factor(TestData[[eval(Target)]])) {
      TestData[, eval(Target) := as.factor(get(Target))]
    }
  }
  
  # Binary Grid Tune Check----
  if (GridTune) {
    # Binary Start Up H2O----
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    
    # Binary Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
    
    # Binary Grid Tune Search Criteria----
    search_criteria  <- list(
      strategy             = "RandomDiscrete",
      max_runtime_secs     = 3600 * 24 * 7,
      max_models           = MaxModelsInGrid,
      seed                 = 1234,
      stopping_rounds      = 10,
      stopping_metric      = eval_metric,
      stopping_tolerance   = 1e-3
    )
    
    # Binary Grid Parameters----
    hyper_params <- list(
      max_depth                        = c(6, 9, 12),
      balance_classes                  = c(TRUE, FALSE),
      sample_rate                      = c(0.5, 0.75, 1.0),
      col_sample_rate_per_tree         = c(0.5, 0.75, 1.0),
      col_sample_rate_change_per_level = c(0.9, 1.0, 1.1),
      min_rows                         = c(1, 5),
      nbins                            = c(10, 20, 30),
      nbins_cats                       = c(64, 256, 512),
      histogram_type                   = c("UniformAdaptive",
                                           "QuantilesGlobal",
                                           "RoundRobin")
    )
    
    # Binary Grid Train Model----
    grid <- h2o::h2o.grid(
      hyper_params         = hyper_params,
      search_criteria      = search_criteria,
      is_supervised        = TRUE,
      algorithm            = "randomForest",
      grid_id              = paste0(ModelID, "_Grid"),
      x                    = FeatureColNames,
      y                    = TargetColumnName,
      ntrees               = Trees,
      training_frame       = datatrain,
      validation_frame     = datavalidate,
      max_runtime_secs     = 3600 * 24 * 7,
      stopping_rounds      = 10,
      stopping_tolerance   = 1e-3,
      stopping_metric      = eval_metric,
      score_tree_interval  = 10,
      seed                 = 1234
    )
    
    # Binary Get Best Model----
    Grid_Out   <- h2o::h2o.getGrid(
      grid_id = paste0(ModelID, "_Grid"),
      sort_by = eval_metric,
      decreasing = Decreasing
    )
    
    # Binary Collect Best Grid Model----
    grid_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1]])
  }
  
  # Binary Start Up H2O----
  if (!GridTune) {
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    
    # Binary Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
  }
  
  # Binary Build Baseline Model----
  base_model <- h2o::h2o.randomForest(
    x                = FeatureColNames,
    y                = TargetColumnName,
    training_frame   = datatrain,
    validation_frame = datavalidate,
    model_id         = ModelID,
    ntrees           = Trees
  )
  
  # Binary Get Metrics----
  if (GridTune) {
    if (!is.null(TestData)) {
      datatest        <-  h2o::as.h2o(TestData)
      GridMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datatest)
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datatest)
    } else {
      GridMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datavalidate)
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datavalidate)
    }
  } else {
    if (!is.null(TestData)) {
      datatest        <-  h2o::as.h2o(TestData)
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datatest)
    } else {
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datavalidate)
    }
  }
  
  # Binary Evaluate Metrics----
  if (GridTune) {
    if (tolower(eval_metric) == "auc") {
      BaseMetric <- BaseMetrics@metrics$AUC
      GridMetric <- GridMetrics@metrics$AUC
      if (GridMetric > BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        FinalThresholdTable <-
          data.table::as.data.table(GridMetrics@metrics$max_criteria_and_metric_scores)
        data.table::setnames(
          FinalThresholdTable,
          c("metric", "threshold", "value"),
          c("Metric", "Threshold", "Value")
        )
        FinalThresholdTable[, idx := NULL]
        FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4),
                                    Value = round(Value, 4))]
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        FinalThresholdTable <-
          data.table::as.data.table(BaseMetrics@metrics$max_criteria_and_metric_scores)
        data.table::setnames(
          FinalThresholdTable,
          c("metric", "threshold", "value"),
          c("Metric", "Threshold", "Value")
        )
        FinalThresholdTable[, idx := NULL]
        FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4),
                                    Value = round(Value, 4))]
      }
    } else if (tolower(eval_metric) == "logloss") {
      BaseMetric <- BaseMetrics@metrics$logloss
      GridMetric <- GridMetrics@metrics$logloss
      if (GridMetric < BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        FinalThresholdTable <-
          data.table::as.data.table(GridMetrics@metrics$max_criteria_and_metric_scores)
        data.table::setnames(
          FinalThresholdTable,
          c("metric", "threshold", "value"),
          c("Metric", "Threshold", "Value")
        )
        FinalThresholdTable[, idx := NULL]
        FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4),
                                    Value = round(Value, 4))]
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        FinalThresholdTable <-
          data.table::as.data.table(BaseMetrics@metrics$max_criteria_and_metric_scores)
        data.table::setnames(
          FinalThresholdTable,
          c("metric", "threshold", "value"),
          c("Metric", "Threshold", "Value")
        )
        FinalThresholdTable[, idx := NULL]
        FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4),
                                    Value = round(Value, 4))]
      }
    }
  } else {
    if (tolower(eval_metric) == "auc") {
      BaseMetric <- BaseMetrics@metrics$AUC
      FinalModel <- base_model
      EvalMetric <- BaseMetric
      FinalThresholdTable <-
        data.table::as.data.table(BaseMetrics@metrics$max_criteria_and_metric_scores)
      data.table::setnames(
        FinalThresholdTable,
        c("metric", "threshold", "value"),
        c("Metric", "Threshold", "Value")
      )
      FinalThresholdTable[, idx := NULL]
      FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4),
                                  Value = round(Value, 4))]
    } else {
      BaseMetric <- BaseMetrics@metrics$logloss
      FinalModel <- base_model
      EvalMetric <- BaseMetric
      FinalThresholdTable <-
        data.table::as.data.table(BaseMetrics@metrics$max_criteria_and_metric_scores)
      data.table::setnames(
        FinalThresholdTable,
        c("metric", "threshold", "value"),
        c("Metric", "Threshold", "Value")
      )
      FinalThresholdTable[, idx := NULL]
      FinalThresholdTable[, ':=' (Threshold = round(Threshold, 4),
                                  Value = round(Value, 4))]
    }
  }
  
  # Binary Save Final Model----
  if (SaveModelObjects) {
    if (tolower(IfSaveModel) == "mojo") {
      SaveModel <- h2o::h2o.saveMojo(object = FinalModel,
                                     path = model_path,
                                     force = TRUE)
      h2o::h2o.download_mojo(
        model = FinalModel,
        path = model_path,
        get_genmodel_jar = TRUE,
        genmodel_path = model_path,
        genmodel_name = ModelID
      )
    } else {
      SaveModel <- h2o::h2o.saveModel(object = FinalModel,
                                      path = model_path,
                                      force = TRUE)
    }
  }
  
  # Binary Score Final Test Data----
  if (!is.null(TestData)) {
    Predict <-
      data.table::as.data.table(h2o::h2o.predict(object = FinalModel,
                                                 newdata = datatest))
    Predict[, p0 := NULL]
    
  } else {
    Predict <-
      data.table::as.data.table(h2o::h2o.predict(object = FinalModel,
                                                 newdata = datavalidate))
    Predict[, p0 := NULL]
  }
  
  # Binary Variable Importance----
  VariableImportance <-
    data.table::as.data.table(h2o::h2o.varimp(object = FinalModel))
  
  # Binary Format Variable Importance Table----
  data.table::setnames(
    VariableImportance,
    c(
      "variable",
      "relative_importance",
      "scaled_importance",
      "percentage"
    ),
    c(
      "Variable",
      "RelativeImportance",
      "ScaledImportance",
      "Percentage"
    )
  )
  VariableImportance[, ':=' (
    RelativeImportance = round(RelativeImportance, 4),
    ScaledImportance = round(ScaledImportance, 4),
    Percentage = round(Percentage, 4)
  )]
  
  # Binary Save Variable Importance----
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_VariableImportance.csv"))
  }
  
  # Binary H2O Shutdown----
  h2o::h2o.shutdown(prompt = FALSE)
  
  # Binary Create Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(TestData, Predict))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(dataTest, Predict))
  }
  
  # Binary Change Prediction Name----
  data.table::setnames(ValidationData, "predict", "Predict")
  
  # Binary Save Validation Data to File----
  if (SaveModelObjects) {
    data.table::fwrite(ValidationData,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_ValidationData.csv"))
  }
  
  # Binary Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "p1",
    TargetColName = Target,
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Binary Evaluation Plot Update Title----
  if (GridTune) {
    EvaluationPlot <- EvaluationPlot +
      ggplot2::ggtitle(paste0(
        "Random Forest Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(EvalMetric, 3)
      ))
  } else {
    EvaluationPlot <- EvaluationPlot +
      ggplot2::ggtitle(paste0(
        "Calibration Evaluation Plot: ",
        toupper(eval_metric),
        " = ",
        round(EvalMetric, 3)
      ))
  }
  
  # Binary Save plot to file----
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID,
                           "_EvaluationPlot.png"))
  }
  
  # Binary AUC Object Create----
  AUC_Metrics <-
    pROC::roc(
      response = ValidationData[[eval(Target)]],
      predictor = ValidationData[["p1"]],
      na.rm = TRUE,
      algorithm = 3,
      auc = TRUE,
      ci = TRUE
    )
  
  # Binary AUC Conversion to data.table----
  AUC_Data <- data.table::data.table(
    ModelNumber = 0,
    Sensitivity = AUC_Metrics$sensitivities,
    Specificity = AUC_Metrics$specificities
  )
  
  # Binary Plot ROC Curve----
  ROC_Plot <-
    ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity)) +
    ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]]), color = "blue") +
    ggplot2::geom_abline(slope = 1, color = "black") +
    ggplot2::ggtitle(paste0("RandomForest AUC: ",
                            100 * round(AUC_Metrics$auc, 3), "%")) +
    ChartTheme() + ggplot2::xlab("Specificity") +
    ggplot2::ylab("Sensitivity")
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID,
                           "_ROC_Plot.png"))
  }
  
  # Binary Save EvaluationMetrics to File----
  if (SaveModelObjects) {
    data.table::fwrite(FinalThresholdTable,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_EvaluationMetrics.csv"))
  }
  
  # Binary Partial Dependence----
  ParDepPlots <- list()
  j <- 0
  for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
    tryCatch({
      Out <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "p1",
        TargetColName = Target,
        IndepVar = VariableImportance[i, Variable],
        GraphType = "calibration",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      j <- j + 1
      ParDepPlots[[paste0(VariableImportance[j, Variable])]] <-
        Out
    }, error = function(x)
      "skip")
  }
  
  # Binary Save ParDepPlots to file----
  if (SaveModelObjects) {
    save(ParDepPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepPlots.R"))
  }
  
  # Binary Return Objects----
  if (ReturnModelObjects) {
    return(
      list(
        Model = FinalModel,
        ValidationData = ValidationData,
        ROC_Plot = ROC_Plot,
        EvaluationPlot = EvaluationPlot,
        EvaluationMetrics = FinalThresholdTable,
        VariableImportance = VariableImportance,
        PartialDependencePlots = ParDepPlots
      )
    )
  }
}

#' AutoH2oGBMMultiClass is an automated H2O modeling framework with grid-tuning and model evaluation
#'
#' AutoH2oGBMMultiClass is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, confusion matrix, and variable importance.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "logloss", "r2", "RMSE", "MSE"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxMem Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, Target :=
#' ifelse(Independent_Variable2 < 0.25, "A",
#'        ifelse(Independent_Variable2 < 0.45, "B",
#'               ifelse(Independent_Variable2 < 0.65, "C",
#'                      ifelse(Independent_Variable2 < 0.85,  "D", "E"))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' TestModel <- AutoH2oGBMMultiClass(data,
#'                                   ValidationData = NULL,
#'                                   TestData = NULL,
#'                                   TargetColumnName = "Target",
#'                                   FeatureColNames = 2:ncol(data),
#'                                   eval_metric = "logloss",
#'                                   Trees = 50,
#'                                   GridTune = FALSE,
#'                                   MaxMem = "32G",
#'                                   MaxModelsInGrid = 10,
#'                                   model_path = NULL,
#'                                   ModelID = "FirstModel",
#'                                   ReturnModelObjects = TRUE,
#'                                   SaveModelObjects = FALSE,
#'                                   IfSaveModel = "mojo")
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvaluationMetrics.csv, GridCollect, and GridList
#' @export
AutoH2oGBMMultiClass <- function(data,
                                 ValidationData = NULL,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 eval_metric = "logloss",
                                 Trees = 50,
                                 GridTune = FALSE,
                                 MaxMem = "32G",
                                 MaxModelsInGrid = 2,
                                 model_path = NULL,
                                 ModelID = "FirstModel",
                                 ReturnModelObjects = TRUE,
                                 SaveModelObjects = FALSE,
                                 IfSaveModel = "mojo") {
  # MultiClass Check Arguments----
  if (!(tolower(eval_metric) %chin% c("auc", "logloss"))) {
    warning("eval_metric not in AUC, logloss")
    
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (MaxModelsInGrid < 1 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path))
      warning("model_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  if (!(tolower(eval_metric) == "auc")) {
    eval_metric <- tolower(eval_metric)
  } else {
    eval_metric <- toupper(eval_metric)
  }
  if (tolower(eval_metric) %chin% c("auc")) {
    Decreasing <- TRUE
  } else {
    Decreasing <- FALSE
  }
  
  # MultiClass Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # MultiClass Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # MultiClass Ensure data is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # MultiClass Ensure data is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # MultiClass Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = Target,
      TimeColumnName = NULL
    )
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # MultiClass ModelDataPrep----
  dataTrain <- ModelDataPrep(data = data,
                             Impute = FALSE,
                             CharToFactor = TRUE)
  
  # MultiClass ModelDataPrep----
  dataTest <- ModelDataPrep(data = ValidationData,
                            Impute = FALSE,
                            CharToFactor = TRUE)
  
  # MultiClass ModelDataPrep----
  if (!is.null(TestData)) {
    TestData <- ModelDataPrep(data = TestData,
                              Impute = FALSE,
                              CharToFactor = TRUE)
  }
  
  # MultiClass Ensure Target Is a Factor Type----
  if (!is.factor(dataTrain[[eval(Target)]])) {
    dataTrain[, eval(Target) := as.factor(get(Target))]
  }
  
  # MultiClass Ensure Target Is a Factor Type----
  if (!is.factor(dataTest[[eval(Target)]])) {
    dataTest[, eval(Target) := as.factor(get(Target))]
  }
  
  # MultiClass Ensure Target Is a Factor Type----
  if (!is.null(TestData)) {
    if (!is.factor(TestData[[eval(Target)]])) {
      TestData[, eval(Target) := as.factor(get(Target))]
    }
  }
  
  # MultiClass Grid Tune Check----
  if (GridTune) {
    # MultiClass Start Up H2O----
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    
    # MultiClass Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
    
    # MultiClass Grid Tune Search Criteria----
    search_criteria  <- list(
      strategy             = "RandomDiscrete",
      max_runtime_secs     = 3600 * 24 * 7,
      max_models           = MaxModelsInGrid,
      seed                 = 1234,
      stopping_rounds      = 10,
      stopping_metric      = eval_metric,
      stopping_tolerance   = 1e-3
    )
    
    # MultiClass Grid Parameters----
    hyper_params <- list(
      max_depth                        = c(6, 9, 12),
      balance_classes                  = c(TRUE, FALSE),
      sample_rate                      = c(0.5, 0.75, 1.0),
      col_sample_rate_per_tree         = c(0.5, 0.75, 1.0),
      col_sample_rate_change_per_level = c(0.9, 1.0, 1.1),
      min_rows                         = c(1, 5),
      nbins                            = c(10, 20, 30),
      nbins_cats                       = c(64, 256, 512),
      histogram_type                   = c("UniformAdaptive",
                                           "QuantilesGlobal",
                                           "RoundRobin")
    )
    
    # MultiClass Grid Train Model----
    grid <- h2o::h2o.grid(
      hyper_params         = hyper_params,
      search_criteria      = search_criteria,
      is_supervised        = TRUE,
      algorithm            = "gbm",
      distribution         = "multinomial",
      grid_id              = paste0(ModelID, "_Grid"),
      x                    = FeatureColNames,
      y                    = TargetColumnName,
      ntrees               = Trees,
      training_frame       = datatrain,
      validation_frame     = datavalidate,
      max_runtime_secs     = 3600 * 24 * 7,
      stopping_rounds      = 10,
      stopping_tolerance   = 1e-3,
      stopping_metric      = eval_metric,
      score_tree_interval  = 10,
      seed                 = 1234
    )
    
    # MultiClass Get Best Model----
    Grid_Out   <- h2o::h2o.getGrid(
      grid_id = paste0(ModelID, "_Grid"),
      sort_by = eval_metric,
      decreasing = Decreasing
    )
    
    # MultiClass Collect Best Grid Model----
    grid_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1]])
  }
  
  # MultiClass Start Up H2O----
  if (!GridTune) {
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    
    # MultiClass Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
  }
  
  # MultiClass Build Baseline Model----
  base_model <- h2o::h2o.gbm(
    x                = FeatureColNames,
    y                = TargetColumnName,
    distribution     = "multinomial",
    training_frame   = datatrain,
    validation_frame = datavalidate,
    model_id         = ModelID,
    ntrees           = Trees
  )
  
  # MultiClass Get Metrics----
  if (GridTune) {
    if (!is.null(TestData)) {
      datatest        <-  h2o::as.h2o(TestData)
      GridMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datatest)
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datatest)
    } else {
      GridMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datavalidate)
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datavalidate)
    }
  } else {
    if (!is.null(TestData)) {
      datatest    <- h2o::as.h2o(TestData)
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datatest)
    } else {
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datavalidate)
    }
  }
  
  # MultiClass Evaluate Metrics----
  if (GridTune) {
    if (tolower(eval_metric) == "logloss") {
      BaseMetric <- BaseMetrics@metrics$logloss
      GridMetric <- GridMetrics@metrics$logloss
      if (GridMetric < BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        ConfusionMatrix <-
          data.table::as.data.table(GridMetrics@metrics$cm$table)
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetrics@metrics$logloss
        ConfusionMatrix <-
          data.table::as.data.table(BaseMetrics@metrics$cm$table)
      }
    } else if (tolower(eval_metric) == "r2") {
      BaseMetric <- BaseMetrics@metrics$r2
      GridMetric <- GridMetrics@metrics$r2
      if (GridMetric > BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        ConfusionMatrix <-
          data.table::as.data.table(GridMetrics@metrics$cm$table)
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        ConfusionMatrix <-
          data.table::as.data.table(BaseMetrics@metrics$cm)
      }
    } else if (tolower(eval_metric) == "rmse") {
      BaseMetric <- BaseMetrics@metrics$logloss
      GridMetric <- GridMetrics@metrics$logloss
      if (GridMetric < BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        ConfusionMatrix <-
          data.table::as.data.table(GridMetrics@metrics$cm$table)
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        ConfusionMatrix <-
          data.table::as.data.table(BaseMetrics@metrics$cm)
      }
    } else if (tolower(eval_metric) == "mse") {
      BaseMetric <- BaseMetrics@metrics$logloss
      GridMetric <- GridMetrics@metrics$logloss
      if (GridMetric < BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        ConfusionMatrix <-
          data.table::as.data.table(GridMetrics@metrics$cm$table)
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        ConfusionMatrix <-
          data.table::as.data.table(BaseMetrics@metrics$cm$table)
      }
    }
  } else {
    if (tolower(eval_metric) == "logloss") {
      FinalModel <- base_model
      EvalMetric <- BaseMetrics@metrics$logloss
      ConfusionMatrix <-
        data.table::as.data.table(BaseMetrics@metrics$cm$table)
    } else if (tolower(eval_metric) == "r2") {
      FinalModel <- base_model
      EvalMetric <- BaseMetrics@metrics$r2
      ConfusionMatrix <-
        data.table::as.data.table(BaseMetrics@metrics$cm$table)
    } else if (tolower(eval_metric) == "rmse") {
      FinalModel <- base_model
      EvalMetric <- BaseMetrics@metrics$RMSE
      ConfusionMatrix <-
        data.table::as.data.table(BaseMetrics@metrics$cm$table)
    } else if (tolower(eval_metric) == "mse") {
      FinalModel <- base_model
      EvalMetric <- BaseMetrics@metrics$MSE
      ConfusionMatrix <-
        data.table::as.data.table(BaseMetrics@metrics$cm$table)
    }
  }
  
  # MultiClass Save Final Model----
  if (SaveModelObjects) {
    if (tolower(IfSaveModel) == "mojo") {
      SaveModel <- h2o::h2o.saveMojo(object = FinalModel,
                                     path = model_path,
                                     force = TRUE)
      h2o::h2o.download_mojo(
        model = FinalModel,
        path = model_path,
        get_genmodel_jar = TRUE,
        genmodel_path = model_path,
        genmodel_name = ModelID
      )
    } else {
      SaveModel <- h2o::h2o.saveModel(object = FinalModel,
                                      path = model_path,
                                      force = TRUE)
    }
  }
  
  # MultiClass Score Final Test Data----
  if (!is.null(TestData)) {
    Predict <-
      data.table::as.data.table(h2o::h2o.predict(object = FinalModel,
                                                 newdata = datatest))
  } else {
    Predict <-
      data.table::as.data.table(h2o::h2o.predict(object = FinalModel,
                                                 newdata = datavalidate))
  }
  
  # MultiClass Variable Importance----
  VariableImportance <-
    data.table::as.data.table(h2o::h2o.varimp(object = FinalModel))
  
  # MultiClass Format Variable Importance Table----
  data.table::setnames(
    VariableImportance,
    c(
      "variable",
      "relative_importance",
      "scaled_importance",
      "percentage"
    ),
    c(
      "Variable",
      "RelativeImportance",
      "ScaledImportance",
      "Percentage"
    )
  )
  VariableImportance[, ':=' (
    RelativeImportance = round(RelativeImportance, 4),
    ScaledImportance = round(ScaledImportance, 4),
    Percentage = round(Percentage, 4)
  )]
  
  # MultiClass Save Variable Importance----
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_VariableImportance.csv"))
  }
  
  # MultiClass H2O Shutdown----
  h2o::h2o.shutdown(prompt = FALSE)
  
  # MultiClass Create Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(TestData, Predict))
    data.table::setnames(ValidationData, "predict", "Predict", skip_absent = TRUE)
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(dataTest, Predict))
    data.table::setnames(ValidationData, "predict", "Predict", skip_absent = TRUE)
  }
  
  # MultiClass Metrics Accuracy----
  MetricAcc <-
    ValidationData[, mean(ifelse(as.character(eval(Target)) ==
                                   as.character(Predict),
                                 1.0,
                                 0.0),
                          na.rm = TRUE)]
  
  # MultiClass Metrics MicroAUC----
  y <- ValidationData[[eval(Target)]]
  keep <-
    names(ValidationData)[(ncol(data) + 2):(ncol(ValidationData))]
  x <- as.matrix(ValidationData[, ..keep])
  z <-
    tryCatch({
      pROC::multiclass.roc(response = y, predictor = x)
    },
    error = function(x)
      0)
  MetricAUC <- round(as.numeric(noquote(
    stringr::str_extract(z$auc, "\\d+\\.*\\d*")
  )), 4)
  
  # MultiClass Evaluation Metrics Table----
  EvaluationMetrics <- data.table::data.table(
    Metric = c("Accuracy", "MicroAUC", "temp"),
    Value = c(
      round(MetricAcc, 4),
      round(MetricAUC, 4),
      round(EvalMetric, 4)
    )
  )
  data.table::set(
    EvaluationMetrics,
    i = 3L,
    j = 1L,
    value = paste0(eval_metric)
  )
  
  # MultiClass Save Validation Data to File----
  if (SaveModelObjects) {
    data.table::fwrite(ValidationData,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_ValidationData.csv"))
  }
  
  # MultiClass Save ConfusionMatrix to File----
  if (SaveModelObjects) {
    data.table::fwrite(ConfusionMatrix,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_EvaluationMetrics.csv"))
  }
  
  # MultiClass Return Objects----
  if (ReturnModelObjects) {
    return(
      list(
        Model = FinalModel,
        ValidationData = ValidationData,
        ConfusionMatrix = ConfusionMatrix,
        EvaluationMetrics = EvaluationMetrics,
        VariableImportance = VariableImportance
      )
    )
  }
}

#' AutoH2oDRFMultiClass is an automated H2O modeling framework with grid-tuning and model evaluation
#'
#' AutoH2oDRFMultiClass is an automated H2O modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, confusion matrix, and variable importance.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "logloss", "r2", "RMSE", "MSE"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param MaxMem Set the maximum amount of memory you'd like to dedicate to the model run. E.g. "32G"
#' @param MaxModelsInGrid Number of models to test from grid options (1080 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param IfSaveModel Set to "mojo" to save a mojo file, otherwise "standard" to save a regular H2O model object
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 1000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, Target :=
#' ifelse(Independent_Variable2 < 0.25, "A",
#'        ifelse(Independent_Variable2 < 0.45, "B",
#'               ifelse(Independent_Variable2 < 0.65, "C",
#'                      ifelse(Independent_Variable2 < 0.85,  "D", "E"))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' TestModel <- AutoH2oDRFMultiClass(data,
#'                                   ValidationData = NULL,
#'                                   TestData = NULL,
#'                                   TargetColumnName = "Target",
#'                                   FeatureColNames = 2:ncol(data),
#'                                   eval_metric = "logloss",
#'                                   Trees = 50,
#'                                   GridTune = FALSE,
#'                                   MaxMem = "32G",
#'                                   MaxModelsInGrid = 10,
#'                                   model_path = NULL,
#'                                   ModelID = "FirstModel",
#'                                   ReturnModelObjects = TRUE,
#'                                   SaveModelObjects = FALSE,
#'                                   IfSaveModel = "mojo")
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvaluationMetrics.csv, GridCollect, and GridList
#' @export
AutoH2oDRFMultiClass <- function(data,
                                 ValidationData = NULL,
                                 TestData = NULL,
                                 TargetColumnName = NULL,
                                 FeatureColNames = NULL,
                                 eval_metric = "logloss",
                                 Trees = 50,
                                 GridTune = FALSE,
                                 MaxMem = "32G",
                                 MaxModelsInGrid = 2,
                                 model_path = NULL,
                                 ModelID = "FirstModel",
                                 ReturnModelObjects = TRUE,
                                 SaveModelObjects = FALSE,
                                 IfSaveModel = "mojo") {
  # MultiClass Check Arguments----
  if (!(tolower(eval_metric) %chin% c("auc", "logloss"))) {
    warning("eval_metric not in AUC, logloss")
    
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (MaxModelsInGrid < 1 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path))
      warning("model_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  if (!(tolower(eval_metric) == "auc")) {
    eval_metric <- tolower(eval_metric)
  } else {
    eval_metric <- toupper(eval_metric)
  }
  if (tolower(eval_metric) %chin% c("auc")) {
    Decreasing <- TRUE
  } else {
    Decreasing <- FALSE
  }
  
  # MultiClass Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # MultiClass Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # MultiClass Ensure data is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # MultiClass Ensure data is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # MultiClass Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = Target,
      TimeColumnName = NULL
    )
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # MultiClass ModelDataPrep----
  dataTrain <- ModelDataPrep(data = data,
                             Impute = FALSE,
                             CharToFactor = TRUE)
  
  # MultiClass ModelDataPrep----
  dataTest <- ModelDataPrep(data = ValidationData,
                            Impute = FALSE,
                            CharToFactor = TRUE)
  
  # MultiClass ModelDataPrep----
  if (!is.null(TestData)) {
    TestData <- ModelDataPrep(data = TestData,
                              Impute = FALSE,
                              CharToFactor = TRUE)
  }
  
  # MultiClass Ensure Target Is a Factor Type----
  if (!is.factor(dataTrain[[eval(Target)]])) {
    dataTrain[, eval(Target) := as.factor(get(Target))]
  }
  
  # MultiClass Ensure Target Is a Factor Type----
  if (!is.factor(dataTest[[eval(Target)]])) {
    dataTest[, eval(Target) := as.factor(get(Target))]
  }
  
  # MultiClass Ensure Target Is a Factor Type----
  if (!is.null(TestData)) {
    if (!is.factor(TestData[[eval(Target)]])) {
      TestData[, eval(Target) := as.factor(get(Target))]
    }
  }
  
  # MultiClass Grid Tune Check----
  if (GridTune) {
    # MultiClass Start Up H2O----
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    
    # MultiClass Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
    
    # MultiClass Grid Tune Search Criteria----
    search_criteria  <- list(
      strategy             = "RandomDiscrete",
      max_runtime_secs     = 3600 * 24 * 7,
      max_models           = MaxModelsInGrid,
      seed                 = 1234,
      stopping_rounds      = 10,
      stopping_metric      = eval_metric,
      stopping_tolerance   = 1e-3
    )
    
    # MultiClass Grid Parameters----
    hyper_params <- list(
      max_depth                        = c(6, 9, 12),
      balance_classes                  = c(TRUE, FALSE),
      sample_rate                      = c(0.5, 0.75, 1.0),
      col_sample_rate_per_tree         = c(0.5, 0.75, 1.0),
      col_sample_rate_change_per_level = c(0.9, 1.0, 1.1),
      min_rows                         = c(1, 5),
      nbins                            = c(10, 20, 30),
      nbins_cats                       = c(64, 256, 512),
      histogram_type                   = c("UniformAdaptive",
                                           "QuantilesGlobal",
                                           "RoundRobin")
    )
    
    # MultiClass Grid Train Model----
    grid <- h2o::h2o.grid(
      hyper_params         = hyper_params,
      search_criteria      = search_criteria,
      is_supervised        = TRUE,
      algorithm            = "randomForest",
      grid_id              = paste0(ModelID, "_Grid"),
      x                    = FeatureColNames,
      y                    = TargetColumnName,
      ntrees               = Trees,
      training_frame       = datatrain,
      validation_frame     = datavalidate,
      max_runtime_secs     = 3600 * 24 * 7,
      stopping_rounds      = 10,
      stopping_tolerance   = 1e-3,
      stopping_metric      = eval_metric,
      score_tree_interval  = 10,
      seed                 = 1234
    )
    
    # MultiClass Get Best Model----
    Grid_Out   <- h2o::h2o.getGrid(
      grid_id = paste0(ModelID, "_Grid"),
      sort_by = eval_metric,
      decreasing = Decreasing
    )
    
    # MultiClass Collect Best Grid Model----
    grid_model <- h2o::h2o.getModel(Grid_Out@model_ids[[1]])
  }
  
  # MultiClass Start Up H2O----
  if (!GridTune) {
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    
    # MultiClass Define data sets----
    datatrain    <- h2o::as.h2o(dataTrain)
    datavalidate <- h2o::as.h2o(dataTest)
  }
  
  # MultiClass Build Baseline Model----
  base_model <- h2o::h2o.randomForest(
    x                = FeatureColNames,
    y                = TargetColumnName,
    training_frame   = datatrain,
    validation_frame = datavalidate,
    model_id         = ModelID,
    ntrees           = Trees
  )
  
  # MultiClass Get Metrics----
  if (GridTune) {
    if (!is.null(TestData)) {
      datatest        <-  h2o::as.h2o(TestData)
      GridMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datatest)
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datatest)
    } else {
      GridMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datavalidate)
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datavalidate)
    }
  } else {
    if (!is.null(TestData)) {
      datatest    <- h2o::as.h2o(TestData)
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datatest)
    } else {
      BaseMetrics <- h2o::h2o.performance(model = base_model,
                                          newdata = datavalidate)
    }
  }
  
  # MultiClass Evaluate Metrics----
  if (GridTune) {
    if (tolower(eval_metric) == "logloss") {
      BaseMetric <- BaseMetrics@metrics$logloss
      GridMetric <- GridMetrics@metrics$logloss
      if (GridMetric < BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        ConfusionMatrix <-
          data.table::as.data.table(GridMetrics@metrics$cm$table)
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetrics@metrics$logloss
        ConfusionMatrix <-
          data.table::as.data.table(BaseMetrics@metrics$cm$table)
      }
    } else if (tolower(eval_metric) == "r2") {
      BaseMetric <- BaseMetrics@metrics$r2
      GridMetric <- GridMetrics@metrics$r2
      if (GridMetric > BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        ConfusionMatrix <-
          data.table::as.data.table(GridMetrics@metrics$cm$table)
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        ConfusionMatrix <-
          data.table::as.data.table(BaseMetrics@metrics$cm)
      }
    } else if (tolower(eval_metric) == "rmse") {
      BaseMetric <- BaseMetrics@metrics$logloss
      GridMetric <- GridMetrics@metrics$logloss
      if (GridMetric < BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        ConfusionMatrix <-
          data.table::as.data.table(GridMetrics@metrics$cm$table)
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        ConfusionMatrix <-
          data.table::as.data.table(BaseMetrics@metrics$cm)
      }
    } else if (tolower(eval_metric) == "mse") {
      BaseMetric <- BaseMetrics@metrics$logloss
      GridMetric <- GridMetrics@metrics$logloss
      if (GridMetric < BaseMetric) {
        FinalModel <- grid_model
        EvalMetric <- GridMetric
        ConfusionMatrix <-
          data.table::as.data.table(GridMetrics@metrics$cm$table)
      } else {
        FinalModel <- base_model
        EvalMetric <- BaseMetric
        ConfusionMatrix <-
          data.table::as.data.table(BaseMetrics@metrics$cm$table)
      }
    }
  } else {
    if (tolower(eval_metric) == "logloss") {
      FinalModel <- base_model
      EvalMetric <- BaseMetrics@metrics$logloss
      ConfusionMatrix <-
        data.table::as.data.table(BaseMetrics@metrics$cm$table)
    } else if (tolower(eval_metric) == "r2") {
      FinalModel <- base_model
      EvalMetric <- BaseMetrics@metrics$r2
      ConfusionMatrix <-
        data.table::as.data.table(BaseMetrics@metrics$cm$table)
    } else if (tolower(eval_metric) == "rmse") {
      FinalModel <- base_model
      EvalMetric <- BaseMetrics@metrics$RMSE
      ConfusionMatrix <-
        data.table::as.data.table(BaseMetrics@metrics$cm$table)
    } else if (tolower(eval_metric) == "mse") {
      FinalModel <- base_model
      EvalMetric <- BaseMetrics@metrics$MSE
      ConfusionMatrix <-
        data.table::as.data.table(BaseMetrics@metrics$cm$table)
    }
  }
  
  # MultiClass Save Final Model----
  if (SaveModelObjects) {
    if (tolower(IfSaveModel) == "mojo") {
      SaveModel <- h2o::h2o.saveMojo(object = FinalModel,
                                     path = model_path,
                                     force = TRUE)
      h2o::h2o.download_mojo(
        model = FinalModel,
        path = model_path,
        get_genmodel_jar = TRUE,
        genmodel_path = model_path,
        genmodel_name = ModelID
      )
    } else {
      SaveModel <- h2o::h2o.saveModel(object = FinalModel,
                                      path = model_path,
                                      force = TRUE)
    }
  }
  
  # MultiClass Score Final Test Data----
  if (!is.null(TestData)) {
    Predict <-
      data.table::as.data.table(h2o::h2o.predict(object = FinalModel,
                                                 newdata = datatest))
  } else {
    Predict <-
      data.table::as.data.table(h2o::h2o.predict(object = FinalModel,
                                                 newdata = datavalidate))
  }
  
  # MultiClass Variable Importance----
  VariableImportance <-
    data.table::as.data.table(h2o::h2o.varimp(object = FinalModel))
  
  # MultiClass Format Variable Importance Table----
  data.table::setnames(
    VariableImportance,
    c(
      "variable",
      "relative_importance",
      "scaled_importance",
      "percentage"
    ),
    c(
      "Variable",
      "RelativeImportance",
      "ScaledImportance",
      "Percentage"
    )
  )
  VariableImportance[, ':=' (
    RelativeImportance = round(RelativeImportance, 4),
    ScaledImportance = round(ScaledImportance, 4),
    Percentage = round(Percentage, 4)
  )]
  
  # MultiClass Save Variable Importance----
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_VariableImportance.csv"))
  }
  
  # MultiClass H2O Shutdown----
  h2o::h2o.shutdown(prompt = FALSE)
  
  # MultiClass Create Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(TestData, Predict))
    data.table::setnames(ValidationData, "predict", "Predict", skip_absent = TRUE)
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(dataTest, Predict))
    data.table::setnames(ValidationData, "predict", "Predict", skip_absent = TRUE)
  }
  
  # MultiClass Metrics Accuracy----
  MetricAcc <-
    ValidationData[, mean(ifelse(as.character(eval(Target)) ==
                                   as.character(Predict),
                                 1.0,
                                 0.0),
                          na.rm = TRUE)]
  
  # MultiClass Metrics MicroAUC----
  y <- ValidationData[[eval(Target)]]
  keep <-
    names(ValidationData)[(ncol(data) + 2):(ncol(ValidationData))]
  x <- as.matrix(ValidationData[, ..keep])
  z <-
    tryCatch({
      pROC::multiclass.roc(response = y, predictor = x)
    },
    error = function(x)
      0)
  MetricAUC <- round(as.numeric(noquote(
    stringr::str_extract(z$auc, "\\d+\\.*\\d*")
  )), 4)
  
  # MultiClass Evaluation Metrics Table----
  EvaluationMetrics <- data.table::data.table(
    Metric = c("Accuracy", "MicroAUC", "temp"),
    Value = c(
      round(MetricAcc, 4),
      round(MetricAUC, 4),
      round(EvalMetric, 4)
    )
  )
  data.table::set(
    EvaluationMetrics,
    i = 3L,
    j = 1L,
    value = paste0(eval_metric)
  )
  
  # MultiClass Save Validation Data to File----
  if (SaveModelObjects) {
    data.table::fwrite(ValidationData,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_ValidationData.csv"))
  }
  
  # MultiClass Save ConfusionMatrix to File----
  if (SaveModelObjects) {
    data.table::fwrite(ConfusionMatrix,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_EvaluationMetrics.csv"))
  }
  
  # MultiClass Return Objects----
  if (ReturnModelObjects) {
    return(
      list(
        Model = FinalModel,
        ValidationData = ValidationData,
        ConfusionMatrix = ConfusionMatrix,
        EvaluationMetrics = EvaluationMetrics,
        VariableImportance = VariableImportance
      )
    )
  }
}

#' AutoXGBoostRegression is an automated XGBoost modeling framework with grid-tuning and model evaluation
#'
#' AutoXGBoostRegression is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types).
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "r2", "RMSE", "MSE", "MAE"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param grid_eval_metric Choose from "poisson","mae","mape","mse","msle","kl","cs","r2"
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param MaxModelsInGrid Number of models to test from grid options (243 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param Verbose Set to 0 if you want to suppress model evaluation updates in training
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param PassInGrid Default is NULL. Provide a data.table of grid options from a previous run.
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 10000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' TestModel <- AutoXGBoostRegression(data,
#'                                    ValidationData = NULL,
#'                                    TestData = NULL,
#'                                    TargetColumnName = 1,
#'                                    FeatureColNames = 2:12,
#'                                    IDcols = NULL,
#'                                    eval_metric = "RMSE",
#'                                    Trees = 50,
#'                                    GridTune = TRUE,
#'                                    grid_eval_metric = "mae",
#'                                    MaxModelsInGrid = 10,
#'                                    NThreads = 8,
#'                                    TreeMethod = "hist",
#'                                    model_path = getwd(),
#'                                    ModelID = "FirstModel",
#'                                    NumOfParDepPlots = 3,
#'                                    ReturnModelObjects = TRUE,
#'                                    SaveModelObjects = FALSE,
#'                                    PassInGrid = NULL)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and GridList
#' @export
# Train code
AutoXGBoostRegression <- function(data,
                                  ValidationData = NULL,
                                  TestData = NULL,
                                  TargetColumnName = NULL,
                                  FeatureColNames = NULL,
                                  IDcols = NULL,
                                  eval_metric = "RMSE",
                                  Trees = 50,
                                  GridTune = FALSE,
                                  grid_eval_metric = "mae",
                                  TreeMethod = "hist",
                                  MaxModelsInGrid = 10,
                                  NThreads = 8,
                                  model_path = NULL,
                                  ModelID = "FirstModel",
                                  NumOfParDepPlots = 3,
                                  Verbose = 0,
                                  ReturnModelObjects = TRUE,
                                  SaveModelObjects = FALSE,
                                  PassInGrid = NULL) {
  # Regression Check Arguments----
  if (!(tolower(eval_metric) %chin% c("rmse", "mae", "mape", "r2"))) {
    warning("eval_metric not in RMSE, MAE, MAPE, R2")
    
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (MaxModelsInGrid < 1 |
      MaxModelsInGrid > 1080 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1 and less than 1080")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path))
      warning("model_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (NumOfParDepPlots < 0)
    warning("NumOfParDepPlots needs to be a positive number")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  
  # Regression Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Regression Ensure data is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # Regression Ensure TestData is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # Regression Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # Regression IDcol Name Storage----
  if (!is.null(IDcols)) {
    if (!is.character(IDcols)) {
      IDcols <- names(data)[IDcols]
    }
  }
  
  # Regression Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(
    sapply(data, is.factor)
  )),
  as.numeric(which(
    sapply(data, is.character)
  ))))
  CatFeatures <- names(data)[CatFeatures]
  
  # Regression Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = NULL,
      TimeColumnName = NULL
    )
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # Regression data Subset Columns Needed----
  if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    keep1 <- names(data)[c(FeatureColNames)]
    keep <- c(keep1, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  } else {
    keep <- c(FeatureColNames, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  }
  
  # Regression TestData Subset Columns Needed----
  if (!is.null(TestData)) {
    if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
      keep1 <- names(TestData)[c(FeatureColNames)]
      if (!is.null(IDcols)) {
        keep <- c(IDcols, keep1, Target)
      } else {
        keep <- c(keep1, Target)
      }
      TestData <- TestData[, ..keep]
    } else {
      keep1 <- c(FeatureColNames)
      if (!is.null(IDcols)) {
        keep <- c(IDcols, FeatureColNames, Target)
      } else {
        keep <- c(FeatureColNames, Target)
      }
      TestData <- TestData[, ..keep]
    }
    if (!is.null(IDcols)) {
      TestMerge <- data.table::copy(TestData)
      keep <- c(keep1, Target)
      TestData <- TestData[, ..keep]
    } else {
      TestMerge <- data.table::copy(TestData)
    }
  }
  
  # Regression Dummify dataTrain Categorical Features----
  if (SaveModelObjects) {
    if (!is.null(dataTest) & !is.null(TestData)) {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "VALIDATE")
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = "TEST")
      temp <-
        data.table::rbindlist(list(dataTrain, dataTest, TestData))
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = TRUE,
        SavePath = model_path,
        ImportFactorLevels = FALSE
      )
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
      TestData <- temp[ID_Factorizer == "TEST"]
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = NULL)
    } else {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      temp <- data.table::rbindlist(list(dataTrain, dataTest))
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = TRUE,
        SavePath = model_path,
        ImportFactorLevels = FALSE
      )
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
    }
  } else {
    if (!is.null(dataTest) & !is.null(TestData)) {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "VALIDATE")
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = "TEST")
      temp <-
        data.table::rbindlist(list(dataTrain, dataTest, TestData))
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = FALSE,
        SavePath = NULL,
        ImportFactorLevels = FALSE
      )
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
      TestData <- temp[ID_Factorizer == "TEST"]
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = NULL)
      
    } else {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      temp <- data.table::rbindlist(list(dataTrain, dataTest))
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = FALSE,
        SavePath = NULL,
        ImportFactorLevels = FALSE
      )
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
    }
  }
  
  # Regression Save Names of data----
  Names <- data.table::as.data.table(names(data))
  data.table::setnames(Names, "V1", "ColNames")
  if (SaveModelObjects) {
    data.table::fwrite(Names, paste0(model_path,
                                     "/"
                                     , ModelID, "_ColNames.csv"))
  }
  
  # Regression Subset Target Variables----
  TrainTarget <-
    tryCatch({
      dataTrain[, get(Target)]
    }, error = function(x)
      dataTrain[, eval(Target)])
  TestTarget <-
    tryCatch({
      dataTest[, get(Target)]
    }, error = function(x)
      dataTest[, eval(Target)])
  if (!is.null(TestData)) {
    FinalTestTarget <-
      tryCatch({
        TestData[, get(Target)]
      }, error = function(x)
        TestData[, eval(Target)])
  }
  
  # Regression Remove Target Variable from Feature Data
  dataTrain[, eval(Target) := NULL]
  dataTest[, eval(Target) := NULL]
  if (!is.null(TestData)) {
    TestData[, eval(Target) := NULL]
  }
  
  # Regression Initialize Catboost Data Conversion----
  datatrain <-
    xgboost::xgb.DMatrix(as.matrix(dataTrain), label = TrainTarget)
  datavalidate <-
    xgboost::xgb.DMatrix(as.matrix(dataTest), label = TestTarget)
  if (!is.null(TestData)) {
    datatest <-
      xgboost::xgb.DMatrix(as.matrix(TestData), label = FinalTestTarget)
    EvalSets <- list(train = datavalidate, test = datatest)
  } else {
    EvalSets <- list(train = datatrain, test = datavalidate)
  }
  
  # Regression Grid Tune or Not Check----
  if (GridTune) {
    # Regression Grid Create data.table To Store Results----
    GridCollect <-
      data.table::data.table(
        ParamRow = 1:(MaxModelsInGrid + 1),
        EvalStat = rep(9999999, MaxModelsInGrid + 1)
      )
    
    # Regression Grid Define Hyper Parameters----
    if (!is.null(PassInGrid)) {
      if (!data.table::is.data.table(PassInGrid)) {
        PassInGrid <- data.table::as.data.table(PassInGrid)
      }
      grid_params <- data.table::CJ(
        eta = c(0.30, 0.25, 0.35),
        max_depth = c(6, 8, 10),
        min_child_weight = c(1, 2, 3),
        subsample = c(1, 0.90, 0.80),
        colsample_bytree = c(1, 0.90, 0.80)
      )
      grid_params[, ID := runif(nrow(grid_params))]
      grid_params <-
        grid_params[order(ID)][1:(MaxModelsInGrid)][, ID := NULL]
      grid_params <-
        data.table::rbindlist(list(PassInGrid, grid_params))
    } else {
      grid_params <- data.table::CJ(
        eta = c(0.30, 0.25, 0.35),
        max_depth = c(6, 8, 10),
        min_child_weight = c(1, 2, 3),
        subsample = c(1, 0.90, 0.80),
        colsample_bytree = c(1, 0.90, 0.80)
      )
      grid_params[, ID := runif(nrow(grid_params))]
      grid_params <-
        grid_params[order(ID)][1:(MaxModelsInGrid + 1)][, ID := NULL]
    }
    
    # Regression Grid Tuning Main Loop----
    for (i in as.integer(seq_len(MaxModelsInGrid + 1))) {
      # Print i
      print(i)
      
      # Regression Grid Define Base Parameters----
      if (i == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:linear',
          eval_metric = tolower(eval_metric),
          eta = 0.30,
          max_depth = 6,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
      } else {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:linear',
          eval_metric = tolower(eval_metric),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
      }
      
      # Regression Grid Merge Model Parameters----
      # Have first model be the baseline model
      if (i != 1) {
        base_params <- c(as.list(grid_params[i,]), base_params)
      }
      
      # Regression Grid Train Model----
      if (Verbose == 0) {
        model <- xgboost::xgb.train(
          params = base_params,
          data = datatrain,
          watchlist = EvalSets,
          nrounds = Trees,
          verbose = Verbose,
          early_stopping_rounds = 10
        )
      } else {
        model <- xgboost::xgb.train(
          params = base_params,
          data = datatrain,
          watchlist = EvalSets,
          nrounds = Trees,
          early_stopping_rounds = 10
        )
      }
      
      
      # Regression Grid Score Model----
      if (!is.null(TestData)) {
        predict <- stats::predict(model, datatest)
      } else {
        predict <- stats::predict(model, datavalidate)
      }
      
      # Regression Grid Validation Data----
      if (!is.null(TestData)) {
        calibEval <-
          data.table::as.data.table(cbind(Target = FinalTestTarget, Predicted = predict))
      } else {
        calibEval <-
          data.table::as.data.table(cbind(Target = TestTarget, Predicted = predict))
      }
      
      # Regression Grid Evaluation Metrics----
      if (tolower(grid_eval_metric) == "poisson") {
        if (MinVal > 0 & min(calibEval[["Predicted"]], na.rm = TRUE) > 0) {
          calibEval[, Metric := Predicted - Target * log(Predicted + 1)]
          Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(grid_eval_metric) == "mae") {
        calibEval[, Metric := abs(Target - Predicted)]
        Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(grid_eval_metric) == "mape") {
        calibEval[, Metric := abs((Target - Predicted) / (Target + 1))]
        Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(grid_eval_metric) == "mse") {
        calibEval[, Metric := (Target - Predicted) ^ 2]
        Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(grid_eval_metric) == "msle") {
        if (MinVal > 0 & min(calibEval[["Predicted"]], na.rm = TRUE) > 0) {
          calibEval[, Metric := (log(Target + 1) - log(Predicted + 1)) ^ 2]
          Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(grid_eval_metric) == "kl") {
        if (MinVal > 0 & min(calibEval[["Predicted"]], na.rm = TRUE) > 0) {
          calibEval[, Metric := Target * log((Target + 1) / (Predicted + 1))]
          Metric <- calibEval[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(grid_eval_metric) == "cs") {
        calibEval[, ':=' (
          Metric1 = Target * Predicted,
          Metric2 = Target ^ 2,
          Metric3 = Predicted ^ 2
        )]
        Metric <-
          calibEval[, sum(Metric1, na.rm = TRUE)] / (sqrt(calibEval[, sum(Metric2, na.rm = TRUE)]) *
                                                       sqrt(calibEval[, sum(Metric3, na.rm = TRUE)]))
      } else if (tolower(grid_eval_metric) == "r2") {
        Metric <-
          (calibEval[, stats::cor(eval(Target), Predicted)][[1]]) ^ 2
      }
      
      # Regression Metrics Collection----
      data.table::set(GridCollect,
                      i = i,
                      j = 1L,
                      value = i)
      data.table::set(
        GridCollect,
        i = i,
        j = 2L,
        value = round(Metric, 4)
      )
    }
  }
  
  # Regression Define Final Model Parameters----
  if (GridTune) {
    if (grid_eval_metric %chin% c("kl", "cs", "r2")) {
      BestGrid <- GridCollect[order(-EvalStat)][1, ParamRow]
      if (BestGrid == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:linear',
          eval_metric = tolower(eval_metric),
          eta = 0.30,
          max_depth = 6,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        
      } else {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:linear',
          eval_metric = tolower(eval_metric),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        base_params <-
          c(as.list(grid_params[BestGrid,]), base_params)
      }
    } else {
      BestGrid <- GridCollect[order(EvalStat)][1, ParamRow]
      if (BestGrid == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:linear',
          eval_metric = tolower(eval_metric),
          eta = 0.30,
          max_depth = 6,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        
      } else {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:linear',
          eval_metric = tolower(eval_metric),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        base_params <-
          c(as.list(grid_params[BestGrid,]), base_params)
      }
    }
  } else {
    base_params <- list(
      booster = "gbtree",
      objective = 'reg:linear',
      eval_metric = tolower(eval_metric),
      nthread = NThreads,
      max_bin = 64,
      tree_method = TreeMethod
    )
    if (!is.null(PassInGrid)) {
      base_params <- c(base_params, as.list(PassInGrid[1, ]))
    }
  }
  
  # Regression Train Final Model----
  if (Verbose == 0) {
    model <- xgboost::xgb.train(
      params = base_params,
      data = datatrain,
      watchlist = EvalSets,
      nrounds = Trees,
      verbose = Verbose,
      early_stopping_rounds = 10
    )
  } else {
    model <- xgboost::xgb.train(
      params = base_params,
      data = datatrain,
      watchlist = EvalSets,
      nrounds = Trees,
      early_stopping_rounds = 10
    )
  }
  
  # Regression Save Model----
  if (SaveModelObjects) {
    xgboost::xgb.save(model = model, fname = ModelID)
  }
  
  # Regression Grid Score Model----
  if (!is.null(TestData)) {
    predict <- stats::predict(model, datatest)
  } else {
    predict <- stats::predict(model, datavalidate)
  }
  
  # Regression Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, Predict = predict))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(Target = TestTarget, dataTest, Predict = predict))
  }
  
  # Regression r2 via sqrt of correlation
  r_squared <- (ValidationData[, stats::cor(Target, Predict)]) ^ 2
  
  # Save Validation Data to File----
  if (SaveModelObjects) {
    data.table::fwrite(ValidationData,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_ValidationData.csv"))
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "Predict",
    TargetColName = "Target",
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Add Number of Trees to Title
  EvaluationPlot <- EvaluationPlot +
    ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ",
                            round(r_squared, 3)))
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID, "_EvaluationPlot.png"))
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationBoxPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "Predict",
    TargetColName = "Target",
    GraphType = "boxplot",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Add Number of Trees to Title
  EvaluationBoxPlot <- EvaluationBoxPlot +
    ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ",
                            round(r_squared, 3)))
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path,
                           "/",
                           ModelID,
                           "_EvaluationBoxPlot.png"))
  }
  
  # Regression Evaluation Metrics----
  EvaluationMetrics <-
    data.table::data.table(
      Metric = c("Poisson", "MAE",
                 "MAPE", "MSE", "MSLE",
                 "KL", "CS", "R2"),
      MetricValue = rep(999999, 8)
    )
  i <- 0
  for (metric in c("poisson", "mae", "mape", "mse", "msle", "kl", "cs", "r2")) {
    i <- as.integer(i + 1)
    tryCatch({
      # Regression Grid Evaluation Metrics----
      if (tolower(metric) == "poisson") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := Predict - Target * log(Predict + 1)]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "mae") {
        ValidationData[, Metric := abs(Target - Predict)]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mape") {
        ValidationData[, Metric := abs((Target - Predict) / (Target + 1))]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mse") {
        ValidationData[, Metric := (Target - Predict) ^ 2]
        Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "msle") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := (log(Target + 1) - log(Predict + 1)) ^ 2]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "kl") {
        if (MinVal > 0 &
            min(ValidationData[["Predict"]], na.rm = TRUE) > 0) {
          ValidationData[, Metric := Target * log((Target + 1) /
                                                    (Predict + 1))]
          Metric <- ValidationData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "cs") {
        ValidationData[, ':=' (
          Metric1 = Target * Predict,
          Metric2 = Target ^ 2,
          Metric3 = Predict ^ 2
        )]
        Metric <-
          ValidationData[, sum(Metric1, na.rm = TRUE)] / (sqrt(ValidationData[, sum(Metric2, na.rm = TRUE)]) *
                                                            sqrt(ValidationData[, sum(Metric3, na.rm = TRUE)]))
      } else if (tolower(metric) == "r2") {
        Metric <-
          (ValidationData[, stats::cor(eval(Target), Predict)][[1]]) ^ 2
      }
      data.table::set(EvaluationMetrics,
                      i = i,
                      j = 2L,
                      value = Metric)
    }, error = function(x)
      "skip")
  }
  
  # Save EvaluationMetrics to File
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  if (SaveModelObjects) {
    data.table::fwrite(EvaluationMetrics,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_EvaluationMetrics.csv"))
  }
  
  # Regression Variable Importance----
  VariableImportance <- xgboost::xgb.importance(model = model)
  VariableImportance[, ':=' (
    Gain = round(Gain, 4),
    Cover = round(Cover, 4),
    Frequency = round(Frequency, 4)
  )]
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_VariableImportance.csv"))
  }
  
  # Regression Partial Dependence----
  ParDepPlots <- list()
  j <- 0
  ParDepBoxPlots <- list()
  k <- 0
  for (i in seq_len(min(length(VariableImportance[, Feature]), NumOfParDepPlots))) {
    tryCatch({
      Out <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "Predict",
        TargetColName = "Target",
        IndepVar = VariableImportance[i, Feature],
        GraphType = "calibration",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      j <- j + 1
      ParDepPlots[[paste0(VariableImportance[j, Feature])]] <- Out
    }, error = function(x)
      "skip")
    tryCatch({
      Out1 <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "Predict",
        TargetColName = "Target",
        IndepVar = VariableImportance[i, Feature],
        GraphType = "boxplot",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      k <- k + 1
      ParDepBoxPlots[[paste0(VariableImportance[k, Feature])]] <-
        Out1
    }, error = function(x)
      "skip")
  }
  
  # Regression Save ParDepPlots to file----
  if (SaveModelObjects) {
    save(ParDepPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepPlots.R"))
  }
  
  # Regression Save ParDepBoxPlots to file----
  if (SaveModelObjects) {
    save(ParDepBoxPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepBoxPlots.R"))
  }
  
  # Regression Save GridCollect and GridList----
  if (SaveModelObjects & GridTune == TRUE) {
    data.table::fwrite(grid_params,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_grid_params.csv"))
    data.table::fwrite(GridCollect,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_GridCollect.csv"))
  }
  
  # Regression Remove Extraneous Variables----
  ValidationData[, ':=' (
    Metric = NULL,
    Metric1 = NULL,
    Metric2 = NULL,
    Metric3 = NULL
  )]
  
  # Regression Formal Evaluation Table
  EvaluationMetrics[, MetricValue := round(MetricValue, 4)]
  
  # Regression Return Model Objects----
  if (GridTune) {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          EvaluationPlot = EvaluationPlot,
          EvaluationBoxPlot = EvaluationBoxPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          PartialDependencePlots = ParDepPlots,
          PartialDependenceBoxPlots = ParDepBoxPlots,
          GridList = grid_params,
          GridMetrics = GridCollect,
          ColNames = Names
        )
      )
    }
  } else {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          EvaluationPlot = EvaluationPlot,
          EvaluationBoxPlot = EvaluationBoxPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          PartialDependencePlots = ParDepPlots,
          PartialDependenceBoxPlots = ParDepBoxPlots,
          ColNames = Names
        )
      )
    }
  }
}

#' AutoXGBoostClassifier is an automated XGBoost modeling framework with grid-tuning and model evaluation
#'
#' AutoXGBoostClassifier is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation plot, evaluation boxplot, evaluation metrics, variable importance, partial dependence calibration plots, partial dependence calibration box plots, and column names used in model fitting.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Note that the target column needs to be a 0 | 1 numeric variable.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "logloss","error","aucpr","auc"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param grid_eval_metric Set to "f","auc","tpr","fnr","fpr","tnr","prbe","f","odds"
#' @param MaxModelsInGrid Number of models to test from grid options (243 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param NumOfParDepPlots Tell the function the number of partial dependence calibration plots you want to create.
#' @param Verbose Set to 0 if you want to suppress model evaluation updates in training
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param PassInGrid Default is NULL. Provide a data.table of grid options from a previous run.
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 10000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Independent_Variable11 := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' data[, Target := ifelse(Target > 0.5, 1, 0)]
#' TestModel <- AutoXGBoostClassifier(data,
#'                                    ValidationData = NULL,
#'                                    TestData = NULL,
#'                                    TargetColumnName = 1,
#'                                    FeatureColNames = 2:12,
#'                                    IDcols = NULL,
#'                                    eval_metric = "auc",
#'                                    Trees = 50,
#'                                    GridTune = TRUE,
#'                                    grid_eval_metric = "auc",
#'                                    MaxModelsInGrid = 10,
#'                                    NThreads = 8,
#'                                    TreeMethod = "hist",
#'                                    model_path = getwd(),
#'                                    ModelID = "FirstModel",
#'                                    NumOfParDepPlots = 3,
#'                                    ReturnModelObjects = TRUE,
#'                                    SaveModelObjects = FALSE,
#'                                    PassInGrid = NULL)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, GridCollect, and GridList
#' @export
AutoXGBoostClassifier <- function(data,
                                  ValidationData = NULL,
                                  TestData = NULL,
                                  TargetColumnName = NULL,
                                  FeatureColNames = NULL,
                                  IDcols = NULL,
                                  eval_metric = "auc",
                                  Trees = 50,
                                  GridTune = FALSE,
                                  grid_eval_metric = "auc",
                                  TreeMethod = "hist",
                                  MaxModelsInGrid = 10,
                                  NThreads = 8,
                                  model_path = NULL,
                                  ModelID = "FirstModel",
                                  NumOfParDepPlots = 3,
                                  Verbose = 0,
                                  ReturnModelObjects = TRUE,
                                  SaveModelObjects = FALSE,
                                  PassInGrid = NULL) {
  # Binary Check Arguments----
  if (!(
    tolower(grid_eval_metric) %chin% c(
      "accuracy",
      "auc",
      "tpr",
      "fnr",
      "fpr",
      "tnr",
      "prbe",
      "f",
      "odds",
      "chisq"
    )
  )) {
    warning(
      "grid_eval_metric not in c('accuracy','auc','tpr','fnr','fpr','tnr','prbe','f','odds','chisq')"
    )
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (MaxModelsInGrid < 1 |
      MaxModelsInGrid > 1080 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1 and less than 1080")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path))
      warning("model_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (NumOfParDepPlots < 0)
    warning("NumOfParDepPlots needs to be a positive number")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  
  # Binary Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Binary Ensure data is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # Binary Ensure TestData is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # Binary Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # Binary IDcol Name Storage----
  if (!is.null(IDcols)) {
    if (!is.character(IDcols)) {
      IDcols <- names(data)[IDcols]
    }
  }
  
  # Binary Convert CatFeatures to 1-indexed----
  if (length(CatFeatures) > 0) {
    for (i in seq_len(length(CatFeatures))) {
      CatFeatures[i] <- CatFeatures[i] - 1
    }
  }
  
  # Binary Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(
    sapply(data, is.factor)
  )),
  as.numeric(which(
    sapply(data, is.character)
  ))))
  CatFeatures <- names(data)[CatFeatures]
  
  # Binary Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = Target,
      TimeColumnName = NULL
    )
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # Binary data Subset Columns Needed----
  if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    keep1 <- names(data)[c(FeatureColNames)]
    keep <- c(keep1, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  } else {
    keep <- c(FeatureColNames, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  }
  
  # Binary TestData Subset Columns Needed----
  if (!is.null(TestData)) {
    if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
      keep1 <- names(TestData)[c(FeatureColNames)]
      if (!is.null(IDcols)) {
        keep <- c(IDcols, keep1, Target)
      } else {
        keep <- c(keep1, Target)
      }
      TestData <- TestData[, ..keep]
    } else {
      keep1 <- c(FeatureColNames)
      if (!is.null(IDcols)) {
        keep <- c(IDcols, FeatureColNames, Target)
      } else {
        keep <- c(FeatureColNames, Target)
      }
      TestData <- TestData[, ..keep]
    }
    if (!is.null(IDcols)) {
      TestMerge <- data.table::copy(TestData)
      keep <- c(keep1, Target)
      TestData <- TestData[, ..keep]
    } else {
      TestMerge <- data.table::copy(TestData)
    }
  }
  
  # Binary Dummify dataTrain Categorical Features----
  if (SaveModelObjects) {
    if (!is.null(dataTest) & !is.null(TestData)) {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "VALIDATE")
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = "TEST")
      temp <-
        data.table::rbindlist(list(dataTrain, dataTest, TestData))
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = TRUE,
        SavePath = model_path,
        ImportFactorLevels = FALSE
      )
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
      TestData <- temp[ID_Factorizer == "TEST"]
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = NULL)
    } else {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      temp <- data.table::rbindlist(list(dataTrain, dataTest))
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = TRUE,
        SavePath = model_path,
        ImportFactorLevels = FALSE
      )
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
    }
  } else {
    if (!is.null(dataTest) & !is.null(TestData)) {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "VALIDATE")
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = "TEST")
      temp <-
        data.table::rbindlist(list(dataTrain, dataTest, TestData))
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = FALSE,
        SavePath = NULL,
        ImportFactorLevels = FALSE
      )
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
      TestData <- temp[ID_Factorizer == "TEST"]
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = NULL)
      
    } else {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      temp <- data.table::rbindlist(list(dataTrain, dataTest))
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = FALSE,
        SavePath = NULL,
        ImportFactorLevels = FALSE
      )
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
    }
  }
  
  # Binary Save Names of data----
  Names <- data.table::as.data.table(names(data))
  data.table::setnames(Names, "V1", "ColNames")
  if (SaveModelObjects) {
    data.table::fwrite(Names, paste0(model_path,
                                     "/"
                                     , ModelID, "_ColNames.csv"))
  }
  
  # Binary Subset Target Variables----
  TrainTarget <-
    tryCatch({
      dataTrain[, get(Target)]
    }, error = function(x)
      dataTrain[, eval(Target)])
  TestTarget <-
    tryCatch({
      dataTest[, get(Target)]
    }, error = function(x)
      dataTest[, eval(Target)])
  if (!is.null(TestData)) {
    FinalTestTarget <-
      tryCatch({
        TestData[, get(Target)]
      }, error = function(x)
        TestData[, eval(Target)])
  }
  
  # Binary Remove Target Variable from Feature Data
  dataTrain[, eval(Target) := NULL]
  dataTest[, eval(Target) := NULL]
  if (!is.null(TestData)) {
    TestData[, eval(Target) := NULL]
  }
  
  # Binary Initialize Catboost Data Conversion----
  datatrain <-
    xgboost::xgb.DMatrix(as.matrix(dataTrain), label = TrainTarget)
  datavalidate <-
    xgboost::xgb.DMatrix(as.matrix(dataTest), label = TestTarget)
  if (!is.null(TestData)) {
    datatest <-
      xgboost::xgb.DMatrix(as.matrix(TestData), label = FinalTestTarget)
    EvalSets <- list(train = datavalidate, test = datatest)
  } else {
    EvalSets <- list(train = datatrain, test = datavalidate)
  }
  
  
  # Binary Grid Tune or Not Check----
  if (GridTune) {
    # Binary Grid Create data.table To Store Results----
    GridCollect <-
      data.table::data.table(
        ParamRow = 1:(MaxModelsInGrid + 1),
        EvalStat = rep(9999999, MaxModelsInGrid + 1)
      )
    
    # Binary Grid Define Hyper Parameters----
    if (!is.null(PassInGrid)) {
      if (!data.table::is.data.table(PassInGrid)) {
        PassInGrid <- data.table::as.data.table(PassInGrid)
      }
      grid_params <- data.table::CJ(
        eta = c(0.30, 0.25, 0.35),
        max_depth = c(6, 8, 10),
        min_child_weight = c(1, 2, 3),
        subsample = c(1, 0.90, 0.80),
        colsample_bytree = c(1, 0.90, 0.80)
      )
      grid_params[, ID := runif(nrow(grid_params))]
      grid_params <-
        grid_params[order(ID)][1:(MaxModelsInGrid)][, ID := NULL]
      grid_params <-
        data.table::rbindlist(list(PassInGrid, grid_params))
    } else {
      grid_params <- data.table::CJ(
        eta = c(0.30, 0.25, 0.35),
        max_depth = c(6, 8, 10),
        min_child_weight = c(1, 2, 3),
        subsample = c(1, 0.90, 0.80),
        colsample_bytree = c(1, 0.90, 0.80)
      )
      grid_params[, ID := runif(nrow(grid_params))]
      grid_params <-
        grid_params[order(ID)][1:(MaxModelsInGrid + 1)][, ID := NULL]
    }
    
    # Binary Grid Tuning Main Loop----
    for (i in as.integer(seq_len(MaxModelsInGrid + 1))) {
      # Print i
      print(i)
      
      # Binary Grid Define Base Parameters----
      if (i == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:logistic',
          eval_metric = tolower(eval_metric),
          eta = 0.30,
          max_depth = 6,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
      } else {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:logistic',
          eval_metric = tolower(eval_metric),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
      }
      
      # Binary Grid Merge Model Parameters----
      # Have first model be the baseline model
      if (i != 1) {
        base_params <- c(as.list(grid_params[i,]), base_params)
      }
      
      # Binary Grid Train Model----
      if (Verbose == 0) {
        model <- xgboost::xgb.train(
          params = base_params,
          data = datatrain,
          watchlist = EvalSets,
          nrounds = Trees,
          verbose = Verbose,
          early_stopping_rounds = 10
        )
      } else {
        model <- xgboost::xgb.train(
          params = base_params,
          data = datatrain,
          watchlist = EvalSets,
          nrounds = Trees,
          early_stopping_rounds = 10
        )
      }
      
      
      # Binary Grid Score Model----
      if (!is.null(TestData)) {
        predict <- stats::predict(model, datatest)
      } else {
        predict <- stats::predict(model, datavalidate)
      }
      
      # Binary Grid Validation Data----
      if (!is.null(TestData)) {
        calibEval <-
          data.table::as.data.table(cbind(Target = FinalTestTarget, p1 = predict))
      } else {
        calibEval <-
          data.table::as.data.table(cbind(Target = TestTarget, p1 = predict))
      }
      
      # Binary Initialize AUC_List
      AUC_List <- list()
      
      # Binary Grid Evaluation Metrics for Each Grid----
      if (tolower(grid_eval_metric) == "accuracy") {
        j <- 0
        x <- data.table::data.table(
          Metric = "Accuracy",
          MetricValue = 5.0,
          Threshold = seq(0.01, 0.99, 0.001)
        )
        for (k in unique(x[["Threshold"]])) {
          j = as.integer(j + 1)
          Accuracy <-
            mean(calibEval[, ifelse(p1 > k &
                                      Target == 1 |
                                      p1 < k & Target == 0, 1, 0)])
          data.table::set(x,
                          i = j,
                          j = 2L,
                          value = round(Accuracy, 4))
        }
        data.table::setorderv(x,
                              "MetricValue",
                              order = -1,
                              na.last = TRUE)
        Metric <- x[1, MetricValue]
      } else {
        x <-
          ROCR::prediction(predictions = calibEval[["p1"]], labels = calibEval[["Target"]])
        y <-
          ROCR::performance(prediction.obj = x, measure = grid_eval_metric)
        if (any(
          nrow(data.table::as.data.table(y@y.values)) <= 1 |
          nrow(data.table::as.data.table(y@x.values)) <= 1
        )) {
          if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
              nrow(data.table::as.data.table(y@x.values)) <= 1) {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values,
                Threshold = y@x.values
              ))
            Metric <- z[[1]]
          } else if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
                     !(nrow(data.table::as.data.table(y@x.values) <= 1))) {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values,
                Threshold = y@x.values[[1]]
              ))
            Metric <- z[!is.infinite(Threshold)][[1]]
          } else if (!(nrow(data.table::as.data.table(y@y.values)) <= 1) &
                     nrow(data.table::as.data.table(y@x.values) <= 1)) {
            if (grid_eval_metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
              z <-
                data.table::as.data.table(cbind(
                  Metric = y@y.values[[1]],
                  Threshold = y@x.values
                ))
              Metric <-
                z[order(-Metric)][!is.infinite(Metric)][[1]]
            } else {
              z <-
                data.table::as.data.table(cbind(
                  Metric = y@y.values[[1]],
                  Threshold = y@x.values
                ))
              Metric <-
                z[order(Metric)][!is.infinite(Metric)][[1]]
            }
          }
        } else {
          if (metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values[[1]]
              ))
            Metric <-
              z[order(-Metric)][!is.infinite(Threshold) &
                                  !is.infinite(Metric)][1,]
          } else {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values[[1]]
              ))
            Metric <-
              z[order(Metric)][!is.infinite(Threshold) &
                                 !is.infinite(Metric)][1,]
          }
        }
      }
      
      # Binary AUC Object Create----
      AUC_Metrics <- pROC::roc(
        response = calibEval[["Target"]],
        predictor = calibEval[["p1"]],
        na.rm = TRUE,
        algorithm = 3,
        auc = TRUE,
        ci = TRUE
      )
      
      # Binary AUC Conversion to data.table----
      AUC_List[[i]] <- data.table::data.table(
        ModelNumber = i,
        Sensitivity = as.numeric(AUC_Metrics$sensitivities + 0.0001),
        Specificity = as.numeric(AUC_Metrics$specificities + 0.0001)
      )
      
      # Collect Metrics and Corresponding Grids
      # Store Output Information
      if (tolower(grid_eval_metric) == "accuracy") {
        data.table::set(GridCollect,
                        i = i,
                        j = 1L,
                        value = i)
        data.table::set(GridCollect,
                        i = i,
                        j = 2L,
                        value = Metric)
      } else if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
                     nrow(data.table::as.data.table(y@x.values)) <= 1)) {
        data.table::set(GridCollect,
                        i = i,
                        j = 1L,
                        value = i)
        data.table::set(GridCollect,
                        i = i,
                        j = 2L,
                        value = Metric)
      } else {
        data.table::set(GridCollect,
                        i = i,
                        j = 1L,
                        value = i)
        data.table::set(GridCollect,
                        i = i,
                        j = 2L,
                        value = Metric[, 1])
      }
    }
  }
  
  # Binary Define Final Model Parameters----
  if (GridTune) {
    if (eval_metric %chin% c("accuracy", "auc", "tpr", "prbe", "f", "odds")) {
      BestGrid <- GridCollect[order(-EvalStat)][1, ParamRow]
      if (BestGrid == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:logistic',
          eval_metric = tolower(eval_metric),
          eta = 0.30,
          max_depth = 6,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        
      } else {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:logistic',
          eval_metric = tolower(eval_metric),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        base_params <-
          c(as.list(grid_params[BestGrid,]), base_params)
      }
    } else {
      BestGrid <- GridCollect[order(EvalStat)][1, ParamRow]
      BestThresh <- GridCollect[order(EvalStat)][1, EvalStat]
      if (BestGrid == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:logistic',
          eval_metric = tolower(eval_metric),
          eta = 0.30,
          max_depth = 6,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        
      } else {
        base_params <- list(
          booster = "gbtree",
          objective = 'reg:logistic',
          eval_metric = tolower(eval_metric),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        base_params <-
          c(as.list(grid_params[BestGrid,]), base_params)
      }
    }
  } else {
    base_params <- list(
      booster = "gbtree",
      objective = 'reg:logistic',
      eval_metric = tolower(eval_metric),
      nthread = NThreads,
      max_bin = 64,
      tree_method = TreeMethod
    )
    if (!is.null(PassInGrid)) {
      base_params <- c(base_params, as.list(PassInGrid[1, ]))
    }
  }
  
  # Binary Train Final Model----
  if (Verbose == 0) {
    model <- xgboost::xgb.train(
      params = base_params,
      data = datatrain,
      watchlist = EvalSets,
      nrounds = Trees,
      verbose = Verbose,
      early_stopping_rounds = 10
    )
  } else {
    model <- xgboost::xgb.train(
      params = base_params,
      data = datatrain,
      watchlist = EvalSets,
      nrounds = Trees,
      early_stopping_rounds = 10
    )
  }
  
  # Binary Save Model----
  if (SaveModelObjects) {
    xgboost::xgb.save(model = model, fname = ModelID)
  }
  
  # Binary Grid Score Model----
  if (!is.null(TestData)) {
    predict <- stats::predict(model, datatest)
  } else {
    predict <- stats::predict(model, datavalidate)
  }
  
  # Binary Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, p1 = predict))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(Target = TestTarget, dataTest, p1 = predict))
  }
  
  # Binary AUC Object Create----
  AUC_Metrics <- pROC::roc(
    response = ValidationData[["Target"]],
    predictor = ValidationData[["p1"]],
    na.rm = TRUE,
    algorithm = 3,
    auc = TRUE,
    ci = TRUE
  )
  
  # Binary AUC Conversion to data.table----
  AUC_Data <- data.table::data.table(
    ModelNumber = 0,
    Sensitivity = AUC_Metrics$sensitivities,
    Specificity = AUC_Metrics$specificities
  )
  
  # Binary Rbind AUC
  if (GridTune == TRUE & MaxModelsInGrid <= 15) {
    temp <- data.table::rbindlist(AUC_List)
    AUC_Data <- data.table::rbindlist(list(temp, AUC_Data))
    AUC_Data[, ModelNumber := as.factor(ModelNumber)]
    
    # Binary Plot ROC Curve----
    ROC_Plot <-
      ggplot2::ggplot(AUC_Data,
                      ggplot2::aes(
                        x = 1 - Specificity,
                        group = ModelNumber,
                        color = ModelNumber
                      )) +
      ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]])) +
      ggplot2::geom_abline(slope = 1, color = "black") +
      ggplot2::ggtitle(paste0(
        "Catboost Best Model AUC: ",
        100 * round(AUC_Metrics$auc, 3),
        "%"
      )) +
      ChartTheme() + ggplot2::xlab("Specificity") +
      ggplot2::ylab("Sensitivity")
    
  } else {
    ROC_Plot <-
      ggplot2::ggplot(AUC_Data, ggplot2::aes(x = 1 - Specificity)) +
      ggplot2::geom_line(ggplot2::aes(y = AUC_Data[["Sensitivity"]]), color = "blue") +
      ggplot2::geom_abline(slope = 1, color = "black") +
      ggplot2::ggtitle(paste0("Catboost AUC: ",
                              100 * round(AUC_Metrics$auc, 3), "%")) +
      ChartTheme() + ggplot2::xlab("Specificity") +
      ggplot2::ylab("Sensitivity")
  }
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path, "/", ModelID, "_ROC_Plot.png"))
  }
  
  # Binary Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = ValidationData,
    PredictionColName = "p1",
    TargetColName = Target,
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Add Number of Trees to Title
  EvaluationPlot <- EvaluationPlot +
    ggplot2::ggtitle(paste0(
      "Calibration Evaluation Plot: AUC = ",
      round(AUC_Metrics$auc, 3)
    ))
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(model_path, "/", ModelID, "_EvaluationPlot.png"))
  }
  
  # Evaluation Metrics at Optimial Threshold----
  x <- ROCR::prediction(predictions = ValidationData[["p1"]],
                        labels = ValidationData[["Target"]])
  EvaluationMetrics <-
    data.table::data.table(
      Metric = c(
        "AUC",
        "TruePositiveRate",
        "FalseNegativeRate",
        "FalsePositiveRate",
        "TrueNegativeRate",
        "PreceisionRecallBreakEven",
        "F1_Score",
        "Odds"
      ),
      MetricValue = rep(999999, 8),
      Threshold   = rep(999999, 8)
    )
  i <- 0
  for (metric in c("auc", "tpr", "fnr", "fpr", "tnr", "prbe", "f", "odds")) {
    i <- as.integer(i + 1)
    tryCatch({
      y <- ROCR::performance(prediction.obj = x, measure = metric)
      if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
              nrow(data.table::as.data.table(y@x.values)) <= 1)) {
        if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
            nrow(data.table::as.data.table(y@x.values)) <= 1) {
          z <-
            data.table::as.data.table(cbind(
              Metric = y@y.values,
              Threshold = y@x.values
            ))
          Metric <- z[[1]]
        } else if (nrow(data.table::as.data.table(y@y.values)) <= 1 &
                   !(nrow(data.table::as.data.table(y@x.values) <= 1))) {
          z <-
            data.table::as.data.table(cbind(
              Metric = y@y.values,
              Threshold = y@x.values[[1]]
            ))
          Metric <- z[!is.infinite(Threshold)][[1]]
        } else if (!(nrow(data.table::as.data.table(y@y.values)) <= 1) &
                   nrow(data.table::as.data.table(y@x.values) <= 1)) {
          if (metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values
              ))
            Metric <- z[order(-Metric)][!is.infinite(Metric)][[1]]
          } else {
            z <-
              data.table::as.data.table(cbind(
                Metric = y@y.values[[1]],
                Threshold = y@x.values
              ))
            Metric <- z[order(Metric)][!is.infinite(Metric)][[1]]
          }
        }
      } else {
        if (metric %chin% c("auc", "tpr", "tnr", "prbe", "f", "odds")) {
          z <-
            data.table::as.data.table(cbind(
              Metric = y@y.values[[1]],
              Threshold = y@x.values[[1]]
            ))
          Metric <-
            z[order(-Metric)][!is.infinite(Threshold) &
                                !is.infinite(Metric)][1,]
        } else {
          z <-
            data.table::as.data.table(cbind(
              Metric = y@y.values[[1]],
              Threshold = y@x.values[[1]]
            ))
          Metric <-
            z[order(Metric)][!is.infinite(Threshold) &
                               !is.infinite(Metric)][1,]
        }
      }
      
      # Store Output Information
      if (any(nrow(data.table::as.data.table(y@y.values)) <= 1 |
              nrow(data.table::as.data.table(y@x.values)) <= 1)) {
        data.table::set(
          EvaluationMetrics,
          i = i,
          j = 2L,
          value = round(Metric[[1]], 4)
        )
        data.table::set(EvaluationMetrics,
                        i = i,
                        j = 3L,
                        value = NA)
      } else {
        data.table::set(
          EvaluationMetrics,
          i = i,
          j = 2L,
          value = round(Metric[[1]], 4)
        )
        data.table::set(
          EvaluationMetrics,
          i = i,
          j = 3L,
          value = Metric[[2]]
        )
      }
    }, error = function(x)
      "skip")
  }
  
  # Binary Accuracy Threshold and Metric----
  j <- 0
  x <-
    data.table(
      Metric = "Accuracy",
      MetricValue = 5.0,
      Threshold = seq(0.01, 0.99, 0.001)
    )
  for (i in unique(x[["Threshold"]])) {
    j = as.integer(j + 1)
    Accuracy <-
      mean(ValidationData[, ifelse(p1 > i &
                                     Target == 1 |
                                     p1 < i & Target == 0, 1, 0)])
    set(x,
        i = j,
        j = 2L,
        value = round(Accuracy, 4))
  }
  data.table::setorderv(x, "MetricValue", order = -1, na.last = TRUE)
  x <- x[1,]
  EvaluationMetrics <-
    data.table::rbindlist(list(EvaluationMetrics, x))
  
  # Save EvaluationMetrics to File
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  if (SaveModelObjects) {
    data.table::fwrite(EvaluationMetrics,
                       file = paste0(model_path, "/", ModelID, "_EvaluationMetrics.csv"))
  }
  
  # Binary Variable Importance----
  VariableImportance <- xgboost::xgb.importance(model = model)
  VariableImportance[, ':=' (
    Gain = round(Gain, 4),
    Cover = round(Cover, 4),
    Frequency = round(Frequency, 4)
  )]
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_VariableImportance.csv"))
  }
  
  # Binary Partial Dependence----
  ParDepPlots <- list()
  j <- 0
  ParDepBoxPlots <- list()
  k <- 0
  for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
    tryCatch({
      Out <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "p1",
        TargetColName = Target,
        IndepVar = VariableImportance[i, Feature],
        GraphType = "calibration",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      j <- j + 1
      ParDepPlots[[paste0(VariableImportance[j, Feature])]] <- Out
    }, error = function(x)
      "skip")
  }
  
  # Binary Save ParDepPlots to file----
  if (SaveModelObjects) {
    save(ParDepPlots,
         file = paste0(model_path, "/", ModelID, "_ParDepPlots.R"))
  }
  
  # Binary Save GridCollect and GridList----
  if (SaveModelObjects & GridTune == TRUE) {
    data.table::fwrite(grid_params,
                       file = paste0(model_path, "/", ModelID, "_grid_params.csv"))
    data.table::fwrite(GridCollect,
                       file = paste0(model_path, "/", ModelID, "_GridCollect.csv"))
  }
  
  # Binary Return Model Objects----
  if (GridTune) {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          ROC_Plot = ROC_Plot,
          EvaluationPlot = EvaluationPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          PartialDependencePlots = ParDepPlots,
          GridList = grid_params,
          GridMetrics = GridCollect,
          ColNames = Names
        )
      )
    }
  } else {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          ROC_Plot = ROC_Plot,
          EvaluationPlot = EvaluationPlot,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          PartialDependencePlots = ParDepPlots,
          ColNames = Names
        )
      )
    }
  }
}

#' AutoXGBoostMultiClass is an automated XGBoost modeling framework with grid-tuning and model evaluation
#'
#' AutoXGBoostMultiClass is an automated XGBoost modeling framework with grid-tuning and model evaluation that runs a variety of steps. First, a stratified sampling (by the target variable) is done to create train and validation sets. Then, the function will run a random grid tune over N number of models and find which model is the best (a default model is always included in that set). Once the model is identified and built, several other outputs are generated: validation data with predictions, evaluation metrics, variable importance, and column names used in model fitting.
#' @author Adrian Antico
#' @family Supervised Learning
#' @param data This is your data set for training and testing your model
#' @param ValidationData This is your holdout data set used in modeling either refine your hyperparameters.
#' @param TestData This is your holdout data set. Catboost using both training and validation data in the training process so you should evaluate out of sample performance with this data set.
#' @param TargetColumnName Either supply the target column name OR the column number where the target is located (but not mixed types). Target should be in factor or character form.
#' @param FeatureColNames Either supply the feature column names OR the column number where the target is located (but not mixed types)
#' @param IDcols A vector of column names or column numbers to keep in your data but not include in the modeling.
#' @param eval_metric This is the metric used to identify best grid tuned model. Choose from "merror", "mlogloss"
#' @param Trees The maximum number of trees you want in your models
#' @param GridTune Set to TRUE to run a grid tuning procedure. Set a number in MaxModelsInGrid to tell the procedure how many models you want to test.
#' @param NThreads Set the maximum number of threads you'd like to dedicate to the model run. E.g. 8
#' @param TreeMethod Choose from "hist", "gpu_hist"
#' @param grid_eval_metric Set to "accuracy" (only option currently)
#' @param MaxModelsInGrid Number of models to test from grid options (243 total possible options)
#' @param model_path A character string of your path file to where you want your output saved
#' @param ModelID A character string to name your model and output
#' @param Verbose Set to 0 if you want to suppress model evaluation updates in training
#' @param ReturnModelObjects Set to TRUE to output all modeling objects (E.g. plots and evaluation metrics)
#' @param SaveModelObjects Set to TRUE to return all modeling objects to your environment
#' @param PassInGrid Default is NULL. Provide a data.table of grid options from a previous run.
#' @examples
#' \donttest{
#' Correl <- 0.85
#' N <- 10000
#' data <- data.table::data.table(Target = runif(N))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(N)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable2 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
#'                                                 sqrt(1-Correl^2) * qnorm(x2))))]
#' data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
#'                                              sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Independent_Variable6 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.10]
#' data[, Independent_Variable7 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.25]
#' data[, Independent_Variable8 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^0.75]
#' data[, Independent_Variable9 := (pnorm(Correl * x1 +
#'                                          sqrt(1-Correl^2) * qnorm(x2)))^2]
#' data[, Independent_Variable10 := (pnorm(Correl * x1 +
#'                                           sqrt(1-Correl^2) * qnorm(x2)))^4]
#' data[, Target := as.factor(
#'   ifelse(Independent_Variable2 < 0.20, "A",
#'          ifelse(Independent_Variable2 < 0.40, "B",
#'                 ifelse(Independent_Variable2 < 0.6,  "C",
#'                        ifelse(Independent_Variable2 < 0.8,  "D", "E")))))]
#' data[, Independent_Variable11 := as.factor(
#' ifelse(Independent_Variable2 < 0.25, "A",
#'        ifelse(Independent_Variable2 < 0.35, "B",
#'               ifelse(Independent_Variable2 < 0.65,  "C",
#'                      ifelse(Independent_Variable2 < 0.75,  "D", "E")))))]
#' data[, ':=' (x1 = NULL, x2 = NULL)]
#' TestModel <- AutoXGBoostMultiClass(data,
#'                                    ValidationData = NULL,
#'                                    TestData = NULL,
#'                                    TargetColumnName = 1,
#'                                    FeatureColNames = 2:12,
#'                                    IDcols = NULL,
#'                                    eval_metric = "merror",
#'                                    Trees = 50,
#'                                    GridTune = TRUE,
#'                                    grid_eval_metric = "accuracy",
#'                                    MaxModelsInGrid = 10,
#'                                    NThreads = 8,
#'                                    TreeMethod = "hist",
#'                                    model_path = getwd(),
#'                                    ModelID = "FirstModel",
#'                                    ReturnModelObjects = TRUE,
#'                                    SaveModelObjects = FALSE,
#'                                    PassInGrid = NULL)
#' }
#' @return Saves to file and returned in list: VariableImportance.csv, Model, ValidationData.csv, EvaluationMetrics.csv, GridCollect, and GridList
#' @export
AutoXGBoostMultiClass <- function(data,
                                  ValidationData = NULL,
                                  TestData = NULL,
                                  TargetColumnName = NULL,
                                  FeatureColNames = NULL,
                                  IDcols = NULL,
                                  eval_metric = "merror",
                                  Trees = 50,
                                  GridTune = FALSE,
                                  grid_eval_metric = "merror",
                                  TreeMethod = "hist",
                                  MaxModelsInGrid = 10,
                                  NThreads = 8,
                                  model_path = NULL,
                                  ModelID = "FirstModel",
                                  Verbose = 0,
                                  ReturnModelObjects = TRUE,
                                  SaveModelObjects = FALSE,
                                  PassInGrid = NULL) {
  # MultiClass Check Arguments----
  if (!(tolower(grid_eval_metric) %chin% c("accuracy"))) {
    warning("grid_eval_metric not accuracy")
  }
  if (Trees < 1)
    warning("Trees must be greater than 1")
  if (!GridTune %in% c(TRUE, FALSE))
    warning("GridTune needs to be TRUE or FALSE")
  if (MaxModelsInGrid < 1 |
      MaxModelsInGrid > 1080 & GridTune == TRUE) {
    warning("MaxModelsInGrid needs to be at least 1 and less than 1080")
  }
  if (!is.null(model_path)) {
    if (!is.character(model_path))
      warning("model_path needs to be a character type")
  }
  if (!is.character(ModelID))
    warning("ModelID needs to be a character type")
  if (!(ReturnModelObjects %in% c(TRUE, FALSE)))
    warning("ReturnModelObjects needs to be TRUE or FALSE")
  if (!(SaveModelObjects %in% c(TRUE, FALSE)))
    warning("SaveModelObjects needs to be TRUE or FALSE")
  
  # MultiClass Ensure data is a data.table----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # MultiClass Ensure data is a data.table----
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  
  # MultiClass Ensure TestData is a data.table----
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # MultiClass Target Name Storage----
  if (is.character(TargetColumnName)) {
    Target <- TargetColumnName
  } else {
    Target <- names(data)[TargetColumnName]
  }
  
  # MultiClass IDcol Name Storage----
  if (!is.null(IDcols)) {
    if (!is.character(IDcols)) {
      IDcols <- names(data)[IDcols]
    }
  }
  
  # MultiClass Identify column numbers for factor variables----
  CatFeatures <- sort(c(as.numeric(which(
    sapply(data, is.factor)
  )),
  as.numeric(which(
    sapply(data, is.character)
  ))))
  CatFeatures <- names(data)[CatFeatures]
  CatFeatures <- setdiff(CatFeatures, Target)
  
  # MultiClass Data Partition----
  if (is.null(ValidationData) & is.null(TestData)) {
    dataSets <- AutoDataPartition(
      data,
      NumDataSets = 3,
      Ratios = c(0.70, 0.20, 0.10),
      PartitionType = "random",
      StratifyColumnNames = Target,
      TimeColumnName = NULL
    )
    data <- dataSets$TrainData
    ValidationData <- dataSets$ValidationData
    TestData <- dataSets$TestData
  }
  
  # MultiClass data Subset Columns Needed----
  if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    keep1 <- names(data)[c(FeatureColNames)]
    keep <- c(keep1, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  } else {
    keep <- c(FeatureColNames, Target)
    dataTrain <- data[, ..keep]
    dataTest <- ValidationData[, ..keep]
  }
  
  # MultiClass TestData Subset Columns Needed----
  if (!is.null(TestData)) {
    if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
      keep1 <- names(TestData)[c(FeatureColNames)]
      if (!is.null(IDcols)) {
        keep <- c(IDcols, keep1, Target)
      } else {
        keep <- c(keep1, Target)
      }
      TestData <- TestData[, ..keep]
    } else {
      keep1 <- c(FeatureColNames)
      if (!is.null(IDcols)) {
        keep <- c(IDcols, FeatureColNames, Target)
      } else {
        keep <- c(FeatureColNames, Target)
      }
      TestData <- TestData[, ..keep]
    }
    if (!is.null(IDcols)) {
      TestMerge <- data.table::copy(TestData)
      keep <- c(keep1, Target)
      TestData <- TestData[, ..keep]
    } else {
      TestMerge <- data.table::copy(TestData)
    }
  }
  
  # MultiClass Obtain Unique Target Levels
  if (!is.null(TestData)) {
    temp <- data.table::rbindlist(list(dataTrain, dataTest, TestData))
  } else {
    temp <- data.table::rbindlist(list(dataTrain, dataTest))
  }
  TargetLevels <-
    data.table::as.data.table(sort(unique(temp[[eval(Target)]])))
  data.table::setnames(TargetLevels, "V1", "OriginalLevels")
  TargetLevels[, NewLevels := 0:(.N - 1)]
  if (SaveModelObjects) {
    data.table::fwrite(TargetLevels,
                       file = paste0(model_path,
                                     "/",
                                     ModelID,
                                     "_TargetLevels.csv"))
  }
  
  # MultiClass Convert Target to Numeric Factor
  dataTrain <- merge(
    dataTrain,
    TargetLevels,
    by.x = eval(Target),
    by.y = "OriginalLevels",
    all = FALSE
  )
  dataTrain[, paste0(Target) := NewLevels]
  dataTrain[, NewLevels := NULL]
  dataTest <- merge(
    dataTest,
    TargetLevels,
    by.x = eval(Target),
    by.y = "OriginalLevels",
    all = FALSE
  )
  dataTest[, paste0(Target) := NewLevels]
  dataTest[, NewLevels := NULL]
  if (!is.null(TestData)) {
    TestData <- merge(
      TestData,
      TargetLevels,
      by.x = eval(Target),
      by.y = "OriginalLevels",
      all = FALSE
    )
    TestData[, paste0(Target) := NewLevels]
    TestData[, NewLevels := NULL]
  }
  
  # MultiClass Dummify dataTrain Categorical Features----
  if (SaveModelObjects) {
    if (!is.null(dataTest) & !is.null(TestData)) {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "VALIDATE")
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = "TEST")
      temp <-
        data.table::rbindlist(list(dataTrain, dataTest, TestData))
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = TRUE,
        SavePath = model_path,
        ImportFactorLevels = FALSE
      )
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
      TestData <- temp[ID_Factorizer == "TEST"]
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = NULL)
    } else {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      temp <- data.table::rbindlist(list(dataTrain, dataTest))
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = TRUE,
        SavePath = model_path,
        ImportFactorLevels = FALSE
      )
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
    }
  } else {
    if (!is.null(dataTest) & !is.null(TestData)) {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "VALIDATE")
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = "TEST")
      temp <-
        data.table::rbindlist(list(dataTrain, dataTest, TestData))
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = FALSE,
        SavePath = NULL,
        ImportFactorLevels = FALSE
      )
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
      TestData <- temp[ID_Factorizer == "TEST"]
      data.table::set(TestData,
                      j = "ID_Factorizer",
                      value = NULL)
      
    } else {
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = "TRAIN")
      temp <- data.table::rbindlist(list(dataTrain, dataTest))
      temp <- DummifyDT(
        data = temp,
        cols = CatFeatures,
        KeepFactorCols = FALSE,
        OneHot = FALSE,
        SaveFactorLevels = FALSE,
        SavePath = NULL,
        ImportFactorLevels = FALSE
      )
      dataTrain <- temp[ID_Factorizer == "TRAIN"]
      data.table::set(dataTrain,
                      j = "ID_Factorizer",
                      value = NULL)
      dataTest <- temp[ID_Factorizer == "VALIDATE"]
      data.table::set(dataTest,
                      j = "ID_Factorizer",
                      value = NULL)
    }
  }
  
  # MultiClass Save Names of data----
  Names <- data.table::as.data.table(names(data))
  data.table::setnames(Names, "V1", "ColNames")
  if (SaveModelObjects) {
    data.table::fwrite(Names, paste0(model_path,
                                     "/"
                                     , ModelID, "_ColNames.csv"))
  }
  
  # MultiClass Subset Target Variables----
  TrainTarget <-
    tryCatch({
      dataTrain[, get(Target)]
    }, error = function(x)
      dataTrain[, eval(Target)])
  TestTarget <-
    tryCatch({
      dataTest[, get(Target)]
    }, error = function(x)
      dataTest[, eval(Target)])
  if (!is.null(TestData)) {
    FinalTestTarget <-
      tryCatch({
        TestData[, get(Target)]
      }, error = function(x)
        TestData[, eval(Target)])
  }
  
  # MultiClass Remove Target Variable from Feature Data
  dataTrain[, eval(Target) := NULL]
  dataTest[, eval(Target) := NULL]
  if (!is.null(TestData)) {
    TestData[, eval(Target) := NULL]
  }
  
  # MultiClass Initialize XGBoost Data Conversion----
  datatrain <-
    xgboost::xgb.DMatrix(as.matrix(dataTrain), label = TrainTarget)
  datavalidate <-
    xgboost::xgb.DMatrix(as.matrix(dataTest), label = TestTarget)
  if (!is.null(TestData)) {
    datatest <-
      xgboost::xgb.DMatrix(as.matrix(TestData), label = FinalTestTarget)
    EvalSets <- list(train = datavalidate, test = datatest)
  } else {
    EvalSets <- list(train = datatrain, test = datavalidate)
  }
  
  # MultiClass Grid Tune or Not Check----
  if (GridTune) {
    # MultiClass Grid Create data.table To Store Results----
    GridCollect <-
      data.table::data.table(
        ParamRow = 1:(MaxModelsInGrid + 1),
        EvalStat = rep(9999999, MaxModelsInGrid + 1)
      )
    
    # MultiClass Grid Define Hyper Parameters----
    if (!is.null(PassInGrid)) {
      if (!data.table::is.data.table(PassInGrid)) {
        PassInGrid <- data.table::as.data.table(PassInGrid)
      }
      grid_params <- data.table::CJ(
        eta = c(0.30, 0.25, 0.35),
        max_depth = c(6, 8, 10),
        min_child_weight = c(1, 2, 3),
        subsample = c(1, 0.90, 0.80),
        colsample_bytree = c(1, 0.90, 0.80)
      )
      grid_params[, ID := runif(nrow(grid_params))]
      grid_params <-
        grid_params[order(ID)][1:(MaxModelsInGrid)][, ID := NULL]
      grid_params <-
        data.table::rbindlist(list(PassInGrid, grid_params))
    } else {
      grid_params <- data.table::CJ(
        eta = c(0.30, 0.25, 0.35),
        max_depth = c(6, 8, 10),
        min_child_weight = c(1, 2, 3),
        subsample = c(1, 0.90, 0.80),
        colsample_bytree = c(1, 0.90, 0.80)
      )
      grid_params[, ID := runif(nrow(grid_params))]
      grid_params <-
        grid_params[order(ID)][1:(MaxModelsInGrid + 1)][, ID := NULL]
    }
    
    # MultiClass Grid Tuning Main Loop----
    for (i in as.integer(seq_len(MaxModelsInGrid + 1))) {
      # Print i
      print(i)
      
      # MultiClass Grid Define Base Parameters----
      if (i == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'multi:softmax',
          eval_metric = tolower(eval_metric),
          num_class = (TargetLevels[, max(NewLevels)] +
                         1),
          eta = 0.30,
          max_depth = 6,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
      } else {
        base_params <- list(
          booster = "gbtree",
          objective = 'multi:softmax',
          eval_metric = tolower(eval_metric),
          num_class = (TargetLevels[, max(NewLevels)] +
                         1),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
      }
      
      # MultiClass Grid Merge Model Parameters----
      # Have first model be the baseline model
      if (i != 1) {
        base_params <- c(as.list(grid_params[i,]), base_params)
      }
      
      # MultiClass Grid Train Model----
      if (Verbose == 0) {
        model <- xgboost::xgb.train(
          params = base_params,
          data = datatrain,
          watchlist = EvalSets,
          nrounds = Trees,
          verbose = Verbose,
          early_stopping_rounds = 10
        )
      } else {
        model <- xgboost::xgb.train(
          params = base_params,
          data = datatrain,
          watchlist = EvalSets,
          nrounds = Trees,
          early_stopping_rounds = 10
        )
      }
      
      
      # MultiClass Grid Score Model----
      if (!is.null(TestData)) {
        predict <- stats::predict(model, datatest)
      } else {
        predict <- stats::predict(model, datavalidate)
      }
      
      # MultiClass Grid Validation Data----
      if (!is.null(TestData)) {
        calibEval <-
          data.table::as.data.table(cbind(Target = FinalTestTarget, p1 = predict))
      } else {
        calibEval <-
          data.table::as.data.table(cbind(Target = TestTarget, p1 = predict))
      }
      
      # MultiClass Accuracy
      Metric <-
        calibEval[, mean(ifelse(p1 == eval(Target), 1, 0), na.rm = TRUE)]
      
      # MultiClass Store Output Information----
      data.table::set(GridCollect,
                      i = i,
                      j = 1L,
                      value = i)
      data.table::set(GridCollect,
                      i = i,
                      j = 2L,
                      value = Metric)
    }
  }
  
  # MultiClass Define Final Model Parameters----
  if (GridTune) {
    if (eval_metric %chin% c("merror", "mlogloss")) {
      BestGrid <- GridCollect[order(-EvalStat)][1, ParamRow]
      if (BestGrid == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'multi:softmax',
          eval_metric = tolower(eval_metric),
          num_class = (TargetLevels[, max(NewLevels)] +
                         1),
          eta = 0.30,
          max_depth = 6,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        
      } else {
        base_params <- list(
          booster = "gbtree",
          objective = 'multi:softmax',
          eval_metric = tolower(eval_metric),
          num_class = (TargetLevels[, max(NewLevels)] +
                         1),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        base_params <-
          c(as.list(grid_params[BestGrid,]), base_params)
      }
    } else {
      BestGrid <- GridCollect[order(EvalStat)][1, ParamRow]
      BestThresh <- GridCollect[order(EvalStat)][1, EvalStat]
      if (BestGrid == 1) {
        base_params <- list(
          booster = "gbtree",
          objective = 'multi:softmax',
          eval_metric = tolower(eval_metric),
          num_class = (TargetLevels[, max(NewLevels)] +
                         1),
          eta = 0.30,
          max_depth = 6,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        
      } else {
        base_params <- list(
          booster = "gbtree",
          objective = 'multi:softmax',
          eval_metric = tolower(eval_metric),
          num_class = (TargetLevels[, max(NewLevels)] +
                         1),
          nthread = NThreads,
          max_bin = 64,
          tree_method = TreeMethod
        )
        base_params <-
          c(as.list(grid_params[BestGrid,]), base_params)
      }
    }
  } else {
    base_params <- list(
      booster = "gbtree",
      objective = "multi:softmax",
      eval_metric = tolower(eval_metric),
      num_class = (TargetLevels[, max(NewLevels)] +
                     1),
      nthread = NThreads,
      max_bin = 64,
      tree_method = TreeMethod
    )
    if (!is.null(PassInGrid)) {
      base_params <- c(base_params, as.list(PassInGrid[1, ]))
    }
  }
  
  # MultiClass Train Final Model----
  if (Verbose == 0) {
    model <- xgboost::xgb.train(
      params = base_params,
      data = datatrain,
      watchlist = EvalSets,
      nrounds = Trees,
      verbose = Verbose,
      early_stopping_rounds = 10
    )
  } else {
    model <- xgboost::xgb.train(
      params = base_params,
      data = datatrain,
      watchlist = EvalSets,
      nrounds = Trees,
      early_stopping_rounds = 10
    )
  }
  
  # MultiClass Save Model----
  if (SaveModelObjects) {
    xgboost::xgb.save(model = model, fname = ModelID)
  }
  
  # MultiClass Grid Score Model----
  if (!is.null(TestData)) {
    predict <- stats::predict(model, datatest)
  } else {
    predict <- stats::predict(model, datavalidate)
  }
  
  # MultiClass Validation Data----
  if (!is.null(TestData)) {
    ValidationData <-
      data.table::as.data.table(cbind(Target = FinalTestTarget, TestMerge, p1 = predict))
  } else {
    ValidationData <-
      data.table::as.data.table(cbind(Target = TestTarget, dataTest, p1 = predict))
  }
  
  # MultiClass Evaluation Metrics----
  EvaluationMetrics <- data.table::data.table(Metric = "Accuracy",
                                              MetricValue = ValidationData[, mean(ifelse(p1 == eval(Target), 1, 0),
                                                                                  na.rm = TRUE)])
  
  
  
  # Save EvaluationMetrics to File
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  if (SaveModelObjects) {
    data.table::fwrite(EvaluationMetrics,
                       file = paste0(model_path, "/", ModelID, "_EvaluationMetrics.csv"))
  }
  
  # MultiClass Variable Importance----
  VariableImportance <- xgboost::xgb.importance(model = model)
  VariableImportance[, ':=' (
    Gain = round(Gain, 4),
    Cover = round(Cover, 4),
    Frequency = round(Frequency, 4)
  )]
  if (SaveModelObjects) {
    data.table::fwrite(VariableImportance,
                       file = paste0(model_path,
                                     "/",
                                     ModelID, "_VariableImportance.csv"))
  }
  
  # MultiClass Save GridCollect and grid_metrics----
  if (SaveModelObjects & GridTune == TRUE) {
    data.table::fwrite(grid_params,
                       file = paste0(model_path, "/", ModelID, "_grid_params.csv"))
    data.table::fwrite(GridCollect,
                       file = paste0(model_path, "/", ModelID, "_GridCollect.csv"))
  }
  
  # MultiClass Return Model Objects----
  if (GridTune) {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          GridList = grid_params,
          GridMetrics = GridCollect,
          ColNames = Names
        )
      )
    }
  } else {
    if (ReturnModelObjects) {
      return(
        list(
          Model = model,
          ValidationData = ValidationData,
          EvaluationMetrics = EvaluationMetrics,
          VariableImportance = VariableImportance,
          ColNames = Names
        )
      )
    }
  }
}

#' AutoCatBoostScoring is an automated scoring function that compliments the AutoCatBoost model training functions.
#'
#' AutoCatBoostScoring is an automated scoring function that compliments the AutoCatBoost model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep() to prepare your features for catboost data conversion and scoring.
#'
#' @family Supervised Learning
#' @param TargetType Set this value to "regression", "classification", or "multiclass" to score models built using AutoCatBoostRegression(), AutoCatBoostClassify() or AutoCatBoostMultiClass().
#' @param ScoringData This is your data.table of features for scoring. Can be a single row or batch.
#' @param FeatureColumnNames Supply either column names or column numbers used in the AutoCatBoostRegression() function
#' @param IDcols Supply ID column numbers for any metadata you want returned with your predicted values
#' @param ModelObject Supply the model object directly for scoring instead of loading it from file. If you supply this, ModelID and ModelPath will be ignored.
#' @param ModelPath Supply your path file used in the AutoCatBoost__() function
#' @param ModelID Supply the model ID used in the AutoCatBoost__() function
#' @param ReturnFeatures Set to TRUE to return your features with the predicted values.
#' @param MDP_Impute Set to TRUE if you did so for modeling and didn't do so before supplying ScoringData in this function
#' @param MDP_CharToFactor Set to TRUE to turn your character columns to factors if you didn't do so to your ScoringData that you are supplying to this function
#' @param MDP_RemoveDates Set to TRUE if you have date of timestamp columns in your ScoringData
#' @param MDP_MissFactor If you set MDP_Impute to TRUE, supply the character values to replace missing values with
#' @param MDP_MissNum If you set MDP_Impute to TRUE, supply a numeric value to replace missing values with
#' @examples
#' \donttest{
#' Preds <- AutoCatBoostScoring(TargetType = "regression",
#'                              ScoringData = data,
#'                              FeatureColumnNames = 2:12,
#'                              IDcols = NULL,
#'                              ModelObject = NULL,
#'                              ModelPath = "home",
#'                              ModelID = "ModelTest",
#'                              ReturnFeatures = TRUE,
#'                              MDP_Impute = TRUE,
#'                              MDP_CharToFactor = TRUE,
#'                              MDP_RemoveDates = TRUE,
#'                              MDP_MissFactor = "0",
#'                              MDP_MissNum = -1)
#' }
#' @return A data.table of predicted values with the option to return model features as well.
#' @export
AutoCatBoostScoring <- function(TargetType = NULL,
                                ScoringData = NULL,
                                FeatureColumnNames = NULL,
                                IDcols = NULL,
                                ModelObject = NULL,
                                ModelPath = NULL,
                                ModelID = NULL,
                                ReturnFeatures = TRUE,
                                MDP_Impute = TRUE,
                                MDP_CharToFactor = TRUE,
                                MDP_RemoveDates = TRUE,
                                MDP_MissFactor = "0",
                                MDP_MissNum = -1) {
  # Load catboost----
  loadNamespace(package = "catboost")
  
  # Check arguments----
  if (is.null(ScoringData)) {
    warning("ScoringData cannot be NULL")
  }
  if (is.null(FeatureColumnNames)) {
    warning("FeatureColumnNames cannot be NULL")
  }
  if (!data.table::is.data.table(ScoringData)) {
    ScoringData <- data.table::as.data.table(ScoringData)
  }
  if (!is.logical(MDP_Impute)) {
    warning("MDP_Impute (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.logical(MDP_CharToFactor)) {
    warning("MDP_CharToFactor (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.logical(MDP_RemoveDates)) {
    warning("MDP_RemoveDates (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.character(MDP_MissFactor) & !is.factor(MDP_MissFactor)) {
    warning("MDP_MissFactor should be a character or factor value")
  }
  if (!is.numeric(MDP_MissNum)) {
    warning("MDP_MissNum should be a numeric or integer value")
  }
  
  # ModelDataPrep Check----
  ScoringData <- ModelDataPrep(
    data = ScoringData,
    Impute = MDP_Impute,
    CharToFactor = MDP_CharToFactor,
    RemoveDates = MDP_RemoveDates,
    MissFactor = MDP_MissFactor,
    MissNum = MDP_MissNum
  )
  
  # Identify column numbers for factor variables----
  CatFeatures <-
    sort(c(as.numeric(which(
      sapply(ScoringData, is.factor)
    )),
    as.numeric(which(
      sapply(ScoringData, is.character)
    ))))
  
  # Convert CatFeatures to 1-indexed----
  if (!is.null(CatFeatures)) {
    for (i in seq_len(length(CatFeatures))) {
      CatFeatures[i] <- CatFeatures[i] - 1
    }
  }
  
  # IDcols conversion----
  if (is.numeric(IDcols) | is.integer(IDcols)) {
    IDcols <- names(data)[IDcols]
  }
  
  # Subset Columns Needed----
  if (is.numeric(FeatureColumnNames) |
      is.integer(FeatureColumnNames)) {
    keep1 <- names(ScoringData)[c(FeatureColumnNames)]
    if (!is.null(IDcols)) {
      keep <- c(IDcols, keep1)
    } else {
      keep <- c(keep1)
    }
    ScoringData <- ScoringData[, ..keep]
  } else {
    keep1 <- c(FeatureColumnNames)
    if (!is.null(IDcols)) {
      keep <- c(IDcols, FeatureColumnNames)
    } else {
      keep <- c(FeatureColumnNames)
    }
    ScoringData <- ScoringData[, ..keep]
  }
  if (!is.null(IDcols)) {
    ScoringMerge <- data.table::copy(ScoringData)
    keep <- c(keep1)
    ScoringData <- ScoringData[, ..keep]
  } else {
    ScoringMerge <- data.table::copy(ScoringData)
  }
  
  # Initialize Catboost Data Conversion----
  if (!is.null(CatFeatures)) {
    ScoringPool <-
      catboost::catboost.load_pool(ScoringData, cat_features = CatFeatures)
  } else {
    ScoringPool <-
      catboost::catboost.load_pool(ScoringData)
  }
  
  # Load model----
  if (!is.null(ModelObject)) {
    model <- ModelObject
  } else {
    model <- tryCatch({
      catboost::catboost.load_model(paste0(ModelPath, "/", ModelID))
    },
    error = function(x)
      return("Model not found in ModelPath"))
  }
  
  # Score model----
  if (tolower(TargetType) == "regression") {
    predict <- data.table::as.data.table(
      catboost::catboost.predict(
        model = model,
        pool = ScoringPool,
        prediction_type = "RawFormulaVal",
        thread_count = -1
      )
    )
  } else if (tolower(TargetType) == "classification") {
    predict <- data.table::as.data.table(
      catboost::catboost.predict(
        model = model,
        pool = ScoringPool,
        prediction_type = "Probability",
        thread_count = -1
      )
    )
  } else if (tolower(TargetType) == "multiclass") {
    predict <- data.table::as.data.table(cbind(
      1 + catboost::catboost.predict(
        model = model,
        pool = ScoringPool,
        prediction_type = "Class"
      ),
      catboost::catboost.predict(
        model = model,
        pool = ScoringPool,
        prediction_type = "Probability"
      )
    ))
  }
  
  # Change Output Predictions Column Name----
  if (tolower(TargetType) != "multiclass") {
    data.table::setnames(predict, "V1", "Predictions")
  } else if (tolower(TargetType) == "multiclass") {
    TargetLevels <-
      data.table::fread(paste0(ModelPath, "/", ModelID, "_TargetLevels.csv"))
    k <- 1
    for (name in as.character(TargetLevels[[1]])) {
      k <- k + 1
      data.table::setnames(predict, paste0("V", k), name)
    }
    data.table::setnames(predict, "V1", "Predictions")
    predict <- merge(
      predict,
      TargetLevels,
      by.x = "Predictions",
      by.y = "NewLevels",
      all = FALSE
    )
    predict[, Predictions := OriginalLevels][, OriginalLevels := NULL]
  }
  
  # Merge features back on----
  if (ReturnFeatures) {
    predict <- cbind(predict, ScoringMerge)
  }
  
  # Garbage Collection----
  gc()
  
  # Return data----
  return(predict)
}

#' AutoXGBoostScoring is an automated scoring function that compliments the AutoCatBoost model training functions.
#'
#' AutoXGBoostScoring is an automated scoring function that compliments the AutoCatBoost model training functions. This function requires you to supply features for scoring. It will run ModelDataPrep() and the DummifyDT() function to prepare your features for xgboost data conversion and scoring.
#'
#' @family Supervised Learning
#' @param TargetType Set this value to "regression", "classification", or "multiclass" to score models built using AutoCatBoostRegression(), AutoCatBoostClassify() or AutoCatBoostMultiClass().
#' @param ScoringData This is your data.table of features for scoring. Can be a single row or batch.
#' @param FeatureColumnNames Supply either column names or column numbers used in the AutoXGBoost__() function
#' @param IDcols Supply ID column numbers for any metadata you want returned with your predicted values
#' @param ModelPath Supply your path file used in the AutoXGBoost__() function
#' @param ModelID Supply the model ID used in the AutoXGBoost__() function
#' @param ReturnFeatures Set to TRUE to return your features with the predicted values.
#' @param MDP_Impute Set to TRUE if you did so for modeling and didn't do so before supplying ScoringData in this function
#' @param MDP_CharToFactor Set to TRUE to turn your character columns to factors if you didn't do so to your ScoringData that you are supplying to this function
#' @param MDP_RemoveDates Set to TRUE if you have date of timestamp columns in your ScoringData
#' @param MDP_MissFactor If you set MDP_Impute to TRUE, supply the character values to replace missing values with
#' @param MDP_MissNum If you set MDP_Impute to TRUE, supply a numeric value to replace missing values with
#' @examples
#' \donttest{
#' Preds <- AutoXGBoostScoring(TargetType = "regression",
#'                             ScoringData = data,
#'                             FeatureColumnNames = 2:12,
#'                             IDcols = NULL,
#'                             ModelPath = "home",
#'                             ModelID = "ModelTest",
#'                             ReturnFeatures = TRUE,
#'                             MDP_Impute = TRUE,
#'                             MDP_CharToFactor = TRUE,
#'                             MDP_RemoveDates = TRUE,
#'                             MDP_MissFactor = "0",
#'                             MDP_MissNum = -1)
#' }
#' @return A data.table of predicted values with the option to return model features as well.
#' @export
AutoXGBoostScoring <- function(TargetType = NULL,
                               ScoringData = NULL,
                               FeatureColumnNames = NULL,
                               IDcols = NULL,
                               ModelPath = NULL,
                               ModelID = NULL,
                               ReturnFeatures = TRUE,
                               MDP_Impute = TRUE,
                               MDP_CharToFactor = TRUE,
                               MDP_RemoveDates = TRUE,
                               MDP_MissFactor = "0",
                               MDP_MissNum = -1) {
  # Check arguments----
  if (is.null(ScoringData)) {
    warning("ScoringData cannot be NULL")
  }
  if (is.null(FeatureColumnNames)) {
    warning("FeatureColumnNames cannot be NULL")
  }
  if (!data.table::is.data.table(ScoringData)) {
    ScoringData <- data.table::as.data.table(ScoringData)
  }
  if (!is.logical(MDP_Impute)) {
    warning("MDP_Impute (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.logical(MDP_CharToFactor)) {
    warning("MDP_CharToFactor (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.logical(MDP_RemoveDates)) {
    warning("MDP_RemoveDates (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.character(MDP_MissFactor) & !is.factor(MDP_MissFactor)) {
    warning("MDP_MissFactor should be a character or factor value")
  }
  if (!is.numeric(MDP_MissNum)) {
    warning("MDP_MissNum should be a numeric or integer value")
  }
  
  # IDcols conversion----
  if (is.numeric(IDcols) | is.integer(IDcols)) {
    IDcols <- names(data)[IDcols]
  }
  
  # Subset Columns Needed----
  if (is.numeric(FeatureColumnNames) |
      is.integer(FeatureColumnNames)) {
    keep1 <- names(ScoringData)[c(FeatureColumnNames)]
    if (!is.null(IDcols)) {
      keep <- c(IDcols, keep1)
    } else {
      keep <- c(keep1)
    }
    ScoringData <- ScoringData[, ..keep]
  } else {
    keep1 <- c(FeatureColumnNames)
    if (!is.null(IDcols)) {
      keep <- c(IDcols, FeatureColumnNames)
    } else {
      keep <- c(FeatureColumnNames)
    }
    ScoringData <- ScoringData[, ..keep]
  }
  if (!is.null(IDcols)) {
    ScoringMerge <- data.table::copy(ScoringData)
    keep <- c(keep1)
    ScoringData <- ScoringData[, ..keep]
  } else {
    ScoringMerge <- data.table::copy(ScoringData)
  }
  
  # Binary Identify column numbers for factor variables----
  CatFeatures <-
    sort(c(as.numeric(which(
      sapply(ScoringData, is.factor)
    )),
    as.numeric(which(
      sapply(ScoringData, is.character)
    ))))
  CatFeatures <- names(ScoringData)[CatFeatures]
  
  # DummifyDT categorical columns----
  ScoringData <- DummifyDT(
    data = ScoringData,
    cols = CatFeatures,
    KeepFactorCols = FALSE,
    OneHot = FALSE,
    SaveFactorLevels = FALSE,
    SavePath = ModelPath,
    ImportFactorLevels = TRUE,
    ClustScore = FALSE
  )
  
  # ModelDataPrep Check----
  ScoringData <- ModelDataPrep(
    data = ScoringData,
    Impute = MDP_Impute,
    CharToFactor = MDP_CharToFactor,
    RemoveDates = MDP_RemoveDates,
    MissFactor = MDP_MissFactor,
    MissNum = MDP_MissNum
  )
  
  # Initialize XGBoost Data Conversion----
  ScoringMatrix <-
    xgboost::xgb.DMatrix(as.matrix(ScoringData))
  
  # Load model----
  model <- xgboost::xgb.load(paste0(ModelPath, "/", ModelID))
  
  # Score model----
  predict <-
    data.table::as.data.table(stats::predict(model, ScoringMatrix))
  
  # Change Output Predictions Column Name----
  if (tolower(TargetType) != "multiclass") {
    data.table::setnames(predict, "V1", "Predictions")
  } else if (tolower(TargetType) == "multiclass") {
    TargetLevels <-
      data.table::fread(paste0(ModelPath, "/", ModelID, "_TargetLevels.csv"))
    data.table::setnames(predict, "V1", "Predictions")
    predict <- merge(
      predict,
      TargetLevels,
      by.x = "Predictions",
      by.y = "NewLevels",
      all = FALSE
    )
    predict[, Predictions := OriginalLevels][, OriginalLevels := NULL]
  }
  
  # Merge features back on----
  if (ReturnFeatures) {
    predict <- cbind(predict, ScoringMerge)
  }
  
  # Return data----
  return(predict)
}

#' AutoH2OMLScoring is an automated scoring function that compliments the AutoH2o model training functions.
#'
#' AutoH2OMLScoring is an automated scoring function that compliments the AutoH2oGBM__() and AutoH2oDRF__() models training functions. This function requires you to supply features for scoring. It will run ModelDataPrep()to prepare your features for H2O data conversion and scoring.
#'
#' @family Supervised Learning
#' @param ScoringData This is your data.table of features for scoring. Can be a single row or batch.
#' @param FeatureColumnNames Supply either column names or column numbers used in the AutoH2o__() function
#' @param ModelType Set to either "mojo" or "standard" depending on which version you saved
#' @param H2OShutdown Set to TRUE is you are scoring a "standard" model file and you aren't planning on continuing to score.
#' @param MaxMem Set to you dedicated amount of memory. E.g. "28G"
#' @param JavaOptions Change the default to your machines specification if needed. Default is '-Xmx1g -XX:ReservedCodeCacheSize=256m',
#' @param ModelPath Supply your path file used in the AutoH2o__() function
#' @param ModelID Supply the model ID used in the AutoH2o__() function
#' @param ReturnFeatures Set to TRUE to return your features with the predicted values.
#' @param MDP_Impute Set to TRUE if you did so for modeling and didn't do so before supplying ScoringData in this function
#' @param MDP_CharToFactor Set to TRUE to turn your character columns to factors if you didn't do so to your ScoringData that you are supplying to this function
#' @param MDP_RemoveDates Set to TRUE if you have date of timestamp columns in your ScoringData
#' @param MDP_MissFactor If you set MDP_Impute to TRUE, supply the character values to replace missing values with
#' @param MDP_MissNum If you set MDP_Impute to TRUE, supply a numeric value to replace missing values with
#' @examples
#' \donttest{
#' Preds <- AutoH2OMLScoring(ScoringData = data,
#'                           FeatureColumnNames = 2:12,
#'                           ModelType = "mojo",
#'                           H2OShutdown = TRUE,
#'                           MaxMem = "28G",
#'                           JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
#'                           ModelPath = NULL,
#'                           ModelID = "ModelTest",
#'                           ReturnFeatures = TRUE,
#'                           MDP_Impute = TRUE,
#'                           MDP_CharToFactor = TRUE,
#'                           MDP_RemoveDates = TRUE,
#'                           MDP_MissFactor = "0",
#'                           MDP_MissNum = -1)
#' }
#' @return A data.table of predicted values with the option to return model features as well.
#' @export
AutoH2OMLScoring <- function(ScoringData = NULL,
                             FeatureColumnNames = NULL,
                             ModelType = "mojo",
                             H2OShutdown = TRUE,
                             MaxMem = "28G",
                             JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
                             ModelPath = NULL,
                             ModelID = NULL,
                             ReturnFeatures = TRUE,
                             MDP_Impute = TRUE,
                             MDP_CharToFactor = TRUE,
                             MDP_RemoveDates = TRUE,
                             MDP_MissFactor = "0",
                             MDP_MissNum = -1) {
  # Check arguments----
  if (is.null(ScoringData)) {
    warning("ScoringData cannot be NULL")
  }
  if (is.null(FeatureColumnNames)) {
    warning("FeatureColumnNames cannot be NULL")
  }
  if (!data.table::is.data.table(ScoringData)) {
    ScoringData <- data.table::as.data.table(ScoringData)
  }
  if (!is.logical(MDP_Impute)) {
    warning("MDP_Impute (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.logical(MDP_CharToFactor)) {
    warning("MDP_CharToFactor (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.logical(MDP_RemoveDates)) {
    warning("MDP_RemoveDates (ModelDataPrep) should be TRUE or FALSE")
  }
  if (!is.character(MDP_MissFactor) & !is.factor(MDP_MissFactor)) {
    warning("MDP_MissFactor should be a character or factor value")
  }
  if (!is.numeric(MDP_MissNum)) {
    warning("MDP_MissNum should be a numeric or integer value")
  }
  
  # IDcols conversion----
  if (is.numeric(IDcols) | is.integer(IDcols)) {
    IDcols <- names(data)[IDcols]
  }
  
  # Subset Columns Needed----
  if (is.numeric(FeatureColumnNames) |
      is.integer(FeatureColumnNames)) {
    keep1 <- names(ScoringData)[c(FeatureColumnNames)]
    if (!is.null(IDcols)) {
      keep <- c(IDcols, keep1)
    } else {
      keep <- c(keep1)
    }
    ScoringData <- ScoringData[, ..keep]
  } else {
    keep1 <- c(FeatureColumnNames)
    if (!is.null(IDcols)) {
      keep <- c(IDcols, FeatureColumnNames)
    } else {
      keep <- c(FeatureColumnNames)
    }
    ScoringData <- ScoringData[, ..keep]
  }
  if (!is.null(IDcols)) {
    ScoringMerge <- data.table::copy(ScoringData)
    keep <- c(keep1)
    ScoringData <- ScoringData[, ..keep]
  } else {
    ScoringMerge <- data.table::copy(ScoringData)
  }
  
  # ModelDataPrep Check----
  ScoringData <- ModelDataPrep(
    data = ScoringData,
    Impute = MDP_Impute,
    CharToFactor = MDP_CharToFactor,
    RemoveDates = MDP_RemoveDates,
    MissFactor = MDP_MissFactor,
    MissNum = MDP_MissNum
  )
  
  # Initialize H2O Data Conversion----
  if (tolower(ModelType) != "mojo") {
    h2o::h2o.init(max_mem_size = MaxMem,
                  enable_assertions = FALSE)
    ScoreData    <- h2o::as.h2o(ScoringData)
  } else {
    ScoreData <- ScoringData
  }
  
  # Make Predictions----
  if (tolower(ModelType) == "mojo") {
    predict <- data.table::as.data.table(
      h2o::h2o.mojo_predict_df(
        frame = ScoreData,
        mojo_zip_path = file.path(ModelPath, paste0(ModelID, ".zip")),
        genmodel_jar_path = file.path(ModelPath, paste0(ModelID)),
        java_options = JavaOptions
      )
    )
    
  } else if (tolower(ModelType) == "standard") {
    model <- h2o::h2o.loadModel(path = paste0(ModelPath, "/", ModelID))
    predict <-
      data.table::as.data.table(h2o::h2o.predict(object = model,
                                                 newdata = ScoreData))
  }
  
  # Change column name----
  data.table::setnames(predict, "predict", "Predictions")
  
  # Merge features----
  if (ReturnFeatures) {
    ReturnPreds <- cbind(predict, ScoringData)
  }
  
  # Shut down H2O----
  if (tolower(ModelType) != "mojo") {
    if (H2OShutdown) {
      h2o::h2o.shutdown(prompt = FALSE)
    }
  }
  
  # Merge features back on----
  if (ReturnFeatures) {
    predict <- cbind(predict, ScoringMerge)
  }
  
  # Return data----
  return(predict)
}

#' AutoCatBoostdHurdleModel is a Retrain Function for the Regression Models for the Subsetted Data in P6
#'
#' @family Supervised Learning
#' @param data Source training data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param ValidationData Source validation data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param TestData Souce test data. Do not include a column that has the class labels for the buckets as they are created internally.
#' @param Buckets A numeric vector of the buckets used for subsetting the data. NOTE: the final Bucket value will first create a subset of data that is less than the value and a second one thereafter for data greater than the bucket value.
#' @param TargetColumnName Supply the column name or number for the target variable
#' @param FeatureColNames Supply the column names or number of the features (not included the PrimaryDateColumn)
#' @param PrimaryDateColumn Supply a date column if the data is functionally related to it
#' @param IDcols Includes PrimaryDateColumn and any other columns you want returned in the validation data with predictions
#' @param ClassWeights Utilize these for the classifier model
#' @param SplitRatios Supply vector of partition ratios. For example, c(0.70,0.20,0,10).
#' @param task_type Set to "GPU" or "CPU"
#' @param ModelID Define a character name for your models
#' @param Paths A character vector of the path file strings. EITHER SUPPLY 1 file path or N file paths for N models
#' @param SaveModelObjects Set to TRUE to save the model objects to file in the folders listed in Paths
#' @param Trees Default 15000
#' @param GridTune Set to TRUE if you want to grid tune the models
#' @param NumberModelsInGrid Set to a numeric value for the number of models to try in grid tune
#' @param NumOfParDepPlots Set to pull back N number of partial dependence calibration plots.
#' @param PassInGrid Pass in a grid for changing up the parameter settings for catboost
#' @return Returns AutoCatBoostRegression() model objects: VariableImportance.csv, Model, ValidationData.csv, EvalutionPlot.png, EvalutionBoxPlot.png, EvaluationMetrics.csv, ParDepPlots.R a named list of features with partial dependence calibration plots, ParDepBoxPlots.R, GridCollect, and catboostgrid
#' @examples
#' \donttest{
#' Output <- RemixAutoML::AutoCatBoostdHurdleModel(
#'   data,
#'   ValidationData = NULL,
#'   TestData = NULL,
#'   Buckets = c(1, 5, 10, 20),
#'   TargetColumnName = "PLND_LABOR_UNITS",
#'   FeatureColNames = 4:ncol(data),
#'   PrimaryDateColumn = "PLND_STRT_DT",
#'   IDcols = c(1,3),
#'   ClassWeights = NULL,
#'   SplitRatios = c(0.7, 0.2, 0.1),
#'   task_type = "GPU",
#'   ModelID = "P6",
#'   Paths = c(paste0(getwd(),"/P6_Buckets")),
#'   SaveModelObjects = TRUE,
#'   Trees = 5000,
#'   GridTune = FALSE,
#'   MaxModelsInGrid = 1,
#'   NumOfParDepPlots = 10,
#'   PassInGrid = grid)
#' }
#' @export
AutoCatBoostdHurdleModel <- function(data,
                                     ValidationData = NULL,
                                     TestData = NULL,
                                     Buckets = c(1, 5, 10, 20),
                                     TargetColumnName = "Target",
                                     FeatureColNames = 4:ncol(data),
                                     PrimaryDateColumn = NULL,
                                     IDcols = NULL,
                                     ClassWeights = NULL,
                                     SplitRatios = c(0.70, 0.20, 0.10),
                                     task_type = "GPU",
                                     ModelID = "ModelTest",
                                     Paths = NULL,
                                     SaveModelObjects = TRUE,
                                     Trees = 15000,
                                     GridTune = TRUE,
                                     MaxModelsInGrid = 1,
                                     NumOfParDepPlots = 10,
                                     PassInGrid = NULL) {
  # Check args----
  if (is.character(Buckets) |
      is.factor(Buckets) | is.logical(Buckets)) {
    return("Buckets needs to be a numeric scalar or vector")
  }
  if (is.null(PassInGrid)) {
    PassInGrid <- data.table::data.table(
      l2_leaf_reg = 0,
      boosting_type = "Plain",
      learning_rate = 0.01,
      bootstrap_type = "Bayesian",
      depth = 4
    )
  }
  if (!is.logical(SaveModelObjects)) {
    return("SaveModelOutput needs to be set to either TRUE or FALSE")
  }
  if (is.character(Trees) |
      is.factor(Trees) | is.logical(Trees) | length(Trees) > 1) {
    return("NumTrees needs to be a numeric scalar")
  }
  if (!is.logical(GridTune)) {
    return("GridTune needs to be either TRUE or FALSE")
  }
  if (is.character(MaxModelsInGrid) |
      is.factor(MaxModelsInGrid) |
      is.logical(MaxModelsInGrid) | length(MaxModelsInGrid) > 1) {
    return("NumberModelsInGrid needs to be a numeric scalar")
  }
  
  # Initialize collection and counter----
  ModelInformationList <- list()
  if (length(Paths) == 1) {
    Paths <- rep(Paths, length(Buckets) + 1)
  }
  
  # Data.table check----
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  if (!is.null(ValidationData)) {
    if (!data.table::is.data.table(ValidationData)) {
      ValidationData <- data.table::as.data.table(ValidationData)
    }
  }
  if (!is.null(TestData)) {
    if (!data.table::is.data.table(TestData)) {
      TestData <- data.table::as.data.table(TestData)
    }
  }
  
  # IDcols to Names----
  if (!is.null(IDcols)) {
    if (is.numeric(IDcols) | is.integer(IDcols)) {
      IDcols <- names(data)[IDcols]
    }
  }
  
  # Primary Date Column----
  if (is.numeric(PrimaryDateColumn) |
      is.integer(PrimaryDateColumn)) {
    PrimaryDateColumn <- names(data)[PrimaryDateColumn]
  }
  
  # FeatureColumnNames----
  if (is.numeric(FeatureColNames) | is.integer(FeatureColNames)) {
    FeatureNames <- names(data)[FeatureColNames]
  } else {
    FeatureNames <- FeatureColNames
  }
  
  # Add target bucket column----
  data[, Target_Buckets := as.factor(Buckets[1])]
  for (i in seq_len(length(Buckets) + 1)) {
    if (i == 1) {
      data.table::set(
        data,
        i = which(data[[eval(TargetColumnName)]] <= Buckets[i]),
        j = "Target_Buckets",
        value = as.factor(Buckets[i])
      )
    } else if (i == length(Buckets) + 1) {
      data.table::set(
        data,
        i = which(data[[eval(TargetColumnName)]] > Buckets[i -
                                                             1]),
        j = "Target_Buckets",
        value = as.factor(paste0(Buckets[i - 1], "+"))
      )
    } else {
      data.table::set(
        data,
        i = which(data[[eval(TargetColumnName)]] <= Buckets[i] &
                    data[[eval(TargetColumnName)]] > Buckets[i -
                                                               1]),
        j = "Target_Buckets",
        value = as.factor(Buckets[i])
      )
    }
  }
  
  # Add target bucket column----
  if (!is.null(ValidationData)) {
    ValidationData[, Target_Buckets := as.factor(Buckets[1])]
    for (i in seq_len(length(Buckets) + 1)) {
      if (i == 1) {
        data.table::set(
          ValidationData,
          i = which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i]),
          j = "Target_Buckets",
          value = as.factor(Buckets[i])
        )
      } else if (i == length(Buckets) + 1) {
        data.table::set(
          ValidationData,
          i = which(ValidationData[[eval(TargetColumnName)]] > Buckets[i -
                                                                         1]),
          j = "Target_Buckets",
          value = as.factor(paste0(Buckets[i - 1], "+"))
        )
      } else {
        data.table::set(
          ValidationData,
          i = which(ValidationData[[eval(TargetColumnName)]] <= Buckets[i] &
                      ValidationData[[eval(TargetColumnName)]] > Buckets[i -
                                                                           1]),
          j = "Target_Buckets",
          value = as.factor(Buckets[i])
        )
      }
    }
  }
  
  # Add target bucket column----
  if (!is.null(TestData)) {
    TestData[, Target_Buckets := as.factor(Buckets[1])]
    for (i in seq_len(length(Buckets) + 1)) {
      if (i == 1) {
        data.table::set(
          TestData,
          i = which(TestData[[eval(TargetColumnName)]] <= Buckets[i]),
          j = "Target_Buckets",
          value = as.factor(Buckets[i])
        )
      } else if (i == length(Buckets) + 1) {
        data.table::set(
          TestData,
          i = which(TestData[[eval(TargetColumnName)]] > Buckets[i -
                                                                   1]),
          j = "Target_Buckets",
          value = as.factor(paste0(Buckets[i - 1], "+"))
        )
      } else {
        data.table::set(
          TestData,
          i = which(TestData[[eval(TargetColumnName)]] <= Buckets[i] &
                      TestData[[eval(TargetColumnName)]] > Buckets[i -
                                                                     1]),
          j = "Target_Buckets",
          value = as.factor(Buckets[i])
        )
      }
    }
  }
  
  # AutoDataPartition if Validation and TestData are NULL----
  if (is.null(ValidationData) & is.null(TestData)) {
    DataSets <- AutoDataPartition(
      data = data,
      NumDataSets = 3,
      Ratios = SplitRatios,
      PartitionType = "random",
      StratifyColumnNames = "Target_Buckets",
      TimeColumnName = NULL
    )
    data <- DataSets$TrainData
    ValidationData <- DataSets$ValidationData
    TestData <- DataSets$TestData
    rm(DataSets)
  }
  
  # Begin classification model building----
  if (length(Buckets) == 2) {
    ClassifierModel <- AutoCatBoostClassifier(
      data = data,
      ValidationData = ValidationData,
      TestData = TestData,
      TargetColumnName = "Target_Buckets",
      FeatureColNames = FeatureNames,
      PrimaryDateColumn = PrimaryDateColumn,
      ClassWeights = ClassWeights,
      IDcols = IDcols,
      MaxModelsInGrid = MaxModelsInGrid,
      task_type = task_type,
      eval_metric = "AUC",
      grid_eval_metric = "auc",
      Trees = Trees,
      GridTune = GridTune,
      model_path = Paths[1],
      ModelID = ModelID,
      NumOfParDepPlots = NumOfParDepPlots,
      ReturnModelObjects = TRUE,
      SaveModelObjects = SaveModelObjects,
      PassInGrid = NULL
    )
  } else {
    ClassifierModel <- AutoCatBoostMultiClass(
      data = data,
      ValidationData = ValidationData,
      TestData = TestData,
      TargetColumnName = "Target_Buckets",
      FeatureColNames = FeatureNames,
      PrimaryDateColumn = PrimaryDateColumn,
      ClassWeights = ClassWeights,
      IDcols = IDcols,
      MaxModelsInGrid = MaxModelsInGrid,
      task_type = task_type,
      eval_metric = "MultiClass",
      grid_eval_metric = "Accuracy",
      Trees = Trees,
      GridTune = GridTune,
      model_path = Paths[1],
      ModelID = ModelID,
      ReturnModelObjects = TRUE,
      SaveModelObjects = SaveModelObjects,
      PassInGrid = NULL
    )
  }
  
  # Store metadata----
  ClassModel <- ClassifierModel$Model
  ClassEvaluationMetrics <- ClassifierModel$EvaluationMetrics
  VariableImportance <- ClassifierModel$VariableImportance
  rm(ClassifierModel)
  
  # Add Target to IDcols----
  IDcols <- c(IDcols, TargetColumnName)
  
  # Score Classification Model----
  if (length(Buckets) == 2) {
    TestData <- AutoCatBoostScoring(
      TargetType = "classification",
      ScoringData = TestData,
      FeatureColumnNames = FeatureNames,
      IDcols = IDcols,
      ModelObject = ClassModel,
      ModelPath = Paths[1],
      ModelID = ModelID,
      ReturnFeatures = TRUE,
      MDP_Impute = FALSE,
      MDP_CharToFactor = TRUE,
      MDP_RemoveDates = FALSE,
      MDP_MissFactor = "0",
      MDP_MissNum = -1
    )
  } else {
    TestData <- AutoCatBoostScoring(
      TargetType = "multiclass",
      ScoringData = TestData,
      FeatureColumnNames = FeatureNames,
      IDcols = IDcols,
      ModelObject = ClassModel,
      ModelPath = Paths[1],
      ModelID = ModelID,
      ReturnFeatures = TRUE,
      MDP_Impute = FALSE,
      MDP_CharToFactor = TRUE,
      MDP_RemoveDates = FALSE,
      MDP_MissFactor = "0",
      MDP_MissNum = -1
    )
  }
  
  # Remove Model Object----
  rm(ClassModel)
  
  # Remove Target_Buckets----
  data[, Target_Buckets := NULL]
  ValidationData[, Target_Buckets := NULL]
  
  # Remove Target From IDcols----
  IDcols <- IDcols[!(IDcols %chin% TargetColumnName)]
  
  # Change Name of Predicted MultiClass Column----
  data.table::setnames(TestData, "Predictions", "Predictions_MultiClass")
  
  # Begin regression model building----
  counter <- 0
  for (bucket in rev(seq_len(length(Buckets) + 1))) {
    # Filter By Buckets----
    if (bucket == max(seq_len(length(Buckets) + 1))) {
      if (!is.null(TestData)) {
        trainBucket <-
          data[get(TargetColumnName) > eval(Buckets[bucket - 1])]
        validBucket <-
          ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1])]
        testBucket <-
          TestData[get(TargetColumnName) > eval(Buckets[bucket - 1])]
        testBucket[, setdiff(names(testBucket), names(data)) := NULL]
      } else {
        trainBucket <-
          data[get(TargetColumnName) > eval(Buckets[bucket - 1])]
        validBucket <-
          ValidationData[get(TargetColumnName) > eval(Buckets[bucket - 1])]
        testBucket <- NULL
      }
    } else if (bucket == 1) {
      if (!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
        validBucket <-
          ValidationData[get(TargetColumnName) <= eval(Buckets[bucket])]
        testBucket <-
          TestData[get(TargetColumnName) <= eval(Buckets[bucket])]
        testBucket[, setdiff(names(testBucket), names(data)) := NULL]
      } else {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket])]
        validBucket <-
          ValidationData[get(TargetColumnName) <= eval(Buckets[bucket])]
        testBucket <- NULL
      }
    } else {
      if (!is.null(TestData)) {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) &
                              get(TargetColumnName) > eval(Buckets[bucket -
                                                                     1])]
        validBucket <-
          ValidationData[get(TargetColumnName) <= eval(Buckets[bucket]) &
                           get(TargetColumnName) > eval(Buckets[bucket -
                                                                  1])]
        testBucket <-
          TestData[get(TargetColumnName) <= eval(Buckets[bucket]) &
                     get(TargetColumnName) > eval(Buckets[bucket -
                                                            1])]
        testBucket[, setdiff(names(testBucket), names(data)) := NULL]
      } else {
        trainBucket <- data[get(TargetColumnName) <= eval(Buckets[bucket]) &
                              get(TargetColumnName) > eval(Buckets[bucket -
                                                                     1])]
        validBucket <-
          ValidationData[get(TargetColumnName) <= eval(Buckets[bucket]) &
                           get(TargetColumnName) > eval(Buckets[bucket -
                                                                  1])]
        testBucket <- NULL
      }
    }
    
    # Create Modified IDcols----
    IDcolsModified <-
      c(IDcols, setdiff(names(TestData), names(trainBucket)), TargetColumnName)
    
    # Load Winning Grid if it exists----
    if (file.exists(paste0(Paths[bucket], "/grid", Buckets[bucket], ".csv"))) {
      gridSaved <-
        data.table::fread(paste0(Paths[bucket], "/grid", Buckets[bucket], ".csv"))
    }
    
    # AutoCatBoostRegression()----
    if (trainBucket[, .N] != 0) {
      if (var(trainBucket[[eval(TargetColumnName)]]) > 0) {
        counter <- counter + 1
        if (bucket == max(seq_len(length(Buckets) + 1))) {
          TestModel <- AutoCatBoostRegression(
            data = trainBucket,
            ValidationData = validBucket,
            TestData = testBucket,
            TargetColumnName = TargetColumnName,
            FeatureColNames = FeatureNames,
            PrimaryDateColumn = PrimaryDateColumn,
            IDcols = IDcols,
            MaxModelsInGrid = MaxModelsInGrid,
            task_type = task_type,
            eval_metric = "RMSE",
            grid_eval_metric = "r2",
            Trees = Trees,
            GridTune = GridTune,
            model_path = Paths[bucket - 1],
            ModelID = paste0("P6_", bucket - 1, "+"),
            NumOfParDepPlots = NumOfParDepPlots,
            ReturnModelObjects = TRUE,
            SaveModelObjects = SaveModelObjects,
            PassInGrid = PassInGrid
          )
        } else {
          TestModel <- AutoCatBoostRegression(
            data = trainBucket,
            ValidationData = validBucket,
            TestData = testBucket,
            TargetColumnName = TargetColumnName,
            FeatureColNames = FeatureNames,
            PrimaryDateColumn = PrimaryDateColumn,
            IDcols = IDcols,
            MaxModelsInGrid = MaxModelsInGrid,
            task_type = task_type,
            eval_metric = "RMSE",
            grid_eval_metric = "r2",
            Trees = Trees,
            GridTune = GridTune,
            model_path = Paths[bucket],
            ModelID = paste0("P6_", bucket),
            NumOfParDepPlots = NumOfParDepPlots,
            ReturnModelObjects = TRUE,
            SaveModelObjects = SaveModelObjects,
            PassInGrid = PassInGrid
          )
        }
        
        # Store Model----
        RegressionModel <- TestModel$Model
        rm(TestModel)
        
        # Garbage Collection----
        gc()
        
        # Score TestData----
        if (bucket == max(seq_len(length(Buckets) + 1))) {
          TestData <- AutoCatBoostScoring(
            TargetType = "regression",
            ScoringData = TestData,
            FeatureColumnNames = FeatureNames,
            IDcols = IDcolsModified,
            Model = RegressionModel,
            ModelPath = Path[buckets - 1],
            ModelID = paste0("P6_", bucket - 1, "+"),
            ReturnFeatures = TRUE,
            MDP_Impute = TRUE,
            MDP_CharToFactor = TRUE,
            MDP_RemoveDates = FALSE,
            MDP_MissFactor = "0",
            MDP_MissNum = -1
          )
        } else {
          TestData <- AutoCatBoostScoring(
            TargetType = "regression",
            ScoringData = TestData,
            FeatureColumnNames = FeatureNames,
            IDcols = IDcolsModified,
            Model = RegressionModel,
            ModelPath = Path[buckets],
            ModelID = paste0("P6_", bucket),
            ReturnFeatures = TRUE,
            MDP_Impute = TRUE,
            MDP_CharToFactor = TRUE,
            MDP_RemoveDates = FALSE,
            MDP_MissFactor = "0",
            MDP_MissNum = -1
          )
        }
        
        # Clear TestModel From Memory----
        rm(RegressionModel)
        
        # Change prediction name to prevent duplicates----
        if (bucket == max(seq_len(length(Buckets) + 1))) {
          data.table::setnames(TestData,
                               "Predictions",
                               paste0("Predictions_", Buckets[bucket - 1], "+"))
        } else {
          data.table::setnames(TestData,
                               "Predictions",
                               paste0("Predictions_", Buckets[bucket]))
        }
      } else {
        # Use single value for predictions in the case of zero variance----
        if (bucket == max(seq_len(length(Buckets) + 1))) {
          TestData[, paste0("Predictions", Buckets[bucket - 1], "+") := Buckets[bucket]]
        } else {
          TestData[, paste0("Predictions", Buckets[bucket]) := Buckets[bucket]]
        }
      }
    }
  }
  
  # Rearrange Column order----
  data.table::setcolorder(TestData, c(2:(1 + length(IDcols)), 1, (2 + length(IDcols)):ncol(TestData)))
  data.table::setcolorder(TestData, c(
    1,
    2,
    (length(IDcols) + counter + 1),
    (length(IDcols) + counter + 1 + counter +
       1):ncol(TestData),
    (length(IDcols) + 1):(length(IDcols) +
                            counter),
    (length(IDcols) + counter + 2):(length(IDcols) + counter + 1 + counter)
  ))
  
  # Final Combination of Predictions----
  # Logic: 1 Buckets --> 4 columns of preds
  #        2 Buckets --> 6 columns of preds
  #        3 Buckets --> 8 columns of preds
  # Secondary logic: for i == 1, need to create the final column first
  #                  for i > 1, need to take the final column and add the product of the next preds
  Cols <- ncol(TestData)
  for (i in seq_len(length(Buckets) + 1)) {
    if (length(Buckets) == 1) {
      if (i == 1) {
        data.table::set(TestData,
                        j = "UpdatedPrediction",
                        value = TestData[[(Cols - (4 - i))]] *
                          TestData[[Cols - (2 - i)]])
      } else {
        data.table::set(TestData,
                        j = "UpdatedPrediction",
                        value = TestData[["UpdatedPrediction"]] +
                          TestData[[(Cols - (4 - i))]] *
                          TestData[[(Cols - (2 - i))]])
      }
    } else {
      if (i == 1) {
        data.table::set(TestData,
                        j = "UpdatedPrediction",
                        value = TestData[[(Cols - ((length(Buckets) + 1) * 2 - i))]] *
                          TestData[[(Cols - ((length(Buckets) + 1) - i))]])
      } else {
        data.table::set(TestData,
                        j = "UpdatedPrediction",
                        value = TestData[["UpdatedPrediction"]] +
                          TestData[[(Cols - ((length(Buckets) + 1) * 2 - i))]] *
                          TestData[[(Cols - ((length(Buckets) + 1) - i))]])
      }
    }
  }
  
  # Regression r2 via sqrt of correlation
  r_squared <-
    (TestData[, stats::cor(get(TargetColumnName), UpdatedPrediction)]) ^ 2
  
  # Regression Save Validation Data to File----
  if (SaveModelObjects) {
    data.table::fwrite(TestData,
                       file = paste0(Paths[1],
                                     "/",
                                     ModelID,
                                     "_ValidationData.csv"))
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationPlot <- EvalPlot(
    data = TestData,
    PredictionColName = "UpdatedPrediction",
    TargetColName = eval(TargetColumnName),
    GraphType = "calibration",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Add Number of Trees to Title
  EvaluationPlot <- EvaluationPlot +
    ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ",
                            round(r_squared, 3)))
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(Paths[1],
                           "/",
                           ModelID, "_EvaluationPlot.png"))
  }
  
  # Regression Evaluation Calibration Plot----
  EvaluationBoxPlot <- EvalPlot(
    data = TestData,
    PredictionColName = "UpdatedPrediction",
    TargetColName = eval(TargetColumnName),
    GraphType = "boxplot",
    PercentileBucket = 0.05,
    aggrfun = function(x)
      mean(x, na.rm = TRUE)
  )
  
  # Add Number of Trees to Title
  EvaluationBoxPlot <- EvaluationBoxPlot +
    ggplot2::ggtitle(paste0("Calibration Evaluation Plot: R2 = ",
                            round(r_squared, 3)))
  
  # Save plot to file
  if (SaveModelObjects) {
    ggplot2::ggsave(paste0(Paths[1],
                           "/",
                           ModelID,
                           "_EvaluationBoxPlot.png"))
  }
  
  # Regression Evaluation Metrics----
  EvaluationMetrics <-
    data.table::data.table(
      Metric = c("Poisson", "MAE",
                 "MAPE", "MSE", "MSLE",
                 "KL", "CS", "R2"),
      MetricValue = rep(999999, 8)
    )
  i <- 0
  MinVal <-
    min(TestData[, min(get(TargetColumnName))], TestData[, min(UpdatedPrediction)])
  for (metric in c("poisson", "mae", "mape", "mse", "msle", "kl", "cs", "r2")) {
    i <- as.integer(i + 1)
    tryCatch({
      # Regression Grid Evaluation Metrics----
      if (tolower(metric) == "poisson") {
        if (MinVal > 0 &
            min(TestData[["UpdatedPrediction"]], na.rm = TRUE) > 0) {
          TestData[, Metric := UpdatedPrediction - get(TargetColumnName) * log(UpdatedPrediction + 1)]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "mae") {
        TestData[, Metric := abs(get(TargetColumnName) - UpdatedPrediction)]
        Metric <- TestData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mape") {
        TestData[, Metric := abs((get(TargetColumnName) - UpdatedPrediction) / (get(TargetColumnName) + 1))]
        Metric <- TestData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "mse") {
        TestData[, Metric := (get(TargetColumnName) - UpdatedPrediction) ^ 2]
        Metric <- TestData[, mean(Metric, na.rm = TRUE)]
      } else if (tolower(metric) == "msle") {
        if (MinVal > 0 &
            min(TestData[["UpdatedPrediction"]], na.rm = TRUE) > 0) {
          TestData[, Metric := (log(get(TargetColumnName) + 1) - log(UpdatedPrediction + 1)) ^ 2]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "kl") {
        if (MinVal > 0 &
            min(TestData[["UpdatedPrediction"]], na.rm = TRUE) > 0) {
          TestData[, Metric := get(TargetColumnName) * log((get(TargetColumnName) + 1) /
                                                             (UpdatedPrediction + 1))]
          Metric <- TestData[, mean(Metric, na.rm = TRUE)]
        }
      } else if (tolower(metric) == "cs") {
        TestData[, ':=' (
          Metric1 = get(TargetColumnName) * UpdatedPrediction,
          Metric2 = get(TargetColumnName) ^ 2,
          Metric3 = UpdatedPrediction ^ 2
        )]
        Metric <-
          TestData[, sum(Metric1, na.rm = TRUE)] / (sqrt(TestData[, sum(Metric2, na.rm = TRUE)]) *
                                                      sqrt(TestData[, sum(Metric3, na.rm = TRUE)]))
      } else if (tolower(metric) == "r2") {
        TestData[, ':=' (
          Metric1 = (get(TargetColumnName) - mean(get(
            TargetColumnName
          ))) ^ 2,
          Metric2 = (get(TargetColumnName) - UpdatedPrediction) ^ 2
        )]
        Metric <-
          1 - TestData[, sum(Metric2, na.rm = TRUE)] /
          TestData[, sum(Metric1, na.rm = TRUE)]
      }
      data.table::set(
        EvaluationMetrics,
        i = i,
        j = 2L,
        value = round(Metric, 4)
      )
      data.table::set(EvaluationMetrics,
                      i = i,
                      j = 3L,
                      value = NA)
    }, error = function(x)
      "skip")
  }
  
  # Remove Cols----
  TestData[, ':=' (
    Metric = NULL,
    Metric1 = NULL,
    Metric2 = NULL,
    Metric3 = NULL
  )]
  
  # Save EvaluationMetrics to File
  EvaluationMetrics <- EvaluationMetrics[MetricValue != 999999]
  if (SaveModelObjects) {
    data.table::fwrite(EvaluationMetrics,
                       file = paste0(Paths[1],
                                     "/",
                                     ModelID, "_EvaluationMetrics.csv"))
  }
  
  # Regression Partial Dependence----
  ParDepPlots <- list()
  j <- 0
  ParDepBoxPlots <- list()
  k <- 0
  for (i in seq_len(min(length(FeatureColNames), NumOfParDepPlots))) {
    tryCatch({
      Out <- ParDepCalPlots(
        data = TestData,
        PredictionColName = "UpdatedPrediction",
        TargetColName = eval(TargetColumnName),
        IndepVar = VariableImportance[i, Variable],
        GraphType = "calibration",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      j <- j + 1
      ParDepPlots[[paste0(VariableImportance[j, Variable])]] <-
        Out
    }, error = function(x)
      "skip")
    tryCatch({
      Out1 <- ParDepCalPlots(
        data = ValidationData,
        PredictionColName = "UpdatedPrediction",
        TargetColName = eval(TargetColumnName),
        IndepVar = VariableImportance[i, Variable],
        GraphType = "boxplot",
        PercentileBucket = 0.05,
        FactLevels = 10,
        Function = function(x)
          mean(x, na.rm = TRUE)
      )
      
      k <- k + 1
      ParDepBoxPlots[[paste0(VariableImportance[k, Variable])]] <-
        Out1
    }, error = function(x)
      "skip")
  }
  
  # Regression Save ParDepBoxPlots to file----
  if (SaveModelObjects) {
    save(ParDepBoxPlots,
         file = paste0(Paths[1], "/", ModelID, "_ParDepBoxPlots.R"))
  }
  
  # Return Output----
  return(
    list(
      ClassificationMetrics = ClassEvaluationMetrics,
      FinalTestData = TestData,
      EvaluationPlot = EvaluationPlot,
      EvaluationBoxPlot = EvaluationBoxPlot,
      EvaluationMetrics = EvaluationMetrics,
      PartialDependencePlots = ParDepPlots,
      PartialDependenceBoxPlots = ParDepBoxPlots
    )
  )
}
