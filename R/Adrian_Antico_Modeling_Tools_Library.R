#' Final Data Preparation Function
#'
#' This function replaces inf values with NA, converts characters to factors, and imputes with constants
#'
#' @author Adrian Antico at RemixInstitute.com
#' @param Impute Defaults to TRUE which tells the function to impute the data
#' @param CharToFactor Defaults to TRUE which tells the function to convert characters to factors
#' @param data This is your source data you'd like to modify
#' @param MissFactor Supply the value to impute missing factor levels with
#' @param MissNum Supply  the value to impute missing numeric values with
#' @examples
#' data <- ModelDataPrep(data,
#'                       Impute = TRUE,
#'                       CharToFactor = TRUE,
#'                       MissFactor = "0",
#'                       MissNum    = -1)
#' @return Returns the original data table with corrected values
#' @export
ModelDataPrep <- function(data,
                          Impute     = TRUE,
                          CharToFactor = TRUE,
                          MissFactor = "0",
                          MissNum    = -1) {

  # Convert to data.table if not already
  if(!is.data.table(data)) data <- as.data.table(data)

  # Replace any inf values with NA
  for (col in seq_along(data)) {
    set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]),NA))
  }

  # Turn character columns into factors
  if(CharToFactor) {
    for (col in seq_along(data)) {
      if(is.character(data[[col]])) {
        set(data, j = col, value = as.factor(data[[col]]))
      }
    }
  }

  # Impute missing values
  if(Impute) {
    for (j in seq_along(data)) {
      if(is.factor(data[[j]])) {
        set(data,which(!(data[[j]] %in% levels(data[[j]]))),j,MissFactor)
      } else {
        set(data,which(is.na(data[[j]])),j,MissNum)
      }
    }
  }
  return(data)
}

#' Utility maximizing thresholds for binary classification
#'
#' This function will return the utility maximizing threshold for future predictions along with the data generated to estimate the threshold
#'
#' @author Adrian Antico at RemixInstitute.com
#' @param data data is the data table you are building the modeling on
#' @param actTar The column number where the actual target variable is located (in binary form)
#' @param predTar The column number where the predicted values are located
#' @param tpProfit This is the utility for generating a true positive prediction
#' @param tnProfit This is the utility for generating a true negative prediction
#' @param fpProfit This is the cost of generating a false positive prediction
#' @param fnProfit This is the cost of generating a false negative prediction
#' @examples
#' test <- data.table(actual = ifelse(runif(1000) > 0.5,1,0),target = runif(1000))
#' data <- threshOptim(test,
#'                     actTar   = 1,
#'                     predTar  = 2,
#'                     tpProfit = 1,
#'                     tnProfit = 5,
#'                     fpProfit = -1,
#'                     fnProfit = -1)
#' optimalThreshold <- data[[1]]
#' allResults       <- data[[2]]
#' @return Optimal threshold and corresponding utilities for the range of thresholds tested
#' @export
threshOptim <- function(data,
                        actTar   = 1,
                        predTar  = 2,
                        tpProfit = 1,
                        tnProfit = 5,
                        fpProfit = -1,
                        fnProfit = -1) {

  # Convert factor target to numeric
  data[, eval(actTar) := as.numeric(as.character(get(actTar)))]

  # Optimize each column's classification threshold ::
  popTrue <- mean(data[[(actTar)]])
  store   <- list()
  j <- 0
  options(warn = -1)
  for (i in seq(from = 0.01, to = 0.99, by = 0.01)) {
    j <- j + 1
    tp      <- sum(ifelse(data[[actTar]] == 1 & data[[predTar]] >= i, 1, 0))
    tn      <- sum(ifelse(data[[actTar]] == 0 & data[[predTar]] <  i, 1, 0))
    fp      <- sum(ifelse(data[[actTar]] == 0 & data[[predTar]] >= i, 1, 0))
    fn      <- sum(ifelse(data[[actTar]] == 1 & data[[predTar]] <  i, 1, 0))
    tpr     <- ifelse((tp+fn) == 0, 0, tp / (tp + fn))
    fpr     <- ifelse((fp+tn) == 0, 0, fp / (fp + tn))
    utility <- popTrue * (tpProfit*tpr + fnProfit*(1-tpr)) + (1-popTrue) * (fpProfit * fpr + tnProfit * (1-fpr))
    store[[j]] <- c(i, utility)
  }
  all <- rbindlist(list(store))
  utilities <- melt(all[2,])
  setnames(utilities, "value", "Utilities")
  thresholds <- melt(all[1,])
  setnames(thresholds, "value", "Thresholds")
  results <- cbind(utilities, thresholds)[,c(-1,-3)]
  thresh <- results[order(-Utilities)][1,2][[1]]
  options(warn = 1)
  return(list(thresh, results))
}

#' nlsModelFit is a function for automatically building nls models
#'
#' This function will build models for 9 different nls models, along with a non-parametric monotonic regression and a polynomial regression. The models are evaluated, a winner is picked, and the predicted values are stored in your data table.
#'
#' @author Adrian Antico at RemixInstitute.com
#' @param data Data is the data table you are building the modeling on
#' @param y Y is the target variable name in quotes
#' @param x X is the independent variable name in quotes
#' @param monotonic This is a TRUE/FALSE indicator - choose TRUE if you want monotonic regression over polynomial regression
#' @examples
#' data <- data[, value := nlsModelFit(y = "Target", x = "Variable")]
#' @return A data table with your original column replaced by the nls model predictions
#' @export
nlsModelFit <- function(data, y, x, monotonic = TRUE) {
  DATA <- data
  nls_collection <- data.table(ModelName = c("Poly", "Asymp", "AsympOff", "AsympOrig",
                                             "Biexp", "FourParmLog","Gompertz", "Logistic",
                                             "Michal_Menton", "Weilbull"),
                               Accuracy = rep(999,10))
  # Convert to data.table if not already
  if(!is.data.table(data)) data <- as.data.table(data)

  xx <- x
  yy <- y
  z <- DATA[, get(xx)][[1]]
  zz <- DATA[, get(yy)][[1]]
  tryCatch({
    if (monotonic == TRUE) {
      tryCatch({
        baseline <- monreg(z,zz,hr = 0.5, hd = 0.5)
        preds    <- baseline$estimation
        preds[preds < 0] <- 0
        val0     <- mean(abs(zz - preds))
        set(nls_collection, 1, 2, value = val0)
        #plot(preds)
      },error=function(x) {return("skip")})
    } else {
      tryCatch({
        baseline <- lm(as.formula(paste(y," ~ poly(",x,",5)", sep="")), data=DATA)
        preds    <- baseline$fitted.values
        preds[preds < 0] <- 0
        val0     <- mean(abs(zz - preds))
        set(nls_collection, 1, 2, value = val0)
        #plot(baseline)
      },error=function(x) {return("skip")})
    }
  },error=function(x) {return("skip")})
  tryCatch({
    model1 <- nls(as.formula(paste(y," ~ SSasymp(",x,", Asym, R0, lrc)", sep="")), data = DATA)
    preds1 <- fitted(model1, DATA)
    preds1[preds1 < 0] <- 0
    val    <- mean(abs(zz - preds1))
    set(nls_collection, 2, 2, value = val1)
    #plot(preds1)
  },error=function(x) {return("skip")})

  # Asymp offset model
  tryCatch({
    model2 <- nls(as.formula(paste(y," ~ SSasympOff(",x,", Asym, lrc, c0)", sep="")), data = DATA)
    preds2 <- fitted(model2, DATA)
    preds2[preds2 < 0] <- 0
    va2    <- mean(abs(zz - preds2))
    set(nls_collection, 3, 2, value = val2)
    #plot(preds2)
  },error=function(x) {return("skip")})

  # Asymp origin model
  tryCatch({
    model3 <- nls(as.formula(paste(y," ~ SSasympOrig(",x,", Asym, lrc)", sep="")), data = DATA)
    preds3 <- fitted(model3, DATA)
    preds3[preds3 < 0] <- 0
    va3    <- mean(abs(zz - preds3))
    set(nls_collection, 4, 2, value = val3)
    #plot(preds3)
  },error=function(x) {return("skip")})

  # Biexp model
  tryCatch({
    model4 <- nls(as.formula(paste(y," ~ SSbiexp(",x,", A1, lrc1, A2, lrc2)", sep="")), data = DATA)
    preds4 <- fitted(model4, DATA)
    preds4[preds4 < 0] <- 0
    val4   <- mean(abs(zz - preds4))
    set(nls_collection, 5, 2, value = val4)
    #plot(preds4)
  },error=function(x) {return("skip")})

  # Four parameter logistic model
  tryCatch({
    model5 <- nls(as.formula(paste(y," ~ SSfpl(",x,", A, B, xmid, scal)", sep="")), data = DATA)
    preds5 <- fitted(model5, DATA)
    preds5[preds5 < 0] <- 0
    val5   <- mean(abs(zz - preds5))
    set(nls_collection, 6, 2, value = val5)
    #plot(preds5)
  },error=function(x) {return("skip")})

  # Gompertz model
  tryCatch({
    model6 <- nls(as.formula(paste(y," ~ SSgompertz(",x,", Asym, b2, b3)", sep="")), data = DATA)
    preds6 <- fitted(model6, DATA)
    preds6[preds6 < 0] <- 0
    val6   <- mean(abs(zz - preds6))
    set(nls_collection, 7, 2, value = val6)
    #plot(preds6)
  },error=function(x) {return("skip")})

  # Logistic model
  tryCatch({
    model7 <- nls(as.formula(paste(y," ~ SSlogis(",x,", Asym, xmid, scal)", sep="")), data = DATA)
    preds7 <- fitted(model7, DATA)
    preds7[preds7 < 0] <- 0
    val7   <- mean(abs(zz - preds7))
    set(nls_collection, 8, 2, value = val7)
    #plot(preds7)
  },error=function(x) {return("skip")})

  # Michaelis-Menton model
  tryCatch({
    model8 <- nls(as.formula(paste(y," ~ SSmicmen(",x,", Vm, K)", sep="")), data = DATA)
    preds8 <- fitted(model8, DATA)
    preds8[preds8 < 0] <- 0
    val8   <- mean(abs(zz - preds8))
    set(nls_collection, 9, 2, value = val8)
    #plot(preds8)
  },error=function(x) {return("skip")})

  # Weibull Growth model
  tryCatch({
    model9 <- nls(as.formula(paste(y," ~ SSweibull(",x,", Asym, Drop, lrc, pwr)", sep="")), data = DATA)
    preds9 <- fitted(model9, DATA)
    preds9[preds9 < 0] <- 0
    val9   <- mean(abs(zz - preds9))
    set(nls_collection, 10, 2, value = val9)
    #plot(preds9)
  },error=function(x) {return("skip")})

  # Store best model name
  name <- nls_collection[Accuracy != 999][order(Accuracy)][1,1][[1]]

  # Create column using best model
  if(name == nls_collection[10,1][[1]]) {
    return(DATA[, eval(y) := preds9])
  } else if (name == nls_collection[2,1][[1]]) {
    return(DATA[, eval(y) := preds1])
  } else if (name == nls_collection[3,1][[1]]) {
    return(DATA[, eval(y) := preds2])
  } else if (name == nls_collection[4,1][[1]]) {
    return(DATA[, eval(y) := preds3])
  } else if (name == nls_collection[5,1][[1]]) {
    return(DATA[, eval(y) := preds4])
  } else if (name == nls_collection[6,1][[1]]) {
    return(DATA[, eval(y) := preds5])
  } else if (name == nls_collection[7,1][[1]]) {
    return(DATA[, eval(y) := preds6])
  } else if (name == nls_collection[8,1][[1]]) {
    return(DATA[, eval(y) := preds7])
  } else if (name == nls_collection[9,1][[1]]) {
    return(DATA[, eval(y) := preds8])
  } else {
    return(DATA[, eval(y) := preds])
  }
}

#' Multiplot is a function for combining multiple plots
#'
#' Sick of copying this one into your code? Well, not anymore.
#'
#' @author Adrian Antico at RemixInstitute.com
#' @param plotlist This is the list of your charts
#' @param rows This is the number of rows in your multiplot
#' @param cols This is the number of columns in your multiplot
#' @examples
#' p1 <- ggplot(data1, aes(x = weeks, y = quantity)) + geom_line()
#' p1 <- p1 + ChartTheme(BackGround = "aquamarine", OtherColor = "purple4")
#'
#' p2 <- ggplot(data2, aes(x = weeks, y = quantity)) + geom_line()
#' p2 <- p2 + ChartTheme(BackGround = "aquamarine", OtherColor = "purple4")
#'
#' p3 <- ggplot(data3, aes(x = weeks, y = quantity)) + geom_line()
#' p3 <- p3 + ChartTheme(BackGround = "aquamarine", OtherColor = "purple4")
#'
#' p4 <- ggplot(data4, aes(x = weeks, y = quantity)) + geom_line()
#' p4 <- p4 + ChartTheme(BackGround = "aquamarine", OtherColor = "purple4")
#'
#' multiplot(plotlist=list(p1,p2,p3,p4), cols = 2)
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)

  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots == 1) {
    print(plots[[1]])

  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' ChartTheme function is a ggplot theme generator for ggplots
#'
#' This function helps your ggplots look professional with the choice of the two main colors that will dominate the theme
#'
#' @author Adrian Antico at RemixInstitute.com
#' @param Background This is the color of the background on the chart
#' @param OtherColor This is the color for borders and text
#' @examples
#' p <- ggplot(data, aes(x = weeks, y = quantity)) + geom_line()
#' p <- p + ChartTheme(BackGround = "aquamarine", OtherColor = "purple4")
#' p
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
ChartTheme <- function(BackGround = "aquamarine",
                       OtherColor = "purple4",
                       Size = 0.25) {
  chart_theme <- theme(plot.background = element_rect(fill = "gray94"),
                       panel.background = element_rect(fill = BackGround, colour = OtherColor, size = 0.25, color = OtherColor),
                       panel.grid.major = element_line(colour = OtherColor, size=0.01, color = "white", linetype = 1),
                       panel.grid.minor = element_line(colour = OtherColor, size=0.01, color= "white", linetype = 1),
                       legend.position = "bottom",
                       legend.title = element_text(color = OtherColor, size=Size, face = "bold"),
                       legend.background = element_rect(fill = "gray95",size = 1, linetype = "solid", color = OtherColor),
                       plot.title=element_text(color = OtherColor, size=Size, face = "bold"),
                       axis.title=element_text(color = OtherColor, size=Size, face = "bold"),
                       axis.text=element_text(colour=OtherColor, face = "bold", angle = 90),
                       axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 20, l = 20)),
                       axis.title.y = element_text(margin = margin(t = 20, r = 20, b = 20, l = 20)),
                       panel.border = element_rect(colour = OtherColor, fill = NA, size = 1.5))
  chart_theme
}

#' Percentile rank function
#'
#' This function computes percentile ranks for each row in your data like Excel's PERCENT_RANK
#'
#' @author Adrian Antico at RemixInstitute.com
#' @param x X is your variable of interest
#' @examples
#' percRank(x)
#' @return vector of percentile ranks
#' @export
percRank <- function(x) trunc(rank(x))/length(x)

#' Function automatically builds partial dependence calibration plots for model evaluation
#'
#' This function automatically builds partial dependence calibration plots and partial dependence calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#' @author Adrian Antico at RemixInstitute.com
#'
#' @param data Data containing predicted values and actual values for comparison
#' @param PredColName String representation of the column name with predicted values from model
#' @param ActColName String representation of the column name with actual values from model
#' @param IndepVar String representation of the column name with the independent variable of choice
#' @param type Calibration or boxplot - calibration aggregated data based on summary statistic; boxplot shows variation
#' @param bucket Number of buckets to partition the space on (0,1) for evaluation
#' @param FactLevels The number of levels to show on the chart (1. Levels are chosen based on frequency; 2. all other levels grouped and labeled as "Other")
#' @return Partial dependence calibration plot or boxplot
#' @examples
#' aa <- data.table(target = runif(10000))
#' aa[, x1 := qnorm(target)]
#' aa[, x2 := runif(10000)]
#' aa[, Independent_Variable := log(pnorm(0.75 * x1 + sqrt(1-0.75^2) * qnorm(x2)))]
#' aa[, ':=' (x1 = NULL, x2 = NULL)]
#' h2o.init()
#' data <- as.h2o(aa)
#' model <- h2o.gbm(x = 2, y = 1, training_frame = data)
#' preds1 <- as.data.table(h2o.predict(model, data))
#' preds2 <- cbind(preds1, aa)
#' data <- preds2
#' output <- ParDepCalPlots(preds2,
#'                          PredColName = "predict",
#'                          ActColName  = "target",
#'                          IndepVar    = "Independent_Variable",
#'                          type        = "calibration", # c("calibration","boxplot"),
#'                          bucket      = 0.05,
#'                          Background  = "blue",
#'                          Borders     = "red")
#' @export
ParDepCalPlots <- function(data,
                           PredColName = "PredictedValues",
                           ActColName  = "ActualValues",
                           IndepVar    = "Independent_Variable_Name",
                           type        = "calibration",
                           bucket      = 0.05,
                           Background  = "lightsteelblue1",
                           Borders     = "navyblue",
                           FactLevels  = 10) {

  # Turn off ggplot2 warnings
  options(warn = -1)

  # Build buckets by independent variable of choice
  preds2 <- as.data.table(data)

  # Subset columns
  cols <- c(PredColName, ActColName, IndepVar)
  preds2 <- preds2[, ..cols]

  # Structure data
  cols <- c(PredColName, ActColName, IndepVar)
  data <- data[, ..cols]
  setcolorder(data, c(PredColName, ActColName, IndepVar))

  # If actual is in factor form, convert to numeric and coerce type to calibration
  if(!is.numeric(preds2[[ActColName]])) {
    preds2[, eval(ActColName) := as.numeric(as.character(get(ActColName)))]
    type <- "calibration"
  }

  # Prepare for both calibration and boxplot
  if (is.numeric(preds2[[IndepVar]]) || is.integer(preds2[[IndepVar]])) {
    preds2[, rank := 100*(round(percRank(preds2[[IndepVar]])/bucket)*bucket)]
  } else {
    type <- "FactorVar"
    preds2[, id := seq_len(.N), by = get(IndepVar)]
    preds2 <- preds2[, .(mean(get(ActColName), na.rm = TRUE), mean(get(PredColName), na.rm = TRUE), max(id)), by = get(IndepVar)][order(-V3)]
    if(nrow(preds2) > FactLevels) {
      temp1 <- preds2[1:FactLevels][, V3 := NULL]
      temp2 <- preds2[(FactLevels+1):nrow(preds2)]
      temp2[, ':=' (V1 = V1 * V3 / sum(V3),
                    V2 = V2 * V3 / sum(V3))]
      temp3 <- temp2[, .(sum(V1), sum(V2))]
      temp3[, get := "Other"]
      setcolorder(temp3, c(3,1,2))
    }
    preds2[, V3 := NULL]
    if(nrow(preds2) > FactLevels) {
      preds3 <- rbindlist(list(temp1, temp3))
    } else {
      preds3 <- preds2
    }
    setnames(preds3, old = c("get","V1","V2"), new = c(IndepVar, ActColName, PredColName))
    preds3 <- preds3[order(-get(PredColName))]
  }

  # Build plots
  if (type == "calibration") {
    # Aggregate by rank for calibration
    preds3 <- preds2[, lapply(.SD, mean, na.rm = TRUE), by = rank][order(rank)]
    preds3[, eval(IndepVar) := as.numeric(get(IndepVar))]

    # Partial dependence calibration plot
    plot <- ggplot(preds3, aes(x = preds3[[IndepVar]])) +
      geom_line(aes(y = preds3[[PredColName]], color = "Predicted")) +
      geom_line(aes(y = preds3[[ActColName]], color = "Actuals")) +
      ylab("Actual / Predicted") + xlab(IndepVar) +
      scale_colour_manual("",
                          breaks = c("Actuals", "Predicted"),
                          values = c("blue", "red")) +
      ChartTheme(Background, Borders, Size = 15) + ggtitle("Partial Dependence Calibration Plot")
    #plot <- plotly_build(ggplotly(plot))
  } else if (type == "boxplot"){
    # Partial dependence boxplot
    keep <- c("rank", ActColName, IndepVar)
    actual <- preds2[, ..keep]
    actual[, Type := "actual"]
    setnames(actual, ActColName, "Output")

    keep <- c("rank", PredColName, IndepVar)
    predicted <- preds2[, ..keep]
    predicted[, Type := "predicted"]
    setnames(predicted, PredColName, "Output")

    data <- rbindlist(list(actual, predicted))[order(rank)]
    data[, rank := as.factor(rank)]
    data <- data[ , eval(IndepVar) := as.numeric(get(IndepVar))]
    data <- data[ , eval(IndepVar) := round(mean(get(IndepVar), na.rm = TRUE),3), by = rank]
    data[, eval(IndepVar) := as.factor(get(IndepVar))]
    data[, rank := NULL]
    plot <- ggplot(data, aes(x = data[[IndepVar]], y = Output)) +
      geom_boxplot(aes(fill = Type)) + scale_fill_manual(values = c("red", "blue")) +
      ggtitle("Partial Dependence Calibration Boxplot") +
      xlab(eval(IndepVar)) +
      ChartTheme(Background, Borders, Size= 15)
    #plot <- plotly_build(ggplotly(plot))
  } else if (type == "FactorVar") {
    keep <- c(IndepVar, ActColName)
    actual <- preds3[, ..keep]
    actual[, Type := "actual"]
    setnames(actual, ActColName, "Output")

    keep <- c(IndepVar, PredColName)
    predicted <- preds3[, ..keep]
    predicted[, Type := "predicted"]
    setnames(predicted, PredColName, "Output")
    data <- rbindlist(list(actual, predicted))[order(-Output)]

    plot <- ggplot(data, aes(x = data[[IndepVar]], y = Output)) +
      geom_bar(stat="identity", position="dodge", aes(fill = Type)) + scale_fill_manual(values = c("red", "blue")) +
      ggtitle("Partial Dependence Calibration Barplot") +
      xlab(eval(IndepVar)) +
      ChartTheme(Background, Borders, Size= 15)
    #plot <- plotly_build(ggplotly(plot))
  }
  return(plot)
}

#' Function automatically builds calibration plots for model evaluation
#'
#' This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#' @author Adrian Antico at RemixInstitute.com
#' @param data Data containing predicted values and actual values for comparison
#' @param PredColName String representation of column name with predicted values from model
#' @param ActColName String representation of column name with actual values from model
#' @param type Calibration or boxplot - calibration aggregated data based on summary statistic; boxplot shows variation
#' @param bucket Number of buckets to partition the space on (0,1) for evaluation
#' @param aggrfun The statistics function used in aggregation, listed as a function
#' @return Calibration plot or boxplot
#' @examples
#' out1 <- EvalPlot(calibration,
#'                  PredColName = "preds",
#'                  ActColName  = "actuals",
#'                  type        = "calibration",
#'                  bucket      = 0.05,
#'                  aggrfun     = function(x) quantile(x, probs = 0.50, na.rm = TRUE))
#'
#' out2 <- EvalPlot(calibration,
#'                  PredColName = "preds",
#'                  ActColName  = "actuals",
#'                  type        = "boxplot",
#'                  bucket      = 0.05)
#' @export
EvalPlot <- function(data,
                     PredColName = "PredictedValues",
                     ActColName  = "ActualValues",
                     type        = c("calibration","boxplot"),
                     bucket      = 0.05,
                     aggrfun     = function(x) mean(x, na.rm = TRUE)) {

  # Turn data into data.table if not already
  if(!is.data.table(data)) data <- as.data.table(data)

  # Structure data
  cols <- c(PredColName, ActColName)
  data <- data[, ..cols]
  setcolorder(data, c(PredColName, ActColName))
  setnames(data, c(PredColName,ActColName), c("preds","acts"))

  # If actual is in factor form, convert to numeric and coerce type to calibration
  if(!is.numeric(data[["acts"]])) {
    data[, acts := as.numeric(as.character(acts))]
    type <- "calibration"
  }

  # Add a column that ranks predicted values
  data[, rank := 100*(round(percRank(data[[1]])/bucket)*bucket)]

  # Plot
  if(type == "boxplot") {
    # Remove classification and non-event predicted values
    data[, rank := as.factor(rank)]

    cols <- c("rank","preds")
    zz1 <- data[, ..cols]
    zz1[, Type := 'predicted']
    setnames(zz1, c("preds"), c("output"))

    cols <- c("rank","acts")
    zz2 <- data[, ..cols]
    zz2[, Type := 'actual']
    setnames(zz2, c("acts"), c("output"))

    data <- rbindlist(list(zz1,zz2))

    plot <- ggplot(data, aes(x=rank, y=output, fill=Type)) +
      geom_boxplot(outlier.color = "red", color = "black") +
      ggtitle("Calibration Evaluation Boxplot") +
      xlab("Predicted Percentile") + ylab("Observed Values") +
      ChartTheme(BackGround = "lightsteelblue1", OtherColor = "navyblue", Size= 15)

  } else {
    # Aggregate all columns by rank, utilizing mean as the aggregator statistic
    data <- data[, lapply(.SD, noquote(aggrfun)), by=rank]

    # Build calibration plot
    plot  <- ggplot(data, aes(x=rank))  +
      geom_line(aes(y=data[[3]], colour="Actual")) +
      geom_line(aes(y=data[[2]], colour="Predicted")) +
      xlab("Predicted Percentile") + ylab("Observed Values") +
      scale_color_manual(values=c("red", "blue")) +
      theme(axis.text.x=element_text(angle=90, hjust=1)) +
      theme(legend.position="bottom") +
      ggtitle("Calibration Evaluation Plot") +
      ChartTheme(BackGround = "lightsteelblue1", OtherColor = "navyblue", Size= 15)
  }
  return(plot)
}

#' An Automated Feature Engineering Function
#'
#' Builds autoregressive and rolling stats from target columns and distributed lags and distributed rolling stats for independent features distributed across time. On top of that, you can also create time between instances along with their associated lags and rolling stats. This function works for data with groups and without groups.
#' @author Adrian Antico at RemixInstitute.com
#' @param data Core instruction file for automation
#' @param lags The ceiling amount of memory H20 will utilize
#' @param statsFuns List of functions for your rolling windows, such as mean, sd, min, max, quantile
#' @param statsNames The corresponding names to append to your colnames created associated with statsFuns
#' @param targets The column(s) in which you will build your lags and rolling stats
#' @param groupingVars Categorical variables you will build your lags and rolling stats by
#' @param sortDateName String name of your core date column in your transaction data
#' @param timeDiffTarget List a name in order to create time between events with assiciated lags and rolling features
#' @param timeAgg Unit of time to aggregate by
#' @param WindowingLag   Build moving stats off of target column(s) or one of their lags (1+)
#' @param Type input "Lag" if you want features built on historical values; use "Lead" if you want features built on future values
#' @param Timer Set to TRUE if you want a time run for the operation; useful when there is grouping
#' @param SkipCols Defaults to NULL; otherwise name the vector containing the names of columns to skip
#' @param SimpleImpute Set to TRUE for factor level imputation of "0" and numeric imputation of -1
#' @return data.table of original data plus newly created features
#' @examples
#' quick_model <- GDL_Feature_Engineering(quick_model,
#'                                        lags           = c(seq(1,5,1)),
#'                                        periods        = c(3,5,10,15,20,25),
#'                                        statsFUNs      = c(function(x) quantile(x, probs = 0.0, na.rm = TRUE),
#'                                                           function(x) quantile(x, probs = 1.0, na.rm = TRUE),
#'                                                           function(x) mean(x, na.rm = TRUE),
#'                                                           function(x) sd(x, na.rm = TRUE),
#'                                                           function(x) quantile(x, probs = 0.2, na.rm = TRUE),
#'                                                           function(x) quantile(x, probs = 0.8, na.rm = TRUE)),
#'                                        statsNames     = c("min","max","mean","sd","q20","q80"),
#'                                        targets        = c("qty","price"),
#'                                        groupingVars   = c("SKU","VENDOR_NAME"),
#'                                        sortDateName   = "RECEIPT_DATE",
#'                                        timeDiffTarget = c("ISSUE_GAP"),
#'                                        timeAgg        = c("auto","secs","mins","hours","days","weeks"),
#'                                        WindowingLag   = 0,
#'                                        Type           = "Lag",
#'                                        Timer          = TRUE,
#'                                        SkipCols       = FALSE,
#'                                        SimpleImpute   = TRUE)
#' @export
GDL_Feature_Engineering <- function(data,
                                    lags           = c(seq(1,5,1)),
                                    periods        = c(3,5,10,15,20,25),
                                    statsFUNs      = c(function(x) quantile(x, probs = 0.0, na.rm = TRUE),
                                                       function(x) quantile(x, probs = 1, na.rm = TRUE),
                                                       function(x) mean(x, na.rm = TRUE),
                                                       function(x) sd(x, na.rm = TRUE),
                                                       function(x) quantile(x, probs = 0.2, na.rm = TRUE),
                                                       function(x) quantile(x, probs = 0.8, na.rm = TRUE)),
                                    statsNames     = c("min","max","mean","sd","q20","q80"),
                                    targets        = c("qty"),
                                    groupingVars   = c("Group1","Group2"),
                                    sortDateName   = "date",
                                    timeDiffTarget = c("TimeDiffName"),
                                    timeAgg        = c("auto","secs","mins","hours","days","weeks"),
                                    WindowingLag   = 0,
                                    Type           = c("Lag","Lead"),
                                    Timer          = TRUE,
                                    SkipCols       = NULL,
                                    SimpleImpute   = TRUE) {

  # Convert to data.table if not already
  if(!is.data.table(data)) data <- as.data.table(data)

  # Set up counter for countdown
  CounterIndicator = 0
  if (!is.null(timeDiffTarget)) {
    tarNum <- length(targets) + 1
  } else {
    tarNum <- length(targets)
  }

  # Define total runs
  if (!is.null(groupingVars)) {
    runs <- length(groupingVars) * length(periods) * length(statsNames) * tarNum + length(lags)
  } else {
    runs <- length(periods) * length(statsNames) * tarNum
  }

  # Begin feature engineering
  if(!is.null(groupingVars)) {
    for (i in seq_along(groupingVars)) {

      # Sort data
      if(Type == "Lag") {
        colVar <- c(groupingVars[i],sortDateName[1])
        setorderv(data, colVar, order = 1)
      } else {
        colVar <- c(groupingVars[i],sortDateName[1])
        setorderv(data, colVar, order = -1)
      }

      # Lags
      for(l in seq_along(lags)) {
        for (t in targets) {
          if(!(paste0(groupingVars[i],"_LAG_",l,"_",t) %in% SkipCols)) {
            data[, paste0(groupingVars[i],"_LAG_",l,"_",t) := data.table::shift(get(t), n = l, type = "lag"), by = get(groupingVars[i])]
          }
        }
      }

      # Time lags
      if(!is.null(timeDiffTarget)) {

        # Lag the dates first
        for(l in seq_along(lags)) {
          if(!(paste0(groupingVars[i],"TEMP",l) %in% SkipCols)) {
            data[, paste0(groupingVars[i],"TEMP",l) := data.table::shift(get(sortDateName), n = l, type = "lag"), by = get(groupingVars[i])]
          }
        }

        # Difference the lag dates
        if(WindowingLag != 0) {
          for(l in seq_along(lags)) {
            if(!(paste0(timeDiffTarget,l) %in% SkipCols) || l == 1) {
              data[, paste0(timeDiffTarget,l) := as.numeric(
                difftime(
                  get(paste0(groupingVars[i],"TEMP",(l-1))),
                  get(paste0(groupingVars[i],"TEMP",l)),
                  units = eval(timeAgg))), by = get(groupingVars[i])]
            }
          }
        } else {
          for(l in seq_along(lags)) {
            if (l == 1) {
              if(!(paste0(groupingVars[i],timeDiffTarget,l) %in% SkipCols)) {
                data[, paste0(groupingVars[i],timeDiffTarget,l) := as.numeric(
                  difftime(get(sortDateName),
                           get(paste0(groupingVars[i],"TEMP",l)),
                           units = eval(timeAgg))), by = get(groupingVars[i])]
              }
            } else {
              if(!(paste0(groupingVars[i],timeDiffTarget,l) %in% SkipCols)) {
                data[, paste0(groupingVars[i],timeDiffTarget,l) := as.numeric(
                  difftime(
                    get(paste0(groupingVars[i],"TEMP",(l-1))),
                    get(paste0(groupingVars[i],"TEMP",l)),
                    units = eval(timeAgg))), by = get(groupingVars[i])]
              }
            }
          }
        }

        # Remove temporary lagged dates
        for (l in seq_along(lags)) {
          data[, paste0(groupingVars[i],"TEMP",l) := NULL]
        }

        # Store new target
        timeTarget <- paste0(groupingVars[i],timeDiffTarget,"1")
      }

      # Define targets
      if(WindowingLag != 0) {
        if (!is.null(timeDiffTarget)) {
          targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets), timeTarget)
        } else {
          targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets))
        }
      } else {
        if (!is.null(timeDiffTarget)) {
          targets <- c(targets, timeTarget)
        } else {
          targets <- targets
        }
      }

      # Moving stats
      for (j in seq_along(periods)) {
        for (k in seq_along(statsNames)) {
          for (t in targets) {
            if(!(paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) %in% SkipCols)) {
              data[, paste0(groupingVars[i],statsNames[k],"_",periods[j],"_",t) := zoo::rollapply(get(t), periods[j], statsFUNs[k][[1]], partial = TRUE),
                   by = get(groupingVars[i])]
              CounterIndicator = CounterIndicator + 1
              if(Timer) {
                print(CounterIndicator / runs)
              }
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(data)) {
      set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]),NA))
    }

    # Turn character columns into factors
    for (col in seq_along(data)) {
      if(is.character(data[[col]])) {
        set(data, j = col, value = as.factor(data[[col]]))
      }
    }

    # Impute missing values
    if(SimpleImpute) {
      for (j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          set(data,which(!(data[[j]] %in% levels(data[[j]]))),j,"0")
        } else {
          set(data,which(is.na(data[[j]])),j,-1)
        }
      }
    }

    # Done!!
    return(data)

  } else {
    if (Type == "Lag") {
      colVar <- c(sortDateName[1])
      setorderv(data, colVar, order = 1)
    } else {
      colVar <- c(sortDateName[1])
      setorderv(data, colVar, order = -1)
    }

    # Lags
    for(l in seq_along(lags)) {
      for (t in targets) {
        if(!(paste0("LAG_",l,"_",t) %in% SkipCols)) {
          data[, paste0("LAG_",l,"_",t) := data.table::shift(get(t), n = l, type = "lag")]
        }
      }
    }

    # Time lags
    if(!is.null(timeDiffTarget)) {

      # Lag the dates first
      for(l in seq_along(lags)) {
        if(!(paste0("TEMP",l) %in% SkipCols)) {
          data[, paste0("TEMP",l) := data.table::shift(get(sortDateName), n = l, type = "lag")]
        }
      }

      # Difference the lag dates
      if(WindowingLag != 0) {
        for(l in seq_along(lags)) {
          if(!(paste0(timeDiffTarget,"_",l) %in% SkipCols)) {
            data[, paste0(timeDiffTarget,"_",l) := as.numeric(
              difftime(
                get(paste0("TEMP",(l-1))),
                get(paste0("TEMP",l)),
                units = eval(timeAgg)))]
          }
        }
      } else {
        for(l in seq_along(lags)) {
          if (l == 1) {
            if(!(paste0(timeDiffTarget,"_",l) %in% SkipCols)) {
              data[, paste0(timeDiffTarget,"_",l) := as.numeric(
                difftime(get(sortDateName),
                         get(paste0("TEMP",l)),
                         units = eval(timeAgg)))]
            }
          } else {
            if(!(paste0(timeDiffTarget,"_",l) %in% SkipCols)) {
              data[, paste0(timeDiffTarget,"_",l) := as.numeric(
                difftime(
                  get(paste0("TEMP",(l-1))),
                  get(paste0("TEMP",l)),
                  units = eval(timeAgg)))]
            }
          }
        }
      }

      # Remove temporary lagged dates
      for (l in seq_along(lags)) {
        data[, paste0("TEMP",l) := NULL]
      }

      # Store new target
      timeTarget <- paste0(timeDiffTarget,"_1")
    }

    # Define targets
    if(WindowingLag !=0) {
      if (!is.null(timeDiffTarget)) {
        targets <- c(paste0("LAG_",WindowingLag,"_",targets), timeTarget)
      } else {
        targets <- c(paste0("LAG_",WindowingLag,"_",targets))
      }
    } else {
      if(!is.null(timeDiffTarget)) {
        targets <- c(targets, timeTarget)
      } else {
        targets <- targets
      }
    }

    # Define targets
    if(WindowingLag != 0) {
      if (!is.null(timeDiffTarget)) {
        targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets), timeTarget)
      } else {
        targets <- c(paste0(groupingVars[i],"_LAG_",WindowingLag,"_",targets))
      }
    } else {
      if (!is.null(timeDiffTarget)) {
        targets <- c(targets, timeTarget)
      } else {
        targets <- targets
      }
    }

    # Moving stats
    for (j in seq_along(periods)) {
      for (k in seq_along(statsNames)) {
        for (t in targets) {
          if(!(paste0(statsNames[k],"_",periods[j],"_",t) %in% SkipCols)) {
            data[, paste0(statsNames[k],"_",periods[j],"_",t) := zoo::rollapply(get(t), periods[j], statsFUNs[k][[1]], partial = TRUE)]
            CounterIndicator = CounterIndicator + 1
            if(Timer) {
              print(CounterIndicator / runs)
            }
          }
        }
      }
    }

    # Replace any inf values with NA
    for (col in seq_along(data)) {
      set(data, j = col, value = replace(data[[col]], is.infinite(data[[col]]),NA))
    }

    # Turn character columns into factors
    for (col in seq_along(data)) {
      if(is.character(data[[col]])) {
        set(data, j = col, value = as.factor(data[[col]]))
      }
    }

    # Impute missing values
    if(SimpleImpute) {
      for (j in seq_along(data)) {
        if(is.factor(data[[j]])) {
          set(data,which(!(data[[j]] %in% levels(data[[j]]))),j,"0")
        } else {
          set(data,which(is.na(data[[j]])),j,-1)
        }
      }
    }

    # Done!!
    return(data)
  }
}

#' An Automated Machine Learning Framework using H20
#'
#' 1. Logic: Error checking in the modeling arguments from your Construction file
#' 2. ML: Build grid-tuned models and baseline models for comparison and checks which one performs better on validation data
#' 3. Evaluation: Collects the performance metrics for both
#' 4. Evaluation: Generates calibration plots (and boxplots for regression) for the winning model
#' 5. Evaluation: Generates partial dependence calibration plots (and boxplots for regression) for the winning model
#' 6. Evaluation: Generates variable importance tables and a table of non-important features
#' 7. Production: Creates a storage file containing: model name, model path, grid tune performance, baseline performance, and threshold (if classification) and stores that file in your model_path location
#' @author Adrian Antico at RemixInstitute.com
#' @param Construct Core instruction file for automation
#' @param max_memory The ceiling amount of memory H20 will utilize
#' @param ratios The percentage of train samples from source data (remainder goes to validation set)
#' @param BL_Trees The number of trees to build in baseline GBM or RandomForest
#' @param nthreads Set the number of threads to run function
#' @return Returns saved models, corrected Construct file, variable importance tables, evaluation and partial dependence calibration plots, model performance measure, etc.
#' @examples
#'aa <- data.table(target = runif(10000))
#'aa[, x1 := qnorm(target)]
#'aa[, x2 := runif(10000)]
#'aa[, Independent_Variable1 := log(pnorm(0.75 * x1 + sqrt(1-0.75^2) * qnorm(x2)))]
#'aa[, Independent_Variable2 := (pnorm(0.75 * x1 + sqrt(1-0.75^2) * qnorm(x2)))]
#'aa[, Independent_Variable3 := exp(pnorm(0.75 * x1 + sqrt(1-0.75^2) * qnorm(x2)))]
#'aa[, Independent_Variable4 := exp(exp(pnorm(0.75 * x1 + sqrt(1-0.75^2) * qnorm(x2))))]
#'aa[, Independent_Variable5 := sqrt(pnorm(0.75 * x1 + sqrt(1-0.75^2) * qnorm(x2)))]
#'aa[, Independent_Variable6 := (pnorm(0.75 * x1 + sqrt(1-0.75^2) * qnorm(x2)))^0.10]
#'aa[, Independent_Variable7 := (pnorm(0.75 * x1 + sqrt(1-0.75^2) * qnorm(x2)))^0.25]
#'aa[, Independent_Variable8 := (pnorm(0.75 * x1 + sqrt(1-0.75^2) * qnorm(x2)))^0.75]
#'aa[, Independent_Variable9 := (pnorm(0.75 * x1 + sqrt(1-0.75^2) * qnorm(x2)))^2]
#'aa[, Independent_Variable10 := (pnorm(0.75 * x1 + sqrt(1-0.75^2) * qnorm(x2)))^4]
#'aa[, ':=' (x1 = NULL, x2 = NULL)]
#'aa[, target := as.factor(ifelse(target > 0.5,1,0))]
#'Construct <- data.table(Targets         = "target",
#'                        Distribution    = "bernoulli",
#'                        Loss            = "AUC",
#'                        Quantile        = 0.01,
#'                        ModelName       = "bla",
#'                        Algoithm        = "gbm",
#'                        dataName        = "aa",
#'                        TargetCol       = c("1"),
#'                        FeatureCols     = c("2:4"),
#'                        CreateDate      = Sys.time(),
#'                        GridTune        = FALSE,
#'                        ExportValidData = TRUE,
#'                        ParDep          = 10,
#'                        PD_Data         = "All",
#'                        ThreshType      = "f1",
#'                        FSC             = 0.001,
#'                        tpProfit        = rep(0,N),
#'                        tnProfit        = rep(0,N),
#'                        fpProfit        = rep(-1,N),
#'                        fnProfit        = rep(-5,N))
#'AutoH20Modeler(Construct,
#'               max_memory = "28G",
#'               ratios = 0.75,
#'               BL_Trees = 500,
#'               nthreads = 5,
#'               model_path = getwd())
#' @export
AutoH20Modeler <- function(Construct,
                           max_memory,
                           ratios,
                           BL_Trees,
                           nthreads,
                           model_path) {

  ######################################
  # Error handling
  ######################################

  # 1. Check for errors
  # 2. Replace values with proper case values

  # ERROR PROCESS CHECKING
  # 1. Identify model type, record if not in supported model list
  # 2. Check to see if loss function is in supported loss function list for model types
  # 3. Check to see if distribution is in supported distribution list for model types
  # 4. For non-regression, check to see if distribution type corresponds to correct option set for loss functions
  # 5. For regression, check to see if distribution type corresponds to correct option set for loss functions
  # 6. For quantile regression, ensure the model is in the available model list for quantile regression
  # 7. For quantile regression, ensure chosen quantiles are within 0 and 1

  # REPLACING VALUES WITH PROPER CASE VALUES
  # 1. Store current value from Construct file
  # 2. Create data.table with current value repeated, lower case possible values, proper cased actual values
  # 3. Subset based on current value matching lower case value, and grabbing proper case value
  # 4. Replace current value for proper case value in Construct file

  ErrorCollection <- data.table(Row = rep(-720,10000), Msg = "I like modeling")
  j = 0
  for (i in 1:nrow(Construct)) {
    # Algorithm specific
    if (tolower(Construct[i,6][[1]]) %in% c("gbm","randomforest")) {

      # GBM and RF loss functions existence
      if (!(tolower(Construct[i,3][[1]]) %in% c("auto","deviance","mse", "rmse", "mae", "rmsle", "auc", "lift_top_group","misclassification", "mean_per_class_error","logloss"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = i, j = 2L, value = c(paste0("Loss function ",Construct[i,3][[1]]," is not in list: AUTO | deviance | logloss | MSE | RMSE | MAE | RMSLE | AUC | lift_top_group | misclassification | mean_per_class_error")))
      } else {
        temp <- tolower(Construct[i,3][[1]])
        lower <- c("auto","deviance", "logloss", "mse", "rmse", "mae", "rmsle", "auc", "lift_top_group","misclassification", "mean_per_class_error")
        proper <- c("AUTO","deviance","logloss","MSE","RMSE","MAE","RMSLE","AUC","lift_top_group","misclassification","mean_per_class_error")
        distMatch <- data.table(act = rep(temp, 11), LCVals = lower, Proper = proper)
        ReplaceValue <- distMatch[act == LCVals][["Proper"]][[1]]
        set(Construct, i, 3L, value = ReplaceValue)
      }

      # GBM and RF distributions
      if (!(tolower(Construct[i,2][[1]]) %in% c("auto","bernoulli","quasibinomial","multinomial","gaussian","poisson","gamma","tweedie","laplace","quantile","huber"))) {
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Distribution ",Construct[i,2][[1]]," is not in list: AUTO | bernoulli | quasibinomial | multinomial | gaussian | poisson | gamma | tweedie | laplace | quantile | huber")))
      } else {
        temp <- tolower(Construct[i,2][[1]])
        lower <- c("auto","bernoulli","quasibinomial","multinomial","gaussian","poisson","gamma","tweedie","laplace","quantile","huber")
        proper <- c("AUTO","bernoulli","quasibinomial","multinomial","gaussian","poisson","gamma","tweedie","laplace","quantile","huber")
        distMatch <- data.table(act = rep(temp, 11), LCVals = lower, Proper = proper)
        ReplaceValue2 <- distMatch[act == LCVals][["Proper"]][[1]]
        set(Construct, i, 2L, value = ReplaceValue2)
      }

      # Distribution and loss combos for non-regression
      if(tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli","multinomial") && !(tolower(Construct[i,3][[1]]) %in% c("auc","logloss","auto","lift_top_group","misclassification","mean_per_class_error"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Loss function ",Construct[i,3][[1]]," is not in list: AUC | logloss | AUTO | lift_top_group | misclassification | mean_per_class_error")))
      }

      # Distribution and loss combos for regression
      if(tolower(Construct[i,2][[1]]) %in% c("gaussian","poisson","gamma","tweedie","laplace","quantile","huber") && !(tolower(Construct[i,3][[1]]) %in% c("auto","mse", "rmse", "mae", "rmsle"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Loss function ",Construct[i,2][[1]]," is not in list: AUTO | MSE | RMSE | MAE | RMSLE")))
      }

      # Quantile Regression with GBM
      if(tolower(Construct[i,2][[1]]) %in% c("quantile") && (Construct[i,4][[1]] > 1 || Construct[i,4][[1]] < 0 || !is.numeric(Construct[i,4][[1]]))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Quantiles using ",Construct[i,6][[1]]," must be a number less than or equal to 1 AND greater than or equal to 0")))
      }

      # RF Quantile regression fail
      if(tolower(Construct[i,6][[1]]) == "randomforest" && tolower(Construct[i,2][[1]]) == "quantile") {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Quantile regression is only supported by GBM and Deeplearning models, not ",Construct[i,6][[1]]," models")))
      }

      # Quantile regression loss metrics
      if(tolower(Construct[i,2][[1]]) == "quantile" && tolower(Construct[i,3][[1]]) != "mae") {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Quantile regression is best supported by MAE when using ",Construct[i,6][[1]]," models")))
      }

    } else if (tolower(Construct[i,6][[1]]) == "deeplearning") {

      # Deeplearning loss functions
      if (!(tolower(Construct[i,3][[1]]) %in% c("automatic", "crossentropy", "quadratic","huber", "absolute", "quantile"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Loss function ",Construct[i,3][[1]]," is not in list: Automatic | CrossEntropy | Quadratic | Huber | Absolute | Quantile")))
      } else {
        temp <- tolower(Construct[i,2][[1]])
        lower <- c("auto","bernoulli","quasibinomial","multinomial","gaussian","poisson","gamma","tweedie","laplace","quantile","huber")
        proper <- c("Automatic", "CrossEntropy", "Quadratic","Huber", "Absolute", "Quantile")
        distMatch <- data.table(act = rep(temp, 11), LCVals = lower, Proper = proper)
        ReplaceVal <- distMatch[act == LCVals][["Proper"]][[1]]
        set(Construct, i, 3L, value = ReplaceVal)
      }

      # Deeplearning distributions
      if (!(tolower(Construct[i,2][[1]]) %in% c("auto", "bernoulli","multinomial", "gaussian", "poisson", "gamma", "tweedie", "laplace","quantile", "huber"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Distributions ",Construct[i,2][[1]]," is not in list: AUTO | bernoulli | multinomial | gaussian | poisson | gamma | tweedie | laplace | quantile | huber")))
      } else {
        temp <- tolower(Construct[i,2][[1]])
        lower <- c("auto", "bernoulli","multinomial", "gaussian", "poisson", "gamma", "tweedie", "laplace","quantile", "huber")
        proper <- c("AUTO", "bernoulli","multinomial", "gaussian", "poisson", "gamma", "tweedie", "laplace","quantile", "huber")
        distMatch <- data.table(act = rep(temp, 11), LCVals = lower, Proper = proper)
        ReplaceVal2 <- distMatch[act == LCVals][["Proper"]][[1]]
        set(Construct, i, 2L, value = ReplaceVal2)
      }

      # Distribution and loss combos for non-regression
      if(tolower(Construct[i,2][[1]]) %in% c("bernoulli","multinomial") && !(tolower(Construct[i,3][[1]]) %in% c("automatic", "crossentropy"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Loss function ",Construct[i,3][[1]]," is not in list: Automatic | CrossEntropy")))
      }

      # Distribution and loss combos for regression
      if(tolower(Construct[i,2][[1]]) %in% c("gaussian", "poisson", "gamma", "tweedie", "laplace","quantile", "huber") && !(tolower(Construct[i,3][[1]]) %in% c("automatic","quadratic","huber", "absolute", "quantile"))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Loss function ",Construct[i,3][[1]]," is not in list: Automatic | Quadratic | Huber | Absolute | Quantile")))
      }

      # Quantile regression loss metrics
      if(tolower(Construct[i,2][[1]]) == "quantile" && tolower(Construct[i,3][[1]]) != "quantile") {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Quantile regression needs to use Quantile for the loss function with ",Construct[i,6][[1]]," models")))
      }

      # Quantile Regression with DL
      if(tolower(Construct[i,2][[1]]) %in% c("quantile") && (Construct[i,4][[1]] > 1 || Construct[i,4][[1]] < 0 || !is.numeric(Construct[i,4][[1]]))) {
        j = j + 1
        set(ErrorCollection, i = j, j = 1L, value = i)
        set(ErrorCollection, i = j, j = 2L, value = c(paste0("Quantiles using ",Construct[i,6][[1]]," must be a number less than or equal to 1 AND greater than or equal to 0")))
      }

    } else {
      j = j + 1
      set(ErrorCollection, i = j, j = 1L, value = i)
      set(ErrorCollection, i = j, j = 2L, value = c(paste0("Models supported are: GBM, randomForest, and deeplearning, while ",Construct[i,6][[1]]," is not")))
    }

    # Ensure target column is of the correct type, but not repeatedly
    if (i > 1) {
      if (Construct[i,7][[1]] != Construct[(i-1),7][[1]]) {
        if(tolower(Construct[i,2][[1]]) %in% c("binomial","bernoulli","quasibinomial")) {
          col <- names(eval(parse(text = paste0(Construct[i,7][[1]]))))[eval(parse(text = paste0(Construct[i,8][[1]])))]
          set(eval(parse(text = noquote(paste0(Construct[i,7][[1]])))), i = i, j = col, value = factor(eval(parse(text = noquote(paste0(Construct[i,7][[1]]))))[[col]]))
        }
      }
    } else if (tolower(Construct[i,2][[1]]) %in% c("binomial","bernoulli","quasibinomial")) {
      col <- names(eval(parse(text = paste0(Construct[i,7][[1]]))))[eval(parse(text = paste0(Construct[i,8][[1]])))]
      set(eval(parse(text = noquote(paste0(Construct[i,7][[1]])))), i = i, j = col, value = factor(eval(parse(text = noquote(paste0(Construct[i,7][[1]]))))[[col]]))
    }
  }

  # Error stopping point and Construct file save
  ErrorCollection <- ErrorCollection[Row != -720]
  if(nrow(ErrorCollection) >= 1) {
    ErrorCollectionLog <<- ErrorCollection
    stop(print("Your model construction file has errors and an error log has been stored globally as 'ErrorCollectionLog'"))
  } else {
    save(Construct, file = paste0(model_path, "/Construct.Rdata"))
  }

  # Set up grid_tuned_paths.R file
  if (file.exists(paste0(model_path, "/grid_tuned_paths.Rdata"))) {
    load(paste0(model_path,"/grid_tuned_paths.Rdata"))
  } else {
    grid_tuned_paths <- data.table(Model = rep("a", nrow(Construct)),
                                   Path = rep("a", nrow(Construct)),
                                   GT_Metric = rep(1234.5678, nrow(Construct)),
                                   BL_Metric = rep(1234.5678, nrow(Construct)),
                                   BinThresh = rep(1234.5678, nrow(Construct)))
  }

  ######################################
  # Loop through model building
  ######################################

  for(i in 1:nrow(Construct)) {

    # No crossentropy stopping metric
    if(tolower(Construct[i,3][[1]]) == "crossentropy") {
      if(tolower(Construct[i,2][[1]]) == "multinomial") {
        StoppingMetric = "logloss"
      } else {
        StoppingMetric = "AUC"
      }
    } else {
      StoppingMetric = Construct[i,3][[1]]
    }

    # Define grid tune search scheme in a named list
    search_criteria  <- list(strategy             = "RandomDiscrete",
                             max_runtime_secs     = 3600,
                             max_models           = 30,
                             seed                 = 1234,
                             stopping_rounds      = 10,
                             stopping_metric      = StoppingMetric,
                             stopping_tolerance   = 1e-3)

    # Set up H20 environment instance
    Sys.sleep(10)
    h2o.init(nthreads = nthreads, max_mem_size = max_memory, enable_assertions = FALSE)
    data_h2o       <- eval(parse(text = paste0("as.h2o(",Construct[i,7][[1]],")")))

    # Keep setting
    data_train     <- h2o.splitFrame(data_h2o, ratios = ratios)
    train          <- data_train[[1]]
    validate       <- data_train[[2]]
    target         <- eval(parse(text = paste0(Construct[i,8][[1]])))
    features       <- eval(parse(text = paste0(Construct[i,9][[1]])))
    N              <- length(features)
    P5             <- 2^(-1/5)
    P4             <- 2^(-1/4)
    P3             <- 2^(-1/3)
    set(grid_tuned_paths, i = i, j = 1L, value = Construct[i,5][[1]])

    ######################################
    # Hyperparameters
    ######################################

    # 1. Check if GridTune is true
    # 2. Check to see which model is chosen
    # 3. Check to see if this is classification / multinomial or not
    # 4. Select hyperparameter list

    if (Construct[i,11][[1]]) {
      if (tolower(Construct[i,6][[1]]) == "gbm") {
        if (tolower(Construct[i,3][[1]] %in% c("auc","logloss","auto","lift_top_group","misclassification","mean_per_class_error"))) {
          hyper_params <- list(max_depth                        = seq(5,8,1),
                               balance_classes                  = c(TRUE,FALSE),
                               ntrees                           = c(100,200,300),
                               sample_rate                      = seq(0.2,1,0.01),
                               col_sample_rate                  = seq(0.2,1,0.01),
                               col_sample_rate_per_tree         = seq(0.2,1,0.01),
                               col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
                               min_rows                         = 2^seq(0,log2(eval(parse(text=paste0("nrow(",Construct[i,7][[1]],")")))*ratios[1])-1,1),
                               nbins                            = 2^seq(4,10,1),
                               nbins_cats                       = 2^seq(4,12,1),
                               min_split_improvement            = c(0,1e-8,1e-6,1e-4),
                               histogram_type                   = c("UniformAdaptive","QuantilesGlobal","RoundRobin"))
        } else {
          hyper_params <- list(max_depth                        = seq(5,8,1),
                               ntrees                           = c(100,200,300),
                               sample_rate                      = seq(0.2,1,0.01),
                               col_sample_rate                  = seq(0.2,1,0.01),
                               col_sample_rate_per_tree         = seq(0.2,1,0.01),
                               col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
                               min_rows                         = 2^seq(0,log2(eval(parse(text=paste0("nrow(",Construct[i,7][[1]],")")))*ratios[1])-1,1),
                               nbins                            = 2^seq(4,10,1),
                               nbins_cats                       = 2^seq(4,12,1),
                               min_split_improvement            = c(0,1e-8,1e-6,1e-4),
                               histogram_type                   = c("UniformAdaptive","QuantilesGlobal","RoundRobin"))
        }

      } else if (tolower(Construct[i,6][[1]]) == "deeplearning") {
        if (tolower(Construct[i,3][[1]] %in% c("automatic", "crossentropy"))) {
          hyper_params <- list(activation = c("Rectifier", "Maxout", "Tanh", "RectifierWithDropout", "MaxoutWithDropout", "TanhWithDropout"),
                               hidden              = list(c(floor(N*P5), floor(N*P5*P5), floor(N*P5*P5*P5), floor(N*P5*P5*P5*P5), floor(N*P5*P5*P5*P5*P5)),
                                                          c(floor(N*P4), floor(N*P4*P4), floor(N*P4*P4*P4), floor(N*P4*P4*P4*P4)),
                                                          c(floor(N*P3), floor(N*P3*P3), floor(N*P3*P3*P3))),
                               balance_classes     = c(TRUE,FALSE),
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
                               max_w2              = c(10, 100, 1000, 3.4028235e+38))
        } else {
          hyper_params <- list(activation = c("Rectifier", "Maxout", "Tanh", "RectifierWithDropout", "MaxoutWithDropout", "TanhWithDropout"),
                               hidden              = list(c(floor(N*P5), floor(N*P5*P5), floor(N*P5*P5*P5), floor(N*P5*P5*P5*P5), floor(N*P5*P5*P5*P5*P5)),
                                                          c(floor(N*P4), floor(N*P4*P4), floor(N*P4*P4*P4), floor(N*P4*P4*P4*P4)),
                                                          c(floor(N*P3), floor(N*P3*P3), floor(N*P3*P3*P3))),
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
                               max_w2              = c(10, 100, 1000, 3.4028235e+38))
        }
      } else if (tolower(Construct[i,6][[1]]) == "randomforest") {
        if (tolower(Construct[i,3][[1]] %in% c("auc","logloss","auto","lift_top_group","misclassification","mean_per_class_error"))) {
          hyper_params <- list(max_depth                        = seq(5,8,1),
                               balance_classes                  = c(TRUE,FALSE),
                               ntrees                           = c(250,500,1000,2000),
                               mtries                           = -1,
                               sample_rate                      = seq(0.2,1,0.05),
                               col_sample_rate_per_tree         = seq(0.2,1,0.05),
                               col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
                               min_rows                         = 2^seq(0,log2(eval(parse(text=paste0("nrow(",Construct[i,7][[1]],")")))*ratios[1])-1,1),
                               nbins                            = 2^seq(4,10,1),
                               nbins_cats                       = 2^seq(4,12,1),
                               min_split_improvement            = c(0,1e-8,1e-6,1e-4),
                               histogram_type                   = c("UniformAdaptive","QuantilesGlobal","RoundRobin"))
        } else {
          hyper_params <- list(max_depth                        = seq(5,8,1),
                               ntrees                           = c(250,500,1000,2000),
                               mtries                           = -1,
                               sample_rate                      = seq(0.2,1,0.05),
                               col_sample_rate_per_tree         = seq(0.2,1,0.05),
                               col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
                               min_rows                         = 2^seq(0,log2(eval(parse(text=paste0("nrow(",Construct[i,7][[1]],")")))*ratios[1])-1,1),
                               nbins                            = 2^seq(4,10,1),
                               nbins_cats                       = 2^seq(4,12,1),
                               min_split_improvement            = c(0,1e-8,1e-6,1e-4),
                               histogram_type                   = c("UniformAdaptive","QuantilesGlobal","RoundRobin"))
        }
      } else {
        stop("This error should not occur")
      }
    }

    ######################################
    # Grid Tune Models
    ######################################

    # Check to see if GridTune is TRUE
    # Check to see if Distribution is quantile
    # Select model

    # Grid tuned model build
    if (Construct[i,11][[1]]) {
      if(tolower(Construct[i,2][[1]]) == "quantile") {
        if(tolower(Construct[i,6][[1]]) == "gbm") {
          grid <- h2o.grid(hyper_params         = hyper_params,
                           search_criteria      = search_criteria,
                           algorithm            = Construct[i,6][[1]],
                           grid_id              = Construct[i,5][[1]],
                           x                    = features,
                           y                    = target,
                           training_frame       = train,
                           validation_frame     = validate,
                           distribution         = Construct[i,2][[1]],
                           quantile_alpha       = Construct[i,4][[1]],
                           learn_rate           = 0.05,
                           learn_rate_annealing = 0.99,
                           max_runtime_secs     = 3600,
                           stopping_rounds      = 5,
                           stopping_tolerance   = 1e-4,
                           stopping_metric      = StoppingMetric,
                           score_tree_interval  = 10,
                           seed                 = 1234)
        } else if (tolower(Construct[i,6][[1]]) == "deeplearning") {
          grid <- h2o.grid(hyper_params         = hyper_params,
                           search_criteria      = search_criteria,
                           algorithm            = Construct[i,6][[1]],
                           grid_id              = Construct[i,5][[1]],
                           x                    = features,
                           y                    = target,
                           training_frame       = train,
                           validation_frame     = validate,
                           distribution         = Construct[i,2][[1]],
                           quantile_alpha       = Construct[i,4][[1]],
                           seed                 = 42)
        }
      } else {
        if (tolower(Construct[i,6][[1]]) == "gbm") {
          grid <- h2o.grid(hyper_params         = hyper_params,
                           search_criteria      = search_criteria,
                           algorithm            = Construct[i,6][[1]],
                           grid_id              = Construct[i,5][[1]],
                           x                    = features,
                           y                    = target,
                           training_frame       = train,
                           validation_frame     = validate,
                           distribution         = Construct[i,2][[1]],
                           learn_rate           = 0.05,
                           learn_rate_annealing = 0.99,
                           max_runtime_secs     = 3600,
                           stopping_rounds      = 5,
                           stopping_tolerance   = 1e-4,
                           stopping_metric      = StoppingMetric,
                           score_tree_interval  = 10,
                           seed                 = 1234)
        } else if (tolower(Construct[i,6][[1]]) == "deeplearning") {
          grid <- h2o.grid(hyper_params         = hyper_params,
                           search_criteria      = search_criteria,
                           algorithm            = Construct[i,6][[1]],
                           grid_id              = Construct[i,5][[1]],
                           x                    = features,
                           y                    = target,
                           training_frame       = train,
                           validation_frame     = validate,
                           distribution         = Construct[i,2][[1]],
                           seed                 = 42)
        } else if (tolower(Construct[i,6][[1]]) == "randomforest") {
          grid <- h2o.grid(hyper_params         = hyper_params,
                           search_criteria      = search_criteria,
                           algorithm            = Construct[i,6][[1]],
                           grid_id              = Construct[i,5][[1]],
                           x                    = features,
                           y                    = target,
                           training_frame       = train,
                           validation_frame     = validate,
                           #distribution         = Construct[i,2][[1]],
                           max_runtime_secs     = 3600,
                           stopping_rounds      = 5,
                           stopping_tolerance   = 1e-4,
                           stopping_metric      = StoppingMetric,
                           score_tree_interval  = 10,
                           seed                 = 1234)
        }
      }

      # Store all models built sorted by metric
      if (tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli","multinomial")) {
        Decreasing = TRUE
        Grid_Out   <- h2o.getGrid(grid_id = Construct[i,5][[1]], sort_by = StoppingMetric, decreasing = Decreasing)
      } else {
        Decreasing = FALSE
        Grid_Out   <- h2o.getGrid(grid_id = Construct[i,5][[1]], sort_by = StoppingMetric, decreasing = Decreasing)
      }

      # Store best model
      best_model <- h2o.getModel(Grid_Out@model_ids[[1]])

      # Collect accuracy metric on validation data
      if(tolower(Construct[i,3][[1]]) == "crossentropy") {
        cc <- h2o.auc(h2o.performance(bl_model, valid = TRUE))
      } else {
        cc <- eval(parse(text = paste0("h2o.",tolower(StoppingMetric),"(h2o.performance(best_model, valid = TRUE))")))
      }
      # Store results in metadata file
      set(grid_tuned_paths, i = i, j = 3L, value = cc)
    }

    ######################################
    # Baseline Models
    ######################################

    # Check to see if quantile is selected
    # Choose model

    if(tolower(Construct[i,2][[1]]) == "quantile") {
      if(tolower(Construct[i,6][[1]]) == "gbm") {
        bl_model <- h2o.gbm(x                = features,
                            y                = target,
                            training_frame   = train,
                            validation_frame = validate,
                            distribution     = Construct[i,2][[1]],
                            quantile_alpha   = Construct[i,4][[1]],
                            model_id         = paste0("BL_GBM_",Construct[i,5][[1]]),
                            ntrees           = BL_Trees)
      } else if (tolower(Construct[i,6][[1]]) == "deeplearning") {
        bl_model <- h2o.deeplearning(x                = features,
                                     y                = target,
                                     hidden           = c(floor(N*P4), floor(N*P4*P4), floor(N*P4*P4*P4), floor(N*P4*P4*P4*P4)),
                                     training_frame   = train,
                                     validation_frame = validate,
                                     distribution     = Construct[i,2][[1]],
                                     model_id         = paste0("BL_DL_",Construct[i,5][[1]]),
                                     quantile_alpha   = Construct[i,4][[1]])
      }
    } else if (tolower(Construct[i,6][[1]]) == "gbm") {
      bl_model <- h2o.gbm(x                = features,
                          y                = target,
                          training_frame   = train,
                          validation_frame = validate,
                          distribution     = Construct[i,2][[1]],
                          model_id         = paste0("BL_GBM_",Construct[i,5][[1]]),
                          ntrees           = BL_Trees)
    } else if (tolower(Construct[i,6][[1]]) == "deeplearning") {
      bl_model <- h2o.deeplearning(x                = features,
                                   y                = target,
                                   hidden           = c(floor(N*P4), floor(N*P4*P4), floor(N*P4*P4*P4), floor(N*P4*P4*P4*P4)),
                                   training_frame   = train,
                                   validation_frame = validate,
                                   model_id         = paste0("BL_DL_",Construct[i,5][[1]]),
                                   distribution     = Construct[i,2][[1]])
    } else if (tolower(Construct[i,6][[1]]) == "randomforest") {
      bl_model <- h2o.randomForest(x                = features,
                                   y                = target,
                                   training_frame   = train,
                                   validation_frame = validate,
                                   #distribution     = Construct[i,2][[1]],
                                   model_id         = paste0("BL_RF_",Construct[i,5][[1]]),
                                   ntrees           = BL_Trees)
    }

    # Collect accuracy metric on validation data
    if(tolower(Construct[i,3][[1]]) == "crossentropy") {
      if(tolower(Construct[i,2][[1]]) == "multinomial") {
        dd <- h2o.logloss(h2o.performance(bl_model, valid = TRUE))
      } else {
        dd <- h2o.auc(h2o.performance(bl_model, valid = TRUE))
      }
    } else {
      dd <- eval(parse(text = paste0("h2o.", tolower(StoppingMetric), "(h2o.performance(bl_model, valid = TRUE))")))
    }

    # Store results in metadata file
    set(grid_tuned_paths, i = i, j = 4L, value = dd)

    ######################################
    # Model Evaluation & Saving
    ######################################

    # Check to see if GridTune is TRUE
    # Check to see if Distribution is multinomial
    # Proceed

    if (Construct[i,11][[1]]) {
      if(!(tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli"))) {
        if(cc < dd) {
          # Save model
          if(grid_tuned_paths[i,2][[1]] != "a") file.remove(grid_tuned_paths[i,2][[1]])
          save_model <- h2o.saveModel(object = best_model, path = model_path, force = TRUE)

          # Save info
          set(grid_tuned_paths, i = i, j = 2L, value = save_model)
          save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))

          # Save VarImp and VarNOTImp
          VIMP <- as.data.table(h2o.varimp(best_model))
          save(VIMP, file = paste0(model_path, "/VarImp_", Construct[i,5][[1]],".Rdata"))
          NIF <- VIMP[percentage < Construct[i,16][[1]], 1][[1]]
          if (length(NIF) > 0) {
            save(NIF, file = paste0(model_path, "/VarNOTImp_", Construct[i,5][[1]],".Rdata"))
          }

          # Gather predicted values
          preds <- h2o.predict(best_model, newdata = validate)[,1]
          if(Construct[i,14][[1]] == "All") {
            predsPD <- h2o.predict(best_model, newdata = data_h2o)[,1]
          } else if (Construct[i,14][[1]] == "Train") {
            predsPD <- h2o.predict(best_model, newdata = train)[,1]
          } else if (Construct[i,14][[1]] == "Validate") {
            predsPD <- h2o.predict(best_model, newdata = validate)[,1]
          }
        } else {
          # Save model
          if(grid_tuned_paths[i,2][[1]] != "a") file.remove(grid_tuned_paths[i,2][[1]])
          save_model <- h2o.saveModel(object = bl_model, path = model_path, force = TRUE)

          # Save info
          set(grid_tuned_paths, i = i, j = 2L, value = save_model)
          save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))

          # Save VarImp
          VIMP <- as.data.table(h2o.varimp(bl_model))
          save(VIMP, file = paste0(model_path, "/VarImp_", Construct[i,5][[1]], ".Rdata"))
          NIF <- VIMP[percentage < Construct[i,16][[1]], 1][[1]]
          if (length(NIF) > 0) {
            save(NIF, file = paste0(model_path, "/VarNOTImp_", Construct[i,5][[1]],".Rdata"))
          }

          # Gather predicted values
          preds <- h2o.predict(bl_model, newdata = validate)[,1]
          if(Construct[i,14][[1]] == "All") {
            predsPD <- h2o.predict(bl_model, newdata = data_h2o)[,1]
          } else if (Construct[i,14][[1]] == "Train") {
            predsPD <- h2o.predict(bl_model, newdata = train)[,1]
          } else if (Construct[i,14][[1]] == "Validate") {
            predsPD <- h2o.predict(bl_model, newdata = validate)[,1]
          }
        }
      } else {
        if(cc > dd) {
          # Save model
          if(grid_tuned_paths[i,2][[1]] != "a") file.remove(grid_tuned_paths[i,2][[1]])
          save_model <- h2o.saveModel(object = best_model, path = model_path, force = TRUE)

          # Store threshold
          store_results <- data.table(best_model@model$training_metrics@metrics$thresholds_and_metric_scores)
          if (Construct[i,15][[1]] == "f1" || is.null(Construct[i,15][[1]])) {
            Thresh <<- store_results[order(-f1)][1,1][[1]]
            Label  <<- "f1"
          } else if (Construct[i,15][[1]] == "f2") {
            Thresh <<- store_results[order(-f2)][1,1][[1]]
            Label  <<- "f2"
          } else if (Construct[i,15][[1]] == "f0point5") {
            Thresh <<- store_results[order(-f0point5)][1,1][[1]]
            Label <<- "f0point5"
          } else if (Construct[i,15][[1]] == "CS") {
            predsPDD <- h2o.predict(bl_model, newdata = data_h2o)[,3]
            data    <- as.data.table(h2o.cbind(data_h2o, predsPDD))
            data[, eval(Construct[i,1][[1]]) := as.numeric(as.character(get(Construct[i,1][[1]])))]
            temp  <- threshOptim(data     = data,
                                 actTar   = Construct[i,1][[1]],
                                 predTar  = 'p1',
                                 tpProfit = Construct[i,17][[1]],
                                 tnProfit = Construct[i,18][[1]],
                                 fpProfit = Construct[i,19][[1]],
                                 fnProfit = Construct[i,20][[1]])
            Thresh <<- temp[[1]]
            Label <<- "CS"
          }
          set(grid_tuned_paths, i = i, j = 5L, value = Thresh)

          # Save info
          set(grid_tuned_paths, i = i, j = 2L, value = save_model)
          save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))

          # Save VarImp
          VIMP <- as.data.table(h2o.varimp(best_model))
          save(VIMP, file = paste0(model_path, "/VarImp_", Construct[i,5][[1]],".Rdata"))
          NIF <- VIMP[percentage < Construct[i,16][[1]], 1][[1]]
          if (length(NIF) > 0) {
            save(NIF, file = paste0(model_path, "/VarNOTImp_", Construct[i,5][[1]],".Rdata"))
          }

          # Gather predicted values
          preds <- h2o.predict(best_model, newdata = validate)[,3]
          if(Construct[i,14][[1]] == "All") {
            predsPD <- h2o.predict(best_model, newdata = data_h2o)[,3]
          } else if (Construct[i,14][[1]] == "Train") {
            predsPD <- h2o.predict(best_model, newdata = train)[,3]
          } else if (Construct[i,14][[1]] == "Validate") {
            predsPD <- h2o.predict(best_model, newdata = validate)[,3]
          }
        } else {
          # Save model
          if(grid_tuned_paths[i,2][[1]] != "a") file.remove(grid_tuned_paths[i,2][[1]])
          save_model <- h2o.saveModel(object = bl_model, path = model_path, force = TRUE)

          # Store threshold
          store_results <- data.table(bl_model@model$training_metrics@metrics$thresholds_and_metric_scores)
          if (Construct[i,15][[1]] == "f1" || is.null(Construct[i,15][[1]])) {
            Thresh <<- store_results[order(-f1)][1,1][[1]]
            Label  <<- "f1"
          } else if (Construct[i,15][[1]] == "f2") {
            Thresh <<- store_results[order(-f2)][1,1][[1]]
            Label  <<- "f2"
          } else if (Construct[i,15][[1]] == "f0point5") {
            Thresh <<- store_results[order(-f0point5)][1,1][[1]]
            Label <<- "f0point5"
          } else if (Construct[i,15][[1]] == "CS") {
            predsPDD <- h2o.predict(bl_model, newdata = data_h2o)[,3]
            data    <- as.data.table(h2o.cbind(data_h2o, predsPDD))
            data[, eval(Construct[i,1][[1]]) := as.numeric(as.character(get(Construct[i,1][[1]])))]
            temp  <- threshOptim(data     = data,
                                 actTar   = Construct[i,1][[1]],
                                 predTar  = 'p1',
                                 tpProfit = Construct[i,17][[1]],
                                 tnProfit = Construct[i,18][[1]],
                                 fpProfit = Construct[i,19][[1]],
                                 fnProfit = Construct[i,20][[1]])
            Thresh <<- temp[[1]]
            Label <<- "CS"
          }
          set(grid_tuned_paths, i = i, j = 5, value = Thresh)

          # Save info
          set(grid_tuned_paths, i = i, j = 2L, value = save_model)
          save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))

          # Save VarImp
          VIMP <- as.data.table(h2o.varimp(bl_model))
          save(VIMP, file = paste0(model_path, "/VarImp_", Construct[i,5][[1]], ".Rdata"))
          NIF <- VIMP[percentage < Construct[i,16][[1]], 1][[1]]
          if (length(NIF) > 0) {
            save(NIF, file = paste0(model_path, "/VarNOTImp_", Construct[i,5][[1]],".Rdata"))
          }

          # Gather predicted values
          preds <- h2o.predict(bl_model, newdata = validate)[,3]
          if(Construct[i,14][[1]] == "All") {
            predsPD <- h2o.predict(bl_model, newdata = data_h2o)[,3]
          } else if (Construct[i,14][[1]] == "Train") {
            predsPD <- h2o.predict(bl_model, newdata = train)[,3]
          } else if (Construct[i,14][[1]] == "Validate") {
            predsPD <- h2o.predict(bl_model, newdata = validate)[,3]
          }
        }
      }
    } else {
      # Save model
      if(grid_tuned_paths[i,2][[1]] != "a") file.remove(grid_tuned_paths[i,2][[1]])
      save_model <- h2o.saveModel(object = bl_model, path = model_path, force = TRUE)

      # Store threshold for binary classification
      if(tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli")) {
        store_results <- data.table(bl_model@model$training_metrics@metrics$thresholds_and_metric_scores)
        if (Construct[i,15][[1]] == "f1" || is.null(Construct[i,15][[1]])) {
          Thresh <<- store_results[order(-f1)][1,1][[1]]
          Label  <<- "f1"
        } else if (Construct[i,15][[1]] == "f2") {
          Thresh <<- store_results[order(-f2)][1,1][[1]]
          Label  <<- "f2"
        } else if (Construct[i,15][[1]] == "f0point5") {
          Thresh <<- store_results[order(-f0point5)][1,1][[1]]
          Label <<- "f0point5"
        } else if (Construct[i,15][[1]] == "CS") {
          predsPDD <- h2o.predict(bl_model, newdata = data_h2o)[,3]
          data    <- as.data.table(h2o.cbind(data_h2o, predsPDD))
          data[, eval(Construct[i,1][[1]]) := as.numeric(as.character(get(Construct[i,1][[1]])))]
          temp  <- threshOptim(data     = data,
                               actTar   = Construct[i,1][[1]],
                               predTar  = 'p1',
                               tpProfit = Construct[i,17][[1]],
                               tnProfit = Construct[i,18][[1]],
                               fpProfit = Construct[i,19][[1]],
                               fnProfit = Construct[i,20][[1]])
          Thresh <<- temp[[1]]
          Label <<- "CS"
        }
        set(grid_tuned_paths, i = i, j = 5L, value = Thresh)
        preds <- h2o.predict(bl_model, newdata = validate)[,3]
        if(tolower(Construct[i,14][[1]]) == "all") {
          predsPD <- h2o.predict(bl_model, newdata = data_h2o)[,3]
        } else if (tolower(Construct[i,14][[1]]) == "train") {
          predsPD <- h2o.predict(bl_model, newdata = train)[,3]
        } else if (tolower(Construct[i,14][[1]]) == "validate") {
          predsPD <- h2o.predict(bl_model, newdata = validate)[,3]
        }
      } else {
        # Store predicted values against validate data for calibration plot
        preds <- h2o.predict(bl_model, newdata = validate)[,1]
        if(tolower(Construct[i,14][[1]]) == "all") {
          predsPD <- h2o.predict(bl_model, newdata = data_h2o)[,1]
        } else if (tolower(Construct[i,14][[1]]) == "train") {
          predsPD <- h2o.predict(bl_model, newdata = train)[,1]
        } else if (tolower(Construct[i,14][[1]]) == "validate") {
          predsPD <- h2o.predict(bl_model, newdata = validate)[,1]
        }
      }

      # Save info
      set(grid_tuned_paths, i = i, j = 2L, value = save_model)
      save(grid_tuned_paths, file = paste0(model_path, "/grid_tuned_paths.Rdata"))

      # Save VarImp
      VIMP <- as.data.table(h2o.varimp(bl_model))
      save(VIMP, file = paste0(model_path, "/VarImp_", Construct[i,5][[1]], ".Rdata"))
      NIF <- VIMP[percentage < Construct[i,16][[1]], 1][[1]]
      if (length(NIF) > 0) {
        save(NIF, file = paste0(model_path, "/VarNOTImp_", Construct[i,5][[1]],".Rdata"))
      }
    }

    ######################################
    # Model Evaluation Plots
    ######################################

    # Generate plots
    col <- Construct[i,1][[1]]
    calibration <- as.data.table(h2o.cbind(preds, validate[, col]))
    if (tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli")) {
      calibration[, eval(col) := as.numeric(as.character(get(col)))]
    }
    if(Construct[i,13][[1]] >= 1) {
      if (tolower(Construct[i,14][[1]]) == "all") {
        calibEval <- as.data.table(h2o.cbind(preds, validate))
        calib <- as.data.table(h2o.cbind(predsPD, data_h2o))
      } else if (tolower(Construct[i,14][[1]]) == "train") {
        calibEval <- as.data.table(h2o.cbind(preds, validate))
        calib <- as.data.table(h2o.cbind(predsPD, train))
      } else if (tolower(Construct[i,14][[1]]) == "validate") {
        calibEval <- as.data.table(h2o.cbind(preds, validate))
        calib <- as.data.table(h2o.cbind(predsPD, validate))
      }
      if(Construct[i,12][[1]]) {
        save(calibEval, file = paste0(model_path,"/",Construct[i,5][[1]],".Rdata"))
      }
    } else {
      if(Construct[i,12][[1]]) {
        calibEval <- as.data.table(h2o.cbind(preds, validate))
        save(calibEval, file = paste0(model_path,"/",Construct[i,5][[1]],".Rdata"))
      }
    }
    predName <- names(calibration[,1])

    # Generate evaluation plots
    if (tolower(Construct[i,2][[1]]) != "multinomial") {
      if (tolower(Construct[i,2][[1]]) == "quantile") {

        # Calibration plot
        out1 <- EvalPlot(calibration,
                         PredColName = predName,
                         ActColName  = Construct[i,1][[1]],
                         type        = "calibration",
                         bucket      = 0.05,
                         aggrfun     = function(x) quantile(x, probs = Construct[i,4][[1]], na.rm = TRUE))
        ggsave(paste0(model_path, "/CalP_", Construct[i,5][[1]], ".png"))

        # Calibration boxplot
        out2 <- EvalPlot(calibration,
                         PredColName = predName,
                         ActColName  = Construct[i,1][[1]],
                         type        = "boxplot",
                         bucket      = 0.05)
        ggsave(paste0(model_path, "/CalBP_", Construct[i,5][[1]], ".png"))
      } else if (tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli")) {

        # Calibration plot
        #interc <<- mean(calibration[[eval(predName)]], na.rm = TRUE)
        out1 <- EvalPlot(calibration,
                         PredColName = predName,
                         ActColName  = Construct[i,1][[1]],
                         type        = "calibration",
                         bucket      = 0.05,
                         aggrfun     = function(x) mean(x, na.rm = TRUE))

        out1 <- out1 + geom_hline(yintercept = Thresh)
        out1 <- out1 #+ geom_text(aes(x = interc, y = Thresh, label = Label, hjust = 1.75, angle = 90))
        ggsave(paste0(model_path, "/CalP_", Construct[i,5][[1]], ".png"))
      } else {
        # Calibration plot
        out1 <- EvalPlot(calibration,
                         PredColName = predName,
                         ActColName  = Construct[i,1][[1]],
                         type        = "calibration",
                         bucket      = 0.05,
                         aggrfun     = function(x) mean(x, na.rm = TRUE))
        ggsave(paste0(model_path, "/CalP_", Construct[i,5][[1]], ".png"))

        # Calibration boxplot
        out2 <- EvalPlot(calibration,
                         PredColName = predName,
                         ActColName  = Construct[i,1][[1]],
                         type        = "boxplot",
                         bucket      = 0.05)
        ggsave(paste0(model_path, "/CalBP_", Construct[i,5][[1]], ".png"))
      }
    } else {
      # Multinomial case
      # Stack each level's predicted values and actual values
      if (Construct[i,11][[1]] && cc <= dd) {
        predsMulti <- h2o.predict(best_model, newdata = validate)
        col <- Construct[i,1][[1]]
        xx <- as.data.table(h2o.cbind(validate[, col],predsMulti))
        if(Construct[i,12][[1]]) {
          calib <- as.data.table(h2o.cbind(validate, preds))
          save(calib, file = paste0(model_path,"/",Construct[i,5][[1]],".Rdata"))
        }
        N <- (ncol(xx) - 2)
        data <- eval(parse(text = Construct[i,7][[1]]))
        for (lev in levels(data[[Construct[i,1][[1]]]])) {
          xx[, paste0("V",lev) := ifelse(xx[[1]] %in% lev,1,0)]
        }
        RemoveCols <- names(xx)[1:2]
        KeepCols   <- names(xx)[3:length(names(xx))]
        xx[, (RemoveCols) := NULL]
        store <- list()
        for (k in 1:N) {
          j = k + N
          temp <- cbind(xx[,..k],xx[,..j])
          setnames(temp, KeepCols[k], "Preds")
          setnames(temp, KeepCols[j], "Act")
          store[[k]] <- temp
        }
        xxx <- rbindlist(store)

        # Calibration plot
        out1 <- EvalPlot(xxx,
                         PredColName = "Preds",
                         ActColName  = "Act",
                         type        = "calibration",
                         bucket      = 0.05,
                         aggrfun     = function(x) mean(x, na.rm = TRUE))
        ggsave(paste0(model_path, "/CalP_", Construct[i,5][[1]], ".png"))
      } else {
        predsMulti <- h2o.predict(bl_model, newdata = validate)
        col <- Construct[i,1][[1]]
        xx <- as.data.table(h2o.cbind(validate[, col],predsMulti))
        if(Construct[i,12][[1]]) {
          calib <- as.data.table(h2o.cbind(validate, preds))
          save(calib, file = paste0(model_path,"/",Construct[i,5][[1]],".Rdata"))
        }
        N <- (ncol(xx) - 2)
        data <- eval(parse(text = Construct[i,7][[1]]))
        for (lev in levels(data[[Construct[i,1][[1]]]])) {
          xx[, paste0("V",lev) := ifelse(xx[[1]] %in% lev,1,0)]
        }
        RemoveCols <- names(xx)[1:2]
        KeepCols   <- names(xx)[3:length(names(xx))]
        xx[, (RemoveCols) := NULL]
        store <- list()
        for (k in 1:N) {
          j = k + N
          temp <- cbind(xx[,..k],xx[,..j])
          setnames(temp, KeepCols[k], "Preds")
          setnames(temp, KeepCols[j], "Act")
          store[[k]] <- temp
        }
        xxx <- rbindlist(store)

        # Calibration plot
        out1 <- EvalPlot(xxx,
                         PredColName = "Preds",
                         ActColName  = "Act",
                         type        = "calibration",
                         bucket      = 0.05,
                         aggrfun     = function(x) mean(x, na.rm = TRUE))
        ggsave(paste0(model_path, "/CalP_", Construct[i,5][[1]], ".png"))
      }

      # IF WE WANT MULTINOMIAL AUC THE BELOW CAN GET YOU THERE
      # if (Construct[i,11][[1]]) {
      #   xx <- as.data.table(h2o.cbind(validate[,1],h2o.predict(best_model, newdata = validate)))
      #   xx[, predict := as.character(predict)]
      #   xx[, vals := 0.5]
      #   z <- ncol(xx)
      #   col <- Construct[i,1][[1]]
      #   for (l in 1:nrow(xx)) {
      #     cols <- xx[l, get(col)][[1]]
      #     valss <- xx[l, ..cols][[1]]
      #     set(xx, l, j = z, value = valss)
      #   }
      #   aucM <- round(as.numeric(noquote(stringr::str_extract(pROC::multiclass.roc(xx$target, xx$vals)$auc, "\\d+\\.*\\d*"))),4)
      #   set(grid_tuned_paths, i = i, j = 4, value = aucM)
      # } else {
      #   xx <- as.data.table(h2o.cbind(validate[,1],h2o.predict(bl_model, newdata = validate)))
      #   xx[, predict := as.character(predict)]
      #   xx[, vals := 0.5]
      #   z <- ncol(xx)
      #   col <- Construct[i,1][[1]]
      #   for (l in 1:nrow(xx)) {
      #     cols <- xx[l, get(col)][[1]]
      #     valss <- xx[l, ..cols][[1]]
      #     set(xx, l, j = z, value = valss)
      #   }
      #   aucM <- round(as.numeric(noquote(stringr::str_extract(pROC::multiclass.roc(xx$target, xx$vals)$auc, "\\d+\\.*\\d*"))),4)
      #   set(grid_tuned_paths, i = i, j = 4, value = aucM)
      # }
    }

    # Partial dependence calibration plots
    if(Construct[i,13][[1]] >= 1) {
      cols <- VIMP[1:Construct[i,13][[1]], 1][[1]]
      calibr <- list()
      boxplotr <- list()
      j <- 0
      if(!(tolower(Construct[i,2][[1]]) %in% c("multinomial"))) {
        for (col in cols) {
          j <- j + 1
          #if (tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli")) {
          #interc <<- mean(calib[[eval(col)]], na.rm = TRUE)
          #}
          out1 <- ParDepCalPlots(calib,
                                 PredColName = predName,
                                 ActColName  = Construct[i,1][[1]],
                                 IndepVar    = col,
                                 type        = "calibration",
                                 bucket      = 0.05,
                                 Background  = "lightsteelblue1",
                                 Borders     = "navyblue",
                                 FactLevels  = 10)
          if (tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli")) {
            out1 <- out1 + geom_hline(yintercept = Thresh)
            out1 <- out1 #+ geom_text(aes(x = interc, y = Thresh, label = Label, hjust = 1.75, angle = 90))
            calibr[[j]] <- out1
          } else {
            calibr[[j]] <- out1
          }

          # If not regression do not do
          if (!(tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli"))) {
            boxplotr[[j]] <- ParDepCalPlots(calib,
                                            PredColName = predName,
                                            ActColName  = Construct[i,1][[1]],
                                            IndepVar    = col,
                                            type        = "boxplot",
                                            bucket      = 0.05,
                                            Background  = "lightsteelblue1",
                                            Borders     = "navyblue",
                                            FactLevels  = 10)
          }
        }

        # Save output
        if (!(tolower(Construct[i,2][[1]]) %in% c("quasibinomial","binomial","bernoulli"))) {
          save(boxplotr, file = paste0(model_path,"/",Construct[i,5][[1]],"_ParDepCalBoxPlots.Rdata"))
        }
        save(calibr, file = paste0(model_path,"/",Construct[i,5][[1]],"_ParDepCalPlots.Rdata"))
      }
    }

    # Clear H20 environment between runs
    h2o.rm(data_h2o)
    h2o.rm(data_train)
    h2o.rm(train)
    h2o.rm(validate)
    if (Construct[i,11][[1]]) {
      h2o.rm(best_model)
    }
    h2o.rm(bl_model)
    h2o.rm(preds)
    h2o.shutdown(prompt = FALSE)

    # Clear R environment between runs
    if (Construct[i,11][[1]]) {
      if (Construct[i,2] != "multinomial") {
        rm(grid, Grid_Out, cc, dd, VIMP, calibration, calib, out2, out1, features, target, save_model)
      } else {
        rm(grid, Grid_Out, cc, dd, VIMP, features, target, save_model, predsMulti)
      }
    } else {
      if (Construct[i,2] != "multinomial") {
        rm(dd, VIMP, calibration, calib, out2, out1, features, target, save_model)
      } else {
        rm(dd, VIMP, features, target, save_model)
      }
    }

    # Remove data if no longer needed
    if (i > 1) {
      if (Construct[i,7][[1]] != Construct[(i-1),7][[1]]) {
        eval(parse(text = paste0("rm(",Construct[(i-1),7][[1]],")")))
      }
    }
  }
}

#' Automated word2vec data generation via H20
#'
#' This function allows you to automatically build a word2vec model and merge the data onto your supplied dataset
#' @author Adrian Antico
#' @param data Source data table to merge vects onto
#' @param stringCol A string name for the column to convert via word2vec
#' @param KeepStringCol Set to TRUE if you want to keep the original string column that you convert via word2vec
#' @param model_path A string path to the location where you want the model and metadata stored
#' @param ModelID A vector of your model names
#' @param vects The number of vectors to retain from the word2vec model
#' @param SaveStopWords Set to TRUE to save the stop words used
#' @param MinWords For H20 word2vec model
#' @param WindowSize For H20 word2vec model
#' @param Epochs For H20 word2vec model
#' @param StopWords For H20 word2vec model
#' @examples
#'Word2VecModel(data,
#'              stringCol = "Comment",
#'              KeepStringCol = FALSE,
#'              model_path = getwd(),
#'              vects = 50,
#'              SaveStopWords = FALSE)
#' @export
Word2VecModel <- function(datax,
                          stringCol = c("TITLE", "DESCR", "LI_NAME", "ACTION_TAKEN_TEXT", "SUG_DISP"),
                          KeepStringCol = FALSE,
                          model_path = "Query_Multilabel_Models",
                          ModelID = c("TITLE", "DESCR", "LI_NAME", "ACTION_TAKEN_TEXT", "SUG_DISP"),
                          vects = 25,
                          SaveStopWords = FALSE,
                          MinWords = 1,
                          WindowSize = 1,
                          Epochs = 25,
                          StopWords = NULL) {

  # Ensure data is a data.table
  data <- as.data.table(datax)

  # Create storage file
  N <- length(stringCol)
  StoreFile <- data.table(ModelName = rep("a", N), Path = c("aa",N))
  i <- 0

  # Loop through all the string columns
  for (string in stringCol) {
    i <- i + 1
    Sys.sleep(10)
    data[, eval(string) := as.character(get(string))]
    h2o.init(nthreads = 4, max_mem_size = "14G")

    # It is important to remove "\n" -- it appears to cause a parsing error when converting to an H2OFrame
    data[, TEMP := gsub("'|\"|'|“|”|\"|\n|,|\\.|…|\\?|\\+|\\-|\\/|\\=|\\(|\\)|‘", "", data[[string]])]
    data[,":="(TEMP=gsub("  ", " ", TEMP))]
    data2 <- data[, "TEMP"]
    data3 <- as.h2o(data2, destination_frame = "TEMP", col.types=c("String"))

    # Using only questions from the training set because the test set has 'questions' that are fake
    if(is.null(StopWords)) {
      STOP_WORDS = c("ax","i","you","edu","s","t","m","subject","can","lines","re","what",
                     "there","all","we","one","the","a","an","of","or","in","for","by","on",
                     "but","is","in","not","with","as","was","if","they","are","this","and","it","have",
                     "from","at","my","be","by","that","to","from","com","org","like","likes","so")
    } else {
      STOP_WORDS <- StopWords
    }


    # Store stop words?
    if(SaveStopWords) {
      save(STOP_WORDS, file = paste0(model_path,"/STOP_WORDS.Rdata"))
    }

    # Tokenize
    tokenized <- as.data.table(h2o.tokenize(data3, "\\\\W+"))

    # convert to lower case
    tokenized_lower <- tokenized[, C1 := tolower(C1)]

    # remove stop words
    tokenized_words <- tokenized_lower[!(C1 %in% STOP_WORDS)]
    tokenized_words <- as.h2o(tokenized_words[!(is.na(C1))])

    # Build model
    w2v.model <- h2o.word2vec(tokenized_words,
                              model_id           = ModelID[i],
                              word_model         = "SkipGram",
                              norm_model         = "HSM",
                              vec_size           = vects,
                              min_word_freq      = MinWords,
                              window_size        = WindowSize,
                              init_learning_rate = 0.025,
                              sent_sample_rate   = 0.05,
                              epochs             = Epochs)

    # Save model
    w2vPath <- h2o.saveModel(w2v.model, path = model_path, force = TRUE)
    set(StoreFile, i = i, j = 1, value = ModelID[i])
    set(StoreFile, i = i, j = 2, value = w2vPath)
    save(StoreFile, file = paste0(model_path, "/StoreFile.Rdata"))
    h2o.rm('data3')

    # Score model
    all_vecs <- h2o.transform(w2v.model, tokenized_words, aggregate_method = "AVERAGE")

    # Convert to data.table
    all_vecs <- as.data.table(all_vecs)
    data <- data.table(cbind(data, all_vecs))


    data[, ':=' (TEMP = NULL)]
    if(!KeepStringCol) {
      data[, eval(string) := NULL]
    }

    # Replace Colnames
    cols <- names(data[, (ncol(data)-vects+1):ncol(data)])
    for (c in cols) {
      data[, paste0(string,"_",c) := get(c)]
      data[, eval(c) := NULL]
    }

    # Final Prep
    h2o.rm(w2v.model)
    h2o.shutdown(prompt = FALSE)
  }
  return(data)
}


