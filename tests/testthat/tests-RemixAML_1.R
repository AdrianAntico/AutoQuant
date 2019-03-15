context("RemixAML")
library(RemixAML)

test_that("DummifyDT", {
  # Test 1: number of columns for KeepBaseCols = F and OneHot = F
  test <- data.table::data.table(Value = runif(100000),
                     FactorCol = sample(x = c(letters,
                                              LETTERS,
                                              paste0(letters,letters),
                                              paste0(LETTERS,LETTERS),
                                              paste0(letters,LETTERS),
                                              paste0(LETTERS,letters)),
                                        size = 100000,
                                        replace = TRUE))
  test <- DummifyDT(data = test,
                    cols = "FactorCol",
                    KeepBaseCols = FALSE,
                    OneHot = FALSE)

  expect_equal(ncol(test),157)

  # Test KeepBaseCols = T and OneHot = F
  testy <- data.table::data.table(Value = runif(100000),
                                  FactorCol = sample(x = c(letters,
                                                           LETTERS,
                                                           paste0(letters,letters),
                                                           paste0(LETTERS,LETTERS),
                                                           paste0(letters,LETTERS),
                                                           paste0(LETTERS,letters)),
                                                     size = 100000,
                                                     replace = TRUE))

  testy <- DummifyDT(data = testy,
                     cols = "FactorCol",
                     KeepBaseCols = TRUE,
                     OneHot = FALSE)

  expect_equal(ncol(testy),158)

  # Test KeepBaseCols = T and OneHot = T
  testz <- data.table::data.table(Value = runif(100000),
                                  FactorCol = sample(x = c(letters,
                                                           LETTERS,
                                                           paste0(letters,letters),
                                                           paste0(LETTERS,LETTERS),
                                                           paste0(letters,LETTERS),
                                                           paste0(LETTERS,letters)),
                                                     size = 100000,
                                                     replace = TRUE))

  testz <- DummifyDT(data = testy,
                     cols = "FactorCol",
                     KeepBaseCols = TRUE,
                     OneHot = TRUE)

  expect_equal(ncol(testz),159)
})

test_that("GenTSAnomVars", {
  # Check with zero groups
  data <- data.table::data.table(DateTime = as.Date(Sys.time()), Target = stats::filter(rnorm(10000,mean = 50, sd = 20), filter=rep(1,10), circular=TRUE))
  data[, temp := seq(1:10000)][, DateTime := DateTime - temp][, temp := NULL]
  data <- data[order(DateTime)]
  x <- data.table::as.data.table(sde::GBM(N=10000)*1000)
  data[, predicted := x[-1,]]
  stuff    <- GenTSAnomVars(data,
                            ValueCol    = "Target",
                            GroupVar1   = NULL,
                            GroupVar2   = NULL,
                            DateVar     = "DateTime",
                            High        = 1.96,
                            Low         = -1.96,
                            KeepAllCols = TRUE,
                            DataScaled  = FALSE)
  expect_equal(ncol(stuff), 10)

  # Check with a single group
  data <- data.table::data.table(DateTime = as.Date(Sys.time()),
                     Target = stats::filter(rnorm(10000,
                                                  mean = 50,
                                                  sd = 20),
                                            filter = rep(1,10),
                                            circular=TRUE),
                     Group = as.character(sample(x = letters,
                                                 size = 10000,
                                                 replace = TRUE)))
  data[, temp := seq(1:10000)][, DateTime := DateTime - temp][, temp := NULL]
  data <- data[order(DateTime)]
  x <- data.table::as.data.table(sde::GBM(N=10000)*1000)
  data[, predicted := x[-1,]]
  stuff    <- GenTSAnomVars(data,
                            ValueCol    = "Target",
                            GroupVar1   = "Group",
                            GroupVar2   = NULL,
                            DateVar     = "DateTime",
                            High        = 1.96,
                            Low         = -1.96,
                            KeepAllCols = TRUE,
                            DataScaled  = FALSE)
  expect_equal(ncol(stuff), 11)

  # Check with a two group
  data <- data.table::data.table(DateTime = as.Date(Sys.time()),
                     Target = stats::filter(rnorm(10000,
                                                  mean = 50,
                                                  sd = 20),
                                            filter = rep(1,10),
                                            circular=TRUE),
                     Group = as.character(sample(x = letters,
                                                 size = 10000,
                                                 replace = TRUE)),
                     Group2 = as.character(sample(x = letters,
                                                 size = 10000,
                                                 replace = TRUE)))
  data[, temp := seq(1:10000)][, DateTime := DateTime - temp][, temp := NULL]
  data <- data[order(DateTime)]
  x <- data.table::as.data.table(sde::GBM(N=10000)*1000)
  data[, predicted := x[-1,]]
  stuff    <- GenTSAnomVars(data,
                            ValueCol    = "Target",
                            GroupVar1   = "Group",
                            GroupVar2   = "Group2",
                            DateVar     = "DateTime",
                            High        = 1.96,
                            Low         = -1.96,
                            KeepAllCols = TRUE,
                            DataScaled  = FALSE)
  expect_equal(ncol(stuff), 12)
})

test_that("ResidualOutliers", {
  # Check to make sure stuff contains 3 items
  data <- data.table::data.table(a = seq(0,10000,1), predicted = sde::GBM(N=10000)*1000)[-1,]
  stuff    <- ResidualOutliers(data = data, maxN = 5, cvar = 4)
  data     <- stuff[[1]]
  model    <- stuff[[2]]
  resid    <- stuff[[3]]
  outliers <- data[type != "<NA>"]
  expect_equal(length(stuff), 3)
})

test_that("GLRM_KMeans_Col", {
  # Check that GridTuneGLRM = T and GridTuneKMeans = T
  data <- data.table::as.data.table(iris)
  data <- GLRM_KMeans_Col(data,
                          GridTuneGLRM = FALSE,
                          GridTuneKMeans = FALSE,
                          nthreads = 8,
                          MaxMem = "28G",
                          glrmCols = 1:(ncol(data)-1),
                          IgnoreConstCols = TRUE,
                          glrmFactors = 2,
                          Loss = "Absolute",
                          glrmMaxIters = 1000,
                          SVDMethod = "Randomized",
                          MaxRunTimeSecs = 3600,
                          KMeansK = 5)
  expect_equal(ncol(data), 6)

  #Check that GridTuneGLRM = F and GridTuneKMeans = F
  data1 <- data.table::as.data.table(iris)
  Sys.sleep(10)
  data1 <- GLRM_KMeans_Col(data1,
                           GridTuneGLRM = FALSE,
                           GridTuneKMeans = FALSE,
                           nthreads = 8,
                           MaxMem = "28G",
                           glrmCols = 1:(ncol(data1)-1),
                           IgnoreConstCols = TRUE,
                           glrmFactors = 2,
                           Loss = "Absolute",
                           glrmMaxIters = 1000,
                           SVDMethod = "Randomized",
                           MaxRunTimeSecs = 3600,
                           KMeansK = 5)
  expect_equal(ncol(data1), 6)
})

test_that("AutoTS", {
  # Check function works
  data <- data.table::data.table(DateTime = as.Date(Sys.time()),
                                 Target = stats::filter(rnorm(1000,
                                                              mean = 50,
                                                              sd = 20),
                                                        filter=rep(1,10),
                                                        circular=TRUE))
  data[, temp := seq(1:1000)][, DateTime := DateTime - temp][, temp := NULL]
  data <- data[order(DateTime)]
  output <-   AutoTS(data,
                     TargetName     = "Target",
                     DateName       = "DateTime",
                     FCPeriods      = 30,
                     HoldOutPeriods = 30,
                     TimeUnit       = "day", # c("hour","day","week","month","quarter","year"),
                     Lags           = 5,
                     SLags          = 1,
                     NumCores       = 4,
                     SkipModels     = NULL,
                     StepWise       = TRUE)
  x <- nrow(output[[1]])
  expect_equal(x, 30)
})

test_that("SimpleCap", {
  # Check function works
  x <- "adrian"
  x <- SimpleCap(x)
  expect_match(substring(x,1,2), "Ad")
})

test_that("ModelDataPrep", {
  # Check function works
  data <- data.table::data.table(Value = runif(100000),
                                 FactorCol = as.character(sample(x = c(letters,
                                                                       LETTERS,
                                                                       paste0(letters,letters),
                                                                       paste0(LETTERS,LETTERS),
                                                                       paste0(letters,LETTERS),
                                                                       paste0(LETTERS,letters)),
                                                                 size = 100000,
                                                                 replace = TRUE)))
  data <- ModelDataPrep(data,
                        Impute = TRUE,
                        CharToFactor = TRUE,
                        MissFactor = "0",
                        MissNum    = -1)
  expect_true(is.factor(data[["FactorCol"]]))
})

test_that("RedYellowGreen", {
  # Check function works
  Correl <- 0.85
  aa <- data.table::data.table(target = runif(1000))
  aa[, x1 := qnorm(target)]
  aa[, x2 := runif(1000)]
  aa[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  aa[, target := as.numeric(ifelse(target < 0.3333, 0,1))]
  aa[, predict := rnorm(1000)]
  data <- RedYellowGreen(aa,
                         PredictColNumber  = 5,
                         ActualColNumber   = 1,
                         TruePositiveCost  = 0,
                         TrueNegativeCost  = 0,
                         FalsePositiveCost = -1,
                         FalseNegativeCost = -2,
                         MidTierCost       = -0.5)
  expect_match(names(data)[10], "Utility")
})

test_that("threshOptim", {
  # Check function works
  Correl <- 0.85
  aa <- data.table::data.table(target = runif(1000))
  aa[, x1 := qnorm(target)]
  aa[, x2 := runif(1000)]
  aa[, Independent_Variable1 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  aa[, target := as.numeric(ifelse(target < 0.3333, 0,1))]
  aa[, predict := rnorm(1000)]
  data <- threshOptim(data     = aa,
                      actTar   = "target",
                      predTar  = "predict",
                      tpProfit = 0,
                      tnProfit = 0,
                      fpProfit = -1,
                      fnProfit = -2)
  optimalThreshold <- data[[1]]
  expect_equal(length(optimalThreshold), 1)
})

test_that("nlsModelFit", {
  # Check function works
  data <- data.table::data.table(Variable = seq(1,500,1), Target = rep(1, 500))
  for (i in as.integer(1:500)) {
    if(i == 1) {
      var <- data[i, "Variable"][[1]]
      data.table::set(data, i = i, j = 2L, value = var * (1 + runif(1)/100))
  } else {
      var = data[i-1, "Target"][[1]]
      data.table::set(data, i = i, j = 2L, value = var * (1 + runif(1)/100))
    }
  }

  # To keep original values
  data1 <- data.table::copy(data)

  # Merge and Model data
  data2 <- merge(data1,
                 nlsModelFit(data = data, y = "Target", x = "Variable", monotonic = FALSE),
                 by = "Variable",
                 all = TRUE)

  # Plot graphs of predicted vs actual
  p <- ggplot2::ggplot(data2, ggplot2::aes(x = Variable)) +
    ggplot2::geom_line(ggplot2::aes(y = data2[["Target.x"]], color = "blue")) +
    ggplot2::geom_line(ggplot2::aes(y = data2[["Target.y"]], color = "red")) +
    ChartTheme(Size = 12) + ggplot2::ggtitle("Growth Models") +
    ggplot2::ylab("Target Variable") + ggplot2::xlab("Independent Variable")
  class(p)
  expect_match(class(p)[1], "gg")
})

test_that("Name_Matching_GDL", {

  # Grouping Case
  N = 25116
  data <- data.table::data.table(GroupVariable = sample(x = c(letters,
                                                  LETTERS,
                                                  paste0(letters,letters),
                                                  paste0(LETTERS,LETTERS),
                                                  paste0(letters,LETTERS),
                                                  paste0(LETTERS,letters))),
                     DateTime = base::as.Date(Sys.time()),
                     Target = stats::filter(rnorm(N,
                                                  mean = 50,
                                                  sd = 20),
                                            filter=rep(1,10),
                                            circular=TRUE))
  data[, temp := seq(1:161), by = "GroupVariable"][, DateTime := DateTime - temp][, temp := NULL]
  data <- data[order(DateTime)]
  data <- DT_GDL_Feature_Engineering(data,
                                     lags           = c(seq(1,5,1)),
                                     periods        = c(3,5,10,15,20,25),
                                     statsNames     = c("MA"),
                                     targets        = c("Target"),
                                     groupingVars   = "GroupVariable",
                                     sortDateName   = "DateTime",
                                     timeDiffTarget = c("Time_Gap"),
                                     timeAgg        = c("days"),
                                     WindowingLag   = 1,
                                     Type           = "Lag",
                                     Timer          = TRUE,
                                     SkipCols       = FALSE,
                                     SimpleImpute   = TRUE)

  N = 25116
  data1 <- data.table::data.table(GroupVariable = sample(x = c(letters,
                                                  LETTERS,
                                                  paste0(letters,letters),
                                                  paste0(LETTERS,LETTERS),
                                                  paste0(letters,LETTERS),
                                                  paste0(LETTERS,letters))),
                     DateTime = base::as.Date(Sys.time()),
                     Target = stats::filter(rnorm(N,
                                                  mean = 50,
                                                  sd = 20),
                                            filter=rep(1,10),
                                            circular=TRUE))
  data1[, temp := seq(1:161), by = "GroupVariable"][, DateTime := DateTime - temp]
  data1 <- data1[order(DateTime)]
  data1 <- Scoring_GDL_Feature_Engineering(data1,
                           lags           = c(seq(1,5,1)),
                           periods        = c(3,5,10,15,20,25),
                           statsFUNs      = c(function(x) mean(x,na.rm = TRUE)),
                           statsNames     = c("MA"),
                           targets        = c("Target"),
                           groupingVars   = c("GroupVariable"),
                           sortDateName   = c("DateTime"),
                           timeDiffTarget = c("Time_Gap"),
                           timeAgg        = "days",
                           WindowingLag   = 1,
                           Type           = "Lag",
                           Timer          = TRUE,
                           SkipCols       = FALSE,
                           SimpleImpute   = TRUE,
                           AscRowByGroup  = "temp",
                           RecordsKeep    = 1)

  x <- names(data)
  y <- names(data1[,temp := NULL])
  expect_equal(x,y)

  # Non Grouping Case
  N = 25116
  data <- data.table::data.table(DateTime = as.Date(Sys.time()),
                                 Target = stats::filter(rnorm(N,
                                                              mean = 50,
                                                              sd = 20),
                                                        filter=rep(1,10),
                                                        circular=TRUE))
  data[, temp := seq(1:N)][, DateTime := DateTime - temp][, temp := NULL]
  data <- data[order(DateTime)]
  data <- DT_GDL_Feature_Engineering(data,
                                     lags           = c(seq(1,5,1)),
                                     periods        = c(3,5,10,15,20,25),
                                     statsNames     = c("MA"),
                                     targets        = c("Target"),
                                     groupingVars   = NULL,
                                     sortDateName   = "DateTime",
                                     timeDiffTarget = c("Time_Gap"),
                                     timeAgg        = c("days"),
                                     WindowingLag   = 1,
                                     Type           = "Lag",
                                     Timer          = TRUE,
                                     SkipCols       = FALSE,
                                     SimpleImpute   = TRUE)



  N = 25116
  data1 <- data.table::data.table(DateTime = as.Date(Sys.time()),
    Target = stats::filter(rnorm(25116,
                                 mean = 50,
                                 sd = 20),
                           filter=rep(1,10),
                           circular=TRUE))
  data1[, temp := seq(1:N)][, DateTime := DateTime - temp]
  data1 <- data1[order(DateTime)]
  data1 <- Scoring_GDL_Feature_Engineering(data1,
                                           lags           = c(seq(1,5,1)),
                                           periods        = c(3,5,10,15,20,25),
                                           statsFUNs      = c(function(x) mean(x,na.rm = TRUE)),
                                           statsNames     = c("MA"),
                                           targets        = c("Target"),
                                           groupingVars   = NULL,
                                           sortDateName   = c("DateTime"),
                                           timeDiffTarget = c("Time_Gap"),
                                           timeAgg        = "days",
                                           WindowingLag   = 1,
                                           Type           = "Lag",
                                           Timer          = TRUE,
                                           SkipCols       = FALSE,
                                           SimpleImpute   = TRUE,
                                           AscRowByGroup  = "temp",
                                           RecordsKeep    = 1)

  x <- names(data)
  y <- names(data1[,temp := NULL])
  expect_equal(x,y)
})
