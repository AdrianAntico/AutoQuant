context("RemixAutoML")
library(RemixAutoML)

test_that("DummifyDT", {
  # Test 1: number of columns for KeepFactorCols = F and OneHot = F
  test <- data.table::data.table(Value = runif(100000),
                                 FactorCol = sample(
                                   x = c(
                                     letters,
                                     LETTERS,
                                     paste0(letters, letters),
                                     paste0(LETTERS, LETTERS),
                                     paste0(letters, LETTERS),
                                     paste0(LETTERS, letters)
                                   ),
                                   size = 100000,
                                   replace = TRUE
                                 ))
  test <- DummifyDT(
    data = test,
    cols = "FactorCol",
    KeepFactorCols = FALSE,
    SaveFactorLevels = FALSE,
    SavePath = NULL,
    ImportFactorLevels = FALSE,
    OneHot = FALSE,
    FactorLevelsList = NULL, 
    ReturnFactorLevels = FALSE,
    ClustScore = FALSE
  )

  expect_equal(ncol(test), 157)

  # Test KeepFactorCols = T and OneHot = F
  testy <- data.table::data.table(Value = runif(100000),
                                  FactorCol = sample(
                                    x = c(
                                      letters,
                                      LETTERS,
                                      paste0(letters, letters),
                                      paste0(LETTERS, LETTERS),
                                      paste0(letters, LETTERS),
                                      paste0(LETTERS, letters)
                                    ),
                                    size = 100000,
                                    replace = TRUE
                                  ))

  testy <- DummifyDT(
    data = testy,
    cols = "FactorCol",
    KeepFactorCols = TRUE,
    SaveFactorLevels = FALSE,
    SavePath = NULL,
    ImportFactorLevels = FALSE,
    FactorLevelsList = NULL, 
    ReturnFactorLevels = FALSE,
    OneHot = FALSE,
    ClustScore = FALSE
  )

  expect_equal(ncol(testy), 158)

  # Test KeepFactorCols = T and OneHot = T
  testz <- data.table::data.table(Value = runif(100000),
                                  FactorCol = sample(
                                    x = c(
                                      letters,
                                      LETTERS,
                                      paste0(letters, letters),
                                      paste0(LETTERS, LETTERS),
                                      paste0(letters, LETTERS),
                                      paste0(LETTERS, letters)
                                    ),
                                    size = 100000,
                                    replace = TRUE
                                  ))

  testz <- DummifyDT(
    data = testz,
    cols = "FactorCol",
    KeepFactorCols = TRUE,
    SaveFactorLevels = FALSE,
    SavePath = NULL,
    ImportFactorLevels = FALSE,
    FactorLevelsList = NULL, 
    ReturnFactorLevels = FALSE,
    OneHot = TRUE,
    ClustScore = FALSE
  )

  expect_equal(ncol(testz), 159)

  # Test Factor Variables and KeepFactorCols = T and OneHot = T
  testz <- data.table::data.table(Value = runif(100000),
                                  FactorCol = as.factor(sample(
                                    x = c(
                                      letters,
                                      LETTERS,
                                      paste0(letters, letters),
                                      paste0(LETTERS, LETTERS),
                                      paste0(letters, LETTERS),
                                      paste0(LETTERS, letters)
                                    ),
                                    size = 100000,
                                    replace = TRUE
                                  )))

  testz <- DummifyDT(
    data = testz,
    cols = "FactorCol",
    KeepFactorCols = TRUE,
    SaveFactorLevels = FALSE,
    FactorLevelsList = NULL, 
    ReturnFactorLevels = FALSE,
    SavePath = NULL,
    ImportFactorLevels = FALSE,
    OneHot = TRUE,
    ClustScore = FALSE
  )

  expect_equal(ncol(testz), 159)
})

test_that("PrintObjectsSize", {
  x <- PrintObjectsSize(N = 10)
  expect_equal(length(x),1)
})

test_that("GenTSAnomVars", {
  # Check with zero groups
  data <-
    data.table::data.table(
      DateTime = as.Date(Sys.time()),
      Target = stats::filter(
        rnorm(10000, mean = 50, sd = 20),
        filter = rep(1, 10),
        circular = TRUE
      )
    )
  data[, temp := seq(1:10000)][, DateTime := DateTime - temp][, temp := NULL]
  data <- data[order(DateTime)]
  x <- data.table::as.data.table(sde::GBM(N = 10000) * 1000)
  data[, predicted := x[-1, ]]
  stuff    <- GenTSAnomVars(
    data,
    ValueCol    = "Target",
    GroupVars   = NULL,
    DateVar     = "DateTime",
    High        = 1.96,
    Low         = -1.96,
    KeepAllCols = TRUE,
    IsDataScaled  = FALSE
  )
  testthat::expect_equal(ncol(stuff), 11)

  # Check with a single group
  data <- data.table::data.table(
    DateTime = as.Date(Sys.time()),
    Target = stats::filter(
      rnorm(10000,
            mean = 50,
            sd = 20),
      filter = rep(1, 10),
      circular = TRUE
    ),
    Group = as.character(sample(
      x = letters,
      size = 10000,
      replace = TRUE
    ))
  )
  data[, temp := seq(1:10000)][, DateTime := DateTime - temp][, temp := NULL]
  data <- data[order(DateTime)]
  x <- data.table::as.data.table(sde::GBM(N = 10000) * 1000)
  data[, predicted := x[-1, ]]
  stuff    <- GenTSAnomVars(
    data,
    ValueCol    = "Target",
    GroupVars   = "Group",
    DateVar     = "DateTime",
    High        = 1.96,
    Low         = -1.96,
    KeepAllCols = TRUE,
    IsDataScaled  = FALSE
  )
  expect_equal(ncol(stuff), 12)

  # Check with a two group
  data <- data.table::data.table(
    DateTime = as.Date(Sys.time()),
    Target = stats::filter(
      rnorm(10000,
            mean = 50,
            sd = 20),
      filter = rep(1, 10),
      circular = TRUE
    ),
    Group = as.character(sample(
      x = letters,
      size = 10000,
      replace = TRUE
    )),
    Group2 = as.character(sample(
      x = letters,
      size = 10000,
      replace = TRUE
    ))
  )
  data[, temp := seq(1:10000)][,
                               DateTime := DateTime - temp][,
                                                            temp := NULL]
  data <- data[order(DateTime)]
  x <- data.table::as.data.table(sde::GBM(N = 10000) * 1000)
  data[, predicted := x[-1, ]]
  stuff    <- GenTSAnomVars(
    data,
    ValueCol    = "Target",
    GroupVars   = c("Group","Group2"),
    DateVar     = "DateTime",
    High        = 1.96,
    Low         = -1.96,
    KeepAllCols = TRUE,
    IsDataScaled  = FALSE
  )
  expect_equal(ncol(stuff), 13)
})

test_that("ResidualOutliers", {
  # Check to make sure stuff contains 3 items
  data <- data.table::data.table(DateTime = as.Date(Sys.time()),
                                 Target = as.numeric(stats::filter(rnorm(1000,
                                                                         mean = 50,
                                                                         sd = 20),
                                                                   filter=rep(1,10),
                                                                   circular=TRUE)))
  data[, temp := seq(1:1000)][, DateTime := DateTime - temp][, temp := NULL]
  data <- data[order(DateTime)]
  data[, Predicted := as.numeric(stats::filter(rnorm(1000,
                                                     mean = 50,
                                                     sd = 20),
                                               filter=rep(1,10),
                                               circular=TRUE))]
  stuff    <- ResidualOutliers(data = data,
                               DateColName = "DateTime",
                               TargetColName = "Target",
                               PredictedColName = NULL,
                               TimeUnit = "day",
                               maxN = 5,
                               tstat = 2)
  data     <- stuff$FullData
  model    <- stuff$ARIMA_MODEL
  outliers <- data[type != "<NA>"]
  expect_equal(length(stuff), 2)
})

test_that("SimpleCap", {
  # Check function works
  x <- "adrian"
  x <- SimpleCap(x)
  expect_match(substring(x, 1, 2), "Ad")
})

test_that("ModelDataPrep", {
  # Check function works
  data <- data.table::data.table(Value = runif(100000),
                                 FactorCol = as.character(sample(
                                   x = c(
                                     letters,
                                     LETTERS,
                                     paste0(letters, letters),
                                     paste0(LETTERS, LETTERS),
                                     paste0(letters, LETTERS),
                                     paste0(LETTERS, letters)
                                   ),
                                   size = 100000,
                                   replace = TRUE
                                 )))
  data <- ModelDataPrep(
    data,
    Impute = TRUE,
    CharToFactor = TRUE,
    MissFactor = "0",
    MissNum    = -1
  )
  expect_true(is.factor(data[["FactorCol"]]))
})

test_that("RedYellowGreen", {
  # Check function works
  Correl <- 0.70
  aa <- data.table::data.table(target = runif(1000))
  aa[, x1 := qnorm(target)]
  aa[, x2 := runif(1000)]
  aa[, predict := pnorm(Correl * x1 +
                              sqrt(1 - Correl ^2) *
                              qnorm(x2))]
  aa[, target := as.numeric(ifelse(target < 0.5, 0, 1))]
  data <- RedYellowGreen(
    aa,
    PredictColNumber  = 4,
    ActualColNumber   = 1,
    TruePositiveCost  = 0,
    TrueNegativeCost  = 0,
    FalsePositiveCost = -3,
    FalseNegativeCost = -2,
    MidTierCost       = -0.5,
    Cores             = 2,
    Precision         = 0.25,
    Boundaries        = c(0.25,0.75)
  )
  expect_match(names(data)[10], "Utility")
})

test_that("threshOptim", {
  # Check function works
  Correl <- 0.85
  aa <- data.table::data.table(target = runif(1000))
  aa[, x1 := qnorm(target)]
  aa[, x2 := runif(1000)]
  aa[, Independent_Variable1 := log(pnorm(Correl * x1 +
                                            sqrt(1 - Correl ^2)
                                          * qnorm(x2)))]
  aa[, target := as.numeric(ifelse(target < 0.3333, 0, 1))]
  aa[, predict := rnorm(1000)]
  data <- threshOptim(
    data     = aa,
    actTar   = "target",
    predTar  = "predict",
    tpProfit = 0,
    tnProfit = 0,
    fpProfit = -1,
    fnProfit = -2
  )
  optimalThreshold <- data[[1]]
  expect_equal(length(optimalThreshold), 1)
})

test_that("AutoNLS", {
  # Check function works
  data <-
    data.table::data.table(Target = seq(1, 500, 1),
                           Variable = rep(1, 500))
  for (i in as.integer(1:500)) {
    if (i == 1) {
      var <- data[i, "Target"][[1]]
      data.table::set(data,
                      i = i,
                      j = 2L,
                      value = var * (1 + runif(1) / 100))
    } else {
      var <- data[i - 1, "Variable"][[1]]
      data.table::set(data,
                      i = i,
                      j = 2L,
                      value = var * (1 + runif(1) / 100))
    }
  }

  # To keep original values
  data1 <- data.table::copy(data)

  # Merge and Model data
  data11 <- AutoNLS(
    data = data,
    y = "Target",
    x = "Variable",
    monotonic = FALSE
  )

  data2 <- merge(
    data1,
    data11[[1]],
    by = "Variable",
    all = FALSE
  )

  # Plot graphs of predicted vs actual
  p <- ggplot2::ggplot(data2, ggplot2::aes(x = Variable)) +
    ggplot2::geom_line(ggplot2::aes(y = data2[["Target.x"]],
                                    color = "blue")) +
    ggplot2::geom_line(ggplot2::aes(y = data2[["Target.y"]],
                                    color = "red")) +
    ChartTheme(Size = 12) + ggplot2::ggtitle("Growth Models") +
    ggplot2::ylab("Target Variable") +
    ggplot2::xlab("Independent Variable")
  class(p)
  expect_match(class(p)[1], "gg")
})

test_that("DT_GDL_Feature_Engineering", {
  # DT and Scoring

  # Grouping Case
  N <- 25116
  data <-
    data.table::data.table(
      GroupVariable = sample(x = c(
        letters,
        LETTERS,
        paste0(letters, letters),
        paste0(LETTERS, LETTERS),
        paste0(letters, LETTERS),
        paste0(LETTERS, letters)
      )),
      DateTime = base::as.Date(Sys.time()),
      Target = stats::filter(
        rnorm(N,
              mean = 50,
              sd = 20),
        filter = rep(1, 10),
        circular = TRUE
      )
    )
  data[, temp := seq(1:161),
       by = "GroupVariable"][
         , DateTime := DateTime - temp][
           , temp := NULL]
  data <- data[order(DateTime)]
  data <- DT_GDL_Feature_Engineering(
    data,
    lags           = c(seq(1, 1, 1)),
    periods        = c(3),
    statsNames     = c("MA"),
    targets        = c("Target"),
    groupingVars   = "GroupVariable",
    sortDateName   = "DateTime",
    timeDiffTarget = c("Time_Gap"),
    timeAgg        = c("days"),
    WindowingLag   = 1,
    Type           = "Lag",
    SimpleImpute   = TRUE
  )

  N <- 25116
  data1 <-
    data.table::data.table(
      GroupVariable = sample(x = c(
        letters,
        LETTERS,
        paste0(letters, letters),
        paste0(LETTERS, LETTERS),
        paste0(letters, LETTERS),
        paste0(LETTERS, letters)
      )),
      DateTime = base::as.Date(Sys.time()),
      Target = stats::filter(
        rnorm(N,
              mean = 50,
              sd = 20),
        filter = rep(1, 10),
        circular = TRUE
      )
    )
  data1[, temp := seq(1:161),
        by = "GroupVariable"][, DateTime := DateTime - temp]
  data1 <- data1[order(DateTime)]
  data1 <- Scoring_GDL_Feature_Engineering(
    data1,
    lags           = c(seq(1, 1, 1)),
    periods        = c(3),
    statsNames     = c("MA"),
    targets        = c("Target"),
    groupingVars   = c("GroupVariable"),
    sortDateName   = c("DateTime"),
    timeDiffTarget = c("Time_Gap"),
    timeAgg        = "days",
    WindowingLag   = 1,
    Type           = "Lag",
    SimpleImpute   = TRUE,
    AscRowByGroup  = "temp",
    RecordsKeep    = 1
  )

  x <- names(data)
  y <- names(data1[, temp := NULL])
  expect_equal(x, y)

  # Non Grouping Case
  N <- 25116
  data <- data.table::data.table(
    DateTime = as.Date(Sys.time()),
    Target = stats::filter(
      rnorm(N,
            mean = 50,
            sd = 20),
      filter = rep(1, 10),
      circular = TRUE
    )
  )
  data[, temp := seq(1:N)][, DateTime := DateTime - temp][
    , temp := NULL]
  data <- data[order(DateTime)]
  data <- DT_GDL_Feature_Engineering(
    data,
    lags           = c(seq(1, 1, 1)),
    periods        = c(3),
    statsNames     = c("MA"),
    targets        = c("Target"),
    groupingVars   = NULL,
    sortDateName   = "DateTime",
    timeDiffTarget = c("Time_Gap"),
    timeAgg        = c("days"),
    WindowingLag   = 1,
    Type           = "Lag",
    SimpleImpute   = TRUE
  )

  N <- 25116
  data1 <- data.table::data.table(
    DateTime = as.Date(Sys.time()),
    Target = stats::filter(
      rnorm(25116,
            mean = 50,
            sd = 20),
      filter = rep(1, 10),
      circular = TRUE
    )
  )
  data1[, temp := seq(1:N)][, DateTime := DateTime - temp]
  data1 <- data1[order(DateTime)]
  data1 <- Scoring_GDL_Feature_Engineering(
    data1,
    lags           = c(seq(1, 1, 1)),
    periods        = c(3),
    statsNames     = c("MA"),
    targets        = c("Target"),
    groupingVars   = NULL,
    sortDateName   = c("DateTime"),
    timeDiffTarget = c("Time_Gap"),
    timeAgg        = "days",
    WindowingLag   = 1,
    Type           = "Lag",
    SimpleImpute   = TRUE,
    AscRowByGroup  = "temp",
    RecordsKeep    = 1
  )

  x <- names(data)
  y <- names(data1[, temp := NULL])
  expect_equal(x, y)

  # Non Grouping Case
  N <- 25116
  data <- data.table::data.table(
    DateTime = as.Date(Sys.time()),
    Target = stats::filter(
      rnorm(N,
            mean = 50,
            sd = 20),
      filter = rep(1, 10),
      circular = TRUE
    )
  )
  data[, temp := seq(1:N)][, DateTime := DateTime - temp]
  data <- data[order(DateTime)]
  data <- Partial_DT_GDL_Feature_Engineering(
    data,
    lags           = c(seq(1, 1, 1)),
    periods        = c(3),
    statsNames     = c("MA"),
    targets        = c("Target"),
    groupingVars   = NULL,
    sortDateName   = "DateTime",
    timeDiffTarget = c("Time_Gap"),
    timeAgg        = c("days"),
    WindowingLag   = 1,
    Type           = "Lag",
    SimpleImpute   = TRUE,
    AscRowByGroup  = "temp",
    RecordsKeep    = 100
  )

  N <- 25116
  data1 <- data.table::data.table(
    DateTime = as.Date(Sys.time()),
    Target = stats::filter(
      rnorm(25116,
            mean = 50,
            sd = 20),
      filter = rep(1, 10),
      circular = TRUE
    )
  )
  data1[, temp := seq(1:N)][, DateTime := DateTime - temp]
  data1 <- data1[order(DateTime)]
  data1 <- Scoring_GDL_Feature_Engineering(
    data1,
    lags           = c(seq(1, 1, 1)),
    periods        = c(3),
    statsNames     = c("MA"),
    targets        = c("Target"),
    groupingVars   = NULL,
    sortDateName   = c("DateTime"),
    timeDiffTarget = c("Time_Gap"),
    timeAgg        = "days",
    WindowingLag   = 1,
    Type           = "Lag",
    SimpleImpute   = TRUE,
    AscRowByGroup  = "temp",
    RecordsKeep    = 1
  )

})
