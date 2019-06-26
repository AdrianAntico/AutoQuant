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
  if (!is.null(FilesPath)) {
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
              )[, -1]
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
              )[, -1]
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
                                                                 newdata = features)[, -1])
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
                                                                 newdata = features)[, -1])
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