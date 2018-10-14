library(data.table)
library(h2o)
library(ggplot2);options(warn=-1)
library(lubridate)

# Load in model reference data
stopifnot(file.exists("Target_Models/grid_tuned_paths.Rdata"))
load("Target_Models/grid_tuned_paths.Rdata")
stopifnot(file.exists("Target_Models/APN_TXNS.RData"))
load("Target_Models/APN_TXNS.RData")

# Load models and environment
startH2o <- function(){
  max_memory = "2G"
  h2o.init(nthreads=-1, max_mem_size=max_memory)
  
  # Need to load in grid tuned paths for qty/cnt models
  load("./Target_Models/grid_tuned_paths.Rdata")
  
  # load QTY models
  QTY_Models <<- lapply(grid_tuned_paths[[1]], function(x) list(Percentile = x))
  for (i in 1:21) {
    QTY_Models[[i]]$Models <<- h2o.loadModel(path = grid_tuned_paths[i,2][[1]])
  }
  
  # load Count models
  CNT_Models <<- lapply(grid_tuned_paths[[1]], function(x) list(Percentile = x))
  for (i in 1:21) {
    CNT_Models[[i]]$Models <<- h2o.loadModel(path = grid_tuned_paths[i,5][[1]])
  }
  
  # Need to load grid tuned paths for LT models
  load("./LT_Target_Models/grid_tuned_paths.Rdata")
  LT_Models <<- lapply(grid_tuned_paths[[1]], function(x) list(Percentile = x))
  for (i in 1:21) {
    LT_Models[[i]]$Models <<- h2o.loadModel(path = grid_tuned_paths[i,2][[1]])
  }
}

# Initialize H20
#check if h20 is running, start if it is not
tryCatch(expr = {h2o.init(startH2O = FALSE)}, 
         error = function(e){
           startH2o()
         })

#stop if we cannot find the models
if(!exists("QTY_Models") || 
   !exists("CNT_Models")){
  startH2o()
}

# Create feature data
FeaturesGenerator <- function(APN_TXNS) {
  data <- APN_TXNS
  
  # Gather most recent recrods for each APN
  data <- data[TargetID == 1]
  
  # Update Time Gap
  data[ , KNOWN_GAP_CURRENT := difftime(as.Date(Sys.time()), ISSUE_DTM, units = "day")]
  
  # Remove non-predictors
  data[, ':=' (MR_ID = NULL, MRLI_ID = NULL, MO_NO = NULL,
               KNOWN_DT = NULL, MO500CREATEDATE = NULL, ISSUE_DTM = NULL)]
  
  # Output data file(s)
  return(data)
}

# Score all APN's
Scoring <- function(ScoringData, TargetWindows = seq(1,52*3,1)) {
  
  #check if h2o cluster is up, start the cluster if not, and reload all the models
  tryCatch(expr = {h2o.init(startH2O = FALSE)}, 
           error = function(){startH2o()})
  
  # Build feature set
  temp  <- FeaturesGenerator(ScoringData)
  TargetWindows <- as.data.table(TargetWindows)
  
  if(is.null(temp)) return(NULL)
  datax <- merge(temp, TargetWindows, all=TRUE)
  names(datax)[1] <- 'target_window'
  data <- as.h2o(datax)
  
  score <- datax[, list("APN", "target_windows")]
  score[, ':=' (QTY_01        = as.data.table(h2o.predict(QTY_Models[[1]]$Models, data)),
                QTY_05        = as.data.table(h2o.predict(QTY_Models[[2]]$Models, data)),
                QTY_10        = as.data.table(h2o.predict(QTY_Models[[3]]$Models, data)),
                QTY_15        = as.data.table(h2o.predict(QTY_Models[[4]]$Models, data)),
                QTY_20        = as.data.table(h2o.predict(QTY_Models[[5]]$Models, data)),
                QTY_25        = as.data.table(h2o.predict(QTY_Models[[6]]$Models, data)),
                QTY_30        = as.data.table(h2o.predict(QTY_Models[[7]]$Models, data)),
                QTY_35        = as.data.table(h2o.predict(QTY_Models[[8]]$Models, data)),
                QTY_40        = as.data.table(h2o.predict(QTY_Models[[9]]$Models, data)),
                QTY_45        = as.data.table(h2o.predict(QTY_Models[[10]]$Models, data)),
                QTY_50        = as.data.table(h2o.predict(QTY_Models[[11]]$Models, data)),
                QTY_55        = as.data.table(h2o.predict(QTY_Models[[12]]$Models, data)),
                QTY_60        = as.data.table(h2o.predict(QTY_Models[[13]]$Models, data)),
                QTY_65        = as.data.table(h2o.predict(QTY_Models[[14]]$Models, data)),
                QTY_70        = as.data.table(h2o.predict(QTY_Models[[15]]$Models, data)),
                QTY_75        = as.data.table(h2o.predict(QTY_Models[[16]]$Models, data)),
                QTY_80        = as.data.table(h2o.predict(QTY_Models[[17]]$Models, data)),
                QTY_85        = as.data.table(h2o.predict(QTY_Models[[18]]$Models, data)),
                QTY_90        = as.data.table(h2o.predict(QTY_Models[[19]]$Models, data)),
                QTY_95        = as.data.table(h2o.predict(QTY_Models[[20]]$Models, data)),
                QTY_99        = as.data.table(h2o.predict(QTY_Models[[21]]$Models, data)),
                CNT_01        = as.data.table(h2o.predict(CNT_Models[[1]]$Models, data)),
                CNT_05        = as.data.table(h2o.predict(CNT_Models[[2]]$Models, data)),
                CNT_10        = as.data.table(h2o.predict(CNT_Models[[3]]$Models, data)),
                CNT_15        = as.data.table(h2o.predict(CNT_Models[[4]]$Models, data)),
                CNT_20        = as.data.table(h2o.predict(CNT_Models[[5]]$Models, data)),
                CNT_25        = as.data.table(h2o.predict(CNT_Models[[6]]$Models, data)),
                CNT_30        = as.data.table(h2o.predict(CNT_Models[[7]]$Models, data)),
                CNT_35        = as.data.table(h2o.predict(CNT_Models[[8]]$Models, data)),
                CNT_40        = as.data.table(h2o.predict(CNT_Models[[9]]$Models, data)),
                CNT_45        = as.data.table(h2o.predict(CNT_Models[[10]]$Models, data)),
                CNT_50        = as.data.table(h2o.predict(CNT_Models[[11]]$Models, data)),
                CNT_55        = as.data.table(h2o.predict(CNT_Models[[12]]$Models, data)),
                CNT_60        = as.data.table(h2o.predict(CNT_Models[[13]]$Models, data)),
                CNT_65        = as.data.table(h2o.predict(CNT_Models[[14]]$Models, data)),
                CNT_70        = as.data.table(h2o.predict(CNT_Models[[15]]$Models, data)),
                CNT_75        = as.data.table(h2o.predict(CNT_Models[[16]]$Models, data)),
                CNT_80        = as.data.table(h2o.predict(CNT_Models[[17]]$Models, data)),
                CNT_85        = as.data.table(h2o.predict(CNT_Models[[18]]$Models, data)),
                CNT_90        = as.data.table(h2o.predict(CNT_Models[[19]]$Models, data)),
                CNT_95        = as.data.table(h2o.predict(CNT_Models[[20]]$Models, data)),
                CNT_99        = as.data.table(h2o.predict(CNT_Models[[21]]$Models, data)))]
  
  # Finalize
  h2o.shutdown(prompt=FALSE)
  return(score)
}

# Generate Scoring data
ScoringData <- FeaturesGenerator(APN_TXNS)

# Generate Preds
Scores <- Scoring(ScoringData, TargetWindows = seq(1,52*3,1))
