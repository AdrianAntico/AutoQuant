# Environment setup----------------
model_path <- "C:\\Users\\aantico\\Desktop\\Work\\H20\\Inventory_Optimization\\data"
setwd(model_path)
packagesReq <- c("h2o","curl","data.table","zoo","lubridate","caTools","ggplot2")
packs <- packagesReq[!(packagesReq %in% installed.packages()[,"Package"])]
if(length(packs)) install.packages(packs)
library(data.table)
library(h2o)
library(ggplot2);options(warn = -1)
library(AdrianModelingTools)

# Define functions----------------
DataLoader     <- function(SampleRate = 1) {
  # Set sampling parameter
  sample_rate <- SampleRate
  
  # Load data from Model_Data_Generator.R
  load(paste0(model_path,"/final_count_model_data.Rdata"))
  load(paste0(model_path,"/qty_model_data.Rdata"))
  
  # Create log transformed targets and remove final meta data
  final_count_model_data[, logCounts := log(V1+1)][, V1 := NULL]
  qty_model_data[, logQTY := log(TARGET_QTY)][, TARGET_QTY := NULL]
  
  # Stratify sampling (reduce data size)
  if (sample_rate < 1) {
    qty_model_data <- qty_model_data[,.SD[sample(.N, .N*sample_rate)],by = sku]
    final_count_model_data <- final_count_model_data[,.SD[sample(.N, .N*sample_rate)],by = sku]
  }
  
  # Departure
  return(list(final_count_model_data, qty_model_data))
}

# Load & Prep Data----------------
loader <- DataLoader(SampleRate = 1)
count_model_data <- loader[[1]]
qty_model_data   <- loader[[2]]
rm(loader)

# Define modeling meta data----------------
if (file.exists(paste0(model_path, "/DemandPlanning.Rdata"))) {
  load(paste0(model_path, "/DemandPlanning.Rdata"))
} else {
  
  # Build modeling constructor file
  DemandPlanning <- data.table(Targets = c(rep("logQTY", 21), 
                                           rep("logCounts", 21)),
       Distribution    = c(rep("quantile", 42)),
       Loss            = c(rep("MAE", 42)),
       Quantile        = c(0.01, seq(0.05, 0.95, 0.05), 
                           0.99, 0.01, seq(0.05, 0.95, 0.05), 0.99),
       ModelName       = c(paste0("QTY", 
                                  as.factor(c(1, seq(5, 95, 5), 99))),
                           paste0("CNT", 
                                  as.factor(c(1, seq(5, 95, 5), 99)))),
       Algoithm        = rep("gbm",42),
       dataName        = c(rep("qty_model_data", 21), 
                           rep("count_model_data", 21)),
       TargetCol       = c(rep("ncol(count_model_data)", 21),
                           rep("ncol(qty_model_data)", 21)),
       FeatureCols     = c(rep("1:(ncol(count_model_data)-1)", 21),
                           rep("1:(ncol(qty_model_data)-1)", 21)),
       CreateDate      = Sys.time(),
       GridTune        = rep(FALSE, 42),
       ExportValidData = rep(FALSE, 42), 
       ParDep          = rep(5, 42), 
       PD_Data         = rep("All", 42), 
       ThreshType      = rep(NA, 42), 
       FSC             = rep(0.001,42),
       tpProfit        = rep(0, 42),
       tnProfit        = rep(0, 42),
       fpProfit        = rep(0, 42),
       fnProfit        = rep(0, 42),
       SaveModel       = rep(TRUE,42),
       SaveModelType   = rep("standard",42))
  
  # Save modeling constructor file
  save(DemandPlanning, file = paste0(model_path, "/DemandPlanning.Rdata"))
}

# Model builds----------------
AutoH20Modeler(Construct  = DemandPlanning,
               max_memory = "32G",
               ratios     = 0.75,
               BL_Trees   = 750,
               nthreads   = 7,
               model_path = model_path)


