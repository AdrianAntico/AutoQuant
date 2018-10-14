# Set directories
project_path <- "C:\\Users\\aantico\\Desktop\\Work\\H20\\Inventory_Optimization\\data"
path <- "C:\\Users\\aantico\\Desktop\\Work\\H20\\Inventory_Optimization\\data"
setwd(project_path)

# Pull in acquisition cost data for skus
load(file = paste0(project_path,"\\SkuCostData.Rdata"))
load(file = paste0(project_path,"\\sku_list.Rdata"))

# Load model reference data
stopifnot(file.exists(paste0(project_path, "\\grid_tuned_paths.Rdata")))
load(paste0(project_path, "\\grid_tuned_paths.Rdata"))
stopifnot(file.exists(paste0(project_path, "\\SKU_TXNS.Rdata")))
load(paste0(project_path, "\\SKU_TXNS.Rdata"))
load(paste0(project_path, "\\SkuCostData.Rdata"))

# SKU List for Shiny DropDown Selection Box
SKUS <- list(SKU_LABEL = sku_list, SKU = sku_list)

# Sku Cost from original import in DataPrep()
sku_cost <- SkuCostData[sku == sku, mean(UnitPrice)][[1]]

# Define functions / Load up when starting Shiny----------------

# System function
systemCall <- function (command) {
  on.exit(Sys.setenv(GFORTRAN_STDOUT_UNIT = "-1"), add = TRUE)
  on.exit(Sys.setenv(GFORTRAN_STDERR_UNIT = "-1"), add = TRUE)
  flag <- 22L
  .Internal(system(command, 22L, "", "", "", 0))
}

# For mojo function
safeSystem <- function(x) {
  print(sprintf("+ CMD: %s", x))
  res <- system(x)
  print(res)
  if (res != 0) {
    msg <- sprintf("SYSTEM COMMAND FAILED (exit status %d)", res)
    stop(msg)
    }
  }

# Revised h2o mojo scoring function
mojo <- function(input_csv_path, 
                 mojo_zip_path, 
                 output_csv_path = NULL,
                 genmodel_jar_path = NULL, 
                 classpath = NULL, 
                 java_options = NULL) {
  prediction_output_file <- "prediction.csv"
  mojo_zip_path <- normalizePath(mojo_zip_path)
  parent_dir <- dirname(mojo_zip_path)
  output_csv_path <- file.path(parent_dir, prediction_output_file)
  classpath <- genmodel_jar_path
  cmd <- c("java")
  java_options_list <- strsplit(java_options, " ")
  for (i in 1:length(java_options_list)) {
    cmd <- c(cmd, java_options_list[[i]])
  }
  cmd <- c(cmd, "-cp", classpath, "hex.genmodel.tools.PredictCsv", 
           "--mojo", mojo_zip_path, "--input", input_csv_path, "--output", 
           output_csv_path, "--decimal")
  cmd_str <- paste(cmd, collapse = " ")
  safeSystem(cmd_str)
  result <- read.csv(output_csv_path)
  return(result)
}

# (Outer) For plotting the cost graphs
plotCharts <- function(data_collect, data) {
  minWeek      <- data_collect[, MinCostWeek]
  orderPoints  <- as.numeric(
    unlist(
      regmatches(data_collect[,BestStrategy], 
                 gregexpr("[[:digit:]]+", 
                          data_collect[,BestStrategy]
                          )
                 )
      )
    )
  
  # Plot Total Costs and GAM Fit
  totalCostPlot  <- ggplot(data)  +
    geom_point(aes(x=data$WEEKS_OUT, y=data$totalCost), size = 1, color = "red") +
    stat_smooth(aes(x=data$WEEKS_OUT,y=data$totalCost), method = "gam",
                formula = y ~ s(x)) +
    xlab("Weeks") + ylab("Total Cost") +
    ChartTheme(BackGround = "lightsteelblue1",OtherColor = "navyblue", Size = 15)
  
  # Plot SLA Costs and GAM Fit
  StockOutCostPlot  <- ggplot(data)  +
    geom_point(aes(x=data$WEEKS_OUT, y=data$stockOutCost), size = 1, color = "red") +
    stat_smooth(aes(x=data$WEEKS_OUT,y=data$stockOutCost), method = "gam",
                formula = y ~ s(x)) +
    xlab("Weeks") + ylab("Stockout Cost") +
    ChartTheme(BackGround = "lightsteelblue1",OtherColor = "navyblue", Size = 15)
  
  # Plot Carry Costs and GAM Fit
  CarryCostPlot  <- ggplot(data)  +
    geom_point(aes(x=data$WEEKS_OUT, y=data$carryCost), size = 1, color = "red") +
    stat_smooth(aes(x=data$WEEKS_OUT,y=data$carryCost), method = "gam",
                formula = y ~ s(x)) +
    xlab("Weeks") + ylab("Carrying Cost") +
    ChartTheme(BackGround = "lightsteelblue1",OtherColor = "navyblue", Size = 15)
  
  # View graphical details
  tcp  <- totalCostPlot + 
    geom_vline(xintercept = minWeek - orderPoints[1], 
               lty = 6, 
               colour = "purple4", 
               lwd = 1.25) +
    geom_vline(xintercept = minWeek + orderPoints[2], 
               lty = 6, 
               colour = "purple4",
               lwd = 1.25) # +

  sop  <- StockOutCostPlot +
    geom_vline(xintercept = minWeek - orderPoints[1], 
               lty = 6, 
               colour = "purple4", 
               lwd = 1.25) +
    geom_vline(xintercept = minWeek + orderPoints[2], 
               lty = 6, 
               colour = "purple4",
               lwd = 1.25)

  ccp  <- CarryCostPlot + 
    geom_vline(xintercept = minWeek - orderPoints[1], 
               lty = 6, 
               colour = "purple4", 
               lwd = 1.25) +
    geom_vline(xintercept = minWeek + orderPoints[2], 
               lty = 6, 
               colour = "purple4",
               lwd = 1.25)
  return(list(tcp,ccp,sop))
}

# (Inner) Create feature data for models
FeaturesGenerator <- function(SKU_TXNS, sku, randomStartDate){
  
  # Subset entire table to specific APN
  sku_x <- sku
  sub_sku <- SKU_TXNS[sku == sku_x]               
  
  # historical data -- point in time -- target window
  # Split data into previous history and target window data
  SKU <- sub_sku[as.Date(date) < randomStartDate][order(date)]
  
  # Remove meta data for feature creation set
  features <- SKU[, ':=' (sku = NULL)][order(-date)][1,]
  
  # Final Prep
  if(nrow(features) != 0) {
    features[, ':=' (date = NULL)]
  } 
  
  # Output data file(s)
  return(features)
}

# (Inner) Score models for a single sku
singleSKU_UD <- function(sku, TargetWindows) {

  # Build feature set
  temp  <- FeaturesGenerator(SKU_TXNS, sku = sku, randomStartDate=as.Date(Sys.time()))
  datax <- cbind(TargetWindows,temp)
  setnames(datax, "TargetWindows", "target_window")
  fwrite(datax, file  = paste0(path,"\\datax.csv"))
  
  # Build scoring data
  score <- data.table(TargetWindow = TargetWindows,
                      sku          = rep(sku, length(TargetWindows)),
                      QTY_01       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[1,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[1,6][[1]]))[1]),
                      QTY_05       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[2,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[2,6][[1]]))[1]),
                      QTY_10       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[3,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[3,6][[1]]))[1]),
                      QTY_15       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[4,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[4,6][[1]]))[1]),
                      QTY_20       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[5,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[5,6][[1]]))[1]),
                      QTY_25       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[6,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[6,6][[1]]))[1]),
                      QTY_30       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[7,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[7,6][[1]]))[1]),
                      QTY_35       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[8,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[8,6][[1]]))[1]),
                      QTY_40       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[9,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[9,6][[1]]))[1]),
                      QTY_45       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[10,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[10,6][[1]]))[1]),
                      QTY_50       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[11,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[11,6][[1]]))[1]),
                      QTY_55       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[12,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[12,6][[1]]))[1]),
                      QTY_60       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[13,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[13,6][[1]]))[1]),
                      QTY_65       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[14,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[14,6][[1]]))[1]),
                      QTY_70       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[15,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[15,6][[1]]))[1]),
                      QTY_75       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[16,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[16,6][[1]]))[1]),
                      QTY_80       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[17,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[17,6][[1]]))[1]),
                      QTY_85       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[18,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[18,6][[1]]))[1]),
                      QTY_90       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[19,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[19,6][[1]]))[1]),
                      QTY_95       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[20,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[20,6][[1]]))[1]),
                      QTY_99       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[21,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[21,6][[1]]))[1]),
                      CNT_01       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[22,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[22,6][[1]]))[1]),
                      CNT_05       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[23,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[23,6][[1]]))[1]),
                      CNT_10       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[24,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[24,6][[1]]))[1]),
                      CNT_15       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[25,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[25,6][[1]]))[1]),
                      CNT_20       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[26,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[26,6][[1]]))[1]),
                      CNT_25       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[27,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[27,6][[1]]))[1]),
                      CNT_30       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[28,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[28,6][[1]]))[1]),
                      CNT_35       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[29,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[29,6][[1]]))[1]),
                      CNT_40       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[30,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[30,6][[1]]))[1]),
                      CNT_45       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[31,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[31,6][[1]]))[1]),
                      CNT_50       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[32,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[32,6][[1]]))[1]),
                      CNT_55       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[33,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[33,6][[1]]))[1]),
                      CNT_60       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[34,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[34,6][[1]]))[1]),
                      CNT_65       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[35,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[35,6][[1]]))[1]),
                      CNT_70       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[36,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[36,6][[1]]))[1]),
                      CNT_75       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[37,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[37,6][[1]]))[1]),
                      CNT_80       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[38,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[38,6][[1]]))[1]),
                      CNT_85       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[39,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[39,6][[1]]))[1]),
                      CNT_90       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[40,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[40,6][[1]]))[1]),
                      CNT_95       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[41,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[41,6][[1]]))[1]),
                      CNT_99       = exp(mojo(input_csv_path = paste0(path,"\\datax.csv"), 
                                              mojo_zip_path = grid_tuned_paths[42,2][[1]], 
                                              java_options = '-Xmx1g -XX:ReservedCodeCacheSize=256m', 
                                              genmodel_jar_path = paste0(path,"\\",grid_tuned_paths[42,6][[1]]))[1]))
  
  # Finalize
  return(score)
}

# Demand Simulation
cppFunction(code = "
 // load Rcpp
#include <Rcpp.h>

using namespace Rcpp;		// shorthand

// [[Rcpp::export]]
NumericVector demandSim(NumericVector score, NumericVector percentileList, int nSims) {

    int i,j,k,l,loops,count;
    double rn1,rn2,min_val,max_val,sumQ,sumq;
    loops = percentileList.size();
    min_val = percentileList[0];
    max_val = percentileList[20];
    NumericVector store(nSims);
    
    for (i=0; i < nSims; i++) {
      rn1 = rand() / (RAND_MAX + 1.);
      k = ceil(rn1 * (loops-1));
      if(rn1 <= min_val) {
        count = floor(rand() / (RAND_MAX + 1.) + ceil(score[23])) - 1;
      } else if (rn1 >= max_val) {
        count = floor(rand() / (RAND_MAX + 1.) + ceil(score[43])) - 1;
      } else {
        count = floor(rand() / (RAND_MAX + 1.) + ceil(score[k+22]*pow(score[k+23]/score[k+22],(rn1 - percentileList[k])/(percentileList[k+1]-percentileList[k]))) - 1);
      }

      if (count > 0) {
        sumQ = 0;
        for (j=0; j < count; j++) {
          sumq = 0;
          rn2 = rand() / (RAND_MAX + 1.);
          l = ceil(rn2 * (loops-1));
          if(rn2 <= min_val) {
            sumq = floor(rand() / (RAND_MAX + 1.) + ceil(score[2])) - 1;
          } else if (rn2 >= max_val) {
            sumq = floor(rand() / (RAND_MAX + 1.) + ceil(score[22])) - 1;
          } else {
            sumq = floor(rand() / (RAND_MAX + 1.) + ceil(score[l+2]*pow(score[l+3]/score[l+2],(rn2 - percentileList[l])/(percentileList[l+1] - percentileList[l])))) - 1;
          }
          sumQ += sumq;
        }
        store[i] = sumQ;
      } else {
        store[i] = 0;
      }
    }
    return store;
}")


# (Outer) Run full simulation
Demand <- function(sku, weeksOut, nSims){
  score <- singleSKU_UD(sku, TargetWindows = 1:weeksOut)
  simResults <- lapply(1:weeksOut, function(w){
    temp <- as.numeric(score[TargetWindow == w])
    demandSim(temp, percentileList = c(0.01,seq(0.05,0.95,0.05),0.99), nSims = nSims)
  })
  return(rbindlist(list(simResults)))
}

# simulationData <- Demand(sku, weeksOut, nSims)

# (Inner) Get AvailableStock data
AvailableStock <- function(simulationData, on_hand, weeksOut, quantiles) {
  aggResults <- data.table(WEEKS_OUT = 1:weeksOut)
  aggResults[,paste0("Q", quantiles) := on_hand]
  for(i in 1:weeksOut){
    aggResults[i, 2:ncol(aggResults)] <- as.list(on_hand - quantile(simulationData[[i]], quantiles, names = F)) 
  }
  setnames(aggResults, c("Q0.05","Q0.2","Q0.5","Q0.8","Q0.95"), c("Q0.95","Q0.8","Q0.5","Q0.2","Q0.05"))
  setcolorder(aggResults, c(1,6,5,4,3,2))
  for (i in 2:ncol(aggResults)) {
    set(aggResults, which(aggResults[[i]] < 0),i,0)
  } 
  return(aggResults)
}

# (Shiny) Adds more demand lines for graphical purposes
AvailableStocks <- function(simulationData, on_hand, weeksOut, quantiles) {
  aggResults <- data.table(WEEKS_OUT = 1:weeksOut)
  aggResults[,paste0("Q", quantiles) := on_hand]
  for(i in 1:weeksOut){
    aggResults[i, 2:ncol(aggResults)] <- as.list(on_hand - quantile(simulationData[[i]], quantiles, names = F)) 
  }
  setnames(aggResults, c("Q0.05","Q0.1","Q0.2","Q0.3","Q0.4","Q0.5","Q0.6","Q0.7","Q0.8","Q0.9","Q0.95"), c("Q0.95","Q0.9","Q0.8","Q0.7","Q0.6","Q0.5","Q0.4","Q0.3","Q0.2","Q0.1","Q0.05"))
  setcolorder(aggResults, c(1,12,11,10,9,8,7,6,5,4,3,2))
  for (i in 2:ncol(aggResults)) {
    set(aggResults,which(aggResults[[i]] < 0),i,0)
  } 
  return(aggResults)
}

# (Inner) Compute and merge cost data to AvailableStock data
stockOutData <- function(simulationData,weeksOut,SO_Cost,sku_cost,carryCostPerc,on_hand,quantity,nSims) {
  probabilityOut <- AvailableStock(simulationData, quantity, weeksOut, c(.05, .2, .5, .8, .95))
  for (i in 1:(weeksOut)) {
    probabilityOut[i, avgInventory := quantity - mean(simulationData[[i]])]
  }
  probabilityOut[, avgInventory := ifelse(avgInventory < 0, 0, avgInventory)]
  probabilityOut[, countOut := 0.0]
  probabilityOut[, probOut := 0.0]
  for (i in 1:(weeksOut)) {
    probabilityOut[i, countOut := sum(simulationData[[i]] > quantity)]
    probabilityOut[i, probOut := round(countOut / nSims,4)]
  }
  probabilityOut[, countOut := NULL]
  
  # Stockout costs
  probabilityOut[, stockOutCost := probOut * SO_Cost]
  Range <- range(probabilityOut[,stockOutCost])
  if (Range[2]-Range[1] != 0) {
    probabilityOut <- tryCatch({nlsModelFit(probabilityOut, y = "stockOutCost", x = "WEEKS_OUT", monotonic = TRUE)}, error=function(x) return(probabilityOut))
  }
  probabilityOut[, stockOutCost := ifelse(stockOutCost < 0, 0, stockOutCost)]
  
  # Carrying costs
  probabilityOut[, carryCost    := carryCostPerc * avgInventory * sku_cost]
  Range <- range(probabilityOut[,carryCost])
  if (Range[2]-Range[1] != 0) {
    probabilityOut <- tryCatch({nlsModelFit(probabilityOut, y = "carryCost", x = "WEEKS_OUT", monotonic = TRUE)}, error=function(x) return(probabilityOut))
  }
  probabilityOut[, carryCost    := ifelse(carryCost < 0, 0, carryCost)]
  
  # Total costs
  probabilityOut[, totalCost    := carryCost + stockOutCost]
  
  # Return results
  return(probabilityOut)
}

# (Inner) Stockout data with lead time models
stockOutDataLT <- function(simulationData, 
                           pltdata, 
                           weeksOut,
                           SO_Cost,
                           sku_cost,
                           carryCostPerc,
                           on_hand,
                           quantity,
                           nSims) {
  probabilityOut <- AvailableStock(simulationData, quantity, weeksOut, c(.05, .2, .5, .8, .95))
  for (i in 1:(weeksOut)) {
    probabilityOut[i, avgInventory := quantity - mean(simulationData[[i]])]
  }
  probabilityOut[, avgInventory := ifelse(avgInventory < 0, 0, avgInventory)]
  
  # Stockout risk
  probabilityOut[, countOut := 0.0]
  probabilityOut[, probOut := 0.0]
  for (i in 1:(weeksOut)) {
    probabilityOut[i, countOut := sum(simulationData[[i]] > quantity)]
    probabilityOut[i, probOut := round(countOut / nSims,4)]
  }
  probabilityOut[, countOut := NULL]
  
  # PLT survival time steps > 0.0001
  #n <- length(pltdata)
  n <- length(part_lead_time[[2]])
  pl <- list()
  k=1
  repeat {
    #z <- sum(pltdata > k)/n
    z <- sum(part_lead_time[[2]] > k)/n
    if (z < 0.0001) break
    pl[[k]] <- z
    k = k + 1
  }
  
  # Probability of stockout within LT
  probabilityOut[, OOS_RISK_ORDER_DATE := rep(0.0, weeksOut)]
  for(i in 1:weeksOut) {
    out = 0
    for (j in 1:length(pl)) {
      out = out + sum(simulationData[[min(weeksOut,j+i-1)]] > quantity)/n * pl[[j]]
    }
    set(probabilityOut, i, 9, value = out)
  }
  probabilityOut <- nlsModelFit(probabilityOut, y = "OOS_RISK_ORDER_DATE", x = "WEEKS_OUT", monotonic = TRUE)
  
  # Stockout costs
  probabilityOut[, stockOutCost := OOS_RISK_ORDER_DATE * SO_Cost]
  Range <- range(probabilityOut[,stockOutCost])
  if (Range[2]-Range[1] != 0) {
    probabilityOut <- tryCatch({nlsModelFit(probabilityOut, y = "stockOutCost", x = "WEEKS_OUT", monotonic = TRUE)}, error=function(x) return(probabilityOut))
  }
  probabilityOut[, stockOutCost := ifelse(stockOutCost < 0, 0, stockOutCost)]
  
  # Carrying costs
  probabilityOut[, carryCost    := carryCostPerc * avgInventory * sku_cost]
  Range <- range(probabilityOut[,carryCost])
  if (Range[2]-Range[1] != 0) {
    probabilityOut <- tryCatch({nlsModelFit(probabilityOut, y = "carryCost", x = "WEEKS_OUT", monotonic = TRUE)}, error=function(x) return(probabilityOut))
  }
  probabilityOut[, carryCost    := ifelse(carryCost < 0, 0, carryCost)]
  
  # Total costs
  probabilityOut[, totalCost    := carryCost + stockOutCost]
  
  # Return results
  return(probabilityOut)
}

# (Outer) Find starting sku quantity range for optimization
QuantityRangeFinder <- function(simulationData, weeksOut, SO_Cost, carryCost, binSearchGrowth = 2, TC_MinWeek = NULL) {
  
  # Find upper boundary for quantity
  quantity <- 0
  i        <- 0
  minWeek  <- 1
  data1    <- list()
  if(is.null(TC_MinWeek)) {
    thresh <- ceiling(weeksOut / 2)
  } else {
    thresh <- TC_MinWeek
  }
  while(minWeek < thresh) {
    if (i == 0) {
      i <- i + 1 
    } else if (quantity == 0) {
      quantity <- quantity + 2
      i <- i + 1
    } else {
      quantity <- round(quantity * binSearchGrowth,0)
      i <- i + 1
    }
    
    # Gauge
    print(i)
    
    # Return data
    data       <- stockOutData(simulationData = simulationData,
                               weeksOut       = weeksOut,
                               SO_Cost        = SO_Cost,
                               sku_cost       = sku_cost,
                               carryCost      = carryCost,
                               on_hand        = on_hand,
                               quantity       = quantity,
                               nSims          = nrow(simulationData))
    minWeek    <- data[, minTC := min(totalCost)][totalCost == minTC, WEEKS_OUT][[1]]
    data1[[i]] <- data.table(MinWeek = minWeek, QuantityAvailable = quantity)
  }
  
  # Step 1 output
  dataFinal <- rbindlist(data1)
  return(dataFinal[c((nrow(dataFinal)-1),nrow(dataFinal)), QuantityAvailable])
}

# UpperBound <- dataFinal[c((nrow(dataFinal)-1),nrow(dataFinal)), QuantityAvailable]

# (Outer) Find midpoint for quantity such that min total costs is around half the forecast interval
MidPointQuantity <- function(UpperBound, simulationData, weeksOut, SO_Cost, carryCost) {
  j     <- 0
  left  <- UpperBound[1]
  right <- UpperBound[2]
  data1 <- list()
  while (left + 1 < right) {  
    quantity <- ceiling((left + right) / 2)
    j = j + 1
    
    # Gauge
    print(paste(j, left, right, sep = " "))
    
    # Define costs info
    data <- stockOutData(simulationData = simulationData,
                         weeksOut       = weeksOut,
                         SO_Cost        = SO_Cost,
                         sku_cost       = sku_cost,
                         carryCost      = carryCost,
                         on_hand        = on_hand,
                         quantity       = quantity,
                         nSims          = nrow(simulationData))
    minWeek    <- data[, minTC := min(totalCost)][totalCost == minTC, WEEKS_OUT]
    data1[[j]] <- data.table(MinWeek=minWeek, QuantityAvailable=quantity)
    if (minWeek < ceiling(weeksOut / 2)) {
      left <- quantity
    } else {
      right <- quantity
    }
  }
  
  # Quantity value to pass to optimization routine
  return(rbindlist(data1))
}

# StartingQuantity <- rbindlist(data1)
# quantity <- StartingQuantity[, QWeek := abs(MinWeek - ceiling(weeksOut / 2))]
# quantity <- quantity[QWeek == min(QWeek)]
# quantity <- quantity[order(QuantityAvailable)][["QuantityAvailable"]][[1]]
# Q <- length(quantity)

# Strategy Comparison
strategyCompare <- function(min_week,
                            shipping_costS1=SC1, shipping_costS2=SC2, 
                            strat1TimeAdd=ST1Add, strat1TimeSub=ST1Sub, 
                            strat2TimeAdd=ST2Add, strat2TimeSub=ST2Sub,
                            const, m1, m2, m3, m4, m5, m6, m7, m8, m9) {
  
  # Error handling
  if(min_week - strat1TimeSub < 0 | min_week - strat2TimeSub < 0) {
    return("Out of the minimum range")    
  }

  # Update cost function
  costs <- function(x) { 
    const  +
      m1 * x   +
      m2 * x^2 +
      m3 * x^3 +
      m4 * x^4 +
      m5 * x^5 +
      m6 * x^6 +
      m7 * x^7 +
      m8 * x^8 +
      m9 * x^9
  }
  
  # Integration of both strategies
  strategy1 <- tryCatch({as.numeric(
    as.character(
      substr(
        integrate(costs, 
                  lower = min_week - strat1TimeSub, 
                  upper = min_week + strat1TimeAdd)[[1]],
        1,5)[[1]]))}, error = function(x) return(1))
  strategy2 <- tryCatch({as.numeric(
    as.character(
      substr(
        integrate(costs, 
                  lower = min_week - strat2TimeSub, 
                  upper = min_week + strat2TimeAdd)[[1]],
        1,5)[[1]]))}, error = function(x) return(1))
  
  # Comparison weight
  weight <- (strat2TimeAdd + strat2TimeSub) / (strat1TimeAdd + strat1TimeSub)
  annualize <- (strat2TimeAdd + strat2TimeSub)/52
  
  # Obtain equal time length costs
  A <- weight*(strategy1+shipping_costS1)
  B <- strategy2+shipping_costS2
  
  # Store strategy choice text
  if(A <= B) {
    strat_choice <- paste0("Strategy: L",
                           strat1TimeSub,
                           " - R",
                           strat1TimeAdd,"is better")
  } else {
    strat_choice <- paste0("Strategy: L",
                           strat2TimeSub,
                           " - R",
                           strat2TimeAdd,
                           "is better")
  }
  
  # Store Total Cost
  if(A <= B) {
    totalCost <- A/annualize 
  } else {
    totalCost <- B/annualize
  }
  
  # Break even shipping cost for strategy1
  break_even_shipping_discount <- A/annualize - B/annualize
  
  # Return the pertinent information
  return(list(
    StrategyChoice            = strat_choice,
    MinCostWeek               = min_week, 
    BreakEvenShippingDiscount = break_even_shipping_discount,
    TotalCost                 = totalCost))
}

# Comparison Check Function
comparisonCheck <- function(min_week = a, 
                            WalkLeft, 
                            WalkRight, 
                            ST1Sub, 
                            ST1Add, 
                            SC1, 
                            SC2,
                            const, 
                            m1,m2,m3,m4,m5,m6,m7,m8,m9) {
  
  # Loop through NL possibilities
  temp_results <- list()
  j <- 0
  for (nl in WalkLeft) {
    
    # Increment iterator for collection purposes
    j <- j + 1
    
    # For building data.table to store results
    NL <- length(WalkLeft)
    NR <- length(WalkRight)
    MW <- max(NR,NL)
    
    # Set up storage table
    results <- data.table(StrategyChoice=as.character(rep("a",MW)),
                          MinCostWeek=as.double(rnorm(MW)), 
                          BreakEvenShippingDiscount=as.double(rnorm(MW)),
                          strat2Add=as.double(rnorm(MW)),
                          strat2Sub=as.double(rnorm(MW)),
                          TotalCost=as.double(rnorm(MW)))
    
    # Initialize counter
    k <- 0
    
    # Loop through NR possibilities
    for (nr in WalkRight) {
      k = k + 1
      ST2Sub <- nl 
      ST2Add <- nr
      
      # Run comparison function
      store <- strategyCompare(min_week,
                               shipping_costS1=SC1, shipping_costS2=SC2, 
                               strat1TimeAdd=ST1Add, strat1TimeSub=ST1Sub, 
                               strat2TimeAdd=ST2Add, strat2TimeSub=ST2Sub,
                               const, m1, m2, m3, m4, m5, m6, m7, m8, m9)
      
      # Store valid results
      if (store[[1]] == "Out of the minimum range") next
      set(results, k, 1, value = store[[1]])
      set(results, k, 2, value = store[[2]])
      set(results, k, 3, value = round(store[[3]],2))
      set(results, k, 4, value = ST2Add)
      set(results, k, 5, value = ST2Sub)
      set(results, k, 6, value = store[[4]])
    }
    if(results[order(-StrategyChoice)][1,1][[1]] == "a") {
      next
    } else {
      temp_results[[j]] <- results
    }
  }
  results <- rbindlist(temp_results)
  results <- results[BreakEvenShippingDiscount > 0][order(-BreakEvenShippingDiscount)]
  return(list(results, min_week))
}

# (Outer Final) Optimization Function
Optimize <- function(simulationData,
                     quantity,
                     sku_cost,
                     SO_Cost,
                     carryCost,
                     MaxOrderWindow,
                     nSims,
                     sku,
                     on_hand,
                     ShippingCostBase=25,
                     ShippingCostTest=25) {
  
  # Storage table
  reorderParameters <- data.table(SKU                            = rep("a",1),
                                  AvgUnitCost                    = rep(-720,1),
                                  ActualQuantityAvailable        = rep(-720,1),
                                  QuantityAvailableHypothetical  = rep(-720,1),
                                  ReorderQuantity                = rep(-720,1),
                                  MaxQuantity                    = rep(-720,1),
                                  MinQuantity                    = rep(-720,1),
                                  CurrentShippingCosts           = rep(-720,1),
                                  OrderFrequencyBest             = rep(-720,1),
                                  OrderFrequencyBaseline         = rep(-720,1),
                                  AnnualizedTotalCost            = rep(-720,1),
                                  AnnualizedSavingsOverBase      = rep(-720,1),
                                  BestStrategy                   = rep("a",1),
                                  MinCostWeek                    = rep(-720,1))

  # Pull in stockOut data for updated quantity value
  data <- stockOutData(simulationData,
                       MaxOrderWindow,
                       SO_Cost,
                       sku_cost,
                       carryCost,
                       on_hand,
                       quantity,
                       nSims)
  
  # Cost model
  cost1 <- lm(totalCost ~ WEEKS_OUT + I(WEEKS_OUT^2) + I(WEEKS_OUT^3) + 
                I(WEEKS_OUT^4) + I(WEEKS_OUT^5) + I(WEEKS_OUT^6) + I(WEEKS_OUT^7) + 
                I(WEEKS_OUT^8) + I(WEEKS_OUT^9),data=data)
  
  # Cost function coefficients
  const <- cost1$coefficients[[1]]
  m1    <- cost1$coefficients[[2]]
  m2    <- cost1$coefficients[[3]]
  m3    <- cost1$coefficients[[4]]
  m4    <- cost1$coefficients[[5]]
  m5    <- cost1$coefficients[[6]]
  m6    <- cost1$coefficients[[7]]
  m7    <- cost1$coefficients[[8]]
  m8    <- cost1$coefficients[[9]]
  m9    <- cost1$coefficients[[10]]
  
  # Define the linear regression prediction function for minimization
  costs <- function(x) { 
    -1*(const  +
          m1 * x   +
          m2 * x^2 +
          m3 * x^3 +
          m4 * x^4 +
          m5 * x^5 +
          m6 * x^6 +
          m7 * x^7 +
          m8 * x^8 +
          m9 * x^9)
  }
  
  # Define the linear regression prediction function for maximization
  temp_costs <- function(x) { 
    const  +
      m1 * x   +
      m2 * x^2 +
      m3 * x^3 +
      m4 * x^4 +
      m5 * x^5 +
      m6 * x^6 +
      m7 * x^7 +
      m8 * x^8 +
      m9 * x^9
  }
  
  # Shipping Costs
  SC1 <- ShippingCostBase
  SC2 <- ShippingCostTest
  
  # Get min cost week
  a  <- ceiling(ga(type = "real-valued",
                   fitness = costs,
                   min = 1,
                   max = MaxOrderWindow)@solution[1])
  
  # Find all weeks such that at least a single floored unit of average 
  #   inventory has changed compared to at min total cost week
  LoopTable <- data.table(WalkLeft = rep(9999,(a*a)*(a*a)), 
                          WalkRight = rep(9999,(a*a)*(a*a)))
  k = 0
  for (l in 0:(a - 1)) {
    for (j in 0:(MaxOrderWindow - a - 1)) {
      k = k + 1
      new <- floor(data[WEEKS_OUT == (a-l),"avgInventory"][[1]]) - 
             floor(data[WEEKS_OUT == (a+j),"avgInventory"][[1]])
      if(!is.null(new) & new >= 1) {
        set(LoopTable, i = k, j = 1, value = l)
        set(LoopTable, i = k, j = 2, value = j)
      }
    }
  }
  
  # Collect all possible strategies
  LoopTable <- LoopTable[WalkLeft != 9999]
  WalkLeft  <- unique(LoopTable[, "WalkLeft"][[1]])
  WalkRight <- sort(unique(LoopTable[, "WalkRight"][[1]]))
  costqlow  <- tryCatch({as.numeric(
    as.character(
      substr(
        integrate(temp_costs,
                  lower = a,
                  upper = a + ceiling(min(WalkRight[WalkRight > 0 & WalkRight != Inf]))),
        1,5)[[1]]))}, error = function(x) return(1))
  costqhigh <- tryCatch({as.numeric(
    as.character(
      substr(
        integrate(temp_costs, 
                  lower = a - floor(min(WalkLeft[WalkLeft > 0])), 
                  upper = a),
        1,5)[[1]]))}, error = function(x) return(1))
  
  # Define base strategy
  if (costqhigh < costqlow) {
    ST1Sub  <- max(1,min(WalkLeft[WalkLeft > 0]))
    ST1Add  <- 0
  } else {
    ST1Sub  <- 0
    ST1Add  <- max(1,min(WalkRight[WalkRight > 0]))
  }
  
  # Run comparison function
  temp <- tryCatch({comparisonCheck(min_week=a, 
                          WalkLeft, 
                          WalkRight, 
                          ST1Sub, 
                          ST1Add, 
                          SC1, 
                          SC2,
                          const, 
                          m1, m2, m3, m4, m5, m6, m7, m8, m9)},
                   error = function(x) data.table(StrategyChoice=as.character(rep("a",1)),
                                                  MinCostWeek=as.double(rnorm(1)), 
                                                  BreakEvenShippingDiscount=as.double(rnorm(1)),
                                                  strat2Add=as.double(rnorm(1)),
                                                  strat2Sub=as.double(rnorm(1)),
                                                  TotalCost=as.double(rnorm(1))))

  # Store results from comparison check
  results <- NA
  if (length(temp) == 2) {
    results  <- temp[[1]]
    min_week <- temp[[2]]      
  } else if (!is.na(temp)){
    results <- temp[[1]]
  } 
  
  # Collect reorder amount and reorder timing
  set(reorderParameters,1,1,value = sku) # SKU
  set(reorderParameters,1,2,value = round(sku_cost,2)) # AVG UNIT COST
  set(reorderParameters,1,3,value = on_hand) # ActualQuantityAvailable
  set(reorderParameters,1,4,value = quantity) # QuantityAvailableHypothetical
  if (is.data.table(results) && nrow(results) > 0) {
    # Find the reorder amount and reorder timing
    reorderCalculator <- function(data, results) {
      lookforward  <- results[order(-BreakEvenShippingDiscount)][1, 4][[1]]
      lookback     <- results[order(-BreakEvenShippingDiscount)][1, 5][[1]]
      TotalCost    <- results[order(-BreakEvenShippingDiscount)][1, 6][[1]]
      BestStrategy <- results[order(-BreakEvenShippingDiscount)][1, 1][[1]]
      MinQOH       <- floor(
        data[WEEKS_OUT == ceiling(min_week+lookforward), avgInventory][[1]])
      MaxQOH       <- ceiling(
        data[WEEKS_OUT == ceiling(min_week-lookback), avgInventory][[1]])
      return(list(amount         = ceiling(MaxQOH - MinQOH), 
                  OrderFrequency = lookforward + lookback,
                  MinQOH,
                  TotalCost,
                  BestStrategy))
    }
    
    collect <- reorderCalculator(data, results)
    set(reorderParameters,1,5,value = max(
         0,ceiling(collect[[3]] + collect[[1]] - on_hand))
       )
    set(reorderParameters,1,6,value = ceiling(collect[[3]] + collect[[1]]))
    set(reorderParameters,1,7,value = ceiling(collect[[3]]))
    set(reorderParameters,1,8,value = SC1)
    set(reorderParameters,1,9,value = collect[[2]])
    set(reorderParameters,1,10,value = (ST1Sub + ST1Add))
    set(reorderParameters,1,11,value = round(collect[[4]],2))
    set(reorderParameters,1,12,value = round(
         results[order(-BreakEvenShippingDiscount)][1, 3][[1]],2)
       )
    set(reorderParameters,1,13,value = collect[[5]])
    set(reorderParameters,1,14,value = a)
  }

  return(list(reorderParameters, data))
}


