###################################################
# Data preparation environment setup
###################################################

# Set the storage location for files (overwrite the getwd())
data_path <- paste0("C:\\Users\\aantico\\Desktop\\Work\\H20\\Demand Modeling for Strategic Management\\data") 
setwd(data_path)

# Set up environment
packagesReq <- c("curl","data.table","zoo","lubridate")
packs <- packagesReq[!(packagesReq %in% installed.packages()[,"Package"])]
if(length(packs)) install.packages(packs)

# Load libraries
library(data.table)
library(zoo)
library(lubridate)
library(curl)
library(AdrianModelingTools)

# Define Data Preparation Functions----------------

# Data import and cleanup----------------
DataSetUp          <- function() {
  dropbox_URL <- "https://www.dropbox.com/s/bx9v0vbwsq2h3n8/Online%20Retail.csv?dl=1"
  SKU_TXNS    <- data.table::fread(dropbox_URL, header = TRUE, stringsAsFactors = FALSE)
  rm(dropbox_URL)
  
  # Handle the InvoiceDate column from Excel
  tempDatesFun <- Vectorize(function(x) {
    strsplit(x, " ")[[1]][1]
  })
  
  SKU_TXNS[, InvoiceDate := tempDatesFun(InvoiceDate)]
  SKU_TXNS[, InvoiceDate := as.Date(InvoiceDate, "%m/%d/%Y")]  
  
  # Test code quickly
  # SKU_TXNS <- SKU_TXNS[1:25000]

  # Add in a TotalSales column
  SKU_TXNS[, TotalSales := Quantity * UnitPrice]
  
  # Remove records with missing customer id's or bad values
  SKU_TXNS <- na.omit(SKU_TXNS, cols = "CustomerID")
  SKU_TXNS <- SKU_TXNS[TotalSales > 0]
  
  # Change names
  setnames(SKU_TXNS, c("Quantity", "StockCode", "InvoiceDate"), c("qty", "sku", "date"))
  
  # Save a copy before final column removal for later
  SkuCostData <- SKU_TXNS
  save(SkuCostData, file = paste0(data_path,"/SkuCostData.Rdata"))
  
  # Remove columns
  keep <- c("sku", "qty", "date")
  SKU_TXNS <- SKU_TXNS[, ..keep]
  
  # Aggregate to the daily level
  SKU_TXNS <- SKU_TXNS[, .(qty = sum(qty)), by = c("sku","date")]
  
  # Save data
  save(SKU_TXNS, file = paste0(data_path,"/SKU_TXNS.Rdata"))
  return(SKU_TXNS)
}

# Define metadata generator----------------
MetadataGenerator  <- function(data,
                               min_window = 1,
                               min_txn_records = 2,
                               DateInterval = "day",
                               DateDiffUnits = "day") {
  
  # define max date to sample from
  max_date <- data[, max(date)][[1]] - 60*60*24*7*min_window
  
  # Set up base table
  sku <- data[, .(.N, floor_date(as.Date(max_date, unit = DateInterval))), by = sku][order(-N)] 
  
  # Gather second to last distinct date by sku
  temp <- data[, .(.N, date), by=sku][order(sku,-date)] 
  temp <- temp[, sum(N), by = .(sku,date)]
  temp <- temp[, txn := .N:1, by = sku]
  temp <- temp[txn == min_txn_records]
  temp[, txn := NULL]
  setnames(temp,"date","STL_Issue_Date")
  temp <- temp[,c("sku","STL_Issue_Date")]
  setkey(temp,sku)
  
  # Merge, change names, filter out infrequent sku's
  sku <- temp[sku]
  setnames(sku, "N", "Issuances")
  setnames(sku, "V2", "Max_Issue_Date")
  skus <- sku[, Date_Range := as.numeric(difftime(Max_Issue_Date,STL_Issue_Date, units = DateDiffUnits))][order(-Issuances)]
  
  # Remove sku's with less than two distinct past dates
  sku_metadata <- skus[sku != '*' & Issuances >= 2][Date_Range > 0]
  save(sku_metadata, file = "sku_metadata.Rdata")
  
  # Set up looping matrix
  sku_list <- as.matrix(sku_metadata[, "sku"])
  save(sku_list, file = "sku_list.Rdata")
  return(list(sku_list,sku_metadata))
}

# Build Modeling Data for single sku----------------
TrainingGenerator  <- function(sub_sku,
                               randomStartDate,
                               targetWindow){
  
  # historical data <--> point in time <--> target window
  # Split data into previous history and target window data and sort for created features
  histDemandRaw <- sub_sku[as.Date(date) < randomStartDate]
  
  # Data within target window
  targetDemand  <- sub_sku[date >= randomStartDate & 
                             date - targetWindow <= randomStartDate]
  
  # Remove meta data for feature creation set
  features <- histDemandRaw[order(-date)][, date := NULL][1,]
  features[, target_window := targetWindow]
  
  #########################################################
  # Final Prep - remove nuisance columns and rename columns
  #########################################################
  
  # Remove data and rename target variable
  keep <- c("qty")
  targetDemand <- targetDemand[, ..keep]
  setnames(targetDemand, old = "qty", new = "TARGET_QTY")

  ############################################
  # Create Targets & Merge Features
  ############################################
  
  # Merge Features and Targets
  if(nrow(targetDemand) != 0) {
    TargetCount <- cbind(targetDemand[, .N],features)
    TargetSize  <- cbind(targetDemand, features)
  } else {
    TargetCount <- cbind(data.table(N = 0),features)
    TargetSize  <- cbind(data.table(TARGET_QTY = 0),features)
  }
  
  # Output data file
  return(list(TargetCount, TargetSize))
}

# Generate full data----------------
DataGenerator      <- function(sku_list,
                               sku_metadata,
                               SKU_TXNS,
                               maxTargetWindow,
                               DateDiffUnits = "week",
                               Batches = 100,
                               PowerSample = 0.75) {
  
  # DateDiffUnits
  if(DateDiffUnits == "week") {
    DateUnit <- 7
  } else if (DateDiffUnits == "day") {
    DateUnit <- 1
  } else if (DateDiffUnits == "month") {
    DateUnit <- 30
  }
  
  # Ensure sku_metadata STL_Issue_Date is a date
  sku_metadata[, STL_Issue_Date := as.Date(STL_Issue_Date)]
  
  print("Feature Engineering begin")
  
  # Feature engineering on entire data set (scoring will be for single sku at a time)
  SKU_TXNS[, ':=' (month = lubridate::month(date), week = lubridate::week(date))]
  
  # Add in the time varying features 
  SKU_TXNS <- GDL_Feature_Engineering(SKU_TXNS,
                                      lags           = c(seq(1,5,1)),
                                      periods        = c(3,5,10,15,20,25), 
                                      statsFUNs      = c(function(x) quantile(x, probs = 0.1, na.rm = TRUE),
                                                         function(x) quantile(x, probs = 0.9, na.rm = TRUE),
                                                         function(x) mean(x, na.rm = TRUE),
                                                         function(x) sd(x, na.rm = TRUE),
                                                         function(x) quantile(x, probs = 0.25, na.rm = TRUE),
                                                         function(x) quantile(x, probs = 0.75, na.rm = TRUE)),
                                      statsNames     = c("q10","q90","mean","sd","q25","q75"),
                                      targets        = c("qty"),
                                      groupingVars   = c("sku"),
                                      sortDateName   = "date",
                                      timeDiffTarget = c("ISSUE_GAP"),
                                      timeAgg        = "days",
                                      WindowingLag   = 0,
                                      Type           = "Lag",
                                      Timer          = TRUE,
                                      SkipCols       = FALSE,
                                      SimpleImpute   = TRUE)
  
  # Save file at this point in case of error
  save(SKU_TXNS, file = "./SKU_TXNS.Rdata")
  
  # Batch sku_list
  N <- Batches
  batchSize <- floor(nrow(sku_list)/N)
  batchList <- list()
  for (n in 1:N) {
    if (n == 1) {
      batchList[[n]] <- sku_list[1:batchSize]
    } else if (n < N){
      batchList[[n]] <- sku_list[((n-1)*batchSize+1):(n*batchSize)]
    } else {
      batchList[[n]] <- sku_list[((n-1)*batchSize+1):nrow(sku_list)]
    }
  }
  
  # loop through batchList
  for (bl in seq_along(batchList)) {
    newlist <- batchList[[bl]]
    SMD     <- list()
    CMD     <- list()
    j       <- 0
    
    # Create Size Modeling Data via bootstrapping
    for(sku_ind in newlist) {
  
      # Track progress
      print(sku_ind)
      
      # Set iterations
      issuances  <- as.numeric(ceiling(sku_metadata[sku == sku_ind, "Issuances"][[1]]))
      iterations <- ceiling(issuances^PowerSample)
      
      # Check to ensure issuances and iterations exist
      if(length(issuances) == 0 || length(iterations) == 0) next
      j = j + 1
      
      # Initialize / reset storage lists
      countData <- list()
      sizeData  <- list()
      
      # Subset data before looping through a single sku
      sub_sku <- SKU_TXNS[sku == sku_ind]
      
      # Set date range
      dateRange <- sku_metadata[sku == sku_ind, "Date_Range"][[1]]
      
      # Data generator
      for (i in 1:iterations) {
        
        # Set parameters
        randomStartDate  <- sku_metadata[sku == sku_ind, "STL_Issue_Date"][[1]] + DateUnit  * ceiling(sample(1:1*dateRange, 1))
        targetWindowMax  <- ceiling(min(as.numeric(difftime(floor_date(sku_metadata[sku == sku_ind, "Max_Issue_Date"][[1]], unit = DateDiffUnits), randomStartDate, units = DateDiffUnits)), maxTargetWindow))
        targetWindow     <- sample(1:targetWindowMax,1)
        
        # Create data
        data <- TrainingGenerator(sub_sku, randomStartDate, targetWindow)
        countData[[i]] <- data[[1]]
        sizeData[[i]]  <- data[[2]]
      }
      # Collect data.tables    
      CMD[[j]] <- rbindlist(countData)
      SMD[[j]] <- rbindlist(sizeData)
    }
    # Append data collection lists
    count_model_data <- rbindlist(CMD)
    size_model_data  <- rbindlist(SMD)

    # Save modeling data sets
    save(count_model_data, file = paste0(data_path, "/count_model_data",bl,".Rdata"))
    save(size_model_data, file = paste0(data_path, "/size_model_data",bl,".Rdata"))
    rm(count_model_data, size_model_data)
  }
  return(batches <- seq_along(batchList))
}

# Define model data prep and save to file----------------
ModelDataPrepSave  <- function(batches,
                               KeepBatches = FALSE) {
  
  # Collect count data
  for (i in seq_along(batches)) {
    # Load data one by one
    count_model_data <- fread(paste0(data_path,"/count_model_data",i,".csv"))
    if (i == 1) {
      final_count_model_data <- count_model_data
      rm(count_model_data)
      file.remove(paste0(data_path,"/count_model_data",i,".csv"))
    } else {
      final_count_model_data <- rbindlist(list(final_count_model_data, 
                                               count_model_data))
      rm(count_model_data)
      file.remove(paste0(data_path,"/count_model_data",i,".csv"))
    }
  }
  
  # Set target variable as first column 
  setcolorder(final_count_model_data, c(2,1,3:ncol(final_count_model_data)))
  
  # Store data
  fwrite(final_count_model_data, file = "final_count_model_data.csv")
  
  # Remove data
  rm(final_count_model_data)
  
  # Collect size data
  for (i in seq_along(batches)) {
    size_model_data <- fread(paste0(data_path,"/size_model_data",i,".csv"))
    if (i == 1) {
      final_size_model_data <- size_model_data
      rm(size_model_data)
      if(!KeepBatches) {
        file.remove(paste0(data_path,"/size_model_data",i,".csv"))
      }
    } else {
      final_size_model_data <- rbindlist(list(final_size_model_data, 
                                              size_model_data))
      rm(size_model_data)
      if(!KeepBatches) {
        file.remove(paste0(data_path,"/size_model_data",i,".csv"))
      }
    }
  }
  
  # Remove records where target == 0
  qty_model_data <- final_size_model_data[TARGET_QTY != 0]
  
  # Set target variable as first column 
  setcolorder(qty_model_data, c(2,1,3:ncol(qty_model_data)))
  
  # Store data
  fwrite(qty_model_data, file = "qty_model_data.csv")
}

# Run all steps----------------
RunFunction        <- function() {
  
  # Pull data and save it
  SKU_TXNS <- tryCatch({DataSetUp()}, 
                       error = DataSetUp())
  
  # Run metatdata generator
  SkuData <- MetadataGenerator(SKU_TXNS, 
                               min_window = 1, 
                               min_txn_records = 2, 
                               DateInterval = "week", 
                               DateDiffUnits = "week") 
  
  # Run data generator
  model_data <- DataGenerator(SkuData[[1]], 
                              SkuData[[2]], 
                              SKU_TXNS, 
                              maxTargetWindow = 52, 
                              DateDiffUnits = "week",
                              Batches = 100,
                              PowerSample = 0.75)

  # Run model data prep and save function
  ModelDataPrepSave(model_data,
                    KeepBatches = FALSE)
}

###################################################
# Run Process
###################################################

# Build modeling data sets----------------
RunFunction()




