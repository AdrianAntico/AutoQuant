#' PathNormalizer Converts path files to proper path files
#'
#' PathNormalizer Converts path files to proper path files
#'
#' @author Adrian Antico
#' @family System Functions
#' @param Path This is the path file you are supplying#'
#' @return Returns a proper path file string
#' @export
PathNormalizer <- function(Path) {
  return(gsub("\\\\", "/", Path))
}

#' PathNormalizer Converts path files to proper path files
#'
#' PathNormalizer Converts path files to proper path files
#'
#' @author Adrian Antico
#' @family System Functions
#' @param ProjectName This is the name of a project which will be the name of the file created in the root folder
#' @param RootPath This is the path file to the root folder
#' @param ExistsButNoProjectList Set to TRUE if the folder exists but not the ProjectList file
#' @return Returns a proper path file string
#' @export
CreateProjectFolders <- function(ProjectName = input$ID_NewProjectName, 
                                 RootPath = input$ID_Root_Folder,
                                 ExistsButNoProjectList = FALSE,
                                 Local = FALSE) {
  
  # Create Collection List----
  ProjectList <- list()
  
  # Store Project Name----
  ProjectList[["ProjectName"]] <- ProjectName
  
  # Define Project Folder Root Path----
  if(Local) {
    ProjectPath <- file.path(PathNormalizer(RootPath),ProjectName)  
  } else {
    ProjectPath <- paste0("./",ProjectName)
  }
  
  ProjectList[["ProjectFolderPath"]] <- ProjectPath
  
  # Create Project Folder----
  if(!ExistsButNoProjectList) {
    
    # Create Data folder----
    DataPath <- file.path(ProjectPath,"Data")
    dir.create(path =  DataPath, showWarnings = TRUE, recursive = TRUE)  
    ProjectList[["DataFolderPath"]] <- DataPath
    
    # Create Models folder----
    ModelsPath <- file.path(ProjectPath,"Models")
    dir.create(path =  ModelsPath, showWarnings = TRUE, recursive = TRUE)
    ProjectList[["ModelsFolderPath"]] <- ModelsPath
    
    # Create MetaData folder----
    MetaDataPath <- file.path(ProjectPath,"MetaData")
    dir.create(path =  MetaDataPath, 
               showWarnings = TRUE, recursive = TRUE)
    ProjectList[["MetaDataPath"]] <- MetaDataPath
    
    # Save ProjectList to File----
    save(ProjectList, file = file.path(MetaDataPath,paste0(ProjectName,"_ProjectList.Rdata")))
  
  } else {
    
    # Create Data folder----
    DataPath <- file.path(ProjectPath,"Data")
    ProjectList[["DataFolderPath"]] <- DataPath
    
    # Create Models folder----
    ModelsPath <- file.path(ProjectPath,"Models")
    ProjectList[["ModelsFolderPath"]] <- ModelsPath
    
    # Create MetaData folder----
    MetaDataPath <- file.path(ProjectPath,"MetaData")
    ProjectList[["MetaDataPath"]] <- MetaDataPath
    
    # Save ProjectList to File----
    save(ProjectList, file = file.path(MetaDataPath,paste0(ProjectName,"_ProjectList.Rdata")))
  }
  
  # Return List to Use in Other Modules----
  return(ProjectList)
}


# Refresh forecasting data----
library(data.table)
ProjectRoot <- "C:/Users/aantico/Desktop/Forecasting"
ForecastOutput <- "C:/Users/aantico/Desktop/Forecasting/ForecastOutput"
RawData <- "C:/Users/aantico/Desktop/Forecasting/RawData"
ModelData <- "C:/Users/aantico/Desktop/Forecasting/ModelData"
Modeling <- "C:/Users/aantico/Desktop/Forecasting/Modeling"
load(file.path(normalizePath(RawData), "MARS_Segments.Rdata"))
# source(file = file.path(normalizePath(ProjectRoot), "DataPrepare.R"), echo = TRUE)

# For testing code
# seg <- "Agg"
# MARS_Segments <- "Agg"

# Load up data----
InquiryFC <- data.table::fread(file.path(normalizePath(ModelData), "FC_2020.csv"))
MaxInquiryDate <- InquiryFC[, max(InquiryDate, na.rm = TRUE)]
MaxCohortDays <- 65

# Generate forecasts----
for(seg in MARS_Segments) {

  # Forecasting data generator----
  data <- data.table::fread(file.path(normalizePath(ModelData), paste0(seg, "_BaseData.csv")))
  MaxDateForecasted <- data[, max(InquiryDate, na.rm = TRUE)]
  FC_Period <- 0L

  # Loop through all periods to forecast----
  while(MaxDateForecasted < MaxInquiryDate) {

    # Increment FC_Period----
    FC_Period <- FC_Period + 1L
    for(bla in seq_len(20L)) print(paste0("Working on Forecast for period: ", FC_Period, " ::: Periods left to forecast: ", difftime(MaxInquiryDate, MaxDateForecasted)))

    # Prepare data----
    print("# Prepare data----")
    if(class(data$InquiryDate) != "Date") data[, ":=" (InquiryDate = as.Date(InquiryDate), TransferDate = as.Date(TransferDate))]
    data[, ScoreRecords := 2]
    if(class(data$CohortDaysOut) == "factor") data[, CohortDaysOut := as.numeric(as.character(CohortDaysOut))]
    maxct <- data[, list(CohortDaysOut = max(CohortDaysOut), ScoreRecords = data.table::first(ScoreRecords)), by = list(InquiryDate)]
    maxct[, CohortDaysOut := CohortDaysOut + 1L]
    maxct[, TransferDate := as.Date(InquiryDate) + lubridate::days(CohortDaysOut)]
    maxct[, MARS_Segment := eval(seg)]

    # Subset data and update data----
    print("# Subset data and update data----")
    InquiryForecast <- InquiryFC[MARS_Segment == eval(seg)]
    InquiryForecast <- InquiryForecast[InquiryDate > max(maxct$InquiryDate)]
    NextInquiryDay <- InquiryForecast[1L]
    NextInquiryDay[, ":=" (InquiryDate = as.Date(InquiryDate), TransferDate = as.Date(TransferDate))]
    NextInquiryDay[, ScoreRecords := 1]
    InquiryForecast <- InquiryForecast[2L:.N]

    # Merge on next date of Inquiries----
    print("# Merge on next date of Inquiries----")
    maxct <- merge(maxct, data[, list(Inquiry = data.table::first(Inquiry)), by = list(InquiryDate)][, InquiryDate := as.Date(InquiryDate)], by = "InquiryDate", all.x = TRUE)
    maxct[, ":=" (Transfer = 0, Rate = 0)]
    maxct[, ScoreRecords := 1]
    maxct <- data.table::rbindlist(list(maxct, NextInquiryDay), use.names = TRUE)

    # Remove CohortDaysOut beyond MaxCohortDays days----
    maxct <- maxct[CohortDaysOut <= MaxCohortDays]
    ScoreDate <- maxct[, max(TransferDate)]

    # Stack onto modeling data for seg----
    print("# Stack onto modeling data for seg----")
    data <- data.table::rbindlist(list(data, maxct), fill = TRUE, use.names = TRUE)
    rm(maxct)

    # Feature Engineering: Calendar & Holiday Variables----
    print("# Feature Engineering----")
    data <- RemixAutoML::CreateCalendarVariables(data, DateCols = c("InquiryDate", "TransferDate"), AsFactor = FALSE, TimeUnits = c("wday","mday","yday","week", "isoweek", "month", "quarter", "year"))
    data <- RemixAutoML::CreateHolidayVariables(data, DateCols = c("InquiryDate"), HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"), Holidays = NULL, GroupingVars = "MARS_Segment", Print = FALSE)
    data.table::setnames(data, old = "HolidayCounts", new = "InquiryDateHolidayCounts")
    data <- RemixAutoML::CreateHolidayVariables(data, DateCols = c("TransferDate"), HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"), Holidays = NULL, GroupingVars = "MARS_Segment", Print = FALSE)
    data.table::setnames(data, old = "HolidayCounts", new = "TransferDateHolidayCounts")
    data.table::setorderv(data, cols = c("InquiryDate","CohortDaysOut"), c(1L, 1L))

    # Feature Engineering: Transfers and Rate AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    data.table::set(temp, j = "InquiryDate", value = as.character(temp[["InquiryDate"]]))
    Lagss                 = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L))
    MA_RollWindowss       = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L), "month" = c(1L,2L))
    SD_RollWindowss       = NULL
    Skew_RollWindowss     = NULL
    Kurt_RollWindowss     = NULL
    Quantile_RollWindowss = NULL
    Quantiles_Selecteds   = c("q5")
    temp <- RemixAutoML::AutoLagRollStatsScoring(

      # Data
      data                 = temp,
      DateColumn           = "TransferDate",
      Targets              = c("Transfer","Rate"),
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      HierarchyGroups      = NULL,
      IndependentGroups    = "InquiryDate",
      TimeUnit             = c("days"),
      TimeGroups           = c("day","week","month"),
      TimeUnitAgg          = c("days"),

      # Services
      TimeBetween          = NULL,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = FALSE,

      # Calculated Columns
      Lags                 = Lagss,
      MA_RollWindows       = MA_RollWindowss,
      SD_RollWindows       = SD_RollWindowss,
      Skew_RollWindows     = Skew_RollWindowss,
      Kurt_RollWindows     = Kurt_RollWindowss,
      Quantile_RollWindows = Quantile_RollWindowss,
      Quantiles_Selected   = Quantiles_Selecteds,
      Debug                = TRUE)

    # Join datasets
    print("# Combine datasets----")
    temp[, ":=" (InquiryDate = as.Date(InquiryDate))]
    data <- merge(data, temp[, .SD, .SDcols = c("InquiryDate","TransferDate", setdiff(names(temp),names(data)))], by = c("InquiryDate","TransferDate"), all.x = TRUE)
    rm(temp)

    # Feature Engineering: TransferDateHolidayCounts AutoLagRollStatsScoring----
    print("# TransferHolidayCounts AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    data.table::set(temp, j = "InquiryDate", value = as.character(temp[["InquiryDate"]]))
    Lagss                 = c(1L,2L,7L)
    MA_RollWindowss       = c(3L,7L)
    SD_RollWindowss       = NULL
    Skew_RollWindowss     = NULL
    Kurt_RollWindowss     = NULL
    Quantile_RollWindowss = NULL
    Quantiles_Selecteds   = c("q5")
    temp <- RemixAutoML::AutoLagRollStatsScoring(

      # Data
      data                 = temp,
      DateColumn           = "TransferDate",
      Targets              = c("TransferDateHolidayCounts"),
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      HierarchyGroups      = NULL,
      IndependentGroups    = "InquiryDate",
      TimeUnit             = c("days"),
      TimeGroups           = c("day"),
      TimeUnitAgg          = c("days"),

      # Services
      TimeBetween          = NULL,
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = FALSE,

      # Calculated Columns
      Lags                 = Lagss,
      MA_RollWindows       = MA_RollWindowss,
      SD_RollWindows       = SD_RollWindowss,
      Skew_RollWindows     = Skew_RollWindowss,
      Kurt_RollWindows     = Kurt_RollWindowss,
      Quantile_RollWindows = Quantile_RollWindowss,
      Quantiles_Selected   = Quantiles_Selecteds,
      Debug                = TRUE)

    # Join datasets
    print("# Combine datasets----")
    temp[, ":=" (InquiryDate = as.Date(InquiryDate))]
    data <- merge(data, temp[, .SD, .SDcols = c("InquiryDate","TransferDate", setdiff(names(temp),names(data)))], by = c("InquiryDate","TransferDate"), all.x = TRUE)
    rm(temp)

    # Feature Engineering: Inquiry AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    temp <- temp[, list(Inquiry = data.table::first(Inquiry)), by = list(InquiryDate)]
    temp[, ScoreRecords := data.table::fifelse(InquiryDate == ScoreDate, 1, 2)]
    data.table::set(temp, j = "InquiryDate", value = as.Date(temp[["InquiryDate"]]))
    Lagss                 = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L,10L,12L,25L,26L))
    MA_RollWindowss       = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L,10L,12L,20L,24L), "month" = c(6L,12L))
    SD_RollWindowss       = NULL
    Skew_RollWindowss     = NULL
    Kurt_RollWindowss     = NULL
    Quantile_RollWindowss = NULL
    Quantiles_Selecteds   = c("q5")
    temp <- RemixAutoML::AutoLagRollStatsScoring(

      # Data
      data                 = temp,
      DateColumn           = "InquiryDate",
      Targets              = c("Inquiry"),
      HierarchyGroups      = NULL,
      IndependentGroups    = NULL,
      TimeGroups           = c("day","week","month"),
      TimeUnitAgg          = c("day"),

      # Services
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      TimeBetween          = NULL,
      TimeUnit             = "day",
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = FALSE,

      # Calculated Columns
      Lags                 = Lagss,
      MA_RollWindows       = MA_RollWindowss,
      SD_RollWindows       = SD_RollWindowss,
      Skew_RollWindows     = Skew_RollWindowss,
      Kurt_RollWindows     = Kurt_RollWindowss,
      Quantile_RollWindows = Quantile_RollWindowss,
      Quantiles_Selected   = Quantiles_Selecteds,
      Debug                = TRUE)

    # Join datasets
    print("# Combine datasets----")
    temp[, ":=" (InquiryDate = as.Date(InquiryDate))]
    data <- merge(data, temp[, .SD, .SDcols = c("InquiryDate", setdiff(names(temp),names(data)))], by = c("InquiryDate"), all.x = TRUE)
    rm(temp)

    # Feature Engineering: Total Transfers by InquiryDate AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    temp <- temp[TransferDate == InquiryDate, list(Transfer = sum(Transfer)), by = list(InquiryDate)]
    temp[, ScoreRecords := data.table::fifelse(InquiryDate == ScoreDate, 1, 2)]
    data.table::set(temp, j = "InquiryDate", value = as.Date(temp[["InquiryDate"]]))
    Lagss                 = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L,10L,12L,25L,26L))
    MA_RollWindowss       = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L,10L,12L,20L,24L), "month" = c(6L,12L))
    SD_RollWindowss       = NULL
    Skew_RollWindowss     = NULL
    Kurt_RollWindowss     = NULL
    Quantile_RollWindowss = NULL
    Quantiles_Selecteds   = c("q5")
    temp <- RemixAutoML::AutoLagRollStatsScoring(

      # Data
      data                 = temp,
      DateColumn           = "InquiryDate",
      Targets              = c("Transfer"),
      HierarchyGroups      = NULL,
      IndependentGroups    = NULL,
      TimeGroups           = c("day","week","month"),
      TimeUnitAgg          = c("day"),

      # Services
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      TimeBetween          = NULL,
      TimeUnit             = "day",
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = TRUE,

      # Calculated Columns
      Lags                 = Lagss,
      MA_RollWindows       = MA_RollWindowss,
      SD_RollWindows       = SD_RollWindowss,
      Skew_RollWindows     = Skew_RollWindowss,
      Kurt_RollWindows     = Kurt_RollWindowss,
      Quantile_RollWindows = Quantile_RollWindowss,
      Quantiles_Selected   = Quantiles_Selecteds,
      Debug                = TRUE)

    # Join datasets
    print("# Combine datasets----")
    temp[, ":=" (InquiryDate = as.Date(InquiryDate))]
    data <- merge(data, temp[, .SD, .SDcols = c("InquiryDate", setdiff(names(temp),names(data)))], by = c("InquiryDate"), all.x = TRUE)
    rm(temp)

    # Feature Engineering: InquiryDateHolidayCounts AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    temp <- data.table::copy(data)
    temp <- temp[, list(InquiryDateHolidayCounts = max(InquiryDateHolidayCounts)), by = list(InquiryDate)]
    temp[, ScoreRecords := data.table::fifelse(InquiryDate == ScoreDate, 1, 2)]
    data.table::set(temp, j = "InquiryDate", value = as.Date(temp[["InquiryDate"]]))
    Lagss                 = c(1L,2L,7L)
    MA_RollWindowss       = c(3L,7L)
    SD_RollWindowss       = NULL
    Skew_RollWindowss     = NULL
    Kurt_RollWindowss     = NULL
    Quantile_RollWindowss = NULL
    Quantiles_Selecteds   = c("q5")
    print(str(temp))
    temp <- RemixAutoML::AutoLagRollStatsScoring(

      # Data
      data                 = temp,
      DateColumn           = "InquiryDate",
      Targets              = c("InquiryDateHolidayCounts"),
      HierarchyGroups      = NULL,
      IndependentGroups    = NULL,
      TimeGroups           = c("day"),
      TimeUnitAgg          = c("day"),

      # Services
      RowNumsID            = "ScoreRecords",
      RowNumsKeep          = 1,
      TimeBetween          = NULL,
      TimeUnit             = "day",
      RollOnLag1           = TRUE,
      Type                 = "Lag",
      SimpleImpute         = TRUE,

      # Calculated Columns
      Lags                 = Lagss,
      MA_RollWindows       = MA_RollWindowss,
      SD_RollWindows       = SD_RollWindowss,
      Skew_RollWindows     = Skew_RollWindowss,
      Kurt_RollWindows     = Kurt_RollWindowss,
      Quantile_RollWindows = Quantile_RollWindowss,
      Quantiles_Selected   = Quantiles_Selecteds,
      Debug                = TRUE)

    # Join datasets
    print("# Combine datasets----")
    temp[, ":=" (InquiryDate = as.Date(InquiryDate))]
    data <- merge(data, temp[, .SD, .SDcols = c("InquiryDate", setdiff(names(temp),names(data)))], by = c("InquiryDate"), all.x = TRUE)
    rm(temp)

    # Model data prep----
    print("# Model data prep----")
    data <- RemixAutoML::ModelDataPrep(
      data         = data,
      Impute       = TRUE,
      CharToFactor = FALSE,
      FactorToChar = FALSE,
      IntToNumeric = FALSE,
      DateToChar   = FALSE,
      RemoveDates  = FALSE,
      MissFactor   = "0",
      MissNum      = -1,
      IgnoreCols   = NULL)

    # Type Change: CorhortDaysOut as numeric and the dates as Dates----
    print("# Convert features types to correct ones----")
    if(!all(class(data$CohortDaysOut) %chin% "numeric")) data[, CohortDaysOut := as.numeric(as.character(CohortDaysOut))]
    if(!all(class(data$InquiryDate) %chin% "Date")) data[, InquiryDate := as.Date(InquiryDate)]
    if(!all(class(data$TransferDate) %chin% "Date")) data[, TransferDate := as.Date(TransferDate)]

    # Load model artifacts----
    print("# Load model artifacts----")
    if(FC_Period == 1L) load(file = file.path(normalizePath(Modeling), paste0(seg, "_FinalTrain.Rdata"))) #, envir = new.env())

    # Score Model----
    print("# Score Model----")
    temp1 <- data.table::copy(data)
    temp <- temp1[ScoreRecords == 1]
    Features <- TestModel$ColNames[[1]]
    temp <- RemixAutoML::AutoCatBoostScoring(
      TargetType = "regression",
      ScoringData = temp,
      FeatureColumnNames = Features,
      IDcols = names(temp)[!names(temp) %in% Features],
      ModelObject = TestModel$Model,
      ModelPath = NULL,
      ModelID = "ModelTest",
      ReturnFeatures = FALSE,
      MultiClassTargetLevels = NULL,
      TransformNumeric = FALSE,
      BackTransNumeric = FALSE,
      TargetColumnName = "Rate",
      TransformationObject = TestModel$TransformationResults,
      TransID = NULL,
      TransPath = NULL,
      MDP_Impute = TRUE,
      MDP_CharToFactor = TRUE,
      MDP_RemoveDates = TRUE,
      MDP_MissFactor = "0",
      MDP_MissNum = -1,
      RemoveModel = FALSE)

    # Update forecast data----
    print("# Update forecast data----")
    temp1[ScoreRecords == 1, Rate := temp[which(Predictions < 0), Predictions := 0][[1]]]
    temp1[ScoreRecords == 1, Transfer := Rate * (Inquiry + 1)]
    temp1 <- temp1[ScoreRecords == 1, .SD, .SDcols = c("InquiryDate","TransferDate","CohortDaysOut","MARS_Segment","Inquiry","Transfer","Rate")]
    data <- data.table::rbindlist(list(data[ScoreRecords != 1, .SD, .SDcols = c("InquiryDate","TransferDate","CohortDaysOut","MARS_Segment","Inquiry","Transfer","Rate")], temp1), fill = TRUE, use.names = TRUE)

    # Save forecasts to file----
    data.table::fwrite(data, file = file.path(normalizePath(ForecastOutput), paste0(seg, "_Forecasts.csv")))

    # Update MaxDateForecasted to know when to stop----
    MaxDateForecasted <- data[, max(InquiryDate)]
  }
  rm(data, TestModel)
}

