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
# source(file = file.path(normalizePath(ProjectRoot), "DataPrepare.R"), echo = TRUE)

# Define paths and load MARS_Segments
ForecastOutput <- "C:/Users/aantico/Desktop/Forecasting/ForecastOutput"
RawData <- "C:/Users/aantico/Desktop/Forecasting/RawData"
ModelData <- "C:/Users/aantico/Desktop/Forecasting/ModelData"
Modeling <- "C:/Users/aantico/Desktop/Forecasting/Modeling"
load(file.path(normalizePath(ModelData), "MARS_Segments.Rdata"))

# For testing code
# seg <- "Agg"

# Load up data----
InquiryFC <- data.table::fread(file.path(normalizePath(ModelData), "FC_2020.csv"))
MaxInquiryDate <- InquiryFC[, max(InquiryDate, na.rm = TRUE)]

# Generate forecasts----
for(seg in MARS_Segments) {

  # Forecasting data generator----
  data <- data.table::fread(file.path(normalizePath(ModelData), paste0(seg, "_BaseData.csv")))
  MaxDateForecasted <- data[, max(InquiryDate, na.rm = TRUE)]

  # Loop through all periods to forecast----
  FC_Period <- 0L
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

    # ?
    InquiryForecast <- InquiryForecast[2L:.N]

    # Merge on next date of Inquiries----
    print("# Merge on next date of Inquiries----")
    maxct <- merge(maxct, data[, list(Inquiry = data.table::first(Inquiry)), by = list(InquiryDate)][, InquiryDate := as.Date(InquiryDate)], by = "InquiryDate", all.x = TRUE)
    maxct[, ":=" (Transfer = -1, Rate = 0)]
    maxct[, ScoreRecords := 1]
    maxct <- data.table::rbindlist(list(maxct, NextInquiryDay), use.names = TRUE)

    # Remove CohortDaysOut beyond 120 days
    maxct <- maxct[CohortDaysOut <= 120]

    # Stack onto modeling data for seg----
    print("# Stack onto modeling data for seg----")
    data <- data.table::rbindlist(list(data, maxct), use.names = TRUE)
    rm(maxct)

    # Feature Engineering----
    print("# Feature Engineering----")
    temp <- data.table::copy(data)
    temp <- RemixAutoML::CreateCalendarVariables(temp, DateCols = c("InquiryDate", "TransferDate"), AsFactor = FALSE, TimeUnits = c( "week", "isoweek", "month", "quarter", "year"))
    temp <- RemixAutoML::CreateHolidayVariables(temp, DateCols = c("InquiryDate"), HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"), Holidays = NULL, GroupingVars = "MARS_Segment", Print = FALSE)
    data.table::setnames(temp, old = "HolidayCounts", new = "InquiryDateHolidayCounts")
    temp <- RemixAutoML::CreateHolidayVariables(temp, DateCols = c("TransferDate"), HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"), Holidays = NULL, GroupingVars = "MARS_Segment", Print = FALSE)
    data.table::setnames(temp, old = "HolidayCounts", new = "TransferDateHolidayCounts")
    data.table::setorderv(temp, cols = c("InquiryDate","CohortDaysOut"), c(1L, 1L))

    # AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    data.table::set(temp, j = "InquiryDate", value = as.character(temp[["InquiryDate"]]))
    Lagss                 = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L,10L,12L))
    MA_RollWindowss       = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L,10L,12L), "month" = c(1L,2L,3L,4L))
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

    # AutoLagRollStatsScoring----
    print("# AutoLagRollStatsScoring----")
    temp1 <- data.table::copy(data)
    data.table::set(temp1, j = "InquiryDate", value = as.Date(temp1[["InquiryDate"]]))
    Lagss                 = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L,10L,12L,25L,26L))
    MA_RollWindowss       = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L,10L,12L,20L,24L), "month" = c(6L,12L))
    SD_RollWindowss       = NULL
    Skew_RollWindowss     = NULL
    Kurt_RollWindowss     = NULL
    Quantile_RollWindowss = NULL
    Quantiles_Selecteds   = c("q5")
    print(str(temp1))
    temp1 <- RemixAutoML::AutoLagRollStatsScoring(

      # Data
      data                 = temp1,
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

    # Combine datasets----
    print("# Combine datasets----")
    temp[, ":=" (InquiryDate = as.Date(InquiryDate))]
    temp <- merge(temp, temp1[, .SD, .SDcols = c("InquiryDate","TransferDate", setdiff(names(temp1),names(temp)))], by = c("InquiryDate","TransferDate"))
    rm(temp1)

    # Model data prep----
    print("# Model data prep----")
    temp <- RemixAutoML::ModelDataPrep(
      data         = temp,
      Impute       = TRUE,
      CharToFactor = FALSE,
      FactorToChar = FALSE,
      IntToNumeric = FALSE,
      DateToChar   = FALSE,
      RemoveDates  = FALSE,
      MissFactor   = "0",
      MissNum      = -1,
      IgnoreCols   = NULL)

    # Convert CohortDaysOut to factor----
    print("# Convert features types to correct ones----")
    if(!all(class(data$CohortDaysOut) %chin% "numeric")) data[, CohortDaysOut := as.numeric(as.character(CohortDaysOut))]
    if(!all(class(data$InquiryDate) %chin% "Date")) data[, InquiryDate := as.Date(InquiryDate)]
    if(!all(class(data$TransferDate) %chin% "Date")) data[, TransferDate := as.Date(TransferDate)]

    # Load model artifacts----
    print("# Load model artifacts----")
    if(FC_Period == 1L) load(file = file.path(normalizePath(Modeling), paste0(seg, "_FinalTrain.Rdata"))) #, envir = new.env())

    # Score Model----
    print("# Score Model----")
    temp1 <- data.table::copy(temp)
    Features <- TestModel$ColNames[[1]]
    temp1 <- RemixAutoML::AutoCatBoostScoring(
      TargetType = "regression",
      ScoringData = temp1,
      FeatureColumnNames = Features,
      IDcols = names(temp1)[!names(temp1) %in% Features],
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
    temp[, Rate := temp1$Predictions]
    temp[, Transfer := Rate * (Inquiry + 1)]
    temp <- temp[, .SD, .SDcols = c("InquiryDate","TransferDate","CohortDaysOut","MARS_Segment","Inquiry","Transfer","Rate")]
    data <- data.table::rbindlist(list(data[ScoreRecords != 1], temp), fill = TRUE, use.names = TRUE)
    data[, ScoreRecords := NULL]

    # Save forecasts to file----
    data.table::fwrite(data, file = file.path(normalizePath(ForecastOutput), paste0(seg, "_Forecasts.csv")))

    # Update MaxDateForecasted to know when to stop----
    MaxDateForecasted <- data[, max(InquiryDate)]
  }
  rm(data, TestModel)
}

