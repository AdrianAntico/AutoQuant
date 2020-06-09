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






####################################################
# Transfer Forecast Model Building----
####################################################

# Select services to run----
library(data.table)
Jobs <- c("Evaluate","Train")
metric <- "MSE" # choose from MAE, MSE, MAPE

# Save segments values----
ProjectRoot <- "C:/Users/aantico/Desktop/Forecasting"
ModelData <- "C:/Users/aantico/Desktop/Forecasting/ModelData"
Modeling <- "C:/Users/aantico/Desktop/Forecasting/Modeling"
load(file.path(normalizePath(ModelData), "MARS_Segments.Rdata"))

# Investigate model performance----
# load(file.path(normalizePath(Modeling), "Agg_Evaluation.Rdata"))
# Investigate features
# TestModel$TransformationResults
# View(TestModel$VariableImportance)
# TestModel$VI_Plot
# TestModel$PartialDependencePlots$months_InquiryDateMean_12_months_InquiryDate_LAG_1_Transfer

# Save data by segment----
# seg <- MARS_Segments[1L]
# proc <- "Evaluate"

# Run procedure----
for(seg in MARS_Segments) {

  # Setup timers----
  TimerDataEval <- data.table::data.table(Process = rep("a", 25L), Time = rep(999, 25L))
  TimerDataTrain <- data.table::data.table(Process = rep("a", 25L), Time = rep(999, 25L))
  TimerDataFC <- data.table::data.table(Process = rep("a", 25L), Time = rep(999, 25L))

  # Build models----
  for(proc in Jobs) {

    # Import segment data for training----
    if(proc %in% c("Evaluate","Train")) {
      x <- system.time(gcFirst = FALSE, data <- data.table::fread(file.path(normalizePath(ModelData), paste0(seg, "_BaseData.csv"))))
      if(proc == "Evaluate") {
        data.table::set(TimerDataEval, i = 1L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataEval, i = 1L, j = "Process", value = "# Import segment data for training----")
      } else if(proc == "Train") {
        data.table::set(TimerDataTrain, i = 1L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataTrain, i = 1L, j = "Process", value = "# Import segment data for training----")
      }
    }

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    } else if(proc == "Train") {
      data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
    }

    # Add InquiryDate and TransferDate calendar variables----
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::CreateCalendarVariables(data, DateCols = c("InquiryDate", "TransferDate"), AsFactor = FALSE, TimeUnits = c( "week", "isoweek", "month", "quarter", "year")))
    if(proc == "Evaluate") {
      data.table::set(TimerDataEval, i = 2L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 2L, j = "Process", value = "# Add InquiryDate and TransferDate calendar variables----")
    } else if(proc == "Train") {
      data.table::set(TimerDataTrain, i = 2L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 2L, j = "Process", value = "# Add InquiryDate and TransferDate calendar variables----")
    }

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    } else if(proc == "Train") {
      data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
    }

    # Add InquiryDate holiday variables----
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::CreateHolidayVariables(data, DateCols = c("InquiryDate"), HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"), Holidays = NULL, GroupingVars = "MARS_Segment", Print = FALSE))
    data.table::setnames(data, old = "HolidayCounts", new = "InquiryDateHolidayCounts")
    if(proc == "Evaluate") {
      data.table::set(TimerDataEval, i = 3L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 3L, j = "Process", value = "# Add InquiryDate holiday variables----")
    } else if(proc == "Train") {
      data.table::set(TimerDataTrain, i = 3L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 3L, j = "Process", value = "# Add InquiryDate holiday variables----")
    }

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    } else if(proc == "Train") {
      data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
    }

    # Add TransferDate holiday variables----
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::CreateHolidayVariables(data, DateCols = c("TransferDate"), HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"), Holidays = NULL, GroupingVars = "MARS_Segment", Print = FALSE))
    data.table::setnames(data, old = "HolidayCounts", new = "TransferDateHolidayCounts")
    if(proc == "Evaluate") {
      data.table::set(TimerDataEval, i = 4L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 4L, j = "Process", value = "# Add TransferDate holiday variables----")
    } else if(proc == "Train") {
      data.table::set(TimerDataTrain, i = 4L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 4L, j = "Process", value = "# Add TransferDate holiday variables----")
    }

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    } else if(proc == "Train") {
      data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
    }

    # Sort data by InquiryDate and then by CohortDaysOut----
    x <- system.time(gcFirst = FALSE, data.table::setorderv(data, cols = c("InquiryDate","CohortDaysOut"), c(1L, 1L)))
    if(proc == "Evaluate") {
      data.table::set(TimerDataEval, i = 5L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 5L, j = "Process", value = "# Sort data by InquiryDate and then by CohortDaysOut----")
    } else if(proc == "Train") {
      data.table::set(TimerDataTrain, i = 5L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 5L, j = "Process", value = "# Sort data by InquiryDate and then by CohortDaysOut----")
    }

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    } else if(proc == "Train") {
      data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
    }

    # Convert InquiryDate to Character to treat as Cohort Group----
    x <- system.time(gcFirst = FALSE, data.table::set(data, j = "InquiryDate", value = as.character(data[["InquiryDate"]])))
    if(proc == "Evaluate") {
      data.table::set(TimerDataEval, i = 6L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 6L, j = "Process", value = "# Convert InquiryDate to Character to treat as Cohort Group----")
    } else if(proc == "Train") {
      data.table::set(TimerDataTrain, i = 6L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 6L, j = "Process", value = "# Convert InquiryDate to Character to treat as Cohort Group----")
    }

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    } else if(proc == "Train") {
      data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
    }

    #----

    #----

    # Rolling stats for TransferDate with InquiryDate as a Grouping Variable----
    Lagss                 = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L,10L,12L,25L,26L))
    MA_RollWindowss       = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L,10L,12L,20L,24L), "month" = c(6L,12L))
    SD_RollWindowss       = NULL
    Skew_RollWindowss     = NULL
    Kurt_RollWindowss     = NULL
    Quantile_RollWindowss = NULL
    Quantiles_Selecteds   = c("q5")
    if(proc %in% c("Evaluate","Train")) {
      x <- system.time(gcFirst = FALSE, data <- RemixAutoML::AutoLagRollStats(

        # Data
        data                 = data,
        DateColumn           = "TransferDate",
        Targets              = c("Transfer","Rate"),
        HierarchyGroups      = NULL,
        IndependentGroups    = "InquiryDate",
        TimeUnit             = c("days"),
        TimeGroups           = c("days","weeks","month"),
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
        Debug                = TRUE))
      if(proc == "Evaluate") {
        data.table::set(TimerDataEval, i = 7L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataEval, i = 7L, j = "Process", value = "# Rolling stats for TransferDate with InquiryDate as a Grouping Variable----")
      } else if(proc == "Train") {
        data.table::set(TimerDataTrain, i = 7L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataTrain, i = 7L, j = "Process", value = "# Rolling stats for TransferDate with InquiryDate as a Grouping Variable----")
      }

      # Save Timers to file----
      if(proc == "Evaluate") {
        data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
      } else if(proc == "Train") {
        data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
      }
    }


    #----

    #----

    # Change InquiryDate to Date----
    x <- system.time(gcFirst = FALSE, data.table::set(data, j = "InquiryDate", value = as.Date(data[["InquiryDate"]])))
    if(proc == "Evaluate") {
      data.table::set(TimerDataEval, i = 8L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 8L, j = "Process", value = "# Convert InquiryDate back to Date----")
    } else if(proc == "Train") {
      data.table::set(TimerDataTrain, i = 8L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 8L, j = "Process", value = "# Convert InquiryDate back to Date----")
    }

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    } else if(proc == "Train") {
      data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
    }

    #----

    #----

    # Rolling stats for Inquiries----
    Lagss                 = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L,10L,12L,25L,26L))
    MA_RollWindowss       = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L,10L,12L,20L,24L), "month" = c(6L,12L))
    SD_RollWindowss       = NULL
    Skew_RollWindowss     = NULL
    Kurt_RollWindowss     = NULL
    Quantile_RollWindowss = NULL
    Quantiles_Selecteds   = c("q5")
    if(proc %in% c("Evaluate","Train")) {
      x <- system.time(gcFirst = FALSE, data <- RemixAutoML::AutoLagRollStats(

        # Data
        data                 = data,
        DateColumn           = "InquiryDate",
        Targets              = c("Inquiry"),
        HierarchyGroups      = NULL,
        IndependentGroups    = NULL,
        TimeGroups           = c("day","week","month"),
        TimeUnitAgg          = c("day"),

        # Services
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
        Debug                = TRUE))
      if(proc == "Evaluate") {
        data.table::set(TimerDataEval, i = 9L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataEval, i = 9L, j = "Process", value = "# Rolling stats for Inquiries----")
      } else if(proc == "Train") {
        data.table::set(TimerDataTrain, i = 9L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataTrain, i = 9L, j = "Process", value = "# Rolling stats for Inquiries----")
      }
    }


    #----

    #----

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    } else if(proc == "Train") {
      data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
    }

    # Model data prep----
    x <- system.time(gcFirst = FALSE, data <- RemixAutoML::ModelDataPrep(
      data         = data,
      Impute       = TRUE,
      CharToFactor = FALSE,
      FactorToChar = FALSE,
      IntToNumeric = FALSE,
      DateToChar   = FALSE,
      RemoveDates  = FALSE,
      MissFactor   = "0",
      MissNum      = -1,
      IgnoreCols   = NULL))
    if(proc == "Evaluate") {
      data.table::set(TimerDataEval, i = 10L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 10L, j = "Process", value = "# Model data prep----")
    } else if(proc == "Train") {
      data.table::set(TimerDataTrain, i = 10L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 10L, j = "Process", value = "# Model data prep----")
    }

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    } else if(proc == "Train") {
      data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
    }

    # Convert InquiryDate to Character to treat as Cohort Group----
    x <- system.time(gcFirst = FALSE, data.table::set(data, j = "InquiryDate", value = as.character(data[["InquiryDate"]])))
    if(proc == "Evaluate") {
      data.table::set(TimerDataEval, i = 11L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 11L, j = "Process", value = "# Convert InquiryDate to Character to treat as Cohort Group----")
    } else if(proc == "Train") {
      data.table::set(TimerDataTrain, i = 11L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 11L, j = "Process", value = "# Convert InquiryDate to Character to treat as Cohort Group----")
    }

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    } else if(proc == "Train") {
      data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
    }

    # Save mars specific segment data to file----
    x <- system.time(gcFirst = FALSE, data.table::fwrite(data, file = file.path(ModelData, paste0(seg, "_ModelDataReady.csv"))))
    if(proc == "Evaluate") {
      data.table::set(TimerDataEval, i = 12L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 12L, j = "Process", value = "# Save mars specific segment data to file----")
    } else if(proc == "Train") {
      data.table::set(TimerDataTrain, i = 12L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 12L, j = "Process", value = "# Save mars specific segment data to file----")
    }

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    } else if(proc == "Train") {
      data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
    }

    # Load data if updating models without new data----
    if(!exists("data")) data <- data.table::fread(file = file.path(ModelData, paste0(seg, "_ModelDataReady.csv")))
    if(proc == "Evaluate") {
      data.table::set(TimerDataEval, i = 13L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 13L, j = "Process", value = "# Load data if updating models without new data----")
    } else if(proc == "Train") {
      data.table::set(TimerDataTrain, i = 13L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataTrain, i = 13L, j = "Process", value = "# Load data if updating models without new data----")
    }

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    } else if(proc == "Train") {
      data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
    }

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    } else if(proc == "Train") {
      data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Train_Timer.csv")))
    }

    # Check for whether or not features are the right type before partitioning so you don't have to do it 3 times later----
    if(!all(class(data$CohortDaysOut) %chin% "numeric")) data[, CohortDaysOut := as.numeric(as.character(CohortDaysOut))]
    if(!all(class(data$InquiryDate) %chin% "Date")) data[, InquiryDate := as.Date(InquiryDate)]
    if(!all(class(data$TransferDate) %chin% "Date")) data[, TransferDate := as.Date(TransferDate)]

    # Partition Data----
    if(proc %in% c("Evaluate")) {
      data[, Rate := Rate * 10000]
      data[, CohortDaysOut := as.numeric(as.character(CohortDaysOut))]
      x <- system.time(gcFirst = FALSE, DataSets <- RemixAutoML::AutoDataPartition(
        data = data,
        NumDataSets = 3L,
        Ratios = c(0.70,0.20,0.10),
        PartitionType = "time",
        StratifyColumnNames = NULL,
        StratifyNumericTarget = NULL,
        StratTargetPrecision = 1L,
        TimeColumnName = NULL))
      data.table::set(TimerDataEval, i = 15L, j = "Time", value = x[[3L]])
      data.table::set(TimerDataEval, i = 15L, j = "Process", value = "# Convert CohortDaysOut to Factor----")
      TrainData <- DataSets$TrainData
      ValidationData <- DataSets$ValidationData
      TestData <- DataSets$TestData
      rm(DataSets, data)
    }

    # Save Timers to file----
    if(proc == "Evaluate") {
      data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(ModelData), paste0(seg, "_Eval_Timer.csv")))
    }


    #----

    #----

    # Build model using CatBoostRegression()----
    if(proc %in% c("Evaluate","Train")) {

      # Define features----
      Features <- names(TrainData)[c(3L, 5L, 8L:ncol(TrainData))]

      # Build model
      x <- system.time(gcFirst = FALSE, TestModel <- RemixAutoML::AutoCatBoostRegression(

        # GPU or CPU and the number of available GPUs
        task_type = "CPU",
        NumGPUs = 1,

        # Metadata arguments:
        #   'ModelID' is used to create part of the file names generated when saving to file'
        #   'model_path' is where the minimal model objects for scoring will be stored
        #      'ModelID' will be the name of the saved model object
        #   'metadata_path' is where model evaluation and model interpretation files are saved
        #      objects saved to model_path if metadata_path is null
        #      Saved objects include:
        #         'ModelID_ValidationData.csv' is the supplied or generated TestData with predicted values
        #         'ModelID_VariableImportance.csv' is the variable importance.
        #            This won't be saved to file if GrowPolicy is either "Depthwise" or "Lossguide" was used
        #         'ModelID_ExperimentGrid.csv' if GridTune = TRUE.
        #            Results of all model builds including parameter settings, bandit probs, and grid IDs
        #         'ModelID_EvaluationMetrics.csv' which contains MSE, MAE, MAPE, R2
        ModelID = paste0(seg,"_", proc, "_"),
        model_path = Modeling,
        metadata_path = NULL,
        SaveModelObjects = FALSE,
        ReturnModelObjects = TRUE,

        # Data arguments:
        #   'TrainOnFull' is to train a model with 100 percent of your data.
        #     That means no holdout data will be used for evaluation
        #   If ValidationData and TestData are NULL and TrainOnFull is FALSE then data will be split 70 20 10
        #   'PrimaryDateColumn' is a date column in data that is meaningful when sorted.
        #     CatBoost categorical treatment is enhanced when supplied
        #   'IDcols' are columns in your data that you don't use for modeling but get returned with ValidationData
        #   'TransformNumericColumns' is for transforming your target variable. Just supply the name of it
        data = if(proc == "Evaluate") TrainData else data,
        TrainOnFull = if(proc == "Evaluate") FALSE else TRUE,
        ValidationData = if(proc == "Evaluate") ValidationData else NULL,
        TestData = if(proc == "Evaluate") TestData else NULL,
        TargetColumnName = "Rate",
        FeatureColNames = Features,
        PrimaryDateColumn = "TransferDate",
        IDcols = names(TrainData)[!names(TrainData) %in% Features],
        TransformNumericColumns = "Rate",
        Methods = c("LogPlus1","YeoJohnson"),

        # Model evaluation:
        #   'eval_metric' is the measure catboost uses when evaluting on holdout data during its bandit style process
        #   'loss_function' the loss function used in training optimization
        #   'NumOfParDepPlots' Number of partial dependence calibration plots generated.
        #     A value of 3 will return plots for the top 3 variables based on variable importance
        #     Won't be returned if GrowPolicy is either "Depthwise" or "Lossguide" is used
        #     Can run the RemixAutoML::ParDepCalPlots() with the outputted ValidationData
        eval_metric = "RMSE",
        loss_function = "RMSE",
        MetricPeriods = 50L,
        NumOfParDepPlots = 12L,

        # Grid tuning arguments:
        #   'PassInGrid' is for retraining using a previous grid winning args
        #   'MaxModelsInGrid' is a cap on the number of models that will run
        #   'MaxRunsWithoutNewWinner' number of runs without a new winner before exiting grid tuning
        #   'MaxRunMinutes' is a cap on the number of minutes that will run
        #   'Shuffles' is the number of times you want the random grid arguments shuffled
        #   'BaselineComparison' default means to compare each model build with a default built of catboost using max(Trees)
        #   'MetricPeriods' is the number of trees built before evaluting holdoutdata internally. Used in finding actual Trees used.
        PassInGrid = NULL,
        GridTune = FALSE,
        MaxModelsInGrid = 100L,
        MaxRunsWithoutNewWinner = 100L,
        MaxRunMinutes = 60L * 60L,
        Shuffles = 4L,
        BaselineComparison = "default",

        # Trees, Depth, and LearningRate used in the bandit grid tuning
        # Must set Trees to a single value if you are not grid tuning
        # The ones below can be set to NULL and the values in the example will be used
        # GrowPolicy is turned off for CPU runs
        # BootStrapType utilizes Poisson only for GPU and MVS only for CPU
        Trees = 3000L, #seq(100L, 2000L, 100L),
        Depth = seq(4L, 8L, 1L),
        LearningRate = seq(0.01,0.10,0.01),
        L2_Leaf_Reg = seq(1.0, 10.0, 1.0),
        RSM = c(0.80, 0.85, 0.90, 0.95, 1.0),
        BootStrapType = c("Bayesian", "Bernoulli", "Poisson", "MVS", "No"),
        GrowPolicy = c("SymmetricTree", "Depthwise", "Lossguide")))

      # Store results----
      if(proc == "Evaluate") {
        data.table::set(TimerDataEval, i = 16L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataEval, i = 16L, j = "Process", value = "# Build model using CatBoostRegression()----")
      } else if(proc == "Train") {
        data.table::set(TimerDataTrain, i = 16L, j = "Time", value = x[[3L]])
        data.table::set(TimerDataTrain, i = 16L, j = "Process", value = "# Build model using CatBoostRegression()----")
      }

      # Save timer file----
      if(proc == "Evaluate") {
        data.table::fwrite(TimerDataEval[Process != "a"], file = file.path(normalizePath(Modeling), paste0(seg, "_Eval_Timer.csv")))
      } else if(proc == "Train") {
        data.table::fwrite(TimerDataTrain[Process != "a"], file = file.path(normalizePath(Modeling), paste0(seg, "_Train_Timer.csv")))
      }

      # Save model objects----
      if(proc == "Evaluate") {
        save(TestModel, file = file.path(normalizePath(Modeling), paste0(seg, "_Evaluation.Rdata")))
      } else if(proc == "Train") {
        save(TestModel, file = file.path(normalizePath(Modeling), paste0(seg, "_FinalTrain.Rdata")))
      }

      # Remove objects before next run----
      if(proc %in% c("Evaluate")) {
        rm(TestModel)
      } else {
        rm(TestModel, data)
      }

      # Garbage collection----
      gc()
    }

    #----

    #----

  }
}

# Clear environment----
rm(list = ls())
