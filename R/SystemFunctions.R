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

RemixAutoML::AutoCatBoostChainLadder(
    data = data,
    PartitionRatios = c(0.70,0.20,0.10),
    BaseFunnelMeasure = "Inquiry",
    ConversionMeasure = "Transfer",
    CalendarDate = "InquiryDate",
    CohortDate = "TransferDate",
    TimeUnit = "days",
    CalendarTimeGroups = c("days","weeks","months"),
    CohortTimeGroups = c("days", "weeks"),
    Jobs = c("eval","train"),
    ModelID = MarsSegment,
    ModelPath = Modeling,
    MetaDataPath = ModelData,
    TaskType = "CPU",
    NumGPUs = 1,
    DT_Threads = max(1L, parallel::detectCores() - 2L),
    EvaluationMetric = "RMSE",
    LossFunction = "RMSE",
    CalendarVariables = c("wday","mday","yday","week","month","quarter","year"),
    HolidayGroups = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
    ImputeRollStats = -0.001,
    CohortHolidayLags = c(1L,2L,7L),
    CohortHolidayMovingAverages = c(3L,7L),
    CalendarHolidayLags = c(1L,2L,7L),
    CalendarHolidayMovingAverages = c(3L,7L),
    CalendarLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L,10L,12L,25L,26L)),
    CalendarMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L,10L,12L,20L,24L), "month" = c(6L,12L)),
    CalendarStandardDeviations = NULL,
    CalendarSkews = NULL,
    CalendarKurts = NULL,
    CalendarQuantiles = NULL,
    CalendarQuantilesSelected = "q50",
    CohortLags = list("day" = c(1L,2L,7L,35L,42L), "week" = c(5L,6L)),
    CohortMovingAverages = list("day" = c(7L,14L,35L,42L), "week" = c(5L,6L), "month" = c(1L,2L)),
    CohortStandardDeviations = NULL,
    CohortSkews = NULL,
    CohortKurts = NULL,
    CohortQuantiles = NULL,
    CohortQuantilesSelected = "q50",
    GridTune = FALSE,
    BaselineComparison = "default",
    MaxModelsInGrid = 25L,
    MaxRunMinutes = 180L,
    MaxRunsWithoutNewWinner = 10L,
    Trees = 4000L,
    Depth = seq(4L,8L,1L),
    LearningRate = seq(0.01,0.10,0.01),
    L2_Leaf_Reg = seq(1.0,10.0,1.0),
    RSM = c(0.80,0.85,0.90,0.95,1.0),
    BootStrapType = c("Bayesian","Bernoulli","Poisson","MVS","No"),
    GrowPolicy = c("SymmetricTree","Depthwise","Lossguide"))
