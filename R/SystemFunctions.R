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
