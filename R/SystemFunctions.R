# AutoQuant is a package for quickly creating high quality visualizations under a common and easy api.
# Copyright (C) <year>  <name of author>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' CreateProjectFolders Converts path files to proper path files
#'
#' CreateProjectFolders Converts path files to proper path files
#'
#' @author Adrian Antico
#' @family System Functions
#' @param ProjectName This is the name of a project which will be the name of the file created in the root folder
#' @param RootPath This is the path file to the root folder
#' @param ExistsButNoProjectList Set to TRUE if the folder exists but not the ProjectList file
#' @param Local Local or cloud
#' @return Returns a proper path file string
#' @noRd
CreateProjectFolders <- function(ProjectName = input$ID_NewProjectName,
                                 RootPath = input$ID_Root_Folder,
                                 ExistsButNoProjectList = FALSE,
                                 Local = FALSE) {

  # First pass validity check
  RootPath <- gsub("\\\\", "/", RootPath)

  # Check for validity in path
  RootPath <- tryCatch({normalizePath(RootPath, mustWork = TRUE)}, error = function(x) NULL)

  # Proceed or stop
  if(!is.null(RootPath)) {

    # Create Collection List ----
    ProjectList <- list()

    # Store Project Name ----
    ProjectList[["ProjectName"]] <- ProjectName

    # Define Project Folder Root Path ----
    if(Local) {
      ProjectPath <- file.path(normalizePath(RootPath), ProjectName)
    } else {
      ProjectPath <- paste0("./",ProjectName)
    }

    # Store value
    ProjectList[["ProjectFolderPath"]] <- ProjectPath

    # Create Project Folder ----
    if(!ExistsButNoProjectList) {

      # Create Data folder ----
      DataPath <- normalizePath(file.path(ProjectPath, "Data"), mustWork = FALSE)
      dir.create(path =  DataPath, showWarnings = TRUE, recursive = TRUE)
      ProjectList[["DataFolderPath"]] <- DataPath

      # Create Models folder ----
      ModelsPath <- normalizePath(file.path(ProjectPath, "Models"), mustWork = FALSE)
      dir.create(path =  ModelsPath, showWarnings = TRUE, recursive = TRUE)
      ProjectList[["ModelsFolderPath"]] <- ModelsPath

      # Create MetaData folder ----
      MetaDataPath <- normalizePath(file.path(ProjectPath, "MetaData"), mustWork = FALSE)
      dir.create(path =  MetaDataPath, showWarnings = TRUE, recursive = TRUE)
      ProjectList[["MetaDataPath"]] <- MetaDataPath

      # Save ProjectList to File ----
      save(ProjectList, file = file.path(MetaDataPath, "ProjectList.Rdata"))

    } else {

      # Create Data folder ----
      DataPath <- normalizePath(file.path(ProjectPath,"Data"), mustWork = FALSE)
      ProjectList[["DataFolderPath"]] <- DataPath

      # Create Models folder ----
      ModelsPath <- normalizePath(file.path(ProjectPath,"Models"), mustWork = FALSE)
      ProjectList[["ModelsFolderPath"]] <- ModelsPath

      # Create MetaData folder ----
      MetaDataPath <- normalizePath(file.path(ProjectPath,"MetaData"), mustWork = FALSE)
      ProjectList[["MetaDataPath"]] <- MetaDataPath

      # Save ProjectList to File ----
      save(ProjectList, file = file.path(MetaDataPath,paste0(ProjectName,"_ProjectList.Rdata")))
    }

    # Return List to Use in Other Modules----
    return(ProjectList)

  } else {
    return(NULL)
  }
}

#' DownloadCSVFromStorageExplorer
#'
#' DownloadCSVFromStorageExplorer
#'
#' @author Adrian Antico
#' @family Azure
#' @param UploadCSVObjectName Name of the file you uploaded to the Microsoft Azure Storage Explorer
#' @param SaveCSVFilePath Path file to where you want to save your csv in Azure
#' @param SaveCSVName The name you want to give the csv that will be saved
#' @param UploadLocation The location to where the data is saved in the Azure Storage Explorer
#' @param DataStoreName The name of the store in data factory where you uploaded your data
#' @noRd
DownloadCSVFromStorageExplorer <- function(UploadCSVObjectName = 'data.csv',
                                           SaveCSVFilePath = file.path(Root),
                                           SaveCSVName = "RawData.csv",
                                           UploadLocation = 'Analytics Sandbox/Machine Learning',
                                           DataStoreName = NULL) {

  # Check if azuremlsdk is installed
  if(!"azuremlsdk" %chin% installed.packages()) stop("You need to run install.packages('azuremlsdk')")

  # Options
  options(warn = -1)

  # Azure stuff
  ws <- azuremlsdk::load_workspace_from_config()
  ds <- azuremlsdk::get_datastore(ws, DataStoreName)

  # transfer files
  dset <- azuremlsdk::create_tabular_dataset_from_delimited_files(
    path = ds$path(file.path(UploadLocation, UploadCSVObjectName)),
    validate = TRUE,
    include_path = FALSE,
    infer_column_types = TRUE,
    set_column_types = NULL,
    separator = ",",
    header = TRUE,
    partition_format = NULL)

  # Pull in data, convert it to data.table
  data <- dset$to_pandas_dataframe()
  data <- data.table::as.data.table(data)

  # Write data to file
  data.table::fwrite(data, file = file.path(SaveCSVFilePath, SaveCSVName))

  # Turn warnings back on
  options(warn = 0)
}
