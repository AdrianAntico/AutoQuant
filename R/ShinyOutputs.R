#' @title ExternalDataServer
#'
#' @description Create output objects for csv external data and .Rdata external data
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param id = 'ExternalData'
#' @param StorageAccount = NULL
#' @param Container = NULL
#' @param BlobStorageURL = NULL
#' @param Key = NULL
#' @param Debug = FALSE
#'
#' @noRd
ExternalDataServer <- function(id = 'ExternalData',
                               StorageAccount = NULL,
                               Container = NULL,
                               BlobStorageURL = NULL,
                               Key = NULL,
                               Debug = FALSE) {

  # Module
  shiny::moduleServer(
    id,

    # Anonomous function
    function(input, output, session) {

      # .csv from Local
      output$DataLoad <- shiny::renderUI({
        shiny::fileInput(
          inputId = 'DataLoad',
          label = NULL,
          accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
      })

      # .Rdata from local
      output$ModelObjectLoad <- shiny::renderUI({
        shiny::fileInput(inputId = "ModelObjectLoad", label = NULL)
      })

      # .csv from Azure Blob Storage
      output$blob <- shiny::renderUI({
        if(Debug) paste0('https://', StorageAccount, '.blob.core.windows.net/', Container)
        BlobStorageURL <- paste0('https://', StorageAccount, '.blob.core.windows.net/', Container)
        assign(x = 'BlobStorageURL', value = BlobStorageURL, envir = .GlobalEnv)
        cont <<- AzureStor::blob_container(BlobStorageURL, key = Key)
        rawfiles <- AzureStor::list_storage_files(cont, info = 'name')
        rawfiles <<- rawfiles[c(which(grepl(pattern = '.csv', x = rawfiles)), which(grepl(pattern = '.Rdata', x = rawfiles)))]
        RemixAutoML::SelectizeInput(
          InputID = 'blob',
          Label = 'Select Azure .csv File',
          Choices = rawfiles[which(grepl(pattern = '.csv', x = rawfiles))],
          SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = Debug)
      })

      # .Rdata from Azure Blob Storage
      output$rdatablob <- shiny::renderUI({
        RemixAutoML::SelectizeInput(
          InputID = 'rdatablob',
          Label = 'Select Azure .Rdata File',
          Choices = rawfiles[which(grepl(pattern = '.Rdata', x = rawfiles))],
          SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1, CloseAfterSelect = TRUE, Debug = Debug)
      })

      shiny::observeEvent(eventExpr = input$LoadAzure, {

        # csv
        FileName <- tryCatch({input[['blob']]}, error = function(x) NULL)
        if(Debug) print(FileName)
        if(length(FileName) != 0 && FileName != "Load" && FileName != "") {
          data
          AzureStor::download_blob(container = cont, src = input[['blob']], dest = file.path('/inputdata', input[['blob']]), overwrite=TRUE)
        }

        # .Rdata
        inFile2 <- tryCatch({input[['rdatablob']]}, error = function(x) NULL)
        if(!is.null(inFile2)) print(inFile2)
        if(length(inFile2) != 0 && inFile2 != "") {
          if(Debug) {print('data check 3')}
          AzureStor::download_blob(container = cont, src = input[['rdatablob']], dest = file.path('/inputdata', input[['rdatablob']]), overwrite=TRUE)
        }
      })
    }
  )
}
