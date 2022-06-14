#' @title Shiny.Utils.CachePath
#'
#' @description Combines a directory path with a file name and extension
#'
#' @param CacheDir Path to folder
#' @param CacheName Name of file
#'
#' @keywords internal
Shiny.Utils.CachePath <- function(CacheName, CacheDir, Ext = '.csv') {
  file.path(CacheDir, paste0(gsub(Ext,'',CacheName),Ext))
}

#' @title Shiny.DW.DeleteColumns
#'
#' @description server.R observeEvent() for concatenating columns in a data.table
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList CodeList from app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.DW.DeleteColumns <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

    # Pull in values
    if(Debug) print('Shiny.DW.DeleteColumns')
    temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input$DeleteVariables_SelectData}, error = function(x) NULL), VarName = 'DeleteVariables_SelectData', Type = 'character', Default = NULL, Debug = Debug)
    Cols <- RemixAutoML:::ReturnParam(xx = tryCatch({input$DeleteVariables}, error = function(x) NULL), VarName = 'DeleteVariables', Type = 'character', Default = NULL, Debug = Debug)

    # Dispatch
    if(Debug) {print(temp);print(Cols)}
    if(length(Cols) > 0 && length(temp) > 0L) {

      # Caching: non-function version
      if(length(CacheDir) == 0L) {
        if(Debug) print('Shiny.DW.DeleteColumns 1a')
        x <- DataList[[temp]]
        path <- NULL
      } else {
        if(Debug) print('Shiny.DW.DeleteColumns 1b')
        path <- RemixAutoML:::Shiny.Utils.CachePath(CacheName, CacheDir, Ext = '.csv')
        if(Debug) {print(path);print(length(path))}
        x <- RemixAutoML:::ReactiveLoadCSV(Infile = path, Debug = Debug)
      }

      # Run code
      if(Debug) print('Shiny.DW.DeleteColumns 2')
      data.table::set(x, j = c(eval(Cols)), value = NULL)
      DataList[[temp]] <- x

      # Caching: non-function version
      if(Debug) print('Shiny.DW.DeleteColumns 3')
      if(length(path) > 0L) data.table::fwrite(x, file = path)

      # Display
      if(Debug) print('Shiny.DW.DeleteColumns 4')
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(DataList[[temp]][seq_len(min(.N, 1000L))])
      })

      # Code
      if(Debug) print('Shiny.DW.DeleteColumns 5')
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Delete Columns\n",
        "temp <- ", RemixAutoML:::CEP(temp), "\n",
        "Cols <- c(", RemixAutoML:::ExpandText(Cols), ")\n",
        "data.table::set(DataList[[temp]], j = c(Cols), value = NULL)\n"))

      # Return
      if(Debug) {
        print('Shiny.DW.DeleteColumns 6')
        print(names(DataList))
        print(CodeList)
      }

      return(list(
        DataList = DataList,
        CodeList = CodeList
      ))
    } else {
      print(' :( FAILED ): ')
    }
}

#' @title Shiny.DW.ConcatenateColumns
#'
#' @description server.R observeEvent() for concatenating columns in a data.table
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.DW.ConcatenateColumns <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Concatenate columns has begun..', value = 0, {

    if(Debug) print('Shiny.DW.ConcatenateColumns')

    # Pull in values
    Cols <- RemixAutoML:::ReturnParam(xx = tryCatch({input$ConcatColumns}, error = function(x) NULL), VarName = 'ConcatColumns', Type = 'character', Default = NULL, Debug = Debug)
    temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input$ConcatColumns_SelectData}, error = function(x) NULL), VarName = 'ConcatColumns_SelectData', Type = 'character', Default = NULL, Debug = Debug)

    # Dispatch
    if(length(Cols) > 0L && length(temp) > 0L) {

      # Caching: non-function version
      if(Debug) print('Shiny.DW.ConcatenateColumns 1')
      if(length(CacheDir) == 0L) {
        x <- DataList[[temp]]
        path <- NULL
      } else {
        path <- RemixAutoML:::Shiny.Utils.CachePath(CacheName, CacheDir, Ext = '.csv')
        if(Debug) {print(path);print(length(path))}
        x <- RemixAutoML:::ReactiveLoadCSV(Infile = path, Debug = Debug)
      }

      # Run code
      if(Debug) print('Shiny.DW.ConcatenateColumns 2')
      x <- DataList[[temp]]
      x[, paste0(Cols, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(Cols)]
      if(length(CacheDir) > 0L) data.table::fwrite(x, file = path)

      # Caching: non-function version
      if(Debug) print('Shiny.DW.ConcatenateColumns 3')
      DataList[[temp]] <- x

      # Display
      if(Debug) print('Shiny.DW.ConcatenateColumns 4')
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(DataList[[temp]][seq_len(min(.N, 1000L))])
      })

      # Code
      if(Debug) print('Shiny.DW.ConcatenateColumns 5')
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Concatenate Columns\n",
        "temp <- ", RemixAutoML:::CEP(temp),"\n",
        "Cols <- c(", RemixAutoML:::ExpandText(Cols), ")\n",
        "DataList[[temp]][, paste0(Cols, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(Cols)]\n"))

      # Return
      if(Debug) {
        print('Shiny.DW.ConcatenateColumns 6')
        print(names(DataList))
        print(CodeList)
      }

      return(list(
        DataList = DataList,
        CodeList = CodeList
      ))
    } else {
      print(' :( FAILED ): ')
    }
  })
}



# rx <- callr::r_bg(function() {Sys.sleep(12); 1})
# rx$wait()
# rx$is_alive()
# rx$get_result()

# #' @title Shiny.DW.ConcatenateColumns
# #'
# #' @description server.R observeEvent() for concatenating columns in a data.table
# #'
# #' @param input shiny input
# #' @param output shiny output
# #' @param session shiny session
# #' @param DataList DataList contains the data sets in session
# #' @param CodeList From app
# #' @param CacheDir From app
# #' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
# #' @param Debug Debug from app
# #'
# #' @keywords internal
# Shiny.DW.ConcatenateColumns <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
#   shiny::withProgress(message = 'Concatenate columns has begun..', value = 0, {
#
#     print('Shiny.DW.ConcatenateColumns')
#
#     # Dispatch
#     Cols <- RemixAutoML:::ReturnParam(xx = tryCatch({input$ConcatColumns}, error = function(x) NULL), VarName = 'ConcatColumns', Type = 'character', Default = NULL, Debug = Debug)
#     if(length(Cols) != 0) {
#
#       # Pull in values
#       temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input$ConcatColumns_SelectData}, error = function(x) NULL), VarName = 'ConcatColumns_SelectData', Type = 'character', Default = NULL, Debug = Debug)
#
#       # Caching: non-function version
#       if(length(CacheDir) == 0L) {
#         x <- DataList[[temp]]
#         path <- NULL
#       } else {
#         path <- file.path(CacheDir, paste0(gsub('.csv', '', CacheName), '.csv'))
#         x <- RemixAutoML:::ReactiveLoadCSV(Infile = path, Debug = Debug)
#       }
#
#       # Run code
#       rx <- callr::r_bg(function(x,DataList,Cols,path) {
#         x <- DataList[[temp]]
#         x[, paste0(Cols, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(Cols)]
#         if(length(path) > 0L) data.table::fwrite(x, file = path)
#         x
#       })
#
#       # Caching: non-function version
#       rx$wait()
#       rx$is_alive()
#
#       DataList[[temp]] <- x
#
#       # Display
#       output$FE_DisplayData <- DT::renderDataTable({
#         RemixAutoML::DataTable(x[seq_len(min(.N, 1000))])
#       })
#
#       # Code
#       CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
#         "\n",
#         "# Concatenate Columns\n",
#         "temp <- ", RemixAutoML:::CEP(temp),"\n",
#         "x <- DataList[[temp]]\n",
#         "Cols <- c(", RemixAutoML:::ExpandText(Cols), ")\n",
#         "x[, paste0(Cols, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(Cols)]\n",
#         "DataList[[temp]] <- x\n"))
#
#       # Return
#       return(list(
#         DataList = DataList,
#         CodeList = CodeList))
#     }
#   })
# }
