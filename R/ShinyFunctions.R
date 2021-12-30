#' @importFrom rstudioapi isAvailable getSourceEditorContext
GetData <- function(data = NULL, name = NULL) {
  if(!is.null(data)) {
    if(is.character(data)) {
      remix_data <- try({
        dat <- get(x = data, envir = globalenv())
        if(inherits(dat, what = 'data.table')) {
          dat
        } else {
          data.table::as.data.table(dat)
        }
      }, silent = TRUE)
      remix_data_name <- data
      if("try-error" %in% class(remix_data)) {
        warning(paste0("'", data, "' not found"), call. = FALSE)
        remix_data <- NULL
        remix_data_name <- ""
      }
    } else if(inherits(x = data, what = 'data.table')) {
      remix_data <- try({
        if(inherits(data, what = 'data.table')) {
          data
        } else {
          data.table::as.data.table(data)
        }
      }, silent = TRUE)
      if("try-error" %in% class(remix_data)) {
        warning(paste0("'", data, "' not found"), call. = FALSE)
        remix_data <- NULL
        remix_data_name <- ""
      } else {
        if(!is.null(name)) {
          remix_data_name <- as.character(name)
        } else {
          remix_data_name <- deparse(substitute(data))
        }
      }
    } else {
      remix_data <- NULL
      remix_data_name <- ""
    }
  } else {
    if(rstudioapi::isAvailable()) {
      context <- try(rstudioapi::getSourceEditorContext(), silent = TRUE)
      if("try-error" %in% class(context) || is.null(context)) {
        remix_data <- NULL
        remix_data_name <- ""
      } else {
        context_select <- context$selection[[1]]$text
        if(isTRUE(nzchar(context_select))) {
          remix_data <- try(data.table::as.data.table(get(x = context_select, envir = globalenv())), silent = TRUE)
          remix_data_name <- context_select
          if("try-error" %in% class(remix_data)) {
            warning(paste0("Failed to retrieve data from the selection"), call. = FALSE)
            remix_data <- NULL
            remix_data_name <- ""
          }
        } else {
          remix_data <- NULL
          remix_data_name <- ""
        }
      }
    } else {
      remix_data <- NULL
      remix_data_name <- ""
    }
  }
  list(remix_data = remix_data, remix_data_name = remix_data_name)
}

#' Search for object with specific class in an environment
#'
#' @param what a class to look for
#' @param env An environment
#'
#' @return Character vector of the names of objects, NULL if none
#' @noRd
#'
#' @examples
#'
#' # NULL if no data.frame
#' search_obj("data.frame")
#'
#' library(ggplot2)
#' data("mpg")
#' search_obj("data.frame")
#'
#'
#' gg <- ggplot()
#' search_obj("ggplot")
#'
FindObject <- function(what = "data.table", env = globalenv()) {
  all <- ls(name = env)
  objs <- lapply(
    X = all,
    FUN = function(x) {
      if(inherits(get(x, envir = env), what = what)) {
        x
      } else {
        NULL
      }
    })
  objs <- unlist(objs)
  if (length(objs) == 1 && objs == "") {
    NULL
  } else {
    objs
  }
}

#' Tag to display code
#'
#' @param ... Character strings
#'
#' @noRd
rCodeContainer <- function(...) {
  code <- htmltools::HTML(as.character(tags$code(class = "language-r", ...)))
  htmltools::tags$div(htmltools::tags$pre(code))
}

#' @noRd
CEP <- function(x) if(is.null(x)) "NULL" else if(identical(x, character(0))) "NULL" else if(identical(x, numeric(0))) "NULL" else if(identical(x, integer(0))) "NULL" else if(identical(x, logical(0))) "NULL" else if(is.numeric(x)) x else if(length(x) > 1) paste0("c(", noquote(paste0("'", x, "'", collapse = ',')), ")") else paste0("'", x, "'")

#' @title UniqueLevels
#'
#' @param input passthrough
#' @param data data.table
#' @param GroupVars passthrough
#'
#' @export
UniqueLevels <- function(input, data, n, GroupVars=NULL) {
  if(is.null(GroupVars[[n]]) || is.na(GroupVars[[n]])) {
    x <- NULL
  } else {
    x <- tryCatch({
      c(sort(unique(data[[eval(GroupVars[[n]])]])))}, error = function(x)  NULL)
  }
  x
}

#' @title FilterValues
#'
#' @param data data.table
#' @param VarName Variable name
#' @param type 1 for min, 2 for max
#'
#' @export
FilterValues <- function(data, VarName = input[['FilterVariable_1']], type = 1) {
  if(tolower(class(data[[eval(VarName)]]) %chin% c('numeric', 'integer'))) {
    x <- unique(as.numeric(sort(data[, quantile(get(VarName), probs = c(seq(0, 1, 0.05)), na.rm = TRUE)])))
  } else if(tolower(class(data[[eval(VarName)]])) %chin% c('factor', 'character')) {
    x <- sort(data[, unique(get(VarName))])
  } else {
    x <- NULL
  }
  x
}

#' @title FilterLogicData
#'
#' @param data1 data.table
#' @param FilterLogic passthrough
#' @param FilterVariable passthrough
#' @param FilterValue passthrough
#' @param FilterValue2 passthrough
#'
#' @export
FilterLogicData <- function(data1, FilterLogic = input[['FilterLogic']], FilterVariable = input[['FilterVariable_1']], FilterValue = input[['FilterValue_1a']], FilterValue2 = input[['FilterValue_1b']], Debug = FALSE) {
  if(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('factor', 'character') || FilterLogic %in% c('%in%', '%like')) {
    if(Debug) print('FilterLogicData else if')
    if(Debug) print(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('factor', 'character'))
    if(FilterLogic == '%in%') {
      data1 <- data1[get(FilterVariable) %chin% c(eval(FilterValue))]
    } else if(FilterLogic == '%like%') {
      data1 <- data1[get(eval(FilterVariable)) %like% c(eval(FilterValue))]
    }
  } else if(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('numeric', 'integer', 'date', 'posix')) {
    if(Debug) print('FilterLogicData else if')
    if(Debug) print(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('numeric', 'integer', 'date', 'posix'))
    if(FilterLogic == '>') {
      data1 <- data1[get(FilterVariable) > eval(as.numeric(FilterValue))]
    } else if(FilterLogic == '>=') {
      data1 <- data1[get(FilterVariable) >= eval(as.numeric(FilterValue))]
    } else if(FilterLogic == '<') {
      data1 <- data1[get(FilterVariable) < eval(as.numeric(FilterValue2))]
    } else if(FilterLogic == '%between%') {
      if(Debug) print('At %between% section')
      if(Debug) print(as.numeric(FilterVariable))
      if(Debug) print(as.numeric(FilterValue))
      if(Debug) print(as.numeric(FilterValue2))
      if(Debug) print(data1)
      if(Debug) print('Run data.table operation')
      data1 <- data1[get(FilterVariable) >= eval(as.numeric(FilterValue)) & get(FilterVariable) <= eval(as.numeric(FilterValue2))]
      if(Debug) print('Done with data.table operation')
      if(Debug) print(data1)
    } else if(FilterLogic == 'not %between%') {
      data1 <- data1[get(FilterVariable) < eval(as.numeric(FilterValue)) | get(FilterVariable) > eval(as.numeric(FilterValue2))]
    } else {
      data1 <- data1[get(FilterVariable) <= eval(as.numeric(FilterValue2))]
    }
  }
  data1
}

#' @title KeyVarsInit
#'
#' @param data data.table
#' @param VarName Variable name
#' @param type 1 for min, 2 for max
#'
#' @export
KeyVarsInit <- function(data, VarName = NULL, type = 1) {
  if(!is.null(VarName) && tolower(VarName) != 'none' && any(c('numeric','integer') %chin% class(data[[eval(VarName)]]))) {
    minn <- tryCatch({floor(data[, min(get(VarName), na.rm = TRUE)])}, error = function(x) NULL)
    maxx <- tryCatch({ceiling(data[, max(get(VarName), na.rm = TRUE)])}, error = function(x) NULL)
    UData <- tryCatch({data[, unique(get(VarName))]}, error = function(x) NULL)
    if(!is.null(UData) && length(UData) <= 10L) {
      choices <- UData
    } else {
      choices <- tryCatch({unique(as.character(round(as.numeric(sort(data[, quantile(get(VarName), probs = c(seq(0, 1, 0.05)), na.rm = TRUE)])), 5L)))}, error = function(x) {
        tryCatch({UData}, error = NULL)
      })
    }
  } else if(!is.null(VarName) && tolower(VarName) != 'none' && any(c('Date','IDate','POSIXct','POSIXt','character','factor') %chin% class(data[[(eval(VarName))]][[1L]]))) {
    choices <- tryCatch({unique(data[[eval(VarName)]])}, error = function(x) NULL)
    maxx <- tryCatch({data[, max(get(VarName), na.rm = TRUE)]}, error = function(x) NULL)
    minn <- tryCatch({data[, min(get(VarName), na.rm = TRUE)]}, error = function(x) NULL)
  } else {
    minn <- NULL
    maxx <- NULL
    choices <- NULL
  }
  return(list(MinVal = minn, MaxVal = maxx, ChoiceInput = choices))
}

#' @title GetFilterValueLabel
#'
#' @param data data.table
#' @param VarName Variable name
#' @param type 1 for min, 2 for max
#'
#' @export
GetFilterValueLabel <- function(data, VarName = NULL, type = 1) {
  if((!is.null(VarName) || tolower(VarName) != 'none') && !is.null(data)) {
    if(is.numeric(data[[eval(VarName)]])) {
      if(type == 1) x <- 'Min Value' else x <- 'Max Value'
    }  else {
      x <- 'Select Levels'
    }
  } else {
    x <- 'N/A'
  }
  x
}

#' @title GetFilterValueMultiple
#'
#' @param data data.table
#' @param VarName Variable name
#' @param type 1 for min, 2 for max
#'
#' @export
GetFilterValueMultiple <- function(data, VarName = NULL, type = 1) {
  if((!is.null(VarName) || tolower(VarName) != 'none') && !is.null(data)) {
    if(!is.numeric(data[[eval(VarName)]])) x <- TRUE else x <- FALSE
  } else {
    x <- FALSE
  }
  x
}

#' @title CharNull
#'
#' @param x Value
#'
#' @export
CharNull <- function(x) {
  if(exists('x') && length(x) != 0) {
    return(as.character(x))
  } else {
    return(NULL)
  }
}

#' @title NumNull
#'
#' @param x value
#'
#' @export
NumNull <- function(x) {
  if(exists('x') && length(x) != 0) {
    return(as.numeric(x))
  } else {
    return(NULL)
  }
}

#' @title IntNull
#'
#' @param x value
#'
#' @export
IntNull <- function(x) {
  if(exists('x') && length(x) != 0) {
    return(as.integer(x))
  } else {
    return(NULL)
  }
}

#' Assign a data.table by name from an environment
#'
#' @param df character, name of the object
#' @param env an environment
#'
#' @return the object
#' @export
AssignData <- function(data, env = globalenv()) {
  if(deparse(substitute(data)) %in% ls(name = env)) {
    get(x = data, envir = env)
  } else {
    NULL
  }
}

#' @title BlankRow
#'
#' @description BlankRow add blank row with width w
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param W width of column
#'
#' @examples
#' \dontrun{
#' RemixAutoML::BlankRow(12)
#' }
#' @return Adds a row to your UI of width W
#' @export
BlankRow <- function(W) {
  shiny::fluidRow(shiny::column(width = W, htmltools::tags$br()))
}

#' @title observeEventLoad
#'
#' @description Used to load .Rdata files in Shiny and assign the object a name
#'
#' @family Shiny
#' @author Adrian Antico
#'
#' @param input Passthrough
#' @param InputVal The values that goes after input$
#' @param ObjectName The name of the object to assign the load output to
#'
#' @export
observeEventLoad <- function(input, InputVal = NULL, ObjectName = NULL) {
  shiny::observeEvent(eventExpr = input[[eval(InputVal)]], {
    inFile <- input[[eval(InputVal)]]
    e = new.env()
    name <- load(inFile$datapath, envir = e)
    assign(x = ObjectName, value = e[[name]], envir = .GlobalEnv)
  })
}

#' @title ReactiveLoadCSV
#'
#' @description Use this function to import csv's, track the time it was imported, and remove other objects
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param input Passthrough
#' @param InputVal Values that follows input$
#' @param ProjectList Supply the project list if available. NULL otherwise
#' @param DateUpdateName Supply the name for the ProjectList to store the import time
#' @param RemoveObjects List of objects to remove
#'
#' @export
ReactiveLoadCSV <- function(input, InputVal = NULL, ProjectList = NULL, DateUpdateName = NULL, RemoveObjects = NULL) {
  inFile <- input[[eval(InputVal)]]
  if(is.null(inFile)) return(NULL)
  if(!is.null(ProjectList)) ProjectList[[eval(DateUpdateName)]] <<- Sys.Date()
  if(!is.null(RemoveObjects)) for(i in seq_along(RemoveObjects)) if(exists(RemoveObjects[i])) rm(RemoveObjects[i])
  x <- data.table::fread(file = inFile$datapath)
  g <- RemixAutoML:::ColTypes(x)
  print(x)
  print(g)
  print(any('IDate' %in% g))
  if(any('IDate' %in% g)) {
    for(zz in seq_along(x)) {
      if(class(x[[names(x)[zz]]])[1L] == 'IDate') {
        print(class(x[[names(x)[zz]]])[1L] == 'IDate')
        x[, eval(names(x)[zz]) := as.Date(get(names(x)[zz]))]
      }
    }
  }
  return(x)
}

#' @title Store Args values within a project
#'
#' @description Automatically save arguments to project list
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param input This is the input value within a Shiny context
#' @param ProjectList This is the VarNameument collection list
#' @param VarName The name of the VarNameument you want to store
#' @param Type "character" "numeric" "logical"
#' @param Default default value that gets saved
#'
#' @examples
#' \dontrun{
#' StoreVarNames(input, ProjectList, "NTrees", "numeric", 1000)
#' }
#'
#' @return Updates ProjectList inside function. Do not assign function to anything
#' @export
StoreArgs <- function(input,
                      ProjectList,
                      VarName,
                      Type,
                      Default) {
  if(Type == "character") {
    tryCatch({if(class(input[[VarName]]) != "NULL") {
      ProjectList[[VarName]] <<- as.character(input[[VarName]])
    } else {
      ProjectList[[VarName]] <<- Default
    }}, error = function(x) Default)

  } else if(Type == "numeric") {
    tryCatch({if(class(input[[VarName]]) != "NULL") {
      ProjectList[[VarName]] <<- as.numeric(input[[VarName]])
    } else {
      ProjectList[[VarName]] <<- Default
    }}, error = function(x) Default)

  } else if(Type == "logical") {
    tryCatch({if(class(input[[VarName]]) != "NULL") {
      ProjectList[[VarName]] <<- as.logical(input[[VarName]])
    } else {
      ProjectList[[VarName]] <<- Default
    }}, error = function(x) Default)
  }
}

#' @title Save VarName values within a project
#'
#' @description Automatically save VarNames to project list
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param input This is the input value within a Shiny context
#' @param VarName The name of the VarNameument you want to store
#' @param Type "character" "numeric" "logical"
#' @param Default default value
#' @param Switch = FALSE
#'
#' @examples
#' \dontrun{
#' Aggregate <- RemixAutoML::ReturnParam(input, VarName = "TS_AggregateFunction", Type = "character", Default = "mean")
#' }
#'
#' @return Updates ProjectList inside function
#' @export
ReturnParam <- function(input,
                        VarName,
                        Type,
                        Default,
                        Switch = FALSE) {

  # Type == numeric
  if(Switch) {
    if(Type == "numeric") {
      if(class(input[[VarName]]) != "NULL") {
        return(as.numeric(input[[VarName]]))
      } else if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(class(input[[VarName]]) != "NULL") {
          return(as.numeric(input[[VarName]]))
        } else {
          return(Default)
        }
      } else {
        return(Default)
      }
    }
  } else {
    if(Type == "numeric") {
      if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(class(input[[VarName]]) != "NULL") {
          return(as.numeric(input[[VarName]]))
        } else {
          return(Default)
        }
      } else if(class(input[[VarName]]) != "NULL") {
        return(as.numeric(input[[VarName]]))
      } else {
        return(Default)
      }
    }
  }

  # Type == logical
  if(Switch) {
    if(Type == "logical") {
      if(class(input[[VarName]]) != "NULL") {
        return(as.logical(input[[VarName]]))
      } else if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(class(input[[VarName]]) != "NULL") {
          return(as.logical(input[[VarName]]))
        } else {
          return(Default)
        }
      } else {
        return(Default)
      }
    }
  } else {
    if(Type == "logical") {
      if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(class(input[[VarName]]) != "NULL") {
          return(as.logical(input[[VarName]]))
        } else {
          return(Default)
        }
      } else if(class(input[[VarName]]) != "NULL") {
        return(as.logical(input[[VarName]]))
      } else {
        return(Default)
      }
    }
  }

  # Type == character
  if(Switch) {
    if(Type == "character") {
      if(class(input[[VarName]]) != "NULL") {
        return(as.character(input[[VarName]]))
      } else if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(class(input[[VarName]]) != "NULL") {
          return(as.character(input[[VarName]]))
        } else {
          return(Default)
        }
      } else {
        return(Default)
      }
    }
  } else {
    if(Type == "character") {
      if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(class(input[[VarName]]) != "NULL") {
          return(as.character(input[[VarName]]))
        } else {
          return(Default)
        }
      } else if(class(input[[VarName]]) != "NULL") {
        return(as.character(input[[VarName]]))
      } else {
        return(Default)
      }
    }
  }
}

#' @title ArgNullCheck
#'
#' @description ArgNullCheck
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param Input input
#' @param InputID inputId
#' @param Default Default value
#' @examples
#' \dontrun{
#' ArgValue <<- RemixAutoML::ArgNullCheck(Input = input, InputID = "TS_Value", Default = 2)
#' }
#' @export
ArgNullCheck <- function(Input,
                         InputID,
                         Default) {
  if(!is.null(Input[[InputID]])) {
    return(Input[[InputID]])
  } else {
    return(Default)
  }
}

#' @title ArgNullCheck2
#'
#' @description ArgNullCheck2
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param Input input
#' @param InputID inputId
#' @param Default Default value
#' @param Type numeric character logical
#' @examples
#' \dontrun{
#' ArgValue <<- RemixAutoML::ArgNullCheck2(Input = input, InputID = "TS_Value", Default = 2, Type = "numeric")
#' }
#' @export
ArgNullCheck2 <- function(Input,
                          InputID,
                          Default,
                          Type = "numeric") {

  # Numeric
  if(Type == "numeric") {
    if(Type == "character") {
      if(exists("ProjectList")) {
        if(InputID %chin% names(ProjectList)) {
          return(ProjectList[[InputID]])
        } else if(class(Input[[InputID]]) != "NULL") {
          return(as.numeric(Input[[InputID]]))
        } else {
          return(Default)
        }
      } else if(class(Input[[InputID]]) != "NULL") {
        return(as.numeric(Input[[InputID]]))
      } else {
        return(Default)
      }
    }
  }

  # Character
  if(Type == "character") {
    if(exists("ProjectList")) {
      if(InputID %chin% names(ProjectList)) {
        return(ProjectList[[InputID]])
      } else if(class(Input[[InputID]]) != "NULL") {
        return(as.character(Input[[InputID]]))
      } else {
        return(Default)
      }
    } else if(class(Input[[InputID]]) != "NULL") {
      return(as.character(Input[[InputID]]))
    } else {
      return(Default)
    }
  }

  # Logical
  if(Type == "logical") {
    if(exists("ProjectList")) {
      if(InputID %chin% names(ProjectList)) {
        return(ProjectList[[InputID]])
      } else if(class(Input[[InputID]]) != "NULL") {
        return(as.logical(Input[[InputID]]))
      } else {
        return(Default)
      }
    } else if(class(Input[[InputID]]) != "NULL") {
      return(as.logical(Input[[InputID]]))
    } else {
      return(Default)
    }
  }
}

#' @title PickerInput
#'
#' @description PickerInput automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID Feeds ProjectList and inputId. Argument saved in ProjectList
#' @param Label Feeds label
#' @param Choices Feeds choices
#' @param SelectedDefault Feeds selected for cases where ProjectList has a null element
#' @param Size Feeds size in the options list
#' @param SelectedText Feeds selected-text-format in options list
#' @param Multiple Feeds multiple for enabling selecting more than one element from list
#' @param ActionBox Feeds actions-box for option list
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   RemixAutoML::PickerInput(InputID = "TS_CARMA_HolidayMovingAverages", Label = "Select Holiday Count MA's", Choices = as.character(0:50),
#'                            SelectedDefault = as.character(c(1,2)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
PickerInput <- function(InputID = "TS_CARMA_HolidayMovingAverages",
                        Label = "Select Holiday Count MA's",
                        Choices = as.character(0:50),
                        SelectedDefault = as.character(c(1,2)),
                        Size = 10,
                        SelectedText = "count > 1",
                        Multiple = TRUE,
                        ActionBox = TRUE,
                        Debug = FALSE) {
  Options <- list(
    `actions-box` = ActionBox,
    size = Size,
    `selected-text-format` = SelectedText)
  return(if(exists("ProjectList")) {
    tryCatch({
      if(!is.null(ProjectList[[InputID]])) {
        shinyWidgets::pickerInput(inputId = InputID, label = Label, choices = Choices, selected = ProjectList[[InputID]], options = Options, multiple = Multiple)
      } else {
        shinyWidgets::pickerInput(inputId = InputID, label = Label, choices = Choices, selected = SelectedDefault, options = Options, multiple = Multiple)
      }}, error = function(x) shinyWidgets::pickerInput(inputId = InputID, label = Label, choices = Choices, selected = SelectedDefault, options = Options, multiple = Multiple))
  } else {
    if(Debug) {
      print(InputID)
      print(Label)
      print(Choices)
      print(SelectedDefault)
      print(Options)
      print(Multiple)
    }
    tryCatch({
      shinyWidgets::pickerInput(inputId = InputID, label = Label, choices = Choices, selected = SelectedDefault, options = Options, multiple = Multiple)},
      error = function(x) {
        shinyWidgets::pickerInput(inputId = InputID, label = Label, choices = "No Data Loaded !!", selected = "No Data Loaded !!", options = Options, multiple = Multiple)
    })
  })
}

#' @title NumericInput
#'
#' @description NumericInput automatically builds a numeric input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID Feeds ProjectList and inputId. Argument saved in ProjectList
#' @param Label Feeds label
#' @param Step Feeds size in the options list
#' @param Value Default
#' @param Min Min value
#' @param Max Max value
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   RemixAutoML::NumericInput(InputID = "TS_CARMA_HolidayMovingAverages",
#'                             Label = "Select Holiday Count MA's",
#'                             Min = 1,
#'                             Max = 1,
#'                             Value = 1,
#'                             Step = 1)})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
NumericInput <- function(InputID = "TS_CARMA_HolidayMovingAverages",
                         Label = "Select Holiday Count MA's",
                         Step = 10,
                         Value = 1,
                         Min = 1,
                         Max = 10) {
  return(if(exists("ProjectList")) {
    tryCatch({
      if(!is.null(ProjectList[[InputID]])) {
        shiny::numericInput(inputId = InputID, label = Label, value = ProjectList[[InputID]][[1L]], min = Min, max = Max, step = Step)
      } else {
        shiny::numericInput(inputId = InputID, label = Label, value = Value, min = Min, max = Max, step = Step)
      }}, error = function(x) shiny::numericInput(inputId = InputID, label = Label, value = Value, min = Min, max = Max, step = Step))
  } else {
    shiny::numericInput(inputId = InputID, label = Label, value = Value, min = Min, max = Max, step = Step)
  })
}

#' @title DateInput
#'
#' @description DateInput automatically builds a date input with ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID Feeds ProjectList and inputId. Argument saved in ProjectList
#' @param Label Feeds label
#' @param Value Default
#' @param Min Min value
#' @param Max Max value
#' @param Format Date format
#'
#' @examples
#' \dontrun{
#' output$TS_Date <- renderUI({
#'   RemixAutoML::DateInput(InputID = "TS_CARMA_HolidayMovingAverages",
#'                          Label = "Import Data Creation Date",
#'                          Value = Sys.Date(),
#'                          Min = "1970-01-01",
#'                          Max = "2100-01-01",
#'                          Format = "yyyy-mm-dd")})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
DateInput <- function(InputID = "TS_CARMA_HolidayMovingAverages",
                      Label = "Import Data Creation Date",
                      Value = Sys.Date(),
                      Min = "1970-01-01",
                      Max = "2100-01-01",
                      Format = "yyyy-mm-dd") {
  return(if(exists("ProjectList")) {
    if(!is.null(ProjectList[[InputID]])) {
      dateInput(inputId = InputID, label = Label, value = ProjectList[[InputID]], min = Min, max = Max, format = Format)
    } else {
      dateInput(inputId = InputID, label = Label, value = Sys.Date(), min = Min, max = Max, format = Format)
    }
  } else {
    dateInput(inputId = InputID, label = Label, value = Sys.Date(), min = Min, max = Max, format = Format)
  })
}

#' @title TextInput
#'
#' @description TextInput automatically builds a text input with ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID Feeds ProjectList and inputId. Argument saved in ProjectList
#' @param Label Feeds label
#' @param Value Default
#' @param Width width arg
#' @param Format Date format
#'
#' @examples
#' \dontrun{
#' output$TS_Date <- renderUI({
#'   RemixAutoML::TextInput(InputID = "TS_CARMA_HolidayMovingAverages",
#'                          Label = "Import Data Creation Date",
#'                          Value = Sys.Date(),
#'                          Placeholder = "yyyy-mm-dd")})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
TextInput <- function(InputID = "TS_CARMA_HolidayMovingAverages",
                      Label = "Path to Data",
                      Value = NULL,
                      Width = "100%",
                      Placeholder = "NULL") {
  return(if(exists("ProjectList")) {
    if(!is.null(ProjectList[[InputID]])) {
      textInput(inputId = InputID, label = Label, value = ProjectList[[InputID]], width = Width, placeholder = Placeholder)
    } else {
      textInput(inputId = InputID, label = Label, value = Value, width = Width, placeholder = Placeholder)
    }
  } else {
    textInput(inputId = InputID, label = Label, value = Value, width = Width, placeholder = Placeholder)
  })
}

#' @title PickerInput_GetLevels
#'
#' @description PickerInput_GetLevels automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param input input object within shiny context
#' @param data 'SourceData' or whatever the name of your data is
#' @param NumGroupVar Which group var to select
#' @param InputID Feeds ProjectList and inputId. Argument saved in ProjectList
#' @param InputID2 Secondary object name
#' @param Choices Feeds choices
#' @param SelectedDefault Feeds selected for cases where ProjectList has a null element
#' @param Size Feeds size in the options list
#' @param SelectedText Feeds selected-text-format in options list
#' @param Multiple Feeds multiple for enabling selecting more than one element from list
#' @param ActionBox Feeds actions-box for option list
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   RemixAutoML::PickerInput_GetLevels(
#'     input, InputID = "TS_CARMA_HolidayMovingAverages", Label = "Select Holiday Count MA's", Choices = as.character(0:50),
#'     SelectedDefault = as.character(c(1,2)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
PickerInput_GetLevels <- function(input,
                                  data = 'SourceData',
                                  NumGroupVar = 3,
                                  InputID = "TS_CARMA_HolidayMovingAverages",
                                  InputID2 = "timeSeriesGroupVars",
                                  Choices = as.character(0:50),
                                  SelectedDefault = as.character(c(1,2)),
                                  Size = 10,
                                  SelectedText = "count > 1",
                                  Multiple = TRUE,
                                  ActionBox = TRUE) {
  return(
    if(exists(eval(data))) {
      if(!is.null(input[[InputID2]])) {
        if(length(input[[InputID2]]) >= NumGroupVar && !'None' %in% input[[InputID2]]) {
          shinyWidgets::pickerInput(inputId = InputID, label = tags$span(style='color: blue;', paste0(input[[InputID2]][[NumGroupVar]]," Levels")),
                      choices = Choices, selected = SelectedDefault,
                      options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE, width = "100%")
        } else {
          shinyWidgets::pickerInput(inputId = InputID, label = tags$span(style='color: blue;', "< N/A >"), choices = SelectedDefault, selected = SelectedDefault, multiple = TRUE, width = "100%")
        }
      } else {
        shinyWidgets::pickerInput(inputId = InputID, label = tags$span(style='color: blue;', "< N/A >"), choices = SelectedDefault, selected = SelectedDefault, multiple = TRUE, width = "100%")
      }
    } else {
      shinyWidgets::pickerInput(inputId = InputID, label = tags$span(style='color: blue;', "< N/A >"), choices = SelectedDefault, selected = SelectedDefault, multiple = TRUE, width = "100%")
    }
  )
}

#' @title PickerInput_GetLevels2
#'
#' @description PickerInput_GetLevels2 automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param input input object within shiny context
#' @param data 'SourceData' or whatever the name of your data is
#' @param NumGroupVar Which group var to select
#' @param InputID Feeds ProjectList and inputId. Argument saved in ProjectList
#' @param InputID2 Values from input2. In first version the input is referenced inside function
#' @param Choices Feeds choices
#' @param SelectedDefault Feeds selected for cases where ProjectList has a null element
#' @param Size Feeds size in the options list
#' @param SelectedText Feeds selected-text-format in options list
#' @param Multiple Feeds multiple for enabling selecting more than one element from list
#' @param ActionBox Feeds actions-box for option list
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   RemixAutoML::PickerInput_GetLevels2(
#'     input, InputID = "TS_CARMA_HolidayMovingAverages", Label = "Select Holiday Count MA's", Choices = as.character(0:50),
#'     SelectedDefault = as.character(c(1,2)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
PickerInput_GetLevels2 <- function(input,
                                  data = 'SourceData',
                                  NumGroupVar = 3,
                                  InputID = "TS_CARMA_HolidayMovingAverages",
                                  InputID2 = "timeSeriesGroupVars",
                                  Choices = as.character(0:50),
                                  SelectedDefault = as.character(c(1,2)),
                                  Size = 10,
                                  SelectedText = "count > 1",
                                  Multiple = TRUE,
                                  ActionBox = TRUE) {
  return(
    if(exists(eval(data))) {
      if(!is.null(InputID2)) {
        if(length(InputID2) >= NumGroupVar && !'None' %in% InputID2) {
          shinyWidgets::pickerInput(inputId = InputID, label = tags$span(style='color: blue;', paste0(InputID2[[NumGroupVar]]," Levels")),
                                    choices = Choices, selected = SelectedDefault,
                                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE, width = "100%")
        } else {
          shinyWidgets::pickerInput(inputId = InputID, label = tags$span(style='color: blue;', "< N/A >"), choices = SelectedDefault, selected = SelectedDefault, multiple = TRUE, width = "100%")
        }
      } else {
        shinyWidgets::pickerInput(inputId = InputID, label = tags$span(style='color: blue;', "< N/A >"), choices = SelectedDefault, selected = SelectedDefault, multiple = TRUE, width = "100%")
      }
    } else {
      shinyWidgets::pickerInput(inputId = InputID, label = tags$span(style='color: blue;', "< N/A >"), choices = SelectedDefault, selected = SelectedDefault, multiple = TRUE, width = "100%")
    }
  )
}

#' @title PreparePlotData
#'
#' @description PreparePlotData automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param data Source data in shiny app
#' @param SubsetOnly Set to TRUE to only subset data
#' @param Aggregate Session object indicating whether to use mean or sum
#' @param TargetVariable Target variable name
#' @param GroupVariables Group variable names
#' @param DateVariable Date variable name
#' @param G1Levels Name of group 1 levels list element
#' @param G2Levels Name of group 2 levels list element
#' @param G3Levels Name of group 3 levels list element
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#'   PlotData <- RemixAutoML::PreparePlotData(
#'     data,
#'     SubsetOnly = FALSE,
#'     Aggregate = "mean",
#'     TargetVariable = "TargetVariables",
#'     DateVariable = "DateVariables",
#'     GroupVariables = GroupVariables,
#'     G1Levels = "TS_Group1Levels",
#'     G2Levels = "TS_Group2Levels",
#'     G3Levels = "TS_Group3Levels")
#' }
#' @return PreparePlotData object for server.R to
#' @export
PreparePlotData <- function(data,
                            SubsetOnly = FALSE,
                            Aggregate = NULL,
                            TargetVariable = NULL,
                            DateVariable = NULL,
                            GroupVariables = NULL,
                            G1Levels = NULL,
                            G2Levels = NULL,
                            G3Levels = NULL,
                            Debug = FALSE) {

  # Define function ----
  if(Aggregate == "mean") {
    Agg <- as.function(x = , mean)
  } else if(Aggregate == "sum") {
    Agg <- as.function(x = , sum)
  }

  AggFun <- function(dt = data, A = Agg, S = SubsetOnly, t = TargetVariable, G = GroupVariables, D = DateVariable) {
    if(!S && !is.null(G) && !is.null(D)) {
      dt <- dt[, lapply(.SD, A), by = c(eval(G), eval(D)), .SDcols = c(t)]
    } else if(!S && is.null(G) && !is.null(D)) {
      dt <- dt[, lapply(.SD, A), by = c(eval(D)), .SDcols = c(t)]
    } else if(!S && !is.null(G) && is.null(D)) {
      dt <- dt[, lapply(.SD, A), by = c(eval(G)), .SDcols = c(t)]
    }
    return(dt)
  }

  # G1 & G2 & G3 ----
  if(!is.null(GroupVariables[1L]) && !is.null(GroupVariables[2L]) && !is.null(GroupVariables[3L])) {

    if(Debug) print('G1 & G2 & G3 ----')
    if(Debug) {print(G1Levels); print(G2Levels); print(G3Levels)}

    # G2 & G3 ----
    if(is.null(G1Levels) && !is.null(G2Levels) && !is.null(G3Levels)) {
      if(Debug) print('G2 & G3 ----')
      x <- data[get(GroupVariables[2L]) %in% c(eval(G2Levels)) & get(GroupVariables[3L]) %in% c(eval(G3Levels))]
      x <- AggFun(dt = x, A = Agg, S = SubsetOnly)
      return(x)
    }

    # G1 & G3 ----
    if(!is.null(G1Levels) && is.null(G2Levels) && !is.null(G3Levels)) {
      if(Debug) print('G1 & G3 ----')
      x <- data[get(GroupVariables[1L]) %in% c(eval(G1Levels)) & get(GroupVariables[3L]) %in% c(eval(G3Levels))]
      x <- AggFun(dt = x, A = Agg, S = SubsetOnly)
      return(x)
    }

    # G1 & G2 ----
    if(!is.null(G1Levels) && !is.null(G2Levels) && is.null(G3Levels)) {
      if(Debug) print('G1 & G2 ----')
      x <- data[get(GroupVariables[1L]) %in% c(eval(G1Levels)) & get(GroupVariables[2L]) %in% c(eval(G2Levels))]
      x <- AggFun(dt = x, A = Agg, S = SubsetOnly)
      return(x)
    }

    # G3 ----
    if(is.null(G1Levels) && is.null(G2Levels) && !is.null(G3Levels)) {
      if(Debug) print('G3 ----')
      x <- data[get(GroupVariables[3L]) %in% c(eval(G3Levels))]
      x <- AggFun(dt = x, A = Agg, S = SubsetOnly)
      return(x)
    }

    # G2 ----
    if(is.null(G1Levels) && !is.null(G2Levels) && is.null(G3Levels)) {
      if(Debug) print('G2 ----')
      x <- data[get(GroupVariables[2L]) %in% c(eval(G2Levels))]
      x <- AggFun(dt = x, A = Agg, S = SubsetOnly)
      return(x)
    }

    # G1 ----
    if(!is.null(G1Levels) && is.null(G2Levels) && is.null(G3Levels)) {
      if(Debug) print('G1 ----')
      x <- data[get(GroupVariables[1L]) %in% c(eval(G1Levels))]
      x <- AggFun(dt = x, A = Agg, S = SubsetOnly)
      return(x)
    }

    # None ----
    if(is.null(G1Levels) && is.null(G2Levels) && is.null(G3Levels)) {
      if(Debug) print('None ----')
      x <- AggFun(dt = data, A = Agg, S = SubsetOnly)
      if(Debug) print(x)
      return(x)
    }

    # G1 & G2 & G3 ----
    if(!is.null(G1Levels) && !is.null(G2Levels) && !is.null(G3Levels)) {
      if(Debug) print('# G1 & G2 & G3 ----')
      x <- data[get(GroupVariables[1L]) %in% c(eval(G1Levels)) & get(GroupVariables[2L]) %in% c(eval(G2Levels)) & get(GroupVariables[3L]) %in% c(eval(G3Levels))]
      x <- AggFun(dt = x, A = Agg, S = SubsetOnly)
      return(x)
    }
  }

  # G1 & G2 Top ----
  if(!is.null(GroupVariables[1L]) && !is.null(GroupVariables[2L]) && is.null(GroupVariables[3L])) {

    if(Debug) print('G1 & G2 Top ----')

    # G2 ----
    if(is.null(G1Levels) && !is.null(G2Levels)) {
      if(Debug) print('G2 ----')
      x <- data[get(GroupVariables[2L]) %in% c(eval(G2Levels))]
      x <- AggFun(dt = x, A = Agg, S = SubsetOnly)
      return(x)
    }

    # G1 ----
    if(!is.null(G1Levels) && is.null(G2Levels)) {
      if(Debug) print('G1 ----')
      x <- data[get(GroupVariables[1L]) %in% c(eval(G1Levels))]
      x <- AggFun(dt = x, A = Agg, S = SubsetOnly)
      return(x)
    }

    # G1 & G2 ----
    if(!is.null(G1Levels) && !is.null(G2Levels)) {
      if(Debug) print('G1 & G2 ----')
      data <- data[get(GroupVariables[1L]) %in% c(G1Levels) & get(GroupVariables[2L]) %in% c(G2Levels)]
      if(!SubsetOnly) data <- data[, .SD, .SDcols = c(eval(TargetVariable), eval(GroupVariables), eval(DateVariable))]
      return(data)
    }

    # None ----
    if(is.null(G1Levels) && is.null(G2Levels)) {
      if(Debug) print('None ----')
      if(!SubsetOnly && !is.null(DateVariable)) {
        if(GroupVariables == 'None') GroupVariables <- NULL
        x <- data[, lapply(.SD, Agg), by = c(eval(DateVariable), eval(GroupVariables)), .SDcols = c(TargetVariable)]
      } else {
        x <- data
      }
      return(x)
    }
  }

  # G1 ----
  if(!is.null(GroupVariables[1L]) && is.null(GroupVariables[2L])) {

    if(Debug) print('G1 Tope ----')

    # None ----
    if(is.null(G1Levels)) {
      if(Debug) print('None ----')
      x <- AggFun(dt = data, A = Agg, S = SubsetOnly)
      return(x)
    }

    # G1 ----
    if(!is.null(G1Levels)) {
      if(Debug) print('G1 ----')
      x <- data[get(GroupVariables[1L]) %in% c(eval(G1Levels))]
      x <- AggFun(dt = x, A = Agg, S = SubsetOnly)
      return(x)
    }
  }

  # NO Grouping Variables ----
  if(is.null(GroupVariables)) {
    if(Debug) print('No Goruping Variables ----')
    if(!SubsetOnly && !is.null(DateVariable)) {
      x <- data[, .SD, .SDcols = c(eval(TargetVariable), eval(DateVariable))]
      x <- AggFun(dt = x, A = Agg, S = SubsetOnly)
    } else {
      x <- data
    }
    return(x)
  }

  # None up till now ----
  if(!exists("x")) {
    x <- AggFun(dt = data, A = Agg, S = SubsetOnly)
    return(x)
  }

  # Return
  return(NULL)
}

#' @title GenerateEvaluationMetrics
#'
#' @description GenerateEvaluationMetrics calculates evaluation metrics for out of sample forecast and evaluation data
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param EvalData Source data in shiny app
#' @param TargetName Target variable name
#' @param GroupNames Group variable names
#' @param DateName Date variable name
#'
#' @examples
#' \dontrun{
#'   PlotData <- RemixAutoML::PreparePlotData(input, TargetVariable = "TargetVariables", DateVariable = "DateVariables", GroupVariables = GroupVariables, G1Levels = "TS_Group1Levels", G2Levels = "TS_Group2Levels", G3Levels = "TS_Group3Levels")
#' }
#' @return PreparePlotData object for server.R to
#' @export
GenerateEvaluationMetrics <- function(EvalData = NULL,
                                      TargetName = NULL,
                                      DateName = NULL,
                                      GroupNames = NULL) {

  # Weekly Rollup ----
  Metrics1 <- data.table::rollup(x = EvalData, j = lapply(.SD, sum), .SDcols = c(eval(TargetName), "Predictions"), by = c(eval(DateName), eval(GroupNames)))
  for(x in GroupNames) Metrics1[, eval(x) := data.table::fifelse(is.na(get(x)), "Total", get(x))]
  Metrics1 <- Metrics1[!is.na(get(DateName))]

  # Add Metrics ----
  Metrics1[, Error := get(TargetName) - Predictions]
  Metrics1[, PercError := data.table::fifelse(get(TargetName) == 0, 12345, Error / get(TargetName))]
  Metrics1[, MAE := abs(get(TargetName) - Predictions)]
  Metrics1[, RMSE := (get(TargetName) - Predictions) ^ 2]
  Metrics1[, MAPE := data.table::fifelse(get(TargetName) == 0, 12345, MAE / get(TargetName))]

  # Remove Date and Final Calcs ----
  if(any(Metrics1$MAPE == 12345)) Metrics1 <- Metrics1[MAPE != 12345]
  FinalMetrics <- Metrics1[, list(Error = mean(Error), MAE = mean(MAE), PercError = mean(PercError), MAPE = mean(MAPE), RMSE = sqrt(mean(RMSE))), by = c(eval(GroupNames))]
  FinalMetrics[, PercError := round(100 * PercError, 3)]
  FinalMetrics[, MAPE := round(100 * MAPE, 3)]
  FinalMetrics[, RMSE := round(RMSE, 1)]
  FinalMetrics[, Error := round(100 * Error, 1)]
  FinalMetrics[, MAE := round(MAE, 1)]

  # Return data
  return(FinalMetrics)
}

#' @title withConsoleRedirect
#'
#' @param containerId Passthrough
#' @param expr Code
withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

#' @title ColumnSubsetDataTable
#'
#' @description ColumnSubsetDataTable will subset data tables by column
#'
#' @family Data Wrangling
#'
#' @author Adrian Antico
#'
#' @param data data.table
#' @param TargetColumnName Target variable
#' @param DateColumnName Date variable
#' @param GroupVars Group variables
#'
#' @noRd
ColumnSubsetDataTable <- function(data,
                                  TargetColumnName = NULL,
                                  DateColumnName = NULL,
                                  GroupVars = NULL) {

  # Check to see if data is actual data----
  if(!(any(class(data) %in% c("data.table","data.frame","tibble")))) {
    return(NULL)
  }

  # Subset----
  data <- data[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName),eval(GroupVars))]

  # Ensure Date Column is Date----
  if(is.character(data[[eval(DateColumnName)]])) {
    x <- data[1,get(DateColumnName)]
    x1 <- lubridate::guess_formats(x, orders = c("mdY","BdY","Bdy","bdY","bdy","mdy","dby","Ymd","Ydm"))
    data[, eval(DateColumnName) := as.Date(get(DateColumnName), tryFormats = x1)]
  }

  # Return data----
  return(data)
}

#' @title DataDisplayMeta
#'
#' @description DataDisplayMeta
#'
#' @author Adrian Antico
#'
#' @family Data Wrangling
#'
#' @param data Source data
#'
#' @noRd
DataDisplayMeta <- function(data) {

  # Check to see if data is actual data ----
  if(!(any(class(data) %in% c("data.table","data.frame","tibble")))) stop("data is not a data.table")

  # Begin process----
  Counter <- 0L
  N <- data[, .N]
  x <- data.table::data.table(Variable = rep("donotuse", N), Type = rep("donotuse", N))
  for(name in names(data)) {
    Counter <- Counter + 1L
    data.table::set(x, i = Counter, j = "Variable", value = eval(name))
    data.table::set(x, i = Counter, j = "DataType", value = class(data[[eval(name)]]))
  }

  # Return table
  return(x[Variable != "donotuse"])
}

#' @title TimeSeriesMelt
#'
#' @description TimeSeriesMelt
#'
#' @family Data Wrangling
#'
#' @author Adrian Antico
#'
#' @param data source data
#' @param TargetVariable vector of target variable names
#' @param DateVariable Name of date variable
#' @param GroupVariables Vector of group variable names
#'
#' @noRd
TimeSeriesMelt <- function(data,
                           TargetVariable = NULL,
                           DateVariable = NULL,
                           GroupVariables = NULL) {

  # 2 Cases:
  #  Multiple Targets + Grouping Variables
  #  Multiple Targets + No Grouping Variables
  if(length(TargetVariable) > 1) {
    if(!is.null(GroupVariables)) {
      data <- data.table::melt(
        data = data,
        id.vars = c(eval(DateVariable),eval(GroupVariables)),
        measure.vars = eval(TargetVariable),
        variable.name = "GroupVar",
        value.name = "TargetSeries")
    } else {
      data <- data.table::melt(
        data = data,
        id.vars = eval(DateVariable),
        measure.vars = c(eval(TargetVariable)),
        variable.name = "GroupVar",
        value.name = "TargetSeries")
    }
  }

  # Return
  return(data)
}
