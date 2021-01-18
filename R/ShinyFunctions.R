#' @title Store Args values within a project
#'
#' @description Automatically save VarNameuments to project list
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

#' @title Save VarNameument values within a project
#'
#' @description Automatically save VarNameuments to project list
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param input This is the input value within a Shiny context
#' @param VarName The name of the VarNameument you want to store
#' @param Type "character" "numeric" "logical"
#' @param Default default value
#'
#' @examples
#' \dontrun{
#' Aggregate <- RemixAutoML::ReturnParam(input, VarName = "TS_AggregateFunction", Type = "character", Default = "mean")
#' }
#'
#' @return Updates ProjectList inside function. Do not assign function to anything
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
                        ActionBox = TRUE) {
  return(if(exists("ProjectList")) {
    tryCatch({
      if(!is.null(ProjectList[[InputID]])) {
        shinyWidgets::pickerInput(inputId = InputID, label = Label, choices = Choices, selected = ProjectList[[InputID]],
                                  options = list(`actions-box` = ActionBox, size = Size, `selected-text-format` = SelectedText), multiple = Multiple)
      } else {
        shinyWidgets::pickerInput(inputId = InputID, label = Label, choices = Choices, selected = SelectedDefault,
                    options = list(`actions-box` = ActionBox, size = Size, `selected-text-format` = SelectedText), multiple = Multiple)
      }}, error = function(x) shinyWidgets::pickerInput(inputId = InputID, label = Label, choices = Choices, selected = SelectedDefault,
                                                        options = list(`actions-box` = ActionBox, size = Size, `selected-text-format` = SelectedText), multiple = Multiple))
  } else {
    tryCatch({
    shinyWidgets::pickerInput(inputId = InputID, label = Label, choices = Choices, selected = SelectedDefault,
                              options = list(`actions-box` = ActionBox, size = Size, `selected-text-format` = SelectedText), multiple = Multiple)},
    error = function(x) {
      shinyWidgets::pickerInput(inputId = InputID, label = Label, choices = "No Data Loaded !!", selected = "No Data Loaded !!",
                                options = list(`actions-box` = ActionBox, size = Size, `selected-text-format` = SelectedText), multiple = Multiple)
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
                                  NumGroupVar = 3,
                                  InputID = "TS_CARMA_HolidayMovingAverages",
                                  InputID2 = "timeSeriesGroupVars",
                                  Choices = as.character(0:50),
                                  SelectedDefault = as.character(c(1,2)),
                                  Size = 10,
                                  SelectedText = "count > 1",
                                  Multiple = TRUE,
                                  ActionBox = TRUE) {
  return(if(exists("SourceData")) {
    if(!is.null(input[[InputID2]])) {
      if(length(input[[InputID2]]) >= NumGroupVar) {
        pickerInput(inputId = InputID, label = paste0(input[[InputID2]][[NumGroupVar]]," Levels"),
                    choices = Choices, selected = "",
                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = TRUE, width = "100%")
      } else {
        pickerInput(inputId = InputID, label = "< N/A >",
                    choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
      }
    } else {
      pickerInput(inputId = InputID, label = "< N/A >",
                  choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
    }
  } else {
    pickerInput(inputId = InputID, label = "< N/A >",
                choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
  })
}

#' @title PreparePlotData
#'
#' @description PreparePlotData automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param input input object within shiny context
#' @param PlotDataForecast Source data in shiny app
#' @param Aggregate Session object indicating whether to use mean or sum
#' @param TargetVariable Target variable name
#' @param GroupVariables Group variable names
#' @param DateVariable Date variable name
#' @param G1Levels Name of group 1 levels list element
#' @param G2Levels Name of group 2 levels list element
#' @param G3Levels Name of group 3 levels list element
#'
#' @examples
#' \dontrun{
#'   PlotData <- RemixAutoML::PreparePlotData(input, PlotDataForecast, Aggregate = "mean", TargetVariable = "TargetVariables", DateVariable = "DateVariables", GroupVariables = GroupVariables, G1Levels = "TS_Group1Levels", G2Levels = "TS_Group2Levels", G3Levels = "TS_Group3Levels")
#' }
#' @return PreparePlotData object for server.R to
#' @export
PreparePlotData <- function(input,
                            PlotDataForecast,
                            Aggregate = NULL,
                            TargetVariable = NULL,
                            DateVariable = NULL,
                            GroupVariables = NULL,
                            G1Levels = NULL,
                            G2Levels = NULL,
                            G3Levels = NULL) {

  # Define function ----
  if(Aggregate == "mean") {
    Agg <- as.function(x = , mean)
  } else if(Aggregate == "sum") {
    Agg <- as.function(x = , sum)
  }

  # G1 & G2 & G3 ----
  if(!is.null(GroupVariables[1L]) && !is.na(GroupVariables[2L]) && !is.na(GroupVariables[3L])) {

    # G2 & G3 ----
    if(is.null(input[[G1Levels]]) && !is.null(input[[G2Levels]]) && !is.null(input[[G3Levels]])) {
      PlotDataForecastFinal <- PlotDataForecast[get(GroupVariables[2L]) %in% c(eval(input[[G2Levels]])) & get(GroupVariables[3L]) %in% c(eval(input[[G3Levels]]))]
      PlotDataForecastFinal <- PlotDataForecastFinal[, lapply(.SD, Agg), by = c(eval(GroupVariables), eval(DateVariable)), .SDcols = c(TargetVariable)]
      return(PlotDataForecastFinal)
    }

    # G1 & G3 ----
    if(!is.null(input[[G1Levels]]) && is.null(input[[G2Levels]]) && !is.null(input[[G3Levels]])) {
      PlotDataForecastFinal <- PlotDataForecast[get(GroupVariables[1L]) %in% c(eval(input[[G1Levels]])) & get(GroupVariables[3L]) %in% c(eval(input[[G3Levels]]))]
      PlotDataForecastFinal <- PlotDataForecastFinal[, lapply(.SD, Agg), by = c(eval(GroupVariables), eval(DateVariable)), .SDcols = c(TargetVariable)]
      return(PlotDataForecastFinal)
    }

    # G1 & G2 ----
    if(!is.null(input[[G1Levels]]) && !is.null(input[[G2Levels]]) && is.null(input[[G3Levels]])) {
      PlotDataForecastFinal <- PlotDataForecast[get(GroupVariables[1L]) %in% c(eval(input[[G1Levels]])) & get(GroupVariables[2L]) %in% c(eval(input[[G2Levels]]))]
      PlotDataForecastFinal <- PlotDataForecastFinal[, lapply(.SD, Agg), by = c(eval(GroupVariables), eval(DateVariable)), .SDcols = c(TargetVariable)]
      return(PlotDataForecastFinal)
    }

    # G3 ----
    if(is.null(input[[G1Levels]]) && is.null(input[[G2Levels]]) && !is.null(input[[G3Levels]])) {
      PlotDataForecastFinal <- PlotDataForecast[get(GroupVariables[3L]) %in% c(eval(input[[G3Levels]]))]
      PlotDataForecastFinal <- PlotDataForecastFinal[, lapply(.SD, Agg), by = c(eval(GroupVariables), eval(DateVariable)), .SDcols = c(TargetVariable)]
      return(PlotDataForecastFinal)
    }

    # G2 ----
    if(is.null(input[[G1Levels]]) && !is.null(input[[G2Levels]]) && is.null(input[[G3Levels]])) {
      PlotDataForecastFinal <- PlotDataForecast[get(GroupVariables[2L]) %in% c(eval(input[[G2Levels]]))]
      PlotDataForecastFinal <- PlotDataForecastFinal[, lapply(.SD, Agg), by = c(eval(GroupVariables), eval(DateVariable)), .SDcols = c(TargetVariable)]
      return(PlotDataForecastFinal)
    }

    # G1 ----
    if(!is.null(input[[G1Levels]]) && is.null(input[[G2Levels]]) && is.null(input[[G3Levels]])) {
      PlotDataForecastFinal <- PlotDataForecast[get(GroupVariables[1L]) %in% c(eval(input[[G1Levels]]))]
      PlotDataForecastFinal <- PlotDataForecastFinal[, lapply(.SD, Agg), by = c(eval(GroupVariables), eval(DateVariable)), .SDcols = c(TargetVariable)]
      return(PlotDataForecastFinal)
    }

    # None ----
    if(is.null(input[[G1Levels]]) && is.na(input[[G2Levels]]) && is.na(input[[G3Levels]])) {
      PlotDataForecastFinal <- PlotDataForecast[, lapply(.SD, Agg), by = c(eval(DateVariable), eval(GroupVariables)), .SDcols = c(TargetVariable)]
      return(PlotDataForecastFinal)
    }

    # G1 & G2 & G3 ----
    if(!is.null(input[[G1Levels]]) && !is.na(input[[G2Levels]]) && !is.na(input[[G3Levels]])) {
      return(PlotDataForecast[get(GroupVariables[1L]) %in% c(eval(input[[G1Levels]])) & get(GroupVariables[2L]) %in% c(eval(input[[G2Levels]])) & get(GroupVariables[3L]) %in% c(eval(input[[G3Levels]]))][, .SD, .SDcols = c(eval(TargetVariable), eval(DateVariable), eval(GroupVariables))])
    }
  }

  # G1 & G2 ----
  if(!is.null(GroupVariables[1L]) && !is.na(GroupVariables[2L]) && is.na(GroupVariables[3L])) {

    # G2 ----
    if(is.null(input[[G1Levels]]) && !is.null(input[[G2Levels]])) {
      PlotDataForecastFinal <- PlotDataForecast[get(GroupVariables[2L]) %in% c(eval(input[[G2Levels]]))]
      PlotDataForecastFinal <- PlotDataForecastFinal[, lapply(.SD, Agg), by = c(eval(GroupVariables), eval(DateVariable)), .SDcols = c(TargetVariable)]
      return(PlotDataForecastFinal)
    }

    # G1 ----
    if(!is.null(input[[G1Levels]]) && is.null(input[[G2Levels]])) {
      PlotDataForecastFinal <- PlotDataForecast[get(GroupVariables[1L]) %in% c(eval(input[[G1Levels]]))]
      PlotDataForecastFinal <- PlotDataForecastFinal[, lapply(.SD, Agg), by = c(eval(GroupVariables), eval(DateVariable)), .SDcols = c(TargetVariable)]
      return(PlotDataForecastFinal)
    }

    # G1 & G2 ----
    if(!is.null(input[[G1Levels]]) && !is.null(input[[G2Levels]])) {
      return(PlotDataForecast[get(GroupVariables[1L]) %in% c(eval(input[[G1Levels]])) & get(GroupVariables[2L]) %in% c(eval(input[[G2Levels]]))][, .SD, .SDcols = c(eval(TargetVariable), eval(GroupVariables), eval(DateVariable))])
    }

    # None ----
    if(is.null(input[[G1Levels]]) && is.null(input[[G2Levels]])) {
      PlotDataForecastFinal <- PlotDataForecast[, lapply(.SD, Agg), by = c(eval(DateVariable), eval(GroupVariables)), .SDcols = c(TargetVariable)]
      return(PlotDataForecastFinal)
    }
  }

  # G1 ----
  if(!is.null(GroupVariables[1L]) && is.na(GroupVariables[2L])) {

    # None ----
    if(is.null(input[[G1Levels]])) {
      PlotDataForecastFinal <- PlotDataForecast[, lapply(.SD, Agg), by = c(eval(GroupVariables), eval(DateVariable)), .SDcols = c(TargetVariable)]
      return(PlotDataForecastFinal)
    }

    # G1 ----
    if(!is.null(input[[G1Levels]])) {
      PlotDataForecastFinal <- PlotDataForecast[get(GroupVariables[1L]) %in% c(eval(input[[G1Levels]]))]
      PlotDataForecastFinal <- PlotDataForecastFinal[, lapply(.SD, Agg), by = c(eval(GroupVariables), eval(DateVariable)), .SDcols = c(TargetVariable)]
      return(PlotDataForecastFinal)
    }
  }

  # NO Grouping Variables ----
  if(is.null(GroupVariables)) {
    PlotDataForecastFinal <- PlotDataForecast[, .SD, .SDcols = c(eval(TargetVariable), eval(DateVariable))]
    PlotDataForecastFinal <- PlotDataForecastFinal[, lapply(.SD, Agg), by = eval(DateVariable), .SDcols = c(TargetVariable)]
    return(PlotDataForecastFinal)
  }

  # None up till now ----
  if(!exists("PlotDataForecastFinal")) {
    PlotDataForecastFinal <- PlotDataForecast[, .SD, .SDcols = c(eval(TargetVariable), eval(DateVariable), eval(GroupVariables))]
    PlotDataForecastFinal <- PlotDataForecastFinal[, lapply(.SD, Agg), by = c(eval(DateVariable), eval(GroupVariables)), .SDcols = c(TargetVariable)]
    return(PlotDataForecastFinal)
  }
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
