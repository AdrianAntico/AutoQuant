#' @noRd
ExpandText <- function(x) {
  return(paste0("'",paste0(x, collapse = "','"),"'"))
}

#' @param DataNames = names(data)
#' @param ModelDataNames names(ModelData)
#' @param PlotName = Plot1_react()
#'
#' @noRd
VarNamesDisplay <- function(DataNames=names(data), ModelDataNames=names(ModelData), PlotName=NULL) {
  if(any(c('Box','BoxPlot','ViolinPlot','Violin','Line','Bar','BarPlot','Scatter','Copula','CorrMatrix','Histogram','Hist') %in% PlotName)) {
    return(DataNames)
  } else {
    return(ModelDataNames)
  }
}


#' @title PlotLimits
#'
#' @param TotalPlots data.table
#'
#' @noRd
InitializePlotObjects <- function(TotalPlots) {

  # Define total number of possible plots
  TotalPlots <- 4L

  # Initalize PlotObjectLists
  for(po in seq_len(TotalPlots)) {
    if(po < 10L) {
      assign(x = paste0('PlotObjectList_0', po), value = list())
    } else {
      assign(x = paste0('PlotObjectList_', po), value = list())
    }
  }

  x <- list()
  for(num in seq_len(TotalPlots)) {

    # Fill out all slots ahead of time and update value blow when encountered, otherwise pass through what's in there
    PlotMetaData <- list(

      # MetaData:
      #  PlotID -> connect plot metadata to fixed plot button to drag and drop
      #  DataSource -> enable multiple data sets to be loaded
      #  PlotType -> reactive so that options below can adjust accordingly
      #  UpdateMethod -> not sure if needed but idea is to ensure that no action is taken that isn't needed, such as filtering
      #               -> Modify value of this as list gets updated via user selection
      'DataSource' = NULL,             # (listed for reference)
      'PlotType' = NULL,               # (listed for reference)
      'UpdateMethod' = 'All',

      # Data Usage:
      #   Sample Size -> would like to add sampling options or even allow for bootstrapping
      'SampleSize' = 100000L,
      'NumberGroupsDisplay' = 5L,

      # Plot extras
      'ShapAgg' = 'meanabs',
      'GamFitScatter' = FALSE,
      'NumberBins' = 30L,
      'Percentile_Buckets' = 20L,

      # Variables Selection (listed for reference)
      'YVars' = NULL,
      'YTicks' = NULL,
      'XVars' = NULL,
      'XTicks' = NULL,
      'CorVariables' = NULL,
      'PDP_Variable' = NULL,
      'ScoreVar' = NULL,
      'SizeVars' = NULL,
      'FacetVar1' = NULL,
      'FacetVar2' = NULL,
      'GroupVars' = NULL,
      'Levels1' = NULL,
      'Levels2' = NULL,
      'Levels3' = NULL,

      # Filter Variables, logic, and values (listed for reference)
      'FilterVar1' = NULL,
      'FilterVar2' = NULL,
      'FilterVar3' = NULL,
      'FilterVar4' = NULL,
      'FilterLogic1' = NULL,
      'FilterLogic2' = NULL,
      'FilterLogic3' = NULL,
      'FilterLogic4' = NULL,
      'FilterValue_1_1' = NULL,
      'FilterValue_1_2' = NULL,
      'FilterValue_1_3' = NULL,
      'FilterValue_1_4' = NULL,
      'FilterValue_2_1' = NULL,
      'FilterValue_2_2' = NULL,
      'FilterValue_2_3' = NULL,
      'FilterValue_2_4' = NULL,

      # Separate Button to Update These Inside DropDown Box
      'PlotWidth' = '950px',
      'PlotHeight' = '550px',
      'AngleY' = 0,
      'AngleX' = 90,
      'TextSize' = 15,
      'OutlierSize' = 0.01,
      'LegendPosition' = 'right',
      'LegendBorderSize' = 0.01,
      'LegendLineType' = 'solid',
      'TextColor' = 'darkblue',
      'ChartColor' = 'lightsteelblue1',
      'GridColor' = 'white',
      'BackGroundColor' = 'gray95',
      'BorderColor' = 'darkblue',
      'OutlierColor' = 'red',
      'FillColor' = 'gray25',
      'SubTitleColor' = 'darkblue')

    # Fill in master list
    for(meta in names(PlotMetaData)) {
      x[[paste0('Plot_', num)]][[meta]] <- PlotMetaData[[meta]]
    }
  }
  return(x)
}

#' @title PlotLimits
#'
#' @param p data.table
#' @param YMin Y Min Value
#' @param YMax Y Max Value
#' @param XMin X Min Value
#' @param XMax X Max Value
#' @param Debug = FALSE
#'
#' @export
PlotLimits <- function(p, YMin, YMax, XMin, XMax, Debug = FALSE) {

  if(Debug) {
    print(YMin)
    print(YMax)
    print(XMin)
    print(XMax)
    print(length(p))
  }

  if(missing(p) || length(p) == 0) return(NULL)
  if(Debug) {print('here 1'); print(missing(p))}
  if(missing(YMin)) YMin <- NA else if(length(YMin) == 0) YMin <- NA else if(YMin == "") YMin <- NA
  if(Debug) {print('here 2'); print(YMin)}
  if(missing(YMax)) YMax <- NA else if(length(YMax) == 0) YMax <- NA else if(YMax == "") YMax <- NA
  if(Debug) {print('here 3'); print(YMax)}
  if(missing(XMin)) XMin <- NA else if(length(XMin) == 0) XMin <- NA else if(XMin == "") XMin <- NA
  if(Debug) {print('here 4'); print(XMin)}
  if(missing(XMax)) XMax <- NA else if(length(XMax) == 0) XMax <- NA else if(XMax == "") XMax <- NA
  if(Debug) {
    print(is.na(YMin))
    print(is.na(YMax))
    print(is.na(XMin))
    print(is.na(XMax))
    print(!all(is.na(YMin), is.na(YMax), is.na(XMin), is.na(XMax)))
  }
  if(!all(is.na(YMin), is.na(YMax), is.na(XMin), is.na(XMax))) {
    p <- p + ggplot2::coord_cartesian(xlim = c(as.numeric(XMin), as.numeric(XMax)), ylim = c(as.numeric(YMin), as.numeric(YMax)))
    return(eval(p))
  } else {
    return(eval(p))
  }
}

# Creates two small text inputs for min and max
# div(style='display:inline-block', textInput3(inputId="xlimitsmin", label="x-min", value = 0.0, class="input-small")),
# div(style='display:inline-block', textInput3(inputId="xlimitsmax", label="x-max", value = 0.5, class="input-small")),
textInput2 <- function(inputId, label, value = "", ...) {
  tagList(
    tags$label(
      label, `for` = inputId),
    tags$input(
      id = inputId,
      type = "text",
      value = value,...))
}
textInput3<-function (inputId, label, value = "", ...) {
  div(
    style="display:inline-block",
    tags$label(
      label,
      `for` = inputId),
    tags$input(
      id = inputId,
      type = "text",
      value = value,
      ...))
}

#' @noRd
AvailableAppInsightsPlots <- function(x = 'bla', PlotNamesLookup=NULL, Debug = NULL) {
  if(Debug) {
    print('AvailableAppInsightsPlots Start')
    print('ModelOutputListNames below')
    print(x)
  }

  if(length(x) == 0) {
    if(Debug) print('aaip here 1')
    x <- NULL
  } else {
    if(Debug) print('aaip here 2')
    if(Debug) {print(PlotNamesLookup); print(names(PlotNamesLookup))}
    for(i in seq_along(x)) x[i] <- PlotNamesLookup[[x[i]]]
    x[length(x)+1L] <- 'ShapleyVarImp'
    if(Debug) print('aaip here 3')
    if(Debug) print(x)
  }
  StandardPlots <- c('Histogram','BoxPlot','ViolinPlot','Line','Bar','Scatter','Copula','CorrMatrix')
  if(Debug) print('StandardPlots below')
  if(Debug) print(StandardPlots)
  for(i in seq_along(StandardPlots)) {
    if(Debug) {
      print(StandardPlots[i])
      print(PlotNamesLookup[[StandardPlots[i]]])
    }
    StandardPlots[i] <- PlotNamesLookup[[StandardPlots[i]]]
  }
  if(Debug) print('Return output below')
  if(Debug) print(c(StandardPlots, x))
  return(c(StandardPlots, x))
}

#' @noRd
FL_Default <- function(data, x=input[['FilterVariable_1_1']]) {
  if(missing(data)) {
    print('FL_Default: data is missing')
    return('>=')
  }
  if(is.null(data)) {
    print('FL_Default: data is NULL')
    return('>=')
  }
  if(missing(x)) {
    print('FL_Default: x is missing')
    return('>=')
  }
  if(length(x) == 0) {
    print('FL_Default: x is NULL')
    return('>=')
  }
  if(x != 'None') {
    z <- class(data[[eval(x)]])
  } else {
    z <- 'Adrian'
  }
  if(any(z %in% c('factor', 'character'))) return('%chin%') else return('>=')
}

#' @noRd
LevelValues <- function(x) {
  if(missing(x)) {
    print('LevelValues x is missing')
    return(NULL)
  } else if(!exists('x')) {
    print('LevelValues x does not exist')
    return(NULL)
  } else if(length(x) == 0) {
    print('LevelValues x has length 0')
    return(NULL)
  } else if(!'None' %in% x && length(x) >= 1L) {
    return(x)
  } else if('None' %in% x && length(x) >= 1L) {
    return(x[!x %in% 'None'])
  } else {
    return(NULL)
  }
}

#' @noRd
PDPVar <- function(ModelOutputList) {
  if(!is.null(ModelOutputList)) {
    x <- names(ModelOutputList$PlotList$Test_ParDepPlots)
    y <- x[1L]
  } else {
    x <- NULL
    y <- NULL
  }
  return(list(Names = x, Default = y))
}

#' @noRd
YTicks <- function(data, yvar = 'None') {
  if(!any(yvar %in% 'None') && length(yvar) == 1L) {
    Uniques <- tryCatch({data[, unique(get(yvar))]}, error = function(x) NULL)
    if(!is.null(Uniques) && all(Uniques != 'Default')) {
      if(any(class(data[[eval(yvar)]]) %in% c('numeric','integer')) && Uniques > 10L) {
        x <- c('Default', 'percentiles', '5th-tiles', 'Deciles', 'Quantiles', 'Quartiles', as.character(data[, quantile(round(get(yvar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]))
      } else {
        x <- c('Default', Uniques)
      }
    } else {
      x <- 'Default'
    }
  } else {
    x <- 'Default'
  }
  return(x)
}

#' @noRd
XTicks <- function(data, xvar='None',datevar='None') {
  if(!any(xvar %in% 'None') && length(xvar) == 1L) {
    Uniques <- tryCatch({data[, unique(get(xvar))]}, error = function(x) NULL)
    x <- class(data[[eval(xvar)]])[1L]
    if(any(x %chin% c('numeric','integer')) && length(Uniques) > 10L) {
      choices <- c('Default', 'Percentiles', 'Every 5th percentile', 'Deciles', 'Quantiles', 'Quartiles', as.character(data[, quantile(round(get(xvar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]))
    } else if(any(x %chin% c('Date'))) {
      choices <- c('Default', '1 year', '1 day', '1 week', '1 month', '3 day', '2 week', '3 month', '6 month', '2 year', '5 year', '10 year')
    } else if(any(x %like% c('POSIX'))) {
      choices <- c('Default', '1 year', '1 day', '3 day', '1 week', '2 week', '1 month', '3 month', '6 month', '2 year', '5 year', '10 year', '1 minute', '15 minutes', '30 minutes', '1 hour', '3 hour', '6 hour', '12 hour')
    } else if(any(x %like% c('character','numeric','integer'))) {
      choices <- c('Default', Uniques)
    } else {
      choices <- c('Default', Uniques)
    }
  } else if(datevar != 'None') {
    Uniques <- tryCatch({data[, unique(get(datevar))]}, error = function(x) NULL)
    x <- class(data[[eval(datevar)]])[1L]
    if(any(x %chin% c('Date'))) {
      choices <- c('Default', '1 year', '1 day', '1 week', '1 month', '3 day', '2 week', '3 month', '6 month', '2 year', '5 year', '10 year')
    } else if(any(x %like% c('POSIX'))) {
      choices <- c('Default', '1 year', '1 day', '3 day', '1 week', '2 week', '1 month', '3 month', '6 month', '2 year', '5 year', '10 year', '1 minute', '15 minutes', '30 minutes', '1 hour', '3 hour', '6 hour', '12 hour')
    } else {
      choices <- c('Default', Uniques)
    }
  } else {
    choices <- 'Default'
  }
  return(choices)
}

#' @importFrom rstudioapi isAvailable getSourceEditorContext
GetData <- function(data = NULL, name = NULL, Debug=FALSE) {
  if(!is.null(data)) {
    if(Debug) print('GetData: !is.null(data) was TRUE')
    if(is.character(data)) {
      if(Debug) print('GetData: is.character(data) was TRUE')
      data <- try({
        dat <- get(x = data, envir = globalenv())
        if(inherits(dat, what = 'data.table')) {
          if(Debug) print('GetData returned dat from here 1')
          dat
        } else if(inherits(dat, what = 'data.frame')) {
          if(Debug) print('GetData returned dat from here 2')
          data.table::as.data.table(dat)
        } else {
          if(Debug) print('GetData returned dat from here 3')
          tryCatch({data.table::as.data.table(dat)}, error = NULL)
        }
      }, silent = TRUE)
      data_name <- data
      if("try-error" %in% class(data)) {
        warning(paste0("'", data, "' not found"), call. = FALSE)
        data <- NULL
        data_name <- ""
      }
    } else if(inherits(x = data, what = 'data.table')) {
      data <- try({
        if(inherits(data, what = 'data.table')) {
          data
        } else {
          data.table::as.data.table(data)
        }
      }, silent = TRUE)
      if("try-error" %in% class(data)) {
        warning(paste0("'", data, "' not found"), call. = FALSE)
        data <- NULL
        data_name <- ""
      } else {
        if(!is.null(name)) {
          data_name <- as.character(name)
        } else {
          data_name <- deparse(substitute(data))
        }
      }
    } else {
      data <- NULL
      data_name <- ""
    }
  } else {
    if(rstudioapi::isAvailable()) {
      context <- try(rstudioapi::getSourceEditorContext(), silent = TRUE)
      if("try-error" %in% class(context) || is.null(context)) {
        data <- NULL
        data_name <- ""
      } else {
        context_select <- context$selection[[1]]$text
        if(isTRUE(nzchar(context_select))) {
          data <- try(data.table::as.data.table(get(x = context_select, envir = globalenv())), silent = TRUE)
          data_name <- context_select
          if("try-error" %in% class(data)) {
            warning(paste0("Failed to retrieve data from the selection"), call. = FALSE)
            data <- NULL
            data_name <- ""
          }
        } else {
          data <- NULL
          data_name <- ""
        }
      }
    } else {
      data <- NULL
      data_name <- ""
    }
  }
  list(data = data, data_name = data_name)
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
CEP <- function(x) if(missing(x)) 'NULL' else if(!exists('x')) 'NULL' else if(is.null(x)) "NULL" else if(identical(x, character(0))) "NULL" else if(identical(x, numeric(0))) "NULL" else if(identical(x, integer(0))) "NULL" else if(identical(x, logical(0))) "NULL" else if(x == "") "NULL" else if(is.na(x)) "NULL" else if(x == 'None') "NULL" else if(is.numeric(x)) x else if(length(x) > 1) paste0("c(", noquote(paste0("'", x, "'", collapse = ',')), ")") else paste0("'", x, "'")

#' @noRd
CEPP <- function(x, Default = NULL, Type = 'character') if(missing(x)) Default else if(!exists('x')) Default else if(length(x) == 0) Default else if(is.na(x)) Default else if(x == "") Default else if(Type == 'numeric') RemixAutoML:::NumNull(x) else if(Type == 'character') RemixAutoML:::CharNull(x)

#' @title UniqueLevels
#'
#' @param input passthrough
#' @param data data.table
#' @param GroupVars passthrough
#'
#' @export
UniqueLevels <- function(input, data, n, GroupVars=NULL) {
  if(missing(n) || missing(GroupVars) && is.null(GroupVars[[n]]) || is.na(GroupVars[[n]])) {
    return(NULL)
  } else {
    if(any(class(data[[GroupVars[[n]]]]) %in% c('factor'))) {
      return(tryCatch({c(sort(as.character(unique(data[[eval(GroupVars[[n]])]]))))}, error = function(x)  NULL))
    } else {
      return(tryCatch({c(sort(unique(data[[eval(GroupVars[[n]])]])))}, error = function(x)  NULL))
    }
  }
}

#' @title FilterValues
#'
#' @param data data.table
#' @param VarName Variable name
#' @param type 1 for min, 2 for max
#'
#' @export
FilterValues <- function(data, VarName = NULL, type = 1) {

  if(missing(data)) {
    print('FilterValues(): data was missing')
    x <- NULL
  } else if(is.null(data)) {
    print('FilterValues(): data was NULL')
    x <- NULL
  } else if(length(VarName) == 0) {
    print('FilterValues(): VarName was length 0')
    x <- NULL
  }
  gg <- tolower(class(data[[eval(VarName)]]))
  if(tolower(VarName) != 'none') {
    if(any(gg %chin% c('numeric', 'integer'))) {
      if(type == 1) {
        x <- sort(decreasing = FALSE, unique(as.numeric(data[, quantile(get(VarName), probs = c(seq(0, 1, 0.05)), na.rm = TRUE)])))
      } else {
        x <- sort(decreasing = TRUE, unique(as.numeric(data[, quantile(get(VarName), probs = c(seq(0, 1, 0.05)), na.rm = TRUE)])))
      }
    } else if(any(gg %chin% c('factor','character','date','idate','posixct'))) {
      if(type == 1) {
        if(lubridate::is.Date(data[[eval(VarName)]])) {
          x <- as.Date(sort(decreasing = FALSE, data[, unique(get(VarName))]))
        } else {
          x <- sort(decreasing = FALSE, data[, unique(get(VarName))])
        }
      } else {
        if(lubridate::is.Date(data[[eval(VarName)]])) {
          x <- as.Date(sort(decreasing = TRUE, data[, unique(get(VarName))]))
        } else {
          x <- sort(decreasing = TRUE, data[, unique(get(VarName))])
        }
      }
    } else {
      x <- NULL
    }
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

  if(Debug) {
    print(data1)
    print(FilterLogic)
    print(FilterVariable)
    print(FilterValue)
    print(FilterValue2)
  }

  if(missing(data1)) {
    return(NULL)
  }

  if(length(FilterVariable) != 0 && FilterVariable %in% names(data1)) {
    if(any(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('factor', 'character')) || FilterLogic %in% c('%in%', '%chin%', '%like')) {
      if(Debug) print('FilterLogicData else if 1')
      if(Debug) print(any(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('factor', 'character')))
      if(FilterLogic %in% c('%in%','%chin%')) {
        data1 <- data1[get(FilterVariable) %chin% c(eval(FilterValue))]
      } else if(FilterLogic == '%like%') {
        data1 <- data1[get(eval(FilterVariable)) %like% c(eval(FilterValue))]
      }
    } else if(any(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('numeric', 'integer', 'date', 'posix'))) {
      if(Debug) print('FilterLogicData else if 2')
      if(Debug) print(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('numeric', 'integer', 'date', 'posix'))
      if(FilterLogic == '>') {
        data1 <- data1[get(FilterVariable) > eval(FilterValue)]
      } else if(FilterLogic == '>=') {
        data1 <- data1[get(FilterVariable) >= eval(FilterValue)]
      } else if(FilterLogic == '<') {
        data1 <- data1[get(FilterVariable) < eval(FilterValue2)]
      } else if(FilterLogic == '%between%') {
        if(Debug) print('At %between% section')
        if(Debug) print(as.numeric(FilterVariable))
        if(Debug) print(as.numeric(FilterValue))
        if(Debug) print(as.numeric(FilterValue2))
        if(Debug) print(data1)
        if(Debug) print('Run data.table operation')
        data1 <- data1[get(FilterVariable) >= eval(FilterValue) & get(FilterVariable) <= eval(FilterValue2)]
        if(Debug) print('Done with data.table operation')
        if(Debug) print(data1)
      } else if(FilterLogic == 'not %between%') {
        data1 <- data1[get(FilterVariable) < eval(FilterValue) | get(FilterVariable) > eval(FilterValue2)]
      } else {
        data1 <- data1[get(FilterVariable) <= eval(FilterValue2)]
      }
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

  # Return of data is missing altogether
  if(missing(data)) {
    print('KeyVarsInit: data is missing')
    return(list(MinVal = NULL, MaxVal = NULL, ChoiceInput = NULL))
  }

  # Run VarName through function to NULL it out if any issues
  VarName <- RemixAutoML:::CEPP(VarName, Default = NULL)

  # Return if VarName is NULL or if some sort of character(0) slipped through the cracks
  if(length(VarName) == 0) {
    print('KeyVarsInit: VarName is length 0')
    return(list(MinVal = NULL, MaxVal = NULL, ChoiceInput = NULL))
  }

  # If data doesnt have VarName in it, the next check will be ModelData
  if(!VarName %in% names(data)) {
    minn <- NULL
    maxx <- NULL
    choices <- NULL
    return(list(MinVal = minn, MaxVal = maxx, ChoiceInput = choices))
  }

  # If VarName is set to 'None' then just return NULLs for list values
  if(tolower(VarName) == 'none') {
    minn <- NULL
    maxx <- NULL
    choices <- NULL
    return(list(MinVal = minn, MaxVal = maxx, ChoiceInput = choices))
  }

  # If VarName is
  if(any(class(data[[eval(VarName)]]) %chin% c('numeric','integer','double','float'))) {
    minn <- tryCatch({floor(data[, min(get(VarName), na.rm = TRUE)])}, error = function(x) NULL)
    maxx <- tryCatch({ceiling(data[, max(get(VarName), na.rm = TRUE)])}, error = function(x) NULL)
    UData <- tryCatch({sort(data[, unique(get(VarName))])}, error = function(x) NULL)
    if(!is.null(UData) && length(UData) <= 10L) {
      choices <- UData
    } else {
      choices <- tryCatch({unique(sort(round(as.numeric(data[, quantile(get(VarName), probs = c(seq(0, 1, 0.05)), na.rm = TRUE)]), 5L)))}, error = function(x) {
        tryCatch({UData}, error = NULL)
      })
    }
  } else if(any(tolower(class(data[[(eval(VarName))]])) %chin% c('date','idate','date','posixct','posixt','character','factor'))) {
    choices <- tryCatch({sort(unique(data[[eval(VarName)]]))}, error = function(x) NULL)
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
  if(missing(data)) {
    print('GetFilterValueLabel(): data was missing')
    x <- 'N/A'
  } else if(is.null(data)) {
    print('GetFilterValueLabel(): data was NULL')
    x <- 'N/A'
  } else if(length(VarName) == 0) {
    print('GetFilterValueLabel(): VarName was length 0')
    x <- 'N/A'
  } else if(tolower(VarName) != 'none') {
    if(any(tolower(class(data[[eval(VarName)]])) %chin% c('numeric', 'integer', 'float', 'double', 'date', 'idate', 'posixct'))) {
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
  if(missing(data)) {
    print('GetFilterValueMultiple(): data was missing')
    x <- FALSE
  } else if(is.null(data)) {
    print('GetFilterValueMultiple(): data was NULL')
    x <- FALSE
  } else if(length(VarName) == 0) {
    print('GetFilterValueMultiple(): VarName was length 0')
    x <- FALSE
  } else if(tolower(VarName) != 'none') {
    if(any(tolower(class(data[[eval(VarName)]])) %in% c('character', 'factor', 'date', 'idate', 'posixct'))) {
      x <- TRUE
    } else {
      x <- FALSE
    }
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
CharNull <- function(x, Char = FALSE) {

  if(missing(x)) {
    print('CharNull: missing x')
    return(NULL)
  }

  if(!exists('x')) {
    print('CharNull: x does not exist')
    return(NULL)
  }

  if(length(x) == 0) {
    print('CharNull: length(x) == 0')
    return(NULL)
  }

  if(all(is.na(suppressWarnings(as.character(x))))) {

    return(NULL)

  } else if(any(is.na(suppressWarnings(as.character(x)))) && length(x) > 1) {

    x <- x[!is.na(x)]
    x <- suppressWarnings(as.character(x))
    return(x)

  } else if(any(is.na(suppressWarnings(as.character(x)))) && length(x) == 1) {

    return(NULL)

  } else {

    x <- suppressWarnings(as.character(x))
    return(x)

  }

  if(!Char) {
    return(NULL)
  } else {
    return("NULL")
  }
}

#' @title NumNull
#'
#' @param x value
#'
#' @export
NumNull <- function(x, Char = FALSE) {

  if(missing(x)) {
    print('NumNull: missing x')
    return(NULL)
  }

  if(!exists('x')) {
    print('NumNull: x does not exist')
    return(NULL)
  }

  if(length(x) == 0) {
    print('NumNull: length(x) == 0')
  }

  if(all(is.na(suppressWarnings(as.numeric(x))))) {

    return(NULL)

  } else if(any(is.na(suppressWarnings(as.numeric(x)))) && length(x) > 1) {

    x <- x[!is.na(x)]
    x <- suppressWarnings(as.numeric(x))
    return(x)

  } else if(any(is.na(suppressWarnings(as.numeric(x)))) && length(x) == 1) {

    return(NULL)

  } else {

    x <- suppressWarnings(as.numeric(x))
    return(x)

  }

  if(!Char) {
    return(NULL)
  } else {
    return("NULL")
  }
}

#' @title IntNull
#'
#' @param x value
#'
#' @export
IntNull <- function(x, Char = FALSE) {

  if(missing(x)) {
    print('IntNull: missing x')
    return(NULL)
  }

  if(!exists('x')) {
    print('IntNull: x does not exist')
    return(NULL)
  }

  if(length(x) == 0) {
    print('IntNull: length(x) == 0')
  }

  if(all(is.na(suppressWarnings(as.integer(x))))) {

    return(NULL)

  } else if(any(is.na(suppressWarnings(as.integer(x)))) && length(x) > 1) {

    x <- x[!is.na(x)]
    x <- suppressWarnings(as.integer(x))
    return(x)

  } else if(any(is.na(suppressWarnings(as.integer(x)))) && length(x) == 1) {

    return(NULL)

  } else {

    x <- suppressWarnings(as.integer(x))
    return(x)

  }

  if(!Char) {
    return(NULL)
  } else {
    return("NULL")
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
#' @param Debug FALSE
#'
#' @export
ReactiveLoadCSV <- function(Infile = input[[eval(InputVal)]], ProjectList = NULL, DateUpdateName = NULL, RemoveObjects = NULL, Debug = FALSE) {
  if(Debug) print('ReactiveLoadCSV 1')
  if(is.null(Infile)) return(NULL)
  if(Debug) print('ReactiveLoadCSV 2')
  if(!is.null(ProjectList)) ProjectList[[eval(DateUpdateName)]] <<- Sys.Date()
  if(Debug) print('ReactiveLoadCSV 3')
  if(!is.null(RemoveObjects)) for(i in seq_along(RemoveObjects)) if(exists(RemoveObjects[i])) rm(RemoveObjects[i])
  if(Debug) print('ReactiveLoadCSV 4')
  x <- data.table::fread(file = Infile$datapath)
  if(Debug) print('ReactiveLoadCSV 5')
  g <- RemixAutoML:::ColTypes(x)
  if(Debug) {print('ReactiveLoadCSV 6'); print(x); print(g); print(any('IDate' %in% g))}
  if(any('IDate' %in% g)) {
    if(Debug) print('ReactiveLoadCSV 7')
    for(zz in seq_along(x)) {
      if(Debug) print(paste0('ReactiveLoadCSV ', zz))
      if(any(class(x[[names(x)[zz]]]) == 'IDate')) {
        if(Debug) print(class(x[[names(x)[zz]]]))
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
    tryCatch({if(any(class(input[[VarName]]) != "NULL")) {
      ProjectList[[VarName]] <<- as.character(input[[VarName]])
    } else {
      ProjectList[[VarName]] <<- Default
    }}, error = function(x) Default)

  } else if(Type == "numeric") {
    tryCatch({if(any(class(input[[VarName]]) != "NULL")) {
      ProjectList[[VarName]] <<- as.numeric(input[[VarName]])
    } else {
      ProjectList[[VarName]] <<- Default
    }}, error = function(x) Default)

  } else if(Type == "logical") {
    tryCatch({if(any(class(input[[VarName]]) != "NULL")) {
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
#' @param xx This is the input value within a Shiny context; input$value or better yet, tryCatch({input$value}, error = function(x) NULL)
#' @param VarName The name of the VarNameument you want to store
#' @param Type 'character' 'numeric' 'logical' 'date', or 'infer'
#' @param Default default value
#' @param Switch = FALSE
#' @param Debug = FALSE
#'
#' @examples
#' \dontrun{
#' Aggregate <- RemixAutoML::ReturnParam(input, VarName = "TS_AggregateFunction", Type = "character", Default = "mean")
#' }
#'
#' @return Updates ProjectList inside function
#' @export
ReturnParam <- function(xx = NULL,
                        VarName = NULL,
                        Type = 'numeric',
                        Default = 1,
                        Switch = TRUE,
                        Debug = FALSE) {

  # Return if null or length(0) (can't have NULL elements or )
  if(length(xx) == 0) {
    if(Debug) print('ReturnParam: length(xx) == 0 -> TRUE')
    return(Default)
  }

  # NA's
  if(all(is.na(xx))) {
    if(Debug) print('ReturnParam: all(is.na(xx)) -> TRUE')
    return(Default)
  }
  if(any(is.na(xx))) {
    if(Debug) print('ReturnParam: any(is.na(xx)) -> TRUE')
    xx <- xx[!is.na(xx)]
    if(Debug) {print(xx); print('ReturnParam: any(is.na(xx)) -> TRUE END')}
  }

  # ""
  if(all(xx %in% "")) {
    if(Debug) {print("all(xx %in% '')")}
    return(Default)
  }
  if(any(xx %in% "")) {
    if(Debug) print("any(xx %in% '') -> TRUE")
    xx <- xx[!xx %in% ""]
    if(Debug) {print(xx); print("any(xx %in% '') -> TRUE END")}
  }

  # 'No Data Loaded !!'
  if(all(xx %in% 'No Data Loaded !!')) {
    if(Debug) print("all(xx %in% 'No Data Loaded !!') -> TRUE")
    return(Default)
  }
  if(any(xx %in% 'No Data Loaded !!')) {
    if(Debug) print("any(xx %in% 'No Data Loaded !!') -> TRUE")
    xx <- xx[!xx %in% 'No Data Loaded !!']
    if(Debug) {print(xx); print("any(xx %in% 'No Data Loaded !!') -> TRUE END")}
  }

  # Type == numeric
  if(Type == "numeric") {
    if(Switch) {
      if(!all(xx %in% c('None', 'Default'))) {
        if(any(xx %in% c('None', 'Default'))) xx <- xx[!xx %in% c('None','Default')]
        return(as.numeric(xx))
      } else if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(!all(xx %in% c('None', 'Default'))) {
          if(any(xx %in% c('None', 'Default'))) xx <- xx[!xx %in% c('None','Default')]
          return(as.numeric(xx))
        } else {
          return(Default)
        }
      } else {
        return(Default)
      }
    } else {
      if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(!all(xx %in% c('None', 'Default'))) {
          return(as.numeric(xx))
        } else {
          return(Default)
        }
      } else if(!all(xx %in% c('None', 'Default'))) {
        return(as.numeric(xx))
      } else {
        return(Default)
      }
    }
  }

  # Type == logical
  if(Type == "logical") {
    if(Switch) {
      if(!all(xx %in% c('None', 'Default'))) {
        if(any(xx %in% c('None', 'Default'))) xx <- xx[!xx %in% c('None','Default')]
        return(as.logical(xx))
      } else if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(!all(xx %in% c('None', 'Default'))) {
          if(any(xx %in% c('None', 'Default'))) xx <- xx[!xx %in% c('None','Default')]
          return(as.logical(xx))
        } else {
          return(Default)
        }
      } else {
        return(Default)
      }
    } else {
      if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(!all(xx %in% c('None', 'Default'))) {
          return(as.logical(xx))
        } else {
          return(Default)
        }
      } else if(!all(xx %in% c('None', 'Default'))) {
        return(as.logical(xx))
      } else {
        return(Default)
      }
    }
  }

  # Type == character
  if(Type == "character") {
    if(Switch) {
      if(!all(xx %in% c('None', 'Default'))) {
        if(any(xx %in% c('None', 'Default'))) xx <- xx[!xx %in% c('None','Default')]
        return(as.character(xx))
      } else if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(!all(xx %in% c('None', 'Default'))) {
          if(any(xx %in% c('None', 'Default'))) xx <- xx[!xx %in% c('None','Default')]
          return(as.character(xx))
        } else {
          return(Default)
        }
      } else {
        return(Default)
      }
    } else {
      if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(!all(xx %in% c('None', 'Default'))) {
          return(as.character(xx))
        } else {
          return(Default)
        }
      } else if(!all(xx %in% c('None', 'Default'))) {
        return(as.character(xx))
      } else {
        return(Default)
      }
    }
  }

  # Type == date
  if(Type == "date") {
    if(Switch) {
      if(!all(xx %in% c('None', 'Default'))) {
        if(any(xx %in% c('None', 'Default'))) xx <- xx[!xx %in% c('None','Default')]
        return(as.character(xx))
      } else if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(any(class(xx) != "NULL") && !all(xx %in% c('None', 'Default'))) {
          if(any(xx %in% c('None', 'Default'))) xx <- xx[!xx %in% c('None','Default')]
          return(xx)
        } else {
          return(Default)
        }
      } else {
        return(Default)
      }
    } else {
      if(exists("ProjectList")) {
        if(!is.null(ProjectList[[VarName]])) {
          return(ProjectList[[VarName]])
        } else if(any(class(xx) != "NULL") && !all(xx %in% c('None', 'Default'))) {
          return(xx)
        } else {
          return(Default)
        }
      } else if(any(class(xx) != "NULL") && !all(xx %in% c('None', 'Default'))) {
        return(xx)
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
        } else if(any(class(Input[[InputID]]) != "NULL")) {
          return(as.numeric(Input[[InputID]]))
        } else {
          return(Default)
        }
      } else if(any(class(Input[[InputID]]) != "NULL")) {
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
      } else if(any(class(Input[[InputID]]) != "NULL")) {
        return(as.character(Input[[InputID]]))
      } else {
        return(Default)
      }
    } else if(any(class(Input[[InputID]]) != "NULL")) {
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
      } else if(any(class(Input[[InputID]]) != "NULL")) {
        return(as.logical(Input[[InputID]]))
      } else {
        return(Default)
      }
    } else if(any(class(Input[[InputID]]) != "NULL")) {
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
  return(
    if(exists("ProjectList")) {
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

#' @title SelectizeInput
#'
#' @description SelectizeInput automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID Feeds ProjectList and inputId. Argument saved in ProjectList
#' @param Label Feeds label
#' @param Choices Feeds choices
#' @param SelectedDefault Feeds selected for cases where ProjectList has a null element
#' @param Multiple Feeds multiple for enabling selecting more than one element from list
#' @param MaxVars = NULL
#' @param CloseAfterSelect = FALSE,
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   RemixAutoML::SelectizeInput(InputID = "TS_CARMA_HolidayMovingAverages", Label = "Select Holiday Count MA's", Choices = as.character(0:50),
#'                            SelectedDefault = as.character(c(1,2)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
#' }
#' @return SelectizeInput object for server.R to go into renderUI({SelectizeInput()})
#' @export
SelectizeInput <- function(InputID = "",
                           Label = "",
                           Choices = NULL,
                           SelectedDefault = NULL,
                           Multiple = TRUE,
                           MaxVars = 5000,
                           CloseAfterSelect = FALSE,
                           Debug = FALSE) {
  Options <- list()
  Options[['allowEmptyOption']] <- TRUE
  Options[['maxItems']] <- MaxVars
  Options[['maxOptions']] <- MaxVars
  Options[['closeAfterSelect']] <- CloseAfterSelect

  return(
    if(exists("ProjectList")) {
      tryCatch({
        if(!is.null(ProjectList[[InputID]])) {
          shiny::selectizeInput(inputId = InputID, label = Label, choices = c(unique(c("", Choices))), selected = ProjectList[[InputID]], options = Options, multiple = Multiple)
        } else {
          shiny::selectizeInput(inputId = InputID, label = Label, choices = c(unique(c("", Choices))), selected = SelectedDefault, options = Options, multiple = Multiple)
        }}, error = function(x) shiny::selectizeInput(inputId = InputID, label = Label, choices = c(unique(c("", Choices))), selected = SelectedDefault, options = Options, multiple = Multiple))
    } else {
      if(Debug) {
        print(InputID)
        print(Label)
        print(c(unique(c("", Choices))))
        print(SelectedDefault)
        print(Options)
        print(Multiple)
      }
      tryCatch({
        shiny::selectizeInput(inputId = InputID, label = Label, choices = c(unique(c("", Choices))), selected = SelectedDefault, options = Options, multiple = Multiple)},
        error = function(x) {
          shiny::selectizeInput(inputId = InputID, label = Label, choices = "No Data Loaded !!", selected = "No Data Loaded !!", options = Options, multiple = Multiple)
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
#' @param DataExist Logical
#' @param NumGroupVar Which group var to select
#' @param InputID Feeds ProjectList and inputId. Argument saved in ProjectList
#' @param InputID2 Values from input2. In first version the input is referenced inside function
#' @param Choices Feeds choices
#' @param SelectedDefault Feeds selected for cases where ProjectList has a null element
#' @param Multiple Feeds multiple for enabling selecting more than one element from list
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   RemixAutoML::PickerInput_GetLevels2(
#'     InputID = "TS_CARMA_HolidayMovingAverages", Label = "Select Holiday Count MA's", Choices = as.character(0:50),
#'     SelectedDefault = as.character(c(1,2)), Multiple = TRUE)})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
PickerInput_GetLevels2 <- function(DataExist = TRUE,
                                   NumGroupVar = 3,
                                   InputID = "TS_CARMA_HolidayMovingAverages",
                                   InputID2 = "timeSeriesGroupVars",
                                   Choices = as.character(0:50),
                                   SelectedDefault = as.character(c(1,2)),
                                   Multiple = TRUE) {
  return(

    if(DataExist) {

      if(!is.null(InputID2)) {

        if(length(InputID2) >= NumGroupVar && !'None' %in% InputID2) {

          shinyWidgets::pickerInput(
            inputId = InputID, label = tags$span(style='color: blue;', paste0(InputID2[[NumGroupVar]]," Levels")),
            choices = Choices, selected = SelectedDefault,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 1"), multiple = Multiple, width = "100%")

        } else {

          shinyWidgets::pickerInput(inputId = InputID, label = tags$span(style='color: blue;', "< N/A >"), choices = SelectedDefault, selected = SelectedDefault, multiple = Multiple, width = "100%")

        }

      } else {

        shinyWidgets::pickerInput(inputId = InputID, label = tags$span(style='color: blue;', "< N/A >"), choices = SelectedDefault, selected = SelectedDefault, multiple = Multiple, width = "100%")

      }

    } else {

      shinyWidgets::pickerInput(inputId = InputID, label = tags$span(style='color: blue;', "< N/A >"), choices = SelectedDefault, selected = SelectedDefault, multiple = Multiple, width = "100%")

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
  if(!is.null(GroupVariables[1L]) && !is.na(GroupVariables[1L]) && !is.null(GroupVariables[2L]) && !is.na(GroupVariables[2L]) && !is.null(GroupVariables[3L]) && !is.na(GroupVariables[3L])) {

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
  if(!is.null(GroupVariables[1L]) && !is.na(GroupVariables[1L]) && !is.null(GroupVariables[2L]) && !is.na(GroupVariables[2L]) && (is.null(GroupVariables[3L]) || is.na(GroupVariables[3L]))) {

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
  if(!is.null(GroupVariables[1L]) && !is.na(GroupVariables[1L]) && (is.null(GroupVariables[2L]) || (!is.null(GroupVariables[2L]) && is.na(GroupVariables[2L])))) {

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
  if(Debug) {print('No Grouping Variables'); print(GroupVariables); print(DateVariable); print(SubsetOnly)}
  if(is.null(GroupVariables) || (!is.null(GroupVariables) && is.na(GroupVariables)) || !exists('DateVariable')) {
    if(Debug) print('No Goruping Variables ----')
    if(!SubsetOnly && length(DateVariable) != 0) {
      x <- data[, .SD, .SDcols = c(eval(TargetVariable), eval(DateVariable))]
      x <- AggFun(dt = x, A = Agg, S = SubsetOnly)
    } else {
      x <- data
    }
    return(x)
  }

  # None up till now ----
  if(Debug) print('PreparePlotData final check')
  if(!exists('x')) {
    x <- AggFun(dt = data, A = Agg, S = SubsetOnly)
    return(x)
  }

  if(Debug) print('PreparePlotData return')

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
