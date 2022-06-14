#' @title ExpandText
#'
#' @description This function is for pasting character vector arguments into their respective parameter slots for code printing (and command line vector argument passing)
#'
#'
#' @keywords internal
ExpandText <- function(x) {
  if(length(x) > 0L) {
    if(is.character(x) || is.factor(x) || lubridate::is.Date(x) || lubridate::is.POSIXct(x)) {
      return(paste0("c('", paste0(x, collapse = "','"), "')"))
    } else if(is.numeric(x) || is.logical(x)) {
      return(paste0("c(", paste0(x, collapse = ","), ")"))
    }
  } else {
    return('NULL')
  }
}

#' @noRd
#.underscore_removal <- function(x) {
Shiny.Utils.underscore_removal <- function(x) {
  for(i in 7L:ncol(x)) {
    nam <- names(x)[i]
    if(grep(pattern = '_', x = nam)) {
      data.table::setnames(x, old = nam, gsub(pattern = '_', replacement = ' ', x = nam))
    }
  }
  return(x)
}

#' @title ChoicesByType
#'
#' @param data data.table
#'
#' @return a list of columns names by data type
#'
#' @keywords internal
ChoicesByType <- function(data) {
  categorical_cols <- RemixAutoML:::CEPP(names(data)[unique(c(which(unlist(lapply(data, is.character))), which(unlist(lapply(data, is.factor)))))])
  date_cols <- RemixAutoML:::CEPP(names(data)[unique(c(which(unlist(lapply(data, lubridate::is.Date))), which(unlist(lapply(data, lubridate::is.POSIXct)))))])
  numeric_cols <- RemixAutoML:::CEPP(names(data)[which(unlist(lapply(data, is.numeric)))])
  logical_cols <- RemixAutoML:::CEPP(names(data)[which(unlist(lapply(data, is.logical)))])
  x <- c()
  if(length(date_cols) > 0L) x['Date'] <- date_cols
  if(length(categorical_cols) > 0L) x <- c(x, categorical_cols)
  if(length(numeric_cols) > 0L) x['Numeric'] <- numeric_cols
  if(length(logical_cols) > 0L) x['Logical'] <- logical_cols
  return(unlist(x))
}

#' @title IntraSessionDefaults
#'
#' @param List named list of args where the args can be many
#' @param InputName 'Plot1_SelectData' in input$Plot1_SelectData
#' @param ArgName 'SelectedDefault' in List[[InputName]][[Default]]
#' @param Default default value to assign
#' @param Debug Logical
#'
#' @keywords internal
IntraSessionDefaults <- function(List = NULL,
                                 InputName = 'Plot1_SelectData',
                                 ArgName = 'SelectedDefault',
                                 Default = names(DataList)[[1L]],
                                 Debug = Debug) {

  # Check args
  if(Debug) print(paste0('IntraSessionDefaults: check args for :: ', InputName))
  if(length(List) == 0L) stop(paste0('IntraSessionDefaults: ', InputName, ': length(List) == 0'))
  if(length(InputName) == 0L) stop(paste0('IntraSessionDefaults: ', InputName, ': length(InputName) == 0'))
  if(length(ArgName) == 0L) stop(paste0('IntraSessionDefaults: ', InputName, ': length(ArgName) == 0'))

  # Select last item from history or use Default
  if(length(List[[InputName]][[ArgName]]) == 0L) {
    if(Debug) print('IntraSessionDefaults: length(List[[InputName]][[ArgName]]) == 0L')
    selected_default <- Default
  } else {
    if(Debug) print('IntraSessionDefaults: length(List[[InputName]][[ArgName]]) != 0L')
    selected_default <- List[[InputName]][[ArgName]][[length(List[[InputName]][[ArgName]])]]
  }

  # Return Default Value
  return(selected_default)
}

#' @title Shiny.CodePrint.Collect
#'
#' @param x code being collected
#' @param y collection list to append to
#' @param Debug passthrough
#'
#' @keywords internal
Shiny.CodePrint.Collect <- function(y,x) {
  options(digits.secs = 6)
  if(missing(y) || length(y) == 0L) y <- list()
  y[['TimeStamp']] <- c(y[['TimeStamp']], Sys.time())
  y[['Code']] <- c(y[['Code']], x)
  return(y)
}

#' @title Shiny.CodePrint.OrganizeCode
#'
#' @param DM = NULL
#' @param DW = NULL
#' @param FE = NULL
#' @param ML = NULL
#' @param PL = NULL
#'
#' @examples
#' \dontrun{
#' # Step through
#' # #listname in c('DataMgtCode','DataWranglingCode','FeatureEngineeringCode','MachineLearningCode','PlottingCode')
#' # listname <- 'DataMgtCode'
#' # DataMgtCode <- list()
#' # DataMgtCode[[paste0(Sys.time())]] <- 'data.table::data.table(A = 1:12)'
#' # DataMgtCode[[paste0(Sys.time())]] <- 'data.table::data.table(B = 1:12)'
#' # DataWranglingCode <- list()
#' # DataWranglingCode[[paste0(Sys.time())]] <- 'data.table::data.table(C = 1:12)'
#' # DataWranglingCode[[paste0(Sys.time())]] <- 'data.table::data.table(D = 1:12)'
#' # MasterSet <- data.table::data.table(TimeStamp = Sys.time(), Code = 'Sys.time() # Lists intialization')
#' }
#'
#' @keywords internal
Shiny.CodePrint.OrganizeCode <- function(DM = NULL,
                                         DW = NULL,
                                         FE = NULL,
                                         ML = NULL,
                                         PL = NULL) {

  # Create data.table with 3 columns:
  #  TimeStamp, Code, Type
  #  Timestamp is for the time the code was ran
  #  Code is the code that was run at the associated TimeStamp
  #  Type is the name of the list the code came from (functional area of app)
  # Sorting by TimeStamp only will work because of the order of arrival for
  #  the code that comes in. Sorting will not rearrange for ties unless we
  #  specify Type as well and then it could rearrange but that would mess up
  #  things.
  if(length(DM) > 0L) {
    MasterSet <- tryCatch({data.table::data.table(TimeStamp = DM$TimeStamp, Code = DM$Code)}, error = function(x) NULL)
    if(length(MasterSet) > 0L) {
      MasterSet[, Type := 'DM']
      data.table::setorderv(MasterSet, cols = 'TimeStamp', order = 1)
    }
  }
  if(length(DW) > 0L) {
    if(exists('MasterSet') && length(MasterSet) > 0L) {
      temp <- tryCatch({data.table::data.table(TimeStamp = DW$TimeStamp, Code = DW$Code)}, error = function(x) NULL)
      if(length(temp) > 0L) {
        temp[, Type := 'DW']
        MasterSet <- data.table::rbindlist(list(MasterSet, temp))
      }
    } else {
      MasterSet <- tryCatch({data.table::data.table(TimeStamp = DW$TimeStamp, Code = DW$Code)}, error = function(x) NULL)
      if(length(MasterSet) > 0L) {
        MasterSet[, Type := 'DW']
      }
    }
  }
  if(length(FE) > 0L) {
    if(exists('MasterSet') && length(MasterSet) > 0L) {
      temp <- tryCatch({data.table::data.table(TimeStamp = FE$TimeStamp, Code = FE$Code)}, error = function(x) NULL)
      if(length(temp) > 0L) {
        temp[, Type := 'FE']
        MasterSet <- data.table::rbindlist(list(MasterSet, temp))
      }
    } else {
      MasterSet <- tryCatch({data.table::data.table(TimeStamp = FE$TimeStamp, Code = FE$Code)}, error = function(x) NULL)
      if(length(MasterSet) > 0L) {
        MasterSet[, Type := 'FE']
      }
    }
  }
  if(length(ML) > 0L) {
    if(exists('MasterSet') && length(MasterSet) > 0L) {
      temp <- tryCatch({data.table::data.table(TimeStamp = ML$TimeStamp, Code = ML$Code)}, error = function(x) NULL)
      if(length(temp) > 0L) {
        temp[, Type := 'ML']
        MasterSet <- data.table::rbindlist(list(MasterSet, temp))
      }
    } else {
      MasterSet <- tryCatch({data.table::data.table(TimeStamp = ML$TimeStamp, Code = ML$Code)}, error = function(x) NULL)
      if(length(MasterSet) > 0L) {
        MasterSet[, Type := 'ML']
      }
    }
  }
  if(length(PL) > 0L) {
    if(exists('MasterSet') && length(MasterSet) > 0L) {
      temp <- tryCatch({data.table::data.table(TimeStamp = PL$TimeStamp, Code = PL$Code)}, error = function(x) NULL)
      if(length(temp) > 0L) {
        temp[, Type := 'PL']
        MasterSet <- data.table::rbindlist(list(MasterSet, temp))
      }
    } else {
      MasterSet <- tryCatch({data.table::data.table(TimeStamp = PL$TimeStamp, Code = PL$Code)}, error = function(x) NULL)
      if(length(MasterSet) > 0L) {
        MasterSet[, Type := 'PL']
      }
    }
  }
  return(MasterSet)
}

#' @title InitializePlotObjects
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
