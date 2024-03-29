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

#' @title PrintToPDF
#'
#' @description Print a list of elements to pdf
#'
#' @author Adrian Antico
#' @family Misc
#'
#' @param Path Path file to the location where you want your pdf saved
#' @param OutputName Supply a name for the file you want saved
#' @param ObjectList List of objects to print to pdf
#' @param Tables TRUE for data tables, FALSE for plots
#' @param MaxPages Default of 500
#' @param Title The title of the pdf
#' @param Width Default is 12
#' @param Height Default is 7
#' @param Paper 'USr' for landscape. 'special' means that Width and Height are used to determine page size
#' @param BackgroundColor Default is 'transparent'
#' @param ForegroundColor Default is 'black'
#'
#' @noRd
PrintToPDF <- function(Path,
                       OutputName,
                       ObjectList = NULL,
                       Tables = FALSE,
                       MaxPages = 500,
                       Title = "Model Output",
                       Width = 12,
                       Height = 7,
                       Paper = "USr",
                       BackgroundColor = "transparent",
                       ForegroundColor = "black") {

  # Check for installed package
  if("grDevices" %chin% installed.packages()) {

    # Ensure procedure can run ----
    if(is.null(ObjectList) ||
       is.null(Path) ||
       !is.list(ObjectList)) {
      print("Nothing to print")
      stop()
    }

    # Print away ----
    if(!Tables) {
      grDevices::pdf(file = file.path(Path, paste0(OutputName,".pdf")),
                     onefile = TRUE,
                     title = Title,
                     width = Width,
                     height = Height,
                     fonts = NULL,
                     paper = Paper,
                     bg = BackgroundColor,
                     fg = ForegroundColor,
                     compress = TRUE)
      for(i in seq_along(ObjectList)) for(j in seq_along(ObjectList[[i]])) multiplot(plotlist = list(ObjectList[[i]][[j]]))
      while(grDevices::dev.cur() > 1) grDevices::dev.off()
    } else {
      for(i in seq_along(ObjectList)) {
        grDevices::pdf(file = file.path(normalizePath(Path), paste0(OutputName,"_",i,".pdf")),
                       onefile = TRUE,
                       title = Title,
                       width = Width,
                       height = Height,
                       fonts = NULL,
                       paper = Paper,
                       bg = BackgroundColor,
                       fg = ForegroundColor,
                       compress = TRUE)
        tryCatch({
          for(j in seq_along(ObjectList[[i]])) {
            counter <- 1L
            repeat{
              temp <- ObjectList[[i]][[j]][counter:(counter + 14L)]
              temp <- temp[!is.na(temp[[eval(names(temp)[1])]])]
              counter <- counter + 15L
              if(temp[,.N] < 15 || counter == MaxPages+1L) break
              print(gridExtra::grid.table(temp, rows = NULL))
              grid::grid.newpage()
            }
          }}, error = function(x) NULL)
        while(grDevices::dev.cur() > 1) grDevices::dev.off()
      }
    }
  } else {
    warning("Need to install the package grDevices in order to run this function")
  }
}

#' @title DeleteFile
#'
#' @description DeleteFile will prompt you for a file to delete and then permanently delete a file. You won't have to go the the recycle bin to delete it a second time
#'
#' @family System Functions
#'
#' @author Adrian Antico
#'
#' @param File If NULL a prompt will allow you to click on the file to have it removed. Otherwise, supply a path to the file including its name and extension
#' @noRd
DeleteFile <- function(File = NULL) {
  if(is.null(File)) {
    shell(paste0("del ", file.choose()))
  } else {
    shell(paste0("del ", File))
  }
}

#' @title Logger
#'
#' @description Logging errors and warnings from repeated calls to a function
#'
#' @author Adrian Antico
#'
#' @family Misc
#'
#' @param x Function to call repeatedly
#'
#' @examples
#' \dontrun{
#' Output <- lapply(1:10, FUN = Logger(PrintToPDF))
#' }
#'
#' @noRd
Logger <- function(x) {
  function(...) {
    warn <- err <- NULL
    res <- withCallingHandlers(
      tryCatch(x(...), error = function(e) {
        err <<- conditionMessage(e)
        NULL
      }), warning = function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    list(res, warn = warn, err = err)
  }
}

#' @title LB
#'
#' @description Create default for CreateHolidayVariables
#'
#' @author Adrian Antico
#'
#' @family Misc
#'
#' @param TimeAgg Valid options are "hour", "hours", "1min", "1mins", "1minute", "1minutes", "5min", "5mins", "5minute", "5minutes","10min", "10mins", "10minute", "10minutes", "15min", "15mins", "15minute", "15minutes", "30min", "30mins", "30minute", "30minutes", "day", "days", "week", "weeks", "month", "months", "quarter", "quarters", "years", "year"
#'
#' @examples
#' \dontrun{
#' Lookback <- LB("days")
#' }
#'
#' @noRd
LB <- function(TimeAgg) {
  if(tolower(TimeAgg) %chin% c("hour","hours",
                               "1min","1mins","1minute","1minutes",
                               "5min","5mins","5minute","5minutes",
                               "10min","10mins","10minute","10minutes",
                               "15min","15mins","15minute","15minutes",
                               "30min","30mins","30minute","30minutes",
                               "day","days")) {
    return(1)
  } else if(tolower(TimeAgg) %chin% c("week","weeks")) {
    return(7)
  } else if(tolower(TimeAgg) %chin% c("month","months")) {
    return(30)
  } else if(tolower(TimeAgg) %chin% c("quarter","quarters")) {
    return(120)
  } else if(tolower(TimeAgg) %chin% c("years","year")) {
    return(365)
  }
}

#' @title BuildBinary
#'
#' @description Build package binary
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @param Root NULL will setwd to project root as defined in function
#'
#' @noRd
BuildBinary <- function(Root = NULL) {
  x <- getwd()
  if(!is.null(Root)) {
    setwd(Root)
    devtools::install(pkg = "AutoQuant", dependencies = FALSE)
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub")
    devtools::build(pkg = "AutoQuant")
  }
  setwd(x)
}

#' @title Install
#'
#' @description To install the package
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @param Root NULL will setwd to project root as defined in function
#'
#' @noRd
Install <- function(Root = NULL) {
  x <- getwd()
  if(!is.null(Root)) {
    setwd(Root)
    devtools::install(pkg = "AutoQuant", dependencies = FALSE)
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub")
    devtools::install(pkg = "AutoQuant", dependencies = FALSE)
  }
  setwd(x)
}

#' @title UpdateDocs
#'
#' @description Update helf files and reference manual
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @noRd
UpdateDocs <- function(BuildVignette = FALSE, Root = NULL) {
  x <- getwd()
  if(!is.null(Root)) {
    setwd(Root)
    devtools::document()
    if(BuildVignette) devtools::build_manual()
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub/AutoQuant")
    devtools::document()
    if(BuildVignette) devtools::build_manual()
  }
  setwd(x)
}

#' @title ParseOptParse
#' @param x object value from optparse. e.g. ArgsList$TargetVariables
#' @noRd
ParseOptParse <- function(x) {
  if(!is.null(x)) {
    return(as.character(if(is.list(strsplit(x, ","))) unlist(strsplit(x, ",")) else x))
  } else {
    return(x)
  }
}

#' @title ColTypes
#' @param data Source data.table
#' @noRd
ColTypes <- function(data) {
  CT <- c()
  for(Col in names(data)) CT <- c(CT, class(data[[Col]])[[1L]])
  CT
}

#' @title LoadAssign
#'
#' @description LoadAssign will assign the loaded object to a new object. xx <- LoadAssign`(FilePath)
#'
#' @author Adrian Antico
#' @family Utilities
#'
#' @param FilePath
#'
#' @noRd
LoadAssign <- function(FilePath) {
  load(FilePath, envir = .GlobalEnv)
  get(ls()[ls() != "FilePath"])
}

#' @title ColNames
#'
#' @family Misc
#' @author Adrian Antico
#'
#' @param data source data.table
#' @param Types 'all' (basically names(data)), 'numeric', 'character', 'factor', 'logical', 'posix', 'date', 'integer64'
#'
#' @noRd
ColNameFilter <- function(data, Types = 'all') {
  if(Types == 'all') return(names(data))
  nam <- c()
  for(t in Types) {
    if(tolower(t) == 'numeric') {
      nam <- c(nam, NumericColNames(data))
    } else if(tolower(t) == 'character') {
      nam <- c(nam, CharacterColNames(data))
    } else if(tolower(t) == 'factor') {
      nam <- c(nam, FactorColNames(data))
    } else if(tolower(t) == 'logical') {
      nam <- c(nam, LogicalColNames(data))
    }
  }
  return(unique(nam))
}

#' @noRd
NumericColNames <- function(data) {
  x <- names(data)[which(sapply(data, is.numeric))]
  if(!identical(x, character(0))) return(x) else return(NULL)
}

#' @noRd
CharacterColNames <- function(data) {
  x <- names(data)[which(sapply(data, is.character))]
  if(!identical(x, character(0))) return(x) else return(NULL)
}

#' @noRd
FactorColNames <- function(data) {
  x <- names(data)[which(sapply(data, is.factor))]
  if(!identical(x, character(0))) return(x) else return(NULL)
}

#' @noRd
LogicalColNames <- function(data) {
  x <- names(data)[which(sapply(data, is.logical))]
  if(!identical(x, character(0))) return(x) else return(NULL)
}

# ColNameFilter(data, Types = 'numeric')
# ColNameFilter(data, Types = 'factor')
# ColNameFilter(data, Types = 'charcter')
# ColNameFilter(data, Types = 'logical')

#' @noRd
Names2Vector <- function(xx) {
  for(gg in seq_along(names(xx))) if(gg == 1L) cat(paste0("c('",names(xx)[gg], "',\n", collapse = "")) else if(gg != max(seq_along(names(xx)))) cat(paste0("  '", names(xx)[gg], "',\n")) else cat(paste0("  '", names(xx)[gg], "')\n"))
}
