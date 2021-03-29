#' PrintToPDF
#'
#' @author Adrian Antico
#' @family Misc
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
#' @export
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
    if(!"grDevices" %chin% installed.packages() ||
       is.null(ObjectList) ||
       is.null(Path) ||
       !is.list(ObjectList)) {
      print("Nothing to print")
      stop()
    }

    # Print away ----
    if(!Tables) {
      grDevices::pdf(file = file.path(normalizePath(Path), paste0(OutputName,".pdf")),
                     onefile = TRUE,
                     title = Title,
                     width = Width,
                     height = Height,
                     fonts = NULL,
                     paper = Paper,
                     bg = BackgroundColor,
                     fg = ForegroundColor,
                     compress = TRUE)
      for(i in seq_along(ObjectList)) multiplot(plotlist = list(ObjectList[[i]]), cols = 1)
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
        tryCatch({if(nrow(ObjectList[[i]]) > 15) {
          counter <- 1L
          repeat{
            temp <- ObjectList[[i]][counter:(counter + 14L)]
            temp <- temp[!is.na(temp[[eval(names(temp)[1])]])]
            counter <- counter + 15L
            if(temp[,.N] < 15 || counter == MaxPages+1L) break
            print(gridExtra::grid.table(temp, rows = NULL))
            grid::grid.newpage()
          }
        } else {
          print(gridExtra::grid.table(ObjectList[[i]], rows = NULL))
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
#' @export
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
#' @export
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
#' @export
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

#' @title UpdatePackageDocs
#' @param BuildManual FALSE
#' @param Path Package path
#' @noRd
UpdatePackageDocs <- function(BuildManual = FALSE, Path = "C:/Users/Bizon/Documents/GitHub/RemixAutoML") {
  setwd(Path)
  devtools::document()
  if(BuildManual) devtools::build_manual()
}

#' @title InstallLocal
#' @param Path Package path cd ..
#' @noRd
InstallLocal <- function(Path = "C:/Users/Bizon/Documents/GitHub") {
  setwd(Path)
  devtools::install(pkg = "RemixAutoML", dependencies = FALSE)
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
  for(Col in names(data)) CT <- c(CT, class(data[[Col]][[1L]]))
}
