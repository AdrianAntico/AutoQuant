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
#' @param Width Default is 7
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
                       Width = 7,
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
        if(nrow(ObjectList[[i]]) > 15) {
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
        }
        while(grDevices::dev.cur() > 1) grDevices::dev.off()
      }
    }
  } else {
    warning("Need to install the package grDevices in order to run this function")
  }
}

#' tempDatesFun Convert Excel datetime char columns to Date columns
#'
#' tempDatesFun takes the Excel datetime column, which imports as character, and converts it into a date type
#'
#' @author Adrian Antico
#' @family Misc
#' @param x The column of interest
#' @examples
#' \dontrun{
#' Cdata <- data.table::data.table(DAY_DATE = "2018-01-01 8:53")
#' Cdata[, DAY_DATE := tempDatesFun(DAY_DATE)]
#' }
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
tempDatesFun <- base::Vectorize(function(x) {
  data.table::tstrsplit(x, " ")[[1]][1]
})

#' SimpleCap function is for capitalizing the first letter of words
#'
#' SimpleCap function is for capitalizing the first letter of words (need I say more?)
#'
#' @author Adrian Antico
#' @family Misc
#' @param x Column of interest
#' @examples
#' \dontrun{
#' x <- "adrian"
#' x <- SimpleCap(x)
#' }
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
SimpleCap <- function(x) {
  s <- data.table::tstrsplit(x, " ")[[1]]
  paste(
    toupper(base::substring(s, 1, 1)),
    substring(s, 2),
    sep = "",
    collapse = " "
  )
}

#' PrintObjectsSize prints out the top N objects and their associated sizes, sorted by size
#'
#' @author Adrian Antico
#' @family Misc
#' @param N The number of objects to display
#' @examples
#' \dontrun{
#' PrintObjectsSize(N = 10)
#' }
#' @return A print to your console of the sizes of the objects in your environment
#' @export
PrintObjectsSize <- function(N = 10) {
  m <- length(ls())
  z <- min(m, N)
  print(sort(-vapply(ls(), FUN.VALUE = 1.1, function(x) {
    object.size(get(x))
  }))[1:z] / 1024 / 1024)
}
