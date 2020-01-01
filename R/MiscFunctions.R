#' tempDatesFun Convert Excel datetime char columns to Date columns
#'
#' tempDatesFun takes the Excel datetime column, which imports as character, and converts it into a date type
#'
#' @author Adrian Antico
#' @family Misc
#' @param x The column of interest
#' @examples
#' Cdata <- data.table::data.table(DAY_DATE = "2018-01-01 8:53")
#' Cdata[, DAY_DATE := tempDatesFun(DAY_DATE)]
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
#' x <- "adrian"
#' x <- SimpleCap(x)
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
SimpleCap <- function(x) {
  s <- data.table::tstrsplit(x, " ")[[1]]
  paste(
    base::toupper(base::substring(s, 1, 1)),
    base::substring(s, 2),
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
#' \donttest{
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

#' RemixTheme function is a ggplot theme generator for ggplots
#'
#' This function adds the Remix Theme to ggplots
#'
#' @author Douglas Pestana
#' @family Misc
#' @examples
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(1000,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' p <- ggplot2::ggplot(data, ggplot2::aes(x = DateTime, y = Target)) + ggplot2::geom_line()
#' p <- p + RemixTheme()
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
RemixTheme <- function() {
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 11),
    axis.text = ggplot2::element_text(size = 11),
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(color = "#1c1c1c",
                                        size = 11),
    legend.title = ggplot2::element_blank(),
    legend.justification = 0,
    legend.position = "top",
    plot.background = ggplot2::element_rect(fill = "#E7E7E7"),
    panel.background = ggplot2::element_rect(fill = "#E7E7E7"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "white"),
    panel.grid.minor.y = ggplot2::element_line(color = "white"),
    plot.title = ggplot2::element_text(
      color = "#1c1c1c",
      size = 28,
      hjust = 0,
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      color = "#1c1c1c",
      size = 16,
      hjust = 0
    ),
    plot.caption = ggplot2::element_text(
      size = 9,
      hjust = 0,
      face = "italic"
    )
  )
}

#' Multiplot is a function for combining multiple plots
#'
#' Sick of copying this one into your code? Well, not anymore.
#'
#' @author Adrian Antico
#' @family Misc
#' @param plotlist This is the list of your charts
#' @param cols This is the number of columns in your multiplot
#' @param layout Leave NULL
#' @param ... Passthrough arguments
#' @examples
#' Correl <- 0.85
#' data <- data.table::data.table(Target = runif(100))
#' data[, x1 := qnorm(Target)]
#' data[, x2 := runif(100)]
#' data[, Independent_Variable1 := log(pnorm(Correl * x1 +
#'                                             sqrt(1-Correl^2) * qnorm(x2)))]
#' data[, Predict := (pnorm(Correl * x1 +
#'                            sqrt(1-Correl^2) * qnorm(x2)))]
#' p1 <- RemixAutoML::ParDepCalPlots(data,
#'                                   PredictionColName = "Predict",
#'                                   TargetColName = "Target",
#'                                   IndepVar = "Independent_Variable1",
#'                                   GraphType = "calibration",
#'                                   PercentileBucket = 0.20,
#'                                   FactLevels = 10,
#'                                   Function = function(x) mean(x, na.rm = TRUE))
#' p2 <- RemixAutoML::ParDepCalPlots(data,
#'                                   PredictionColName = "Predict",
#'                                   TargetColName = "Target",
#'                                   IndepVar = "Independent_Variable1",
#'                                   GraphType = "boxplot",
#'                                   PercentileBucket = 0.20,
#'                                   FactLevels = 10,
#'                                   Function = function(x) mean(x, na.rm = TRUE))
#' RemixAutoML::multiplot(plotlist = list(p1,p2), cols = 2)
#' @return Multiple ggplots on a single image
#' @export
multiplot <-
  function(...,
           plotlist = NULL,
           cols     = 2,
           layout   = NULL) {
    plots <- c(list(...), plotlist)
    
    numPlots <- length(plots)
    
    if (is.null(layout)) {
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols,
                       nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
      
    } else {
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout),
                                                                   ncol(layout))))
      
      for (i in 1:numPlots) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(
          plots[[i]],
          vp = grid::viewport(
            layout.pos.row = matchidx$row,
            layout.pos.col = matchidx$col
          )
        )
      }
    }
  }

#' ChartTheme function is a ggplot theme generator for ggplots
#'
#' This function helps your ggplots look professional with the choice of the two main colors that will dominate the theme
#'
#' @author Adrian Antico
#' @family Misc
#' @param Size The size of the axis labels and title
#' @param AngleX The angle of the x axis labels
#' @param AngleY The angle of the Y axis labels
#' @param ChartColor "lightsteelblue1",
#' @param BorderColor "darkblue",
#' @param TextColor "darkblue",
#' @param GridColor "white",
#' @param BackGroundColor "gray95",
#' @param LegendPosition Where to place legend
#' @examples
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(1000,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][, temp := NULL]
#' data <- data[order(DateTime)]
#' p <- ggplot2::ggplot(data, ggplot2::aes(x = DateTime, y = Target)) + ggplot2::geom_line()
#' p <- p + ChartTheme(Size = 12)
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
ChartTheme <- function(Size = 12, 
                       AngleX = 35, 
                       AngleY = 0,
                       ChartColor = "lightsteelblue1",
                       BorderColor = "darkblue",
                       TextColor = "darkblue",
                       GridColor = "white",
                       BackGroundColor = "gray95",
                       LegendPosition = "bottom") {
  chart_theme <-
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = BackGroundColor),
      panel.background = ggplot2::element_rect(
        fill = ChartColor,
        colour = BorderColor,
        size = 0.25,
        color = BorderColor
      ),
      panel.grid.major = ggplot2::element_line(
        colour = BorderColor,
        size = 0.01,
        color = GridColor,
        linetype = 1
      ),
      panel.grid.minor = ggplot2::element_line(
        colour = BorderColor,
        size = 0.01,
        color = GridColor,
        linetype = 1
      ),
      legend.position = LegendPosition,
      legend.title = ggplot2::element_text(
        color = BorderColor,
        size = Size,
        face = "bold"
      ),
      legend.background = ggplot2::element_rect(
        fill = BackGroundColor,
        size = 1,
        linetype = "solid",
        color = BorderColor
      ),
      plot.title = ggplot2::element_text(
        color = TextColor,
        size = Size,
        face = "bold"
      ),
      axis.title = ggplot2::element_text(
        color = TextColor,
        size = Size,
        face = "bold"
      ),
      axis.text.x = ggplot2::element_text(
        colour = TextColor,
        face = "bold",
        angle = AngleX
      ),
      axis.text.y = ggplot2::element_text(
        colour = TextColor,
        face = "bold",
        angle = AngleY
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(
        t = 20,
        r = 20,
        b = 20,
        l = 20
      )),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(
        t = 20,
        r = 20,
        b = 20,
        l = 20
      )),
      panel.border = ggplot2::element_rect(
        colour = BorderColor,
        fill = NA,
        size = 1.5
      )
    )
  chart_theme
}

#' Percentile rank function
#'
#' This function computes percentile ranks for each row in your data like Excel's PERCENT_RANK
#'
#' @author Adrian Antico
#' @family Misc
#' @param x X is your variable of interest
#' @examples
#' data <- data.table::data.table(A = runif(100))
#' data[, Rank := percRank(A)]
#' @return vector of percentile ranks
#' @examples
#' data <- data.table::data.table(A = runif(100))
#' data[, Percentile := percRank(A)]
#' @export
percRank <- function(x) {
  trunc(rank(x)) / length(x)
}