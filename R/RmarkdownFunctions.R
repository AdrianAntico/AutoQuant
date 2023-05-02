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

#' @param X Vector
#' @param FUN E.g. FUN = CustomFunction. Function with elements of X as the single input argument
#' @param depth Header depth, #, ##, ###, etc. corresponds to 1, 2, 3, etc.
#' @param AddDetails Defaults to TRUE. Iterated output will be contained in Details sections (need to expand to see content).
#' @param FontSizes A named list with the names being identical but the numeric values can vary: list('h1' = 6, 'h2' = 5, 'h3' = 4)
#' @param Opts Default c(echo = FALSE)
#'
#' @examples
#' \dontrun{
#'
#' # Run this outside of any code blocks
#' # but after required functions have been defined
#'
#' ```{r code}
#' my_list <- list()
#' for(i in seq_along(CustomerID)) my_list[[as.character(CustomerID[i])]] <- i
#' ```
#'
#' `r mdapply(list(X = seq_along(ListOfInterest)), CustomerDetailDay, 1, TRUE, list('h1' = 6, 'h2' = 5, 'h3' = 4), c(echo=FALSE))`
#' }
#'
#' @noRd
RmdApply <- function(X,
                     FUN,
                     depth,
                     AddDetails = TRUE,
                     FontSizes = list('h1' = 6, 'h2' = 5, 'h3' = 4),
                     Opts = list(echo = FALSE)) {
  FUN <- as.character(substitute(FUN))
  list_name <- as.character(substitute(X))
  if(!all(Opts == "")) Opts <- paste(",", names(Opts), "=", Opts, collapse="")
  if(AddDetails) {
    build_chunk <- function(HeaderName) {
      paste0(
        paste0(rep("#", depth), collapse = ""), " ", "<font size=", shQuote(as.character(FontSizes[[paste0('h',as.character(depth))]])), ">", list_name, ": ", HeaderName, "</font>",

        "\n\n<details><summary>Reports</summary>\n<p>\n",

        "\n\n```{r", Opts, "}\n",
        FUN, "(", list_name, "[['", HeaderName, "']])\n```", " \n</details>\n</p>")
    }
  } else {
    build_chunk <- function(HeaderName) {
      paste0(
        paste0(rep("#", depth), collapse = ""), " ", list_name, ": ", HeaderName,
        "\n\n```{r", Opts, "}\n",
        FUN, "(", list_name, "[['", HeaderName, "']])\n```")
    }
  }
  parts <- sapply(names(X), build_chunk)
  whole <- paste(parts, collapse = "\n\n")
  knitr::knit(text = whole)
}

#' @title DataTable
#'
#' @description Fully loaded DT::datatable() with args prefilled
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param data source data.table
#' @param FixedCols Number of columns from the left to Freeze, like freeze panes in Excel. Default is 2
#'
#' @examples
#' \dontrun{
#' # Rmarkdown example of DataTable inside a <details> </Details> section
#'
#' ```{r Get Dependencies For DT::datatable(), echo=FALSE,include = FALSE}
#' # You need this code to conduct the magic dependences attaching...
#' DT::datatable(matrix())
#' ```
#'
#' ```{js Nest All DT::datatable() inside a details drop down, echo=FALSE}
#' setTimeout(function() {
#'   var codes = document.querySelectorAll('.dataTables_wrapper');
#'   var code, i, d, s, p;
#'   for (i = 0; i < codes.length; i++) {
#'     code = codes[i];
#'     p = code.parentNode;
#'     d = document.createElement('details');
#'     s = document.createElement('summary');
#'     s.innerText = 'Details';
#'     // <details><summary>Details</summary></details>
#'       d.appendChild(s);
#'     // move the code into <details>
#'       p.replaceChild(d, code);
#'     d.appendChild(code);
#'   }
#' });
#' ```
#'
#' ```{r Example, echo = FALSE}
#' AutoQuant::DataTable(data)
#' ````
#'
#' # Shiny Usage
#' output$Table <- shiny::renderUI({AutoQuant::DataTable(data)})
#'
#' }
#'
#' @export
DataTable <- function(data, FixedCols = 2) {
  DT::datatable(
    data,
    filter = 'bottom',
    editable = TRUE,
    rownames = FALSE,
    extensions = c('Buttons','ColReorder','FixedColumns'), # Only usable in Rmarkdown  'Select'),
    options = list(
      select = list(style = 'os', items = 'row'),
      dom = 'Brtip', #Bfrtip
      #dom = 'ltipr',
      fixedColumns = list(leftColumns = FixedCols),
      buttons = c('copy','pdf'), # Only usable in Rmarkdown 'selectRows', 'selectColumns', 'selectCells', 'selectAll', 'selectNone'),
      colReorder = TRUE,
      autoWidth = TRUE,
      selection = list(mode = 'multiple', target = 'row+column'), # 'row', 'column'
      style = 'bootstrap', # 'auto', 'default', 'bootstrap', or 'bootstrap4'
      columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(data)-1L))),
      targets = "_all",
      scrollX = TRUE,
      fillContainer = TRUE,
      autoHideNavigation = TRUE,
      lengthMenu = c(5, 30, 50),
      pageLength = 10))
}

#' @title DataTable2
#'
#' @description Fully loaded DT::datatable() with args prefilled
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param data source data.table
#' @param FixedCols = 2L
#'
#' @examples
#' \dontrun{
#' # Rmarkdown example of DataTable2 inside a <details> </Details> section
#'
#' ```{r Get Dependencies For DT::datatable(), echo=FALSE,include = FALSE}
#' # You need this code to conduct the magic dependences attaching...
#' DT::datatable(matrix())
#' ```
#'
#' ```{js Nest All DT::datatable() inside a details drop down, echo=FALSE}
#' setTimeout(function() {
#'   var codes = document.querySelectorAll('.dataTables_wrapper');
#'   var code, i, d, s, p;
#'   for (i = 0; i < codes.length; i++) {
#'     code = codes[i];
#'     p = code.parentNode;
#'     d = document.createElement('details');
#'     s = document.createElement('summary');
#'     s.innerText = 'Details';
#'     // <details><summary>Details</summary></details>
#'       d.appendChild(s);
#'     // move the code into <details>
#'       p.replaceChild(d, code);
#'     d.appendChild(code);
#'   }
#' });
#' ```
#'
#' ```{r Example, echo = FALSE}
#' AutoQuant::DataTable2(data)
#' ````
#'
#' # Shiny Usage
#' output$Table <- shiny::renderUI({AutoQuant::DataTable2(data)})
#'
#' }
#'
#' @export
DataTable2 <- function(data, FixedCols = 2L) {
  DT::datatable(
    data,
    filter = 'bottom',
    editable = TRUE,
    rownames = FALSE,
    extensions = c('Buttons','ColReorder','FixedColumns'), # Only usable in Rmarkdown  'Select'),
    options = list(
      select = list(style = 'os', items = 'row'),
      dom = 'Brtip', #Bfrtip
      #dom = 'ltipr',
      fixedColumns = list(leftColumns = 0L),
      buttons = c('copy','pdf', 'selectRows', 'selectColumns', 'selectCells', 'selectAll', 'selectNone'),
      colReorder = TRUE,
      autoWidth = TRUE,
      selection = list(mode = 'multiple', target = 'row+column'), # 'row', 'column'
      style = 'bootstrap', # 'auto', 'default', 'bootstrap', or 'bootstrap4'
      columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(data)-1L))),
      targets = "_all",
      scrollX = TRUE,
      fillContainer = TRUE,
      autoHideNavigation = TRUE,
      lengthMenu = c(5, 30, 50),
      pageLength = 10))
}
