# ============================================================
# EDA Artifact Generation
# Converted from Rmarkdown chunks into regular R code.
#
# Purpose:
#   1. Generate all EDA artifacts before rendering.
#   2. Pass those artifacts into Rmarkdown for dynamic display.
#   3. Optionally export PNG/HTML sidecars for LLM vision/API usage.
#
# Notes:
#   - This file intentionally delegates object export work to the helpers in
#     object_export_helpers.R: ObjectToPNG(), ObjectToHTML(),
#     ObjectFileToDataURL(), and ObjectFileToMarkdown().
#   - In a package, include both files in R/ and export the object helpers.
#   - When sourcing manually, source object_export_helpers.R before this file.
# ============================================================



# ============================================================
# Artifact Sidecar Helpers
# ============================================================

eda_slug <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- tolower(x)
  if (!nzchar(x)) x <- "artifact"
  x
}

eda_object_export_helpers_available <- function() {
  all(vapply(
    c("ObjectToPNG", "ObjectToHTML", "ObjectFileToDataURL", "ObjectFileToMarkdown"),
    exists,
    logical(1L),
    mode = "function"
  ))
}

eda_require_object_export_helpers <- function() {
  if (!eda_object_export_helpers_available()) {
    stop(
      paste(
        "Object export helpers are required for ExportPNG/ExportHTML/IncludeDataURL.",
        "Source object_export_helpers.R first, or include it in the package R/ directory.",
        "Required functions: ObjectToPNG(), ObjectToHTML(), ObjectFileToDataURL(), ObjectFileToMarkdown()."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

eda_is_renderable_object <- function(object) {
  inherits(object, "ggplot") ||
    inherits(object, "htmlwidget") ||
    inherits(object, "recordedplot") ||
    is.function(object)
}

eda_artifact <- function(
    object,
    section,
    artifact_type,
    name,
    title = NULL,
    description = NULL,
    metadata = list()
) {
  list(
    object = object,
    png = NULL,
    html = NULL,
    data_url = NULL,
    markdown = NULL,
    section = section,
    artifact_type = artifact_type,
    name = name,
    title = title,
    description = description,
    metadata = metadata
  )
}

eda_wrap_named_objects <- function(x, section, artifact_type, metadata = list()) {
  if (is.null(x)) {
    return(NULL)
  }

  if (!is.list(x) || inherits(x, "htmlwidget") || inherits(x, "ggplot") || inherits(x, "reactable")) {
    nm <- artifact_type
    return(eda_artifact(x, section = section, artifact_type = artifact_type, name = nm, metadata = metadata))
  }

  nms <- names(x)
  if (is.null(nms)) nms <- as.character(seq_along(x))

  stats::setNames(
    lapply(seq_along(x), function(i) {
      nm <- nms[[i]]
      obj <- x[[i]]

      if (is.list(obj) &&
          !inherits(obj, "htmlwidget") &&
          !inherits(obj, "ggplot") &&
          !inherits(obj, "reactable") &&
          !inherits(obj, "datatables")) {
        eda_wrap_named_objects(obj, section = section, artifact_type = artifact_type, metadata = metadata)
      } else {
        eda_artifact(
          object = obj,
          section = section,
          artifact_type = artifact_type,
          name = nm,
          metadata = c(metadata, list(variable = nm))
        )
      }
    }),
    nms
  )
}

eda_export_artifact_tree <- function(
    tree,
    output_path,
    export_png = FALSE,
    export_html = FALSE,
    width = 1400,
    height = 900,
    dpi = 150,
    background = "white",
    delay = 0.2,
    include_data_url = FALSE,
    path_parts = character()
) {
  if (is.null(tree)) {
    return(tree)
  }

  if (isTRUE(export_png) || isTRUE(export_html) || isTRUE(include_data_url)) {
    eda_require_object_export_helpers()
  }

  is_artifact_node <- is.list(tree) &&
    all(c("object", "png", "html", "section", "artifact_type", "name") %in% names(tree))

  if (is_artifact_node) {
    obj <- tree$object

    if (eda_is_renderable_object(obj)) {
      stem <- eda_slug(paste(c(path_parts, tree$section, tree$artifact_type, tree$name), collapse = "_"))

      if (isTRUE(export_png)) {
        png_file <- file.path(output_path, "images", paste0(stem, ".png"))

        tree$png <- tryCatch(
          ObjectToPNG(
            object = obj,
            file = png_file,
            width = width,
            height = height,
            dpi = dpi,
            background = background,
            overwrite = TRUE,
            delay = delay
          ),
          error = function(e) {
            tree$png_error <<- conditionMessage(e)
            NULL
          }
        )

        if (!is.null(tree$png)) {
          tree$markdown <- ObjectFileToMarkdown(
            file = tree$png,
            alt = ifelse(is.null(tree$title), tree$name, tree$title)
          )

          if (isTRUE(include_data_url)) {
            tree$data_url <- tryCatch(
              ObjectFileToDataURL(tree$png, mime_type = "image/png"),
              error = function(e) {
                tree$data_url_error <<- conditionMessage(e)
                NULL
              }
            )
          }
        }
      }

      if (isTRUE(export_html)) {
        html_file <- file.path(output_path, "html", paste0(stem, ".html"))

        tree$html <- tryCatch(
          ObjectToHTML(
            object = obj,
            file = html_file,
            width = width,
            height = height,
            dpi = dpi,
            background = background,
            overwrite = TRUE
          ),
          error = function(e) {
            tree$html_error <<- conditionMessage(e)
            NULL
          }
        )
      }
    }

    return(tree)
  }

  if (is.list(tree)) {
    nms <- names(tree)
    if (is.null(nms)) nms <- as.character(seq_along(tree))

    for (i in seq_along(tree)) {
      tree[[i]] <- eda_export_artifact_tree(
        tree[[i]],
        output_path = output_path,
        export_png = export_png,
        export_html = export_html,
        width = width,
        height = height,
        dpi = dpi,
        background = background,
        delay = delay,
        include_data_url = include_data_url,
        path_parts = c(path_parts, nms[[i]])
      )
    }
  }

  tree
}

eda_collect_llm_image_manifest <- function(tree, path = character()) {
  if (is.null(tree)) return(list())

  is_artifact_node <- is.list(tree) &&
    all(c("object", "png", "html", "section", "artifact_type", "name") %in% names(tree))

  if (is_artifact_node) {
    return(list(list(
      id = paste(c(path, tree$section, tree$artifact_type, tree$name), collapse = "/"),
      section = tree$section,
      artifact_type = tree$artifact_type,
      name = tree$name,
      title = tree$title,
      description = tree$description,
      png = tree$png,
      html = tree$html,
      metadata = tree$metadata,
      png_error = tree$png_error,
      html_error = tree$html_error,
      data_url_error = tree$data_url_error
    )))
  }

  if (!is.list(tree)) return(list())

  nms <- names(tree)
  if (is.null(nms)) nms <- as.character(seq_along(tree))

  unlist(
    lapply(seq_along(tree), function(i) eda_collect_llm_image_manifest(tree[[i]], c(path, nms[[i]]))),
    recursive = FALSE
  )
}

#' Generate EDA Artifacts
#'
#' Generates tables, widgets, plots, QA outputs, and LLM-friendly context for an
#' EDA report. The function returns the original dynamic objects for RMarkdown
#' display and, optionally, sidecar PNG/HTML exports for LLM vision workflows or
#' portable reporting.
#'
#' The legacy return structure is preserved through `tables`, `widgets`, and
#' `plots`. A wrapped artifact tree is also returned in `artifacts`, where each
#' display item has an `object`, optional `png`, optional `html`, and compact
#' metadata.
#'
#' @param data A data.frame or data.table.
#' @param DataName Optional dataset name used in metadata.
#' @param UnivariateVars Character vector of variables for univariate analysis.
#'   If `NULL`, all columns are considered.
#' @param CorrVars Character vector of variables for correlation analysis.
#' @param TrendVars Character vector of variables for trend analysis.
#' @param TrendDateVar Character scalar date variable used for trend plots.
#' @param TrendGroupVar Character scalar grouping variable used for grouped plots.
#' @param Theme Character scalar passed to AutoPlots plotting functions.
#' @param MaxCategoricalLevels Integer. Maximum categorical levels before an
#'   "Other" bucket is created in categorical bar plots.
#' @param MaxDiscreteNumericLevels Integer. Maximum unique numeric values for a
#'   numeric variable to be treated as discrete in bar plots.
#' @param CorrelationMethod Character scalar. Correlation method passed to
#'   [stats::cor()] and [stats::cor.test()].
#' @param MaxCorrelationPairsToPlot Integer. Maximum correlation pair plots.
#' @param HighCorrelationThreshold Numeric. Absolute correlation threshold used
#'   for high-correlation flags.
#' @param HistogramPlotCols,BoxPlotCols,GroupedBoxPlotCols,DiscreteNumericBarPlotCols,CategoricalBarPlotCols
#'   Integer layout hints for downstream RMarkdown display.
#' @param OutputPath Character scalar. Required when `ExportPNG` or `ExportHTML`
#'   is `TRUE`. Sidecars are written under `images/` and `html/` subdirectories.
#' @param ExportPNG Logical. If `TRUE`, render display artifacts to PNG sidecars.
#' @param ExportHTML Logical. If `TRUE`, render display artifacts to HTML sidecars.
#' @param PNGWidth,PNGHeight Numeric. PNG export dimensions in pixels.
#' @param PNGDPI Numeric. PNG export resolution.
#' @param PNGBackground Character scalar. Background color used when rasterizing.
#' @param WebshotDelay Numeric. Delay in seconds before capturing htmlwidgets
#'   with webshot2.
#' @param IncludeDataURL Logical. If `TRUE`, include Base64 data URLs during
#'   sidecar export. Defaults to `FALSE` to avoid bloating memory/RDS files.
#'
#' @return A named list with `report_type`, `metadata`, `qa`, `tables`,
#'   `widgets`, `plots`, `layout`, `exports`, and `context`. The returned
#'   object contains one canonical copy of each table/widget/plot; PNG/HTML
#'   sidecar paths are stored in `exports$image_manifest`.
#'
#' @examples
#' \dontrun{
#' artifacts <- generate_eda_artifacts(
#'   data = data,
#'   DataName = "My Dataset",
#'   ExportPNG = TRUE,
#'   OutputPath = "eda_artifacts"
#' )
#'
#' # Dynamic RMarkdown object:
#' artifacts$plots$univariate$Histograms[[1]]
#'
#' # PNG/HTML sidecar manifest:
#' artifacts$exports$image_manifest
#' }
#'
#' @export
generate_eda_artifacts <- function(
    data,
    DataName = NULL,
    UnivariateVars = NULL,
    CorrVars = NULL,
    TrendVars = NULL,
    TrendDateVar = NULL,
    TrendGroupVar = NULL,
    Theme = "dark",
    MaxCategoricalLevels = 25L,
    MaxDiscreteNumericLevels = 20L,
    CorrelationMethod = "spearman",
    MaxCorrelationPairsToPlot = 25L,
    HighCorrelationThreshold = 0.70,
    HistogramPlotCols = 2L,
    BoxPlotCols = 2L,
    GroupedBoxPlotCols = 2L,
    DiscreteNumericBarPlotCols = 2L,
    CategoricalBarPlotCols = 1L,
    OutputPath = NULL,
    ExportPNG = FALSE,
    ExportHTML = FALSE,
    PNGWidth = 1400,
    PNGHeight = 900,
    PNGDPI = 150,
    PNGBackground = "white",
    WebshotDelay = 0.2,
    IncludeDataURL = FALSE
) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.", call. = FALSE)
  }
  if (!requireNamespace("reactable", quietly = TRUE)) {
    stop("Package 'reactable' is required.", call. = FALSE)
  }
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is required.", call. = FALSE)
  }
  if (!requireNamespace("e1071", quietly = TRUE)) {
    stop("Package 'e1071' is required.", call. = FALSE)
  }
  if (!requireNamespace("echarts4r", quietly = TRUE)) {
    stop("Package 'echarts4r' is required.", call. = FALSE)
  }
  if (!requireNamespace("AutoPlots", quietly = TRUE)) {
    stop("Package 'AutoPlots' is required.", call. = FALSE)
  }

  data <- data.table::as.data.table(data)

  # ============================================================
  # Shared Formatting Safety Helper
  # ============================================================

  safe_format_digits <- function(digits = NULL, default = 3L, max_digits = 8L) {

    if (is.null(digits) || length(digits) == 0L) {
      return(as.integer(default))
    }

    digits <- suppressWarnings(as.integer(digits[1L]))

    if (is.na(digits) || digits < 0L) {
      return(as.integer(default))
    }

    as.integer(min(digits, max_digits))
  }

  # ============================================================
  # Data Description / Column Audit
  # ============================================================

  null_count <- function(x) {
    # Atomic data.frame/data.table columns cannot contain row-level NULLs.
    # List columns can.
    if (is.list(x)) {
      return(sum(vapply(x, is.null, logical(1L))))
    }

    0L
  }

  empty_count <- function(x) {
    # Empty strings for character/factor-like fields.
    # Empty elements for list columns.
    if (is.character(x)) {
      return(sum(!is.na(x) & trimws(x) == ""))
    }

    if (is.factor(x)) {
      y <- as.character(x)
      return(sum(!is.na(y) & trimws(y) == ""))
    }

    if (is.list(x)) {
      return(sum(vapply(x, function(z) length(z) == 0L, logical(1L))))
    }

    0L
  }

  infinite_count <- function(x) {
    if (is.numeric(x) || is.integer(x)) {
      return(sum(is.infinite(x)))
    }

    0L
  }

  nan_count <- function(x) {
    if (is.numeric(x) || is.integer(x)) {
      return(sum(is.nan(x)))
    }

    0L
  }

  unique_count <- function(x) {
    if (is.list(x)) {
      # uniqueN() can be awkward on arbitrary list columns.
      return(NA_integer_)
    }

    data.table::uniqueN(x, na.rm = FALSE)
  }

  non_missing_unique_count <- function(x) {
    if (is.list(x)) {
      return(NA_integer_)
    }

    data.table::uniqueN(x, na.rm = TRUE)
  }

  mode_value <- function(x) {
    if (is.list(x)) {
      return(NA_character_)
    }

    y <- x[!is.na(x)]

    if (!length(y)) {
      return(NA_character_)
    }

    tab <- sort(table(y), decreasing = TRUE)
    as.character(names(tab)[1L])
  }

  mode_count <- function(x) {
    if (is.list(x)) {
      return(NA_integer_)
    }

    y <- x[!is.na(x)]

    if (!length(y)) {
      return(NA_integer_)
    }

    max(as.integer(table(y)))
  }

  first_values <- function(x, n = 5L) {
    y <- utils::head(x, n)

    if (is.list(y)) {
      y <- vapply(
        y,
        function(z) {
          if (is.null(z)) {
            "<NULL>"
          } else if (length(z) == 0L) {
            "<EMPTY>"
          } else {
            paste(as.character(z), collapse = "|")
          }
        },
        character(1L)
      )
    } else {
      y <- as.character(y)
      y[is.na(y)] <- "<NA>"
      y[trimws(y) == ""] <- "<EMPTY STRING>"
    }

    paste(y, collapse = ", ")
  }

  is_numeric_like_character <- function(x) {
    if (!is.character(x) && !is.factor(x)) {
      return(FALSE)
    }

    y <- trimws(as.character(x))
    y <- y[!is.na(y) & y != ""]

    if (!length(y)) {
      return(FALSE)
    }

    mean(!is.na(suppressWarnings(as.numeric(y)))) >= 0.95
  }

  is_date_like_character <- function(x) {
    if (!is.character(x) && !is.factor(x)) {
      return(FALSE)
    }

    y <- trimws(as.character(x))
    y <- y[!is.na(y) & y != ""]

    if (!length(y)) {
      return(FALSE)
    }

    if (length(y) > 1000L) {
      y <- sample(y, 1000L)
    }

    # Only check highly standard formats.
    # Do not use loose as.Date(y), because it can error and is too magical.
    formats <- c(
      "%Y-%m-%d",
      "%Y/%m/%d",
      "%m/%d/%Y",
      "%m-%d-%Y",
      "%d-%b-%Y",
      "%d %b %Y"
    )

    rates <- vapply(
      formats,
      function(fmt) {
        parsed <- tryCatch(
          suppressWarnings(as.Date(y, format = fmt)),
          error = function(e) rep(as.Date(NA), length(y))
        )

        mean(!is.na(parsed))
      },
      numeric(1L)
    )

    max(rates, na.rm = TRUE) >= 0.95
  }

  column_role_guess <- function(x, var_name = NULL, n_rows = NULL) {

    cls <- class(x)[1L]
    unique_n <- unique_count(x)
    non_missing_n <- sum(!is.na(x))

    if (is.list(x)) {
      return("List Column")
    }

    if (!is.null(var_name) && grepl("(^id$|_id$|id$|uuid|guid)", var_name, ignore.case = TRUE)) {
      return("Possible ID")
    }

    if (inherits(x, c("Date", "POSIXct", "POSIXlt", "IDate", "ITime"))) {
      return("Date/Time")
    }

    if (cls %in% c("character", "factor")) {
      if (!is.na(unique_n) && unique_n <= 2L) {
        return("Binary Categorical")
      }

      if (is_numeric_like_character(x)) {
        return("Numeric-like Text")
      }

      if (is_date_like_character(x)) {
        return("Date-like Text")
      }

      if (!is.na(unique_n) && !is.null(n_rows) && unique_n > 0.9 * n_rows) {
        return("High-Cardinality Text")
      }

      return("Categorical")
    }

    if (cls %in% c("logical")) {
      return("Logical")
    }

    if (cls %in% c("numeric", "integer")) {
      if (!is.na(unique_n) && unique_n <= 2L) {
        return("Binary Numeric")
      }

      if (!is.na(unique_n) && !is.null(n_rows) && unique_n > 0.9 * n_rows) {
        return("Continuous / Possible ID")
      }

      return("Numeric")
    }

    cls
  }

  n_rows <- nrow(data)

  DescribeData <- data.table::data.table(
    `Variable Name` = names(data)
  )

  DescribeData[, `Variable Type` := vapply(
    data,
    function(x) class(x)[1L],
    character(1L)
  )]

  DescribeData[, `Column Role Guess` := vapply(
    names(data),
    function(nm) column_role_guess(data[[nm]], var_name = nm, n_rows = n_rows),
    character(1L)
  )]

  DescribeData[, `Rows` := n_rows]

  DescribeData[, `NULL Counts` := vapply(
    data,
    null_count,
    integer(1L)
  )]

  DescribeData[, `NA Counts` := vapply(
    data,
    function(x) sum(is.na(x)),
    integer(1L)
  )]

  DescribeData[, `NA Percent` := round(
    vapply(data, function(x) mean(is.na(x)) * 100, numeric(1L)),
    2
  )]

  DescribeData[, `NaN Counts` := vapply(
    data,
    nan_count,
    integer(1L)
  )]

  DescribeData[, `Infinite Counts` := vapply(
    data,
    infinite_count,
    integer(1L)
  )]

  DescribeData[, `Empty Counts` := vapply(
    data,
    empty_count,
    integer(1L)
  )]

  DescribeData[, `Unique Values` := vapply(
    data,
    unique_count,
    integer(1L)
  )]

  DescribeData[, `Non-Missing Unique Values` := vapply(
    data,
    non_missing_unique_count,
    integer(1L)
  )]

  DescribeData[, `Unique Percent` := round(
    `Unique Values` / pmax(`Rows`, 1L) * 100,
    2
  )]

  DescribeData[, `Mode` := vapply(
    data,
    mode_value,
    character(1L)
  )]

  DescribeData[, `Mode Count` := vapply(
    data,
    mode_count,
    integer(1L)
  )]

  DescribeData[, `Mode Percent` := round(
    `Mode Count` / pmax(`Rows` - `NA Counts`, 1L) * 100,
    2
  )]

  DescribeData[, `Is Constant` := fifelse(
    !is.na(`Non-Missing Unique Values`) & `Non-Missing Unique Values` <= 1L,
    TRUE,
    FALSE
  )]

  DescribeData[, `Has Missing` := `NA Counts` > 0L | `NULL Counts` > 0L | `NaN Counts` > 0L]

  DescribeData[, `Has Infinite` := `Infinite Counts` > 0L]

  DescribeData[, `First 5 Values` := vapply(
    data,
    first_values,
    character(1L)
  )]

  # ============================================================
  # Describe Data Reactable Theme Helpers
  # ============================================================

  audit_badge <- function(value, type = c("neutral", "good", "warning", "danger", "info")) {

    type <- match.arg(type)

    colors <- list(
      neutral = list(bg = "#263247", fg = "#CBD5E1", border = "#475569"),
      good    = list(bg = "#064E3B", fg = "#A7F3D0", border = "#10B981"),
      warning = list(bg = "#78350F", fg = "#FDE68A", border = "#F59E0B"),
      danger  = list(bg = "#7F1D1D", fg = "#FECACA", border = "#EF4444"),
      info    = list(bg = "#1E3A8A", fg = "#BFDBFE", border = "#3B82F6")
    )

    htmltools::tags$span(
      style = paste0(
        "display:inline-block;",
        "padding:2px 8px;",
        "border-radius:999px;",
        "font-size:11px;",
        "font-weight:700;",
        "letter-spacing:0.02em;",
        "background:", colors[[type]]$bg, ";",
        "color:", colors[[type]]$fg, ";",
        "border:1px solid ", colors[[type]]$border, ";"
      ),
      as.character(value)
    )
  }

  fmt_num <- function(x, digits = 0L) {
    ifelse(
      is.na(x),
      NA_character_,
      formatC(x, format = "f", big.mark = ",", digits = digits)
    )
  }

  fmt_pct <- function(x, digits = 2L) {
    ifelse(
      is.na(x),
      NA_character_,
      paste0(formatC(x, format = "f", big.mark = ",", digits = digits), "%")
    )
  }

  numeric_cell_style <- function(value) {
    list(
      textAlign = "right",
      fontVariantNumeric = "tabular-nums",
      color = if (length(value) == 0 || is.na(value)) "#64748B" else "#E2E8F0"
    )
  }

  problem_count_cell <- function(value) {
    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    if (value > 0) {
      audit_badge(fmt_num(value, 0L), "danger")
    } else {
      htmltools::tags$span(style = "color:#64748B;", "0")
    }
  }

  percent_cell <- function(value, danger_cutoff = 25, warning_cutoff = 5) {
    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    type <- if (value >= danger_cutoff) {
      "danger"
    } else if (value >= warning_cutoff) {
      "warning"
    } else if (value > 0) {
      "info"
    } else {
      "neutral"
    }

    audit_badge(fmt_pct(value, 2L), type)
  }

  bool_cell <- function(value) {
    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    if (isTRUE(value)) {
      audit_badge("Yes", "danger")
    } else {
      audit_badge("No", "good")
    }
  }

  role_cell <- function(value) {
    type <- switch(
      as.character(value),
      "Possible ID" = "warning",
      "Continuous / Possible ID" = "warning",
      "High-Cardinality Text" = "warning",
      "Numeric-like Text" = "info",
      "Date-like Text" = "info",
      "Date/Time" = "info",
      "List Column" = "warning",
      "Binary Numeric" = "neutral",
      "Binary Categorical" = "neutral",
      "Numeric" = "good",
      "Categorical" = "good",
      "Logical" = "neutral",
      "neutral"
    )

    audit_badge(value, type)
  }

  type_cell <- function(value) {
    type <- switch(
      as.character(value),
      "numeric" = "good",
      "integer" = "good",
      "character" = "info",
      "factor" = "info",
      "Date" = "info",
      "IDate" = "info",
      "POSIXct" = "info",
      "logical" = "neutral",
      "list" = "warning",
      "neutral"
    )

    audit_badge(value, type)
  }

  sample_values_cell <- function(value) {
    htmltools::tags$span(
      title = as.character(value),
      style = paste0(
        "display:block;",
        "max-width:420px;",
        "overflow:hidden;",
        "text-overflow:ellipsis;",
        "white-space:nowrap;",
        "color:#CBD5E1;",
        "font-family:ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;",
        "font-size:12px;"
      ),
      as.character(value)
    )
  }

  # ============================================================
  # Describe Data Table
  # ============================================================

  Describe_Data <- reactable::reactable(
    data = DescribeData,

    # Core functionality
    compact = TRUE,
    defaultPageSize = 15,
    pageSizeOptions = c(10, 15, 25, 50, 100),
    wrap = FALSE,
    filterable = TRUE,
    fullWidth = TRUE,
    highlight = TRUE,
    pagination = TRUE,
    resizable = TRUE,
    searchable = TRUE,
    selection = "multiple",
    showPagination = TRUE,
    showPageSizeOptions = TRUE,
    showSortable = TRUE,
    showSortIcon = TRUE,
    sortable = TRUE,
    striped = TRUE,
    bordered = FALSE,
    outlined = FALSE,

    # Quality of life
    defaultColDef = reactable::colDef(
      minWidth = 110,
      headerStyle = list(
        background = "#0F1B33",
        color = "#E5E7EB",
        fontWeight = "800",
        borderBottom = "1px solid #334155",
        textTransform = "uppercase",
        letterSpacing = "0.045em",
        fontSize = "11px"
      ),
      style = list(
        borderBottom = "1px solid rgba(148, 163, 184, 0.14)"
      )
    ),

    columns = list(

      `Variable Name` = reactable::colDef(
        name = "Variable",
        sticky = "left",
        minWidth = 220,
        filterable = TRUE,
        style = list(
          position = "sticky",
          left = 0,
          zIndex = 1,
          background = "#101B2F",
          color = "#F8FAFC",
          fontWeight = "800",
          fontFamily = "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace"
        ),
        headerStyle = list(
          position = "sticky",
          left = 0,
          zIndex = 2,
          background = "#0B1326",
          color = "#F8FAFC",
          fontWeight = "900",
          borderBottom = "1px solid #334155"
        )
      ),

      `Variable Type` = reactable::colDef(
        name = "Type",
        minWidth = 125,
        cell = type_cell,
        align = "center"
      ),

      `Column Role Guess` = reactable::colDef(
        name = "Role Guess",
        minWidth = 185,
        cell = role_cell,
        align = "center"
      ),

      Rows = reactable::colDef(
        minWidth = 95,
        align = "right",
        cell = function(value) fmt_num(value, 0L),
        style = numeric_cell_style
      ),

      `NULL Counts` = reactable::colDef(
        name = "NULL",
        minWidth = 95,
        align = "center",
        cell = problem_count_cell
      ),

      `NA Counts` = reactable::colDef(
        name = "NA",
        minWidth = 95,
        align = "center",
        cell = problem_count_cell
      ),

      `NA Percent` = reactable::colDef(
        name = "NA %",
        minWidth = 105,
        align = "center",
        cell = function(value) percent_cell(value, danger_cutoff = 25, warning_cutoff = 5)
      ),

      `NaN Counts` = reactable::colDef(
        name = "NaN",
        minWidth = 95,
        align = "center",
        cell = problem_count_cell
      ),

      `Infinite Counts` = reactable::colDef(
        name = "Inf",
        minWidth = 95,
        align = "center",
        cell = problem_count_cell
      ),

      `Empty Counts` = reactable::colDef(
        name = "Empty",
        minWidth = 95,
        align = "center",
        cell = problem_count_cell
      ),

      `Unique Values` = reactable::colDef(
        name = "Unique",
        minWidth = 105,
        align = "right",
        cell = function(value) fmt_num(value, 0L),
        style = numeric_cell_style
      ),

      `Non-Missing Unique Values` = reactable::colDef(
        name = "Unique Non-NA",
        minWidth = 135,
        align = "right",
        cell = function(value) fmt_num(value, 0L),
        style = numeric_cell_style
      ),

      `Unique Percent` = reactable::colDef(
        name = "Unique %",
        minWidth = 115,
        align = "center",
        cell = function(value) {
          if (is.na(value)) {
            return(htmltools::tags$span(style = "color:#64748B;", "—"))
          }

          type <- if (value >= 90) {
            "warning"
          } else if (value <= 1) {
            "warning"
          } else {
            "neutral"
          }

          audit_badge(fmt_pct(value, 2L), type)
        }
      ),

      Mode = reactable::colDef(
        minWidth = 160,
        cell = function(value) {
          htmltools::tags$span(
            title = as.character(value),
            style = paste0(
              "display:block;",
              "max-width:150px;",
              "overflow:hidden;",
              "text-overflow:ellipsis;",
              "white-space:nowrap;",
              "color:#CBD5E1;",
              "font-family:ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;",
              "font-size:12px;"
            ),
            ifelse(is.na(value), "—", as.character(value))
          )
        }
      ),

      `Mode Count` = reactable::colDef(
        name = "Mode N",
        minWidth = 105,
        align = "right",
        cell = function(value) fmt_num(value, 0L),
        style = numeric_cell_style
      ),

      `Mode Percent` = reactable::colDef(
        name = "Mode %",
        minWidth = 115,
        align = "center",
        cell = function(value) {
          if (is.na(value)) {
            return(htmltools::tags$span(style = "color:#64748B;", "—"))
          }

          type <- if (value >= 95) {
            "danger"
          } else if (value >= 75) {
            "warning"
          } else {
            "neutral"
          }

          audit_badge(fmt_pct(value, 2L), type)
        }
      ),

      `Is Constant` = reactable::colDef(
        name = "Constant",
        minWidth = 105,
        align = "center",
        cell = bool_cell
      ),

      `Has Missing` = reactable::colDef(
        name = "Missing",
        minWidth = 105,
        align = "center",
        cell = bool_cell
      ),

      `Has Infinite` = reactable::colDef(
        name = "Infinite",
        minWidth = 105,
        align = "center",
        cell = bool_cell
      ),

      `First 5 Values` = reactable::colDef(
        name = "Sample Values",
        minWidth = 430,
        cell = sample_values_cell
      )
    ),

    theme = reactable::reactableTheme(
      color = "#E2E8F0",
      backgroundColor = "#0B1326",
      borderColor = "rgba(148, 163, 184, 0.22)",

      stripedColor = "rgba(30, 41, 59, 0.78)",
      highlightColor = "rgba(59, 130, 246, 0.18)",

      rowSelectedStyle = list(
        backgroundColor = "rgba(37, 99, 235, 0.28)",
        boxShadow = "inset 3px 0 0 #60A5FA"
      ),

      cellPadding = "9px 12px",

      style = list(
        fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Inter, Helvetica, Arial, sans-serif",
        fontSize = "13px",
        border = "1px solid rgba(148, 163, 184, 0.22)",
        borderRadius = "14px",
        overflow = "hidden",
        boxShadow = "0 18px 45px rgba(0, 0, 0, 0.28)"
      ),

      tableStyle = list(
        borderCollapse = "separate",
        borderSpacing = "0"
      ),

      headerStyle = list(
        background = "#0F1B33",
        color = "#F8FAFC",
        borderBottom = "1px solid rgba(148, 163, 184, 0.28)"
      ),

      rowStyle = list(
        backgroundColor = "#0B1326"
      ),

      searchInputStyle = list(
        width = "100%",
        backgroundColor = "#111C33",
        color = "#E2E8F0",
        border = "1px solid #334155",
        borderRadius = "10px",
        padding = "8px 12px",
        outline = "none"
      ),

      filterInputStyle = list(
        backgroundColor = "#111C33",
        color = "#E2E8F0",
        border = "1px solid #334155",
        borderRadius = "8px",
        padding = "5px 8px",
        outline = "none"
      ),

      selectStyle = list(
        backgroundColor = "#111C33",
        color = "#E2E8F0",
        border = "1px solid #60A5FA",
        borderRadius = "10px",
        padding = "6px 10px",
        minWidth = "76px",
        height = "34px",
        fontSize = "13px",
        fontWeight = "800",
        lineHeight = "1.2",
        cursor = "pointer",
        outline = "none",
        boxShadow = paste(
          "inset 0 1px 0 rgba(255, 255, 255, 0.06),",
          "0 0 0 1px rgba(96, 165, 250, 0.14),",
          "0 8px 18px rgba(0, 0, 0, 0.24)"
        )
      )
    )
  )



  # ============================================================
  # Univariate Analysis
  # ============================================================

  # ----------------------------
  # Helpers
  # ----------------------------

  is_numeric_col <- function(x) {
    class(x)[1L] %in% c("numeric", "integer")
  }

  is_categorical_col <- function(x) {
    class(x)[1L] %in% c("character", "factor", "logical")
  }

  safe_min <- function(x) {
    x <- x[!is.na(x)]
    x <- x[is.finite(x)]

    if (!length(x)) {
      return(NA_real_)
    }

    min(x)
  }

  safe_max <- function(x) {
    x <- x[!is.na(x)]
    x <- x[is.finite(x)]

    if (!length(x)) {
      return(NA_real_)
    }

    max(x)
  }

  safe_mean <- function(x) {
    x <- x[!is.na(x)]
    x <- x[is.finite(x)]

    if (!length(x)) {
      return(NA_real_)
    }

    mean(x)
  }

  safe_median <- function(x) {
    x <- x[!is.na(x)]
    x <- x[is.finite(x)]

    if (!length(x)) {
      return(NA_real_)
    }

    stats::median(x)
  }

  safe_sd <- function(x) {
    x <- x[!is.na(x)]
    x <- x[is.finite(x)]

    if (length(x) < 2L) {
      return(NA_real_)
    }

    stats::sd(x)
  }

  safe_iqr <- function(x) {
    x <- x[!is.na(x)]
    x <- x[is.finite(x)]

    if (!length(x)) {
      return(NA_real_)
    }

    stats::IQR(x)
  }

  safe_mad <- function(x) {
    x <- x[!is.na(x)]
    x <- x[is.finite(x)]

    if (!length(x)) {
      return(NA_real_)
    }

    stats::mad(x)
  }

  safe_skewness <- function(x) {
    x <- x[!is.na(x)]
    x <- x[is.finite(x)]

    if (length(x) < 3L || isTRUE(all.equal(stats::sd(x), 0))) {
      return(NA_real_)
    }

    e1071::skewness(x, na.rm = TRUE)
  }

  safe_kurtosis <- function(x) {
    x <- x[!is.na(x)]
    x <- x[is.finite(x)]

    if (length(x) < 4L || isTRUE(all.equal(stats::sd(x), 0))) {
      return(NA_real_)
    }

    e1071::kurtosis(x, na.rm = TRUE)
  }

  safe_cv <- function(x) {
    m <- safe_mean(x)
    s <- safe_sd(x)

    if (is.na(m) || is.na(s) || isTRUE(all.equal(m, 0))) {
      return(NA_real_)
    }

    s / m
  }

  safe_quantile <- function(x, p) {
    x <- x[!is.na(x)]
    x <- x[is.finite(x)]

    if (!length(x)) {
      return(NA_real_)
    }

    as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 7))
  }

  outlier_count_iqr <- function(x) {
    x <- x[!is.na(x)]
    x <- x[is.finite(x)]

    if (length(x) < 4L) {
      return(NA_integer_)
    }

    q1 <- safe_quantile(x, 0.25)
    q3 <- safe_quantile(x, 0.75)
    iqr <- q3 - q1

    if (is.na(iqr) || iqr == 0) {
      return(0L)
    }

    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr

    sum(x < lower | x > upper)
  }

  zero_count <- function(x) {
    sum(!is.na(x) & x == 0)
  }

  negative_count <- function(x) {
    sum(!is.na(x) & is.finite(x) & x < 0)
  }

  positive_count <- function(x) {
    sum(!is.na(x) & is.finite(x) & x > 0)
  }

  infinite_count_numeric <- function(x) {
    sum(is.infinite(x))
  }

  nan_count_numeric <- function(x) {
    sum(is.nan(x))
  }

  fmt_num <- function(x, digits = 2L) {
    ifelse(
      is.na(x),
      NA_character_,
      formatC(x, format = "f", big.mark = ",", digits = digits)
    )
  }

  fmt_int <- function(x) {
    ifelse(
      is.na(x),
      NA_character_,
      formatC(x, format = "d", big.mark = ",")
    )
  }

  fmt_pct <- function(x, digits = 2L) {
    ifelse(
      is.na(x),
      NA_character_,
      paste0(formatC(x, format = "f", big.mark = ",", digits = digits), "%")
    )
  }

  stat_badge <- function(value, type = c("neutral", "good", "warning", "danger", "info")) {

    type <- match.arg(type)

    colors <- list(
      neutral = list(bg = "#263247", fg = "#CBD5E1", border = "#475569"),
      good    = list(bg = "#064E3B", fg = "#A7F3D0", border = "#10B981"),
      warning = list(bg = "#78350F", fg = "#FDE68A", border = "#F59E0B"),
      danger  = list(bg = "#7F1D1D", fg = "#FECACA", border = "#EF4444"),
      info    = list(bg = "#1E3A8A", fg = "#BFDBFE", border = "#3B82F6")
    )

    htmltools::tags$span(
      style = paste0(
        "display:inline-block;",
        "padding:2px 8px;",
        "border-radius:999px;",
        "font-size:11px;",
        "font-weight:700;",
        "letter-spacing:0.02em;",
        "background:", colors[[type]]$bg, ";",
        "color:", colors[[type]]$fg, ";",
        "border:1px solid ", colors[[type]]$border, ";"
      ),
      as.character(value)
    )
  }

  numeric_style <- function(value) {
    list(
      textAlign = "right",
      fontVariantNumeric = "tabular-nums",
      color = if (is.na(value)) "#64748B" else "#E2E8F0"
    )
  }

  numeric_cell <- function(value, digits = 2L) {
    if (is.na(value)) {
      htmltools::tags$span(style = "color:#64748B;", "—")
    } else {
      htmltools::tags$span(
        style = paste0(
          "font-variant-numeric:tabular-nums;",
          "color:#E2E8F0;"
        ),
        fmt_num(value, digits)
      )
    }
  }

  integer_cell <- function(value) {
    if (is.na(value)) {
      htmltools::tags$span(style = "color:#64748B;", "—")
    } else {
      htmltools::tags$span(
        style = paste0(
          "font-variant-numeric:tabular-nums;",
          "color:#E2E8F0;"
        ),
        fmt_int(value)
      )
    }
  }

  percent_badge_cell <- function(value, danger_cutoff = 25, warning_cutoff = 5) {
    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    type <- if (value >= danger_cutoff) {
      "danger"
    } else if (value >= warning_cutoff) {
      "warning"
    } else if (value > 0) {
      "info"
    } else {
      "neutral"
    }

    stat_badge(fmt_pct(value, 2L), type)
  }

  count_badge_cell <- function(value) {
    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    if (value > 0) {
      stat_badge(fmt_int(value), "warning")
    } else {
      htmltools::tags$span(style = "color:#64748B;", "0")
    }
  }

  problem_count_badge_cell <- function(value) {
    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    if (value > 0) {
      stat_badge(fmt_int(value), "danger")
    } else {
      htmltools::tags$span(style = "color:#64748B;", "0")
    }
  }

  skewness_cell <- function(value) {
    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    abs_value <- abs(value)

    type <- if (abs_value >= 2) {
      "danger"
    } else if (abs_value >= 1) {
      "warning"
    } else {
      "neutral"
    }

    stat_badge(fmt_num(value, 2L), type)
  }

  kurtosis_cell <- function(value) {
    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    type <- if (abs(value) >= 7) {
      "danger"
    } else if (abs(value) >= 3) {
      "warning"
    } else {
      "neutral"
    }

    stat_badge(fmt_num(value, 2L), type)
  }

  cv_cell <- function(value) {
    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    type <- if (abs(value) >= 2) {
      "danger"
    } else if (abs(value) >= 1) {
      "warning"
    } else {
      "neutral"
    }

    stat_badge(fmt_num(value, 2L), type)
  }

  bool_badge_cell <- function(value) {
    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    if (isTRUE(value)) {
      stat_badge("Yes", "danger")
    } else {
      stat_badge("No", "good")
    }
  }


  # ----------------------------
  # Variable Selection
  # ----------------------------

  if (is.null(UnivariateVars)) {
    UnivariateVars_Valid <- names(data)
  } else {
    UnivariateVars_Valid <- intersect(UnivariateVars, names(data))
  }

  UnivariateVars_Missing <- if (is.null(UnivariateVars)) {
    character(0L)
  } else {
    setdiff(UnivariateVars, names(data))
  }

  UnivariateNumericVars <- UnivariateVars_Valid[
    vapply(data[, ..UnivariateVars_Valid], is_numeric_col, logical(1L))
  ]

  UnivariateCategoricalVars <- UnivariateVars_Valid[
    vapply(data[, ..UnivariateVars_Valid], is_categorical_col, logical(1L))
  ]

  UnivariateSkippedVars <- setdiff(
    UnivariateVars_Valid,
    c(UnivariateNumericVars, UnivariateCategoricalVars)
  )


  # ----------------------------
  # Numeric Univariate Stats
  # ----------------------------

  if (length(UnivariateNumericVars) > 0L) {

    UnivariateStats <- data.table::rbindlist(lapply(UnivariateNumericVars, function(v) {

      x <- data[[v]]

      n_total <- length(x)
      n_na <- sum(is.na(x))
      n_nan <- nan_count_numeric(x)
      n_inf <- infinite_count_numeric(x)
      n_finite <- sum(!is.na(x) & is.finite(x))
      n_zero <- zero_count(x)
      n_negative <- negative_count(x)
      n_positive <- positive_count(x)
      n_outlier <- outlier_count_iqr(x)

      q01 <- safe_quantile(x, 0.01)
      q05 <- safe_quantile(x, 0.05)
      q10 <- safe_quantile(x, 0.10)
      q25 <- safe_quantile(x, 0.25)
      q50 <- safe_quantile(x, 0.50)
      q75 <- safe_quantile(x, 0.75)
      q90 <- safe_quantile(x, 0.90)
      q95 <- safe_quantile(x, 0.95)
      q99 <- safe_quantile(x, 0.99)

      data.table::data.table(
        Variable = v,
        Type = class(x)[1L],
        Rows = n_total,
        `Valid N` = n_finite,
        `NA N` = n_na,
        `NA %` = ifelse(n_total > 0L, n_na / n_total * 100, NA_real_),
        `NaN N` = n_nan,
        `Inf N` = n_inf,
        `Zero N` = n_zero,
        `Zero %` = ifelse(n_total > 0L, n_zero / n_total * 100, NA_real_),
        `Negative N` = n_negative,
        `Positive N` = n_positive,
        `Unique N` = data.table::uniqueN(x, na.rm = FALSE),
        `Unique %` = ifelse(n_total > 0L, data.table::uniqueN(x, na.rm = FALSE) / n_total * 100, NA_real_),
        Min = safe_min(x),
        q01 = q01,
        q05 = q05,
        q10 = q10,
        q25 = q25,
        Median = q50,
        Mean = safe_mean(x),
        q75 = q75,
        q90 = q90,
        q95 = q95,
        q99 = q99,
        Max = safe_max(x),
        Range = safe_max(x) - safe_min(x),
        IQR = q75 - q25,
        MAD = safe_mad(x),
        `Standard Deviation` = safe_sd(x),
        `Coef of Variation` = safe_cv(x),
        Skewness = safe_skewness(x),
        Kurtosis = safe_kurtosis(x),
        `IQR Outlier N` = n_outlier,
        `IQR Outlier %` = ifelse(n_finite > 0L, n_outlier / n_finite * 100, NA_real_),
        `Is Constant` = data.table::uniqueN(x, na.rm = TRUE) <= 1L,
        `Has Missing` = n_na > 0L,
        `Has Infinite` = n_inf > 0L
      )
    }), fill = TRUE)

  } else {

    UnivariateStats <- data.table::data.table(
      Variable = character(),
      Type = character(),
      Rows = integer(),
      `Valid N` = integer(),
      `NA N` = integer(),
      `NA %` = numeric(),
      `NaN N` = integer(),
      `Inf N` = integer(),
      `Zero N` = integer(),
      `Zero %` = numeric(),
      `Negative N` = integer(),
      `Positive N` = integer(),
      `Unique N` = integer(),
      `Unique %` = numeric(),
      Min = numeric(),
      q01 = numeric(),
      q05 = numeric(),
      q10 = numeric(),
      q25 = numeric(),
      Median = numeric(),
      Mean = numeric(),
      q75 = numeric(),
      q90 = numeric(),
      q95 = numeric(),
      q99 = numeric(),
      Max = numeric(),
      Range = numeric(),
      IQR = numeric(),
      MAD = numeric(),
      `Standard Deviation` = numeric(),
      `Coef of Variation` = numeric(),
      Skewness = numeric(),
      Kurtosis = numeric(),
      `IQR Outlier N` = integer(),
      `IQR Outlier %` = numeric(),
      `Is Constant` = logical(),
      `Has Missing` = logical(),
      `Has Infinite` = logical()
    )
  }


  # ----------------------------
  # Optional Input QA Table
  # ----------------------------

  UnivariateInputQA <- data.table::data.table(
    Metric = c(
      "Requested Variables",
      "Valid Variables",
      "Missing Variables",
      "Numeric Variables",
      "Categorical Variables",
      "Skipped Variables"
    ),
    Count = c(
      ifelse(is.null(UnivariateVars), length(names(data)), length(UnivariateVars)),
      length(UnivariateVars_Valid),
      length(UnivariateVars_Missing),
      length(UnivariateNumericVars),
      length(UnivariateCategoricalVars),
      length(UnivariateSkippedVars)
    ),
    Values = c(
      ifelse(is.null(UnivariateVars), "All columns", paste(UnivariateVars, collapse = ", ")),
      paste(UnivariateVars_Valid, collapse = ", "),
      paste(UnivariateVars_Missing, collapse = ", "),
      paste(UnivariateNumericVars, collapse = ", "),
      paste(UnivariateCategoricalVars, collapse = ", "),
      paste(UnivariateSkippedVars, collapse = ", ")
    )
  )


  # ----------------------------
  # Univariate Stats Reactable
  # ----------------------------

  Univariate_Stats <- reactable::reactable(
    data = UnivariateStats,

    compact = TRUE,
    defaultPageSize = 15,
    pageSizeOptions = c(10, 15, 25, 50, 100),
    wrap = FALSE,
    filterable = TRUE,
    fullWidth = TRUE,
    highlight = TRUE,
    pagination = TRUE,
    resizable = TRUE,
    searchable = TRUE,
    selection = "multiple",
    showPagination = TRUE,
    showPageSizeOptions = TRUE,
    showSortable = TRUE,
    showSortIcon = TRUE,
    sortable = TRUE,
    striped = TRUE,
    bordered = FALSE,
    outlined = FALSE,

    defaultColDef = reactable::colDef(
      minWidth = 105,
      headerStyle = list(
        background = "#0F1B33",
        color = "#E5E7EB",
        fontWeight = "800",
        borderBottom = "1px solid #334155",
        textTransform = "uppercase",
        letterSpacing = "0.045em",
        fontSize = "11px"
      ),
      style = list(
        borderBottom = "1px solid rgba(148, 163, 184, 0.14)"
      )
    ),

    columns = list(

      Variable = reactable::colDef(
        sticky = "left",
        minWidth = 220,
        filterable = TRUE,
        style = list(
          position = "sticky",
          left = 0,
          zIndex = 1,
          background = "#101B2F",
          color = "#F8FAFC",
          fontWeight = "800",
          fontFamily = "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace"
        ),
        headerStyle = list(
          position = "sticky",
          left = 0,
          zIndex = 2,
          background = "#0B1326",
          color = "#F8FAFC",
          fontWeight = "900",
          borderBottom = "1px solid #334155"
        )
      ),

      Type = reactable::colDef(
        minWidth = 105,
        align = "center",
        cell = function(value) stat_badge(value, ifelse(value == "integer", "info", "good"))
      ),

      Rows = reactable::colDef(
        minWidth = 95,
        align = "right",
        cell = integer_cell,
        style = numeric_style
      ),

      `Valid N` = reactable::colDef(
        minWidth = 105,
        align = "right",
        cell = integer_cell,
        style = numeric_style
      ),

      `NA N` = reactable::colDef(
        minWidth = 95,
        align = "center",
        cell = problem_count_badge_cell
      ),

      `NA %` = reactable::colDef(
        minWidth = 95,
        align = "center",
        cell = function(value) percent_badge_cell(value, danger_cutoff = 25, warning_cutoff = 5)
      ),

      `NaN N` = reactable::colDef(
        minWidth = 95,
        align = "center",
        cell = problem_count_badge_cell
      ),

      `Inf N` = reactable::colDef(
        minWidth = 95,
        align = "center",
        cell = problem_count_badge_cell
      ),

      `Zero N` = reactable::colDef(
        minWidth = 95,
        align = "center",
        cell = count_badge_cell
      ),

      `Zero %` = reactable::colDef(
        minWidth = 95,
        align = "center",
        cell = function(value) percent_badge_cell(value, danger_cutoff = 75, warning_cutoff = 25)
      ),

      `Negative N` = reactable::colDef(
        minWidth = 110,
        align = "center",
        cell = count_badge_cell
      ),

      `Positive N` = reactable::colDef(
        minWidth = 110,
        align = "right",
        cell = integer_cell,
        style = numeric_style
      ),

      `Unique N` = reactable::colDef(
        minWidth = 105,
        align = "right",
        cell = integer_cell,
        style = numeric_style
      ),

      `Unique %` = reactable::colDef(
        minWidth = 105,
        align = "center",
        cell = function(value) {
          if (is.na(value)) {
            return(htmltools::tags$span(style = "color:#64748B;", "—"))
          }

          type <- if (value >= 90) {
            "warning"
          } else if (value <= 1) {
            "warning"
          } else {
            "neutral"
          }

          stat_badge(fmt_pct(value, 2L), type)
        }
      ),

      Min = reactable::colDef(
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      q01 = reactable::colDef(
        name = "P01",
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      q05 = reactable::colDef(
        name = "P05",
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      q10 = reactable::colDef(
        name = "P10",
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      q25 = reactable::colDef(
        name = "P25",
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      Median = reactable::colDef(
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      Mean = reactable::colDef(
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      q75 = reactable::colDef(
        name = "P75",
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      q90 = reactable::colDef(
        name = "P90",
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      q95 = reactable::colDef(
        name = "P95",
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      q99 = reactable::colDef(
        name = "P99",
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      Max = reactable::colDef(
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      Range = reactable::colDef(
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      IQR = reactable::colDef(
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      MAD = reactable::colDef(
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      `Standard Deviation` = reactable::colDef(
        name = "SD",
        minWidth = 110,
        align = "right",
        cell = function(value) numeric_cell(value, 2L),
        style = numeric_style
      ),

      `Coef of Variation` = reactable::colDef(
        name = "CV",
        minWidth = 105,
        align = "center",
        cell = cv_cell
      ),

      Skewness = reactable::colDef(
        minWidth = 115,
        align = "center",
        cell = skewness_cell
      ),

      Kurtosis = reactable::colDef(
        minWidth = 115,
        align = "center",
        cell = kurtosis_cell
      ),

      `IQR Outlier N` = reactable::colDef(
        name = "Outlier N",
        minWidth = 115,
        align = "center",
        cell = count_badge_cell
      ),

      `IQR Outlier %` = reactable::colDef(
        name = "Outlier %",
        minWidth = 115,
        align = "center",
        cell = function(value) percent_badge_cell(value, danger_cutoff = 10, warning_cutoff = 2)
      ),

      `Is Constant` = reactable::colDef(
        name = "Constant",
        minWidth = 105,
        align = "center",
        cell = bool_badge_cell
      ),

      `Has Missing` = reactable::colDef(
        name = "Missing",
        minWidth = 105,
        align = "center",
        cell = bool_badge_cell
      ),

      `Has Infinite` = reactable::colDef(
        name = "Infinite",
        minWidth = 105,
        align = "center",
        cell = bool_badge_cell
      )
    ),

    theme = reactable::reactableTheme(
      color = "#E2E8F0",
      backgroundColor = "#0B1326",
      borderColor = "rgba(148, 163, 184, 0.22)",

      stripedColor = "rgba(30, 41, 59, 0.78)",
      highlightColor = "rgba(59, 130, 246, 0.18)",

      rowSelectedStyle = list(
        backgroundColor = "rgba(37, 99, 235, 0.28)",
        boxShadow = "inset 3px 0 0 #60A5FA"
      ),

      cellPadding = "9px 12px",

      style = list(
        fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Inter, Helvetica, Arial, sans-serif",
        fontSize = "13px",
        border = "1px solid rgba(148, 163, 184, 0.22)",
        borderRadius = "14px",
        overflow = "hidden",
        boxShadow = "0 18px 45px rgba(0, 0, 0, 0.28)"
      ),

      tableStyle = list(
        borderCollapse = "separate",
        borderSpacing = "0"
      ),

      headerStyle = list(
        background = "#0F1B33",
        color = "#F8FAFC",
        borderBottom = "1px solid rgba(148, 163, 184, 0.28)"
      ),

      rowStyle = list(
        backgroundColor = "#0B1326"
      ),

      searchInputStyle = list(
        width = "100%",
        backgroundColor = "#111C33",
        color = "#E2E8F0",
        border = "1px solid #334155",
        borderRadius = "10px",
        padding = "8px 12px",
        outline = "none"
      ),

      filterInputStyle = list(
        backgroundColor = "#111C33",
        color = "#E2E8F0",
        border = "1px solid #334155",
        borderRadius = "8px",
        padding = "5px 8px",
        outline = "none"
      ),

      selectStyle = list(
        backgroundColor = "#111C33",
        color = "#E2E8F0",
        border = "1px solid #60A5FA",
        borderRadius = "10px",
        padding = "6px 10px",
        minWidth = "76px",
        height = "34px",
        fontSize = "13px",
        fontWeight = "800",
        lineHeight = "1.2",
        cursor = "pointer",
        outline = "none",
        boxShadow = paste(
          "inset 0 1px 0 rgba(255, 255, 255, 0.06),",
          "0 0 0 1px rgba(96, 165, 250, 0.14),",
          "0 8px 18px rgba(0, 0, 0, 0.24)"
        )
      )
    )
  )


  # ============================================================
  # Univariate Plots
  # ============================================================

  # ----------------------------
  # Plot settings
  # ----------------------------

  MaxCategoricalLevels <- as.integer(MaxCategoricalLevels)
  MaxDiscreteNumericLevels <- as.integer(MaxDiscreteNumericLevels)

  has_trend_group <- !is.null(TrendGroupVar) &&
    length(TrendGroupVar) == 1L &&
    !is.na(TrendGroupVar) &&
    nzchar(TrendGroupVar) &&
    TrendGroupVar %in% names(data)

  # ----------------------------
  # Defensive variable selection for plots
  # ----------------------------

  if (!exists("UnivariateVars") || is.null(UnivariateVars)) {
    UnivariateVars_PlotRequested <- names(data)
  } else {
    UnivariateVars_PlotRequested <- as.character(UnivariateVars)
    UnivariateVars_PlotRequested <- trimws(UnivariateVars_PlotRequested)
    UnivariateVars_PlotRequested <- UnivariateVars_PlotRequested[
      !is.na(UnivariateVars_PlotRequested) &
        nzchar(UnivariateVars_PlotRequested)
    ]

    if (length(UnivariateVars_PlotRequested) == 0L) {
      UnivariateVars_PlotRequested <- names(data)
    }
  }

  UnivariateVars_PlotValid <- intersect(UnivariateVars_PlotRequested, names(data))
  UnivariateVars_PlotMissing <- setdiff(UnivariateVars_PlotRequested, names(data))

  UnivariateNumericVars <- UnivariateVars_PlotValid[
    vapply(
      UnivariateVars_PlotValid,
      function(v) {
        is.numeric(data[[v]]) &&
          !inherits(data[[v]], c("Date", "POSIXct", "POSIXlt", "IDate", "ITime"))
      },
      logical(1L)
    )
  ]

  UnivariateCategoricalVars <- UnivariateVars_PlotValid[
    vapply(
      UnivariateVars_PlotValid,
      function(v) {
        is.character(data[[v]]) ||
          is.factor(data[[v]]) ||
          is.logical(data[[v]])
      },
      logical(1L)
    )
  ]

  UnivariatePlotSkippedVars <- setdiff(
    UnivariateVars_PlotValid,
    c(UnivariateNumericVars, UnivariateCategoricalVars)
  )

  # ----------------------------
  # Plot storage lists
  # ----------------------------

  UnivariatePlotList <- list()
  UnivariateCharacterPlotList <- list()

  UnivariateHistogramPlotList <- list()
  UnivariateBoxPlotList <- list()
  UnivariateGroupedBoxPlotList <- list()
  UnivariateDiscreteNumericPlotList <- list()
  UnivariateCategoricalTopNPlotList <- list()

  UnivariatePlots <- list(
    Histograms = list(),
    BoxPlots = list(),
    GroupedBoxPlots = list(),
    DiscreteNumericBarPlots = list(),
    CategoricalTopNBarPlots = list()
  )

  # ----------------------------
  # Numeric Histograms
  # ----------------------------

  if (length(UnivariateNumericVars) > 0L) {

    for (i in UnivariateNumericVars) {

      data_plot <- data[
        !is.na(get(i)) &
          is.finite(get(i))
      ]

      if (nrow(data_plot) > 0L) {

        UnivariateHistogramPlotList[[i]] <- AutoPlots::Histogram(
          dt = data_plot,
          XVar = i,
          title.text = paste0(i, " Distribution"),
          xAxis.title = i,
          yAxis.title = "Count",
          Theme = Theme
        )

        UnivariatePlots$Histograms[[i]] <- UnivariateHistogramPlotList[[i]]
      }
    }
  }

  # ----------------------------
  # Numeric Box Plots - All Data
  # ----------------------------

  if (length(UnivariateNumericVars) > 0L) {

    for (i in UnivariateNumericVars) {

      data_box <- data[
        !is.na(get(i)) &
          is.finite(get(i))
      ]

      if (nrow(data_box) > 0L) {

        data_box <- data.table::copy(data_box)
        data_box[, `.All Data` := "All Data"]

        UnivariateBoxPlotList[[i]] <- AutoPlots::Box(
          dt = data_box,
          XVar = ".All Data",
          YVar = i,
          title.text = paste0(i, " Box Plot"),
          yAxis.title = i,
          xAxis.title = NULL,
          Theme = Theme,
          tooltip.show = FALSE
        )

        UnivariatePlotList[[i]] <- UnivariateBoxPlotList[[i]]
        UnivariatePlots$BoxPlots[[i]] <- UnivariateBoxPlotList[[i]]
      }
    }
  }

  # ----------------------------
  # Numeric Box Plots - Grouped
  # ----------------------------

  if (has_trend_group && length(UnivariateNumericVars) > 0L) {

    for (i in UnivariateNumericVars) {

      data_grouped_box <- data[
        !is.na(get(i)) &
          is.finite(get(i)) &
          !is.na(get(TrendGroupVar))
      ]

      if (nrow(data_grouped_box) > 0L) {

        UnivariateGroupedBoxPlotList[[i]] <- AutoPlots::Box(
          dt = data_grouped_box,
          XVar = TrendGroupVar,
          YVar = i,
          title.text = paste0(i, " by ", TrendGroupVar),
          yAxis.title = i,
          xAxis.title = TrendGroupVar,
          Theme = Theme,
          tooltip.show = FALSE
        ) |> echarts4r::e_flip_coords()

        UnivariatePlots$GroupedBoxPlots[[i]] <- UnivariateGroupedBoxPlotList[[i]]
      }
    }
  }

  # ----------------------------
  # Low-Cardinality Numeric Bar Plots
  # ----------------------------

  if (length(UnivariateNumericVars) > 0L) {

    for (i in UnivariateNumericVars) {

      unique_n <- data.table::uniqueN(data[[i]], na.rm = TRUE)

      if (!is.na(unique_n) && unique_n > 0L && unique_n <= MaxDiscreteNumericLevels) {

        data_discrete <- data.table::data.table(
          Value = data[[i]]
        )

        data_discrete[, Value := as.character(Value)]
        data_discrete[is.na(Value), Value := "<NA>"]

        data2 <- data_discrete[
          ,
          .(Counts = .N),
          by = Value
        ]

        data2[, Percent := Counts / sum(Counts) * 100]

        sort_value <- suppressWarnings(as.numeric(data2$Value))
        data2[, SortValue := sort_value]
        data.table::setorderv(data2, c("SortValue", "Value"), order = c(1L, 1L), na.last = TRUE)
        data2[, SortValue := NULL]

        data.table::setnames(data2, "Value", i)
        data2 <- aq_report_sort_for_flipped_bar(data2, "Counts", i)

        UnivariateDiscreteNumericPlotList[[i]] <- AutoPlots::Bar(
          dt = data2,
          XVar = i,
          YVar = "Counts",
          title.text = paste0(i, " Value Counts"),
          yAxis.title = "Count",
          xAxis.title = i,
          Theme = Theme
        ) |> echarts4r::e_flip_coords()

        UnivariatePlots$DiscreteNumericBarPlots[[i]] <- UnivariateDiscreteNumericPlotList[[i]]
      }
    }
  }

  # ----------------------------
  # Categorical Top-N Bar Plots
  # ----------------------------

  if (length(UnivariateCategoricalVars) > 0L) {

    for (i in UnivariateCategoricalVars) {

      data_cat <- data.table::data.table(
        Value = as.character(data[[i]])
      )

      data_cat[is.na(Value), Value := "<NA>"]
      data_cat[trimws(Value) == "", Value := "<EMPTY STRING>"]

      data2 <- data_cat[
        ,
        .(Counts = .N),
        by = Value
      ]

      data.table::setorderv(data2, "Counts", order = -1L)

      data2[, Percent := Counts / sum(Counts) * 100]

      if (nrow(data2) > MaxCategoricalLevels) {

        data_top <- data2[seq_len(MaxCategoricalLevels)]

        other_count <- data2[-seq_len(MaxCategoricalLevels), sum(Counts)]
        other_pct <- other_count / sum(data2$Counts) * 100

        other_row <- data.table::data.table(
          Value = "Other",
          Counts = other_count,
          Percent = other_pct
        )

        data2 <- data.table::rbindlist(
          list(data_top, other_row),
          fill = TRUE
        )
      }

      data.table::setnames(data2, "Value", i)
      data2 <- aq_report_sort_for_flipped_bar(data2, "Counts", i)

      UnivariateCategoricalTopNPlotList[[i]] <- AutoPlots::Bar(
        dt = data2,
        XVar = i,
        YVar = "Counts",
        title.text = paste0(i, " Counts"),
        yAxis.title = "Count",
        xAxis.title = i,
        Theme = Theme
      ) |> echarts4r::e_flip_coords()

      UnivariateCharacterPlotList[[i]] <- UnivariateCategoricalTopNPlotList[[i]]
      UnivariatePlots$CategoricalTopNBarPlots[[i]] <- UnivariateCategoricalTopNPlotList[[i]]
    }
  }




  HistogramPlotCols <- as.integer(HistogramPlotCols)
  BoxPlotCols <- as.integer(BoxPlotCols)
  GroupedBoxPlotCols <- as.integer(BoxPlotCols)
  DiscreteNumericBarPlotCols <- as.integer(DiscreteNumericBarPlotCols)
  CategoricalBarPlotCols <- as.integer(CategoricalBarPlotCols)


  # ============================================================
  # Correlation Analysis
  # ============================================================

  # ----------------------------
  # Correlation settings
  # ----------------------------

  CorrelationMethod <- as.character(CorrelationMethod)[1L]
  MaxCorrelationPairsToPlot <- as.integer(MaxCorrelationPairsToPlot)
  HighCorrelationThreshold <- as.numeric(HighCorrelationThreshold)[1L]


  # ============================================================
  # Safe Value Helpers
  # ============================================================

  corr_is_empty <- function(value) {
    is.null(value) ||
      length(value) == 0L ||
      all(is.na(value))
  }

  corr_is_not_empty <- function(value) {
    !corr_is_empty(value)
  }

  corr_is_positive <- function(value) {
    corr_is_not_empty(value) &&
      suppressWarnings(is.numeric(value) || is.integer(value)) &&
      length(value) == 1L &&
      !is.na(value) &&
      value > 0
  }

  corr_is_true <- function(value) {
    corr_is_not_empty(value) &&
      isTRUE(value)
  }

  corr_safe_chr <- function(value, empty_value = "—") {
    if (corr_is_empty(value)) {
      empty_value
    } else {
      as.character(value)
    }
  }

  corr_safe_num <- function(value) {
    if (corr_is_empty(value)) {
      return(NA_real_)
    }

    suppressWarnings(as.numeric(value)[1L])
  }

  corr_safe_index <- function(index) {
    if (is.null(index) || length(index) == 0L || is.na(index)) {
      return(1L)
    }

    as.integer(index[1L])
  }


  # ============================================================
  # Correlation Computation Helpers
  # ============================================================

  is_corr_numeric_col <- function(x) {
    is.numeric(x) &&
      !inherits(x, c("Date", "POSIXct", "POSIXlt", "IDate", "ITime"))
  }

  corr_valid_n <- function(x, y) {
    sum(!is.na(x) & !is.na(y) & is.finite(x) & is.finite(y))
  }

  corr_missing_n <- function(x, y) {
    sum(is.na(x) | is.na(y) | !is.finite(x) | !is.finite(y))
  }

  corr_is_constant <- function(x) {
    x <- x[!is.na(x) & is.finite(x)]

    if (length(x) <= 1L) {
      return(TRUE)
    }

    data.table::uniqueN(x) <= 1L
  }

  safe_cor <- function(x, y, method = "spearman") {
    keep <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
    x <- x[keep]
    y <- y[keep]

    if (length(x) < 3L) {
      return(NA_real_)
    }

    if (data.table::uniqueN(x) <= 1L || data.table::uniqueN(y) <= 1L) {
      return(NA_real_)
    }

    suppressWarnings(stats::cor(x, y, method = method, use = "complete.obs"))
  }

  safe_cor_p_value <- function(x, y, method = "spearman") {
    keep <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
    x <- x[keep]
    y <- y[keep]

    if (length(x) < 4L) {
      return(NA_real_)
    }

    if (data.table::uniqueN(x) <= 1L || data.table::uniqueN(y) <= 1L) {
      return(NA_real_)
    }

    out <- tryCatch(
      suppressWarnings(
        stats::cor.test(
          x = x,
          y = y,
          method = method,
          exact = FALSE
        )$p.value
      ),
      error = function(e) NA_real_
    )

    as.numeric(out)
  }

  corr_strength_label <- function(r) {
    if (is.na(r)) {
      return("Not available")
    }

    ar <- abs(r)

    if (ar >= 0.90) {
      "Very Strong"
    } else if (ar >= 0.70) {
      "Strong"
    } else if (ar >= 0.50) {
      "Moderate"
    } else if (ar >= 0.30) {
      "Weak"
    } else {
      "Very Weak"
    }
  }

  corr_direction_label <- function(r) {
    if (is.na(r)) {
      return("Not available")
    }

    if (r > 0) {
      "Positive"
    } else if (r < 0) {
      "Negative"
    } else {
      "Zero"
    }
  }

  corr_pair_name <- function(x, y) {
    paste0(x, "  ↔  ", y)
  }


  # ============================================================
  # Correlation Formatting Helpers
  # ============================================================

  corr_fmt_int <- function(x) {
    if (corr_is_empty(x)) {
      return(NA_character_)
    }

    x <- suppressWarnings(as.numeric(x))

    if (length(x) == 0L || is.na(x[1L])) {
      return(NA_character_)
    }

    formatC(round(x[1L]), format = "f", digits = 0L, big.mark = ",")
  }

  corr_fmt_pct <- function(x, digits = 2L) {
    if (corr_is_empty(x)) {
      return(NA_character_)
    }

    x <- suppressWarnings(as.numeric(x))

    if (length(x) == 0L || is.na(x[1L])) {
      return(NA_character_)
    }

    paste0(formatC(x[1L], format = "f", big.mark = ",", digits = digits), "%")
  }


  # ============================================================
  # Correlation Reactable Cell Helpers
  # ============================================================

  corr_badge <- function(value, type = c("neutral", "good", "warning", "danger", "info")) {

    type <- match.arg(type)

    colors <- list(
      neutral = list(bg = "#263247", fg = "#CBD5E1", border = "#475569"),
      good    = list(bg = "#064E3B", fg = "#A7F3D0", border = "#10B981"),
      warning = list(bg = "#78350F", fg = "#FDE68A", border = "#F59E0B"),
      danger  = list(bg = "#7F1D1D", fg = "#FECACA", border = "#EF4444"),
      info    = list(bg = "#1E3A8A", fg = "#BFDBFE", border = "#3B82F6")
    )

    htmltools::tags$span(
      style = paste0(
        "display:inline-block;",
        "padding:2px 8px;",
        "border-radius:999px;",
        "font-size:11px;",
        "font-weight:700;",
        "letter-spacing:0.02em;",
        "background:", colors[[type]]$bg, ";",
        "color:", colors[[type]]$fg, ";",
        "border:1px solid ", colors[[type]]$border, ";"
      ),
      corr_safe_chr(value)
    )
  }

  corr_numeric_cell <- function(value, digits = 3L) {
    if (corr_is_empty(value)) {
      htmltools::tags$span(style = "color:#64748B;", "—")
    } else {
      htmltools::tags$span(
        style = "font-variant-numeric:tabular-nums;color:#E2E8F0;",
        formatC(corr_safe_num(value), format = "f", big.mark = ",", digits = safe_format_digits(digits))
      )
    }
  }

  corr_integer_cell <- function(value) {
    if (corr_is_empty(value)) {
      htmltools::tags$span(style = "color:#64748B;", "—")
    } else {
      htmltools::tags$span(
        style = "font-variant-numeric:tabular-nums;color:#E2E8F0;",
        corr_fmt_int(value)
      )
    }
  }

  corr_value_cell <- function(value) {
    value <- corr_safe_num(value)

    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    ar <- abs(value)

    type <- if (ar >= 0.90) {
      "danger"
    } else if (ar >= 0.70) {
      "warning"
    } else if (ar >= 0.50) {
      "info"
    } else {
      "neutral"
    }

    corr_badge(formatC(value, format = "f", big.mark = ",", digits = 3L), type)
  }

  corr_abs_value_cell <- function(value) {
    value <- corr_safe_num(value)

    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    type <- if (value >= 0.90) {
      "danger"
    } else if (value >= 0.70) {
      "warning"
    } else if (value >= 0.50) {
      "info"
    } else {
      "neutral"
    }

    corr_badge(formatC(value, format = "f", big.mark = ",", digits = 3L), type)
  }

  corr_p_value_cell <- function(value) {
    value <- corr_safe_num(value)

    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    type <- if (value <= 0.001) {
      "danger"
    } else if (value <= 0.01) {
      "warning"
    } else if (value <= 0.05) {
      "info"
    } else {
      "neutral"
    }

    corr_badge(formatC(value, format = "f", big.mark = ",", digits = 4L), type)
  }

  corr_strength_cell <- function(value) {
    value <- corr_safe_chr(value)

    type <- switch(
      value,
      "Very Strong" = "danger",
      "Strong" = "warning",
      "Moderate" = "info",
      "Weak" = "neutral",
      "Very Weak" = "neutral",
      "neutral"
    )

    corr_badge(value, type)
  }

  corr_direction_cell <- function(value) {
    value <- corr_safe_chr(value)

    type <- switch(
      value,
      "Positive" = "good",
      "Negative" = "warning",
      "Zero" = "neutral",
      "neutral"
    )

    corr_badge(value, type)
  }

  corr_bool_cell <- function(value) {
    if (corr_is_empty(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    if (isTRUE(value)) {
      corr_badge("Yes", "danger")
    } else {
      corr_badge("No", "good")
    }
  }

  corr_positive_count_cell <- function(value) {
    if (corr_is_positive(value)) {
      corr_badge(corr_fmt_int(value), "danger")
    } else {
      htmltools::tags$span(style = "color:#64748B;", "0")
    }
  }

  corr_text_cell <- function(value) {
    htmltools::tags$span(
      style = "color:#CBD5E1;",
      corr_safe_chr(value)
    )
  }

  corr_truncated_mono_cell <- function(value) {
    htmltools::tags$span(
      title = corr_safe_chr(value, empty_value = ""),
      style = paste0(
        "display:block;",
        "max-width:720px;",
        "overflow:hidden;",
        "text-overflow:ellipsis;",
        "white-space:nowrap;",
        "color:#CBD5E1;",
        "font-family:ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;",
        "font-size:12px;"
      ),
      corr_safe_chr(value)
    )
  }


  # ============================================================
  # Correlation Reactable Style Helpers
  # ============================================================

  corr_row_bg <- function(index) {
    index <- corr_safe_index(index)

    if (index %% 2L == 0L) {
      "#0B1326"
    } else {
      "#111C33"
    }
  }

  corr_sticky_row_bg <- function(index) {
    index <- corr_safe_index(index)

    if (index %% 2L == 0L) {
      "#101B2F"
    } else {
      "#13203A"
    }
  }

  corr_dark_row_style <- function(index) {
    list(
      backgroundColor = corr_row_bg(index),
      color = "#E2E8F0"
    )
  }

  corr_dark_cell_style <- function(value, index = NULL) {
    list(
      backgroundColor = corr_row_bg(index),
      color = if (corr_is_empty(value)) "#64748B" else "#E2E8F0",
      borderBottom = "1px solid rgba(148, 163, 184, 0.14)"
    )
  }

  corr_dark_numeric_cell_style <- function(value, index = NULL) {
    list(
      backgroundColor = corr_row_bg(index),
      color = if (corr_is_empty(value)) "#64748B" else "#E2E8F0",
      textAlign = "right",
      fontVariantNumeric = "tabular-nums",
      borderBottom = "1px solid rgba(148, 163, 184, 0.14)"
    )
  }

  corr_dark_center_cell_style <- function(value, index = NULL) {
    list(
      backgroundColor = corr_row_bg(index),
      color = if (corr_is_empty(value)) "#64748B" else "#E2E8F0",
      textAlign = "center",
      borderBottom = "1px solid rgba(148, 163, 184, 0.14)"
    )
  }

  corr_dark_sticky_cell_style <- function(value, index = NULL) {
    list(
      position = "sticky",
      left = 0,
      zIndex = 1,
      backgroundColor = corr_sticky_row_bg(index),
      color = "#F8FAFC",
      fontWeight = "800",
      fontFamily = "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace",
      borderBottom = "1px solid rgba(148, 163, 184, 0.14)",
      boxShadow = "6px 0 10px rgba(0, 0, 0, 0.18)"
    )
  }

  # Backward-compatible replacement for older table code paths.
  corr_numeric_style <- function(value, index = NULL) {
    corr_dark_numeric_cell_style(value, index)
  }

  corr_center_style <- function(value, index = NULL) {
    corr_dark_center_cell_style(value, index)
  }

  corr_cell_style <- function(value, index = NULL) {
    corr_dark_cell_style(value, index)
  }

  corr_default_header_style <- list(
    background = "#0F1B33",
    color = "#E5E7EB",
    fontWeight = "800",
    borderBottom = "1px solid #334155",
    textTransform = "uppercase",
    letterSpacing = "0.045em",
    fontSize = "11px"
  )

  corr_theme <- reactable::reactableTheme(
    color = "#E2E8F0",
    backgroundColor = "#0B1326",
    borderColor = "rgba(148, 163, 184, 0.22)",
    highlightColor = "rgba(59, 130, 246, 0.18)",
    rowSelectedStyle = list(
      backgroundColor = "rgba(37, 99, 235, 0.28)",
      boxShadow = "inset 3px 0 0 #60A5FA"
    ),
    cellPadding = "9px 12px",
    style = list(
      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Inter, Helvetica, Arial, sans-serif",
      fontSize = "13px",
      border = "1px solid rgba(148, 163, 184, 0.22)",
      borderRadius = "14px",
      overflow = "hidden",
      boxShadow = "0 18px 45px rgba(0, 0, 0, 0.28)"
    ),
    tableStyle = list(
      borderCollapse = "separate",
      borderSpacing = "0"
    ),
    headerStyle = list(
      background = "#0F1B33",
      color = "#F8FAFC",
      borderBottom = "1px solid rgba(148, 163, 184, 0.28)"
    ),
    searchInputStyle = list(
      width = "100%",
      backgroundColor = "#111C33",
      color = "#E2E8F0",
      border = "1px solid #334155",
      borderRadius = "10px",
      padding = "8px 12px",
      outline = "none"
    ),
    filterInputStyle = list(
      backgroundColor = "#111C33",
      color = "#E2E8F0",
      border = "1px solid #334155",
      borderRadius = "8px",
      padding = "5px 8px",
      outline = "none"
    ),
    selectStyle = list(
      backgroundColor = "#111C33",
      color = "#E2E8F0",
      border = "1px solid #60A5FA",
      borderRadius = "10px",
      padding = "6px 10px",
      minWidth = "76px",
      height = "34px",
      fontSize = "13px",
      fontWeight = "800",
      lineHeight = "1.2",
      cursor = "pointer",
      outline = "none"
    )
  )


  # ============================================================
  # Validate Selected Correlation Variables
  # ============================================================

  if (is.null(CorrVars)) {
    CorrVars_Requested <- names(data)
  } else {
    CorrVars_Requested <- as.character(CorrVars)
    CorrVars_Requested <- trimws(CorrVars_Requested)
    CorrVars_Requested <- CorrVars_Requested[!is.na(CorrVars_Requested) & nzchar(CorrVars_Requested)]

    if (length(CorrVars_Requested) == 0L) {
      CorrVars_Requested <- names(data)
    }
  }

  CorrVars_Valid <- intersect(CorrVars_Requested, names(data))
  CorrVars_Missing <- setdiff(CorrVars_Requested, names(data))

  if (length(CorrVars_Valid) > 0L) {
    CorrVars_Numeric <- CorrVars_Valid[
      vapply(
        CorrVars_Valid,
        function(v) is_corr_numeric_col(data[[v]]),
        logical(1L)
      )
    ]
  } else {
    CorrVars_Numeric <- character(0L)
  }

  CorrVars_NonNumeric <- setdiff(CorrVars_Valid, CorrVars_Numeric)

  if (length(CorrVars_Numeric) > 0L) {
    CorrVars_Constant <- CorrVars_Numeric[
      vapply(
        CorrVars_Numeric,
        function(v) corr_is_constant(data[[v]]),
        logical(1L)
      )
    ]
  } else {
    CorrVars_Constant <- character(0L)
  }

  CorrVars_Usable <- setdiff(CorrVars_Numeric, CorrVars_Constant)

  CorrVars_Skipped <- unique(c(CorrVars_Missing, CorrVars_NonNumeric, CorrVars_Constant))


  # ============================================================
  # Correlation Input QA Table
  # ============================================================

  CorrelationInputQA <- data.table::data.table(
    Metric = c(
      "Requested Variables",
      "Valid Variables",
      "Missing Variables",
      "Numeric Variables",
      "Non-Numeric Variables",
      "Constant Numeric Variables",
      "Usable Correlation Variables",
      "Correlation Method",
      "High Correlation Threshold"
    ),
    Count = c(
      length(CorrVars_Requested),
      length(CorrVars_Valid),
      length(CorrVars_Missing),
      length(CorrVars_Numeric),
      length(CorrVars_NonNumeric),
      length(CorrVars_Constant),
      length(CorrVars_Usable),
      NA_integer_,
      NA_integer_
    ),
    Values = c(
      paste(CorrVars_Requested, collapse = ", "),
      paste(CorrVars_Valid, collapse = ", "),
      paste(CorrVars_Missing, collapse = ", "),
      paste(CorrVars_Numeric, collapse = ", "),
      paste(CorrVars_NonNumeric, collapse = ", "),
      paste(CorrVars_Constant, collapse = ", "),
      paste(CorrVars_Usable, collapse = ", "),
      CorrelationMethod,
      as.character(HighCorrelationThreshold)
    )
  )


  # ============================================================
  # Per-Variable Correlation Diagnostics
  # ============================================================

  if (length(CorrVars_Numeric) > 0L) {

    CorrelationVariableDiagnostics <- data.table::rbindlist(
      lapply(CorrVars_Numeric, function(v) {

        x <- data[[v]]
        n_total <- length(x)
        n_na <- sum(is.na(x))
        n_nan <- sum(is.nan(x))
        n_inf <- sum(is.infinite(x))
        n_finite <- sum(!is.na(x) & is.finite(x))
        unique_n <- data.table::uniqueN(x[!is.na(x) & is.finite(x)])

        data.table::data.table(
          Variable = v,
          Type = class(x)[1L],
          Rows = n_total,
          `Finite N` = n_finite,
          `NA N` = n_na,
          `NaN N` = n_nan,
          `Inf N` = n_inf,
          `Unique Finite N` = unique_n,
          `Usable For Correlation` = v %in% CorrVars_Usable,
          `Skip Reason` = if (v %in% CorrVars_Constant) {
            "Constant / insufficient variation"
          } else {
            ""
          }
        )
      }),
      fill = TRUE
    )

  } else {

    CorrelationVariableDiagnostics <- data.table::data.table(
      Variable = character(),
      Type = character(),
      Rows = integer(),
      `Finite N` = integer(),
      `NA N` = integer(),
      `NaN N` = integer(),
      `Inf N` = integer(),
      `Unique Finite N` = integer(),
      `Usable For Correlation` = logical(),
      `Skip Reason` = character()
    )
  }


  # ============================================================
  # Pairwise Correlation Stats
  # ============================================================

  CorrelationStats <- data.table::data.table(
    Variable1 = character(),
    Variable2 = character(),
    Pair = character(),
    Method = character(),
    N = integer(),
    `Missing Pair N` = integer(),
    Correlation = numeric(),
    `Abs Correlation` = numeric(),
    Direction = character(),
    Strength = character(),
    `P Value` = numeric(),
    `High Correlation` = logical()
  )

  if (length(CorrVars_Usable) >= 2L) {

    pair_matrix <- utils::combn(CorrVars_Usable, 2L)

    CorrelationStats <- data.table::rbindlist(
      lapply(seq_len(ncol(pair_matrix)), function(j) {

        x_name <- pair_matrix[1L, j]
        y_name <- pair_matrix[2L, j]

        x <- data[[x_name]]
        y <- data[[y_name]]

        valid_n <- corr_valid_n(x, y)
        missing_n <- corr_missing_n(x, y)
        r <- safe_cor(x, y, method = CorrelationMethod)
        p <- safe_cor_p_value(x, y, method = CorrelationMethod)

        data.table::data.table(
          Variable1 = x_name,
          Variable2 = y_name,
          Pair = corr_pair_name(x_name, y_name),
          Method = CorrelationMethod,
          N = valid_n,
          `Missing Pair N` = missing_n,
          Correlation = r,
          `Abs Correlation` = abs(r),
          Direction = corr_direction_label(r),
          Strength = corr_strength_label(r),
          `P Value` = p,
          `High Correlation` = !is.na(r) && abs(r) >= HighCorrelationThreshold
        )
      }),
      fill = TRUE
    )

    data.table::setorderv(
      CorrelationStats,
      cols = c("Abs Correlation", "N"),
      order = c(-1L, -1L),
      na.last = TRUE
    )
  }


  # ============================================================
  # Summary Tables for Strongest Relationships
  # ============================================================

  TopAbsCorrelationStats <- CorrelationStats[
    !is.na(`Abs Correlation`)
  ][
    order(-`Abs Correlation`)
  ]

  if (nrow(TopAbsCorrelationStats) > MaxCorrelationPairsToPlot) {
    TopAbsCorrelationStats <- TopAbsCorrelationStats[seq_len(MaxCorrelationPairsToPlot)]
  }

  TopPositiveCorrelationStats <- CorrelationStats[
    !is.na(Correlation) & Correlation > 0
  ][
    order(-Correlation)
  ]

  if (nrow(TopPositiveCorrelationStats) > MaxCorrelationPairsToPlot) {
    TopPositiveCorrelationStats <- TopPositiveCorrelationStats[seq_len(MaxCorrelationPairsToPlot)]
  }

  TopNegativeCorrelationStats <- CorrelationStats[
    !is.na(Correlation) & Correlation < 0
  ][
    order(Correlation)
  ]

  if (nrow(TopNegativeCorrelationStats) > MaxCorrelationPairsToPlot) {
    TopNegativeCorrelationStats <- TopNegativeCorrelationStats[seq_len(MaxCorrelationPairsToPlot)]
  }

  HighCorrelationStats <- CorrelationStats[
    !is.na(`Abs Correlation`) & `Abs Correlation` >= HighCorrelationThreshold
  ][
    order(-`Abs Correlation`)
  ]


  # ============================================================
  # Reactable: Correlation Input QA
  # ============================================================

  Correlation_Input_QA <- reactable::reactable(
    data = CorrelationInputQA,

    compact = TRUE,
    defaultPageSize = 10,
    wrap = TRUE,
    filterable = TRUE,
    fullWidth = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    resizable = TRUE,
    searchable = TRUE,
    showSortable = TRUE,
    showSortIcon = TRUE,
    sortable = TRUE,
    striped = FALSE,
    rowStyle = corr_dark_row_style,
    bordered = FALSE,
    outlined = FALSE,

    defaultColDef = reactable::colDef(
      headerStyle = corr_default_header_style,
      style = corr_dark_cell_style
    ),

    columns = list(
      Metric = reactable::colDef(
        minWidth = 230,
        style = function(value, index) {
          out <- corr_dark_cell_style(value, index)
          out$color <- "#F8FAFC"
          out$fontWeight <- "800"
          out
        }
      ),
      Count = reactable::colDef(
        minWidth = 95,
        align = "right",
        cell = corr_integer_cell,
        style = corr_dark_numeric_cell_style
      ),
      Values = reactable::colDef(
        minWidth = 600,
        cell = corr_truncated_mono_cell,
        style = corr_dark_cell_style
      )
    ),

    theme = corr_theme
  )


  # ============================================================
  # Reactable: Variable Diagnostics
  # ============================================================

  Correlation_Variable_Diagnostics <- reactable::reactable(
    data = CorrelationVariableDiagnostics,

    compact = TRUE,
    defaultPageSize = 15,
    pageSizeOptions = c(10, 15, 25, 50, 100),
    wrap = FALSE,
    filterable = TRUE,
    fullWidth = TRUE,
    highlight = TRUE,
    pagination = TRUE,
    resizable = TRUE,
    searchable = TRUE,
    selection = "multiple",
    showPagination = TRUE,
    showPageSizeOptions = TRUE,
    showSortable = TRUE,
    showSortIcon = TRUE,
    sortable = TRUE,
    striped = FALSE,
    rowStyle = corr_dark_row_style,
    bordered = FALSE,
    outlined = FALSE,

    defaultColDef = reactable::colDef(
      minWidth = 105,
      headerStyle = corr_default_header_style,
      style = corr_dark_cell_style
    ),

    columns = list(
      Variable = reactable::colDef(
        sticky = "left",
        minWidth = 220,
        filterable = TRUE,
        style = corr_dark_sticky_cell_style,
        headerStyle = list(
          position = "sticky",
          left = 0,
          zIndex = 2,
          background = "#0B1326",
          color = "#F8FAFC",
          fontWeight = "900",
          borderBottom = "1px solid #334155",
          boxShadow = "6px 0 10px rgba(0, 0, 0, 0.22)"
        )
      ),

      Type = reactable::colDef(
        minWidth = 105,
        align = "center",
        style = corr_dark_center_cell_style,
        cell = function(value) {
          if (corr_is_empty(value)) {
            htmltools::tags$span(style = "color:#64748B;", "—")
          } else {
            corr_badge(
              value,
              ifelse(value == "integer", "info", "good")
            )
          }
        }
      ),

      Rows = reactable::colDef(
        align = "right",
        cell = corr_integer_cell,
        style = corr_dark_numeric_cell_style
      ),

      `Finite N` = reactable::colDef(
        align = "right",
        cell = corr_integer_cell,
        style = corr_dark_numeric_cell_style
      ),

      `NA N` = reactable::colDef(
        align = "center",
        style = corr_dark_center_cell_style,
        cell = corr_positive_count_cell
      ),

      `NaN N` = reactable::colDef(
        align = "center",
        style = corr_dark_center_cell_style,
        cell = corr_positive_count_cell
      ),

      `Inf N` = reactable::colDef(
        align = "center",
        style = corr_dark_center_cell_style,
        cell = corr_positive_count_cell
      ),

      `Unique Finite N` = reactable::colDef(
        align = "right",
        cell = corr_integer_cell,
        style = corr_dark_numeric_cell_style
      ),

      `Usable For Correlation` = reactable::colDef(
        name = "Usable",
        align = "center",
        style = corr_dark_center_cell_style,
        cell = function(value) {
          if (corr_is_true(value)) {
            corr_badge("Yes", "good")
          } else {
            corr_badge("No", "danger")
          }
        }
      ),

      `Skip Reason` = reactable::colDef(
        minWidth = 240,
        style = corr_dark_cell_style,
        cell = corr_text_cell
      )
    ),

    theme = corr_theme
  )


  # ============================================================
  # Reactable: Pairwise Correlation Stats
  # ============================================================

  Correlation_Stats <- reactable::reactable(
    data = CorrelationStats,

    compact = TRUE,
    defaultPageSize = 15,
    pageSizeOptions = c(10, 15, 25, 50, 100),
    wrap = FALSE,
    filterable = TRUE,
    fullWidth = TRUE,
    highlight = TRUE,
    pagination = TRUE,
    resizable = TRUE,
    searchable = TRUE,
    selection = "multiple",
    showPagination = TRUE,
    showPageSizeOptions = TRUE,
    showSortable = TRUE,
    showSortIcon = TRUE,
    sortable = TRUE,
    striped = FALSE,
    rowStyle = corr_dark_row_style,
    bordered = FALSE,
    outlined = FALSE,

    defaultColDef = reactable::colDef(
      minWidth = 105,
      headerStyle = corr_default_header_style,
      style = corr_dark_cell_style
    ),

    columns = list(
      Pair = reactable::colDef(
        sticky = "left",
        minWidth = 360,
        filterable = TRUE,
        style = corr_dark_sticky_cell_style,
        headerStyle = list(
          position = "sticky",
          left = 0,
          zIndex = 2,
          background = "#0B1326",
          color = "#F8FAFC",
          fontWeight = "900",
          borderBottom = "1px solid #334155",
          boxShadow = "6px 0 10px rgba(0, 0, 0, 0.22)"
        )
      ),

      Variable1 = reactable::colDef(show = FALSE),

      Variable2 = reactable::colDef(show = FALSE),

      Method = reactable::colDef(
        minWidth = 110,
        align = "center",
        style = corr_dark_center_cell_style,
        cell = function(value) {
          if (corr_is_empty(value)) {
            htmltools::tags$span(style = "color:#64748B;", "—")
          } else {
            corr_badge(value, "info")
          }
        }
      ),

      N = reactable::colDef(
        minWidth = 95,
        align = "right",
        cell = corr_integer_cell,
        style = corr_dark_numeric_cell_style
      ),

      `Missing Pair N` = reactable::colDef(
        name = "Missing N",
        minWidth = 115,
        align = "right",
        cell = corr_integer_cell,
        style = corr_dark_numeric_cell_style
      ),

      Correlation = reactable::colDef(
        minWidth = 125,
        align = "center",
        style = corr_dark_center_cell_style,
        cell = corr_value_cell
      ),

      `Abs Correlation` = reactable::colDef(
        name = "|Corr|",
        minWidth = 110,
        align = "center",
        style = corr_dark_center_cell_style,
        cell = corr_abs_value_cell
      ),

      Direction = reactable::colDef(
        minWidth = 115,
        align = "center",
        style = corr_dark_center_cell_style,
        cell = corr_direction_cell
      ),

      Strength = reactable::colDef(
        minWidth = 125,
        align = "center",
        style = corr_dark_center_cell_style,
        cell = corr_strength_cell
      ),

      `P Value` = reactable::colDef(
        name = "P-Value",
        minWidth = 115,
        align = "center",
        style = corr_dark_center_cell_style,
        cell = corr_p_value_cell
      ),

      `High Correlation` = reactable::colDef(
        name = "High Corr",
        minWidth = 115,
        align = "center",
        style = corr_dark_center_cell_style,
        cell = corr_bool_cell
      )
    ),

    theme = corr_theme
  )


  # ============================================================
  # Correlation Matrix Plot
  # ============================================================

  CorrelogramPlot <- NULL

  if (length(CorrVars_Usable) >= 2L) {

    CorrelogramPlot <- AutoPlots::CorrMatrix(
      dt = data,
      PreAgg = FALSE,
      CorrVars = CorrVars_Usable,
      Method = CorrelationMethod,
      title.text = paste0("Correlation Matrix - ", tools::toTitleCase(CorrelationMethod)),
      yAxis.title = "",
      xAxis.title = "",
      Theme = Theme,
      tooltip.show = FALSE,
      xAxis.axisLabel.rotate = 45
    )
  }


  # ============================================================
  # Top Correlation Bar Plots
  # ============================================================

  CorrelationPlotList <- list()

  if (nrow(TopAbsCorrelationStats) > 0L) {

    TopAbsCorrelationStatsPlot <- aq_report_sort_for_flipped_bar(TopAbsCorrelationStats, "Abs Correlation", "Pair")

    CorrelationPlotList[["Top Absolute Correlations"]] <- AutoPlots::Bar(
      dt = TopAbsCorrelationStatsPlot,
      XVar = "Pair",
      YVar = "Abs Correlation",
      title.text = "Top Absolute Correlations",
      yAxis.title = "|Correlation|",
      xAxis.title = NULL,
      Theme = Theme
    ) |> echarts4r::e_flip_coords()
  }

  if (nrow(TopPositiveCorrelationStats) > 0L) {

    TopPositiveCorrelationStatsPlot <- aq_report_sort_for_flipped_bar(TopPositiveCorrelationStats, "Correlation", "Pair")

    CorrelationPlotList[["Top Positive Correlations"]] <- AutoPlots::Bar(
      dt = TopPositiveCorrelationStatsPlot,
      XVar = "Pair",
      YVar = "Correlation",
      title.text = "Top Positive Correlations",
      yAxis.title = "Correlation",
      xAxis.title = NULL,
      Theme = Theme
    ) |> echarts4r::e_flip_coords()
  }

  if (nrow(TopNegativeCorrelationStats) > 0L) {

    TopNegativeCorrelationStatsPlot <- data.table::copy(TopNegativeCorrelationStats)
    TopNegativeCorrelationStatsPlot[, `Negative Correlation Magnitude` := abs(Correlation)]
    TopNegativeCorrelationStatsPlot <- aq_report_sort_for_flipped_bar(TopNegativeCorrelationStatsPlot, "Negative Correlation Magnitude", "Pair")

    CorrelationPlotList[["Top Negative Correlations"]] <- AutoPlots::Bar(
      dt = TopNegativeCorrelationStatsPlot,
      XVar = "Pair",
      YVar = "Negative Correlation Magnitude",
      title.text = "Top Negative Correlations",
      yAxis.title = "|Negative Correlation|",
      xAxis.title = NULL,
      Theme = Theme
    ) |> echarts4r::e_flip_coords()
  }


  # ============================================================
  # Trend Analysis / Plots
  # ============================================================

  # ----------------------------
  # Trend plot storage
  # ----------------------------

  TrendAreaPlotList <- list()
  TrendPlotList <- list()

  TrendStatsList <- list()
  TrendGroupedStatsList <- list()

  TrendQA <- data.table::data.table(
    Metric = character(),
    Count = integer(),
    Values = character()
  )

  # ----------------------------
  # Trend helpers
  # ----------------------------

  clean_trend_var_vector <- function(x) {
    if (is.null(x)) {
      return(character(0L))
    }

    x <- as.character(x)
    x <- trimws(x)
    x <- x[!is.na(x) & nzchar(x)]
    unique(x)
  }

  is_trend_numeric_col <- function(x) {
    is.numeric(x) &&
      !inherits(x, c("Date", "POSIXct", "POSIXlt", "IDate", "ITime"))
  }

  is_valid_trend_date_col <- function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXlt", "IDate", "IDateTime"))
  }

  # ----------------------------
  # Validate trend inputs
  # ----------------------------

  TrendVars_Clean <- clean_trend_var_vector(TrendVars)

  TrendDateVar_Valid <- !is.null(TrendDateVar) &&
    length(TrendDateVar) == 1L &&
    !is.na(TrendDateVar) &&
    nzchar(TrendDateVar) &&
    TrendDateVar %in% names(data)

  TrendDateVar_Usable <- FALSE

  if (TrendDateVar_Valid) {
    TrendDateVar_Usable <- is_valid_trend_date_col(data[[TrendDateVar]])
  }

  TrendGroupVar_Valid <- !is.null(TrendGroupVar) &&
    length(TrendGroupVar) == 1L &&
    !is.na(TrendGroupVar) &&
    nzchar(TrendGroupVar) &&
    TrendGroupVar %in% names(data)

  TrendVars_Requested <- if (length(TrendVars_Clean) == 0L) {
    character(0L)
  } else {
    TrendVars_Clean
  }

  TrendVars_Valid <- intersect(TrendVars_Requested, names(data))
  TrendVars_Missing <- setdiff(TrendVars_Requested, names(data))

  TrendVars_Numeric <- TrendVars_Valid[
    vapply(
      TrendVars_Valid,
      function(v) is_trend_numeric_col(data[[v]]),
      logical(1L)
    )
  ]

  TrendVars_NonNumeric <- setdiff(TrendVars_Valid, TrendVars_Numeric)

  # ----------------------------
  # Base Trend QA table
  # ----------------------------

  TrendQA <- data.table::data.table(
    Metric = c(
      "TrendDateVar Provided",
      "TrendDateVar Exists",
      "TrendDateVar Usable Date Type",
      "TrendGroupVar Provided",
      "TrendGroupVar Exists",
      "Requested Trend Variables",
      "Valid Trend Variables",
      "Missing Trend Variables",
      "Numeric Trend Variables",
      "Non-Numeric Trend Variables"
    ),
    Count = c(
      as.integer(!is.null(TrendDateVar) && length(TrendDateVar) > 0L),
      as.integer(TrendDateVar_Valid),
      as.integer(TrendDateVar_Usable),
      as.integer(!is.null(TrendGroupVar) && length(TrendGroupVar) > 0L),
      as.integer(TrendGroupVar_Valid),
      length(TrendVars_Requested),
      length(TrendVars_Valid),
      length(TrendVars_Missing),
      length(TrendVars_Numeric),
      length(TrendVars_NonNumeric)
    ),
    Values = c(
      ifelse(is.null(TrendDateVar), "", paste(TrendDateVar, collapse = ", ")),
      as.character(TrendDateVar_Valid),
      as.character(TrendDateVar_Usable),
      ifelse(is.null(TrendGroupVar), "", paste(TrendGroupVar, collapse = ", ")),
      as.character(TrendGroupVar_Valid),
      paste(TrendVars_Requested, collapse = ", "),
      paste(TrendVars_Valid, collapse = ", "),
      paste(TrendVars_Missing, collapse = ", "),
      paste(TrendVars_Numeric, collapse = ", "),
      paste(TrendVars_NonNumeric, collapse = ", ")
    )
  )

  # ----------------------------
  # Build overall ungrouped area trend plots
  # ----------------------------

  if (TrendDateVar_Valid && TrendDateVar_Usable && length(TrendVars_Numeric) > 0L) {

    for (i in TrendVars_Numeric) {

      data1 <- data[
        !is.na(get(TrendDateVar)) &
          !is.na(get(i)) &
          is.finite(get(i)),
        .(Value = mean(get(i), na.rm = TRUE)),
        by = TrendDateVar
      ]

      if (nrow(data1) > 0L) {

        data.table::setnames(data1, "Value", i)
        data.table::setorderv(data1, TrendDateVar)

        TrendStatsList[[i]] <- data1

        TrendAreaPlotList[[i]] <- AutoPlots::Area(
          dt = data1,
          XVar = TrendDateVar,
          YVar = i,
          title.text = paste0(i, " Overall Trend"),
          yAxis.title = i,
          xAxis.title = TrendDateVar,
          Theme = Theme
        )
      }
    }
  }

  # ----------------------------
  # Build grouped line trend plots
  # ----------------------------

  if (TrendDateVar_Valid && TrendDateVar_Usable && TrendGroupVar_Valid && length(TrendVars_Numeric) > 0L) {

    for (i in TrendVars_Numeric) {

      data2 <- data[
        !is.na(get(TrendDateVar)) &
          !is.na(get(i)) &
          is.finite(get(i)) &
          !is.na(get(TrendGroupVar)),
        .(Value = mean(get(i), na.rm = TRUE)),
        by = c(TrendDateVar, TrendGroupVar)
      ]

      if (nrow(data2) > 0L) {

        data.table::setnames(data2, "Value", i)

        data.table::setorderv(
          data2,
          cols = c(TrendGroupVar, TrendDateVar)
        )

        TrendGroupedStatsList[[i]] <- data2

        TrendPlotList[[i]] <- AutoPlots::Line(
          dt = data2,
          XVar = TrendDateVar,
          YVar = i,
          GroupVar = TrendGroupVar,
          title.text = paste0(i, " Trend by ", TrendGroupVar),
          yAxis.title = i,
          xAxis.title = TrendDateVar,
          Theme = Theme
        )
      }
    }
  }

  # ----------------------------
  # Update Trend QA with output counts
  # ----------------------------

  TrendQA <- data.table::rbindlist(
    list(
      TrendQA,
      data.table::data.table(
        Metric = c(
          "Overall Trend Stats Created",
          "Overall Trend Area Plots Created",
          "Grouped Trend Stats Created",
          "Grouped Trend Line Plots Created"
        ),
        Count = c(
          length(TrendStatsList),
          length(TrendAreaPlotList),
          length(TrendGroupedStatsList),
          length(TrendPlotList)
        ),
        Values = c(
          paste(names(TrendStatsList), collapse = ", "),
          paste(names(TrendAreaPlotList), collapse = ", "),
          paste(names(TrendGroupedStatsList), collapse = ", "),
          paste(names(TrendPlotList), collapse = ", ")
        )
      )
    ),
    fill = TRUE
  )


  # ============================================================
  # LLM-friendly metadata / summaries
  # ============================================================

  artifact_counts <- data.table::data.table(
    ArtifactGroup = c(
      "DescribeData",
      "UnivariateStats",
      "UnivariateHistograms",
      "UnivariateBoxPlots",
      "UnivariateGroupedBoxPlots",
      "UnivariateDiscreteNumericBarPlots",
      "UnivariateCategoricalTopNBarPlots",
      "CorrelationStats",
      "CorrelationPairPlots",
      "TrendStats",
      "TrendAreaPlots",
      "TrendGroupedStats",
      "TrendGroupedLinePlots"
    ),
    Count = c(
      if (exists("DescribeData", inherits = FALSE)) nrow(DescribeData) else 0L,
      if (exists("UnivariateStats", inherits = FALSE)) nrow(UnivariateStats) else 0L,
      if (exists("UnivariateHistogramPlotList", inherits = FALSE)) length(UnivariateHistogramPlotList) else 0L,
      if (exists("UnivariateBoxPlotList", inherits = FALSE)) length(UnivariateBoxPlotList) else 0L,
      if (exists("UnivariateGroupedBoxPlotList", inherits = FALSE)) length(UnivariateGroupedBoxPlotList) else 0L,
      if (exists("UnivariateDiscreteNumericPlotList", inherits = FALSE)) length(UnivariateDiscreteNumericPlotList) else 0L,
      if (exists("UnivariateCategoricalTopNPlotList", inherits = FALSE)) length(UnivariateCategoricalTopNPlotList) else 0L,
      if (exists("CorrelationStats", inherits = FALSE)) nrow(CorrelationStats) else 0L,
      if (exists("CorrelationPlotList", inherits = FALSE)) length(CorrelationPlotList) else 0L,
      if (exists("TrendStatsList", inherits = FALSE)) length(TrendStatsList) else 0L,
      if (exists("TrendAreaPlotList", inherits = FALSE)) length(TrendAreaPlotList) else 0L,
      if (exists("TrendGroupedStatsList", inherits = FALSE)) length(TrendGroupedStatsList) else 0L,
      if (exists("TrendPlotList", inherits = FALSE)) length(TrendPlotList) else 0L
    )
  )

  llm_context <- list(
    DataName = DataName,
    Dimensions = list(
      Rows = nrow(data),
      Columns = ncol(data)
    ),
    Inputs = list(
      UnivariateVars = UnivariateVars,
      CorrVars = CorrVars,
      TrendVars = TrendVars,
      TrendDateVar = TrendDateVar,
      TrendGroupVar = TrendGroupVar,
      Theme = Theme
    ),
    ArtifactCounts = artifact_counts,
    InputQA = list(
      UnivariateInputQA = if (exists("UnivariateInputQA", inherits = FALSE)) UnivariateInputQA else NULL,
      CorrelationInputQA = if (exists("CorrelationInputQA", inherits = FALSE)) CorrelationInputQA else NULL,
      TrendQA = if (exists("TrendQA", inherits = FALSE)) TrendQA else NULL
    ),
    Tables = list(
      DescribeData = if (exists("DescribeData", inherits = FALSE)) DescribeData else NULL,
      UnivariateStats = if (exists("UnivariateStats", inherits = FALSE)) UnivariateStats else NULL,
      CorrelationStats = if (exists("CorrelationStats", inherits = FALSE)) CorrelationStats else NULL,
      HighCorrelationStats = if (exists("HighCorrelationStats", inherits = FALSE)) HighCorrelationStats else NULL,
      TopAbsCorrelationStats = if (exists("TopAbsCorrelationStats", inherits = FALSE)) TopAbsCorrelationStats else NULL,
      TopPositiveCorrelationStats = if (exists("TopPositiveCorrelationStats", inherits = FALSE)) TopPositiveCorrelationStats else NULL,
      TopNegativeCorrelationStats = if (exists("TopNegativeCorrelationStats", inherits = FALSE)) TopNegativeCorrelationStats else NULL
    )
  )

  legacy_tables <- list(
    describe_data = if (exists("DescribeData", inherits = FALSE)) DescribeData else NULL,
    univariate_stats = if (exists("UnivariateStats", inherits = FALSE)) UnivariateStats else NULL,
    correlation_stats = if (exists("CorrelationStats", inherits = FALSE)) CorrelationStats else NULL,
    high_correlation_stats = if (exists("HighCorrelationStats", inherits = FALSE)) HighCorrelationStats else NULL,
    top_abs_correlation_stats = if (exists("TopAbsCorrelationStats", inherits = FALSE)) TopAbsCorrelationStats else NULL,
    top_positive_correlation_stats = if (exists("TopPositiveCorrelationStats", inherits = FALSE)) TopPositiveCorrelationStats else NULL,
    top_negative_correlation_stats = if (exists("TopNegativeCorrelationStats", inherits = FALSE)) TopNegativeCorrelationStats else NULL,
    trend_stats = if (exists("TrendStatsList", inherits = FALSE)) TrendStatsList else NULL,
    trend_grouped_stats = if (exists("TrendGroupedStatsList", inherits = FALSE)) TrendGroupedStatsList else NULL
  )

  legacy_widgets <- list(
    describe_data = if (exists("Describe_Data", inherits = FALSE)) Describe_Data else NULL,
    univariate_stats = if (exists("Univariate_Stats", inherits = FALSE)) Univariate_Stats else NULL,
    correlation_variable_diagnostics = if (exists("Correlation_Variable_Diagnostics", inherits = FALSE)) Correlation_Variable_Diagnostics else NULL,
    correlation_stats = if (exists("Correlation_Stats", inherits = FALSE)) Correlation_Stats else NULL,
    correlation_input_qa = if (exists("Correlation_Input_QA", inherits = FALSE)) Correlation_Input_QA else NULL
  )

  legacy_plots <- list(
    univariate = if (exists("UnivariatePlots", inherits = FALSE)) UnivariatePlots else NULL,
    correlogram = if (exists("CorrelogramPlot", inherits = FALSE)) CorrelogramPlot else NULL,
    correlation_pairs = if (exists("CorrelationPlotList", inherits = FALSE)) CorrelationPlotList else NULL,
    trend_area = if (exists("TrendAreaPlotList", inherits = FALSE)) TrendAreaPlotList else NULL,
    trend_grouped_lines = if (exists("TrendPlotList", inherits = FALSE)) TrendPlotList else NULL
  )

  artifact_objects <- list(
    widgets = list(
      describe_data = eda_artifact(
        legacy_widgets$describe_data,
        section = "Data Description",
        artifact_type = "Reactable",
        name = "describe_data",
        title = "Describe Data",
        metadata = list(table = "DescribeData")
      ),
      univariate_stats = eda_artifact(
        legacy_widgets$univariate_stats,
        section = "Univariate Analysis",
        artifact_type = "Reactable",
        name = "univariate_stats",
        title = "Univariate Statistics",
        metadata = list(table = "UnivariateStats")
      ),
      correlation_variable_diagnostics = eda_artifact(
        legacy_widgets$correlation_variable_diagnostics,
        section = "Correlation Analysis",
        artifact_type = "Reactable",
        name = "correlation_variable_diagnostics",
        title = "Correlation Variable Diagnostics"
      ),
      correlation_stats = eda_artifact(
        legacy_widgets$correlation_stats,
        section = "Correlation Analysis",
        artifact_type = "Reactable",
        name = "correlation_stats",
        title = "Correlation Statistics"
      ),
      correlation_input_qa = eda_artifact(
        legacy_widgets$correlation_input_qa,
        section = "Correlation Analysis",
        artifact_type = "Reactable",
        name = "correlation_input_qa",
        title = "Correlation Input QA"
      )
    ),
    plots = list(
      univariate = list(
        Histograms = eda_wrap_named_objects(
          if (exists("UnivariateHistogramPlotList", inherits = FALSE)) UnivariateHistogramPlotList else NULL,
          section = "Univariate Analysis",
          artifact_type = "Histogram"
        ),
        BoxPlots = eda_wrap_named_objects(
          if (exists("UnivariateBoxPlotList", inherits = FALSE)) UnivariateBoxPlotList else NULL,
          section = "Univariate Analysis",
          artifact_type = "Box Plot"
        ),
        GroupedBoxPlots = eda_wrap_named_objects(
          if (exists("UnivariateGroupedBoxPlotList", inherits = FALSE)) UnivariateGroupedBoxPlotList else NULL,
          section = "Univariate Analysis",
          artifact_type = "Grouped Box Plot",
          metadata = list(group_var = TrendGroupVar)
        ),
        DiscreteNumericBarPlots = eda_wrap_named_objects(
          if (exists("UnivariateDiscreteNumericPlotList", inherits = FALSE)) UnivariateDiscreteNumericPlotList else NULL,
          section = "Univariate Analysis",
          artifact_type = "Discrete Numeric Bar Plot"
        ),
        CategoricalTopNBarPlots = eda_wrap_named_objects(
          if (exists("UnivariateCategoricalTopNPlotList", inherits = FALSE)) UnivariateCategoricalTopNPlotList else NULL,
          section = "Univariate Analysis",
          artifact_type = "Categorical Top-N Bar Plot"
        )
      ),
      correlogram = eda_artifact(
        legacy_plots$correlogram,
        section = "Correlation Analysis",
        artifact_type = "Correlogram",
        name = "correlogram",
        title = "Correlation Matrix"
      ),
      correlation_pairs = eda_wrap_named_objects(
        legacy_plots$correlation_pairs,
        section = "Correlation Analysis",
        artifact_type = "Correlation Pair Plot"
      ),
      trend_area = eda_wrap_named_objects(
        legacy_plots$trend_area,
        section = "Trend Analysis",
        artifact_type = "Trend Area Plot",
        metadata = list(date_var = TrendDateVar)
      ),
      trend_grouped_lines = eda_wrap_named_objects(
        legacy_plots$trend_grouped_lines,
        section = "Trend Analysis",
        artifact_type = "Grouped Trend Line Plot",
        metadata = list(date_var = TrendDateVar, group_var = TrendGroupVar)
      )
    )
  )

  export_requested <- isTRUE(ExportPNG) || isTRUE(ExportHTML)

  if (export_requested) {
    if (is.null(OutputPath) || !is.character(OutputPath) || length(OutputPath) != 1L || is.na(OutputPath) || !nzchar(OutputPath)) {
      stop("`OutputPath` must be supplied when `ExportPNG = TRUE` or `ExportHTML = TRUE`.", call. = FALSE)
    }

    artifact_objects <- eda_export_artifact_tree(
      tree = artifact_objects,
      output_path = OutputPath,
      export_png = ExportPNG,
      export_html = ExportHTML,
      width = PNGWidth,
      height = PNGHeight,
      dpi = PNGDPI,
      background = PNGBackground,
      delay = WebshotDelay,
      include_data_url = IncludeDataURL
    )
  }

  image_manifest <- data.table::rbindlist(
    lapply(eda_collect_llm_image_manifest(artifact_objects), function(x) {
      data.table::data.table(
        id = x$id,
        section = x$section,
        artifact_type = x$artifact_type,
        name = x$name,
        title = ifelse(is.null(x$title), NA_character_, x$title),
        png = ifelse(is.null(x$png), NA_character_, x$png),
        html = ifelse(is.null(x$html), NA_character_, x$html),
        png_error = ifelse(is.null(x$png_error), NA_character_, x$png_error),
        html_error = ifelse(is.null(x$html_error), NA_character_, x$html_error)
      )
    }),
    fill = TRUE
  )

  llm_context$ImageManifest <- image_manifest
  llm_context$ArtifactInstruction <- paste(
    "Use dynamic `object` values for RMarkdown display.",
    "Use `png` sidecars for LLM vision input when present.",
    "Use compact tables and metadata for exact numeric statements.",
    "Base64/data URLs are intentionally omitted unless `IncludeDataURL = TRUE`."
  )

  list(
    report_type = "eda",

    metadata = list(
      DataName = DataName,
      n_rows = nrow(data),
      n_cols = ncol(data),
      artifact_counts = artifact_counts,
      output_path = OutputPath,
      export_png = ExportPNG,
      export_html = ExportHTML,
      include_data_url = IncludeDataURL,
      created_at = as.character(Sys.time())
    ),

    qa = list(
      univariate = if (exists("UnivariateInputQA", inherits = FALSE)) UnivariateInputQA else NULL,
      correlation = if (exists("CorrelationInputQA", inherits = FALSE)) CorrelationInputQA else NULL,
      trend = if (exists("TrendQA", inherits = FALSE)) TrendQA else NULL
    ),

    tables = legacy_tables,
    widgets = legacy_widgets,
    plots = legacy_plots,

    layout = list(
      HistogramPlotCols = HistogramPlotCols,
      BoxPlotCols = BoxPlotCols,
      GroupedBoxPlotCols = GroupedBoxPlotCols,
      DiscreteNumericBarPlotCols = DiscreteNumericBarPlotCols,
      CategoricalBarPlotCols = CategoricalBarPlotCols
    ),

    exports = list(
      image_manifest = image_manifest
    ),

    context = llm_context
  )
}


# ============================================================
# Minimal Rmarkdown usage pattern
# ============================================================
#
# artifacts <- generate_eda_artifacts(
#   data = data,
#   DataName = "My Dataset",
#   UnivariateVars = UnivariateVars,
#   CorrVars = CorrVars,
#   TrendVars = TrendVars,
#   TrendDateVar = TrendDateVar,
#   TrendGroupVar = TrendGroupVar,
#   Theme = Theme
# )
#
# In Rmarkdown display chunks:
# artifacts$widgets$describe_data
# artifacts$widgets$univariate_stats
# artifacts$plots$univariate$Histograms[[1]]
#
# PNG/HTML sidecar manifest when ExportPNG/ExportHTML = TRUE:
# artifacts$exports$image_manifest
#
# LLM/commentary context:
# artifacts$context
# artifacts$context$ImageManifest
# ============================================================
