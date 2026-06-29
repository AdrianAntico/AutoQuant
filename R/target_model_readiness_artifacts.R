# ============================================================
# Target Analysis Artifact Generation
# ============================================================

#' Generate Target Analysis Artifacts
#'
#' Generates target-oriented analysis artifacts from a data set and target
#' variable. This function performs the calculations that previously lived
#' inside the Target Analysis R Markdown report and returns reusable tables,
#' widgets, plots, metadata, and optional export sidecars.
#'
#' This function does not call any LLM provider. It only prepares artifacts that
#' can be rendered by R Markdown, Shiny, Quarto, or passed to an LLM workflow
#' later.
#'
#' @param data A data.frame or data.table.
#' @param DataName Optional data set name.
#' @param TargetVar Character scalar. Target variable to analyze.
#' @param TrendDateVar Optional date or datetime variable used for target trends,
#'   feature drift, and concept drift diagnostics.
#' @param TrendGroupVar Optional character vector of grouping variables used for
#'   grouped target trend plots.
#' @param Theme AutoPlots theme value.
#' @param TargetMaxCategoricalLevels Maximum number of categorical levels retained
#'   in categorical target analysis and drift views.
#' @param TargetMaxAssociationRows Maximum rows retained for target association
#'   tables where applicable.
#' @param TargetMaxPlotRows Maximum rows retained for target-oriented plot data
#'   where applicable.
#' @param MaxCorrelationPairsToPlot Maximum number of association/drift rows to
#'   plot.
#' @param TargetHighLiftThreshold Lift threshold used for high-lift formatting.
#' @param TargetLowLiftThreshold Lift threshold used for low-lift formatting.
#' @param TargetExtremeRateThreshold Extreme target-rate threshold used for
#'   categorical separation diagnostics.
#' @param TargetMinLevelN Minimum level count used for low-N categorical flags.
#' @param TargetMinTimePeriods Minimum number of periods used for trend-related
#'   diagnostics.
#' @param TargetDriftRecentShare Share of rows used to define early/recent drift
#'   windows.
#' @param TargetConceptDriftMinN Minimum N used for concept drift diagnostics.
#' @param TargetConceptDriftMinPeriods Minimum number of time periods required for
#'   concept drift diagnostics.
#' @param OutputPath Optional directory used for PNG/HTML sidecar exports.
#' @param ExportPNG Logical. If TRUE, exports renderable artifacts to PNG.
#' @param ExportHTML Logical. If TRUE, exports renderable artifacts to HTML.
#' @param IncludeDataURL Logical. If TRUE, includes Base64 PNG data URLs for
#'   exported PNGs. Defaults to FALSE to avoid bloating the returned object.
#' @param PNGWidth PNG export width in pixels.
#' @param PNGHeight PNG export height in pixels.
#' @param PNGDPI PNG export resolution.
#' @param PNGBackground PNG export background color.
#' @param RunGAMDiagnostics Logical. If TRUE and `mgcv` is available, runs
#'   univariate GAM shape diagnostics for numeric features against the target.
#' @param MaxGAMFeatures Maximum number of numeric features to include in GAM
#'   shape diagnostics. Features are prioritized by absolute target association.
#' @param GAMSampleSize Maximum number of rows sampled for GAM diagnostics.
#' @param PreferredModelFamily Character scalar used when creating conditional
#'   feature engineering guidance. Defaults to `"catboost"`.
#' @param ModelingObjective Character scalar. One of `"prediction"`,
#'   `"explanation"`, or `"balanced"`. Used to contextualize readiness
#'   and feature engineering guidance.
#'
#' @return A list containing tables, widgets, plots, metadata, diagnostics, and
#'   optional export manifests.
#'
#' @examples
#' \dontrun{
#' target_artifacts <- generate_target_analysis_artifacts(
#'   data = dt,
#'   DataName = "Modeling Data",
#'   TargetVar = "converted",
#'   TrendDateVar = "event_date",
#'   TrendGroupVar = c("channel", "market"),
#'   Theme = "dark",
#'   OutputPath = "target_artifacts",
#'   ExportPNG = TRUE
#' )
#' }
#'
#' @export
generate_target_analysis_artifacts <- function(
    data,
    DataName = "Target Analysis Data",
    TargetVar = NULL,
    TrendDateVar = NULL,
    TrendGroupVar = NULL,
    Theme = "dark",
    TargetMaxCategoricalLevels = 25L,
    TargetMaxAssociationRows = 25L,
    TargetMaxPlotRows = 25L,
    MaxCorrelationPairsToPlot = 25L,
    TargetHighLiftThreshold = 2.00,
    TargetLowLiftThreshold = 0.50,
    TargetExtremeRateThreshold = 0.90,
    TargetMinLevelN = 20L,
    TargetMinTimePeriods = 4L,
    TargetDriftRecentShare = 0.25,
    TargetConceptDriftMinN = 20L,
    TargetConceptDriftMinPeriods = 4L,
    OutputPath = NULL,
    ExportPNG = FALSE,
    ExportHTML = FALSE,
    IncludeDataURL = FALSE,
    PNGWidth = 1400,
    PNGHeight = 900,
    PNGDPI = 150,
    PNGBackground = "white",
    RunGAMDiagnostics = TRUE,
    MaxGAMFeatures = 25L,
    GAMSampleSize = 50000L,
    PreferredModelFamily = "catboost",
    ModelingObjective = "balanced",
    StopOnInvalidTarget = TRUE
) {

  if (missing(data) || is.null(data)) {
    stop("`data` must be supplied.", call. = FALSE)
  }

  data <- data.table::as.data.table(data)

  # Resolve user-supplied column names defensively.
  # Exact matches are preferred. Case-insensitive unique matches are accepted.
  # This prevents "converted" vs "Converted" style issues from producing a mostly empty report.
  target_resolve_column_name <- function(x, choices) {
    if (is.null(x) || length(x) == 0L || is.na(x[1L]) || !nzchar(as.character(x[1L]))) {
      return(NULL)
    }

    x <- trimws(as.character(x[1L]))
    choices <- as.character(choices)

    if (x %in% choices) {
      return(x)
    }

    idx <- which(tolower(choices) == tolower(x))
    if (length(idx) == 1L) {
      return(choices[idx])
    }

    # Also allow make.names-style matching for non-syntactic names.
    idx2 <- which(make.names(choices) == make.names(x))
    if (length(idx2) == 1L) {
      return(choices[idx2])
    }

    NULL
  }

  target_close_column_matches <- function(x, choices, max_matches = 8L) {
    if (is.null(x) || length(x) == 0L || is.na(x[1L]) || !nzchar(as.character(x[1L]))) {
      return(character(0L))
    }

    x <- trimws(as.character(x[1L]))
    choices <- as.character(choices)

    direct <- grep(x, choices, ignore.case = TRUE, value = TRUE, fixed = TRUE)

    fuzzy <- tryCatch(
      choices[utils::adist(tolower(x), tolower(choices)) <= max(2L, floor(nchar(x) * 0.35))],
      error = function(e) character(0L)
    )

    unique(utils::head(c(direct, fuzzy), max_matches))
  }


  DataName <- if (
    is.null(DataName) ||
    length(DataName) == 0L ||
    is.na(DataName[1L]) ||
    !nzchar(as.character(DataName[1L]))
  ) {
    "Target Analysis Data"
  } else {
    as.character(DataName[1L])
  }

  TargetVarRequested <- if (!is.null(TargetVar) && length(TargetVar) > 0L) {
    as.character(TargetVar[1L])
  } else {
    NULL
  }

  TargetVar <- target_resolve_column_name(TargetVarRequested, names(data))
  TargetVarResolution <- if (is.null(TargetVarRequested)) {
    "not_provided"
  } else if (!is.null(TargetVar) && identical(TargetVar, TargetVarRequested)) {
    "exact"
  } else if (!is.null(TargetVar)) {
    "resolved"
  } else {
    "not_found"
  }
  PreferredModelFamily <- tolower(as.character(PreferredModelFamily[1L]))
  PreferredModelFamily <- ifelse(
    is.na(PreferredModelFamily) || !nzchar(PreferredModelFamily),
    "catboost",
    PreferredModelFamily
  )

  ModelingObjective <- tolower(as.character(ModelingObjective[1L]))
  if (!ModelingObjective %in% c("prediction", "explanation", "balanced")) {
    ModelingObjective <- "balanced"
  }

  MaxGAMFeatures <- suppressWarnings(as.integer(MaxGAMFeatures[1L]))
  if (is.na(MaxGAMFeatures) || MaxGAMFeatures < 0L) {
    MaxGAMFeatures <- 25L
  }

  GAMSampleSize <- suppressWarnings(as.integer(GAMSampleSize[1L]))
  if (is.na(GAMSampleSize) || GAMSampleSize < 100L) {
    GAMSampleSize <- 50000L
  }

  TrendDateVarRequested <- if (!is.null(TrendDateVar) && length(TrendDateVar) > 0L) {
    as.character(TrendDateVar[1L])
  } else {
    NULL
  }

  TrendDateVar <- target_resolve_column_name(TrendDateVarRequested, names(data))

  TrendGroupVarRequested <- if (!is.null(TrendGroupVar)) {
    as.character(TrendGroupVar)
  } else {
    NULL
  }

  TrendGroupVar <- if (!is.null(TrendGroupVarRequested)) {
    resolved_groups <- vapply(
      TrendGroupVarRequested,
      function(g) {
        out <- target_resolve_column_name(g, names(data))
        if (is.null(out)) NA_character_ else out
      },
      character(1L)
    )
    unique(resolved_groups[!is.na(resolved_groups) & nzchar(resolved_groups)])
  } else {
    NULL
  }

  # Convert date-like trend variable when possible.
  if (!is.null(TrendDateVar) && TrendDateVar %in% names(data)) {
    if (!inherits(data[[TrendDateVar]], c("Date", "POSIXct", "POSIXlt", "IDate", "IDateTime"))) {
      data[, (TrendDateVar) := as.Date(get(TrendDateVar))]
    }
  }

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

  DescribeData[, `Is Constant` := data.table::fifelse(
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
  # Target-Oriented Analysis
  # ============================================================

  # ----------------------------
  # Target settings
  # ----------------------------

  # ----------------------------
  # Target settings
  # ----------------------------


  # ============================================================
  # Target Safe Helpers
  # ============================================================
  target_safe_mean <- function(x) {
    out <- suppressWarnings(mean(x, na.rm = TRUE))
    as.numeric(out)
  }

  target_safe_median <- function(x) {
    out <- suppressWarnings(stats::median(x, na.rm = TRUE))
    as.numeric(out)
  }

  target_safe_sd <- function(x) {
    out <- suppressWarnings(stats::sd(x, na.rm = TRUE))
    as.numeric(out)
  }

  target_safe_min <- function(x) {
    out <- suppressWarnings(min(x, na.rm = TRUE))
    as.numeric(out)
  }

  target_safe_max <- function(x) {
    out <- suppressWarnings(max(x, na.rm = TRUE))
    as.numeric(out)
  }

  target_is_empty <- function(value) {
    is.null(value) ||
      length(value) == 0L ||
      all(is.na(value))
  }

  target_safe_chr <- function(value, empty_value = "—") {
    if (target_is_empty(value)) {
      empty_value
    } else {
      as.character(value)
    }
  }

  target_safe_num <- function(value) {
    if (target_is_empty(value)) {
      return(NA_real_)
    }

    suppressWarnings(as.numeric(value)[1L])
  }

  target_clean_var_vector <- function(x) {
    if (is.null(x)) {
      return(character(0L))
    }

    x <- as.character(x)
    x <- trimws(x)
    x <- x[!is.na(x) & nzchar(x)]
    unique(x)
  }

  target_is_numeric_col <- function(x) {
    is.numeric(x) &&
      !inherits(x, c("Date", "POSIXct", "POSIXlt", "IDate", "ITime"))
  }

  target_is_categorical_col <- function(x) {
    is.character(x) ||
      is.factor(x) ||
      is.logical(x)
  }

  target_is_date_col <- function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXlt", "IDate", "IDateTime"))
  }

  target_is_binary_numeric <- function(x) {
    if (!target_is_numeric_col(x)) {
      return(FALSE)
    }

    y <- x[!is.na(x) & is.finite(x)]

    if (!length(y)) {
      return(FALSE)
    }

    vals <- sort(unique(y))

    length(vals) == 2L && all(vals %in% c(0, 1))
  }

  target_mode_value <- function(x) {
    if (!length(x)) {
      return(NA_character_)
    }

    x <- x[!is.na(x)]

    if (!length(x)) {
      return(NA_character_)
    }

    tab <- sort(table(x), decreasing = TRUE)

    names(tab)[1L]
  }

  target_fmt_int <- function(x) {
    if (target_is_empty(x)) {
      return(NA_character_)
    }

    x <- suppressWarnings(as.numeric(x))

    if (!length(x) || is.na(x[1L])) {
      return(NA_character_)
    }

    formatC(round(x[1L]), format = "f", digits = 0L, big.mark = ",")
  }

  target_fmt_num <- function(x, digits = 3L) {
    if (target_is_empty(x)) {
      return(NA_character_)
    }

    x <- suppressWarnings(as.numeric(x))

    if (!length(x) || is.na(x[1L])) {
      return(NA_character_)
    }

    digits <- safe_format_digits(
      digits = digits,
      default = 3L,
      max_digits = 8L
    )

    formatC(x[1L], format = "f", digits = digits, big.mark = ",")
  }

  target_fmt_pct <- function(x, digits = 2L) {
    if (target_is_empty(x)) {
      return(NA_character_)
    }

    x <- suppressWarnings(as.numeric(x))

    if (!length(x) || is.na(x[1L])) {
      return(NA_character_)
    }

    paste0(formatC(x[1L], format = "f", big.mark = ",", digits = digits), "%")
  }

  target_safe_cor <- function(x, y, method = "spearman") {
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

  target_safe_cor_p_value <- function(x, y, method = "spearman") {
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

  target_strength_label <- function(x) {
    if (is.na(x)) {
      return("Not available")
    }

    ax <- abs(x)

    if (ax >= 0.90) {
      "Very Strong"
    } else if (ax >= 0.70) {
      "Strong"
    } else if (ax >= 0.50) {
      "Moderate"
    } else if (ax >= 0.30) {
      "Weak"
    } else {
      "Very Weak"
    }
  }

  target_direction_label <- function(x) {
    if (is.na(x)) {
      return("Not available")
    }

    if (x > 0) {
      "Positive"
    } else if (x < 0) {
      "Negative"
    } else {
      "Zero"
    }
  }

  target_risk_word_match <- function(x) {
    x <- tolower(x)

    patterns <- c(
      "target",
      "outcome",
      "result",
      "status",
      "converted",
      "conversion",
      "complete",
      "completed",
      "approved",
      "accepted",
      "rejected",
      "enrolled",
      "enrollment",
      "churn",
      "churned",
      "cancel",
      "cancelled",
      "retained",
      "retention",
      "response",
      "label",
      "score",
      "decision",
      "final",
      "post",
      "after"
    )

    any(vapply(patterns, function(p) grepl(p, x, fixed = TRUE), logical(1L)))
  }

  target_feature_role <- function(v, x) {
    if (v == TargetVar) {
      return("Target")
    }

    if (target_is_numeric_col(x)) {
      return("Numeric Feature")
    }

    if (target_is_categorical_col(x)) {
      return("Categorical Feature")
    }

    if (target_is_date_col(x)) {
      return("Date/Time")
    }

    "Skipped / Unsupported"
  }


  # ============================================================
  # Target Reactable Styling Helpers
  # ============================================================

  target_safe_index <- function(index) {
    if (is.null(index) || length(index) == 0L || is.na(index)) {
      return(1L)
    }

    as.integer(index[1L])
  }

  target_row_bg <- function(index) {
    index <- target_safe_index(index)

    if (index %% 2L == 0L) {
      "#0B1326"
    } else {
      "#111C33"
    }
  }

  target_sticky_row_bg <- function(index) {
    index <- target_safe_index(index)

    if (index %% 2L == 0L) {
      "#101B2F"
    } else {
      "#13203A"
    }
  }

  target_dark_row_style <- function(index) {
    list(
      backgroundColor = target_row_bg(index),
      color = "#E2E8F0"
    )
  }

  target_dark_cell_style <- function(value, index = NULL) {
    list(
      backgroundColor = target_row_bg(index),
      color = if (target_is_empty(value)) "#64748B" else "#E2E8F0",
      borderBottom = "1px solid rgba(148, 163, 184, 0.14)"
    )
  }

  target_dark_numeric_cell_style <- function(value, index = NULL) {
    list(
      backgroundColor = target_row_bg(index),
      color = if (target_is_empty(value)) "#64748B" else "#E2E8F0",
      textAlign = "right",
      fontVariantNumeric = "tabular-nums",
      borderBottom = "1px solid rgba(148, 163, 184, 0.14)"
    )
  }

  target_dark_center_cell_style <- function(value, index = NULL) {
    list(
      backgroundColor = target_row_bg(index),
      color = if (target_is_empty(value)) "#64748B" else "#E2E8F0",
      textAlign = "center",
      borderBottom = "1px solid rgba(148, 163, 184, 0.14)"
    )
  }

  target_dark_sticky_cell_style <- function(value, index = NULL) {
    list(
      position = "sticky",
      left = 0,
      zIndex = 1,
      backgroundColor = target_sticky_row_bg(index),
      color = "#F8FAFC",
      fontWeight = "800",
      fontFamily = "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace",
      borderBottom = "1px solid rgba(148, 163, 184, 0.14)",
      boxShadow = "6px 0 10px rgba(0, 0, 0, 0.18)"
    )
  }

  target_numeric_cell <- function(value, index = NULL, name = NULL, digits = 3L) {

    digits <- safe_format_digits(
      digits = digits,
      default = 3L,
      max_digits = 8L
    )

    if (target_is_empty(value)) {
      htmltools::tags$span(style = "color:#64748B;", "—")
    } else {
      htmltools::tags$span(
        style = "font-variant-numeric:tabular-nums;color:#E2E8F0;",
        target_fmt_num(value, digits = digits)
      )
    }
  }

  target_pct_cell <- function(value, index = NULL, name = NULL, digits = 2L) {

    digits <- safe_format_digits(
      digits = digits,
      default = 2L,
      max_digits = 8L
    )

    if (target_is_empty(value)) {
      htmltools::tags$span(style = "color:#64748B;", "—")
    } else {
      htmltools::tags$span(
        style = "font-variant-numeric:tabular-nums;color:#E2E8F0;",
        target_fmt_pct(value, digits = digits)
      )
    }
  }

  target_default_header_style <- list(
    background = "#0F1B33",
    color = "#E5E7EB",
    fontWeight = "800",
    borderBottom = "1px solid #334155",
    textTransform = "uppercase",
    letterSpacing = "0.045em",
    fontSize = "11px"
  )

  target_theme <- reactable::reactableTheme(
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

  target_badge <- function(value, type = c("neutral", "good", "warning", "danger", "info")) {

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
      target_safe_chr(value)
    )
  }

  target_integer_cell <- function(value) {
    if (target_is_empty(value)) {
      htmltools::tags$span(style = "color:#64748B;", "—")
    } else {
      htmltools::tags$span(
        style = "font-variant-numeric:tabular-nums;color:#E2E8F0;",
        target_fmt_int(value)
      )
    }
  }

  target_numeric_cell <- function(value, digits = 3L) {
    if (target_is_empty(value)) {
      htmltools::tags$span(style = "color:#64748B;", "—")
    } else {
      htmltools::tags$span(
        style = "font-variant-numeric:tabular-nums;color:#E2E8F0;",
        target_fmt_num(value, digits)
      )
    }
  }

  target_pct_cell <- function(value, digits = 2L) {
    if (target_is_empty(value)) {
      htmltools::tags$span(style = "color:#64748B;", "—")
    } else {
      htmltools::tags$span(
        style = "font-variant-numeric:tabular-nums;color:#E2E8F0;",
        target_fmt_pct(value, digits)
      )
    }
  }

  target_corr_cell <- function(value) {
    value <- target_safe_num(value)

    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    av <- abs(value)

    type <- if (av >= 0.90) {
      "danger"
    } else if (av >= 0.70) {
      "warning"
    } else if (av >= 0.50) {
      "info"
    } else {
      "neutral"
    }

    target_badge(target_fmt_num(value, 3L), type)
  }

  target_abs_corr_cell <- function(value) {
    value <- target_safe_num(value)

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

    target_badge(target_fmt_num(value, 3L), type)
  }

  target_lift_cell <- function(value) {
    value <- target_safe_num(value)

    if (is.na(value)) {
      return(htmltools::tags$span(style = "color:#64748B;", "—"))
    }

    type <- if (value >= TargetHighLiftThreshold) {
      "danger"
    } else if (value <= TargetLowLiftThreshold) {
      "warning"
    } else {
      "neutral"
    }

    target_badge(target_fmt_num(value, 3L), type)
  }

  target_flag_cell <- function(value) {
    value <- target_safe_chr(value)

    type <- switch(
      value,
      "High" = "danger",
      "Medium" = "warning",
      "Low" = "info",
      "Yes" = "danger",
      "No" = "good",
      "Not available" = "neutral",
      "neutral"
    )

    target_badge(value, type)
  }

  target_text_cell <- function(value) {
    htmltools::tags$span(
      style = "color:#CBD5E1;",
      target_safe_chr(value)
    )
  }

  target_truncated_mono_cell <- function(value) {
    htmltools::tags$span(
      title = target_safe_chr(value, empty_value = ""),
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
      target_safe_chr(value)
    )
  }


  # ============================================================
  # Target Object Initialization
  # ============================================================

  TargetVar_Clean <- target_clean_var_vector(TargetVar)

  TargetVar_Provided <- length(TargetVar_Clean) > 0L
  TargetVar_Name <- if (TargetVar_Provided) TargetVar_Clean[1L] else NA_character_
  TargetVar_Valid <- TargetVar_Provided && TargetVar_Name %in% names(data)

  if (!TargetVar_Valid && isTRUE(StopOnInvalidTarget)) {
    close_matches <- target_close_column_matches(TargetVarRequested, names(data))
    close_msg <- if (length(close_matches) > 0L) {
      paste0(" Close matches: ", paste(close_matches, collapse = ", "), ".")
    } else {
      ""
    }

    stop(
      "`TargetVar` was not found in `data`. Requested: ",
      ifelse(is.null(TargetVarRequested), "<NULL>", TargetVarRequested),
      ". Available columns include: ",
      paste(utils::head(names(data), 20L), collapse = ", "),
      ifelse(length(names(data)) > 20L, ", ...", ""),
      ".",
      close_msg,
      call. = FALSE
    )
  }

  TargetVar_Type <- NA_character_
  TargetVar_Class <- NA_character_
  TargetVar_Usable <- FALSE
  TargetProblemType <- "Not provided"

  TargetQA <- data.table::data.table()
  TargetDistributionTable <- data.table::data.table()
  TargetNumericAssociationStats <- data.table::data.table()
  TargetCategoricalLevelStats <- data.table::data.table()
  TargetCategoricalAssociationStats <- data.table::data.table()
  TargetTrendStats <- data.table::data.table()
  TargetGroupedTrendStats <- data.table::data.table()
  TargetFeatureDriftStats <- data.table::data.table()
  TargetConceptDriftStats <- data.table::data.table()
  TargetRiskFlags <- data.table::data.table()
  TargetPlotList <- list()
  TargetTrendPlotList <- list()
  TargetGroupedTrendPlotList <- list()
  TargetDistributionPlot <- NULL
  TargetAssociationPlotList <- list()
  TargetDriftPlotList <- list()
  TargetRiskPlotList <- list()


  # ============================================================
  # Target Validation / QA
  # ============================================================

  if (TargetVar_Valid) {

    target_y <- data[[TargetVar_Name]]

    TargetVar_Class <- class(target_y)[1L]

    TargetVar_Type <- if (target_is_binary_numeric(target_y)) {
      "Binary Numeric"
    } else if (target_is_numeric_col(target_y)) {
      "Continuous Numeric"
    } else if (target_is_categorical_col(target_y)) {
      if (data.table::uniqueN(target_y, na.rm = TRUE) == 2L) {
        "Binary Categorical"
      } else {
        "Multiclass Categorical"
      }
    } else {
      "Unsupported"
    }

    TargetProblemType <- if (TargetVar_Type %in% c("Binary Numeric", "Binary Categorical")) {
      "Binary"
    } else if (TargetVar_Type == "Continuous Numeric") {
      "Regression"
    } else if (TargetVar_Type == "Multiclass Categorical") {
      "Multiclass"
    } else {
      "Unsupported"
    }

    TargetVar_Usable <- TargetProblemType %in% c("Binary", "Regression", "Multiclass")

    TargetQA <- data.table::data.table(
      Metric = c(
        "TargetVar Provided",
        "TargetVar Valid",
        "Target Variable",
        "Target Class",
        "Target Type",
        "Target Problem Type",
        "Target Usable",
        "Rows",
        "Target NA N",
        "Target NA %",
        "Target Unique N",
        "Target Mode"
      ),
      Count = c(
        as.integer(TargetVar_Provided),
        as.integer(TargetVar_Valid),
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        as.integer(TargetVar_Usable),
        length(target_y),
        sum(is.na(target_y)),
        NA_integer_,
        data.table::uniqueN(target_y, na.rm = TRUE),
        NA_integer_
      ),
      Value = c(
        as.character(TargetVar_Provided),
        as.character(TargetVar_Valid),
        TargetVar_Name,
        TargetVar_Class,
        TargetVar_Type,
        TargetProblemType,
        as.character(TargetVar_Usable),
        as.character(length(target_y)),
        as.character(sum(is.na(target_y))),
        target_fmt_pct(mean(is.na(target_y)) * 100, 2L),
        as.character(data.table::uniqueN(target_y, na.rm = TRUE)),
        target_mode_value(target_y)
      )
    )

  } else {

    TargetQA <- data.table::data.table(
      Metric = c(
        "TargetVar Provided",
        "TargetVar Valid",
        "Target Variable",
        "Target Class",
        "Target Type",
        "Target Problem Type",
        "Target Usable",
        "Rows",
        "Target NA N",
        "Target NA %",
        "Target Unique N",
        "Target Mode"
      ),
      Count = c(
        as.integer(TargetVar_Provided),
        as.integer(TargetVar_Valid),
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        0L,
        nrow(data),
        NA_integer_,
        NA_integer_,
        NA_integer_,
        NA_integer_
      ),
      Value = c(
        as.character(TargetVar_Provided),
        as.character(TargetVar_Valid),
        ifelse(TargetVar_Provided, TargetVar_Name, ""),
        "",
        "",
        "Not provided",
        "FALSE",
        as.character(nrow(data)),
        "",
        "",
        "",
        ""
      )
    )
  }


  # ============================================================
  # Target Distribution
  # ============================================================

  if (TargetVar_Valid && TargetVar_Usable) {

    target_y <- data[[TargetVar_Name]]

    if (TargetProblemType %in% c("Binary", "Multiclass")) {

      target_dist_dt <- data.table::data.table(
        TargetValue = as.character(target_y)
      )

      target_dist_dt[is.na(TargetValue), TargetValue := "<NA>"]
      target_dist_dt[trimws(TargetValue) == "", TargetValue := "<EMPTY STRING>"]

      TargetDistributionTable <- target_dist_dt[
        ,
        .(N = .N),
        by = TargetValue
      ]

      data.table::setorderv(TargetDistributionTable, "N", order = -1L)

      TargetDistributionTable[, Percent := N / sum(N) * 100]

      TargetDistributionPlot <- AutoPlots::Bar(
        dt = TargetDistributionTable,
        XVar = "TargetValue",
        YVar = "N",
        title.text = paste0(TargetVar_Name, " Distribution"),
        yAxis.title = "Count",
        xAxis.title = TargetVar_Name,
        Theme = Theme
      ) |> echarts4r::e_flip_coords()

      TargetPlotList[["Target Distribution"]] <- TargetDistributionPlot

    } else if (TargetProblemType == "Regression") {

      TargetDistributionTable <- data.table::data.table(
        Metric = c(
          "N",
          "NA N",
          "Mean",
          "Median",
          "Standard Deviation",
          "Min",
          "Q25",
          "Q75",
          "Max"
        ),
        Value = as.numeric(c(
          sum(!is.na(target_y) & is.finite(target_y)),
          sum(is.na(target_y)),
          target_safe_mean(target_y),
          target_safe_median(target_y),
          target_safe_sd(target_y),
          target_safe_min(target_y),
          suppressWarnings(as.numeric(stats::quantile(target_y, probs = 0.25, na.rm = TRUE, names = FALSE))),
          suppressWarnings(as.numeric(stats::quantile(target_y, probs = 0.75, na.rm = TRUE, names = FALSE))),
          target_safe_max(target_y)
        ))
      )

      TargetDistributionPlot <- AutoPlots::Histogram(
        dt = data[!is.na(get(TargetVar_Name)) & is.finite(get(TargetVar_Name))],
        XVar = TargetVar_Name,
        title.text = paste0(TargetVar_Name, " Distribution"),
        xAxis.title = TargetVar_Name,
        yAxis.title = "Count",
        Theme = Theme
      )

      TargetPlotList[["Target Distribution"]] <- TargetDistributionPlot
    }
  }


  # ============================================================
  # Feature Set for Target Analysis
  # ============================================================

  TargetCandidateVars <- setdiff(names(data), TargetVar_Name)

  TargetNumericFeatureVars <- TargetCandidateVars[
    vapply(
      TargetCandidateVars,
      function(v) target_is_numeric_col(data[[v]]),
      logical(1L)
    )
  ]

  TargetCategoricalFeatureVars <- TargetCandidateVars[
    vapply(
      TargetCandidateVars,
      function(v) target_is_categorical_col(data[[v]]),
      logical(1L)
    )
  ]

  TargetSkippedFeatureVars <- setdiff(
    TargetCandidateVars,
    c(TargetNumericFeatureVars, TargetCategoricalFeatureVars)
  )


  # ============================================================
  # Numeric Feature Association With Target
  # ============================================================

  if (TargetVar_Valid && TargetVar_Usable && TargetProblemType %in% c("Binary", "Regression") && length(TargetNumericFeatureVars) > 0L) {

    target_y <- data[[TargetVar_Name]]

    if (TargetProblemType == "Binary" && !target_is_numeric_col(target_y)) {
      target_levels <- sort(unique(as.character(target_y[!is.na(target_y)])))
      target_y_num <- as.integer(as.character(target_y) == target_levels[length(target_levels)])
    } else {
      target_y_num <- target_y
    }

    TargetNumericAssociationStats <- data.table::rbindlist(
      lapply(TargetNumericFeatureVars, function(v) {

        x <- data[[v]]

        keep <- !is.na(x) &
          !is.na(target_y_num) &
          is.finite(x) &
          is.finite(target_y_num)

        n_valid <- sum(keep)

        r <- target_safe_cor(x, target_y_num, method = "spearman")
        p <- target_safe_cor_p_value(x, target_y_num, method = "spearman")

        data.table::data.table(
          Feature = v,
          FeatureType = class(x)[1L],
          N = n_valid,
          `Feature NA N` = sum(is.na(x)),
          `Feature NA %` = mean(is.na(x)) * 100,
          `Target NA N` = sum(is.na(target_y_num)),
          `Correlation` = r,
          `Abs Correlation` = abs(r),
          Direction = target_direction_label(r),
          Strength = target_strength_label(r),
          `P Value` = p,
          `Suspicious Name` = target_risk_word_match(v)
        )
      }),
      fill = TRUE
    )

    data.table::setorderv(
      TargetNumericAssociationStats,
      cols = c("Abs Correlation", "N"),
      order = c(-1L, -1L),
      na.last = TRUE
    )
  }


  # ============================================================
  # Categorical Feature Association With Target
  # ============================================================

  if (TargetVar_Valid && TargetVar_Usable && TargetProblemType %in% c("Binary", "Regression") && length(TargetCategoricalFeatureVars) > 0L) {

    target_y <- data[[TargetVar_Name]]

    if (TargetProblemType == "Binary" && !target_is_numeric_col(target_y)) {
      target_levels <- sort(unique(as.character(target_y[!is.na(target_y)])))
      target_y_num <- as.integer(as.character(target_y) == target_levels[length(target_levels)])
    } else {
      target_y_num <- target_y
    }

    global_target_mean <- mean(target_y_num, na.rm = TRUE)

    TargetCategoricalLevelStats <- data.table::rbindlist(
      lapply(TargetCategoricalFeatureVars, function(v) {

        x <- as.character(data[[v]])
        x[is.na(x)] <- "<NA>"
        x[trimws(x) == ""] <- "<EMPTY STRING>"

        data2 <- data.table::data.table(
          Feature = v,
          Level = x,
          TargetValue = target_y_num
        )

        out <- data2[
          ,
          .(
            N = as.integer(.N),
            `Target NonMissing N` = as.integer(sum(!is.na(TargetValue))),
            `Target Mean` = target_safe_mean(TargetValue),
            `Target Median` = target_safe_median(TargetValue),
            `Target SD` = target_safe_sd(TargetValue)
          ),
          by = .(Feature, Level)
        ]

        out[, `Level N %` := N / sum(N) * 100]
        out[, `Global Target Mean` := global_target_mean]
        out[, Lift := `Target Mean` / global_target_mean]
        out[, `Abs Lift From 1` := abs(Lift - 1)]
        out[, `Suspicious Name` := target_risk_word_match(v)]
        out[, `Low N Flag` := N < TargetMinLevelN]

        data.table::setorderv(out, c("Feature", "N"), order = c(1L, -1L))

        if (nrow(out) > TargetMaxCategoricalLevels) {
          out <- out[seq_len(TargetMaxCategoricalLevels)]
        }

        out
      }),
      fill = TRUE
    )

    TargetCategoricalAssociationStats <- TargetCategoricalLevelStats[
      ,
      .(
        Levels = data.table::uniqueN(Level),
        N = sum(N),
        `Max Target Mean` = max(`Target Mean`, na.rm = TRUE),
        `Min Target Mean` = min(`Target Mean`, na.rm = TRUE),
        `Target Mean Range` = max(`Target Mean`, na.rm = TRUE) - min(`Target Mean`, na.rm = TRUE),
        `Max Lift` = max(Lift, na.rm = TRUE),
        `Min Lift` = min(Lift, na.rm = TRUE),
        `Max Abs Lift From 1` = max(`Abs Lift From 1`, na.rm = TRUE),
        `Extreme Level Count` = sum(
          !is.na(`Target Mean`) &
            (`Target Mean` >= TargetExtremeRateThreshold | `Target Mean` <= (1 - TargetExtremeRateThreshold))
        ),
        `Low N Level Count` = sum(`Low N Flag`, na.rm = TRUE),
        `Suspicious Name` = any(`Suspicious Name`, na.rm = TRUE)
      ),
      by = Feature
    ]

    TargetCategoricalAssociationStats[, RiskLevel := data.table::fifelse(
      `Suspicious Name` & `Max Abs Lift From 1` >= 1,
      "High",
      data.table::fifelse(
        `Max Abs Lift From 1` >= 1 | `Extreme Level Count` > 0,
        "Medium",
        data.table::fifelse(`Max Abs Lift From 1` >= 0.5, "Low", "None")
      )
    )]

    data.table::setorderv(
      TargetCategoricalAssociationStats,
      cols = c("RiskLevel", "Max Abs Lift From 1"),
      order = c(1L, -1L)
    )
  }


  # ============================================================
  # Target Trend / Target Grouped Trend
  # ============================================================

  TargetTrendDateVar_Valid <- !is.null(TrendDateVar) &&
    length(TrendDateVar) == 1L &&
    !is.na(TrendDateVar) &&
    nzchar(TrendDateVar) &&
    TrendDateVar %in% names(data) &&
    target_is_date_col(data[[TrendDateVar]])

  TargetTrendGroupVars_Requested <- target_clean_var_vector(TrendGroupVar)
  TargetTrendGroupVars_Valid <- TargetTrendGroupVars_Requested[
    TargetTrendGroupVars_Requested %in% names(data)
  ]
  TargetTrendGroupVars_Missing <- setdiff(
    TargetTrendGroupVars_Requested,
    TargetTrendGroupVars_Valid
  )

  TargetTrendGroupVar_Valid <- length(TargetTrendGroupVars_Valid) > 0L

  TargetGroupedTrendStatsList <- list()

  if (TargetVar_Valid && TargetVar_Usable && TargetProblemType %in% c("Binary", "Regression") && TargetTrendDateVar_Valid) {

    target_y <- data[[TargetVar_Name]]

    if (TargetProblemType == "Binary" && !target_is_numeric_col(target_y)) {

      target_levels <- sort(unique(as.character(target_y[!is.na(target_y)])))

      if (length(target_levels) >= 2L) {
        target_y_num <- as.integer(as.character(target_y) == target_levels[length(target_levels)])
      } else {
        target_y_num <- rep(NA_integer_, length(target_y))
      }

    } else {
      target_y_num <- target_y
    }

    data_target_trend <- data.table::copy(data)
    data_target_trend[, `.TargetValue` := target_y_num]

    # ----------------------------
    # Overall target trend
    # ----------------------------

    TargetTrendStats <- data_target_trend[
      !is.na(get(TrendDateVar)) &
        !is.na(`.TargetValue`) &
        is.finite(`.TargetValue`),
      .(
        N = as.integer(.N),
        TargetMean = target_safe_mean(`.TargetValue`),
        TargetMedian = target_safe_median(`.TargetValue`),
        TargetSD = target_safe_sd(`.TargetValue`)
      ),
      by = TrendDateVar
    ]

    data.table::setorderv(TargetTrendStats, TrendDateVar)

    if (nrow(TargetTrendStats) > 0L) {

      TargetTrendPlotList[["Target Overall Trend"]] <- AutoPlots::Area(
        dt = TargetTrendStats,
        XVar = TrendDateVar,
        YVar = "TargetMean",
        title.text = paste0(TargetVar_Name, " Overall Trend"),
        yAxis.title = ifelse(TargetProblemType == "Binary", "Target Rate", "Target Mean"),
        xAxis.title = TrendDateVar,
        Theme = Theme
      )
    }

    # ----------------------------
    # Grouped target trends
    # one plot per valid TrendGroupVar
    # ----------------------------

    if (TargetTrendGroupVar_Valid) {

      for (g in TargetTrendGroupVars_Valid) {

        data2 <- data_target_trend[
          !is.na(get(TrendDateVar)) &
            !is.na(get(g)) &
            !is.na(`.TargetValue`) &
            is.finite(`.TargetValue`),
          .(
            N = as.integer(.N),
            TargetMean = target_safe_mean(`.TargetValue`),
            TargetMedian = target_safe_median(`.TargetValue`),
            TargetSD = target_safe_sd(`.TargetValue`)
          ),
          by = c(TrendDateVar, g)
        ]

        if (nrow(data2) > 0L) {

          data.table::setorderv(
            data2,
            cols = c(g, TrendDateVar)
          )

          # Standardize grouped stats columns so multiple group variables
          # can be combined into one output table.
          data2[, TrendGroupVar := g]
          data.table::setnames(data2, old = g, new = "TrendGroupValue")

          data.table::setcolorder(
            data2,
            c(
              "TrendGroupVar",
              "TrendGroupValue",
              TrendDateVar,
              "N",
              "TargetMean",
              "TargetMedian",
              "TargetSD"
            )
          )

          TargetGroupedTrendStatsList[[g]] <- data2

          TargetGroupedTrendPlotList[[paste0("Target Trend by ", g)]] <- AutoPlots::Line(
            dt = data2,
            XVar = TrendDateVar,
            YVar = "TargetMean",
            GroupVar = "TrendGroupValue",
            title.text = paste0(TargetVar_Name, " Trend by ", g),
            yAxis.title = ifelse(TargetProblemType == "Binary", "Target Rate", "Target Mean"),
            xAxis.title = TrendDateVar,
            Theme = Theme
          )
        }
      }

      if (length(TargetGroupedTrendStatsList) > 0L) {
        TargetGroupedTrendStats <- data.table::rbindlist(
          TargetGroupedTrendStatsList,
          fill = TRUE
        )

        data.table::setorderv(
          TargetGroupedTrendStats,
          cols = c("TrendGroupVar", "TrendGroupValue", TrendDateVar)
        )
      }
    }
  }


  # ============================================================
  # Feature Drift Diagnostics
  # ============================================================

  if (TargetTrendDateVar_Valid && length(TargetCandidateVars) > 0L) {

    data_drift <- data.table::copy(data)
    data.table::setorderv(data_drift, TrendDateVar)

    n_rows_drift <- nrow(data_drift)
    recent_n <- max(1L, floor(n_rows_drift * TargetDriftRecentShare))
    early_n <- max(1L, recent_n)

    early_dt <- data_drift[seq_len(min(early_n, .N))]
    recent_dt <- data_drift[(.N - min(recent_n, .N) + 1L):.N]

    numeric_drift <- data.table::data.table()

    if (length(TargetNumericFeatureVars) > 0L) {

      numeric_drift <- data.table::rbindlist(
        lapply(TargetNumericFeatureVars, function(v) {

          early_x <- early_dt[[v]]
          recent_x <- recent_dt[[v]]

          early_mean <- mean(early_x, na.rm = TRUE)
          recent_mean <- mean(recent_x, na.rm = TRUE)

          data.table::data.table(
            Feature = v,
            FeatureType = "Numeric",
            `Early N` = sum(!is.na(early_x) & is.finite(early_x)),
            `Recent N` = sum(!is.na(recent_x) & is.finite(recent_x)),
            `Early Mean` = early_mean,
            `Recent Mean` = recent_mean,
            `Absolute Change` = recent_mean - early_mean,
            `Percent Change` = ifelse(is.na(early_mean) || early_mean == 0, NA_real_, (recent_mean - early_mean) / abs(early_mean) * 100),
            `Early Missing %` = mean(is.na(early_x)) * 100,
            `Recent Missing %` = mean(is.na(recent_x)) * 100,
            `Missing % Change` = (mean(is.na(recent_x)) - mean(is.na(early_x))) * 100
          )
        }),
        fill = TRUE
      )
    }

    categorical_drift <- data.table::data.table()

    if (length(TargetCategoricalFeatureVars) > 0L) {

      categorical_drift <- data.table::rbindlist(
        lapply(TargetCategoricalFeatureVars, function(v) {

          early_x <- as.character(early_dt[[v]])
          recent_x <- as.character(recent_dt[[v]])

          early_x[is.na(early_x)] <- "<NA>"
          recent_x[is.na(recent_x)] <- "<NA>"

          early_tab <- data.table::data.table(Level = early_x)[, .(EarlyN = .N), by = Level]
          recent_tab <- data.table::data.table(Level = recent_x)[, .(RecentN = .N), by = Level]

          out <- merge(early_tab, recent_tab, by = "Level", all = TRUE)
          out[is.na(EarlyN), EarlyN := 0L]
          out[is.na(RecentN), RecentN := 0L]

          out[, Feature := v]
          out[, FeatureType := "Categorical"]
          out[, `Early Share` := EarlyN / sum(EarlyN) * 100]
          out[, `Recent Share` := RecentN / sum(RecentN) * 100]
          out[, `Share Change` := `Recent Share` - `Early Share`]

          data.table::setorderv(out, "Share Change", order = -1L)

          if (nrow(out) > TargetMaxCategoricalLevels) {
            out <- out[seq_len(TargetMaxCategoricalLevels)]
          }

          out
        }),
        fill = TRUE
      )
    }

    TargetFeatureDriftStats <- data.table::rbindlist(
      list(numeric_drift, categorical_drift),
      fill = TRUE
    )
  }


  # ============================================================
  # Concept Drift Diagnostics
  # ============================================================

  if (TargetVar_Valid && TargetVar_Usable && TargetProblemType %in% c("Binary", "Regression") && TargetTrendDateVar_Valid && length(TargetNumericFeatureVars) > 0L) {

    target_y <- data[[TargetVar_Name]]

    if (TargetProblemType == "Binary" && !target_is_numeric_col(target_y)) {
      target_levels <- sort(unique(as.character(target_y[!is.na(target_y)])))
      target_y_num <- as.integer(as.character(target_y) == target_levels[length(target_levels)])
    } else {
      target_y_num <- target_y
    }

    data_concept <- data.table::copy(data)
    data_concept[, `.TargetValue` := target_y_num]

    date_values <- sort(unique(data_concept[[TrendDateVar]][!is.na(data_concept[[TrendDateVar]])]))

    if (length(date_values) >= TargetConceptDriftMinPeriods) {

      midpoint <- floor(length(date_values) / 2L)
      early_dates <- date_values[seq_len(midpoint)]
      recent_dates <- date_values[(midpoint + 1L):length(date_values)]

      early_concept <- data_concept[get(TrendDateVar) %in% early_dates]
      recent_concept <- data_concept[get(TrendDateVar) %in% recent_dates]

      TargetConceptDriftStats <- data.table::rbindlist(
        lapply(TargetNumericFeatureVars, function(v) {

          early_r <- target_safe_cor(early_concept[[v]], early_concept[[".TargetValue"]], method = "spearman")
          recent_r <- target_safe_cor(recent_concept[[v]], recent_concept[[".TargetValue"]], method = "spearman")

          early_n <- sum(
            !is.na(early_concept[[v]]) &
              !is.na(early_concept[[".TargetValue"]]) &
              is.finite(early_concept[[v]]) &
              is.finite(early_concept[[".TargetValue"]])
          )

          recent_n <- sum(
            !is.na(recent_concept[[v]]) &
              !is.na(recent_concept[[".TargetValue"]]) &
              is.finite(recent_concept[[v]]) &
              is.finite(recent_concept[[".TargetValue"]])
          )

          data.table::data.table(
            Feature = v,
            `Early N` = early_n,
            `Recent N` = recent_n,
            `Early Correlation` = early_r,
            `Recent Correlation` = recent_r,
            `Correlation Change` = recent_r - early_r,
            `Abs Correlation Change` = abs(recent_r - early_r),
            `Direction Changed` = ifelse(is.na(early_r) | is.na(recent_r), FALSE, sign(early_r) != sign(recent_r))
          )
        }),
        fill = TRUE
      )

      data.table::setorderv(
        TargetConceptDriftStats,
        cols = "Abs Correlation Change",
        order = -1L,
        na.last = TRUE
      )
    }
  }


  # ============================================================
  # Target Risk Flags
  # ============================================================

  TargetRiskFlags <- data.table::data.table(
    Feature = character(),
    RiskType = character(),
    RiskLevel = character(),
    Reason = character(),
    Evidence = character()
  )

  if (TargetVar_Valid && TargetVar_Usable) {

    suspicious_name_flags <- data.table::data.table(
      Feature = names(data)[vapply(names(data), target_risk_word_match, logical(1L))]
    )

    suspicious_name_flags <- suspicious_name_flags[Feature != TargetVar_Name]

    if (nrow(suspicious_name_flags) > 0L) {
      suspicious_name_flags[, `:=`(
        RiskType = "Name-based leakage risk",
        RiskLevel = "Medium",
        Reason = "Feature name contains a word commonly associated with outcome, status, decision, post-treatment, or label information.",
        Evidence = Feature
      )]

      TargetRiskFlags <- data.table::rbindlist(
        list(TargetRiskFlags, suspicious_name_flags),
        fill = TRUE
      )
    }

    if (nrow(TargetNumericAssociationStats) > 0L) {

      numeric_high_assoc <- TargetNumericAssociationStats[
        !is.na(`Abs Correlation`) & `Abs Correlation` >= 0.90
      ]

      if (nrow(numeric_high_assoc) > 0L) {
        numeric_high_assoc <- numeric_high_assoc[
          ,
          .(
            Feature,
            RiskType = "Extreme numeric target association",
            RiskLevel = "High",
            Reason = "Numeric feature has extremely high absolute association with the target.",
            Evidence = paste0("Abs correlation = ", target_fmt_num(`Abs Correlation`, 3L))
          )
        ]

        TargetRiskFlags <- data.table::rbindlist(
          list(TargetRiskFlags, numeric_high_assoc),
          fill = TRUE
        )
      }
    }

    if (nrow(TargetCategoricalAssociationStats) > 0L) {

      categorical_high_assoc <- TargetCategoricalAssociationStats[
        RiskLevel %in% c("High", "Medium")
      ]

      if (nrow(categorical_high_assoc) > 0L) {
        categorical_high_assoc <- categorical_high_assoc[
          ,
          .(
            Feature,
            RiskType = "Categorical target separation",
            RiskLevel,
            Reason = "Categorical feature has levels with unusually large target-rate or target-mean separation.",
            Evidence = paste0(
              "Max lift = ", target_fmt_num(`Max Lift`, 3L),
              "; min lift = ", target_fmt_num(`Min Lift`, 3L),
              "; target mean range = ", target_fmt_num(`Target Mean Range`, 3L)
            )
          )
        ]

        TargetRiskFlags <- data.table::rbindlist(
          list(TargetRiskFlags, categorical_high_assoc),
          fill = TRUE
        )
      }
    }

    if (nrow(TargetConceptDriftStats) > 0L) {

      concept_flags <- TargetConceptDriftStats[
        !is.na(`Abs Correlation Change`) & `Abs Correlation Change` >= 0.30
      ]

      if (nrow(concept_flags) > 0L) {
        concept_flags <- concept_flags[
          ,
          .(
            Feature,
            RiskType = "Potential concept drift",
            RiskLevel = ifelse(`Abs Correlation Change` >= 0.50, "High", "Medium"),
            Reason = "Feature-target relationship changed materially between early and recent periods.",
            Evidence = paste0(
              "Early corr = ", target_fmt_num(`Early Correlation`, 3L),
              "; recent corr = ", target_fmt_num(`Recent Correlation`, 3L),
              "; change = ", target_fmt_num(`Correlation Change`, 3L)
            )
          )
        ]

        TargetRiskFlags <- data.table::rbindlist(
          list(TargetRiskFlags, concept_flags),
          fill = TRUE
        )
      }
    }

    if (nrow(TargetRiskFlags) > 0L) {
      data.table::setorderv(
        TargetRiskFlags,
        cols = c("RiskLevel", "RiskType", "Feature"),
        order = c(1L, 1L, 1L)
      )
    }
  }


  # ============================================================
  # Target Reactable Artifacts
  # ============================================================


  target_safe_reactable <- function(data, ...) {
    if (is.null(data) || !is.data.frame(data) || ncol(data) == 0L) {
      return(NULL)
    }

    reactable::reactable(data = data, ...)
  }

  Target_QA <- target_safe_reactable(
    data = TargetQA,
    compact = TRUE,
    defaultPageSize = 25,
    wrap = TRUE,
    filterable = TRUE,
    fullWidth = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    resizable = TRUE,
    searchable = TRUE,
    sortable = TRUE,
    striped = FALSE,
    rowStyle = target_dark_row_style,
    bordered = FALSE,
    outlined = FALSE,
    defaultColDef = reactable::colDef(
      headerStyle = target_default_header_style,
      style = target_dark_cell_style
    ),
    columns = list(
      Metric = reactable::colDef(
        minWidth = 260,
        style = function(value, index) {
          out <- target_dark_cell_style(value, index)
          out$color <- "#F8FAFC"
          out$fontWeight <- "800"
          out
        }
      ),
      Count = reactable::colDef(
        minWidth = 100,
        align = "right",
        cell = target_integer_cell,
        style = target_dark_numeric_cell_style
      ),
      Value = reactable::colDef(
        minWidth = 500,
        cell = target_truncated_mono_cell,
        style = target_dark_cell_style
      )
    ),
    theme = target_theme
  )

  Target_Distribution <- target_safe_reactable(
    data = TargetDistributionTable,
    compact = TRUE,
    defaultPageSize = 25,
    pageSizeOptions = c(10, 25, 50, 100),
    wrap = FALSE,
    filterable = TRUE,
    fullWidth = TRUE,
    highlight = TRUE,
    pagination = TRUE,
    resizable = TRUE,
    searchable = TRUE,
    sortable = TRUE,
    striped = FALSE,
    rowStyle = target_dark_row_style,
    bordered = FALSE,
    outlined = FALSE,
    defaultColDef = reactable::colDef(
      headerStyle = target_default_header_style,
      style = target_dark_cell_style
    ),
    theme = target_theme
  )

  Target_Numeric_Association <- target_safe_reactable(
    data = TargetNumericAssociationStats,
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
    sortable = TRUE,
    striped = FALSE,
    rowStyle = target_dark_row_style,
    bordered = FALSE,
    outlined = FALSE,
    defaultColDef = reactable::colDef(
      minWidth = 105,
      headerStyle = target_default_header_style,
      style = target_dark_cell_style
    ),
    columns = list(
      Feature = reactable::colDef(
        sticky = "left",
        minWidth = 260,
        filterable = TRUE,
        style = target_dark_sticky_cell_style,
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
      N = reactable::colDef(align = "right", cell = target_integer_cell, style = target_dark_numeric_cell_style),
      `Feature NA N` = reactable::colDef(align = "right", cell = target_integer_cell, style = target_dark_numeric_cell_style),
      `Feature NA %` = reactable::colDef(align = "right", cell = target_pct_cell, style = target_dark_numeric_cell_style),
      `Target NA N` = reactable::colDef(align = "right", cell = target_integer_cell, style = target_dark_numeric_cell_style),
      Correlation = reactable::colDef(align = "center", cell = target_corr_cell, style = target_dark_center_cell_style),
      `Abs Correlation` = reactable::colDef(name = "|Corr|", align = "center", cell = target_abs_corr_cell, style = target_dark_center_cell_style),
      Direction = reactable::colDef(align = "center", cell = function(value) target_badge(value, ifelse(value == "Positive", "good", ifelse(value == "Negative", "warning", "neutral"))), style = target_dark_center_cell_style),
      Strength = reactable::colDef(align = "center", cell = function(value) target_badge(value, ifelse(value %in% c("Very Strong", "Strong"), "danger", ifelse(value == "Moderate", "warning", "neutral"))), style = target_dark_center_cell_style),
      `P Value` = reactable::colDef(align = "center", cell = function(value) target_numeric_cell(value, 4L), style = target_dark_center_cell_style),
      `Suspicious Name` = reactable::colDef(align = "center", cell = function(value) if (isTRUE(value)) target_badge("Yes", "danger") else target_badge("No", "good"), style = target_dark_center_cell_style)
    ),
    theme = target_theme
  )

  Target_Categorical_Association <- target_safe_reactable(
    data = TargetCategoricalAssociationStats,
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
    sortable = TRUE,
    striped = FALSE,
    rowStyle = target_dark_row_style,
    bordered = FALSE,
    outlined = FALSE,
    defaultColDef = reactable::colDef(
      minWidth = 105,
      headerStyle = target_default_header_style,
      style = target_dark_cell_style
    ),
    columns = list(
      Feature = reactable::colDef(
        sticky = "left",
        minWidth = 260,
        filterable = TRUE,
        style = target_dark_sticky_cell_style,
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
      Levels = reactable::colDef(align = "right", cell = target_integer_cell, style = target_dark_numeric_cell_style),
      N = reactable::colDef(align = "right", cell = target_integer_cell, style = target_dark_numeric_cell_style),
      `Max Target Mean` = reactable::colDef(align = "right", cell = target_numeric_cell, style = target_dark_numeric_cell_style),
      `Min Target Mean` = reactable::colDef(align = "right", cell = target_numeric_cell, style = target_dark_numeric_cell_style),
      `Target Mean Range` = reactable::colDef(align = "right", cell = target_numeric_cell, style = target_dark_numeric_cell_style),
      `Max Lift` = reactable::colDef(align = "center", cell = target_lift_cell, style = target_dark_center_cell_style),
      `Min Lift` = reactable::colDef(align = "center", cell = target_lift_cell, style = target_dark_center_cell_style),
      `Max Abs Lift From 1` = reactable::colDef(align = "right", cell = target_numeric_cell, style = target_dark_numeric_cell_style),
      `Extreme Level Count` = reactable::colDef(align = "right", cell = target_integer_cell, style = target_dark_numeric_cell_style),
      `Low N Level Count` = reactable::colDef(align = "right", cell = target_integer_cell, style = target_dark_numeric_cell_style),
      `Suspicious Name` = reactable::colDef(align = "center", cell = function(value) if (isTRUE(value)) target_badge("Yes", "danger") else target_badge("No", "good"), style = target_dark_center_cell_style),
      RiskLevel = reactable::colDef(align = "center", cell = target_flag_cell, style = target_dark_center_cell_style)
    ),
    theme = target_theme
  )

  Target_Categorical_Level_Association <- target_safe_reactable(
    data = TargetCategoricalLevelStats,
    compact = TRUE,
    defaultPageSize = 25,
    pageSizeOptions = c(10, 25, 50, 100),
    wrap = FALSE,
    filterable = TRUE,
    fullWidth = TRUE,
    highlight = TRUE,
    pagination = TRUE,
    resizable = TRUE,
    searchable = TRUE,
    sortable = TRUE,
    striped = FALSE,
    rowStyle = target_dark_row_style,
    bordered = FALSE,
    outlined = FALSE,
    defaultColDef = reactable::colDef(
      minWidth = 105,
      headerStyle = target_default_header_style,
      style = target_dark_cell_style
    ),
    columns = list(
      Feature = reactable::colDef(sticky = "left", minWidth = 240, style = target_dark_sticky_cell_style),
      Level = reactable::colDef(minWidth = 240, cell = target_truncated_mono_cell, style = target_dark_cell_style),
      N = reactable::colDef(align = "right", cell = target_integer_cell, style = target_dark_numeric_cell_style),
      `Level N %` = reactable::colDef(align = "right", cell = target_pct_cell, style = target_dark_numeric_cell_style),
      `Target Mean` = reactable::colDef(align = "right", cell = target_numeric_cell, style = target_dark_numeric_cell_style),
      Lift = reactable::colDef(align = "center", cell = target_lift_cell, style = target_dark_center_cell_style),
      `Abs Lift From 1` = reactable::colDef(align = "right", cell = target_numeric_cell, style = target_dark_numeric_cell_style),
      `Low N Flag` = reactable::colDef(align = "center", cell = function(value) if (isTRUE(value)) target_badge("Yes", "warning") else target_badge("No", "good"), style = target_dark_center_cell_style)
    ),
    theme = target_theme
  )

  Target_Feature_Drift <- target_safe_reactable(
    data = TargetFeatureDriftStats,
    compact = TRUE,
    defaultPageSize = 25,
    pageSizeOptions = c(10, 25, 50, 100),
    wrap = FALSE,
    filterable = TRUE,
    fullWidth = TRUE,
    highlight = TRUE,
    pagination = TRUE,
    resizable = TRUE,
    searchable = TRUE,
    sortable = TRUE,
    striped = FALSE,
    rowStyle = target_dark_row_style,
    bordered = FALSE,
    outlined = FALSE,
    defaultColDef = reactable::colDef(
      minWidth = 105,
      headerStyle = target_default_header_style,
      style = target_dark_cell_style
    ),
    theme = target_theme
  )

  Target_Concept_Drift <- target_safe_reactable(
    data = TargetConceptDriftStats,
    compact = TRUE,
    defaultPageSize = 25,
    pageSizeOptions = c(10, 25, 50, 100),
    wrap = FALSE,
    filterable = TRUE,
    fullWidth = TRUE,
    highlight = TRUE,
    pagination = TRUE,
    resizable = TRUE,
    searchable = TRUE,
    sortable = TRUE,
    striped = FALSE,
    rowStyle = target_dark_row_style,
    bordered = FALSE,
    outlined = FALSE,
    defaultColDef = reactable::colDef(
      minWidth = 105,
      headerStyle = target_default_header_style,
      style = target_dark_cell_style
    ),
    columns = list(
      Feature = reactable::colDef(sticky = "left", minWidth = 240, style = target_dark_sticky_cell_style),
      `Early N` = reactable::colDef(align = "right", cell = target_integer_cell, style = target_dark_numeric_cell_style),
      `Recent N` = reactable::colDef(align = "right", cell = target_integer_cell, style = target_dark_numeric_cell_style),
      `Early Correlation` = reactable::colDef(align = "center", cell = target_corr_cell, style = target_dark_center_cell_style),
      `Recent Correlation` = reactable::colDef(align = "center", cell = target_corr_cell, style = target_dark_center_cell_style),
      `Correlation Change` = reactable::colDef(align = "right", cell = target_numeric_cell, style = target_dark_numeric_cell_style),
      `Abs Correlation Change` = reactable::colDef(align = "right", cell = target_numeric_cell, style = target_dark_numeric_cell_style),
      `Direction Changed` = reactable::colDef(align = "center", cell = function(value) if (isTRUE(value)) target_badge("Yes", "warning") else target_badge("No", "good"), style = target_dark_center_cell_style)
    ),
    theme = target_theme
  )

  Target_Risk_Flags <- target_safe_reactable(
    data = TargetRiskFlags,
    compact = TRUE,
    defaultPageSize = 25,
    pageSizeOptions = c(10, 25, 50, 100),
    wrap = TRUE,
    filterable = TRUE,
    fullWidth = TRUE,
    highlight = TRUE,
    pagination = TRUE,
    resizable = TRUE,
    searchable = TRUE,
    sortable = TRUE,
    striped = FALSE,
    rowStyle = target_dark_row_style,
    bordered = FALSE,
    outlined = FALSE,
    defaultColDef = reactable::colDef(
      minWidth = 130,
      headerStyle = target_default_header_style,
      style = target_dark_cell_style
    ),
    columns = list(
      Feature = reactable::colDef(sticky = "left", minWidth = 240, style = target_dark_sticky_cell_style),
      RiskType = reactable::colDef(minWidth = 240, cell = target_text_cell, style = target_dark_cell_style),
      RiskLevel = reactable::colDef(align = "center", cell = target_flag_cell, style = target_dark_center_cell_style),
      Reason = reactable::colDef(minWidth = 500, cell = target_text_cell, style = target_dark_cell_style),
      Evidence = reactable::colDef(minWidth = 500, cell = target_text_cell, style = target_dark_cell_style)
    ),
    theme = target_theme
  )


  # ============================================================
  # Target-Oriented Plots
  # ============================================================

  # ----------------------------
  # Target distribution plot is already created above and stored in:
  # TargetDistributionPlot
  # TargetPlotList[["Target Distribution"]]
  # ----------------------------


  # ----------------------------
  # Numeric association plots
  # ----------------------------

  if (exists("TargetNumericAssociationStats") && nrow(TargetNumericAssociationStats) > 0L) {

    TargetNumericAssociationPlotData <- data.table::copy(
      TargetNumericAssociationStats[
        !is.na(`Abs Correlation`)
      ]
    )

    data.table::setorderv(
      TargetNumericAssociationPlotData,
      cols = c("Abs Correlation", "N"),
      order = c(-1L, -1L),
      na.last = TRUE
    )

    if (nrow(TargetNumericAssociationPlotData) > MaxCorrelationPairsToPlot) {
      TargetNumericAssociationPlotData <- TargetNumericAssociationPlotData[seq_len(MaxCorrelationPairsToPlot)]
    }

    if (nrow(TargetNumericAssociationPlotData) > 0L) {

      TargetAssociationPlotList[["Top Numeric Target Associations"]] <- AutoPlots::Bar(
        dt = TargetNumericAssociationPlotData,
        XVar = "Feature",
        YVar = "Abs Correlation",
        title.text = paste0("Top Numeric Associations with ", TargetVar_Name),
        yAxis.title = "|Spearman Correlation|",
        xAxis.title = NULL,
        Theme = Theme
      ) |> echarts4r::e_flip_coords()
    }
  }


  # ----------------------------
  # Categorical association plots
  # ----------------------------

  if (exists("TargetCategoricalAssociationStats") && nrow(TargetCategoricalAssociationStats) > 0L) {

    TargetCategoricalAssociationPlotData <- data.table::copy(
      TargetCategoricalAssociationStats[
        !is.na(`Max Abs Lift From 1`)
      ]
    )

    data.table::setorderv(
      TargetCategoricalAssociationPlotData,
      cols = c("Max Abs Lift From 1", "Target Mean Range"),
      order = c(-1L, -1L),
      na.last = TRUE
    )

    if (nrow(TargetCategoricalAssociationPlotData) > MaxCorrelationPairsToPlot) {
      TargetCategoricalAssociationPlotData <- TargetCategoricalAssociationPlotData[seq_len(MaxCorrelationPairsToPlot)]
    }

    if (nrow(TargetCategoricalAssociationPlotData) > 0L) {

      TargetAssociationPlotList[["Top Categorical Target Associations"]] <- AutoPlots::Bar(
        dt = TargetCategoricalAssociationPlotData,
        XVar = "Feature",
        YVar = "Max Abs Lift From 1",
        title.text = paste0("Top Categorical Associations with ", TargetVar_Name),
        yAxis.title = "Max |Lift - 1|",
        xAxis.title = NULL,
        Theme = Theme
      ) |> echarts4r::e_flip_coords()
    }
  }


  # ----------------------------
  # Categorical level lift plot
  # ----------------------------

  if (exists("TargetCategoricalLevelStats") && nrow(TargetCategoricalLevelStats) > 0L) {

    TargetCategoricalLevelPlotData <- data.table::copy(
      TargetCategoricalLevelStats[
        !is.na(`Abs Lift From 1`)
      ]
    )

    TargetCategoricalLevelPlotData[
      ,
      FeatureLevel := paste0(Feature, " = ", Level)
    ]

    data.table::setorderv(
      TargetCategoricalLevelPlotData,
      cols = c("Abs Lift From 1", "N"),
      order = c(-1L, -1L),
      na.last = TRUE
    )

    if (nrow(TargetCategoricalLevelPlotData) > MaxCorrelationPairsToPlot) {
      TargetCategoricalLevelPlotData <- TargetCategoricalLevelPlotData[seq_len(MaxCorrelationPairsToPlot)]
    }

    if (nrow(TargetCategoricalLevelPlotData) > 0L) {

      TargetAssociationPlotList[["Top Categorical Level Lifts"]] <- AutoPlots::Bar(
        dt = TargetCategoricalLevelPlotData,
        XVar = "FeatureLevel",
        YVar = "Abs Lift From 1",
        title.text = paste0("Top Categorical Level Lifts for ", TargetVar_Name),
        yAxis.title = "|Lift - 1|",
        xAxis.title = NULL,
        Theme = Theme
      ) |> echarts4r::e_flip_coords()
    }
  }


  # ----------------------------
  # Target overall trend plots already created above and stored in:
  # TargetTrendPlotList
  # ----------------------------


  # ----------------------------
  # Target grouped trend plots already created above and stored in:
  # TargetGroupedTrendPlotList
  # ----------------------------


  # ----------------------------
  # Numeric feature drift plots
  # ----------------------------

  if (exists("TargetFeatureDriftStats") && nrow(TargetFeatureDriftStats) > 0L) {

    TargetNumericFeatureDriftPlotData <- data.table::copy(
      TargetFeatureDriftStats[
        FeatureType == "Numeric" &
          !is.na(`Percent Change`)
      ]
    )

    if (nrow(TargetNumericFeatureDriftPlotData) > 0L) {

      TargetNumericFeatureDriftPlotData[
        ,
        `Abs Percent Change` := abs(`Percent Change`)
      ]

      data.table::setorderv(
        TargetNumericFeatureDriftPlotData,
        cols = "Abs Percent Change",
        order = -1L,
        na.last = TRUE
      )

      if (nrow(TargetNumericFeatureDriftPlotData) > MaxCorrelationPairsToPlot) {
        TargetNumericFeatureDriftPlotData <- TargetNumericFeatureDriftPlotData[seq_len(MaxCorrelationPairsToPlot)]
      }

      TargetDriftPlotList[["Top Numeric Feature Drift"]] <- AutoPlots::Bar(
        dt = TargetNumericFeatureDriftPlotData,
        XVar = "Feature",
        YVar = "Abs Percent Change",
        title.text = "Top Numeric Feature Drift",
        yAxis.title = "|Recent Mean % Change|",
        xAxis.title = NULL,
        Theme = Theme
      ) |> echarts4r::e_flip_coords()
    }
  }


  # ----------------------------
  # Numeric missingness drift plot
  # ----------------------------

  if (exists("TargetFeatureDriftStats") && nrow(TargetFeatureDriftStats) > 0L) {

    TargetMissingDriftPlotData <- data.table::copy(
      TargetFeatureDriftStats[
        FeatureType == "Numeric" &
          !is.na(`Missing % Change`)
      ]
    )

    if (nrow(TargetMissingDriftPlotData) > 0L) {

      TargetMissingDriftPlotData[
        ,
        `Abs Missing % Change` := abs(`Missing % Change`)
      ]

      data.table::setorderv(
        TargetMissingDriftPlotData,
        cols = "Abs Missing % Change",
        order = -1L,
        na.last = TRUE
      )

      if (nrow(TargetMissingDriftPlotData) > MaxCorrelationPairsToPlot) {
        TargetMissingDriftPlotData <- TargetMissingDriftPlotData[seq_len(MaxCorrelationPairsToPlot)]
      }

      TargetDriftPlotList[["Top Missingness Drift"]] <- AutoPlots::Bar(
        dt = TargetMissingDriftPlotData,
        XVar = "Feature",
        YVar = "Abs Missing % Change",
        title.text = "Top Feature Missingness Drift",
        yAxis.title = "|Recent Missing % - Early Missing %|",
        xAxis.title = NULL,
        Theme = Theme
      ) |> echarts4r::e_flip_coords()
    }
  }


  # ----------------------------
  # Categorical feature share drift plot
  # ----------------------------

  if (exists("TargetFeatureDriftStats") && nrow(TargetFeatureDriftStats) > 0L) {

    TargetCategoricalDriftPlotData <- data.table::copy(
      TargetFeatureDriftStats[
        FeatureType == "Categorical" &
          !is.na(`Share Change`)
      ]
    )

    if (nrow(TargetCategoricalDriftPlotData) > 0L) {

      TargetCategoricalDriftPlotData[
        ,
        FeatureLevel := paste0(Feature, " = ", Level)
      ]

      TargetCategoricalDriftPlotData[
        ,
        `Abs Share Change` := abs(`Share Change`)
      ]

      data.table::setorderv(
        TargetCategoricalDriftPlotData,
        cols = "Abs Share Change",
        order = -1L,
        na.last = TRUE
      )

      if (nrow(TargetCategoricalDriftPlotData) > MaxCorrelationPairsToPlot) {
        TargetCategoricalDriftPlotData <- TargetCategoricalDriftPlotData[seq_len(MaxCorrelationPairsToPlot)]
      }

      TargetDriftPlotList[["Top Categorical Share Drift"]] <- AutoPlots::Bar(
        dt = TargetCategoricalDriftPlotData,
        XVar = "FeatureLevel",
        YVar = "Abs Share Change",
        title.text = "Top Categorical Level Share Drift",
        yAxis.title = "|Recent Share - Early Share|",
        xAxis.title = NULL,
        Theme = Theme
      ) |> echarts4r::e_flip_coords()
    }
  }


  # ----------------------------
  # Concept drift plot
  # ----------------------------

  if (exists("TargetConceptDriftStats") && nrow(TargetConceptDriftStats) > 0L) {

    TargetConceptDriftPlotData <- data.table::copy(
      TargetConceptDriftStats[
        !is.na(`Abs Correlation Change`)
      ]
    )

    data.table::setorderv(
      TargetConceptDriftPlotData,
      cols = "Abs Correlation Change",
      order = -1L,
      na.last = TRUE
    )

    if (nrow(TargetConceptDriftPlotData) > MaxCorrelationPairsToPlot) {
      TargetConceptDriftPlotData <- TargetConceptDriftPlotData[seq_len(MaxCorrelationPairsToPlot)]
    }

    if (nrow(TargetConceptDriftPlotData) > 0L) {

      TargetDriftPlotList[["Top Concept Drift"]] <- AutoPlots::Bar(
        dt = TargetConceptDriftPlotData,
        XVar = "Feature",
        YVar = "Abs Correlation Change",
        title.text = paste0("Top Concept Drift Against ", TargetVar_Name),
        yAxis.title = "|Recent Corr - Early Corr|",
        xAxis.title = NULL,
        Theme = Theme
      ) |> echarts4r::e_flip_coords()
    }
  }


  # ----------------------------
  # Risk flag plots
  # ----------------------------

  if (exists("TargetRiskFlags") && nrow(TargetRiskFlags) > 0L) {

    TargetRiskLevelPlotData <- TargetRiskFlags[
      ,
      .(N = .N),
      by = RiskLevel
    ]

    data.table::setorderv(
      TargetRiskLevelPlotData,
      cols = "N",
      order = -1L
    )

    TargetRiskPlotList[["Risk Flags by Severity"]] <- AutoPlots::Bar(
      dt = TargetRiskLevelPlotData,
      XVar = "RiskLevel",
      YVar = "N",
      title.text = "Target Risk Flags by Severity",
      yAxis.title = "Flag Count",
      xAxis.title = NULL,
      Theme = Theme
    ) |> echarts4r::e_flip_coords()

    TargetRiskTypePlotData <- TargetRiskFlags[
      ,
      .(N = .N),
      by = RiskType
    ]

    data.table::setorderv(
      TargetRiskTypePlotData,
      cols = "N",
      order = -1L
    )

    TargetRiskPlotList[["Risk Flags by Type"]] <- AutoPlots::Bar(
      dt = TargetRiskTypePlotData,
      XVar = "RiskType",
      YVar = "N",
      title.text = "Target Risk Flags by Type",
      yAxis.title = "Flag Count",
      xAxis.title = NULL,
      Theme = Theme
    ) |> echarts4r::e_flip_coords()
  }


  # ----------------------------
  # Master target plot list
  # ----------------------------

  TargetPlotList <- c(
    TargetPlotList,
    TargetAssociationPlotList,
    TargetTrendPlotList,
    TargetGroupedTrendPlotList,
    TargetDriftPlotList,
    TargetRiskPlotList
  )

  # ----------------------------
  # Target plot QA
  # ----------------------------

  TargetPlotQA <- data.table::data.table(
    PlotSection = c(
      "Target Distribution Plots",
      "Target Association Plots",
      "Target Trend Plots",
      "Target Grouped Trend Plots",
      "Target Drift Plots",
      "Target Risk Plots",
      "Total Target Plots"
    ),
    Count = c(
      as.integer(!is.null(TargetDistributionPlot)),
      length(TargetAssociationPlotList),
      length(TargetTrendPlotList),
      length(TargetGroupedTrendPlotList),
      length(TargetDriftPlotList),
      length(TargetRiskPlotList),
      length(TargetPlotList)
    ),
    Values = c(
      ifelse(!is.null(TargetDistributionPlot), "Target Distribution", ""),
      paste(names(TargetAssociationPlotList), collapse = ", "),
      paste(names(TargetTrendPlotList), collapse = ", "),
      paste(names(TargetGroupedTrendPlotList), collapse = ", "),
      paste(names(TargetDriftPlotList), collapse = ", "),
      paste(names(TargetRiskPlotList), collapse = ", "),
      paste(names(TargetPlotList), collapse = ", ")
    )
  )



  # ============================================================
  # Model Readiness / Target-Aware Modeling Intelligence
  # ============================================================

  target_readiness_reactable <- function(dt, default_page_size = 15L) {
    if (is.null(dt) || !data.table::is.data.table(dt) || nrow(dt) == 0L) {
      return(NULL)
    }

    reactable::reactable(
      data = dt,
      compact = TRUE,
      defaultPageSize = default_page_size,
      pageSizeOptions = c(10, 15, 25, 50, 100),
      wrap = FALSE,
      filterable = TRUE,
      searchable = TRUE,
      fullWidth = TRUE,
      highlight = TRUE,
      pagination = TRUE,
      resizable = TRUE,
      selection = "multiple",
      showPagination = TRUE,
      showPageSizeOptions = TRUE,
      showSortable = TRUE,
      showSortIcon = TRUE,
      sortable = TRUE,
      striped = FALSE,
      bordered = FALSE,
      outlined = FALSE,
      rowStyle = target_dark_row_style,
      defaultColDef = reactable::colDef(
        minWidth = 130,
        headerStyle = target_default_header_style,
        style = target_dark_cell_style
      ),
      theme = target_theme
    )
  }

  target_flag_score <- function(level) {
    level <- as.character(level)
    fifelse(
      level == "High", 4L,
      fifelse(
        level == "Medium", 3L,
        fifelse(
          level == "Low", 2L,
          fifelse(level %in% c("None", "Safe", "Not available"), 0L, 1L)
        )
      )
    )
  }

  target_cardinality_band <- function(n_levels) {
    if (is.na(n_levels)) {
      return("Not available")
    }
    if (n_levels <= 2L) {
      "Binary"
    } else if (n_levels <= 10L) {
      "Low"
    } else if (n_levels <= 50L) {
      "Medium"
    } else if (n_levels <= 250L) {
      "High"
    } else {
      "Extreme"
    }
  }

  target_encoding_recommendation <- function(cardinality, low_n_count, risk_level, objective, preferred_model) {
    cardinality <- target_safe_num(cardinality)
    low_n_count <- target_safe_num(low_n_count)
    risk_level <- target_safe_chr(risk_level, empty_value = "None")

    if (risk_level %in% c("High")) {
      return("Review or exclude before encoding")
    }

    if (preferred_model == "catboost") {
      if (!is.na(cardinality) && cardinality > 2L) {
        if (!is.na(low_n_count) && low_n_count > 0L) {
          return("CatBoost native categorical; consider rare-level consolidation")
        }
        return("CatBoost native categorical")
      }
      return("Binary indicator or CatBoost native categorical")
    }

    if (!is.na(cardinality) && cardinality <= 10L) {
      if (objective == "explanation") {
        return("Consolidate sparse levels, then dummy encode if needed")
      }
      return("Dummy encode or native categorical if supported")
    }

    if (!is.na(cardinality) && cardinality <= 50L) {
      if (objective == "explanation") {
        return("Consolidate levels; consider supervised grouping before dummy encoding")
      }
      return("James-Stein / target encoding with leakage-safe CV")
    }

    if (objective == "explanation") {
      "Avoid raw high-cardinality dummies; consolidate or exclude unless defensible"
    } else {
      "Target / James-Stein / hashing with leakage-safe validation"
    }
  }

  target_numeric_transform_recommendation <- function(skewness, zero_share, missing_pct, nonlinearity_flag, objective, preferred_model) {
    recs <- character(0L)

    if (!is.na(missing_pct) && missing_pct > 0) {
      recs <- c(recs, "Evaluate missingness indicator")
    }

    if (!is.na(zero_share) && zero_share >= 25) {
      recs <- c(recs, "Consider zero flag + positive-value transform")
    }

    if (!is.na(skewness) && abs(skewness) >= 2) {
      if (preferred_model == "catboost") {
        recs <- c(recs, "Skew noted; monotonic transforms usually unnecessary for CatBoost")
      } else {
        recs <- c(recs, "Consider log/sqrt/Yeo-Johnson transform")
      }
    }

    if (isTRUE(nonlinearity_flag)) {
      if (objective == "explanation") {
        recs <- c(recs, "Use spline/GAM term or piecewise effect")
      } else if (preferred_model == "catboost") {
        recs <- c(recs, "CatBoost can model nonlinear thresholds directly")
      } else {
        recs <- c(recs, "Consider spline/binning/tree model")
      }
    }

    if (!length(recs)) {
      recs <- "No major transformation indicated"
    }

    paste(unique(recs), collapse = "; ")
  }

  target_explanatory_status <- function(primary_risk, assoc_strength, suspicious_name, concept_change) {
    if (!is.na(primary_risk) && primary_risk == "High") {
      return("Exclude or quarantine")
    }
    if (isTRUE(suspicious_name)) {
      return("Business review required")
    }
    if (!is.na(concept_change) && concept_change >= 0.30) {
      return("Use with caveat; relationship unstable")
    }
    if (assoc_strength %in% c("Strong", "Very Strong")) {
      return("Usable, but validate causal / timing interpretation")
    }
    "Generally usable"
  }

  target_prediction_status <- function(primary_risk, availability_risk, drift_risk) {
    if (!is.na(primary_risk) && primary_risk == "High") {
      return("Review before production")
    }
    if (!is.na(availability_risk) && availability_risk == "High") {
      return("Only use if available at scoring time")
    }
    if (!is.na(drift_risk) && drift_risk %in% c("High", "Medium")) {
      return("Usable with monitoring")
    }
    "Generally usable"
  }

  target_skewness <- function(x) {
    x <- x[!is.na(x) & is.finite(x)]
    if (length(x) < 3L) {
      return(NA_real_)
    }
    m <- mean(x)
    s <- stats::sd(x)
    if (is.na(s) || s == 0) {
      return(NA_real_)
    }
    mean(((x - m) / s)^3)
  }

  target_kurtosis <- function(x) {
    x <- x[!is.na(x) & is.finite(x)]
    if (length(x) < 4L) {
      return(NA_real_)
    }
    m <- mean(x)
    s <- stats::sd(x)
    if (is.na(s) || s == 0) {
      return(NA_real_)
    }
    mean(((x - m) / s)^4)
  }

  target_top_n <- function(dt, n = 10L, order_col = NULL) {
    if (is.null(dt) || !data.table::is.data.table(dt) || nrow(dt) == 0L) {
      return(data.table::data.table())
    }
    out <- data.table::copy(dt)
    if (!is.null(order_col) && order_col %in% names(out)) {
      data.table::setorderv(out, order_col, order = -1L, na.last = TRUE)
    }
    out[seq_len(min(nrow(out), n))]
  }


  target_missing_value_for_template <- function(template) {
    if (is.logical(template)) {
      NA
    } else if (is.integer(template)) {
      NA_integer_
    } else if (is.numeric(template)) {
      NA_real_
    } else {
      NA_character_
    }
  }

  target_ensure_dt_columns <- function(dt, schema) {

    if (is.null(dt) || !data.table::is.data.table(dt)) {
      dt <- data.table::data.table()
    } else {
      dt <- data.table::copy(dt)
    }

    for (nm in names(schema)) {
      if (!nm %in% names(dt)) {
        template <- schema[[nm]]

        if (nrow(dt) == 0L) {
          dt[, (nm) := template[0L]]
        } else {
          dt[, (nm) := target_missing_value_for_template(template)]
        }
      }
    }

    dt
  }

  # Normalize optional source tables before downstream readiness joins.
  # Several target-readiness sections can legitimately be empty depending on
  # target type and available feature classes. These schemas prevent data.table
  # expressions such as DT[, .(Feature, ...)] from failing when a section is
  # empty or unsupported.
  TargetNumericAssociationStats <- target_ensure_dt_columns(
    TargetNumericAssociationStats,
    list(
      Feature = character(),
      Correlation = numeric(),
      `Abs Correlation` = numeric(),
      Direction = character(),
      Strength = character(),
      `P Value` = numeric(),
      `Suspicious Name` = logical()
    )
  )

  TargetCategoricalAssociationStats <- target_ensure_dt_columns(
    TargetCategoricalAssociationStats,
    list(
      Feature = character(),
      Levels = integer(),
      `Max Target Mean` = numeric(),
      `Min Target Mean` = numeric(),
      `Target Mean Range` = numeric(),
      `Max Lift` = numeric(),
      `Min Lift` = numeric(),
      `Max Abs Lift From 1` = numeric(),
      `Extreme Level Count` = integer(),
      `Low N Level Count` = integer(),
      `Suspicious Name` = logical(),
      RiskLevel = character()
    )
  )

  TargetCategoricalLevelStats <- target_ensure_dt_columns(
    TargetCategoricalLevelStats,
    list(
      Feature = character(),
      Level = character(),
      N = integer(),
      `Target Mean` = numeric(),
      Lift = numeric(),
      `Abs Lift From 1` = numeric(),
      `Low N Flag` = logical(),
      `Suspicious Name` = logical()
    )
  )

  TargetFeatureDriftStats <- target_ensure_dt_columns(
    TargetFeatureDriftStats,
    list(
      Feature = character(),
      FeatureType = character(),
      `Percent Change` = numeric(),
      `Share Change` = numeric(),
      `Missing % Change` = numeric()
    )
  )

  TargetConceptDriftStats <- target_ensure_dt_columns(
    TargetConceptDriftStats,
    list(
      Feature = character(),
      `Early Correlation` = numeric(),
      `Recent Correlation` = numeric(),
      `Correlation Change` = numeric(),
      `Abs Correlation Change` = numeric(),
      `Direction Changed` = logical()
    )
  )

  TargetRiskFlags <- target_ensure_dt_columns(
    TargetRiskFlags,
    list(
      Feature = character(),
      RiskType = character(),
      RiskLevel = character(),
      Reason = character(),
      Evidence = character()
    )
  )

  # ----------------------------
  # Numeric feature distribution + shape diagnostics
  # ----------------------------

  TargetNumericFeatureDiagnostics <- data.table::data.table()

  if (length(TargetNumericFeatureVars) > 0L) {
    TargetNumericFeatureDiagnostics <- data.table::rbindlist(
      lapply(TargetNumericFeatureVars, function(v) {
        x <- data[[v]]
        x_valid <- x[!is.na(x) & is.finite(x)]
        n_valid <- length(x_valid)
        q <- if (n_valid > 0L) {
          suppressWarnings(stats::quantile(x_valid, probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99), na.rm = TRUE, names = FALSE))
        } else {
          rep(NA_real_, 7L)
        }
        data.table::data.table(
          Feature = v,
          FeatureType = class(x)[1L],
          N = n_valid,
          `Missing N` = sum(is.na(x)),
          `Missing %` = mean(is.na(x)) * 100,
          `Unique N` = data.table::uniqueN(x, na.rm = TRUE),
          Mean = target_safe_mean(x),
          Median = target_safe_median(x),
          SD = target_safe_sd(x),
          Min = target_safe_min(x),
          Q01 = q[1L],
          Q05 = q[2L],
          Q25 = q[3L],
          Q50 = q[4L],
          Q75 = q[5L],
          Q95 = q[6L],
          Q99 = q[7L],
          Max = target_safe_max(x),
          Skewness = target_skewness(x),
          Kurtosis = target_kurtosis(x),
          `Zero N` = sum(!is.na(x) & is.finite(x) & x == 0),
          `Zero %` = mean(!is.na(x) & is.finite(x) & x == 0) * 100,
          `Outlier IQR N` = {
            if (n_valid >= 4L) {
              iqr <- q[5L] - q[3L]
              lo <- q[3L] - 1.5 * iqr
              hi <- q[5L] + 1.5 * iqr
              sum(x_valid < lo | x_valid > hi)
            } else {
              NA_integer_
            }
          },
          `Suspicious Name` = target_risk_word_match(v)
        )
      }),
      fill = TRUE
    )
  }

  TargetNumericShapeDiagnostics <- data.table::data.table()

  if (
    isTRUE(RunGAMDiagnostics) &&
    requireNamespace("mgcv", quietly = TRUE) &&
    TargetVar_Valid &&
    TargetVar_Usable &&
    TargetProblemType %in% c("Binary", "Regression") &&
    length(TargetNumericFeatureVars) > 0L
  ) {

    target_y <- data[[TargetVar_Name]]

    if (TargetProblemType == "Binary" && !target_is_numeric_col(target_y)) {
      target_levels <- sort(unique(as.character(target_y[!is.na(target_y)])))
      if (length(target_levels) >= 2L) {
        target_y_num <- as.integer(as.character(target_y) == target_levels[length(target_levels)])
      } else {
        target_y_num <- rep(NA_integer_, length(target_y))
      }
    } else {
      target_y_num <- target_y
    }

    gam_candidate_vars <- if (nrow(TargetNumericAssociationStats) > 0L) {
      target_top_n(TargetNumericAssociationStats, n = MaxGAMFeatures, order_col = "Abs Correlation")$Feature
    } else {
      TargetNumericFeatureVars[seq_len(min(length(TargetNumericFeatureVars), MaxGAMFeatures))]
    }

    TargetNumericShapeDiagnostics <- data.table::rbindlist(
      lapply(gam_candidate_vars, function(v) {
        x <- data[[v]]
        keep <- !is.na(x) & !is.na(target_y_num) & is.finite(x) & is.finite(target_y_num)

        if (sum(keep) > GAMSampleSize) {
          set.seed(8675309L)
          idx <- which(keep)
          idx <- sample(idx, GAMSampleSize)
          keep <- rep(FALSE, length(x))
          keep[idx] <- TRUE
        }

        dt_gam <- data.table::data.table(
          y = target_y_num[keep],
          x = x[keep]
        )

        dt_gam <- dt_gam[!is.na(y) & !is.na(x) & is.finite(y) & is.finite(x)]

        if (nrow(dt_gam) < 50L || data.table::uniqueN(dt_gam$x) < 5L || data.table::uniqueN(dt_gam$y) < 2L) {
          return(data.table::data.table(
            Feature = v,
            N = nrow(dt_gam),
            LinearAIC = NA_real_,
            GAMAIC = NA_real_,
            DeltaAIC = NA_real_,
            GAMEDF = NA_real_,
            GAMDevianceExplained = NA_real_,
            NonlinearityScore = NA_real_,
            NonlinearityFlag = FALSE,
            RecommendedTreatment = "Insufficient variation for GAM diagnostic"
          ))
        }

        fam <- if (TargetProblemType == "Binary") stats::binomial() else stats::gaussian()

        linear_fit <- tryCatch(
          stats::glm(y ~ x, data = dt_gam, family = fam),
          error = function(e) NULL
        )

        gam_fit <- tryCatch(
          mgcv::gam(y ~ s(x, k = min(10L, max(4L, data.table::uniqueN(dt_gam$x) - 1L))), data = dt_gam, family = fam, method = "REML"),
          error = function(e) NULL
        )

        linear_aic <- if (!is.null(linear_fit)) suppressWarnings(stats::AIC(linear_fit)) else NA_real_
        gam_aic <- if (!is.null(gam_fit)) suppressWarnings(stats::AIC(gam_fit)) else NA_real_
        delta_aic <- linear_aic - gam_aic

        gam_summary <- if (!is.null(gam_fit)) tryCatch(summary(gam_fit), error = function(e) NULL) else NULL
        edf <- if (!is.null(gam_summary) && !is.null(gam_summary$s.table)) {
          as.numeric(gam_summary$s.table[1L, "edf"])
        } else {
          NA_real_
        }
        dev_exp <- if (!is.null(gam_summary) && !is.null(gam_summary$dev.expl)) {
          as.numeric(gam_summary$dev.expl) * 100
        } else {
          NA_real_
        }

        nonlinearity_score <- ifelse(is.na(delta_aic), NA_real_, delta_aic)
        nonlinearity_flag <- !is.na(delta_aic) && delta_aic >= 10 && !is.na(edf) && edf >= 2.0

        data.table::data.table(
          Feature = v,
          N = nrow(dt_gam),
          LinearAIC = linear_aic,
          GAMAIC = gam_aic,
          DeltaAIC = delta_aic,
          GAMEDF = edf,
          GAMDevianceExplained = dev_exp,
          NonlinearityScore = nonlinearity_score,
          NonlinearityFlag = nonlinearity_flag,
          RecommendedTreatment = target_numeric_transform_recommendation(
            skewness = TargetNumericFeatureDiagnostics[Feature == v, Skewness][1L],
            zero_share = TargetNumericFeatureDiagnostics[Feature == v, `Zero %`][1L],
            missing_pct = TargetNumericFeatureDiagnostics[Feature == v, `Missing %`][1L],
            nonlinearity_flag = nonlinearity_flag,
            objective = ModelingObjective,
            preferred_model = PreferredModelFamily
          )
        )
      }),
      fill = TRUE
    )

    if (nrow(TargetNumericShapeDiagnostics) > 0L) {
      data.table::setorderv(TargetNumericShapeDiagnostics, "NonlinearityScore", order = -1L, na.last = TRUE)
    }
  }

  # ----------------------------
  # Categorical diagnostics + encoding guidance
  # ----------------------------

  TargetCategoricalFeatureDiagnostics <- data.table::data.table()

  if (length(TargetCategoricalFeatureVars) > 0L) {
    TargetCategoricalFeatureDiagnostics <- data.table::rbindlist(
      lapply(TargetCategoricalFeatureVars, function(v) {
        x <- as.character(data[[v]])
        x[is.na(x)] <- "<NA>"
        x[trimws(x) == ""] <- "<EMPTY STRING>"
        tab <- data.table::data.table(Level = x)[, .(N = .N), by = Level]
        tab[, Share := N / sum(N) * 100]
        p <- tab$N / sum(tab$N)
        entropy <- -sum(p * log(p), na.rm = TRUE)
        effective_levels <- exp(entropy)
        data.table::data.table(
          Feature = v,
          FeatureType = class(data[[v]])[1L],
          Levels = nrow(tab),
          CardinalityBand = target_cardinality_band(nrow(tab)),
          EffectiveLevels = effective_levels,
          TopLevel = tab[order(-N), Level][1L],
          TopLevelShare = max(tab$Share, na.rm = TRUE),
          RareLevelCount = sum(tab$N < TargetMinLevelN),
          RareLevelShare = sum(tab[N < TargetMinLevelN, N]) / sum(tab$N) * 100,
          SingletonLevelCount = sum(tab$N == 1L),
          MissingLevelPresent = any(tab$Level == "<NA>"),
          EmptyStringLevelPresent = any(tab$Level == "<EMPTY STRING>"),
          SuspiciousName = target_risk_word_match(v)
        )
      }),
      fill = TRUE
    )
  }

  TargetCategoricalEncodingRecommendations <- data.table::data.table()

  if (nrow(TargetCategoricalFeatureDiagnostics) > 0L) {
    TargetCategoricalEncodingRecommendations <- merge(
      TargetCategoricalFeatureDiagnostics,
      TargetCategoricalAssociationStats[
        ,
        .(
          Feature,
          LevelsWithTarget = Levels,
          MaxLift = `Max Lift`,
          MinLift = `Min Lift`,
          MaxAbsLiftFrom1 = `Max Abs Lift From 1`,
          ExtremeLevelCount = `Extreme Level Count`,
          LowNLevelCount = `Low N Level Count`,
          TargetRiskLevel = RiskLevel
        )
      ],
      by = "Feature",
      all.x = TRUE
    )

    TargetCategoricalEncodingRecommendations[, RecommendedEncoding := mapply(
      target_encoding_recommendation,
      cardinality = Levels,
      low_n_count = RareLevelCount,
      risk_level = TargetRiskLevel,
      MoreArgs = list(
        objective = ModelingObjective,
        preferred_model = PreferredModelFamily
      )
    )]

    TargetCategoricalEncodingRecommendations[, RecommendedPreprocessing := fifelse(
      RareLevelCount > 0L | SingletonLevelCount > 0L,
      "Create rare/other bucket before non-CatBoost encodings",
      "No level consolidation required by frequency"
    )]

    TargetCategoricalEncodingRecommendations[, ExplanatoryCaution := fifelse(
      TargetRiskLevel %in% c("High", "Medium") | SuspiciousName,
      "Review for leakage, collider, or post-treatment meaning before explanatory use",
      "No immediate explanatory-use flag from available diagnostics"
    )]

    data.table::setorderv(
      TargetCategoricalEncodingRecommendations,
      cols = c("TargetRiskLevel", "MaxAbsLiftFrom1", "Levels"),
      order = c(1L, -1L, -1L),
      na.last = TRUE
    )
  }

  # ----------------------------
  # Numeric feature engineering guidance
  # ----------------------------

  TargetNumericEngineeringRecommendations <- data.table::data.table()

  if (nrow(TargetNumericFeatureDiagnostics) > 0L) {
    TargetNumericEngineeringRecommendations <- merge(
      TargetNumericFeatureDiagnostics,
      TargetNumericAssociationStats[
        ,
        .(
          Feature,
          TargetCorrelation = Correlation,
          AbsTargetCorrelation = `Abs Correlation`,
          Direction,
          Strength,
          PValue = `P Value`,
          NumericSuspiciousName = `Suspicious Name`
        )
      ],
      by = "Feature",
      all.x = TRUE
    )

    if (nrow(TargetNumericShapeDiagnostics) > 0L) {
      TargetNumericEngineeringRecommendations <- merge(
        TargetNumericEngineeringRecommendations,
        TargetNumericShapeDiagnostics[
          ,
          .(
            Feature,
            GAMEDF,
            GAMDevianceExplained,
            NonlinearityScore,
            NonlinearityFlag,
            GAMRecommendedTreatment = RecommendedTreatment
          )
        ],
        by = "Feature",
        all.x = TRUE
      )
    }

    # Keep the numeric engineering schema stable even when GAM diagnostics
    # are disabled, mgcv is unavailable, or no numeric feature has enough
    # variation for a shape diagnostic. This does not suppress the feature;
    # it records that no nonlinear-shape evidence was produced.
    target_ensure_col <- function(dt, name, value) {
      if (!name %in% names(dt)) {
        dt[, (name) := value]
      }
      invisible(dt)
    }

    target_ensure_col(TargetNumericEngineeringRecommendations, "GAMEDF", NA_real_)
    target_ensure_col(TargetNumericEngineeringRecommendations, "GAMDevianceExplained", NA_real_)
    target_ensure_col(TargetNumericEngineeringRecommendations, "NonlinearityScore", NA_real_)
    target_ensure_col(TargetNumericEngineeringRecommendations, "NonlinearityFlag", FALSE)
    target_ensure_col(TargetNumericEngineeringRecommendations, "GAMRecommendedTreatment", NA_character_)

    TargetNumericEngineeringRecommendations[, NonlinearityFlag := fifelse(is.na(NonlinearityFlag), FALSE, NonlinearityFlag)]

    TargetNumericEngineeringRecommendations[, RecommendedTreatment := mapply(
      target_numeric_transform_recommendation,
      skewness = Skewness,
      zero_share = `Zero %`,
      missing_pct = `Missing %`,
      nonlinearity_flag = NonlinearityFlag,
      MoreArgs = list(
        objective = ModelingObjective,
        preferred_model = PreferredModelFamily
      )
    )]

    TargetNumericEngineeringRecommendations[, TreeModelTransformValue := fifelse(
      PreferredModelFamily %in% c("catboost", "xgboost", "lightgbm", "tree", "randomforest"),
      "Monotonic transforms generally do not improve tree split capacity; use only for robustness, constraints, reporting, or downstream linear models.",
      "Transforms may improve linearity, variance behavior, or coefficient interpretability."
    )]

    data.table::setorderv(
      TargetNumericEngineeringRecommendations,
      cols = c("AbsTargetCorrelation", "NonlinearityScore"),
      order = c(-1L, -1L),
      na.last = TRUE
    )
  }

  # ----------------------------
  # Calendar / time feature guidance
  # ----------------------------

  TargetCalendarFeatureDiagnostics <- data.table::data.table()
  TargetCalendarEngineeringRecommendations <- data.table::data.table()

  TargetDateVars <- names(data)[
    vapply(data, target_is_date_col, logical(1L))
  ]

  if (length(TargetDateVars) > 0L) {
    TargetCalendarFeatureDiagnostics <- data.table::rbindlist(
      lapply(TargetDateVars, function(v) {
        x <- data[[v]]
        x_non_missing <- x[!is.na(x)]
        data.table::data.table(
          Feature = v,
          FeatureType = class(x)[1L],
          N = length(x_non_missing),
          `Missing N` = sum(is.na(x)),
          `Missing %` = mean(is.na(x)) * 100,
          MinDate = if (length(x_non_missing)) as.character(min(x_non_missing)) else NA_character_,
          MaxDate = if (length(x_non_missing)) as.character(max(x_non_missing)) else NA_character_,
          UniqueDates = data.table::uniqueN(as.Date(x_non_missing)),
          SuspiciousName = target_risk_word_match(v),
          IsTrendDateVar = !is.null(TrendDateVar) && v == TrendDateVar
        )
      }),
      fill = TRUE
    )

    TargetCalendarEngineeringRecommendations <- data.table::copy(TargetCalendarFeatureDiagnostics)
    TargetCalendarEngineeringRecommendations[, RecommendedFeatures := paste(
      c(
        "trend index / recency",
        "day-of-week",
        "month / quarter",
        "holiday / event flags",
        "cyclic sin/cos terms where periodicity matters",
        "lags / rolling summaries only when available before scoring"
      ),
      collapse = "; "
    )]

    TargetCalendarEngineeringRecommendations[, ValidationImplication := fifelse(
      IsTrendDateVar,
      "Prefer time-based validation, blocked CV, or forward-chaining split",
      "Consider time leakage if derived from post-outcome or unavailable event timestamps"
    )]

    TargetCalendarEngineeringRecommendations[, ProductionCaution := fifelse(
      SuspiciousName,
      "Review timestamp semantics; may represent post-treatment or workflow status timing",
      "Confirm timestamp exists at scoring time and is generated consistently"
    )]
  }

  # ----------------------------
  # Schema contract helpers
  # ----------------------------

  target_add_missing_col <- function(dt, name, value) {
    if (is.null(dt)) {
      dt <- data.table::data.table()
    }

    if (!data.table::is.data.table(dt)) {
      dt <- data.table::as.data.table(dt)
    }

    if (!name %in% names(dt)) {
      dt[, (name) := value]
    }

    dt
  }

  target_apply_schema <- function(dt, schema) {
    if (is.null(dt)) {
      dt <- data.table::data.table()
    }

    if (!data.table::is.data.table(dt)) {
      dt <- data.table::as.data.table(dt)
    }

    for (nm in names(schema)) {
      if (!nm %in% names(dt)) {
        dt[, (nm) := schema[[nm]]]
      }
    }

    dt
  }

  target_schema_issues <- data.table::data.table(
    Artifact = character(),
    MissingColumn = character(),
    Severity = character()
  )

  target_record_schema_issues <- function(dt, artifact, required_cols) {
    missing_cols <- setdiff(required_cols, names(dt))

    if (length(missing_cols) > 0L) {
      target_schema_issues <<- data.table::rbindlist(
        list(
          target_schema_issues,
          data.table::data.table(
            Artifact = artifact,
            MissingColumn = missing_cols,
            Severity = "Patched with default column"
          )
        ),
        fill = TRUE
      )
    }

    invisible(NULL)
  }

  # ----------------------------
  # Feature risk registry
  # ----------------------------

  TargetFeatureRiskRegistry <- data.table::data.table(
    Feature = TargetCandidateVars
  )

  if (nrow(TargetFeatureRiskRegistry) > 0L) {
    TargetFeatureRiskRegistry[, FeatureType := vapply(
      Feature,
      function(v) target_feature_role(v, data[[v]]),
      character(1L)
    )]

    TargetFeatureRiskRegistry[, SuspiciousName := vapply(Feature, target_risk_word_match, logical(1L))]

    if (nrow(TargetNumericAssociationStats) > 0L) {
      TargetFeatureRiskRegistry <- merge(
        TargetFeatureRiskRegistry,
        TargetNumericAssociationStats[
          ,
          .(
            Feature,
            NumericAbsAssociation = `Abs Correlation`,
            NumericAssociationStrength = Strength,
            NumericAssociationDirection = Direction
          )
        ],
        by = "Feature",
        all.x = TRUE
      )
    }

    if (nrow(TargetCategoricalAssociationStats) > 0L) {
      TargetFeatureRiskRegistry <- merge(
        TargetFeatureRiskRegistry,
        TargetCategoricalAssociationStats[
          ,
          .(
            Feature,
            CategoricalRiskLevel = RiskLevel,
            CategoricalMaxAbsLiftFrom1 = `Max Abs Lift From 1`,
            CategoricalExtremeLevelCount = `Extreme Level Count`,
            CategoricalLowNLevelCount = `Low N Level Count`
          )
        ],
        by = "Feature",
        all.x = TRUE
      )
    }

    if (nrow(TargetFeatureDriftStats) > 0L) {
      drift_summary <- TargetFeatureDriftStats[
        ,
        .(
          MaxAbsFeatureDrift = max(abs(c(`Percent Change`, `Share Change`, `Missing % Change`)), na.rm = TRUE)
        ),
        by = Feature
      ]
      drift_summary[!is.finite(MaxAbsFeatureDrift), MaxAbsFeatureDrift := NA_real_]

      TargetFeatureRiskRegistry <- merge(
        TargetFeatureRiskRegistry,
        drift_summary,
        by = "Feature",
        all.x = TRUE
      )
    }

    if (nrow(TargetConceptDriftStats) > 0L) {
      TargetFeatureRiskRegistry <- merge(
        TargetFeatureRiskRegistry,
        TargetConceptDriftStats[
          ,
          .(
            Feature,
            AbsConceptDrift = `Abs Correlation Change`,
            DirectionChanged = `Direction Changed`
          )
        ],
        by = "Feature",
        all.x = TRUE
      )
    }

    registry_schema <- list(
      Feature = character(),
      FeatureType = NA_character_,
      SuspiciousName = FALSE,
      NumericAbsAssociation = NA_real_,
      NumericAssociationStrength = NA_character_,
      NumericAssociationDirection = NA_character_,
      CategoricalRiskLevel = NA_character_,
      CategoricalMaxAbsLiftFrom1 = NA_real_,
      CategoricalExtremeLevelCount = NA_integer_,
      CategoricalLowNLevelCount = NA_integer_,
      MaxAbsFeatureDrift = NA_real_,
      AbsConceptDrift = NA_real_,
      DirectionChanged = FALSE
    )

    target_record_schema_issues(
      TargetFeatureRiskRegistry,
      artifact = "TargetFeatureRiskRegistry_pre_risk_scoring",
      required_cols = names(registry_schema)
    )

    TargetFeatureRiskRegistry <- target_apply_schema(
      TargetFeatureRiskRegistry,
      registry_schema
    )

    TargetFeatureRiskRegistry[, SuspiciousName := fifelse(is.na(SuspiciousName), FALSE, SuspiciousName)]
    TargetFeatureRiskRegistry[, DirectionChanged := fifelse(is.na(DirectionChanged), FALSE, DirectionChanged)]

    TargetFeatureRiskRegistry[, LeakageRiskLevel := fifelse(
      SuspiciousName | (!is.na(NumericAbsAssociation) & NumericAbsAssociation >= 0.90) | (!is.na(CategoricalRiskLevel) & CategoricalRiskLevel == "High"),
      "High",
      fifelse(
        (!is.na(NumericAbsAssociation) & NumericAbsAssociation >= 0.70) | (!is.na(CategoricalRiskLevel) & CategoricalRiskLevel == "Medium"),
        "Medium",
        fifelse(SuspiciousName, "Medium", "Low")
      )
    )]

    TargetFeatureRiskRegistry[, ColliderRiskLevel := fifelse(
      SuspiciousName & FeatureType %in% c("Categorical Feature", "Numeric Feature"),
      "Medium",
      fifelse(
        (!is.na(CategoricalExtremeLevelCount) & CategoricalExtremeLevelCount > 0L) | (!is.na(NumericAbsAssociation) & NumericAbsAssociation >= 0.95),
        "Medium",
        "Low"
      )
    )]

    TargetFeatureRiskRegistry[, DriftRiskLevel := fifelse(
      (!is.na(MaxAbsFeatureDrift) & MaxAbsFeatureDrift >= 50) | (!is.na(AbsConceptDrift) & AbsConceptDrift >= 0.50),
      "High",
      fifelse(
        (!is.na(MaxAbsFeatureDrift) & MaxAbsFeatureDrift >= 20) | (!is.na(AbsConceptDrift) & AbsConceptDrift >= 0.30),
        "Medium",
        "Low"
      )
    )]

    TargetFeatureRiskRegistry[, AvailabilityRiskLevel := fifelse(
      grepl("post|after|final|complete|completed|approved|accepted|rejected|status|decision", Feature, ignore.case = TRUE),
      "High",
      fifelse(SuspiciousName, "Medium", "Unknown")
    )]

    TargetFeatureRiskRegistry[, OverallRiskScore :=
                                target_flag_score(LeakageRiskLevel) +
                                target_flag_score(ColliderRiskLevel) +
                                target_flag_score(DriftRiskLevel) +
                                target_flag_score(AvailabilityRiskLevel)
    ]

    TargetFeatureRiskRegistry[, OverallRiskLevel := fifelse(
      OverallRiskScore >= 10L,
      "High",
      fifelse(OverallRiskScore >= 6L, "Medium", fifelse(OverallRiskScore >= 3L, "Low", "None"))
    )]

    TargetFeatureRiskRegistry[, ExplanatoryUseStatus := mapply(
      target_explanatory_status,
      primary_risk = OverallRiskLevel,
      assoc_strength = NumericAssociationStrength,
      suspicious_name = SuspiciousName,
      concept_change = AbsConceptDrift
    )]

    TargetFeatureRiskRegistry[, PredictionUseStatus := mapply(
      target_prediction_status,
      primary_risk = OverallRiskLevel,
      availability_risk = AvailabilityRiskLevel,
      drift_risk = DriftRiskLevel
    )]

    TargetFeatureRiskRegistry[, RecommendedAction := fifelse(
      OverallRiskLevel == "High",
      "Quarantine or require business review before modeling",
      fifelse(
        OverallRiskLevel == "Medium",
        "Use with review, monitoring, or sensitivity analysis",
        fifelse(OverallRiskLevel == "Low", "Generally usable with normal validation", "Generally usable")
      )
    )]

    data.table::setorderv(TargetFeatureRiskRegistry, c("OverallRiskScore", "Feature"), order = c(-1L, 1L), na.last = TRUE)
  }

  # Keep feature registry schema stable even when no candidate features exist.
  TargetFeatureRiskRegistry <- target_apply_schema(
    TargetFeatureRiskRegistry,
    list(
      Feature = character(),
      FeatureType = NA_character_,
      SuspiciousName = FALSE,
      NumericAbsAssociation = NA_real_,
      NumericAssociationStrength = NA_character_,
      NumericAssociationDirection = NA_character_,
      CategoricalRiskLevel = NA_character_,
      CategoricalMaxAbsLiftFrom1 = NA_real_,
      CategoricalExtremeLevelCount = NA_integer_,
      CategoricalLowNLevelCount = NA_integer_,
      MaxAbsFeatureDrift = NA_real_,
      AbsConceptDrift = NA_real_,
      DirectionChanged = FALSE,
      LeakageRiskLevel = NA_character_,
      ColliderRiskLevel = NA_character_,
      DriftRiskLevel = NA_character_,
      AvailabilityRiskLevel = NA_character_,
      OverallRiskScore = NA_real_,
      OverallRiskLevel = NA_character_,
      ExplanatoryUseStatus = NA_character_,
      PredictionUseStatus = NA_character_,
      RecommendedAction = NA_character_
    )
  )

  # ----------------------------
  # Target suitability + validation + monitoring recommendations
  # ----------------------------

  TargetSuitabilityAssessment <- data.table::data.table(
    AssessmentArea = character(),
    Status = character(),
    RiskLevel = character(),
    Evidence = character(),
    Recommendation = character()
  )

  if (TargetVar_Valid && TargetVar_Usable) {
    target_y <- data[[TargetVar_Name]]
    target_missing_pct <- mean(is.na(target_y)) * 100

    TargetSuitabilityAssessment <- data.table::rbindlist(list(
      TargetSuitabilityAssessment,
      data.table::data.table(
        AssessmentArea = "Target validity",
        Status = "Usable",
        RiskLevel = "Low",
        Evidence = paste0("Target type = ", TargetProblemType, "; class = ", TargetVar_Class),
        Recommendation = "Proceed with target-aware readiness diagnostics."
      ),
      data.table::data.table(
        AssessmentArea = "Target missingness",
        Status = ifelse(target_missing_pct > 0, "Missing target values present", "No missing target values"),
        RiskLevel = ifelse(target_missing_pct >= 20, "High", ifelse(target_missing_pct > 0, "Medium", "Low")),
        Evidence = paste0("Missing target % = ", target_fmt_pct(target_missing_pct, 2L)),
        Recommendation = ifelse(target_missing_pct > 0, "Exclude missing-target rows from supervised training and investigate missingness mechanism.", "No target missingness action required.")
      )
    ), fill = TRUE)

    if (TargetProblemType %in% c("Binary", "Multiclass") && nrow(TargetDistributionTable) > 0L) {
      min_class_pct <- min(TargetDistributionTable$Percent, na.rm = TRUE)
      TargetSuitabilityAssessment <- data.table::rbindlist(list(
        TargetSuitabilityAssessment,
        data.table::data.table(
          AssessmentArea = "Class balance",
          Status = ifelse(min_class_pct < 5, "Rare class / imbalance", "No severe class imbalance"),
          RiskLevel = ifelse(min_class_pct < 1, "High", ifelse(min_class_pct < 5, "Medium", "Low")),
          Evidence = paste0("Smallest class share = ", target_fmt_pct(min_class_pct, 2L)),
          Recommendation = ifelse(min_class_pct < 5, "Use PR-AUC/lift/recall-oriented metrics, stratified or time-aware validation, and consider threshold optimization.", "Standard classification metrics are acceptable, with calibration and lift still recommended.")
        )
      ), fill = TRUE)
    }

    if (TargetProblemType == "Regression" && nrow(TargetDistributionTable) > 0L) {
      y_skew <- target_skewness(target_y)
      TargetSuitabilityAssessment <- data.table::rbindlist(list(
        TargetSuitabilityAssessment,
        data.table::data.table(
          AssessmentArea = "Regression target shape",
          Status = ifelse(!is.na(y_skew) && abs(y_skew) >= 2, "Highly skewed target", "No severe target skew"),
          RiskLevel = ifelse(!is.na(y_skew) && abs(y_skew) >= 3, "High", ifelse(!is.na(y_skew) && abs(y_skew) >= 2, "Medium", "Low")),
          Evidence = paste0("Target skewness = ", ifelse(is.na(y_skew), "NA", target_fmt_num(y_skew, 3L))),
          Recommendation = ifelse(!is.na(y_skew) && abs(y_skew) >= 2, "Evaluate transformed target, robust losses, Tweedie/Gamma-like objectives, and error metrics resilient to heavy tails.", "Standard regression objectives are plausible; still inspect residuals after modeling.")
        )
      ), fill = TRUE)
    }
  } else {
    TargetSuitabilityAssessment <- data.table::data.table(
      AssessmentArea = "Target validity",
      Status = "Not usable",
      RiskLevel = "High",
      Evidence = paste0("TargetVar = ", target_safe_chr(TargetVar_Name)),
      Recommendation = "Provide a valid target variable before model readiness analysis."
    )
  }

  TargetValidationRecommendations <- data.table::data.table(
    ValidationArea = c(
      "Primary split strategy",
      "Leakage control",
      "Metric family",
      "Segment validation",
      "Temporal backtesting"
    ),
    Recommendation = c(
      ifelse(TargetTrendDateVar_Valid, "Prefer time-based split, blocked CV, or forward-chaining validation.", "Use random/stratified split unless group or temporal leakage exists."),
      "Split before target encoding, imputation learning, rare-level consolidation rules, or supervised feature engineering.",
      switch(TargetProblemType,
             Binary = "Use ROC-AUC, PR-AUC, lift/gain, calibration, threshold metrics, and confusion metrics.",
             Multiclass = "Use log loss, macro/micro F1, class-level recall, confusion matrix, and calibration where possible.",
             Regression = "Use RMSE/MAE/R-squared, residual diagnostics, calibration-by-bin, and segment-level error.",
             "Use problem-appropriate metrics once target is valid."),
      "Evaluate performance across important categorical groups and high-risk segments.",
      ifelse(TargetTrendDateVar_Valid, "Backtest across multiple time windows to estimate decay and retraining cadence.", "Not available without a valid trend date variable.")
    ),
    RiskAddressed = c(
      "Temporal leakage and deployment realism",
      "Preprocessing leakage",
      "Objective/metric mismatch",
      "Hidden subgroup failure",
      "Model decay and retraining cadence"
    )
  )

  TargetMonitoringRecommendations <- data.table::data.table(
    MonitoringArea = c(
      "Data quality",
      "Feature drift",
      "Target drift",
      "Concept drift",
      "Calibration",
      "Categorical levels",
      "Production availability"
    ),
    Recommendation = c(
      "Monitor missingness, empty values, invalid values, and schema changes for model features.",
      "Track numeric distribution shifts and categorical share changes for all high-importance or high-risk features.",
      ifelse(TargetTrendDateVar_Valid, "Track target rate/mean over time and by major segments.", "Track target distribution when labels become available."),
      "Recompute feature-target associations on recent labeled windows and compare to training-period associations.",
      "Monitor predicted probability calibration or residual calibration by score bands and segments.",
      "Track new, disappearing, and rare categorical levels; route unknown levels through stable handling rules.",
      "Monitor upstream feature freshness, latency, scoring-time availability, and data contract failures."
    ),
    SuggestedCadence = c(
      "Daily/weekly",
      ifelse(nrow(TargetFeatureRiskRegistry[DriftRiskLevel == "High"]) > 0L, "Weekly", "Monthly"),
      ifelse(TargetTrendDateVar_Valid, "Weekly/monthly", "When labels mature"),
      ifelse(nrow(TargetConceptDriftStats) > 0L, "Monthly or each label maturity cycle", "After enough labeled production data exists"),
      "Monthly or each label maturity cycle",
      "Daily/weekly",
      "Each scoring run"
    )
  )

  high_drift_n <- if (nrow(TargetFeatureRiskRegistry) > 0L) nrow(TargetFeatureRiskRegistry[DriftRiskLevel == "High"]) else 0L
  medium_drift_n <- if (nrow(TargetFeatureRiskRegistry) > 0L) nrow(TargetFeatureRiskRegistry[DriftRiskLevel == "Medium"]) else 0L
  high_risk_n <- if (nrow(TargetFeatureRiskRegistry) > 0L) nrow(TargetFeatureRiskRegistry[OverallRiskLevel == "High"]) else 0L

  TargetRetrainingCadenceRecommendation <- data.table::data.table(
    Cadence = fifelse(
      high_drift_n > 0L,
      "Frequent / event-triggered",
      fifelse(medium_drift_n > 0L, "Monthly to quarterly", "Quarterly or performance-triggered")
    ),
    Reason = fifelse(
      high_drift_n > 0L,
      paste0(high_drift_n, " high-drift features detected."),
      fifelse(medium_drift_n > 0L, paste0(medium_drift_n, " medium-drift features detected."), "No strong drift evidence from available diagnostics.")
    ),
    MinimumMonitoring = "Monitor feature drift, target drift, calibration, and subgroup performance before deciding actual retraining frequency.",
    ProductionNote = "Cadence should be adjusted based on label latency, business cycles, intervention changes, and observed performance decay."
  )

  TargetModelingStrategyRecommendations <- data.table::data.table(
    Strategy = c(
      "Prediction-first baseline",
      "Explanation-first baseline",
      "Balanced strategy",
      "High-risk feature sensitivity model",
      "Production monitoring model plan"
    ),
    RecommendedPipeline = c(
      "CatBoost with native categorical handling, minimal monotonic transformations, leakage-safe validation, calibration/lift evaluation.",
      "GLM/GAM with carefully consolidated categoricals, splines for nonlinear numeric effects, and exclusion/review of leakage/collider candidates.",
      "CatBoost for predictive benchmark plus GAM/regularized GLM explanation model for interpretable effect checks.",
      "Train with and without high-risk variables to quantify dependence on suspicious, unavailable, or unstable features.",
      "Use readiness diagnostics to choose monitored features, alert thresholds, label maturity windows, and retraining triggers."
    ),
    BestFor = c(
      "Operational prediction on tabular data",
      "Explanatory modeling and defensible variable interpretation",
      "Business analytics where both accuracy and explanation matter",
      "Leakage, collider, and production-readiness risk management",
      "Deployment and lifecycle management"
    ),
    Caveat = c(
      "Algorithm choice rarely fixes target leakage, bad validation, or unavailable features.",
      "May sacrifice predictive performance if interactions and discontinuities dominate.",
      "Requires reconciling differences between predictive and explanatory evidence.",
      "Performance drop may be acceptable if risk reduction is substantial.",
      "Monitoring is only useful if labels, feature logs, and prediction logs are retained."
    )
  )

  TargetModelReadinessSummary <- data.table::data.table(
    Area = c(
      "Target problem type",
      "Candidate features",
      "High-risk features",
      "Medium-risk features",
      "Numeric features inspected",
      "Categorical features inspected",
      "Calendar features inspected",
      "GAM diagnostics run",
      "Preferred model family",
      "Modeling objective",
      "Retraining cadence signal"
    ),
    Value = c(
      TargetProblemType,
      as.character(length(TargetCandidateVars)),
      as.character(high_risk_n),
      as.character(if (nrow(TargetFeatureRiskRegistry) > 0L) nrow(TargetFeatureRiskRegistry[OverallRiskLevel == "Medium"]) else 0L),
      as.character(length(TargetNumericFeatureVars)),
      as.character(length(TargetCategoricalFeatureVars)),
      as.character(length(TargetDateVars)),
      as.character(nrow(TargetNumericShapeDiagnostics) > 0L),
      PreferredModelFamily,
      ModelingObjective,
      TargetRetrainingCadenceRecommendation$Cadence[1L]
    )
  )

  # ----------------------------
  # Reactable widgets for readiness layers
  # ----------------------------

  Target_Suitability_Assessment <- target_readiness_reactable(TargetSuitabilityAssessment, default_page_size = 10L)
  Target_Feature_Risk_Registry <- target_readiness_reactable(TargetFeatureRiskRegistry, default_page_size = 15L)
  Target_Numeric_Feature_Diagnostics <- target_readiness_reactable(TargetNumericFeatureDiagnostics, default_page_size = 15L)
  Target_Numeric_Shape_Diagnostics <- target_readiness_reactable(TargetNumericShapeDiagnostics, default_page_size = 15L)
  Target_Numeric_Engineering_Recommendations <- target_readiness_reactable(TargetNumericEngineeringRecommendations, default_page_size = 15L)
  Target_Categorical_Feature_Diagnostics <- target_readiness_reactable(TargetCategoricalFeatureDiagnostics, default_page_size = 15L)
  Target_Categorical_Encoding_Recommendations <- target_readiness_reactable(TargetCategoricalEncodingRecommendations, default_page_size = 15L)
  Target_Calendar_Feature_Diagnostics <- target_readiness_reactable(TargetCalendarFeatureDiagnostics, default_page_size = 10L)
  Target_Calendar_Engineering_Recommendations <- target_readiness_reactable(TargetCalendarEngineeringRecommendations, default_page_size = 10L)
  Target_Validation_Recommendations <- target_readiness_reactable(TargetValidationRecommendations, default_page_size = 10L)
  Target_Monitoring_Recommendations <- target_readiness_reactable(TargetMonitoringRecommendations, default_page_size = 10L)
  Target_Modeling_Strategy_Recommendations <- target_readiness_reactable(TargetModelingStrategyRecommendations, default_page_size = 10L)
  Target_Retraining_Cadence_Recommendation <- target_readiness_reactable(TargetRetrainingCadenceRecommendation, default_page_size = 5L)
  Target_Model_Readiness_Summary <- target_readiness_reactable(TargetModelReadinessSummary, default_page_size = 15L)

  # ----------------------------
  # LLM / synthesis-ready context, no provider-specific calls
  # ----------------------------

  TargetReadinessContext <- list(
    target = list(
      name = TargetVar_Name,
      problem_type = TargetProblemType,
      target_type = TargetVar_Type,
      usable = TargetVar_Usable,
      trend_date_var = TrendDateVar,
      trend_date_valid = TargetTrendDateVar_Valid,
      valid_group_vars = TargetTrendGroupVars_Valid
    ),
    model_readiness_summary = TargetModelReadinessSummary,
    top_numeric_associations = target_top_n(TargetNumericAssociationStats, n = 10L, order_col = "Abs Correlation"),
    top_categorical_risks = target_top_n(TargetCategoricalAssociationStats, n = 10L, order_col = "Max Abs Lift From 1"),
    top_feature_risks = target_top_n(TargetFeatureRiskRegistry, n = 20L, order_col = "OverallRiskScore"),
    numeric_shape_diagnostics = target_top_n(TargetNumericShapeDiagnostics, n = 15L, order_col = "NonlinearityScore"),
    categorical_encoding_recommendations = target_top_n(TargetCategoricalEncodingRecommendations, n = 20L, order_col = "Levels"),
    validation_recommendations = TargetValidationRecommendations,
    monitoring_recommendations = TargetMonitoringRecommendations,
    modeling_strategy_recommendations = TargetModelingStrategyRecommendations,
    retraining_cadence = TargetRetrainingCadenceRecommendation
  )

  # ============================================================
  # Artifact Assembly
  # ============================================================

  target_artifacts <- list(
    report_type = "model_readiness",
    metadata = list(
      DataName = DataName,
      TargetVar = TargetVar_Name,
      TargetVarProvided = TargetVar_Provided,
      TargetVarValid = TargetVar_Valid,
      TargetVarType = TargetVar_Type,
      TargetVarClass = TargetVar_Class,
      TargetProblemType = TargetProblemType,
      TargetVarUsable = TargetVar_Usable,
      TrendDateVar = TrendDateVar,
      TrendDateVarValid = TargetTrendDateVar_Valid,
      TrendGroupVar = TrendGroupVar,
      TrendGroupVarsValid = TargetTrendGroupVars_Valid,
      TrendGroupVarsMissing = TargetTrendGroupVars_Missing,
      Rows = nrow(data),
      OutputPath = OutputPath,
      ExportPNG = ExportPNG,
      ExportHTML = ExportHTML,
      IncludeDataURL = IncludeDataURL,
      CreatedAt = as.character(Sys.time())
    ),
    settings = list(
      TargetMaxCategoricalLevels = TargetMaxCategoricalLevels,
      TargetMaxAssociationRows = TargetMaxAssociationRows,
      TargetMaxPlotRows = TargetMaxPlotRows,
      MaxCorrelationPairsToPlot = MaxCorrelationPairsToPlot,
      TargetHighLiftThreshold = TargetHighLiftThreshold,
      TargetLowLiftThreshold = TargetLowLiftThreshold,
      TargetExtremeRateThreshold = TargetExtremeRateThreshold,
      TargetMinLevelN = TargetMinLevelN,
      TargetMinTimePeriods = TargetMinTimePeriods,
      TargetDriftRecentShare = TargetDriftRecentShare,
      TargetConceptDriftMinN = TargetConceptDriftMinN,
      TargetConceptDriftMinPeriods = TargetConceptDriftMinPeriods,
      RunGAMDiagnostics = RunGAMDiagnostics,
      MaxGAMFeatures = MaxGAMFeatures,
      GAMSampleSize = GAMSampleSize,
      PreferredModelFamily = PreferredModelFamily,
      ModelingObjective = ModelingObjective,
      Theme = Theme
    ),
    tables = list(
      describe_data = DescribeData,
      target_qa = TargetQA,
      target_distribution = TargetDistributionTable,
      numeric_association = TargetNumericAssociationStats,
      categorical_association = TargetCategoricalAssociationStats,
      categorical_level_association = TargetCategoricalLevelStats,
      target_trend = TargetTrendStats,
      target_grouped_trend = TargetGroupedTrendStats,
      feature_drift = TargetFeatureDriftStats,
      concept_drift = TargetConceptDriftStats,
      risk_flags = TargetRiskFlags,
      target_suitability_assessment = TargetSuitabilityAssessment,
      feature_risk_registry = TargetFeatureRiskRegistry,
      numeric_feature_diagnostics = TargetNumericFeatureDiagnostics,
      numeric_shape_diagnostics = TargetNumericShapeDiagnostics,
      numeric_engineering_recommendations = TargetNumericEngineeringRecommendations,
      categorical_feature_diagnostics = TargetCategoricalFeatureDiagnostics,
      categorical_encoding_recommendations = TargetCategoricalEncodingRecommendations,
      calendar_feature_diagnostics = TargetCalendarFeatureDiagnostics,
      calendar_engineering_recommendations = TargetCalendarEngineeringRecommendations,
      validation_recommendations = TargetValidationRecommendations,
      monitoring_recommendations = TargetMonitoringRecommendations,
      modeling_strategy_recommendations = TargetModelingStrategyRecommendations,
      retraining_cadence_recommendation = TargetRetrainingCadenceRecommendation,
      model_readiness_summary = TargetModelReadinessSummary,
      schema_issues = target_schema_issues,
      plot_qa = TargetPlotQA
    ),
    widgets = list(
      describe_data = Describe_Data,
      target_qa = Target_QA,
      target_distribution = Target_Distribution,
      numeric_association = Target_Numeric_Association,
      categorical_association = Target_Categorical_Association,
      categorical_level_association = Target_Categorical_Level_Association,
      feature_drift = Target_Feature_Drift,
      concept_drift = Target_Concept_Drift,
      risk_flags = Target_Risk_Flags,
      target_suitability_assessment = Target_Suitability_Assessment,
      feature_risk_registry = Target_Feature_Risk_Registry,
      numeric_feature_diagnostics = Target_Numeric_Feature_Diagnostics,
      numeric_shape_diagnostics = Target_Numeric_Shape_Diagnostics,
      numeric_engineering_recommendations = Target_Numeric_Engineering_Recommendations,
      categorical_feature_diagnostics = Target_Categorical_Feature_Diagnostics,
      categorical_encoding_recommendations = Target_Categorical_Encoding_Recommendations,
      calendar_feature_diagnostics = Target_Calendar_Feature_Diagnostics,
      calendar_engineering_recommendations = Target_Calendar_Engineering_Recommendations,
      validation_recommendations = Target_Validation_Recommendations,
      monitoring_recommendations = Target_Monitoring_Recommendations,
      modeling_strategy_recommendations = Target_Modeling_Strategy_Recommendations,
      retraining_cadence_recommendation = Target_Retraining_Cadence_Recommendation,
      model_readiness_summary = Target_Model_Readiness_Summary,
      schema_issues = target_readiness_reactable(target_schema_issues, default_page_size = 10L)
    ),
    plots = list(
      target = list(
        distribution = TargetDistributionPlot,
        association = TargetAssociationPlotList,
        trend = TargetTrendPlotList,
        grouped_trend = TargetGroupedTrendPlotList,
        drift = TargetDriftPlotList,
        risk = TargetRiskPlotList,
        all = TargetPlotList
      )
    ),
    diagnostics = list(
      numeric_features = TargetNumericFeatureVars,
      categorical_features = TargetCategoricalFeatureVars,
      calendar_features = TargetDateVars,
      skipped_features = TargetSkippedFeatureVars,
      plot_qa = TargetPlotQA
    ),
    context = TargetReadinessContext
  )

  target_artifacts$exports <- list(
    image_manifest = data.table::data.table()
  )

  if (isTRUE(ExportPNG) || isTRUE(ExportHTML)) {

    # Build wrapped objects only as a temporary internal export tree.
    # The returned object keeps one canonical copy of tables/widgets/plots and
    # only stores export sidecar paths in `exports`.
    target_artifacts$artifacts <- target_build_wrapped_target_artifacts(target_artifacts)

    target_artifacts <- target_export_target_artifact_sidecars(
      artifacts = target_artifacts,
      output_path = OutputPath,
      export_png = ExportPNG,
      export_html = ExportHTML,
      include_data_url = IncludeDataURL,
      png_width = PNGWidth,
      png_height = PNGHeight,
      png_dpi = PNGDPI,
      png_background = PNGBackground
    )

    export_manifest_list <- target_artifacts$export_manifest

    target_artifacts$exports$image_manifest <- if (length(export_manifest_list)) {
      data.table::rbindlist(
        lapply(export_manifest_list, data.table::as.data.table),
        fill = TRUE
      )
    } else {
      data.table::data.table()
    }

    target_artifacts$artifacts <- NULL
    target_artifacts$export_manifest <- NULL
  }

  class(target_artifacts) <- c("target_analysis_artifacts", class(target_artifacts))

  target_artifacts
}


# ============================================================
# Target Artifact Sidecar Helpers
# ============================================================

target_clean_artifact_name <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)

  if (!nzchar(x)) {
    x <- "artifact"
  }

  x
}


target_wrap_artifact <- function(object, artifact_id, section, artifact_type, title = NULL, metadata = list()) {

  list(
    object = object,
    png = NULL,
    html = NULL,
    data_url = NULL,
    metadata = c(
      list(
        artifact_id = artifact_id,
        section = section,
        artifact_type = artifact_type,
        title = title,
        object_class = paste(class(object), collapse = ", ")
      ),
      metadata
    )
  )
}


target_build_wrapped_target_artifacts <- function(artifacts) {

  out <- list(
    widgets = list(),
    plots = list()
  )

  widget_names <- names(artifacts$widgets)

  for (nm in widget_names) {
    obj <- artifacts$widgets[[nm]]

    if (!is.null(obj)) {
      out$widgets[[nm]] <- target_wrap_artifact(
        object = obj,
        artifact_id = paste0("target_widget_", nm),
        section = "Target Analysis",
        artifact_type = "widget",
        title = nm
      )
    }
  }

  out$plots$target <- list()

  target_plot_groups <- artifacts$plots$target

  for (group_name in names(target_plot_groups)) {

    group_obj <- target_plot_groups[[group_name]]

    if (is.null(group_obj)) {
      next
    }

    if (inherits(group_obj, "htmlwidget") || inherits(group_obj, "ggplot")) {

      out$plots$target[[group_name]] <- target_wrap_artifact(
        object = group_obj,
        artifact_id = paste0("target_plot_", group_name),
        section = "Target Analysis",
        artifact_type = "plot",
        title = group_name
      )

    } else if (is.list(group_obj)) {

      out$plots$target[[group_name]] <- list()

      for (plot_name in names(group_obj)) {

        plot_obj <- group_obj[[plot_name]]

        if (is.null(plot_obj)) {
          next
        }

        out$plots$target[[group_name]][[plot_name]] <- target_wrap_artifact(
          object = plot_obj,
          artifact_id = paste0("target_plot_", group_name, "_", target_clean_artifact_name(plot_name)),
          section = "Target Analysis",
          artifact_type = "plot",
          title = plot_name
        )
      }
    }
  }

  out
}


target_export_target_artifact_sidecars <- function(
    artifacts,
    output_path = NULL,
    export_png = FALSE,
    export_html = FALSE,
    include_data_url = FALSE,
    png_width = 1400,
    png_height = 900,
    png_dpi = 150,
    png_background = "white"
) {

  if (is.null(output_path)) {
    output_path <- file.path(getwd(), "target_analysis_artifacts")
  }

  image_dir <- file.path(output_path, "images")
  html_dir <- file.path(output_path, "html")

  if (isTRUE(export_png) && !dir.exists(image_dir)) {
    dir.create(image_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (isTRUE(export_html) && !dir.exists(html_dir)) {
    dir.create(html_dir, recursive = TRUE, showWarnings = FALSE)
  }

  export_one <- function(x) {

    if (!is.list(x) || is.null(x$object) || is.null(x$metadata$artifact_id)) {
      return(x)
    }

    file_stem <- target_clean_artifact_name(x$metadata$artifact_id)

    if (isTRUE(export_png)) {
      png_file <- file.path(image_dir, paste0(file_stem, ".png"))

      png_result <- tryCatch(
        ObjectToPNG(
          object = x$object,
          file = png_file,
          width = png_width,
          height = png_height,
          dpi = png_dpi,
          background = png_background,
          overwrite = TRUE
        ),
        error = function(e) NULL
      )

      if (!is.null(png_result)) {
        x$png <- png_result

        if (isTRUE(include_data_url)) {
          x$data_url <- tryCatch(
            ObjectFileToDataURL(png_result, mime_type = "image/png"),
            error = function(e) NULL
          )
        }
      }
    }

    if (isTRUE(export_html)) {
      html_file <- file.path(html_dir, paste0(file_stem, ".html"))

      html_result <- tryCatch(
        ObjectToHTML(
          object = x$object,
          file = html_file,
          width = png_width,
          height = png_height,
          dpi = png_dpi,
          background = png_background,
          overwrite = TRUE
        ),
        error = function(e) NULL
      )

      if (!is.null(html_result)) {
        x$html <- html_result
      }
    }

    x
  }

  recurse <- function(x) {
    if (is.list(x) && !is.null(x$object) && !is.null(x$metadata$artifact_id)) {
      return(export_one(x))
    }

    if (is.list(x)) {
      return(lapply(x, recurse))
    }

    x
  }

  artifacts$artifacts <- recurse(artifacts$artifacts)

  image_manifest <- list()

  collect_manifest <- function(x) {
    if (is.list(x) && !is.null(x$metadata$artifact_id)) {
      image_manifest[[length(image_manifest) + 1L]] <<- list(
        artifact_id = x$metadata$artifact_id,
        section = x$metadata$section,
        artifact_type = x$metadata$artifact_type,
        title = x$metadata$title,
        png = x$png,
        html = x$html,
        has_data_url = !is.null(x$data_url)
      )
      return(invisible(NULL))
    }

    if (is.list(x)) {
      invisible(lapply(x, collect_manifest))
    }

    invisible(NULL)
  }

  collect_manifest(artifacts$artifacts)

  artifacts$export_manifest <- image_manifest
  artifacts
}
