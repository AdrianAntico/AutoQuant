aq_reactable_exclusion_filter <- function() {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    return(NULL)
  }

  htmlwidgets::JS("
    function(rows, columnId, filterValue) {
      if (!filterValue) return rows;

      const filter = String(filterValue).toLowerCase().trim();
      const exclude = filter.startsWith('!') || filter.startsWith('-');
      const term = exclude ? filter.slice(1).trim() : filter;

      if (!term) return rows;

      return rows.filter(function(row) {
        const rawValue = row.values[columnId];
        const value = rawValue == null ? '' : String(rawValue).toLowerCase();
        const match = value.includes(term);
        return exclude ? !match : match;
      });
    }
  ")
}

aq_reactable_is_text_like <- function(x) {
  is.character(x) || is.factor(x) || is.logical(x)
}

aq_reactable_text_filter_columns <- function(data, columns = NULL) {
  if (!requireNamespace("reactable", quietly = TRUE)) {
    return(columns)
  }

  filter_method <- aq_reactable_exclusion_filter()
  if (is.null(filter_method)) {
    return(columns)
  }

  if (is.null(columns)) {
    columns <- list()
  }

  text_cols <- names(data)[vapply(data, aq_reactable_is_text_like, logical(1L))]
  for (column in text_cols) {
    column_def <- columns[[column]]
    if (is.null(column_def)) {
      column_def <- reactable::colDef()
    }

    if (is.null(column_def$filterMethod) && !identical(column_def$filterable, FALSE)) {
      column_def$filterMethod <- filter_method
    }

    columns[[column]] <- column_def
  }

  columns
}

qa_reactable_exclusion_filter_helpers <- function() {
  sample_data <- data.table::data.table(
    metric = c("Impressions", "Clicks", "Revenue", "Total Impressions"),
    category = factor(c("Media", "Media", "Sales", "Media")),
    active = c(TRUE, TRUE, FALSE, TRUE),
    value = c(100L, 20L, 300L, 140L),
    date = as.Date("2026-01-01") + 0:3
  )

  columns <- aq_reactable_text_filter_columns(sample_data)
  has_filter <- function(column) {
    !is.null(columns[[column]]) && !is.null(columns[[column]]$filterMethod)
  }
  filter_js <- aq_reactable_exclusion_filter()
  filter_source <- paste(as.character(filter_js), collapse = "\n")

  data.table::data.table(
    check = c(
      "filter_helper_available",
      "normal_filter_semantics_documented",
      "bang_exclusion_semantics_documented",
      "dash_exclusion_semantics_documented",
      "text_column_filter",
      "factor_column_filter",
      "logical_column_filter",
      "numeric_column_unmodified",
      "date_column_unmodified"
    ),
    status = c(
      if (!is.null(filter_js)) "success" else "warning",
      if (nzchar(filter_source) && grepl("includes\\(term\\)", filter_source, fixed = FALSE)) "success" else "error",
      if (nzchar(filter_source) && grepl("startsWith('!')", filter_source, fixed = TRUE)) "success" else "error",
      if (nzchar(filter_source) && grepl("startsWith('-')", filter_source, fixed = TRUE)) "success" else "error",
      if (has_filter("metric")) "success" else "error",
      if (has_filter("category")) "success" else "error",
      if (has_filter("active")) "success" else "error",
      if (!has_filter("value")) "success" else "error",
      if (!has_filter("date")) "success" else "error"
    ),
    message = c(
      "The shared reactable exclusion JS filter can be created when htmlwidgets is available.",
      "Plain text filters keep matching rows.",
      "`!term` excludes matching rows.",
      "`-term` excludes matching rows.",
      "Character columns receive the exclusion filter.",
      "Factor columns receive the exclusion filter.",
      "Logical columns receive the exclusion filter.",
      "Numeric columns are left to reactable defaults.",
      "Date columns are left to reactable defaults."
    )
  )
}
