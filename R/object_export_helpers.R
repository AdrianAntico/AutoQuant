# ============================================================
# Object Export Helpers
# ============================================================
# These helpers convert common R visualization objects into
# portable artifacts for reports, RMarkdown, and LLM vision input.
#
# Supported object types:
#   - ggplot2 objects
#   - htmlwidgets, including echarts4r and plotly
#   - recordedplot objects from base graphics
#   - functions that draw base graphics when called
#
# Suggested package dependencies:
#   Imports:
#     ggplot2
#     htmlwidgets
#     webshot2
#     base64enc
#     htmltools
#
# Optional:
#     ragg
# ============================================================


make_artifact_rmd_chunk <- function(key, object_path = "artifacts$widgets") {
  title <- gsub("_", " ", key)
  title <- tools::toTitleCase(title)

  chunk_name <- gsub("[^A-Za-z0-9_]", "_", title)

  paste0(
    '# <font size="5">', title, '</font>\n\n',
    '<details><summary>Expand</summary>\n',
    '<p>\n\n',
    '```{r ', chunk_name, ', echo=FALSE}\n',
    'if (!is.null(', object_path, '$', key, ')) {\n',
    '  ', object_path, '$', key, '\n',
    '} else {\n',
    '  cat("', title, ' was not created.")\n',
    '}\n',
    '```\n\n',
    '</details>\n',
    '</p>\n'
  )
}

#' Check Whether an Object Is an HTML Widget
#'
#' Internal helper used by the object export functions.
#'
#' @param object Any R object.
#'
#' @return Logical scalar.
#'
#' @keywords internal
is_htmlwidget_object <- function(object) {
  inherits(object, "htmlwidget")
}


#' Check Whether an Object Is a ggplot Object
#'
#' Internal helper used by the object export functions.
#'
#' @param object Any R object.
#'
#' @return Logical scalar.
#'
#' @keywords internal
is_ggplot_object <- function(object) {
  inherits(object, "ggplot")
}


#' Check Whether an Object Is a Recorded Base Plot
#'
#' Internal helper used by the object export functions.
#'
#' @param object Any R object.
#'
#' @return Logical scalar.
#'
#' @keywords internal
is_recordedplot_object <- function(object) {
  inherits(object, "recordedplot")
}


#' Normalize a File Path for Output
#'
#' Internal helper that expands and normalizes an output path after
#' ensuring the parent directory exists.
#'
#' @param file Character scalar. Output file path.
#' @param overwrite Logical. If `FALSE`, an existing file triggers an error.
#'
#' @return Character scalar containing a normalized path.
#'
#' @keywords internal
normalize_output_file <- function(file, overwrite = TRUE) {

  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("`file` must be a non-empty character scalar.", call. = FALSE)
  }

  file <- path.expand(file)

  dir_path <- dirname(file)

  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  if (file.exists(file) && !isTRUE(overwrite)) {
    stop("File already exists and `overwrite = FALSE`: ", file, call. = FALSE)
  }

  normalizePath(file, winslash = "/", mustWork = FALSE)
}


#' Create a Temporary HTML File
#'
#' Internal helper for rendering HTML widgets.
#'
#' @return Character scalar path to a temporary HTML file.
#'
#' @keywords internal
temp_html_file <- function() {
  tempfile(pattern = "object_export_", fileext = ".html")
}


#' Save an Object as Standalone HTML
#'
#' Saves a supported object as an HTML file. This is primarily useful for
#' HTML widgets such as \pkg{echarts4r}, \pkg{plotly}, and other objects
#' built on \pkg{htmlwidgets}.
#'
#' For \pkg{ggplot2} and base graphics objects, this helper currently writes
#' a PNG image and wraps it in a minimal HTML document.
#'
#' @param object Object to save.
#' @param file Character scalar. Output HTML path.
#' @param width Numeric. Width in pixels for rendered images.
#' @param height Numeric. Height in pixels for rendered images.
#' @param dpi Numeric. Resolution for rasterized non-widget objects.
#' @param background Character scalar. Background color for rasterized plots.
#' @param selfcontained Logical. Passed to [htmlwidgets::saveWidget()] for
#'   HTML widgets.
#' @param overwrite Logical. If `FALSE`, an existing file triggers an error.
#' @param ... Additional arguments passed to lower-level methods.
#'
#' @return Invisibly returns the normalized HTML file path.
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#'
#' ObjectToHTML(p, "plot.html")
#' }
#'
#' @export
ObjectToHTML <- function(
    object,
    file,
    width = 1400,
    height = 900,
    dpi = 150,
    background = "white",
    selfcontained = TRUE,
    overwrite = TRUE,
    ...
) {

  file <- normalize_output_file(file, overwrite = overwrite)

  if (is_htmlwidget_object(object)) {

    htmlwidgets::saveWidget(
      widget = object,
      file = file,
      selfcontained = selfcontained,
      ...
    )

    return(invisible(file))
  }

  png_file <- tempfile(pattern = "object_export_", fileext = ".png")

  ObjectToPNG(
    object = object,
    file = png_file,
    width = width,
    height = height,
    dpi = dpi,
    background = background,
    overwrite = TRUE,
    ...
  )

  img_data <- ObjectFileToDataURL(png_file, mime_type = "image/png")

  html <- paste0(
    "<!doctype html>\n",
    "<html>\n",
    "<head>\n",
    "<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<style>\n",
    "body{margin:0;background:", background, ";}\n",
    "img{display:block;max-width:100%;height:auto;}\n",
    "</style>\n",
    "</head>\n",
    "<body>\n",
    "<img src=\"", img_data, "\" />\n",
    "</body>\n",
    "</html>\n"
  )

  writeLines(html, con = file, useBytes = TRUE)

  unlink(png_file)

  invisible(file)
}


#' Save an Object as a PNG Image
#'
#' Converts a supported visualization object to a PNG file.
#'
#' This helper is designed for artifact-first reporting workflows where plots
#' are generated once and then reused in RMarkdown, HTML reports, PDFs, or
#' LLM vision prompts.
#'
#' Supported object types:
#'
#' \itemize{
#'   \item \pkg{ggplot2} objects.
#'   \item HTML widgets, including \pkg{echarts4r}, \pkg{plotly}, and other
#'     \pkg{htmlwidgets}.
#'   \item `recordedplot` objects from base R graphics.
#'   \item Functions that draw a plot when called.
#' }
#'
#' HTML widgets are saved to a temporary HTML file and rendered to PNG using
#' \pkg{webshot2}. Base graphics are drawn into a PNG device.
#'
#' @param object Plot object or plot-drawing function.
#' @param file Character scalar. Output PNG path.
#' @param width Numeric. Output width in pixels.
#' @param height Numeric. Output height in pixels.
#' @param dpi Numeric. Output resolution.
#' @param background Character scalar. Background color.
#' @param overwrite Logical. If `FALSE`, an existing file triggers an error.
#' @param selector Character scalar or `NULL`. Optional CSS selector passed to
#'   [webshot2::webshot()] for HTML widgets.
#' @param delay Numeric. Delay in seconds before taking a webshot. Useful for
#'   widgets that need a moment to render.
#' @param zoom Numeric. Zoom factor passed to [webshot2::webshot()].
#' @param selfcontained Logical. Passed to [htmlwidgets::saveWidget()] for
#'   HTML widgets. Defaults to `FALSE` so widget screenshot staging does not
#'   require pandoc.
#' @param libdir Character scalar or `NULL`. Dependency directory passed to
#'   [htmlwidgets::saveWidget()] when `selfcontained = FALSE`.
#' @param use_ragg Logical. If `TRUE` and \pkg{ragg} is installed, uses
#'   [ragg::agg_png()] for base graphics and recorded plots.
#' @param ... Additional arguments passed to lower-level methods.
#'
#' @return Invisibly returns the normalized PNG file path.
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#'
#' ObjectToPNG(
#'   object = p,
#'   file = "scatter.png",
#'   width = 1400,
#'   height = 900
#' )
#' }
#'
#' @export
ObjectToPNG <- function(
    object,
    file,
    width = 1400,
    height = 900,
    dpi = 150,
    background = "white",
    overwrite = TRUE,
    selector = NULL,
    delay = 0.2,
    zoom = 1,
    selfcontained = FALSE,
    libdir = NULL,
    use_ragg = TRUE,
    ...
) {

  file <- normalize_output_file(file, overwrite = overwrite)

  if (is_ggplot_object(object)) {

    ggplot2::ggsave(
      filename = file,
      plot = object,
      width = width / dpi,
      height = height / dpi,
      dpi = dpi,
      bg = background,
      units = "in",
      device = "png",
      ...
    )

    return(invisible(file))
  }

  if (is_htmlwidget_object(object)) {

    html_file <- temp_html_file()
    if (is.null(libdir) || !nzchar(libdir)) {
      libdir <- paste0(tools::file_path_sans_ext(html_file), "_files")
    }

    on.exit({
      if (file.exists(html_file)) {
        unlink(html_file, force = TRUE)
      }
      if (isFALSE(selfcontained) && dir.exists(libdir)) {
        unlink(libdir, recursive = TRUE, force = TRUE)
      }
    }, add = TRUE)

    htmlwidgets::saveWidget(
      widget = object,
      file = html_file,
      selfcontained = selfcontained,
      libdir = libdir
    )

    webshot2::webshot(
      url = html_file,
      file = file,
      vwidth = width,
      vheight = height,
      selector = selector,
      delay = delay,
      zoom = zoom,
      ...
    )

    result <- file
    attr(result, "html_path") <- normalizePath(html_file, winslash = "/", mustWork = FALSE)
    attr(result, "libdir") <- normalizePath(libdir, winslash = "/", mustWork = FALSE)
    attr(result, "selfcontained") <- isTRUE(selfcontained)
    return(invisible(result))
  }

  if (is_recordedplot_object(object)) {

    open_png_device(
      file = file,
      width = width,
      height = height,
      dpi = dpi,
      background = background,
      use_ragg = use_ragg
    )

    on.exit(grDevices::dev.off(), add = TRUE)

    grDevices::replayPlot(object)

    return(invisible(file))
  }

  if (is.function(object)) {

    open_png_device(
      file = file,
      width = width,
      height = height,
      dpi = dpi,
      background = background,
      use_ragg = use_ragg
    )

    on.exit(grDevices::dev.off(), add = TRUE)

    object()

    return(invisible(file))
  }

  stop(
    "Unsupported object class: ",
    paste(class(object), collapse = ", "),
    call. = FALSE
  )
}


#' Open a PNG Graphics Device
#'
#' Internal helper used by [ObjectToPNG()] for base graphics workflows.
#'
#' @param file Character scalar. Output PNG path.
#' @param width Numeric. Width in pixels.
#' @param height Numeric. Height in pixels.
#' @param dpi Numeric. Resolution.
#' @param background Character scalar. Background color.
#' @param use_ragg Logical. If `TRUE` and \pkg{ragg} is installed, uses
#'   [ragg::agg_png()].
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
open_png_device <- function(
    file,
    width = 1400,
    height = 900,
    dpi = 150,
    background = "white",
    use_ragg = TRUE
) {

  if (isTRUE(use_ragg) && requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(
      filename = file,
      width = width,
      height = height,
      units = "px",
      res = dpi,
      background = background
    )
  } else {
    grDevices::png(
      filename = file,
      width = width,
      height = height,
      units = "px",
      res = dpi,
      bg = background
    )
  }

  invisible(NULL)
}


#' Convert a File to Base64
#'
#' Reads a file from disk and returns a Base64-encoded character string.
#'
#' @param file Character scalar. Path to the file.
#'
#' @return Character scalar containing the Base64-encoded file.
#'
#' @examples
#' \dontrun{
#' ObjectFileToBase64("plot.png")
#' }
#'
#' @export
ObjectFileToBase64 <- function(file) {

  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("`file` must be a non-empty character scalar.", call. = FALSE)
  }

  if (!file.exists(file)) {
    stop("File does not exist: ", file, call. = FALSE)
  }

  base64enc::base64encode(file)
}


#' Convert a File to a Data URL
#'
#' Converts a local file to a data URL, suitable for API image inputs.
#'
#' @param file Character scalar. Path to the file.
#' @param mime_type Character scalar. MIME type. Defaults to `"image/png"`.
#'
#' @return Character scalar containing a data URL.
#'
#' @examples
#' \dontrun{
#' ObjectFileToDataURL("plot.png")
#' }
#'
#' @export
ObjectFileToDataURL <- function(file, mime_type = "image/png") {

  b64 <- ObjectFileToBase64(file)

  paste0("data:", mime_type, ";base64,", b64)
}


#' Convert an Object Directly to a Base64 PNG
#'
#' Saves a supported object to a temporary PNG and returns the Base64 string.
#'
#' @param object Plot object or plot-drawing function.
#' @param width Numeric. Output width in pixels.
#' @param height Numeric. Output height in pixels.
#' @param dpi Numeric. Output resolution.
#' @param background Character scalar. Background color.
#' @param ... Additional arguments passed to [ObjectToPNG()].
#'
#' @return Character scalar containing the Base64-encoded PNG.
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#'
#' b64 <- ObjectToBase64PNG(p)
#' }
#'
#' @export
ObjectToBase64PNG <- function(
    object,
    width = 1400,
    height = 900,
    dpi = 150,
    background = "white",
    ...
) {

  file <- tempfile(pattern = "object_export_", fileext = ".png")

  on.exit({
    if (file.exists(file)) {
      unlink(file, force = TRUE)
    }
  }, add = TRUE)

  ObjectToPNG(
    object = object,
    file = file,
    width = width,
    height = height,
    dpi = dpi,
    background = background,
    overwrite = TRUE,
    ...
  )

  ObjectFileToBase64(file)
}


#' Convert an Object Directly to a PNG Data URL
#'
#' Saves a supported object to a temporary PNG and returns a data URL.
#'
#' This is useful for OpenAI API image inputs where the image is supplied as
#' an inline Base64 data URL.
#'
#' @param object Plot object or plot-drawing function.
#' @param width Numeric. Output width in pixels.
#' @param height Numeric. Output height in pixels.
#' @param dpi Numeric. Output resolution.
#' @param background Character scalar. Background color.
#' @param ... Additional arguments passed to [ObjectToPNG()].
#'
#' @return Character scalar containing a PNG data URL.
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#'
#' image_url <- ObjectToDataURL(p)
#' }
#'
#' @export
ObjectToDataURL <- function(
    object,
    width = 1400,
    height = 900,
    dpi = 150,
    background = "white",
    ...
) {

  b64 <- ObjectToBase64PNG(
    object = object,
    width = width,
    height = height,
    dpi = dpi,
    background = background,
    ...
  )

  paste0("data:image/png;base64,", b64)
}


#' Create a Markdown Image Reference
#'
#' Creates a Markdown image tag for a local image path.
#'
#' @param file Character scalar. Path to an image.
#' @param alt Character scalar. Alt text.
#'
#' @return Character scalar containing a Markdown image tag.
#'
#' @examples
#' \dontrun{
#' ObjectFileToMarkdown("plot.png", alt = "Scatter plot")
#' }
#'
#' @export
ObjectFileToMarkdown <- function(file, alt = "Plot image") {

  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("`file` must be a non-empty character scalar.", call. = FALSE)
  }

  if (!is.character(alt) || length(alt) != 1L || is.na(alt)) {
    stop("`alt` must be a character scalar.", call. = FALSE)
  }

  paste0("![", alt, "](", file, ")")
}


#' Save an Object and Create a Markdown Image Reference
#'
#' Saves a supported object as PNG and returns a Markdown image tag.
#'
#' @param object Plot object or plot-drawing function.
#' @param file Character scalar. Output PNG path.
#' @param alt Character scalar. Alt text.
#' @param ... Additional arguments passed to [ObjectToPNG()].
#'
#' @return Character scalar containing a Markdown image tag.
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#'
#' ObjectToMarkdown(p, "plot.png", alt = "Scatter plot")
#' }
#'
#' @export
ObjectToMarkdown <- function(object, file, alt = "Plot image", ...) {

  png_file <- ObjectToPNG(
    object = object,
    file = file,
    ...
  )

  ObjectFileToMarkdown(png_file, alt = alt)
}


#' Create an LLM-Ready Image Artifact
#'
#' Saves a supported plot object to PNG and returns a compact metadata object
#' that can be used by reporting pipelines or LLM API calls.
#'
#' The returned list includes the original object optionally, the PNG path,
#' a Markdown image reference, optional data URL, and user-provided metadata.
#'
#' @param object Plot object or plot-drawing function.
#' @param file Character scalar. Output PNG path.
#' @param artifact_id Character scalar or `NULL`. Optional artifact identifier.
#' @param title Character scalar or `NULL`. Optional title.
#' @param description Character scalar or `NULL`. Optional description.
#' @param metadata Named list. Additional compact metadata or statistics.
#' @param include_object Logical. If `TRUE`, includes the original object in the
#'   returned artifact.
#' @param include_data_url Logical. If `TRUE`, includes a Base64 data URL. This
#'   can make the returned object large.
#' @param alt Character scalar. Alt text for the Markdown image reference.
#' @param ... Additional arguments passed to [ObjectToPNG()].
#'
#' @return A named list with class `"llm_image_artifact"`.
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#'
#' artifact <- ObjectToLLMArtifact(
#'   object = p,
#'   file = "scatter.png",
#'   artifact_id = "mtcars_scatter",
#'   title = "Weight vs MPG",
#'   metadata = list(
#'     rows = nrow(mtcars),
#'     x = "wt",
#'     y = "mpg"
#'   )
#' )
#' }
#'
#' @export
ObjectToLLMArtifact <- function(
    object,
    file,
    artifact_id = NULL,
    title = NULL,
    description = NULL,
    metadata = list(),
    include_object = FALSE,
    include_data_url = FALSE,
    alt = "Plot image",
    ...
) {

  png_file <- ObjectToPNG(
    object = object,
    file = file,
    ...
  )

  out <- list(
    artifact_id = artifact_id,
    title = title,
    description = description,
    type = "image/png",
    file = png_file,
    markdown = ObjectFileToMarkdown(png_file, alt = alt),
    metadata = metadata
  )

  if (isTRUE(include_object)) {
    out$object <- object
  }

  if (isTRUE(include_data_url)) {
    out$data_url <- ObjectFileToDataURL(png_file, mime_type = "image/png")
  }

  class(out) <- c("llm_image_artifact", class(out))

  out
}


#' Print an LLM Image Artifact
#'
#' @param x An object created by [ObjectToLLMArtifact()].
#' @param ... Unused.
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.llm_image_artifact <- function(x, ...) {

  cat("<llm_image_artifact>\n")

  if (!is.null(x$artifact_id)) {
    cat("  id: ", x$artifact_id, "\n", sep = "")
  }

  if (!is.null(x$title)) {
    cat("  title: ", x$title, "\n", sep = "")
  }

  if (!is.null(x$file)) {
    cat("  file: ", x$file, "\n", sep = "")
  }

  if (!is.null(x$description)) {
    cat("  description: ", x$description, "\n", sep = "")
  }

  if (length(x$metadata)) {
    cat("  metadata names: ", paste(names(x$metadata), collapse = ", "), "\n", sep = "")
  }

  invisible(x)
}


#' Build an OpenAI Responses API Image Content Item
#'
#' Creates the image content item shape commonly used when sending a local
#' PNG image as an inline data URL to the OpenAI Responses API.
#'
#' This helper only builds the R list. It does not call the API.
#'
#' @param file Character scalar. Path to a PNG file.
#' @param detail Character scalar. Image detail level, usually `"auto"`,
#'   `"low"`, or `"high"`.
#'
#' @return A named list suitable for JSON serialization.
#'
#' @examples
#' \dontrun{
#' image_item <- ObjectFileToOpenAIImageInput("plot.png")
#' }
#'
#' @export
ObjectFileToOpenAIImageInput <- function(file, detail = "auto") {

  list(
    type = "input_image",
    image_url = ObjectFileToDataURL(file, mime_type = "image/png"),
    detail = detail
  )
}


#' Build an OpenAI Responses API Text Content Item
#'
#' Creates the text content item shape commonly used by the OpenAI Responses
#' API.
#'
#' This helper only builds the R list. It does not call the API.
#'
#' @param text Character scalar. Prompt text.
#'
#' @return A named list suitable for JSON serialization.
#'
#' @examples
#' \dontrun{
#' text_item <- ObjectTextToOpenAIInput("Analyze this plot.")
#' }
#'
#' @export
ObjectTextToOpenAIInput <- function(text) {

  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    stop("`text` must be a character scalar.", call. = FALSE)
  }

  list(
    type = "input_text",
    text = text
  )
}


#' Build an OpenAI Responses API Message for a Plot Image
#'
#' Creates a user message containing text and a local image encoded as a data
#' URL. This is useful when you want the LLM to comment on a rendered plot.
#'
#' This helper only builds the R list. It does not call the API.
#'
#' @param file Character scalar. Path to a PNG file.
#' @param prompt Character scalar. Prompt text.
#' @param detail Character scalar. Image detail level, usually `"auto"`,
#'   `"low"`, or `"high"`.
#'
#' @return A named list suitable for JSON serialization.
#'
#' @examples
#' \dontrun{
#' msg <- ObjectFileToOpenAIPlotMessage(
#'   file = "plot.png",
#'   prompt = "Analyze this plot like a senior data scientist."
#' )
#' }
#'
#' @export
ObjectFileToOpenAIPlotMessage <- function(
    file,
    prompt = "Analyze this plot like a senior data scientist.",
    detail = "auto"
) {

  list(
    role = "user",
    content = list(
      ObjectTextToOpenAIInput(prompt),
      ObjectFileToOpenAIImageInput(file, detail = detail)
    )
  )
}


#' Save an Object and Build an OpenAI Plot Message
#'
#' Saves a supported object to PNG and creates a Responses API-compatible
#' message containing the plot image and prompt text.
#'
#' This helper only builds the R list. It does not call the API.
#'
#' @param object Plot object or plot-drawing function.
#' @param file Character scalar. Output PNG path.
#' @param prompt Character scalar. Prompt text.
#' @param detail Character scalar. Image detail level, usually `"auto"`,
#'   `"low"`, or `"high"`.
#' @param ... Additional arguments passed to [ObjectToPNG()].
#'
#' @return A named list suitable for JSON serialization.
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#'
#' msg <- ObjectToOpenAIPlotMessage(
#'   object = p,
#'   file = "scatter.png"
#' )
#' }
#'
#' @export
ObjectToOpenAIPlotMessage <- function(
    object,
    file,
    prompt = "Analyze this plot like a senior data scientist.",
    detail = "auto",
    ...
) {

  png_file <- ObjectToPNG(
    object = object,
    file = file,
    ...
  )

  ObjectFileToOpenAIPlotMessage(
    file = png_file,
    prompt = prompt,
    detail = detail
  )
}
