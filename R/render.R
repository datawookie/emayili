manifest <- function(msg,
                     markdown,
                     params = NULL,
                     squish = TRUE,
                     css,
                     include_css,
                     language) {
  stopifnot(is.null(params) || is.list(params))

  if (plain <- attr(markdown, "plain")) {
    output <- markdown_html(markdown) %>% read_html()
  } else {
    # Create temporary Rmd file.
    #
    # This needs to be in the current directory in case the Rmd is accessing
    # files (like CSV) using a relative path.
    #
    input <- tempfile(fileext = ".Rmd", tmpdir = getwd())
    output <- sub("\\.Rmd", ".html", input)

    # Clean up rendered artefacts.
    #
    on.exit(unlink(sub("\\.Rmd", "*", input), recursive = TRUE))

    # Write input to file.
    stri_write_lines(markdown, input)

    output_format <- html_document(
      self_contained = TRUE,
      # Silence pandoc warnings (mostly due to missing document title).
      pandoc_args = "--quiet"
    )

    output_format$knitr$knit_hooks <- list(
      plot = function(x, options) {
        caption <- options$fig.cap
        alt <- options$fig.alt
        if (is.null(alt)) alt <- caption
        class <- options$fig.class
        width <- options$out.width

        as.character(
          tags$figure(
            tags$img(
              src = x,
              alt = alt,
              width = width
            ),
            if (is.null(caption)) NULL else tags$figcaption(caption),
            class = class
          )
        )
      }
    )

    # Render from file to file.
    rmarkdown::render(
      input,
      output_format = output_format,
      output_file = output,
      params = params,
      quiet = TRUE,
      # Pass a clean environment. This makes it possible to use params name in
      # this scope.
      envir = new.env()
    )

    # Read output from file.
    output <- read_html(output)
  }

  # Decide what sources of CSS to retain.
  #
  # * Inline CSS in <link> tags.
  if (any("rmd" == include_css)) {
    log_debug("- Retain CSS from inline <link> tags.")
  } else {
    log_debug("- Remove CSS from inline <link> tags.")
    xml_remove(xml_find_all(output, "//link[starts-with(@href,'data:text/css')]"))
  }
  #
  # * External CSS in <link> tags.
  # - Doesn't apply to Plain Markdown.
  log_debug("- Remove CSS from external <link> tags.")
  xml_remove(xml_find_all(output, "//link[not(starts-with(@href,'data:text/css'))]"))
  #
  # * Inline CSS in <style> tags.
  if (any("rmd" == include_css)) {
    log_debug("- Retain inline <style>.")
  } else {
    log_debug("- Remove inline <style>.")
    xml_remove(xml_find_all(output, "//style"))
  }

  # Drop NULL items.
  css <- discard(css, is.null)

  if (length(css)) {
    css_file <- tempfile(fileext = ".css")
    cat(paste(css, collapse = "\n"), file = css_file)
  } else {
    css_file <- NULL
  }

  attach_images(msg, output, disposition = "inline", charset = "utf-8", encoding = NA, css_file, language)
}

# If {memoise} is installed then memoise manifest().
#
if (requireNamespace("memoise", quietly = TRUE)) {
  manifest <- memoise::memoise(manifest)
}

#' Render Markdown into email
#'
#' @description
#'
#' Render either Plain Markdown or R Markdown directly into the body of an email.
#'
#' If \code{input} is a file then it will be interpreted as R Markdown it its
#' extension is either \code{"Rmd"} or \code{"Rmarkdown"}. Otherwise it will be
#' processed as Plain Markdown.
#'
#' @section Plain Markdown:
#'
#' Plain Markdown is processed with [commonmark::markdown_html()].
#'
#' @section R Markdown:
#'
#' R Markdown is processed with [rmarkdown::render()].
#'
#' Regardless of what \code{output} type is specified in the input file,
#' \code{render()} will always use the \code{"html_document"} output format.
#'
#' Rending an R Markdown document can result in a lot of CSS. When all of the
#' CSS is included in the HTML `<head>` and sent to GMail it can result in a
#' message which is not correctly displayed inline in the Gmail web client.
#' To get around this you can specify `include_css = FALSE`. This will mean
#' that some styling will not be present in the resulting message, but that
#' the message content will be correctly rendered inline.
#'
#' @inheritParams text
#' @inheritParams html
#' @param msg A message object.
#' @param input The input Markdown file to be rendered or a character vector of
#'   Markdown text.
#' @param params A list of named parameters that override custom parameters
#'   specified in the YAML front-matter.
#' @param squish Whether to clean up whitespace in rendered document.
#' @param include_css Whether to include rendered CSS from various sources
#'   (\code{"rmd"} — native R Markdown CSS; \code{"bootstrap"} — Bootstrap CSS).
#'
#' @return A message object.
#' @seealso \code{\link{text}}, \code{\link{html}}
#' @export
#'
#' @examples
#'
#' # Plain Markdown
#'
#' markdown <- "[This](https://www.google.com) is a link."
#' filename <- "message.md"
#'
#' # Render from Markdown in character vector.
#' msg <- envelope() %>% render(markdown)
#'
#' # Create a file containing Markdown
#' cat(markdown, file = filename)
#'
#' # Render from Markdown in file.
#' msg <- envelope() %>% render(filename)
#'
#' # Cleanup.
#' file.remove(filename)
#'
#' # R Markdown
#'
#' filename <- "gh-doc.Rmd"
#'
#' # Create an Rmd document from template.
#' rmarkdown::draft(
#'   filename,
#'   template = "github_document",
#'   package = "rmarkdown",
#'   edit = FALSE
#' )
#'
#' # Check for suitable version of Pandoc (https://pandoc.org/).
#' #
#' # Need to have version 2.0 or greater to support required --quiet option.
#' #
#' pandoc <- rmarkdown::find_pandoc()
#' suitable_pandoc <- !is.null(pandoc$dir) && grepl("^2", pandoc$version)
#'
#' # Render from Rmd file.
#' if (suitable_pandoc) {
#'   msg <- envelope() %>%
#'     render(filename, include_css = c("rmd", "bootstrap"))
#' }
#'
#' # Cleanup.
#' file.remove(filename)
render <- function(msg,
                   input,
                   params = NULL,
                   squish = TRUE,
                   css_files = c(),
                   include_css = c("rmd", "bootstrap"),
                   language = FALSE,
                   interpolate = TRUE,
                   .open = "{{",
                   .close = "}}",
                   .envir = NULL) {
  stopifnot(is.envelope(msg))
  stopifnot(is.logical(interpolate))
  stopifnot(is.character(.open))
  stopifnot(is.character(.close))
  stopifnot(!length(css_files) || is.character(css_files))

  # What are permissible options for include_css?
  INCLUDE_CSS_OPTIONS <- eval(formals(render)$include_css)

  # Translate Boolean include_css:
  #
  # TRUE  - all CSS
  # FALSE - no CSS
  #
  if (is.logical(include_css) && length(include_css) == 1) {
    include_css <- if (include_css) INCLUDE_CSS_OPTIONS else NULL
  }

  # Check for include_css options that are not available.
  if (length(setdiff(include_css, INCLUDE_CSS_OPTIONS)) > 0) {
    stop(
      "Valid options for include_css are: ",
      paste(INCLUDE_CSS_OPTIONS, collapse = ", "),
      "."
    )
  }

  if (is.null(.envir)) {
    .envir <- parent.frame()
  } else {
    .envir <- list2env(.envir)
  } # nocov

  if (is_filepath(input)) {
    log_debug("Interpreting input as path to file.")
    plain <- !(file_ext(input) %in% c("Rmd", "Rmarkdown"))
    markdown <- read_text(input)
  } else {
    log_debug("Interpreting input as character vector.")
    plain <- TRUE
    markdown <- input
  }
  log_debug("Assuming input is ", ifelse(plain, "Plain", "R"), " Markdown.")

  if (interpolate) {
    markdown <- glue(markdown, .open = .open, .close = .close, .envir = .envir)
  }

  if (markdown == "") stop("Input is empty!")

  attr(markdown, "plain") <- plain

  msg <- manifest(
    msg,
    markdown,
    params,
    squish,
    css = list(extra = read_text(css_files)),
    include_css,
    language
  )

  if (get_option_invisible()) invisible(msg) else msg # nocov
}
