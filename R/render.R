manifest <- function(
  msg,
  markdown,
  params = NULL,
  squish = TRUE,
  css,
  include_css,
  language
) {
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
    cat(markdown, file = input)

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
            if(is.null(caption)) NULL else tags$figcaption(caption),
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

  # Extract CSS from <link> and <style> tags and append.
  #
  css <- c(
    # * Inline CSS in <link> tags.
    if (any("rmd" == include_css)) {
      inline = xml_find_all(output, "//link[starts-with(@href,'data:text/css')]") %>%
        xml_attr("href") %>%
        unlist() %>%
        url_decode() %>%
        str_replace("data:text/css,", "")
    } else NULL,
    # * External CSS in <link> tags.
    #
    # This is CSS for:
    #
    # - Bootstrap and
    # - highlight.js.
    #
    # Doesn't apply to Plain Markdown.
    #
    if (exists("input")) {
      external = xml_find_all(output, "//link[not(starts-with(@href,'data:text/css'))]") %>%
        xml_attr("href") %>%
        map(function(path) {
          include_css <- setdiff(include_css, "rmd")
          # Check is CSS path matches one of the requested options.
          if (length(include_css)) {
            if (str_detect(path, paste0("/", include_css, collapse = "|"))) path else NULL
          } else NULL
        }) %>%
        unlist() %>%
        file.path(dirname(input), .) %>%
        map_chr(read_text)
    } else NULL,
    # * Inline CSS in <style> tags.
    if (any("rmd" == include_css)) {
      style = xml_find_all(output, "//style") %>%
        xml_text() %>%
        unlist()
    } else NULL,
    # * Add custom CSS rules last so that it overrides preceding rules.
    css
  )

  css_files <- tempfile(fileext = ".css")
  writeLines(paste(css, collapse = "\n"), css_files)

  attach_images(msg, output, disposition = "inline", charset = "utf-8", encoding = NA, css_files, language)
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
#' @inheritParams text
#' @inheritParams html
#' @param msg A message object.
#' @param input The input Markdown file to be rendered or a character vector of Markdown text.
#' @param params A list of named parameters that override custom parameters specified in the YAML front-matter.
#' @param squish Whether to clean up whitespace in rendered document.
#' @param include_css Whether to include rendered CSS from various sources (\code{"rmd"} — native R Markdown CSS; \code{"bootstrap"} — Bootstrap CSS; \code{"highlight"} — highlight.js CSS).
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
#'     render(filename, include_css = c("rmd", "highlight"))
#' }
#'
#' # Cleanup.
#' file.remove(filename)
render <- function(
  msg,
  input,
  params = NULL,
  squish = TRUE,
  css_files = c(),
  include_css = c("rmd", "bootstrap", "highlight"),
  language = FALSE,
  interpolate = TRUE,
  .open = "{{",
  .close = "}}",
  .envir = NULL
) {
  stopifnot(is.envelope(msg))
  stopifnot(is.logical(interpolate))
  stopifnot(is.character(.open))
  stopifnot(is.character(.close))
  stopifnot(!length(css_files) || is.character(css_files))

  INCLUDE_CSS_OPTIONS <- eval(formals(render)$include_css)

  # Translate Boolean include_css:
  #
  # TRUE  - all CSS
  # FALSE - no CSS).
  #
  if (is.logical(include_css) && length(include_css) == 1) {
    include_css <- if(include_css) INCLUDE_CSS_OPTIONS else NULL
  }

  if (length(setdiff(include_css, INCLUDE_CSS_OPTIONS)) > 0) {
    stop(
      "Valid options for include_css are: ",
      paste(INCLUDE_CSS_OPTIONS, collapse = ", "),
      "."
    )
  }

  if (is.null(.envir)) .envir = parent.frame()
  else .envir = list2env(.envir) # nocov

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
    list(extra = read_text(css_files)),
    include_css,
    language
  )

  if (get_option_invisible()) invisible(msg) else msg # nocov
}
