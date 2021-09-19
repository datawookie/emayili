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
#' @param msg A message object.
#' @param input The input Markdown file to be rendered or a character vector of Markdown text.
#' @param params A list of named parameters that override custom parameters specified in the YAML front-matter.
#' @param css_files Extra CSS files.
#' @param include_css Whether to include rendered CSS.
#'
#' @return A message object.
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
#' # Render from Rmd file.
#' if (!is.null(rmarkdown::find_pandoc()$dir)) {
#'   msg <- envelope() %>% render(filename)
#' }
#'
#' # Cleanup.
#' file.remove(filename)
render <- function(
  msg,
  input,
  params = NULL,
  css_files = c(),
  include_css = TRUE,
  interpolate = TRUE,
  .open = "{{",
  .close = "}}",
  .envir = NULL
) {
  stopifnot(is.envelope(msg))
  stopifnot(is.null(params) || is.list(params))
  stopifnot(!length(css_files) || is.character(css_files))
  stopifnot(is.logical(include_css))
  stopifnot(is.logical(interpolate))
  stopifnot(is.character(.open))
  stopifnot(is.character(.close))

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

  output <- tempfile(fileext = ".html")
  image_path <- file.path(sub(".html", "_files", output), "figure-html")

  if (plain) {
    output <- markdown_html(markdown)
    body <- text_html(output)
  } else {
    # Write input to file.
    input <- tempfile(fileext = ".Rmd")
    cat(markdown, file = input)

    # Render from file to file.
    rmarkdown::render(
      input,
      output_format = "html_document",
      output_file = output,
      params = params,
      quiet = TRUE,
      # Pass a clean environment. This makes it possible to use params name in
      # this scope.
      envir = new.env(),
      # Inline images don't work with GMail web client.
      output_options = list(self_contained = FALSE)
    )

    # Read output from file.
    output <- read_html(output)

    # Check that extra CSS files exist.
    #
    for (css in css_files) {
      if (!file.exists(css)) stop("Unable to find CSS file: ", css, ".")
    }

    # Extract CSS from <link> and <style> tags.
    #
    css <- list(
      # Inline CSS in <link> tags.
      xml_find_all(output, "//link[starts-with(@href,'data:text/css')]") %>%
        xml_attr("href") %>%
        unlist() %>%
        url_decode() %>%
        str_replace("data:text/css,", ""),
      # External CSS in <link> tags.
      xml_find_all(output, "//link[not(starts-with(@href,'data:text/css'))]") %>%
        xml_attr("href") %>%
        file.path(dirname(input), .) %>%
        map_chr(read_text),
      # Inline CSS in <style> tags.
      xml_find_all(output, "//style") %>%
        xml_text() %>%
        unlist(),
      # Extra CSS.
      css_files %>%
        map_chr(read_text)
    ) %>%
      unlist() %>%
      str_c(collapse = "\n") %>%
      css_remove_comment() %>%
      str_squish()

    # Delete <script>, <link> and <style> tags.
    #
    xml_find_all(output, "//script | //link | //style") %>% xml_remove()

    # Remove comments.
    #
    xml_find_all(output, "//comment()") %>% xml_remove()

    if (include_css) {
      # Remove all other tags in <head>
      xml_find_all(output, "//head/*") %>% xml_remove()
      # Write consolidated CSS to single <style> tag.
      xml_add_child(
        xml_find_first(output, "//head"),
        "style",
        css,
        type = "text/css"
      )
    } else {
      xml_find_first(output, "//head") %>% xml_remove()
    }

    # Convert image links into CID references.
    #
    for (img in xml_find_all(output, "//img")) {
      src <- xml_attr(img, "src")
      # Check for Base64 encoded inline image.
      if (!str_detect(src, "^data:image")) {
        xml_attr(img, "src") <- paste0('cid:', hexkey(basename(src)))
      }
    }

    output <- as.character(output) %>%
      # Remove <!DOCTYPE> tag.
      str_replace("[:space:]*<!DOCTYPE html>[:space:]*", "")

    body <- multipart_related(
      children = list(
        text_html(output)
      )
    )
  }

  # Attach images with appropriate CID.
  #
  for (image in list.files(image_path, full.names = TRUE)) {
    body <- append(
      body,
      other(
        filename = image,
        cid = hexkey(basename(image)),
        disposition = "inline"
      )
    )
  }

  msg <- append(msg, body)

  if (get_option_invisible()) invisible(msg) else msg # nocov
}
