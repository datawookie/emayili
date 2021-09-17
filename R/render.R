#' Render Markdown into email
#'
#' @description
#'
#' Render either Plain Markdown or R Markdown directly into the body of an email.
#'
#' Regardless of what is specified in the input file, \code{render()} will always
#' use the \code{"html_document"} output format.
#'
#' @inheritParams text
#' @param msg A message object.
#' @param input The input Markdown file to be rendered or a character vector of Markdown text.
#' @param plain Whether to treat the input as plain or R Markdown.
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
#' msg <- envelope() %>% render(markdown, plain = TRUE)
#'
#' # Create a file containing Markdown
#' cat(markdown, file = filename)
#'
#' # Render from Markdown in file.
#' msg <- envelope() %>% render(filename, plain = TRUE)
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
#' msg <- envelope() %>% render(filename)
#'
#' # Cleanup.
#' file.remove(filename)
render <- function(
  msg,
  input,
  plain = FALSE,
  include_css = TRUE,
  interpolate = TRUE,
  .open = "{{",
  .close = "}}",
  .envir = NULL
) {
  stopifnot(is.envelope(msg))
  stopifnot(is.logical(plain))
  stopifnot(is.logical(include_css))
  stopifnot(is.logical(interpolate))
  stopifnot(is.character(.open))
  stopifnot(is.character(.close))

  if (is.null(.envir)) .envir = parent.frame()
  else .envir = list2env(.envir) # nocov

  if (is_filepath(input)) {
    log_debug("Interpreting input as path to Markdown file.")
    markdown <- read_text(input)
  } else {
    log_debug("Interpreting input as character vector of Markdown.")
    markdown <- input
  }

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
      quiet = TRUE,
      # Inline images don't work with GMail web client.
      output_options = list(self_contained = FALSE)
    )

    # Read output from file.
    output <- read_html(output)

    # Extract CSS from <link> and <style> tags.
    #
    css <- list(
      # Inline CSS in <link> tags.
      xml_find_all(output, "//link[starts-with(@href,'data:text/css')]") %>%
        xml_attr("href") %>%
        unlist() %>%
        url_decode() %>%
        str_replace("data:text/css,", ""),
      # Inline CSS in <style> tags.
      xml_find_all(output, "//style") %>%
        xml_text() %>%
        unlist()
    ) %>%
      unlist() %>%
      str_c(collapse = "\n") %>%
      str_squish()

    # Delete <link> and <style> tags.
    #
    xml_find_all(output, "//link") %>% xml_remove()
    xml_find_all(output, "//style") %>% xml_remove()

    # Strip out <script> tags.
    #
    xml_find_all(output, "//script") %>% xml_remove()

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

  for (image in list.files(image_path, full.names = TRUE)) {
    body <- append(body, other(filename = image, cid = hexkey(basename(image))))
  }

  msg <- append(msg, body)

  if (get_option_invisible()) invisible(msg) else msg # nocov
}
