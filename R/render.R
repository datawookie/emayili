#' Render plain markdown into email
#'
#' @inheritParams text
#'
#' @param msg A message object.
#' @param input The input markdown file to be rendered or a character vector of markdown text.
#' @param .open The opening delimiter.
#' @param .close The closing delimiter.
#'
#' @return A message object.
#' @export
#'
#' @examples
#' markdown <- "[This](https://www.google.com) is a link."
#' filename <- "message.md"
#'
#' # Render from markdown in character vector.
#' msg <- envelope() %>% md(markdown)
#'
#' # Create a file containing markdown.
#' cat(markdown, file = filename)
#'
#' # Render from markdown in file.
#' msg <- envelope() %>% md(filename)
#'
#' # Cleanup.
#' file.remove(filename)
md <- function(
  msg,
  input,
  interpolate = TRUE,
  .open = "{{",
  .close = "}}"
) {
  if (is_filepath(input)) {
    markdown <- read_text(input)
  } else {
    log_warn("Interpreting input as character vector.")
    markdown <- input
  }

  if (interpolate) {
    markdown <- glue(markdown, .open = .open, .close = .close)
  }

  msg <- msg %>% html(markdown_html(markdown))

  if (get_option_invisible()) invisible(msg) else msg
}

#' Render R markdown into email
#'
#' @inheritParams text
#' @inheritParams md
#'
#' @param msg A message object.
#' @param input The input R markdown file to be rendered or a character vector of R markdown text.
#'
#' @return A message object.
#' @export
#'
#' @examples
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
#' msg <- envelope() %>% rmd(filename)
#'
#' # Cleanup.
#' file.remove(filename)
rmd <- function(
  msg,
  input,
  interpolate = TRUE,
  .open = "{{",
  .close = "}}"
) {
  if (is_filepath(input)) {
    markdown <- read_text(input)
  } else {
    log_warn("Interpreting input as character vector.")
    markdown <- input
  }

  if (interpolate) {
    markdown <- glue(markdown, .open = .open, .close = .close)
  }

  if (markdown == "") stop("Input is empty!", call. = FALSE)

  input <- tempfile(fileext = ".Rmd")
  output <- tempfile(fileext = ".html")
  image_path <- file.path(sub(".html", "_files", output), "figure-html")

  cat(markdown, file = input)

  rmarkdown::render(
    input,
    output_file = output,
    quiet = TRUE,
    # Inline images don't work with GMail web client.
    output_options = list(self_contained = FALSE)
  )
  output <- read_html(output)

  # Strip out <script> tags. These don't work in email, right?
  #
  xml_find_all(output, "//script") %>% xml_remove()

  # Remove comments.
  #
  xml_find_all(output, "//comment()") %>% xml_remove()

  # Extract CSS from <link> tags.
  #
  css <- xml_find_all(output, "//link[starts-with(@href,'data:text/css')]") %>%
    xml_attr("href") %>%
    unlist() %>%
    url_decode() %>%
    str_replace("data:text/css,", "")

  # Delete <link> tags.
  #
  xml_find_all(output, "//link") %>% xml_remove()

  # Add extracted CSS to CSS from <style> tags.
  #
  style <- xml_find_all(output, "//style")
  css <- style %>%
    xml_contents() %>%
    as.character() %>%
    c(css) %>%
    str_c(collapse = "\n") %>%
    str_squish()

  # Delete (multiple) existing <style> tags.
  #
  xml_remove(style)

  # Write consolidated CSS to single <style> tag.
  #
  xml_add_child(
    xml_find_first(output, "//head"),
    "style",
    css,
    type = "text/css"
  )

  # Convert image links into CID references.
  #
  for (img in xml_find_all(output, "//img")) {
    src <- xml_attr(img, "src")
    # Check for Base64 encoded inline image.
    if (!str_detect(src, "^data:image")) {
      xml_attr(img, "src") <- paste0('cid:', hexkey(basename(src)))
    }
  }

  body <- multipart_related(
    children = list(
      text_html(as.character(output))
    )
  )

  for (image in list.files(image_path, full.names = TRUE)) {
    body <- append(body, other(filename = image, cid = hexkey(basename(image))))
  }

  msg <- append(msg, body)

  if (get_option_invisible()) invisible(msg) else msg
}
