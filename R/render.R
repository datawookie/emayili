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
  images <- file.path(sub(".html", "_files", output), "figure-html")

  cat(markdown, file = input)

  rmarkdown::render(
    input,
    output_file = output,
    quiet = TRUE,
    # Inline images don't work with GMail web client.
    output_options = list(self_contained = FALSE)
  )
  output = read_text(output)

  # Strip out <script> tags. These don't work in email, right?
  #
  xml <- read_html(output)
  xml_find_all(xml, "//script") %>% xml_remove()
  #
  # Don't actually want to strip out all <link> tags because one of them has
  # important CSS, but this is just to get things working in GMail web client.
  #
  xml_find_all(xml, "//link") %>% xml_remove()

  for (img in xml_find_all(xml, "//img")) {
    src <- xml_attr(img, "src")
    src <- paste0('cid:', hexkey(basename(src)))
    xml_attr(img, "src") <- src
  }

  msg <- msg %>% html(
    as.character(xml),
    images = list.files(images, full.names = TRUE)
  )

  if (get_option_invisible()) invisible(msg) else msg
}
