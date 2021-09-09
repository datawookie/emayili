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
    markdown <- read_file(input)
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
    markdown <- read_file(input)
  } else {
    log_warn("Interpreting input as character vector.")
    markdown <- input
  }

  if (interpolate) {
    markdown <- glue(markdown, .open = .open, .close = .close)
  }

  input <- tempfile(tmpdir = TMPDIR, fileext = ".Rmd")
  output <- tempfile(tmpdir = TMPDIR, fileext = ".html")

  cat(markdown, file = input)

  rmarkdown::render(
    input,
    output_file = output,
    quiet = TRUE
  )

  msg <- msg %>% html(read_file(output))

  if (get_option_invisible()) invisible(msg) else msg
}
