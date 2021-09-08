# render <- function(msg, rmd_filepath, quiet = TRUE){
#   output_path = tempfile(fileext = ".html")
#   #
#   rmarkdown::render(rmd_filepath, output_file = output_path, quiet = quiet)
#
#   # OPTION 1
#   #
#   # msg <- msg %>% attachment(output_path)
#
#   # OPTION 2
#   #
#   content <- readChar(output_path, file.info(output_path)$size)
#   content <- sub("^<!DOCTYPE html>\n", "", content)
#   msg <- msg %>% html(content)
#
#  if (is.null(environment))
# environment <- parent.frame()
# content <- knitr::knit2html(
#   text = markdown,
#   options = c("use_xhtml", "smartypants", "mathjax", "highlight_code"),
#   stylesheet = GetMarkDownCss(),
#   envir = environment
# )
#
#   print(output_path)
#
#   invisible(msg)
# }

#' Render markdown into email
#'
#' @param msg A message object.
#' @param input The input markdown file to be rendered or a character vector of markdown text.
#'
#' @return A message object.
#' @export
#'
#' @examples
#' md <- "[This](https://www.google.com) is a link."
#' filename <- "message.md"
#'
#' # Render from markdown in character vector.
#' msg <- envelope() %>% render(md)
#'
#' # Create a file containing markdown.
#' cat(md, file = filename)
#'
#' # Render from markdown in file.
#' msg <- envelope() %>% render(filename)
#'
#' # Cleanup.
#' file.remove(filename)
render <- function(msg, input) {
  path <- try(normalizePath(input, mustWork = TRUE), silent = TRUE)
  #
  if (class(path) == "try-error") {
    log_debug("Input doesn't seem to be a file.")
    md <- input
  } else {
    md <- xfun::read_utf8(path)
  }

  content <- markdown_html(
    paste(
      as.character(md),
      collapse = "\n"
    )
  )

  invisible(msg %>% html(content))
}
