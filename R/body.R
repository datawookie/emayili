check_message_body <- function(content) {
  if (length(content) > 1) {
    stop("Only a single message body allowed.", call. = FALSE)
  }
}

#' Add a text body to a message.
#'
#' Add \code{text/plain} content to a message.
#'
#' The \code{text/plain} format is
#' described in \href{https://www.ietf.org/rfc/rfc2646.txt}{RFC 2646}.
#'
#' Uses \code{glue::glue()} to evaluate expressions enclosed in brackets as R code.
#'
#' @inheritParams mime-parameters
#' @param msg A message object.
#'
#' @return A message object.
#' @seealso \code{\link{html}}
#'
#' @export
#' @examples
#' library(magrittr)
#'
#' msg <- envelope() %>% text("Hello!")
#'
#' # Using {glue} interpolation.
#' #
#' name <- "Alice"
#' msg <- envelope() %>% text("Hello {name}.")
#'
#' print(msg, details = TRUE)
#'
#' # Disable {glue} interpolation.
#' #
#' msg <- envelope() %>% text("This is a set: {1, 2, 3}.", interpolate = FALSE)
text <- function(
  msg,
  content,
  disposition = "inline",
  charset = "utf-8",
  encoding = "7bit",
  interpolate = TRUE,
  .open = "{{",
  .close = "}}",
  .envir = NULL
) {
  check_message_body(content)

  if (is.null(.envir)) .envir = parent.frame()
  else .envir = list2env(.envir)

  if (interpolate) content <- glue(content, .open = .open, .close = .close, .envir = .envir)

  body <- text_plain(content, disposition, charset, encoding)

  msg <- append(msg, body)

  if (get_option_invisible()) invisible(msg) else msg # nocov
}

#' Add an HTML body to a message object.
#'
#' @inheritParams text
#' @param css_files Extra CSS files.
#' @return A message object.
#' @seealso \code{\link{text}}
#' @export
#' @examples
#' library(magrittr)
#'
#' # Inline HTML message.
#' envelope() %>% html("<b>Hello!</b>")
#'
#' # Read HTML message from a file.
#' htmlfile <- tempfile(fileext = ".html")
#' cat("<p>Hello!</p>\n", file = htmlfile)
#' envelope() %>% html(htmlfile)
html <- function(
  msg,
  content,
  disposition = "inline",
  charset = "utf-8",
  encoding = "quoted-printable",
  css_files = c(),
  interpolate = TRUE,
  .open = "{{",
  .close = "}}",
  .envir = NULL
) {
  check_message_body(content)

  # Check if it's a file.
  #
  if (file.exists(content)) {
    content <- paste(readLines(content), collapse = "\n")
  }

  if (is.null(.envir)) .envir = parent.frame()
  else .envir = list2env(.envir)

  if (interpolate) content <- glue(content, .open = .open, .close = .close, .envir = .envir)

  body <- text_html(
    content, disposition, charset, encoding,
    css = read_text(css_files)
  )

  msg <- append(msg, body)

  if (get_option_invisible()) invisible(msg) else msg # nocov
}
