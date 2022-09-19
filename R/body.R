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
#' @seealso \code{\link{html}}, \code{\link{render}}
#'
#' @export
#' @examples
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
text <- function(msg,
                 content,
                 disposition = "inline",
                 charset = "utf-8",
                 encoding = "7bit",
                 language = FALSE,
                 interpolate = TRUE,
                 .open = "{{",
                 .close = "}}",
                 .envir = NULL) {
  check_message_body(content)

  if (is.null(.envir)) {
    .envir <- parent.frame()
  } else {
    .envir <- list2env(.envir)
  }

  if (interpolate) content <- glue(content, .open = .open, .close = .close, .envir = .envir)

  body <- text_plain(content, disposition, charset, encoding, language)

  msg <- after(msg, body)

  if (get_option_invisible()) invisible(msg) else msg # nocov
}

#' Add an HTML body to a message object.
#'
#' @inheritParams text
#' @param css_files Extra CSS files.
#' @return A message object.
#' @seealso \code{\link{text}}, \code{\link{render}}
#' @export
#' @examples
#' # Inline HTML message.
#' envelope() %>% html("<b>Hello!</b>")
#'
#' # Read HTML message from a file.
#' htmlfile <- tempfile(fileext = ".html")
#' cat("<p>Hello!</p>\n", file = htmlfile)
#' envelope() %>% html(htmlfile)
#'
#' # You can pass a vector of character. Components will be separated by a
#' # "\n".
#' envelope() %>% html(c("<b>Hello</b>", "<p>World!</p>"))
#'
#' # You can also pass a tagList from {htmltools}.
#' if (requireNamespace("htmltools", quietly = TRUE)) {
#'   library(htmltools)
#'   envelope() %>% html(tagList(h2("Hello"), p("World!")))
#' }
html <- function(msg,
                 content,
                 disposition = "inline",
                 charset = "utf-8",
                 encoding = NA,
                 css_files = c(),
                 language = FALSE,
                 interpolate = TRUE,
                 .open = "{{",
                 .close = "}}",
                 .envir = NULL) {
  content <- list_to_char(content)

  # Check if it's a file.
  #
  if (file.exists(content)) {
    content <- stri_read_lines(content) %>% paste(collapse = "\n")
  }

  if (is.null(.envir)) {
    .envir <- parent.frame()
  } else {
    .envir <- list2env(.envir)
  }

  if (interpolate) content <- glue(content, .open = .open, .close = .close, .envir = .envir)

  content <- possibly(read_html, otherwise = NULL)(content)
  if (is.null(content)) {
    stop("Unable to find HTML file. Did you mean to provide verbatim HTML? Are you missing tags?")
  }

  msg <- attach_images(msg, content, disposition, charset, encoding, css_files, language)

  if (get_option_invisible()) invisible(msg) else msg # nocov
}
