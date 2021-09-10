check_message_body <- function(content) {
  if (length(content) > 1) {
    stop("Only a single message body allowed.", call. = FALSE)
  }
}

#' Add a text body to a message.
#'
#' Uses \code{glue::glue()} to evaluate expressions enclosed in brackets as R code.
#'
#' @param msg A message object.
#' @param content A string of message content.
#' @param disposition How content is presented (Content-Disposition).
#' @param charset How content is encoded.
#' @param encoding How content is transformed to ASCII (Content-Transfer-Encoding).
#' @param interpolate Whether to interpolate using \code{glue}.
#' @param .envir Environment used for \code{glue} interpolation. Defaults to \code{parent.frame()}.
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
  .envir = NULL
) {
  if (is.null(.envir)) .envir = parent.frame()
  else .envir = list2env(.envir)

  check_message_body(content)

  if (interpolate) content <- glue(content, .envir = .envir)

  body <- text_plain(content, disposition, charset, encoding)

  msg$parts <- c(msg$parts, list(body))

  if (get_option_invisible()) invisible(msg) else msg
}

#' Add an HTML body to a message object.
#'
#' @param msg A message object.
#' @param content A string of message content or the path to a file containing HTML.
#' @param disposition How content is presented (Content-Disposition).
#' @param charset How content is encoded.
#' @param encoding How content is transformed to ASCII (Content-Transfer-Encoding).
#' @return A message object.
#' @seealso \code{\link{text}}
#' @export
#' @examples
#' library(magrittr)
#'
#' # Inline HTML message.
#' msg <- envelope() %>% html("<b>Hello!</b>")
#'
#' # Read HTML message from a file.
#' msg <- envelope() %>% html("message.html")
html <- function(msg, content, images = c(), disposition = "inline", charset = "utf-8", encoding = "quoted-printable") {
  check_message_body(content)

  # Check if it's a file.
  #
  if (file.exists(content)) {
    content <- paste(readLines(content), collapse = "\n")
  }

  body <- text_html(content, disposition, charset, encoding)

  msg$parts <- c(msg$parts, list(body))

  for (image in images) {
    msg <- msg %>%
      attachment(path = image, cid = hexkey(basename(image)))
  }

  invisible(msg)
}
