check_message_body <- function(content) {
  if (length(content) > 1) {
    stop("Only a single message body allowed.", call. = FALSE)
  }
}

#' Add a text body to a message.
#'
#' @param msg A message object.
#' @param content A string of message content.
#' @param disposition How content is presented (Content-Disposition).
#' @param charset How content is encoded.
#' @param encoding How content is transformed to ASCII (Content-Transfer-Encoding).
#' @return A message object.
#' @seealso \code{\link{html}}
#' @export
#' @examples
#' library(magrittr)
#'
#' msg <- envelope() %>% text("Hello!")
text <- function(msg, content, disposition = "inline", charset = "utf-8", encoding = "7bit") {
  check_message_body(content)

  type <- "text/plain"

  body <- mime(type, disposition, charset, encoding, NA)
  body$body <- content

  msg$parts <- c(msg$parts, list(body))

  invisible(msg)
}

#' Add an HTML body to a message object.
#'
#' @param msg A message object.
#' @param content A string of message content.
#' @param disposition How content is presented (Content-Disposition).
#' @param charset How content is encoded.
#' @param encoding How content is transformed to ASCII (Content-Transfer-Encoding).
#' @return A message object.
#' @seealso \code{\link{text}}
#' @export
#' @examples
#' library(magrittr)
#'
#' msg <- envelope() %>% html("<b>Hello!</b>")
html <- function(msg, content, disposition = "inline", charset = "utf-8", encoding = "quoted-printable") {
  check_message_body(content)

  type <- "text/html"

  body <- mime(type, disposition, charset, encoding, NA)
  body$body <- qp_encode(content)

  msg$parts <- c(msg$parts, list(body))

  invisible(msg)
}
