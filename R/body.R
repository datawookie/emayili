#' Add a text body to a message object
#'
#' @param msg A message object.
#' @param content A string of message content.
#' @param disposition How content is presented (Content-Disposition).
#' @param charset How content is encoded.
#' @param encoding How content is transformed to ASCII (Content-Transfer-Encoding).
#' @return A message object.
#' @export
#' @examples
#' msg <- envelope()
#' text(msg, "Hello!")
text <- function(msg, content = NULL, disposition = "inline", charset = "utf-8", encoding = "7bit") {
  if (is.null(content)) {
    msg$body
  } else {
    type <- "text/plain"

    body <- mime(type, disposition, encoding, "flowed", charset)
    body$body <- content

    msg$parts <- c(msg$parts, list(body))

    invisible(msg)
  }
}

#' Add an HTML body to a message object
#'
#' @param msg A message object.
#' @param content A string of message content.
#' @param disposition How content is presented (Content-Disposition).
#' @param charset How content is encoded.
#' @param encoding How content is transformed to ASCII (Content-Transfer-Encoding).
#' @return A message object.
#' @export
#' @examples
#' msg <- envelope()
#' html(msg, "<b>Hello!</b>")
html <- function(msg, content = NULL, disposition = "inline", charset = "utf-8", encoding = "quoted-printable") {
  if (is.null(content)) {
    msg$body
  } else {
    type <- "text/html"

    body <- mime(type, disposition, encoding, NULL, charset)
    body$body <- qp_encode(content)

    msg$parts <- c(msg$parts, list(body))

    invisible(msg)
  }
}
