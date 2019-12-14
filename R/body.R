#' Add a text body to a message object
#'
#' @param msg A message object.
#' @param content A string of message content.
#' @return A message object.
#' @export
#' @examples
#' msg <- envelope()
#' text(msg, "Hello!")
text <- function(msg, content = NULL) {
  if (is.null(content)) {
    msg$body
  } else {
    type <- "text/plain"

    body <- mime(type, "inline", "7bit", "flowed", "utf-8")
    body$body <- content

    msg$parts <- c(msg$parts, list(body))

    invisible(msg)
  }
}

#' Add an HTML body to a message object
#'
#' @param msg A message object.
#' @param content A string of message content.
#' @return A message object.
#' @export
#' @examples
#' msg <- envelope()
#' html(msg, "<b>Hello!</b>")
html <- function(msg, content = NULL) {
  if (is.null(content)) {
    msg$body
  } else {
    type <- "text/html"

    body <- mime(type, "inline", "quoted-printable", NULL, "utf-8")
    body$body <- qp_encode(content)

    msg$parts <- c(msg$parts, list(body))

    invisible(msg)
  }
}
