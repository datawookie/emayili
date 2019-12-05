#' Add a body to a message object
#'
#' @param msg A message object.
#' @param content A string of message content.
#' @param type Content type (plain text or HTML).
#' @return A message object.
#' @export
#' @examples
#' msg <- envelope()
#' body(msg, "Hello!")
#' body(msg, "<b>Hello!</b>", "html")
body <- function(msg, content = NULL, type = c("plain", "html")){
  type = type[1]

  if (is.null(content)) {
    msg$body
  } else {
    type <- paste("text", type, sep = "/")

    if (type == "text/plain") {
      body <- mime(type, "inline", "7bit", "flowed", "utf-8")
      body$body <- content
    } else {
      body <- mime(type, "inline", "quoted-printable", NULL, "utf-8")
      body$body <- qp_encode(content)
    }

    msg$parts <- c(msg$parts, list(body))

    invisible(msg)
  }
}
