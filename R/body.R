#' @export
body <- function(x, ...) UseMethod("body")

#' @rdname envelope
#' @export
body.envelope <- function(msg, content = NULL, type = c("plain", "html")){
  type = type[1]

  if (is.null(content)) {
    msg$body
  } else {
    type <- paste("text", type, sep = "/")

    if (type == "text/plain") {
      body <- mime(type, "quoted-printable", "flowed", "utf-8")
    } else {
      body <- mime(type, "quoted-printable", NULL, "utf-8")
    }
    body$body <- content

    msg$parts <- c(msg$parts, list(body))

    msg
  }
}
