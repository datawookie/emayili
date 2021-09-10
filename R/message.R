#' Create formatted header.
#'
#' @param msg A message object.
#'
#' @return A message header.
header <- function(msg) {
  if (!is.null(msg$header$To)) {
    msg$header$To <- paste0(msg$header$To, collapse = ", ")
  }
  if (!is.null(msg$header$Cc)) {
    msg$header$Cc <- paste0(msg$header$Cc, collapse = ", ")
  }
  if (!is.null(msg$header$Bcc)) {
    msg$header$Bcc <- paste0(msg$header$Bcc, collapse = ", ")
  }
  if (!is.null(msg$header$From)) msg$header$From <- as.character(msg$header$From)
  if (!is.null(msg$header$Sender)) msg$header$Sender <- as.character(msg$header$Sender)

  msg$header$`X-Mailer` <- paste("{emayili}", packageVersion("emayili"), sep = "-")

  paste(
    msg$header %>% names() %>% sub("_", "-", .) %>% paste0(":") %>% sprintf("%-13s", .),
    msg$header,
    collapse = "\r\n"
  )
}

#' Create formatted message.
#'
#' Accepts a message object and formats it as a MIME document.
#'
#' @section MIME Multipart Types:
#'
#' There are a number of options for multipart messages:
#'
#' \itemize{
#'  \item{\code{multipart/mixed} — }{Used for sending content with multiple independent parts either inline or as attachments. Each part can have different \code{Content-Type}.}
#'  \item{\code{multipart/alternative} — }{Used when each part of the message is an "alternative" version of the same content. The order of the parts is important: preferred and/or more complex formats should be found towards the end.
#'
#'  \emph{Example:} A message with both plain text and HTML versions.}
#'  \item{\code{multipart/digest} — }{Used to send multiple plain text messages.}
#'  \item{\code{multipart/related} — }{Used when each part of the the message represents a component of the complete message.
#'
#'  \emph{Example:} A web page with images.}
#'  \item{\code{multipart/signed} — }{Used when a message has a digital signature attached.}
#'  \item{\code{multipart/encrypted} — }{Used for a message with encrypted content.}
#' }
#'
#' A nice illustration of how some of these relate can be found at \url{https://stackoverflow.com/a/40420648/633961}.
#'
#' @param x A message object.
#' @param ... Further arguments passed to or from other methods.
#' @export
#'
#' @return A formatted message object.
as.character.envelope <- function(x, ...) {
  CONTENT_TYPE = "multipart/related"

  message <- list(
    header(x),
    "MIME-Version: 1.0",
    sprintf('Content-type: %s; boundary="%s"', CONTENT_TYPE, x$boundary)
  )

  if (length(x$parts)) {
    for (part in x$parts) {
      message <- c(message, paste0("\r\n--", x$boundary, "\r\n", format(part)))
    }
    message <- c(message, paste0("\r\n--", x$boundary, "--\r\n"))
  }

  do.call(paste0, c(list(message), collapse = "\r\n"))
}
