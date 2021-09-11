#' Create a message.
#'
#' @param to See \code{to()}
#' @param from See \code{from()}
#' @param cc See \code{cc()}
#' @param bcc See \code{bcc()}
#' @param reply See \code{reply()}
#' @param subject See \code{subject()}
#' @param text See \code{text()}
#' @param html See \code{html()}
#'
#' @return A message object.
#' @seealso \code{\link{subject}}, \code{\link{from}}, \code{\link{to}}, \code{\link{cc}}, \code{\link{bcc}} and \code{\link{reply}}
#' @export
#' @examples
#' # Create an (empty) message object.
#' msg <- envelope()
#'
#' # Create a complete message object.
#' envelope(
#'   to = "bob@gmail.com",
#'   from = "craig@gmail.com",
#'   cc = "alex@gmail.com",
#'   bcc = "shannon@gmail.com",
#'   reply = "craig@yahoo.com",
#'   subject = "Hiya!",
#'   text = "Hi Bob, how are you?"
#' )
envelope <- function(
  to = NULL,
  from = NULL,
  cc = NULL,
  bcc = NULL,
  reply = NULL,
  subject = NULL,
  text = NULL,
  html = NULL
) {
  koevert <- structure(
    list(
      header = list(
        Date = http_date(Sys.time())
      ),
      parts = NULL
    ),
    class="envelope")

  if (!is.null(to)) koevert <- to(koevert, to)
  if (!is.null(from)) koevert <- from(koevert, from)
  if (!is.null(cc)) koevert <- cc(koevert, cc)
  if (!is.null(bcc)) koevert <- bcc(koevert, bcc)
  if (!is.null(reply)) koevert <- reply(koevert, reply)
  if (!is.null(subject)) koevert <- subject(koevert, subject)
  if (!is.null(text)) koevert <- text(koevert, text)
  if (!is.null(html)) koevert <- html(koevert, html)

  koevert
}

#' Print a message object
#'
#' The message body will be printed if `details` is `TRUE` or if the `envelope_details`
#' option is `TRUE`.
#'
#' @param x A message object.
#' @param details Whether or not to display full message content.
#' @param ... Any other arguments (for consistency of generic function).
#'
#' @export
#' @examples
#' msg <- envelope() %>% text("Hello, World!")
#'
#' print(msg)
#' print(msg, details = TRUE)
#'
#' options(envelope_details = TRUE)
#' print(msg)
print.envelope <- function(x, details = NA, ...) {
  if (is.na(details)) {
    details = get_option_details(default = FALSE)
  }
  if (!is.logical(details)) stop("details must be Boolean.", call. = FALSE)
  #
  ifelse(details, as.character(x), header(x)) %>% cat()
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
    "MIME-Version:              1.0"
  )

  if (length(x$parts) > 1) {
    body <- multipart_mixed(children = x$parts)
  } else {
    body <- x$parts[[1]]
  }

  message <- c(message, as.character(body))

  do.call(paste0, c(list(message), collapse = "\r\n"))
}

#' Append children to message
#'
#' @param x Message object
#' @param child A child to be appended
append.envelope <- function(x, child) {
  if(is.null(x$parts)) {
    x$parts <- list(child)
  } else {
    x$parts <- c(list(msg$parts), list(child))
  }

  x
}
