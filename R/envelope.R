is.envelope <- function(x) {
  "envelope" %in% class(x)
}

new_envelope <- function(
  to,
  from,
  cc,
  bcc,
  reply,
  subject,
  text,
  html
) {
  koevert <- structure(
    list(
      headers = list(),
      parts = NULL
    ),
    class="envelope"
  ) %>%
    header_set("Date", http_date(Sys.time()), append = FALSE) %>%
    header_set("X-Mailer", paste("{emayili}", packageVersion("emayili"), sep = "-"), append = FALSE) %>%
    header_set("MIME-Version", "1.0", append = FALSE)

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
  new_envelope(to, from, cc, bcc, reply, subject, text, html)
}

headers <- function(x) {
  paste(sapply(x$headers, as.character), collapse = "\r\n")
}

#' Print a message object
#'
#' The message body will be printed if `details` is `TRUE` or if the `envelope_details`
#' option is `TRUE`.
#'
#' @param x A message object.
#' @param details Whether or not to display full message content.
#' @param ... Further arguments passed to or from other methods.
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
  stopifnot(is.logical(details))
  #
  as.character(x, details = details) %>% cat("\n", sep = "")
}

#' Create formatted message.
#'
#' Accepts a message object and formats it as a MIME document.
#'
#' @inheritParams print.envelope
#' @export
#'
#' @return A formatted message object.
as.character.envelope <- function(x, ..., details = TRUE) {
  message <- list(
    headers(x)
  )

  if (length(x$parts) > 1) {
    body <- multipart_mixed(children = x$parts)
  } else {
    body <- x$parts[[1]]
  }

  if (details) {
    message <- c(message, as.character(body))
  }

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
    x$parts <- c(list(x$parts), list(child))
  }

  x
}
