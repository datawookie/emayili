is.envelope <- function(x) {
  "envelope" %in% class(x)
}

#' Create a message.
#'
#' @param to See [to()].
#' @param from See [from()].
#' @param cc See [cc()].
#' @param bcc See [bcc()].
#' @param reply See [reply()].
#' @param subject See [subject()].
#' @param importance See [importance()].
#' @param priority See [priority()].
#' @param text See [text()].
#' @param html See [html()].
#'
#' @return A message object.
#' @seealso [subject()], [from()], [to()], [cc()], [bcc()] and [reply()].
#' @export
#' @examples
#' # Create an (empty) message object.
#' #
#' msg <- envelope()
#'
#' # Create a complete message object, specifying all available fields.
#' #
#' envelope(
#'   to = "bob@gmail.com",
#'   from = "craig@gmail.com",
#'   cc = "alex@gmail.com",
#'   bcc = "shannon@gmail.com",
#'   reply = "craig@yahoo.com",
#'   importance = "high",
#'   priority = "urgent",
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
  importance = NULL,
  priority = NULL,
  text = NULL,
  html = NULL
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
  if (!is.null(importance)) koevert <- importance(koevert, importance)
  if (!is.null(priority)) koevert <- priority(koevert, priority)
  if (!is.null(text)) koevert <- text(koevert, text)
  if (!is.null(html)) koevert <- html(koevert, html)

  koevert
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
    log_debug("Adding first child.")
    x$parts <- list(child)
  } else {
    log_debug("Adding subsequent child.")
    x$parts <- c(x$parts, list(child))
  }

  x
}
