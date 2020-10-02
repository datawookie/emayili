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
      boundary = paste(sprintf("%x", sample(64, size = 16, replace = TRUE)), collapse = ""),
      parts = list()
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
#' @param x A message object.
#' @param details Whether or not to display full message content.
#' @param ... Any other arguments (for consistency of generic function).
#'
#' @export
print.envelope <- function(x, details=FALSE, ...) {
  ifelse(details, as.character(x), header(x)) %>% cat()
}
