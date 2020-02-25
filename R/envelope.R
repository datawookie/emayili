#' Create a message.
#'
#' @return A message object.
#' @seealso \code{\link{subject}}, \code{\link{from}}, \code{\link{to}}, \code{\link{cc}}, \code{\link{bcc}} and \code{\link{reply}}
#' @export
#' @examples
#' # Create an (empty) message object.
#' msg <- envelope()
envelope <- function() {
  structure(
    list(
      header = list(
        Date = http_date(Sys.time())
      ),
      boundary = paste(sprintf("%x", sample(64, size = 16, replace = TRUE)), collapse = ""),
      parts = list()
    ),
    class="envelope")
}

#' @export
print.envelope <- function(x, ...) {
  cat(paste0(header(x), "\r\n"))
}
