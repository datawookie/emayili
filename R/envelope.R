#' Create a message object
#'
#' @family envelope
#' @export
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
print.envelope <- function(msg) {
  cat(header(msg))
}
