#' Create formatted header
#'
#' @param msg
#'
#' @return
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

  paste(
    msg$header %>% names() %>% sub("_", "-", .) %>% paste0(":") %>% sprintf("%-13s", .),
    msg$header,
    collapse = "\r\n"
  )
}

#' create formatted message
#'
#' @param msg
#'
#' @return
message <- function(msg){
  CONTENT_TYPE = "multipart/related"

  message <- list(
    header(msg),
    "MIME-Version: 1.0",
    sprintf('Content-type: %s; boundary="%s"', CONTENT_TYPE, msg$boundary)
  )

  if (length(msg$parts)) {
    for (part in msg$parts) {
      message <- c(message, paste0("\r\n--", msg$boundary, "\r\n", format(part)))
    }
    message <- c(message, paste0("\r\n--", msg$boundary, "--\r\n"))
  }

  do.call(paste0, c(list(message), collapse = "\r\n"))
}
