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

  msg$header$`X-Mailer` <- paste("{emayili}", packageVersion("emayili"), sep = "-")

  paste(
    msg$header %>% names() %>% sub("_", "-", .) %>% paste0(":") %>% sprintf("%-13s", .),
    msg$header,
    collapse = "\r\n"
  )
}

#' Create formatted message.
#'
#' @param msg A message object.
#'
#' @return A formatted message object.
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
