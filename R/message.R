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
    collapse = "\n"
  )
}

message <- function(msg){
  # CONTENT_TYPE = "multipart/mixed"
  CONTENT_TYPE = "multipart/related"

  message <- list(
    header(msg),
    "MIME-Version: 1.0",
    sprintf('Content-type: %s; boundary="%s"', CONTENT_TYPE, msg$boundary)
  )

  if (length(msg$parts)) {
    for (part in msg$parts) {
      message <- c(message, paste0("\n--", msg$boundary, "\n", format(part)))
    }
    message <- c(message, paste0("\n--", msg$boundary, "--\n"))
  }

  do.call(paste0, c(list(message), collapse = "\n"))
}
