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
    msg$header %>% names() %>% sub("_", "-", .) %>% paste0(":") %>% sprintf("%-26s", .),
    msg$header,
    collapse = "\r\n"
  )
}
