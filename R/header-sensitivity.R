LEVELS_SENSITIVITY <- c("personal", "private", "company-confidential")

#' Set or query message sensitivity
#'
#' Manipulate the \code{Sensitivity} field as specified in \href{https://www.ietf.org/rfc/rfc2156.txt}{RFC 2156}.
#'
#' @param msg A message object.
#' @param sensitivity Sensitivity level. One of \code{"personal"}, \code{"private"}, or \code{"company-confidential"}.
#' @return A message object.
#'
#' @export
#'
#' @examples
#' # Not sensitive.
#' envelope() %>%
#'   subject("Your daily dose of spam)
#'
#' # Sensitive personal message.
#' envelope() %>%
#'   subject("The results from your test") %>%
#'   sensitivity("personal")
#'
#' # Sensitive private message.
#' envelope() %>%
#'   subject("Your OTP (don't show this to anybody!") %>%
#'   sensitivity("private")
#'
#' # Sensitive business message.
#' envelope() %>%
#'   subject("Top Secret Strategy Document") %>%
#'   sensitivity("company-confidential")
#'
sensitivity <- function(msg, sensitivity = NULL) {
  if (is.null(sensitivity)) {
    header_get(msg, "Sensitivity")
  } else {
    if (!(sensitivity %in% LEVELS_SENSITIVITY)) {
      stop("Invalid sensitivity Options are: ", paste(LEVELS_SENSITIVITY, collapse = ", "), ".")
    }

    msg <- header_set(msg, "Sensitivity", sensitivity, append = FALSE)

    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}
