#' Set or query message expiry or reply-by time
#'
#' Functions to specify the time at which a message expires or by which a reply
#' is requested.
#'
#' Manipulate the \code{Expires} and \code{Reply-By} fields as specified in
#' \href{https://www.ietf.org/rfc/rfc2156.txt}{RFC 2156}.
#'
#' @name cutoff
#'
#' @param msg A message object.
#' @param datetime Date and time.
#' @param tz A character string specifying the time zone.
#' @return A message object.
NULL

#' @rdname cutoff
#' @export
#'
#' @examples
#' envelope() %>%
#'   expires("2030-01-01 13:25:00", "UTC")
expires <- function(msg, datetime = NULL, tz = "") {
  if (is.null(datetime)) {
    header_get(msg, "Expires")
  } else {
    datetime <- parse_datetime(datetime, tz)%>%
      format_datetime()

    msg <- header_set(msg, "Expires", datetime, append = FALSE)

    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}

#' @rdname cutoff
#' @export
#'
#' @examples
#' envelope() %>%
#'   replyby("2021-12-25 06:00:00", "GMT")
replyby <- function(msg, datetime = NULL, tz = "") {
  if (is.null(datetime)) {
    header_get(msg, "Reply-By")
  } else {
    datetime <- parse_datetime(datetime, tz)%>%
      format_datetime()

    msg <- header_set(msg, "Reply-By", datetime, append = FALSE)

    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}
