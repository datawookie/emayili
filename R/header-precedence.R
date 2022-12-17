#' Add fields for message importance and priority
#'
#' Functions to influence message delivery speed and importance.
#'
#' @name precedence
#'
#' @param msg A message object.
#' @return A message object.
NULL

#' Set message priority
#'
#' @details
#' The `priority()` function adds the `Priority` header field which gives a hint
#' to influence transmission speed and delivery. Valid values are
#' `"non-urgent"`, `"normal"`, and `"urgent"`. The non-standard `X-Priority`
#' header field is similar, for which valid values are `1` (Highest), `2`
#' (High), `3` (Normal, the default), `4` (Low), and `5` (Lowest).
#'
#' @rdname precedence
#' @param priority Priority level. One of \code{"non-urgent"}, \code{"normal"}, or \code{"urgent"}.
#'
#' @export
#'
#' @examples
#' # How rapidly does the message need to be delivered?
#' #
#' envelope() %>%
#'   subject("Deliver this immediately!") %>%
#'   priority("urgent")
#'
#' envelope(priority = "non-urgent") %>%
#'   subject("No rush with this.")
#'
priority <- function(msg, priority = NULL) {
  if (is.null(priority)) {
    header_get(msg, "Priority")
  } else {
    if (!(priority %in% LEVELS_PRIORITY)) {
      stop("Invalid priority. Options are: ", paste(LEVELS_PRIORITY, collapse = ", "), ".")
    }

    msg <- header_set(msg, "Priority", priority, append = FALSE)

    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}

#' Set message importance
#'
#' @details
#' The `importance()` function adds the `Importance` header field, which gives a
#' hint to the message recipient about how important the message is. Does not
#' influence delivery speed.
#'
#' @rdname precedence
#' @param importance Importance level. One of \code{"low"}, \code{"normal"}, or \code{"high"}.
#'
#' @export
#'
#' @examples
#' # How much attention should be paid by recipient?
#' #
#' envelope() %>%
#'   subject("Read this immediately!") %>%
#'   importance("high")
#'
#' envelope(importance = "low") %>%
#'   subject("Not important at all. Just delete.")
importance <- function(msg, importance = NULL) {
  if (is.null(importance)) {
    header_get(msg, "Importance")
  } else {
    if (!(importance %in% LEVELS_IMPORTANCE)) {
      stop("Invalid importance. Options are: ", paste(LEVELS_IMPORTANCE, collapse = ", "), ".")
    }

    msg <- header_set(msg, "Importance", importance, append = FALSE)

    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}
