#' Add or query subject of message
#'
#' @param msg A message object.
#' @param subject A subject for the message.
#'
#' @return A message object or the subject of the message object (if \code{subject} is \code{NULL}).
#' @export
#' @examples
#' msg %>% subject("Updated report")
subject <- function(msg, subject = NULL){
  if (is.null(subject)) {
    msg$header$Subject
  } else {
    msg$header$Subject <- subject
    msg
  }
}
