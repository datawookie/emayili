#' Add or query subject of message.
#'
#' @param msg A message object.
#' @param subject A subject for the message.
#'
#' @return A message object or the subject of the message object (if \code{subject} is \code{NULL}).
#' @seealso \code{\link{to}}, \code{\link{from}}, \code{\link{cc}}, \code{\link{bcc}} and \code{\link{reply}}
#' @export
#' @examples
#' library(magrittr)
#'
#' # Create a message and set the subject
#' msg <- envelope() %>% subject("Updated report")
#' # Retrieve the subject for a message
#' subject(msg)
subject <- function(msg, subject = NULL){
  if (is.null(subject)) {
    msg$header$Subject
  } else {
    msg$header$Subject <- subject
    invisible(msg)
  }
}
