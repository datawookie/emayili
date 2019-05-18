#' @export
subject <- function(x, ...) UseMethod("subject")

#' @rdname envelope
#' @export
subject.envelope <- function(msg, topic = NULL){
  if (is.null(topic)) {
    msg$header$Subject
  } else {
    msg$header$Subject <- topic
    msg
  }
}
