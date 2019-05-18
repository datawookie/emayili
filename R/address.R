#' @export
to <- function(x, ...) UseMethod("to")

#' @export
from <- function(x, ...) UseMethod("from")

#' @export
cc <- function(x, ...) UseMethod("cc")

#' @export
bcc <- function(x, ...) UseMethod("bcc")

#' @rdname envelope
#' @export
to.envelope <- function(msg, ...){
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$To
  } else {
    msg$header$To <- arguments
    msg
  }
}

#' @rdname envelope
#' @export
from.envelope <- function(msg, from = NULL){
  if (is.null(from)) {
    msg$header$From
  } else {
    msg$header$From <- from
    msg
  }
}

#' @rdname envelope
#' @export
cc.envelope <- function(msg, ...){
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$Cc
  } else {
    msg$header$Cc <- paste0(arguments, collapse = ", ")
    msg
  }
}

#' @rdname envelope
#' @export
bcc.envelope <- function(msg, ...){
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$Bcc
  } else {
    msg$header$Bcc <- paste0(arguments, collapse = ", ")
    msg
  }
}
