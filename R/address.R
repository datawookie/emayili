#' Add To field to message object header
#'
#' @param msg A message object.
#' @param ... Email addresses.
#' @return A message object.
#' @export
#' @examples
#' msg <- envelope()
#' to(msg, "bob@gmail.com", "alice@yahoo.com")
#' to(msg, c("bob@gmail.com", "alice@yahoo.com"))
to <- function(msg, ...){
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$To
  } else {
    msg$header$To <- arguments
    invisible(msg)
  }
}

#' Add From field to message object header
#'
#' @param msg A message object.
#' @param ... Email address.
#' @return A message object.
#' @export
#' @examples
#' msg <- envelope()
#' from(msg, "craig@gmail.com")
from <- function(msg, from = NULL){
  if (is.null(from)) {
    msg$header$From
  } else {
    msg$header$From <- from
    invisible(msg)
  }
}

#' Add Cc field to message object header
#'
#' @param msg A message object.
#' @param ... Email addresses.
#' @return A message object.
#' @export
#' @examples
#' msg <- envelope()
#' cc(msg, "bob@gmail.com", "alice@yahoo.com")
#' cc(msg, c("bob@gmail.com", "alice@yahoo.com"))
cc <- function(msg, ...){
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$Cc
  } else {
    msg$header$Cc <- paste0(arguments, collapse = ", ")
    invisible(msg)
  }
}

#' Add Bcc field to message object header
#'
#' @param msg A message object.
#' @param ... Email addresses.
#' @return A message object.
#' @export
#' @examples
#' msg <- envelope()
#' bcc(msg, "bob@gmail.com", "alice@yahoo.com")
#' bcc(msg, c("bob@gmail.com", "alice@yahoo.com"))
bcc <- function(msg, ...){
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$Bcc
  } else {
    msg$header$Bcc <- paste0(arguments, collapse = ", ")
    invisible(msg)
  }
}
