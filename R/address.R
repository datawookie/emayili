#' Add To field to message object header
#'
#' @param msg A message object.
#' @param ... Email addresses.
#' @return A message object.
#' @export
#' @examples
#' envelope() %>% to("bob@gmail.com", "alice@yahoo.com")
#' envelope() %>% to(c("bob@gmail.com", "alice@yahoo.com"))
to <- function(msg, ...){
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$To
  } else {
    msg$header$To <- arguments
    msg
  }
}

#' Add From field to message object header
#'
#' @param msg A message object.
#' @param ... Email address.
#' @return A message object.
#' @export
#' @examples
#' envelope() %>% from("craig@gmail.com")
from <- function(msg, from = NULL){
  if (is.null(from)) {
    msg$header$From
  } else {
    msg$header$From <- from
    msg
  }
}

#' Add Cc field to message object header
#'
#' @param msg A message object.
#' @param ... Email addresses.
#' @return A message object.
#' @export
#' @examples
#' envelope() %>% cc("bob@gmail.com", "alice@yahoo.com")
#' envelope() %>% cc(c("bob@gmail.com", "alice@yahoo.com"))
cc <- function(msg, ...){
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$Cc
  } else {
    msg$header$Cc <- paste0(arguments, collapse = ", ")
    msg
  }
}

#' Add Bcc field to message object header
#'
#' @param msg A message object.
#' @param ... Email addresses.
#' @return A message object.
#' @export
#' @examples
#' envelope() %>% bcc("bob@gmail.com", "alice@yahoo.com")
#' envelope() %>% bcc(c("bob@gmail.com", "alice@yahoo.com"))
bcc <- function(msg, ...){
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$Bcc
  } else {
    msg$header$Bcc <- paste0(arguments, collapse = ", ")
    msg
  }
}
