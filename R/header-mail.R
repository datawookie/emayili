# Headers for the mail protocol.

#' Add address fields to message
#'
#' @name addresses
#'
#' @param msg A message object.
#' @param addr Single address.
#' @param append Whether to append or replace addresses.
#' @return A message object.
NULL

#' @rdname addresses
#'
#' @param ... Addresses.
#'
#' @export
#' @examples
#' # Populating the To field.
#' msg <- envelope()
#' msg %>% to("bob@gmail.com, alice@yahoo.com")
#' msg %>% to("bob@gmail.com", "alice@yahoo.com")
#' msg %>% to(c("bob@gmail.com", "alice@yahoo.com"))
#'
to <- function(msg, ..., append = TRUE) {
  arguments <- c(...)
  if (is.null(arguments)) {
    header_get(msg, "To")
  } else {
    msg <- header_set(msg, "To", as.address(arguments), append = append, sep = ", ")
    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}

#' @rdname addresses
#'
#' @export
#' @examples
#' # Populating the Cc field.
#' msg <- envelope()
#' msg %>% cc("bob@gmail.com, alice@yahoo.com")
#' msg %>% cc("bob@gmail.com", "alice@yahoo.com")
#' msg %>% cc(c("bob@gmail.com", "alice@yahoo.com"))
#'
cc <- function(msg, ..., append = TRUE) {
  arguments <- c(...)
  if (is.null(arguments)) {
    header_get(msg, "Cc")
  } else {
    msg <- header_set(msg, "Cc", as.address(arguments), append = append, sep = ", ")
    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}

#' @rdname addresses
#'
#' @export
#' @examples
#' # Populating the Bcc field.
#' msg <- envelope()
#' msg %>% bcc("bob@gmail.com, alice@yahoo.com")
#' msg %>% bcc("bob@gmail.com", "alice@yahoo.com")
#' msg %>% bcc(c("bob@gmail.com", "alice@yahoo.com"))
#'
bcc <- function(msg, ..., append = TRUE) {
  arguments <- c(...)
  if (is.null(arguments)) {
    header_get(msg, "Bcc")
  } else {
    msg <- header_set(msg, "Bcc", as.address(arguments), append = append, sep = ", ")
    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}

#' @rdname addresses
#'
#' @export
#' @examples
#' msg <- envelope()
#'
#' # Populating the From field.
#' msg %>% from("craig@gmail.com")
#'
from <- function(msg, addr = NULL) {
  if (is.null(addr)) {
    header_get(msg, "From")
  } else {
    msg <- header_set(msg, "From", as.address(addr), append = FALSE)
    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}

#' @rdname addresses
#'
#' @export
#' @examples
#' # Populating the Reply-To field.
#' msg <- envelope()
#' msg %>% reply("gerry@gmail.com")
#'
reply <- function(msg, addr = NULL) {
  if (is.null(addr)) {
    header_get(msg, "Reply-To")
  } else {
    msg <- header_set(msg, "Reply-To", as.address(addr), append = FALSE)
    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}

#' @rdname addresses
#'
#' @export
#' @examples
#' # Populating the Sender field.
#' msg <- envelope()
#' msg %>% sender("on_behalf_of@gmail.com")
sender <- function(msg, addr = NULL) {
  if (is.null(addr)) {
    header_get(msg, "Sender")
  } else {
    msg <- header_set(msg, "Sender", as.address(addr), append = FALSE)
    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}

#' Add or query subject of message.
#'
#' @inheritParams text
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
#'
#' # Retrieve the subject for a message
#' subject(msg)
subject <- function(
  msg,
  subject = NULL,
  interpolate = TRUE,
  .open = "{{",
  .close = "}}",
  .envir = NULL
){
  if (is.null(subject)) {
    header_get(msg, "Subject")
  } else {
    if (is.null(.envir)) .envir = parent.frame()
    else .envir = list2env(.envir)

    if (interpolate) subject <- glue(subject, .open = .open, .close = .close, .envir = .envir)

    msg <- header_set(msg, "Subject", subject, append = FALSE)
    invisible(msg)
  }
}
