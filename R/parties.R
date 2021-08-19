#' Extract sender and recipient(s)
#'
#' @param msg A message object.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' email <- envelope() %>%
#'   from("Gerald <gerald@gmail.com>") %>%
#'   to(c("bob@gmail.com", "alice@yahoo.com")) %>%
#'   cc("Craig     < craig@gmail.com>") %>%
#'   bcc("  Erin   <erin@yahoo.co.uk    >")
#'
#' parties(email)
parties <- function(msg) {
  # Avoid "no visible binding for global variable" note.
  address <- NULL

  map_dfr(c("From", "To", "Cc", "Bcc"), function(type) {
    tibble(
      type,
      address = as.list(msg$header[type])
    )
  }) %>%
    unnest(cols = c(address)) %>%
    mutate(
      address = as.address(address),
      display = display(address),
      raw = raw(address)
    )
}

#' Add From field to message
#'
#' @param msg A message object.
#' @param from Email address.
#' @return A message object.
#' @seealso \code{\link{to}}, \code{\link{cc}}, \code{\link{bcc}}, \code{\link{sender}}, \code{\link{reply}} and \code{\link{subject}}
#' @export
#' @examples
#' msg <- envelope()
#' from(msg, "craig@gmail.com")
from <- function(msg, from = NULL) {
  if (is.null(from)) {
    msg$header$From
  } else {
    msg$header$From <- as.address(from)
    invisible(msg)
  }
}

#' Add To field to message
#'
#' @param msg A message object.
#' @param ... Email addresses.
#' @return A message object.
#' @seealso \code{\link{cc}}, \code{\link{bcc}}, \code{\link{from}}, \code{\link{sender}}, \code{\link{reply}} and \code{\link{subject}}
#' @export
#' @examples
#' msg <- envelope()
#' to(msg, "bob@gmail.com", "alice@yahoo.com")
#' to(msg, c("bob@gmail.com", "alice@yahoo.com"))
to <- function(msg, ...) {
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$To
  } else {
    msg$header$To <- as.address(arguments)
    invisible(msg)
  }
}

#' Add Cc field to message
#'
#' @param msg A message object.
#' @param ... Email addresses.
#' @return A message object.
#' @seealso \code{\link{to}}, \code{\link{bcc}}, \code{\link{from}}, \code{\link{sender}}, \code{\link{reply}} and \code{\link{subject}}
#' @export
#' @examples
#' msg <- envelope()
#' cc(msg, "bob@gmail.com", "alice@yahoo.com")
#' cc(msg, c("bob@gmail.com", "alice@yahoo.com"))
cc <- function(msg, ...) {
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$Cc
  } else {
    msg$header$Cc <- as.address(arguments)
    invisible(msg)
  }
}

#' Add Bcc field to message
#'
#' @param msg A message object.
#' @param ... Email addresses.
#' @return A message object.
#' @seealso \code{\link{to}}, \code{\link{cc}}, \code{\link{from}}, \code{\link{sender}}, \code{\link{reply}} and \code{\link{subject}}
#' @export
#' @examples
#' msg <- envelope()
#' bcc(msg, "bob@gmail.com", "alice@yahoo.com")
#' bcc(msg, c("bob@gmail.com", "alice@yahoo.com"))
bcc <- function(msg, ...) {
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$Bcc
  } else {
    msg$header$Bcc <- as.address(arguments)
    invisible(msg)
  }
}

#' Add Reply-To field to message
#'
#' @param msg A message object.
#' @param reply_to Email address.
#' @return A message object.
#' @seealso \code{\link{to}}, \code{\link{cc}}, \code{\link{bcc}}, \code{\link{from}}, \code{\link{sender}} and \code{\link{subject}}
#' @export
#' @examples
#' msg <- envelope()
#' reply(msg, "gerry@gmail.com")
reply <- function(msg, reply_to = NULL) {
  if (is.null(reply_to)) {
    msg$header$Reply_To
  } else {
    msg$header$Reply_To <- as.address(reply_to)
    invisible(msg)
  }
}

#' Add Sender (on behalf of) field to message
#'
#' @param msg A message object.
#' @param sender Email address.
#' @return A message object.
#' @seealso \code{\link{to}}, \code{\link{cc}}, \code{\link{bcc}}, \code{\link{from}}, \code{\link{reply}} and \code{\link{subject}}
#' @export
#' @examples
#' msg <- envelope()
#' sender(msg, "on_behalf_of@gmail.com")
sender <- function(msg, sender = NULL) {
  if (is.null(sender)) {
    msg$header$Sender
  } else {
    msg$header$Sender <- as.address(sender)
    invisible(msg)
  }
}
