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
    msg$header$From <- normalise(as.address(from))
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
    msg$header$To <- normalise(as.address(arguments))
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
    msg$header$Cc <- normalise(as.address(arguments))
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
    msg$header$Bcc <- normalise(as.address(arguments))
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
    msg$header$Reply_To <- reply_to
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
    msg$header$Sender <- sender
    invisible(msg)
  }
}

#' Create an address object
#'
#' @param address An email address.
#'
#' @return An \code{address} object.
#' @export
#'
#' @examples
#' as.address("gerry@gmail.com")
as.address <- function(address) {
  class(address) <- "address"
  address
}

#' Print an address object
#'
#' @param address An \code{address} object.
#'
#' @export
#'
#' @examples
#' gerry <- as.address("gerry@gmail.com")
#' print(gerry)
print.address <- function(address) {
  print(as.character(address))
}

#' @export
raw <- function (x, ...) {
  UseMethod("raw", x)
}

#' Extract raw email address
#'
#' Strips the display name off an email address (if present).
#'
#' @param address An \code{address} object.
#'
#' @return A raw email address.
#' @export
#'
#' @examples
raw.address <- function(address) {
  if (length(address) > 1) {
    map_chr(address, raw.address)
  } else {
    address %>%
      str_remove("^.* <") %>%
      str_remove(">.*$") %>%
      str_trim()
  }
}

#' @export
display <- function (x, ...) {
  UseMethod("display", x)
}

#' Extract display name
#'
#' Extracts the display name from an email address.
#'
#' @param address An \code{address} object.
#'
#' @return The display name or \code{NA}.
#' @export
#'
#' @examples
display.address <- function(address) {
  if (length(address) > 1) {
    map_chr(address, display.address)
  } else {
    address <- as.address(address)
    # Check if address has display name.
    if (str_detect(address, "[<>]")) {
      address %>%
        str_remove("<.*$") %>%
        str_squish()
    } else {
      NA
    }
  }
}

#' @export
normalise <- function (x, ...) {
  UseMethod("normalise", x)
}

#' Normalise email address
#'
#' Makes an email address conform to RFC-5321.
#'
#' @param address An \code{address} object.
#'
#' @return
#' @export
#'
#' @examples
normalise.address <- function(address) {
  if (length(address) > 1) {
    map_chr(address, normalise.address)
  } else {
    address <- as.address(address)
    raw <- raw(address)
    display <- display(address)
    if (!is.na(display)) {
      paste0(display, " <", raw, ">")
    } else {
      raw
    }
  }
}
