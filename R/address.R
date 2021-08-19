#' Email Address
#'
#' @param email Email address.
#' @param display Display name.
#'
#' @return An \code{address} object, representing an email address.
#' @export
#'
#' @examples
#' address("gerry@gmail.com")
#' address("gerry@gmail.com", "Gerald")
address <- function(email, display = NA, normalise = TRUE) {
  if (normalise) {
    email = str_trim(email)
    display = str_squish(display)
  }
  address <- structure(
    list(
      email = email,
      display = display
    ), class = c("address")
  )
  address
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
  if (class(address) == "address") {
    address
  } else {
    if (str_detect(address, "[<>]")) {
      display <- str_extract(address, ".*<") %>% str_remove("<")
      email <- str_extract(address, "<.*>") %>% str_remove_all("[<>]")

      if (is.na(email)) stop("Unable to parse email address.", call. = FALSE)
    } else {
      display <- NA
      email <- address
    }

    address(email, display)
  }
}

as.character.address <- function(x, ...) {
  if (is.na(x$display)) {
    x$email
  } else {
    glue("{x$display} <{x$email}>")
  }
}

#' Print an address object
#'
#' @param x An \code{address} object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
#'
#' @examples
#' gerry <- as.address("gerry@gmail.com")
#' print(gerry)
print.address <- function(x, ...) {
  print(as.character(x))
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
#' gerry <- as.address("Gerald <gerry@gmail.com>")
#' raw(gerry)
raw <- function(address) {
  if (length(address) > 1) {
    map_chr(address, raw)
  } else {
    address %>%
      str_remove("^.* <") %>%
      str_remove(">.*$") %>%
      str_trim()
  }
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
#' gerry <- as.address("Gerald <gerry@gmail.com>")
#' display(gerry)
display <- function(address) {
  # if (length(address) > 1) {
  #   map_chr(address, display)
  # } else {
  #   address <- as.address(address)
  #   # Check if address has display name.
  #   # if (str_detect(address, "[<>]")) {
  #   #   address %>%
  #   #     str_remove("<.*$") %>%
  #   #     str_squish()
  #   address$display
  #   # } else {
  #   #   NA
  #   # }
  # }
  as.address(address)$display
}

#' Normalise email address
#'
#' Makes an email address conform to RFC-5321.
#'
#' @param address An \code{address} object.
#'
#' @return An RFC-5321 email address.
#' @export
#'
#' @examples
#' gerry <- as.address("     Gerald   Durrell   <   gerry@gmail.com  >   ")
#' normalise(gerry)
normalise <- function(address) {
  if (length(address) > 1) {
    map_chr(address, normalise)
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
