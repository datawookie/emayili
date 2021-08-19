#' Helper function for creating address objects
#'
#' @inheritParams address
#'
#' @return An \code{address} object, representing an email address.
new_address <- function(email = character(), display = character(), normalise = TRUE) {
  vec_assert(email, ptype = character())
  vec_assert(display, ptype = character())

  if (normalise) {
    email <- str_trim(email)
    display <- str_squish(display)
  }

  new_rcrd(list(email = email, display = display), class = "vctrs_address")
}

#' Email Address
#'
#' Create an \code{address} object which represents an email address.
#'
#' Implemented as an \href{https://cran.r-project.org/web/packages/vctrs/vignettes/s3-vector.html}{S3 vector class}.
#'
#' @param email Email address.
#' @param display Display name.
#' @param normalise Whether to normalise address to RFC-5321 requirements.
#'
#' @return An \code{address} object, representing an email address.
#' @export
#'
#' @examples
#' address("gerry@gmail.com")
#' address("gerry@gmail.com", "Gerald")
#' address(
#'   c("gerry@gmail.com", "alice@yahoo.com", "jim@aol.com", NA),
#'   c("Gerald", "Alice", NA, "Bob")
#' )
address <- function(email, display = NA, normalise = TRUE) {
  # Cast email and display to character and recycle to same length.
  #
  # This operator could be done more cleanly using %<-% from {zeallot} but
  # not doing that for the moment to avoid another dependency.
  #
  do.call(
    vec_recycle_common,
    vec_cast_common(email, display, .to = character())
  ) %>%
    setNames(c("email", "display")) %>%
    set("normalise", normalise) %>%
    do.call(new_address, .)
}

#' Encode email addresses in a common format
#'
#' @param x A vector of \code{address} objects.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A character vector.
#' @export
format.vctrs_address <- function(x, ...) {
  email <- field(x, "email")
  display <- field(x, "display")

  fmt <- ifelse(is.na(display), email, glue("{display} <{email}>"))
  fmt[is.na(email)] <- NA

  fmt
}

#' Display full type of vector
#'
#' @export
vec_ptype_full.vctrs_address <- function(x, ...) "address"

#' Display abbreviated type of vector
#'
#' @export
vec_ptype_abbr.vctrs_address <- function(x, ...) "addr"

#' Convert address object to character
#'
#' @param x  A vector of \code{address} objects.
#' @param ...
#'
#' @return A character vector.
#' @export
as.character.vctrs_address <- function(x, ...) {
  format(x, ...)
}

Ops.vctrs_address <- function(lhs, rhs)
{
  op = .Generic[[1]]
  switch(op,
         `==` = {
           compare(raw(lhs), raw(rhs)) & compare(display(lhs), display(rhs))
         },
         stop("Undefined operation.", call. = FALSE)
  )
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
#' as.address("Gerald <gerry@gmail.com>")
#' as.address(c("Gerald <gerry@gmail.com>", "alice@yahoo.com", "jim@aol.com"))
as.address <- function(address) {
  display <- ifelse(
    str_detect(address, "[<>]"),
    str_extract(address, ".*<") %>% str_remove("<"),
    NA
  )
  email <- ifelse(
    str_detect(address, "[<>]"),
    str_extract(address, "<.*>") %>% str_remove_all("[<>]"),
    address
  )

  address(email, display)
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
print.vctrs_address <- function(x, ...) {
  print(format(x))
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
  field(address, "email")
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
  field(address, "display")
}
