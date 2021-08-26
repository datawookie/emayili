sanitise <- function(email, strip_comments = TRUE) {
  email %>%
    str_trim() %>%
    str_replace("[:blank:]+@[:blank:]+", "@") %>% {
      if (strip_comments) {
        str_remove_all(., "\\([^)]*\\)")
      } else {
        .
      }
    }
}

#' Tests whether an email address is syntactically correct
#'
#' Checks whether an email address conforms to the [syntax rules](https://en.wikipedia.org/wiki/Email_address#Syntax).
#'
#' An email address may take either of the following forms:
#'
#' - `local@domain` or
#' - `Display Name <local@domain>`.
#'
#' @param addr An email address.
#'
#' @return A Boolean.
#' @export
#'
#' @examples
#' compliant("alice@example.com")
#' compliant("alice?example.com")
compliant <- function(addr) {
  addr <- as.address(addr)

  email <- addr %>% raw()
  local <- addr %>% local()
  domain <- addr %>% domain()

  # Test on whole email address.
  #
  email <- grepl("[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}", email, ignore.case=TRUE)

  # Test on local part.
  #
  local <- TRUE &
    # No consecutive ".".
    !grepl("[.]{2,}.*", local) &
    # Only alphanumeric at beginning and end.
    !grepl("^[^[:alnum:]]", local) &
    !grepl("[^[:alnum:]]$", local) &
    # Not longer than 64 characters.
    !grepl(".{65,}", local) &
    ifelse(
      grepl('".*"', local),
      # Quoted.
      TRUE,
      # Unquoted.
      !grepl('[[:blank:]]+.*', local)             # No spaces in local.
    )

  # Test on domain.
  #
  domain <- TRUE &
    # No funky symbols.
    !grepl("[_]*", domain) &
    # Not longer than 255 characters.
    !grepl(".{256,}", domain)

  email & local
}

#' Helper function for creating address objects
#'
#' @inheritParams address
#'
#' @return An \code{address} object, representing an email address.
new_address <- function(
  email = character(),
  display = character(),
  local = character(),
  domain = character(),
  normalise = TRUE
) {
  vec_assert(email, ptype = character())
  vec_assert(display, ptype = character())
  vec_assert(local, ptype = character())
  vec_assert(domain, ptype = character())

  email <- ifelse(!is.na(email), email, paste0(local, "@", domain))

  if (normalise) {
    email <- sanitise(email)
    display <- str_squish(display)
  }

  new_rcrd(list(email = email, display = display), class = "vctrs_address")
}

#' Email Address
#'
#' Create an \code{address} object which represents an email address.
#'
#' Implemented as an \href{https://cran.r-project.org/package=vctrs/vignettes/s3-vector.html}{S3 vector class}.
#'
#' @param email Email address.
#' @param display Display name.
#' @param local Local part of email address.
#' @param domain Domain part of email address.
#' @param normalise Whether to normalise address to RFC-5321 requirements.
#'
#' @return An \code{address} object, representing an email address.
#' @export
#'
#' @examples
#' address("gerry@gmail.com")
#' address("gerry@gmail.com", "Gerald")
#' address(
#'   c("gerry@gmail.com", "alice@yahoo.com", "jim@aol.com"),
#'   c("Gerald", "Alice", NA)
#' )
address <- function(
  email = NA,
  display = NA,
  local = NA,
  domain = NA,
  normalise = TRUE
) {
  if (any(is.na(email) & is.na(local) & is.na(domain))) {
    stop("Either email or local and domain must be specified.", call. = FALSE)
  }
  if (any(!is.na(email) & (!is.na(local) | !is.na(domain)))) {
    stop("Cannot specify both email and local/domain.", call. = FALSE)
  }
  if (any(is.na(email) & is.na(local) & !is.na(domain))) {
    stop("Must specify local with domain.", call. = FALSE)
  }
  if (any(is.na(email) & !is.na(local) & is.na(domain))) {
    stop("Must specify domain with local.", call. = FALSE)
  }

  # Cast email and display to character and recycle to same length.
  #
  # This operator could be done more cleanly using %<-% from {zeallot} but
  # not doing that for the moment to avoid another dependency.
  #
  args <- do.call(
    vec_recycle_common,
    vec_cast_common(email, display, local, domain, .to = character())
  ) %>%
    setNames(c("email", "display", "local", "domain")) %>%
    set("normalise", normalise)

  do.call(new_address, args)
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
#' @inheritParams vctrs::vec_ptype_full
#' @export
vec_ptype_full.vctrs_address <- function(x, ...) "address"

#' Display abbreviated type of vector
#' @inheritParams vctrs::vec_ptype_abbr
#' @export
vec_ptype_abbr.vctrs_address <- function(x, ...) "addr"

#' Convert address object to character
#'
#' @param x  A vector of \code{address} objects.
#' @param ... Further arguments passed to or from other methods.
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
#' @param addr An email address.
#'
#' @return An \code{address} object.
#' @export
#'
#' @examples
#' as.address("gerry@gmail.com")
#' as.address("Gerald <gerry@gmail.com>")
#' as.address(c("Gerald <gerry@gmail.com>", "alice@yahoo.com", "jim@aol.com"))
as.address <- function(addr) {
  if ("vctrs_address" %in% class(addr)) {
    addr
  } else {
    display <- ifelse(
      str_detect(addr, "[<>]"),
      str_extract(addr, ".*<") %>% str_remove("<"),
      NA
    )
    email <- ifelse(
      str_detect(addr, "[<>]"),
      str_extract(addr, "<.*>") %>% str_remove_all("[<>]"),
      addr
    )

    address(email, display)
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
print.vctrs_address <- function(x, ...) {
  print(format(x))
}

#' Extract raw email address
#'
#' Strips the display name off an email address (if present).
#'
#' @param addr An \code{address} object.
#'
#' @return A raw email address.
#' @export
#'
#' @examples
#' gerry <- as.address("Gerald <gerry@gmail.com>")
#' raw(gerry)
raw <- function(addr) {
  field(addr, "email")
}

#' Extract display name
#'
#' Extracts the display name from an email address.
#'
#' @param addr An \code{address} object.
#'
#' @return The display name or \code{NA}.
#' @export
#'
#' @examples
#' gerry <- as.address("Gerald <gerry@gmail.com>")
#' display(gerry)
display <- function(addr) {
  field(addr, "display")
}

#' Extract local part of email address
#'
#' @param addr An \code{address} object.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' local("alice@example.com")
local <- function(addr) {
  addr <- as.address(addr)
  raw(addr) %>%
    str_remove("@.*$")
}

#' Extract domain of email address
#'
#' @param addr An \code{address} object.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' domain("alice@example.com")
domain <- function(addr) {
  addr <- as.address(addr)
  raw(addr) %>%
    str_remove("^.*@")
}
