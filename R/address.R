sanitise <- function(email, strip_comments = TRUE) {
  email %>%
    str_trim() %>%
    str_replace("[:blank:]+@[:blank:]+", "@") %>% {
      if (strip_comments) {
        str_remove_all(., "\\([^)]*\\)")
      } else {
        . # nocov
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
#' @param error Whether to create an error if not compliant.
#'
#' @return A Boolean.
#' @export
#'
#' @examples
#' compliant("alice@example.com")
#' compliant("alice?example.com")
compliant <- function(addr, error = FALSE) {
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

  okay <- email & local
  if (error && !okay) {
    stop(paste0("Address '", addr, "' invalid."), call. = FALSE)
  } else {
    okay
  }
}

#' Email Address
#'
#' Create an \code{address} object which represents an email address.
#'
#' @param email Email address.
#' @param display Display name.
#' @param local Local part of email address.
#' @param domain Domain part of email address.
#' @param normalise Whether to try to normalise address to RFC-5321 requirements.
#'
#' @return An \code{address} object, representing an email address.
#' @export
#'
#' @examples
#' address("gerry@gmail.com")
#' address("gerry@gmail.com", "Gerald")
#' address("gerry@gmail.com", "Durrell, Gerald")
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

  email <- as.character(email)
  display <- as.character(display)
  local <- as.character(local)
  domain <- as.character(domain)

  args <- possibly(data.frame, NULL)(
    email,
    display,
    local,
    domain
  )
  if (is.null(args)) stop("Unable to recycle arguments in a meaningful way.")

  args <- args %>% mutate(
    email = ifelse(is.na(email), paste0(local, "@", domain), email)
  )

  email = ifelse(is.na(email), paste0(local, "@", domain), email)

  if (normalise) {
    email = sanitise(email)
    display = str_squish(display)
  }

  structure(
    list(
      email = email,
      display = display
    ),
    class = "address"
  )
}

#' Length of address object
#'
#' @param x An \code{address} object.
#'
#' @return A character vector.
#' @export
length.address <- function(x) {
  length(x$email)
}

#' Encode email addresses in a common format
#'
#' @param x An \code{address} object.
#' @param quote Whether to quote display name (only relevant if display name is
#'   given in "Last, First" format).
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A character vector.
#' @export
format.address <- function(x, quote = TRUE, ...) {
  email <- x$email
  display <- x$display

  # If the display name includes a comma, then quote it.
  #
  display <- ifelse(
    !is.na(display) & str_detect(display, ",") & quote,
    paste0('"', display, '"'),
    display
  )

  fmt <- ifelse(is.na(display), email, glue("{display} <{email}>"))
  fmt[is.na(email)] <- NA

  fmt
}

#' Convert address object to character
#'
#' If display name is specifed as "Last, First" then the display name will be
#' quoted.
#'
#' @param x  An \code{address} object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A character vector.
#' @export
as.character.address <- function(x, ...) {
  format(x, TRUE, ...)
}

#' Compare address objects
#'
#' @noRd
#'
#' @param e1 LHS operand.
#' @param e2 RHS operand.
#'
#' @return A Boolean, \code{TRUE} if the \code{e1} address is the same as the
#'   \code{e2} address (ignores the display name).
#' @export
Ops.address <- function(e1, e2)
{
  if (!("address" %in% class(e1))) e1 <- as.address(e1)
  if (!("address" %in% class(e2))) e2 <- as.address(e2)

  get(.Generic)(raw(e1), raw(e2))
}

#' Create an address object
#'
#' This is capable of handling more than one address at a time.
#'
#' @param addr An email address.
#' @param split Pattern for splitting multiple addresses.
#'
#' @return A list of \code{address} objects.
#' @export
#'
#' @examples
#' as.address("gerry@gmail.com")
#' as.address("Gerald <gerry@gmail.com>")
#' as.address(c("Gerald <gerry@gmail.com>", "alice@yahoo.com", "jim@aol.com"))
#' as.address("Gerald <gerry@gmail.com>, alice@yahoo.com, jim@aol.com")
#' as.address("Durrell, Gerald <gerry@gmail.com>", FALSE)
as.address <- function(addr, split = ", *") {
  if ("address" %in% class(addr)) {
    addr
  } else {
    # Check if multiple addresses.
    #
    if (is.character(split)) {
      addr <- str_split(addr, split) %>% unlist()
    }
    #
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
#' If display name is specifed as "Last, First" then the display name will be
#' quoted.
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
  print(format(x, FALSE))
}

#' Concatenate address objects
#'
#' @param ... Address objects to be concatenated.
#'
#' @return An \code{address} object.
#' @export
#'
#' @examples
#' gerry <- as.address("Gerald <gerry@gmail.com>")
#' alice <- address("alice@yahoo.com")
#' jim <- address("jim@aol.com", "Jim")
#' c(gerry, alice)
#' c(gerry, c(alice, jim))
c.address <- function(...) {
  addr <- list(...)
  email <- map(addr, ~ .$email) %>% do.call(c, .)
  display <- map(addr, ~ .$display) %>% do.call(c, .)
  structure(
    list(
      email = email,
      display = display
    ),
    class = "address"
  )
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
  addr$email
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
  addr$display
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
  as.address(addr) %>%
    raw() %>%
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
  as.address(addr) %>%
    raw() %>%
    str_remove("^.*@")
}

#' Split a compound address object
#'
#' @param addr An \code{address} object.
#'
#' @return A list of \code{address} objects, each of which contains only a single
#'   address.
#' @export
#'
#' @examples
#' cleave(as.address(c("foo@yahoo.com", "bar@yahoo.com")))
cleave <- function(addr) {
  addr <- as.address(addr)

  addr %>%
    unclass() %>%
    data.frame() %>%
    pmap(function(email, display) address(email, display))
}
