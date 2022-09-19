#' Normalise email address
#'
#' Ensure that email address is in a standard format.
#'
#' Performs the following transformations:
#'
#' - lowercase the domain part
#' - replace some Unicode characters with compatible equivalents. See
#'   [Unicode equivalence](https://en.wikipedia.org/wiki/Unicode_equivalence).
#'
#' @param email An email address.
#'
#' @return An email address.
#' @export
#'
#' @examples
#' normalise("bob@GMAIL.COM")
normalise <- function(email) {
  email <- email %>%
    str_trim() %>%
    str_replace("[:blank:]+@[:blank:]+", "@")

  # Strip comments.
  email <- str_remove_all(email, "\\([^)]*\\)")

  # Unicode NFC normalisation.
  email <- stri_trans_nfkc(email)

  # Domain part to lowercase.
  #
  # Need to use sub() here because {stringr} doesn't support perl option, which
  # is required to get "\\L" (lowercase) working.
  #
  email <- sub("(?<=@)(.*)", "\\L\\1", email, perl = TRUE)

  email
}

#' Validate email address
#'
#' @param addr An email address.
#' @param deliverability Whether to check for deliverability (valid domain).
#'
#' @return A `logical` indicating whether or not the address is valid.
#' @export
#'
#' @examples
#' # A valid address.
#' validate("cran-sysadmin@r-project.org")
#' # An invalid address.
#' validate("help@this-domain-does-not-exist.com")
validate <- function(addr, deliverability = TRUE) {
  if (!inherits(addr, "address")) addr <- as.address(addr)

  addr <- addr$email
  log_debug("Check address: {addr}")

  log_debug("- syntax")
  if (!compliant(addr)) {
    log_warn("Email address doesn't satisfy syntax requirements.")
    FALSE
  } else {
    if (deliverability) {
      domain <- domain(addr)
      log_debug("- domain:      {domain}")
      if (is.null(safely(nslookup)(domain)$result)) {
        log_warn("* Email address doesn't have a valid domain.")
        FALSE
      } else {
        TRUE
      }
    } else {
      TRUE
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
  email <- grepl("[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}", email, ignore.case = TRUE)

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
      !grepl("[[:blank:]]+.*", local) # No spaces in local.
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
#' @param validate Whether to validate the address.
#'
#' @return An \code{address} object, representing an email address.
#' @export
#'
#' @examples
#' address("gerry@gmail.com")
#' address("gerry@gmail.com", "Gerald")
#' address("gerry@gmail.com", "Gerald Durrell")
#' # Display name in "Last, First" format.
#' address("gerry@gmail.com", "Durrell, Gerald")
#' # Display name contains non-ASCII characters.
#' address("hans@gmail.com", "Hansjörg Müller")
address <- function(email = NA,
                    display = NA,
                    local = NA,
                    domain = NA,
                    normalise = TRUE,
                    validate = FALSE) {
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

  email <- ifelse(is.na(email), paste0(local, "@", domain), email)

  if (normalise) {
    email <- normalise(email)
    display <- str_squish(display)
  }

  if (validate) {
    if (!validate(email)) stop("Email address is not valid!")
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
#' @param encode Whether to encode headers.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A character vector.
#' @export
format.address <- function(x, quote = TRUE, encode = FALSE, ...) {
  email <- x$email
  display <- x$display

  display <- encodable(display) %>% as.character(encode = encode)

  fmt <- ifelse(is.na(display), email, glue("{display} <{email}>"))
  fmt[is.na(email)] <- NA

  fmt
}

#' Convert address object to character
#'
#' If display name is specified as "Last, First" then the display name will be
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
Ops.address <- function(e1, e2) {
  if (!("address" %in% class(e1))) e1 <- as.address(e1)
  if (!("address" %in% class(e2))) e2 <- as.address(e2)

  get(.Generic)(raw(e1), raw(e2))
}

#' Create an address object
#'
#' @param addr An email address.
#' @inheritParams address
#'
#' @return A list of \code{address} objects.
#' @export
#'
#' @examples
#' as.address("gerry@gmail.com")
#' as.address("Gerald <gerry@gmail.com>")
#' as.address(c("Gerald <gerry@gmail.com>", "alice@yahoo.com", "jim@aol.com"))
#' as.address("Gerald <gerry@gmail.com>, alice@yahoo.com, jim@aol.com")
#' as.address("Durrell, Gerald <gerry@gmail.com>")
as.address <- function(addr, validate = FALSE) {
  if ("address" %in% class(addr)) {
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

    address(email, display, validate = validate)
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
