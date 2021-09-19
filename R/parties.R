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
  address <- NULL # nocov

  # TODO: This might be useful for sorting out data type in address column:
  #
  # https://vctrs.r-lib.org/articles/pillar.html

  map_dfr(c("From", "To", "Cc", "Bcc"), function(type) {
    tibble(
      type,
      address = msg$header[type]
    )
  }) %>%
    unnest(cols = c(address)) %>%
    mutate(
      display = display(address),
      raw = raw(address),
      local = local(address),
      domain = domain(address)
    )
}

#' Add address fields to message
#'
#' @name addresses
#'
#' @param msg A message object.
#' @param addr Single address.
#' @return A message object.
NULL

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
    msg$header$From
  } else {
    msg$header$From <- as.address(addr)
    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}

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
to <- function(msg, ...) {
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$To
  } else {
    msg$header$To <- as.address(arguments)
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
cc <- function(msg, ...) {
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$Cc
  } else {
    msg$header$Cc <- as.address(arguments)
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
bcc <- function(msg, ...) {
  arguments <- c(...)
  if (is.null(arguments)) {
    msg$header$Bcc
  } else {
    msg$header$Bcc <- as.address(arguments)
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
    msg$header$Reply_To
  } else {
    msg$header$Reply_To <- as.address(addr)
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
    msg$header$Sender
  } else {
    msg$header$Sender <- as.address(addr)
    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}
