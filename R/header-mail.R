#' Add address fields to message
#'
#' @name addresses
#'
#' @param msg A message object.
#' @param addr Single address.
#' @param append Whether to append or replace addresses.
#' @return A message object.
NULL

#' Function factory for recipient functions
#'
#' Header records consist of key/value pairs.
#'
#' Coverage tests don't work as expected with factory functions.
#'
#' @param key Header key.
#' @return A header function.
#' @noRd
recipient <- function(key, single = FALSE) {
  force(key) # nocov start
  force(single)

  if (single) {
    function(msg, addr = NULL) {
      if (is.null(addr)) {
        header_get(msg, key)
      } else {
        addr <- as.address(addr)
        if (length(addr) > 1) stop("Only one address allowed.")
        msg <- header_set(msg, key, addr, append = FALSE)
        if (get_option_invisible()) invisible(msg) else msg
      }
    }
  } else {
    function(msg, ..., append = TRUE) {
      addr <- list(...)
      if (length(addr)) {
        addr <- map(addr, as.address) %>% do.call(c, .)
        msg <- header_set(msg, key, addr, append = append, sep = ",")
        if (get_option_invisible()) invisible(msg) else msg
      } else {
        header_get(msg, key)
      }
    }
  } # nocov end
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
to <- recipient("To")

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
cc <- recipient("Cc")

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
bcc <- recipient("Bcc")

#' @rdname addresses
#'
#' @export
#' @examples
#' msg <- envelope()
#'
#' # Populating the From field.
#' msg %>% from("craig@gmail.com")
#'
from <- recipient("From", TRUE)

#' @rdname addresses
#'
#' @export
#' @examples
#' # Populating the Reply-To field.
#' msg <- envelope()
#' msg %>% reply("gerry@gmail.com")
#'
reply <- recipient("Reply-To", TRUE)

#' @rdname addresses
#'
#' @export
#' @examples
#' # Populating the Return-Path field.
#' msg <- envelope()
#' msg %>% return_path("bounced-mail@devnull.org")
#'
return_path <- recipient("Return-Path", TRUE)

#' @rdname addresses
#'
#' @export
#' @examples
#' # Populating the Sender field.
#' msg <- envelope()
#' msg %>% sender("on_behalf_of@gmail.com")
sender <- recipient("Sender", TRUE)

#' Add or query subject of message.
#'
#' @details
#' The `prefix` and `suffix` can be used to add extra
#' \href{https://en.wikipedia.org/wiki/List_of_email_subject_abbreviations}{subject
#' abbreviations}.
#'
#' @inheritParams text
#' @param msg A message object.
#' @param subject A subject for the message.
#' @param prefix A subject prefix.
#' @param suffix A subject suffix.
#'
#' @return A message object or the subject of the message object (if \code{subject} is \code{NULL}).
#' @seealso \code{\link{to}}, \code{\link{from}}, \code{\link{cc}}, \code{\link{bcc}} and \code{\link{reply}}
#' @export
#' @examples
#' # Create a message and set the subject
#' msg <- envelope() %>% subject("Updated report")
#'
#' # Retrieve the subject for a message
#' subject(msg)
subject <- function(msg,
                    subject = NULL,
                    prefix = NA,
                    suffix = NA,
                    interpolate = TRUE,
                    .open = "{{",
                    .close = "}}",
                    .envir = NULL) {
  if (is.null(subject)) {
    header_get(msg, "Subject")
  } else {
    if (is.null(.envir)) {
      .envir <- parent.frame()
    } else {
      .envir <- list2env(.envir)
    }

    if (!is.na(prefix)) subject <- paste(prefix, subject)
    if (!is.na(suffix)) subject <- paste(subject, suffix)

    if (interpolate) subject <- glue(subject, .open = .open, .close = .close, .envir = .envir)

    msg <- header_set(msg, "Subject", encodable(subject), append = FALSE)
    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}

#' Set message ID.
#'
#' @param msg A message object.
#' @param id An ID for the message.
#'
#' @return A message object.
#' @export
#' @examples
#' # Create a message and set the ID
#' msg <- envelope() %>% id("1234567890.123456@example.com")
#' # Create a message with specified ID
#' msg <- envelope(id = "1234567890.123456@example.com")
id <- function(msg, id) {
  # Remove inclosing <> to standardise argument.
  id <- gsub("^<|>$", "", id)
  # Validate id.
  if (!grepl("^[^@]+@[^@]+\\.[^@]+$", id)) {
    stop("Invalid message ID!")
  }

  msg <- header_set(msg, "Message-ID", paste0("<", id, ">"), append = FALSE)
  if (get_option_invisible()) invisible(msg) else msg # nocov
}

#' Add In-Reply-To and References header fields
#'
#' @name response
#'
#' @inheritParams addresses
#' @param msgid A message ID. This would be the contents of the \code{Message-ID}
#'   field from another message.
#' @param subject_prefix Prefix to add to subject. If specified will be prepended
#'   onto the \code{Subject} field. Set to `NULL` if not required.
#' @return A message object.
NULL

#' @rdname response
#'
#' @export
#' @examples
#' envelope() %>% inreplyto("<6163c08e.1c69fb81.65b78.183c@mx.google.com>")
#' # Now for German.
#' envelope() %>%
#'   inreplyto("6163c08e.1c69fb81.65b78.183c@mx.google.com", "AW: ")
inreplyto <- function(msg, msgid, subject_prefix = "Re: ") {
  msg <- msg %>%
    header_set("In-Reply-To", wrap_angle_brackets(msgid), append = FALSE) %>%
    subject(paste0(subject_prefix, subject(msg)))
  if (get_option_invisible()) invisible(msg) else msg # nocov
}

#' @rdname response
#'
#' @export
#' @examples
#' # And also for Danish, Norwegian and Swedish (but not Finnish!).
#' envelope() %>%
#'   references("6163c08e.1c69fb81.65b78.183c@mx.google.com", "SV: ")
#' # Can reference multiple messages.
#' envelope() %>%
#'   references(c(
#'     "6163c08e.1c69fb81.65b78.183c@mx.google.com",
#'     "e8e338ff-a05c-4c0f-99f2-0dc8fb72682f@mail.gmail.com"
#'   ))
references <- function(msg, msgid, subject_prefix = "Re: ") {
  msg <- msg %>%
    header_set(
      "References",
      paste(wrap_angle_brackets(msgid), collapse = " "),
      append = FALSE
    ) %>%
    subject(paste0(subject_prefix, subject(msg)))
  if (get_option_invisible()) invisible(msg) else msg # nocov
}

#' Add or query comments of message.
#'
#' @param msg A message object.
#' @param comments Comments for the message.
#'
#' @return A message object or the comments of the message object (if \code{comments} is \code{NULL}).
#' @seealso \code{\link{subject}}
#' @export
#' @examples
#' # Create a message and set the comments.
#' msg <- envelope() %>% comments("This is a comment")
#'
#' # Retrieve the comments for a message.
#' comments(msg)
comments <- function(msg, comments = NULL) {
  if (is.null(comments)) {
    header_get(msg, "Comments")
  } else {
    msg <- header_set(msg, "Comments", comments, append = FALSE)
    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}

#' Add or query keywords of message.
#'
#' @param msg A message object.
#' @param ... Keywords.
#' @param append Whether to append or replace keywords.
#'
#' @return A message object or the comments of the message object (if \code{comments} is \code{NULL}).
#' @seealso \code{\link{to}}, \code{\link{from}}, \code{\link{cc}}, \code{\link{bcc}} and \code{\link{reply}}
#' @export
#' @examples
#' # Create a message and set the keywords.
#' envelope() %>% keywords("newsletter, marketing")
#' envelope() %>% keywords("newsletter", "marketing")
#' envelope() %>% keywords(c("newsletter", "marketing"))
#'
#' # Retrieve the keywords for a message.
#' msg <- envelope() %>% keywords("newsletter, marketing")
#' keywords(msg)
keywords <- function(msg, ..., append = FALSE) {
  arguments <- c(...)
  if (is.null(arguments)) {
    header_get(msg, "Keywords")
  } else {
    msg <- header_set(msg, "Keywords", arguments, append = append, sep = ", ")
    if (get_option_invisible()) invisible(msg) else msg # nocov
  }
}
