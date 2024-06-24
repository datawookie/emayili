REGEX_BARE_LINEFEED <- "(?<!\r)\n"

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

# For adding elements to a list using %>%.
#
set <- .Primitive("[[<-")

#' Compare vectors
#'
#' Returns \code{TRUE} wherever elements are the same (including \code{NA}),
#' and \code{FALSE} everywhere else.
#'
#' @param lhs LHS of operation.
#' @param rhs RHS of operation.
#'
#' @return A Boolean value.
compare <- function(lhs, rhs) {
  same <- (lhs == rhs) | (is.na(lhs) & is.na(rhs))
  same[is.na(same)] <- FALSE
  same
}

#' Read entire text file into character vector
#'
#' @noRd
#'
#' @inheritParams stringr::str_c
#' @param path Relative or absolute file path. This can also be a vector of
#'             paths, in which case their content is concatenated.
#'
#' @return A character vector. If `path` is `NULL` or an empty vector then return `NULL`.
read_text <- function(path, encoding = NULL, collapse = "\n") {
  if (is.null(path) || !length(path)) {
    NULL
  } else {
    map(path, function(p) {
      if (!file.exists(p)) stop("Unable to find file: ", p, ".", call. = FALSE)
      stri_read_lines(p, encoding = encoding)
    }) %>%
      unlist() %>%
      str_c(collapse = collapse)
  }
}

#' Read entire binary file into character vector
#'
#' @noRd
#'
#' @param path Relative or absolute file path
#'
#' @return A character vector
read_bin <- function(path) {
  readBin(path, "raw", file.info(path)$size)
}

#' Normalise file path
#'
#' @noRd
#'
#' @param path Relative or absolute file path
#'
#' @return An absolute file path (if the file exists) or \code{NA}.
normalise_filepath <- function(path) {
  possibly(normalizePath, NA_character_)(path, mustWork = TRUE)
}

#' Check if character vector is a file name or file path
#'
#' @noRd
#'
#' @param path A character vector (which might also be a file path)
#'
#' @return If it is a file path, then return \code{TRUE}, otherwise return \code{FALSE}.
is_filepath <- function(path) {
  !is.na(normalise_filepath(path))
}

hexkey <- function(object = runif(1), algorithm = "crc32") {
  digest(object, algorithm)
}

#' Drape line feeds
#'
#' Replace empty line-feeds, "\n", with carriage-return and line-feed, "\r\n".
#'
#' @noRd
drape_linefeed <- function(txt) {
  str_replace_all(txt, REGEX_BARE_LINEFEED, "\r\n")
}

#' Remove gratuitous whitespace from HTML
#'
#' @param html HTML content as character vector.
#'
#' @noRd
html_squish <- function(html) {
  html %>%
    # Remove duplicate \n (when surrounded by whitespace and tags).
    #
    # <div>\n\n\n<p>foo \n\n bar</p>\n\n</div> -> <div>\n<p>foo \n\n bar</p>\n</div>
    #
    str_replace_all("(?<=>) *(\n)+ *(?=<)", "\n") %>%
    # Remove just whitespace between tags.
    #
    # <div>  <p>foo    bar</p>  </div>         -> <div><p>foo    bar</p></div>
    #
    str_replace_all("(^|(?<=>)) +($|(?=<))", "")
}

mime_base64encode <- function(raw, linewidth = 76L) {
  if (!is.raw(raw)) {
    if (tryCatch(file.exists(raw), error = function(e) FALSE)) {
      log_debug("Assuming that input is a file.")
    } else {
      log_debug("Assuming that input is not a file.")
      if (is.character(raw)) {
        raw <- charToRaw(raw)
      } else {
        raw <- as.raw(raw)
      }
    }
  }

  base64encode(
    raw,
    linewidth,
    "\r\n"
  )
}

#' Generate MD5 checksum for Content-MD5 header field
#'
#' The MD5 checksum is a 128 bit digest. This corresponds to 16 bytes (octets)
#' of binary data. These 16 bytes are then Base64 encoded.
#'
#' @noRd
#'
#' @param object An arbitrary R object.
#'
#' @return Base64 encoded MD5 checksum.
#'
#' @examples
#' # Result should be "XrY7u+Ae7tCTyyK7j1rNww==".
#' md5("hello world")
md5 <- function(object) {
  digest(object, algo = "md5", serialize = FALSE, raw = TRUE) %>%
    mime_base64encode()
}

parse_datetime <- function(datetime, tz) {
  as.POSIXct(datetime, tz)
}

#' Format date
#'
#' Format like "Fri, 08 Oct 2021 22:06:39 -0700 (PDT)".
#'
#' @noRd
format_datetime <- function(datetime) {
  strftime(datetime, "%a, %d %b %Y %H:%M:%S %z (%Z)")
}

#' Enclose in angle brackets
#'
#' @noRd
wrap_angle_brackets <- function(x) {
  sub("(?<!>)$", ">", sub("^(?!<)", "<", x, perl = TRUE), perl = TRUE)
}

#' Test if list is nested or flat
#'
#' @noRd
#' @param x A list.
#' @return A Boolean.
is.nested <- function(x) {
  stopifnot(is.list(x))
  any(sapply(x, function(x) any(class(x) == "list")))
}

smtp_url <- function(host, port, protocol = NA, helo = NA) {
  helo <- ifelse(is.na(helo), "", helo)

  # Check if host includes protocol.
  if (!grepl("^smtps?://", host)) {
    if (is.na(protocol)) {
      protocol <- ifelse(port == 465, "smtps", "smtp")
    } else {
      protocol <- tolower(protocol)
      if (!grepl("^smtps?$", protocol)) {
        stop("Invalid protocol: only SMTP and SMTPS are allowed.")
      }
    }
    protocol <- paste0(protocol, "://")
  } else {
    protocol <- ""
  }

  sprintf("%s%s:%d/%s", protocol, host, port, helo)
}

# nolint start
stop <- function(..., call. = FALSE, domain = NULL) {
  txt <- glue::glue(...)
  base::stop(txt, call. = call., domain = domain)
}
# nolint end

file.ext <- function(path) {
  sub(".*\\.", "", basename(path))
}

#' Transform a (tag)list to a character string
#'
#' @param content Element to transform.
#'
#' @return If the content is a list, a tagList or a tag,
#'     a character vector. Otherwise, it will return the
#'     input unchanged.
#'
#' @noRd
#' @examples
#' list_to_char(list("<b>Hello</b>", "<p>World!</p>"))
#' library(htmltools)
#' list_to_char(tagList(h2("Hello"), p("World!")))
list_to_char <- function(content) {
  if (
    # We do the change if the element is a
    # tag or a tag.list
    inherits(content, "shiny.tag.list") ||
      inherits(content, "shiny.tag")
  ) {
    content <- as.character(content)
  }
  # Then if we have a list, we collapse it to
  # a character vector
  if (length(content) > 1) {
    content <- paste(content, collapse = "\n")
  }
  content
}

#' Create a message ID
#'
#' @param domain Originating domain.
#'
#' @return A message ID.
#' @export
#'
#' @examples
#' message_id()
#' message_id("example.com")
message_id <- function(domain = "mail.gmail.com") {
  unique_id <- paste(
    format(Sys.time(), "%Y%m%d%H%M%S"),
    paste0(
      sample(c(0:9, letters, LETTERS), 10, replace = TRUE),
      collapse = ""
    ),
    sep = "-"
  )

  paste0(unique_id, "@", domain)
}
