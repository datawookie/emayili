REGEX_BARE_LINEFEED = "(?<!\r)\n"

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
#' @return A character vector
read_text <- function(path, encoding = "UTF-8", collapse = "\n") {
  map(path, function(p) {
    if (!file.exists(p)) stop("Unable to find file: ", p, ".", call. = FALSE)
    readLines(p, encoding = encoding, warn = FALSE)
  }) %>%
    unlist()%>%
    str_c(collapse = collapse)
}

#' Read entire binary file into character vector
#'
#' @noRd
#'
#' @param path Relative or absolute file path
#'
#' @return A character vector
read_bin <- function(path) {
  readBin(path, "raw",  file.info(path)$size)
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

hexkey <- function(object = runif(1), algorithm="crc32") {
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

#' Remove comments from CSS
#'
#' Will handle comments with the following form:
#'
#' - /* ... */
#' - /*! ... */
#'
#' @noRd
css_remove_comment <- function(css) {
  str_replace_all(css, "/\\*!?(\\*(?!/)|[^\\*])*\\*/", "")
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
  if (!grepl("^<", x)) x <- paste0("<", x)
  if (!grepl(">$", x)) x <- paste0(x, ">")
  x
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
    protocol= ""
  }

  sprintf("%s%s:%d/%s", protocol, host, port, helo)
}

stop <- function(..., call. = FALSE, domain = NULL) {
  txt <- glue::glue(...)
  base::stop(txt, call. = call., domain = domain)
}

#' Encode parameter values for MIME headers
#'
#' Formats UTF-8 parameter values for the use in MIME headers. Non-ASCII
#' characters, control characters, and some special characters have to be
#' specially encoded according to the
#' \href{https://tools.ietf.org/html/rfc2231}{RFC2231} specification.
#'
#' @noRd
#'
#' @param x \code{character(1)}. UTF-8 string to format.
#' @return \code{character(1)}. String to put right after the parameter name in
#'   a MIME header. The equal sign and possible quotation marks are included.
#'   For example \code{"I'm not quoted.csv"} turns to
#'   \code{"=\\"I'm not quoted.csv\\""} while \code{"\\"I'm quoted\\".csv"}
#'   results in \code{"*=utf-8''\%22I'm\%20quoted\%22.csv"}.
#' @examples
#' emayili:::parameter_value_encode("I'm not quoted.csv")
#' emayili:::parameter_value_encode("\"I'm quoted\".csv")
parameter_value_encode <- function(x){
  raw <- charToRaw(x)
  ascii_to_encode <- as.raw(c(0x00:0x1F, 0x22, 0x5C, 0x7F))
  # Control characters, "\"" and sometimes "\\" must also be encoded.
  ascii <- raw <= as.raw(0x7F) & !(raw %in% ascii_to_encode)
  if (all(ascii)) {
    return(paste0("=\"", x, "\""))
  } else {
    syntax_ascii <- as.raw(c(0x20, 0x25, 0x3B))
    # In case of encoding, " ", "%" and ";" must be encoded as well.
    ascii <- ascii & !(raw %in% syntax_ascii)
    encoded <- character(length(raw))
    encoded[ascii] <- rawToChar(raw[ascii], multiple = TRUE)
    encoded[!ascii] <- paste0("%", toupper(as.character(raw[!ascii])))
    return(paste0("*=utf-8''", paste(encoded, collapse = "")))
  }
}
