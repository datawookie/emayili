#' Create a MIME (Multipurpose Internet Mail Extensions) object.
#'
#' @param content_type The MIME type of the content.
#' @param content_disposition Should the content be displayed inline or as an attachment?
#' @param encoding How to encode binary data to ASCII.
#' @param charset How to interpret the characters in the content. Most often either UTF-8 or ISO-8859-1.
#' @param cid An optional Content-Id.
#' @param ... Other arguments.
#' @return A MIME object.
mime <- function(content_type, content_disposition, charset, encoding, cid = NA, ...) {
  structure(
    list(
      header = list(
        content_type = content_type,
        content_disposition = content_disposition,
        encoding = encoding,
        format = format,
        charset = charset,
        cid = cid,
        ...
      ),
      body = NULL
    ),
    class="mime")
}

#' Encode parameter values for MIME headers
#'
#' Formats UTF-8 parameter values for the use in MIME headers. Non-ASCII
#' characters, control characters, and some special characters have to be
#' specially encoded according to the
#' \href{https://tools.ietf.org/html/rfc2231}{RFC2231} specification.
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
  x_raw <- charToRaw(x)
  ascii_to_encode <- as.raw(c(0x00:0x1F, 0x22, 0x5C, 0x7F))
  # testing showed that control characters, "\"" and sometimes "\\" also must be
  # encoded
  ascii <- x_raw <= as.raw(0x7F) & !(x_raw %in% ascii_to_encode)
  if (all(ascii)) {
    return(paste0("=\"", x, "\""))
  } else {
    syntax_ascii <- as.raw(c(0x20, 0x25, 0x3B))
    # in case of encoding, " ", "%" and ";" must be encoded as well
    ascii <- ascii & !(x_raw %in% syntax_ascii)
    encoded <- character(length(x_raw))
    encoded[ascii] <- rawToChar(x_raw[ascii], multiple = TRUE)
    encoded[!ascii] <- paste0("%", toupper(as.character(x_raw[!ascii])))
    return(paste0("*=utf-8''", paste(encoded, collapse = "")))
  }
}

#' Format the header of a MIME object.
#'
#' The header must conform to the SMTP Protocol (https://tools.ietf.org/html/rfc821). Specifically, lines should be
#' terminated by CRLF (not just LF). The \dQuote{name} and \dQuote{filename}
#' fields can contain any possible UTF-8 characters as they are
#' \link[=parameter_value_encode]{parameter value encoded}.
#'
#' @param msg A message object.
#' @return A formatted header string.
format.mime <- function(msg) {
  # Unpack relevant variables here so that we can contain the environment which
  # glue() searches.
  #
  content_type        <- msg$header$content_type
  charset             <- msg$header$charset
  name                <- msg$header$name
  content_disposition <- msg$header$content_disposition
  filename            <- msg$header$filename
  cid                 <- msg$header$cid
  encoding            <- msg$header$encoding

  if (!is.null(name))     name     <- parameter_value_encode(name)
  if (!is.null(filename)) filename <- parameter_value_encode(filename)

  headers <-  c(
    'Content-Type: {content_type}',
    ifelse(!is.null(charset), '; charset={charset}', ''),
    ifelse(!is.null(name), '; name{name}', ''),
    '\r\nContent-Disposition: {content_disposition}',
    ifelse(!is.null(filename), '; filename{filename}', ''),
    ifelse(!is.na(cid), '\r\nContent-Id: <{cid}>\r\nX-Attachment-Id: {cid}', ''),
    '\r\nContent-Transfer-Encoding: {encoding}'
  ) %>%
    paste(collapse = "") %>%
    glue()

  paste(headers, msg$body, sep = "\n\n")
}
