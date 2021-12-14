# Headers for the MIME protocol.

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

content_type <- function(type, protocol, charset, boundary, format = NA, name = NA) {
  header(
    "Content-Type",
    c(
      type,
      if (!is.na(protocol)) glue('protocol="{protocol}"') else NULL,
      if (!is.na(charset)) glue('charset={charset}') else NULL,
      if (!is.na(boundary)) glue('boundary="{boundary}"') else NULL,
      if (!is.na(format)) glue('format={format}') else NULL,
      if (!is.na(name)) glue('name{parameter_value_encode(name)}') else NULL
    ),
    sep = "; "
  )
}

content_disposition <- function(disposition = NA, filename = NA) {
  if (is.na(disposition)) {
    NULL
  } else {
    if (!is.na(filename)) {
      disposition <- paste(disposition,
                           glue('filename{parameter_value_encode(filename)}'),
                           sep = "; ")
    }
    header("Content-Disposition", disposition)
  }
}

content_transfer_encoding <- function(encoding = NA) {
  if (is.na(encoding)) {
    NULL
  } else {
    header("Content-Transfer-Encoding", encoding)
  }
}

content_description <- function(description = NA) {
  if (is.na(description)) {
    NULL
  } else {
    header("Content-Description", description)
  }
}

x_attachment_id <- function(cid = NULL) {
  if (is.null(cid)) {
    NULL
  } else {
    header("X-Attachment-Id", cid)
  }
}

content_id <- function(cid = NULL) {
  if (is.null(cid)) {
    NULL
  } else {
    header("Content-ID", paste0("<", cid, ">"))
  }
}

content_md5 <- function(content) {
  header("Content-MD5", md5(content))
}
