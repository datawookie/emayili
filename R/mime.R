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

#' Format the header of a MIME object.
#'
#' The header must conform to the SMTP Protocol (https://tools.ietf.org/html/rfc821). Specifically, lines should be
#' terminated by CRLF (not just LF).
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

  headers <-  c(
    'Content-Type: {content_type}',
    ifelse(!is.null(charset), '; charset={charset}', ''),
    ifelse(!is.null(name), '; name="{name}"', ''),
    '\r\nContent-Disposition: {content_disposition}',
    ifelse(!is.null(filename), '; filename="{filename}"', ''),
    ifelse(!is.na(cid), '\r\nContent-Id: <{cid}>\r\nX-Attachment-Id: {cid}', ''),
    '\r\nContent-Transfer-Encoding: {encoding}'
  ) %>%
    paste(collapse = "") %>%
    glue()

  paste(headers, msg$body, sep = "\n\n")
}
