#' Create a MIME (Multipurpose Internet Mail Extensions) object
#'
#' @return A MIME object.
mime <- function(content_type, content_disposition, encoding, format, charset, cid = NA, ...) {
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

#' Format the header of a MIME object
#'
#' @return A formatted header string.
format.mime <- function(msg) {
  headers <-  with(msg$header, c(
    'Content-Type: {content_type}',
    ifelse(exists("charset") && !is.null(charset), '; charset="{charset}"', ''),
    ifelse(exists("name"), '; name="{name}"', ''),
    '\nContent-Disposition: {content_disposition}',
    ifelse(exists("filename"), '; filename="{filename}"', ''),
    ifelse(!is.na(cid), '\nContent-Id: <{cid}>\nX-Attachment-Id: {cid}', ''),
    '\nContent-Transfer-Encoding: {encoding}'
  ) %>%
    paste(collapse = "") %>%
    glue())

  paste(headers, msg$body, sep = "\n\n")
}
