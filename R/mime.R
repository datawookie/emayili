#' Create a MIME (Multipurpose Internet Mail Extensions) object
#'
#' @return A MIME object.
mime <- function(content_type, encoding, format, charset, ...) {
  structure(
    list(
      header = list(
        content_type = content_type,
        encoding = encoding,
        format = format,
        charset = charset,
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
    '\nContent-Disposition: ',
    if(grepl("text", content_type)){
      'inline'
      } else if(grepl("image", content_type)){
      if(exists("cid")) 'inline' else 'attachment'

    } else {'attachment'},

    ifelse(exists("filename"), '; filename="{filename}"', ''),
    ifelse(exists("cid"), '\nContent-Id: <{cid}>\nX-Attachment-Id: {cid}', ''),
    '\nContent-Transfer-Encoding: {encoding}'
  ) %>%
    paste(collapse = "") %>%
    glue())

  paste(headers, msg$body, sep = "\n\n")
}
