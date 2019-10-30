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
  with(msg$header,
       c(
         'Content-Type: {content_type}',
         ifelse(exists("charset") && !is.null(charset), '; charset="{charset}"', ''),
         ifelse(exists("name"), '; name="{name}"', ''),
         '\nContent-Disposition: ',
         ifelse(grepl("text|image", content_type), 'inline', 'attachment'),
         ifelse(exists("filename"), '; filename="{filename}"', ''),
         ifelse(exists("cid"), '\nContent-Id: <{cid}>', ''),
         ifelse(exists("cid"), '\nX-Attachment-Id: {cid}', ''),
         '\nContent-Transfer-Encoding: {encoding}'
       ) %>% paste(collapse = "") %>% glue()
  ) %>%
    paste(msg$body, sep = "\n\n")
}
