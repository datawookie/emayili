#' Create a MIME (Multipurpose Internet Mail Extensions) object
#'
#' @export
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

# content-type: text/plain
# Content-Disposition: inline
# Content-Transfer-Encoding: quoted-printable

# content-type: text/html

header.mime <- function(msg) {
  header <- msg$header[lengths(msg$header) != 0]
  #
  c(
    "Content-Type: ",
    header$content_type
  )
}

#' @export
format <- function(x, ...) UseMethod("format")

#' @rdname mime
#' @export
format.mime <- function(msg) {
  with(msg$header,
       c(
         'Content-Type: {content_type}',
         ifelse(exists("charset") && !is.null(charset), '; charset="{charset}"', ''),
         ifelse(exists("name"), '; name="{name}"', ''),
         '\nContent-Disposition: ',
         ifelse(grepl("text", content_type), 'inline', 'attachment'),
         ifelse(exists("filename"), '; filename="{filename}"', ''),
         '\nContent-Transfer-Encoding: {encoding}'
       ) %>% paste(collapse = "") %>% glue()
  ) %>%
    paste(msg$body, sep = "\n\n")
}
