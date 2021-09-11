# CONSTRUCTOR -----------------------------------------------------------------

is.mime <- function(x) {
  "MIME" %in% class(x)
}

#' Create a MIME object
#'
#' ```
#' MIME
#'  ├── multipart/mixed
#'  ├── multipart/related
#'  ├── text/plain
#'  ├── text/html
#'  └── other
#' ```
#'
#' @param content Content.
#' @param disposition Should the content be displayed inline or as an attachment?
#' @param charset How to interpret the characters in the content. Most often either UTF-8 or ISO-8859-1.
#' @param encoding How to encode binary data to ASCII.
#' @param boundary Boundary string.
#' @param type The MIME type of the content.
#' @param children
#'
#' @return A MIME object.
#' @export
#'
#' @examples
MIME <- function(
  content = NULL,
  disposition = NA,
  charset = NA,
  encoding = NA,
  boundary = emayili:::hexkey(),
  type = NA,
  children = list()
) {
  # If just a single child, convert to list.
  if (!all(class(children) == c("list"))) children <- list(children)
  # Check that all children are MIME.
  for (child in children) {
    if (!is.mime(child)) stop("Child is not a MIME object.", call. = FALSE)
  }

  structure(
    list(
      content = content,
      disposition = disposition,
      charset = charset,
      encoding = encoding,
      boundary = boundary,
      children = children,
      type = type
    ),
    class = "MIME"
  )
}

multipart_related <- function(...) {
  structure(
    c(
      MIME(...),
      list()
    ),
    class = c("multipart_related", "MIME")
  )
}

multipart_mixed <- function(...) {
  structure(
    c(
      MIME(...),
      list()
    ),
    class = c("multipart_mixed", "MIME")
  )
}

text_plain <- function(
  content,
  disposition = "inline",
  charset = "utf-8",
  encoding = "7bit",
  ...
) {
  structure(
    c(
      MIME(content, disposition, charset, encoding, boundary = NA, ...),
      list()
    ),
    class = c("text_plain", "MIME")
  )
}

#' Create \code{text/html} MIME object
#'
#' @inheritParams MIME
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A \code{}text_html object derived from \code{MIME}.
#' @export
#'
#' @examples
text_html <- function(
  content,
  disposition = "inline",
  charset = "utf-8",
  encoding = NA,
  ...
) {
  # Clean up content.
  #
  content <- content %>%
    #
    # Remove whitespace outside of tags.
    #
    str_replace_all("(^|(?<=>))[:space:]+($|(?=<))", "")

  structure(
    c(
      MIME(
        content,
        disposition,
        charset,
        encoding,
        boundary = NA,
        ...
      ),
      list()
    ),
    class = c("text_html", "MIME")
  )
}

#' Title
#'
#' @param filename
#' @param name
#' @param type
#' @param cid An optional Content-Id.
#' @param disposition
#' @param charset
#' @param encoding
#' @param ...
#'
#' @export
other <- function(
  filename,
  name = NA,
  type = NA,
  cid = NA,
  disposition = "attachment",
  encoding = "base64",
  ...
) {
  charset <- NA
  basename <- basename(filename)
  name <- ifelse(is.na(name), basename, name)

  if (!is.na(type)) {
    # Could use mime::mimemap to map from specific extensions to MIME types,
    # so that the following would give the same result:
    #
    # attachment(..., type = "pdf")
    # attachment(..., type = "application/pdf")
  } else {
    type <- guess_type(filename, empty = "application/octet-stream")
  }
  type <- glue('{type}; name="{name}"')

  if (is.na(disposition)) {
    disposition <- ifelse(
      grepl("text", type),
      # If it's text...
      "inline",
      # ... otherwise:
      "attachment"
    )
  }
  disposition <- glue('{disposition}; filename="{basename}"')

  content <- base64encode(
    read_bin(filename),
    76L,
    "\r\n"
  )

  structure(
    c(
      MIME(content, disposition, charset, encoding, boundary = NA, type = type, ...),
      list(
        cid = ifelse(is.na(cid), hexkey(), cid)
      )
    ),
    class = c("attachment", "MIME")
  )
}

# APPEND ----------------------------------------------------------------------

#' Title
#'
#' @param x
#' @param child
append <- function(x, child) {
  UseMethod("append", x)
}

#' Title
#'
#' @param x
#' @param child
append.MIME <- function(x, child) {
  if (!is.mime(child)) stop("Child is not a MIME object.", call. = FALSE)
  x$children <- c(x$children, list(child))
  x
}

#' Title
#'
#' @param x
#' @param child
append.multipart_related <- function(x, child) NextMethod(x, child)

#' Title
#'
#' @param x
#' @param child
append.multipart_mixed <- function(x, child) NextMethod(x, child)

# CHARACTER -------------------------------------------------------------------

#' Title
#'
#' @param x
#' @param ...
#'
#' @export
as.character.MIME <- function(x, ...) {
  children <- sapply(x$children, function(child) {
    paste(paste0("--", x$boundary), as.character.MIME(child), sep = "\n")
  })
  type <- ifelse(!is.na(x$type), x$type, sub("_", "/", class(x)[1]))
  #
  body <- c(
    # Head.
    paste(
      c(
        glue('Content-Type:              {type}'),
        if (!is.na(x$charset)) glue('charset={x$charset}') else NULL,
        if (!is.na(x$boundary)) glue('boundary="{x$boundary}"') else NULL
      ),
      collapse = "; "
    ),
    if (!is.na(x$disposition)) {
      glue('Content-Disposition:       {x$disposition}')
    } else NULL,
    if (!is.na(x$encoding)) {
      glue('Content-Transfer-Encoding: {x$encoding}')
    } else NULL,
    if (!is.null(x$cid)) {
      paste(
        glue('X-Attachment-Id:           {x$cid}'),
        glue('Content-ID:                <{x$cid}>'),
        sep = "\n"
      )
    } else NULL,
    "",
    # Content (if any).
    x$content,
    # Children (if any).
    if(length(children)) children else NULL,
    # Foot.
    if (!is.na(x$boundary)) glue('--{x$boundary}--') else NULL
  )

  paste(body, collapse = "\n")
}

# PRINT -----------------------------------------------------------------------

#' Title
#'
#' @param x
#'
#' @export
print.MIME <- function(x) {
  cat(as.character(x))
}

#' #' Title
#' #'
#' #' @param x
#' #'
#' #' @export
#' print.multipart_related <- function(x) {
#'   NextMethod()
#' }

#' #' Title
#' #'
#' #' @param x
#' #'
#' #' @export
#' print.multipart_mixed <- function(x) {
#'   NextMethod()
#' }
