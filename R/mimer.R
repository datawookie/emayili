# CONSTRUCTOR -----------------------------------------------------------------

is.mime <- function(x) {
  "MIME" %in% class(x)
}

# MIME
#  ├── multipart/mixed
#  ├── multipart/related
#  ├── text/plain
#  └── text/html
#
MIME <- function(
  content = NULL,
  disposition = NA,
  charset = NA,
  encoding = NA,
  boundary = emayili:::hexkey(),
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
      children = children
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

text_html <- function(
  content,
  disposition = "inline",
  charset = "utf-8",
  encoding = "quoted-printable",
  ...
) {
  structure(
    c(
      MIME(content, disposition, charset, encoding, boundary = NA, ...),
      list()
    ),
    class = c("text_html", "MIME")
  )
}

# APPEND ----------------------------------------------------------------------

append <- function(x, child) {
  UseMethod("append", x)
}
append.MIME <- function(x, child) {
  if (!is.mime(child)) stop("Child is not a MIME object.", call. = FALSE)
  x$children <- c(x$children, list(child))
  x
}
append.multipart_related <- function(x, child) NextMethod(x, child)
append.multipart_mixed <- function(x, child) NextMethod(x, child)

# CHARACTER -------------------------------------------------------------------

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as.character.MIME <- function(x, ...) {
  children <- sapply(x$children, function(child) {
    paste(paste0("--", x$boundary), as.character.MIME(child), sep = "\n")
  })
  type <- sub("_", "/", class(x)[1])
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

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as.character.multipart_related <- function(x, ...) {
  NextMethod()
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as.character.multipart_mixed <- function(x, ...) {
  NextMethod()
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as.character.text_plain <- function(x, ...) {
  paste(NextMethod(), x$content, sep = "\n")
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as.character.text_html <- function(x, ...) {
  paste(NextMethod(), x$content, sep = "\n")
}

# PRINT -----------------------------------------------------------------------

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.MIME <- function(x) {
  cat(as.character(x))
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.multipart_related <- function(x) {
  NextMethod()
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.multipart_mixed <- function(x) {
  NextMethod()
}
