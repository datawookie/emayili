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

text_html <- function(
  content,
  disposition = "inline",
  charset = "utf-8",
  encoding = NA,
  # encoding = "quoted-printable",
  ...
) {
  structure(
    c(
      MIME(
        # qp_encode(content),
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
#' @param disposition
#' @param charset
#' @param encoding
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
      # ifelse(
      #   # If it's an image...
      #   grepl("image", type),
      #   ifelse(!is.na(cid), "inline", "attachment"),
      #   "attachment"
      # )
    )
  }
  disposition <- glue('{disposition}; filename="{basename}"')

  body <- read_bin(filename)
  #
  # mime <- mime(
  #   type,
  #   disposition,
  #   NULL,
  #   "base64",
  #   cid = as.character(cid),
  #   filename = ifelse(is.na(name), basename(path), name)
  # )
  #
  content <- base64encode(body, 76L, "\r\n")

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
#'
#' @return
#' @export
#'
#' @examples
append <- function(x, child) {
  UseMethod("append", x)
}

#' Title
#'
#' @param x
#' @param child
#'
#' @return
#' @export
#'
#' @examples
append.MIME <- function(x, child) {
  if (!is.mime(child)) stop("Child is not a MIME object.", call. = FALSE)
  x$children <- c(x$children, list(child))
  x
}

#' Title
#'
#' @param x
#' @param child
#'
#' @return
#' @export
#'
#' @examples
append.multipart_related <- function(x, child) NextMethod(x, child)

#' Title
#'
#' @param x
#' @param child
#'
#' @return
#' @export
#'
#' @examples
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
