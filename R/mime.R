ERROR_NOT_MIME_OBJECT <- "Child is not a MIME object."

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
#' @section MIME Multipart Types:
#'
#' There are a number of options for multipart messages:
#'
#' \itemize{
#'  \item{\code{multipart/mixed} — }{Used for sending content with multiple independent parts either inline or as attachments. Each part can have different \code{Content-Type}.}
#'  \item{\code{multipart/alternative} — }{Used when each part of the message is an "alternative" version of the same content. The order of the parts is important: preferred and/or more complex formats should be found towards the end.
#'
#'  \emph{Example:} A message with both plain text and HTML versions.}
#'  \item{\code{multipart/digest} — }{Used to send multiple plain text messages.}
#'  \item{\code{multipart/related} — }{Used when each part of the the message represents a component of the complete message.
#'
#'  \emph{Example:} A web page with images.}
#'  \item{\code{multipart/signed} — }{Used when a message has a digital signature attached.}
#'  \item{\code{multipart/encrypted} — }{Used for a message with encrypted content.}
#' }
#'
#' A nice illustration of how some of these relate can be found at \url{https://stackoverflow.com/a/40420648/633961}.
#'
#' @noRd
#'
#' @param content Content.
#' @param disposition Should the content be displayed inline or as an attachment? Valid options are \code{"inline"} and \code{"attachment"}. If set to \code{NA} then will guess appropriate value.
#' @param charset How to interpret the characters in the content. Most often either UTF-8 or ISO-8859-1.
#' @param encoding How to encode binary data to ASCII.
#' @param boundary Boundary string.
#' @param type The MIME type of the content.
#' @param children
#'
#' @return A MIME object.
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
    if (!is.mime(child)) stop(ERROR_NOT_MIME_OBJECT)
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
#' @noRd
#'
#' @inheritParams MIME
#' @param squish Whether to remove whitespace outside of tags.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A \code{text_html} object derived from \code{MIME}.
text_html <- function(
  content,
  disposition = "inline",
  charset = "utf-8",
  encoding = NA,
  squish = FALSE,
  css = NA,
  ...
) {
  # Clean up content.
  #
  if (squish) {
    content <- html_squish(content)
  }
  content <- read_html(content)

  if (length(css) && !all(is.na(css) | css == "")) {
    css <- css %>%
      unlist() %>%
      str_c(collapse = "\n") %>%
      css_remove_comment() %>%
      str_squish()

    # Add <head> (can be missing if rendering Plain Markdown).
    if (is.na(xml_find_first(content, "//head"))) {
      xml_add_sibling(
        xml_find_first(content, "//body"),
        "head",
        .where = "before"
      )
    }

    # Write consolidated CSS to single <style> tag.
    if (nchar(css)) {
      xml_add_child(
        xml_find_first(content, "//head"),
        "style",
        css,
        type = "text/css"
      )
    }
  }

  # Replace bare line-feeds.
  content <- drape_linefeed(as.character(content))

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

#' Other (not text or HTML) MIME content
#'
#' @noRd
#'
#' @inheritParams MIME
#'
#' @param filename Path to a file.
#' @param name Name used when downloading file.
#' @param cid An optional Content-Id.
#' @param ... Further arguments passed to or from other methods.
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

# APPEND & PREPEND ------------------------------------------------------------

#' Add children to MIME element
#'
#' @name add_children
#'
#' @param x MIME element
#' @param child Child MIME element
#' @return A MIME element.
#'
#' @noRd
NULL

#' @rdname add_children
#' @noRd
#'
append <- function(x, child) {
  UseMethod("append", x)
}
append.MIME <- function(x, child) {
  if (!is.mime(child)) stop(ERROR_NOT_MIME_OBJECT)
  x$children <- c(x$children, list(child))
  x
}

#' @rdname add_children
#' @noRd
#'
prepend <- function(x, child) {
  UseMethod("prepend", x)
}
prepend.MIME <- function(x, child) {
  if (!is.mime(child)) stop(ERROR_NOT_MIME_OBJECT)
  x$children <- c(list(child), x$children)
  x
}

# CHARACTER -------------------------------------------------------------------

#' Convert MIME object to character vector
#'
#' @param x MIME object
#' @param ... Further arguments passed to or from other methods.
as.character.MIME <- function(x, ...) {
  children <- sapply(x$children, function(child) {
    paste(paste0("--", x$boundary), as.character.MIME(child), sep = "\r\n")
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

  paste(body, collapse = "\r\n")
}

# PRINT -----------------------------------------------------------------------

#' Print a MIME object
#'
#' @noRd
#'
#' @param x MIME object
#' @param ... Further arguments passed to or from other methods.
print.MIME <- function(x, ...) {
  cat(as.character(x))
}
