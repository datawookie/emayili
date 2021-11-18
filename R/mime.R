ERROR_NOT_MIME_OBJECT <- "Child is not a MIME object."

# CONSTRUCTOR -----------------------------------------------------------------

is.mime <- function(x) {
  "MIME" %in% class(x)
}

#' Parameters for MIME functions
#'
#' These are parameters which occur commonly across functions for components of
#' a MIME document.
#'
#' @name mime-parameters
#'
#' @param content A string of message content.
#' @param disposition Should the content be displayed inline or as an
#'   attachment? Valid options are \code{"inline"} and \code{"attachment"}. If
#'   set to \code{NA} then will guess appropriate value.
#' @param charset What character set is used. Most often either \code{"UTF-8"}
#'   or \code{"ISO-8859-1"}.
#' @param encoding How content is transformed to ASCII. Options are
#'   \code{"7bit"}, \code{"quoted-printable"} and \code{"base64"}. Use \code{NA}
#'   or \code{NULL} for no (or "identity") encoding.
#' @param language Langauge of content. If \code{FALSE} then will not include
#'   language field. If \code{TRUE} then will attempt to auto-detect language.
#'   Otherwise will use the specified language.
#' @param boundary Boundary string.
#' @param type The MIME type of the content.
#' @param children List of child MIME objects.
#' @param interpolate Whether or not to interpolate into input using \link[glue]{glue}.
#' @param .open The opening delimiter.
#' @param .close The closing delimiter.
#' @param .envir Environment used for \code{glue} interpolation. Defaults to \code{parent.frame()}.
NULL

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
#' @return A MIME object.
MIME <- function(
  content = NULL,
  disposition = NA,
  charset = NA,
  encoding = NA,
  language = NA,
  format = NA,
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
      language = language,
      format = format,
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
      MIME(content, disposition, charset, encoding, format = "flowed", boundary = NA, ...),
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
  disposition <- glue('{disposition}; filename="{name}"')

  structure(
    c(
      MIME(read_bin(filename), disposition, charset, encoding, boundary = NA, type = type, ...),
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

#' MIME type of message part
#'
#' @noRd
#' @param part Message part.
#'
#' @return Character vector.
type <- function(x) {
  ifelse(!is.na(x$type), x$type, sub("_", "/", class(x)[1]))
}

#' Convert MIME object to character vector
#'
#' @param x MIME object
#' @param ... Further arguments passed to or from other methods.
as.character.MIME <- function(x, ...) {
  if (is.null(x$encoding)) x$encoding <- NA

  if (!is.na(x$encoding) && !(x$encoding %in% LEVELS_ENCODING)) {
    stop("Invalid encoding. Options are: ", paste(LEVELS_ENCODING, collapse = ", "), ".")
  }

  children <- sapply(x$children, function(child) {
    paste(paste0("--", x$boundary), as.character.MIME(child), sep = "\r\n")
  })
  #
  headers <- list(
    content_type(type(x), x$charset, x$boundary, x$format),
    content_disposition(x$disposition),
    content_transfer_encoding(x$encoding),
    content_language(x$language, x$content),
    x_attachment_id(x$cid),
    content_id(x$cid)
  )
  #
  content <- x$content
  if (!is.na(x$encoding)) {
    if (x$encoding == "base64") {
      content <- mime_base64encode(content)
    } else if (x$encoding == "quoted-printable") {
      content <- qp_encode(content)
    }

    headers <- c(headers, list(content_md5(x$content)))
  }
  #
  body <- c(
    # Head.
    headers %>% compact() %>% sapply(as.character),
    "",
    # Content (if any).
    content,
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

# LENGTH ----------------------------------------------------------------------

#' Length of a MIME object
#'
#' The underlying object is a list, but we don't want the length of this
#' object to be the length of the list.
#'
#' This is especially important for when we have a message that only consists
#' of one MIME item. In that case we don't want it rendered as multipart/mixed.
#'
#' @noRd
#'
#' @param x A MIME object.
#'
#' @return The length of a MIME object (which is always one in units of MIME objects!).
length.MIME <- function(x) {
  1
}
