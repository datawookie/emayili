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
#' @param description Description of content.
#' @param name Name used when downloading file.
#' @param filename Path to a file.
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
#' @return A MIME object.
MIME <- function(
  content = NULL,
  disposition = NA,
  protocol = NA,
  charset = NA,
  encoding = NA,
  language = NA,
  description = NA,
  name = NA,
  filename = NA,
  format = NA,
  boundary = hexkey(),
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
      protocol = protocol,
      charset = charset,
      encoding = encoding,
      language = language,
      description = description,
      name = name,
      filename = filename,
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

multipart_alternative <- function(...) {
  structure(
    c(
      MIME(...),
      list()
    ),
    class = c("multipart_alternative", "MIME")
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

#' Create \code{multipart/encrypted} MIME object
#'
#' @noRd
#'
#' @inheritParams MIME
multipart_encrypted <- function(
  content,
  ...
) {
  structure(
    c(
      MIME(
        "This is an OpenPGP/MIME encrypted message (RFC 4880 and 3156).",
        protocol = "application/pgp-encrypted",
        ...
      ),
      list()
    ),
    class = c("multipart_encrypted", "MIME")
  )
}

#' Create \code{multipart/signed} MIME object
#'
#' @noRd
#'
#' @inheritParams MIME
#' @param micalg Message Integrity Check ALGorithm. Valid options are:
#'   \code{"pgp-sha256"}, \code{"pgp-md5"}, \code{"pgp-sha1"},
#'   \code{"pgp-ripemd160"}, \code{"pgp-md2"}, \code{"pgp-tiger192"}, and
#'  \code{"pgp-haval-5-160"}.
multipart_signed <- function(
  micalg = "pgp-sha256",
  ...
) {
  if (!(micalg %in% LEVELS_MICALG)) stop('Invalid micalg: "{micalg}".')
  structure(
    c(
      MIME(
        "This is an OpenPGP/MIME signed message (RFC 4880 and 3156).",
        protocol = "application/pgp-signature",
        type = c("multipart/signed", 'micalg="{micalg}"'),
        ...
      ),
      list()
    ),
    class = c("multipart_signed", "MIME")
  )
}

application_pgp_encrypted <- function(
  content = "Version: 1",
  ...
) {
  structure(
    c(
      MIME(
        content,
        type = "application/pgp-encrypted",
        description = "PGP/MIME version identification",
        boundary = NA,
        ...
      ),
      list()
    ),
    class = c("application_pgp_encrypted", "MIME")
  )
}

application_pgp_signature <- function(
  content,
  ...
) {
  structure(
    c(
      MIME(
        content,
        type = "application/pgp-signature",
        description = "OpenPGP digital signature",
        name = "signature.asc",
        filename = "signature.asc",
        disposition = "attachment",
        boundary = NA,
        ...
      ),
      list()
    ),
    class = c("application_pgp_signature", "MIME")
  )
}

application_pgp_keys <- function(
  content,
  ...
) {
  structure(
    c(
      MIME(
        content,
        type = "application/pgp-keys",
        description = "OpenPGP public key",
        name = "keys.asc",
        filename = "keys.asc",
        disposition = "attachment",
        encoding = "quoted-printable",
        boundary = NA,
        ...
      ),
      list()
    ),
    class = c("application_pgp_keys", "MIME")
  )
}

application_octet_stream <- function(
  content,
  disposition = "inline",
  filename,
  ...
) {
  structure(
    c(
      MIME(
        content,
        disposition = disposition,
        type = "application/octet-stream",
        description = "OpenPGP encrypted message",
        name = filename,
        filename = filename,
        boundary = NA,
        ...
      ),
      list()
    ),
    class = c("application_octet_stream", "MIME")
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
      MIME(content, disposition, NA, charset, encoding, format = "flowed", boundary = NA, ...),
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
#' @param content An \code{xml_document} object. Will try to coerce to
#'   \code{xml_document} object
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
  if (!("xml_document" %in% class(content))) {
    content <- read_html(content)
  }

  # Replace CSS @import content.
  content <- css_import(content)

  # Add custom CSS rules last so that they overrides preceding rules.
  css <- c(css_inline(content), css)

  # Clean up HTML content.
  #
  # - Delete <script>, <link>, <style> and <meta> tags. There might be multiple
  #   <style> tags in the original document. Remove all of those and then add
  #   back a single consolidated <style> tag.
  log_debug("- Remove various tags.")
  xml_find_all(content, "//script | //meta | //link | //style") %>% xml_remove()
  # - Remove comments.
  log_debug("- Remove comments.")
  xml_find_all(content, "//comment()") %>% xml_remove()

  if (length(css) && !all(is.na(css) | css == "")) {
    log_debug("- Consolidate CSS.")

    css <- css %>%
      unlist() %>%
      na.omit() %>%
      str_c(collapse = "\n") %>%
      css_remove_comment() %>%
      str_squish()

    # Add <head> (can be missing if rendering Plain Markdown).
    if (is.na(xml_find_first(content, "//head"))) {
      log_debug("- Add <head>.")
      xml_add_sibling(
        xml_find_first(content, "//body"),
        "head",
        .where = "before"
      )
    }

    # Write consolidated CSS to single <style> tag.
    if (!is.na(css) && nchar(css)) {
      xml_add_child(
        xml_find_first(content, "//head"),
        "style",
        css,
        type = "text/css"
      )
    }
  }

  # Convert from xml_document to string.
  #
  content <- as.character(content)

  # Clean up content.
  if (squish) {
    content <- html_squish(content)
  }

  content <- content %>%
    # Remove <!DOCTYPE> tag.
    str_replace("[:space:]*<!DOCTYPE html[^>]*>[:space:]*", "") %>%
    # Remove <meta> tag (a "Content-Type" <meta> inserted by {xml2}).
    str_replace("<meta[^>]*>", "")

  # Replace bare line-feeds.
  content <- drape_linefeed(content)

  structure(
    c(
      MIME(
        content,
        disposition,
        NA,
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
  if (is.na(name)) name <- basename(filename)

  if (!is.na(type)) {
    # Could use mime::mimemap to map from specific extensions to MIME types,
    # so that the following would give the same result:
    #
    # attachment(..., type = "pdf")
    # attachment(..., type = "application/pdf")
  } else {
    type <- guess_type(filename, empty = "application/octet-stream")
  }
  type <- glue('{type}; name{parameter_value_encode(name)}')

  if (is.na(disposition)) {
    disposition <- ifelse(
      grepl("text", type),
      # If it's text...
      "inline",
      # ... otherwise:
      "attachment"
    )
  }

  structure(
    c(
      MIME(read_bin(filename), disposition, NA, charset, encoding, boundary = NA, filename = name, type = type, ...),
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
#' @export
#' @noRd
#'
after.MIME <- function(x, child) {
  if (!is.mime(child)) stop(ERROR_NOT_MIME_OBJECT)
  x$children <- c(x$children, list(child))
  x
}

#' @rdname add_children
#' @export
#' @noRd
#'
before.MIME <- function(x, child) {
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
    content_type(type(x), x$protocol, x$charset, x$boundary, x$format, x$name),
    content_description(x$description),
    content_disposition(x$disposition, x$filename),
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
#' @export
#' @noRd
#'
#' @param x A MIME object.
#'
#' @return The length of a MIME object (which is always one in units of MIME objects!).
length.MIME <- function(x) {
  1
}
