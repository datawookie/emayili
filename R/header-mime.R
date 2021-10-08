# Headers for the MIME protocol.

content_type <- function(type, charset, boundary, format = NA) {
  new_header(
    "Content-Type",
    paste(
      c(
        type,
        if (!is.na(charset)) glue('charset={charset}') else NULL,
        if (!is.na(boundary)) glue('boundary="{boundary}"') else NULL,
        if (!is.na(format)) glue('format={format}') else NULL
      ),
      collapse = "; "
    )
  )
}

content_disposition <- function(disposition = NA) {
  if (is.na(disposition)) {
    NULL
  } else {
    new_header("Content-Disposition", disposition)
  }
}

content_transfer_encoding <- function(encoding = NA) {
  if (is.na(encoding)) {
    NULL
  } else {
    new_header("Content-Transfer-Encoding", encoding)
  }
}

x_attachment_id <- function(cid = NULL) {
  if (is.null(cid)) {
    NULL
  } else {
    new_header("X-Attachment-Id", cid)
  }
}

content_id <- function(cid = NULL) {
  if (is.null(cid)) {
    NULL
  } else {
    new_header("Content-ID", paste0("<", cid, ">"))
  }
}

content_md5 <- function(content) {
  new_header("Content-MD5", md5(content))
}
