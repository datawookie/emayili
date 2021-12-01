# Headers for the MIME protocol.

content_type <- function(type, protocol, charset, boundary, format = NA, name = NA) {
  new_header(
    "Content-Type",
    paste(
      c(
        type,
        if (!is.na(protocol)) glue('protocol="{protocol}"') else NULL,
        if (!is.na(charset)) glue('charset={charset}') else NULL,
        if (!is.na(boundary)) glue('boundary="{boundary}"') else NULL,
        if (!is.na(format)) glue('format={format}') else NULL,
        if (!is.na(name)) glue('name="{name}"') else NULL
      ),
      collapse = "; "
    )
  )
}

content_disposition <- function(disposition = NA, filename = NA) {
  if (is.na(disposition)) {
    NULL
  } else {
    if (!is.na(filename)) {
      disposition <- paste(disposition, glue('filename="{filename}"'), sep = "; ")
    }
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
