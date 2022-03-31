content_type <- function(type, protocol, charset, boundary, format = NA, name = NA) {
  name <- encodable(name) %>% as.character(encode = TRUE)

  header(
    "Content-Type",
    c(
      type,
      if (!is.na(protocol)) glue('protocol="{protocol}"') else NULL,
      if (!is.na(charset)) glue("charset={charset}") else NULL,
      if (!is.na(boundary)) glue('boundary="{boundary}"') else NULL,
      if (!is.na(format)) glue("format={format}") else NULL,
      if (!is.na(name)) glue('name="{name}"') else NULL
    ),
    sep = "; "
  )
}

content_disposition <- function(disposition = NA, filename = NA) {
  filename <- encodable(filename) %>% as.character(encode = TRUE)

  if (is.na(disposition)) {
    NULL
  } else {
    if (!is.na(filename)) {
      header(
        "Content-Disposition",
        c(
          disposition,
          if (!is.na(filename)) glue('filename="{filename}"') else NULL
        ),
        sep = "; "
      )
    }
  }
}

content_transfer_encoding <- function(encoding = NA) {
  if (is.na(encoding)) {
    NULL
  } else {
    header("Content-Transfer-Encoding", encoding)
  }
}

content_description <- function(description = NA) {
  if (is.na(description)) {
    NULL
  } else {
    header("Content-Description", description)
  }
}

x_attachment_id <- function(cid = NULL) {
  if (is.null(cid)) {
    NULL
  } else {
    header("X-Attachment-Id", cid)
  }
}

content_id <- function(cid = NULL) {
  if (is.null(cid)) {
    NULL
  } else {
    header("Content-ID", paste0("<", cid, ">"))
  }
}

content_md5 <- function(content) {
  header("Content-MD5", md5(content))
}
