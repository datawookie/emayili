#' @export
attachment <- function(x, ...) UseMethod("send")

encode_base64 <- function(x, line_length = 76L, newline = "\r\n") {
  if(is.raw(x)) {
    base64encode(x, 76L, newline)
  } else {
    base64encode(charToRaw(x), 76L, "\r\n")
  }
}

#' @rdname envelope
#' @export
attachment <- function(msg, path){
  type <- guess_type(path, empty = NULL)

  con <- file(path, "rb")
  body <- readBin(con, "raw",  file.info(path)$size)
  close(con)

  mime <- mime(type, "base64", NULL, NULL,
               name = basename(path),
               filename = basename(path),
               content_transfer_encoding = "base64")

  mime$body <- base64encode(body, 76L, "\r\n")

  msg$parts <- c(msg$parts, list(mime))

  msg
}
