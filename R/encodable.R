#' Encodable Text
#'
#' Create an \code{encodable} object which represents text that might be encoded
#' with the purpose of embedding in a header field.
#'
#' @noRd
encodable <- function(text) {
  if ("encodable" %in% class(text)) {
    text
  } else {
    structure(
      text,
      class = "encodable"
    )
  }
}

#' Convert encodable object to character
#'
#' @param x  An \code{encodable} object.
#' @param encode Whether to encode.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A character vector.
#' @noRd
as.character.encodable <- function(x, encode = FALSE, ...) {
  x <- unclass(x)
  if (stri_enc_mark(x) != "ASCII" && encode) {
    paste0("=?UTF-8?B?", base64encode(charToRaw(x)), "?=")
  } else {
    x
  }
}
