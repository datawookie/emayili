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
#' Apply encoding as specified in Section 2 ("Syntax of encoded-words") of
#' RFC 2047.
#'
#' The `?B?` presumably indicates that the data is Base64 encoded. You can also
#' use `?Q?`, which presumably indicates that the data is Quoted Printable
#' encoded.
#'
#' @param x  An \code{encodable} object.
#' @param encode Whether to encode.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A character vector.
#' @noRd
as.character.encodable <- function(x, encode = FALSE, ...) {
  # Just keep the underlying text.
  x <- unclass(x)

  ifelse(
    is.na(x),
    NA,
    ifelse(
      (stri_enc_mark(x) != "ASCII" | str_detect(x, ",")) & encode,
      paste0("=?UTF-8?B?", map_chr(x, ~ base64encode(charToRaw(.))), "?="),
      x
    )
  )
}

Ops.encodable <- function(e1, e2) {
  e1 <- encodable(e1)
  e2 <- encodable(e2)

  get(.Generic)(as.character(e1), as.character(e2))
}
