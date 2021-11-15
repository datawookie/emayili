new_header <- function(
  name,
  values,
  sep = NULL
) {
  structure(
    list(
      name = name,
      values = values,
      sep = sep
    ),
    class = "header"
  )
}

#' Create formatted header.
#'
#' Accepts a header object and formats it as a header field.
#'
#' @param x A header object.
#' @param ... Further arguments passed to or from other methods.
#' @export
#'
#' @return A formatted header field.
as.character.header <- function(x, ...) {
  sprintf(
    "%-28s %s",
    paste0(x$name, ":"),
    paste(
      sapply(x$values, as.character),
      collapse = x$sep
    )
  )
}

print.header <- function(x, ... ) {
  print(as.character(x))
}

header_get <- function(msg, name) {
  msg$headers[[name]]$values
}

header_set <- function(msg, name, values, append = FALSE, sep = NULL) {
  # Get current header.
  header <- msg$headers[[name]]
  # Header has not previously been set.
  if (is.null(header)) {
    header <- new_header(name, c(), sep)
  } else {
    if (append) {
      values <- c(header$values, values)
    }
  }

  # Set current header values.
  header$values <- values

  msg$headers[[name]] <- header

  msg
}
