header <- function(name,
                   values,
                   sep = NULL) {
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
#' @param width The width of the head name field.
#' @param ... Further arguments passed to or from other methods.
#' @export
#'
#' @return A formatted header field.
as.character.header <- function(x, width = 30, ...) {
  FORMAT <- glue("%-{width}s")
  INDENT <- strrep(" ", width)

  paste0(
    sprintf(
      FORMAT,
      paste0(x$name, ":")
    ),
    paste(
      as.character(x$values, ...),
      collapse = paste0(x$sep, CRLF, INDENT)
    )
  )
}
# as.character.header(msg$headers$To) %>% cat()

print.header <- function(x, ...) {
  print(as.character(x))
}

header_get <- function(msg, name) {
  msg$headers[[name]]$values
}

header_set <- function(msg, name, values, append = FALSE, sep = NULL) {
  # Get current header.
  header <- msg$headers[[name]]
  if (is.null(header)) {
    # Header has not previously been set.
    header <- header(name, c(), sep)
  } else {
    # Header has previously been set.
    if (append) {
      values <- c(header$values, values)
    }
  }

  # Set current header values.
  header$values <- values

  msg$headers[[name]] <- header

  msg
}
