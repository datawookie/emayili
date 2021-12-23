header <- function(
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
#' @param width The width of the head name field.
#' @param ... Further arguments passed to or from other methods.
#' @export
#'
#' @return A formatted header field.
as.character.header <- function(x, width = 28, ...) {
  FORMAT <- glue("%-{width}s %s")
  INDENT <- strrep(" ", width + 1)

  header <- sprintf(
    FORMAT,
    paste0(x$name, ":"),
    paste(
      as.character(x$values),
      collapse = x$sep
    )
  )

  # Split header parameters across lines.
  if (!is.null(x$sep)) {
    header <- header %>%
      str_replace_all(
        paste0("(", str_trim(x$sep, side = "right"), ")"),
        paste0("\\1", CRLF, INDENT)
      )
  }

  header
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
    header <- header(name, c(), sep)
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
