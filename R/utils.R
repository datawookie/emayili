REGEX_BARE_LINEFEED = "(?<!\r)\n"

#' Pipe operator
#'
#' \link[magrittr]{%>%}
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# For adding elements to a list using %>%.
#
set <- .Primitive("[[<-")

#' Compare vectors
#'
#' Returns \code{TRUE} wherever elements are the same (including \code{NA}),
#' and \code{FALSE} everywhere else.
#'
#' @param lhs LHS of operation.
#' @param rhs RHS of operation.
#'
#' @return A Boolean value.
compare <- function(lhs, rhs) {
  same <- (lhs == rhs) | (is.na(lhs) & is.na(rhs))
  same[is.na(same)] <- FALSE
  same
}

#' Read entire text file into character vector
#'
#' @noRd
#'
#' @param path Relative or absolute file path
#'
#' @return A character vector
read_text <- function(path) {
  readChar(path, file.info(path)$size)
}

#' Read entire binary file into character vector
#'
#' @noRd
#'
#' @param path Relative or absolute file path
#'
#' @return A character vector
read_bin <- function(path) {
  readBin(path, "raw",  file.info(path)$size)
}

#' Normalise file path
#'
#' @noRd
#'
#' @param path Relative or absolute file path
#'
#' @return An absolute file path (if the file exists) or \code{NA}.
normalise_filepath <- function(path) {
  possibly(normalizePath, NA_character_)(path, mustWork = TRUE)
}

#' Check if character vector is a file name or file path
#'
#' @noRd
#'
#' @param path A character vector (which might also be a file path)
#'
#' @return If it is a file path, then return \code{TRUE}, otherwise return \code{FALSE}.
is_filepath <- function(path) {
  !is.na(normalise_filepath(path))
}

hexkey <- function(object = runif(1), algorithm="crc32") {
  digest(object, algorithm)
}

#' Drape line feeds
#'
#' Replace empty line-feeds, "\n", with carriage-return and line-feed, "\r\n".
#'
#' @noRd
drape_linefeed <- function(txt) {
  str_replace_all(txt, REGEX_BARE_LINEFEED, "\r\n")
}

#' Remove comments from CSS
#'
#' Will handle comments with the following form:
#'
#' - /* ... */
#' - /*! ... */
#'
#' @noRd
css_remove_comment <- function(css) {
  str_replace_all(css, "/\\*!?(\\*(?!/)|[^\\*])*\\*/", "")
}
