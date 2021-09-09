#' Pipe operator
#'
#' FOO \link[magrittr]{%>%}
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

get_option_invisible <- function(default = TRUE) {
  getOption("envelope_invisible", default = default)
}

get_option_details <- function(default = TRUE) {
  getOption("envelope_details", default = default)
}

#' Read entire text file into character vector
#'
#' @noRd
#'
#' @param path Relative or absolute file path
#'
#' @return A character vector
read_file <- function(path) {
  readChar(path, file.info(path)$size)
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