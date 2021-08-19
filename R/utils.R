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
#' @return
compare <- function(lhs, rhs) {
  same <- (lhs == rhs) | (is.na(lhs) & is.na(rhs))
  same[is.na(same)] <- FALSE
  same
}
