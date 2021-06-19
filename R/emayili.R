#' @import curl
#' @import magrittr
#' @import stringr
#' @importFrom purrr insistently rate_backoff
#' @importFrom base64enc base64encode
#' @importFrom glue glue
#' @importFrom httr http_date
#' @importFrom mime guess_type
#' @importFrom utils packageVersion
NULL

# Deal with "no visible binding for global variable ‘.’".
#
globalVariables(c("."))
