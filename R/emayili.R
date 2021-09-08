#' @import curl
#' @import magrittr
#' @import stringr
#' @import dplyr
#' @import tibble
#' @import logger
#' @importFrom vctrs new_rcrd vec_recycle_common vec_cast_common vec_assert field
#' @importFrom purrr insistently rate_backoff map map_dfr map_chr
#' @importFrom tidyr unnest
#' @importFrom base64enc base64encode
#' @importFrom glue glue
#' @importFrom httr http_date
#' @importFrom mime guess_type
#' @importFrom utils packageVersion
#' @importFrom stats setNames
#' @importFrom commonmark markdown_html
#' @importFrom xfun read_utf8
NULL

# Deal with "no visible binding for global variable ‘.’".
#
globalVariables(c("."))
