#' @import curl
#' @import dplyr
#' @import logger
#' @import magrittr
#' @import stringr
#' @import tibble
#' @importFrom base64enc base64encode
#' @importFrom commonmark markdown_html
#' @importFrom glue glue
#' @importFrom httr http_date
#' @importFrom mime guess_type
#' @importFrom purrr insistently rate_backoff map map_dfr map_chr
#' @importFrom rmarkdown render
#' @importFrom stats setNames
#' @importFrom tidyr unnest
#' @importFrom utils packageVersion
#' @importFrom vctrs new_rcrd vec_recycle_common vec_cast_common vec_assert field
#' @importFrom xfun read_utf8
NULL

# Deal with "no visible binding for global variable ‘.’".
#
globalVariables(c("."))

TMPDIR <- "/tmp"
