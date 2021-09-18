#' @import curl
#' @import digest
#' @import dplyr
#' @import logger
#' @import magrittr
#' @import stringr
#' @import tibble
#' @import xml2
#' @importFrom base64enc base64encode
#' @importFrom commonmark markdown_html
#' @importFrom glue glue
#' @importFrom httr http_date
#' @importFrom mime guess_type
#' @importFrom purrr possibly insistently rate_backoff map map_dfr map_chr
#' @importFrom rmarkdown render
#' @importFrom stats setNames
#' @importFrom tidyr unnest
#' @importFrom tools file_ext
#' @importFrom urltools url_decode
#' @importFrom utils packageVersion
#' @importFrom vctrs new_rcrd vec_recycle_common vec_cast_common vec_assert field
#' @importFrom vctrs vec_ptype_abbr vec_ptype_full
#' @importFrom xfun read_utf8
NULL

globalVariables(
  c(
    ".",
    "msg",
    "runif"
    )
  )
