#' @import curl
#' @import digest
#' @import dplyr
#' @import htmltools
#' @import logger
#' @import purrr
#' @import stringr
#' @import tidyr
#' @import xml2
#' @importFrom base64enc base64encode
#' @importFrom commonmark markdown_html
#' @importFrom glue glue
#' @importFrom httr http_date
#' @importFrom mime guess_type
#' @importFrom rmarkdown render html_document
#' @importFrom stats setNames
#' @importFrom stringi stri_replace_all_fixed stri_replace_all_regex
#' @importFrom tools file_ext
#' @importFrom urltools url_decode
#' @importFrom utils packageVersion
#' @importFrom vctrs new_rcrd vec_recycle_common vec_cast_common vec_assert field
#' @importFrom vctrs vec_ptype_abbr vec_ptype_full
#' @importFrom xfun read_utf8
NULL

LEVELS_ENCODING <- c("7bit", "quoted-printable", "base64")
LEVELS_PRIORITY <- c("non-urgent", "normal", "urgent")
LEVELS_IMPORTANCE <- c("low", "normal", "high")

globalVariables(
  c(
    ".",
    "e1",
    "e2",
    "fingerprint",
    "msg",
    "runif",
    "detect_language"
  )
)
