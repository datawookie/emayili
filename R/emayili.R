#' @import curl
#' @import digest
#' @import dplyr
#' @import htmltools
#' @import logger
#' @import purrr
#' @import rvest
#' @import stringr
#' @import tidyr
#' @import xml2
#' @importFrom base64enc base64decode base64encode
#' @importFrom commonmark markdown_html
#' @importFrom glue glue
#' @importFrom httr http_date
#' @importFrom mime guess_type
#' @importFrom rmarkdown render html_document
#' @importFrom stats setNames na.omit
#' @importFrom stringi stri_replace_all_fixed stri_replace_all_regex stri_enc_mark
#' @importFrom tools file_ext
#' @importFrom urltools url_decode
#' @importFrom utils packageVersion download.file modifyList
#' @importFrom xfun read_utf8
NULL

.onLoad <- function(libname, pkgname){
  log_layout(layout_glue_generator('{str_pad(level, 7, side = "right")} [{time}] {msg}'))
}

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
