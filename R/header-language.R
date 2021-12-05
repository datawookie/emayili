#' Specify content language
#'
#' Implements \href{https://datatracker.ietf.org/doc/html/rfc3282}{RFC 3282}.
#'
#' @noRd
content_language <- function(language, content) {
  if (is.na(language)) language <- FALSE

  if (is.logical(language)) {
    if (language) {
      # Auto-detect language.
      if (requireNamespace("cld3", quietly = TRUE)) {           # nocov start
        detect_language <- cld3::detect_language
      } else if (requireNamespace("cld2", quietly = TRUE)) {
        detect_language <- cld2::detect_language
      } else {
        stop("Unable to auto-detect language. Install {cld3} or {cld2}.")
      }                                                         # nocov end

      language <- detect_language(content)
    } else {
      # Don't include Content-Language.
      return(NULL)
    }
  } else {
    if (!is.character(language)) {
      stop("Language must either be a string or TRUE/FALSE.")   # nocov
    }
  }
  header("Content-Language", paste(language, collapse = ", "))
}
