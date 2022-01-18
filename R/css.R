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

#' Replace @import() content in CSS
#'
#' Assumes that there is just a single \code{<style>} tag in \code{<head>}.
#'
#' @noRd
css_import <- function(content) {
  # Find first <style> tag in <head>.
  css <- xml_find_first(content, "//head/style")

  if (is.na(css)) {
    log_debug("No embedded <style>.")
  } else {
    # Extract content of <style> tag.
    css_text <- xml_text(css)

    # Iterate over @import statements.
    #
    for (import in unlist(str_match_all(css_text, "@import url\\(.*\\);"))) {
      # Get URL from url().
      url <- import %>% str_remove_all("(^@import url\\([\"']|[\"']\\);$)")
      # Pull content from URL.
      imported <- readLines(url) %>% paste(collapse = "\n")
      # Replace @import url(); with downloaded content.
      css_text <- sub(import, imported, css_text, fixed = TRUE)
    }

    # Replace content of <style> tag.
    xml_text(css) <- css_text
  }

  content
}
