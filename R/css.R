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

css_inline <- function(content) {
  # Extract CSS from <link> and <style> tags and append.
  #
  css <- c(
    # * Inline CSS in <link> tags.
    inline = xml_find_all(content, "//link[starts-with(@href,'data:text/css')]") %>%
      xml_attr("href") %>%
      unlist() %>%
      url_decode() %>%
      str_replace("data:text/css,", ""),
    # * External CSS in <link> tags.
    # - Doesn't apply to Plain Markdown.
    # external = xml_find_all(content, "//link[not(starts-with(@href,'data:text/css'))]") %>%
    #   xml_attr("href") %>%
    #   map(function(path) {
    #     include_css <- setdiff(include_css, "rmd")
    #     # Check is CSS path matches one of the requested options.
    #     if (length(include_css)) {
    #       if (str_detect(path, paste0("/", include_css, collapse = "|"))) path else NULL
    #     } else NULL
    #   }) %>%
    #   unlist(),
      # file.path(dirname(input), .) %>%
      # map_chr(read_text),
    # * Inline CSS in <style> tags.
    style = xml_find_all(content, "//style") %>%
      xml_text() %>%
      unlist(),
    NULL
  )

  css
}
