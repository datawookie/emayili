test_that("error with empty input", {
  expect_error(envelope() %>% render(""))
})

test_that("render plain Markdown from string", {
  expect_match(
    envelope() %>%
      render("[This](https://www.google.com) is a link.") %>%
      as.character(),
    "<a href=\"https://www.google.com\">"
  )
})

test_that("render R Markdown from file", {
  expect_match(
    envelope() %>%
      render(RMD_TEMPLATE) %>%
      as.character(),
    "<strong>knitr</strong>"
  )
})

test_that("interpolate into Markdown", {
  name <- "Alice"
  expect_match(
    envelope() %>%
      render("Hello {{name}}!") %>%
      as.character(),
    "Hello Alice!"
  )
})

test_that("whether to include rendered CSS", {
  # No rendered CSS.
  expect_no_match(
    envelope() %>%
      render(RMD_TEMPLATE, include_css = c()) %>%
      as.character(),
    '<style type="text/css">'
  )
  expect_no_match(
    envelope() %>%
      render(RMD_TEMPLATE, include_css = FALSE) %>%
      as.character(),
    '<style type="text/css">'
  )
  # Only CSS from {rmarkdown}.
  expect_match(
    envelope() %>%
      render(RMD_TEMPLATE, include_css = "rmd") %>%
      as.character(),
    '<style type="text/css">'
  )
  # All CSS.
  expect_match(
    envelope() %>%
      render(RMD_TEMPLATE, include_css = TRUE) %>%
      as.character(),
    '<style type="text/css">.*\\.hljs-literal'
  )
})

test_that("invalid value for include_css", {
  expect_error(
    envelope() %>%
      render(RMD_TEMPLATE, include_css = "css"),
    'Valid options for include_css'
  )
})

test_that("include extra CSS", {
  expect_match(
    envelope() %>%
      render(PLAIN_MARKDOWN, css_files = CSSPATH) %>%
      as.character(),
    COLOUR_GLAUCOUS
  )
  expect_match(
    envelope() %>%
      render(RMD_TEMPLATE, css_files = CSSPATH, include_css = FALSE) %>%
      as.character(),
    COLOUR_GLAUCOUS
  )
})

test_that("extra CSS: unable to find file", {
  expect_error(
    envelope() %>% render(RMD_TEMPLATE, css_files = "missing.css"),
    "Unable to find"
  )
})
