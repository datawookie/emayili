test_that("CSS in <link>", {
  content <- read_html('<html><head><link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"></head></html>')
  css <- css_inline(content)
  expect_true("external" %in% names(css))
})
