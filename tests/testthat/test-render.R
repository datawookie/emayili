test_that("render plain Markdown from string", {
  expect_match(
    envelope() %>%
      md("[This](https://www.google.com) is a link.") %>%
      as.character(),
    "<a href=\"https://www.google.com\">"
  )
})

test_that("render R Markdown from file", {
  expect_match(
    envelope() %>%
      rmd(FILE_RMD) %>%
      as.character(),
    "<h2 id=\"github-documents\">GitHub Documents</h2>"
  )
})

