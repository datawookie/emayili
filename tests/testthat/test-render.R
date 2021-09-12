test_that("error with empty input", {
  expect_error(envelope() %>% render(""))
})

test_that("render plain Markdown from string", {
  expect_match(
    envelope() %>%
      render("[This](https://www.google.com) is a link.", plain = TRUE) %>%
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
      render("Hello {{name}}!", plain = TRUE) %>%
      as.character(),
    "Hello Alice!"
  )
})
