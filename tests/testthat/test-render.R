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
      render(FILE_RMD) %>%
      as.character(),
    "<h2 id=\"github-documents\">GitHub Documents</h2>"
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

test_that("interpolate from environment", {
  variables <- list(name = "Alice")
  expect_match(
    envelope() %>%
      render("Hello {{name}}!", plain = TRUE, .envir = variables) %>%
      as.character(),
    "Hello Alice!"
  )
})

test_that("interpolation delimeters", {
  name <- "Alice"
  expect_match(
    envelope() %>%
      render(
        "Hello <<name>>!",
        plain = TRUE,
        .open = "<<",
        .close = ">>"
        ) %>%
      as.character(),
    "Hello Alice!"
  )
})

test_that("disable interpolation", {
  expect_match(
    envelope() %>%
      render("Hello {{name}}!", plain = TRUE, interpolate = FALSE) %>%
      as.character(),
    "Hello \\{\\{name\\}\\}!"
  )
})

