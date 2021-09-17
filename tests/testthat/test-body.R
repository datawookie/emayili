test_that("text: only single message body", {
  expect_error(envelope() %>% text("foo"), NA)
  expect_error(envelope() %>% text(c("foo", "bar")))
})

test_that("html: only single message body", {
  expect_error(envelope() %>% html("foo"), NA)
  expect_error(envelope() %>% html(c("foo", "bar")))
})

test_that("html: HTML from file", {
  expect_true(
    grepl(
      HTMLCONTENT,
      envelope() %>% html(HTMLPATH) %>% as.character()
    )
  )
})

test_that("disable interpolation", {
  expect_match(
    envelope() %>%
      text("Hello {{name}}!", interpolate = FALSE) %>%
      as.character(),
    "Hello \\{\\{name\\}\\}!"
  )
})

test_that("interpolate from environment", {
  variables <- list(name = "Alice")
  expect_match(
    envelope() %>%
      text("Hello {{name}}!", .envir = variables) %>%
      as.character(),
    "Hello Alice!"
  )
  expect_match(
    envelope() %>%
      html("<p>Hello {{name}}!</p>", .envir = variables) %>%
      as.character(),
    "Hello Alice!"
  )
})

test_that("interpolation delimeters", {
  name <- "Alice"
  expect_match(
    envelope() %>%
      text("Hello <<name>>!", .open = "<<", .close = ">>") %>%
      as.character(),
    "Hello Alice!"
  )
})

test_that("toggle visibility", {
  options(envelope_invisible = FALSE)
  expect_visible(envelope() %>% text("Hello!"))
  options(envelope_invisible = TRUE)
  expect_invisible(envelope() %>% text("Hello!"))
})
