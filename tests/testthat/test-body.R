test_that("text: only single message body", {
  expect_error(envelope() %>% text("<p>foo</p>"), NA)
  expect_error(envelope() %>% text(c("<p>foo</p>", "<p>bar</p>")))
})

test_that("html: only single message body", {
  expect_error(envelope() %>% html("<p>foo</p>"), NA)
  expect_error(envelope() %>% html(c("<p>foo</p>", "<p>bar</p>")))
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
  options(envelope.invisible = FALSE)
  expect_visible(envelope() %>% text("Hello!"))
  options(envelope.invisible = TRUE)
  expect_invisible(envelope() %>% text("Hello!"))
})

test_that("html: inject CSS", {
  expect_match(
    envelope() %>%
      html("<p>foo</p>", css_files = CSSPATH) %>%
      as.character(),
    COLOUR_GLAUCOUS
  )

})
