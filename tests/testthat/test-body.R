test_that("text: only single message body", {
  expect_error(envelope() %>% text("<p>foo</p>"), NA)
  expect_error(envelope() %>% text(c("<p>foo</p>", "<p>bar</p>")))
})

test_that("list_to_char: tagList & vec of html are casted to character", {
  expect_equal(
    list_to_char(c("<b>Hello!</b>", "<p>World</p>")),
    "<b>Hello!</b>\n<p>World</p>"
  )
  skip_if_not_installed("htmltools")
  expect_equal(
    list_to_char(
      htmltools::tagList(
        htmltools::h2("this"),
        htmltools::p("That")
      )
    ),
    "<h2>this</h2>\n<p>That</p>"
  )
})

test_that("html: tagList & vec of html are cast to character", {
  msg <- envelope() %>% html(c("<b>Hello!</b>", "<p>World</p>"))
  expect_true(
    grepl(
      "<b>Hello!</b>",
      msg$parts[[1]]$content
    )
  )
  expect_true(
    grepl(
      "<p>World</p>",
      msg$parts[[1]]$content
    )
  )

  skip_if_not_installed("htmltools")
  msg <- envelope() %>% html(
    htmltools::tagList(
      htmltools::h2("Hello"),
      htmltools::p("World")
    )
  )

  expect_true(
    grepl(
      "<h2>Hello</h2>",
      msg$parts[[1]]$content
    )
  )
  expect_true(
    grepl(
      "<p>World</p>",
      msg$parts[[1]]$content
    )
  )
})

test_that("html: Text not HTML", {
  expect_error(envelope %>% html(TXTCONTENT))
})

test_that("html: <img> without src", {
  expect_error(envelope %>% html("<img>"))
})

test_that("html: <img> from URL", {
  msg <- envelope() %>% html(glue('<img src="{IMG_URL}">'))
  expect_match(as.character(msg), '<img src="cid:[0-9a-z]{8}">')
})

test_that("html: <img> from file", {
  msg <- envelope() %>% html(glue('<img src="{JPGPATH}">'))
  expect_match(as.character(msg), '<img src="cid:[0-9a-z]{8}">')
})

test_that("html: <img> with Base64 encoded content", {
  img_uri <- dataURI(file = JPGPATH, mime = "image/jpg")
  msg <- envelope() %>% html(glue('<img src="{img_uri}">'))
  expect_match(as.character(msg), '<img src="cid:[0-9a-z]{8}">')
})

test_that("html: HTML from file", {
  expect_true(
    grepl(
      HTMLCONTENT,
      envelope() %>% html(HTMLPATH, encoding = NULL) %>% as.character()
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

test_that("css: inject CSS", {
  expect_match(
    envelope() %>%
      html("<p>foo</p>", css_files = CSSPATH) %>%
      as.character(),
    COLOUR_GLAUCOUS
  )
})

test_that("css: multiple CSS sources", {
  MSG <- "<html>
  <head>
    <style>
      @import url('https://fonts.googleapis.com/css2?family=Roboto&display=swap');
      b {color: green;}
    </style>
    <link rel='stylesheet' href='https://pro.fontawesome.com/releases/v5.10.0/css/all.css'/>
  </head>
  <body>
    <p>Some test text.<b>BOLD!</b></p>
  </body>
</html>"

  msg <- envelope() %>%
    html(MSG, css_files = CSSPATH) %>%
    as.character()

  # CSS from file.
  expect_match(msg, COLOUR_GLAUCOUS)
  # CSS from <style>.
  expect_match(msg, "b \\{color: green;\\}")
  # CSS from <link>.
  expect_match(msg, "Font Awesome")
  # CSS from @import.
  expect_match(msg, "font-family: 'Roboto';")
})

test_that("Content-Type header", {
  expect_match(
    envelope() %>% text("Hello!") %>% as.character(),
    "Content-Type: +text/plain;[[:space:]]+charset=utf-8;[[:space:]]+format=flowed"
  )
})
