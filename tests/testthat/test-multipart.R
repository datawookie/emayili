msg <- envelope() %>%
  from(EMAIL_FROM) %>%
  to(EMAIL_TO)

test_that("message with just text", {
  msg <- msg %>%
    subject("MIME: text") %>%
    text("Hello!")

  txt <- as.character(msg)

  expect_no_match(txt, "multipart/alternative")
  expect_no_match(txt, "multipart/mixed")
  expect_match(txt, "Hello!$")

  skip_on_cran()
  expect_error(smtp_gmail(msg), NA)
})

test_that("message with just HTML", {
  msg <- msg %>%
    subject("MIME: HTML") %>%
    html("<p>Hello!</p>")

  txt <- as.character(msg)

  expect_no_match(txt, "multipart/alternative")
  expect_no_match(txt, "multipart/mixed")
  expect_match(txt, "<html><body><p>Hello!</p></body></html>$")

  skip_on_cran()
  expect_error(smtp_gmail(msg), NA)
})

test_that("message with text and HTML", {
  msg <- msg %>%
    subject("MIME: text+HTML") %>%
    text("Hello!") %>%
    html("<p>Hello!</p>")

  txt <- as.character(msg)

  expect_match(txt, "multipart/alternative")
  expect_no_match(txt, "multipart/mixed")
  expect_match(txt, "Hello!")
  expect_match(txt, "<html><body><p>Hello!</p></body></html>")

  skip_on_cran()
  expect_error(smtp_gmail(msg), NA)
})

test_that("message with just text and attachment", {
  msg <- msg %>%
    subject("MIME: text+attachment") %>%
    text("Hello!") %>%
    attachment(TXTPATH)

  txt <- as.character(msg)

  expect_no_match(txt, "multipart/alternative")
  expect_match(txt, "multipart/mixed")
  expect_match(txt, "Content-Type: +text/plain;")
  expect_match(txt, "Content-Disposition: +attachment;")
  expect_match(txt, "Hello!")

  skip_on_cran()
  expect_error(smtp_gmail(msg), NA)
})

test_that("message with just HTML and attachment", {
  msg <- msg %>%
    subject("MIME: HTML+attachment") %>%
    html("<p>Hello!</p>") %>%
    attachment(TXTPATH)

  txt <- as.character(msg)

  expect_no_match(txt, "multipart/alternative")
  expect_match(txt, "multipart/mixed")
  expect_match(txt, "Content-Type: +text/plain;")
  expect_match(txt, "Content-Disposition: +attachment;")
  expect_match(txt, "<html><body><p>Hello!</p></body></html>")

  skip_on_cran()
  expect_error(smtp_gmail(msg), NA)
})

test_that("message with text and HTML and attachment", {
  msg <- msg %>%
    subject("MIME: text+HTML+attachment") %>%
    text("Hello!") %>%
    html("<p>Hello!</p>") %>%
    attachment(TXTPATH)

  txt <- as.character(msg)

  expect_no_match(txt, "multipart/alternative")
  expect_match(txt, "multipart/mixed")
  expect_match(txt, "Content-Type: +text/plain;")
  expect_match(txt, "Content-Disposition: +attachment;")
  expect_match(txt, "Hello!")
  expect_match(txt, "<html><body><p>Hello!</p></body></html>")

  skip_on_cran()
  expect_error(smtp_gmail(msg), NA)
})
