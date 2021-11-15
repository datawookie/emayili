test_that("envelope print() output", {
  msg <- envelope() %>% subject("Test message")
  expect_output(print(msg), "Date: +.*\nSubject: +Test message")
})

test_that("class envelope", {
  object <- envelope()
  expect_equal(class(object), "envelope")
})

test_that("recipient address", {
  recipient <- envelope(to = "bob@gmail.com")
  expect_equal(recipient$headers$To$values[[1]], address("bob@gmail.com"))
})

test_that("sender address", {
  sender <- envelope(from = "bob@gmail.com")
  expect_equal(sender$headers$From$values[[1]], address("bob@gmail.com"))
})

test_that("cc", {
  cc <- envelope(cc = "bob@gmail.com")
  expect_equal(cc$headers$Cc$values[[1]], address("bob@gmail.com"))
})

test_that("bcc", {
  bcc <- envelope(bcc = "bob@gmail.com")
  expect_equal(bcc$headers$Bcc$values[[1]], address("bob@gmail.com"))
})

test_that("reply to", {
  reply <- envelope(reply = "bob@gmail.com")
  expect_equal(reply$header[["Reply-To"]]$values[[1]], address("bob@gmail.com"))
})

test_that("subject", {
  subject <- envelope(subject = "Email Subject")
  expect_equal(subject$header$Subject$values[[1]], "Email Subject")
})

test_that("body text", {
  email_text <- envelope(text = "foo")
  expect_equal(email_text$parts[[1]]$content, "foo")
})

test_that("body html", {
  html <- envelope(html = "<p>foo</p>")
  expect_match(html$parts[[1]]$content, "<body><p>foo</p></body>")
})

test_that("append another body", {
  msg <- envelope() %>% text("Hello!") %>% html("<p>Goodbye!</p>")
  expect_equal(length(msg$parts), 2)
})

test_that("parts are not nested", {
  msg <- envelope() %>%
    text("Hello!") %>%
    html(HTMLPATH) %>%
    attachment(JPGPATH)
  expect_false(is.nested(msg$parts))
})
