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
  expect_equal(recipient$headers$To$values, address("bob@gmail.com"))
})

test_that("sender address", {
  sender <- envelope(from = "bob@gmail.com")
  expect_equal(sender$headers$From$values, address("bob@gmail.com"))
})

test_that("maximum one sender address", {
  expect_error(envelope(from = c("bob@gmail.com", "anne@example.com")))
  expect_error(envelope() %>% from(c("bob@gmail.com", "anne@example.com")))
})

test_that("cc", {
  cc <- envelope(cc = "bob@gmail.com")
  expect_equal(cc$headers$Cc$values, address("bob@gmail.com"))
})

test_that("bcc", {
  bcc <- envelope(bcc = "bob@gmail.com")
  expect_equal(bcc$headers$Bcc$values, address("bob@gmail.com"))
})

test_that("reply to", {
  reply <- envelope(reply = "bob@gmail.com")
  expect_equal(reply$header[["Reply-To"]]$values, address("bob@gmail.com"))
})

test_that("subject", {
  subject <- envelope(subject = "Email Subject")
  expect_equal(subject$header$Subject$values %>% as.character(), "Email Subject")
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
  msg <- envelope() %>%
    text("Hello!") %>%
    html("<p>Goodbye!</p>")
  expect_equal(length(msg$parts), 2)
})

test_that("parts are not nested", {
  msg <- envelope() %>%
    text("Hello!") %>%
    html(HTMLPATH) %>%
    attachment(JPGPATH)
  expect_false(is.nested(msg$parts))
})

test_that("not splitting addresses", {
  msg <- envelope(
    to = c("Durrell, Gerald <gerry@gmail.com>", "Jones, Bob <bob@yahoo.com>"),
    cc = c("Durrell, Gerald <gerry@gmail.com>", "Jones, Bob <bob@yahoo.com>"),
    bcc = c("Durrell, Gerald <gerry@gmail.com>", "Jones, Bob <bob@yahoo.com>")
  )

  expect_match(
    headers(msg),
    "To:[[:space:]]+Durrell, Gerald <gerry@gmail.com>,\r\n[[:space:]]+Jones, Bob <bob@yahoo.com>"
  )
  expect_match(
    headers(msg),
    "Cc:[[:space:]]+Durrell, Gerald <gerry@gmail.com>,\r\n[[:space:]]+Jones, Bob <bob@yahoo.com>"
  )
  expect_no_match(
    headers(msg),
    "Bcc:[[:space:]]+Durrell, Gerald <gerry@gmail.com>,\r\n[[:space:]]+Jones, Bob <bob@yahoo.com>"
  )
})
