test_that("envelope print() output", {
  msg <- envelope() %>% subject("Test message")
  expect_output(print(msg), "Date:         .*\nSubject:                   Test message")
})

test_that("class envelope", {
  object <- envelope()
  expect_equal(class(object), "envelope")
})

test_that("recipient address", {
  recipient <- envelope(to = "bob@gmail.com")
  expect_equal(recipient$header$To, address("bob@gmail.com"))
})

test_that("sender address", {
  sender <- envelope(from = "bob@gmail.com")
  expect_equal(sender$header$From, address("bob@gmail.com"))
})

test_that("cc", {
  cc <- envelope(cc = "bob@gmail.com")
  expect_equal(cc$header$Cc, address("bob@gmail.com"))
})

test_that("bcc", {
  bcc <- envelope(bcc = "bob@gmail.com")
  expect_equal(bcc$header$Bcc, address("bob@gmail.com"))
})

test_that("reply to", {
  reply <- envelope(reply = "bob@gmail.com")
  expect_equal(reply$header$Reply_To, address("bob@gmail.com"))
})

test_that("subject", {
  subject <- envelope(subject = "Email Subject")
  expect_equal(subject$header$Subject, "Email Subject")
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
