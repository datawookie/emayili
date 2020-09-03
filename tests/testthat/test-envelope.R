test_that("envelope print() output", {
  msg <- envelope() %>% subject("Test message")
  expect_output(print(msg), "Date:         .*\nSubject:      Test message")
})

test_that("class envelope", {
  object <- envelope()
  expect_equal(class(object), "envelope")
})

test_that("recipient address", {
  recipient <- envelope(to = "bob@gmail.com")
  expect_equal(recipient$header$To, "bob@gmail.com")
})

test_that("sender address", {
  sender <- envelope(from = "bob@gmail.com")
  expect_equal(sender$header$From, "bob@gmail.com")
})

test_that("subject", {
  subject <- envelope(subject = "Email Subject")
  expect_equal(subject$header$Subject, "Email Subject")
})

test_that("body text", {
  email_text <- envelope(text = "foo")
  expect_equal(email_text$parts[[1]]$body, "foo")
})

test_that("body html", {
  html <- envelope(html = "foo")
  expect_equal(html$parts[[1]]$body, "foo")
})


