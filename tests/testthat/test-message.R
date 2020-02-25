test_that("header fields", {
  msg <- envelope() %>%
    subject("Test message") %>%
    cc("bob@gmail.com") %>%
    bcc("alice@yahoo.com")
  expect_match(emayili:::header(msg), "Date: +.*\r\nSubject: +Test message\r\nCc: +bob@gmail.com\r\nBcc: +alice@yahoo.com")
})
