test_that("header fields", {
  msg <- envelope() %>%
    subject("Test message") %>%
    cc("bob@gmail.com") %>%
    bcc("alice@yahoo.com")
  expect_match(emayili:::header(msg), "Date: +.*\nSubject: +Test message\nCc: +bob@gmail.com\nBcc: +alice@yahoo.com")
})
