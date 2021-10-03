test_that("all header fields", {
  msg <- envelope() %>%
    subject("Test message") %>%
    to("frank@yahoo.co.uk") %>%
    cc("bob@gmail.com") %>%
    bcc("alice@yahoo.com") %>%
    from("olivia&google.com") %>%
    sender("olivia@gov.uk")
  expect_match(headers(msg), "Date: +.*\r\nX-Mailer: +\\{emayili\\}-[0-9]+\\.[0-9]+\\.[0-9]+\r\nMIME-Version: +1\\.0\r\nSubject: +Test message\r\nTo: +frank@yahoo.co.uk\r\nCc: +bob@gmail.com\r\nBcc: +alice@yahoo.com\r\nFrom: +olivia&google.com\r\nSender: +olivia@gov.uk")
})
