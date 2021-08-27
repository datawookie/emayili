test_that("all header fields", {
  msg <- envelope() %>%
    subject("Test message") %>%
    to("frank@yahoo.co.uk") %>%
    cc("bob@gmail.com") %>%
    bcc("alice@yahoo.com") %>%
    from("olivia&google.com") %>%
    sender("olivia@gov.uk")
  expect_match(emayili:::header(msg), "Date: +.*\r\nSubject: +Test message\r\nTo: +frank@yahoo.co.uk\r\nCc: +bob@gmail.com\r\nBcc: +alice@yahoo.com\r\nFrom: +olivia&google.com\r\nSender: +olivia@gov.uk\r\nX-Mailer: +\\{emayili\\}-[0-9]+\\.[0-9]+\\.[0-9]+")
})
