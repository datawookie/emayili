test_that("read receipt", {
  expect_error(envelope() %>% request_receipt_read())

  msg <- envelope() %>%
    from("olivia@google.com") %>%
    request_receipt_read()

  expect_match(headers(msg), "Disposition-Notification-To: +olivia@google.com")
  expect_match(headers(msg), "X-Confirm-Reading-To: +olivia@google.com")
})

test_that("delivery receipt", {
  expect_error(envelope() %>% request_receipt_delivery())

  msg <- envelope() %>%
    from("olivia@google.com") %>%
    request_receipt_delivery()

  expect_match(headers(msg), "Return-Receipt-To: +olivia@google.com")
})
