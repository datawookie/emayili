test_that("subject: set/get", {
  msg <- envelope() %>% subject("Test message")
  expect_equal(subject(msg), "Test message")
})
