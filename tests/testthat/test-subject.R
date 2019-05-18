test_that("subject: set", {
  msg <- envelope() %>% subject("Test message")
  expect_equal(msg$header$Subject, "Test message")
})
