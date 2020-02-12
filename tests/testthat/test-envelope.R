test_that("envelope print() output", {
  msg <- envelope() %>% subject("Test message")
  expect_output(print(msg), "Date:         .*\nSubject:      Test message")
})
