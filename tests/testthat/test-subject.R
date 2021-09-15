test_that("subject: set/get", {
  msg <- envelope() %>% subject("Test message")
  expect_equal(subject(msg), "Test message")
})

test_that("interpolate", {
  name <- "Alice"
  msg <- envelope() %>% subject("Hello {{name}}!")

  expect_equal(subject(msg), "Hello Alice!")
})
