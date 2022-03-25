test_that("subject: set/get", {
  msg <- envelope() %>% subject("Test message")
  expect_equal(subject(msg), encodable("Test message"))
})

test_that("interpolate", {
  name <- "Alice"
  variables <- list(name = name)

  expect_equal(
    envelope() %>%
      subject("Hello {{name}}!") %>%
      subject(),
    encodable("Hello Alice!")
  )
  expect_equal(
    envelope() %>%
      subject("Hello {{name}}!", .envir = variables) %>%
      subject(),
    encodable("Hello Alice!")
  )
})
