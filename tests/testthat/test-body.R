test_that("Only single message body.", {
  expect_error(envelope() %>% text("foo"), NA)
  expect_error(envelope() %>% text(c("foo", "bar")))
  expect_error(envelope() %>% html("foo"), NA)
  expect_error(envelope() %>% html(c("foo", "bar")))
})
