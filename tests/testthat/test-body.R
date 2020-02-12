test_that("text: only single message body", {
  expect_error(envelope() %>% text("foo"), NA)
  expect_error(envelope() %>% text(c("foo", "bar")))
})

test_that("html: only single message body", {
  expect_error(envelope() %>% html("foo"), NA)
  expect_error(envelope() %>% html(c("foo", "bar")))
})
