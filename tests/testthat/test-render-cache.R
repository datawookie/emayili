object_id <- function(x) sub(" .*", "", capture.output(.Internal(inspect(x)))[1])

test_that("cache same render", {
  msg <- envelope()

  name <- "Bob"
  # These two will be precisely the same (because of caching).
  x1 <- msg %>% render(PLAIN_MARKDOWN_INTERPOLATE)
  x2 <- msg %>% render(PLAIN_MARKDOWN_INTERPOLATE)
  name <- "Alice"
  # This one will be different.
  y <- msg %>% render(PLAIN_MARKDOWN_INTERPOLATE)

  expect_true(object_id(x1$parts[[1]]) == object_id(x2$parts[[1]]))
  expect_false(object_id(x1$parts[[1]]) == object_id(y$parts[[1]]))
  expect_false(object_id(x2$parts[[1]]) == object_id(y$parts[[1]]))
})
