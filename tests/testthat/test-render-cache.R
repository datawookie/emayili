address <- function(x) sub(" .*", "", capture.output(.Internal(inspect(x)))[1])

test_that("cache same render", {
  msg <- envelope()

  name <- "Bob"
  x1 <- msg %>% render(PLAIN_MARKDOWN_INTERPOLATE)
  x2 <- msg %>% render(PLAIN_MARKDOWN_INTERPOLATE)
  name <- "Alice"
  y <- msg %>% render(PLAIN_MARKDOWN_INTERPOLATE)

  expect_true(address(x1$parts[[1]]) == address(x2$parts[[1]]))
  expect_false(address(x1$parts[[1]]) == address(y$parts[[1]]))
  expect_false(address(x2$parts[[1]]) == address(y$parts[[1]]))
})
