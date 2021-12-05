test_that("print header", {
  expect_output(
    print(header("Header", "value")),
    "Header: +value"
  )
})

test_that("header append", {
  # Don't append.
  expect_equal(
    envelope() %>%
      header_set("Header", "first") %>%
      header_set("Header", "second") %>%
      header_get("Header"),
    "second"
  )
  # Do append.
  expect_equal(
    envelope() %>%
      header_set("Header", "first") %>%
      header_set("Header", "second", append = TRUE) %>%
      header_get("Header"),
    c("first", "second")
  )
})
