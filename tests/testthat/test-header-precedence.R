test_that("invalid levels", {
  expect_error(envelope %>% priority("none"), "Invalid")
  expect_error(envelope %>% importance("urgent"), "Invalid")
})

test_that("set priority", {
  expect_true(is.null(priority(envelope())))

  expect_equal(envelope() %>% priority("urgent") %>% priority(), "urgent")
  expect_equal(envelope(priority = "urgent") %>% priority(), "urgent")
})

test_that("set importance", {
  expect_true(is.null(importance(envelope())))

  expect_equal(envelope() %>% importance("high") %>% importance(), "high")
  expect_equal(envelope(importance = "high") %>% importance(), "high")
})
