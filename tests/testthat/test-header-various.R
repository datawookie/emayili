test_that("invalid levels", {
  expect_error(envelope %>% sensitivity("none"), "Invalid")
})

test_that("set sensitivity", {
  expect_true(is.null(priority(envelope())))

  expect_equal(envelope() %>% sensitivity("personal") %>% sensitivity(), "personal")
})

test_that("set expires & reply-by", {
  expect_true(is.null(priority(envelope())))

  expect_equal(envelope() %>% expires("2030-01-01 13:25:00", "UTC") %>% expires(), "Tue, 01 Jan 2030 13:25:00 +0000 (GMT)")
  expect_equal(envelope() %>% replyby("2021-12-25 06:00:00", "GMT") %>% replyby(), "Sat, 25 Dec 2021 06:00:00 +0000 (GMT)")
})
