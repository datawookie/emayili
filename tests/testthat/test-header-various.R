test_that("invalid levels", {
  expect_error(envelope %>% sensitivity("none"), "Invalid")
})

test_that("set sensitivity", {
  expect_true(is.null(priority(envelope())))

  expect_equal(envelope() %>% sensitivity("personal") %>% sensitivity(), "personal")
})

test_that("set expires & reply-by", {
  with_tz({
    expect_match(
      envelope() %>% expires("2030-01-01 13:25:00", "UTC") %>% expires(),
      "Tue, 01 Jan 2030 13:25:00 \\+0000 \\(UTC|GMT\\)"
    )
    expect_match(
      envelope() %>% replyby("2021-12-25 06:00:00", "GMT") %>% replyby(),
      "Sat, 25 Dec 2021 06:00:00 \\+0000 \\(UTC|GMT\\)"
    )
  })
})

test_that("in-reply-to & references", {
  expect_match(
    envelope() %>% inreplyto("<6163c08e.1c69fb81.65b78.183c@mx.google.com>") %>% as.character(),
    "In-Reply-To: +<6163c08e.1c69fb81.65b78.183c@mx.google.com>"
  )
  expect_match(
    envelope() %>%
      subject("Test") %>%
      inreplyto("<6163c08e.1c69fb81.65b78.183c@mx.google.com>", "AW: ") %>%
      as.character(),
    "Subject: +AW: Test"
  )
  expect_match(
    envelope() %>% references("<6163c08e.1c69fb81.65b78.183c@mx.google.com>") %>% as.character(),
    "References: +<6163c08e.1c69fb81.65b78.183c@mx.google.com>"
  )
  expect_match(
    envelope() %>%
      subject("Test") %>%
      references("<6163c08e.1c69fb81.65b78.183c@mx.google.com>", "AW: ") %>%
      as.character(),
    "Subject: +AW: Test"
  )
})

test_that("return-path", {
  expect_equal(
    envelope() %>% return_path("bob@gmail.com") %>% return_path(),
    address("bob@gmail.com")
  )
})

test_that("comments", {
  msg <- envelope() %>% comments("Test message")
  expect_equal(comments(msg), "Test message")

  expect_match(
    envelope() %>% comments("Test comments") %>% as.character(),
    "Test comments"
  )
})

test_that("keywords", {
  msg <- envelope() %>% keywords("Test keyword")
  expect_equal(keywords(msg), "Test keyword")

  expect_match(
    envelope() %>% keywords("Test keyword") %>% as.character(),
    "Test keyword"
  )

  msg <- envelope() %>% keywords("Test keyword1, Test keyword2")
  expect_equal(keywords(msg), "Test keyword1, Test keyword2")

  expect_match(
    envelope() %>% keywords("Test keyword1, Test keyword2") %>% as.character(),
    "Test keyword1,[[:space:]]+Test keyword2"
  )

  msg <- envelope() %>% keywords(c("Test keyword1, Test keyword2"))
  expect_equal(keywords(msg), "Test keyword1, Test keyword2")

  expect_match(
    envelope() %>% keywords(c("Test keyword1, Test keyword2")) %>% as.character(),
    "Test keyword1,[[:space:]]+Test keyword2"
  )
})
