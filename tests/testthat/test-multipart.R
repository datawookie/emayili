# test_that("message with just text", {
#   mime <- envelope() %>%
#     text("Hello!") %>%
#     as.character()
#
#   expect_no_match(mime, "multipart/alternative")
#   expect_no_match(mime, "multipart/mixed")
#   expect_match(mime, "Hello!$")
# })
#
# test_that("message with just HTML", {
#   mime <- envelope() %>%
#     html("<p>Hello!</p>") %>%
#     as.character()
#
#   expect_no_match(mime, "multipart/alternative")
#   expect_no_match(mime, "multipart/mixed")
#   expect_match(mime, "<html><body><p>Hello!</p></body></html>$")
# })
#
# test_that("message with text and HTML", {
#   mime <- envelope() %>%
#     text("Hello!") %>%
#     html("<p>Hello!</p>") %>%
#     as.character()
#
#   expect_match(mime, "multipart/alternative")
#   expect_no_match(mime, "multipart/mixed")
#   expect_match(mime, "Hello!")
#   expect_match(mime, "<html><body><p>Hello!</p></body></html>")
# })

test_that("message with just text and attachment", {
  mime <- envelope() %>%
    text("Hello!") %>%
    attachment(TXTPATH) %>%
    as.character()

  expect_no_match(mime, "multipart/alternative")
  expect_match(mime, "multipart/mixed")
  expect_match(mime, "Content-Type: +text/plain;")
  expect_match(mime, "Content-Disposition: +attachment;")
  expect_match(mime, "Hello!")
})

test_that("message with just HTML and attachment", {
  mime <- envelope() %>%
    html("<p>Hello!</p>") %>%
    attachment(TXTPATH) %>%
    as.character()

  expect_no_match(mime, "multipart/alternative")
  expect_match(mime, "multipart/mixed")
  expect_match(mime, "Content-Type: +text/plain;")
  expect_match(mime, "Content-Disposition: +attachment;")
  expect_match(mime, "<html><body><p>Hello!</p></body></html>")
})

test_that("message with text and HTML and attachment", {
  mime <- envelope() %>%
    text("Hello!") %>%
    html("<p>Hello!</p>") %>%
    attachment(TXTPATH) %>%
    as.character()

  expect_no_match(mime, "multipart/alternative")
  expect_match(mime, "multipart/mixed")
  expect_match(mime, "Content-Type: +text/plain;")
  expect_match(mime, "Content-Disposition: +attachment;")
  expect_match(mime, "Hello!")
  expect_match(mime, "<html><body><p>Hello!</p></body></html>")
})
