test_that("children must be MIME", {
  expect_error(MIME(children = list(1)), ERROR_NOT_MIME_OBJECT)
  expect_error(append.MIME(MIME(), 1), ERROR_NOT_MIME_OBJECT)
  expect_error(prepend.MIME(MIME(), 1), ERROR_NOT_MIME_OBJECT)
})

test_that("create multipart/mixed", {
  expect_equal(class(multipart_mixed()), c("multipart_mixed", "MIME"))
})

test_that("convert single child to list", {
  expect_type(MIME(children = text_plain("Hello!"))$children, "list")
})

test_that("missing disposition", {
  mime_txt <- other(TXTPATH, disposition = NA)
  mime_png <- other(PNGPATH, disposition = NA)

  expect_match(mime_txt$type, "^text/plain")
  expect_match(mime_png$type, "^image/png")

  expect_match(mime_txt$disposition, "^inline")
  expect_match(mime_png$disposition, "^attachment")
})

test_that("print", {
  mime_txt <- emayili:::other(TXTPATH, disposition = NA)

  expect_output(
    print.MIME(mime_txt),
    as.character.MIME(mime_txt) %>% str_replace_all("\r\n", "\n"),
    fixed = TRUE
  )
})

test_that("squish", {
  expect_match(
    text_html(
      "   <div>   <p>Hello!</p>   </div>   ",
      squish = TRUE
    )$content,
    "<div><p>Hello!</p></div>"
  )
})

test_that("header fields", {
  mime_txt <- other(TXTPATH, disposition = NA)

  expect_match(as.character.MIME(mime_txt), "Content-Type:              text/plain; name=\"[^.]+\\.txt\"\r\nContent-Disposition:       inline; filename=\"[^.]+\\.txt\"\r\nContent-Transfer-Encoding: base64\r\nX-Attachment-Id:           .+\nContent-ID:                <[^>]+>\r\n")
})

test_that("base64 encoding & MD5 checksum", {
  mime_txt <- emayili:::other(TXTPATH, disposition = NA)

  expect_match(
    as.character.MIME(mime_txt),
    TXTCONTENT_ENCODED
  )
  expect_match(
    as.character.MIME(mime_txt),
    "Content-MD5:               XrY7u+Ae7tCTyyK7j1rNww==",
    fixed = TRUE
  )
})

test_that("order doesn't matter", {
  expect_error(
    envelope() %>% text("Hello!") %>% attachment(TXTPATH) %>% as.character(),
    NA
  )
  expect_error(
    envelope() %>% attachment(TXTPATH) %>% text("Hello!") %>% as.character(),
    NA
  )
})

test_that("MD5 checksum", {
  mime_png <- other(PNGPATH, disposition = NA)

  expect_match(
    as.character.MIME(mime_png),
    "Content-MD5:               8SV0BThEP3geDkvudZFKEA==",
    fixed = TRUE
  )
})

test_that("order doesn't matter", {
  expect_error(
    envelope() %>% text("Hello!") %>% attachment(TXTPATH) %>% as.character(),
    NA
  )
  expect_error(
    envelope() %>% attachment(TXTPATH) %>% text("Hello!") %>% as.character(),
    NA
  )
})
