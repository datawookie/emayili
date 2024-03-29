test_that("children must be MIME", {
  expect_error(MIME(children = list(1)), ERROR_NOT_MIME_OBJECT)
  expect_error(after(MIME(), 1), ERROR_NOT_MIME_OBJECT)
  expect_error(before(MIME(), 1), ERROR_NOT_MIME_OBJECT)
})

test_that("(ap|pre)pend children", {
  foo <- text_plain("foo")
  bar <- text_plain("bar")

  related <- MIME(children = foo)

  expect_equal(
    before(related, bar)$children[[1]]$content,
    "bar"
  )
  expect_equal(
    after(related, bar)$children[[2]]$content,
    "bar"
  )
})

test_that("length of MIME", {
  mime <- MIME()
  expect_equal(length(mime), 1)
})

test_that("create multipart/mixed", {
  expect_equal(class(multipart_mixed()), c("multipart_mixed", "MIME"))
})

test_that("convert single child to list", {
  expect_type(MIME(children = text_plain("Hello!"))$children, "list")
})

test_that("missing disposition", {
  mime_txt <- other(TXTPATH, disposition = NA)
  mime_jpg <- other(JPGPATH, disposition = NA)

  expect_match(mime_txt$type, "^text/plain")
  expect_match(mime_jpg$type, "^image/jpeg")

  expect_match(mime_txt$disposition, "^inline")
  expect_match(mime_jpg$disposition, "^attachment")
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

  expect_match(
    as.character.MIME(mime_txt),
    # nolint start
    "Content-Type: +text/plain;[[:space:]]+name=\"[^.]+\\.txt\"\r\nContent-Disposition: +inline;[[:space:]]+filename=\"[^.]+\\.txt\"\r\nContent-Transfer-Encoding: +base64\r\nX-Attachment-Id: +.*\nContent-ID: +<[^>]+>\r\n"
    # nolint end
  )
})

test_that("valid encoding", {
  expect_error(MIME(encoding = "klingon") %>% as.character.MIME())
})

test_that("base64 encoding & MD5 checksum", {
  mime_txt <- emayili:::other(TXTPATH, disposition = NA)

  expect_match(
    as.character.MIME(mime_txt),
    TXTCONTENT_ENCODED
  )
  expect_match(
    as.character.MIME(mime_txt),
    "Content-MD5: +7r/PnFfETLz0CFCESunVvA==",
    fixed = FALSE
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
  mime_jpg <- other(JPGPATH, disposition = NA)

  expect_match(
    as.character.MIME(mime_jpg),
    "Content-MD5: +0KTj0bnhRWCUK4N7LnvNmA=="
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

test_that("invalid hash algorithm", {
  expect_error(multipart_signed(micalg = "pgp-sha0"))
})
