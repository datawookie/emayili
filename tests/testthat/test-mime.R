test_that("children must be MIME", {
  expect_error(MIME(children = list(1)), ERROR_NOT_MIME_OBJECT)
  expect_error(append.MIME(emayili:::MIME(), 1), ERROR_NOT_MIME_OBJECT)
  expect_error(prepend.MIME(emayili:::MIME(), 1), ERROR_NOT_MIME_OBJECT)
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
  mime_txt <- other(TXTPATH, disposition = NA)

  expect_output(print(mime_txt), as.character(mime_txt) %>% str_replace_all("\r\n", "\n"))
})

test_that("squish", {
  expect_match(
    emayili:::text_html(
      "   <div>   <p>Hello!</p>   </div>   ",
      squish = TRUE
    )$content,
    "<div><p>Hello!</p></div>"
  )
})
