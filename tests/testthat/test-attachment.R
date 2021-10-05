test_that("attachment: set", {
  msg <- envelope() %>% attachment(TXTPATH)

  expect_match(as.character(msg), TXTCONTENT_ENCODED)
})

test_that("attachment: specify CID", {
  cid <- "histogram"

  msg <- envelope() %>% attachment(JPGPATH, cid = cid)

  expect_true(
    grepl(
      mime_base64encode(JPGPATH),
      as.character(msg),
      fixed = TRUE
    )
  )
  expect_equal(msg$parts$cid, cid)
})

test_that("attachment: number of files", {
  msg <- envelope()

  expect_error(msg %>% attachment())
  expect_error(msg %>% attachment(c(TXTPATH, JPGPATH)))
})

