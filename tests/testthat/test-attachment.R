test_that("attachment: set", {
  msg <- envelope() %>% attachment(TXTPATH)

  expect_equal(msg$parts[[1]]$content, base64encode(TXTPATH, 76L, "\r\n"))
})

test_that("attachment: specify CID", {
  cid <- "histogram"

  msg <- envelope() %>% attachment(PNGPATH, cid = cid)

  expect_equal(msg$parts[[1]]$content, base64encode(PNGPATH, 76L, "\r\n"))
  expect_equal(msg$parts[[1]]$cid, cid)
})

test_that("attachment: number of files", {
  msg <- envelope()

  expect_error(msg %>% attachment())
  expect_error(msg %>% attachment(c(TXTPATH, PNGPATH)))
})

