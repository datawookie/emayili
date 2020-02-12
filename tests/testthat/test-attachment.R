test_that("attachment: set", {
  msg <- envelope() %>% attachment(TXTPATH)

  expect_equal(msg$parts[[1]]$body, base64encode(TXTPATH, 76L, "\r\n"))
})

test_that("attachment: specify CID", {
  cid <- "histogram"

  msg <- envelope() %>% attachment(PNGPATH, cid = cid)

  expect_equal(msg$parts[[1]]$body, base64encode(PNGPATH, 76L, "\r\n"))
  expect_equal(msg$parts[[1]]$header$cid, cid)
})
