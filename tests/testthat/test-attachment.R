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
  expect_equal(msg$parts[[1]]$cid, cid)
})

test_that("attachment: number of files", {
  msg <- envelope()

  expect_error(msg %>% attachment())
  expect_error(msg %>% attachment(c(TXTPATH, JPGPATH)))
})
