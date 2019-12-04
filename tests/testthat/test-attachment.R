# write.csv(mtcars, here::here("tests", "testthat", "mtcars.csv"))
#
# png(file=here::here("tests", "testthat", "mtcars-disp-hist.png"),
#     width=600, height=350)
# hist(mtcars$disp)
# dev.off()

test_that("attachment: set", {
  csv <- "./mtcars.csv"

  msg <- envelope() %>% attachment(csv)

  expect_equal(msg$parts[[1]]$body, base64encode(csv, 76L, "\r\n"))
})

test_that("attachment: specify CID", {
  png <- "./mtcars-disp-hist.png"
  cid <- "histogram"

  msg <- envelope() %>% attachment(png, cid = cid)

  expect_equal(msg$parts[[1]]$body, base64encode(png, 76L, "\r\n"))
  expect_equal(msg$parts[[1]]$header$cid, cid)
})
