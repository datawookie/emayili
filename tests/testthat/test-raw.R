test_that("returns raw email address", {
  expect_equal(
    raw(as.address(c("Gerald <gerry@gmail.com>", "gerry@gmail.com"))),
    rep("gerry@gmail.com", 2)
  )
})
