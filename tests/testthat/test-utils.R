test_that("drape line feeds", {
  expect_equal(drape_linefeed("foo\nbar"), "foo\r\nbar")
  expect_equal(drape_linefeed("foo\r\nbar"), "foo\r\nbar")
  expect_equal(drape_linefeed("foo\n\n\nbar"), "foo\r\n\r\n\r\nbar")
})
