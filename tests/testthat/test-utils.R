test_that("drape line feeds", {
  expect_equal(drape_linefeed("foo\nbar"), "foo\r\nbar")
  expect_equal(drape_linefeed("foo\r\nbar"), "foo\r\nbar")
  expect_equal(drape_linefeed("foo\n\n\nbar"), "foo\r\n\r\n\r\nbar")
})

test_that("remove CSS comments", {
  expect_equal(css_remove_comment("/*! * Bootstrap v3.3.5 */"), "")
  expect_equal(css_remove_comment("/* * Bootstrap v3.3.5 */"), "")
  expect_equal(
    css_remove_comment("/* * Bootstrap v3.3.5 */ p {} /*! normalize.css */"),
    " p {} "
  )
})
