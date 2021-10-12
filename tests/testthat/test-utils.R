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

test_that("enclose angle brackets", {
  expect_equal(wrap_angle_brackets("<foo"), "<foo>")
  expect_equal(wrap_angle_brackets("foo>"), "<foo>")
  expect_equal(wrap_angle_brackets("<foo>"), "<foo>")
  expect_equal(wrap_angle_brackets("foo"), "<foo>")
})

test_that("smtp server URL", {
  expect_equal(smtp_url("smtp.gmail.com", 465, protocol = NA), "smtps://smtp.gmail.com:465/")
  expect_equal(smtp_url("smtp.gmail.com", 587, protocol = NA), "smtp://smtp.gmail.com:587/")
  expect_equal(smtp_url("smtp.gmail.com", 465, protocol = "smtp"), "smtp://smtp.gmail.com:465/")
  expect_equal(smtp_url("smtp.gmail.com", 587, protocol = "smtps"), "smtps://smtp.gmail.com:587/")
})
