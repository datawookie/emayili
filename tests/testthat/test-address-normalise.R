test_that("normalise: remove comments", {
  expect_equal(normalise("alice@yahoo.com(comment)"), "alice@yahoo.com")
  expect_equal(normalise("alice(comment)@yahoo.com"), "alice@yahoo.com")
})

test_that("normalise: remove spaces around @", {
  expect_equal(normalise("alice  @     yahoo.com"), "alice@yahoo.com")
})

test_that("normalise: domain to lowercase", {
  expect_equal(normalise("bob@GMAIL.COM"), "bob@gmail.com")
})

test_that("normalise: Unicode character equivalents", {
  expect_equal(normalise("bob@ｇｍａｉｌ.com"), "bob@gmail.com")
})
