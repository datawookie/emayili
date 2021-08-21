test_that("remove comments", {
  expect_equal(sanitise("alice@yahoo.com(comment)"), "alice@yahoo.com")
  expect_equal(sanitise("alice(comment)@yahoo.com"), "alice@yahoo.com")
})
