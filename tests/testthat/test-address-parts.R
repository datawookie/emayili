test_that("returns local part of email address", {
  expect_equal(local("alice@example.com"), "alice")
})

test_that("returns domain of email address", {
  expect_equal(domain("alice@example.com"), "example.com")
})
