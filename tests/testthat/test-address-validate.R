test_that("validate: syntax", {
  expect_false(validate("alice?example.com"))
  expect_true(validate("alice@example.com"))
})

test_that("validate: deliverability", {
  expect_false(validate("alice@not-a-valid-example.com"))
  expect_true(validate("alice@not-a-valid-example.com", deliverability = FALSE))
})

test_that("validate: from address", {
  expect_s3_class(address("alice@not-a-valid-example.com"), "address")
  expect_error(address("alice@not-a-valid-example.com", validate = TRUE))
})
