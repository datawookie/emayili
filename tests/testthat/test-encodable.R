test_that("encodable() is idempotent", {
  hans <- encodable(ADDRESS_HANS)
  expect_s3_class(hans, "encodable")
  hans <- encodable(hans)
  expect_s3_class(hans, "encodable")
})

test_that("encodable objects can be compared", {
  hans <- encodable(ADDRESS_HANS_DISPLAY)
  expect_true(hans == hans)
})
