test_that("compliant", {
  expect_true(compliant("alice@example.com"))
  expect_true(compliant("al.ice@example.com"))
})

test_that("not compliant", {
  expect_false(compliant("alice?example.com"))
  # More than one consecutive ".".
  expect_false(compliant("al..ice@example.com"))
  # Start or end with ".".
  expect_false(compliant(".alice@example.com"))
  expect_false(compliant("alice.@example.com"))
  expect_false(compliant("alice.@example.com"))
  expect_false(compliant("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX@gmail.com"))
  expect_false(compliant("al ice@example.com"))
  # Missing '@'.
  expect_false(compliant("alice.yahoo.com"))
})

test_that("not compliant raises error", {
  expect_error(compliant("alice?example.com", error = TRUE))
})
