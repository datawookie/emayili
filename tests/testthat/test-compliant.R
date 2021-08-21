test_that("compliant", {
  expect_true(compliant("alice@example.com"))
  expect_true(compliant("al.ice@example.com"))
})
test_that("not compliant", {
  expect_false(compliant("alice?example.com"))
  expect_false(compliant("al..ice@example.com"))
  expect_false(compliant(".alice@example.com"))
  expect_false(compliant("alice.@example.com"))
  expect_false(compliant("alice.@example.com"))
  expect_false(compliant("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX@gmail.com"))
  expect_false(compliant("al ice@example.com"))
})
