test_that("returns display name", {
  expect_equal(display(as.address("Gerald   <gerry@gmail.com>")), "Gerald")
})

test_that("returns NA when no display name present", {
  expect_equal(display(as.address("gerry@gmail.com")), NA_character_)
})
