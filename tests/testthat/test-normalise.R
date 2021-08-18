test_that("strip spaces", {
  expect_equal(normalise(as.address("     Gerald    <   gerry@gmail.com    >")), "Gerald <gerry@gmail.com>")
})
