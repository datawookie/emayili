test_that("create address", {
  expect_equal(
    address(
      c("gerry@gmail.com", "alice@yahoo.com", "jim@aol.com", NA),
      c("Gerald", "Alice", NA, "Bob")
    ),
    c(
      address("gerry@gmail.com", "Gerald"),
      address("alice@yahoo.com", "Alice"),
      address("jim@aol.com"),
      address(NA, "Bob")
    )
  )
})

test_that("convert to character", {
  expect_equal(as.character(address("gerry@gmail.com")), "gerry@gmail.com")
  expect_equal(as.character( address("gerry@gmail.com", "Gerald")), "Gerald <gerry@gmail.com>")
})

test_that("normalise", {
  expect_equal(
    as.character(address("    gerry@gmail.com    ", normalise = FALSE)),
    "    gerry@gmail.com    "
  )
  expect_equal(as.character(address("    gerry@gmail.com    ")), "gerry@gmail.com")
  expect_equal(
    as.character( address("    gerry@gmail.com    ", "      Gerald    ")),
    "Gerald <gerry@gmail.com>"
  )
})

test_that("parse address", {
  expect_equal(as.address("     Gerald    <   gerry@gmail.com  >   "), address("gerry@gmail.com", "Gerald"))
})

test_that("normalise", {
  expect_equal(as.address("     Gerald    <   gerry@gmail.com    >"), address("gerry@gmail.com", "Gerald"))
})
