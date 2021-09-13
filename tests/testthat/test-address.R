test_that("create address", {
  expect_equal(
    address(
      c("gerry@gmail.com", "alice@yahoo.com", "jim@aol.com"),
      c("Gerald", "Alice", NA)
    ),
    c(
      address("gerry@gmail.com", "Gerald"),
      address("alice@yahoo.com", "Alice"),
      address("jim@aol.com")
    )
  )
})

test_that("create address from local and domain", {
  expect_equal(
    address(local = c("alice", "erin"), domain = "yahoo.com"),
    address(c("alice@yahoo.com", "erin@yahoo.com"))
  )
})

test_that("mandatory arguments", {
  expect_error(address())
  expect_error(address(NA, "Bob"))
  expect_error(address(local = "alice"))
  expect_error(address(domain = "yahoo.com"))
  expect_error(address("alice@yahoo.com", local = "alice"))
  expect_error(address("alice@yahoo.com", domain = "yahoo.com"))
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
  expect_equal(as.character(address("    gerry  @ gmail.com    ")), "gerry@gmail.com")
  expect_equal(
    as.character( address("    gerry  @ gmail.com    ", "      Gerald    ")),
    "Gerald <gerry@gmail.com>"
  )
})

test_that("parse address", {
  expect_equal(as.address("     Gerald    <   gerry@gmail.com  >   "), address("gerry@gmail.com", "Gerald"))
})

test_that("normalise", {
  expect_equal(as.address("     Gerald    <   gerry@gmail.com    >"), address("gerry@gmail.com", "Gerald"))
})

test_that("full type of vector", {
  expect_equal(vec_ptype_full(address("alice@yahoo.com", "Alice")), "address")
})

test_that("abbreviated type of vector", {
  expect_equal(vec_ptype_abbr(address("alice@yahoo.com", "Alice")), "addr")
})

test_that("print address", {
  expect_output(print(address("alice@yahoo.com", "Alice")), "Alice <alice@yahoo.com>")
})

test_that("address operators", {
  expect_true(address("alice@yahoo.com", "Alice") == address("alice@yahoo.com", "Alice"))
  expect_true(address("alice@yahoo.com", "Alice") == "Alice <alice@yahoo.com>")
  expect_true(address("alice@yahoo.com", "Alice") != address("bob@gmail.com", "Bob"))
  expect_true(address("alice@yahoo.com", "Alice") != address("alice@yahoo.com", "Gerald"))
  expect_true(address("alice@yahoo.com", "Alice") != address("gerry@gmail.com", "Alice"))
  #
  # Undefined operation.
  #
  expect_error(address("alice@yahoo.com", "Alice") / address("gerry@gmail.com", "Alice"))
})
