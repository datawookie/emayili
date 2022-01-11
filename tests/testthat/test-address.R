test_that("create address", {
  addr <- address(
    c("gerry@gmail.com", "alice@yahoo.com", "jim@aol.com"),
    c("Gerald", "Alice", NA)
  )

  expect_equal(
    as.character(addr),
    c("Gerald <gerry@gmail.com>", "Alice <alice@yahoo.com>", "jim@aol.com")
  )
})

test_that("create address from local and domain", {
  expect_equal(
    address(local = c("alice", "erin"), domain = "yahoo.com"),
    address(c("alice@yahoo.com", "erin@yahoo.com"))
  )
})

test_that("unable to recycle", {
  expect_error(
    address(
      email = c("bob@gmail.com", "alice@yahoo.com"),
      display = c("Bob", "Alice", "Jim")
    )
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
    as.character(address("    gerry  @ gmail.com    ", "      Gerald    ")),
    "Gerald <gerry@gmail.com>"
  )
})

test_that("parse address", {
  expect_equal(as.address("     Gerald    <   gerry@gmail.com  >   "), address("gerry@gmail.com", "Gerald"))
})

test_that("normalise", {
  expect_equal(as.address("     Gerald    <   gerry@gmail.com    >"), address("gerry@gmail.com", "Gerald"))
})

test_that("print address", {
  expect_output(print(address("alice@yahoo.com", "Alice")), "Alice <alice@yahoo.com>")
})

test_that("address operators", {
  expect_true(address("alice@yahoo.com", "Alice") == address("alice@yahoo.com", "Alice"))
  expect_true(address("alice@yahoo.com", "Alice") == "Alice <alice@yahoo.com>")
  expect_true("Alice <alice@yahoo.com>" == address("alice@yahoo.com", "Alice"))
  expect_true(address("alice@yahoo.com", "Alice") == address("alice@yahoo.com", "Gerald"))
  expect_true(address("alice@yahoo.com", "Alice") != address("bob@gmail.com", "Bob"))
  expect_true(address("alice@yahoo.com", "Alice") != address("gerry@gmail.com", "Alice"))
  #
  # Undefined operation.
  #
  expect_error(address("alice@yahoo.com", "Alice") / address("gerry@gmail.com", "Alice"))
})

test_that("split address list", {
  addr_list <- address(
    c("gerry@gmail.com", "alice@yahoo.com", "jim@aol.com"),
    c("Gerald", NA, NA)
  )

  expect_equal(
    as.address(c("Gerald <gerry@gmail.com>", "alice@yahoo.com", "jim@aol.com")),
    addr_list
  )
  expect_equal(
    as.address("Gerald <gerry@gmail.com>, alice@yahoo.com, jim@aol.com"),
    addr_list
  )
  expect_equal(
    as.address(c("Gerald <gerry@gmail.com>", "alice@yahoo.com, jim@aol.com")),
    addr_list
  )
})
