test_that("generate parties table", {
  msg <- envelope() %>%
    to(address("gerry@gmail.com", "Gerald")) %>%
    from("jim@aol.com") %>%
    cc("alice@yahoo.com", "bob@yahoo.co.uk") %>%
    bcc("Erin <erin@yahoo.com>")

  addresses <- parties(msg)

  expect_type(addresses, "list")
  expect_equal(
    names(addresses),
    c("type", "address", "display", "raw", "local", "domain")
  )
})
