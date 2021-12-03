skip_if_not_installed("gpg")

test_that("sign/encrypt empty message", {
  msg <- envelope(
    to = "alice@yahoo.com",
    from = "bob@gmail.com",
    encrypt = TRUE,
    sign = TRUE
  )

  expect_error(as.character(msg))
})

test_that("sign", {
  msg <- envelope(
    to = "alice@yahoo.com",
    from = "bob@gmail.com",
    sign = TRUE
  ) %>% text(TXTCONTENT)

  expect_match(as.character(msg), "-----BEGIN PGP SIGNATURE-----")
  expect_match(as.character(msg), "-----END PGP SIGNATURE-----")

  expect_no_match(as.character(msg), "-----BEGIN PGP MESSAGE-----")
  expect_no_match(as.character(msg), "-----END PGP MESSAGE-----")
  expect_no_match(as.character(msg), "-----BEGIN PGP PUBLIC KEY BLOCK-----")
  expect_no_match(as.character(msg), "-----END PGP PUBLIC KEY BLOCK-----")
})

test_that("encrypt", {
  msg <- envelope(
    to = "alice@yahoo.com",
    from = "bob@gmail.com",
    encrypt = TRUE
  ) %>% text(TXTCONTENT)

  expect_match(as.character(msg), "-----BEGIN PGP MESSAGE-----")
  expect_match(as.character(msg), "-----END PGP MESSAGE-----")

  expect_no_match(as.character(msg), "-----BEGIN PGP SIGNATURE-----")
  expect_no_match(as.character(msg), "-----END PGP SIGNATURE-----")
  expect_no_match(as.character(msg), "-----BEGIN PGP PUBLIC KEY BLOCK-----")
  expect_no_match(as.character(msg), "-----END PGP PUBLIC KEY BLOCK-----")
})

test_that("sign & encrypt", {
  msg <- envelope(
    to = "alice@yahoo.com",
    from = "bob@gmail.com",
    sign = TRUE,
    encrypt = TRUE
  ) %>% text(TXTCONTENT)

  expect_match(as.character(msg), "-----BEGIN PGP MESSAGE-----")
  expect_match(as.character(msg), "-----END PGP MESSAGE-----")

  expect_no_match(as.character(msg), "-----BEGIN PGP SIGNATURE-----")
  expect_no_match(as.character(msg), "-----END PGP SIGNATURE-----")
  expect_no_match(as.character(msg), "-----BEGIN PGP PUBLIC KEY BLOCK-----")
  expect_no_match(as.character(msg), "-----END PGP PUBLIC KEY BLOCK-----")
})

test_that("public key", {
  msg <- envelope(
    to = "alice@yahoo.com",
    from = "bob@gmail.com",
    public_key = TRUE
  ) %>% text(TXTCONTENT)

  expect_match(as.character(msg), "-----BEGIN PGP PUBLIC KEY BLOCK-----")
  expect_match(as.character(msg), "-----END PGP PUBLIC KEY BLOCK-----")

  expect_no_match(as.character(msg), "-----BEGIN PGP SIGNATURE-----")
  expect_no_match(as.character(msg), "-----END PGP SIGNATURE-----")
  expect_no_match(as.character(msg), "-----BEGIN PGP MESSAGE-----")
  expect_no_match(as.character(msg), "-----END PGP MESSAGE-----")
})
