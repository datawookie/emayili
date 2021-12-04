skip_if_not_installed("gpg")
#
# Encryption tests currently don't work with macOS on GitHub Actions.
#
skip_on_os(os = "mac")

library(gpg)

# - If running locally then use random home directory for keyring.
# - Don't do this on CI because GPG doesn't work on macOS with non-default
#   home folder.
#
if (Sys.getenv("CI") == "") {
  message("Use random home folder for GPG.")
  gpg_restart(home = tempdir(), silent = TRUE)
}
#
gpg_keygen(name = "Alice", email = "alice@yahoo.com")
gpg_keygen(name = "Bob", email = "bob@gmail.com")
gpg_keygen(name = "Jim", email = "jim@aol.com")
#
# The keys should all be RSA.
#
stopifnot(all(gpg_list_keys() %>% pull(algo) == "RSA"))

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
