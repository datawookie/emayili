skip_if_not_installed("gpg")

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
gpg_keygen(name = "Jim", email = "jim@aol.com")
#
# The keys should all be RSA.
#
stopifnot(all(gpg_list_keys() %>% pull(algo) %in% c("RSA", "EdDSA")))

BEGIN_PGP_MESSAGE          <- "-----BEGIN PGP MESSAGE-----"
END_PGP_MESSAGE            <- "-----END PGP MESSAGE-----"
BEGIN_PGP_SIGNATURE        <- "-----BEGIN PGP SIGNATURE-----"
END_PGP_SIGNATURE          <- "-----END PGP SIGNATURE-----"
BEGIN_PGP_PUBLIC_KEY_BLOCK <- "-----BEGIN PGP PUBLIC KEY BLOCK-----"
END_PGP_PUBLIC_KEY_BLOCK   <- "-----END PGP PUBLIC KEY BLOCK-----"

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
    from = "bob@gmail.com"
  ) %>%
    text(TXTCONTENT) %>%
    signature(public_key = FALSE)

  expect_match(as.character(msg), BEGIN_PGP_SIGNATURE)
  expect_match(as.character(msg), END_PGP_SIGNATURE)

  expect_no_match(as.character(msg), BEGIN_PGP_MESSAGE)
  expect_no_match(as.character(msg), END_PGP_MESSAGE)
  expect_no_match(as.character(msg), BEGIN_PGP_PUBLIC_KEY_BLOCK)
  expect_no_match(as.character(msg), END_PGP_PUBLIC_KEY_BLOCK)
})

test_that("encrypt", {
  msg <- envelope(
    to = "alice@yahoo.com",
    from = "bob@gmail.com",
    encrypt = TRUE
  ) %>% text(TXTCONTENT)

  expect_match(as.character(msg), BEGIN_PGP_MESSAGE)
  expect_match(as.character(msg), END_PGP_MESSAGE)

  expect_no_match(as.character(msg), BEGIN_PGP_SIGNATURE)
  expect_no_match(as.character(msg), END_PGP_SIGNATURE)
  expect_no_match(as.character(msg), BEGIN_PGP_PUBLIC_KEY_BLOCK)
  expect_no_match(as.character(msg), END_PGP_PUBLIC_KEY_BLOCK)
})

test_that("sign & encrypt", {
  msg <- envelope(
    to = "alice@yahoo.com",
    from = "bob@gmail.com",
    sign = TRUE,
    encrypt = TRUE
  ) %>% text(TXTCONTENT)

  expect_match(as.character(msg), BEGIN_PGP_MESSAGE)
  expect_match(as.character(msg), END_PGP_MESSAGE)

  expect_no_match(as.character(msg), BEGIN_PGP_SIGNATURE)
  expect_no_match(as.character(msg), END_PGP_SIGNATURE)
  expect_no_match(as.character(msg), BEGIN_PGP_PUBLIC_KEY_BLOCK)
  expect_no_match(as.character(msg), END_PGP_PUBLIC_KEY_BLOCK)
})

test_that("public key", {
  msg <- envelope(
    to = "alice@yahoo.com",
    from = "bob@gmail.com",
    public_key = TRUE
  ) %>% text(TXTCONTENT)

  expect_match(as.character(msg), BEGIN_PGP_PUBLIC_KEY_BLOCK)
  expect_match(as.character(msg), END_PGP_PUBLIC_KEY_BLOCK)

  expect_no_match(as.character(msg), BEGIN_PGP_SIGNATURE)
  expect_no_match(as.character(msg), END_PGP_SIGNATURE)
  expect_no_match(as.character(msg), BEGIN_PGP_MESSAGE)
  expect_no_match(as.character(msg), END_PGP_MESSAGE)
})

test_that("fail without sender or recipients", {
  expect_error(
    envelope(to = "alice@yahoo.com") %>% encrypt() %>% as.character(),
    "without sender"
    )
  expect_error(
    envelope(from = "bob@gmail.com") %>% encrypt() %>% as.character(),
    "without recipients"
    )
})

test_that("missing public keys", {
  # Missing sender key.
  expect_error(
    envelope(to = "alice@yahoo.com", from = "tim@gmail.com") %>%
      encrypt() %>%
      as.character(),
    "missing keys",
    ignore.case = TRUE
  )
  # Missing recipient key.
  expect_error(
    envelope(to = "jenny@yahoo.com", from = "bob@gmail.com") %>%
      encrypt() %>%
      as.character(),
    "missing keys",
    ignore.case = TRUE
  )
})

test_that("sign with/without body", {
  nobody <- envelope(to = "alice@yahoo.com", from = "bob@gmail.com")
  body <- nobody %>% text("Hello!")

  # With public key.
  expect_error(nobody %>% encrypt(FALSE, TRUE, TRUE) %>% as.character(), NA)
  expect_error(body %>% encrypt(FALSE, TRUE, TRUE) %>% as.character(), NA)

  # Without public key.
  expect_error(nobody %>% encrypt(FALSE, TRUE, FALSE) %>% as.character(), "empty message")
  expect_error(body %>% encrypt(FALSE, TRUE, FALSE) %>% as.character(), NA)
})

test_that("multiple keys", {
  msg <- envelope(to = "alice@yahoo.com", from = "jim@aol.com") %>%
    text("Hello!") %>%
    encrypt()

  expect_error(msg %>% as.character(), NA)
})
