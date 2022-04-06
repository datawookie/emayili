#' Encrypt or sign a message
#'
#' Specify whether the message should be encrypted, signed or have a public key
#' attached.
#'
#' The `signature()` function will add a digital signature to a message. It will
#' also optionally include a copy of the sender's public key.
#'
#' The `encrypt()` function will encrypt the contents of a message using the
#' public key(s) of the recipient(s). It can also add a digital signature to the
#' message (this is the default behaviour) and include a copy of the sender's
#' public key. Signing happens _before_ encryption, so the digital signature
#' will only be accessible once the message has been decrypted. If a recipient
#' no longer has access to their private key or their email client is unable to
#' decrypt the message then they will not be able to access the message
#'  contents.
#'
#' @name encrypt
#'
#' @inheritParams envelope
#' @inheritParams parties
#'
#' @return A message object.
#' @export
#'
#' @examples
#' \dontrun{
#' msg <- envelope(
#'   from = "flotilla@kriegsmarine.gov",
#'   to = "schunk@u-boat.com",
#'   subject = "Top Secret Message",
#'   text = "Immediate readiness. There are indications that the invasion has begun."
#' )
#' # Encrypt and sign the message.
#' msg %>% encrypt()
#' # Only encrypt the message.
#' msg %>% encrypt(sign = FALSE)
#' # Only sign the message.
#' msg %>% signature()
#' msg %>% encrypt(encrypt = FALSE)
#' }
encrypt <- function(msg, encrypt = TRUE, sign = TRUE, public_key = TRUE) {
  encrypt <- ifelse(is.null(encrypt), FALSE, encrypt)           # nocov start
  sign <- ifelse(is.null(sign), FALSE, sign)
  stopifnot(is.logical(encrypt) && is.logical(sign))

  msg$encrypt <- encrypt
  msg$sign <- sign
  msg$public_key <- public_key                                  # nocov end

  if (get_option_invisible()) invisible(msg) else msg # nocov
}

#' @rdname encrypt
#'
#' @inheritParams encrypt
#'
#' @export
signature <- function(msg, public_key = TRUE) {
  encrypt(msg, sign = TRUE, encrypt = FALSE, public_key = public_key)
}

encrypt_body <- function(content, parties, encrypt, sign, public_key) {
  encrypt <- ifelse(is.null(encrypt), FALSE, encrypt)
  sign <- ifelse(is.null(sign), FALSE, sign)

  # - Can't encrypt or sign an empty message...
  # - ... unless that message just contains a public key.
  if ((encrypt || sign) && is.null(content) && !public_key) stop("Can't sign or encrypt an empty message!")

  if (encrypt || sign || public_key) {
    if (!requireNamespace("gpg", quietly = TRUE)) {
      stop("Install {gpg} to encrypt and/or sign messages.")    # nocov
    }
    log_debug("Encrypt message: {encrypt}")
    log_debug("Sign message:    {sign}")

    parties <- parties %>% select(type, email = raw)

    sender <- parties %>% filter(type == "From")
    if (!nrow(sender)) stop("Can't sign or encrypt without sender!")
    recipients <- parties %>% anti_join(sender, by = c("type", "email"))
    if (!nrow(recipients) && encrypt) stop("Can't encrypt without recipients!")

    keys <- gpg::gpg_list_keys()

    # Always need to have sender keys (if signing and/or encrypting).
    missing_keys <- anti_join(sender, keys, by = "email")
    # Need to have recipient keys if encrypting.
    if (encrypt) {
      missing_keys <- rbind(
        missing_keys,
        anti_join(recipients, keys, by = "email")
      )
    }

    if (nrow(missing_keys)) {
      stop("Missing keys for the follow addresses: ", paste(missing_keys$email, collapse = ", "), ".")
    }

    # Get the fingerprints of the senders' keys.
    #
    sender_fingerprint <- keys %>%
      inner_join(sender, by = "email")

    if (nrow(sender_fingerprint) > 1) {
      log_warn("There are multiple keys for sender.")
    }

    if (nrow(sender_fingerprint) > 1) {
      log_warn("Selecting most recent key.")

      sender_fingerprint <- sender_fingerprint %>%
        # Timestamp reflects when key was created not when added to keychain.
        arrange(desc(timestamp)) %>%
        slice(1)
    }

    sender_fingerprint <- sender_fingerprint %>%
      pull(fingerprint)

    # Get the fingerprints of the recipients' keys.
    #
    recipients_fingerprint <- keys %>%
      inner_join(recipients, by = "email") %>%
      pull(fingerprint)

    log_debug("Found fingerprints for {length(recipients_fingerprint)} recipient(s).")

    TMPFILE <- tempfile()

    if (sign || public_key) {
      if (!("multipart_mixed" %in% class(content))) {
        if (is.null(content)) {
          log_debug("Create empty multipart/mixed.")
          content <- multipart_mixed()
        } else {
          log_debug("Convert message to multipart/mixed.")
          content <- multipart_mixed(
            children = list(content)
          )
        }
      }
    }

    if (public_key) {
      if (public_key) {
        log_debug("Export public key.")
        public_key <- gpg::gpg_export(sender_fingerprint)
        log_debug("Done!")
        log_debug("Append public key.")
        content <- after(content, application_pgp_keys(public_key))
      }
    }

    # - Sign content from temporary file.
    # - Write result back to temporary file.
    #
    if (sign) {
      log_debug("Write message to {TMPFILE}.")
      cat(as.character.MIME(content), file = TMPFILE)
      log_debug("Sign message from {TMPFILE}.")
      signature <- gpg::gpg_sign(TMPFILE, sender_fingerprint, mode = "detach")
      log_debug("Done!")
      log_debug("Add signature.")
      signed <- multipart_signed(
        children = list(
          content,
          application_pgp_signature(signature)
        )
      )
      log_debug("Write multipart/signed message to {TMPFILE}.")
      as.character.MIME(signed) %>% writeLines(TMPFILE)
    } else {
      log_debug("Write message to {TMPFILE}.")
      as.character.MIME(content) %>% writeLines(TMPFILE)
    }

    # - Encrypt content from temporary file.
    # - Write result back to temporary file.
    #
    if (encrypt) {
      log_debug("Encrypt message from {TMPFILE}.")
      # Don't sign here because signature is handled separately.
      encrypted <- gpg::gpg_encrypt(TMPFILE, recipients_fingerprint, signer = NULL)
      log_debug("Done!")
      encrypted <- multipart_encrypted(
        children = list(
          application_pgp_encrypted(),
          application_octet_stream(
            encrypted,
            disposition = "inline",
            filename = "encrypted.asc"
          )
        )
      )
      log_debug("Write multipart/encrypted message to {TMPFILE}.")
      as.character.MIME(encrypted) %>% writeLines(TMPFILE)
    }

    log_debug("Read message from {TMPFILE}.")
    content <- read_text(TMPFILE)

    # Delete temporary file.
    #
    unlink(TMPFILE)
  }

  content
}
