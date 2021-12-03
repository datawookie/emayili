#' Encrypt message
#'
#' @inheritParams parties
#'
#' @return A message object.
#' @export
#'
#' @examples#'
#' \dontrun{
#' # A WWII D-Day message.
#' #
#' # See https://twitter.com/bletchleypark/status/1136490396626800640.
#' #
#' msg <- envelope(
#'   to = "schunk@u-boat.com",
#'   subject = "Top Secret Message",
#'   text = "Immediate readiness. There are indications that the invasion has begun."
#' )
#' msg %>% encrypt()
#' }
encrypt <- function(msg, encrypt = TRUE, sign = TRUE, public_key = TRUE) {
  encrypt <- ifelse(is.null(encrypt), FALSE, encrypt)
  sign <- ifelse(is.null(sign), FALSE, sign)
  stopifnot(is.logical(encrypt) && is.logical(sign))

  msg$encrypt <- encrypt
  msg$sign <- sign
  msg$public_key <- public_key

  if (get_option_invisible()) invisible(msg) else msg # nocov
}

encrypt_body <- function(content, parties, encrypt, sign, public_key) {
  encrypt <- ifelse(is.null(encrypt), FALSE, encrypt)
  sign <- ifelse(is.null(sign), FALSE, sign)

  # - Can't encrypt or sign an empty message...
  # - ... unless that message just contains a public key.
  if ((encrypt || sign) && is.null(content) && !public_key) stop("Can't sign or encrypt an empty message!")

  if (encrypt || sign || public_key) {
    if(!requireNamespace("gpg", quietly = TRUE)) {
      stop("Install {gpg} to encrypt and/or sign messages.")
    }
    log_debug("Encrypt message: {encrypt}")
    log_debug("Sign message:    {sign}")

    parties <- parties %>% select(type, email = raw)

    sender <- parties %>% filter(type == "From")
    if (!nrow(sender)) stop("Can't sign or encrypt without sender!")
    recipients <- parties %>% anti_join(sender, by = c("type", "email"))
    if (!nrow(recipients)) stop("Can't sign or encrypt without recipients!")

    keys <- gpg::gpg_list_keys()

    missing_keys <- anti_join(recipients, keys, by = "email")

    if (nrow(missing_keys)) {
      stop("Missing public keys for the follow addresses: ", paste(missing_keys$email, collapse = ", "), ".")
    }

    # Get the fingerprints of the senders' keys.
    #
    sender_fingerprint <- keys %>%
      inner_join(sender, by = "email") %>%
      pull(fingerprint)

    # Get the fingerprints of the recipients' keys.
    #
    recipients_fingerprint <- keys %>%
      inner_join(recipients, by = "email") %>%
      pull(fingerprint)

    TMPFILE <- tempfile()

    if (sign || public_key) {
      if (!("multipart_mixed" %in% class(content))) {
        log_debug("Convert message to multipart/mixed.")
        content <- emayili:::multipart_mixed(
          children = list(content)
        )
      }
    }

    if (public_key) {
      if (public_key) {
        log_debug("Export public key.")
        public_key <- gpg::gpg_export(sender_fingerprint)
        log_debug("Done!")
        log_debug("Append public key.")
        content <- emayili:::append.MIME(content, application_pgp_keys(public_key))
      }
    }

    # - Sign content from temporary file.
    # - Write result back to temporary file.
    #
    if (sign) {
      log_debug("Write message to {TMPFILE}.")
      cat(emayili:::as.character.MIME(content), file = TMPFILE)
      log_debug("Sign message from {TMPFILE}.")
      signature <- gpg::gpg_sign(TMPFILE, sender_fingerprint, mode = "detach")
      log_debug("Done!")
      log_debug("Add signature.")
      signed <- emayili:::multipart_signed(
        children = list(
          content,
          emayili:::application_pgp_signature(signature)
        )
      )
      log_debug("Write multipart/signed message to {TMPFILE}.")
      emayili:::as.character.MIME(signed) %>% writeLines(TMPFILE)
    } else {
      log_debug("Write message to {TMPFILE}.")
      emayili:::as.character.MIME(content) %>% writeLines(TMPFILE)
    }

    # - Encrypt content from temporary file.
    # - Write result back to temporary file.
    #
    if (encrypt) {
      log_debug("Encrypt message from {TMPFILE}.")
      encrypted <- gpg::gpg_encrypt(TMPFILE, recipients_fingerprint)
      log_debug("Done!")
      encrypted <- emayili:::multipart_encrypted(
        children = list(
          emayili:::application_pgp_encrypted(),
          emayili:::application_octet_stream(
            encrypted,
            disposition = "inline",
            filename = "encrypted.asc"
          )
        )
      )
      log_debug("Write multipart/encrypted message to {TMPFILE}.")
      emayili:::as.character.MIME(encrypted) %>% writeLines(TMPFILE)
    }

    log_debug("Read message from {TMPFILE}.")
    content <- read_text(TMPFILE)

    # Delete temporary file.
    #
    unlink(TMPFILE)
  }

  content
}
