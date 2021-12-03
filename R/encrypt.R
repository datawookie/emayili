#' Encrypt message
#'
#' @inheritParams parties
#' @param sign Whether to sign the message.
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
encrypt <- function(msg, sign = TRUE) {
  sign <- ifelse(is.null(sign), FALSE, sign)
  stopifnot(is.logical(sign))

  msg$encrypt <- TRUE
  msg$sign <- sign
  if (get_option_invisible()) invisible(msg) else msg # nocov
}

encrypt_body <- function(content, parties, encrypt, sign, share_public_key = TRUE) {
  encrypt <- ifelse(is.null(encrypt), FALSE, encrypt)
  sign <- ifelse(is.null(sign), FALSE, sign)

  if (encrypt || sign) {
    if(!requireNamespace("gpg", quietly = TRUE)) {
      stop("Install {gpg} to encrypt and/or sign messages.")
    }
    log_debug("Encrypt message: {encrypt}")
    log_debug("Sign message:    {sign}")

    parties <- parties %>% select(type, email = raw)

    sender <- parties %>% filter(type == "From")
    recipients <- parties %>% anti_join(sender, by = c("type", "email"))

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

    # TODO: DON'T USE /TMP
    # TODO: DON'T USE /TMP
    # TODO: DON'T USE /TMP
    # TODO: DON'T USE /TMP
    # TODO: DON'T USE /TMP
    # TODO: DON'T USE /TMP
    # TODO: DON'T USE /TMP
    # TODO: DON'T USE /TMP
    # TODO: DON'T USE /TMP
    # TODO: DON'T USE /TMP
    # TODO: DON'T USE /TMP
    # TODO: DON'T USE /TMP
    # TODO: DON'T USE /TMP
    # TODO: DON'T USE /TMP
    # TMPFILE <- tempfile()
    TMPFILE <- tempfile(tmpdir = "/tmp")

    # - Sign content from temporary file.
    # - Write result back to temporary file.
    #
    if (sign) {
      # log_debug("Export public key.")
      # public_key <- gpg::gpg_export(sender_fingerprint)
      # log_debug("Done!")
      # #
      if (!("multipart_mixed" %in% class(content))) {
        log_debug("- Convert message to multipart/mixed.")
        content <- emayili:::multipart_mixed(
          children = list(content)
        )
      }
      # if (share_public_key) {
      #   log_debug("- Append public key.")
      #   content <- emayili:::append.MIME(content, application_pgp_keys(public_key))
      # }
      # #
      # log_debug("Create outer multipart/mixed.")
      # content <- emayili:::multipart_mixed(
      #   children = list(content)
      # )

      log_debug("Write message to {TMPFILE}.")
      cat(emayili:::as.character.MIME(content), file = TMPFILE)
      print(emayili:::as.character.MIME(content))
      print(paste(readLines(TMPFILE), collapse = "\n"))
      # file.copy(TMPFILE, "/tmp/body.txt")
      log_debug("Sign message from {TMPFILE}.")
      signature <- gpg::gpg_sign(TMPFILE, sender_fingerprint, mode = "detach")
      # cat(signature, file = "/tmp/signature.txt")
      log_debug("Done!")
      cat(paste(readLines(TMPFILE), collapse = "\n"))
      cat(signature)
      log_debug("Add signature.")
      signed <- emayili:::multipart_signed(
        children = list(
          content,
          emayili:::application_pgp_signature(signature)
        )
      )
      log_debug("Write multipart/signed message to {TMPFILE}.")
      emayili:::as.character.MIME(signed) %>% writeLines(TMPFILE)
    }

    # THIS WILL NUKE THE SIGNED CONTENT? MAYBE JUST CHECK IF FILE ALREADY EXISTS?
    # log_debug("Write message to {TMPFILE}.")
    # emayili:::as.character.MIME(content) %>% writeLines(TMPFILE)

    # # - Encrypt content from temporary file.
    # # - Write result back to temporary file.
    # #
    # if (encrypt) {
    #   log_debug("Encrypt message from {TMPFILE}.")
    #   encrypted <- gpg::gpg_encrypt(TMPFILE, recipients_fingerprint)
    #   log_debug("Done!")
    #   encrypted <- emayili:::multipart_encrypted(
    #     children = list(
    #       emayili:::application_pgp_encrypted(),
    #       emayili:::application_octet_stream(
    #         encrypted,
    #         disposition = "inline",
    #         filename = "encrypted.asc"
    #       )
    #     )
    #   )
    #   log_debug("Write multipart/encrypted message to {TMPFILE}.")
    #   emayili:::as.character.MIME(encrypted) %>% writeLines(TMPFILE)
    # }

    log_debug("Read message from {TMPFILE}.")
    content <- read_text(TMPFILE)

    # TODO!!
    # TODO!!
    # TODO!!
    # TODO!!
    # TODO!!
    # TODO!!
    # TODO!!
    # TODO!!
    # TODO!!
    # TODO!!
    # TODO!!
    # TODO!!
    # TODO!!
    # TODO!!
    # TODO!!
    # TODO!!
    # Delete temporary file.
    #
    # unlink(TMPFILE)
  }

  content
}
