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
encrypt <- function(msg, sign = TRUE) {
  msg$encrypt <- TRUE
  if (get_option_invisible()) invisible(msg) else msg # nocov
}

encrypt_body <- function(content, parties, encrypt, sign) {
  encrypt <- ifelse(is.null(encrypt), FALSE, encrypt)
  sign <- ifelse(is.null(sign), FALSE, sign)

  if (encrypt || sign) {
    if(!require(gpg, quietly = TRUE)) {
      stop("Install {gpg} to encrypt and/or sign messages.")
    }
    log_debug("Encrypt message: {msg$encrypt}")
    log_debug("Sign message:    {msg$encrypt}")

    parties <- parties %>% select(type, email = raw)

    sender <- parties %>% filter(type == "From")
    recipients <- parties %>% anti_join(sender, by = c("type", "email"))

    keys <- gpg_list_keys()

    missing_keys <- anti_join(recipients, keys, by = "email")

    if (nrow(missing_keys)) {
      stop("Missing public keys for the follow addresses: ", paste(missing_keys$email, collapse = ", "), ".")
    }

    # Get the fingerprints of the recipients' keys.
    fingerprints <- keys %>%
      inner_join(recipients, by = "email") %>%
      pull(fingerprint)

    # Write message contents to a temporary file.
    #
    TMPFILE <- tempfile()
    as.character(content) %>% writeLines(TMPFILE)

    # Encrypt content from temporary file.
    #
    encrypted <- gpg_encrypt(TMPFILE, fingerprints)

    # Delete temporary file.
    #
    unlink(TMPFILE)

    content <- emayili:::multipart_encrypted()
    content <- emayili:::append.MIME(content, emayili:::application_pgp_encrypted())
    content <- emayili:::append.MIME(content, emayili:::application_octet_stream(encrypted, disposition = "inline",  filename = "encrypted.asc"))
    emayili:::as.character.MIME(content) %>% cat()
  }

  content
}
