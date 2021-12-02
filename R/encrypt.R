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

encrypt_body <- function(content, parties, encrypt, sign) {
  print("FOO")
  print("FOO")
  print("FOO")
  print("FOO")
  print("FOO")
  print("FOO")
  encrypt <- ifelse(is.null(encrypt), FALSE, encrypt)
  sign <- ifelse(is.null(sign), FALSE, sign)
  print(encrypt)
  print(sign)

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

    # Write message contents to a temporary file.
    #
    TMPFILE <- tempfile()
    log_debug("Write message to {TMPFILE}.")
    emayili:::as.character.MIME(content) %>% writeLines(TMPFILE)

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

    # - Sign content from temporary file.
    # - Write result back to temporary file.
    #
    if (sign) {
      log_debug("Sign message from {TMPFILE}.")
      signature <- gpg::gpg_sign(TMPFILE, sender_fingerprint, mode = "detach")
      log_debug("Done!")
      if (!("multipart_mixed" %in% class(content))) {
        log_debug("Converting message to multipart/mixed.")
        content <- emayili:::multipart_mixed(children = list(content))
      }
      #
      # INSERT application_pgp_keys() INTO content MULTIPART/MIXED
      # INSERT application_pgp_keys() INTO content MULTIPART/MIXED
      # INSERT application_pgp_keys() INTO content MULTIPART/MIXED
      # INSERT application_pgp_keys() INTO content MULTIPART/MIXED
      # INSERT application_pgp_keys() INTO content MULTIPART/MIXED
      #
      signed <- emayili:::multipart_signed(
        children = list(
          content2,
          emayili:::application_pgp_signature(signature)
        )
      )
      log_debug("Write multipart/signed message to {TMPFILE}.")
      emayili:::as.character.MIME(signed) %>% writeLines(TMPFILE)
    }


    # Content-Type: multipart/signed; micalg=pgp-sha256;
    # protocol="application/pgp-signature";
    # boundary="IKnJ4TqXo95ddlFrtwLSYpANZnV5KvJWV"
    #
    # This is an OpenPGP/MIME signed message (RFC 4880 and 3156)
    # --IKnJ4TqXo95ddlFrtwLSYpANZnV5KvJWV
    # Content-Type: multipart/mixed; boundary="CMdYTA21gPAOyCXOEcSHUxZnKN1WV88Zi";
    # protected-headers="v1"
    # From: Andrew Collier <andrew@fathomdata.dev>
    #   To: Andrew Collier <andrew.b.collier@gmail.com>
    #   Message-ID: <45d1fb2a-5e28-89f9-79b2-ef6cba7a699a@fathomdata.dev>
    #   Subject: Just Signed
    #
    # --CMdYTA21gPAOyCXOEcSHUxZnKN1WV88Zi
    # Content-Type: multipart/mixed;
    # boundary="------------E2671E3FEE4701B34168D575"
    # Content-Language: en-GB
    #
    # This is a multi-part message in MIME format.
    # --------------E2671E3FEE4701B34168D575
    # Content-Type: text/plain; charset=utf-8; format=flowed
    # Content-Transfer-Encoding: quoted-printable
    #
    # Verify this, MOFO.
    #
    #
    # --------------E2671E3FEE4701B34168D575
    # Content-Type: application/pgp-keys;
    # name="OpenPGP_0x87CC261267801A17.asc"
    # Content-Transfer-Encoding: quoted-printable
    # Content-Description: OpenPGP public key
    # Content-Disposition: attachment;
    # filename="OpenPGP_0x87CC261267801A17.asc"
    #
    # -----BEGIN PGP PUBLIC KEY BLOCK-----
    #
    #   xsFNBGGgYjQBEACaevZlELmd7QdzCg6PTdj4uoW0iKSjtaYpLyrfkP3/TxF/rWL272hU8ETke=
    #   uzD
    # Kykoe3gKV0y5RycOA/+MMavrJI+l9k9iPtM9ETuPILyE4INZssinaN8eiTQveqxYSUK7A3wVy=
    #   1YM
    # 35pnEj+trQJuUApuzzWdsvKHzoTcldtSZNvpwtIz5V/po9HmSNNF4vI7V1U9o1XSXhwP+2Qv3=
    #   4m2
    # tMxeQ9eamYbfrfzzK7T1jie3qg6/v8lbAUerH6Yxo+yn0dtbScl5bJ9h/5QZGGxg2aSqcIJ68=
    #   KZq
    # /x8L8Q2JbDZk4aiAGCFVMueb7u0N/M5PkcpWHo64/uMsJwBSMt14l0VB10jYpFcY9eWe9Qxxr=
    #   LwJ
    # qA6M7dWH1i5lMdYg5qW2GplOxW71n4fhlUObT2C2GTvp21NrkU468S7msIpgJsZ0YJmfBjraW=
    #   xH7
    # wt+6iGmg0aO4yaIqfe9sHz2lKtmra1gHKhpr5rJyIR6jMERg+BgQstpXDe35O0Vxvc16aVQLv=
    #   8La
    # bpyoozpoqhTw11+dnVspAqhlVY/SQ9ekc5UkbdL3pjYB27H1LWeJ2BI6+gTjFulKKgCb/NmmV=
    #   2Hf
    # bTuA7Khh7BTXneZt7OONfUfLpf+iu0ZVaXMvP97sOe1sAlDyspWJYw2UX4UWChbQ/JOtr+h6D=
    #   M8d
    # bNxhFWesZCd92wARAQABzSlBbmRyZXcgQi4gQ29sbGllciA8YW5kcmV3QGZhdGhvbWRhdGEuZ=
    #   GV2
    # PsLBjQQTAQoAOBYhBB2jEz6KWqyV9UNEOYfMJhJngBoXBQJhoGI0AhsDBQsJCAcCBhUKCQgLA=
    #   gQW
    # AgMBAh4BAheAAAoJEIfMJhJngBoXHu4P9Ro9rPNhJ2zLr6ynwecu2dfWdTgkI9awLO6Y7/O7F=
    #   KOW
    # qJ7KDDXtZd4BI8vlveuYZA1105Ff4gCkwzOPerYMX++zHSVfDyjjEuqF4IzsWEivUYbLVyvA8=
    #   ZXi
    # Ckh6Qcn4niqSeVyk1Ypt8bQ2K3Gq0U6TVP9lse14KVluqbKqAFhzuCIR6taqK1pqqWBqcQRKt=
    #   DY8
    # zZGI1XIZBT5cQI9KVHu7BAeb7LY6FR26Xoxw9qHhGcZiL0T8j6kPFAR0AngagqaSyRF2j/C95=
    #   hu2
    # XINQ1mwvmGpAwRa1tEi9i4LlaYyloPQEFr5l5BrNDrIfX3Fwha3kyCUNhSzrQcwEvTY0XZe+1=
    #   W1Y
    # UWsiR9JdicGRvSeQKeGhobXybXL5qr5E3onGoSd4VRiTHKtpI4jcCBuoxelRPIus0to/IWMQ6=
    #   QRE
    # /An8E3IUkDuAyCd/oiXYt2Gy2IeQ8ZO9UtqJwzg2WjRy7D7fS9T2R34QQ1YjY3bfBGk0ulMaI=
    #   i+s
    # LesbvNwgjvAEXNqnciK9zpjmpWqnxv7iOiQ27TnIpVUC3aZmPD7vF4nBiVbFjgdu90cwHhIQy=
    #   Lza
    # 56q5CBlE5Z8UTLjqd+NRh4u0xiayCJ7BM5NcIiW6BFVdfj4GBkXP9snh4JcGbZeIO1afe0pH5=
    #   vuD
    # pmKuABg0McvhjxqzrlysvI0pcJwSzdPOwU0EYaBiNAEQAKleFR5bcE0ILOZufooh9RSAM0RVe=
    #   WOi
    # U6tL4st1/q3GqTxLSasBxnWFxFmffcrGpq7KRB9FoPrE29I35IX7s/hd0cpQ/I4qcrWxScrkg=
    #   5C5
    # DkOu99Qlf85GmXbctlP6g3k/bmEkyx55AH+PSOMRCXRofaJVBmM1XFyvkW0FzHmwytdkU2LLx=
    #   c8a
    # h6KCe5y4YQAQ8xdFP5Iq6vz3Adq0HRBrabqnRNiBJCQQabsnPBY8OgDtD03OFDLkKGoy9+p8R=
    #   7sE
    # zAnr+ApVZH7ZxHmS8CsT5Qcv4ufhT9tU68Y82tvQcnVxY4Xv1b17tsfur1lQ2G41c8jFooM1T=
    #   7j3
    # qrpKg75V+77ng5K04f3TtGrT0Y//v86lZ+FNO8yIBdeV6h5EcPhIFB7ZpwxuWuHxy7KdFSWgE=
    #   T1c
    # UflY8sGr8nTFwVhAjYOcXSDSMIQftUrJHL3g5INiGx5sXHgl80aMXDRSLCc9oWqzQ/+eYmX0L=
    #   uXw
    # r/hyvNTxt+JBsvobTXxk980ImHjKGZUYKEl3sXZr9FPS+4CKhr7dx3OH5herG6m287hficsFh=
    #   soF
    # mdkfaBrFzL0kdSWCEgTHKUYtUzMbS82uMDIKCAr1AjTlQj5w0BVsfBIvmTL555+K4peNNp/qG=
    #   OVF
    # qOujKjaXwA9HGzk0OazdtTozp4P80qFBZq2dPCOz2ASZABEBAAHCwXYEGAEKACAWIQQdoxM+i=
    #   lqs
    # lfVDRDmHzCYSZ4AaFwUCYaBiNAIbDAAKCRCHzCYSZ4AaFxLCD/9AUl4/uF6Ovxg5nNe9+yInK=
    #   SAF
    # 0C+qY1rH+H8NwNeaEQ7ncSsp6BNVOKItCKuANh9QwbpTbgTduPvSQFuKw8luaXPIZepjIYzx5=
    #   x9x
    # PQEdeAGiUZW250uX8ZJqRvibWsqjN9NXPCIYC6moo53/2kY48hQDaeiy6dmSrmj/XEpFIm1hQ=
    #   Gw4
    # ZlvsiA/kVl/XfxuxUOHB+AeTmFoVspwihW/KjSa5F2MfW7W6CdkehfTFLlGZdApvmZFj/d13r=
    #   V5v
    # 9Hy5ldeTkiEBRAReSRQGy7oCDCi/B/04J+zQ4nd1LvFngeUvMuZDtPtWcU9kV+L//iVnq09BB=
    #   3f2
    # /ZcNc+tfhYyKs/M2nMT13wByRuuj5dzr6opNaswle7q55pmXnd9qloLi2djma2iF5+IVVFAxA=
    #   iqT
    # uL7A36lymExI1BiDY0rlQCsb7Xw+aNgjx2I1gZcdzHA+daDT0LDvoM342aU/TqsDRhGVCD8N8=
    #   ylW
    # jlQpVTuTjcyj65MrQCTI9DIUpuSbYMw8tMsDfavFxSQ3oPVRq87J20atwnUHQ5Tofd2bzRNwI=
    #   0JZ
    # /R8eIiIs2tPd8cyyMLZz0uJ7cf4+POGMV6/5mBtlwberiKZ6tAcaPS1wtOqzI1E884eC55/YT=
    #   bfz
    # ha3gdKtUmIOAemwxrHF8u9DQpPmfDKhXrL9GtiFjGTcXabbslw=3D=3D
    # =3Dfr2w
    # -----END PGP PUBLIC KEY BLOCK-----
    #
    #   --------------E2671E3FEE4701B34168D575--
    #
    #   --CMdYTA21gPAOyCXOEcSHUxZnKN1WV88Zi--
    #
    #   --IKnJ4TqXo95ddlFrtwLSYpANZnV5KvJWV
    # Content-Type: application/pgp-signature; name="OpenPGP_signature.asc"
    # Content-Description: OpenPGP digital signature
    # Content-Disposition: attachment; filename="OpenPGP_signature"
    #
    # -----BEGIN PGP SIGNATURE-----
    #
    #   wsF5BAABCAAjFiEEHaMTPoparJX1Q0Q5h8wmEmeAGhcFAmGohjYFAwAAAAAACgkQh8wmEmeAGhdS
    # fw/+Ox3osPttqAf8zjkPsGWs4wLvZe1GsayOwxH7dlinialiQ0m86MRgtmcI8Q6Qo7/Fmt0o+H7/
    #   nxzceQIVDlMgK3GGk2mosO8EnqDgOQAh9BqWLLOKhly1jBwOYp2dvNT3NmAmgr/JXAoQm0z5SSOv
    # Jn71CP2MlKC6v0VPL7ravta3vQIs4j2HJZPHkDLNXwA/yzn5y2H+deRyS+88IXJZdClLBM86mf0E
    # HsNMstlDI9buLqwGq41UPMkrTCu5ySNSUYlRSuryRebagDNK+7O3G0z1RzGFwvwqZwhVM2zZ8iDm
    # el9J8b7CzF941ldmHovhuAZenBvx7KsIgS7hD1MOPF51+aI9gmTgg1dgNfqVMaVbdwv0yWhWpTCb
    # jSj//VaZme4+W/3f34TuuT4kpirBZEdgolgSceDwG7SNdZSWqcSYhl69zIsuS1wxCY7wK9266OXw
    # NPcAbhVxB5vy4YyWJUPW5hhUop7d+uX5Vnsqm2mt2+8IPhrE77upQuHYWqQEniQ9VHr7ku21qLvn
    # 6VWFDlQEmgBxcnn9uk6okdEa06x9RUcHuPU1Mrz/Ca21sA5mAYny7dOWtpdVzVcjXtIMIFh5dShG
    # nfgSZPUbW5VVWHPp2jN00w3jkRJbv0FkhssSBVuHwuP/rww80kl3sGe7xvmYpkazhMgeHb4U9Or1
    # 61Q=
    #   =X6gI
    # -----END PGP SIGNATURE-----
    #
    #   --IKnJ4TqXo95ddlFrtwLSYpANZnV5KvJWV--



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
