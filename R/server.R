#' Create a SMTP server object.
#'
#' Create an object which can be used to send messages to an SMTP server.
#'
#' These functions return a function that can then be called with a message
#' object. This function mediates the interaction with the Simple Mail Transfer
#' Protocol (SMTP) server.
#'
#' SMTP is a plain text protocol, which means that it is not secure. The secure
#' variant, SMTPS, comes in two flavours: TLS and StartTLS. With TLS (also
#' called Implicit TLS) the connection with the server is initiated using an
#' Secure Socket Layer (SSL) or Transport Layer Security (TLS) certificate. Such
#' a connection is secure from the start. By contract, a StartTLS connection is
#' initiated in plain text and then upgraded to TLS if possible. By convention
#' TLS operates on port 465 and StartTLS on port 587.
#'
#' The specifications of an SMTP server are given in an SMTP URL, which takes one
#' of the following forms:
#'
#' - `mail.example.com` — hostname only
#' - `mail.example.com:587` — hostname and port
#' - `smtp://mail.example.com` — SMTP URL (default port)
#' - `smtps://mail.example.com` — SMTPS URL (default port)
#' - `smtp://mail.example.com:25` — SMTP URL (explicit port)
#' - `smtps://mail.example.com:587` — SMTPS URL (explicit port)
#'
#' @name server
#'
#' @param host DNS name or IP address of the SMTP server.
#' @param port Port that the SMTP server is listening on.
#' @param username Username for SMTP server.
#' @param password Password for SMTP server or API key.
#' @param insecure Whether to ignore SSL issues.
#' @param reuse Whether the connection to the SMTP server should be left open for reuse.
#' @param helo The HELO domain name of the sending host. If left as \code{NA} then will use local host name.
#' @param protocol Which protocol (SMTP or SMTPS) to use for communicating with
#'  the server. Default will choose appropriate protocol based on port.
#' @param use_ssl Whether to use SSL. If not specified then SSL will be used if the port is 465 or 587.
#'  This enables SSL on non-standard ports.
#' @param test Test login to server.
#' @param max_times Maximum number of times to retry.
#' @param pause_base Base delay (in seconds) for exponential backoff. See \link[purrr]{rate_backoff}.
#' @param ... Additional curl options. See \code{curl::curl_options()} for a list of supported options.
#'
#' @return A function which is used to send messages to the server.
#' @export
#' @examples
#' # Set parameters for SMTP server (with username and password).
#' smtp <- server(
#'   host = "smtp.gmail.com",
#'   port = 587,
#'   username = "bob@gmail.com",
#'   password = "bd40ef6d4a9413de9c1318a65cbae5d7"
#' )
#'
#' # Set parameters for a (fake) testing SMTP server.
#' #
#' # More information about this service can be found at https://www.smtpbucket.com/.
#' #
#' smtp <- server(
#'   host = "mail.smtpbucket.com",
#'   port = 8025
#' )
#'
#' # Create a message
#' msg <- envelope() %>%
#'   from("bob@gmail.com") %>%
#'   to("alice@yahoo.com")
#'
#' # Send message (verbose output from interactions with server)
#' \dontrun{
#' smtp(msg, verbose = TRUE)
#' }
#'
#' # To confirm that the message was sent, go to https://www.smtpbucket.com/ then:
#' #
#' # - fill in "bob@gmail.com" for the Sender field and
#' # - fill in "alice@yahoo.com" for the Recipient field then
#' # - press the Search button.
#'
#' # With explicit HELO domain.
#' #
#' smtp <- server(
#'   host = "mail.example.com",
#'   helo = "client.example.com"
#' )
server <- function(host,
                   port = 25,
                   username = NULL,
                   password = NULL,
                   insecure = FALSE,
                   reuse = TRUE,
                   helo = NA,
                   protocol = NA,
                   use_ssl = NA,
                   test = FALSE,
                   pause_base = 1,
                   max_times = 5,
                   ...) {
  # See curl::curl_options() for available options.
  #
  # * SSL
  #
  # - If you get the "The certificate chain was issued by an authority that is not trusted." error then
  #   can add in ssl_verifypeer = FALSE.
  # - Other flags:
  #
  #   - ssl_verifyhost
  #   - ssl_verifypeer
  #   - ssl_verifystatus
  #
  #   Run curl_options('ssl') to see other options.
  #
  if (insecure) {
    ssl_verifypeer <- FALSE
  } else {
    ssl_verifypeer <- TRUE
  }

  port <- as.integer(port)
  if (is.na(use_ssl)) {
    if (port %in% c(465, 587)) {
      use_ssl <- 1
    } else {
      use_ssl <- 0
    }
  } else {
    # If use_ssl can be interpreted as Boolean then set accordingly.
    use_ssl <- ifelse(use_ssl, 1, 0)
    # If not then set use_ssl to false.
    #
    # NOTE: Need to check for NA again here because NA can come from previous ifelse().
    #
    use_ssl <- ifelse(is.na(use_ssl), 0, use_ssl)
  }

  # Create an insistent version of send_mail().
  #
  # This is to mitigate occasional curl_fetch_memory() errors.
  #
  if (max_times > 1) {
    send_mail <- insistently(
      send_mail,
      rate = rate_backoff(
        max_times = max_times,
        pause_base = pause_base,
        pause_cap = pause_base * 2**max_times,
        jitter = FALSE
      )
    )
  }

  smtp_server <- smtp_url(host, port, protocol, helo)

  if (test) {
    log_debug("Check login to {smtp_server}.")

    response <- safely(send_mail)(
      c(),
      c(),
      message = "",
      smtp_server = smtp_server,
      use_ssl = use_ssl,
      username = username,
      password = password,
      verbose = FALSE
    )

    if (!is.null(response$error)) {
      log_error("Login denied.")
      stop(response$error$message)
    } else {
      log_debug("Login successful.")
    }
  }

  function(msg, verbose = FALSE) {
    debugfunction <- if (verbose) function(type, msg) cat(readBin(msg, character()), file = stderr()) # nocov

    if (is.null(from(msg))) stop("Must specify who the email is from.")

    recipients <- c(to(msg), cc(msg), bcc(msg))
    #
    if (length(recipients) < 1) stop("Must specify at least one email recipient.")

    if (verbose) {
      log_debug("Sending email to {smtp_server}.")
    }

    result <- send_mail(
      # Strip descriptive name (retained in email header so that it appears
      # in email client).
      mail_from = raw(from(msg)),
      mail_rcpt = raw(recipients),
      # Get character representation of envelope object.
      message = as.character(msg, encode = TRUE),
      smtp_server = smtp_server,
      username = username,
      password = password,
      verbose = verbose,
      debugfunction = debugfunction,
      ssl_verifypeer = ssl_verifypeer,
      use_ssl = use_ssl,
      forbid_reuse = !reuse,
      ...
    )

    invisible(result)
  }
}

#' @rdname server
#'
#' @section Gmail:
#'
#' If you're having trouble authenticating with Gmail then you should try the following:
#'
#' - enable 2-factor authentication and
#' - create an app password.
#'
#' Then use the app password rather than your usual account password.
#'
#' @export
#'
#' @examples
#'
#' # Set parameters for Gmail SMTP server. The host and port are implicit.
#' smtp <- gmail(
#'   username = "bob@gmail.com",
#'   password = "bd40ef6d4a9413de9c1318a65cbae5d7"
#' )
gmail <- function(username,
                  password,
                  ...) {
  # Retrieve function.
  fcall <- match.call(expand.dots = TRUE)
  # Substitute server() as function to call.
  fcall[[1]] <- server
  # Fill in host and port arguments.
  fcall$host <- "smtp.gmail.com"
  fcall$port <- ifelse(is.null(fcall$port), 587, fcall$port)
  # Now run the function call.
  eval(fcall, parent.frame())
}

#' @rdname server
#'
#' @section Sendgrid:
#'
#' To use SendGrid you'll need to first \href{https://docs.sendgrid.com/for-developers/sending-email/integrating-with-the-smtp-api}{create an API key}. # nolint
#' Then use the API key as the password.
#'
#' SendGrid will accept messages on ports 25, 587 and 2525 (using SMTP) as well
#' as 465 (using SMTPS).
#'
#' @export
#'
#' @examples
#'
#' # Set API key for SendGrid SMTP server.
#' smtp <- sendgrid(
#'   password = "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"
#' )
sendgrid <- function(password,
                     ...) {
  fcall <- match.call(expand.dots = TRUE)

  fcall[[1]] <- server
  fcall$host <- "smtp.sendgrid.net"
  fcall$port <- ifelse(is.null(fcall$port), 587, fcall$port)
  fcall$username <- "apikey"

  eval(fcall, parent.frame())
}

#' @rdname server
#'
#' @section Mailgun:
#'
#' To use Mailgun you'll need to first register a sender domain. This will then
#' be assigned a username and password.
#'
#' Mailgun will accept messages on ports 25 and 587 (using SMTP) as well as 465
#' (using SMTPS).
#'
#' @export
#'
#' @examples
#'
#' # Set username and password for Mailgun SMTP server.
#' smtp <- mailgun(
#'   username = "postmaster@sandbox9ptce35fdf0b31338dec4284eb7aaa59.mailgun.org",
#'   password = "44d072e7g2b5f3bf23b2b642da0fe3a7-2ac825a1-a5be680a"
#' )
mailgun <- function(username,
                    password,
                    ...) {
  fcall <- match.call(expand.dots = TRUE)

  fcall[[1]] <- server
  fcall$host <- "smtp.mailgun.org"
  fcall$port <- ifelse(is.null(fcall$port), 587, fcall$port)

  eval(fcall, parent.frame())
}

#' @rdname server
#'
#' @section Sendinblue:
#'
#' To use Sendinblue you'll need to first create an account. You'll find your
#' SMTP username and password in the SMTP & API section of your account
#' settings.
#'
#' @export
#'
#' @examples
#'
#' # Set username and password for Sendinblue SMTP server.
#' smtp <- sendinblue(
#'   username = "bob@gmail.com",
#'   password = "xsmtpsib-c75cf91323adc53a1747c005447cbc9a893c35888635bb7bef1a624bf773da33"
#' )
sendinblue <- function(username,
                       password,
                       ...) {
  fcall <- match.call(expand.dots = TRUE)

  fcall[[1]] <- server
  fcall$host <- "smtp-relay.sendinblue.com"
  fcall$port <- 587

  eval(fcall, parent.frame())
}

#' @rdname server
#'
#' @section MailerSend:
#'
#' To use MailerSend you'll need to first create an account. You'll find your
#' SMTP username and password under Domains. See \href{https://www.mailersend.com/help/smtp-relay}{How to send emails
#' via SMTP with MailerSend}.
#'
#' Although this is not likely to be a problem in practice, MailerSend insists
#' that all messages have at minimum a valid subject and either text or HTML
#' content.
#'
#' @export
#'
#' @examples
#'
#' # Set username and password for MailerSend SMTP server.
#' smtp <- mailersend(
#'   username = "NS_Pf3ALM@gmail.com",
#'   password = "e5ATWLlTnWWDaKeE"
#' )
mailersend <- function(username,
                       password,
                       ...) {
  fcall <- match.call(expand.dots = TRUE)

  fcall[[1]] <- server
  fcall$host <- "smtp.mailersend.net"
  fcall$port <- 587

  eval(fcall, parent.frame())
}

#' @rdname server
#'
#' @section Mailfence:
#'
#' To use Mailfence you'll need to create a premium account.
#'
#' @export
#'
#' @examples
#'
#' # Set username and password for Mailfence SMTP server.
#' smtp <- mailfence(
#'   username = "bob",
#'   password = "F!Uosd6xbhSjd%63"
#' )
mailfence <- function(username,
                      password,
                      ...) {
  fcall <- match.call(expand.dots = TRUE) # nocov start

  fcall[[1]] <- server
  fcall$host <- "smtp.mailfence.com"
  fcall$port <- 465

  eval(fcall, parent.frame()) # nocov end
}

#' @rdname server
#'
#' @section ZeptoMail:
#'
#' SMTP Bucket is a fake SMTP server that captures all the messages it receives
#' and makes them available through a website or REST API.
#'
#' @export
#'
#' @examples
#'
#' # Set password for ZeptoMail SMTP server.
#' # nolint start
#' smtp <- zeptomail("yA6KbHsL4l2mmI8Ns0/fs9iSTj8yG0dYBgfIG0j6Fsv4P2uV32xh8ciEYNYlRkgCC7wRfkgWA==")
#' # nolint end
zeptomail <- function(password,
                      ...) {
  fcall <- match.call(expand.dots = TRUE) # nocov start

  fcall[[1]] <- server
  fcall$host <- "smtp.zeptomail.eu"
  # Available ports: 465 (SSL) and 587 (TLS).
  fcall$port <- 587
  fcall$username <- "emailapikey"

  eval(fcall, parent.frame()) # nocov end
}

#' @rdname server
#'
#' @section SMTP Bucket:
#'
#' SMTP Bucket is a fake SMTP server that captures all the messages it receives
#' and makes them available through a website or REST API.
#'
#' @export
#'
#' @examples
#'
#' # SMTP Bucket server.
#' smtp <- smtpbucket()
smtpbucket <- function(...) {
  fcall <- match.call(expand.dots = TRUE)

  fcall[[1]] <- server
  fcall$host <- "mail.smtpbucket.com"
  fcall$port <- 8025

  eval(fcall, parent.frame())
}
