#' Create a SMTP server object.
#'
#' @param host DNS name or IP address of the SMTP server.
#' @param port Port that the SMTP server is listening on.
#' @param username Username for SMTP server.
#' @param password Password for SMTP server.
#' @param insecure Whether to ignore SSL issues.
#' @param reuse Whether the connection to the SMTP server should be left open for reuse.
#' @param helo The HELO domain name of the sending host. If left as \code{NA} then will use local host name.
#' @param max_times Maximum number of times to retry.
#' @param pause_base Base delay (in seconds) for exponential backoff. See \link[purrr]{rate_backoff}.
#' @param ... Additional curl options. See \code{curl::curl_options()} for a list of supported options.
#'
#' @return A function which is used to send messages to the server.
#' @export
#' @examples
#' library(magrittr)
#'
#' # Set parameters for SMTP server (with username and password)
#' smtp <- server(
#'   host = "smtp.gmail.com",
#'   port = 465,
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
#' smtp <- server(host = "mail.example.com",
#'                helo = "client.example.com")
server <- function(
  host,
  port = 25,
  username = NULL,
  password = NULL,
  insecure = FALSE,
  reuse = TRUE,
  helo = NA,
  pause_base = 1,
  max_times = 5,
  ...) {
  function(msg, verbose = FALSE) {
    debugfunction <- if (verbose) function(type, msg) cat(readBin(msg, character()), file = stderr()) # nocov

    if (is.null(from(msg))) stop("Must specify who the email is from.")

    recipients <- c(to(msg), cc(msg), bcc(msg))
    #
    if (length(recipients) < 1) stop("Must specify at least one email recipient.")

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
      ssl_verifypeer = FALSE
    } else {
      ssl_verifypeer = TRUE
    }

    port <- as.integer(port)
    if (port %in% c(465, 587)) {
      use_ssl = 1
    } else {
      use_ssl = 0
    }

    helo <- ifelse(is.na(helo), "", helo)

    smtp_server <- sprintf("%s:%d/%s", host, port, helo)
    #
    if (verbose) {
      cat("Sending email to ", smtp_server, ".\n", file = stderr(), sep = "")
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

    result <- send_mail(
      # Strip descriptive name (retained in email header so that it appears
      # in email client).
      mail_from = raw(from(msg)),
      mail_rcpt = raw(recipients),
      #
      message = as.character(msg),
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
