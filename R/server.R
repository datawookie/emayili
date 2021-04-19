#' Create a SMTP server object.
#'
#' @param host DNS name or IP address of the SMTP server.
#' @param port Port that the SMTP server is listening on.
#' @param username Username for SMTP server.
#' @param password Password for SMTP server.
#' @param insecure Whether to ignore SSL issues.
#' @param reuse Whether the connection to the SMTP server should be left open for reuse.
#' @param ... Additional curl options. See \code{curl::curl_options()} for a list of supported options.
#'
#' @return A function which is used to send messages to the server.
#' @export
#' @examples
#' library(magrittr)
#'
#' # Set parameters for SMTP server (with username and password)
#' smtp <- server(host = "smtp.gmail.com",
#'                port = 465,
#'                username = "bob@gmail.com",
#'                password = "bd40ef6d4a9413de9c1318a65cbae5d7")
#'
#' # Set parameters for a (fake) testing SMTP server.
#' #
#' # More information about this service can be found at https://www.smtpbucket.com/.
#' #
#' smtp <- server(host = "mail.smtpbucket.com",
#'                port = 8025)
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
server <- function(host, port = 25, username = NULL, password = NULL, insecure = FALSE, reuse = TRUE,...) {
  sender <- function(msg, verbose = FALSE) {
    debugfunction <- if (verbose) function(type, msg) cat(readBin(msg, character()), file = stderr())

    recipients <- c(msg$header$To, msg$header$Cc, msg$header$Bcc)
    #
    if (length(recipients) < 1) stop("Must specify at least one email recipient.", call. = FALSE)

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

    protocol <- ifelse(port == 465, "smtps", "smtp")

    smtp_server <- sprintf("%s://%s:%d/", protocol, host, port)
    #
    if (verbose) {
      cat("Sending email to ", smtp_server, ".\n", file = stderr(), sep = "")
    }

    # Create an insistent version of send_mail().
    #
    # This is to mitigate occasional curl_fetch_memory() errors.
    #
    send_mail <- insistently(
      send_mail,
      rate = rate_backoff(
        max_times = 5,
        jitter = FALSE
      )
    )

    result <- send_mail(
      mail_from = msg$header$From,
      mail_rcpt = recipients,
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
