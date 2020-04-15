#' Create a SMTP server object.
#'
#' @param host DNS name or IP address of the SMTP server.
#' @param port Port that the SMTP server is listening on.
#' @param username Username for SMTP server.
#' @param password Password for SMTP server.
#' @param insecure Whether to ignore SSL issues.
#' @param ... Additional curl options (run curl_options() for a list of supported options)
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
server <- function(host, port = 25, username = NULL, password = NULL, insecure = FALSE, ...) {
  function(msg, verbose = FALSE){
    smtp_server <- paste(host, port, sep = ":")
    debugfunction <- if (verbose) function(type, data) write(rawToChar(data), stderr())

    result <- send_mail(
      mail_from = msg$header$From,
      mail_rcpt = c(msg$header$To, msg$header$Cc, msg$header$Bcc),
      message = message(msg),
      smtp_server = smtp_server,
      username = username,
      password = password,
      verbose = verbose,
      debugfunction = debugfunction,
      ...
    )

    invisible(result)
  }
}
