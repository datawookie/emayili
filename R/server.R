#' Create a SMTP server object.
#'
#' @param host DNS name or IP address of the SMTP server.
#' @param port Port that the SMTP server is listening on.
#' @param username Username for SMTP server.
#' @param password Password for SMTP server.
#' @param insecure Whether to ignore SSL issues.
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
#' smtp(msg, verbose = TRUE)
#'
#' # To confirm that the message was sent, go to https://www.smtpbucket.com/ then:
#' #
#' # - fill in "bob@gmail.com" for the Sender field and
#' # - fill in "alice@yahoo.com" for the Recipient field then
#' # - press the Search button.
server <- function(host, port = 25, username = NULL, password = NULL, insecure = FALSE) {
  function(msg, verbose = FALSE){
    tmpfile = tempfile()
    #
    writeLines(message(msg), tmpfile)

    h <- new_handle(
      mail_from = msg$header$From,
      mail_rcpt = c(msg$header$To, msg$header$Cc, msg$header$Bcc)
    )

    if (!is.null(username)) {
      handle_setopt(h, username = username, password = password)
    }

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
      handle_setopt(h, ssl_verifypeer = FALSE)
    }

    # Setup to capture output from underlying command.
    #
    # See https://github.com/jeroen/curl/issues/120.
    #
    log <- rawConnection(raw(), 'r+')
    on.exit(close(log))
    handle_setopt(h,
                  debugfunction = function(type, data) {
                    writeBin(data, log)
                  },
                  verbose = verbose
    )

    con <- file(tmpfile, open = 'rb')
    #
    handle_setopt(h, readfunction = function(nbytes, ...) {
      readBin(con, raw(), nbytes)
    }, upload = TRUE)

    port <- as.integer(port)
    if (port %in% c(465, 587)) {
      handle_setopt(h, use_ssl = 1)
    }

    protocol <- ifelse(port == 465, "smtps", "smtp")

    url = sprintf("%s://%s:%d", protocol, host, port)
    #
    if (verbose) {
      cat("Sending email to ", url, ".\n", file = stderr())
    }

    result <- curl_fetch_memory(url, handle = h)

    if (verbose) {
      cat(rawToChar(rawConnectionValue(log)), file = stderr())
    }

    close(con)

    invisible(result)
  }
}
