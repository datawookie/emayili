# NOTE: There is now a send_mail() function in {curl} but it does not have the full range of functionality
# below (like specifying port).

#' Create a SMTP server object
#'
#' @param host DNS name or IP address of the SMTP server.
#' @param port Port that the SMTP server is listening on.
#' @param username Username for SMTP server.
#' @param password Password for SMTP server.
#'
#' @return A function which is used to send messages to the server.
#' @export
#' @examples
#' library(magrittr)
#'
#' smtp <- server(host = "smtp.gmail.com",
#'                port = 465,
#'                username = "bob@gmail.com",
#'                password = "bd40ef6d4a9413de9c1318a65cbae5d7")
#' msg <- envelope() %>%
#'   from("bob@gmail.com") %>%
#'   to("alice@yahoo.com")
#' \dontrun{
#' smtp(msg, verbose = TRUE)
#' }
server <- function(host, port, username, password) {
  function(msg, verbose = FALSE){
    tmpfile = tempfile()
    #
    writeLines(message(msg), tmpfile)

    # If you get the "The certificate chain was issued by an authority that is not trusted." error then
    # can add in ssl_verifypeer = FALSE.
    #
    h <- new_handle(username = username,
                    password = password,
                    mail_from = msg$header$From,
                    mail_rcpt = c(msg$header$To, msg$header$Cc, msg$header$Bcc))

    con <- file(tmpfile, open = 'rb')
    #
    handle_setopt(h, readfunction = function(nbytes, ...) {
      readBin(con, raw(), nbytes)
    }, upload = TRUE)

    handle_setopt(h, verbose = verbose)

    port <- as.integer(port)
    if (port %in% c(465, 587)) {
      handle_setopt(h, use_ssl = 1)
    }

    protocol <- ifelse(port == 465, "smtps", "smtp")

    url = sprintf("%s://%s:%d", protocol, host, port)
    #
    if (verbose) base::message("Sending email to ", url, ".")

    curl_fetch_memory(url, handle = h)

    close(con)
  }
}
