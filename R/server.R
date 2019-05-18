#' Create a SMTP server object
#'
#' @family server
#' @export
server <- function(host, port, username, password) {
  function(msg, verbose = FALSE){
    tmpfile = tempfile()
    #
    writeLines(message(msg), tmpfile)

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

    if (port %in% c(465, 587)) {
      handle_setopt(h, use_ssl = 1)
    }

    protocol <- ifelse(port == 465, "smtps", "smtp")

    url = sprintf("%s://%s:%d", protocol, host, port)

    curl_fetch_memory(url, handle = h)

    close(con)
  }
}
