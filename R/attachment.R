#' Add an attachment to a message object
#'
#' @param msg A message object.
#' @param path Path to a file.
#' @return A message object.
#' @export
#' @examples
#' \dontrun{
#' msg <- envelope()
#' attachment(msg, "report.xlsx")
#' attachment(msg, "visualisations.png")
#' }
attachment <- function(msg, path){
  type <- guess_type(path, empty = NULL)

  con <- file(path, "rb")
  body <- readBin(con, "raw",  file.info(path)$size)
  close(con)

  mime <- mime(type, "base64", NULL, NULL,
               name = basename(path),
               filename = basename(path),
               content_transfer_encoding = "base64")

  mime$body <- base64encode(body, 76L, "\r\n")

  msg$parts <- c(msg$parts, list(mime))

  invisible(msg)
}
