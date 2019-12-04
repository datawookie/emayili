#' Add attachments to a message object
#'
#' @param msg A message object.
#' @param path Paths to files.
#' @param cid a content id.
#' @return A message object.
#' @export
#' @examples
#' \dontrun{
#' msg <- envelope()
#' attachment(msg, "report.xlsx")
#' attachment(msg, c("visualisations.png", "report.pdf"))
#' }
attachment <- function(msg, path, cid = round(runif(1,0,10000))){

  types <- guess_type(path, empty = NULL)

  for (i in seq_along(path)) {
    con <- file(path[i], "rb")
    body <- readBin(con, "raw",  file.info(path[i])$size)
    close(con)

    mime <- mime(
      types[i],
      "base64",
      NULL,
      NULL,
      name = basename(path[i]),
      filename = basename(path[i]),
      cid=cid
    )

    mime$body <- base64encode(body, 76L, "\r\n")

    msg$parts <- c(msg$parts, list(mime))
  }

  invisible(msg)
}
