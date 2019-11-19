#' Add a single attachment to a message object
#'
#' @param msg A message object.
#' @param path Paths to files.
#' @param cid Associate a Content ID.
#' @return A message object.
#' @export
#' @examples
#' \dontrun{
#' msg <- envelope()
#' attachment(msg, "report.xlsx")
#' attachment(msg, c("visualisations.png", "report.pdf"))
#' attachment(msg, c("visualisations.png", "image.jpg"), cid = TRUE)
#' }
attachment <- function(msg, path, cid = NULL) {
  type <- guess_type(path, empty = NULL)

  if (length(path) != 1) stop("Can only add one attachment at a time!")

  con <- file(path, "rb")
  body <- readBin(con, "raw",  file.info(path)$size)
  close(con)

  mime <- mime(
    type,
    "base64",
    NULL,
    NULL,
    name = basename(path),
    filename = basename(path),
    content_transfer_encoding = "base64",
    cid = ifelse(is.null(cid), NA, cid)
  )

  mime$body <- base64encode(body, 76L, "\r\n")

  msg$parts <- c(msg$parts, list(mime))

  invisible(msg)
}

#' Add a multiple attachments to a message object
#'
#' @param msg
#' @param paths
#'
#' @return
#' @export
#'
#' @examples
attachments <- function(msg, paths) {
  for (path in paths) {
    msg <- msg %>% attachment(path)
  }

  invisible(msg)
}
