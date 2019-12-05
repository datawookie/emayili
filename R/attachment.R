#' Add attachments to a message object
#'
#' @param msg A message object.
#' @param path Path to file.
#' @param cid Content-ID or \code{NA}.
#' @param mime_type fixed mime type without guessing. Use \link[mime]{mimemap} to set. (optional)
#' @return A message object.
#' @export
#' @examples
#' \dontrun{
#' msg <- envelope()
#' attachment(msg, "report.xlsx")
#' attachment(msg, "cat.png")
#' attachment(msg, "visualisations.png", "visuals")
#' }
attachment <- function(msg, path, cid = NA, disposition = NA, mime_type){
  if (length(path) != 1) stop("Must be precisely one attachment.", call. = F)

  type <- guess_type(path, empty = NULL)
  if (!missing(mime_type)) type <- mime_type

  if(is.na(disposition)) {
    disposition <- ifelse(
      grepl("text", type),
      "inline",
      ifelse(
        grepl("image", type),
        ifelse(!is.na(cid), "inline", "attachment"),
        "attachment"
      )
    )
  }

  con <- file(path, "rb")
  body <- readBin(con, "raw",  file.info(path)$size)
  close(con)

  mime <- mime(
    type,
    disposition,
    "base64",
    NULL,
    NULL,
    cid = as.character(cid),
    name = basename(path),
    filename = basename(path)
  )

  mime$body <- base64encode(body, 76L, "\r\n")

  msg$parts <- c(msg$parts, list(mime))

  invisible(msg)
}
