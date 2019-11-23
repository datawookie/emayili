#' Add attachments to a message object
#'
#' @param msg A message object.
#' @param path Paths to files.
#' @param cid A vector of the same length as \code{path} containing for each attachment either \code{NA} or a Content ID.
#' @return A message object.
#' @export
#' @examples
#' \dontrun{
#' msg <- envelope()
#' attachment(msg, "report.xlsx")
#' attachment(msg, c("visualisations.png", "report.pdf"), c("visuals", NA))
#' }
attachment <- function(msg, path, cid = NULL){
  types <- guess_type(path, empty = NULL)

  if(missing(cid) || is.null(cid)) cid = rep(NA, length(path))
  else if(max(table(na.omit(cid))) > 1) stop("Duplicate CIDs were found. Please provide unique identifiers.", call. = F)
  if(length(cid)!= length(path)) stop ("cid must be of the same length as path. Use NA if no ID should be assigned to a specific attachment.", call. = F)

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
      content_transfer_encoding = "base64",
      cid = as.character(cid[i])
    )

    mime$body <- base64encode(body, 76L, "\r\n")

    msg$parts <- c(msg$parts, list(mime))
  }

  invisible(msg)
}
