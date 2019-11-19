#' Add attachments to a message object
#'
#' @param msg A message object.
#' @param path Paths to files.
#' @param cid A vector of the same length of path containing either NA or string values specifying a Content ID for each attachment.
#' @return A message object.
#' @export
#' @examples
#' \dontrun{
#' msg <- envelope()
#' attachment(msg, "report.xlsx")
#' attachment(msg, c("visualisations.png", "report.pdf"), c("visualizations_01", NA))
#' }
attachment <- function(msg, path, cid = NULL){

  types <- guess_type(path, empty = NULL)

  #check if 'cid' is missing or else if it is correct
  if(missing(cid) || is.null(cid)) cid = rep(NA,length(path))
  else if(max(table(na.omit(cid)))>1) stop("Duplicate CIDs were found. Please provide unique identifiers.",call. = F)
  if(length(cid)!= length(path)) stop ("'cid' must be of the same length of 'path'. Use 'NA' if no ID should be assigned to a specific attachment",call. = F)


  for (i in seq_along(path)) {
    con <- file(path[i], "rb")
    body <- readBin(con, "raw",  file.info(path[i])$size)
    close(con)


    if(!is.na(cid[i])){
      mime <- mime(types[i], "base64", NULL, NULL,
                   name = basename(path[i]),
                   filename = basename(path[i]),
                   content_transfer_encoding = "base64", cid=as.character(cid[i]))

    } else {
      mime <- mime(types[i], "base64", NULL, NULL,
                   name = basename(path[i]),
                   filename = basename(path[i]),
                   content_transfer_encoding = "base64")

    }

    mime$body <- base64encode(body, 76L, "\r\n")

    msg$parts <- c(msg$parts, list(mime))
  }

  invisible(msg)
}
