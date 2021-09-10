#' Add attachments to a message object
#'
#' @param msg A message object.
#' @param path Path to file.
#' @param name Name to be used for attachment (defaults to base name of \code{path}).
#' @param type MIME type or \cite{NA}, which will result in a guess based on file extension.
#' @param cid Content-ID or \code{NA}.
#' @param disposition Should the content be displayed inline or as an attachment? Valid options are \code{"inline"} and \code{"attachment"}. If set to \code{NA} then will guess appropriate value.
#' @return A message object.
#' @export
#' @examples
#' library(magrittr)
#'
#' path_mtcars  <- tempfile(fileext = ".csv")
#' path_scatter <- tempfile(fileext = ".png")
#' path_cats    <- system.file("cats.jpg", package = "emayili")
#'
#' write.csv(mtcars, path_mtcars)
#'
#' png(path_scatter)
#' plot(1:10)
#' dev.off()
#'
#' msg <- envelope() %>%
#'   attachment(path_mtcars) %>%
#'   # This attachment will have file name "cats.jpg".
#'   attachment(path_cats, name = "cats.jpg", type = "image/jpeg") %>%
#'   attachment(path_scatter, cid = "scatter")
#'
#' file.remove(path_scatter, path_mtcars)
attachment <- function(msg, path, name = NA, type = NA, cid = NA) {
    if (length(path) != 1)
      stop("Must be precisely one attachment.", call. = F)

  body <- other(path, type)

  msg$parts <- c(msg$parts, list(body))

    # if (!is.na(type)) {
    #   # Could use mime::mimemap to map from specific extensions to MIME types,
    #   # so that the following would give the same result:
    #   #
    #   # attachment(..., type = "pdf")
    #   # attachment(..., type = "application/pdf")
    # } else {
    #   type <- guess_type(path, empty = "application/octet-stream")
    # }
    #
    # if (is.na(disposition)) {
    #   disposition <- ifelse(grepl("text", type),
    #                         "inline",
    #                         ifelse(
    #                           grepl("image", type),
    #                           ifelse(!is.na(cid), "inline", "attachment"),
    #                           "attachment"
    #                         ))
    # }
    #
    # con <- file(path, "rb")
    # body <- readBin(con, "raw",  file.info(path)$size)
    # close(con)
    #
    # mime <- mime(
    #   type,
    #   disposition,
    #   NULL,
    #   "base64",
    #   cid = as.character(cid),
    #   filename = ifelse(is.na(name), basename(path), name)
    # )
    #
    # mime$body <- base64encode(body, 76L, "\r\n")
    #
    # msg$parts <- c(msg$parts, list(mime))

    invisible(msg)
  }
