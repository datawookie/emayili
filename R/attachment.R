#' Add attachments to a message object
#'
#' @param msg A message object.
#' @param path Path to file.
#' @param type MIME type or \cite{NA}, which will result in a guess based on file extension.
#' @param cid Content-ID or \code{NA}.
#' @param disposition Should the content be displayed inline or as an attachment?
#' @return A message object.
#' @export
#' @examples
#' library(magrittr)
#'
#' download.file("https://bit.ly/2P4LUO8", "cats.jpg")
#'
#' png("scatter.png")
#' plot(1:10)
#' dev.off()
#'
#' write.csv(mtcars, "mtcars.csv")
#'
#' msg <- envelope() %>%
#'   attachment("mtcars.csv") %>%
#'   attachment("cats.jpg", type = "image/jpeg") %>%
#'   attachment("scatter.png", cid = "scatter")
#'
#' file.remove("mtcars.csv", "scatter.png", "cats.jpg")
attachment <-
  function(msg,
           path,
           type = NA,
           cid = NA,
           disposition = NA) {
    if (length(path) != 1)
      stop("Must be precisely one attachment.", call. = F)

    if (!is.na(type)) {
      # Could use mime::mimemap to map from specific extensions to MIME types,
      # so that the following would give the same result:
      #
      # attachment(..., type = "pdf")
      # attachment(..., type = "application/pdf")
    } else {
      type <- guess_type(path, empty = "application/octet-stream")
    }

    if (is.na(disposition)) {
      disposition <- ifelse(grepl("text", type),
                            "inline",
                            ifelse(
                              grepl("image", type),
                              ifelse(!is.na(cid), "inline", "attachment"),
                              "attachment"
                            ))
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
