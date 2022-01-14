#' Add attachments to a message object
#'
#' @param msg A message object.
#' @param path Path to file.
#' @param name Name to be used for attachment (defaults to base name of \code{path}).
#' @param type MIME type or \cite{NA}, which will result in a guess based on file extension.
#' @param cid Content-ID or \code{NA}.
#' @param disposition How is attachment to be presented (\code{"inline"} or \code{"attachment"})?
#' @return A message object.
#' @export
#' @examples
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
attachment <- function(msg, path, name = NA, type = NA, cid = NA, disposition = "attachment") {
  if (length(path) != 1)
    stop("Must be precisely one attachment.", call. = F)

  body <- other(path, name, type, cid, disposition)

  msg <- append(msg, body)

  if (get_option_invisible()) invisible(msg) else msg # nocov
}

attach_images <- function(
  msg,
  content,
  disposition,
  charset,
  encoding,
  css_files,
  language
) {
  images <- content %>% html_nodes("img")

  images <- map(images, function(img) {
    log_debug("* Processing image in HTML.")
    src = img %>% html_attr("src")
    if (is.na(src)) stop("Image doesn't have src attribute.")

    if (grepl("^cid:", src)) {
      log_debug("- Image is already CID. Skipping.")
    } else {
      tmp <- tempfile()
      log_debug("- Temporary location: {tmp}")

      if (grepl("^data", src)) {
        log_debug("- Image is Base64 encoded. Decoding.")
        writeBin(
          base64decode(sub("^data:[^;]+;base64,", "", src)),
          tmp
        )
        ext <- sub(";.*$", "", sub("^data:image/", "", src))
      } else {
        if (grepl("^http", src)) {
          log_debug("- Image is at URL. Downloading.")
          download.file(src, tmp, quiet = TRUE)
        } else {
          log_debug("- Assuming image is a local file. Copying.")
          file.copy(src, tmp)
        }
        ext <- file.ext(src)
      }
      log_debug("- Image extension: ", ext)
      path <- paste(tmp, ext, sep = ".")
      file.rename(tmp, path)
      info <- file.info(path)
      log_debug("- Image file size: ", info$size)
      log_debug("- Final location:     {path}")

      cid <- hexkey(basename(path))
      xml_attr(img, "src") <- sprintf("cid:%s", cid)

      path
    }
  })

  body <- text_html(
    content, disposition, charset, encoding,
    css = read_text(css_files),
    language = language
  )

  images <- images[!sapply(images, is.null)]

  if (length(images)) {
    related <- multipart_related() %>% append(body)

    # TODO: Something similar is done in manifest(). Can we refactor?

    for (path in images) {
      cid <- hexkey(basename(path))

      related <- append(
        related,
        other(
          filename = path,
          cid = cid,
          disposition = "inline"
        )
      )
    }

    msg <- append(msg, related)
  } else {
    msg <- append(msg, body)
  }

  msg
}
