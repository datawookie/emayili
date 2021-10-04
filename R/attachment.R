#' Add attachments to a message object
#'
#' @param msg A message object.
#' @param path Path to file.
#' @param name Name to be used for attachment (defaults to base name of \code{path}).
#' @param type MIME type or \cite{NA}, which will result in a guess based on file extension.
#' @param cid Content-ID or \code{NA}.
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

  body <- other(path, name, type, cid)

  msg <- append(msg, body)

  if (get_option_invisible()) invisible(msg) else msg
}
