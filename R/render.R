#' @export
render <- function(msg, path){
  body <- mime("text/html", "quoted-printable", NULL, "utf-8")

  html <- readChar("foo.html", file.info("foo.html")$size)
  html <- sub("^<!DOCTYPE html>\n", "", html)

  # body$body <- "<h1>This is the HTML Section!</h1>"
  body$body <- html

  msg$parts <- c(msg$parts, list(body))

  invisible(msg)
}
