library(glue)

# MIME
#  ├── mixed
#  └── related
#
MIME <- function(children = list()) {
  # If just a single child, convert to list.
  if (!all(class(children) == c("list"))) children <- list(children)
  # Check that all children are MIME.
  for (child in children) {
    if (!("MIME" %in% class(child))) stop("Child is not a MIME object.", call. = FALSE)
  }

  structure(
    list(
      boundary = emayili:::hexkey(),
      children = children
    ),
    class = "MIME"
  )
}

related <- function(...) {
  structure(
    c(
      MIME(...),
      list(
      )
    ),
    class = c("related", "MIME")
  )
}

mixed <- function(...) {
  structure(
    c(
      MIME(...),
      list(
      )
    ),
    class = c("mixed", "MIME")
  )
}

format_mime <- function(x) {
  children <- sapply(x$children, format_mime)
  #
  body <- c(
    # Head.
    glue('Content-Type: multipart/{class(x)[1]}; boundary="{x$boundary}"\n', .trim = FALSE),
    # Children (if any).
    if(length(children)) children else NULL,
    # Foot.
    glue('--{x$boundary}--')
  )

  paste(body, collapse = "\n")
}

as.character.MIME <- function(x, ...) {
  # Check if this is a derived class.
  class = class(x)
  if (length(class) == 1) {
    # Base class.
    "BASE"
  } else {
    "DERIVED"
  }



  # print(class(x))
  format_mime(x)
}
as.character.related <- function(x, ...) as.character.MIME(x, ...)
as.character.mixed <- function(x, ...) as.character.MIME(x, ...)

format.MIME <- function(x) as.character(x)
format.related <- function(x) "FOO"
format.mixed <- function(x) "BAR"

print.MIME <- function(x) {
  cat(as.character(x))
}
print.related <- function(x) NextMethod()
print.mixed <- function(x) NextMethod()

x <- MIME()
y <- related()
z0 <- mixed(children = y)
z2 <- mixed(children = list(y, y))

print(x)
print(y)
print(z0)
print(z2)
