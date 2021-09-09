# MIME
#  ├── mixed
#  └── related
#
MIME <- function() {
  structure(
    list(
    ),
    class = "MIME"
  )
}

related <- function() {
  structure(
    list(
    ),
    class = c("related", "MIME")
  )
}

mixed <- function() {
  structure(
    list(
    ),
    class = c("mixed", "MIME")
  )
}

format.MIME <- function(x) "MIME"
format.related <- function(x) c("related", NextMethod())
format.mixed <- function(x) c("mixed", NextMethod())

x <- MIME()
y <- related()
z <- mixed()

print(x)
print(y)
print(z)
