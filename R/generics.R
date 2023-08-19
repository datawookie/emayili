after <- function(x, child) {
  UseMethod("after", x)
}

before <- function(x, child) {
  UseMethod("before", x)
}
