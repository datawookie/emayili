template <- function(msg, name, ..., .envir = NULL) {
  if(!requireNamespace("gpg", quietly = TRUE)) {
    stop("Install {jinjar} to to use templates.")    # nocov
  }

  # Override values in environment.
  env <- as.environment(list(...))
  # Merge in parent environment (fallback if not defined in ...).
  parent.env(env) <- ifelse(is.null(.envir), parent.frame(), list2env(.envir))

  PATH <- NULL
  for (path in c(
    # Look in relative or absolute path.
    name,
    # Look in system folder.
    file.path(system.file(package = "emayili"), "template", name)
  )) {
    if (dir.exists(path)) {
      PATH <- path
      log_debug("Found template in {PATH}.")
      break
    }
  }
  if (is.null(PATH)) stop(glue("Unable to find '{name}' template."))

  # path_html <- file.path(path, "template.html")
  # path_text <- file.path(path, "template.txt")
  #
  # if (!file.exists(path_html)) stop("Unable to find HTML template.")
  # if (!file.exists(path_text)) stop("Unable to find text template.")
  #
  # template_html <- emayili:::read_text(path_html)
  # template_text <- emayili:::read_text(path_text)
  #
  # print("------------------------------------------------------")
  # print(sapply(ls(env), function(x) get(x, envir = env)))
  # print("------------------------------------------------------")
  #
  # template_html <- glue(template_html, .envir = env)
  # template_text <- glue(template_text, .envir = env)
  #
  # print("HTML")
  # print(template_html)
  # print("TEXT")
  # print(template_text)
}

# library(dplyr)
# library(glue)
#
# logo_path = "/home/foobar/Downloads/fathom-logo.png"
#
# envelope() %>%
#   template("fud", logo_path = "/home/wookie/Downloads/fathom-logo.png") %>%
#   as.character(details = TRUE)
#
# envelope() %>%
#   template("branded-letter", logo_path = "/home/wookie/Downloads/fathom-logo.png") %>%
#   as.character(details = TRUE)
#
# envelope() %>%
#   template("branded-letter") %>%
#   as.character(details = TRUE)
