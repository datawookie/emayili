template <- function(msg, name, ..., .envir = NULL) {
  if (is.null(.envir)) {
    env_parent <- parent.frame()
  } else {
    env_parent <- list2env(.envir)
  }

  # Override values from enclosing environment.
  print(list(...))
  env <- as.environment(list(...))
  # Merge in parent environment.
  parent.env(env) <- env_parent

  path <- file.path("/home/wookie/proj/438-emayili/inst/template", name)

  path_html <- file.path(path, "template.html")
  path_text <- file.path(path, "template.txt")

  if (!file.exists(path_html)) stop("Unable to find HTML template.")
  if (!file.exists(path_text)) stop("Unable to find text template.")

  template_html <- emayili:::read_text(path_html)
  template_text <- emayili:::read_text(path_text)

  print("------------------------------------------------------")
  print(sapply(ls(env), function(x) get(x, envir = env)))
  print("------------------------------------------------------")

  template_html <- glue(template_html, .envir = env)
  template_text <- glue(template_text, .envir = env)

  print("HTML")
  print(template_html)
  print("TEXT")
  print(template_text)
}
# template(envelope(), "foo", bar = 9)

envelope() %>%
  template("branded-letter", logo_path = "/home/wookie/Downloads/fathom-logo.png") %>%
  as.character(details = TRUE)
