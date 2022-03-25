`%|>%` <- magrittr::pipe_nested

#' Add message body from template
#'
#' Variables given as named arguments will override any variables in the
#' environment with the same name.
#'
#' Will probably not get variables from environment if used as part of a
#' pipeline. In this case might need to use the \code{%|>%} (nested pipe)
#' operator.
#'
#' @param msg A message object.
#' @param .name A template name. This can be provided as either: (i) the name
#'   of a template that's baked into the package, (ii) a relative path or (iii)
#'   an absolute path. The paths must be for the directory containing the
#'   template files, not the files themselves.
#' @param ... Variables for substitution.
#' @param .envir Environment for substitution.
#'
#' @return A message object.
#' @export
#'
#' @examples
#' # Use a builtin template.
#' envelope() %>%
#'   template(
#'     "newsletter",
#'     title = "A Sample Newsletter",
#'     articles = list(
#'       list(
#'         "title" = "Article (with date)",
#'         "content" = as.list("Vivamus, justo quisque, sed."),
#'         "date" = "1 January 2022"
#'       ),
#'       list(
#'         "title" = "Another Article (without date)",
#'         "content" = as.list("Quam lorem sed metus egestas.")
#'       )
#'     )
#'   )
#' # Use a custom local template.
#' \dontrun{
#' envelope() %>%
#'   template("./templates/custom-template")
#' }
template <- function (msg, .name, ..., .envir = parent.frame()) {
  if(!requireNamespace("jinjar", quietly = TRUE)) {
    stop("Install {jinjar} to to use templates.")    # nocov
  }

  JINJAR_CONFIG <- jinjar::jinjar_config(
    lstrip_blocks = FALSE
  )

  # Convert environment to list.
  params <- as.list(.envir)
  # Override values from environment.
  params <- modifyList(params, list(...))

  PATH <- NULL
  for (path in c(
    # Look in relative or absolute path.
    .name,
    # Look in system folder.
    file.path(system.file(package = "emayili"), "template", .name)
  )) {
    log_debug("Looking for template in {path}.")
    if (dir.exists(path)) {
      PATH <- path
      log_debug("Found template in {PATH}.")
      break
    }
  }
  if (is.null(PATH)) stop(glue("Unable to find '{.name}' template. Did you specify a file (rather than a directory)?"))

  path_html <- file.path(path, "template.html")
  path_text <- file.path(path, "template.txt")

  if (file.exists(path_html)) {
    template_html <- read_text(path_html)
    log_debug("Found HTML template. Populating...")
    template_html <- jinjar::render(template_html, !!!params, .config = JINJAR_CONFIG)
    log_debug("Done.")
  } else {
    template_html <- NULL                            # nocov
    log_debug("Unable to find HTML template.")       # nocov
  }
  if (file.exists(path_text)) {
    template_text <- read_text(path_text)
    log_debug("Found text template. Populating...")
    template_text <- jinjar::render(template_text, !!!params, .config = JINJAR_CONFIG)
    log_debug("Done.")
  } else {
    template_text <- NULL
    log_debug("Unable to find text template.")
  }

  if (is.null(template_text) && is.null(template_html)) {
    stop("Could not find either HTML or text template.")
  }

  if (!is.null(template_text) && !is.null(template_html)) {
    content <- multipart_alternative()
    content <- after(content, text_html(template_html))
    content <- after(content, text_plain(template_text))

    msg <- after(msg, content)
  } else {
    if (!is.null(template_html)) {
      msg <- attach_images(msg, template_html, disposition = "inline", charset = "utf-8")
    }
    if (!is.null(template_text)) {
      msg <- after(msg, text_plain(template_text))
    }
  }

  if (get_option_invisible()) invisible(msg) else msg # nocov
}
