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
#' @param .name A template name.
#' @param ... Variables for substitution.
#' @param .envir Environment for substitution.
#'
#' @return
#' @export
#'
#' @examples
template <- function (msg, .name, ..., .envir = parent.frame()) {
  if(!requireNamespace("jinjar", quietly = TRUE)) {
    stop("Install {jinjar} to to use templates.")    # nocov
  }

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
    if (dir.exists(path)) {
      PATH <- path
      log_debug("Found template in {PATH}.")
      break
    }
  }
  if (is.null(PATH)) stop(glue("Unable to find '{.name}' template."))

  path_html <- file.path(path, "template.html")
  path_text <- file.path(path, "template.txt")

  if (!file.exists(path_html)) stop("Unable to find HTML template.")
  if (!file.exists(path_text)) stop("Unable to find text template.")

  template_html <- emayili:::read_text(path_html)
  template_text <- emayili:::read_text(path_text)

  template_html <- jinjar::render(template_html, !!!params)
  template_text <- jinjar::render(template_text, !!!params)

  msg <- msg %>% html(template_html)

  if (get_option_invisible()) invisible(msg) else msg # nocov
}
