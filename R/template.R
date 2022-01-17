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
#' # Use a builtin template.
#' envelope() %>%
#'   template(
#'     "bootstrap-newsletter",
#'     title = "A Sample Newsletter",
#'     articles = list(
#'       list(
#'         "title" = "Article (with date)",
#'         "content" = as.list(stri_rand_lipsum(7)),
#'         "date" = "1 January 2022"
#'       ),
#'       list(
#'         "title" = "Another Article (without date)",
#'         "content" = as.list(stri_rand_lipsum(5)),
#'         "date" = "3 January 2022"
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

  if (file.exists(path_html)) {
    template_html <- emayili:::read_text(path_html)
    log_debug("Found HTML template. Populating...")
    template_html <- jinjar::render(template_html, !!!params)
    log_debug("Done.")
  } else {
    template_html <- NULL
    log_debug("Unable to find HTML template.")
  }
  if (file.exists(path_text)) {
    template_text <- emayili:::read_text(path_text)
    log_debug("Found text template. Populating...")
    template_text <- jinjar::render(template_text, !!!params)
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
    content <- append.MIME(content, text_html(template_html))
    content <- append.MIME(content, text_plain(template_text))

    msg <- append(msg, content)
  } else {
    if (!is.null(template_html)) {
      msg <- append(msg, text_html(template_html))
    }
    if (!is.null(template_text)) {
      msg <- append(msg, text_plain(template_html))
    }
  }

  if (get_option_invisible()) invisible(msg) else msg # nocov
}
