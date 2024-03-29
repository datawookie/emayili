% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/template.R
\name{template}
\alias{template}
\title{Add message body from template}
\usage{
template(msg, .name, ..., .envir = parent.frame())
}
\arguments{
\item{msg}{A message object.}

\item{.name}{A template name. This can be provided as either: (i) the name
of a template that's baked into the package, (ii) a relative path or (iii)
an absolute path. The paths must be for the directory containing the
template files, not the files themselves.}

\item{...}{Variables for substitution.}

\item{.envir}{Environment for substitution.}
}
\value{
A message object.
}
\description{
Variables given as named arguments will override any variables in the
environment with the same name.
}
\details{
Will probably not get variables from environment if used as part of a
pipeline. In this case might need to use the \code{\%|>\%} (nested pipe)
operator.
}
\examples{
# Use a builtin template.
envelope() \%>\%
  template(
    "newsletter",
    title = "A Sample Newsletter",
    articles = list(
      list(
        "title" = "Article (with date)",
        "content" = as.list("Vivamus, justo quisque, sed."),
        "date" = "1 January 2022"
      ),
      list(
        "title" = "Another Article (without date)",
        "content" = as.list("Quam lorem sed metus egestas.")
      )
    )
  )
# Use a custom local template.
\dontrun{
envelope() \%>\%
  template("./templates/custom-template")
}
}
