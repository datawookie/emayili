% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attachment.R
\name{attachment}
\alias{attachment}
\title{Add attachments to a message object}
\usage{
attachment(
  msg,
  path,
  name = NA,
  type = NA,
  cid = NA,
  disposition = "attachment"
)
}
\arguments{
\item{msg}{A message object.}

\item{path}{Path to file.}

\item{name}{Name to be used for attachment (defaults to base name of \code{path}).}

\item{type}{MIME type or \cite{NA}, which will result in a guess based on file extension.}

\item{cid}{Content-ID or \code{NA}.}

\item{disposition}{How is attachment to be presented (\code{"inline"} or \code{"attachment"})?}
}
\value{
A message object.
}
\description{
Add attachments to a message object
}
\examples{
path_mtcars <- tempfile(fileext = ".csv")
path_scatter <- tempfile(fileext = ".png")
path_cats <- system.file("cats.jpg", package = "emayili")

write.csv(mtcars, path_mtcars)

png(path_scatter)
plot(1:10)
dev.off()

msg <- envelope() \%>\%
  attachment(path_mtcars) \%>\%
  # This attachment will have file name "cats.jpg".
  attachment(path_cats, name = "cats.jpg", type = "image/jpeg") \%>\%
  attachment(path_scatter, cid = "scatter")

file.remove(path_scatter, path_mtcars)
}
