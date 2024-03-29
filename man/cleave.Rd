% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/address.R
\name{cleave}
\alias{cleave}
\title{Split a compound address object}
\usage{
cleave(addr)
}
\arguments{
\item{addr}{An \code{address} object.}
}
\value{
A list of \code{address} objects, each of which contains only a single
address.
}
\description{
Split a compound address object
}
\examples{
cleave(as.address(c("foo@yahoo.com", "bar@yahoo.com")))
}
