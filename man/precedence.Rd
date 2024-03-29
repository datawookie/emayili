% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/header-precedence.R
\name{precedence}
\alias{precedence}
\alias{priority}
\alias{importance}
\title{Add fields for message importance and priority}
\usage{
priority(msg, priority = NULL)

importance(msg, importance = NULL)
}
\arguments{
\item{msg}{A message object.}

\item{priority}{Priority level. One of \code{"non-urgent"}, \code{"normal"}, or \code{"urgent"}.}

\item{importance}{Importance level. One of \code{"low"}, \code{"normal"}, or \code{"high"}.}
}
\value{
A message object.
}
\description{
Functions to influence message delivery speed and importance.
}
\details{
The \code{priority()} function adds the \code{Priority} header field which gives a hint
to influence transmission speed and delivery. Valid values are
\code{"non-urgent"}, \code{"normal"}, and \code{"urgent"}. The non-standard \code{X-Priority}
header field is similar, for which valid values are \code{1} (Highest), \code{2}
(High), \code{3} (Normal, the default), \code{4} (Low), and \code{5} (Lowest).

The \code{importance()} function adds the \code{Importance} header field, which gives a
hint to the message recipient about how important the message is. Does not
influence delivery speed.
}
\examples{
# How rapidly does the message need to be delivered?
#
envelope() \%>\%
  subject("Deliver this immediately!") \%>\%
  priority("urgent")

envelope(priority = "non-urgent") \%>\%
  subject("No rush with this.")

# How much attention should be paid by recipient?
#
envelope() \%>\%
  subject("Read this immediately!") \%>\%
  importance("high")

envelope(importance = "low") \%>\%
  subject("Not important at all. Just delete.")
}
