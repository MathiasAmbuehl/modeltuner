% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/last_cv.R
\name{last_cv}
\alias{last_cv}
\alias{set_last_cv}
\title{Get and set the last cv object}
\usage{
last_cv()

set_last_cv(value)
}
\arguments{
\item{value}{Any object of class \dQuote{cv}.}
}
\value{
\code{last_cv()} returns a \code{\link{cv}} object,
}
\description{
\code{last_cv()} returns the last generated \dQuote{cv} object.
This allows recovering a cross-validation if you have (for example) used the
method \code{\link{cv_performance}.multimodel()} directly, in which case
the \dQuote{cv} is not saved.

\code{set_last_cv(value)} causes that \code{value} will be returned by \code{last_cv()}.
}
\examples{
data(mcycle, package = "MASS")
m <- lm(accel ~ times, mcycle)
cv_performance(m)  # m \%>\% cv \%>\% cv_performance
last_cv()      # recovers unsaved output from cv() in previous call

}
\seealso{
\code{\link{cv}}
}
