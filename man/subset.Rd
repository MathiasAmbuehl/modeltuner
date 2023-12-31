% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset.R
\name{subset}
\alias{subset}
\alias{subset.multimodel}
\alias{subset.cv}
\alias{subset.performance}
\alias{subset.evaluation_log}
\title{Subset an object of class \dQuote{multimodel}, \dQuote{cv}, \dQuote{performance} or \dQuote{evaluation_log}}
\usage{
\method{subset}{multimodel}(x, subset = TRUE, ...)

\method{subset}{cv}(x, subset = TRUE, ...)

\method{subset}{performance}(x, subset = TRUE, ...)

\method{subset}{evaluation_log}(x, subset = TRUE, ...)
}
\arguments{
\item{x}{\code{multimodel} or \code{cv} or other object.}

\item{subset}{Selection of models: An integer vector or a logical vector of appropriate length,
or a character vector of model \code{label}s.}

\item{\dots}{Not used.}
}
\value{
Object of same class as \code{x}.
}
\description{
\code{\link[base]{subset}()} methods for classes \dQuote{\link{multimodel}},
\dQuote{\link{cv}}, \dQuote{performance} and \dQuote{\link{evaluation_log}}.
}
\examples{
mm <- c(speed_lm = model(lm(dist ~ speed, cars)),
        speed_loess = model(loess(dist ~ speed, cars,
                                  control = loess.control(surface = "direct"))),
        speed_rpart = model(rpart::rpart(dist ~ speed, cars)))
# subset.multimodel:
subset(mm, 1:2)
# subset.cv:
cv_mm <- cv(mm)
subset(cv_mm, 3:2)
# subset.performance:
cv_perf <- cv_performance(cv_mm)
subset(cv_perf, c(1, 3))


}
\seealso{
\code{\link{extract_model}} (in particular examples),
\code{\link{label}}, \code{\link{sort_models}}
}
