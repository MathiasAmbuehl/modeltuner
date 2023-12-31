% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv.R
\name{cv}
\alias{cv}
\alias{cv.model}
\alias{cv.multimodel}
\alias{cv.default}
\alias{print.cv}
\title{Run a cross-validation}
\usage{
cv(x, ...)

\method{cv}{model}(
  x,
  nfold = getOption("cv_nfold"),
  folds = NULL,
  ...,
  metric = NULL,
  iter = getOption("cv_iter"),
  param = TRUE,
  keep_fits = FALSE,
  verbose = getOption("cv_verbose")
)

\method{cv}{multimodel}(
  x,
  nfold = getOption("cv_nfold"),
  folds = NULL,
  metric = NULL,
  iter = getOption("cv_iter"),
  param = TRUE,
  keep_fits = FALSE,
  verbose = getOption("cv_verbose"),
  ...
)

\method{cv}{default}(
  x,
  nfold = getOption("cv_nfold"),
  folds = NULL,
  ...,
  metric = NULL,
  iter = getOption("cv_iter"),
  param = TRUE,
  keep_fits = FALSE
)

\method{print}{cv}(
  x,
  what = c("class", "formula", "weights"),
  show_metric = TRUE,
  abbreviate = TRUE,
  n = getOption("print_max_model"),
  width = getOption("width"),
  param = TRUE,
  ...
)
}
\arguments{
\item{x}{A \code{\link{model}}, \code{\link{multimodel}} or fitted model
(see sections \dQuote{Methods}).}

\item{\dots}{These arguments are passed internally to methods of \code{cv_simple()},
a currently undocumented generic that runs the cross-validation on a single model.}

\item{nfold, folds}{Passed to \code{\link{make_folds}}.}

\item{metric}{A metric (see \code{\link{metrics}}).
\code{metric=NULL} selects the default metric, see \code{\link{default_metric}}.}

\item{iter}{A preference criterion, or a list of several criteria.
Only relevant for \emph{iteratively fitted models} (see \link{ifm}), ignored otherwise.}

\item{param}{Logical. Include parameter table in output? See \code{?\link{multimodel}}.}

\item{keep_fits}{Logical: Keep the cross-validation's model fits?}

\item{verbose}{Logical: Output information on execution progress in console?}

\item{what}{Which elements of the multimodel should be printed? See \code{\link{print.model}}.}

\item{show_metric}{Logical: Whether to print the cross-validated models' metric.}

\item{abbreviate}{Logical. If \code{TRUE} (the default), long formulas and calls are printed in abbreviated mode,
such that they usually fit on 4 or fewer output lines; otherwise they are printed entirely, no matter how long they are.}

\item{n}{Integer: Model details are printed for first \code{n} models in \code{print.cv()}.}

\item{width}{Integer: Width of printed output.}
}
\value{
The output from \code{cv()} is a list of class \dQuote{cv} having the following elements:
\itemize{
\item{\emph{multimodel}: a \code{multimodel};}
\item{\emph{folds}: the folds, as defined in \code{nfold} or \code{folds} (see \code{\link{make_folds}});}
\item{\emph{fits}: if \code{keep_fits=FALSE} (the default): \code{NULL}; if \code{keep_fits=TRUE}: the list of the model fits
resulting from the cross-validation, see \code{\link{extract_fits}});}
\item{\emph{metric}: a list: the default evaluation metrics, not necessarily the same for all models;}
\item{\emph{predictions}: a list of matrices of dimension \eqn{n \times k} where
\eqn{n} is the number of observations in the model data and \eqn{k} is the number of folds;
each of these list entries corresponds to a model;}
\item{\emph{performance}: a list of performance tables (see \code{\link{cv_performance}}),
that are saved only for certain model classes; often \code{NULL};}
\item{\emph{timing}: execution time of cross-validation;}
\item{\emph{extras}: a list of extra results from cross-validation,
which are saved only for certain model classes; often \code{NULL}.
If the model \code{x} is an iteratively fitted model (\link{ifm}), \code{extras} contain the
cross-validated model's evaluation log and information on preferred iterations.}
}
}
\description{
\code{cv()} executes a cross-validation procedure.
For each fold (specified in argument \code{nfold }or \code{folds}), the original model is re-fitted using the complement
of the fold as training data.
Cross-validations of multiple models are executed using identical \code{folds}.
}
\details{
The same cross-validations groups (folds) are used for all models.

Each model in \code{x} is processed separately with the function \code{\link{cv_simple}()},
a generic function for internal use.
Besides the standard method \code{cv_simple.model()}, there are currently specific methods of \code{cv_simple()}
for models generated with \code{\link{fm_xgb}()} and \code{\link{fm_glmnet}()}.
}
\section{Methods}{

\itemize{
\item{\code{cv.multimodel()}, the core method.}
\item{\code{cv.model(x, ...)} corresponds to \code{x \%>\% multimodel \%>\% cv(...)}.}
\item{The default method essentially executes \code{x \%>\% model \%>\% cv(...)} and
thus expects a fitted model as its \code{x}.}
}
}

\examples{
mm <- multimodel(model(fm_knn(Sepal.Length ~ ., iris)), k = 1:5)
cv(mm)

mm_cars <- c(simpleLinear = model(lm(mpg ~ cyl, mtcars)),
             linear = model(lm(mpg ~ ., mtcars)),
             if (require(ranger)) model(ranger(mpg ~ ., mtcars), label = "forest"))
mm_cars
cv_cars <- cv(mm_cars, nfold = 5)
cv_cars
cv_performance(cv_cars)

# Non-default metric:
cv_performance(cv_cars, metric = "medae")

}
\seealso{
\code{\link{make_folds}}, \code{\link{multimodel}},
\code{\link{cv_performance}}, \code{\link{cv_predict}}, \code{\link{c.cv}}, \code{\link{extract_fits}}
}
