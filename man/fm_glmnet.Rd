% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fm_glmnet.R
\name{fm_glmnet}
\alias{fm_glmnet}
\alias{predict.fm_glmnet}
\alias{coef.fm_glmnet}
\alias{plot.fm_glmnet}
\title{\code{formula}-based wrapper for \code{glmnet()}}
\usage{
fm_glmnet(
  formula,
  data,
  weights = NULL,
  family = c("gaussian", "binomial", "poisson"),
  pref_iter = NULL,
  na.action = na.omit,
  keep_x = TRUE,
  ...
)

\method{predict}{fm_glmnet}(
  object,
  newdata,
  pref_iter = object$pref_iter,
  s = tail(object$fit$lambda, 1),
  ...
)

\method{coef}{fm_glmnet}(object, s = tail(object$fit$lambda, 1), ...)

\method{plot}{fm_glmnet}(
  x,
  coefs = NULL,
  intercept = FALSE,
  plot_type = c("colors", "facets"),
  size = 0.6,
  lwd = 0.5,
  ...,
  plot = TRUE,
  zeroline = TRUE
)
}
\arguments{
\item{formula}{A \code{formula}.}

\item{data}{A \code{data.frame}}

\item{weights}{weights}

\item{family, \dots}{Passed to \code{\link[glmnet]{glmnet}()}. Not all will work properly:
avoid argument settings that change the structure of the output of \code{glmnet()}!
In \code{plot.fm_glmnet()},  \dQuote{...} are passed to both \code{geom_point()} and \code{geom_line()}.}

\item{pref_iter}{An integer, the \emph{preferred iteration}. This is the iteration that is used by default
when predictions from the model are computed with \code{predict()}.
If \code{pref_iter=NULL}, the \emph{last} iteration will be used.
See \code{\link{ifm}} and \code{vignette("ifm")} for information on the concepts of
\emph{iteratively fitted models} and preferred iterations.
The preferred iteration of a model can be changed without re-fitting the model,
see \code{\link{set_pref_iter}()}.}

\item{na.action}{A function which indicates what should happen when the data contain \code{NA}s.
\code{\link{na.omit}} is the default, \code{\link{na.exclude}} or \code{\link{na.fail}}
could be useful alternative settings.}

\item{keep_x}{Logical: Whether to keep the model matrix \code{x} as a component of the return value.}

\item{object, x}{Object of class \dQuote{fm_glmnet}.}

\item{newdata}{Data for prediction.}

\item{s}{Choice of lambda.}

\item{coefs}{Character vector: An optional subset of \code{x} variables' names to be included in plot.
By default, all are included.}

\item{intercept}{Logical: Whether to include the intercept's profile in the plot.}

\item{plot_type}{A character string, either \code{colors} (profiles of all coefficients are shown in same facet
distinguished by colors) or \code{facet} (profiles of different coefficients appear in separate facets).}

\item{size}{Graphic detail: Size of point.}

\item{lwd}{Graphic detail: Line width of interpolating line.}

\item{plot}{Logical: If \code{TRUE}, a ggplot is returned, if \code{FALSE} a \code{data.frame}.
\code{plot()} first prepares a \code{data.frame} and then draws some ggplot using this data,
with limited options for customization.
If you want to design your own plot, you can set \code{plot=FALSE}, and use the \code{data.frame} returned by \code{plot()}
to create your plot.}

\item{zeroline}{Logical: Whether to include a horizontal reference line at level 0.}
}
\value{
\code{fm_glmnet()} returns a list of class \dQuote{fm_glmnet} with components
\itemize{
\item{\emph{fit}: the fitted model, of class \dQuote{glmnet};}
\item{\emph{formula}: the formula;}
\item{\emph{x}: the model matrix (resulting from the \code{formula} using \code{model.matrix()});}
\item{\emph{weights}: the fitting weights;}
\item{\emph{xlevels}: list of the levels of the factors included in the model;}
\item{\emph{pref_iter}: the preferred iteration, an integer (see argument \code{pref_iter});}
\item{\emph{na.action}: the \code{\link{na.action}} used during data preparation;}
\item{\emph{contrasts}: the \code{\link{contrasts}} used during data preparation;}
\item{\emph{call}: the matched call generating the model.}
}
}
\description{
\code{fm_glmnet()} is a wrapper for \code{\link{glmnet}()} (from package \CRANpkg{glmnet})
that fits into the \code{modeltuner} framework.
The model is specified by the arguments \code{formula} and \code{data}.
The resulting models belong to the class of so-called \emph{iteratively fitted models},
see \code{\link{ifm}} and \code{vignette("ifm")} for information.
}
\details{
\code{family} must be one of \code{"gaussian"}, \code{"binomial"}, \code{"poisson"}.

The parameters \code{x} and \code{y} to be passed to \code{glmnet()} are extracted from
\code{formula} and \code{data} by means of \code{\link{model.frame}}, \code{\link{model.matrix}}
and \code{\link{model.response}}.

\emph{Features of cross-validation of models generated with} \code{fm_glmnet()}:
\itemize{
\item{The model class \dQuote{fm_glmnet} belongs to the class of so-called
\emph{iteratively fitted models}; see \link{ifm} and \code{vignette("ifm")} for information on
the peculiarities of cross-validating such models. In particular,
note the role of the parameter \code{iter} in \code{\link{cv}()}.}
\item{When \code{cv()} is executed with \code{keep_fits=TRUE}, the fitted models from
cross-validation that are stored in the result (and returned by \code{\link{extract_fits}()})
will not be of class \dQuote{fm_glmnet}, but of class \dQuote{glmnet},}
}
}
\section{Methods (by generic)}{
\itemize{
\item \code{predict(fm_glmnet)}: \code{predict()} method

\item \code{coef(fm_glmnet)}: \code{coef()} method

\item \code{plot(fm_glmnet)}: \code{plot()} method. Produces a coefficient profile plot.

}}
\examples{
d <- simuldat()  
(mod1 <- fm_glmnet(Y ~., d))
(mod2 <- fm_glmnet(Y ~.^2, d))

# Plot mod1:
plot(mod1)
# Plot profiles for a subset of the coefficients' only and the intercept
plot(mod1, coefs = paste0("X", 1:10), intercept = TRUE)

# Cross-validate
mycv <- cv(c(model(mod1, label = "mod1"), 
             model(mod2, label = "mod2")), 
           nfold = 5)
mycv

# Plot cv_performance and evaluation_log:
plot(cv_performance(mycv))
plot(evaluation_log(mycv))

}
\seealso{
\code{\link[glmnet]{glmnet}}, \code{\link[glmnet]{cv.glmnet}}  (both from package \CRANpkg{glmnet});
\link{ifm} and \code{vignette("ifm")}; \code{\link{fit.model_fm_glmnet}}; \code{\link{set_pref_iter}}
}
