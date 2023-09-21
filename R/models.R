
#' Combine several fitted models in a multimodel
#'
#' The function `models()` is a more general version of \code{\link{c.model}()}.
#' While the latter expects arguments inheriting of classes \dQuote{model} or \dQuote{multimodel}
#' in its \dQuote{...} arguments, the former also accepts *fitted* models (e.g. an \dQuote{lm}).
#'
#' @param \dots Passed to \code{\link{c.model}()}, after application of 
#' \code{\link{model}()} to *fitted* models in the list of \dQuote{...} arguments.
#' @param .env For internal use.  
#' 
#' @return A \link{multimodel}.
#' 
#' @examples
#' mm <- models(
#'   lm = lm(Sepal.Length ~ ., iris),
#'   rpart = model(rpart::rpart(Sepal.Length ~ ., iris)),
#'   xgboost = tune(fm_xgb(Sepal.Length ~ ., iris))
#' )
#' mm
#' cv_performance(mm)
#'   
#' @export
models <- function(..., .env = parent.frame()){
  dots <- list(...)
  is_fitted <- !vapply(dots, inherits, what = c("model", "multimodel"), 
                       FUN.VALUE = logical(1))
  dots[is_fitted] <- lapply(dots[is_fitted], model, env = .env)
  do.call(c, dots)
}
