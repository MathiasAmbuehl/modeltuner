
#' Extract the models fitted in a cross-validation from a \dQuote{cv} object.
#' 
#' Extraction of the model fits is only possible if these have been saved as a part of the \dQuote{cv} object, 
#' which is not the case by default. See the argument `keep_fits` in \code{?\link{cv}}.
#' 
#' @param x A `cv`-object.
#' @param \dots Currently not used.
#' 
#' @return
#' A \link{param_table}. 
#' Besides the model fits, it will also have a column containing the `folds`.
#' 
#' @seealso \code{\link{cv}}
#' 
#' @examples
#' mm <- c(lm =    model(lm(Sepal.Width ~ ., iris)), 
#'         if (require(rpart)) model(rpart(Sepal.Width ~ ., iris), label = "rpart"))
#' mycv <- cv(mm, keep_fits = TRUE)
#' extract_fits(mycv)
#' 
#' @export
extract_fits <- function(x, ...) UseMethod("extract_fits")

#' @export
extract_fits.cv <- function(x, ...){
  fits_ok <- has_cv_fits(x)
  if (!any(fits_ok)) return(NULL)
  fits <- x$fits[fits_ok]
  lbls <- label(x)[fits_ok]
  out <- data.frame(row.names = seq_along(x$folds))
  out$folds <- x$folds
  for (i in seq_along(lbls)){
    out[[lbls[[i]]]] <- fits[[i]]
  }
  param_table(out)
}
