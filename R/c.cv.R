
#' Add models to a \code{cv} object
#' 
#' Combines the cross-validation results of different models.
#' The \dQuote{...} arguments can include `cv`s, \code{\link{model}}s or \code{\link{multimodel}}s.
#' `cv()` will be applied to the `(multi)model`s in \dQuote{...}, using the `folds` from `x`.
#' All \dQuote{cv} objects in \dQuote{...} must have the same `folds` as `x`.
#' 
#' @param x An object of class \dQuote{cv}.
#' @param \dots One or several \code{\link{model}}s, \code{\link{multimodel}}s or `cv`s.
#' Any `cv` must have the same `folds` as `x`.
#' @param param Logical: Keep the parameter table? See \code{\link{multimodel}}.
#' 
#' @return
#' An object of class \dQuote{cv} including all models featured in `x` and \dQuote{...}.
#' 
#' @seealso \code{\link{cv}}
#' 
#' @examples
#' mod1 <- model(lm(mpg ~ cyl, mtcars), label = "simpleLinear")
#' mod2 <- model(lm(mpg ~ ., mtcars), label = "linear")
#' # Define common folds
#' mtcars_folds <- make_folds(mtcars, nfold = 5)
#' # Cross validate both models separately
#' cv1 <- cv(mod1, folds = mtcars_folds)
#' cv2 <- cv(mod2, folds = mtcars_folds)
#' # Combine the two
#' cv_cars <- c(cv1, cv2)
#' cv_performance(cv_cars)
#' 
#' # Add a model to a cv object:
#' c(cv_cars, constant = model(lm(mpg ~ 1, mtcars)))
#' 
#' @importFrom utils relist
#' @export
c.cv <- function(x, ..., param = TRUE){
  dots <- list(...)
  lbls0_dots <- lapply(dots, label)
  lbls0 <- c(label(x), unlist(lbls0_dots))
  lbls <- make.unique(lbls0, sep = "")
  # Adjust labels if required
  if (any(renamed <- (lbls0 != lbls))){
    nmodel_x <- n_model(x)
    lbls_dots <- relist(lbls[-seq_len(nmodel_x)], lbls0_dots)
    rnm <- mapply(identical, lbls_dots, lbls0_dots, SIMPLIFY = TRUE)
    for (i in which(!rnm)){
      label(dots[[i]]) <- lbls_dots[[i]]
    }
  }
  is_cv <- sapply(dots, inherits, "cv")
  if (any(is_cv)){
    cv_ok <- duplicated(c(list(unclass(x$folds)), 
                          lapply(dots[is_cv], function(f) unclass(f$folds))))[-1]
    if (all(!cv_ok)) stop("Can not concatenate ", dQuote("cv"),
                          " objects with different folds")
  } 
  if (!all(is_cv)){
    dots[!is_cv] <- lapply(dots[!is_cv], cv, folds = x$folds)
  }
  dots <- c(list(x), dots)
  out <- list(multimodel = 
    do.call(c, c(lapply(dots, extract_multimodel, use_cv_info = FALSE), 
                 list(param = param))))
  out$folds <- dots[[1]]$folds
  fits_list <- vapply(dots, function(x) !is.null(x$fits), FUN.VALUE = logical(1))
  if (any(fits_list)){
    for (i in which(!fits_list))
      dots[[i]]$fits <- vector("list", n_model(dots[[i]]))
    out$fits <- do.call(c, lapply(dots, "[[", "fits"))
  }
  out$metric <- do.call(c, lapply(dots, "[[", "metric"))
  out$performance <- do.call(c, lapply(dots, "[[", "performance"))
  out$predictions <- do.call(c, lapply(dots, "[[", "predictions"))
  out$timing <- do.call(c, lapply(dots, "[[", "timing"))
  out$extras <- do.call(c, lapply(dots, "[[", "extras"))
  class(out) <- c("cv", "list")
  out
}
