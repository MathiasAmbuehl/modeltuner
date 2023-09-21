
#' Predictions and residuals from a (multi-)model
#' 
#' `predict.model()` and `residuals.model()` \code{\link{fit}} a model and 
#' extract the *in-sample* predictions and residuals, respectively.
#' 
#' @param object A \code{\link{model}} or \code{\link{multimodel}}.
#' @param newdata newdata
#' @param \dots Passed to \code{\link{fit}}.
#' 
#' @return A vector if `object` is a \dQuote{model}, a matrix if it is a \dQuote{multimodel} or \dQuote{cv} containing several models.
#' 
#' @seealso 
#' \code{\link{fit}}, \code{\link{cv_predict}} (in particular examples), \code{\link{response}}.
#'
#' @examples
#' mod <- model(lm(Sepal.Length ~ .,  iris), 
#'              label = "lm_iris")
#' predict(mod)
#' residuals(mod)
#' 
#' @name predict.model
NULL

#' @rdname predict.model
#' 
#' @importFrom stats predict
#' @export
predict.model <- function(object, newdata = object$data, ...){
  pr <- object$predict_function(fit(object), newdata, ...)
  pr <- unlist(as.vector(pr))
  names(pr) <- rownames(newdata)
  pr
}

#' @rdname predict.model
#' @importFrom stats residuals
#' @export
residuals.model <- function(object, ...){
  response(object) - predict(object, ...)
}

# ------------------------------------------------------------------------------

#' @rdname predict.model
#' @export
predict.multimodel <- function(object, newdata, ...){
  nmodel <- n_model(object)
  if (missing(newdata)){
    n <- length(response(object))
    out <- vapply(seq_len(nmodel), function(i) predict(extract_model(object, i), ...), 
                  numeric(n))
    rownames(out) <- rownames(object$models[[1]]$data)
  } else {
    n <- nrow(newdata)
    out <- vapply(seq_len(nmodel), function(i) predict(extract_model(object, i), newdata, ...), 
                  numeric(n))
    rownames(out) <- rownames(newdata)
  }
  colnames(out) <- label(object)
  out
}

#' @rdname predict.model
#' @export
residuals.multimodel <- function(object, ...){
  nmodel <- n_model(object)
  n <- nrow(object$data)
  out <- vapply(seq_len(nmodel), function(i) residuals(extract_model(object, i)), 
                numeric(n))
  colnames(out) <- label(object)
  rownames(out) <- rownames(object$data)
  out
}
