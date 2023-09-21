
#' Extract the values of the model response from an object
#' 
#' @description 
#' Extract the values of the model response from an object of class
#' \dQuote{\link{model}}, \dQuote{\link{multimodel}}, \dQuote{\link{cv}} or a fitted model.
#' 
#' @param object An object of class \dQuote{model}, \dQuote{multimodel} or \dQuote{cv} or a fitted model.
#' @param complete Logical: If `FALSE`, the output includes only the observations featuring exactly once in the 
#' `folds` (sorted by their position in the model data); otherwise, it returns the full vector.
#' @param \dots Currently not used.
#' 
#' @details
#' The default method applies `response.model()` to `model(object, ...)`.
#' 
#' The method for class \dQuote{cv} with `complete=FALSE` returns a vector of the same length as 
#' \code{\link{cv_predict.cv}(x)} and \code{\link{cv_resid.cv}(x)}.
#' 
#' @return A vector containing the values of the model^s response variable.
#' 
#' @seealso \code{\link{cv_predict}} (in particular, the examples), \code{\link{predict.model}}.
#' 
#' @examples 
#' response(lm(mpg ~., mtcars))
#' 
#' @name response
#' @export
response <- function(object, ...) UseMethod("response")

#' @rdname response
#' @importFrom xgboost getinfo
#' @export
response.model <- function(object, ...){
  if (object$response %in% names(object$data)){
    out <- object$data[[object$response]]
  } else if (inherits(object$data, "xgb.DMatrix")){ # special case xgb.DMatrix data
    out <- getinfo(object$data, "label")
  } else {
    parsed <- parse(text = object$response)
    out <- eval(parsed, c(object$data, object$saved_objects))
  }
  as.numeric(out)
}

#' @rdname response
#' @export
response.default <- function(object, ...){
  response(model(object, ..., env = parent.frame()))
}

#' @rdname response
#' @export
response.multimodel <- function(object, ...){
  response(object$models[[1]])
}

# ------------------------------------------------------------------------------

#' @rdname response
#' @export
response.cv <- function(object, complete = FALSE, ...){
  out <- response(object$multimodel)
  if (!complete) out <- out[oos_indices(object$folds)[, 1]]
  out
}

