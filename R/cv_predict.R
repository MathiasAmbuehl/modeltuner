
#' Extract out-of-sample predictions and residuals from cross-validation
#' 
#' `cv_predict()` extracts out-of-sample predictions from a \dQuote{cv} or related object, 
#' `cv_resid()` extracts out-of-sample residuals. Both are generic.
#' 
#' @param x A \dQuote{cv} object or an object of another class. See \dQuote{Methods}.
#' @param simplify Logical: If `FALSE`, a list of vectors will be returned in case of multiple models, 
#' if `TRUE` the output will be converted to a matrix. 
#' @param \dots Not used in \code{*.cv} methods, passed to \code{cv} in all other methods.
#' 
#' @details
#' The output will depend on the `folds` used in \code{\link{cv}()}.
#' If the folds do not include all observations in the model data,
#' the output vector will be shorter than the number of rows in the model data.
#' 
#' @return
#' A vector, or if there is more than one model, 
#' a matrix (if `simplify=TRUE`) or list of vectors (`simplify=FALSE`). 

#' 
#' @section Methods:
#' \itemize{
#' \item{`cv_predict.cv(x, ...)` is the core method described above.}
#' \item{`cv_predict.model(x, ...)` and `cv_predict.multimodel` execute
#' `x %>% cv(...) %>% cv_predict`.}
#' \item{`cv_predict.default(x, ...)` is applied to a fitted model; it executes `x %>% model %>% cv(...) %>% cv_predict`.}
#' }
#' `cv_resid` has the same methods as `cv_predict`.
#' 
#' @seealso
#' \code{\link{response}}, \code{\link{predict.model}}
#' 
#' @examples
#' # Simulate data 
#' set.seed(123)
#' n <- 30
#' p <- 20
#' x <- matrix(rnorm(p*n), nrow = n)
#' y <- rowSums(x) + rnorm(n)
#' mymodel <- model(lm(y~x))
#' 
#' # Compare correlation of in-sample and out-of-sample predictions
#' # with response
#' preds <- data.frame(
#'   training = predict(mymodel), 
#'   test = drop(cv_predict(mymodel)))
#' cor(response(mymodel), preds)
#' 
#' @name cv_predict
NULL

#' @rdname cv_predict
#' @export
cv_predict <- function(x, ...){
  UseMethod("cv_predict")
}

#' @export
cv_predict.cv_simple <- function(x, ...){
  predictions <- cv_pred_matrix(x)
  predictions[oos_indices(x$folds)]
}

#' @rdname cv_predict
#' @export
cv_predict.cv <- function(x, simplify = TRUE, ...){
  nmodel <- n_model(x)
  nms <- rownames(extract_model(x, 1)$data)
  out <- vector("list", nmodel)
  for (i in seq_len(nmodel)){
    out[[i]] <- cv_predict(extract(x, i), ...)
    names(out[[i]]) <- nms
  }
  names(out) <- label(x)
  if (simplify) out <- simplify2array(out)
  out
}

#' @rdname cv_predict
#' @export
cv_predict.model <- function(x, simplify = TRUE, ...){
  cvobj <- cv(x, ...)
  cv_predict(cvobj, simplify = simplify)
}

#' @rdname cv_predict
#' @export
cv_predict.multimodel <- function(x, simplify = TRUE, ...){
  cvobj <- cv(x, ...)
  cv_predict(cvobj, simplify = simplify)
}

#' @rdname cv_predict
#' @export
cv_predict.default <- function(x, simplify = TRUE, ...){
  modelobj <- model(x, env = parent.frame())
  cvobj <- cv(modelobj, ...)
  cv_predict(cvobj, simplify = simplify)
}

#' @details
#' \code{oos_indices()} identifies the out-of-sample indices in a `predictions` matrix.
#' 
#' @rdname modeltuner-internal
#' @export
oos_indices <- function(folds){
  out <- cbind(unlist(folds, use.names = FALSE), 
               rep(seq_along(folds), lengths(folds)))
  if (length(dups <- which(tabulate(out[, 1], nbins = attr(folds, "n")) > 1))){
    out <- out[!(out[, 1] %in% dups), , drop = FALSE] 
  }
  out <- out[order(out[, 1]), , drop = FALSE]
  out
}

# Aux
cv_pred_matrix <- function(x){
  stopifnot(inherits(x, "cv_simple"))
  if (is.matrix(x$predictions))
    return(x$predictions)
  if (is.list(x$predictions))
    return(x$predictions[[1]])
}



# ------------------------------------------------------------------------------

#' @rdname cv_predict
#' @export
cv_resid <- function(x, ...){
  UseMethod("cv_resid")
}

#' @rdname cv_predict
#' @export
cv_resid.cv <- function(x, simplify = TRUE, ...){
  out <- cv_predict.cv(x, simplify = simplify, ...)
  response <- response(x$multimodel)
  out[] <- relist(unlist(out) - response, out)
  out
}

#' @rdname cv_predict
#' @export
cv_resid.model <- function(x, ...){
  cvobj <- cv(x, ...)
  cv_resid(cvobj)
}

#' @rdname cv_predict
#' @export
cv_resid.multimodel <- function(x, simplify = TRUE, ...){
  cvobj <- cv(x, ...)
  cv_resid(cvobj, simplify = simplify)
}

#' @rdname cv_predict
#' @export
cv_resid.default <- function(x, simplify = TRUE, ...){
  modelobj <- model(x, env = parent.frame())
  cvobj <- cv(modelobj, ...)
  cv_resid(cvobj, simplify = simplify)
}

