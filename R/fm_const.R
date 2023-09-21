
#' Fitting a constant model
#' 
#' @description
#' This is a model fitting function with the usual arguments `formula`, 
#' `data` and optionally `weights`, defining a \dQuote{constant model}.
#' Model predictions will be equal to the result of applying `fun()` to the 
#' response (left hand side variable of `formula`) for all observations.
#' Fitting such a model thus amounts to evaluating `fun(y)`, where `y` denotes 
#' the model response.
#' 
#' @param formula A formula. Only the left hand side relevant. 
#' @param data A `data.frame`.
#' @param weights Optional numeric vector, The fitting weights.
#' @param fun A function defining a location estimate.
#' Takes a numeric vector as its first argument, optionally an argument `weights`, 
#' and returns a numeric value.
#' @param na.action A function which indicates what should happen when the 
#' response variable contains `NA`s.
#' @param \dots Arguments passed to `fun()`.
#' @param x,object Object of class \dQuote{fm_const}. 
#' @param newdata Data for prediction.
#' 
#' @return
#' `fm_const()` returns a list of class \dQuote{fm_glmnet} with components
#' \itemize{
#' \item{*estimate*: the estimated value;}
#' \item{*formula*: the formula;}
#' \item{*n*: the number of rows in `data`;} 
#' \item{*rownames*: the rownames of the `data`;}
#' \item{*fun*: the fitting function `fun()`;} 
#' \item{*weights*: the fitting weights;}
#' \item{*na.action*: the \code{\link{na.action}} used during data preparation;}
#' \item{*call*: the matched call generating the model.}
#' } 
#' 
#' @examples
#' fm <- fm_const(Sepal.Width ~ 1, iris)
#' fm
#' unique(predict(fm)) # all equal values
#' 
#' # Weighted constant model
#' wmean <- function(x, weights, ...) weighted.mean(x, w = weights, ...)
#' w <- runif(150)
#' fm_const(Sepal.Length ~ 1, iris, weights = w, fun = wmean)
#' 
#' @export
fm_const <- function(formula, data, fun = mean, 
                     weights = NULL, na.action = na.omit, ...){
  stopifnot(length(formula) == 3, is.function(fun))
  formula <- update(formula, . ~ 1)
  data <- na.action(model.frame(formula, data = data))
  y <- eval(formula[[2]], data)
  if ("weights" %in% names(formals(fun))){
    estimate <- fun(y, weights, ...)
  } else {
    if (!missing(weights))
      stop("weights were specified, but fun() does not have a formal argument ", 
           sQuote("weights"))
    estimate <- fun(y, ...)
  }
  n <- nrow(data)
  structure(
    list(estimate = estimate,
         formula = formula,
         n = n,
         rownames = if (missing(data)) NULL else rownames(data), 
         fun = fun,
         weights = weights,
         na.action = na.action, 
         call = match.call()),
    class = "fm_const")
}

#' @inheritParams print.model
#' @describeIn fm_const `print()` method
#' @export
print.fm_const <- function(x, abbreviate = TRUE, ...){
   cat("fm_const-model (class ", sQuote("fm_knn"), 
      ")\n", sep = "")
  prnt <- list(formula = x$formula, data = deparse(x$call$data)[[1]], 
               estimate = format(x$estimate, digits = getOption("digits")),
               fun = capture.output(str(x$fun, give.attr = F)), 
               weights = prnt_compact(x$weights))
  render_list(prnt, abbreviate = abbreviate)
  invisible(x)
}

#' @describeIn fm_const `predict()` method
#' @export
predict.fm_const <- function (object, newdata, ...){
  n <- if (missing(newdata)) object$n else nrow(newdata)
  prediction <- rep(object$estimate, n)
  if (missing(newdata)){
    names(prediction) <- object$rownames
  } else {
    names(prediction) <- rownames(newdata)
  }
  prediction
}

