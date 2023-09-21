
#' Re-fit a model object using the complete model data
#' 
#' @description
#' Fits the fitted model (such as `lm`) expressed in a \dQuote{\link{model}} object using the complete model data.
#' In a way, `fit()` is the inverse operation of `model()`.
#'
#' `add_fit()` adds the model fit to a \dQuote{model} object, as its component `fit` (if not already present).
#' The methods for classes \dQuote{multimodel}  and \dQuote{cv} similarly add the fitted models
#' to all included models. 
#'
#' `has_fit()` is a generic function that discerns whether a (multi-)model contains the
#' corresponding fitted model(s).
#'
#' @param x A \dQuote{model} object
#' @param eval Logical: If `FALSE`, the call to the model fitting function is returned, but not executed.
#' @param use_original_args Logical: If `FALSE`, the model generating call is evaluated internally, 
#' using the data (and potential additional objects) saved in `x`; 
#' if `TRUE`, evaluation takes place in the calling environment of `fit()`, using the original input data.
#' @param force Logical: If `FALSE` and a fitted model is present as the `fit` component of the model `x`, 
#' then this fit is returned. If `force=TRUE`, the model will be re-fitted in any case.
#' @param env An environment. Used for internal purposes.
#' @param \dots Passed to the model fitting function.
#' @param iter iteration
#' @inheritParams extract_model
#' 
#' @details
#' `fit()` is generic and has specific methods for classes \dQuote{model_fm_glmnet} and \dQuote{model_fm_xgb}.
#' See \code{\link{fm_glmnet}}, \code{\link{fm_xgb}} and the examples below.
#' 
#' For *iteratively fitted models* (\link{ifm}s, currently classes  \dQuote{fm_xgb} and \dQuote{fm_glmnet}),
#' `fit()` executes \code{\link{set_pref_iter}()} internally.
#' If there is information on the preferred iteration (from a cross-validation) attached to `x`,
#' the call to the model generating function is thereby adjusted. 
#' To suppress this adjustment, run `fit()` with `iter=NULL`.
#' 
#' Another peculiarity of `fit(x)` when `x` is an IFM and has information on preferred iterations 
#' comes into play when, in addition, `x` has a model fit attached to it (i.e. `has_fit(x)==TRUE`).
#' In this case, the fitted model is partially adapted according to the preferred iteration.
#' Specifically, the `pref_iter` is set in the fitted model and in the call 
#' (and more parameters related to stopping or choice of iteration for predictions are modified). 
#' The model fit attached to the output is improper in the sense that 
#' it differs from what would result when you execute its call (but predictions are the same).
#' This default behavior can be suppressed by setting `force=TRUE`, which consequently increases execution time 
#' by the time required to fit the model.
#' 
#' @return
#' `fit()` returns a fitted model. `add_fit()` returns an object of class \dQuote{\link{model}}.
#' `has_fit()` returns a logical vector of length `n_model(x)`.
#' 
#' @section Methods:
#' \itemize{
#' \item{`fit.model()` converts the \dQuote{model} object `x` to a fitted model of class `x$class`.}
#' \item{`fit.multimodel()` and `fit.cv()` extract a single model from `x` according to argument `which`
#' (see \code{\link{extract_model}()}) and applies `fit.model()` to the result.}
#' \item{`fit.model_fm_glmnet()` and `fit.model_fm_xgb()` are specific methods for these two model classes. 
#' A special feature of these methods goes into action if `x` contains results on preferred iteration resulting from a cross-validation. 
#' In that case the preferred iteration is selected from the cross-validation results 
#' (see \code{\link{set_pref_iter}()}).}
#' }
#' 
#' @seealso \code{\link{model}}; \code{\link{set_pref_iter}}; \code{\link{fm_xgb}} and \code{\link{fm_glmnet}}.
#' 
#' @examples
#' # Applying model() then fit() to a model returns the original model:
#' mod <- model(lm(Sepal.Width ~ ., iris), label = "lm")
#' mod
#' fit(mod)
#' 
#' # Obtain the model call without executing it:
#' fit(mod, eval = FALSE)
#' # Note the generic 'data=data' in the result
#' # In order to obtain the original call, do:
#' fit(mod, eval = FALSE, use_original_args = TRUE)
#' 
#' @name fit
NULL


#' @rdname fit
#' @export
fit <- function(x, ...){
  UseMethod("fit")
}

#' @rdname fit
#' @export
fit.model <- function(x, eval = TRUE, use_original_args = FALSE, force = FALSE, 
                      env = parent.frame(), ...){
  if (eval && !force && !use_original_args && inherits(x$fit, x$class)){ 
    return(x$fit)
  }
  cl <- modify_call(x$call, list(...))
  envir <- c(list(data = x$data), x$saved_objects)
  if (!is.null(x$weights))
    envir$weights <- x$weights
  if (use_original_args){
    if (inherits(x$original_args, "unknown")){
      warning("blablabla")
    } else {
      cl$data <- x$original_args$data
      cl$weights <- x$original_args$weights
    }
  }
  if (!eval) return(cl)
  if (use_original_args){
    envir <- env
  } else {
    envir <- as.environment(envir)
    parent.env(envir) <- env
  }
  form <- x$formula
  if (!is.null(form)) environment(form) <- envir
  cl$formula <- form
  eval(cl, envir = envir)
}

#' @rdname fit
#' @export
fit.multimodel <- function(x, eval = TRUE, use_original_args = FALSE, 
                           force = FALSE, which, env = parent.frame(), ...){
  if (n_model(x) == 1 && missing(which)){
    which <- 1
  }
  if (missing(which)){
    stop("fit(x) with a multimodel input including more than one model requires specification of ", sQuote("which"), ".")
  }
  x <- extract_model(x, which)
  fit(x, eval = eval, use_original_args = use_original_args, force = force, env = env, ...)
}

#' @rdname fit
#' @export
fit.cv <- function(x, eval = TRUE, use_original_args = FALSE, force = FALSE, which, 
                   env = parent.frame(), ...){
  if (n_model(x) == 1 && missing(which)){
    which <- 1
  }
  if (missing(which)){
    stop("fit(x) with a cv input including more than one model requires specification of ", sQuote("which"), ".")
  }
  x <- extract_model(x, which)
  fit(x, eval = eval, use_original_args = use_original_args, force = force, env = env, ...)
}


# --------------------------------------

#' @rdname fit
#' @export
add_fit <- function(x, ...){
  UseMethod("add_fit")
}

#' @export
add_fit.model <- function(x, force = FALSE, ...){
  if (!has_fit(x)) x$fit <- fit(x, force = force, ...)
  x
}

#' @export
add_fit.multimodel <- function(x, force = FALSE, ...){
  out <- lapply(seq_len(n_model(x)), 
                function(i) add_fit(extract_model(x, i), force = force, ...))
  out <- do.call(c, c(out, list(simplify = FALSE)))
  out$param <- x$param
  out
}

#' @export
add_fit.cv <- function(x, force = FALSE, ...){
  mm <- extract_multimodel(x)
  x$multimodel <- add_fit(mm, force = force, ...)
  x
}


#' @rdname fit
#' @export
has_fit <- function(x) UseMethod("has_fit")

#' @export
has_fit.model <- function(x){
  out <- !is.null(x$fit)
  names(out) <- label(x)
  out
}

#' @export
has_fit.multimodel <- function(x){
  vapply(x$models, has_fit, FUN.VALUE = logical(1))
}

#' @export
has_fit.cv <- function(x){
  out <- has_fit(x$multimodel)
  out
}

