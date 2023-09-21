
#' Update an object of class \dQuote{model} or \dQuote{multimodel}
#' 
#' @description
#' The method `update.model()` allows updating selected arguments in the original call to the model fitting function, 
#' in particular `formula`, `data`. 
#' Changing the model fitting function is not possible, however.
#' `update.multimodel()` will apply `update.model()` to each of its models.
#' 
#' `absent()`, `unchanged()` and `null()` are auxiliary functions designed for usage in `update.model()` and `multimodel()`.
#' `absent()` states the removal/absence of an argument in the model fitting call, 
#' `unchanged()` leaves an argument unchanged, and `null()` attributes an explicit `NULL` to a formal parameter.
#' Find more about the usage of these functions in the the \dQuote{Details} section and the examples.
#' 
#' @details
#' `update(model, parameter=NULL)` is ambiguous -- it's clearer to use `null()` or `absent()`.
#'  
#' Using a dot in a formula can be ambiguous, too. `update(model, formula=new_formula)` *updates* the model's original formula 
#' with `new_formula` (using \code{\link[stats]{update.formula}}). 
#' In order to *replace* the existing formula by a different one containing a dot, use `update(model, formula=I(new_formula))`.
#' See the examples.
#'  
#'  `absent()`, `unchanged()` and `null()` can also be used in `multimodel()`, which uses `update.model()` internally.
#' As an example, `multimodel(lm(y~x), weights = list(w, absent()))` will return a multimodel containing the two model definitions
#' based on calls `lm(y~x, weights = w)` and `lm(y~x)`. 
#' Always enclose `absent()`, `unchanged()` and `null()` in a `list()` if  several alternative model parameterizations are proposed
#' in `multimodel()`! The example won't work properly with `c(w, absent())` instead of `list(w, absent())`!
#' 
#' @param object A \dQuote{model} or \dQuote{multimodel}.
#' @param \dots Arguments to be updated.
#' @param ignore_changes_in_arg Parameter for internal use.
#' @inheritParams model
#' 
#' @return
#' `udpate.model()` returns the modified \link{model}. 
#' If \dQuote{...} is empty, `x` is not changed. Otherwise, the output will not contain the model fit.
#' 
#' @seealso \code{\link{model}}, \code{\link{multimodel}}; \code{\link[stats]{update.formula}}
#' 
#' @examples
#' if (require(lme4)){
#'   # Simluate data
#'   d <- simuldat()
#'   (mixmod <- lmer(Y ~ (X1|g) + X2 + X3, d, REML = TRUE))
#'   
#'   # update.model
#'   m_REML <- model(mixmod, label = "REML")
#'   # update parater "REML"
#'   m_ML <- update(m_REML, REML = FALSE, label = "ML")
#'   
#'   # absent(), unchanged(), null()
#'   update(m_REML, REML = absent())$call
#'   update(m_REML, REML = unchanged())$call
#'   update(m_REML, REML = null())$call # Note: not meaningful - fit() will fail
#'   
#'   # update.multimodel
#'   mm <- multimodel(m_REML, REML = c(TRUE, FALSE))
#'   w <- runif(nrow(d))
#'   update(mm, weights = w)
#'   
#'   # Dots in formula: 
#'   update(m_REML, formula = . ~ . - X2)
#'   
#'   # Updating vs replacing a formula:
#'   update(m_REML, formula = exp(Y) ~ .)    # updates the formula
#'   update(m_REML, formula = I(exp(Y) ~ .)) # replaces the formula
#'   
#'   # Usage of unchanged() in multimodel()
#'   multimodel(m_REML, REML = list(unchanged(), FALSE))
#' }
#' 
#' @importFrom stats update
#' @name update.model
#' @export
update.model <- function(object, ..., label = label.model(object), add_fit = FALSE, 
                         ignore_changes_in_arg = NULL){
  fn <- if (is.symbol(object$call[[1]])){
    as.character(object$call[[1]])
  } else {
    eval(object$call[[1]])
  }
  dots <- match.call(match.fun(fn), 
                     call = do.call(call, list("list", ...)))
  dots <- as.list(dots[-1])
  if (length(dots) == 0 || !makes_changes(dots, object)){
    return(set_label(object, label))
  }
  original_args <- object$original_args
  .updateData. <- 
    "data" %in% names(dots) && !inherits(dots$data, "unchanged")
  if (.updateData.){
    modeldata <- dots$data
    original_args$data <- match.call()$data
    dots$data <- NULL
  }
  .updateFormula. <- 
    "formula" %in% names(dots) && !inherits(dots$formula, c("AsIs", "unchanged"))
  if (.updateFormula.){
    dots$formula <- update(object$formula, dots$formula)
  }
  .updateWeights. <- "weights" %in% names(dots)
  if (.updateWeights.){
    original_args$weights <- match.call()$weights
  } else if (!is.null(object$weights)){
    dots$weights <- object$weights
  }
  cl <- object$call
  cl$formula <- eval(modify_call(cl, dots)$formula, object$saved_objects)
  cl$data <- if (.updateData.) modeldata else object$data
  out <- model(modify_call(cl, dots), label = label, class = object$class, 
               add_fit = add_fit, response_type = object$response_type,
               predict_function = object$predict_function, 
               env = list2env(object$saved_objects, parent = parent.frame()))
  if (!makes_changes(dots, object, ignore_changes_in_arg = ignore_changes_in_arg)){
    out$fit <- object$fit 
  }
  if (!is.null(original_args))
    out$original_args <- original_args
  out
}

# aux fn
makes_changes <- function(dots, object, ignore_changes_in_arg = NULL){
  dots <- dots[setdiff(names(dots), ignore_changes_in_arg)]
  for (nm in names(dots)){
    if (!(nm %in% names(object$call))) return(TRUE)
    val <- eval(object$call[[nm]], object$saved_objects, parent.frame())
    if (!isTRUE(all.equal(dots[[nm]], val))) return(TRUE)
    }
  FALSE
}

# ------------------------------------------------------------------------------

#' @rdname update.model
#' @export
update.multimodel <- function(object, ...){
  nmodel <- length(object$models)
  models <- lapply(seq_len(nmodel), 
                   function(i) update(extract(object, i), ...))
  do.call(c, c(models, list(simplify = FALSE)))
}

# ------------------------------------------------------------------------------

#' @rdname update.model
#' @export
absent <- function() structure(list(NA), class = "absent")

#' @rdname update.model
#' @export
null <- function() structure(list(NA), class = "null")

#' @rdname update.model
#' @export
unchanged <- function() structure(list(NA), class = "unchanged")


# Specifying that a parameter's value is not known
# Used in parameter tables
# 
# @export
unknown <- function() structure(NA, class = "unknown")


#' @export
format.unknown <- function(x, ...) "<unknown>"

#' @export
format.absent <- function(x, ...) "<absent>"

#' @export
format.null <- function(x, ...) "<NULL>"

